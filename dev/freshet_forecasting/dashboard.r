basins_shp <- sf::st_read(
    system.file(
        "snow_survey/swe_basins/swe_basins.shp",
        package = "YGwater",
        mustWork = TRUE
    ),
    quiet = TRUE
)


con <- YGwater::AquaConnect(
    name = "aquacache",
    host = Sys.getenv("aquacacheHostProd"),
    port = Sys.getenv("aquacachePortProd"),
    user = Sys.getenv("aquacacheAdminUser"),
    password = Sys.getenv("aquacacheAdminPass"),
)


fva_path <- file.path(
    "dev",
    "freshet_forecasting",
    "flood_vulnerable_gauges.yaml"
)

fva <- yaml::read_yaml(fva_path)

community <- "Dawson"

gauges_ds <- fva[community]


# Data access helpers -------------------------------------------------------

gauge_names <- unique(stats::na.omit(unlist(gauges_ds, use.names = FALSE)))
if (length(gauge_names) == 0) {
    stop(sprintf("No gauge names found for community '%s'", community))
}

#' Resolve Location Names To Location Metadata
#'
#' Looks up location names and returns identifying station metadata used
#' by downstream spatial and timeseries queries.
#'
#' @param location_names Character vector of location names.
#' @param con DBI connection object.
#'
#' @return A data.frame with `name`, `location_code`, `location_id`,
#'   `latitude`, and `longitude`.
#' @noRd
location_names_to_codes <- function(location_names, con) {
    location_names <- unique(stats::na.omit(unlist(
        location_names,
        use.names = FALSE
    )))
    if (length(location_names) == 0) {
        stop("location_names must contain at least one value")
    }

    location_names_sql <- paste(
        DBI::dbQuoteString(con, location_names),
        collapse = ","
    )

    YGwater::dbGetQueryDT(
        sprintf(
            "SELECT name, location_code, location_id, latitude, longitude FROM locations WHERE name IN (%s)",
            location_names_sql
        ),
        con = con
    )
}


#' Get Recent Images For Locations
#'
#' Fetches recent image metadata for one or more location codes, with an
#' optional file-bytes column for direct rendering.
#'
#' @param location_codes Character vector of location codes.
#' @param con DBI connection object.
#' @param lookback_hours Numeric lookback window in hours.
#' @param include_file Logical; include image byte payload when `TRUE`.
#'
#' @return A data.frame of image records ordered by location and recency.
#' @noRd
get_recent_images_by_location <- function(
    location_codes,
    con,
    lookback_hours = 1,
    include_file = FALSE
) {
    location_codes <- unique(stats::na.omit(unlist(
        location_codes,
        use.names = FALSE
    )))
    if (length(location_codes) == 0) {
        stop("location_codes must contain at least one value")
    }

    lookback_hours <- as.numeric(lookback_hours)
    if (
        length(lookback_hours) != 1 ||
            is.na(lookback_hours) ||
            lookback_hours <= 0
    ) {
        stop("lookback_hours must be a single positive number")
    }

    include_file <- as.logical(include_file)
    if (length(include_file) != 1 || is.na(include_file)) {
        stop("include_file must be TRUE or FALSE")
    }

    location_codes_sql <- paste(
        DBI::dbQuoteString(con, location_codes),
        collapse = ","
    )

    file_col_sql <- if (include_file) {
        ", i.file"
    } else {
        ""
    }

    YGwater::dbGetQueryDT(
        sprintf(
            paste(
                "SELECT",
                "    i.image_id,",
                "    i.img_series_id,",
                "    i.datetime,",
                "    i.image_type,",
                "    i.format,",
                "    l.location_id,",
                "    l.location_code,",
                "    l.name%s",
                "FROM images AS i",
                "JOIN image_series AS s",
                "  ON s.img_series_id = i.img_series_id",
                "JOIN locations AS l",
                "  ON l.location_id = s.location_id",
                "WHERE l.location_code IN (%s)",
                "  AND i.datetime >= NOW() - (INTERVAL '1 hour' * %.6f)",
                "ORDER BY l.location_code, i.datetime DESC"
            ),
            file_col_sql,
            location_codes_sql,
            lookback_hours
        ),
        con = con
    )
}

gauge_codes <- location_names_to_codes(gauge_names, con)

# Spatial helpers -----------------------------------------------------------

#' Retrieve Vector Layer Features As sf
#'
#' Pulls spatial features from `spatial.vectors` and returns an `sf` object
#' in the requested EPSG.
#'
#' @param feature_name Optional character vector of feature names to filter.
#' @param layer_name Character vector of layer names.
#' @param con DBI connection object.
#' @param epsg Integer EPSG code for output geometry.
#'
#' @return An `sf` object.
#' @noRd
get_spatial_layer_as_sf <- function(
    feature_name = NULL,
    layer_name,
    con,
    epsg = 4326
) {
    layer_values <- unlist(layer_name, use.names = FALSE)
    if (length(layer_values) == 0) {
        stop("layer_name must contain at least one value")
    }

    layer_sql <- paste(DBI::dbQuoteString(con, layer_values), collapse = ", ")
    where_clauses <- c(sprintf("layer_name IN (%s)", layer_sql))

    if (!is.null(feature_name)) {
        feature_values <- unlist(feature_name, use.names = FALSE)
        if (length(feature_values) > 0) {
            feature_sql <- paste(
                DBI::dbQuoteString(con, feature_values),
                collapse = ", "
            )
            where_clauses <- c(
                where_clauses,
                sprintf("feature_name IN (%s)", feature_sql)
            )
        }
    }
    query <- sprintf(
        "SELECT *, ST_AsText(ST_Transform(geom, %d)) as geom_wkt
         FROM spatial.vectors
         WHERE %s",
        epsg,
        paste(where_clauses, collapse = " AND ")
    )

    sf::st_as_sf(
        YGwater::dbGetQueryDT(query, con = con),
        wkt = "geom_wkt",
        crs = epsg
    )
}


#' Get Monitoring Locations With Available Parameters
#'
#' Builds an `sf` table of monitoring locations and boolean parameter
#' availability flags across timeseries and sample/result sources.
#'
#' @param con DBI connection object.
#'
#' @return An `sf` object with one row per location.
#' @noRd
get_monitoring_locations_as_sf <- function(con) {
    location_codes_with_params <- YGwater::dbGetQueryDT(
        paste(
            "WITH location_params AS (",
            "    SELECT l.location_code, l.name, l.latitude, l.longitude, p.param_name",
            "    FROM locations l",
            "    JOIN timeseries ts ON l.location_id = ts.location_id",
            "    JOIN parameters p ON p.parameter_id = ts.parameter_id",
            "    UNION",
            "    SELECT l.location_code, l.name, l.latitude, l.longitude, p.param_name",
            "    FROM locations l",
            "    JOIN samples s ON l.location_id = s.location_id",
            "    JOIN results r ON r.sample_id = s.sample_id",
            "    JOIN parameters p ON p.parameter_id = r.parameter_id",
            ")",
            "SELECT",
            "    location_code,",
            "    name,",
            "    latitude,",
            "    longitude,",
            "    BOOL_OR(param_name = 'temperature, air') AS \"temperature, air\",",
            "    BOOL_OR(param_name = 'precipitation, total') AS \"precipitation, total\",",
            "    BOOL_OR(param_name = 'water level') AS \"water level\",",
            "    BOOL_OR(param_name = 'water flow') AS \"water flow\",",
            "    BOOL_OR(param_name = 'snow water equivalent') AS \"snow water equivalent\",",
            "    BOOL_OR(param_name = 'snow depth') AS \"snow depth\"",
            "FROM location_params",
            "WHERE param_name IN ('temperature, air', 'precipitation, total', 'water level', 'water flow', 'snow water equivalent', 'snow depth')",
            "GROUP BY location_code, name, latitude, longitude",
            "ORDER BY location_code"
        ),
        con = con
    )

    sf::st_as_sf(
        location_codes_with_params,
        coords = c("longitude", "latitude"),
        crs = 4326,
        remove = FALSE
    )
}

#' Select Monitoring Stations Intersecting Basin Polygons
#'
#' Spatially joins monitoring stations to basin polygons and keeps the most
#' representative basin match per station.
#'
#' @param location_codes Character vector of basin feature names/codes.
#' @param poly `sf` polygon layer.
#' @param monitoring_stations `sf` points layer.
#'
#' @return An `sf` object of stations by basin.
#' @noRd
get_stations_by_basin <- function(location_codes, poly, monitoring_stations) {
    location_codes <- unique(stats::na.omit(unlist(
        location_codes,
        use.names = FALSE
    )))
    if (length(location_codes) == 0) {
        stop("location_codes must contain at least one value")
    }

    basins <- poly
    if ("feature_name" %in% names(basins)) {
        basins <- basins[basins$feature_name %in% location_codes, ]
    }

    if (nrow(basins) == 0) {
        return(monitoring_stations[0, ])
    }

    basins$basin_area_km2 <- as.numeric(
        sf::st_area(sf::st_transform(basins, 3578))
    ) /
        1e6

    if (is.na(sf::st_crs(monitoring_stations))) {
        stop("monitoring_stations must have a defined CRS")
    }

    if (is.na(sf::st_crs(basins))) {
        stop("poly must have a defined CRS")
    }

    if (sf::st_crs(monitoring_stations) != sf::st_crs(basins)) {
        basins <- sf::st_transform(basins, sf::st_crs(monitoring_stations))
    }

    basin_cols <- intersect(
        c("feature_name", "layer_name", "basin_area_km2"),
        names(basins)
    )

    stations_by_basin <- sf::st_join(
        monitoring_stations,
        basins[basin_cols],
        join = sf::st_intersects,
        left = FALSE
    )

    station_key <- if ("location_code" %in% names(stations_by_basin)) {
        "location_code"
    } else if ("location_id" %in% names(stations_by_basin)) {
        "location_id"
    } else {
        stop(
            "monitoring_stations requires location_code or location_id for deduplication"
        )
    }

    stations_by_basin <- stations_by_basin[
        order(
            stations_by_basin[[station_key]],
            stations_by_basin$basin_area_km2,
            na.last = TRUE
        ),
    ]

    stations_by_basin[!duplicated(stations_by_basin[[station_key]]), ]
}


# lookup drainage basins for POIs
basins <- get_spatial_layer_as_sf(
    layer_name = "Drainage basins",
    feature_name = gauge_codes$location_code,
    con = con
)

# query all met and hydro stations
locations_sf <- get_monitoring_locations_as_sf(con)

monitoring_stations <- get_stations_by_basin(
    location_codes = gauge_codes$location_code,
    poly = basins,
    monitoring_stations = locations_sf
)

# plot(monitoring_stations)

# communities <- get_spatial_layer_as_sf(
#     feature_name = community,
#     layer_name = list("Communities"),
#     con = con
# )

# Summary query helpers -----------------------------------------------------

#' Validate Summary Query Inputs
#'
#' Normalizes and validates location codes and summary options used across
#' summary query builders.
#'
#' @param location_codes Character vector of location codes.
#' @param con DBI connection object.
#' @param max_missing_percent Numeric threshold between 0 and 100.
#' @param historical_median Logical flag enabling historical median fields.
#'
#' @return A named list of normalized query inputs.
#' @noRd
prepare_summary_inputs <- function(
    location_codes,
    con,
    max_missing_percent,
    historical_median
) {
    location_codes <- unique(stats::na.omit(unlist(
        location_codes,
        use.names = FALSE
    )))
    if (length(location_codes) == 0) {
        stop("location_codes must contain at least one value")
    }

    max_missing_percent <- as.numeric(max_missing_percent)
    if (
        length(max_missing_percent) != 1 ||
            is.na(max_missing_percent) ||
            max_missing_percent < 0 ||
            max_missing_percent > 100
    ) {
        stop("max_missing_percent must be a single number between 0 and 100")
    }

    historical_median <- as.logical(historical_median)
    if (length(historical_median) != 1 || is.na(historical_median)) {
        stop("historical_median must be TRUE or FALSE")
    }

    list(
        location_codes_sql = paste(
            DBI::dbQuoteString(con, location_codes),
            collapse = ","
        ),
        max_missing_percent = max_missing_percent,
        historical_median = historical_median
    )
}


#' Build Optional Coverage SQL Fragments
#'
#' Produces SQL fragments for coverage metrics when historical median mode
#' is enabled.
#'
#' @param historical_median Logical flag.
#' @param max_missing_percent Numeric threshold.
#' @param coverage_cte_sql Character SQL CTE text.
#' @param coverage_join_sql Character SQL JOIN text.
#'
#' @return A named list of SQL fragment strings.
#' @noRd
build_optional_coverage_fragments <- function(
    historical_median,
    max_missing_percent,
    coverage_cte_sql,
    coverage_join_sql
) {
    if (!historical_median) {
        return(list(
            coverage_cte_sql = "",
            coverage_select_sql = "",
            coverage_join_sql = "",
            coverage_filter_sql = ""
        ))
    }

    list(
        coverage_cte_sql = coverage_cte_sql,
        coverage_select_sql = paste(
            "    cm.total_years_available,",
            "    cm.percent_missing_observations,"
        ),
        coverage_join_sql = coverage_join_sql,
        coverage_filter_sql = sprintf(
            "  AND COALESCE(cm.percent_missing_observations, 100) <= %.6f",
            max_missing_percent
        )
    )
}

#' Sort Summary Rows By Drainage Area
#'
#' Adds a temporary drainage-area sort key based on basin matches, then
#' returns rows sorted from largest to smallest basin area.
#'
#' @param dat Summary data.frame.
#' @param stations Station `sf`/data.frame containing basin area values.
#'
#' @return Reordered summary data.frame.
#' @noRd
sort_summary_by_drainage_area <- function(dat, stations) {
    if (
        is.null(dat) ||
            nrow(dat) == 0 ||
            !"location_code" %in% names(dat)
    ) {
        return(dat)
    }

    if (
        is.null(stations) ||
            nrow(stations) == 0 ||
            !all(c("location_code", "basin_area_km2") %in% names(stations))
    ) {
        return(dat)
    }

    lookup <- data.frame(
        location_code = as.character(stations$location_code),
        basin_area_km2 = as.numeric(stations$basin_area_km2),
        stringsAsFactors = FALSE
    )
    lookup <- lookup[!duplicated(lookup$location_code), ]

    dat$drainage_area_sort_km2 <- lookup$basin_area_km2[
        match(as.character(dat$location_code), lookup$location_code)
    ]
    dat <- dat[
        order(dat$drainage_area_sort_km2, decreasing = TRUE, na.last = TRUE),
    ]
    dat$drainage_area_sort_km2 <- NULL

    dat
}


# Timeseries utilities ------------------------------------------------------

#' Interpret Loaded Datetimes As Local Wall Time
#'
#' Parses datetime-like columns as fixed UTC-7 local wall-clock values
#' without applying UTC offset conversion.
#'
#' @param dat Data.frame containing datetime columns.
#' @param time_columns Character vector of datetime column names.
#'
#' @return Data.frame with parsed POSIXct datetime columns.
#' @noRd
interpret_loaded_times_as_local <- function(
    dat,
    time_columns = c("datetime", "latest_time")
) {
    fixed_timezone <- "Etc/GMT+7"

    if (is.null(dat) || nrow(dat) == 0) {
        return(dat)
    }

    for (col_name in intersect(time_columns, names(dat))) {
        local_text <- if (inherits(dat[[col_name]], c("POSIXct", "POSIXt"))) {
            format(
                dat[[col_name]],
                tz = "UTC",
                usetz = FALSE,
                format = "%Y-%m-%d %H:%M:%S"
            )
        } else {
            as.character(dat[[col_name]])
        }

        x <- suppressWarnings(as.POSIXct(
            local_text,
            tz = fixed_timezone,
            format = "%Y-%m-%d %H:%M:%S"
        ))
        if (all(is.na(x))) {
            next
        }

        attr(x, "tzone") <- fixed_timezone
        dat[[col_name]] <- x
    }

    dat
}


#' Sanitize Loaded Series Values
#'
#' Replaces negative values with `NA` for non-temperature parameters.
#'
#' @param dat Data.frame with `value` column.
#' @param parameter Parameter name.
#'
#' @return Data.frame with sanitized values.
#' @noRd
sanitize_loaded_series_values <- function(dat, parameter) {
    if (
        is.null(dat) ||
            nrow(dat) == 0 ||
            !"value" %in% names(dat) ||
            identical(parameter, "temperature, air")
    ) {
        return(dat)
    }

    dat$value[!is.na(dat$value) & dat$value < 0] <- NA_real_
    dat
}


#' Summarize Instantaneous Water-Level Timeseries
#'
#' Computes latest value, recent changes, and optional historical/coverage
#' metrics for water-level stations.
#'
#' @param location_codes Character vector of location codes.
#' @param con DBI connection object
#' @param max_missing_percent Numeric threshold for optional coverage filter.
#' @param historical_median Logical; include historical median fields.
#' @param reference_time Optional POSIXct reference timestamp.
#'
#' @return Summary data.frame.
#' @noRd
get_instantaneous_timeseries_summary <- function(
    location_codes,
    con,
    max_missing_percent = 100,
    historical_median = FALSE,
    reference_time = NULL
) {
    inputs <- prepare_summary_inputs(
        location_codes = location_codes,
        con = con,
        max_missing_percent = max_missing_percent,
        historical_median = historical_median
    )
    location_codes_sql <- inputs$location_codes_sql
    max_missing_percent <- inputs$max_missing_percent
    historical_median <- inputs$historical_median

    ref_ts_sql <- if (is.null(reference_time)) {
        "NOW()"
    } else {
        paste0(
            "'",
            format(as.POSIXct(reference_time), "%Y-%m-%d %H:%M:%S"),
            "'::timestamp"
        )
    }
    ref_date_sql <- if (is.null(reference_time)) {
        "CURRENT_DATE"
    } else {
        paste0("'", format(as.POSIXct(reference_time), "%Y-%m-%d"), "'::date")
    }
    historical_cte_sql <- if (historical_median) {
        paste(
            ", historical_targets AS (",
            "    SELECT",
            "        l.location_id,",
            "        l.timeseries_id,",
            "        EXTRACT(DOY FROM l.last_datetime)::int AS target_doy",
            "    FROM latest_by_location l",
            "),",
            "historical_median AS (",
            "    SELECT",
            "        ht.location_id,",
            "        PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY mc.value) AS median_value",
            "    FROM historical_targets ht",
            "    JOIN measurements_continuous mc",
            "      ON mc.timeseries_id = ht.timeseries_id",
            "    WHERE mc.value IS NOT NULL",
            paste0(
                "      AND mc.datetime < DATE_TRUNC('year', ",
                ref_date_sql,
                ")"
            ),
            "      AND EXTRACT(DOY FROM mc.datetime)::int = ht.target_doy",
            "    GROUP BY ht.location_id",
            "),"
        )
    } else {
        ""
    }

    historical_select_sql <- if (historical_median) {
        paste(
            "    hm.median_value AS historical_median,",
            "    CASE",
            "        WHEN hm.median_value IS NULL OR hm.median_value = 0 THEN NULL",
            "        ELSE (c.current_water_level / hm.median_value) * 100",
            "    END AS relative_to_median,"
        )
    } else {
        ""
    }

    historical_join_sql <- if (historical_median) {
        paste(
            "LEFT JOIN historical_median hm",
            "  ON hm.location_id = loc.location_id"
        )
    } else {
        ""
    }

    coverage_cte_sql <- if (historical_median) {
        paste(
            "coverage_stats AS (",
            "    SELECT",
            "        t.location_id,",
            "        t.timeseries_id,",
            "        MIN(mc.datetime) AS first_datetime,",
            "        MAX(mc.datetime) AS last_datetime,",
            "        COUNT(*) FILTER (WHERE mc.value IS NOT NULL) AS observed_count,",
            "        GREATEST(",
            "            FLOOR(EXTRACT(EPOCH FROM (MAX(mc.datetime) - MIN(mc.datetime))) / 300)::bigint + 1,",
            "            0",
            "        ) AS expected_count",
            "    FROM target_timeseries t",
            "    LEFT JOIN measurements_continuous mc",
            "      ON mc.timeseries_id = t.timeseries_id",
            "    GROUP BY t.location_id, t.timeseries_id",
            "),",
            "coverage_metrics AS (",
            "    SELECT",
            "        location_id,",
            "        timeseries_id,",
            "        CASE",
            "            WHEN first_datetime IS NULL OR last_datetime IS NULL THEN NULL",
            "            ELSE EXTRACT(EPOCH FROM (last_datetime - first_datetime)) / (365.25 * 24 * 3600)",
            "        END AS total_years_available,",
            "        CASE",
            "            WHEN expected_count <= 0 THEN NULL",
            "            ELSE ((expected_count - observed_count)::numeric / expected_count) * 100",
            "        END AS percent_missing_observations",
            "    FROM coverage_stats",
            "),"
        )
    } else {
        ""
    }

    coverage_join_sql <- paste(
        "LEFT JOIN coverage_metrics cm",
        "  ON cm.location_id = c.location_id",
        " AND cm.timeseries_id = c.timeseries_id"
    )

    coverage_fragments <- build_optional_coverage_fragments(
        historical_median = historical_median,
        max_missing_percent = max_missing_percent,
        coverage_cte_sql = coverage_cte_sql,
        coverage_join_sql = coverage_join_sql
    )

    dat <- YGwater::dbGetQueryDT(
        sprintf(
            paste(
                "WITH target_locations AS (",
                "    SELECT UNNEST(ARRAY[%s]::text[]) AS location_code",
                "),",
                "target_timeseries AS (",
                "    SELECT ts.location_id, ts.timeseries_id",
                "    FROM timeseries ts",
                "    JOIN locations loc",
                "      ON loc.location_id = ts.location_id",
                "    JOIN target_locations tl",
                "      ON tl.location_code = loc.location_code",
                "    JOIN parameters p",
                "      ON p.parameter_id = ts.parameter_id",
                "    WHERE ts.record_rate = INTERVAL '5 minutes'",
                "      AND p.param_name = 'water level'",
                "),",
                "filtered AS MATERIALIZED (",
                "    SELECT t.location_id, t.timeseries_id,",
                "           mc.datetime, mc.value AS value",
                "    FROM measurements_continuous mc",
                "    JOIN target_timeseries t",
                "      ON t.timeseries_id = mc.timeseries_id",
                "    WHERE mc.value IS NOT NULL",
                paste0(
                    "      AND mc.datetime >= ",
                    ref_date_sql,
                    " - INTERVAL '7 days'"
                ),
                paste0(
                    "      AND mc.datetime < ",
                    ref_date_sql,
                    " + INTERVAL '1 day'"
                ),
                "),",
                "%s",
                "latest_by_location AS (",
                "    SELECT DISTINCT ON (location_id)",
                "        location_id,",
                "        timeseries_id,",
                "        datetime AS last_datetime,",
                "        value AS current_water_level",
                "    FROM filtered",
                "    ORDER BY location_id, datetime DESC, timeseries_id",
                "),",
                "changes AS (",
                "    SELECT",
                "        l.location_id,",
                "        l.timeseries_id,",
                "        l.last_datetime,",
                "        l.current_water_level,",
                "        v24.value AS value_24h,",
                "        v48.value AS value_48h,",
                "        v72.value AS value_72h,",
                "        v1w.value AS value_1w",
                "    FROM latest_by_location l",
                "    LEFT JOIN LATERAL (",
                "        SELECT f.value",
                "        FROM filtered f",
                "        WHERE f.location_id = l.location_id",
                "          AND f.timeseries_id = l.timeseries_id",
                "          AND f.datetime <= l.last_datetime - INTERVAL '24 hours'",
                "        ORDER BY f.datetime DESC",
                "        LIMIT 1",
                "    ) v24 ON TRUE",
                "    LEFT JOIN LATERAL (",
                "        SELECT f.value",
                "        FROM filtered f",
                "        WHERE f.location_id = l.location_id",
                "          AND f.timeseries_id = l.timeseries_id",
                "          AND f.datetime <= l.last_datetime - INTERVAL '48 hours'",
                "        ORDER BY f.datetime DESC",
                "        LIMIT 1",
                "    ) v48 ON TRUE",
                "    LEFT JOIN LATERAL (",
                "        SELECT f.value",
                "        FROM filtered f",
                "        WHERE f.location_id = l.location_id",
                "          AND f.timeseries_id = l.timeseries_id",
                "          AND f.datetime <= l.last_datetime - INTERVAL '72 hours'",
                "        ORDER BY f.datetime DESC",
                "        LIMIT 1",
                "    ) v72 ON TRUE",
                "    LEFT JOIN LATERAL (",
                "        SELECT f.value",
                "        FROM filtered f",
                "        WHERE f.location_id = l.location_id",
                "          AND f.timeseries_id = l.timeseries_id",
                "          AND f.datetime <= l.last_datetime - INTERVAL '7 days'",
                "        ORDER BY f.datetime DESC",
                "        LIMIT 1",
                "    ) v1w ON TRUE",
                ")",
                "%s",
                "SELECT",
                "    loc.location_id,",
                "    loc.location_code,",
                "    loc.name,",
                "    c.timeseries_id,",
                "    c.last_datetime AS latest_time,",
                "    c.current_water_level,",
                "%s",
                "    c.current_water_level - c.value_24h AS change_24h,",
                "    c.current_water_level - c.value_48h AS change_48h,",
                "    c.current_water_level - c.value_72h AS change_72h,",
                "    c.current_water_level - c.value_1w AS change_1w,",
                "%s",
                "    EXTRACT(EPOCH FROM (",
                paste0(ref_ts_sql, " - c.last_datetime))"),
                "      / 3600.0 AS last_data_age_hours",
                "FROM target_locations tl",
                "JOIN locations loc",
                "  ON loc.location_code = tl.location_code",
                "LEFT JOIN changes c",
                "  ON c.location_id = loc.location_id",
                "%s",
                "%s",
                "WHERE c.last_datetime IS NOT NULL",
                "%s",
                "ORDER BY loc.location_code"
            ),
            location_codes_sql,
            coverage_fragments$coverage_cte_sql,
            historical_cte_sql,
            historical_select_sql,
            coverage_fragments$coverage_select_sql,
            historical_join_sql,
            coverage_fragments$coverage_join_sql,
            coverage_fragments$coverage_filter_sql
        ),
        con = con
    )

    interpret_loaded_times_as_local(dat, time_columns = c("latest_time"))
}

#' Summarize Precipitation Accumulations
#'
#' Computes latest weekly/monthly/seasonal precipitation accumulations and
#' optional historical/coverage metrics per location.
#'
#' @param location_codes Character vector of location codes.
#' @param con DBI connection object.
#' @param max_missing_percent Numeric threshold for optional coverage filter.
#' @param historical_median Logical; include historical median fields.
#' @param reference_time Optional POSIXct reference timestamp.
#'
#' @return Summary data.frame.
#' @noRd
get_precipitation_timeseries_summary <- function(
    location_codes,
    con,
    max_missing_percent = 100,
    historical_median = FALSE,
    reference_time = NULL
) {
    inputs <- prepare_summary_inputs(
        location_codes = location_codes,
        con = con,
        max_missing_percent = max_missing_percent,
        historical_median = historical_median
    )
    location_codes_sql <- inputs$location_codes_sql
    max_missing_percent <- inputs$max_missing_percent
    historical_median <- inputs$historical_median

    ref_ts_sql <- if (is.null(reference_time)) {
        "NOW()"
    } else {
        paste0(
            "'",
            format(as.POSIXct(reference_time), "%Y-%m-%d %H:%M:%S"),
            "'::timestamp"
        )
    }
    ref_date_sql <- if (is.null(reference_time)) {
        "CURRENT_DATE"
    } else {
        paste0("'", format(as.POSIXct(reference_time), "%Y-%m-%d"), "'::date")
    }

    historical_cte_sql <- if (historical_median) {
        paste(
            ", historical_targets AS (",
            "    SELECT",
            "        l.location_id,",
            "        l.timeseries_id,",
            "        EXTRACT(DOY FROM l.last_date)::int AS target_doy",
            "    FROM latest_by_location l",
            "),",
            "historical_anchors AS (",
            "    SELECT",
            "        ht.location_id,",
            "        ht.timeseries_id,",
            "        y.yr,",
            "        (MAKE_DATE(y.yr, 1, 1) + (ht.target_doy - 1) * INTERVAL '1 day')::date AS anchor_date",
            "    FROM historical_targets ht",
            "    JOIN LATERAL (",
            "        SELECT DISTINCT EXTRACT(YEAR FROM mc.date)::int AS yr",
            "        FROM measurements_calculated_daily mc",
            "        WHERE mc.timeseries_id = ht.timeseries_id",
            paste0(
                "          AND mc.date < DATE_TRUNC('year', ",
                ref_date_sql,
                ")"
            ),
            "    ) y ON TRUE",
            "),",
            "historical_monthly AS (",
            "    SELECT",
            "        ha.location_id,",
            "        ha.yr,",
            "        SUM(mc.value) AS precipitation_1m_accumulation",
            "    FROM historical_anchors ha",
            "    JOIN measurements_calculated_daily mc",
            "      ON mc.timeseries_id = ha.timeseries_id",
            "    WHERE mc.value IS NOT NULL",
            "      AND mc.date > ha.anchor_date - INTERVAL '1 month'",
            "      AND mc.date <= ha.anchor_date",
            "    GROUP BY ha.location_id, ha.yr",
            "),",
            "historical_median AS (",
            "    SELECT",
            "        location_id,",
            "        PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY precipitation_1m_accumulation) AS historical_1m_median",
            "    FROM historical_monthly",
            "    GROUP BY location_id",
            "),"
        )
    } else {
        ""
    }

    historical_select_sql <- if (historical_median) {
        paste(
            "    hm.historical_1m_median AS historical_median,",
            "    CASE",
            "        WHEN hm.historical_1m_median IS NULL OR hm.historical_1m_median = 0 THEN NULL",
            "        ELSE (ca.precipitation_1m_accumulation / hm.historical_1m_median) * 100",
            "    END AS relative_to_median,"
        )
    } else {
        ""
    }

    historical_join_sql <- if (historical_median) {
        paste(
            "LEFT JOIN historical_median hm",
            "  ON hm.location_id = loc.location_id"
        )
    } else {
        ""
    }

    coverage_cte_sql <- if (historical_median) {
        paste(
            "coverage_stats AS (",
            "    SELECT",
            "        t.location_id,",
            "        t.timeseries_id,",
            "        MIN(mc.date) AS first_date,",
            "        MAX(mc.date) AS last_date,",
            "        COUNT(*) FILTER (WHERE mc.value IS NOT NULL) AS observed_count,",
            "        GREATEST((MAX(mc.date) - MIN(mc.date) + 1), 0) AS expected_count",
            "    FROM target_timeseries t",
            "    LEFT JOIN measurements_calculated_daily mc",
            "      ON mc.timeseries_id = t.timeseries_id",
            "    GROUP BY t.location_id, t.timeseries_id",
            "),",
            "coverage_metrics AS (",
            "    SELECT",
            "        location_id,",
            "        timeseries_id,",
            "        CASE",
            "            WHEN first_date IS NULL OR last_date IS NULL THEN NULL",
            "            ELSE (last_date - first_date + 1)::numeric / 365.25",
            "        END AS total_years_available,",
            "        CASE",
            "            WHEN expected_count <= 0 THEN NULL",
            "            ELSE ((expected_count - observed_count)::numeric / expected_count) * 100",
            "        END AS percent_missing_observations",
            "    FROM coverage_stats",
            "),"
        )
    } else {
        ""
    }

    coverage_join_sql <- paste(
        "LEFT JOIN coverage_metrics cm",
        "  ON cm.location_id = l.location_id",
        " AND cm.timeseries_id = l.timeseries_id"
    )

    coverage_fragments <- build_optional_coverage_fragments(
        historical_median = historical_median,
        max_missing_percent = max_missing_percent,
        coverage_cte_sql = coverage_cte_sql,
        coverage_join_sql = coverage_join_sql
    )

    dat <- YGwater::dbGetQueryDT(
        sprintf(
            paste(
                "WITH target_locations AS (",
                "    SELECT UNNEST(ARRAY[%s]::text[]) AS location_code",
                "),",
                "target_timeseries AS (",
                "    SELECT ts.location_id, ts.timeseries_id",
                "    FROM timeseries ts",
                "    JOIN locations loc",
                "      ON loc.location_id = ts.location_id",
                "    JOIN target_locations tl",
                "      ON tl.location_code = loc.location_code",
                "    JOIN parameters p",
                "      ON p.parameter_id = ts.parameter_id",
                "    WHERE p.param_name = 'precipitation, total'",
                "),",
                "filtered AS MATERIALIZED (",
                "    SELECT t.location_id, t.timeseries_id,",
                "           mc.date, mc.value AS value",
                "    FROM measurements_calculated_daily mc",
                "    JOIN target_timeseries t",
                "      ON t.timeseries_id = mc.timeseries_id",
                "    WHERE mc.value IS NOT NULL",
                paste0(
                    "      AND mc.date >= ",
                    ref_date_sql,
                    " - INTERVAL '6 months'"
                ),
                paste0(
                    "      AND mc.date < ",
                    ref_date_sql,
                    " + INTERVAL '1 day'"
                ),
                "),",
                "%s",
                "latest_by_location AS (",
                "    SELECT DISTINCT ON (location_id)",
                "        location_id,",
                "        timeseries_id,",
                "        date AS last_date",
                "    FROM filtered",
                "    ORDER BY location_id, date DESC, timeseries_id",
                "),",
                "current_accumulations AS (",
                "    SELECT",
                "        l.location_id,",
                "        SUM(mc.value) FILTER (",
                "            WHERE mc.date > l.last_date - INTERVAL '7 days'",
                "              AND mc.date <= l.last_date",
                "        ) AS precipitation_1w_accumulation,",
                "        SUM(mc.value) FILTER (",
                "            WHERE mc.date > l.last_date - INTERVAL '1 month'",
                "              AND mc.date <= l.last_date",
                "        ) AS precipitation_1m_accumulation,",
                "        SUM(mc.value) FILTER (",
                "            WHERE mc.date > l.last_date - INTERVAL '6 months'",
                "              AND mc.date <= l.last_date",
                "        ) AS precipitation_6m_accumulation",
                "    FROM latest_by_location l",
                "    JOIN measurements_calculated_daily mc",
                "      ON mc.timeseries_id = l.timeseries_id",
                "    WHERE mc.value IS NOT NULL",
                "      AND mc.date > l.last_date - INTERVAL '6 months'",
                "      AND mc.date <= l.last_date",
                "    GROUP BY l.location_id",
                ")",
                "%s",
                "SELECT",
                "    loc.location_id,",
                "    loc.location_code,",
                "    loc.name,",
                "    l.timeseries_id,",
                "    l.last_date::timestamp AS latest_time,",
                "    ca.precipitation_1w_accumulation,",
                "    ca.precipitation_1m_accumulation,",
                "    ca.precipitation_6m_accumulation,",
                "%s",
                "%s",
                paste0(
                    "    EXTRACT(EPOCH FROM (",
                    ref_ts_sql,
                    " - l.last_date::timestamp)) / 3600.0 AS last_data_age_hours"
                ),
                "FROM target_locations tl",
                "JOIN locations loc",
                "  ON loc.location_code = tl.location_code",
                "LEFT JOIN latest_by_location l",
                "  ON l.location_id = loc.location_id",
                "LEFT JOIN current_accumulations ca",
                "  ON ca.location_id = loc.location_id",
                "%s",
                "%s",
                "WHERE l.last_date IS NOT NULL",
                "%s",
                "ORDER BY loc.location_code"
            ),
            location_codes_sql,
            coverage_fragments$coverage_cte_sql,
            historical_cte_sql,
            historical_select_sql,
            coverage_fragments$coverage_select_sql,
            historical_join_sql,
            coverage_fragments$coverage_join_sql,
            coverage_fragments$coverage_filter_sql
        ),
        con = con
    )

    interpret_loaded_times_as_local(dat, time_columns = c("latest_time"))
}

#' Get Annual Peak Discharge By Year
#'
#' Retrieves annual maximum daily values for a location set and parameter.
#'
#' @param location_codes Character vector of location codes.
#' @param parameter Parameter name.
#' @param con DBI connection object.
#'
#' @return Data.frame of annual peak values by location and year.
#' @noRd
get_annual_peak_discharge <- function(
    location_codes,
    parameter = "water flow",
    con
) {
    location_codes_str <- paste(sprintf("'%s'", location_codes), collapse = ",")

    YGwater::dbGetQueryDT(
        sprintf(
            paste(
                "SELECT",
                "    ts.location_id,",
                "    l.location_code,",
                "    l.name,",
                "    EXTRACT(YEAR FROM mc.date)::int AS year,",
                "    MAX(mc.value) AS max_annual_peak_discharge",
                "FROM measurements_calculated_daily mc",
                "JOIN timeseries ts ON mc.timeseries_id = ts.timeseries_id",
                "JOIN locations l ON ts.location_id = l.location_id",
                "JOIN parameters p ON ts.parameter_id = p.parameter_id",
                "WHERE l.location_code IN (%s)",
                "  AND p.param_name = '%s'",
                "GROUP BY ts.location_id, l.location_code, l.name, EXTRACT(YEAR FROM mc.date)",
                "ORDER BY l.location_code, year"
            ),
            location_codes_str,
            parameter
        ),
        con = con
    )
}

# Separate query: return-period discharge estimates from annual peak series

#' Estimate Return-Period Discharge Quantiles
#'
#' Uses annual peak daily values to estimate return-period quantiles via
#' `percentile_cont`.
#'
#' @param location_codes Character vector of location codes.
#' @param parameter Parameter name.
#' @param return_periods Numeric vector of return periods in years.
#' @param con DBI connection object.
#'
#' @return Data.frame with return-period columns (`rp_*`).
#' @noRd
get_return_period_discharge <- function(
    location_codes,
    parameter = "water flow",
    return_periods = c(2, 5, 10, 25, 50, 100),
    con
) {
    location_codes_str <- paste(sprintf("'%s'", location_codes), collapse = ",")

    # Convert return periods to percentiles (1 - 1/RP)
    percentiles <- 1 - (1 / return_periods)
    percentile_cols <- paste(
        sprintf(
            "percentile_cont(%.4f) WITHIN GROUP (ORDER BY annual_peak) AS rp_%d",
            percentiles,
            return_periods
        ),
        collapse = ", "
    )

    YGwater::dbGetQueryDT(
        sprintf(
            paste(
                "WITH annual_peaks AS (",
                "    SELECT",
                "        ts.location_id,",
                "        l.location_code,",
                "        EXTRACT(YEAR FROM mc.date)::int AS year,",
                "        MAX(mc.value) AS annual_peak",
                "    FROM measurements_calculated_daily mc",
                "    JOIN timeseries ts ON mc.timeseries_id = ts.timeseries_id",
                "    JOIN locations l ON ts.location_id = l.location_id",
                "    JOIN parameters p ON ts.parameter_id = p.parameter_id",
                "    WHERE l.location_code IN (%s)",
                "      AND p.param_name = '%s'",
                "    GROUP BY ts.location_id, l.location_code, EXTRACT(YEAR FROM mc.date)",
                ")",
                "SELECT",
                "    location_id,",
                "    location_code,",
                "    COUNT(*) AS n_years,",
                "    %s",
                "FROM annual_peaks",
                "GROUP BY location_id, location_code"
            ),
            location_codes_str,
            parameter,
            percentile_cols
        ),
        con = con
    )
}

#' Normalize Historical Plot Start Year
#'
#' Validates and coerces the selected historical start year used by
#' percentile envelopes and historical daily traces.
#'
#' @param historical_start_year Year-like scalar value.
#'
#' @return Integer year.
#' @noRd
normalize_historical_start_year <- function(historical_start_year) {
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    historical_start_year <- suppressWarnings(as.integer(
        historical_start_year[[1]]
    ))

    if (
        length(historical_start_year) != 1 ||
            is.na(historical_start_year) ||
            historical_start_year < 1980 ||
            historical_start_year > current_year
    ) {
        stop(sprintf(
            "historical_start_year must be between 1980 and %d",
            current_year
        ))
    }

    historical_start_year
}

parameter_axis_title <- function(parameter) {
    axis_titles <- c(
        "precipitation (1wk)" = "Precipitation (1wk accumulation, mm)",
        "precipitation (24hr)" = "Precipitation (24hr, mm)",
        "FDD" = "Freezing Degree Days (FDD)",
        "DDT" = "Thawing Degree Days (DDT)"
    )

    if (parameter %in% names(axis_titles)) {
        axis_titles[[parameter]]
    } else {
        parameter
    }
}

empty_plotly_widget <- function(title = NULL, annotations = NULL) {
    widget <- plotly::plot_ly(
        x = numeric(0),
        y = numeric(0),
        type = "scatter",
        mode = "lines",
        hoverinfo = "skip",
        showlegend = FALSE
    )

    plotly::layout(
        widget,
        title = title,
        annotations = annotations
    )
}

# Separate query: daily percentiles by day-of-year (1-365), excluding Feb 29
#' Get Day-Of-Year Percentile Bands
#'
#' Builds daily percentile envelopes for a parameter, with special handling
#' for precipitation weekly accumulation and leap-day alignment.
#'
#' @param location_codes Character vector of location codes.
#' @param parameter Parameter name.
#' @param con DBI connection object.
#' @param historical_start_year Integer year used as the lower bound for
#'   historical daily values.
#'
#' @return Data.frame of percentile bands by location and day-of-year.
#' @noRd
get_daily_percentiles <- function(
    location_codes,
    parameter = "water flow",
    con,
    historical_start_year = 2020
) {
    location_codes_str <- paste(sprintf("'%s'", location_codes), collapse = ",")
    historical_start_year <- normalize_historical_start_year(
        historical_start_year
    )
    historical_start_date_sql <- paste0(
        "MAKE_DATE(",
        historical_start_year,
        ", 1, 1)"
    )

    if (identical(parameter, "FDD")) {
        return(YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "WITH daily_temp AS (",
                    "    SELECT",
                    "        l.location_code,",
                    "        DATE_TRUNC('day', mc.datetime)::date AS date,",
                    "        AVG(mc.value) AS mean_temp",
                    "    FROM measurements_continuous mc",
                    "    JOIN timeseries ts ON mc.timeseries_id = ts.timeseries_id",
                    "    JOIN locations l ON ts.location_id = l.location_id",
                    "    JOIN parameters p ON ts.parameter_id = p.parameter_id",
                    "    WHERE l.location_code IN (%s)",
                    "      AND p.param_name = 'temperature, air'",
                    "      AND mc.value IS NOT NULL",
                    paste0(
                        "      AND mc.datetime >= ",
                        historical_start_date_sql,
                        "::timestamp"
                    ),
                    "    GROUP BY l.location_code, DATE_TRUNC('day', mc.datetime)::date",
                    "),",
                    "daily_fdd AS (",
                    "    SELECT",
                    "        dt.location_code,",
                    "        dt.date,",
                    "        GREATEST(-dt.mean_temp, 0) AS fdd_day,",
                    "        CASE",
                    "            WHEN EXTRACT(MONTH FROM dt.date) >= 10",
                    "            THEN EXTRACT(YEAR FROM dt.date)::int",
                    "            ELSE EXTRACT(YEAR FROM dt.date)::int - 1",
                    "        END AS season_start_year",
                    "    FROM daily_temp dt",
                    "),",
                    "seasonal_fdd AS (",
                    "    SELECT",
                    "        df.location_code,",
                    "        df.date,",
                    "        1 +",
                    "            SUM(df.fdd_day) OVER (",
                    "                PARTITION BY df.location_code, df.season_start_year",
                    "                ORDER BY df.date",
                    "                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW",
                    "            ) -",
                    "            FIRST_VALUE(df.fdd_day) OVER (",
                    "                PARTITION BY df.location_code, df.season_start_year",
                    "                ORDER BY df.date",
                    "                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW",
                    "            ) AS value",
                    "    FROM daily_fdd df",
                    "),",
                    "daily_values AS (",
                    "    SELECT",
                    "        location_code,",
                    "        CASE",
                    "            WHEN EXTRACT(MONTH FROM date) > 2",
                    "                 AND (",
                    "                     (EXTRACT(YEAR FROM date)::int %% 4 = 0 AND EXTRACT(YEAR FROM date)::int %% 100 <> 0)",
                    "                     OR EXTRACT(YEAR FROM date)::int %% 400 = 0",
                    "                 )",
                    "            THEN EXTRACT(DOY FROM date)::int - 1",
                    "            ELSE EXTRACT(DOY FROM date)::int",
                    "        END AS doy,",
                    "        value",
                    "    FROM seasonal_fdd",
                    "    WHERE NOT (EXTRACT(MONTH FROM date) = 2 AND EXTRACT(DAY FROM date) = 29)",
                    "      AND value IS NOT NULL",
                    "),",
                    "by_doy AS (",
                    "    SELECT",
                    "        location_code,",
                    "        doy,",
                    "        COUNT(*) AS n_values,",
                    "        percentile_cont(0.00) WITHIN GROUP (ORDER BY value) AS p0,",
                    "        percentile_cont(0.10) WITHIN GROUP (ORDER BY value) AS p10,",
                    "        percentile_cont(0.25) WITHIN GROUP (ORDER BY value) AS p25,",
                    "        percentile_cont(0.50) WITHIN GROUP (ORDER BY value) AS p50,",
                    "        percentile_cont(0.75) WITHIN GROUP (ORDER BY value) AS p75,",
                    "        percentile_cont(0.90) WITHIN GROUP (ORDER BY value) AS p90,",
                    "        percentile_cont(1.00) WITHIN GROUP (ORDER BY value) AS p100",
                    "    FROM daily_values",
                    "    GROUP BY location_code, doy",
                    ")",
                    "SELECT",
                    "    lc.location_code,",
                    "    gs.doy,",
                    "    b.n_values,",
                    "    b.p0,",
                    "    b.p10,",
                    "    b.p25,",
                    "    b.p50,",
                    "    b.p75,",
                    "    b.p90,",
                    "    b.p100",
                    "FROM (SELECT DISTINCT location_code FROM daily_values) lc",
                    "CROSS JOIN generate_series(1, 365) AS gs(doy)",
                    "LEFT JOIN by_doy b ON b.location_code = lc.location_code AND b.doy = gs.doy",
                    "ORDER BY lc.location_code, gs.doy"
                ),
                location_codes_str
            ),
            con = con
        ))
    }

    if (identical(parameter, "DDT")) {
        return(YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "WITH daily_temp AS (",
                    "    SELECT",
                    "        l.location_code,",
                    "        DATE_TRUNC('day', mc.datetime)::date AS date,",
                    "        AVG(mc.value) AS mean_temp",
                    "    FROM measurements_continuous mc",
                    "    JOIN timeseries ts ON mc.timeseries_id = ts.timeseries_id",
                    "    JOIN locations l ON ts.location_id = l.location_id",
                    "    JOIN parameters p ON ts.parameter_id = p.parameter_id",
                    "    WHERE l.location_code IN (%s)",
                    "      AND p.param_name = 'temperature, air'",
                    "      AND mc.value IS NOT NULL",
                    paste0(
                        "      AND mc.datetime >= ",
                        historical_start_date_sql,
                        "::timestamp"
                    ),
                    "    GROUP BY l.location_code, DATE_TRUNC('day', mc.datetime)::date",
                    "),",
                    "daily_ddt AS (",
                    "    SELECT",
                    "        dt.location_code,",
                    "        dt.date,",
                    "        GREATEST(dt.mean_temp, 0) AS ddt_day,",
                    "        CASE",
                    "            WHEN EXTRACT(MONTH FROM dt.date) >= 4",
                    "            THEN EXTRACT(YEAR FROM dt.date)::int",
                    "            ELSE EXTRACT(YEAR FROM dt.date)::int - 1",
                    "        END AS season_start_year",
                    "    FROM daily_temp dt",
                    "),",
                    "seasonal_ddt AS (",
                    "    SELECT",
                    "        df.location_code,",
                    "        df.date,",
                    "        1 +",
                    "            SUM(df.ddt_day) OVER (",
                    "                PARTITION BY df.location_code, df.season_start_year",
                    "                ORDER BY df.date",
                    "                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW",
                    "            ) -",
                    "            FIRST_VALUE(df.ddt_day) OVER (",
                    "                PARTITION BY df.location_code, df.season_start_year",
                    "                ORDER BY df.date",
                    "                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW",
                    "            ) AS value",
                    "    FROM daily_ddt df",
                    "),",
                    "daily_values AS (",
                    "    SELECT",
                    "        location_code,",
                    "        CASE",
                    "            WHEN EXTRACT(MONTH FROM date) > 2",
                    "                 AND (",
                    "                     (EXTRACT(YEAR FROM date)::int %% 4 = 0 AND EXTRACT(YEAR FROM date)::int %% 100 <> 0)",
                    "                     OR EXTRACT(YEAR FROM date)::int %% 400 = 0",
                    "                 )",
                    "            THEN EXTRACT(DOY FROM date)::int - 1",
                    "            ELSE EXTRACT(DOY FROM date)::int",
                    "        END AS doy,",
                    "        value",
                    "    FROM seasonal_ddt",
                    "    WHERE NOT (EXTRACT(MONTH FROM date) = 2 AND EXTRACT(DAY FROM date) = 29)",
                    "      AND value IS NOT NULL",
                    "),",
                    "by_doy AS (",
                    "    SELECT",
                    "        location_code,",
                    "        doy,",
                    "        COUNT(*) AS n_values,",
                    "        percentile_cont(0.00) WITHIN GROUP (ORDER BY value) AS p0,",
                    "        percentile_cont(0.10) WITHIN GROUP (ORDER BY value) AS p10,",
                    "        percentile_cont(0.25) WITHIN GROUP (ORDER BY value) AS p25,",
                    "        percentile_cont(0.50) WITHIN GROUP (ORDER BY value) AS p50,",
                    "        percentile_cont(0.75) WITHIN GROUP (ORDER BY value) AS p75,",
                    "        percentile_cont(0.90) WITHIN GROUP (ORDER BY value) AS p90,",
                    "        percentile_cont(1.00) WITHIN GROUP (ORDER BY value) AS p100",
                    "    FROM daily_values",
                    "    GROUP BY location_code, doy",
                    ")",
                    "SELECT",
                    "    lc.location_code,",
                    "    gs.doy,",
                    "    b.n_values,",
                    "    b.p0,",
                    "    b.p10,",
                    "    b.p25,",
                    "    b.p50,",
                    "    b.p75,",
                    "    b.p90,",
                    "    b.p100",
                    "FROM (SELECT DISTINCT location_code FROM daily_values) lc",
                    "CROSS JOIN generate_series(1, 365) AS gs(doy)",
                    "LEFT JOIN by_doy b ON b.location_code = lc.location_code AND b.doy = gs.doy",
                    "ORDER BY lc.location_code, gs.doy"
                ),
                location_codes_str
            ),
            con = con
        ))
    }

    if (identical(parameter, "precipitation (1wk)")) {
        return(YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "WITH daily_values AS (",
                    "    SELECT",
                    "        l.location_code,",
                    "        mc.date,",
                    "        mc.value",
                    "    FROM measurements_calculated_daily mc",
                    "    JOIN timeseries ts ON mc.timeseries_id = ts.timeseries_id",
                    "    JOIN locations l ON ts.location_id = l.location_id",
                    "    JOIN parameters p ON ts.parameter_id = p.parameter_id",
                    "    WHERE l.location_code IN (%s)",
                    "      AND p.param_name = 'precipitation, total'",
                    paste0(
                        "      AND mc.date >= ",
                        historical_start_date_sql
                    ),
                    "      AND NOT (EXTRACT(MONTH FROM mc.date) = 2 AND EXTRACT(DAY FROM mc.date) = 29)",
                    "      AND mc.value IS NOT NULL",
                    "),",
                    "rolling_week AS (",
                    "    SELECT",
                    "        location_code,",
                    "        date,",
                    "        SUM(value) OVER (",
                    "            PARTITION BY location_code",
                    "            ORDER BY date",
                    "            ROWS BETWEEN 6 PRECEDING AND CURRENT ROW",
                    "        ) AS weekly_accumulation",
                    "    FROM daily_values",
                    "),",
                    "daily_weekly AS (",
                    "    SELECT",
                    "        location_code,",
                    "        CASE",
                    "            WHEN EXTRACT(MONTH FROM date) > 2",
                    "                 AND (",
                    "                     (EXTRACT(YEAR FROM date)::int %% 4 = 0 AND EXTRACT(YEAR FROM date)::int %% 100 <> 0)",
                    "                     OR EXTRACT(YEAR FROM date)::int %% 400 = 0",
                    "                 )",
                    "            THEN EXTRACT(DOY FROM date)::int - 1",
                    "            ELSE EXTRACT(DOY FROM date)::int",
                    "        END AS doy,",
                    "        weekly_accumulation AS value",
                    "    FROM rolling_week",
                    "),",
                    "by_doy AS (",
                    "    SELECT",
                    "        location_code,",
                    "        doy,",
                    "        COUNT(*) AS n_values,",
                    "        percentile_cont(0.00) WITHIN GROUP (ORDER BY value) AS p0,",
                    "        percentile_cont(0.10) WITHIN GROUP (ORDER BY value) AS p10,",
                    "        percentile_cont(0.25) WITHIN GROUP (ORDER BY value) AS p25,",
                    "        percentile_cont(0.50) WITHIN GROUP (ORDER BY value) AS p50,",
                    "        percentile_cont(0.75) WITHIN GROUP (ORDER BY value) AS p75,",
                    "        percentile_cont(0.90) WITHIN GROUP (ORDER BY value) AS p90,",
                    "        percentile_cont(1.00) WITHIN GROUP (ORDER BY value) AS p100",
                    "    FROM daily_weekly",
                    "    GROUP BY location_code, doy",
                    ")",
                    "SELECT",
                    "    lc.location_code,",
                    "    gs.doy,",
                    "    b.n_values,",
                    "    b.p0,",
                    "    b.p10,",
                    "    b.p25,",
                    "    b.p50,",
                    "    b.p75,",
                    "    b.p90,",
                    "    b.p100",
                    "FROM (SELECT DISTINCT location_code FROM daily_weekly) lc",
                    "CROSS JOIN generate_series(1, 365) AS gs(doy)",
                    "LEFT JOIN by_doy b ON b.location_code = lc.location_code AND b.doy = gs.doy",
                    "ORDER BY lc.location_code, gs.doy"
                ),
                location_codes_str
            ),
            con = con
        ))
    }

    if (identical(parameter, "precipitation (24hr)")) {
        return(YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "WITH daily_values AS (",
                    "    SELECT",
                    "        l.location_code,",
                    "        CASE",
                    "            WHEN EXTRACT(MONTH FROM mc.date) > 2",
                    "                 AND (",
                    "                     (EXTRACT(YEAR FROM mc.date)::int %% 4 = 0 AND EXTRACT(YEAR FROM mc.date)::int %% 100 <> 0)",
                    "                     OR EXTRACT(YEAR FROM mc.date)::int %% 400 = 0",
                    "                 )",
                    "            THEN EXTRACT(DOY FROM mc.date)::int - 1",
                    "            ELSE EXTRACT(DOY FROM mc.date)::int",
                    "        END AS doy,",
                    "        mc.value",
                    "    FROM measurements_calculated_daily mc",
                    "    JOIN timeseries ts ON mc.timeseries_id = ts.timeseries_id",
                    "    JOIN locations l ON ts.location_id = l.location_id",
                    "    JOIN parameters p ON ts.parameter_id = p.parameter_id",
                    "    WHERE l.location_code IN (%s)",
                    "      AND p.param_name = 'precipitation, total'",
                    paste0(
                        "      AND mc.date >= ",
                        historical_start_date_sql
                    ),
                    "      AND NOT (EXTRACT(MONTH FROM mc.date) = 2 AND EXTRACT(DAY FROM mc.date) = 29)",
                    "      AND mc.value IS NOT NULL",
                    "),",
                    "by_doy AS (",
                    "    SELECT",
                    "        location_code,",
                    "        doy,",
                    "        COUNT(*) AS n_values,",
                    "        percentile_cont(0.00) WITHIN GROUP (ORDER BY value) AS p0,",
                    "        percentile_cont(0.10) WITHIN GROUP (ORDER BY value) AS p10,",
                    "        percentile_cont(0.25) WITHIN GROUP (ORDER BY value) AS p25,",
                    "        percentile_cont(0.50) WITHIN GROUP (ORDER BY value) AS p50,",
                    "        percentile_cont(0.75) WITHIN GROUP (ORDER BY value) AS p75,",
                    "        percentile_cont(0.90) WITHIN GROUP (ORDER BY value) AS p90,",
                    "        percentile_cont(1.00) WITHIN GROUP (ORDER BY value) AS p100",
                    "    FROM daily_values",
                    "    GROUP BY location_code, doy",
                    ")",
                    "SELECT",
                    "    lc.location_code,",
                    "    gs.doy,",
                    "    b.n_values,",
                    "    b.p0,",
                    "    b.p10,",
                    "    b.p25,",
                    "    b.p50,",
                    "    b.p75,",
                    "    b.p90,",
                    "    b.p100",
                    "FROM (SELECT DISTINCT location_code FROM daily_values) lc",
                    "CROSS JOIN generate_series(1, 365) AS gs(doy)",
                    "LEFT JOIN by_doy b ON b.location_code = lc.location_code AND b.doy = gs.doy",
                    "ORDER BY lc.location_code, gs.doy"
                ),
                location_codes_str
            ),
            con = con
        ))
    }

    YGwater::dbGetQueryDT(
        sprintf(
            paste(
                "WITH daily_values AS (",
                "    SELECT",
                "        l.location_code,",
                "        CASE",
                "            WHEN EXTRACT(MONTH FROM mc.date) > 2",
                "                 AND (",
                "                     (EXTRACT(YEAR FROM mc.date)::int %% 4 = 0 AND EXTRACT(YEAR FROM mc.date)::int %% 100 <> 0)",
                "                     OR EXTRACT(YEAR FROM mc.date)::int %% 400 = 0",
                "                 )",
                "            THEN EXTRACT(DOY FROM mc.date)::int - 1",
                "            ELSE EXTRACT(DOY FROM mc.date)::int",
                "        END AS doy,",
                "        mc.value",
                "    FROM measurements_calculated_daily mc",
                "    JOIN timeseries ts ON mc.timeseries_id = ts.timeseries_id",
                "    JOIN locations l ON ts.location_id = l.location_id",
                "    JOIN parameters p ON ts.parameter_id = p.parameter_id",
                "    WHERE l.location_code IN (%s)",
                "      AND p.param_name = '%s'",
                paste0(
                    "      AND mc.date >= ",
                    historical_start_date_sql
                ),
                "      AND NOT (EXTRACT(MONTH FROM mc.date) = 2 AND EXTRACT(DAY FROM mc.date) = 29)",
                "      AND mc.value IS NOT NULL",
                "),",
                "by_doy AS (",
                "    SELECT",
                "        location_code,",
                "        doy,",
                "        COUNT(*) AS n_values,",
                "        percentile_cont(0.00) WITHIN GROUP (ORDER BY value) AS p0,",
                "        percentile_cont(0.10) WITHIN GROUP (ORDER BY value) AS p10,",
                "        percentile_cont(0.25) WITHIN GROUP (ORDER BY value) AS p25,",
                "        percentile_cont(0.50) WITHIN GROUP (ORDER BY value) AS p50,",
                "        percentile_cont(0.75) WITHIN GROUP (ORDER BY value) AS p75,",
                "        percentile_cont(0.90) WITHIN GROUP (ORDER BY value) AS p90,",
                "        percentile_cont(1.00) WITHIN GROUP (ORDER BY value) AS p100",
                "    FROM daily_values",
                "    GROUP BY location_code, doy",
                ")",
                "SELECT",
                "    lc.location_code,",
                "    gs.doy,",
                "    b.n_values,",
                "    b.p0,",
                "    b.p10,",
                "    b.p25,",
                "    b.p50,",
                "    b.p75,",
                "    b.p90,",
                "    b.p100",
                "FROM (SELECT DISTINCT location_code FROM daily_values) lc",
                "CROSS JOIN generate_series(1, 365) AS gs(doy)",
                "LEFT JOIN by_doy b ON b.location_code = lc.location_code AND b.doy = gs.doy",
                "ORDER BY lc.location_code, gs.doy"
            ),
            location_codes_str,
            parameter
        ),
        con = con
    )
}

get_parameter_percentile_limits <- function(
    location_code,
    parameter,
    con,
    historical_start_year = 2020
) {
    if (is.na(location_code) || !nzchar(location_code)) {
        return(list(p10 = NA_real_, p90 = NA_real_))
    }

    location_sql <- DBI::dbQuoteString(con, location_code)
    historical_start_year <- normalize_historical_start_year(
        historical_start_year
    )
    historical_start_date_sql <- paste0(
        "MAKE_DATE(",
        historical_start_year,
        ", 1, 1)"
    )

    query_result <- if (identical(parameter, "FDD")) {
        YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "WITH daily_temp AS (",
                    "    SELECT",
                    "        DATE_TRUNC('day', mc.datetime)::date AS date,",
                    "        AVG(mc.value) AS mean_temp",
                    "    FROM measurements_continuous mc",
                    "    JOIN timeseries ts ON mc.timeseries_id = ts.timeseries_id",
                    "    JOIN parameters p ON ts.parameter_id = p.parameter_id",
                    "    JOIN locations l ON ts.location_id = l.location_id",
                    "    WHERE l.location_code = %s",
                    "      AND p.param_name = 'temperature, air'",
                    "      AND mc.value IS NOT NULL",
                    paste0(
                        "      AND mc.datetime >= ",
                        historical_start_date_sql,
                        "::timestamp"
                    ),
                    "    GROUP BY DATE_TRUNC('day', mc.datetime)::date",
                    "),",
                    "daily_fdd AS (",
                    "    SELECT",
                    "        dt.date,",
                    "        GREATEST(-dt.mean_temp, 0) AS fdd_day,",
                    "        CASE",
                    "            WHEN EXTRACT(MONTH FROM dt.date) >= 10",
                    "            THEN EXTRACT(YEAR FROM dt.date)::int",
                    "            ELSE EXTRACT(YEAR FROM dt.date)::int - 1",
                    "        END AS season_start_year",
                    "    FROM daily_temp dt",
                    "),",
                    "seasonal_fdd AS (",
                    "    SELECT",
                    "        1 +",
                    "            SUM(df.fdd_day) OVER (",
                    "                PARTITION BY df.season_start_year",
                    "                ORDER BY df.date",
                    "                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW",
                    "            ) -",
                    "            FIRST_VALUE(df.fdd_day) OVER (",
                    "                PARTITION BY df.season_start_year",
                    "                ORDER BY df.date",
                    "                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW",
                    "            ) AS value",
                    "    FROM daily_fdd df",
                    ")",
                    "SELECT",
                    "    percentile_cont(0.10) WITHIN GROUP (ORDER BY value) AS p10,",
                    "    percentile_cont(0.90) WITHIN GROUP (ORDER BY value) AS p90",
                    "FROM seasonal_fdd",
                    "WHERE value IS NOT NULL"
                ),
                location_sql
            ),
            con = con
        )
    } else if (identical(parameter, "DDT")) {
        YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "WITH daily_temp AS (",
                    "    SELECT",
                    "        DATE_TRUNC('day', mc.datetime)::date AS date,",
                    "        AVG(mc.value) AS mean_temp",
                    "    FROM measurements_continuous mc",
                    "    JOIN timeseries ts ON mc.timeseries_id = ts.timeseries_id",
                    "    JOIN parameters p ON ts.parameter_id = p.parameter_id",
                    "    JOIN locations l ON ts.location_id = l.location_id",
                    "    WHERE l.location_code = %s",
                    "      AND p.param_name = 'temperature, air'",
                    "      AND mc.value IS NOT NULL",
                    paste0(
                        "      AND mc.datetime >= ",
                        historical_start_date_sql,
                        "::timestamp"
                    ),
                    "    GROUP BY DATE_TRUNC('day', mc.datetime)::date",
                    "),",
                    "daily_ddt AS (",
                    "    SELECT",
                    "        dt.date,",
                    "        GREATEST(dt.mean_temp, 0) AS ddt_day,",
                    "        CASE",
                    "            WHEN EXTRACT(MONTH FROM dt.date) >= 4",
                    "            THEN EXTRACT(YEAR FROM dt.date)::int",
                    "            ELSE EXTRACT(YEAR FROM dt.date)::int - 1",
                    "        END AS season_start_year",
                    "    FROM daily_temp dt",
                    "),",
                    "seasonal_ddt AS (",
                    "    SELECT",
                    "        1 +",
                    "            SUM(df.ddt_day) OVER (",
                    "                PARTITION BY df.season_start_year",
                    "                ORDER BY df.date",
                    "                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW",
                    "            ) -",
                    "            FIRST_VALUE(df.ddt_day) OVER (",
                    "                PARTITION BY df.season_start_year",
                    "                ORDER BY df.date",
                    "                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW",
                    "            ) AS value",
                    "    FROM daily_ddt df",
                    ")",
                    "SELECT",
                    "    percentile_cont(0.10) WITHIN GROUP (ORDER BY value) AS p10,",
                    "    percentile_cont(0.90) WITHIN GROUP (ORDER BY value) AS p90",
                    "FROM seasonal_ddt",
                    "WHERE value IS NOT NULL"
                ),
                location_sql
            ),
            con = con
        )
    } else if (identical(parameter, "precipitation (1wk)")) {
        YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "WITH daily_values AS (",
                    "    SELECT mc.date, mc.value",
                    "    FROM measurements_calculated_daily mc",
                    "    JOIN timeseries ts ON mc.timeseries_id = ts.timeseries_id",
                    "    JOIN parameters p ON ts.parameter_id = p.parameter_id",
                    "    JOIN locations l ON ts.location_id = l.location_id",
                    "    WHERE l.location_code = %s",
                    "      AND p.param_name = 'precipitation, total'",
                    paste0(
                        "      AND mc.date >= ",
                        historical_start_date_sql
                    ),
                    "      AND mc.value IS NOT NULL",
                    "),",
                    "rolling_week AS (",
                    "    SELECT",
                    "        SUM(value) OVER (",
                    "            ORDER BY date",
                    "            ROWS BETWEEN 6 PRECEDING AND CURRENT ROW",
                    "        ) AS value",
                    "    FROM daily_values",
                    ")",
                    "SELECT",
                    "    percentile_cont(0.10) WITHIN GROUP (ORDER BY value) AS p10,",
                    "    percentile_cont(0.90) WITHIN GROUP (ORDER BY value) AS p90",
                    "FROM rolling_week",
                    "WHERE value IS NOT NULL"
                ),
                location_sql
            ),
            con = con
        )
    } else if (identical(parameter, "precipitation (24hr)")) {
        YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "SELECT",
                    "    percentile_cont(0.10) WITHIN GROUP (ORDER BY mc.value) AS p10,",
                    "    percentile_cont(0.90) WITHIN GROUP (ORDER BY mc.value) AS p90",
                    "FROM measurements_calculated_daily mc",
                    "JOIN timeseries ts ON mc.timeseries_id = ts.timeseries_id",
                    "JOIN parameters p ON ts.parameter_id = p.parameter_id",
                    "JOIN locations l ON ts.location_id = l.location_id",
                    "WHERE l.location_code = %s",
                    "  AND p.param_name = 'precipitation, total'",
                    paste0(
                        "  AND mc.date >= ",
                        historical_start_date_sql
                    ),
                    "  AND mc.value IS NOT NULL"
                ),
                location_sql
            ),
            con = con
        )
    } else {
        YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "SELECT",
                    "    percentile_cont(0.10) WITHIN GROUP (ORDER BY mc.value) AS p10,",
                    "    percentile_cont(0.90) WITHIN GROUP (ORDER BY mc.value) AS p90",
                    "FROM measurements_calculated_daily mc",
                    "JOIN timeseries ts ON mc.timeseries_id = ts.timeseries_id",
                    "JOIN parameters p ON ts.parameter_id = p.parameter_id",
                    "JOIN locations l ON ts.location_id = l.location_id",
                    "WHERE l.location_code = %s",
                    "  AND p.param_name = %s",
                    paste0(
                        "  AND mc.date >= ",
                        historical_start_date_sql
                    ),
                    "  AND mc.value IS NOT NULL"
                ),
                location_sql,
                DBI::dbQuoteString(con, parameter)
            ),
            con = con
        )
    }

    if (is.null(query_result) || nrow(query_result) == 0) {
        return(list(p10 = NA_real_, p90 = NA_real_))
    }

    list(
        p10 = suppressWarnings(as.numeric(query_result$p10[[1]])),
        p90 = suppressWarnings(as.numeric(query_result$p90[[1]]))
    )
}

#' Retrieve Continuous Timeseries Data
#'
#' Fetches raw continuous measurements for a parameter over a date window,
#' defaulting to the latest 30 days when no dates are provided.
#'
#' @param location_codes Character vector of location codes.
#' @param parameter Parameter name.
#' @param start_date Optional POSIXct-compatible start.
#' @param end_date Optional POSIXct-compatible end.
#' @param con DBI connection object.
#'
#' @return Data.frame of continuous observations.
#' @noRd
get_continuous_data <- function(
    location_codes,
    parameter = "water flow",
    start_date = NULL,
    end_date = NULL,
    con
) {
    location_codes_str <- paste(sprintf("'%s'", location_codes), collapse = ",")

    if (is.null(end_date)) {
        latest_dt <- YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "SELECT MAX(mc.datetime) AS end_date",
                    "FROM measurements_continuous mc",
                    "JOIN timeseries ts ON mc.timeseries_id = ts.timeseries_id",
                    "JOIN parameters p ON ts.parameter_id = p.parameter_id",
                    "JOIN locations l ON ts.location_id = l.location_id",
                    "WHERE l.location_code IN (%s)",
                    "AND p.param_name = '%s'",
                    "AND ts.record_rate::text = '00:05:00'"
                ),
                location_codes_str,
                parameter
            ),
            con = con
        )
        end_date <- latest_dt$end_date[1]
    }

    if (is.null(start_date)) {
        start_date <- Sys.time() - as.difftime(30, units = "days")
    }

    start_date <- as.POSIXct(start_date)
    end_date <- as.POSIXct(end_date)

    if (is.na(end_date)) {
        stop("No continuous data available to determine default end_date")
    }

    if (is.na(start_date)) {
        stop("Invalid start_date")
    }

    if (start_date > end_date) {
        stop("start_date must be earlier than or equal to end_date")
    }

    start_date_sql <- format(start_date, "%Y-%m-%d %H:%M:%S")
    end_date_sql <- format(end_date, "%Y-%m-%d %H:%M:%S")

    dat <- YGwater::dbGetQueryDT(
        sprintf(
            paste(
                "SELECT ts.location_id, l.location_code, ts.timeseries_id, mc.datetime, mc.value AS value",
                "FROM measurements_continuous mc",
                "JOIN timeseries ts ON mc.timeseries_id = ts.timeseries_id",
                "JOIN parameters p ON ts.parameter_id = p.parameter_id",
                "JOIN locations l ON ts.location_id = l.location_id",
                "WHERE l.location_code IN (%s)",
                "AND p.param_name = '%s'",
                "AND ts.record_rate::text = '00:05:00'",
                "AND mc.datetime >= '%s'::timestamp",
                "AND mc.datetime <= '%s'::timestamp",
                "AND mc.value IS NOT NULL",
                "ORDER BY ts.location_id, ts.timeseries_id, mc.datetime"
            ),
            location_codes_str,
            parameter,
            start_date_sql,
            end_date_sql
        ),
        con = con
    )

    interpret_loaded_times_as_local(dat, time_columns = c("datetime"))
}

# Plot construction ---------------------------------------------------------

#' Build Station Timeseries Plot
#'
#' Creates a Plotly figure with observed data, percentile ribbons, optional
#' return-period lines, and a vertical reference-time marker.
#'
#' @param location_code Character location code.
#' @param continuous_data Optional preloaded series data.
#' @param percentiles Optional preloaded percentile data.
#' @param return_periods Optional preloaded return-period data.
#' @param parameter Parameter name.
#' @param return_period_values Numeric return-period vector.
#' @param start_date Optional plot start datetime.
#' @param end_date Optional plot end datetime.
#' @param view_start_ago Character timespan for default start.
#' @param view_end_ahead Optional timespan extending default end.
#' @param reference_time Optional POSIXct reference timestamp.
#' @param load_entire_record Logical; use full record extent.
#' @param con DBI connection object when data is not preloaded.
#' @param historical_start_year Integer year used as the lower bound for
#'   historical daily values and percentile envelopes.
#'
#' @return A Plotly htmlwidget.
#' @noRd
plot_continuous_with_percentiles_and_return_periods <- function(
    location_code,
    continuous_data = NULL,
    percentiles = NULL,
    return_periods = NULL,
    parameter = "water flow",
    return_period_values = c(2, 5, 10, 25, 50, 100),
    start_date = NULL,
    end_date = NULL,
    view_start_ago = "7 days",
    view_end_ahead = NULL,
    reference_time = NULL,
    load_entire_record = FALSE,
    con = NULL,
    historical_start_year = 2020
) {
    if (
        is.null(con) &&
            (is.null(continuous_data) ||
                is.null(percentiles) ||
                is.null(return_periods))
    ) {
        stop("Provide `con` when data inputs are NULL")
    }

    include_return_periods <- parameter %in% c("water flow", "water level")
    include_percentiles <- parameter %in%
        c(
            "water flow",
            "water level",
            "precipitation (1wk)",
            "precipitation (24hr)",
            "temperature, air",
            "FDD",
            "DDT"
        )
    historical_start_year <- normalize_historical_start_year(
        historical_start_year
    )

    parse_timespan_seconds <- function(x, arg_name) {
        if (inherits(x, "difftime")) {
            return(as.numeric(x, units = "secs"))
        }

        if (is.numeric(x) && length(x) == 1 && !is.na(x)) {
            return(as.numeric(x))
        }

        if (!is.character(x) || length(x) != 1) {
            stop(sprintf("%s must be a timespan like '7 days'", arg_name))
        }

        m <- regexec(
            "^\\s*([0-9]*\\.?[0-9]+)\\s*(second|seconds|minute|minutes|hour|hours|day|days|week|weeks)\\s*$",
            x,
            ignore.case = TRUE
        )
        parts <- regmatches(x, m)[[1]]
        if (length(parts) != 3) {
            stop(sprintf("%s must be a timespan like '7 days'", arg_name))
        }

        value <- as.numeric(parts[2])
        unit <- tolower(parts[3])
        multiplier <- switch(
            unit,
            second = 1,
            seconds = 1,
            minute = 60,
            minutes = 60,
            hour = 3600,
            hours = 3600,
            day = 86400,
            days = 86400,
            week = 604800,
            weeks = 604800,
            stop(sprintf("Unsupported time unit in %s", arg_name))
        )

        value * multiplier
    }

    safe_as_posix <- function(x, tz) {
        tryCatch(
            suppressWarnings(as.POSIXct(
                x,
                tz = tz,
                tryFormats = c(
                    "%Y-%m-%d %H:%M:%S",
                    "%Y-%m-%d %H:%M",
                    "%Y-%m-%dT%H:%M:%S",
                    "%Y-%m-%dT%H:%M",
                    "%Y-%m-%d"
                )
            )),
            error = function(e) as.POSIXct(NA, tz = tz)
        )
    }

    now_ts <- if (is.null(reference_time)) {
        Sys.time()
    } else {
        safe_as_posix(reference_time, tz = "America/Whitehorse")
    }
    if (is.na(now_ts)) {
        now_ts <- Sys.time()
    }

    default_view_start <- now_ts - as.difftime(7, units = "days")
    default_view_end <- seq(now_ts, by = "2 months", length.out = 2)[2]

    if (!is.null(start_date)) {
        view_start <- safe_as_posix(start_date, tz = "America/Whitehorse")
    } else {
        view_start <- default_view_start
    }

    if (!is.null(end_date)) {
        view_end <- safe_as_posix(end_date, tz = "America/Whitehorse")
    } else if (!is.null(view_end_ahead)) {
        view_end <- now_ts +
            parse_timespan_seconds(view_end_ahead, "view_end_ahead")
    } else {
        view_end <- default_view_end
    }
    if (is.na(view_start)) {
        view_start <- default_view_start
    }
    if (is.na(view_end)) {
        view_end <- default_view_end
    }

    if (is.null(continuous_data)) {
        continuous_data <- get_station_timeseries(
            location_code = location_code,
            parameter = parameter,
            reference_time = now_ts,
            load_entire_record = load_entire_record,
            con = con,
            historical_start_year = historical_start_year
        )
    }
    if (!"location_code" %in% names(continuous_data)) {
        continuous_data$location_code <- location_code
    }

    series <- continuous_data[continuous_data$location_code == location_code, ]
    if (nrow(series) == 0) {
        return(
            empty_plotly_widget(
                title = sprintf(
                    "No data for %s (%s)",
                    location_code,
                    parameter
                )
            )
        )
    }

    plot_timezone <- attr(series$datetime, "tzone")
    if (
        length(plot_timezone) == 0 ||
            is.na(plot_timezone[[1]]) ||
            !nzchar(plot_timezone[[1]])
    ) {
        plot_timezone <- "America/Whitehorse"
    } else {
        plot_timezone <- plot_timezone[[1]]
    }

    coerce_to_plot_tz <- function(x) {
        x <- safe_as_posix(x, tz = plot_timezone)
        attr(x, "tzone") <- plot_timezone
        x
    }

    series$datetime <- coerce_to_plot_tz(series$datetime)
    now_ts <- coerce_to_plot_tz(now_ts)
    view_start <- coerce_to_plot_tz(view_start)
    view_end <- coerce_to_plot_tz(view_end)

    # Ensure both the latest observation and reference time are inside the x-window.
    latest_obs <- max(series$datetime, na.rm = TRUE)
    if (is.finite(latest_obs)) {
        view_start <- min(view_start, latest_obs, now_ts)
        view_end <- max(view_end, latest_obs, now_ts)
    } else {
        view_start <- min(view_start, now_ts)
        view_end <- max(view_end, now_ts)
    }

    if (isTRUE(load_entire_record)) {
        series_start <- min(series$datetime, na.rm = TRUE)
        series_end <- max(series$datetime, na.rm = TRUE)

        if (is.finite(series_start)) {
            view_start <- min(view_start, series_start)
        }
        if (is.finite(series_end)) {
            view_end <- max(view_end, series_end, now_ts)
        }
    }

    pct <- data.frame()
    envelope_xlim <- NULL
    if (include_percentiles) {
        if (is.null(percentiles)) {
            percentiles <- get_daily_percentiles(
                location_codes = c(location_code),
                parameter = parameter,
                con = con,
                historical_start_year = historical_start_year
            )
        }
        pct <- percentiles[percentiles$location_code == location_code, ]
    }

    p <- plotly::plot_ly()

    if (include_percentiles && nrow(pct) > 0) {
        # Extend the historical envelope from the selected start year through
        # the end of reference year + 1.
        ref_year <- suppressWarnings(as.integer(
            format(now_ts, "%Y", tz = plot_timezone)
        ))
        if (is.na(ref_year)) {
            ref_year <- as.integer(format(Sys.time(), "%Y", tz = plot_timezone))
        }
        percentile_cycle_start_date <- as.Date(sprintf(
            "%d-01-01",
            historical_start_year
        ))
        percentile_cycle_end_date <- as.Date(
            sprintf("%d-12-31", ref_year + 1L)
        )
        percentile_dates <- seq(
            percentile_cycle_start_date,
            percentile_cycle_end_date,
            by = "day"
        )
        pct_years <- as.integer(format(percentile_dates, "%Y"))
        pct_months <- as.integer(format(percentile_dates, "%m"))
        pct_days <- as.integer(format(percentile_dates, "%d"))
        pct_doys <- as.integer(format(percentile_dates, "%j"))
        pct_is_leap <- (pct_years %% 4 == 0 & pct_years %% 100 != 0) |
            (pct_years %% 400 == 0)

        pct_plot <- data.frame(
            location_code = location_code,
            datetime = as.POSIXct(percentile_dates, tz = plot_timezone),
            doy = ifelse(pct_months > 2 & pct_is_leap, pct_doys - 1L, pct_doys),
            stringsAsFactors = FALSE
        )
        pct_plot$doy[pct_months == 2 & pct_days == 29] <- NA_integer_
        pct_lookup <- pct[, c(
            "location_code",
            "doy",
            "p0",
            "p10",
            "p25",
            "p50",
            "p75",
            "p90",
            "p100"
        )]
        pct_key <- paste(pct_lookup$location_code, pct_lookup$doy)
        plot_key <- paste(pct_plot$location_code, pct_plot$doy)
        match_idx <- match(plot_key, pct_key)
        for (col in c("p0", "p10", "p25", "p50", "p75", "p90", "p100")) {
            pct_plot[[col]] <- pct_lookup[[col]][match_idx]
        }
        pct_plot <- pct_plot[order(pct_plot$datetime), ]

        # Prevent ribbon self-clipping caused by NA gaps in percentile bands.
        pct_plot <- pct_plot[
            stats::complete.cases(pct_plot[, c(
                "datetime",
                "p0",
                "p10",
                "p25",
                "p50",
                "p75",
                "p90",
                "p100"
            )]),
        ]

        if (nrow(pct_plot) > 0) {
            envelope_xlim <- range(pct_plot$datetime, na.rm = TRUE)
            if (any(!is.finite(envelope_xlim))) {
                envelope_xlim <- NULL
            }
        }

        p <- p %>%
            plotly::add_ribbons(
                data = pct_plot,
                x = ~datetime,
                ymin = ~p0,
                ymax = ~p10,
                name = "P0-P10",
                legendrank = 80,
                hovertemplate = "P0-P10<extra></extra>",
                fillcolor = "rgba(127, 29, 29, 0.3)",
                line = list(color = "transparent")
            ) %>%
            plotly::add_ribbons(
                data = pct_plot,
                x = ~datetime,
                ymin = ~p10,
                ymax = ~p25,
                name = "P10-P25",
                legendrank = 70,
                hovertemplate = "P10-P25<extra></extra>",
                fillcolor = "rgba(180, 83, 9, 0.3)",
                line = list(color = "transparent")
            ) %>%
            plotly::add_ribbons(
                data = pct_plot,
                x = ~datetime,
                ymin = ~p25,
                ymax = ~p50,
                name = "P25-P50",
                legendrank = 60,
                hovertemplate = "P25-P50<extra></extra>",
                fillcolor = "rgba(245, 158, 11, 0.3)",
                line = list(color = "transparent")
            ) %>%
            plotly::add_ribbons(
                data = pct_plot,
                x = ~datetime,
                ymin = ~p50,
                ymax = ~p75,
                name = "P50-P75",
                legendrank = 50,
                hovertemplate = "P50-P75<extra></extra>",
                fillcolor = "rgba(163, 230, 53, 0.3)",
                line = list(color = "transparent")
            ) %>%
            plotly::add_ribbons(
                data = pct_plot,
                x = ~datetime,
                ymin = ~p75,
                ymax = ~p90,
                name = "P75-P90",
                legendrank = 40,
                hovertemplate = "P75-P90<extra></extra>",
                fillcolor = "rgba(34, 197, 94, 0.3)",
                line = list(color = "transparent")
            ) %>%
            plotly::add_ribbons(
                data = pct_plot,
                x = ~datetime,
                ymin = ~p90,
                ymax = ~p100,
                name = "P90-P100",
                legendrank = 30,
                hovertemplate = "P90-P100<extra></extra>",
                fillcolor = "rgba(15, 118, 110, 0.3)",
                line = list(color = "transparent")
            )
    }

    observed_name <- if (identical(parameter, "precipitation (1wk)")) {
        "Observed 1-week accumulation"
    } else if (identical(parameter, "precipitation (24hr)")) {
        "Observed 24-hour"
    } else {
        "Observed"
    }

    hover_datetime_fmt <- "%Y-%m-%d"

    y_axis_title <- parameter_axis_title(parameter)

    if (!"trace_source" %in% names(series)) {
        series$trace_source <- "observed"
    }

    if (isTRUE(load_entire_record)) {
        historical_series <- series[series$trace_source == "historical_daily", ]
        realtime_series <- series[
            series$trace_source == "realtime_continuous",
        ]

        if (nrow(historical_series) > 0) {
            p <- p %>%
                plotly::add_lines(
                    data = historical_series,
                    x = ~datetime,
                    y = ~value,
                    name = "Daily observed",
                    legendrank = 20,
                    hovertemplate = "Daily observed: %{y:.3f}<extra></extra>",
                    line = list(color = "#4b5563", width = 1)
                )
        }

        if (nrow(realtime_series) > 0) {
            p <- p %>%
                plotly::add_lines(
                    data = realtime_series,
                    x = ~datetime,
                    y = ~value,
                    name = observed_name,
                    legendrank = 10,
                    hovertemplate = paste0(
                        observed_name,
                        ": %{y:.3f}<extra></extra>"
                    ),
                    line = list(color = "#000000", width = 2)
                )
        }

        if (nrow(historical_series) == 0 && nrow(realtime_series) == 0) {
            p <- p %>%
                plotly::add_lines(
                    data = series,
                    x = ~datetime,
                    y = ~value,
                    name = observed_name,
                    legendrank = 10,
                    hovertemplate = paste0(
                        observed_name,
                        ": %{y:.3f}<extra></extra>"
                    ),
                    line = list(color = "#000000", width = 2)
                )
        }
    } else {
        p <- p %>%
            plotly::add_lines(
                data = series,
                x = ~datetime,
                y = ~value,
                name = observed_name,
                legendrank = 10,
                hovertemplate = paste0(
                    observed_name,
                    ": %{y:.3f}<extra></extra>"
                ),
                line = list(color = "#000000", width = 1)
            )
    }

    rp_shapes <- list()
    rp_annotations <- list()

    if (include_return_periods) {
        if (is.null(return_periods)) {
            return_periods <- get_return_period_discharge(
                location_codes = c(location_code),
                parameter = parameter,
                return_periods = return_period_values,
                con = con
            )
        }
        rp <- return_periods[return_periods$location_code == location_code, ]

        if (nrow(rp) > 0) {
            rp_cols <- grep("^rp_", names(rp), value = TRUE)
            if (length(rp_cols) > 0) {
                min_x <- min(series$datetime, na.rm = TRUE)
                max_x <- max(series$datetime, na.rm = TRUE)
                rp_x0 <- if (!is.null(envelope_xlim)) {
                    envelope_xlim[[1]]
                } else {
                    max(min_x, view_start)
                }
                rp_x1 <- if (!is.null(envelope_xlim)) {
                    envelope_xlim[[2]]
                } else {
                    min(max_x, view_end)
                }
                rp_label_x_positions <- 0.995 - c(0, 0.03, 0.06, 0.09)
                for (i in seq_along(rp_cols)) {
                    rp_col <- rp_cols[i]
                    rp_years <- sub("^rp_", "", rp_col)
                    rp_shapes[[i]] <- list(
                        type = "line",
                        x0 = rp_x0,
                        x1 = rp_x1,
                        y0 = rp[[rp_col]][1],
                        y1 = rp[[rp_col]][1],
                        line = list(color = "#dc2626", width = 1)
                    )
                    rp_annotations[[i]] <- list(
                        x = rp_label_x_positions[
                            ((i - 1) %% length(rp_label_x_positions)) + 1
                        ],
                        y = rp[[rp_col]][1],
                        xref = "paper",
                        yref = "y",
                        text = sprintf("RP %s", rp_years),
                        showarrow = FALSE,
                        xanchor = "right",
                        align = "right",
                        bgcolor = "rgba(255, 255, 255, 0.85)",
                        bordercolor = "rgba(220, 38, 38, 0.35)",
                        borderwidth = 1
                    )
                }
            }
        }
    }

    now_line <- list(
        type = "line",
        x0 = now_ts,
        x1 = now_ts,
        xref = "x",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        line = list(color = "#9ca3af", dash = "dash")
    )

    all_shapes <- c(list(now_line), rp_shapes)

    p %>%
        plotly::layout(
            xaxis = list(
                title = "Date",
                range = c(view_start, view_end),
                hoverformat = hover_datetime_fmt
            ),
            yaxis = list(title = y_axis_title),
            hovermode = "x unified",
            showlegend = FALSE,
            shapes = all_shapes,
            annotations = rp_annotations,
            legend = list(orientation = "v", x = 0.01, y = 0.99),
            margin = list(r = 80)
        )
}

simple_continuous_plotly_widget <- function(
    location_code,
    parameter,
    continuous_data,
    title_prefix = NULL
) {
    if (is.null(continuous_data) || nrow(continuous_data) == 0) {
        return(empty_plotly_widget(
            title = title_prefix,
            annotations = list(list(
                text = sprintf("No data available for %s.", location_code),
                x = 0.5,
                y = 0.5,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE
            ))
        ))
    }

    series <- continuous_data
    if ("location_code" %in% names(series)) {
        series <- series[series$location_code == location_code, , drop = FALSE]
    }

    required_cols <- c("datetime", "value")
    if (nrow(series) == 0 || !all(required_cols %in% names(series))) {
        return(empty_plotly_widget(
            title = title_prefix,
            annotations = list(list(
                text = sprintf(
                    "No plottable data available for %s.",
                    location_code
                ),
                x = 0.5,
                y = 0.5,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE
            ))
        ))
    }

    plot_timezone <- attr(series$datetime, "tzone")
    if (
        length(plot_timezone) == 0 ||
            is.na(plot_timezone[[1]]) ||
            !nzchar(plot_timezone[[1]])
    ) {
        plot_timezone <- "America/Whitehorse"
    } else {
        plot_timezone <- plot_timezone[[1]]
    }

    series$datetime <- suppressWarnings(as.POSIXct(
        series$datetime,
        tz = plot_timezone,
        tryFormats = c(
            "%Y-%m-%d %H:%M:%S",
            "%Y-%m-%d %H:%M",
            "%Y-%m-%dT%H:%M:%S",
            "%Y-%m-%dT%H:%M",
            "%Y-%m-%d"
        )
    ))
    series$value <- suppressWarnings(as.numeric(series$value))
    series <- series[
        stats::complete.cases(series[, required_cols]),
        ,
        drop = FALSE
    ]
    series <- series[order(series$datetime), , drop = FALSE]

    if (nrow(series) == 0) {
        return(empty_plotly_widget(
            title = title_prefix,
            annotations = list(list(
                text = sprintf(
                    "No plottable data available for %s.",
                    location_code
                ),
                x = 0.5,
                y = 0.5,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE
            ))
        ))
    }

    title_text <- if (!is.null(title_prefix) && nzchar(title_prefix)) {
        title_prefix
    } else {
        sprintf("%s [%s]", location_code, parameter)
    }

    plotly::plot_ly(
        data = series,
        x = ~datetime,
        y = ~value,
        type = "scatter",
        mode = "lines",
        name = "Observed",
        line = list(color = "#000000", width = 1.5),
        hovertemplate = "Observed: %{y:.3f}<extra></extra>"
    ) %>%
        plotly::layout(
            title = title_text,
            xaxis = list(title = "Date"),
            yaxis = list(title = parameter_axis_title(parameter)),
            hovermode = "x unified",
            showlegend = FALSE
        )
}

build_station_plot <- function(
    location_code,
    parameter,
    reference_time,
    con,
    snow_survey_parameters,
    load_entire_record = FALSE,
    continuous_data = NULL,
    percentiles = NULL,
    return_periods = NULL
) {
    tryCatch(
        suppressWarnings({
            if (parameter %in% snow_survey_parameters) {
                return(plot_snow_survey_boxplot(
                    location_code = location_code,
                    parameter = parameter,
                    reference_time = as.character(reference_time),
                    con = con
                ))
            }

            if (is.null(continuous_data)) {
                continuous_data <- get_station_timeseries(
                    location_code = location_code,
                    parameter = parameter,
                    reference_time = reference_time,
                    load_entire_record = load_entire_record,
                    con = con,
                    historical_start_year = 2020L
                )
            }

            if (is.null(continuous_data) || nrow(continuous_data) == 0) {
                return(simple_continuous_plotly_widget(
                    location_code = location_code,
                    parameter = parameter,
                    continuous_data = continuous_data,
                    title_prefix = sprintf("%s [%s]", location_code, parameter)
                ))
            }

            if (!"location_code" %in% names(continuous_data)) {
                continuous_data$location_code <- location_code
            }

            tryCatch(
                plot_continuous_with_percentiles_and_return_periods(
                    location_code = location_code,
                    continuous_data = continuous_data,
                    percentiles = percentiles,
                    return_periods = return_periods,
                    parameter = parameter,
                    reference_time = as.character(reference_time),
                    load_entire_record = load_entire_record,
                    con = con,
                    historical_start_year = 2020L
                ),
                error = function(e) {
                    # Fall back to the observed series without overlays so
                    # hydrometric plots still render if percentile or return-
                    # period decoration fails.
                    simple_continuous_plotly_widget(
                        location_code = location_code,
                        parameter = parameter,
                        continuous_data = continuous_data,
                        title_prefix = sprintf(
                            "%s [%s]",
                            location_code,
                            parameter
                        )
                    )
                }
            )
        }),
        error = function(e) {
            simple_continuous_plotly_widget(
                location_code = location_code,
                parameter = parameter,
                continuous_data = continuous_data,
                title_prefix = sprintf("%s [%s]", location_code, parameter)
            )
        }
    )
}

plot_ggplot_to_data_uri <- function(
    plot_obj,
    width = 900,
    height = 500,
    dpi = 96
) {
    if (is.null(plot_obj) || !requireNamespace("ggplot2", quietly = TRUE)) {
        return(NULL)
    }

    tryCatch(
        {
            tmpfile <- tempfile(fileext = ".png")
            on.exit(unlink(tmpfile), add = TRUE)

            device_open <- FALSE
            grDevices::png(
                filename = tmpfile,
                width = width,
                height = height,
                units = "px",
                res = dpi,
                bg = "white"
            )
            device_open <- TRUE
            on.exit(
                if (isTRUE(device_open)) {
                    grDevices::dev.off()
                },
                add = TRUE
            )

            if (inherits(plot_obj, "ggplot")) {
                print(plot_obj)
            } else {
                graphics::plot.new()
                graphics::text(
                    0.5,
                    0.5,
                    labels = "Static plot could not be rendered.",
                    cex = 1.1
                )
            }

            grDevices::dev.off()
            device_open <- FALSE

            if (!file.exists(tmpfile) || file.size(tmpfile) <= 0) {
                return(NULL)
            }

            raw_bytes <- readBin(
                tmpfile,
                what = "raw",
                n = file.size(tmpfile)
            )
            base64enc::dataURI(data = raw_bytes, mime = "image/png")
        },
        error = function(e) NULL
    )
}

plot_snow_survey_static <- function(
    location_code,
    parameter,
    reference_time = NULL,
    con = NULL
) {
    if (is.null(con)) {
        stop("Provide `con` for snow survey plotting")
    }

    snow <- get_snow_survey_history(
        location_code = location_code,
        parameter = parameter,
        reference_time = reference_time,
        con = con
    )

    if (nrow(snow) == 0) {
        return(
            ggplot2::ggplot() +
                ggplot2::theme_void() +
                ggplot2::annotate(
                    "text",
                    x = 1,
                    y = 1,
                    label = sprintf(
                        "No snow survey data for %s (%s)",
                        location_code,
                        parameter
                    ),
                    size = 5
                )
        )
    }

    snow$survey_year <- as.integer(format(snow$target_datetime, "%Y"))
    survey_month <- as.integer(format(snow$target_datetime, "%m"))
    snow$survey_month <- factor(
        ifelse(
            survey_month == 3,
            "Mar 1",
            ifelse(survey_month == 4, "Apr 1", "May 1")
        ),
        levels = c("Mar 1", "Apr 1", "May 1")
    )

    ref_ts <- if (is.null(reference_time)) {
        Sys.time()
    } else {
        as.POSIXct(reference_time)
    }
    ref_year <- as.integer(format(ref_ts, "%Y"))

    historical <- snow[snow$survey_year < ref_year, ]
    current <- snow[snow$survey_year == ref_year, ]

    p <- ggplot2::ggplot() +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::labs(x = "Survey date", y = parameter)

    if (nrow(historical) > 0) {
        p <- p +
            ggplot2::geom_boxplot(
                data = historical,
                ggplot2::aes(x = survey_month, y = value),
                fill = "#e5e7eb",
                color = "#4b5563",
                outlier.alpha = 0.25
            )
    }

    if (nrow(current) > 0) {
        p <- p +
            ggplot2::geom_point(
                data = current,
                ggplot2::aes(x = survey_month, y = value),
                color = "#000000",
                size = 2.5
            )
    }

    p
}

plot_continuous_static_with_percentiles_and_return_periods <- function(
    location_code,
    continuous_data = NULL,
    percentiles = NULL,
    return_periods = NULL,
    parameter = "water flow",
    return_period_values = c(2, 5, 10, 25, 50, 100),
    start_date = NULL,
    end_date = NULL,
    reference_time = NULL,
    load_entire_record = FALSE,
    con = NULL,
    historical_start_year = 2020
) {
    if (
        is.null(con) &&
            (is.null(continuous_data) ||
                is.null(percentiles) ||
                is.null(return_periods))
    ) {
        stop("Provide `con` when data inputs are NULL")
    }

    include_return_periods <- parameter %in% c("water flow", "water level")
    include_percentiles <- parameter %in%
        c(
            "water flow",
            "water level",
            "precipitation (1wk)",
            "precipitation (24hr)",
            "temperature, air",
            "FDD",
            "DDT"
        )
    historical_start_year <- normalize_historical_start_year(
        historical_start_year
    )

    safe_as_posix <- function(x, tz) {
        tryCatch(
            suppressWarnings(as.POSIXct(
                x,
                tz = tz,
                tryFormats = c(
                    "%Y-%m-%d %H:%M:%S",
                    "%Y-%m-%d %H:%M",
                    "%Y-%m-%dT%H:%M:%S",
                    "%Y-%m-%dT%H:%M",
                    "%Y-%m-%d"
                )
            )),
            error = function(e) as.POSIXct(NA, tz = tz)
        )
    }

    now_ts <- if (is.null(reference_time)) {
        Sys.time()
    } else {
        safe_as_posix(reference_time, tz = "America/Whitehorse")
    }
    if (is.na(now_ts)) {
        now_ts <- Sys.time()
    }

    default_view_start <- now_ts - as.difftime(7, units = "days")
    default_view_end <- seq(now_ts, by = "2 months", length.out = 2)[2]

    view_start <- if (!is.null(start_date)) {
        safe_as_posix(start_date, tz = "America/Whitehorse")
    } else {
        default_view_start
    }
    view_end <- if (!is.null(end_date)) {
        safe_as_posix(end_date, tz = "America/Whitehorse")
    } else {
        default_view_end
    }

    if (is.na(view_start)) {
        view_start <- default_view_start
    }
    if (is.na(view_end)) {
        view_end <- default_view_end
    }

    if (is.null(continuous_data)) {
        continuous_data <- get_station_timeseries(
            location_code = location_code,
            parameter = parameter,
            reference_time = now_ts,
            load_entire_record = load_entire_record,
            con = con,
            historical_start_year = historical_start_year
        )
    }
    if (!"location_code" %in% names(continuous_data)) {
        continuous_data$location_code <- location_code
    }

    series <- continuous_data[continuous_data$location_code == location_code, ]
    if (nrow(series) == 0) {
        return(
            ggplot2::ggplot() +
                ggplot2::theme_void() +
                ggplot2::annotate(
                    "text",
                    x = 1,
                    y = 1,
                    label = sprintf(
                        "No data for %s (%s)",
                        location_code,
                        parameter
                    ),
                    size = 5
                )
        )
    }

    plot_timezone <- attr(series$datetime, "tzone")
    if (
        length(plot_timezone) == 0 ||
            is.na(plot_timezone[[1]]) ||
            !nzchar(plot_timezone[[1]])
    ) {
        plot_timezone <- "America/Whitehorse"
    } else {
        plot_timezone <- plot_timezone[[1]]
    }

    coerce_to_plot_tz <- function(x) {
        x <- safe_as_posix(x, tz = plot_timezone)
        attr(x, "tzone") <- plot_timezone
        x
    }

    series$datetime <- coerce_to_plot_tz(series$datetime)
    series$value <- suppressWarnings(as.numeric(series$value))
    series <- series[
        stats::complete.cases(series[, c("datetime", "value")]),
        ,
        drop = FALSE
    ]

    if (nrow(series) == 0) {
        return(
            ggplot2::ggplot() +
                ggplot2::theme_void() +
                ggplot2::annotate(
                    "text",
                    x = 1,
                    y = 1,
                    label = sprintf(
                        "No plottable data for %s (%s)",
                        location_code,
                        parameter
                    ),
                    size = 5
                )
        )
    }

    now_ts <- coerce_to_plot_tz(now_ts)
    view_start <- coerce_to_plot_tz(view_start)
    view_end <- coerce_to_plot_tz(view_end)

    latest_obs <- max(series$datetime, na.rm = TRUE)
    if (is.finite(latest_obs)) {
        view_start <- min(view_start, latest_obs, now_ts)
        view_end <- max(view_end, latest_obs, now_ts)
    } else {
        view_start <- min(view_start, now_ts)
        view_end <- max(view_end, now_ts)
    }

    pct_plot <- data.frame()
    if (include_percentiles) {
        if (is.null(percentiles)) {
            percentiles <- get_daily_percentiles(
                location_codes = c(location_code),
                parameter = parameter,
                con = con,
                historical_start_year = historical_start_year
            )
        }
        pct <- percentiles[percentiles$location_code == location_code, ]

        if (nrow(pct) > 0) {
            ref_year <- suppressWarnings(as.integer(
                format(now_ts, "%Y", tz = plot_timezone)
            ))
            if (is.na(ref_year)) {
                ref_year <- as.integer(format(
                    Sys.time(),
                    "%Y",
                    tz = plot_timezone
                ))
            }

            percentile_dates <- seq(
                as.Date(sprintf("%d-01-01", historical_start_year)),
                as.Date(sprintf("%d-12-31", ref_year + 1L)),
                by = "day"
            )
            pct_years <- as.integer(format(percentile_dates, "%Y"))
            pct_months <- as.integer(format(percentile_dates, "%m"))
            pct_days <- as.integer(format(percentile_dates, "%d"))
            pct_doys <- as.integer(format(percentile_dates, "%j"))
            pct_is_leap <- (pct_years %% 4 == 0 & pct_years %% 100 != 0) |
                (pct_years %% 400 == 0)

            pct_plot <- data.frame(
                location_code = location_code,
                datetime = as.POSIXct(percentile_dates, tz = plot_timezone),
                doy = ifelse(
                    pct_months > 2 & pct_is_leap,
                    pct_doys - 1L,
                    pct_doys
                ),
                stringsAsFactors = FALSE
            )
            pct_plot$doy[pct_months == 2 & pct_days == 29] <- NA_integer_
            pct_lookup <- pct[, c(
                "location_code",
                "doy",
                "p0",
                "p10",
                "p25",
                "p50",
                "p75",
                "p90",
                "p100"
            )]
            match_idx <- match(
                paste(pct_plot$location_code, pct_plot$doy),
                paste(pct_lookup$location_code, pct_lookup$doy)
            )
            for (col in c("p0", "p10", "p25", "p50", "p75", "p90", "p100")) {
                pct_plot[[col]] <- pct_lookup[[col]][match_idx]
            }
            pct_plot <- pct_plot[
                stats::complete.cases(pct_plot[, c(
                    "datetime",
                    "p0",
                    "p10",
                    "p25",
                    "p50",
                    "p75",
                    "p90",
                    "p100"
                )]),
            ]
        }
    }

    if (!"trace_source" %in% names(series)) {
        series$trace_source <- "observed"
    }

    p <- ggplot2::ggplot()

    if (nrow(pct_plot) > 0) {
        p <- p +
            ggplot2::geom_ribbon(
                data = pct_plot,
                ggplot2::aes(x = datetime, ymin = p0, ymax = p10),
                fill = grDevices::adjustcolor("#7f1d1d", alpha.f = 0.3)
            ) +
            ggplot2::geom_ribbon(
                data = pct_plot,
                ggplot2::aes(x = datetime, ymin = p10, ymax = p25),
                fill = grDevices::adjustcolor("#b45309", alpha.f = 0.3)
            ) +
            ggplot2::geom_ribbon(
                data = pct_plot,
                ggplot2::aes(x = datetime, ymin = p25, ymax = p50),
                fill = grDevices::adjustcolor("#f59e0b", alpha.f = 0.3)
            ) +
            ggplot2::geom_ribbon(
                data = pct_plot,
                ggplot2::aes(x = datetime, ymin = p50, ymax = p75),
                fill = grDevices::adjustcolor("#a3e635", alpha.f = 0.3)
            ) +
            ggplot2::geom_ribbon(
                data = pct_plot,
                ggplot2::aes(x = datetime, ymin = p75, ymax = p90),
                fill = grDevices::adjustcolor("#22c55e", alpha.f = 0.3)
            ) +
            ggplot2::geom_ribbon(
                data = pct_plot,
                ggplot2::aes(x = datetime, ymin = p90, ymax = p100),
                fill = grDevices::adjustcolor("#0f766e", alpha.f = 0.3)
            )
    }

    if (isTRUE(load_entire_record)) {
        historical_series <- series[series$trace_source == "historical_daily", ]
        realtime_series <- series[
            series$trace_source == "realtime_continuous",
        ]

        if (nrow(historical_series) > 0) {
            p <- p +
                ggplot2::geom_line(
                    data = historical_series,
                    ggplot2::aes(x = datetime, y = value),
                    color = "#4b5563",
                    linewidth = 0.35
                )
        }
        if (nrow(realtime_series) > 0) {
            p <- p +
                ggplot2::geom_line(
                    data = realtime_series,
                    ggplot2::aes(x = datetime, y = value),
                    color = "#000000",
                    linewidth = 0.5
                )
        }
        if (nrow(historical_series) == 0 && nrow(realtime_series) == 0) {
            p <- p +
                ggplot2::geom_line(
                    data = series,
                    ggplot2::aes(x = datetime, y = value),
                    color = "#000000",
                    linewidth = 0.5
                )
        }
    } else {
        p <- p +
            ggplot2::geom_line(
                data = series,
                ggplot2::aes(x = datetime, y = value),
                color = "#000000",
                linewidth = 0.4
            )
    }

    if (include_return_periods) {
        if (is.null(return_periods)) {
            return_periods <- get_return_period_discharge(
                location_codes = c(location_code),
                parameter = parameter,
                return_periods = return_period_values,
                con = con
            )
        }
        rp <- return_periods[return_periods$location_code == location_code, ]

        if (nrow(rp) > 0) {
            rp_cols <- grep("^rp_", names(rp), value = TRUE)
            if (length(rp_cols) > 0) {
                rp_values <- unlist(rp[1, rp_cols], use.names = TRUE)
                rp_values <- rp_values[is.finite(rp_values)]
                if (length(rp_values) > 0) {
                    label_positions <- c(0.995, 0.965, 0.935, 0.905)
                    rp_indices <- seq_along(rp_values)
                    rp_x <- as.POSIXct(
                        as.numeric(view_start) +
                            (as.numeric(view_end) - as.numeric(view_start)) *
                                label_positions[
                                    ((rp_indices - 1) %%
                                        length(label_positions)) +
                                        1
                                ],
                        origin = "1970-01-01",
                        tz = plot_timezone
                    )
                    rp_df <- data.frame(
                        label = sprintf(
                            "RP %s",
                            sub("^rp_", "", names(rp_values))
                        ),
                        y = as.numeric(rp_values),
                        x = rp_x
                    )

                    p <- p +
                        ggplot2::geom_hline(
                            data = rp_df,
                            ggplot2::aes(yintercept = y),
                            color = "#dc2626",
                            linewidth = 0.3
                        ) +
                        ggplot2::geom_label(
                            data = rp_df,
                            ggplot2::aes(x = x, y = y, label = label),
                            hjust = 1,
                            size = 3,
                            fill = "white",
                            label.size = 0.25,
                            color = "#7f1d1d"
                        )
                }
            }
        }
    }

    p +
        ggplot2::annotate(
            "segment",
            x = now_ts,
            xend = now_ts,
            y = -Inf,
            yend = Inf,
            color = "#9ca3af",
            linetype = "dashed"
        ) +
        ggplot2::scale_x_datetime(
            date_labels = "%d-%b"
        ) +
        ggplot2::coord_cartesian(
            xlim = c(view_start, view_end)
        ) +
        ggplot2::labs(x = "Date", y = parameter_axis_title(parameter)) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(legend.position = "none")
}

build_station_static_plot <- function(
    location_code,
    parameter,
    reference_time,
    con,
    snow_survey_parameters,
    load_entire_record = FALSE
) {
    simple_continuous_static_plot <- function(
        location_code,
        parameter,
        continuous_data,
        title_prefix = NULL
    ) {
        load_static_fallback_series <- function() {
            if (!(parameter %in% c("water flow", "water level"))) {
                return(continuous_data)
            }

            fallback_end <- suppressWarnings(as.POSIXct(reference_time))
            if (is.na(fallback_end)) {
                fallback_end <- Sys.time()
            }

            fallback_start <- fallback_end - as.difftime(14, units = "days")

            tryCatch(
                get_continuous_data(
                    location_codes = location_code,
                    parameter = parameter,
                    start_date = fallback_start,
                    end_date = fallback_end,
                    con = con
                ),
                error = function(e) continuous_data
            )
        }

        title_text <- if (!is.null(title_prefix) && nzchar(title_prefix)) {
            title_prefix
        } else {
            sprintf("%s [%s]", location_code, parameter)
        }

        if (is.null(continuous_data) || nrow(continuous_data) == 0) {
            continuous_data <- load_static_fallback_series()
        }

        if (is.null(continuous_data) || nrow(continuous_data) == 0) {
            return(
                ggplot2::ggplot() +
                    ggplot2::theme_void() +
                    ggplot2::labs(title = title_text) +
                    ggplot2::annotate(
                        "text",
                        x = 1,
                        y = 1,
                        label = sprintf(
                            "No data available for %s.",
                            location_code
                        ),
                        size = 5
                    )
            )
        }

        series <- continuous_data
        if ("location_code" %in% names(series)) {
            series <- series[
                series$location_code == location_code,
                ,
                drop = FALSE
            ]
        }

        if (nrow(series) == 0) {
            continuous_data <- load_static_fallback_series()
            series <- continuous_data
            if ("location_code" %in% names(series)) {
                series <- series[
                    series$location_code == location_code,
                    ,
                    drop = FALSE
                ]
            }
        }

        required_cols <- c("datetime", "value")
        if (nrow(series) == 0 || !all(required_cols %in% names(series))) {
            return(
                ggplot2::ggplot() +
                    ggplot2::theme_void() +
                    ggplot2::labs(title = title_text) +
                    ggplot2::annotate(
                        "text",
                        x = 1,
                        y = 1,
                        label = sprintf(
                            "No plottable data available for %s.",
                            location_code
                        ),
                        size = 5
                    )
            )
        }

        plot_timezone <- attr(series$datetime, "tzone")
        if (
            length(plot_timezone) == 0 ||
                is.na(plot_timezone[[1]]) ||
                !nzchar(plot_timezone[[1]])
        ) {
            plot_timezone <- "America/Whitehorse"
        } else {
            plot_timezone <- plot_timezone[[1]]
        }

        series$datetime <- suppressWarnings(as.POSIXct(
            series$datetime,
            tz = plot_timezone,
            tryFormats = c(
                "%Y-%m-%d %H:%M:%S",
                "%Y-%m-%d %H:%M",
                "%Y-%m-%dT%H:%M:%S",
                "%Y-%m-%dT%H:%M",
                "%Y-%m-%d"
            )
        ))
        series$value <- suppressWarnings(as.numeric(series$value))
        series <- series[
            stats::complete.cases(series[, required_cols]),
            ,
            drop = FALSE
        ]
        series <- series[order(series$datetime), , drop = FALSE]

        if (nrow(series) == 0) {
            return(
                ggplot2::ggplot() +
                    ggplot2::theme_void() +
                    ggplot2::labs(title = title_text) +
                    ggplot2::annotate(
                        "text",
                        x = 1,
                        y = 1,
                        label = sprintf(
                            "No plottable data available for %s.",
                            location_code
                        ),
                        size = 5
                    )
            )
        }

        ggplot2::ggplot(
            series,
            ggplot2::aes(x = datetime, y = value)
        ) +
            ggplot2::geom_line(color = "#000000", linewidth = 0.4) +
            ggplot2::scale_x_datetime(date_labels = "%d-%b") +
            ggplot2::labs(
                title = title_text,
                x = "Date",
                y = parameter_axis_title(parameter)
            ) +
            ggplot2::theme_minimal(base_size = 11)
    }

    tryCatch(
        suppressWarnings({
            if (parameter %in% snow_survey_parameters) {
                return(plot_snow_survey_static(
                    location_code = location_code,
                    parameter = parameter,
                    reference_time = as.character(reference_time),
                    con = con
                ))
            }

            continuous_data <- get_station_timeseries(
                location_code = location_code,
                parameter = parameter,
                reference_time = reference_time,
                load_entire_record = load_entire_record,
                con = con,
                historical_start_year = 2020L
            )

            if (is.null(continuous_data) || nrow(continuous_data) == 0) {
                return(simple_continuous_static_plot(
                    location_code = location_code,
                    parameter = parameter,
                    continuous_data = continuous_data,
                    title_prefix = sprintf("%s [%s]", location_code, parameter)
                ))
            }

            percentiles <- if (
                parameter %in%
                    c(
                        "water flow",
                        "water level",
                        "precipitation (1wk)",
                        "precipitation (24hr)",
                        "temperature, air",
                        "FDD",
                        "DDT"
                    )
            ) {
                get_daily_percentiles(
                    location_codes = location_code,
                    parameter = parameter,
                    con = con,
                    historical_start_year = 2020L
                )
            } else {
                data.frame()
            }

            return_periods <- if (
                parameter %in% c("water flow", "water level")
            ) {
                get_return_period_discharge(
                    location_codes = location_code,
                    parameter = parameter,
                    con = con
                )
            } else {
                data.frame()
            }

            tryCatch(
                {
                    static_plot <- plot_continuous_static_with_percentiles_and_return_periods(
                        location_code = location_code,
                        continuous_data = continuous_data,
                        percentiles = percentiles,
                        return_periods = return_periods,
                        parameter = parameter,
                        reference_time = as.character(reference_time),
                        load_entire_record = load_entire_record,
                        con = con,
                        historical_start_year = 2020L
                    )

                    # Some ggplot failures surface only when the plot is built
                    # for rendering, not when the object is created.
                    ggplot2::ggplot_build(static_plot)
                    static_plot
                },
                error = function(e) {
                    simple_continuous_static_plot(
                        location_code = location_code,
                        parameter = parameter,
                        continuous_data = continuous_data,
                        title_prefix = sprintf(
                            "%s [%s]",
                            location_code,
                            parameter
                        )
                    )
                }
            )
        }),
        error = function(e) {
            simple_continuous_static_plot(
                location_code = location_code,
                parameter = parameter,
                continuous_data = NULL,
                title_prefix = sprintf("%s [%s]", location_code, parameter)
            )
        }
    )
}

#' Summarize Latest Parameter Value Per Station
#'
#' Computes latest value and data age for a parameter by selecting the most
#' recent observation across continuous and daily sources.
#'
#' @param location_codes Character vector of location codes.
#' @param parameter Parameter name.
#' @param con DBI connection object.
#' @param reference_time Optional POSIXct reference timestamp.
#'
#' @return Summary data.frame with latest value metadata.
#' @noRd
get_latest_parameter_summary <- function(
    location_codes,
    parameter,
    con,
    reference_time = NULL
) {
    location_codes <- unique(stats::na.omit(unlist(
        location_codes,
        use.names = FALSE
    )))
    if (length(location_codes) == 0) {
        return(data.frame())
    }

    location_codes_sql <- paste(
        DBI::dbQuoteString(con, location_codes),
        collapse = ","
    )
    parameter_sql <- DBI::dbQuoteString(con, parameter)

    ref_ts_sql <- if (is.null(reference_time)) {
        "NOW()"
    } else {
        paste0(
            "'",
            format(as.POSIXct(reference_time), "%Y-%m-%d %H:%M:%S"),
            "'::timestamp"
        )
    }
    ref_date_sql <- if (is.null(reference_time)) {
        "CURRENT_DATE"
    } else {
        paste0("'", format(as.POSIXct(reference_time), "%Y-%m-%d"), "'::date")
    }

    dat <- YGwater::dbGetQueryDT(
        sprintf(
            paste(
                "WITH target_locations AS (",
                "    SELECT UNNEST(ARRAY[%s]::text[]) AS location_code",
                "),",
                "target_timeseries AS (",
                "    SELECT ts.location_id, ts.timeseries_id",
                "    FROM timeseries ts",
                "    JOIN locations l",
                "      ON l.location_id = ts.location_id",
                "    JOIN target_locations tl",
                "      ON tl.location_code = l.location_code",
                "    JOIN parameters p",
                "      ON p.parameter_id = ts.parameter_id",
                "    WHERE p.param_name = %s",
                "),",
                "continuous_latest AS (",
                "    SELECT DISTINCT ON (tt.location_id)",
                "        tt.location_id,",
                "        tt.timeseries_id,",
                "        mc.datetime AS latest_time,",
                "        mc.value AS current_value",
                "    FROM target_timeseries tt",
                "    JOIN measurements_continuous mc",
                "      ON mc.timeseries_id = tt.timeseries_id",
                "    WHERE mc.value IS NOT NULL",
                paste0("      AND mc.datetime <= ", ref_ts_sql),
                "    ORDER BY tt.location_id, mc.datetime DESC, tt.timeseries_id",
                "),",
                "continuous_change AS (",
                "    SELECT",
                "        cl.location_id,",
                "        cl.timeseries_id,",
                "        cl.latest_time,",
                "        cl.current_value,",
                "        cl.current_value - prev.value AS change_24h,",
                "        1 AS source_priority",
                "    FROM continuous_latest cl",
                "    LEFT JOIN LATERAL (",
                "        SELECT mc.value AS value",
                "        FROM measurements_continuous mc",
                "        WHERE mc.timeseries_id = cl.timeseries_id",
                "          AND mc.value IS NOT NULL",
                "          AND mc.datetime <= cl.latest_time - INTERVAL '24 hours'",
                "        ORDER BY mc.datetime DESC",
                "        LIMIT 1",
                "    ) prev ON TRUE",
                "),",
                "daily_latest AS (",
                "    SELECT DISTINCT ON (tt.location_id)",
                "        tt.location_id,",
                "        tt.timeseries_id,",
                "        mcd.date::timestamp AS latest_time,",
                "        mcd.value AS current_value",
                "    FROM target_timeseries tt",
                "    JOIN measurements_calculated_daily mcd",
                "      ON mcd.timeseries_id = tt.timeseries_id",
                "    WHERE mcd.value IS NOT NULL",
                paste0("      AND mcd.date <= ", ref_date_sql),
                "    ORDER BY tt.location_id, mcd.date DESC, tt.timeseries_id",
                "),",
                "daily_change AS (",
                "    SELECT",
                "        dl.location_id,",
                "        dl.timeseries_id,",
                "        dl.latest_time,",
                "        dl.current_value,",
                "        dl.current_value - prev.value AS change_24h,",
                "        2 AS source_priority",
                "    FROM daily_latest dl",
                "    LEFT JOIN LATERAL (",
                "        SELECT mcd.value AS value",
                "        FROM measurements_calculated_daily mcd",
                "        WHERE mcd.timeseries_id = dl.timeseries_id",
                "          AND mcd.value IS NOT NULL",
                "          AND mcd.date::timestamp <= dl.latest_time - INTERVAL '1 day'",
                "        ORDER BY mcd.date DESC",
                "        LIMIT 1",
                "    ) prev ON TRUE",
                "),",
                "latest AS (",
                "    SELECT location_id, timeseries_id, latest_time, current_value, change_24h, source_priority FROM continuous_change",
                "    UNION ALL",
                "    SELECT location_id, timeseries_id, latest_time, current_value, change_24h, source_priority FROM daily_change",
                "),",
                "best_latest AS (",
                "    SELECT",
                "        l.*,",
                "        ROW_NUMBER() OVER (",
                "            PARTITION BY l.location_id",
                "            ORDER BY l.latest_time DESC, l.source_priority, l.timeseries_id",
                "        ) AS rn",
                "    FROM latest l",
                ")",
                "SELECT",
                "    loc.location_id,",
                "    loc.location_code,",
                "    loc.name,",
                "    b.timeseries_id,",
                "    b.latest_time AS latest_time,",
                "    b.current_value,",
                "    b.change_24h,",
                paste0(
                    "    EXTRACT(EPOCH FROM (",
                    ref_ts_sql,
                    " - b.latest_time)) / 3600.0 AS last_data_age_hours"
                ),
                "FROM best_latest b",
                "JOIN locations loc",
                "  ON loc.location_id = b.location_id",
                "WHERE b.rn = 1",
                "ORDER BY loc.location_code"
            ),
            location_codes_sql,
            parameter_sql
        ),
        con = con
    )

    interpret_loaded_times_as_local(dat, time_columns = c("latest_time"))
}


#' Summarize Seasonal Freezing Degree Days By Station
#'
#' Computes seasonal cumulative FDD from daily mean air temperature,
#' partitioned by Oct 1 season start year and reset to 1 at season start.
#'
#' @param location_codes Character vector of location codes.
#' @param con DBI connection object.
#' @param reference_time Optional POSIXct reference timestamp.
#'
#' @return Summary data.frame with latest FDD metadata.
#' @noRd
get_fdd_summary <- function(
    location_codes,
    con,
    reference_time = NULL
) {
    location_codes <- unique(stats::na.omit(unlist(
        location_codes,
        use.names = FALSE
    )))
    if (length(location_codes) == 0) {
        return(data.frame())
    }

    location_codes_sql <- paste(
        DBI::dbQuoteString(con, location_codes),
        collapse = ","
    )

    ref_ts_sql <- if (is.null(reference_time)) {
        "NOW()"
    } else {
        paste0(
            "'",
            format(as.POSIXct(reference_time), "%Y-%m-%d %H:%M:%S"),
            "'::timestamp"
        )
    }
    ref_date_sql <- if (is.null(reference_time)) {
        "CURRENT_DATE"
    } else {
        paste0("'", format(as.POSIXct(reference_time), "%Y-%m-%d"), "'::date")
    }

    dat <- YGwater::dbGetQueryDT(
        sprintf(
            paste(
                "WITH target_locations AS (",
                "    SELECT UNNEST(ARRAY[%s]::text[]) AS location_code",
                "),",
                "target_timeseries AS (",
                "    SELECT ts.location_id, ts.timeseries_id",
                "    FROM timeseries ts",
                "    JOIN locations l",
                "      ON l.location_id = ts.location_id",
                "    JOIN target_locations tl",
                "      ON tl.location_code = l.location_code",
                "    JOIN parameters p",
                "      ON p.parameter_id = ts.parameter_id",
                "    WHERE p.param_name = 'temperature, air'",
                "),",
                "daily_temp AS (",
                "    SELECT",
                "        tt.location_id,",
                "        tt.timeseries_id,",
                "        DATE_TRUNC('day', mc.datetime)::date AS date,",
                "        AVG(mc.value) AS mean_temp",
                "    FROM target_timeseries tt",
                "    JOIN measurements_continuous mc",
                "      ON mc.timeseries_id = tt.timeseries_id",
                "    WHERE mc.value IS NOT NULL",
                paste0("      AND mc.datetime <= ", ref_ts_sql),
                "    GROUP BY tt.location_id, tt.timeseries_id, DATE_TRUNC('day', mc.datetime)::date",
                "),",
                "daily_fdd AS (",
                "    SELECT",
                "        dt.location_id,",
                "        dt.timeseries_id,",
                "        dt.date,",
                "        GREATEST(-dt.mean_temp, 0) AS fdd_day,",
                "        CASE",
                "            WHEN EXTRACT(MONTH FROM dt.date) >= 10",
                "            THEN EXTRACT(YEAR FROM dt.date)::int",
                "            ELSE EXTRACT(YEAR FROM dt.date)::int - 1",
                "        END AS season_start_year",
                "    FROM daily_temp dt",
                "    WHERE dt.date <= ",
                ref_date_sql,
                "),",
                "seasonal_fdd AS (",
                "    SELECT",
                "        df.location_id,",
                "        df.timeseries_id,",
                "        df.date,",
                "        1 +",
                "            SUM(df.fdd_day) OVER (",
                "                PARTITION BY df.location_id, df.timeseries_id, df.season_start_year",
                "                ORDER BY df.date",
                "                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW",
                "            ) -",
                "            FIRST_VALUE(df.fdd_day) OVER (",
                "                PARTITION BY df.location_id, df.timeseries_id, df.season_start_year",
                "                ORDER BY df.date",
                "                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW",
                "            ) AS fdd",
                "    FROM daily_fdd df",
                "),",
                "latest_by_location AS (",
                "    SELECT DISTINCT ON (location_id)",
                "        location_id,",
                "        timeseries_id,",
                "        date,",
                "        fdd",
                "    FROM seasonal_fdd",
                "    ORDER BY location_id, date DESC, timeseries_id",
                ")",
                "SELECT",
                "    loc.location_id,",
                "    loc.location_code,",
                "    loc.name,",
                "    l.timeseries_id,",
                "    l.date::timestamp AS latest_time,",
                "    l.fdd AS current_value,",
                paste0(
                    "    EXTRACT(EPOCH FROM (",
                    ref_ts_sql,
                    " - l.date::timestamp)) / 3600.0 AS last_data_age_hours"
                ),
                "FROM latest_by_location l",
                "JOIN locations loc",
                "  ON loc.location_id = l.location_id",
                "ORDER BY loc.location_code"
            ),
            location_codes_sql
        ),
        con = con
    )

    interpret_loaded_times_as_local(dat, time_columns = c("latest_time"))
}


#' Summarize Seasonal Thawing Degree Days By Station
#'
#' Computes seasonal cumulative DDT from daily mean air temperature,
#' partitioned by Apr 1 season start year and reset to 1 at season start.
#'
#' @param location_codes Character vector of location codes.
#' @param con DBI connection object.
#' @param reference_time Optional POSIXct reference timestamp.
#'
#' @return Summary data.frame with latest DDT metadata.
#' @noRd
get_ddt_summary <- function(
    location_codes,
    con,
    reference_time = NULL
) {
    location_codes <- unique(stats::na.omit(unlist(
        location_codes,
        use.names = FALSE
    )))
    if (length(location_codes) == 0) {
        return(data.frame())
    }

    location_codes_sql <- paste(
        DBI::dbQuoteString(con, location_codes),
        collapse = ","
    )

    ref_ts_sql <- if (is.null(reference_time)) {
        "NOW()"
    } else {
        paste0(
            "'",
            format(as.POSIXct(reference_time), "%Y-%m-%d %H:%M:%S"),
            "'::timestamp"
        )
    }
    ref_date_sql <- if (is.null(reference_time)) {
        "CURRENT_DATE"
    } else {
        paste0("'", format(as.POSIXct(reference_time), "%Y-%m-%d"), "'::date")
    }

    dat <- YGwater::dbGetQueryDT(
        sprintf(
            paste(
                "WITH target_locations AS (",
                "    SELECT UNNEST(ARRAY[%s]::text[]) AS location_code",
                "),",
                "target_timeseries AS (",
                "    SELECT ts.location_id, ts.timeseries_id",
                "    FROM timeseries ts",
                "    JOIN locations l",
                "      ON l.location_id = ts.location_id",
                "    JOIN target_locations tl",
                "      ON tl.location_code = l.location_code",
                "    JOIN parameters p",
                "      ON p.parameter_id = ts.parameter_id",
                "    WHERE p.param_name = 'temperature, air'",
                "),",
                "daily_temp AS (",
                "    SELECT",
                "        tt.location_id,",
                "        tt.timeseries_id,",
                "        DATE_TRUNC('day', mc.datetime)::date AS date,",
                "        AVG(mc.value) AS mean_temp",
                "    FROM target_timeseries tt",
                "    JOIN measurements_continuous mc",
                "      ON mc.timeseries_id = tt.timeseries_id",
                "    WHERE mc.value IS NOT NULL",
                paste0("      AND mc.datetime <= ", ref_ts_sql),
                "    GROUP BY tt.location_id, tt.timeseries_id, DATE_TRUNC('day', mc.datetime)::date",
                "),",
                "daily_ddt AS (",
                "    SELECT",
                "        dt.location_id,",
                "        dt.timeseries_id,",
                "        dt.date,",
                "        GREATEST(dt.mean_temp, 0) AS ddt_day,",
                "        CASE",
                "            WHEN EXTRACT(MONTH FROM dt.date) >= 4",
                "            THEN EXTRACT(YEAR FROM dt.date)::int",
                "            ELSE EXTRACT(YEAR FROM dt.date)::int - 1",
                "        END AS season_start_year",
                "    FROM daily_temp dt",
                "    WHERE dt.date <= ",
                ref_date_sql,
                "),",
                "seasonal_ddt AS (",
                "    SELECT",
                "        df.location_id,",
                "        df.timeseries_id,",
                "        df.date,",
                "        1 +",
                "            SUM(df.ddt_day) OVER (",
                "                PARTITION BY df.location_id, df.timeseries_id, df.season_start_year",
                "                ORDER BY df.date",
                "                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW",
                "            ) -",
                "            FIRST_VALUE(df.ddt_day) OVER (",
                "                PARTITION BY df.location_id, df.timeseries_id, df.season_start_year",
                "                ORDER BY df.date",
                "                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW",
                "            ) AS ddt",
                "    FROM daily_ddt df",
                "),",
                "latest_by_location AS (",
                "    SELECT DISTINCT ON (location_id)",
                "        location_id,",
                "        timeseries_id,",
                "        date,",
                "        ddt",
                "    FROM seasonal_ddt",
                "    ORDER BY location_id, date DESC, timeseries_id",
                ")",
                "SELECT",
                "    loc.location_id,",
                "    loc.location_code,",
                "    loc.name,",
                "    l.timeseries_id,",
                "    l.date::timestamp AS latest_time,",
                "    l.ddt AS current_value,",
                paste0(
                    "    EXTRACT(EPOCH FROM (",
                    ref_ts_sql,
                    " - l.date::timestamp)) / 3600.0 AS last_data_age_hours"
                ),
                "FROM latest_by_location l",
                "JOIN locations loc",
                "  ON loc.location_id = l.location_id",
                "ORDER BY loc.location_code"
            ),
            location_codes_sql
        ),
        con = con
    )

    interpret_loaded_times_as_local(dat, time_columns = c("latest_time"))
}


#' Summarize Snow Survey Results By Station
#'
#' Retrieves latest snow survey observations and current-year values for
#' March 1, April 1, and May 1 from discrete sample results.
#'
#' @param location_codes Character vector of location codes.
#' @param parameter Parameter name (`snow depth` or `snow water equivalent`).
#' @param con DBI connection object.
#' @param reference_time Optional POSIXct reference timestamp.
#'
#' @return Summary data.frame with latest value and seasonal checkpoints.
#' @noRd
get_snow_survey_summary <- function(
    location_codes,
    parameter,
    con,
    reference_time = NULL
) {
    location_codes <- unique(stats::na.omit(unlist(
        location_codes,
        use.names = FALSE
    )))
    if (length(location_codes) == 0) {
        return(data.frame())
    }

    location_codes_sql <- paste(
        DBI::dbQuoteString(con, location_codes),
        collapse = ","
    )
    parameter_sql <- DBI::dbQuoteString(con, parameter)

    ref_ts <- if (is.null(reference_time)) {
        Sys.time()
    } else {
        as.POSIXct(reference_time)
    }
    ref_ts_sql <- paste0(
        "'",
        format(ref_ts, "%Y-%m-%d %H:%M:%S"),
        "'::timestamp"
    )
    ref_year <- as.integer(format(ref_ts, "%Y"))

    dat <- YGwater::dbGetQueryDT(
        sprintf(
            paste(
                "WITH target_locations AS (",
                "    SELECT UNNEST(ARRAY[%s]::text[]) AS location_code",
                "),",
                "survey_values AS (",
                "    SELECT",
                "        l.location_id,",
                "        l.location_code,",
                "        s.sample_id,",
                "        s.target_datetime AS target_datetime,",
                "        dr.result AS value",
                "    FROM target_locations tl",
                "    JOIN locations l",
                "      ON l.location_code = tl.location_code",
                "    JOIN samples s",
                "      ON s.location_id = l.location_id",
                "    JOIN discrete.results dr",
                "      ON dr.sample_id = s.sample_id",
                "    JOIN parameters p",
                "      ON p.parameter_id = dr.parameter_id",
                "    WHERE p.param_name = %s",
                "      AND dr.result IS NOT NULL",
                "      AND s.target_datetime IS NOT NULL",
                paste0("      AND s.target_datetime <= ", ref_ts_sql),
                "      AND EXTRACT(MONTH FROM s.target_datetime) IN (3, 4, 5)",
                "      AND EXTRACT(DAY FROM s.target_datetime) = 1",
                "),",
                "latest_by_location AS (",
                "    SELECT DISTINCT ON (location_id)",
                "        location_id,",
                "        sample_id,",
                "        target_datetime AS latest_time,",
                "        value AS current_value",
                "    FROM survey_values",
                "    ORDER BY location_id, target_datetime DESC, sample_id DESC",
                "),",
                "current_year_values AS (",
                "    SELECT",
                "        location_id,",
                "        MAX(value) FILTER (",
                sprintf(
                    "            WHERE EXTRACT(YEAR FROM target_datetime)::int = %d",
                    ref_year
                ),
                "              AND EXTRACT(MONTH FROM target_datetime) = 3",
                "        ) AS march_1_value,",
                "        MAX(value) FILTER (",
                sprintf(
                    "            WHERE EXTRACT(YEAR FROM target_datetime)::int = %d",
                    ref_year
                ),
                "              AND EXTRACT(MONTH FROM target_datetime) = 4",
                "        ) AS april_1_value,",
                "        MAX(value) FILTER (",
                sprintf(
                    "            WHERE EXTRACT(YEAR FROM target_datetime)::int = %d",
                    ref_year
                ),
                "              AND EXTRACT(MONTH FROM target_datetime) = 5",
                "        ) AS may_1_value",
                "    FROM survey_values",
                "    GROUP BY location_id",
                ")",
                "SELECT",
                "    loc.location_id,",
                "    loc.location_code,",
                "    loc.name,",
                "    l.sample_id AS timeseries_id,",
                "    l.latest_time,",
                "    l.current_value,",
                "    cy.march_1_value,",
                "    cy.april_1_value,",
                "    cy.may_1_value,",
                paste0(
                    "    EXTRACT(EPOCH FROM (",
                    ref_ts_sql,
                    " - l.latest_time)) / 3600.0 AS last_data_age_hours"
                ),
                "FROM latest_by_location l",
                "JOIN locations loc",
                "  ON loc.location_id = l.location_id",
                "LEFT JOIN current_year_values cy",
                "  ON cy.location_id = l.location_id",
                "ORDER BY loc.location_code"
            ),
            location_codes_sql,
            parameter_sql
        ),
        con = con
    )

    interpret_loaded_times_as_local(dat, time_columns = c("latest_time"))
}


#' Get Snow Survey Results For Plotting
#'
#' Retrieves March/April/May first-day snow survey measurements from
#' discrete sample results for a single station.
#'
#' @param location_code Character location code.
#' @param parameter Parameter name (`snow depth` or `snow water equivalent`).
#' @param con DBI connection object.
#' @param reference_time Optional POSIXct reference timestamp.
#'
#' @return Data.frame with target datetimes and values.
#' @noRd
get_snow_survey_history <- function(
    location_code,
    parameter,
    con,
    reference_time = NULL
) {
    if (is.na(location_code) || length(location_code) == 0) {
        return(data.frame())
    }

    location_sql <- DBI::dbQuoteString(con, location_code)
    parameter_sql <- DBI::dbQuoteString(con, parameter)

    ref_ts_sql <- if (is.null(reference_time)) {
        "NOW()"
    } else {
        paste0(
            "'",
            format(as.POSIXct(reference_time), "%Y-%m-%d %H:%M:%S"),
            "'::timestamp"
        )
    }

    dat <- YGwater::dbGetQueryDT(
        sprintf(
            paste(
                "SELECT",
                "    l.location_id,",
                "    l.location_code,",
                "    s.sample_id,",
                "    s.target_datetime,",
                "    dr.result AS value",
                "FROM locations l",
                "JOIN samples s",
                "  ON s.location_id = l.location_id",
                "JOIN discrete.results dr",
                "  ON dr.sample_id = s.sample_id",
                "JOIN parameters p",
                "  ON p.parameter_id = dr.parameter_id",
                "WHERE l.location_code = %s",
                "  AND p.param_name = %s",
                "  AND dr.result IS NOT NULL",
                "  AND s.target_datetime IS NOT NULL",
                paste0("  AND s.target_datetime <= ", ref_ts_sql),
                "  AND EXTRACT(MONTH FROM s.target_datetime) IN (3, 4, 5)",
                "  AND EXTRACT(DAY FROM s.target_datetime) = 1",
                "ORDER BY s.target_datetime"
            ),
            location_sql,
            parameter_sql
        ),
        con = con
    )

    interpret_loaded_times_as_local(dat, time_columns = c("target_datetime"))
}


#' Build Snow Survey Boxplot
#'
#' Plots historical distributions for March/April/May snow surveys as
#' boxplots and overlays current-year observed points.
#'
#' @param location_code Character location code.
#' @param parameter Parameter name (`snow depth` or `snow water equivalent`).
#' @param reference_time Optional POSIXct reference timestamp.
#' @param con DBI connection object.
#'
#' @return A Plotly htmlwidget.
#' @noRd
plot_snow_survey_boxplot <- function(
    location_code,
    parameter,
    reference_time = NULL,
    con = NULL
) {
    if (is.null(con)) {
        stop("Provide `con` for snow survey plotting")
    }

    snow <- get_snow_survey_history(
        location_code = location_code,
        parameter = parameter,
        reference_time = reference_time,
        con = con
    )

    if (nrow(snow) == 0) {
        return(
            empty_plotly_widget(
                title = sprintf(
                    "No snow survey data for %s (%s)",
                    location_code,
                    parameter
                )
            )
        )
    }

    snow$survey_year <- as.integer(format(snow$target_datetime, "%Y"))
    survey_month <- as.integer(format(snow$target_datetime, "%m"))
    snow$survey_month <- factor(
        ifelse(
            survey_month == 3,
            "Mar 1",
            ifelse(survey_month == 4, "Apr 1", "May 1")
        ),
        levels = c("Mar 1", "Apr 1", "May 1")
    )

    ref_ts <- if (is.null(reference_time)) {
        Sys.time()
    } else {
        as.POSIXct(reference_time)
    }
    ref_year <- as.integer(format(ref_ts, "%Y"))

    historical <- snow[snow$survey_year < ref_year, ]
    current <- snow[snow$survey_year == ref_year, ]

    p <- plotly::plot_ly()

    if (nrow(historical) > 0) {
        p <- p %>%
            plotly::add_boxplot(
                data = historical,
                x = ~survey_month,
                y = ~value,
                name = "Historical",
                boxpoints = FALSE,
                marker = list(color = "#4b5563"),
                line = list(color = "#4b5563"),
                hovertemplate = "%{x}: %{y:.3f}<extra></extra>"
            )
    }

    if (nrow(current) > 0) {
        p <- p %>%
            plotly::add_markers(
                data = current,
                x = ~survey_month,
                y = ~value,
                name = sprintf("%d observed", ref_year),
                marker = list(color = "#000000", size = 10),
                hovertemplate = "Observed: %{y:.3f}<extra></extra>"
            )
    }

    p %>%
        plotly::layout(
            xaxis = list(
                title = "Survey date",
                type = "category",
                categoryorder = "array",
                categoryarray = c("Mar 1", "Apr 1", "May 1")
            ),
            yaxis = list(title = parameter),
            hovermode = "x unified",
            showlegend = FALSE
        )
}


#' Get Latest Station Timeseries For Plotting
#'
#' Loads a station timeseries for plotting at a reference time, using
#' precipitation-specific rolling accumulation logic and daily fallback where
#' needed.
#'
#' @param location_code Character location code.
#' @param parameter Parameter name.
#' @param con DBI connection object.
#' @param reference_time Optional POSIXct reference timestamp.
#' @param load_entire_record Logical; bypasses recent-window filters.
#' @param historical_start_year Integer year used as the lower bound for
#'   historical daily values when loading full-record traces.
#'
#' @return Data.frame with `datetime` and `value` columns.
#' @noRd
get_station_timeseries <- function(
    location_code,
    parameter,
    con,
    reference_time = NULL,
    load_entire_record = FALSE,
    historical_start_year = 2020
) {
    if (is.na(location_code) || length(location_code) == 0) {
        return(data.frame(
            datetime = as.POSIXct(character()),
            value = numeric()
        ))
    }

    location_sql <- DBI::dbQuoteString(con, location_code)
    parameter_sql <- DBI::dbQuoteString(con, parameter)
    historical_start_year <- normalize_historical_start_year(
        historical_start_year
    )

    ref_ts_sql <- if (is.null(reference_time)) {
        "NOW()"
    } else {
        paste0(
            "'",
            format(as.POSIXct(reference_time), "%Y-%m-%d %H:%M:%S"),
            "'::timestamp"
        )
    }
    ref_date_sql <- if (is.null(reference_time)) {
        "CURRENT_DATE"
    } else {
        paste0("'", format(as.POSIXct(reference_time), "%Y-%m-%d"), "'::date")
    }

    historical_daily_min_date_sql <- paste0(
        "MAKE_DATE(",
        historical_start_year,
        ", 1, 1)"
    )

    precip_start_filter_sql <- if (isTRUE(load_entire_record)) {
        paste0("  AND mcd.date >= ", historical_daily_min_date_sql)
    } else {
        paste0("  AND mcd.date >= ", ref_date_sql, " - INTERVAL '2 months'")
    }
    # Keep the high-resolution continuous trace scoped to the recent window.
    # Full-record mode backfills older history from daily corrected data.
    continuous_start_filter_sql <- paste0(
        "  AND mc.datetime >= ",
        ref_ts_sql,
        " - INTERVAL '7 days'"
    )
    daily_fallback_start_filter_sql <- if (isTRUE(load_entire_record)) {
        paste0("  AND mcd.date >= ", historical_daily_min_date_sql)
    } else {
        paste0("  AND mcd.date >= ", ref_date_sql, " - INTERVAL '2 months'")
    }

    if (identical(parameter, "FDD")) {
        fdd_start_filter_sql <- if (isTRUE(load_entire_record)) {
            paste0("  AND dt.date >= ", historical_daily_min_date_sql)
        } else {
            paste0(
                "  AND dt.date >= MAKE_DATE(",
                "CASE WHEN EXTRACT(MONTH FROM ",
                ref_date_sql,
                ") >= 10 ",
                "THEN EXTRACT(YEAR FROM ",
                ref_date_sql,
                ")::int ",
                "ELSE EXTRACT(YEAR FROM ",
                ref_date_sql,
                ")::int - 1 END, ",
                "10, 1)"
            )
        }

        fdd <- YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "WITH daily_temp AS (",
                    "    SELECT",
                    "        DATE_TRUNC('day', mc.datetime)::date AS date,",
                    "        AVG(mc.value) AS mean_temp",
                    "    FROM measurements_continuous mc",
                    "    JOIN timeseries ts ON ts.timeseries_id = mc.timeseries_id",
                    "    JOIN parameters p ON p.parameter_id = ts.parameter_id",
                    "    JOIN locations l ON l.location_id = ts.location_id",
                    "    WHERE l.location_code = %s",
                    "      AND p.param_name = 'temperature, air'",
                    "      AND mc.value IS NOT NULL",
                    paste0("      AND mc.datetime <= ", ref_ts_sql),
                    "    GROUP BY DATE_TRUNC('day', mc.datetime)::date",
                    "),",
                    "daily_fdd AS (",
                    "    SELECT",
                    "        dt.date,",
                    "        GREATEST(-dt.mean_temp, 0) AS fdd_day,",
                    "        CASE",
                    "            WHEN EXTRACT(MONTH FROM dt.date) >= 10",
                    "            THEN EXTRACT(YEAR FROM dt.date)::int",
                    "            ELSE EXTRACT(YEAR FROM dt.date)::int - 1",
                    "        END AS season_start_year",
                    "    FROM daily_temp dt",
                    "    WHERE dt.date <= ",
                    ref_date_sql,
                    fdd_start_filter_sql,
                    "),",
                    "seasonal_fdd AS (",
                    "    SELECT",
                    "        df.date::timestamp AS datetime,",
                    "        1 +",
                    "            SUM(df.fdd_day) OVER (",
                    "                PARTITION BY df.season_start_year",
                    "                ORDER BY df.date",
                    "                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW",
                    "            ) -",
                    "            FIRST_VALUE(df.fdd_day) OVER (",
                    "                PARTITION BY df.season_start_year",
                    "                ORDER BY df.date",
                    "                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW",
                    "            ) AS value",
                    "    FROM daily_fdd df",
                    ")",
                    "SELECT datetime, value",
                    "FROM seasonal_fdd",
                    "ORDER BY datetime"
                ),
                location_sql
            ),
            con = con
        )

        fdd$trace_source <- "observed"
        return(interpret_loaded_times_as_local(
            fdd,
            time_columns = c("datetime")
        ))
    }

    if (identical(parameter, "DDT")) {
        ddt_start_filter_sql <- if (isTRUE(load_entire_record)) {
            paste0("  AND dt.date >= ", historical_daily_min_date_sql)
        } else {
            paste0(
                "  AND dt.date >= MAKE_DATE(",
                "CASE WHEN EXTRACT(MONTH FROM ",
                ref_date_sql,
                ") >= 4 ",
                "THEN EXTRACT(YEAR FROM ",
                ref_date_sql,
                ")::int ",
                "ELSE EXTRACT(YEAR FROM ",
                ref_date_sql,
                ")::int - 1 END, ",
                "4, 1)"
            )
        }

        ddt <- YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "WITH daily_temp AS (",
                    "    SELECT",
                    "        DATE_TRUNC('day', mc.datetime)::date AS date,",
                    "        AVG(mc.value) AS mean_temp",
                    "    FROM measurements_continuous mc",
                    "    JOIN timeseries ts ON ts.timeseries_id = mc.timeseries_id",
                    "    JOIN parameters p ON p.parameter_id = ts.parameter_id",
                    "    JOIN locations l ON l.location_id = ts.location_id",
                    "    WHERE l.location_code = %s",
                    "      AND p.param_name = 'temperature, air'",
                    "      AND mc.value IS NOT NULL",
                    paste0("      AND mc.datetime <= ", ref_ts_sql),
                    "    GROUP BY DATE_TRUNC('day', mc.datetime)::date",
                    "),",
                    "daily_ddt AS (",
                    "    SELECT",
                    "        dt.date,",
                    "        GREATEST(dt.mean_temp, 0) AS ddt_day,",
                    "        CASE",
                    "            WHEN EXTRACT(MONTH FROM dt.date) >= 4",
                    "            THEN EXTRACT(YEAR FROM dt.date)::int",
                    "            ELSE EXTRACT(YEAR FROM dt.date)::int - 1",
                    "        END AS season_start_year",
                    "    FROM daily_temp dt",
                    "    WHERE dt.date <= ",
                    ref_date_sql,
                    ddt_start_filter_sql,
                    "),",
                    "seasonal_ddt AS (",
                    "    SELECT",
                    "        df.date::timestamp AS datetime,",
                    "        1 +",
                    "            SUM(df.ddt_day) OVER (",
                    "                PARTITION BY df.season_start_year",
                    "                ORDER BY df.date",
                    "                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW",
                    "            ) -",
                    "            FIRST_VALUE(df.ddt_day) OVER (",
                    "                PARTITION BY df.season_start_year",
                    "                ORDER BY df.date",
                    "                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW",
                    "            ) AS value",
                    "    FROM daily_ddt df",
                    ")",
                    "SELECT datetime, value",
                    "FROM seasonal_ddt",
                    "ORDER BY datetime"
                ),
                location_sql
            ),
            con = con
        )

        ddt$trace_source <- "observed"
        return(interpret_loaded_times_as_local(
            ddt,
            time_columns = c("datetime")
        ))
    }

    if (parameter %in% c("precipitation (1wk)", "precipitation (24hr)")) {
        precip_db_sql <- DBI::dbQuoteString(con, "precipitation, total")

        if (identical(parameter, "precipitation (1wk)")) {
            precip_sql_select <- paste(
                "SELECT",
                "    mcd.date::timestamp AS datetime,",
                "    SUM(mcd.value) OVER (",
                "        ORDER BY mcd.date",
                "        ROWS BETWEEN 6 PRECEDING AND CURRENT ROW",
                "    ) AS value",
                "FROM measurements_calculated_daily mcd",
                "JOIN timeseries ts ON ts.timeseries_id = mcd.timeseries_id",
                "JOIN parameters p ON p.parameter_id = ts.parameter_id",
                "JOIN locations l ON l.location_id = ts.location_id",
                "WHERE l.location_code = %s",
                "  AND p.param_name = %s",
                "  AND mcd.value IS NOT NULL",
                precip_start_filter_sql,
                paste0("  AND mcd.date <= ", ref_date_sql),
                "ORDER BY mcd.date"
            )
            precip_sql_select_fallback <- paste(
                "SELECT",
                "    mcd.date::timestamp AS datetime,",
                "    SUM(mcd.value) OVER (",
                "        ORDER BY mcd.date",
                "        ROWS BETWEEN 6 PRECEDING AND CURRENT ROW",
                "    ) AS value",
                "FROM measurements_calculated_daily mcd",
                "JOIN timeseries ts ON ts.timeseries_id = mcd.timeseries_id",
                "JOIN parameters p ON p.parameter_id = ts.parameter_id",
                "JOIN locations l ON l.location_id = ts.location_id",
                "WHERE l.location_code = %s",
                "  AND p.param_name = %s",
                "  AND mcd.value IS NOT NULL",
                paste0("  AND mcd.date <= ", ref_date_sql),
                "ORDER BY mcd.date"
            )
        } else {
            # precipitation (24hr): raw daily value
            precip_sql_select <- paste(
                "SELECT",
                "    mcd.date::timestamp AS datetime,",
                "    mcd.value AS value",
                "FROM measurements_calculated_daily mcd",
                "JOIN timeseries ts ON ts.timeseries_id = mcd.timeseries_id",
                "JOIN parameters p ON p.parameter_id = ts.parameter_id",
                "JOIN locations l ON l.location_id = ts.location_id",
                "WHERE l.location_code = %s",
                "  AND p.param_name = %s",
                "  AND mcd.value IS NOT NULL",
                precip_start_filter_sql,
                paste0("  AND mcd.date <= ", ref_date_sql),
                "ORDER BY mcd.date"
            )
            precip_sql_select_fallback <- paste(
                "SELECT",
                "    mcd.date::timestamp AS datetime,",
                "    mcd.value AS value",
                "FROM measurements_calculated_daily mcd",
                "JOIN timeseries ts ON ts.timeseries_id = mcd.timeseries_id",
                "JOIN parameters p ON p.parameter_id = ts.parameter_id",
                "JOIN locations l ON l.location_id = ts.location_id",
                "WHERE l.location_code = %s",
                "  AND p.param_name = %s",
                "  AND mcd.value IS NOT NULL",
                paste0("  AND mcd.date <= ", ref_date_sql),
                "ORDER BY mcd.date"
            )
        }

        precipitation <- YGwater::dbGetQueryDT(
            sprintf(precip_sql_select, location_sql, precip_db_sql),
            con = con
        )

        if (nrow(precipitation) == 0 && !isTRUE(load_entire_record)) {
            precipitation <- YGwater::dbGetQueryDT(
                sprintf(
                    precip_sql_select_fallback,
                    location_sql,
                    precip_db_sql
                ),
                con = con
            )
        }

        precipitation <- sanitize_loaded_series_values(precipitation, parameter)
        precipitation$trace_source <- "observed"
        return(interpret_loaded_times_as_local(
            precipitation,
            time_columns = c("datetime")
        ))
    }

    continuous <- tryCatch(
        YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "SELECT",
                    "    mc.datetime,",
                    "    mc.value AS value",
                    "FROM measurements_continuous mc",
                    "JOIN timeseries ts ON ts.timeseries_id = mc.timeseries_id",
                    "JOIN parameters p ON p.parameter_id = ts.parameter_id",
                    "JOIN locations l ON l.location_id = ts.location_id",
                    "WHERE l.location_code = %s",
                    "  AND p.param_name = %s",
                    "  AND mc.value IS NOT NULL",
                    continuous_start_filter_sql,
                    paste0("  AND mc.datetime <= ", ref_ts_sql),
                    "ORDER BY mc.datetime"
                ),
                location_sql,
                parameter_sql
            ),
            con = con
        ),
        error = function(e) data.frame()
    )

    if (nrow(continuous) == 0 && !isTRUE(load_entire_record)) {
        continuous <- tryCatch(
            YGwater::dbGetQueryDT(
                sprintf(
                    paste(
                        "SELECT",
                        "    mc.datetime,",
                        "    mc.value AS value",
                        "FROM measurements_continuous mc",
                        "JOIN timeseries ts ON ts.timeseries_id = mc.timeseries_id",
                        "JOIN parameters p ON p.parameter_id = ts.parameter_id",
                        "JOIN locations l ON l.location_id = ts.location_id",
                        "WHERE l.location_code = %s",
                        "  AND p.param_name = %s",
                        "  AND mc.value IS NOT NULL",
                        paste0("  AND mc.datetime <= ", ref_ts_sql),
                        "ORDER BY mc.datetime"
                    ),
                    location_sql,
                    parameter_sql
                ),
                con = con
            ),
            error = function(e) data.frame()
        )
    }

    if (nrow(continuous) == 0 && !isTRUE(load_entire_record)) {
        continuous <- YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "SELECT",
                    "    mc.datetime,",
                    "    mc.value AS value",
                    "FROM measurements_continuous mc",
                    "JOIN timeseries ts ON ts.timeseries_id = mc.timeseries_id",
                    "JOIN parameters p ON p.parameter_id = ts.parameter_id",
                    "JOIN locations l ON l.location_id = ts.location_id",
                    "WHERE l.location_code = %s",
                    "  AND p.param_name = %s",
                    "  AND mc.value IS NOT NULL",
                    paste0("  AND mc.datetime <= ", ref_ts_sql),
                    "ORDER BY mc.datetime"
                ),
                location_sql,
                parameter_sql
            ),
            con = con
        )
    }

    daily_fallback <- YGwater::dbGetQueryDT(
        sprintf(
            paste(
                "SELECT",
                "    mcd.date::timestamp AS datetime,",
                "    mcd.value",
                "FROM measurements_calculated_daily mcd",
                "JOIN timeseries ts ON ts.timeseries_id = mcd.timeseries_id",
                "JOIN parameters p ON p.parameter_id = ts.parameter_id",
                "JOIN locations l ON l.location_id = ts.location_id",
                "WHERE l.location_code = %s",
                "  AND p.param_name = %s",
                "  AND mcd.value IS NOT NULL",
                daily_fallback_start_filter_sql,
                paste0("  AND mcd.date <= ", ref_date_sql),
                "ORDER BY mcd.date"
            ),
            location_sql,
            parameter_sql
        ),
        con = con
    )

    if (nrow(daily_fallback) == 0 && !isTRUE(load_entire_record)) {
        daily_fallback <- YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "SELECT",
                    "    mcd.date::timestamp AS datetime,",
                    "    mcd.value",
                    "FROM measurements_calculated_daily mcd",
                    "JOIN timeseries ts ON ts.timeseries_id = mcd.timeseries_id",
                    "JOIN parameters p ON p.parameter_id = ts.parameter_id",
                    "JOIN locations l ON l.location_id = ts.location_id",
                    "WHERE l.location_code = %s",
                    "  AND p.param_name = %s",
                    "  AND mcd.value IS NOT NULL",
                    paste0("  AND mcd.date <= ", ref_date_sql),
                    "ORDER BY mcd.date"
                ),
                location_sql,
                parameter_sql
            ),
            con = con
        )
    }

    continuous <- sanitize_loaded_series_values(continuous, parameter)
    daily_fallback <- sanitize_loaded_series_values(daily_fallback, parameter)
    continuous$trace_source <- "realtime_continuous"
    daily_fallback$trace_source <- "historical_daily"

    if (isTRUE(load_entire_record)) {
        if (nrow(continuous) == 0) {
            return(interpret_loaded_times_as_local(
                daily_fallback,
                time_columns = c("datetime")
            ))
        }

        if (nrow(daily_fallback) == 0) {
            return(interpret_loaded_times_as_local(
                continuous,
                time_columns = c("datetime")
            ))
        }

        first_continuous_time <- min(continuous$datetime, na.rm = TRUE)
        # Include the daily row for the same calendar day the continuous trace
        # starts so there is no visible gap between the two traces.
        first_continuous_date <- as.POSIXct(
            format(first_continuous_time, "%Y-%m-%d"),
            tz = attr(first_continuous_time, "tzone") %||% "America/Whitehorse"
        )
        daily_history <- daily_fallback[
            daily_fallback$datetime <= first_continuous_date,
        ]

        merged_series <- rbind(
            daily_history[, c("datetime", "value", "trace_source")],
            continuous[, c("datetime", "value", "trace_source")]
        )
        merged_series <- merged_series[order(merged_series$datetime), ]

        return(interpret_loaded_times_as_local(
            merged_series,
            time_columns = c("datetime")
        ))
    }

    if (nrow(continuous) == 0) {
        daily_fallback$trace_source <- "observed"
        return(interpret_loaded_times_as_local(
            daily_fallback,
            time_columns = c("datetime")
        ))
    }

    if (nrow(daily_fallback) == 0) {
        continuous$trace_source <- "observed"
        return(interpret_loaded_times_as_local(
            continuous,
            time_columns = c("datetime")
        ))
    }

    latest_continuous <- max(continuous$datetime, na.rm = TRUE)
    latest_daily <- max(daily_fallback$datetime, na.rm = TRUE)

    selected_series <- if (
        is.finite(latest_daily) &&
            (!is.finite(latest_continuous) || latest_daily > latest_continuous)
    ) {
        daily_fallback
    } else {
        continuous
    }

    selected_series$trace_source <- "observed"

    interpret_loaded_times_as_local(
        selected_series,
        time_columns = c("datetime")
    )
}


# App entrypoint ------------------------------------------------------------

#' Launch Freshet Dashboard App
#'
#' Constructs and returns the Shiny app used for freshet monitoring, report
#' export, map visualization, and image-series browsing.
#'
#' @param con DBI connection object.
#'
#' @return A `shiny.appobj`.
#' @noRd
launch_freshet_dashboard <- function(con) {
    fva_path <- file.path(
        "dev",
        "freshet_forecasting",
        "flood_vulnerable_gauges.yaml"
    )
    fva <- yaml::read_yaml(fva_path)
    communities <- names(fva)

    ui <- bslib::page_fillable(
        shiny::tags$style(shiny::HTML(paste(
            "#dashboard-panels { display:grid; gap:1rem; align-items:start; }",
            "#dashboard-panels.compact { grid-template-columns: repeat(2, minmax(0, 1fr)); }",
            "#dashboard-panels.comfortable { grid-template-columns: 1fr; }",
            ".station-plot-shell { position: relative; }",
            ".station-plot-shell.is-stale .js-plotly-plot { opacity: 0.45; filter: grayscale(1); }",
            ".station-plot-stale-banner { position: absolute; top: 0.75rem; left: 50%; transform: translateX(-50%); z-index: 10; padding: 0.35rem 0.75rem; border: 1px solid #64748b; border-radius: 999px; background: rgba(255,255,255,0.92); color: #334155; font-size: 0.9rem; font-weight: 600; pointer-events: none; }",
            "@media (max-width: 992px) { #dashboard-panels.compact { grid-template-columns: 1fr; } }",
            sep = "\n"
        ))),
        shiny::tags$script(shiny::HTML(
            paste(
                "Shiny.addCustomMessageHandler('set-dashboard-mode', function(mode) {\n  var el = document.getElementById('dashboard-panels');\n  if (!el) return;\n  el.classList.remove('compact', 'comfortable');\n  el.classList.add(mode === 'comfortable' ? 'comfortable' : 'compact');\n});",
                "Shiny.addCustomMessageHandler('set-station-plot-stale', function(isStale) {\n  var el = document.getElementById('station-plot-shell');\n  if (!el) return;\n  el.classList.toggle('is-stale', !!isStale);\n});",
                sep = "\n"
            )
        )),
        # Inputs rearranged as requested, label removed, view renamed
        bslib::accordion(
            id = "controls",
            open = TRUE,
            bslib::accordion_panel(
                "", # Remove 'Filters' label by using empty string
                # Row 1: Community, Time 0, View, Interactive, Export
                bslib::layout_columns(
                    col_widths = c(3, 3, 2, 4),
                    shiny::selectInput(
                        "community",
                        "Community",
                        choices = communities,
                        selected = communities[[1]]
                    ),
                    shiny::tags$div(
                        class = "form-group shiny-input-container",
                        style = "margin-bottom:0;",
                        shiny::tags$label(
                            `for` = "time0",
                            "Time 0",
                            class = "control-label"
                        ),
                        shiny::tags$input(
                            id = "time0",
                            type = "datetime-local",
                            class = "form-control",
                            value = format(Sys.time(), "%Y-%m-%dT%H:%M"),
                            max = format(Sys.time(), "%Y-%m-%dT%H:%M"),
                            oninput = "if(this.max && this.value > this.max){ this.value = this.max; } Shiny.setInputValue('time0', this.value, {priority: 'event'});"
                        )
                    ),
                    shiny::selectInput(
                        "view_mode",
                        "View",
                        choices = c(
                            "Compact" = "compact",
                            "Detailed" = "comfortable"
                        ),
                        selected = "compact"
                    ),
                    shiny::tags$div(
                        class = "form-group shiny-input-container",
                        style = "margin-bottom:0;",
                        shiny::tags$label(
                            `for` = "export_html_report",
                            "Export",
                            class = "control-label"
                        ),
                        shiny::downloadButton(
                            "export_html_report",
                            "Export to HTML",
                            style = "width:100%;"
                        )
                    )
                ),
                # Row 2: Inputs side by side, no bins/cards
                bslib::layout_columns(
                    col_widths = c(2.5, 2.5, 2.5, 2.5, 2),
                    shiny::selectizeInput(
                        "parameter",
                        "Primary parameter",
                        choices = NULL,
                        selected = NULL,
                        options = list(placeholder = "Choose parameter")
                    ),
                    shiny::selectizeInput(
                        "station",
                        "Primary station",
                        choices = NULL,
                        selected = NULL,
                        options = list(placeholder = "Choose station")
                    ),
                    shiny::selectizeInput(
                        "secondary_parameter",
                        "Secondary parameter",
                        choices = NULL,
                        selected = NULL,
                        options = list(placeholder = "None (optional)")
                    ),
                    shiny::selectizeInput(
                        "secondary_station",
                        "Secondary station",
                        choices = NULL,
                        selected = NULL,
                        options = list(placeholder = "None (optional)")
                    ),
                    shiny::tags$div(
                        class = "form-group shiny-input-container",
                        style = "margin-bottom:0; padding-top:1.75rem; display:flex; align-items:center; height:100%;",
                        shiny::actionButton(
                            "create_plot",
                            "Create plot",
                            class = "btn btn-primary",
                            style = "width:100%;"
                        )
                    )
                )
            )
        ),
        shiny::tags$div(
            id = "dashboard-panels",
            class = "compact",
            bslib::card(
                bslib::card_header("Summary"),
                DT::DTOutput("summary_table")
            ),
            bslib::card(
                bslib::card_header(shiny::textOutput(
                    "station_plot_title",
                    inline = TRUE
                )),
                shiny::tags$div(
                    id = "station-plot-shell",
                    class = "station-plot-shell",
                    plotly::plotlyOutput("station_plot"),
                    shiny::uiOutput("station_plot_stale_banner")
                )
            ),
            bslib::card(
                bslib::card_header(
                    "Drainage area and hydrometeorological station map"
                ),
                leaflet::leafletOutput("stations_map")
            ),
            bslib::card(
                bslib::card_header(shiny::textOutput(
                    "image_series_card_title",
                    inline = TRUE
                )),
                bslib::card_body(
                    shiny::uiOutput("image_series_navigation"),
                    shiny::uiOutput("station_image")
                )
            )
        ),
        bslib::card(
            bslib::card_body(
                shiny::tags$p(
                    shiny::tags$b("Disclaimer: "),
                    "Historical ranges and return periods are calculated on the fly using daily corrected data and have not been independently verified by a hydrologist. Note that sub-daily variance may not be captured by daily average historical ranges, especially for variables such as temperature. Real-time data may be unreliable and subject to future corrections.",
                    style = "font-size:0.85em; color:#6b7280; margin:0;"
                )
            )
        )
    )

    server <- function(input, output, session) {
        time_zero <- shiny::reactive({
            now <- Sys.time()
            now_input <- format(now, "%Y-%m-%dT%H:%M")

            session$sendInputMessage("time0", list(max = now_input))

            if (is.null(input$time0) || !nzchar(input$time0)) {
                return(now)
            }

            selected <- suppressWarnings(as.POSIXct(
                input$time0,
                format = "%Y-%m-%dT%H:%M",
                tz = Sys.timezone()
            ))

            if (is.na(selected)) {
                return(now)
            }

            pmin(selected, now)
        }) %>%
            shiny::debounce(800)

        shiny::observeEvent(
            input$view_mode,
            {
                session$sendCustomMessage(
                    "set-dashboard-mode",
                    if (is.null(input$view_mode)) {
                        "compact"
                    } else {
                        input$view_mode
                    }
                )
            },
            ignoreInit = FALSE
        )

        # Map trend helper: computes 24-hour change from the freshest source.
        get_parameter_change_24h <- function(
            location_codes,
            parameter_name,
            reference_time = NULL
        ) {
            location_codes <- unique(stats::na.omit(unlist(
                location_codes,
                use.names = FALSE
            )))
            if (length(location_codes) == 0) {
                return(data.frame(
                    location_code = character(),
                    change_24h = numeric(),
                    last_data_age_hours = numeric()
                ))
            }

            location_codes_sql <- paste(
                DBI::dbQuoteString(con, location_codes),
                collapse = ","
            )
            parameter_sql <- DBI::dbQuoteString(con, parameter_name)
            ref_ts_sql <- if (is.null(reference_time)) {
                "NOW()"
            } else {
                paste0(
                    "'",
                    format(as.POSIXct(reference_time), "%Y-%m-%d %H:%M:%S"),
                    "'::timestamp"
                )
            }
            ref_date_sql <- if (is.null(reference_time)) {
                "CURRENT_DATE"
            } else {
                paste0(
                    "'",
                    format(as.POSIXct(reference_time), "%Y-%m-%d"),
                    "'::date"
                )
            }

            YGwater::dbGetQueryDT(
                sprintf(
                    paste(
                        "WITH target_locations AS (",
                        "    SELECT UNNEST(ARRAY[%s]::text[]) AS location_code",
                        "),",
                        "target_timeseries AS (",
                        "    SELECT ts.location_id, ts.timeseries_id",
                        "    FROM timeseries ts",
                        "    JOIN locations l ON l.location_id = ts.location_id",
                        "    JOIN target_locations tl ON tl.location_code = l.location_code",
                        "    JOIN parameters p ON p.parameter_id = ts.parameter_id",
                        "    WHERE p.param_name = %s",
                        "),",
                        "continuous_latest AS (",
                        "    SELECT DISTINCT ON (tt.location_id)",
                        "        tt.location_id,",
                        "        tt.timeseries_id,",
                        "        mc.datetime AS latest_time,",
                        "        mc.value AS current_value",
                        "    FROM target_timeseries tt",
                        "    JOIN measurements_continuous mc ON mc.timeseries_id = tt.timeseries_id",
                        "    WHERE mc.value IS NOT NULL",
                        paste0("      AND mc.datetime <= ", ref_ts_sql),
                        "    ORDER BY tt.location_id, mc.datetime DESC, tt.timeseries_id",
                        "),",
                        "continuous_change AS (",
                        "    SELECT",
                        "        cl.location_id,",
                        "        cl.latest_time,",
                        paste0(
                            "        EXTRACT(EPOCH FROM (",
                            ref_ts_sql,
                            " - cl.latest_time)) / 3600.0 AS last_data_age_hours,"
                        ),
                        "        cl.current_value - prev.value AS change_24h,",
                        "        1 AS source_priority",
                        "    FROM continuous_latest cl",
                        "    LEFT JOIN LATERAL (",
                        "        SELECT mc.value AS value",
                        "        FROM measurements_continuous mc",
                        "        WHERE mc.timeseries_id = cl.timeseries_id",
                        "          AND mc.value IS NOT NULL",
                        "          AND mc.datetime <= cl.latest_time - INTERVAL '24 hours'",
                        "        ORDER BY mc.datetime DESC",
                        "        LIMIT 1",
                        "    ) prev ON TRUE",
                        "),",
                        "daily_latest AS (",
                        "    SELECT DISTINCT ON (tt.location_id)",
                        "        tt.location_id,",
                        "        tt.timeseries_id,",
                        "        mcd.date::timestamp AS latest_time,",
                        "        mcd.value AS current_value",
                        "    FROM target_timeseries tt",
                        "    JOIN measurements_calculated_daily mcd ON mcd.timeseries_id = tt.timeseries_id",
                        "    WHERE mcd.value IS NOT NULL",
                        paste0("      AND mcd.date <= ", ref_date_sql),
                        "    ORDER BY tt.location_id, mcd.date DESC, tt.timeseries_id",
                        "),",
                        "daily_change AS (",
                        "    SELECT",
                        "        dl.location_id,",
                        "        dl.latest_time,",
                        paste0(
                            "        EXTRACT(EPOCH FROM (",
                            ref_ts_sql,
                            " - dl.latest_time)) / 3600.0 AS last_data_age_hours,"
                        ),
                        "        dl.current_value - prev.value AS change_24h,",
                        "        2 AS source_priority",
                        "    FROM daily_latest dl",
                        "    LEFT JOIN LATERAL (",
                        "        SELECT mcd.value",
                        "        FROM measurements_calculated_daily mcd",
                        "        WHERE mcd.timeseries_id = dl.timeseries_id",
                        "          AND mcd.value IS NOT NULL",
                        "          AND mcd.date::timestamp <= dl.latest_time - INTERVAL '1 day'",
                        "        ORDER BY mcd.date DESC",
                        "        LIMIT 1",
                        "    ) prev ON TRUE",
                        "),",
                        "combined AS (",
                        "    SELECT location_id, latest_time, last_data_age_hours, change_24h, source_priority FROM continuous_change",
                        "    UNION ALL",
                        "    SELECT location_id, latest_time, last_data_age_hours, change_24h, source_priority FROM daily_change",
                        "),",
                        "ranked AS (",
                        "    SELECT",
                        "        c.*,",
                        "        ROW_NUMBER() OVER (",
                        "            PARTITION BY c.location_id",
                        "            ORDER BY c.latest_time DESC, c.source_priority",
                        "        ) AS rn",
                        "    FROM combined c",
                        ")",
                        "SELECT",
                        "    l.location_code,",
                        "    r.change_24h,",
                        "    r.last_data_age_hours",
                        "FROM ranked r",
                        "JOIN locations l ON l.location_id = r.location_id",
                        "WHERE r.rn = 1",
                        "ORDER BY l.location_code"
                    ),
                    location_codes_sql,
                    parameter_sql
                ),
                con = con
            )
        }

        row_aligned_flag <- function(values, n) {
            flags <- !is.na(values) & as.logical(values)
            if (length(flags) == n) {
                return(flags)
            }
            if (length(flags) == 0) {
                return(rep(FALSE, n))
            }
            rep_len(flags, n)
        }

        parameter_candidates <- c(
            "water level",
            "water flow",
            "precipitation (1wk)",
            "precipitation (24hr)",
            "temperature, air",
            "FDD",
            "DDT",
            "snow water equivalent",
            "snow depth"
        )
        snow_survey_parameters <- c("snow water equivalent", "snow depth")

        station_has_parameter <- function(stations, param) {
            if (is.null(stations) || nrow(stations) == 0) {
                return(logical(0))
            }

            if (identical(param, "FDD")) {
                if (!("temperature, air" %in% names(stations))) {
                    return(rep(FALSE, nrow(stations)))
                }
                return(row_aligned_flag(
                    stations[["temperature, air"]],
                    nrow(stations)
                ))
            }

            if (identical(param, "DDT")) {
                if (!("temperature, air" %in% names(stations))) {
                    return(rep(FALSE, nrow(stations)))
                }
                return(row_aligned_flag(
                    stations[["temperature, air"]],
                    nrow(stations)
                ))
            }

            if (param %in% c("precipitation (1wk)", "precipitation (24hr)")) {
                if (!("precipitation, total" %in% names(stations))) {
                    return(rep(FALSE, nrow(stations)))
                }
                return(row_aligned_flag(
                    stations[["precipitation, total"]],
                    nrow(stations)
                ))
            }

            if (!(param %in% names(stations))) {
                return(rep(FALSE, nrow(stations)))
            }

            row_aligned_flag(stations[[param]], nrow(stations))
        }

        # Spatial helper: link each gauge to nearest image series point.
        get_nearest_image_series_for_gauges <- function(
            location_codes,
            buffer_m = 10000
        ) {
            location_codes <- unique(stats::na.omit(unlist(
                location_codes,
                use.names = FALSE
            )))
            if (length(location_codes) == 0) {
                return(data.frame(
                    location_code = character(),
                    gauge_location_id = integer(),
                    nearest_img_series_id = integer(),
                    image_series_location_id = integer(),
                    nearest_image_series_distance_m = numeric(),
                    has_image_series_within_10km = logical()
                ))
            }

            location_codes_sql <- paste(
                DBI::dbQuoteString(con, location_codes),
                collapse = ","
            )

            YGwater::dbGetQueryDT(
                sprintf(
                    paste(
                        "WITH target_locations AS (",
                        "    SELECT UNNEST(ARRAY[%s]::text[]) AS location_code",
                        "),",
                        "gauge_points AS (",
                        "    SELECT",
                        "        l.location_id AS gauge_location_id,",
                        "        l.location_code,",
                        "        ST_SetSRID(ST_MakePoint(l.longitude, l.latitude), 4326)::geography AS gauge_geog",
                        "    FROM locations l",
                        "    JOIN target_locations tl",
                        "      ON tl.location_code = l.location_code",
                        "    WHERE l.longitude IS NOT NULL",
                        "      AND l.latitude IS NOT NULL",
                        "),",
                        "series_points AS (",
                        "    SELECT",
                        "        s.img_series_id,",
                        "        s.location_id AS image_series_location_id,",
                        "        ST_SetSRID(ST_MakePoint(l.longitude, l.latitude), 4326)::geography AS series_geog",
                        "    FROM image_series s",
                        "    JOIN locations l",
                        "      ON l.location_id = s.location_id",
                        "    WHERE l.longitude IS NOT NULL",
                        "      AND l.latitude IS NOT NULL",
                        "),",
                        "ranked_matches AS (",
                        "    SELECT",
                        "        gp.location_code,",
                        "        gp.gauge_location_id,",
                        "        sp.img_series_id AS nearest_img_series_id,",
                        "        sp.image_series_location_id,",
                        "        ST_Distance(gp.gauge_geog, sp.series_geog) AS nearest_image_series_distance_m,",
                        "        ROW_NUMBER() OVER (",
                        "            PARTITION BY gp.location_code",
                        "            ORDER BY ST_Distance(gp.gauge_geog, sp.series_geog), sp.img_series_id",
                        "        ) AS rn",
                        "    FROM gauge_points gp",
                        "    JOIN series_points sp",
                        "      ON ST_DWithin(sp.series_geog, gp.gauge_geog, %.6f)",
                        ")",
                        "SELECT",
                        "    gp.location_code,",
                        "    gp.gauge_location_id,",
                        "    rm.nearest_img_series_id,",
                        "    rm.image_series_location_id,",
                        "    rm.nearest_image_series_distance_m,",
                        "    (rm.nearest_img_series_id IS NOT NULL) AS has_image_series_within_10km",
                        "FROM gauge_points gp",
                        "LEFT JOIN ranked_matches rm",
                        "  ON rm.location_code = gp.location_code",
                        " AND rm.rn = 1",
                        "ORDER BY gp.location_code"
                    ),
                    location_codes_sql,
                    buffer_m
                ),
                con = con
            )
        }

        add_image_buffer_flag <- function(dat, location_codes) {
            if (
                is.null(dat) ||
                    nrow(dat) == 0 ||
                    !("location_code" %in% names(dat))
            ) {
                return(dat)
            }

            matches <- get_nearest_image_series_for_gauges(
                location_codes = location_codes,
                buffer_m = 10000
            )
            dat$nearest_img_series_id <- NA_integer_
            dat$image_series_location_id <- NA_integer_
            dat$nearest_image_series_distance_m <- NA_real_
            dat$has_image_series_within_10km <- FALSE

            if (nrow(matches) > 0) {
                idx <- match(dat$location_code, matches$location_code)
                dat$nearest_img_series_id <- matches$nearest_img_series_id[idx]
                dat$image_series_location_id <- matches$image_series_location_id[
                    idx
                ]
                dat$nearest_image_series_distance_m <- matches$nearest_image_series_distance_m[
                    idx
                ]
                dat$has_image_series_within_10km <- ifelse(
                    is.na(idx),
                    FALSE,
                    as.logical(matches$has_image_series_within_10km[idx])
                )
            }

            dat
        }

        # Reuse the full monitoring-stations inventory across community changes.
        all_monitoring_locations_sf <- get_monitoring_locations_as_sf(con)

        community_stations <- shiny::reactive({
            req(input$community)

            gauge_names <- unique(stats::na.omit(unlist(
                fva[[input$community]],
                use.names = FALSE
            )))
            if (length(gauge_names) == 0) {
                return(list(
                    stations = NULL,
                    gauge_codes = data.frame(),
                    basins = NULL,
                    upstream_basins = NULL
                ))
            }

            gauge_codes <- location_names_to_codes(gauge_names, con)
            if (
                !"location_code" %in% names(gauge_codes) ||
                    nrow(gauge_codes) == 0
            ) {
                return(list(
                    stations = NULL,
                    gauge_codes = gauge_codes,
                    basins = NULL,
                    upstream_basins = NULL
                ))
            }

            basins <- get_spatial_layer_as_sf(
                layer_name = "Drainage basins",
                feature_name = gauge_codes$location_code,
                con = con
            )
            stations <- get_stations_by_basin(
                location_codes = gauge_codes$location_code,
                poly = basins,
                monitoring_stations = all_monitoring_locations_sf
            )

            has_wl <- if ("water level" %in% names(stations)) {
                row_aligned_flag(stations[["water level"]], nrow(stations))
            } else {
                rep(FALSE, nrow(stations))
            }
            has_wf <- if ("water flow" %in% names(stations)) {
                row_aligned_flag(stations[["water flow"]], nrow(stations))
            } else {
                rep(FALSE, nrow(stations))
            }
            upstream_codes <- unique(stats::na.omit(stations$location_code[
                has_wl | has_wf
            ]))

            upstream_basins <- NULL
            if (length(upstream_codes) > 0) {
                upstream_basins <- get_spatial_layer_as_sf(
                    layer_name = "Drainage basins",
                    feature_name = upstream_codes,
                    con = con
                )
            }

            list(
                stations = stations,
                gauge_codes = gauge_codes,
                basins = basins,
                upstream_basins = upstream_basins
            )
        })

        normalize_image_extension <- function(format_value) {
            ext <- tolower(trimws(as.character(format_value[[1]])))
            if (is.na(ext) || !nzchar(ext)) {
                ext <- "jpg"
            }
            ext <- gsub("^\\.", "", ext)

            if (ext %in% c("jpg", "jpeg")) {
                return("jpeg")
            }
            if (ext %in% c("tif", "tiff")) {
                return("tiff")
            }
            if (ext == "svg") {
                return("svg+xml")
            }

            ext
        }

        coerce_image_blob_to_raw <- function(blob) {
            if (is.null(blob)) {
                return(NULL)
            }

            if (is.list(blob)) {
                if (length(blob) == 0) {
                    return(NULL)
                }
                return(coerce_image_blob_to_raw(blob[[1]]))
            }

            if (is.raw(blob)) {
                return(blob)
            }

            if (is.character(blob) && length(blob) == 1 && nzchar(blob)) {
                hex <- blob[[1]]
                if (startsWith(hex, "\\\\x")) {
                    hex <- substr(hex, 3, nchar(hex))
                }
                is_hex <- grepl("^[0-9A-Fa-f]+$", hex)
                if (is_hex && (nchar(hex) %% 2) == 0) {
                    bytes <- substring(
                        hex,
                        first = seq(1, nchar(hex), by = 2),
                        last = seq(2, nchar(hex), by = 2)
                    )
                    return(as.raw(strtoi(bytes, base = 16L)))
                }
            }

            if (is.numeric(blob) || is.integer(blob)) {
                vals <- as.integer(blob)
                vals <- vals[!is.na(vals)]
                if (length(vals) == 0) {
                    return(NULL)
                }
                vals <- pmin(255L, pmax(0L, vals))
                return(as.raw(vals))
            }

            stop("Unsupported image byte format")
        }

        image_blob_to_data_uri <- function(image_blob, image_format) {
            raw_bytes <- coerce_image_blob_to_raw(image_blob)
            if (is.null(raw_bytes) || length(raw_bytes) == 0) {
                return(NULL)
            }

            mime <- paste0("image/", normalize_image_extension(image_format))
            base64enc::dataURI(data = raw_bytes, mime = mime)
        }

        build_report_map_plot <- function(
            basins,
            upstream_basins,
            stations,
            image_series_data
        ) {
            has_any_layer <-
                (!is.null(basins) && nrow(basins) > 0) ||
                (!is.null(upstream_basins) && nrow(upstream_basins) > 0) ||
                (!is.null(stations) && nrow(stations) > 0) ||
                (!is.null(image_series_data) && nrow(image_series_data) > 0)

            if (!has_any_layer) {
                return(
                    ggplot2::ggplot() +
                        ggplot2::theme_void() +
                        ggplot2::annotate(
                            "text",
                            x = 1,
                            y = 1,
                            label = "No map data available",
                            size = 5
                        )
                )
            }

            p <- ggplot2::ggplot() +
                ggplot2::theme_void(base_size = 11) +
                ggplot2::theme(
                    panel.background = ggplot2::element_rect(
                        fill = "#f8fafc",
                        color = NA
                    ),
                    plot.background = ggplot2::element_rect(
                        fill = "white",
                        color = NA
                    )
                )

            if (!is.null(basins) && nrow(basins) > 0) {
                p <- p +
                    ggplot2::geom_sf(
                        data = basins,
                        fill = grDevices::adjustcolor(
                            "#4b5563",
                            alpha.f = 0.08
                        ),
                        color = "#4b5563",
                        linewidth = 0.4
                    )
            }

            if (!is.null(upstream_basins) && nrow(upstream_basins) > 0) {
                p <- p +
                    ggplot2::geom_sf(
                        data = upstream_basins,
                        fill = grDevices::adjustcolor(
                            "#0f766e",
                            alpha.f = 0.08
                        ),
                        color = "#0f766e",
                        linewidth = 0.5
                    )
            }

            if (!is.null(stations) && nrow(stations) > 0) {
                has_wl <- if ("water level" %in% names(stations)) {
                    row_aligned_flag(stations[["water level"]], nrow(stations))
                } else {
                    rep(FALSE, nrow(stations))
                }
                has_wf <- if ("water flow" %in% names(stations)) {
                    row_aligned_flag(stations[["water flow"]], nrow(stations))
                } else {
                    rep(FALSE, nrow(stations))
                }
                is_hydrometric <- has_wl | has_wf

                stations_triangle <- stations[which(is_hydrometric), ]
                stations_circle <- stations[which(!is_hydrometric), ]

                if (nrow(stations_circle) > 0) {
                    p <- p +
                        ggplot2::geom_sf(
                            data = stations_circle,
                            shape = 21,
                            size = 2.2,
                            stroke = 0.4,
                            color = "#1d4ed8",
                            fill = "#2563eb"
                        )
                }

                if (nrow(stations_triangle) > 0) {
                    p <- p +
                        ggplot2::geom_sf(
                            data = stations_triangle,
                            shape = 24,
                            size = 2.8,
                            stroke = 0.4,
                            color = "#1d4ed8",
                            fill = "#2563eb"
                        )
                }
            }

            if (!is.null(image_series_data) && nrow(image_series_data) > 0) {
                p <- p +
                    ggplot2::geom_sf(
                        data = image_series_data,
                        shape = 21,
                        size = 2.2,
                        stroke = 0.5,
                        color = "#7e22ce",
                        fill = "#a855f7"
                    )
            }

            p + ggplot2::coord_sf(expand = TRUE)
        }

        # Report export pipeline (static HTML with embedded PNG plots).
        output$export_html_report <- shiny::downloadHandler(
            filename = function() {
                community_name <- if (is.null(input$community)) {
                    "community"
                } else {
                    input$community
                }
                community_slug <- gsub(
                    "[^A-Za-z0-9_-]",
                    "-",
                    community_name
                )
                paste0(
                    "freshet-report-",
                    community_slug,
                    "-",
                    format(Sys.time(), "%Y%m%d-%H%M"),
                    ".html"
                )
            },
            content = function(file) {
                community_data <- community_stations()
                stations <- community_data$stations
                basins <- community_data$basins
                upstream_basins <- community_data$upstream_basins
                reference_ts <- as.POSIXct(time_zero())
                write_report_html <- function(blocks, target_file) {
                    report_tag <- htmltools::browsable(
                        do.call(htmltools::tagList, blocks)
                    )

                    tmp_dir <- tempfile("freshet-report-")
                    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
                    tmp_html <- file.path(tmp_dir, "report.html")

                    htmltools::save_html(
                        report_tag,
                        file = tmp_html,
                        libdir = "report_files"
                    )

                    can_self_contain <-
                        requireNamespace("rmarkdown", quietly = TRUE) &&
                        nzchar(Sys.which("pandoc"))

                    if (can_self_contain) {
                        rmarkdown::pandoc_self_contained_html(
                            input = tmp_html,
                            output = target_file
                        )
                    } else {
                        file.copy(tmp_html, target_file, overwrite = TRUE)
                    }

                    invisible(NULL)
                }

                report_blocks <- list(
                    shiny::tags$style(shiny::HTML(
                        paste(
                            "body { font-family: 'Segoe UI', Arial, sans-serif; margin: 1.25rem; color: #0f172a; }",
                            ".report-title { margin: 0 0 0.35rem 0; color: #0f172a; border-bottom: 3px solid #0f766e; padding-bottom: 0.4rem; }",
                            ".report-meta { margin: 0.15rem 0; color: #475569; font-size: 0.95rem; }",
                            ".report-section { margin-top: 1.25rem; margin-bottom: 0.5rem; color: #0f766e; border-left: 4px solid #0f766e; padding-left: 0.5rem; }",
                            ".report-station { margin-top: 0.85rem; margin-bottom: 0.35rem; color: #1e3a8a; }",
                            sep = "\n"
                        )
                    )),
                    shiny::tags$h1(
                        sprintf(
                            "Freshet report: %s",
                            input$community
                        ),
                        class = "report-title"
                    ),
                    shiny::tags$p(
                        sprintf(
                            "Generated: %s",
                            format(Sys.time(), "%Y-%m-%d %H:%M")
                        ),
                        class = "report-meta"
                    ),
                    shiny::tags$p(
                        sprintf(
                            "Reference time (Time 0): %s",
                            format(reference_ts, "%Y-%m-%d %H:%M")
                        ),
                        class = "report-meta"
                    ),
                    shiny::tags$hr()
                )

                # Map section
                if (!is.null(stations) && nrow(stations) > 0) {
                    station_labels <- if ("name" %in% names(stations)) {
                        as.character(stations$name)
                    } else {
                        as.character(stations$location_code)
                    }
                    station_labels[
                        is.na(station_labels) | !nzchar(station_labels)
                    ] <- as.character(
                        stations$location_code[
                            is.na(station_labels) | !nzchar(station_labels)
                        ]
                    )
                    stations$report_tooltip <- paste0(
                        station_labels,
                        " (",
                        stations$location_code,
                        ")"
                    )

                    stations$variables_available <- vapply(
                        seq_len(nrow(stations)),
                        function(i) {
                            available <- parameter_candidates[
                                vapply(
                                    parameter_candidates,
                                    function(param) {
                                        flags <- station_has_parameter(
                                            stations,
                                            param
                                        )
                                        length(flags) >= i &&
                                            isTRUE(flags[[i]])
                                    },
                                    logical(1)
                                )
                            ]
                            if (length(available) == 0) {
                                return("None")
                            }
                            paste(available, collapse = ", ")
                        },
                        character(1)
                    )

                    stations$popup_html <- vapply(
                        seq_len(nrow(stations)),
                        function(i) {
                            code <- as.character(stations$location_code[[i]])
                            label <- station_labels[[i]]

                            sprintf(
                                "<b>%s</b><br>Code: %s<br>Variables available: %s",
                                htmltools::htmlEscape(label),
                                htmltools::htmlEscape(code),
                                htmltools::htmlEscape(stations$variables_available[[
                                    i
                                ]])
                            )
                        },
                        character(1)
                    )
                }

                image_series_data <- community_image_series()
                if (
                    !is.null(image_series_data) && nrow(image_series_data) > 0
                ) {
                    image_series_tooltip <- paste0(
                        "Image series: ",
                        image_series_data$name,
                        " (",
                        image_series_data$location_code,
                        ")"
                    )
                    image_series_tooltip[
                        is.na(image_series_data$name) |
                            !nzchar(image_series_data$name)
                    ] <-
                        paste0(
                            "Image series: ",
                            image_series_data$location_code[
                                is.na(image_series_data$name) |
                                    !nzchar(image_series_data$name)
                            ]
                        )
                }

                report_map_plot <- build_report_map_plot(
                    basins = basins,
                    upstream_basins = upstream_basins,
                    stations = stations,
                    image_series_data = image_series_data
                )

                report_blocks <- c(
                    report_blocks,
                    list(
                        shiny::tags$h2("Map", class = "report-section"),
                        {
                            map_png <- plot_ggplot_to_data_uri(
                                report_map_plot,
                                width = 1200,
                                height = 600
                            )
                            if (!is.null(map_png)) {
                                shiny::tags$img(
                                    src = map_png,
                                    style = "max-width:100%; height:auto; border:1px solid #ccc;"
                                )
                            } else {
                                shiny::tags$p(
                                    "Unable to render static map."
                                )
                            }
                        },
                        shiny::tags$hr()
                    )
                )

                # Photo section
                report_blocks <- c(
                    report_blocks,
                    list(shiny::tags$h2("Photos", class = "report-section"))
                )

                if (
                    is.null(image_series_data) || nrow(image_series_data) == 0
                ) {
                    report_blocks <- c(
                        report_blocks,
                        list(
                            shiny::tags$p(
                                "No image series found upstream of the selected community."
                            ),
                            shiny::tags$hr()
                        )
                    )
                } else {
                    time0_sql <- DBI::dbQuoteString(
                        con,
                        format(reference_ts, "%Y-%m-%d %H:%M:%S")
                    )

                    for (i in seq_len(nrow(image_series_data))) {
                        img_series_id <- image_series_data$img_series_id[[i]]
                        photo_label <- as.character(image_series_data$name[[i]])
                        if (is.na(photo_label) || !nzchar(photo_label)) {
                            photo_label <- as.character(
                                image_series_data$location_code[[i]]
                            )
                        }

                        latest_image <- DBI::dbGetQuery(
                            con,
                            paste0(
                                "SELECT image_id, datetime FROM images WHERE img_series_id = ",
                                img_series_id,
                                " AND datetime <= ",
                                time0_sql,
                                " ORDER BY datetime DESC LIMIT 1"
                            )
                        )

                        if (
                            nrow(latest_image) == 0 ||
                                is.na(latest_image$datetime[[1]])
                        ) {
                            next
                        }

                        image_id <- latest_image$image_id[[1]]
                        image <- DBI::dbGetQuery(
                            con,
                            paste0(
                                "SELECT format, file FROM images WHERE image_id = ",
                                image_id
                            )
                        )

                        if (nrow(image) == 0 || is.null(image$file[[1]])) {
                            next
                        }

                        img_src <- tryCatch(
                            image_blob_to_data_uri(
                                image_blob = image$file[[1]],
                                image_format = image$format[[1]]
                            ),
                            error = function(e) NULL
                        )

                        if (is.null(img_src)) {
                            next
                        }

                        report_blocks <- c(
                            report_blocks,
                            list(
                                shiny::tags$h3(photo_label),
                                shiny::tags$p(sprintf(
                                    "Captured: %s",
                                    format(
                                        as.POSIXct(latest_image$datetime[[1]]),
                                        "%Y-%m-%d %H:%M"
                                    )
                                )),
                                shiny::tags$img(
                                    src = img_src,
                                    style = "max-width:100%; max-height:420px; object-fit:contain;"
                                ),
                                shiny::tags$hr()
                            )
                        )
                    }
                }

                if (is.null(stations) || nrow(stations) == 0) {
                    report_blocks <- c(
                        report_blocks,
                        list(shiny::tags$p(
                            "No stations found for the selected community."
                        ))
                    )
                    write_report_html(report_blocks, file)
                    return(invisible(NULL))
                }

                available_parameters <- parameter_candidates[
                    vapply(
                        parameter_candidates,
                        function(param) {
                            any(station_has_parameter(stations, param))
                        },
                        logical(1)
                    )
                ]

                if (length(available_parameters) == 0) {
                    report_blocks <- c(
                        report_blocks,
                        list(shiny::tags$p(
                            "No available parameters for the selected community."
                        ))
                    )
                } else {
                    for (param in available_parameters) {
                        report_blocks <- c(
                            report_blocks,
                            list(
                                shiny::tags$h2(param, class = "report-section")
                            )
                        )

                        station_codes <- unique(stats::na.omit(
                            stations$location_code[
                                station_has_parameter(stations, param)
                            ]
                        ))

                        if (length(station_codes) == 0) {
                            report_blocks <- c(
                                report_blocks,
                                list(shiny::tags$p(
                                    "No stations with this parameter."
                                ))
                            )
                            next
                        }

                        parameter_summary <- build_summary_data_for_parameter(
                            param = param,
                            codes = station_codes,
                            stations = stations,
                            reference_time = reference_ts
                        )

                        if (
                            !is.null(parameter_summary) &&
                                nrow(parameter_summary) > 0
                        ) {
                            report_blocks <- c(
                                report_blocks,
                                list(summary_table_html(parameter_summary))
                            )
                        }

                        for (code in station_codes) {
                            station_label <- code
                            if ("name" %in% names(stations)) {
                                idx <- match(code, stations$location_code)
                                if (
                                    !is.na(idx) &&
                                        !is.na(stations$name[[idx]]) &&
                                        nzchar(stations$name[[idx]])
                                ) {
                                    station_label <- sprintf(
                                        "%s [%s]",
                                        stations$name[[idx]],
                                        code
                                    )
                                } else {
                                    station_label <- sprintf("[%s]", code)
                                }
                            } else {
                                station_label <- sprintf("[%s]", code)
                            }

                            station_plot <- build_station_static_plot(
                                location_code = code,
                                parameter = param,
                                reference_time = reference_ts,
                                con = con,
                                snow_survey_parameters = snow_survey_parameters,
                                load_entire_record = FALSE
                            )

                            if (is.null(station_plot)) {
                                report_blocks <- c(
                                    report_blocks,
                                    list(
                                        shiny::tags$h3(
                                            station_label,
                                            class = "report-station"
                                        ),
                                        shiny::tags$p(
                                            "Unable to generate this plot."
                                        ),
                                        shiny::tags$hr()
                                    )
                                )
                            } else {
                                static_plot <- plot_ggplot_to_data_uri(
                                    station_plot,
                                    width = 900,
                                    height = 500
                                )
                                plot_to_add <- if (!is.null(static_plot)) {
                                    shiny::tags$img(
                                        src = static_plot,
                                        style = "max-width:100%; height:auto; border:1px solid #ddd;"
                                    )
                                } else {
                                    shiny::tags$p(
                                        "Could not render static plot."
                                    )
                                }

                                report_blocks <- c(
                                    report_blocks,
                                    list(
                                        shiny::tags$h3(
                                            station_label,
                                            class = "report-station"
                                        ),
                                        plot_to_add,
                                        shiny::tags$hr()
                                    )
                                )
                            }
                        }
                    }
                }

                write_report_html(report_blocks, file)
            }
        )

        get_image_series_locations_as_sf <- function(con) {
            image_series_locations <- YGwater::dbGetQueryDT(
                paste(
                    "SELECT",
                    "    s.img_series_id,",
                    "    s.location_id,",
                    "    l.location_code,",
                    "    l.name,",
                    "    l.latitude,",
                    "    l.longitude",
                    "FROM image_series s",
                    "JOIN locations l",
                    "  ON l.location_id = s.location_id",
                    "WHERE l.latitude IS NOT NULL",
                    "  AND l.longitude IS NOT NULL",
                    "ORDER BY l.name, s.img_series_id"
                ),
                con = con
            )

            sf::st_as_sf(
                image_series_locations,
                coords = c("longitude", "latitude"),
                crs = 4326,
                remove = FALSE
            )
        }

        image_series_locations_sf <- get_image_series_locations_as_sf(con)
        selected_image_series_index <- shiny::reactiveVal(1L)

        community_image_series <- shiny::reactive({
            basins <- community_stations()$basins
            req(!is.null(basins), nrow(basins) > 0)

            image_series_sf <- image_series_locations_sf
            if (sf::st_crs(image_series_sf) != sf::st_crs(basins)) {
                image_series_sf <- sf::st_transform(
                    image_series_sf,
                    sf::st_crs(basins)
                )
            }

            basin_cols <- intersect(
                c("feature_name", "layer_name"),
                names(basins)
            )
            # Buffer basins by 25km for image series discovery
            basins_buffered <- sf::st_buffer(
                basins[basin_cols],
                25000 # 25 km in meters (if in projected CRS)
            )
            series_in_basins <- sf::st_join(
                image_series_sf,
                basins_buffered,
                join = sf::st_intersects,
                left = FALSE
            )

            if (nrow(series_in_basins) == 0) {
                return(series_in_basins)
            }

            series_in_basins <- series_in_basins[
                order(series_in_basins$name, series_in_basins$img_series_id),
            ]
            series_in_basins[!duplicated(series_in_basins$img_series_id), ]
        })

        selected_image_series <- shiny::reactive({
            series <- community_image_series()
            req(nrow(series) > 0)

            idx <- selected_image_series_index()
            if (
                length(idx) != 1 || is.na(idx) || idx < 1 || idx > nrow(series)
            ) {
                idx <- 1L
                selected_image_series_index(idx)
            }

            series[idx, , drop = FALSE]
        })

        shiny::observeEvent(
            input$community,
            {
                selected_image_series_index(1L)
            },
            ignoreInit = FALSE
        )

        shiny::observeEvent(
            community_image_series(),
            {
                series <- community_image_series()
                if (nrow(series) == 0) {
                    selected_image_series_index(1L)
                    return()
                }

                idx <- selected_image_series_index()
                if (
                    length(idx) != 1 ||
                        is.na(idx) ||
                        idx < 1 ||
                        idx > nrow(series)
                ) {
                    selected_image_series_index(1L)
                }
            },
            ignoreInit = FALSE
        )

        shiny::observeEvent(input$previous_image_series, {
            series <- community_image_series()
            req(nrow(series) > 0)

            idx <- selected_image_series_index()
            if (
                length(idx) != 1 || is.na(idx) || idx < 1 || idx > nrow(series)
            ) {
                idx <- 1L
            }

            selected_image_series_index(
                if (idx <= 1) nrow(series) else idx - 1L
            )
        })

        shiny::observeEvent(input$next_image_series, {
            series <- community_image_series()
            req(nrow(series) > 0)

            idx <- selected_image_series_index()
            if (
                length(idx) != 1 || is.na(idx) || idx < 1 || idx > nrow(series)
            ) {
                idx <- 1L
            }

            selected_image_series_index(
                if (idx >= nrow(series)) 1L else idx + 1L
            )
        })

        output$image_series_card_title <- shiny::renderText({
            series <- selected_image_series()
            location <- as.character(series$name[[1]])
            if (is.na(location) || !nzchar(location)) {
                location <- as.character(series$location_code[[1]])
            }

            time0_sql <- DBI::dbQuoteString(
                con,
                format(as.POSIXct(time_zero()), "%Y-%m-%d %H:%M:%S")
            )

            latest_image <- DBI::dbGetQuery(
                con,
                paste0(
                    "SELECT datetime FROM images WHERE img_series_id = ",
                    series$img_series_id[[1]],
                    " AND datetime <= ",
                    time0_sql,
                    " ORDER BY datetime DESC LIMIT 1"
                )
            )

            if (nrow(latest_image) == 0 || is.na(latest_image$datetime[[1]])) {
                return(paste("Image Series:", location))
            }

            latest_time <- format(
                as.POSIXct(latest_image$datetime[[1]]),
                "%d-%b-%y %H:%M"
            )
            paste("Image Series:", location, "(Latest:", latest_time, ")")
        })

        output$image_series_navigation <- shiny::renderUI({
            series <- community_image_series()
            if (nrow(series) == 0) {
                return(shiny::tags$div(
                    "No recent image series' found upstream of the selected community"
                ))
            }

            idx <- selected_image_series_index()
            if (
                length(idx) != 1 || is.na(idx) || idx < 1 || idx > nrow(series)
            ) {
                idx <- 1L
            }

            shiny::tags$div(
                style = "display:flex; align-items:center; gap:0.4rem; margin-bottom:0.75rem;",
                shiny::actionButton(
                    "previous_image_series",
                    "◀",
                    style = "padding:0.25rem 0.5rem; font-size:0.9rem; height:auto;"
                ),
                shiny::tags$span(
                    sprintf("%d / %d", idx, nrow(series)),
                    style = "font-size:0.9rem; min-width:3rem; text-align:center;"
                ),
                shiny::actionButton(
                    "next_image_series",
                    "▶",
                    style = "padding:0.25rem 0.5rem; font-size:0.9rem; height:auto;"
                )
            )
        })

        shiny::observeEvent(
            community_stations(),
            {
                stations <- community_stations()$stations
                if (is.null(stations) || nrow(stations) == 0) {
                    shiny::updateSelectizeInput(
                        session,
                        "parameter",
                        choices = character(0),
                        selected = character(0),
                        server = TRUE
                    )
                    return()
                }

                available <- parameter_candidates[
                    vapply(
                        parameter_candidates,
                        function(param) {
                            any(station_has_parameter(stations, param))
                        },
                        logical(1)
                    )
                ]

                selected <- if ("water level" %in% available) {
                    "water level"
                } else if (
                    !is.null(input$parameter) && input$parameter %in% available
                ) {
                    input$parameter
                } else if (length(available) > 0) {
                    available[[1]]
                } else {
                    character(0)
                }

                shiny::updateSelectizeInput(
                    session,
                    "parameter",
                    choices = available,
                    selected = selected,
                    server = TRUE
                )
            },
            ignoreInit = FALSE
        )

        primary_parameter_value <- shiny::reactive({
            stations <- community_stations()$stations
            if (is.null(stations) || nrow(stations) == 0) {
                return("")
            }

            available <- parameter_candidates[
                vapply(
                    parameter_candidates,
                    function(param) {
                        any(station_has_parameter(stations, param))
                    },
                    logical(1)
                )
            ]

            if (
                !is.null(input$parameter) &&
                    nzchar(input$parameter) &&
                    input$parameter %in% available
            ) {
                return(input$parameter)
            }

            if ("water level" %in% available) {
                return("water level")
            }

            if (length(available) > 0) {
                return(available[[1]])
            }

            ""
        })

        filtered_station_codes <- shiny::reactive({
            stations <- community_stations()$stations
            param <- primary_parameter_value()
            req(!is.null(stations), nzchar(param))

            idx <- station_has_parameter(stations, param)
            unique(stats::na.omit(stations$location_code[idx]))
        })

        build_summary_data_for_parameter <- function(
            param,
            codes,
            stations,
            reference_time
        ) {
            if (is.null(param) || !nzchar(param) || length(codes) == 0) {
                return(data.frame())
            }

            if (identical(param, "water level")) {
                dat <- get_instantaneous_timeseries_summary(
                    location_codes = codes,
                    con = con,
                    historical_median = FALSE,
                    reference_time = reference_time
                )
                dat <- add_image_buffer_flag(dat, codes)
                return(sort_summary_by_drainage_area(dat, stations))
            }

            if (
                param %in%
                    c("precipitation (1wk)", "precipitation (24hr)")
            ) {
                dat <- get_precipitation_timeseries_summary(
                    location_codes = codes,
                    con = con,
                    historical_median = FALSE,
                    reference_time = reference_time
                )
                return(add_image_buffer_flag(dat, codes))
            }

            if (param %in% snow_survey_parameters) {
                dat <- get_snow_survey_summary(
                    location_codes = codes,
                    parameter = param,
                    con = con,
                    reference_time = reference_time
                )
                return(add_image_buffer_flag(dat, codes))
            }

            if (identical(param, "FDD")) {
                dat <- get_fdd_summary(
                    location_codes = codes,
                    reference_time = reference_time,
                    con = con
                )
                return(add_image_buffer_flag(dat, codes))
            }

            if (identical(param, "DDT")) {
                dat <- get_ddt_summary(
                    location_codes = codes,
                    reference_time = reference_time,
                    con = con
                )
                return(add_image_buffer_flag(dat, codes))
            }

            dat <- get_latest_parameter_summary(
                location_codes = codes,
                parameter = param,
                reference_time = reference_time,
                con = con
            )

            dat <- add_image_buffer_flag(dat, codes)

            if (identical(param, "water flow")) {
                return(sort_summary_by_drainage_area(dat, stations))
            }

            dat
        }

        summary_data <- shiny::reactive({
            param <- primary_parameter_value()
            req(nzchar(param))
            codes <- filtered_station_codes()
            if (length(codes) == 0) {
                return(data.frame())
            }

            stations <- community_stations()$stations
            build_summary_data_for_parameter(
                param = param,
                codes = codes,
                stations = stations,
                reference_time = time_zero()
            )
        })

        pretty_summary_col_names <- function(nms) {
            out <- gsub("_", " ", nms)
            out <- tools::toTitleCase(out)
            out <- gsub("\\b24h\\b", "24 h", out, ignore.case = TRUE)
            out <- gsub("\\b48h\\b", "48 h", out, ignore.case = TRUE)
            out <- gsub("\\b72h\\b", "72 h", out, ignore.case = TRUE)
            out <- gsub("\\b1w\\b", "1 week", out, ignore.case = TRUE)
            out <- gsub("\\b1m\\b", "1 month", out, ignore.case = TRUE)
            out <- gsub("\\b6m\\b", "6 months", out, ignore.case = TRUE)
            out <- gsub("\\bId\\b", "ID", out)
            out <- gsub("\\bCode\\b", "Station code", out)
            out <- gsub("\\bName\\b", "Station name", out)
            out <- gsub("^Latest Time$", "Latest observation time", out)
            out <- gsub("^Current Value$", "Current value", out)
            out <- gsub("^Current Water Level$", "Current water level", out)
            out <- gsub("^Last Data Age Hours$", "Data age (hours)", out)
            out <- gsub("^Relative To Median$", "Relative to median (%)", out)
            out
        }

        prepare_summary_display_data <- function(dat) {
            dat_display <- as.data.frame(dat)
            hidden_cols <- intersect(
                c(
                    "location_id",
                    "timeseries_id",
                    "nearest_img_series_id",
                    "image_series_location_id",
                    "nearest_image_series_distance_m",
                    "has_image_series_within_10km"
                ),
                names(dat_display)
            )
            if (length(hidden_cols) > 0) {
                dat_display <- dat_display[setdiff(
                    names(dat_display),
                    hidden_cols
                )]
            }

            if ("latest_time" %in% names(dat_display)) {
                latest_time <- dat_display$latest_time
                if (inherits(latest_time, c("POSIXct", "POSIXt"))) {
                    dat_display$latest_time <- format(
                        latest_time,
                        "%d-%b-%y, %H:%M"
                    )
                } else {
                    parsed_latest_time <- suppressWarnings(as.POSIXct(
                        latest_time,
                        tz = "America/Whitehorse"
                    ))
                    keep_original <- is.na(parsed_latest_time)
                    formatted_latest_time <- format(
                        parsed_latest_time,
                        "%d-%b-%y, %H:%M"
                    )
                    formatted_latest_time[
                        keep_original
                    ] <- as.character(latest_time[keep_original])
                    dat_display$latest_time <- formatted_latest_time
                }
            }

            names(dat_display) <- pretty_summary_col_names(names(dat_display))

            numeric_cols <- vapply(dat_display, is.numeric, logical(1))
            if (any(numeric_cols)) {
                cols_to_round <- names(dat_display)[numeric_cols]
                dat_display[cols_to_round] <- lapply(
                    dat_display[cols_to_round],
                    signif,
                    digits = 3
                )
            }

            dat_display
        }

        summary_table_html <- function(dat) {
            dat_display <- prepare_summary_display_data(dat)
            if (nrow(dat_display) == 0) {
                return(shiny::tags$p("No stations for this parameter."))
            }

            shiny::tags$table(
                style = "width:100%; border-collapse:collapse; margin:0.5rem 0 1rem 0; font-size:0.92rem;",
                shiny::tags$thead(
                    shiny::tags$tr(
                        lapply(
                            names(dat_display),
                            function(col) {
                                shiny::tags$th(
                                    col,
                                    style = "text-align:left; padding:0.45rem; border-bottom:1px solid #cbd5e1; background:#f8fafc;"
                                )
                            }
                        )
                    )
                ),
                shiny::tags$tbody(
                    lapply(
                        seq_len(nrow(dat_display)),
                        function(i) {
                            shiny::tags$tr(
                                lapply(
                                    dat_display[i, , drop = FALSE],
                                    function(val) {
                                        shiny::tags$td(
                                            as.character(val[[1]]),
                                            style = "padding:0.45rem; border-bottom:1px solid #e2e8f0; vertical-align:top;"
                                        )
                                    }
                                )
                            )
                        }
                    )
                )
            )
        }

        # Keep the Station selectize in sync with summary_data rows.
        shiny::observeEvent(
            summary_data(),
            {
                dat <- summary_data()
                if (is.null(dat) || nrow(dat) == 0) {
                    shiny::updateSelectizeInput(
                        session,
                        "station",
                        choices = character(0),
                        selected = character(0),
                        server = TRUE
                    )
                    return()
                }

                labels <- format_station_choices(dat)

                current <- input$station
                new_selected <- if (
                    !is.null(current) &&
                        nzchar(current) &&
                        current %in% dat$location_code
                ) {
                    current
                } else {
                    character(0)
                }

                shiny::updateSelectizeInput(
                    session,
                    "station",
                    choices = labels,
                    selected = new_selected,
                    server = TRUE
                )
            },
            ignoreInit = FALSE
        )

        # When a table row is clicked, sync the selectize to match.
        shiny::observeEvent(
            input$summary_table_rows_selected,
            {
                dat <- summary_data()
                selected_row <- input$summary_table_rows_selected
                if (
                    length(selected_row) == 1 &&
                        selected_row >= 1 &&
                        selected_row <= nrow(dat)
                ) {
                    code <- dat$location_code[[selected_row]]
                    if (!identical(input$station, code)) {
                        shiny::updateSelectizeInput(
                            session,
                            "station",
                            selected = code
                        )
                    }
                }
            },
            ignoreNULL = TRUE
        )

        # Populate secondary_parameter choices from community stations.
        shiny::observeEvent(
            community_stations(),
            {
                stations <- community_stations()$stations
                if (is.null(stations) || nrow(stations) == 0) {
                    shiny::updateSelectizeInput(
                        session,
                        "secondary_parameter",
                        choices = character(0),
                        selected = character(0),
                        server = TRUE
                    )
                    return()
                }

                available <- parameter_candidates[
                    vapply(
                        parameter_candidates,
                        function(param) {
                            any(station_has_parameter(stations, param))
                        },
                        logical(1)
                    )
                ]

                current <- input$secondary_parameter
                new_selected <- if (
                    !is.null(current) &&
                        nzchar(current) &&
                        current %in% available
                ) {
                    current
                } else {
                    character(0)
                }

                shiny::updateSelectizeInput(
                    session,
                    "secondary_parameter",
                    choices = available,
                    selected = new_selected,
                    server = TRUE
                )
            },
            ignoreInit = FALSE
        )

        # Update secondary_station choices when secondary_parameter changes.
        shiny::observeEvent(
            list(input$secondary_parameter, community_stations()),
            {
                sec_param <- input$secondary_parameter
                stations <- community_stations()$stations
                if (
                    is.null(sec_param) ||
                        !nzchar(sec_param) ||
                        is.null(stations) ||
                        nrow(stations) == 0
                ) {
                    shiny::updateSelectizeInput(
                        session,
                        "secondary_station",
                        choices = character(0),
                        selected = character(0),
                        server = TRUE
                    )
                    return()
                }

                idx <- station_has_parameter(stations, sec_param)
                eligible <- stations[idx, , drop = FALSE]

                if (is.null(eligible) || nrow(eligible) == 0) {
                    shiny::updateSelectizeInput(
                        session,
                        "secondary_station",
                        choices = character(0),
                        selected = character(0),
                        server = TRUE
                    )
                    return()
                }

                labels <- format_station_choices(eligible)

                current <- input$secondary_station
                new_selected <- if (
                    !is.null(current) &&
                        nzchar(current) &&
                        current %in% eligible$location_code
                ) {
                    current
                } else {
                    character(0)
                }

                shiny::updateSelectizeInput(
                    session,
                    "secondary_station",
                    choices = labels,
                    selected = new_selected,
                    server = TRUE
                )
            },
            ignoreInit = FALSE
        )

        output$summary_table <- DT::renderDT({
            dat <- summary_data()
            if (nrow(dat) == 0) {
                return(DT::datatable(
                    data.frame(message = "No stations for current filters"),
                    options = list(dom = "t"),
                    rownames = FALSE
                ))
            }

            # Identify change columns before processing
            original_dat <- as.data.frame(dat)
            change_col_names <- grep(
                "change",
                names(original_dat),
                value = TRUE,
                ignore.case = TRUE
            )

            dt_options <- list(
                pageLength = 8,
                scrollX = TRUE,
                dom = "tip",
                searching = FALSE,
                lengthChange = FALSE
            )
            if (input$parameter %in% c("water level", "water flow")) {
                # Keep the incoming row order (already sorted by drainage area).
                dt_options$order <- list()
            }

            result_table <- DT::datatable(
                prepare_summary_display_data(dat),
                selection = "single",
                options = dt_options,
                rownames = FALSE
            )

            # Apply conditional formatting to change columns
            if (length(change_col_names) > 0) {
                # Determine which positions change columns are in the final display
                final_names <- names({
                    dat_display <- as.data.frame(dat)
                    hidden_cols <- intersect(
                        c(
                            "location_id",
                            "timeseries_id",
                            "nearest_img_series_id",
                            "image_series_location_id",
                            "nearest_image_series_distance_m",
                            "has_image_series_within_10km"
                        ),
                        names(dat_display)
                    )
                    if (length(hidden_cols) > 0) {
                        dat_display <- dat_display[setdiff(
                            names(dat_display),
                            hidden_cols
                        )]
                    }
                    dat_display
                })
                change_positions <- which(final_names %in% change_col_names)

                if (length(change_positions) > 0) {
                    result_table <- result_table %>%
                        DT::formatStyle(
                            columns = change_positions,
                            backgroundColor = DT::styleInterval(
                                c(-0.5, 0.5),
                                c("#86efac", "#fef08a", "#fca5a5")
                            )
                        )
                }
            }

            result_table
        })

        selected_station_code <- shiny::reactive({
            dat <- summary_data()
            req(nrow(dat) > 0)

            # Selectize input takes priority; fall back to table row.
            if (
                !is.null(input$station) &&
                    nzchar(input$station) &&
                    input$station %in% dat$location_code
            ) {
                return(input$station)
            }

            selected <- input$summary_table_rows_selected
            if (
                length(selected) == 1 && selected >= 1 && selected <= nrow(dat)
            ) {
                return(dat$location_code[[selected]])
            }

            ""
        })

        primary_station_for_plot <- shiny::reactive({
            dat <- summary_data()
            if (is.null(dat) || nrow(dat) == 0) {
                return("")
            }

            selected_code <- selected_station_code()
            if (!is.null(selected_code) && nzchar(selected_code)) {
                return(selected_code)
            }

            dat$location_code[[1]]
        })

        format_station_choices <- function(stations) {
            if (is.null(stations) || nrow(stations) == 0) {
                return(character(0))
            }
            codes <- as.character(stations$location_code)
            # Use only the station name for display; fallback to code if missing
            station_names <- if ("name" %in% names(stations)) {
                as.character(stations$name)
            } else {
                codes
            }
            station_names[
                is.na(station_names) | !nzchar(station_names)
            ] <- codes[is.na(station_names) | !nzchar(station_names)]
            setNames(codes, station_names)
        }

        station_label_for_code <- function(code) {
            code <- code %||% ""
            if (!nzchar(code)) {
                return("")
            }

            stations <- tryCatch(
                community_stations()$stations,
                error = function(e) NULL
            )
            if (!is.null(stations) && nrow(stations) > 0) {
                idx <- match(code, as.character(stations$location_code))
                if (!is.na(idx) && "name" %in% names(stations)) {
                    station_name <- as.character(stations$name[[idx]])
                    if (!is.na(station_name) && nzchar(station_name)) {
                        return(sprintf("%s (%s)", station_name, code))
                    }
                }
            }

            dat <- tryCatch(summary_data(), error = function(e) NULL)
            if (!is.null(dat) && nrow(dat) > 0 && "name" %in% names(dat)) {
                idx <- match(code, as.character(dat$location_code))
                if (!is.na(idx)) {
                    station_name <- as.character(dat$name[[idx]])
                    if (!is.na(station_name) && nzchar(station_name)) {
                        return(sprintf("%s (%s)", station_name, code))
                    }
                }
            }

            code
        }

        normalize_plot_request <- function(request) {
            list(
                station = request$station %||% "",
                parameter = request$parameter %||% "",
                secondary_station = request$secondary_station %||% "",
                secondary_parameter = request$secondary_parameter %||% "",
                reference_time = request$reference_time %||% ""
            )
        }

        current_station_plot_signature <- shiny::reactive({
            normalize_plot_request(list(
                station = plot_key()$station,
                parameter = plot_key()$parameter,
                secondary_station = secondary_plot_key()$station,
                secondary_parameter = secondary_plot_key()$parameter,
                reference_time = input$time0 %||% ""
            ))
        })

        # ---------------------------------------------------------------------------
        # Bundle all plot inputs and data fetching into a single reactive request.
        #
        # This keeps the plot from rendering against intermediate states while the
        # station, parameter, reference time, and optional secondary series are
        # still updating in separate flush cycles.
        # ---------------------------------------------------------------------------

        plot_key <- shiny::reactive({
            list(
                station = primary_station_for_plot(),
                parameter = primary_parameter_value()
            )
        })

        secondary_plot_key <- shiny::reactive({
            list(
                station = input$secondary_station %||% "",
                parameter = input$secondary_parameter %||% ""
            )
        })

        create_plot_pending <- shiny::reactiveVal(FALSE)
        station_plot_request <- shiny::reactiveVal(NULL)

        shiny::observeEvent(
            input$create_plot,
            {
                create_plot_pending(TRUE)
            },
            ignoreInit = TRUE
        )

        shiny::observe({
            if (!isTRUE(create_plot_pending())) {
                return()
            }

            # Touch summary_data so the first click can wait until the current
            # primary parameter/station context has finished resolving.
            summary_data()

            request <- list(
                station = plot_key()$station,
                parameter = plot_key()$parameter,
                secondary_station = secondary_plot_key()$station,
                secondary_parameter = secondary_plot_key()$parameter,
                reference_time = time_zero(),
                request_signature = current_station_plot_signature()
            )

            if (!nzchar(request$station) || !nzchar(request$parameter)) {
                return()
            }

            station_plot_request(request)
            create_plot_pending(FALSE)
        })

        station_plot_is_stale <- shiny::reactive({
            request <- station_plot_request()
            if (
                is.null(input$create_plot) ||
                    input$create_plot < 1 ||
                    is.null(request)
            ) {
                return(FALSE)
            }

            !identical(
                current_station_plot_signature(),
                request$request_signature
            )
        })

        station_plot_bundle <- shiny::reactive({
            request <- station_plot_request()
            req(!is.null(request))

            code <- request$station
            param <- request$parameter
            if (!nzchar(code) || !nzchar(param)) {
                return(list(
                    request = request,
                    ready = FALSE,
                    continuous = NULL,
                    percentiles = data.frame(),
                    return_periods = data.frame(),
                    secondary = NULL
                ))
            }

            if (param %in% snow_survey_parameters) {
                return(list(
                    request = request,
                    ready = TRUE,
                    continuous = NULL,
                    percentiles = data.frame(),
                    return_periods = data.frame(),
                    secondary = NULL
                ))
            }

            continuous <- tryCatch(
                get_station_timeseries(
                    location_code = code,
                    parameter = param,
                    reference_time = request$reference_time,
                    load_entire_record = FALSE,
                    con = con,
                    historical_start_year = 2020L
                ),
                error = function(e) NULL
            )

            if (is.null(continuous)) {
                return(list(
                    request = request,
                    ready = FALSE,
                    continuous = NULL,
                    percentiles = data.frame(),
                    return_periods = data.frame(),
                    secondary = NULL
                ))
            }

            percentiles <- tryCatch(
                get_daily_percentiles(
                    location_codes = code,
                    parameter = param,
                    con = con,
                    historical_start_year = 2020L
                ),
                error = function(e) data.frame()
            )

            return_periods <- if (param %in% c("water flow", "water level")) {
                tryCatch(
                    get_return_period_discharge(
                        location_codes = code,
                        parameter = param,
                        con = con
                    ),
                    error = function(e) data.frame()
                )
            } else {
                data.frame()
            }

            secondary <- NULL
            secondary_limits <- list(p10 = NA_real_, p90 = NA_real_)
            sec_param <- request$secondary_parameter
            sec_station <- request$secondary_station
            if (
                !is.null(sec_param) &&
                    nzchar(sec_param) &&
                    !is.null(sec_station) &&
                    nzchar(sec_station)
            ) {
                secondary <- tryCatch(
                    get_station_timeseries(
                        location_code = sec_station,
                        parameter = sec_param,
                        reference_time = request$reference_time,
                        load_entire_record = FALSE,
                        con = con,
                        historical_start_year = 2020L
                    ),
                    error = function(e) NULL
                )

                secondary_limits <- tryCatch(
                    get_parameter_percentile_limits(
                        location_code = sec_station,
                        parameter = sec_param,
                        con = con,
                        historical_start_year = 2020L
                    ),
                    error = function(e) list(p10 = NA_real_, p90 = NA_real_)
                )
            }

            list(
                request = request,
                ready = TRUE,
                continuous = continuous,
                percentiles = percentiles,
                return_periods = return_periods,
                secondary = secondary,
                secondary_limits = secondary_limits
            )
        })

        output$station_plot_title <- shiny::renderText({
            request <- station_plot_request()
            if (
                is.null(input$create_plot) ||
                    input$create_plot < 1 ||
                    is.null(request)
            ) {
                return("Station Plot")
            }

            code <- request$station
            if (
                is.null(request$parameter) ||
                    !nzchar(request$parameter) ||
                    is.null(code) ||
                    !nzchar(code)
            ) {
                return("Station Plot")
            }

            title_parts <- c(sprintf(
                "%s: %s",
                station_label_for_code(code),
                request$parameter
            ))

            if (
                nzchar(request$secondary_station) &&
                    nzchar(request$secondary_parameter)
            ) {
                title_parts <- c(
                    title_parts,
                    sprintf(
                        "Secondary %s: %s",
                        station_label_for_code(request$secondary_station),
                        request$secondary_parameter
                    )
                )
            }

            if (isTRUE(station_plot_is_stale())) {
                title_parts <- c(title_parts, "Outdated")
            }

            paste(title_parts, collapse = " | ")
        })

        shiny::observe({
            session$sendCustomMessage(
                "set-station-plot-stale",
                isTRUE(station_plot_is_stale())
            )
        })

        output$station_plot_stale_banner <- shiny::renderUI({
            if (!isTRUE(station_plot_is_stale())) {
                return(NULL)
            }

            shiny::tags$div(
                class = "station-plot-stale-banner",
                "Plot is outdated. Click Create plot."
            )
        })

        station_plot_widget <- shiny::reactive({
            bundle <- station_plot_bundle()
            if (!isTRUE(bundle$ready)) {
                return(NULL)
            }
            request <- bundle$request

            code <- request$station
            param <- request$parameter
            req(nzchar(code), nzchar(param))

            p <- build_station_plot(
                location_code = code,
                parameter = param,
                reference_time = request$reference_time,
                con = con,
                snow_survey_parameters = snow_survey_parameters,
                load_entire_record = FALSE,
                continuous_data = bundle$continuous,
                percentiles = bundle$percentiles,
                return_periods = bundle$return_periods
            )

            req(!is.null(p))

            # Append secondary trace when both secondary inputs are set.
            sec_param <- request$secondary_parameter
            sec_station <- request$secondary_station
            if (
                !is.null(sec_param) &&
                    nzchar(sec_param) &&
                    !is.null(sec_station) &&
                    nzchar(sec_station)
            ) {
                sec_data <- bundle$secondary
                sec_label <- station_label_for_code(sec_station)
                sec_limits <- bundle$secondary_limits

                if (!is.null(sec_data) && nrow(sec_data) > 0) {
                    if (!"trace_source" %in% names(sec_data)) {
                        sec_data$trace_source <- "observed"
                    }

                    sec_hist <- sec_data[
                        sec_data$trace_source == "historical_daily",
                    ]
                    sec_rt <- sec_data[
                        sec_data$trace_source == "realtime_continuous",
                    ]

                    sec_y_title <- parameter_axis_title(sec_param)
                    sec_range <- NULL
                    if (
                        !is.null(sec_limits) &&
                            is.finite(sec_limits$p10) &&
                            is.finite(sec_limits$p90) &&
                            sec_limits$p90 > sec_limits$p10
                    ) {
                        sec_span <- sec_limits$p90 - sec_limits$p10
                        sec_pad <- max(sec_span * 0.1, .Machine$double.eps)
                        sec_range <- c(
                            sec_limits$p10 - sec_pad,
                            sec_limits$p90 + sec_pad
                        )
                    }

                    if (nrow(sec_hist) > 0) {
                        p <- p %>%
                            plotly::add_lines(
                                data = sec_hist,
                                x = ~datetime,
                                y = ~value,
                                yaxis = "y2",
                                name = sprintf(
                                    "Daily observed (%s \u2013 %s)",
                                    sec_label,
                                    sec_param
                                ),
                                hovertemplate = paste0(
                                    sec_label,
                                    " daily: %{y:.3f}<extra></extra>"
                                ),
                                line = list(color = "#93c5fd", width = 1)
                            )
                    }

                    if (nrow(sec_rt) > 0) {
                        p <- p %>%
                            plotly::add_lines(
                                data = sec_rt,
                                x = ~datetime,
                                y = ~value,
                                yaxis = "y2",
                                name = sprintf(
                                    "Observed (%s \u2013 %s)",
                                    sec_label,
                                    sec_param
                                ),
                                hovertemplate = paste0(
                                    sec_label,
                                    ": %{y:.3f}<extra></extra>"
                                ),
                                line = list(color = "#2563eb", width = 2)
                            )
                    } else if (nrow(sec_hist) == 0) {
                        # Single-source data (e.g. FDD/DDT observed only)
                        p <- p %>%
                            plotly::add_lines(
                                data = sec_data,
                                x = ~datetime,
                                y = ~value,
                                yaxis = "y2",
                                name = sprintf(
                                    "Observed (%s \u2013 %s)",
                                    sec_label,
                                    sec_param
                                ),
                                hovertemplate = paste0(
                                    sec_label,
                                    ": %{y:.3f}<extra></extra>"
                                ),
                                line = list(color = "#2563eb", width = 2)
                            )
                    }

                    p <- p %>%
                        plotly::layout(
                            yaxis2 = list(
                                title = sec_y_title,
                                range = sec_range,
                                overlaying = "y",
                                side = "right",
                                showgrid = FALSE
                            )
                        )
                }
            }

            p
        })

        output$station_plot <- plotly::renderPlotly({
            if (is.null(input$create_plot) || input$create_plot < 1) {
                return(
                    empty_plotly_widget(
                        title = "Station Plot",
                        annotations = list(list(
                            text = "Choose parameters/stations, then click Create plot.",
                            x = 0.5,
                            y = 0.5,
                            xref = "paper",
                            yref = "paper",
                            showarrow = FALSE
                        ))
                    ) %>%
                        plotly::layout(
                            xaxis = list(visible = FALSE),
                            yaxis = list(visible = FALSE)
                        )
                )
            }

            plot_widget <- station_plot_widget()

            if (is.null(plot_widget)) {
                return(
                    empty_plotly_widget(
                        title = "Station Plot",
                        annotations = list(list(
                            text = "Choose parameters/stations, then click Create plot.",
                            x = 0.5,
                            y = 0.5,
                            xref = "paper",
                            yref = "paper",
                            showarrow = FALSE
                        ))
                    ) %>%
                        plotly::layout(
                            xaxis = list(visible = FALSE),
                            yaxis = list(visible = FALSE)
                        )
                )
            }

            plot_widget
        })

        output$stations_map <- leaflet::renderLeaflet({
            stations <- community_stations()$stations
            req(!is.null(stations), nrow(stations) > 0, input$parameter)
            basins <- community_stations()$basins
            upstream_basins <- community_stations()$upstream_basins
            summary_now <- summary_data()

            selected_code <- tryCatch(
                selected_station_code(),
                error = function(e) NA_character_
            )

            station_label <- if ("name" %in% names(stations)) {
                as.character(stations$name)
            } else {
                as.character(stations$location_code)
            }
            station_label[
                is.na(station_label) | !nzchar(station_label)
            ] <- as.character(
                stations$location_code[
                    is.na(station_label) | !nzchar(station_label)
                ]
            )

            stations$variables_available <- vapply(
                seq_len(nrow(stations)),
                function(i) {
                    available <- parameter_candidates[
                        vapply(
                            parameter_candidates,
                            function(param) {
                                flags <- station_has_parameter(
                                    stations,
                                    param
                                )
                                length(flags) >= i && isTRUE(flags[[i]])
                            },
                            logical(1)
                        )
                    ]
                    if (length(available) == 0) {
                        return("None")
                    }
                    paste(available, collapse = ", ")
                },
                character(1)
            )

            stations$popup_html <- vapply(
                seq_len(nrow(stations)),
                function(i) {
                    code <- as.character(stations$location_code[[i]])
                    label <- station_label[[i]]

                    sprintf(
                        "<b>%s</b><br>Code: %s<br>Variables available: %s",
                        htmltools::htmlEscape(label),
                        htmltools::htmlEscape(code),
                        htmltools::htmlEscape(stations$variables_available[[i]])
                    )
                },
                character(1)
            )
            stations$tooltip_text <- paste0(
                station_label,
                " (",
                stations$location_code,
                ")"
            )

            has_selected_param <- station_has_parameter(
                stations,
                input$parameter
            )

            target_codes <- unique(stats::na.omit(stations$location_code[
                has_selected_param
            ]))
            summary_selected <- as.data.frame(summary_now)

            recent_codes <- character(0)
            if (
                nrow(summary_selected) > 0 &&
                    all(
                        c("location_code", "last_data_age_hours") %in%
                            names(summary_selected)
                    )
            ) {
                recent_codes <- unique(stats::na.omit(
                    summary_selected$location_code[
                        !is.na(summary_selected$last_data_age_hours) &
                            summary_selected$last_data_age_hours <= 24
                    ]
                ))
            }

            trend_lookup <- data.frame(
                location_code = character(),
                change_24h = numeric()
            )
            if (
                nrow(summary_selected) > 0 &&
                    all(
                        c("location_code", "change_24h") %in%
                            names(summary_selected)
                    )
            ) {
                trend_lookup <- summary_selected[,
                    c("location_code", "change_24h"),
                    drop = FALSE
                ]
            } else if (
                length(target_codes) > 0 &&
                    !identical(input$parameter, "FDD") &&
                    !identical(input$parameter, "DDT")
            ) {
                trend_lookup <- get_parameter_change_24h(
                    location_codes = target_codes,
                    parameter_name = input$parameter,
                    reference_time = time_zero()
                )
                if (!"change_24h" %in% names(trend_lookup)) {
                    trend_lookup$change_24h <- NA_real_
                }
            }

            stations$change_24h <- NA_real_
            if (nrow(trend_lookup) > 0) {
                idx <- match(stations$location_code, trend_lookup$location_code)
                stations$change_24h <- trend_lookup$change_24h[idx]
            }

            is_target <- stations$location_code %in% target_codes
            is_recent_target <- is_target &
                (stations$location_code %in% recent_codes)
            is_stale_target <- is_target & !is_recent_target

            stations_triangle <- stations[which(is_recent_target), ]
            stations_circle <- stations[which(!is_recent_target), ]

            stations_circle$fill_color <- rep(
                character(1),
                nrow(stations_circle)
            )
            if (nrow(stations_circle) > 0) {
                stations_circle$fill_color <- ifelse(
                    stations_circle$location_code %in%
                        stations$location_code[is_stale_target],
                    "#9ca3af",
                    "#2563eb"
                )
            }
            stations_circle$stroke_color <- stations_circle$fill_color
            stations_circle$stroke_weight <- rep(1, nrow(stations_circle))
            stations_circle$radius <- rep(6, nrow(stations_circle))
            stations_circle$fill_opacity <- rep(0.9, nrow(stations_circle))

            map_obj <- leaflet::leaflet() %>%
                leaflet::addProviderTiles("CartoDB.Positron")

            if (!is.null(basins) && nrow(basins) > 0) {
                map_obj <- map_obj %>%
                    leaflet::addPolygons(
                        data = basins,
                        color = "#4b5563",
                        weight = 2,
                        opacity = 1,
                        fillColor = "#4b5563",
                        fillOpacity = 0.05
                    )
            }

            if (!is.null(upstream_basins) && nrow(upstream_basins) > 0) {
                map_obj <- map_obj %>%
                    leaflet::addPolygons(
                        data = upstream_basins,
                        color = "#0f766e",
                        weight = 2,
                        opacity = 1,
                        fillColor = "#0f766e",
                        fillOpacity = 0.05
                    )
            }

            if (nrow(stations_circle) > 0) {
                map_obj <- map_obj %>%
                    leaflet::addCircleMarkers(
                        data = stations_circle,
                        radius = ~radius,
                        stroke = TRUE,
                        weight = ~stroke_weight,
                        color = ~stroke_color,
                        fillColor = ~fill_color,
                        fillOpacity = ~fill_opacity,
                        label = ~tooltip_text,
                        labelOptions = leaflet::labelOptions(noHide = FALSE),
                        popup = ~popup_html
                    )
            }

            if (nrow(stations_triangle) > 0) {
                triangle_labels <- lapply(
                    seq_len(nrow(stations_triangle)),
                    function(i) {
                        clr <- if (
                            !is.na(stations_triangle$change_24h[[i]]) &&
                                stations_triangle$change_24h[[i]] > 0
                        ) {
                            "#dc2626"
                        } else {
                            "#16a34a"
                        }
                        glyph <- if (
                            !is.na(stations_triangle$change_24h[[i]]) &&
                                stations_triangle$change_24h[[i]] > 0
                        ) {
                            "&#9650;"
                        } else {
                            "&#9660;"
                        }
                        htmltools::HTML(sprintf(
                            "<span style='color:%s; font-size:18px; line-height:14px;'>%s</span>",
                            clr,
                            glyph
                        ))
                    }
                )

                map_obj <- map_obj %>%
                    leaflet::addLabelOnlyMarkers(
                        data = stations_triangle,
                        lng = ~longitude,
                        lat = ~latitude,
                        label = triangle_labels,
                        labelOptions = leaflet::labelOptions(
                            noHide = TRUE,
                            textOnly = TRUE,
                            direction = "center"
                        )
                    )

                map_obj <- map_obj %>%
                    leaflet::addCircleMarkers(
                        data = stations_triangle,
                        lng = ~longitude,
                        lat = ~latitude,
                        radius = 10,
                        stroke = FALSE,
                        fillOpacity = 0,
                        opacity = 0,
                        label = ~tooltip_text,
                        labelOptions = leaflet::labelOptions(noHide = FALSE),
                        popup = ~popup_html
                    )
            }

            selected_station <- stations[
                stations$location_code == selected_code,
            ]
            if (nrow(selected_station) > 0) {
                map_obj <- map_obj %>%
                    leaflet::addCircleMarkers(
                        data = selected_station,
                        lng = ~longitude,
                        lat = ~latitude,
                        radius = 12,
                        stroke = TRUE,
                        weight = 4,
                        color = "#ec4899",
                        fillOpacity = 0,
                        opacity = 1
                    )
            }

            # Add image series locations to map as purple circle markers
            image_series_data <- community_image_series()
            if (nrow(image_series_data) > 0) {
                selected_image_series_id <- tryCatch(
                    selected_image_series()$img_series_id[[1]],
                    error = function(e) NA_integer_
                )

                image_series_tooltip <- paste0(
                    "Image series: ",
                    image_series_data$name,
                    " (",
                    image_series_data$location_code,
                    ")"
                )
                image_series_tooltip[
                    is.na(image_series_data$name) |
                        !nzchar(image_series_data$name)
                ] <-
                    paste0(
                        "Image series: ",
                        image_series_data$location_code[
                            is.na(image_series_data$name) |
                                !nzchar(image_series_data$name)
                        ]
                    )

                map_obj <- map_obj %>%
                    leaflet::addCircleMarkers(
                        data = image_series_data,
                        lng = ~longitude,
                        lat = ~latitude,
                        radius = 6,
                        stroke = TRUE,
                        weight = 2,
                        color = "#a855f7",
                        fillColor = "#a855f7",
                        fillOpacity = 0.7,
                        label = image_series_tooltip,
                        labelOptions = leaflet::labelOptions(noHide = FALSE),
                        popup = image_series_tooltip
                    )

                selected_image_series_row <- image_series_data[
                    image_series_data$img_series_id == selected_image_series_id,
                ]
                if (nrow(selected_image_series_row) > 0) {
                    map_obj <- map_obj %>%
                        leaflet::addCircleMarkers(
                            data = selected_image_series_row,
                            lng = ~longitude,
                            lat = ~latitude,
                            radius = 12,
                            stroke = TRUE,
                            weight = 4,
                            color = "#06b6d4",
                            fillOpacity = 0,
                            opacity = 1
                        )
                }
            }

            map_obj
        })

        output$station_image <- shiny::renderUI({
            series <- community_image_series()
            req(nrow(series) > 0)

            image_series <- selected_image_series()
            img_series_id <- image_series$img_series_id[[1]]
            time0_sql <- DBI::dbQuoteString(
                con,
                format(as.POSIXct(time_zero()), "%Y-%m-%d %H:%M:%S")
            )

            latest_image <- DBI::dbGetQuery(
                con,
                paste0(
                    "SELECT image_id, datetime FROM images WHERE img_series_id = ",
                    img_series_id,
                    " AND datetime <= ",
                    time0_sql,
                    " ORDER BY datetime DESC LIMIT 1"
                )
            )

            if (nrow(latest_image) == 0) {
                return(shiny::tags$div(
                    "No images available for this image series location"
                ))
            }

            image_id <- latest_image$image_id[[1]]
            image <- DBI::dbGetQuery(
                con,
                paste0(
                    "SELECT format, file FROM images WHERE image_id = ",
                    image_id
                )
            )

            if (nrow(image) == 0 || is.null(image$file[[1]])) {
                return(shiny::tags$div("Image bytes not available"))
            }

            img_src <- tryCatch(
                image_blob_to_data_uri(
                    image_blob = image$file[[1]],
                    image_format = image$format[[1]]
                ),
                error = function(e) NULL
            )

            if (is.null(img_src)) {
                return(shiny::tags$div("Unable to render image"))
            }

            shiny::tags$img(
                src = img_src,
                style = "max-width:100%; max-height:420px; object-fit:contain;"
            )
        })
    }

    shiny::shinyApp(ui = ui, server = server)
}

app <- launch_freshet_dashboard(con = con)
shiny::runApp(app)
