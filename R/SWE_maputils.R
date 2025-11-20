#' @title Snow Water Equivalent (SWE) Mapping Utilities
#' @description
#' This module provides comprehensive functionality for creating interactive and static maps
#' of Snow Water Equivalent (SWE) data across the Yukon Territory. It includes data loading,
#' processing, visualization, and mapping functions for both continuous (snow pillow) and
#' discrete (manual snow course) measurements.
#'
#' @section Key Features:
#' \itemize{
#'   \item Interactive leaflet maps with popup plots
#'   \item Static ggplot2 maps for publication
#'   \item Basin-averaged SWE calculations using weighted station data
#'   \item Historical comparison with percentile-based coloring
#'   \item Support for both absolute (mm) and relative (% of normal) values
#' }
#'
#' @section Data Sources:
#' \itemize{
#'   \item SWE basins: Local shapefile with basin polygons
#'   \item Stations/Communities/Roads: PostgreSQL spatial database (AquaCache)
#'   \item Timeseries data: Continuous and discrete measurements database
#'   \item Snowcourse factors: CSV file with basin weighting factors
#' }
#'
#' @author Yukon Government, Water Resources

# =============================================================================
# CONFIGURATION
# =============================================================================
#' Initialize visualization parameters
#'
#' @description
#' Sets up color schemes, bins, and other visualization parameters used across
#' both leaflet and ggplot2 mapping functions.
#'
#' @return A nested list containing all plotting options for basins, stations, roads, boundaries, labels, etc.
#'
#' @details
#' The relative SWE bins are designed to highlight significant departures from normal
#'
#' @export
#' @noRd
initialize_visualization_parameters <- function() {
    # Color scheme and visualization parameters
    # Bins represent percentage of normal SWE (relative_swe values)
    relative_bins <- c(-2, -1, 0, 50, 70, 90, 110, 130, 150, Inf)
    relative_colors <- c(
        "#27A62C", # Green (much below normal)
        "#B200DD", # Purple (below normal)
        "#EBB966", # Orange (near normal)
        "#EEE383", # Yellow (normal)
        "#C1FB80", # Light green (above normal)
        "#6CFC88", # Green (well above normal)
        "#8CEFE1", # Cyan (high)
        "#85B4F8", # Light blue (very high)
        "#6772F7" # Blue (extremely high)
    )

    absolute_bins <- c(0, 50, 100, 150, 200, 250, 300, 400, 500, Inf)
    absolute_colors <- c(
        "#e6f5ff", # Very light sky blue
        "#b3e2ff", # Light blue
        "#80c7e6", # Cyan/teal
        "#41b6c4", # Light blue (existing)
        "#2c7fb8", # Medium blue
        "#253494", # Dark blue
        "#762a83", # Purple
        "#5d1a86", # Dark purple
        "#2d004b" # Very dark purple
    )

    # SVG icon for communities
    communities_icon_svg <- "data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='16' height='16' viewBox='0 0 16 16'><polygon points='8,0 16,8 8,16 0,8' fill='black' stroke='white' stroke-width='2'/></svg>"

    # Percentile bins and colors (Spectral palette, 0-100)
    percentile_bins <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
    percentile_colors <- c(
        "#9e0142", # Very low (0-10)
        "#d53e4f", # Low (10-20)
        "#f46d43", # Below normal (20-30)
        "#fdae61", # Below normal (30-40)
        "#fee090", # Near normal (40-50)
        "#e6f598", # Near normal (50-60)
        "#abdda4", # Above normal (60-70)
        "#66c2a5", # Above normal (70-80)
        "#3288bd", # High (80-90)
        "#5e4fa2" # Very high (90-100)
    )

    list(
        basins = list(
            fillColor = relative_colors,
            bins = relative_bins,
            color = "white",
            weight = 2,
            opacity = 1,
            fillOpacity = 0.7,
            label = list(
                color = "#222",
                fontSize = "14px",
                fontWeight = "bold",
                textShadow = "2px 2px 4px #fff"
            ),
            popupOptions = list(
                maxWidth = 320,
                closeButton = TRUE,
                autoPan = TRUE
            ),
            highlightOptions = list(
                color = "#FF6600",
                weight = 4,
                opacity = 1,
                bringToFront = TRUE,
                sendToBack = FALSE
            )
        ),
        pillows = list(
            color = "blue",
            radius = 8,
            weight = 2,
            opacity = 1,
            fillOpacity = 1,
            highlightOptions = list(
                color = "#00FFFF",
                weight = 5,
                opacity = 1,
                bringToFront = TRUE,
                sendToBack = FALSE
            )
        ),
        surveys = list(
            color = "black",
            radius = 8,
            weight = 2,
            opacity = 1,
            fillOpacity = 1,
            highlightOptions = list(
                color = "#00FFFF",
                weight = 5,
                opacity = 1,
                bringToFront = TRUE,
                sendToBack = FALSE
            )
        ),
        roads = list(
            color = "#8B0000",
            weight = 2,
            opacity = 0.8,
            label = list(
                color = "#8B0000"
            )
        ),
        boundary = list(
            color = "#222222",
            weight = 3,
            fill = FALSE
        ),
        communities = list(
            icon = communities_icon_svg,
            iconWidth = 16,
            iconHeight = 16,
            label = list(
                color = "#222",
                fontSize = "13px",
                fontWeight = "bold",
                fontStyle = "italic",
                textShadow = "1px 1px 2px #fff"
            ),
            labelOptions = list(
                noHide = TRUE,
                direction = "top",
                textOnly = TRUE
            )
        ),

        relative_bins = relative_bins,
        relative_colors = relative_colors,
        absolute_bins = absolute_bins,
        absolute_colors = absolute_colors,
        percentile_bins = percentile_bins,
        percentile_colors = percentile_colors
    )
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Get the most recent non-missing value from a timeseries
#'
#' @param ts A data.frame with 'datetime' and 'value' columns
#' @return POSIXct datetime of the most recent valid measurement, or \code{NA}
#'
#' @description
#' This function finds the latest date in a timeseries that has a valid
#' (non-NA, non-NaN) value. Used to filter out stations with stale data.
#'
#' @details
#' The function validates input structure and searches backwards from the most
#' recent datetime to find the last valid measurement. This is used to determine
#' data currency for station filtering.
#'
#' @noRd
#' @examples
#' \dontrun{
#' ts_data <- data.frame(
#'   datetime = as.POSIXct(c("2024-01-01", "2024-01-02", "2024-01-03")),
#'   value = c(100, NA, 150)
#' )
#' get_most_recent_date(ts_data)  # Returns 2024-01-03
#' }
get_most_recent_date <- function(ts) {
    if (
        is.null(ts) ||
            nrow(ts) == 0 ||
            !"datetime" %in% names(ts) ||
            !"value" %in% names(ts)
    ) {
        return(NA_real_)
    }
    ts$datetime <- as.POSIXct(ts$datetime)
    valid_idx <- which(!is.na(ts$value) & !is.nan(ts$value))
    if (length(valid_idx) == 0) {
        return(NA_real_)
    }
    latest_time <- max(ts$datetime[valid_idx], na.rm = TRUE)
    return(latest_time)
}

#' Convert year and month to POSIXct datetime
#'
#' @param year Integer year (e.g., 2025)
#' @param month Integer month (1-12)
#' @return POSIXct datetime object in UTC timezone, set to first day of month
#' @noRd
#' @description
#' Creates a standardized datetime for the first day of the specified month and year.
#' Used throughout the application for consistent date handling.
#'
#' @examples
#' \dontrun{
#' get_datetime(2025, 3)  # Returns 2025-03-01 00:00:00 UTC
#' get_datetime(2024, 12) # Returns 2024-12-01 00:00:00 UTC
#' }
get_datetime <- function(year, month) {
    as.POSIXct(as.Date(paste0(year, "-", month, "-01")), tz = "UTC")
}

# =============================================================================
# DATA LOADING FUNCTIONS
# =============================================================================

#' Load spatial data from database with coordinate transformation
#'
#' @param con DBI database connection object
#' @param layer_name Character string of the spatial layer name in the database
#' @param additional_query Optional character string for additional WHERE conditions
#' @return sf object in WGS84 (EPSG:4326) coordinate system, or \code{NULL} if no data found
#'
#' @description
#' Queries the spatial.vectors table and transforms geometries to WGS84.
#' Used for loading administrative boundaries, communities, roads, etc.
#' @noRd
#' @details
#' The function first verifies that the requested layer exists in the database,
#' then downloads and transforms the geometry to WGS84 for consistent mapping.
#' Additional SQL WHERE conditions can be specified for filtering.
#'
#' @examples
#' \dontrun{
#' # Load all communities
#' communities <- download_spatial_layer(con, "Communities")
#'
#' # Load only major communities
#' major_communities <- download_spatial_layer(
#'   con,
#'   "Communities",
#'   "AND description IN ('City', 'Town')"
#' )
#' }
download_spatial_layer <- function(con, layer_name, additional_query = NULL) {
    query <- sprintf(
        "SELECT *, ST_AsText(ST_Transform(geom, 4326)) as geom_4326 
         FROM spatial.vectors 
         WHERE layer_name = %s",
        DBI::dbQuoteString(con, layer_name)
    )

    if (!is.null(additional_query) && nzchar(additional_query)) {
        query <- paste(query, additional_query)
    }

    data <- DBI::dbGetQuery(con, query)
    if (nrow(data) == 0) {
        warning(sprintf("No data found for layer: %s", layer_name))
        return(NULL)
    }

    geom <- sf::st_as_sfc(data$geom_4326, crs = 4326)
    sf::st_sf(data, geometry = geom, crs = 4326)
}

#' Retrieve continuous SWE timeseries and station metadata
#'
#' @param con DBI database connection object
#' @param start_date Character date string (YYYY-MM-DD) for filtering data
#' @param end_date Character date string (YYYY-MM-DD) for filtering data
#' @param resolution Character, either "daily" or "monthly" for data aggregation
#' @return A list with two elements:
#' \describe{
#'   \item{timeseries}{Named list containing a 'swe' data.frame with datetime and station columns}
#'   \item{metadata}{sf object with station locations and latest_date column}
#' }
#'
#' @description
#' Loads continuous snow water equivalent measurements from snow pillow stations.
#' Data is aggregated to daily resolution by default. Metadata includes the
#' most recent measurement date for each station.
#'
#' @details
#' The function queries the continuous.timeseries and continuous.measurements_calculated_daily_corrected
#' tables to retrieve SWE data. Each station becomes a column in the wide-format
#' timeseries data.frame, facilitating efficient historical analysis.
#'
#' @examples
#' \dontrun{
#' continuous_data <- download_continuous_ts(con)
#' print(names(continuous_data$timeseries$swe))  # Shows datetime + station columns
#' print(nrow(continuous_data$metadata))         # Number of stations
#' }
download_continuous_ts <- function(
    con,
    start_date = sprintf("%d-01-01", 1950),
    end_date = sprintf("%d-01-01", 2100),
    resolution = "daily"
) {
    # Build metadata query for continuous SWE timeseries
    md_query <- paste0(
        "SELECT 
            t.timeseries_id,
            t.location_id,
            l.location,
            l.name,
            l.latitude,
            l.longitude,
            dc.conversion_m
         FROM continuous.timeseries t
         JOIN public.locations l ON t.location_id = l.location_id
         LEFT JOIN datum_conversions dc ON l.location_id = dc.location_id
         WHERE t.parameter_id = (SELECT parameter_id FROM public.parameters 
                                 WHERE param_name = 'snow water equivalent')"
    )

    md_continuous_df <- DBI::dbGetQuery(con, md_query)

    if (nrow(md_continuous_df) == 0) {
        warning("No continuous SWE stations found")
        return(list(
            timeseries = data.frame(datetime = as.POSIXct(character(0))),
            metadata = NULL
        ))
    }

    # Convert metadata to sf (WGS84)
    md_continuous <- sf::st_as_sf(
        md_continuous_df,
        coords = c("longitude", "latitude"),
        crs = 4326,
        remove = FALSE
    )

    ts_ids <- unique(md_continuous$timeseries_id)
    # Temporary list to hold per-station data.frames for latest-date calculation
    ts_list_temp <- vector("list", length(ts_ids))
    names(ts_list_temp) <- as.character(ts_ids)

    # Master timeseries dataframe (will contain datetime + one column per station)
    master_df <- NULL

    for (i in seq_along(ts_ids)) {
        ts_id <- ts_ids[i]

        # Retrieve measurements for this timeseries
        ts_query <- sprintf(
            "SELECT date, value FROM continuous.measurements_calculated_daily_corrected
             WHERE timeseries_id = %d AND value IS NOT NULL",
            as.integer(ts_id)
        )

        if (!is.null(start_date)) {
            ts_query <- paste0(
                ts_query,
                " AND date >= ",
                DBI::dbQuoteString(con, start_date)
            )
        }
        if (!is.null(end_date)) {
            ts_query <- paste0(
                ts_query,
                " AND date <= ",
                DBI::dbQuoteString(con, end_date)
            )
        }

        ts_data <- DBI::dbGetQuery(con, ts_query)

        if (nrow(ts_data) == 0) {
            # store empty df for latest-date processing
            ts_list_temp[[as.character(ts_id)]] <- data.frame(
                datetime = as.POSIXct(character(0)),
                value = numeric(0)
            )
            next
        }
        # Rename 'date' column to 'datetime'
        if ("date" %in% names(ts_data)) {
            names(ts_data)[names(ts_data) == "date"] <- "datetime"
        }
        # Parse datetimes and sort
        ts_data$datetime <- as.POSIXct(ts_data$datetime, tz = "UTC")
        ts_data <- ts_data[order(ts_data$datetime), , drop = FALSE]

        # Resample to daily or monthly as requested
        if (resolution == "daily") {
            ts_data$day <- as.Date(ts_data$datetime)
            ts_daily <- stats::aggregate(
                value ~ day,
                data = ts_data,
                FUN = mean,
                na.rm = TRUE
            )
            names(ts_daily) <- c("datetime", "value")
            ts_daily$datetime <- as.POSIXct(ts_daily$datetime, tz = "UTC")
            ts_out <- ts_daily
        } else if (resolution == "monthly") {
            ts_data$month <- format(ts_data$datetime, "%Y-%m")
            ts_monthly <- stats::aggregate(
                value ~ month,
                data = ts_data,
                FUN = mean,
                na.rm = TRUE
            )
            names(ts_monthly) <- c("datetime", "value")
            # set to first day of month
            ts_monthly$datetime <- as.POSIXct(
                paste0(ts_monthly$datetime, "-01"),
                tz = "UTC"
            )
            ts_out <- ts_monthly
        } else {
            warning(sprintf(
                "Unknown resolution '%s' for timeseries_id: %s",
                resolution,
                ts_id
            ))
            ts_out <- ts_data[, c("datetime", "value"), drop = FALSE]
        }

        # Keep a copy for latest-date calculation
        ts_list_temp[[as.character(ts_id)]] <- ts_out

        # Merge into master dataframe (wide format)
        # rename value column to station id
        station_col <- as.character(ts_id)
        ts_wide <- ts_out
        names(ts_wide)[names(ts_wide) == "value"] <- station_col

        if (is.null(master_df)) {
            master_df <- ts_wide
        } else {
            # base merge by datetime, keep all dates
            master_df <- merge(
                master_df,
                ts_wide,
                by = "datetime",
                all = TRUE,
                sort = TRUE
            )
        }
    }

    if (is.null(master_df)) {
        # No data found for any station
        master_df <- data.frame(datetime = as.POSIXct(character(0)))
    } else {
        # Ensure master_df sorted by datetime
        master_df <- master_df[order(master_df$datetime), , drop = FALSE]
        # convert columns other than datetime to numeric (they may be list factors)
        non_dt_cols <- setdiff(names(master_df), "datetime")
        for (col in non_dt_cols) {
            master_df[[col]] <- as.numeric(master_df[[col]])
        }
    }

    # Compute most recent non-NaN date per station using ts_list_temp
    station_latest_dates <- vapply(
        names(ts_list_temp),
        function(id) {
            ts <- ts_list_temp[[id]]
            get_most_recent_date(ts)
        },
        FUN.VALUE = as.POSIXct(NA)
    )

    station_means_df <- data.frame(
        timeseries_id = as.integer(names(station_latest_dates)),
        latest_date = as.POSIXct(
            station_latest_dates,
            origin = "1970-01-01",
            tz = "UTC"
        ),
        latest_observation = format(
            as.POSIXct(
                station_latest_dates,
                origin = "1970-01-01",
                tz = "UTC"
            ),
            "%Y-%m-%d"
        ),
        stringsAsFactors = FALSE
    )

    md_no_geom <- sf::st_drop_geometry(md_continuous)
    merged_df <- merge(
        md_no_geom,
        station_means_df,
        by = "timeseries_id",
        all.x = TRUE,
        sort = FALSE
    )

    # Reattach geometry in the order of merged_df
    geom_match <- match(merged_df$timeseries_id, md_continuous$timeseries_id)
    geom <- sf::st_geometry(md_continuous)[geom_match]

    metadata_sf <- sf::st_sf(
        merged_df,
        geometry = geom,
        crs = sf::st_crs(md_continuous)
    )

    return(list(timeseries = list(swe = master_df), metadata = metadata_sf))
}

#' Retrieve discrete SWE timeseries and station metadata
#'
#' @param con DBI database connection object
#' @param start_date Character date string (YYYY-MM-DD) for filtering data
#' @param end_date Character date string (YYYY-MM-DD) for filtering data
#' @return A list with two elements:
#' \describe{
#'   \item{timeseries}{Named list containing a 'swe' data.frame with datetime and station columns}
#'   \item{metadata}{sf object with station locations and latest_date column}
#' }
#'
#' @description
#' Loads discrete (manual) snow water equivalent measurements from snow
#' courses. Uses target_datetime when available, falls back to datetime.
#'
#' @details
#' Manual snow course measurements are typically taken monthly during winter.
#' The function prefers target_datetime (scheduled measurement date) over
#' actual datetime when available for consistent temporal analysis.
#'
#' @examples
#' \dontrun{
#' discrete_data <- download_discrete_ts(con)
#' # Check data currency
#' recent_stations <- discrete_data$metadata[
#'   !is.na(discrete_data$metadata$latest_date),
#' ]
#' }
download_discrete_ts <- function(
    con,
    start_date = sprintf("%d-01-01", 1950),
    end_date = sprintf("%d-01-01", 2100)
) {
    # Build metadata query for discrete SWE timeseries
    md_discrete_df <- DBI::dbGetQuery(
        con,
        "SELECT DISTINCT
            l.location_id,
            l.latitude,
            l.longitude,
            l.location,
            l.name,
            dc.conversion_m
         FROM discrete.samples s
         JOIN discrete.results r ON s.sample_id = r.sample_id
         JOIN public.locations l ON s.location_id = l.location_id
         LEFT JOIN datum_conversions dc ON l.location_id = dc.location_id
         WHERE r.parameter_id = (SELECT parameter_id FROM public.parameters
                                 WHERE param_name = 'snow water equivalent')"
    )

    if (nrow(md_discrete_df) == 0) {
        warning("No discrete SWE stations found")
        return(list(
            timeseries = list(
                swe = data.frame(datetime = as.POSIXct(character(0)))
            ),
            metadata = NULL
        ))
    }

    # Convert metadata to sf (WGS84)
    md_discrete <- sf::st_as_sf(
        md_discrete_df,
        coords = c("longitude", "latitude"),
        crs = 4326,
        remove = FALSE
    )

    ts_ids <- unique(md_discrete$location_id)
    ts_list_temp <- vector("list", length(ts_ids))
    names(ts_list_temp) <- as.character(ts_ids)

    master_df <- NULL

    for (i in seq_along(ts_ids)) {
        loc_id <- ts_ids[i]

        ts_query <- sprintf(
            "SELECT s.datetime, s.target_datetime, r.result
             FROM discrete.samples s
             JOIN discrete.results r ON s.sample_id = r.sample_id
             WHERE s.location_id = %s
               AND r.parameter_id = (SELECT parameter_id FROM public.parameters
                                     WHERE param_name = 'snow water equivalent')
               AND r.result IS NOT NULL",
            DBI::dbQuoteLiteral(con, loc_id)
        )

        if (!is.null(start_date)) {
            ts_query <- paste0(
                ts_query,
                " AND (COALESCE(s.target_datetime, s.datetime) >= ",
                DBI::dbQuoteString(con, start_date),
                ")"
            )
        }
        if (!is.null(end_date)) {
            ts_query <- paste0(
                ts_query,
                " AND (COALESCE(s.target_datetime, s.datetime) <= ",
                DBI::dbQuoteString(con, end_date),
                ")"
            )
        }

        ts <- DBI::dbGetQuery(con, ts_query)

        if (nrow(ts) == 0) {
            ts_list_temp[[as.character(loc_id)]] <- data.frame(
                datetime = as.POSIXct(character(0)),
                value = numeric(0)
            )
            next
        }

        # prefer target_datetime when present
        if ("target_datetime" %in% names(ts)) {
            ts$target_datetime[is.na(ts$target_datetime)] <- ts$datetime[is.na(
                ts$target_datetime
            )]
            ts$datetime <- as.POSIXct(ts$target_datetime, tz = "UTC")
            ts$target_datetime <- NULL
        } else {
            ts$datetime <- as.POSIXct(ts$datetime, tz = "UTC")
        }

        # Convert datetime to just the date (day resolution)
        ts$datetime <- as.Date(ts$datetime)

        # rename result -> value
        if ("result" %in% names(ts)) {
            names(ts)[names(ts) == "result"] <- "value"
        }

        ts <- ts[order(ts$datetime), , drop = FALSE]

        # store temporary for latest-date calc
        ts_list_temp[[as.character(loc_id)]] <- ts[,
            c("datetime", "value"),
            drop = FALSE
        ]

        # merge into master_df (wide)
        station_col <- as.character(loc_id)
        ts_wide <- ts[, c("datetime", "value"), drop = FALSE]
        names(ts_wide)[names(ts_wide) == "value"] <- station_col

        if (is.null(master_df)) {
            master_df <- ts_wide
        } else {
            master_df <- merge(
                master_df,
                ts_wide,
                by = "datetime",
                all = TRUE,
                sort = TRUE
            )
        }
    }

    if (is.null(master_df)) {
        master_df <- data.frame(datetime = as.POSIXct(character(0)))
    } else {
        master_df <- master_df[order(master_df$datetime), , drop = FALSE]
        non_dt_cols <- setdiff(names(master_df), "datetime")
        for (col in non_dt_cols) {
            master_df[[col]] <- as.numeric(master_df[[col]])
        }
    }

    # Compute most recent non-NaN value per station using ts_list_temp
    station_latest_dates <- vapply(
        names(ts_list_temp),
        function(id) {
            ts <- ts_list_temp[[id]]
            get_most_recent_date(ts)
        },
        FUN.VALUE = as.POSIXct(NA)
    )

    station_means_df <- data.frame(
        location_id = as.integer(names(station_latest_dates)),
        latest_date = as.POSIXct(
            station_latest_dates,
            origin = "1970-01-01",
            tz = "UTC"
        ),
        latest_observation = format(
            as.POSIXct(
                station_latest_dates,
                origin = "1970-01-01",
                tz = "UTC"
            ),
            "%Y-%m-%d"
        ),
        stringsAsFactors = FALSE
    )

    md_no_geom <- sf::st_drop_geometry(md_discrete)
    merged_df <- merge(
        md_no_geom,
        station_means_df,
        by = "location_id",
        all.x = TRUE,
        sort = FALSE
    )

    # Reattach geometry in original order
    geom_match <- match(merged_df$location_id, md_discrete$location_id)
    geom <- sf::st_geometry(md_discrete)[geom_match]

    metadata_sf <- sf::st_sf(
        merged_df,
        geometry = geom,
        crs = sf::st_crs(md_discrete)
    )

    return(list(timeseries = list(swe = master_df), metadata = metadata_sf))
}

#' Load and process snowcourse factors for discrete SWE stations
#'
#' @param metadata_discrete sf object with discrete station metadata
#' @return data.frame with location IDs and basin weighting factors
#'
#' @description
#' Reads the CSV file containing weights for how much each snow course
#' station contributes to each SWE basin calculation. Removes duplicates and
#' merges with station location IDs.
#'
#' @details
#' The snowcourse factors define how measurements from individual snow course
#' stations are weighted when calculating basin-average SWE values. Each row
#' represents a station, with columns for each basin showing the contribution weight.
#'
#' @examples
#' \dontrun{
#' weights <- load_snowcourse_factors(discrete_metadata)
#' # Check which basins a station contributes to
#' station_weights <- weights[weights$location_id == 123, ]
#' }
load_snowcourse_factors <- function(
    metadata_discrete
) {
    snowcourse_factors <- utils::read.csv(
        system.file(
            "snow_survey/snowcourse_factors.csv",
            package = "YGwater"
        ),
        stringsAsFactors = FALSE
    )

    # Remove duplicate Hyland River value
    snowcourse_factors <- snowcourse_factors[
        snowcourse_factors$location != "10AD-SC01",
    ]
    # Drop location_name column
    snowcourse_factors <- snowcourse_factors[,
        !colnames(snowcourse_factors) %in% "location_name"
    ]

    if (!is.null(metadata_discrete)) {
        # Replace 'location' in snowcourse_factors with 'location_id' from metadata_discrete
        # Assume metadata_discrete has columns 'location' and 'location_id'
        if (
            "location" %in%
                colnames(snowcourse_factors) &&
                "location" %in% colnames(metadata_discrete) &&
                "location_id" %in% colnames(metadata_discrete)
        ) {
            snowcourse_factors <- merge(
                snowcourse_factors,
                metadata_discrete[, c("location", "location_id")],
                by = "location",
                all.x = TRUE,
                sort = FALSE
            )
            # Remove old 'location' column if desired, keep only 'location_id'
            snowcourse_factors$location <- NULL
        }
    }

    return(snowcourse_factors)
}

# =============================================================================
# DATA PROCESSING FUNCTIONS
# =============================================================================

#' Calculate historic daily median and relative change for timeseries
#'
#' @param ts Wide-format data.frame with 'datetime' column and station columns
#' @param lookback_length Integer number of years to look back from each measurement (optional)
#' @param lookback_year Integer year to start lookback period from (e.g., 1980) (optional)
#' @return A list with two elements:
#' \describe{
#'   \item{historic_median}{data.frame with historic median values for each date/station}
#'   \item{relative_swe}{data.frame with current values as percentage of historic median}
#' }
#'
#' @description
#' For each measurement, calculates the historic median for the same day-of-year
#' using data from previous years within the lookback period. Supports both
#' fixed lookback periods and rolling windows.
#'
#' @details
#' The function handles special cases for relative SWE calculation:
#' \itemize{
#'   \item Standard percentage when historic median > 0
#'   \item Value of -2 when both current and historic are zero
#'   \item Value of -1 when current > 0 but historic is zero
#' }
#'
#' Data is filtered to February-May and snapped to the 1st of each month for
#' consistency with snow bulletin reporting periods.
#'
#' @examples
#' \dontrun{
#' # Calculate using fixed lookback year
#' result <- calculate_historic_daily_median(ts_data, lookback_year = 1980)
#'
#' # Calculate using rolling 30-year window
#' result <- calculate_historic_daily_median(ts_data, lookback_length = 30)
#' }
calculate_historic_daily_median <- function(
    ts,
    lookback_length = NULL,
    lookback_year = NULL
) {
    # Default behaviour if neither provided
    if (is.null(lookback_year) && is.null(lookback_length)) {
        lookback_year <- 1980
    } else if (!is.null(lookback_year) && !is.null(lookback_length)) {
        stop("Specify either lookback_year or lookback_length, not both.")
    }

    # For each unique year/month, if no value exists exactly on the 1st, grab the nearest value within 5 days
    if ("datetime" %in% names(ts)) {
        ts$year <- as.integer(format(ts$datetime, "%Y"))
        ts$month <- as.integer(format(ts$datetime, "%m"))
        ts$day <- as.integer(format(ts$datetime, "%d"))

        # Only keep Feb-May
        ts <- ts[ts$month %in% 2:5, , drop = FALSE]

        # For each year/month, ensure a value exists for the 1st (or nearest within 5 days)
        keep_rows <- logical(nrow(ts))
        # Track which rows are snapped (not on the 1st)
        snapped_rows <- logical(nrow(ts))
        unique_ym <- unique(ts[, c("year", "month")])
        for (i in seq_len(nrow(unique_ym))) {
            y <- unique_ym$year[i]
            m <- unique_ym$month[i]
            idx <- which(ts$year == y & ts$month == m)
            # Prefer day==1
            idx1 <- idx[ts$day[idx] == 1]
            if (length(idx1) > 0) {
                keep_rows[idx1] <- TRUE
            } else {
                # Find nearest to 1st within 5 days
                days_from_first <- abs(ts$day[idx] - 1)
                min_diff <- min(days_from_first)
                if (min_diff <= 5) {
                    nearest_idx <- idx[which.min(days_from_first)]
                    keep_rows[nearest_idx] <- TRUE
                    snapped_rows[nearest_idx] <- TRUE
                }
            }
        }
        # Snap datetime to 1st of month for snapped rows
        if (any(snapped_rows)) {
            ts$datetime[snapped_rows] <- as.POSIXct(
                sprintf(
                    "%d-%02d-01",
                    ts$year[snapped_rows],
                    ts$month[snapped_rows]
                ),
                tz = "UTC"
            )
            ts$day[snapped_rows] <- 1
        }
        ts <- ts[keep_rows, , drop = FALSE]
        # Remove extra columns
        ts$year <- NULL
        ts$month <- NULL
        ts$day <- NULL
        ts$date_first_of_month <- NULL
        ts$date_diff_days <- NULL
    }

    # Wide timeseries: datetime + one column per station (stations as columns)
    if ("datetime" %in% names(ts)) {
        ts$datetime <- as.POSIXct(ts$datetime, tz = "UTC")
        n <- nrow(ts)
        day <- as.integer(format(ts$datetime, "%d"))
        month <- as.integer(format(ts$datetime, "%m"))
        year <- as.integer(format(ts$datetime, "%Y"))

        #doy <- snap_doy(doy)

        # station columns are everything except datetime (and any preexisting doy/year)
        station_cols <- setdiff(names(ts), c("datetime", "doy", "year"))
        # prepare output data.frames
        hist_df <- data.frame(datetime = ts$datetime, stringsAsFactors = FALSE)
        rel_df <- data.frame(datetime = ts$datetime, stringsAsFactors = FALSE)
        perc_df <- data.frame(datetime = ts$datetime, stringsAsFactors = FALSE)

        # Vectorized and grouped computation for speed
        p <- length(station_cols)
        if (p > 0) {
            vals_mat <- as.matrix(ts[, station_cols, drop = FALSE]) # n x p
            hist_mat <- matrix(NA_real_, nrow = n, ncol = p)
            rel_mat <- matrix(NA_real_, nrow = n, ncol = p)
            perc_mat <- matrix(NA_real_, nrow = n, ncol = p)

            # Group by month-day (usually day==1 after snapping), compute per group
            grp <- paste(month, day, sep = "-")
            ug <- unique(grp)

            for (g in ug) {
                idx_g <- which(grp == g)
                # Ensure chronological order within group
                idx_g <- idx_g[order(year[idx_g])]

                # Precompute years vector for the group
                yg <- year[idx_g]

                for (j_idx in seq_along(idx_g)) {
                    j <- idx_g[j_idx]

                    if (!is.null(lookback_year)) {
                        prev_idx <- idx_g[which(
                            yg < yg[j_idx] & yg >= lookback_year
                        )]
                    } else {
                        prev_idx <- idx_g[which(
                            yg < yg[j_idx] & yg >= (yg[j_idx] - lookback_length)
                        )]
                    }

                    if (length(prev_idx) > 0) {
                        prev_block <- vals_mat[prev_idx, , drop = FALSE]
                        hist_mat[j, ] <- apply(
                            prev_block,
                            2,
                            stats::median,
                            na.rm = TRUE
                        )

                        # Calculate percentile: what % of historical values are <= current value

                        if (yg[j_idx] >= 2000 && length(prev_idx) >= 4) {
                            current_values <- vals_mat[j, ]
                            if (all(is.na(current_values))) {
                                perc_mat[j, ] <- NA_real_
                            } else {
                                perc_mat[j, ] <- (colSums(
                                    prev_block <=
                                        matrix(
                                            current_values,
                                            nrow = nrow(prev_block),
                                            ncol = ncol(prev_block),
                                            byrow = TRUE
                                        ),
                                    na.rm = TRUE
                                ) /
                                    colSums(!is.na(prev_block), na.rm = TRUE)) *
                                    100
                            }
                        }
                    }
                }
            }

            # Compute relative SWE in a fully vectorized way
            cur <- vals_mat
            hist <- hist_mat

            # Case 1: standard percentage where historic median != 0
            mask1 <- !is.na(hist) & !is.na(cur) & (hist != 0)
            rel_mat[mask1] <- 100 * cur[mask1] / hist[mask1]

            # Case 2: both zero => -2
            mask2 <- !is.na(cur) & (cur == 0) & !is.na(hist) & (hist == 0)
            rel_mat[mask2] <- -2

            # Case 3: current > 0, historic == 0 => -1
            mask3 <- !is.na(cur) & (cur > 0) & !is.na(hist) & (hist == 0)
            rel_mat[mask3] <- -1

            colnames(hist_mat) <- station_cols
            colnames(rel_mat) <- station_cols

            hist_df[station_cols] <- as.data.frame(
                hist_mat,
                stringsAsFactors = FALSE
            )
            rel_df[station_cols] <- as.data.frame(
                rel_mat,
                stringsAsFactors = FALSE
            )

            colnames(perc_mat) <- station_cols
            perc_df[station_cols] <- as.data.frame(
                perc_mat,
                stringsAsFactors = FALSE
            )
        }

        # ensure ordering by datetime
        hist_df <- hist_df[order(hist_df$datetime), , drop = FALSE]
        rel_df <- rel_df[order(rel_df$datetime), , drop = FALSE]
        perc_df <- perc_df[order(perc_df$datetime), , drop = FALSE]

        return(list(
            historic_median = hist_df,
            relative_swe = rel_df,
            percentile = perc_df
        ))
    }

    stop(
        "Input timeseries must contain a 'datetime' column (and either 'value' for single-station or station columns for wide format)."
    )
}

#' Extract SWE data at specific points for a given year and month
#'
#' @param data List containing timeseries and metadata from download functions
#' @param year Integer target year for data extraction
#' @param month Integer target month for data extraction
#' @param key Character name of the key column in metadata (e.g., "timeseries_id", "location_id")
#' @return data.frame subset of metadata with additional SWE value columns:
#' \describe{
#'   \item{swe}{Absolute SWE value in mm}
#'   \item{relative_swe}{SWE as percentage of historic median}
#'   \item{historic_median}{Historic median SWE value for this date}
#' }
#'
#' @description
#' Extracts values for the target date from the reorganized timeseries structure
#' where data is organized by parameter type with stations as columns.
#'
#' @details
#' The function matches the target date (first of specified month/year) with
#' available timeseries data and extracts the relevant values for each station.
#' Missing data is represented as NA.
#'
#' @examples
#' \dontrun{
#' # Get pillow data for March 2025
#' pillow_data <- get_swe_state(
#'   base_data$pillows,
#'   year = 2025,
#'   month = 3,
#'   key = "timeseries_id"
#' )
#'
#' # Check data availability
#' valid_stations <- pillow_data[!is.na(pillow_data$swe), ]
#' }
get_swe_state <- function(
    data,
    year,
    month,
    key
) {
    # Assert that data contains timeseries and metadata
    stopifnot(is.list(data))
    stopifnot("timeseries" %in% names(data))
    stopifnot("metadata" %in% names(data))
    stopifnot(all(
        c("swe", "historic_median", "relative_swe") %in%
            names(data$timeseries)
    ))
    point_source_data <- data$metadata
    target_date <- get_datetime(year, month)

    # Validate that key column exists in metadata
    stopifnot(key %in% colnames(point_source_data))

    # Helper to extract value for each station at the closest matching date
    extract_at_date <- function(ts_df, col_key, target_date) {
        vals <- rep(NA_real_, nrow(point_source_data))
        if (!is.null(ts_df) && "datetime" %in% names(ts_df)) {
            for (i in seq_len(nrow(point_source_data))) {
                station_id <- as.character(point_source_data[[col_key]][i])
                if (!is.null(station_id) && station_id %in% names(ts_df)) {
                    # Find row in ts_df with closest date to target_date
                    dt <- as.Date(ts_df$datetime)
                    idx <- which(dt == as.Date(target_date))
                    if (length(idx) == 1) {
                        vals[i] <- as.numeric(ts_df[[station_id]][idx])
                    }
                }
            }
        }
        vals
    }

    point_source_data$swe <- extract_at_date(
        data$timeseries$swe,
        key,
        target_date
    )
    point_source_data$relative_swe <- extract_at_date(
        data$timeseries$relative_swe,
        key,
        target_date
    )
    point_source_data$historic_median <- extract_at_date(
        data$timeseries$historic_median,
        key,
        target_date
    )

    point_source_data$percentile <- extract_at_date(
        data$timeseries$percentile,
        key,
        target_date
    )

    return(point_source_data)

    # For backward compatibility, also set $value to match selected value_type if needed
    # (optional, depending on how downstream code expects it)
}

# =============================================================================
# POPUP CREATION FUNCTIONS
# =============================================================================

#' Create base64-encoded ggplotOverlap plot for continuous stations
#'
#' @param timeseries data.frame with 'datetime' and station value columns
#' @param year Integer year for plot focus
#' @param con Database connection for historical data access
#' @return Character string containing HTML with embedded base64 image
#'
#' @description
#' Creates a YGwater ggplotOverlap plot showing current year data
#' compared to historical range. Plot is saved as PNG, encoded to base64,
#' and embedded in HTML for use in leaflet popups.
#'
#' @details
#' The plot shows:
#' \itemize{
#'   \item Historical range (min/max) as shaded area
#'   \item Historical median as dashed line
#'   \item Current year data as solid line
#'   \item Click-to-expand functionality for full-size viewing
#' }
#'
#' @examples
#' \dontrun{
#' station_ts <- base_data$pillows$timeseries$swe[, c("datetime", "123")]
#' popup_html <- create_continuous_plot_popup(station_ts, 2025, con)
#' }
create_continuous_plot_popup <- function(
    timeseries,
    year,
    con,
    station_name = NULL
) {
    # Validate timeseries structure
    if (!is.data.frame(timeseries) || ncol(timeseries) < 2) {
        stop("Timeseries must have columns datetime and value")
    }

    if (is.null(station_name)) {
        station_name <- names(timeseries)[2]
    }

    names(timeseries) <- c("datetime", "value")

    timeseries <- timeseries[
        !is.na(timeseries$value) & !is.nan(timeseries$value),
    ]

    tryCatch(
        {
            plot <- ggplotOverlap(
                parameter = "Snow Water Equivalent",
                startDay = 240,
                endDay = 239,
                years = c(year - 1),
                returns = "none",
                custom_title = station_name,
                line_scale = 1 * 0.9,
                axis_scale = 1 * 0.9,
                legend_scale = 1 * 0.9,
                snowbulletin = TRUE,
                lang = "en",
                gridx = FALSE,
                gridy = FALSE,
                filter = c(100, 100),
                continuous_data = timeseries,
                con = con
            )
        },
        error = function(e) {
            return(
                "<div style='text-align: center; padding: 20px;'><p style='color: #d9534f;'>Error generating plot</p></div>"
            )
        }
    )

    # Save the ggplotOverlap plot as a PNG tempfile with smaller dimensions
    plot_file <- file.path(tempdir(), "plot.png")
    grDevices::png(plot_file, width = 800, height = 600, res = 120)
    print(plot)
    grDevices::dev.off()

    # Encode PNG to base64
    plot_data <- readBin(plot_file, "raw", file.info(plot_file)$size)
    plot_base64 <- base64enc::base64encode(plot_data)
    unlink(plot_file)

    # HTML for popup with smaller image and click-to-open functionality
    popup_html <- sprintf(
        paste0(
            "<div style='text-align: center; max-width: 660px; width: 100%%;'>",
            "<b>%s</b><br>",
            "<img src='data:image/png;base64,%s' ",
            "style='width: 100%%; height: auto; cursor: pointer; border: 1px solid #ccc; margin-top: 10px;' ",
            "onclick='window.open(\"data:image/png;base64,%s\", \"_blank\", \"width=800,height=600\");' ",
            "title='Click to open full size in new window' />",
            "<br><small style='color: #666;'>Click image to open full size</small>",
            "</div>"
        ),
        htmltools::htmlEscape(station_name),
        plot_base64,
        plot_base64
    )
    popup_html
}

#' Create base64-encoded plot for discrete stations
#'
#' @param timeseries data.frame with timeseries data formatted for plotting
#' @return Character string containing HTML with embedded base64 image
#'
#' @description
#' Creates a YGwater hydrometDiscrete plot of discrete measurements over time.
#' Plot is saved as PNG, encoded to base64, and embedded in HTML for leaflet popups.
#'
#' @details
#' The discrete plot shows monthly snow course measurements as:
#' \itemize{
#'   \item Line plot connecting measurements within each year
#'   \item Box plots showing distribution across years for each month
#'   \item Current year highlighted if available
#' }
#'
#' @examples
#' \dontrun{
#' station_ts <- base_data$surveys$timeseries$swe[, c("datetime", "456")]
#' popup_html <- create_discrete_plot_popup(station_ts)
#' }
create_discrete_plot_popup <- function(timeseries) {
    # Clean and validate the data before plotting
    station_name <- names(timeseries)[2]
    names(timeseries) <- c("datetime", "value")

    timeseries$month <- as.integer(format(timeseries$datetime, "%m"))
    timeseries$year <- as.integer(format(timeseries$datetime, "%Y"))
    timeseries$units <- "mm"
    tryCatch(
        {
            plot <- hydrometDiscrete(
                parameter = "SWE",
                location = station_name,
                startDay = 1,
                discrete_data = timeseries,
                years = c(max(timeseries$year, na.rm = TRUE)),
                plot_type = "boxplot"
            )
        },
        error = function(e) {
            return(
                "<div style='text-align: center; padding: 20px;'><p style='color: #d9534f;'>Error generating plot</p></div>"
            )
        }
    )

    # Save the plot as a PNG tempfile with smaller dimensions
    plot_file <- tempfile(fileext = ".png")

    # Suppress warnings during PNG creation to avoid the geom_line warning
    suppressWarnings({
        png(plot_file, width = 800, height = 600, res = 120)
        print(plot)
        dev.off()
    })

    # Encode PNG to base64
    plot_data <- readBin(plot_file, "raw", file.info(plot_file)$size)
    plot_base64 <- base64enc::base64encode(plot_data)
    unlink(plot_file)

    # HTML for popup with smaller image and click-to-open functionality
    popup_html <- sprintf(
        paste0(
            "<div style='text-align: center; max-width: 660px; width: 100%%;'>",
            "<b>%s</b><br>",
            "<img src='data:image/png;base64,%s' ",
            "style='width: 100%%; height: auto; cursor: pointer; border: 1px solid #ccc; margin-top: 10px;' ",
            "onclick='window.open(\"data:image/png;base64,%s\", \"_blank\", \"width=800,height=600\");' ",
            "title='Click to open full size in new window' />",
            "<br><small style='color: #666;'>Click image to open full size</small>",
            "</div>"
        ),
        htmltools::htmlEscape(station_name),
        plot_base64,
        plot_base64
    )
    return(popup_html)
}

#' Load all base data for the SWE mapping application
#'
#' @param con DBI database connection object
#' @return A list containing all loaded base data:
#' \describe{
#'   \item{pillows}{List with continuous station timeseries and metadata}
#'   \item{surveys}{List with discrete station timeseries and metadata}
#'   \item{basins}{List with basin-averaged timeseries and metadata}
#'   \item{shapefiles}{List with spatial data (yukon, communities, roads)}
#' }
#'
#' @description
#' Loads all static data and reorganizes timeseries structure for efficient access.
#' Basin-averaged SWE is calculated for each timestep using discrete station data
#' and weighting factors.
#'
#' @details
#' This function performs several key operations:
#' \enumerate{
#'   \item Loads continuous and discrete SWE timeseries
#'   \item Calculates historical medians and relative values
#'   \item Computes basin-averaged SWE using weighting factors
#'   \item Loads spatial layers (boundaries, communities, roads)
#'   \item Applies label position adjustments for optimal display
#' }
#'
#' The resulting data structure is optimized for both interactive (Shiny) and
#' static (export) mapping applications.
#'
#' @examples
#' \dontrun{
#' con <- AquaCache::AquaConnect(...)
#' base_data <- load_base_data(con)
#'
#' # Check data availability
#' print(sprintf("Loaded %d pillow stations", nrow(base_data$pillows$metadata)))
#' print(sprintf("Loaded %d survey stations", nrow(base_data$surveys$metadata)))
#' print(sprintf("Loaded %d basins", nrow(base_data$basins$metadata)))
#' }
load_base_data <- function(con) {
    base_data <- list(
        pillows = list(),
        surveys = list(),
        basins = list(),
        shapefiles = list()
    )

    # Load timeseries data from both continuous and discrete sources
    continuous_data <- download_continuous_ts(con)

    ret <- calculate_historic_daily_median(
        continuous_data$timeseries$swe,
        lookback_year = 1980
    )
    continuous_data$timeseries$historic_median <- ret$historic_median
    continuous_data$timeseries$relative_swe <- ret$relative_swe
    continuous_data$timeseries$percentile <- ret$percentile
    base_data$pillows <- continuous_data

    discrete_data <- download_discrete_ts(con)

    ret <- calculate_historic_daily_median(
        discrete_data$timeseries$swe,
        lookback_year = 1980
    )
    discrete_data$timeseries$historic_median <- ret$historic_median
    discrete_data$timeseries$relative_swe <- ret$relative_swe
    discrete_data$timeseries$percentile <- ret$percentile
    base_data$surveys <- discrete_data

    # Ensure discrete SWE wide timeseries is available

    # Load or infer weight matrix from snowcourse factors CSV using discrete metadata
    weights_df <- load_snowcourse_factors(
        metadata_discrete = discrete_data$metadata
    )

    # If we can read basin polygons from shapefile now (so basin names exist), prefer them
    basins_shp <- sf::st_read(
        system.file(
            "snow_survey/swe_basins/swe_basins.shp",
            package = "YGwater",
            mustWork = TRUE
        ),
        quiet = TRUE
    )

    # Calculate basin areas in square kilometers
    basins_shp$area_km2 <- sf::st_area(basins_shp) |>
        units::set_units("km^2") |>
        as.numeric()

    if ("SWE_Basin" %in% names(basins_shp)) {
        names(basins_shp)[names(basins_shp) == "SWE_Basin"] <- "name"
    }

    # Ensure CRS is WGS84 for leaflet plotting
    if (sf::st_crs(basins_shp)$epsg != 4326) {
        basins_shp <- sf::st_transform(basins_shp, 4326)
    }

    # Prepare dates and station list from discrete wide timeseries
    basin_dates <- base_data$surveys$timeseries$swe$datetime

    # Convert station_cols from location_id to location (name) using discrete metadata
    basin_names <- basins_shp$name
    basin_swe_mat <- matrix(
        NA_real_,
        nrow = length(basin_dates),
        ncol = length(basin_names)
    )

    for (i in seq_along(basin_dates)) {
        basin_swe_mat[i, ] <- NA_real_

        weight_matrix <- weights_df[,
            c("location_id", basin_names),
            drop = FALSE
        ]

        swe_samples <- as.numeric(discrete_data$timeseries$swe[
            i,
            as.character(weight_matrix$location_id),
            drop = TRUE
        ])

        nan_samples <- is.na(swe_samples)

        swe_samples <- swe_samples[!nan_samples]
        weight_matrix <- weight_matrix[!nan_samples, ]
        # Drop location_id column before normalization and multiplication
        weight_matrix <- weight_matrix[, -1, drop = FALSE]

        # Normalize columns so that each basin's weights sum to 1
        weight_matrix <- sweep(
            weight_matrix,
            2,
            colSums(weight_matrix, na.rm = TRUE),
            FUN = "/"
        )

        # Remove the location_id column for multiplication
        basin_vals <- as.numeric(
            t(matrix(swe_samples, ncol = 1)) %*% as.matrix(weight_matrix)
        )

        basin_swe_mat[i, ] <- basin_vals
    }

    # Build timeseries data.frame (datetime + basin columns)
    basin_timeseries <- data.frame(
        datetime = basin_dates,
        stringsAsFactors = FALSE
    )

    basin_timeseries[, basin_names] <- NA_real_
    basin_timeseries[, basin_names] <- basin_swe_mat

    # Compute historic median and relative change for basins (wide format)
    ret <- calculate_historic_daily_median(
        basin_timeseries,
        lookback_year = 1980
    )

    base_data$basins$timeseries <- list(
        swe = basin_timeseries,
        historic_median = ret$historic_median,
        relative_swe = ret$relative_swe,
        percentile = ret$percentile
    )

    prov_sf <- download_spatial_layer(
        con,
        "Provincial/Territorial Boundaries",
        additional_query = "AND feature_name = 'Yukon'"
    )
    # Ensure CRS is WGS84 for leaflet
    if (!is.null(prov_sf) && sf::st_crs(prov_sf)$epsg != 4326) {
        prov_sf <- sf::st_transform(prov_sf, 4326)
    }
    base_data$shapefiles$yukon <- prov_sf

    bbox_extent <- sf::st_as_sfc(sf::st_bbox(
        c(xmin = -170, xmax = -90, ymin = 50, ymax = 90),
        crs = 4326
    ))
    # Ensure CRS match for st_difference
    if (
        !is.null(base_data$shapefiles$yukon) &&
            sf::st_crs(bbox_extent) != sf::st_crs(base_data$shapefiles$yukon)
    ) {
        base_data$shapefiles$yukon <- sf::st_transform(
            base_data$shapefiles$yukon,
            sf::st_crs(bbox_extent)
        )
    }
    # Guard against NULL Yukon geometry
    if (!is.null(base_data$shapefiles$yukon)) {
        base_data$shapefiles$inverted_yukon <- sf::st_difference(
            bbox_extent,
            sf::st_union(base_data$shapefiles$yukon)
        )
    } else {
        base_data$shapefiles$inverted_yukon <- bbox_extent
    }

    roads <- download_spatial_layer(con, "Roads")
    # Ensure CRS is WGS84 for leaflet
    if (!is.null(roads) && sf::st_crs(roads)$epsg != 4326) {
        roads <- sf::st_transform(roads, 4326)
    }
    base_data$shapefiles$roads <- roads

    place_types <- c("City", "Town", "Village") # Filter to major communities only
    communities <- download_spatial_layer(
        con,
        "Communities",
        additional_query = sprintf(
            "AND (description IN (%s) OR feature_name IN ('Old Crow', 'Beaver Creek'))",
            paste(
                sapply(
                    place_types,
                    function(x) DBI::dbQuoteString(con, x)
                ),
                collapse = ", "
            )
        )
    )
    # Ensure CRS is WGS84 for leaflet
    if (!is.null(communities) && sf::st_crs(communities)$epsg != 4326) {
        communities <- sf::st_transform(communities, 4326)
    }

    # Add popup, annotate, and annotation columns to communities
    if (!is.null(communities)) {
        communities$popup <- sprintf(
            "<div style='text-align: left; padding: 10px;'><b>%s</b><br><span style='font-size: 12px; color: #666;'>%s</span></div>",
            htmltools::htmlEscape(communities$feature_name),
            htmltools::htmlEscape(communities$description)
        )
        communities$annotate <- communities$description %in% place_types
        communities$annotation <- communities$feature_name

        communities$annotation <- vapply(
            communities$annotation,
            FUN.VALUE = character(1),
            FUN = function(x) {
                if (is.na(x) || x == "") {
                    return(x)
                }
                # Add line break after first word if it's 5+ characters
                sub("^(\\S{5,})\\s+", "\\1<br>", x, perl = TRUE)
            }
        )

        # Create named list of label position adjustments for each community
        community_adjustments <- stats::setNames(
            lapply(communities$feature_name, function(name) {
                list(x = 0, y = 0) # Default: no offset
            }),
            communities$feature_name
        )

        # Customize specific communities
        community_adjustments[["Whitehorse"]] <- list(x = 0, y = 10)
        community_adjustments[["Dawson City"]] <- list(x = 0, y = 0)
        community_adjustments[["Watson Lake"]] <- list(x = 60, y = 0)
        community_adjustments[["Haines Junction"]] <- list(x = -70, y = -80)
        community_adjustments[["Carmacks"]] <- list(x = 20, y = -40)
        community_adjustments[["Mayo"]] <- list(x = 0, y = -40)
        community_adjustments[["Pelly Crossing"]] <- list(x = 0, y = 0)
        community_adjustments[["Ross River"]] <- list(x = 0, y = 0)
        community_adjustments[["Teslin"]] <- list(x = 60, y = 10)
        community_adjustments[["Beaver Creek"]] <- list(x = 40, y = 0)
        community_adjustments[["Burwash Landing"]] <- list(x = 0, y = 0)
        community_adjustments[["Carcross"]] <- list(x = 0, y = 0)
        community_adjustments[["Faro"]] <- list(x = 0, y = -30)
        community_adjustments[["Old Crow"]] <- list(x = 0, y = 10)
        community_adjustments[["Inuvik"]] <- list(x = 0, y = 0)

        # Get coordinates and apply adjustments
        comm_coords <- sf::st_coordinates(communities)

        communities$x <- comm_coords[, 1]
        communities$y <- comm_coords[, 2]
        communities$x_adjusted <- communities$x +
            vapply(
                communities$feature_name,
                function(n) {
                    if (n %in% names(community_adjustments)) {
                        community_adjustments[[n]]$x / 111.32 # convert km -> degrees (approx)
                    } else {
                        0
                    }
                },
                numeric(1)
            )
        communities$y_adjusted <- communities$y +
            vapply(
                communities$feature_name,
                function(n) {
                    if (n %in% names(community_adjustments)) {
                        community_adjustments[[n]]$y / 111.32 # convert km -> degrees (approx)
                    } else {
                        0
                    }
                },
                numeric(1)
            )
    }

    base_data$shapefiles$communities <- communities

    # Process basin names for better display on map
    base_data$basins$metadata <- basins_shp
    base_data$basins$metadata$annotation <- base_data$basins$metadata$Label
    base_data$basins$metadata$annotation <- gsub(
        "_",
        " ",
        as.character(base_data$basins$metadata$annotation)
    )
    base_data$basins$metadata$annotation <- vapply(
        base_data$basins$metadata$annotation,
        FUN.VALUE = character(1),
        FUN = function(x) {
            if (is.na(x) || x == "") {
                return(x)
            }
            # Add line break after first word if it's 5+ characters
            sub("^(\\S{5,})\\s+", "\\1<br>", x, perl = TRUE)
        }
    )

    # Create named list of label position adjustments for each basin
    basin_adjustments <- stats::setNames(
        lapply(base_data$basins$metadata$name, function(name) {
            list(x = 0, y = 0) # Default: no offset
        }),
        base_data$basins$metadata$name
    )

    # Customize specific basins
    basin_adjustments[["Lower_Yukon"]] <- list(x = -100, y = 40)
    basin_adjustments[["Liard"]] <- list(x = -50, y = 60)
    basin_adjustments[["Upper_Yukon"]] <- list(x = -75, y = -10)
    basin_adjustments[["Pelly"]] <- list(x = 0, y = 20)
    basin_adjustments[["Porcupine"]] <- list(x = 120, y = -30)
    basin_adjustments[["Stewart"]] <- list(x = 0, y = 40)
    basin_adjustments[["Teslin_Big_Salmon"]] <- list(x = -70, y = 90)
    basin_adjustments[["Central_Yukon"]] <- list(x = -115, y = 30)
    basin_adjustments[["White"]] <- list(x = 10, y = -20)
    basin_adjustments[["Alsek"]] <- list(x = 0, y = -80)

    # Get basin centroids and apply adjustments
    basin_centroids <- sf::st_centroid(base_data$basins$metadata)
    basin_coords <- sf::st_coordinates(basin_centroids)
    base_data$basins$metadata$x <- basin_coords[, 1]
    base_data$basins$metadata$y <- basin_coords[, 2]

    base_data$basins$metadata$x_adjusted <- base_data$basins$metadata$x +
        vapply(
            base_data$basins$metadata$name,
            function(n) {
                if (n %in% names(basin_adjustments)) {
                    basin_adjustments[[n]]$x / 111.32 # convert km -> degrees (approx)
                } else {
                    0
                }
            },
            numeric(1)
        )
    base_data$basins$metadata$y_adjusted <- base_data$basins$metadata$y +
        vapply(
            base_data$basins$metadata$name,
            function(n) {
                if (n %in% names(basin_adjustments)) {
                    basin_adjustments[[n]]$y / 111.32 # convert km -> degrees (approx)
                } else {
                    0
                }
            },
            numeric(1)
        )
    return(base_data)
}

get_processed_data <- function(year, month, base_data, shiny = TRUE) {
    # Extract data at points for the selected date
    swe_at_basins <- get_swe_state(
        data = base_data$basins,
        year = year,
        month = month,
        key = "name"
    )
    swe_at_surveys <- get_swe_state(
        data = base_data$surveys,
        year = year,
        month = month,
        key = "location_id"
    )
    swe_at_pillows <- get_swe_state(
        data = base_data$pillows,
        year = year,
        month = month,
        key = "timeseries_id"
    )

    # Ensure all swe_at_* columns are numeric (especially percentiles)
    swe_at_basins$swe <- as.numeric(swe_at_basins$swe)
    swe_at_basins$relative_swe <- as.numeric(swe_at_basins$relative_swe)
    swe_at_basins$historic_median <- as.numeric(swe_at_basins$historic_median)
    swe_at_basins$percentile <- as.numeric(swe_at_basins$percentile)

    swe_at_surveys$swe <- as.numeric(swe_at_surveys$swe)
    swe_at_surveys$relative_swe <- as.numeric(swe_at_surveys$relative_swe)
    swe_at_surveys$historic_median <- as.numeric(swe_at_surveys$historic_median)
    swe_at_surveys$percentile <- as.numeric(swe_at_surveys$percentile)

    swe_at_pillows$swe <- as.numeric(swe_at_pillows$swe)
    swe_at_pillows$relative_swe <- as.numeric(swe_at_pillows$relative_swe)
    swe_at_pillows$historic_median <- as.numeric(swe_at_pillows$historic_median)
    swe_at_pillows$percentile <- as.numeric(swe_at_pillows$percentile)

    swe_at_basins$annotation <- paste0(
        swe_at_basins$annotation,
        "<br>(",
        round(swe_at_basins$relative_swe, 1),
        "%)"
    )

    generate_popup_content <- function(
        type,
        swe,
        relative_swe,
        historic_median,
        name,
        location = NULL,
        id = NULL,
        percentile = NULL
    ) {
        type_label <- switch(
            type,
            "basin" = "<b>Type:</b> Discrete (weighted average)<br>",
            "survey" = "<b>Type:</b> Discrete (Snow Course)<br>",
            "pillow" = "<b>Type:</b> Continuous (Pillow)<br>",
            ""
        )

        name <- gsub("_", " ", name)

        plot_button <- if (shiny && !is.null(id)) {
            sprintf(
                "<button onclick='generatePlot(\"%s\", \"%s\", \"%s\")' style='margin-top: 10px;'>Generate Plot</button>",
                type,
                id,
                name
            )
        } else {
            ""
        }
        location_html <- if (!is.null(location)) {
            paste0(
                "<span style='font-size: 12px; color: #666;'>",
                location,
                "</span><br><br>"
            )
        } else {
            "<br>"
        }

        # Get basin area if type is basin, otherwise get coordinates
        area_html <- if (type == "basin") {
            basin_area <- base_data$basins$metadata$area_km2[
                base_data$basins$metadata$name == name
            ]
            basin_elev <- base_data$basins$metadata$MeanElev_m[
                base_data$basins$metadata$name == name
            ]
            area_html <- ""
            if (length(basin_area) > 0 && !is.na(basin_area)) {
                area_html <- paste0(
                    "<b>Area:</b> ",
                    round(basin_area, 1),
                    " km²<br>"
                )
            }
            if (length(basin_elev) > 0 && !is.na(basin_elev)) {
                area_html <- paste0(
                    area_html,
                    "<b>Mean Elevation:</b> ",
                    round(basin_elev, 0),
                    " m<br>"
                )
            }
            area_html
        } else if (type %in% c("survey", "pillow")) {
            # Get coordinates and round them
            if (type == "survey") {
                coord_data <- base_data$surveys$metadata[
                    base_data$surveys$metadata$location_id == id,
                ]
            } else {
                coord_data <- base_data$pillows$metadata[
                    base_data$pillows$metadata$timeseries_id == id,
                ]
            }
            if (nrow(coord_data) > 0) {
                coords <- sf::st_coordinates(coord_data)
                lat <- round(coords[1, 2], 2)
                lon <- round(coords[1, 1], 2)
                elev_html <- ""
                if (!is.na(coord_data$conversion_m[1])) {
                    elev_html <- paste0(
                        "<b>Elevation:</b> ",
                        round(coord_data$conversion_m[1], 0),
                        " m<br>"
                    )
                }
                paste0(
                    "<b>Coordinates:</b> ",
                    lat,
                    "N, ",
                    lon,
                    "W<br>",
                    elev_html
                )
            } else {
                ""
            }
        } else {
            ""
        }

        paste0(
            "<div style='text-align: left; padding: 10px; width: 300px;'>",
            "<b style='font-size: 16px;'>",
            name,
            "</b><br>",
            location_html,
            type_label,
            area_html,
            "<br>",
            "<b>SWE Value:</b> ",
            if (!is.na(swe)) paste0(round(swe, 1), " mm") else "No data",
            "<br>",
            "<b>SWE Value:</b> ",
            if (!is.na(relative_swe)) {
                paste0(round(relative_swe, 1), "% of normal")
            } else {
                "No data"
            },
            "<br>",
            "<b>Historic Median:</b> ",
            if (!is.na(historic_median)) {
                paste0(round(historic_median, 1), " mm")
            } else {
                "No data"
            },
            "<br>",
            "<b>Percentile:</b> ",
            if (!is.null(percentile) && !is.na(percentile)) {
                paste0(round(percentile, 1), "th percentile")
            } else {
                "No data"
            },
            "<br>",
            plot_button,
            "</div>"
        )
    }

    # Fix mapply argument mismatch for popup_content
    swe_at_basins$popup_content <- mapply(
        function(swe, relative_swe, historic_median, percentile, name) {
            generate_popup_content(
                "basin",
                swe,
                relative_swe,
                historic_median,
                name,
                location = NULL,
                id = name,
                percentile = percentile
            )
        },
        swe_at_basins$swe,
        swe_at_basins$relative_swe,
        swe_at_basins$historic_median,
        swe_at_basins$percentile,
        swe_at_basins$name,
        SIMPLIFY = FALSE
    )
    swe_at_surveys$popup_content <- mapply(
        function(
            swe,
            relative_swe,
            historic_median,
            percentile,
            name,
            location,
            id
        ) {
            generate_popup_content(
                "survey",
                swe,
                relative_swe,
                historic_median,
                name,
                location,
                id,
                percentile
            )
        },
        swe_at_surveys$swe,
        swe_at_surveys$relative_swe,
        swe_at_surveys$historic_median,
        swe_at_surveys$percentile,
        swe_at_surveys$name,
        swe_at_surveys$location,
        swe_at_surveys$location_id,
        SIMPLIFY = FALSE
    )
    swe_at_pillows$popup_content <- mapply(
        function(
            swe,
            relative_swe,
            historic_median,
            percentile,
            name,
            location,
            id
        ) {
            generate_popup_content(
                "pillow",
                swe,
                relative_swe,
                historic_median,
                name,
                location,
                id,
                percentile
            )
        },
        swe_at_pillows$swe,
        swe_at_pillows$relative_swe,
        swe_at_pillows$historic_median,
        swe_at_pillows$percentile,
        swe_at_pillows$name,
        swe_at_pillows$location,
        swe_at_pillows$timeseries_id,
        SIMPLIFY = FALSE
    )

    filter_by_latest_date <- function(df, input_date, cutoff_days) {
        df[
            is.na(df$latest_date) |
                as.numeric(difftime(
                    input_date,
                    df$latest_date,
                    units = "days"
                )) <=
                    cutoff_days,
            ,
            drop = FALSE
        ]
    }

    input_date <- get_datetime(year, month)

    swe_at_surveys <- filter_by_latest_date(
        swe_at_surveys,
        input_date,
        366
    )

    swe_at_pillows <- filter_by_latest_date(
        swe_at_pillows,
        input_date,
        366
    )

    list(
        swe_at_basins = swe_at_basins,
        swe_at_surveys = swe_at_surveys,
        swe_at_pillows = swe_at_pillows
    )
}


#' Create an interactive leaflet map for SWE basins and stations
#'
#' @param year Integer year (e.g., 2025)
#' @param month Integer month (e.g., 3 for March)
#' @param base_data Optional preloaded base_data from \code{load_base_data()}, otherwise loads from default connection
#' @param filename Optional character string for HTML output file path
#' @return A leaflet map object with SWE basins and stations
#'
#' @description
#' Creates an interactive leaflet map showing SWE conditions across the Yukon Territory.
#' Includes basin polygons, station markers, and interactive popups with detailed information.
#'
#' @details
#' The map includes:
#' \itemize{
#'
#'   \item Historical range (min/max) as shaded area
#'   \item Historical median as dashed line
#'   \item Current year data as solid line
#'   \item Click-to-expand functionality for full-size viewing
#' }
#'
#' @examples
#' \dontrun{
#' # Create map for April 2025
#' map <- leaflet_snow_bulletin_map(2025, 4)
#'
#' # Save to file
#' leaflet_snow_bulletin_map(2025, 4, filename = "swe_map_apr2025.html")
#' }
#'
#' @export
leaflet_snow_bulletin_map <- function(
    year,
    month,
    base_data = NULL,
    filename = NULL
) {
    # Load required packages
    requireNamespace("leaflet")
    requireNamespace("sf")
    requireNamespace("htmltools")

    # Define color palettes and bins
    viz_params <- initialize_visualization_parameters()

    # Load base_data if not provided

    if (is.null(base_data)) {
        con <- AquaCache::AquaConnect(
            host = "10.250.12.154",
            port = 5432,
            user = "public_reader",
            password = "aquacache"
        )
        on.exit(DBI::dbDisconnect(con))
        base_data <- load_base_data(con)
    }

    data <- get_processed_data(
        year = year,
        month = month,
        base_data = base_data,
        shiny = FALSE
    )

    value_col <- "relative_swe"
    data$swe_at_basins$value_to_show <- data$swe_at_basins[[value_col]]
    data$swe_at_surveys$value_to_show <- data$swe_at_surveys[[value_col]]
    data$swe_at_pillows$value_to_show <- data$swe_at_pillows[[value_col]]
    pal_domain <- c(
        data$swe_at_basins$value_to_show,
        data$swe_at_surveys$value_to_show,
        data$swe_at_pillows$value_to_show
    )

    swe_pal <- leaflet::colorBin(
        palette = viz_params$basins$fillColor,
        bins = viz_params$basins$bins,
        domain = pal_domain,
        na.color = "gray"
    )

    # Set default bounds to Yukon if available, otherwise use fallback
    if (!is.null(base_data$shapefiles$yukon)) {
        bbox <- sf::st_bbox(base_data$shapefiles$yukon)
    } else {
        bbox <- c(xmin = -141, ymin = 60, xmax = -123, ymax = 69.6)
    }

    m <- leaflet::leaflet(
        options = leaflet::leafletOptions(
            zoomDelta = 0.5,
            zoomSnap = 0.25,
            wheelPxPerZoomLevel = 120
        )
    ) %>%
        leaflet::addProviderTiles(
            leaflet::providers$Esri.WorldTerrain,
            group = "Topographic"
        ) %>%
        leaflet::fitBounds(
            as.numeric(bbox["xmin"]),
            as.numeric(bbox["ymin"]),
            as.numeric(bbox["xmax"]),
            as.numeric(bbox["ymax"])
        ) %>%
        leaflet::addPolygons(
            data = data$swe_at_basins,
            fillColor = ~ swe_pal(value_to_show),
            color = viz_params$basins$color,
            weight = viz_params$basins$weight,
            opacity = viz_params$basins$opacity,
            fillOpacity = viz_params$basins$fillOpacity,
            label = ~ lapply(annotation, htmltools::HTML),
            popup = ~ lapply(popup_content, htmltools::HTML),
            popupOptions = do.call(
                leaflet::popupOptions,
                viz_params$basins$popupOptions
            ),
            group = "Basins averages"
        ) %>%
        leaflet::addLabelOnlyMarkers(
            data = data$swe_at_basins,
            lng = data$swe_at_basins$x_adjusted,
            lat = data$swe_at_basins$y_adjusted,
            label = ~ lapply(data$swe_at_basins$annotation, htmltools::HTML),
            labelOptions = leaflet::labelOptions(
                noHide = TRUE,
                direction = "center",
                textOnly = TRUE,
                style = viz_params$basins$label
            ),
            group = "Basins averages"
        ) %>%
        {
            if (!is.null(base_data$shapefiles$roads)) {
                leaflet::addPolylines(
                    .,
                    data = base_data$shapefiles$roads,
                    color = viz_params$roads$color,
                    weight = viz_params$roads$weight,
                    opacity = viz_params$roads$opacity,
                    group = "Roads",
                    label = ~ lapply(
                        as.character(feature_name),
                        htmltools::HTML
                    )
                )
            } else {
                .
            }
        } %>%
        {
            if (!is.null(base_data$shapefiles$yukon)) {
                leaflet::addPolygons(
                    .,
                    data = base_data$shapefiles$yukon,
                    color = viz_params$boundary$color,
                    weight = viz_params$boundary$weight,
                    fill = viz_params$boundary$fill,
                    group = "Boundary"
                )
            } else {
                .
            }
        } %>%
        leaflet::addCircleMarkers(
            data = data$swe_at_surveys,
            radius = viz_params$surveys$radius,
            color = viz_params$surveys$color,
            fillColor = ~ swe_pal(value_to_show),
            weight = viz_params$surveys$weight,
            opacity = viz_params$surveys$opacity,
            fillOpacity = viz_params$surveys$fillOpacity,
            label = ~ lapply(paste0(name, "<br>", location), htmltools::HTML),
            popup = ~ lapply(popup_content, htmltools::HTML),
            popupOptions = do.call(
                leaflet::popupOptions,
                viz_params$basins$popupOptions
            ),
            group = "Snow surveys (discrete)"
        ) %>%
        leaflet::addCircleMarkers(
            data = data$swe_at_pillows,
            radius = viz_params$pillows$radius,
            color = viz_params$pillows$color,
            fillColor = ~ swe_pal(value_to_show),
            weight = viz_params$pillows$weight,
            opacity = viz_params$pillows$opacity,
            fillOpacity = viz_params$pillows$fillOpacity,
            label = ~ lapply(paste0(name, "<br>", location), htmltools::HTML),
            popup = ~ lapply(popup_content, htmltools::HTML),
            popupOptions = do.call(
                leaflet::popupOptions,
                viz_params$basins$popupOptions
            ),
            group = "Snow pillows (continuous)"
        ) %>%
        {
            if (!is.null(base_data$shapefiles$communities)) {
                comm <- base_data$shapefiles$communities
                . <- leaflet::addMarkers(
                    .,
                    data = comm,
                    icon = leaflet::icons(
                        iconUrl = viz_params$communities$icon,
                        iconWidth = viz_params$communities$iconWidth,
                        iconHeight = viz_params$communities$iconHeight
                    ),
                    label = ~ lapply(annotation, htmltools::HTML),
                    popup = ~ lapply(popup, htmltools::HTML),
                    popupOptions = do.call(
                        leaflet::popupOptions,
                        viz_params$basins$popupOptions
                    ),
                    group = "Communities"
                )
                . <- leaflet::addLabelOnlyMarkers(
                    .,
                    data = comm,
                    lng = comm$x,
                    lat = comm$y,
                    label = ~ lapply(comm$annotation, htmltools::HTML),
                    labelOptions = leaflet::labelOptions(
                        noHide = viz_params$communities$labelOptions$noHide,
                        direction = viz_params$communities$labelOptions$direction,
                        textOnly = viz_params$communities$labelOptions$textOnly,
                        style = list(
                            color = viz_params$communities$label$color,
                            fontSize = viz_params$communities$label$fontSize,
                            fontWeight = viz_params$communities$label$fontWeight,
                            fontStyle = viz_params$communities$label$fontStyle,
                            textShadow = viz_params$communities$label$textShadow
                        )
                    ),
                    group = "Communities"
                )
                .
            } else {
                .
            }
        } %>%
        leaflet::addLayersControl(
            baseGroups = "Topographic",
            overlayGroups = c(
                "Boundary",
                "Roads",
                "Communities",
                "Basins averages",
                "Snow surveys (discrete)",
                "Snow pillows (continuous)"
            ),
            options = leaflet::layersControlOptions(collapsed = FALSE)
        ) %>%
        leaflet::addLegend(
            position = "bottomright",
            pal = swe_pal,
            values = pal_domain,
            title = "% of Normal",
            labFormat = leaflet::labelFormat(suffix = "%"),
            opacity = 1
        )

    if (!is.null(filename)) {
        cat(sprintf("Saving map to file: %s\n", filename))
        requireNamespace("pandoc")

        loc <- pandoc::pandoc_locate()
        if (is.null(loc)) {
            stop("Pandoc installation not found. Please install pandoc.")
        }

        htmlwidgets::saveWidget(
            m,
            file = filename,
            selfcontained = TRUE
        )
    }

    return(m)
}


#' Create a static ggplot2 map for SWE basins and stations
#'
#' @param year Integer year (e.g., 2025)
#' @param month Integer month (e.g., 3 for March)
#' @param base_data Optional preloaded base_data from \code{load_base_data()}, otherwise loads from default connection
#' @param width Numeric width of the plot in inches (default: 12)
#' @param height Numeric height of the plot in inches (default: 8)
#' @param filename Optional character string for PNG output file path
#' @param dpi Numeric resolution in dots per inch (default: 300)
#' @return A ggplot2 object with SWE basins and stations
#'
#' @description
#' Creates a publication-ready static map showing SWE conditions across the Yukon Territory.
#' Optimized for high-resolution output and professional presentation.
#'
#' @details
#' The static map features:
#' \itemize{
#'   \item High-quality terrain background
#'   \item SWE basins with color-coded values and labels
#'   \item Station markers with consistent symbology
#'   \item Communities with optimally positioned labels
#'   \item Roads and territorial boundaries
#'   \item Professional typography and layout
#'   \item Proper coordinate system and extent
#' }
#'
#' Label positions are pre-calculated and adjusted to minimize overlap and
#' maximize readability across different map extents.
#'
#' @examples
#' \dontrun{
#' # Create plot for May 2025
#' p <- ggplot_snow_bulletin_map(2025, 5)
#' print(p)
#'
#' # Save high-resolution PNG
#' ggplot_snow_bulletin_map(
#'   2025, 5,
#'   filename = "swe_map_may2025.png",
#'   width = 16, height = 10, dpi = 300
#' )
#' }
#'
#' @export
ggplot_snow_bulletin_map <- function(
    year,
    month,
    base_data = NULL,
    width = 12,
    height = 8,
    filename = NULL,
    dpi = 300
) {
    # Load required packages
    requireNamespace("ggplot2")
    requireNamespace("sf")

    viz_params <- initialize_visualization_parameters()

    # Load base_data if not provided
    if (is.null(base_data)) {
        con <- AquaCache::AquaConnect(
            host = "10.250.12.154",
            port = 5432,
            user = "public_reader",
            password = "aquacache"
        )
        on.exit(DBI::dbDisconnect(con))
        base_data <- load_base_data(con)
    }
    data <- get_processed_data(
        year = year,
        month = month,
        base_data = base_data,
        shiny = FALSE
    )

    value_col <- "relative_swe"
    data$swe_at_basins$value_to_show <- data$swe_at_basins[[value_col]]
    data$swe_at_surveys$value_to_show <- data$swe_at_surveys[[value_col]]
    data$swe_at_pillows$value_to_show <- data$swe_at_pillows[[value_col]]

    # Create color mapping function using same bins and colors as leaflet
    create_color_mapping <- function(values) {
        cut_values <- cut(
            values,
            breaks = viz_params$basins$bins,
            include.lowest = TRUE,
            right = FALSE
        )
        colors <- viz_params$basins$fillColor[as.numeric(cut_values)]
        colors[is.na(colors)] <- "gray"
        return(colors)
    }

    # Apply colors to data
    data$swe_at_basins$fill_color <- create_color_mapping(
        data$swe_at_basins$value_to_show
    )
    data$swe_at_surveys$fill_color <- create_color_mapping(
        data$swe_at_surveys$value_to_show
    )
    data$swe_at_pillows$fill_color <- create_color_mapping(
        data$swe_at_pillows$value_to_show
    )

    # Get coordinates for stations
    surveys_coords <- sf::st_coordinates(data$swe_at_surveys)
    data$swe_at_surveys$x <- surveys_coords[, 1]
    data$swe_at_surveys$y <- surveys_coords[, 2]

    pillows_coords <- sf::st_coordinates(data$swe_at_pillows)
    data$swe_at_pillows$x <- pillows_coords[, 1]
    data$swe_at_pillows$y <- pillows_coords[, 2]

    # Create the base plot
    p <- ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::theme(
            legend.position = "right",
            plot.title = ggplot2::element_text(
                hjust = 0.5,
                size = 14,
                face = "bold"
            ),
            plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 12)
        )

    # Add Yukon boundary background (underneath everything except basins)
    if (!is.null(base_data$shapefiles$yukon)) {
        p <- p +
            ggplot2::geom_sf(
                data = base_data$shapefiles$yukon,
                fill = "#F5F5DC", # Beige terrain-like color
                color = "black",
                size = viz_params$boundary$weight * 0.25,
                alpha = 0.5 # 50% transparent background
            )
    }

    # Add SWE basins
    p <- p +
        ggplot2::geom_sf(
            data = data$swe_at_basins,
            fill = data$swe_at_basins$fill_color,
            color = viz_params$basins$color,
            size = viz_params$basins$weight * 0.25,
            alpha = viz_params$basins$fillOpacity
        )

    # Add Yukon boundary background (underneath everything except basins)
    if (!is.null(base_data$shapefiles$yukon)) {
        p <- p +
            ggplot2::geom_sf(
                data = base_data$shapefiles$yukon,
                color = viz_params$boundary$color,
                size = viz_params$boundary$weight * 0.25
            )
    }

    # Add roads (below stations)
    if (!is.null(base_data$shapefiles$roads)) {
        p <- p +
            ggplot2::geom_sf(
                data = base_data$shapefiles$roads,
                color = viz_params$roads$color,
                size = viz_params$roads$weight * 0.15,
                alpha = viz_params$roads$opacity
            )
    }

    # Add survey stations (discrete)
    if (nrow(data$swe_at_surveys) > 0) {
        p <- p +
            ggplot2::geom_point(
                data = data$swe_at_surveys,
                ggplot2::aes(
                    x = data$swe_at_surveys$x,
                    y = data$swe_at_surveys$y
                ),
                fill = data$swe_at_surveys$fill_color,
                color = viz_params$surveys$color,
                size = viz_params$surveys$radius / 2.5,
                shape = 21,
                stroke = viz_params$surveys$weight * 0.5
            )
    }

    # Add pillow stations (continuous)
    if (nrow(data$swe_at_pillows) > 0) {
        p <- p +
            ggplot2::geom_point(
                data = data$swe_at_pillows,
                ggplot2::aes(
                    x = data$swe_at_pillows$x,
                    y = data$swe_at_pillows$y
                ),
                fill = data$swe_at_pillows$fill_color,
                color = viz_params$pillows$color,
                size = viz_params$pillows$radius / 2.5,
                shape = 21,
                stroke = viz_params$pillows$weight * 0.5
            )
    }

    # Add communities using pre-calculated adjusted coordinates
    if (!is.null(base_data$shapefiles$communities)) {
        comm_coords <- sf::st_coordinates(base_data$shapefiles$communities)
        communities_df <- data.frame(
            x = comm_coords[, 1],
            y = comm_coords[, 2],
            name = base_data$shapefiles$communities$feature_name,
            annotation = gsub(
                "<br>",
                "\n",
                base_data$shapefiles$communities$annotation
            ),
            x_adjust = base_data$shapefiles$communities$x_adjusted,
            y_adjust = base_data$shapefiles$communities$y_adjusted
        )

        p <- p +
            ggplot2::geom_point(
                data = communities_df,
                ggplot2::aes(x = x, y = y),
                fill = "black",
                size = viz_params$communities$iconWidth / 8,
                shape = 18
            ) +
            ggplot2::geom_text(
                data = communities_df,
                ggplot2::aes(
                    x = x_adjust,
                    y = y_adjust,
                    label = annotation
                ),
                size = 2,
                fontface = "bold.italic",
                color = viz_params$communities$label$color,
                vjust = -0.5,
                hjust = 0.5,
                family = "serif"
            )
    }

    # Add basin labels using pre-calculated adjusted coordinates
    basin_labels_df <- data.frame(
        x = data$swe_at_basins$x_adjusted,
        y = data$swe_at_basins$y_adjusted,
        annotation = data$swe_at_basins$annotation
    )

    basin_labels_df$annotation <- gsub("<br>", "\n", basin_labels_df$annotation)

    p <- p +
        shadowtext::geom_shadowtext(
            data = basin_labels_df,
            ggplot2::aes(x = x, y = y, label = annotation),
            size = 2.25,
            fontface = "bold",
            color = viz_params$basins$label$color,
            bg.color = "white",
            bg.r = 0.1
        )

    # Add title and subtitle
    month_name <- c(
        "",
        "January",
        "February",
        "March",
        "April",
        "May",
        "June",
        "July",
        "August",
        "September",
        "October",
        "November",
        "December"
    )[as.numeric(month) + 1]
    # Add colormap legend for SWE (relative_swe)
    # Use complete predefined colormap for consistent legend
    bin_ranges <- viz_params$relative_bins[-length(viz_params$relative_bins)]
    bin_colors <- viz_params$relative_colors
    bin_labels <- c(
        "≥ 150%",
        "130 - 149%",
        "110 - 129%",
        "90 - 109%",
        "70 - 89%",
        "50 - 69%",
        "< 50%",
        "Snow present where historical median is zero",
        "No snow present were historical mean is zero"
    )

    # Create manual legend using annotation_custom
    swe_legend_df <- data.frame(
        bin = seq_along(bin_colors),
        color = bin_colors,
        label = bin_labels,
        stringsAsFactors = FALSE
    )

    p <- p +
        ggplot2::labs(
            title = sprintf(
                "Yukon Snow Water Equivalent - %s %s",
                month_name,
                year
            ),
            subtitle = "SWE as % of Normal | Basins (polygons) | Discrete stations (black) | Continuous stations (blue)"
        )

    # Add coordinate system
    # Calculate basin extents with 50km buffer
    basin_bbox <- sf::st_bbox(data$swe_at_basins)
    yukon_bbox <- sf::st_bbox(base_data$shapefiles$yukon)
    basin_bbox <- sf::st_bbox(c(
        xmin = min(basin_bbox["xmin"], yukon_bbox["xmin"]),
        xmax = max(basin_bbox["xmax"], yukon_bbox["xmax"]),
        ymin = min(basin_bbox["ymin"], yukon_bbox["ymin"]),
        ymax = max(basin_bbox["ymax"], yukon_bbox["ymax"])
    ))

    buffer_degrees <- 50 / 111.32 # Convert 50km to degrees (approx)

    # Create a manual legend for SWE values
    legend_df <- data.frame(
        x = rep(basin_bbox["xmax"] - 1.5 * buffer_degrees, length(bin_colors)),
        y = seq(
            from = basin_bbox["ymax"] - buffer_degrees,
            to = basin_bbox["ymax"] - buffer_degrees - 2.0,
            length.out = length(bin_colors)
        ),
        color = bin_colors,
        label = bin_labels,
        stringsAsFactors = FALSE
    )

    # Add legend squares and text
    for (i in seq_len(nrow(legend_df))) {
        p <- p +
            ggplot2::annotate(
                "rect",
                xmin = legend_df$x[i] - 0.64,
                xmax = legend_df$x[i] - 0.04,
                ymin = legend_df$y[i] - 0.08,
                ymax = legend_df$y[i] + 0.08,
                fill = legend_df$color[i],
                color = "black",
                size = 0.2
            ) +
            ggplot2::annotate(
                "text",
                x = legend_df$x[i],
                y = legend_df$y[i],
                label = legend_df$label[i],
                hjust = 0,
                size = 2,
                fontface = "bold"
            )
    }

    # Add legend title
    p <- p +
        ggplot2::annotate(
            "text",
            x = legend_df$x[1] - 0.1,
            y = legend_df$y[1] + 0.2,
            label = "% of Normal",
            hjust = 0.5,
            size = 2.5,
            fontface = "bold"
        )

    p <- p +
        ggplot2::coord_sf(
            crs = 4326,
            xlim = c(
                basin_bbox["xmin"] - buffer_degrees,
                basin_bbox["xmax"] + buffer_degrees
            ),
            ylim = c(
                basin_bbox["ymin"] - buffer_degrees,
                basin_bbox["ymax"] + buffer_degrees
            )
        )

    if (!is.null(filename)) {
        if (!grepl("\\.png$", filename, ignore.case = TRUE)) {
            filename <- paste0(filename, ".png")
        }

        cat(sprintf("Saving ggplot map to to file: %s\n", filename))

        ggplot2::ggsave(
            filename = filename,
            plot = p,
            width = width,
            height = height,
            units = "in",
            dpi = dpi
        )
    }
    return(p)
}


#' Create demo maps for snow bulletin visualization
#'
#' @param con DBI database connection object (optional)
#' @return list containing both leaflet and ggplot map objects
#' @details Demonstrates the snow bulletin mapping functionality by creating
#'   both interactive (leaflet) and static (ggplot2) maps for April and May 2025.
#' @export
#' @noRd
demo_snow_bulletin_maps <- function(con = NULL) {
    # Create database connection if not provided
    if (is.null(con)) {
        con <- AquaCache::AquaConnect(
            name = "aquacache",
            host = "10.250.12.154",
            port = 5432,
            user = "public_reader",
            password = "aquacache"
        )
        on.exit(DBI::dbDisconnect(con))
    }

    base_data <- load_base_data(con)

    leaflet_map <- leaflet_snow_bulletin_map(
        year = 2025,
        month = 4,
        base_data = base_data,
        filename = "snowbul.html"
    )

    ggplot_map <- ggplot_snow_bulletin_map(
        year = 2025,
        month = 5,
        base_data = base_data,
        filename = "snowbul.png"
    )

    return(list(
        leaflet_map = leaflet_map,
        ggplot_map = ggplot_map,
        base_data = base_data
    ))
}

#demo_snow_bulletin_maps()
