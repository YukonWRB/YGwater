create_aquacache_connection <- function() {
    YGwater::AquaConnect(
        name = "aquacache",
        host = Sys.getenv("aquacacheHostProd"),
        port = Sys.getenv("aquacachePortProd"),
        user = Sys.getenv("aquacacheAdminUser"),
        password = Sys.getenv("aquacacheAdminPass")
    )
}

load_target_gauge_names <- function(
    community = "Dawson",
    yaml_path = file.path(
        "dev",
        "freshet_forecasting",
        "flood_vulnerable_gauges.yaml"
    )
) {
    targets <- yaml::read_yaml(yaml_path)

    if (!(community %in% names(targets))) {
        stop(sprintf("Community '%s' not found in %s", community, yaml_path))
    }

    gauge_names <- unique(stats::na.omit(unlist(
        targets[[community]],
        use.names = FALSE
    )))

    if (length(gauge_names) == 0) {
        stop(sprintf("No target gauges found for community '%s'", community))
    }

    gauge_names
}

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
            paste(
                "SELECT name, location_code, location_id, latitude, longitude",
                "FROM locations",
                "WHERE name IN (%s)",
                "ORDER BY name"
            ),
            location_names_sql
        ),
        con = con
    )
}

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
        paste(
            "SELECT *, ST_AsText(ST_Transform(geom, %d)) AS geom_wkt",
            "FROM spatial.vectors",
            "WHERE %s"
        ),
        epsg,
        paste(where_clauses, collapse = " AND ")
    )

    sf::st_as_sf(
        YGwater::dbGetQueryDT(query, con = con),
        wkt = "geom_wkt",
        crs = epsg
    )
}

get_monitoring_locations_as_sf <- function(con) {
    location_codes_with_params <- YGwater::dbGetQueryDT(
        paste(
            "WITH location_params AS (",
            "    SELECT l.location_code, l.name, l.latitude, l.longitude, p.param_name, 'continuous' AS data_source",
            "    FROM locations l",
            "    JOIN timeseries ts ON l.location_id = ts.location_id",
            "    JOIN parameters p ON p.parameter_id = ts.parameter_id",
            "    JOIN measurements_continuous mc ON mc.timeseries_id = ts.timeseries_id",
            "    WHERE mc.value IS NOT NULL",
            "      AND mc.datetime >= NOW() - INTERVAL '14 days'",
            "    UNION ALL",
            "    SELECT l.location_code, l.name, l.latitude, l.longitude, p.param_name, 'continuous' AS data_source",
            "    FROM locations l",
            "    JOIN timeseries ts ON l.location_id = ts.location_id",
            "    JOIN parameters p ON p.parameter_id = ts.parameter_id",
            "    JOIN measurements_calculated_daily mcd ON mcd.timeseries_id = ts.timeseries_id",
            "    WHERE mcd.value IS NOT NULL",
            "      AND mcd.date >= CURRENT_DATE - INTERVAL '14 days'",
            "    UNION ALL",
            "    SELECT l.location_code, l.name, l.latitude, l.longitude, p.param_name, 'survey' AS data_source",
            "    FROM locations l",
            "    JOIN samples s ON l.location_id = s.location_id",
            "    JOIN results r ON r.sample_id = s.sample_id",
            "    JOIN parameters p ON p.parameter_id = r.parameter_id",
            ")",
            "SELECT DISTINCT location_code, name, latitude, longitude",
            "FROM location_params",
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

    stations_by_basin <- stations_by_basin[
        order(
            stations_by_basin$location_code,
            stations_by_basin$basin_area_km2,
            na.last = TRUE
        ),
    ]

    stations_by_basin[!duplicated(stations_by_basin$location_code), ]
}

load_target_gauge_context <- function(
    community = "Dawson",
    target_gauge_name = NULL,
    con = create_aquacache_connection()
) {
    target_gauge_names <- load_target_gauge_names(community = community)
    target_gauges <- location_names_to_codes(target_gauge_names, con = con)

    if (nrow(target_gauges) == 0) {
        stop(sprintf(
            "No target gauge metadata found for community '%s'",
            community
        ))
    }

    target_basins <- get_spatial_layer_as_sf(
        layer_name = "Drainage basins",
        feature_name = target_gauges$location_code,
        con = con
    )
    target_basins$basin_area_km2 <- as.numeric(
        sf::st_area(sf::st_transform(target_basins, 3578))
    ) /
        1e6

    monitoring_locations <- get_monitoring_locations_as_sf(con = con)

    if (is.null(target_gauge_name)) {
        target_gauge_name <- target_gauges$name[[1]]
    }

    target_index <- match(target_gauge_name, target_gauges$name)
    if (is.na(target_index)) {
        stop(sprintf(
            "Target gauge '%s' not found in community '%s'",
            target_gauge_name,
            community
        ))
    }

    target_gauge <- target_gauges[target_index, , drop = FALSE]
    target_gauge_sf <- sf::st_as_sf(
        target_gauge,
        coords = c("longitude", "latitude"),
        crs = 4326,
        remove = FALSE
    )

    target_basin <- target_basins[
        target_basins$feature_name == target_gauge$location_code[[1]],
        ,
        drop = FALSE
    ]
    if (nrow(target_basin) == 0) {
        stop(sprintf(
            "No drainage basin found for target gauge code '%s'",
            target_gauge$location_code[[1]]
        ))
    }

    gauges_in_target_basin <- get_stations_by_basin(
        location_codes = target_gauge$location_code,
        poly = target_basin,
        monitoring_stations = monitoring_locations
    )

    list(
        community = community,
        con = con,
        target_gauges = target_gauges,
        target_basins = target_basins,
        target_gauge = target_gauge,
        target_gauge_sf = target_gauge_sf,
        target_basin = target_basin,
        gauges_in_target_basin = gauges_in_target_basin
    )
}

plot_target_gauge_drainage_area <- function(target_context) {
    target_gauge <- target_context$target_gauge
    target_gauge_sf <- target_context$target_gauge_sf
    target_basin <- target_context$target_basin
    gauges_in_target_basin <- target_context$gauges_in_target_basin

    gauges_in_target_basin$is_target_gauge <-
        gauges_in_target_basin$location_code %in% target_gauge$location_code
    basin_area_km2 <- round(target_basin$basin_area_km2[[1]], 1)

    ggplot2::ggplot() +
        ggplot2::geom_sf(
            data = target_basin,
            fill = "#bfdbfe",
            color = "#1d4ed8",
            linewidth = 0.5,
            alpha = 0.45
        ) +
        ggplot2::geom_sf(
            data = gauges_in_target_basin,
            color = "#475569",
            size = 2.2,
            alpha = 0.8
        ) +
        ggplot2::geom_sf_text(
            data = gauges_in_target_basin,
            ggplot2::aes(label = rlang::.data$location_code),
            size = 3,
            check_overlap = TRUE,
            nudge_y = 0.05
        ) +
        ggplot2::geom_sf(
            data = target_gauge_sf,
            color = "#b91c1c",
            fill = "#fbbf24",
            shape = 21,
            size = 4,
            stroke = 1.1
        ) +
        ggplot2::geom_sf_text(
            data = target_gauge_sf,
            ggplot2::aes(label = rlang::.data$name),
            color = "#991b1b",
            fontface = "bold",
            size = 3.2,
            nudge_y = 0.1
        ) +
        ggplot2::labs(
            title = sprintf(
                "%s drainage area and gauges within basin",
                target_gauge$name[[1]]
            ),
            subtitle = sprintf(
                "Community: %s | Drainage area: %s km2 | Gauges in basin: %d",
                target_context$community,
                format(basin_area_km2, big.mark = ",", trim = TRUE),
                nrow(gauges_in_target_basin)
            ),
            x = NULL,
            y = NULL
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
            panel.grid.major = ggplot2::element_line(color = "#cbd5e1"),
            axis.text = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank()
        )
}


target_gauge_context <- load_target_gauge_context(
    community = "Dawson",
    target_gauge_name = "Yukon River At Dawson"
)

target_gauges <- target_gauge_context$target_gauges
target_gauge_drainage_areas <- sf::st_drop_geometry(
    target_gauge_context$target_basins[, c("feature_name", "basin_area_km2")]
)
dawson_target_gauge <- target_gauge_context$target_gauge
dawson_target_basin <- target_gauge_context$target_basin
gauges_in_dawson_basin <- target_gauge_context$gauges_in_target_basin

dawson_drainage_area_plot <- plot_target_gauge_drainage_area(
    target_gauge_context
)

print(dawson_drainage_area_plot)
