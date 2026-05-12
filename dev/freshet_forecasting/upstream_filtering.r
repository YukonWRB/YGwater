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

load_all_target_gauges <- function(
    yaml_path = file.path(
        "dev",
        "freshet_forecasting",
        "flood_vulnerable_gauges.yaml"
    ),
    con
) {
    targets <- yaml::read_yaml(yaml_path)

    target_lookup <- data.table::rbindlist(
        lapply(names(targets), function(community_name) {
            gauge_names <- unique(stats::na.omit(unlist(
                targets[[community_name]],
                use.names = FALSE
            )))

            if (length(gauge_names) == 0) {
                return(NULL)
            }

            data.table::data.table(
                community = community_name,
                name = gauge_names
            )
        }),
        use.names = TRUE,
        fill = TRUE
    )

    if (nrow(target_lookup) == 0) {
        stop(sprintf("No target gauges found in %s", yaml_path))
    }

    target_lookup <- unique(target_lookup)
    target_gauges <- location_names_to_codes(target_lookup$name, con = con)

    merge(
        target_lookup,
        target_gauges,
        by = "name",
        all.x = TRUE,
        sort = TRUE
    )
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
            "    SELECT l.location_id, l.location_code, l.name, l.latitude, l.longitude, p.parameter_id, p.param_name, 'continuous' AS data_source",
            "    FROM locations l",
            "    JOIN timeseries ts ON l.location_id = ts.location_id",
            "    JOIN parameters p ON p.parameter_id = ts.parameter_id",
            "    JOIN measurements_continuous mc ON mc.timeseries_id = ts.timeseries_id",
            "    WHERE mc.value IS NOT NULL",
            "      AND mc.datetime >= NOW() - INTERVAL '14 days'",
            "    UNION ALL",
            "    SELECT l.location_id, l.location_code, l.name, l.latitude, l.longitude, p.parameter_id, p.param_name, 'continuous' AS data_source",
            "    FROM locations l",
            "    JOIN timeseries ts ON l.location_id = ts.location_id",
            "    JOIN parameters p ON p.parameter_id = ts.parameter_id",
            "    JOIN measurements_calculated_daily mcd ON mcd.timeseries_id = ts.timeseries_id",
            "    WHERE mcd.value IS NOT NULL",
            "      AND mcd.date >= CURRENT_DATE - INTERVAL '14 days'",
            "    UNION ALL",
            "    SELECT l.location_id, l.location_code, l.name, l.latitude, l.longitude, p.parameter_id, p.param_name, 'survey' AS data_source",
            "    FROM locations l",
            "    JOIN samples s ON l.location_id = s.location_id",
            "    JOIN results r ON r.sample_id = s.sample_id",
            "    JOIN parameters p ON p.parameter_id = r.parameter_id",
            ")",
            "SELECT DISTINCT location_id, location_code, name, latitude, longitude",
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

get_monitoring_location_parameter_ids <- function(con) {
    YGwater::dbGetQueryDT(
        paste(
            "WITH location_params AS (",
            "    SELECT l.location_id, l.location_code, p.parameter_id",
            "    FROM locations l",
            "    JOIN timeseries ts ON l.location_id = ts.location_id",
            "    JOIN parameters p ON p.parameter_id = ts.parameter_id",
            "    JOIN measurements_continuous mc ON mc.timeseries_id = ts.timeseries_id",
            "    WHERE mc.value IS NOT NULL",
            "      AND mc.datetime >= NOW() - INTERVAL '14 days'",
            "    UNION ALL",
            "    SELECT l.location_id, l.location_code, p.parameter_id",
            "    FROM locations l",
            "    JOIN timeseries ts ON l.location_id = ts.location_id",
            "    JOIN parameters p ON p.parameter_id = ts.parameter_id",
            "    JOIN measurements_calculated_daily mcd ON mcd.timeseries_id = ts.timeseries_id",
            "    WHERE mcd.value IS NOT NULL",
            "      AND mcd.date >= CURRENT_DATE - INTERVAL '14 days'",
            "    UNION ALL",
            "    SELECT l.location_id, l.location_code, p.parameter_id",
            "    FROM locations l",
            "    JOIN samples s ON l.location_id = s.location_id",
            "    JOIN results r ON r.sample_id = s.sample_id",
            "    JOIN parameters p ON p.parameter_id = r.parameter_id",
            ")",
            "SELECT DISTINCT location_id, location_code, parameter_id",
            "FROM location_params",
            "ORDER BY location_code, parameter_id"
        ),
        con = con
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

get_locations_within_polygon <- function(locations, poly) {
    if (nrow(poly) == 0 || nrow(locations) == 0) {
        return(locations[0, ])
    }

    if (sf::st_crs(locations) != sf::st_crs(poly)) {
        poly <- sf::st_transform(poly, sf::st_crs(locations))
    }

    locations_in_polygon <- sf::st_join(
        locations,
        poly[, intersect(names(poly), c("feature_name", "layer_name"))],
        join = sf::st_intersects,
        left = FALSE
    )

    dedupe_col <- intersect(
        c("location_code", "location_id", "feature_name", "name"),
        names(locations_in_polygon)
    )

    if (length(dedupe_col) == 0) {
        return(locations_in_polygon)
    }

    locations_in_polygon[
        !duplicated(locations_in_polygon[[dedupe_col[[1]]]]),
    ]
}

merge_basin_polygons <- function(basins) {
    if (nrow(basins) == 0) {
        return(basins[0, , drop = FALSE])
    }

    sf::st_as_sf(
        data.frame(geometry = sf::st_union(sf::st_geometry(basins))),
        crs = sf::st_crs(basins)
    )
}

empty_lines_sf <- function(template_data, crs) {
    sf::st_sf(
        template_data,
        geometry = sf::st_sfc(crs = crs)
    )
}

build_target_basin_hierarchy <- function(target_gauges, target_basins) {
    if (nrow(target_gauges) == 0 || nrow(target_basins) == 0) {
        return(data.frame())
    }

    has_required_overlap <- function(
        source_geom,
        candidate_geom,
        min_overlap_ratio = 0.95
    ) {
        source_geom_projected <- sf::st_transform(source_geom, 3578)
        candidate_geom_projected <- sf::st_transform(candidate_geom, 3578)
        source_area_m2 <- sum(as.numeric(sf::st_area(source_geom_projected)))

        if (!is.finite(source_area_m2) || source_area_m2 <= 0) {
            return(FALSE)
        }

        intersection_geom <- suppressWarnings(tryCatch(
            sf::st_intersection(
                source_geom_projected,
                candidate_geom_projected
            ),
            error = function(e) NULL
        ))

        if (is.null(intersection_geom) || nrow(intersection_geom) == 0) {
            return(FALSE)
        }

        overlap_area_m2 <- sum(as.numeric(sf::st_area(intersection_geom)))
        overlap_ratio <- overlap_area_m2 / source_area_m2

        is.finite(overlap_ratio) && overlap_ratio >= min_overlap_ratio
    }

    basin_lookup <- merge(
        sf::st_drop_geometry(target_basins[, c(
            "feature_name",
            "basin_area_km2"
        )]),
        sf::st_drop_geometry(target_gauges[, c(
            "location_code",
            "name",
            "location_id"
        )]),
        by.x = "feature_name",
        by.y = "location_code",
        all.x = TRUE,
        sort = FALSE
    )

    basin_lookup <- basin_lookup[
        order(basin_lookup$basin_area_km2),
        ,
        drop = FALSE
    ]
    hierarchy_rows <- vector("list", nrow(basin_lookup))

    for (row_index in seq_len(nrow(basin_lookup))) {
        source_code <- basin_lookup$feature_name[[row_index]]
        source_area <- basin_lookup$basin_area_km2[[row_index]]
        source_geom <- target_basins[
            target_basins$feature_name == source_code,
        ]

        candidate_lookup <- basin_lookup[
            basin_lookup$basin_area_km2 > source_area,
            ,
            drop = FALSE
        ]

        if (nrow(candidate_lookup) == 0) {
            next
        }

        overlapping_candidates <- candidate_lookup[
            vapply(
                candidate_lookup$feature_name,
                function(candidate_code) {
                    candidate_geom <- target_basins[
                        target_basins$feature_name == candidate_code,
                        ,
                        drop = FALSE
                    ]

                    has_required_overlap(
                        source_geom = source_geom,
                        candidate_geom = candidate_geom
                    )
                },
                logical(1)
            ),
            ,
            drop = FALSE
        ]

        if (nrow(overlapping_candidates) == 0) {
            next
        }

        downstream_row <- overlapping_candidates[
            which.min(overlapping_candidates$basin_area_km2),
            ,
            drop = FALSE
        ]

        hierarchy_rows[[row_index]] <- data.frame(
            from_location_code = source_code,
            from_location_id = basin_lookup$location_id[[row_index]],
            from_name = basin_lookup$name[[row_index]],
            from_basin_area_km2 = source_area,
            to_location_code = downstream_row$feature_name[[1]],
            to_location_id = downstream_row$location_id[[1]],
            to_name = downstream_row$name[[1]],
            to_basin_area_km2 = downstream_row$basin_area_km2[[1]],
            stringsAsFactors = FALSE
        )
    }

    hierarchy_rows <- Filter(Negate(is.null), hierarchy_rows)
    if (length(hierarchy_rows) == 0) {
        return(data.frame())
    }

    do.call(rbind, hierarchy_rows)
}

build_target_gauge_routing_lines <- function(
    hierarchy_table,
    target_gauges_sf
) {
    if (nrow(hierarchy_table) == 0 || nrow(target_gauges_sf) == 0) {
        return(empty_lines_sf(
            hierarchy_table[0, , drop = FALSE],
            sf::st_crs(target_gauges_sf)
        ))
    }

    line_geometries <- lapply(
        seq_len(nrow(hierarchy_table)),
        function(row_index) {
            route_row <- hierarchy_table[row_index, , drop = FALSE]
            from_point <- target_gauges_sf[
                target_gauges_sf$location_code ==
                    route_row$from_location_code[[1]],
                ,
                drop = FALSE
            ]
            to_point <- target_gauges_sf[
                target_gauges_sf$location_code ==
                    route_row$to_location_code[[1]],
                ,
                drop = FALSE
            ]

            if (nrow(from_point) == 0 || nrow(to_point) == 0) {
                return(NULL)
            }

            from_coords <- sf::st_coordinates(from_point)[1, c("X", "Y")]
            to_coords <- sf::st_coordinates(to_point)[1, c("X", "Y")]

            sf::st_linestring(rbind(from_coords, to_coords))
        }
    )

    valid_index <- !vapply(line_geometries, is.null, logical(1))
    if (!any(valid_index)) {
        return(empty_lines_sf(
            hierarchy_table[0, , drop = FALSE],
            sf::st_crs(target_gauges_sf)
        ))
    }

    sf::st_as_sf(
        hierarchy_table[valid_index, , drop = FALSE],
        geometry = sf::st_sfc(
            line_geometries[valid_index],
            crs = sf::st_crs(target_gauges_sf)
        )
    )
}

build_target_gauge_routing_segments <- function(
    hierarchy_table,
    target_gauges_sf
) {
    empty_segment_table <- data.frame(
        from_location_code = character(),
        to_location_code = character(),
        x = numeric(),
        y = numeric(),
        xend = numeric(),
        yend = numeric(),
        stringsAsFactors = FALSE
    )

    if (nrow(hierarchy_table) == 0 || nrow(target_gauges_sf) == 0) {
        return(empty_segment_table)
    }

    route_rows <- lapply(seq_len(nrow(hierarchy_table)), function(row_index) {
        route_row <- hierarchy_table[row_index, , drop = FALSE]
        from_point <- target_gauges_sf[
            target_gauges_sf$location_code == route_row$from_location_code[[1]],
            ,
            drop = FALSE
        ]
        to_point <- target_gauges_sf[
            target_gauges_sf$location_code == route_row$to_location_code[[1]],
            ,
            drop = FALSE
        ]

        if (nrow(from_point) == 0 || nrow(to_point) == 0) {
            return(NULL)
        }

        from_coords <- sf::st_coordinates(from_point)[1, c("X", "Y")]
        to_coords <- sf::st_coordinates(to_point)[1, c("X", "Y")]

        data.frame(
            from_location_code = route_row$from_location_code[[1]],
            to_location_code = route_row$to_location_code[[1]],
            x = from_coords[["X"]],
            y = from_coords[["Y"]],
            xend = to_coords[["X"]],
            yend = to_coords[["Y"]],
            stringsAsFactors = FALSE
        )
    })

    route_rows <- Filter(Negate(is.null), route_rows)
    if (length(route_rows) == 0) {
        return(empty_segment_table)
    }

    do.call(rbind, route_rows)
}

build_target_gauge_route_table <- function(
    hierarchy_table,
    target_gauges_sf
) {
    if (nrow(target_gauges_sf) == 0) {
        return(data.frame())
    }

    route_table <- sf::st_drop_geometry(target_gauges_sf[, c(
        "location_code",
        "location_id",
        "name"
    )])
    route_table$route_to_location_id <- NA_integer_
    route_table$route_to_location_code <- NA_character_
    route_table$route_to_name <- NA_character_

    if (nrow(hierarchy_table) == 0) {
        return(route_table)
    }

    route_match <- match(
        route_table$location_code,
        hierarchy_table$from_location_code
    )
    matched_rows <- !is.na(route_match)

    route_table$route_to_location_id[matched_rows] <-
        hierarchy_table$to_location_id[route_match[matched_rows]]
    route_table$route_to_location_code[matched_rows] <-
        hierarchy_table$to_location_code[route_match[matched_rows]]
    route_table$route_to_name[matched_rows] <-
        hierarchy_table$to_name[route_match[matched_rows]]

    route_table
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
    all_target_gauges <- load_all_target_gauges(con = con)
    all_target_basins <- get_spatial_layer_as_sf(
        layer_name = "Drainage basins",
        feature_name = all_target_gauges$location_code,
        con = con
    )
    all_target_gauges_sf <- sf::st_as_sf(
        all_target_gauges,
        coords = c("longitude", "latitude"),
        crs = 4326,
        remove = FALSE
    )

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
    target_gauges_sf <- sf::st_as_sf(
        target_gauges,
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

    other_community_target_gauges <- all_target_gauges_sf[
        all_target_gauges_sf$community != community &
            !(all_target_gauges_sf$location_code %in%
                target_gauge$location_code),
        ,
        drop = FALSE
    ]
    other_community_target_gauges_in_basin <- get_locations_within_polygon(
        locations = other_community_target_gauges,
        poly = target_basin
    )
    other_community_target_basins_in_basin <- all_target_basins[
        all_target_basins$feature_name %in%
            other_community_target_gauges_in_basin$location_code,
        ,
        drop = FALSE
    ]
    merged_other_target_basin <- merge_basin_polygons(
        other_community_target_basins_in_basin
    )

    gauges_in_target_basin$station_status <- 1L
    gauges_in_target_basin$station_status[
        gauges_in_target_basin$location_code %in% target_gauge$location_code
    ] <- 0L

    non_target_gauge_index <- gauges_in_target_basin$station_status != 0L
    if (any(non_target_gauge_index) && nrow(merged_other_target_basin) > 0) {
        upstream_basin_for_join <- merged_other_target_basin
        if (
            sf::st_crs(gauges_in_target_basin) !=
                sf::st_crs(upstream_basin_for_join)
        ) {
            upstream_basin_for_join <- sf::st_transform(
                upstream_basin_for_join,
                sf::st_crs(gauges_in_target_basin)
            )
        }

        gauges_in_target_basin$station_status[non_target_gauge_index] <- ifelse(
            lengths(sf::st_intersects(
                gauges_in_target_basin[non_target_gauge_index, , drop = FALSE],
                upstream_basin_for_join
            )) >
                0,
            2L,
            1L
        )
    }

    station_status_table <- sf::st_drop_geometry(
        gauges_in_target_basin[, c(
            "name",
            "location_code",
            "location_id",
            "station_status"
        )]
    )

    gauges_with_basins_in_target_basin <- get_locations_within_polygon(
        locations = gauges_in_target_basin,
        poly = target_basin
    )
    gauge_basins_in_target_basin <- get_spatial_layer_as_sf(
        layer_name = "Drainage basins",
        feature_name = gauges_with_basins_in_target_basin$location_code,
        con = con
    )
    gauge_basins_in_target_basin$basin_area_km2 <- as.numeric(
        sf::st_area(sf::st_transform(gauge_basins_in_target_basin, 3578))
    ) /
        1e6
    target_basin_hierarchy <- build_target_basin_hierarchy(
        target_gauges = gauges_with_basins_in_target_basin,
        target_basins = gauge_basins_in_target_basin
    )
    target_gauge_routing <- build_target_gauge_routing_lines(
        hierarchy_table = target_basin_hierarchy,
        target_gauges_sf = gauges_with_basins_in_target_basin
    )
    target_gauge_routing_segments <- build_target_gauge_routing_segments(
        hierarchy_table = target_basin_hierarchy,
        target_gauges_sf = gauges_with_basins_in_target_basin
    )
    target_gauge_route_table <- build_target_gauge_route_table(
        hierarchy_table = target_basin_hierarchy,
        target_gauges_sf = gauges_with_basins_in_target_basin
    )
    waterbodies <- suppressWarnings(sf::st_intersection(
        get_spatial_layer_as_sf(
            layer_name = "Waterbodies",
            con = con
        ),
        target_basin
    ))
    communities <- get_locations_within_polygon(
        locations = get_spatial_layer_as_sf(
            layer_name = "Communities",
            con = con
        ),
        poly = target_basin
    )
    community_label_col <- intersect(
        c("feature_name", "feature_name.x", "feature_name.y", "name"),
        names(communities)
    )
    if (length(community_label_col) > 0) {
        communities$community_name <- communities[[community_label_col[[1]]]]
    }

    list(
        community = community,
        con = con,
        target_gauges = target_gauges,
        target_gauges_sf = target_gauges_sf,
        all_target_gauges = all_target_gauges,
        all_target_basins = all_target_basins,
        target_basins = target_basins,
        target_gauge = target_gauge,
        target_gauge_sf = target_gauge_sf,
        target_basin = target_basin,
        gauges_in_target_basin = gauges_in_target_basin,
        station_status_table = station_status_table,
        gauges_with_basins_in_target_basin = gauges_with_basins_in_target_basin,
        gauge_basins_in_target_basin = gauge_basins_in_target_basin,
        target_basin_hierarchy = target_basin_hierarchy,
        target_gauge_routing = target_gauge_routing,
        target_gauge_routing_segments = target_gauge_routing_segments,
        target_gauge_route_table = target_gauge_route_table,
        waterbodies = waterbodies,
        communities = communities,
        other_community_target_gauges_in_basin = other_community_target_gauges_in_basin,
        merged_other_target_basin = merged_other_target_basin
    )
}

plot_target_gauge_routing <- function(target_context) {
    target_basin <- target_context$target_basin
    target_gauge_sf <- target_context$target_gauge_sf
    gauges_with_basins_in_target_basin <-
        target_context$gauges_with_basins_in_target_basin
    gauge_basins_in_target_basin <-
        target_context$gauge_basins_in_target_basin
    target_gauge_routing <- target_context$target_gauge_routing
    target_gauge_routing_segments <-
        target_context$target_gauge_routing_segments
    waterbodies <- target_context$waterbodies
    communities <- target_context$communities

    upstream_target_gauges <- gauges_with_basins_in_target_basin[
        !(gauges_with_basins_in_target_basin$location_code %in%
            target_context$target_gauge$location_code),
        ,
        drop = FALSE
    ]

    ggplot2::ggplot() +
        ggplot2::geom_sf(
            data = target_basin,
            fill = "#dbeafe",
            color = "#1d4ed8",
            linewidth = 0.6,
            alpha = 0.35
        ) +
        ggplot2::geom_sf(
            data = waterbodies,
            fill = "lightblue",
            color = "lightblue",
            alpha = 0.5
        ) +
        ggplot2::geom_sf(
            data = gauge_basins_in_target_basin,
            fill = NA,
            color = "#94a3b8",
            linewidth = 0.5,
            linetype = "dashed"
        ) +
        ggplot2::geom_sf(
            data = target_gauge_routing,
            color = "#7c3aed",
            linewidth = 0.5,
            alpha = 0.5
        ) +
        ggplot2::geom_segment(
            data = target_gauge_routing_segments,
            ggplot2::aes(
                x = .data[["x"]],
                y = .data[["y"]],
                xend = .data[["xend"]],
                yend = .data[["yend"]]
            ),
            color = "#7c3aed",
            linewidth = 0.45,
            alpha = 0.7,
            arrow = grid::arrow(
                length = grid::unit(0.14, "inches"),
                type = "closed"
            )
        ) +
        ggplot2::geom_sf(
            data = upstream_target_gauges,
            color = "#0f766e",
            fill = "#5eead4",
            shape = 21,
            size = 3.2,
            stroke = 1
        ) +
        ggplot2::geom_sf(
            data = target_gauge_sf,
            color = "#b91c1c",
            fill = "#fbbf24",
            shape = 21,
            size = 4,
            stroke = 1.1
        ) +
        ggplot2::geom_sf(
            data = communities,
            color = "#111827",
            fill = "#111827",
            shape = 23,
            size = 2.5,
            stroke = 0.4
        ) +
        ggplot2::geom_sf_text(
            data = communities,
            ggplot2::aes(label = .data[["community_name"]]),
            color = "#111827",
            size = 2.8,
            fontface = "bold",
            check_overlap = TRUE
        ) +
        ggplot2::geom_sf_text(
            data = target_gauge_sf,
            ggplot2::aes(label = .data[["name"]]),
            color = "#991b1b",
            fontface = "bold",
            size = 3.2
        ) +
        ggplot2::labs(
            title = sprintf(
                "%s approximate upstream routing",
                target_context$target_gauge$name[[1]]
            ),
            subtitle = sprintf(
                "Community: %s | Routed target gauges: %d",
                target_context$community,
                nrow(target_gauge_routing)
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

plot_target_gauge_drainage_area <- function(target_context) {
    target_gauge <- target_context$target_gauge
    target_gauge_sf <- target_context$target_gauge_sf
    target_basin <- target_context$target_basin
    gauges_in_target_basin <- target_context$gauges_in_target_basin
    other_community_target_gauges_in_basin <-
        target_context$other_community_target_gauges_in_basin
    merged_other_target_basin <- target_context$merged_other_target_basin

    non_target_gauges_in_target_basin <- gauges_in_target_basin[
        gauges_in_target_basin$station_status != 0L,
        ,
        drop = FALSE
    ]

    gauges_only_in_target_basin <- non_target_gauges_in_target_basin[
        non_target_gauges_in_target_basin$station_status == 1L,
        ,
        drop = FALSE
    ]
    gauges_in_both_basins <- non_target_gauges_in_target_basin[
        non_target_gauges_in_target_basin$station_status == 2L,
        ,
        drop = FALSE
    ]
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
            data = merged_other_target_basin,
            fill = NA,
            color = "#b91c1c",
            linewidth = 0.8
        ) +
        ggplot2::geom_sf(
            data = gauges_only_in_target_basin,
            color = "#1d4ed8",
            fill = "#60a5fa",
            shape = 21,
            size = 2.8,
            stroke = 0.9,
            alpha = 0.9
        ) +
        ggplot2::geom_sf(
            data = gauges_in_both_basins,
            color = "#b91c1c",
            fill = "#f87171",
            shape = 21,
            size = 2.8,
            stroke = 0.9,
            alpha = 0.9
        ) +
        ggplot2::geom_sf_text(
            data = non_target_gauges_in_target_basin,
            ggplot2::aes(label = .data[["location_code"]]),
            size = 3,
            check_overlap = TRUE
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
            ggplot2::aes(label = .data[["name"]]),
            color = "#991b1b",
            fontface = "bold",
            size = 3.2
        ) +
        ggplot2::labs(
            title = sprintf(
                "%s drainage area and gauges within basin",
                target_gauge$name[[1]]
            ),
            subtitle = sprintf(
                paste(
                    "Community: %s | Drainage area: %s km2 | Gauges in basin: %d |",
                    "Other community target gauges in basin: %d"
                ),
                target_context$community,
                format(basin_area_km2, big.mark = ",", trim = TRUE),
                nrow(gauges_in_target_basin),
                nrow(other_community_target_gauges_in_basin)
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

build_target_gauge_station_export <- function(
    target_context,
    location_parameter_ids
) {
    station_status_table <- target_context$station_status_table
    route_table <- target_context$target_gauge_route_table
    target_gauges <- target_context$target_gauges
    target_gauge <- target_context$target_gauge

    if (is.null(station_status_table) || nrow(station_status_table) == 0) {
        station_status_table <- data.frame(
            name = character(),
            location_code = character(),
            location_id = integer(),
            station_status = integer(),
            stringsAsFactors = FALSE
        )
    }

    # Always include the active target gauge and always force its encoding to 0.
    # This prevents the target from being dropped by monitoring-data filters.
    if (!is.null(target_gauge) && nrow(target_gauge) > 0) {
        target_rows <- data.frame(
            name = as.character(target_gauge$name),
            location_code = as.character(target_gauge$location_code),
            location_id = as.integer(target_gauge$location_id),
            station_status = 0L,
            stringsAsFactors = FALSE
        )

        target_keys <- paste0(
            target_rows$location_code,
            "::",
            target_rows$location_id
        )
        station_keys <- paste0(
            as.character(station_status_table$location_code),
            "::",
            as.integer(station_status_table$location_id)
        )

        station_status_table <- station_status_table[
            !(station_keys %in% target_keys),
            ,
            drop = FALSE
        ]
        station_status_table <- unique(rbind(station_status_table, target_rows))
    }

    lapply(seq_len(nrow(station_status_table)), function(row_index) {
        station_row <- station_status_table[row_index, , drop = FALSE]
        station_location_code <- station_row$location_code[[1]]
        station_location_id <- station_row$location_id[[1]]

        station_location_name <- as.character(station_row$name[[1]])
        if (is.na(station_location_name) || !nzchar(station_location_name)) {
            station_location_name <- target_gauges[
                target_gauges$location_code == station_location_code &
                    target_gauges$location_id == station_location_id,
                "name"
            ][[1]]
        }
        if (is.na(station_location_name) || !nzchar(station_location_name)) {
            station_location_name <- station_location_code
        }

        station_parameters <- location_parameter_ids[
            location_parameter_ids$location_code == station_location_code &
                location_parameter_ids$location_id == station_location_id,
        ]
        station_parameter_ids <- sort(unique(as.integer(unlist(
            station_parameters$parameter_id,
            use.names = FALSE
        ))))

        route_row <- route_table[
            route_table$location_code == station_location_code &
                route_table$location_id == station_location_id,
            ,
            drop = FALSE
        ]

        route_to_location_id <- NULL
        route_to_location_code <- NULL
        route_to_name <- NULL

        if (
            nrow(route_row) > 0 && !is.na(route_row$route_to_location_id[[1]])
        ) {
            route_to_location_id <- jsonlite::unbox(as.integer(
                route_row$route_to_location_id[[1]]
            ))
            route_to_location_code <- jsonlite::unbox(
                route_row$route_to_location_code[[1]]
            )
            route_to_name <- jsonlite::unbox(route_row$route_to_name[[1]])
        }

        list(
            location_code = jsonlite::unbox(station_location_code),
            location_id = jsonlite::unbox(as.integer(station_location_id)),
            location_name = jsonlite::unbox(station_location_name),
            parameter_ids = unname(station_parameter_ids),
            encoding = jsonlite::unbox(as.integer(station_row$station_status[[
                1
            ]])),
            route_to_location_id = route_to_location_id,
            route_to_location_code = route_to_location_code,
            route_to_name = route_to_name
        )
    })
}

read_target_gauge_station_encoding_json <- function(
    input_path = NULL,
    simplify_vector = FALSE
) {
    candidate_paths <- unique(stats::na.omit(c(
        input_path,
        file.path(
            "inst",
            "data-raw",
            "flood_vulnerable_gauges_encoded.json"
        ),
        file.path(
            "data-raw",
            "flood_vulnerable_gauges_encoded.json"
        ),
        file.path(
            "inst",
            "extdata",
            "flood_vulnerable_gauges_encoded.json"
        ),
        file.path(
            "dev",
            "freshet_forecasting",
            "flood_vulnerable_gauges_encoding.json"
        ),
        file.path(
            "dev",
            "freshet_forecasting",
            "flood_vulnerable_gauges_encoded.json"
        )
    )))

    existing_path <- candidate_paths[file.exists(candidate_paths)][[1]]

    if (is.null(existing_path) || is.na(existing_path)) {
        stop(sprintf(
            "No encoded gauge JSON file found. Checked: %s",
            paste(candidate_paths, collapse = ", ")
        ))
    }

    jsonlite::fromJSON(
        existing_path,
        simplifyVector = simplify_vector
    )
}

write_target_gauge_station_encoding_json <- function(
    output_path = file.path(
        "inst",
        "extdata",
        "flood_vulnerable_gauges_encoded.json"
    ),
    yaml_path = file.path(
        "dev",
        "freshet_forecasting",
        "flood_vulnerable_gauges.yaml"
    ),
    con = create_aquacache_connection()
) {
    targets <- yaml::read_yaml(yaml_path)
    location_parameter_ids <- get_monitoring_location_parameter_ids(con = con)
    export_payload <- list()

    for (community_name in names(targets)) {
        community_targets <- load_target_gauge_names(
            community = community_name,
            yaml_path = yaml_path
        )
        community_payload <- list()

        for (target_gauge_name in community_targets) {
            target_context <- load_target_gauge_context(
                community = community_name,
                target_gauge_name = target_gauge_name,
                con = con
            )
            community_payload[[target_gauge_name]] <-
                build_target_gauge_station_export(
                    target_context = target_context,
                    location_parameter_ids = location_parameter_ids
                )
        }

        export_payload[[community_name]] <- community_payload
    }

    json_payload <- jsonlite::toJSON(
        export_payload,
        pretty = TRUE,
        auto_unbox = FALSE,
        null = "null"
    )
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(json_payload, con = file(output_path, encoding = "UTF-8"))

    invisible(export_payload)
}


target_gauge_context <- load_target_gauge_context(
    community = "Old Crow",
    target_gauge_name = "Porcupine River Below Old Crow River",
)

target_gauge_hierarchy <- target_gauge_context$target_basin_hierarchy
target_gauge_routing <- target_gauge_context$target_gauge_routing
target_gauge_route_table <- target_gauge_context$target_gauge_route_table
dawson_routing_plot <- plot_target_gauge_routing(target_gauge_context)

print(dawson_routing_plot)


write_target_gauge_station_encoding_json()
