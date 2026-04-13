basin <- "yukon"

gauge_hierarchy_path <- file.path(
    "dev",
    "freshet_forecasting",
    "gauge_hierarchy.yaml"
)

gauge_hierarchy <- yaml::read_yaml(gauge_hierarchy_path)

if (!basin %in% names(gauge_hierarchy)) {
    stop(sprintf("Basin '%s' not found in %s", basin, gauge_hierarchy_path))
}

gauges <- unlist(gauge_hierarchy[[basin]], use.names = FALSE)


con <- YGwater::AquaConnect(
    name = "aquacache",
    host = Sys.getenv("aquacacheHostProd"),
    port = Sys.getenv("aquacachePortProd"),
    user = Sys.getenv("aquacacheAdminUser"),
    password = Sys.getenv("aquacacheAdminPass"),
)


locations <- YGwater::dbGetQueryDT(
    sprintf(
        "SELECT name, latitude, longitude, location_code, location_id FROM locations WHERE location_code IN ('%s')",
        paste(gauges, collapse = "','")
    ),
    con = con
)

locations$location_id[1]


today <- Sys.Date()

# last 30 days
YGwater::ggplotOverlap(
    location = 80,
    parameter = "water level",
    startDay = as.POSIXct(today - 30),
    endDay = as.POSIXct(today + 7),
    allowed_missing = 1
)

# full year
YGwater::ggplotOverlap(
    location = 80,
    parameter = "water level",
    allowed_missing = 1
)


flows <- YGwater::dbGetQueryDT(
    sprintf(
        paste(
            "SELECT ts.location_id, ts.timeseries_id, mc.datetime, mc.value",
            "FROM measurements_continuous mc",
            "JOIN timeseries ts ON mc.timeseries_id = ts.timeseries_id",
            "JOIN parameters p ON ts.parameter_id = p.parameter_id",
            "WHERE ts.location_id IN (%s)",
            "AND p.param_name = 'water level'",
            "AND mc.datetime >= CURRENT_DATE - INTERVAL '7 days'",
            "AND mc.datetime < CURRENT_DATE + INTERVAL '1 day'",
            "ORDER BY ts.location_id, ts.timeseries_id, mc.datetime"
        ),
        paste(locations$location_id, collapse = ",")
    ),
    con = con
)

summary_table <- YGwater::dbGetQueryDT(
    sprintf(
        paste(
            "WITH filtered AS (",
            "    SELECT ts.location_id, ts.timeseries_id, mc.datetime, mc.value",
            "    FROM measurements_continuous mc",
            "    JOIN timeseries ts ON mc.timeseries_id = ts.timeseries_id",
            "    JOIN parameters p ON ts.parameter_id = p.parameter_id",
            "    WHERE ts.location_id IN (%s)",
            "      AND p.param_name = 'water level'",
            "      AND mc.datetime >= CURRENT_DATE - INTERVAL '7 days'",
            "      AND mc.datetime < CURRENT_DATE + INTERVAL '1 day'",
            "),",
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
            "        (",
            "            SELECT f.value",
            "            FROM filtered f",
            "            WHERE f.location_id = l.location_id",
            "              AND f.timeseries_id = l.timeseries_id",
            "              AND f.datetime <= l.last_datetime - INTERVAL '24 hours'",
            "            ORDER BY f.datetime DESC",
            "            LIMIT 1",
            "        ) AS value_24h,",
            "        (",
            "            SELECT f.value",
            "            FROM filtered f",
            "            WHERE f.location_id = l.location_id",
            "              AND f.timeseries_id = l.timeseries_id",
            "              AND f.datetime <= l.last_datetime - INTERVAL '48 hours'",
            "            ORDER BY f.datetime DESC",
            "            LIMIT 1",
            "        ) AS value_48h,",
            "        (",
            "            SELECT f.value",
            "            FROM filtered f",
            "            WHERE f.location_id = l.location_id",
            "              AND f.timeseries_id = l.timeseries_id",
            "              AND f.datetime <= l.last_datetime - INTERVAL '72 hours'",
            "            ORDER BY f.datetime DESC",
            "            LIMIT 1",
            "        ) AS value_72h,",
            "        (",
            "            SELECT f.value",
            "            FROM filtered f",
            "            WHERE f.location_id = l.location_id",
            "              AND f.timeseries_id = l.timeseries_id",
            "              AND f.datetime <= l.last_datetime - INTERVAL '7 days'",
            "            ORDER BY f.datetime DESC",
            "            LIMIT 1",
            "        ) AS value_1w",
            "    FROM latest_by_location l",
            ")",
            "SELECT",
            "    loc.location_id,",
            "    loc.location_code,",
            "    loc.name,",
            "    c.timeseries_id,",
            "    c.last_datetime - INTERVAL '7 hours' AS latest_time,",
            "    c.current_water_level,",
            "    c.current_water_level - c.value_24h AS change_24h,",
            "    c.current_water_level - c.value_48h AS change_48h,",
            "    c.current_water_level - c.value_72h AS change_72h,",
            "    c.current_water_level - c.value_1w AS change_1w,",
            "    EXTRACT(EPOCH FROM (NOW() - c.last_datetime)) / 3600.0 AS last_data_age_hours",
            "FROM locations loc",
            "LEFT JOIN changes c ON c.location_id = loc.location_id",
            "WHERE loc.location_id IN (%s)",
            "ORDER BY loc.location_code"
        ),
        paste(locations$location_id, collapse = ","),
        paste(locations$location_id, collapse = ",")
    ),
    con = con
)

summary_table
