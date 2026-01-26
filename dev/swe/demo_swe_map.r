load_all()

con <- YGwater::AquaConnect()


snow_id <- DBI::dbGetQuery(
    con,
    "SELECT media_id FROM media_types WHERE media_type = 'snow'"
)[1, 1]


bulletin_month <- 3
bulletin_year <- 2024


dat <- load_bulletin_timeseries(
    con = con,
    load_temp = TRUE
)

ok <- get_normalized_bulletin_values(
    bulletin_month = 4,
    bulletin_year = 2025,
    ts = dat$fdd$timeseries$data,
    parameter = "fdd",
    norms = dat$fdd$norms
)

timeseries <- dat$fdd$timeseries$data


timeseries_key_to_name <- function(timeseries, metadata) {
    station_ids <- names(timeseries)
    # Exclude datetime column if present
    station_ids <- setdiff(station_ids, "datetime")
    # Create a named vector for renaming
    rename_map <- setNames(metadata$location_id, metadata$key)
    # Only rename columns that exist in both timeseries and metadata
    common_ids <- intersect(station_ids, names(rename_map))
    names(timeseries)[match(common_ids, names(timeseries))] <- rename_map[
        common_ids
    ]
    return(timeseries)
}


timeseries <- timeseries_key_to_name(
    timeseries = dat$fdd$timeseries$data,
    metadata = dat$fdd$metadata
)

# on.exit(DBI::dbDisconnect(con), add = TRUE)

# snowBulletin(
#     year = 2024,
#     month = 3,
#     save_path = "C:\\Users\\esniede\\Documents\\github\\YGwater\\dev\\swe\\exports",
#     con = con
# )

query <- "SELECT locations.name AS location_name, locations.location AS location_id, datetime, value_corrected AS value FROM measurements_continuous_corrected INNER JOIN timeseries ON measurements_continuous_corrected.timeseries_id = timeseries.timeseries_id INNER JOIN locations ON timeseries.location = locations.location\n                                           WHERE measurements_continuous_corrected.timeseries_id IN ('663', '665', '666', '668', '664', '671', '667')"

DBI::dbGetQuery(con, query)


# download_spatial_layer

ret <- make_snowbull_map(
    con = con,
    year = 2025,
    month = 4,
    format = "ggplot",
    parameter_name = "temperature",
    statistic = "anomalies",
    language = "English",
    filename = "dev\\swe\\exports\\swe_bulletin_apr2025.png"
)


ret$map

dat <- download_continuous_ts(
    con,
    parameter_name = "temperature",
    start_date = "1980-01-01"
)


plot(
    fdd$datetime,
    fdd[[station_list[1]]],
    type = "l",
    xlab = "Date",
    ylab = "Cumulative FDD (°C days)",
    main = paste("Cumulative FDD for station", station_list[1])
)


# Reshape fdd_df to wide format: rows = (water_year, year, month, datetime), columns = station, values = FDD
id_vars <- c("water_year", "year", "month", "datetime")

fdd_wide <- reshape(
    fdd_df,
    idvar = id_vars,
    timevar = "station",
    direction = "wide"
)

# Optionally, clean up column names
colnames(fdd_wide) <- sub("^FDD\\.", "", colnames(fdd_wide))


# locations <- DBI::dbReadTable(con, "locations")

# snowbull_timeseries <- load_bulletin_timeseries(
#     con = con
# )

year <- 2025
month <- 3
language <- "English"
statistic <- "relative_to_med"
parameter_name <- "swe"

# This code calculates aggregated statistics (mean or sum) for each station over historical water years.
# - The `statistic` variable determines whether to compute the mean or sum.
# - An aggregation function (`aggr_fun`) is defined based on the selected statistic, handling missing values.
# - For each year in the historical period, the code:
#   - Defines the water year period (October 1 of the previous year to March 31 of the current year).
#   - Identifies the indices in the time series (`ts`) that fall within this period.
#   - For each station, applies the aggregation function to the relevant data and stores the result in `historical_sums`.
# - Finally, the code calculates the median value (norm) for each station across all historical years and stores it in `station_norms`.
