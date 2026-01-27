load_all()

con <- YGwater::AquaConnect()


# con <- YGwater::AquaConnect(
#     name = "aquacache",
#     host = Sys.getenv("aquacacheHostDev"),
#     port = Sys.getenv("aquacachePortDev"),
#     user = Sys.getenv("aquacacheUserDev"),
#     password = Sys.getenv("aquacachePassDev"),
# )

snow_id <- DBI::dbGetQuery(
    con,
    "SELECT media_id FROM media_types WHERE media_type = 'snow'"
)[1, 1]


bulletin_month <- 3
bulletin_year <- 2024


dat <- load_bulletin_timeseries(
    con = con,
    load_swe = TRUE,
    load_temp = TRUE,
    load_precip = TRUE,
    october_start = TRUE
)

swe_pillows <- get_display_data(
    dataset = dat$swe$pillows,
    year = bulletin_year,
    month = bulletin_month,
    statistic = "relative_to_med"
)

swe_basins <- get_display_data(
    dataset = dat$swe$basins,
    year = bulletin_year,
    month = bulletin_month,
    statistic = "relative_to_med"
)

precip <- get_display_data(
    dataset = dat$precipitation,
    year = bulletin_year,
    month = bulletin_month,
    statistic = "relative_to_med"
)

fdd <- get_display_data(
    dataset = dat$fdd,
    year = bulletin_year,
    month = bulletin_month,
    statistic = "relative_to_med"
)


swe <- get_normalized_bulletin_values(
    bulletin_month = 3,
    bulletin_year = 2025,
    ts = dat$swe$basins$timeseries$data,
    parameter = "swe",
    norms = dat$swe$basins$norms
)


plot(dat$fdd$timeseries$data[, 'datetime'], dat$fdd$timeseries$data[, '489'])


View(p)
plot(1:10, rnorm(10), main = "Example Plot")


get_state_as_shp(
    con = con,
    year = 2025,
    month = 3,
    parameter_name = "swe",
    statistic = "relative_to_med"
)$

swe$relative_to_norm

timeseries <- dat$swe$basins$timeseries$data


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


ok <- get_normalized_bulletin_values(
    bulletin_month = 3,
    bulletin_year = 2025,
    ts = dat$swe$basins$timeseries$data,
    parameter = "swe",
    norms = dat$swe$basins$norms
)


swe_stats <- as.data.frame(t(do.call(rbind, ok)))
swe_stats$name <- rownames(swe_stats)
rownames(swe_stats) <- NULL
swe_stats <- swe_stats[, c("name", setdiff(names(swe_stats), "name"))]


df$description <- description

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
