load_all()
con <- AquaCache::AquaConnect(
    name = "aquacache",
    host = "10.250.12.154",
    port = 5432,
    user = "public_reader",
    password = "aquacache"
)

on.exit(DBI::dbDisconnect(con), add = TRUE)

snowBulletin(
    year = 2024,
    month = 3,
    save_path = "C:\\Users\\esniede\\Documents\\github\\YGwater\\dev\\swe",
    con = con
)

year <- 2025
month <- 3
language <- "English"
statistic <- "relative_to_med"
parameter_name <- "swe"

# snowbull_timeseries <- load_bulletin_timeseries(
#     con = con,
#     load_temp = TRUE
# )

# This code calculates aggregated statistics (mean or sum) for each station over historical water years.
# - The `statistic` variable determines whether to compute the mean or sum.
# - An aggregation function (`aggr_fun`) is defined based on the selected statistic, handling missing values.
# - For each year in the historical period, the code:
#   - Defines the water year period (October 1 of the previous year to March 31 of the current year).
#   - Identifies the indices in the time series (`ts`) that fall within this period.
#   - For each station, applies the aggregation function to the relevant data and stores the result in `historical_sums`.
# - Finally, the code calculates the median value (norm) for each station across all historical years and stores it in `station_norms`.

bulletin_month <- 3
bulletin_year <- 2024

make_snowbull_map(
    year = 2025,
    month = 5,
    format = "ggplot",
    parameter_name = "swe",
    statistic = "relative_to_med",
    language = "English"
)
