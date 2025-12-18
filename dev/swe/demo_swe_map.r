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

snowbull_timeseries <- load_bulletin_timeseries(
    con = con,
    load_temp = TRUE
)

start_year_historical <- 1991
end_year_historical <- 2020

current_year <- 2023
ts <- snowbull_timeseries$temperature$timeseries$data

# ts should have columns: datetime, station1, station2, ...
library(lubridate)


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


parameter <- "precipitation"


get_norms <- function(
    start_year_historical,
    end_year_historical,
    ts,
    parameter,
    end_months_historical = c(2, 3, 4, 5)
) {
    aggr_fun <- switch(
        parameter,
        precipitation = function(x) sum(x, na.rm = TRUE),
        swe = function(x) mean(x, na.rm = TRUE),
        temperature = function(x) mean(x, na.rm = TRUE),
        function(x) mean(x, na.rm = TRUE)
    )

    station_names <- setdiff(colnames(ts), "datetime")

    # Create a 3D array: years x months x stations
    historical_distr <- array(
        NA,
        dim = c(
            end_year_historical - start_year_historical + 1,
            length(end_months_historical),
            length(station_names)
        ),
        dimnames = list(
            year = as.character(start_year_historical:end_year_historical),
            month = as.character(end_months_historical),
            station = station_names
        )
    )

    for (yr in start_year_historical:end_year_historical) {
        for (m in end_months_historical) {
            start_month_historical <- switch(
                parameter,
                precipitation = 10,
                swe = m - 1,
                temperature = m - 1,
                10
            )

            start_date <- as.Date(sprintf(
                "%d-%02d-01",
                yr - 1,
                start_month_historical
            ))
            end_date <- as.Date(sprintf("%d-%02d-01", yr, m))
            idx <- which(ts$datetime >= start_date & ts$datetime < end_date)
            for (station in station_names) {
                aggr_value <- aggr_fun(ts[idx, station])
                historical_distr[
                    as.character(yr),
                    as.character(m),
                    station
                ] <- aggr_value
            }
        }
    }

    # Calculate median (norm) for each station and month
    station_norms <- apply(historical_distr, c(2, 3), median, na.rm = TRUE)
    # station_norms: months x stations

    list(
        station_norms = station_norms,
        historical_distr = historical_distr
    )
}

norms <- get_norms(
    start_year_historical = start_year_historical,
    end_year_historical = end_year_historical,
    ts = ts,
    parameter = "temperature"
)

apply_norms <- function(
    bulletin_month,
    bulletin_year,
    ts,
    station_norms,
    parameter
) {
    aggr_fun <- switch(
        parameter,
        precipitation = function(x) sum(x, na.rm = TRUE),
        swe = function(x) mean(x, na.rm = TRUE),
        temperature = function(x) mean(x, na.rm = TRUE),
        function(x) mean(x, na.rm = TRUE)
    )

    station_names <- setdiff(colnames(ts), "datetime")

    start_month <- switch(
        parameter,
        precipitation = 10,
        swe = bulletin_month - 1,
        temperature = bulletin_month - 1,
        10
    )

    start_date <- as.Date(sprintf("%d-%02d-01", bulletin_year - 1, start_month))
    end_date <- as.Date(sprintf("%d-%02d-01", bulletin_year, bulletin_month))

    idx <- which(ts$datetime >= start_date & ts$datetime < end_date)
    station_current <- setNames(
        sapply(station_names, function(station) aggr_fun(ts[idx, station])),
        station_names
    )
    # Save current_aggr in the same format as station_norms (named numeric vector)
    station_current <- as.numeric(station_current)
    names(station_current) <- station_names

    # Calculate the ratio of current_aggr to station_norms for each station
    relative_to_norm <- 100 * (station_current / station_norms)

    # Calculate the percentile of station_current within historical values for each station
    station_percentiles <- sapply(station_names, function(station) {
        hist_values <- historical_values[, station]
        # Remove NA values
        hist_values <- hist_values[!is.na(hist_values)]
        # Percentile: proportion of historical values less than or equal to current
        mean(hist_values <= station_current[station]) * 100
    })

    return(list(
        norms = station_norms,
        current = station_current,
        relative_to_norm = relative_to_norm,
        percentiles = station_percentiles
    ))
}

get_norms(
    ts,
    bulletin_year = 2024,
    bulletin_month = 3,
    start_year_historical = 1991,
    end_year_historical = 2020,
    aggr_fun = aggr_fun
)


make_snowbull_map(
    year = 2024,
    month = 3,
    format = "ggplot",
    parameter_name = "temp",
    statistic = "relative_to_med",
    language = "English"
)
