load_all()

con <- YGwater::AquaConnect()

dat <- load_bulletin_timeseries(
    con = con,
    load_swe = TRUE,
    load_temp = TRUE,
    load_precip = TRUE,
    load_streamflow = TRUE,
    start_year_historical = 1991,
    end_year_historical = 2020,
    october_start = TRUE,
    epsg = "EPSG:3579"
)

make_snowbull_map(
    year = 2024,
    month = 3,
    statistic = "anomalies",
    language = "English",
    format = "ggplot",
    param_name = "temperature, air",
    snowbull_timeseries = dat,
    con = con,
    filename = "map.png"
)


snowBulletin(
    year = 2025,
    month = 3,
    save_path = "C:\\Users\\esniede\\Documents\\github\\YGwater\\dev\\swe",
    con = con
)

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
bulletin_scale <- 1
language <- list(language = "English", abbrev = "en")
start_year_historical <- 1991
end_year_historical <- 2020


month <- bulletin_month
month_param <- bulletin_month
year_param <- bulletin_year
year <- bulletin_year
scale <- 1
scale_param <- 1


epsg <- "EPSG:3005"


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
    rename_map <- stats::setNames(metadata$location_id, metadata$key)
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
