load_all()

con <- YGwater::AquaConnect(
    name = "aquacache",
    host = Sys.getenv("aquacacheHostProd"),
    port = Sys.getenv("aquacachePortProd"),
    user = Sys.getenv("aquacacheUserProd"),
    password = Sys.getenv("aquacachePassProd"),
)


# ggplotOverlap(
#     location = "",
#     continuous_data = timeseries,
#     parameter = "swe",
#     snowbulletin = TRUE,
#     start_year_historical = 2010,
#     end_year_historical = 2020
# )

load_all()
dat <- load_bulletin_timeseries(
    con = con,
    load_swe = TRUE,
    load_temp = FALSE,
    load_precip = FALSE,
    load_streamflow = FALSE,
    start_year_historical = 1991,
    end_year_historical = 2020,
    october_start = TRUE,
    epsg = 3579
)

make_snowbull_map(
    year = 2025,
    month = 3,
    statistic = "relative_to_med",
    language = "English",
    format = "ggplot",
    param_name = "snow water equivalent",
    snowbull_timeseries = dat,
    con = con,
    filename = "map.png",
    start_year_historical = 1991,
    end_year_historical = 2020
)

snowBulletin(
    year = 2025,
    month = 3,
    save_path = "C:\\Users\\esniede\\Documents\\github\\YGwater\\dev\\swe",
    con = con
)

bulletin_month <- 3
bulletin_year <- 2025
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
param_name <- "snow water equivalent"

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
)

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


library(sf)
library(ggplot2)

# Download Yukon boundary from a GeoJSON URL (no rnaturalearth)
yukon_url <- "https://raw.githubusercontent.com/codeforgermany/click_that_hood/main/public/data/yukon.geojson"
yukon_boundary <- sf::st_read(yukon_url, quiet = TRUE)

# Download Canada boundary from a GeoJSON URL
canada_url <- "https://raw.githubusercontent.com/johan/world.geo.json/master/countries/CAN.geo.json"
canada <- sf::st_read(canada_url, quiet = TRUE)

# Plot Yukon boundary with terrain basemap
library(ggspatial)
library(ggmap)

# Get terrain basemap from Stamen Maps

ggmap::register_stadiamaps("774f7648-cc81-4dc5-a1f6-1d18e74e2f10")

terrain_map <- get_stadiamap(
    bbox = c(left = -142, bottom = 59, right = -123, top = 69),
    zoom = 6,
    maptype = "stamen_terrain_background",
    color = 'bw',
    force = TRUE
)


ggmap(terrain_map)
+geom_sf(data = yukon_boundary, fill = NA, color = "red", inherit.aes = FALSE) +
    theme_minimal() +
    labs(title = "Yukon Boundary Map with Terrain Basemap")
