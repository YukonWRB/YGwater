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

dat <- load_bulletin_timeseries(
    con = con,
    load_swe = TRUE,
    load_temp = FALSE,
    load_precip = FALSE,
    load_streamflow = FALSE,
    start_year_historical = 1991,
    end_year_historical = 2020,
    october_start = TRUE,
    epsg = 4326
)

make_snowbull_map(
    year = 2025,
    month = 3,
    statistic = "relative_to_med",
    language = "English",
    format = "leaflet",
    param_name = "snow water equivalent",
    snowbull_timeseries = dat,
    con = con,
    filename = "map.html",
    start_year_historical = 1991,
    end_year_historical = 2020
)

snowBulletin(
    year = 2025,
    month = 3,
    save_path = "C:\\Users\\esniede\\Documents\\github\\YGwater\\dev\\swe",
    con = con
)


surveys_table <- dat$swe$surveys$metadata[c(
    "name",
    "location",
    "location_id",
    "basin",
    "conversion_m"
)]

target <- c(
    "Upper_Yukon",
    "Teslin_Big_Salmon",
    "Central_Yukon",
    "Pelly",
    "Stewart",
    "White",
    "Lower_Yukon",
    "Porcupine",
    "Peel",
    "Liard",
    "Alsek",
    "Alaska"
)

# Order by basin (factor with levels = target), then by location
surveys_table <- surveys_table[
    order(factor(surveys_table$basin, levels = target), surveys_table$location),
]

stations <- c(
    "09AA-SC01",
    "09AA-SC02",
    "09AA-SC03",
    "09AA-SC04",
    "09AB-SC01",
    "09AB-SC02",
    "09AD-SC01",
    "09AD-SC02",
    "09AE-SC01",
    "09AH-SC01",
    "09AH-SC03",
    "09AH-SC04",
    "09BA-SC02",
    "09BA-SC02",
    "09BA-SC03",
    "09BA-SC04",
    "09BA-SC05",
    "09BB-SC03",
    "09BB-SC04",
    "09BC-SC01",
    "09CD-SC03",
    "09DA-SC01",
    "09DB-SC01",
    "09DB-SC02",
    "09DC-SC01",
    "09DC-SC01",
    "09DC-SC02",
    "09DD-SC01",
    "09CA-SC01",
    "09CA-SC02",
    "09CA-SC03",
    "09CB-SC01",
    "09CB-SC02",
    "09CD-SC01",
    "09EA-SC01",
    "09EA-SC02",
    "09EB-SC01",
    "09EC-SC02",
    "09FA-SC01",
    "09FB-SC01",
    "09FB-SC02",
    "09FD-SC01",
    "10MA-SC01",
    "10MA-SC02",
    "10MB-SC01",
    "10AA-SC01",
    "10AA-SC02",
    "10AA-SC03",
    "10AA-SC04",
    "10AB-SC01",
    "10AD-SC01",
    "08AA-SC01",
    "08AA-SC02",
    "08AA-SC03",
    "08AA-SC04",
    "08AB-SC03",
    "08AK-SC01",
    "08AK-SC02"
)


# con <- YGwater::AquaConnect(
#     name = "aquacache",
#     host = Sys.getenv("aquacacheHostDev"),
#     port = Sys.getenv("aquacachePortDev"),
#     user = Sys.getenv("aquacacheUserDev"),
#     password = Sys.getenv("aquacachePassDev"),
# )

# Query snow survey dates and snow depths for the stats table
location_ids_list <- paste0(
    "{",
    paste(surveys_table$location_id, collapse = ","),
    "}"
)

target_date <- paste(
    bulletin_year,
    sprintf("%02d", bulletin_month),
    "01",
    sep = "-"
)

# Query for snow depth
query_snow_depth <- "
    SELECT s.location_id, s.datetime as survey_date, r.result as snow_depth
    FROM discrete.samples s
    JOIN discrete.results r ON s.sample_id = r.sample_id
    WHERE s.location_id = ANY($1)
      AND DATE(s.target_datetime) = DATE($2)
      AND r.parameter_id = (
          SELECT parameter_id FROM public.parameters WHERE param_name = $3
      )
      AND r.result IS NOT NULL
"

results_snow_depth <- DBI::dbGetQuery(
    con,
    query_snow_depth,
    params = list(location_ids_list, target_date, "snow depth")
)

surveys_table <- merge(
    surveys_table,
    results_snow_depth[, c("location_id", "survey_date", "snow_depth")],
    by.x = "location_id",
    by.y = "location_id",
    all.x = TRUE
)

# Query for snow water equivalent
query_swe <- "
    SELECT s.location_id, r.result as snow_water_equivalent
    FROM discrete.samples s
    JOIN discrete.results r ON s.sample_id = r.sample_id
    WHERE s.location_id = ANY($1)
      AND DATE(s.target_datetime) = DATE($2)
      AND r.parameter_id = (
          SELECT parameter_id FROM public.parameters WHERE param_name = $3
      )
      AND r.result IS NOT NULL
"

results_swe <- DBI::dbGetQuery(
    con,
    query_swe,
    params = list(location_ids_list, target_date, "snow water equivalent")
)

surveys_table <- merge(
    surveys_table,
    results_swe[, c("location_id", "snow_water_equivalent")],
    by.x = "location_id",
    by.y = "location_id",
    all.x = TRUE
)


# Query for snow water equivalent
query_swe <- "
    SELECT s.location_id, r.result as last_year_snow_water_equivalent
    FROM discrete.samples s
    JOIN discrete.results r ON s.sample_id = r.sample_id
    WHERE s.location_id = ANY($1)
    AND DATE(s.target_datetime) = DATE($2) - INTERVAL '1 year'
      AND r.parameter_id = (
          SELECT parameter_id FROM public.parameters WHERE param_name = $3
      )
      AND r.result IS NOT NULL
"

results_swe <- DBI::dbGetQuery(
    con,
    query_swe,
    params = list(location_ids_list, target_date, "snow water equivalent")
)

surveys_table <- merge(
    surveys_table,
    results_swe[, c("location_id", "last_year_snow_water_equivalent")],
    by.x = "location_id",
    by.y = "location_id",
    all.x = TRUE
)


# Query to get the min and max survey dates per station for the target month and day across all years,
# and return the difference in years (max_date - min_date)
query <- "
    SELECT
        s.location_id,
        MIN(s.target_datetime) AS min_date,
        MAX(s.target_datetime) AS max_date,
        PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY r.result) AS swe_historical_median,
        MIN(r.result) AS swe_historical_min,
        MAX(r.result) AS swe_historical_max
    FROM discrete.samples s
    JOIN discrete.results r ON s.sample_id = r.sample_id
    WHERE s.location_id = ANY($1)
      AND EXTRACT(MONTH FROM s.target_datetime) = $2
      AND EXTRACT(DAY FROM s.target_datetime) = $3
      AND r.parameter_id = (
          SELECT parameter_id FROM public.parameters WHERE param_name = $4
      )
      AND r.result IS NOT NULL
      AND EXTRACT(YEAR FROM s.target_datetime) < EXTRACT(YEAR FROM DATE($5))
    GROUP BY s.location_id
"

median_results_df <- DBI::dbGetQuery(
    con,
    query,
    params = list(
        location_ids_list,
        bulletin_month,
        1,
        "snow water equivalent",
        target_date
    )
)


surveys_table <- merge(
    surveys_table,
    median_results_df[, c(
        "location_id",
        "swe_historical_median",
        "years_of_record"
    )],
    by.x = "location_id",
    by.y = "location_id",
    all.x = TRUE
)

query <- "
    SELECT
        s.location_id,
        MIN(s.target_datetime) AS min_date,
        MAX(s.target_datetime) AS max_date,
        COUNT(DISTINCT EXTRACT(YEAR FROM s.target_datetime)) AS num_years
    FROM discrete.samples s
    JOIN discrete.results r ON s.sample_id = r.sample_id
    WHERE s.location_id = ANY($1)
      AND EXTRACT(MONTH FROM s.target_datetime) = $2
      AND EXTRACT(DAY FROM s.target_datetime) = $3
      AND r.parameter_id = (
          SELECT parameter_id FROM public.parameters WHERE param_name = $4
      )
      AND r.result IS NOT NULL
      AND EXTRACT(YEAR FROM s.target_datetime) <= EXTRACT(YEAR FROM DATE($5))
    GROUP BY s.location_id
"


daterange_results_df <- DBI::dbGetQuery(
    con,
    query,
    params = list(
        location_ids_list,
        bulletin_month,
        1,
        "snow water equivalent",
        target_date
    )
)

surveys_table <- merge(
    surveys_table,
    daterange_results_df[, c(
        "location_id",
        "num_years"
    )],
    by.x = "location_id",
    by.y = "location_id",
    all.x = TRUE
)


surveys_table[["relative_to_med"]] <- 100 *
    surveys_table[["snow_water_equivalent"]] /
    surveys_table[["swe_historical_median"]]


surveys_table <- surveys_table[surveys_table$location %in% stations, ]


surveys_table$location_id <- NULL


surveys_table[, c("name", "location", "survey_date", "snow_depth", )]
survey_date <- format(as.Date(surveys_table[["survey_date"]]), "%m-%d")

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
