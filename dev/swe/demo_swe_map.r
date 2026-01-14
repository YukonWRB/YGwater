load_all()

con <- AquaCache::AquaConnect(
    name = "aquacache",
    host = Sys.getenv("aquacacheHost"),
    port = Sys.getenv("aquacachePort"),
    user = Sys.getenv("aquacacheUser"),
    password = Sys.getenv("aquacachePass")
)

query <- "SELECT FEATURE_NAME FROM VECTORS"
df <- DBI::dbGetQuery(con, query)

# Filter FEATURE_NAME values that match "YOWN-%" using regex
yown_features <- df[grepl("YOWN-\\d{4}", df$feature_name), ]

vect_data <- YGwater::getVector(
    con = con,
    feature_name = yown_features
)

# read in well data from excel
excel_file <- "G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\2_SPREADSHEETS\\1_YOWN_MASTER_TABLE\\YOWN_MASTER.xlsx"
well_data <- openxlsx::read.xlsx(
    xlsxFile = excel_file,
    sheet = "YOWN_MASTER",
    startRow = 1
)

# rename features to match those in the database
well_data$feature_name <- well_data$YOWN.Code
well_data$description <- well_data$Name

# create spatial points from well data
well_points <- terra::vect(
    well_data[, c(
        "feature_name",
        "description",
        "Longitude.(decimal.degree)",
        "Latitude.(decimal.degree)"
    )],
    geom = c("Longitude.(decimal.degree)", "Latitude.(decimal.degree)"),
    crs = "EPSG:4326"
)

# Find YOWN codes in well_points that are not already in yown_features
missing_yown <- setdiff(well_points$feature_name, yown_features)
well_points_new <- well_points[well_points$feature_name %in% missing_yown, ]

print(well_points_new)
[1] "YOWN-2308"  "YOWN-2401S" "YOWN-2401D" "YOWN-2402S" "YOWN-2402D"
[6] "YOWN-2403S" "YOWN-2403D" "YOWN-2404"  NA




communities <- getVector(
    con = con,
    layer_name = "Communities")


communities$label <- communities$feature_name
terra::writeVector(communities, "G:\\water\\Hydrology\\21_Flood_Forecasting\\05-FEWS\\shapefiles\\yukon_communities.geojson", overwrite = TRUE)

terra::writeVector(well_points, "well_points.shp", overwrite = TRUE)

leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
        data = well_points,
        radius = 5,
        color = "red",
        label = ~paste0(feature_name, ": ", description)
    ) %>%
    leaflet::addCircleMarkers(
        data = well_points_new,
        radius = 5,
        color = "blue",
        label = ~paste0(feature_name, ": ", description)
    )


    
terra::plot(well_points, col="red", cex=2)
terra::plot(well_points_new, col="blue", add=TRUE, cex=2)



terra::writeVector(vect_data, "testtest.geojson", overwrite = TRUE)


bulletin_month <- 3
bulletin_year <- 2024

make_snowbull_map(
    year = 2025,
    month = 5,
    format = "ggplot",
    parameter_name = "swe",
    statistic = "relative_to_med",
    language = "English",
    filename = "swe_bulletin_may2025.png"
)

precip_data <- download_continuous_ts(
    con,
    parameter_name = "precipitation",
    start_date = "1980-01-01",
    record_rate = "1 day"
)

# locations <- DBI::dbReadTable(con, "locations")

# snowbull_timeseries <- load_bulletin_timeseries(
#     con = con
# )

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

# This code calculates aggregated statistics (mean or sum) for each station over historical water years.
# - The `statistic` variable determines whether to compute the mean or sum.
# - An aggregation function (`aggr_fun`) is defined based on the selected statistic, handling missing values.
# - For each year in the historical period, the code:
#   - Defines the water year period (October 1 of the previous year to March 31 of the current year).
#   - Identifies the indices in the time series (`ts`) that fall within this period.
#   - For each station, applies the aggregation function to the relevant data and stores the result in `historical_sums`.
# - Finally, the code calculates the median value (norm) for each station across all historical years and stores it in `station_norms`.
