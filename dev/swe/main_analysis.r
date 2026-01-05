library(AquaCache)
library(YGwater)
library(DBI)
library(plotly)
library(leaflet)
library(sf)


config <- list(
    dbName = "aquacache",
    dbHost = Sys.getenv("aquacacheHostProd"),
    dbPort = Sys.getenv("aquacachePortProd"),
    dbUser = Sys.getenv("aquacacheUserProd"),
    dbPass = Sys.getenv("aquacachePassProd")
)

con2 <- AquaConnect(
    name = config$dbName,
    host = config$dbHost,
    port = config$dbPort,
    user = config$dbUser,
    pass = config$dbPass
)


download_spatial_layer <- function(con, layer_name, additional_query = NULL) {
    query <- sprintf(
        "SELECT *, ST_AsText(ST_Transform(geom, 4326)) as geom_4326 
         FROM spatial.vectors 
         WHERE layer_name = %s",
        DBI::dbQuoteString(con, layer_name)
    )

    if (!is.null(additional_query) && nzchar(additional_query)) {
        query <- paste(query, additional_query)
    }

    data <- DBI::dbGetQuery(con, query)
    if (nrow(data) == 0) {
        warning(sprintf("No data found for layer: %s", layer_name))
        return(NULL)
    }

    geom <- sf::st_as_sfc(data$geom_4326, crs = 4326)
    sf::st_sf(data, geometry = geom, crs = 4326)
}

place_types <- c("City", "Town", "Village") # Filter to major communities only
communities <- download_spatial_layer(
    con2,
    "Communities",
    additional_query = sprintf(
        "AND (description IN (%s) OR feature_name IN ('Old Crow', 'Beaver Creek'))",
        paste(
            sapply(
                place_types,
                function(x) DBI::dbQuoteString(con, x)
            ),
            collapse = ", "
        )
    )
)


query_date <- as.Date("2025-11-10", format = "%Y-%m-%d")

# Single query to get raster values for ERA5 model
era5_query <- "
SELECT 
    r.reference_id,
    rr.valid_from as datetime
FROM spatial.raster_series_index rsi
JOIN spatial.rasters_reference rr ON rsi.raster_series_id = rr.raster_series_id
JOIN spatial.rasters r ON r.reference_id = rr.reference_id
WHERE rsi.model = 'ERA5_land'
ORDER BY rr.valid_from, r.reference_id"
era5_raster_data <- DBI::dbGetQuery(con2, era5_query)

# Convert to time series object
era5_raster_data$datetime <- as.POSIXct(era5_raster_data$datetime)

# Filter for records that match the day of year of query_date
query_day_of_year <- as.numeric(format(query_date, "%j"))
era5_raster_data$day_of_year <- as.numeric(format(
    era5_raster_data$datetime,
    "%j"
))
era5_raster_data <- era5_raster_data[
    era5_raster_data$day_of_year == query_day_of_year,
]
era5_raster_data$day_of_year <- NULL
# Filter to only include data up to year 2000
era5_raster_data$year <- as.numeric(format(era5_raster_data$datetime, "%Y"))
era5_raster_data <- era5_raster_data[era5_raster_data$year >= 1980, ]
era5_raster_data$year <- NULL

# Get all raster objects for the ERA5 data
era5_rasters <- list()
for (i in 1:nrow(era5_raster_data)) {
    rid <- era5_raster_data$reference_id[i]
    rast <- YGwater::getRaster(
        con = con2,
        clauses = paste0("WHERE reference_id = ", rid)
    )
    era5_rasters[[i]] <- rast
    cat(
        "Retrieved raster",
        i,
        "of",
        nrow(era5_raster_data),
        "for reference_id:",
        rid,
        "\n"
    )
}


# Combine into a single raster stack if they have the same dimensions
era5_stack <- do.call(c, era5_rasters)
#time_vector <- as.POSIXct(
#    terra::depth(era5_stack),
#    origin = "1970-01-01",
#    tz = "UTC"
#)
time_vector <- era5_raster_data$datetime
month_vector <- as.numeric(format(time_vector, "%m"))


# Remove the most recent date from the stack before calculating median
n_layers <- terra::nlyr(era5_stack)
era5_stack_subset <- era5_stack[[1:(n_layers - 1)]]

# Calculate median across time dimension (layers), excluding most recent
era5_median <- terra::app(era5_stack_subset, fun = median, na.rm = TRUE)
# Get the most recent layer from the stack
era5_recent <- era5_stack[[n_layers]]

# Calculate relative change: most recent divided by median
era5_relative_change <- 100 * era5_recent / era5_median


# Load shapefile
hybas_shapefile <- sf::st_read(
    "H:/esniede/data/HydroSheds/hybas_ar_lev01-12_v1c/hybas_ar_lev07_v1c.shp"
)
hybas_shapefile <- sf::st_transform(hybas_shapefile, crs = 4326)

# Select the specific HYBAS basin with ID 8070214100
hybas_id_to_select <- 8070212940
selected_basin <- hybas_shapefile[
    hybas_shapefile$HYBAS_ID == hybas_id_to_select,
]
# Plot ERA5 median SWE as base layer

# Function to get plot limits from geometry with buffer
get_lims_from_geom <- function(geom, buffer_m = 5000) {
    # Get extents of geometry
    geom_bbox <- sf::st_bbox(geom)

    # Convert to degrees (approximate conversion at this latitude)
    lat_center <- mean(c(geom_bbox["ymin"], geom_bbox["ymax"]))
    m_to_deg_lon <- 1 / (111320 * cos(lat_center * pi / 180))
    m_to_deg_lat <- 1 / 110540

    buffer_deg_lon <- buffer_m * m_to_deg_lon
    buffer_deg_lat <- buffer_m * m_to_deg_lat

    # Apply buffer to bounding box
    xlim <- c(
        geom_bbox["xmin"] - buffer_deg_lon,
        geom_bbox["xmax"] + buffer_deg_lon
    )
    ylim <- c(
        geom_bbox["ymin"] - buffer_deg_lat,
        geom_bbox["ymax"] + buffer_deg_lat
    )

    return(list(xlim = xlim, ylim = ylim))
}

# Get extents of selected basin with 500m buffer
lims <- get_lims_from_geom(selected_basin, buffer_m = 5000)
xlim <- lims$xlim
ylim <- lims$ylim

# Plot ERA5 median SWE with zoomed extents
terra::plot(
    era5_relative_change,
    main = paste(
        "ERA5 SWE Relative to historic median (1980-2024) on",
        format(query_date, "%Y-%m-%d"),
        "with HYBAS Basin:",
        hybas_id_to_select
    ),
    xlab = "Longitude",
    ylab = "Latitude",
    range = c(0, 200),
    xlim = xlim,
    ylim = ylim,
    col = colorRampPalette(c("red", "white", "blue"))(100)
)
# Add the selected basin as overlay
plot(selected_basin$geometry, border = "darkblue", lwd = 2, add = TRUE)

# Add communities as points
plot(communities$geometry, pch = 16, bg = "orange", cex = 2.5, add = TRUE)
# Add community labels
text(
    sf::st_coordinates(communities),
    labels = communities$feature_name,
    pos = 1,
    cex = 1.2
)


terra::plot(
    era5_recent * 1000,
    main = "ERA5 Median SWE",
    col = terrain.colors(100),
    range = c(0, 100)
)

# Function to get the most recent September 1st given a date
get_date_datum <- function(date_input, month = 9) {
    if (month < 1 | month > 12) {
        stop("Month must be between 1 and 12")
    }
    if (date_input == NA) {
        stop("date_input cannot be NA")
    }

    date_input <- as.Date(date_input)
    year <- as.numeric(format(date_input, "%Y"))

    # Create September 1st for the current year
    sept1_current <- as.Date(paste0(year, "-", sprintf("%02d", month), "-01"))

    # If the input date is before Sept 1st of current year, use previous year
    if (date_input < sept1_current) {
        sept1_recent <- as.Date(paste0(
            year - 1,
            "-",
            sprintf("%02d", month),
            "-01"
        ))
    } else {
        sept1_recent <- sept1_current
    }

    return(sept1_recent)
}


# Get the most recent September 1st for March 1, 2023
test_date <- as.Date("2025-01-01")
recent_sept <- get_date_datum(test_date, 8)
# Get raster corresponding to test_date
test_date_raster_idx <- which(as.Date(time_vector) == test_date)
test_date_raster <- era5_rasters[[test_date_raster_idx]]


annual_low_idx <- which(as.Date(time_vector) == recent_sept)
annual_low_raster <- era5_rasters[[annual_low_idx]]

swe_minus_perenial <- test_date_raster - annual_low_raster

swe_perrenial_mask <- annual_low_raster > 0.1

swe_masked <- test_date_raster
swe_masked[swe_perrenial_mask] <- NA


# Load shapefile
swe_basins <- sf::st_read("inst/snow_survey/swe_basins/swe_basins.shp")
swe_basins <- sf::st_transform(swe_basins, crs = 4326)

# Clip swe_masked based on swe_basins
swe_clipped <- terra::crop(swe_masked, swe_basins)
# Mask and calculate mean for each basin
basin_means <- data.frame(
    basin_id = 1:nrow(swe_basins),
    mean_swe = NA
)

for (i in 1:nrow(swe_basins)) {
    # Extract single basin polygon
    single_basin <- swe_basins[i, ]

    # Crop and mask raster to this basin
    basin_raster <- terra::crop(swe_clipped, single_basin)
    basin_raster <- terra::mask(basin_raster, single_basin)

    # Calculate mean value for this basin
    basin_means$mean_swe[i] <- terra::global(
        basin_raster,
        fun = "mean",
        na.rm = TRUE
    )$mean

    cat("Basin", i, "mean SWE:", basin_means$mean_swe[i], "\n")
}

# Add mean SWE values to the shapefile
swe_basins$mean_swe <- basin_means$mean_swe

# Plot the basins with SWE values
plot(swe_basins["mean_swe"], main = "Basin Average SWE")


#swe_minus_perenial[swe_minus_perenial < 0] <- 0

plot(swe_minus_perenial)
plot(test_date_raster)
plot(annual_low_raster)


# Get raster corresponding to recent_sept
recent_sept_raster_idx <- which(as.Date(time_vector) == recent_sept)
if (length(recent_sept_raster_idx) > 0) {
    recent_sept_raster <- era5_rasters[[recent_sept_raster_idx]]
    cat(
        "Found raster for recent_sept",
        recent_sept,
        "at index",
        recent_sept_raster_idx,
        "\n"
    )
} else {
    cat("No raster found for recent_sept", recent_sept, "\n")
    recent_sept_raster <- NULL
}


cat("Most recent September 1st for", test_date, "is:", recent_sept, "\n")

plot(era5_rasters[[16]])

era5_mean <- global(era5_stack, fun = max, na.rm = TRUE)


# Convert to timeseries
era5_ts <- data.frame(
    datetime = time_vector,
    mean_value = era5_mean$mean
)

# Remove February 29th if it exists
era5_ts <- era5_ts[!(format(era5_ts$datetime, "%m-%d") == "02-29"), ]

# Convert datetime to day of year
era5_ts$day_of_year <- as.numeric(format(era5_ts$datetime, "%j"))

plot(
    era5_ts$day_of_year,
    era5_ts$mean_value,
    type = "l",
    xlab = "Day of Year",
    ylab = "Mean Value",
    main = "ERA5 Mean Values Timeseries 2000 (without Feb 29)"
)


# Get datetime corresponding to era5_ts minimum
min_index <- which.min(era5_ts$mean_value)
min_datetime <- era5_ts$datetime[min_index]

cat(
    "Minimum value occurred at:",
    format(min_datetime, "%Y-%m-%d %H:%M:%S"),
    "\n"
)

plot(era5_ts)

# Plot the timeseries
plot(
    era5_ts$datetime,
    era5_ts$mean_value,
    type = "l",
    xlab = "Date",
    ylab = "Mean Value",
    main = "ERA5 Mean Values Timeseries 2000"
)


# Query point directly from database without downloading entire raster
coord <- c(-135.0, 60.7) # Whitehorse


getRasterSeriesAtPoint <- function(con, model, parameter, lon, lat) {
    # First check if model and parameter exist in the table
    check_query <- paste0(
        "SELECT COUNT(*) as count FROM spatial.raster_series_index 
         WHERE model = '",
        model,
        "' AND parameter = '",
        parameter,
        "'"
    )
    check_result <- DBI::dbGetQuery(con, check_query)

    if (check_result$count == 0) {
        stop(
            "Model '",
            model,
            "' with parameter '",
            parameter,
            "' not found in raster_series_index table"
        )
    }

    query <- paste0(
        "SELECT rr.reference_id, rr.valid_from as datetime, ST_Value(r.rast, ST_SetSRID(ST_Point(",
        lon,
        ", ",
        lat,
        "), 4326)) as value
         FROM spatial.raster_series_index rsi
         JOIN spatial.rasters_reference rr ON rsi.raster_series_id = rr.raster_series_id
         JOIN spatial.rasters r ON r.reference_id = rr.reference_id
         WHERE rsi.model = '",
        model,
        "' 
         AND rsi.parameter = '",
        parameter,
        "'
         AND ST_Intersects(r.rast, ST_SetSRID(ST_Point(",
        lon,
        ", ",
        lat,
        "), 4326))"
    )
    DBI::dbGetQuery(con, query)

    # Convert datetime to Date for plotting
    sampled_values$datetime <- as.POSIXct(sampled_values$datetime)

    # Sort by datetime
    sampled_values <- sampled_values[order(sampled_values$datetime), ]

    # Query to get units from raster_reference table
    units_query <- paste0(
        "SELECT DISTINCT rr.units
         FROM spatial.raster_series_index rsi
         JOIN spatial.rasters_reference rr ON rsi.raster_series_id = rr.raster_series_id
         WHERE rsi.model = '",
        model,
        "' 
         AND rsi.parameter = '",
        parameter,
        "'"
    )
    units_result <- DBI::dbGetQuery(con, units_query)

    # Calculate metadata
    metadata <- list(
        count = nrow(sampled_values),
        min_value = min(sampled_values$value, na.rm = TRUE),
        max_value = max(sampled_values$value, na.rm = TRUE),
        mean_value = mean(sampled_values$value, na.rm = TRUE),
        date_range = range(sampled_values$datetime, na.rm = TRUE),
        na_count = sum(is.na(sampled_values$value)),
        unit = units_result$units[1],
        model = model,
        type = parameter
    )

    return(list(data = sampled_values, metadata = metadata))
}
# Merged query to get ERA5 land SWE values at specific coordinates
result <- getRasterSeriesAtPoint(
    con2,
    model = "ERA5_land",
    parameter = "snow water equivalent",
    lon = coord[1],
    lat = coord[2]
)

sampled_values <- result$data
metadata <- result$metadata


# Create basic plot
plot(
    sampled_values$datetime,
    sampled_values$value,
    type = "l",
    xlab = "Date",
    ylab = "Snow Water Equivalent",
    main = "ERA5 Land SWE at Whitehorse"
)


# Function to get raster data for a given model and parameter
getRasterData <- function(con, model, parameter) {
    tryCatch(
        {
            # First, get the raster series info
            series_query <- paste0(
                "SELECT raster_series_id FROM spatial.raster_series_index 
             WHERE model = '",
                model,
                "' AND parameter = '",
                parameter,
                "'"
            )
            series_result <- DBI::dbGetQuery(con, series_query)

            if (nrow(series_result) == 0) {
                stop(
                    "No raster series found for model '",
                    model,
                    "' and parameter '",
                    parameter,
                    "'"
                )
            }

            raster_series_id <- series_result$raster_series_id[1]

            # Get the first reference_id for this series
            ref_query <- paste0(
                "SELECT reference_id FROM spatial.rasters_reference 
             WHERE raster_series_id = ",
                raster_series_id,
                " 
             ORDER BY reference_id LIMIT 1"
            )
            ref_result <- DBI::dbGetQuery(con, ref_query)

            if (nrow(ref_result) == 0) {
                stop(
                    "No reference found for raster series ID ",
                    raster_series_id
                )
            }

            reference_id <- ref_result$reference_id[1]

            # Get the raster data and convert to vector
            raster_query <- paste0(
                "SELECT 
                (ST_PixelAsPolygons(rast)).geom as geometry,
                (ST_PixelAsPolygons(rast)).val as value,
                ST_X(ST_Centroid((ST_PixelAsPolygons(rast)).geom)) as lon,
                ST_Y(ST_Centroid((ST_PixelAsPolygons(rast)).geom)) as lat
             FROM spatial.rasters 
             WHERE reference_id = ",
                reference_id
            )

            cat("Executing raster query for reference_id:", reference_id, "\n")
            raster_data <- DBI::dbGetQuery(con, raster_query)

            if (nrow(raster_data) == 0) {
                stop("No raster data found for reference_id ", reference_id)
            }

            cat("Retrieved", nrow(raster_data), "raster pixels\n")

            return(raster_data)
        },
        error = function(e) {
            cat("Error in getRasterData:", e$message, "\n")
            return(NULL)
        }
    )
}

# Example: Get raster data and plot it
# Get the raster data
raster_data <- getRasterData(con2, "ERA5_land", "snow water equivalent")

if (!is.null(raster_data)) {
    cat("Successfully retrieved raster with", nrow(raster_data), "pixels\n")
    cat("Value range:", range(raster_data$value, na.rm = TRUE), "\n")

    # Plot the raster grid (uniform grid showing cell locations)
    if (requireNamespace("ggplot2", quietly = TRUE)) {
        library(ggplot2)
        p <- ggplot(raster_data, aes(x = lon, y = lat)) +
            geom_tile(fill = "lightblue", color = "black", size = 0.1) +
            labs(
                title = "ERA5 Land Raster Grid",
                subtitle = paste("Grid cells:", nrow(raster_data)),
                x = "Longitude",
                y = "Latitude"
            ) +
            theme_minimal() +
            coord_equal()

        print(p)
    } else {
        # Fallback to base R plot
        plot(
            raster_data$lon,
            raster_data$lat,
            col = "lightblue",
            pch = 15,
            cex = 0.5,
            main = "ERA5 Land Raster Grid",
            sub = paste("Grid cells:", nrow(raster_data)),
            xlab = "Longitude",
            ylab = "Latitude"
        )
        grid()
    }
}
