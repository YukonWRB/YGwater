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

# Select first of each month
era5_raster_data$year_month <- format(era5_raster_data$datetime, "%Y-%m")
era5_raster_data <- era5_raster_data[!duplicated(era5_raster_data$year_month), ]
era5_raster_data$year_month <- NULL

# Filter for years > 2000
era5_raster_data <- era5_raster_data[
    format(era5_raster_data$datetime, "%Y") > "2020",
]


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
test_date <- as.Date("2023-04-01")
recent_sept <- get_date_datum(test_date, 8)
# Get raster corresponding to test_date
test_date_raster_idx <- which(as.Date(time_vector) == test_date)
test_date_raster <- era5_rasters[[test_date_raster_idx]]


annual_low_idx <- which(as.Date(time_vector) == recent_sept)
annual_low_raster <- era5_rasters[[annual_low_idx]]

swe_minus_perenial <- test_date_raster - annual_low_raster
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
