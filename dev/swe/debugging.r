library(shiny)
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
