# Function to get raster outline/boundary where data exists
getRasterOutline <- function(con, model, parameter) {
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

            # Get the outline/boundary of the raster where data exists
            outline_query <- paste0(
                "SELECT 
                    ST_AsText(ST_Union(ST_ConvexHull(rast))) as outline_wkt,
                    ST_XMin(ST_Union(rast)) as xmin,
                    ST_XMax(ST_Union(rast)) as xmax,
                    ST_YMin(ST_Union(rast)) as ymin,
                    ST_YMax(ST_Union(rast)) as ymax,
                    COUNT(*) as num_tiles
                 FROM spatial.rasters 
                 WHERE reference_id = ",
                reference_id
            )

            cat(
                "Executing raster outline query for reference_id:",
                reference_id,
                "\n"
            )
            outline_data <- DBI::dbGetQuery(con, outline_query)

            if (nrow(outline_data) == 0) {
                stop("No raster data found for reference_id ", reference_id)
            }

            cat(
                "Retrieved raster outline with",
                outline_data$num_tiles,
                "tiles\n"
            )
            cat(
                "Bounds: X(",
                outline_data$xmin,
                ",",
                outline_data$xmax,
                ") Y(",
                outline_data$ymin,
                ",",
                outline_data$ymax,
                ")\n"
            )

            return(outline_data)
        },
        error = function(e) {
            cat("Error in getRasterOutline:", e$message, "\n")
            return(NULL)
        }
    )
}

# Example: Get raster outline and plot it
# Get the raster outline
outline_data <- getRasterOutline(con2, "ERA5_land", "snow water equivalent")

if (!is.null(outline_data)) {
    cat("Successfully retrieved raster outline\n")

    # Parse the WKT geometry for plotting
    if (requireNamespace("sf", quietly = TRUE)) {
        # Convert WKT to sf object
        outline_geom <- sf::st_as_sfc(outline_data$outline_wkt)
        outline_coords <- sf::st_coordinates(outline_geom)

        # Plot with ggplot2 if available
        if (requireNamespace("ggplot2", quietly = TRUE)) {
            library(ggplot2)
            p <- ggplot() +
                geom_polygon(
                    data = data.frame(outline_coords),
                    aes(x = X, y = Y),
                    fill = "lightblue",
                    color = "black",
                    alpha = 0.7
                ) +
                labs(
                    title = "ERA5 Land Raster Outline",
                    subtitle = paste(
                        "Coverage area with",
                        outline_data$num_tiles,
                        "tiles"
                    ),
                    x = "Longitude",
                    y = "Latitude"
                ) +
                theme_minimal() +
                coord_equal()

            print(p)
        }
    } else {
        # Simple bounding box plot if sf not available
        plot(
            c(
                outline_data$xmin,
                outline_data$xmax,
                outline_data$xmax,
                outline_data$xmin,
                outline_data$xmin
            ),
            c(
                outline_data$ymin,
                outline_data$ymin,
                outline_data$ymax,
                outline_data$ymax,
                outline_data$ymin
            ),
            type = "l",
            col = "blue",
            lwd = 2,
            main = "ERA5 Land Raster Bounding Box",
            xlab = "Longitude",
            ylab = "Latitude"
        )
        rect(
            outline_data$xmin,
            outline_data$ymin,
            outline_data$xmax,
            outline_data$ymax,
            col = rgb(0, 0, 1, 0.3),
            border = "blue"
        )
    }
}
