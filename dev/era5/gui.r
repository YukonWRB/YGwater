library(shiny)
library(AquaCache)
library(YGwater)
library(DBI)
library(plotly)
library(leaflet)
library(sf)


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
    sampled_values <- DBI::dbGetQuery(con, query)

    # Convert datetime to Date for plotting
    sampled_values$datetime <- as.POSIXct(sampled_values$datetime)

    # Sort by datetime
    sampled_values <- sampled_values[
        order(sampled_values$datetime),
        c("datetime", "value")
    ]

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


# Function to get and plot raster data for a given model and parameter
getRasterAndPlot <- function(con, model, parameter) {
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

            # Create a simple plot
            if (requireNamespace("ggplot2", quietly = TRUE)) {
                p <- ggplot2::ggplot(
                    raster_data,
                    ggplot2::aes(x = lon, y = lat, fill = value)
                ) +
                    ggplot2::geom_tile() +
                    ggplot2::scale_fill_viridis_c() +
                    ggplot2::labs(
                        title = paste("Raster Plot:", model, "-", parameter),
                        subtitle = paste("Reference ID:", reference_id),
                        x = "Longitude",
                        y = "Latitude",
                        fill = "Value"
                    ) +
                    ggplot2::theme_minimal() +
                    ggplot2::coord_equal()

                print(p)
            } else {
                # Fallback to base R plot if ggplot2 not available
                plot(
                    raster_data$lon,
                    raster_data$lat,
                    col = heat.colors(100)[as.numeric(cut(
                        raster_data$value,
                        breaks = 100
                    ))],
                    pch = 15,
                    cex = 0.5,
                    main = paste("Raster Plot:", model, "-", parameter),
                    xlab = "Longitude",
                    ylab = "Latitude"
                )
            }

            return(list(
                data = raster_data,
                model = model,
                parameter = parameter,
                reference_id = reference_id,
                raster_series_id = raster_series_id
            ))
        },
        error = function(e) {
            cat("Error in getRasterAndPlot:", e$message, "\n")
            return(NULL)
        }
    )
}

# Example usage function
plotExampleRaster <- function(con) {
    # Try to plot ERA5 land snow water equivalent
    result <- getRasterAndPlot(con, "ERA5_land", "snow water equivalent")

    if (!is.null(result)) {
        cat("Successfully plotted raster with", nrow(result$data), "pixels\n")
        cat("Value range:", range(result$data$value, na.rm = TRUE), "\n")
    }

    return(result)
}

# UI
ui <- fluidPage(
    titlePanel("Water Data Time Series"),

    # Main content with tabs
    tabsetPanel(
        # First tab - Time Series View
        tabPanel(
            "Time Series View",
            # Data selection controls accordion - moved to top
            tags$div(
                class = "panel-group",
                id = "accordion",
                style = "margin-bottom: 15px;",
                tags$div(
                    class = "panel panel-default",
                    tags$div(
                        class = "panel-heading",
                        tags$h4(
                            class = "panel-title",
                            tags$a(
                                `data-toggle` = "collapse",
                                `data-parent` = "#accordion",
                                href = "#collapseControls",
                                "Data Selection Controls"
                            )
                        )
                    ),
                    tags$div(
                        id = "collapseControls",
                        class = "panel-collapse collapse in",
                        tags$div(
                            class = "panel-body",
                            fluidRow(
                                column(
                                    6,
                                    selectizeInput(
                                        "timeseries_id",
                                        "Select Continuous Time Series:",
                                        choices = NULL,
                                        multiple = TRUE,
                                        options = list(
                                            placeholder = "Select continuous data...",
                                            maxItems = 10
                                        )
                                    )
                                ),
                                column(
                                    6,
                                    selectizeInput(
                                        "discrete_id",
                                        "Select Discrete Time Series:",
                                        choices = NULL,
                                        multiple = TRUE,
                                        options = list(
                                            placeholder = "Select discrete data...",
                                            maxItems = 10
                                        )
                                    )
                                )
                            ),
                            fluidRow(
                                column(
                                    4,
                                    checkboxInput(
                                        "show_seasonal_model",
                                        "Show Seasonal Model (Discrete Data)",
                                        value = FALSE
                                    )
                                ),
                                column(
                                    4,
                                    selectInput(
                                        "add_trace_type",
                                        "Add Trace:",
                                        choices = list(
                                            "Select data type..." = "",
                                            "Snow Pillow (Continuous)" = "continuous",
                                            "Snow Survey (Discrete)" = "discrete",
                                            "ERA5 (Gridded)" = "era5"
                                        ),
                                        selected = ""
                                    )
                                ),
                                column(
                                    4,
                                    actionButton(
                                        "deselect_all",
                                        "Deselect All",
                                        class = "btn-warning",
                                        style = "width: 100%;"
                                    )
                                )
                            )
                        )
                    )
                )
            ),

            fluidRow(
                column(
                    6,
                    tags$h4("Location Map"),
                    leafletOutput("locationMap2", height = "600px")
                ),
                column(
                    6,
                    tags$h4("Time Series Plot"),
                    plotlyOutput("timeseriesPlot", height = "600px")
                )
            )
        ),

        # Second tab - View by Date
        tabPanel(
            "View by Date",
            fluidRow(
                column(
                    8,
                    tags$h4("Location Map - Date View"),
                    leafletOutput("dateMap", height = "600px")
                ),
                column(
                    4,
                    tags$h4("Date Selection"),
                    dateInput(
                        "selected_date",
                        "Select Date:",
                        value = Sys.Date(),
                        min = "2000-01-01",
                        max = Sys.Date()
                    ),
                    br(),
                    tags$h5("Data Summary"),
                    verbatimTextOutput("date_summary")
                )
            )
        )
    )
) # Server
server <- function(input, output, session) {
    # Database connection
    config <- list(
        dbName = "aquacache",
        dbHost = Sys.getenv("aquacacheHost"),
        dbPort = Sys.getenv("aquacachePort"),
        dbUser = Sys.getenv("aquacacheAdminUser"),
        dbPass = Sys.getenv("aquacacheAdminPass")
    )

    con <- AquaConnect(
        name = config$dbName,
        host = config$dbHost,
        port = config$dbPort,
        user = config$dbUser,
        pass = config$dbPass
    )

    config <- list(
        dbName = "aquacache",
        dbHost = Sys.getenv("aquacacheHostProd"),
        dbPort = Sys.getenv("aquacachePortProd"),
        dbUser = Sys.getenv("aquacacheUserProd"),
        dbPass = Sys.getenv("aquacachePassProd")
    )

    # Second connection for ERA5 data
    con2 <- AquaConnect(
        name = config$dbName,
        host = config$dbHost,
        port = config$dbPort,
        user = config$dbUser,
        pass = config$dbPass
    )

    # Calculate initial map center from station centroids
    initial_center <- reactive({
        all_lats <- c()
        all_lngs <- c()

        if (nrow(md_continuous_df) > 0) {
            all_lats <- c(all_lats, md_continuous_df$latitude)
            all_lngs <- c(all_lngs, md_continuous_df$longitude)
        }

        if (nrow(md_discrete_df) > 0) {
            all_lats <- c(all_lats, md_discrete_df$latitude)
            all_lngs <- c(all_lngs, md_discrete_df$longitude)
        }

        if (length(all_lats) > 0) {
            list(lat = mean(all_lats), lng = mean(all_lngs), zoom = 6)
        } else {
            list(lat = 60, lng = -115, zoom = 5)
        }
    })

    # Reactive value to store map view state
    era5_click_data <- reactiveVal(NULL)

    # Get continuous metadata and convert to geodataframe
    md_continuous_df <- DBI::dbGetQuery(
        con,
        "SELECT 
            t.timeseries_id, 
            t.location_id,
            l.name,
            l.latitude,
            l.longitude
        FROM continuous.timeseries t
        JOIN public.locations l ON t.location_id = l.location_id
        WHERE t.parameter_id = (SELECT parameter_id FROM public.parameters 
                               WHERE param_name = 'snow water equivalent')"
    )

    md_continuous <- reactive({
        if (nrow(md_continuous_df) > 0) {
            sf::st_as_sf(
                md_continuous_df,
                coords = c("longitude", "latitude"),
                crs = 4326
            )
        } else {
            NULL
        }
    })

    # Get discrete metadata and convert to geodataframe
    md_discrete_df <- DBI::dbGetQuery(
        con,
        "SELECT DISTINCT
            s.location_id,
            l.latitude,
            l.longitude,
            l.name
        FROM discrete.results r 
        JOIN discrete.samples s ON r.sample_id = s.sample_id 
        JOIN public.locations l ON s.location_id = l.location_id
        WHERE r.parameter_id = (SELECT parameter_id FROM public.parameters 
                               WHERE param_name = 'snow water equivalent')"
    )

    md_discrete <- reactive({
        if (nrow(md_discrete_df) > 0) {
            sf::st_as_sf(
                md_discrete_df,
                coords = c("longitude", "latitude"),
                crs = 4326
            )
        } else {
            NULL
        }
    })

    # Update choices for selectInputs
    updateSelectizeInput(
        session,
        "timeseries_id",
        choices = if (nrow(md_continuous_df) > 0) {
            setNames(md_continuous_df$timeseries_id, md_continuous_df$name)
        } else {
            NULL
        }
    )

    updateSelectizeInput(
        session,
        "discrete_id",
        choices = if (nrow(md_discrete_df) > 0) {
            setNames(md_discrete_df$location_id, md_discrete_df$name)
        } else {
            NULL
        }
    )

    # Reactive data for continuous - now handles multiple selections
    continuous_data <- reactive({
        if (!is.null(input$timeseries_id) && length(input$timeseries_id) > 0) {
            all_data <- list()

            for (ts_id in input$timeseries_id) {
                ts <- DBI::dbGetQuery(
                    con,
                    paste0(
                        "SELECT datetime, value, '",
                        ts_id,
                        "' as timeseries_id FROM continuous.measurements_continuous WHERE timeseries_id = ",
                        ts_id
                    )
                )
                ts$datetime <- as.POSIXct(ts$datetime)

                # Add location name
                location_name <- md_continuous_df[
                    md_continuous_df$timeseries_id == as.numeric(ts_id),
                    "name"
                ]
                ts$location_name <- location_name

                all_data[[ts_id]] <- ts
            }

            return(all_data)
        }
        return(NULL)
    })

    # Reactive data for discrete - now handles multiple selections
    discrete_data <- reactive({
        if (!is.null(input$discrete_id) && length(input$discrete_id) > 0) {
            all_data <- list()

            for (loc_id in input$discrete_id) {
                ts <- DBI::dbGetQuery(
                    con,
                    paste0(
                        "SELECT s.datetime, r.result, '",
                        loc_id,
                        "' as location_id FROM discrete.samples s ",
                        "JOIN discrete.results r ON s.sample_id = r.sample_id ",
                        "WHERE s.location_id = ",
                        loc_id,
                        " AND r.parameter_id = (SELECT parameter_id FROM public.parameters ",
                        "WHERE param_name = 'snow water equivalent')"
                    )
                )
                ts$datetime <- as.POSIXct(ts$datetime)

                # Add location name
                location_name <- md_discrete_df[
                    md_discrete_df$location_id == as.numeric(loc_id),
                    "name"
                ]
                ts$location_name <- location_name

                all_data[[loc_id]] <- ts
            }

            return(all_data)
        }
        return(NULL)
    })

    # Leaflet map output with dynamic layer visibility
    output$locationMap <- renderLeaflet({
        cont_gdf <- md_continuous()
        disc_gdf <- md_discrete()

        map <- leaflet() %>%
            addTiles() %>%
            setView(lng = -115, lat = 60, zoom = 5)

        # Add continuous locations
        if (!is.null(cont_gdf)) {
            coords_cont <- sf::st_coordinates(cont_gdf)
            map <- map %>%
                addCircleMarkers(
                    lng = coords_cont[, 1],
                    lat = coords_cont[, 2],
                    radius = 6,
                    color = "blue",
                    fillColor = "lightblue",
                    fillOpacity = 0.7,
                    layerId = paste0("cont_", cont_gdf$timeseries_id),
                    label = cont_gdf$name,
                    popup = paste0(
                        "<b>",
                        cont_gdf$name,
                        "</b><br>",
                        "Type: Snow Pillow (Continuous)<br>",
                        "ID: ",
                        cont_gdf$timeseries_id
                    ),
                    group = "Continuous"
                )
        }

        # Add discrete locations with slight offset
        if (!is.null(disc_gdf)) {
            coords_disc <- sf::st_coordinates(disc_gdf)

            # Simple offset for all discrete markers to avoid overlap
            offset_lng <- coords_disc[, 1] + 0.001
            offset_lat <- coords_disc[, 2] + 0.001

            map <- map %>%
                addCircleMarkers(
                    lng = offset_lng,
                    lat = offset_lat,
                    radius = 6,
                    color = "red",
                    fillColor = "pink",
                    fillOpacity = 0.7,
                    layerId = paste0("disc_", disc_gdf$location_id),
                    label = disc_gdf$name,
                    popup = paste0(
                        "<b>",
                        disc_gdf$name,
                        "</b><br>",
                        "Type: Snow Survey (Discrete)<br>",
                        "ID: ",
                        disc_gdf$location_id,
                        "<br><i>(Slightly offset for visibility)</i>"
                    ),
                    group = "Discrete"
                )
        }

        return(map)
    })

    # Improved map output - renders once with station centroid view
    output$locationMap2 <- renderLeaflet({
        cont_gdf <- md_continuous()
        disc_gdf <- md_discrete()
        center <- initial_center()

        map <- leaflet() %>%
            addTiles() %>%
            setView(lng = center$lng, lat = center$lat, zoom = center$zoom)

        # Add all continuous locations (static rendering)
        if (!is.null(cont_gdf)) {
            coords_cont <- sf::st_coordinates(cont_gdf)
            map <- map %>%
                addCircleMarkers(
                    lng = coords_cont[, 1],
                    lat = coords_cont[, 2],
                    radius = 6,
                    color = "blue",
                    fillColor = "lightblue",
                    fillOpacity = 0.7,
                    layerId = paste0("cont_", cont_gdf$timeseries_id),
                    label = cont_gdf$name,
                    popup = paste0(
                        "<b>",
                        cont_gdf$name,
                        "</b><br>",
                        "Type: Snow Pillow (Continuous)<br>",
                        "ID: ",
                        cont_gdf$timeseries_id
                    ),
                    group = "Continuous"
                )
        }

        # Add all discrete locations (static rendering)
        if (!is.null(disc_gdf)) {
            coords_disc <- sf::st_coordinates(disc_gdf)
            offset_lng <- coords_disc[, 1] + 0.001
            offset_lat <- coords_disc[, 2] + 0.001

            map <- map %>%
                addCircleMarkers(
                    lng = offset_lng,
                    lat = offset_lat,
                    radius = 6,
                    color = "red",
                    fillColor = "pink",
                    fillOpacity = 0.7,
                    layerId = paste0("disc_", disc_gdf$location_id),
                    label = disc_gdf$name,
                    popup = paste0(
                        "<b>",
                        disc_gdf$name,
                        "</b><br>",
                        "Type: Snow Survey (Discrete)<br>",
                        "ID: ",
                        disc_gdf$location_id,
                        "<br><i>(Slightly offset for visibility)</i>"
                    ),
                    group = "Discrete"
                )
        }

        return(map)
    })

    # Observer to toggle marker visibility and styling based on trace type
    observeEvent(input$add_trace_type, {
        trace_type <- input$add_trace_type

        if (trace_type == "continuous") {
            # Show continuous, hide discrete
            leafletProxy("locationMap2") %>%
                showGroup("Continuous") %>%
                hideGroup("Discrete")
        } else if (trace_type == "discrete") {
            # Show discrete, hide continuous
            leafletProxy("locationMap2") %>%
                showGroup("Discrete") %>%
                hideGroup("Continuous")
        } else if (trace_type == "era5") {
            # Hide all markers for ERA5
            leafletProxy("locationMap2") %>%
                hideGroup("Continuous") %>%
                hideGroup("Discrete")
        } else {
            # Show all markers when no selection
            leafletProxy("locationMap2") %>%
                showGroup("Continuous") %>%
                showGroup("Discrete")
        }
    })

    # Function to calculate distance between two points (Haversine formula)
    calculate_distance <- function(lat1, lon1, lat2, lon2) {
        # Convert degrees to radians
        lat1_rad <- lat1 * pi / 180
        lon1_rad <- lon1 * pi / 180
        lat2_rad <- lat2 * pi / 180
        lon2_rad <- lon2 * pi / 180

        # Haversine formula
        dlat <- lat2_rad - lat1_rad
        dlon <- lon2_rad - lon1_rad
        a <- sin(dlat / 2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(dlon / 2)^2
        c <- 2 * atan2(sqrt(a), sqrt(1 - a))

        # Earth's radius in km
        R <- 6371
        distance <- R * c

        return(distance)
    }

    # Function to fit seasonal model with autoregressive components to discrete data
    fit_seasonal_model <- function(data) {
        if (is.null(data) || nrow(data) == 0) {
            return(NULL)
        }

        tryCatch(
            {
                # Prepare data
                df <- data.frame(
                    date = as.Date(data$datetime),
                    value = as.numeric(data$result),
                    location_name = data$location_name[1]
                )

                # Remove NA values and sort by date
                df <- df[!is.na(df$value), ]
                df <- df[order(df$date), ]
                if (nrow(df) < 4) {
                    return(NULL)
                } # Need minimum data points

                # Create time variables
                df$year <- as.numeric(format(df$date, "%Y"))
                df$doy <- as.numeric(format(df$date, "%j")) # Day of year

                # Create seasonal features using day of year
                df$sin_doy <- sin(2 * pi * df$doy / 365.25)
                df$cos_doy <- cos(2 * pi * df$doy / 365.25)

                # Try to create autoregressive features if enough data
                use_ar <- FALSE
                if (nrow(df) >= 6) {
                    # Create lag features safely
                    df$lag1 <- c(NA, head(df$value, -1))
                    df$lag2 <- c(NA, NA, head(df$value, -2))

                    # Remove rows with NA lags for model fitting
                    df_ar <- df[complete.cases(df), ]

                    if (nrow(df_ar) >= 4) {
                        # Try AR model
                        model <- tryCatch(
                            {
                                lm(
                                    value ~
                                        year + sin_doy + cos_doy + lag1 + lag2,
                                    data = df_ar
                                )
                            },
                            error = function(e) NULL
                        )

                        if (!is.null(model)) {
                            use_ar <- TRUE
                            fitted_data <- df_ar
                        }
                    }
                }

                # Fallback to simple model if AR failed
                if (!use_ar) {
                    model <- lm(value ~ year + sin_doy + cos_doy, data = df)
                    fitted_data <- df
                }

                # Create daily interpolation grid
                date_range <- seq(
                    from = min(df$date),
                    to = max(df$date),
                    by = "day"
                )

                pred_df <- data.frame(
                    date = date_range,
                    year = as.numeric(format(date_range, "%Y")),
                    doy = as.numeric(format(date_range, "%j"))
                )

                # Create seasonal features for prediction
                pred_df$sin_doy <- sin(2 * pi * pred_df$doy / 365.25)
                pred_df$cos_doy <- cos(2 * pi * pred_df$doy / 365.25)

                if (use_ar) {
                    # For AR model, predict with simple approach
                    # Use mean values for missing lags in prediction
                    mean_val <- mean(df$value)
                    pred_df$lag1 <- mean_val
                    pred_df$lag2 <- mean_val

                    raw_predictions <- predict(model, newdata = pred_df)
                    pred_df$predicted <- pmax(0, raw_predictions)
                } else {
                    # Simple model without AR terms
                    raw_predictions <- predict(model, newdata = pred_df)
                    pred_df$predicted <- pmax(0, raw_predictions)
                }

                # Add metadata
                pred_df$location_name <- df$location_name[1]
                pred_df$datetime <- as.POSIXct(pred_df$date)

                # Calculate model performance metrics
                fitted_values <- pmax(0, fitted(model))
                observed_values <- fitted_data$value

                r_squared <- cor(observed_values, fitted_values)^2
                rmse <- sqrt(mean((observed_values - fitted_values)^2))

                # Calculate Nash-Sutcliffe Efficiency (NSE)
                mean_observed <- mean(observed_values)
                nse <- 1 -
                    sum((observed_values - fitted_values)^2) /
                        sum((observed_values - mean_observed)^2)

                return(list(
                    model = model,
                    predictions = pred_df,
                    original_data = df,
                    r_squared = r_squared,
                    rmse = rmse,
                    nse = nse,
                    use_ar = use_ar,
                    n_points = nrow(fitted_data),
                    date_range = paste(min(df$date), "to", max(df$date))
                ))
            },
            error = function(e) {
                cat("Error fitting seasonal model:", e$message, "\n")
                return(NULL)
            }
        )
    }

    # Reactive data for seasonal models
    seasonal_models <- reactive({
        if (!input$show_seasonal_model) {
            return(NULL)
        }

        disc_data <- discrete_data()
        if (is.null(disc_data)) {
            return(NULL)
        }

        models <- list()
        for (i in seq_along(disc_data)) {
            location_id <- names(disc_data)[i]
            model_result <- fit_seasonal_model(disc_data[[i]])
            if (!is.null(model_result)) {
                models[[location_id]] <- model_result
            }
        }

        return(if (length(models) > 0) models else NULL)
    })

    # Handle map clicks (simplified for trace type selection)
    observeEvent(input$locationMap2_click, {
        click <- input$locationMap2_click
        trace_type <- input$add_trace_type

        # Skip if this is a marker click (has an id) or no trace type selected
        if (!is.null(click$id) || is.null(trace_type) || trace_type == "") {
            return()
        }

        click_lat <- click$lat
        click_lng <- click$lng

        # Handle ERA5 data retrieval for gridded data
        if (trace_type == "era5") {
            # Validate coordinates
            if (
                click_lng < -180 ||
                    click_lng > 180 ||
                    click_lat < -90 ||
                    click_lat > 90
            ) {
                showNotification(
                    paste0(
                        "Invalid coordinates: (",
                        round(click_lng, 4),
                        ", ",
                        round(click_lat, 4),
                        ")"
                    ),
                    type = "error",
                    duration = 3
                )
                return()
            }

            # Add visual feedback for ERA5 click
            leafletProxy("locationMap2") %>%
                clearGroup("click_feedback") %>%
                addCircleMarkers(
                    lng = click_lng,
                    lat = click_lat,
                    radius = 8,
                    color = "green",
                    fillColor = "green",
                    fillOpacity = 0.8,
                    weight = 3,
                    group = "click_feedback",
                    popup = paste0(
                        "ERA5 Click Point<br>Lat: ",
                        round(click_lat, 4),
                        "°<br>Lon: ",
                        round(click_lng, 4),
                        "°"
                    )
                )

            # Retrieve ERA5 data
            tryCatch(
                {
                    cat(
                        "Retrieving ERA5 data at: Lon =",
                        click_lng,
                        ", Lat =",
                        click_lat,
                        "\n"
                    )

                    result <- getRasterSeriesAtPoint(
                        con2,
                        model = "ERA5_land",
                        parameter = "snow water equivalent",
                        lon = click_lng,
                        lat = click_lat
                    )

                    era5_data <- result$data
                    era5_data$value <- era5_data$value * 1000 # Convert m to mm

                    if (!is.null(era5_data) && nrow(era5_data) > 0) {
                        era5_click_data(era5_data)
                        showNotification(
                            paste0(
                                "Retrieved ",
                                nrow(era5_data),
                                " ERA5 land SWE data points"
                            ),
                            type = "message",
                            duration = 4
                        )
                    } else {
                        era5_click_data(NULL)
                        showNotification(
                            "No ERA5 land SWE data available at this location",
                            type = "warning",
                            duration = 4
                        )
                    }
                },
                error = function(e) {
                    showNotification(
                        paste0("Error retrieving ERA5 data: ", e$message),
                        type = "error",
                        duration = 5
                    )
                }
            )
        } else {
            # For continuous and discrete, show message about clicking markers
            showNotification(
                "Click directly on stations to add them to your selection",
                type = "message",
                duration = 3
            )
        }
    })

    # Handle direct marker clicks - only for appropriate trace types
    observeEvent(input$locationMap2_marker_click, {
        marker_click <- input$locationMap2_marker_click
        trace_type <- input$add_trace_type

        if (!is.null(marker_click$id)) {
            # Parse the marker ID to determine type and ID
            if (
                startsWith(marker_click$id, "cont_") &&
                    trace_type == "continuous"
            ) {
                # Continuous marker clicked and continuous mode selected
                timeseries_id <- as.numeric(sub("cont_", "", marker_click$id))
                current_selection <- input$timeseries_id
                new_selection <- unique(c(current_selection, timeseries_id))
                updateSelectizeInput(
                    session,
                    "timeseries_id",
                    selected = new_selection
                )

                cont_name <- md_continuous_df[
                    md_continuous_df$timeseries_id == timeseries_id,
                    "name"
                ]
                showNotification(
                    paste("Added snow pillow:", cont_name),
                    type = "message",
                    duration = 2
                )
            } else if (
                startsWith(marker_click$id, "disc_") && trace_type == "discrete"
            ) {
                # Discrete marker clicked and discrete mode selected
                location_id <- as.numeric(sub("disc_", "", marker_click$id))
                current_selection <- input$discrete_id
                new_selection <- unique(c(current_selection, location_id))
                updateSelectizeInput(
                    session,
                    "discrete_id",
                    selected = new_selection
                )

                disc_name <- md_discrete_df[
                    md_discrete_df$location_id == location_id,
                    "name"
                ]
                showNotification(
                    paste("Added snow survey:", disc_name),
                    type = "message",
                    duration = 2
                )
            } else if (trace_type == "" || is.null(trace_type)) {
                showNotification(
                    "Please select a trace type first from the 'Add Trace' dropdown",
                    type = "warning",
                    duration = 3
                )
            } else {
                showNotification(
                    paste0(
                        "Switch to '",
                        if (startsWith(marker_click$id, "cont_")) {
                            "Snow Pillow (Continuous)"
                        } else {
                            "Snow Survey (Discrete)"
                        },
                        "' mode to add this station"
                    ),
                    type = "warning",
                    duration = 3
                )
            }
        }
    })

    # Handle deselect all button
    observeEvent(input$deselect_all, {
        updateSelectizeInput(session, "timeseries_id", selected = character(0))
        updateSelectizeInput(session, "discrete_id", selected = character(0))

        # Clear selected markers and click feedback on map
        leafletProxy("locationMap2") %>%
            clearGroup("selected") %>%
            clearGroup("click_feedback")

        # Clear ERA5 data
        era5_click_data(NULL)

        showNotification(
            "All selections cleared",
            type = "message",
            duration = 2
        )
    }) # Interactive plot output - now handles multiple series with individual traces
    output$timeseriesPlot <- renderPlotly({
        cont_data <- continuous_data()
        disc_data <- discrete_data()
        seasonal_data <- seasonal_models()
        era5_data <- era5_click_data()

        p <- plot_ly()

        # Add continuous data as separate traces for each location
        if (!is.null(cont_data)) {
            for (i in seq_along(cont_data)) {
                data <- cont_data[[i]]
                if (nrow(data) > 0) {
                    location_name <- data$location_name[1]
                    p <- p %>%
                        add_trace(
                            data = data,
                            x = ~datetime,
                            y = ~value,
                            type = "scatter",
                            mode = "lines",
                            name = paste("Continuous -", location_name),
                            line = list(width = 2),
                            hovertemplate = paste0(
                                "<b>Continuous - ",
                                location_name,
                                "</b><br>",
                                "Date: %{x}<br>",
                                "Value: %{y}<extra></extra>"
                            )
                        )
                }
            }
        }

        # Add discrete data as separate traces for each location
        if (!is.null(disc_data)) {
            for (i in seq_along(disc_data)) {
                data <- disc_data[[i]]
                if (nrow(data) > 0) {
                    location_name <- data$location_name[1]
                    p <- p %>%
                        add_trace(
                            data = data,
                            x = ~datetime,
                            y = ~result,
                            type = "scatter",
                            mode = "markers",
                            name = paste("Discrete -", location_name),
                            marker = list(size = 8),
                            hovertemplate = paste0(
                                "<b>Discrete - ",
                                location_name,
                                "</b><br>",
                                "Date: %{x}<br>",
                                "Value: %{y}<extra></extra>"
                            )
                        )
                }
            }
        }

        # Add seasonal model predictions if enabled (without confidence intervals)
        if (!is.null(seasonal_data) && input$show_seasonal_model) {
            for (i in seq_along(seasonal_data)) {
                model_result <- seasonal_data[[i]]
                pred_data <- model_result$predictions
                location_name <- pred_data$location_name[1]

                # Add model prediction line with dotted style and NSE in legend
                p <- p %>%
                    add_trace(
                        data = pred_data,
                        x = ~datetime,
                        y = ~predicted,
                        type = "scatter",
                        mode = "lines",
                        name = paste0(
                            "Model - ",
                            location_name,
                            " (NSE: ",
                            round(model_result$nse, 3),
                            ")"
                        ),
                        line = list(dash = "dot", width = 2),
                        hovertemplate = paste0(
                            "<b>Seasonal Model - ",
                            location_name,
                            "</b><br>",
                            "Date: %{x}<br>",
                            "Predicted: %{y:.2f}<br>",
                            "NSE: ",
                            round(model_result$nse, 3),
                            "<br>",
                            "R²: ",
                            round(model_result$r_squared, 3),
                            "<br>",
                            "RMSE: ",
                            round(model_result$rmse, 2),
                            "<extra></extra>"
                        )
                    )
            }
        }

        # Add ERA5 data if available
        if (!is.null(era5_data)) {
            # Ensure datetime column exists and is properly formatted
            if ("datetime" %in% names(era5_data)) {
                era5_data$datetime <- as.POSIXct(era5_data$datetime)
            } else if ("date" %in% names(era5_data)) {
                era5_data$datetime <- as.POSIXct(era5_data$date)
            }

            # Determine the value column name
            value_col <- "value"
            if ("snow_water_equivalent" %in% names(era5_data)) {
                value_col <- "snow_water_equivalent"
            } else if ("swe" %in% names(era5_data)) {
                value_col <- "swe"
            }

            if (
                "datetime" %in%
                    names(era5_data) &&
                    value_col %in% names(era5_data)
            ) {
                p <- p %>%
                    add_trace(
                        data = era5_data,
                        x = ~datetime,
                        y = era5_data[[value_col]],
                        type = "scatter",
                        mode = "lines",
                        name = "ERA5 Land SWE",
                        line = list(
                            color = "green",
                            width = 2,
                            dash = "dashdot"
                        ),
                        hovertemplate = paste0(
                            "<b>ERA5 Land SWE</b><br>",
                            "Date: %{x}<br>",
                            "Value: %{y:.3f}<br>",
                            "<extra></extra>"
                        )
                    )
            }
        }

        # Configure layout
        p <- p %>%
            layout(
                title = "Time Series Plot - Multiple Locations",
                xaxis = list(title = "Date"),
                yaxis = list(title = "Snow Water Equivalent"),
                hovermode = "closest",
                legend = list(
                    orientation = "h",
                    x = 0,
                    y = -0.2,
                    xanchor = "left",
                    yanchor = "top"
                ),
                margin = list(b = 100)
            )

        return(p)
    })

    # Date-based map output
    output$dateMap <- renderLeaflet({
        cont_gdf <- md_continuous()
        disc_gdf <- md_discrete()

        map <- leaflet() %>%
            addTiles() %>%
            setView(lng = -115, lat = 60, zoom = 5)

        # Add continuous locations (same as main map)
        if (!is.null(cont_gdf)) {
            coords_cont <- sf::st_coordinates(cont_gdf)
            map <- map %>%
                addCircleMarkers(
                    lng = coords_cont[, 1],
                    lat = coords_cont[, 2],
                    radius = 6,
                    color = "blue",
                    fillColor = "lightblue",
                    fillOpacity = 0.7,
                    layerId = paste0("date_cont_", cont_gdf$timeseries_id),
                    label = cont_gdf$name,
                    popup = paste0(
                        "<b>",
                        cont_gdf$name,
                        "</b><br>",
                        "Type: Continuous<br>",
                        "ID: ",
                        cont_gdf$timeseries_id
                    ),
                    group = "Continuous"
                )
        }

        # Add discrete locations (same as main map)
        if (!is.null(disc_gdf)) {
            coords_disc <- sf::st_coordinates(disc_gdf)

            # Simple offset for all discrete markers to avoid overlap
            offset_lng <- coords_disc[, 1] + 0.001
            offset_lat <- coords_disc[, 2] + 0.001

            map <- map %>%
                addCircleMarkers(
                    lng = offset_lng,
                    lat = offset_lat,
                    radius = 6,
                    color = "red",
                    fillColor = "pink",
                    fillOpacity = 0.7,
                    layerId = paste0("date_disc_", disc_gdf$location_id),
                    label = disc_gdf$name,
                    popup = paste0(
                        "<b>",
                        disc_gdf$name,
                        "</b><br>",
                        "Type: Discrete<br>",
                        "ID: ",
                        disc_gdf$location_id,
                        "<br><i>(Slightly offset for visibility)</i>"
                    ),
                    group = "Discrete"
                )
        }

        # Add layer control
        map <- map %>%
            addLayersControl(
                overlayGroups = c("Continuous", "Discrete"),
                options = layersControlOptions(collapsed = FALSE)
            )

        return(map)
    })

    # Date summary output
    output$date_summary <- renderText({
        selected_date <- input$selected_date
        if (is.null(selected_date)) {
            return("No date selected")
        }

        # Count continuous data points for selected date
        continuous_count <- 0
        if (nrow(md_continuous_df) > 0) {
            for (i in 1:nrow(md_continuous_df)) {
                ts_id <- md_continuous_df$timeseries_id[i]
                data_check <- DBI::dbGetQuery(
                    con,
                    paste0(
                        "SELECT COUNT(*) as count FROM continuous.measurements_continuous ",
                        "WHERE timeseries_id = ",
                        ts_id,
                        " ",
                        "AND DATE(datetime) = '",
                        selected_date,
                        "'"
                    )
                )
                continuous_count <- continuous_count + data_check$count
            }
        }

        # Count discrete data points for selected date
        discrete_count <- 0
        if (nrow(md_discrete_df) > 0) {
            for (i in 1:nrow(md_discrete_df)) {
                loc_id <- md_discrete_df$location_id[i]
                data_check <- DBI::dbGetQuery(
                    con,
                    paste0(
                        "SELECT COUNT(*) as count FROM discrete.samples s ",
                        "JOIN discrete.results r ON s.sample_id = r.sample_id ",
                        "WHERE s.location_id = ",
                        loc_id,
                        " ",
                        "AND DATE(s.datetime) = '",
                        selected_date,
                        "' ",
                        "AND r.parameter_id = (SELECT parameter_id FROM public.parameters ",
                        "WHERE param_name = 'snow water equivalent')"
                    )
                )
                discrete_count <- discrete_count + data_check$count
            }
        }

        paste0(
            "Date: ",
            format(selected_date, "%Y-%m-%d"),
            "\n",
            "Continuous measurements: ",
            continuous_count,
            "\n",
            "Discrete measurements: ",
            discrete_count,
            "\n",
            "Total measurements: ",
            continuous_count + discrete_count
        )
    })

    # Cleanup on session end
    session$onSessionEnded(function() {
        DBI::dbDisconnect(con)
        DBI::dbDisconnect(con2)
    })
}

# Run the app
shinyApp(ui = ui, server = server)
