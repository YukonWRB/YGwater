# =============================================================================
# Snow Water Equivalent (SWE) Basin Visualization - Shiny App
# =============================================================================
# This Shiny app creates an interactive leaflet map showing:
# - SWE basins colored by weighted SWE values from discrete stations
# - Continuous SWE monitoring stations colored by relative change
# - Discrete SWE monitoring stations colored by relative change
# - Communities, roads, and geographic boundaries
#
# The map displays snow water equivalent data as a percentage of historic normal
# for a specific date (year and day-of-year). Basin values are calculated using
# weighted averages from discrete snow course stations.
#
# Data Sources:
# - SWE basins: Local shapefile (swe_basins_ExportFeatures.shp)
# - Stations/Communities/Roads: PostgreSQL spatial database (AquaCache)
# - Timeseries data: Continuous and discrete measurements database
# - Snowcourse factors: CSV file with basin weighting factors
# =============================================================================
# Load utility functions and color palettes
source("R/SWE_maputils.R")
# =============================================================================
# DATA LOADING AND PROCESSING
# =============================================================================

# Load all base data once at startup

# =============================================================================
# SHINY APP UI
# =============================================================================
mapSnowbullUI <- function(id) {
  cat("DEBUG: Creating mapSnowbullUI with id:", id, "\n")

  ns <- shiny::NS(id)
  shiny::fluidPage(
    # Sidebar + main contentexpandLimits
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # Controls section
        div(
          id = ns("controls_panel"),
          # Year select (dropdown)
          div(
            style = "margin-bottom: 15px;",
            tags$label(
              "Year:",
              style = "display: block; margin-bottom: 5px;"
            ),
            selectInput(
              ns("year"),
              label = NULL,
              choices = as.character(2025:2000), # 2025 at top, down to 2000
              selected = "2025",
              width = "100%"
            )
          ),
          # Month select
          div(
            style = "margin-bottom: 15px;",
            tags$label(
              "Month:",
              style = "display: block; margin-bottom: 5px;"
            ),
            selectInput(
              ns("month"),
              label = NULL,
              choices = setNames(
                as.character(c(2, 3, 4, 5)),
                c("February", "March", "April", "May")
              ),
              selected = "3",
              width = "100%"
            )
          ),
          # Value type radio
          div(
            style = "margin-bottom: 10px;",
            tags$label(
              "Values:",
              style = "display: block; margin-bottom: 5px;"
            ),
            radioButtons(
              ns("value_type"),
              label = NULL,
              choices = list(
                "% of Normal" = "relative_swe",
                "Absolute (mm)" = "swe"
              ),
              selected = "relative_swe",
              inline = FALSE
            )
          )
        ),
        width = 3
      ),
      shiny::mainPanel(
        leaflet::leafletOutput(ns("map"), height = "100vh"),
        width = 9
      )
    ),
    # JavaScript for plot generation
    tags$script(HTML(sprintf(
      "function generatePlot(type, stationId, stationName) {
        console.log('DEBUG: generatePlot called with:', type, stationId, stationName);
        var popup = document.querySelector('.leaflet-popup-content');
        if (popup) {
          // Show loading and widen only this popup immediately on button press
          popup.innerHTML = '<div style=\"text-align: center; padding: 20px;\"><b>' + 
            stationName + '</b><br><br>Loading plot...<br>' + 
            '<div style=\"margin-top: 10px;\">⏳</div></div>';
          var wrapper = popup.closest('.leaflet-popup-content-wrapper');
          if (wrapper) wrapper.style.maxWidth = '720px';
          popup.style.width = '660px';
        }
        Shiny.setInputValue('%s', {
          type: type,
          station_id: stationId,
          station_name: stationName,
          timestamp: Date.now()
        });
      }",
      ns("generate_plot")
    ))),
    # Remove global wide popup CSS (keep popups narrow by default)
    # tags$head(tags$style(HTML(
    #   "
    #   .leaflet-popup-content-wrapper { max-width: 720px !important; }
    #   .leaflet-popup-content { width: 660px !important; }
    # "
    # ))),
    # Receive plot HTML from server; widen only the current popup
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('updatePopup', function(message) {
         console.log('DEBUG: updatePopup handler called');
         var popup = document.querySelector('.leaflet-popup-content');
         if (popup && message && message.html) {
           popup.innerHTML = message.html;
           var wrapper = popup.closest('.leaflet-popup-content-wrapper');
           if (wrapper) wrapper.style.maxWidth = '720px';
           popup.style.width = '660px';
         }
       });"
    ))
  )
}

mapSnowbull <- function(id, language) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    print(paste("DEBUG: Inside moduleServer, namespace:", ns("test")))

    con <- session$userData$AquaCache
    print(paste("DEBUG: Database connection established:", !is.null(con)))

    print("DEBUG: Loading base data...")
    base_data <- load_base_data(con)
    print("Base data loaded successfully")
    print(sprintf("Number of pillows: %d", nrow(base_data$pillows$metadata)))
    print(sprintf("Number of surveys: %d", nrow(base_data$surveys$metadata)))
    print(sprintf("Number of basins: %d", nrow(base_data$basins$metadata)))

    print("DEBUG: Initializing visualization parameters...")
    viz_params <- initialize_visualization_parameters()
    print(paste(
      "DEBUG: Viz params initialized with",
      length(viz_params$relative_colors),
      "colors"
    ))

    # Render UI elements ####
    # (placeholder for now)
    # ---- End color palette functions ----
    # Reactive expression for selected date display
    output$selected_date <- shiny::renderText({
      cat("DEBUG: Rendering selected_date\n")
      shiny::req(input$year, input$month)
      # Convert DOY from character to numeric
      date_obj <- get_datetime(input$year, input$month)
      format(date_obj, "%B %d, %Y")
    })

    # Reactive data processing based on user inputs

    # Generic function to get processed data for a given year/month

    processed_data <- reactive({
      print("DEBUG: processed_data reactive triggered")
      shiny::req(input$year, input$month)
      print(sprintf(
        "DEBUG: Processing data for year=%s, month=%s",
        input$year,
        input$month
      ))

      result <- get_processed_data(
        year = input$year,
        month = input$month,
        base_data = base_data,
        shiny = TRUE
      )

      print("DEBUG: processed_data reactive completed")
      return(result)
    })

    # Initialize map - This creates the map as a server output
    output$map <- leaflet::renderLeaflet({
      print("DEBUG: Rendering initial leaflet map")

      # Use Yukon boundary if available, otherwise fallback
      if (!is.null(base_data$shapefiles$yukon)) {
        bbox <- sf::st_bbox(base_data$shapefiles$yukon)
        print("DEBUG: Using Yukon boundary for initial map bounds")
      } else {
        bbox <- c(xmin = -141, ymin = 60, xmax = -123, ymax = 69.6)
        print("DEBUG: Using fallback bounds for initial map")
      }

      print("DEBUG: Getting initial processed data for March 2025")
      initial_data <- get_processed_data(
        2025,
        3,
        base_data = base_data,
        shiny = TRUE
      )

      value_col <- "relative_swe"
      initial_data$swe_at_basins$value_to_show <- initial_data$swe_at_basins[[
        value_col
      ]]
      initial_data$swe_at_surveys$value_to_show <- initial_data$swe_at_surveys[[
        value_col
      ]]
      initial_data$swe_at_pillows$value_to_show <- initial_data$swe_at_pillows[[
        value_col
      ]]
      pal_domain <- c(
        initial_data$swe_at_basins$value_to_show,
        initial_data$swe_at_surveys$value_to_show,
        initial_data$swe_at_pillows$value_to_show
      )

      print("DEBUG: Creating initial color palette")
      swe_pal <- leaflet::colorBin(
        palette = viz_params$relative_colors,
        bins = viz_params$relative_bins,
        domain = pal_domain,
        na.color = "gray"
      )

      print(sprintf(
        "DEBUG: Building leaflet map with %d basins",
        nrow(initial_data$swe_at_basins)
      ))

      leaflet::leaflet(
        options = leaflet::leafletOptions(
          zoomDelta = 0.5,
          zoomSnap = 0.25,
          wheelPxPerZoomLevel = 120
        )
      ) %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldTerrain,
          group = "Topographic"
        ) %>%
        leaflet::fitBounds(
          as.numeric(bbox["xmin"]),
          as.numeric(bbox["ymin"]),
          as.numeric(bbox["xmax"]),
          as.numeric(bbox["ymax"])
        ) %>%
        leaflet::addPolygons(
          data = initial_data$swe_at_basins,
          fillColor = ~ swe_pal(value_to_show),
          color = "white",
          weight = 2,
          opacity = 1,
          fillOpacity = 0.7,
          label = ~ lapply(annotation, htmltools::HTML),
          popup = ~ lapply(popup_content, htmltools::HTML),
          popupOptions = leaflet::popupOptions(
            maxWidth = 320,
            closeButton = TRUE,
            autoPan = TRUE
          ),
          group = "Basins averages"
          # highlightOptions removed
        ) %>%
        leaflet::addLabelOnlyMarkers(
          data = initial_data$swe_at_basins,
          lng = initial_data$swe_at_basins$x,
          lat = initial_data$swe_at_basins$y,
          label = ~ lapply(
            initial_data$swe_at_basins$annotation,
            htmltools::HTML
          ),
          labelOptions = leaflet::labelOptions(
            noHide = TRUE,
            direction = "center",
            textOnly = TRUE,
            style = list(
              "color" = "#222",
              "font-size" = "14px",
              "font-weight" = "bold",
              "text-shadow" = "2px 2px 4px #fff"
            )
          ),
          group = "Basins averages"
        ) %>%
        {
          if (!is.null(base_data$shapefiles$roads)) {
            print(sprintf(
              "DEBUG: Adding %d roads to map",
              nrow(base_data$shapefiles$roads)
            ))
            leaflet::addPolylines(
              .,
              data = base_data$shapefiles$roads,
              color = "#8B0000",
              weight = 2,
              opacity = 0.8,
              group = "Roads",
              label = ~ lapply(as.character(feature_name), htmltools::HTML)
            )
          } else {
            print("DEBUG: No roads data available")
            .
          }
        } %>%
        {
          if (!is.null(base_data$shapefiles$yukon)) {
            print("DEBUG: Adding Yukon boundary")
            leaflet::addPolygons(
              .,
              data = base_data$shapefiles$yukon,
              color = "#222222",
              weight = 3,
              fill = FALSE,
              group = "Boundary"
            )
          } else {
            print("DEBUG: No Yukon boundary data available")
            .
          }
        } %>%
        leaflet::addCircleMarkers(
          data = initial_data$swe_at_surveys,
          radius = 8,
          color = "black",
          fillColor = ~ swe_pal(value_to_show),
          weight = 2,
          opacity = 1,
          fillOpacity = 1,
          label = ~ lapply(paste0(name, "<br>", location), htmltools::HTML),
          popup = ~ lapply(popup_content, htmltools::HTML),
          popupOptions = leaflet::popupOptions(
            maxWidth = 320,
            closeButton = TRUE,
            autoPan = TRUE
          ),
          group = "Snow surveys (discrete)",
          highlightOptions = leaflet::highlightOptions(
            color = "#00FFFF",
            weight = 5,
            opacity = 1,
            bringToFront = TRUE,
            sendToBack = FALSE
          )
        ) %>%
        leaflet::addCircleMarkers(
          data = initial_data$swe_at_pillows,
          radius = 8,
          color = "blue",
          fillColor = ~ swe_pal(value_to_show),
          weight = 2,
          opacity = 1,
          fillOpacity = 1,
          label = ~ lapply(paste0(name, "<br>", location), htmltools::HTML),
          popup = ~ lapply(popup_content, htmltools::HTML),
          popupOptions = leaflet::popupOptions(
            maxWidth = 320,
            closeButton = TRUE,
            autoPan = TRUE
          ),
          group = "Snow pillows (continuous)",
          highlightOptions = leaflet::highlightOptions(
            color = "#00FFFF",
            weight = 5,
            opacity = 1,
            bringToFront = TRUE,
            sendToBack = FALSE
          )
        ) %>%
        {
          if (!is.null(base_data$shapefiles$communities)) {
            print(sprintf(
              "DEBUG: Adding %d communities",
              nrow(base_data$shapefiles$communities)
            ))
            comm <- base_data$shapefiles$communities
            communities_icon_svg <- "data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='16' height='16' viewBox='0 0 16 16'><polygon points='8,0 16,8 8,16 0,8' fill='black' stroke='white' stroke-width='2'/></svg>"
            . <- leaflet::addMarkers(
              .,
              data = comm,
              icon = leaflet::icons(
                iconUrl = communities_icon_svg,
                iconWidth = 16,
                iconHeight = 16
              ),
              label = ~ lapply(annotation, htmltools::HTML),
              popup = ~ lapply(popup, htmltools::HTML),
              popupOptions = leaflet::popupOptions(
                maxWidth = 320,
                closeButton = TRUE,
                autoPan = TRUE
              ),
              group = "Communities"
            )
            . <- leaflet::addLabelOnlyMarkers(
              .,
              data = comm,
              lng = comm$x_adjusted,
              lat = comm$y_adjusted,
              label = ~ lapply(comm$annotation, htmltools::HTML),
              labelOptions = leaflet::labelOptions(
                noHide = TRUE,
                direction = "top",
                textOnly = TRUE,
                style = list(
                  "color" = "#222",
                  "font-size" = "13px",
                  "font-weight" = "bold",
                  "text-shadow" = "1px 1px 2px #fff"
                )
              ),
              group = "Communities"
            )
            .
          } else {
            print("DEBUG: No communities data available")
            .
          }
        } %>%
        leaflet::addLayersControl(
          baseGroups = "Topographic",
          overlayGroups = c(
            "Boundary",
            "Roads",
            "Communities",
            "Basins averages",
            "Snow surveys (discrete)",
            "Snow pillows (continuous)"
          ),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )
    })

    # Helper: add SWE layers (basins, surveys, pillows)
    update_swe_layers <- function() {
      cat("DEBUG: Starting update_swe_layers function\n")

      # Use default values if input$year or input$month are NULL (initial load)
      year <- if (!is.null(input$year)) input$year else "2025"
      month <- if (!is.null(input$month)) input$month else "3"
      value_type <- if (!is.null(input$value_type)) {
        input$value_type
      } else {
        "relative_swe"
      }

      cat(sprintf(
        "DEBUG: Updating layers for year=%s, month=%s, value_type=%s\n",
        year,
        month,
        value_type
      ))

      data <- processed_data()
      cat("DEBUG: Got processed data in update_swe_layers\n")

      # Select value column based on input$value_type
      value_col <- if (value_type == "relative_swe") {
        "relative_swe"
      } else {
        "swe"
      }

      # Precompute a unified column so leaflet formulas can access it
      data$swe_at_basins$value_to_show <- data$swe_at_basins[[value_col]]
      data$swe_at_surveys$value_to_show <- data$swe_at_surveys[[value_col]]
      data$swe_at_pillows$value_to_show <- data$swe_at_pillows[[value_col]]

      # Build palette domain from all layers' values
      pal_domain <- c(
        data$swe_at_basins$value_to_show,
        data$swe_at_surveys$value_to_show,
        data$swe_at_pillows$value_to_show
      )

      cat("DEBUG: Creating color palette for", length(pal_domain), "values\n")

      # Create appropriate color palette based on value type
      swe_pal <- if (value_type == "relative_swe") {
        leaflet::colorBin(
          palette = viz_params$relative_colors,
          bins = viz_params$relative_bins,
          domain = pal_domain,
          na.color = "gray"
        )
      } else {
        leaflet::colorBin(
          palette = viz_params$absolute_colors,
          bins = viz_params$absolute_bins,
          domain = pal_domain,
          na.color = "gray"
        )
      }

      cat("DEBUG: Updating map layers via leafletProxy\n")

      # Add SWE layers first (so they are underneath)
      leaflet::leafletProxy("map", session = session) %>%
        leaflet::clearGroup("Basins averages") %>%
        leaflet::clearGroup("Snow surveys (discrete)") %>%
        leaflet::clearGroup("Snow pillows (continuous)") %>%
        # --- SWE layers (underneath) ---
        leaflet::addPolygons(
          data = data$swe_at_basins,
          fillColor = ~ swe_pal(value_to_show),
          color = "white",
          weight = 2,
          opacity = 1,
          fillOpacity = 0.7,
          label = ~ lapply(annotation, htmltools::HTML),
          popup = ~ lapply(popup_content, htmltools::HTML),
          popupOptions = leaflet::popupOptions(
            maxWidth = 320,
            closeButton = TRUE,
            autoPan = TRUE
          ),
          group = "Basins averages"
        ) %>%
        leaflet::addLabelOnlyMarkers(
          data = data$swe_at_basins,
          lng = data$swe_at_basins$x,
          lat = data$swe_at_basins$y,
          label = ~ lapply(data$swe_at_basins$annotation, htmltools::HTML),
          labelOptions = leaflet::labelOptions(
            noHide = TRUE,
            direction = "center",
            textOnly = TRUE,
            style = list(
              "color" = "#222",
              "font-size" = "11px",
              "font-weight" = "bold",
              "text-shadow" = "1px 1px 2px #fff"
            )
          ),
          group = "Basins averages"
        ) %>%
        leaflet::addCircleMarkers(
          data = data$swe_at_surveys,
          radius = 8,
          color = "black",
          fillColor = ~ swe_pal(value_to_show),
          weight = 2,
          opacity = 1,
          fillOpacity = 1,
          label = ~ lapply(paste0(name, "<br>", location), htmltools::HTML),
          popup = ~ lapply(popup_content, htmltools::HTML),
          popupOptions = leaflet::popupOptions(
            maxWidth = 320,
            closeButton = TRUE,
            autoPan = TRUE
          ),
          group = "Snow surveys (discrete)"
        ) %>%
        {
          # Roads
          if (!is.null(base_data$shapefiles$roads)) {
            leaflet::addPolylines(
              .,
              data = base_data$shapefiles$roads,
              color = "#8B0000",
              weight = 2,
              opacity = 0.8,
              group = "Roads"
            )
          } else {
            .
          }
        } %>%
        leaflet::addCircleMarkers(
          data = data$swe_at_pillows,
          radius = 8,
          color = "blue",
          fillColor = ~ swe_pal(value_to_show),
          weight = 2,
          opacity = 1,
          fillOpacity = 1,
          label = ~ lapply(paste0(name, "<br>", location), htmltools::HTML),
          popup = ~ lapply(popup_content, htmltools::HTML),
          popupOptions = leaflet::popupOptions(
            maxWidth = 320,
            closeButton = TRUE,
            autoPan = TRUE
          ),
          group = "Snow pillows (continuous)"
        ) %>%
        # --- Re-add base layers on top after SWE layers ---
        {
          # Boundary
          if (!is.null(base_data$shapefiles$yukon)) {
            leaflet::addPolygons(
              .,
              data = base_data$shapefiles$yukon,
              color = "#222222",
              weight = 3,
              fill = FALSE,
              group = "Boundary"
            )
          } else {
            .
          }
        } %>%
        {
          # Communities and labels
          if (!is.null(base_data$shapefiles$communities)) {
            . <- leaflet::addMarkers(
              .,
              data = base_data$shapefiles$communities,
              icon = leaflet::icons(
                iconUrl = "data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='16' height='16' viewBox='0 0 16 16'><polygon points='8,0 16,8 8,16 0,8' fill='black' stroke='white' stroke-width='2'/></svg>",
                iconWidth = 16,
                iconHeight = 16
              ),
              group = "Communities"
            )
            . <- leaflet::addLabelOnlyMarkers(
              .,
              data = base_data$shapefiles$communities,
              lng = base_data$shapefiles$communities$x_adjusted,
              lat = base_data$shapefiles$communities$y_adjusted,
              label = ~ lapply(annotation, htmltools::HTML),
              labelOptions = leaflet::labelOptions(
                noHide = TRUE,
                direction = "top",
                textOnly = TRUE,
                style = list(
                  "color" = "#222",
                  "font-size" = "13px",
                  "font-weight" = "bold",
                  "font-style" = "italic",
                  "text-shadow" = "1px 1px 2px #fff"
                )
              ),
              group = "Communities"
            )
            .
          } else {
            .
          }
        } %>%
        leaflet::clearControls()

      cat("DEBUG: update_swe_layers completed\n")
    }

    # Update map when data changes
    observe({
      cat("DEBUG: Map update observer triggered\n")
      update_swe_layers()
    })

    # Add observer for plot generation requests
    observeEvent(input$generate_plot, {
      cat("DEBUG: Plot generation event triggered\n")
      print(sprintf(
        "Generating plot - Type: %s, Station: %s",
        input$generate_plot$type,
        input$generate_plot$station_name
      ))

      req(
        input$generate_plot$type,
        input$generate_plot$station_id,
        input$generate_plot$station_name
      )

      cat(
        "DEBUG: Creating plot for station type:",
        input$generate_plot$type,
        "\n"
      )

      plot_html <- if (input$generate_plot$type == "pillow") {
        cat("DEBUG: Creating continuous plot popup\n")
        create_continuous_plot_popup(
          timeseries = base_data$pillows$timeseries$swe[, c(
            "datetime",
            as.character(input$generate_plot$station_id)
          )],
          year = as.integer(input$year), # ensure numeric for arithmetic
          con = con
        )
      } else if (input$generate_plot$type == "survey") {
        cat("DEBUG: Creating survey discrete plot popup\n")
        create_discrete_plot_popup(
          timeseries = base_data$surveys$timeseries$swe[, c(
            "datetime",
            as.character(input$generate_plot$station_id)
          )]
        )
      } else if (input$generate_plot$type == "basin") {
        cat("DEBUG: Creating basin discrete plot popup\n")
        create_discrete_plot_popup(
          timeseries = base_data$basins$timeseries$swe[, c(
            "datetime",
            input$generate_plot$station_id
          )]
        )
      }

      cat("DEBUG: Sending plot HTML to client\n")
      # Send the plot HTML back to update the popup
      session$sendCustomMessage("updatePopup", list(html = plot_html))
      print("Plot generated successfully")
    })

    cat("DEBUG: mapSnowbull server function setup completed\n")
  }) # End of moduleServer
} # End of mapLocs function
