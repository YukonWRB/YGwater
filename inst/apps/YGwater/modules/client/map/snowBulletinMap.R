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
#source("R/SWE_maputils.R")
# =============================================================================
# DATA LOADING AND PROCESSING
# =============================================================================

# Load all base data once at startup

# =============================================================================
# SHINY APP UI
# =============================================================================
mapSnowbullUI <- function(id) {
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
                "Absolute (mm)" = "swe",
                "Percentile" = "percentile"
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

    con <- session$userData$AquaCache

    base_data <- load_bulletin_data(con)

    viz_params <- initialize_visualization_parameters()

    # Render UI elements ####
    # (placeholder for now)
    # ---- End color palette functions ----
    # Reactive expression for selected date display
    output$selected_date <- shiny::renderText({
      shiny::req(input$year, input$month)
      # Convert DOY from character to numeric
      date_obj <- get_datetime(input$year, input$month)
      format(date_obj, "%B %d, %Y")
    })

    # Reactive data processing based on user inputs

    # Generic function to get processed data for a given year/month

    processed_data <- reactive({
      shiny::req(input$year, input$month)

      result <- get_processed_data(
        year = input$year,
        month = input$month,
        base_data = base_data,
        shiny = TRUE
      )

      return(result)
    })

    output$map <- leaflet::renderLeaflet({
      # Use Yukon boundary if available, otherwise fallback
      if (!is.null(base_data$shapefiles$yukon)) {
        bbox <- sf::st_bbox(base_data$shapefiles$yukon)
      } else {
        bbox <- c(xmin = -141, ymin = 60, xmax = -123, ymax = 69.6)
      }
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
      swe_pal <- leaflet::colorBin(
        palette = viz_params$basins$fillColor,
        bins = viz_params$basins$bins,
        domain = pal_domain,
        na.color = "gray"
      )

      leaflet::leaflet(
        options = leaflet::leafletOptions(
          zoomDelta = viz_params$zoomDelta %||% 0.5,
          zoomSnap = viz_params$zoomSnap %||% 0.25,
          wheelPxPerZoomLevel = viz_params$wheelPxPerZoomLevel %||% 120
        )
      ) %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery,
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
          color = viz_params$basins$color,
          weight = viz_params$basins$weight,
          opacity = viz_params$basins$opacity,
          fillOpacity = viz_params$basins$fillOpacity,
          label = ~ lapply(annotation, htmltools::HTML),
          popup = ~ lapply(popup_content, htmltools::HTML),
          popupOptions = do.call(
            leaflet::popupOptions,
            viz_params$basins$popupOptions
          ),
          group = "Basins averages"
        ) %>%
        leaflet::addLabelOnlyMarkers(
          data = initial_data$swe_at_basins,
          lng = initial_data$swe_at_basins$x_adjusted,
          lat = initial_data$swe_at_basins$y_adjusted,
          label = ~ lapply(
            initial_data$swe_at_basins$annotation,
            htmltools::HTML
          ),
          labelOptions = leaflet::labelOptions(
            noHide = viz_params$basins$labelOptions$noHide %||% TRUE,
            direction = viz_params$basins$labelOptions$direction %||% "center",
            textOnly = viz_params$basins$labelOptions$textOnly %||% TRUE,
            style = viz_params$basins$label
          ),
          group = "Basins averages"
        ) %>%
        {
          if (!is.null(base_data$shapefiles$roads)) {
            leaflet::addPolylines(
              .,
              data = base_data$shapefiles$roads,
              color = viz_params$roads$color,
              weight = viz_params$roads$weight,
              opacity = viz_params$roads$opacity,
              group = "Roads",
              label = ~ lapply(as.character(feature_name), htmltools::HTML)
            )
          } else {
            .
          }
        } %>%
        {
          if (!is.null(base_data$shapefiles$yukon)) {
            leaflet::addPolygons(
              .,
              data = base_data$shapefiles$yukon,
              color = viz_params$boundary$color,
              weight = viz_params$boundary$weight,
              fill = viz_params$boundary$fill,
              group = "Boundary"
            )
          } else {
            .
          }
        } %>%
        leaflet::addCircleMarkers(
          data = initial_data$swe_at_surveys,
          radius = viz_params$surveys$radius,
          color = viz_params$surveys$color,
          fillColor = ~ swe_pal(value_to_show),
          weight = viz_params$surveys$weight,
          opacity = viz_params$surveys$opacity,
          fillOpacity = viz_params$surveys$fillOpacity,
          label = ~ lapply(paste0(name, "<br>", location), htmltools::HTML),
          popup = ~ lapply(popup_content, htmltools::HTML),
          popupOptions = do.call(
            leaflet::popupOptions,
            viz_params$basins$popupOptions
          ),
          group = "Snow surveys (discrete)"
        ) %>%
        leaflet::addCircleMarkers(
          data = initial_data$swe_at_pillows,
          radius = viz_params$pillows$radius,
          color = viz_params$pillows$color,
          fillColor = ~ swe_pal(value_to_show),
          weight = viz_params$pillows$weight,
          opacity = viz_params$pillows$opacity,
          fillOpacity = viz_params$pillows$fillOpacity,
          label = ~ lapply(paste0(name, "<br>", location), htmltools::HTML),
          popup = ~ lapply(popup_content, htmltools::HTML),
          popupOptions = do.call(
            leaflet::popupOptions,
            viz_params$basins$popupOptions
          ),
          group = "Snow pillows (continuous)"
        ) %>%
        {
          if (!is.null(base_data$shapefiles$communities)) {
            comm <- base_data$shapefiles$communities
            . <- leaflet::addMarkers(
              .,
              data = comm,
              icon = leaflet::icons(
                iconUrl = viz_params$communities$icon,
                iconWidth = viz_params$communities$iconWidth,
                iconHeight = viz_params$communities$iconHeight
              ),
              label = ~ lapply(annotation, htmltools::HTML),
              popup = ~ lapply(popup, htmltools::HTML),
              popupOptions = do.call(
                leaflet::popupOptions,
                viz_params$basins$popupOptions
              ),
              group = "Communities"
            )
            . <- leaflet::addLabelOnlyMarkers(
              .,
              data = comm,
              lng = comm$x,
              lat = comm$y,
              label = ~ lapply(comm$annotation, htmltools::HTML),
              labelOptions = leaflet::labelOptions(
                noHide = viz_params$communities$labelOptions$noHide,
                direction = viz_params$communities$labelOptions$direction,
                textOnly = viz_params$communities$labelOptions$textOnly,
                style = viz_params$communities$label
              ),
              group = "Communities"
            )
            .
          } else {
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
        ) %>%
        leaflet::addLegend(
          position = "bottomright",
          pal = swe_pal,
          values = pal_domain,
          title = "% of Normal",
          labFormat = leaflet::labelFormat(suffix = "%"),
          opacity = 1
        )
    })

    # Helper: add SWE layers (basins, surveys, pillows)
    update_swe_layers <- function() {
      # Use default values if input$year or input$month are NULL (initial load)
      year <- if (!is.null(input$year)) input$year else "2025"
      month <- if (!is.null(input$month)) input$month else "3"
      value_type <- if (!is.null(input$value_type)) {
        input$value_type
      } else {
        "relative_swe"
      }

      data <- processed_data()
      data <<- data

      print(data$swe_at_pillows$percentile)
      print(viz_params$percentile_colors)
      print(viz_params$percentile_bins)
      print(value_type)
      # Select value column based on input$value_type
      value_col <- switch(
        value_type,
        "relative_swe" = "relative_swe",
        "swe" = "swe",
        "percentile" = "percentile",
        "relative_swe"
      )

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

      # Create appropriate color palette based on value type
      swe_pal <- switch(
        value_type,
        "relative_swe" = leaflet::colorBin(
          palette = viz_params$relative_colors,
          bins = viz_params$relative_bins,
          domain = pal_domain,
          na.color = "gray"
        ),
        "swe" = leaflet::colorBin(
          palette = viz_params$absolute_colors,
          bins = viz_params$absolute_bins,
          domain = pal_domain,
          na.color = "gray"
        ),
        "percentile" = leaflet::colorBin(
          palette = viz_params$percentile_colors,
          bins = viz_params$percentile_bins,
          domain = pal_domain,
          na.color = "gray"
        )
      )
      legend_title <- switch(
        value_type,
        "relative_swe" = "% of Normal",
        "swe" = "SWE (mm)",
        "percentile" = "Percentile",
        "% of Normal"
      )
      legend_labFormat <- switch(
        value_type,
        "relative_swe" = leaflet::labelFormat(suffix = "%"),
        "swe" = leaflet::labelFormat(suffix = " mm"),
        "percentile" = leaflet::labelFormat(suffix = "th"),
        leaflet::labelFormat(suffix = "%")
      )

      # Add SWE layers first (so they are underneath)
      leaflet::leafletProxy("map", session = session) %>%
        leaflet::clearGroup("Basins averages") %>%
        leaflet::clearGroup("Snow surveys (discrete)") %>%
        leaflet::clearGroup("Snow pillows (continuous)") %>%
        leaflet::clearGroup("Roads") %>%
        leaflet::clearGroup("Boundary") %>%
        leaflet::clearGroup("Communities") %>%
        leaflet::addPolygons(
          data = data$swe_at_basins,
          fillColor = ~ swe_pal(value_to_show),
          color = viz_params$basins$color,
          weight = viz_params$basins$weight,
          opacity = viz_params$basins$opacity,
          fillOpacity = viz_params$basins$fillOpacity,
          label = ~ lapply(annotation, htmltools::HTML),
          popup = ~ lapply(popup_content, htmltools::HTML),
          popupOptions = leaflet::popupOptions(
            maxWidth = viz_params$basins$popupOptions$maxWidth,
            closeButton = viz_params$basins$popupOptions$closeButton,
            autoPan = viz_params$basins$popupOptions$autoPan
          ),
          group = "Basins averages"
        ) %>%
        leaflet::addLabelOnlyMarkers(
          data = data$swe_at_basins,
          lng = data$swe_at_basins$x_adjusted,
          lat = data$swe_at_basins$y_adjusted,
          label = ~ lapply(data$swe_at_basins$annotation, htmltools::HTML),
          labelOptions = leaflet::labelOptions(
            noHide = viz_params$basins$labelOptions$noHide %||% TRUE,
            direction = viz_params$basins$labelOptions$direction %||% "center",
            textOnly = viz_params$basins$labelOptions$textOnly %||% TRUE,
            style = viz_params$basins$label
          ),
          group = "Basins averages"
        ) %>%
        leaflet::addCircleMarkers(
          data = data$swe_at_surveys,
          radius = viz_params$surveys$radius,
          color = viz_params$surveys$color,
          fillColor = ~ swe_pal(value_to_show),
          weight = viz_params$surveys$weight,
          opacity = viz_params$surveys$opacity,
          fillOpacity = viz_params$surveys$fillOpacity,
          label = ~ lapply(paste0(name, "<br>", location), htmltools::HTML),
          popup = ~ lapply(popup_content, htmltools::HTML),
          popupOptions = leaflet::popupOptions(
            maxWidth = viz_params$basins$popupOptions$maxWidth,
            closeButton = viz_params$basins$popupOptions$closeButton,
            autoPan = viz_params$basins$popupOptions$autoPan
          ),
          group = "Snow surveys (discrete)"
        ) %>%
        {
          if (!is.null(base_data$shapefiles$roads)) {
            leaflet::addPolylines(
              .,
              data = base_data$shapefiles$roads,
              color = viz_params$roads$color,
              weight = viz_params$roads$weight,
              opacity = viz_params$roads$opacity,
              group = "Roads"
            )
          } else {
            .
          }
        } %>%
        leaflet::addCircleMarkers(
          data = data$swe_at_pillows,
          radius = viz_params$pillows$radius,
          color = viz_params$pillows$color,
          fillColor = ~ swe_pal(value_to_show),
          weight = viz_params$pillows$weight,
          opacity = viz_params$pillows$opacity,
          fillOpacity = viz_params$pillows$fillOpacity,
          label = ~ lapply(paste0(name, "<br>", location), htmltools::HTML),
          popup = ~ lapply(popup_content, htmltools::HTML),
          popupOptions = leaflet::popupOptions(
            maxWidth = viz_params$basins$popupOptions$maxWidth,
            closeButton = viz_params$basins$popupOptions$closeButton,
            autoPan = viz_params$basins$popupOptions$autoPan
          ),
          group = "Snow pillows (continuous)"
        ) %>%
        {
          if (!is.null(base_data$shapefiles$yukon)) {
            leaflet::addPolygons(
              .,
              data = base_data$shapefiles$yukon,
              color = viz_params$boundary$color,
              weight = viz_params$boundary$weight,
              fill = viz_params$boundary$fill,
              group = "Boundary"
            )
          } else {
            .
          }
        } %>%
        {
          if (!is.null(base_data$shapefiles$communities)) {
            . <- leaflet::addMarkers(
              .,
              data = base_data$shapefiles$communities,
              icon = leaflet::icons(
                iconUrl = viz_params$communities$icon,
                iconWidth = viz_params$communities$iconWidth,
                iconHeight = viz_params$communities$iconHeight
              ),
              group = "Communities"
            )
            . <- leaflet::addLabelOnlyMarkers(
              .,
              data = base_data$shapefiles$communities,
              lng = base_data$shapefiles$communities$x,
              lat = base_data$shapefiles$communities$y,
              label = ~ lapply(annotation, htmltools::HTML),
              labelOptions = leaflet::labelOptions(
                noHide = viz_params$communities$labelOptions$noHide,
                direction = viz_params$communities$labelOptions$direction,
                textOnly = viz_params$communities$labelOptions$textOnly,
                style = viz_params$communities$label
              ),
              group = "Communities"
            )
            .
          } else {
            .
          }
        } %>%
        leaflet::clearControls() %>%
        leaflet::addLegend(
          position = "bottomright",
          pal = swe_pal,
          values = pal_domain,
          title = legend_title,
          labFormat = legend_labFormat,
          opacity = 1
        )
    }

    # Update map when data changes
    observe(
      update_swe_layers()
    )

    # Add observer for plot generation requests
    observeEvent(input$generate_plot, {
      req(
        input$generate_plot$type,
        input$generate_plot$station_id,
        input$generate_plot$station_name
      )

      plot_html <- if (input$generate_plot$type == "pillow") {
        create_continuous_plot_popup(
          timeseries = base_data$pillows$timeseries$swe[
            base_data$pillows$timeseries$swe$datetime <=
              as.Date(paste0(input$year, "-12-31")),
            c("datetime", as.character(input$generate_plot$station_id))
          ],
          year = as.integer(input$year),
          con = con
        )
      } else if (input$generate_plot$type == "survey") {
        create_discrete_plot_popup(
          timeseries = base_data$surveys$timeseries$swe[
            base_data$surveys$timeseries$swe$datetime <=
              as.Date(paste0(input$year, "-12-31")),
            c("datetime", as.character(input$generate_plot$station_id))
          ]
        )
      } else if (input$generate_plot$type == "basin") {
        create_discrete_plot_popup(
          timeseries = base_data$basins$timeseries$swe[
            base_data$basins$timeseries$swe$datetime <=
              as.Date(paste0(input$year, "-12-31")),
            c(
              "datetime",
              input$generate_plot$station_id
            )
          ]
        )
      }

      # Send the plot HTML back to update the popup
      session$sendCustomMessage("updatePopup", list(html = plot_html))
    })
  }) # End of moduleServer
} # End of mapLocs function
