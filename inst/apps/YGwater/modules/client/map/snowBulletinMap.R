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
                "Relative to Historical Median (%)" = "relative_to_med",
                "Snow Water Equivalent (mm)" = "absolute",
                "Percentile of Historical Range (%)" = "percentile"
              ),
              selected = "relative_to_med",
              inline = FALSE
            )
          ),
          # Collapsible text output
          tags$details(
            tags$summary("Show details"),
            shiny::uiOutput(ns("selected_date"))
          )
        ),
        width = 2
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

    static_style_elements <- get_static_style_elements()
    dynamic_style_elements <- get_dynamic_style_elements()

    # --- Reactive for processed data ---
    processed_data <- shiny::reactive({
      shiny::req(input$year, input$month)
      get_processed_data(
        year = input$year,
        month = input$month,
        base_data = base_data,
        shiny = TRUE
      )
    })

    # --- Reactive for map output ---
    map_output <- shiny::reactive({
      data <- processed_data()
      value_col <- switch(
        input$value_type,
        "relative_to_med" = "relative_to_med",
        "swe" = "data",
        "percentile" = "percentile",
        "relative_to_med"
      )
      data$swe_at_basins$value_to_show <- data$swe_at_basins[[value_col]]
      data$swe_at_surveys$value_to_show <- data$swe_at_surveys[[value_col]]
      data$swe_at_pillows$value_to_show <- data$swe_at_pillows[[value_col]]
      pal_domain <- c(
        data$swe_at_basins$value_to_show,
        data$swe_at_surveys$value_to_show,
        data$swe_at_pillows$value_to_show
      )

      legend_title <- paste0(
        "<b>Snow Water Equivalent:</b><br>",
        switch(
          input$value_type,
          "relative_to_med" = "Percent of Historical Median",
          "swe" = "SWE (mm)",
          "percentile" = "Percentile of Historical Range}",
          ""
        ),
        "<br>",
        month.name[as.integer(input$month)],
        " ",
        input$year
      )

      bbox <- if (!is.null(base_data$shapefiles$yukon)) {
        sf::st_bbox(base_data$shapefiles$yukon)
      } else {
        c(xmin = -141, ymin = 60, xmax = -123, ymax = 69.6)
      }

      leaflet::leaflet(
        options = leaflet::leafletOptions(
          zoomDelta = static_style_elements$leaflet$zoomDelta %||% 0.5,
          zoomSnap = static_style_elements$leaflet$zoomSnap %||% 0.25,
          wheelPxPerZoomLevel = static_style_elements$leaflet$wheelPxPerZoomLevel %||%
            120
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
          data = data$swe_at_basins,
          fillColor = ~ get_state_style_elements(
            value_to_show,
            dynamic_style_elements[[input$value_type]]
          ),
          color = static_style_elements$basins$color,
          weight = 2 * static_style_elements$basins$weight,
          opacity = static_style_elements$basins$opacity,
          fillOpacity = static_style_elements$basins$fillOpacity,
          label = ~ lapply(annotation, htmltools::HTML),
          popup = ~ lapply(popup_content, htmltools::HTML),
          popupOptions = leaflet::popupOptions(
            maxWidth = static_style_elements$basins$popupOptions$maxWidth,
            closeButton = static_style_elements$basins$popupOptions$closeButton,
            autoPan = static_style_elements$basins$popupOptions$autoPan
          ),
          group = "Basins averages"
        ) %>%
        leaflet::addPolygons(
          data = data$swe_at_basins,
          fill = FALSE,
          color = "black",
          weight = 0.5 * static_style_elements$basins$weight,
          opacity = static_style_elements$basins$opacity,
          fillOpacity = 0,
          label = NULL,
          popup = NULL,
          group = "Basins averages"
        ) %>%
        leaflet::addLabelOnlyMarkers(
          data = data$swe_at_basins,
          lng = data$swe_at_basins$x_adjusted,
          lat = data$swe_at_basins$y_adjusted,
          label = lapply(data$swe_at_basins$annotation, htmltools::HTML),
          labelOptions = leaflet::labelOptions(
            noHide = static_style_elements$basins$labelOptions$noHide %||% TRUE,
            direction = static_style_elements$basins$labelOptions$direction %||%
              "center",
            textOnly = static_style_elements$basins$labelOptions$textOnly %||%
              TRUE,
            style = static_style_elements$basins$label
          ),
          group = "Basins averages"
        ) %>%
        leaflet::addCircleMarkers(
          data = data$swe_at_surveys,
          radius = static_style_elements$surveys$radius,
          color = static_style_elements$surveys$color,
          fillColor = ~ get_state_style_elements(
            value_to_show,
            dynamic_style_elements[[input$value_type]]
          ),
          weight = static_style_elements$surveys$weight,
          opacity = static_style_elements$surveys$opacity,
          fillOpacity = static_style_elements$surveys$fillOpacity,
          label = ~ lapply(paste0(name, "<br>", location), htmltools::HTML),
          popup = ~ lapply(popup_content, htmltools::HTML),
          popupOptions = leaflet::popupOptions(
            maxWidth = static_style_elements$basins$popupOptions$maxWidth,
            closeButton = static_style_elements$basins$popupOptions$closeButton,
            autoPan = static_style_elements$basins$popupOptions$autoPan
          ),
          group = "Snow surveys (discrete)"
        ) %>%
        {
          if (!is.null(base_data$shapefiles$roads)) {
            leaflet::addPolylines(
              .,
              data = base_data$shapefiles$roads,
              color = static_style_elements$roads$color,
              weight = static_style_elements$roads$weight,
              opacity = static_style_elements$roads$opacity,
              group = "Roads"
            )
          } else {
            .
          }
        } %>%
        leaflet::addMarkers(
          data = data$swe_at_pillows,
          icon = leaflet::icons(
            iconUrl = "data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='16' height='16'><rect x='2' y='2' width='12' height='12' fill='none' stroke='black' stroke-width='2'/></svg>",
            iconWidth = 2.7 * static_style_elements$pillows$radius,
            iconHeight = 2.7 * static_style_elements$pillows$radius
          ),
          label = ~ lapply(paste0(name, "<br>", location), htmltools::HTML),
          popup = ~ lapply(popup_content, htmltools::HTML),
          popupOptions = do.call(
            leaflet::popupOptions,
            static_style_elements$basins$popupOptions
          ),
          group = "Snow pillows (continuous)"
        ) %>%
        leaflet::addCircleMarkers(
          data = data$swe_at_pillows,
          radius = static_style_elements$pillows$radius,
          color = static_style_elements$pillows$color,
          fillColor = ~ get_state_style_elements(
            value_to_show,
            dynamic_style_elements[[input$value_type]]
          ),
          weight = static_style_elements$pillows$weight,
          opacity = static_style_elements$pillows$opacity,
          fillOpacity = static_style_elements$pillows$fillOpacity,
          label = ~ lapply(paste0(name, "<br>", location), htmltools::HTML),
          popup = ~ lapply(popup_content, htmltools::HTML),
          popupOptions = leaflet::popupOptions(
            maxWidth = static_style_elements$basins$popupOptions$maxWidth,
            closeButton = static_style_elements$basins$popupOptions$closeButton,
            autoPan = static_style_elements$basins$popupOptions$autoPan
          ),
          group = "Snow pillows (continuous)"
        ) %>%
        {
          if (!is.null(base_data$shapefiles$yukon)) {
            leaflet::addPolygons(
              .,
              data = base_data$shapefiles$yukon,
              color = static_style_elements$boundary$color,
              weight = static_style_elements$boundary$weight,
              fill = static_style_elements$boundary$fill,
              group = "Boundary"
            )
          } else {
            .
          }
        } %>%
        {
          communities_split <- split_communities(
            base_data$shapefiles$communities
          )

          . <- leaflet::addMarkers(
            .,
            data = communities_split$large,
            icon = static_style_elements$communities$icon,
            label = ~ lapply(annotation, htmltools::HTML),
            popup = ~ lapply(popup, htmltools::HTML),
            popupOptions = do.call(
              leaflet::popupOptions,
              static_style_elements$basins$popupOptions
            ),
            group = c("Communities_large")
          )
          . <- leaflet::addLabelOnlyMarkers(
            .,
            data = communities_split$large,
            lng = communities_split$large$x,
            lat = communities_split$large$y,
            label = ~ lapply(
              communities_split$large$annotation,
              htmltools::HTML
            ),
            labelOptions = static_style_elements$communities$labelOptions,
            group = c("Communities_large")
          )
          . <- leaflet::addMarkers(
            .,
            data = communities_split$small,
            icon = static_style_elements$communities$icon,
            label = ~ lapply(annotation, htmltools::HTML),
            popup = ~ lapply(popup, htmltools::HTML),
            popupOptions = do.call(
              leaflet::popupOptions,
              static_style_elements$basins$popupOptions
            ),
            group = c("Communities_small")
          )
          . <- leaflet::addLabelOnlyMarkers(
            .,
            data = communities_split$small,
            lng = communities_split$small$x,
            lat = communities_split$small$y,
            label = ~ lapply(
              communities_split$small$annotation,
              htmltools::HTML
            ),
            labelOptions = static_style_elements$communities$labelOptions,
            group = c("Communities_small")
          )
          .
        } %>%
        leaflet::groupOptions(
          "Communities_large",
          zoomLevels = seq(7, 18, 0.25)
        ) %>%
        leaflet::groupOptions(
          "Communities_small",
          zoomLevels = seq(8, 18, 0.25)
        ) %>%
        leaflet::addControl(
          # here we specify a dummy HTML legend since it's much easier than the alternative.
          # we grab some style elements from the static styles to keep it consistent, but this isn't possible for all cases
          html = paste0(
            "<div style='padding: 8px; border-radius: 6px; font-size: 13px; line-height: 1.4; min-width: 140px;'>",
            "<b>Locations</b><br>",
            "<svg width='18' height='18' style='vertical-align:middle;'><circle cx='9' cy='9' r='7' fill='none' stroke='black' stroke-width='2'/></svg> ",
            "Snow course (survey)<br>",
            "<svg width='18' height='18' style='vertical-align:middle;'><rect x='3' y='3' width='12' height='12' fill='none' stroke='black' stroke-width='2'/><circle cx='9' cy='9' r='5' fill='none' stroke='black' stroke-width='2'/></svg> ",
            "Snow pillow (continuous)<br>",
            sprintf(
              "<svg width='18' height='18' style='vertical-align:middle;'><line x1='2' y1='16' x2='16' y2='2' style='stroke:%s;stroke-width:%d'/></svg> ",
              static_style_elements$roads$color,
              static_style_elements$roads$weight
            ),
            "Roads<br>",
            "<svg width='18' height='18' style='vertical-align:middle;'><polygon points='9,2 16,9 9,16 2,9' fill='black' stroke='white' stroke-width='2'/></svg> ",
            "Community<br>",
            "</div>"
          ),
          position = "bottomright"
        ) %>%
        leaflet::addLegend(
          position = "bottomright",
          colors = dynamic_style_elements[[input$value_type]]$colors,
          title = legend_title,
          labels = dynamic_style_elements[[input$value_type]]$labels,
          opacity = 1
        )
    })

    # --- Render map using reactive output ---
    output$map <- leaflet::renderLeaflet({
      map_output()
    })

    # --- Observer: plot generation requests ---
    shiny::observeEvent(input$generate_plot, {
      req(
        input$generate_plot$type,
        input$generate_plot$station_id,
        input$generate_plot$station_name
      )
      plot_html <-
        if (input$generate_plot$type == "pillow") {
          create_continuous_plot_popup(
            timeseries = base_data$pillows$timeseries$data[
              base_data$pillows$timeseries$data$datetime <=
                as.Date(paste0(input$year, "-12-31")),
              c("datetime", as.character(input$generate_plot$station_id))
            ],
            year = as.integer(input$year),
            con = con,
            station_name = input$generate_plot$station_name
          )
        } else if (input$generate_plot$type == "survey") {
          create_discrete_plot_popup(
            timeseries = base_data$surveys$timeseries$data[
              base_data$surveys$timeseries$data$datetime <=
                as.Date(paste0(input$year, "-12-31")),
              c("datetime", as.character(input$generate_plot$station_id))
            ],
            station_name = input$generate_plot$station_name
          )
        } else if (input$generate_plot$type == "basin") {
          create_discrete_plot_popup(
            timeseries = base_data$basins$timeseries$data[
              base_data$basins$timeseries$data$datetime <=
                as.Date(paste0(input$year, "-12-31")),
              c(
                "datetime",
                input$generate_plot$station_id
              )
            ],
            station_name = input$generate_plot$station_name
          )
        }
      session$sendCustomMessage("updatePopup", list(html = plot_html))
    })

    # --- Render selected date text with details ---
    output$selected_date <- shiny::renderText({
      shiny::req(input$year, input$month)
      date_obj <- get_datetime(input$year, input$month)
      shiny::HTML(paste(
        "<b>Snow Bulletin Date: </b>",
        format(date_obj, "%B %d, %Y"),
        "<br>",
        "<b>Normals period:</b> 1991–2020<br>",
        "<b>Data source:</b> AquaCache database<br>",
        "<b>Basemap:</b> Esri World Imagery<br>",
        "<br>",
        "<i>Disclaimer:</i> Basin average estimates may differ slightly from archived bulletins.<br>",
        "<br>",
        "<b>Map generated on: </b>",
        format(Sys.time(), "%B %d, %Y at %H:%M %Z"),
        "<br>",
        "<b>App version:</b>",
        as.character(utils::packageVersion("YGwater")),
        sep = ""
      ))
    })
  })
}
