mapParamUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(
        # Remove the regular leaflet zoom control as the map filters are in that location, add it in another location when rendering the map
        HTML("
      .leaflet-left .leaflet-control{
        visibility: hidden;
      }"
        ))
    ),
    tags$script(
      HTML("
       // Handles custom tooltips updates, binds tooltip properties to elements
            Shiny.addCustomMessageHandler('update-tooltip', function(message) {
                var selector = '#' + message.id;
                $(selector).attr('title', message.title)
                .tooltip('fixTitle').tooltip('hide');
            });
            
             // Function to change tabs based on namespace and input target, utilized in map popups
      function changeTab(namespace, target_input, locationId) {
        Shiny.setInputValue(namespace + target_input, locationId, {priority: 'event'});
      }
      
      // Resets Shiny input values to null, preventing unwanted reactive triggers
      shinyjs.resetInput = function(params) {
            Shiny.setInputValue(params.name, null, {priority: 'event'});
        }
    ")
    ),
    leaflet::leafletOutput(ns("map"), height = '80vh'),
    absolutePanel(id = ns("controls"), class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 240, left = "auto", width = "200px",
                  # Panel content
                  uiOutput(ns("controls_ui")),
                  style = "opacity: 1; z-index: 400;"  # Adjust styling
    ) # End of absolutePanel
  ) # End of tagList
} # End of mapParamsUI

mapParamServer <- function(id, AquaCache, data, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create a diverging color palette centered at 50%
    
    # "#8c510a",  # Dark Brown (Extreme Drought)
    # "#d8b365",  # Light Brown
    # "#f6e8c3",  # Beige (Normal)
    # "#fddbc7",  # Light Peach
    # "#f4a582",  # Salmon
    # "#d6604d",  # Brick Red
    # "#b2182b"   # Dark Red (Extreme Flood)
    
    # palette <- c("#313695", "#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8", "#FFFFBF", "#FEE090", "#FDAE61", "#F46D43", "#D73027", "#A50026")
    
    palette <- c("#d8b365", "#FEE090", "#74ADD1", "#4575D2", "#313695",  "#A50026")
  
    value_palette <- leaflet::colorBin(
      palette = palette,
      domain = c(0, 100),
      bins = c(0, 20, 40, 60, 80, 100),
      na.color = "#808080"
    )
    
    
    # Create the basic map ###########################################################
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet(options = leaflet::leafletOptions(maxZoom = 18)) %>%
        leaflet::addTiles() %>%
        leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Topographic") %>%
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        leaflet::addLayersControl(baseGroups = c("Topographic", "Satellite")) %>%
        leaflet::addScaleBar(position = "bottomleft", options = leaflet::scaleBarOptions(imperial = FALSE)) %>%
        leaflet::setView(lng = -135.05, lat = 65.00, zoom = 5) %>% # Center on Yukon
        htmlwidgets::onRender(
          "function(el, x) {
          L.control.zoom({position:'bottomright'}).addTo(this);
        }")
    })
    
    # Create the filter inputs ############################################################################
    # Generate all controls in a single renderUI
    output$controls_ui <- renderUI({
      req(data, language$language, language$abbrev)
      
      tagList(
        selectizeInput(
          ns("param"),
          label = translations[id == "parameter", get(language$language)][[1]],
          choices = stats::setNames(
            data$parameters$parameter_id,
              titleCase(data$parameters[[translations[id == "param_name_col", get(language$language)][[1]]]], language$abbrev)
          ),
          selected = 1165,
          multiple = FALSE
        ),
        dateInput(ns("target"),
                  label = translations[id == "map_target_date", get(language$language)][[1]],
                  value = Sys.Date(),
                  max = Sys.Date(),
                  format = "yyyy-mm-dd",
                  language = language$abbrev),
        numericInput(ns("yrs"), 
                     label = translations[id == "map_min_yrs", get(language$language)][[1]], 
                     value = 10, 
                     min = 3, 
                     max = 100, 
                     step = 1),
        numericInput(ns("days"), 
                     label = translations[id == "map_date_within_select", get(language$language)][[1]], 
                     value = 1, 
                     min = 0, 
                     max = 365, 
                     step = 1),
        actionButton(ns("go"), translations[id == "render_map", get(language$language)][[1]])
      )
    })
    
    # Update the filter text based on the selected language ############################
    observeEvent(language$language, {
      updateSelectizeInput(session,
                           "param",
                           label = translations[id == "parameter", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$parameters$parameter_id),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$parameters[[translations[id == "param_name_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateDateInput(session, "target", label = translations[id == "map_target_date", get(language$language)][[1]])
      updateNumericInput(session, "days", label = translations[id == "map_date_within_select", get(language$language)][[1]])
      updateNumericInput(session, "yrs", label = translations[id == "map_min_yrs", get(language$language)][[1]])
      updateActionButton(session, "go", label = translations[id == "render_map", get(language$language)][[1]])
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    # Listen for the user's selection of a parameter and update the map markers and popups ########
    observeEvent(input$go, {
      req(input$param)
      
      if (is.na(input$yrs) || is.na(input$days)) {
        return()
      }
      
      # Stop if the parameter does not exist; it's possible that the user typed something in themselves
      if (!input$param %in% data$parameters$parameter_id) {
        return()
      }
      
      tsids <- dbGetQueryDT(AquaCache, sprintf(
        "SELECT timeseries_id FROM timeseries WHERE parameter_id = %s;",
        input$param
      ))$timeseries_id
      
      range <- dbGetQueryDT(AquaCache, paste0("SELECT timeseries_id, date, value, percent_historic_range, max, min, doy_count FROM measurements_calculated_daily WHERE doy_count >= ", as.numeric(input$yrs), " AND timeseries_id IN (", paste(tsids, collapse = ","), ") AND date BETWEEN '", input$target - as.numeric(input$days), "' AND '", input$target + as.numeric(input$days), "';"))
      
      # Calculate the absolute difference in days between each date and the target date
      range[, date_diff := abs(date - as.Date(input$target))]
      
      # Order the data by 'timeseries_id' and 'date_diff'
      data.table::setorder(range, timeseries_id, date_diff)
      
      # For each 'timeseries_id', select the row with the smallest 'date_diff'
      closest_measurements <- range[, .SD[1], by = timeseries_id]
      
      locs_tsids <- merge(data$locations[, c("latitude", "longitude", "location_id", "name", "name_fr")], data$timeseries[data$timeseries$timeseries_id %in% closest_measurements$timeseries_id, c("timeseries_id", "location_id")], by = "location_id")
      
      mapping_data <- merge(
        closest_measurements,
        locs_tsids,
        by = "timeseries_id",
        all.x = TRUE
      )
      # Cap values at -20% and 120% (above or below use symbology of -20 or 120)
      mapping_data[, percent_historic_range_capped := pmax(pmin(percent_historic_range, 100), 0)]

      param_name <- data$parameters[data$parameters$parameter_id == input$param,  get(translations[id == "param_name_col", get(language$language)])]
      param_unit <- data$parameters[data$parameters$parameter_id == input$param,  "unit_default"]
      # Need help here. The markers should have a color that varies in intensity based on the percent_historic_range value. USe value_palette to set the color. NA values should be grey.
      leaflet::leafletProxy("map", session) %>%
        leaflet::clearMarkers() %>%
        leaflet::addCircleMarkers(
          data = mapping_data,
          lng = ~longitude,
          lat = ~latitude,
          fillColor = ~value_palette(percent_historic_range_capped),
          color = ~value_palette(percent_historic_range_capped),
          fillOpacity = 1,
          stroke = TRUE,
          weight = 1,
          radius = 8,
          popup = ~paste0(
            "<strong>", get(translations[id == "generic_name_col", get(language$language)][[1]]),  "</strong><br/>",
            titleCase(param_name, language$abbrev), "<br>",
            translations[id == "map_actual_date", get(language$language)][[1]], ": ", date, "<br/>",
            translations[id == "map_relative", get(language$language)][[1]], ": ", round(percent_historic_range, 0), "% <br/>",
            translations[id == "map_absolute", get(language$language)][[1]], ": ", round(value, 0), " ", param_unit, "<br/>",
            translations[id == "map_actual_hist_range", get(language$language)][[1]], ": ", round(min, 0), " ", translations[id == "to", get(language$language)][[1]], " ", round(max, 0)," ", param_unit, "<br/>",
            translations[id == "map_actual_yrs", get(language$language)][[1]], ": ", doy_count
          )
        ) %>%
        leaflet::clearControls() %>%  # Clear existing legends
        leaflet::addLegend(
          position = "bottomright",
          pal = value_palette,
          values = mapping_data$percent_historic_range_capped,
          title = NULL,
          labFormat = leaflet::labelFormat(suffix = "%"),
          opacity = 1
        )
      
      
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  })
}
