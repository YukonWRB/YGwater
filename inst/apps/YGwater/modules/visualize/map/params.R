mapParamUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(
        # Remove the regular leaflet zoom control to decrease map business
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
        leaflet::setView(lng = -135.05, lat = 65.00, zoom = 5) # Center on Yukon
    })
    
    # Create the filter inputs ############################################################################
    # Generate all controls in a single renderUI
    output$controls_ui <- renderUI({
      req(data, language$language, language$abbrev)
      
      tagList(
        
        #TODO: Give users the option to plot absolute values or relative to historic range. For absolute values only one parameter is allowed. Extra controls are necessary also to give user option for 'latest measurement', possibly as a checkboxInput. If not selected, then a date selector will be shown. If selected, the date selector will be hidden. This  will also necessitate a modal to let users select their 'bins' for the map symbology (which will by default use the data's range)
        selectizeInput(
          ns("mapType"),
          label = NULL,
          choices = stats::setNames(
            c("range", "abs"),
            c(translations[id == "map_relative", get(language$language)][[1]], translations[id == "map_absolute", get(language$language)][[1]])
          ),
          selected = "range",
          multiple = FALSE
        ),
        selectizeInput(  # Allows user to select a secondary map parameter, to be used for locations where a parameter is missing. Allows having streams and lake levels on the same map, for example.
          ns("params"),
          label = NULL,
          choices = stats::setNames(
            c(1,2),
            c(translations[id == "map_one_parameter", get(language$language)][[1]], translations[id == "map_two_parameters", get(language$language)][[1]])
          ),
          selected = 1,
          multiple = FALSE
        ),
        selectizeInput(
          ns("param1"),
          label = translations[id == "parameter", get(language$language)][[1]],
          choices = stats::setNames(
            data$parameters$parameter_id,
              titleCase(data$parameters[[translations[id == "param_name_col", get(language$language)][[1]]]], language$abbrev)
          ),
          selected = 1050, # Water flow
          multiple = FALSE
        ),
        selectizeInput(
          ns("param2"),
          label = translations[id == "parameter", get(language$language)][[1]],
          choices = stats::setNames(
            data$parameters$parameter_id,
            titleCase(data$parameters[[translations[id == "param_name_col", get(language$language)][[1]]]], language$abbrev)
          ),
          selected = 1165, # Water level (for places where flow does not exist)
          multiple = FALSE
        ),
        checkboxInput(ns("latest"), translations[id == "map_latest_measurement", get(language$language)][[1]], value = TRUE),
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
        actionButton(ns("bins"), "Modify legend bins"),
        actionButton(ns("go"), translations[id == "render_map", get(language$language)][[1]])
      )
    })
    
    # Update the filter text based on the selected language ############################
    observeEvent(language$language, {
      updateSelectizeInput(session,
                           "mapType",
                           label = NULL,
                           choices = stats::setNames(
                             c("range", "abs"),
                             c(translations[id == "map_relative", get(language$language)][[1]], translations[id == "map_absolue", get(language$language)][[1]])
                           )
      )
      updateSelectizeInput(session,
                           "params",
                           label = NULL,
                           choices = stats::setNames(
                             c(1,2),
                             c(translations[id == "map_one_parameter", get(language$language)][[1]], translations[id == "map_two_parameters", get(language$language)][[1]])
                           )
      )
      updateSelectizeInput(session,
                           "param1",
                           label = translations[id == "parameter", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$parameters$parameter_id),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$parameters[[translations[id == "param_name_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "param2",
                           label = translations[id == "parameter", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$parameters$parameter_id),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$parameters[[translations[id == "param_name_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateCheckboxInput(session, "latest", label = translations[id == "map_latest_measurement", get(language$language)][[1]])
      updateDateInput(session, "target", label = translations[id == "map_target_date", get(language$language)][[1]])
      updateNumericInput(session, "days", label = translations[id == "map_date_within_select", get(language$language)][[1]])
      updateNumericInput(session, "yrs", label = translations[id == "map_min_yrs", get(language$language)][[1]])
      updateActionButton(session, "go", label = translations[id == "render_map", get(language$language)][[1]])
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    observeEvent(input$mapType, {
      if (input$mapType == "abs") {
        shinyjs::hide("param2")
        shinyjs::hide("yrs")
        shinyjs::show("bins")
        updateSelectizeInput(session,
                             "params",
                             selected = 1
        )
      } else {
        shinyjs::show("param2")
        shinyjs::show("yrs")
        shinyjs::hide("bins")
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$latest, {
      if (input$latest) {
        shinyjs::hide("target")
      } else {
        shinyjs::show("target")
      }
    }, ignoreInit = TRUE)
  
    observeEvent(input$params, {
      if (input$params == 1) {
        shinyjs::hide("param2")
      } else {
        shinyjs::show("param2")
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$bins, {
      showModal(
        modalDialog(
          title = 
        )
      )
    }, ignoreInit = TRUE)
    
    # Listen for the user's selection of a parameter and update the map markers and popups ########
    observeEvent(input$go, {
      req(input$param1)
      
      if (input$params == 2) {
        req(input$param2)
      }
      
      if (is.na(input$yrs) || is.na(input$days)) {
        return()
      }
      
      # Stop if the parameter does not exist; it's possible that the user typed something in themselves
      if (!input$param1 %in% data$parameters$parameter_id) {
        return()
      }
      if (input$params == 2 && !input$param2 %in% data$parameters$parameter_id) {
        return()
      }
      
      
      tsids1 <- dbGetQueryDT(AquaCache, sprintf(
        "SELECT timeseries_id FROM timeseries WHERE parameter_id = %s;",
        input$param1
      ))$timeseries_id
      
      range1 <- dbGetQueryDT(AquaCache, paste0("SELECT timeseries_id, date, value, percent_historic_range, max, min, doy_count FROM measurements_calculated_daily WHERE doy_count >= ", as.numeric(input$yrs), " AND timeseries_id IN (", paste(tsids1, collapse = ","), ") AND date BETWEEN '", input$target - as.numeric(input$days), "' AND '", input$target + as.numeric(input$days), "';"))
      
      # Calculate the absolute difference in days between each date and the target date
      range1[, date_diff := abs(date - as.Date(input$target))]
      
      # Order the data by 'timeseries_id' and 'date_diff'
      data.table::setorder(range1, timeseries_id, date_diff)
      
      # For each 'timeseries_id', select the row with the smallest 'date_diff'
      closest_measurements1 <- range1[, .SD[1], by = timeseries_id]
      
      locs_tsids1 <- merge(data$locations[, c("latitude", "longitude", "location_id", "name", "name_fr")], data$timeseries[data$timeseries$timeseries_id %in% closest_measurements1$timeseries_id, c("timeseries_id", "location_id")], by = "location_id")
      
      locs_tsids1$param_name <- data$parameters[data$parameters$parameter_id == input$param1,  get(translations[id == "param_name_col", get(language$language)])]
      locs_tsids1$param_unit <- data$parameters[data$parameters$parameter_id == input$param1,  "unit_default"]
      
      
      # Now if the user has selected two parameters, repeat the process for the second parameter BUT only for the locations that did not have a match for the first parameter
      if (input$params == 2) {
        tsids2 <- dbGetQueryDT(AquaCache, paste0("SELECT timeseries_id FROM timeseries WHERE parameter_id = ", input$param2, " AND timeseries_id NOT IN (", paste(locs_tsids1$timeseries_id, sep = ", "), ";"))$timeseries_id
        
        range2 <- dbGetQueryDT(AquaCache, paste0("SELECT timeseries_id, date, value, percent_historic_range, max, min, doy_count FROM measurements_calculated_daily WHERE doy_count >= ", as.numeric(input$yrs), " AND timeseries_id IN (", paste(tsids2, collapse = ","), ") AND date BETWEEN '", input$target - as.numeric(input$days), "' AND '", input$target + as.numeric(input$days), "';"))
        
        # Calculate the absolute difference in days between each date and the target date
        range2[, date_diff := abs(date - as.Date(input$target))]
        
        # Order the data by 'timeseries_id' and 'date_diff'
        data.table::setorder(range2, timeseries_id, date_diff)
        
        # For each 'timeseries_id', select the row with the smallest 'date_diff'
        closest_measurements2 <- range2[, .SD[1], by = timeseries_id]
        
        locs_tsids2 <- merge(data$locations[, c("latitude", "longitude", "location_id", "name", "name_fr")], data$timeseries[data$timeseries$timeseries_id %in% closest_measurements2$timeseries_id, c("timeseries_id", "location_id")], by = "location_id")
        
        locs_tsids2$param_name <- data$parameters[data$parameters$parameter_id == input$param2,  get(translations[id == "param_name_col", get(language$language)])]
        locs_tsid2$param_unit <- data$parameters[data$parameters$parameter_id == input$param1,  "unit_default"]
        
        
        # Merge the two sets of locations and timeseries IDs
        locs_tsids <- rbind(locs_tsids1, locs_tsids2)
        closest_measurements <- rbind(closest_measurements1, closest_measurements2)
      } else {
        locs_tsids <- locs_tsids1
        closest_measurements <- closest_measurements1
      }
      
      mapping_data <- merge(
        closest_measurements,
        locs_tsids,
        by = "timeseries_id",
        all.x = TRUE
      )
      # Cap values at 0 and 100% (above or below use symbology of 0 or 100)
      mapping_data[, percent_historic_range_capped := pmax(pmin(percent_historic_range, 100), 0)]
      
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
