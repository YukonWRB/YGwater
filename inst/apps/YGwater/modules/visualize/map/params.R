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
                  draggable = TRUE, top = 210, left = "auto", width = "240px",
                  # Panel content
                  uiOutput(ns("controls_ui")),
                  style = "opacity: 1; z-index: 400;"  # Adjust styling
    ) # End of absolutePanel
  ) # End of tagList
} # End of mapParamsUI

mapParamServer <- function(id, moduleData, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create the filter inputs ############################################################################
    map_params <- reactiveValues(
      param1 = 1150,  # Water flow
      param2 = 1165,  # Water level
      yrs1 = 10,
      yrs2 = 10,
      days1 = 1,
      days2 = 1,
      latest = TRUE,
      target = Sys.Date(),
      params = 2,
      bins = c(0, 20, 40, 60, 80, 100),
      colors = c("#d8b365", "#FEE090", "#74ADD1", "#4575D2", "#313695",  "#A50026")
    )
    # Generate all controls in a renderUI
    output$controls_ui <- renderUI({
      req(moduleData, language$language, language$abbrev)
      
      tagList(
        #TODO: Give users the option to plot absolute values or relative to historic range. For absolute values only one parameter is allowed. Extra controls are necessary also to give user option for 'latest measurement', possibly as a checkboxInput. If not selected, then a date selector will be shown. If selected, the date selector will be hidden. This  will also necessitate a modal to let users select their 'bins' for the map symbology (which will by default use the data's range)
        selectizeInput(
          ns("mapType"),
          label = translations[id == "map_mapType", get(language$language)][[1]],
          choices = stats::setNames(
            c("range", "abs"),
            c(translations[id == "map_relative", get(language$language)][[1]], translations[id == "map_absolute1", get(language$language)][[1]])
          ),
          selected = "range",
          multiple = FALSE
        ),
        dateInput(ns("target"),
                  label = translations[id == "map_target_date", get(language$language)][[1]],
                  value = Sys.Date(),
                  max = Sys.Date(),
                  format = "yyyy-mm-dd",
                  language = language$abbrev),
        checkboxInput(ns("latest"), translations[id == "map_latest_measurements", get(language$language)][[1]], value = TRUE),
        htmlOutput(ns("primary_param")),  # This will be text showing details of the selected parameter, the min yrs, within how many days, etc.
        actionButton(ns("edit_primary_param"), translations[id == "map_edit_primary_param", get(language$language)][[1]], style = "display: block; width: 100%"),
        htmlOutput(ns("secondary_param")),
        actionButton(ns("edit_secondary_param"), translations[id == "map_edit_second_param", get(language$language)][[1]], style = "display: block; width: 100%")
        # actionButton(ns("go"), translations[id == "render_map", get(language$language)][[1]], style = "display: block; width: 100%; margin-top: 10px;")
      )
    })
    
    # This updates automatically when the language changes so is not part of the language observer
    output$primary_param <- renderUI({
      tagList(
        h4(translations[id == "map_primary_param", get(language$language)][[1]]), # Text for primary parameter
        p(titleCase(moduleData$parameters[moduleData$parameters$parameter_id == map_params$param1,  get(translations[id == "param_name_col", get(language$language)])], language$abbrev)), # Name of primary parameter
        p(translations[id == "map_min_yrs_selected1", get(language$language)][[1]], " ", map_params$yrs1, " ", translations[id == "map_min_yrs_selected2", get(language$language)][[1]], # Text for min years selected
          translations[id == "map_date_within_selected1", get(language$language)][[1]], map_params$days1, translations[id == "map_date_within_selected2", get(language$language)][[1]]) # Text for within x days
      )
    })
    output$secondary_param <- renderUI({
      if (map_params$params == 1) {
        return(NULL)
      } else {
        tagList(
          h4(translations[id == "map_second_param", get(language$language)][[1]]), # Text for secondary parameter
          p(titleCase(moduleData$parameters[moduleData$parameters$parameter_id == map_params$param2,  get(translations[id == "param_name_col", get(language$language)])], language$abbrev)), # Name of secondary parameter
          p(translations[id == "map_min_yrs_selected1", get(language$language)][[1]], " ", map_params$yrs2, " ", translations[id == "map_min_yrs_selected2", get(language$language)][[1]], # Text for min years selected
            translations[id == "map_date_within_selected1", get(language$language)][[1]], map_params$days2, translations[id == "map_date_within_selected2", get(language$language)][[1]]) # Text for within x days
        )
      }

    })
    
    # Observe 'map_latest_measurements'. If TRUE, 'target' is adjusted to Sys.Date()
    observeEvent(input$latest, {
      if (input$latest) {
        updateDateInput(session, "target", value = Sys.Date())
        map_params$latest <- TRUE
        map_params$target <- Sys.Date()
      } else {
        map_params$latest <- FALSE
        map_params$target <- input$target
      }
    })
    
    # edit primary and secondary parameter options in a modal
    observeEvent(input$edit_primary_param, {
      showModal(modalDialog(
        title = NULL,
        selectizeInput(
          ns("param"),
          label = translations[id == "parameter", get(language$language)][[1]],
          choices = stats::setNames(
            moduleData$parameters$parameter_id,
            titleCase(moduleData$parameters[[translations[id == "param_name_col", get(language$language)][[1]]]], language$abbrev)
          ),
          selected = map_params$param1,
          multiple = FALSE
        ),
        numericInput(ns("yrs"),
                     label = translations[id == "map_min_yrs", get(language$language)][[1]],
                     value = map_params$yrs1,
                     min = 3,
                     max = 100,
                     step = 1),
        numericInput(ns("days"),
                     label = translations[id == "map_date_within", get(language$language)][[1]],
                     value = map_params$days1,
                     min = 0,
                     max = 365,
                     step = 1),
        footer = tagList(
          actionButton(ns("save_primary_param"), translations[id == "save", get(language$language)][[1]]),
          actionButton(ns("close"), translations[id == "close", get(language$language)][[1]])
        )
      ))
    })
    observeEvent(input$save_primary_param, {
      map_params$param1 <- input$param
      map_params$yrs1 <- input$yrs
      map_params$days1 <- input$days
      removeModal()
    })
    
    observeEvent(input$edit_secondary_param, {
      showModal(modalDialog(
        title = NULL,
        selectizeInput(
          ns("param"),
          label = translations[id == "parameter", get(language$language)][[1]],
          choices = stats::setNames(
            moduleData$parameters$parameter_id,
            titleCase(moduleData$parameters[[translations[id == "param_name_col", get(language$language)][[1]]]], language$abbrev)
          ),
          selected = map_params$param2,
          multiple = FALSE
        ),
        numericInput(ns("yrs"),
                     label = translations[id == "map_min_yrs", get(language$language)][[1]],
                     value = map_params$yrs2,
                     min = 3,
                     max = 100,
                     step = 1),
        numericInput(ns("days"),
                     label = translations[id == "map_date_within", get(language$language)][[1]],
                     value = map_params$days2,
                     min = 0,
                     max = 365,
                     step = 1),
        footer = tagList(
                  actionButton(ns("save_secondary_param"), translations[id == "save", get(language$language)][[1]]),
                  if (map_params$params == 2) {
                    actionButton(ns("remove_secondary_param"), translations[id == "map_rm_second_param", get(language$language)][[1]])
                  },
                  actionButton(ns("close"), translations[id == "close", get(language$language)][[1]])
        )
      ))
    })
    observeEvent(input$save_secondary_param, {
      if (map_params$params == 1) {
        map_params$params <- 2
      }
      updateActionButton(session, "edit_secondary_param", translations[id == "map_edit_second_param", get(language$language)][[1]])
      map_params$param2 <- input$param
      map_params$yrs2 <- input$yrs
      map_params$days2 <- input$days
      removeModal()
    })
    observeEvent(input$remove_secondary_param, {
      map_params$params <- 1
      updateActionButton(session, "edit_secondary_param", translations[id == "map_add_second_param", get(language$language)][[1]])
      removeModal()
    })
    
    # remove any modal
    observeEvent(input$close, {
      removeModal()
    })
    
    # Update the text of multiple elements based on the selected language ############################
    # Not applicable to elements within modals as these are generated every time the modal is opened
    observeEvent(language$language, {
      updateSelectizeInput(session,
                           "mapType",
                           label = NULL,
                           choices = stats::setNames(
                             c("range", "abs"),
                             c(translations[id == "map_relative", get(language$language)][[1]], translations[id == "map_absolute1", get(language$language)][[1]])
                           )
      )
      updateCheckboxInput(session, "latest", label = translations[id == "map_latest_measurements", get(language$language)][[1]])
      updateDateInput(session, "target", label = translations[id == "map_target_date", get(language$language)][[1]])
      updateActionButton(session, "go", label = translations[id == "render_map", get(language$language)][[1]])
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    observeEvent(input$mapType, {
      if (input$mapType == "abs") {
        shinyjs::hide("secondary_param")
        shinyjs::hide("edit_secondary_param")
        
      } else {
        shinyjs::show("secondary_param")
        shinyjs::show("edit_secondary_param")
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$target, {
      map_params$target <- input$target
    })
    
    # Listen for input changes and update the map ########################################################
    updateMap <- function() {
      req(moduleData, map_params$param1, map_params$param2, map_params$yrs1, map_params$yrs2, map_params$days1, map_params$days2, map_params$target, map_params$params, input$map_zoom)

      # integrity checks
      if (is.na(map_params$yrs1) || is.na(map_params$days1)) {
        return()
      }
      if (map_params$params == 2 && (is.na(map_params$yrs2) || is.na(map_params$days2))) {
        return()
      }
      
      # Stop if the parameter does not exist; it's possible that the user typed something in themselves
      if (!map_params$param1 %in% moduleData$parameters$parameter_id) {
        return()
      }
      if (map_params$params == 2 && !map_params$param2 %in% moduleData$parameters$parameter_id) {
        return()
      }
      
      # Deal with parameter 1
      tsids1 <- dbGetQueryDT(session$userData$AquaCache, sprintf(
        "SELECT timeseries_id FROM timeseries WHERE parameter_id = %s;",
        map_params$param1
      ))$timeseries_id
      
      if (map_params$latest) {
        # Pull the most recent measurement in view table measurements_continuous_corrected for each timeseries IF a measurement is available within map_params$days1 days
        closest_measurements1 <- dbGetQueryDT(session$userData$AquaCache, 
                                              paste0("WITH ranked_data AS (
                    SELECT 
                        timeseries_id, 
                        datetime, 
                        value_corrected AS value,
                        ROW_NUMBER() OVER (PARTITION BY timeseries_id ORDER BY datetime DESC) AS row_num
                    FROM 
                        measurements_continuous_corrected
                    WHERE 
                        timeseries_id IN (", paste(tsids1, collapse = ","), ") 
                        AND datetime > '", as.POSIXct(Sys.time(), tz = "UTC") - as.numeric(map_params$days1) - 60*60*24, "'
                )
                SELECT 
                    timeseries_id, datetime, value
                FROM 
                    ranked_data
                WHERE 
                    row_num = 1;")
        )
        
        # For timeseries where there was a measurement above, get historical range data and add
        range1 <- dbGetQueryDT(session$userData$AquaCache, 
                               paste0("WITH ranked_data AS (
                                          SELECT
                                              timeseries_id, 
                                              max, 
                                              min, 
                                              doy_count,
                                              ROW_NUMBER() OVER (PARTITION BY timeseries_id ORDER BY date DESC) AS row_num
                                          FROM 
                                              measurements_calculated_daily_corrected 
                                          WHERE 
                                              doy_count >= ", as.numeric(map_params$yrs1), " 
                                          AND timeseries_id IN (", paste(closest_measurements1$timeseries_id, collapse = ","), ")
                                      )
                                      SELECT 
                                          timeseries_id, max, min, doy_count 
                                      FROM 
                                          ranked_data
                                      WHERE 
                                          row_num = 1;")
        )
        
        # Merge the two sets on timeseries_id so as to get historic range data for each timeseries (this will drop records where there were not enough years of record)
        closest_measurements1 <- merge(closest_measurements1, range1, by = "timeseries_id", all.y = TRUE)
        # Calculate the percent of the historic range using the value, max and min
        closest_measurements1[, percent_historic_range := 100 * (value - min) / (max - min)]
        
      } else { # not requesting latest measurements
        range1 <- dbGetQueryDT(session$userData$AquaCache, paste0("SELECT timeseries_id, date, value, percent_historic_range, max, min, doy_count FROM measurements_calculated_daily_corrected WHERE doy_count >= ", as.numeric(map_params$yrs1), " AND timeseries_id IN (", paste(tsids1, collapse = ","), ") AND date BETWEEN '", map_params$target - as.numeric(map_params$days1), "' AND '", map_params$target + as.numeric(map_params$days1), "';"))
        
        # Calculate the absolute difference in days between each date and the target date
        range1[, date_diff := abs(date - map_params$target)]
        
        # Order the data by 'timeseries_id' and 'date_diff'
        data.table::setorder(range1, timeseries_id, date_diff)
        
        # For each 'timeseries_id', select the row with the smallest 'date_diff'
        closest_measurements1 <- range1[, .SD[1], by = timeseries_id]
      }
      
      
      locs_tsids1 <- merge(moduleData$locations[, c("latitude", "longitude", "location_id", "name", "name_fr")], moduleData$timeseries[moduleData$timeseries$timeseries_id %in% closest_measurements1$timeseries_id, c("timeseries_id", "location_id")], by = "location_id")
      
      locs_tsids1$param_name <- moduleData$parameters[moduleData$parameters$parameter_id == map_params$param1,  get(translations[id == "param_name_col", get(language$language)])]
      locs_tsids1$param_unit <- moduleData$parameters[moduleData$parameters$parameter_id == map_params$param1,  "unit_default"]
      
      
      # Now if the user has selected two parameters, repeat the process for the second parameter BUT only for the locations that did not have a match for the first parameter
      if (map_params$params == 2) {
        tsids2 <- dbGetQueryDT(session$userData$AquaCache, paste0("SELECT timeseries_id FROM timeseries WHERE parameter_id = ", map_params$param2, " AND location_id NOT IN (", paste(locs_tsids1$location_id, collapse = ", "), ");"))$timeseries_id
        
        if (map_params$latest) {
          # Pull the most recent measurement in view table measurements_continuous_corrected for each timeseries IF a measurement is available within map_params$days2 days
          closest_measurements2 <- dbGetQueryDT(session$userData$AquaCache, 
                                                paste0("WITH ranked_data AS (
                      SELECT 
                          timeseries_id, 
                          datetime, 
                          value_corrected AS value,
                          ROW_NUMBER() OVER (PARTITION BY timeseries_id ORDER BY datetime DESC) AS row_num
                      FROM 
                          measurements_continuous_corrected
                      WHERE 
                          timeseries_id IN (", paste(tsids2, collapse = ","), ") 
                          AND datetime > '", as.POSIXct(Sys.time(), tz = "UTC") - as.numeric(map_params$days2) - 60*60*24, "'
                  )
                  SELECT 
                      timeseries_id, datetime, value
                  FROM 
                      ranked_data
                  WHERE 
                      row_num = 1;")
          )
          
          # For timeseries where there was a measurement above, get historical range data and add
          range2 <- dbGetQueryDT(session$userData$AquaCache, 
                                 paste0("WITH ranked_data AS (
                                            SELECT
                                                timeseries_id, 
                                                max, 
                                                min, 
                                                doy_count,
                                                ROW_NUMBER() OVER (PARTITION BY timeseries_id ORDER BY date DESC) AS row_num
                                            FROM 
                                                measurements_calculated_daily_corrected 
                                            WHERE 
                                                doy_count >= ", as.numeric(map_params$yrs2), " 
                                            AND timeseries_id IN (", paste(closest_measurements2$timeseries_id, collapse = ","), ")
                                        )
                                        SELECT 
                                            timeseries_id, max, min, doy_count 
                                        FROM 
                                            ranked_data
                                        WHERE 
                                            row_num = 1;")
          )
          
          # Merge the two sets on timeseries_id so as to get historic range data for each timeseries (this will drop records where there were not enough years of record)
          closest_measurements2 <- merge(closest_measurements2, range2, by = "timeseries_id", all.y = TRUE)
          # Calculate the percent of the historic range using the value, max and min
          closest_measurements2[, percent_historic_range := 100 * (value - min) / (max - min)]
          
        } else {
          range2 <- dbGetQueryDT(session$userData$AquaCache, paste0("SELECT timeseries_id, date, value, percent_historic_range, max, min, doy_count FROM measurements_calculated_daily_corrected WHERE doy_count >= ", as.numeric(map_params$yrs2), " AND timeseries_id IN (", paste(tsids2, collapse = ","), ") AND date BETWEEN '", map_params$target - as.numeric(map_params$days2), "' AND '", map_params$target + as.numeric(map_params$days2), "';"))
          
          # Calculate the absolute difference in days between each date and the target date
          range2[, date_diff := abs(date - map_params$target)]
          
          # Order the data by 'timeseries_id' and 'date_diff'
          data.table::setorder(range2, timeseries_id, date_diff)
          
          # For each 'timeseries_id', select the row with the smallest 'date_diff'
          closest_measurements2 <- range2[, .SD[1], by = timeseries_id]
        }
        
        locs_tsids2 <- merge(moduleData$locations[, c("latitude", "longitude", "location_id", "name", "name_fr")], moduleData$timeseries[moduleData$timeseries$timeseries_id %in% closest_measurements2$timeseries_id, c("timeseries_id", "location_id")], by = "location_id")
        
        locs_tsids2$param_name <- moduleData$parameters[moduleData$parameters$parameter_id == map_params$param2,  get(translations[id == "param_name_col", get(language$language)])]
        locs_tsids2$param_unit <- moduleData$parameters[moduleData$parameters$parameter_id == map_params$param2,  "unit_default"]
        
        
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
      
      value_palette <- leaflet::colorBin(
        palette = map_params$colors,
        domain = c(0, 100),
        bins = c(0, 20, 40, 60, 80, 100),
        na.color = "#808080"
      )
      
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
            translations[id == "map_actual_date", get(language$language)][[1]], ": ", if (map_params$latest) paste0(datetime, " UTC") else paste0(date, " (daily mean)"), "<br/>",
            translations[id == "map_relative", get(language$language)][[1]], ": ", round(percent_historic_range, 2), "% <br/>",
            translations[id == "map_absolute2", get(language$language)][[1]], ": ", round(value, 2), " ", param_unit, "<br/>",
            translations[id == "map_actual_hist_range", get(language$language)][[1]], ": ", round(min, 2), " ", translations[id == "to", get(language$language)][[1]], " ", round(max, 2)," ", param_unit, "<br/>",
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
    }
    
    # Create the basic map
    mapCreated <- reactiveVal(FALSE) # Used to track map creation so that points show up right away with defaults
    output$map <- leaflet::renderLeaflet({
      mapCreated(TRUE)
      
      leaflet::leaflet(options = leaflet::leafletOptions(maxZoom = 18)) %>%
        leaflet::addTiles() %>%
        leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Topographic") %>%
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        leaflet::addLayersControl(baseGroups = c("Topographic", "Satellite")) %>%
        leaflet::addScaleBar(position = "bottomleft", 
                             options = leaflet::scaleBarOptions(imperial = FALSE)) %>%
        leaflet::setView(lng = -135.05, lat = 64.00, zoom = 5)
    })
    
    # Observe the map being created and update it when the parameters change
    observe({
      req(mapCreated())  # Ensure the map has been created before updating
      reactiveValuesToList(map_params)  # Triggers whenever map_params changes
      
      try({
        updateMap() 
      })
       
    })
    
    # observeEvent(input$go, { # Reloads the map when the user requests it
    #   updateMap()
    # }, ignoreInit = TRUE, ignoreNULL = TRUE)
  })
}
