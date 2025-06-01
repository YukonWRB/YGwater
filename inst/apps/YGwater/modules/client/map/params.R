mapParamsUI <- function(id) {
  ns <- NS(id)
  
  # All UI elements rendered in server function to allow multi-language functionality
  page_fluid(
    uiOutput(ns("sidebar_page"))
  )
} # End of mapParamsUI

mapParams <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    moduleData <- reactiveValues(
      locations = dbGetQueryDT(session$userData$AquaCache, "SELECT location, name, name_fr, latitude, longitude, location_id, geom_id, visibility_public, location_type FROM locations"),
      timeseries = dbGetQueryDT(session$userData$AquaCache, "SELECT ts.timeseries_id, ts.location_id, p.param_name, p.param_name_fr, m.media_type, ts.media_id, ts.parameter_id, ts.aggregation_type_id, ts.start_datetime, ts.end_datetime, z FROM timeseries AS ts LEFT JOIN parameters AS p ON ts.parameter_id = p.parameter_id LEFT JOIN media_types AS m ON ts.media_id = m.media_id"),
      parameters = dbGetQueryDT(session$userData$AquaCache, "SELECT DISTINCT p.parameter_id, p.param_name, COALESCE(p.param_name_fr, p.param_name) AS param_name_fr, p.unit_default, pr.group_id, pr.sub_group_id FROM parameters AS p RIGHT JOIN timeseries AS ts ON p.parameter_id = ts.parameter_id LEFT JOIN parameter_relationships AS pr ON p.parameter_id = pr.parameter_id;")
    )
    
    output$sidebar_page <- renderUI({
      req(moduleData, language)
      page_sidebar(
        # bg = config$main_bg,  # background for the main panel
        sidebar = sidebar(
          title = NULL,
          bg = config$sidebar_bg, # Set in globals file
          tagList(
            selectizeInput(
              ns("mapType"),
              label = tr("map_mapType", language$language),
              choices = stats::setNames(
                c("range", "abs"),
                c(tr("map_relative", language$language), tr("map_absolute1", language$language))
              ),
              selected = "range",
              multiple = FALSE
            ),
            dateInput(ns("target"),
                      label = tr("map_target_date", language$language),
                      value = Sys.Date(),
                      max = Sys.Date(),
                      format = "yyyy-mm-dd",
                      language = language$abbrev),
            checkboxInput(ns("latest"), tr("map_latest_measurements", language$language), value = TRUE),
            htmlOutput(ns("primary_param")), # Primary parameter information, rendered separately as it needs to update if selections change
            actionButton(ns("edit_primary_param"), tr("map_edit_primary_param", language$language), style = "display: block; width: 100%"),
            if (!config$public) {
              htmlOutput(ns("secondary_param"))
            },
            if (!config$public) {
              actionButton(
                ns("edit_secondary_param"),
                tr(if (map_params$params == 2) "map_edit_second_param" else "map_add_second_param", language$language),
                style = "display: block; width: 100%"
              )
            }
          )
        ),
        # Main panel (left)
        leaflet::leafletOutput(ns("map"), height = '80vh')
      )
    }) |> bindEvent(language$language)
    
    output$primary_param <- renderUI({
      req(map_params, language)
      tagList(
        h4(tr("map_primary_param", language$language)), # Text for primary parameter
        p(titleCase(moduleData$parameters[moduleData$parameters$parameter_id == map_params$param1,  get(tr("param_name_col", language$language))], language$abbrev)), # Name of primary parameter
        p(tr("map_min_yrs_selected1", language$language), " ", map_params$yrs1, " ", tr("map_min_yrs_selected2", language$language), # Text for min years selected
          tr("map_date_within_selected1", language$language), map_params$days1, tr("map_date_within_selected2", language$language)) # Text for within x days
      )
    }) |> bindEvent(map_params$param1, language$language)
    
    output$secondary_param <- renderUI({
      req(map_params, language)
      if (config$public || map_params$params == 1) {
        return(NULL)
      } else {
        tagList(
          h4(tr("map_second_param", language$language)), # Text for secondary parameter
          p(titleCase(moduleData$parameters[moduleData$parameters$parameter_id == map_params$param2,  get(tr("param_name_col", language$language))], language$abbrev)), # Name of secondary parameter
          p(tr("map_min_yrs_selected1", language$language), " ", map_params$yrs2, " ", tr("map_min_yrs_selected2", language$language), # Text for min years selected
            tr("map_date_within_selected1", language$language), map_params$days2, tr("map_date_within_selected2", language$language)) # Text for within x days
        )
      }
    }) |> bindEvent(map_params$param2, language$language)
    
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
      params = 1,
      bins = c(-Inf, 0, 20, 40, 60, 80, 100, Inf),
      colors = c(
        "#8c510a", "#d8b365", "#FEE090",
        "#74ADD1", "#4575D2", "#313695", "#A50026"
      )
    )
    
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
          label = tr("parameter", language$language),
          choices = stats::setNames(
            moduleData$parameters$parameter_id,
            titleCase(moduleData$parameters[[tr("param_name_col", language$language)]], language$abbrev)
          ),
          selected = map_params$param1,
          multiple = FALSE
        ),
        numericInput(ns("yrs"),
                     label = tr("map_min_yrs", language$language),
                     value = map_params$yrs1,
                     min = 3,
                     max = 100,
                     step = 1),
        numericInput(ns("days"),
                     label = tr("map_date_within", language$language),
                     value = map_params$days1,
                     min = 0,
                     max = 365,
                     step = 1),
        footer = tagList(
          actionButton(ns("save_primary_param"), tr("save", language$language)),
          actionButton(ns("close"), tr("close", language$language))
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
          label = tr("parameter", language$language),
          choices = stats::setNames(
            moduleData$parameters$parameter_id,
            titleCase(moduleData$parameters[[tr("param_name_col", language$language)]], language$abbrev)
          ),
          selected = map_params$param2,
          multiple = FALSE
        ),
        numericInput(ns("yrs"),
                     label = tr("map_min_yrs", language$language),
                     value = map_params$yrs2,
                     min = 3,
                     max = 100,
                     step = 1),
        numericInput(ns("days"),
                     label = tr("map_date_within", language$language),
                     value = map_params$days2,
                     min = 0,
                     max = 365,
                     step = 1),
        footer = tagList(
          actionButton(ns("save_secondary_param"), tr("save", language$language)),
          if (map_params$params == 2) {
            actionButton(ns("remove_secondary_param"), tr("map_rm_second_param", language$language))
          },
          actionButton(ns("close"), tr("close", language$language))
        )
      ))
    })
    
    observeEvent(input$save_secondary_param, {
      if (map_params$params == 1) {
        map_params$params <- 2
      }
      updateActionButton(session, "edit_secondary_param", tr("map_edit_second_param", language$language))
      map_params$param2 <- input$param
      map_params$yrs2 <- input$yrs
      map_params$days2 <- input$days
      removeModal()
    })
    observeEvent(input$remove_secondary_param, {
      map_params$params <- 1
      updateActionButton(session, "edit_secondary_param", tr("map_add_second_param", language$language))
      removeModal()
    })
    
    # remove any modal
    observeEvent(input$close, {
      removeModal()
    })
    
    observeEvent(input$mapType, {
      if (input$mapType == "abs" || config$public) {
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
      if (length(tsids1) == 0) {
        return()
      }
      
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
      
      locs_tsids1$param_name <- moduleData$parameters[moduleData$parameters$parameter_id == map_params$param1,  get(tr("param_name_col", language$language))]
      locs_tsids1$param_unit <- moduleData$parameters[moduleData$parameters$parameter_id == map_params$param1,  "unit_default"]
      
      
      # Now if the user has selected two parameters, repeat the process for the second parameter BUT only for the locations that did not have a match for the first parameter
      if (map_params$params == 2) {
        tsids2 <- dbGetQueryDT(session$userData$AquaCache, paste0("SELECT timeseries_id FROM timeseries WHERE parameter_id = ", map_params$param2, " AND location_id NOT IN (", paste(locs_tsids1$location_id, collapse = ", "), ");"))$timeseries_id
        if (length(tsids2) == 0) {
          return()
        }
        
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
        
        locs_tsids2$param_name <- moduleData$parameters[moduleData$parameters$parameter_id == map_params$param2,  get(tr("param_name_col", language$language))]
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
      mapping_data[, percent_historic_range_capped := percent_historic_range]

      if (input$mapType == "abs") {
        abs_vals <- abs(mapping_data$value)
        abs_range <- range(abs_vals, na.rm = TRUE)
        abs_bins <- seq(abs_range[1], abs_range[2], length.out = length(map_params$colors) + 1)

        value_palette <- leaflet::colorBin(
          palette = map_params$colors,
          domain = abs_vals,
          bins = abs_bins,
          pretty = FALSE,
          na.color = "#808080"
        )
        map_values <- abs_vals
        lab_format <- leaflet::labelFormat()
      } else {
        value_palette <- leaflet::colorBin(
          palette = map_params$colors,
          domain = mapping_data$percent_historic_range_capped,
          bins = map_params$bins,
          pretty = FALSE,
          na.color = "#808080"
        )
        map_values <- mapping_data$percent_historic_range_capped
        lab_format <- leaflet::labelFormat(suffix = "%")
      }
      
      leaflet::leafletProxy("map", session) %>%
        leaflet::clearMarkers() %>%
        leaflet::addCircleMarkers(
          data = mapping_data,
          lng = ~longitude,
          lat = ~latitude,
          fillColor = ~value_palette(if (input$mapType == "abs") abs(value) else percent_historic_range_capped),
          color = ~value_palette(if (input$mapType == "abs") abs(value) else percent_historic_range_capped),
          fillOpacity = 1,
          stroke = TRUE,
          weight = 1,
          radius = 8,
          popup = ~paste0(
            "<strong>", get(tr("generic_name_col", language$language)),  "</strong><br/>",
            titleCase(param_name, language$abbrev), "<br>",
            tr("map_actual_date", language$language), ": ", if (map_params$latest) paste0(datetime, " UTC") else paste0(date, " (daily mean)"), "<br/>",
            tr("map_relative", language$language), ": ", round(percent_historic_range, 2), "% <br/>",
            tr("map_absolute2", language$language), ": ", round(value, 2), " ", param_unit, "<br/>",
            tr("map_actual_hist_range", language$language), ": ", round(min, 2), " ", tr("to", language$language), " ", round(max, 2)," ", param_unit, "<br/>",
            tr("map_actual_yrs", language$language), ": ", doy_count
          )
        ) %>%
        leaflet::clearControls() %>%  # Clear existing legends
        leaflet::addLegend(
          position = "bottomright",
          pal = value_palette,
          values = map_values,
          title = NULL,
          labFormat = lab_format,
          opacity = 1
        )
    }
    
    # Create the basic map
    mapCreated <- reactiveVal(FALSE) # Used to track map creation so that points show up right away with defaults
    output$map <- leaflet::renderLeaflet({
      mapCreated(TRUE)
      
      leaflet::leaflet(options = leaflet::leafletOptions(maxZoom = 15)) %>%
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
      req(mapCreated(), map_params, input$map_zoom)  # Ensure the map has been created before updating
      try({
        updateMap() 
      })
    })
    
  }) # End of moduleServer
} # End of mapParams server function
