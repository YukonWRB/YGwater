mapRasterUI <- function(id) {
  ns <- NS(id)

  # All UI elements rendered in server function to allow multi-language functionality
  page_fluid(
    uiOutput(ns("sidebar_page"))
  )
} # End of mapRasterUI

mapRaster <- function(id, language) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    if (session$userData$user_logged_in) {
      cached <- map_params_module_data(con = session$userData$AquaCache, env = session$userData$app_cache)
    } else {
      cached <- map_params_module_data(con = session$userData$AquaCache)
    }

    # Query available raster models and their types from raster_series_index
    models_types <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT DISTINCT model, type FROM spatial.raster_series_index")
    models_types$model_type <- paste0(models_types$model, " (", models_types$type, ")")

    moduleData <- reactiveValues(
      locations = cached$locations,
      timeseries = cached$timeseries,
      parameters = cached$parameters
    )

    output$sidebar_page <- renderUI({
      req(moduleData, language)
      page_sidebar(
        sidebar = sidebar(
          title = NULL,
          bg = config$sidebar_bg, # Set in globals file
          open = list(mobile = "always-above"),
          tagList(
            selectizeInput(
              ns("param"),
              label = tr("parameter", language$language),
              choices = stats::setNames(
                moduleData$parameters$parameter_id,
                titleCase(moduleData$parameters[[tr("param_name_col", language$language)]], language$abbrev)
              ),
              selected = map_params$point_id,
              multiple = FALSE
            ),

            # selectizeInput(
            #   ns("mapType"),
            #   label = tr("map_mapType", language$language),
            #   choices = stats::setNames(
            #     c("range", "abs"),
            #     c(tr("map_relative", language$language), tr("map_absolute1", language$language))
            #   ),
            #   selected = "range",
            #   multiple = FALSE
            # ),


            dateInput(
              ns("target"),
              label = tr("map_target_date", language$language),
              value = Sys.Date(),
              max = Sys.Date(),
              format = "yyyy-mm-dd",
              language = language$abbrev
            ),



            checkboxInput(ns("latest"), tr("map_latest_measurements", language$language), value = FALSE),
            
              numericInput(
              ns("days"),
              label = tr("map_date_within", language$language),
              value = map_params$days,
              min = 0,
              max = 365,
              step = 1
            ),


            selectizeInput(
              ns("targetDataset"),
              label = "Raster dataset",
              choices = stats::setNames(
                models_types$model,
                models_types$model_type
              ),
              selected = "",
              multiple = FALSE
            ),
            selectizeInput(
              ns("targetParam"),
              label = "Raster parameter",
              choices = "",
              selected = "",
              multiple = FALSE
            )
          )
        ),
        # Main panel (left)
        leaflet::leafletOutput(ns("map"), height = '80vh')
      )
    }) |> bindEvent(language$language)

    output$primary_param <- renderUI({
      req(map_params, language)
      tagList(
        h4(if (config$public) tr("map_primary_param_solo", language$language) else tr("map_primary_param", language$language)), # Text for primary parameter
        p(titleCase(moduleData$parameters[moduleData$parameters$parameter_id == map_params$point_id,  get(tr("param_name_col", language$language))], language$abbrev)), # Name of primary parameter
        p(
          tr("map_min_yrs_selected1", language$language), " ", map_params$yrs, " ", tr("map_min_yrs_selected2", language$language), # Text for min years selected
          tr("map_date_within_selected1", language$language), map_params$days, tr("map_date_within_selected2", language$language) # Text for within x days
        )
      )
    }) |> bindEvent(map_params$point_id, language$language)

    # PLACEHOLDER OUTPUT RASTER

    # Create the filter inputs ############################################################################
    map_params <- reactiveValues(
      point_id = 1150,  # Water flow
      point_name = 'Flow',
      point_units = 'm3/s',
      raster_id = NULL,
      raster_name = '',
      raster_units = '',
      yrs = 10,
      days = 1,
      latest = FALSE,
      target = Sys.Date(),
      params = 1,
      bins = c(-Inf, 0, 20, 40, 60, 80, 100, Inf),
      colors = c(
        "#8c510a", "#d8b365", "#FEE090",
        "#74ADD1", "#4575D2", "#313695", "#A50026"
      )
    )

    observeEvent(input$param, {
      map_params$point_id <- input$param
      
      map_params$point_name <- moduleData$parameters[moduleData$parameters$parameter_id == map_params$point_id, get(tr("param_name_col", language$language))]
      
      
      map_params$point_units <- moduleData$parameters[moduleData$parameters$parameter_id == map_params$point_id,"unit_default"]

    })

    observeEvent(input$targetParam, {
      map_params$raster_id <- input$targetParam

      if (!is.null(map_params$raster_id) && nzchar(map_params$raster_id)) {
        map_params$raster_name <- dbGetQueryDT(
          session$userData$AquaCache,
          sprintf(
        "SELECT parameter FROM spatial.raster_series_index WHERE raster_series_id = %s LIMIT 1",
        as.character(map_params$raster_id)
          )
        )

        units <- dbGetQueryDT(
          session$userData$AquaCache,
          sprintf(
        "SELECT units FROM spatial.rasters_reference WHERE raster_series_id = %s LIMIT 1",
        as.character(map_params$raster_id)
          )
        )
        if (nrow(units) > 0) {
          map_params$raster_units <- units$units[1]
        } else {
          map_params$raster_units <- ""
        }
      } else {
        map_params$raster_name <- ""
        map_params$raster_units <- ""
      }
    })

    # Observe 'map_latest_measurements'. If TRUE, 'target' is adjusted to Sys.Date()
    observeEvent(input$latest, {
      if (input$latest) {
        # Show the user a modal that explains that the most recent values will be compared against daily means
        showModal(modalDialog(
          tr("map_latest_measurements_modal", language$language),
          easyClose = TRUE,
          footer = modalButton(tr("close", language$language))
        ))
        updateDateInput(session, "target", value = Sys.Date())
        map_params$latest <- TRUE
        map_params$target <- Sys.Date()
      } else {
        map_params$latest <- FALSE
        map_params$target <- input$target
      }
    })

    # remove any modal
    observeEvent(input$close, {
      removeModal()
    })

    observeEvent(input$target, {
      map_params$target <- input$target
    })

    observeEvent(input$targetDataset, {
      req(input$targetDataset)
      # Update the targetParam choices based on the selected dataset
      selected_model <- models_types[models_types$model == input$targetDataset, ]
      if (nrow(selected_model) > 0) {
        params <- dbGetQueryDT(
          session$userData$AquaCache,
            sprintf(
            "SELECT parameter, raster_series_id FROM spatial.raster_series_index WHERE model = '%s' AND type = '%s'",
            selected_model$model, selected_model$type
            )
        )

        
        updateSelectizeInput(session, "targetParam", choices = stats::setNames(params$raster_series_id, params$parameter), selected = "")
      } else {
        updateSelectizeInput(session, "targetParam", choices = character(0), selected = "")
      }
    })

    # Listen for input changes and update the map ########################################################
    updateMap <- function() {


      leaflet::leafletProxy("map", session) %>% leaflet::clearControls()


      # integrity checks
      if (is.na(map_params$yrs) || is.na(map_params$days)) {
        return()
      }

      # Stop if the parameter does not exist; it's possible that the user typed something in themselves
      if (!map_params$point_id %in% moduleData$parameters$parameter_id) {
        return()
      }

      # Deal with parameter 1
      tsids1 <- dbGetQueryDT(session$userData$AquaCache, sprintf(
        "SELECT timeseries_id FROM timeseries WHERE parameter_id = %s;",
        map_params$point_id
      ))$timeseries_id
      if (length(tsids1) == 0) {
        return()
      }

      # Deal with raster
      print(paste0("Selected raster series ID: ", map_params$raster_id))
      
      # Query valid_from, valid_to, and reference_id for the selected raster series
      if (!is.null(map_params$raster_id) && nzchar(map_params$raster_id)) {
        raster_dates <- dbGetQueryDT(
          session$userData$AquaCache,
          paste0(
        "SELECT valid_from, valid_to, reference_id FROM spatial.rasters_reference WHERE raster_series_id = ",
        as.character(map_params$raster_id)
          )
        )
        # Convert valid_from and valid_to to POSIXct datetime array (if not already)

              # Calculate midpoint date as average of valid_from and valid_to
      if (exists("raster_dates") && nrow(raster_dates) > 0) {
        raster_dates$valid_from <- as.POSIXct(raster_dates$valid_from, tz = "UTC")
        raster_dates$valid_to <- as.POSIXct(raster_dates$valid_to, tz = "UTC")
        raster_dates$midpoint <- as.POSIXct((as.numeric(raster_dates$valid_from) + as.numeric(raster_dates$valid_to)) / 2, origin = "1970-01-01", tz = "UTC")
        print(raster_dates)
      }

      # Calculate the difference in days between valid_from and map_params$target
      raster_dates$date_diff_days <- as.numeric(difftime(map_params$target, raster_dates$valid_from, units = "days"))
      raster_dates <- raster_dates[order(abs(raster_dates$date_diff_days)), ]
      if (nrow(raster_dates) > 0) {
        selected_reference_id <- raster_dates$reference_id[1]
        date_diff <- raster_dates$date_diff_days[1]
      } else {
        selected_reference_id <- NA
        date_diff <- NA
      }


      if (!is.na(selected_reference_id)) {
        r_db <- getRaster(
          session$userData$AquaCache,
          name = c("spatial", "rasters"),
          clause = sprintf("WHERE reference_id = %d", selected_reference_id),
          bands = 1
        )

        if (!is.null(r_db)) {
          legend_title <- paste0(map_params$raster_name, " (", map_params$raster_units, ")")

          leaflet::leafletProxy("map", session) %>%
            leaflet::clearImages() %>%
            leaflet::addRasterImage(
              r_db,
              colors = leaflet::colorNumeric(
                palette = "viridis",
                domain = raster::values(r_db),
                na.color = "#808080"
              ),
              opacity = 0.9,
              project = TRUE,
              layerId = "rasterLayer"
            ) %>%
            leaflet::addLegend(
              position = "bottomleft",
              pal = leaflet::colorNumeric(
                palette = "viridis",
                domain = raster::values(r_db),
                na.color = "#808080"
              ),
              values = raster::values(r_db),
              title = legend_title
            )
        }
        # You can add further processing of r_db here if needed
      }


      }


      if (map_params$latest) {
        # Pull the most recent measurement in view table measurements_continuous_corrected for each timeseries IF a measurement is available within map_params$days1 days
        closest_measurements1 <- dbGetQueryDT(
          session$userData$AquaCache,
          paste0(
            "WITH ranked_data AS (
              SELECT 
                timeseries_id, 
                datetime, 
                value_corrected AS value,
                ROW_NUMBER() OVER (PARTITION BY timeseries_id ORDER BY datetime DESC) AS row_num
              FROM 
                measurements_continuous_corrected
              WHERE 
                timeseries_id IN (", paste(tsids1, collapse = ","), ") 
                AND datetime > '", as.POSIXct(Sys.time(), tz = "UTC") - as.numeric(map_params$days) - 60*60*24, "'
            )
            SELECT 
              timeseries_id, datetime, value
            FROM 
              ranked_data
            WHERE 
              row_num = 1;"
          )
        )

        # For timeseries where there was a measurement above, get historical range data and add
        range1 <- dbGetQueryDT(
          session$userData$AquaCache,
          paste0(
            "WITH ranked_data AS (
              SELECT
                timeseries_id, 
                max, 
                min, 
                doy_count,
                ROW_NUMBER() OVER (PARTITION BY timeseries_id ORDER BY date DESC) AS row_num
              FROM 
                measurements_calculated_daily_corrected 
              WHERE 
                doy_count >= ", as.numeric(map_params$yrs), " 
                AND timeseries_id IN (", paste(closest_measurements1$timeseries_id, collapse = ","), ")
            )
            SELECT 
              timeseries_id, max, min, doy_count 
            FROM 
              ranked_data
            WHERE 
              row_num = 1;"
          )
        )

        # Merge the two sets on timeseries_id so as to get historic range data for each timeseries (this will drop records where there were not enough years of record)
        closest_measurements1 <- merge(closest_measurements1, range1, by = "timeseries_id", all.y = TRUE)
        # Calculate the percent of the historic range using the value, max and min
        closest_measurements1[, percent_historic_range := 100 * (value - min) / (max - min)]

      } else { # not requesting latest measurements
        range1 <- dbGetQueryDT(
          session$userData$AquaCache,
          paste0(
            "SELECT timeseries_id, date, value, percent_historic_range, max, min, doy_count FROM measurements_calculated_daily_corrected WHERE doy_count >= ",
            as.numeric(map_params$yrs), " AND timeseries_id IN (", paste(tsids1, collapse = ","), ") AND date BETWEEN '",
            map_params$target - as.numeric(map_params$days), "' AND '", map_params$target + as.numeric(map_params$days), "';"
          )
        )

        # Calculate the absolute difference in days between each date and the target date
        range1[, date_diff := abs(date - map_params$target)]

        # Order the data by 'timeseries_id' and 'date_diff'
        data.table::setorder(range1, timeseries_id, date_diff)

        # For each 'timeseries_id', select the row with the smallest 'date_diff'
        closest_measurements1 <- range1[, .SD[1], by = timeseries_id]
      }

      locs_tsids1 <- merge(
        moduleData$locations[, c("latitude", "longitude", "location_id", "name", "name_fr")],
        moduleData$timeseries[moduleData$timeseries$timeseries_id %in% closest_measurements1$timeseries_id, c("timeseries_id", "location_id")],
        by = "location_id"
      )
      print(moduleData$parameters$parameter_id)
      locs_tsids1$param_name <- moduleData$parameters[moduleData$parameters$parameter_id == map_params$point_id,  get(tr("param_name_col", language$language))]
      locs_tsids1$param_unit <- moduleData$parameters[moduleData$parameters$parameter_id == map_params$point_id,  "unit_default"]

      # Now if the user has selected two parameters, repeat the process for the second parameter BUT only for the locations that did not have a match for the first parameter

      mapping_data <- merge(
        closest_measurements1,
        locs_tsids1,
        by = "timeseries_id",
        all.x = TRUE
      )
      mapping_data[, percent_historic_range_capped := percent_historic_range]



        abs_vals <- abs(mapping_data$value)

        if (length(abs_vals) == 0) {
          leaflet::leafletProxy("map", session) %>% leaflet::clearMarkers() %>% leaflet::clearControls()
          return()
        }

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
        legend_digits <- function(vals) {
          if (length(vals) == 0 || all(!is.finite(vals))) return(0)
          max_val <- max(abs(vals), na.rm = TRUE)
          if (max_val >= 100) {
            return(0)
          } else if (max_val >= 10) {
            return(1)
          } else {
            return(2)
          }
        }
        lab_format <- leaflet::labelFormat(digits = legend_digits(abs_vals))
        legend_title <- sprintf(
          "%s (%s)",
          titleCase(map_params$point_name, language$abbrev),
          map_params$point_units
        )
      
      leaflet::leafletProxy("map", session) %>%
        leaflet::clearMarkers() %>%
        leaflet::addCircleMarkers(
          data = mapping_data,
          lng = ~longitude,
          lat = ~latitude,
          fillColor = ~value_palette(abs(value)),
          color = ~value_palette(abs(value)),
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
        leaflet::addLegend(
          position = "bottomright",
          pal = value_palette,
          values = map_values,
          title = legend_title,
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
        leaflet::addScaleBar(
          position = "topleft",
          options = leaflet::scaleBarOptions(imperial = FALSE)
        ) %>%
        leaflet::setView(lng = -135.05, lat = 64.00, zoom = 5)
    })

    # Observe the map being created and update it when the parameters change
    observe({
      req(mapCreated(), map_params, language$language)  # Ensure the map has been created before updating
      updateMap()  # Call the updateMap function to refresh the map with the current parameters
    })
  }) # End of moduleServer
} # End of mapParams server function
