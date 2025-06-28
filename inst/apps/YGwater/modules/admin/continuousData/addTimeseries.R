# UI and server code for add new location module

addTimeseriesUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    page_fluid(
      uiOutput(ns("ui"))
    )
  )
}

addTimeseries <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    moduleData <- reactiveValues()
    
    getModuleData <- function() {
      moduleData$timeseries <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT timeseries_id, location_id, sub_location_id, z, media_id, parameter_id, aggregation_type_id, sensor_priority, default_owner, record_rate, source_fx, source_fx_args, note FROM timeseries")
      moduleData$locations <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT l.location_id, l.name, lt.type, l.latitude, l.longitude FROM locations l INNER JOIN location_types lt ON l.location_type = lt.type_id")
      moduleData$sub_locations <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT sub_location_id, sub_location_name, location_id FROM sub_locations")
      moduleData$parameters <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT parameter_id, param_name FROM parameters")
      moduleData$media <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT media_id, media_type FROM media_types")
      moduleData$aggregation_types <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT aggregation_type_id, aggregation_type FROM aggregation_types")
      moduleData$organizations <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT organization_id, name FROM organizations")
      moduleData$users = DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM get_roles_with_select_on_locations();")  # This is a helper function run with SECURITY DEFINER and created by postgres that pulls all users with select privileges on locations table
      
      moduleData$timeseries_display <- DBI::dbGetQuery(session$userData$AquaCache, "
        SELECT ts.timeseries_id, l.name AS location_name, sl.sub_location_name, p.param_name, m.media_type, at.aggregation_type, ts.z AS depth_height_m, ts.sensor_priority, o.name AS owner, ts.record_rate
        FROM timeseries ts
        INNER JOIN locations l ON ts.location_id = l.location_id
        LEFT JOIN sub_locations sl ON ts.sub_location_id = sl.sub_location_id
        INNER JOIN parameters p ON ts.parameter_id = p.parameter_id
        INNER JOIN media_types m ON ts.media_id = m.media_id
        INNER JOIN aggregation_types at ON ts.aggregation_type_id = at.aggregation_type_id
        INNER JOIN organizations o ON ts.default_owner = o.organization_id
        ")
    }
    
    getModuleData()  # Initial data load
    
    choices <- ls(getNamespace("AquaCache"))
    moduleData$source_fx <- choices[grepl("^download", choices)]
    
    output$ui <- renderUI({
      tagList(
        radioButtons(ns("mode"), NULL,
                     choices = c("Add new" = "add", "Modify existing" = "modify"),
                     inline = TRUE),
        conditionalPanel(
          condition = "input.mode == 'modify'",
          ns = ns,
          DT::DTOutput(ns("ts_table"))
        ),
        conditionalPanel(
          condition = "input.mode == 'add'",
          ns = ns,
          tags$div(
            class = "alert alert-info",
            "Tip: if you add a new timeseries with a source_fx and appropriate arguments, data will automatically be fetched from the source when you click 'Add timeseries'. If you leave the source_fx blank, you can enter data manually or use other methods. Note that WSC timeseries will get daily mean measurements as well as realtime measurements as far back as exist."),
        ),
        selectizeInput(ns("location"), "Location (add new under the 'locations' menu)", 
                       choices = stats::setNames(moduleData$locations$location_id, moduleData$locations$name),
                       multiple = TRUE,
                       options = list(maxItems = 1,
                                      placeholder = 'Select a location'),
                       width = "100%"
        ),
        selectizeInput(ns("sub_location"), "Sub-location (add new under the 'locations' menu)",
                       choices = stats::setNames(moduleData$sub_locations$sub_location_id, moduleData$sub_locations$sub_location_name),
                       multiple = TRUE,
                       options = list(maxItems = 1,
                                      placeholder = 'Optional'),
                       width = "100%"
        ),
        checkboxInput(ns("z_specify"), "Specify an elevation or depth?", value = FALSE),
        numericInput(ns("z"), "Elevation or depth, m (signed appropriately)", value = NA, min = -1000, max = 10000,
                     width = "100%"),
        
        selectizeInput(ns("parameter"), "Parameter",
                       choices = stats::setNames(moduleData$parameters$parameter_id, moduleData$parameters$param_name),
                       multiple = TRUE,
                       options = list(maxItems = 1,
                                      placeholder = 'Select a parameter'),
                       width = "100%"
        ),
        selectizeInput(ns("media"), "Media",
                       choices = stats::setNames(moduleData$media$media_id, moduleData$media$media_type),
                       multiple = TRUE,
                       options = list(maxItems = 1,
                                      placeholder = 'Select media type'),
                       width = "100%"
        ),
        selectizeInput(ns("aggregation_type"), "Aggregation type",
                       choices = stats::setNames(moduleData$aggregation_types$aggregation_type_id, moduleData$aggregation_types$aggregation_type),
                       multiple = TRUE,
                       options = list(maxItems = 1,
                                      placeholder = 'Select aggregation type'),
                       width = "100%"
        ),
        textInput(ns("record_rate"), "Rough record rate, as  '5 minutes', '1 hour', '1 day', '1 week', '4 weeks', '1 month', etc.", value = ""),
        selectizeInput(ns("sensor_priority"), "Sensor priority",
                       choices = c("Primary" = 1, "Secondary" = 2, "Tertiary" = 3),
                       selected = 1,
                       multiple = TRUE,
                       options = list(maxItems = 1,
                                      placeholder = 'Select sensor priority'),
                       width = "100%"
        ),
        selectizeInput(ns("default_owner"), "Default owner",
                       choices = stats::setNames(moduleData$organizations$organization_id, moduleData$organizations$name),
                       multiple = TRUE,
                       options = list(maxItems = 1,
                                      placeholder = 'Select default owner'),
                       width = "100%"
        ),
        selectizeInput(ns("share_with"), 
                       "Share with groups (1 or more, type your own if not in list)", 
                       choices = moduleData$users$role_name,
                       selected = "public_reader",
                       multiple = TRUE,
                       width = "100%"
        ),
        # htmlOutput to tell the user when they should use the source functions and what the arguments are
        tags$div(
          class = "alert alert-info",
          "The source function is used to download data from external sources using the AquaCache R package. It is optional and can be left blank if you are entering data manually or using other methods. For more information on the available source functions, please refer to the AquaCache package documentation."
        ),
        selectizeInput(ns("source_fx"), "Source function (see AquaCache package documentation for details)",
                       choices = moduleData$source_fx,
                       multiple = TRUE,
                       options = list(maxItems = 1,
                                      placeholder = 'Select source function'),
                       width = "100%"
        ),
        # htmlOutput to tell the user how the source function arguments should be formatted
        tags$div(
          class = "alert alert-info",
          "Source function arguments must be formatted as key-value pairs for conversion to JSON, e.g. 'arg1: value1, arg2: value2'.",
          "If the source function does not require any arguments, leave this field blank."
        ),
        textInput(ns("source_fx_args"), "Source function arguments ", 
                  value = "", 
                  placeholder = "arg1: value1, arg2: value2",
                  width = "100%"),
        textAreaInput(ns("note"), "Note (optional)", 
                      value = "", 
                      rows = 3, 
                      placeholder = "Any additional information about this timeseries (optional)",
                      width = "100%"),
        actionButton(ns("add_timeseries"), "Add timeseries", class = "btn-primary")
      )
    })
    
    # Render the timeseries table for modification
    output$ts_table <- DT::renderDT({
      print("Rendering timeseries table")
      DT::datatable(
        moduleData$timeseries_display,
        selection = "single",
        options = list(
          columnDefs = list(list(targets = 0, visible = FALSE)),
          scrollX = TRUE,
          initComplete = htmlwidgets::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({",
            "  'background-color': '#079',",
            "  'color': '#fff',",
            "  'font-size': '100%',",
            "});",
            "$(this.api().table().body()).css({",
            "  'font-size': '90%',",
            "});",
            "}"
          )
        ),
        rownames = FALSE)
    }) |> bindEvent(moduleData$timeseries_display)

    observeEvent(input$z_specify, {
      if (input$z_specify) {
        shinyjs::show("z")
      } else {
        shinyjs::hide("z")
      }
    })
    
    ## Observers to modify existing entry ##########################################
    selected_tsid <- reactiveVal(NULL)
    
    # Observe row selection and update inputs accordingly
    observeEvent(input$ts_table_rows_selected, {
      sel <- input$ts_table_rows_selected
      if (length(sel) > 0) {
        tsid <- moduleData$timeseries_display[sel, "timeseries_id"]
        selected_tsid(tsid)
        # Fetch the record from the basic timeseries table, not the timeseries_display as we need the numeric keys
        details <- moduleData$timeseries[moduleData$timeseries$timeseries_id == tsid, ]
        if (nrow(details) > 0) {
          # Update inputs with the selected timeseries details
          updateSelectizeInput(session, "location", selected = details$location_id)
          updateSelectizeInput(session, "sub_location", selected = details$sub_location_id)
          updateCheckboxInput(session, "z_specify", value = if (!is.na(details$z)) TRUE else FALSE)
          updateNumericInput(session, "z", value = ifelse(is.na(details$z), NA, details$z))
          updateSelectizeInput(session, "parameter", selected = details$parameter_id)
          updateSelectizeInput(session, "media", selected = details$media_id)
          updateSelectizeInput(session, "aggregation_type", selected = details$aggregation_type_id)
          updateTextInput(session, "record_rate", value = ifelse(is.na(details$record_rate), "", details$record_rate))
          updateSelectizeInput(session, "sensor_priority", selected = details$sensor_priority)
          updateSelectizeInput(session, "default_owner", selected = details$default_owner)
          updateSelectizeInput(session, "source_fx", selected = details$source_fx)
          updateTextInput(session, "source_fx_args", value = ifelse(is.na(details$source_fx_args), "", details$source_fx_args))
          updateTextAreaInput(session, "note", value = ifelse(is.na(details$note), "", details$note))
        } else {
          showNotification("Selected timeseries not found in the database.", type = "error")
        }
      } else {
        selected_tsid(NULL)
      }
    })
    
    observeEvent(input$mode, {
      if (input$mode == "modify") {
        updateActionButton(session, "add_timeseries", label = "Update timeseries")
      } else {
        updateActionButton(session, "add_timeseries", label = "Add timeseries")
      }
    })
    
    observeEvent(input$add_timeseries, {
      # validate inputs
      shiny::validate(
        shiny::need(input$location, "Please select a location."),
        shiny::need(input$parameter, "Please select a parameter."),
        shiny::need(input$media, "Please select a media type."),
        shiny::need(input$aggregation_type, "Please select an aggregation type."),
        shiny::need(input$default_owner, "Please select a default owner."),
        shiny::need(input$sensor_priority, "Please select a sensor priority.")
      )
      
      
      if (input$mode == "modify") {
        # If we are modifying an existing timeseries, we need to check if it exists
        selected_row <- input$loc_table_rows_selected
        if (is.null(selected_row) || length(selected_row) != 1) {
          shiny::showNotification("Please select a single timeseries to modify.", type = "error")
          return()
        }
        selected_timeseries <- moduleData$timeseries[selected_row, ]
        # Check if the timeseries already exists
        existing_timeseries <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT * FROM timeseries WHERE timeseries_id = ", selected_timeseries$timeseries_id))
        if (nrow(existing_timeseries) == 0) {
          shiny::showNotification("Selected timeseries does not exist in the database.", type = "error")
          return()
        }
        
        # If it exists, update the timeseries
        DBI::dbBegin(session$userData$AquaCache)
        
        tryCatch({
          if (input$location != selected_timeseries$location_id) {
            DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET location_id = '", input$location, "' WHERE timeseries_id = ", selected_timeseries$timeseries_id))
          }
          if (!is.na(selected_timeseries$sub_location_id)) {
            if (input$sub_location != selected_timeseries$sub_location_id) {
              DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET sub_location_id = '", input$sub_location, "' WHERE timeseries_id = ", selected_timeseries$timeseries_id))
            }
          } else {
            if (!is.na(input$sub_location) && input$sub_location != "") {
              DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET sub_location_id = '", input$sub_location, "' WHERE timeseries_id = ", selected_timeseries$timeseries_id))
            }
          }
          
          if (input$z_specify) {
            if (!is.na(selected_timeseries$z)) {
              if (input$z != selected_timeseries$z) {
                DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET z = ", input$z, " WHERE timeseries_id = ", selected_timeseries$timeseries_id))
              } else if (!input$z_specify) {
                DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET z = NULL WHERE timeseries_id = ", selected_timeseries$timeseries_id))
              }
            } else {
              DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET z = ", input$z, " WHERE timeseries_id = ", selected_timeseries$timeseries_id))
            }
          } else {
            DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET z = NULL WHERE timeseries_id = ", selected_timeseries$timeseries_id))
          }
          
          if (input$parameter != selected_timeseries$parameter_id) {
            DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET parameter_id = '", input$parameter, "' WHERE timeseries_id = ", selected_timeseries$timeseries_id))
          }
          
          if (input$media != selected_timeseries$media_id) {
            DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET media_id = '", input$media, "' WHERE timeseries_id = ", selected_timeseries$timeseries_id))
          }
          
          if (input$aggregation_type != selected_timeseries$aggregation_type_id) {
            DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET aggregation_type_id = '", input$aggregation_type, "' WHERE timeseries_id = ", selected_timeseries$timeseries_id))
          }
          
          if (!is.na(selected_timeseries$record_rate)) {
            if (input$record_rate != selected_timeseries$record_rate) {
              if (input$record_rate != "") {
                DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET record_rate = '", input$record_rate, "' WHERE timeseries_id = ", selected_timeseries$timeseries_id))
              } else {
                DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET record_rate = NULL WHERE timeseries_id = ", selected_timeseries$timeseries_id))
              }
            }
          } else {
            if (input$record_rate != "") {
              DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET record_rate = '", input$record_rate, "' WHERE timeseries_id = ", selected_timeseries$timeseries_id))
            } else {
              DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET record_rate = NULL WHERE timeseries_id = ", selected_timeseries$timeseries_id))
            }
          }
          
          if (!is.na(selected_timeseries$sensor_priority)) {
            if (input$sensor_priority != selected_timeseries$sensor_priority) {
              DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET sensor_priority = ", input$sensor_priority, " WHERE timeseries_id = ", selected_timeseries$timeseries_id))
            }
          } else {
            DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET sensor_priority = ", input$sensor_priority, " WHERE timeseries_id = ", selected_timeseries$timeseries_id))
          }
          
          if (!is.na(selected_timeseries$default_owner)) {
            if (input$default_owner != selected_timeseries$default_owner) {
              DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET default_owner = '", input$default_owner, "' WHERE timeseries_id = ", selected_timeseries$timeseries_id))
            }
          } else {
            DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET default_owner = '", input$default_owner, "' WHERE timeseries_id = ", selected_timeseries$timeseries_id))
          }
          
          if (!is.na(selected_timeseries$source_fx)) {
            if (input$source_fx != selected_timeseries$source_fx) {
              DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET source_fx = '", input$source_fx, "' WHERE timeseries_id = ", selected_timeseries$timeseries_id))
            }
          } else {
            DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET source_fx = '", input$source_fx, "' WHERE timeseries_id = ", selected_timeseries$timeseries_id))
          }
          
          
          if (!is.na(selected_timeseries$source_fx_args)) {
            if (nchar(input$source_fx_args == 0)) {
              DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET source_fx_args = NULL WHERE timeseries_id = ", selected_timeseries$timeseries_id))
              return()
            }
            
            # source_fx_args are fetched from DB as json, so we need to handle them accordingly for comparison
            # The following gives us a data.frame with column names as the keys and values as the values
            parsed_json <- jsonlite::fromJSON(selected_timeseries$source_fx_args, simplifyVector = TRUE, flatten = TRUE)
            
            # Now work the input$source_fx_args into a data.frame with the same shape as parsed_json
            # split into “arg:value” pairs, trim whitespace
            arg_pairs <- strsplit(input$source_fx_args, ",")[[1]]       # e.g. c("arg1: value1", "arg2: value2")
            arg_pairs <- trimws(arg_pairs)                              # remove leading/trailing spaces
            # split each on “:”, extract keys & values
            kv <- strsplit(arg_pairs, ":")
            keys <- sapply(kv, `[`, 1)
            vals <- sapply(kv, `[`, 2)
            # build a named list and then a one-row data.frame
            input_df <- setNames(as.list(vals), keys)
            input_df   <- as.data.frame(input_df, stringsAsFactors = FALSE)
            # now `parsed_json` and `input_df` have the same shape
            
            if (!identical(parsed_json, input_df)) {
              # Make the source_fx_args a json object
              args <- input$source_fx_args
              # split into "argument1: value1" etc.
              args <- strsplit(args, ",\\s*")[[1]]
              # split each pair on ":" and trim whitespace
              args <- strsplit(args, ":\\s*")
              # build a named list: names = keys, values = values
              args <- stats::setNames(
                lapply(args, function(x) x[2]),
                sapply(args, function(x) x[1])
              )
              # convert to JSON
              args <- jsonlite::toJSON(args, auto_unbox = TRUE)
              
              DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET source_fx_args = '", args, "' WHERE timeseries_id = ", selected_timeseries$timeseries_id))
            }
          } else {
            if (nchar(input$source_fx_args) == 0) {
              DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET source_fx_args = NULL WHERE timeseries_id = ", selected_timeseries$timeseries_id))
              return()
            }
            # Make the source_fx_args a json object
            args <- input$source_fx_args
            # split into "argument1: value1" etc.
            args <- strsplit(args, ",\\s*")[[1]]
            # split each pair on ":" and trim whitespace
            args <- strsplit(args, ":\\s*")
            # build a named list: names = keys, values = values
            args <- stats::setNames(
              lapply(args, function(x) x[2]),
              sapply(args, function(x) x[1])
            )
            # convert to JSON
            args <- jsonlite::toJSON(args, auto_unbox = TRUE)
            DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET source_fx_args = '", args, "' WHERE timeseries_id = ", selected_timeseries$timeseries_id))
          }
          
          if (!is.na(selected_timeseries$note)) {
            if (input$note != selected_timeseries$note) {
              DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET note = '", input$note, "' WHERE timeseries_id = ", selected_timeseries$timeseries_id))
            }
          } else {
            DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE timeseries SET note = '", input$note, "' WHERE timeseries_id = ", selected_timeseries$timeseries_id))
          }
          
          DBI::dbCommit(sesion$userData$AquaCache)
          showNotification("Timeseries updated successfully!", type = "message")
          getModuleData()
        }, error = function(e) {
          DBI::dbRollback(session$userData$AquaCache)
          showNotification(paste("Error updating timeseries:", e$message), type = "error")
        })
        
        return()
      }
      
      # If we got to here it's a new timeseries
      # start a transaction
      DBI::dbBegin(session$userData$AquaCache)
      tryCatch({
        AquaCache::addACTimeseries(start_datetime = "1800-01-01 00:00",
                                   location = input$location,
                                   sub_location = input$sub_location,
                                   z = if (input$z_specify) input$z else NA,
                                   parameter = input$parameter,
                                   media = input$media,
                                   sensor_priority = input$sensor_priority,
                                   aggregation_type = input$aggregation_type,
                                   record_rate = input$record_rate,
                                   share_with = input$share_with,
                                   owner = input$default_owner,
                                   source_fx = input$source_fx,
                                   source_fx_args = input$source_fx_args,
                                   note = input$note,
                                   con = session$userData$AquaCache
        )
        DBI::dbCommit(session$userData$AquaCache)
        showNotification("Timeseries added successfully! Historical data was fetched and daily means calculated if you provided a source_fx.", type = "message")
        getModuleData()
      }, error = function(e) {
        DBI::dbRollback(session$userData$AquaCAche)
      })
      
      # Reset all fields
      updateSelectizeInput(session, "location", selected = NULL)
      updateSelectizeInput(session, "sub_location", selected = NULL)
      updateCheckboxInput(session, "z_specify", value = FALSE)
      updateNumericInput(session, "z", value = NA)
      updateSelectizeInput(session, "parameter", selected = NULL)
      updateSelectizeInput(session, "media", selected = NULL)
      updateSelectizeInput(session, "aggregation_type", selected = NULL)
      updateTextInput(session, "record_rate", value = "")
      updateSelectizeInput(session, "sensor_priority", selected = 1)
      updateSelectizeInput(session, "default_owner", selected = NULL)
      updateSelectizeInput(session, "source_fx", selected = NULL)
      updateTextInput(session, "source_fx_args", value = "")
      updateTextAreaInput(session, "note", value = "")
      
      
    })
    
    # HEADS UP! find the modules which depend on timeseries. These will have cached data, which will need to be cleared when a new location or timeseries is added using the clear_cached function (R/app_cache.R)
    
  }) # End of moduleServer
}
