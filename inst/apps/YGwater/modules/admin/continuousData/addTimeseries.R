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
      moduleData$locations <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT l.location_id, l.location, l.name, lt.type, l.latitude, l.longitude FROM locations l INNER JOIN location_types lt ON l.location_type = lt.type_id")
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
        splitLayout(
          cellWidths = c("0%", "50%", "50%"),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
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
          )
        ),
        checkboxInput(ns("z_specify"), "Specify an elevation or depth?", value = FALSE),
        numericInput(ns("z"), "Elevation or depth, m (signed appropriately)", value = NA, min = -1000, max = 10000,
                     width = "100%"),
        
        splitLayout(
          cellWidths = c("0%", "50%", "50%"),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
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
          )
        ),
        splitLayout(
          cellWidths = c("0%", "50%", "50%"),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
          selectizeInput(ns("aggregation_type"), "Aggregation type",
                         choices = stats::setNames(moduleData$aggregation_types$aggregation_type_id, moduleData$aggregation_types$aggregation_type),
                         multiple = TRUE,
                         options = list(maxItems = 1,
                                        placeholder = 'Select aggregation type'),
                         width = "100%"
          ),
          textInput(ns("record_rate"), "Rough record rate, as  '5 minutes', '1 hour', '1 day', '1 week', '4 weeks', '1 month', etc.", value = "", width = "100%")
        ),
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
        splitLayout(
          cellWidths = c("50%", "50%"),
          verticalLayout(
            # htmlOutput to tell the user when they should use the source functions and what the arguments are
            tags$div(
              class = "alert alert-info",
              "The source function is used to download data using the AquaCache R package. Leave blank if entering data manually or using other methods. For more information refer to the AquaCache package documentation."
            ),
            selectizeInput(ns("source_fx"), "Source function (see AquaCache package documentation for details)",
                           choices = moduleData$source_fx,
                           multiple = TRUE,
                           options = list(maxItems = 1,
                                          placeholder = 'Select source function (optional)'),
                           width = "100%"
            )
          ),
          verticalLayout(
            # htmlOutput to tell the user how the source function arguments should be formatted
            tags$div(
              class = "alert alert-info",
              "Arguments must be formatted as key-value pairs for conversion to JSON, e.g. 'arg1: value1, arg2: value2'. Leave blank if not using a source_fx, otherwise refer to the function documentation in AquaCache."
            ),
            textInput(ns("source_fx_args"), "Source function arguments", 
                      value = "", 
                      placeholder = "arg1: value1, arg2: value2",
                      width = "100%")
          )
        ),
        textAreaInput(ns("note"), "Note (optional)", 
                      value = "", 
                      rows = 3, 
                      placeholder = "Any additional information about this timeseries (optional)",
                      width = "100%"),
        conditionalPanel(
          condition = "input.mode == 'add'",
          ns = ns,
          bslib::input_task_button(ns("add_timeseries"), label = "Add timeseries")
        ),
        conditionalPanel(
          condition = "input.mode == 'modify'",
          ns = ns,
          bslib::input_task_button(ns("modify_timeseries"), label = "Modify timeseries")
        )
      )
    })
    
    # Render the timeseries table for modification
    output$ts_table <- DT::renderDT({
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
    
    
    # Add a new timeseries #############
    # Create an extendedTask to add a new timeseries
    addNewTimeseries <- ExtendedTask$new(function(con, loc, sub_loc, z, z_specify, parameter, media, priority, agg_type, rate, owner, note, source_fx, source_fx_args, data) {
      promises::future_promise({
        # start a transaction
        DBI::dbBegin(con)
        tryCatch({
          if (is.null(sub_loc)) {
            sub_loc <- NA
          } else if (nchar(sub_loc) > 0) {
            sub_loc <- as.numeric(sub_loc)
          } else {
            sub_loc <- NA
          }
          
          # Get the location code from table locations
          loc_code <- data$locations[data$locations$location_id == loc, "location"]
          
          if (!is.null(source_fx_args)) {
            if (nchar(source_fx_args) > 0) {
              # Make the json object for source_fx_args
              # Make the source_fx_args a json object
              args <- source_fx_args
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
            } else {
              # if the source_fx_args is empty, we set it to NA
              args <- NA
            }
          } else {
            # if the source_fx_args is NULL, we set it to NA
            args <- NA
          }
          
          if (!is.null(source_fx)) {
            if (nchar(source_fx) > 0) {
              source_fx <- source_fx 
            } else {
              source_fx <- NA
            }
            if (source_fx == "downloadNWIS") { # NWIS data is only available from 2007 onwards, and errors if a date in the 1900s or earlier is specified.
              end_datetime <- "2000-01-01" 
            } else  {
              end_datetime <- "1800-01-01"
            }
          } else {
            source_fx <- NA
            end_datetime <- NA
          }
          
          # Make a new entry to the timeseries table
          df <- data.frame(location = loc_code,
                           location_id = as.numeric(loc),
                           sub_location_id = sub_loc,
                           z = if (z_specify) as.numeric(z) else NA,
                           parameter_id = as.numeric(parameter),
                           media_id = as.numeric(media),
                           sensor_priority = as.numeric(priority),
                           aggregation_type_id = as.numeric(agg_type),
                           record_rate = rate,
                           default_owner = as.numeric(owner),
                           source_fx = source_fx,
                           source_fx_args = args,
                           note = if (nchar(note) > 0) note else NA,
                           end_datetime = end_datetime)
          DBI::dbAppendTable(con, "timeseries", df)
          
          print("df out")
          df <<- df
          
          # Get the new timeseries_id
          new_timeseries_id <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location_id = ", df$location_id, 
                                                           " AND sub_location_id ", ifelse(is.na(df$sub_location_id), "IS NULL", paste0("= ", df$sub_location_id)), 
                                                           " AND z ", ifelse(is.na(df$z), "IS NULL", paste0("= ", df$z)), 
                                                           " AND parameter_id = ", df$parameter_id, 
                                                           " AND media_id = ", df$media_id, 
                                                           " AND aggregation_type_id = ", df$aggregation_type_id, 
                                                           " AND sensor_priority = ", df$sensor_priority, 
                                                           " AND record_rate = '", df$record_rate, "'",
                                                           " AND default_owner = ", df$default_owner,
                                                           " AND end_datetime = '", df$end_datetime, "'"))[1,1]
          
          # Fetch historical data if source_fx is provided
          if (!is.na(source_fx)) {
            AquaCache::getNewContinuous(con = con, timeseries_id = new_timeseries_id)
            new_start <- DBI::dbGetQuery(con, paste0("SELECT MIN(datetime) FROM measurements_continuous WHERE timeseries_id = ", new_timeseries_id, ";"))[1,1]
            DBI::dbExecute(con, paste0("UPDATE timeseries SET start_datetime = '", new_start, "' WHERE timeseries_id = ", new_timeseries_id, ";"))
            
            # Now conditionally check for HYDAT historical data
            if (source_fx == "downloadWSC" ) {
              param_name <- data$parameters[data$parameters$parameter_id == df$parameter_id, "param_name"]
              if (param_name %in% c("water level", "water flow")) {
                suppressMessages(AquaCache::update_hydat(timeseries_id = new_timeseries_id, force_update = TRUE, con = con))
              }
            }
          }
          
          # Now calculate stats
          if (lubridate::period(df$record_rate) <= lubridate::period("1 day")) {
            AquaCache::calculate_stats(timeseries_id = new_timeseries_id, con = con, start_recalc = NULL)
          }
          DBI::dbCommit(con)
          return("success")
        }, error = function(e) {
          DBI::dbRollback(con)
          return(paste("Error adding timeseries:", e$message))
        }, warning = function(w) {
          DBI::dbRollback(con)
          return(paste("Error adding timeseries:", w$message))
        })
      })
    } # end of ExtendedTask$new
    ) |> bslib::bind_task_button("add_timeseries")
    
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
      
      if (input$mode != "add") {
        # This is an error: show the user a notification to select 'add' mode
        shiny::showNotification("Please select 'Add new' mode to add a timeseries.", type = "error")
        return()
      }
      
      # Call the extendedTask to add a new timeseries
      addNewTimeseries$invoke(
        con = session$userData$AquaCache,
        loc = input$location,
        sub_loc = input$sub_location,
        z = input$z,
        z_specify = input$z_specify,
        parameter = input$parameter,
        media = input$media,
        priority = input$sensor_priority,
        agg_type = input$aggregation_type,
        rate = input$record_rate,
        owner = input$default_owner,
        note = input$note,
        source_fx = input$source_fx,
        source_fx_args = input$source_fx_args,
        data = moduleData
      )
    }, ignoreInit = TRUE)
    
    # Observe the result of the ExtendedTask
    observeEvent(addNewTimeseries$result(), {
      if (is.null(addNewTimeseries$result())) {
        return()  # No result yet, do nothing
      } else if (addNewTimeseries$result() != "success") {
        # If the result is not "success", show an error notification
        shiny::showNotification(addNewTimeseries$result(), type = "error")
        return()
      } else {
        # If the result is "success", show a success notification
        shiny::showNotification("Timeseries added successfully! Historical data was fetched and daily means calculated if you provided a source_fx.", type = "message")
        
        getModuleData()
        
        # Reset all fields
        updateSelectizeInput(session, "location", selected = character(0))
        updateSelectizeInput(session, "sub_location", selected = character(0))
        updateCheckboxInput(session, "z_specify", value = FALSE)
        updateNumericInput(session, "z", value = NA)
        updateSelectizeInput(session, "parameter", selected = character(0))
        updateSelectizeInput(session, "media", selected = character(0))
        updateSelectizeInput(session, "aggregation_type", selected = character(0))
        updateTextInput(session, "record_rate", value = "")
        updateSelectizeInput(session, "sensor_priority", selected = 1)
        updateSelectizeInput(session, "default_owner", selected = character(0))
        updateSelectizeInput(session, "share_with", selected = "public_reader")
        updateSelectizeInput(session, "source_fx", selected = character(0))
        updateTextInput(session, "source_fx_args", value = "")
        updateTextAreaInput(session, "note", value = "")
      }
    }, ignoreInit = TRUE)
    
    
    # Modify existing timeseries ###############
    observeEvent(input$modify_timeseries, {
      if (input$mode != "modify") {
        # This is an error: show the user a notification to select 'modify' mode
        shiny::showNotification("Please select 'Modify existing' mode to modify a timeseries.", type = "error")
        return()
      }
      # If we are modifying an existing timeseries, we need to check if it exists
      selected_row <- input$ts_table_rows_selected
      if (is.null(selected_row) || length(selected_row) != 1) {
        shiny::showNotification("Please select a single timeseries to modify.", type = "error")
        return()
      }
      tsid <- moduleData$timeseries_display[selected_row, "timeseries_id"]
      selected_timeseries <- moduleData$timeseries[moduleData$timeseries$timeseries_id == tsid, ]
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
          if (nchar(input$source_fx_args) == 0) {
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
        
        DBI::dbCommit(session$userData$AquaCache)
        showNotification("Timeseries updated successfully!", type = "message")
        getModuleData()
      }, error = function(e) {
        DBI::dbRollback(session$userData$AquaCache)
        showNotification(paste("Error updating timeseries:", e$message), type = "error")
      })
    }, ignoreInit = TRUE)
    
    # HEADS UP! find the modules which depend on timeseries. These will have cached data, which will need to be cleared when a new location or timeseries is added using the clear_cached function (R/app_cache.R)
    
  }) # End of moduleServer
}
