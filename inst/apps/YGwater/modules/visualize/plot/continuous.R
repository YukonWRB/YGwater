continuousPlotUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      selectizeInput(ns("type"), label = "Plot type", choices = c("Long timeseries", "Overlapping years"), selected = "Long timeseries"),
      selectizeInput(ns("param"), label = "Plotting parameter", choices = "placeholder"), #Choices are selected in the server
      selectizeInput(ns("loc_code"), "Select location by code", choices = "placeholder"), #Choices are selected in the server
      selectizeInput(ns("loc_name"), "Select location by name", choices = "placeholder"), #Choices are selected in the server
      
      # Now make a conditional panel depending on the selected plot type
      conditionalPanel(
        ns = ns,
        condition = "input.type == 'Overlapping years'",
        dateInput(ns("start_doy"), "Start day-of-year", value = paste0(lubridate::year(Sys.Date()), "-01-01")),
        dateInput(ns("end_doy"), "End day-of-year", value = paste0(lubridate::year(Sys.Date()), "-12-31")),
        selectizeInput(ns("years"), label = "Select years to plot", choices = "placeholder", multiple = TRUE), #Choices are selected in the server
        selectizeInput(ns("historic_range_overlap"), "Historic range includes all years of record or up to last year plotted?", choices = c("all", "last"), selected = "all"),
        selectizeInput(ns("return_periods"), "Plot return periods?", choices = stats::setNames(c("none", "auto", "calculate", "table"), c("none", "auto select", "calculate", "from table")), selected = "auto select"),
        selectizeInput(ns("return_type"), "Select return type", choices = c("Min", "Max"), selected = "Max"),
        numericInput(ns("return_yrs"), "Last year for return calculations", value = lubridate::year(Sys.Date()), 1900, 2100, 1),
        textInput(ns("return_months"), "Months for return calculation (comma delimited)", value = "5,6,7,8,9"),
      ),
      
      conditionalPanel(
        ns = ns,
        condition = "input.type == 'Long timeseries'",
        dateInput(ns("start_date"), "Start date", value = Sys.Date() - 365, max = Sys.Date() - 1),
        dateInput(ns("end_date"), "End date", value = Sys.Date(), max = Sys.Date()),
        uiOutput(ns("trace1_ui")), # Will be a button with the trace values. Upon click, user can edit or remove the trace.
        uiOutput(ns("trace2_ui")), # Will be a button with the trace values. Upon click, user can edit or remove the trace.
        uiOutput(ns("trace3_ui")),
        uiOutput(ns("trace4_ui")),
        actionButton(ns("add_trace"),
                     "Add trace"),
        checkboxInput(ns("log_y"), "Log scale y-axis?"),
        checkboxInput(ns("historic_range"), "Plot historic range?")
      ),
      checkboxInput(ns("apply_datum"), "Apply vertical datum?"),
      checkboxInput(ns("plot_filter"), "Filter extreme values?"),
      actionButton(ns("make_plot"),
                   "Create Plot")
    ),
    mainPanel(
      plotOutput(ns("plot_gg"), height = "800px"),
      plotly::plotlyOutput(ns("plot_plotly"), width = "100%", height = "800px", inline = TRUE),
      actionButton(ns("full_screen"),
                   "Full screen")
    )
  )
}

continuousPlotServer <- function(id, AquaCache, data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns # Used to create UI elements within server
    
    values <- reactiveValues()
    
    observe({
      # Find the parameter_ids for 'water level', 'snow water equivalent', 'snow depth'
      values$water_level <- data$parameters$parameter_id[data$parameters$param_name == "water level"]
      values$swe <- data$parameters$parameter_id[data$parameters$param_name == "snow water equivalent"]
      values$snow_depth <- data$parameters$parameter_id[data$parameters$param_name == "snow depth"]
      # Update the parameter choices
      updateSelectizeInput(session, "param", choices = stats::setNames(data$parameters$parameter_id, titleCase(data$parameters$param_name)), selected = values$water_level)
    })
    
    observeEvent(input$param, {
      # Update the location choices
      updateSelectizeInput(session, "loc_name", choices = unique(data$all_ts[data$all_ts$parameter_id == input$param, "name"]))
      updateSelectizeInput(session, "loc_code", choices = unique(data$all_ts[data$all_ts$parameter_id == input$param, "location"]))
      if (input$param %in% c(values$swe, values$snow_depth)) {
        updateDateInput(session, "start_doy", value = paste0(lubridate::year(Sys.Date()) - 1, "-09-01"))
        updateDateInput(session, "end_doy", value = paste0(lubridate::year(Sys.Date()), "-06-01"))
        updateTextInput(session, "return_months", value = "3,4,5")
      } else {
        updateDateInput(session, "start_doy", value = paste0(lubridate::year(Sys.Date()), "-01-01"))
        updateDateInput(session, "end_doy", value = paste0(lubridate::year(Sys.Date()), "-12-31"))
        updateTextInput(session, "return_months", value = "5,6,7,8,9")
      }
      if (input$param == values$water_level) {
        shinyjs::show("apply_datum")
      } else {
        shinyjs::hide("apply_datum")
        updateCheckboxInput(session, "apply_datum", value = FALSE)
      }
    })
    
    # Cross-updating of plot selection location name or code, and toggle apply_datum visible/invisible
    observeEvent(input$loc_code, {
      if (input$loc_code %in% data$all_ts$location) { #otherwise it runs without actually getting any information, which results in an error
        updateSelectizeInput(session, "loc_name", selected = unique(data$all_ts[data$all_ts$location == input$loc_code, "name"]))
        try({
          possible_years <- seq(
            as.numeric(substr(data$all_ts[data$all_ts$location == input$loc_code & data$all_ts$parameter_id == input$param, "start_datetime"], 1, 4)),
            as.numeric(substr(data$all_ts[data$all_ts$location == input$loc_code & data$all_ts$parameter_id == input$param, "end_datetime"], 1, 4))
            )
          updateSelectizeInput(session, "years", choices = possible_years)
        })

        data$possible_datums <- data$datums[data$datums$location == input$loc_code & data$datums$conversion_m != 0, ]
        if (nrow(data$possible_datums) < 1) {
          shinyjs::hide("apply_datum")
          updateCheckboxInput(session, "apply_datum", value = FALSE)
        } else {
          shinyjs::show("apply_datum")
        }
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$loc_name, {
      updateSelectizeInput(session, "loc_code", selected = unique(data$all_ts[data$all_ts$name == input$loc_name, "location"]))
    }, ignoreInit = TRUE)
    
    observeEvent(input$return_periods, {
      if (input$return_periods == "none") {
        shinyjs::hide("return_type")
        shinyjs::hide("return_months")
        shinyjs::hide("return_yrs")
      } else {
        shinyjs::show("return_type")
        shinyjs::show("return_months")
        shinyjs::show("return_yrs")
      }
    }, ignoreInit = TRUE)
  
    # Add/remove/modify trace buttons #######################################################################
    
    traces <- reactiveValues()
    traceCount <- reactiveVal(1)
    
    ## Add extra trace
    runTraceNew <- reactiveVal(FALSE)
    observeEvent(input$add_trace, {
      # When the button is clicked, a modal will appear with the necessary fields to add a trace. The trace values are then displayed to the user under button 'trace_x'
      
      if (runTraceNew() == FALSE) {
        showModal(modalDialog(
          selectizeInput(ns("traceNew_param"), "Select parameter", choices = stats::setNames(data$parameters$parameter_id, titleCase(data$parameters$param_name)), selected = as.numeric(input$param)),
          selectizeInput(ns("traceNew_loc_code"), "Select location by code", choices = unique(data$all_ts[data$all_ts$parameter_id == input$param, "location"])),
          selectizeInput(ns("traceNew_loc_name"), "Select location by name", choices = unique(data$all_ts[data$all_ts$parameter_id == input$param, "name"])),
          numericInput(ns("traceNew_lead_lag"), "Lead/lag in hours", value = 0),
          footer = tagList(
            actionButton(ns("add_new_trace"), "Add trace"),
            actionButton(ns("cancel"), "Cancel")
          ),
          easyClose = TRUE
        ))
        runTraceNew(TRUE)
      } else { # The modal has already run once, use the previously selected values
        showModal(modalDialog(
          selectizeInput(ns("traceNew_param"), "Select parameter", choices = stats::setNames(data$parameters$parameter_id, titleCase(data$parameters$param_name)), selected = as.numeric(input$traceNew_param)),
          selectizeInput(ns("traceNew_loc_code"), "Select location by code", choices = unique(data$all_ts[data$all_ts$parameter_id == input$traceNew_param, "location"])),
          selectizeInput(ns("traceNew_loc_name"), "Select location by name", choices = unique(data$all_ts[data$all_ts$parameter_id == input$traceNew_param, "name"])),
          numericInput(ns("traceNew_lead_lag"), "Lead/lag in hours", value = 0),
          footer = tagList(
            actionButton(ns("add_new_trace"), "Add trace"),
            actionButton(ns("cancel"), "Cancel")
          ),
          easyClose = TRUE
        ))
      }
    })
    observeEvent(input$cancel, {
      removeModal()
    })
    
    # Observe the param inputs for all traces and update the location choices in the modal
    observeEvent(input$traceNew_param, {
      updateSelectizeInput(session, "traceNew_loc_code", choices = unique(data$all_ts[data$all_ts$parameter_id == input$traceNew_param, "location"]))
      updateSelectizeInput(session, "traceNew_loc_name", choices = unique(data$all_ts[data$all_ts$parameter_id == input$traceNew_param, "name"]))
    }, ignoreInit = TRUE)
    
    observeEvent(input$traceNew_loc_code, {
      if (input$traceNew_loc_code %in% data$all_ts$location) { #otherwise it runs without actually getting any information, which results in an error
        updateSelectizeInput(session, "traceNew_loc_name", selected = unique(data$all_ts[data$all_ts$location == input$traceNew_loc_code, "name"]))
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$traceNew_loc_name, {
      updateSelectizeInput(session, "traceNew_loc_code", selected = unique(data$all_ts[data$all_ts$name == input$traceNew_loc_name, "location"]))
    }, ignoreInit = TRUE)
    
    observeEvent(input$add_new_trace, {
      if (traceCount() == 1) {
        traces$trace1 <- list(trace = "trace1",
                              parameter = as.numeric(input$param),
                              location = input$loc_code,
                              lead_lag = 0)
        traces$trace2 <- list(trace = "trace2",
                              parameter = as.numeric(input$traceNew_param),
                              location = input$traceNew_loc_code,
                              lead_lag = input$traceNew_lead_lag)
        button1Text <- HTML(paste0("<b>Trace 1</b><br>", titleCase(data$parameters[data$parameters$parameter_id == traces$trace1$parameter, "param_name"]), "<br>", unique(data$all_ts[data$all_ts$location == traces$trace1$location, "name"])))
        button2Text <- HTML(paste0("<b>Trace 2</b><br>", titleCase(data$parameters[data$parameters$parameter_id == traces$trace2$parameter, "param_name"]), "<br>", unique(data$all_ts[data$all_ts$location == traces$trace2$location, "name"]), "<br>Lead/lag ", traces$trace2$lead_lag, " hours"))
        output$trace1_ui <- renderUI({
          actionButton(ns("trace1"), button1Text)
        })
        shinyjs::show("trace1_ui")
        output$trace2_ui <- renderUI({
          actionButton(ns("trace2"), button2Text)
        })
        traceCount(2)
        shinyjs::hide("param")
        shinyjs::hide("loc_code")
        shinyjs::hide("loc_name")
        
      } else if (traceCount() == 2) {
        traces$trace3 <- list(trace = "trace3",
                              parameter = as.numeric(input$traceNew_param),
                              location = input$traceNew_loc_code,
                              lead_lag = input$traceNew_lead_lag)
        button3Text <- HTML(paste0("<b>Trace 3</b><br>", titleCase(data$parameters[data$parameters$parameter_id == traces$trace3$parameter, "param_name"]), "<br>", unique(data$all_ts[data$all_ts$location == traces$trace3$location, "name"]), "<br>Lead/lag ", traces$trace3$lead_lag, " hours"))
        output$trace3_ui <- renderUI({
          actionButton(ns("trace3"), button3Text)
        })
        traceCount(3)
        
      } else if (traceCount() == 3) {
        traces$trace4 <- list(trace = "trace4",
                              parameter = as.numeric(input$traceNew_param),
                              location = input$traceNew_loc_code,
                              lead_lag = input$traceNew_lead_lag)
        button4Text <- HTML(paste0("<b>Trace 4</b><br>", titleCase(data$parameters[data$parameters$parameter_id == traces$trace4$parameter, "param_name"]), "<br>", unique(data$all_ts[data$all_ts$location == traces$trace4$location, "name"]), "<br>Lead/lag ", traces$trace4$lead_lag, " hours"))
        output$trace4_ui <- renderUI({
          actionButton(ns("trace4"), button4Text)
        })
        
        traceCount(4)
        shinyjs::hide("add_trace")
      }
      
      removeModal()
    })
    
    # Observer for when user clicks a trace button. This should bring up a populated modal with the trace information, allowing user to edit the trace. As well, a new button to remove the trace should appear. Removal of a trace requires rejigging traceCount and elements of traces$trace_n
    clicked_trace <- reactiveVal(NULL)
    observeEvent(input$trace1, {
      showModal(modalDialog(
        selectizeInput(ns("traceNew_param"), "Select parameter", choices = stats::setNames(data$parameters$parameter_id, titleCase(data$parameters$param_name)), selected = traces$trace1$parameter),
        selectizeInput(ns("traceNew_loc_code"), "Select location by code", choices = unique(data$all_ts[data$all_ts$parameter_id == traces$trace1$parameter, "location"]), selected = traces$trace1$location),
        selectizeInput(ns("traceNew_loc_name"), "Select location by name", choices = unique(data$all_ts[data$all_ts$parameter_id == traces$trace1$parameter, "name"]), selected = unique(data$all_ts[data$all_ts$location == traces$trace1$location, "name"])),
        footer = tagList(
          actionButton(ns("modify_trace"), "Modify trace"),
          actionButton(ns("remove_trace"), "Remove trace"),
          actionButton(ns("cancel_modify"), "Cancel")
        ),
        easyClose = TRUE
      ))
      clicked_trace(traces$trace1$trace)
    })
    observeEvent(input$trace2, {
      showModal(modalDialog(
        selectizeInput(ns("traceNew_param"), "Select parameter", choices = stats::setNames(data$parameters$parameter_id, titleCase(data$parameters$param_name)), selected = traces$trace2$parameter),
        selectizeInput(ns("traceNew_loc_code"), "Select location by code", choices = unique(data$all_ts[data$all_ts$parameter_id == traces$trace2$parameter, "location"]), selected = traces$trace2$location),
        selectizeInput(ns("traceNew_loc_name"), "Select location by name", choices = unique(data$all_ts[data$all_ts$parameter_id == traces$trace2$parameter, "name"]), selected = unique(data$all_ts[data$all_ts$location == traces$trace2$location, "name"])),
        numericInput(ns("traceNew_lead_lag"), "Lead/lag in hours", value = traces$trace2$lead_lag),
        footer = tagList(
          actionButton(ns("modify_trace"), "Modify trace"),
          actionButton(ns("remove_trace"), "Remove trace"),
          actionButton(ns("cancel_modify"), "Cancel")
        ),
        easyClose = TRUE
      ))
      clicked_trace(traces$trace2$trace)
    })
    observeEvent(input$trace3, {
      showModal(modalDialog(
        selectizeInput(ns("traceNew_param"), "Select parameter", choices = stats::setNames(data$parameters$parameter_id, titleCase(data$parameters$param_name)), selected = traces$trace3$parameter),
        selectizeInput(ns("traceNew_loc_code"), "Select location by code", choices = unique(data$all_ts[data$all_ts$parameter_id == traces$trace3$parameter, "location"]), selected = traces$trace3$location),
        selectizeInput(ns("traceNew_loc_name"), "Select location by name", choices = unique(data$all_ts[data$all_ts$parameter_id == traces$trace3$parameter, "name"])), selected = unique(data$all_ts[data$all_ts$location == traces$trace3$location, "name"]),
        numericInput(ns("traceNew_lead_lag"), "Lead/lag in hours", value = traces$trace3$lead_lag),
        footer = tagList(
          actionButton(ns("modify_trace"), "Modify trace"),
          actionButton(ns("remove_trace"), "Remove trace"),
          actionButton(ns("cancel_modify"), "Cancel")
        ),
        easyClose = TRUE
      ))
      clicked_trace(traces$trace3$trace)
    })
    observeEvent(input$trace4, {
      showModal(modalDialog(
        selectizeInput(ns("traceNew_param"), "Select parameter", choices = stats::setNames(data$parameters$parameter_id, titleCase(data$parameters$param_name)), selected = traces$trace4$parameter),
        selectizeInput(ns("traceNew_loc_code"), "Select location by code", choices = unique(data$all_ts[data$all_ts$parameter_id == traces$trace4$parameter, "location"]), selected = traces$trace4$location),
        selectizeInput(ns("traceNew_loc_name"), "Select location by name", choices = unique(data$all_ts[data$all_ts$parameter_id == traces$trace4$parameter, "name"])), selected = unique(data$all_ts[data$all_ts$location == traces$trace4$location, "name"]),
        numericInput(ns("traceNew_lead_lag"), "Lead/lag in hours", value = traces$trace4$lead_lag),
        footer = tagList(
          actionButton(ns("modify_trace"), "Modify trace"),
          actionButton(ns("remove_trace"), "Remove trace"),
          actionButton(ns("cancel_modify"), "Cancel")
        ),
        easyClose = TRUE
      ))
      clicked_trace(traces$trace4$trace)
    })
    
    observeEvent(input$cancel_modify, {
      clicked_trace(NULL)
      removeModal()
    })
    
    
    ## modify/delete trace
    observeEvent(input$modify_trace, {
      # Update the trace values
      target_trace <- clicked_trace()
      traces[[target_trace]]$parameter <- as.numeric(input$traceNew_param)
      traces[[target_trace]]$location <- input$traceNew_loc_code
      traces[[target_trace]]$lead_lag <- input$traceNew_lead_lag
      
      # Update the trace button text
      if (target_trace == "trace1") {
        button_text <- HTML(paste0("<b>Trace ", target_trace, "</b><br>", titleCase(data$parameters[data$parameters$parameter_id == traces[[target_trace]]$parameter, "param_name"]), "<br>", unique(data$all_ts[data$all_ts$location == traces[[target_trace]]$location, "name"])))
      } else {
        button_text <- HTML(paste0("<b>Trace ", target_trace, "</b><br>", titleCase(data$parameters[data$parameters$parameter_id == traces[[target_trace]]$parameter, "param_name"]), "<br>", unique(data$all_ts[data$all_ts$location == traces[[target_trace]]$location, "name"]), "<br>Lead/lag ", traces[[target_trace]]$lead_lag, " hours"))
      }
      
      output[[paste0(target_trace, "_ui")]] <- renderUI({
        actionButton(ns(paste0(target_trace)), button_text)
      })
      removeModal()
    })
    
    new_traces <- reactiveValues() # This will enable a rename of reactiveValue names
    observeEvent(input$remove_trace, {
      # Remove the selected trace values
      target_trace <- clicked_trace()
      # Remove the trace from the reactiveValues
      traces[[target_trace]] <- NULL
      # Remove the trace button
      output[[paste0(target_trace, "_ui")]] <- NULL
      # Decrement the trace count
      traceCount(traceCount() - 1)
      # Re-jig the trace button text and the names of elements of traces
      increment <- 1
      for (i in names(traces)) {
        if (i != target_trace) {
          new_traces[[paste0("trace", increment)]] <- traces[[i]]
          increment <- increment + 1
        }
      }
      traces <- new_traces
      # Re-render text for all buttons
      for (i in 1:traceCount()) {
        if (i == 1) {
          button_text <- HTML(paste0("<b>Trace ", i, "</b><br>", titleCase(data$parameters[data$parameters$parameter_id == traces[[paste0("trace", i)]]$parameter, "param_name"]), "<br>", unique(data$all_ts[data$all_ts$location == traces[[paste0("trace", i)]]$location, "name"])))
        } else {
          button_text <- HTML(paste0("<b>Trace ", i, "</b><br>", titleCase(data$parameters[data$parameters$parameter_id == traces[[paste0("trace", i)]]$parameter, "param_name"]), "<br>", unique(data$all_ts[data$all_ts$location == traces[[paste0("trace", i)]]$location, "name"]), "<br>Lead/lag ", traces[[paste0("trace", i)]]$lead_lag, " hours"))
        }
        updateActionButton(session, paste0("trace", i), label = button_text)
      }
      
      if (traceCount() == 1) {
        # Remove the remaining trace button and show the param and location selectors
        shinyjs::hide("trace1_ui")
        shinyjs::show("param")
        shinyjs::show("loc_code")
        shinyjs::show("loc_name")
        updateSelectizeInput(session, "param", choices = stats::setNames(data$parameters$parameter_id, titleCase(data$parameters$param_name)), selected = traces$trace1$parameter)
        updateSelectizeInput(session, "loc_code", choices =  unique(data$all_ts[data$all_ts$parameter_id == input$param, "location"]), selected = traces$trace1$location)
        updateSelectizeInput(session, "loc_name", choices = unique(data$all_ts[data$all_ts$parameter_id == input$param, "name"]), selected = unique(data$all_ts[data$all_ts$location == traces$trace1$location, "name"]))
      } else {
        shinyjs::show("trace1_ui")
      }
      removeModal()
      
      traces$trace1$lead_lag <- 0
    })

    
    # Create and render the plot ############################################################
    observeEvent(input$make_plot, {
      shinyjs::hide("full_screen")

      tryCatch({
        
        filter <- if (input$plot_filter) 20 else NULL
      
        if (input$type == "Overlapping years") {
          shinyjs::hide("plot_plotly")
          
          return_months <- as.numeric(unlist(strsplit(input$return_months, ",")))
          
          plot <- plotOverlap(location = input$loc_code,
                              parameter = as.numeric(input$param),
                              startDay = input$start_doy,
                              endDay = input$end_doy,
                              years = input$years,
                              historic_range = input$historic_range_overlap,
                              datum = input$apply_datum,
                              filter = filter,
                              returns = input$return_periods,
                              return_type = input$return_type,
                              return_months = return_months,
                              return_max_year = input$return_yrs,
                              plot_scale = 1.4,
                              con = AquaCache)

          output$plot_gg <- renderPlot(plot)
          shinyjs::show("plot_gg")

        } else if (input$type == "Long timeseries") {
          shinyjs::hide("plot_gg")

          # Check if multiple traces are selected

          if (traceCount() == 1) {
            
            plot <- plotTimeseries(location = input$loc_code,
                                   parameter = as.numeric(input$param),
                                   start_date = input$start_date,
                                   end_date = input$end_date,
                                   historic_range = input$historic_range,
                                   datum = input$apply_datum,
                                   filter = filter,
                                   con = AquaCache)
          } else {
            locs <- c(traces$trace1$location, traces$trace2$location, traces$trace3$location, traces$trace4$location)
            params <- c(traces$trace1$parameter, traces$trace2$parameter, traces$trace3$parameter, traces$trace4$parameter)
            lead_lags <- c(traces$trace1$lead_lag, traces$trace2$lead_lag, traces$trace3$lead_lag, traces$trace4$lead_lag)
            plot <- plotMultiTimeseries(locations = locs,
                                        parameters = params,
                                        lead_lag = lead_lags,
                                        start_date = input$start_date,
                                        end_date = input$end_date,
                                        historic_range = input$historic_range,
                                        datum = input$apply_datum,
                                        filter = filter,
                                        con = AquaCache)
          }
          

          output$plot_plotly <- plotly::renderPlotly(plot)
          shinyjs::show("plot_plotly")
        }


        # Create a full screen button
        output$full_screen_ui <- renderUI({
          actionButton(ns("full_screen"), "Full screen")
        })

        shinyjs::show("full_screen")
      }, error = function(e) {
        showModal(modalDialog(paste0("An error occurred while creating the plot. Please check your inputs and try again.\n  \n  Error: ", e$message), easyClose = TRUE))
        return()
      })
    }, ignoreInit = TRUE) # End of plot rendering loop


    observeEvent(input$full_screen, {
      if (input$type == "Overlapping years") {
        shinyjs::runjs(paste0("toggleFullScreen('", session$ns("plot_gg"), "');"))
      } else if (input$type == "Long timeseries"){
        shinyjs::runjs(paste0("toggleFullScreen('", session$ns("plot_plotly"), "');"))
      }
    }, ignoreInit = TRUE)

  }) # End of moduleServer
}

