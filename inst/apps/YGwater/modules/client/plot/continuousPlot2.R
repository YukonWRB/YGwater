continuousPlotUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$style(
      HTML(sprintf("
     /* Add colors to the accordion. Using ns() makes it specific to each accordion */
      #%s.accordion {
        /* body background */
        --bs-accordion-bg:          #FFFCF5;
        /* collapsed header */
        --bs-accordion-btn-bg:      #FBE5B2;
        /* expanded header */
        --bs-accordion-active-bg:   #FBE5B2;
      }
    ", ns("accordion1")))
    ),
    
    page_sidebar(
      sidebar = sidebar(
        title = NULL,
        width = 350,
        bg = config$sidebar_bg,
        open = list(mobile = "always-above"),
        uiOutput(ns("sidebar"))
      ),
      uiOutput(ns("main"))
    )
  )
}

continuousPlot <- function(id, language, windowDims) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns # Used to create UI elements within server
    
    # Initial setup and data loading ########################################################################
    # Get the data to populate drop-downs. Runs every time this module is loaded.
    # !important!!! shares a cache with the data module
    cached <- cont_data.plot_module_data(con = session$userData$AquaCache)
    
    moduleData <- reactiveValues(
      locs = cached$locs,
      sub_locs = cached$sub_locs,
      params = cached$params,
      media = cached$media,
      aggregation_types = cached$aggregation_types,
      parameter_relationships = cached$parameter_relationships,
      range = cached$range,
      timeseries = cached$timeseries,
      rates = cached$rates,
      z = cached$z,
      locations_projects = cached$locations_projects,
      projects = cached$projects,
      locations_networks = cached$locations_networks,
      networks = cached$networks,
      param_groups = cached$param_groups,
      param_sub_groups = cached$param_sub_groups
    )
    
    
    values <- reactiveValues()
    # Find the parameter_ids for 'water level', 'snow water equivalent', 'snow depth' - this is used to change default plot start/end dates and to show the datum checkbox
    values$water_level <- moduleData$params$parameter_id[moduleData$params$param_name == "water level"]
    values$swe <- moduleData$params$parameter_id[moduleData$params$param_name == "snow water equivalent"]
    values$snow_depth <- moduleData$params$parameter_id[moduleData$params$param_name == "snow depth"]
    
    # Create the UI for the sidebar and main panel #########################################################
    # Render the sidebar UI
    output$sidebar <- renderUI({
      req(moduleData, language$language, language$abbrev)
      tagList(
        selectizeInput(ns("type"), 
                       label = tooltip(
                         trigger = list(
                           "Plot type",
                           bs_icon("info-circle-fill")
                         ),
                         "'Long timeseries' plots help visualize a change with time and allows for multiple traces or subplots, while 'Overlapping years' plots are useful for comparing data across years at one location."
                       ),
                       choices = c("Long timeseries", "Overlapping years"), 
                       selected = "Long timeseries"),
        selectizeInput(ns("param"), label = "Plotting parameter", stats::setNames(moduleData$params$parameter_id, titleCase(moduleData$params$param_name)), selected = values$water_level),
        selectizeInput(ns("loc_name"), "Select location", choices = NULL), # Choices are populated based on the parameter
        
        # Now make a conditional panel depending on the selected plot type
        conditionalPanel(
          ns = ns,
          condition = "input.type == 'Overlapping years'",
          
          div(
            dateInput(ns("start_doy"), "Start day-of-year", value = paste0(lubridate::year(Sys.Date()), "-01-01")),
            style = "display: flex; align-items: center;",
            span(
              id = ns("log_info_start_doy"),
              `data-bs-toggle` = "tooltip",
              `data-placement` = "right",
              `data-trigger` = "click hover",
              title = "The year is ignored; only the day-of-year is used.",
              icon("info-circle", style = "font-size: 100%; margin-left: 5px;")
            )
          ),
          div(
            dateInput(ns("end_doy"), "End day-of-year", value = paste0(lubridate::year(Sys.Date()), "-01-01")),
            style = "display: flex; align-items: center;",
            span(
              id = ns("log_info_end_doy"),
              `data-bs-toggle` = "tooltip",
              `data-placement` = "right",
              `data-trigger` = "click hover",
              title = "The year is ignored; only the day-of-year is used.",
              icon("info-circle", style = "font-size: 100%; margin-left: 5px;")
            )
          ),
          div(
            selectizeInput(ns("years"), label = "Select years to plot", choices = NULL, multiple = TRUE), #Choices are populated based on the location and parameter
            style = "display: flex; align-items: center;",
            span(
              id = ns("log_info_years"),
              `data-bs-toggle` = "tooltip",
              `data-placement` = "right",
              `data-trigger` = "click hover",
              title = "For periods overlaping the new year select the December year.",
              icon("info-circle", style = "font-size: 100%; margin-left: 5px;")
            )
          ),
          div(
            selectizeInput(ns("historic_range_overlap"),"Historic range includes all years of record or up to last year plotted?", 
                           choices = c("all", "last"), selected = "all"),
            style = "display: flex; align-items: center;",
            span(
              id = ns("log_info_hist_range"),
              `data-bs-toggle` = "tooltip",
              `data-placement` = "right",
              `data-trigger` = "click hover",
              title = "Historic ranges are plotted as a gray ribbon.",
              icon("info-circle", style = "font-size: 100%; margin-left: 5px;")
            )
          )
          # lines below were used with the ggplot overlaping years plot, these params have yet to be included in the plotly plot
          # selectizeInput(ns("return_periods"), "Plot return periods?", choices = stats::setNames(c("none", "auto", "calculate", "table"), c("none", "auto select", "calculate", "from table")), selected = "auto select"),
          # selectizeInput(ns("return_type"), "Select return type", choices = c("Min", "Max"), selected = "Max"),
          # numericInput(ns("return_yrs"), "Last year for return calculations", value = lubridate::year(Sys.Date()), 1900, 2100, 1),
          # textInput(ns("return_months"), "Months for return calculation (comma delimited)", value = "5,6,7,8,9"),
        ),
        
        conditionalPanel(
          ns = ns,
          condition = "input.type == 'Long timeseries'",
          dateInput(ns("start_date"), "Start date", value = Sys.Date() - 365, max = Sys.Date() - 1),
          dateInput(ns("end_date"), "End date", value = Sys.Date(), max = Sys.Date()),
          uiOutput(ns("trace1_ui")), # Will be a button with the trace values. Upon click, user can edit or remove the trace.
          uiOutput(ns("trace2_ui")),
          uiOutput(ns("trace3_ui")),
          uiOutput(ns("trace4_ui")),
          uiOutput(ns("subplot1_ui")), # Will be a button with the subplot values. Upon click, user can edit or remove the subplot.
          uiOutput(ns("subplot2_ui")),
          uiOutput(ns("subplot3_ui")),
          uiOutput(ns("subplot4_ui")),
          div(
            style = "display: flex; justify-content: flex-start; margin-bottom: 10px", # Use flexbox to align buttons side by side
            actionButton(ns("add_trace"),
                         "Add trace",
                         style = "margin-right: 5px;"),
            actionButton(ns("add_subplot"),
                         "Add subplot",
                         style = "margin-right: 10px;"),
            tooltip(
              trigger = list(
                bsicons::bs_icon("info-circle-fill")
              ),
              "Click “Add trace” to overlay a new timeseries on the same plot, or “Add subplot” to create a separate panel.",
              placement = "right"
            )
          ),
          checkboxInput(ns("log_y"), "Log scale y-axis?"),
          uiOutput(ns("share_axes")),
          checkboxInput(ns("historic_range"), "Plot historic range?")
        ),
        accordion(
          id = ns("accordion1"),
          open = FALSE,
          accordion_panel(
            id = ns("accordion1"),
            title = "Extra options",
            icon = bsicons::bs_icon("gear"),
            checkboxInput(ns("apply_datum"), "Apply vertical datum?"),
            checkboxInput(ns("plot_filter"), "Filter extreme values?"),
            checkboxInput(ns("unusable"), "Show unusable data?"),
            checkboxInput(ns("grades"), "Show grades?"),
            checkboxInput(ns("approvals"), "Show approvals?"),
            checkboxInput(ns("qualifiers"), "Show qualifiers?"),
            actionButton(ns("extra_aes"),
                         "Modify plot aesthetics",
                         title = "Modify plot aesthetics such as language, line size, text size.",
                         style = "display: block; width: 100%; margin-bottom: 10px;"), # Ensure block display and full width
          )
        ),
        br(),
        input_task_button(ns("make_plot"), 
                          label = "Create Plot", 
                          style = "display: block; width: 100%;")
      ) # End tagList
    }) %>% # End renderUI
      bindEvent(language$language, moduleData) # Re-render the UI if the language or moduleData changes
    
    output$main <- renderUI({
      tagList(
        plotly::plotlyOutput(ns("plot"), width = "100%", height = "800px", inline = TRUE),
        uiOutput(ns("full_screen_ui"))
      ) # End tagList
    }) %>% # End renderUI
      bindEvent(language$language) # Re-render the UI if the language or moduleData changes
    
    # Show/hide the approvals, grades, qualifiers checkboxes based on the selected plot type
    observe({
      req(input$type, traceCount(), subplotCount())
      if (input$type == "Long timeseries" && traceCount() == 1 && subplotCount() == 1) {
        shinyjs::show("grades")
        shinyjs::show("approvals")
        shinyjs::show("qualifiers")
      } else if (input$type == "Long timeseries" && (traceCount() > 1 || subplotCount() > 1)) {
        shinyjs::hide("grades")
        shinyjs::hide("approvals")
        shinyjs::hide("qualifiers")
        shinyjs::hide("loc_name") # Could have been shown again by selecting overlapping years
        shinyjs::hide("param") # Could have been shown again by selecting overlapping years
      } else if (input$type == "Overlapping years") {
        shinyjs::hide("grades")
        shinyjs::hide("approvals")
        shinyjs::hide("qualifiers")
        shinyjs::show("loc_name") # Would be hidden if multiple subplots or traces are still selected
        shinyjs::show("param") # Would be hidden if multiple subplots or traces are still selected
      }
    })
    
    # Update the location choices based on the selected parameter ##########################################
    observeEvent(input$param, {
      # Update the location choices
      locs <- unique(moduleData$all_ts[moduleData$all_ts$parameter_id == input$param, c(tr("generic_name_col", language$language), "location_id")])
      names(locs) <- c("name", "location_id")
      updateSelectizeInput(session, 
                           "loc_name", 
                           choices = stats::setNames(locs$location_id, locs$name), 
                           selected = if (input$loc_name %in% locs$name) input$loc_name else character(0)) # already ordered by the query
      if (input$param %in% c(values$swe, values$snow_depth)) {
        updateDateInput(session, "start_doy", value = paste0(lubridate::year(Sys.Date()) - 1, "-09-01"))
        updateDateInput(session, "end_doy", value = paste0(lubridate::year(Sys.Date()), "-06-01"))
        # updateTextInput(session, "return_months", value = "3,4,5")
      } else {
        updateDateInput(session, "start_doy", value = paste0(lubridate::year(Sys.Date()), "-01-01"))
        updateDateInput(session, "end_doy", value = paste0(lubridate::year(Sys.Date()), "-12-31"))
        # updateTextInput(session, "return_months", value = "5,6,7,8,9")
      }
      if (input$param == values$water_level) {
        shinyjs::show("apply_datum")
      } else {
        shinyjs::hide("apply_datum")
        updateCheckboxInput(session, "apply_datum", value = FALSE)
      }
    }, ignoreNULL = TRUE)
    
    observeEvent(input$loc_name, {
      req(input$loc_name, input$param)
      # Update years, used for the overlapping years plot (runs regardless of plot type selected because the user could switch plot types)
      try({
        possible_years <- seq(
          as.numeric(substr(moduleData$all_ts[moduleData$all_ts$location_id == input$loc_name & moduleData$all_ts$parameter_id == input$param, "start_datetime"], 1, 4)),
          as.numeric(substr(moduleData$all_ts[moduleData$all_ts$location_id == input$loc_name & moduleData$all_ts$parameter_id == input$param, "end_datetime"], 1, 4))
        )
        updateSelectizeInput(session, "years", choices = possible_years)
      })
      moduleData$possible_datums <- moduleData$datums[moduleData$datums$location_id == input$loc_name & moduleData$datums$conversion_m != 0, ]
      if (nrow(moduleData$possible_datums) < 1) {
        shinyjs::hide("apply_datum")
        updateCheckboxInput(session, "apply_datum", value = FALSE)
      } else {
        shinyjs::show("apply_datum")
      }
      
    }, ignoreInit = TRUE)
    
    # Modal dialog for extra aesthetics ########################################################################
    # Create a list with default aesthetic values
    plot_aes <- reactiveValues(lang = "en",
                               showgridx = FALSE,
                               showgridy = FALSE,
                               line_scale = 1,
                               axis_scale = 1,
                               legend_scale = 1)
    
    observeEvent(input$extra_aes, {
      showModal(modalDialog(
        title = "Modify plot aesthetics",
        tags$div(
          tags$h5("Language"),
          radioButtons(ns("lang"),
                       NULL,
                       choices = stats::setNames(c("en", "fr"), c("English", "French")),
                       selected = plot_aes$lang),
          checkboxInput(ns("showgridx"),
                        "Show x-axis gridlines",
                        value = plot_aes$showgridx),
          checkboxInput(ns("showgridy"),
                        "Show y-axis gridlines",
                        value = plot_aes$showgridy),
          sliderInput(ns("line_scale"),
                      "Line scale factor",
                      min = 0.2,
                      max = 3,
                      value = plot_aes$line_scale,
                      step = 0.1),
          sliderInput(ns("axis_scale"),
                      "Axes scale factor (text and values)",
                      min = 0.2,
                      max = 3,
                      value = plot_aes$axis_scale,
                      step = 0.1),
          sliderInput(ns("legend_scale"),
                      "Legend text scale factor",
                      min = 0.2,
                      max = 3,
                      value = plot_aes$legend_scale,
                      step = 0.1)
        ),
        easyClose = FALSE,
        footer = tagList(
          actionButton(ns("aes_apply"), "Apply"),
          actionButton(ns("aes_cancel"), "Cancel")
        )
      ))
    })
    
    observeEvent(input$aes_apply, {
      plot_aes$lang <- input$lang
      plot_aes$showgridx <- input$showgridx
      plot_aes$showgridy <- input$showgridy
      plot_aes$line_scale <- input$line_scale
      plot_aes$axis_scale <- input$axis_scale
      plot_aes$legend_scale <- input$legend_scale
      removeModal()
    })
    
    observeEvent(input$aes_cancel, {
      removeModal()
    })
    
    
    # Add/remove/modify trace buttons #######################################################################
    traces <- reactiveValues()
    traceCount <- reactiveVal(1)
    
    ## Add extra trace
    runTraceNew <- reactiveVal(FALSE) # Used to determine if the modal has run before so that previously selected values can be used
    observeEvent(input$add_trace, {
      # When the button is clicked, a modal will appear with the necessary fields to add a trace. The trace values are then displayed to the user under button 'trace_x'
      # Make sure that there is an input$param, input$loc_name before running the modal; give the user an informative modal if not
      if (nchar(input$loc_name) == 0 | nchar(input$param) == 0) {
        showModal(modalDialog(
          "Please select a location and parameter for the first trace before adding another.",
          footer = tagList(
            actionButton(ns("cancel"), "Cancel")
          ),
          easyClose = TRUE
        ))
        return()
      }
      
      if (runTraceNew() == FALSE) {
        showModal(modalDialog(
          selectizeInput(ns("traceNew_param"), "Select parameter", 
                         choices = stats::setNames(moduleData$params$parameter_id, titleCase(moduleData$params$param_name)), 
                         selected = input$param),
          selectizeInput(ns("traceNew_loc_id"), "Select location", 
                         choices = "placeholder"), # Choices are populated based on the parameter
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
          selectizeInput(ns("traceNew_param"), "Select parameter", 
                         choices = stats::setNames(moduleData$params$parameter_id, titleCase(moduleData$params$param_name)), 
                         selected = input$traceNew_param),
          selectizeInput(ns("traceNew_loc_id"), "Select location", 
                         choices = stats::setNames(moduleData$all_ts[moduleData$all_ts$parameter_id == input$traceNew_param, "location_id"], 
                                                   moduleData$all_ts[moduleData$all_ts$parameter_id == input$traceNew_param, tr("generic_name_col", language$language)]),
                         selected = input$traceNew_loc_id),
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
      # Update the location choices
      locs <- unique(moduleData$all_ts[moduleData$all_ts$parameter_id == input$traceNew_param, c(tr("generic_name_col", language$language), "location_id")])
      names(locs) <- c("name", "location_id")
      
      updateSelectizeInput(session, 
                           "traceNew_loc_id", 
                           choices = stats::setNames(locs$location_id, locs$name),
                           selected = if (input$traceNew_loc_id %in% locs$location_id) input$traceNew_loc_id else character(0))
    }, ignoreInit = TRUE)
    
    observeEvent(input$add_new_trace, {
      shinyjs::hide("add_subplot")
      if (traceCount() == 1) {
        traces$trace1 <- list(trace = "trace1",
                              parameter = as.numeric(input$param),
                              location_id = input$loc_name,
                              lead_lag = 0)
        traces$trace2 <- list(trace = "trace2",
                              parameter = as.numeric(input$traceNew_param),
                              location_id = input$traceNew_loc_id,
                              lead_lag = input$traceNew_lead_lag)
        button1Text <- HTML(paste0("<b>Trace 1</b><br>", titleCase(moduleData$params[moduleData$params$parameter_id == traces$trace1$parameter, "param_name"]), "<br>", unique(moduleData$all_ts[moduleData$all_ts$location_id == traces$trace1$location_id, tr("generic_name_col", language$language)])))
        button2Text <- HTML(paste0("<b>Trace 2</b><br>", titleCase(moduleData$params[moduleData$params$parameter_id == traces$trace2$parameter, "param_name"]), "<br>", unique(moduleData$all_ts[moduleData$all_ts$location_id == traces$trace2$location_id, tr("generic_name_col", language$language)]), "<br>Lead/lag ", traces$trace2$lead_lag, " hours"))
        output$trace1_ui <- renderUI({
          actionButton(ns("trace1"), button1Text)
        })
        shinyjs::show("trace1_ui")
        output$trace2_ui <- renderUI({
          actionButton(ns("trace2"), button2Text)
        })
        traceCount(2)
        shinyjs::hide("param")
        shinyjs::hide("loc_name")
        
      } else if (traceCount() == 2) {
        traces$trace3 <- list(trace = "trace3",
                              parameter = as.numeric(input$traceNew_param),
                              location_id = input$traceNew_loc_id,
                              lead_lag = input$traceNew_lead_lag)
        button3Text <- HTML(paste0("<b>Trace 3</b><br>", titleCase(moduleData$params[moduleData$params$parameter_id == traces$trace3$parameter, "param_name"]), "<br>", unique(moduleData$all_ts[moduleData$all_ts$location_id == traces$trace3$location_id, tr("generic_name_col", language$language)]), "<br>Lead/lag ", traces$trace3$lead_lag, " hours"))
        output$trace3_ui <- renderUI({
          actionButton(ns("trace3"), button3Text)
        })
        traceCount(3)
        
      } else if (traceCount() == 3) {
        traces$trace4 <- list(trace = "trace4",
                              parameter = as.numeric(input$traceNew_param),
                              location_id = input$traceNew_loc_id,
                              lead_lag = input$traceNew_lead_lag)
        button4Text <- HTML(paste0("<b>Trace 4</b><br>", titleCase(moduleData$params[moduleData$params$parameter_id == traces$trace4$parameter, "param_name"]), "<br>", unique(moduleData$all_ts[moduleData$all_ts$location_id == traces$trace4$location_id, tr("generic_name_col", language$language)]), "<br>Lead/lag ", traces$trace4$lead_lag, " hours"))
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
        selectizeInput(ns("traceNew_param"), "Select parameter", 
                       choices = stats::setNames(moduleData$params$parameter_id, titleCase(moduleData$params$param_name)), 
                       selected = traces$trace1$parameter),
        selectizeInput(ns("traceNew_loc_id"), "Select location", 
                       choices = stats::setNames(
                         moduleData$all_ts[moduleData$all_ts$parameter_id == traces$trace1$parameter, "location_id"], 
                         moduleData$all_ts[moduleData$all_ts$parameter_id == traces$trace1$parameter, tr("generic_name_col", language$language)]),
                       selected = traces$trace1$location_id),
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
        selectizeInput(ns("traceNew_param"), "Select parameter", 
                       choices = stats::setNames(moduleData$params$parameter_id, titleCase(moduleData$params$param_name)), 
                       selected = traces$trace2$parameter),
        selectizeInput(ns("traceNew_loc_id"), "Select location", 
                       choices = stats::setNames(
                         moduleData$all_ts[moduleData$all_ts$parameter_id == traces$trace2$parameter, "location_id"], 
                         moduleData$all_ts[moduleData$all_ts$parameter_id == traces$trace2$parameter, tr("generic_name_col", language$language)]),
                       selected = traces$trace2$location_id),
        numericInput(ns("traceNew_lead_lag"), "Lead/lag in hours", 
                     value = traces$trace2$lead_lag),
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
        selectizeInput(ns("traceNew_param"), "Select parameter", 
                       choices = stats::setNames(moduleData$params$parameter_id, titleCase(moduleData$params$param_name)), 
                       selected = traces$trace3$parameter),
        selectizeInput(ns("traceNew_loc_id"), "Select location", 
                       choices = stats::setNames(
                         moduleData$all_ts[moduleData$all_ts$parameter_id == traces$trace3$parameter, "location_id"], 
                         moduleData$all_ts[moduleData$all_ts$parameter_id == traces$trace3$parameter, tr("generic_name_col", language$language)]),
                       selected = traces$trace3$location_id),
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
        selectizeInput(ns("traceNew_param"), "Select parameter", 
                       choices = stats::setNames(moduleData$params$parameter_id, titleCase(moduleData$params$param_name)), 
                       selected = traces$trace4$parameter),
        selectizeInput(ns("traceNew_loc_id"), "Select location", 
                       choices = stats::setNames(
                         moduleData$all_ts[moduleData$all_ts$parameter_id == traces$trace4$parameter, "location_id"], 
                         moduleData$all_ts[moduleData$all_ts$parameter_id == traces$trace4$parameter, tr("generic_name_col", language$language)]),
                       selected = traces$trace4$location_id),
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
      traces[[target_trace]]$location_id <- input$traceNew_loc_id
      traces[[target_trace]]$lead_lag <- input$traceNew_lead_lag
      
      # Update the trace button text
      if (target_trace == "trace1") {
        button_text <- HTML(paste0("<b>Trace ", target_trace, "</b><br>", titleCase(moduleData$params[moduleData$params$parameter_id == traces[[target_trace]]$parameter, "param_name"]), "<br>", unique(moduleData$all_ts[moduleData$all_ts$location_id == traces[[target_trace]]$location_id, tr("generic_name_col", language$language)])))
      } else {
        button_text <- HTML(paste0("<b>Trace ", target_trace, "</b><br>", titleCase(moduleData$params[moduleData$params$parameter_id == traces[[target_trace]]$parameter, "param_name"]), "<br>", unique(moduleData$all_ts[moduleData$all_ts$location_id == traces[[target_trace]]$location_id, tr("generic_name_col", language$language)]), "<br>Lead/lag ", traces[[target_trace]]$lead_lag, " hours"))
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
      
      isolate({
        for (nm in names(new_traces)) {
          traces[[nm]] <- new_traces[[nm]]
        }
      })
      
      # Re-render text for all buttons
      for (i in 1:traceCount()) {
        if (i == 1) {
          button_text <- HTML(paste0("<b>Trace ", i, "</b><br>", titleCase(moduleData$params[moduleData$params$parameter_id == traces[[paste0("trace", i)]]$parameter, "param_name"]), "<br>", unique(moduleData$all_ts[moduleData$all_ts$location_id == traces[[paste0("trace", i)]]$location_id, tr("generic_name_col", language$language)])))
        } else {
          button_text <- HTML(paste0("<b>Trace ", i, "</b><br>", titleCase(moduleData$params[moduleData$params$parameter_id == traces[[paste0("trace", i)]]$parameter, "param_name"]), "<br>", unique(moduleData$all_ts[moduleData$all_ts$location_id == traces[[paste0("trace", i)]]$location_id, tr("generic_name_col", language$language)]), "<br>Lead/lag ", traces[[paste0("trace", i)]]$lead_lag, " hours"))
        }
        updateActionButton(session, paste0("trace", i), label = button_text)
      }
      
      if (traceCount() == 1) {
        # Remove the remaining trace button and show the param and location selectors
        shinyjs::hide("trace1_ui")
        shinyjs::show("param")
        shinyjs::show("loc_name")
        shinyjs::show("add_subplot")
        updateSelectizeInput(session, "param", 
                             choices = stats::setNames(moduleData$params$parameter_id, titleCase(moduleData$params$param_name)), 
                             selected = traces$trace1$parameter)
        
        # Update the location choices
        locs <- unique(moduleData$all_ts[moduleData$all_ts$parameter_id == input$param, c(tr("generic_name_col", language$language), "location_id")])
        names(locs) <- c("name", "location_id")
        updateSelectizeInput(session, "loc_name", 
                             choices = stats::setNames(locs$location_id, locs$name), 
                             selected = traces$trace1$location_id)
      } else {
        shinyjs::show("trace1_ui")
      }
      removeModal()
      
      traces$trace1$lead_lag <- 0
    })
    
    # Add/remove/modify subplot buttons #######################################################################
    subplots <- reactiveValues()
    subplotCount <- reactiveVal(1)
    
    ## Add extra subplot
    runSubplotNew <- reactiveVal(FALSE) # Used to determine if the modal has run before so that previously selected values can be used
    observeEvent(input$add_subplot, {
      # When the button is clicked, a modal will appear with the necessary fields to add a subplot. The subplot values are then displayed to the user under button 'subplot_x'
      
      # Make sure that there is an input$param, input$loc_name before running the modal; give the user an informative modal if not
      if (nchar(input$loc_name) == 0 | nchar(input$param) == 0) {
        showModal(modalDialog(
          "Please select a location and parameter for the first subplot before adding another.",
          footer = tagList(
            actionButton(ns("cancel"), "Cancel")
          ),
          easyClose = TRUE
        ))
        return()
      }
      
      if (runSubplotNew() == FALSE) {
        showModal(modalDialog(
          selectizeInput(ns("subplotNew_param"), "Select parameter", 
                         choices = stats::setNames(moduleData$params$parameter_id, titleCase(moduleData$params$param_name)), 
                         selected = input$param),
          selectizeInput(ns("subplotNew_loc_id"), "Select location", 
                         choices = "placeholder"), # Choices are populated based on the parameter in another observer
          footer = tagList(
            actionButton(ns("add_new_subplot"), "Add subplot"),
            actionButton(ns("cancel"), "Cancel")
          ),
          easyClose = TRUE
        ))
        runSubplotNew(TRUE)
      } else { # The modal has already run once, use the previously selected values
        showModal(modalDialog(
          selectizeInput(ns("subplotNew_param"), "Select parameter", 
                         choices = stats::setNames(moduleData$params$parameter_id, titleCase(moduleData$params$param_name)), 
                         selected = input$subplotNew_param),
          selectizeInput(ns("subplotNew_loc_id"), "Select location", 
                         choices = stats::setNames(moduleData$all_ts[moduleData$all_ts$parameter_id == input$subplotNew_param, "location_id"], 
                                                   moduleData$all_ts[moduleData$all_ts$parameter_id == input$subplotNew_param, tr("generic_name_col", language$language)]),
                         selected = input$subplotNew_loc_id),
          footer = tagList(
            actionButton(ns("add_new_subplot"), "Add subplot"),
            actionButton(ns("cancel"), "Cancel")
          ),
          easyClose = TRUE
        ))
      }
    })
    
    # No need for an observeEvent for the cancel button as it is handled by an observer above
    
    # Observe the param inputs for all traces and update the location choices in the modal
    observeEvent(input$subplotNew_param, {
      # Update the location choices
      locs <- unique(moduleData$all_ts[moduleData$all_ts$parameter_id == input$subplotNew_param, c(tr("generic_name_col", language$language), "location_id")])
      names(locs) <- c("name", "location_id")
      
      updateSelectizeInput(session,
                           "subplotNew_loc_id",
                           choices = stats::setNames(locs$location_id, locs$name),
                           selected = if (input$subplotNew_loc_id %in% moduleData$all_ts[moduleData$all_ts$parameter_id == input$subplotNew_param, "location_id"]) input$subplotNew_loc_id else character(0))
    }, ignoreInit = TRUE)
    
    share_axes_run <- reactiveVal(FALSE)
    observeEvent(input$add_new_subplot, {
      shinyjs::hide("add_trace")
      if (subplotCount() == 1) {
        if (!share_axes_run()) {
          output$share_axes <- renderUI({
            div(
              checkboxInput(ns("shareX"),
                            label = tooltip(
                              trigger = list(
                                "Share X axis between subplots (dates are aligned)",
                                bs_icon("info-circle-fill")
                              ),
                              "This will align the x-axis across all subplots, making it easier to compare trends over time."
                            ),
                            value = TRUE),
              checkboxInput(ns("shareY"),
                            label = tooltip(
                              trigger = list(
                                "Share Y axis between subplots (values are aligned)",
                                bs_icon("info-circle-fill")
                              ),
                              "This will align the y-axis across all subplots, making it easier to compare values across different locations or parameters."
                            ),
                            value = FALSE),
            )
          })
          share_axes_run(TRUE)
        } else {
          shinyjs::show("shareX")
          shinyjs::show("shareY")
        }
        
        subplots$subplot1 <- list(subplot = "subplot1",
                                  parameter = as.numeric(input$param),
                                  location_id = input$loc_name)
        subplots$subplot2 <- list(subplot = "subplot2",
                                  parameter = as.numeric(input$subplotNew_param),
                                  location_id = input$subplotNew_loc_id)
        button1Text <- HTML(paste0("<b>Subplot 1</b><br>", titleCase(moduleData$params[moduleData$params$parameter_id == subplots$subplot1$parameter, "param_name"]), "<br>", unique(moduleData$all_ts[moduleData$all_ts$location_id == subplots$subplot1$location_id, tr("generic_name_col", language$language)])))
        button2Text <- HTML(paste0("<b>Subplot 2</b><br>", titleCase(moduleData$params[moduleData$params$parameter_id == subplots$subplot2$parameter, "param_name"]), "<br>", unique(moduleData$all_ts[moduleData$all_ts$location_id == subplots$subplot2$location_id, tr("generic_name_col", language$language)])))
        output$subplot1_ui <- renderUI({
          actionButton(ns("subplot1"), button1Text)
        })
        shinyjs::show("subplot1_ui")
        output$subplot2_ui <- renderUI({
          actionButton(ns("subplot2"), button2Text)
        })
        subplotCount(2)
        shinyjs::hide("param")
        shinyjs::hide("loc_name")
        
      } else if (subplotCount() == 2) {
        subplots$subplot3 <- list(subplot = "subplot3",
                                  parameter = as.numeric(input$subplotNew_param),
                                  location_id = input$subplotNew_loc_id)
        button3Text <- HTML(paste0("<b>Subplot 3</b><br>", titleCase(moduleData$params[moduleData$params$parameter_id == subplots$subplot3$parameter, "param_name"]), "<br>", unique(moduleData$all_ts[moduleData$all_ts$location_id == subplots$subplot3$location_id, tr("generic_name_col", language$language)])))
        output$subplot3_ui <- renderUI({
          actionButton(ns("subplot3"), button3Text)
        })
        subplotCount(3)
        
      } else if (subplotCount() == 3) {
        subplots$subplot4 <- list(subplot = "subplot4",
                                  parameter = as.numeric(input$subplotNew_param),
                                  location_id = input$subplotNew_loc_id)
        button4Text <- HTML(paste0("<b>Subplot 4</b><br>", titleCase(moduleData$params[moduleData$params$parameter_id == subplots$subplot4$parameter, "param_name"]), "<br>", unique(moduleData$all_ts[moduleData$all_ts$location_id == subplots$subplot4$location_id, tr("generic_name_col", language$language)])))
        output$subplot4_ui <- renderUI({
          actionButton(ns("subplot4"), button4Text)
        })
        
        subplotCount(4)
        shinyjs::hide("add_subplot")
      }
      
      removeModal()
    })
    
    # Observer for when user clicks a subplot button. This should bring up a populated modal with the subplot information, allowing user to edit the subplot. As well, a new button to remove the subplot should appear. Removal of a subplot requires rejigging subplotCount and elements of subplots$subplot_n
    clicked_subplot <- reactiveVal(NULL)
    observeEvent(input$subplot1, {
      showModal(modalDialog(
        selectizeInput(ns("subplotNew_param"), "Select parameter", 
                       choices = stats::setNames(moduleData$params$parameter_id, titleCase(moduleData$params$param_name)), 
                       selected = subplots$subplot1$parameter),
        selectizeInput(ns("subplotNew_loc_id"), "Select location by name", 
                       choices = stats::setNames(moduleData$all_ts[moduleData$all_ts$parameter_id == subplots$subplot1$parameter, "location_id"], 
                                                 moduleData$all_ts[moduleData$all_ts$parameter_id == subplots$subplot1$parameter, tr("generic_name_col", language$language)]),
                       selected = subplots$subplot1$location_id),
        footer = tagList(
          actionButton(ns("modify_subplot"), "Modify subplot"),
          actionButton(ns("remove_subplot"), "Remove subplot"),
          actionButton(ns("cancel_modify"), "Cancel")
        ),
        easyClose = TRUE
      ))
      clicked_subplot(subplots$subplot1$subplot)
    })
    observeEvent(input$subplot2, {
      showModal(modalDialog(
        selectizeInput(ns("subplotNew_param"), "Select parameter", 
                       choices = stats::setNames(moduleData$params$parameter_id, titleCase(moduleData$params$param_name)), 
                       selected = subplots$subplot2$parameter),
        selectizeInput(ns("subplotNew_loc_id"), "Select location by name", 
                       choices = stats::setNames(moduleData$all_ts[moduleData$all_ts$parameter_id == subplots$subplot2$parameter, "location_id"], 
                                                 moduleData$all_ts[moduleData$all_ts$parameter_id == subplots$subplot2$parameter, tr("generic_name_col", language$language)]),
                       selected = subplots$subplot2$location_id),
        footer = tagList(
          actionButton(ns("modify_subplot"), "Modify subplot"),
          actionButton(ns("remove_subplot"), "Remove subplot"),
          actionButton(ns("cancel_modify"), "Cancel")
        ),
        easyClose = TRUE
      ))
      clicked_subplot(subplots$subplot2$subplot)
    })
    observeEvent(input$subplot3, {
      showModal(modalDialog(
        selectizeInput(ns("subplotNew_param"), "Select parameter", 
                       choices = stats::setNames(moduleData$params$parameter_id, titleCase(moduleData$params$param_name)), 
                       selected = subplots$subplot3$parameter),
        selectizeInput(ns("subplotNew_loc_id"), "Select location", 
                       choices = stats::setNames(moduleData$all_ts[moduleData$all_ts$parameter_id == subplots$subplot3$parameter, "location_id"], 
                                                 moduleData$all_ts[moduleData$all_ts$parameter_id == subplots$subplot3$parameter, tr("generic_name_col", language$language)]),
                       selected = subplots$subplot3$location_id),
        footer = tagList(
          actionButton(ns("modify_subplot"), "Modify subplot"),
          actionButton(ns("remove_subplot"), "Remove subplot"),
          actionButton(ns("cancel_modify"), "Cancel")
        ),
        easyClose = TRUE
      ))
      clicked_subplot(subplots$subplot3$subplot)
    })
    observeEvent(input$subplot4, {
      showModal(modalDialog(
        selectizeInput(ns("subplotNew_param"), "Select parameter", 
                       choices = stats::setNames(moduleData$params$parameter_id, titleCase(moduleData$params$param_name)), 
                       selected = subplots$subplot4$parameter),
        selectizeInput(ns("subplotNew_loc_id"), "Select location", 
                       choices = stats::setNames(moduleData$all_ts[moduleData$all_ts$parameter_id == subplots$subplot4$parameter, "location_id"], 
                                                 moduleData$all_ts[moduleData$all_ts$parameter_id == subplots$subplot4$parameter, tr("generic_name_col", language$language)]),
                       selected = subplots$subplot4$location_id),
        footer = tagList(
          actionButton(ns("modify_subplot"), "Modify subplot"),
          actionButton(ns("remove_subplot"), "Remove subplot"),
          actionButton(ns("cancel_modify"), "Cancel")
        ),
        easyClose = TRUE
      ))
      clicked_subplot(subplots$subplot4$subplot)
    })
    
    ## modify/delete subplot
    observeEvent(input$modify_subplot, {
      # Update the subplot values
      target_subplot <- clicked_subplot()
      subplots[[target_subplot]]$parameter <- as.numeric(input$subplotNew_param)
      subplots[[target_subplot]]$location_id <- input$subplotNew_loc_id
      
      # Update the subplot button text
      if (target_subplot == "subplot1") {
        button_text <- HTML(paste0("<b>Subplot ", target_subplot, "</b><br>", titleCase(moduleData$params[moduleData$params$parameter_id == subplots[[target_subplot]]$parameter, "param_name"]), "<br>", unique(moduleData$all_ts[moduleData$all_ts$location_id == subplots[[target_subplot]]$location_id, tr("generic_name_col", language$language)])))
      } else {
        button_text <- HTML(paste0("<b>Subplot ", target_subplot, "</b><br>", titleCase(moduleData$params[moduleData$params$parameter_id == subplots[[target_subplot]]$parameter, "param_name"]), "<br>", unique(moduleData$all_ts[moduleData$all_ts$location_id == subplots[[target_subplot]]$location_id, tr("generic_name_col", language$language)])))
      }
      
      output[[paste0(target_subplot, "_ui")]] <- renderUI({
        actionButton(ns(paste0(target_subplot)), button_text)
      })
      removeModal()
    })
    
    new_subplots <- reactiveValues() # This will enable a rename of reactiveValue names
    observeEvent(input$remove_subplot, {
      # Remove the selected subplot values
      target_subplot <- clicked_subplot()
      # Remove the subplot from the reactiveValues
      subplots[[target_subplot]] <- NULL
      # Remove the subplot button
      output[[paste0(target_subplot, "_ui")]] <- NULL
      # Decrement the subplot count
      subplotCount(subplotCount() - 1)
      # Re-jig the subplot button text and the names of elements of subplots
      increment <- 1
      for (i in names(subplots)) {
        if (i != target_subplot) {
          new_subplots[[paste0("subplot", increment)]] <- subplots[[i]]
          increment <- increment + 1
        }
      }
      
      isolate({
        for (nm in names(new_subplots)) {
          subplots[[nm]] <- new_subplots[[nm]]
        }
      })
      
      # Re-render text for all buttons
      for (i in 1:subplotCount()) {
        if (i == 1) {
          button_text <- HTML(paste0("<b>Subplot ", i, "</b><br>", titleCase(moduleData$params[moduleData$params$parameter_id == subplots[[paste0("subplot", i)]]$parameter, "param_name"]), "<br>", unique(moduleData$all_ts[moduleData$all_ts$location_id == subplots[[paste0("subplot", i)]]$location_id, tr("generic_name_col", language$language)])))
        } else {
          button_text <- HTML(paste0("<b>Subplot ", i, "</b><br>", titleCase(moduleData$params[moduleData$params$parameter_id == subplots[[paste0("subplot", i)]]$parameter, "param_name"]), "<br>", unique(moduleData$all_ts[moduleData$all_ts$location_id == subplots[[paste0("subplot", i)]]$location_id, tr("generic_name_col", language$language)])))
        }
        updateActionButton(session, paste0("subplot", i), label = button_text)
      }
      
      if (subplotCount() == 1) {
        # Remove the remaining subplot button and show the param and location selectors
        shinyjs::hide("subplot1_ui")
        shinyjs::hide("shareX")
        shinyjs::hide("shareY")
        shinyjs::show("param")
        shinyjs::show("loc_name")
        shinyjs::show("add_trace")
        updateSelectizeInput(session, "param", 
                             choices = stats::setNames(moduleData$params$parameter_id, titleCase(moduleData$params$param_name)), 
                             selected = subplots$subplot1$parameter)
        
        # Update the location choices
        locs <- unique(moduleData$all_ts[moduleData$all_ts$parameter_id == input$param, c(tr("generic_name_col", language$language), "location_id")])
        names(locs) <- c("name", "location_id")
        updateSelectizeInput(session, "loc_name", 
                             choices = stats::setNames(locs$location_id, locs$name), 
                             selected = subplots$subplot1$location_id)
      } else {
        shinyjs::show("subplot1_ui")
      }
      removeModal()
    })
    
    
    # Create ExtendedTasks to render plots ############################################################
    # Overlapping years plot
    plot_output_overlap <- ExtendedTask$new(function(loc, param, start_doy, end_doy, yrs, historic_range, apply_datum, filter, unusable, line_scale, axis_scale, legend_scale, legend_position, lang, gridx, gridy, config) {
      promises::future_promise({
        tryCatch({
          con <- AquaConnect(name = config$dbName,
                             host = config$dbHost,
                             port = config$dbPort,
                             username = config$dbUser,
                             password = config$dbPass,
                             silent = TRUE)
          on.exit(DBI::dbDisconnect(con))
          
          plot <- plotOverlap(location = loc,
                              sub_location = NULL,
                              record_rate = NULL,
                              parameter = param,
                              startDay = start_doy,
                              endDay = end_doy,
                              years = yrs,
                              historic_range = historic_range,
                              datum = apply_datum,
                              title = TRUE,
                              filter = filter,
                              unusable = unusable,
                              line_scale = line_scale,
                              axis_scale = axis_scale,
                              legend_scale = legend_scale,
                              legend_position = legend_position,
                              slider = FALSE,
                              lang = lang,
                              gridx = gridx,
                              gridy = gridy,
                              con = con,
                              data = TRUE,
                              rate = 'max')
          return(plot)
        }, error = function(e) {
          return(e$message)
        })
      })
    } # End of ExtendedTask function
    ) |> bind_task_button("make_plot")
    
    
    # Single timeseries plot
    plot_output_timeseries <- ExtendedTask$new(function(loc, param, start_date, end_date, historic_range, apply_datum, filter, unusable, grades, approvals, qualifiers, line_scale, axis_scale, legend_scale, legend_position, lang, gridx, gridy, config) {
      promises::future_promise({
        tryCatch({
          con <- AquaConnect(name = config$dbName, 
                             host = config$dbHost,
                             port = config$dbPort,
                             username = config$dbUser,
                             password = config$dbPass,
                             silent = TRUE)
          on.exit(DBI::dbDisconnect(con))
          
          plot <- plotTimeseries(location = loc,
                                 parameter = param,
                                 start_date = start_date,
                                 end_date = end_date,
                                 historic_range = historic_range,
                                 datum = apply_datum,
                                 filter = filter,
                                 unusable = unusable,
                                 grades = grades,
                                 approvals = approvals,
                                 qualifiers = qualifiers,
                                 lang = lang,
                                 line_scale = line_scale,
                                 axis_scale = axis_scale,
                                 legend_scale = legend_scale,
                                 legend_position = legend_position,
                                 slider = FALSE,
                                 gridx = gridx,
                                 gridy = gridy,
                                 con = con,
                                 data = TRUE)
          
          return(plot)
        }, error = function(e) {
          return(e$message)
        })
      })
    } # End of ExtendedTask function
    ) |> bind_task_button("make_plot")
    
    # Multiple traces plot
    plot_output_timeseries_traces <- ExtendedTask$new(function(locs, params, lead_lags, start_date, end_date, historic_range, apply_datum, filter, unusable, line_scale, axis_scale, legend_scale, legend_position, lang, gridx, gridy, shareX, shareY, config) {
      promises::future_promise({
        tryCatch({
          con <- AquaConnect(name = config$dbName, 
                             host = config$dbHost,
                             port = config$dbPort,
                             username = config$dbUser,
                             password = config$dbPass,
                             silent = TRUE)
          on.exit(DBI::dbDisconnect(con))
          
          plot <- plotMultiTimeseries(type = "traces",
                                      locations = locs,
                                      parameters = params,
                                      lead_lag = lead_lags,
                                      start_date = start_date,
                                      end_date = end_date,
                                      historic_range = historic_range,
                                      datum = apply_datum,
                                      filter = filter,
                                      unusable = unusable,
                                      lang = lang,
                                      line_scale = line_scale,
                                      axis_scale = axis_scale,
                                      legend_scale = legend_scale,
                                      legend_position = legend_position,
                                      gridx = gridx,
                                      gridy = gridy,
                                      shareX = shareX,
                                      shareY = shareY,
                                      con = con,
                                      data = TRUE)
          
          return(plot)
        }, error = function(e) {
          return(e$message)
        })
      })
    } # End of ExtendedTask function
    ) |> bind_task_button("make_plot")
    
    # Multiple subplots plot
    plot_output_timeseries_subplots <- ExtendedTask$new(function(locs, params, start_date, end_date, historic_range, apply_datum, filter, unusable, line_scale, axis_scale, legend_scale, legend_position, lang, gridx, gridy, shareX, shareY, config) {
      promises::future_promise({
        tryCatch({
          con <- AquaConnect(name = config$dbName, 
                             host = config$dbHost,
                             port = config$dbPort,
                             username = config$dbUser,
                             password = config$dbPass,
                             silent = TRUE)
          on.exit(DBI::dbDisconnect(con))
          
          plot <- plotMultiTimeseries(type = "subplots",
                                      locations = locs,
                                      parameters = params,
                                      start_date = start_date,
                                      end_date = end_date,
                                      historic_range = historic_range,
                                      datum = apply_datum,
                                      filter = filter,
                                      unusable = unusable,
                                      lang = lang,
                                      line_scale = line_scale,
                                      axis_scale = axis_scale,
                                      legend_scale = legend_scale,
                                      legend_position = legend_position,
                                      gridx = gridx,
                                      gridy = gridy,
                                      shareX = shareX,
                                      shareY = shareY,
                                      con = con,
                                      data = TRUE)
          
          return(plot)
        }, error = function(e) {
          return(e$message)
        })
      })
    } # End of ExtendedTask function
    ) |> bind_task_button("make_plot")
    
    
    # Create the plots and render ############################################################
    plot_created <- reactiveVal(FALSE) # Flag to determine if a plot has been created
    # Call up the ExtendedTask and render the plot
    observeEvent(input$make_plot, {
      if (plot_created()) {
        shinyjs::hide("full_screen_ui")
      }
      
      if (input$type == "Overlapping years") {
        if (nchar(input$loc_name) == 0) {
          showModal(modalDialog("Please select a location.", easyClose = TRUE))
          return()
        }
        if (nchar(input$param) == 0) {
          showModal(modalDialog("Please select a parameter", easyClose = TRUE))
          return()
        }
        plot_output_overlap$invoke(loc = as.numeric(input$loc_name), param = as.numeric(input$param), start_doy = input$start_doy, end_doy = input$end_doy, yrs = input$years, historic_range = input$historic_range_overlap, apply_datum = input$apply_datum, filter = if (input$plot_filter) 20 else NULL, unusable = input$unusable, line_scale = plot_aes$line_scale, axis_scale = plot_aes$axis_scale, legend_scale = plot_aes$legend_scale, legend_position = if (windowDims()$width > 1.3 * windowDims()$height) "v" else "h", lang = plot_aes$lang, gridx = plot_aes$showgridx, gridy = plot_aes$showgridy, config = session$userData$config)
      } else if (input$type == "Long timeseries") {
        if (traceCount() == 1) { # Either a single trace or more than 1 subplot
          if (subplotCount() > 1) { # Multiple sub plots
            locs <- c(subplots$subplot1$location_id, subplots$subplot2$location_id, subplots$subplot3$location_id, subplots$subplot4$location_id)
            params <- c(subplots$subplot1$parameter, subplots$subplot2$parameter, subplots$subplot3$parameter, subplots$subplot4$parameter)
            plot_output_timeseries_subplots$invoke(locs = as.numeric(locs), params = params, start_date = input$start_date, end_date = input$end_date, historic_range = input$historic_range, apply_datum = input$apply_datum, filter = if (input$plot_filter) 20 else NULL, unusable = input$unusable, line_scale = plot_aes$line_scale, axis_scale = plot_aes$axis_scale, legend_scale = plot_aes$legend_scale, legend_position = if (windowDims()$width > 1.3 * windowDims()$height) "v" else "h", lang = plot_aes$lang, gridx = plot_aes$showgridx, gridy = plot_aes$showgridy, shareX = input$shareX, shareY = input$shareY, config = session$userData$config)
          } else {  # Single trace
            if (nchar(input$loc_name) == 0) {
              showModal(modalDialog("Please select a location.", easyClose = TRUE))
              return()
            }
            if (nchar(input$param) == 0) {
              showModal(modalDialog("Please select a parameter", easyClose = TRUE))
              return()
            }
            plot_output_timeseries$invoke(loc = as.numeric(input$loc_name), param = as.numeric(input$param), start_date = input$start_date, end_date = input$end_date, historic_range = input$historic_range, apply_datum = input$apply_datum, filter = if (input$plot_filter) 20 else NULL, unusable = input$unusable, grades = input$grades, approvals = input$approvals, qualifiers = input$qualifiers, line_scale = plot_aes$line_scale, axis_scale = plot_aes$axis_scale, legend_scale = plot_aes$legend_scale, legend_position = if (windowDims()$width > 1.3 * windowDims()$height) "v" else "h", lang = plot_aes$lang, gridx = plot_aes$showgridx, gridy = plot_aes$showgridy, config = session$userData$config)
          }
        } else { # Multiple traces, single plot
          locs <- c(traces$trace1$location_id, traces$trace2$location_id, traces$trace3$location_id, traces$trace4$location_id)
          params <- c(traces$trace1$parameter, traces$trace2$parameter, traces$trace3$parameter, traces$trace4$parameter)
          lead_lags <- c(traces$trace1$lead_lag, traces$trace2$lead_lag, traces$trace3$lead_lag, traces$trace4$lead_lag)
          
          plot_output_timeseries_traces$invoke(locs = as.numeric(locs), params = params, lead_lags = lead_lags, start_date = input$start_date, end_date = input$end_date, historic_range = input$historic_range, apply_datum = input$apply_datum, filter = if (input$plot_filter) 20 else NULL, unusable = input$unusable, line_scale = plot_aes$line_scale, axis_scale = plot_aes$axis_scale, legend_scale = plot_aes$legend_scale, legend_position = if (windowDims()$width > 1.3 * windowDims()$height) "v" else "h", lang = plot_aes$lang, gridx = plot_aes$showgridx, gridy = plot_aes$showgridy, shareX = input$shareX, shareY = input$shareY, config = session$userData$config)
        }
      }
      
      # Create a full screen button if necessary
      if (!plot_created()) {
        output$full_screen_ui <- renderUI({
          # Side-by-side buttons
          page_fluid(
            div(class = "d-inline-block", actionButton(ns("full_screen"), "Full screen")),
            div(class = "d-inline-block", downloadButton(ns("download_data"), "Download data"))
          )
        })
      } else {
        shinyjs::show("full_screen_ui")
      }
      plot_created(TRUE)
    }, ignoreInit = TRUE)
    
    
    ## Observe the results of the ExtendedTasks and render the plot ##############
    observeEvent(plot_output_overlap$result(), {
      if (inherits(plot_output_overlap$result(), "character")) {
        showModal(modalDialog(
          title = "Error",
          plot_output_overlap$result(),
          easyClose = TRUE
        ))
        return()
      }
      
      output$plot <- plotly::renderPlotly({
        isolate(plot_output_overlap$result()$plot)
      })
    })
    
    observeEvent(plot_output_timeseries$result(), {
      if (inherits(plot_output_timeseries$result(), "character")) {
        showModal(modalDialog(
          title = "Error",
          plot_output_timeseries$result(),
          easyClose = TRUE
        ))
        return()
      }
      output$plot <- plotly::renderPlotly({
        isolate(plot_output_timeseries$result()$plot)
      })
    })
    
    observeEvent(plot_output_timeseries_traces$result(), {
      if (inherits(plot_output_timeseries_traces$result(), "character")) {
        showModal(modalDialog(
          title = "Error",
          plot_output_timeseries_traces$result(),
          easyClose = TRUE
        ))
        return()
      }
      output$plot <- plotly::renderPlotly({
        isolate(plot_output_timeseries_traces$result()$plot)
      })
    })
    
    observeEvent(plot_output_timeseries_subplots$result(), {
      if (inherits(plot_output_timeseries_subplots$result(), "character")) {
        showModal(modalDialog(
          title = "Error",
          plot_output_timeseries_subplots$result(),
          easyClose = TRUE
        ))
        return()
      }
      output$plot <- plotly::renderPlotly({
        isolate(plot_output_timeseries_subplots$result()$plot)
      })
    })
    
    # Observe changes to the windowDims reactive value and update the legend position using plotlyProxy
    # The js function takes care of debouncing the window resize event and also reacts to a change in orientation or full screen event
    observeEvent(windowDims(), {
      req(plot_created())
      if (is.null(windowDims())) return()
      if (windowDims()$width > 1.3 * windowDims()$height) {
        plotly::plotlyProxy("plot", session) %>%
          plotly::plotlyProxyInvoke("relayout", legend = list(orientation = "v"))
      } else {
        plotly::plotlyProxy("plot", session) %>%
          plotly::plotlyProxyInvoke("relayout", legend = list(orientation = "h"))
      }
    }, ignoreNULL = TRUE)
    
    # Observe the full screen button and run the javascript function to make the plot full screen
    observeEvent(input$full_screen, {
      shinyjs::runjs(paste0("toggleFullScreen('", session$ns("plot"), "');"))
      
      # Manually trigger a window resize event after some delay
      shinyjs::runjs("
                      setTimeout(function() {
                        sendWindowSizeToShiny();
                      }, 700);
                    ")
    }, ignoreInit = TRUE)
    
    # Send the user the plotting data
    output$download_data <- downloadHandler(
      filename = function() {
        time <- Sys.time()
        attr(time, "tzone") <- "UTC"
        paste0("continuous_plot_data_", gsub("-", "", gsub(" ", "_", gsub(":", "", substr(time, 0, 16)))), "_UTC.xlsx")
      },
      content = function(file) {
        if (input$type == "Overlapping years") {
          openxlsx::write.xlsx(plot_output_overlap$result()$data, file)
          
        } else if (input$type == "Long timeseries") {
          if (traceCount() == 1) { # Either a single trace or more than 1 subplot
            if (subplotCount() > 1) { # Multiple sub plots
              openxlsx::write.xlsx(plot_output_timeseries_subplots$result()$data, file)
            } else {  # Single trace
              openxlsx::write.xlsx(plot_output_timeseries$result()$data, file)
            }
          } else { # Multiple traces, single plot
            openxlsx::write.xlsx(plot_output_timeseries_traces$result()$data, file)
          }
        }
      } # End content
    ) # End downloadHandler
    
  }) # End of moduleServer
}
