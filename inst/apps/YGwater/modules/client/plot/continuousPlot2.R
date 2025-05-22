continuousPlotUI <- function(id) {
  ns <- NS(id)
  
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
}

continuousPlot <- function(id, language, windowDims) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns # Used to create UI elements within server
    
    # Initial setup and data loading ########################################################################
    moduleData <- reactiveValues(all_ts = DBI::dbGetQuery(session$userData$AquaCache, "SELECT ts.timeseries_id, ts.location_id, ts.parameter_id, ts.media_id, ts.start_datetime, ts.end_datetime, loc.name, loc.name_fr FROM timeseries AS ts INNER JOIN locations AS loc ON ts.location_id = loc.location_id AND ts.location = loc.location ORDER BY loc.name;"),
                                 parameters = DBI::dbGetQuery(session$userData$AquaCache, "SELECT DISTINCT parameters.parameter_id, parameters.param_name FROM timeseries INNER JOIN parameters ON timeseries.parameter_id = parameters.parameter_id ORDER BY parameters.param_name;"),
                                 datums = DBI::dbGetQuery(session$userData$AquaCache, "SELECT l.location_id, dc.conversion_m FROM datum_conversions dc INNER JOIN locations l ON dc.location_id = l.location_id INNER JOIN datum_list dl ON dc.datum_id_to = dl.datum_id;")
    )
    
    values <- reactiveValues()
    # Find the parameter_ids for 'water level', 'snow water equivalent', 'snow depth' - this is used to change default plot start/end dates and to show the datum checkbox
    values$water_level <- moduleData$parameters$parameter_id[moduleData$parameters$param_name == "water level"]
    values$swe <- moduleData$parameters$parameter_id[moduleData$parameters$param_name == "snow water equivalent"]
    values$snow_depth <- moduleData$parameters$parameter_id[moduleData$parameters$param_name == "snow depth"]
    
    # Create the UI for the sidebar and main panel #########################################################
    # Render the sidebar UI
    output$sidebar <- renderUI({
      req(moduleData, language$language, language$abbrev)
      tagList(
        selectizeInput(ns("type"), label = "Plot type", choices = c("Long timeseries", "Overlapping years"), selected = "Long timeseries"),
        selectizeInput(ns("param"), label = "Plotting parameter", stats::setNames(moduleData$parameters$parameter_id, titleCase(moduleData$parameters$param_name)), selected = values$water_level),
        actionButton()
        selectizeInput(ns("loc"), "Select location", choices = NULL), # Choices are populated based on the parameter
        selectizeInput(ns("sub_loc"), "Select sub-location", choices = NULL), # Choices are populated based on the parameter, and only if there are sub locations
        
        # Now make a conditional panel depending on the selected plot type
        conditionalPanel(
          ns = ns,
          condition = "input.type == 'Overlapping years'",
          
          div(
            dateInput(ns("start_doy"), "Start day-of-year", value = paste0(lubridate::year(Sys.Date()), "-01-01")),
            style = "display: flex; align-items: center;",
            span(
              id = ns("log_info"),
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
              id = ns("log_info"),
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
              id = ns("log_info"),
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
              id = ns("log_info"),
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
            style = "display: flex; justify-content: flex-start;", # Use flexbox to align buttons side by side
            actionButton(ns("add_trace"),
                         "Add trace",
                         style = "margin-right: 5px;"),
            actionButton(ns("add_subplot"),
                         "Add subplot")
          ),
          checkboxInput(ns("log_y"), "Log scale y-axis?"),
          uiOutput(ns("share_axes")),
          checkboxInput(ns("historic_range"), "Plot historic range?")
        ),
        checkboxInput(ns("apply_datum"), "Apply vertical datum?"),
        checkboxInput(ns("plot_filter"), "Filter extreme values?"),
        checkboxInput(ns("unusable"), "Show unusable data?"),
        checkboxInput(ns("grades"), "Show grades?"),
        checkboxInput(ns("approvals"), "Show approvals?"),
        checkboxInput(ns("qualifiers"), "Show qualifiers?"),
        
        div(
          actionButton(ns("extra_aes"),
                       "Modify plot aesthetics",
                       title = "Modify plot aesthetics such as language, line size, text size.",
                       style = "display: block; width: 100%; margin-bottom: 10px;"), # Ensure block display and full width
          input_task_button(ns("make_plot"), 
                            label = "Create Plot", 
                            style = "display: block; width: 100%;")
        )
      ) # End tagList
    }) %>% # End renderUI
      bindEvent(language$language, moduleData) # Re-render the UI if the language or moduleData changes
    
    output$main <- renderUI({
      tagList(
        uiOutput(ns("working")),
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
      } else {
        shinyjs::hide("grades")
        shinyjs::hide("approvals")
        shinyjs::hide("qualifiers")
      }
    })
    
  }) # End moduleServer
} # End server function
  
