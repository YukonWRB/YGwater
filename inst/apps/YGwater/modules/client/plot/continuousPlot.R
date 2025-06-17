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

continuousPlot <- function(id, language, windowDims, inputs) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns # Used to create UI elements within server
    
    # Adjust multiple selection based on if 'all' is selected
    observeFilterInput <- function(inputId) {
      observeEvent(input[[inputId]], {
        # Check if 'all' is selected and adjust accordingly
        if (length(input[[inputId]]) > 1) { # If 'all' was selected last, remove all other selections
          if (input[[inputId]][length(input[[inputId]])] == "all") {
            updateSelectizeInput(session, inputId, selected = "all")
          } else if ("all" %in% input[[inputId]]) { # If 'all' is already selected and another option is selected, remove 'all'
            updateSelectizeInput(session, inputId, selected = input[[inputId]][length(input[[inputId]])])
          }
        }
      })
    }
    
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
    
    # Create a function to create the filteredData reactiveValues object
    createFilteredData <- function() {
      data <-  reactiveValues(
        locs = isolate(moduleData$locs),
        sub_locs = isolate(moduleData$sub_locs),
        z = isolate(moduleData$z),
        params = isolate(moduleData$params),
        media = isolate(moduleData$media),
        rates = isolate(moduleData$rates),
        aggregation_types = isolate(moduleData$aggregation_types),
        parameter_relationships = isolate(moduleData$parameter_relationships),
        range = isolate(moduleData$range),
        timeseries = isolate(moduleData$timeseries),
        locations_networks = isolate(moduleData$locations_networks),
        locations_projects = isolate(moduleData$locations_projects),
        param_groups = isolate(moduleData$param_groups),
        param_sub_groups = isolate(moduleData$param_sub_groups)
      )
      return(data)
    }

    # Safely calculate date ranges and avoid warnings when no dates are present
    calc_range <- function(df) {
      if (nrow(df) == 0 ||
          all(is.na(df$start_datetime)) ||
          all(is.na(df$end_datetime))) {
        data.frame(min_date = as.Date(NA), max_date = as.Date(NA))
      } else {
        data.frame(
          min_date = min(df$start_datetime, na.rm = TRUE),
          max_date = max(df$end_datetime, na.rm = TRUE)
        )
      }
    }
    
    # Assign the input value to a reactive right away (passed in from the main server) as it's reset to NULL as soon as this module is loaded
    moduleInputs <- reactiveValues(location_id = if (!is.null(inputs$location_id)) as.numeric(inputs$location_id) else NULL)
    
    # If a location was provided from the map module, pre-filter the data, else create the full filteredData object
    if (!is.null(moduleInputs$location_id)) {
      filteredData <- createFilteredData()
      # If the location_id is not in the filteredData$locs, return early
      if (!moduleInputs$location_id %in% filteredData$locs$location_id) {
        moduleInputs$location_id <- NULL
        return()
      }
      
      loc_id <- moduleInputs$location_id
      filteredData$timeseries <- filteredData$timeseries[filteredData$timeseries$location_id %in% loc_id, ]
      filteredData$locs <- filteredData$locs[filteredData$locs$location_id %in% loc_id, ]
      filteredData$sub_locs <- filteredData$sub_locs[filteredData$sub_locs$location_id %in% loc_id, ]
      filteredData$z <- unique(filteredData$timeseries$z[!is.na(filteredData$timeseries$z)])
      filteredData$media <- filteredData$media[filteredData$media$media_id %in% filteredData$timeseries$media_id, ]
      filteredData$aggregation_types <- filteredData$aggregation_types[filteredData$aggregation_types$aggregation_type_id %in% filteredData$timeseries$aggregation_type_id, ]
      filteredData$rates <- filteredData$rates[filteredData$rates$seconds %in% filteredData$timeseries$record_rate, ]
      
      filteredData$range <- calc_range(filteredData$timeseries)
      
      filteredData$params <- filteredData$params[filteredData$params$parameter_id %in% filteredData$timeseries$parameter_id, ]
      if (nrow(filteredData$params) > 0) {
        filteredData$parameter_relationships <- filteredData$parameter_relationships[filteredData$parameter_relationships$parameter_id %in% filteredData$params$parameter_id, ]
        if (length(filteredData$parameter_relationships$group_id) > 0) {
          filteredData$param_groups <- filteredData$param_groups[filteredData$param_groups$group_id %in% filteredData$parameter_relationships$group_id, ]
        } else {
          filteredData$param_groups <- data.frame(group_id = numeric(), group_name = character(), group_name_fr = character(), description = character(), description_fr = character())
        }
        sub_groups <- filteredData$parameter_relationships$sub_group_id[!is.na(filteredData$parameter_relationships$sub_group_id)]
        if (length(sub_groups) > 0) {
          filteredData$param_sub_groups <- filteredData$param_sub_groups[filteredData$param_sub_groups$sub_group_id %in% sub_groups, ]
        } else {
          filteredData$param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = numeric(), sub_group_name_fr = character(), description = character(), description_fr = character())
        }
      }
    } else {
      filteredData <- createFilteredData()
    }
    
    values <- reactiveValues()
    # Find the parameter_ids for 'water level', 'snow water equivalent', 'snow depth' - this is used to change default plot start/end dates and to show the datum checkbox if suitable
    values$water_level <- moduleData$params$parameter_id[moduleData$params$param_name == "water level"]
    values$swe <- moduleData$params$parameter_id[moduleData$params$param_name == "snow water equivalent"]
    values$snow_depth <- moduleData$params$parameter_id[moduleData$params$param_name == "snow depth"]
    
    # Create UI elements and necessary helpers ################
    # NOTE: output$sidebar is rendered at module load time, but also re-rendered whenever a change to the language is made.
    
    # Reactive values to store the input values so that inputs can be reset if the user ends up narrowing their selections to 0 samples
    input_values <- reactiveValues()
    
    # flags to prevent running observers when the sidebar is first rendered
    render_flags <- reactiveValues(
      plot_type = FALSE,
      date_range = FALSE,
      location = FALSE,
      sub_location = FALSE, # Only shown if needed
      z = FALSE, # Only shown if needed
      aggregation = FALSE,
      rate = FALSE,
      media = FALSE,
      param = FALSE
    )
    
    output$sidebar <- renderUI({
      req(moduleData, language$language)
      
      render_flags$plot_type <- TRUE
      render_flags$date_range <- TRUE
      render_flags$location <- TRUE
      render_flags$aggregation <- TRUE
      render_flags$rate <- TRUE
      render_flags$media <- TRUE
      render_flags$param <- TRUE
      
      earliest <- min(filteredData$range$min_date, filteredData$range$max_date - 365, na.rm = TRUE)
      
      tags <- tagList(
        selectizeInput(ns("plot_type"),
                       label = tooltip(
                         trigger = list(
                           "Plot type",
                           bs_icon("info-circle-fill")
                         ),
                         "Long timeseries plots help visualize a change with time and allows for multiple traces or subplots, while overlapping years plots are useful for comparing data across years at one location."
                       ),
                       choices = stats::setNames(c("ts", "over"), c("Long timeseries", "Overlapping years")), 
                       selected = "ts"),
        
        selectizeInput(ns("location"),
                       label = tr("loc", language$language),
                       choices =  stats::setNames(filteredData$locs$location_id,
                                                  filteredData$locs[, tr("generic_name_col", language$language)]),
                       multiple = TRUE,
                       options = list(maxItems = 1),
                       selected = if (!is.null(moduleInputs$location_id)) moduleInputs$location_id else NULL
        ),
        # Button for modal to let users filter locations by network or project
        actionButton(ns("loc_modal"),
                     label = tr("loc_modal", language$language),
                     width = "100%",
                     style = "font-size: 14px; margin-top: 5px;"
        ),
        uiOutput(ns("sub_loc_ui")), # Will be a selectizeInput for sub-locations, shows up only if needed
        
        uiOutput(ns("z_ui")), # Will be a selectizeInput for depth/height, shows up only if needed
        
        # Selectize input for media type
        selectizeInput(ns("media"),
                       label = tr("media_type", language$language),
                       choices = 
                         stats::setNames(filteredData$media$media_id,
                                         filteredData$media[, tr("media_type_col", language$language)]),
                       multiple = TRUE,
                       options = list(maxItems = 1)
        ),
        # Selectize input for aggregation types
        selectizeInput(ns("aggregation"),
                       label = tr("aggregation_type", language$language),
                       choices = stats::setNames(filteredData$aggregation_types$aggregation_type_id,
                                                 filteredData$aggregation_types[, tr("aggregation_type_col", language$language)]),
                       multiple = TRUE,
                       options = list(maxItems = 1)
        ),
        # Selectize input for record rate
        selectizeInput(ns("rate"),
                       label = tr("nominal_rate", language$language),
                       choices = stats::setNames(filteredData$rates$seconds,
                                                 filteredData$rates[, "period"]),
                       multiple = TRUE,
                       options = list(maxItems = 1)
        ),
        selectizeInput(ns("param"),
                       label = tr("parameter", language$language),
                       choices = stats::setNames(filteredData$params$parameter_id,
                                                 filteredData$params[, tr("param_name_col", language$language)]),
                       multiple = TRUE,
                       options = list(maxItems = 1)
        ),
        uiOutput(ns("param_modal_ui")), # Will be a modal button to let users filter parameters by group or sub-group, if there are more than 10 parameters
        # start and end datetime
        dateRangeInput(ns("date_range"),
                       tr("date_range_select", language$language),
                       start = earliest,
                       end = as.Date(filteredData$range$max_date),
                       min = as.Date(filteredData$range$min_date),
                       format = "yyyy-mm-dd",
                       language = language$abbrev
        ),
        
        # Now make a conditional panel depending on the selected plot type
        conditionalPanel(
          ns = ns,
          condition = "input.plot_type == 'over'",
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
        ),
        conditionalPanel(
          ns = ns,
          condition = "input.plot_type == 'ts'",
          uiOutput(ns("trace1_ui")), # Will be a button with the trace values. Upon click, user can edit or remove the trace.
          uiOutput(ns("trace2_ui")),
          uiOutput(ns("trace3_ui")),
          uiOutput(ns("trace4_ui")),
          uiOutput(ns("subplot1_ui")), # Will be a button with the subplot values. Upon click, user can edit or remove the subplot.
          uiOutput(ns("subplot2_ui")),
          uiOutput(ns("subplot3_ui")),
          uiOutput(ns("subplot4_ui")),
          div(
            style = "display: flex; justify-content: flex-start; margin-bottom: 10px; margin-top: 10px;", # Use flexbox to align buttons side by side
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
      
      # Store the input values in the reactiveValues object
      input_values$plot_type <- input$plot_type
      input_values$date_range <- input$date_range
      input_values$location <- input$location
      input_values$sub_location <- character(0)
      input_values$z <- character(0)
      input_values$aggregation <- input$aggregation
      input_values$rate <- input$rate
      input_values$media <- input$media
      input_values$param <- input$param
      
      return(tags)
    }) %>% # End renderUI for sidebar
      bindEvent(language$language) # Re-render the UI if the language or moduleData changes
    
    output$main <- renderUI({
      tagList(
        plotly::plotlyOutput(ns("plot"), width = "100%", height = "800px", inline = TRUE),
        uiOutput(ns("full_screen_ui"))
      ) # End tagList
    }) # End renderUI for main panel
    
    ## Run observeFilterInput for each selectize input where 'all' is an option ##### 
    observeFilterInput("networks")
    observeFilterInput("projects")
    
    
    ## Modals to narrow selections ################
    # Observer for a modal that allows for location filtering based on projects and networks
    observeEvent(input$loc_modal, {
      showModal(modalDialog(
        title = tr("loc_modal", language$language),
        selectizeInput(ns("networks"),
                       label = tr("network(s)", language$language),
                       choices = stats::setNames(c("all", moduleData$networks$network_id),
                                                 c(tr("all", language$language), moduleData$networks[, tr("generic_name_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        selectizeInput(ns("projects"),
                       label = tr("project(s)", language$language),
                       choices = stats::setNames(c("all", moduleData$projects$project_id),
                                                 c(tr("all", language$language), moduleData$projects[, tr("generic_name_col", language$language)])),
                       multiple = TRUE,
                       selected = "all"
        ),
        footer = tagList(
          actionButton(ns("loc_modal_filter"), tr("filter", language$language)),
          modalButton(tr("close", language$language))
        )
      ))
    })
    
    # Observer for the locations modal filter button
    observeEvent(input$loc_modal_filter, {
      # Filter the locations based on the selected projects and networks, update the locations selectizeInput
      req(input$projects, input$networks, filteredData$locations_projects, filteredData$locations_networks)
      
      remain_locs <- filteredData$locs
      
      if (!("all" %in% input$networks)) {
        net_ids <- filteredData$locations_networks$location_id[
          filteredData$locations_networks$network_id %in% input$networks
        ]
        remain_locs <- remain_locs[remain_locs$location_id %in% net_ids, ]
      }
      
      if (!("all" %in% input$projects)) {
        proj_ids <- filteredData$locations_projects$location_id[
          filteredData$locations_projects$project_id %in% input$projects
        ]
        remain_locs <- remain_locs[remain_locs$location_id %in% proj_ids, ]
      }
      updateSelectizeInput(session, "location",
                           choices = stats::setNames(remain_locs$location_id,
                                                     remain_locs[, tr("generic_name_col", language$language)]),
                           selected = character(0))
      removeModal()
    })
    
    
    ## Show/hide the approvals, grades, qualifiers checkboxes based on the selected plot type and traces/subplots ##########################
    observe({
      req(input$plot_type, traceCount(), subplotCount())
      if (input$plot_type == "ts" && traceCount() == 1 && subplotCount() == 1) {
        shinyjs::show("grades")
        shinyjs::show("approvals")
        shinyjs::show("qualifiers")
      } else if (input$plot_type == "ts" && (traceCount() > 1 || subplotCount() > 1)) {
        shinyjs::hide("grades")
        shinyjs::hide("approvals")
        shinyjs::hide("qualifiers")
        shinyjs::hide("location") # Could have been shown again by selecting overlapping years
        shinyjs::hide("param") # Could have been shown again by selecting overlapping years
      } else if (input$plot_type == "over") {
        shinyjs::hide("grades")
        shinyjs::hide("approvals")
        shinyjs::hide("qualifiers")
        shinyjs::show("location") # Would be hidden if multiple subplots or traces are still selected
        shinyjs::show("param") # Would be hidden if multiple subplots or traces are still selected
      }
    })
    
    # Observe the plot type selection and update UI elements accordingly
    observeEvent(input$plot_type, {
      # Don't run this observer if the plot type was set by the module server
      if (render_flags$plot_type) {
        render_flags$plot_type <- FALSE
        return()
      }
      if (input$plot_type == "ts") {
        updateDateRangeInput(session, "date_range",
                             label = tr("date_range_select", language$language))
      } else if (input$plot_type == "over") {
        updateDateRangeInput(session, "date_range",
                             label = tr("date_range_select_doy", language$language))
      }
    })
    
    # Create reactiveValues objects for sub_locs and z here because their observers will not run unless there is a need. This allows filtering to proceed down the line without those observers.
    filteredData_sub_locs <- reactiveValues()
    filteredData_z <- reactiveValues()
  
    observeEvent(input$location, {
      req(filteredData)

      # Filter the data based on the selected locations
      filteredData$timeseries <- moduleData$timeseries[moduleData$timeseries$location_id %in% input$location, ]
      
      filteredData$locs <- moduleData$locs[moduleData$locs$location_id == input$location, ]
      filteredData$sub_locs <- moduleData$sub_locs[moduleData$sub_locs$location_id == filteredData$locs$location_id, ]
      filteredData$z <- unique(filteredData$timeseries$z[!is.na(filteredData$timeseries$z)])
      filteredData$media <- moduleData$media[moduleData$media$media_id %in% filteredData$timeseries$media_id, ]
      filteredData$aggregation_types <- moduleData$aggregation_types[moduleData$aggregation_types$aggregation_type_id %in% filteredData$timeseries$aggregation_type_id, ]
      filteredData$rates <- moduleData$rates[moduleData$rates$seconds %in% filteredData$timeseries$record_rate, ]
      
      filteredData$range <- calc_range(filteredData$timeseries)
      
      filteredData$params <- moduleData$params[moduleData$params$parameter_id %in% filteredData$timeseries$parameter_id, ]
      if (nrow(filteredData$params) > 0) {
        filteredData$parameter_relationships <- moduleData$parameter_relationships[moduleData$parameter_relationships$parameter_id %in% filteredData$params$parameter_id, ]
        if (length(filteredData$parameter_relationships$group_id) > 0) {
          filteredData$param_groups <- moduleData$param_groups[moduleData$param_groups$group_id %in% filteredData$parameter_relationships$group_id, ]
        } else {
          filteredData$param_groups <- data.frame(group_id = numeric(), group_name = character(), group_name_fr = character(), description = character(), description_fr = character())
        }
        sub_groups <- filteredData$parameter_relationships$sub_group_id[!is.na(filteredData$parameter_relationships$sub_group_id)]
        if (length(sub_groups) > 0) {
          filteredData$param_sub_groups <- filteredData$param_sub_groups[filteredData$param_sub_groups$sub_group_id %in% sub_groups, ]
        } else {
          filteredData$param_sub_groups <- data.frame(sub_group_id = numeric(), sub_group_name = numeric(), sub_group_name_fr = character(), description = character(), description_fr = character())
        }
      }
      
      if (nrow(filteredData$sub_locs) > 1) {
        if (!render_flags$sub_location) {
          output$sub_loc_ui <- renderUI({
            # If there are sub-locations for the selected location, show a selectizeInput for sub-locations
            selectizeInput(ns("sub_location"),
                           label = tr("sub_loc", language$language),
                           choices = stats::setNames(filteredData$sub_locs[filteredData$sub_locs$location_id == input$location, "sub_location_id"],
                                                     filteredData$sub_locs[filteredData$sub_locs$location_id == input$location, tr("sub_location_col", language$language)]),
                           multiple = TRUE,
                           options = list(maxItems = 1))
          })
          render_flags$sub_location <- TRUE
        } else {
          updateSelectizeInput(session, "sub_location",
                             choices = stats::setNames(filteredData$sub_locs[filteredData$sub_locs$location_id == input$location, "sub_location_id"],
                                                       filteredData$sub_locs[filteredData$sub_locs$location_id == input$location, tr("sub_location_col", language$language)]),
                             selected = character(0))
          shinyjs::show("sub_location")
        }
      } else {
        shinyjs::hide("sub_location")
      }
      
      if (length(filteredData$z) > 1) {
        if (!render_flags$z) {
          output$z_ui <- renderUI({
            # If there are z values for the selected location, show a selectizeInput for z
            selectizeInput(ns("z"), 
                           label = tr("z", language$language),
                           choices = filteredData$z,
                           multiple = TRUE,
                           options = list(maxItems = 1))
          })
          render_flags$z <- TRUE
        } else {
          updateSelectizeInput(session, "z",
                               choices = filteredData$z,
                               selected = character(0))
          shinyjs::show("z")
        }
      } else {
        shinyjs::hide("z")
      }
      
      # Update the media, aggregation, rate, and param selectizeInputs with what's left in filteredData.
      # If possible, keep the previous selection, otherwise if there's only one choice available, select it, else null
      tmp.choices <- stats::setNames(filteredData$media$media_id,
                                     filteredData$media[, tr("media_type_col", language$language)])
      if (!is.null(input$media)) {
        tmp.selected <- if (input$media %in% tmp.choices) input$media else if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      }
      updateSelectizeInput(session, "media",
                           choices = tmp.choices,
                           selected = tmp.selected)
      
      tmp.choices <- stats::setNames(filteredData$aggregation_types$aggregation_type_id,
                                     filteredData$aggregation_types[, tr("aggregation_type_col", language$language)])
      if (!is.null(input$aggregation)) {
        tmp.selected <- if (input$aggregation %in% tmp.choices) input$aggregation else if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      }
      updateSelectizeInput(session, "aggregation",
                           choices = tmp.choices,
                           selected = tmp.selected)
      
      tmp.choices <- stats::setNames(filteredData$rates$seconds,
                                     filteredData$rates[, "period"])
      if (!is.null(input$rate)) {
        tmp.selected <- if (input$rate %in% tmp.choices) input$rate else if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      }
      updateSelectizeInput(session, "rate",
                           choices = tmp.choices,
                           selected = tmp.selected)
      
      tmp.choices <- stats::setNames(filteredData$params$parameter_id,
                                     filteredData$params[, tr("param_name_col", language$language)])
      if (!is.null(input$param)) {
        tmp.selected <- if (input$param %in% tmp.choices) input$param else if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      }
      updateSelectizeInput(session, "param",
                           choices = tmp.choices,
                           selected = tmp.selected)
      
      earliest <- min(filteredData$range$min_date, filteredData$range$max_date - 365, na.rm = TRUE)
      updateDateRangeInput(session, "date_range",
                           start = earliest,
                           end = as.Date(filteredData$range$max_date),
                           min = as.Date(filteredData$range$min_date),
                           max = as.Date(filteredData$range$max_date))
      
      filteredData_sub_locs <- filteredData
      filteredData_z <- filteredData
    }, ignoreInit = TRUE)
    
    
    observeEvent(input$sub_location, {
      req(filteredData)

      # Filter the data based on the selected sub-locations
      if (is.null(input$sub_location) || length(input$sub_location) != 1) return()
      
      # Find the new timeseries rows based on the selected sub-location
      filteredData_sub_locs$timeseries <- filteredData$timeseries[filteredData$timeseries$sub_location_id == input$sub_location, ]
        
      filteredData_sub_locs$z <- unique(filteredData_sub_locs$timeseries$z[!is.na(filteredData_sub_locs$timeseries$z)])
      filteredData_sub_locs$media <- filteredData$media[filteredData$media$media_id %in% filteredData_sub_locs$timeseries$media_id, ]
      filteredData_sub_locs$aggregation_types <- filteredData$aggregation_types[filteredData$aggregation_types$aggregation_type_id %in% filteredData_sub_locs$timeseries$aggregation_type_id, ]
      filteredData_sub_locs$rates <- filteredData$rates[filteredData$rates$seconds %in% filteredData_sub_locs$timeseries$record_rate, ]
      filteredData_sub_locs$params <- filteredData$params[filteredData$params$parameter_id %in% filteredData_sub_locs$timeseries$parameter_id, ]
      
      filteredData_sub_locs$range <- calc_range(filteredData_sub_locs$timeseries)
      
      # Update the z selectizeInput based on the selected sub-locations
      if (length(filteredData_sub_locs$z) > 1) {
        updateSelectizeInput(session, "z",
                             choices = filteredData_sub_locs$z,
                             selected = character(0))
        shinyjs::show("z")
      } else {
        shinyjs::hide("z")
      }
      
      # Update the media, aggregation, rate, and param selectizeInputs with what's left in filteredData_sub_locs.
      # If possible, keep the previous selection, otherwise if there's only one choice available, select it, else null
      tmp.choices <- stats::setNames(filteredData_sub_locs$media$media_id,
                                     filteredData_sub_locs$media[, tr("media_type_col", language$language)])
      if (!is.null(input$media)) {
        tmp.selected <- if (input$media %in% tmp.choices) input$media else if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      }
      updateSelectizeInput(session, "media",
                           choices = tmp.choices,
                           selected = tmp.selected)
      
      tmp.choices <- stats::setNames(filteredData_sub_locs$aggregation_types$aggregation_type_id,
                                     filteredData_sub_locs$aggregation_types[, tr("aggregation_type_col", language$language)])
      if (!is.null(input$aggregation)) {
        tmp.selected <- if (input$aggregation %in% tmp.choices) input$aggregation else if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      }
      updateSelectizeInput(session, "aggregation",
                           choices = tmp.choices,
                           selected = tmp.selected)
      
      tmp.choices <- stats::setNames(filteredData_sub_locs$rates$seconds,
                                     filteredData_sub_locs$rates[, "period"])
      if (!is.null(input$rate)) {
        tmp.selected <- if (input$rate %in% tmp.choices) input$rate else if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      }
      updateSelectizeInput(session, "rate",
                           choices = tmp.choices,
                           selected = tmp.selected)
      
      tmp.choices <- stats::setNames(filteredData_sub_locs$params$parameter_id,
                                     filteredData_sub_locs$params[, tr("param_name_col", language$language)])
      if (!is.null(input$param)) {
        tmp.selected <- if (input$param %in% tmp.choices) input$param else if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      }
      updateSelectizeInput(session, "param",
                           choices = tmp.choices,
                           selected = tmp.selected)
      
      earliest <- min(filteredData_sub_locs$range$min_date, filteredData_sub_locs$range$max_date - 365, na.rm = TRUE)
      updateDateRangeInput(session, "date_range",
                           start = earliest,
                           end = as.Date(filteredData_sub_locs$range$max_date),
                           min = as.Date(filteredData_sub_locs$range$min_date),
                           max = as.Date(filteredData_sub_locs$range$max_date))
      filteredData_z <- filteredData_sub_locs
    })
    
    
    observeEvent(input$z, {
      req(filteredData_sub_locs)
      
      # Filter the data based on the selected z values
      if (is.null(input$z) || length(input$z) != 1) return()
      
      # Find the new timeseries rows based on the selected z value
      filteredData_z$timeseries <- filteredData_sub_locs$timeseries[filteredData_sub_locs$timeseries$z == input$z, ]
      
      filteredData_z$media <- filteredData_sub_locs$media[filteredData_sub_locs$media$media_id %in% filteredData_z$timeseries$media_id, ]
      filteredData_z$aggregation_types <- filteredData_sub_locs$aggregation_types[filteredData_sub_locs$aggregation_types$aggregation_type_id %in% filteredData_z$timeseries$aggregation_type_id, ]
      filteredData_z$rates <- filteredData_sub_locs$rates[filteredData_sub_locs$rates$seconds %in% filteredData_z$timeseries$record_rate, ]
      filteredData_z$params <- filteredData_sub_locs$params[filteredData_sub_locs$params$parameter_id %in% filteredData_z$timeseries$parameter_id, ]
      filteredData_z$range <- calc_range(filteredData_z$timeseries)
      
      # Update the media, aggregation, rate, and param selectizeInputs with what's left in filteredData_z.
      # If possible, keep the previous selection, otherwise if there's only one choice available, select it, else null
      tmp.choices <- stats::setNames(filteredData_z$media$media_id,
                                     filteredData_z$media[, tr("media_type_col", language$language)])
      if (!is.null(input$media)) {
        tmp.selected <- if (input$media %in% tmp.choices) input$media else if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      }
      updateSelectizeInput(session, "media",
                           choices = tmp.choices,
                           selected = tmp.selected)
      
      tmp.choices <- stats::setNames(filteredData_z$aggregation_types$aggregation_type_id,
                                     filteredData_z$aggregation_types[, tr("aggregation_type_col", language$language)])
      if (!is.null(input$aggregation)) {
        tmp.selected <- if (input$aggregation %in% tmp.choices) input$aggregation else if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      }
      updateSelectizeInput(session, "aggregation",
                           choices = tmp.choices,
                           selected = tmp.selected)
      
      tmp.choices <- stats::setNames(filteredData_z$rates$seconds,
                                     filteredData_z$rates[, "period"])
      if (!is.null(input$rate)) {
        tmp.selected <- if (input$rate %in% tmp.choices) input$rate else if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      }
      updateSelectizeInput(session, "rate",
                           choices = tmp.choices,
                           selected = tmp.selected)
      
      tmp.choices <- stats::setNames(filteredData_z$params$parameter_id,
                                     filteredData_z$params[, tr("param_name_col", language$language)])
      if (!is.null(input$param)) {
        tmp.selected <- if (input$param %in% tmp.choices) input$param else if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      }
      updateSelectizeInput(session, "param",
                           choices = tmp.choices,
                           selected = tmp.selected)
      
      earliest <- min(filteredData_z$range$min_date, filteredData_z$range$max_date - 365, na.rm = TRUE)
      updateDateRangeInput(session, "date_range",
                           start = earliest,
                           end = as.Date(filteredData_z$range$max_date),
                           min = as.Date(filteredData_z$range$min_date),
                           max = as.Date(filteredData_z$range$max_date))
      
    })
    
    
    filteredData_media <- reactiveValues()
    observeEvent(input$media, {
      req(filteredData_z)

      # Filter the data based on the selected media
      if (is.null(input$media) || length(input$media) != 1) return()
      
      # Find the new timeseries rows based on the selected media
      filteredData_media$timeseries <- filteredData_z$timeseries[filteredData_z$timeseries$media_id == input$media, ]

      filteredData_media$aggregation_types <- filteredData_z$aggregation_types[filteredData_z$aggregation_types$aggregation_type_id %in% filteredData_media$timeseries$aggregation_type_id, ]
      filteredData_media$rates <- filteredData_z$rates[filteredData_z$rates$seconds %in% filteredData_media$timeseries$record_rate, ]
      filteredData_media$params <- filteredData_z$params[filteredData_z$params$parameter_id %in% filteredData_media$timeseries$parameter_id, ]
      filteredData_media$range <- calc_range(filteredData_media$timeseries)
      
      # Update the aggregation, rate, and param selectizeInputs with what's left in filteredData_media.
      # If possible, keep the previous selection, otherwise if there's only one choice available, select it, else null
      tmp.choices <- stats::setNames(filteredData_media$aggregation_types$aggregation_type_id,
                                     filteredData_media$aggregation_types[, tr("aggregation_type_col", language$language)])
      if (!is.null(input$aggregation)) {
        tmp.selected <- if (input$aggregation %in% tmp.choices) input$aggregation else if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      }
      updateSelectizeInput(session, "aggregation",
                           choices = tmp.choices,
                           selected = tmp.selected)
      
      tmp.choices <- stats::setNames(filteredData_media$rates$seconds,
                                     filteredData_media$rates[, "period"])
      if (!is.null(input$rate)) {
        tmp.selected <- if (input$rate %in% tmp.choices) input$rate else if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      }
      updateSelectizeInput(session, "rate",
                           choices = tmp.choices,
                           selected = tmp.selected)
      
      tmp.choices <- stats::setNames(filteredData_media$params$parameter_id,
                                     filteredData_media$params[, tr("param_name_col", language$language)])
      if (!is.null(input$param)) {
        tmp.selected <- if (input$param %in% tmp.choices) input$param else if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      }
      updateSelectizeInput(session, "param",
                           choices = tmp.choices,
                           selected = tmp.selected)
      
      earliest <- min(filteredData_media$range$min_date, filteredData_media$range$max_date - 365, na.rm = TRUE)
      updateDateRangeInput(session, "date_range",
                           start = earliest,
                           end = as.Date(filteredData_media$range$max_date),
                           min = as.Date(filteredData_media$range$min_date),
                           max = as.Date(filteredData_media$range$max_date))
    })
      
    filteredData_aggregation <- reactiveValues()
    observeEvent(input$aggregation, {
      req(filteredData_media)

      # Filter the data based on the selected aggregation type
      if (is.null(input$aggregation) || length(input$aggregation) != 1) return()
      
      # Find the new timeseries rows based on the selected aggregation type
      filteredData_aggregation$timeseries <- filteredData_media$timeseries[filteredData_media$timeseries$aggregation_type_id == input$aggregation, ]
      
      filteredData_aggregation$rates <- filteredData_media$rates[filteredData_media$rates$seconds %in% filteredData_aggregation$timeseries$record_rate, ]
      filteredData_aggregation$params <- filteredData_media$params[filteredData_media$params$parameter_id %in% filteredData_aggregation$timeseries$parameter_id, ]
      filteredData_aggregation$range <- calc_range(filteredData_aggregation$timeseries)
      
      # Update the rate and param selectizeInputs with what's left in filteredData_aggregation.
      # If possible, keep the previous selection, otherwise if there's only one choice available, select it, else null
      tmp.choices <- stats::setNames(filteredData_aggregation$rates$seconds,
                                     filteredData_aggregation$rates[, "period"])
      if (!is.null(input$rate)) {
        tmp.selected <- if (input$rate %in% tmp.choices) input$rate else if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      }
      updateSelectizeInput(session, "rate",
                           choices = tmp.choices,
                           selected = tmp.selected)
      
      tmp.choices <- stats::setNames(filteredData_aggregation$params$parameter_id,
                                     filteredData_aggregation$params[, tr("param_name_col", language$language)])
      if (!is.null(input$param)) {
        tmp.selected <- if (input$param %in% tmp.choices) input$param else if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      }
      updateSelectizeInput(session, "param",
                           choices = tmp.choices,
                           selected = tmp.selected)
      
      earliest <- min(filteredData_aggregation$range$min_date, filteredData_aggregation$range$max_date - 365, na.rm = TRUE)
      updateDateRangeInput(session, "date_range",
                           start = earliest,
                           end = as.Date(filteredData_aggregation$range$max_date),
                           min = as.Date(filteredData_aggregation$range$min_date),
                           max = as.Date(filteredData_aggregation$range$max_date))
    })
    
    filteredData_rate <- reactiveValues()
    observeEvent(input$rate, {
      req(filteredData_aggregation)

      # Filter the data based on the selected rate
      if (is.null(input$rate) || length(input$rate) != 1) return()
      
      # Find the new timeseries rows based on the selected rate
      filteredData_rate$timeseries <- filteredData_aggregation$timeseries[filteredData_aggregation$timeseries$record_rate == input$rate, ]
      
      filteredData_rate$params <- filteredData_aggregation$params[filteredData_aggregation$params$parameter_id %in% filteredData_rate$timeseries$parameter_id, ]
      filteredData_rate$range <- calc_range(filteredData_rate$timeseries)
      
      # Update the param selectizeInput with what's left in filteredData_rate.
      # If possible, keep the previous selection, otherwise if there's only one choice available, select it, else null
      tmp.choices <- stats::setNames(filteredData_rate$params$parameter_id,
                                     filteredData_rate$params[, tr("param_name_col", language$language)])
      if (!is.null(input$param)) {
        tmp.selected <- if (input$param %in% tmp.choices) input$param else if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) tmp.choices[1] else character(0)
      }
      updateSelectizeInput(session, "param",
                           choices = tmp.choices,
                           selected = tmp.selected)
      
      earliest <- min(filteredData_rate$range$min_date, filteredData_rate$range$max_date - 365, na.rm = TRUE)
      updateDateRangeInput(session, "date_range",
                           start = earliest,
                           end = as.Date(filteredData_rate$range$max_date),
                           min = as.Date(filteredData_rate$range$min_date),
                           max = as.Date(filteredData_rate$range$max_date))
    })
    
    # observing for parameter, only need to update the date range and years inputs
    observeEvent(input$param, {
      req(filteredData_rate)

      # Filter the data based on the selected parameter
      if (is.null(input$param) || length(input$param) != 1) return()
      
      # Find the single timeseries rows based on the selected parameter
      tmp.timeseries <- filteredData_rate$timeseries[filteredData_rate$timeseries$parameter_id == input$param, ]
      
      tmp.range <- calc_range(tmp.timeseries)
      
      earliest <- min(tmp.range$min_date, tmp.range$max_date - 365, na.rm = TRUE)
      updateDateRangeInput(session, "date_range",
                           start = earliest,
                           end = as.Date(tmp.range$max_date),
                           min = as.Date(tmp.range$min_date),
                           max = as.Date(tmp.range$max_date))
      possible_years <- seq(
        as.numeric(substr(tmp.range$min_date, 1, 4)),
        as.numeric(substr(tmp.range$max_date, 1, 4))
      )
      updateSelectizeInput(session, "years",
                           choices = possible_years,
                           selected = max(possible_years))
    })
    
    # When the user has narrowed down to a single selection for location, sub-location (if necessary), z (if necessary), media, aggregation, rate, and parameter, update the selections for 'date_range' and 'years' inputs
    # observe({
    #   req(filteredData, input$location, input$media, input$aggregation, input$rate, input$param)
    #   
    #   # If the user has selected a single location, sub-location (if necessary), z (if necessary), media, aggregation, rate, and parameter, update the date range and years inputs
    #   if (length(input$location) == 1 && 
    #       (is.null(input$sub_location) || length(input$sub_location) == 1) &&
    #       (is.null(input$z) || length(input$z) == 1) &&
    #       length(input$media) == 1 && 
    #       length(input$aggregation) == 1 && 
    #       length(input$rate) == 1 && 
    #       length(input$param) == 1) {
    #     
    #     # Update the date range input
    #     rows <- filteredData$timeseries$location_id == input$location &
    #       filteredData$timeseries$parameter_id == input$param &
    #       filteredData$timeseries$media_id == input$media &
    #       filteredData$timeseries$record_rate == input$rate &
    #       filteredData$timeseries$aggregation_type_id == input$aggregation
    #     
    #     if (any(rows)) {
    #       start_date <- as.Date(filteredData$timeseries[rows, "start_datetime"])
    #       end_date <- as.Date(filteredData$timeseries[rows, "end_datetime"])
    #       updateDateRangeInput(session, "date_range",
    #                            start = end_date - 365,
    #                            end = end_date,
    #                            min = start_date,
    #                            max = end_date)
    #       
    #       possible_years <- seq(
    #         as.numeric(substr(start_date, 1, 4)),
    #         as.numeric(substr(end_date, 1, 4))
    #       )
    #       updateSelectizeInput(session, "years",
    #                            choices = possible_years,
    #                            selected = max(possible_years))
    #     } else {
    #       updateDateRangeInput(session, "date_range",
    #                            start = as.Date(filteredData$range$max_date) - 365,
    #                            end = as.Date(filteredData$range$max_date),
    #                            min = as.Date(filteredData$range$min_date),
    #                            max = as.Date(filteredData$range$max_date))
    #       updateSelectizeInput(session, "years", choices = NULL)
    #     }
    #     
    #   } else {
    #     # Reset the date range and years inputs if not narrowed down to a single selection
    #     updateDateRangeInput(session, "date_range",
    #                          start = as.Date(filteredData$range$max_date) - 365,
    #                          end = as.Date(filteredData$range$max_date),
    #                          min = as.Date(filteredData$range$min_date),
    #                          max = as.Date(filteredData$range$max_date))
    #     updateSelectizeInput(session, "years", choices = NULL)
    #   }
    # })
    
    
    
    
    
    
    
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
      # Make sure that there is an input$param, input$location before running the modal; give the user an informative modal if not
      if (nchar(input$location) == 0 | nchar(input$param) == 0) {
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
                              location_id = input$location,
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
        shinyjs::hide("location")
        
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
        shinyjs::show("location")
        shinyjs::show("add_subplot")
        updateSelectizeInput(session, "param", 
                             choices = stats::setNames(moduleData$params$parameter_id, titleCase(moduleData$params$param_name)), 
                             selected = traces$trace1$parameter)
        
        # Update the location choices
        locs <- unique(moduleData$all_ts[moduleData$all_ts$parameter_id == input$param, c(tr("generic_name_col", language$language), "location_id")])
        names(locs) <- c("name", "location_id")
        updateSelectizeInput(session, "location", 
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
      
      # Make sure that there is an input$param, input$location before running the modal; give the user an informative modal if not
      if (nchar(input$location) == 0 | nchar(input$param) == 0) {
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
                                  location_id = input$location)
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
        shinyjs::hide("location")
        
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
        shinyjs::show("location")
        shinyjs::show("add_trace")
        updateSelectizeInput(session, "param", 
                             choices = stats::setNames(moduleData$params$parameter_id, titleCase(moduleData$params$param_name)), 
                             selected = subplots$subplot1$parameter)
        
        # Update the location choices
        locs <- unique(moduleData$all_ts[moduleData$all_ts$parameter_id == input$param, c(tr("generic_name_col", language$language), "location_id")])
        names(locs) <- c("name", "location_id")
        updateSelectizeInput(session, "location", 
                             choices = stats::setNames(locs$location_id, locs$name), 
                             selected = subplots$subplot1$location_id)
      } else {
        shinyjs::show("subplot1_ui")
      }
      removeModal()
    })
    
    
    # Create ExtendedTasks to render plots ############################################################
    # Overlapping years plot
    plot_output_overlap <- ExtendedTask$new(function(loc, param, date_start, date_end, yrs, historic_range, apply_datum, filter, unusable, line_scale, axis_scale, legend_scale, legend_position, lang, gridx, gridy, config) {
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
                              startDay = date_start,
                              endDay = date_end,
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
    plot_output_timeseries <- ExtendedTask$new(function(loc, param, date_start, date_end, historic_range, apply_datum, filter, unusable, grades, approvals, qualifiers, line_scale, axis_scale, legend_scale, legend_position, lang, gridx, gridy, config) {
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
                                 start_date = date_start,
                                 end_date = date_end,
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
    plot_output_timeseries_traces <- ExtendedTask$new(function(locs, params, lead_lags, date_start, date_end, historic_range, apply_datum, filter, unusable, line_scale, axis_scale, legend_scale, legend_position, lang, gridx, gridy, shareX, shareY, config) {
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
                                      start_date = date_start,
                                      end_date = date_end,
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
    plot_output_timeseries_subplots <- ExtendedTask$new(function(locs, params, date_start, date_end, historic_range, apply_datum, filter, unusable, line_scale, axis_scale, legend_scale, legend_position, lang, gridx, gridy, shareX, shareY, config) {
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
                                      start_date = date_start,
                                      end_date = date_end,
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
      
      if (input$plot_type == "over") {
        if (nchar(input$location) == 0) {
          showModal(modalDialog("Please select a location.", easyClose = TRUE))
          return()
        }
        if (nchar(input$param) == 0) {
          showModal(modalDialog("Please select a parameter", easyClose = TRUE))
          return()
        }
        plot_output_overlap$invoke(loc = as.numeric(input$location), param = as.numeric(input$param), date_start = input$date_range[1], date_end = input$date_range[2], yrs = input$years, historic_range = input$historic_range_overlap, apply_datum = input$apply_datum, filter = if (input$plot_filter) 20 else NULL, unusable = input$unusable, line_scale = plot_aes$line_scale, axis_scale = plot_aes$axis_scale, legend_scale = plot_aes$legend_scale, legend_position = if (windowDims()$width > 1.3 * windowDims()$height) "v" else "h", lang = plot_aes$lang, gridx = plot_aes$showgridx, gridy = plot_aes$showgridy, config = session$userData$config)
      } else if (input$plot_type == "ts") {
        if (traceCount() == 1) { # Either a single trace or more than 1 subplot
          if (subplotCount() > 1) { # Multiple sub plots
            locs <- c(subplots$subplot1$location_id, subplots$subplot2$location_id, subplots$subplot3$location_id, subplots$subplot4$location_id)
            params <- c(subplots$subplot1$parameter, subplots$subplot2$parameter, subplots$subplot3$parameter, subplots$subplot4$parameter)
            plot_output_timeseries_subplots$invoke(locs = as.numeric(locs), params = params, date_start = input$date_range[1], date_end = input$date_range[2], historic_range = input$historic_range, apply_datum = input$apply_datum, filter = if (input$plot_filter) 20 else NULL, unusable = input$unusable, line_scale = plot_aes$line_scale, axis_scale = plot_aes$axis_scale, legend_scale = plot_aes$legend_scale, legend_position = if (windowDims()$width > 1.3 * windowDims()$height) "v" else "h", lang = plot_aes$lang, gridx = plot_aes$showgridx, gridy = plot_aes$showgridy, shareX = input$shareX, shareY = input$shareY, config = session$userData$config)
          } else {  # Single trace
            if (nchar(input$location) == 0) {
              showModal(modalDialog("Please select a location.", easyClose = TRUE))
              return()
            }
            if (nchar(input$param) == 0) {
              showModal(modalDialog("Please select a parameter", easyClose = TRUE))
              return()
            }
            plot_output_timeseries$invoke(loc = as.numeric(input$location), param = as.numeric(input$param), date_start = input$date_range[1], date_end = input$date_range[2], historic_range = input$historic_range, apply_datum = input$apply_datum, filter = if (input$plot_filter) 20 else NULL, unusable = input$unusable, grades = input$grades, approvals = input$approvals, qualifiers = input$qualifiers, line_scale = plot_aes$line_scale, axis_scale = plot_aes$axis_scale, legend_scale = plot_aes$legend_scale, legend_position = if (windowDims()$width > 1.3 * windowDims()$height) "v" else "h", lang = plot_aes$lang, gridx = plot_aes$showgridx, gridy = plot_aes$showgridy, config = session$userData$config)
          }
        } else { # Multiple traces, single plot
          locs <- c(traces$trace1$location_id, traces$trace2$location_id, traces$trace3$location_id, traces$trace4$location_id)
          params <- c(traces$trace1$parameter, traces$trace2$parameter, traces$trace3$parameter, traces$trace4$parameter)
          lead_lags <- c(traces$trace1$lead_lag, traces$trace2$lead_lag, traces$trace3$lead_lag, traces$trace4$lead_lag)
          
          plot_output_timeseries_traces$invoke(locs = as.numeric(locs), params = params, lead_lags = lead_lags, date_start = input$date_range[1], date_end = input$date_range[2], historic_range = input$historic_range, apply_datum = input$apply_datum, filter = if (input$plot_filter) 20 else NULL, unusable = input$unusable, line_scale = plot_aes$line_scale, axis_scale = plot_aes$axis_scale, legend_scale = plot_aes$legend_scale, legend_position = if (windowDims()$width > 1.3 * windowDims()$height) "v" else "h", lang = plot_aes$lang, gridx = plot_aes$showgridx, gridy = plot_aes$showgridy, shareX = input$shareX, shareY = input$shareY, config = session$userData$config)
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
        if (input$plot_type == "over") {
          openxlsx::write.xlsx(plot_output_overlap$result()$data, file)
          
        } else if (input$plot_type == "ts") {
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
