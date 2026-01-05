contPlotOldUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(
      HTML(sprintf(
        "
     /* Add colors to the accordion. Using ns() makes it specific to each accordion */
      #%s.accordion {
        /* body background */
        --bs-accordion-bg:          #FFFCF5;
        /* collapsed header */
        --bs-accordion-btn-bg:      #FBE5B2;
        /* expanded header */
        --bs-accordion-active-bg:   #FBE5B2;
      }
    ",
        ns("accordion1")
      ))
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

contPlotOld <- function(id, language, windowDims, inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # Used to create UI elements within server

    # Adjust multiple selection based on if 'all' is selected
    observeFilterInput <- function(inputId) {
      observeEvent(input[[inputId]], {
        # Check if 'all' is selected and adjust accordingly
        if (length(input[[inputId]]) > 1) {
          # If 'all' was selected last, remove all other selections
          if (input[[inputId]][length(input[[inputId]])] == "all") {
            updateSelectizeInput(session, inputId, selected = "all")
          } else if ("all" %in% input[[inputId]]) {
            # If 'all' is already selected and another option is selected, remove 'all'
            updateSelectizeInput(
              session,
              inputId,
              selected = input[[inputId]][length(input[[inputId]])]
            )
          }
        }
      })
    }

    # Initial setup and data loading ########################################################################
    # Get the data to populate drop-downs. Runs every time this module is loaded.
    # !important!!! shares a cache with the data module
    if (session$userData$user_logged_in) {
      # If logged in, get or create data that lives only with this session,
      cached <- cont_data.plot_module_data(
        con = session$userData$AquaCache,
        env = session$userData$app_cache
      )
    } else {
      cached <- cont_data.plot_module_data(con = session$userData$AquaCache)
    }
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
      data <- reactiveValues(
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

    filteredData <- createFilteredData()

    # Safely calculate date ranges and avoid warnings when no dates are present
    calc_range <- function(df) {
      if (
        nrow(df) == 0 ||
          all(is.na(df$start_datetime)) ||
          all(is.na(df$end_datetime))
      ) {
        data.frame(min_date = as.Date(NA), max_date = as.Date(NA))
      } else {
        data.frame(
          min_date = as.Date(min(df$start_datetime, na.rm = TRUE)),
          max_date = as.Date(max(df$end_datetime, na.rm = TRUE))
        )
      }
    }

    # Assign the input value to a reactive right away (passed in from the main server) as it's reset to NULL as soon as this module is loaded. Will be used in the initial renderUI to set the location selectizeInput value,
    moduleInputs <- reactiveValues(
      location_id = if (!is.null(inputs$location_id)) {
        as.numeric(inputs$location_id)
      } else {
        NULL
      }
    )

    values <- reactiveValues()
    # Find the parameter_ids for 'water level', 'snow water equivalent', 'snow depth' - this is used to change default plot start/end dates and to show the datum checkbox if suitable
    values$water_level <- moduleData$params$parameter_id[
      moduleData$params$param_name == "water level"
    ]
    values$water_flow <- moduleData$params$parameter_id[
      moduleData$params$param_name == "water flow"
    ]
    values$swe <- moduleData$params$parameter_id[
      moduleData$params$param_name == "snow water equivalent"
    ]
    values$snow_depth <- moduleData$params$parameter_id[
      moduleData$params$param_name == "snow depth"
    ]

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

      earliest <- filteredData$range$max_date - 366
      if (earliest < filteredData$range$min_date) {
        earliest <- filteredData$range$min_date
      }

      tags <- tagList(
        selectizeInput(
          ns("plot_type"),
          label = tooltip(
            trigger = list(
              tr("plot_type", language$language),
              bsicons::bs_icon("info-circle-fill")
            ),
            tr("plot_type_tooltip", language$language),
          ),
          choices = stats::setNames(
            c("ts", "over"),
            c(
              tr("plot_type_ts", language$language),
              tr("plot_type_overlap", language$language)
            )
          ),
          selected = "ts"
        ),

        selectizeInput(
          ns("location"),
          label = tr("loc", language$language),
          choices = stats::setNames(
            filteredData$locs$location_id,
            filteredData$locs[[tr("generic_name_col", language$language)]]
          ),
          multiple = TRUE,
          options = list(maxItems = 1),
          selected = if (!is.null(moduleInputs$location_id)) {
            moduleInputs$location_id
          } else {
            NULL
          }
        ),
        # Button for modal to let users filter locations by network or project
        actionButton(
          ns("loc_modal"),
          label = tr("loc_modal", language$language),
          width = "100%",
          style = "font-size: 14px; margin-top: 5px;"
        ),
        uiOutput(ns("sub_loc_ui")), # Will be a selectizeInput for sub-locations, shows up only if needed

        uiOutput(ns("z_ui")), # Will be a selectizeInput for depth/height, shows up only if needed

        # Selectize input for media type
        selectizeInput(
          ns("media"),
          label = tr("media_type", language$language),
          choices = stats::setNames(
            filteredData$media$media_id,
            filteredData$media[[tr("media_type_col", language$language)]]
          ),
          multiple = TRUE,
          options = list(maxItems = 1)
        ),
        # Selectize input for aggregation types
        selectizeInput(
          ns("aggregation"),
          label = tr("aggregation_type", language$language),
          choices = stats::setNames(
            filteredData$aggregation_types$aggregation_type_id,
            filteredData$aggregation_types[[tr(
              "aggregation_type_col",
              language$language
            )]]
          ),
          multiple = TRUE,
          options = list(maxItems = 1)
        ),
        # Selectize input for record rate
        selectizeInput(
          ns("rate"),
          label = tr("nominal_rate", language$language),
          choices = stats::setNames(
            filteredData$rates$seconds,
            filteredData$rates[["period"]]
          ),
          multiple = TRUE,
          options = list(maxItems = 1)
        ),
        selectizeInput(
          ns("param"),
          label = tr("parameter", language$language),
          choices = stats::setNames(
            filteredData$params$parameter_id,
            filteredData$params[[tr("param_name_col", language$language)]]
          ),
          multiple = TRUE,
          options = list(maxItems = 1)
        ),
        # start and end datetime
        dateRangeInput(
          ns("date_range"),
          tr("date_range_lab", language$language),
          start = earliest,
          end = as.Date(filteredData$range$max_date),
          min = as.Date(filteredData$range$min_date),
          format = "yyyy-mm-dd",
          language = language$abbrev,
          separator = tr("date_sep", language$language)
        ),

        # Now make a conditional panel depending on the selected plot type
        conditionalPanel(
          ns = ns,
          condition = "input.plot_type == 'over'",
          div(
            selectizeInput(
              ns("years"),
              label = tr("plot_select_years", language$language),
              choices = NULL,
              multiple = TRUE
            ), # Choices are populated based on the location and parameter
            style = "display: flex; align-items: center;",
            span(
              id = ns("log_info_years"),
              `data-bs-toggle` = "tooltip",
              `data-placement` = "right",
              `data-trigger` = "click hover",
              title = tr("plot_select_yrs", language$language),
              icon("info-circle", style = "font-size: 100%; margin-left: 5px;")
            )
          ),
          div(
            selectizeInput(
              ns("historic_range_overlap"),
              label = tr("plot_hist_range_select", language$language),
              choices = stats::setNames(
                c("all", "last"),
                c(
                  tr("all_yrs_record", language$language),
                  tr("last_yr_only", language$language)
                )
              ),
              selected = "all"
            ),
            style = "display: flex; align-items: center;",
            span(
              id = ns("log_info_hist_range"),
              `data-bs-toggle` = "tooltip",
              `data-placement` = "right",
              `data-trigger` = "click hover",
              title = tr("plot_hist_range_select_tooltip", language$language),
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
            actionButton(
              ns("add_trace"),
              tr("add_trace", language$language),
              style = "margin-right: 5px;"
            ),
            actionButton(
              ns("add_subplot"),
              tr("add_subplot", language$language),
              style = "margin-right: 10px;"
            ),
            tooltip(
              trigger = list(
                bsicons::bs_icon("info-circle-fill")
              ),
              tr("add_subplot_trace_tooltip", language$language),
              placement = "right"
            )
          ),
          # checkboxInput(ns("log_y"), "Log scale y-axis?"),
          uiOutput(ns("share_axes")),
          checkboxInput(
            ns("historic_range"),
            label = tr("plot_hist_range", language$language),
            value = TRUE
          )
        ),
        accordion(
          id = ns("accordion1"),
          open = FALSE,
          accordion_panel(
            id = ns("accordion1"),
            title = tr("plot_extra_options", language$language),
            icon = bsicons::bs_icon("gear"),
            checkboxInput(
              ns("apply_datum"),
              tr("plot_apply_vert_datum", language$language)
            ),
            checkboxInput(
              ns("plot_filter"),
              tr("plot_filter", language$language)
            ),
            checkboxInput(
              ns("unusable"),
              tr("plot_show_unusable", language$language)
            ),
            checkboxInput(
              ns("grades"),
              tr("plot_show_grades", language$language)
            ),
            checkboxInput(
              ns("approvals"),
              tr("plot_show_approvals", language$language)
            ),
            checkboxInput(
              ns("qualifiers"),
              tr("plot_show_qualifiers", language$language)
            ),
            actionButton(
              ns("extra_aes"),
              tr("modify_plot_aes", language$language),
              style = "display: block; width: 100%; margin-bottom: 10px;"
            ),
          )
        ),
        br(),
        input_task_button(
          ns("make_plot"),
          label = tr("create_plot", language$language),
          label_busy = tr("processing", language$language),
          style = "display: block; width: 100%;",
          class = "btn btn-primary"
        )
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
        plotly::plotlyOutput(
          ns("plot"),
          width = "100%",
          height = "800px",
          inline = TRUE
        ),
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
        selectizeInput(
          ns("networks"),
          label = tr("network(s)", language$language),
          choices = stats::setNames(
            c("all", moduleData$networks$network_id),
            c(
              tr("all_m", language$language),
              moduleData$networks[[tr("generic_name_col", language$language)]]
            )
          ),
          multiple = TRUE,
          selected = "all"
        ),
        selectizeInput(
          ns("projects"),
          label = tr("project(s)", language$language),
          choices = stats::setNames(
            c("all", moduleData$projects$project_id),
            c(
              tr("all_m", language$language),
              moduleData$projects[[tr("generic_name_col", language$language)]]
            )
          ),
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
      req(
        input$projects,
        input$networks,
        filteredData$locations_projects,
        filteredData$locations_networks
      )

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
      updateSelectizeInput(
        session,
        "location",
        choices = stats::setNames(
          remain_locs$location_id,
          remain_locs[[tr("generic_name_col", language$language)]]
        ),
        selected = character(0)
      )
      removeModal()
    })

    ## Show/hide the approvals, grades, qualifiers checkboxes based on the selected plot type and traces/subplots ##########################
    observe({
      req(input$plot_type, traceCount(), subplotCount())
      if (input$plot_type == "ts" && traceCount() == 1 && subplotCount() == 1) {
        shinyjs::show("grades")
        shinyjs::show("approvals")
        shinyjs::show("qualifiers")
      } else if (
        input$plot_type == "ts" && (traceCount() > 1 || subplotCount() > 1)
      ) {
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
        earliest <- filteredData$range$max_date - 366
        if (earliest < filteredData$range$min_date) {
          earliest <- filteredData$range$min_date
        }

        updateDateRangeInput(
          session,
          "date_range",
          min = as.Date(filteredData$range$min_date),
          max = as.Date(filteredData$range$max_date),
          start = earliest,
          end = as.Date(filteredData$range$max_date)
        )
      } else if (input$plot_type == "over") {
        if (!is.null(input$param) && length(input$param) == 1) {
          if (input$param %in% c(values$swe, values$snow_depth)) {
            updateDateRangeInput(
              session,
              "date_range",
              min = paste0(lubridate::year(Sys.Date()) - 1, "-01-01"),
              max = paste0(lubridate::year(Sys.Date()), "-12-31"),
              start = paste0(lubridate::year(Sys.Date()) - 1, "-09-01"),
              end = paste0(lubridate::year(Sys.Date()), "-06-01")
            )
          } else {
            updateDateRangeInput(
              session,
              "date_range",
              min = paste0(lubridate::year(Sys.Date()), "-01-01"),
              max = paste0(lubridate::year(Sys.Date()), "-12-31"),
              start = paste0(lubridate::year(Sys.Date()), "-01-01"),
              end = paste0(lubridate::year(Sys.Date()), "-12-31")
            )
          }
        } else {
          updateDateRangeInput(
            session,
            "date_range",
            min = paste0(lubridate::year(Sys.Date()), "-01-01"),
            max = paste0(lubridate::year(Sys.Date()), "-12-31"),
            start = paste0(lubridate::year(Sys.Date()), "-01-01"),
            end = paste0(lubridate::year(Sys.Date()), "-12-31")
          )
        }
      }
    })

    # Filter sidebar pieces ###################
    # Create reactiveValues objects for sub_locs and z here because their observers will not run unless there is a need. This allows filtering to proceed down the line without those observers.
    filteredData_sub_locs <- reactiveValues()
    filteredData_z <- reactiveValues()
    filteredData_media <- reactiveValues()
    filteredData_aggregation <- reactiveValues()
    filteredData_rate <- reactiveValues()
    filteredData_param <- reactiveValues()

    ### Filter based on the selected location ##########################
    observeEvent(
      input$location,
      {
        req(filteredData)

        # Filter the data based on the selected locations
        filteredData$timeseries <- moduleData$timeseries[
          moduleData$timeseries$location_id %in% input$location,
        ]

        filteredData$locs <- moduleData$locs[
          moduleData$locs$location_id == input$location,
        ]
        filteredData$sub_locs <- moduleData$sub_locs[
          moduleData$sub_locs$location_id == filteredData$locs$location_id,
        ]
        filteredData$z <- unique(filteredData$timeseries$z[
          !is.na(filteredData$timeseries$z)
        ])
        filteredData$media <- moduleData$media[
          moduleData$media$media_id %in% filteredData$timeseries$media_id,
        ]
        filteredData$aggregation_types <- moduleData$aggregation_types[
          moduleData$aggregation_types$aggregation_type_id %in%
            filteredData$timeseries$aggregation_type_id,
        ]
        filteredData$rates <- moduleData$rates[
          moduleData$rates$seconds %in% filteredData$timeseries$record_rate,
        ]

        filteredData$range <- calc_range(filteredData$timeseries)

        filteredData$params <- moduleData$params[
          moduleData$params$parameter_id %in%
            filteredData$timeseries$parameter_id,
        ]

        if (nrow(filteredData$sub_locs) > 1) {
          if (!render_flags$sub_location) {
            output$sub_loc_ui <- renderUI({
              # If there are sub-locations for the selected location, show a selectizeInput for sub-locations
              selectizeInput(
                ns("sub_location"),
                label = tr("sub_loc", language$language),
                choices = stats::setNames(
                  filteredData$sub_locs[
                    filteredData$sub_locs$location_id == input$location,
                    "sub_location_id"
                  ],
                  filteredData$sub_locs[
                    filteredData$sub_locs$location_id == input$location,
                    tr("sub_location_col", language$language)
                  ]
                ),
                multiple = TRUE,
                options = list(maxItems = 1)
              )
            })
            render_flags$sub_location <- TRUE
          } else {
            updateSelectizeInput(
              session,
              "sub_location",
              choices = stats::setNames(
                filteredData$sub_locs[
                  filteredData$sub_locs$location_id == input$location,
                  "sub_location_id"
                ],
                filteredData$sub_locs[
                  filteredData$sub_locs$location_id == input$location,
                  tr("sub_location_col", language$language)
                ]
              ),
              selected = character(0)
            )
            shinyjs::show("sub_location")
          }
        } else {
          shinyjs::hide("sub_location")
        }

        if (length(filteredData$z) > 1) {
          if (!render_flags$z) {
            output$z_ui <- renderUI({
              # If there are z values for the selected location, show a selectizeInput for z
              selectizeInput(
                ns("z"),
                label = tr("z", language$language),
                choices = filteredData$z,
                multiple = TRUE,
                options = list(maxItems = 1)
              )
            })
            render_flags$z <- TRUE
          } else {
            updateSelectizeInput(
              session,
              "z",
              choices = filteredData$z,
              selected = character(0)
            )
            shinyjs::show("z")
          }
        } else {
          shinyjs::hide("z")
        }

        # Update the media, aggregation, rate, and param selectizeInputs with what's left in filteredData.
        # If possible, keep the previous selection, otherwise if there's only one choice available, select it, else null
        tmp.choices <- stats::setNames(
          filteredData$media$media_id,
          filteredData$media[[tr("media_type_col", language$language)]]
        )
        if (!is.null(input$media)) {
          tmp.selected <- if (input$media %in% tmp.choices) {
            input$media
          } else if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        } else {
          tmp.selected <- if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        }
        updateSelectizeInput(
          session,
          "media",
          choices = tmp.choices,
          selected = tmp.selected
        )

        tmp.choices <- stats::setNames(
          filteredData$aggregation_types$aggregation_type_id,
          filteredData$aggregation_types[[tr(
            "aggregation_type_col",
            language$language
          )]]
        )
        if (!is.null(input$aggregation)) {
          tmp.selected <- if (input$aggregation %in% tmp.choices) {
            input$aggregation
          } else if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        } else {
          tmp.selected <- if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        }
        updateSelectizeInput(
          session,
          "aggregation",
          choices = tmp.choices,
          selected = tmp.selected
        )

        tmp.choices <- stats::setNames(
          filteredData$rates$seconds,
          filteredData$rates[, "period"]
        )
        if (!is.null(input$rate)) {
          tmp.selected <- if (input$rate %in% tmp.choices) {
            input$rate
          } else if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        } else {
          tmp.selected <- if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        }
        updateSelectizeInput(
          session,
          "rate",
          choices = tmp.choices,
          selected = tmp.selected
        )

        tmp.choices <- stats::setNames(
          filteredData$params$parameter_id,
          filteredData$params[[tr("param_name_col", language$language)]]
        )
        if (!is.null(input$param)) {
          tmp.selected <- if (input$param %in% tmp.choices) {
            input$param
          } else if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        } else {
          tmp.selected <- if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        }
        updateSelectizeInput(
          session,
          "param",
          choices = tmp.choices,
          selected = tmp.selected
        )

        earliest <- filteredData$range$max_date - 366
        if (earliest < filteredData$range$min_date) {
          earliest <- filteredData$range$min_date
        }

        possible_years <- seq(
          as.numeric(substr(filteredData$range$min_date, 1, 4)),
          as.numeric(substr(filteredData$range$max_date, 1, 4))
        )
        updateSelectizeInput(
          session,
          "years",
          choices = possible_years,
          selected = max(possible_years)
        )

        if (input$plot_type == "over") {
          if (!is.null(input$param) && length(input$param) == 1) {
            if (input$param %in% c(values$swe, values$snow_depth)) {
              updateDateRangeInput(
                session,
                "date_range",
                min = paste0(lubridate::year(Sys.Date()) - 1, "-01-01"),
                max = paste0(lubridate::year(Sys.Date()), "-12-31"),
                start = paste0(lubridate::year(Sys.Date()) - 1, "-09-01"),
                end = paste0(lubridate::year(Sys.Date()), "-06-01")
              )
            } else {
              updateDateRangeInput(
                session,
                "date_range",
                min = paste0(lubridate::year(Sys.Date()), "-01-01"),
                max = paste0(lubridate::year(Sys.Date()), "-12-31"),
                start = paste0(lubridate::year(Sys.Date()), "-01-01"),
                end = paste0(lubridate::year(Sys.Date()), "-12-31")
              )
            }
          } else {
            updateDateRangeInput(
              session,
              "date_range",
              min = paste0(lubridate::year(Sys.Date()), "-01-01"),
              max = paste0(lubridate::year(Sys.Date()), "-12-31"),
              start = paste0(lubridate::year(Sys.Date()), "-01-01"),
              end = paste0(lubridate::year(Sys.Date()), "-12-31")
            )
          }
        } else if (input$plot_type == "ts") {
          updateDateRangeInput(
            session,
            "date_range",
            start = earliest,
            end = as.Date(filteredData$range$max_date),
            min = as.Date(filteredData$range$min_date),
            max = as.Date(filteredData$range$max_date)
          )
        }

        filteredData_sub_locs$timeseries <- filteredData$timeseries
        filteredData_sub_locs$z <- filteredData$z
        filteredData_sub_locs$media <- filteredData$media
        filteredData_sub_locs$aggregation_types <- filteredData$aggregation_types
        filteredData_sub_locs$rates <- filteredData$rates
        filteredData_sub_locs$params <- filteredData$params

        filteredData_z$timeseries <- filteredData$timeseries
        filteredData_z$media <- filteredData$media
        filteredData_z$aggregation_types <- filteredData$aggregation_types
        filteredData_z$rates <- filteredData$rates
        filteredData_z$params <- filteredData$params

        filteredData_media$timeseries <- filteredData$timeseries
        filteredData_media$aggregation_types <- filteredData$aggregation_types
        filteredData_media$rates <- filteredData$rates
        filteredData_media$params <- filteredData$params

        filteredData_aggregation$timeseries <- filteredData$timeseries
        filteredData_aggregation$rates <- filteredData$rates
        filteredData_aggregation$params <- filteredData$params

        filteredData_rate$timeseries <- filteredData$timeseries
        filteredData_rate$params <- filteredData$params
      },
      priority = 999
    )

    ## Filter based on the sub_location ##########################
    observeEvent(
      input$sub_location,
      {
        req(filteredData)

        # Filter the data based on the selected sub-locations
        if (is.null(input$sub_location) || length(input$sub_location) != 1) {
          return()
        }

        # Find the new timeseries rows based on the selected sub-location
        filteredData_sub_locs$timeseries <- filteredData$timeseries[
          filteredData$timeseries$sub_location_id == input$sub_location,
        ]

        filteredData_sub_locs$z <- unique(filteredData_sub_locs$timeseries$z[
          !is.na(filteredData_sub_locs$timeseries$z)
        ])
        filteredData_sub_locs$media <- filteredData$media[
          filteredData$media$media_id %in%
            filteredData_sub_locs$timeseries$media_id,
        ]
        filteredData_sub_locs$aggregation_types <- filteredData$aggregation_types[
          filteredData$aggregation_types$aggregation_type_id %in%
            filteredData_sub_locs$timeseries$aggregation_type_id,
        ]
        filteredData_sub_locs$rates <- filteredData$rates[
          filteredData$rates$seconds %in%
            filteredData_sub_locs$timeseries$record_rate,
        ]
        filteredData_sub_locs$params <- filteredData$params[
          filteredData$params$parameter_id %in%
            filteredData_sub_locs$timeseries$parameter_id,
        ]

        filteredData_sub_locs$range <- calc_range(
          filteredData_sub_locs$timeseries
        )

        # Update the z selectizeInput based on the selected sub-locations
        if (length(filteredData_sub_locs$z) > 1) {
          if (!render_flags$z) {
            output$z_ui <- renderUI({
              # If there are z values for the selected location, show a selectizeInput for z
              selectizeInput(
                ns("z"),
                label = tr("z", language$language),
                choices = filteredData_sub_locs$z,
                multiple = TRUE,
                options = list(maxItems = 1)
              )
            })
            render_flags$z <- TRUE
          } else {
            updateSelectizeInput(
              session,
              "z",
              choices = filteredData_sub_locs$z,
              selected = character(0)
            )
            shinyjs::show("z")
          }
        } else {
          shinyjs::hide("z")
        }

        # Update the media, aggregation, rate, and param selectizeInputs with what's left in filteredData_sub_locs.
        # If possible, keep the previous selection, otherwise if there's only one choice available, select it, else null
        tmp.choices <- stats::setNames(
          filteredData_sub_locs$media$media_id,
          filteredData_sub_locs$media[[tr("media_type_col", language$language)]]
        )
        if (!is.null(input$media)) {
          tmp.selected <- if (input$media %in% tmp.choices) {
            input$media
          } else if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        } else {
          tmp.selected <- if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        }
        updateSelectizeInput(
          session,
          "media",
          choices = tmp.choices,
          selected = tmp.selected
        )

        tmp.choices <- stats::setNames(
          filteredData_sub_locs$aggregation_types$aggregation_type_id,
          filteredData_sub_locs$aggregation_types[[tr(
            "aggregation_type_col",
            language$language
          )]]
        )
        if (!is.null(input$aggregation)) {
          tmp.selected <- if (input$aggregation %in% tmp.choices) {
            input$aggregation
          } else if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        } else {
          tmp.selected <- if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        }
        updateSelectizeInput(
          session,
          "aggregation",
          choices = tmp.choices,
          selected = tmp.selected
        )

        tmp.choices <- stats::setNames(
          filteredData_sub_locs$rates$seconds,
          filteredData_sub_locs$rates[, "period"]
        )
        if (!is.null(input$rate)) {
          tmp.selected <- if (input$rate %in% tmp.choices) {
            input$rate
          } else if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        } else {
          tmp.selected <- if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        }
        updateSelectizeInput(
          session,
          "rate",
          choices = tmp.choices,
          selected = tmp.selected
        )

        tmp.choices <- stats::setNames(
          filteredData_sub_locs$params$parameter_id,
          filteredData_sub_locs$params[[tr(
            "param_name_col",
            language$language
          )]]
        )
        if (!is.null(input$param)) {
          tmp.selected <- if (input$param %in% tmp.choices) {
            input$param
          } else if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        } else {
          tmp.selected <- if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        }
        updateSelectizeInput(
          session,
          "param",
          choices = tmp.choices,
          selected = tmp.selected
        )

        earliest <- filteredData_sub_locs$range$max_date - 366
        if (earliest < filteredData_sub_locs$range$min_date) {
          earliest <- filteredData_sub_locs$range$min_date
        }

        possible_years <- seq(
          as.numeric(substr(filteredData_sub_locs$range$min_date, 1, 4)),
          as.numeric(substr(filteredData_sub_locs$range$max_date, 1, 4))
        )
        updateSelectizeInput(
          session,
          "years",
          choices = possible_years,
          selected = max(possible_years)
        )

        if (input$plot_type == "over") {
          if (!is.null(input$param) && length(input$param) == 1) {
            if (input$param %in% c(values$swe, values$snow_depth)) {
              updateDateRangeInput(
                session,
                "date_range",
                min = paste0(lubridate::year(Sys.Date()) - 1, "-01-01"),
                max = paste0(lubridate::year(Sys.Date()), "-12-31"),
                start = paste0(lubridate::year(Sys.Date()) - 1, "-09-01"),
                end = paste0(lubridate::year(Sys.Date()), "-06-01")
              )
            } else {
              updateDateRangeInput(
                session,
                "date_range",
                min = paste0(lubridate::year(Sys.Date()), "-01-01"),
                max = paste0(lubridate::year(Sys.Date()), "-12-31"),
                start = paste0(lubridate::year(Sys.Date()), "-01-01"),
                end = paste0(lubridate::year(Sys.Date()), "-12-31")
              )
            }
          } else {
            updateDateRangeInput(
              session,
              "date_range",
              min = paste0(lubridate::year(Sys.Date()), "-01-01"),
              max = paste0(lubridate::year(Sys.Date()), "-12-31"),
              start = paste0(lubridate::year(Sys.Date()), "-01-01"),
              end = paste0(lubridate::year(Sys.Date()), "-12-31")
            )
          }
        } else if (input$plot_type == "ts") {
          updateDateRangeInput(
            session,
            "date_range",
            start = earliest,
            end = as.Date(filteredData_sub_locs$range$max_date),
            min = as.Date(filteredData_sub_locs$range$min_date),
            max = as.Date(filteredData_sub_locs$range$max_date)
          )
        }

        filteredData_z$timeseries <- filteredData_sub_locs$timeseries
        filteredData_z$media <- filteredData_sub_locs$media
        filteredData_z$aggregation_types <- filteredData_sub_locs$aggregation_types
        filteredData_z$rates <- filteredData_sub_locs$rates
        filteredData_z$params <- filteredData_sub_locs$params

        filteredData_media$timeseries <- filteredData_sub_locs$timeseries
        filteredData_media$aggregation_types <- filteredData_sub_locs$aggregation_types
        filteredData_media$rates <- filteredData_sub_locs$rates
        filteredData_media$params <- filteredData_sub_locs$params

        filteredData_aggregation$timeseries <- filteredData_sub_locs$timeseries
        filteredData_aggregation$rates <- filteredData_sub_locs$rates
        filteredData_aggregation$params <- filteredData_sub_locs$params

        filteredData_rate$timeseries <- filteredData_sub_locs$timeseries
        filteredData_rate$params <- filteredData_sub_locs$params
      },
      priority = 998
    ) # End observeEvent for sub_location

    ## Filter based on the z value ##########################
    observeEvent(
      input$z,
      {
        # Filter the data based on the selected z values
        if (is.null(input$z) || length(input$z) != 1) {
          return()
        }

        # Find the new timeseries rows based on the selected z value
        filteredData_z$timeseries <- filteredData_sub_locs$timeseries[
          filteredData_sub_locs$timeseries$z == input$z,
        ]

        filteredData_z$media <- filteredData_sub_locs$media[
          filteredData_sub_locs$media$media_id %in%
            filteredData_z$timeseries$media_id,
        ]
        filteredData_z$aggregation_types <- filteredData_sub_locs$aggregation_types[
          filteredData_sub_locs$aggregation_types$aggregation_type_id %in%
            filteredData_z$timeseries$aggregation_type_id,
        ]
        filteredData_z$rates <- filteredData_sub_locs$rates[
          filteredData_sub_locs$rates$seconds %in%
            filteredData_z$timeseries$record_rate,
        ]
        filteredData_z$params <- filteredData_sub_locs$params[
          filteredData_sub_locs$params$parameter_id %in%
            filteredData_z$timeseries$parameter_id,
        ]
        filteredData_z$range <- calc_range(filteredData_z$timeseries)

        # Update the media, aggregation, rate, and param selectizeInputs with what's left in filteredData_z.
        # If possible, keep the previous selection, otherwise if there's only one choice available, select it, else null
        tmp.choices <- stats::setNames(
          filteredData_z$media$media_id,
          filteredData_z$media[[tr("media_type_col", language$language)]]
        )
        if (!is.null(input$media)) {
          tmp.selected <- if (input$media %in% tmp.choices) {
            input$media
          } else if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        } else {
          tmp.selected <- if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        }
        updateSelectizeInput(
          session,
          "media",
          choices = tmp.choices,
          selected = tmp.selected
        )

        tmp.choices <- stats::setNames(
          filteredData_z$aggregation_types$aggregation_type_id,
          filteredData_z$aggregation_types[[tr(
            "aggregation_type_col",
            language$language
          )]]
        )
        if (!is.null(input$aggregation)) {
          tmp.selected <- if (input$aggregation %in% tmp.choices) {
            input$aggregation
          } else if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        } else {
          tmp.selected <- if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        }
        updateSelectizeInput(
          session,
          "aggregation",
          choices = tmp.choices,
          selected = tmp.selected
        )

        tmp.choices <- stats::setNames(
          filteredData_z$rates$seconds,
          filteredData_z$rates[, "period"]
        )
        if (!is.null(input$rate)) {
          tmp.selected <- if (input$rate %in% tmp.choices) {
            input$rate
          } else if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        } else {
          tmp.selected <- if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        }
        updateSelectizeInput(
          session,
          "rate",
          choices = tmp.choices,
          selected = tmp.selected
        )

        tmp.choices <- stats::setNames(
          filteredData_z$params$parameter_id,
          filteredData_z$params[[tr("param_name_col", language$language)]]
        )
        if (!is.null(input$param)) {
          tmp.selected <- if (input$param %in% tmp.choices) {
            input$param
          } else if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        } else {
          tmp.selected <- if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        }
        updateSelectizeInput(
          session,
          "param",
          choices = tmp.choices,
          selected = tmp.selected
        )

        earliest <- filteredData_z$range$max_date - 366
        if (earliest < filteredData_z$range$min_date) {
          earliest <- filteredData_z$range$min_date
        }

        possible_years <- seq(
          as.numeric(substr(filteredData_z$range$min_date, 1, 4)),
          as.numeric(substr(filteredData_z$range$max_date, 1, 4))
        )
        updateSelectizeInput(
          session,
          "years",
          choices = possible_years,
          selected = max(possible_years)
        )

        if (input$plot_type == "over") {
          if (input$param %in% c(values$swe, values$snow_depth)) {
            updateDateRangeInput(
              session,
              "date_range",
              min = paste0(lubridate::year(Sys.Date()) - 1, "-01-01"),
              max = paste0(lubridate::year(Sys.Date()), "-12-31"),
              start = paste0(lubridate::year(Sys.Date()) - 1, "-09-01"),
              end = paste0(lubridate::year(Sys.Date()), "-06-01")
            )
          } else {
            updateDateRangeInput(
              session,
              "date_range",
              min = paste0(lubridate::year(Sys.Date()), "-01-01"),
              max = paste0(lubridate::year(Sys.Date()), "-12-31"),
              start = paste0(lubridate::year(Sys.Date()), "-01-01"),
              end = paste0(lubridate::year(Sys.Date()), "-12-31")
            )
          }
        } else if (input$plot_type == "ts") {
          updateDateRangeInput(
            session,
            "date_range",
            start = earliest,
            end = as.Date(filteredData_z$range$max_date),
            min = as.Date(filteredData_z$range$min_date),
            max = as.Date(filteredData_z$range$max_date)
          )
        }

        filteredData_media$timeseries <- filteredData_z$timeseries
        filteredData_media$aggregation_types <- filteredData_z$aggregation_types
        filteredData_media$rates <- filteredData_z$rates
        filteredData_media$params <- filteredData_z$params

        filteredData_aggregation$timeseries <- filteredData_z$timeseries
        filteredData_aggregation$rates <- filteredData_z$rates
        filteredData_aggregation$params <- filteredData_z$params

        filteredData_rate$timeseries <- filteredData_z$timeseries
        filteredData_rate$params <- filteredData_z$params
      },
      priority = 997
    ) # End observeEvent for z

    ## Filter based on the media ##########################
    observeEvent(
      input$media,
      {
        # Filter the data based on the selected media
        if (is.null(input$media) || length(input$media) != 1) {
          return()
        }

        # Find the new timeseries rows based on the selected media
        filteredData_media$timeseries <- filteredData_z$timeseries[
          filteredData_z$timeseries$media_id == input$media,
        ]

        filteredData_media$aggregation_types <- filteredData_z$aggregation_types[
          filteredData_z$aggregation_types$aggregation_type_id %in%
            filteredData_media$timeseries$aggregation_type_id,
        ]
        filteredData_media$rates <- filteredData_z$rates[
          filteredData_z$rates$seconds %in%
            filteredData_media$timeseries$record_rate,
        ]
        filteredData_media$params <- filteredData_z$params[
          filteredData_z$params$parameter_id %in%
            filteredData_media$timeseries$parameter_id,
        ]
        filteredData_media$range <- calc_range(filteredData_media$timeseries)

        # Update the aggregation, rate, and param selectizeInputs with what's left in filteredData_media.
        # If possible, keep the previous selection, otherwise if there's only one choice available, select it, else null
        tmp.choices <- stats::setNames(
          filteredData_media$aggregation_types$aggregation_type_id,
          filteredData_media$aggregation_types[[tr(
            "aggregation_type_col",
            language$language
          )]]
        )
        if (!is.null(input$aggregation)) {
          tmp.selected <- if (input$aggregation %in% tmp.choices) {
            input$aggregation
          } else if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        } else {
          tmp.selected <- if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        }
        updateSelectizeInput(
          session,
          "aggregation",
          choices = tmp.choices,
          selected = tmp.selected
        )

        tmp.choices <- stats::setNames(
          filteredData_media$rates$seconds,
          filteredData_media$rates[, "period"]
        )
        if (!is.null(input$rate)) {
          tmp.selected <- if (input$rate %in% tmp.choices) {
            input$rate
          } else if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        } else {
          tmp.selected <- if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        }
        updateSelectizeInput(
          session,
          "rate",
          choices = tmp.choices,
          selected = tmp.selected
        )

        tmp.choices <- stats::setNames(
          filteredData_media$params$parameter_id,
          filteredData_media$params[[tr("param_name_col", language$language)]]
        )
        if (!is.null(input$param)) {
          tmp.selected <- if (input$param %in% tmp.choices) {
            input$param
          } else if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        } else {
          tmp.selected <- if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        }
        updateSelectizeInput(
          session,
          "param",
          choices = tmp.choices,
          selected = tmp.selected
        )

        earliest <- filteredData_media$range$max_date - 366
        if (earliest < filteredData_media$range$min_date) {
          earliest <- filteredData_media$range$min_date
        }

        possible_years <- seq(
          as.numeric(substr(filteredData_media$range$min_date, 1, 4)),
          as.numeric(substr(filteredData_media$range$max_date, 1, 4))
        )
        updateSelectizeInput(
          session,
          "years",
          choices = possible_years,
          selected = max(possible_years)
        )

        if (input$plot_type == "over") {
          if (!is.null(input$param) && length(input$param) == 1) {
            if (input$param %in% c(values$swe, values$snow_depth)) {
              updateDateRangeInput(
                session,
                "date_range",
                min = paste0(lubridate::year(Sys.Date()) - 1, "-01-01"),
                max = paste0(lubridate::year(Sys.Date()), "-12-31"),
                start = paste0(lubridate::year(Sys.Date()) - 1, "-09-01"),
                end = paste0(lubridate::year(Sys.Date()), "-06-01")
              )
            } else {
              updateDateRangeInput(
                session,
                "date_range",
                min = paste0(lubridate::year(Sys.Date()), "-01-01"),
                max = paste0(lubridate::year(Sys.Date()), "-12-31"),
                start = paste0(lubridate::year(Sys.Date()), "-01-01"),
                end = paste0(lubridate::year(Sys.Date()), "-12-31")
              )
            }
          } else {
            updateDateRangeInput(
              session,
              "date_range",
              min = paste0(lubridate::year(Sys.Date()), "-01-01"),
              max = paste0(lubridate::year(Sys.Date()), "-12-31"),
              start = paste0(lubridate::year(Sys.Date()), "-01-01"),
              end = paste0(lubridate::year(Sys.Date()), "-12-31")
            )
          }
        } else if (input$plot_type == "ts") {
          updateDateRangeInput(
            session,
            "date_range",
            start = earliest,
            end = as.Date(filteredData_media$range$max_date),
            min = as.Date(filteredData_media$range$min_date),
            max = as.Date(filteredData_media$range$max_date)
          )
        }

        filteredData_aggregation$timeseries <- filteredData_media$timeseries
        filteredData_aggregation$rates <- filteredData_media$rates
        filteredData_aggregation$params <- filteredData_media$params

        filteredData_rate$timeseries <- filteredData_media$timeseries
        filteredData_rate$params <- filteredData_media$params
      },
      priority = 996
    ) # End observeEvent for media

    ## Filter based on the aggregation type ##########################
    observeEvent(
      input$aggregation,
      {
        # Filter the data based on the selected aggregation type
        if (is.null(input$aggregation) || length(input$aggregation) != 1) {
          return()
        }

        # Find the new timeseries rows based on the selected aggregation type
        filteredData_aggregation$timeseries <- filteredData_media$timeseries[
          filteredData_media$timeseries$aggregation_type_id ==
            input$aggregation,
        ]

        filteredData_aggregation$rates <- filteredData_media$rates[
          filteredData_media$rates$seconds %in%
            filteredData_aggregation$timeseries$record_rate,
        ]
        filteredData_aggregation$params <- filteredData_media$params[
          filteredData_media$params$parameter_id %in%
            filteredData_aggregation$timeseries$parameter_id,
        ]
        filteredData_aggregation$range <- calc_range(
          filteredData_aggregation$timeseries
        )

        # Update the rate and param selectizeInputs with what's left in filteredData_aggregation.
        # If possible, keep the previous selection, otherwise if there's only one choice available, select it, else null
        tmp.choices <- stats::setNames(
          filteredData_aggregation$rates$seconds,
          filteredData_aggregation$rates[, "period"]
        )
        if (!is.null(input$rate)) {
          tmp.selected <- if (input$rate %in% tmp.choices) {
            input$rate
          } else if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        } else {
          tmp.selected <- if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        }
        updateSelectizeInput(
          session,
          "rate",
          choices = tmp.choices,
          selected = tmp.selected
        )

        tmp.choices <- stats::setNames(
          filteredData_aggregation$params$parameter_id,
          filteredData_aggregation$params[[tr(
            "param_name_col",
            language$language
          )]]
        )
        if (!is.null(input$param)) {
          tmp.selected <- if (input$param %in% tmp.choices) {
            input$param
          } else if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        } else {
          tmp.selected <- if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        }
        updateSelectizeInput(
          session,
          "param",
          choices = tmp.choices,
          selected = tmp.selected
        )

        earliest <- filteredData_aggregation$range$max_date - 366
        if (earliest < filteredData_aggregation$range$min_date) {
          earliest <- filteredData_aggregation$range$min_date
        }

        possible_years <- seq(
          as.numeric(substr(filteredData_aggregation$range$min_date, 1, 4)),
          as.numeric(substr(filteredData_aggregation$range$max_date, 1, 4))
        )
        updateSelectizeInput(
          session,
          "years",
          choices = possible_years,
          selected = max(possible_years)
        )

        if (input$plot_type == "over") {
          if (!is.null(input$param) && length(input$param) == 1) {
            if (input$param %in% c(values$swe, values$snow_depth)) {
              updateDateRangeInput(
                session,
                "date_range",
                min = paste0(lubridate::year(Sys.Date()) - 1, "-01-01"),
                max = paste0(lubridate::year(Sys.Date()), "-12-31"),
                start = paste0(lubridate::year(Sys.Date()) - 1, "-09-01"),
                end = paste0(lubridate::year(Sys.Date()), "-06-01")
              )
            } else {
              updateDateRangeInput(
                session,
                "date_range",
                min = paste0(lubridate::year(Sys.Date()), "-01-01"),
                max = paste0(lubridate::year(Sys.Date()), "-12-31"),
                start = paste0(lubridate::year(Sys.Date()), "-01-01"),
                end = paste0(lubridate::year(Sys.Date()), "-12-31")
              )
            }
          } else {
            updateDateRangeInput(
              session,
              "date_range",
              min = paste0(lubridate::year(Sys.Date()), "-01-01"),
              max = paste0(lubridate::year(Sys.Date()), "-12-31"),
              start = paste0(lubridate::year(Sys.Date()), "-01-01"),
              end = paste0(lubridate::year(Sys.Date()), "-12-31")
            )
          }
        } else if (input$plot_type == "ts") {
          updateDateRangeInput(
            session,
            "date_range",
            start = earliest,
            end = as.Date(filteredData_aggregation$range$max_date),
            min = as.Date(filteredData_aggregation$range$min_date),
            max = as.Date(filteredData_aggregation$range$max_date)
          )
        }

        filteredData_rate$timeseries <- filteredData_aggregation$timeseries
        filteredData_rate$params <- filteredData_aggregation$params
      },
      priority = 995
    ) # End observeEvent for aggregation

    ## Filter based on the rate ##########################
    observeEvent(
      input$rate,
      {
        # Filter the data based on the selected rate
        if (is.null(input$rate) || length(input$rate) != 1) {
          return()
        }

        # Find the new timeseries rows based on the selected rate
        filteredData_rate$timeseries <- filteredData_aggregation$timeseries[
          filteredData_aggregation$timeseries$record_rate == input$rate,
        ]

        filteredData_rate$params <- filteredData_aggregation$params[
          filteredData_aggregation$params$parameter_id %in%
            filteredData_rate$timeseries$parameter_id,
        ]
        filteredData_rate$range <- calc_range(filteredData_rate$timeseries)

        # Update the param selectizeInput with what's left in filteredData_rate.
        # If possible, keep the previous selection, otherwise if there's only one choice available, select it, else null
        tmp.choices <- stats::setNames(
          filteredData_rate$params$parameter_id,
          filteredData_rate$params[[tr("param_name_col", language$language)]]
        )
        if (!is.null(input$param)) {
          tmp.selected <- if (input$param %in% tmp.choices) {
            input$param
          } else if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        } else {
          tmp.selected <- if (length(tmp.choices) == 1) {
            tmp.choices[1]
          } else {
            character(0)
          }
        }
        updateSelectizeInput(
          session,
          "param",
          choices = tmp.choices,
          selected = tmp.selected
        )

        earliest <- filteredData_rate$range$max_date - 366
        if (earliest < filteredData_rate$range$min_date) {
          earliest <- filteredData_rate$range$min_date
        }

        possible_years <- seq(
          as.numeric(substr(filteredData_rate$range$min_date, 1, 4)),
          as.numeric(substr(filteredData_rate$range$max_date, 1, 4))
        )
        updateSelectizeInput(
          session,
          "years",
          choices = possible_years,
          selected = max(possible_years)
        )

        if (input$plot_type == "over") {
          if (!is.null(input$param) && length(input$param) == 1) {
            if (input$param %in% c(values$swe, values$snow_depth)) {
              updateDateRangeInput(
                session,
                "date_range",
                min = paste0(lubridate::year(Sys.Date()) - 1, "-01-01"),
                max = paste0(lubridate::year(Sys.Date()), "-12-31"),
                start = paste0(lubridate::year(Sys.Date()) - 1, "-09-01"),
                end = paste0(lubridate::year(Sys.Date()), "-06-01")
              )
            } else {}
          } else {
            updateDateRangeInput(
              session,
              "date_range",
              min = paste0(lubridate::year(Sys.Date()), "-01-01"),
              max = paste0(lubridate::year(Sys.Date()), "-12-31"),
              start = paste0(lubridate::year(Sys.Date()), "-01-01"),
              end = paste0(lubridate::year(Sys.Date()), "-12-31")
            )
          }
        } else if (input$plot_type == "ts") {
          updateDateRangeInput(
            session,
            "date_range",
            start = earliest,
            end = as.Date(filteredData_rate$range$max_date),
            min = as.Date(filteredData_rate$range$min_date),
            max = as.Date(filteredData_rate$range$max_date)
          )
        }
      },
      priority = 994
    ) # End observeEvent for rate

    ## Filter based on the parameter ##########################
    # only need to update the date range and years inputs
    observeEvent(
      input$param,
      {
        req(filteredData_rate)

        # Filter the data based on the selected parameter
        if (is.null(input$param) || length(input$param) != 1) {
          return()
        }

        # Find the single timeseries rows based on the selected parameter
        tmp.timeseries <- filteredData_rate$timeseries[
          filteredData_rate$timeseries$parameter_id == input$param,
        ]

        tmp.range <- calc_range(tmp.timeseries)

        earliest <- tmp.range$max_date - 366
        if (earliest < tmp.range$min_date) {
          earliest <- tmp.range$min_date
        }

        possible_years <- seq(
          as.numeric(substr(tmp.range$min_date, 1, 4)),
          as.numeric(substr(tmp.range$max_date, 1, 4))
        )
        updateSelectizeInput(
          session,
          "years",
          choices = possible_years,
          selected = max(possible_years)
        )

        if (input$plot_type == "over") {
          if (!is.null(input$param) && length(input$param) == 1) {
            if (input$param %in% c(values$swe, values$snow_depth)) {
              updateDateRangeInput(
                session,
                "date_range",
                min = paste0(lubridate::year(Sys.Date()) - 1, "-01-01"),
                max = paste0(lubridate::year(Sys.Date()), "-12-31"),
                start = paste0(lubridate::year(Sys.Date()) - 1, "-09-01"),
                end = paste0(lubridate::year(Sys.Date()), "-06-01")
              )
            } else {
              updateDateRangeInput(
                session,
                "date_range",
                min = paste0(lubridate::year(Sys.Date()), "-01-01"),
                max = paste0(lubridate::year(Sys.Date()), "-12-31"),
                start = paste0(lubridate::year(Sys.Date()), "-01-01"),
                end = paste0(lubridate::year(Sys.Date()), "-12-31")
              )
            }
          } else {
            updateDateRangeInput(
              session,
              "date_range",
              min = paste0(lubridate::year(Sys.Date()), "-01-01"),
              max = paste0(lubridate::year(Sys.Date()), "-12-31"),
              start = paste0(lubridate::year(Sys.Date()), "-01-01"),
              end = paste0(lubridate::year(Sys.Date()), "-12-31")
            )
          }
        } else if (input$plot_type == "ts") {
          updateDateRangeInput(
            session,
            "date_range",
            start = earliest,
            end = as.Date(tmp.range$max_date),
            min = as.Date(tmp.range$min_date),
            max = as.Date(tmp.range$max_date)
          )
        }
      },
      priority = 993
    ) # End observeEvent for param

    # Modal dialog for extra aesthetics ########################################################################
    # Create a list with default aesthetic values
    plot_aes <- reactiveValues(
      lang = "en",
      showgridx = FALSE,
      showgridy = FALSE,
      line_scale = 1,
      axis_scale = 1,
      legend_scale = 1
    )

    observeEvent(input$extra_aes, {
      showModal(modalDialog(
        title = tr("modify_plot_aes", language$language),
        tags$div(
          tags$h5(tr("language", language$language)),
          radioButtons(
            ns("lang"),
            NULL,
            choices = stats::setNames(
              c("en", "fr"),
              c(
                tr("english", language$language),
                tr("francais", language$language)
              )
            ),
            selected = plot_aes$lang
          ),
          checkboxInput(
            ns("showgridx"),
            tr("show_x_grid", language$language),
            value = plot_aes$showgridx
          ),
          checkboxInput(
            ns("showgridy"),
            tr("show_y_grid", language$language),
            value = plot_aes$showgridy
          ),
          sliderInput(
            ns("line_scale"),
            tr("line_scale", language$language),
            min = 0.2,
            max = 3,
            value = plot_aes$line_scale,
            step = 0.1
          ),
          sliderInput(
            ns("axis_scale"),
            tr("axis_scale", language$language),
            min = 0.2,
            max = 3,
            value = plot_aes$axis_scale,
            step = 0.1
          ),
          sliderInput(
            ns("legend_scale"),
            tr("legend_scale", language$language),
            min = 0.2,
            max = 3,
            value = plot_aes$legend_scale,
            step = 0.1
          )
        ),
        easyClose = FALSE,
        footer = tagList(
          actionButton(ns("aes_apply"), tr("apply", language$language)),
          actionButton(ns("aes_cancel"), tr("cancel", language$language))
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

    # Add/remove/modify trace and subplots #######################################################################
    ## Common pieces for traces and subplots ########################################################
    traces <- reactiveValues()
    traceCount <- reactiveVal(1)
    subplots <- reactiveValues()
    subplotCount <- reactiveVal(1)

    # Helper to determine the available date range for a given timeseries selection
    ts_range <- function(sel) {
      df <- moduleData$timeseries[
        moduleData$timeseries$location_id == sel$location_id &
          moduleData$timeseries$parameter_id == sel$parameter &
          moduleData$timeseries$media_id == sel$media &
          moduleData$timeseries$aggregation_type_id == sel$aggregation &
          moduleData$timeseries$record_rate == sel$rate,
      ]
      if (!is.null(sel$sub_location_id) && !is.na(sel$sub_location_id)) {
        df <- df[df$sub_location_id == sel$sub_location_id, ]
      }
      if (!is.null(sel$z) && !is.na(sel$z)) {
        df <- df[df$z == sel$z, ]
      }
      calc_range(df)
    }

    # Update the date range input based on all selected traces and subplots
    update_date_range <- function() {
      ranges <- data.frame(
        min_date = as.Date(character()),
        max_date = as.Date(character())
      )
      if (length(names(traces)) > 0) {
        for (nm in names(traces)) {
          ranges <- rbind(ranges, ts_range(traces[[nm]]))
        }
      }
      if (length(names(subplots)) > 0) {
        for (nm in names(subplots)) {
          ranges <- rbind(ranges, ts_range(subplots[[nm]]))
        }
      }
      if (
        nrow(ranges) == 0 ||
          all(is.na(ranges$min_date)) ||
          all(is.na(ranges$max_date))
      ) {
        return()
      }
      min_all <- min(ranges$min_date, na.rm = TRUE)
      max_all <- max(ranges$max_date, na.rm = TRUE)
      start <- as.Date(input$date_range[1])
      end <- as.Date(input$date_range[2])
      if (is.na(start) || start < min_all) {
        start <- min_all
      }
      if (is.na(end) || end > max_all) {
        end <- max_all
      }
      updateDateRangeInput(
        session,
        "date_range",
        min = min_all,
        max = max_all,
        start = start,
        end = end
      )
    }

    # Function to create modals for adding new traces or subplots; one function for both, the only thing different in the produced modal is the add_new button
    trace_subplot_modal <- function(type) {
      showModal(modalDialog(
        selectizeInput(
          ns("traceNew_location"),
          label = tr("loc", language$language),
          choices = stats::setNames(
            moduleData$locs$location_id,
            moduleData$locs[[tr("generic_name_col", language$language)]]
          ),
          multiple = TRUE,
          options = list(maxItems = 1),
          selected = input$location
        ),
        uiOutput(ns("traceNew_sub_loc_ui")), # Will be a selectizeInput for sub-locations, shows up only if needed

        uiOutput(ns("traceNew_z_ui")), # Will be a selectizeInput for depth/height, shows up only if needed

        # Selectize input for media type
        selectizeInput(
          ns("traceNew_media"),
          label = tr("media_type", language$language),
          choices = stats::setNames(
            moduleData$media$media_id,
            moduleData$media[[tr("media_type_col", language$language)]]
          ),
          multiple = TRUE,
          options = list(maxItems = 1)
        ),
        # Selectize input for aggregation types
        selectizeInput(
          ns("traceNew_aggregation"),
          label = tr("aggregation_type", language$language),
          choices = stats::setNames(
            moduleData$aggregation_types$aggregation_type_id,
            moduleData$aggregation_types[[tr(
              "aggregation_type_col",
              language$language
            )]]
          ),
          multiple = TRUE,
          options = list(maxItems = 1)
        ),
        # Selectize input for record rate
        selectizeInput(
          ns("traceNew_rate"),
          label = tr("nominal_rate", language$language),
          choices = stats::setNames(
            moduleData$rates$seconds,
            moduleData$rates[, "period"]
          ),
          multiple = TRUE,
          options = list(maxItems = 1)
        ),
        selectizeInput(
          ns("traceNew_param"),
          label = tr("parameter", language$language),
          choices = stats::setNames(
            moduleData$params$parameter_id,
            moduleData$params[[tr("param_name_col", language$language)]]
          ),
          selected = character(0)
        ),
        numericInput(
          ns("traceNew_lead_lag"),
          tr("lead_lag", language$language),
          value = 0
        ),
        footer = tagList(
          actionButton(
            ns(paste0("add_new_", type)),
            tr(
              if (type == "trace") {
                "add_trace"
              } else if (type == "subplot") {
                "add_subplot"
              },
              language$language
            )
          ),
          actionButton(ns("cancel"), tr("cancel", language$language))
        ),
        easyClose = TRUE
      ))
    }

    ## Add extra trace #########
    observeEvent(input$add_trace, {
      # When the button is clicked, a modal will appear with the necessary fields to add a trace. The trace values are then displayed to the user under button 'trace_x'
      # Make sure that there is a single selected input$location, input$media, input$aggregation, input$rate, input$param before running the modal; give the user an informative modal if not
      if (
        is.null(input$location) ||
          is.null(input$media) ||
          is.null(input$aggregation) ||
          is.null(input$rate) ||
          is.null(input$param)
      ) {
        showModal(modalDialog(
          tr("modal_select_multi_not_null", language$language),
          footer = tagList(
            actionButton(ns("cancel"), tr("cancel", language$language))
          ),
          easyClose = TRUE
        ))
        return()
      }
      if (
        nchar(input$location) == 0 ||
          nchar(input$media) == 0 ||
          nchar(input$aggregation) == 0 ||
          nchar(input$rate) == 0 ||
          nchar(input$param) == 0
      ) {
        showModal(modalDialog(
          tr("modal_select_multi_not_null", language$language),
          footer = tagList(
            actionButton(ns("cancel"), tr("cancel", language$language))
          ),
          easyClose = TRUE
        ))
        return()
      }

      # Show the modal dialog for adding a new trace
      trace_subplot_modal(type = "trace")
    })
    observeEvent(input$cancel, {
      removeModal()
    })

    ## Add extra subplot #########
    observeEvent(input$add_subplot, {
      # When the button is clicked, a modal will appear with the necessary fields to add a subplot. The subplot values are then displayed to the user under button 'subplot_x'

      # Make sure that there is a single selected input$location, input$media, input$aggregation, input$rate, input$param before running the modal; give the user an informative modal if not
      if (
        is.null(input$location) ||
          is.null(input$media) ||
          is.null(input$aggregation) ||
          is.null(input$rate) ||
          is.null(input$param)
      ) {
        showModal(modalDialog(
          tr("modal_select_multi_not_null", language$language),
          footer = tagList(
            actionButton(ns("cancel"), tr("cancel", language$language))
          ),
          easyClose = TRUE
        ))
        return()
      }
      if (
        nchar(input$location) == 0 ||
          nchar(input$media) == 0 ||
          nchar(input$aggregation) == 0 ||
          nchar(input$rate) == 0 ||
          nchar(input$param) == 0
      ) {
        showModal(modalDialog(
          tr("modal_select_multi_not_null", language$language),
          footer = tagList(
            actionButton(ns("cancel"), tr("cancel", language$language))
          ),
          easyClose = TRUE
        ))
        return()
      }
      # Create the modal for adding a subplot
      trace_subplot_modal(type = "subplot")
    })

    # Filter modal pieces ######################################################################
    filteredDataModal <- reactiveValues()
    filteredDataModal_sub_locs <- reactiveValues()
    filteredDataModal_z <- reactiveValues()
    filteredDataModal_media <- reactiveValues()
    filteredDataModal_aggregation <- reactiveValues()
    filteredDataModal_rate <- reactiveValues()
    filteredDataModal_param <- reactiveValues()

    ### Filter based on the selected location #################################################################
    observeEvent(input$traceNew_location, {
      req(filteredDataModal)

      # Filter the data based on the selected locations
      filteredDataModal$timeseries <- moduleData$timeseries[
        moduleData$timeseries$location_id %in% input$traceNew_location,
      ]

      filteredDataModal$locs <- moduleData$locs[
        moduleData$locs$location_id == input$traceNew_location,
      ]
      filteredDataModal$sub_locs <- moduleData$sub_locs[
        moduleData$sub_locs$location_id == filteredDataModal$locs$location_id,
      ]
      filteredDataModal$z <- unique(filteredDataModal$timeseries$z[
        !is.na(filteredDataModal$timeseries$z)
      ])
      filteredDataModal$media <- moduleData$media[
        moduleData$media$media_id %in% filteredDataModal$timeseries$media_id,
      ]
      filteredDataModal$aggregation_types <- moduleData$aggregation_types[
        moduleData$aggregation_types$aggregation_type_id %in%
          filteredDataModal$timeseries$aggregation_type_id,
      ]
      filteredDataModal$rates <- moduleData$rates[
        moduleData$rates$seconds %in% filteredDataModal$timeseries$record_rate,
      ]

      filteredDataModal$params <- moduleData$params[
        moduleData$params$parameter_id %in%
          filteredDataModal$timeseries$parameter_id,
      ]

      if (nrow(filteredDataModal$sub_locs) > 1) {
        output$traceNew_sub_loc_ui <- renderUI({
          # If there are sub-locations for the selected location, show a selectizeInput for sub-locations
          selectizeInput(
            ns("traceNew_sub_location"),
            label = tr("sub_loc", language$language),
            choices = stats::setNames(
              filteredDataModal$sub_locs[
                filteredDataModal$sub_locs$location_id ==
                  input$traceNew_location,
                "sub_location_id"
              ],
              filteredDataModal$sub_locs[
                filteredDataModal$sub_locs$location_id ==
                  input$traceNew_location,
                tr("sub_location_col", language$language)
              ]
            ),
            multiple = TRUE,
            options = list(maxItems = 1)
          )
        })
      } else {
        output$traceNew_sub_loc_ui <- NULL
      }

      if (length(filteredDataModal$z) > 1) {
        output$traceNew_z_ui <- renderUI({
          # If there are z values for the selected location, show a selectizeInput for z
          selectizeInput(
            ns("traceNew_z"),
            label = tr("z", language$language),
            choices = filteredDataModal$z,
            multiple = TRUE,
            options = list(maxItems = 1)
          )
        })
      } else {
        output$traceNew_z_ui <- NULL
      }

      # Update the media, aggregation, rate, and param selectizeInputs with what's left in filteredDataModal.
      # If possible, keep the previous selection, otherwise if there's only one choice available, select it, else null
      tmp.choices <- stats::setNames(
        filteredDataModal$media$media_id,
        filteredDataModal$media[[tr("media_type_col", language$language)]]
      )
      if (!is.null(input$traceNew_media)) {
        tmp.selected <- if (input$traceNew_media %in% tmp.choices) {
          input$traceNew_media
        } else if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      }
      updateSelectizeInput(
        session,
        "traceNew_media",
        choices = tmp.choices,
        selected = tmp.selected
      )

      tmp.choices <- stats::setNames(
        filteredDataModal$aggregation_types$aggregation_type_id,
        filteredDataModal$aggregation_types[[tr(
          "aggregation_type_col",
          language$language
        )]]
      )
      if (!is.null(input$traceNew_aggregation)) {
        tmp.selected <- if (input$traceNew_aggregation %in% tmp.choices) {
          input$traceNew_aggregation
        } else if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      }
      updateSelectizeInput(
        session,
        "traceNew_aggregation",
        choices = tmp.choices,
        selected = tmp.selected
      )

      tmp.choices <- stats::setNames(
        filteredDataModal$rates$seconds,
        filteredDataModal$rates[, "period"]
      )
      if (!is.null(input$traceNew_rate)) {
        tmp.selected <- if (input$traceNew_rate %in% tmp.choices) {
          input$traceNew_rate
        } else if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      }
      updateSelectizeInput(
        session,
        "traceNew_rate",
        choices = tmp.choices,
        selected = tmp.selected
      )

      tmp.choices <- stats::setNames(
        filteredDataModal$params$parameter_id,
        filteredDataModal$params[[tr("param_name_col", language$language)]]
      )
      if (!is.null(input$traceNew_param)) {
        tmp.selected <- if (input$traceNew_param %in% tmp.choices) {
          input$traceNew_param
        } else if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      }
      updateSelectizeInput(
        session,
        "traceNew_param",
        choices = tmp.choices,
        selected = tmp.selected
      )

      # Unlike the sidebar inputs, the date range and years inputs are not present

      filteredDataModal_sub_locs$timeseries <- filteredDataModal$timeseries
      filteredDataModal_sub_locs$z <- filteredDataModal$z
      filteredDataModal_sub_locs$media <- filteredDataModal$media
      filteredDataModal_sub_locs$aggregation_types <- filteredDataModal$aggregation_types
      filteredDataModal_sub_locs$rates <- filteredDataModal$rates
      filteredDataModal_sub_locs$params <- filteredDataModal$params

      filteredDataModal_z$timeseries <- filteredDataModal$timeseries
      filteredDataModal_z$media <- filteredDataModal$media
      filteredDataModal_z$aggregation_types <- filteredDataModal$aggregation_types
      filteredDataModal_z$rates <- filteredDataModal$rates
      filteredDataModal_z$params <- filteredDataModal$params

      filteredDataModal_media$timeseries <- filteredDataModal$timeseries
      filteredDataModal_media$aggregation_types <- filteredDataModal$aggregation_types
      filteredDataModal_media$rates <- filteredDataModal$rates
      filteredDataModal_media$params <- filteredDataModal$params

      filteredDataModal_aggregation$timeseries <- filteredDataModal$timeseries
      filteredDataModal_aggregation$rates <- filteredDataModal$rates
      filteredDataModal_aggregation$params <- filteredDataModal$params

      filteredDataModal_rate$timeseries <- filteredDataModal$timeseries
      filteredDataModal_rate$params <- filteredDataModal$params
    })

    ### Filter based on the selected sub-location ##############################################################
    observeEvent(input$traceNew_sub_location, {
      req(filteredDataModal_sub_locs)

      # Filter the data based on the selected sub-locations
      if (
        is.null(input$traceNew_sub_location) ||
          length(input$traceNew_sub_location) != 1
      ) {
        return()
      }

      # Find the new timeseries rows based on the selected sub-location
      filteredDataModal_sub_locs$timeseries <- filteredDataModal$timeseries[
        filteredDataModal$timeseries$sub_location_id ==
          input$traceNew_sub_location,
      ]

      filteredDataModal_sub_locs$z <- unique(filteredDataModal_sub_locs$timeseries$z[
        !is.na(filteredDataModal_sub_locs$timeseries$z)
      ])
      filteredDataModal_sub_locs$media <- filteredDataModal$media[
        filteredDataModal$media$media_id %in%
          filteredDataModal_sub_locs$timeseries$media_id,
      ]
      filteredDataModal_sub_locs$aggregation_types <- filteredDataModal$aggregation_types[
        filteredDataModal$aggregation_types$aggregation_type_id %in%
          filteredDataModal_sub_locs$timeseries$aggregation_type_id,
      ]
      filteredDataModal_sub_locs$rates <- filteredDataModal$rates[
        filteredDataModal$rates$seconds %in%
          filteredDataModal_sub_locs$timeseries$record_rate,
      ]
      filteredDataModal_sub_locs$params <- filteredDataModal$params[
        filteredDataModal$params$parameter_id %in%
          filteredDataModal_sub_locs$timeseries$parameter_id,
      ]

      # Update the z selectizeInput based on the selected sub-locations
      if (length(filteredDataModal_sub_locs$z) > 1) {
        output$traceNew_z_ui <- renderUI({
          # If there are z values for the selected location, show a selectizeInput for z
          selectizeInput(
            ns("traceNew_z"),
            label = tr("z", language$language),
            choices = filteredDataModal_sub_locs$z,
            multiple = TRUE,
            options = list(maxItems = 1)
          )
        })
      } else {
        output$traceNew_z_ui <- NULL
      }

      # Update the media, aggregation, rate, and param selectizeInputs with what's left in filteredDataModal_sub_locs.
      # If possible, keep the previous selection, otherwise if there's only one choice available, select it, else null
      tmp.choices <- stats::setNames(
        filteredDataModal_sub_locs$media$media_id,
        filteredDataModal_sub_locs$media[[tr(
          "media_type_col",
          language$language
        )]]
      )
      if (!is.null(input$traceNew_media)) {
        tmp.selected <- if (input$traceNew_media %in% tmp.choices) {
          input$traceNew_media
        } else if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      }
      updateSelectizeInput(
        session,
        "traceNew_media",
        choices = tmp.choices,
        selected = tmp.selected
      )

      tmp.choices <- stats::setNames(
        filteredDataModal_sub_locs$aggregation_types$aggregation_type_id,
        filteredDataModal_sub_locs$aggregation_types[[tr(
          "aggregation_type_col",
          language$language
        )]]
      )
      if (!is.null(input$traceNew_aggregation)) {
        tmp.selected <- if (input$traceNew_aggregation %in% tmp.choices) {
          input$traceNew_aggregation
        } else if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      }
      updateSelectizeInput(
        session,
        "traceNew_aggregation",
        choices = tmp.choices,
        selected = tmp.selected
      )

      tmp.choices <- stats::setNames(
        filteredDataModal_sub_locs$rates$seconds,
        filteredDataModal_sub_locs$rates[, "period"]
      )
      if (!is.null(input$traceNew_rate)) {
        tmp.selected <- if (input$traceNew_rate %in% tmp.choices) {
          input$traceNew_rate
        } else if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      }
      updateSelectizeInput(
        session,
        "traceNew_rate",
        choices = tmp.choices,
        selected = tmp.selected
      )

      tmp.choices <- stats::setNames(
        filteredDataModal_sub_locs$params$parameter_id,
        filteredDataModal_sub_locs$params[[tr(
          "param_name_col",
          language$language
        )]]
      )
      if (!is.null(input$traceNew_param)) {
        tmp.selected <- if (input$traceNew_param %in% tmp.choices) {
          input$traceNew_param
        } else if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      }
      updateSelectizeInput(
        session,
        "traceNew_param",
        choices = tmp.choices,
        selected = tmp.selected
      )

      filteredDataModal_z$timeseries <- filteredDataModal_sub_locs$timeseries
      filteredDataModal_z$media <- filteredDataModal_sub_locs$media
      filteredDataModal_z$aggregation_types <- filteredDataModal_sub_locs$aggregation_types
      filteredDataModal_z$rates <- filteredDataModal_sub_locs$rates
      filteredDataModal_z$params <- filteredDataModal_sub_locs$params

      filteredDataModal_media$timeseries <- filteredDataModal_sub_locs$timeseries
      filteredDataModal_media$aggregation_types <- filteredDataModal_sub_locs$aggregation_types
      filteredDataModal_media$rates <- filteredDataModal_sub_locs$rates
      filteredDataModal_media$params <- filteredDataModal_sub_locs$params

      filteredDataModal_aggregation$timeseries <- filteredDataModal_sub_locs$timeseries
      filteredDataModal_aggregation$rates <- filteredDataModal_sub_locs$rates
      filteredDataModal_aggregation$params <- filteredDataModal_sub_locs$params

      filteredDataModal_rate$timeseries <- filteredDataModal_sub_locs$timeseries
      filteredDataModal_rate$params <- filteredDataModal_sub_locs$params
    })

    ### Filter based on the selected z #################################################################
    observeEvent(input$traceNew_z, {
      req(filteredDataModal_z)

      # Filter the data based on the selected z value
      if (is.null(input$traceNew_z) || length(input$traceNew_z) != 1) {
        return()
      }

      # Find the new timeseries rows based on the selected sub-location
      filteredDataModal_z$timeseries <- filteredDataModal$timeseries[
        filteredDataModal$timeseries$z == input$traceNew_z,
      ]

      filteredDataModal_z$media <- filteredDataModal$media[
        filteredDataModal$media$media_id %in%
          filteredDataModal_z$timeseries$media_id,
      ]
      filteredDataModal_z$aggregation_types <- filteredDataModal$aggregation_types[
        filteredDataModal$aggregation_types$aggregation_type_id %in%
          filteredDataModal_z$timeseries$aggregation_type_id,
      ]
      filteredDataModal_z$rates <- filteredDataModal$rates[
        filteredDataModal$rates$seconds %in%
          filteredDataModal_z$timeseries$record_rate,
      ]
      filteredDataModal_z$params <- filteredDataModal$params[
        filteredDataModal$params$parameter_id %in%
          filteredDataModal_z$timeseries$parameter_id,
      ]

      # Update the media, aggregation, rate, and param selectizeInputs with what's left in filteredDataModal_z.
      # If possible, keep the previous selection, otherwise if there's only one choice available, select it, else null
      tmp.choices <- stats::setNames(
        filteredDataModal_z$media$media_id,
        filteredDataModal_z$media[[tr("media_type_col", language$language)]]
      )
      if (!is.null(input$traceNew_media)) {
        tmp.selected <- if (input$traceNew_media %in% tmp.choices) {
          input$traceNew_media
        } else if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      }
      updateSelectizeInput(
        session,
        "traceNew_media",
        choices = tmp.choices,
        selected = tmp.selected
      )

      tmp.choices <- stats::setNames(
        filteredDataModal_z$aggregation_types$aggregation_type_id,
        filteredDataModal_z$aggregation_types[[tr(
          "aggregation_type_col",
          language$language
        )]]
      )
      if (!is.null(input$traceNew_aggregation)) {
        tmp.selected <- if (input$traceNew_aggregation %in% tmp.choices) {
          input$traceNew_aggregation
        } else if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      }
      updateSelectizeInput(
        session,
        "traceNew_aggregation",
        choices = tmp.choices,
        selected = tmp.selected
      )

      tmp.choices <- stats::setNames(
        filteredDataModal_z$rates$seconds,
        filteredDataModal_z$rates[, "period"]
      )
      if (!is.null(input$traceNew_rate)) {
        tmp.selected <- if (input$traceNew_rate %in% tmp.choices) {
          input$traceNew_rate
        } else if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      }
      updateSelectizeInput(
        session,
        "traceNew_rate",
        choices = tmp.choices,
        selected = tmp.selected
      )

      tmp.choices <- stats::setNames(
        filteredDataModal_z$params$parameter_id,
        filteredDataModal_z$params[[tr("param_name_col", language$language)]]
      )
      if (!is.null(input$traceNew_param)) {
        tmp.selected <- if (input$traceNew_param %in% tmp.choices) {
          input$traceNew_param
        } else if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      }
      updateSelectizeInput(
        session,
        "traceNew_param",
        choices = tmp.choices,
        selected = tmp.selected
      )

      filteredDataModal_media$timeseries <- filteredDataModal_z$timeseries
      filteredDataModal_media$aggregation_types <- filteredDataModal_z$aggregation_types
      filteredDataModal_media$rates <- filteredDataModal_z$rates
      filteredDataModal_media$params <- filteredDataModal_z$params

      filteredDataModal_aggregation$timeseries <- filteredDataModal_z$timeseries
      filteredDataModal_aggregation$rates <- filteredDataModal_z$rates
      filteredDataModal_aggregation$params <- filteredDataModal_z$params

      filteredDataModal_rate$timeseries <- filteredDataModal_z$timeseries
      filteredDataModal_rate$params <- filteredDataModal_z$params
    })

    ### Filter based on the selected media #################################################################
    observeEvent(input$traceNew_media, {
      req(filteredDataModal_media)

      # Filter the data based on the selected media
      if (is.null(input$traceNew_media) || length(input$traceNew_media) != 1) {
        return()
      }

      # Find the new timeseries rows based on the selected media
      filteredDataModal_media$timeseries <- filteredDataModal$timeseries[
        filteredDataModal$timeseries$media_id == input$traceNew_media,
      ]

      filteredDataModal_media$aggregation_types <- filteredDataModal$aggregation_types[
        filteredDataModal$aggregation_types$aggregation_type_id %in%
          filteredDataModal_media$timeseries$aggregation_type_id,
      ]
      filteredDataModal_media$rates <- filteredDataModal$rates[
        filteredDataModal$rates$seconds %in%
          filteredDataModal_media$timeseries$record_rate,
      ]
      filteredDataModal_media$params <- filteredDataModal$params[
        filteredDataModal$params$parameter_id %in%
          filteredDataModal_media$timeseries$parameter_id,
      ]

      # Update the aggregation, rate, and param selectizeInputs with what's left in filteredDataModal_media.
      # If possible, keep the previous selection, otherwise if there's only one choice available, select it, else null
      tmp.choices <- stats::setNames(
        filteredDataModal_media$aggregation_types$aggregation_type_id,
        filteredDataModal_media$aggregation_types[[tr(
          "aggregation_type_col",
          language$language
        )]]
      )
      if (!is.null(input$traceNew_aggregation)) {
        tmp.selected <- if (input$traceNew_aggregation %in% tmp.choices) {
          input$traceNew_aggregation
        } else if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      }
      updateSelectizeInput(
        session,
        "traceNew_aggregation",
        choices = tmp.choices,
        selected = tmp.selected
      )

      tmp.choices <- stats::setNames(
        filteredDataModal_media$rates$seconds,
        filteredDataModal_media$rates[, "period"]
      )
      if (!is.null(input$traceNew_rate)) {
        tmp.selected <- if (input$traceNew_rate %in% tmp.choices) {
          input$traceNew_rate
        } else if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      }
      updateSelectizeInput(
        session,
        "traceNew_rate",
        choices = tmp.choices,
        selected = tmp.selected
      )

      tmp.choices <- stats::setNames(
        filteredDataModal_media$params$parameter_id,
        filteredDataModal_media$params[[tr(
          "param_name_col",
          language$language
        )]]
      )
      if (!is.null(input$traceNew_param)) {
        tmp.selected <- if (input$traceNew_param %in% tmp.choices) {
          input$traceNew_param
        } else if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      }
      updateSelectizeInput(
        session,
        "traceNew_param",
        choices = tmp.choices,
        selected = tmp.selected
      )

      filteredDataModal_aggregation$timeseries <- filteredDataModal_media$timeseries
      filteredDataModal_aggregation$rates <- filteredDataModal_media$rates
      filteredDataModal_aggregation$params <- filteredDataModal_media$params

      filteredDataModal_rate$timeseries <- filteredDataModal_media$timeseries
      filteredDataModal_rate$params <- filteredDataModal_media$params
    }) # End filter based on the selected media

    ### Filter based on the selected aggregation type ##############################################################
    observeEvent(input$traceNew_aggregation, {
      req(filteredDataModal_aggregation)

      # Filter the data based on the selected aggregation type
      if (
        is.null(input$traceNew_aggregation) ||
          length(input$traceNew_aggregation) != 1
      ) {
        return()
      }

      # Find the new timeseries rows based on the selected aggregation type
      filteredDataModal_aggregation$timeseries <- filteredDataModal$timeseries[
        filteredDataModal$timeseries$aggregation_type_id ==
          input$traceNew_aggregation,
      ]

      filteredDataModal_aggregation$rates <- filteredDataModal$rates[
        filteredDataModal$rates$seconds %in%
          filteredDataModal_aggregation$timeseries$record_rate,
      ]
      filteredDataModal_aggregation$params <- filteredDataModal$params[
        filteredDataModal$params$parameter_id %in%
          filteredDataModal_aggregation$timeseries$parameter_id,
      ]

      # Update the rate and param selectizeInputs with what's left in filteredDataModal_aggregation.
      # If possible, keep the previous selection, otherwise if there's only one choice available, select it, else null
      tmp.choices <- stats::setNames(
        filteredDataModal_aggregation$rates$seconds,
        filteredDataModal_aggregation$rates[, "period"]
      )
      if (!is.null(input$traceNew_rate)) {
        tmp.selected <- if (input$traceNew_rate %in% tmp.choices) {
          input$traceNew_rate
        } else if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      }
      updateSelectizeInput(
        session,
        "traceNew_rate",
        choices = tmp.choices,
        selected = tmp.selected
      )

      tmp.choices <- stats::setNames(
        filteredDataModal_aggregation$params$parameter_id,
        filteredDataModal_aggregation$params[[tr(
          "param_name_col",
          language$language
        )]]
      )
      if (!is.null(input$traceNew_param)) {
        tmp.selected <- if (input$traceNew_param %in% tmp.choices) {
          input$traceNew_param
        } else if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      }
      updateSelectizeInput(
        session,
        "traceNew_param",
        choices = tmp.choices,
        selected = tmp.selected
      )

      filteredDataModal_rate$timeseries <- filteredDataModal_aggregation$timeseries
      filteredDataModal_rate$params <- filteredDataModal_aggregation$params
    }) # End filter based on the selected aggregation type

    ### Filter based on the selected record rate ##############################################################
    observeEvent(input$traceNew_rate, {
      req(filteredDataModal_rate)

      # Filter the data based on the selected record rate
      if (is.null(input$traceNew_rate) || length(input$traceNew_rate) != 1) {
        return()
      }

      # Find the new timeseries rows based on the selected record rate
      filteredDataModal_rate$timeseries <- filteredDataModal$timeseries[
        filteredDataModal$timeseries$record_rate == input$traceNew_rate,
      ]

      filteredDataModal_rate$params <- filteredDataModal$params[
        filteredDataModal$params$parameter_id %in%
          filteredDataModal_rate$timeseries$parameter_id,
      ]

      # Update the param selectizeInput with what's left in filteredDataModal_rate.
      # If possible, keep the previous selection, otherwise if there's only one choice available, select it, else null
      tmp.choices <- stats::setNames(
        filteredDataModal_rate$params$parameter_id,
        filteredDataModal_rate$params[[tr("param_name_col", language$language)]]
      )
      if (!is.null(input$traceNew_param)) {
        tmp.selected <- if (input$traceNew_param %in% tmp.choices) {
          input$traceNew_param
        } else if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      } else {
        tmp.selected <- if (length(tmp.choices) == 1) {
          tmp.choices[1]
        } else {
          character(0)
        }
      }
      updateSelectizeInput(
        session,
        "traceNew_param",
        choices = tmp.choices,
        selected = tmp.selected
      )
    }) # End filter based on the selected record rate

    ## Add/remove/modify trace buttons #######################################################################

    observeEvent(input$add_new_trace, {
      shinyjs::hide("add_subplot")
      if (traceCount() == 1) {
        traces$trace1 <- list(
          trace = "trace1",
          location_id = input$location,
          sub_location_id = input$sub_location,
          z = input$z,
          media = input$media,
          aggregation = input$aggregation,
          rate = input$rate,
          parameter = input$param,
          lead_lag = 0
        )
        traces$trace2 <- list(
          trace = "trace2",
          location_id = input$traceNew_location,
          sub_location_id = input$traceNew_sub_location,
          z = input$traceNew_z,
          media = input$traceNew_media,
          aggregation = input$traceNew_aggregation,
          rate = input$traceNew_rate,
          parameter = input$traceNew_param,
          lead_lag = input$traceNew_lead_lag
        )

        button1Text <- HTML(paste0(
          "<b>Trace 1</b><br>",
          moduleData$params[
            moduleData$params$parameter_id == traces$trace1$parameter,
            tr("param_name_col", language$language)
          ],
          "<br>",
          moduleData$locs[
            moduleData$locs$location_id == traces$trace1$location_id,
            tr("generic_name_col", language$language)
          ]
        ))
        button2Text <- HTML(paste0(
          "<b>Trace 2</b><br>",
          moduleData$params[
            moduleData$params$parameter_id == traces$trace2$parameter,
            tr("param_name_col", language$language)
          ],
          "<br>",
          moduleData$locs[
            moduleData$locs$location_id == traces$trace2$location_id,
            tr("generic_name_col", language$language)
          ],
          "<br>Lead/lag ",
          traces$trace2$lead_lag,
          " hours"
        ))
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
        traces$trace3 <- list(
          trace = "trace3",
          location_id = input$traceNew_location,
          sub_location_id = input$traceNew_sub_location,
          z = input$traceNew_z,
          media = input$traceNew_media,
          aggregation = input$traceNew_aggregation,
          rate = input$traceNew_rate,
          parameter = input$traceNew_param,
          lead_lag = input$traceNew_lead_lag
        )
        button3Text <- HTML(paste0(
          "<b>Trace 3</b><br>",
          moduleData$params[
            moduleData$params$parameter_id == traces$trace3$parameter,
            tr("param_name_col", language$language)
          ],
          "<br>",
          moduleData$locs[
            moduleData$locs$location_id == traces$trace3$location_id,
            tr("generic_name_col", language$language)
          ],
          "<br>Lead/lag ",
          traces$trace3$lead_lag,
          " hours"
        ))
        output$trace3_ui <- renderUI({
          actionButton(ns("trace3"), button3Text)
        })
        traceCount(3)
      } else if (traceCount() == 3) {
        traces$trace4 <- list(
          trace = "trace4",
          location_id = input$traceNew_location,
          sub_location_id = input$traceNew_sub_location,
          z = input$traceNew_z,
          media = input$traceNew_media,
          aggregation = input$traceNew_aggregation,
          rate = input$traceNew_rate,
          parameter = input$traceNew_param,
          lead_lag = input$traceNew_lead_lag
        )
        button4Text <- HTML(paste0(
          "<b>Trace 4</b><br>",
          moduleData$params[
            moduleData$params$parameter_id == traces$trace4$parameter,
            tr("param_name_col", language$language)
          ],
          "<br>",
          moduleData$locs[
            moduleData$locs$location_id == traces$trace4$location_id,
            tr("generic_name_col", language$language)
          ],
          "<br>Lead/lag ",
          traces$trace4$lead_lag,
          " hours"
        ))
        output$trace4_ui <- renderUI({
          actionButton(ns("trace4"), button4Text)
        })

        traceCount(4)
        shinyjs::hide("add_trace")
      }

      update_date_range()
      removeModal()
    })

    # Observer for when user clicks a trace button. This should bring up a populated modal with the trace information, allowing user to edit the trace. As well, a new button to remove the trace should appear. Removal of a trace requires rejigging traceCount and elements of traces$trace_n
    clicked_trace <- reactiveVal(NULL)
    trace_modal <- function(trace, ll) {
      showModal(modalDialog(
        selectizeInput(
          ns("traceNew_location"),
          label = tr("loc", language$language),
          choices = stats::setNames(
            moduleData$locs$location_id,
            moduleData$locs[[tr("generic_name_col", language$language)]]
          ),
          multiple = TRUE,
          options = list(maxItems = 1),
          selected = traces[[trace]]$location_id
        ),
        uiOutput(ns("traceNew_sub_loc_ui")),
        uiOutput(ns("traceNew_z_ui")),
        selectizeInput(
          ns("traceNew_media"),
          label = tr("media_type", language$language),
          choices = stats::setNames(
            filteredDataModal_z$media$media_id,
            filteredDataModal_z$media[[tr("media_type_col", language$language)]]
          ),
          selected = traces[[trace]]$media,
          multiple = TRUE,
          options = list(maxItems = 1)
        ),
        selectizeInput(
          ns("traceNew_aggregation"),
          label = tr("aggregation_type", language$language),
          choices = stats::setNames(
            filteredDataModal_media$aggregation_types$aggregation_type_id,
            filteredDataModal_media$aggregation_types[[tr(
              "aggregation_type_col",
              language$language
            )]]
          ),
          selected = traces[[trace]]$aggregation,
          multiple = TRUE,
          options = list(maxItems = 1)
        ),
        selectizeInput(
          ns("traceNew_rate"),
          label = tr("nominal_rate", language$language),
          choices = stats::setNames(
            filteredDataModal_aggregation$rates$seconds,
            filteredDataModal_aggregation$rates[, "period"]
          ),
          selected = traces[[trace]]$rate,
          multiple = TRUE,
          options = list(maxItems = 1)
        ),
        selectizeInput(
          ns("traceNew_param"),
          label = tr("parameter", language$language),
          choices = stats::setNames(
            filteredDataModal_rate$params$parameter_id,
            filteredDataModal_rate$params[[tr(
              "param_name_col",
              language$language
            )]]
          ),
          selected = traces[[trace]]$parameter
        ),
        if (ll) {
          numericInput(
            ns("traceNew_lead_lag"),
            "Lead/lag in hours",
            value = traces[[trace]]$lead_lag
          )
        } else {
          NULL
        },
        footer = tagList(
          actionButton(
            ns("modify_trace"),
            tr("modify_trace", language$language)
          ),
          actionButton(
            ns("remove_trace"),
            tr("remove_trace", language$language)
          ),
          actionButton(ns("cancel_modify"), tr("cancel", language$language))
        ),
        easyClose = TRUE
      ))
    }
    observeEvent(input$trace1, {
      trace_modal("trace1", FALSE)
      clicked_trace(traces$trace1$trace)
    })
    observeEvent(input$trace2, {
      trace_modal("trace2", TRUE)
      clicked_trace(traces$trace2$trace)
    })
    observeEvent(input$trace3, {
      trace_modal("trace3", TRUE)
      clicked_trace(traces$trace3$trace)
    })
    observeEvent(input$trace4, {
      trace_modal("trace4", TRUE)
      clicked_trace(traces$trace4$trace)
    })

    observeEvent(input$cancel_modify, {
      clicked_trace(NULL)
      removeModal()
    })

    ## modify/delete traces
    observeEvent(input$modify_trace, {
      # Update the trace values
      target_trace <- clicked_trace()
      traces[[target_trace]]$parameter <- as.numeric(input$traceNew_param)
      traces[[target_trace]]$location_id <- input$traceNew_location
      traces[[target_trace]]$lead_lag <- input$traceNew_lead_lag

      # Update the trace button text
      if (target_trace == "trace1") {
        button_text <- HTML(paste0(
          "<b>Trace ",
          target_trace,
          "</b><br>",
          moduleData$params[
            moduleData$params$parameter_id == traces[[target_trace]]$parameter,
            "param_name"
          ],
          "<br>",
          moduleData$locs[
            moduleData$locs$location_id == traces[[target_trace]]$location_id,
            tr("generic_name_col", language$language)
          ]
        ))
      } else {
        button_text <- HTML(paste0(
          "<b>Trace ",
          target_trace,
          "</b><br>",
          moduleData$params[
            moduleData$params$parameter_id == traces[[target_trace]]$parameter,
            "param_name"
          ],
          "<br>",
          moduleData$locs[
            moduleData$locs$location_id == traces[[target_trace]]$location_id,
            tr("generic_name_col", language$language)
          ],
          "<br>Lead/lag ",
          traces[[target_trace]]$lead_lag,
          " hours"
        ))
      }

      output[[paste0(target_trace, "_ui")]] <- renderUI({
        actionButton(ns(paste0(target_trace)), button_text)
      })
      update_date_range()
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
          button_text <- HTML(paste0(
            "<b>Trace ",
            i,
            "</b><br>",
            moduleData$params[
              moduleData$params$parameter_id ==
                traces[[paste0("trace", i)]]$parameter,
              "param_name"
            ],
            "<br>",
            moduleData$locs[
              moduleData$locs$location_id ==
                traces[[paste0("trace", i)]]$location_id,
              tr("generic_name_col", language$language)
            ]
          ))
        } else {
          button_text <- HTML(paste0(
            "<b>Trace ",
            i,
            "</b><br>",
            moduleData$params[
              moduleData$params$parameter_id ==
                traces[[paste0("trace", i)]]$parameter,
              "param_name"
            ],
            "<br>",
            moduleData$locs[
              moduleData$locs$location_id ==
                traces[[paste0("trace", i)]]$location_id,
              tr("generic_name_col", language$language)
            ],
            "<br>Lead/lag ",
            traces[[paste0("trace", i)]]$lead_lag,
            " hours"
          ))
        }
        updateActionButton(session, paste0("trace", i), label = button_text)
      }

      if (traceCount() == 1) {
        # Remove the remaining trace button and show the param and location selectors
        shinyjs::hide("trace1_ui")
        shinyjs::show("param")
        shinyjs::show("location")
        shinyjs::show("add_subplot")
        updateSelectizeInput(
          session,
          "param",
          choices = stats::setNames(
            moduleData$params$parameter_id,
            moduleData$params$param_name
          ),
          selected = traces$trace1$parameter
        )

        # Update the location choices
        loc_ids <- unique(moduleData$timeseries$location_id[
          moduleData$timeseries$parameter_id == input$param
        ])
        locs <- moduleData$locs[
          moduleData$locs$location_id %in% loc_ids,
          c("location_id", tr("generic_name_col", language$language))
        ]
        updateSelectizeInput(
          session,
          "location",
          choices = stats::setNames(
            locs$location_id,
            locs[[tr("generic_name_col", language$language)]]
          ),
          selected = traces$trace1$location_id
        )
      } else {
        shinyjs::show("trace1_ui")
      }
      update_date_range()
      removeModal()

      traces$trace1$lead_lag <- 0
    })

    ## Add/remove/modify subplot buttons #######################################################################
    share_axes_run <- reactiveVal(FALSE)
    observeEvent(input$add_new_subplot, {
      shinyjs::hide("add_trace")
      if (subplotCount() == 1) {
        if (!share_axes_run()) {
          output$share_axes <- renderUI({
            div(
              checkboxInput(
                ns("shareX"),
                label = tooltip(
                  trigger = list(
                    tr("share_x_axis", language$language),
                    bsicons::bs_icon("info-circle-fill")
                  ),
                  tr("share_x_axis_tooltip", language$language)
                ),
                value = TRUE
              ),
              checkboxInput(
                ns("shareY"),
                label = tooltip(
                  trigger = list(
                    tr("share_y_axis", language$language),
                    bsicons::bs_icon("info-circle-fill")
                  ),
                  tr("share_y_axis_tooltip", language$language)
                ),
                value = FALSE
              ),
            )
          })
          share_axes_run(TRUE)
        } else {
          shinyjs::show("shareX")
          shinyjs::show("shareY")
        }

        subplots$subplot1 <- list(
          subplot = "subplot1",
          location_id = input$traceNew_location,
          sub_location_id = input$traceNew_sub_location,
          z = input$traceNew_z,
          media = input$traceNew_media,
          aggregation = input$traceNew_aggregation,
          rate = input$traceNew_rate,
          parameter = input$traceNew_param,
          lead_lag = input$traceNew_lead_lag
        )
        subplots$subplot2 <- list(
          subplot = "subplot2",
          location_id = input$traceNew_location,
          sub_location_id = input$traceNew_sub_location,
          z = input$traceNew_z,
          media = input$traceNew_media,
          aggregation = input$traceNew_aggregation,
          rate = input$traceNew_rate,
          parameter = input$traceNew_param,
          lead_lag = input$traceNew_lead_lag
        )
        button1Text <- HTML(paste0(
          "<b>Subplot 1</b><br>",
          moduleData$params[
            moduleData$params$parameter_id == subplots$subplot1$parameter,
            "param_name"
          ],
          "<br>",
          moduleData$locs[
            moduleData$locs$location_id == subplots$subplot1$location_id,
            tr("generic_name_col", language$language)
          ]
        ))
        button2Text <- HTML(paste0(
          "<b>Subplot 2</b><br>",
          moduleData$params[
            moduleData$params$parameter_id == subplots$subplot2$parameter,
            "param_name"
          ],
          "<br>",
          moduleData$locs[
            moduleData$locs$location_id == subplots$subplot2$location_id,
            tr("generic_name_col", language$language)
          ]
        ))
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
        subplots$subplot3 <- list(
          subplot = "subplot3",
          location_id = input$traceNew_location,
          sub_location_id = input$traceNew_sub_location,
          z = input$traceNew_z,
          media = input$traceNew_media,
          aggregation = input$traceNew_aggregation,
          rate = input$traceNew_rate,
          parameter = input$traceNew_param,
          lead_lag = input$traceNew_lead_lag
        )
        button3Text <- HTML(paste0(
          "<b>Subplot 3</b><br>",
          moduleData$params[
            moduleData$params$parameter_id == subplots$subplot3$parameter,
            "param_name"
          ],
          "<br>",
          moduleData$locs[
            moduleData$locs$location_id == subplots$subplot3$location_id,
            tr("generic_name_col", language$language)
          ]
        ))
        output$subplot3_ui <- renderUI({
          actionButton(ns("subplot3"), button3Text)
        })
        subplotCount(3)
      } else if (subplotCount() == 3) {
        subplots$subplot4 <- list(
          subplot = "subplot4",
          location_id = input$traceNew_location,
          sub_location_id = input$traceNew_sub_location,
          z = input$traceNew_z,
          media = input$traceNew_media,
          aggregation = input$traceNew_aggregation,
          rate = input$traceNew_rate,
          parameter = input$traceNew_param,
          lead_lag = input$traceNew_lead_lag
        )
        button4Text <- HTML(paste0(
          "<b>Subplot 4</b><br>",
          moduleData$params[
            moduleData$params$parameter_id == subplots$subplot4$parameter,
            "param_name"
          ],
          "<br>",
          moduleData$locs[
            moduleData$locs$location_id == subplots$subplot4$location_id,
            tr("generic_name_col", language$language)
          ]
        ))
        output$subplot4_ui <- renderUI({
          actionButton(ns("subplot4"), button4Text)
        })

        subplotCount(4)
        shinyjs::hide("add_subplot")
      }

      update_date_range()
      removeModal()
    })

    # Observer for when user clicks a subplot button. This should bring up a populated modal with the subplot information, allowing user to edit the subplot. As well, a new button to remove the subplot should appear. Removal of a subplot requires rejigging subplotCount and elements of subplots$subplot_n
    clicked_subplot <- reactiveVal(NULL)
    subplot_modal <- function(subplot) {
      showModal(modalDialog(
        selectizeInput(
          ns("traceNew_location"),
          label = tr("loc", language$language),
          choices = stats::setNames(
            moduleData$locs$location_id,
            moduleData$locs[[tr("generic_name_col", language$language)]]
          ),
          multiple = TRUE,
          options = list(maxItems = 1),
          selected = subplots[[subplot]]$location_id
        ),
        uiOutput(ns("traceNew_sub_loc_ui")),
        uiOutput(ns("traceNew_z_ui")),
        selectizeInput(
          ns("traceNew_media"),
          label = tr("media_type", language$language),
          choices = stats::setNames(
            filteredDataModal_z$media$media_id,
            filteredDataModal_z$media[[tr("media_type_col", language$language)]]
          ),
          selected = subplots[[subplot]]$media,
          multiple = TRUE,
          options = list(maxItems = 1)
        ),
        selectizeInput(
          ns("traceNew_aggregation"),
          label = tr("aggregation_type", language$language),
          choices = stats::setNames(
            filteredDataModal_media$aggregation_types$aggregation_type_id,
            filteredDataModal_media$aggregation_types[[tr(
              "aggregation_type_col",
              language$language
            )]]
          ),
          selected = subplots[[subplot]]$aggregation,
          multiple = TRUE,
          options = list(maxItems = 1)
        ),
        selectizeInput(
          ns("traceNew_rate"),
          label = tr("nominal_rate", language$language),
          choices = stats::setNames(
            filteredDataModal_aggregation$rates$seconds,
            filteredDataModal_aggregation$rates[, "period"]
          ),
          selected = subplots[[subplot]]$rate,
          multiple = TRUE,
          options = list(maxItems = 1)
        ),
        selectizeInput(
          ns("traceNew_param"),
          label = tr("parameter", language$language),
          choices = stats::setNames(
            filteredDataModal_rate$params$parameter_id,
            filteredDataModal_rate$params[[tr(
              "param_name_col",
              language$language
            )]]
          ),
          selected = subplots[[subplot]]$parameter
        ),
        footer = tagList(
          actionButton(
            ns("modify_subplot"),
            tr("modify_subplot", language$language)
          ),
          actionButton(
            ns("remove_subplot"),
            tr("remove_subplot", language$language)
          ),
          actionButton(ns("cancel_modify"), tr("cancel", language$language))
        ),
        easyClose = TRUE
      ))
    }

    observeEvent(input$subplot1, {
      subplot_modal("subplot1")
      clicked_subplot(subplots$subplot1$subplot)
    })
    observeEvent(input$subplot2, {
      subplot_modal("subplot2")
      clicked_subplot(subplots$subplot2$subplot)
    })
    observeEvent(input$subplot3, {
      subplot_modal("subplot3")
      clicked_subplot(subplots$subplot3$subplot)
    })
    observeEvent(input$subplot4, {
      subplot_modal("subplot4")
      clicked_subplot(subplots$subplot4$subplot)
    })

    ## modify/delete subplot
    observeEvent(input$modify_subplot, {
      # Update the subplot values
      target_subplot <- clicked_subplot()
      subplots[[target_subplot]]$parameter <- as.numeric(input$traceNew_param)
      subplots[[target_subplot]]$location_id <- input$traceNew_location

      # Update the subplot button text
      button_text <- HTML(paste0(
        "<b>Subplot ",
        target_subplot,
        "</b><br>",
        moduleData$params[
          moduleData$params$parameter_id ==
            subplots[[target_subplot]]$parameter,
          "param_name"
        ],
        "<br>",
        moduleData$locs[
          moduleData$locs$location_id == subplots[[target_subplot]]$location_id,
          tr("generic_name_col", language$language)
        ]
      ))

      output[[paste0(target_subplot, "_ui")]] <- renderUI({
        actionButton(ns(paste0(target_subplot)), button_text)
      })
      update_date_range()
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
          button_text <- HTML(paste0(
            "<b>Subplot ",
            i,
            "</b><br>",
            moduleData$params[
              moduleData$params$parameter_id ==
                subplots[[paste0("subplot", i)]]$parameter,
              "param_name"
            ],
            "<br>",
            moduleData$locs[
              moduleData$locs$location_id ==
                subplots[[paste0("subplot", i)]]$location_id,
              tr("generic_name_col", language$language)
            ]
          ))
        } else {
          button_text <- HTML(paste0(
            "<b>Subplot ",
            i,
            "</b><br>",
            moduleData$params[
              moduleData$params$parameter_id ==
                subplots[[paste0("subplot", i)]]$parameter,
              "param_name"
            ],
            "<br>",
            moduleData$locs[
              moduleData$locs$location_id ==
                subplots[[paste0("subplot", i)]]$location_id,
              tr("generic_name_col", language$language)
            ]
          ))
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
        updateSelectizeInput(
          session,
          "param",
          choices = stats::setNames(
            moduleData$params$parameter_id,
            moduleData$params$param_name
          ),
          selected = subplots$subplot1$parameter
        )

        # Update the location choices
        loc_ids <- unique(moduleData$timeseries$location_id[
          moduleData$timeseries$parameter_id == input$param
        ])
        locs <- moduleData$locs[
          moduleData$locs$location_id %in% loc_ids,
          c("location_id", tr("generic_name_col", language$language))
        ]
        updateSelectizeInput(
          session,
          "location",
          choices = stats::setNames(
            locs$location_id,
            locs[[tr("generic_name_col", language$language)]]
          ),
          selected = subplots$subplot1$location_id
        )
      } else {
        shinyjs::show("subplot1_ui")
      }
      update_date_range()
      removeModal()
    })

    # Create ExtendedTasks to render plots ############################################################
    # Overlapping years plot
    plot_output_overlap <- ExtendedTask$new(
      function(
        loc,
        sub_loc,
        record_rate,
        aggregation_type,
        z,
        param,
        date_start,
        date_end,
        yrs,
        historic_range,
        apply_datum,
        filter,
        unusable,
        line_scale,
        axis_scale,
        legend_scale,
        legend_position,
        lang,
        gridx,
        gridy,
        webgl,
        config
      ) {
        promises::future_promise({
          tryCatch(
            {
              con <- AquaConnect(
                name = config$dbName,
                host = config$dbHost,
                port = config$dbPort,
                username = config$dbUser,
                password = config$dbPass,
                silent = TRUE
              )
              on.exit(DBI::dbDisconnect(con))

              plot <- plotOverlap(
                location = loc,
                sub_location = sub_loc,
                z = z,
                record_rate = record_rate,
                aggregation_type = aggregation_type,
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
                webgl = webgl,
                slider = FALSE,
                gridx = gridx,
                gridy = gridy,
                con = con,
                data = TRUE,
                resolution = 'max'
              )
              return(plot)
            },
            error = function(e) {
              return(e$message)
            }
          )
        })
      } # End of ExtendedTask function
    ) |>
      bind_task_button("make_plot")

    # Single timeseries plot
    plot_output_timeseries <- ExtendedTask$new(
      function(
        loc,
        sub_loc,
        record_rate,
        aggregation_type,
        z,
        param,
        date_start,
        date_end,
        historic_range,
        apply_datum,
        filter,
        unusable,
        grades,
        approvals,
        qualifiers,
        line_scale,
        axis_scale,
        legend_scale,
        legend_position,
        lang,
        gridx,
        gridy,
        webgl,
        config
      ) {
        promises::future_promise({
          tryCatch(
            {
              con <- AquaConnect(
                name = config$dbName,
                host = config$dbHost,
                port = config$dbPort,
                username = config$dbUser,
                password = config$dbPass,
                silent = TRUE
              )
              on.exit(DBI::dbDisconnect(con))

              plot <- plotTimeseries(
                location = loc,
                sub_location = sub_loc,
                parameter = param,
                record_rate = record_rate,
                aggregation_type = aggregation_type,
                z = z,
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
                webgl = webgl,
                slider = FALSE,
                gridx = gridx,
                gridy = gridy,
                con = con,
                data = TRUE,
                tzone = "MST"
              )

              return(plot)
            },
            error = function(e) {
              return(e$message)
            }
          )
        })
      } # End of ExtendedTask function
    ) |>
      bind_task_button("make_plot")

    # Multiple traces plot
    plot_output_timeseries_traces <- ExtendedTask$new(
      function(
        locs,
        sub_locs,
        z,
        record_rates,
        aggregation_types,
        params,
        lead_lags,
        date_start,
        date_end,
        historic_range,
        apply_datum,
        filter,
        unusable,
        line_scale,
        axis_scale,
        legend_scale,
        legend_position,
        lang,
        gridx,
        gridy,
        shareX,
        shareY,
        webgl,
        config
      ) {
        promises::future_promise({
          tryCatch(
            {
              con <- AquaConnect(
                name = config$dbName,
                host = config$dbHost,
                port = config$dbPort,
                username = config$dbUser,
                password = config$dbPass,
                silent = TRUE
              )
              on.exit(DBI::dbDisconnect(con))

              plot <- plotMultiTimeseries(
                type = "traces",
                locations = locs,
                sub_locations = sub_locs,
                z = z,
                record_rates = record_rates,
                aggregation_types = aggregation_types,
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
                webgl = webgl,
                gridx = gridx,
                gridy = gridy,
                shareX = shareX,
                shareY = shareY,
                con = con,
                data = TRUE
              )

              return(plot)
            },
            error = function(e) {
              return(e$message)
            }
          )
        })
      } # End of ExtendedTask function
    ) |>
      bind_task_button("make_plot")

    # Multiple subplots plot
    plot_output_timeseries_subplots <- ExtendedTask$new(
      function(
        locs,
        sub_locs,
        z,
        record_rates,
        aggregation_types,
        params,
        date_start,
        date_end,
        historic_range,
        apply_datum,
        filter,
        unusable,
        line_scale,
        axis_scale,
        legend_scale,
        legend_position,
        lang,
        gridx,
        gridy,
        shareX,
        shareY,
        webgl,
        config
      ) {
        promises::future_promise({
          tryCatch(
            {
              con <- AquaConnect(
                name = config$dbName,
                host = config$dbHost,
                port = config$dbPort,
                username = config$dbUser,
                password = config$dbPass,
                silent = TRUE
              )
              on.exit(DBI::dbDisconnect(con))

              plot <- plotMultiTimeseries(
                type = "subplots",
                locations = locs,
                sub_locations = sub_locs,
                z = z,
                record_rates = record_rates,
                aggregation_types = aggregation_types,
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
                webgl = webgl,
                gridx = gridx,
                gridy = gridy,
                shareX = shareX,
                shareY = shareY,
                con = con,
                data = TRUE
              )

              return(plot)
            },
            error = function(e) {
              return(e$message)
            }
          )
        })
      } # End of ExtendedTask function
    ) |>
      bind_task_button("make_plot")

    # Create the plots and render ############################################################
    plot_created <- reactiveVal(FALSE) # Flag to determine if a plot has been created
    # Call up the ExtendedTask and render the plot
    observeEvent(
      input$make_plot,
      {
        if (plot_created()) {
          shinyjs::hide("full_screen_ui")
        }

        if (input$plot_type == "over") {
          if (is.null(input$location)) {
            showModal(modalDialog(
              tr("pl_select_loc", language$language),
              footer = tagList(
                actionButton(ns("cancel"), tr("cancel", language$language))
              ),
              easyClose = TRUE
            ))
            return()
          }
          if (nchar(input$location) == 0) {
            showModal(modalDialog(
              tr("pl_select_loc", language$language),
              footer = tagList(
                actionButton(ns("cancel"), tr("cancel", language$language))
              ),
              easyClose = TRUE
            ))
            return()
          }
          loc <- as.numeric(input$location)
          if (is.null(input$param)) {
            showModal(modalDialog(
              tr("pl_select_param", language$language),
              footer = tagList(
                actionButton(ns("cancel"), tr("cancel", language$language))
              ),
              easyClose = TRUE
            ))
            return()
          }
          if (nchar(input$param) == 0) {
            showModal(modalDialog(
              tr("pl_select_param", language$language),
              footer = tagList(
                actionButton(ns("cancel"), tr("cancel", language$language))
              ),
              easyClose = TRUE
            ))
            return()
          }
          param <- as.numeric(input$param)

          if (is.null(input$sub_location)) {
            sub_loc <- NULL
          } else if (nchar(input$sub_location) > 0) {
            sub_loc <- as.numeric(input$sub_location)
          } else {
            sub_loc <- NULL
          }
          if (is.null(input$z)) {
            z <- NULL
          } else if (nchar(input$z) > 0) {
            z <- as.numeric(input$z)
          } else {
            z <- NULL
          }
          if (is.null(input$rate)) {
            record_rate <- NULL
          } else {
            record_rate <- if (nchar(input$rate) > 0) {
              as.numeric(input$rate)
            } else {
              NULL
            }
          }
          if (is.null(input$aggregation)) {
            aggregation_type <- NULL
          } else {
            aggregation_type <- if (nchar(input$aggregation) > 0) {
              as.numeric(input$aggregation)
            } else {
              NULL
            }
          }

          plot_output_overlap$invoke(
            loc = loc,
            sub_loc = sub_loc,
            z = z,
            record_rate = record_rate,
            aggregation_type = aggregation_type,
            param = param,
            date_start = input$date_range[1],
            date_end = input$date_range[2],
            yrs = input$years,
            historic_range = input$historic_range_overlap,
            apply_datum = input$apply_datum,
            filter = if (input$plot_filter) 20 else NULL,
            unusable = input$unusable,
            line_scale = plot_aes$line_scale,
            axis_scale = plot_aes$axis_scale,
            legend_scale = plot_aes$legend_scale,
            legend_position = if (
              windowDims()$width > 1.3 * windowDims()$height
            ) {
              "v"
            } else {
              "h"
            },
            lang = plot_aes$lang,
            gridx = plot_aes$showgridx,
            gridy = plot_aes$showgridy,
            webgl = session$userData$use_webgl,
            config = session$userData$config
          )
        } else if (input$plot_type == "ts") {
          if (traceCount() == 1) {
            # Either a single trace or more than 1 subplot
            if (subplotCount() > 1) {
              # Multiple sub plots
              locs <- as.numeric(c(
                subplots$subplot1$location_id,
                subplots$subplot2$location_id,
                subplots$subplot3$location_id,
                subplots$subplot4$location_id
              ))
              sub_locs <- as.numeric(c(
                subplots$subplot1$sub_location_id,
                subplots$subplot2$sub_location_id,
                subplots$subplot3$sub_location_id,
                subplots$subplot4$sub_location_id
              ))
              z <- as.numeric(c(
                subplots$subplot1$z,
                subplots$subplot2$z,
                subplots$subplot3$z,
                subplots$subplot4$z
              ))
              record_rates <- as.numeric(c(
                subplots$subplot1$rate,
                subplots$subplot2$rate,
                subplots$subplot3$rate,
                subplots$subplot4$rate
              ))
              aggregation_types <- as.numeric(c(
                subplots$subplot1$aggregation,
                subplots$subplot2$aggregation,
                subplots$subplot3$aggregation,
                subplots$subplot4$aggregation
              ))
              params <- as.numeric(c(
                subplots$subplot1$parameter,
                subplots$subplot2$parameter,
                subplots$subplot3$parameter,
                subplots$subplot4$parameter
              ))
              lead_lags <- c(
                subplots$subplot1$lead_lag,
                subplots$subplot2$lead_lag,
                subplots$subplot3$lead_lag,
                subplots$subplot4$lead_lag
              )

              # Make sure that each combination of locs[n], sub_locs[n], z[n], record_rates[n], aggregation_types[n], params[n] and lead_lags[n] is unique
              combinations <- data.frame(
                locs,
                record_rates,
                aggregation_types,
                params,
                lead_lags
              )
              if (length(sub_locs) > 0) {
                combinations$sub_locs <- sub_locs
              }
              if (length(z) > 0) {
                combinations$z <- z
              }
              unique_combinations <- unique(combinations)

              if (nrow(unique_combinations) != nrow(combinations)) {
                showModal(modalDialog(
                  title = tr("error", language$language),
                  tr("modal_error_multi_trace", language$language),
                  footer = tagList(
                    actionButton(ns("cancel"), tr("cancel", language$language))
                  ),
                  easyClose = TRUE
                ))
                return()
              }

              plot_output_timeseries_subplots$invoke(
                locs = locs,
                sub_locs = if (length(sub_locs) == 0) NULL else sub_locs,
                z = if (length(z) == 0) NULL else z,
                record_rates = record_rates,
                aggregation_types = aggregation_types,
                params = params,
                date_start = input$date_range[1],
                date_end = input$date_range[2],
                historic_range = input$historic_range,
                apply_datum = input$apply_datum,
                filter = if (input$plot_filter) 20 else NULL,
                unusable = input$unusable,
                line_scale = plot_aes$line_scale,
                axis_scale = plot_aes$axis_scale,
                legend_scale = plot_aes$legend_scale,
                legend_position = if (
                  windowDims()$width > 1.3 * windowDims()$height
                ) {
                  "v"
                } else {
                  "h"
                },
                lang = plot_aes$lang,
                gridx = plot_aes$showgridx,
                gridy = plot_aes$showgridy,
                webgl = session$userData$use_webgl,
                shareX = input$shareX,
                shareY = input$shareY,
                config = session$userData$config
              )
            } else {
              # Single trace
              if (is.null(input$location)) {
                showModal(modalDialog(
                  tr("pl_select_loc", language$language),
                  footer = tagList(
                    actionButton(ns("cancel"), tr("cancel", language$language))
                  ),
                  easyClose = TRUE
                ))
                return()
              }
              if (nchar(input$location) == 0) {
                showModal(modalDialog(
                  tr("pl_select_loc", language$language),
                  footer = tagList(
                    actionButton(ns("cancel"), tr("cancel", language$language))
                  ),
                  easyClose = TRUE
                ))
                return()
              }
              loc <- as.numeric(input$location)

              if (is.null(input$param)) {
                showModal(modalDialog(
                  tr("pl_select_param", language$language),
                  footer = tagList(
                    actionButton(ns("cancel"), tr("cancel", language$language))
                  ),
                  easyClose = TRUE
                ))
                return()
              }
              if (nchar(input$param) == 0) {
                showModal(modalDialog(
                  tr("pl_select_param", language$language),
                  footer = tagList(
                    actionButton(ns("cancel"), tr("cancel", language$language))
                  ),
                  easyClose = TRUE
                ))
                return()
              }
              param <- as.numeric(input$param)

              if (is.null(input$sub_location)) {
                sub_loc <- NULL
              } else if (nchar(input$sub_location) > 0) {
                sub_loc <- as.numeric(input$sub_location)
              } else {
                sub_loc <- NULL
              }
              if (is.null(input$z)) {
                z <- NULL
              } else if (nchar(input$z) > 0) {
                z <- as.numeric(input$z)
              } else {
                z <- NULL
              }
              if (is.null(input$rate)) {
                record_rate <- NULL
              } else {
                record_rate <- if (nchar(input$rate) > 0) {
                  as.numeric(input$rate)
                } else {
                  NULL
                }
              }
              if (is.null(input$aggregation)) {
                aggregation_type <- NULL
              } else {
                aggregation_type <- if (nchar(input$aggregation) > 0) {
                  as.numeric(input$aggregation)
                } else {
                  NULL
                }
              }

              plot_output_timeseries$invoke(
                loc = loc,
                sub_loc = sub_loc,
                z = z,
                record_rate = record_rate,
                aggregation_type = aggregation_type,
                param = param,
                date_start = input$date_range[1],
                date_end = input$date_range[2],
                historic_range = input$historic_range,
                apply_datum = input$apply_datum,
                filter = if (input$plot_filter) 20 else NULL,
                unusable = input$unusable,
                grades = input$grades,
                approvals = input$approvals,
                qualifiers = input$qualifiers,
                line_scale = plot_aes$line_scale,
                axis_scale = plot_aes$axis_scale,
                legend_scale = plot_aes$legend_scale,
                legend_position = if (
                  windowDims()$width > 1.3 * windowDims()$height
                ) {
                  "v"
                } else {
                  "h"
                },
                lang = plot_aes$lang,
                gridx = plot_aes$showgridx,
                gridy = plot_aes$showgridy,
                webgl = session$userData$use_webgl,
                config = session$userData$config
              )
            }
          } else {
            # Multiple traces, single plot
            locs <- as.numeric(c(
              traces$trace1$location_id,
              traces$trace2$location_id,
              traces$trace3$location_id,
              traces$trace4$location_id
            ))
            sub_locs <- as.numeric(c(
              traces$trace1$sub_location_id,
              traces$trace2$sub_location_id,
              traces$trace3$sub_location_id,
              traces$trace4$sub_location_id
            ))
            z <- as.numeric(c(
              traces$trace1$z,
              traces$trace2$z,
              traces$trace3$z,
              traces$trace4$z
            ))
            record_rates <- as.numeric(c(
              traces$trace1$rate,
              traces$trace2$rate,
              traces$trace3$rate,
              traces$trace4$rate
            ))
            aggregation_types <- as.numeric(c(
              traces$trace1$aggregation,
              traces$trace2$aggregation,
              traces$trace3$aggregation,
              traces$trace4$aggregation
            ))
            params <- as.numeric(c(
              traces$trace1$parameter,
              traces$trace2$parameter,
              traces$trace3$parameter,
              traces$trace4$parameter
            ))
            lead_lags <- c(
              traces$trace1$lead_lag,
              traces$trace2$lead_lag,
              traces$trace3$lead_lag,
              traces$trace4$lead_lag
            )

            # Make sure that each combination of locs[n], sub_locs[n], z[n], record_rates[n], aggregation_types[n], params[n] and lead_lags[n] is unique
            combinations <- data.frame(
              locs,
              record_rates,
              aggregation_types,
              params,
              lead_lags
            )
            if (length(sub_locs) > 0) {
              combinations$sub_locs <- sub_locs
            }
            if (length(z) > 0) {
              combinations$z <- z
            }
            unique_combinations <- unique(combinations)

            if (nrow(unique_combinations) != nrow(combinations)) {
              showModal(modalDialog(
                title = tr("error", language$language),
                tr("modal_error_multi_trace", language$language),
                footer = tagList(
                  actionButton(ns("cancel"), tr("cancel", language$language))
                ),
                easyClose = TRUE
              ))
              return()
            }

            plot_output_timeseries_traces$invoke(
              locs = locs,
              sub_locs = if (length(sub_locs) == 0) NULL else sub_locs,
              z = if (length(z) == 0) NULL else z,
              record_rates = record_rates,
              aggregation_types = aggregation_types,
              params = params,
              lead_lags = lead_lags,
              date_start = input$date_range[1],
              date_end = input$date_range[2],
              historic_range = input$historic_range,
              apply_datum = input$apply_datum,
              filter = if (input$plot_filter) 20 else NULL,
              unusable = input$unusable,
              line_scale = plot_aes$line_scale,
              axis_scale = plot_aes$axis_scale,
              legend_scale = plot_aes$legend_scale,
              legend_position = if (
                windowDims()$width > 1.3 * windowDims()$height
              ) {
                "v"
              } else {
                "h"
              },
              lang = plot_aes$lang,
              gridx = plot_aes$showgridx,
              gridy = plot_aes$showgridy,
              webgl = session$userData$use_webgl,
              shareX = input$shareX,
              shareY = input$shareY,
              config = session$userData$config
            )
          }
        }
      },
      ignoreInit = TRUE
    )

    ## Observe the results of the ExtendedTasks and render the plot ##############
    observeEvent(plot_output_overlap$result(), {
      if (inherits(plot_output_overlap$result(), "character")) {
        showModal(modalDialog(
          title = tr("error", language$language),
          plot_output_overlap$result(),
          footer = tagList(
            actionButton(ns("cancel"), tr("cancel", language$language))
          ),
          easyClose = TRUE
        ))
        return()
      }

      output$plot <- plotly::renderPlotly({
        isolate(plot_output_overlap$result()$plot)
      })

      # Create a full screen button if necessary
      if (!plot_created()) {
        output$full_screen_ui <- renderUI({
          # Side-by-side buttons
          page_fluid(
            div(
              class = "d-inline-block",
              actionButton(
                ns("full_screen"),
                tr("full_screen", language$language)
              ),
              style = "display: none;"
            ),
            div(
              class = "d-inline-block",
              downloadButton(
                ns("download_data"),
                tr("dl_data", language$language)
              ),
              style = "display: none;"
            )
          )
        })
      } else {
        shinyjs::show("full_screen_ui")
      }
      plot_created(TRUE)
    })

    observeEvent(plot_output_timeseries$result(), {
      if (inherits(plot_output_timeseries$result(), "character")) {
        showModal(modalDialog(
          title = tr("error", language$language),
          plot_output_timeseries$result(),
          footer = tagList(
            actionButton(ns("cancel"), tr("cancel", language$language))
          ),
          easyClose = TRUE
        ))
        return()
      }
      output$plot <- plotly::renderPlotly({
        isolate(plot_output_timeseries$result()$plot)
      })

      # Create a full screen button if necessary
      if (!plot_created()) {
        output$full_screen_ui <- renderUI({
          # Side-by-side buttons
          page_fluid(
            div(
              class = "d-inline-block",
              actionButton(ns("full_screen"), "Full screen"),
              style = "display: none;"
            ),
            div(
              class = "d-inline-block",
              downloadButton(ns("download_data"), "Download data"),
              style = "display: none;"
            )
          )
        })
      } else {
        shinyjs::show("full_screen_ui")
      }
      plot_created(TRUE)
    })

    observeEvent(plot_output_timeseries_traces$result(), {
      if (inherits(plot_output_timeseries_traces$result(), "character")) {
        showModal(modalDialog(
          title = tr("error", language$language),
          plot_output_timeseries_traces$result(),
          footer = tagList(
            actionButton(ns("cancel"), tr("cancel", language$language))
          ),
          easyClose = TRUE
        ))
        return()
      }
      output$plot <- plotly::renderPlotly({
        isolate(plot_output_timeseries_traces$result()$plot)
      })

      # Create a full screen button if necessary
      if (!plot_created()) {
        output$full_screen_ui <- renderUI({
          # Side-by-side buttons
          page_fluid(
            div(
              class = "d-inline-block",
              actionButton(ns("full_screen"), "Full screen"),
              style = "display: none;"
            ),
            div(
              class = "d-inline-block",
              downloadButton(ns("download_data"), "Download data"),
              style = "display: none;"
            )
          )
        })
      } else {
        shinyjs::show("full_screen_ui")
      }
      plot_created(TRUE)
    })

    observeEvent(plot_output_timeseries_subplots$result(), {
      if (inherits(plot_output_timeseries_subplots$result(), "character")) {
        showModal(modalDialog(
          title = tr("error", language$language),
          plot_output_timeseries_subplots$result(),
          footer = tagList(
            actionButton(ns("cancel"), tr("cancel", language$language))
          ),
          easyClose = TRUE
        ))
        return()
      }
      output$plot <- plotly::renderPlotly({
        isolate(plot_output_timeseries_subplots$result()$plot)
      })

      # Create a full screen button if necessary
      if (!plot_created()) {
        output$full_screen_ui <- renderUI({
          # Side-by-side buttons
          page_fluid(
            div(
              class = "d-inline-block",
              actionButton(ns("full_screen"), "Full screen"),
              style = "display: none;"
            ),
            div(
              class = "d-inline-block",
              downloadButton(ns("download_data"), "Download data"),
              style = "display: none;"
            )
          )
        })
      } else {
        shinyjs::show("full_screen_ui")
      }
      plot_created(TRUE)
    })

    # Observe changes to the windowDims reactive value and update the legend position using plotlyProxy
    # The js function takes care of debouncing the window resize event and also reacts to a change in orientation or full screen event
    observeEvent(
      windowDims(),
      {
        req(plot_created())
        if (is.null(windowDims())) {
          return()
        }
        if (windowDims()$width > 1.3 * windowDims()$height) {
          plotly::plotlyProxy("plot", session) %>%
            plotly::plotlyProxyInvoke(
              "relayout",
              legend = list(orientation = "v")
            )
        } else {
          plotly::plotlyProxy("plot", session) %>%
            plotly::plotlyProxyInvoke(
              "relayout",
              legend = list(orientation = "h")
            )
        }
      },
      ignoreNULL = TRUE
    )

    # Observe the full screen button and run the javascript function to make the plot full screen
    observeEvent(
      input$full_screen,
      {
        shinyjs::runjs(paste0("toggleFullScreen('", ns("plot"), "');"))
        # Manually trigger a window resize event after some delay
        shinyjs::runjs(
          "
                      setTimeout(function() {
                        sendWindowSizeToShiny();
                      }, 700);
                    "
        )
      },
      ignoreInit = TRUE
    )

    # Send the user the plotting data
    output$download_data <- downloadHandler(
      filename = function() {
        time <- Sys.time()
        attr(time, "tzone") <- "UTC"
        paste0(
          "continuous_plot_data_",
          gsub("-", "", gsub(" ", "_", gsub(":", "", substr(time, 0, 16)))),
          "_UTC.xlsx"
        )
      },
      content = function(file) {
        if (input$plot_type == "over") {
          openxlsx::write.xlsx(plot_output_overlap$result()$data, file)
        } else if (input$plot_type == "ts") {
          if (traceCount() == 1) {
            # Either a single trace or more than 1 subplot
            if (subplotCount() > 1) {
              # Multiple sub plots
              openxlsx::write.xlsx(
                plot_output_timeseries_subplots$result()$data,
                file
              )
            } else {
              # Single trace
              openxlsx::write.xlsx(plot_output_timeseries$result()$data, file)
            }
          } else {
            # Multiple traces, single plot
            openxlsx::write.xlsx(
              plot_output_timeseries_traces$result()$data,
              file
            )
          }
        }
      } # End content
    ) # End downloadHandler
  }) # End of moduleServer
}
