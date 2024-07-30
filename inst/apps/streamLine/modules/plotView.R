plotUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$script(
      HTML("
       // Handles custom tooltips updates, binds tooltip properties to elements
            Shiny.addCustomMessageHandler('update-tooltip', function(message) {
                var selector = '#' + message.id;
                $(selector).attr('title', message.title)
                .tooltip('fixTitle').tooltip('hide');
            });
    ")
    ),
    sidebarLayout(
      sidebarPanel(
        radioButtons(ns("main_choice"), NULL, choices = list("Map" = "map", "Plot" = "plot"), selected = "map", inline = TRUE),
        conditionalPanel(
          condition = "input.main_choice == 'map'",
          ns = ns,
          span(
            id = ns("infoIcon"),
            `data-toggle` = "tooltip",
            `data-placement` = "right",
            `data-trigger` = "click hover",
            title = "Placeholder",
            icon("info-circle", style = "font-size: 150%;")),
          # All inputs below are actualized in the server module
          
          # selectizeInput(ns("type"), "Data Type", choices = c("All" = "All"), multiple = TRUE), # choices and labels are updated in the server module
          selectizeInput(ns("pType"), "Parameter Type", choices = c("All" = "All"), multiple = TRUE),
          selectizeInput(ns("pGrp"), "Parameter Group", choices = c("All" = "All"), multiple = TRUE),
          selectizeInput(ns("proj"), "Project", choices = c("All" = "All"), multiple = TRUE),
          selectizeInput(ns("net"), "Network", choices = c("All" = "All"), multiple = TRUE),
          selectizeInput(ns("param"), "Parameter", choices = "Placeholder", multiple = FALSE), # This differs from other modules because the map can only have one parameter at a time
          dateInput(ns("date"), "Placeholder", value = Sys.Date(), format = "yyyy-mm-dd"),
          numericInput(ns("approx"), "Placeholder", value = 1),
          radioButtons(ns("abs_rel"), label = NULL, choices = "Placeholder", inline = TRUE),
          actionButton(ns("reset"), "Reset Filters"),
          actionButton(ns("render_map"), "Create Map")
        ),
        
        conditionalPanel(
          condition = "input.main_choice == 'plot'",
          ns = ns,
          span(
            id = ns("infoIcon"),
            `data-toggle` = "tooltip",
            `data-placement` = "right",
            `data-trigger` = "click hover",
            title = "Placeholder",
            icon("info-circle", style = "font-size: 150%;")),
          # All inputs below are actualized in the server module
          selectizeInput(ns("type"), "Data Type", choices = c("Discrete" = "discrete"), multiple = FALSE), # choices and labels are updated in the server module
          selectizeInput(ns("pType"), "Parameter Type", choices = c("All" = "All"), multiple = TRUE),
          selectizeInput(ns("pGrp"), "Parameter Group", choices = c("All" = "All"), multiple = TRUE),
          selectizeInput(ns("param"), "Parameter", choices = c("All" = "All"), multiple = TRUE),
          selectizeInput(ns("proj"), "Project", choices = c("All" = "All"), multiple = TRUE),
          selectizeInput(ns("net"), "Network", choices = c("All" = "All"), multiple = TRUE),
          sliderInput(ns("yrs"), "With data between...", sep = "", min = 1897, max = lubridate::year(Sys.Date()), value = c(1897, lubridate::year(Sys.Date())), step = 1),
          selectizeInput(ns("loc"), "Or choose one or more locations", choices = c("All" = "All"), multiple = TRUE),
          actionButton(ns("reset"), "Reset Filters")
        ),
        
      ),
      mainPanel(
        conditionalPanel(
          condition = "input.main_choice == 'map'",
          uiOutput(ns("map"))
        ),
        conditionalPanel(condition = "input.main_choice == 'plot'",
                         uiOutput(ns("plot"))             
        ),
        
      )
    )
  )
}

plot <- function(id, con, language, restoring, data, inputs) {
  moduleServer(id, function(input, output, session) {
    
    print(input$main_choice)
    setBookmarkExclude(c())
    ns <- session$ns
    
    # Adjust filter selections based on if 'All' is selected (remove selections other than 'all') ################
    observeFilterInput <- function(inputId) {
      observeEvent(input[[inputId]], {
        # Check if 'All' is selected and adjust accordingly
        if (length(input[[inputId]]) > 1) {
          if ("All" %in% input[[inputId]]) {
            updateSelectizeInput(session, inputId, selected = "All")
          }
        }
      })
    }
    observeFilterInput("type")
    observeFilterInput("pType")
    observeFilterInput("pGrp")
    observeFilterInput("param")
    observeFilterInput("proj")
    observeFilterInput("net")
    
    # Update text based on language ###########################################
    observe({
      # Update the tooltip's text
      tooltipText <- translations[id == if (input$main_choice == "map") "tooltip_visualize_map" else if (input$main_choice == "plot") "tooltip_visualize_plot", get(language$language)][[1]]
      session$sendCustomMessage(type = 'update-tooltip', message = list(id = ns("infoIcon"), title = tooltipText))
      
      # Update selectizeInputs
      # updateSelectizeInput(session, 
      #                      "type",
      #                      label = translations[id == "data_type", get(language$language)][[1]],
      #                      choices = stats::setNames(c("discrete", "continuous"),
      #                                                c(titleCase(c(translations[id == "discrete", get(language$language)][[1]], translations[id == "continuous", get(language$language)][[1]]), language$abbrev)
      #                                                )
      #                      ),
      #                      options = list(placeholder = translations[id == "optional_placeholder", get(language$language)][[1]]),
      #                      selected = NULL
      # )
      updateSelectizeInput(session, 
                           "pType",
                           label = translations[id == "param_type", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$param_types$param_type_code),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$param_types[[translations[id == "param_type_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           ),
                           options = list(placeholder = translations[id == "optional_placeholder", get(language$language)][[1]]),
                           selected = NULL
      )
      updateSelectizeInput(session, 
                           "pGrp",
                           label = translations[id == "param_group", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$param_groups$group),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$param_groups[[translations[id == "param_group_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           ),
                           options = list(placeholder = translations[id == "optional_placeholder", get(language$language)][[1]]),
                           selected = NULL
      )
      updateSelectizeInput(session,
                           "proj",
                           label = translations[id == "project", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$projects$project_id),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$projects[[translations[id == "generic_name_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           ),
                           options = list(placeholder = translations[id == "optional_placeholder", get(language$language)][[1]]),
                           selected = NULL
      )
      updateSelectizeInput(session,
                           "net",
                           label = translations[id == "network", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$networks$network_id),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$networks[[translations[id == "generic_name_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           ),
                           options = list(placeholder = translations[id == "optional_placeholder", get(language$language)][[1]]),
                           selected = NULL
      )
      updateSelectizeInput(session,
                           "param",
                           label = translations[id == "parameter", get(language$language)][[1]],
                           choices = stats::setNames(c(data$parameters$param_code),
                                                     c(titleCase(data$parameters[[translations[id == "param_name_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           ),
                           options = list(placeholder = translations[id == "mandatory_placeholder", get(language$language)]),
                           selected = character(0)
      )
      updateDateInput(session,
                      "date",
                      label = translations[id == "map_date_select", get(language$language)],
                      value = Sys.Date()
      )
      updateNumericInput(session,
                         "approx",
                         label = translations[id == "map_date_within_select", get(language$language)][[1]],
      )
      updateRadioButtons(session, 
                         "abs_rel",
                         label = NULL,
                         choices = stats::setNames(c("absolute", "relative"),
                                                   c(translations[id == "map_absolute", get(language$language)], translations[id == "map_relative", get(language$language)])
                                                   ),
                         inline = TRUE
      )
      # updateSelectizeInput(session,
      #                      "loc",
      #                      label = translations[id == "choose_locs", get(language$language)][[1]],
      #                      choices = stats::setNames(c("All", data$locations$location_id),
      #                                                c(translations[id == "all", get(language$language)][[1]], titleCase(data$locations[[translations[id == "generic_name_col", get(language$language)][[1]]]], language$abbrev)
      #                                                )
      #                      ),
      #                      selected = if (!is.null(inputs)) inputs else NULL
      # )
      updateActionButton(session,
                         "reset",
                         label = translations[id == "reset", get(language$language)][[1]]
      )
      output$instructions <- renderUI(translations[id == "view_data_instructions", get(language$language)][[1]])
      updateActionButton(session,
                         "render_map",
                         label = translations[id == "render_map", get(language$language)])
    }) # End of text updates based on language selection
    
    
    # Reset all filters when button pressed ##################################
    observeEvent(input$reset, {
      # updateSelectizeInput(session, 
      #                      "type",
      #                      choices = stats::setNames(c("discrete", "continuous"),
      #                                                c(titleCase(c(translations[id == "discrete", get(language$language)][[1]], translations[id == "continuous", get(language$language)][[1]]), language$abbrev)
      #                                                )
      #                      )
      # )
      updateSelectizeInput(session, 
                           "pType",
                           choices = stats::setNames(c("All", data$param_types$param_type_code),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$param_types[[translations[id == "param_type_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session, 
                           "pGrp",
                           choices = stats::setNames(c("All", data$param_groups$group),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$param_groups[[translations[id == "param_group_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "proj",
                           choices = stats::setNames(c("All", data$projects$project_id),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$projects[[translations[id == "generic_name_col", get(language$language)][[1]]]], language$abbrev))
                           )
      )
      updateSelectizeInput(session,
                           "net",
                           choices = stats::setNames(c("All", data$networks$network_id),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$networks[[translations[id == "generic_name_col", get(language$language)][[1]]]], language$abbrev))
                           )
      )
      updateSelectizeInput(session,
                           "param",
                           choices = stats::setNames(data$parameters$param_code,
                                                     titleCase(data$parameters[[translations[id == "param_name_col", get(language$language)][[1]]]], language$abbrev)
                                                     
                           ),
                           options = list(placeholder = translations[id == "mandatory_placeholder", get(language$language)]),
                           selected = character(0)
      )
      updateNumericInput(session,
                         "approx",
                         label = translations[id == "map_date_within_select", get(language$language)][[1]],
                         value = 1
      )
      updateRadioButtons(session, 
                         "abs_rel",
                         label = NULL,
                         choices = stats::setNames(c("absolute", "relative"),
                                                   c(translations[id == "map_absolute", get(language$language)], translations[id == "map_relative", get(language$language)])
                         ),
                         selected = "absolute",
                         inline = TRUE
      )
      # updateSelectizeInput(session, "loc", choices = stats::setNames(c("All", data$locations$location_id),
      #                                                                c(translations[id == "all", get(language$language)][[1]], titleCase(data$locations[[translations[id == "generic_name_col", get(language$language)][[1]]]], language$abbrev)
      #                                                                )
      # ))
    }) # End of observeEvent for reset filters button
    
    # Create map #############################################################
    observeEvent(input$render_map, {
      # Initial checks on parameter selection
      if (nchar(input$param) == 0) {
        modalDialog(
          title = "Error",
          "Please select a parameter to visualize.",
          easyClose = TRUE
        )
        return()
      }
      if (!input$param %in%  data$parameters$param_code) {
        modalDialog(
          title = "Error",
          "Selected parameter does not exist in the database. Make sure you're selecting a menu option.",
          easyClose = TRUE
        )
        return()
      }
      # Now check if there actually is data to map for the selected parameter on the selected date and date approximation
      # If there is no data, return a modal dialog
      timeseries <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE parameter = ", input$param, ";"))
      continuous_data <- DBI::dbGetQuery(con, "SELECT ")
      discrete_data <- DBI::dbGetQuery(con, )
    }) # End of observeEvent for map creation
    
  }) # End of moduleServer
}
