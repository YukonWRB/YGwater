
dataUI <- function(id) {
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
    sidebarPanel(
      span(
        id = ns("infoIcon"),
        `data-toggle` = "tooltip",
        `data-placement` = "right",
        `data-trigger` = "click hover",
        title = "Placeholder",
        icon("info-circle", style = "font-size: 150%;")),
      selectizeInput(ns("type"), "Data Type", choices = c("All" = "All"), multiple = TRUE), # choices and labels are updated in the server module
      selectizeInput(ns("pType"), "Parameter Type", choices = c("All" = "All"), multiple = TRUE),
      selectizeInput(ns("pGrp"), "Parameter Group", choices = c("All" = "All"), multiple = TRUE),
      selectizeInput(ns("param"), "Parameter", choices = c("All" = "All"), multiple = TRUE),
      selectizeInput(ns("proj"), "Project", choices = c("All" = "All"), multiple = TRUE),
      selectizeInput(ns("net"), "Network", choices = c("All" = "All"), multiple = TRUE),
      sliderInput(ns("yrs"), "With data between...", sep = "", min = 1897, max = lubridate::year(Sys.Date()), value = c(1897, lubridate::year(Sys.Date())), step = 1),
      selectizeInput(ns("loc"), "Or choose one or more locations", choices = c("All" = "All"), multiple = TRUE),
      actionButton(ns("reset"), "Reset Filters")
    ),
    mainPanel(
      DT::dataTableOutput(ns("tbl")) # Table with timeseries, filtered by the sidebar inputs
    )
    
  ) # end tagList
} # end dataUI


data <- function(id, con, language, restoring, data) {
  moduleServer(id, function(input, output, session) {
    
    setBookmarkExclude(c("reset"))
    ns <- session$ns
    
    outputs <- reactiveValues()  # This allows the module to pass values back to the main server
    
    # Adjust multiple selection based on if 'All' is selected ################
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
    observeFilterInput("loc")
    
    
    # Update text (including map popup) based on language ###########################################
    observe({
      # Update the tooltip's text
      tooltipText <- translations[id == "tooltip_reset", get(language$language)][[1]]
      session$sendCustomMessage(type = 'update-tooltip', message = list(id = ns("infoIcon"), title = tooltipText))
      
      # Update selectizeInputs
      updateSelectizeInput(session, 
                           "type",
                           label = translations[id == "data_type", get(language$language)][[1]],
                           choices = stats::setNames(c("All", "discrete", "continuous"),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(c(translations[id == "discrete", get(language$language)][[1]], translations[id == "continuous", get(language$language)][[1]]), language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session, 
                           "pType",
                           label = translations[id == "param_type", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$param_types$param_type_code),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$param_types[[translations[id == "param_type_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session, 
                           "pGrp",
                           label = translations[id == "param_group", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$param_groups$group),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$param_groups[[translations[id == "param_group_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "param",
                           label = translations[id == "parameter", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$parameters$param_code),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$parameters[[translations[id == "param_name_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "proj",
                           label = translations[id == "project", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$projects$project_id),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$projects[[translations[id == "generic_name_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateSelectizeInput(session,
                           "net",
                           label = translations[id == "network", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$networks$network_id),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$networks[[translations[id == "generic_name_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateSliderInput(session,
                        "yrs",
                        label = translations[id == "year_filter", get(language$language)][[1]],
                        min = lubridate::year(min(data$timeseries$start_datetime)),
                        max = lubridate::year(max(data$timeseries$end_datetime)),
                        value = lubridate::year(c(min(data$timeseries$start_datetime), max(data$timeseries$end_datetime)))
      )
      updateSelectizeInput(session,
                           "loc",
                           label = translations[id == "choose_locs", get(language$language)][[1]],
                           choices = stats::setNames(c("All", data$locations$location_id),
                                                     c(translations[id == "all", get(language$language)][[1]], titleCase(data$locations[[translations[id == "generic_name_col", get(language$language)][[1]]]], language$abbrev)
                                                     )
                           )
      )
      updateActionButton(session,
                         "reset",
                         label = translations[id == "reset", get(language$language)][[1]]
      )
    })
    
    table_data <- reactive({
      req(input$type, input$pType, input$pGrp, input$param, input$proj, input$net, input$yrs, input$loc)
      # apply the filters one by one to get the timeseries remaining.
      if (!is.null(input$loc) & input$loc != "All") {
        tbl <- data.table::copy(data$timeseries)[location_id %in% input$loc]
        
        # Attach location
        tbl[data$locations, on = c(location_id = "location_id"), translations[id == "loc", get(language$language)] := .(get(translations[id == "generic_name_col", get(language$language)]))]
        tbl[, location_id := NULL]
        
        # Attach parameter type
        tbl[data$param_types, on = c(param_type = "param_type_code"), 
            translations[id == "param_type", get(language$language)] := get(translations[id == "param_type_col", get(language$language)])]
        tbl[, param_type := NULL]
        
        # Attach parameter descriptions
        tbl[data$parameters, on = c(parameter = "param_code"), 
            c(translations[id == "group", get(language$language)], translations[id == "parameter", get(language$language)], translations[id == "units", get(language$language)]) 
            := 
              .(get(translations[id == "param_group_col", get(language$language)]), get(translations[id == "param_name_col", get(language$language)]), unit)]
        tbl[, parameter := NULL]
        
        # Modify category to Category and change text
        
        # apply titleCase to all char columns
        
        # Drop period_type
        tbl[, period_type := NULL]
        
        tbl

      } else {
        
      }
    })
    
    observe({
      tbl <- table_data()
    })
    
  })
}
