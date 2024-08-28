discretePlotUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(ns = ns,
                       condition = "input.plot_type == 'Discrete'",
                       # Toggle for data source
                       radioButtons(ns("data_source"),
                                    NULL,
                                    choices = stats::setNames(c("AC", "EQ"), c("AquaCache", "EQWin")),
                                    selected = "AC"),
                       
                       # start and end datetime
                       dateRangeInput(ns("date_range"),
                                      "Select date range",
                                      start = Sys.Date() - 30,
                                      end = Sys.Date(),
                                      max = Sys.Date() + 1,
                                      format = "yyyy-mm-dd"),
                       
                       # Toggle button for locations or location groups (only show if data source  == EQWin)
                       radioButtons(ns("locs_groups"),
                                    NULL,
                                    choices = c("Locations", "Location Groups"),
                                    selected = "Locations"),
                       # Selectize input for locations, populated once connection is established
                       selectizeInput(ns("locations"),
                                      "Select locations",
                                      choices = "Placeholder",
                                      multiple = TRUE),
                       # Selectize input for location groups, populated once connection is established. only shown if data source is EQWin
                       selectizeInput(ns("location_groups"),
                                      "Select a location group",
                                      choices = "Placeholder",
                                      multiple = FALSE),
                       
                       # Toggle button for parameters or parameter groups (only show if data source == EQWin)
                       radioButtons(ns("params_groups"),
                                    NULL,
                                    choices = c("Parameters", "Parameter Groups"),
                                    selected = "Parameters"),
                       # Selectize input for parameters, populated once connection is established
                       selectizeInput(ns("parameters"),
                                      "Select parameters",
                                      choices = "Placeholder",
                                      multiple = TRUE),
                       # Selectize input for parameter groups, populated once connection is established. only shown if data source is EQWin
                       selectizeInput(ns("parameter_groups"),
                                      "Select a parameter group",
                                      choices = "Placeholder",
                                      multiple = FALSE),
                       radioButtons(ns("facet_on"),
                                    "Facet on",
                                    choices = stats::setNames(c("locs", "params"), c("Locations", "Parameters")),
                                    selected = "locs"),
                       checkboxInput(ns("log_scale"),
                                     "Log scale"),
                       checkboxInput(ns("target_datetime"),
                                     "Use target instead of actual datetime"),
                       checkboxInput(ns("colorblind"),
                                     "Colorblind friendly"),
                       radioButtons(ns("lang"),
                                    NULL,
                                    choices = stats::setNames(c("en", "fr"), c("English", "French")),
                                    selected = "en"),
                       actionButton(ns("make_plot"),
                                    "Create Plot")
      ),
    )
  )
  
}

discretePlotServer <- function(id) {
  
}
