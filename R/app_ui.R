#' The hydroApp User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {

  #set up for background colors
  jsCode <- '
    shinyjs.backgroundCol = function(params) {
      var defaultParams = {
        id : null,
        col : "red"
      };
      params = shinyjs.getParams(params, defaultParams);

      var el = $("#" + params.id);
      el.css("background-color", params.col);
    }'

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      tags$head(
        tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
        tags$link(rel = "manifest", href = "manifest.json"),
        tags$link(rel = "apple-touch-icon", href = "icon.png"),
        tags$link(rel = "icon", type = "image/png", href = "icon.png"),
        tags$script(src = "serviceworker.js", type = "text/javascript"),
        tags$style(type='text/css', ".selectize-dropdown-content {max-height: 400px; }")
      ),
      shinyjs::extendShinyjs(text = jsCode, functions = c("backgroundCol")),

      # Title
      titlePanel("Yukon Water Science and Stewardship Branch Hydro App"),
      # Broad category selection
      selectInput("first_selection", "Choose a task", choices = c("View hydromet plots + data", "View precipitation maps + data", "Browse FOD comments")),
      conditionalPanel(
        condition = "input.first_selection == 'Browse FOD comments'",
        sidebarPanel(
          selectInput("comment_type", "Select comment type", choices = c("General comments", "Location-specific comments"), selected = "General comments"),
          selectInput("comment_data_type", "Select a data type", choices = c("All", "Water levels", "Water flows", "Bridge freeboard", "Snow pillows", "Precipitation")),
          dateInput("comment_start_date", "Start date (inclusive)", value = Sys.Date()-7, max = Sys.Date()-1),
          dateInput("comment_end_date", "End date (inclusive)",value = Sys.Date(), max = Sys.Date()),
          actionButton("FOD_go", "Load data")
        ),
        mainPanel(
          DT::dataTableOutput("FOD_table"),
          downloadButton("export_fod_comments", "Export as .csv")
        )
      ),
      conditionalPanel(
        condition = "input.first_selection == 'View precipitation maps + data'",
        sidebarPanel(
          selectizeInput("precip_loc_code", "Select watershed by code", choices = ""),
          selectizeInput("precip_loc_name", "Select watershed by name", choices = ""),
          shinyWidgets::airDatepickerInput("precip_start", "Start date/time MST", value = as.POSIXct(round(.POSIXct(Sys.time() - 60*60*24*7, tz = "MST"), "hours")), timepicker = TRUE, maxDate = Sys.Date()+3, startView = Sys.Date(), update_on = "close", timepickerOpts = shinyWidgets::timepickerOptions(hoursStep = 1, minutesStep = 30, timeFormat = "HH:mm"), todayButton = TRUE),
          shinyWidgets::airDatepickerInput("precip_end", "End date/time MST", value = as.POSIXct(round(.POSIXct(Sys.time(), tz = "MST"), "hours")), timepicker = TRUE, maxDate = Sys.Date()+3, startView = Sys.Date(), update_on = "close", timepickerOpts = shinyWidgets::timepickerOptions(hoursStep = 1, minutesStep = 30, timeFormat = "HH:mm"), todayButton = TRUE),
          checkboxInput("show_map", "Render map?"),
          actionButton("precip_go", "Calculate precip"),
          textOutput("time_adj_note"),
          htmlOutput("results_head"),
          textOutput("start_time"),
          textOutput("end_time"),
          textOutput("mean"),
          textOutput("min"),
          textOutput("max"),
          textOutput("watershed_area")
        ),
        mainPanel(
          htmlOutput("standby"),
          plotOutput("precip_map", height = "700px"),
          downloadButton("export_precip_map", "Export as png")
        )
      ),
      conditionalPanel(
        condition = "input.first_selection == 'View hydromet plots + data'",
        sidebarPanel(
          shinyWidgets::radioGroupButtons("plot_data_type", "Data type", choices = c("Continuous", "Discrete"), selected = "Continuous"),
          selectizeInput("plot_param", label = "Plotting parameter", choices = c("Level", "Flow", "Bridge freeboard", "SWE", "Snow depth")),
          selectizeInput("plot_loc_code", "Select location by code", choices = ""),
          selectizeInput("plot_loc_name", "Select location by name", choices = ""),
          dateInput("start_doy", "Start day-of-year", value = paste0(lubridate::year(Sys.Date()), "-01-01")),
          dateInput("end_doy", "End day-of-year", value = paste0(lubridate::year(Sys.Date()), "-12-31")),
          textOutput("plot_years_note"),
          shinyWidgets::pickerInput("plot_years", "Select years to plot", choices = "", multiple = TRUE, options = list("max-options" = 10,
                                                                                                                        "max-options-text" = "Cannot plot more than 10 lines")),
          shinyWidgets::pickerInput("discrete_plot_type", "Select plot type", choices = c("Violin plot", "Box plot"), selected = "Violin plot"),
          selectizeInput("historic_range", "Historic range to today or last year plotted?", choices = c("all", "last"), selected = "all"),
          selectizeInput("return_periods", "Plot return periods?", choices = c("none", "auto select", "calculate", "from table"), selected = "auto select"),
          shinyWidgets::pickerInput("return_type", "Select return type", choices = c("Min", "Max"), selected = "Max"),
          numericInput("return_yrs", "Last year for return calculations", value = lubridate::year(Sys.Date()), 1900, 2100, 1),
          textInput("return_months", "Months for return calculation (comma delimited)", value = "5,6,7,8,9"),
          checkboxInput("apply_datum", "Apply vertical datum?"),
          checkboxInput("plot_filter", "Filter extreme values?"),
          actionButton("plot_go", "Render plot")
        ),
        mainPanel(
          plotOutput("hydro_plot", height = "600px"),
          downloadButton("export_hydro_plot", "Export as png"),
          downloadButton("export_plot_data", "Export data as .csv")
        )
      )
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "hydroApp"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyjs::useShinyjs()
  )
}
