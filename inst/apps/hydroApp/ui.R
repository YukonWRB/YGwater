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
    fluidPage(
      tags$head(
        tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
        tags$style(type = 'text/css', ".selectize-dropdown-content {max-height: 400px; }")
      ),
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(text = jsCode, functions = c("backgroundCol")),

      # Title
      titlePanel("Yukon Water Science and Stewardship Branch Hydro App"),
      # Broad category selection
      selectInput("first_selection", "Choose a task", choices = c("View hydrometric plots + data", "View precipitation maps + data", "Browse FOD comments")),
      conditionalPanel(
        condition = "input.first_selection == 'Browse FOD comments'",
        sidebarPanel(
          selectInput("comment_type", "Select comment type", choices = c("General comments", "Location-specific comments"), selected = "General comments"),
          selectInput("comment_data_type", "Select a data type", choices = c("All", "Water levels", "Water flows", "Bridge freeboard", "Snow pillows", "Precipitation")),
          dateInput("comment_start_date", "Start date (inclusive)", value = Sys.Date() - 7, max = Sys.Date() - 1),
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
          shinyWidgets::airDatepickerInput("precip_start", "Start date/time MST", value = as.POSIXct(round(.POSIXct(Sys.time() - 60 * 60 * 24 * 7, tz = "MST"), "hours")), timepicker = TRUE, maxDate = Sys.Date() + 3, startView = Sys.Date(), update_on = "close", timepickerOpts = shinyWidgets::timepickerOptions(hoursStep = 1, minutesStep = 30, timeFormat = "HH:mm"), todayButton = TRUE),
          shinyWidgets::airDatepickerInput("precip_end", "End date/time MST", value = as.POSIXct(round(.POSIXct(Sys.time(), tz = "MST"), "hours")), timepicker = TRUE, maxDate = Sys.Date() + 3, startView = Sys.Date(), update_on = "close", timepickerOpts = shinyWidgets::timepickerOptions(hoursStep = 1, minutesStep = 30, timeFormat = "HH:mm"), todayButton = TRUE),
          checkboxInput("show_map", "Render map?"),
          radioButtons("map_type", "Map type", choices = c("Static", "Dynamic"), selected = "Dynamic"),
          actionButton("precip_go", "Calculate precip"),
          textOutput("time_adj_note"),
          htmlOutput("results_head"),
          textOutput("start_time"),
          textOutput("end_time"),
          textOutput("mean"),
          textOutput("min"),
          textOutput("max")
        ),
        mainPanel(
          htmlOutput("standby"),
          leaflet::leafletOutput("precip_map_leaflet", width = "auto", height = "700px"), # Render the map (leaflet)
          imageOutput("precip_map", width = "auto", height = "auto"),
          tags$div(style = "text-align: center; margin-top: 20px; margin-bottom: 20px",  # Center the button below the image
                   downloadButton("export_precip_map", "Export as png")
          )
        )
      ),
      conditionalPanel(
        condition = "input.first_selection == 'View hydrometric plots + data'",
        sidebarPanel(
          shinyWidgets::radioGroupButtons("plot_data_type", "Data type", choices = c("Continuous", "Discrete"), selected = "Continuous"),
          selectizeInput("plot_type", label = "Plot type", choices = c("Overlapping years", "Long timeseries", "Multi timeseries", "Binned", "Scatter")), #Discrete plot types are selected in the server
          selectizeInput("plot_sub_type", label = "Plot sub-type", choices = c("Violin plot", "Box plot", "Line-box plot"), selected = "Violin plot"),
          selectizeInput("plot_param", label = "Plotting parameter", choices = "placeholder"), #Choices are selected in the server
          selectizeInput("plot_loc_code", "Select location by code", choices = "placeholder"), #Choices are selected in the server
          selectizeInput("plot_loc_name", "Select location by name", choices = "placeholder"), #Choices are selected in the server
          numericInput("lead_lag", "Lead/lag in hours", value = 0),
          dateInput("start_doy", "Start day-of-year", value = paste0(lubridate::year(Sys.Date()), "-01-01")), # Only used for plotOverlap, turned on/off by shinyjs
          dateInput("end_doy", "End day-of-year", value = paste0(lubridate::year(Sys.Date()), "-12-31")), # Only used for plotOverlap, turned on/off by shinyjs
          dateInput("start_date", "Start date", value = Sys.Date() - 365, max = Sys.Date() - 1), # Only used for plotTimeseries, turned on/off by shinyjs
          dateInput("end_date", "End date", value = Sys.Date(), max = Sys.Date()), # Only used for plotTimeseries, turned on/off by shinyjs
          textOutput("plot_years_note"),
          shinyWidgets::pickerInput("plot_years", "Select years to plot", choices = "", multiple = TRUE, options = list("max-options" = 10, "max-options-text" = "Cannot plot more than 10 lines")), # Only used for plotTimeseries, turned on/off by shinyjs
          selectizeInput("historic_range_overlap", "Historic range includes all years of record or up to last year plotted?", choices = c("all", "last"), selected = "all"), # Only used for plotOverlap, turned on/off by shinyjs
          selectizeInput("return_periods", "Plot return periods?", choices = c("none", "auto select", "calculate", "from table"), selected = "auto select"),
          shinyWidgets::pickerInput("return_type", "Select return type", choices = c("Min", "Max"), selected = "Max"),
          numericInput("return_yrs", "Last year for return calculations", value = lubridate::year(Sys.Date()), 1900, 2100, 1),
          textInput("return_months", "Months for return calculation (comma delimited)", value = "5,6,7,8,9"),
          actionButton("add_trace2", "Add a trace"), # Only used for plotMulti, turned on/off by shinyjs
          selectizeInput("plot_param2", label = "Plotting parameter", choices = ""), #Choices are selected in the server
          selectizeInput("plot_loc_code2", "Select location by code", choices = ""), #Choices are selected in the server
          selectizeInput("plot_loc_name2", "Select location by name", choices = ""), #Choices are selected in the server
          numericInput("lead_lag2", "Lead/lag in hours", value = 0),
          actionButton("remove_trace2", "Remove this trace"),
          actionButton("add_trace3", "Add a trace"), # Only used for plotMulti, turned on/off by shinyjs
          selectizeInput("plot_param3", label = "Plotting parameter", choices = ""), #Choices are selected in the server
          selectizeInput("plot_loc_code3", "Select location by code", choices = ""), #Choices are selected in the server
          selectizeInput("plot_loc_name3", "Select location by name", choices = ""), #Choices are selected in the server
          numericInput("lead_lag3", "Lead/lag in hours", value = 0),
          actionButton("remove_trace3", "Remove this trace"),
          actionButton("add_trace4", "Add a trace"), # Only used for plotMulti, turned on/off by shinyjs
          selectizeInput("plot_param4", label = "Plotting parameter", choices = ""), # Choices are selected in the server
          selectizeInput("plot_loc_code4", "Select location by code", choices = ""), # Choices are selected in the server
          selectizeInput("plot_loc_name4", "Select location by name", choices = ""), # Choices are selected in the server
          numericInput("lead_lag4", "Lead/lag in hours", value = 0),
          actionButton("remove_trace4", "Remove this trace"),
          checkboxGroupInput("log_y", "Log scale y-axis?", c("Trace 1" = 1)),
          checkboxInput("historic_range", "Plot historic range?"), # Only used for plotTimeseries and plotMultiTimeseries, turned on/off by shinyjs"
          checkboxInput("apply_datum", "Apply vertical datum?"),
          checkboxInput("plot_filter", "Filter extreme values?"),
          actionButton("plot_go", "Render plot")
        ),
        mainPanel(
          plotOutput("hydro_plot", height = "600px"),
          plotly::plotlyOutput("hydro_plotly", height = "600px"),
          downloadButton("export_hydro_plot", "Export as png"),
          downloadButton("export_plot_data", "Export data as .csv")
        )
      )
    )
  )
}
