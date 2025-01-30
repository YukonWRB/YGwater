#' The floodAtlas User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {
  
  fluidPage(
    shinyjs::useShinyjs(),

    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/fonts.css"), # YG fonts (the CSS refers to files in the www/fonts folder so there is no external dependency)
      tags$style(HTML("#loc_code { display: none; }")), # Hide the input by default
      tags$style(HTML("#param_code { display: none; }")),
      tags$style(HTML("#lang { display: none; }")),
      tags$style(HTML("#error { display: none; }")),
      # Add custom CSS for spacing and alignment
      tags$style(HTML("
        .button-row {
          display: flex;
          gap: 10px; /* Space between buttons */
          align-items: center;
          margin-bottom: 0px; /* Space below buttons */
          margin-top: 0px; /* Space above buttons */
        }
      "))
    ),
    
    # Hidden input fields to set default parameters
    textInput("loc_code", label = NULL, value = "09AB004"), # Marsh lk
    numericInput("param_code", label = NULL, value = 1165), # level
    textInput("lang", label = NULL, value = "en"),
    
    # Make visible buttons in a fluidRow
    fluidRow(
      class = "button-row",
      actionButton("info", "Info"),
      selectizeInput("yrs", label = "Add years", choices = as.character(format(Sys.Date(), "%Y")), selected = as.character(format(Sys.Date(), "%Y")), multiple = TRUE, options = list(maxItems = 10))
    ),
    # Calculate remaining height for the plot
    tags$style(HTML("
  #plot_plotly {
    height: calc(100vh - 120px) !important;
    width: 100% !important;
  }
")),
    tags$div(
      # style = "height: calc(100vh - 100px); width: 100%;",
      plotly::plotlyOutput("plot_plotly", height = "100%", width = "100%")
    ),
    htmlOutput("error")
    
  ) # End fluidPage
}
