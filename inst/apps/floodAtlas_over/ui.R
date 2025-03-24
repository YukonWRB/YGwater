#' The floodAtlas overlap User-Interface
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
      tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css"), # YG styled buttons
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
    
    # Make visible buttons in a fluidRow (rendered in server)
    uiOutput("visible_buttons"),
    
    # Calculate remaining height for the plot
    tags$style(HTML("
  #plot {
    height: calc(100vh - 120px) !important;
    width: 100% !important;
  }
")),
    tags$div(
      # style = "height: calc(100vh - 100px); width: 100%;",
      plotly::plotlyOutput("plot", height = "100%", width = "100%")
    ),
    htmlOutput("error")
    
  ) # End fluidPage
}
