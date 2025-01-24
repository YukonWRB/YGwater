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
          margin-bottom: 20px; /* Space below buttons */
        }
      "))
    ),
    
    # Hidden input fields to set default parameters
    textInput("loc_code", label = NULL, value = "09AB004"), # Marsh lk
    numericInput("param_code", label = NULL, value = 1165), # level
    textInput("lang", label = NULL, value = "en"),
    
    # Buttons and selectize input arranged in a row
    div(
      class = "button-row",
      actionButton("info", "Info"),
      selectizeInput("yrs", label = NULL, choices = as.character(format(Sys.Date(), "%Y")), selected = as.character(format(Sys.Date(), "%Y")), multiple = TRUE, options = list(maxItems = 10)),
    ),
    
    plotly::plotlyOutput("plot_plotly", width = "100%", height = "800px", inline = TRUE),
    
    htmlOutput("error")
    
  ) # End fluidPage
}
