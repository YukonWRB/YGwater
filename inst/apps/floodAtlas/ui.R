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
      tags$style(HTML("#loc_code { display: none; }")), # Hide the input by default
      tags$style(HTML("#param_code { display: none; }")),
      tags$style(HTML("#start_dt { display: none; }")),
      tags$style(HTML("#end_dt { display: none; }")),
      tags$style(HTML("#lang { display: none; }")),
      tags$style(HTML("#error { display: none; }"))
    ),
    
    # Hidden input fields to set default parameters
    textInput("loc_code", label = NULL, value = NULL),
    numericInput("param_code", label = NULL, value = NA),
    textInput("start_dt", label = NULL, value = NULL),
    textInput("end_dt", label = NULL, value = NULL),
    textInput("lang", label = NULL, value = NULL),
    
    plotly::plotlyOutput("plot_plotly", width = "100%", height = "800px", inline = TRUE),
    
    htmlOutput("error")
    
  ) # End fluidPage
}
