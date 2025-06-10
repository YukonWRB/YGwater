# UI and server code for main timeseries module

tsUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    textOutput(ns("placeholder"))
  )
}

ts <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$placeholder <- renderText("Placeholder for timeseries main module")
    
    
  }) # End of moduleServer
}
