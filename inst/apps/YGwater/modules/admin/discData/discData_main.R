# UI and server code for main discrete data module

discDataUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    textOutput(ns("placeholder"))
  )
}

discData <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$placeholder <- renderText("Placeholder for discrete data main module")
    
    
  }) # End of moduleServer
}
