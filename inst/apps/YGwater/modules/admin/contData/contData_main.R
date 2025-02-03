# UI and server code for main continuous data module

contDataUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    textOutput(ns("placeholder"))
  )
}

contData <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$placeholder <- renderText("Placeholder for continuous data main module")
    
    
  }) # End of moduleServer
}
