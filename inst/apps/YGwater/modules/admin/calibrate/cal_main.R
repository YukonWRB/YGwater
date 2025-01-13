# UI and server code for main calibrations module

calUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    textOutput(ns("placeholder"))
  )
}

cal <- function(id, AquaCache) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$placeholder <- renderText("Placeholder for calibrations main module")
    
    
  }) # End of moduleServer
}
