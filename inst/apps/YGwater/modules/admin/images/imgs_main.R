# UI and server code for main images module

addImgsUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    textOutput(ns("placeholder"))
  )
}

addImgs <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$placeholder <- renderText("Placeholder for images main module")
    
    
  }) # End of moduleServer
}
