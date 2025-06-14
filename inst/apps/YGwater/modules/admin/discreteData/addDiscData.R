# UI and server code for main discrete data module

addDiscDataUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    textOutput(ns("placeholder"))
  )
}

addDiscData <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$placeholder <- renderText("Placeholder for discrete data addition module")
    
    
  }) # End of moduleServer
}
