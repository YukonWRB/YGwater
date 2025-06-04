# UI and server code for main continuous data module

addContDataUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    textOutput(ns("placeholder"))
  )
}

addContData <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$placeholder <- renderText("Placeholder for continuous data adition module")
    
    
  }) # End of moduleServer
}
