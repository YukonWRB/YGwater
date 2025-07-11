# UI and server code for discrete data edit/delete module

editDiscDataUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    textOutput(ns("placeholder"))
  )
}

editDiscData <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$placeholder <- renderText("Placeholder for discrete data modify/delete module")
    
    
  }) # End of moduleServer
}
