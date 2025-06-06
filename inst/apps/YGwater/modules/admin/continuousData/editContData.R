# UI and server code for discrete data edits

editContDataUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    textOutput(ns("placeholder"))
  )
}

editContData <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$placeholder <- renderText("Placeholder for continuous data modify/delete module")
    
    
  }) # End of moduleServer
}
