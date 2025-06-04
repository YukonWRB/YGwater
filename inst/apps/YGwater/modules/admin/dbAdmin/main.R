# UI and server code for main locations view/edit module

locsMainUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    textOutput(ns("placeholder"))
  )
}

locsMainServer <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$placeholder <- renderText("Placeholder for main locations view/edit module")
    
  }) # End of moduleServer
}
