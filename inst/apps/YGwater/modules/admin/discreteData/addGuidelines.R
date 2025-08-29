# UI and server code for water quality guidelines management module

addGuidelinesUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    textOutput(ns("placeholder"))
  )
}

addGuidelines <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$placeholder <- renderText("Placeholder for adding/removing water quality guidelines")
    
    
  }) # End of moduleServer
}
