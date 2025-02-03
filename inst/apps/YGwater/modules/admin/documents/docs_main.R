# UI and server code for main documents module

addDocsUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    textOutput(ns("placeholder"))
  )
}

addDocs <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$placeholder <- renderText("Placeholder for documents main module")
    
    
  }) # End of moduleServer
}
