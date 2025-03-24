# UI and server code for main equipment module

deploy_recover_UI <- function(id) {
  ns <- NS(id)
  fluidPage(
    textOutput(ns("placeholder"))
  )
}

deploy_recover <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$placeholder <- renderText("Placeholder for equipment deloyment/recovery")
    
    
  }) # End of moduleServer
}
