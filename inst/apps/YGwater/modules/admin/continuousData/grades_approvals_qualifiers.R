# UI and server code for adding/modifying grades, approvals, qualifiers

grades_approvals_qualifiersUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    textOutput(ns("placeholder"))
  )
}

grades_approvals_qualifiers <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$placeholder <- renderText("Placeholder for continuous grade/approval/qualifier application/edit module")
    
    
  }) # End of moduleServer
}
