
newsUI <- function(id) {
  ns <- NS(id)
  verticalLayout(
    # tags$div(style = "height: 10px;"),

    uiOoutput(ns("news"))
    
  )
}

news <- function(id, language, restoring) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$news <- renderUI({
      
    })
  })
}
