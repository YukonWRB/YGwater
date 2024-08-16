vizUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      textInput(ns("text"), "Text"),
      actionButton(ns("action"), "Action")
    ),
    mainPanel(
      textOutput(ns("text_output"))
    )
  )
}

viz <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$text, {
      print("observed")
      output$text_output <- renderText({
        input$text
      })
    })
    observeEvent(input$action, {
      output$text_output <- renderText({
        paste("Action: ", input$text)
      })
    })
  })
}
