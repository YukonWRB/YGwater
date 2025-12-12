# UI and server code for locations metadata view/edit module

locsMetaUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    textOutput(ns("placeholder"))
  )
}

locsMetaServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$placeholder <- renderText(
      "Placeholder for location metadata view/edit module"
    )
  }) # End of moduleServer
}
