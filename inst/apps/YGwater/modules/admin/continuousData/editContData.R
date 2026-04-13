# UI and server code for discrete data edits

editContDataUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    uiOutput(ns("banner")),
    textOutput(ns("placeholder"))
  )
}

editContData <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "editContData"
      )
    })

    output$placeholder <- renderText(
      "Placeholder for continuous data modify/delete module"
    )
  }) # End of moduleServer
}
