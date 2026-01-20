# UI and server code for main documents module

addDocsUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    uiOutput(ns("banner")),
    textOutput(ns("placeholder"))
  )
}

addDocs <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "addDocs"
      )
    })

    output$placeholder <- renderText("Placeholder for documents main module")
  }) # End of moduleServer
}
