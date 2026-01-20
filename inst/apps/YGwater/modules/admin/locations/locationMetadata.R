# UI and server code for locations metadata view/edit module

locsMetaUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    uiOutput(ns("banner")),
    textOutput(ns("placeholder"))
  )
}

locsMetaServer <- function(id, data, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "locsMeta"
      )
    })

    output$placeholder <- renderText(
      "Placeholder for location metadata view/edit module"
    )
  }) # End of moduleServer
}
