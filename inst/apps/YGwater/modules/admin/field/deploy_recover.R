# UI and server code for main equipment module

deploy_recover_UI <- function(id) {
  ns <- NS(id)
  page_fluid(
    uiOutput(ns("banner")),
    textOutput(ns("placeholder"))
  )
}

deploy_recover <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "deploy_recover"
      )
    })

    output$placeholder <- renderText(
      "Placeholder for equipment deloyment/recovery"
    )
  }) # End of moduleServer
}
