viewFeedbackUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    uiOutput(ns("banner")),
    DT::DTOutput(ns("feedback_table"))
  )
}

viewFeedback <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "viewFeedback"
      )
    })

    check <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT has_table_privilege(current_user, 'application.feedback', 'SELECT') AS can_select"
    )

    if (!check$can_select) {
      showModal(modalDialog(
        title = 'Insufficient Privileges',
        'You do not have the necessary privileges to view feedback.',
        easyClose = TRUE,
        footer = modalButton('Close')
      ))
      return()
    }

    feedback_data <- reactiveVal(DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT * FROM application.feedback ORDER BY timestamp DESC"
    ))

    output$feedback_table <- DT::renderDT({
      DT::datatable(
        feedback_data(),
        rownames = FALSE,
        options = list(scrollX = TRUE)
      )
    })
  })
}
