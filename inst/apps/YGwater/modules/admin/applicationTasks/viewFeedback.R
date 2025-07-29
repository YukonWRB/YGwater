viewFeedbackUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    DT::DTOutput(ns("feedback_table"))
  )
}

viewFeedback <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    check <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT has_table_privilege(current_user, 'application.feedback_temp', 'SELECT') AS can_select"
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
      "SELECT * FROM application.feedback_temp ORDER BY timestamp DESC"
    ))
    
    output$feedback_table <- DT::renderDT({
      DT::datatable(feedback_data(), rownames = FALSE, options = list(scrollX = TRUE))
    })
  })
}
