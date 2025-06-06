#' User feedback app server
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {

  conn <- AquaConnect(???)

  if (!DBI::dbExistsTable(conn, "feedback")) {
    DBI::dbExecute(conn, "CREATE TABLE feedback (timestamp TEXT, user TEXT, bugs TEXT, ui_comment TEXT, module_comment TEXT, got_data TEXT, missing TEXT)")
  }

  observeEvent(input$???, {
   if (input$???) {
     shinyjs::show("missing")
     } else {
     shinyjs::hide("missing")
  })

  observeEvent(input$submit, {
    info <- data.frame(
      timestamp = as.character(Sys.time()),
      user = input$user,
      bugs = input$bugs,
      ui_comment = input$ui_comment,
      module_comment = input$module_comment,
      got_data = input$got_data,
      missing = input$missing,
      stringsAsFactors = FALSE
    )
    DBI::dbWriteTable(conn, "feedback", info, append = TRUE)
    output$thanks <- renderText("Thank you for your feedback!")
    updateTextInput(session, "user", value = "")
    updateTextAreaInput(session, "bugs", value = "")
    updateTextAreaInput(session, "ui_comment", value = "")
    updateTextAreaInput(session, "module_comment", value = "")
    updateRadioButtons(session, "got_data", selected = character(0))
    updateTextAreaInput(session, "missing", value = "")
    updateCheckboxInput(session, "???", value = TRUE)
    shinyjs::hide("missing")
  })

  session$onSessionEnded(function() {
    DBI::dbDisconnect(conn)
  })
}
