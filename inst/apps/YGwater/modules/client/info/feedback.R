

feedbackUI <- function(id) {
  ns <- NS(id)
  
  page_fluid(
    titlePanel("Water Data Explorer Feedback"),
    htmlOutput(ns("intro_text")),
    # add horizontal line
    tags$hr(),
    tags$br(),
    textInput(ns("user"), "Name or Email (optional)"),
    textAreaInput(ns("bugs"), "Did you encounter any bugs? If yes, please describe what the bug was and how it was triggered.", rows = 3, width = "100%"),
    textAreaInput(ns("ui_comment"), "Do you have any comments or concerns about the user interface in general?", "", rows = 3, width = "100%"),
    textAreaInput(ns("module_comment"), "Do you have comments about any particular module (page)?", "", rows = 3, width = "100%"),
    radioButtons(ns("got_data"), "Did the app give you the data or visualization you were seeking?", choices = c("Yes", "No"), selected = "Yes", width = "100%"),
    textAreaInput(ns("missing"), "What was missing?", "", rows = 3, width = "100%"),
    textAreaInput(ns("addtional_comments"), "Do you have any additional comments?", "", rows = 3, width = "100%"),
    actionButton(ns("submit"), "Submit Feedback"),
    textOutput(ns("thanks"))
  )
}

#' User feedback app server
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

feedback <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  ns <- session$ns  # Used to create UI elements within the server code
  
  output$intro_text <- renderUI({
    HTML("Welcome to the user testing feedback page! We appreciate your input to help us improve the Water Data Explorer. Please fill out the form below to share your experience.<br><br>NOTE: this page will be deleted once internal and external user testing is complete.<br>")
  })
  
  observeEvent(input$got_data, {
    if (input$got_data == "No") {
      shinyjs::show("missing")
    } else {
      shinyjs::hide("missing")
    }
  })
  
  observeEvent(input$submit, {
    info <- data.frame(
      timestamp = .POSIXct(Sys.time(), tz = "UTC"),
      username = if (nchar(input$user) > 0) input$user else "NOT SPECIFIED",
      bugs = if (nchar(input$bugs) > 0) input$bugs else NA,
      ui_comment = if (nchar(input$ui_comment) > 0) input$ui_comment else NA,
      module_comment = if (nchar(input$module_comment) > 0) input$module_comment else NA,
      got_data = if (input$got_data == "Yes") TRUE else FALSE,
      missing = if (nchar(input$missing > 0)) input$missing else NA,
      stringsAsFactors = FALSE
    )
    DBI::dbWriteTable(session$userData$AquaCache, "feedback", info, append = TRUE)
    output$thanks <- renderText("Thank you for your feedback!")
    updateTextInput(session, "user", value = "")
    updateTextAreaInput(session, "bugs", value = "")
    updateTextAreaInput(session, "ui_comment", value = "")
    updateTextAreaInput(session, "module_comment", value = "")
    updateRadioButtons(session, "got_data", selected = character(0))
    updateTextAreaInput(session, "missing", value = "")
    updateCheckboxInput(session, "got_data", value = "Yes")
    shinyjs::hide("missing")
  })
  
  }) # end moduleServer
} # end feedback function


