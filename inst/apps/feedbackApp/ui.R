#' User feedback app UI
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {

  fluidPage(
    titlePanel("Water Data Explorer Feedback"),
    textInput("user", "Name or Email (optional)"),
    textAreaInput("bugs", "Did you encounter any bugs? If yes, please describe what the bug was and how it was triggered.", "", rows = 3),
    textAreaInput("ui_comment", "Do you have any comments or concerns about the user interface in general?", "", rows = 3),
    textAreaInput("module_comment", "Do you have comments about any particular module (page)?", "", rows = 3),
    radioButtons("got_data", "Did the app give you the data or visualization you were seeking?", choices = c("Yes", "No")),
    textAreaInput("missing", "What was missing?", "", rows = 3),
    textAreInput("addtional_comments", "Do you have any additional comments?", "", rows = 3),
    actionButton("submit", "Submit Feedback"),
    textOutput("thanks")
  )
}
