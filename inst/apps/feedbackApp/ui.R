#' User feedback app UI
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {

  fluidPage(
    titlePanel("YGwater Feedback"),
    textInput("user", "Name or Email (optional)"),
    textAreaInput("bugs", "Did you encounter any bugs? If yes, please describe:", "", rows = 3),
    textAreaInput("ui_comment", "Comments about the user interface:", "", rows = 3),
    textAreaInput("module_comment", "Comments about any particular module:", "", rows = 3),
    radioButtons("got_data", "Did the app give you the data or visualization you were seeking?", choices = c("Yes", "No")),
    textAreaInput("missing", "If not, what was missing?", "", rows = 3),
    actionButton("submit", "Submit Feedback"),
    textOutput("thanks")
  )
}
