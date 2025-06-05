#' Launch the YGwater feedback application
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This application collects feedback from internal users about the YGwater app.
#' Feedback is stored in a local SQLite database.
#'
#' @param db_path Path to the SQLite database file where feedback will be stored.
#'   Defaults to "feedback.sqlite" in the current working directory.
#' @param host Host address. Defaults to the value of `getOption('shiny.host', '127.0.0.1')`.
#' @param port Port number. Defaults to `getOption('shiny.port')`.
#' @param server Set to TRUE to run on Shiny Server, otherwise FALSE to run locally.
#'
#' @return Opens the feedback Shiny application.
#' @export

YGwater_feedback <- function(db_path = "feedback.sqlite",
                             host = getOption("shiny.host", "127.0.0.1"),
                             port = getOption("shiny.port"),
                             server = FALSE) {

  rlang::check_installed("shiny", reason = "required to use feedback app")
  rlang::check_installed("RSQLite", reason = "required to store feedback")

  appDir <- system.file("apps/feedbackApp", package = "YGwater")

  if (appDir == "") {
    stop("Feedback app not found.")
  }

  source(system.file("apps/feedbackApp/feedback_globals.R", package = "YGwater"))
  feedback_globals(db_path = db_path)

  if (server) {
    shiny::shinyAppDir(appDir)
  } else {
    shiny::runApp(appDir, display.mode = "normal", host = host, port = port)
  }
}
