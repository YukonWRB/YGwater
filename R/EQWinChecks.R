#' Shiny application to facilitate QA/QC checks on EQWin data
#' 
#' @description
#' `r lifecycle::badge('experimental')`
#' 
#' Allows the user to perform automatic check on EQWin samples within a user-defined date range and, optionally, at certain locations or for certain sampleIds only.
#'
#' @param host Host address. Leave default to run locally, set to "0.0.0.0" to enable others to connect. Depends on the port specified in `port` to be open on the host machine.
#' @param port Port number (numeric). Leave default to use the default port specified in your user options. The port you specify must be open on the host machine for it to broadcast to the network.
#' 
#' @return Opens a Shiny application.
#' @export

EQWinChecks <- function(host = getOption("shiny.host", "127.0.0.1"), port = getOption("shiny.port")) {
  
  warning("The application is under development and not functional yet.")
  
  rlang::check_installed("shiny", reason = "required to use EQchecks")
  rlang::check_installed("shinyjs", reason = "required to use EQchecks")
  rlang::check_installed("DT", reason = "required to use EQchecks")
  rlang::check_installed("shinyWidgets", reason = "required to use EQchecks")
  
  appDir <- system.file("apps/EQchecks", package = "YGwater")
  
  if (appDir == "") {
    stop("EQchecks app not found.")
  }
  
  # Load the global variables, library calls, and possibly in future a connection to the DB.
  source(system.file("apps/EQchecks/EQWinChecks_globals.R", package = "YGwater"))
  EQWinChecks_globals()
  
  
  
  shiny::runApp(appDir, display.mode = "normal", host = host, port = port)
}
