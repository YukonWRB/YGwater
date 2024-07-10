#' Original Shiny application (hydroApp) for WRB data viewing
#' 
#' @description
#' `r lifecycle::badge('stable')`
#' 
#' The original WRB Shiny application. Not intended for public viewing.
#'
#' @param host Host address. Leave default to run locally, set to "0.0.0.0" to enable others to connect. Depends on the port specified in `port` to be open on the host machine.
#' @param port Port number (numeric). Leave default to use the default port specified in your user options. The port you specify must be open on the host machine for it to broadcast to the network.
#' @param dev Set to TRUE to run with the dev database. Default is FALSE.
#' 
#' @return Opens a Shiny application.
#' @export

hydroApp <- function(host = getOption("shiny.host", "127.0.0.1"), port = getOption("shiny.port"), dev = FALSE) {
  
  rlang::check_installed("shiny", reason = "required to use hydroApp.")
  rlang::check_installed("shinyjs", reason = "required to use hydroApp.")
  rlang::check_installed("shinyWidgets", reason = "required to use hydroApp.")
  rlang::check_installed("shinyalert", reason = "required to use hydroApp.")
  rlang::check_installed("DT", reason = "required to use hydroApp.")

  appDir <- system.file("apps/hydroApp", package = "YGwater")

  # Load the global variables. Contains modules as well as call to pool::pool() for connection to WRB database, library calls, and loads the translations data.table.
  source(system.file("apps/hydroApp/hydroApp_globals.R", package = "YGwater"))
  hydroApp_globals(dev = dev)
  
  if (appDir == "") {
    stop("hydroApp app not found.")
  }
  
  shiny::runApp(appDir, display.mode = "normal", host = host, port = port)
}
