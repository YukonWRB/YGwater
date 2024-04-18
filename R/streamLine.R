#' StreamLine Shiny application for WRB data viewing
#' 
#' @description
#' `r lifecycle::badge('experimental')`
#' 
#' The in-development public-facing Shiny application for viewing WRB data. Not intended for public viewing yet, but please experiment!.
#'
#' @param host Host address. Leave default to run locally, set to "0.0.0.0" to enable others to connect. Depends on the port specified in `port` to be open on the host machine.
#' @param port Port number (numeric). Leave default to use the default port specified in your user options. The port you specify must be open on the host machine for it to broadcast to the network.
#' @param browser Open the application in a browser window right away (TRUE), or in a R window (FALSE). Default is TRUE.
#' @param display.mode The display mode for the application. Default is "normal". See `shiny::runApp()` for more information.
#' 
#' @return Opens a Shiny application.
#' @export
#'

streamLine <- function(host = getOption("shiny.host", "127.0.0.1"), port = getOption("shiny.port"), browser = TRUE, display.mode = "normal") {
  
  rlang::check_installed("shiny", reason = "required to use streamLine application.")
  rlang::check_installed("shinyjs", reason = "required to use streamLine application.")
  rlang::check_installed("shinythemes", reason = "required to use streamLine application.")
  rlang::check_installed("leaflet", reason = "required to use streamLine application.")
  rlang::check_installed("pool", reason = "required to use streamLine application.")
  rlang::check_installed("htmlwidgets", reason = "required to use streamLine application.")
  
  
  appDir <- system.file("apps/streamLine", package = "YGwater")
  source(system.file("apps/streamLine/app_globals.R", package = "YGwater"))
  
  if (appDir == "") {
    stop("StreamLine app not found.")
  }
  
  shiny::enableBookmarking(store = "url")
  shiny::runApp(appDir, display.mode = display.mode, host = host, port = port, launch.browser = browser, quiet = FALSE)
}
