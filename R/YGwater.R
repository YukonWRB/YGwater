#' Shiny application for internal WRB use
#' 
#' @description
#' `r lifecycle::badge('experimental')`
#' 
#' This application is designed for use by WRB or other YG staff, and is designed to allow greater flexibility and tune ability of plots and outputs than the public facing app (Streamline). In addition, certain portions of the application interface with databases other than AquaCache such as EQWin or the snow database, and may allow for edit privileges to these databases. This app is also used to roll out new, experimental features before they are added to Streamline.
#'
#' @param host Host address. Leave default to run locally, set to "0.0.0.0" to enable others to connect. Depends on the port specified in `port` to be open on the host machine.
#' @param port Port number (numeric). Leave default to use the default port specified in your user options. The port you specify must be open on the host machine for it to broadcast to the network.
#' 
#' @return Opens a Shiny application.
#' @export

YGwater <- function(host = getOption("shiny.host", "127.0.0.1"), port = getOption("shiny.port")) {
  
  rlang::check_installed("shiny", reason = "required to use YGwater app")
  rlang::check_installed("shinyjs", reason = "required to use YGwater app")
  rlang::check_installed("DT", reason = "required to use YGwater app")
  rlang::check_installed("shinyWidgets", reason = "required to use YGwater app")
  
  appDir <- system.file("apps/YGwater", package = "YGwater")
  
  if (appDir == "") {
    stop("YGwater app not found.")
  }
  
  # Load the global variables, library calls, and possibly in future a connection to the DB.
  source(system.file("apps/YGwater/YGwater_globals.R", package = "YGwater"))
  YGwater_globals()
  
  
  
  shiny::runApp(appDir, display.mode = "normal", host = host, port = port)
}
