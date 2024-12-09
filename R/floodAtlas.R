#' Shiny application for internal WRB use
#' 
#' @description
#' `r lifecycle::badge('experimental')`
#' 
#' This application is designed for use by WRB or other YG staff, and is designed to allow greater flexibility and tune ability of plots and outputs than the public facing app (Streamline). In addition, certain portions of the application interface with databases other than aquacache such as EQWin or the snow database, and may allow for edit privileges to these databases. This app is also used to roll out new, experimental features before they are added to Streamline.
#'
#' @param host Host address. Leave default to run locally, set to "0.0.0.0" to enable others to connect. Depends on the port specified in `port` to be open on the host machine.
#' @param port Port number (numeric). Leave default to use the default port specified in your user options. The port you specify must be open on the host machine for it to broadcast to the network.
#' @param dbName Name of the aquacache database. Default is "aquacache".
#' @param dbHost Host address of the aquacache database. Default is pulled from the .Renviron file.
#' @param dbPort Port number of the aquacache database. Default is pulled from the .Renviron file.
#' @param dbUser Username for the aquacache database. Default is pulled from the .Renviron file.
#' @param dbPass Password for the aquacache database. Default is pulled from the .Renviron file.
#' 
#' @return Opens a Shiny application.
#' @export

floodAtlas <- function(host = getOption("shiny.host", "127.0.0.1"), port = getOption("shiny.port"), dbPort = Sys.getenv("aquacachePort"), dbName = "aquacache", dbHost = Sys.getenv("aquacacheHost"), dbUser = Sys.getenv("aquacacheUser"), dbPass = Sys.getenv("aquacachePass")) {
  
  rlang::check_installed("shiny", reason = "required to use floodAtlas app")
  rlang::check_installed("shinyjs", reason = "required to use floodAtlas app")
  
  appDir <- system.file("apps/floodAtlas", package = "YGwater")
  
  if (appDir == "") {
    stop("floodAtlas app not found.")
  }
  
  # Load the global variables, library calls, and possibly in future a connection to the DB.
  source(system.file("apps/floodAtlas/floodAtlas_globals.R", package = "YGwater"))
  floodAtlas_globals(dbName = dbName, dbHost = dbHost, dbPort = dbPort, dbUser = dbUser, dbPass = dbPass)
  
  
  
  shiny::runApp(appDir, display.mode = "normal", host = host, port = port)
}
