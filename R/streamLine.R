#' StreamLine Shiny application for WRB data viewing
#' 
#' Loads a Shiny application for viewing WRB data.
#'
#' @param dev Logical. If TRUE, loads the development version of the app.
#' @return Opens a Shiny application.
#' @export
#'
#' @examples
#' streamLine()

streamLine <- function(dev = FALSE) {
  
  rlang::check_installed("shiny", reason = "required to use streamLine application.")
  rlang::check_installed("shinyjs", reason = "required to use streamLine application.")
  
  if (dev) {
    appDir <- system.file("apps/streamLine_dev", package = "YGwater")
    source(system.file("apps/streamLine_dev/app_globals.R", package = "YGwater"))
  } else {
    appDir <- system.file("apps/streamLine", package = "YGwater")
    source(system.file("apps/streamLine/app_globals.R", package = "YGwater"))
  }
  
  if (appDir == "") {
    stop("StreamLine app not found.")
  }
  
  enableBookmarking(store = "url")
  shiny::runApp(appDir, display.mode = "normal")
}