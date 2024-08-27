EQWinChecks_globals <- function() {
  
  library(shiny)
  library(shinyjs)
  
  source(system.file("apps/EQchecks/modules/checks.R", package = "YGwater"))
  source(system.file("apps/EQchecks/modules/viz.R", package = "YGwater"))
  
  
  # Establish database connection
  # This is being done at the server level instead for EQchecks. Since the app will be used infrequently, this ensures that connections are only made when a user opens the app and is closed when the user closes the app.

  
}

