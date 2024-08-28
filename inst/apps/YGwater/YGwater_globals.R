YGwater_globals <- function() {
  
  library(shiny)
  library(shinyjs)
  
  source(system.file("apps/YGwater/modules/checks.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/plot/plot_main.R", package = "YGwater"))
  # source(system.file("apps/YGwater/modules/plot/discrete.R", package = "YGwater"))
  
  
  # Establish database connection
  # This is being done at the server level instead for YGwater Since the app will be used infrequently, this ensures that connections are only made when a user opens the app and is closed when the user closes the app.

  
}

