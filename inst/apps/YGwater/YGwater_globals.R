YGwater_globals <- function() {
  
  library(shiny)
  library(shinyjs)
  
  source(system.file("apps/YGwater/modules/checks.R", package = "YGwater"))
  
  source(system.file("apps/YGwater/modules/plot/plot_main.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/plot/discrete.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/plot/continuous.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/plot/mix.R", package = "YGwater"))
  
  source(system.file("apps/YGwater/modules/map/map_main.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/map/precip.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/map/params.R", package = "YGwater"))
  
  source(system.file("apps/YGwater/modules/FOD/FOD_main.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/basins/basins_main.R", package = "YGwater"))
  
  # Establish database connection
  # This is being done at the server level instead for YGwater Since the app will be used infrequently, this ensures that connections are only made when a user opens the app and is closed when the user closes the app.

  
}

