YGwater_globals <- function() {
  
  library(shiny)
  library(shinyjs)
  
  source(system.file("apps/YGwater/modules/admin.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/admin/new_ts_loc/new_ts_loc.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/admin/metadata/metadata_main.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/admin/basins/basins_main.R", package = "YGwater"))

  source(system.file("apps/YGwater/modules/visualize.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/plot/plot_main.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/plot/discrete.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/plot/continuous.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/plot/mix.R", package = "YGwater"))
  
  source(system.file("apps/YGwater/modules/visualize/map/map_main.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/map/precip.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/map/params.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/FOD/FOD_main.R", package = "YGwater"))
  
  # Establish database connection
  # This is being done at the server level instead for YGwater Since the app will be used infrequently, this ensures that connections are only made when a user opens the app and is closed when the user closes the app.

  
}

