YGwater_globals <- function(dbName, dbHost, dbPort, dbUser, dbPass, RLS_user, RLS_pass, accessPath) {
  
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
  
  source(system.file("apps/YGwater/modules/visualize/generate/generate_main.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/generate/basins.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/generate/WQReport.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/generate/snowReport.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/generate/waterReport.R", package = "YGwater"))
  
  source(system.file("apps/YGwater/modules/visualize/map/map_main.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/map/precip.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/map/params.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/FOD/FOD_main.R", package = "YGwater"))
  
  # Establish database connection parameters
  # The actual connection is being done at the server level instead for YGwater Since the app will be used infrequently, this ensures that connections are only made when a user opens the app and is closed when the user closes the app.
  config <<- list(
    dbName = dbName,
    dbHost = dbHost,
    dbPort = dbPort,
    dbUser = dbUser,
    dbPass = dbPass,
    RLS_user = RLS_user,
    RLS_pass = RLS_pass,
    accessPath = accessPath
  )
}

