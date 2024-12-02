YGwater_globals <- function(dbName, dbHost, dbPort, dbUser, dbPass, RLS_user, RLS_pass, accessPath) {
  
  library(shiny)
  library(shinyjs)
  
  source(system.file("apps/YGwater/modules/admin/admin.R", package = "YGwater"))
  
  source(system.file("apps/YGwater/modules/admin/addTsLoc/addTsLoc.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/admin/metadata/metadata_main.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/admin/basins/basins_main.R", package = "YGwater"))
  # source(system.file("apps/YGwater/modules/admin/images/addImg.R", package = "YGwater"))

  
  source(system.file("apps/YGwater/modules/visualize/visualize.R", package = "YGwater"))
  
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
  source(system.file("apps/YGwater/modules/visualize/map/locations.R", package = "YGwater"))
  
  source(system.file("apps/YGwater/modules/visualize/images/image_view.R", package = "YGwater"))
  
  source(system.file("apps/YGwater/modules/visualize/FOD/FOD_main.R", package = "YGwater"))
  
  
  
  # Load translations infrastructure
  translations <<- data.table::setDT(openxlsx::read.xlsx(system.file("apps/YGwater/translations.xlsx", package = "YGwater"), sheet = 1))
  
  # Establish database connection parameters
  # The actual connection is being done at the server level for YGwater. This allows using a login input form to connect to the database with edit privileges.
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

