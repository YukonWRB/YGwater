YGwater_globals <- function(dbName, dbHost, dbPort, dbUser, dbPass, RLS_user, RLS_pass, accessPath) {
  
  library(shiny)
  library(shinyjs)
  
  # 'Admin' side modules ###
  source(system.file("apps/YGwater/modules/admin/admin.R", package = "YGwater"))
  
  source(system.file("apps/YGwater/modules/admin/locations/loc_main.R", package = "YGwater"))
  # locations sub-modules
  source(system.file("apps/YGwater/modules/admin/timeseries/ts_main.R", package = "YGwater"))
  # timeseries sub-modules
  source(system.file("apps/YGwater/modules/admin/equipment/equip_main.R", package = "YGwater"))
  # equipment sub-modules
  source(system.file("apps/YGwater/modules/admin/calibrate/cal_main.R", package = "YGwater"))
  # calibration sub-modules
  source(system.file("apps/YGwater/modules/admin/contData/contData_main.R", package = "YGwater"))
  # continuous data sub-modules
  source(system.file("apps/YGwater/modules/admin/discData/discData_main.R", package = "YGwater"))
  # discrete data sub-modules
  source(system.file("apps/YGwater/modules/admin/field/field_main.R", package = "YGwater"))
  # Field visit sub-modules
  source(system.file("apps/YGwater/modules/admin/documents/docs_main.R", package = "YGwater"))
  # Document sub-modules
  source(system.file("apps/YGwater/modules/admin/images/imgs_main.R", package = "YGwater"))
  # Document sub-modules
  
  # The two file references below need to be removed when their code is incorporated into the new structure:
  # source(system.file("apps/YGwater/modules/admin/add/add_main.R", package = "YGwater"))
  # source(system.file("apps/YGwater/modules/admin/images/addImg.R", package = "YGwater"))


  
  # 'Visualize' side modules ####
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

