YGwater_globals <- function(dbName, dbHost, dbPort, dbUser, dbPass, RLS_user, RLS_pass, accessPath, public) {
  
  library(shiny)
  library(shinyjs)

  # 'Admin' side modules ###
  source(system.file("apps/YGwater/modules/admin/admin.R", package = "YGwater"))
  
  source(system.file("apps/YGwater/modules/admin/locations/loc_main.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/admin/locations/main.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/admin/locations/meta.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/admin/locations/new_loc.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/admin/locations/new_ts.R", package = "YGwater"))
  
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


  
  # 'Visualize' side modules ####
  source(system.file("apps/YGwater/modules/visualize/visualize.R", package = "YGwater"))
  
  source(system.file("apps/YGwater/modules/visualize/plot/plot_main.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/plot/discrete.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/plot/continuous.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/plot/mix.R", package = "YGwater"))
  
  source(system.file("apps/YGwater/modules/visualize/reports/reports_main.R", package = "YGwater"))
  # source(system.file("apps/YGwater/modules/visualize/reports/basins.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/reports/WQReport.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/reports/snowInfo.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/reports/waterInfo.R", package = "YGwater"))
  
  source(system.file("apps/YGwater/modules/visualize/map/map_main.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/map/precip.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/map/params.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/map/locations.R", package = "YGwater"))
  
  source(system.file("apps/YGwater/modules/visualize/images/image_view.R", package = "YGwater"))
  
  source(system.file("apps/YGwater/modules/visualize/info/home.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/info/news.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/visualize/info/about.R", package = "YGwater"))
  
  
  source(system.file("apps/YGwater/modules/visualize/FOD/FOD_main.R", package = "YGwater"))
  
  
  
  # Load translations infrastructure to the global environment
  translations <<- data.table::setDT(openxlsx::read.xlsx(system.file("apps/YGwater/translations.xlsx", package = "YGwater"), sheet = 1))
  
  
  # New method to gradually move to:
  translations <- openxlsx::read.xlsx(system.file("apps/YGwater/translations.xlsx", package = "YGwater"), sheet = 1)
  # Build a list from the data.frame
  translation_cache <<- lapply(setdiff(names(translations[, -2]), "id"), function(lang) { # Removes the second, "description" column, builds lists for each language
    setNames(translations[[lang]], translations$id)
  })
  names(translation_cache) <<- setdiff(names(translations)[-2], "id")

  # Make a helper function, send to global environment
  tr <<- function(key, lang) {
    translation_cache[[lang]][[key]]  # list 'lang', item 'key'
  }
  
  # When testing, the function option is ~300 times faster than the data.table option
  
  
  # Establish database connection parameters
  # The actual connection is being done at the server level for YGwater. This allows using a login input form to connect to the database with edit privileges or to see additional elements
  # double assignment creates a global variable that can be accessed by all UI and server functions
  
  # confirm G drive access
  g_drive <- dir.exists("//env-fs/env-data/corp/water")
  
  config <<- list(
    dbName = dbName,
    dbHost = dbHost,
    dbPort = dbPort,
    dbUser = dbUser,
    dbPass = dbPass,
    accessPath = accessPath,
    public = public,
    g_drive = g_drive,
    admin = FALSE
  )
  
}

