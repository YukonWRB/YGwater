YGwater_globals <- function(dbName, dbHost, dbPort, dbUser, dbPass, RLS_user, RLS_pass, accessPath1, accessPath2, public) {
  
  library(shiny)
  library(shinyjs)
  library(bslib)

  # Use a user-writable cache directory for sass
  cache_dir <- tools::R_user_dir("YGwater", "cache")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  options(bslib.sass.cache = cache_dir)

  # Initialize a shared cache environment available to all sessions
  if (!exists("app_cache", envir = .GlobalEnv)) {
    assign("app_cache", new.env(parent = emptyenv()), envir = .GlobalEnv)
  }
  
  # Load the cache functions (in a file so they can be used across a few modules)
  source(system.file("apps/YGwater/modules/cache_functions.R", package = "YGwater"))
  
  # 'Admin' side modules #####
  # database admin modules
  source(system.file("apps/YGwater/modules/admin/locations/locationMetadata.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/admin/locations/addLocation.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/admin/locations/addSubLocation.R", package = "YGwater"))
  
  # equipment sub-modules
  source(system.file("apps/YGwater/modules/admin/equipment/deploy_recover.R", package = "YGwater"))
  
  # calibration sub-modules
  source(system.file("apps/YGwater/modules/admin/equipment/calibrate.R", package = "YGwater"))
  
  # continuous data sub-modules
  source(system.file("apps/YGwater/modules/admin/continuousData/addContData.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/admin/continuousData/continuousCorrections.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/admin/continuousData/imputeMissing.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/admin/continuousData/editContData.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/admin/continuousData/grades_approvals_qualifiers.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/admin/continuousData/addTimeseries.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/admin/continuousData/syncCont.R", package = "YGwater"))
  
  # discrete data sub-modules
  source(system.file("apps/YGwater/modules/admin/discreteData/addDiscData.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/admin/discreteData/editDiscData.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/admin/discreteData/syncDisc.R", package = "YGwater"))
  
  
  source(system.file("apps/YGwater/modules/admin/field/field_main.R", package = "YGwater"))
  
  # Files/document/image sub-modules
  source(system.file("apps/YGwater/modules/admin/documents/addDocs.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/admin/imgupload/addImgs.R", package = "YGwater"))
  
  source(system.file("apps/YGwater/modules/admin/applicationTasks/manageNewsContent.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/admin/applicationTasks/viewFeedback.R", package = "YGwater"))
  
  
  # 'client' side modules #####
  # Plot modules
  source(system.file("apps/YGwater/modules/client/plot/discretePlot.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/client/plot/continuousPlot.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/client/plot/mixPlot.R", package = "YGwater"))
  
  # Report modules
  source(system.file("apps/YGwater/modules/client/reports/basins.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/client/reports/WQReport.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/client/reports/snowInfo.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/client/reports/waterInfo.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/client/reports/snowBulletin.R", package = "YGwater"))
  
  # Map modules
  source(system.file("apps/YGwater/modules/client/map/precip.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/client/map/params.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/client/map/raster.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/client/map/locations.R", package = "YGwater"))
  
  # Image modules
  source(system.file("apps/YGwater/modules/client/images/image_table_view.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/client/images/image_map_view.R", package = "YGwater"))
  
  # Data modules
  source(system.file("apps/YGwater/modules/client/data/continuousData.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/client/data/discreteData.R", package = "YGwater"))
  
  # Info modules
  source(system.file("apps/YGwater/modules/client/info/home.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/client/info/news.R", package = "YGwater"))
  source(system.file("apps/YGwater/modules/client/info/about.R", package = "YGwater"))
  
  
  source(system.file("apps/YGwater/modules/client/info/feedback.R", package = "YGwater"))  # !!! delete this line when the feedback module is removed
  
  # FOD module (only visible internally)
  source(system.file("apps/YGwater/modules/client/FOD/FOD_main.R", package = "YGwater"))
  
  
  
  # Load translations infrastructure to the global environment
  
  translations <- openxlsx::read.xlsx(system.file("apps/YGwater/translations.xlsx", package = "YGwater"), sheet = 1)
  # Build a list from the data.frame
  translation_cache <<- lapply(setdiff(names(translations[, -2]), "id"), function(lang) { # Removes the second, "description" column, builds lists for each language
    setNames(translations[[lang]], translations$id)
  })
  names(translation_cache) <<- setdiff(names(translations)[-2], "id")
  
  # Make a helper function, send to global environment
  tr <<- function(key, lang) {
    # Ensure that 'key' is a value in the 'id' column of the translations data.frame
    if (!key %in% translations$id) {
      stop(paste("Translation key", key, "not found in translations data."))
    }
    translation_cache[[lang]][[key]]  # list 'lang', item 'key'
  }
  
  
  # Establish database connection parameters
  # The actual connection is being done at the server level and stored in session$userData$AquaCache. This allows using a login input form to connect to the database with edit privileges or to see additional elements
  # double assignment creates a global variable that can be accessed by all UI and server functions
  
  # confirm G drive access
  g_drive <- dir.exists("//env-fs/env-data/corp/water")
  
  config <<- list(
    dbName = dbName,
    dbHost = dbHost,
    dbPort = dbPort,
    dbUser = dbUser,
    dbPass = dbPass,
    accessPath1 = accessPath1,
    accessPath2 = accessPath2,
    public = public,
    g_drive = g_drive,
    admin = FALSE,
    sidebar_bg = "#FFFCF5", # Default background color for all sidebars
    main_bg = "#D9EFF2" # Default background color for all main panels
  )
  
  # Load the YG BS 5 theme
  app_theme <<- bslib::bs_theme(version = 5) %>%
    bs_add_rules(paste(readLines(system.file("apps/YGwater/www/css/YG_bs5.css", package = "YGwater")), collapse = "\n"))
  
}

