YGwater_globals <- function(
  dbName,
  dbHost,
  dbPort,
  dbUser,
  dbPass,
  RLS_user,
  RLS_pass,
  accessPath1,
  accessPath2,
  public
) {
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
  source(system.file(
    "apps/YGwater/modules/cache_functions.R",
    package = "YGwater"
  ))

  g_drive <- FALSE

  if (!public) {
    # confirm G drive access for FOD reports
    g_drive <- dir.exists(
      "//env-fs/env-data/corp/water/Hydrology/03_Reporting/Conditions/tabular_internal_reports/"
    )

    # 'Admin' side modules #####
    # database admin modules
    source(system.file(
      "apps/YGwater/modules/admin/locations/locationMetadata.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/locations/addLocation.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/locations/addSubLocation.R",
      package = "YGwater"
    ))

    # equipment sub-modules
    source(system.file(
      "apps/YGwater/modules/admin/equipment/calibrate.R",
      package = "YGwater"
    ))

    # continuous data sub-modules
    source(system.file(
      "apps/YGwater/modules/admin/continuousData/addContData.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/continuousData/continuousCorrections.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/continuousData/imputeMissing.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/continuousData/editContData.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/continuousData/grades_approvals_qualifiers.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/continuousData/addTimeseries.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/continuousData/syncCont.R",
      package = "YGwater"
    ))

    # discrete data sub-modules
    source(system.file(
      "apps/YGwater/modules/admin/discreteData/addDiscData.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/discreteData/addSamples.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/discreteData/editDiscData.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/discreteData/addSampleSeries.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/discreteData/addGuidelines.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/discreteData/syncDisc.R",
      package = "YGwater"
    ))

    # Borehole/well modules
    source(system.file(
      "apps/YGwater/modules/admin/boreholes_wells/simplerIndex.R",
      package = "YGwater"
    ))

    # Field visit modules
    source(system.file(
      "apps/YGwater/modules/admin/field/field_visit.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/field/deploy_recover.R",
      package = "YGwater"
    ))

    # Files/document/image sub-modules
    source(system.file(
      "apps/YGwater/modules/admin/documents/addDocs.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/imgupload/addImgs.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/imgupload/addImgSeries.R",
      package = "YGwater"
    ))

    source(system.file(
      "apps/YGwater/modules/admin/applicationTasks/manageNewsContent.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/applicationTasks/viewFeedback.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/users/manageUsers.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/admin/users/changePassword.R",
      package = "YGwater"
    ))

    if (g_drive) {
      # FOD module (only visible internally)
      source(system.file(
        "apps/YGwater/modules/client/FOD/FOD_main.R",
        package = "YGwater"
      ))
    }

    # Set up a temporary directory for storing R documentation files during app runtime
    .rd_dir <<- file.path(tempdir(), "rdocs")
    dir.create(.rd_dir, showWarnings = FALSE, recursive = TRUE)
    shiny::addResourcePath("rdocs", .rd_dir)

    # Increase the maximum upload size to 100 MB, necessary for some admin modules (NOTE that a change to NGINX parameters is also necessary)
    options(shiny.maxRequestSize = 1024 * 1024^2)

    # define some functions for later use
    parse_share_with <<- function(value) {
      if (is.null(value) || !length(value) || all(is.na(value))) {
        return(character())
      }
      if (is.list(value)) {
        value <- value[[1]]
      }
      value <- gsub("[{}\"]", "", value)
      out <- trimws(unlist(strsplit(value, ",")))
      out[nzchar(out)]
    }

    format_share_with <<- function(groups) {
      if (is.null(groups) || !length(groups) || all(!nzchar(groups))) {
        groups <- "public_reader"
      }
      groups <- gsub('"', '\\"', groups, fixed = TRUE)
      paste0("{", paste(sprintf('"%s"', groups), collapse = ","), "}")
    }
  }

  # 'client' side modules #####
  # Plot modules
  source(system.file(
    "apps/YGwater/modules/client/plot/discretePlot.R",
    package = "YGwater"
  ))
  source(system.file(
    "apps/YGwater/modules/client/plot/continuousPlot.R",
    package = "YGwater"
  ))

  # Non-public client-side modules
  if (!public) {
    # Old plot modules (kept for backward compatibility)
    source(system.file(
      "apps/YGwater/modules/client/plot/continuousPlot_old.R",
      package = "YGwater"
    ))

    # Report modules
    if (g_drive) {
      source(system.file(
        "apps/YGwater/modules/client/reports/WQReport.R",
        package = "YGwater"
      ))
      source(system.file(
        "apps/YGwater/modules/client/reports/snowBulletin.R",
        package = "YGwater"
      ))
    }
    source(system.file(
      "apps/YGwater/modules/client/reports/snowInfo.R",
      package = "YGwater"
    ))
    source(system.file(
      "apps/YGwater/modules/client/reports/waterInfo.R",
      package = "YGwater"
    ))
  }

  # Map modules
  source(system.file(
    "apps/YGwater/modules/client/map/paramsMap.R",
    package = "YGwater"
  ))
  source(system.file(
    "apps/YGwater/modules/client/map/rasterMap.R",
    package = "YGwater"
  ))
  source(system.file(
    "apps/YGwater/modules/client/map/locationsMap.R",
    package = "YGwater"
  ))
  source(system.file(
    "apps/YGwater/modules/client/map/snowBulletinMap.R",
    package = "YGwater"
  ))
  source(system.file(
    "apps/YGwater/modules/client/WWR/registry_front_end.R",
    package = "YGwater"
  ))

  # Image modules
  source(system.file(
    "apps/YGwater/modules/client/images/image_table_view.R",
    package = "YGwater"
  ))
  source(system.file(
    "apps/YGwater/modules/client/images/image_map_view.R",
    package = "YGwater"
  ))

  # Data modules
  source(system.file(
    "apps/YGwater/modules/client/data/continuousData.R",
    package = "YGwater"
  ))
  source(system.file(
    "apps/YGwater/modules/client/data/discreteData.R",
    package = "YGwater"
  ))

  # Info modules
  source(system.file(
    "apps/YGwater/modules/client/info/home.R",
    package = "YGwater"
  ))
  source(system.file(
    "apps/YGwater/modules/client/info/news.R",
    package = "YGwater"
  ))
  source(system.file(
    "apps/YGwater/modules/client/info/about.R",
    package = "YGwater"
  ))

  # Establish database connection parameters
  # The actual connection to AquaCache is being done at the server level and stored in session$userData$AquaCache. This allows using a login input form to connect to the database with edit privileges or to see additional elements

  ## Access database connections ###########
  # Look for .mdb files in the AccessPath directories
  if (g_drive) {
    if (!is.null(accessPath1)) {
      if (dir.exists(accessPath1) & !public) {
        # List the *.mdb files in the directory
        mdb_files1 <- list.files(
          accessPath1,
          pattern = "*.mdb",
          full.names = TRUE
        )
        if (length(mdb_files1) == 0) {
          mdb_files1 <- NULL
        }
      } else {
        mdb_files1 <- NULL
      }
    } else {
      mdb_files1 <- NULL
    }
    if (!is.null(accessPath2)) {
      if (dir.exists(accessPath2) & !public) {
        # List the *.mdb files in the directory
        mdb_files2 <- list.files(
          accessPath2,
          pattern = "*.mdb",
          full.names = TRUE
        )
        if (length(mdb_files2) == 0) {
          mdb_files2 <- NULL
        }
      } else {
        mdb_files2 <- NULL
      }
    } else {
      mdb_files2 <- NULL
    }

    mdb_files <- c(mdb_files1, mdb_files2)

    if (is.null(mdb_files) & !public) {
      print("No .mdb files found in the accessPath directories.")
    }
  } else {
    mdb_files <- NULL
  }

  # Make the configuration list available globally
  # double assignment creates a global variable that can be accessed by all UI and server functions

  config <<- list(
    dbName = dbName,
    dbHost = dbHost,
    dbPort = dbPort,
    dbUser = dbUser,
    dbPass = dbPass,
    public = public,
    g_drive = g_drive,
    mdb_files = mdb_files,
    admin = FALSE,
    sidebar_bg = "#FFFCF5", # Default background color for all sidebars
    main_bg = "#D9EFF2" # Default background color for all main panels
  )
}
