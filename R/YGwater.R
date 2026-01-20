#' Launch the YGwater Shiny application
#'
#' @description
#' Launches a Shiny application to visualize the WRB's water-related data in maps and plots, and to download data from the WRB's hydrometric database. The application is designed to work with the WRB's aquacache database, which contains hydrometric data, and optionally with EQWin databases for water quality data.
#'
#' @param host Host address. Leave default to run locally, set to "0.0.0.0" to enable others to connect. Depends on the port specified in `port` to be open on the host machine. Ignored if `server` is set to TRUE (Shiny Server takes care of that).
#' @param port Port number (numeric) on which to serve the app. Leave default to use the default port specified in your user options. The port you specify must be open on the host machine for it to broadcast to the network. Ignored if `server` is set to TRUE (Shiny Server takes care of that).
#' @param dbName Name of the aquacache database. Default is "aquacache".
#' @param dbHost Host address of the aquacache database. Default is pulled from the .Renviron file.
#' @param dbPort Port number of the aquacache database. Default is pulled from the .Renviron file.
#' @param dbUser Username for the aquacache database. Default is pulled from the .Renviron file.
#' @param dbPass Password for the aquacache database. Default is pulled from the .Renviron file.
#' @param accessPath1 to the folder where EQWin databases are stored. Default is "//env-fs/env-data/corp/water/Data/Databases_virtual_machines/databases/EQWinDB". The function will search for all *.mdb files in this folder (but not its sub-folders) and list them as options.
#' @param accessPath2 Path to the folder where EQWin databases are stored. Default is "//carver/infosys/EQWin". The function will search for all *.mdb files in this folder (but not its sub-folders) and list them as options, combined with the files in accessPath1.
#' @param server Set to TRUE to run on Shiny Server, otherwise FALSE to run locally.
#' @param public TRUE restricts or doesn't create some UI elements that are not intended for public use, such as a login button and some crude report generation modules. Default is TRUE.
#'
#' @return Opens a Shiny application.
#' @export

YGwater <- function(
  host = getOption("shiny.host", "127.0.0.1"),
  port = getOption("shiny.port"),
  dbName = Sys.getenv("aquacachenName", "aquacache"),
  dbHost = Sys.getenv("aquacacheHost"),
  dbPort = Sys.getenv("aquacachePort", "5432"),
  dbUser = Sys.getenv("aquacacheUser"),
  dbPass = Sys.getenv("aquacachePass"),
  accessPath1 = "//env-fs/env-data/corp/water/Data/Databases_virtual_machines/databases/EQWinDB",
  accessPath2 = "//carver/infosys/EQWin",
  server = FALSE,
  public = TRUE
) {
  rlang::check_installed("shiny", reason = "required to use YGwater app")
  rlang::check_installed("shinyjs", reason = "required to use YGwater app")
  rlang::check_installed("DT", reason = "required to use YGwater app")
  rlang::check_installed("tidyhydat", reason = "required to use YGwater app")
  rlang::check_installed("htmltools", reason = "required to use YGwater app")
  rlang::check_installed(
    "future",
    reason = "required to enable asynchronous operations in YGwater apps"
  )
  rlang::check_installed(
    "bslib",
    reason = "required to enable bootstrap 5 themes and elements in YGwater apps"
  )
  rlang::check_installed("bsicons", reason = "required to use YGwater app")
  rlang::check_installed("exifr", reason = "required to use YGwater app")
  rlang::check_installed("leaflet", reason = "required to use YGwater app")
  rlang::check_installed("zip", reason = "required to use YGwater app")
  rlang::check_installed("htmlwidgets", reason = "required to use YGwater app")
  rlang::check_installed("jsonlite", reason = "required to use YGwater app")
  rlang::check_installed(
    "base64enc",
    reason = "to create base64-encoded plot images"
  )

  if (!rlang::is_installed("AquaCache")) {
    rlang::check_installed(
      "remotes",
      reason = "required to install AquaCache from GitHub"
    )
    rlang::check_installed(
      "AquaCache",
      reason = "required to use YGwater app",
      action = \(pkg, ...) remotes::install_github("YukonWRB/AquaCache")
    )
  }

  if (!public) {
    rlang::check_installed(
      "shinyWidgets",
      reason = "required to use YGwater app with public = FALSE"
    )
    rlang::check_installed(
      "respR",
      reason = "required to use YGwater app with public = FALSE"
    )
    rlang::check_installed(
      "pdftools",
      reason = "required to use YGwater app with public = FALSE"
    )
    rlang::check_installed(
      "tesseract",
      reason = "required to use Simpler Index in the app"
    )
    rlang::check_installed(
      "magick",
      reason = "required to use Simpler Index in the app"
    )
  }

  appDir <- system.file("apps/YGwater", package = "YGwater")

  if (appDir == "") {
    stop("YGwater app not found.")
  }

  # Load the global variables, library calls, and possibly in future a connection to the DB.
  source(system.file("apps/YGwater/YGwater_globals.R", package = "YGwater"))
  YGwater_globals(
    dbName = dbName,
    dbHost = dbHost,
    dbPort = dbPort,
    dbUser = dbUser,
    dbPass = dbPass,
    accessPath1 = accessPath1,
    accessPath2 = accessPath2,
    public = public
  )

  # Connect and check that the database has the required tables/schemas; disconnect immediately afterwards because connections are made in app
  # Connection via AquaCache will also check for updates to apply to the database schema
  con <- AquaCache::AquaConnect(
    name = dbName,
    host = dbHost,
    port = dbPort,
    username = dbUser,
    password = dbPass,
    silent = TRUE
  )

  # Check that the DB has the 'application' schema
  if (
    !DBI::dbExistsTable(con, "page_content", schema = "application") ||
      !DBI::dbExistsTable(con, "notifications", schema = "application")
  ) {
    stop(
      "The database does not have the required 'application' schema, or is at minimum missing the 'page_content' or 'notifications' tables. You'll need to bring the AquaCache database up to revision 31 at minimum.",
      appDir,
      "."
    )
  }

  # Check that the connection can see a few tables: 'timeseries', 'locations', 'parameters', 'measurements_continuous_calculated', 'samples', 'results'
  if (!DBI::dbExistsTable(con, "timeseries")) {
    stop(
      "The user you're connecting with can't see the table 'timeseries'. This table is required for the app to function."
    )
  }
  if (!DBI::dbExistsTable(con, "locations")) {
    stop(
      "The user you're connecting with can't see the table 'locations'. This table is required for the app to function."
    )
  }
  if (!DBI::dbExistsTable(con, "parameters")) {
    stop(
      "The user you're connecting with can't see the table 'parameters'. This table is required for the app to function."
    )
  }
  if (!DBI::dbExistsTable(con, "measurements_continuous_corrected")) {
    stop(
      "The user you're connecting with can't see the view table 'measurements_continuous_corrected'. This table is required for the app to function."
    )
  }
  if (!DBI::dbExistsTable(con, "samples")) {
    stop(
      "The user you're connecting with can't see the table 'samples'. This table is required for the app to function."
    )
  }
  if (!DBI::dbExistsTable(con, "results")) {
    stop(
      "The user you're connecting with can't see the table 'results'. This table is required for the app to function."
    )
  }
  #... there are other necessary tables, but if the user has access to the ones listed above everything is probably fine.

  # Check the necessary DB version number
  ver <- as.numeric(DBI::dbGetQuery(
    con,
    "SELECT version FROM information.version_info WHERE item = 'Last patch number';"
  )[1, 1])
  if (ver < 31) {
    stop(
      "The aquacache database version is too old. Please update to at least version 31. Current version is ",
      ver,
      ". DB updates are done by updating the AquaCache R package and creating a new connection as admin or postgres user. Refer to the AquaCache::AquaConnect documentation for more details."
    )
  }

  # Disconnect from the database
  DBI::dbDisconnect(con)

  # shiny::enableBookmarking(store = "url")  # Enable bookmarking

  # Set up for ExtendedTasks or promises
  # If on Windows OR running interactively, use multisession, else use multicore
  if (Sys.info()["sysname"] == "Windows" | interactive()) {
    future::plan("multisession")
  } else {
    future::plan("multicore")
  }
  if (server) {
    shiny::shinyAppDir(appDir)
  } else {
    shiny::runApp(appDir, display.mode = "normal", host = host, port = port)
  }
}
