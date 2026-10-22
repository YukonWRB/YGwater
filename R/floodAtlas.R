#' Shiny application for internal WRB use
#'
#' @description
#' This application is designed for use by WRB or other YG staff, and is designed to allow greater flexibility and tune ability of plots and outputs than the public facing app (Streamline). In addition, certain portions of the application interface with databases other than aquacache such as EQWin or the snow database, and may allow for edit privileges to these databases. This app is also used to roll out new, experimental features before they are added to Streamline.
#'
#' @param app The name of the app to run. Options are "overlap" or "timeseries".
#' @param host Host address. Leave default to run locally, set to "0.0.0.0" to enable others to connect. Depends on the port specified in `port` to be open on the host machine. Ignored if `server` is set to TRUE (Shiny Server takes care of that).
#' @param port Port number (numeric). Leave default to use the default port specified in your user options. The port you specify must be open on the host machine for it to broadcast to the network. Ignored if `server` is set to TRUE (Shiny Server takes care of that).
#' @param dbName Name of the aquacache database. Default is "aquacache".
#' @param dbHost Host address of the aquacache database. Default is pulled from the .Renviron file.
#' @param dbPort Port number of the aquacache database. Default is pulled from the .Renviron file.
#' @param dbUser Username for the aquacache database. Default is pulled from the .Renviron file.
#' @param dbPass Password for the aquacache database. Default is pulled from the .Renviron file.
#' @param server Set to TRUE to run on Shiny Server, otherwise FALSE to run locally.
#' @param workers Number of local mirai daemons. `NULL` (the default) divides the cores available to R, after reserving one, between the two FloodAtlas app processes and uses at least one daemon per app. Set explicitly to tune resource use when the apps share a host or when only one app is running.
#'
#' @return Opens a Shiny application.
#' @export

floodAtlas <- function(
  app,
  host = getOption("shiny.host", "127.0.0.1"),
  port = getOption("shiny.port"),
  dbPort = Sys.getenv("aquacachePort"),
  dbName = "aquacache",
  dbHost = Sys.getenv("aquacacheHost"),
  dbUser = Sys.getenv("aquacacheUser"),
  dbPass = Sys.getenv("aquacachePass"),
  server = FALSE,
  workers = NULL
) {
  rlang::check_installed("shiny", reason = "to use floodAtlas app")
  rlang::check_installed("shinyjs", reason = "to use floodAtlas app")
  rlang::check_installed(
    "mirai",
    reason = "to enable asynchronous operations in floodAtlas apps",
    version = "2.0.0"
  )
  rlang::check_installed(
    "promises",
    reason = "to connect mirai tasks to Shiny ExtendedTask",
    version = "1.3.0"
  )
  rlang::check_installed(
    "parallelly",
    reason = "to determine the available CPU allocation for mirai daemons"
  )
  rlang::check_installed(
    "bslib",
    reason = "to enable bootstrap 5 themes and elements in floodAtlas apps"
  )
  rlang::check_installed(
    "shinycssloaders",
    reason = "to show a 'loading' spinner in the app"
  )

  if (is.null(workers)) {
    workers <- max(1L, floor(parallelly::availableCores(omit = 1L) / 2L))
  } else if (
    !is.numeric(workers) ||
      length(workers) != 1L ||
      is.na(workers) ||
      !is.finite(workers) ||
      workers < 1 ||
      workers != floor(workers) ||
      workers > .Machine$integer.max
  ) {
    stop("`workers` must be NULL or a positive whole number.", call. = FALSE)
  }
  workers <- as.integer(workers)

  if (app == "overlap") {
    appDir <- system.file("apps/floodAtlas_over", package = "YGwater")
  } else if (app == "timeseries") {
    appDir <- system.file("apps/floodAtlas_ts", package = "YGwater")
  } else {
    stop("Invalid app name. Choose either 'overlap' or 'timeseries'.")
  }

  if (appDir == "") {
    stop("target floodAtlas app not found.")
  }

  # Load the global variables, library calls, and possibly in future a connection to the DB.
  if (app == "overlap") {
    source(system.file(
      "apps/floodAtlas_over/floodAtlas_over_globals.R",
      package = "YGwater"
    ))
    floodAtlas_over_globals(
      dbName = dbName,
      dbHost = dbHost,
      dbPort = dbPort,
      dbUser = dbUser,
      dbPass = dbPass
    )
  } else if (app == "timeseries") {
    source(system.file(
      "apps/floodAtlas_ts/floodAtlas_ts_globals.R",
      package = "YGwater"
    ))
    floodAtlas_ts_globals(
      dbName = dbName,
      dbHost = dbHost,
      dbPort = dbPort,
      dbUser = dbUser,
      dbPass = dbPass
    )
  }

  shiny::enableBookmarking("url")

  # Daemons are persistent and shared by sessions in this app process.
  compute_profile <- paste0("YGwater_floodAtlas_", app)
  mirai::daemons(n = workers, .compute = compute_profile)
  shiny::onStop(function() {
    mirai::daemons(n = 0L, .compute = compute_profile)
  })

  if (server) {
    shiny::shinyAppDir(appDir)
  } else {
    shiny::runApp(appDir, display.mode = "normal", host = host, port = port)
  }
}
