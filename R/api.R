#' Run the YGwater API
#'
#' @description
#' Launches the YGwater API using the plumber package.
#'
#' @param version The API version to use. Default is NULL, which uses the latest version.
#' @param host The host address to bind the server to. Default is the local host.
#' @param port The port number to listen on. Default is 8000.
#' @param server The server path for the API. Default is '/water-data/api'.
#' @param dbName The name of the PostgreSQL database. Default is taken from the environment variable 'aquacacheName'.
#' @param dbHost The host address of the PostgreSQL database. Default is taken from the environment variable 'aquacacheHost'.
#' @param dbPort The port number of the PostgreSQL database. Default is taken from the environment variable 'aquacachePort'.
#' @param dbUser The username for the PostgreSQL database. Default is taken from the environment variable 'aquacacheUser'.
#' @param dbPass The password for the PostgreSQL database. Default is taken from the environment variable 'aquacachePass'.
#' @param run Whether to run the API immediately. Default is TRUE. Set to FALSE for testing purposes.
#' @return Runs the API server.
#' @export
#' @examples
#' \dontrun{
#' api()
#' }

api <- function(
  version = NULL,
  host = "127.0.0.1",
  port = 8000,
  server = "/water-data/api",
  dbName = Sys.getenv("aquacacheName"),
  dbHost = Sys.getenv("aquacacheHost"),
  dbPort = Sys.getenv("aquacachePort"),
  dbUser = Sys.getenv("aquacacheUser"),
  dbPass = Sys.getenv("aquacachePass"),
  run = TRUE
) {
  rlang::check_installed("plumber", reason = "required to run a plumber API")
  rlang::check_installed("memoise", reason = "required to run a plumber API")
  rlang::check_installed("cachem", reason = "required to run a plumber API")

  if (!is.null(version)) {
    api_path <- system.file(
      paste0("plumber/v", version, ".R"),
      package = "YGwater"
    )
  } else {
    # Find the latest version available and use it
    api_files <- list.files(
      system.file("plumber", package = "YGwater"),
      pattern = "^v[0-9]+\\.R$"
    )
    if (length(api_files) == 0) {
      stop("No API versions found in the package.")
    }
    versions <- as.numeric(gsub("v([0-9]+)\\.R", "\\1", api_files))
    latest_version <- max(versions, na.rm = TRUE)
    api_path <- system.file(
      paste0("plumber/v", latest_version, ".R"),
      package = "YGwater"
    )
  }

  if (api_path == "") {
    stop("YGwater API not found.")
  }
  pr <- plumber::plumb(api_path)

  # Set environment variables for database connection
  # username and password are handled via Basic Authentication or use the default read-only user
  Sys.setenv(APIaquacacheName = dbName)
  Sys.setenv(APIaquacacheHost = dbHost)
  Sys.setenv(APIaquacachePort = dbPort)
  Sys.setenv(APIaquacacheUser = dbUser)
  Sys.setenv(APIaquacachePass = dbPass)

  spec <- pr$getApiSpec()
  spec$components$securitySchemes$BasicAuth <- list(
    type = "http",
    scheme = "basic"
  )
  spec$security <- list(list(BasicAuth = list()))
  spec$servers <- list(list(url = server))
  pr$setApiSpec(spec)

  if (!run) {
    return(pr)
  }

  pr$run(host = host, port = port)
}
