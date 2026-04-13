#' Normalize API version input
#' Converts various forms of version input (e.g., "v1", "1", NULL) into a standardized integer format. This function ensures that the version input is valid and can be used to determine which API implementation to run. It accepts NULL (indicating the latest version), a positive integer, or a "v"-prefixed version string, and returns the corresponding integer version number or NULL if the input is NULL.
#' @param version The API version input to normalize. Can be NULL, a positive integer, or a "v"-prefixed version string.
#' @return An integer representing the API version, or NULL if the input was NULL.
#' @noRd
#' @keywords internal
api_normalize_version <- function(version) {
  if (is.null(version)) {
    return(NULL)
  }

  if (is.character(version) && length(version) == 1L) {
    version <- sub("^v", "", version, ignore.case = TRUE)
  }

  version <- suppressWarnings(as.integer(version))

  if (length(version) != 1L || is.na(version) || version < 1L) {
    stop(
      "`version` must be NULL, a positive integer, or a 'v'-prefixed version string.",
      call. = FALSE
    )
  }

  version
}

#' Find the API implementation target based on the requested version
#' Searches for the appropriate API implementation file or directory based on the normalized version input. It looks for specific files or directories in the package's "plumber" directory that match the requested version. If a specific version is requested, it checks for the existence of the corresponding file or directory. If no version is specified, it defaults to the latest complete file-based API version. The function returns a list containing the version number, the engine type (e.g., "plumber" or "plumber2"), and the path to the API implementation.
#' @param version The normalized API version to find. Can be NULL (for the latest version) or a positive integer.
#' @return A list with the following components:
#'   - version: The integer version number of the API implementation found.
#'   - engine: A string indicating the API engine type (e.g., "plumber" or "plumber2").
#'   - path: The file path to the API implementation.
#' @noRd
#' @keywords internal

api_find_target <- function(version = NULL) {
  plumber_root <- system.file("plumber", package = "YGwater")

  if (plumber_root == "") {
    stop("YGwater API not found.", call. = FALSE)
  }

  if (!is.null(version)) {
    plumber_file <- system.file(
      sprintf("plumber/v%d.R", version),
      package = "YGwater"
    )
    if (plumber_file != "") {
      return(list(
        version = version,
        engine = "plumber",
        path = plumber_file
      ))
    }

    plumber2_dir <- system.file(
      file.path("plumber", sprintf("v%d", version)),
      package = "YGwater"
    )
    if (plumber2_dir != "" && dir.exists(plumber2_dir)) {
      return(list(
        version = version,
        engine = "plumber2",
        path = plumber2_dir
      ))
    }

    stop(sprintf("YGwater API version v%d not found.", version), call. = FALSE)
  }

  api_files <- list.files(plumber_root, pattern = "^v[0-9]+\\.R$")
  if (length(api_files) == 0) {
    stop("No default API versions found in the package.", call. = FALSE)
  }

  versions <- as.integer(gsub("^v([0-9]+)\\.R$", "\\1", api_files))
  latest_version <- max(versions, na.rm = TRUE)

  list(
    version = latest_version,
    engine = "plumber",
    path = system.file(
      sprintf("plumber/v%d.R", latest_version),
      package = "YGwater"
    )
  )
}

#' Run the YGwater API
#'
#' @description
#' Launches the YGwater API using the installed router implementation for the
#' requested version. The current default remains the latest complete
#' file-based API in `inst/plumber` so in-progress versions can be added
#' without replacing the existing surface unintentionally.
#'
#' @param version The API version to use. Accepts `NULL`, a positive integer,
#'   or a `"v"`-prefixed version string such as `"v2"`. Default is `NULL`,
#'   which uses the latest complete file-based API version.
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
  version <- api_normalize_version(version)
  api_target <- api_find_target(version)

  if (identical(api_target$engine, "plumber")) {
    rlang::check_installed("plumber", reason = "required to run API v1")
    rlang::check_installed("memoise", reason = "required to run API v1")
    rlang::check_installed("cachem", reason = "required to run API v1")
  } else if (identical(api_target$engine, "plumber2")) {
    rlang::check_installed(
      "plumber2",
      reason = "required to run API v2 or newer"
    )
  } else {
    stop("Unsupported API router engine.", call. = FALSE)
  }

  # Set environment variables for database connection
  # username and password are handled via Basic Authentication or use the default read-only user
  Sys.setenv(APIaquacacheName = dbName)
  Sys.setenv(APIaquacacheHost = dbHost)
  Sys.setenv(APIaquacachePort = dbPort)
  Sys.setenv(APIaquacacheUser = dbUser)
  Sys.setenv(APIaquacachePass = dbPass)

  if (identical(api_target$engine, "plumber2")) {
    pr <- plumber2::api(api_target$path)
    pr <- plumber2::api_doc_add(
      pr,
      list(servers = list(list(url = server))),
      overwrite = TRUE,
      subset = "servers"
    )

    if (!run) {
      return(pr)
    }

    return(plumber2::api_run(pr, host = host, port = port, silent = TRUE))
  }

  pr <- plumber::plumb(api_target$path)

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
