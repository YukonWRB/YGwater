#' Connect to the aquacache
#'
#' @description
#'
#' This function exists to facilitate connecting to the aquacache database. You can pass your own database connection parameters, for example to connect to a dev instance instead of the default production instance.
#' Record and table visibility as well as table privileges depends on the user account used to connect to the database. If you are unsure of your account's permissions, please contact your system administrator.
#'
#' @details
#' An attribute is added to the connection object to track if a transaction is active. This can be used by functions to determine if a transaction is already open, in which case functions can forgo opening a new transaction and instead use the existing one.
#'
#' @param name Database name. By default searches the .Renviron file for parameter=value pair of form aquacacheName="name". If you want to connect to a different database, pass the name of that database here.
#' @param host Database host address. By default searches the .Renviron file for parameter:value pair of form aquacacheHost="hostname".
#' @param port Connection port. By default searches the .Renviron file for parameter=value pair of form aquacachePort="1234".
#' @param username Username. By default searches the .Renviron file for parameter=value pair of form aquacacheUser="username". Refrain from using username with write privileges unless you absolutely know what you're doing.
#' @param password Password. By default searches the .Renviron file for parameter=value pair of form aquacachePass="password".
#' @param silent TRUE suppresses messages except for errors.
#'
#' @return A connection to the database.
#'
#' @export
#'

AquaConnect <- function(
  name = Sys.getenv("aquacacheName"),
  host = Sys.getenv("aquacacheHost"),
  port = Sys.getenv("aquacachePort"),
  username = Sys.getenv("aquacacheUser"),
  password = Sys.getenv("aquacachePass"),
  silent = FALSE
) {
  tryCatch(
    {
      con <- DBI::dbConnect(
        drv = RPostgres::Postgres(),
        dbname = name,
        host = host,
        port = port,
        user = username,
        password = password
      )

      # Check if the connection was successful
      check <- DBI::dbGetQuery(con, "SELECT 1")
      if (nrow(check) == 0) {
        stop("Could not fetch any data.")
      }

      # Check if the database is at a minimum version. Within a try block in case the user can't see the information schema or the table.
      try(
        {
          version <- as.integer(
            DBI::dbGetQuery(
              con,
              "SELECT version FROM information.version_info WHERE item = 'Last patch number'"
            )$version
          )

          if (length(version) == 1L && !is.na(version) && version < 74L) {
            warning(
              "YGwater requires AquaCache Patch 74 or later. This database is at Patch ",
              version,
              ".",
              call. = FALSE
            )
          }
        },
        silent = TRUE
      )

      # Add a new attribute to the connection object to track if a transaction is active
      attr(con, "active_transaction") <- FALSE

      # Explicitly set the timezone to UTC as all functions in this package work with UTC timezones
      DBI::dbExecute(con, "SET timezone = 'UTC'")

      if (!silent) {
        message(
          "Connected to the ",
          name,
          " database with the timezone set to UTC."
        )
        message(
          "Remember to disconnect using DBI::dbDisconnect() when finished."
        )
      }
      return(con)
    },
    error = function(e) {
      stop("AquaCache database connection failed: ", e$message)
    }
  )
}
