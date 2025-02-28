#' Connect to the aquacache
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function exists to facilitate connecting to the aquacache database. You can pass your own database connection parameters, for example to connect to a dev instance instead of the default production instance.
#' Database superusers or admins will have access to all database records (unless your instance of aquacache was set up differently), but other users will be asked to provide their row level security username and password to access records other than the 'public' ones. Note that this is *not necessarily* the same username and password as the one used to log into the database itself.
#'
#' @param name Database name.
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

AquaConnect <- function(name = "aquacache", host = Sys.getenv("aquacacheHost"), port = Sys.getenv("aquacachePort"), username = Sys.getenv("aquacacheUser"), password = Sys.getenv("aquacachePass"), silent = FALSE){

  tryCatch({
    con <- DBI::dbConnect(drv = RPostgres::Postgres(),
                            dbname = name,
                            host = host,
                            port = port,
                            user = username,
                            password = password)
    
    # Explicitly set the timezone to UTC as all functions in this package work with UTC timezones
    DBI::dbExecute(con, "SET timezone = 'UTC'")
    
    if (!silent) {
      message("Connected to the aquacache database with the timezone set to UTC.")
      message("Remember to disconnect using DBI::dbDisconnect() when finished.")
    }
    return(con)
  }, error = function(e){
    stop("AquaCache database connection failed: ", e$message)
  })
}
