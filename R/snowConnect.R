#' Connect to the snow postgres database
#'
#' @description
#' This function exists to facilitate connecting to the snow database, especially if the database type and connection method changes in the future.
#'
#' @param name Database name.
#' @param host Database host address.
#' @param port Connection port.
#' @param username Username. Refrain from using username with write privileges unless you absolutely know what you're doing.
#' @param password Password.
#' @param silent TRUE suppresses messages except for errors.
#'
#' @return A connection to the database.
#'
#' @export
#'

snowConnect <- function(name = "snow", host = Sys.getenv("snowHost"), port = Sys.getenv("snowPort"), username = Sys.getenv("snowUser"), password = Sys.getenv("snowPass"), silent = FALSE){
  
  
  
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
      message("Connected to the snow database with the timezone set to UTC.")
      message("Remember to disconnect using DBI::dbDisconnect() when finished.")
    }
    return(con)
  }, error = function(e){
    stop("Snow database connection failed: ", e$message)
  })
}
