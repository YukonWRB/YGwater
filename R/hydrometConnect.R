#' Connect to the hydromet database
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Superseded by function [AquaConnect()]. Still exported by this package for backwards compatibility, as old database 'hydromet' is now a development environment.
#'
#' @param name Database name.
#' @param host Database host address. By default searches the .Renviron file for parameter:value pair of form hydrometHost="hostname".
#' @param port Connection port. By default searches the .Renviron file for parameter=value pair of form hydrometPort="1234".
#' @param username Username. By default searches the .Renviron file for parameter=value pair of form hydrometUser="username". Refrain from using username with write privileges unless you absolutely know what you're doing.
#' @param password Password. By default searches the .Renviron file for parameter=value pair of form hydrometPass="password".
#' @param silent TRUE suppresses messages except for errors.
#'
#' @return A connection to the database.
#'
#' @export
#'

hydrometConnect <- function(name = "hydromet", host = Sys.getenv("hydrometHost"), port = Sys.getenv("hydrometPort"), username = Sys.getenv("hydrometUser"), password = Sys.getenv("hydrometPass"), silent = FALSE){

  warning("Function hydrometConnect() is superseded by function AquaConnect(). Please use AquaConnect() instead unless you need to access the dev database.")
  
  tryCatch({
    hydro <- DBI::dbConnect(drv = RPostgres::Postgres(),
                            dbname = name,
                            host = host,
                            port = port,
                            user = username,
                            password = password)
    if (!silent) {
      message("Remember to disconnect using DBI::dbDisconnect() when finished.")
    }
    return(hydro)
  }, error = function(e){
    stop("Connection failed.")
  })
}
