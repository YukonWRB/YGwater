#' Connect to the RWIS database
#'
#' @description
#'
#' Establishes a connection to the RWIS (Road Weather Information System) database. Only works from within YG networks.
#'
#' @param name Database name.
#' @param host Database host address.
#' @param port Connection port.
#' @param username Username. Refrain from using username with write privileges unless you absolutely know what you're doing.
#' @param password Password.
#' @return A connection to the database
#'
#' @seealso [AquaConnect()] for establishing a connection to the WRB's hydrometric database.
#'
#' @export
#'

RWISConnect <- function(name = "rwdm", host = "rwis.gov.yk.ca", port = "5432", username = "rwdmread", password = "rwdmread") 
{
  
  #initial checks
  rlang::check_installed("RPostgres", reason = "Package RPostgres is required to use function RWISConnect") #This is here because RPostgreSQL is not a 'depends' of this package; it is only necessary for this function and is therefore in "suggests"
  
  RWIS <- DBI::dbConnect(drv = RPostgres::Postgres(),
                         dbname = name,
                         host = host,
                         port = port,
                         user = username,
                         password = password)
  
  if (!DBI::dbIsValid(RWIS)) {
    stop("Connection failed.")
  } else {
    return(RWIS)
  }
}

