#' Connect to snow survey database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Connects to the old Snow Survey Access database.To connect to the new postgres database, use [snowConnect()].
#'
#' @param path The path to the database. Currently supports either a Microsoft Access database or an .sqlite database. 'default' points to //carver/infosys/Snow/DB/SnowDB.mdb.
#' @param silent TRUE suppresses messages except for errors.
#'
#' @return A connection to the database.
#' @export
#'

snowConnect_access <- function(path = "default", silent=FALSE){

  if (path == "default") {
    path <- "//carver/infosys/Snow/DB/SnowDB.mdb"
  }

  #Check the paths and make a connection
  if (!file.exists(path)) {
    stop("The path you specified either does not exist or this computer does not have access to that drive.")
  }
  if (stringr::str_detect(path, ".mdb")) {
    rlang::check_installed("odbc", reason = "required to use function snowConnect_access with an .mdb database") #This is here because odbc is not a 'depends' of this package; it is only necessary for this function and is therefore in "suggests"
    snowCon <- DBI::dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", path))
    if (!silent) {
      message("Remember to disconnect using DBI::dbDisconnect() when finished.")
    }
  } else {
    stop("This script is not designed to work with a database having that file extension. Currently supporting .mdb and .sqlite databases.")
  }
  return(snowCon)
}
