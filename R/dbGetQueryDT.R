#' Retrieve a data.table from a database connection
#' 
#' @description
#' `r lifecycle::badge('stable')`
#' 
#' A simple wrapper function around [DBI::dbGetQuery()] that returns a data.table object instead of a data.frame. Can be used as a direct replacement for [DBI::dbGetQuery()], but be aware of the downstream effects of using a data.table object instead of a data.frame.
#'
#' @param con A connection object as returned by a [DBI::dbConnect()] call or, in the context of this package, a [AquaConnect()], or [RWISConnect()], or [snowConnect()] call.
#' @param statement A SQL statement to be passed to the database.
#' @param params A list of parameters to be passed to the SQL statement. This is optional and can be used to parameterize the SQL query.
#' @param ... Other parameters passed to the ... parameter of [DBI::dbGetQuery()].
#'
#' @return A data.table object.
#' @export
#' 
#' @import data.table
#'

dbGetQueryDT <- function(con, statement, params = NULL, ...) {
  .datatable.aware <- TRUE
  
  df <- DBI::dbGetQuery(con, statement, ...)
  
  data.table::setDT(df)
  
  return(df)
}
