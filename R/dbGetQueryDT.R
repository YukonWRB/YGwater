#' Retrieve a data.table from a database connection
#' 
#' @description
#' `r lifecycle::badge('stable')`
#' 
#' A simple wrapper function around [DBI::dbGetQuery()] that returns a data.table object instead of a data.frame. Can be used as a direct replacement for [DBI::dbGetQuery()], but be aware of the downstream effects of using a data.table object instead of a data.frame.
#' 
#' The `params` argument allows for parameterized queries, which can help prevent SQL injection attacks. However, BE AWARE THAT THIS IS SLOW where any element of `params` is a vector of length greater than 1 as this results in multiple queries. In such cases, it is recommended to use a different approach, such as preparing the statement with [glue::glue_sql()] instead (see notes the note about what to do for IN operations in the glue help file).
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
  
  df <- DBI::dbGetQuery(con, statement, params, ...)
  
  data.table::setDT(df)
  
  return(df)
}
