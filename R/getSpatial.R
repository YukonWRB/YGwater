#' Retrieve raster files from the database
#'
#' @description
#' `r lifecycle::badge('experimental')`
#' 
#' Placeholder function, not yet complete.
#'
#' @return An error message telling the user what to do instead.
#' @export
#'

getRaster <- function() {
  stop("This function isn't finished yet. Use rpostgis::pgGetRast() to extract a raster instead.")
}

#' Retrieve vector files from the database
#' 
#' @description
#' `r lifecycle::badge('stable')`
#'
#' This function formulates an SQL query to retrieve points, lines, or polygons from the database and returns them as a terra object. At minimum, only one of `geom_id`, `layer_name`, `feature_name`, or `geom_type` are required, though if the combination you enter results in more than one geom_type you will get a descriptive error.
#'
#' @param geom_id A numeric vector of geom_ids from the 'vectors' table.
#' @param layer_name A character vector (one or more elements) specifying the target layer_name from the 'vectors' table.
#' @param feature_name A character vector (one or more elements) specifying the target feature_name from the 'vectors' table.
#' @param geom_type One of c('ST_Point', 'ST_MultiPoint', 'ST_LineString', 'ST_MultiLineString', 'ST_Polygon', 'ST_MultiPolygon').
#' @param return_cols The names of columns to return.
#' @param bounds Optional bounds by which to limit the spatial extent returned. Currently returns the full extent of every feature intersecting the bounds. Refer to parameter 'boundary' of [rpostgis::pgGetGeom()] for details of how to specify this parameter.
#' @param table The target table in the database (as character string). If not under the public schema, use format c("schema", "table").
#' @param geom_col The name of the database table column in which to insert the geometry object.
#' @param silent Should the function suppress all messages?
#' @param con A connection to the target database. NULL uses [AquaConnect()] and automatically disconnects.
#'
#' @return If successful, a terra object. If unsuccessful because the query targets more than 1 geometry types, a table showing you the result of the query.
#' @export
#'

getVector <- function(geom_id = NULL, layer_name = NULL, feature_name = NULL, geom_type = NULL, return_cols = c("geom_id", "geom_type", "layer_name", "feature_name", "description"), bounds = NULL, table = "vectors", geom_col = "geom", silent = FALSE, con = NULL) {
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  rlang::check_installed("rpostgis", reason = "required to use function getVector.")
  
  if (is.null(geom_id) & is.null(layer_name) & is.null(feature_name) & is.null(geom_type)) {
    stop("You need to specify at least one of the NULL parameters.")
  }
  if (!is.null(geom_type)) {
    if (length(geom_type) > 1) {
      stop("You can only select one geom_type at a time.")
    } else if (!(geom_type %in% c('ST_Point', 'ST_MultiPoint', 'ST_LineString', 'ST_MultiLineString', 'ST_Polygon', 'ST_MultiPolygon'))) {
      stop("Parameter geom_type is not one of the possible choices. Refer to the help file.")
    }
  }
  
  # build the query to see if data exists in the table
  query <- paste0("SELECT geom_id, layer_name, feature_name, geom_type FROM ", table, " WHERE")
  
  if (!is.null(geom_id)) {
    query <- paste0(query,  " geom_id IN (", paste(geom_id, collapse = ", "), ") AND")
  }
  if (!is.null(layer_name)) {
    query <- paste0(query,  " layer_name IN ('", paste(layer_name, collapse = "', '"), "') AND")
  }
  if (!is.null(feature_name)) {
    query <- paste0(query,  " feature_name IN ('", paste(feature_name, collapse = "', '"), "') AND")
  }
  if (!is.null(geom_type)) {
    query <- paste0(query,  " geom_type IN ('", paste(geom_type, collapse = "', '"), "') AND")
  }
  query <- gsub("\\s+AND$", "", query)
  tbl <- DBI::dbGetQuery(con, query)
  
  if (nrow(tbl) == 0) {
    stop("Your query returned no results (even before bounds were applied).")
  }
  
  # Check if query resulted in multiple geom_types
  if (length(unique(tbl$geom_type)) > 1) {
    if (!silent) {
      message("Your query resulted in more than one geometry type: ", paste(unique(tbl$geom_type), collapse = ", AND "), " were returned. This may result in a non-functional terra object if, for example, lines and polygons are returned together.")
    }
  }
  
  subquery <- paste0("WHERE")

  if (!is.null(geom_id)) {
    subquery <- paste0(subquery,  " geom_id IN (", paste(geom_id, collapse = ", "), ") AND")
  }
  if (!is.null(layer_name)) {
    subquery <- paste0(subquery,  " layer_name IN ('", paste(layer_name, collapse = "', '"), "') AND")
  }
  if (!is.null(feature_name)) {
    subquery <- paste0(subquery,  " feature_name IN ('", paste(feature_name, collapse = "', '"), "') AND")
  }
  if (!is.null(geom_type)) {
    subquery <- paste0(subquery,  " geom_type IN ('", paste(geom_type, collapse = "', '"), "') AND")
  }
  subquery <- gsub("\\s+AND$", "", subquery)
  
  if (silent) {
    res <- suppressMessages(rpostgis::pgGetGeom(con, name = "vectors", geom = "geom", gid = "geom_id", other.cols = TRUE, clauses = subquery, returnclass = "terra", boundary = bounds))
  } else {
    res <- rpostgis::pgGetGeom(con, name = "vectors", geom = "geom", gid = "geom_id", other.cols = TRUE, clauses = subquery, returnclass = "terra", boundary = bounds)
  }
  return(res)
}
