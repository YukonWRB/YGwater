#' Get information on spatial files in the database
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Wondering what's in the database? This function helps you see what spatial files we have, with an eye to helping you create a query for function [DB_get_spatial()]. Leaving all NULL defaults will show you every polygon and raster in the database.
#'
#'@details
#' Spatial files are not stored directly in the database but rather in folders situated alongside the database. The database stores the file description and other identifiers, as well as the path to the file.
#'
#' @param con A connection to the database. Default uses function [hydrometConnect()] with default settings.
#' @param type 'polygon', 'point', 'line' or 'raster'? Currently only works with polygons.
#' @param name Narrow by associated polygon name.
#' @param description Narrow by polygon description(s) if you wish. Specify multiple descriptions as a single character vector. These should be standardized to only a few names. To view description options, run this function with all default parameters perform a unique() operation on the description column.
#' @param parameter Narrow by parameter(s) if you wish. Specify multiple parameters as a single character vector. Note that this can be NULL for polygons, and that NULLs will not be returned.
#'
#' @seealso [DB_get_spatial()] if you want the spatial layer.
#' @return A data.frame containing the database entries for polygons and/or rasters (NOT the spatial files themselves - get these using DB_get_spatial using the rowid column).
#' @export
#'

DB_browse_spatial <- function(con = hydrometConnect(), type, name = NULL, description = NULL, parameter = NULL) {

  if (type != "polygon") {
    stop("Not set up to pull types other than polygons for now.")
  }
  if (type == "polygon"){
    polys <- DBI::dbGetQuery(con, "SELECT * FROM polygons")
    if (!is.null(location)){
      polys <- polys[polys$location %in% location ,]
    }
    if (!is.null(description)){
      polys <- polys[polys$description %in% description ,]
    }
    if (!is.null(parameter)){
      polys <- polys[polys$parameter %in% parameter ,]
    }
  } else if (type == "raster"){
    rasters <- DBI::dbGetQuery(con, "SELECT rowid, * FROM rasters")
    if (!is.null(location)){
      rasters <- rasters[rasters$location %in% location ,]
    }
    if (!is.null(description)){
      rasters <- rasters[rasters$description %in% description ,]
    }
    if (!is.null(parameter)){
      rasters <- rasters[rasters$description %in% description ,]
    }
  } else if (type == "point"){

  } else if (type == "line") {

  } else {
    stop("You must specify a type of either 'polygon', 'point', 'line' or 'raster'. Try again.")
  }

  if (type == "polygon"){
    if (nrow(polys) > 0){
      return(polys)
    } else {
      stop("No records matched your inputs.")
    }

  } else if (type == "raster"){
    if (nrow(rasters) > 0){
      return(rasters)
    } else {
      stop("No records matched your inputs.")
    }
  }
}
