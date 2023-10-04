#' Get spatial files from the database
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Loads a database spatial file to the R environment and, optionally, saves the file to disc. Refer to function [DB_browse_spatial()] to identify the exact record you are looking for.
#'
#'@details
#' Spatial files are not stored directly in the database but rather in folders situated alongside the database. The database stores the file description and other identifiers, as well as the path to the file.
#'
#' @param path The path to the database, passed to [hydroConnect()]. Default uses hydroConnect default path.
#' @param type 'polygon' or 'raster'?
#' @param rowid The rowid of the file you wish to download. If unsure, use function [DB_browse_spatial()] to narrow your search down to a single file.
#' @param save_path Optional; the path where the raster (as tif) or polygon (as shapefile) should be saved. You can enter 'choose' to select the path interactively.
#' @param save_name Optional, the name of the layer written to disk (without extension). Leave NULL to have it named vector or raster with the system date.
#'
#' @seealso [DB_browse_spatial()] if you need to identify the exact record you're after.
#'
#' @return A spatial object as per the terra package and, if requested, a shapefile or tif saved to disc.
#' @export
#'

DB_get_spatial <- function(path = "default", type, rowid, save_path = NULL, save_name = NULL) {

  if (!is.null(save_path)){
    if (save_path %in% c("Choose", "choose")) {
      print("Select the folder where you want this polygon/raster saved.")
      save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
    }
    if (!dir.exists(save_path)){
      stop("The save path you pointed me to does not exist.")
    }
  }

  DB <- hydroConnect(path = path, silent = TRUE)
  on.exit(DBI::dbDisconnect(DB), add=TRUE)

  if (type == "polygon"){
    poly <- DBI::dbGetQuery(DB, paste0("SELECT * FROM polygons WHERE ROWID = '", rowid, "'"))
    if (nrow(poly != 1)){
      stop("There is no polygon associated with that rowid. Refer to DB_browse_spatial() to find the right rowid.")
    }
    poly <- terra::vect(poly$file_path)
    if (!is.null(save_path)){
      if (!is.null(save_name)){
        terra::writeVector(poly, paste0(save_path, "/", save_name, ".shp"))
      } else {
        terra::writeVector(poly, paste0(save_path, "/vector_", Sys.Date(), ".shp"))
      }
    }
    return(poly)
  } else if (type == "raster"){
    raster <- DBI::dbGetQuery(DB, paste0("SELECT * FROM rasters WHERE ROWID = '", rowid, "'"))
    if (nrow(raster != 1)){
      stop("There is no raster associated with that rowid. Refer to DB_browse_spatial() to find the right rowid.")
    }
    raster <- terra::rast(raster$file_path)
    if (!is.null(save_path)){
      if (!is.null(save_name)){
        terra::writeRaster(raster, paste0(save_path, "/", save_name, ".tif"))
      } else {
        terra::writeRaster(raster, paste0(save_path, "/raster_", Sys.Date(), ".tif"))
      }
    }
    return(raster)
  } else {
    stop("You must specify a type of either 'polygon' or 'raster'. Tray again.")
  }
}
