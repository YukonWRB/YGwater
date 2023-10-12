#' Get location metadata from the hydromet database
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Retrieves metadata for any valid location ID in the database. Returns the name of the location, the lat/long and (if possible) vertical datum information.
#'
#' @param path The path to the database, passed to [hydrometConnect()]. Default uses hydrometConnect default path.
#' @param location A character vector of 1 or more location IDs, or default "all" to retrieval all locations.
#' @param save_path Specify a path here if you want an Excel workbook saved to disk. "choose" lets you interactively choose your folder.
#'
#' @return A list of two data.frames: one with the location code, name, latitude, and longitude, and another with vertical datum information. Optionally, an Excel workbook saved to disk.
#'
#' @seealso [DB_browse_ts()] and [DB_get_ts()] to browse for and extract timeseries data; [DB_browse_spatial()] and [DB_get_spatial()] to browse and extract spatial layers.
#' @export
#'

DB_get_meta <- function(path = "default", location = "all", save_path = NULL) {

  if (!is.null(save_path)){
    if (save_path %in% c("Choose", "choose")) {
      print("Select the the folder where you want location metadata saved.")
      save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
    }
    if (!dir.exists(save_path)){
      stop("The save path you pointed me to does not exist.")
    }
  }


  DB <- hydrometConnect(path = path, silent = TRUE)
  on.exit(DBI::dbDisconnect(DB), add=TRUE)

  if (location == "all"){
    location <- DBI::dbGetQuery(DB, "SELECT location FROM locations")[,1]
  }

  meta <- data.frame()
  datum <- data.frame()
  for (i in location){
    latlong <- DBI::dbGetQuery(DB, paste0("SELECT latitude, longitude FROM locations WHERE location = '", i, "'"))
    datums <- DBI::dbGetQuery(DB, paste0("SELECT datum_id_from, datum_id_to, conversion_m, current FROM datum_conversions WHERE location = '", i, "'"))
    datum_codes <- unique(c(datums$datum_id_from, datums$datum_id_to))
    for (j in 1:nrow(datums)){
      datums$datum_id_from[j] <- DBI::dbGetQuery(DB, paste0("SELECT datum_name_en FROM datum_list WHERE datum_id = '", datums$datum_id_from[j], "'"))
      datums$datum_id_to[j] <- DBI::dbGetQuery(DB, paste0("SELECT datum_name_en FROM datum_list WHERE datum_id = '", datums$datum_id_to[j], "'"))
    }
    datums$location <- i
    datums <- datums[, c(5, 1,2,3,4)]
    names(datums) <- c("location", "origin datum", "end datum", "conversion (m)", "current")

    name <- DBI::dbGetQuery(DB, paste0("SELECT name FROM locations WHERE location = '", i, "'"))

    meta <- rbind(meta, data.frame("location_ID" = i,
                                   "name" = name[1,1],
                                   "latitude" = latlong[1,1],
                                   "longitude" = latlong[1,2]))
    datum <- rbind(datum, datums)
  }

  ls <- list("locations" = meta, "vertical_datums" = datums)

  if (!is.null(save_path)){
    openxlsx::write.xlsx(ls, paste0(save_path, "/"))
  }

  return(ls)
}
