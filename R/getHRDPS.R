#' Download/proces ECCC HRDPS rasters
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' NOTE: If you have access to the WRB aquacache these rasters are already in there. Use function [getRaster()] to extract what you need to a .tiff file.
#'
#' Utility function to retrieve gridded predictions output from the [HRDPS model](https://collaboration.cmc.ec.gc.ca/cmc/cmoi/product_guide/docs/tech_notes/technote_hrdps_e.pdf). In current form will delete all old files in the save directory.
#'
#' @param clip The two-digit abbreviation(s) as per [Canadian Census](https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/tab/index-eng.cfm?ID=t1_8) for the province(s) with which to clip the HRDPA. A 300 km buffer is added beyond the provincial boundaries. Set to NULL for no clip
#' @param save_path The path to the directory (folder) where the rasters should be saved. A new sub-directory will be created based on the `param` selected if not already present. Default `"choose"` lets you select your folder (do not choose the one named after the `param`), or enter the path as a character string. You must specify a save path when running in non-interactive mode.
#' @param param The HRDPS parameter you wish to download. As of 2023-04-15, this list of [published abbreviations](https://weather.gc.ca/grib/HRDPS_HR/HRDPS_ps2p5km_PNONZERO_deterministic_e.html) is out of date, so cross-reference it with those of the [model outputs](https://dd.weather.gc.ca/model_hrdps/continental/2.5km/12/010/). Defaults to accumulated precipitation at the surface.
#'
#' @seealso [getHRDPA()] if looking for precipitation reanalysis rasters instead. For nice precipitation maps and tabular reports of precipitation (past or future), try [basinPrecip()].
#'
#' @return Up to 48 rasters (one per hour) representing the HRDPS modeled output for the parameter selected.
#' @export
#'

getHRDPS <- function(clip = c("YT"),
                     save_path = "choose",
                     param = "APCP_Sfc")
{

  # Save path
  if (save_path == "choose") {
    if (!interactive()) {
      stop("You must specify a save path when running in non-interactive mode.")
    }
    message("Select the path to the folder where you want these rasters saved.")
    save_path <- rstudioapi::selectDirectory(caption = "Select Save Folder")
  }

  suppressWarnings(dir.create(paste0(save_path, "/", param)))

  save_path <- paste0(save_path, "/", param)

  #NOTE:: models seem to be run 3 hours post and take some time to write files. Use a lag of 4 hours to be realistic.
  current_utc <- Sys.time()
  attr(current_utc, "tzone") <- "UTC"
  latest_run <- as.character(lubridate::floor_date(current_utc - 4*60*60, "6 hours"))
  if (nchar(latest_run) < 13) {
    latest_run <- paste0(latest_run, " 00:00")
  }
  issue_timedate <- gsub("-", "", latest_run)
  issue_date <- substr(gsub(" ", "", issue_timedate), 1, 8)
  issue_hour <- substr(issue_timedate, 10, 11)
  issue_timedate <- paste0(issue_date, "T", issue_hour, "Z")

  #Delete old files with the same param and NOT the same issue_timedate
  existing <- list.files(save_path, full.names = TRUE)
  keep <- paste0(param, "_", issue_timedate)
  delete <- !grepl(keep, existing)
  file.remove(existing[delete])

  #Make clip polygon
  extent <- paste(clip, collapse = "_")
  if (!is.null(clip)) {
    clip <- prov_buff[prov_buff$PREABBR %in% clip, ]
    if (nrow(clip) == 0) {
      clip <- NULL
    }
  }

  existing <- list.files(save_path)
  clipped <- FALSE #So that clip happens the first time around
  tryCatch({
    for (i in 1:48) {
      if (nchar(i) == 1) { # format i so that it works in creating the url
        i <- paste0("0", i)
      }

      if (!is.null(clip)) {
        name <- paste0(param, "_", issue_timedate, "_", i, "_clipped_", extent, ".tiff")
      } else {
        name <- paste0(param, "_", issue_timedate, "_", i, ".tiff")
      }
      if (!(TRUE %in% grepl(name, existing))) { # Checks if the file exists already, runs if not.
        raster <- terra::rast(paste0("https://dd.weather.gc.ca/model_hrdps/continental/2.5km/", issue_hour, "/0", i, "/", issue_timedate, "_MSC_HRDPS_", param, "_RLatLon0.0225_PT0", i, "H.grib2"))

        if (!clipped) {
          if (!is.null(clip)) {
            clip <- terra::project(clip, raster) #project clip vector to crs of the raster
          }
          clipped <- TRUE #So that project doesn't happen after the first iteration
        }
        if (!is.null(clip)) {
          raster <- terra::mask(raster, clip) #Makes NA values beyond the boundary of clip
          raster <- terra::trim(raster) #Trims the NA values
        }
        terra::writeRaster(raster, paste0(save_path, "/", name), overwrite = TRUE)
      }
    }
    failed <- FALSE
  }, error = function(e) {
    cli::cli_alert_danger(
      "{.fg_red Fetching rasters failed on at least one file for the most recent release (issue time {issue_hour} UTC); fetching the prior issue time forecasts. This is probably temporary, try again once the files have been written to the URL.}"
    )
    failed <<- TRUE
  })
  
  if (failed) {
    #Clear the folder in case some rasters were in fact downloaded in previous loop
    existing <- list.files(save_path, full.names = TRUE)
    keep <- paste0(param, "_", issue_timedate)
    delete <- grepl(keep, existing)
    file.remove(existing[delete])

    #Reset time parameters to get the previous issue hour rasters
    previous_run <- as.character(lubridate::floor_date(current_utc - 8*60*60, "6 hours"))
    if (nchar(previous_run) < 13) {
      previous_run <- paste0(previous_run, " 00:00")
    }
    issue_timedate <- gsub("-", "", previous_run)
    issue_date <- substr(gsub(" ", "", issue_timedate), 1, 8)
    issue_hour <- substr(issue_timedate, 10, 11)
    issue_timedate <- paste0(issue_date, "T", issue_hour, "Z")

    tryCatch({
      for (i in 1:48) {
        if (nchar(i) == 1) { #format i so that it works in creating the url
          i <- paste0("0", i)
        }
        if (!is.null(clip)) {
          name <- paste0(param, "_", issue_timedate, "_", i, "_clipped_", extent, ".tiff")
        } else {
          name <- paste0(param, "_", issue_timedate, "_", i, ".tiff")
        }
        if (!(TRUE %in% grepl(name, existing))) { #Checks if the file exists already, runs if not.
          raster <- terra::rast(paste0("https://dd.weather.gc.ca/model_hrdps/continental/2.5km/", issue_hour, "/0", i, "/", issue_timedate, "_MSC_HRDPS_", param, "_RLatLon0.0225_PT0", i, "H.grib2"))

          if (!clipped) {
            if (!is.null(clip)) {
              clip <- terra::project(clip, raster) #project clip vector to crs of the raster
            }
            clipped <- TRUE #So that project doesn't happen after the first iteration
          }
          if (!is.null(clip)) {
            raster <- terra::mask(raster, clip) #Makes NA values beyond the boundary of clip
            raster <- terra::trim(raster) #Trims the NA values
          }
          terra::writeRaster(raster, paste0(save_path, "/", name), overwrite = TRUE)
        }
      }
    }, error = function(e) {
      cli::cli_alert_danger(
        "{.fg_red Fetching rasters failed on the most recent release (issue time {issue_hour} UTC) as well as the prior release. Suggest you investigate connection issues: try accessing the rasters directly at https://dd.weather.gc.ca/model_hrdps/continental/2.5km and see if the directories are populated. If they are the issue is likely on this end, otherwise it's an ECCC issue.}"
      )
    })
  }
}
