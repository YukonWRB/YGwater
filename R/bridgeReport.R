#' Bridge radar reporting utility - internal
#'
#' This function generates a report of distance between the water surface and bridges or other important infrastructure. The output is a Microsoft Word document on a Yukon Government template.
#'
#' @param con A connection to the database. Default uses function [AquaConnect()] with default settings.
#' @param locations The list of locations codes for which you want a distance measurement, or default "all" to get every one in the database. These must be reporting radar distance in the WRB database as parameter 'distance'. Default "all" will only fetch locations where the network is listed as 'Highway Observation Network' in the locations_networks table of the database.
#' @param zoom Set TRUE if you want zoomed-in plots.
#' @param zoom_days Set the number of days on the x-axis of the zoomed in plots.
#' @param save_path The path to the directory (folder) where the report should be saved. Default "choose" lets you select your folder, otherwise enter the path as a character string. WARNING: option 'choose' only works on Windows, and some late-build R versions have a bug that prevents it from working every time.
#'
#' @return A Microsoft Word report containing bridge radar information, saved in the location specified.
#' @export
#'

bridgeReport <- function(
  con = AquaConnect(silent = TRUE),
  locations = "all",
  zoom = TRUE,
  zoom_days = 30,
  save_path = "choose"
) {
  on.exit(DBI::dbDisconnect(con))
  rlang::check_installed(
    "knitr",
    reason = "necessary to create a report using Rmarkdown."
  )
  if (!rlang::is_installed("knitr")) {
    #This is here because knitr is not a 'depends' of this package; it is only necessary for this function and is therefore in "suggests"
    message("Installing dependency 'knitr'...")
    utils::install.packages("knitr")
    if (rlang::is_installed("knitr")) {
      message("Package knitr successfully installed.")
    } else {
      stop(
        "Failed to install package knitr. You could troubleshoot by running utils::install.packages('knitr') by itself."
      )
    }
  }

  if (save_path == "choose") {
    if (!interactive()) {
      stop("You must specify a save path when running in non-interactive mode.")
    }
    message("Select the path to the folder where you want this report saved.")
    save_path <- rstudioapi::selectDirectory(
      caption = "Select Save Folder",
      path = file.path(Sys.getenv("USERPROFILE"), "Desktop")
    )
  } else {
    if (!dir.exists(save_path)) {
      stop("The save path you specified does not exist.")
    }
  }

  if (locations == "all") {
    tsid <- DBI::dbGetQuery(
      con,
      "
    SELECT l.location_code AS location, t.timeseries_id, l.name 
    FROM timeseries AS t 
    JOIN parameters AS p ON t.parameter_id = p.parameter_id 
    JOIN locations_networks AS ln ON t.location_id = ln.location_id 
    JOIN networks AS n ON ln.network_id = n.network_id 
    JOIN locations AS l ON t.location_id = l.location_id
    WHERE p.parameter_id = 1160
    AND n.name = 'Highway Observation Network';
"
    )
  } else {
    tsid <- DBI::dbGetQuery(
      con,
      "
    SELECT l.location_code AS location, t.timeseries_id, l.name 
    FROM timeseries AS t 
    JOIN parameters AS p ON t.parameter_id = p.parameter_id 
    JOIN locations_networks AS ln ON t.location_id = ln.location_id 
    JOIN networks AS n ON ln.network_id = n.network_id 
    JOIN locations AS l ON t.location_id = l.location_id
    WHERE p.parameter_id = 1160
    AND n.name = 'Highway Observation Network'
    AND l.location_code IN ('",
      paste(locations, collapse = "', '"),
      "');
"
    )
  }

  rmarkdown::render(
    input = system.file("rmd", "Bridge_report.Rmd", package = "YGwater"),
    output_file = paste0(Sys.Date(), "_Bridge-Radar-Report"),
    output_dir = save_path,
    params = list(
      con = con,
      tsid = tsid,
      zoom = zoom,
      zoom_days = zoom_days
    )
  )
}
