#' Download ECCC weather station data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This script downloads data from an ECCC station for a given date range, calling [weathercan::weather_dl()] to download the data. This function facilitates interaction with that package by modifying start and end dates if your request is out of range, and allows you to interactively search for locations by name. Note that this function may take a long time to complete if you are requesting multiple years of data!
#' 
#' @seealso [chooseWeather()], [combineWeather()].
#'
#' @param station The station for which you want data. You can specify the 7-digit/letter Climate ID, the 4 or 5 digit ECCC station ID, the 5-digit WMO ID (starts with a 7), or the three-letter Transport Canada ID (i.e YDA and not CYDA). If working interactively you can also specify the station name or part thereof (as character vector) and select from a list. If you're unsure of the ID of the station you're looking for you can run weathercan::stations() to get a list of all stations or, for more advanced functionality, run [chooseWeather()]
#' @param start The start date for which you want data. Input either a character vector of form "2022-12-30" or a Date formatted object.
#' @param end The end date for which you want data. Input either a character vector of form "2022-12-30" or a Date formatted object.
#' @param save_path The path where you wish to save the resultant .csv file. Defaults to NULL, in which case you should assign the function to an R object. Set to "choose" to interactively select the location.
#' @param tzone Choose from "local" or "UTC". Note that "local" is whatever ECCC thinks the station local TZ is: this might be inaccurate in Yukon, where many (maybe even all) stations report in UTC-8.
#' @param interval Select from 'hour', 'day', or 'month'. However, if requesting a location by name you will need to select your chosen interval  interactively (so don't bother modifying the default).
#'
#'@seealso [weathercan::weather_dl()] for a simpler, pared-down means of downloading ECCC weather data. For nice precipitation maps and tabular reports of precipitation (past or future), try [basinPrecip()].
#'
#' @return A data.frame of weather data and, if save_path is specified, a csv of this same data located in the save_path.
#' @export
#'
getWeather <- function(station,
                       start,
                       end = Sys.Date(),
                       tzone = "UTC",
                       interval = "hour",
                       save_path = NULL)
{

  #initial checks
  rlang::check_installed("remotes", reason = "to update dependencies for this function.")
  if (!rlang::is_installed("weathercan")) { #This is here because getWeather is not a 'depends' of this package; it is only necessary for this function and is therefore in "suggests"
    message("Installing dependency 'weathercan'...")
    remotes::install_github("ropensci/weathercan")
    if (rlang::is_installed("weathercan")) {
      message("Package weathercan successfully installed.")
    } else {
      stop("Failed to install package weathercan. You could troubleshoot by running 'remotes::install_github('ropensci/weathercan')' by itself.")
    }
  }

  if (!(tzone %in% c("UTC", "local"))) {
    stop("The parameter tzone must be one of 'UTC' or 'local'.")
  }
  if (tzone == "local") tzone <- "none" #the parameter has a stupid name in weathercan::weather_dl.
  interval <- tolower(interval)
  if (!(interval %in% c("hour", "day", "month"))) {
    stop("The parameter interval must be one of 'hour', 'day', 'month'.")
  }

  if (!is.null(save_path)) {
    if (save_path %in% c("Choose", "choose")) {
      message("Select the path to the folder where you want this data saved.")
      save_path <- as.character(utils::choose.dir(caption = "Select Save Folder"))
    } else {
      if (!dir.exists(save_path)) {
        stop("The save directory you pointed to does not exist. Try again, or set save_path = `choose` to select it interactively")
      }
    }
  }

  station <- as.character(station)
  station <- toupper(station)

  # Check if station list needs to be updated
  if (weathercan::stations_meta()$ECCC_modified < Sys.time() - 180*24*60*60) {
    tryCatch({
      rlang::check_installed("lutz", reason = "to update the station list.")
      rlang::check_installed("sf", reason = "to update the station list.")
      suppressWarnings(weathercan::stations_dl(quiet = TRUE))
    }, error = function(e) {
      warning("The local list of stations is outdated and automatically updating it failed. Please update it by running weathercan::stations_dl(), especially if there's an issue running this function.")
    })
  } else {
    message(paste0("Station list was last updated on ", substr(weathercan::stations_meta()$ECCC_modified, 1, 10), ". You can manually update it by running weathercan::stations_dl() if you think this is needed."))
  }

  #Match the input numbers to the proper ECCC station ID
  stations <- suppressMessages(weathercan::stations())
  if (grepl("^[7]{1}", station)) { #Then WMO ID
    station <- stations[stations$WMO_id==station & !is.na(stations$WMO_id) & stations$interval == interval,]
  } else if (grepl("^[0-9]{4}[0-9A-Za-z]{3}$", station)) { #Climate ID
    station <- stations[stations$climate_id==station & !is.na(stations$climate_id) & stations$interval == interval,]
  } else if (grepl("^[0-6,8-9]{1}", station)) { #Station ID
    station <- stations[stations$station_id==station & !is.na(stations$station_id) & stations$interval == interval,]
  } else if (grepl("^[A-Za-z]{3}$", station)) { #TC ID
    station <- stations[stations$TC_id==station & !is.na(stations$TC_id) & stations$interval == interval,]
  } else if (grepl("^[A-Za-z]{4,}", station)) { #station name or part of
    possibilities <- dplyr::filter(stations, grepl(station, .data$station_name))
    possible_names <- possibilities$station_name
    possible_yrs <- paste0(possibilities$start, " to ", possibilities$end)
    possible_coords <- paste0(substr(possibilities$lat, 1, 7), ", ", substr(possibilities$lon, 1, 9))
    possible_interval <- possibilities$interval
    message("The following ECCC stations are possible matches for your input:")
    for (i in 1:nrow(possibilities)) {
      cat(crayon::bold$blue$underline("Choice", i, ":"), possible_names[i], crayon::bold$green(" Interval"), possible_interval[i], crayon::bold$green(" Years"), possible_yrs[i], crayon::bold$green(" Coords"), possible_coords[i], "\n")
    }
    choice <- readline(prompt =
                         writeLines(crayon::bold$red("\nChoose your desired station from the list and enter the number corresponding to the choice below:")))
    station <- possibilities[choice,]
    interval <- station$interval
  }
  if (nrow(station) < 1) {
    stop("The station you requested could not be found in my internal tables. You could try again by typing the station name (partial is ok). If that fails, try updating the internal stations table by running weathercan::stations_dl().")
  } else if (nrow(station) > 1) {
    stop("Something strange happened: we've got more than one station selected here! Check your options, but if everything looks ok you should try typing in the station name (partial is ok) and selecting form the list.")
  }

  yr_start <- substr(start, 1, 4)
  yr_end <- substr(end, 1, 4)

  if (is.na(station$end) | is.na(station$start)) {
    stop("Looks like you've selected a station with no data for the time range: the start and end years I have for that location are empty. Try again with a different interval.")
  }

  if (station$start > yr_start) {
    start <- gsub(substr(start, 1, 4), station$start, start)
    message(paste0("Your specified start date is before the actual start of records. The start date has been modified to begin in year ", station$start))
  }

  # Commented out because the weathercan package station list is broken
  # if (station$end + 1 < yr_end) {
  #   end <- gsub(substr(end, 1, 4), as.numeric(station$end) + 1, end)
  #   message(paste0("Your specified end date is after the last available records. The end date year has been modified to ", as.numeric(station$end)), ".")
  # }

  data <- suppressWarnings(weathercan::weather_dl(station$station_id, start = as.character(start), end = as.character(end), interval = interval, time_disp = tzone))

  #write the output to a .csv file for upload into Aquarius or other end use.
  if (!(is.null(save_path))) {
    utils::write.csv(data, file=paste0(save_path, "/ECCC_station", station$station_id, "_from", start, "_to", end, ".csv"), row.names=FALSE)

    writeLines(paste0("All done! Your data is in the folder ", save_path))
  }

  return(data)
}
