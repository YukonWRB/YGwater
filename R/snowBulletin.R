#' Function for creating snow bulletin (Rmarkdown)- internal
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' This function generates the snow bulletin for a specified date as a Microsoft Word document on a Yukon Government template.
#'
#' This function fetches data from the local postgresql hydrometric database created/maintained by the AquaCache package; see details for more info on database connections.
#'
#' @details
#' To download data, you MUST have the database credentials loaded into your .Renviron profile. See function [AquaConnect()] for more information, and contact the database administrator/data scientist for credentials or help.
#' 
#' If you also wish to synchronize timeseries on our database with new data provided by others (such as when the WSC adjusts or publishes flow values) you must also have write credentials to the database. Connection for this will be done with [AquaCache::AquaConnect()]. Please talk to the Data Scientist to get these credentials.
#'
#' @param year Year for which the snow bulletin is to be created.
#' @param month Month for which the snow bulletin is to be created. Options are 3, 4 or 5.
#' @param scale Scale of the snow bulletin plots. Default is 1. Enter a scale number above or below 1 to get larger or smaller titles and axis labels.
#' @param basins The name of the sub_basin you wish to generate. One or many of "Upper Yukon", "Teslin", "Central Yukon", "Pelly", "Stewart", "White", "Lower Yukon", "Porcupine", "Peel", "Liard", "Alsek". North Slope will be added when AquaCache is updated with the new snow database. Default is NULL, where all basins are shown in bulletin.
#' @param save_path The path to the directory (folder) where the report should be saved. Enter the path as a character string.
#' @param synchronize Should the timeseries be synchronized with source data? If TRUE, all timeseries used in the snow bulletin will be synchronized. If FALSE (default), none will be synchronized. This requires installation of the AquaCache package (installed or updated each time this function is run with synchronize = TRUE) as well as write privileges to the database. See Details for more info.
#' @param language The language of the snow bulletin. Currently only changes language of plots. Options are "english" and "french". Default is "english".
#' @param precip_period The period to use for precipitation stats. Options are "last 40 years", "all years" (all years of record), "1981-2010" (old climate normal period), "1991-2020" (current climate normal period). Default is "last 40 years".
#' @param cddf_period The period to use for the cumulative degree day plot historic range. Options are "last 40 years", "all years" (all years of record), "1981-2010" (old climate normal period), "1991-2020" (current climate normal period). Default is "last 40 years".
#' @param snow_period The period to use for the snow survey plot historic range. Options are "all years" (all years of record), "last 40 years". Default is "all years". CURRENTLY NOT DOING ANYTHING FOR THE PLOTS, JUST FOR THE TEXT.
#' @param water_period The period to use for the water level/flow historic ranges. Options are "all years" (all years of record), "last 40 years". Default is "all years". CURRENTLY NOT DOING ANYTHING FOR THE PLOTS, JUST FOR THE TEXT.
#' @param lookback The number of past years to consider for all plots and statistics. Default is 30. NOT CURRENTLY USED, but will replace other _period parameters.
#' @param con A connection to the AquaCache database. If left NULL connection will be attempted with function [AquaConnect()] using default arguments. Note that if synchronize = TRUE this connection must have edit privileges to the database!!!
#'
#' @return A snow bulletin in Microsoft Word format.
#'
#' @export
#'

snowBulletin <- function(year,
                         month,
                         scale = 1,
                         basins = NULL,
                         save_path = 'choose',
                         synchronize = FALSE,
                         language = "english",
                         precip_period = "last 40 years",
                         cddf_period = "last 40 years",
                         snow_period = "all years",
                         water_period = "all years",
                         lookback = 40,
                         con = NULL) {

  #Check parameters
  #Language
  if (!(language %in% c("french", "english"))) {
    stop("Parameter 'language' must be one of the options: 'english' or 'french'.")
  }
  
  # precip_period
  if (!(precip_period %in% c("last 40 years", "all years", "1981-2010", "1991-2020"))) {
    stop("Parameter 'precip_period' must be one of the options: 'last 40 years', 'all years', '1981-2010', '1991-2020'.")
  }
  # cddf_period
  if (!(cddf_period %in% c("last 40 years", "all years", "1981-2010", "1991-2020"))) {
    stop("Parameter 'cddf_period' must be one of the options: 'last 40 years', 'all years', '1981-2010', '1991-2020'.")
  }
  # snow_period
  # if (!(snow_period %in% c("last 40 years", "all years", "1981-2010", "1991-2020"))) {
  #   stop("Parameter 'snow_period' must be one of the options: 'all years', 'last 40 years', '1981-2010', '1991-2020'.")
  # }
  if (!(snow_period == "all years")) {
    stop("Parameter 'snow_period' must be 'all years', at least until the underlying functions and bulletin code can handle other options.")
  }
  # water_period
  # if (!(water_period %in% c("last 40 years", "all years", "1981-2010", "1991-2020"))) {
  #   stop("Parameter 'water_period' must be one of the options: 'all years', 'last 40 years', '1981-2010', '1991-2020'.")
  # }
  if (!(water_period == "all years")) {
    stop("Parameter 'water_period' must be 'all years', at least until the underlying functions and bulletin code can handle other options.")
  }
  
  # Make sure officer is installed
  rlang::check_installed("officer", reason = "necessary to create word document with special formatting using Rmarkdown.")
  # Make sure officedown is installed
  rlang::check_installed("officedown", reason = "necessary to create word document with special formatting using Rmarkdown.")
  # Make sure knitr is installed
  rlang::check_installed("knitr", reason = "necessary to create a report using Rmarkdown.")
  rlang::check_installed("flextable", reason = "necessary to create report tables.")
  
  # Select save path
  if (!is.null(save_path)) {
    if (save_path %in% c("Choose", "choose")) {
      # print("Select the folder where you want this graph saved.")
      save_path <- rstudioapi::selectDirectory(caption = "Select Save Folder", path = file.path(Sys.getenv("USERPROFILE"), "Desktop"))
    } else {
      if (!dir.exists(save_path)) {
        stop("The directory you pointed to with parameter 'save_path' does not exist")
      }
    }
  }
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }
  
  ## Synchronize time series of interest
  # Check for credentials with read/write authority
  if (synchronize) {
    # Make sure most recent version of AquaCache R package is downloaded
    if (!rlang::is_installed("AquaCache", version = "2.3.3")) {
      stop("You must have the AquaCache package version minimum 2.3.3 installed to synchronize data with source. Please install the package and try again, or run without synchronization.")
    }
    if (DBI::dbIsReadOnly(con)) {
      message("User does not have read/write database privileges required for synchronizing data with source. Data was not synchronized.")
    } else {
      message("Synchronizing necessary timeseries. This could take a while, please be patient.")
      # TODO: this now call several locations which are part of the 'sample_series' table.
      target_sample_series <- DBI::dbGetQuery(con, "SELECT sample_series_id FROM sample_series WHERE source_fx = 'downloadSnowCourse'") # Snow survey sites 
      AquaCache::synchronize_discrete(con = con, 
                                      sample_series_id = target_sample_series$sample_series_id,
                                      start_datetime = paste0(year - 1, "-09-01"))
      AquaCache::synchronize_continuous(con = con, 
                                        timeseries_id = c(20, 145, 51, 75, 122, # For plot A
                                                          649, 217, 85, 317, # For other plot A
                                                          #663, 665, 666, 668, 664, 671, 667, # For plot c (cannot be synchronized)
                                                          484, 532, 540, 500, 548, 492, 556, 508, # For plot D
                                                          30, 31, 38, 48, 57, 81, 69, 71, 107, 132, 110, 14), 
                                        start_datetime = paste0(year - 1, "-09-01"))
    }
  }
  
  ### Generate a snow bulletin for the whole territory###
  if (is.null(basins)) {
    basins <- c("Upper Yukon", "Teslin", "Central Yukon", "Pelly", "Stewart", "White", 
                "Lower Yukon", "Porcupine", "Peel", "Liard", "Alsek")
  } else {
    # Check that basin names are correct
    for (b in basins) {
      if (!(b %in%  c("Upper Yukon", "Teslin", "Central Yukon", "Pelly", "Stewart", "White", 
                      "Lower Yukon", "Porcupine", "Peel", "Liard", "Alsek"))) {
        basins <- basins[!basins %in% b]
        message(b, " is not a basin option and was not output in the snow bulletin word document. Please check spelling.")
      }
    }
    if (length(basins) == 0) {
      stop("There are no valid basins requested. Please check the basin names and try again.")
    }
  }
  
  ### Generate a snow bulletin for specified basins ###

  
  rmarkdown::render(
    input = system.file("rmd", "Snow_bulletin.Rmd", package = "YGwater"),
    output_file = paste0("Snow Bulletin ", year, "-0", month, " issued ", Sys.Date()),
    output_dir = save_path,
    params = list(year = year,
                  month = month,
                  scale = scale,
                  basins = basins,
                  language = language,
                  precip_period = precip_period,
                  cddf_period = cddf_period,
                  # snow_period = snow_period,
                  # water_period = water_period,
                  # lookback = lookback,
                  con = con)
  )
}
