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
                         language = "english") {
  
  # year <- 2024
  # month <- 3
  # scale <- 1
  # basins <- NULL
  # save_path <- "C:/Users/gtdelapl/Desktop"
  # synchronize <- TRUE
  # language <- "english"
  
  #Check parameters
  #Language
  if (!(language %in% c("french", "english"))) {
    stop("Parameter 'language' must be one of the options: 'english' or 'french'.")
  }
  if (language == "french") {
    lc <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", "French")
    on.exit(Sys.setlocale("LC_TIME", lc), add = TRUE)
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
  
  ## Synchronize time series of interest
  # Check for credentials with read/write authority
  if (synchronize) {
    # Make sure most recent version of AquaCache R package is downloaded
    if (!rlang::is_installed("AquaCache", version = "2.3.2")) {
      stop("You must have the AquaCache package version minimum 2.3.2 installed to synchronize data with source. Please install the package and try again, or run without synchronization.")
    }
    con <- AquaCache::AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
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
        message(b, " is not a basin option and was not output in the snow bulletin word document. Please check spelling.")
      }
    }
  }
  
  
  ### Generate a snow bulletin for specified basins ###
  
  rmarkdown::render(
    input = system.file("rmd", "Snow_bulletin.Rmd", package = "YGwater"),
    output_file = paste0("Snow Bulletin ", Sys.Date()),
    output_dir = save_path,
    params = list(year = year,
                  month = month,
                  scale = scale,
                  basins = basins,
                  language = language)
  )
}
