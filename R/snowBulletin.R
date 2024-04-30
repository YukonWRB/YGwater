#' Function for creating snow bulletin (Rmarkdown)- internal
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function generates the snow bulletin for a specified date. The output is a Microsoft Word document on a Yukon Government template.
#'
#' This function fetches data from the local postgresql hydrometric database created/maintained by the HydroMetDB package.
#'
#' @details
#' To download data, you MUST have your hydromet credentials loaded
#' into your .Renviron profile as values pairs of hydrometHost="10.250.12.154", hydrometPort="5433", hydrometUser="hydromet_read", hydrometPass="hydromet".
#'
#' @param year Year for which the snow bulletin is to be created.
#' @param month Month for which the snow bulletin is to be created. Options are 3, 4 or 5.
#' @param scale Scale of the snow bulletin plots. Default is 1. Enter a scale number above or below 1 to get larger or smaller titles and axis labels.
#' @param basins The name of the sub_basin you wish to generate. One or many of "Upper Yukon", "Teslin", "Central Yukon", "Pelly", "Stewart", "White", "Lower Yukon", "Porcupine", "Peel", "Liard", "Alsek". North Slope will be added when hydromet is updated with the new snow database. Default is NULL, where all basins are shown in bulletin.
#' @param save_path The path to the directory (folder) where the report should be saved. Enter the path as a character string.
#' @param synchronize Should the timeseries be synchronized with source data? If TRUE, all timeseries used in the snow bulletin will be synchronized. If FALSE (default), none will be synchronized. 
#' @param language The language of the snow bulletin. Currently only changes language of plots. Options are "english" and "french". Default is "english".
#'
#' @return A snow bulletin in Microsoft Word format.
#'
#' @export
#'

#TODO:

# snowBulletin(year=2024,month=5, scale = 1, basins = NULL, save_path = "Choose", synchronize=FALSE) 

snowBulletin <-
  function(year,
           month,
           scale = 1,
           basins = NULL,
           save_path,
           synchronize=FALSE,
           language="english") {

    #Check parameters
    #Language
    if (!(language %in% c("french", "english"))) {
      stop("Parameter 'language' must be one of the options: 'english' or 'french'.")
    }
    if (language=="french") {
      lc <- Sys.getlocale("LC_TIME")
      Sys.setlocale("LC_TIME", "French")
      on.exit(Sys.setlocale("LC_TIME", lc), add=TRUE)
    }

    # Make sure officer is installed
    rlang::check_installed("officer", reason = "necessary to create word document with special formatting using Rmarkdown.")

    # Make sure officer is installed
    rlang::check_installed("officedown", reason = "necessary to create word document with special formatting using Rmarkdown.")

    # Make sure knitr is installed
    rlang::check_installed("knitr", reason = "necessary to create a report using Rmarkdown.")
    rlang::check_installed("HydroMetDB", reason = "necessary to synchronize data with source.")

    ## Synchronize time series of interest
    # Check for credentials with read/write authority
    if (synchronize) {
      if (DBI::dbIsReadOnly(HydroMetDB::hydrometConnect(silent = TRUE))) {
        message("User does not have read/write database privileges required for synchronizing data with source. Data was not synchronized.")
      } else {
        # Make sure most recent version of HydroMetDB is downloaded
        remotes::install_github("YukonWRB/HydroMetDB")
        # Synchronize
        HydroMetDB::synchronize(con = HydroMetDB::hydrometConnect(silent = TRUE), 
                                timeseries_id = c(20, 145, 51, 75, 122, # For plot A
                                                  649, 217, 85, 317, # For other plot A
                                                  #663, 665, 666, 668, 664, 671, 667, # For plot c (cannot be synchronized)
                                                  484, 532, 540, 500, 548, 492, 556, 508, # For plot D
                                                  30, 31, 38, 48, 57, 81, 69, 71, 107, 132, 110, 14, # For plot E
                                                  323, 324, 209, 210, 211, 212, # Alaska snow survey sites
                                                  189:356), # Snow survey sites 
                                start_datetime = paste0(year-1, "-09-01"), discrete = TRUE)
      }
      
    }
    
    
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
    
    ### Generate a snow bulletin for the whole territory###
    if (is.null(basins) == TRUE) {

        basins <- c("Upper Yukon", "Teslin", "Central Yukon", "Pelly", "Stewart", "White", 
                    "Lower Yukon", "Porcupine", "Peel", "Liard", "Alsek")

    } else {
    # Check that basin names are correct
    for (b in basins) {
      if (!(b %in%  c("Upper Yukon", "Teslin", "Central Yukon", "Pelly", "Stewart", "White", 
                          "Lower Yukon", "Porcupine", "Peel", "Liard", "Alsek"))){
        message(paste0(b, " is not a basin option and was not outputted in the snow bulletin word document. Please check spelling."))
      }
    }
    
    }
    

    ### Generate a snow bulletin for specified basins ###
    
      rmarkdown::render(
        input = system.file("rmd", "Snow_bulletin.Rmd", package="YGwater"),
        output_file = paste0("Snow Bulletin ", Sys.Date()),
        output_dir = save_path,
        params = list(year = year,
                      month = month,
                      scale = scale,
                      basins = basins,
                      language = language)
      )


  }
