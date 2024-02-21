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
#'
#' @return A snow bulletin in Microsoft Word format.
#'
#' @export
#'

#TODO:

# snowBulletin(year = 2023, month = 3, scale = 1, basins = "Upper Yukon", save_path = "C:/Users/estewart/Documents/R/Projects/YGwater")

snowBulletin <-
  function(year,
           month,
           scale = 1,
           basins = NULL,
           save_path) {

    # Make sure knitr is installed
    rlang::check_installed("knitr", reason = "necessary to create a report using Rmarkdown.")
    if (!rlang::is_installed("knitr")) { #This is here because knitr is not a 'depends' of this package; it is only necessary for this function and is therefore in "suggests"
      message("Installing dependency 'knitr'...")
      utils::install.packages("knitr")
      if (rlang::is_installed("knitr")){
        message("Package knitr successfully installed.")
      } else {
        stop("Failed to install package knitr You could troubleshoot by running utils::install.packages('knitr') by itself.")
      }
    }

    # Choose save_path
    if (save_path == "choose") {
      message("Select the path to the folder where you want this report saved.")
      save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
    }

    ### Generate a snow bulletin for the whole territory###
    if (is.null(basins) == TRUE) {

        basins <- c("Upper Yukon", "Teslin", "Central Yukon", "Pelly", "Stewart", "White", "Lower Yukon", "Porcupine", "Peel", "Liard", "Alsek")

        rmarkdown::render(
          input = system.file("rmd", "Snow_bulletin.Rmd", package="YGwater"),
          output_file = paste0("Snow Bulletin ", Sys.Date()),
          output_dir = save_path,
          params = list(year = year,
                        month = month,
                        scale = scale,
                        basins = basins)
        )
      } #End of territory report

    ### Generate a snow bulletin for specified basins ###
    if (is.null(basins) == FALSE) {

      rmarkdown::render(
        input = system.file("rmd", "Snow_bulletin.Rmd", package="YGwater"),
        output_file = paste0("Snow Bulletin ", Sys.Date()),
        output_dir = save_path,
        params = list(year = year,
                      month = month,
                      scale = scale,
                      basins = basins)
      )
    } #End of report

  }
