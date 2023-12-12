#' Bridge radar reporting utility - internal
#'
#' This function generates a report of distance between the water surface and bridges or other important infrastructure. The output is a Microsoft Word document on a Yukon Government template.
#'
#' @param con A connection to the database. Default uses function [hydrometConnect()] with default settings.
#' @param locations The list of locations for which you want a distance measurement. These must either be reporting radar distance in Aquarius as Distance.Corrected or in the WRB database as type distance.
#' @param zoom Set TRUE if you want a zoomed-in plot.
#' @param zoom_days Set the number of days on the x-axis of the zoomed in plot.
#' @param save_path The path to the directory (folder) where the report should be saved. Default "choose" lets you select your folder, otherwise enter the path as a character string.
#'
#' @return A Microsoft Word report containing bridge radar information, saved in the location specified.
#' @export
#'

bridgeReport <- function(con = hydrometConnect(),
                         locations = c("29AH001", "09AH005", "29AE007", "29AB011", "29AB010"),
                         zoom = TRUE,
                         zoom_days = 30,
                         save_path = "choose"
                         )
{

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

  if (save_path == "choose") {
    message("Select the path to the folder where you want this report saved.")
    save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
  }

  #check the database connection or Aquarius credentials
  if (!is.null(con)){
    database <- con
  } else {
    if (is.null(Sys.getenv("AQPASS"))){
      stop("Your Aquarius password must be available in the .Renviron file in the form AQPASS='yourpassword'")
    }
    if (is.null(Sys.getenv("AQUSER"))){
      stop("Your Aquarius user name must be available in the .Renviron file in the form AQUSER='yourusername'")
    }
  }

  rmarkdown::render(
    input = system.file("rmd", "Bridge_report.Rmd", package = "YGwater"),
    output_file = paste0(Sys.Date(), "_Bridge-Radar-Report"),
    output_dir = save_path,
    params = list(
      database = database,
      locations = locations,
      zoom = zoom,
      zoom_days = zoom_days
    )
  )
}
