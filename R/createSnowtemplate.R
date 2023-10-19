#' Creates snow survey template for specific survey target date and circuit
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function creates snow survey templates for a snow survey circuit in an excel workbook. The function takes the master template and copies it into multiple sheets with circuit name, course name and snow survey target date pre-filled. The outputted template is meant to be filled out by those conducting snow surveys.
#'
#' @param target_date The target date of the snow survey, given in the form yyyy-mm-dd. Example: for the march snow survey of 2024, target_date = '2024-03-01'
#' @param path The path to the master snow survey template. This will be removed when the template is added to package data.
#' @param circuit The circuit for which we are creating a snow survey template. Options are Carmacks, Dawson, HJ, KluaneP, Mayo, North Slope, Old Crow, PellyFarm, Ross, SLakes, Teslin, Watson, WRB, and YEC.
#'
#' @return A snow survey template called template_test (currently) for the specified circuit and target date. This excel workbook has a sheet for every snow course within the circuit and contains a summary sheet with current and previous years stats.
#'
#' @export
#'

# For testing
# path = "C:/Users/estewart/Documents/R/Projects/"
# circuit = "Carmacks"
# target_date = "2023-03-01"
# createSnowtemplate(target_date = "2023-03-01", path = "C:/Users/estewart/Documents/R/Projects/", circuit = "Carmacks")

createSnowtemplate <- function(target_date, path, circuit) {

template <- openxlsx::loadWorkbook(paste0(path, "NewTemplate.xlsx"))

if (circuit == "Carmacks") {
  courses <- c("Casino Creek", "MacIntosh", "Mount Berdoe", "Mount Nansen", "Satasha Lake", "Williams Creek")
}

if (circuit == "Dawson") {
  courses <- c("Blackstone River", "Eagle Plains", "Eagle River", "Grizzly Creek", "King Solomon Dome", "Midnight Dome", "Ogilvie River", "Riff's Ridge")
}

# Clone worksheets and fill in
for (c in courses) {
  # Clone worksheet
  openxlsx::cloneWorksheet(template, sheetName = c, clonedSheet = "Sheet1")
  # Fill in worksheet
  openxlsx::writeData(template, sheet = c, x = circuit, xy = c(4,4))
  openxlsx::writeData(template, sheet = c, x = c, xy = c(4,5))
  openxlsx::writeData(template, sheet = c, x = target_date, xy = c(4,6))

}

### Add summary sheet
  openxlsx::addWorksheet(template, sheetName = "Summary")
  # Pull data from hydro database
  summary <- SWE_station(year = as.numeric(substr(target_date, start = 1, stop = 4)),
                         month = as.numeric(substr(target_date, start = 7, stop = 7)),
                         return_missing = TRUE)
  # Subset to locations of interest
  summary <- summary[summary$location_name %in% courses, ]

  # Add data to summary worksheet
  openxlsx::writeData(template, sheet = "Summary", x = summary, xy = c(1,2))

# Write new template (template_test)
openxlsx::saveWorkbook(template, file=paste0(path, "template_test.xlsx"), overwrite = TRUE)

}
