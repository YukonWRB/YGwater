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

# TODO: (3) add conditional formatting to summary sheet to colour when QAQC is complete
# TODO: (4) Link in summary to sheet

# # For testing
# path = "C:/Users/estewart/Documents/R/Projects/"
# circuit = "Carmacks"
# target_date = "2023-03-01"
#createSnowtemplate(target_date = "2023-03-01", path = "C:/Users/estewart/Documents/R/Projects/", circuit = "Carmacks")


createSnowtemplate <- function(target_date, path, circuit) {

template <- openxlsx::loadWorkbook(paste0(path, "NewTemplate.xlsx"))

#### ----------------------- Create circuits ------------------------------ ####
if (circuit == "Carmacks") {
  courses <- c("Casino Creek", "MacIntosh", "Mount Berdoe", "Mount Nansen", "Satasha Lake", "Williams Creek")
}
if (circuit == "Dawson") {
  courses <- c("Blackstone River", "Eagle Plains", "Eagle River", "Grizzly Creek", "King Solomon Dome", "Midnight Dome", "Ogilvie River", "Riff's Ridge")
}
if (circuit == "HJ") {
  courses <- c("Beaver Creek", "Burwash Airstrip", "Chair Mountain", "Haines Junction Farm", "Summit")
}
if (circuit == "KluaneP") {
  courses <- c("Alder Creek")
}
if (circuit == "Mayo") {
  courses <- c("Calumet", "Mayo Airpot A", "Mayo Airport B", "Mayo Airport C")
}
if (circuit == "NorthSlope") {
  courses <- c("AK Border", "Herschel Island", "Komakuk", "NWT/YK Border", "Stakes", "Shingle Point")
}
if (circuit == "OldCrow") {
  courses <- c("Old Crow")
}
if (circuit == "PellyFarm") {
  courses <- c("Pelly Farm")
}
if (circuit == "Ross") {
  courses <- c("Bonnet Plume Lake", "Burns Lake", "Edwards Lake", "Finlayson Airstrip", "Ford Lake", "Fuller Lake", "Hoole River", "Jordan Lake", "Plata Airstrip", "Rackla Lake", "Rose Creek", "Russell Lake", "Tintina Airstrip", "Twin Creeks A", "Twin Creeks B", "Twin Creeks B Snow Scale", "Withers Lake", "Withers Pillow", "Withers Scale")
}
if (circuit == "SLakes") {
  courses <- c("Atlin (B.C)", "Log Cabin (B.C.)", "Montana Mountain", "Tagish Snow Scale", "Tagish Snow Pillow", "Tagish")
}
if (circuit == "Teslin") {
  courses <- c("Meadow Creek", "Morley Lake", "Pine Lake Airstrip")
}
if (circuit == "Watson") {
  courses <- c("Frances River", "Hyland River", "Hyland River B", "Hyland Snow Scale", "Watson Lake Airport")
}
if (circuit == "WRB") {
  courses <- c("Buckbrush Snow Scales", "Mt McIntyre B", "Whitehorse Airport A", "Whitehorse Airport B")
}
if (circuit == "YEC") {
  courses <- c("Aishihik Lake", "Canyon Lake")
}

#### ------------ Add sheet for each snow course of the circuit ----------- ####
# Add _ to worksheet names
  courses2 <- gsub(" ", "_", courses)
# Clone worksheets and fill in
for (c in 1:length(courses2)) {
  # Clone worksheet
  openxlsx::cloneWorksheet(template, sheetName = courses2[c], clonedSheet = "Sheet1")
  # Fill in worksheet
  openxlsx::writeData(template, sheet = courses2[c], x = circuit, xy = c(4,4))
  openxlsx::writeData(template, sheet = courses2[c], x = courses[c], xy = c(4,5))
  openxlsx::writeData(template, sheet = courses2[c], x = target_date, xy = c(4,6))

}
# Delete original worksheet
openxlsx::removeWorksheet(template, "Sheet1")

#### ------------------------- Create summary sheet ----------------------- ####
  # Pull data from hydro database
  summary <- SWE_station(year = as.numeric(substr(target_date, start = 1, stop = 4)),
                         month = as.numeric(substr(target_date, start = 7, stop = 7)),
                         return_missing = TRUE)
  # Subset to locations of interest
  summary <- summary[summary$location_name %in% courses, ]
  # Put into alphabetical order
  summary <- summary[order(summary$location_name), ]

  # Add data to summary worksheet
  openxlsx::writeData(template, sheet = "Summary", x = summary[1:3], xy = c(1,3), colNames = FALSE)
  openxlsx::writeData(template, sheet = "Summary", x = summary[6:7], xy = c(7,3), colNames = FALSE)
  # Add formula to get values from other sheets
  # Sample date
  for (s in 1:length(courses)) {
    openxlsx::writeFormula(template, sheet="Summary", x=paste0("='", courses2[s], "'!D7"), startCol=3, startRow=2+s)
  }
  # Depth
  for (s in 1:length(courses)) {
    openxlsx::writeFormula(template, sheet="Summary", x=paste0("='", courses2[s], "'!C25"), startCol=4, startRow=2+s)
  }
  # Density
  for (s in 1:length(courses)) {
    openxlsx::writeFormula(template, sheet="Summary", x=paste0("='", courses2[s], "'!H25"), startCol=5, startRow=2+s)
  }
  # SWE
  for (s in 1:length(courses)) {
    openxlsx::writeFormula(template, sheet="Summary", x=paste0("='", courses2[s], "'!G25*10"), startCol=6, startRow=2+s)
  }
  # SWE ratio
  for (s in 1:length(courses)) {
    openxlsx::writeFormula(template, sheet="Summary", x=paste0("= F", 2+s, "/H", 2+s, "*100"), startCol=9, startRow=2+s)
  }
  # Create hyperlink
  for (s in 1:length(courses)) {
    openxlsx::writeFormula(template, sheet="Summary",
                          x = paste0("=HYPERLINK(", '"#', "'", courses2[s], "'", '!D4", "', courses[s], '")'),
                           startCol=10, startRow=2+s)
  }

  # Add hyperlink to each page
  for (s in 1:length(courses2)) {
    # openxlsx::writeFormula(template, sheet=courses2[s],
    #                        x = paste0("=HYPERLINK(", '"#', "'Summary'", '!A', 2+s, '", "', courses[s], '")'),
    #                        startCol=2, startRow=1)
    openxlsx::writeFormula(template, sheet=courses2[s],
                           x = paste0("=HYPERLINK(", '"#', "'Summary'", '!A', 2+s, '", "Link to summary sheet")'),
                           startCol=2, startRow=1)
  }

# Write new template (template_test)
openxlsx::saveWorkbook(template, file=paste0(path, "template_test.xlsx"), overwrite = TRUE)

}
