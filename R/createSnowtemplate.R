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

#TODO: add option to create workbook for all circuits

# # For testing
# path = "C:/Users/estewart/Documents/R/Projects/"
# circuit = "Carmacks"
# target_date = "2023-03-01"
#createSnowtemplate(target_date = "2023-04-01", path = "C:/Users/estewart/Documents/R/Projects/", circuit = "Ross")


createSnowtemplate <- function(target_date, path, circuit) {

template <- openxlsx::loadWorkbook(paste0(path, "NewTemplate.xlsx"))

#### ----------------------- Create circuits ------------------------------ ####
if (circuit == "Carmacks") {
  courses <- c("Casino Creek", "MacIntosh", "Mount Berdoe", "Mount Nansen", "Satasha Lake", "Williams Creek")
}
if (circuit == "Dawson") {
  courses <- c("Blackstone River", "Eagle Plains", "Eagle River", "Grizzly Creek", "King Solomon Dome", "Midnight Dome", "Ogilvie River", "Riffs Ridge")
}
if (circuit == "HJ") {
  courses <- c("Beaver Creek", "Burwash Airstrip", "Chair Mountain", "Haines Junction Farm", "Summit")
}
if (circuit == "KluaneP") {
  courses <- c("Alder Creek")
}
if (circuit == "Mayo") {
  courses <- c("Calumet", "Mayo Airport A", "Mayo Airport B", "Mayo Airport C") #
}
# North slope locations don't currently exist in database
if (circuit == "NorthSlope") {
  courses <- c("AK Border", "Herschel Island", "Komakuk", "NWT/YK Border", "Stakes", "Shingle Point")
}
if (circuit == "OldCrow") {
  courses <- c("Old Crow")
}
if (circuit == "PellyFarm") {
  courses <- c("Pelly Farm")
}
# Snow scale/pillow locations not in db. Twin Creeks not in hydromet, but in snow db
if (circuit == "Ross") {
  courses <- c("Bonnet Plume Lake", "Burns Lake", "Edwards Lake", "Finlayson Airstrip", "Ford Lake", "Fuller Lake", "Hoole River", "Jordan Lake", "Plata Airstrip", "Rackla Lake", "Rose Creek", "Russell Lake", "Tintina Airstrip", "Twin Creeks A", "Twin Creeks B", "Withers Lake", "Twin Creeks B Snow Scale", "Withers Pillow", "Withers Scale") #
}
# Snow scale/pillow locations not in db.
if (circuit == "SLakes") {
  courses <- c("Atlin (B.C)", "Log Cabin (B.C.)", "Montana Mountain", "Tagish", "Tagish Snow Scale", "Tagish Snow Pillow")
}
if (circuit == "Teslin") {
  courses <- c("Meadow Creek", "Morley Lake", "Pine Lake Airstrip")
}
# Hyland River does not exist in hydromet db, but is in snow db
if (circuit == "Watson") {
  courses <- c("Frances River", "Hyland River B", "Hyland Snow Scale", "Watson Lake Airport", "Hyland River") #
}
# Buckbrush snow scales is not in db. Whitehorse Airport A is actually Whitehorse Airport A
if (circuit == "WRB") {
  courses <- c("Buckbrush Snow Scales", "Mt McIntyre B", "Whitehorse Airport", "Whitehorse Airport B")
}
if (circuit == "YEC") {
  courses <- c("Aishihik Lake", "Canyon Lake")
}

#### ------------ Add sheet for each snow course of the circuit ----------- ####
# Put courses in alphabetical order
  courses <- sort(courses)
# Add _ to worksheet names and remove '
  courses2 <- gsub(" ", "_", courses)
  courses2 <- gsub("'", "", courses2)
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
                         return_missing = TRUE, source = "snow")
  # Subset to locations of interest and columns of interest
  summary <- summary[summary$location_name %in% courses, c("location_name", "location_id", "swe_prevyear", "swe_med")]
  # Add locations to summary that are not in database
    # Get courses that are not in summary, but in courses
  locs <- setdiff(courses, summary$location_name)
  summary <- rbind(summary, data.frame("location_name"=locs, "location_id"=rep(NA, times=length(locs)), "swe_prevyear"=rep(NA, times=length(locs)), "swe_med"=rep(NA, times=length(locs))))

  # Put into alphabetical order
  summary <- summary[order(summary$location_name), ]

  # Add data to summary worksheet
  openxlsx::writeData(template, sheet = "Summary", x = summary[1:2], xy = c(1,3), colNames = FALSE)
  openxlsx::writeData(template, sheet = "Summary", x = summary[3:4], xy = c(7,3), colNames = FALSE)
  # Add formula to get values from other sheets
  # Sample date
  for (s in 1:length(courses)) {
    openxlsx::writeFormula(template, sheet="Summary", x=paste0("=IF(ISBLANK('", courses2[s], "'!D7), ", '"", ', "'", courses2[s], "'!D7)"), startCol=3, startRow=2+s)
  }
  # Depth
  for (s in 1:length(courses)) {
    openxlsx::writeFormula(template, sheet="Summary", x=paste0("='", courses2[s], "'!C25"), startCol=4, startRow=2+s)
  }
  # Density
  for (s in 1:length(courses)) {
    openxlsx::writeFormula(template, sheet="Summary", x=paste0("=IF('", courses2[s], "'!D9=", '"bulk", ', "'", courses2[s], "'!H23, '", courses2[s], "'!H25)"), startCol=5, startRow=2+s)
  }
  # SWE
  for (s in 1:length(courses)) {
    openxlsx::writeFormula(template, sheet="Summary", x=paste0("=IFERROR(IF('", courses2[s], "'!D9=", '"bulk", ', "'", courses2[s], "'!G23*10, '", courses2[s], "'!G25*10), ", '"")'), startCol=6, startRow=2+s)
  }
  # SWE ratio
  for (s in 1:length(courses)) {
    openxlsx::writeFormula(template, sheet="Summary", x=paste0("=IFERROR(F", 2+s, "/H", 2+s, "*100, ", '"")'), startCol=9, startRow=2+s)
  }
  # Create hyperlink
  for (s in 1:length(courses)) {
    openxlsx::writeFormula(template, sheet="Summary",
                          x = paste0("=HYPERLINK(", '"#', "'", courses2[s], "'", '!D4", "', courses[s], '")'),
                           startCol=10, startRow=2+s)
  }
  # Add hyperlink to each page
  for (s in 1:length(courses2)) {
    openxlsx::writeFormula(template, sheet=courses2[s],
                           x = paste0("=HYPERLINK(", '"#', "'Summary'", '!A', 2+s, '", "Link to summary sheet")'),
                           startCol=2, startRow=1)
  }
  # Add QAQC yes/no
  for (s in 1:length(courses)) {
    openxlsx::writeFormula(template, sheet="Summary", x=paste0("=IF(ISBLANK('", courses2[s], "'!L25), ",'"no", "yes")'), startCol=11, startRow=2+s)
  }

# Write new template (template_test)
openxlsx::saveWorkbook(template, file=paste0(path, paste0(circuit, "_", target_date, ".xlsx")), overwrite = TRUE)

}
