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
# path = "C:/Users/estewart/Documents/R/Projects/NewTemplate_test.xlsx"
# circuit = "Carmacks"
# target_date = "2023-04-01"
# template = "NewTemplate_test.xlsx"
#createSnowtemplate(target_date = "2024-03-01", path = "C:/Users/estewart/Documents/R/Projects/NewTemplate.xlsx", circuit = "Carmacks")


createSnowtemplate <- function(target_date, path, circuit) {

template <- openxlsx::loadWorkbook(path)

#### ----------------------- Create circuits ------------------------------ ####
if (circuit == "Carmacks") {
  #courses <- c("09CD-SC01", "09CA-SC02", "09AH-SC01", "09CA-SC01", "09AH-SC03", "09AH-SC04")
  courses <- c("Casino Creek", "MacIntosh", "Mount Berdoe", "Mount Nansen", "Satasha Lake", "Williams Creek")
}
if (circuit == "Dawson") {
  #courses <- c("09EB-SC01", "09FB-SC01", "09FB-SC02", "09EA-SC02", "10MA-SC02", "10MA-SC01", "09EA-SC01", "09FA-SC01")
  courses <- c("Blackstone River", "Eagle Plains", "Eagle River", "Grizzly Creek", "King Solomon Dome", "Midnight Dome", "Ogilvie River", "Riffs Ridge")
}
if (circuit == "HJ") {
  #courses <- c("09CA-SC03", "08AA-SC04", "09CB-SC01", "08AB-SC03", "09CB-SC02")
  courses <- c("Beaver Creek", "Burwash Airstrip", "Chair Mountain", "Haines Junction Farm", "Summit")
}
if (circuit == "KluaneP") {
  #courses <- c("08AA-SC02")
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
if (circuit == "Whitehorse") {
  courses <- c("Buckbrush Snow Pillow", "Mt McIntyre B", "Whitehorse Airport", "Whitehorse Airport B")
}
if (circuit == "YEC") {
  courses <- c("Aishihik Lake", "Canyon Lake")
}

## Get maintenance for all courses
con <- snowConnect()
maintenance <- DBI::dbGetQuery(con, paste0("SELECT maintenance.maintenance, locations.location, locations.name FROM maintenance ",
                                           "INNER JOIN locations ON maintenance.location = locations.location " ,
                                           "WHERE completed = FALSE AND name IN ('", paste(courses, collapse = "', '"), "') ",
                                           "AND completed = FALSE"))
DBI::dbDisconnect(con)


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
  # Fill in maintenance
  maint <- maintenance[maintenance$name == courses[c],]
  for (m in maint$maintenance) {
    if (m == "Brush snow course") {openxlsx::writeData(template, sheet = courses2[c], x = "x", xy = c(9,49))}
    if (m == "Brush helipad/access trail") {openxlsx::writeData(template, sheet = courses2[c], x = "x", xy = c(9,50))}
    if (m == "Replace marker plate/plates") {openxlsx::writeData(template, sheet = courses2[c], x = "x", xy = c(9,51))}
  }
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
    openxlsx::writeFormula(template, sheet="Summary", x=paste0("=IFERROR('", courses2[s], "'!G25*10, \"\")"), startCol=6, startRow=2+s)
  }
  # for (s in 1:length(courses)) {
  #   openxlsx::writeFormula(template, sheet="Summary", x=paste0("=IFERROR(IF('", courses2[s], "'!D9=", '"bulk", ', "'", courses2[s], "'!G23*10, '", courses2[s], "'!G25*10), ", '"")'), startCol=6, startRow=2+s)
  # }
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
  # Add maintenance required yes/no
  for (s in 1:length(courses)) {
    openxlsx::writeFormula(template, sheet="Summary", x=paste0("=IF(OR('", courses2[s], "'!I49<>\"\", '", courses2[s], "'!I50<>\"\", '", courses2[s], "'!I51<>\"\"), ",'"yes", "no")'), startCol=12, startRow=2+s)
  }
  # Add ice yes/no
  for (s in 1:length(courses)) {
    openxlsx::writeFormula(template, sheet="Summary", x=paste0("=IF(OR('", courses2[s], "'!E38<>\"\", '", courses2[s], "'!I38<>\"\", '", courses2[s], "'!B40<>\"\"), ",'"yes", "no")'), startCol=13, startRow=2+s)
  }

# Write new template (template_test)
openxlsx::saveWorkbook(template, file=paste0(sub("/[^/]+$", "/", path), paste0(circuit, "_", target_date, ".xlsx")), overwrite = TRUE)

}
