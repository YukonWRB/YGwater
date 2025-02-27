#' Creates snow survey template for specific survey target date and circuit
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This function creates snow survey templates for a snow survey circuit in an excel workbook. The function takes the master template and copies it into multiple sheets with circuit name, course name and snow survey target date pre-filled. The outputted template is meant to be filled out by those conducting snow surveys.
#' 
#' You should be running this function from a computer with access to the Snow database for full functionality: 
#'
#' @param target_date The target date of the snow survey, given in the form yyyy-mm-dd as a text string or Date object. Example: for the march snow survey of 2024, target_date = '2024-03-01'
#' @param circuit The circuit for which we are creating a snow survey template. Options are Carmacks, Dawson, HJ, KluaneP, Mayo, NorthSlope, OldCrow, PellyFarm, Ross, SLakes, Teslin, Watson, Whitehorse, and YEC. "all" will create a workbook for all circuits.
#' @param save_path The path where you want the circuit workbook(s) saved. Default "choose" lets you pick your folder.
#' @param snowCon A snow database connection object. If left NULL the function will attempt to connect to the snow database using function [snowConnect()] and close the connection when finished.
#' 
#' @return A snow survey template for the specified circuit(s) and target date. This excel workbook has a sheet for every snow course within the circuit and contains a summary sheet with current and previous years stats.
#'
#' @export
#'

createSnowTemplate <- function(target_date, circuit = "all", save_path = "choose", snowCon = NULL) {
  
  if (save_path == "choose") {
    if (!interactive()) {
      stop("You must specify a save path when running in non-interactive mode.")
    }
    message("Select the path to the folder where you want the workbook(s) saved.")
    save_path <- rstudioapi::selectDirectory(caption = "Select Save Folder", path = file.path(Sys.getenv("USERPROFILE"),"Desktop"))
  } else {
    if (!dir.exists(save_path)) {
      stop("The save path you specified does not exist.")
    }
  }
  
  if (inherits(target_date, "Date")) {
    target_date <- as.character(target_date)
  } else if (!inherits(target_date, "character")) {
    stop("target_date must be a character string or Date object")
  }
  
  if (length(circuit) != 1) {
    stop("parameter circuit must be a character vector of length 1, either 'all' or a specific circuit name.")
  }
  
  circuit <- tolower(circuit)
  
  if (circuit == "all") {
    circuits <- c("carmacks", "dawson", "hj", "kluanep", "mayo", "northslope", "oldcrow", "pellyfarm", "ross", "slakes", "teslin", "watson", "whitehorse", "yec")
  } else {
    circuits <- circuit
  }
  
  for (circuit in circuits) {
    
    template <- openxlsx::loadWorkbook(system.file("snow_survey/SnowSurveyTemplate.xlsx", package = "YGwater")) # reloaded each time to start from original
    
    
    #### ----------------------- Create circuits ------------------------------ ####
    if (circuit == "carmacks") {
      courses <- c("Casino Creek", "MacIntosh", "Mount Berdoe", "Mount Nansen", "Satasha Lake", "Williams Creek")
    } else if (circuit == "dawson") {
      courses <- c("Blackstone River", "Eagle Plains", "Eagle River", "Grizzly Creek", "King Solomon Dome", "King Solomon Dome Scale", "Midnight Dome", "Ogilvie River", "Riffs Ridge")
    } else if (circuit == "hj") {
      courses <- c("Beaver Creek", "Burwash Airstrip", "Chair Mountain", "Haines Junction Farm", "Summit")
    } else if (circuit == "kluanep") {
      courses <- c("Alder Creek")
    } else if (circuit == "mayo") {
      courses <- c("Calumet", "Mayo Airport A", "Mayo Airport B", "Mayo Airport C") #
    } else if (circuit == "northslope") {
      courses <- c("AK Border", "Herschel Island", "Komakuk Beach", "NWT/YK Border", "Stokes Point", "Shingle Point")
    } else if (circuit == "oldcrow") {
      courses <- c("Old Crow")
    } else if (circuit == "pellyfarm") {
      courses <- c("Pelly Farm")
    } else if (circuit == "ross") { # Snow scale/pillow locations not in  snow db. Twin Creeks A not in aquacache, but is in snow db
      courses <- c("Bonnet Plume Lake", "Burns Lake", "Edwards Lake", "Finlayson Airstrip", "Ford Lake", "Fuller Lake", "Hoole River", "Jordan Lake", "Plata Airstrip", "Rackla Lake", "Rose Creek", "Russell Lake", "Tintina Airstrip", "Twin Creeks A", "Twin Creeks B", "Withers Lake", "Twin Creeks B Snow Scale", "Withers Pillow", "Withers Scale") #
    } else if (circuit == "slakes") { # Snow scale/pillow locations not in db.
      courses <- c("Atlin (B.C.)", "Log Cabin (B.C.)", "Log Cabin Pillow (B.C.)", "Montana Mountain", "Montana Mountain Pillow", "Tagish", "Tagish Snow Scale", "Tagish Snow Pillow")
    } else if (circuit == "teslin") {
      courses <- c("Meadow Creek", "Morley Lake", "Pine Lake Airstrip")
    } else if (circuit == "watson") {
      courses <- c("Frances River", "Hyland River B", "Hyland Snow Scale", "Watson Lake Airport") # Hyland River does not exist in aquacache db, but is in snow db
    } else if (circuit == "whitehorse") { # Buckbrush snow scales is not in snow db. Whitehorse Airport is actually Whitehorse Airport A
      courses <- c("Buckbrush Snow Pillow", "Mt McIntyre B", "Whitehorse Airport", "Whitehorse Airport B_Ice Lake")
    } else if (circuit == "yec") {
      courses <- c("Aishihik Lake", "Canyon Lake")
    } else {
      stop("Circuit not recognized. Please enter a valid circuit name.")
    }
    
    ## Get maintenance for all courses
      if (is.null(snowCon)) { # Create new connection
        tryCatch({
          snowCon <- snowConnect(silent = TRUE)
          on.exit(DBI::dbDisconnect(snowCon), add = TRUE)
          maintenance <- DBI::dbGetQuery(snowCon, paste0("SELECT maintenance.maintenance, locations.location, locations.name FROM maintenance ",
                                                     "INNER JOIN locations ON maintenance.location = locations.location " ,
                                                     "WHERE completed = FALSE AND name IN ('", paste(courses, collapse = "', '"), "') ",
                                                     "AND completed = FALSE"))
          snowCon_flag <- TRUE
        }, error = function(e) {
          snowCon_flag <<- FALSE
          warning("Could not connect to snow database. Maintenance information will not be included in the report. Do you have the correct credentials in your .Renviron file?")
        })
      } else { # Use existing connection (don't close it afterwards)
        # Check that the existing connection is valid
        check <- DBI::dbGetQuery(snowCon, "SELECT 1")
        if (nrow(check) == 0) {
          snowCon_flag <<- FALSE
          warning("Existing snow database connection is invalid. Maintenance information will not be included in the report.")
        } else {
          tryCatch({
            maintenance <- DBI::dbGetQuery(snowCon, paste0("SELECT maintenance.maintenance, locations.location, locations.name FROM maintenance ",
                                                           "INNER JOIN locations ON maintenance.location = locations.location " ,
                                                           "WHERE completed = FALSE AND name IN ('", paste(courses, collapse = "', '"), "') ",
                                                           "AND completed = FALSE"))
            snowCon_flag <- TRUE
          }, error = function(e) {
            snowCon_flag <<- FALSE
            warning("Could not connect to snow database with the connection you provided. Maintenance information will not be included in the report.")
          }) # End tryCatch
        }
      }
    
    #### ------------ Add sheet for each snow course of the circuit ----------- ####
    # Put courses in alphabetical order
    courses <- sort(courses)
    # Add _ to worksheet names and remove '
    sheet_names <- gsub(" ", "_", courses)
    sheet_names <- gsub("'", "", sheet_names)
    sheet_names <- gsub("/", ".", sheet_names)
    # Clone worksheets and fill in
    for (c in 1:length(sheet_names)) {
      # Set template depending on if it is a BC site or not
      if (grepl("(B.C.)", sheet_names[c])) {
        sheet_name <- "Sheet1_bc"
      } else {sheet_name <- "Sheet1"}
      
      # Clone worksheet
      
      openxlsx::cloneWorksheet(template, sheetName = sheet_names[c], clonedSheet = sheet_name)
      # Fill in worksheet
      openxlsx::writeData(template, sheet = sheet_names[c], x = circuit, xy = c(4,4))
      openxlsx::writeData(template, sheet = sheet_names[c], x = courses[c], xy = c(4,5))
      openxlsx::writeData(template, sheet = sheet_names[c], x = target_date, xy = c(4,6))
      # Fill in maintenance
      if (snowCon_flag) {
        maint <- maintenance[maintenance$name == courses[c],]
        for (m in maint$maintenance) {
          if (m == "Brush snow course") {openxlsx::writeData(template, sheet = sheet_names[c], x = "x", xy = c(9,49))}
          if (m == "Brush helipad/access trail") {openxlsx::writeData(template, sheet = sheet_names[c], x = "x", xy = c(9,50))}
          if (m == "Replace marker plate/plates") {openxlsx::writeData(template, sheet = sheet_names[c], x = "x", xy = c(9,51))}
        }
      }
    }
    
    
    # Delete original worksheet
    openxlsx::removeWorksheet(template, "Sheet1")
    openxlsx::removeWorksheet(template, "Sheet1_bc")
    
    
    #### ------------------------- Create summary sheet ----------------------- ####
    # Pull data from snow database
    if (snowCon_flag) {
      summary <- SWE_station(year = as.numeric(substr(target_date, start = 1, stop = 4)),
                             month = as.numeric(substr(target_date, start = 7, stop = 7)),
                             return_missing = TRUE, 
                             source = "snow")
      # Subset to locations of interest and columns of interest
      summary <- summary[summary$location_name %in% courses, c("location_name", "location_id", "swe_prevyear", "swe_med")]
      # Add locations to summary that are not in database
      # Get courses that are not in summary, but in courses
      locs <- setdiff(courses, summary$location_name)
      summary <- rbind(summary, data.frame("location_name" = locs, "location_id" = rep(NA, times = length(locs)), "swe_prevyear" = rep(NA, times = length(locs)), "swe_med" = rep(NA, times = length(locs))))
      
      # Put into alphabetical order
      summary <- summary[order(summary$location_name), ]
      
      # Add data to summary worksheet
      openxlsx::writeData(template, sheet = "Summary", x = summary[1:2], xy = c(1,3), colNames = FALSE)
      openxlsx::writeData(template, sheet = "Summary", x = summary[3:4], xy = c(7,3), colNames = FALSE)
    } else {
      openxlsx::writeData(template, sheet = "Summary", x = "No DB connection", xy = c(1,3), colNames = FALSE)
      openxlsx::writeData(template, sheet = "Summary", x = "No DB connection", xy = c(7,3), colNames = FALSE)
    }
    
    # Add formula to get values from other sheets
    # Sample date
    for (s in 1:length(courses)) {
      openxlsx::writeFormula(template, sheet = "Summary", x = paste0("=IF(ISBLANK('", sheet_names[s], "'!D7), ", '"", ', "'", sheet_names[s], "'!D7)"), startCol = 3, startRow = 2 + s)
    }
    # Depth
    for (s in 1:length(courses)) {
      openxlsx::writeFormula(template, sheet = "Summary", x = paste0("='", sheet_names[s], "'!C25"), startCol = 4, startRow = 2 + s)
    }
    # Density
    for (s in 1:length(courses)) {
      openxlsx::writeFormula(template, sheet = "Summary", x = paste0("=IF('", sheet_names[s], "'!D9=", '"bulk", ', "'", sheet_names[s], "'!H23, '", sheet_names[s], "'!H25)"), startCol = 5, startRow = 2 + s)
    }
    # SWE
    for (s in 1:length(courses)) {
      openxlsx::writeFormula(template, sheet = "Summary", x = paste0("=IFERROR('", sheet_names[s], "'!G25*10, \"\")"), startCol = 6, startRow = 2 + s)
    }
    # SWE ratio
    for (s in 1:length(courses)) {
      openxlsx::writeFormula(template, sheet = "Summary", x = paste0("=IFERROR(F", 2 + s, "/H", 2 + s, "*100, ", '"")'), startCol = 9, startRow = 2 + s)
    }
    # Create hyperlink
    for (s in 1:length(courses)) {
      openxlsx::writeFormula(template, sheet = "Summary",
                             x = paste0("=HYPERLINK(", '"#', "'", sheet_names[s], "'", '!D4", "', courses[s], '")'),
                             startCol = 10, startRow = 2 + s)
    }
    # Add hyperlink to each page
    for (s in 1:length(sheet_names)) {
      openxlsx::writeFormula(template, sheet = sheet_names[s],
                             x = paste0("=HYPERLINK(", '"#', "'Summary'", '!A', 2 + s, '", "Link to summary sheet")'),
                             startCol = 2, startRow = 1)
    }
    # Add QAQC yes/no
    for (s in 1:length(courses)) {
      openxlsx::writeFormula(template, sheet = "Summary", x = paste0("=IF(ISBLANK('", sheet_names[s], "'!L25), ",'"no", "yes")'), startCol = 11, startRow = 2 + s)
    }
    # Add maintenance required yes/no
    for (s in 1:length(courses)) {
      openxlsx::writeFormula(template, sheet = "Summary", x = paste0("=IF(OR('", sheet_names[s], "'!I49<>\"\", '", sheet_names[s], "'!I50<>\"\", '", sheet_names[s], "'!I51<>\"\"), ",'"yes", "no")'), startCol = 12, startRow = 2 + s)
    }
    # Add ice yes/no
    for (s in 1:length(courses)) {
      openxlsx::writeFormula(template, sheet = "Summary", x = paste0("=IF(OR('", sheet_names[s], "'!E38<>\"\", '", sheet_names[s], "'!I38<>\"\", '", sheet_names[s], "'!B40<>\"\"), ",'"yes", "no")'), startCol = 13, startRow = 2 + s)
    }
    
    #### -------------------------- Write new template ------------------------ ####
    openxlsx::saveWorkbook(template,
                           file = paste0(save_path, "/", circuit, "_", target_date, ".xlsx"),
                           overwrite = TRUE)
  }
}
