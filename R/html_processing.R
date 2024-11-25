#' In Situ html logger file processing
#'
#' @param html_file File name of .html file, must be in the logger dropbox folder 
#' @param master_sheet Path to YOWN master sheet
#' @param logger_tracking Path to YOWN logger tracking sheet
#' @param YOWN_logger_folder Path to YOWN logger dropbox folder
#'
#' @return Moves YOWN html file to backups folder and appropriate YOWN Active Wells folder. Uploads data to Aquarius after performing unit checks and conversions. Adds pressure column if missing, based on depth column and conversions calcs on In Situ website
#' 
#' @description Parse YOWN html logger files, upload to Aquarius, sort into respective folder, and track logger metadata. Used "Pressure" column after converting to m water column. If pressure column does not exist, converts from deth using formula supplied by In Situ


html_processing <- function(html_file,
                            master_sheet = "G:/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/2_SPREADSHEETS/1_YOWN_MASTER_TABLE/YOWN_MASTER.xlsx",
                            logger_tracking = "G:/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/2_SPREADSHEETS/3_OTHER/YOWN_Logger_Tracking.xlsx",
                            YOWN_logger_folder = "//env-fs/env-data/corp/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/9_LOGGER_FILE_DROPBOX") {
  
  #### Setup and common function definitions ####
  #Read in reference sheets and logger drop folder
  master <- openxlsx::read.xlsx(master_sheet, sheet = "YOWN_MASTER")
  tracker <- openxlsx::loadWorkbook(logger_tracking)
  YOWNIDs <- master$YOWN.Code 
  
  #### Define pressure conversion function ####
  convert_pressure <- function(column, pressure_unit) {
    
    # Define conversion factors for pressure units to meters of water column
    pressure_conversion_factors <- data.frame(
      unit = c("psi", "kPa", "bar", "mbar", "mm Hg", "in Hg", "cm H2O", "in H2O"),
      to_m_H2O = c(
        0.7030700000, # psi
        0.1019716213, # kPa
        10.1971621298, # bar
        0.0101971621, # mbar
        0.0135950982, # mm Hg
        0.3453187748, # in Hg
        0.0101971621, # cm H2O
        0.0254000000  # in H2O
      )
    )
    
    # Check if the unit is "m H2O"
    if (pressure_unit == "m") {
      return(column)
    } else {
      # Find the conversion factor for the specified unit
      conversion_factor <- pressure_conversion_factors$to_m_H2O[pressure_conversion_factors$unit == pressure_unit]
      # If the unit is found, apply the conversion; if not, throw an error and stop code
      if (length(conversion_factor) == 1) {
        column <- column * conversion_factor
        return(column)
      } else {
        stop(paste("Unrecognized unit:", pressure_unit, "- No conversion applied."))
      }
    }
  }
  
  #### Define depth conversion function ####
  convert_depth <- function(column, depth_unit) {
    
    # Define conversion factors for pressure units to meters of water column
    depth_conversion_factors <- data.frame(
      unit = c("mm", "cm", "in", "ft"),
      to_m = c(1000, # mm
               100, # cm
               39.3701, # in
               3.28084 # ft
      )
    )
    
    # Check if the unit is "m H2O" already
    if (depth_unit != "m") {
      # Find the conversion factor for the specified unit
      conversion_factor <- depth_conversion_factors$to_m[depth_conversion_factors$unit == depth_unit]
      # If the unit is found, apply the conversion; if not, throw an error and stop code
      if (length(conversion_factor) == 1) {
        column <- column * conversion_factor
      } else {
        stop(paste("Unrecognized unit:", depth_unit, "- No conversion applied."))
      }
      return(column)
    } else {
      return(column)
    }
  }
  
  #### Define temperature conversion function ####
  convert_temp <- function(column, temperature_unit) {
    if (temperature_unit == "\u00B0C") {
      # No conversion needed
      return(column)
    } else if (temperature_unit == "\u00B0F") {
      # Convert Fahrenheit to Celsius
      column <- (column - 32) * 5/9
      return(column)
    } else {
      stop("Unsupported temperature unit. Please use \u00B0C or '\u00B0F'.")
    }
  }
  
  #### In Situ html processing ####
  # Define html property parsing function
  parse_properties <- function(html, section_name) {
    html %>%
      rvest::html_nodes(xpath = sprintf("//tr[@class='sectionMember'][td[@isi-group-member='%s']]", section_name)) %>%
      rvest::html_text() %>%
      stringr::str_split(" = ") %>%
      purrr::map_df(~ data.frame(Property = .x[1], Value = .x[2], stringsAsFactors = FALSE))
  }
  
  # Read in html file, convert to text
  html <- xml2::read_html(paste0(YOWN_logger_folder, "/", html_file))
  html_text <- paste(html, collapse = " ")
  
  # Extract headers without units (parameter only) for compatibility with unit check functions
  headers <- html %>% # Create character vector of headers, for use in renaming data columns later
    rvest::html_elements(css = ".dataHeader td") %>% # Locate the header row
    rvest::html_text(trim = TRUE) # Extract text and trim whitespace
  headers_trim <- sub(" .*", "", headers)
  
  # Extract units for check against conversion tables
  if ("Pressure" %in% headers_trim) {
    pressure_unit <- sub(".*Pressure \\(([^)]+)\\).*", "\\1", html_text)}
  if ("Temperature" %in% headers_trim) {
    temperature_unit <-  sub(".*Temperature \\(([^)]+)\\).*", "\\1", html_text)}
  if ("Depth" %in% headers_trim) {
    depth_unit <-  sub(".*Depth \\(([^)]+)\\).*", "\\1", html_text)
  }
  
  # Create separate data frames for each property section using property parsing function
  location_properties <- parse_properties(html, "LocationProperties")
  report_properties <- parse_properties(html, "ReportProperties")
  instrument_properties <- parse_properties(html, "InstrumentProperties")
  log_properties <- parse_properties(html, "LogProperties")
  
  # Initiate log entry
  write(c("\n", as.character(Sys.time())), file = paste0(YOWN_logger_folder, "/LOGBOOK.txt"), append = TRUE, sep = "\n")
  write(paste0("File ", html_file), file = paste0(YOWN_logger_folder, "/LOGBOOK.txt"), append = TRUE, sep = "\n")
  
  # Check location ID against master sheet, throw error if not found and move file to "FAILED" folder
  well_loc <- stringr::str_extract(log_properties$Value[log_properties$Property == "Log Name"], "(?<=YOWN).{4,6}"  ) # Gets the 5 characters after YOWN-
  well_loc <- sub("_", "", well_loc)
  well_loc <- sub("-", "", well_loc)
  well_loc <- sub(" ", "", well_loc)
  well_loc <- sub("/.", "", well_loc)
  if (grepl("[0-9]", substr(well_loc, 5, 5))) { # Check if character 5 is a number. Should be one of D or S
    well_loc <- substr(well_loc, 1, 4)
  }
  well_loc <- paste0("YOWN-", well_loc)
  if (!well_loc %in% YOWNIDs) { #If YOWN ID is not found in master sheet, move file to "FAILED" folder
    file.rename(from = paste0(YOWN_logger_folder, "/", html_file),
                to = paste0("G:/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/9_LOGGER_FILE_DROPBOX/FAILED/", html_file))
    stop("The YOWN code specified in the file name does not exist") 
  }
  
  write(paste0(well_loc, " detected in file name"), file = paste0(YOWN_logger_folder, "/LOGBOOK.txt"), append = TRUE, sep = "\n")
  
  
  data_rows <- html %>%
    rvest::html_nodes("tr.data") %>%  # Select rows with the "data" class
    rvest::html_nodes("td") %>%       # Extract table cell elements
    rvest::html_text(trim = TRUE)     # Get text content and trim whitespace
  
  # Determine the number of columns
  n_cols <- length(data_rows) / length(rvest::html_nodes(html, "tr.data"))
  
  # Reshape data into a matrix and convert to a data frame
  df <- as.data.frame(matrix(data_rows, ncol = n_cols, byrow = TRUE), stringsAsFactors = FALSE) %>%
    dplyr::mutate(
      V1 = lubridate::as_datetime(.data$V1, tz = "MST") %>% lubridate::round_date(unit = "second"),
      dplyr::across(-"V1", as.numeric)) %>%
    lubridate::with_tz("V1", tzone = "UTC")
  colnames(df) <- headers_trim
  
  df <- df[, colnames(df) != "" & !is.na(colnames(df))] # Remove blank columns
  
  attr(df$Date , "tzone") <- "UTC"
  
  # Apply conversions to data
  final_data <- df
  
  if ("Pressure" %in% colnames(final_data)) {
    final_data$Pressure <- convert_pressure(column = final_data$Pressure, pressure_unit)
    pressure_unit <- "m"}
  
  if ("Temperature" %in% colnames(final_data)) {
    final_data$Temperature <- convert_temp(final_data$Temperature, temperature_unit)
    temperature_unit <- "\u00B0C"}
  
  if ("Depth" %in% colnames(final_data)) {
    final_data$Depth <- convert_depth(final_data$Depth, depth_unit)
    depth_unit <- "m"}
  
  final_data <- final_data %>%
    dplyr::rename_with(
      ~ ifelse(. == "date", "timestamp_MST",
               ifelse(. %in% c("Pressure", "Depth"), paste0(., " (m)"),
                      ifelse(. == "Temperature", paste0(., " (\u00B0C)"), .)))
    )
  # Add pressure column
  if (!"Pressure (m)" %in% colnames(final_data)) {
    final_data$`Pressure (m)` <- final_data$`Depth (m)` * 0.999 # Account for default In Situ SG H2O of 0.999, should be 1
  }
  
  # Upload data to Aquarius
  for (i in c("Wlevel_Hgt.level_RAW", "Water Temp.TEMPERATURE")) {
    if (i == "Wlevel_Hgt.level_RAW") { # Upload level data
      temp <- data.frame(Time = final_data$Date, Value = final_data$`Pressure (m)`)
      tryCatch({
        start <- Sys.time()
        result <- YGwater::aq_upload(well_loc, i, temp)
        end <- Sys.time() - start
        write(paste0("Level append successful with ", result$appended, " points appended out of ", result$input, ". Elapsed time ", round(end[[1]], 2), " ", attr(end, "units")), file = paste0(YOWN_logger_folder, "/LOGBOOK.txt"), append = TRUE, sep = "\n")
      }, error = function(e) {
        write("Level append FAILED", file = paste0(YOWN_logger_folder, "/LOGBOOK.txt"), append = TRUE, sep = "\n")
      })
    } else if (i == "Water Temp.TEMPERATURE") {
      temp <- data.frame(Time = final_data$Date, Value = final_data$'Temperature (\u00B0C)')
      tryCatch({
        start <- Sys.time()
        result <- YGwater::aq_upload(well_loc, i, temp)
        end <- Sys.time() - start
        write(paste0("Temperature append successful with ", result$appended, " points appended out of ", result$input, ". Elapsed time ", round(end[[1]], 2), " ", attr(end, "units")), file = paste0(YOWN_logger_folder, "/LOGBOOK.txt"), append = TRUE, sep = "\n")
      }, error = function(e) {
        write("Temperature append FAILED", file = paste0(YOWN_logger_folder, "/LOGBOOK.txt"), append = TRUE, sep = "\n")
      })
    }
  }
  
  #### Track metadata ####
  # Read the existing data
  existing_data <- openxlsx::read.xlsx(logger_tracking, sheet = "Sheet 1")
  
  # Format data to be added
  new_data <- c(well_loc,
                "In Situ",
                instrument_properties$Value[instrument_properties$Property == "Device Model"],
                NA,
                instrument_properties$Value[instrument_properties$Property == "Device SN"],
                as.character(min(as.POSIXct(final_data$Date, tz = "MST"))),
                as.character(max(final_data$Date)))
  
  # Append the new data
  updated_data <- rbind(existing_data, new_data)
  updated_data <- updated_data %>%
    dplyr::distinct()
  
  # Write the updated data back to the Excel file
  tryCatch({
    openxlsx::write.xlsx(x = updated_data, file = logger_tracking, rowNames = FALSE)
    write("Logger tracking successful", file = paste0(YOWN_logger_folder, "/LOGBOOK.txt"), append = TRUE, sep = "\n")
  }, error = function(e) { 
    write("Logger tracking FAILED, make sure nobody has the logger tracking sheet open", file = paste0(YOWN_logger_folder, "/LOGBOOK.txt"), append = TRUE, sep = "\n")
  }, warning = function(w) {
    write("Logger tracking FAILED, make sure nobody has the logger tracking sheet open. Re-add HTML file to logger dropbox to complete tracking.", file = paste0(YOWN_logger_folder, "/LOGBOOK.txt"), append = TRUE, sep = "\n")}
  )
  
  #### Move file to final resting place ####
  #Move the html, making two copies
  year <- substr(max(final_data$Date), 1, 4) #Last year on record in the file
  dirs <- list.dirs("//env-fs/env-data/corp/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/1_YOWN_SITES/1_ACTIVE_WELLS", full.names = FALSE, recursive = FALSE)
  dir <- dirs[grepl(well_loc, dirs)]
  
  # Copy file to backups folder
  file.copy(from = paste0(YOWN_logger_folder, "/", html_file), 
            to = paste0(YOWN_logger_folder, "/backups/", html_file))
  
  #Make a new folder for the year if it does not yet exist
  suppressWarnings(dir.create(paste0("//env-fs/env-data/corp/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/1_YOWN_SITES/1_ACTIVE_WELLS/", dir, "/Logger files and notes/", year)))
  
  # Move the html file to its final resting place in YOWN folder
  tryCatch({file.rename(from = paste0(YOWN_logger_folder, "/", html_file), 
                        to = paste0("//env-fs/env-data/corp/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/1_YOWN_SITES/1_ACTIVE_WELLS/", dir, "/Logger files and notes/", year, "/", html_file)
  )
    write("The html file was successfully moved to its final resting places (Backup folder and appropriate YOWN folder)", file = paste0(YOWN_logger_folder, "/LOGBOOK.txt"), append = TRUE, sep = "\n")
  }, error = function(e) { 
    write("Moving the file to its final resting place FAILED", file = paste0(YOWN_logger_folder, "/LOGBOOK.txt"), append = TRUE, sep = "\n")})
  
}
