#' Convert Solinst logger files to csv format
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Reads a Solinst .xle file and converts it into a .csv with proper column names. Converts units to those in common usage at the Yukon Water Resources Branch, standardizes file naming, and ensures that times are represented in UTC-7.
#'
#' Currently works with Solinst LTCs.
#'
#' @param xle_file The file you wish to convert. Default "choose" allows you to point to the file. WARNING: option 'choose' only works on Windows, and some late-build R versions have a bug that prevents it from working every time.
#' @param location The ID of the well in the form "YOWN-1500". You can also simply name the well, and if there is ambiguity regarding which well is the right one you will get a prompt to select from a list.
#' @param save_path The location where the csv file should be saved. WARNING: option 'choose' only works on Windows, and some late-build R versions have a bug that prevents it from working every time.
#'
#' @return A csv of the logger data, ready for export to Aquarius or for general use.
#' @export
#'
xle_convert <- function(xle_file,
                        location,
                        save_path){
  
  xle_file = "G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\1_YOWN_SITES\\1_ACTIVE_WELLS\\YOWN-2001 Upper Liard SWDF Well 2\\Logger files and notes\\2024\\1083558_YOWN-2001_UpperLiard2_SWDF_LTC_2024_04_15_182828.xle"
  location = "YOWN-2001"
  save_path = "G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\1_YOWN_SITES\\1_ACTIVE_WELLS\\YOWN-2001 Upper Liard SWDF Well 2\\Logger files and notes\\2024"
  
  # CONVERT XLE TO XML
  # Encoding of .xle files are either UTF-8 or Windows-1252 (Where Windows-1252 is the same as ANSI)
  tryCatch( {result <- XML::xmlParse(file = xle_file, encoding = "UTF-8") },
            error = function(e) {suppressWarnings(rm(result))})
  if (!exists("result")) {# If UTF-8 encoding works, running the ANSI encoding will give bad output. Only try ANSI encoding if UTF-8 cannot be applied. Note that ANSI encoding still yields ÂµS/cm rather than µS/cm
    tryCatch( {result <- XML::xmlParse(file = xle_file, encoding = "Windows-1252") },
              error = function(e) {stop("Encoding not Windows-1252/ANSI or UTF-8 and this script will not work.")})
  }
  
  xml_data <- XML::xmlToList(result)
  
  # Set up proper header names and units. Perform unit conversions where needed
  # Get each measurement's header name, parameter name and unit. Store in "check"
  check <- data.frame(header_name = character(), section_name = character(), unit_current = numeric(), stringsAsFactors = FALSE)
  j <- 1 # Counter
  for (i in 1:length(xml_data)) { # Populate "check"
    name <- names(xml_data)[i]
    if (stringr::str_detect(name, "Ch._data_header")) {
      check[j,] <- list(xml_data[[name]][["Identification"]],
                        name,
                        xml_data[[name]][["Unit"]])
      j <- j + 1
    }
  }
  
  # Here "check" looks like (approximately)
  # header_name  | section_name    | unit_current
  # LEVEL        | Ch1_data_header | m
  # TEMPERATURE  | Ch2_data_header | \u00B0CC
  # CONDUCTIVITY | Ch3_data_header | mS/cm
  
  # Storing proper parameter names and units
  proper_LTC <- data.frame("parameter" = c("LEVEL", "TEMPERATURE","CONDUCTIVITY"),
                           "unit_proper" = c("m", "\u00B0CC", "\u00B5S/cm"))
  
  # Storing proper parameter units and their conversions
  conversions_LTC <- data.frame("parameter" = c("CONDUCTIVITY", "LEVEL", "LEVEL", "LEVEL"),
                                "unit_proper" = c("\u00B5S/cm", "m", "m", "m"),
                                "unit_current" = c("mS/cm", "kPa", "psi", "mbar"),
                                "multiplier" = c(1000, 0.101972, 	0.70307, 0.0101971621297))
  
  # Extract instuemnt type
  Instrument_type <- xml_data[["Instrument_info"]][["Instrument_type"]]
  instrument_model <- xml_data[["Instrument_info"]][["Model_number"]]
  
  check <- proper_LTC %>%
    # Do left join on approximate match bw parameter & parameter name from .xle. Necessary because of occasional typos in logger file
    fuzzyjoin::stringdist_left_join(check, by = c("parameter" = "header_name"),
                                    max_dist = 2, ignore_case = TRUE) %>%
    dplyr::select("parameter", "section_name", "unit_proper", "unit_current") %>%
    dplyr::left_join(conversions_LTC,
                     by = c("parameter", "unit_proper", "unit_current"))
  
  # Store the logged data
  df <- XML::xmlToDataFrame(nodes = XML::xmlChildren(XML::xmlRoot(result)[["Data"]]))
  df$Date <- as.Date(df$Date)
  
  # Loop through columns of df and give columns containing level, temp, or conductivity data the correct column name AND perform unit conversion if needed
  for (i in 1:length(df)) {
    curr_name <- names(df)[i]
    if (stringr::str_detect(curr_name, "ch.")) { # If column name has ch we are looking at LTC/LT/BL vals
      check_row <- dplyr::filter(check, grepl(curr_name, section_name, ignore.case = TRUE))# Row containing info we need, including parameter name, multiplier, proper unit
      colnames(df)[i] <- c(check_row$parameter) # Give column in df proper name
      # Unit conversion if needed
      if (!is.na(check_row$multiplier)) {
        df[[i]] <- as.numeric(unlist(df[[i]]))
        # If we're converting from m to kPa, we are using an old logger and the conversion is not as simple as multiplication:
        if (agrepl("kPa", check_row$unit_proper) &
            agrepl("m", check_row$unit_current)) {
          # If Baro pressure was calculated in meters, first add 9.5 (automatic offset), then convert to kPa then subtract the difference in pressure at that elevation from sea level
          altitude <- as.numeric(
            xml_data[["Ch1_data_header"]][["Parameters"]][["Altitude"]][1])
          df[[i]] <- (9.5 + df[[i]])*(check_row$multiplier) - (101.325 - (101.325*(1-2.25577*10^(-5)*altitude)^5.25588))
        } else { # Any other unit conversion is just a simple multiplication
          df[[i]] <- df[[i]]*check_row$multiplier
        }
      }
    }
    
    
  }
  
  # Just in case. Makes sure columns are in the order shown below
  if (grepl("LTC", Instrument_type)) {
    df <- dplyr::select(df, "Date", "Time", "ms", "LEVEL", "TEMPERATURE", "CONDUCTIVITY")
  } else {
    stop("This script is not designed to handle this logger file structure.")
  }
  
# Extract and write info to logger tracking sheet
  logger_tracking <- openxlsx::loadWorkbook("G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\2_SPREADSHEETS\\3_OTHER\\YOWN_Logger_Tracking.xlsx")
  log_sheet_pre <- openxlsx::readWorkbook(logger_tracking, sheet = "Logger_Tracking")
  write_index <- nrow(log_sheet_pre)
  start_datetime <- as.POSIXct(paste(df$Date[1], df$Time[1]))
  end_datetime <- as.POSIXct(paste(df$Date[length(df$Date)], df$Time[length(df$Time)]))
  instrument_make <- "Solinst"
  tracking_data <- c(location, 
                     instrument_make, 
                     xml_data[["Instrument_info"]][["Instrument_type"]], 
                     xml_data[["Instrument_info"]][["Model_number"]], 
                     xml_data[["Instrument_info"]][["Serial_number"]],
                     as.character(start_datetime),
                     as.character(end_datetime))
  log_sheet_post <- rbind(log_sheet_pre, tracking_data)
  openxlsx::writeData(logger_tracking, sheet = "Logger_Tracking", x = log_sheet_post, startRow = 2, colNames = FALSE)
  openxlsx::saveWorkbook(logger_tracking, file = "G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\2_SPREADSHEETS\\3_OTHER\\YOWN_Logger_Tracking.xlsx", overwrite = TRUE)
  
  # Handling daylight savings
  if (start_datetime < "2020-03-08 02:00:00") {
    # Are in time period where daylight savings was used. With exception of 2020, dst returns true for UTC-07, false for UTC-08
    if (!lubridate::dst(start_datetime)) { # Started on UTC-08. Bump up each time stamp by 1h
      df$Time <- format(lubridate::as_datetime(paste(df$Date, df$Time)) + lubridate::hours(1), "%H:%M:%S")
      df$Date <- format(lubridate::as_datetime(paste(df$Date, df$Time)) + lubridate::hours(1), "%Y-%m-%d")
    }
  }
  
  # Setting up file w header and data & exporting to .csv
  Serial_number <- xml_data[["Instrument_info"]][["Serial_number"]]
  if (is.null(xml_data[["Instrument_info_data_header"]][["Project_ID"]])){
    Project_ID <- "No project ID specified"
  } else {
    Project_ID <- xml_data[["Instrument_info_data_header"]][["Project_ID"]]
  }
  if (is.null(xml_data[["Instrument_info_data_header"]][["Location"]])==TRUE) {
    Location <- "No Location in logger file"
  } else {
    Location <- xml_data[["Instrument_info_data_header"]][["Location"]]
  }
  
  Field_visit_date  <- xml_data[["File_info"]][["Date"]]
  
  # If user's supplied location does not match loosely to the field visit in the file, user is given a chance to choose if their location is accurate, or if the location listed in the file is correct.
  # To cover instances of a) missing location names in .xle files and b) incorrect location names (because of a logger moved between locations, for example)
  
  # csv name will be startdate_to_enddate_location_loggertype.csv
  startdate <- gsub("-", "", df$Date[1])
  enddate <- gsub("-", "", df$Date[nrow(df)])
  filename <- paste0(location, "_", startdate, "_to_", enddate, "_", Instrument_type, ".csv")
  f <- file(paste0(save_path, "/", filename), "w")
  
  Level_offset <- as.numeric(xml_data[["Ch1_data_header"]][["Parameters"]][["Offset"]][1])
  
  params_units <- NA
  # params_units will be a string for the header rows (info) for the csv will look something like:
  # LEVEL
  # UNIT: kPa
  # TEMPERATURE
  # UNIT: \u00B0CC
  # etc
  if (identical("LTC", Instrument_type)) {  # We use "check" to determine the units
    params_units <- paste(c("LEVEL", paste("UNIT: ", dplyr::select(dplyr::filter(check, .data$parameter == "LEVEL"), "unit_proper")), paste("Offset: ", Level_offset), "TEMPERATURE", paste("UNIT: ", dplyr::select(dplyr::filter(check, .data$parameter == "TEMPERATURE"), "unit_proper")), "CONDUCTIVITY", paste("UNIT: ", dplyr::select(dplyr::filter(check, .data$parameter == "CONDUCTIVITY"), "unit_proper"))))
  } else {
    stop("This script is not designed to handle this logger file structure.")
  }
  
  # Write the following string to the csv as the header lines
  writeLines(paste(c("Serial_number:", Serial_number,
                     "Project ID:", Project_ID,
                     "Location:", location,
                     "Timezone:", "UTC-07:00",
                     params_units)),
             f)
  
  # Write the actual data into the csv and create the csv
  utils::write.csv(df, f, row.names = FALSE)
  
  close(f)
  
  writeLines(paste0("\n\t\n\tThank you for using this script! Your file is now in ", save_path, "."))
}
