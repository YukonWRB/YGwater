#' Convert Solinst logger files to csv format
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Reads a Solinst .xle file and converts it into a .csv with proper column names. Converts units to those in common usage at the Yukon Water Resources Branch, standardizes file naming, and ensures that times are represented in UTC-7.
#'
#' Currently works with Solinst LT, LTC, and baro loggers. Note that LTs and baro loggers are functionally identical: they are differentiated solely by the pressure scale specified in the logger file header. Barologgers are labelled "M1.5" or similar (1.5 m water column), while the shallowest LT model is an "M5" or similar. Issues will arise if future baro loggers do not use the "1.5" designation, or if you are actually using a barologger to monitor very shallow water columns.
#'
#' @param xle_file The file you wish to convert. Default "choose" allows you to point to the file.
#' @param location The ID of the well in the form "YOWN-1500". You can also simply name the well, and if there is ambiguity regarding which well is the right one you will get a prompt to select from a list.
#' @param save_path The location where the csv file should be saved.
#'
#' @return A csv of the logger data, ready for export to Aquarius or for general use.
#' @export
#'
xle_convert <- function(xle_file = "choose",
                        location,
                        save_path = "choose"){



  if (xle_file == "choose"){
    message("Select the path to the logger file.")
    xle_file <- as.character(utils::choose.files(caption="Select logger file"))
  }
  if (save_path == "choose"){
    message("Select the path to the folder where you want this data saved.")
    save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
  }


  # NAME CHECK AND CORRECTION AGAINST MASTER SPREADSHEET
  # The user could have specified a YOWN-xxxx code, a location name, or both. First check for a YOWN code, then the location name.
  if (grepl("YOWN", location)){
    user_code <- substr(sub(".*YOWN-", "", location), 1, 5) #Gets the 5 characters after YOWN-
    user_code <- sub("_", "", user_code) #removes characters that can't indicate D or S
    user_code <- sub("-", "", user_code)
    user_code <- sub(" ", "", user_code)
    if (grepl("[0-9]", substr(user_code, 5, 5))) { #Check if character 5 is a number, removes it if it is. Should be one of D or S
      user_code <- substr(user_code, 1, 4)
    }
    user_code <- paste0("YOWN-", user_code)
  } else { #YOWN-xxxx is not specified
    user_code <- "no match"
  }

  user_name <- stringr::str_remove(location, user_code) #Subtract the YOWN code from the user-specified location srting
  if (nchar(user_name) == 0){ #if there is nothing other than a YOWN code, user_name is "no match"
    user_name <- "no match"
  }
  # Read in master sheet
  yown_stn_names <- dplyr::filter(openxlsx::read.xlsx("//env-fs/env-data/corp/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/2_SPREADSHEETS/1_YOWN_MASTER_TABLE/YOWN_MASTER.xlsx", sheet = 1),
                           !is.na(.data$`YOWN.Code`), !is.na(.data$`Name`))
  possible_names <- c() #create an empty vector
  for (i in 1:nrow(yown_stn_names)) { # If the YOWN-xxxx provided by user matches a row in the master xlsx OR if the station name provided matches loosely with a row, save that station's code and name in possible_names
    if (grepl(user_code, yown_stn_names$`YOWN.Code`[i]) |
        agrepl(user_name, yown_stn_names$Name[i])) {
      possible_names <- c(possible_names, paste(yown_stn_names$`YOWN.Code`[i],
                                                yown_stn_names$Name[i]))
    }
  }

  # Now correct_name is a list (with 0, 1, or more elements) which corresponds to the different entries in the master spreadsheet that resembled the location you inputted. Here, either select the position of the element in correct_name that matches with the location you desire, or take this time to input the correct location (format: YOWN-XXXX STATIONNAME).
  if (length(possible_names) > 1){
    message("Stations in the YOWN Master sheet that matched your input:")
    for (i in 1:length(possible_names)) {
      cat("Position ", i, " :", possible_names[i], "\n")
    }

    choice <- readline(prompt =
                         writeLines(paste("\nChoose the correct name by selecting",
                                          "its position in the list.",
                                          "\nIf the available options are not",
                                          "correct, type in the correct name",
                                          "(format: YOWN-XXXX STATIONNAME)"
                         )))

    # This regex expression returns true only if "choice" is a digit, false otherwise
    if (grepl("^[[:digit:]]+$", choice)) {
      location <- possible_names[as.numeric(choice)]
      message("Location has been updated to match format in master spreadsheet")
    } else {
      location <- choice
      message("New name has been inputted by user")
    }
  } else if (length(possible_names) == 0){
    stop("It was impossible to find a match for any YOWN wells using the YOWN codes or well names in the YOWN master spreadsheet. Please correct the location you specified or ammend the YOWN master sheet.")
  }

  # CONVERT XLE TO XML
  # Encoding of .xle files are either UTF-8 or Windows-1252 (Where Windows-1252 is the same as ANSI)
  tryCatch( {result <- XML::xmlParse(file = xle_file, encoding = "UTF-8") },
            error=function(e) {suppressWarnings(rm(result))})
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
  proper_LT <- data.frame("parameter" = c("LEVEL", "TEMPERATURE"),
                          "unit_proper" = c("m", "\u00B0CC"))
  proper_BL  <- data.frame("parameter" = c("LEVEL", "TEMPERATURE"),
                           "unit_proper" = c("kPa", "\u00B0CC"))

  # Storing proper parameter units and their conversions
  conversions_LTC <- data.frame("parameter" = c("CONDUCTIVITY", "LEVEL", "LEVEL", "LEVEL"),
                                "unit_proper" = c("\u00B5S/cm", "m", "m", "m"),
                                "unit_current" = c("mS/cm", "kPa", "psi", "mbar"),
                                "multiplier" = c(1000, 0.101972, 	0.70307, 0.0101971621297))
  conversions_LT  <- data.frame("parameter" = c("CONDUCTIVITY", "LEVEL", "LEVEL", "LEVEL"),
                                "unit_proper" = c("\u00B5S/cm", "m", "m", "m"),
                                "unit_current" = c("mS/cm", "kPa", "psi", "mbar"),
                                "multiplier" = c(1000, 0.101972, 	0.70307, 0.0101971621297))
  conversions_BL  <- data.frame("parameter" = c("LEVEL", "LEVEL", "LEVEL"),
                                "unit_proper" = c("kPa", "kPa", "kPa"),
                                "unit_current" = c("psi", "m", "mbar"),
                                "multiplier" = c(6.89467, (1/0.101972), 0.1))

  # Instrument_type either contains "LTC" or "LT". "LT" can represent either a level/temp water sensor, or a barologger: they are identical except for the max pressure rating.
  Instrument_type <- xml_data[["Instrument_info"]][["Instrument_type"]]
  instrument_model <- xml_data[["Instrument_info"]][["Model_number"]]

  if (grepl("LT", Instrument_type)){
    if (grepl("1.5", instrument_model)){ #This here is the distinction for a BL: 1.5 meters of water column
      Instrument_type <- "BL"
    }
  }

  # NOTE: 2019 onwards usually has levelogger files = LTC, barologger files = LT. That is the basis for the if statement in this loop, as well as the "proper_LTC/BL".
  if (grepl("LTC", Instrument_type)) {
    Instrument_type <- "LTC"
    check <- proper_LTC %>%
      # Do left join on approximate match bw parameter & parameter name from .xle. Necessary because of occasional typos in logger file
      fuzzyjoin::stringdist_left_join(check, by = c("parameter" = "header_name"),
                           max_dist = 2, ignore_case = TRUE) %>%
      dplyr::select("parameter", "section_name", "unit_proper", "unit_current") %>%
      dplyr::left_join(conversions_LTC,
                by = c("parameter", "unit_proper", "unit_current"))

  } else if (grepl("LT[^(LTC)]", Instrument_type)) {
    # Are working with LT Instrument -> barologger
    Instrument_type <- "LT"

    check <- proper_LT %>%
      # Do left join on approximate match bw parameter & parameter name from .xle
      # Necessary because of occasional typos in logger file
      fuzzyjoin::stringdist_left_join(check, by = c("parameter" = "header_name"),
                           max_dist = 2, ignore_case = TRUE) %>%
      dplyr::select("parameter", "section_name", "unit_proper", "unit_current") %>%
      dplyr::left_join(conversions_LT,
                by = c("parameter", "unit_proper", "unit_current"))

  } else if (Instrument_type == "BL"){
    check <- proper_BL %>%
      # Do left join on approximate match bw parameter & parameter name from .xle
      # Necessary because of occasional typos in logger file
      fuzzyjoin::stringdist_left_join(check, by = c("parameter" = "header_name"),
                                      max_dist = 2, ignore_case = TRUE) %>%
      dplyr::select("parameter", "section_name", "unit_proper", "unit_current") %>%
      dplyr::left_join(conversions_BL,
                       by = c("parameter", "unit_proper", "unit_current"))
    } else {
    stop("This script is not designed to handle this logger file structure: failure occurred when checking units (degrees, meters, etc) against proper units. Try using the Solinst software for conversion.")
  }

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
  if (identical("LTC", Instrument_type)) {
    df <- dplyr::select(df, "Date", "Time", "ms", "LEVEL", "TEMPERATURE", "CONDUCTIVITY")
  } else if (identical("LT", Instrument_type)){
    df <- dplyr::select(df, "Date", "Time", "ms", "LEVEL", "TEMPERATURE")
  } else if (identical("BL", Instrument_type)) {
    df <- dplyr::select(df, "Date", "Time", "ms", "LEVEL", "TEMPERATURE")
  } else {
    stop("This script is not designed to handle this logger file structure.")
  }

  # Handling daylight savings
  start_datetime <- as.POSIXct(paste(df$Date[1], df$Time[1]))
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
  if (!agrepl(location, Location, ignore.case = TRUE)) {
    choice <- readline(prompt = writeLines(paste("\n\nYour input location:                          ",
                                                 location,
                                                 "\nThe location specified in the logger file:    ",
                                                 Location,
                                                 "\n\nThe location you provided",
                                                 "does not match the location",
                                                 "listed in the xle.\n",
                                                 "Do you want to override the",
                                                 "file's location with your",
                                                 "input location?\nY/N\n")))
    if (grepl("^y", choice, ignore.case = TRUE)) {
      Location <- location
      message("Correct location was user's location")
    } else {
      message("Correct location was file's location")

      possible_names <- c()
      for (i in 1:nrow(yown_stn_names)) {

        user_code <- stringr::str_extract(Location, "YOWN-....")
        user_name <- stringr::str_remove(Location, "YOWN-....")

        # If the YOWN-xxxx provided by user matches a row in the master xlsx OR if the station name provided matches loosely with a row, save that station's code and name in possible_names
        if (grepl(user_code, yown_stn_names$`YOWN Code`[i]) |
            agrepl(user_name, yown_stn_names$Name[i])) {
          possible_names <- c(possible_names, paste(yown_stn_names$`YOWN Code`[i],
                                                    yown_stn_names$Name[i]))
        }
      }

      message("Stations that matched the file:")
      for (i in 1:length(possible_names)) {
        cat("Position ", i, " :", possible_names[i], "\n")
      }
      choice <- readline(prompt = writeLines(paste("Choose the correct name by selecting its position in the list (possible_names)\nIf the available options are not correct, type in the correct name (format: YOWN-XXXX STATIONNAME)")))
      # This regex expression return true only if "choice" is a digit, false otherwise
      if (grepl("^[[:digit:]]+$", choice)) { # Are choosing position in list
        location <- possible_names[as.numeric(choice)]
        message("Location has been updated to match format in master spreadsheet")
      } else {
        location <- choice
        message("New name has been input by user")
      }
    }
  }

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
  } else if (identical("LT", Instrument_type)) {
    params_units <- paste(c("LEVEL", paste("UNIT: ", dplyr::select(dplyr::filter(check, .data$parameter == "LEVEL"), "unit_proper")), paste("Offset: ", Level_offset), "TEMPERATURE", paste("UNIT: ", dplyr::select(dplyr::filter(check, .data$parameter == "TEMPERATURE"), "unit_proper")), "CONDUCTIVITY", "NOT REPORTED"))
  } else if (identical("BL", Instrument_type)){
    params_units <- paste(c("LEVEL", paste("UNIT: ", dplyr::select(dplyr::filter(check, .data$parameter == "LEVEL"), "unit_proper")), paste("Offset: ", Level_offset), "TEMPERATURE", paste("UNIT: ", dplyr::select(dplyr::filter(check, .data$parameter == "TEMPERATURE"), "unit_proper")), "CONDUCTIVITY", "NOT REPORTED"))
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
