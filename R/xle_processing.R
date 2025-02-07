#' Solinst xle logger file processing
#' 
#' @description 
#' Parse xle (Solinst) logger files, upload to Aquarius, sort into respective folder, and track logger metadata. Created to work within the Yukon Water Resources Branch's folder structure.
#'
#' @param file Full path to the .xle file. This must be an XLE file from a Solinst logger.
#' @param aq_upload Logical, whether to upload data to Aquarius. If FALSE a data.frame of the data is returned.
#' @param master_sheet Path to YOWN master sheet (.xlsx), used to match logger data to YOWN ID and ensure integrity.
#' @param logger_tracking Path to YOWN logger tracking sheet (.xlsx), used to track logger metadata. This part is done within a tryCatch block to enable this function to run even if the logger tracking sheet is open, missing, or otherwise inaccessible.
#' @param dropbox Path to YOWN logger dropbox folder, where the xle file is located. This is used to move the file to the appropriate folder after processing.
#' @param repo Path to YOWN Active Wells folder, which will be concatenated with the YOWN ID to create the final resting place for the xle file. If NULL the file will not get moved.
#'
#' @return Moves YOWN xle file to backups folder and appropriate YOWN Active Wells folder. Uploads data to Aquarius after performing unit checks and conversions if aq_upload is TRUE, else returns a data.frame.
#' @export

xle_processing <- function(file,
                           aq_upload = TRUE,
                           master_sheet = "//env-fs/env-data/corp/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/2_SPREADSHEETS/1_YOWN_MASTER_TABLE/YOWN_MASTER.xlsx",
                           logger_tracking = "//env-fs/env-data/corp/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/2_SPREADSHEETS/3_OTHER/YOWN_Logger_Tracking.xlsx",
                           dropbox = "//env-fs/env-data/corp/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/9_LOGGER_FILE_DROPBOX",
                           repo = "//env-fs/env-data/corp/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/1_YOWN_SITES/1_ACTIVE_WELLS") {
  file = "G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\9_LOGGER_FILE_DROPBOX\\FAILED\\1083563_YOWN-2308 Haines Junction Villag_2024_09_17_115603.xle"
  aq_upload = TRUE
  master_sheet = "//env-fs/env-data/corp/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/2_SPREADSHEETS/1_YOWN_MASTER_TABLE/YOWN_MASTER.xlsx"
  logger_tracking = "//env-fs/env-data/corp/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/2_SPREADSHEETS/3_OTHER/YOWN_Logger_Tracking.xlsx"
  dropbox = "//env-fs/env-data/corp/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/9_LOGGER_FILE_DROPBOX"
  repo = "//env-fs/env-data/corp/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/1_YOWN_SITES/1_ACTIVE_WELLS"
  
  # Make sure tibble is installed as it's in suggests
  rlang::check_installed("tibble", reason = "necessary for function to run")
  
  # Ensure the 'file' has extension .xle (last part of the string)
  if (tools::file_ext(file) != "xle") {
    stop("The file must have the extension .xle")
  }
  
  #Read in reference sheets and logger drop folder
  master <- openxlsx::read.xlsx(master_sheet, sheet = "YOWN_MASTER")
  YOWNIDs <- master$YOWN.Code 
  
  #### Define helper functions ####
  # Define pressure conversion function
  convert_level <- function(column, level_unit) {
    
    # Define conversion factors for pressure units to meters of water column
    level_conversion_factors <- data.frame(
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
    if (level_unit == "m") {
      return(column)
    } else {
      # Find the conversion factor for the specified unit
      conversion_factor <- level_conversion_factors$to_m_H2O[level_conversion_factors$unit == level_unit]
      # If the unit is found, apply the conversion; if not, throw an error and stop code
      if (length(conversion_factor) == 1) {
        column <- column * conversion_factor
        return(column)
      } else {
        stop(paste("Unrecognized unit:", level_unit, "- No conversion applied."))
      }
    }
  }
  
  # Define conductivity conversion function
  convert_conductivity <- function(column, conductivity_unit) {
    
    # Define conversion factors for pressure units to meters of water column
    conductivity_conversion_factors <- data.frame(
      unit = c("ms/cm"),
      'to_\u00B5S/cm' = c(0.1) # ms/cm
    )
    
    # Check if the unit is "uS/cm" already
    if (conductivity_unit == "\u00B5S/cm") {
      return(column)
    } else if (conductivity_unit == "mS/cm") {
      column <- column * 0.01
      return(column)
    } else {
      stop(paste("Unrecognized unit:", conductivity_unit, "- No conversion applied."))
    }
  }
  
  # Define temperature conversion function 
  convert_temp <- function(column, temperature_unit) {
    if (temperature_unit == "\u00B0C" || temperature_unit == "Deg C") {
      # No conversion needed
      return(column)
    } else if (temperature_unit == "\u00B0F" || temperature_unit == "Deg F") {
      # Convert Fahrenheit to Celsius
      column <- (column - 32) * 5/9
      return(column)
    } else {
      stop("Unsupported temperature unit. Please use \u00B0C or \u00B0F.")
    }
  }
  
  # Define header data extraction function
  parse_properties <- function(xml_nodes, node_name) {
    tibble::tibble(
      Property = xml_nodes %>%
        xml2::xml_find_all(paste0("//", node_name, "/*")) %>%
        xml2::xml_name(),
      Value = xml_nodes %>%
        xml2::xml_find_all(paste0("//", node_name, "/*")) %>%
        xml2::xml_text()
    )
  }
  
  #### xle processing ####
  xml_file <- xml2::read_xml(file)
  
  # Extract File Info
  log_properties <- parse_properties(xml_file, "File_info")
  
  # Extract Instrument Info
  instrument_properties <- parse_properties(xml_file, "Instrument_info")
  
  # Extract Instrument Info Data Header
  instrument_data_header <- parse_properties(xml_file, "Instrument_info_data_header")
  
  # Extract Channel Data Headers (Ch1, Ch2, Ch3)
  channel_data_header <- xml_file %>%
    xml2::xml_find_all("//Ch1_data_header|//Ch2_data_header|//Ch3_data_header") %>%
    purrr::map_dfr(~{
      tibble::tibble(
        Channel = xml2::xml_name(.x),
        Identification = xml2::xml_find_first(.x, "Identification") %>% xml2::xml_text(),
        Unit = xml2::xml_find_first(.x, "Unit") %>% xml2::xml_text()
      )
    })
  
  LTC <- FALSE # Tracks if there are three channels in the XML file or two
  
  if (nrow(channel_data_header) == 3) {
    LTC <- TRUE
  }
  
  # Extract units for check and correction
  level_unit <- channel_data_header$Unit[1]
  temperature_unit <- channel_data_header$Unit[2]
  
  if (LTC) {
    conductivity_unit <- channel_data_header$Unit[3]
  }
  
  # Initiate log entry
  write(c("\n", as.character(Sys.time())), file = paste0(dropbox, "/LOGBOOK.txt"), append = TRUE, sep = "\n")
  write(paste0("File ", file), file = paste0(dropbox, "/LOGBOOK.txt"), append = TRUE, sep = "\n")
  
  # Check location ID against master sheet, throw error if not found and move file to "FAILED" folder
  well_loc <- stringr::str_extract(instrument_data_header$Value[instrument_data_header$Property == "Location"], "(?<=YOWN).{4,6}"  ) # Gets the 5 characters after YOWN-
  well_loc <- sub("_", "", well_loc)
  well_loc <- sub("-", "", well_loc)
  well_loc <- sub(" ", "", well_loc)
  well_loc <- sub("/.", "", well_loc)
  if (grepl("[0-9]", substr(well_loc, 5, 5))) { # Check if character 5 is a number. Should be one of D or S
    well_loc <- substr(well_loc, 1, 4)
  }
  well_loc <- paste0("YOWN-", well_loc)
  if (!well_loc %in% YOWNIDs) { #If YOWN ID is not found in master sheet, move file to "FAILED" folder and stop
    file.rename(from = paste0(dropbox, "/", file),
                to = paste0(dropbox, "/FAILED/", file))
    stop(write(paste0("Please check the YOWN code for errors"), file = paste0(dropbox, "/LOGBOOK.txt"), append = TRUE, sep = "\n")) 
  }
  
  write(paste0(well_loc, " detected in file name"), file = paste0(dropbox, "/LOGBOOK.txt"), append = TRUE, sep = "\n")
  
  # Extract Logs into a Data Frame
  data <- xml_file %>%
    xml2::xml_find_all("//Data/Log") %>%
    purrr::map_df(~{
      xml2::xml_children(.x) %>% 
        purrr::set_names(xml2::xml_name(.)) %>% 
        purrr::map(xml2::xml_text) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(id = xml2::xml_attr(.x, "id"))
    })
  
  # Convert columns to appropriate types
  if (LTC) {
    final_data <- data %>%
      dplyr::mutate(
        Time = as.POSIXct(paste0(.data$Date, " ", .data$Time), tz = "MST"),
        ch1 = as.numeric(.data$ch1),
        ch2 = as.numeric(.data$ch2),
        ch3 = as.numeric(.data$ch3)
      ) %>%
      lubridate::with_tz("Time", tzone = "UTC") %>%
      dplyr::select(-c("id", "ms", "Date")) %>%
      stats::setNames(c("Time", 
                 channel_data_header$Identification[1], 
                 channel_data_header$Identification[2], 
                 channel_data_header$Identification[3]))
  } else {
    final_data <- data %>%
      dplyr::mutate(
        Time = as.POSIXct(paste0(.data$Date, " ", .data$Time), tz = "MST"),
        ch1 = as.numeric(.data$ch1),
        ch2 = as.numeric(.data$ch2)
      ) %>%
      lubridate::with_tz("Time", tzone = "UTC") %>%
      dplyr::select(-c("id", "ms", "Date")) %>%
      stats::setNames(c("Time", 
                 channel_data_header$Identification[1], 
                 channel_data_header$Identification[2]))
  }

  if ("LEVEL" %in% colnames(final_data)) {
    final_data$LEVEL <- convert_level(column = final_data$LEVEL, level_unit)
    level_unit <- "m"}
  
  if ("TEMPERATURE" %in% colnames(final_data)) {
    final_data$TEMPERATURE <- convert_temp(final_data$TEMPERATURE, temperature_unit)
    temperature_unit <- "\u00B0C"}
  
  if ("CONDUCTIVITY" %in% colnames(final_data)) {
    final_data$CONDUCTIVITY <- convert_conductivity(final_data$CONDUCTIVITY, conductivity_unit)
    conductivity_unit <- "\u00B5S/cm"}
  
  final_data <- final_data %>%
    dplyr::rename(
      "Level (m)" = "LEVEL",
      "Temperature (\u00B0C)" = "TEMPERATURE",
      "Conductivity (\u00B5S/cm)" = "CONDUCTIVITY"
    )
  
  
  # Upload data to Aquarius
  if (aq_upload) {
    for (i in c("Wlevel_Hgt.level_RAW", "Water Temp.TEMPERATURE", "Conductivity Field.Econdy-F")) {
      if (i == "Wlevel_Hgt.level_RAW") { # Upload level data
        temp <- data.frame(Time = final_data$Time, Value = final_data$`Level (m)`)
        tryCatch({
          start <- Sys.time()
          result <- aq_upload(well_loc, i, temp)
          end <- Sys.time() - start
          write(paste0("Level append successful with ", result$appended, " points appended out of ", result$input, ". Elapsed time ", round(end[[1]], 2), " ", attr(end, "units")), file = paste0(dropbox, "/LOGBOOK.txt"), append = TRUE, sep = "\n")
        }, error = function(e) {
          write("Level append FAILED", file = paste0(dropbox, "/LOGBOOK.txt"), append = TRUE, sep = "\n")
        })
      } else if (i == "Water Temp.TEMPERATURE") {
        temp <- data.frame(Time = final_data$Time, Value = final_data$'Temperature (\u00B0C)')
        tryCatch({
          start <- Sys.time()
          result <- aq_upload(well_loc, i, temp)
          end <- Sys.time() - start
          write(paste0("Temperature append successful with ", result$appended, " points appended out of ", result$input, ". Elapsed time ", round(end[[1]], 2), " ", attr(end, "units")), file = paste0(dropbox, "/LOGBOOK.txt"), append = TRUE, sep = "\n")
        }, error = function(e) {
          write("Temperature append FAILED", file = paste0(dropbox, "/LOGBOOK.txt"), append = TRUE, sep = "\n")})
      } else if (i == "Conductivity Field.Econdy-F") {
        tryCatch({
          temp <- data.frame(Time = final_data$Time, Value = final_data$'Conductivity (\u00B5S/cm)')
          start <- Sys.time()
          result <- aq_upload(well_loc, i, temp)
          end <- Sys.time() - start
          write(paste0("Conductivity append successful with ", result$appended, " points appended out of ", result$input, ". Elapsed time ", round(end[[1]], 2), " ", attr(end, "units")), file = paste0(dropbox, "/LOGBOOK.txt"), append = TRUE, sep = "\n")
        }, error = function(e) {
          write("Conductivity append FAILED", file = paste0(dropbox, "/LOGBOOK.txt"), append = TRUE, sep = "\n")})
      }
    }
  }

  
  #### Track metadata ####
  existing_data <- openxlsx::read.xlsx(logger_tracking, sheet = "Sheet 1")
  
  # Format data to be added
  new_data <- data.frame(YOWN_ID = well_loc,
                Logger_Make = "Solinst",
                Logger_Type = instrument_properties$Value[instrument_properties$Property == "Instrument_type"],
                Logger_Model = instrument_properties$Value[instrument_properties$Property == "Model_number"],
                Logger_Serial = instrument_properties$Value[instrument_properties$Property == "Serial_number"],
                Deploy_Date = as.character(min(final_data$Time)),
                Retrieve_Date = as.character(max(final_data$Time)))
  
  # Append the new data
  updated_data <- rbind(existing_data, new_data) %>%
    dplyr::distinct()
  
  # Write the updated data back to the Excel file and make an entry in the logger tracker
  tryCatch({
    openxlsx::write.xlsx(x = updated_data, file = logger_tracking, rowNames = FALSE)
    write("Logger tracking successful", file = paste0(dropbox, "/LOGBOOK.txt"), append = TRUE, sep = "\n")
  }, error = function(e) { 
    write("Logger tracking FAILED, make sure nobody has the logger tracking sheet open", file = paste0(dropbox, "/LOGBOOK.txt"), append = TRUE, sep = "\n")
  }, warning = function(w) {
    write("Logger tracking FAILED, make sure nobody has the logger tracking sheet open. Re-add XLE file to logger dropbox to complete tracking.", file = paste0(dropbox, "/LOGBOOK.txt"), append = TRUE, sep = "\n")}
  )
  
  #### Move file to final resting place ####
  
  if (!is.null(repo)) {
    # Move the xle, making two copies
    year <- substr(max(final_data$Time), 1, 4) #Last year on record in the file
    dirs <- list.dirs(repo, full.names = FALSE, recursive = FALSE)
    dir <- dirs[grepl(well_loc, dirs)] # Name of the well, so that files can go in the right place
    
    if (length(dir) == 0) {
      dir <- well_loc
    }
    
    # Check if the backups folder exists, if not create it
    if (!dir.exists(paste0(dropbox, "/backups"))) {
      dir.create(paste0(dropbox, "/backups"))
    }
    
    # Copy file to backups folder
    file.copy(from = file,
              to = paste0(dropbox, "/backups/", basename(file)))
    
    # Move the XLE file to its final resting place in the correct YOWN folder
    # Make a new folder for the year if it does not yet exist
    if (!dir.exists(paste0(repo, "/", dir, "/Logger files and notes/", year))) {
      dir.create(paste0(repo, "/", dir, "/Logger files and notes/", year), recursive = TRUE)
    }
    
    tryCatch({
      file.rename(from = file, 
                  to = paste0(repo, "/", dir, "/Logger files and notes/", year, "/", basename(file))
      )
      write("The xle file was successfully moved to its final resting places", file = paste0(dropbox, "/LOGBOOK.txt"), append = TRUE, sep = "\n")
    }, error = function(e) { 
      write("Moving the file to its final resting place FAILED", file = paste0(dropbox, "/LOGBOOK.txt"), append = TRUE, sep = "\n")
    })
  }

  
  if (!aq_upload) {
    return(final_data)
  }
}

