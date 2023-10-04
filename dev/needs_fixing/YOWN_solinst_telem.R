#' YOWN Solinst LevelSender Telemetry Processing
#'
#' @description
#' This script searches for new messages from the YOWN telemetry gmail inbox by specified sites with active telemetry, then downloads messages and combines each day's report into an XLE file which is then places in the XLE-dropbox folder for processing as usual. Trashed email upon successful creation of a .xle file.
#'
#' @param active_telem Character vector of YOWN sites for processing of SOLINST telemetry data.
#' @param path The path to the folder in which to save the .xle file.
#' @param auth_json The path to the .json file used for connection to gmail.
#'
#' @return Creates an .xle file in the folder specified by parameter `path`.
#' @export
#'
YOWN_solinst_telem <- function(active_telem = c("YOWN-2201S"), path = "G:/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/9_XLE_file_dropbox/", auth_json = "G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\4_YOWN_DATA_ANALYSIS\\1_WATER LEVEL\\00_AUTOMATED_REPORTING\\04_TELEMETRY\\gmail_interface.json"){


  #Initial setup
  rlang::check_installed("gmailr", reason = "Package gmailr is required to use function YOWN_solinst_telem") #This is here because gmailr is not a 'depends' of this package; it is only necessary for this function and this function isn't use by many users.

  #### Gmail configuration ####
  gmailr::gm_auth_configure(path = auth_json)
  gmailr::gm_auth(email = T, cache = ".secret")
  for(z in active_telem){
    query <- paste0("from:", z, " ", "subject:(LS Report)") # Create gmail search query
    file <- gmailr::gm_messages(search = query,
                                num_results = 1000000000) # Generate a list of all emails for active telemetry site, if no results length will be 1

    #### Message and report number identification and sorting ####
    if(length(file[[1]]) > 1){
      idlist <- list() # Create blank list for storing IDs of all messages that match the active telem
      for(i in 1:length(file)){ # Extract message IDs from each item in "file"
        for(j in 1:length(file[[i]][["messages"]])){
          x <- file[[i]][["messages"]][[j]][[1]]
          names(x) <- x
          idlist[[x]] <- x
        }
      }

      messages <- do.call(rbind, idlist) # rbind all message IDs into a single vector

      # Create df of message ID, report number, and report sub-number
      rptlist <- list() # Create blank list
      for(i in messages){ # Populate list with message subjects
        raw <- unlist(gmailr::gm_subject(gmailr::gm_message(i)))
        rptlist[[i]] <- raw
      }
      report <- stringr::word(do.call(rbind, rptlist), start = 4, end = 4) # extract all report numbers (ie. 64-1) and combine into vector
      id_rpt <- stats::setNames(data.frame(messages, report), c("ID", "Report")) # Combine message ID and report number into a df
      id_rpt <- tidyr::separate(stats::setNames(data.frame(messages, report), c("ID", "Report")), "Report", into = c("Report", "Number"), sep = "-") # Separate report number from sub-report number
      row.names(id_rpt) <- NULL

      #### Download and merge all emails for each report by subreport number, extract LevelSender and LevelLogger headers, extract data, format all into data frames
      for(i in unique(id_rpt$Report)){
        body_list <- list() # Create blank list for storing all messager bodies of each report
        for(j in min(id_rpt$Number):max(id_rpt$Number)){
          # Create new query specific to each report number, download message, and extract message body of all emails of every report
          query2 <- paste0("from:", active_telem, " ", "subject:(", i, "-", j, ")") # Create query for inbox search
          msg <- gmailr::gm_messages(search = query2, num_results = 1) # Run query to find matching email
          msgid <- msg[[1]][["messages"]][[1]][["id"]] # Extract message ID
          if(!is.null(msgid)){
            body <- unlist(gmailr::gm_body(gmailr::gm_message(msgid))) # Extract body of message
            body_list[[j]] <- body # Add each message to list
            gmailr::gm_trash_message(msgid)
          }
        }
        combine <- paste0(body_list, collapse = "") # Paste all messages by report together into one saucy character vector

        # Extract LevelSender header, format
        LS_rawheader <- unlist(strsplit(sub("Logger 1.*", "", combine), "\r\n"))
        LS_header <- suppressWarnings(data.frame(LS_rawheader[LS_rawheader != ""])%>%
                                        tidyr::separate(1, into = c("Param", "Value"), sep = ":", extra = "merge"))
        LS_header <- LS_header[2:nrow(LS_header),]
        LS_header$Value <- trimws(LS_header$Value)

        # Extract LevelLogger header
        LL_rawheader <- unlist(strsplit(stringr::str_match(combine, "Logger 1\r\nLocation: [^\\r\\n]*\\r\nType: [^\\r\\n]*\\r\nSerial: \\d+\\r\nBattery: \\d+%\\r\nTotal Logs: \\d+ of \\d+\\r\nLog Rate: \\d+ seconds\\r\nMemory Mode: [^\\r\\n]*\\r\nLog Type: [^\\r\\n]*\\r\nState: [^\\r\\n]*\\r\nStart Logger: \\d{2}/\\d{2}/\\d{4} \\d{2}:\\d{2}:\\d{2}"), "\r\n"))
        LL_header <- suppressWarnings(data.frame(LL_rawheader[LL_rawheader != ""]) %>%
                                        tidyr::separate(1, into = c("Param", "Value"), sep = ":", extra = "merge"))
        LL_header <- LL_header[2:nrow(LL_header),]
        LL_header$Value <- trimws(LL_header$Value)
        LL_header$Value <- gsub("%", "", LL_header$Value)

        # Extract channels header
        Ch1_data_header <- c("LEVEL", trimws(stringr::str_match(combine, "(?<=Level\\().*?(?=\\)\\,\\sConductivity)")), "")
        Ch2_data_header <- c("TEMPERATURE", trimws(stringr::str_match(combine, "(?<=Temperature\\().*?(?=\\)\\,\\sLevel)")), "")
        if(Ch2_data_header[2] == "C"){
          Ch2_data_header[2] <- "°C"}
        Ch3_data_header <- c("CONDUCTIVITY", trimws(stringr::str_extract(combine, "(?<=Conductivity\\().*?(?=\\))")), "")
        if(Ch3_data_header[2] == "uS/cm"){
          Ch3_data_header[2] <- "µS/cm"}
        Ch_dataheader <- data.frame(rbind(Ch1_data_header, Ch2_data_header, Ch3_data_header))
        rownames(Ch_dataheader) <- NULL
        colnames(Ch_dataheader) <- c("Identification", "Unit", "Parameters")

        # Extract data
        data <- unlist(strsplit(sub(".*Time, Temperature\\( C\\), Level\\(m\\), Conductivity\\(uS/cm\\)\r\n", "", combine), "\r\n"))
        data <- stringr::str_remove(data, ".*MESSAGES.*")
        data <- data.frame(data[data != ""])
        colnames(data) <- "X"
        data <- data.frame(stringr::str_split_fixed(data$X, ", ", n = 4))
        colnames(data) <- c("Timestamp_MST", "TEMPERATURE", "LEVEL", "CONDUCTIVITY")
        data <- data %>%
          dplyr::mutate_at(c("TEMPERATURE", "LEVEL", "CONDUCTIVITY"), as.numeric)
        data <- data[, c("Timestamp_MST", "LEVEL", "TEMPERATURE", "CONDUCTIVITY")]

        #### XLE document generation ####
        xml <- XML::newXMLDoc()
        Body_xle <- XML::newXMLNode("Body_xle", doc = xml)

        File_info <- XML::newXMLNode("File_info")
        XML::addChildren(Body_xle, File_info)
        Company <- XML::newXMLNode("Company", "")
        License <- XML::newXMLNode("LICENSE", "")
        Date <- XML::newXMLNode("Date", format(Sys.Date(), "%Y/%m/%d"))
        Time <- XML::newXMLNode("Time", format(Sys.time(),"%T"))
        FileName <- XML::newXMLNode("Filename", "")
        Created_by <- XML::newXMLNode("created_by", "Version 4.6.1")
        Downloaded_by <- XML::newXMLNode("Downloaded_by", "")
        ls_File_info <- list(Company, License, Date, Time, FileName, Created_by, Downloaded_by)
        for(i in ls_File_info){
          XML::addChildren(File_info, i)}

        Instrument_info <- XML::newXMLNode("Instrument_info")
        XML::addChildren(Body_xle, Instrument_info)
        Instrument_type <- XML::newXMLNode("Instrument_type", stringr::word(LL_header[LL_header$Param == "Type", 2], start = 1, end = 1, sep = ", "))
        Model_number <- XML::newXMLNode("Model_number", stringr::word(LL_header[LL_header$Param == "Type", 2], start = 2, end = 2, sep = ", "))
        Instrument_state <- XML::newXMLNode("Instrument_state", LL_header[LL_header$Param == "State", 2])
        Serial_number <- XML::newXMLNode("Serial_number", LL_header[LL_header$Param == "Serial", 2])
        Battery_level <- XML::newXMLNode("Battery_level", LL_header[LL_header$Param == "Battery", 2])
        Channel <- XML::newXMLNode("Channel", "3")
        Firmware <- XML::newXMLNode("Firmware", stringr::word(LL_header[LL_header$Param == "Type", 2], start = 3, end = 3, sep = ", "))
        ls_Instrument_info <- list(Instrument_type, Model_number, Instrument_state, Serial_number, Battery_level, Channel, Firmware)
        for(i in ls_Instrument_info){
          XML::addChildren(Instrument_info, i)}

        Instrument_info_data_header <- XML::newXMLNode("Instrument_info_data_header")
        XML::addChildren(Body_xle, Instrument_info_data_header)
        Project_ID <- XML::newXMLNode("Project_ID", "YOWN")
        Location <- XML::newXMLNode("Location",  LL_header[LL_header$Param == "Location", 2])
        Latitude <- XML::newXMLNode("Latitude", "0.000")
        Longitude <- XML::newXMLNode("Longitude", "0.000")
        Sample_rate <- XML::newXMLNode("Sample_rate", LL_header[LL_header$Param == "Log Rate", 2])
        Sample_mode <- XML::newXMLNode("Sample_mode", "0")
        Event_ch <- XML::newXMLNode("Event_ch", "1")
        Event_threshold <- XML::newXMLNode("Event_threshold", "0.000000")
        Schedule <- XML::newXMLNode("Schedule")
        Start_time <- XML::newXMLNode("Start_time", as.character(min(data$Timestamp_MST)))
        Stop_time <- XML::newXMLNode("Stop_time", as.character(max(data$Timestamp_MST)))
        #TODO: object df below is not define prior to here
        Num_log <- XML::newXMLNode("Num_log", nrow(df))
        ls_Instrument_info_data_header <- list(Project_ID, Location, Latitude, Longitude, Sample_rate, Sample_mode, Event_ch, Event_threshold, Schedule, Start_time, Stop_time, Num_log)
        for(i in ls_Instrument_info_data_header){
          XML::addChildren(Instrument_info_data_header, i)}

        headers <- c("Ch1_data_header", "Ch2_data_header","Ch3_data_header")
        for(i in 1:length(headers)){
          if(i == 1){
            X <- XML::newXMLNode(headers[i])
            assign(headers[i], X)
            Identification <- XML::newXMLNode("Identification", Ch_dataheader[i, 1])
            Unit <- XML::newXMLNode("Unit", Ch_dataheader[i, 2])
            Parameters <- XML::newXMLNode("Parameters", Ch_dataheader[i, 3])
            Offset <- XML::newXMLNode("Offset", attrs = c(Val = "0", Unit = "m"))
            Altitude <- XML::newXMLNode("Altitude", attrs = c(Val = "0", Unit = "m"))
            Density <- XML::newXMLNode("Density", attrs = c(Val = "1.00", Unit = "kg/L"))
            ls_Parameters <- list(Offset, Altitude, Density)
            for(i in ls_Parameters){
              XML::addChildren(Parameters, i)}
          } else {
            X <- XML::newXMLNode(headers[i])
            assign(headers[i], X)
            Identification <- XML::newXMLNode("Identification", Ch_dataheader[i, 1])
            Unit <- XML::newXMLNode("Unit", Ch_dataheader[i, 2])
            Parameters <- XML::newXMLNode("Parameters", Ch_dataheader[i, 3])
          }
          ls_Ch_data_header <- list(Identification, Unit, Parameters)
          for(j in ls_Ch_data_header){
            XML::addChildren(X, j)}
          XML::addChildren(Body_xle, X)
        }

        Data <- XML::newXMLNode("Data")
        XML::addChildren(Body_xle, Data)

        for(i in 1:nrow(data)){
          log <- XML::newXMLNode("Log", attrs = c(id = as.character(i)))
          XML::addChildren(Data, log)
          Date <- XML::newXMLNode("Date", format(as.Date(data$Timestamp_MST[i], "%d/%m/%Y"), "%Y/%m/%d"))
          Time <- XML::newXMLNode("Time", format(as.POSIXct(data$Timestamp_MST[i], format = "%d/%m/%Y %H:%M:%S"), "%H:%M:%S"))
          ms <- XML::newXMLNode("ms", "0")
          ch1 <- XML::newXMLNode("ch1", data$LEVEL[i])
          ch2 <- XML::newXMLNode("ch2", data$TEMPERATURE[i])
          ch3 <- XML::newXMLNode("ch3", data$CONDUCTIVITY[i])
          ls_log <- list(Date, Time, ms, ch1, ch2, ch3)
          for(j in ls_log){
            XML::addChildren(log, j)}
        }

        # Save file into the XLE_File_Dropbox folder for normal processing
        XML::saveXML(xml, file = paste0(path, LL_header[LL_header$Param == "Location", 2], "_", gsub("/", "", substr(as.character(min(data$Timestamp_MST)), start = 1, stop = 10)), "_to_", gsub("/", "", substr(as.character(max(data$Timestamp_MST)), start = 1, stop = 10)), "_TELEM.xle"))

      }
    }
  }
  print(paste0("Completed telemetry processing for: ", z))
}


