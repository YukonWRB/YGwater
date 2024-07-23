# YOWN SOlinst Telemetry V2, direct upload to Aquarius without the need to create .xle files
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
#'
# YOWN_solinst_telem <- function(active_telem = c("YOWN-2201S"), path = "G:/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/9_XLE_file_dropbox/", auth_json = "G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\4_YOWN_DATA_ANALYSIS\\1_WATER LEVEL\\00_AUTOMATED_REPORTING\\04_TELEMETRY\\gmail_interface.json"){

# Development parameters, leave as comments
active_telem = c("YOWN-2305")
path = "G:/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/9_XLE_file_dropbox/"
auth_json = "G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\4_YOWN_DATA_ANALYSIS\\1_WATER LEVEL\\00_AUTOMATED_REPORTING\\04_TELEMETRY\\gmail_interface.json"

# Import list of YOWN IDs and names for matching
master <- openxlsx::read.xlsx(xlsxFile = "G:/water//Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/2_SPREADSHEETS/1_YOWN_MASTER_TABLE/YOWN_MASTER.xlsx", sheet = "YOWN_MASTER")
YOWNnames <- paste(master$YOWN.Code, master$Name)[!grepl("NA NA", paste(master$YOWN.Code, master$Name))]

#### Gmail configuration ####
rlang::check_installed("gmailr", reason = "Package gmailr is required to use function YOWN_solinst_telem")
gmailr::gm_auth_configure(path = auth_json)
gmailr::gm_auth(email = T, cache = ".secret")

for (z in active_telem) {
  query <- paste0("from:", z, " ", "subject:(LS Report)") # Create gmail search query
  file <- gmailr::gm_messages(search = query,
                              num_results = 1000000000) # Generate a list of all emails for z telem site, if no results length will be 1
  
  #### Message identification and sorting ####
  # if(length(file[[1]]) > 1){
  messageIDs <- unique(unlist(file))
  messageIDs <- messageIDs[nchar(messageIDs) == 16]
  
  # Download body text for each email into a df
  body_list <- list()
  for (i in messageIDs) {
    msg <- gmailr::gm_message(i) # Run query to find matching email
    body <- gmailr::gm_body(msg) # Extract body of message
    body_list[[i]] <- body # Add each message to list
    # gmailr::gm_trash_message(i)
  }
  
  #### Extract and check data from message body, upload to Aquarius, append data to master _telem .csv in appropriate folder ####
  
  for (i in body_list) {
    lines <- unlist(strsplit(unlist(i), "\r\n"))
    lines <- lines[lines != ""]
    
    # Extract LevelSender heading
    ls_head_st_index <- which(lines == "Logger 1") # Find the index of the line containing "Logger 1"
    LS_heading <- lines[1:( ls_head_st_index - 1)] # Extract all lines before "Logger 1"
    LS_head_df <- data.frame(matrix(ncol = 2, nrow = length(LS_heading)))
    colnames(LS_head_df) = c("field", "value")
    for (j in 1:length(LS_heading)) {
      LS_head_df[j, 1] <- stringr::str_split_i(LS_heading[j], ": ", 1)
      if (length(unlist(stringr::str_split(LS_heading[j], ": "))) > 1) {
        LS_head_df[j, 2] <- stringr::str_split_i(LS_heading[j], ": ", 2)
      }
    }
    
    # Extract Logger heading
    ltc_head_st_index <- which(lines == "Logger 1") # Find the index of the line containing "Logger 1" 
    ltc_head_end_index <- which(lines == "Logger 2") # Find the index of the line containing "Logger 2"
    LTC_heading <- lines[(ltc_head_st_index):(ltc_head_end_index - 1)] # Extract all lines after "Logger 1" and before "Logger 2"
    LTC_head_df <- data.frame(matrix(ncol = 2, nrow = length(LTC_heading)))
    colnames(LTC_head_df) = c("field", "value")
    for (j in 1:length(LTC_heading)) {
      LTC_head_df[j, 1] <- stringr::str_split_i(LTC_heading[j], ": ", 1)
      if (length(unlist(stringr::str_split(LTC_heading[j], ": "))) > 1) {
        LTC_head_df[j, 2] <- stringr::str_split_i(LTC_heading[j], ": ", 2)
      }
    }
    
    # Extract data
    data_st_index <- which(lines == "Logger 1 Samples") # Find the index of the line containing "Logger 1 Samples" 
    data_txt <- lines[(data_st_index + 1 ):(length(lines) - 1)]
    data <- data.frame(matrix(ncol = 4, nrow = (length(data_txt) - 1)))
    colnames(data) <- unlist(stringr::str_split(data_txt[1], ", "))
    
    # Format into data frame
    data_lines <- stringr::str_split(data_txt, "\n") # Split the text into lines
    data_lines <- data_lines[2:length(data_lines)]
    for (i in 2:nrow(data)) {
      data$Time <- unlist(stringr::str_split_i(data_lines, ", ", 1))
      data$`Temperature( C)` <- unlist(stringr::str_split_i(data_lines, ", ", 2))
      data$`Level(m)` <- unlist(stringr::str_split_i(data_lines, ", ", 3))
      data$`Conductivity(uS/cm)` <- unlist(stringr::str_split_i(data_lines, ", ", 4))
    }
    data$Time <- as.POSIXct(data$Time, format = "%d/%m/%Y %H:%M:%S", tz = "MST")
    # Change timestamps from UTC to MST
    attr(data$Time , "tzone") <- "UTC"
    
    # Check units, stop if not correct
    if (all(colnames(data) != c("Time", "Temperature( C)", "Level(m)", "Conductivity(uS/cm)"))) {
      email <- gmailr::gm_mime() %>%
        gmailr::gm_to("cole.fischer@yukon.ca") %>%
        gmailr::gm_from("YOWNtelemetry@gmail.com") %>%
        gmailr::gm_subject("LevelSender Error") %>%
        gmailr::gm_text_body(paste("LevelSender", LS_head_df$value[2], "has encountered an error"))
      gmailr::gm_send_message(email)
      stop("LevelSender has encountered an error during units check")
    }
    
    #### Upload to Aquarius ####
    # Parse time series name
    for (i in colnames(data)[2:4]) {
      ts_ID <- grep(gsub("\\(.*?\\)", "", i), c("Water Temp.TEMPERATURE", "Wlevel_Hgt.level_RAW", "Conductivity Field.Econdy-F"), value = TRUE, ignore.case = TRUE)[1]
      df <- data.frame("Time" = data$Time,
                       "Value" = data[, i])
      YGwater::aq_upload(loc_id = z,
                         ts_name = ts_ID,
                         data = df,
                         login = Sys.getenv(c("AQUSER", "AQPASS")),
                         server = "https://yukon.aquaticinformatics.net/AQUARIUS")
    }
    
    #### Create or add data to .csv in appropriate folder ####
    # Check to see if year directory exists, if not create it
    directory <- paste0("G:/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/1_YOWN_SITES/1_ACTIVE_WELLS/",  grep(z, YOWNnames, value = TRUE), "/Logger files and notes/", lubridate::year(max(data$Time)))
    if (!file.exists(directory)) {
      dir.create(directory, recursive = TRUE)
    }
    
    # Check to see if telem file exists, if not write excel file with one tab per LTC serial # and one for LS heading
    telem_xlsx_path <- paste0(directory, "/", "telem_LS_", LS_head_df$value[LS_head_df$field == "Serial"], ".xlsx")
    if (!file.exists(telem_xlsx_path)) { #Check if telem file exists, if not write it and add telem unit metadata tab
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "LS_header")
      openxlsx::writeData(wb = wb, sheet = "LS_header", na.omit(c(rbind(as.character(LS_head_df$field), as.character(LS_head_df$value)))), colNames = FALSE)
      openxlsx::saveWorkbook(wb, file = telem_xlsx_path)
    }
    
    telem_xlsx <- openxlsx::loadWorkbook(telem_xlsx_path) # Load telem xlsx for editing
    sheet_names <- as.vector(openxlsx::getSheetNames(telem_xlsx_path)) # Extract sheet names
    LTC_tabName <- paste0(LTC_head_df[LTC_head_df$field == "Serial", "value"], " ", as.POSIXct(format = "%d/%m/%Y", tz = "MST", LTC_head_df[LTC_head_df$field == "Start Logger", "value"])) # Extract LTC serial and start date for sheet naming
    
    if (!LTC_tabName %in% sheet_names) { # Check if LTC serial has dedicated tab, if not create it. Add LTC metadata and column headings
      telem_xlsx <- openxlsx::loadWorkbook(telem_xlsx_path)
      openxlsx::addWorksheet(telem_xlsx, LTC_tabName)
      openxlsx::writeData(wb = telem_xlsx, sheet = LTC_tabName, na.omit(c(rbind(as.character(LTC_head_df$field), as.character(LTC_head_df$value)))), colNames = FALSE) # Write LTC metadata
      openxlsx::saveWorkbook(telem_xlsx, file = telem_xlsx_path, overwrite = TRUE)
      sheet <- openxlsx::readWorkbook(telem_xlsx_path, LTC_tabName)
      openxlsx::writeData(wb = telem_xlsx, sheet = LTC_tabName, x = matrix(c("Time", "Temperature( C)", "Level(m)",	"Conductivity(uS/cm)"
      ), ncol = 4), startRow = max(which(!is.na(sheet[,1]) & sheet[,] != "") + 2), startCol = 1, colNames = FALSE) # Write column headings
      openxlsx::saveWorkbook(telem_xlsx, file = telem_xlsx_path, overwrite = TRUE)
    }
    
    # Change data back to MST, write to excel sheet, checking to make sure header row is unchanged
    header_row <- names(openxlsx::read.xlsx(telem_xlsx, sheet = LTC_tabName, rows = length(na.omit(c(rbind(as.character(LTC_head_df$field), as.character(LTC_head_df$value))))) + 1 , sep.names = " "))
    
    if (all(header_row == names(data))) {
      attr(data$Time, "tzone") <- "MST"
      telem_xlsx <- openxlsx::loadWorkbook(telem_xlsx_path)
      sheet <- openxlsx::readWorkbook(telem_xlsx_path, LTC_tabName)
      openxlsx::writeData(wb = telem_xlsx, sheet = LTC_tabName, startRow = length(sheet$Logger.1) + 2, x = data, colNames = FALSE)
      openxlsx::saveWorkbook(telem_xlsx, file = telem_xlsx_path, overwrite = TRUE)
      sheet <- openxlsx::readWorkbook(telem_xlsx_path, LTC_tabName)
      sort_start_row <- length(na.omit(c(rbind(as.character(LTC_head_df$field), as.character(LTC_head_df$value))))) + 2
      sorted_data <- sheet[sort_start_row:nrow(sheet),]
      colnames(sorted_data) <- c("Time", "Temperature( C)", "Level(m)",	"Conductivity(uS/cm)")
      sorted_data <- sorted_data[order(sorted_data$Time, decreasing = TRUE), ]
      sheet[sort_start_row:nrow(sheet), ] <- sorted_data
      openxlsx::writeData(telem_xlsx, sheet = LTC_tabName, x = sheet, colNames = FALSE)
      openxlsx::saveWorkbook(telem_xlsx, file = telem_xlsx_path, overwrite = TRUE)
    } else {
      email <- gmailr::gm_mime() %>%
        gmailr::gm_to("cole.fischer@yukon.ca") %>%
        gmailr::gm_from("YOWNtelemetry@gmail.com") %>%
        gmailr::gm_subject("LevelSender Error") %>%
        gmailr::gm_text_body(paste("LevelSender processing has encountered an error"))
      gmailr::gm_send_message(email)
      stop("Something is wrong with the data headers")
    }
  }
}
  
  
  
  
  
  
