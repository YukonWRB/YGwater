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
    if (nchar(body) < 5) {
      gmailr::gm_trash_message(i)
    } else {
    body_list[[i]] <- body # Add each message to list
    }
    
  }
  
  #### Extract and check data from message body, upload to Aquarius ####
  
  for (n in 1:length(body_list)) {
    lines <- unlist(strsplit(unlist(body_list[[n]]), "\r\n"))
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
data <- data %>%
  dplyr::select(c("Time", "Temperature( C)", "Level(m)", "Conductivity(uS/cm)"))
    
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
    gmailr::gm_trash_message(names(body_list[n]))
  }
} 
    
