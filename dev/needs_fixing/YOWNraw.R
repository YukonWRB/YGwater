#' Export raw data from Aquarius
#'
#' @description
#' Wrapper around aq_download function to export three types of data for YOWN sites
#'
#' Export of raw data from aquarius to csv, writes 3 data frames for raw data, compensated data, and manually corrected data
#'
#' @param AQID YOWN location
#' @param saveTo Directory in which the data will be saved. Can specify "desktop" to automatically create YOWN ID folder on desktop as save directory.
#' @param login Aquarius username and password, taken from Renviron files
#' @param server Aquarius server ID
#'
#' @return Writes three csv files containing YOWN data in the specified directory.
#' @export

#REVIEW This function looks like it could easily be used for data other than just YOWN. Consider renaming the function and reviewing the parameters to make it clear to the user that it can work with any AQID
#REVIEW Right now you've got this set up to work only for the water level timeseries. Can you make a new parameter, perhaps called tsid or something, where the user can specify the timeseries? That would make this function more versatile.
#REVIEW It would be great if the user could specify, as a parameter, the cut-off grade at or below which "redacted" is applied. Consider adding this as a parameter.
#NOTE: This function should return the data.frames as an environment object.

YOWNraw <- function(AQID,
                    dateRange = "all",
                    saveTo = "desktop",
                    login = Sys.getenv(c("AQUSER", "AQPASS")),
                    server ="https://yukon.aquaticinformatics.net/AQUARIUS")
  {

  # # Debug and development params. Leave as comments.
  # AQID = "YOWN-0101"
  # timeSeriesID = "Wlevel_Hgt.level_RAW"
  # saveTo = "desktop"
  # login = Sys.getenv(c("AQUSER", "AQPASS"))
  # server ="https://yukon.aquaticinformatics.net/AQUARIUS"

  #### Setup ####
  # Sort out save location
  saveTo <- tolower(saveTo)
  if(saveTo == "desktop") {
    saveTo <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/Desktop/")
  } else if (dir.exists(saveTo) == FALSE) {
    stop("Specified directory does not exist. Consider specifying save path as one of 'choose' or 'desktop'; refer to help file.")
  }

  #### Download time series data from Aquarius, preliminary formatting ####
  # Download data from Aquarius
  list <- list()
  for(i in c("Wlevel_Hgt.level_RAW", "Wlevel_Hgt.Compensated", "Wlevel_bgs.Calculated")){
    datalist <- suppressMessages(aq_download(loc_id = AQID,
                                                       ts_name = i,
                                                       server = server,
                                                       login = login))

    # Unlist time series data
    timeseries <- datalist$timeseries

    # Replace all grades below C with "Redacted"
    timeseries$grade_description[timeseries$grade_description != "A" & timeseries$grade_description != "B" & timeseries$grade_description != "C" & timeseries$grade_description != "MISSING DATA"] <- "REDACTED"

    # Change timestamps from UTC to MST
    attr(timeseries$timestamp_UTC , "tzone") <- "MST"
    names(timeseries)[names(timeseries) == "timestamp_UTC"] <- "timestamp_MST"
    # final format, write to .csv
    if(i == "Wlevel_Hgt.level_RAW"| i == "Wlevel_Hgt.Compensated"){
      fulldf <- timeseries %>%
      dplyr::select(c("timestamp_MST", "value"))
    } else {
      fulldf <- timeseries %>%
        dplyr::select(c("timestamp_MST", "value", "grade_level", "grade_description"))
    }
    list[[i]] <- fulldf
  }
  openxlsx::write.xlsx(x = list, file = paste0(saveTo, "/", AQID, "_RAW.xlsx"), rowNames = FALSE)
}




