#' YOWN raw data writing
#'
#' This function downloads YOWN level data from Aquarius, screens data graded below "C", writes the csv file containing the entire period of record, and copies the grading key from the master location to the SaveTo directory
#'
#' To store login credentials in your .renviron profile, run usethis::edit_r_environ() and enter your username and password as value pairs, as AQUSER="your username" and AQPASS = "your password".
#'
#' @param AQID Identity of YOWN site in the following format: "YOWN-XXXX" or "YOWN-XXXXD"
#' @param timeSeriesID Identity of the time series exactly as written in Aquarius (eg."Wlevel_bgs.Calculated")
#' @param saveTo Location for data files to be saved. Will create directory if it doesn't exist. Defaults to user's desktop.
#' @param login Your Aquarius login credentials as a character vector of two (eg. c("cmfische", "password") Default pulls information from your .renviron profile; see details. Passed to [aq_download()].
#' @param server The URL for your organization's Aquarius web server. Default is for the Yukon Water Resources Branch. Passed to [aq_download()].
#'
#' @return Writes .pdf of the full period of record for a specified YOWN site and time series ID, as well as a .csv containing all the raw data and copies a grade key to interpret data grading
#' @export
#'
YOWNplot_RawData <- function(AQID,
                             timeSeriesID="Wlevel_bgs.Calculated",
                             saveTo = "desktop",
                             login = Sys.getenv(c("AQUSER", "AQPASS")),
                             server ="https://yukon.aquaticinformatics.net/AQUARIUS")
{

  # AQID = "YOWN-1901"
  # timeSeriesID="Wlevel_bgs.Calculated"
  # chartXInterval ="1 year"
  # saveTo = "desktop"
  # login = Sys.getenv(c("AQUSER", "AQPASS"))
  # AQTSServerID ="https://yukon.aquaticinformatics.net/AQUARIUS"

  # Sort out save location
  saveTo <- tolower(saveTo)
  if (save_path %in% c("Choose", "choose")) {
    print("Select the folder where you want this graph saved.")
    save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
  } else if(saveTo == "desktop") {
    saveTo <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/Desktop/")
  } else if (dir.exists(saveTo) == FALSE) {
    stop("Specified directory does not exist. Consider specifying save path as one of 'choose' or 'desktop'; refer to help file.")
  }

  # Download data from Aquarius
  timeRange = c("00:00:00", "23:59:59")
  datalist <- suppressMessages(aq_download(loc_id = AQID,
                                                     ts_name = "Wlevel_bgs.Calculated",
                                                     login = login,
                                                     server = server))

  # Unlist time series data
  timeseries <- datalist$timeseries

  # Change AQTS number grades into letters, replace all below C with "Redacted"
  timeseries$grade_description[timeseries$grade_description != "A" & timeseries$grade_description != "B" & timeseries$grade_description != "C" & timeseries$grade_description != "MISSING DATA"] <- "REDACTED"

  # Replace all values with  grade of less than C with NA, to remove values from plots. This screens out GW recovery patterns
  timeseries$value[timeseries$grade_description != "A" & timeseries$grade_description != "B" & timeseries$grade_description != "C" & timeseries$grade_description != "Missing Data"] <- NA

  # Change timestamps from UTC to MST
  attr(timeseries$timestamp_UTC , "tzone") <- "MST"
  names(timeseries)[names(timeseries) == "timestamp_UTC"] <- "timestamp_MST"

  #Find data gaps of greater than 6 hours (indicative of logger failure) and generate value NA data sets to fill in gaps
  timeseries$ts_lag <- dplyr::lag(timeseries$timestamp_MST)
  timeseries$lag_val <- difftime(timeseries$timestamp_MST, timeseries$ts_lag, units = "hours")
  gapdf <- timeseries %>%
    dplyr::filter(lag_val > 6)
  gapdf$lag_val <- as.numeric(gapdf$lag_val)

  # If there are gaps present, fill in gaps with hourly timestamps, with NA values in the "value" column
  if(nrow(gapdf != 0)){
    # Create a list of data frames for each identified data gap, fill in hourly time stamps
    gaplist <- list()
    for(i in 1:nrow(gapdf)) {
      df <- data.frame(seq.POSIXt(from = gapdf[i, 1], by = "-1 hour", length.out = gapdf[i, 8]), NA, as.character(-5), "MISSING DATA", gapdf$approval_level[i], gapdf$approval_description[i], NA, NA)
      colnames(df) <- colnames(gapdf)
      gaplist[[i]] <- df
    }

    # Merge all listed gap data frames, combine with original timeseries, order and format. I fno gaps proceed with base timeseries
    gapmerge <- do.call(rbind, gaplist)
    rawdf <- suppressMessages(dplyr::full_join(timeseries, gapmerge))
  } else {
    rawdf <- timeseries
  }

  rawdf <- rawdf[order(rawdf$timestamp_MST),] # Order by timestamp
  rawdf <- rawdf[!duplicated(rawdf["timestamp_MST"]),] #Remove second entry for duplicated timestamps
  rawdf <- rawdf %>%
    dplyr::select(timestamp_MST,
                   value,
                   grade_description)
  rawdf$value <- round(rawdf$value, 3)


  dir.create(paste0(saveTo, "/", AQID), showWarnings = FALSE)

  # Prepare and write time series and grading csv exports
  utils::write.csv(x = rawdf,
            file = paste0(saveTo, "/", AQID, "/", AQID, "_FullRecord", ".csv"),
            row.names = FALSE)

  file.copy(from = "G:/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/4_YOWN_DATA_ANALYSIS/1_WATER LEVEL/00_AUTOMATED_REPORTING/02_R_SUPPORT_FILES/YOWN_GradeKey.txt",
            to = paste0(saveTo, "/", AQID, "/", "YOWN_GradeKey.txt"),
            overwrite = TRUE)

  print(paste0("Grade key and full record .csv written to ", saveTo, "/", AQID))
}







