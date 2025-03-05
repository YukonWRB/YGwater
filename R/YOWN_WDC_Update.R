#' YOWN Water Data Catalogue Update Function
#'
#' @param WDC_struc Link to master directory containing folders linked to the WDC
#' @param master_sheet Path to YOWN master sheet
#'
#' @return Updated files in the WDC folder structure
#' @export

YOWN_WDC_Update <- function(
    WDC_struc = "\\\\envgeoserver\\share\\WaterResources\\Groundwater\\YOWN_DATA\\",
    master_file = "G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\2_SPREADSHEETS\\1_YOWN_MASTER_TABLE\\YOWN_MASTER.xlsx") {
  
  
  # Function arg checks
  if (!file.exists(master_file)) {
    stop("Master file not found, check file location")
  } else { # File exists, so if reading fails it must mean the sheet doesn't exist
    #Read in reference sheets and logger drop folder
    tryCatch({
      master_sheet <- openxlsx::read.xlsx(master_file, sheet = "YOWN_MASTER")
    }, error = function(e) {
      stop("Master Excel file found but not not the sheet named 'YOWN_MASTER', check the file and try again.")
    })
    # Make sure the table has the required columns
    tryCatch({
      publish <- master_sheet$Publish
      YOWNIDs <- master_sheet$YOWN.Code[master_sheet$Publish == "YES"]
    }, error = function(e) {
      stop("Master Excel file found but not not the column named 'Publish', check the file and try again.")
    })
  }
}
  
  # Iterate through YOWNIDs
  for (i in YOWNIDs) {
    print(i)
    print("Writing data")
    # Download raw data list, extract timeseries
    raw_data_list <- aq_download(loc_id = i, ts_name = "Wlevel_Hgt.level_RAW", start = "1990-01-01", end = Sys.Date())
    raw_ts <- raw_data_list[["timeseries"]]
    write.csv(raw_ts, file = paste0(WDC_struc, i, "\\", i, "_raw.csv"))
    
    # Download corrected BTOC data and metadata
    corr_data_list <- aq_download(loc_id = i, ts_name = "Wlevel_btoc.Calculated", start = "1990-01-01", end = Sys.Date())
    corr_ts <- corr_data_list[["timeseries"]]
    corr_meta <- corr_data_list[["metadata"]] %>%
      dplyr::filter(attribute != "TS name")
    utils::write.csv(corr_ts, file = paste0(WDC_struc, i, "\\", i, "_btoc_corr.csv"))
    utils::write.csv(corr_meta, file = paste0(WDC_struc, i, "\\", i, "_meta.csv"))
    
    # Download bgs data
    bgs_data_list <- aq_download(loc_id = i, ts_name = "Wlevel_bgs.Calculated", start = "1990-01-01", end = Sys.Date())
    bgs_ts <- bgs_data_list[["timeseries"]]
    utils::write.csv(bgs_ts, file = paste0(WDC_struc, i, "\\", i, "_bgs.csv"))
    
    # Copy most current version of grade key
    file.copy(from = "G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\4_YOWN_DATA_ANALYSIS\\1_WATER LEVEL\\00_AUTOMATED_REPORTING\\02_R_SUPPORT_FILES\\YOWN_GradeKey.txt",
              to = paste0(WDC_struc, i, "\\grade_key.txt"),
              overwrite = TRUE)
    
    print("Writing plots")
    # Generate plots
    YOWNplot(AQID = i,
             stats = FALSE,
             saveTo = paste0("\\\\envgeoserver\\share\\WaterResources\\Groundwater\\YOWN_DATA\\"))
    
    YOWNplot(AQID = i,
             stats = "ribbon",
             saveTo = paste0("\\\\envgeoserver\\share\\WaterResources\\Groundwater\\YOWN_DATA\\"))
    
    YOWNplot(AQID = i,
             stats = "line",
             saveTo = paste0("\\\\envgeoserver\\share\\WaterResources\\Groundwater\\YOWN_DATA\\"))
    
    # Remove i from YOWNIDs
    YOWNIDs <- YOWNIDs[-which(YOWNIDs == i)]
    
  }
}



