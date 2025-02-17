#' YOWN Water Data Catalogue Update Function
#'
#' @param WDC_struc Link to master directory containing folders linked to the WDC
#' @param master_sheet Path to YOWN master sheet
#'
#' @return Updated files in the WDC folder structure
#' @export

YOWN_WDC_Update <- function(
    WDC_struc = "\\\\envgeoserver\\share\\WaterResources\\Groundwater\\YOWN_DATA\\",
    master_sheet = "G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\2_SPREADSHEETS\\1_YOWN_MASTER_TABLE\\YOWN_MASTER.xlsx") {
  
  
  # Function arg checks
  if (!dir.exists(WDC_struc)) {
    stop("WDC_struc does not exist")
  }
  if (!file.exists(master_sheet)) {
    stop("master_sheet does not exist")
  }
  
  # Import YOWN master spreadsheet
  master_sheet <- openxlsx::read.xlsx(master_sheet, sheet = "YOWN_MASTER")
  
  # Extract list of YOWN codes which contain a Y in the "publish?" column
  YOWNIDs <- master_sheet$YOWN.Code[master_sheet$Publish == "YES"]
  if (length(YOWNIDs) == 0) {
    stop("No YOWN codes found, check spreadsheet headings")
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
    write.csv(bgs_ts, file = paste0(WDC_struc, i, "\\", i, "_bgs.csv"))
    
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



