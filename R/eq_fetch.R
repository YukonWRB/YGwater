#' Data retrieval from EQWin
#'
#' Fetches sample data from the WRB database and returns a list of data frames suitable for modification for plot generation and other comparisons
#'
#' @details Insert here what happens to values > DL, where the standards are taken from,
#'
#' @param EQcode Site code as it appears in EQWin eg. "(LOB)" or "(KNO)". Function only works for stations with  designated project code in brackets
#' @param stationIDs "all" for all stations (default) OR character vector of selected stations as they appear in the EQWin database WITHOUT the EQcode c("MW-01", "MW-02)
#' @param paramIDs "all" for all parameters (default) OR vector of selected parameters exactly as they appear in the EQWin database
#' @param dates "all" for all dates (default) OR character vector of length 2 of start and end date in format c("YYYY-MM-DD", "YYYY-MM-DD")
#' @param BD Treatment of values below detection limits (0 = Set to zero; 1 = Set to NA; 2 = Set to 0.5*(LOD); 3 = Set to sqrt(2)LOD). Above detection values are set to the upper limit of detection.
#' @param apply_standards TRUE or FALSE, include standards with data. Provides a pop-up list for selection.
#' @return A list with one sub-list per station, each one containing 2 data frames with sample data and calculated standards
#'
#' @export

eq_fetch <- function(EQcode,
                     stationIDs = "all",
                     paramIDs = "all",
                     dates = "all",
                     BD = 2,
                     apply_standards = TRUE){

  EQcode <- "WLV"
  stationIDs <- "all"
  # Specify a vector of station IDs (eg. c("(LOB)GW-4", "(LOB)GW-5")) OR "all"
  # paramIDs <- c("Al-D","As-D","Cd-D","Cu-D","Fe-D","Hg-D","Pb-D","Se-D","U-D","Zn-D","Fluord","SO4","N-NH4") # Specify a vector of parameter IDs (eg. c("Zn-T, Zn-D") OR "all")
  paramIDs = "all"
  dates <- "all"
  BD <- 2
  apply_standards = TRUE

  # Set a few options (I'll probs remove these)

  old_scipen <- getOption("scipen")
  old_dplyr <- getOption("dplyr.summarise.inform")
  options(dplyr.summarise.inform = FALSE)
  options(scipen = 999)
  on.exit(options(scipen = old_scipen), add = TRUE)
  on.exit(options(dplyr.summarise.inform = old_dplyr), add = TRUE)

  # Set path to access database
  dbpath <- "X:/EQWin/WR/DB/Water_Resources.mdb"

  #### Begin EQWin fetch ####
  EQWin <- DBI::dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", dbpath))
  on.exit(DBI::dbDisconnect(EQWin), add = TRUE)

  # Download stations and filter to user input
  eqstns<- DBI::dbGetQuery(EQWin, "SELECT StnId, StnCode, StnName, StnType, udf_Stn_Status FROM eqstns WHERE StnCode")

  tryCatch({
    if(tolower(paste(stationIDs, collapse = "")) == "all"){
      stns <- eqstns %>%
        dplyr::filter(stringr::str_detect(StnCode, paste0("^", "\\(", EQcode, "\\)"))) %>%
        dplyr::mutate(StnCode = gsub(paste0("(", EQcode, ")"), "", StnCode, fixed = TRUE))
    } else if(all(paste0("(", EQcode, ")", stationIDs) %in% eqstns$StnCode)){
      stns <- eqstns %>%
        dplyr::filter(stringr::str_detect(StnCode, paste0("^", "\\(", EQcode, "\\)")))%>%
        dplyr::mutate(StnCode = gsub(paste0("(", EQcode, ")"), "", StnCode, fixed = TRUE)) %>%
        dplyr::filter(StnCode %in% stationIDs)
    } else {
      stop()
    }
  },
  error = function(e) {
    message("Please check your Station IDs")
    print(e)
  }
  )

  # Download all samples for specified stations, filter by user choice
  eqsampls <- DBI::dbGetQuery(EQWin, "SELECT SampleId, StnId, CollectDateTime FROM eqsampls") %>%
    dplyr::filter(StnId %in% stns$StnId)

  tryCatch({
    if(tolower(paste(dates, collapse = "") != "all")){
      samps <- eqsampls %>%
        dplyr::filter(dplyr::between(as.Date(CollectDateTime), as.Date(dates[1]), as.Date(dates[2])))
    } else {
      samps <- eqsampls
    }},
    error = function(e) {
      message("Please check your date format")
    }
  )

  # Download list of all parameters, filter to user choice
  eqparams <- as.data.frame(DBI::dbGetQuery(EQWin, "SELECT ParamId, ParamCode, Units FROM eqparams"))
  tryCatch({
    if(tolower(paste(paramIDs, collapse = "") == "all")){
      params <- eqparams
    } else if(all(paramIDs %in% eqparams$ParamCode)){
      params <- subset(eqparams,ParamCode %in% paramIDs)
    } else {
      stop()}
  },
  error = function(e){
    message("Please check your parameter list")
    print(e)
  }
  )

  # Download all results
  print("Fetching sample results")
  results <- DBI::dbGetQuery(EQWin, paste0("SELECT ", paste0('SampleId', ", ", 'ParamId', ", ", 'Result'), " FROM eqdetail WHERE ParamID IN (", paste(eqparams$ParamId, collapse = ", "),") AND SampleId IN (", paste0(eqsampls$SampleId, collapse = ", "), ")"))

  # Deal with values below detection limits according to user choice
  if(BD == 0){
    results[grepl(results$Result, pattern = "<"),] <- 0
    results$Result <- suppressWarnings(as.numeric(results$Result))
  } else if(BD == 1){
    results$Result <- suppressWarnings(as.numeric(results$Result))
  } else if(BD == 2){
    isBD <- grepl("<", results$Result)
    results$Result[isBD] <- round(as.numeric(gsub("<(.*)", "\\1", results$Result[isBD]))/2, digits = 7)
    rm(isBD)
  } else if(BD == 3){
    isBD <- grepl("<", results$Result)
    results$Result[isBD] <- round(as.numeric(gsub("<(.*)", "\\1", results$Result[isBD]))/sqrt(2), digits = 7)
    rm(isBD)
  }

  # Deal with values above the detection limit (frequently occurs with turbidity)
  results$Result <- gsub(">", "", results$Result)

  # Sequentially merge data frames to agglomerate samples, pivot to wide format and minor formatting tweaks
  merge1 <- merge(samps, stns, by.x = "StnId", by.y = "StnId")
  merge2 <- merge(results, merge1, by.x = "SampleId", by.y = "SampleId")
  merge3 <- merge(merge2, params, by.x = "ParamId", by.y = "ParamId")
  suppressMessages(sampledata <- merge3 %>%
                     dplyr::mutate(Param = paste0(merge3$ParamCode, " (", merge3$Units, ")")) %>%
                     dplyr::select(StnCode, CollectDateTime, StnType, Param, Units, Result) %>%
                     dplyr::group_by(StnCode, CollectDateTime, StnType, Param) %>%
                     dplyr::summarize(Result = suppressWarnings(mean(as.numeric(Result)))) %>%
                     tidyr::pivot_wider(id_cols = c("StnCode", "CollectDateTime", "StnType"), names_from = Param, values_from = Result) %>%
                     as.data.frame())
  sampledata <- sampledata[with(sampledata, order(StnCode)), ]
  rm(merge1, merge2, merge3)
  rownames(sampledata) <- NULL

  # Download all standards, filter by user choice via popup window
  if(apply_standards == TRUE){
    print("Processing standards")
    # Extract eqstds and eqstdval tables from access database, merge together by StdId
    eqstds <- as.data.frame(DBI::dbReadTable(EQWin, "eqstds") %>%
                              subset(select=c("StdId", "StdCode", "StdName", "udf_StnGroup")))
    eqstdval <- as.data.frame(DBI::dbReadTable(EQWin, "eqstdval") %>%
                                subset(select=c("StdId", "ParamId", "MaxVal", "MinVal")))
    stdmerge1 <- merge(eqstds, eqstdval, by.x = "StdId", by.y = "StdId")
    stds <- merge(stdmerge1, params, by.x = "ParamId", by.y = "ParamId")

    stds <- merge(as.data.frame(DBI::dbReadTable(EQWin, "eqstds") %>%
                                  subset(select=c("StdId", "StdCode", "StdName", "udf_StnGroup"))),
                  as.data.frame(DBI::dbReadTable(EQWin, "eqstdval") %>%
                                  subset(select=c("StdId", "ParamId", "MaxVal", "MinVal"))),
                  by.x = "StdId", by.y = "StdId")

    # Filter stds by user choice, merge with parameters to associate standards with parameters by param code
    stds <- dplyr::filter(stds, stds$StdCode %in% utils::select.list(choices = sort(unique(stds$StdCode)),
                                                                     title = "Select Standards",
                                                                     graphics = TRUE,
                                                                     multiple = TRUE)) %>%
      merge(params, by.x ="ParamId", by.y = "ParamId")
    stds <- stds[, c("ParamCode", "ParamId", "StdCode", "StdName", "MaxVal", "MinVal","Units")] # Select relevant columns, reorder

    # Separate calculated from set standards
    std_set <- suppressWarnings(stds %>%
                                  dplyr::mutate_at("MaxVal", as.numeric) %>% # Convert MaxVal to numeric
                                  tidyr::drop_na("MaxVal"))
    std_calc_tmp <- stds %>%
      dplyr::filter(stringr::str_extract(MaxVal, "=*") == "=") # Extract standards with MaxVal value beginning with "=" (calculated standard)
    std_calc_tmp$MaxVal <- stringr::str_remove_all(std_calc_tmp$MaxVal, "=*") # Remove equal sign, leaving MaxVal with values matching values in eqcalcs access table

    # Process calculated standards
    std_calcs <- eq_std_calc(data = sampledata,
                             calcs = std_calc_tmp)


    # Combine set and calculated standards, format and order
    stnstd <- rbind(std_set, std_calcs)
    stnstd <- stnstd %>%
      dplyr::mutate_at(c("MaxVal", "MinVal"), as.numeric) %>%
      dplyr::mutate(Param = paste0(stnstd$ParamCode, " (", stnstd$Units, ")"))
    stnstd <- stnstd[order(stnstd$ParamId), ]
    rownames(stnstd) <- NULL
    stnstd <- tidyr::pivot_wider(stnstd, id_cols = c("StdName", "StdCode"), names_from = Param, values_from = MaxVal)

    # Match parameter columns between sampledata and std data frames
    params_data <- grep("\\(.*\\)", colnames(sampledata), value = TRUE) # Extract data parameters
    params_std <- grep("\\(.*\\)", colnames(stnstd), value = TRUE) # Extract std parameters
    head_data <- colnames(sampledata)[-c(grep("\\(*.\\)", colnames(sampledata)))]
    head_stds <- colnames(stnstd)[-c(grep("\\(*.\\)", colnames(stnstd)))]
    stnstd <- stnstd %>% # Remove std params that do not have exist in sampledata
      dplyr::select(c("StdName", "StdCode", params_std[params_std %in% params_data]))
    match <- data.frame(matrix(ncol = length(params_data), nrow = 0)) # Create data frame containing all parameters that exist in sampledata

    colnames(match) <- params_data
    stnstd <- plyr::rbind.fill(stnstd, match) # Fill in match with stnstd
    stnstd <- stnstd %>% #Convert columns containing all NA to numeric
      dplyr::mutate_if(is.logical, as.numeric)
    stnstd <- stnstd %>% # Arrange stnstd such that parameter order matches sampledata
      dplyr::select(all_of(c(head_stds, params_data)))

  }

  # Extract by-station data and station standards, put into by-location list then add list to master EQ_fetch output
  EQ_fetch_list <- list()
  for(i in unique(stns$StnCode)){
    list <- list()
    stndata <- sampledata %>%
      dplyr::filter(StnCode == i)
    list[["stndata"]] <- stndata
    if(apply_standards == TRUE){
      list[["stnstd"]] <- stnstd
    }
    EQ_fetch_list[[i]] <- list
  }

  return(EQ_fetch_list)
}
