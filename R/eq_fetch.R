#' Data retrieval from EQWin
#'
#' Fetches sample data from the WRB database and returns a list of data frames suitable for modification for plot generation and other comparisons. Connection to the EQWin database is made via function [AccessConnect()].
#'
#' @details Insert here what happens to values > DL, where the standards are taken from, etc
#'
#' @param EQcode Site code as it appears in EQWin eg. "LOB" or "KNO". Function only works for stations with  designated project code in brackets
#' @param stationIDs "all" for all stations (default) OR character vector of selected stations as they appear in the EQWin database WITHOUT the EQcode c("MW-01", "MW-02)
#' @param paramIDs "all" for all parameters (default) OR vector of selected parameters exactly as they appear in the EQWin database
#' @param dates "all" for all dates (default) OR character vector of length 2 of start and end date in format c("YYYY-MM-DD", "YYYY-MM-DD")
#' @param BD Treatment of values below detection limits (0 = Set to zero; 1 = Set to NA; 2 = Set to 0.5*(LOD); 3 = Set to sqrt(2)LOD). Above detection values are set to the upper limit of detection.
#' @param apply_standards TRUE or FALSE, include standards with data. Provides a pop-up list for selection.
#' @param path The path to the EQWin database.

#' @return A list with one sub-list per station, each one containing 2 data frames with sample data and calculated standards
#'
#' @export

eq_fetch <- function(EQcode,
                     stationIDs = "all",
                     paramIDs = "all",
                     dates = "all",
                     BD = 2,
                     apply_standards = TRUE,
                     path = eqwin_db_path()){
  
  # EQcode <- "KNO"
  # stationIDs = c("FORMO-01", "FORMO-CK", "FO-WQ-SE-1", "FO-WQ-SE2-1", "FO-WQ-ST1-1", "FO-WQ-ST1-2", "FO-WQ-ST1-3")
  # paramIDs = c("Fe-D", "Zn-D", "Cond-F")
  # dates = "all"
  # BD <- 2
  # apply_standards = TRUE
  # path = eqwin_db_path()
  
  # Set a few options
  old_scipen <- getOption("scipen")
  old_dplyr <- getOption("dplyr.summarise.inform")
  options(dplyr.summarise.inform = FALSE)
  options(scipen = 999)
  on.exit(options(scipen = old_scipen), add = TRUE)
  on.exit(options(dplyr.summarise.inform = old_dplyr), add = TRUE)
  
  # Connect to EQWin
  EQWin <- AccessConnect(path, silent = TRUE)
  on.exit(DBI::dbDisconnect(EQWin), add = TRUE)
  
  # Add project code to EQWin stations
  stationIDs <- paste0("(", EQcode, ")", stationIDs)
  
  # Download stations and filter to user input
  eqstns <- DBI::dbGetQuery(EQWin, "SELECT StnId, StnCode, StnName, StnType, udf_Stn_Status FROM eqstns WHERE StnCode")
  
  if (tolower(paste(stationIDs, collapse = "")) == "all") {
    stns <- eqstns %>%
      dplyr::filter(stringr::str_detect(.data$StnCode, paste0("^", "\\(", EQcode, "\\)")))
  } else if (all(stationIDs %in% eqstns$StnCode)) { # Optimal case, all requested stations exist in EQWin database
    stns <- eqstns %>%
      dplyr::filter(stringr::str_detect(.data$StnCode, paste0("^", "\\(", EQcode, "\\)"))) %>%
      dplyr::filter(.data$StnCode %in% stationIDs)
  } else { # If certain stations do not exist, remove and continue but print warning
    stns <- eqstns %>%
      dplyr::filter(stringr::str_detect(.data$StnCode, paste0("^", "\\(", EQcode, "\\)"))) %>%
      dplyr::filter(.data$StnCode %in% stationIDs[stationIDs %in% eqstns$StnCode])
    warning(paste0("The following stations do not match exactly what is in EQWin and were omitted: ",
                   paste(setdiff(stationIDs, stns$StnCode), collapse = ", "),". Check spelling and letter case"))
  }
  
  # Download all samples for specified stations, filter by user choice
  eqsampls <- DBI::dbGetQuery(EQWin, "SELECT SampleId, StnId, CollectDateTime FROM eqsampls") %>%
    dplyr::filter(.data$StnId %in% stns$StnId)
  
  tryCatch({
    if (tolower(paste(dates, collapse = "")) != "all") {
      samps <- eqsampls %>%
        dplyr::filter(dplyr::between(as.Date(.data$CollectDateTime), as.Date(dates[1]), as.Date(dates[2])))
    } else {
      samps <- eqsampls
    }},
    error = function(e) {
      message("Please check your date format")
    }
  )
  
  # Check to make sure all parameters are valid, keep all params for now until after std calculations
  eqparams <- as.data.frame(DBI::dbGetQuery(EQWin, "SELECT ParamId, ParamCode, Units FROM eqparams"))
  tryCatch({
    if (tolower(paste(paramIDs, collapse = "")) == "all") {
      params <- eqparams
      paramIDs <- params$ParamCode
    } else if (all(paramIDs %in% eqparams$ParamCode)) {
      params <- eqparams
    } else {
      params <- eqparams
      stop()}
  },
  error = function(e){
    message(paste("The following parameters do not match exactly what is in EQWin:", paste(setdiff(paramIDs, params$ParamCode), collapse = ", ")))
  })
  
  # Download all results
  print("Fetching sample results")
  results <- DBI::dbGetQuery(EQWin, paste0("SELECT ", paste0('SampleId', ", ", 'ParamId', ", ", 'Result'), " FROM eqdetail WHERE ParamID IN (", paste(eqparams$ParamId, collapse = ", "),") AND SampleId IN (", paste0(eqsampls$SampleId, collapse = ", "), ")"))
  
  # Deal with values below detection limits according to user choice
  if (BD == 0) {
    results$Result[grepl("<", results$Result)] <- 0
  } else if (BD == 1) {
    results$Result <- suppressWarnings(as.numeric(results$Result))
    results$Result <- as.numeric(results$Result)
  } else if (BD == 2) {
    isBD <- grepl("<", results$Result)
    results$Result[isBD] <- round(as.numeric(gsub("<(.*)", "\\1", results$Result[isBD]))/2, digits = 7)
    rm(isBD)
  } else if (BD == 3) {
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
                     dplyr::filter(.data$StnCode %in% stationIDs) %>%
                     dplyr::mutate(Param = paste0(merge3$ParamCode, " (", merge3$Units, ")")) %>%
                     dplyr::select(.data$StnCode, .data$CollectDateTime, .data$StnType, .data$Param, .data$Units, .data$Result) %>%
                     dplyr::group_by(.data$StnCode, .data$CollectDateTime, .data$StnType, .data$Param) %>%
                     dplyr::summarize(Result = suppressWarnings(mean(as.numeric(.data$Result)))) %>%
                     tidyr::pivot_wider(id_cols = c("StnCode", "CollectDateTime", "StnType"), names_from = .data$Param, values_from = .data$Result) %>%
                     as.data.frame())
  sampledata <- sampledata[with(sampledata, order(StnCode)), ]
  rm(merge1, merge2, merge3)
  rownames(sampledata) <- NULL
  
  # Download all standards, filter by user choice via popup window
  if (apply_standards) {
    print("Processing standards")
    # Extract eqstds and eqstdval tables from access database, merge together by StdId
    eqstds <- as.data.frame(DBI::dbReadTable(EQWin, "eqstds") %>%
                              subset(select = c("StdId", "StdCode", "StdName", "udf_StnGroup")))
    eqstdval <- as.data.frame(DBI::dbReadTable(EQWin, "eqstdval") %>%
                                subset(select = c("StdId", "ParamId", "MaxVal", "MinVal")))
    stdmerge1 <- merge(eqstds, eqstdval, by.x = "StdId", by.y = "StdId")
    stds <- merge(stdmerge1, params, by.x = "ParamId", by.y = "ParamId")
    
    stds <- merge(as.data.frame(DBI::dbReadTable(EQWin, "eqstds") %>%
                                  subset(select = c("StdId", "StdCode", "StdName", "udf_StnGroup"))),
                  as.data.frame(DBI::dbReadTable(EQWin, "eqstdval") %>%
                                  subset(select = c("StdId", "ParamId", "MaxVal", "MinVal"))),
                  by.x = "StdId", by.y = "StdId")
    
    # Filter stds by user choice, merge with parameters to associate standards with parameters by param code
    stds <- dplyr::filter(stds, stds$StdCode %in% utils::select.list(choices = sort(unique(stds$StdCode)),
                                                                     title = "Select Standards",
                                                                     graphics = TRUE,
                                                                     multiple = TRUE)) %>%
      merge(params, by.x = "ParamId", by.y = "ParamId")
    stds <- stds[, c("ParamCode", "ParamId", "StdCode", "StdName", "MaxVal", "MinVal","Units")] # Select relevant columns, reorder
    
    # Separate stations and process calculated standards for each
    stdlist <- list()
    for (i in unique(sampledata$StnCode)) {
      sampledatafilt <- sampledata %>%
        dplyr::filter(.data$StnCode == i)
      
      # Separate calculated from set standards
      std_set <- suppressWarnings(stds %>%
                                    dplyr::mutate_at("MaxVal", as.numeric) %>% # Convert MaxVal to numeric
                                    tidyr::drop_na("MaxVal"))
      std_calc_tmp <- stds %>%
        dplyr::filter(stringr::str_extract(.data$MaxVal, "=*") == "=") # Extract standards with MaxVal value beginning with "=" (calculated standard)
      std_calc_tmp$MaxVal <- stringr::str_remove_all(std_calc_tmp$MaxVal, "=*") # Remove equal sign, leaving MaxVal with values matching values in eqcalcs access table
      
      # Process calculated standards
      std_calcs <- eq_std_calc(sampledata = sampledatafilt,
                               calcs = std_calc_tmp)
      
      # Combine set and calculated standards, format and order
      stnstd <- rbind(std_set, std_calcs)
      stnstd <- stnstd %>%
        dplyr::mutate_at(c("MaxVal", "MinVal"), as.numeric) %>%
        dplyr::mutate(Param = paste0(stnstd$ParamCode, " (", stnstd$Units, ")"))
      stnstd <- stnstd[order(stnstd$ParamId), ]
      rownames(stnstd) <- NULL
      stnstd <- tidyr::pivot_wider(stnstd, id_cols = c("StdName", "StdCode"), names_from = .data$Param, values_from = .data$MaxVal)
      
      # Match parameter columns between sampledata and std data frames
      params_data <- grep("\\(.*\\)", colnames(sampledata), value = TRUE) # Extract data parameters
      params_std <- grep("\\(.*\\)", colnames(stnstd), value = TRUE) # Extract std parameters
      head_data <- colnames(sampledata)[-c(grep("\\(*.\\)", colnames(sampledata)))]
      head_stds <- colnames(stnstd)[-c(grep("\\(*.\\)", colnames(stnstd)))]
      stnstd <- stnstd %>% # Remove std params that do not have exist in sampledata
        dplyr::select(c("StdName", "StdCode", params_std[params_std %in% params_data]))
      match <- data.frame(matrix(ncol = length(params_data), nrow = 0)) # Create data frame containing all parameters that exist in sampledata
      
      colnames(match) <- params_data
      # stnstd <- plyr::rbind.fill(stnstd, match) # Fill in match with stnstd
      stnstd <- dplyr::bind_rows(stnstd, match) # Fill in match with stnstd
      stnstd <- stnstd %>% #Convert columns containing all NA to numeric
        dplyr::mutate_if(is.logical, as.numeric)
      stnstd <- stnstd %>% # Arrange stnstd such that parameter order matches sampledata
        dplyr::select(dplyr::all_of(c(head_stds, params_data)))
      stdlist[[i]] <- stnstd
      
    }
  }
  
  # Extract by-station data and station standards filtered to desired output parameters, put into by-location list then add list to master EQ_fetch output
  EQ_fetch_list <- list()
  for (i in unique(sampledata$StnCode)) {
    list <- list()
    stndata <- sampledata %>%
      dplyr::filter(.data$StnCode == i) %>%
      dplyr::select(c("StnCode", "CollectDateTime", "StnType", which(as.vector(sapply(names(sampledata), function(x) sub(" \\(.*", "", x))) %in% paramIDs)))
    list[["stndata"]] <- stndata
    if (apply_standards) {
      list[["stnstd"]] <- stdlist[[i]] %>%
        dplyr::select(c("StdName", "StdCode", which(as.vector(sapply(names(stnstd), function(x) sub(" \\(.*", "", x))) %in% paramIDs)))
    }
    EQ_fetch_list[[i]] <- list
  }
  
  return(EQ_fetch_list)
}
