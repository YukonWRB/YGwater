#' Extract EQWin data to an Excel report or .csv
#'
#' @description
#' 
#' This function extracts data from the EQWin database and outputs it to an Excel workbook. The function can extract data from a specified date range, station(s), and parameter(s). The function can also output standard values as well as station-specific standards. .
#' 
#' @details
#' Connection to EQWin is made using function [AccessConnect()].
#' 
#' @param start The date to fetch data from, passed as a Date, POSIXct, or character vector of form 'yyyy-mm-dd HH:MM'. Dates and character vectors are converted to POSIXct with timezone 'MST'.
#' @param end The end date to fetch data up to, passed as a Date, POSIXct, or character vector of form 'yyyy-mm-dd HH:MM'. Dates and character vectors are converted to POSIXct with timezone 'MST'. Default is the current date.
#' @param stations A vector of station names as listed in the EQWiN eqstns table, column StnCode. Leave NULL to use stnGrp instead.
#' @param stnGrp A station group as listed in the EWQin eqgroups table, column groupname. Leave NULL to use stations instead.
#' @param parameters A vector of parameter names as listed in the EQWin eqparams table, column ParamCode. Leave NULL to use paramGrp instead.
#' @param paramGrp A parameter group as listed in the EQWin eqgroups table, column groupname. Leave NULL to use parameters instead.
#' @param format 'long' or 'wide' to output the data in a long or wide format. Default is 'wide', in which locations are separated in workbook tabs, sample datetimes in columns, and parameters in rows. Long format is a single table with columns for 'location', 'parameter', and 'value'. Long format is useful for programmatic analysis and plotting, while wide format is useful for looking at the data and making plots in Excel.
#' @param DL1 Behavior with respect to values listed as < the detection limit. Default is NULL which leaves the values as is; 'negative' replaces '<' with '-' and converts to numeric; 'half' replaces with 1/2 the detection limit and converts to numeric; 'NA' replaces the value with 'NA' and converts the rest to numeric.
#' @param DL2 Behavior with respect to values listed as > the detection limit. Default is NULL which leaves the values as is; 'convert' simple removes the '<' and converts to numeric; 'NA' replaces the value with 'NA' and converts the rest to numeric.
#' @param stds A vector of standard names as listed in the EQWin eqstds table. Leave NULL to exclude standards. Standards will be output to an Excel workbook, one labelled tab per standard.
#' @param stnStds TRUE/FALSE to include/exclude the station-specific standards listed in the eqstns table, column StnStd. Station standards will be added to the 'standards' workbook as per parameter `stds`.
#' @param save_path The path to save the Excel file(s). Default is "choose" to allow user to select a folder interactively.
#' @param dbPath The path to the EQWin database. Default is "X:/EQWin/WR/DB/Water Resources.mdb".
#' 
#' @return An Excel workbook containing the requested station data, and optionally a workbook containing the standards.
#' @export
#'
#' @examples
#' \dontrun{
#' # Using station and parameter groups
#' EQWinData(start = "2024-07-01 00:00", end = Sys.Date(), stnGrp = "QZ Eagle Gold HLF",
#' paramGrp = "EG-HLF-failure", format = 'wide', stds = c("CCME_LT", "CCME_ST"), stnStds = TRUE)
#' 
#' With specific stations and parameters
#' EQWinData(start = "2024-01-01 00:00", end = Sys.Date(), stations = c("(EG)W23"),
#' parameters = c("pH-F"), format = 'wide', stds = c("CCME_LT", "CCME_ST"), stnStds = TRUE)
#' }

EQWinData <- function(start, end = Sys.Date() + 1, stations = NULL, stnGrp = NULL, parameters = NULL, paramGrp = NULL, format = 'wide', DL1 = NULL, DL2 = NULL, stds = NULL, stnStds = TRUE, save_path = "choose", dbPath = "X:/EQWin/WR/DB/Water Resources.mdb") {
  
  # initial checks, connection, and validations #######################################################################################
  if (is.null(stations) & is.null(stnGrp)) stop("You must specify either stations or stnGrp")
  if (!is.null(stations) & !is.null(stnGrp)) stop("You must specify either stations or stnGrp, not both")
  if (is.null(parameters) & is.null(paramGrp)) stop("You must specify either parameters or paramGrp")
  if (!is.null(parameters) & !is.null(paramGrp)) stop("You must specify either parameters or paramGrp, not both")
  
  format <- tolower(format)
  if (!format %in% c("long", "wide")) stop("format must be either 'long' or 'wide'")
  
  if (!is.null(DL1)) {
    if (!DL1 %in% c("negative", "half", "NA")) stop("DL1 must be NULL, 'negative', 'half', or 'NA'")
  }
  if (!is.null(DL2)) {
    if (!DL2 %in% c("convert", "NA")) stop("DL2 must be NULL, 'convert', or 'NA'")
  }
  
  if (save_path == "choose") {
    if (!interactive()) {
      stop("You must specify a save path when running in non-interactive mode.")
    }
    message("Select the path to the folder where you want this report saved.")
    save_path <- rstudioapi::selectDirectory(caption = "Select Save Folder", path = file.path(Sys.getenv("USERPROFILE"),"Desktop"))
  }
  
  if (inherits(start, "character")) {
    start <- as.Date(start)
  }
  if (inherits(start, "Date")) {
    start <- as.POSIXct(start, tz = "MST")
  }
  if (inherits(end, "character")) {
    end <- as.Date(end)
  }
  if (inherits(end, "Date")) {
    end <- as.POSIXct(end, tz = "MST")
  }
  
  # Connect to EQWin
  EQWin <- AccessConnect(dbPath, silent = TRUE)
  on.exit(DBI::dbDisconnect(EQWin), add = TRUE)
  
  # Fetch the station and/or parameter list if necessary (stnGrp or paramGrp was specified)
  if (!is.null(stnGrp)) {
    # Check if the group actually exists
    grp_count <- DBI::dbGetQuery(EQWin, paste0("SELECT COUNT(*) FROM eqgroups WHERE groupname = '", stnGrp, "' AND dbtablename = 'eqstns'"))[1,1]
    if (grp_count == 0) {
      stop("The station group '", stnGrp, "' does not exist in the EQWin database")
    } else if (grp_count > 1) {
      stop("There are multiple station groups with the name '", stnGrp, "' in the EQWin database")
    } # otherwise proceed to fetch the stations
    
    StnIds <- DBI::dbGetQuery(EQWin, paste0("SELECT groupitems FROM eqgroups WHERE groupname = '", stnGrp, "' AND dbtablename = 'eqstns'"))$groupitems
    StnIds <- strsplit(StnIds, ",")[[1]]
    if (length(StnIds) == 0) {
      stop("No stations found in the station group '", stnGrp, "'")
    }
  }
  if (!is.null(paramGrp)) {
    # Check if the group actually exists
    grp_count <- DBI::dbGetQuery(EQWin, paste0("SELECT COUNT(*) FROM eqgroups WHERE groupname = '", paramGrp, "' AND dbtablename = 'eqparams'"))[1,1]
    if (grp_count == 0) {
      stop("The parameter group '", paramGrp, "' does not exist in the EQWin database")
    } else if (grp_count > 1) {
      stop("There are multiple parameter groups with the name '", paramGrp, "' in the EQWin database")
    } # otherwise proceed to fetch the parameters
    
    ParamIds <- DBI::dbGetQuery(EQWin, paste0("SELECT groupitems FROM eqgroups WHERE groupname = '", paramGrp, "' AND dbtablename = 'eqparams'"))$groupitems
    ParamIds <- strsplit(ParamIds, ",")[[1]]
    if (length(ParamIds) == 0) {
      stop("No parameters found in the parameter group '", paramGrp, "'")
    }
  }
  
  # Validate existence of parameters and/or stations
  if (!is.null(stations)) {
    StnIds <- DBI::dbGetQuery(EQWin, paste0("SELECT StnId, StnCode FROM eqstns WHERE StnCode IN ('", paste0(stations, collapse = "', '"), "')"))
    if (nrow(StnIds) == 0) {
      stop("No stations found in the EQWin database with the names '", paste0(stations, collapse = "', '"), "'")
    }
    if (nrow(StnIds) < length(stations)) {
      # Find the missing stations and tell the user which ones are missing
      missing <- setdiff(stations, StnIds$StnCode)
      warning("The following stations were not found in the EQWin database: ", paste0(missing, collapse = ", "))
    }
    StnIds <- StnIds$StnId
  }
  
  if (!is.null(parameters)) {
    ParamIds <- DBI::dbGetQuery(EQWin, paste0("SELECT ParamId, ParamCode FROM eqparams WHERE ParamCode IN ('", paste0(parameters, collapse = "', '"), "')"))
    if (nrow(ParamIds) == 0) {
      stop("No parameters found in the EQWin database with the names '", paste0(parameters, collapse = "', '"), "'")
    }
    if (nrow(ParamIds) < length(parameters)) {
      # Find the missing parameters and tell the user which ones are missing
      missing <- setdiff(parameters, ParamIds$ParamCode)
      warning("The following parameters were not found in the EQWin database: ", paste0(missing, collapse = ", "))
    }
    ParamIds <- ParamIds$ParamId
  }
  
  # Validate existence of standards
  if (!is.null(stds)) {
    standards <- DBI::dbGetQuery(EQWin, paste0("SELECT StdId, StdCode, StdFlag, StdDesc FROM eqstds WHERE StdCode IN ('", paste0(stds, collapse = "', '"), "')"))
    if (nrow(standards) == 0) {
      stop("No standards found in the EQWin database with the names '", paste0(stds, collapse = "', '"), "'")
    }
    if (nrow(standards) < length(stds)) {
      # Find the missing standards and tell the user which ones are missing
      missing <- setdiff(stds, standards$StdCode)
      warning("The following standards were not found in the EQWin database: ", paste0(missing, collapse = ", "))
    }
  }
  
  # Fetch the data #######################################################################################
  write_time <- format(Sys.time(), "%Y%m%d %H%M")
  sampleIds <- DBI::dbGetQuery(EQWin, paste0("SELECT eqsampls.StnId, eqsampls.SampleId, eqsampls.CollectDateTime, eqcodes.CodeDesc FROM eqsampls INNER JOIN eqcodes ON eqsampls.SampleClass = eqcodes.CodeValue WHERE eqcodes.CodeField = 'eqsampls.SampleClass' AND eqsampls.StnId IN (", paste0(StnIds, collapse = ", "), ") AND eqsampls.CollectDateTime > #", as.character(start), "# AND eqsampls.CollectDateTime < #", as.character(end), "#;"))
  
  if (nrow(sampleIds) == 0) {
    stop("No samples found for the date range and stations specified.")
  }
  # Extract the first word from CodeDesc and format
  sampleIds$CodeDesc <- sapply(strsplit(sampleIds$CodeDesc, " "), `[`, 1)
  sampleIds$CodeDesc <- gsub(":", "", sampleIds$CodeDesc)
  sampleIds$CodeDesc <- gsub(",", "", sampleIds$CodeDesc)
  sampleIds$CodeDesc <- paste0("(", sampleIds$CodeDesc, ")")
  
  results <- DBI::dbGetQuery(EQWin, paste0("SELECT eqdetail.SampleId, eqdetail.ParamId, eqdetail.Result, eqparams.ParamCode, eqparams.ParamName FROM eqdetail INNER JOIN eqparams ON eqdetail.ParamId = eqparams.ParamId WHERE eqdetail.SampleId IN (", paste0(sampleIds$SampleId, collapse = ", "), ") AND eqdetail.ParamId IN (", paste0(ParamIds, collapse = ", "), ");"))
  params <- DBI::dbGetQuery(EQWin, paste0("SELECT ParamId, ParamName FROM eqparams;"))
  
  samps <- sampleIds[sampleIds$SampleId %in% results$SampleId, ]
  locations <- DBI::dbGetQuery(EQWin, paste0("SELECT StnId, StnCode, StnName, StnDesc FROM eqstns WHERE StnId IN (", paste0(samps$StnId, collapse = ", "), ");"))
  samps_locs <- merge(locations, samps)
  
  
  if (format == 'wide') {
    datalist <- list()
    for (i in unique(samps_locs$StnId)) { # Making one tab per station
      location <- samps_locs[samps_locs$StnId == i, ]
      ids <- sampleIds[sampleIds$StnId == i, ]
      datastn <- data.frame()
      for (j in 1:nrow(ids)) { # This will add one column per sample ID
        id <- ids$SampleId[j]
        rawdata <- results[results$SampleId == id, c("Result", "ParamName", "SampleId")]
        if (nrow(rawdata) == 0) next
        rawdata <- merge(rawdata, ids[, c("CollectDateTime", "CodeDesc", "SampleId")])
        data <- data.frame(Parameter = unique(rawdata$ParamName), tmp = NA)
        names(data) <- c("Parameter", paste0(as.character(rawdata$CollectDateTime[1]), " ", rawdata$CodeDesc[1]))
        for (k in 1:nrow(data)) {
          data[k, 2] <- rawdata$Result[rawdata$ParamName == data$Parameter[k]]
        }
        if (!is.null(DL1)) {
          if (DL1 == "negative") {
            data <- sapply(data, function(x) {
              if (grepl("^<", x)) {
                return(as.numeric(paste0("-", gsub("^<", "", x))))
              } else {
                return(as.numeric(x))
              }
            })
          } else if (DL1 == "half") {
            data <- sapply(data, function(x) {
              if (grepl("^<", x)) {
                return(as.numeric(gsub("^<", "", x)) / 2)
              } else {
                return(as.numeric(x))
              }
            })
          } else if (DL1 == "NA") {
            data <- sapply(data, function(x) {
              if (grepl("^<", x)) {
                return(NA)
              } else {
                return(as.numeric(x))
              }
            })
          }
        }
        if (!is.null(DL2)) {
          if (DL2 == "convert") {
            data <- sapply(data, function(x) {
              if (grepl("^>", x)) {
                return(as.numeric(gsub("^>", "", x)))
              } else {
                return(as.numeric(x))
              }
            })
          } else if (DL2 == "NA") {
            data <- sapply(data, function(x) {
              if (grepl("^>", x)) {
                return(NA)
              } else {
                return(as.numeric(x))
              }
            })
          }
        }
        datastn <- merge(datastn, data, all = TRUE)
      }
      datalist[[locations[locations$StnId == i, "StnName"]]] <- datastn
    }
    openxlsx::write.xlsx(datalist, paste0(save_path, "/EQWinData_wide ", write_time, ".xlsx"))
    message("Data workbook saved to ", paste0(save_path, "/EQWinData_wide ", write_time, ".xlsx"))
    
  } else if (format == 'long') {
    datalist <- list()
    datalist[["data"]] <- merge(results[, c("SampleId", "ParamId", "Result")], samps_locs[, c("SampleId", "StnId", "CollectDateTime")])
    if (!is.null(DL1)) {
      if (DL1 == "negative") {
        datalist[["data"]]$Result <- sapply(datalist[["data"]]$Result, function(x) {
          if (grepl("^<", x)) {
            return(as.numeric(paste0("-", gsub("^<", "", x))))
          } else {
            return(as.numeric(x))
          }
        })
      } else if (DL1 == "half") {
        datalist[["data"]]$Result <- sapply(datalist[["data"]]$Result, function(x) {
          if (grepl("^<", x)) {
            return(as.numeric(gsub("^<", "", x)) / 2)
          } else {
            return(as.numeric(x))
          }
        })
      } else if (DL1 == "NA") {
        datalist[["data"]]$Result <- sapply(datalist[["data"]]$Result, function(x) {
          if (grepl("^<", x)) {
            return(NA)
          } else {
            return(as.numeric(x))
          }
        })
      }
    }
    if (!is.null(DL2)) {
      if (DL2 == "convert") {
        datalist[["data"]]$Result <- sapply(datalist[["data"]]$Result, function(x) {
          if (grepl("^>", x)) {
            return(as.numeric(gsub("^>", "", x)))
          } else {
            return(as.numeric(x))
          }
        })
      } else if (DL2 == "NA") {
        datalist[["data"]]$Result <- sapply(datalist[["data"]]$Result, function(x) {
          if (grepl("^>", x)) {
            return(NA)
          } else {
            return(as.numeric(x))
          }
        })
      }
    }
    datalist[["locations"]] <- locations[locations$StnId %in% datalist[["data"]]$StnId, c("StnId", "StnName", "StnDesc")]
    datalist[["parameters"]] <- unique(results[, c("ParamId", "ParamName")])
    openxlsx::write.xlsx(datalist, paste0(save_path, "/EQWinData_long", write_time, ".xlsx"), overwrite = TRUE)
    message("Data workbook saved to ", paste0(save_path, "/EQWinData_long ", write_time, ".xlsx"))
  }
  
  
  # Fetch the standards #######################################################################################
  # Get standards data for the parameters retained
  if (!is.null(stds)) {
    stdVals <- DBI::dbGetQuery(EQWin, paste0("SELECT StdId, ParamId, MaxVal, MinVal FROM eqstdval WHERE StdId IN (", paste(standards$StdId, collapse = ", "), ");"))
    stdVals <- merge(stdVals, params, all.x = TRUE)

    standards_list <- list()
    for (i in 1:nrow(standards)) {
      standards_list[[standards$StdCode[i]]] <- stdVals[stdVals$StdId == standards$StdId[i], c("ParamName", "ParamId", "MinVal", "MaxVal")]
    }
    openxlsx::write.xlsx(standards_list, paste0(save_path, "/EQWinStandards ", write_time, ".xlsx"))
    message("Standards workbook saved to ", paste0(save_path, "/EQWinStandards ", write_time, ".xlsx"))
  }
  
  
  # Now find station-specific standards, which will be used to apply conditional formatting and notes when the workbook is made
  if (stnStds) {
    tmp <- DBI::dbGetQuery(EQWin, paste0("SELECT StnId, StnStd AS StdCode FROM eqstns WHERE StnId IN (", paste0(unique(samps_locs$StnId), collapse = ", "), ") AND StnStd IS NOT NULL;"))
    if (nrow(tmp) == 0) {
      stnStds <- FALSE
    } else {
      tmp <- merge(tmp, DBI::dbGetQuery(EQWin, paste0("SELECT StdCode, StdDesc, StdId FROM eqstds WHERE StdCode IN ('", paste0(tmp$StdCode, collapse = "', '"), "')")))
      
      stn_standards_list <- list()
      for (i in unique(tmp$StdCode)) {
        applies <- tmp[tmp$StdCode == i, "StnId", drop = FALSE]
        applies <- merge(applies, locations[, c("StnId", "StnName")])$StnName  # This is used to make a note at the top of the workbook sheet
        
        standard <- DBI::dbGetQuery(EQWin, paste0("SELECT ParamId, MaxVal, MinVal, StdValNote FROM eqstdval WHERE StdId = ", unique(tmp[tmp$StdCode == i, "StdId"]), ";"))
        standard <- merge(params, standard, all.y = TRUE)
        standard$ParamId <- NULL
        standard$`Applies to locations` <- paste0(applies, collapse = ", ")
        stn_standards_list[[i]] <- standard
      }
      openxlsx::write.xlsx(stn_standards_list, paste0(save_path, "/EQWinStnStandards ", write_time, ".xlsx"))
      message("Stations standards workbook saved to ", paste0(save_path, "/EQWinStnStandards ", write_time, ".xlsx"))
    }
  }
}
