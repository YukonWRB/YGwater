#' Create an Excel workbook report of water quality data from the EQWin database
#' 
#' @description
#' 
#' Generates a report of water quality data for a given date, set of stations (or EQWin specified station group), and parameters (or specified parameter group). Guidelines passed to argument 'stds' are included as columns in the table, while station-specific guidelines are listed in a note in the cell containing the station name. 
#' 
#' Exceedances are noted using conditional formatting, with a note added to explain which standard(s) are exceeded.
#' 
#' @details
#' Connection to EQWin is made using function [AccessConnect()].
#' 
#' @param date The date for which to fetch results as a Date object or character vector that can be coerced to a date.
#' @param stations A vector of station names as listed in the EQWiN eqstns table, column StnCode. Leave NULL to use stnGrp instead.
#' @param stnGrp A station group as listed in the EWQin eqgroups table, column groupname. Leave NULL to use stations instead.
#' @param parameters A vector of parameter names as listed in the EQWin eqparams table, column ParamCode. Leave NULL to use paramGrp instead.
#' @param paramGrp A parameter group as listed in the EQWin eqgroups table, column groupname. Leave NULL to use parameters instead.
#' @param stds A vector of standard names as listed in the EQWin eqstds table. Leave NULL to exclude standards. As these can apply to all stations standard values will be listed in a column to the left of the results.
#' @param stnStds TRUE/FALSE to include/exclude the station-specific standards listed in the eqstns table, column StnStd. As these are station-specific, the standard values will be listed in a comment linked to the station name in the table header.
#' @param date_approx An optional maximum number of days in the past or future to fetch results for stations where data is unavailable for the exact date. If a station is found to not have any samples on the given 'date', the function will look for samples on the date + 1 day, - 1 day, + 2 days, etc. up to 'date_approx' days in the past and future and stop searching for a station once a sample is found. Default is 0.
#' @param save_path The path to save the Excel file. Default is "choose" to allow user to select a folder interactively.
#' @param dbPath The path to the EQWin database. Default is "X:/EQWin/WR/DB/Water Resources.mdb".
#' 
#' @return An Excel workbook saved where requested.
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' # These examples require access to the G drive to work and must write to a file. They are 
#' # provided for demonstration purposes only and can't be run using the 'Run examples' button above.
#' # Copy/paste them into your R console to run them if needed.
#' 
#' # Generate a report for July 1, 2024 using a station group, parameter group, 
#' # both CCME standards, and look for data within +- 1 day of July 1
#' EQWinReport("2024-07-25", stnGrp = "QZ Eagle Gold HLF", paramGrp = "EG-HLF-failure",
#' stds = c("CCME_ST", "CCME_LT"), date_approx = 1)
#' 
#' 
#' # Generate a report for a single location and single parameter, no standards.
#' EQWinReport("1991-02-12", stations = c("(CM)CM-u/s"), parameters = c("pH-F"), stnStds = FALSE)
#' 
#' }

EQWinReport <- function(date, stations = NULL, stnGrp = NULL, parameters = NULL, paramGrp = NULL, stds = NULL, stnStds = TRUE, date_approx = 0, save_path = "choose", dbPath = "X:/EQWin/WR/DB/Water Resources.mdb") {
  
  # initial checks, connection, and validations #######################################################################################
  if (is.null(stations) & is.null(stnGrp)) stop("You must specify either stations or stnGrp")
  if (!is.null(stations) & !is.null(stnGrp)) stop("You must specify either stations or stnGrp, not both")
  if (is.null(parameters) & is.null(paramGrp)) stop("You must specify either parameters or paramGrp")
  if (!is.null(parameters) & !is.null(paramGrp)) stop("You must specify either parameters or paramGrp, not both")
  
  if (length(date) > 1) stop("date must be a single date")
  if (inherits(date, "Date")) {
    date <- as.character(date)
  } else if (!inherits(date, "character")) {
    stop("date must be a Date object or a character vector that can be coerced to a date")
  }
  
  # Date is at this point a character vector of 1 and must be converted to a Date object
  tryCatch({
    date <- as.Date(date)
  }, error = function(e) {
    stop("Failed to convert 'date' parameter to a Date object. Please provide a valid date in the format 'YYYY-MM-DD'")
  })
  
  if (!inherits(date_approx, "numeric")) stop("date_approx must be a numeric value")
  
  
  if (save_path == "choose") {
    if (!interactive()) {
      stop("You must specify a save path when running in non-interactive mode.")
    }
    message("Select the path to the folder where you want this report saved.")
    save_path <- rstudioapi::selectDirectory(caption = "Select Save Folder", path = file.path(Sys.getenv("USERPROFILE"),"Desktop"))
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
  
  
  # Fetch the data and form tables ###############################################################################################
  # Fetch the sample data for the date in question, plus or minus the date_approx if specified if there is no data for the exact date
  # Get some data and merge dfs
  sampleIds <- DBI::dbGetQuery(EQWin, paste0("SELECT StnId, SampleId, CollectDateTime FROM eqsampls WHERE StnId IN (", paste0(StnIds, collapse = ", "), ") AND DateValue(CollectDateTime) = '", date, "' AND SampleClass <> 'D';"))
  
  if (date_approx > 0 ) {
    # Find which element of StnIds is not in sampleIds$StnId
    missing <- setdiff(StnIds, sampleIds$StnId)
    if (length(missing) > 0) {
      # Look for samples one day at a time out from the 'date' parameter. Start with one day in future, then one day in past, then two days in future, two days in past, etc.
      extra_sampleIds <- data.frame()
      for (i in 1:date_approx) {
        extra <- DBI::dbGetQuery(EQWin, paste0("SELECT StnId, SampleId, CollectDateTime FROM eqsampls WHERE StnId IN (", paste0(missing, collapse = ", "), ") AND DateValue(CollectDateTime) = '", date + i, "';"))
        if (nrow(extra) > 0) {
          extra_sampleIds <- rbind(extra_sampleIds, extra)
          # remove the stations that have been found from the missing list
          missing <- setdiff(missing, extra$StnId)
        } else { # Now go to i days in the past
          extra <- DBI::dbGetQuery(EQWin, paste0("SELECT StnId, SampleId, CollectDateTime FROM eqsampls WHERE StnId IN (", paste0(missing, collapse = ", "), ") AND DateValue(CollectDateTime) = '", date - i, "';"))
          if (nrow(extra) > 0) {
            extra_sampleIds <- rbind(extra_sampleIds, extra)
            # remove the stations that have been found from the missing list
            missing <- setdiff(missing, extra$StnId)
          }
        }
      }
      if (nrow(extra_sampleIds) > 0) {
        sampleIds <- rbind(sampleIds, extra_sampleIds)
      }
    }
  }
  
  if (nrow(sampleIds) == 0) {
    stop("No samples found for the date '", date, "', or within ", date_approx, " days of that date.")
  }
  
  results <- DBI::dbGetQuery(EQWin, paste0("SELECT eqdetail.SampleId, eqdetail.ParamId, eqdetail.Result, eqparams.ParamCode, eqparams.ParamName FROM eqdetail INNER JOIN eqparams ON eqdetail.ParamId = eqparams.ParamId WHERE eqdetail.SampleId IN (", paste0(sampleIds$SampleId, collapse = ", "), ") AND eqdetail.ParamId IN (", paste0(ParamIds, collapse = ", "), ");"))
  params <- DBI::dbGetQuery(EQWin, paste0("SELECT ParamId, ParamCode, ParamName FROM eqparams WHERE ParamId IN (", paste0(results$ParamId, collapse = ", "), ");"))
  
  samps <- sampleIds[sampleIds$SampleId %in% results$SampleId, ]
  locations <- DBI::dbGetQuery(EQWin, paste0("SELECT StnId, StnCode, StnName, StnDesc FROM eqstns WHERE StnId IN (", paste0(samps$StnId, collapse = ", "), ");"))
  samps_locs <- merge(locations, samps)
  
  # Some locations and datetimes are duplicated with different sampleIds. Appeld a (1), (2) to these so that they create new columns.
  samps_locs$colnames <- c(paste0(samps_locs$StnName, " (", samps_locs$CollectDateTime, ")"))
  for (i in unique(samps_locs$colnames)) {
    iter <- which(samps_locs$colnames == i)
    if (length(iter) > 1) {
      count <- 2
      for (j in iter[-1]) {
        samps_locs$colnames[j] <- paste0(samps_locs$colnames[j], " (", count, ")")
        count <- count + 1
      }
    }
  }
  
  # Create the tables, first with only parameter names. the values_table and std_table will be filled in later and merged to final_table so that the standards show up first.
  final_table <- data.frame(Parameter = params$ParamName)
  values_table <- data.frame(Parameter = params$ParamName)
  # Add in columns for each location (date) and fill with results
  for (i in 1:nrow(samps_locs)) {
    tmp <- data.frame(tmp = results[results$SampleId == samps_locs$SampleId[i], "Result"], Parameter = results[results$SampleId == samps_locs$SampleId[i], "ParamName"], check.names = FALSE)
    names(tmp) <- c(samps_locs$colnames[i], "Parameter")
    values_table <- merge(values_table, tmp, all.x = TRUE)
  }
  
  # Get standards data for the parameters retained
  if (!is.null(stds)) {
    stdVals <- DBI::dbGetQuery(EQWin, paste0("SELECT StdId, ParamId, MaxVal, MinVal FROM eqstdval WHERE StdId IN (", paste(standards$StdId, collapse = ", "), ") AND ParamId IN (", paste0(params$ParamId, collapse = ", "), ");"))
    stdVals <- merge(stdVals, params)
    standard_remove_idx <- numeric(0)
    for (i in 1:nrow(standards)) {
      stdivals <- stdVals[stdVals$StdId == standards$StdId[i], ]
      
      if (nrow(stdivals) == 0)  {
        standard_remove_idx <- c(standard_remove_idx, i)  # The offending rows are removed after this for loop
        next  # This means that the standard does not apply to any of the parameters in the report
      }
      
      stdivals$display_min <- NA
      stdivals$display_max <- NA
      # Some MaxVal and MinVal fields are labelled as =xxxx. These refer to calculations stored in table eqcalcs. Since the standard varies by sample, the column for the standard value will simply say 'calc'
      for (j in 1:nrow(stdivals)) {
        if (grepl("=", stdivals$MaxVal[j])) {
          stdivals$display_max[j] <- "calc"
        } else {
          stdivals$display_max[j] <- stdivals$MaxVal[j]
        }
        if (grepl("=", stdivals$MinVal[j])) {
          stdivals$display_min[j] <- "calc"
        } else {
          stdivals$display_min[j] <- stdivals$MinVal[j]
        }
      }
      stdivals <- merge(stdivals, params)
      
      tbl <- data.frame(Parameter = stdivals$ParamName, tmp = NA)
      names(tbl) <- c("Parameter", standards$StdCode[i])
      for (j in 1:nrow(stdivals)) {
        min <- stdivals[j, "display_min"]
        max <- stdivals[j, "display_max"]
        string <- paste0(if (is.na(min)) "" else paste0("Min: ", min, " - "), if (is.na(max)) "" else paste0("Max: ", max))
        tbl[j, standards$StdCode[i]] <- if (nchar(string) > 0) string else string
      }
      if (i == 1) {
        std_table <- tbl
      } else {
        std_table <- merge(std_table, tbl, all.x = TRUE)
      }
      
    }
    if (length(standard_remove_idx) > 0) {
      standards <- standards[-standard_remove_idx, ]
    }
    final_table <- merge(final_table, std_table, all.x = TRUE)
  } else {
    standards <- data.frame()
  }
  final_table <- merge(final_table, values_table, all.x = TRUE)
  
  
  # Now find station-specific standards, which will be used to apply conditional formatting and notes when the workbook is made
  if (stnStds) {
    tmp <- DBI::dbGetQuery(EQWin, paste0("SELECT StnId, StnStd AS StdCode FROM eqstns WHERE StnId IN (", paste0(samps_locs$StnId, collapse = ", "), ") AND StnStd IS NOT NULL;"))
    if (nrow(tmp) == 0) {
      stnStds <- FALSE
    } else {
      tmp <- merge(tmp, DBI::dbGetQuery(EQWin, paste0("SELECT StdId, StdCode, StdFlag, StdDesc FROM eqstds WHERE StdCode IN ('", paste0(tmp$StdCode, collapse = "', '"), "')")))
      station_stdVals <- DBI::dbGetQuery(EQWin, paste0("SELECT StdId, ParamId, MaxVal, MinVal FROM eqstdval WHERE StdId IN (", paste(tmp$StdId, collapse = ", "), ") AND ParamId IN (", paste0(params$ParamId, collapse = ", "), ");"))
      if (nrow(station_stdVals) > 0) { # Otherwise there are no station-specific standards for the parameters in the report
        
        station_stdVals <- merge(tmp, station_stdVals, all.y = TRUE)
        
        station_stdVals$display_min <- NA
        station_stdVals$display_max <- NA
        # Some MaxVal and MinVal fields are labelled as =xxxx. These refer to calculations stored in table eqcalcs. Since the standard varies by sample, the column for the standard value will simply say 'calc'
        for (j in 1:nrow(station_stdVals)) {
          if (grepl("=", station_stdVals$MaxVal[j])) {
            station_stdVals$display_max[j] <- "calculated"
          } else {
            station_stdVals$display_max[j] <- station_stdVals$MaxVal[j]
          }
          if (grepl("=", station_stdVals$MinVal[j])) {
            station_stdVals$display_min[j] <- "calculated"
          } else {
            station_stdVals$display_min[j] <- station_stdVals$MinVal[j]
          }
        }
        
        station_stdVals$string <- NA
        for (j in 1:nrow(station_stdVals)) {
          min <- station_stdVals[j, "display_min"]
          max <- station_stdVals[j, "display_max"]
          string <- paste0(if (is.na(min)) "" else paste0("Min: ", min, " - "), if (is.na(max)) "" else paste0("Max: ", max))
          station_stdVals[j, "string"] <- if (nchar(string) > 0) string else string
        }
        station_stdVals <- merge(station_stdVals, params, all.x = TRUE)
      } else {
        stnStds <- FALSE
      }
    }
  }
  
  # Make the workbook ###############################################################################################
  wb <- openxlsx::createWorkbook(title = "Water Quality Report")
  time <- Sys.time()
  attr(time, "tzone") <- "MST"
  top <- data.frame(tmp = c(paste0("WQ report for EQWin stns  ", paste(locations$StnName, collapse = ", ")),  paste0("For ", date, if (date_approx > 0) paste0(". Requested stns w/o smpls on this date may show smpls within ", date_approx, " days.") else "" )),
                    tmp2 = NA,
                    tmp3 = NA,
                    tmp4 = NA,
                    tmp5 = NA,
                    tmp6 = NA,
                    tmp7 = NA,
                    tmp8 = c(paste0("Issued at ", substr(time, 1, 16), " MST"), paste0("Created with R package YGwater ", utils::packageVersion("YGwater"))))
  
  # Create styles and comments
  topStyle <- openxlsx::createStyle(fgFill = "turquoise2")
  # fodNameStyle <- openxlsx::createStyle(fgFill = "darkorange", border = "TopBottomLeftRight", borderStyle = "medium")
  standardHeadStyle <- openxlsx::createStyle(fgFill = "azure3")
  standardStyle <- openxlsx::createStyle(fgFill = "azure1")
  sampleHeadStyle <- openxlsx::createStyle(fgFill = "lemonchiffon2")
  sampleStyle <- openxlsx::createStyle(fgFill = "lemonchiffon")
  paramHeadStyle <- openxlsx::createStyle(fgFill = "goldenrod")
  paramStyle <- openxlsx::createStyle(fgFill = "goldenrod1")
  exceedStyle <- openxlsx::createStyle(fontColour = "black", border = "TopBottomLeftRight", borderColour = "red2", borderStyle = "medium")
  
  
  # Create the first tab with general internal + external comments
  openxlsx::addWorksheet(wb, "Report")
  
  # Create the header
  openxlsx::writeData(wb, "Report", top, startCol = 1, startRow = 1, colNames = FALSE)
  openxlsx::mergeCells(wb, "Report", cols = c(1:7), rows = 1)
  openxlsx::mergeCells(wb, "Report", cols = c(1:7), rows = 2)
  openxlsx::addStyle(wb, "Report", topStyle, rows = 1, cols = c(1:10))
  openxlsx::addStyle(wb, "Report", topStyle, rows = 2, cols = c(1:10))
  
  openxlsx::writeData(wb, "Report", NA, startCol = 1, startRow = 3, colNames = FALSE) # Empty row for spacing
  
  # Create the table
  openxlsx::writeData(wb, "Report", "Standards", startCol = 2, startRow = 4, colNames = FALSE)
  openxlsx::writeData(wb, "Report", "Stations (sample date-time)", startCol = 2 + nrow(standards), startRow = 4, colNames = FALSE)
  openxlsx::writeData(wb, "Report", final_table, startCol = 1, startRow = 5, colNames = TRUE)
  
  openxlsx::addStyle(wb, "Report", standardHeadStyle, rows = 4, cols = c(2:(2 + nrow(standards))))
  openxlsx::addStyle(wb, "Report", standardHeadStyle, rows = 5, cols = c(2:(2 + nrow(standards))))
  
  openxlsx::addStyle(wb, "Report", sampleHeadStyle, rows = 4, cols = c((2 + nrow(standards)):(2 + nrow(standards) + (ncol(final_table) - (nrow(standards) + 2)))))
  openxlsx::addStyle(wb, "Report", sampleHeadStyle, rows = 5, cols = c((2 + nrow(standards)):(2 + nrow(standards) + (ncol(final_table) - (nrow(standards) + 2)))))
  
  for (i in 1:nrow(final_table)) {
    openxlsx::addStyle(wb, "Report", standardStyle, rows = 5 + i, cols = c(2:(2 + nrow(standards))))
    openxlsx::addStyle(wb, "Report", sampleStyle, rows = 5 + i, cols = c((2 + nrow(standards)):(2 + nrow(standards) + (ncol(final_table) - (nrow(standards) + 2)))))
  }
  openxlsx::addStyle(wb, "Report", paramHeadStyle, rows = 5, cols = 1)
  openxlsx::addStyle(wb, "Report", paramStyle, rows = 6:(5 + nrow(final_table)), cols = 1)
  
  # Adjust column widths for readability
  openxlsx::setColWidths(wb, "Report", cols = c(1), widths = c(20))
  openxlsx::setColWidths(wb, "Report", cols = c(2:ncol(final_table)), widths = c(25))
  
  # Add comments for location names and also station-specific standards where available
  for (i in names(final_table)[(2 + nrow(standards)):ncol(final_table)]) {
    code <- sub(" \\(.*", "", i)
    comment <- unique(samps_locs[samps_locs$StnName == code, "StnDesc"])
    
    # And if there are station-specific standards, add them to the comment
    if (stnStds) {
      id <- unique(samps_locs[samps_locs$StnName == code, "StnId"])
      stn_std_comment <- station_stdVals[station_stdVals$StnId == id, c("ParamName", "string")]
      if (nrow(stn_std_comment) > 0) {
        df_string <- apply(stn_std_comment, 1, paste, collapse = " ")
        df_string <- paste(df_string, collapse = "\n")
        comment <- paste(comment, "", paste0("Station-Specific Standards (", unique(station_stdVals$StdDesc), ") "), df_string, sep = "\n")
      }
    }
    stn_std_comment <- data.frame()
    # Now add the comment to the correct cell
    openxlsx::writeComment(wb, "Report", row = 5, col = which(names(final_table) == i), comment = openxlsx::createComment(comment, visible = FALSE, height = if (nrow(stn_std_comment) > 0) 20 else 1))
  }
  
  
  # Flag when parameters are exceeded using comments and conditional formatting
  # Work with all-station standards first as these apply to all stations, then work on station-specific standards. Comments go in a cell of a data.frame so that station-specific comments can be accumulated later, then all comments are added cell-by-cell to the workbook.
  exceed_comments <- matrix(character(0), nrow = nrow(final_table), ncol = ncol(final_table) - (1 + nrow(standards)))
  if (!is.null(stds) ) {
    for (i in 1:nrow(final_table)) {
      # Work cell by cell, iterating over the standards to see if any are exceeded. If match, add a comment with the standard_code and conditional formatting using exceedStyle
      param <- final_table$Parameter[i]
      # flag_comment <- character(0)
      for (j in 1:nrow(standards)) {
        std_applies <- stdVals[stdVals$ParamName == param & stdVals$StdId == standards[j, "StdId"], ]
        if (nrow(std_applies) == 0 || (is.na(std_applies$MaxVal) & is.na(std_applies$MinVal))) {  # If there is no standard for this parameter, skip
          next()
        } else {  # There is a standard, so check if the MaxVal and the MinVal standards are exceeded
          for (k in (2 + nrow(standards)):ncol(final_table)) {  # Now we can work left to right cell by cell
            if (is.na(final_table[i, k])) next # it's character at this point, and NA means that there is nothing in the field
            
            compare_value <- suppressWarnings(as.numeric(final_table[i, k])) # The value to compare against the standard
            if (is.na(compare_value)) next # If the value is NA, skip. This happens notably when the value is < the detection limit.
            
            for (l in c("MinVal", "MaxVal")) {
              if (is.na(std_applies[[l]])) next  # If the standard is NA, skip
              
              # Check if it's a simple standard or a calculated one.
              if (grepl("=", std_applies[[l]])) {
                # Find the CalcId corresponding to the referenced standard
                calc_id <- DBI::dbGetQuery(EQWin, paste0("SELECT CalcId FROM eqcalcs WHERE CalcCode = '", sub("=", "", std_applies[[l]]), "';"))$CalcId
                # Find the SampleId
                stn_name <- names(final_table)[k]
                sid <- samps_locs[paste0(samps_locs$StnName, " (", samps_locs$CollectDateTime, ")") == stn_name, "SampleId"]
                min_max <- EQWinStd(calc_id, sid, EQWin)
              } else { # It's a simple standard! Nice and easy.
                min_max <- as.numeric(std_applies[[l]])
              }
              if (l == "MinVal") {
                if (compare_value < min_max) {
                  exceed_comments[i, (k - (1 + nrow(standards)))] <- paste0(if (!is.na(exceed_comments[i, (k - (1 + nrow(standards)))])) exceed_comments[i, (k - (1 + nrow(standards)))], if (!is.na(exceed_comments[i, (k - (1 + nrow(standards)))])) "\n", "Below ", standards[j, "StdCode"], " of ", min_max)
                }
              } else if (l == "MaxVal") {
                if (compare_value > min_max) {
                  exceed_comments[i, (k - (1 + nrow(standards)))] <- paste0(if (!is.na(exceed_comments[i, (k - (1 + nrow(standards)))])) exceed_comments[i, (k - (1 + nrow(standards)))], if (!is.na(exceed_comments[i, (k - (1 + nrow(standards)))])) "\n", "Exceeds ", standards[j, "StdCode"], " of ", min_max)
                }
              }
            }
          }
        }
      }
    }
  }
  if (stnStds) { # Finally, check the station-specific standard. If any are exceeded add to the matrix.
    row_names <- names(final_table)[(2 + nrow(standards)):ncol(final_table)]
    for (i in 1:length(row_names)) {
      name <- row_names[i]
      col_number_final <- which(names(final_table) == name)
      id <- unique(samps_locs[paste0(samps_locs$StnName, " (", samps_locs$CollectDateTime, ")") == name, "StnId"])  # station ID
      stn_std <- station_stdVals[station_stdVals$StnId == id, ]
      
      if (nrow(stn_std) > 0) { # Then a standard of some sort applies
        for (j in 1:nrow(final_table)) { # Now we can go cell by cell
          param <- final_table$Parameter[j]
          # see if there's a corresponding entry in stn_std
          std_applies <- stn_std[stn_std$ParamName == param, ]
          if (nrow(std_applies) == 0 || (is.na(std_applies$MaxVal) & is.na(std_applies$MinVal))) {  # If there is no standard for this parameter, skip
            next
          } else {  # There is a standard, so check if the MaxVal and the MinVal standards are exceeded
            if (is.na(final_table[i, col_number_final])) next # it's character at this point, and NA means that there is nothing in the field
            
            compare_value <- suppressWarnings(as.numeric(final_table[j, col_number_final])) # The value to compare agains the standard
            if (is.na(compare_value)) next # If the value is NA, skip. This happens notably when the value is < the detection limit.
            
            for (l in c("MinVal", "MaxVal")) {
              if (is.na(std_applies[[l]])) next  # If the standard is NA, skip
              # Check if it's a simple standard or a calculated one.
              if (grepl("=", std_applies[[l]])) {
                # Find the CalcId corresponding to the referenced standard
                calc_id <- DBI::dbGetQuery(EQWin, paste0("SELECT CalcId FROM eqcalcs WHERE CalcCode = '", sub("=", "", std_applies[[l]]), "';"))$CalcId
                # Find the SampleId
                sid <- samps_locs[paste0(samps_locs$StnName, " (", samps_locs$CollectDateTime, ")") == name, "SampleId"]
                min_max <- EQWinStd(calc_id, sid, EQWin)
              } else { # It's a simple standard! Nice and easy.
                min_max <- as.numeric(std_applies[[l]])
              }
              if (l == "MinVal") {
                if (compare_value < min_max) {
                  exceed_comments[j,i] <- paste0(if (!is.na(exceed_comments[j,i])) exceed_comments[j,i], if (!is.na(exceed_comments[j,i])) "\n", "Below ", std_applies$StdCode, " of ", min_max)
                }
              } else if (l == "MaxVal") {
                if (compare_value > min_max) {
                  exceed_comments[j,i] <- paste0(if (!is.na(exceed_comments[j,i])) exceed_comments[j,i], if (!is.na(exceed_comments[j,i])) "\n", "Exceeds ", std_applies$StdCode, " of ", min_max)
                }
              }
            }
          }
        }
      }
    }
  }
  
  # Work through the exceed_comments matrix and add comments and conditional formatting to the workbook
  for (i in 1:nrow(exceed_comments)) {
    for (j in 1:ncol(exceed_comments)) {
      comment <- exceed_comments[i, j]
      if (is.na(comment)) next()
      openxlsx::writeComment(wb, "Report", row = 5 + i, col = j + (1 + nrow(standards)), comment = openxlsx::createComment(comment, visible = FALSE, height = 20))
      openxlsx::addStyle(wb, "Report", exceedStyle, rows = 5 + i, cols = j + (1 + nrow(standards)), stack = TRUE)
    }
  }
  
  # Freeze panes so the parameters, standards, and location header are always visible
  openxlsx::freezePane(wb, "Report", firstActiveRow = 6, firstActiveCol = (2 + nrow(standards)))
  
  openxlsx::saveWorkbook(wb, paste0(save_path, "/WQ Report for ", date, " Issued ", Sys.Date(), ".xlsx"), overwrite = TRUE)
  
  return(message("Report saved to ", paste0(save_path, "/WQ Report for ", date, " Issued ", Sys.Date(), ".xlsx")))
  
}
