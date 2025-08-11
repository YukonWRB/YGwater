#' Discrete (lab or field point data) plotting function
#' 
#' Plots data directly from EQWin or from the aquacache for one or more location (station) and one or more parameter. Depending on the setting for argument 'facet_on', the function can either make a facet plot where each station is a facet (with parameters as distinct traces) or where each parameter is a facet (with locations as distinct traces). Values above or below the detection limit are shown as the detection limit but symbolized differently (open circles). 
#' 
#' @param start The date to fetch data from, passed as a Date, POSIXct, or character vector of form 'yyyy-mm-dd HH:MM'. Dates and character vectors are converted to POSIXct with timezone 'MST'. Uses the actual sample datetime, not the target datetime.
#' @param end The end date to fetch data up to, passed as a Date, POSIXct, or character vector of form 'yyyy-mm-dd HH:MM'. Dates and character vectors are converted to POSIXct with timezone 'MST'. Uses the actual sample datetime, not the target datetime. Default is the current date.
#' @param locations A vector of station names or codes. If dbSource == 'AC': from aquacache 'locations' table use column 'location', 'name', or 'name_fr' (character vector) or 'location_id' (numeric vector). If dbSource == 'EQ' use EQWiN 'eqstns' table, column 'StnCode' or leave NULL to use `locGrp` instead.
#' @param locGrp Only used if `dbSource` is 'EQ'. A station group as listed in the EWQin 'eqgroups' table, column 'groupname.' Leave NULL to use `locations` instead.
#' @param sub_locations A vector of sub-location names or codes, only used if dbSource == 'AC' and table 'sub_locations'. Default is NULL; if there are sub-locations applicable, these will all be fetched and displayed as distinct traces. Must match the length of 'locations', use NA for locations without sub-locations.
#' @param parameters A vector of parameter names or codes. If dbSource == 'AC': from aquacache 'parameters' table use column 'param_name' or 'param_name_fr' (character vector) or 'parameter_id' (numeric vector). If dbSource == 'EQ' use EQWin 'eqparams' table, column 'ParamCode' or leave NULL to use `paramGrp` instead.
#' @param paramGrp Only used if `dbSource` is 'EQ'. A parameter group as listed in the EQWin 'eqgroups' table, column 'groupname.' Leave NULL to use `parameters` instead.
#' @param standard A standard or guideline name as listed in the EQWin eqstds table, column StdCode. Leave NULL to exclude standards. Only valid if `dbSource` is 'EQ'.
#' @param log Should the y-axis be log-transformed?
#' @param facet_on Should the plot be faceted by locations or by parameters? Specify one of 'locs' or 'params'. Default is 'locs'.
#' @param loc_code Should the location code be used instead of the full location name? Options are 'code', 'name', 'codeName', 'nameCode'. Default is 'name'.
#' @param shareX Should the x-axis be shared across facets? Default is TRUE (dates are shared).
#' @param shareY Should the y-axis be shared across facets? Default is FALSE (values are not shared).
#' @param rows The number of rows to use in the facet grid. Default is 'auto' to automatically determine the number of rows based on the number of facets.
#' @param target_datetime Should the plot datetime use the 'target' datetime instead of the 'actual' datetime? Default is TRUE. This is only applicable is dbSource == 'AC'.
#' @param colorblind Should the plot be colorblind-friendly? Default is FALSE.
#' @param lang The language to use for the plot. Currently only "en" and "fr" are supported. Default is "en", and this is only supported for dbSource == 'AC'.
#' @param point_scale A scale factor to apply to the size of points. Default is 1.
#' @param guideline_scale A scale factor to apply to the size of standard/guideline values Default is 1.
#' @param axis_scale A scale factor to apply to the size of axis labels. Default is 1.
#' @param legend_scale A scale factor to apply to the size of text in the legend. Default is 1.
#' @param legend_position The position of the legend, 'v' for vertical on the right side or 'h' for horizontal on the bottom. Default is 'v'. If 'h', slider will be set to FALSE due to interference.
#' @param gridx Should gridlines be drawn on the x-axis? Default is FALSE
#' @param gridy Should gridlines be drawn on the y-axis? Default is FALSE
#' @param dbSource The database source to use, 'AC' for aquacache or 'EQ' for EQWin. Default is 'EQ'. Connections to aquacache are made using function [AquaConnect()] while EQWin connections use [AccessConnect()].
#' @param dbCon A database connection object, optional. Leave NULL to create a new connection and have it closed automatically.
#' @param dbPath The path to the EQWin database, if called for in parameter `dbSource`.
#' @param data Should the data used to create the plot be returned? Default is FALSE.
#'
#' @return An interactive HTML plot of the data from EQWin.
#' @export
#'

plotDiscrete <- function(start, 
                         end = Sys.Date() + 1, 
                         locations = NULL, locGrp = NULL, 
                         sub_locations = NULL,
                         parameters = NULL, paramGrp = NULL, 
                         standard = NULL, 
                         log = FALSE, 
                         facet_on = 'params', 
                         loc_code = 'name',
                         shareX = TRUE,
                         shareY = FALSE,
                         rows = 'auto', 
                         target_datetime = TRUE, 
                         colorblind = FALSE, 
                         lang = "en", 
                         point_scale = 1, 
                         guideline_scale = 1, 
                         axis_scale = 1, 
                         legend_scale = 1,
                         legend_position = 'v',
                         gridx = FALSE,
                         gridy = FALSE,
                         dbSource = "EQ", 
                         dbCon = NULL,
                         dbPath = eqwin_db_path(),
                         data = FALSE) {
  
  # testing parameters for EQWIN direct
  # start <- "2024-06-24"
  # end <- "2025-03-13"
  # locations <- c("(EG)W29")
  # parameters <- c("CN-WAD", "Co-T", "Cu-T")
  # locGrp <- NULL
  # paramGrp <- NULL
  # standard = "CCME_LT"
  # loc_code = 'name'
  # shareX = TRUE
  # shareY = FALSE
  # log = FALSE
  # facet_on = 'params'
  # rows = 'auto'
  # colorblind = FALSE
  # target_datetime = FALSE
  # point_scale = 1
  # axis_scale = 1
  # legend_scale = 1
  # guideline_scale = 1
  # gridx = FALSE
  # gridy = FALSE
  # dbSource = "EQ"
  # lang = "en"
  # dbPath = "//env-fs/env-data/corp/water/Data/Databases_virtual_machines/databases/EQWinDB/WaterResourcesEG.mdb"

  # start <- "2024-06-09"
  # end <- "2024-10-09"
  # locations <- "(EG)W4-mix"
  # parameters <- "HG-D"
  # locGrp <- NULL
  # paramGrp <- NULL
  # standard = "CSR_s3_fAL"
  # loc_code = 'name'
  # log = FALSE
  # facet_on = 'locs'
  # rows = 'auto'
  # colorblind = FALSE
  # target_datetime = FALSE
  # point_scale = 1
  # axis_scale = 1
  # legend_scale = 1
  # guideline_scale = 1
  # dbSource = "EQ"
  # lang = "en"
  # dbPath = "//env-fs/env-data/corp/water/Data/Databases_virtual_machines/databases/EQWinDB/WaterResources.mdb"
  
  # testing parameters for aquacache
  # start <- "2020-01-01"
  # end <- "2024-05-05"
  # locations <- 155
  # sub_locations <- NULL
  # parameters <- 21
  # locGrp <- NULL
  # paramGrp <- NULL
  # standard = NULL
  # log = FALSE
  # loc_code= 'name'
  # facet_on = 'locs'
  # rows = 'auto'
  # colorblind = FALSE
  # lang = "en"
  # point_scale = 1
  # axis_scale = 1
  # legend_scale = 1
  # target_datetime = FALSE
  # guideline_scale = 1
  # dbSource = "AC"
  # dbPath = "//env-fs/env-data/corp/water/Data/Databases_virtual_machines/databases/EQWinDB/WaterResources.mdb"
  # target_datetime = TRUE

  # initial checks, connection, and validations #######################################################################################
  
  return_data <- data   # data is used in this function, but keeping the argument as 'data' keeps things consistent with other functions
  
  if (!dbSource %in% c("AC", "EQ")) stop("dbSource must be either 'AC' or 'EQ'")
  
  # Check on loc_code
  if (!loc_code %in% c("code", "name", "codeName", "nameCode")) stop("loc_code must be either 'code', 'name', 'codeName', or 'nameCode'")
  
  if (dbSource == 'AC') {
    if (is.null(locations)) stop("You must specify locations when 'dbSource' is 'AC'")
    if (is.null(parameters)) stop("You must specify parameters when 'dbSource' is 'AC'")
    
    if (!is.null(locGrp)) {
      stop("Parameter 'locGrp' is only used when 'dbSource' is 'EQ'")
    }
    if (!is.null(paramGrp)) {
      stop("Parameter 'paramGrp' is only used when 'dbSource' is 'EQ'")
    }
    
  } else { # dbSource == 'EQ'
    if (!file.exists(dbPath)) stop("The EQWin database path does not exist or you do not have the necessary privileges.")
    
    if (is.null(locations) & is.null(locGrp)) stop("You must specify either locations or locGrp")
    if (!is.null(locations) & !is.null(locGrp)) stop("You must specify either locations or locGrp, not both")
    if (is.null(parameters) & is.null(paramGrp)) stop("You must specify either parameters or paramGrp")
    if (!is.null(parameters) & !is.null(paramGrp)) stop("You must specify either parameters or paramGrp, not both")
    
    if (target_datetime) {
      warning("Parameter 'target_datetime' is only used when 'dbSource' is 'AC'")
      target_datetime <- FALSE
    }
    
    if (!is.null(locGrp)) {
      if (length(locGrp) > 1) {
        stop("Parameter 'locGrp' must be a single group name/code")
      }
    }
    if (!is.null(paramGrp)) {
      if (length(paramGrp) > 1) {
        stop("Parameter 'paramGrp' must be a single group name/code")
      }
    }
  }
  
  # check for proper call of standards
  if (!is.null(standard)) {
    if (dbSource == "AC") {
      warning("Parameter 'standard' is only used when 'dbSource' is 'EQ'")
      standard <- NULL
    } else {
      if (length(standard) > 1) stop("Parameter 'standard' must be a single standard name/code. Refer to function documentation.")
    }
  }
  
  facet_on <- tolower(facet_on)
  if (!facet_on %in% c("locs", "params")) stop("facet_on must be either 'locs' or 'params'")
  
  if (rows != "auto") {
    if (!is.numeric(rows)) stop("rows must be a numeric value or 'auto'")
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
  
  
  # Create a data.frame to hold the plotting data. EQWin and aquacache fill this list in differently, but the end result is the same to pass on to the plotting portion.
  # 'data' should contain columns named location, location_name, parameter, datetime, result, result_condition (e.g. <DL, >DL, etc.), result_condition_value (the detection limit value), units. Optional columns (used by aquacache) are sample_type, collection_method, sample_fraction, result_speciation.
  data <- data.frame()
  
  # Fetch the data ##############################################################################################################
  if (dbSource == "EQ") {
    ## Fetch data from EQWin #####################################################################################################
    # Connect to EQWin
    if (!is.null(dbCon)) {
      EQWin <- dbCon
    } else {
      EQWin <- AccessConnect(dbPath, silent = TRUE)
      on.exit(DBI::dbDisconnect(EQWin), add = TRUE)
    }
    
    # Validate existence of standards
    if (!is.null(standard)) {
      standards <- DBI::dbGetQuery(EQWin, glue::glue_sql("SELECT StdId, StdCode, StdFlag, StdDesc FROM eqstds WHERE StdCode IN ({standard*})", .con = EQWin))
      if (nrow(standards) == 0) {
        stop("No standards found in the EQWin database with the name '", standard, "'")
      }
    }
    
    # Fetch the station and/or parameter list if necessary (locGrp or paramGrp was specified)
    if (!is.null(locGrp)) {
      # Check if the group actually exists
      grp_count <- DBI::dbGetQuery(EQWin, glue::glue_sql("SELECT COUNT(*) FROM eqgroups WHERE groupname = {locGrp} AND dbtablename = 'eqstns'", .con = EQWin))[1,1]
      if (grp_count == 0) {
        stop("The station group '", locGrp, "' does not exist in the EQWin database")
      } else if (grp_count > 1) {
        stop("There are multiple station groups with the name '", locGrp, "' in the EQWin database")
      } # otherwise proceed to fetch the locations
      
      StnIds <- DBI::dbGetQuery(EQWin, glue::glue_sql("SELECT groupitems FROM eqgroups WHERE groupname = {locGrp} AND dbtablename = 'eqstns'", .con = EQWin))$groupitems
      StnIds <- strsplit(StnIds, ",")[[1]]
      if (length(StnIds) == 0) {
        stop("No locations found in the station group '", locGrp, "'")
      }
    }
    if (!is.null(paramGrp)) {
      # Check if the group actually exists
      grp_count <- DBI::dbGetQuery(EQWin, glue::glue_sql("SELECT COUNT(*) FROM eqgroups WHERE groupname = {paramGrp} AND dbtablename = 'eqparams'", .con = EQWin))[1,1]
      if (grp_count == 0) {
        stop("The parameter group '", paramGrp, "' does not exist in the EQWin database")
      } else if (grp_count > 1) {
        stop("There are multiple parameter groups with the name '", paramGrp, "' in the EQWin database")
      } # otherwise proceed to fetch the parameters
      
      paramIds <- DBI::dbGetQuery(EQWin, glue::glue_sql("SELECT groupitems FROM eqgroups WHERE groupname = {paramGrp} AND dbtablename = 'eqparams'", .con = EQWin))$groupitems
      paramIds <- strsplit(paramIds, ",")[[1]]
      if (length(paramIds) == 0) {
        stop("No parameters found in the parameter group '", paramGrp, "'")
      }
    }
    
    # Validate existence of parameters and/or locations
    if (!is.null(locations)) {
      StnIds <- DBI::dbGetQuery(EQWin, glue::glue_sql("SELECT StnId, StnCode FROM eqstns WHERE StnCode IN ({locations*})", .con = EQWin))
      if (nrow(StnIds) == 0) {
        stop("No locations found in the EQWin database with the names '", paste0(locations, collapse = "', '"), "'")
      }
      if (nrow(StnIds) < length(locations)) {
        # Find the missing locations and tell the user which ones are missing
        missing <- setdiff(locations, StnIds$StnCode)
        warning("The following locations were not found in the EQWin database: ", paste0(missing, collapse = ", "))
      }
      StnIds <- StnIds$StnId
    }
    
    if (!is.null(parameters)) {
      paramIds <- DBI::dbGetQuery(EQWin, glue::glue_sql("SELECT ParamId, ParamCode FROM eqparams WHERE ParamCode IN ({parameters*})", .con = EQWin))
      if (nrow(paramIds) == 0) {
        stop("No parameters found in the EQWin database with the names '", paste0(parameters, collapse = "', '"), "'")
      }
      if (nrow(paramIds) < length(parameters)) {
        # Find the missing parameters and tell the user which ones are missing
        missing <- setdiff(parameters, paramIds$ParamCode)
        warning("The following parameters were not found in the EQWin database: ", paste0(missing, collapse = ", "))
      }
      paramIds <- paramIds$ParamId
    }
    
    sampleIds <- DBI::dbGetQuery(EQWin, glue::glue_sql("SELECT eqsampls.StnId, eqsampls.SampleId, eqsampls.CollectDateTime FROM eqsampls INNER JOIN eqcodes ON eqsampls.SampleClass = eqcodes.CodeValue WHERE eqcodes.CodeField = 'eqsampls.SampleClass' AND eqsampls.StnId IN ({StnIds*}) AND eqsampls.CollectDateTime > {start} AND eqsampls.CollectDateTime < {end} AND eqcodes.CodeValue <> 'D';", .con = EQWin))
    
    if (nrow(sampleIds) == 0) {
      stop("No samples found for the date range and locations specified.")
    }
    
    results <- DBI::dbGetQuery(EQWin, paste0("SELECT eqdetail.SampleId, eqdetail.ParamId, eqdetail.Result, eqparams.ParamCode, eqparams.ParamName FROM eqdetail INNER JOIN eqparams ON eqdetail.ParamId = eqparams.ParamId WHERE eqdetail.SampleId IN (", paste0(sampleIds$SampleId, collapse = ", "), ") AND eqdetail.ParamId IN (", paste0(paramIds, collapse = ", "), ");"))
    
    
    params <- DBI::dbGetQuery(EQWin, paste0("SELECT ParamId, ParamName, Units FROM eqparams;"))
    
    samps <- sampleIds[sampleIds$SampleId %in% results$SampleId, ]
    locations <- DBI::dbGetQuery(EQWin, paste0("SELECT StnId, StnName, StnDesc FROM eqstns WHERE StnId IN (", paste0(samps$StnId, collapse = ", "), ");"))
    samps_locs <- merge(locations, samps)
    
    data <- merge(results[, c("SampleId", "ParamId", "Result")], samps_locs[, c("SampleId", "StnId", "CollectDateTime")])
    data <- merge(data, params)
    data <- merge(data, locations)
    data <- data[, -which(names(data) %in% c("StnId"))]  # Drop unnecessary column   ! Note that this leaves some columns that are not output from the aquacache data fetch; thse are only for adding standard values to the plot and aquacache will need to work differently.
    names(data) <- c("ParamId", "SampleId", "result", "datetime", "param_name", "units", "location", "location_name")
    
    # Now add the result_condition and result_condition_value columns
    # Sometimes the "." is a "," in the result, so we need to replace it
    data$result <- gsub(",", ".", data$result)
    #result_condition should get < DL, > DL, or NA depending on if '<' or '>' show up in columns 'result'
    data$result_condition <- ifelse(grepl("<", data$result), "< DL", ifelse(grepl(">", data$result), "> DL", NA))
    #result_condition_value should get the numeric portion of the string in 'result' only if '<' or '>' show up in columns 'result'
    data$result_condition_value <- ifelse(grepl("<", data$result), as.numeric(gsub("<", "", data$result)), ifelse(grepl(">", data$result), as.numeric(gsub(">", "", data$result)), NA))
    # turn column 'result' to a numeric, which will remove the '<' and '>' characters
    data$result <- suppressWarnings(as.numeric(data$result))
    
    # Check encoding and if necessary convert to UTF-8, otherwise plotly gets grumpy
    locale_info <- Sys.getlocale("LC_CTYPE")
    encoding <- sub(".*\\.([^@]+).*", "\\1", locale_info)
    for (i in 1:ncol(data)) {
      if (inherits(data[, i], "character")) {
        tryCatch({
          grepl("[^\x01-\x7F]", data[[i]])
        }, warning = function(w) {
          if (encoding != "utf8") {
            data[, i] <<- iconv(data[, i], from = encoding, to = "UTF-8")
          }
        })
      }
    }
    
    # Pull in standards from EQWin if necessary. This may involve calculations and could take some time!
    if (!is.null(standard)) {
      stdVals <- DBI::dbGetQuery(EQWin, paste0("SELECT StdId, ParamId, MaxVal AS std_max, MinVal AS std_min FROM eqstdval WHERE StdId = ", standards$StdId, " AND ParamId IN (", paste0(unique(results$ParamId), collapse = ", "), ");"))
      data <- merge(data, stdVals, all.x = TRUE)

      # Now for rows where the std_max or std_min starts with "=" we need to calculate it and replace with the calculated value
      
      # Identify rows where std_max and std_min start with "="
      std_max_eq_idx <- which(!is.na(data$std_max) & grepl("^=", data$std_max))
      std_min_eq_idx <- which(!is.na(data$std_min) & grepl("^=", data$std_min))
      
      # Extract CalcCodes by removing "="
      data$CalcCode_std_max <- NA_character_
      data$CalcCode_std_min <- NA_character_
      
      data$CalcCode_std_max[std_max_eq_idx] <- sub("^=", "", data$std_max[std_max_eq_idx])
      data$CalcCode_std_min[std_min_eq_idx] <- sub("^=", "", data$std_min[std_min_eq_idx])
      
      # Combine all CalcCodes and get unique values
      all_calc_codes <- unique(c(
        data$CalcCode_std_max[std_max_eq_idx],
        data$CalcCode_std_min[std_min_eq_idx]
      ))
      all_calc_codes <- all_calc_codes[!is.na(all_calc_codes)]
      
      if (length(all_calc_codes) > 0) {
        # Fetch CalcId for all unique CalcCodes in one query
        query <- paste0(
          "SELECT CalcCode, CalcId FROM eqcalcs WHERE CalcCode IN (",
          paste0("'", all_calc_codes, "'", collapse = ", "),
          ");"
        )
        code_to_calcid_df <- DBI::dbGetQuery(EQWin, query)
        
        # Create a lookup table for CalcCode to CalcId
        code_to_calcid <- stats::setNames(code_to_calcid_df$CalcId, code_to_calcid_df$CalcCode)
        
        # Map CalcCodes to CalcIds in data
        data$CalcId_std_max <- NA_integer_
        data$CalcId_std_min <- NA_integer_
        
        data$CalcId_std_max[std_max_eq_idx] <- code_to_calcid[data$CalcCode_std_max[std_max_eq_idx]]
        data$CalcId_std_min[std_min_eq_idx] <- code_to_calcid[data$CalcCode_std_min[std_min_eq_idx]]
        
        # Prepare data frames for std_max and std_min calculations
        if (length(std_max_eq_idx) > 0) {
          std_max_df <- data.frame(
            idx = std_max_eq_idx,
            CalcId = data$CalcId_std_max[std_max_eq_idx],
            SampleId = data$SampleId[std_max_eq_idx],
            std_type = "std_max",
            stringsAsFactors = FALSE
          )
        } else {
          std_max_df <- data.frame()
        }
        
        if (length(std_min_eq_idx) > 0) {
          std_min_df <- data.frame(
            idx = std_min_eq_idx,
            CalcId = data$CalcId_std_min[std_min_eq_idx],
            SampleId = data$SampleId[std_min_eq_idx],
            std_type = "std_min",
            stringsAsFactors = FALSE
          )
        } else {
          std_min_df <- data.frame()
        }
        
        # Combine both data frames
        combined_df <- rbind(std_max_df, std_min_df)
        
        # Get unique combinations of CalcId and SampleId to minimize EQWinStd calls
        unique_combinations <- unique(combined_df[, c("CalcId", "SampleId")])
        
        
        # Initialize a data frame to store EQWinStd results
        EQWinStd_result <- data.frame()
        
        # Process each unique CalcId separately
        for (calc_id in unique(unique_combinations$CalcId)) {
          # Get SampleIds for this CalcId
          sample_ids <- unique_combinations$SampleId[unique_combinations$CalcId == calc_id]
          
          # Call EQWinStd for this CalcId and vector of SampleIds
          result_list <- suppressWarnings(EQWinStd(CalcIds = calc_id, SampleIds = sample_ids, con = EQWin))
          
          # Extract the result data frame
          result_df <- result_list[[as.character(calc_id)]]
          
          # Add CalcId column
          result_df$CalcId <- calc_id
          
          # Append to EQWinStd_result
          EQWinStd_result <- rbind(EQWinStd_result, result_df)
        }
        
        # Merge the results back to the combined_df
        merged_df <- merge(combined_df, EQWinStd_result, by = c("CalcId", "SampleId"), all.x = TRUE)
        
        # Assign the calculated values back to data$std_max and data$std_min
        data$std_max[merged_df$idx[merged_df$std_type == "std_max"]] <- merged_df$Value[merged_df$std_type == "std_max"]
        data$std_min[merged_df$idx[merged_df$std_type == "std_min"]] <- merged_df$Value[merged_df$std_type == "std_min"]
      }

      
      # Convert columns to numeric
      data$std_max <- as.numeric(data$std_max)
      data$std_min <- as.numeric(data$std_min)

    }
    
  } else { # dbSource == "AC"
    
    # Fetch data from aquacache ##################################################################################################
    # Connect to AC
    if (!is.null(dbCon)) {
      AC <- dbCon
    } else {
      AC <- AquaConnect(silent = TRUE)
      on.exit(DBI::dbDisconnect(AC), add = TRUE)
    }
    
    if (!is.null(sub_locations)) {
      if (length(sub_locations) != length(locations)) {
        stop("The length of 'sub_locations' must match the length of 'locations'")
      }
    }
    
    # Validate existence of parameters and/or locations
    if (!is.null(locations)) {
      if (inherits(locations, "character")) {
        query <- paste0(
          "SELECT location_id, location, name, name_fr FROM locations WHERE ",
          "LOWER(location) IN (LOWER('", paste0(locations, collapse = "'), LOWER('"), "')) ",
          "OR LOWER(name) IN (LOWER('", paste0(locations, collapse = "'), LOWER('"), "'))",
          "OR LOWER(name_fr) IN (LOWER('", paste0(locations, collapse = "'), LOWER('"), "')) "
        )
      } else {
        query <- paste0("SELECT location_id, location, name, name_fr FROM locations WHERE location_id IN (", paste0(locations, collapse = ", "), ");")
      }
      
      locIds <- DBI::dbGetQuery(AC, query)
      if (nrow(locIds) == 0) {
        stop("No locations found in the aquacache with the names or codes '", paste0(locations, collapse = "', '"), "'")
      }
      if (nrow(locIds) < length(locations)) {
        # Find the missing locations and tell the user which ones are missing; 
        combined_locIds <- unique(c(locIds$location_id, locIds$location, locIds$name, locIds$name_fr))
        missing <- setdiff(locations, combined_locIds)
        
        # Find the element's index and remove it from locations and sub_locations (if not null)

          missing_idx <- which(locations %in% missing)
          locations <- locations[-missing_idx]
          if (!is.null(sub_locations)) {
            sub_locations <- sub_locations[-missing_idx]
          }
        
        if (inherits(locations, "character")) {
          warning("The following locations were not found in the aquacache despite searching the 'location', 'name', and 'name_fr' columns of table 'locations': ", paste0(missing, collapse = ", "), ". Moving on without that location (and sub-location if applicable).")
        } else {
          warning("The following locations were not found in the aquacache table 'locations': ", paste0(missing, collapse = ", "), ". Moving on without that location (and sub-location if applicable)")
        }
      }
    }
    
    if (!is.null(sub_locations)) {
      query <- paste0(
        "SELECT sub_location_id, sub_location_name, sub_location_name_fr FROM sub_locations WHERE ",
        "LOWER(sub_location) IN (LOWER('", paste0(sub_locations, collapse = "'), LOWER('"), "')) ",
        "OR LOWER(sub_location_name) IN (LOWER('", paste0(sub_locations, collapse = "'), LOWER('"), "'))",
        "OR LOWER(sub_location_name_fr) IN (LOWER('", paste0(sub_locations, collapse = "'), LOWER('"), "')) "
      )
      
      subLocIds <- DBI::dbGetQuery(AC, query)
      if (nrow(subLocIds) == 0) {
        warning("You specified sub_locations but none were found in the aquacache. Ignoring sub_locations.")
      }
      if (nrow(subLocIds) < length(sub_locations)) {
        # Find the missing sub-locations and tell the user which ones are missing
        combined_subLocIds <- unique(c(subLocIds$sub_location_id, subLocIds$sub_location, subLocIds$name, subLocIds$name_fr))
        missing <- setdiff(sub_locations, combined_subLocIds)
        if (inherits(sub_locations, "character")) {
          warning("The following sub-locations were not found in the aquacache despite searching the 'sub_location', 'name', and 'name_fr' columns of table 'sub_locations': ", paste0(missing, collapse = ", "))
        } else {
          warning("The following sub-locations were not found in the aquacache table 'sub_locations': ", paste0(missing, collapse = ", "))
        }
      }
    } else {
      subLocIds <- data.frame()
    }
    
    if (!is.null(parameters)) {
      if (inherits(parameters, "character")) {
        query <- paste0(
          "SELECT parameter_id, param_name, param_name_fr, unit_default AS units FROM parameters WHERE ",
          "LOWER(param_name) IN (LOWER('", paste0(parameters, collapse = "'), LOWER('"), "')) ",
          "OR LOWER(param_name_fr) IN (LOWER('", paste0(parameters, collapse = "'), LOWER('"), "'))"
        )
      } else {
        query <- paste0("SELECT parameter_id, param_name, param_name_fr, unit_default AS units FROM parameters WHERE parameter_id IN (", paste0(parameters, collapse = ", "), ");")
      }
      
      paramIds <- DBI::dbGetQuery(AC, query)
      if (nrow(paramIds) == 0) {
        stop("No parameters found in the aquacache with the names '", paste0(parameters, collapse = "', '"), "'")
      }
      if (nrow(paramIds) < length(parameters)) {
        # Find the missing parameters and tell the user which ones are missing
        combined_paramIds <- unique(c(paramIds$parameter_id, paramIds$param_name, paramIds$param_name_fr))
        missing <- setdiff(parameters, combined_paramIds)
        if (inherits(parameters, "character")) {
          warning("The following parameters were not found in the aquacache despite searching the 'param_name' and 'param_name_fr' columns of table 'parameters': ", paste0(missing, collapse = ", "))
        } else {
          warning("The following parameters were not found in the aquacache table 'parameters': ", paste0(missing, collapse = ", "))
        }
      }
    }
    
    if (is.null(sub_locations) | nrow(subLocIds) == 0) {
      samp_query <- paste0("
    SELECT
        s.sample_id,
        s.location_id,
        sl.sub_location_name,
        sl.sub_location_name_fr,
        s.sub_location_id,
        mt.media_type,
        mt.media_type_fr,
        s.z,
        s.datetime,
        s.target_datetime,
        cm.collection_method,
        st.sample_type,
        gt.grade_type_description,
        gt.grade_type_description_fr,
        at.approval_type_description,
        at.approval_type_description_fr,
        qt.qualifier_type_description,
        qt.qualifier_type_description_fr
    FROM 
        samples as s
    LEFT JOIN
        media_types as mt ON s.media_id = mt.media_id
    LEFT JOIN
        collection_methods as cm ON s.collection_method = cm.collection_method_id
    LEFT JOIN
        sample_types as st ON s.sample_type = st.sample_type_id
    LEFT JOIN
        grade_types as gt ON s.sample_grade = gt.grade_type_id
    LEFT JOIN
        approval_types as at ON s.sample_approval = at.approval_type_id
    LEFT JOIN
        qualifier_types as qt ON s.sample_qualifier = qt.qualifier_type_id
    LEFT JOIN
        sub_locations AS sl ON s.sub_location_id = sl.sub_location_id
    WHERE s.location_id IN (", paste0(locIds$location_id, collapse = ", "), ") 
    AND s.datetime > '", start, "' AND s.datetime < '", end, "';
        ")
    } else {
      samp_query <- paste0("
SELECT
    s.sample_id,
    s.location_id,
    sl.sub_location_name,
    sl.sub_location_name_fr,
    mt.media_type,
    mt.media_type_fr,
    s.z,
    s.datetime,
    s.target_datetime,
    cm.collection_method,
    st.sample_type,
    gt.grade_type_description,
    gt.grade_type_description_fr,
    at.approval_type_description,
    at.approval_type_description_fr,
    qt.qualifier_type_description,
    qt.qualifier_type_description_fr
FROM 
    samples AS s
LEFT JOIN
    media_types AS mt ON s.media_id = mt.media_id
LEFT JOIN
    collection_methods AS cm ON s.collection_method = cm.collection_method_id
LEFT JOIN
    sample_types AS st ON s.sample_type = st.sample_type_id
LEFT JOIN
    grade_types AS gt ON s.sample_grade = gt.grade_type_id
LEFT JOIN
    approval_types AS at ON s.sample_approval = at.approval_type_id
LEFT JOIN
    qualifier_types AS qt ON s.sample_qualifier = qt.qualifier_type_id
LEFT JOIN
    sub_locations AS sl ON s.sub_location_id = sl.sub_location_id
WHERE
    (s.location_id, COALESCE(s.sub_location_id, -1)) IN (
        ", paste0("(", locIds$location_id, ", ", ifelse(is.na(subLocIds$sub_location_id), -1, subLocIds$sub_location_id), ")", collapse = ", "), "
    )
AND s.datetime > '", start, "' AND s.datetime < '", end, "';
")
    }
    
    samples <- DBI::dbGetQuery(AC, samp_query)
    
    
    if (nrow(samples) == 0) {
      stop("No samples were found matching your requested locations and parameters.")
    }
    
    # Merge the locations into the samples data.frame
    samples <- merge(samples, locIds, by = "location_id", all.x = TRUE)
    samples <- samples[, -which(names(samples) == "location_id")] # drop unnecessary columns
    
    # Merge columns for location name and sub_location name (where not null)
    if (lang == "en") {
      samples$name <- ifelse(is.na(samples$sub_location_name), samples$name, paste0(samples$name, " - ", samples$sub_location_name))
    } else {
      samples$name <- ifelse(is.na(samples$sub_location_name_fr), samples$name_fr, paste0(samples$name_fr, " - ", samples$sub_location_name_fr))
    }

    # Get the measurements from table results
    res_query <- paste0("
    SELECT 
        r.sample_id,
        r.parameter_id,
        r.result_type,
        r.result,
        r.result_condition, 
        r.result_condition_value,
        rvt.result_value_type,
        sf.sample_fraction, 
        rs.result_speciation 
    FROM 
        results AS r
    LEFT JOIN 
        sample_fractions AS sf ON r.sample_fraction = sf.sample_fraction_id
    LEFT JOIN 
        result_speciations AS rs ON r.result_speciation = rs.result_speciation_id
    LEFT JOIN
        result_value_types AS rvt ON r.result_value_type = rvt.result_value_type_id
    WHERE 
        r.sample_id IN (", paste0(samples$sample_id, collapse = ", "), ")
    AND
        r.parameter_id IN (", paste0(paramIds$parameter_id, collapse = ", "), ")
        ;")
    
    results <- DBI::dbGetQuery(AC, res_query)
    
    if (nrow(results) == 0) {
      stop("No results were found for the date range locations/parameter combinations specified.")
    }
    
    #Swap the datetime columns if target_datetime is TRUE
    if (target_datetime) {
      names(samples)[names(samples) == "datetime"] <- "actual_datetime"
      names(samples)[names(samples) == "target_datetime"] <- "datetime"
      names(samples)[names(samples) == "actual_datetime"] <- "target_datetime"
    } 
    
    # Merge the results with the samples and paramIds data.frames
    data <- merge(results, samples, by = "sample_id", all.x = TRUE)
    data <- merge(data, paramIds, by = "parameter_id", all.x = TRUE)
    
    # Now make result_condition column understandable
    #result_condition should get < DL, > DL, or NA depending on if 1 or 2 show up in column 'result_condition'
    data$result_condition <- ifelse(grepl("1", data$result_condition), "< DL", ifelse(grepl("2", data$result_condition), "> DL", NA))
    
    # Retain columns depending on if 'fr' or 'en', rename cols to match EQWin output
    if (lang == "fr") {
      data <- data[, c("result", "target_datetime", "datetime", "param_name_fr", "units", "location", "name", "result_condition", "result_condition_value", "sample_type", "collection_method", "sample_fraction", "result_speciation")]
      names(data) <- c("result", "target_datetime", "datetime", "param_name", "units", "location", "location_name", "result_condition", "result_condition_value", "sample_type", "collection_method", "sample_fraction", "result_speciation")
    } else {
      data <- data[, c("result", "target_datetime", "datetime", "param_name", "units", "location", "name", "result_condition", "result_condition_value", "sample_type", "collection_method", "sample_fraction", "result_speciation")]
      names(data) <- c("result", "target_datetime", "datetime", "param_name", "units", "location", "location_name", "result_condition", "result_condition_value", "sample_type", "collection_method", "sample_fraction", "result_speciation")
    }
  }
  
  #Plot the data ####################################################################################################
  
  if (log) {
    if (any(data[!is.na(data$result), "result"] <= 0)) {
      warning("Some values are <= 0 and cannot be log-transformed. These values will be removed to keep your requested log transformation.")
      data <- data[data$result > 0, ]
    }
  }
  
  create_facet_plot <- function(data, facet_by, targ_dt, loc_code) {
    # Split data based on the facet_by column
    df_list <- split(data, data[[facet_by]])

    color_by <- if (facet_by == "param_name") if (loc_code %in% c('code', 'codeName')) "location" else "location_name" else "param_name"


    # Define custom color scale
    if (colorblind) {
       palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")
    } else {
      # palette <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928")
      palette <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF", "#FFA500", "#800080", "#008000", "#000080", "#FFC0CB", "#808080")
    }
    
    custom_colors <- grDevices::colorRampPalette(palette)(length(unique(data[[color_by]])))
    
    # Create a plot for each facet
    plots <- lapply(seq_along(df_list), function(i) {
      facet_value <- names(df_list)[i]
      df <- df_list[[facet_value]]
      conditions <- df[is.na(df$result),] # Isolate the rows that are < DL or > DL
      df <- df[!is.na(df$result),]
      
      if (i == 1) {
        # Add entries for parameter/location_name which show up elsewhere in 'data' but not in facet 1
        missing <- setdiff(unique(data[[color_by]]), unique(df[[color_by]]))
        for (m in missing) {
          if (color_by %in% c("location", "location_name")) {
            unit_text <- if (nrow(df) == 0) unique(conditions$units) else unique(df$units)
            min_dt <- if (nrow(df) == 0) min(conditions$datetime) else min(df$datetime)
            to_bind <- data.frame(result = -Inf, datetime = min_dt, param_name = NA, units = unit_text, location = m, location_name = m, result_condition = NA, result_condition_value = NA)
            df <- dplyr::bind_rows(df, to_bind)  # used instead of rbind because it automatically adds columns with NA values
          } else {
            unit_text <- unique(data[data$param_name == m, "units"])
            loc_text <- unique(data[data$location_name == facet_value, "location"])
            to_bind <- data.frame(result = -Inf, datetime = min(df$datetime), param_name = m, units = unit_text, location = loc_text, location_name = facet_value, result_condition = NA, result_condition_value = NA)
            df <- dplyr::bind_rows(df, to_bind)
          }
        }
      }
      
      # Determine y-axis label based on parameter and units
      y_axis_label <- if (facet_by == "param_name") {
        paste(facet_value, " (", unique(df$units), ")", sep = "")
      } else {
        paste(unique(df$location_name))
      }
      
      # Determine what can be added to the hover labels
      type <- FALSE
      if ("sample_type" %in% names(df)) {
        if (any(!is.na(df$sample_type))) {
          type <- TRUE
          df$sample_type <- titleCase(df$sample_type, lang)
        }
      }
      collection <- FALSE
      if ("collection_method" %in% names(df)) {
        if (any(!is.na(df$collection_method))) {
          collection <- TRUE
          df$collection_method <- titleCase(df$collection_method, lang)
        }
      }
      fraction <- FALSE
      if ("sample_fraction" %in% names(df)) {
        if (any(!is.na(df$sample_fraction))) {
          fraction <- TRUE
          df$sample_fraction <- titleCase(df$sample_fraction, lang)
        }
      }
      speciation <- FALSE
      if ("result_speciation" %in% names(df)) {
        if (any(!is.na(df$result_speciation))) {
          speciation <- TRUE
          df$result_speciation <- titleCase(df$result_speciation, lang)
        }
      }
      if (targ_dt) {
        if (!"target_datetime" %in% names(df)) {
          targ_dt <- FALSE
        }
      }
      
      p <- plotly::plot_ly(df, 
                           x = ~datetime, 
                           y = ~result, 
                           type = 'scatter',
                           mode = 'markers',
                           color = ~get(color_by),
                           colors = custom_colors,
                           legendgroup = ~get(color_by),
                           showlegend = (i == 1),
                           marker = list(
                             opacity = ifelse(all(df$result == -Inf), 0, 1),
                             symbol = "circle",
                             size = point_scale * 7,
                             line = list(width = 0.2, color = grDevices::rgb(0, 0, 0))
                           ),
                           hoverinfo = "text",
                           text = ~paste(get(color_by), "<br>",  # Name or parameter of trace
                                         datetime, "<br>",  # Datetime
                                         if (targ_dt) paste("True sample datetime:", target_datetime, "<br>"),  # true sample datetime if requested and dbSource = 'AC'
                                         as.character(result), units, # result and units
                                         if (type) paste("<br>Sample type:", sample_type),  # Sample type if provided
                                         if (collection) paste("<br>Collection method:", collection_method),  # Collection method if provided
                                         if (fraction) paste("<br>Sample fraction:", sample_fraction),  # Sample fraction if provided
                                         if (speciation) paste("<br>Result speciation", result_speciation)  # Result speciation if provided
                           )
      ) %>%
        plotly::layout(title = NULL,
                       yaxis = list(title = y_axis_label,
                                    showline = TRUE,
                                    showgrid = gridy,
                                    zeroline = FALSE,
                                    type = if (log) "log" else "linear",
                                    titlefont = list(size = axis_scale * 16),
                                    tickfont = list(size = axis_scale * 14)
                                    ),
                       xaxis = list(title = NULL,
                                    showline = TRUE,
                                    showgrid = gridx,
                                    tickfont = list(size = axis_scale * 14)),
                       legend = list(font = list(size = legend_scale * 14),
                                     orientation = legend_position
                                     ),
                       font = list(family = "DejaVu Sans")
                       )
      
      if (nrow(conditions) > 0) {
        p <- plotly::add_trace(p, 
                               data = conditions, 
                               x = ~datetime, 
                               y = ~result_condition_value, 
                               type = 'scatter', 
                               mode = 'markers', 
                               color = ~get(color_by), 
                               colors = custom_colors, 
                               marker = list(opacity = 1, 
                                             # symbol = "circle-open-dot", 
                                             symbol = "star-open-dot",
                                             size = point_scale * 7,
                                             line = list(width = 1, color = NULL)
                                             ), 
                               showlegend = FALSE,
                               hoverinfo = "text",
                               text = ~paste(get(color_by), 
                                             "<br>", # Name or parameter of trace
                                             datetime, 
                                             "<br>", # Datetime
                                             if (targ_dt) paste("True sample datetime", 
                                                                target_datetime, "<br>"), # true sample datetime if requested and dbSource = 'AC'
                                             result_condition, 
                                             "of", 
                                             as.character(result_condition_value), 
                                             units, # Result condition and result
                                             if (type) paste("<br>Sample type:", 
                                                             sample_type),  # Sample type if provided
                                             if (collection) paste("<br>Collection method:", 
                                                                   collection_method),  # Collection method if provided
                                             if (fraction) paste("<br>Sample fraction:", 
                                                                 sample_fraction),  # Sample fraction if provided
                                             if (speciation) paste("<br>Result speciation", 
                                                                   result_speciation)  # Result speciation if provided
                               )
        )
      }
      
      # Now add points (actually lines) for the standard values where applicable
      if (!is.null(standard)) {
        if (length(df$std_max[!is.na(df$std_max)]) > 1) {
          p <- plotly::add_trace(p,
                                 data = df,
                                 x = ~datetime,
                                 y = ~std_max,
                                 type = 'scatter',
                                 mode = 'markers',
                                 color = ~get(color_by),
                                 colors = custom_colors,
                                 marker = list(opacity = 1,
                                               symbol = "line-ew",
                                               size = guideline_scale * 10,  # Controls line length
                                               line = list(width = guideline_scale * 2,
                                                           color = NULL
                                                           )
                                               ), # controls the actual line width and clor
                                 showlegend = FALSE,
                                 hoverinfo = 'text',
                                 text = ~paste(get(color_by), 
                                               "<br>", # Name or parameter of trace,
                                               datetime, 
                                               "<br>", # Datetime
                                               "Standard Max:", 
                                               round(std_max, 6), units)
          )
        }
        if (length(df$std_min[!is.na(df$std_min)]) > 1) {
          p <- plotly::add_trace(p,
                                 data = df,
                                 x = ~datetime,
                                 y = ~std_min,
                                 type = 'scatter',
                                 mode = 'markers',
                                 color = ~get(color_by),
                                 colors = custom_colors,
                                 marker = list(opacity = 1,
                                               symbol = "line-ew",
                                               size = guideline_scale * 10,
                                               line = list(width = guideline_scale * 2,
                                                           color = NULL
                                                           )
                                               ),
                                 showlegend = FALSE,
                                 hoverinfo = 'text',
                                 text = ~paste(get(color_by), "<br>", # Name or parameter of trace,
                                               datetime, "<br>", # Datetime
                                               "Standard Min:", round(std_min, 6), units)
          )
        }
      }

      return(p)
    })
    
    # Combine plots into a single subplot
    if (rows == "auto") {
      nrows <- ceiling(sqrt(length(plots)))
      # Determine how many columns this represents, given the nrows and the number of plots
      ncols <- ceiling(length(plots) / nrows)
    } else {
      nrows <- min(rows, length(plots))
      ncols <- ceiling(length(plots) / nrows)
    }
    
    # Plot widths/heights are a bit buggy. The issue seems to be that the margins are added/subtracted from plot dimensions after the heights/widths are set. If margins are the same everywhere though it works out! Still not quite perfect, but at least a bit better.
    # margins <-  c(0, (0.08 * axis_scale), 0, (0.08 * axis_scale)) # left, right, top, bottom
    margins <- 0.04 * axis_scale
    

    # Apply the layout settings to the final plot
    final_plot <- plotly::subplot(plots,
                                  nrows = nrows,
                                  shareX = FALSE,
                                  shareY = FALSE,
                                  titleX = FALSE,
                                  titleY = TRUE,
                                  margin = margins) %>%
       plotly::layout(showlegend = TRUE,
                      font = list(family = "DejaVu Sans"))
    
    # Link axes if desired
    if (shareX || shareY) {
      layout_settings <- list()
      
      if (shareX) {
        for (i in seq_along(plots)) {
          layout_settings[[paste0("xaxis", if (i > 1) i else "")]] <- list(matches = "x")
        }
      }
      
      if (shareY) {
        for (i in seq_along(plots)) {
          layout_settings[[paste0("yaxis", if (i > 1) i else "")]] <- list(matches = "y")
        }
      }
      
      final_plot <- do.call(plotly::layout, c(list(final_plot), layout_settings))
    }

    return(final_plot)
  } # End of plot creation function
  
  facet_by <- if (facet_on == "params") "param_name" else "location_name" # key to the correct column name

  if (loc_code == 'code') {
    data$location_name <- data$location
  } else if (loc_code == 'name') {
    data$location_name <- titleCase(data$location_name, lang)
  } else if (loc_code == 'codeName') {
    data$location_name <- paste(data$location, " (", titleCase(data$location_name, lang), ")", sep = "")
  } else if (loc_code == 'nameCode') {
    data$location_name <- paste(titleCase(data$location_name, lang), " (", data$location, ")", sep = "")
  }

  data <- data[order(data$location_name),]
    
  plot <- create_facet_plot(data, facet_by, targ_dt = target_datetime, loc_code = loc_code)

  # Return the plot and data if requested ##########################
  if (return_data) {
    return(list(plot = plot, data = data))
  } else {
    return(plot)
  }
  
} # End of plotDiscrete function

