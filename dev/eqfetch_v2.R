#' Retrieve data from Water Resources database
#' Adapted fro GdLP PlotDiscrete function
#' 
#' Retrieves data directly from EQWin or from the aquacache for one or more location (stations) and one or more parameters.
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
#' @param dbSource The database source to use, 'AC' for aquacache or 'EQ' for EQWin. Default is 'EQ'. Connections to aquacache are made using function [AquaConnect()] while EQWin connections use [AccessConnect()].
#' @param dbCon A database connection object, optional. Leave NULL to create a new connection and have it closed automatically.
#' @param dbPath The path to the EQWin database, if called for in parameter `dbSource`. Default is "//env-fs/env-data/corp/water/Data/Databases_virtual_machines/
#' databases/EQWinDB/WaterResources.mdb".
#'
#' @return a df containing reuested data
#' @export
#'

eqfetch <- function(start, 
                         end = Sys.Date() + 1, 
                         locations = NULL, locGrp = NULL, 
                         sub_locations = NULL,
                         parameters = NULL, paramGrp = NULL, 
                         standard = NULL, 
                         log = FALSE, 
                         facet_on = 'params', 
                         loc_code = 'name',
                         dbSource = "EQ", 
                         dbCon = NULL,
                         dbPath = "//env-fs/env-data/corp/water/Data/Databases_virtual_machines/databases/EQWinDB/WaterResources.mdb") {
  
  # testing parameters for EQWIN direct
  start <- "1990-01-01"
  end <- Sys.Date()
  locations <- c("(EG)MW24-09", "(EG)MW24-10", "(EG)IROSA-2")
  sub_locations = NULL
  parameters <- c("Chlord_IC", "Chlord_C", "Chloride")
  locGrp <- NULL
  paramGrp <- NULL
  standard = NULL
  loc_code = 'name'
  dbSource = "EQ"
  dbCon = NULL
  lang = "en"
  dbPath = "//env-fs/env-data/corp/water/Data/Databases_virtual_machines/databases/EQWinDB/WaterResourcesEG.mdb"
  
  # initial checks, connection, and validations #######################################################################################
  
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
      standards <- DBI::dbGetQuery(EQWin, paste0("SELECT StdId, StdCode, StdFlag, StdDesc FROM eqstds WHERE StdCode IN ('", paste0(standard, collapse = "', '"), "')"))
      if (nrow(standards) == 0) {
        stop("No standards found in the EQWin database with the name '", standard, "'")
      }
    }
    
    # Fetch the station and/or parameter list if necessary (locGrp or paramGrp was specified)
    if (!is.null(locGrp)) {
      # Check if the group actually exists
      grp_count <- DBI::dbGetQuery(EQWin, paste0("SELECT COUNT(*) FROM eqgroups WHERE groupname = '", locGrp, "' AND dbtablename = 'eqstns'"))[1,1]
      if (grp_count == 0) {
        stop("The station group '", locGrp, "' does not exist in the EQWin database")
      } else if (grp_count > 1) {
        stop("There are multiple station groups with the name '", locGrp, "' in the EQWin database")
      } # otherwise proceed to fetch the locations
      
      StnIds <- DBI::dbGetQuery(EQWin, paste0("SELECT groupitems FROM eqgroups WHERE groupname = '", locGrp, "' AND dbtablename = 'eqstns'"))$groupitems
      StnIds <- strsplit(StnIds, ",")[[1]]
      if (length(StnIds) == 0) {
        stop("No locations found in the station group '", locGrp, "'")
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
      
      paramIds <- DBI::dbGetQuery(EQWin, paste0("SELECT groupitems FROM eqgroups WHERE groupname = '", paramGrp, "' AND dbtablename = 'eqparams'"))$groupitems
      paramIds <- strsplit(paramIds, ",")[[1]]
      if (length(paramIds) == 0) {
        stop("No parameters found in the parameter group '", paramGrp, "'")
      }
    }
    
    # Validate existence of parameters and/or locations
    if (!is.null(locations)) {
      StnIds <- DBI::dbGetQuery(EQWin, paste0("SELECT StnId, StnCode FROM eqstns WHERE StnCode IN ('", paste0(locations, collapse = "', '"), "')"))
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
      paramIds <- DBI::dbGetQuery(EQWin, paste0("SELECT ParamId, ParamCode FROM eqparams WHERE ParamCode IN ('", paste0(parameters, collapse = "', '"), "')"))
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
    
    sampleIds <- DBI::dbGetQuery(EQWin, paste0("SELECT eqsampls.StnId, eqsampls.SampleId, eqsampls.CollectDateTime FROM eqsampls INNER JOIN eqcodes ON eqsampls.SampleClass = eqcodes.CodeValue WHERE eqcodes.CodeField = 'eqsampls.SampleClass' AND eqsampls.StnId IN (", paste0(StnIds, collapse = ", "), ") AND eqsampls.CollectDateTime > #", as.character(start), "# AND eqsampls.CollectDateTime < #", as.character(end), "# AND eqcodes.CodeValue <> 'D';"))
    
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
        data$std_max[merged_df$idx[merged_df$std_type == "std_max"]] <- merged_df$result[merged_df$std_type == "std_max"]
        data$std_min[merged_df$idx[merged_df$std_type == "std_min"]] <- merged_df$result[merged_df$std_type == "std_min"]
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
        query <- paste0("SELECT location_id FROM locations WHERE location_id IN (", paste0(locations, collapse = ", "), ");")
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
          warning(paste0("The following sub-locations were not found in the aquacache despite searching the 'sub_location', 'name', and 'name_fr' columns of table 'sub_locations': ", paste0(missing, collapse = ", ")))
        } else {
          warning(paste0("The following sub-locations were not found in the aquacache table 'sub_locations': ", paste0(missing, collapse = ", ")))
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
        query <- paste0("SELECT parameter_id FROM parameters WHERE parameter_id IN (", paste0(parameters, collapse = ", "), ");")
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
          warning(paste0("The following parameters were not found in the aquacache despite searching the 'param_name' and 'param_name_fr' columns of table 'parameters': ", paste0(missing, collapse = ", ")))
        } else {
          warning(paste0("The following parameters were not found in the aquacache table 'parameters': ", paste0(missing, collapse = ", ")))
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
      samples$name <- ifelse(is.na(samples$sub_location_name_fr), samples$name, paste0(samples$name, " - ", samples$sub_location_name_fr))
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
  return(data)
}

