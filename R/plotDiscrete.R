#' Discrete (lab or field point data) plotting function
#' 
#' Plots data directly from EQWin or from the AquaCache for one or more location (station) and one or more parameter. Depending on the setting for argument 'facet_on', the function can either make a facet plot where each station is a facet (with parameters as distinct traces) or where each parameter is a facet (with locations as distinct traces). Values above or below the detection limit are shown as the detection limit but symbolized differently (open circles). 
#' 
#' @param start The date to fetch data from, passed as a Date, POSIXct, or character vector of form 'yyyy-mm-dd HH:MM'. Dates and character vectors are converted to POSIXct with timezone 'MST'.
#' @param end The end date to fetch data up to, passed as a Date, POSIXct, or character vector of form 'yyyy-mm-dd HH:MM'. Dates and character vectors are converted to POSIXct with timezone 'MST'. Default is the current date.
#' @param locations A vector of station names or codes. If dbSource == 'AC': from AquaCache 'locations' table use column 'location', 'name', or 'name_fr' (character vector) or 'location_id' (numeric vector). If dbSource == 'EQ' use EQWiN 'eqstns' table, column 'StnCode' or leave NULL to use `locGrp` instead.
#' @param locGrp Only used if `dbSource` is 'EQ'. A station group as listed in the EWQin 'eqgroups' table, column 'groupname.' Leave NULL to use `locations` instead.
#' @param parameters A vector of parameter names or codes. If dbSource == 'AC': from AquaCache 'parameters' table use column 'param_name' or 'param_name_fr' (character vector) or 'parameter_id' (numeric vector). If dbSource == 'EQ' use EQWin 'eqparams' table, column 'ParamCode' or leave NULL to use `paramGrp` instead.
#' @param paramGrp Only used if `dbSource` is 'EQ'. A parameter group as listed in the EQWin 'eqgroups' table, column 'groupname.' Leave NULL to use `parameters` instead.
#' @param log Should the y-axis be log-transformed?
#' @param facet_on Should the plot be faceted by locations or by parameters? Specify one of 'locs' or 'params'. Default is 'locs'.
#' @param loc_code Should the location code be used instead of the full location name?
#' @param rows The number of rows to use in the facet grid. Default is 'auto' to automatically determine the number of rows based on the number of facets.
#' @param target_datetime Should the plot datetime use the 'target' datetime instead of the 'actual' datetime? Default is TRUE. This is only applicable is dbSource == 'AC'.
#' @param colorblind Should the plot be colorblind-friendly? Default is FALSE.
#' @param lang The language to use for the plot. Currently only "en" and "fr" are supported. Default is "en", and this is only supported for dbSource == 'AC'.
#' @param dbSource The database source to use, 'AC' for AquaCache or 'EQ' for EQWin. Default is 'EQ'. Connections to AquaCache are made using function [AquaConnect()].
#' @param dbPath The path to the EQWin database, if called for in parameter `dbSource`. Default is "//carver/infosys/EQWin/WR/DB/Water Resources.mdb".
#'
#' @return A zoomable plot of the data from EQWin.
#' @export
#'

plotDiscrete <- function(start, end = Sys.Date() + 1, locations = NULL, locGrp = NULL, parameters = NULL, paramGrp = NULL, log = FALSE, facet_on = 'params', loc_code = FALSE, rows = 'auto', target_datetime = TRUE, colorblind = FALSE, lang = "en", dbSource = "EQ", dbPath = "//carver/infosys/EQWin/WR/DB/Water Resources.mdb") {
  
  #TODO: Create workflow for dbSource = 'AC'. parameters and locations can be character or numeric for best operation with Shiny and directly from function.

  # testing parameters for EQWIN direct
  # start <- "2024-07-09"
  # end <- "2024-08-28"
  # locations <- NULL
  # parameters <- NULL
  # locGrp <- "QZ Eagle Gold HLF"
  # paramGrp <- "EG-HLF-failure"
  # log = FALSE
  # facet_on = 'params'
  # rows = 'auto'
  # colorblind = FALSE
  # target_datetime = FALSE
  # dbSource = "EQ"
  # lang = "en"
  # dbPath = "//carver/infosys/EQWin/WR/DB/Water Resources.mdb"
  
  # start <- "2024-07-09"
  # end <- "2024-08-28"
  # locations <- "(EG)W4"
  # parameters <- NULL
  # locGrp <- NULL
  # paramGrp <- "EG-HLF-failure"
  # log = FALSE
  # facet_on = 'locs'
  # rows = 'auto'
  # colorblind = FALSE
  # target_datetime = FALSE
  # dbSource = "EQ"
  # lang = "en"
  # dbPath = "//carver/infosys/EQWin/WR/DB/Water Resources.mdb"
  
  # testing parameters for AquaCache
  # start <- "2020-01-01"
  # end <- "2024-05-05"
  # locations <- c("09AD-SC01", "08AA-SC01", "09AK-SC01", "09DC-SC01B")
  # parameters <- c("snow water equivalent", "snow depth")
  # locGrp <- NULL
  # paramGrp <- NULL
  # log = FALSE
  # facet_on = 'locs'
  # rows = 'auto'
  # colorblind = FALSE
  # lang = "en"
  # dbSource = "AC"
  # dbPath = "//carver/infosys/EQWin/WR/DB/Water Resources.mdb"
  # target_datetime = TRUE

  # initial checks, connection, and validations #######################################################################################
  
  if (!dbSource %in% c("AC", "EQ")) stop("dbSource must be either 'AC' or 'EQ'")
  
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
  
  
  # Create a data.frame to hold the plotting data. EQWin and AquaCache fill this list in differently, but the end result is the same to pass on to the plotting portion.
  # 'data' should contain columns named location, location_name, parameter, datetime, value, result_condition (e.g. <DL, >DL, etc.), result_condition_value (the detection limit value), units. Optional columns (used by AquaCache) are sample_type, collection_method, sample_fraction, result_speciation.
  data <- data.frame()
  
  # Fetch the data ##############################################################################################################
  if (dbSource == "EQ") {
    ## Fetch data from EQWin #####################################################################################################
    # Connect to EQWin
    EQWin <- AccessConnect(dbPath, silent = TRUE)
    on.exit(DBI::dbDisconnect(EQWin), add = TRUE)
    
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
    data <- data[, -which(names(data) %in% c("SampleId", "ParamId", "StnId"))]  # Drop unnecessary column
    names(data) <- c("value", "datetime", "param_name", "units", "location", "location_name")
    
    # Now add the result_condition and result_condition_value columns
    # Sometimes the "." is a "," in the result, so we need to replace it
    data$value <- gsub(",", ".", data$value)
    #result_condition should get < DL, > DL, or NA depending on if '<' or '>' show up in columns 'value'
    data$result_condition <- ifelse(grepl("<", data$value), "< DL", ifelse(grepl(">", data$value), "> DL", NA))
    #result_condition_value should get the numeric portion of the string in 'value' only if '<' or '>' show up in columns 'value'
    data$result_condition_value <- ifelse(grepl("<", data$value), as.numeric(gsub("<", "", data$value)), ifelse(grepl(">", data$value), as.numeric(gsub(">", "", data$value)), NA))
    # turn column 'value' to a numeric, which will remove the '<' and '>' characters
    data$value <- suppressWarnings(as.numeric(data$value))
    
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
    
  } else { # dbSource == "AC"
    
    # Fetch data from AquaCache ##################################################################################################
    # Connect to AC
    AC <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(AC), add = TRUE)
    
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
        stop("No locations found in the AquaCache database with the names '", paste0(locations, collapse = "', '"), "'")
      }
      if (nrow(locIds) < length(locations)) {
        # Find the missing locations and tell the user which ones are missing
        combined_locIds <- unique(c(locIds$location_id, locIds$location, locIds$name, locIds$name_fr))
        missing <- setdiff(locations, combined_locIds)
        if (inherits(locations, "character")) {
          warning("The following locations were not found in the AquaCache database despite searching the 'location', 'name', and 'name_fr' columns: ", paste0(missing, collapse = ", "))
        } else {
          warning("The following locations were not found in the AquaCache database: ", paste0(missing, collapse = ", "))
        }
      }
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
        stop("No parameters found in the AquaCache database with the names '", paste0(parameters, collapse = "', '"), "'")
      }
      if (nrow(paramIds) < length(parameters)) {
        # Find the missing parameters and tell the user which ones are missing
        combined_paramIds <- unique(c(paramIds$parameter_id, paramIds$param_name, paramIds$param_name_fr))
        missing <- setdiff(parameters, combined_paramIds)
        if (inherits(parameters, "character")) {
          warning("The following parameters were not found in the AquaCache database despite searching the 'param_name' and 'param_name_fr' columns: ", paste0(missing, collapse = ", "))
        } else {
          warning("The following parameters were not found in the AquaCache database: ", paste0(missing, collapse = ", "))
        }
      }
    }
    
    tsids <- DBI::dbGetQuery(AC, paste0("SELECT location_id, parameter_id AS param_id, timeseries_id FROM timeseries WHERE location_id IN (", paste0(locIds$location_id, collapse = ", "), ") AND parameter_id IN (", paste0(paramIds$parameter_id, collapse = ", "), ");"))
    
    if (nrow(tsids) == 0) {
      stop("No timeseries were found matching your requested locations and parameters.")
    }
    
    # Merge the location and parameter names into the tsids data.frame
    tsids <- merge(tsids, locIds)
    tsids <- merge(tsids, paramIds, by.x = "param_id", by.y = "parameter_id")
    tsids <- tsids[, -which(names(tsids) %in% c("location_id", "param_id"))] # drop unnecessary columns
    
    # Get the measurements from table measurements_discrete
    query <- paste0("
    SELECT 
        m.timeseries_id, 
        m.target_datetime, 
        m.datetime, 
        m.value, 
        m.result_condition, 
        m.result_condition_value, 
        st.sample_type, 
        cm.collection_method, 
        sf.sample_fraction, 
        rs.result_speciation 
    FROM 
        measurements_discrete AS m
    LEFT JOIN 
        sample_types AS st ON m.sample_type = st.sample_type_id
    LEFT JOIN 
        collection_methods AS cm ON m.collection_method = cm.collection_method_id
    LEFT JOIN 
        sample_fractions AS sf ON m.sample_fraction = sf.sample_fraction_id
    LEFT JOIN 
        result_speciations AS rs ON m.result_speciation = rs.result_speciation_id
    WHERE 
        m.timeseries_id IN (", paste0(tsids$timeseries_id, collapse = ", "), ") 
        AND m.datetime > '", as.character(start), "' 
        AND m.datetime < '", as.character(end), "';
")
    
    results <- DBI::dbGetQuery(AC, query)
    
    if (nrow(results) == 0) {
      stop("No results were found for the date range locations/parameter combinations specified.")
    }
    
    #Swap the datetime columns if target_datetime is TRUE
    if (target_datetime) {
      names(results)[names(results) == "datetime"] <- "actual_datetime"
      names(results)[names(results) == "target_datetime"] <- "datetime"
      names(results)[names(results) == "actual_datetime"] <- "target_datetime"
    } 
    
    # Merge the results with the tsids data.frame
    data <- merge(results, tsids)
    
    # Now make result_condition column understandable
    #result_condition should get < DL, > DL, or NA depending on if 1 or 2 show up in column 'result_condition'
    data$result_condition <- ifelse(grepl("1", data$result_condition), "< DL", ifelse(grepl("2", data$result_condition), "> DL", NA))
    
    # Retain columns depending on if 'fr' or 'en', rename cols to match EQWin output
    if (lang == "fr") {
      data <- data[, c("value", "target_datetime", "datetime", "param_name_fr", "units", "location", "name_fr", "result_condition", "result_condition_value", "sample_type", "collection_method", "sample_fraction", "result_speciation")]
      names(data) <- c("value", "target_datetime", "datetime", "param_name", "units", "location", "location_name", "result_condition", "result_condition_value", "sample_type", "collection_method", "sample_fraction", "result_speciation")
    } else {
      data <- data[, c("value", "target_datetime", "datetime", "param_name", "units", "location", "name", "result_condition", "result_condition_value", "sample_type", "collection_method", "sample_fraction", "result_speciation")]
      names(data) <- c("value", "target_datetime", "datetime", "param_name", "units", "location", "location_name", "result_condition", "result_condition_value", "sample_type", "collection_method", "sample_fraction", "result_speciation")
    }
  }
  
  #Plot the data ####################################################################################################
  
  if (log) {
    if (any(data[!is.na(data$value), "value"] <= 0)) {
      warning("Some values are <= 0 and cannot be log-transformed. These values will be removed to keep your requested log transformation.")
      data <- data[data$value > 0, ]
    }
  }
  
  create_facet_plot <- function(data, facet_by, targ_dt, loc_code) {
    # Split data based on the facet_by column
    df_list <- split(data, data[[facet_by]])

    color_by <- if (facet_by == "param_name") if (loc_code) "location" else "location_name" else "param_name"


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
      conditions <- df[is.na(df$value),] # Isolate the rows that are < DL or > DL
      df <- df[!is.na(df$value),]
      
      if (i == 1) {
        # Add entries for parameter/location_name which show up elsewhere in 'data' but not in facet 1
        missing <- setdiff(unique(data[[color_by]]), unique(df[[color_by]]))
        for (m in missing) {
          if (color_by %in% c("location", "location_name")) {
            unit_text <- unique(df$units)
            df <- rbind(df, data.frame(value = -Inf, datetime = min(df$datetime), param_name = NA, units = unit_text, location = m, location_name = m, result_condition = NA, result_condition_value = NA))
          } else {
            unit_text <- unique(data[data$param_name == m, "units"])
            loc_text <- unique(data[data$location_name == facet_value, "location"])
            df <- rbind(df, data.frame(value = -Inf, datetime = min(df$datetime), param_name = m, units = unit_text, location = loc_text, location_name = facet_value, result_condition = NA, result_condition_value = NA))
          }
        }
      }
      
      # Determine y-axis label based on parameter and units
      y_axis_label <- if (facet_by == "param_name") {
        paste(facet_value, " (", unique(df$units), ")", sep = "")
      } else {
        if (loc_code) {
          paste(unique(df$location))
        } else {
          paste(unique(df$location_name))
        }
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
      
      # Determine appropriate number of decimal places
      # Currently not in use!
      # decimals <- df$value %% 1 |>
      #   as.character()
      # # Check if there's anything to the right of the decimal point
      # if (any(grepl("\\.", decimals))) {
      #   decimals <- sub(".*\\.", "", decimals)
      #   decimals <- max(nchar(decimals))
      # } else {
      #   decimals <- 0
      # }
      # sprintDecimals <- paste0("%.", decimals, "f")
      
      p <- plotly::plot_ly(df, 
                           x = ~datetime, 
                           y = ~value, 
                           type = 'scatter',
                           mode = 'markers',
                           color = ~get(color_by),
                           colors = custom_colors,
                           legendgroup = ~get(color_by),
                           showlegend = (i == 1),
                           marker = list(
                             opacity = ifelse(all(df$value == -Inf), 0, 1)
                           ),
                           hoverinfo = "text",
                           text = ~paste(get(color_by), "<br>",  # Name or parameter of trace
                                         datetime, "<br>",  # Datetime
                                         if (targ_dt) paste("True sample datetime:", target_datetime, "<br>"),  # true sample datetime if requested and dbSource = 'AC'
                                         as.character(value), units, # Value and units
                                         if (type) paste("<br>Sample type:", sample_type),  # Sample type if provided
                                         if (collection) paste("<br>Collection method:", collection_method),  # Collection method if provided
                                         if (fraction) paste("<br>Sample fraction:", sample_fraction),  # Sample fraction if provided
                                         if (speciation) paste("<br>Result speciation", result_speciation)  # Result speciation if provided
                           )
      ) %>%
        plotly::layout(title = NULL,
                       yaxis = list(title = y_axis_label, 
                                    type = if (log) "log" else "linear"),
                       xaxis = list(title = NULL))
      
      if (nrow(conditions) > 0) {
        p <- plotly::add_trace(p, 
                               data = conditions, 
                               x = ~datetime, 
                               y = ~result_condition_value, 
                               type = 'scatter', 
                               mode = 'markers', 
                               color = ~get(color_by), 
                               colors = custom_colors, 
                               marker = list(opacity = 0.8, 
                                             symbol = "circle-open", 
                                             size = 5), 
                               showlegend = FALSE,
                               hoverinfo = "text",
                               text = ~paste(get(color_by), "<br>", # Name or parameter of trace
                                             datetime, "<br>", # Datetime
                                             if (targ_dt) paste("True sample datetime", target_datetime, "<br>"), # true sample datetime if requested and dbSource = 'AC'
                                             result_condition, "of", as.character(result_condition_value), units, # Result condition and value
                                             if (type) paste("<br>Sample type:", sample_type),  # Sample type if provided
                                             if (collection) paste("<br>Collection method:", collection_method),  # Collection method if provided
                                             if (fraction) paste("<br>Sample fraction:", sample_fraction),  # Sample fraction if provided
                                             if (speciation) paste("<br>Result speciation", result_speciation)  # Result speciation if provided
                               )
        )
      }
      
      return(p)
    })
    
    # Combine plots into a single subplot
    if (rows == "auto") {
      nrows <- ceiling(sqrt(length(plots)))
    } else {
      nrows <- rows
    }
    
    plotly::subplot(plots, nrows = nrows, shareX = FALSE, titleX = FALSE, titleY = TRUE) %>%
      plotly::layout(showlegend = TRUE)
  } # End of plot creation function
  
  facet_by <- if (facet_on == "params") "param_name" else "location_name" # key to the correct column name
  data$param_name <- titleCase(data$param_name, lang)
  data$location_name <- titleCase(data$location_name, lang)
    
  plot <- create_facet_plot(data, facet_by, targ_dt = target_datetime, loc_code = loc_code)
  plot
  
  return(plot)
  
}

