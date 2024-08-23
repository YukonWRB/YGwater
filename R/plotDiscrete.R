#' Discrete (lab or field point data) plotting function
#' 
#' Plots data directly from EQWin or from the AquaCache for one or more location (station) and one or more parameter. Depending on the setting for argument 'facet_on', the function can either make a facet plot where each station is a facet (with parameters as distinct traces) or where each parameter is a facet (with locations as distinct traces). Values above or below the detection limit are shown as the detection limit but symbolized differently (open circles). 
#' 
#' @param dbSource The database source to use, 'AC' for AquaCache or 'EQ' for EQWin. Default is 'EQ'. Connections to AquaCache are made using function [AquaConnect()].
#' @param start The date to fetch data from, passed as a Date, POSIXct, or character vector of form 'yyyy-mm-dd HH:MM'. Dates and character vectors are converted to POSIXct with timezone 'MST'.
#' @param end The end date to fetch data up to, passed as a Date, POSIXct, or character vector of form 'yyyy-mm-dd HH:MM'. Dates and character vectors are converted to POSIXct with timezone 'MST'. Default is the current date.
#' @param locations A vector of station names pr codes. If dbSource == 'AC': from AquaCache 'locations' table use column 'location' (character vector) or 'location_id' (numeric vector). If dbSource == 'EQ' use EQWiN 'eqstns' table, column 'StnCode' or leave NULL to use `locGrp` instead.
#' @param locGrp Only used if `dbSource` is 'EQ'. A station group as listed in the EWQin 'eqgroups' table, column 'groupname.' Leave NULL to use `locations` instead.
#' @param parameters A vector of parameter names or codes. If dbSource == 'AC': from AquaCache 'parameters' table use column 'param_name' (character vector) or 'param_code' (numeric vector). If dbSource == 'EQ' use EQWin 'eqparams' table, column 'ParamCode' or leave NULL to use `paramGrp` instead.
#' @param paramGrp Only used if `dbSource` is 'EQ'. A parameter group as listed in the EQWin 'eqgroups' table, column 'groupname.' Leave NULL to use `parameters` instead.
#' @param log Should the y-axis be log-transformed?
#' @param title Should the plot have a title?
#' @param facet_on Should the plot be faceted by locations or by parameters? Specify one of 'locs' or 'params'. Default is 'locs'.
#' @param rows The number of rows to use in the facet grid. Default is 'auto' to automatically determine the number of rows based on the number of facets.
#' @param save_path The path to save the plot as an html file. Default is "choose" to allow user to select a folder interactively.
#' @param dbPath The path to the EQWin database, if called for in parameter `dbSource`. Default is "//carver/infosys/EQWin/WR/DB/Water Resources.mdb".
#'
#' @return A zoomable plot of the data from EQWin.
#' @export
#'

plotDiscrete <- function(start, end = Sys.Date() + 1, locations = NULL, locGrp = NULL, parameters = NULL, paramGrp = NULL, log = FALSE, title = TRUE, facet_on = 'locs', rows = 'auto', save_path = "choose", dbSource = "EQ", dbPath = "//carver/infosys/EQWin/WR/DB/Water Resources.mdb") {
  

  # testing parameters
  start <- "2024-06-24"
  end <- "2024-08-20"
  locations <- NULL
  parameters <- NULL
  locGrp <- "QZ Eagle Gold HLF"
  paramGrp <- "EG-HLF-failure"
  log = FALSE
  title = TRUE
  facet_on = 'locs'
  rows = 'auto'
  save_path <- "C:/Users/gtdelapl/Desktop"
  dbSource = "EQ"
  dbPath = "//carver/infosys/EQWin/WR/DB/Water Resources.mdb"

  # initial checks, connection, and validations #######################################################################################
  if (dbSource == 'AC') {
    if (!is.null(locGrp)) {
      stop("Parameter 'locGrp' is only used when 'dbSource' is 'EQ'")
    }
    if (!is.null(paramGrp)) {
      stop("Parameter 'paramGrp' is only used when 'dbSource' is 'EQ'")
    }
  }
  if (is.null(locations) & is.null(locGrp)) stop("You must specify either locations or locGrp")
  if (!is.null(locations) & !is.null(locGrp)) stop("You must specify either locations or locGrp, not both")
  if (is.null(parameters) & is.null(paramGrp)) stop("You must specify either parameters or paramGrp")
  if (!is.null(parameters) & !is.null(paramGrp)) stop("You must specify either parameters or paramGrp, not both")
  
  if (!dbSource %in% c("AC", "EQ")) stop("dbSource must be either 'AC' or 'EQ'")
  if (dbSource == "EQ" & !file.exists(dbPath)) stop("The EQWin database path does not exist or you do not have the necessary privileges.")
  
  facet_on <- tolower(facet_on)
  if (!facet_on %in% c("locs", "params")) stop("facet_on must be either 'locs' or 'params'")
  
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
  
  
  # Create a list to hold the plotting data. EQWin and AquaCache fill this list in differently, but the end result is the same to pass on to the plotting portion.
  # 'data' should contain columns named location, location_name, parameter, datetime, value, result_condition (e.g. <DL, >DL, etc.), result_condition_value (the detection limit value), units.
  # 'locations' should be a vector of the unique 'location' and 'location_name' values show in 'data'
  # 'parameters' should be a vector of the unique 'parameter' and 'unit' values shown in 'data'
  datalist <- list(data = NA,
                   locations = NA,
                   parameters = NA
                   )
  
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
      
      ParamIds <- DBI::dbGetQuery(EQWin, paste0("SELECT groupitems FROM eqgroups WHERE groupname = '", paramGrp, "' AND dbtablename = 'eqparams'"))$groupitems
      ParamIds <- strsplit(ParamIds, ",")[[1]]
      if (length(ParamIds) == 0) {
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
    
    sampleIds <- DBI::dbGetQuery(EQWin, paste0("SELECT eqsampls.StnId, eqsampls.SampleId, eqsampls.CollectDateTime, eqcodes.CodeDesc FROM eqsampls INNER JOIN eqcodes ON eqsampls.SampleClass = eqcodes.CodeValue WHERE eqcodes.CodeField = 'eqsampls.SampleClass' AND eqsampls.StnId IN (", paste0(StnIds, collapse = ", "), ") AND eqsampls.CollectDateTime > #", as.character(start), "# AND eqsampls.CollectDateTime < #", as.character(end), "# AND eqcodes.CodeValue <> 'D';"))
    
    if (nrow(sampleIds) == 0) {
      stop("No samples found for the date range and locations specified.")
    }
    
    # Extract the first word from CodeDesc and format
    sampleIds$CodeDesc <- sapply(strsplit(sampleIds$CodeDesc, " "), `[`, 1)
    sampleIds$CodeDesc <- gsub(":", "", sampleIds$CodeDesc)
    sampleIds$CodeDesc <- gsub(",", "", sampleIds$CodeDesc)
    sampleIds$CodeDesc <- paste0("(", sampleIds$CodeDesc, ")")
    
    results <- DBI::dbGetQuery(EQWin, paste0("SELECT eqdetail.SampleId, eqdetail.ParamId, eqdetail.Result, eqparams.ParamCode, eqparams.ParamName FROM eqdetail INNER JOIN eqparams ON eqdetail.ParamId = eqparams.ParamId WHERE eqdetail.SampleId IN (", paste0(sampleIds$SampleId, collapse = ", "), ") AND eqdetail.ParamId IN (", paste0(ParamIds, collapse = ", "), ");"))
    
    
    params <- DBI::dbGetQuery(EQWin, paste0("SELECT ParamId, ParamName, Units FROM eqparams;"))
    
    samps <- sampleIds[sampleIds$SampleId %in% results$SampleId, ]
    locations <- DBI::dbGetQuery(EQWin, paste0("SELECT StnId, StnName, StnDesc FROM eqstns WHERE StnId IN (", paste0(samps$StnId, collapse = ", "), ");"))
    samps_locs <- merge(locations, samps)
    
    datalist[["data"]] <- merge(results[, c("SampleId", "ParamId", "Result")], samps_locs[, c("SampleId", "StnId", "CollectDateTime")])
    datalist[["data"]] <- merge(datalist[["data"]], params)
    datalist[["data"]] <- merge(datalist[["data"]], locations)
    datalist[["data"]] <- datalist[["data"]][, -which(names(datalist[["data"]]) %in% c("SampleId", "ParamId", "StnId"))]  # Drop unnecessary column
    names(datalist[["data"]]) <- c("value", "datetime", "parameter", "units", "location", "location_name")
    
    # Now add the result_condition and result_condition_value columns
    #result_condition should get < DL, > DL, or NA depending on if '<' or '>' show up in columns 'value'
    datalist[["data"]]$result_condition <- ifelse(grepl("<", datalist[["data"]]$value), "< DL", ifelse(grepl(">", datalist[["data"]]$value), "> DL", NA))
    #result_condition_value should get the numeric portion of the string in 'value' only if '<' or '>' show up in columns 'value'
    datalist[["data"]]$result_condition_value <- ifelse(grepl("<", datalist[["data"]]$value), as.numeric(gsub("<", "", datalist[["data"]]$value)), ifelse(grepl(">", datalist[["data"]]$value), as.numeric(gsub(">", "", datalist[["data"]]$value)), NA))
    # turn column 'value' to a numeric, which will remove the '<' and '>' characters
    datalist[["data"]]$value <- suppressWarnings(as.numeric(datalist[["data"]]$value))
    
    datalist[["locations"]] <- unique(datalist[["data"]][, c("location", "location_name")])
    datalist[["parameters"]] <- unique(datalist[["data"]][, c("parameter", "units")])
  } else { # dbSource == "AC"
    # Fetch data from AquaCache ##################################################################################################
    stop("AquaCache is not yet supported.")
    
  }
  
  #Plot the data ####################################################################################################
  
  create_facet_plot <- function(data, facet_by) {
    # Split data based on the facet_by column
    df_list <- split(data, data[[facet_by]])
    
    color_by <- if (facet_by == "parameter") "location_name" else "parameter"

    # Define custom color scale with sufficient colors
    custom_colors <- colorRampPalette(RColorBrewer::brewer.pal(12, "Paired"))(length(unique(data[[color_by]])))
    
    # Create a plot for each facet
    plots <- lapply(seq_along(df_list), function(i) {
      facet_value <- names(df_list)[i]
      df <- df_list[[facet_value]]
      conditions <- df[is.na(df$value),] # Isolate the rows that are < DL or > DL
      df <- df[!is.na(df$value),]
      unit_text <- unique(df$units)
      
      if (i == 1) {
        # Add entries for parameter/location_name which show up elsewhere in 'data' but not in facet 1
        missing <- setdiff(unique(data[[color_by]]), unique(df[[color_by]]))
        for (m in missing) {
          df <- rbind(df, data.frame(value = -Inf, datetime = min(df$datetime), parameter = facet_value, units = unit_text, location = NA, location_name = m, result_condition = NA, result_condition_value = NA))
        }
      }
      
      # Determine y-axis label based on parameter and units
      y_axis_label <- if (facet_by == "parameter") {
        paste(facet_value, " (", unit_text, ")", sep = "")
      } else {
        paste(unique(df$location_name))
      }
      
      # Create scatter plot with custom colors and symbols
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
                             opacity = ifelse(df$value == -Inf, 0, 1)
                           ),
                           text = if (color_by == "location_name") ~paste(location_name, "<br>", value, units) else ~paste(parameter, "<br>", value, units)
      ) %>%
        plotly::layout(title = NULL,
                       yaxis = list(title = y_axis_label),
                       xaxis = list(title = NULL))
      
      if (nrow(conditions) > 0) {
        p <- plotly::add_trace(p, data = conditions, x = ~datetime, y = ~result_condition_value, type = 'scatter', mode = 'markers', color = ~get(color_by), colors = custom_colors, marker = list(opacity = 0.8, symbol = "circle-open", size = 5), showlegend = FALSE, text = if (color_by == "location_name") ~paste(location_name, "<br>", result_condition, "of", sprintf("%.6f", result_condition_value), units) else ~paste(parameter, "<br>", result_condition, "of", sprintf("%.6f", result_condition_value), units))
      }
      
      return(p)
    })
    
    # Combine plots into a single subplot
    nrows <- floor(sqrt(length(plots)))
    plotly::subplot(plots, nrows = nrows, shareX = FALSE, titleX = FALSE, titleY = TRUE) %>%
      plotly::layout(showlegend = TRUE)
  }
  
  facet_by <- if (facet_on == "params") "parameter" else "location_name" # key to the correct column name
  
  plot <- create_facet_plot(datalist[["data"]], facet_by)
  
  return(plot)
  
  
}

