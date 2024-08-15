# Function to plot directly from EQWin database. Eventually this function may be expanded to plot from AquaCache OR EQWin, TBD.

#' EQWin plotting function
#' 
#' Plots data directly from EQWin for one or more location (station) and one or more parameter. Depending on the setting for argument 'facet_on', the function can either make a facet plot where each station is a facet (with parameters as distinct traces) or where each parameter is a facet (with stations as distinct traces). Values above or below the detection limit are shown as the detection limit but symbolized differently. Duplicate samples are not shown.
#'
#' @param start The date to fetch data from, passed as a Date, POSIXct, or character vector of form 'yyyy-mm-dd HH:MM'. Dates and character vectors are converted to POSIXct with timezone 'MST'.
#' @param end The end date to fetch data up to, passed as a Date, POSIXct, or character vector of form 'yyyy-mm-dd HH:MM'. Dates and character vectors are converted to POSIXct with timezone 'MST'. Default is the current date.
#' @param stations A vector of station names as listed in the EQWiN eqstns table, column StnCode. Leave NULL to use stnGrp instead.
#' @param stnGrp A station group as listed in the EWQin eqgroups table, column groupname. Leave NULL to use stations instead.
#' @param parameters A vector of parameter names as listed in the EQWin eqparams table, column ParamCode. Leave NULL to use paramGrp instead.
#' @param paramGrp A parameter group as listed in the EQWin eqgroups table, column groupname. Leave NULL to use parameters instead.
#' @param log Should the y-axis be log-transformed?
#' @param title Should the plot have a title?
#' @param facet_on Should the plot be faceted by stations or by parameters?
#' @param save_path The path to save the plot as an html file. Default is "choose" to allow user to select a folder interactively.
#' @param dbPath The path to the EQWin database. Default is "X:/EQWin/WR/DB/Water Resources.mdb".
#'
#' @return A zoomable plot of the data from EQWin.
#' @export
#'

EQWinPlot <- function(start, end = Sys.Date() + 1, stations = NULL, stnGrp = NULL, parameters = NULL, paramGrp = NULL, log = FALSE, title = TRUE, facet_on = 'stns', save_path = "choose", dbPath = "X:/EQWin/WR/DB/Water Resources.mdb") {
  

  # testing parameters
  # start <- "2024-01-01"
  # end <- "2024-08-06"
  # stations <- NULL
  # parameters <- NULL
  # stnGrp <- "QZ Eagle Gold HLF"
  # paramGrp <- "EG-HLF-failure"
  # log = FALSE
  # title = TRUE
  # facet_on = 'stns'
  # save_path <- "C:/Users/gtdelapl/Desktop"
  # dbPath = "X:/EQWin/WR/DB/Water Resources.mdb"

  # initial checks, connection, and validations #######################################################################################
  if (is.null(stations) & is.null(stnGrp)) stop("You must specify either stations or stnGrp")
  if (!is.null(stations) & !is.null(stnGrp)) stop("You must specify either stations or stnGrp, not both")
  if (is.null(parameters) & is.null(paramGrp)) stop("You must specify either parameters or paramGrp")
  if (!is.null(parameters) & !is.null(paramGrp)) stop("You must specify either parameters or paramGrp, not both")
  
  facet_on <- tolower(facet_on)
  if (!facet_on %in% c("stns", "params")) stop("facet_on must be either 'stns' or 'params'")
  
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
  
  
  
  
  # Fetch the data and prepare for plotting #######################################################################################
  sampleIds <- DBI::dbGetQuery(EQWin, paste0("SELECT eqsampls.StnId, eqsampls.SampleId, eqsampls.CollectDateTime, eqcodes.CodeDesc FROM eqsampls INNER JOIN eqcodes ON eqsampls.SampleClass = eqcodes.CodeValue WHERE eqcodes.CodeField = 'eqsampls.SampleClass' AND eqsampls.StnId IN (", paste0(StnIds, collapse = ", "), ") AND eqsampls.CollectDateTime > #", as.character(start), "# AND eqsampls.CollectDateTime < #", as.character(end), "# AND eqcodes.CodeValue <> 'D';"))
  
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


    datalist <- list()
    datalist[["data"]] <- merge(results[, c("SampleId", "ParamId", "Result")], samps_locs[, c("SampleId", "StnId", "CollectDateTime")])

    datalist[["locations"]] <- locations[locations$StnId %in% datalist[["data"]]$StnId, c("StnId", "StnName", "StnDesc")]
    datalist[["parameters"]] <- unique(results[, c("ParamId", "ParamName")])
  
  
}
