#' Tabular output of hydrometric data
#'
#' Creates a report of hydrometric, snow pack, and precipitation conditions in Excel format, each table on a separate tab. List of stations/locations can be user-defined if desired. Connection is established using hydrometConnect by default and MUST connect to a database created and maintained by the package HydroMetDB.
#' Note that data can only be as recent as the last incorporation to the database. If you need the most up to date data possible, run HydroMetDB::getNewContinuous first.
#'
#' @param con A connection to the database. Default uses function [hydrometConnect()] with default settings.
#' @param level_locations List of water level locations to include in the report, as a character vector. "default" is a pre-determined list of locations across the territory, "all" fetches all level reporting locations in the DB. NULL will not create the table.
#' @param flow_locations List of flow locations to include in the report, as a character vector. "default" is a pre-determined list of locations across the territory. "all" fetches all flow reporting locations in the DB. NULL will not create the table.
#' @param snow_locations List of snow pillow locations to include in the report, as a character vector. "default" includes all of the WRB snow pillows as of Feb 2023, "all" fetches all snow pillow locations in the DB. NULL will not create the table.
#' @param bridge_locations List of bridge freeboard radar locations to include in the report, as a character vector. "default" includes all of the radars as of Feb 2023, "all" fetches all snow pillow locations in the DB. NULL will not create the table.
#' @param precip_locations List of flow/level locations for which to report precipitation. "default" is a pre-determined list of locations, "all" is all locations for which there is a drainage polygon (which may be more or less than the number of stations reporting level or flow information). NULL will not create the table. WARNING: this portion of the script is slow. Setting this parameter to "all" could take about an hour to get all information together.
#' @param past The number of days in the past for which you want data. Will be rounded to yield table columns covering at least one week, at most 4 weeks. 24, 28, and 72 hour change columns are always rendered.
#' @param save_path The path where you wish to save the Excel workbook. A folder will be created for each day's report.
#' @param archive_path The path to yesterday's file, if you wish to include yesterday's comments in this report. Full path, including exension .xlsx. Function expects a workbook exactly as produced by this function, plus of course the observer comments.
#'
#' @return An Excel workbook containing the report with one tab per timeseries type.
#' @export


# TODO: Adapt to use new DB

tabularReport <- function(con = hydrometConnect(silent = TRUE), level_locations = "all", flow_locations = "all", snow_locations = "all", bridge_locations = "all", precip_locations = "default", past = 28, save_path = "choose", archive_path = "choose") {

  if (level_locations[1] == "default"){
    level_locations <- c("09AH001", "09AH004", "09EA003", "09EB001", "09DC006", "09FD003", "09BC001", "09BC002", "09AE002", "10AA001", "09AB001", "09AB004", "09AB010", "09AA004", "09AA017")
    level_locations <- DBI::dbGetQuery(con, paste0("SELECT location, timeseries_id FROM timeseries WHERE parameter = 'level' AND category = 'continuous' AND location IN ('", paste(level_locations, collapse = "', '"), "') ORDER BY location;"))
  } else if (level_locations[1] == "all"){
    level_locations <- DBI::dbGetQuery(con, "SELECT location, timeseries_id FROM timeseries WHERE parameter = 'level' AND category = 'continuous' ORDER BY location;")
  }
  if (flow_locations[1] == "default"){
    flow_locations <- c("09AH001", "09AH004", "09EA003", "09EB001", "09DC006", "09FD003", "09BC001", "09BC002", "09AE002", "10AA001", "09AB001", "09AB004", "09AB010", "09AA004", "09AA017")
    flow_locations <- DBI::dbGetQuery(con, paste0("SELECT location, timeseries_id FROM timeseries WHERE parameter = 'flow' AND category = 'continuous' AND location IN ('", paste(flow_locations, collapse = "', '"), "') ORDER BY location;"))
  } else if (flow_locations[1] == "all"){
    flow_locations <- DBI::dbGetQuery(con, "SELECT location, timeseries_id FROM timeseries WHERE parameter = 'flow' AND category = 'continuous' ORDER BY location;")
  }
  if (snow_locations[1] == "default"){
    snow_locations <- c("09AA-M1", "09BA-M7", "09DB-M1", "09EA-M1", "10AD-M2", "29AB-M3")
    snow_locations <- DBI::dbGetQuery(con, paste0("SELECT location, timeseries_id FROM timeseries WHERE parameter = 'SWE' AND category = 'continuous' AND location IN ('", paste(snow_locations, collapse = "', '"), "') ORDER BY location;"))
  } else if (snow_locations[1] == "all"){
    snow_locations <- DBI::dbGetQuery(con, "SELECT location, timeseries_id FROM timeseries WHERE parameter = 'SWE' AND category = 'continuous' ORDER BY location")
  }
  if (bridge_locations[1] == "default"){
    bridge_locations <- c("09AH005", "29AB010", "29AB011", "29AE007", "29AH001")
    bridge_locations <- DBI::dbGetQuery(con, paste0("SELECT location, timeseries_id FROM timeseries WHERE parameter = 'distance' AND category = 'continuous' AND location IN ('", paste(bridge_locations, collapse = "', '"), "') ORDER BY location;"))
  } else if (bridge_locations[1] == "all"){
    bridge_locations <- DBI::dbGetQuery(con, "SELECT location, timeseries_id FROM timeseries WHERE parameter = 'distance' AND category = 'continuous' ORDER BY location;")
  }
  if (precip_locations[1] == "default"){
    precip_locations <- c("08AA003", "08AA010", "08AB001", "09AA001", "09AA004", "09AA013", "09AB001", "09AB010", "09AC001", "09AE002", "09AH001", "09AH004", "09BC001", "09BC002", "09CA002", "09DC005", "09DC006", "09EA003", "09EB001", "09FC001", "09FD002", "10AA001", "10AD002", "10MA002", "10BE001")
    precip_locations <- DBI::dbGetQuery(con, paste0("SELECT location FROM timeseries WHERE parameter IN ('level', 'flow') AND category = 'continuous' AND location IN ('", paste(precip_locations, collapse = "', '"), "') ORDER BY location;"))[,1]
    precip_locations <- unique(precip_locations)
  } else if (precip_locations[1] == "all"){
    precip_locations <- DBI::dbGetQuery(con, "SELECT location FROM timeseries WHERE parameter IN ('level', 'flow') AND category = 'continuous' ORDER BY location;")[,1]
    precip_locations <- unique(precip_locations)
  }

  if (save_path == "choose") {
    message("Select the path to the folder where you want this report saved.")
    save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
  }
  if (archive_path == "choose"){
    message("Select the path to yesterday's file (refer to function help).")
    archive_path <- as.character(utils::choose.files(caption="Select Yesterday's File"), multi = FALSE)
  }

  #Set the days for which to generate tables
  if (past < 8){
    past <- 7
  }
  if (past >=8 & past < 15){
    past <- 14
  }
  if (past >= 15 & past < 22){
    past <- 21
  }
  if (past >= 22){
    past <- 28
  }

  #Load yesterday's workbook -----------------
  tryCatch({
    yesterday_workbook <- openxlsx::loadWorkbook(archive_path)
    yesterday <- list(yesterday_general = NULL, yesterday_locs = NULL)
    for (i in names(yesterday_workbook)){
      if (i != "precipitation"){
        yesterday[["yesterday_general"]][[i]] <- openxlsx::read.xlsx(yesterday_workbook, sheet = i, rows = 3, cols = 2, colNames = FALSE)
        yesterday[["yesterday_locs"]][[i]] <- openxlsx::read.xlsx(yesterday_workbook, sheet = i, startRow = 6)
      } else {
        yesterday[["yesterday_general"]][[i]] <- openxlsx::read.xlsx(yesterday_workbook, sheet = i, rows = 3, cols = 2, colNames = FALSE)
        yesterday[["yesterday_locs"]][[i]] <- openxlsx::read.xlsx(yesterday_workbook, sheet = i, startRow = 8)
      }

    }
    yesterday_comments <- TRUE
  }, error = function(e) {
    warning("Could not fetch information from yesterday's workbooks. Perhaps the file path you specified is incorrect; check the function help again.")
    yesterday_comments <- FALSE
  })


  #Get the data -------------------------
  tables <- list()
  if (!is.null(precip_locations)){ #This one is special: get the data and make the table at the same time, before other data as this is the time consuming step. This keeps the more important data more recent. Others get the data then process it later on.
    precip <- data.frame()
    for (i in precip_locations){
      name <- stringr::str_to_title(unique(DBI::dbGetQuery(con, paste0("SELECT name FROM locations WHERE location = '", i, "'"))))
      tryCatch({
        #TODO: Update code below to get polygons direct from the DB once basinPrecip is updated.
        lastWeek <- basinPrecip(location = i, drainage_loc = "\\\\env-fs/env-data/corp/water/Common_GW_SW/Data/database/polygons/watersheds/all_basins.shp", start = Sys.time()-60*60*24*7, end = Sys.time(), silent = TRUE, map = FALSE)
        lastThree <- basinPrecip(location = i, drainage_loc = "\\\\env-fs/env-data/corp/water/Common_GW_SW/Data/database/polygons/watersheds/all_basins.shp", start = Sys.time()-60*60*24*3, end = Sys.time(), silent = TRUE, map = FALSE)
        lastTwo <- basinPrecip(location = i, drainage_loc = "\\\\env-fs/env-data/corp/water/Common_GW_SW/Data/database/polygons/watersheds/all_basins.shp", start = Sys.time()-60*60*24*2, end = Sys.time(), silent = TRUE, map = FALSE)
        lastOne <- basinPrecip(location = i, drainage_loc = "\\\\env-fs/env-data/corp/water/Common_GW_SW/Data/database/polygons/watersheds/all_basins.shp", start = Sys.time()-60*60*24*1, end = Sys.time(), silent = TRUE, map = FALSE)
        next24 <- basinPrecip(location = i, drainage_loc = "\\\\env-fs/env-data/corp/water/Common_GW_SW/Data/database/polygons/watersheds/all_basins.shp", start = Sys.time(), end = Sys.time() + 60*60*24, silent = TRUE, map = FALSE)
        next48 <- basinPrecip(location = i, drainage_loc = "\\\\env-fs/env-data/corp/water/Common_GW_SW/Data/database/polygons/watersheds/all_basins.shp", start = Sys.time(), end = Sys.time() + 60*60*48, silent = TRUE, map = FALSE)
        yesterday_comment_precip <- if (yesterday_comments) yesterday$yesterday_locs$precipitation[yesterday$yesterday_locs$precipitation$Location == i, "Location.specific.comments"] else NA

        precip <- rbind(precip,
                        data.frame("loc" = i,
                                   "name" = name,
                                   "lastWeek" = round(lastWeek$mean_precip, 1),
                                   "lastThree" = round(lastThree$mean_precip, 1),
                                   "lastTwo" = round(lastTwo$mean_precip, 1),
                                   "lastOne" = round(lastOne$mean_precip, 1),
                                   "next24" = round(next24$mean_precip, 1),
                                   "next48"= round(next48$mean_precip, 1),
                                   "location_comments" = NA,
                                   "yesterday_comments" = if (length(yesterday_comment_precip) < 1 | is.null(yesterday_comment_precip)) NA else yesterday_comment_precip))
      }, error = function(e) {
        precip <<- rbind(precip,
                         data.frame("loc" = i,
                                    "name" = name,
                                    "lastWeek" = NA,
                                    "lastThree" = NA,
                                    "lastTwo" = NA,
                                    "lastOne" = NA,
                                    "next24" = NA,
                                    "next48"= NA,
                                    "location_comments" = "Failed to fetch precipitation for this station.",
                                    "yesterday_comments" = if (length(yesterday_comment_precip) < 1 | is.null(yesterday_comment_precip)) NA else yesterday_comment_precip))
      })
    }
    colnames(precip) <- c("Location", "Name", "past 7 days (mm)", "past 3 days (mm)", "past 2 days (mm)", "past 24 hrs (mm)", "next 24 hrs (mm)", "next 48 hrs (mm)", "Location specific comments", "Yesterday's comments")
    precip <- hablar::rationalize(precip)
    if (nrow(precip) > 0){
      tables$precipitation <- precip
    }
  } #End of precip fetch loop

  if (!is.null(level_locations)){
    level_daily <- list()
    level_rt <- list()
    names_level <- NULL
    for (i in 1:nrow(level_locations)){
      daily <- DBI::dbGetQuery(con, paste0("SELECT value, date, percent_historic_range, max, min, q50 FROM calculated_daily WHERE date = '", Sys.Date(), "' AND timeseries_id = ", level_locations[i, "timeseries_id"], ";"))
      if (nrow(daily) == 0){
        daily <- DBI::dbGetQuery(con, paste0("SELECT value, date, percent_historic_range, max, min, q50 FROM calculated_daily WHERE date = '", Sys.Date()-1, "'AND timeseries_id = ", level_locations[i, "timeseries_id"], ";"))
      }
      if (nrow(daily) > 0){
        level_daily[[level_locations[i, "location"]]] <- daily
      }
      rt <-  DBI::dbGetQuery(con, paste0("SELECT value, datetime FROM measurements_continuous WHERE timeseries_id = ", level_locations[i, "timeseries_id"], " AND datetime BETWEEN '", .POSIXct(Sys.time(), "UTC")-(past + 2) * 60*60*24, "' AND '", .POSIXct(Sys.time(), "UTC"), "'"))
      if (nrow(rt) > 0){
        level_rt[[level_locations[i, "location"]]] <- rt
      }
      if (nrow(rt) > 0 | nrow(daily) >0){
        names_level[level_locations[i, "location"]] <- stringr::str_to_title(unique(DBI::dbGetQuery(con, paste0("SELECT name FROM locations WHERE location = '", level_locations[i, "location"], "'"))))
      }
    }
  }

  if (!is.null(flow_locations)){
    flow_daily <- list()
    flow_rt <- list()
    names_flow <- NULL
    for (i in 1:nrow(flow_locations)){
      daily <- DBI::dbGetQuery(con, paste0("SELECT value, date, percent_historic_range, max, min, q50 FROM calculated_daily WHERE date = '", Sys.Date(), "' AND timeseries_id = ", flow_locations[i, "timeseries_id"], ";"))
      if (nrow(daily) == 0){
        daily <- DBI::dbGetQuery(con, paste0("SELECT value, date, percent_historic_range, max, min, q50 FROM calculated_daily WHERE date = '", Sys.Date()-1, "'AND timeseries_id = ", flow_locations[i, "timeseries_id"], ";"))
      }
      if (nrow(daily) > 0){
        flow_daily[[flow_locations[i, "location"]]] <- daily
      }
      rt <-  DBI::dbGetQuery(con, paste0("SELECT value, datetime FROM measurements_continuous WHERE timeseries_id = ", flow_locations[i, "timeseries_id"], " AND datetime BETWEEN '", .POSIXct(Sys.time(), "UTC")-(past + 2) * 60*60*24, "' AND '", .POSIXct(Sys.time(), "UTC"), "'"))
      if (nrow(rt) > 0){
        flow_rt[[flow_locations[i, "location"]]] <- rt
      }
      if (nrow(rt) > 0 | nrow(daily) >0){
        names_flow[flow_locations[i, "location"]] <- stringr::str_to_title(unique(DBI::dbGetQuery(con, paste0("SELECT name FROM locations WHERE location = '", flow_locations[i, "location"], "'"))))
      }
    }
  }
  if (!is.null(snow_locations)){
    snow_daily <- list()
    snow_rt <- list()
    names_snow <- NULL
    for (i in 1:nrow(snow_locations)){
      daily <- DBI::dbGetQuery(con, paste0("SELECT value, date, percent_historic_range, max, min, q50 FROM calculated_daily WHERE date = '", Sys.Date(), "' AND timeseries_id = ", snow_locations[i, "timeseries_id"], ";"))
      if (nrow(daily) == 0){
        daily <- DBI::dbGetQuery(con, paste0("SELECT value, date, percent_historic_range, max, min, q50 FROM calculated_daily WHERE date = '", Sys.Date()-1, "'AND timeseries_id = ", snow_locations[i, "timeseries_id"], ";"))
      }
      if (nrow(daily) > 0){
        snow_daily[[snow_locations[i, "location"]]] <- daily
      }
      rt <-  DBI::dbGetQuery(con, paste0("SELECT value, datetime FROM measurements_continuous WHERE timeseries_id = ", snow_locations[i, "timeseries_id"], " AND datetime BETWEEN '", .POSIXct(Sys.time(), "UTC")-(past + 2) * 60*60*24, "' AND '", .POSIXct(Sys.time(), "UTC"), "'"))
      if (nrow(rt) > 0){
        snow_rt[[snow_locations[i, "location"]]] <- rt
      }
      if (nrow(rt) > 0 | nrow(daily) >0){
        names_snow[snow_locations[i, "location"]] <- stringr::str_to_title(unique(DBI::dbGetQuery(con, paste0("SELECT name FROM locations WHERE location = '", snow_locations[i, "location"], "'"))))
      }
    }
  }
  if (!is.null(bridge_locations)){
    bridges_daily <- list()
    bridges_rt <- list()
    names_bridges <- NULL
    for (i in 1:nrow(bridge_locations)){
      daily <- DBI::dbGetQuery(con, paste0("SELECT value, date, percent_historic_range, max, min, q50 FROM calculated_daily WHERE date = '", Sys.Date(), "' AND timeseries_id = ", bridge_locations[i, "timeseries_id"], ";"))
      if (nrow(daily) == 0){
        daily <- DBI::dbGetQuery(con, paste0("SELECT value, date, percent_historic_range, max, min, q50 FROM calculated_daily WHERE date = '", Sys.Date()-1, "'AND timeseries_id = ", bridge_locations[i, "timeseries_id"], ";"))
      }
      if (nrow(daily) > 0){
        bridges_daily[[bridge_locations[i, "location"]]] <- daily
      }
      rt <-  DBI::dbGetQuery(con, paste0("SELECT value, datetime FROM measurements_continuous WHERE timeseries_id = ", bridge_locations[i, "timeseries_id"], " AND datetime BETWEEN '", .POSIXct(Sys.time(), "UTC")-(past + 2) * 60*60*24, "' AND '", .POSIXct(Sys.time(), "UTC"), "'"))
      if (nrow(rt) > 0){
        bridges_rt[[bridge_locations[i, "location"]]] <- rt
      }
      if (nrow(rt) > 0 | nrow(daily) >0){
        names_bridges[bridge_locations[i, "location"]] <- stringr::str_to_title(unique(DBI::dbGetQuery(con, paste0("SELECT name FROM locations WHERE location = '", bridge_locations[i, "location"], "'"))))
      }
    }
  }  #End of data acquisition



  #Make the remaining tables ----------------
  if (length(level_rt) > 0){ #generate level table
    levels <- data.frame()
    for (i in names(level_rt)){
      rt <- level_rt[[i]]
      last_time <- rt[rt$datetime == max(rt$datetime) ,]$datetime
      age <- difftime(Sys.time(), last_time, units = "hours")
      latest <- stats::median(rt[rt$datetime <= last_time & rt$datetime >= last_time - 60*30 , ]$value) #median of last 30 minutes of data
      percent_historic <- round(((latest - level_daily[[i]]$min) / (level_daily[[i]]$max - level_daily[[i]]$min)) * 100, 0)
      percent_mean <- round(((latest - level_daily[[i]]$min) / (level_daily[[i]]$q50 - level_daily[[i]]$min)) * 100, 0)
      day <- stats::median(rt[rt$datetime <= last_time - 60*60*24 & rt$datetime >= last_time - 60*60*24.5 , ]$value) #median of 30 minutes
      twoday <- stats::median(rt[rt$datetime <= last_time - 60*60*47.5 & rt$datetime >= last_time - 60*60*48.5 , ]$value) #median of 1 hour
      threeday <- stats::median(rt[rt$datetime <= last_time - 60*60*71.5 & rt$datetime >= last_time - 60*60*72.5 , ]$value) #median of 1 hour
      week <- stats::median(rt[rt$datetime <= last_time - 60*60*167 & rt$datetime >= last_time - 60*60*169 , ]$value) #median of 2 hours
      if (is.na(week)){ #expand the range if no data within the 2 hour timespan
        week <- stats::median(rt[rt$datetime <= last_time - 60*60*165 & rt$datetime >= last_time - 60*60*171 , ]$value)
      }
      yesterday_comment_levels <- if(yesterday_comments) yesterday$yesterday_locs$levels[yesterday$yesterday_locs$levels$Location == i, "Location.specific.comments"] else NA

      if (past <= 7){
        levels <- rbind(levels,
                        data.frame("loc" = i,
                                   "name" = names_level[i],
                                   "level" = if (!is.na(latest)) round(latest, 3) else NA,
                                   "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                   "mean" = if (length(percent_mean == 1)) percent_mean else NA,
                                   "24" = if (!is.na(day)) round((latest - day) * 100, 1) else NA,
                                   "48" = if (!is.na(twoday)) round((latest - twoday) * 100, 1) else NA,
                                   "72" = if (!is.na(threeday)) round((latest - threeday) * 100, 1) else NA,
                                   "week" = if (!is.na(week)) round((latest - week) * 100, 1) else NA,
                                   "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                   "Hrs" = as.numeric(paste0(round(age[1],1))),
                                   "location_comment" = NA,
                                   "yesterday_comments" = if (length(yesterday_comment_levels) < 1 | is.null(yesterday_comment_levels)) NA else yesterday_comment_levels
                        ))
      }
      if (past > 7 & past <= 14){
        twoweek <- stats::median(rt[rt$datetime <= last_time - 60*60*335 & rt$datetime >= last_time - 60*60*337 , ]$value)
        if (is.na(twoweek)){
          twoweek <- stats::median(rt[rt$datetime <= last_time - 60*60*331 & rt$datetime >= last_time - 60*60*341 , ]$value)
        }
        levels <- rbind(levels,
                        data.frame("loc" = i,
                                   "name" = names_level[i],
                                   "level" = if (!is.na(latest)) round(latest, 3) else NA,
                                   "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                   "mean" = if (length(percent_mean == 1)) percent_mean else NA,
                                   "24" = if (!is.na(day)) round((latest - day) * 100, 1) else NA,
                                   "48" = if (!is.na(twoday)) round((latest - twoday) * 100, 1) else NA,
                                   "72" = if (!is.na(threeday)) round((latest - threeday) * 100, 1) else NA,
                                   "week" = if (!is.na(week)) round((latest - week) * 100, 1) else NA,
                                   "twoweek" = if (!is.na(twoweek)) round((latest - twoweek) * 100, 1) else NA,
                                   "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                   "Hrs" = as.numeric(paste0(round(age[1],1))),
                                   "location_comment" = NA,
                                   "yesterday_comments" = yesterday_comment_levels
                        ))
      }
      if (past > 14 & past <= 21){
        twoweek <- stats::median(rt[rt$datetime <= last_time - 60*60*335 & rt$datetime >= last_time - 60*60*337 , ]$value)
        if (is.na(twoweek)){
          twoweek <- stats::median(rt[rt$datetime <= last_time - 60*60*331 & rt$datetime >= last_time - 60*60*341 , ]$value)
        }
        threeweek <- stats::median(rt[rt$datetime <= last_time - 60*60*503 & rt$datetime >= last_time - 60*60*505 , ]$value)
        if (is.na(threeweek)){
          threeweek <- stats::median(rt[rt$datetime <= last_time - 60*60*497 & rt$datetime >= last_time - 60*60*511 , ]$value)
        }
        levels <- rbind(levels,
                        data.frame("loc" = i,
                                   "name" = names_level[i],
                                   "level" = if (!is.na(latest)) round(latest, 3) else NA,
                                   "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                   "mean" = if (length(percent_mean == 1)) percent_mean else NA,
                                   "24" = if (!is.na(day)) round((latest - day) * 100, 1) else NA,
                                   "48" = if (!is.na(twoday)) round((latest - twoday) * 100, 1) else NA,
                                   "72" = if (!is.na(threeday)) round((latest - threeday) * 100, 1) else NA,
                                   "week" = if (!is.na(week)) round((latest - week) * 100, 1) else NA,
                                   "twoweek" = if (!is.na(twoweek)) round((latest - twoweek) * 100, 1) else NA,
                                   "threeweek" = if (!is.na(threeweek)) round((latest - threeweek) * 100, 1) else NA,
                                   "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                   "Hrs" = as.numeric(paste0(round(age[1],1))),
                                   "location_comment" = NA,
                                   "yesterday_comments" = if (length(yesterday_comment_levels) < 1 | is.null(yesterday_comment_levels)) NA else yesterday_comment_levels
                        ))
      }
      if (past > 21){
        twoweek <- stats::median(rt[rt$datetime <= last_time - 60*60*335 & rt$datetime >= last_time - 60*60*337 , ]$value)
        if (is.na(twoweek)){
          twoweek <- stats::median(rt[rt$datetime <= last_time - 60*60*331 & rt$datetime >= last_time - 60*60*341 , ]$value)
        }
        threeweek <- stats::median(rt[rt$datetime <= last_time - 60*60*503 & rt$datetime >= last_time - 60*60*505 , ]$value)
        if (is.na(threeweek)){
          threeweek <- stats::median(rt[rt$datetime <= last_time - 60*60*497 & rt$datetime >= last_time - 60*60*511 , ]$value)
        }
        fourweek <- stats::median(rt[rt$datetime <= last_time - 60*60*671 & rt$datetime >= last_time - 60*60*673 , ]$value)
        if (is.na(fourweek)){
          fourweek <- stats::median(rt[rt$datetime <= last_time - 60*60*667 & rt$datetime >= last_time - 60*60*677 , ]$value)
        }
        levels <- rbind(levels,
                        data.frame("loc" = i,
                                   "name" = names_level[i],
                                   "level" = if (!is.na(latest)) round(latest, 3) else NA,
                                   "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                   "mean" = if (length(percent_mean == 1)) percent_mean else NA,
                                   "24" = if (!is.na(day)) round((latest - day) * 100, 1) else NA,
                                   "48" = if (!is.na(twoday)) round((latest - twoday) * 100, 1) else NA,
                                   "72" = if (!is.na(threeday)) round((latest - threeday) * 100, 1) else NA,
                                   "week" = if (!is.na(week)) round((latest - week) * 100, 1) else NA,
                                   "twoweek" = if (!is.na(twoweek)) round((latest - twoweek) * 100, 1) else NA,
                                   "threeweek" = if (!is.na(threeweek)) round((latest - threeweek) * 100, 1) else NA,
                                   "fourweek" = if (!is.na(fourweek)) round((latest - fourweek) * 100, 1) else NA,
                                   "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                   "Hrs" = as.numeric(paste0(round(age[1],1))),
                                   "location_comment" = NA,
                                   "yesterday_comments" = if (length(yesterday_comment_levels) < 1 | is.null(yesterday_comment_levels)) NA else yesterday_comment_levels
                        ))
      }
    }
    if (past <= 7){
      colnames(levels) <- c("Location", "Name", " Level (m)", "% hist rng", "% hist mean", "24 hr chg (cm)", "48 hr chg (cm)", "72 hr chg (cm)", "1 week chg (cm)", "Last data MST", "Hrs", "Location specific comments", "Yesterday's comments")
    }
    if (past > 7 & past <= 14){
      colnames(levels) <- c("Location", "Name", " Level (m)", "% hist rng", "% hist mean", "24 hr chg (cm)", "48 hr chg (cm)", "72 hr chg (cm)", "1 week chg (cm)", "2 week chg (cm)", "Last data MST", "Hrs", "Location specific comments", "Yesterday's comments")
    }
    if (past > 14 & past <= 21){
      colnames(levels) <- c("Location", "Name", " Level (m)", "% hist rng", "% hist mean", "24 hr chg (cm)", "48 hr chg (cm)", "72 hr chg (cm)", "1 week chg (cm)", "2 week chg (cm)", "3 week chg (cm)", "Last data MST", "Hrs", "Location specific comments", "Yesterday's comments")
    }
    if (past > 21){
      colnames(levels) <- c("Location", "Name", " Level (m)", "% hist rng", "% hist mean", "24 hr chg (cm)", "48 hr chg (cm)", "72 hr chg (cm)", "1 week chg (cm)", "2 week chg (cm)", "3 week chg (cm)", "4 week chg (cm)", "Last data MST", "Hrs", "Location specific comments", "Yesterday's comments")
    }
    levels <- hablar::rationalize(levels)
    tables$levels <- levels
  }


  if (length(flow_rt) > 0){ #generate flow table
    flows <- data.frame()
    for (i in names(flow_rt)){
      rt <- flow_rt[[i]]
      last_time <- rt[rt$datetime == max(rt$datetime) ,]$datetime
      age <- difftime(Sys.time(), last_time, units = "hours")
      latest <- stats::median(rt[rt$datetime <= last_time & rt$datetime >= last_time - 60*30 , ]$value) #median of last 30 minutes of data
      percent_historic <- round(((latest - flow_daily[[i]]$min) / (flow_daily[[i]]$max - flow_daily[[i]]$min)) * 100, 0)
      percent_mean <- round(latest/flow_daily[[i]]$q50 * 100, 0)
      day <- stats::median(rt[rt$datetime <= last_time - 60*60*24 & rt$datetime >= last_time - 60*60*24.5 , ]$value) #median of 30 minutes
      twoday <- stats::median(rt[rt$datetime <= last_time - 60*60*47.5 & rt$datetime >= last_time - 60*60*48.5 , ]$value) #median of 1 hour
      threeday <- stats::median(rt[rt$datetime <= last_time - 60*60*71.5 & rt$datetime >= last_time - 60*60*72.5 , ]$value) #median of 1 hour
      week <- stats::median(rt[rt$datetime <= last_time - 60*60*167 & rt$datetime >= last_time - 60*60*169 , ]$value) #median of 2 hours
      if (is.na(week)){ #expand the range if no data within the 2 hour timespan
        week <- stats::median(rt[rt$datetime <= last_time - 60*60*165 & rt$datetime >= last_time - 60*60*171 , ]$value)
      }
      yesterday_comment_flows <- if(yesterday_comments) yesterday$yesterday_locs$flows[yesterday$yesterday_locs$flows$Location == i, "Location.specific.comments"] else NA

      if (past <= 7){
        flows <- rbind(flows,
                        data.frame("loc" = i,
                                   "name" = names_flow[i],
                                   "flow" = if (!is.na(latest)) round(latest, 1) else NA,
                                   "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                   "mean" = if (length(percent_mean == 1)) percent_mean else NA,
                                   "24" = if (!is.na(day)) round((latest - day), 1) else NA,
                                   "48" = if (!is.na(twoday)) round((latest - twoday), 1) else NA,
                                   "72" = if (!is.na(threeday)) round((latest - threeday), 1) else NA,
                                   "week" = if (!is.na(week)) round((latest - week), 1) else NA,
                                   "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                   "Hrs" = as.numeric(paste0(round(age[1],1))),
                                   "location_comment" = NA,
                                   "yesterday_comments" = if (length(yesterday_comment_flows) < 1 | is.null(yesterday_comment_flows)) NA else yesterday_comment_flows
                        ))
      }
      if (past > 7 & past <= 14){
        twoweek <- stats::median(rt[rt$datetime <= last_time - 60*60*335 & rt$datetime >= last_time - 60*60*337 , ]$value)
        if (is.na(twoweek)){
          twoweek <- stats::median(rt[rt$datetime <= last_time - 60*60*331 & rt$datetime >= last_time - 60*60*341 , ]$value)
        }
        flows <- rbind(flows,
                        data.frame("loc" = i,
                                   "name" = names_flow[i],
                                   "flow" = if (!is.na(latest)) round(latest, 1) else NA,
                                   "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                   "mean" = if (length(percent_mean == 1)) percent_mean else NA,
                                   "24" = if (!is.na(day)) round((latest - day), 1) else NA,
                                   "48" = if (!is.na(twoday)) round((latest - twoday), 1) else NA,
                                   "72" = if (!is.na(threeday)) round((latest - threeday), 1) else NA,
                                   "week" = if (!is.na(week)) round((latest - week), 1) else NA,
                                   "twoweek" = if (!is.na(twoweek)) round((latest - twoweek), 1) else NA,
                                   "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                   "Hrs" = as.numeric(paste0(round(age[1],1))),
                                   "location_comment" = NA,
                                   "yesterday_comments" = if (length(yesterday_comment_flows) < 1 | is.null(yesterday_comment_flows)) NA else yesterday_comment_flows
                        ))
      }
      if (past > 14 & past <= 21){
        twoweek <- stats::median(rt[rt$datetime <= last_time - 60*60*335 & rt$datetime >= last_time - 60*60*337 , ]$value)
        if (is.na(twoweek)){
          twoweek <- stats::median(rt[rt$datetime <= last_time - 60*60*331 & rt$datetime >= last_time - 60*60*341 , ]$value)
        }
        threeweek <- stats::median(rt[rt$datetime <= last_time - 60*60*503 & rt$datetime >= last_time - 60*60*505 , ]$value)
        if (is.na(threeweek)){
          threeweek <- stats::median(rt[rt$datetime <= last_time - 60*60*497 & rt$datetime >= last_time - 60*60*511 , ]$value)
        }
        flows <- rbind(flows,
                        data.frame("loc" = i,
                                   "name" = names_flow[i],
                                   "flow" = if (!is.na(latest)) round(latest, 1) else NA,
                                   "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                   "mean" = if (length(percent_mean == 1)) percent_mean else NA,
                                   "24" = if (!is.na(day)) round((latest - day), 1) else NA,
                                   "48" = if (!is.na(twoday)) round((latest - twoday), 1) else NA,
                                   "72" = if (!is.na(threeday)) round((latest - threeday), 1) else NA,
                                   "week" = if (!is.na(week)) round((latest - week), 1) else NA,
                                   "twoweek" = if (!is.na(twoweek)) round((latest - twoweek), 1) else NA,
                                   "threeweek" = if (!is.na(threeweek)) round((latest - threeweek), 1) else NA,
                                   "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                   "Hrs" = as.numeric(paste0(round(age[1],1))),
                                   "location_comment" = NA,
                                   "yesterday_comments" = if (length(yesterday_comment_flows) < 1 | is.null(yesterday_comment_flows)) NA else yesterday_comment_flows
                        ))
      }
      if (past > 21){
        twoweek <- stats::median(rt[rt$datetime <= last_time - 60*60*335 & rt$datetime >= last_time - 60*60*337 , ]$value)
        if (is.na(twoweek)){
          twoweek <- stats::median(rt[rt$datetime <= last_time - 60*60*331 & rt$datetime >= last_time - 60*60*341 , ]$value)
        }
        threeweek <- stats::median(rt[rt$datetime <= last_time - 60*60*503 & rt$datetime >= last_time - 60*60*505 , ]$value)
        if (is.na(threeweek)){
          threeweek <- stats::median(rt[rt$datetime <= last_time - 60*60*497 & rt$datetime >= last_time - 60*60*511 , ]$value)
        }
        fourweek <- stats::median(rt[rt$datetime <= last_time - 60*60*671 & rt$datetime >= last_time - 60*60*673 , ]$value)
        if (is.na(fourweek)){
          fourweek <- stats::median(rt[rt$datetime <= last_time - 60*60*667 & rt$datetime >= last_time - 60*60*677 , ]$value)
        }
        flows <- rbind(flows,
                        data.frame("loc" = i,
                                   "name" = names_flow[i],
                                   "flow" = if (!is.na(latest)) round(latest, 1) else NA,
                                   "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                   "mean" = if (length(percent_mean == 1)) percent_mean else NA,
                                   "24" = if (!is.na(day)) round((latest - day), 1) else NA,
                                   "48" = if (!is.na(twoday)) round((latest - twoday), 1) else NA,
                                   "72" = if (!is.na(threeday)) round((latest - threeday), 1) else NA,
                                   "week" = if (!is.na(week)) round((latest - week), 1) else NA,
                                   "twoweek" = if (!is.na(twoweek)) round((latest - twoweek), 1) else NA,
                                   "threeweek" = if (!is.na(threeweek)) round((latest - threeweek), 1) else NA,
                                   "fourweek" = if (!is.na(fourweek)) round((latest - fourweek), 1) else NA,
                                   "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                   "Hrs" = as.numeric(paste0(round(age[1],1))),
                                   "location_comment" = NA,
                                   "yesterday_comments" = if (length(yesterday_comment_flows) < 1 | is.null(yesterday_comment_flows)) NA else yesterday_comment_flows
                        ))
      }
    }
    if (past <= 7){
      colnames(flows) <- c("Location", "Name", " Flow (m3/s)", "% hist rng", "% hist mean", "24 hr chg", "48 hr chg", "72 hr chg", "1 week chg", "Last data MST", "Hrs", "Location specific comments", "Yesterday's comments")
    }
    if (past > 7 & past <= 14){
      colnames(flows) <- c("Location", "Name", " Flow (m3/s)", "% hist rng", "% hist mean", "24 hr chg", "48 hr chg", "72 hr chg", "1 week chg", "2 week chg", "Last data MST", "Hrs", "Location specific comments", "Yesterday's comments")
    }
    if (past > 14 & past <= 21){
      colnames(flows) <- c("Location", "Name", " Flow (m3/s)", "% hist rng", "% hist mean", "24 hr chg", "48 hr chg", "72 hr chg", "1 week chg", "2 week chg", "3 week chg", "Last data MST", "Hrs", "Location specific comments", "Yesterday's comments")
    }
    if (past > 21){
      colnames(flows) <- c("Location", "Name", " Flow (m3/s)", "% hist rng", "% hist mean", "24 hr chg", "48 hr chg", "72 hr chg", "1 week chg", "2 week chg", "3 week chg", "4 week chg", "Last data MST", "Hrs", "Location specific comments", "Yesterday's comments")
    }
    flows <- hablar::rationalize(flows)
    tables$flows <- flows
  }

  if (length(snow_rt) > 0){ #generate snow table
    snow <- data.frame()
    for (i in names(snow_rt)){
      rt <- snow_rt[[i]]
      last_time <- rt[rt$datetime == max(rt$datetime) ,]$datetime
      age <- difftime(Sys.time(), last_time, units = "hours")
      latest <- stats::median(rt[rt$datetime <= last_time & rt$datetime >= last_time - 60*30 , ]$value) #median of last 30 minutes of data
      percent_historic <- round(((latest - snow_daily[[i]]$min) / (snow_daily[[i]]$max - snow_daily[[i]]$min)) * 100, 0)
      percent_mean <- round(latest/snow_daily[[i]]$q50 * 100, 0)
      day <- stats::median(rt[rt$datetime <= last_time - 60*60*24 & rt$datetime >= last_time - 60*60*24.5 , ]$value) #median of 30 minutes
      twoday <- stats::median(rt[rt$datetime <= last_time - 60*60*47.5 & rt$datetime >= last_time - 60*60*48.5 , ]$value) #median of 1 hour
      threeday <- stats::median(rt[rt$datetime <= last_time - 60*60*71.5 & rt$datetime >= last_time - 60*60*72.5 , ]$value) #median of 1 hour
      week <- stats::median(rt[rt$datetime <= last_time - 60*60*167 & rt$datetime >= last_time - 60*60*169 , ]$value) #median of 2 hours
      if (is.na(week)){ #expand the range if no data within the 2 hour timespan
        week <- stats::median(rt[rt$datetime <= last_time - 60*60*165 & rt$datetime >= last_time - 60*60*171 , ]$value)
      }
      yesterday_comment_snow <- if(yesterday_comments) yesterday$yesterday_locs$snow[yesterday$yesterday_locs$snow$Location == i, "Location.specific.comments"] else NA

      if (past <= 7){
        snow <- rbind(snow,
                       data.frame("loc" = i,
                                  "name" = names_snow[i],
                                  "SWE" = if (!is.na(latest)) round(latest, 1) else NA,
                                  "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                  "mean" = if (length(percent_mean == 1)) percent_mean else NA,
                                  "24" = if (!is.na(day)) round((latest - day), 1) else NA,
                                  "48" = if (!is.na(twoday)) round((latest - twoday), 1) else NA,
                                  "72" = if (!is.na(threeday)) round((latest - threeday), 1) else NA,
                                  "week" = if (!is.na(week)) round((latest - week), 1) else NA,
                                  "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                  "Hrs" = as.numeric(paste0(round(age[1],1))),
                                  "location_comment" = NA,
                                  "yesterday_comments" = if (length(yesterday_comment_snow) < 1 | is.null(yesterday_comment_snow)) NA else yesterday_comment_snow
                       ))
      }
      if (past > 7 & past <= 14){
        twoweek <- stats::median(rt[rt$datetime <= last_time - 60*60*335 & rt$datetime >= last_time - 60*60*337 , ]$value)
        if (is.na(twoweek)){
          twoweek <- stats::median(rt[rt$datetime <= last_time - 60*60*331 & rt$datetime >= last_time - 60*60*341 , ]$value)
        }
        snow <- rbind(snow,
                       data.frame("loc" = i,
                                  "name" = names_snow[i],
                                  "SWE" = if (!is.na(latest)) round(latest, 1) else NA,
                                  "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                  "mean" = if (length(percent_mean == 1)) percent_mean else NA,
                                  "24" = if (!is.na(day)) round((latest - day), 1) else NA,
                                  "48" = if (!is.na(twoday)) round((latest - twoday), 1) else NA,
                                  "72" = if (!is.na(threeday)) round((latest - threeday), 1) else NA,
                                  "week" = if (!is.na(week)) round((latest - week), 1) else NA,
                                  "twoweek" = if (!is.na(twoweek)) round((latest - twoweek), 1) else NA,
                                  "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                  "Hrs" = as.numeric(paste0(round(age[1],1))),
                                  "location_comment" = NA,
                                  "yesterday_comments" = if (length(yesterday_comment_snow) < 1 | is.null(yesterday_comment_snow)) NA else yesterday_comment_snow
                       ))
      }
      if (past > 14 & past <= 21){
        twoweek <- stats::median(rt[rt$datetime <= last_time - 60*60*335 & rt$datetime >= last_time - 60*60*337 , ]$value)
        if (is.na(twoweek)){
          twoweek <- stats::median(rt[rt$datetime <= last_time - 60*60*331 & rt$datetime >= last_time - 60*60*341 , ]$value)
        }
        threeweek <- stats::median(rt[rt$datetime <= last_time - 60*60*503 & rt$datetime >= last_time - 60*60*505 , ]$value)
        if (is.na(threeweek)){
          threeweek <- stats::median(rt[rt$datetime <= last_time - 60*60*497 & rt$datetime >= last_time - 60*60*511 , ]$value)
        }
        snow <- rbind(snow,
                       data.frame("loc" = i,
                                  "name" = names_snow[i],
                                  "SWE" = if (!is.na(latest)) round(latest, 1) else NA,
                                  "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                  "mean" = if (length(percent_mean == 1)) percent_mean else NA,
                                  "24" = if (!is.na(day)) round((latest - day), 1) else NA,
                                  "48" = if (!is.na(twoday)) round((latest - twoday), 1) else NA,
                                  "72" = if (!is.na(threeday)) round((latest - threeday), 1) else NA,
                                  "week" = if (!is.na(week)) round((latest - week), 1) else NA,
                                  "twoweek" = if (!is.na(twoweek)) round((latest - twoweek), 1) else NA,
                                  "threeweek" = if (!is.na(threeweek)) round((latest - threeweek), 1) else NA,
                                  "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                  "Hrs" = as.numeric(paste0(round(age[1],1))),
                                  "location_comment" = NA,
                                  "yesterday_comments" = if (length(yesterday_comment_snow) < 1 | is.null(yesterday_comment_snow)) NA else yesterday_comment_snow
                       ))
      }
      if (past > 21){
        twoweek <- stats::median(rt[rt$datetime <= last_time - 60*60*335 & rt$datetime >= last_time - 60*60*337 , ]$value)
        if (is.na(twoweek)){
          twoweek <- stats::median(rt[rt$datetime <= last_time - 60*60*331 & rt$datetime >= last_time - 60*60*341 , ]$value)
        }
        threeweek <- stats::median(rt[rt$datetime <= last_time - 60*60*503 & rt$datetime >= last_time - 60*60*505 , ]$value)
        if (is.na(threeweek)){
          threeweek <- stats::median(rt[rt$datetime <= last_time - 60*60*497 & rt$datetime >= last_time - 60*60*511 , ]$value)
        }
        fourweek <- stats::median(rt[rt$datetime <= last_time - 60*60*671 & rt$datetime >= last_time - 60*60*673 , ]$value)
        if (is.na(fourweek)){
          fourweek <- stats::median(rt[rt$datetime <= last_time - 60*60*667 & rt$datetime >= last_time - 60*60*677 , ]$value)
        }
        snow <- rbind(snow,
                       data.frame("loc" = i,
                                  "name" = names_snow[i],
                                  "SWE" = if (!is.na(latest)) round(latest, 1) else NA,
                                  "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                  "mean" = if (length(percent_mean == 1)) percent_mean else NA,
                                  "24" = if (!is.na(day)) round((latest - day), 1) else NA,
                                  "48" = if (!is.na(twoday)) round((latest - twoday), 1) else NA,
                                  "72" = if (!is.na(threeday)) round((latest - threeday), 1) else NA,
                                  "week" = if (!is.na(week)) round((latest - week), 1) else NA,
                                  "twoweek" = if (!is.na(twoweek)) round((latest - twoweek), 1) else NA,
                                  "threeweek" = if (!is.na(threeweek)) round((latest - threeweek), 1) else NA,
                                  "fourweek" = if (!is.na(fourweek)) round((latest - fourweek), 1) else NA,
                                  "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                  "Hrs" = as.numeric(paste0(round(age[1],1))),
                                  "location_comment" = NA,
                                  "yesterday_comments" = if (length(yesterday_comment_snow) < 1 | is.null(yesterday_comment_snow)) NA else yesterday_comment_snow
                       ))
      }
    }
    if (past <= 7){
      colnames(snow) <- c("Location", "Name", "SWE (mm)", "% hist rng", "% hist mean", "24 hr chg", "48 hr chg", "72 hr chg", "1 week chg", "Last data MST", "Hrs", "Location specific comments", "Yesterday's comments")
    }
    if (past > 7 & past <= 14){
      colnames(snow) <- c("Location", "Name", "SWE (mm)", "% hist rng", "% hist mean", "24 hr chg", "48 hr chg", "72 hr chg", "1 week chg", "2 week chg", "Last data MST", "Hrs", "Location specific comments", "Yesterday's comments")
    }
    if (past > 14 & past <= 21){
      colnames(snow) <- c("Location", "Name", "SWE (mm)", "% hist rng", "% hist mean", "24 hr chg", "48 hr chg", "72 hr chg", "1 week chg", "2 week chg", "3 week chg", "Last data MST", "Hrs", "Location specific comments", "Yesterday's comments")
    }
    if (past > 21){
      colnames(snow) <- c("Location", "Name", "SWE (mm)", "% hist rng", "% hist mean", "24 hr chg", "48 hr chg", "72 hr chg", "1 week chg", "2 week chg", "3 week chg", "4 week chg", "Last data MST", "Hrs", "Location specific comments", "Yesterday's comments")
    }
    snow <- hablar::rationalize(snow)
    tables$snow <- snow
  }

  if (length(bridges_rt) >0){ #generate bridges table
    bridges <- data.frame()
    for (i in names(bridges_rt)){
      rt <- bridges_rt[[i]]
      last_time <- rt[rt$datetime == max(rt$datetime) ,]$datetime
      age <- difftime(Sys.time(), last_time, units = "hours")
      latest <- stats::median(rt[rt$datetime <= last_time & rt$datetime >= last_time - 60*30 , ]$value) #median of last 30 minutes of data
      percent_historic <- round(((latest - bridges_daily[[i]]$min) / (bridges_daily[[i]]$max - bridges_daily[[i]]$min)) * 100, 0)
      percent_mean <- round(((latest - bridges_daily[[i]]$min) / (bridges_daily[[i]]$q50 - bridges_daily[[i]]$min)) * 100, 0)
      day <- stats::median(rt[rt$datetime <= last_time - 60*60*24 & rt$datetime >= last_time - 60*60*24.5 , ]$value) #median of 30 minutes
      twoday <- stats::median(rt[rt$datetime <= last_time - 60*60*47.5 & rt$datetime >= last_time - 60*60*48.5 , ]$value) #median of 1 hour
      threeday <- stats::median(rt[rt$datetime <= last_time - 60*60*71.5 & rt$datetime >= last_time - 60*60*72.5 , ]$value) #median of 1 hour
      week <- stats::median(rt[rt$datetime <= last_time - 60*60*167 & rt$datetime >= last_time - 60*60*169 , ]$value) #median of 2 hours
      if (is.na(week)){ #expand the range if no data within the 2 hour timespan
        week <- stats::median(rt[rt$datetime <= last_time - 60*60*165 & rt$datetime >= last_time - 60*60*171 , ]$value)
      }
      yesterday_comment_bridges <- if(yesterday_comments) yesterday$yesterday_locs$bridges[yesterday$yesterday_locs$bridges$Location == i, "Location.specific.comments"] else NA


      if (past <= 7){
        bridges <- rbind(bridges,
                      data.frame("loc" = i,
                                 "name" = names_bridges[i],
                                 "distance" = if (!is.na(latest)) round(latest, 1) else NA,
                                 "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                 "mean" = if (length(percent_mean == 1)) percent_mean else NA,
                                 "24" = if (!is.na(day)) round((latest - day) * 100, 1) else NA,
                                 "48" = if (!is.na(twoday)) round((latest - twoday) * 100, 1) else NA,
                                 "72" = if (!is.na(threeday)) round((latest - threeday) * 100, 1) else NA,
                                 "week" = if (!is.na(week)) round((latest - week) * 100, 1) else NA,
                                 "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                 "Hrs" = as.numeric(paste0(round(age[1],1))),
                                 "location_comment" = NA,
                                 "yesterday_comments" = if (length(yesterday_comment_bridges) < 1 | is.null(yesterday_comment_bridges)) NA else yesterday_comment_bridges
                      ))
      }
      if (past > 7 & past <= 14){
        twoweek <- stats::median(rt[rt$datetime <= last_time - 60*60*335 & rt$datetime >= last_time - 60*60*337 , ]$value)
        if (is.na(twoweek)){
          twoweek <- stats::median(rt[rt$datetime <= last_time - 60*60*331 & rt$datetime >= last_time - 60*60*341 , ]$value)
        }
        bridges <- rbind(bridges,
                      data.frame("loc" = i,
                                 "name" = names_bridges[i],
                                 "distance" = if (!is.na(latest)) round(latest, 1) else NA,
                                 "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                 "mean" = if (length(percent_mean == 1)) percent_mean else NA,
                                 "24" = if (!is.na(day)) round((latest - day) * 100, 1) else NA,
                                 "48" = if (!is.na(twoday)) round((latest - twoday) * 100, 1) else NA,
                                 "72" = if (!is.na(threeday)) round((latest - threeday) * 100, 1) else NA,
                                 "week" = if (!is.na(week)) round((latest - week) * 100, 1) else NA,
                                 "twoweek" = if (!is.na(twoweek)) round((latest - twoweek) * 100, 1) else NA,
                                 "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                 "Hrs" = as.numeric(paste0(round(age[1],1))),
                                 "location_comment" = NA,
                                 "yesterday_comments" = if (length(yesterday_comment_bridges) < 1 | is.null(yesterday_comment_bridges)) NA else yesterday_comment_bridges
                      ))
      }
      if (past > 14 & past <= 21){
        twoweek <- stats::median(rt[rt$datetime <= last_time - 60*60*335 & rt$datetime >= last_time - 60*60*337 , ]$value)
        if (is.na(twoweek)){
          twoweek <- stats::median(rt[rt$datetime <= last_time - 60*60*331 & rt$datetime >= last_time - 60*60*341 , ]$value)
        }
        threeweek <- stats::median(rt[rt$datetime <= last_time - 60*60*503 & rt$datetime >= last_time - 60*60*505 , ]$value)
        if (is.na(threeweek)){
          threeweek <- stats::median(rt[rt$datetime <= last_time - 60*60*497 & rt$datetime >= last_time - 60*60*511 , ]$value)
        }
        bridges <- rbind(bridges,
                      data.frame("loc" = i,
                                 "name" = names_bridges[i],
                                 "distance" = if (!is.na(latest)) round(latest, 1) else NA,
                                 "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                 "mean" = if (length(percent_mean == 1)) percent_mean else NA,
                                 "24" = if (!is.na(day)) round((latest - day) * 100, 1) else NA,
                                 "48" = if (!is.na(twoday)) round((latest - twoday) * 100, 1) else NA,
                                 "72" = if (!is.na(threeday)) round((latest - threeday) * 100, 1) else NA,
                                 "week" = if (!is.na(week)) round((latest - week) * 100, 1) else NA,
                                 "twoweek" = if (!is.na(twoweek)) round((latest - twoweek) * 100, 1) else NA,
                                 "threeweek" = if (!is.na(threeweek)) round((latest - threeweek) * 100, 1) else NA,
                                 "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                 "Hrs" = as.numeric(paste0(round(age[1],1))),
                                 "location_comment" = NA,
                                 "yesterday_comments" = if (length(yesterday_comment_bridges) < 1 | is.null(yesterday_comment_bridges)) NA else yesterday_comment_bridges
                      ))
      }
      if (past > 21){
        twoweek <- stats::median(rt[rt$datetime <= last_time - 60*60*335 & rt$datetime >= last_time - 60*60*337 , ]$value)
        if (is.na(twoweek)){
          twoweek <- stats::median(rt[rt$datetime <= last_time - 60*60*331 & rt$datetime >= last_time - 60*60*341 , ]$value)
        }
        threeweek <- stats::median(rt[rt$datetime <= last_time - 60*60*503 & rt$datetime >= last_time - 60*60*505 , ]$value)
        if (is.na(threeweek)){
          threeweek <- stats::median(rt[rt$datetime <= last_time - 60*60*497 & rt$datetime >= last_time - 60*60*511 , ]$value)
        }
        fourweek <- stats::median(rt[rt$datetime <= last_time - 60*60*671 & rt$datetime >= last_time - 60*60*673 , ]$value)
        if (is.na(fourweek)){
          fourweek <- stats::median(rt[rt$datetime <= last_time - 60*60*667 & rt$datetime >= last_time - 60*60*677 , ]$value)
        }
        bridges <- rbind(bridges,
                      data.frame("loc" = i,
                                 "name" = names_bridges[i],
                                 "distance" = if (!is.na(latest)) round(latest, 1) else NA,
                                 "percent" = if (length(percent_historic == 1)) percent_historic else NA,
                                 "mean" = if (length(percent_mean == 1)) percent_mean else NA,
                                 "24" = if (!is.na(day)) round((latest - day) * 100, 1) else NA,
                                 "48" = if (!is.na(twoday)) round((latest - twoday) * 100, 1) else NA,
                                 "72" = if (!is.na(threeday)) round((latest - threeday) * 100, 1) else NA,
                                 "week" = if (!is.na(week)) round((latest - week) * 100, 1) else NA,
                                 "twoweek" = if (!is.na(twoweek)) round((latest - twoweek) * 100, 1) else NA,
                                 "threeweek" = if (!is.na(threeweek)) round((latest - threeweek) * 100, 1) else NA,
                                 "fourweek" = if (!is.na(fourweek)) round((latest - fourweek) * 100, 1) else NA,
                                 "age" = substr(format(last_time, tz = "MST"), 1, 16),
                                 "Hrs" = as.numeric(paste0(round(age[1],1))),
                                 "location_comment" = NA,
                                 "yesterday_comments" = if (length(yesterday_comment_bridges) < 1 | is.null(yesterday_comment_bridges)) NA else yesterday_comment_bridges
                      ))
      }
    }
    if (past <= 7){
      colnames(bridges) <- c("Location", "Name", " Distance (m)", "% hist rng", "% hist mean", "24 hr chg (cm)", "48 hr chg (cm)", "72 hr chg (cm)", "1 week chg", "Last data MST", "Hrs", "Location specific comments", "Yesterday's comments")
    }
    if (past > 7 & past <= 14){
      colnames(bridges) <- c("Location", "Name", " Distance (m)", "% hist rng", "% hist mean", "24 hr chg (cm)", "48 hr chg (cm)", "72 hr chg (cm)", "1 week chg (cm)", "2 week chg (cm)", "Last data MST", "Hrs", "Location specific comments", "Yesterday's comments")
    }
    if (past > 14 & past <= 21){
      colnames(bridges) <- c("Location", "Name", " Distance (m)", "% hist rng", "% hist mean", "24 hr chg (cm)", "48 hr chg (cm)", "72 hr chg (cm)", "1 week chg (cm)", "2 week chg (cm)", "3 week chg (cm)", "Last data MST", "Hrs", "Location specific comments", "Yesterday's comments")
    }
    if (past > 21){
      colnames(bridges) <- c("Location", "Name", " Distance (m)", "% hist rng", "% hist mean", "24 hr chg (cm)", "48 hr chg (cm)", "72 hr chg (cm)", "1 week chg (cm)", "2 week chg (cm)", "3 week chg (cm)", "4 week chg (cm)", "Last data MST", "Hrs", "Location specific comments", "Yesterday's comments")
    }
    bridges <- hablar::rationalize(bridges)
    tables$bridges <- bridges
  }

  #Make the Excel workbook ---------------------------
  wb <- openxlsx::createWorkbook(creator = "Ghislain de Laplante (via automated process)", title = "Hydrometric Condition Report")
  time <- Sys.time()
  head <- data.frame(paste0("Issued at ", substr(format(time, tz = "MST"), 1, 16), " MST"),
                     NA,
                     "Forecaster name:",
                     NA,
                     NA,
                     NA,
                     paste0("Created with YGwater ", utils::packageVersion("YGwater")))
  headStyle <- openxlsx::createStyle(fgFill = "turquoise2")
  fodNameStyle <- openxlsx::createStyle(fgFill = "darkorange", border = "TopBottomLeftRight", borderStyle = "medium")
  fodCommentStyle <- openxlsx::createStyle(fgFill = "lightsteelblue")
  yesterdayFodCommentStyle <- openxlsx::createStyle(fgFill = "lightyellow", textDecoration = "italic")
  colStyleYellow <- openxlsx::createStyle(bgFill = "yellow")
  colStyleRed <- openxlsx::createStyle(bgFill = "red")
  generalCommentStyle <- openxlsx::createStyle(border = "TopBottomLeftRight", fgFill = "lightsteelblue", wrapText = TRUE)
  generalCommentStyle2 <- openxlsx::createStyle(border = "TopBottomLeftRight", textDecoration = "bold", fgFill = "lightsteelblue", wrapText = TRUE)
  yesterdayGeneralCommentStyle <- openxlsx::createStyle(border = "TopBottomLeftRight", fgFill = "lightyellow", wrapText = TRUE, textDecoration = "italic")
  yesterdayGeneralCommentStyle2 <- openxlsx::createStyle(border = "TopBottomLeftRight", textDecoration = c("bold", "italic"), fgFill = "lightyellow", wrapText = TRUE)
  increasingStyle <- openxlsx::createStyle(fontColour = "red3", textDecoration = "bold")
  decreasingStyle <- openxlsx::createStyle(fontColour = "forestgreen", textDecoration = "bold")
  missingDataStyle <- openxlsx::createStyle(bgFill = "grey")
  #comments
  delayComment <- openxlsx::createComment("Yellow: > 2 hours. Red: > 4 hours.", author = "Ghislain", visible = FALSE)
  percHistComment <- openxlsx::createComment("0 = historic min, 100 = historic max. Yellow = >75%, red: >100%.", author = "Ghislain", visible = FALSE)
  percMeanComment <- openxlsx::createComment("Current level / hist. mean (excl. current yr). 100 = historic mean. Yellow: >125%, Red: >150%.", author = "Ghislain", visible = FALSE)
  percMeanAdjComment <- openxlsx::createComment("Adjusted to historic min due to arbitrary 0 point. 100 = historic mean, 0 = historic min. Yellow: >150%, Red: >200%.", author = "Ghislain", visible = FALSE)

  for (i in names(tables)[!(names(tables) %in% "precipitation")]){
    openxlsx::addWorksheet(wb, i)
    #Create/format the header
    openxlsx::writeData(wb, i, head, startCol = 1, startRow = 1, colNames = FALSE)
    openxlsx::writeData(wb, i, NA, startCol = 1, startRow = 2, colNames = FALSE)
    openxlsx::mergeCells(wb, i, cols = c(1:2), rows = 1)
    openxlsx::mergeCells(wb, i, cols = c(3:4), rows = 1)
    openxlsx::mergeCells(wb, i, cols = c(5:6), rows = 1)
    openxlsx::mergeCells(wb, i, cols = c(7:9), rows = 1)
    openxlsx::addStyle(wb, i, style = fodNameStyle, rows = 1, cols = c(5:6))
    #add a line for general comments
    openxlsx::writeData(wb, i, "General comment", startCol = 1, startRow = 3, colNames = FALSE)
    openxlsx::writeData(wb, i, "Yesterday's comment", startCol = 1, startRow = 4, colNames = FALSE)
    #add yesterday's comments
    openxlsx::writeData(wb, i, yesterday[["yesterday_general"]][[i]], startCol = 2, startRow = 4, colNames = FALSE)
    openxlsx::addStyle(wb, i, style = generalCommentStyle2, cols = 1, rows = 3)
    openxlsx::addStyle(wb, i, style = yesterdayGeneralCommentStyle2, cols = 1, rows = 4)
    openxlsx::mergeCells(wb, i, cols = if (past == 7) c(2:12) else if (past == 14) c(2:13) else if (past == 21) c(2:14) else if (past == 28) c(2:15), rows = 3)
    openxlsx::mergeCells(wb, i, cols = if (past == 7) c(2:12) else if (past == 14) c(2:13) else if (past == 21) c(2:14) else if (past == 28) c(2:15), rows = 4)
    openxlsx::addStyle(wb, i, style = generalCommentStyle, cols = if (past == 7) c(2:12) else if (past == 14) c(2:13) else if (past == 21) c(2:14) else if (past == 28) c(2:15), rows = 3, gridExpand = TRUE)
    openxlsx::addStyle(wb, i, style = yesterdayGeneralCommentStyle, cols = if (past == 7) c(2:12) else if (past == 14) c(2:13) else if (past == 21) c(2:14) else if (past == 28) c(2:15), rows = 4, gridExpand = TRUE)
    openxlsx::writeData(wb, i, NA, startCol = 1, startRow = 5, colNames = FALSE) #empty row before the data
    #add content
    openxlsx::writeData(wb, i,  tables[[i]], startRow = 6)
    #format for ease of viewing
    openxlsx::freezePane(wb, sheet = i, firstActiveRow = 7, firstActiveCol = 3)
    openxlsx::setColWidths(wb, i, cols = if (past == 7) c(1:13) else if (past == 14) c(1:14) else if (past == 21) c(1:15) else if (past == 28) c(1:16), widths = if (past == 7) c(10, 30, 10, 10, 10, 12, 12, 12, 12, 15, 4, 60, 60) else if (past == 14) c(10, 30, 10, 10, 10, 12, 12, 12, 12, 12, 15, 4, 60, 60) else if (past == 21) c(10, 30, 10, 10, 10, 12, 12, 12, 12, 12, 12, 15, 4, 60, 60) else if (past == 28) c(10, 30, 10, 10, 10, 12, 12, 12, 12, 12, 12, 12, 15, 4, 60, 60))
    openxlsx::addStyle(wb, i, headStyle, rows = 6, cols = if (past == 7) c(1:13) else if (past == 14) c(1:14) else if (past == 21) c(1:15) else if (past == 28) c(1:16))
    openxlsx::addStyle(wb, i, fodCommentStyle, rows = 1:nrow(tables[[i]])+6, cols = if (past == 7) 12 else if (past == 14) 13 else if (past == 21) 14 else if (past == 28) 15)
    openxlsx::addStyle(wb, i, yesterdayFodCommentStyle, rows = 1:nrow(tables[[i]])+6, cols = if (past == 7) 13 else if (past == 14) 14 else if (past == 21) 15 else if (past == 28) 16)
    #Add comments
    openxlsx::writeComment(wb, sheet = i, col = 4, row = 6, comment = percHistComment)
    openxlsx::writeComment(wb, sheet = i, col = 5, row = 6, comment = if (i == "levels") percMeanAdjComment else percMeanComment)
    openxlsx::writeComment(wb, sheet = i, col = if (past == 7) 11 else if (past == 14) 12 else if (past == 21) 13 else if (past == 28) 14, row = 6, comment = delayComment)
    #Conditional format
    openxlsx::conditionalFormatting(wb, sheet = i, rule = ">75", cols = 4, rows = 1:nrow(tables[[i]])+6, style = colStyleYellow)
    openxlsx::conditionalFormatting(wb, sheet = i, rule = ">100", cols = 4, rows = 1:nrow(tables[[i]])+6, style = colStyleRed)
    openxlsx::conditionalFormatting(wb, sheet = i, rule = if (i == "levels") ">150" else ">125", cols = 5, rows = 1:nrow(tables[[i]])+6, style = colStyleYellow)
    openxlsx::conditionalFormatting(wb, sheet = i, rule = if (i == "levels") ">200" else ">150", cols = 5, rows = 1:nrow(tables[[i]])+6, style = colStyleRed)
    #conditional format for age of last data
    openxlsx::conditionalFormatting(wb, sheet = i, rule = ">2", cols = if (past == 7) 11 else if (past == 14) 12 else if (past == 21) 13 else if (past == 28) 14, rows = 1:nrow(tables[[i]])+6, style = colStyleYellow)
    openxlsx::conditionalFormatting(wb, sheet = i, rule = ">4", cols = if (past == 7) 11 else if (past == 14) 12 else if (past == 21) 13 else if (past == 28) 14, rows = 1:nrow(tables[[i]])+6, style = colStyleRed)
    #Conditional format for increasing/decreasing (!bridge radars are inverse)
    openxlsx::conditionalFormatting(wb, sheet = i, rule = if (i == "bridges") "<0" else ">0", cols = if (past == 7) c(6:9) else if (past == 14) c(6:10) else if (past == 21) c(6:11) else if (past == 28) c(6:12), rows = 1:nrow(tables[[i]])+6, style = increasingStyle)
    openxlsx::conditionalFormatting(wb, sheet = i, rule = if (i == "bridges") ">0" else "<0", cols = if (past == 7) c(6:9) else if (past == 14) c(6:10) else if (past == 21) c(6:11) else if (past == 28) c(6:12), rows = 1:nrow(tables[[i]])+6, style = decreasingStyle)
    openxlsx::conditionalFormatting(wb, sheet = i, rule = '=""', cols = if (past == 7) c(3, 6:9) else if (past == 14) c(3, 6:10) else if (past == 21) c(3, 6:11) else if (past == 28) c(3, 6:12), rows = 1:nrow(tables[[i]])+6, style = missingDataStyle)
  }

  if ("precipitation" %in% names(tables)){
    openxlsx::addWorksheet(wb, "precipitation")
    #Create/format the header
    openxlsx::writeData(wb, "precipitation", head, startCol = 1, startRow = 1, colNames = FALSE)
    openxlsx::writeData(wb, "precipitation", NA, startCol = 1, startRow = 2, colNames = FALSE)
    openxlsx::mergeCells(wb, "precipitation", cols = c(1:2), rows = 1)
    openxlsx::mergeCells(wb, "precipitation", cols = c(3:4), rows = 1)
    openxlsx::mergeCells(wb, "precipitation", cols = c(5:6), rows = 1)
    openxlsx::mergeCells(wb, "precipitation", cols = c(7:9), rows = 1)
    openxlsx::addStyle(wb, "precipitation", style = fodNameStyle, rows = 1, cols = c(5:6))
    #add a line for general and yesterday comments
    openxlsx::writeData(wb, "precipitation", "General comment", startCol = 1, startRow = 3, colNames = FALSE)
    openxlsx::writeData(wb, "precipitation", "Yesterday's comment", startCol = 1, startRow = 4, colNames = FALSE)
    #add yesterday's comments
    openxlsx::writeData(wb, "precipitation", yesterday[["yesterday_general"]][["precipitation"]], startCol = 2, startRow = 4, colNames = FALSE)
    openxlsx::addStyle(wb, "precipitation", style = generalCommentStyle2, cols = 1, rows = 3)
    openxlsx::addStyle(wb, "precipitation", style = yesterdayGeneralCommentStyle2, cols = 1, rows = 4)
    openxlsx::mergeCells(wb, "precipitation", cols = c(2:9), rows = 3)
    openxlsx::mergeCells(wb, "precipitation", cols = c(2:9), rows = 4)
    openxlsx::addStyle(wb, "precipitation", style = generalCommentStyle, cols = c(2:9), rows = 3, gridExpand = TRUE)
    openxlsx::addStyle(wb, "precipitation", style = yesterdayGeneralCommentStyle, cols = c(2:9), rows = 4, gridExpand = TRUE)
    openxlsx::writeData(wb, "precipitation", NA, startCol = 1, startRow = 5, colNames = FALSE) #empty row before comment
    openxlsx::writeData(wb, "precipitation", "Mean precip estimates upstream of locations are derived from HRDPA (reanalysis) and HRDPS (forecast) products. Beware: combination of liquid + solid precip.", startCol = 1, startRow = 6, colNames = FALSE)
    openxlsx::mergeCells(wb, "precipitation", cols = c(1:9), rows = 6)
    openxlsx::writeData(wb, "precipitation", NA, startCol = 1, startRow = 7, colNames = FALSE) #empty row after comment)
    #add content
    openxlsx::writeData(wb, "precipitation",  tables[["precipitation"]], startRow = 8)
    #format for ease of viewing
    openxlsx::freezePane(wb, sheet = "precipitation", firstActiveRow = 9, firstActiveCol = 3)
    openxlsx::setColWidths(wb, "precipitation", cols = c(1:10), widths = c(10, 30, 14, 14, 14, 14, 14, 14, 60, 60))
    openxlsx::addStyle(wb, "precipitation", headStyle, rows = 8, cols = c(1:10))
    openxlsx::addStyle(wb, "precipitation", fodCommentStyle, rows = 1:nrow(tables[["precipitation"]])+8, cols = 9)
    openxlsx::addStyle(wb, "precipitation", yesterdayFodCommentStyle, rows = 1:nrow(tables[["precipitation"]])+8, cols = 10)
    #Conditional format
    precipYellowStyle <- openxlsx::createStyle(fontColour = "black", textDecoration = "bold", border = "TopBottomLeftRight", borderColour = "goldenrod1", borderStyle = "thick")
    precipRedStyle <- openxlsx::createStyle(fontColour = "black", textDecoration = "bold", border = "TopBottomLeftRight", borderColour = "red2", borderStyle = "thick")
    openxlsx::conditionalFormatting(wb, "precipitation", rule = ">10", cols = c(6,7), rows = 1:nrow(tables[["precipitation"]]) + 8, style = precipYellowStyle) #24 hrs precip
    openxlsx::conditionalFormatting(wb, "precipitation", rule = ">20", cols = c(6,7), rows = 1:nrow(tables[["precipitation"]]) + 8, style = precipRedStyle) #24 hrs precip
    openxlsx::conditionalFormatting(wb, "precipitation", rule = ">15", cols = c(5, 8), rows = 1:nrow(tables[["precipitation"]]) + 8, style = precipYellowStyle) #48 hrs precip
    openxlsx::conditionalFormatting(wb, "precipitation", rule = ">25", cols = c(5, 8), rows = 1:nrow(tables[["precipitation"]]) + 8, style = precipRedStyle) #48 hrs precip
    openxlsx::conditionalFormatting(wb, "precipitation", rule = ">20", cols = 4, rows = 1:nrow(tables[["precipitation"]]) + 8, style = precipYellowStyle) #past 3 day precip
    openxlsx::conditionalFormatting(wb, "precipitation", rule = ">30", cols = 4, rows = 1:nrow(tables[["precipitation"]]) + 8, style = precipRedStyle) #past 3 day precip
    openxlsx::conditionalFormatting(wb, "precipitation", rule = ">40", cols = 3, rows = 1:nrow(tables[["precipitation"]]) + 8, style = precipYellowStyle) #past week precip
    openxlsx::conditionalFormatting(wb, "precipitation", rule = ">60", cols = 3, rows = 1:nrow(tables[["precipitation"]]) + 8, style = precipRedStyle) #past week precip
    #Add comments
    dayComment <- openxlsx::createComment("Yellow: > 10mm, Red: > 20mm", author = "Ghislain", visible = FALSE)
    twoDayComment <- openxlsx::createComment("Yellow: > 15mm, Red: > 25mm", author = "Ghislain", visible = FALSE)
    threeDayComment <- openxlsx::createComment("Yellow: > 20mm, Red: > 30mm", author = "Ghislain", visible = FALSE)
    weekComment <- openxlsx::createComment("Yellow: > 40mm, Red: > 60mm", author = "Ghislain", visible = FALSE)
    openxlsx::writeComment(wb, sheet = "precipitation", col = 6, row = 8, comment = dayComment)
    openxlsx::writeComment(wb, sheet = "precipitation", col = 7, row = 8, comment = dayComment)
    openxlsx::writeComment(wb, sheet = "precipitation", col = 5, row = 8, comment = twoDayComment)
    openxlsx::writeComment(wb, sheet = "precipitation", col = 8, row = 8, comment = twoDayComment)
    openxlsx::writeComment(wb, sheet = "precipitation", col = 4, row = 8, comment = threeDayComment)
    openxlsx::writeComment(wb, sheet = "precipitation", col = 3, row = 8, comment = weekComment)
  }
  openxlsx::saveWorkbook(wb, paste0(save_path, "/HydrometricReport_", Sys.Date(), ".xlsx"), overwrite = TRUE)
}
