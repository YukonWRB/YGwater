#' Function that calculates water flow statistics at creeks
#'
#' @description
#' The purpose of this script is to calculate flows of rivers/streams/creeks. It was created because Highways (YG) often wants flows statistics for small streams from which water is pulled during highway work. 
#' 
#' Statistics for each station are averaged over all years/seasons available, except for all-time data points. Years or seasons with more than 5 percent missing data (this is the default) are excluded from the statistics. Additionally, only stations with a minimum of 15 years/seasons of data (this is the default) are retained.
#' 
#' @param stations The stations for which to calculate flow. Given as a vector of the station code matching the database's location table, 'location' column. Ex: c('08AA008', '09BC001'). Leave NULL if using the `data` argument instead.
#' @param data If the stations are not in the hydromet database, provide the data as a single table with the columns: name, location, date, value. Name is the name of the station, location is the location code or id, date is the date of measurement in yyyy-mm-dd, and value is the flow measurement in m3/s.
#' @param perc The percent of data (days) required to include a year or season in the statistics. Default is 95.
#' @param record_length The length of record (# of years/seasons) at a station required to be included in the statistics. Default is 15.
#' @param con A connection to the database. Leave NULL to use the default connection for this package, which is automatically closed when finished.
#' @return A table containing the calculated stats for the river/stream/creek of interest. The table contains 4 columns: location, season, stat and value. The season column refers to the season for which the stat was calculated, where annual is the entire year, fall is Sept. 1st - Oct 31st, freshet is April 20 - June 30, summer is July 1st - August 31st, and winter is Nov. 1st - April 19. The stat column column refers to the type of statistic. The difference between max and max_all is that max is the average maximum for that season and the max_all is the all-time maximum for that season. 
#' @export
#' 


calculateFlowStats <- function(stations = NULL, data = NULL, perc = 95, record_length = 15, con = NULL) {
  
  if (is.null(stations) & is.null(data)) {
    stop("You must provide either the stations or the data.")
  } else if (!is.null(stations) & !is.null(data)) {
    stop("You must provide either the stations or the data, not both.")
  }
  
  if (perc < 0 | perc > 100) {
    stop("The value of perc must be between 0 and 100.")
  }
  
  #### ------------------------------ Get data -----------------------------####
  
  if (is.null(data)) {
    ## Get data
    
    if (is.null(con)) {
      con <- AquaConnect(silent = TRUE)
      on.exit(DBI::dbDisconnect(con))
    }
    
    flow_paramId <- DBI::dbGetQuery(con, "SELECT parameter_id FROM parameters WHERE param_name = 'flow'")$parameter_id
    flow_all <- DBI::dbGetQuery(con, 
                                paste0("SELECT locations.name, timeseries.location, measurements_calculated_daily_corrected.date, measurements_calculated_daily_corrected.value ",
                                       "FROM measurements_calculated_daily_corrected ",
                                       "INNER JOIN timeseries ON measurements_calculated_daily_corrected.timeseries_id = timeseries.timeseries_id ",
                                       "INNER JOIN locations ON timeseries.location = locations.location ",
                                       "WHERE timeseries.location IN ('", paste0(stations, collapse = "', '"), "') AND parameter_id = ", flow_paramId, ";"))
    
  } else {flow_all <- data}
  
  ## Add season column
  ## Add seasons (based off Benoit's)
  flow_all$season <- NA
  # Freshet: April 20 - June 30
  flow_all[format(flow_all$date, "%m-%d") >= "04-20" & format(flow_all$date, "%m-%d") <= "06-30",]$season <- "freshet"
  # Summer: July 1st - August 31st
  flow_all[format(flow_all$date, "%m-%d") >= "07-01" & format(flow_all$date, "%m-%d") <= "08-31",]$season <- "summer"
  # Fall: Sept. 1st - Oct 31st
  flow_all[format(flow_all$date, "%m-%d") >= "09-01" & format(flow_all$date, "%m-%d") <= "10-31",]$season <- "fall"
  # Winter: Nov. 1st - April 19
  flow_all[format(flow_all$date, "%m-%d") >= "11-01" | format(flow_all$date, "%m-%d") <= "04-19",]$season <- "winter"
  
  # Remove non-numeric, NAs and negative numbers in value
  flow_all$value <- as.numeric(flow_all$value)
  flow_all <- flow_all[!is.na(flow_all$value), ]
  flow_all <- flow_all[flow_all$value >= 0, ]
  
  ## Add year
  flow_all$year <- format(flow_all$date, "%Y")
  
  
  #### -------------------------- Calculate maximum -------------------------####
  perc <- perc/100
  ## Calculate measured maximum of all years for each station.
  # Gives a count of the number of high water level seasons (may, June, July, August) are included (with at least 95% of days of data)
  flow_max <- flow_all %>%
    dplyr::group_by(.data$location, .data$year) %>%
    dplyr::summarise(max = round(max(.data$value, na.rm = TRUE), 5),
                     count = sum(format(date, "%m") %in% c('05', '06', '07', '08'))) %>%
    dplyr::filter(!(.data$count < perc*123)) %>%
    dplyr::group_by(.data$location) %>%
    dplyr::summarise(value = round(max(max, na.rm = TRUE), 5),
                     count = sum(!is.na(.data$count))) %>%
    dplyr::mutate(stat = "max_all_time", season = "annual")
  
  ## Calculate measured summer maximum of all years for each station.
  flow_max_season <- flow_all %>%
    dplyr::filter(.data$season %in% c("freshet", "summer")) %>%
    dplyr::group_by(.data$location, .data$year, .data$season) %>%
    dplyr::summarise(max = round(max(.data$value, na.rm = TRUE), 5),
    ) %>%
    dplyr::group_by(.data$location, .data$season) %>%
    dplyr::summarise(value = round(max(max, na.rm = TRUE), 5),
                     count = sum(!is.na(max))) %>%
    dplyr::mutate(stat = "max_all_time")
  #### -------------------------- Calculate seasonal -------------------------####
  
  # Calculate min, max and mean for each location, year and season
  flow_seasonal <- flow_all %>%
    dplyr::group_by(.data$location, .data$year, .data$season) %>%
    dplyr::summarise(min = round(min(.data$value, na.rm = TRUE), 2),
                     max = round(max(.data$value, na.rm = TRUE), 2),
                     mean = round(mean(.data$value, na.rm = TRUE), 2),
                     count = sum(!is.na(.data$value))) # Number of days of data contributing to seasonal stat
  
  # Remove seasons with too little data. Anything with less than the specified percentage of days is removed.
  
  flow_seasonal <- flow_seasonal %>%
    dplyr::filter(!(.data$season == "freshet" & .data$count < perc*(72))) %>%
    dplyr::filter(!(.data$season == "summer" & .data$count < perc*(62))) %>%
    dplyr::filter(!(.data$season == "fall" & .data$count < perc*(61))) %>%
    dplyr::filter(!(.data$season == "winter" & .data$count < perc*(171)))
  
  # Calculate means of min, max and mean for all years of data at each location
  flow_seasonal <- flow_seasonal %>%
    dplyr::group_by(.data$location, .data$season) %>%
    dplyr::summarise(mean_seasonal_min = round(mean(.data$min, na.rm = TRUE), 2),
                     mean_seasonal_max = round(mean(.data$max, na.rm = TRUE), 2),
                     mean_seasonal_mean = round(mean(.data$mean, na.rm = TRUE), 2),
                     count = (sum(!is.na(.data$count)))) # number of years of data contributing to all years stat
  
  # Transform to long format
  flow_seasonal <- flow_seasonal %>%
    tidyr::pivot_longer(
      cols = c("mean_seasonal_min", "mean_seasonal_max", "mean_seasonal_mean"),
      names_to = "stat",
      values_to = "value"
    )
  
  # Remove those with less than  the specified number of seasons
  flow_seasonal <- flow_seasonal[flow_seasonal$count >= record_length, ]
  
  ####------------------------- Calculate annual -----------------------------####
  
  # Calculate mean
  # For mean, remove years with less than 95% of data
  mean <- flow_all %>%
    dplyr::group_by(.data$location, .data$year) %>%
    dplyr::summarise(mean = round(mean(.data$value, na.rm = TRUE), 5),
                     count = sum(!is.na(.data$value))) %>%
    dplyr::filter(!(.data$count < perc*365)) %>%
    dplyr::group_by(.data$location) %>%
    dplyr::summarise(value = round(mean(.data$mean, na.rm = TRUE), 5),
                     count = sum(!is.na(.data$count))) %>%
    dplyr::mutate(stat = "mean_annual_mean")
  
  # Calculate min
  # For minimum, remove years with less than 95% of data in March and April
  min <- flow_all %>%
    dplyr::group_by(.data$location, .data$year) %>%
    dplyr::summarise(min = round(min(.data$value, na.rm = TRUE), 5), # get minimum
                     count = sum(format(.data$date, "%m") %in% c('03', '04'))) %>% # count number of measurements in March and April
    dplyr::filter(!(.data$count < perc*61)) %>%
    dplyr::group_by(.data$location) %>%
    dplyr::summarise(value = round(mean(.data$min, na.rm = TRUE), 5), # caluclate ean minimum
                     count = sum(!is.na(.data$count))) %>%
    dplyr::mutate(stat = "mean_annual_min")
  
  # Calculate max
  # For maximum, remove years with less than 95% of data in May, June, July and August
  max <- flow_all %>%
    dplyr::group_by(.data$location, .data$year) %>%
    dplyr::summarise(max = round(max(.data$value, na.rm = TRUE), 5),
                     count = sum(format(.data$date, "%m") %in% c('05', '06', '07', '08'))) %>%
    dplyr::filter(!(.data$count < perc*123)) %>%
    dplyr::group_by(.data$location) %>%
    dplyr::summarise(value = round(mean(.data$max, na.rm = TRUE), 5),
                     count = sum(!is.na(.data$count))) %>%
    dplyr::mutate(stat = "mean_annual_max")
  
  # Combine mean, min, max
  flow_annual <- rbind(mean, min, max)
  flow_annual$season <- "annual"
  
  # Remove those with less than the required number of seasons
  flow_annual <- flow_annual[flow_annual$count >= record_length, ]
  
  
  ####--------------------- Combine annual and seasonal ----------------------####
  
  # Combine
  flow_stats <- rbind(flow_annual, flow_seasonal, flow_max, flow_max_season)
  
  
  return(flow_stats)
  #return(list(predicted, plot))
}
