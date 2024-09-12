
#' Function that predicts water flow statistics at creeks
#'
#' @description
#' `r lifecycle::badge('experimental')`
#' 
#' The purpose of this script is to estimate flows of rivers/streams/creeks based on the flow at multiple other stations of choice. It was created because Highways (YG) often wants flows statistics for small streams from which water is pulled during highway work. 
#' 
#' Statistics for each station are averaged over all years/seasons available, except for all-time data points. Years or seasons with more than 5 percent missing data (this is the default) are excluded from the statistics. Additionally, only stations with a minimum of 15 years/seasons of data (this is the default) are retained. Next, statistics are expressed as m3/s/km2 and plotted to visualize outliers (plots). Each statistic is averaged for all stations and applied to the ungauged rivers/streams/creeks using their catchment area.
#' 
#' @param gauged_stations The stations used for estimating flow. Given as a vector of the staton code. Ex: c(08AA008, 09BC001).
#' @param ungauged_area The area of the stations of interest. Given in square km and as a vector.
#' @param ungauged_name The name of the stations of interest, given as a character vector. The name is only used to differentiate between ungauged location stats in the outputted table. Note: ungauged_name must be the same length as ungauged_area.
#' @param perc The percent of data (days) required to include a year or season in the statistics. Default is 95.
#' @param record_length The length of record (# of years/seasons) at a station required to be included in the statistics. Default is 15.
#' @return A list containing three objects: two table and a list of plots. The first table contains the estimated stats for the river/stream/creek of interest. The table contains 4 columns: location, season, stat and value. The location column is the name/names given in the 'ungauged_name' parameter. The season column refers to the season for which the stat was calculated, where annual is the entire year, fall is Sept. 1st - Oct 31st, freshet is April 20 - June 30, summer is July 1st - August 31st, and winter is Nov. 1st - April 19. The stat column column refers to the type of statistic. The difference between max and max_all is that max is the average maximum for that season and the max_all is the all-time maximum for that season. The second table is of all the statistics for the gauges water courses by km. The list of plots contains 5 ggplots that display the range of station flows normalized by area for the statistics of interest. They are useful for identifying outliers. The first contains the normalized flows separated into seasons and statistic. The following 4 display the normalized flows for the maximum statistic, the minimum statistic, the mean statistic and the max_all statistic.
#' @export


estimateFlowStats <- function(gauged_stations, ungauged_area, ungauged_name, perc = 95, record_length = 15) {
  
  #### ------------------------------ Get data -----------------------------####
  ## Get predictor data
  con <- YGwater::AquaConnect()
  
  flow_paramId <- DBI::dbGetQuery(con, "SELECT parameter_id FROM parameters WHERE param_name = 'discharge, river/stream'")$parameter_id
  
  flow_all <- DBI::dbGetQuery(con, 
                              paste0("SELECT locations.name, timeseries.location, calculated_daily.date, calculated_daily.value, locations.geom_id ",
                                     "FROM calculated_daily ",
                                     "INNER JOIN timeseries ON calculated_daily.timeseries_id = timeseries.timeseries_id ",
                                     "INNER JOIN locations ON timeseries.location = locations.location ",
                                     "WHERE timeseries.location IN ('", paste0(gauged_stations, collapse = "', '"), "') AND parameter = ", flow_paramId, ";"))
  
  DBI::dbDisconnect(con)
  
  ## Get areas for stations
  areas <- YGwater::getVector(layer_name = "Drainage basins", feature_name = gauged_stations)
  area <- terra::expanse(areas, unit = "km")
  areas <- as.data.frame(areas)
  areas$area <- area
  
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
  flow_all <- subset(flow_all, !is.na(value))
  flow_all <- flow_all[flow_all$value >= 0, ]
  
  ## Add year
  flow_all$year <- format(flow_all$date, "%Y")
  
  
  #### -------------------------- Calculate maximum -------------------------####
  perc <- perc/100
  ## Calculate measured maximum of all years for each station.
  # Gives a count of the number of high water level seasons (may, June, July, August) are included (with at least 95% of days of data)
  flow_max <- flow_all %>%
    dplyr::group_by(.data$location, .data$year) %>%
    dplyr::summarise(max = round(max(value, na.rm = TRUE), 5),
                     count = sum(format(date, "%m") %in% c('05', '06', '07', '08'))) %>%
    dplyr::filter(!(.data$count < perc*123)) %>%
    dplyr::group_by(.data$location) %>%
    dplyr::summarise(value = round(max(max, na.rm = TRUE), 5),
                     count = sum(!is.na(.data$count))) %>%
    dplyr::mutate(stat = "max_all", season = "annual")
  
  ## Calculate measured summer maximum of all years for each station.
  flow_max_season <- flow_all %>%
    dplyr::filter(.data$season %in% c("freshet", "summer")) %>%
    dplyr::group_by(.data$location, .data$year, .data$season) %>%
    dplyr::summarise(max = round(max(value, na.rm = TRUE), 5),
                     #count = sum(!is.na(value))
    ) %>%
    #dplyr::filter(!(count < perc*62)) %>%
    dplyr::group_by(.data$location, .data$season) %>%
    dplyr::summarise(value = round(max(max, na.rm = TRUE), 5),
                     count = sum(!is.na(max))) %>%
    dplyr::mutate(stat = "max_all")
  #### -------------------------- Calculate seasonal -------------------------####
  
  # Calculate min, max and mean for each location, year and season
  flow_seasonal <- flow_all %>%
    dplyr::group_by(.data$location, .data$year, .data$season) %>%
    dplyr::summarise(min = round(min(value, na.rm = TRUE), 2),
                     max = round(max(value, na.rm = TRUE), 2),
                     mean = round(mean(value, na.rm = TRUE), 2),
                     count = sum(!is.na(value))) # Number of days of data contributing to seasonal stat
  
  # Remove seasons with too little data. Anything with less than 95% of days is removed.
  
  flow_seasonal <- flow_seasonal %>%
    dplyr::filter(!(.data$season == "freshet" & .data$count < perc*(72))) %>%
    dplyr::filter(!(.data$season == "summer" & .data$count < perc*(62))) %>%
    dplyr::filter(!(.data$season == "fall" & .data$count < perc*(61))) %>%
    dplyr::filter(!(.data$season == "winter" & .data$count < perc*(171)))
  
  # Calculate means of min, max and mean for all years of data at each location
  flow_seasonal <- flow_seasonal %>%
    dplyr::group_by(.data$location, .data$season) %>%
    dplyr::summarise(min = round(mean(min, na.rm = TRUE), 2),
                     max = round(mean(max, na.rm = TRUE), 2),
                     mean = round(mean(mean, na.rm = TRUE), 2),
                     count = (sum(!is.na(.data$count)))) # number of years of data contributing to all years stat
  
  # Transform to long format
  flow_seasonal <- flow_seasonal %>%
    tidyr::pivot_longer(
      cols = c("min", "max", "mean"),
      names_to = "stat",
      values_to = "value"
    )
  
  # Remove those with less than 15 seasons
  flow_seasonal <- flow_seasonal[flow_seasonal$count >= record_length, ]
  
  ####------------------------- Calculate annual -----------------------------####
  
  # Calculate mean
  # For mean, remove years with less than 95% of data
  mean <- flow_all %>%
    dplyr::group_by(.data$location, .data$year) %>%
    dplyr::summarise(mean = round(mean(value, na.rm = TRUE), 5),
                     count = sum(!is.na(value))) %>%
    dplyr::filter(!(.data$count < perc*365)) %>%
    dplyr::group_by(.data$location) %>%
    dplyr::summarise(value = round(mean(mean, na.rm = TRUE), 5),
                     count = sum(!is.na(.data$count))) %>%
    dplyr::mutate(stat = "mean")
  
  # Calculate min
  # For minimum, remove years with less than 95% of data in March and April
  min <- flow_all %>%
    dplyr::group_by(.data$location, .data$year) %>%
    dplyr::summarise(min = round(min(value, na.rm = TRUE), 5), # get minimum
                     count = sum(format(date, "%m") %in% c('03', '04'))) %>% # count number of measurements in March and April
    dplyr::filter(!(.data$count < perc*61)) %>%
    dplyr::group_by(.data$location) %>%
    dplyr::summarise(value = round(mean(min, na.rm = TRUE), 5), # caluclate ean minimum
                     count = sum(!is.na(.data$count))) %>%
    dplyr::mutate(stat = "min")
  
  # Calculate max
  # For maximum, remove years with less than 95% of data in May, June, July and August
  max <- flow_all %>%
    dplyr::group_by(.data$location, .data$year) %>%
    dplyr::summarise(max = round(max(value, na.rm = TRUE), 5),
                     count = sum(format(date, "%m") %in% c('05', '06', '07', '08'))) %>%
    dplyr::filter(!(.data$count < perc*123)) %>%
    dplyr::group_by(.data$location) %>%
    dplyr::summarise(value = round(mean(max, na.rm = TRUE), 5),
                     count = sum(!is.na(.data$count))) %>%
    dplyr::mutate(stat = "max")
  
  # Combine mean, min, max
  flow_annual <- rbind(mean, min, max)
  flow_annual$season <- "annual"
  
  # Remove those with less than 15 seasons
  flow_annual <- flow_annual[flow_annual$count >= record_length, ]
  
  
  ####--------------------- Combine annual and seasonal ----------------------####
  
  # Combine
  flow_stats <- rbind(flow_annual, flow_seasonal, flow_max, flow_max_season)
  
  
  ####-------------------- Calculate stats per kilometre ---------------------####
  
  flow_stats <- merge(flow_stats, areas[, c("feature_name", "area")], by.x = "location", by.y = "feature_name")
  flow_stats_km <- flow_stats
  flow_stats_km$value <- round(flow_stats$value / flow_stats$area, 5)
  
  
  ## Plot
  # Reformat 
  flow_stats_km$stat <- sub("min", "minimum", flow_stats_km$stat)
  flow_stats_km$stat <- sub("max", "maximum", flow_stats_km$stat)
  flow_stats_km$stat <- sub("maximum_all", "all-time maximum", flow_stats_km$stat)
  flow_stats_km <<- flow_stats_km
  
  colours <- c("black", "#DC4405", "#773F65", "#F2A900", "#244C5A", "#C60D58", "#41763B", "#0097A9", "#7A9A01", "#CD7F32", "#912D2D", "#0729B1", "#E0082F", "#CF08E0", "#E0CF08", "#08E0AF", "#DCB005", "#DC5C05", "#5C05DC")
  text_size <- 12
    
  plot_all <- ggplot2::ggplot(flow_stats_km, ggplot2::aes(stat, value, colour=.data$location)) + 
    ggplot2::geom_point() + 
    ggplot2::facet_grid(rows=dplyr::vars(season)) +
    ggplot2::scale_color_manual(values=colours)
  # For maximum
  plot_max <- ggplot2::ggplot(flow_stats_km[flow_stats_km$stat=="maximum",], ggplot2::aes(season, value, colour=location)) + ggplot2::geom_point(size=3) + ggplot2::facet_grid(cols=ggplot2::vars(stat), scales = "free") + ggplot2::ylab("Discharge (m3/s/km2)") + ggplot2::xlab("Season") + ggplot2::labs(col="Station") + ggplot2::theme(text = ggplot2::element_text(size = text_size)) + ggplot2::scale_color_manual(values=colours)
  # For minimum
  plot_min <- ggplot2::ggplot(flow_stats_km[flow_stats_km$stat=="minimum",], ggplot2::aes(season, value, colour=location)) + ggplot2::geom_point(size=3) + ggplot2::facet_grid(cols=ggplot2::vars(stat), scales = "free") + ggplot2::ylab("Discharge (m3/s/km2)") + ggplot2::xlab("Season") + ggplot2::labs(col="Station") + ggplot2::scale_color_manual(values=colours)
  # For mean
  plot_mean <- ggplot2::ggplot(flow_stats_km[flow_stats_km$stat=="mean",], ggplot2::aes(season, value, colour=location)) + ggplot2::geom_point(size=3) + ggplot2::facet_grid(cols=ggplot2::vars(stat), scales = "free") + ggplot2::ylab("Discharge (m3/s/km2)") + ggplot2::xlab("Season") + ggplot2::labs(col="Station") + ggplot2::scale_color_manual(values=colours)
  # For max_all
  plot_max_all <- ggplot2::ggplot(flow_stats_km[flow_stats_km$stat=="all-time maximum",], ggplot2::aes(season, value, colour=location)) + ggplot2::geom_point(size=3) + ggplot2::facet_grid(cols=ggplot2::vars(stat), scales = "free") + ggplot2::ylab("Discharge (m3/s/km2)") + ggplot2::xlab("Season") + ggplot2::labs(col="Station") + ggplot2::scale_color_manual(values=colours)
  
  # Calculate average of all stations
  flow_stats_km_agg <- flow_stats_km %>%
    dplyr::group_by(.data$season, .data$stat) %>%
    dplyr::summarise(value = round(mean(value, na.rm = TRUE), 5))
  
  
  ####------------------- Apply to streams of interest ---------------------####
  predicted <- data.frame("location" = character(),
                          "season" = character(),
                          "stat" = character(),
                          "value" = numeric())
  
  for (a in 1:length(ungauged_area)) {
    pred <- data.frame(location = ungauged_name[a],
                       season = flow_stats_km_agg$season,
                       stat = flow_stats_km_agg$stat,
                       value = flow_stats_km_agg$value * ungauged_area[a])
    predicted <- rbind(predicted, pred)
  }

  
  return(list(predicted, flow_stats, flow_stats_km, list(plot_all, plot_max, plot_min, plot_mean, plot_max_all)))
  #return(list(predicted, plot))
}

