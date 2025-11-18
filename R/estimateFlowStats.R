#' Function that predicts water flow statistics at creeks
#'
#' @description
#' The purpose of this script is to estimate flows of rivers/streams/creeks based on the flow at multiple other stations of choice. It was created because Highways (YG) often wants flows statistics for small streams from which water is pulled during highway work.
#'
#' Statistics for each station are averaged over all years/seasons available, except for all-time data points. Years or seasons with more than 5 percent missing data (this is the default) are excluded from the statistics. Additionally, only stations with a minimum of 15 years/seasons of data (this is the default) are retained. Next, statistics are expressed as m3/s/km2 and plotted to visualize outliers (plots). Each statistic is averaged for all stations and applied to the ungauged rivers/streams/creeks using their catchment area.
#'
#' @param gauged_stations The stations used for estimating flow. Given as a vector of the station code. Ex: c(08AA008, 09BC001). These must be locations in the databases' location table, column 'location'. In addition there must be correspondingly named polygons in the 'vectors' table under the 'Drainage basins' layer.
#' @param ungauged_area The area of the stations of interest. Given in square km and as a vector.
#' @param ungauged_name The name of the stations of interest, given as a character vector. The name is only used to differentiate between ungauged location stats in the output table. Note: ungauged_name must be the same length as ungauged_area.
#' @param perc The percent of data (days) required to include a year or season in the statistics. Default is 95.
#' @param record_length The length of record (# of years/seasons) at a station required to be included in the statistics. Default is 15.
#' @param con A connection to the database. Leave NULL to use the default connection for this package, which is automatically closed when finished.
#'
#' @return A list containing three objects: two table and a list of plots. The first table contains the estimated stats for the river/stream/creek of interest. The table contains 4 columns: location, season, stat and value. The location column is the name/names given in the 'ungauged_name' parameter. The season column refers to the season for which the stat was calculated, where annual is the entire year, fall is Sept. 1st - Oct 31st, freshet is April 20 - June 30, summer is July 1st - August 31st, and winter is Nov. 1st - April 19. The stat column column refers to the type of statistic. The difference between max and max_all is that max is the average maximum for that season and the max_all is the all-time maximum for that season. The second table is of all the statistics for the gauges water courses by km. The list of plots contains 5 ggplots that display the range of station flows normalized by area for the statistics of interest. They are useful for identifying outliers. The first contains the normalized flows separated into seasons and statistic. The following 4 display the normalized flows for the maximum statistic, the minimum statistic, the mean statistic and the max_all statistic.
#' @export

estimateFlowStats <- function(
  gauged_stations,
  ungauged_area,
  ungauged_name,
  perc = 95,
  record_length = 15,
  con = NULL
) {
  if (length(ungauged_area) != length(ungauged_name)) {
    stop("The length of ungauged_area and ungauged_name must be the same.")
  }

  if (perc < 0 | perc > 100) {
    stop("The value of perc must be between 0 and 100.")
  }

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }

  #### ------------------------------ Get data -----------------------------####
  ## Get predictor data

  flow_paramId <- DBI::dbGetQuery(
    con,
    "SELECT parameter_id FROM parameters WHERE param_name = 'flow'"
  )$parameter_id

  flow_all <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT locations.name, timeseries.location, measurements_calculated_daily_corrected.date, measurements_calculated_daily_corrected.value, locations.geom_id ",
      "FROM measurements_calculated_daily_corrected ",
      "INNER JOIN timeseries ON measurements_calculated_daily_corrected.timeseries_id = timeseries.timeseries_id ",
      "INNER JOIN locations ON timeseries.location = locations.location ",
      "WHERE timeseries.location IN ('",
      paste0(gauged_stations, collapse = "', '"),
      "') AND timeseries.parameter_id = ",
      flow_paramId,
      ";"
    )
  )

  ## Get areas for stations
  areas <- getVector(
    layer_name = "Drainage basins",
    feature_name = gauged_stations
  )
  if (nrow(areas) != length(gauged_stations)) {
    stop(
      "Not all stations have a corresponding polygon in the 'Drainage basins' layer. Found polygons for ",
      paste(areas$feature_name, collapse = ", "),
      " only."
    )
  }
  area <- terra::expanse(areas, unit = "km")
  areas <- as.data.frame(areas)
  areas$area <- area

  ## Add season column
  ## Add seasons (based off Benoit's)
  flow_all$season <- NA
  # Freshet: April 20 - June 30
  flow_all[
    format(flow_all$date, "%m-%d") >= "04-20" &
      format(flow_all$date, "%m-%d") <= "06-30",
  ]$season <- "freshet"
  # Summer: July 1st - August 31st
  flow_all[
    format(flow_all$date, "%m-%d") >= "07-01" &
      format(flow_all$date, "%m-%d") <= "08-31",
  ]$season <- "summer"
  # Fall: Sept. 1st - Oct 31st
  flow_all[
    format(flow_all$date, "%m-%d") >= "09-01" &
      format(flow_all$date, "%m-%d") <= "10-31",
  ]$season <- "fall"
  # Winter: Nov. 1st - April 19
  flow_all[
    format(flow_all$date, "%m-%d") >= "11-01" |
      format(flow_all$date, "%m-%d") <= "04-19",
  ]$season <- "winter"

  # Remove non-numeric, NAs and negative numbers in value
  flow_all$value <- as.numeric(flow_all$value)
  flow_all <- flow_all[!is.na(flow_all$value), ]
  flow_all <- flow_all[flow_all$value >= 0, ]

  ## Add year
  flow_all$year <- format(flow_all$date, "%Y")

  #### -------------------------- Calculate maximum -------------------------####
  perc <- perc / 100
  ## Calculate measured maximum of all years for each station.
  # Gives a count of the number of high water level seasons (may, June, July, August) are included (with at least 95% of days of data)
  flow_max <- flow_all %>%
    dplyr::group_by(.data$location, .data$year) %>%
    dplyr::summarise(
      max = round(max(.data$value, na.rm = TRUE), 5),
      count = sum(format(.data$date, "%m") %in% c('05', '06', '07', '08'))
    ) %>%
    dplyr::filter(!(.data$count < perc * 123)) %>%
    dplyr::group_by(.data$location) %>%
    dplyr::summarise(
      value = round(max(.data$max, na.rm = TRUE), 5),
      count = sum(!is.na(.data$count))
    ) %>%
    dplyr::mutate(stat = "max_all_time", season = "annual")

  ## Calculate measured summer maximum of all years for each station.
  flow_max_season <- flow_all %>%
    dplyr::filter(.data$season %in% c("freshet", "summer")) %>%
    dplyr::group_by(.data$location, .data$year, .data$season) %>%
    dplyr::summarise(max = round(max(.data$value, na.rm = TRUE), 5), ) %>%
    dplyr::group_by(.data$location, .data$season) %>%
    dplyr::summarise(
      value = round(max(.data$max, na.rm = TRUE), 5),
      count = sum(!is.na(.data$max))
    ) %>%
    dplyr::mutate(stat = "max_all_time")
  #### -------------------------- Calculate seasonal -------------------------####

  # Calculate min, max and mean for each location, year and season
  flow_seasonal <- flow_all %>%
    dplyr::group_by(.data$location, .data$year, .data$season) %>%
    dplyr::summarise(
      min = round(min(.data$value, na.rm = TRUE), 2),
      max = round(max(.data$value, na.rm = TRUE), 2),
      mean = round(mean(.data$value, na.rm = TRUE), 2),
      count = sum(!is.na(.data$value))
    ) # Number of days of data contributing to seasonal stat

  # Remove seasons with too little data. Anything with less than 95% of days is removed.

  flow_seasonal <- flow_seasonal %>%
    dplyr::filter(!(.data$season == "freshet" & .data$count < perc * (72))) %>%
    dplyr::filter(!(.data$season == "summer" & .data$count < perc * (62))) %>%
    dplyr::filter(!(.data$season == "fall" & .data$count < perc * (61))) %>%
    dplyr::filter(!(.data$season == "winter" & .data$count < perc * (171)))

  # Calculate means of min, max and mean for all years of data at each location
  flow_seasonal <- flow_seasonal %>%
    dplyr::group_by(.data$location, .data$season) %>%
    dplyr::summarise(
      mean_seasonal_min = round(mean(.data$min, na.rm = TRUE), 2),
      mean_seasonal_max = round(mean(.data$max, na.rm = TRUE), 2),
      mean_seasonal_mean = round(mean(.data$mean, na.rm = TRUE), 2),
      count = (sum(!is.na(.data$count)))
    ) # number of years of data contributing to all years stat

  # Transform to long format
  flow_seasonal <- flow_seasonal %>%
    tidyr::pivot_longer(
      cols = c("mean_seasonal_min", "mean_seasonal_max", "mean_seasonal_mean"),
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
    dplyr::summarise(
      mean = round(mean(.data$value, na.rm = TRUE), 5),
      count = sum(!is.na(.data$value))
    ) %>%
    dplyr::filter(!(.data$count < perc * 365)) %>%
    dplyr::group_by(.data$location) %>%
    dplyr::summarise(
      value = round(mean(.data$mean, na.rm = TRUE), 5),
      count = sum(!is.na(.data$count))
    ) %>%
    dplyr::mutate(stat = "mean_annual_mean")

  # Calculate min
  # For minimum, remove years with less than 95% of data in March and April
  min <- flow_all %>%
    dplyr::group_by(.data$location, .data$year) %>%
    dplyr::summarise(
      min = round(min(.data$value, na.rm = TRUE), 5), # get minimum
      count = sum(format(.data$date, "%m") %in% c('03', '04'))
    ) %>% # count number of measurements in March and April
    dplyr::filter(!(.data$count < perc * 61)) %>%
    dplyr::group_by(.data$location) %>%
    dplyr::summarise(
      value = round(mean(.data$min, na.rm = TRUE), 5), # caluclate ean minimum
      count = sum(!is.na(.data$count))
    ) %>%
    dplyr::mutate(stat = "mean_annual_min")

  # Calculate max
  # For maximum, remove years with less than 95% of data in May, June, July and August
  max <- flow_all %>%
    dplyr::group_by(.data$location, .data$year) %>%
    dplyr::summarise(
      max = round(max(.data$value, na.rm = TRUE), 5),
      count = sum(format(.data$date, "%m") %in% c('05', '06', '07', '08'))
    ) %>%
    dplyr::filter(!(.data$count < perc * 123)) %>%
    dplyr::group_by(.data$location) %>%
    dplyr::summarise(
      value = round(mean(.data$max, na.rm = TRUE), 5),
      count = sum(!is.na(.data$count))
    ) %>%
    dplyr::mutate(stat = "mean_annual_max")

  # Combine mean, min, max
  flow_annual <- rbind(mean, min, max)
  flow_annual$season <- "annual"

  # Remove those with less than 15 seasons
  flow_annual <- flow_annual[flow_annual$count >= record_length, ]

  ####--------------------- Combine annual and seasonal ----------------------####

  # Combine
  flow_stats <- rbind(flow_annual, flow_seasonal, flow_max, flow_max_season)

  ####-------------------- Calculate stats per kilometre ---------------------####

  flow_stats <- merge(
    flow_stats,
    areas[, c("feature_name", "area")],
    by.x = "location",
    by.y = "feature_name"
  )
  flow_stats_km <- flow_stats
  flow_stats_km$value <- round(flow_stats$value / flow_stats$area, 5)

  ## Plot
  colours <- c(
    "black",
    "#DC4405",
    "#773F65",
    "#F2A900",
    "#244C5A",
    "#C60D58",
    "#41763B",
    "#0097A9",
    "#7A9A01",
    "#CD7F32",
    "#912D2D",
    "#0729B1",
    "#E0082F",
    "#CF08E0",
    "#E0CF08",
    "#08E0AF",
    "#DCB005",
    "#DC5C05",
    "#5C05DC"
  )
  text_size <- 12

  plot_all <- ggplot2::ggplot(
    flow_stats_km,
    ggplot2::aes(.data$stat, .data$value, colour = .data$location)
  ) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(rows = dplyr::vars(.data$season)) +
    ggplot2::scale_color_manual(values = colours)
  # For maximum
  plot_max <- ggplot2::ggplot(
    flow_stats_km[flow_stats_km$stat == "mean_seasonal_max", ],
    ggplot2::aes(.data$season, .data$value, colour = .data$location)
  ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::facet_grid(cols = ggplot2::vars(.data$stat), scales = "free") +
    ggplot2::ylab("Discharge (m3/s/km2)") +
    ggplot2::xlab("Season") +
    ggplot2::labs(col = "Station") +
    ggplot2::theme(text = ggplot2::element_text(size = text_size)) +
    ggplot2::scale_color_manual(values = colours)
  # For minimum
  plot_min <- ggplot2::ggplot(
    flow_stats_km[flow_stats_km$stat == "mean_seasonal_min", ],
    ggplot2::aes(.data$season, .data$value, colour = .data$location)
  ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::facet_grid(cols = ggplot2::vars(.data$stat), scales = "free") +
    ggplot2::ylab("Discharge (m3/s/km2)") +
    ggplot2::xlab("Season") +
    ggplot2::labs(col = "Station") +
    ggplot2::scale_color_manual(values = colours)
  # For mean
  plot_mean <- ggplot2::ggplot(
    flow_stats_km[flow_stats_km$stat == "mean_seasonal_mean", ],
    ggplot2::aes(.data$season, .data$value, colour = .data$location)
  ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::facet_grid(cols = ggplot2::vars(.data$stat), scales = "free") +
    ggplot2::ylab("Discharge (m3/s/km2)") +
    ggplot2::xlab("Season") +
    ggplot2::labs(col = "Station") +
    ggplot2::scale_color_manual(values = colours)
  # For max_all
  plot_max_all <- ggplot2::ggplot(
    flow_stats_km[flow_stats_km$stat == "max_all_time", ],
    ggplot2::aes(.data$season, .data$value, colour = .data$location)
  ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::facet_grid(cols = ggplot2::vars(.data$stat), scales = "free") +
    ggplot2::ylab("Discharge (m3/s/km2)") +
    ggplot2::xlab("Season") +
    ggplot2::labs(col = "Station") +
    ggplot2::scale_color_manual(values = colours)

  # Calculate average of all stations
  flow_stats_km_agg <- flow_stats_km %>%
    dplyr::group_by(.data$season, .data$stat) %>%
    dplyr::summarise(value = round(mean(.data$value, na.rm = TRUE), 5))

  ####------------------- Apply to streams of interest ---------------------####
  predicted <- data.frame(
    "location" = character(),
    "season" = character(),
    "stat" = character(),
    "value" = numeric()
  )

  for (a in 1:length(ungauged_area)) {
    pred <- data.frame(
      location = ungauged_name[a],
      season = flow_stats_km_agg$season,
      stat = flow_stats_km_agg$stat,
      value = flow_stats_km_agg$value * ungauged_area[a]
    )
    predicted <- rbind(predicted, pred)
  }

  return(list(
    predicted,
    flow_stats,
    flow_stats_km,
    list(plot_all, plot_max, plot_min, plot_mean, plot_max_all)
  ))
}
