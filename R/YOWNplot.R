#' Generalized plotting for YOWN wells
#'
#' @description
#' Generates a plot of the specified YOWN well's water level data, with options for statistical calculations and formatting. The plot can be saved to a specified directory or displayed in the console.
#'
#' @details
#' To store login credentials in your .renviron profile, call [usethis::edit_r_environ()] and enter your username and password as value pairs, as AQUSER="your username" and AQPASS = "your password".
#'
#' @param AQID YOWN location for which a plot will be generated.
#' @param timeSeriesID Aquarius time series ID exactly as in Aquarius (ie. "Wlevel_bgs.Calculated", "Wlevel_masl.Calculated"). Defaults to m bgs.
#' @param chartXinterval X axis interval, can be specified "auto" for best fit calculation, or as desired (ie. "1 day", "1 month", "1 year", etc.). Defaults to "auto"
#' @param dateRange X axis limits, can be "all" for all data, "1yr" for most recent year of data, or vector of 2 in format c("2020/01/01", "2023/01/01"). Defaults to "all". Does not apply to stats = "line", which always plots the most current year of data.
#' @param stats Can be "line", "ribbon", or FALSE. Line shows years plotted in separate lines, ribbon shows max/min ribbon geom, and FALSE excludes stats. If set to "line", dateRange ignored as most current year of data will be plotted alongside historical data.
#' @param smooth Can be FALSE or a numeric day value (ie. 14) for plotting rolling average.
#' @param saveTo Optional directory in which the plot will be saved. Can specify "desktop" to automatically create YOWN ID folder on your desktop as save directory. Default NULL only outputs the plot to the console.
#' @param format If `saveTo` is not NULL, this parameter specifies the format of the saved plot. Default is "png", other option is "png".
#' @param login Your Aquarius login credentials as a character vector of two (eg. c("cmfische", "password") Default pulls information from your .renviron profile; see details. Passed to [aq_download()].
#' @param server The URL for your organization's Aquarius web server. Default is for the Yukon Water Resources Branch. Passed to [aq_download()].
#'
#' @return Writes a .pdf containing YOWN data in the specified directory.
#' @export

#TODO: Fill in documentation above

YOWNplot <- function(AQID,
                     timeSeriesID = "Wlevel_bgs.Calculated",
                     chartXinterval = "auto",
                     dateRange = "all",
                     stats = FALSE,
                     smooth = FALSE,
                     saveTo = "desktop",
                     format = "png",
                     login = Sys.getenv(c("AQUSER", "AQPASS")),
                     server ="https://yukon.aquaticinformatics.net/AQUARIUS") {

  # Debug and development params. Leave as comments.
  # AQID = "YOWN-1925"
  # timeSeriesID = "Wlevel_bgs.Calculated"
  # chartXinterval = "auto"
  # dateRange = "all"
  # stats = FALSE
  # smooth = FALSE
  # saveTo = "desktop"
  # format = "png"
  # login = Sys.getenv(c("AQUSER", "AQPASS"))
  # server = "https://yukon.aquaticinformatics.net/AQUARIUS"
  
  #### Setup ####
  # Sort out save location
  if (!is.null(saveTo)) {
    if (tolower(saveTo) == "desktop") {
      saveTo <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/Desktop/")
      format <- tolower(format)
      if (!format %in% c("png", "pdf")) {
        stop("Specify format as 'png' or 'pdf', or set 'saveTo' to NULL to only get a console output.")
      }
    } else if (saveTo == "choose") {
      # Select save path using GUI
      saveTo <- rstudioapi::selectDirectory(caption = "Select Save Folder", path = file.path(Sys.getenv("USERPROFILE"), "Desktop"))
    } else if (!dir.exists(saveTo)) {
      stop("Specified directory does not exist. Consider specifying save path as one of 'choose' or 'desktop'; refer to help file.")
    }
  }

  #### Download time series data from Aquarius, preliminary formatting ####
  # Download data from Aquarius
  message("Downloading data from Aquarius")
  datalist <- suppressMessages(YGwater::aq_download(loc_id = AQID,
                                           ts_name = timeSeriesID,
                                           login = login,
                                           server = server))

  # Unlist time series data
  timeseries <- datalist$timeseries

  # Replace all grades below D with "Redacted"
  timeseries$grade_description[timeseries$grade_description != "A" & timeseries$grade_description != "B" & timeseries$grade_description != "C" & timeseries$grade_description !=  "E" & timeseries$grade_description != "MISSING DATA"] <- "REDACTED"

  # Replace all values with  grade of less than C with NA, to remove values from plots. This screens out GW recovery patterns and bad data from plots and stat calculations
  timeseries$value[timeseries$grade_description != "A" & timeseries$grade_description != "B" & timeseries$grade_description != "C" & timeseries$grade_description != "D"] <- NA

  # Change timestamps from UTC to MST
  attr(timeseries$datetime , "tzone") <- "MST"
  names(timeseries)[names(timeseries) == "datetime"] <- "timestamp_MST"

  #### Data gap processing ####
  fulldf <- timeseries

  # Identify data gaps greater than 6 hours, fill with 1hr intervals of NA
  fulldf$ts_lag <- dplyr::lag(fulldf$timestamp_MST) # Calculate lag time between each timestamp
  fulldf$lag_val <- difftime(fulldf$timestamp_MST, fulldf$ts_lag, units = "hours") # format lag as hours
  gapdf <- fulldf %>% # filter gap df to gaps of more than 6 hours
    dplyr::filter(.data$lag_val > 6)
  gapdf$lag_val <- as.numeric(gapdf$lag_val) # convert to numeric

  # Create a list of data frames for each identified data gap, fill in time stamps with NA in "value" column
  if (nrow(gapdf != 0)) {
    gaplist <- list()
    for (i in 1:nrow(gapdf)) {
      df <- data.frame(seq.POSIXt(from = as.POSIXct(gapdf[i, 1]), by = "-1 hour", length.out = gapdf[i, "lag_val"]), NA, as.character(-5), "MISSING DATA", gapdf$approval_level[i], gapdf$approval_description[i], NA, NA)
      colnames(df) <- colnames(gapdf)
      gaplist[[i]] <- df
    }
    # Merge all listed gap data frames, combine with original timeseries, order and format
    gapmerge <- do.call(rbind, gaplist)
    gapmerge <- gapmerge[order(gapmerge$timestamp_MST),] # Order by timestamp
    fulldf <- suppressMessages(dplyr::full_join(fulldf, gapmerge))
  }

  #### Advanced data processing and stat calculations ####
  fulldf <- fulldf %>%
    dplyr::mutate("date" = format(fulldf$timestamp_MST, "%Y-%m-%d"), # Add date column (YMD)
                  "year" = format(fulldf$timestamp_MST, "%Y"), # Add year column
                  "month" = format(fulldf$timestamp_MST, "%m"), # Add month column
                  "day" = format(fulldf$timestamp_MST, "%d"), # Add day column
                  "monthday" = format(fulldf$timestamp_MST, "%m-%d")) # Add month-day column
  datestats <- suppressWarnings(dplyr::group_by(fulldf, date) %>% # Calculate statistics by date (ie. Jan. 1, 2000)
                                  dplyr::summarize("datemin" = min(.data$value, na.rm = TRUE), "datemax" = max(.data$value, na.rm = TRUE), "datemean" = mean(.data$value, na.rm = TRUE)))
  fulldf <- suppressMessages(dplyr::full_join(fulldf, datestats)) # Join full df to datestats
  daystats <- suppressWarnings(dplyr::group_by(fulldf, .data$monthday) %>% # Calculate year-over-year daily statistics (ie. Jan. 1)
                                 dplyr::summarize("daymin" = min(.data$datemin, na.rm = TRUE), "daymax" = max(.data$datemax, na.rm = TRUE), daymean = mean(.data$datemean, na.rm = TRUE), N = dplyr::n()))
  dayavg <- stats::na.omit(daystats)
  fulldf <- suppressMessages(dplyr::full_join(fulldf, daystats)) # Join fulldf to daystats

  # Final fulldf formatting
  fulldf <- fulldf[match(unique(fulldf$date), fulldf$date),] # Extract first occurrence of each date, to trim dataset to one entry per day
  is.na(fulldf) <- sapply(fulldf, is.infinite) # Replace infinite values with NA
  is.na(fulldf) <- sapply(fulldf, is.nan) # Replace NaN values with NA
  fulldf <- fulldf[order(fulldf$timestamp_MST),] # Order by timestamp
  fulldf <- fulldf[!duplicated(fulldf["timestamp_MST"]),] # Remove second entry for duplicated timestamps
  rownames(fulldf) <- NULL

  #### Plot-specific data formatting ####
  # Format and calculate x axis limits
  if (paste(tolower(dateRange), collapse = ",") == "all") {
    dateRange <- c(min(stats::na.omit(fulldf$timestamp_MST)), max(stats::na.omit(fulldf$timestamp_MST)))
  } else if (paste(tolower(dateRange), collapse = ",") == "1yr") {
    dateRange <- c((max(stats::na.omit(fulldf$timestamp_MST)) - lubridate::years(1)), max(stats::na.omit(fulldf$timestamp_MST)))
  } else if (length(dateRange) != 2) {
    message("Chart X limits in incorrect format")
  } else {
    dateRange <- as.POSIXct(x = dateRange, tz = "MST", format = "%Y/%m/%d")
    if (dateRange[2] > max(stats::na.omit(fulldf$timestamp_MST))) {
      dateRange[2] <- max(stats::na.omit(fulldf$timestamp_MST))
    }
  }
  # Trim data to specified limits
  plotdf <- subset(fulldf, fulldf$timestamp_MST >= (min(dateRange)) & fulldf$timestamp_MST <= (max(dateRange)))
  plotdf$monthday <- as.POSIXct(plotdf$monthday, format = "%m-%d")
  plotdf$year <- as.Date(plotdf$year, format = "%Y")

  # Calculate chart X interval if "auto" specified
  if (chartXinterval == "auto") {
    diff <- as.numeric(difftime(max(plotdf$timestamp), min(plotdf$timestamp), units = "days"))
    chartXinterval <- dplyr::case_when(
      diff < 180 ~ "1 week",
      diff >= 180 & diff < 730 ~ "1 month",
      diff >= 730 & diff < 1460 ~ "2 months",
      diff >= 1460 & diff < 2920 ~ "6 months",
      diff >= 2920 ~ "1 year")
  }

  # Apply smoothing function if specified
  if (is.numeric(smooth)) {
    plotdf <- plotdf %>%
      dplyr::mutate("datemean" = zoo::rollapply(data = plotdf$datemean, FUN = mean, width = smooth, partial = TRUE)) %>%
      dplyr::mutate("daymean" = zoo::rollapply(data = plotdf$daymean,  FUN = mean, width = smooth, partial = TRUE)) %>%
      dplyr::mutate("daymin" = zoo::rollapply(data = plotdf$daymin,  FUN = mean, width = smooth, partial = TRUE)) %>%
      dplyr::mutate("daymax" = zoo::rollapply(data = plotdf$daymax,  FUN = mean, width = smooth, partial = TRUE))
  } else if (smooth) {
    stop("ERROR: Specify smoothing value as a number")
  }

  #### Create and format extra metadata blocks for pdf plot ####
  # Create title block
  title <- ggplot2::ggplot() +
    ggplot2::geom_blank() +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0,
                                                      vjust = 0,
                                                      size = 14,
                                                      colour = "#244C5A",
                                                      face = "bold"),
                   plot.margin = ggplot2::unit(c(6.3, 0, 0, 0.51), "cm"))

  # Create subtitle block
  subtitle <- ggplot2::ggplot() +
    ggplot2::geom_blank() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste0("Source Data: ", AQID, "@", timeSeriesID,  "\nLatitude: ", datalist[["metadata"]][5, 2], ", ", "Longitude: ", datalist[["metadata"]][6, 2],", ", "Elevation: ", datalist[["metadata"]][7, 2], " ", datalist[["metadata"]][8, 2])) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0,
                                                      vjust = 0,
                                                      size = 10,
                                                      color = "#464646"),
                   plot.margin = ggplot2::unit(c(6.85, 0, 0, 0.6), "cm"))

  # Create caption block
  caption <- ggplot2::ggplot() +
    ggplot2::geom_blank() +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0,
                                                      vjust = 0,
                                                      size = 9,
                                                      colour = "#464646"),
                   plot.margin = ggplot2::unit(c(-2.39, 0, 0, 0.6), "cm"))

  #### Check for sufficient data for ribbon stat plot generation ####
  if (tolower(stats) == "ribbon" & (max(fulldf$timestamp_MST, na.rm = TRUE) - min(fulldf$timestamp_MST, na.rm = TRUE) < 730)) {
    warning("Insufficient data for ribbon stats calculation, plot produced with no stats instead")
    stats <- FALSE
  }

  #### Plot generation ####
  # Generate ribbon plot if specified
  if (tolower(stats) == "ribbon") {

    # Generate vector of TRUE/FALSE to stop GGplot from filling in gaps when NA values exist
    NAcomp <- rle(!is.na(plotdf$datemean))
    NAcomp$values[which(NAcomp$lengths > 1 & !NAcomp$values)] <- TRUE
    NAadd <- inverse.rle(NAcomp)

    # Assign year as factor variable
    plotdf$year <- factor(plotdf$year)

    # Create  base plot, add aesthetic tweaks
    plot <- ggplot2::ggplot() +
      ggplot2::geom_ribbon(data = plotdf,
                           ggplot2::aes(ymin = .data$daymin, ymax = .data$daymax, x = .data$timestamp_MST, fill = "Range of Historical Max & Min Daily Groundwater Levels")) +
      ggplot2::scale_fill_manual(name = "", values = c("Range of Historical Max & Min Daily Groundwater Levels" = "#B8BDC3")) +
      ggplot2::geom_line(data = plotdf,
                         ggplot2::aes(x = .data$timestamp_MST, y = .data$daymean, colour = "Historical Mean Daily Groundwater Level"),
                         linewidth = 0.3,
                         na.rm = TRUE) +
      ggplot2::scale_colour_manual(name = "", values = c("Historical Mean Daily Groundwater Level" = "#0097A9")) +
      ggnewscale::new_scale_colour() +
      ggplot2::geom_line(data = plotdf[NAadd,],
                         ggplot2::aes(x = .data$timestamp_MST, y = .data$datemean, colour = "Daily Average Groundwater Level"),
                         linewidth = 0.5,
                         na.rm = TRUE) +
      ggplot2::scale_colour_manual(name = "", values = c("Daily Average Groundwater Level" = "#244C5A")) +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 1.5))) +
      cowplot::theme_cowplot() +
      ggplot2::theme(plot.margin = ggplot2::unit(c(4.2, 1.6, 3.1, 1.2), "cm"),
                     panel.border = ggplot2::element_rect(color = "grey",
                                                          fill = NULL,
                                                          linewidth = 0.5),
                     axis.text.x = ggplot2::element_text(angle = 45,
                                                         hjust  = 1,
                                                         vjust = 1,
                                                         size = 10),
                     axis.line.x.bottom = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_text(hjust = 1,
                                                         size = 10),
                     axis.title.y = ggplot2::element_text(vjust = 2,
                                                          size = 12,
                                                          colour = "#464646"),
                     axis.line.y.left = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_line(colour = "lightgrey", linewidth = 0.5, linetype = 1),
                     legend.position = "bottom",
                     legend.justification = "left",
                     legend.margin = ggplot2::margin(0,0,0,0),
                     legend.box.margin = ggplot2::margin(-18, 0, 0, -10),
                     legend.text = ggplot2::element_text(size = 9)) +
      ggplot2::scale_x_datetime(name = "",
                                limits = as.POSIXct(dateRange),
                                date_breaks = chartXinterval,
                                date_labels = "%b. %d, %Y",
                                expand = c(0, 0))
    # Customize title block
    title <- title + ggplot2::labs(title = paste0("Groundwater Level Statistics Chart: ",
                                                  datalist[["metadata"]][1, 2], " ",
                                                  "(", datalist[["metadata"]][3, 2], ")"))

    # Customize caption block
    caption <- caption + ggplot2::labs(title = paste0("Max & Min data calculated from period of record from ",
                                                      strftime(as.POSIXct(min(stats::na.omit(timeseries$timestamp_MST))), format = "%Y-%m-%d"),
                                                      " to ",
                                                      strftime(as.POSIXct(max(stats::na.omit(timeseries$timestamp_MST))), format = "%Y-%m-%d"),
                                                      " (Date of last data entry)\n",
                                                      paste0("Smoothing applied to data: ", smooth, " day rolling mean"),
                                                      "\nPlot generated: ",
                                                      Sys.Date(),
                                                      "; Yukon Observation Well Network"))

  } else if (tolower(stats) == "line") {
    # Plotdf formatting
    plotdf <- fulldf
    plotdf$monthday <- as.POSIXct(plotdf$monthday, format = "%m-%d")
    plotdf$year <- as.Date(plotdf$year, format = "%Y")

    # Apply smoothing function if specified
    if (is.numeric(smooth)) {
      plotdf <- plotdf %>%
        dplyr::mutate("datemean" = zoo::rollapply(data = plotdf$datemean, FUN = mean, width = smooth, partial = TRUE)) %>%
        dplyr::mutate("daymean" = zoo::rollapply(data = plotdf$daymean,  FUN = mean, width = smooth, partial = TRUE)) %>%
        dplyr::mutate("daymin" = zoo::rollapply(data = plotdf$daymin,  FUN = mean, width = smooth, partial = TRUE)) %>%
        dplyr::mutate("daymax" = zoo::rollapply(data = plotdf$daymax,  FUN = mean, width = smooth, partial = TRUE))
    } else if (smooth) {
      stop("ERROR: Specify smoothing value as a number")
    }

    # Separate current year from historical
    plotdf_hist <- plotdf %>%
      dplyr::filter(.data$year != max(plotdf$year))

    plotdf_current <- plotdf %>%
      dplyr::filter(.data$year == max(plotdf$year))

    # Generate vector of TRUE/FALSE to stop GGplot from filling in gaps when NA values exist
    NAcompc <- rle(!is.na(plotdf_current$datemean))
    NAcompc$values[which(NAcompc$lengths>1 & !NAcompc$values)] <- TRUE
    NAaddc <- inverse.rle(NAcompc)

    # Generate vector of TRUE/FALSE to stop GGplot from filling in gaps when NA values exist
    NAcomph <- rle(!is.na(plotdf_hist$datemean))
    NAcomph$values[which(NAcomph$lengths>1 & !NAcomph$values)] <- TRUE
    NAaddh <- inverse.rle(NAcomph)

    # Create plot, add aesthetic tweaks
    plot <- ggplot2::ggplot() +
      ggplot2::geom_line(data = plotdf_hist[NAaddh,],
                         ggplot2::aes(x = .data$monthday,
                                      y = .data$datemean,
                                      group = .data$year,
                                      colour = .data$year),
                         linewidth = 0.2) +
      ggplot2::scale_colour_gradient(trans = "date",
                                     low = "#7A9A01",
                                     high = "#DC4405",
                                     breaks = c(min(plotdf_hist$year), max(plotdf_hist$year)),
                                     name = "",
                                     # labels = scales::label_date(format = "%Y"),
                                     labels = function(x) format(x, "%Y"),
                                     expand = 0) +
      ggnewscale::new_scale_color() +
      ggplot2::geom_line(data = plotdf_current[NAaddc,],
                         ggplot2::aes(x = .data$monthday,
                                      y = .data$datemean,
                                      group = 1,
                                      colour = "Water Level (title year)"),
                         linewidth = 1) +
      ggplot2::scale_colour_manual(name = "", values = c("Water Level (title year)" = "#244C5A")) +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(linewidth = 1.5))) +
      cowplot::theme_cowplot() +
      ggplot2::theme(plot.margin = ggplot2::unit(c(4.2, 1.6, 3.1, 1.2), "cm"),
                     panel.border = ggplot2::element_rect(color = "grey",
                                                          fill = NULL,
                                                          linewidth = 0.5),
                     axis.text.x = ggplot2::element_text(angle = 0,
                                                         hjust  = 0.5,
                                                         vjust = 1,
                                                         size = 10),
                     axis.line.x.bottom = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_text(hjust = 1,
                                                         size = 10),
                     axis.title.y = ggplot2::element_text(vjust = 2,
                                                          size = 12,
                                                          colour = "#464646"),
                     axis.line.y.left = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_line(colour = "lightgrey", linewidth = 0.5, linetype = 1),
                     legend.position = "bottom",
                     legend.title.align = 0.5,
                     legend.justification = "center",
                     legend.margin = ggplot2::margin(0,0,0,0),
                     legend.box.margin = ggplot2::margin(-18, 0, 0, -10),
                     legend.text = ggplot2::element_text(size = 9),
                     legend.key.width = ggplot2::unit(1, "cm")) +
      ggplot2::scale_x_datetime(name = "",
                                date_breaks = "1 month",
                                date_labels = "%b",
                                labels = as.POSIXct(plotdf$year, format = "%Y"),
                                expand = c(0, 0))
    # Create title block
    title <- title + ggplot2::labs(title = paste0("Groundwater Level Chart: ", datalist[["metadata"]][1, 2], " ", "(",datalist[["metadata"]][3, 2], ")", " in ", max(format(plotdf$year, "%Y"))))

    # Create caption block
    caption <- caption + ggplot2::labs(title = paste0("Period of record from ",
                                                      strftime(as.POSIXct(min(stats::na.omit(timeseries$timestamp_MST))), format = "%Y-%m-%d"),
                                                      " to ",
                                                      strftime(as.POSIXct(max(stats::na.omit(timeseries$timestamp_MST))), format = "%Y-%m-%d"),
                                                      " (Date of last data entry)\n",
                                                      paste0("Smoothing applied to data: ", smooth, " day rolling mean"),
                                                      "\nPlot generated: ",
                                                      Sys.Date(),
                                                      "; Yukon Observation Well Network"))
  } else if (!stats) {
    # Generate vector of TRUE/FALSE to stop GGplot from filling in gaps when NA values exist
    NAcomp <- rle(!is.na(plotdf$datemean))
    NAcomp$values[which(NAcomp$lengths > 1 & !NAcomp$values)] <- TRUE
    NAadd <- inverse.rle(NAcomp)

    # Generate plot
    plot <- ggplot2::ggplot() +
      ggplot2::geom_line(data = plotdf[NAadd,],
                         ggplot2::aes(x = .data$timestamp_MST,
                                      y = .data$value,
                                      colour = "Daily Average Water Level"),
                         linewidth = 1) +
      ggplot2::scale_colour_manual(name = "",
                                   values = c("Daily Average Water Level" = "#244C5A")) +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(linewidth = 1.5))) +
      cowplot::theme_cowplot() +
      ggplot2::theme(plot.margin = ggplot2::unit(c(4.2, 1.6, 3.1, 1.2), "cm"),
                     panel.border = ggplot2::element_rect(color = "grey",
                                                          fill = NULL,
                                                          linewidth = 0.5),
                     axis.text.x = ggplot2::element_text(angle = 45,
                                                         hjust  = 1,
                                                         vjust = 1,
                                                         size = 10),
                     axis.line.x.bottom = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_text(hjust = 1,
                                                         size = 10),
                     axis.title.y = ggplot2::element_text(vjust = 2,
                                                          size = 12,
                                                          colour = "#464646"),
                     axis.line.y.left = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_line(colour = "lightgrey", linewidth = 0.5, linetype = 1),
                     legend.position = "bottom",
                     legend.justification = "left",
                     legend.margin = ggplot2::margin(0,0,0,0),
                     legend.box.margin = ggplot2::margin(-18, 0, 0, -10),
                     legend.text = ggplot2::element_text(size = 9),
                     legend.key.width = ggplot2::unit(1, "cm")) +
      ggplot2::scale_x_datetime(name = "",
                                date_breaks = chartXinterval,
                                date_labels = "%b-%Y",
                                expand = c(0, 0))
    # Create title block
    title <- title + ggplot2::labs(title = paste0("Groundwater Level Chart: ", datalist[["metadata"]][1, 2], " ", "(", datalist[["metadata"]][3, 2], ")"))

    # Create caption block
    caption <- caption + ggplot2::labs(title = paste0("Period of record from ",
                                                      strftime(as.POSIXct(min(stats::na.omit(timeseries$timestamp_MST))), format = "%Y-%m-%d"),
                                                      " to ",
                                                      strftime(as.POSIXct(max(stats::na.omit(timeseries$timestamp_MST))), format = "%Y-%m-%d"),
                                                      " (Date of last data entry)\n",
                                                      paste0("Smoothing applied to data: ", smooth, " day rolling mean"),
                                                      "\nPlot generated: ",
                                                      Sys.Date(),
                                                      "; Yukon Observation Well Network"))
  }

  #### Final formatting of X and Y axes and adding grade colour lines ####
  # Set y axis title and name
  if (timeSeriesID == "Wlevel_btoc.Calculated") {
    ytitle <- "Water Level (m below top of casing)"
    name <- "mbtoc"
  } else if (timeSeriesID == "Wlevel_bgs.Calculated") {
    ytitle <- "Water Level (m below ground surface)"
    name <- "mbgs"
  } else if (timeSeriesID == "Wlevel_masl.Calculated") {
    ytitle <- "Water Level (m above sea level)"
    name <- "masl"
  }

  # If time series is bgs or btoc, check for stats and place grade colour line accordingly
  if (timeSeriesID == "Wlevel_bgs.Calculated" | timeSeriesID == "Wlevel_btoc.Calculated") {
    if (stats != FALSE) {
      plot <- plot + ggplot2::scale_y_reverse(name = ytitle,
                                              limits = c(round_any(max(stats::na.omit(plotdf$daymax)), 0.5, f = ceiling),
                                                         round_any(min(stats::na.omit(plotdf$daymin)), 0.5, f = floor)),
                                              breaks = seq(round_any(max(stats::na.omit(plotdf$daymax)), 0.5, f = ceiling),
                                                           round_any(min(stats::na.omit(plotdf$daymin)), 0.5, f = floor), by = -0.25),
                                              expand = c(0, 0))
      
      if (tolower(stats) == "ribbon") {
        plot <- plot +
          ggnewscale::new_scale_colour() +
          ggplot2::geom_path(data = plotdf,
                             ggplot2::aes(x = .data$timestamp_MST,
                                          y = round_any(max(stats::na.omit(.data$daymax)), 0.5, f = ceiling),
                                          colour = factor(.data$grade_description), group = 1),
                             linewidth = 2.5,
                             show.legend = FALSE) +
          ggplot2::scale_colour_manual(name = "Grades", values = c("A" = "#7A9A01",
                                                                   "B" = "#0097A9",
                                                                   "C" = "#F2A900",
                                                                   "D" = "#DC4405",
                                                                   "REDACTED" = "red",
                                                                   "MISSING DATA" = "black"))

      } else if (tolower(stats) == "line") {
        plot <- plot +
          ggnewscale::new_scale_colour() +
          ggplot2::geom_path(data = plotdf_current,
                             ggplot2::aes(x = .data$monthday,
                                          y = round_any(max(stats::na.omit(plotdf$datemax)), 0.5, f = ceiling),
                                          colour = factor(.data$grade_description), group = 1),
                             linewidth = 2.5,
                             show.legend = FALSE) +
          ggplot2::scale_colour_manual(name = "Grades", values = c("A" = "#7A9A01",
                                                                   "B" = "#0097A9",
                                                                   "C" = "#F2A900",
                                                                   "D" = "#DC4405",
                                                                   "REDACTED" = "red",
                                                                   "MISSING DATA" = "black"))
      }
    } else if (!stats) {
      plot <- plot +
        ggnewscale::new_scale_colour() +
        ggplot2::geom_path(data = plotdf,
                           ggplot2::aes(x = .data$timestamp_MST,
                                        y = round_any(max(stats::na.omit(.data$value)), 0.5, f = ceiling),
                                        colour = factor(.data$grade_description), group = 1),
                           linewidth = 2.5,
                           show.legend = FALSE) +
        ggplot2::scale_colour_manual(name = "Grades",
                                     values = c("A" = "#7A9A01",
                                                "B" = "#0097A9",
                                                "C" = "#F2A900",
                                                "D" = "#DC4405",
                                                "REDACTED" = "red",
                                                "MISSING DATA" = "black")) +
        ggplot2::scale_y_reverse(name = ytitle,
                                 limits = c(round_any(max(stats::na.omit(plotdf$value)), 0.5, f = ceiling),
                                            round_any(min(stats::na.omit(plotdf$value)), 0.25, f = floor)),
                                 breaks = seq(round_any(max(stats::na.omit(plotdf$value)), 0.5, f = ceiling),
                                              round_any(min(stats::na.omit(plotdf$value)), 0.25, f = floor), by = -0.25),
                                 expand = c(0, 0))
    }

  } else if (timeSeriesID == "Wlevel_masl.Calculated") {
    # If time series is masl, check for stats and place grade colour line accordingly
    if (stats != FALSE) {
      plot <- plot +
        ggnewscale::new_scale_colour() +
        ggplot2::geom_path(data = plotdf_current, ggplot2::aes(x = .data$timestamp_MST, y = round_any(min(stats::na.omit(.data$daymin)), 0.25, f = floor), colour = factor(.data$grade_description), group = 1), linewidth = 2.5, show.legend = FALSE) +
        ggplot2::scale_colour_manual(name = "Grades", values = c("A" = "#7A9A01",
                                                                 "B" = "#0097A9",
                                                                 "C" = "#F2A900",
                                                                 "D" = "#DC4405",
                                                                 "REDACTED" = "red",
                                                                 "MISSING DATA" = "black")) +
        ggplot2::scale_y_continuous(name = ytitle,
                                    limits = c(round_any(min(stats::na.omit(plotdf$daymin)), 0.25, f = floor), round_any(max(stats::na.omit(plotdf$daymax)), 0.5, f = ceiling)),
                                    breaks = seq(floor(min(stats::na.omit(plotdf$daymin))), ceiling(max(stats::na.omit(plotdf$daymax))), by = 0.25),
                                    expand = c(0, 0))
    } else if (!stats) {
      plot <- plot +
        ggnewscale::new_scale_colour() +
        ggplot2::geom_path(data = plotdf, ggplot2::aes(x = .data$timestamp_MST, y = round_any(min(stats::na.omit(.data$daymean)), 0.25, f = floor), colour = factor(.data$grade_description), group = 1), linewidth = 2.5, show.legend = FALSE) +
        ggplot2::scale_colour_manual(name = "Grades", values = c("A" = "#7A9A01",
                                                                 "B" = "#0097A9",
                                                                 "C" = "#F2A900",
                                                                 "D" = "#DC4405",
                                                                 "REDACTED" = "red",
                                                                 "MISSING DATA" = "black")) +
        ggplot2::scale_y_continuous(name = ytitle,
                                    limits = c(round_any(min(stats::na.omit(plotdf$daymean)), 0.25, f = floor), round_any(max(stats::na.omit(plotdf$daymean)), 0.5, f = ceiling)),
                                    breaks = seq(floor(min(stats::na.omit(plotdf$daymean))), ceiling(max(stats::na.omit(plotdf$daymean))), by = 0.25),
                                    expand = c(0, 0))
    }
  }
  # Set stats to numeric value of false for saving plot title
  if (!smooth) {
    smooth <- 0
  }
  #### Final combination of plot, title, subtitle, caption blocks, format and save plot ####

  # Use plot_grid method to combine titles, captions, and main plot in proper orientation
  final <- cowplot::plot_grid(title, subtitle, plot, caption, ncol = 1, nrow = 4, rel_heights = c(0.1, 0.1, 2, 0.1))

  # Draw arranged plots on background template file
  final_plot <- cowplot::ggdraw() +
    cowplot::draw_image(system.file("YOWNplot/Template_grades.jpg", package = "YGwater")) +
    cowplot::draw_plot(final)

  # Save plot to specified directory
  if (!is.null(saveTo)) {
    # Create save folder in specified directory
    dir.create(paste0(saveTo, "/", AQID), showWarnings = FALSE)
    # Final plot saving
    ggplot2::ggsave(plot = final_plot, filename = paste0(saveTo, "/", AQID, "/", AQID, "_", name, "_", "stats", stats, "_smooth", smooth, ".", format),  height = 8.5, width = 11, units = "in")
    
    message(paste0("Plot written to ", saveTo, "/", AQID))
  }
  return(plot)
}

