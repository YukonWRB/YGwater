#' YOWN site comparative plot generation, in m bgs
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Plots multiple YOWN stations on the same chart
#'
#' @details
#' To store login credentials in your .renviron profile, call [usethis::edit_r_environ()] and enter your username and password as value pairs, as AQUSER="your username" and AQPASS = "your password".
#'
#' @param YOWNindex Character vector of YOWN site IDs (eg. c("YOWN-2201S", "YOWN-2201D", "YOWN-2202"))
#' @param tsunit Desired timeseries for plotting, CHOOSE FROM: "btoc", "bgs", "asl"
#' @param chartRange ???
#' @param chartXInterval ????
#' @param saveTo Location for data files to be saved as a character vector. Default "desktop" is to a new folder on your desktop; "choose" lets you interactively choose your save location.
#' @param login Your Aquarius login credentials as a character vector of two (eg. c("cmfische", "password") Default pulls information from your .renviron profile; see details. Passed to [aq_download()].
#' @param server Defaults to Yukon Water Resources Branch Aquarius web server. Passed to [aq_download()].
#'
#' @return Writes .pdf plot of WSC and YOWN data
#'
#' @export

#TODO: Fill in documentation above
#TODO: pass login and server parameters to the aq_download function

YOWNplot_SiteCompare <- function(YOWNindex,
                                 tsunit = "bgs",
                                 chartRange = "all",
                                 chartXInterval ="6 month",
                                 saveTo = "desktop",
                                 login = Sys.getenv(c("AQUSER", "AQPASS")),
                                 server ="https://yukon.aquaticinformatics.net/AQUARIUS") {

  YOWNindex = c("YOWN-1930S", "YOWN-1930D")
  tsunit = "bgs"
  chartRange = "all"
  chartXInterval ="6 month"
  saveTo = "desktop"
  login = Sys.getenv(c("AQUSER", "AQPASS"))
  server ="https://yukon.aquaticinformatics.net/AQUARIUS"

  # Sort out save location
  saveTo <- tolower(saveTo)
  if (saveTo %in% c("Choose", "choose")) {
    print("Select the folder where you want this graph saved.")
    saveTo <- as.character(utils::choose.dir(caption="Select Save Folder"))
  } else if(saveTo == "desktop") {
    saveTo <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/Desktop/")
  } else if (dir.exists(saveTo) == FALSE) {
    stop("Specified directory does not exist. Consider specifying save path as one of 'choose' or 'desktop'; refer to help file.")
  }

  # Format chart labels based on time series ID
  if(tsunit == "asl"){
    print("Generating plot in m asl")
    axislab <- "Water Level (m above sea level)"
    ts_name <- "Wlevel_masl.Calculated"
  } else if(tsunit == "btoc") {
    print("Generating plot in m btoc")
    axislab <- "Water Level (m below top of casing)"
    ts_name <- "Wlevel_btoc.Calculated"
  } else if(tsunit == "bgs"){
    print("Generating plot in m bgs")
    axislab <- "Water Level (m below ground surface)"
    ts_name <- "Wlevel_bgs.Calculated"
  } else {print("Please double check the type of plot specified")}

  # Create a list of data frames for plotting
  sitelist <- list()

  # Populate list
  for (i in YOWNindex) {

    #TODO: Cole, is line below intended as a debug/dev thing?
    print(i)

    # Download data from Aquarius
    #TODO: pass the aqts server ID, username, password to this function in case they ever change
    datalist <- YGwater::aq_download(loc_id = i,
                                      ts_name = ts_name,
                                     login = Sys.getenv(c("AQUSER", "AQPASS")),
                                     server ="https://yukon.aquaticinformatics.net/AQUARIUS")

    # Unlist time series data
    timeseries <- datalist$timeseries

    # Change AQTS number grades into letters, replace all below C with "Redacted"
    timeseries$grade_description[timeseries$grade_description != "A" & timeseries$grade_description != "B" & timeseries$grade_description != "C" & timeseries$grade_description != "MISSING DATA"] <- "REDACTED"

    # Replace all values with  grade of less than C with NA, to remove values from plots. This screens out GW recovery patterns
    timeseries$value[timeseries$grade_description != "A" & timeseries$grade_description != "B" & timeseries$grade_description != "C" & timeseries$grade_description != "Missing Data"] <- NA

    # Change timestamps from UTC to MST
    attr(timeseries$datetime , "tzone") <- "MST"
    names(timeseries)[names(timeseries) == "datetime"] <- "timestamp_MST"

    #Find data gaps of greater than 6 hours (indicative of logger failure) and generate value NA data sets to fill in gaps
    timeseries$ts_lag <- dplyr::lag(timeseries$timestamp_MST)
    timeseries$lag_val <- difftime(timeseries$timestamp_MST, timeseries$ts_lag, units = "hours")
    gapdf <- timeseries %>%
      dplyr::filter(.data$lag_val > 6)
    gapdf$lag_val <- as.numeric(gapdf$lag_val)

    # If there are gaps present, fill in gaps with hourly timestamps
    if(nrow(gapdf != 0)){
      # Create a list of data frames for each identified data gap, fill in hourly time stamps
      gaplist <- list()
      for(j in 1:nrow(gapdf)) {
        df <- data.frame(seq.POSIXt(from = gapdf[j, 1], by = "-1 hour", length.out = gapdf[j, 8]), NA, as.character(-5), "MISSING DATA", gapdf$approval_level[j], gapdf$approval_description[j], NA, NA)
        colnames(df) <- colnames(gapdf)
        gaplist[[j]] <- df
      }

      # Merge all listed gap data frames, combine with original timeseries, order and format. If no gaps proceed with base timeseries
      gapmerge <- do.call(rbind, gaplist)
      df <- suppressMessages(dplyr::full_join(timeseries, gapmerge))
    } else {
      df <- timeseries
    }
    df$YOWNID <- i
    df <- df %>%
      dplyr::select(.data$YOWNID, tidyselect::everything())
    sitelist[[i]] <- df
  }

  # Combine list into one data frame, adjust df to size based on chartRange function param (TODO)
  plotdf <- do.call(rbind, sitelist)
  rownames(plotdf) <- NULL

  # Plot data, format and export
  plot <- ggplot2::ggplot() +
    ggplot2::geom_line(data = plotdf, ggplot2::aes(x = .data$timestamp_MST, y = .data$value, group = .data$YOWNID, colour = .data$YOWNID),
                       na.rm = TRUE) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 1.5), nrow = 1)) +
    cowplot::theme_cowplot() +
    ggplot2::labs(y = paste0("Groundwater level (m ", tsunit, ")")) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(4.2, 1.6, 3.1, 1.2), "cm"),
                   panel.border = ggplot2::element_rect(color = "grey",
                                                        fill = NULL,
                                                        linewidth = 0.5),
                   axis.text.x = ggplot2::element_text(angle = 0,
                                                       hjust  = 0.5,
                                                       vjust = -0.5,
                                                       size = 10),
                   axis.line.x.bottom = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(hjust = 1,
                                                       size = 10),
                   axis.title.x = ggplot2::element_blank(),
                   axis.line.y.left = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_line(colour = "lightgrey", linewidth = 0.5, linetype = 1),
                   legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   legend.justification = "left",
                   legend.margin = ggplot2::margin(0,0,0,0),
                   legend.box.margin = ggplot2::margin(-10, 0, 0, -10),
                   legend.text = ggplot2::element_text(size = 9))

  # Format chart labels based on time series ID
  if(tsunit == "asl"){
    limits <- c(plyr::round_any(min(stats::na.omit(plotdf$value)), 0.5, f = floor), plyr::round_any(max(stats::na.omit(plotdf$value)), 0.5, f = ceiling))
    breaks <- seq(floor(min(stats::na.omit(plotdf$value))), ceiling(max(stats::na.omit(plotdf$value))), by = 0.25)

    plot <- plot + ggplot2::scale_y_continuous(name = "",
                                       limits = limits,
                                       breaks = breaks,
                                       expand = c(0, 0))
  } else if(tsunit == "bgs" | tsunit == "btoc") {
    limits <- c(plyr::round_any(max(stats::na.omit(plotdf$value)), 0.5, f = ceiling), plyr::round_any(min(stats::na.omit(plotdf$value)), 0.5, f = floor))
    breaks <- seq(ceiling(max(stats::na.omit(plotdf$value))), floor(min(stats::na.omit(plotdf$value))), by = -0.25)
    plot <- plot + ggplot2::scale_y_reverse(name = paste0("Groundwater level (m ", tsunit, " )"),
                                       limits = limits,
                                       breaks = breaks,
                                       expand = c(0, 0))
  }

  plot <- plot + ggplot2::scale_x_datetime(name = "",
                                   limits = c(min(plotdf$timestamp_MST), max(plotdf$timestamp_MST)),
                                   date_breaks = chartXInterval,
                                   date_labels = "%Y-%b",
                                   expand = c(0, 0))


  title <- ggplot2::ggplot() +
    ggplot2::geom_blank() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Groundwater Level Record: Site Comparison - Cowley Creek YOWN-1930") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0,
                                                      vjust = 0,
                                                      size = 14,
                                                      colour = "#244C5A",
                                                      face = "bold"),
                   plot.margin = ggplot2::unit(c(6.3, 0, 0, 0.51), "cm"))

  caption <- ggplot2::ggplot() +
    ggplot2::geom_blank() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste0("Period of Record: ", strftime(as.POSIXct(min(stats::na.omit(timeseries$datetime))), format = "%Y-%m-%d"), " to ", strftime(as.POSIXct(max(stats::na.omit(timeseries$timestamp_MST))), format = "%Y-%m-%d"), " (Date of last data entry)",  "\nPlot generated: ", Sys.Date(), "\nYukon Observation Well Network")) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0,
                                                      vjust = 0,
                                                      size = 9,
                                                      colour = "#464646"),
                   plot.margin = ggplot2::unit(c(-2.39, 0, 0, 0.6), "cm"))

  subtitle <- ggplot2::ggplot() +
    ggplot2::geom_blank() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste0("Source Data: Aquarius Time Series, ", tsunit)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0,
                                                      vjust = 0,
                                                      size = 10,
                                                      color = "#464646"),
                   plot.margin = ggplot2::unit(c(6.85, 0, 0, 0.6), "cm"))

  # Use plot_grid method to combine titles, captions, and main plot in proper orientation
  final <- cowplot::plot_grid(title, subtitle, plot, caption, ncol = 1, nrow = 4, rel_heights = c(0.1, 0.1, 2, 0.1))

  # Add final aesthetic tweaks, print plot onto template
  final_plot <- cowplot::ggdraw() +
    cowplot::draw_image("G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\4_YOWN_DATA_ANALYSIS\\1_WATER LEVEL\\00_AUTOMATED_REPORTING\\01_MARKUP_IMAGES\\template_nogrades.jpg") +
    cowplot::draw_plot(final)

  ggplot2::ggsave(plot = final_plot, filename = paste0(saveTo, "SiteCompare.pdf"),  height = 8.5, width = 11, units = "in")

  print("Site Comparison Plot Generated")

}
