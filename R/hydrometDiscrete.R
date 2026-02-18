#' Discrete hydrometric data plotting
#'
#' @description
#' Deprecated for general usage but retained for use with snow bulletin. Use [plotDiscrete()] instead.
#'
#' Generate plots of snow survey data (snow water equivalent and snow depth) or other variables sampled at regular intervals (weekly or monthly).
#'
#' Notice: in many cases, you're better off using the Shiny app at [YGwater()] to generate and export your plot. Read on if you need additional control over the final product.
#'
#' This function plots data from the aquacache database (maintained by the AquaCache package) and yields consistent-looking plots for discrete data. This function can only plot what's in the database. Data can be represented as violin plots, as regular box plots or as a 'linedbox' plot (imitates plots currently used in the snow bulletin).
#'
#' @param location The location for which you want a plot. Can be left NULL if `discrete_data` is provided.
#' @param parameter The parameter you wish to plot. The location:parameter combo must be in the local database unless `discrete_data` is provided. In this later case, the parameter must still be specified to label the y-axis.
#' @param startDay Can be specified as a number from 1 to 365, as a character string of form "yyyy-mm-dd", or as a date object. Either way the day of year is the only portion used, specify years to plot under parameter `years`.
#' @param endDay The end day of year for the plot x-axis. As per `startDay`.
#' @param binwidth The width of the bins in the violin/boxplot in days, specified as an integer of **n** days or as "1 day", "1 week", "1 month", etc. Default is one month.
#' @param tzone The timezone to use for fetching data. Datetimes are stored in the database in UTC offset, so this parameter could make a difference to what day a particular sample is considered to be on. In most cases you can ignore this parameter.
#' @param years The years to plot. If `startDay` and `endDay` cover December 31 - January 1, select the December year(s). Max 10 years, NULL = current year.
#' @param title Should a title be included?
#' @param custom_title Custom title to be given to the plot. Default is NULL, which will set the title as such: Location 09AB004: Marsh Lake Near Whitehorse.
#' @param plot_type Choose from "violin" , "boxplot" or "linedbox".
#' @param plot_scale Adjusts/scales the size of plot text elements. 1 = standard size, 0.5 = half size, 2 = double the size, etc. Standard size works well in a typical RStudio environment.
#' @param save_path Default is NULL and the graph will be visible in RStudio and can be assigned to an object. Option "choose" brings up the File Explorer for you to choose where to save the file, or you can also specify a save path directly.
#' @param con A connection to the target database. NULL uses [AquaConnect()] and automatically disconnects.
#' @param discrete_data A data.frame with the data to be plotted. Must contain the following columns: datetime, year, month, value and units.
#' @param ref_period_start_datetime The first year to consider when calculating ribbons for a reference period. Default is NULL and will use the first year of data available.
#' @param ref_period_end_datetime The last year to consider when calculating ribbons for a reference period. Default is NULL and will use the last year of data available or the last
#' @param lang The desired language for the plot.
#'
#' @return A .png file of the plot requested (if a save path has been selected), plus the plot displayed in RStudio. Assign the function to a variable to also get a plot in your global environment as a ggplot object which can be further modified
#' @export

hydrometDiscrete <- function(
  location = NULL,
  parameter,
  startDay = 1,
  endDay = 365,
  binwidth = "1 month",
  tzone = "MST",
  years = NULL,
  title = TRUE,
  custom_title = NULL,
  plot_type = "violin",
  plot_scale = 1,
  save_path = NULL,
  con = NULL,
  discrete_data = NULL,
  ref_period_start_datetime = NULL,
  ref_period_end_datetime = NULL,
  lang = "en"
) {
  # TODO Should give a decent error message if the user requests something that doesn't exist. Station not existing, timeseries not existing, years not available (and where they are), etc.
  if (is.null(con)) {
    if (is.null(discrete_data)) {
      con <- AquaConnect(silent = TRUE)
      on.exit(DBI::dbDisconnect(con))
    }
  }

  lang <- shortenLanguage(lang)

  # Suppress warnings otherwise ggplot annoyingly flags every geom that wasn't plotted
  old_warn <- getOption("warn")
  options(warn = -1)
  on.exit(options(warn = old_warn), add = TRUE)

  #### ------- Checks on input parameters  and other start-up bits ---------- ####
  parameter <- tolower(parameter)

  plot_type <- tolower(plot_type)
  if (!(plot_type %in% c("violin", "boxplot", "linedbox"))) {
    stop(
      "Parameter 'plot_type' must be one of 'violin', 'boxplot', or 'linedbox'"
    )
  }

  if (is.null(years)) {
    years <- as.numeric(substr(Sys.Date(), 1, 4))
  } else {
    years <- as.numeric(years)
    years <- sort(years, decreasing = TRUE)
    if (length(years) > 10) {
      years <- years[1:10]
      message(
        "The parameter 'years' can only have up to 10 years. It's been truncated to the first 10 years in the vector."
      )
    }
  }
  # Select save path
  if (!is.null(save_path)) {
    if (save_path %in% c("Choose", "choose")) {
      message("Select the folder where you want this graph saved.")
      save_path <- as.character(utils::choose.dir(
        caption = "Select Save Folder"
      ))
    }
  }

  #### ----------------- Dealing with start/end dates ----------------------- ####
  # Sort out startDay and endDay into actual dates if needed
  last_year <- max(years)
  leap_list <- (seq(1800, 2100, by = 4)) # Create list of all leap years
  tryCatch(
    {
      startDay <- as.character(startDay)
      startDay <- as.POSIXct(startDay, tz = tzone)
      lubridate::year(startDay) <- last_year
    },
    error = function(e) {
      if (last_year %in% leap_list) {
        if (startDay > 59) {
          startDay <<- startDay + 1
        }
      }
      startDay <<- as.POSIXct(
        as.numeric(startDay) * 60 * 60 * 24,
        origin = paste0(last_year - 1, "-12-31"),
        tz = "UTC"
      )
      startDay <<- lubridate::force_tz(startDay, tzone)
    }
  )
  tryCatch(
    {
      endDay <- as.character(endDay)
      endDay <- as.POSIXct(endDay, tz = tzone)
      lubridate::year(endDay) <- last_year
    },
    error = function(e) {
      tempStartDay <- lubridate::yday(startDay) #using yday because start is now in proper Date format and needs to be back-converted to yday
      if (last_year %in% leap_list) {
        if (as.numeric(endDay) > 59) {
          endDay <<- as.character(as.numeric(endDay) + 1)
        }
      }

      endDay <<- as.POSIXct(
        as.numeric(endDay) * 60 * 60 * 24,
        origin = paste0(last_year - 1, "-12-31"),
        tz = "UTC"
      )
      endDay <<- lubridate::force_tz(endDay, tzone)
    }
  )

  if (startDay > endDay) {
    #if the user is wanting a range overlapping the new year
    lubridate::year(endDay) <- lubridate::year(endDay) + 1
    overlaps <- TRUE
    last_year <- last_year + 1
  } else {
    overlaps <- FALSE
  }

  #### ------------------------ Data is not given --------------------------- ####
  if (is.null(discrete_data)) {
    # Check for existence of timeseries ---------------------------------------
    #then for presence of data within the time range requested.
    location_id <- lookup_location_id(con, location)
    if (is.na(location_id)) {
      stop("The location you entered does not exist in the database.")
    }
    parameter_code <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT parameter_id FROM parameters WHERE param_name = '",
        parameter,
        "';"
      )
    )[1, 1]
    exists <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT timeseries_id, start_datetime FROM timeseries WHERE location_id = '",
        location_id,
        "' AND parameter_id = ",
        parameter_code,
        ";"
      )
    )
    if (nrow(exists) == 0) {
      stop(
        "There is no entry for the location and parameter combination that you specified of discrete data type. If you are trying to graph continuous data use plotOverlap or plotTimeseries"
      )
    } else if (nrow(exists) > 1) {
      stop(
        "There is more than one entry in the database for the location and parameter that you specified! Please alert the database manager."
      )
    } else {
      tsid <- exists$timeseries_id
      units <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT unit_default FROM parameters WHERE parameter_id = ",
          parameter_code
        )
      )[1, 1]
    }

    # Get the data ------------------------------------------------------------
    # Pull data from db for timeseries and period of interest
    if (overlaps) {
      all_discrete <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT target_datetime, datetime, value FROM measurements_discrete WHERE timeseries_id = ",
          tsid,
          " AND datetime <= '",
          paste0(max(years) + 1, substr(endDay, 5, 19)),
          "'"
        )
      )
    } else {
      all_discrete <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT target_datetime, datetime, value FROM measurements_discrete WHERE timeseries_id = ",
          tsid,
          " AND datetime <= '",
          paste0(max(years), substr(endDay, 5, 19)),
          "'"
        )
      )
    }

    # Check if data is empty
    if (nrow(all_discrete) == 0) {
      stop(paste0(
        "There doesn't appear to be any data for the year and days you specified: this timeseries starts ",
        exists$start_datetime
      ))
    }

    # Deal with dates
    all_discrete$target_datetime <- as.Date(all_discrete$target_datetime)
    # Now since target_datetime might be missing, replace it with the (mandatory not missing) datetime
    all_discrete$target_datetime[is.na(
      all_discrete$target_datetime
    )] <- as.Date(all_discrete$datetime[is.na(all_discrete$target_datetime)])
    all_discrete$year <- lubridate::year(all_discrete$target_datetime)
    all_discrete$month <- lubridate::month(all_discrete$target_datetime)
    all_discrete$day <- lubridate::day(all_discrete$target_datetime)
    #Separate, modify, and re-bind feb29 days, if any
    feb29 <- all_discrete[all_discrete$month == 2 & all_discrete$day == 29, ]
    if (nrow(feb29) > 0) {
      all_discrete <- all_discrete[
        !(all_discrete$month == 2 & all_discrete$day == 29),
      ]
      feb29$target_datetime <- feb29$target_datetime + 1
      feb29$month <- 3
      feb29$day <- 1
      all_discrete <- rbind(all_discrete, feb29)
    }

    #Make a fake date
    all_discrete$fake_date <- as.Date(gsub(
      "[0-9]{4}",
      last_year,
      all_discrete$target_datetime
    ))
    # Only keep those who's fake_date is within startDay-endDay period
    all_discrete <- all_discrete[
      all_discrete$fake_date >= startDay & all_discrete$fake_date <= endDay,
    ]

    discrete <- data.frame()
    for (i in years) {
      start <- as.Date(paste0(i, substr(startDay, 5, 10)))
      end <- as.Date(paste0(i, substr(endDay, 5, 10)))

      if (overlaps) {
        lubridate::year(end) <- lubridate::year(end) + 1
      }

      new_discrete <- all_discrete[
        all_discrete$target_datetime >= start &
          all_discrete$target_datetime <= end,
      ]
      discrete <- rbind(discrete, new_discrete)
    }
  }

  #### -------------------------- Data is given ----------------------------- ####
  if (!is.null(discrete_data)) {
    ## Create all_discrete
    all_discrete <- discrete_data

    # add fake_date
    all_discrete$fake_date <- NA
    all_discrete$fake_date <- as.Date(all_discrete$fake_date)

    #Make a fake date
    if (overlaps) {
      # Add monthday column
      all_discrete$monthday <- format(all_discrete$datetime, "%m-%d")
      # Dates with monthday between start monthday and Dec get last_year -1 year
      all_discrete[
        all_discrete$monthday >= format(startDay, "%m-%d") &
          all_discrete$monthday <= "12-31",
      ]$fake_date <-
        as.Date(
          gsub(
            "[0-9]{4}",
            last_year - 1,
            all_discrete[
              all_discrete$monthday >= format(startDay, "%m-%d") &
                all_discrete$monthday <= "12-31",
            ]$datetime
          ),
          format = "%Y-%m-%d"
        )
      # Dates with monthday between Jan and end monthday get last_year year
      all_discrete[
        all_discrete$monthday <= format(endDay, "%m-%d") &
          all_discrete$monthday >= "01-01",
      ]$fake_date <-
        as.Date(
          gsub(
            "[0-9]{4}",
            last_year,
            all_discrete[
              all_discrete$monthday <= format(endDay, "%m-%d") &
                all_discrete$monthday >= "01-01",
            ]$datetime
          ),
          format = "%Y-%m-%d"
        )
    } else {
      all_discrete$fake_date <- as.Date(gsub(
        "[0-9]{4}",
        last_year,
        all_discrete$datetime
      ))
    }

    # Only keep those who's fake_date is within startDay-endDay period
    all_discrete <- all_discrete[!is.na(all_discrete$fake_date), ]
    all_discrete <- all_discrete[
      all_discrete$fake_date >= startDay & all_discrete$fake_date <= endDay,
    ]

    # Make fake_date first day of month
    all_discrete$fake_date <- lubridate::floor_date(
      all_discrete$fake_date,
      "month"
    )

    discrete <- data.frame()
    for (i in years) {
      start <- as.Date(paste0(i, substr(startDay, 5, 10)))
      if (overlaps) {
        end <- as.Date(paste0(i + 1, substr(endDay, 5, 10)))
      } else {
        end <- as.Date(paste0(i, substr(endDay, 5, 10)))
      }

      if ("target_datetime" %in% colnames(all_discrete)) {
        new_discrete <- all_discrete[
          all_discrete$target_datetime >= start &
            all_discrete$target_datetime <= end,
        ]
      } else {
        new_discrete <- all_discrete[
          all_discrete$datetime >= start & all_discrete$datetime <= end,
        ]
      }

      discrete <- rbind(discrete, new_discrete)
    }

    ## Give units
    units <- unique(discrete$units)
  }

  #### ------------------ Calculate stats for "lined box" ------------------- ####
  if (plot_type == 'linedbox') {
    # Calculate stats for linedbox plot
    # If reference years are provided, use them for min/max/median; otherwise use all data
    if (
      !is.null(ref_period_start_datetime) && !is.null(ref_period_end_datetime)
    ) {
      # Ensure ref_period_start_datetime and ref_period_end_datetime are Date objects
      ref_period_start_datetime <- as.Date(ref_period_start_datetime)
      ref_period_end_datetime <- as.Date(ref_period_end_datetime)
      # Filter all_discrete for datetimes within the reference period
      stats_ref <- all_discrete[
        all_discrete$datetime >= ref_period_start_datetime &
          all_discrete$datetime <= ref_period_end_datetime,
      ]
      stats_discrete <- stats_ref %>%
        dplyr::group_by(.data$month) %>%
        dplyr::summarise(
          value = min(.data$value, na.rm = TRUE),
          type = "min"
        ) %>%
        dplyr::bind_rows(
          stats_ref %>%
            dplyr::group_by(.data$month) %>%
            dplyr::summarise(
              value = max(.data$value, na.rm = TRUE),
              type = "max"
            )
        ) %>%
        dplyr::bind_rows(
          stats_ref %>%
            dplyr::group_by(.data$month) %>%
            dplyr::summarise(
              value = stats::median(.data$value, na.rm = TRUE),
              type = "median"
            )
        ) %>%
        dplyr::bind_rows(
          stats_ref %>%
            dplyr::group_by(.data$month) %>%
            dplyr::summarise(
              value = stats::quantile(.data$value, probs = 0.25, na.rm = TRUE),
              type = "p25"
            )
        ) %>%
        dplyr::bind_rows(
          stats_ref %>%
            dplyr::group_by(.data$month) %>%
            dplyr::summarise(
              value = stats::quantile(.data$value, probs = 0.75, na.rm = TRUE),
              type = "p75"
            )
        )
      # Calculate all-time high/low using all data
      ath <- all_discrete %>%
        dplyr::group_by(.data$month) %>%
        dplyr::summarise(value = max(.data$value, na.rm = TRUE), type = "ath")
      atl <- all_discrete %>%
        dplyr::group_by(.data$month) %>%
        dplyr::summarise(value = min(.data$value, na.rm = TRUE), type = "atl")
      stats_discrete <- dplyr::bind_rows(stats_discrete, ath, atl)
    } else {
      stats_discrete <- all_discrete %>%
        dplyr::group_by(.data$month) %>%
        dplyr::summarise(
          value = min(.data$value, na.rm = TRUE),
          type = "min"
        ) %>%
        dplyr::bind_rows(
          all_discrete %>%
            dplyr::group_by(.data$month) %>%
            dplyr::summarise(
              value = max(.data$value, na.rm = TRUE),
              type = "max"
            )
        ) %>%
        dplyr::bind_rows(
          all_discrete %>%
            dplyr::group_by(.data$month) %>%
            dplyr::summarise(
              value = stats::median(.data$value, na.rm = TRUE),
              type = "median"
            )
        ) %>%
        dplyr::bind_rows(
          all_discrete %>%
            dplyr::group_by(.data$month) %>%
            dplyr::summarise(
              value = stats::quantile(.data$value, probs = 0.25, na.rm = TRUE),
              type = "p25"
            )
        ) %>%
        dplyr::bind_rows(
          all_discrete %>%
            dplyr::group_by(.data$month) %>%
            dplyr::summarise(
              value = stats::quantile(.data$value, probs = 0.75, na.rm = TRUE),
              type = "p75"
            )
        )
    }

    if (overlaps) {
      stats_discrete$fake_date <- NA
      # Fake_date for those before Jan
      mon <- lubridate::month(startDay)
      stats_discrete[
        as.numeric(stats_discrete$month) >= mon,
      ]$fake_date <- as.Date(paste0(
        max(years),
        "-",
        stats_discrete[as.numeric(stats_discrete$month) >= mon, ]$month,
        "-01"
      ))
      # Fake_date for those after Jan
      mon <- lubridate::month(endDay)
      stats_discrete[
        as.numeric(stats_discrete$month) <= mon,
      ]$fake_date <- as.Date(paste0(
        max(years + 1),
        "-",
        stats_discrete[as.numeric(stats_discrete$month) <= mon, ]$month,
        "-01"
      ))
    } else {
      stats_discrete$fake_date <- as.Date(paste0(
        max(years),
        "-",
        stats_discrete$month,
        "-01"
      ))
    }
    day_seq <- seq(
      min(stats_discrete$fake_date),
      max(stats_discrete$fake_date),
      by = "1 day"
    )
  } else {
    day_seq <- seq(
      min(all_discrete$fake_date),
      max(all_discrete$fake_date),
      by = "1 day"
    )
  }

  # x axis settings
  if (length(day_seq) > 200) {
    date_breaks = "2 months"
    if (lang == "fr") {
      labs = "%d %b"
    } else {
      labs = "%b %d"
    }
  } else if (length(day_seq) > 60) {
    date_breaks = "1 month"
    if (lang == "fr") {
      labs = "%d %b"
    } else {
      labs = "%b %d"
    }
  } else if (length(day_seq) > 14) {
    date_breaks = "1 week"
    if (lang == "fr") {
      labs = "%d %b"
    } else {
      labs = "%b %d"
    }
  } else if (length(day_seq) > 7) {
    date_breaks = "2 days"
    if (lang == "fr") {
      labs = "%d %b"
    } else {
      labs = "%b %d"
    }
  } else if (length(day_seq) >= 2) {
    date_breaks = "1 days"
    if (lang == "fr") {
      labs = "%d %b"
    } else {
      labs = "%b %d"
    }
  } else if (length(day_seq) > 1) {
    date_breaks = "24 hours"
    if (lang == "fr") {
      labs = "%H:%M"
    } else {
      labs = "%H:%M"
    }
  } else if (length(day_seq) == 1) {
    date_breaks = "12 hour"
    if (lang == "fr") {
      labs = "%H:%M"
    } else {
      labs = "%H:%M"
    }
  }

  eng_months <- c(
    "Jan",
    "Feb",
    "Mar",
    "Apr",
    "May",
    "Jun",
    "Jul",
    "Aug",
    "Sep",
    "Oct",
    "Nov",
    "Dec"
  )
  fr_months <- c(
    "janv",
    "f\u00E9vr",
    "mars",
    "avr",
    "mai",
    "juin",
    "juil",
    "ao\u00FBt",
    "sept",
    "oct",
    "nov",
    "d\u00E9c"
  )

  # Define a custom date labeling function
  custom_date_labels <- function(x) {
    label <- format(x, format = labs)

    # Remove leading zeros from day numbers
    label <- gsub("\\b0([1-9])", "\\1", label)

    if (lang == "fr") {
      # Replace month abbreviations
      for (i in seq_along(eng_months)) {
        label <- gsub(eng_months[i], fr_months[i], label)
      }
      # Replace weekday abbreviations (if using %a or %A in labs)
      eng_days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
      fr_days <- c("lun", "mar", "mer", "jeu", "ven", "sam", "dim")
      for (i in seq_along(eng_days)) {
        label <- gsub(eng_days[i], fr_days[i], label)
      }
    }
    return(label)
  }

  #### ---------------------------- Make the plot --------------------------- ####

  colours = c(
    "black",
    "blue",
    "darkorchid3",
    "cyan2",
    "firebrick3",
    "aquamarine4",
    "gold1",
    "chartreuse1",
    "darkorange",
    "lightsalmon4"
  )
  # c("black", "#DC4405", "#512A44", "#F2A900", "#244C5A", "#687C04", "#C60D58", "#0097A9", "#7A9A01", "#834333")
  legend_length <- length(years)
  plot <- ggplot2::ggplot(
    all_discrete,
    ggplot2::aes(x = .data$fake_date, y = .data$value, group = .data$fake_date)
  ) +
    ggplot2::labs(
      x = NULL,
      y = paste0(titleCase(parameter), " (", units, ")")
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "right",
      legend.justification = c(0, 0.95),
      legend.text = ggplot2::element_text(size = 8 * plot_scale),
      legend.title = ggplot2::element_text(size = 10 * plot_scale),
      axis.title.y = ggplot2::element_text(size = 12 * plot_scale),
      axis.text.x = ggplot2::element_text(size = 11 * plot_scale),
      axis.text.y = ggplot2::element_text(size = 11 * plot_scale)
    )

  if (plot_type == "linedbox") {
    for (m in unique(stats_discrete$month)) {
      plot <- plot +
        ggplot2::geom_rect(
          data = stats_discrete[
            (stats_discrete$month == m) &
              (stats_discrete$type %in% c("min", "max", "median")),
          ],
          fill = 'grey90',
          ggplot2::aes(
            xmin = .data$fake_date - 12,
            xmax = .data$fake_date + 12,
            ymin = min(.data$value),
            ymax = max(.data$value)
          )
        ) +
        ggplot2::geom_rect(
          data = stats_discrete[
            (stats_discrete$month == m) &
              (stats_discrete$type %in% c("p25", "p75", "median")),
          ],
          fill = 'grey80',
          ggplot2::aes(
            xmin = .data$fake_date - 12,
            xmax = .data$fake_date + 12,
            ymin = min(.data$value),
            ymax = max(.data$value)
          )
        )
    }

    if (
      !is.null(ref_period_start_datetime) && !is.null(ref_period_end_datetime)
    ) {
      # Only plot ATH, ATL, and Median
      stats_to_plot <- stats_discrete[
        stats_discrete$type %in% c("ath", "atl"),
      ]
      plot <- plot +
        ggplot2::geom_segment(
          data = stats_to_plot,
          linewidth = plot_scale * 1.5,
          ggplot2::aes(
            color = .data$type,
            yend = .data$value,
            x = .data$fake_date - 12,
            xend = .data$fake_date + 12
          )
        ) +
        ggplot2::scale_color_manual(
          name = "",
          labels = c("All-time High", "All-time Low"),
          values = c("#0097A9", "#834333")
        ) +
        ggnewscale::new_scale_color()
    } else {
      plot <- plot +
        ggplot2::geom_segment(
          data = stats_discrete,
          linewidth = plot_scale * 1.5,
          ggplot2::aes(
            color = .data$type,
            yend = .data$value,
            x = .data$fake_date - 12,
            xend = .data$fake_date + 12
          )
        ) +
        ggplot2::scale_color_manual(
          name = "",
          labels = c("Maximum", "Median", "Minimum"),
          values = c("#0097A9", "#7A9A01", "#834333")
        ) +
        ggnewscale::new_scale_color()
    }
  } else if (plot_type == "violin") {
    plot <- plot +
      ggplot2::geom_violin(
        draw_quantiles = c(0.5),
        adjust = 0.7,
        width = 12,
        alpha = 0.8,
        fill = "aliceblue",
        scale = "width"
      )
  } else if (plot_type == "boxplot") {
    plot <- plot +
      ggplot2::geom_boxplot(
        color = "black",
        fill = "#87CEEB",
        varwidth = TRUE,
        outlier.shape = 1,
        outlier.color = "#512A44",
        outlier.fill = "#512A44",
        whisker.linewidth = 1,
        box.linewidth = 1,
        outlier.size = 3 * plot_scale,
        notch = FALSE
      )
  }

  if (nrow(discrete) > 0) {
    if (plot_type == "violin" | plot_type == "boxplot") {
      plot <- plot +
        ggplot2::geom_point(
          data = discrete,
          mapping = ggplot2::aes(
            x = .data$fake_date,
            y = .data$value,
            colour = as.factor(.data$year),
            fill = as.factor(.data$year)
          ),
          size = plot_scale * 3.5 * 2,
          shape = 21
        ) +
        ggplot2::scale_colour_manual(
          name = "Year",
          labels = unique(discrete$year),
          values = grDevices::colorRampPalette(c(
            "#0097A9",
            "#7A9A01",
            "#F2A900",
            "#DC4405"
          ))(length(unique(discrete$year))),
          aesthetics = c("colour", "fill"),
          na.translate = FALSE,
          breaks = unique(stats::na.omit(discrete$year))[1:legend_length]
        )
    }

    if (plot_type == "linedbox") {
      plot <- plot +
        ggplot2::geom_segment(
          data = discrete,
          linewidth = plot_scale * 1.5,
          ggplot2::aes(
            yend = .data$value,
            x = .data$fake_date - 12,
            xend = .data$fake_date + 12
          ),
          color = 'black'
        )
    }
  }

  #### ---------------------- Wrap things up and return() ------------------- ####

  plot <- plot +
    ggplot2::scale_x_date(
      date_breaks = date_breaks,
      labels = custom_date_labels
    ) # The expand argument controls space between the data and the y axis. Default for continuous variable is 0.05
  if (title) {
    if (is.null(custom_title)) {
      if (is.null(discrete_data)) {
        stn_name <- DBI::dbGetQuery(
          con,
          "SELECT name FROM locations where location_id = $1;",
          params = list(location_id)
        )[1, 1]
        titl <- paste0("Location ", location, ": ", titleCase(stn_name))
      } else {
        if (!is.null(location)) {
          titl <- paste0("Location: ", location)
        } else {
          titl <- paste0("Location: ", titleCase(unique(all_discrete$location)))
        }
      }
    } else {
      (titl <- custom_title)
    }

    plot <- plot +
      ggplot2::labs(title = titl) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          hjust = 0.05,
          size = 12 * plot_scale,
          face = "bold"
        )
      )
  }

  #Save it if requested
  if (!is.null(save_path)) {
    ggplot2::ggsave(
      filename = paste0(
        save_path,
        "/",
        location,
        "_",
        parameter,
        "_",
        Sys.Date(),
        "_",
        lubridate::hour(as.POSIXct(format(Sys.time()), tz = tzone)),
        lubridate::minute(as.POSIXct(format(Sys.time()), tz = tzone)),
        ".png"
      ),
      plot = plot,
      height = 8,
      width = 12,
      units = "in",
      device = "png",
      dpi = 500
    )
  }

  return(plot)
}
