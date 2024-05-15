#' Plot a more than one continuous timeseries from the hydromet database
#'
#' @description
#' `r lifecycle::badge('stable')`
#' 
#' This function plots continuous timeseries from the hydromet/AquaCache database. The plot is zoomable and hovering over the historical ranges or the measured values brings up additional information.
#' 
#' @param locations The location or locations for which you want a plot. If specifying multiple locations matched to the parameters and record_rates 1:1. The location:parameter combos must be in the local database.
#' @param parameters The parameter or parameters you wish to plot. If specifying multiple parameters matched to the locations and record_rates 1:1. The location:parameter combos must be in the local database.
#' @param record_rates The recording rate for the parameters and locations. In most cases there are not multiple recording rates for a location and parameter combo and you can leave this NULL. Otherwise NULL will default to the most frequent record rate, or set this as one of '< 1 day', '1 day', '1 week', '4 weeks', '1 month', 'year'. Matched one to one to the locations and parameters or recycled if specified as NULL or length one.
#' @param start_date The day on which to start the plot as character, Date, or POSIXct. Default is 1970-01-01.
#' @param end_date The day on which to end the plot as character, Date, or POSIXct. Default is today.
#' @param log Should any/all y axes use a logarithmic scale? Specify as a logical (TRUE/FALSE) vector of length 1 or of length equal to the number of traces you wish to plot. Default is FALSE.
#' @param slider Should a slider be included to show where you are zoomed in to? If TRUE the slider will be included but this prevents horizontal zooming or zooming in using the box tool.
#' @param datum Should a vertical offset be applied to the data? Looks for it in the database and applies it if it exists, only for water level and distance. Default is TRUE.
#' @param title Should a title be included? TRUE/FALSE.
#' @param custom_title Custom title to be given to the plot. Default is NULL, which will set the title as the location name as entered in the database.
#' @param filter Should an attempt be made to filter out spurious data? Will calculate the rolling IQR and filter out clearly spurious values. Set this parameter to an integer, which specifies the rolling IQR 'window'. The greater the window, the more effective the filter but at the risk of filtering out real data. Negative values are always filtered from parameters "water level" ("niveau d'eau"), "water flow" ("débit d'eau"), "snow depth" ("profondeur de la neige"), "snow water equivalent" ("équivalent en eau de la neige"), "distance", and any "precip" related parameter. Otherwise all values below -100 are removed.
#' @param historic_range Default NULL will plot historic ranges for up to two traces and no historical ranges at all if more than two traces; TRUE will plot historic ranges for all traces; FALSE will not plot historic ranges.
#' @param language The language to use for the plot. Currently only "en" and "fr" are supported. Default is "en".
#' @param rate The rate at which to plot the data. Default is NULL, which will adjust for reasonable plot performance depending on the date range. Otherwise set to one of "max", "hour", "day".
#' @param tzone The timezone to use for the plot. Default is "auto", which will use the system default timezone. Otherwise set to a valid timezone string.
#' @param con A connection to the target database. NULL uses [hydrometConnect()] and automatically disconnects.
#'
#' @return A plotly object 
#' 
#' @export

plotMultiTimeseries <- function(locations,
                                parameters,
                                record_rates = NULL,
                                start_date = "1970-01-01",
                                end_date = Sys.Date(),
                                log = FALSE,
                                slider = FALSE,
                                datum = TRUE,
                                title = TRUE,
                                custom_title = NULL,
                                filter = NULL,
                                historic_range = NULL,
                                language = "en",
                                rate = NULL,
                                tzone = "auto",
                                con = NULL) {
  
  # Checks and initial work ##########################################
  
  # Deal with non-standard evaluations from data.table to silence check() notes
  period_secs <- period <- expected <- datetime <- gap_exists <- NULL
  
  rlang::check_installed("plotly", "necessary for interactive plots")
  
  if (!(language %in% c("en", "fr"))) {
    stop("Your entry for the parameter 'language' is invalid. Please review the function documentation and try again.")
  }
  
  
  if (length(log) != 1) {
    if (length(log) != length(locations)) {
      stop("Your entry for the parameter 'log' is invalid. Please review the function documentation and try again.")
    }
  }
  
  if (length(log) == 1) {
    log <- rep(log, length(locations))
  }
  
  if (!is.null(rate)) {
    tolower(rate)
    if (!(rate %in% c("max", "hour", "day"))) {
      stop("Your entry for the parameter 'rate' is invalid. Please review the function documentation and try again.")
    }
  }
  
  if (is.null(con)) {
    con <- hydrometConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  if (length(locations) == 1 & length(parameters) > 1) {
    locations <- rep(locations, length(parameters))
  }
  if (length(parameters) == 1 & length(locations) > 1) {
    parameters <- rep(parameters, length(locations))
  }
  if (!is.null(record_rates)) {
    if (length(record_rates) == 1 & length(locations) > 1) {
      record_rates <- rep(record_rates, length(locations))
    }
  }
  
  if (length(locations) != length(parameters)) {
    stop("The number of locations and parameters must be the same, or one must be a vector of length 1.")
  }
  parameters <- tolower(parameters)
  
  if (!is.null(record_rates)) {
    if (length(record_rates) != length(locations)) {
      stop("The number of locations and record rates must be the same, or one must be a vector of length 1 or left to the default NULL.")
    }
    for (i in 1:length(record_rates)) {
      if (!(record_rates[i] %in% c('< 1 day', '1 day', '1 week', '4 weeks', '1 month', 'year'))) {
        warning("Your entry ", i, " for parameter record_rates is invalid. It's been reset to the default NULL.")
        record_rates[i] <- NA
      }
    }
  } else {
    record_rates <- NA
  }
  
  if (tzone == "auto") {
    tzone <- Sys.timezone()
  }
  
  if (inherits(start_date, "character")) {
    start_date <- as.Date(start_date)
  }
  if (inherits(start_date, "Date")) {
    start_date <- as.POSIXct(start_date, tz = tzone)
  }
  if (inherits(end_date, "character")) {
    end_date <- as.Date(end_date)
  }
  if (inherits(end_date, "Date")) {
    end_date <- as.POSIXct(end_date, tz = tzone)
  }
  
  #back to UTC because DB queries are in UTC
  attr(start_date, "tzone") <- "UTC"
  attr(end_date, "tzone") <- "UTC"
  
  if (!is.null(historic_range)) {
    if (!inherits(historic_range, "logical")) {
      warning("Parameter `historic_range` must be NULL, TRUE, or FALSE. Resetting it to NULL")
      historic_range <- NULL
    }
  } else {
    if (length(locations) > 2) {
      historic_range <- FALSE
    } else {
      historic_range <- TRUE
    }
  }
  
  # Get the data for each location:parameter:record_rate combo
  # Make a list with one element per location:parameter:record_rate combo
  timeseries <- data.frame(location = locations, parameter = parameters, record_rate = record_rates)
  if (nrow(unique(timeseries)) != nrow(timeseries)) {
    stop("You have duplicate entries in your locations and/or parameters and/or record_rates. Please review the function documentation and try again.")
  }
  
  data <- list()
  remove <- numeric()
  for (i in 1:nrow(timeseries)) {
    
    location <- timeseries$location[i]
    parameter <- timeseries$parameter[i]
    record_rate <- if (is.na(timeseries$record_rate[i])) NULL else timeseries$record_rate[i]
    
    # Determine the timeseries and adjust the date range #################
    location_id <- DBI::dbGetQuery(con, paste0("SELECT location_id FROM locations WHERE location = '", location, "';"))[1,1]
    if (is.na(location_id)) {
      warning("The location you entered, ", location, ", does not exist in the database. Moving on to the next entry.")
      remove <- c(remove, i)
      next
    }
    #Confirm parameter and location exist in the database and that there is only one entry
    escaped_parameter <- gsub("'", "''", parameter)
    parameter_tbl <- DBI::dbGetQuery(con, paste0("SELECT param_code, param_name, param_name_fr FROM parameters WHERE param_name = '", escaped_parameter, "' OR param_name_fr = '", escaped_parameter, "';"))
    parameter_code <- parameter_tbl$param_code[1]
    if (is.na(parameter_code)) {
      warning("The parameter you entered for location ", location, ", parameter ", parameter, " does not exist in the database. Moving on to the next entry.")
      remove <- c(remove, i)
      next
    }
    if (language == "fr") {
      timeseries[i, "parameter_name"] <- titleCase(parameter_tbl$param_name_fr[1], "fr")
    } else if (language == "en" || is.na(timeseries[i, "parameter_name"])) {
      timeseries[i, "parameter_name"] <- titleCase(parameter_tbl$param_name[1], "en")
    }
    
    if (is.null(record_rate)) {
      exist_check <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id, record_rate, start_datetime, end_datetime FROM timeseries WHERE location_id = ", location_id, " AND parameter = ", parameter_code, " AND category = 'continuous' AND period_type = 'instantaneous';"))
    } else {
      exist_check <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id, record_rate, start_datetime, end_datetime FROM timeseries WHERE location_id = ", location_id, " AND parameter = ", parameter_code, " AND category = 'continuous' AND period_type = 'instantaneous' AND record_rate = '", record_rate, "';"))
    }
    if (nrow(exist_check) == 0) {
      if (is.null(record_rate)) {
        warning("There doesn't appear to be a match in the database for location ", location, ", parameter ", parameter, ", and continuous category data. Moving on to the next entry.")
        remove <- c(remove, i)
        next
      } else {
        warning("There doesn't appear to be a match in the database for location ", location, ", parameter ", parameter, ", record rate ", record_rate, " and continuous category data. Moving on to the next entry. You could try leaving the record rate to the default 'null'.")
        remove <- c(remove, i)
        next
      }
    } else if (nrow(exist_check) > 1) {
      if (is.null(record_rate)) {
        warning("There is more than one entry in the database for location ", location, ", parameter ", parameter, ", and continuous category data. Since you left the record_rate as NULL, selecting the one with the most frequent recording rate.")
        tsid <- exist_check[exist_check$record_rate == "< 1 day", "timeseries_id"]
        if (is.na(tsid)) {
          tsid <- exist_check[exist_check$record_rate == "1 day", "timeseries_id"]
        }
        if (is.na(tsid)) {
          tsid <- exist_check[exist_check$record_rate == "1 week", "timeseries_id"]
        }
        if (is.na(tsid)) {
          tsid <- exist_check[exist_check$record_rate == "4 weeks", "timeseries_id"]
        }
        if (is.na(tsid)) {
          tsid <- exist_check[exist_check$record_rate == "1 month", "timeseries_id"]
        }
        if (is.na(tsid)) {
          tsid <- exist_check[exist_check$record_rate == "year", "timeseries_id"]
        }
      }
    } else if (nrow(exist_check) == 1) {
      tsid <- exist_check$timeseries_id
    }
    timeseries[i, "record_rate"] <- exist_check[exist_check$timeseries_id == tsid, "record_rate"]
    
    if  (start_date > exist_check$end_datetime) {
      warning("The start date you entered is after the end date of the data in the database for location ", location, ", parameter ", parameter, ". Moving on to the next entry.")
      remove <- c(remove, i)
      next
    }
    if (end_date < exist_check$start_datetime) {
      warning("The end date you entered is before the start date of the data in the database for location ", location, ", parameter ", parameter, ". Moving on to the next entry.")
      remove <- c(remove, i)
      next
    }
    
    #adjust start and end datetimes
    sub.start_date <- start_date
    if (sub.start_date < exist_check$start_datetime) {
      sub.start_date <- exist_check$start_datetime
    }
    sub.end_date <- end_date
    if (sub.end_date > exist_check$end_datetime) {
      sub.end_date <- exist_check$end_datetime
    }
    timeseries[i, "start"] <- sub.start_date
    timeseries[i, "end"] <- sub.end_date
    
    # Find the necessary datum (latest datum)
    if (datum & parameter %in% c("water level", "distance")) {
      datum.conv <- DBI::dbGetQuery(con, paste0("SELECT conversion_m FROM datum_conversions WHERE location_id = ", location_id, " AND current = TRUE"))
    } else {
      datum.conv <- data.frame(conversion_m = 0)
    }
    
    # Find the ts units
    timeseries[i, "units"] <- DBI::dbGetQuery(con, paste0("SELECT unit FROM parameters WHERE param_code = ", parameter_code, ";"))[1,1]
    
    range <- seq.POSIXt(sub.start_date, sub.end_date, by = "day")
    if (is.null(rate)) {
      if (length(range) > 3000) {
        rate <- "day"
      } else if (length(range) > 1000) {
        rate <- "hour"
      } else {
        rate <- "max"
      }
    }
    
    # Get the data ####################################
    if (historic_range) { # get data from the calculated_daily table for historic ranges plus values from measurements_continuous. Where there isn't any data in measurements_continuous fill in with the value from the daily table.
      range_end <- sub.end_date + 1*24*60*60
      range_start <- sub.start_date - 1*24*60*60
      range_data <- dbGetQueryDT(con, paste0("SELECT date AS datetime, min, max, q75, q25  FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date BETWEEN '", range_start, "' AND '", range_end, "' ORDER BY date ASC;"))
      range_data$datetime <- as.POSIXct(range_data$datetime, tz = "UTC")
      attr(range_data$datetime, "tzone") <- tzone
      if (rate == "day") {
        trace_data <- dbGetQueryDT(con, paste0("SELECT date AS datetime, value FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date BETWEEN '", sub.start_date, "' AND '", sub.end_date, "' ORDER BY date DESC;"))
        trace_data$datetime <- as.POSIXct(trace_data$datetime, tz = "UTC")
      } else if (rate == "hour") {
        trace_data <- dbGetQueryDT(con, paste0("SELECT datetime, value FROM measurements_hourly WHERE timeseries_id = ", tsid, " AND datetime BETWEEN '", sub.start_date, "' AND '", sub.end_date, "' ORDER BY datetime DESC;"))
      } else if (rate == "max") {
        trace_data <- dbGetQueryDT(con, paste0("SELECT datetime, value FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime BETWEEN '", sub.start_date, "' AND '", sub.end_date, "' ORDER BY datetime DESC LIMIT 200000;"))
        if (nrow(trace_data) > 0) {
          if (min(trace_data$datetime) > sub.start_date) {
            infill <- dbGetQueryDT(con, paste0("SELECT datetime, value FROM measurements_hourly WHERE timeseries_id = ", tsid, " AND datetime BETWEEN '", sub.start_date, "' AND '", min(trace_data$datetime) - 1, "' ORDER BY datetime DESC;"))
            trace_data <- rbind(infill, trace_data)
          }
        }
        
      }
      attr(trace_data$datetime, "tzone") <- tzone
    } else { #No historic range requested
      if (rate == "day") {
        trace_data <- dbGetQueryDT(con, paste0("SELECT date AS datetime, value FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date BETWEEN '", sub.start_date, "' AND '", sub.end_date, "' ORDER BY date DESC;"))
        trace_data$datetime <- as.POSIXct(trace_data$datetime, tz = "UTC")
      } else if (rate == "hour") {
        trace_data <- dbGetQueryDT(con, paste0("SELECT datetime, value FROM measurements_hourly WHERE timeseries_id = ", tsid, " AND datetime BETWEEN '", sub.start_date, "' AND '", sub.end_date, "' ORDER BY datetime DESC;"))
      } else if (rate == "max") {
        trace_data <- dbGetQueryDT(con, paste0("SELECT datetime, value FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime BETWEEN '", sub.start_date, "' AND '", sub.end_date, "' ORDER BY datetime DESC LIMIT 200000;"))
        if (nrow(trace_data) > 0) {
          if (min(trace_data$datetime) > sub.start_date) {
            infill <- dbGetQueryDT(con, paste0("SELECT datetime, value FROM measurements_hourly WHERE timeseries_id = ", tsid, " AND datetime BETWEEN '", sub.start_date, "' AND '", min(trace_data$datetime) - 1, "' ORDER BY datetime DESC;"))
            trace_data <- rbind(infill, trace_data)
          }
        }
        
      }
      attr(trace_data$datetime, "tzone") <- tzone
    }
    
    # fill gaps with NA values
    # Since recording rate can change within a timeseries, use calculate_period and some data.table magic to fill in gaps
    min_trace <- suppressWarnings(min(trace_data$datetime, na.rm = TRUE))
    if (!is.infinite(min_trace)) {
      # Assume calculate_period correctly sets up the 'period_secs' and 'expected' columns
      trace_data <- calculate_period(trace_data, tsid)
      trace_data[, period_secs := as.numeric(lubridate::period(period))]
      # Shift datetime and add period_secs to compute the 'expected' next datetime
      trace_data[, expected := data.table::shift(datetime, type = "lead") - period_secs]
      # Create 'gap_exists' column to identify where gaps are
      trace_data[, gap_exists := datetime > expected & !is.na(expected)]
      # Find indices where gaps exist
      gap_indices <- which(trace_data$gap_exists)
      # Create a data.table of NA rows to be inserted
      na_rows <- data.table::data.table(datetime = trace_data[gap_indices, datetime] - 1,  # Subtract 1 second (or any small time unit) to place it just before the gap
                                        value = NA)
      # Combine with NA rows
      trace_data <- data.table::rbindlist(list(trace_data[, c("datetime", "value")], na_rows), use.names = TRUE)
      # order by datetime
      data.table::setorder(trace_data, datetime) 
      
      # Find out where trace_data values need to be filled in with daily means (this usually only deals with HYDAT daily mean data)
      if (min_trace > sub.start_date) {
        extra <- dbGetQueryDT(con, paste0("SELECT date AS datetime, value FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date < '", min(trace_data$datetime), "' AND date >= '", sub.start_date, "';"))
        extra$datetime <- as.POSIXct(extra$datetime, tz = "UTC")
        attr(extra$datetime, "tzone") <- tzone
        trace_data <- rbind(trace_data, extra)
      }
    } else { #this means that no trace data could be had because there are no measurements in measurements_continuous or the hourly views table
      trace_data <- dbGetQueryDT(con, paste0("SELECT date AS datetime, value FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date >= '", sub.start_date, "' AND date <= '", sub.end_date, "';"))
      trace_data$datetime <- as.POSIXct(trace_data$datetime, tz = "UTC")
      attr(trace_data$datetime, "tzone") <- tzone
      trace_data <- rbind(trace_data, trace_data)
    }
    if (datum.conv$conversion_m != 0) {
      if (historic_range) {
        range_data$min <- range_data$min + datum.conv$conversion_m
        range_data$max <- range_data$max + datum.conv$conversion_m
        range_data$q25 <- range_data$q25 + datum.conv$conversion_m
        range_data$q75 <- range_data$q75 + datum.conv$conversion_m
      }
    }
    if (historic_range) {
      data[[paste0(location, "_", parameter_code)]][["range_data"]] <- range_data
    }
    
    if (nrow(trace_data) == 0) {
      if (!historic_range) {
        warning("No data found for location ", location, " parameter ", parameter, " and time range specified.")
        remove <- c(remove, i)
      } else if (nrow(historic_range) > 0) {
        warning("No data found for location ", location, " parameter ", parameter, " and time range specified, but historical data does exist and will be plotted.")
      } else if (nrow(historic_range) == 0) {
        warning("No data found for location ", location, " parameter ", parameter, " and time range specified, and no historical data exists.")
        remove <- c(remove, i)
      }
    } else {
      if (datum.conv$conversion_m != 0) {
        trace_data$value <- trace_data$value + datum.conv$conversion_m
      }
      trace_data <- trace_data[order(trace_data$datetime),]
      
      if (!is.null(filter)) { # Use the same approach as in plotOverlap to filter the value column
        if (!inherits(filter, "numeric")) {
          message("Parameter 'filter' was modified from the default NULL but not properly specified as a class 'numeric'. Filtering will not be done.")
        } else {
          if (parameter %in% c("water level", "niveau d'eau", "water flow", "d\u00E9bit d'eau", "snow depth", "profondeur de la neige", "snow water equivalent", "\u00E9quivalent en eau de la neige", "distance") | grepl("precip", parameter, ignore.case = TRUE)) { #remove all values less than 0
            trace_data[trace_data$value < 0 & !is.na(trace_data$value),"value"] <- NA
          } else { #remove all values less than -100 (in case of negative temperatures or -DL values in lab results)
            trace_data[trace_data$value < -100 & !is.na(trace_data$value),"value"] <- NA
          }
          
          rollmedian <- zoo::rollapply(trace_data$value, width = filter, FUN = "median", align = "center", fill = "extend", na.rm = TRUE)
          rollmad <- zoo::rollapply(trace_data$value, width = filter, FUN = "mad", align = "center", fill = "extend", na.rm = TRUE)
          outlier <- abs(trace_data$value - rollmedian) > 5 * rollmad
          trace_data$value[outlier] <- NA
        }
      }
    }
    data[[paste0(location, "_", parameter_code)]][["trace_data"]] <- trace_data
  } # End of loop iterating over each location:parameter:record_rate combo
  
  if (length(remove) > 0) {
    timeseries <- timeseries[-remove,]
    log <- log[-remove]
  }
  
  if (nrow(timeseries) == 0) {
    stop("Couldn't find data for any of the location and parameter combinations within the time range you specified.")
  }
  
  
  if (language == "fr") {
    for (i in 1:nrow(timeseries)) {
      name <- DBI::dbGetQuery(con, paste0("SELECT name_fr FROM locations where location = '", timeseries[i, "location"], "';"))[1,1]
      if (is.na(name)) {
        name <- DBI::dbGetQuery(con, paste0("SELECT name FROM locations where location = '", timeseries[i, "location"], "';"))[1,1]
      }
      timeseries[i, "name"] <- titleCase(name, language)
    }
  }
  if (language == "en") {
    for (i in 1:nrow(timeseries)) {
      name <- DBI::dbGetQuery(con, paste0("SELECT name FROM locations where location = '", timeseries[i, "location"], "';"))[1,1]
      timeseries[i, "name"] <- titleCase(name, language)
    }      
  }
  
  # Make the title ###################################
  if (title == TRUE) {
    if (is.null(custom_title)) {
      generate_plot_title <- function(tbl) {
        # Group by location to handle different parameters per location
        title <- tbl %>%
          dplyr::group_by(location, name) %>%
          dplyr::summarise(parameter_names = paste(parameter_name, collapse = ", "), .groups = "drop") %>%
          # Truncate long location names
          dplyr::mutate(name = dplyr::if_else(nchar(name) > 40, substr(name, 1, 37) %>% paste0("..."), name),
                        # Format title line by line
                        title = paste(name, " (", .data$parameter_names, ")", sep = "")) %>%
          # Combine all lines into a single title
          dplyr::summarise(plot_title = paste(title, collapse = "<br>")) %>%
          dplyr::pull("plot_title") # Extract the plot title as a string
        return(title)
      }
      
      title <- generate_plot_title(timeseries)
      
    } else {
      title <- custom_title
    }
  }
  
  
  ### Code below could be used if wanting to automatically send traces to one or the other axis
  # Determine the appropriate axis to send each trace to   ############################
  # ranges <- sapply(data, function(x) range(x[["trace_data"]]$value, na.rm = TRUE))
  # timeseries$min <- ranges[1,]
  # timeseries$max <- ranges[2,]
  # timeseries$spread <- timeseries$max - timeseries$min
  # 
  # # Calculate a custom distance matrix emphasizing spread differences
  # calculate_custom_distance <- function(df) {
  #   n <- nrow(df)
  #   dist_matrix <- matrix(0, n, n)
  #   for (i in 1:n) {
  #     for (j in 1:n) {
  #       # Custom distance: emphasizing spread differences more heavily
  #       spread_diff <- abs(log(df$spread[i] + 1) - log(df$spread[j] + 1)) * 2  # Weight spread differences more
  #       position_diff <- abs(df$min[i] - df$min[j]) + abs(df$max[i] - df$max[j])
  #       dist_matrix[i, j] <- spread_diff + position_diff
  #     }
  #   }
  #   as.dist(dist_matrix)
  # }
  # # Apply the function
  # custom_dist_matrix <- calculate_custom_distance(timeseries)
  # # Hierarchical clustering with the custom distance matrix
  # hc <- hclust(custom_dist_matrix)
  # clusters <- cutree(hc, k = yaxis)
  # # Assign clusters to y-axes
  # timeseries$axis <- paste0("y", clusters)
  
  timeseries$axis <- paste0("y", seq_along(1:nrow(timeseries)))
  
  
  # Make the plot ###################################
  
  colors <- grDevices::colorRampPalette(c("#0097A9", "#7A9A01", "#F2A900","#DC4405"))(length(data))
  
  # Function to add an opacity value to a hex color
  add_opacity_to_color <- function(hex_color, opacity) {
    rgb_vals <- grDevices::col2rgb(hex_color)
    sprintf("rgba(%d, %d, %d, %.2f)", rgb_vals[1,], rgb_vals[2,], rgb_vals[3,], opacity)
  }
  
  # Create lists of y-axis properties
  n_axes <- nrow(timeseries)
  for (i in 1:n_axes) {
    # Make axis titles
    # Truncate long strings
    if (nchar(timeseries[i, "name"]) > 20) {
      name <- paste0(substr(timeseries[i, "name"], 1, 17), "...")
    } else {
      name <- timeseries[i, "name"]
    }
    if (nchar(timeseries[i, "parameter_name"]) > 18) {
      parameter_name <- paste0(substr(timeseries[i, "parameter_name"], 1, 15), "...")
    } else {
      parameter_name <- timeseries[i, "parameter_name"]
    }
    timeseries[i, "trace_title"] <- paste0(name, " (", parameter_name, ", ", timeseries[i, "units"], ")")
    timeseries[i, "tooltip_title"] <- paste0(name, " (", parameter_name, ")")
    timeseries[i, "range_title"] <- paste0(name, " (", parameter_name, ", ", timeseries[i, "units"], ")")
    
    tmp <- list(
      titlefont = list(color = colors[i], size = 14),
      tickfont = list(color = colors[i], size = 12),
      tickcolor = colors[i],
      ticks = "outside",
      overlaying = if (i > 1) "y" else NULL,
      side = if (i %% 2 == 0) "right" else "left",  # Check if even or odd
      title = list(
        text = timeseries[i, "trace_title"],
        standoff = ((i - 1) %/% 2) * 22
      ),
      type = if (log[i]) "log" else "linear",
      zeroline = FALSE,
      showline = TRUE,
      showgrid = FALSE,
      showspikes = TRUE,
      spikethickness = 2
    )
    axis <- timeseries$axis[i]
    assign(axis, tmp)
  }
  
  
  plot <- plotly::plot_ly()
  
  if (historic_range) {
    for (i in 1:n_axes) {
      data.sub <- data[[i]][["range_data"]][!is.na(data[[i]][["range_data"]]$min) & !is.na(data[[i]][["range_data"]]$max), ]
      data.sub$tooltip <- paste0(timeseries[i, "tooltip_title"], ": Min: ", round(data.sub$min, 2), " Max: ", round(data.sub$max, 2), ", ", as.Date(data.sub$datetime))
      plot <- plot %>%
        plotly::add_ribbons(
          data = data.sub,
          x = ~datetime, 
          ymin = ~min,
          ymax = ~max, 
          name = timeseries[i, "range_title"],
          color = I(add_opacity_to_color(colors[i], 0.15)),
          yaxis = timeseries[i, "axis"],
          line = list(width = 0.3),
          hoverinfo = "text",
          text = ~tooltip
        )
    }
  }
  # Add the traces
  for (i in 1:n_axes) {
    data.sub <- data[[i]][["trace_data"]]
    data.sub$tooltip <- paste0(timeseries[i, "tooltip_title"], ": ", round(data.sub$value, 4), ", ", data.sub$datetime)
    plot <- plot %>%
      plotly::add_lines(
        data = data.sub, 
        x = ~datetime, 
        y = ~value, 
        type = "scatter", 
        mode = "lines", 
        name = timeseries[i, "trace_title"], 
        yaxis = timeseries[i, "axis"],
        color = I(colors[i]), 
        hoverinfo = "text",
        text = ~tooltip)
  }
  
  
  # Dynamically create axis titles and other settings
  axis_list <- list()
  for (i in 1:n_axes) {
    axis_name <- paste0("yaxis", ifelse(i == 1, "", i))  # yaxis for the first, yaxis2, yaxis3, ...
    axis_list[[axis_name]] <- list(
      title = paste("Axis", i),
      title_standoff = 10,
      overlaying = if (i > 1) "y" else NA,
      side = ifelse(i %% 2 == 0, "right", "left"),
      showgrid = FALSE
    )
  }
  
  # Collect the axis variables dynamically
  axis_layouts <- lapply(seq(n_axes), function(i) {
    axis_var_name <- paste0("y", i)
    get(axis_var_name)
  })
  
  # Names for the layout list based on number of axes
  names(axis_layouts) <- sapply(seq(n_axes), function(i) paste0("yaxis", if (i == 1) "" else i))
  
  # Add other layout parameters
  layout_params <- list(
    title = list(text = title, x = 0.05, xref = "container"),
    margin = list(
      l = 60 + (20 * ((n_axes - 1) %/% 2)),
      r = 60 + (20 * ((n_axes - 1) %/% 2)),
      b = 0,
      t = 30 * length(unique(locations))
    ),
    xaxis = list(
      title = list(standoff = 0, font = list(size = 1)),
      showgrid = FALSE,
      showline = TRUE,
      showspikes = TRUE,
      tickformat = if (language == "fr") "%d %b '%y" else "%b %d '%y",
      rangeslider = list(visible = if (slider) TRUE else FALSE)
    ),
    hovermode = "closest"
  )
  
  # Combine axis layouts with other layout settings
  total_layout_settings <- c(layout_params, axis_layouts)
  # Apply all settings using do.call to handle dynamic list of settings
  plot <- do.call(plotly::layout, c(list(plot), total_layout_settings))
  
  plot <- plotly::config(plot, locale = language)
  
  return(plot)
}

