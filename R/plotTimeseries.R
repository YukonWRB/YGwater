#' Plot a continous timeseries from the aquacache
#'
#' @description
#' `r lifecycle::badge('stable')`
#' 
#' This function plots continuous timeseries from the aquacache. The plot is zoomable and hovering over the historical ranges or the measured values brings up additional information. If corrections are applied to the data within AquaCache, the corrected values will be used.
#' 
#' @param location The location for which you want a plot.
#' @param sub_location Your desired sub-location, if applicable. Default is NULL as most locations do not have sub-locations. Specify as the exact name of the sub-location (character) or the sub-location ID (numeric).
#' @param parameter The parameter name (text) or code (numeric) that you wish to plot. The location:sublocation:parameter combo must be in the local database.
#' @param record_rate The recording rate for the parameter and location to plot. In most cases there are not multiple recording rates for a location and parameter combo and you can leave this NULL. Otherwise NULL will default to the most frequent record rate, or you can set this as one of '< 1 day', '1 day', '1 week', '4 weeks', '1 month', 'year'.
#' @param period_type The period type for the parameter and location to plot. Options other than the default NULL are 'sum', 'min', 'max', or '(min+max)/2', which is how the daily 'mean' temperature is often calculated for meteorological purposes. NULL will search for what's available and get the first timeseries found in this order: 'instantaneous', followed by the 'mean', '(min+max)/2', 'min', and 'max'.
#' @param z Depth/height in meters further identifying the timeseries of interest. Default is NULL, and where multiple elevations exist for the same location/parameter/record_rate/period_type combo the function will default to the absolute elevation value closest to ground. Otherwise set to a numeric value.
#' @param z_approx Number of meters by which to approximate the elevation. Default is NULL, which will use the exact elevation. Otherwise set to a numeric value.
#' @param start_date The day or datetime on which to start the plot as character, Date, or POSIXct. Default is one year ago.
#' @param end_date The day or datetime on which to end the plot as character, Date, or POSIXct. Default is today.
#' @param invert Should the y-axis be inverted? TRUE/FALSE, or leave as default NULL to use the database default value.
#' @param slider Should a slider be included to show where you are zoomed in to? If TRUE the slider will be included but this prevents horizontal zooming or zooming in using the box tool. If legend_position is set to 'h', slider will be set to FALSE due to interference. Default is TRUE.
#' @param datum Should a vertical offset be applied to the data? Looks for it in the database and applies it if it exists. Default is TRUE.
#' @param title Should a title be included?
#' @param custom_title Custom title to be given to the plot. Default is NULL, which will set the title as the location name as entered in the database.
#' @param filter Should an attempt be made to filter out spurious data? Will calculate the rolling IQR and filter out clearly spurious values. Set this parameter to an integer, which specifies the rolling IQR 'window'. The greater the window, the more effective the filter but at the risk of filtering out real data. Negative values are always filtered from parameters "water level" ("niveau d'eau"), "flow" ("débit"), "snow depth" ("profondeur de la neige"), "snow water equivalent" ("équivalent en eau de la neige"), "distance", and any "precip" related parameter. Otherwise all values below -100 are removed.
#' @param historic_range Should the historic range be plotted? Default is TRUE.
#' @param lang The language to use for the plot. Currently only "en" and "fr" are supported. Default is "en".
#' @param line_scale A scale factor to apply to the size (width) of the line. Default is 1.
#' @param axis_scale A scale factor to apply to the size of axis labels. Default is 1.
#' @param legend_scale A scale factor to apply to the size of text in the legend. Default is 1.
#' @param legend_position The position of the legend, 'v' for vertical on the right side or 'h' for horizontal on the bottom. Default is 'v'. If 'h', slider will be set to FALSE due to interference.
#' @param gridx Should gridlines be drawn on the x-axis? Default is FALSE
#' @param gridy Should gridlines be drawn on the y-axis? Default is FALSE
#' @param rate The rate at which to plot the data. Default is NULL, which will adjust for reasonable plot performance depending on the date range. Otherwise set to one of "max", "hour", "day".
#' @param tzone The timezone to use for the plot. Default is "auto", which will use the system default timezone. Otherwise set to a valid timezone string.
#' @param con A connection to the target database. NULL uses [AquaConnect()] and automatically disconnects.
#'
#' @return A plotly object 
#' 
#' @export

plotTimeseries <- function(location,
                           sub_location = NULL,
                           parameter,
                           record_rate = NULL,
                           period_type = NULL,
                           z = NULL,
                           z_approx = NULL,
                           start_date = Sys.Date() - 365,
                           end_date = Sys.Date(),
                           invert = NULL,
                           slider = TRUE,
                           datum = TRUE,
                           title = TRUE,
                           custom_title = NULL,
                           filter = NULL,
                           historic_range = TRUE,
                           lang = "en",
                           line_scale = 1,
                           axis_scale = 1,
                           legend_scale = 1,
                           legend_position = "v",
                           gridx = FALSE,
                           gridy = FALSE,
                           rate = NULL,
                           tzone = "auto",
                           con = NULL) 
{
# 
# location <- "08AA005"
# sub_location <- NULL
# parameter = 1165
# start_date <- "2020-01-01"
# end_date <- Sys.time()
# record_rate = NULL
# period_type = NULL
# z = NULL
# z_approx = NULL
# invert = NULL
# slider = TRUE
# datum = FALSE
# title = TRUE
# custom_title = NULL
# filter = NULL
# historic_range = TRUE
# lang = "en"
# line_scale = 1
# axis_scale = 1
# legend_scale = 1
# legend_position = "v"
# rate = "max"
# tzone = "auto"
# con = NULL
# gridx = FALSE
# gridy = FALSE

  # Checks and initial work ##########################################
  
  # Deal with non-standard evaluations from data.table to silence check() notes
  period_secs <- period <- expected <- datetime <- gap_exists <- NULL
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  rlang::check_installed("plotly", "necessary for interactive plots")
  
  if (tzone == "auto") {
    tzone <- Sys.timezone()
  }
  
  if (!is.null(invert)) {
    if (!inherits(invert, "logical")) {
      stop("Your entry for the parameter 'invert' is invalid. Leave it as NULL or TRUE/FALSE.")
    }
  }
  
  if (!is.null(rate)) {
    tolower(rate)
    if (!(rate %in% c("max", "hour", "day"))) {
      stop("Your entry for the parameter 'rate' is invalid. Please review the function documentation and try again.")
    }
  }
  
  if (!is.null(z)) {
    if (!is.numeric(z)) {
      stop("Your entry for the parameter 'z' is invalid. Please review the function documentation and try again.")
    }
    if (!is.null(z_approx)) {
      if (!is.numeric(z_approx)) {
        stop("Your entry for the parameter 'z_approx' is invalid. Please review the function documentation and try again.")
      }
    }
  }
  
  if (inherits(start_date, "character")) {
    start_date <- as.Date(start_date)
  }
  if (inherits(start_date, "Date")) {
    start_date <- as.POSIXct(start_date, tz = tzone)
    start_date <- start_date + 24*60*60
  }
  if (inherits(end_date, "character")) {
    end_date <- as.Date(end_date)
  }
  if (inherits(end_date, "Date")) {
    end_date <- as.POSIXct(end_date, tz = tzone)
    end_date <- end_date + 24*60*60
  }

  #back to UTC because DB queries are in UTC
  attr(start_date, "tzone") <- "UTC"
  attr(end_date, "tzone") <- "UTC"
  
  if (!(lang %in% c("en", "fr"))) {
    stop("Your entry for the parameter 'lang' is invalid. Please review the function documentation and try again.")
  }
  
  if (!is.null(record_rate)) {
    if (!(record_rate %in% c('< 1 day', '1 day', '1 week', '4 weeks', '1 month', 'year'))) {
      warning("Your entry for parameter record_rate is invalid. It's been reset to the default NULL.")
      record_rate <- NULL
    }
  }
  
  if (!is.null(period_type)) {
    if (!(period_type %in% c('instantaneous', 'sum', 'min', 'max', '(min+max)/2'))) {
      warning("Your entry for parameter period_type is invalid. It's been reset to the default NULL.")
      period_type <- NULL
    }
  }
  
  if (!inherits(historic_range, "logical")) {
    warning("Parameter `historic_range` must be TRUE or FALSE. Resetting it to FALSE.")
    historic_range <- FALSE
  }
  
  # Determine the timeseries and adjust the date range #################
  location_id <- DBI::dbGetQuery(con, paste0("SELECT location_id FROM locations WHERE location = '", location, "';"))[1,1]
  #Confirm parameter and location exist in the database and that there is only one entry
  if (inherits(parameter, "character")) {
    parameter <- tolower(parameter)
    escaped_parameter <- gsub("'", "''", parameter)
    parameter_tbl <- DBI::dbGetQuery(con, 
                                     paste0("SELECT parameter_id, param_name, param_name_fr, plot_default_y_orientation, unit_default FROM parameters WHERE param_name = '", escaped_parameter, "' OR param_name_fr = '", escaped_parameter, "';")
    )
    parameter_code <- parameter_tbl$parameter_id[1]
    if (is.na(parameter_code)) {
      stop("The parameter you entered does not exist in the database.")
    }
  } else if (inherits(parameter, "numeric")) {
    parameter_tbl <- DBI::dbGetQuery(con, paste0("SELECT parameter_id, param_name, param_name_fr, plot_default_y_orientation, unit_default FROM parameters WHERE parameter_id = ", parameter, ";"))
    if (nrow(parameter_tbl) == 0) {
      stop("The parameter you entered does not exist in the database.")
    }
    parameter_code <- parameter
  }
  
  # Where column param_name_fr is not filled in, use the English name
  parameter_tbl$param_name_fr[is.na(parameter_tbl$param_name_fr)] <- parameter_tbl$param_name[is.na(parameter_tbl$param_name_fr)]

  if (lang == "fr") {
    parameter_name <- titleCase(parameter_tbl$param_name_fr[1], "fr")
  } else if (lang == "en" || is.na(parameter_name)) {
    parameter_name <- titleCase(parameter_tbl$param_name[1], "en")
  }
  
  if (is.null(sub_location)) {
    # Check if there are multiple sub_locations for this parameter_code, location regardless of sub_location. If so, throw a stop
    sub_loc_check <- DBI::dbGetQuery(con, paste0("SELECT sub_location_id FROM timeseries WHERE location_id = ", location_id, " AND parameter_id = ", parameter_code, " AND sub_location_id IS NOT NULL;"))
    if (nrow(sub_loc_check) > 1) {
      stop("There are multiple sub-locations for this location and parameter. Please specify a sub-location.")
    }
    
    if (is.null(record_rate)) { # period_type may or may not be NULL
      if (is.null(period_type)) { #both record_rate and period_type are NULL
        exist_check <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id, record_rate, period_type, z, start_datetime, end_datetime FROM timeseries WHERE location_id = ", location_id, " AND parameter_id = ", parameter_code, ";"))
      } else { #period_type is not NULL but record_rate is
        exist_check <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id, record_rate, z, start_datetime, end_datetime FROM timeseries WHERE location_id = ", location_id, " AND parameter_id = ", parameter_code, " AND period_type = '", period_type, "';"))
      }
    } else if (is.null(period_type)) { #record_rate is not NULL but period_type is
      exist_check <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id, period_type, z, start_datetime, end_datetime FROM timeseries WHERE location_id = ", location_id, " AND parameter_id = ", parameter_code, " AND record_rate = '", record_rate, "';"))
    } else { #both record_rate and period_type are not NULL
      exist_check <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id, z, start_datetime, end_datetime FROM timeseries WHERE location_id = ", location_id, " AND parameter_id = ", parameter_code, " AND record_rate = '", record_rate, "' AND period_type = '", period_type, "';"))
    }
  } else {  #sub location was specified
    # Find the sub location_id
    if (inherits(sub_location, "character")) {
      escaped_sub_location <- gsub("'", "''", sub_location)
      sub_location_tbl <- DBI::dbGetQuery(con, paste0("SELECT sub_location_id FROM sub_locations WHERE sub_location_name = '", escaped_sub_location, "';"))
      if (nrow(sub_location_tbl) == 0) {
        stop("The sub-location you entered does not exist in the database.")
      }
      sub_location_id <- sub_location_tbl$sub_location_id[1]
    } else if (inherits(sub_location, "numeric")) {
      sub_location_id <- sub_location
    }
    if (is.null(record_rate)) { # period_type may or may not be NULL
      if (is.null(period_type)) { #both record_rate and period_type are NULL
        exist_check <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id, record_rate, period_type, z, start_datetime, end_datetime FROM timeseries WHERE location_id = ", location_id, " AND sub_location_id = ", sub_location_id, " AND parameter_id = ", parameter_code, ";"))
      } else { #period_type is not NULL but record_rate is
        exist_check <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id, record_rate, z, start_datetime, end_datetime FROM timeseries WHERE location_id = ", location_id, " AND sub_location_id = ", sub_location_id, " AND parameter_id = ", parameter_code, " AND period_type = '", period_type, "';"))
      }
    } else if (is.null(period_type)) { #record_rate is not NULL but period_type is
      exist_check <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id, period_type, z, start_datetime, end_datetime FROM timeseries WHERE location_id = ", location_id, " AND sub_location_id = ", sub_location_id, " AND parameter_id = ", parameter_code, " AND record_rate = '", record_rate, "';"))
    } else { #both record_rate and period_type are not NULL
      exist_check <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id, z, start_datetime, end_datetime FROM timeseries WHERE location_id = ", location_id, " AND sub_location_id = ", sub_location_id, " AND parameter_id = ", parameter_code, " AND record_rate = '", record_rate, "' AND period_type = '", period_type, "';"))
    }
  }
  
  # Narrow down by z if necessary
  if (!is.null(z)) {
    if (is.null(z_approx)) {
      exist_check <- exist_check[exist_check$z == z, ]
    } else {
      exist_check <- exist_check[abs(exist_check$z - z) < z_approx, ]
    }
  }
  
  if (nrow(exist_check) == 0) {
    if (is.null(record_rate) & is.null(period_type) & is.null(z)) {
      stop("There doesn't appear to be a match in the database for location ", location, ", parameter ", parameter, ", and continuous category data.")
    } else {
      stop("There doesn't appear to be a match in the database for location ", location, ", parameter ", parameter, ", record rate ", if (is.null(record_rate)) "(not specified)" else record_rate, ", period type ", if (is.null(period_type)) "(not specified)" else period_type, ", z of ", if (is.null(z)) "(not specified)" else z, " and continuous category data. You could try leaving the record rate and/or period_type to the default 'NULL', or explore different z or z_approx values.")
    }
  } else if (nrow(exist_check) > 1) {
    if (is.null(record_rate)) {
      warning("There is more than one entry in the database for location ", location, ", parameter ", parameter, ", and continuous category data. Since you left the record_rate as NULL, selecting the one(s) with the most frequent recording rate.")
      temp <- exist_check[exist_check$record_rate == "< 1 day", ]
      if (nrow(temp) == 0) {
        temp <- exist_check[exist_check$record_rate == "1 day", ]
      }
      if (nrow(temp) == 0) {
        temp <- exist_check[exist_check$record_rate == "1 week", ]
      }
      if (nrow(temp) == 0) {
        temp <- exist_check[exist_check$record_rate == "4 weeks", ]
      }
      if (nrow(temp) == 0) {
        temp <- exist_check[exist_check$record_rate == "1 month", ]
      }
      if (nrow(temp) == 0) {
        temp <- exist_check[exist_check$record_rate == "year", ]
      }
    }
    if (nrow(temp) > 1) {
      exist_check <- temp
      if (is.null(period_type)) {
        warning("There is more than one entry in the database for location ", location, ", parameter ", parameter, ", and continuous category data. Since you left the period_type as NULL, selecting the one(s) with the most frequent period type.")
        exist_check <- exist_check[exist_check$period_type == "instantaneous", ]
        if (nrow(exist_check) == 0) {
          exist_check <- exist_check[exist_check$period_type == "mean", ]
        }
        if (nrow(exist_check) == 0) {
          exist_check <- exist_check[exist_check$period_type == "(min+max)/2", ]
        }
        if (nrow(exist_check) == 0) {
          exist_check <- exist_check[exist_check$period_type == "min", ]
        }
        if (nrow(exist_check) == 0) {
          exist_check <- exist_check[exist_check$period_type == "max", ]
        }
      }
    } else if (nrow(temp) == 1) {
      exist_check <- temp
    }
  }
  
  # If there are multiple z values after all that, select the one closest to ground
  if (nrow(exist_check) > 1) {
    exist_check <- exist_check[which.min(abs(exist_check$z)), ]
  }
  
  if (title == TRUE) {
    if (is.null(custom_title)) {
      if (lang == "fr") {
        stn_name <- DBI::dbGetQuery(con, paste0("SELECT name_fr FROM locations where location = '", location, "'"))[1,1]
      } 
      if (lang == "en" || is.na(stn_name) == TRUE) {
        stn_name <- DBI::dbGetQuery(con, paste0("SELECT name FROM locations where location = '", location, "'"))[1,1]
      }
      stn_name <- titleCase(stn_name, lang)
    } else {
      stn_name <- custom_title
    }
  }
  
  #adjust start and end datetimes
  if (start_date < exist_check$start_datetime) {
    start_date <- exist_check$start_datetime
  }
  if (end_date > exist_check$end_datetime) {
    end_date <- exist_check$end_datetime
  }
  
  if (end_date < start_date) {
    stop("It looks like data for this location begins before your requested start date.")
  }
  
  # Find the necessary datum (latest datum)
  if (datum) {
    datum <- DBI::dbGetQuery(con, paste0("SELECT conversion_m FROM datum_conversions WHERE location_id = ", location_id, " AND current = TRUE"))
    if (is.na(datum$conversion_m)) {
      datum <- data.frame(conversion_m = 0)
    }
  } else {
    datum <- data.frame(conversion_m = 0)
  }
  
  # Find the ts units
  units <- parameter_tbl$unit_default[1]
  
  range <- seq.POSIXt(start_date, end_date, by = "day")
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
  
  tsid <- exist_check$timeseries_id
  
  # Check if we should spend the extra time to get corrected measurements
  if (!DBI::dbExistsTable(con, "measurements_continuous")) { # IF this table can't be found it means the user doesn't have direct access to it and needs to use the views
    corrections_apply <- TRUE
  } else {
    corrections_apply <- DBI::dbGetQuery(con, paste0("SELECT correction_id FROM corrections WHERE timeseries_id = ", tsid, " AND start_dt <= '", end_date, "' AND end_dt >= '", start_date, "' LIMIT 1;"))
    if (nrow(corrections_apply) == 1) {
      corrections_apply <- TRUE
    } else {
      corrections_apply <- FALSE
    }
  }

  
  if (historic_range) { # get data from the measurements_calculated_daily_corrected table for historic ranges plus values from measurements_continuous (corrected view or not). Where there isn't any data in the table fill in with the value from the daily table.
    range_end <- end_date + 1*24*60*60
    range_start <- start_date - 1*24*60*60
    range_data <- dbGetQueryDT(con, paste0("SELECT date AS datetime, min, max, q75, q25  FROM measurements_calculated_daily_corrected WHERE timeseries_id = ", tsid, " AND date BETWEEN '", range_start, "' AND '", range_end, "' ORDER BY date ASC;"))
    range_data$datetime <- as.POSIXct(range_data$datetime, tz = "UTC")
    attr(range_data$datetime, "tzone") <- tzone
    if (rate == "day") {
      trace_data <- dbGetQueryDT(con, paste0("SELECT date AS datetime, value FROM measurements_calculated_daily_corrected WHERE timeseries_id = ", tsid, " AND date BETWEEN '", start_date, "' AND '", end_date, "' ORDER BY date DESC;"))
      trace_data$datetime <- as.POSIXct(trace_data$datetime, tz = "UTC")
    } else if (rate == "hour") {
      trace_data <- dbGetQueryDT(con, paste0("SELECT datetime, value_corrected AS value FROM measurements_hourly_corrected WHERE timeseries_id = ", tsid, " AND datetime BETWEEN '", start_date, "' AND '", end_date, "' ORDER BY datetime DESC;"))
    } else if (rate == "max") {
      if (corrections_apply) {
        trace_data <- dbGetQueryDT(con, paste0("SELECT datetime, value_corrected AS value FROM measurements_continuous_corrected WHERE timeseries_id = ", tsid, " AND datetime BETWEEN '", start_date, "' AND '", end_date, "' ORDER BY datetime DESC LIMIT 200000;"))
      } else {
        trace_data <- dbGetQueryDT(con, paste0("SELECT datetime, value FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime BETWEEN '", start_date, "' AND '", end_date, "' ORDER BY datetime DESC LIMIT 200000;"))
      }
      if (nrow(trace_data) > 0) {
        if (min(trace_data$datetime) > start_date) {
          infill <- dbGetQueryDT(con, paste0("SELECT datetime, value_corrected AS value FROM measurements_hourly_corrected WHERE timeseries_id = ", tsid, " AND datetime BETWEEN '", start_date, "' AND '", min(trace_data$datetime) - 1, "' ORDER BY datetime DESC;"))
          trace_data <- rbind(infill, trace_data)
        }
      }
    }
    attr(trace_data$datetime, "tzone") <- tzone
    
    # Check if the range data extends to the end of trace data. Because range is taken from calculated daily means, it's possible that there's no data yet for the current day. In this case we'll just use the values from the last available range_data$datetime, incrementing it by a day
    if (max(range_data$datetime) < max(trace_data$datetime)) {
      last <- range_data[nrow(range_data), ]
      range_data <- rbind(range_data, data.table(datetime = last$datetime + 1*24*60*60, min = last$min, max = last$max, q25 = last$q25, q75 = last$q75))
    }
  } else { #No historic range requested
    if (rate == "day") {
      trace_data <- dbGetQueryDT(con, paste0("SELECT date AS datetime, value FROM measurements_calculated_daily_corrected WHERE timeseries_id = ", tsid, " AND date BETWEEN '", start_date, "' AND '", end_date, "' ORDER BY date DESC;"))
      trace_data$datetime <- as.POSIXct(trace_data$datetime, tz = "UTC")
    } else if (rate == "hour") {
        trace_data <- dbGetQueryDT(con, paste0("SELECT datetime, value_corrected AS value FROM measurements_hourly_corrected WHERE timeseries_id = ", tsid, " AND datetime BETWEEN '", start_date, "' AND '", end_date, "' ORDER BY datetime DESC;"))
    } else if (rate == "max") {
      if (corrections_apply) {
        trace_data <- dbGetQueryDT(con, paste0("SELECT datetime, value_corrected AS value FROM measurements_continuous_corrected WHERE timeseries_id = ", tsid, " AND datetime BETWEEN '", start_date, "' AND '", end_date, "' ORDER BY datetime DESC LIMIT 200000;"))
      } else {
        trace_data <- dbGetQueryDT(con, paste0("SELECT datetime, value FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime BETWEEN '", start_date, "' AND '", end_date, "' ORDER BY datetime DESC LIMIT 200000;"))
      }
      if (nrow(trace_data) > 0) {
        if (min(trace_data$datetime) > start_date) {
            infill <- dbGetQueryDT(con, paste0("SELECT datetime, value_corrected AS value FROM measurements_hourly_corrected WHERE timeseries_id = ", tsid, " AND datetime BETWEEN '", start_date, "' AND '", min(trace_data$datetime) - 1, "' ORDER BY datetime DESC;"))
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
    trace_data <- calculate_period(trace_data, timeseries_id = tsid)
    trace_data[, period_secs := as.numeric(lubridate::period(period))]
    # Shift datetime and add period_secs to compute the 'expected' next datetime
    trace_data[, expected := data.table::shift(datetime, type = "lead") - period_secs]
    # Create 'gap_exists' column to identify where gaps are
    trace_data[, gap_exists := datetime < expected & !is.na(expected)]
    # Find indices where gaps exist
    gap_indices <- which(trace_data$gap_exists)
    # Create a data.table of NA rows to be inserted
    na_rows <- data.table::data.table(datetime = trace_data[gap_indices, datetime]  + 1,  # Add 1 second to place it at the start of the gap
                                      value = NA)
    # Combine with NA rows
    trace_data <- data.table::rbindlist(list(trace_data[, c("datetime", "value")], na_rows), use.names = TRUE)
    # order by datetime
    data.table::setorder(trace_data, datetime) 
    
    # Find out where trace_data values need to be filled in with daily means (this usually only deals with HYDAT daily mean data)
    if (min_trace > start_date) {
      extra <- dbGetQueryDT(con, paste0("SELECT date AS datetime, value FROM measurements_calculated_daily_corrected WHERE timeseries_id = ", tsid, " AND date < '", min(trace_data$datetime), "' AND date >= '", start_date, "';"))
      extra$datetime <- as.POSIXct(extra$datetime, tz = "UTC")
      attr(extra$datetime, "tzone") <- tzone
      trace_data <- rbind(trace_data, extra)
    }
  } else { #this means that no trace data could be had because there are no measurements in measurements_continuous or the hourly views table
    trace_data <- dbGetQueryDT(con, paste0("SELECT date AS datetime, value FROM measurements_calculated_daily_corrected WHERE timeseries_id = ", tsid, " AND date >= '", start_date, "' AND date <= '", end_date, "';"))
    trace_data$datetime <- as.POSIXct(trace_data$datetime, tz = "UTC")
    attr(trace_data$datetime, "tzone") <- tzone
    trace_data <- rbind(trace_data, trace_data)
  }
  
  if (nrow(trace_data) == 0) {
    stop("No data found for the specified location, parameter, and time range.")
  }
  
  if (datum != 0) {
    trace_data$value <- trace_data$value + datum$conversion_m
    if (historic_range) {
      range_data$min <- range_data$min + datum$conversion_m
      range_data$max <- range_data$max + datum$conversion_m
      range_data$q25 <- range_data$q25 + datum$conversion_m
      range_data$q75 <- range_data$q75 + datum$conversion_m
    }
  }
  trace_data <- trace_data[order(trace_data$datetime),]
  
  if (!is.null(filter)) { # Use the same approach as in ggplotOverlap to filter the value column
    if (!inherits(filter, "numeric")) {
      message("Parameter 'filter' was modified from the default NULL but not properly specified as a class 'numeric'. Filtering will not be done.")
    } else {
      if (parameter %in% c("water level", "niveau d'eau", "flow", "d\u00E9bit d'eau", "snow depth", "profondeur de la neige", "snow water equivalent", "\u00E9quivalent en eau de la neige", "distance") | grepl("precipitation", parameter, ignore.case = TRUE)) { #remove all values less than 0
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
  
  # Make the plot ###################################
  if (is.null(invert)) {
    if (parameter_tbl$plot_default_y_orientation[1] == "inverted") {
      invert <- TRUE
    } else {
      invert <- FALSE
    }
  }
  
  y_title <- paste0(parameter_name, if (!is.na(exist_check$z)) paste0(" ", exist_check$z, " meters") else "", " (", units, ")")
  
  plot <- plotly::plot_ly()
  if (historic_range) {
    plot <- plot %>%
      plotly::add_ribbons(data = range_data[!is.na(range_data$q25) & !is.na(range_data$q75), ],
                          x = ~datetime, 
                          ymin = ~q25, 
                          ymax = ~q75, 
                          # name = if (lang == "en") "IQR" else "EIQ", 
                          name = if (lang == "en") "Typical" else "Typique",
                          color = I("#5f9da6"), 
                          line = list(width = 0.2), 
                          hoverinfo = "text", 
                          text = ~paste0("Q25: ", round(q25, 2), ", Q75: ", round(q75, 2), " (", as.Date(datetime), ")")) %>%
      plotly::add_ribbons(data = range_data[!is.na(range_data$min) & !is.na(range_data$max), ], 
                          x = ~datetime, 
                          ymin = ~min, 
                          ymax = ~max, 
                          # name = "Min-Max", 
                          name = if (lang == "en") "Historic" else "Historique",
                          color = I("#D4ECEF"), 
                          line = list(width = 0.2), 
                          hoverinfo = "text", 
                          text = ~paste0("Min: ", round(min, 2), ", Max: ", round(max, 2), " (", as.Date(datetime), ")")) 
  }
  
  plot <- plot %>%
    plotly::add_trace(data = trace_data, 
                      x = ~datetime, 
                      y = ~value, 
                      type = "scattergl", 
                      mode = "lines",
                      line = list(width = 2.5 * line_scale),
                      name = parameter_name, 
                      color = I("#00454e"), 
                      hoverinfo = "text", 
                      text = ~paste0(parameter_name, ": ", round(value, 4), " (", datetime, ")")) %>%
    plotly::layout(
      title = if (title) list(text = stn_name, 
                              x = 0.05, 
                              xref = "container",
                              font = list(size = axis_scale * 18))
      else NULL, 
      xaxis = list(title = list(standoff = 0), 
                   showgrid = gridx, 
                   showline = TRUE, 
                   tickformat = if (lang == "en") "%b %d '%y" else "%d %b '%y",
                   titlefont = list(size = axis_scale * 14),
                   tickfont = list(size = axis_scale * 12),
                   rangeslider = list(visible = if (slider & legend_position == "v") TRUE else FALSE)), 
      yaxis = list(title = y_title, 
                   showgrid = gridy, 
                   showline = TRUE,
                   zeroline = FALSE,
                   titlefont = list(size = axis_scale * 14),
                   tickfont = list(size = axis_scale * 12),
                   autorange = if (invert) "reversed" else TRUE), 
      margin = list(b = 0,
                    t = 40 * axis_scale,
                    l = 50 * axis_scale), 
      hovermode = "x unified",
      legend = list(font = list(size = legend_scale * 12),
                    orientation = legend_position)
    ) %>%
    plotly::config(locale = lang)
  
  return(plot)
}
