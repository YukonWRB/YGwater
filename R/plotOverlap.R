#' Plot of overlapping years or portions thereof
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' A pared-down, sped up, simplified version of the original [ggplotOverlap()] function. Created for use with a simple Shiny application linked to the Flood Atlas, but will likely see other uses.
#'
#' @param location The location for which you want a plot.
#' @param sub_location Your desired sub-location, if applicable. Default is NULL as most locations do not have sub-locations. Specify as the exact name of the sub-location (character) or the sub-location ID (numeric).
#' @param parameter The parameter name (text) or code (numeric) you wish to plot. The location:parameter combo must be in the local database.
#' @param record_rate The recording rate for the parameter and location to plot. In most cases there are not multiple recording rates for a location and parameter combo and you can leave this NULL. Otherwise NULL will default to the most frequent record rate, or set this as one of '< 1 day', '1 day', '1 week', '4 weeks', '1 month', 'year'.
#' @param startDay The start day of year for the plot x-axis. Can be specified as a number from 1 to 365, as a character string of form "yyyy-mm-dd", or as a date object. Either way the day of year is the only portion used, specify years to plot under parameter `years`.
#' @param endDay The end day of year for the plot x-axis. As per `startDay`.
#' @param tzone The timezone to use for graphing. Only really evident for a small number of days.
#' @param years The years to plot. If `startDay` and `endDay` cover December 31 - January 1, select the December year(s). Max 10 years, NULL = current year.
#' @param datum Should a vertical datum be applied to the data, if available? TRUE or FALSE.
#' @param title Should a title be included?
#' @param invert Should the y-axis be inverted? TRUE/FALSE, or leave as default NULL to use the database default value.
#' @param slider Should a slider be included to show where you are zoomed in to? If TRUE the slider will be included but this prevents horizontal zooming or zooming in using the box tool. If legend_position is set to 'h', slider will be set to FALSE due to interference. Default is TRUE.
#' @param filter Should an attempt be made to filter out spurious data? Will calculate the rolling IQR and filter out clearly spurious values. Set this parameter to an integer, which specifies the rolling IQR 'window'. The greater the window, the more effective the filter but at the risk of filtering out real data. Negative values are always filtered from parameters "water level" ("niveau d'eau"), "flow" ("débit"), "snow depth" ("profondeur de la neige"), "snow water equivalent" ("équivalent en eau de la neige"), "distance", and any "precip" related parameter. Otherwise all values below -100 are removed.
#' @param historic_range Should the historic range parameters be calculated using all available data (i.e. from start to end of records) or only up to the last year specified in "years"? Choose one of "all" or "last".
#' @param line_scale A scale factor to apply to the size (width) of the lines. Default is 1.
#' @param axis_scale A scale factor to apply to the size of axis labels. Default is 1.
#' @param legend_scale A scale factor to apply to the size of text in the legend. Default is 1.
#' @param legend_position The position of the legend, 'v' for vertical on the right side or 'h' for horizontal on the bottom. Default is 'v'. If 'h', slider will be set to FALSE due to interference.
#' @param gridx Should grid lines be drawn on the x-axis? Default is FALSE
#' @param gridy Should grid lines be drawn on the y-axis? Default is FALSE
#' @param con A connection to the target database. NULL uses AquaConnect from this package and automatically disconnects.
#' @param lang The language to use for the plot. Currently only "en" and "fr" are supported. Default is "en".
#' 
#' @return An html plot.
#' 
#' @export


# location <- "09AB004"
# sub_location <- NULL
# parameter <- "water level"
# record_rate = NULL
# startDay <- 1
# endDay <- 365
# tzone <- "MST"
# years <- c(2024, 2023, 2022, 2021)
# datum <- TRUE
# title <- TRUE
# filter <- 20
# plot_scale <- 1
# lang <- "en"
# gridx = FALSE
# gridy = FALSE
# line_scale = 1
# axis_scale = 1
# legend_scale = 1
# historic_range = 'all'
# legend_position = "v"
# invert = TRUE

# location <- "29AB-M3"
# sub_location <- NULL
# parameter <- "snow water equivalent"
# record_rate = NULL
# startDay <- "2023-09-01"
# endDay <- "2024-06-01"
# tzone <- "MST"
# years <- c("2024", "2023", "2022")
# datum <- FALSE
# title <- TRUE
# filter <- NULL
# plot_scale <- 1
# con <- NULL
# lang <- "en"
# gridx = FALSE
# gridy = FALSE
# legend_position = "v"


plotOverlap <- function(location,
                        sub_location = NULL,
                        parameter,
                        record_rate = NULL,
                        startDay = 1,
                        endDay = 365,
                        tzone = "MST",
                        years = NULL,
                        datum = TRUE,
                        title = TRUE,
                        invert = NULL,
                        slider = TRUE,
                        filter = NULL,
                        historic_range = 'last',
                        line_scale = 1,
                        axis_scale = 1,
                        legend_scale = 1,
                        legend_position = 'v',
                        gridx = FALSE,
                        gridy = TRUE,
                        con = NULL,
                        lang = "en")
{
  
  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con))
  }
  
  #Suppress warnings otherwise ggplot annoyingly flags every geom that wasn't plotted
  old_warn <- getOption("warn")
  options(warn = -1)
  on.exit(options(warn = old_warn), add = TRUE)
  
  #### --------- Checks on input parameters and other start-up bits ------- ####
  if (inherits(parameter, "character")) {
    parameter <- tolower(parameter)
  }
  
  if (!is.null(invert)) {
    if (!inherits(invert, "logical")) {
      stop("Your entry for the parameter 'invert' is invalid. Leave it as NULL or TRUE/FALSE.")
    }
  }
  
  if (!(lang %in% c("en", "fr"))) {
    stop("Your entry for the parameter 'lang' is invalid. Please review the function documentation and try again.")
  }
  
  if (!is.null(record_rate)) {
    if (!(record_rate %in% c('< 1 day', '1 day', '1 week', '4 weeks', '1 month', 'year'))) {
      warning("Your entry for parameter record_rate is invalid. It's been reset to the default NULL.")
      record_rate <- NULL
    }
  }
  
  if (is.null(years)) {
    years <- lubridate::year(Sys.Date())
    null_years <- TRUE
  } else {
    null_years <- FALSE
    years <- as.numeric(years)
    years <- sort(years)
    if (length(years) > 10) {
      years <- years[(length(years) - 10):length(years)]
      message("The parameter 'years' can only have up to 10 years. It's been truncated to the most recent 10 years.")
    }
  }
  
  if (!(historic_range %in% c("all", "last"))) {
    warning("Parameter `historic_range` can only be 'all' or 'last'. Resetting it to the default 'last'.")
    historic_range <- "last"
  }
  
  
  #### ------------------ Dealing with start/end dates ---------------------- ####
  # Sort out startDay and endDay into actual dates if needed
  last_year <- max(years)
  
  leap_list <- (seq(1800, 2100, by = 4))  # Create list of all leap years
  tryCatch({ #This part will fail if startDay specified as a number
    startDay <- as.character(startDay)
    startDay <- as.POSIXct(startDay, tz = tzone)
    lubridate::year(startDay) <- last_year
  }, error = function(e) {
    startDay <<- as.numeric(startDay)
    if (last_year %in% leap_list & length(years) > 1) { #Skips over Feb 29 because feb 29 has no historical info
      if (startDay == 59) {
        startDay <<- startDay + 1
      }
    }
    startDay <<- as.POSIXct(startDay*60*60*24, origin = paste0(last_year - 1, "-12-31"), tz = "UTC")
    startDay <<- lubridate::force_tz(startDay, tzone)
  })
  tryCatch({ #This part will fail if endDay specified as a number
    endDay <- as.character(endDay)
    endDay <- as.POSIXct(endDay, tz = tzone)
    lubridate::year(endDay) <- last_year
  }, error = function(e) {
    endDay <<- as.numeric(endDay)
    if (last_year %in% leap_list & length(years) > 1) { #Skips over Feb 29 because feb 29 has no historical info
      if (endDay == 59) {
        if (endDay < 366) {
          endDay <<- endDay + 1
        }
      }
    }
    endDay <<- as.POSIXct(endDay*60*60*24, origin = paste0(last_year - 1, "-12-31 23:59:59"), tz = "UTC")
    endDay <<- lubridate::force_tz(endDay, tzone)
  })
  if (startDay > endDay) { #if the user is wanting a range overlapping the new year
    overlaps <- TRUE
    if (null_years) {
      years <- lubridate::year(Sys.Date()) - 1
      max_year <- lubridate::year(Sys.Date()) - 1
      lubridate::year(startDay) <- lubridate::year(Sys.Date()) - 1
      lubridate::year(endDay) <- lubridate::year(endDay)
    } else {
      lubridate::year(endDay) <- lubridate::year(endDay) + 1
    }
  } else {
    overlaps <- FALSE
  }
  
  
  
  if (startDay > Sys.Date()) { #If left like this it results in wonky ribbon plotting and extra 'ghost' timeseries. Since there would be no data anyways change the year, endDay can stay in the future to enable plotting graphs with only the ribbon beyond the last day.
    diff <- as.numeric(endDay - startDay)
    lubridate::year(startDay) <- lubridate::year(Sys.Date())
    if (startDay > Sys.Date()) { #Depending on where we are in the year and what the startDay is, startDay could still be in the future.
      lubridate::year(startDay) <- lubridate::year(Sys.Date()) - 1
    }
    endDay <- startDay + (diff*60*60*24)
  }
  
  day_seq <- seq.POSIXt(startDay, endDay, by = "day")
  
  if (length(day_seq) < 30) {
    warning("The date range you have selected is less than 30 days. This graph type is not optimized for fewer than 30 days.")
  }
  
  
  # Get the data ###########
  location_id <- DBI::dbGetQuery(con, paste0("SELECT location_id FROM locations WHERE location = '", location, "';"))[1,1]
  #Confirm parameter and location exist in the database and that there is only one entry
  if (inherits(parameter, "character")) {
    escaped_parameter <- gsub("'", "''", parameter)
    parameter_tbl <- DBI::dbGetQuery(con, paste0("SELECT parameter_id, param_name, param_name_fr, plot_default_y_orientation FROM parameters WHERE param_name = '", escaped_parameter, "' OR param_name_fr = '", escaped_parameter, "';"))
    parameter_code <- parameter_tbl$parameter_id[1]
    if (is.na(parameter_code)) {
      stop("The parameter you entered does not exist in the database.")
    }
  } else if (inherits(parameter, "numeric")) {
    parameter_tbl <- DBI::dbGetQuery(con, paste0("SELECT parameter_id, param_name, param_name_fr, plot_default_y_orientation FROM parameters WHERE parameter_id = ", parameter, ";"))
    if (nrow(parameter_tbl) == 0) {
      stop("The parameter you entered does not exist in the database.")
    }
    parameter_code <- parameter
  }
  # Default to the english name if the french name is not available
  parameter_tbl[is.na(parameter_tbl$param_name_fr), "param_name_fr"] <- parameter_tbl[is.na(parameter_tbl$param_name_fr), "param_name"]
  
  if (lang == "fr") {
    parameter_name <- titleCase(parameter_tbl$param_name_fr[1], "fr")
  }
  if (lang == "en" || is.na(parameter_name)) { # Some parameters don't have a french name in the DB
    parameter_name <- titleCase(parameter_tbl$param_name[1], "en")
  }
  
  if (is.null(sub_location)) {
    # Check if there are multiple timeseries for this parameter_code, location regardless of sub_location. If so, throw a stop
    sub_loc_check <- DBI::dbGetQuery(con, paste0("SELECT sub_location_id FROM timeseries WHERE location_id = ", location_id, " AND parameter_id = ", parameter_code, ";"))
    if (nrow(sub_loc_check) > 1) {
      stop("There are multiple sub-locations for this location and parameter. Please specify a sub-location.")
    }
    
    if (is.null(record_rate)) {
      exist_check <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id, record_rate FROM timeseries WHERE location_id = ", location_id, " AND parameter_id = ", parameter_code, " AND period_type = 'instantaneous';"))
    } else {
      exist_check <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location_id = ", location_id, " AND parameter_id = ", parameter_code, " AND period_type = 'instantaneous' AND record_rate = '", record_rate, "';"))
    }
  } else { # sub location is specified
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
    if (is.null(record_rate)) {
      exist_check <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id, record_rate FROM timeseries WHERE location_id = ", location_id, " AND parameter_id = ", parameter_code, " AND period_type = 'instantaneous' AND sub_location_id = '", sub_location_id, "';"))
    } else {
      exist_check <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id FROM timeseries WHERE location_id = ", location_id, " AND parameter_id = ", parameter_code, " AND period_type = 'instantaneous' AND record_rate = '", record_rate, "' AND sub_location_id = '", sub_location_id, "';"))
    }
  }

  if (nrow(exist_check) == 0) {
    if (is.null(record_rate)) {
      stop("There doesn't appear to be a match in the database for location ", location, ", parameter ", parameter, ", and continuous category data.")
    } else {
      stop("There doesn't appear to be a match in the database for location ", location, ", parameter ", parameter, ", record rate ", record_rate, " and continuous category data. You could try leaving the record rate to the default 'null'.")
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
  
  # Find the ts units
  units <- DBI::dbGetQuery(con, paste0("SELECT unit_default FROM parameters WHERE parameter_id = ", parameter_code, ";"))
  
  # Find the necessary datum (latest datum)
  if (datum) {
    if (units != "m") {
      warning("The parameter you are plotting is not in meters. Datum will not be applied.")
      datum <- data.frame(conversion_m = 0)
    } else {
      datum <- DBI::dbGetQuery(con, paste0("SELECT conversion_m FROM datum_conversions WHERE location_id = ", location_id, " AND current = TRUE"))
    }
  } else {
    datum <- data.frame(conversion_m = 0)
  }
  
  # Get the necessary data -------------------
  # start with daily means data
  daily_end <- endDay
  daily_start <- startDay
  lubridate::year(daily_start) <- min(years) - 1
  if (historic_range == "all") {
    lubridate::year(daily_end) <- max(max(years) + 1, lubridate::year(Sys.time()))
    daily_end <- daily_end + 60*60*24 #adds a day so that the ribbon is complete for the whole plotted line
    if (lubridate::month(daily_end) == 2 & lubridate::day(daily_end) == 29) {
      daily_end <- daily_end + 60*60*24
    }
  } else if (historic_range == "last") {
    if (overlaps) {
      lubridate::year(daily_end) <- last_year + 1
    } else {
      lubridate::year(daily_end) <- last_year
    }
    daily_end <- daily_end + 60*60*24 #adds a day so that the ribbon is complete for the whole plotted line
    if (lubridate::month(daily_end) == 2 & lubridate::day(daily_end) == 29) {
      daily_end <- daily_end + 60*60*24
    }
  }
  daily <- DBI::dbGetQuery(con, paste0("SELECT date, value, max, min, q75, q25 FROM measurements_calculated_daily_corrected WHERE timeseries_id = ", tsid, " AND date BETWEEN '", daily_start, "' AND '", daily_end, "' ORDER by date ASC;"))
  
  
  #Fill in any missing days in daily with NAs
  all_dates <- seq(min(daily$date), max(daily$date), by = "1 day")
  complete <- data.frame(date = all_dates, value = NA, max = NA, min = NA, q75 = NA, q25 = NA)
  complete[match(daily$date, all_dates) , ] <- daily
  daily <- complete
  
  daily$date <- as.POSIXct(daily$date) + 12*60*60 # to posixct and not date so that it plays well with realtime df
  daily$date <- lubridate::force_tz(daily$date, tzone)
  names(daily)[names(daily) == "date"] <- "datetime"
  
  dates <- lubridate::POSIXct(tz = tzone) #creates empty posixct vector to hold days missing realtime data and needing to be infilled with daily
  realtime <- data.frame()
  for (i in rev(years)) { #Using rev so that the most recent year gets realtime, if possible
    start <- as.POSIXct(paste0(i, substr(startDay, 5, 16)), tz = tzone)
    start_UTC <- start
    attr(start_UTC, "tzone") <- "UTC"
    end <- as.POSIXct(paste0(i, substr(endDay, 5, 10), " 23:59:59"), tz = tzone)
    if (overlaps) {
      lubridate::year(end) <- lubridate::year(end) + 1
    }
    end_UTC <- end
    attr(end_UTC, "tzone") <- "UTC"
    if (nrow(realtime) < 20000) { # limits the number of data points to 20000 for performance (rest is populated with daily means. Gives 3 years of data at 1 hour intervals)
      new_realtime <- DBI::dbGetQuery(con, paste0("SELECT datetime, value_corrected AS value FROM measurements_hourly_corrected WHERE timeseries_id = ", tsid, " AND datetime BETWEEN '", as.character(start_UTC), "' AND '", as.character(end_UTC), "' AND value_corrected IS NOT NULL")) #SQL BETWEEN is inclusive. null values are later filled with NAs for plotting purposes.
      if (nrow(new_realtime) > 20000) {
        new_realtime <- new_realtime[order(new_realtime$datetime) , ]
        new_realtime <- utils::tail(new_realtime, 20000) #Retain only max 20000 data points for plotting performance
        end_new_dates <- min(new_realtime$datetime)
        new_dates <- seq.POSIXt(start, end_new_dates, by = "days")
        dates <- c(dates, new_dates)
      }
      if (nrow(new_realtime) > 0) {
        # Fill in any missing hours in realtime with NAs
        all_times <- seq(min(new_realtime$datetime), max(new_realtime$datetime), by = "1 hour")
        complete <- data.frame(datetime = all_times, value = NA)
        complete[match(new_realtime$datetime, all_times) , ] <- new_realtime
        new_realtime <- complete
        realtime <- rbind(realtime, new_realtime)
        get_daily <- FALSE
      } else {
        get_daily <- TRUE
      }
    } else {
      get_daily <- TRUE
    }
    
    if (get_daily) {
      new_realtime <- daily[daily$datetime >= start & daily$datetime <= end , c("datetime", "value")]
      if (nrow(new_realtime) > 0) {
        realtime <- rbind(realtime, new_realtime)
      }
    }
  }
  # Find out where values need to be filled in with daily means
  if (length(dates) > 0) {
    for (i in 1:length(dates)) {
      toDate <- as.Date(dates[i]) # convert to plain date to check if there are any datetimes with that plain date in the data.frame
      if (!(toDate %in% as.Date(realtime$datetime))) {
        row <- daily[daily$datetime == dates[i] , c("datetime", "value")]
        realtime <- rbind(realtime, row)
      }
    }
  }
  if (nrow(realtime) == 0) {
    stop("There is no data to plot within this range of years and days. If you're wanting a plot overlaping the new year, remember that the last year requested should be the *December* year.")
  }
  
  # Add the ribbon values ######################
  # Make the ribbon sequence
  ribbon_yr <- lubridate::year(min((max(daily$datetime) - 24*60*60), (daily_end - 24*60*60))) #Daily was queried as one day longer than the day sequence earlier... this reverses that one day extra, but also finds out if the actual data extracted doesn't go that far back
  if (overlaps) {
    if (historic_range == "all") {
      ribbon_seq <- seq.POSIXt(as.POSIXct(paste0(ribbon_yr - 1, substr(startDay, 5, 16)), tz = tzone), as.POSIXct(paste0(ribbon_yr, substr(endDay, 5, 16)), tz = tzone), by = "day")
    } else {
      ribbon_seq <- seq.POSIXt(as.POSIXct(paste0(last_year, substr(startDay, 5, 16)), tz = tzone), as.POSIXct(paste0(last_year + 1, substr(endDay, 5, 16)), tz = tzone), by = "day")
    }
  } else {
    if (historic_range == "all") {
      ribbon_seq <- seq.POSIXt(as.POSIXct(paste0(ribbon_yr, substr(startDay, 5, 16)), tz = tzone), as.POSIXct(paste0(ribbon_yr, substr(endDay, 5, 16)), tz = tzone), by = "day")
    } else {
      ribbon_seq <- seq.POSIXt(as.POSIXct(paste0(last_year, substr(startDay, 5, 16)), tz = tzone), as.POSIXct(paste0(last_year, substr(endDay, 5, 16)), tz = tzone), by = "day")
    }
  }
  
  ribbon <- data.frame()
  ribbon_start_end <- if (overlaps) paste0(lubridate::year(min(daily$datetime)), "-", lubridate::year(min(daily$datetime)) + 1 ) else lubridate::year(min(daily$datetime))
  for (i in 1:length(ribbon_seq)) {
    target_date <- ribbon_seq[i] + 12*60*60
    plot_date <- day_seq[i]
    if (is.na(plot_date)) {
      plot_date <- day_seq[i - 1] + 60*60*24 - 1
    }
    if (!(lubridate::month(target_date) == 2 & lubridate::day(target_date) == 29)) { #Can't have the Feb 29 date because there is no Feb 29 ribbon
      row <- daily[daily$datetime == target_date, ]
      if (nrow(row) == 0) {
        lubridate::year(target_date) <- lubridate::year(target_date) - 1
        if (is.na(target_date)) {
          next()
        }
        row <- daily[daily$datetime == target_date, ]
      }
      lubridate::year(row$datetime) <- lubridate::year(plot_date)
      if (i == length(ribbon_seq)) {
        row$datetime <- row$datetime - 1 #This keeps the last ribbon point within the same days as the day sequence requested. Without this, a last day requested of 365 causes a point to show up in the following year.
        ribbon_start_end <- if (overlaps) c(ribbon_start_end, paste0(lubridate::year(target_date) - 2, "-", lubridate::year(target_date) - 1)) else c(ribbon_start_end, lubridate::year(target_date) - 1)
      }
      if (nrow(row) > 0) {
        ribbon <- rbind(ribbon, row)
      }
    }
  }
  if (nrow(ribbon) > 0) {
    if (min(ribbon$datetime) < min(realtime$datetime)) {
      first_date <- min(realtime$datetime)
      lubridate::hour(first_date) <- 0
      ribbon[ribbon$datetime == min(ribbon$datetime), "datetime"] <- first_date
    }
  }
  
  attr(realtime$datetime, "tzone") <- tzone
  realtime$year <- lubridate::year(realtime$datetime) #year, month columns used for removing Feb 29 later
  realtime$month <- lubridate::month(realtime$datetime)
  realtime$day <- lubridate::day(realtime$datetime)
  realtime <- realtime[!(realtime$month == 2 & realtime$day == 29), ] #Remove Feb 29
  ribbon$year <- lubridate::year(ribbon$datetime)
  ribbon$month <- lubridate::month(ribbon$datetime)
  ribbon$day <- lubridate::month(ribbon$datetime)
  ribbon <- ribbon[!(ribbon$month == 2 & ribbon$day == 29), ] #Remove Feb 29
  
  if (overlaps) { # This section sorts out the overlap years, builds the plotting column
    temp <- data.frame(date = day_seq)
    temp$year = lubridate::year(temp$date)
    temp <- temp[!(temp$year == max(temp$year)), ] #Remove the rows for days after the new year
    temp$month = lubridate::month(temp$date)
    temp$day = lubridate::day(temp$date)
    temp$day <- stringr::str_pad(temp$day, 2, side = "left", pad = "0")
    
    #Column md is built in both temp and realtime dfs to be able to differentiate the previous year from the next and assign proper plot years (i.e. 2022-2023) and fake datetimes (since every year needs the same "fake year" to plot together)
    temp$md <- paste0(temp$month, temp$day)
    temp$md <- as.numeric(temp$md)
    md_sequence <- seq(min(temp$md), max(temp$md))
    
    realtime$day <- stringr::str_pad(realtime$day, 2, side = "left", pad = "0")
    realtime$md <- paste0(realtime$month, realtime$day)
    realtime$md <- as.numeric(realtime$md)
    
    realtime$fake_datetime <- as.POSIXct(rep(NA, nrow(realtime)))
    realtime$plot_year <- NA
    for (i in 1:nrow(realtime)) {  #!!!This desperately needs to be vectorized in some way. Super slow!
      fake_datetime <- gsub("[0-9]{4}", if (realtime$md[i] %in% md_sequence) last_year else last_year + 1, realtime$datetime[i])
      fake_datetime <- ifelse(nchar(fake_datetime) > 11, fake_datetime, paste0(fake_datetime, " 00:00:00"))
      realtime$fake_datetime[i] <- as.POSIXct(fake_datetime, tz = tzone)
      realtime$plot_year[i] <- if (realtime$md[i] %in% md_sequence) paste0(realtime$year[i], "-", realtime$year[i] + 1) else paste0(realtime$year[i] - 1, "-", realtime$year[i])
    }
  } else { #Does not overlap the new year
    realtime$plot_year <- as.character(realtime$year)
    realtime$fake_datetime <- gsub("[0-9]{4}", last_year, realtime$datetime)
    realtime$fake_datetime <- ifelse(nchar(realtime$fake_datetime) > 11, realtime$fake_datetime, paste0(realtime$fake_datetime, " 00:00:00"))
    realtime$fake_datetime <- as.POSIXct(realtime$fake_datetime, tz = tzone, format = '%Y-%m-%d %H:%M:%S') #Make fake datetimes to permit plotting years together as separate lines. This DOESN'T work if Feb 29 isn't removed first!
  }
  
  # apply datum correction where necessary
  if (datum$conversion_m > 0) {
    ribbon[ , c("max", "min", "q75", "q25")] <- apply(ribbon[ , c("max", "min", "q75", "q25")], 2, function(x) x + datum$conversion_m)
    realtime$value <- realtime$value + datum$conversion_m
  }
  
  if (!is.null(filter)) {
    if (!inherits(filter, "numeric")) {
      message("Parameter 'filter' was modified from the default NULL but not properly specified as a class 'numeric'. Filtering will not be done.")
    } else {
      if (parameter %in% c("water level", "niveau d'eau", "flow", "d\u00E9bit d'eau", "snow depth", "profondeur de la neige", "snow water equivalent", "\u00E9quivalent en eau de la neige", "distance") | grepl("precipitation", parameter, ignore.case = TRUE)) { #remove all values less than 0
        realtime[realtime$value < 0 & !is.na(realtime$value),"value"] <- NA
      } else { #remove all values less than -100 (in case of negative temperatures or -DL values in lab results)
        realtime[realtime$value < -100 & !is.na(realtime$value),"value"] <- NA
      }
      
      rollmedian <- zoo::rollapply(realtime$value, width = filter, FUN = "median", align = "center", fill = "extend", na.rm = TRUE)
      rollmad <- zoo::rollapply(realtime$value, width = filter, FUN = "mad", align = "center", fill = "extend", na.rm = TRUE)
      outlier <- abs(realtime$value - rollmedian) > 5 * rollmad
      realtime$value[outlier] <- NA
    }
  }
  
  # Order realtime
  realtime <- realtime[order(realtime$datetime, decreasing = TRUE), ]
  
  #### ----------------------------- Make the plot -------------------------- ####
  
  if (is.null(invert)) {
    if (parameter_tbl$plot_default_y_orientation[1] == "inverted") {
      invert <- TRUE
    } else {
      invert <- FALSE
    }
  }
  
  # Create basic plot with historic range
  
  plot <- plotly::plot_ly() %>%
    plotly::add_ribbons(data = ribbon[!is.na(ribbon$q25) & !is.na(ribbon$q75), ], x = ~datetime, ymin = ~q25, ymax = ~q75, name = if (lang == "en") "IQR" else "EIQ", color = I("#5f9da6"), line = list(width = 0.2), hoverinfo = "text", text = ~paste0("q25: ", round(q25, 2), ", q75: ", round(q75, 2), " (", as.Date(datetime), ")")) %>%
    plotly::add_ribbons(data = ribbon[!is.na(ribbon$min) & !is.na(ribbon$max), ], x = ~datetime, ymin = ~min, ymax = ~max, name = "Min-Max", color = I("#D4ECEF"), line = list(width = 0.2), hoverinfo = "text", text = ~paste0("Min: ", round(min, 2), ", Max: ", round(max, 2), " (", as.Date(datetime), ")"))
  
  # Add traces
  col_idx <- 1
  
  colors <- grDevices::colorRampPalette(c("#00454e", "#7A9A01", "#FFA900", "#DC4405"))(length(unique(realtime$plot_year)))
  for (i in unique(realtime$plot_year)) {
    plot <- plotly::add_trace(plot,
                              data = realtime[realtime$plot_year == i, ],
                              x = ~fake_datetime,
                              y = ~value,
                              type = "scatter",
                              mode = "lines",
                              line = list(width = 2.5 * line_scale),
                              name = i,
                              color = I(colors[col_idx]),
                              hoverinfo = "text", 
                              text = ~paste0(plot_year, ": ", round(value, 2), " (", datetime, ")"))
    col_idx <- col_idx + 1
  }
  
  
  # Layout elements
  if (title) {
      if (lang == "fr") {
        stn_name <- DBI::dbGetQuery(con, paste0("SELECT name_fr FROM locations where location = '", location, "'"))[1,1]
      } 
      if (lang == "en" || is.na(stn_name)) {
        stn_name <- DBI::dbGetQuery(con, paste0("SELECT name FROM locations where location = '", location, "'"))[1,1]
      }
      stn_name <- titleCase(stn_name, lang)
    }
  
  plot <- plot %>%
    plotly::layout(
      title = if (title) list(text = stn_name, 
                              x = 0.05, 
                              xref = "container",
                              font = list(size = axis_scale * 18))
      else NULL, 
      xaxis = list(title = list(standoff = 0), 
                   showgrid = gridx, 
                   showline = TRUE, 
                   tickformat = if (lang == "en") "%b %d" else "%d %b",
                   titlefont = list(size = axis_scale * 14),
                   tickfont = list(size = axis_scale * 12),
                   rangeslider = list(visible = if (slider & legend_position == "v") TRUE else FALSE)), 
      yaxis = list(title = paste0(parameter_name, " (", units, ")"), 
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
      legend = list(font = list(size = legend_scale * 12))
    ) %>%
    plotly::config(locale = lang)
  
  return(plot)
}
