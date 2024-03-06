#' Plot a continous timeseries from the hydromet database
#'
#' @description
#' `r lifecycle::badge('experimental')`
#' 
#' This function will plot a continuous timeseries from the hydromet database. The plot is zoomable, and hovering over the historical ranges or the measured values brings up additional information. The function will also plot the historic range and returns if requested.
#' 
#' @param location The location for which you want a plot.
#' @param parameter The parameter you wish to plot. The location:parameter combo must be in the local database.
#' @param record_rate The recording rate for the parameter and location to plot. In most cases there are not multiple recording rates for a location and parameter combo and you can leave this NULL. Otherwise NULL will default to the most frequent record rate, or set this as one of '< 1 day', '1 day', '1 week', '4 weeks', '1 month', 'year'.
#' @param start_date The day on which to start the plot as character, Date, or POSIXct. Default is 1970-01-01 but the plot is zoomable.
#' @param end_date The day on which to end the plot as character, Date, or POSIXct. Default is today.
#' @param slider Should a slider be included to show where you are zoomed in to? If TRUE the slider will be included but this prevents horizontal zooming or zooming in using the box tool.
#' @param datum Should a vertical offset be applied to the data? Looks for it in the database and applies it if it exists. Default is TRUE.
#' @param title Should a title be included?
#' @param custom_title Custom title to be given to the plot. Default is NULL, which will set the title as the location name as entered in the database.
#' @param filter Should an attempt be made to filter out spurious data? Will calculate the rolling IQR and filter out clearly spurious values. Set this parameter to an integer, which specifies the rolling IQR 'window'. The greater the window, the more effective the filter but at the risk of filtering out real data. Negative values are always filtered from parameters "level", "flow", "snow depth", "SWE", "distance", and any "precip" related parameter. Otherwise all values below -100 are removed.
#' @param historic_range Should the historic range be plotted? Default is TRUE.
#' @param returns Should returns be plotted? You have the option of using pre-determined level returns only (option "table"), auto-calculated values (option "calculate"), "auto" (priority to "table", fallback to "calculate"), or "none". Defaults to "auto".
#' @param return_type Use minimum ("min") or maximum ("max") values for returns?
#' @param return_months Numeric vector of months during which to look for minimum or maximum values. Only works with calculated returns. Does not have to be within `startDay` and `endDay`, but will only consider data up to the last year specified in `years`. For months overlapping the new year like November-April, should look like c(11:12,1:4). IMPORTANT: the first month in the range should be the first element of the vector: c(1:4, 11:12) would not be acceptable. Think of it as defining a season. Passed to 'months' argument of [fasstr::calc_annual_extremes()] and also used to set the 'water_year_start' parameter of this function.
#' @param return_max_year The last year of data to consider when calculating returns. NULL defaults to the year of end_date.
#' @param allowed_missing Allowable % of data missing during the months specified in 'return_months' to still retain the year for analysis when calculating returns. Passed to 'allowed_missing' argument of [fasstr::calc_annual_extremes()].
#' @param language The language to use for the plot. Currently only "en" and "fr" are supported. Default is "en".

#' @param con A connection to the database. Default uses function [hydrometConnect()].
#'
#' @return A plotly object
#' @export

plotTimeseries <- function(location,
                           parameter,
                           record_rate = NULL,
                           start_date = "1970-01-01",
                           end_date = Sys.Date(),
                           slider = TRUE,
                           datum = TRUE,
                           title = TRUE,
                           custom_title = NULL,
                           filter = NULL,
                           historic_range = TRUE,
                           returns = "none",
                           return_type = "max",
                           return_months = c(5:9),
                           return_max_year = NULL,
                           allowed_missing = 10,
                           language = "en",
                           con = hydrometConnect(silent = TRUE)) {
  
  # Checks and initial work ##########################################
  #Suppress warnings otherwise ggplot annoyingly flags every geom that wasn't plotted
  options(warn = -1)
  old_warn <- getOption("warn")
  on.exit(options(warn = old_warn))
  
  rlang::check_installed("plotly", "necessary for interactive plots")
  
  if (inherits(start_date, "character")) {
    start_date <- as.Date(start_date)
  }
  if (inherits(start_date, "Date")) {
    start_date <- as.POSIXct(start_date)
  }
  if (inherits(end_date, "character")) {
    end_date <- as.Date(end_date)
  }
  if (inherits(end_date, "Date")) {
    end_date <- as.POSIXct(end_date)
  }
  
  if (!(parameter %in% c("EEN", "SWE"))) {
    parameter <- tolower(parameter)
  }
  
  if (!(language %in% c("en", "fr"))) {
    stop("Your entry for the parameter 'language' is invalid. Please review the function documentation and try again.")
  }
  
  if (!is.null(record_rate)) {
    if (!(record_rate %in% c('< 1 day', '1 day', '1 week', '4 weeks', '1 month', 'year'))) {
      warning("Your entry for parameter record_rate is invalid. It's been reset to the default NULL.")
      record_rate <- NULL
    }
  }
  
  return_type <- tolower(return_type)
  returns <- tolower(returns)
  if (!returns %in% c("table", "auto", "calculate", "none")) {
    stop("Your entry for the parameter 'return' is invalid. Please review the function documentation and try again.")
  }
  
  if (!inherits(historic_range, "logical")) {
    warning("Parameter `historic_range` must be TRUE or FALSE. Resetting it to FALSE.")
    historic_range <- FALSE
  }
  
  if (is.null(return_max_year)) {
    return_max_year <- lubridate::year(end_date)
  } else if (!inherits(return_max_year, "numeric")) {
    warning("Your entry for parameter return_max_year is invalid. It's been adjusted to the last year plotted.")
    return_max_year <- lubridate::year(end_date)
  }
  
  # Determine the timseries and adjust the date range #################
  location_id <- DBI::dbGetQuery(con, paste0("SELECT location_id FROM locations WHERE location = '", location, "';"))[1,1]
  #Confirm parameter and location exist in the database and that there is only one entry
  escaped_parameter <- gsub("'", "''", parameter)
  parameter_tbl <- DBI::dbGetQuery(con, paste0("SELECT param_code, param_name, param_name_fr FROM parameters WHERE param_name = '", escaped_parameter, "' OR param_name_fr = '", escaped_parameter, "';"))
  parameter_code <- parameter_tbl$param_code[1]
  if (language == "fr") {
    parameter_name <- titleCase(parameter_tbl$param_name_fr[1], "fr")
  } else if (language == "en" || is.na(parameter_name)) {
    parameter_name <- titleCase(parameter_tbl$param_name[1], "en")
  }
  if (is.na(parameter_code)) {
    stop("The parameter you entered does not exist in the database.")
  }
  if (is.null(record_rate)) {
    exist_check <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id, record_rate, start_datetime, end_datetime, unit FROM timeseries WHERE location_id = ", location_id, " AND parameter = ", parameter_code, " AND category = 'continuous' AND period_type = 'instantaneous';"))
  } else {
    exist_check <- DBI::dbGetQuery(con, paste0("SELECT timeseries_id, start_datetime, end_datetime, unit FROM timeseries WHERE location_id = ", location_id, " AND parameter = ", parameter_code, " AND category = 'continuous' AND period_type = 'instantaneous' AND record_rate = '", record_rate, "';"))
  }
  if (nrow(exist_check) == 0) {
    if (is.null(record_rate)) {
      stop("There doesn't appear to be a match in the database for location ", location, ", parameter ", parameter, ", and continuous category data.")
    } else {
      stop("There doesn't appear to be a match in the database for location ", location, ", parameter ", parameter, ", record rate ", record_rate, " and continuous category data You could try leaving the record rate to the default 'null'.")
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
  
  if (title == TRUE) {
    if (is.null(custom_title)) {
      if (language == "fr") {
        stn_name <- DBI::dbGetQuery(con, paste0("SELECT name_fr FROM locations where location = '", location, "'"))[1,1]
      } 
      if (language == "en" || is.na(stn_name) == TRUE) {
        stn_name <- DBI::dbGetQuery(con, paste0("SELECT name FROM locations where location = '", location, "'"))[1,1]
      }
      stn_name <- titleCase(stn_name, language)
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
  
  # Find the necessary datum (latest datum)
  if (datum & parameter %in% c("water level", "distance")) {
    datum <- DBI::dbGetQuery(con, paste0("SELECT conversion_m FROM datum_conversions WHERE location_id = ", location_id, " AND current = TRUE"))
  } else {
    datum <- data.frame(conversion_m = 0)
  }
  
  # Find the ts units
  units <- exist_check$unit
  
  # Get the data ####################################
  
  # Code below might not be used as it's very time consuming. It's here for reference.
  # if (historic_range) { # get data from the calculated_daily table for historic ranges plus values from measurements_continuous. Where there isn't any data in measurements_continuous fill in with the value from the daily table.
  #   range_end <- end_date + 1*24*60*60
  #   range_start <- start_date - 1*24*60*60
  #   range_data <- DBI::dbGetQuery(con, paste0("SELECT date AS datetime, min, max, q75, q50, q25  FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date BETWEEN '", range_start, "' AND '", range_end, "';"))
  #   range_data$datetime <- as.POSIXct(range_data$datetime)
  #   trace_data <- DBI::dbGetQuery(con, paste0("SELECT datetime, value FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime BETWEEN '", start_date, "' AND '", end_date, "';"))
  #   # Find the most common interval between points in trace_data and fill gaps with NA values
  #   if (nrow(trace_data) > 1) {
  #     interval <- as.numeric(stats::median(diff(trace_data$datetime))) * 60
  #     all_times <- seq.POSIXt(from = min(trace_data$datetime), to = max(trace_data$datetime), by = interval)
  #     trace_data <- merge(trace_data, data.frame(datetime = all_times), by = "datetime", all = TRUE)
  #   }
  # } else {
  #   range_data <- data.frame()
  #   trace_data <- DBI::dbGetQuery(con, paste0("SELECT datetime, value FROM measurements_continuous WHERE timeseries_id = ", tsid, " AND datetime BETWEEN '", start_date, "' AND '", end_date, "';"))
  # }
  # 
  # # Find out where values need to be filled in with daily means
  # if (as.Date(min(trace_data$datetime)) > start_date) {
  #   extra <- DBI::dbGetQuery(con, paste0("SELECT date AS datetime, value FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date < '", min(trace_data$datetime), "' AND date >= '", start_date, "';"))
  #   extra$datetime <- as.POSIXct(extra$datetime)
  #   trace_data <- rbind(extra, trace_data)
  # }
  
  if (historic_range) { # get data from the calculated_daily table for historic ranges plus values from measurements_continuous. Where there isn't any data in measurements_continuous fill in with the value from the daily table.
    plot_data <- DBI::dbGetQuery(con, paste0("SELECT date, value, min, max, q75, q50, q25  FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date BETWEEN '", start_date, "' AND '", end_date, "';"))
  } else {
    plot_data <- DBI::dbGetQuery(con, paste0("SELECT date, value FROM calculated_daily WHERE timeseries_id = ", tsid, " AND date BETWEEN '", start_date, "' AND '", end_date, "';"))
  }
  if (nrow(plot_data) == 0) {
    stop("No data found for the specified location, parameter, and time range.")
  }
  
  if (datum != 0) {
    plot_data$value <- plot_data$value + datum$conversion_m
    if (historic_range) {
      plot_data$min <- plot_data$min + datum$conversion_m
      plot_data$max <- plot_data$max + datum$conversion_m
      plot_data$q25 <- plot_data$q25 + datum$conversion_m
      plot_data$q50 <- plot_data$q50 + datum$conversion_m
      plot_data$q75 <- plot_data$q75 + datum$conversion_m
    }
  }
  plot_data <- plot_data[order(plot_data$date),]
  
  if (!is.null(filter)) { # Use the same approach as in plotOverlap to filter the value column
    if (!inherits(filter, "numeric")) {
      message("Parameter 'filter' was modified from the default NULL but not properly specified as a class 'numeric'. Filtering will not be done.")
    } else {
      if (parameter %in% c("water level", "niveau d'eau", "water flow", "d\u00E9bit d'eau", "snow depth", "profondeur de la neige", "SWE", "EEN", "distance") | grepl("precip", parameter, ignore.case = TRUE)) { #remove all values less than 0
        plot_data[plot_data$value < 0 & !is.na(plot_data$value),"value"] <- NA
      } else { #remove all values less than -100 (in case of negative temperatures or -DL values in lab results)
        plot_data[plot_data$value < -100 & !is.na(plot_data$value),"value"] <- NA
      }
      
      rollmedian <- zoo::rollapply(plot_data$value, width = filter, FUN = "median", align = "center", fill = "extend", na.rm = TRUE)
      rollmad <- zoo::rollapply(plot_data$value, width = filter, FUN = "mad", align = "center", fill = "extend", na.rm = TRUE)
      outlier <- abs(plot_data$value - rollmedian) > 5 * rollmad
      plot_data$value[outlier] <- NA
    }
  }
  
  # Make the plot ###################################
  plot <- plotly::plot_ly(plot_data)
  if (historic_range) {
    plot <- plot %>%
    plotly::add_ribbons(x = ~date, ymin = ~q25, ymax = ~q75, name = if (language == "en") "IQR" else "EIQ", color = I("grey40"), line = list(width = 0.2)) %>%
    plotly::add_ribbons(x = ~date, ymin = ~min, ymax = ~max, name = "Min-Max", color = I("grey80"), line = list(width = 0.2)) 
    }
    plot <- plot %>%
      plotly::layout(title = list(text = stn_name, x = 0.05, xref = "container"), xaxis = list(title = list(standoff = 0, font = list(size = 1)), showgrid = FALSE, showline = TRUE, tickformat = if (language == "en") "%b %d" else "%d %b", rangeslider = list(visible = if (slider) TRUE else FALSE)), yaxis = list(title = paste0(parameter_name, " (", units, ")"), showgrid = FALSE, showline = TRUE), margin = list(b = 0)) %>%
  plotly::add_lines(x = ~date, y = ~value, type = "scatter", mode = "lines", name = parameter_name, color = I("#0097A9")) %>%
      plotly::config(locale = language)
  
  # Add returns if requested ########################
  # If returns != "table" or if returns = "calculate" or if "auto" and no data is available in the database extrema table, get the missing data from range_data (which might have 0 rows) and add.
  if (returns != "none") {
    message("Returns option is not yet implemented")
  }
  
  return(plot)
}
