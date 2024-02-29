#' Plot a continous timeseries from the hydromet database
#'
#' @param location The location for which you want a plot.
#' @param parameter The parameter you wish to plot. The location:parameter combo must be in the local database.
#' @param record_rate The recording rate for the parameter and location to plot. In most cases there are not multiple recording rates for a location and parameter combo and you can leave this NULL. Otherwise NULL will default to the most frequent record rate, or set this as one of '< 1 day', '1 day', '1 week', '4 weeks', '1 month', 'year'.
#' @param start_date The day on which to start the plot as character, Date, or POSIXct. Default is 1970-01-01 but the plot is zoomable.
#' @param end_date The day on which to end the plot as character, Date, or POSIXct.. Default is today.
#' @param title Should a title be included?
#' @param custom_title Custom title to be given to the plot. Default is NULL, which will set the title as Location <<location id>>: <<location name>>. Ex: Location 09AB004: Marsh Lake Near Whitehorse.
#' @param filter Should an attempt be made to filter out spurious data? Will calculate the rolling IQR and filter out clearly spurious values. Set this parameter to an integer, which specifies the rolling IQR 'window'. The greater the window, the more effective the filter but at the risk of filtering out real data. Negative values are always filtered from parameters "level", "flow", "snow depth", "SWE", "distance", and any "precip" related parameter. Otherwise all values below -100 are removed.
#' @param historic_range Should the historic range be plotted? Default is TRUE.
#' @param returns Should returns be plotted? You have the option of using pre-determined level returns only (option "table"), auto-calculated values (option "calculate"), "auto" (priority to "table", fallback to "calculate"), or "none". Defaults to "auto".
#' @param return_type Use minimum ("min") or maximum ("max") values for returns?
#' @param return_months Numeric vector of months during which to look for minimum or maximum values. Only works with calculated returns. Does not have to be within `startDay` and `endDay`, but will only consider data up to the last year specified in `years`. For months overlapping the new year like November-April, should look like c(11:12,1:4). IMPORTANT: the first month in the range should be the first element of the vector: c(1:4, 11:12) would not be acceptable. Think of it as defining a season. Passed to 'months' argument of [fasstr::calc_annual_extremes()] and also used to set the 'water_year_start' parameter of this function.
#' @param return_max_year The last year of data to consider when calculating returns. If left NULL behavior depends on parameter `historic_range`: if `historic_range` is set to 'last' defaults to the last year in `year` otherwise uses all data available. Automatically set to max(years) if `historic_range` is 'last', otherwise set to the current year.
#' @param allowed_missing Allowable % of data missing during the months specified in 'return_months' to still retain the year for analysis when calculating returns. Passed to 'allowed_missing' argument of [fasstr::calc_annual_extremes()].
#' @param plot_scale Adjusts/scales the size of plot text elements. 1 = standard size, 0.5 = half size, 2 = double the size, etc. Standard size works well in a typical RStudio environment.
#' @param legend Should a legend (including text for min/max range and return periods) be added to the plot?
#' @param save_path Default is NULL and the graph will be visible in RStudio and can be assigned to an object. Option "choose" brings up the File Explorer for you to choose where to save the file, or you can also specify a save path directly.
#' @param con A connection to the database. Default uses function [hydrometConnect()].
#'
#' @return
#' @export

location <- "09EB001"
parameter = "water level"
record_rate = NULL
start_date = "1970-01-01"
end_date = Sys.Date()
datum = TRUE
title = TRUE
custom_title = NULL
filter = 20
historic_range = TRUE
returns = "auto"
return_type = "max"
return_months = c(5:9)
return_max_year = NULL
allowed_missing = 10
plot_scale = 1
legend = TRUE
save_path = NULL
con = hydrometConnect(silent = TRUE)



plotTimeseries <- function(location,
                           parameter,
                           record_rate = NULL,
                           start_date = "1970-01-01",
                           end_date = Sys.Date(),
                           datum = TRUE,
                           title = TRUE,
                           custom_title = NULL,
                           filter = NULL,
                           historic_range = FALSE,
                           returns = "none",
                           return_type = "max",
                           return_months = c(5:9),
                           return_max_year = NULL,
                           allowed_missing = 10,
                           plot_scale = 1,
                           legend = TRUE,
                           save_path = NULL,
                           con = hydrometConnect(silent = TRUE)) {
  
  # Checks and initial work ##########################################
  #Suppress warnings otherwise ggplot annoyingly flags every geom that wasn't plotted
  options(warn = -1)
  old_warn <- getOption("warn")
  on.exit(options(warn = old_warn))
  
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
  
  if (parameter != "SWE") {
    parameter <- tolower(parameter)
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
  
  # Select save path
  if (!is.null(save_path)) {
    if (save_path %in% c("Choose", "choose")) {
      # print("Select the folder where you want this graph saved.")
      save_path <- rstudioapi::selectDirectory(caption = "Select Save Folder", path = file.path(Sys.getenv("USERPROFILE"), "Desktop"))
    } else {
      if (!dir.exists(save_path)) {
        stop("The directory you pointed to with parameter 'save_path' does not exist")
      }
    }
  }
  
  if (!inherits(historic_range, "logical")) {
    warning("Parameter `historic_range` must be TRUE or FALSE. Resetting it to FALSE.")
    historic_range <- FALSE
  }
  
  if (is.null(return_max_year)) {
    if (historic_range == "last") {
      return_max_year <- max(lubridate::year(end_date))
    } else {
      return_max_year <- lubridate::year(Sys.Date())
    }
  } else {
    if (return_max_year > max(lubridate::year(end_date))) {
      return_max_year <- max(lubridate::year(end_date))
      message("Your parameter entry for 'return_max_year' is invalid (greater than the last year to plot). It has been adjusted to the last year to plot. See the help file for other options.")
    }
  }
  
  # Determine the timseries and adjust the date range #################
  location_id <- DBI::dbGetQuery(con, paste0("SELECT location_id FROM locations WHERE location = '", location, "';"))[1,1]
  #Confirm parameter and location exist in the database and that there is only one entry
  parameter_code <- DBI::dbGetQuery(con, paste0("SELECT param_code FROM parameters WHERE param_name = '", parameter, "';"))[1,1]
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
  name <- DBI::dbGetQuery(con, paste0("SELECT name FROM locations WHERE location_id = ", location_id, ";"))[1,1]
  
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
    plot_data <- DBI::dbGetQuery(con, paste0("SELECT date, value FROM calculated_daily WHERE timeseries_id = ", tsid, " AND datetime BETWEEN '", start_date, "' AND '", end_date, "';"))
  }
  
  if (datum != 0) {
    plot_data$value <- plot_data$value + datum$conversion_m
    plot_data$min <- plot_data$min + datum$conversion_m
    plot_data$max <- plot_data$max + datum$conversion_m
    plot_data$q25 <- plot_data$q25 + datum$conversion_m
    plot_data$q50 <- plot_data$q50 + datum$conversion_m
    plot_data$q75 <- plot_data$q75 + datum$conversion_m
  }
  plot_data <- plot_data[order(plot_data$date),]
  
  # Make the plot ###################################
  plotly::plot_ly(plot_data) %>%
    plotly::add_ribbons(x = ~date, ymin = ~q25, ymax = ~q75, name = "IQR", color = I("grey40")) %>%
    plotly::add_ribbons(x = ~date, ymin = ~min, ymax = ~max, name = "Min-Max", color = I("grey80")) %>%
    plotly::layout(title = stringr::str_to_title(name), xaxis = list(title = "Date"), yaxis = list(title = paste0(if (parameter != "SWE") stringr::str_to_title(parameter) else parameter, " (", units, ")"))) %>%
  plotly::add_lines(x = ~date, y = ~value, type = "scatter", mode = "lines", name = if (parameter != "SWE") stringr::str_to_title(parameter) else parameter, color = I("#0097A9"))
  
  # Add returns if requested ########################
  # If returns != "table" or if returns = "calculate" or if "auto" and no data is available in the database extrema table, get the missing data from range_data (which might have 0 rows) and add.
  
  # Save the plot if requested ######################
}
