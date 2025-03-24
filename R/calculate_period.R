#' Calculate periodicity of data and add a column
#'
#' Utility function to calculate periodicity of continuous-type temporal data and prepare a column named 'period' with ISO8601 formatted periods. Will identify changes to periodicity within data, for example moving from 1-hour intervals to 6-hour intervals.
#'
#' @param data The data.frame or data.table for which to calculate periodicity. Must contain at minimum a column named 'datetime' (in POSIXct format) with no missing values.
#' @param datetime_col The name of the column in `data` containing the datetime values. Default is 'datetime'.
#' @param timeseries_id The ID of the timeseries for which to calculate periodicity. Used to fetch any data points lacking a period, as well as to search for additional data points if there are too few to calculate a period in the provided `data`. Leave NULL if there is no database entry for this.
#' @param con A connection to the database, created with [DBI::dbConnect()] or using the utility function [AquaConnect()]. Not used if timeseries_id is left NULL.
#'
#' @return A data.frame or data.table with calculated periods as ISO8601 formatted strings in a column named 'period'.
#' @export

calculate_period <- function(data, datetime_col = "datetime", timeseries_id = NULL, con = AquaConnect(silent = TRUE))
{
  names <- names(data)
  data <- data[!is.na(data[[datetime_col]]),] #Remove rows with missing datetime values
  data <- data[order(data[[datetime_col]]) ,] #Sort ascending
  diffs <- as.numeric(diff(data[[datetime_col]]), units = "hours")
  smoothed_diffs <- zoo::rollmedian(diffs, k = 3, fill = NA, align = "center")
  # Initialize variables to track changes
  consecutive_count <- 0
  changes <- data.frame()
  last_diff <- 0
  if (!is.na(smoothed_diffs)) {
    if (length(smoothed_diffs) > 0) {
      for (j in 1:length(smoothed_diffs)) {
        if (!is.na(smoothed_diffs[j]) && smoothed_diffs[j] < 25 && smoothed_diffs[j] != last_diff) { # Check if smoothed interval is less than threshold, which is set to more than a whole day (greatest interval possible is 24 hours) as well as not the same as the last recorded diff
          consecutive_count <- consecutive_count + 1
          if (consecutive_count == 3) { # At three consecutive new measurements it's starting to look like a pattern
            last_diff <- smoothed_diffs[j]
            change <- data.frame(datetime = data[[datetime_col]][j - 3],
                                 period = last_diff)
            changes <- rbind(changes, change)
            consecutive_count <- 0
          }
        } else {
          consecutive_count <- 0
        }
      }
    }
  }

  
  # Calculate the duration in days, hours, minutes, and seconds and assign to the right location in data
  if (nrow(changes) > 0) {
    for (j in 1:nrow(changes)) {
      days <- floor(changes$period[j] / 24)
      remaining_hours <- changes$period[j] %% 24
      minutes <- floor((remaining_hours - floor(remaining_hours)) * 60)
      seconds <- round(((remaining_hours - floor(remaining_hours)) * 60 - minutes) * 60)
      data[data[[datetime_col]] == changes$datetime[j], "period"] <- paste("P", days, "DT", floor(remaining_hours), "H", minutes, "M", seconds, "S", sep = "")
    }
    #carry non-na's forward and backwards, if applicable
    data$period <- zoo::na.locf(zoo::na.locf(data$period, na.rm = FALSE), fromLast = TRUE)
    
  } else { #In this case there were too few measurements to conclusively determine a period so pull a few from the DB and redo the calculation
    if (is.null(timeseries_id)) {
      stop("There are too few measurements to conclusively determine a period, and no timeseries_id was provided to fetch more data.")
    }
    # Check if user can access the measurements_continuous table
    if (!DBI::dbExistsTable(con, "measurements_continuous")) {
      tbl <- 'measurements_continuous_corrected'
      # for this table, names == 'value' must be changed to 'value_corrected AS value'
      names[names == 'value'] <- 'value_corrected AS value'
    } else {
      tbl <- 'measurements_continuous'
    }
    old_tz <- attr(data$datetime, "tzone")
    attr(data$datetime, "tzone") <- "UTC"
    no_period <- dbGetQueryDT(con, paste0("SELECT ", paste(names, collapse = ', '), " FROM ", tbl, " WHERE timeseries_id = ", timeseries_id, " AND datetime NOT IN ('", paste(data$datetime, collapse = "', '"), "') ORDER BY datetime DESC LIMIT 10;"))
    no_period$period <- NA
    if (!"period" %in% names(data)) {
      data$period <- NA
    }
    data <- rbind(data, no_period)
    attr(data$datetime, "tzone") <- old_tz
    data <- data[order(data[[datetime_col]]), ]
    diffs <- as.numeric(diff(data[[datetime_col]]), units = "hours")
    smoothed_diffs <- zoo::rollmedian(diffs, k = 3, fill = NA, align = "center")
    consecutive_count <- 0
    changes <- data.frame()
    last_diff <- 0
    if (length(smoothed_diffs) > 0) {
      for (j in 1:length(smoothed_diffs)) {
        if (!is.na(smoothed_diffs[j]) && smoothed_diffs[j] < 25 && smoothed_diffs[j] != last_diff) {
          consecutive_count <- consecutive_count + 1
          if (consecutive_count == 3) {
            last_diff <- smoothed_diffs[j]
            change <- data.frame(datetime = data[[datetime_col]][j - 3],
                                 period = last_diff)
            changes <- rbind(changes, change)
            consecutive_count <- 0
          }
        } else {
          consecutive_count <- 0
        }
      }
    }
    if (nrow(changes) > 0) {
      for (k in 1:nrow(changes)) {
        days <- floor(changes$period[k] / 24)
        remaining_hours <- changes$period[k] %% 24
        minutes <- floor((remaining_hours - floor(remaining_hours)) * 60)
        seconds <- round(((remaining_hours - floor(remaining_hours)) * 60 - minutes) * 60)
        data[data[[datetime_col]] == changes[[datetime_col]][k], "period"] <- paste("P", days, "DT", floor(remaining_hours), "H", minutes, "M", seconds, "S", sep = "")
      }
      #carry non-na's forward and backwards, if applicable
      data$period <- zoo::na.locf(zoo::na.locf(data$period, na.rm = FALSE), fromLast = TRUE)
    } else {
      data$period <- NULL
      warning("There are too few measurements to conclusively determine a period.")
    }
  }
  return(data)
} # End of function
