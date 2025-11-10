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

calculate_period <- function(
  data,
  datetime_col = "datetime",
  timeseries_id = NULL,
  con = AquaConnect(silent = TRUE)
) {
  as_dt <- data.table::is.data.table(data)
  if (!as_dt) {
    data <- data.table::as.data.table(data)
  }

  names <- names(data)
  data <- data[!is.na(data[[datetime_col]]), ] # Remove rows with missing datetime values
  data <- data[order(data[[datetime_col]])] # Sort data by datetime
  diffs <- as.numeric(diff(data[[datetime_col]]), units = "hours")
  smoothed_diffs <- zoo::rollmedian(diffs, k = 3, fill = NA, align = "center")

  changes <- data.frame()

  if (!all(is.na(smoothed_diffs)) && length(smoothed_diffs) > 0) {
    diff_dt <- data.table::data.table(
      idx = seq_along(smoothed_diffs),
      diff = smoothed_diffs
    )
    diff_dt[, "run" := data.table::rleid(diff_dt$diff)]
    runs <- diff_dt[, .(diff = diff[1], start = .I[1], len = .N), by = "run"]
    runs <- runs[!is.na(runs$diff) & runs$diff < 25 & runs$len >= 3]
    if (nrow(runs) > 0) {
      changes <- data.frame(
        datetime = data[[datetime_col]][runs$start],
        period = runs$diff
      )
    }
  }

  # Calculate the duration in days, hours, minutes, and seconds and assign to the right location in data
  if (nrow(changes) > 0) {
    data[, "period" := NA_character_]
    data[
      match(changes$datetime, data[[datetime_col]]),
      "period" := iso_period(changes$period)
    ]
    data[,
      "period" := zoo::na.locf(
        zoo::na.locf(data$period, na.rm = FALSE),
        fromLast = TRUE
      )
    ]
  } else {
    #In this case there were too few measurements to conclusively determine a period so pull a few from the DB and redo the calculation
    if (is.null(timeseries_id)) {
      stop(
        "There are too few measurements to conclusively determine a period, and no timeseries_id was provided to fetch more data."
      )
    }
    # Check if user can access the measurements_continuous table
    if (!DBI::dbExistsTable(con, "measurements_continuous")) {
      tbl <- 'measurements_continuous_corrected'
      # for this table, names == 'value' must be changed to 'value_corrected AS value'
      names[names == 'value'] <- 'value_corrected AS value'
    } else {
      tbl <- 'measurements_continuous'
    }
    datetime_values <- data[[datetime_col]]
    old_tz <- attr(datetime_values, "tzone")
    formatted_datetimes <- format(datetime_values, tz = "UTC", usetz = FALSE)
    not_in_clause <- ""
    if (length(formatted_datetimes) > 0) {
      not_in_clause <- paste0(
        " AND datetime NOT IN ('",
        paste(formatted_datetimes, collapse = "', '"),
        "')"
      )
    }
    query <- paste0(
      "SELECT ",
      paste(names, collapse = ', '),
      " FROM ",
      tbl,
      " WHERE timeseries_id = ",
      timeseries_id,
      not_in_clause,
      " ORDER BY datetime DESC LIMIT 10;"
    )
    no_period <- dbGetQueryDT(con, query)
    no_period$period <- NA
    if (!"period" %in% names(data)) {
      data$period <- NA
    }
    data <- data.table::rbindlist(list(data, no_period), fill = TRUE)
    if (!inherits(data[[datetime_col]], "POSIXct")) {
      tz_to_use <- if (!is.null(old_tz)) old_tz else "UTC"
      data[, (datetime_col) := as.POSIXct(data[[datetime_col]], tz = tz_to_use)]
    }
    if (!is.null(old_tz)) {
      data.table::setattr(data[[datetime_col]], "tzone", old_tz)
    }
    data <- data[order(data[[datetime_col]]), ]
    diffs <- as.numeric(diff(data[[datetime_col]]), units = "hours")
    smoothed_diffs <- zoo::rollmedian(diffs, k = 3, fill = NA, align = "center")
    changes <- data.frame()

    if (length(smoothed_diffs) > 0) {
      diff_dt <- data.table::data.table(
        idx = seq_along(smoothed_diffs),
        diff = smoothed_diffs
      )
      diff_dt[, "run" := data.table::rleid(diff_dt$diff)]
      runs <- diff_dt[, .(diff = diff[1], start = .I[1], len = .N), by = "run"]
      runs <- runs[!is.na(runs$diff) & runs$diff < 25 & runs$len >= 3]
      if (nrow(runs) > 0) {
        changes <- data.frame(
          datetime = data[[datetime_col]][runs$start],
          period = runs$diff
        )
      }
    }
    if (nrow(changes) > 0) {
      data[, "period" := NA_character_]
      data[
        match(changes$datetime, data[[datetime_col]]),
        "period" := iso_period(changes$period)
      ]
      data[,
        "period" := zoo::na.locf(
          zoo::na.locf(data$period, na.rm = FALSE),
          fromLast = TRUE
        )
      ]
    } else {
      data[, "period" := NULL]
      warning(
        "There are too few measurements to conclusively determine a period."
      )
    }
  }
  result <- if (as_dt) data else as.data.frame(data)
  return(result)
} # End of function
