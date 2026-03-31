# Helpers for continuous trace plotting

#' @title Check if corrected source should be used for continuous trace
#' @description Determines whether the continuous trace should use the corrected source based on the presence of applicable corrections in the database.
#' @param con A DBI database connection object.
#' @param timeseries_id The ID of the timeseries being plotted.
#' @param start_date The start date of the plot range (POSIXct or character).
#' @param end_date The end date of the plot range (POSIXct or character).
#' @return TRUE if the corrected source should be used, FALSE otherwise.
#' @noRd
#' @keywords internal
continuous_trace_uses_corrected_source <- function(
  con,
  timeseries_id,
  start_date,
  end_date
) {
  if (!DBI::dbExistsTable(con, "measurements_continuous")) {
    return(TRUE)
  }

  corrections_apply <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT correction_id FROM corrections ",
      "WHERE timeseries_id = $1 AND start_dt <= $2 AND end_dt >= $3 ",
      "LIMIT 1;"
    ),
    params = list(timeseries_id, end_date, start_date)
  )

  nrow(corrections_apply) > 0
}

#' Normalize the as_of input to a POSIXct object in UTC timezone
#' @param as_of The input value for as_of, which can be NULL, character, Date, or POSIXct.
#' @param tzone The timezone to use for parsing character inputs, which can be a string or numeric offset.
#' @return A POSIXct object in UTC timezone, or NULL if as_of is NULL.
#' @noRd
#' @keywords internal
normalize_as_of_input <- function(as_of, tzone) {
  if (is.null(as_of)) {
    return(NULL)
  }

  if (length(as_of) != 1) {
    stop("`as_of` must be NULL or a single date/datetime value.")
  }

  if (is.character(tzone)) {
    numeric_tzone <- suppressWarnings(as.numeric(tzone))
    if (!is.na(numeric_tzone) && grepl("^[-+]?\\d+$", trimws(tzone))) {
      tzone <- numeric_tzone
    }
  }

  if (is.numeric(tzone)) {
    if (length(tzone) != 1 || is.na(tzone) || tzone %% 1 != 0) {
      stop("Numeric timezone offsets must be a single whole hour value.")
    }
    if (tzone == 0) {
      tzone <- "UTC"
    } else {
      tzone <- sprintf("Etc/GMT%+d", -as.integer(tzone))
    }
  }

  if (inherits(as_of, "character")) {
    as_of_text <- trimws(as.character(as_of[[1]]))
    has_time_component <- grepl("[ T]\\d{1,2}:\\d{2}", as_of_text) ||
      grepl("(Z|[+-]\\d{2}:?\\d{2})$", as_of_text)

    if (has_time_component) {
      as_of <- suppressWarnings(as.POSIXct(as_of_text, tz = tzone))
    } else {
      as_of <- suppressWarnings(as.POSIXct(as.Date(as_of_text), tz = tzone))
      as_of <- as_of + 24 * 60 * 60
    }
  } else if (inherits(as_of, "Date") && !inherits(as_of, "POSIXt")) {
    as_of <- as.POSIXct(as_of, tz = tzone)
    as_of <- as_of + 24 * 60 * 60
  } else if (inherits(as_of, "POSIXt")) {
    as_of <- as.POSIXct(as_of, tz = tzone)
  }

  if (!inherits(as_of, "POSIXt") || is.na(as_of)) {
    stop(
      "`as_of` must be NULL or a single character, Date, or POSIXct value."
    )
  }

  attr(as_of, "tzone") <- "UTC"
  as_of
}

#' Format the as_of value for use in plot titles, respecting the specified timezone and language
#' @param as_of A POSIXct object representing the as_of datetime, or NULL.
#' @param tzone The timezone to use for formatting the datetime, which can be a string or numeric offset.
#' @param lang The language code for formatting the title, either "en" for English or "fr" for French.
#' @return A formatted string for the plot title, or NULL if as_of is NULL.
#' @noRd
#' @keywords internal
format_as_of_title <- function(as_of, tzone, lang = "en") {
  if (is.null(as_of)) {
    return(NULL)
  }

  if (lang == "fr") {
    paste0("Données au ", format(as_of, tz = tzone, usetz = TRUE))
  } else {
    paste0("As of ", format(as_of, tz = tzone, usetz = TRUE))
  }
}

#' @title Fetch hourly trace data
#' @description Fetches hourly aggregated trace data for a given timeseries and date range, with options for using corrected values and specifying an as_of datetime.
#' @param con A DBI database connection object.
#' @param timeseries_id The ID of the timeseries to fetch data for.
#' @param start_date The start date of the range to fetch data for (POSIXct or character).
#' @param end_date The end date of the range to fetch data for (POSIXct or character).
#' @param raw Logical indicating whether to include raw values in the output.
#' @param use_corrected_source Logical indicating whether to use the corrected source table for fetching data.
#' @param as_of An optional datetime (POSIXct, character, or Date) to fetch data as of a specific point in time, which will override the use_corrected_source parameter if provided.
#' @return A data.table containing the hourly aggregated trace data, with columns for datetime, value, and optionally value_raw and imputed.
#' @noRd
fetch_hourly_trace_data <- function(
  con,
  timeseries_id,
  start_date,
  end_date,
  raw = FALSE,
  use_corrected_source = TRUE,
  as_of = NULL
) {
  if (is.null(as_of)) {
    source_table <- if (isTRUE(use_corrected_source)) {
      "measurements_continuous_corrected"
    } else {
      "measurements_continuous"
    }

    value_col <- if (isTRUE(use_corrected_source)) {
      "value_corrected"
    } else {
      "value"
    }
    raw_value_col <- if (isTRUE(use_corrected_source)) {
      "value_raw"
    } else {
      "value"
    }
    where_sql <- "WHERE m.timeseries_id = $1 AND m.datetime BETWEEN $2 AND $3 "
    params <- list(timeseries_id, start_date, end_date)
  } else {
    source_table <- paste0(
      "continuous.measurements_continuous_corrected_at(",
      "$1, ARRAY[$2]::INTEGER[], $3, $4",
      ")"
    )
    value_col <- "value_corrected"
    raw_value_col <- "value_raw"
    where_sql <- ""
    params <- list(as_of, timeseries_id, start_date, end_date)
  }

  agg_sql <- function(column_name) {
    paste0(
      "CASE ",
      "WHEN at.aggregation_type = 'sum' THEN SUM(m.",
      column_name,
      ") ",
      "WHEN at.aggregation_type = 'median' THEN percentile_cont(0.5) ",
      "WITHIN GROUP (ORDER BY m.",
      column_name,
      ") ",
      "WHEN at.aggregation_type IN ('min', 'minimum') THEN MIN(m.",
      column_name,
      ") ",
      "WHEN at.aggregation_type IN ('max', 'maximum') THEN MAX(m.",
      column_name,
      ") ",
      "WHEN at.aggregation_type = '(min+max)/2' THEN ",
      "(MIN(m.",
      column_name,
      ") + MAX(m.",
      column_name,
      ")) / 2.0 ",
      "ELSE AVG(m.",
      column_name,
      ") END"
    )
  }

  query <- paste0(
    "SELECT ",
    "date_trunc('hour', m.datetime) AS datetime, ",
    agg_sql(value_col),
    " AS value, ",
    if (isTRUE(raw)) {
      paste0(agg_sql(raw_value_col), " AS value_raw, ")
    } else {
      ""
    },
    "BOOL_OR(COALESCE(m.imputed, FALSE)) AS imputed ",
    "FROM ",
    source_table,
    " m ",
    "LEFT JOIN timeseries ts ON m.timeseries_id = ts.timeseries_id ",
    "LEFT JOIN aggregation_types at ",
    "ON ts.aggregation_type_id = at.aggregation_type_id ",
    where_sql,
    "GROUP BY date_trunc('hour', m.datetime), at.aggregation_type ",
    "ORDER BY datetime ASC;"
  )

  hourly <- dbGetQueryDT(
    con,
    query,
    params = params
  )

  if (nrow(hourly) == 0) {
    if (isTRUE(raw)) {
      return(data.table::data.table(
        datetime = as.POSIXct(character(), tz = "UTC"),
        value = numeric(),
        value_raw = numeric(),
        imputed = logical()
      ))
    }

    return(data.table::data.table(
      datetime = as.POSIXct(character(), tz = "UTC"),
      value = numeric(),
      imputed = logical()
    ))
  }

  hourly$datetime <- as.POSIXct(hourly$datetime, tz = "UTC")
  attr(hourly$datetime, "tzone") <- "UTC"
  hourly
}

#' @title Add gap markers to trace data
#' @description Inserts rows with NA values into the trace data where there are gaps in the datetime sequence that exceed a specified period, which helps to visually indicate gaps in the data when plotting.
#' @param trace_data A data.table containing the trace data with a datetime column.
#' @param period_seconds The threshold in seconds for identifying gaps in the datetime sequence. If the difference between consecutive datetimes exceeds this threshold, a gap marker will be inserted.
#' @return A data.table with gap markers (rows with NA values) inserted where there are gaps in the datetime sequence.
#' @noRd
#' @keywords internal
add_gap_markers <- function(trace_data, period_seconds) {
  if (nrow(trace_data) == 0 || !("datetime" %in% names(trace_data))) {
    return(trace_data)
  }

  trace_data <- data.table::as.data.table(trace_data)
  data.table::setorder(trace_data, datetime)

  trace_data[, next_datetime := data.table::shift(datetime, type = "lead")]
  gap_indices <- which(
    !is.na(trace_data$next_datetime) &
      as.numeric(
        trace_data$next_datetime - trace_data$datetime,
        units = "secs"
      ) >
        period_seconds
  )

  if (length(gap_indices) == 0) {
    trace_data[, next_datetime := NULL]
    return(trace_data)
  }

  na_rows <- data.table::data.table(
    datetime = trace_data$datetime[gap_indices] + 1
  )
  other_cols <- setdiff(names(trace_data), c("datetime", "next_datetime"))
  for (col in other_cols) {
    na_rows[[col]] <- NA
  }
  if ("imputed" %in% names(na_rows)) {
    na_rows[, imputed := FALSE]
  }

  trace_data[, next_datetime := NULL]
  out <- data.table::rbindlist(
    list(trace_data, na_rows),
    use.names = TRUE,
    fill = TRUE
  )
  data.table::setorder(out, datetime)
  out
}
