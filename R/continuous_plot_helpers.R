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
  ts_type <- DBI::dbGetQuery(
    con,
    "SELECT timeseries_type FROM continuous.timeseries WHERE timeseries_id = $1",
    params = list(timeseries_id)
  )
  if (
    nrow(ts_type) > 0 &&
      !is.na(ts_type$timeseries_type[1]) &&
      ts_type$timeseries_type[1] != "basic"
  ) {
    return(TRUE)
  }

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

#' Normalize the daily-statistics period selector
#' @param stats_period One of "full" or "30yr".
#' @return A normalized period string.
#' @noRd
#' @keywords internal
normalize_daily_stats_period <- function(stats_period) {
  stats_period <- match.arg(stats_period, c("full", "30yr"))
  stats_period
}

#' Resolve the historic-statistics window shown in plot-data exports
#' @param stats_period One of "full" or "30yr".
#' @param trace_data Exported trace data with a datetime column.
#' @param range_data Exported range data with a datetime column.
#' @param timeseries_start Full record start datetime.
#' @param timeseries_end Full record end datetime.
#' @return A list with formatted start and end values.
#' @noRd
#' @keywords internal
historic_stats_export_window <- function(
  stats_period = "full",
  trace_data = NULL,
  range_data = NULL,
  timeseries_start = NULL,
  timeseries_end = NULL
) {
  stats_period <- normalize_daily_stats_period(stats_period)
  na_posix <- as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")

  as_utc_posix <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return(na_posix)
    }
    x <- x[!is.na(x)]
    if (length(x) == 0) {
      return(na_posix)
    }
    out <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
    attr(out, "tzone") <- "UTC"
    out[!is.na(out)]
  }

  min_datetime <- function(x) {
    x <- as_utc_posix(x)
    if (length(x) == 0 || all(is.na(x))) {
      return(na_posix)
    }
    min(x)
  }

  max_datetime <- function(x) {
    x <- as_utc_posix(x)
    if (length(x) == 0 || all(is.na(x))) {
      return(na_posix)
    }
    max(x)
  }

  full_start <- min_datetime(timeseries_start)
  full_end <- max_datetime(timeseries_end)
  export_start <- min_datetime(trace_data$datetime)
  export_end <- max_datetime(trace_data$datetime)

  if (is.data.frame(range_data) && "datetime" %in% names(range_data)) {
    range_start <- min_datetime(range_data$datetime)
    range_end <- max_datetime(range_data$datetime)
    if (!is.na(range_start)) {
      export_start <- range_start
    }
    if (!is.na(range_end)) {
      export_end <- range_end
    }
  }

  if (is.na(full_start)) {
    full_start <- export_start
  }
  if (is.na(full_end)) {
    full_end <- export_end
  }

  stats_start <- full_start
  stats_end <- full_end
  if (identical(stats_period, "30yr")) {
    stats_start <- if (is.na(export_start)) {
      full_start
    } else {
      lubridate::add_with_rollback(export_start, -lubridate::years(30))
    }
    if (!is.na(full_start) && !is.na(stats_start) && stats_start < full_start) {
      stats_start <- full_start
    }

    stats_end <- export_end
    if (!is.na(full_end) && !is.na(stats_end) && stats_end > full_end) {
      stats_end <- full_end
    }
  }

  start_text <- if (is.na(stats_start)) {
    NA_character_
  } else {
    format(stats_start, "%Y-%m-%d %H:%M")
  }
  end_text <- if (is.na(stats_end)) {
    NA_character_
  } else {
    format(stats_end, "%Y-%m-%d %H:%M")
  }

  list(
    start = start_text,
    end = end_text,
    start_year = if (is.na(stats_start)) {
      NA_character_
    } else {
      format(stats_start, "%Y")
    },
    end_year = if (is.na(stats_end)) {
      NA_character_
    } else {
      format(stats_end, "%Y")
    }
  )
}

#' Prepare historic-range data for plot-data exports
#' @param range_data Plot range data returned by continuous plotting helpers.
#' @param units Unit label for exported historic-stat columns.
#' @return A data frame ready for XLSX export, or NULL when no range exists.
#' @noRd
#' @keywords internal
historic_range_data_for_export <- function(range_data, units) {
  expected_cols <- c("datetime", "min", "max", "q75", "q25")
  if (
    !is.data.frame(range_data) ||
      nrow(range_data) == 0L ||
      !all(expected_cols %in% names(range_data))
  ) {
    return(NULL)
  }

  has_complete_stats <- Reduce(
    `&`,
    lapply(range_data[c("min", "max", "q75", "q25")], function(x) !is.na(x))
  )
  if (!any(has_complete_stats)) {
    return(NULL)
  }

  range_data <- data.table::as.data.table(range_data)[, ..expected_cols]
  data.table::setnames(
    range_data,
    expected_cols,
    c(
      "datetime_UTC",
      paste0("historic_min_", units),
      paste0("historic_max_", units),
      paste0("historic_Q75_", units),
      paste0("historic_Q25_", units)
    )
  )
  as.data.frame(range_data)
}

#' Build the historic-statistics caption used below plot legends
#' @param stats_period One of "full" or "30yr".
#' @param plot_data Plot data returned by continuous plotting helpers.
#' @param timeseries_ids Timeseries IDs included in the plot.
#' @param timeseries_table Timeseries metadata with start/end datetimes.
#' @param lang Plot language.
#' @return Caption text, or NULL when no usable year range is available.
#' @noRd
#' @keywords internal
historic_stats_caption_for_plot_data <- function(
  stats_period = "full",
  plot_data = NULL,
  timeseries_ids = NULL,
  timeseries_table = NULL,
  lang = "en"
) {
  if (is.null(timeseries_ids) || length(timeseries_ids) == 0) {
    return(NULL)
  }
  if (
    is.null(timeseries_table) || !"timeseries_id" %in% names(timeseries_table)
  ) {
    return(NULL)
  }

  plot_data_for_id <- function(index, timeseries_id) {
    if (
      is.list(plot_data) &&
        all(c("trace_data", "range_data") %in% names(plot_data))
    ) {
      return(plot_data)
    }
    if (!is.list(plot_data) || length(plot_data) == 0) {
      return(list(trace_data = NULL, range_data = NULL))
    }

    plot_data_names <- names(plot_data)
    if (
      !is.null(plot_data_names) &&
        as.character(timeseries_id) %in% plot_data_names
    ) {
      return(plot_data[[as.character(timeseries_id)]])
    }
    if (length(plot_data) >= index) {
      return(plot_data[[index]])
    }
    list(trace_data = NULL, range_data = NULL)
  }

  windows <- lapply(seq_along(timeseries_ids), function(i) {
    timeseries_id <- timeseries_ids[[i]]
    row_index <- match(
      as.character(timeseries_id),
      as.character(timeseries_table[["timeseries_id"]])
    )
    if (is.na(row_index)) {
      return(NULL)
    }

    item_data <- plot_data_for_id(i, timeseries_id)
    historic_stats_export_window(
      stats_period = stats_period,
      trace_data = item_data$trace_data,
      range_data = item_data$range_data,
      timeseries_start = timeseries_table[["start_datetime"]][[row_index]],
      timeseries_end = timeseries_table[["end_datetime"]][[row_index]]
    )
  })
  windows <- Filter(Negate(is.null), windows)
  if (length(windows) == 0) {
    return(NULL)
  }

  start_years <- suppressWarnings(as.integer(vapply(
    windows,
    function(window) window$start_year,
    character(1)
  )))
  end_years <- suppressWarnings(as.integer(vapply(
    windows,
    function(window) window$end_year,
    character(1)
  )))
  start_years <- start_years[!is.na(start_years)]
  end_years <- end_years[!is.na(end_years)]
  if (length(start_years) == 0 || length(end_years) == 0) {
    return(NULL)
  }

  start_year <- min(start_years)
  end_year <- max(end_years)
  varies <- length(unique(start_years)) > 1 || length(unique(end_years)) > 1

  if (identical(lang, "fr")) {
    caption <- if (start_year == end_year) {
      sprintf(
        "Plage historique calcul\u00E9e avec les donn\u00E9es de %s.",
        start_year
      )
    } else {
      sprintf(
        "Plage historique calcul\u00E9e avec les donn\u00E9es de %s \u00E0 %s.",
        start_year,
        end_year
      )
    }
    if (isTRUE(varies)) {
      caption <- paste(
        caption,
        "La plage varie selon la s\u00E9rie chronologique."
      )
    }
    return(caption)
  }

  caption <- if (start_year == end_year) {
    sprintf("Historic range calculated with data from %s.", start_year)
  } else {
    sprintf(
      "Historic range calculated with data from %s to %s.",
      start_year,
      end_year
    )
  }
  if (isTRUE(varies)) {
    caption <- paste(caption, "Range varies by timeseries.")
  }
  caption
}

#' Build stable daily-statistics SELECT expressions
#' @param con A DBI database connection object.
#' @param stats_period One of "full" or "30yr".
#' @param columns Base daily-statistics columns to select.
#' @param table_alias Optional table alias to prefix column references.
#' @return A character vector of SQL SELECT expressions.
#' @noRd
#' @keywords internal
daily_stats_select_sql <- function(
  con,
  stats_period = "full",
  columns = c("min", "max", "q75", "q25"),
  table_alias = NULL
) {
  stats_period <- normalize_daily_stats_period(stats_period)
  available <- DBI::dbGetQuery(
    con,
    "SELECT column_name
     FROM information_schema.columns
     WHERE table_schema = 'continuous'
       AND table_name = 'measurements_calculated_daily'"
  )$column_name

  suffix <- ""
  if (
    identical(stats_period, "30yr") &&
      all(paste0(columns, "_30yr") %in% available)
  ) {
    suffix <- "_30yr"
  }

  prefix <- if (is.null(table_alias) || !nzchar(table_alias)) {
    ""
  } else {
    paste0(table_alias, ".")
  }

  vapply(
    columns,
    function(column) {
      source_column <- paste0(column, suffix)
      source_sql <- paste0(
        prefix,
        as.character(DBI::dbQuoteIdentifier(con, source_column))
      )
      if (identical(source_column, column)) {
        source_sql
      } else {
        paste0(
          source_sql,
          " AS ",
          as.character(DBI::dbQuoteIdentifier(con, column))
        )
      }
    },
    character(1)
  )
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
    paste0("Donn\u00e9es au ", format(as_of, tz = tzone, usetz = TRUE))
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
#' @keywords internal
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
    if (isTRUE(use_corrected_source)) {
      source_table <- "continuous.measurements_continuous_corrected($1, $2, $3)"
      value_col <- "value_corrected"
      raw_value_col <- "value_raw"
      where_sql <- ""
      params <- list(timeseries_id, start_date, end_date)
    } else {
      source_table <- "measurements_continuous"
      value_col <- "value"
      raw_value_col <- "value"
      where_sql <- "WHERE m.timeseries_id = $1 AND m.datetime BETWEEN $2 AND $3 "
      params <- list(timeseries_id, start_date, end_date)
    }
  } else {
    source_table <- paste0(
      "continuous.measurements_continuous_corrected_at(",
      "$1, $2, $3, $4",
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
  data.table::setorder(trace_data, "datetime")

  trace_data[,
    "next_datetime" := data.table::shift(trace_data$datetime, type = "lead")
  ]
  gap_indices <- which(
    !is.na(trace_data$next_datetime) &
      as.numeric(
        trace_data$next_datetime - trace_data$datetime,
        units = "secs"
      ) >
        period_seconds
  )

  if (length(gap_indices) == 0) {
    trace_data[, "next_datetime" := NULL]
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
    na_rows[, "imputed" := FALSE]
  }

  trace_data[, "next_datetime" := NULL]
  out <- data.table::rbindlist(
    list(trace_data, na_rows),
    use.names = TRUE,
    fill = TRUE
  )
  data.table::setorder(out, "datetime")
  out
}
