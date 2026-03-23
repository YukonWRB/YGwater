continuous_trace_uses_corrected_source <- function(
    con,
    timeseries_id,
    start_date,
    end_date) {
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

fetch_hourly_trace_data <- function(
    con,
    timeseries_id,
    start_date,
    end_date,
    raw = FALSE,
    use_corrected_source = TRUE) {
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

  agg_sql <- function(column_name) {
    paste0(
      "CASE ",
      "WHEN at.aggregation_type = 'sum' THEN SUM(m.", column_name, ") ",
      "WHEN at.aggregation_type = 'median' THEN percentile_cont(0.5) ",
      "WITHIN GROUP (ORDER BY m.", column_name, ") ",
      "WHEN at.aggregation_type IN ('min', 'minimum') THEN MIN(m.", column_name, ") ",
      "WHEN at.aggregation_type IN ('max', 'maximum') THEN MAX(m.", column_name, ") ",
      "WHEN at.aggregation_type = '(min+max)/2' THEN ",
      "(MIN(m.", column_name, ") + MAX(m.", column_name, ")) / 2.0 ",
      "ELSE AVG(m.", column_name, ") END"
    )
  }

  query <- paste0(
    "SELECT ",
    "date_trunc('hour', m.datetime) AS datetime, ",
    agg_sql(value_col),
    " AS value, ",
    if (isTRUE(raw)) {
      paste0(agg_sql("value_raw"), " AS value_raw, ")
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
    "WHERE m.timeseries_id = $1 AND m.datetime BETWEEN $2 AND $3 ",
    "GROUP BY date_trunc('hour', m.datetime), at.aggregation_type ",
    "ORDER BY datetime ASC;"
  )

  hourly <- dbGetQueryDT(
    con,
    query,
    params = list(timeseries_id, start_date, end_date)
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

add_gap_markers <- function(trace_data, period_seconds) {
  if (nrow(trace_data) == 0 || !("datetime" %in% names(trace_data))) {
    return(trace_data)
  }

  trace_data <- data.table::as.data.table(trace_data)
  data.table::setorder(trace_data, datetime)

  trace_data[, next_datetime := data.table::shift(datetime, type = "lead")]
  gap_indices <- which(
    !is.na(trace_data$next_datetime) &
      as.numeric(trace_data$next_datetime - trace_data$datetime, units = "secs") >
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
