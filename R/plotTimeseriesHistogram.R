#' Plot binned/aggregated continuous data as a histogram
#'
#' @description
#' Plot a histogram-style bar chart for a continuous AquaCache timeseries using
#' corrected values. Binning and aggregation are pushed to PostgreSQL where
#' possible for improved performance on high-frequency data.
#'
#' @param timeseries_id Timeseries ID from `continuous.timeseries`.
#' @param start_datetime Start datetime (character, Date, or POSIXct).
#' @param end_datetime End datetime (character, Date, or POSIXct).
#' @param bin_width Numeric width for each bin (e.g., `1`, `6`, `24`).
#' @param bin_width_units Bin units. One of `"hours"`, `"days"`, `"weeks"`,
#'   `"months"`, or `"years"`.
#' @param years Optional numeric vector of calendar years to include
#'   (e.g., `c(2020, 2021, 2024)`). If `NULL`, years are not filtered.
#' @param transformation Aggregation applied within each bin. One of `"sum"`,
#'   `"mean"`, `"min"`, `"max"`, `"median"`, or `"integral"`. `"integral"`
#'   computes `sum(value * duration_seconds)` and is useful for rate→total use
#'   cases (e.g., exported volume from flow).
#' @param title Should a plot title be shown? Default is `TRUE`.
#' @param custom_title Optional custom title.
#' @param lang Language for metadata labels in title/axes (`"en"` or `"fr"`).
#' @param tzone Timezone for binning and axis display. Default is `"UTC"`.
#' @param data If `TRUE`, return plotting data with the plot.
#' @param con A connection to the AquaCache database. `NULL` uses
#'   [AquaConnect()] and automatically disconnects.
#'
#' @return A plotly object, or a list with `plot` and `data` when `data = TRUE`.
#' @export
plotTimeseriesHistogram <- function(
  timeseries_id,
  start_datetime,
  end_datetime,
  bin_width = 1,
  bin_width_units = "days",
  years = NULL,
  transformation = "sum",
  title = TRUE,
  custom_title = NULL,
  lang = "en",
  tzone = "UTC",
  data = FALSE,
  con = NULL
) {
  # Testing parameters
  timeseries_id = 100
  start_datetime = "2025-01-01"
  end_datetime = "2025-12-31"
  bin_width = 1
  bin_width_units = "months"
  years = c(2023, 2024, 2024)
  transformation = "integral"
  title = TRUE
  custom_title = NULL
  lang = "en"
  tzone = "UTC"
  data = FALSE
  con = NULL
  if (
    missing(timeseries_id) ||
      length(timeseries_id) != 1 ||
      !is.numeric(timeseries_id)
  ) {
    stop("timeseries_id must be a single numeric value.")
  }

  if (!(lang %in% c("en", "fr"))) {
    stop("lang must be one of 'en' or 'fr'.")
  }

  bin_width_units <- tolower(bin_width_units)
  valid_units <- c("hours", "days", "weeks", "months", "years")
  if (!(bin_width_units %in% valid_units)) {
    stop(
      "bin_width_units must be one of 'hours', 'days', 'weeks', 'months', or 'years'."
    )
  }

  if (
    !is.numeric(bin_width) ||
      length(bin_width) != 1 ||
      is.na(bin_width) ||
      bin_width <= 0
  ) {
    stop("bin_width must be a single positive numeric value.")
  }

  if (!is.null(years)) {
    if (!is.numeric(years) || any(is.na(years))) {
      stop("years must be NULL or a numeric vector of years.")
    }
    years <- sort(unique(as.integer(years)))
  }

  transformation <- tolower(transformation)
  valid_transformations <- c("sum", "mean", "min", "max", "median", "integral")
  if (!(transformation %in% valid_transformations)) {
    stop(
      "transformation must be one of 'sum', 'mean', 'min', 'max', 'median', or 'integral'."
    )
  }

  if (inherits(start_datetime, "character")) {
    start_datetime <- as.POSIXct(start_datetime, tz = tzone)
  }
  if (inherits(start_datetime, "Date")) {
    start_datetime <- as.POSIXct(start_datetime, tz = tzone)
  }

  if (inherits(end_datetime, "character")) {
    end_datetime <- as.POSIXct(end_datetime, tz = tzone)
  }
  if (inherits(end_datetime, "Date")) {
    end_datetime <- as.POSIXct(end_datetime, tz = tzone)
  }

  if (
    !inherits(start_datetime, "POSIXct") || !inherits(end_datetime, "POSIXct")
  ) {
    stop("start_datetime and end_datetime must be coercible to POSIXct.")
  }
  if (start_datetime > end_datetime) {
    stop("start_datetime must be before end_datetime.")
  }

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }

  attr(start_datetime, "tzone") <- "UTC"
  attr(end_datetime, "tzone") <- "UTC"

  metadata_view <- if (lang == "fr") {
    "continuous.timeseries_metadata_fr"
  } else {
    "continuous.timeseries_metadata_en"
  }
  meta <- dbGetQueryDT(
    con,
    paste0(
      "SELECT timeseries_id, location_name, parameter_name, units ",
      "FROM ",
      metadata_view,
      " WHERE timeseries_id = $1"
    ),
    params = list(timeseries_id)
  )

  if (nrow(meta) == 0) {
    stop("timeseries_id not found in AquaCache metadata.")
  }

  # Build SQL bin expression and x-axis ordering expression.
  bin_width_int <- as.integer(bin_width)
  if (!isTRUE(all.equal(bin_width, bin_width_int))) {
    stop("bin_width must be a whole number for database-side binning.")
  }

  bin_expr <- switch(
    bin_width_units,
    hours = paste0(
      "to_timestamp(floor(extract(epoch from datetime) / ",
      bin_width_int * 3600L,
      ") * ",
      bin_width_int * 3600L,
      ")"
    ),
    days = paste0(
      "to_timestamp(floor(extract(epoch from datetime) / ",
      bin_width_int * 86400L,
      ") * ",
      bin_width_int * 86400L,
      ")"
    ),
    weeks = paste0(
      "to_timestamp(floor(extract(epoch from datetime) / ",
      bin_width_int * 604800L,
      ") * ",
      bin_width_int * 604800L,
      ")"
    ),
    months = paste0(
      "date_trunc('year', datetime) + ",
      "(floor((extract(month from datetime) - 1) / ",
      bin_width_int,
      ")::int * interval '",
      bin_width_int,
      " month')"
    ),
    years = paste0(
      "make_date((floor(extract(year from datetime)::numeric / ",
      bin_width_int,
      ")::int * ",
      bin_width_int,
      "), 1, 1)::timestamp"
    )
  )

  agg_expr <- switch(
    transformation,
    sum = "sum(value_corrected)",
    mean = "avg(value_corrected)",
    min = "min(value_corrected)",
    max = "max(value_corrected)",
    median = "percentile_cont(0.5) within group (order by value_corrected)",
    integral = "sum(value_corrected * duration_seconds)"
  )

  year_filter_sql <- ""
  query_params <- list(timeseries_id, start_datetime, end_datetime)
  if (!is.null(years)) {
    year_filter_sql <- paste0(
      " AND extract(year from datetime)::int IN (",
      paste(years, collapse = ", "),
      ")"
    )
    query_params <- list(timeseries_id, start_datetime, end_datetime)
  }

  sql <- paste0(
    "WITH base AS (",
    "  SELECT timeseries_id, datetime, value_corrected, period ",
    "  FROM continuous.measurements_continuous_corrected ",
    "  WHERE timeseries_id = $1 AND datetime >= $2 AND datetime <= $3",
    year_filter_sql,
    "), prepared AS (",
    "  SELECT datetime, value_corrected, ",
    "         coalesce(extract(epoch from period), ",
    "                  extract(epoch from (lead(datetime) over (order by datetime) - datetime)), ",
    "                  0) AS duration_seconds ",
    "  FROM base",
    "), binned AS (",
    "  SELECT ",
    bin_expr,
    " AS bin_start, ",
    "         ",
    agg_expr,
    " AS value ",
    "  FROM prepared ",
    "  GROUP BY 1",
    ") ",
    "SELECT bin_start, value FROM binned ORDER BY bin_start"
  )

  plot_data <- dbGetQueryDT(con, sql, params = query_params)

  if (nrow(plot_data) == 0) {
    stop("No data found for the requested timeseries/date range/year filter.")
  }

  plot_data[, bin_start := as.POSIXct(bin_start, tz = tzone)]

  units_label <- meta$units[1]
  if (is.na(units_label)) {
    units_label <- ""
  }

  if (transformation == "integral") {
    units_label <- gsub(
      "/(s|sec|second|seconds)$",
      "",
      units_label,
      ignore.case = TRUE
    )
    units_label <- gsub(
      "/(h|hr|hour|hours)$",
      "",
      units_label,
      ignore.case = TRUE
    )
    units_label <- gsub("/(d|day|days)$", "", units_label, ignore.case = TRUE)
    units_label <- gsub(
      "\\bs\\^-1\\b",
      "",
      units_label,
      perl = TRUE,
      ignore.case = TRUE
    )
    units_label <- gsub(
      "\\bh\\^-1\\b",
      "",
      units_label,
      perl = TRUE,
      ignore.case = TRUE
    )
    units_label <- gsub(
      "\\b(day|days|hour|hours|second|seconds)\\^-1\\b",
      "",
      units_label,
      perl = TRUE,
      ignore.case = TRUE
    )
    units_label <- trimws(units_label)
  }

  y_title <- if (nzchar(units_label)) {
    paste0(titleCase(transformation), " (", units_label, ")")
  } else {
    titleCase(transformation)
  }

  # Determine grouping behaviour: if specific multiple years are requested, group by year.
  years_in_result <- sort(unique(lubridate::year(plot_data$bin_start)))
  group_by_year <- length(years_in_result) > 1 && bin_width_units != "years"

  if (group_by_year) {
    plot_data[, year := lubridate::year(bin_start)]

    if (bin_width_units == "years") {
      plot_data[, bin_label := format(bin_start, "%Y")]
      plot_data[, bin_order := as.numeric(format(bin_start, "%Y"))]
    } else if (bin_width_units == "months") {
      plot_data[, bin_label := format(bin_start, "%b")]
      plot_data[, bin_order := lubridate::month(bin_start)]
    } else if (bin_width_units == "weeks") {
      plot_data[, bin_label := format(bin_start, "%b %d")]
      plot_data[, bin_order := as.integer(format(as.Date(bin_start), "%j"))]
    } else if (bin_width_units == "days") {
      plot_data[, bin_label := format(bin_start, "%b %d")]
      plot_data[, bin_order := as.integer(format(as.Date(bin_start), "%j"))]
    } else {
      plot_data[, bin_label := format(bin_start, "%b %d %H:%M")]
      plot_data[,
        bin_order := as.integer(format(as.Date(bin_start), "%j")) *
          24L +
          as.integer(format(bin_start, "%H"))
      ]
    }

    plot_data <- plot_data[order(bin_order, year)]
    year_levels <- as.character(sort(unique(plot_data$year)))
    colors <- rev(grDevices::colorRampPalette(c(
      "#00454e",
      "#7A9A01",
      "#FFA900",
      "#DC4405"
    ))(length(year_levels)))

    p <- plotly::plot_ly(type = "bar")
    for (i in seq_along(year_levels)) {
      yv <- year_levels[i]
      d <- plot_data[as.character(year) == yv]
      p <- plotly::add_trace(
        p,
        data = d,
        x = ~bin_label,
        y = ~value,
        name = yv,
        marker = list(color = colors[i])
      )
    }

    p <- plotly::layout(
      p,
      barmode = "group",
      xaxis = list(title = "Bin"),
      yaxis = list(title = y_title),
      legend = list(title = list(text = "Year"))
    )
  } else {
    p <- plotly::plot_ly(
      data = plot_data,
      x = ~bin_start,
      y = ~value,
      type = "bar",
      marker = list(color = palette[1])
    ) |>
      plotly::layout(
        xaxis = list(title = "Datetime"),
        yaxis = list(title = y_title)
      )
  }

  if (isTRUE(title)) {
    if (is.null(custom_title)) {
      custom_title <- paste0(
        meta$location_name[1],
        " - ",
        meta$parameter_name[1],
        " (",
        transformation,
        ")"
      )
    }
    p <- plotly::layout(p, title = custom_title)
  }

  if (isTRUE(data)) {
    return(list(plot = p, data = plot_data))
  }

  return(p)
}
