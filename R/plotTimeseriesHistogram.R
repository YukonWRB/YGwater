#' Plot binned/aggregated continuous data as a histogram
#'
#' @description
#' Plot a histogram-style bar chart for a continuous AquaCache timeseries using
#' corrected values. Data are binned by a configurable width and unit, then
#' aggregated using a selected transformation.
#'
#' @param timeseries_id Timeseries ID from `continuous.timeseries`.
#' @param start_datetime Start datetime (character, Date, or POSIXct).
#' @param end_datetime End datetime (character, Date, or POSIXct).
#' @param bin_width Numeric width for each bin (e.g., `1`, `6`, `24`).
#' @param bin_width_units Bin units. One of `"hours"`, `"days"`, `"weeks"`, or
#'   `"months"`.
#' @param years Number of years to include, counted backward from `end_datetime`.
#'   Default is `1`. If `years > 1`, bars are grouped by year for each recurring
#'   intra-year bin (e.g., January bins adjacent).
#' @param transformation Aggregation applied within each bin. One of `"sum"`,
#'   `"mean"`, `"min"`, `"max"`, `"median"`, or `"integral"`. `"integral"`
#'   computes `sum(value * period_seconds)` and is useful for exported-volume-type
#'   totals when values are rates.
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
  years = 1,
  transformation = "sum",
  title = TRUE,
  custom_title = NULL,
  lang = "en",
  tzone = "UTC",
  data = FALSE,
  con = NULL
) {
  bin_start <- datetime <- value <- year <- bin_label <- bin_order <- period_seconds <- NULL

  if (missing(timeseries_id) || length(timeseries_id) != 1 || !is.numeric(timeseries_id)) {
    stop("timeseries_id must be a single numeric value.")
  }

  if (!(lang %in% c("en", "fr"))) {
    stop("lang must be one of 'en' or 'fr'.")
  }

  bin_width_units <- tolower(bin_width_units)
  valid_units <- c("hours", "days", "weeks", "months")
  if (!(bin_width_units %in% valid_units)) {
    stop("bin_width_units must be one of 'hours', 'days', 'weeks', or 'months'.")
  }

  if (!is.numeric(bin_width) || length(bin_width) != 1 || is.na(bin_width) || bin_width <= 0) {
    stop("bin_width must be a single positive numeric value.")
  }

  if (!is.numeric(years) || length(years) != 1 || is.na(years) || years < 1) {
    stop("years must be a single numeric value >= 1.")
  }
  years <- as.integer(years)

  transformation <- tolower(transformation)
  valid_transformations <- c("sum", "mean", "min", "max", "median", "integral")
  if (!(transformation %in% valid_transformations)) {
    stop("transformation must be one of 'sum', 'mean', 'min', 'max', 'median', or 'integral'.")
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

  if (!inherits(start_datetime, "POSIXct") || !inherits(end_datetime, "POSIXct")) {
    stop("start_datetime and end_datetime must be coercible to POSIXct.")
  }

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }

  # When years > 1, override start_datetime to include the requested window.
  if (years > 1) {
    start_datetime <- lubridate::`%m-%`(end_datetime, lubridate::years(years))
  }

  attr(start_datetime, "tzone") <- "UTC"
  attr(end_datetime, "tzone") <- "UTC"

  metadata_view <- if (lang == "fr") "continuous.timeseries_metadata_fr" else "continuous.timeseries_metadata_en"
  meta <- dbGetQueryDT(
    con,
    paste0(
      "SELECT timeseries_id, location_name, parameter_name, units ",
      "FROM ", metadata_view, " WHERE timeseries_id = $1"
    ),
    params = list(timeseries_id)
  )

  if (nrow(meta) == 0) {
    stop("timeseries_id not found in AquaCache metadata.")
  }

  ts_data <- dbGetQueryDT(
    con,
    paste0(
      "SELECT datetime, value_corrected AS value, ",
      "EXTRACT(EPOCH FROM period) AS period_seconds ",
      "FROM continuous.measurements_continuous_corrected ",
      "WHERE timeseries_id = $1 AND datetime >= $2 AND datetime <= $3 ",
      "ORDER BY datetime"
    ),
    params = list(timeseries_id, start_datetime, end_datetime)
  )

  if (nrow(ts_data) == 0) {
    stop("No data found for the requested timeseries and date range.")
  }

  ts_data[, datetime := as.POSIXct(datetime, tz = tzone)]

  floor_unit <- paste(bin_width, bin_width_units)
  ts_data[, bin_start := lubridate::floor_date(datetime, unit = floor_unit)]

  if (transformation == "integral") {
    if (all(is.na(ts_data$period_seconds))) {
      step_guess <- stats::median(diff(as.numeric(ts_data$datetime)), na.rm = TRUE)
      if (is.finite(step_guess)) {
        ts_data[, period_seconds := step_guess]
      }
    }
    ts_data[is.na(period_seconds), period_seconds := 0]
  }

  agg_fun <- switch(
    transformation,
    sum = function(x) sum(x, na.rm = TRUE),
    mean = function(x) mean(x, na.rm = TRUE),
    min = function(x) min(x, na.rm = TRUE),
    max = function(x) max(x, na.rm = TRUE),
    median = function(x) stats::median(x, na.rm = TRUE),
    integral = function(x, p) sum(x * p, na.rm = TRUE)
  )

  if (transformation == "integral") {
    plot_data <- ts_data[, .(value = agg_fun(value, period_seconds)), by = .(bin_start)]
  } else {
    plot_data <- ts_data[, .(value = agg_fun(value)), by = .(bin_start)]
  }

  units_label <- meta$units[1]
  y_title <- if (transformation == "integral") {
    paste0("Integral (", units_label, "·s)")
  } else {
    paste0(tools::toTitleCase(transformation), " (", units_label, ")")
  }

  if (years > 1) {
    plot_data[, year := lubridate::year(bin_start)]

    if (bin_width_units == "months") {
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
      plot_data[, bin_order := as.integer(format(as.Date(bin_start), "%j")) * 24L + as.integer(format(bin_start, "%H"))]
    }

    plot_data <- plot_data[order(bin_order, year)]

    p <- plotly::plot_ly(
      data = plot_data,
      x = ~bin_label,
      y = ~value,
      color = ~as.factor(year),
      type = "bar"
    ) |>
      plotly::layout(
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
      type = "bar"
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
