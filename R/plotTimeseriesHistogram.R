#' Plot binned/aggregated continuous data as a histogram
#'
#' @description
#' Plot a histogram-style bar chart for a continuous AquaCache timeseries using corrected values. Binning and aggregation are pushed to PostgreSQL where possible for improved performance on high-frequency data.
#'
#' @param timeseries_id Timeseries ID from `continuous.timeseries`.
#' @param startDay Start day-of-year selector (1-366, Date, or `"yyyy-mm-dd"`).
#' @param endDay End day-of-year selector (1-366, Date, or `"yyyy-mm-dd"`).
#' @param bin_width Numeric width for each bin (e.g., `1`, `6`, `24`).
#' @param bin_width_units Bin units. One of `"hours"`, `"days"`, `"weeks"`, `"months"`, or `"years"`.
#' @param years Optional numeric vector of calendar years to include
#'   (e.g., `c(2020, 2021, 2024)`). If the `startDay`/`endDay` range crosses
#'   Dec 31 -> Jan 1, specify the December year(s). If `NULL`, defaults to the
#'   current year.
#' @param transformation Aggregation applied within each bin. One of `"sum"`, `"mean"`, `"min"`, `"max"`, `"median"`, or `"integral"`. `"integral"` computes `sum(value * duration_seconds)` and is useful for rate→total use cases (e.g., exported volume from flow).
#' @param threshold A number between 0 and 1 indicating the minimum proportion of non-missing values required in a bin for it to be included in the plot. Default is `0` (include all bins regardless of missingness).
#' @param completeness_label Should completeness information be included above each bar? Default is `FALSE`.
#' @param title Should a plot title be shown? Default is `TRUE`.
#' @param custom_title Optional custom title.
#' @param lang Language for metadata labels in title/axes (`"en"` or `"fr"`).
#' @param line_scale A scale factor to apply to the bar outline width. Default is `1`.
#' @param axis_scale A scale factor to apply to the size of axis labels. Default is `1`.
#' @param legend_scale A scale factor to apply to the size of text in the legend. Default is `1`.
#' @param legend_position The position of the legend, `'v'` for vertical on the right side or `'h'` for horizontal on the bottom. Default is `'v'`.
#' @param gridx Should gridlines be drawn on the x-axis? Default is `FALSE`.
#' @param gridy Should gridlines be drawn on the y-axis? Default is `FALSE`.
#' @param tzone The timezone to use for the plot. Default is `"auto"`, which will use the system default timezone. Otherwise set to a valid timezone string or a numeric UTC offset (in hours).
#' @param data If `TRUE`, return plotting data with the plot.
#' @param con A connection to the AquaCache database. `NULL` uses
#'   [AquaConnect()] and automatically disconnects.
#' @param as_of Optional point-in-time timestamp at which measurement values
#'   should be reconstructed. Character, Date, and POSIXct inputs are
#'   supported. Date-like inputs are interpreted as the end of that day in
#'   `tzone`. When `NULL` (default), current data are used.
#'
#' @return A plotly object, or a list with `plot` and `data` when `data = TRUE`.
#' @export
plotTimeseriesHistogram <- function(
  timeseries_id,
  startDay,
  endDay,
  bin_width = 1,
  bin_width_units = "days",
  years = NULL,
  transformation = "sum",
  threshold = 0,
  completeness_label = FALSE,
  title = TRUE,
  custom_title = NULL,
  lang = "en",
  line_scale = 1,
  axis_scale = 1,
  legend_scale = 1,
  legend_position = "v",
  gridx = FALSE,
  gridy = FALSE,
  tzone = "auto",
  data = FALSE,
  con = NULL,
  as_of = NULL
) {
  # Testing parameters
  # timeseries_id = 493 # Dawson ECCC precip total
  # startDay = 31
  # endDay = 180
  # bin_width = 1
  # bin_width_units = "weeks"
  # years = c(2023, 2024)
  # transformation = "sum"
  # title = TRUE
  # custom_title = NULL
  # lang = "en"
  # line_scale = 1
  # axis_scale = 1
  # legend_scale = 1
  # legend_position = "v"
  # gridx = FALSE
  # gridy = FALSE
  # tzone = "auto"
  # data = FALSE
  # threshold = 0
  # completeness_label = FALSE
  # con = NULL

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

  if (tzone == "auto") {
    tzone <- Sys.timezone()
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

  as_of <- normalize_as_of_input(as_of, tzone)

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

  if (
    !is.numeric(threshold) ||
      length(threshold) != 1 ||
      is.na(threshold) ||
      threshold < 0 ||
      threshold > 1
  ) {
    stop("threshold must be a single numeric value between 0 and 1.")
  }

  if (
    !is.logical(completeness_label) ||
      length(completeness_label) != 1 ||
      is.na(completeness_label)
  ) {
    stop("completeness_label must be TRUE or FALSE.")
  }

  to_day_of_year <- function(x, arg_name) {
    if (inherits(x, "Date") || inherits(x, "POSIXt")) {
      return(as.integer(format(as.Date(x), "%j")))
    }

    if (is.character(x)) {
      parsed <- suppressWarnings(as.Date(x))
      if (!is.na(parsed)) {
        return(as.integer(format(parsed, "%j")))
      }
      x_num <- suppressWarnings(as.numeric(x))
      if (!is.na(x_num)) {
        x <- x_num
      } else {
        stop(
          arg_name,
          " must be a day-of-year (1-366), Date, or yyyy-mm-dd string."
        )
      }
    }

    if (!is.numeric(x) || length(x) != 1 || is.na(x)) {
      stop(
        arg_name,
        " must be a day-of-year (1-366), Date, or yyyy-mm-dd string."
      )
    }
    x_int <- as.integer(round(x))
    if (!isTRUE(all.equal(x, x_int)) || x_int < 1 || x_int > 366) {
      stop(
        arg_name,
        " must resolve to an integer day-of-year between 1 and 366."
      )
    }
    x_int
  }

  start_doy <- to_day_of_year(startDay, "startDay")
  end_doy <- to_day_of_year(endDay, "endDay")
  overlaps_doy <- start_doy > end_doy

  if (is.null(years)) {
    years <- as.integer(lubridate::year(Sys.Date()))
  } else {
    years <- sort(unique(as.integer(years)))
  }
  if (length(years) < 1) {
    stop("years must contain at least one year.")
  }

  if (is.null(con)) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }

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

  # Enforce integer-width bins and prepare for R-side binning/aggregation.
  bin_width_int <- as.integer(bin_width)
  if (!isTRUE(all.equal(bin_width, bin_width_int))) {
    stop("bin_width must be a whole number for binning.")
  }

  add_bin_period <- function(dt) {
    switch(
      bin_width_units,
      hours = dt + lubridate::period(hours = bin_width_int),
      days = dt + lubridate::period(days = bin_width_int),
      weeks = dt + lubridate::period(weeks = bin_width_int),
      months = dt + lubridate::period(months = bin_width_int),
      years = dt + lubridate::period(years = bin_width_int)
    )
  }

  doy_to_datetime <- function(year, doy, tz) {
    max_doy <- if (lubridate::leap_year(year)) 366L else 365L
    resolved_doy <- min(doy, max_doy)
    as.POSIXct(
      sprintf("%04d-01-01 00:00:00", year),
      tz = tz
    ) +
      lubridate::days(resolved_doy - 1L)
  }

  extend_to_complete_bin <- function(start_dt, requested_end_dt) {
    extended_end <- start_dt
    safety <- 0L
    while (extended_end < requested_end_dt) {
      extended_end <- add_bin_period(extended_end)
      safety <- safety + 1L
      if (safety > 100000L) {
        stop("Unable to build bins for the selected date window.")
      }
    }
    extended_end
  }

  season_windows <- data.table::rbindlist(lapply(years, function(year_i) {
    season_start_local <- doy_to_datetime(year_i, start_doy, tzone)
    requested_end_local <- if (overlaps_doy) {
      doy_to_datetime(year_i + 1L, end_doy, tzone) + lubridate::days(1L)
    } else {
      doy_to_datetime(year_i, end_doy, tzone) + lubridate::days(1L)
    }
    season_end_local <- extend_to_complete_bin(
      season_start_local,
      requested_end_local
    )

    data.table::data.table(
      plot_year = as.integer(year_i),
      season_start_local = season_start_local,
      requested_end_local = requested_end_local,
      season_end_local = season_end_local
    )
  }))

  season_windows[,
    season_start_utc := lubridate::with_tz(season_start_local, "UTC")
  ]
  season_windows[,
    season_end_utc := lubridate::with_tz(season_end_local, "UTC")
  ]
  data.table::setorder(season_windows, plot_year)

  build_bin_windows <- function(windows_dt) {
    bins <- vector("list", nrow(windows_dt))

    for (i in seq_len(nrow(windows_dt))) {
      bin_rows <- list()
      bin_idx <- 1L
      bin_start <- windows_dt$season_start_utc[i]
      repeat {
        if (bin_start >= windows_dt$season_end_utc[i]) {
          break
        }
        bin_end <- add_bin_period(bin_start)
        bin_rows[[bin_idx]] <- data.table::data.table(
          plot_year = windows_dt$plot_year[i],
          bin_order = bin_idx,
          bin_start_utc = bin_start,
          bin_end_utc = bin_end
        )
        bin_start <- bin_end
        bin_idx <- bin_idx + 1L
        if (bin_idx > 100000L) {
          stop("Unable to build bins for the selected date window.")
        }
      }
      bins[[i]] <- data.table::rbindlist(bin_rows)
    }

    data.table::rbindlist(bins)
  }

  bin_windows <- build_bin_windows(season_windows)

  if (is.null(as_of)) {
    source_sql <- "continuous.measurements_continuous_corrected"
    query_params <- list(timeseries_id)
    next_param <- 2L
  } else {
    source_sql <- paste(
      "continuous.measurements_continuous_corrected_at(",
      "  $1,",
      "  ARRAY[$2]::INTEGER[],",
      "  $3,",
      "  $4",
      ")"
    )
    query_params <- list(
      as_of,
      timeseries_id,
      min(season_windows$season_start_utc),
      max(season_windows$season_end_utc)
    )
    next_param <- 5L
  }

  sql_windows <- character(nrow(season_windows))
  for (i in seq_len(nrow(season_windows))) {
    query_params[[next_param]] <- season_windows$season_start_utc[i]
    query_params[[next_param + 1L]] <- season_windows$season_end_utc[i]
    sql_windows[i] <- paste0(
      "(m.datetime >= $",
      next_param,
      " AND m.datetime < $",
      next_param + 1L,
      ")"
    )
    next_param <- next_param + 2L
  }

  where_sql <- if (is.null(as_of)) {
    paste0("m.timeseries_id = $1 AND (", paste(sql_windows, collapse = " OR "), ")")
  } else {
    paste(sql_windows, collapse = " OR ")
  }

  sql <- paste0(
    "SELECT m.datetime, m.value_corrected AS value
     FROM ",
    source_sql,
    " m
     WHERE ",
    where_sql,
    " ORDER BY datetime"
  )
  db_data <- dbGetQueryDT(con, sql, params = query_params)

  if (nrow(db_data) == 0) {
    stop("No data found for the requested day-of-year range and years.")
  }

  season_join <- season_windows[, .(
    plot_year,
    season_start_utc,
    season_end_utc
  )]
  data.table::setkey(season_join, season_start_utc, season_end_utc)
  raw_data <- season_join[
    db_data,
    on = .(season_start_utc <= datetime, season_end_utc > datetime),
    nomatch = 0L,
    .(
      plot_year,
      datetime = i.datetime,
      value = i.value
    )
  ]

  if (nrow(raw_data) == 0) {
    stop("No data found for the requested day-of-year range and years.")
  }

  # Calculate the period so we can estimate data coverage/completeness in each bin.
  raw_data <- tryCatch(
    calculate_period(raw_data, "datetime", con = NULL),
    error = function(e) {
      raw_data[, period := NA_character_]
      raw_data
    }
  )
  raw_data[, period_seconds := as.numeric(lubridate::period(period))]
  data.table::setorder(raw_data, plot_year, datetime)

  infer_step_seconds <- function(dt) {
    n <- length(dt)
    out <- rep(NA_real_, n)
    if (n < 4) {
      return(out)
    }

    diffs_hours <- as.numeric(diff(dt), units = "hours")
    smoothed_diffs <- zoo::rollmedian(
      diffs_hours,
      k = 3,
      fill = NA,
      align = "center"
    )
    if (length(smoothed_diffs) == 0 || all(is.na(smoothed_diffs))) {
      return(out)
    }

    diff_dt <- data.table::data.table(
      idx = seq_along(smoothed_diffs),
      diff = smoothed_diffs
    )
    diff_dt[, run := data.table::rleid(diff_dt$diff)]
    runs <- diff_dt[, .(diff = diff[1], start = .I[1], len = .N), by = run]
    runs <- runs[!is.na(diff) & diff < 25 & len >= 3]
    if (nrow(runs) == 0) {
      return(out)
    }

    out[runs$start] <- runs$diff * 3600
    out <- zoo::na.locf(zoo::na.locf(out, na.rm = FALSE), fromLast = TRUE)
    out
  }

  raw_data[,
    inferred_step_seconds := infer_step_seconds(datetime),
    by = plot_year
  ]
  raw_data[,
    lead_seconds := as.numeric(
      data.table::shift(datetime, type = "lead") - datetime,
      units = "secs"
    ),
    by = plot_year
  ]
  raw_data[
    !is.finite(lead_seconds) | lead_seconds < 0,
    lead_seconds := NA_real_
  ]
  raw_data[,
    fallback_step := {
      lead_for_fallback <- lead_seconds
      if (any(is.finite(lead_for_fallback), na.rm = TRUE)) {
        q90 <- stats::quantile(
          lead_for_fallback,
          probs = 0.9,
          na.rm = TRUE,
          names = FALSE,
          type = 8
        )
        lead_for_fallback <- lead_for_fallback[lead_for_fallback <= q90]
      }

      candidate_step <- suppressWarnings(stats::median(
        c(period_seconds, inferred_step_seconds, lead_for_fallback),
        na.rm = TRUE
      ))
      if (
        !is.finite(candidate_step) ||
          is.na(candidate_step) ||
          candidate_step <= 0
      ) {
        0
      } else {
        candidate_step
      }
    },
    by = plot_year
  ]
  raw_data[,
    step_seconds := data.table::fcoalesce(
      period_seconds,
      inferred_step_seconds,
      fallback_step
    )
  ]
  raw_data[,
    interval_seconds := data.table::fcoalesce(lead_seconds, step_seconds, 0)
  ]
  raw_data[, duration_seconds := pmax(pmin(interval_seconds, step_seconds), 0)]
  raw_data[, coverage_seconds := ifelse(is.na(value), 0, duration_seconds)]

  data.table::setkey(bin_windows, plot_year, bin_start_utc, bin_end_utc)
  raw_data <- bin_windows[
    raw_data,
    on = .(plot_year, bin_start_utc <= datetime, bin_end_utc > datetime),
    nomatch = 0L,
    .(
      plot_year = x.plot_year,
      bin_order = x.bin_order,
      bin_start_utc = x.bin_start_utc,
      bin_end_utc = x.bin_end_utc,
      datetime = i.datetime,
      value = i.value,
      duration_seconds = i.duration_seconds,
      coverage_seconds = i.coverage_seconds
    )
  ]

  agg_data <- raw_data[,
    .(
      value = {
        v <- value
        if (transformation == "sum") {
          if (all(is.na(v))) NA_real_ else sum(v, na.rm = TRUE)
        } else if (transformation == "mean") {
          if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE)
        } else if (transformation == "min") {
          if (all(is.na(v))) NA_real_ else min(v, na.rm = TRUE)
        } else if (transformation == "max") {
          if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE)
        } else if (transformation == "median") {
          if (all(is.na(v))) NA_real_ else stats::median(v, na.rm = TRUE)
        } else {
          valid <- !is.na(v) & !is.na(duration_seconds)
          if (!any(valid)) {
            NA_real_
          } else {
            sum(v[valid] * duration_seconds[valid], na.rm = TRUE)
          }
        }
      },
      covered_seconds = sum(coverage_seconds, na.rm = TRUE)
    ),
    by = .(plot_year, bin_order, bin_start_utc, bin_end_utc)
  ]

  plot_data <- merge(
    bin_windows,
    agg_data,
    by = c("plot_year", "bin_order", "bin_start_utc", "bin_end_utc"),
    all.x = TRUE,
    sort = FALSE
  )
  plot_data[is.na(covered_seconds), covered_seconds := 0]
  plot_data[,
    bin_seconds := as.numeric(bin_end_utc - bin_start_utc, units = "secs")
  ]
  plot_data[,
    completeness := ifelse(
      bin_seconds > 0,
      pmin(covered_seconds / bin_seconds, 1),
      0
    )
  ]
  plot_data[is.na(completeness), completeness := 0]
  plot_data <- plot_data[completeness >= threshold]

  if (nrow(plot_data) == 0) {
    stop(
      "No data found for the requested day-of-year range/years/threshold."
    )
  }

  data.table::setorder(plot_data, bin_order, plot_year)
  plot_data[, plot_year := as.integer(plot_year)]
  plot_data[, bin_start := lubridate::with_tz(bin_start_utc, tzone = tzone)]
  plot_data[, bin_end := lubridate::with_tz(bin_end_utc, tzone = tzone)]
  plot_data[, completeness_text := paste0(round(completeness * 100), "%")]
  plot_data[,
    bin_label := {
      label_end <- bin_end - 1
      if (bin_width_units == "hours") {
        paste0(
          format(bin_start, "%b %d %H:%M"),
          " - ",
          format(label_end, "%b %d %H:%M")
        )
      } else if (bin_width_units == "years") {
        paste0(format(bin_start, "%Y"), " - ", format(label_end, "%Y"))
      } else {
        paste0(format(bin_start, "%b %d"), " - ", format(label_end, "%b %d"))
      }
    }
  ]

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
    paste0(meta$parameter_name[1], " (", transformation, ", ", units_label, ")")
  } else {
    paste0(meta$parameter_name[1], " (", transformation, ")")
  }

  # Determine grouping behaviour: if specific multiple years are requested, group by year.
  plot_data[, year_group := as.integer(plot_year)]
  years_in_result <- sort(unique(plot_data$year_group))
  group_by_year <- length(years_in_result) > 1 && bin_width_units != "years"
  label_angle <- 0
  label_size <- 13

  if (group_by_year) {
    label_map <- plot_data[
      order(year_group, bin_order),
      .(bin_label = bin_label[1]),
      by = bin_order
    ]
    plot_data[label_map, bin_label := i.bin_label, on = "bin_order"]
    plot_data <- plot_data[order(bin_order, year_group)]
    bin_levels <- label_map[order(bin_order), bin_label]
    if (length(unique(plot_data$bin_label)) > 20) {
      label_angle <- -90
      label_size <- 11
    }

    year_levels <- as.character(sort(unique(plot_data$year_group)))
    colors <- rev(grDevices::colorRampPalette(c(
      "#00454e",
      "#7A9A01",
      "#FFA900",
      "#DC4405"
    ))(length(year_levels)))

    # Setting p <- NULL here instead of creating a blank plotly object, because a blank plotly object is by default assigned a continuous trace type. When followed by plotting discrete data, results in a harmeless but annoying warning.
    p <- NULL
    for (i in seq_along(year_levels)) {
      yv <- year_levels[i]
      d <- plot_data[as.character(year_group) == yv]
      if (is.null(p)) {
        p <- plotly::plot_ly(
          data = d,
          x = ~bin_label,
          y = ~value,
          type = "bar",
          text = if (isTRUE(completeness_label)) ~completeness_text else NULL,
          textposition = if (isTRUE(completeness_label)) "outside" else NULL,
          textangle = if (isTRUE(completeness_label)) label_angle else NULL,
          textfont = if (isTRUE(completeness_label)) {
            list(size = label_size)
          } else {
            NULL
          },
          name = yv,
          marker = list(
            color = colors[i],
            line = list(width = 1 * line_scale, color = colors[i])
          )
        )
      } else {
        p <- plotly::add_trace(
          p,
          data = d,
          x = ~bin_label,
          y = ~value,
          text = if (isTRUE(completeness_label)) ~completeness_text else NULL,
          textposition = if (isTRUE(completeness_label)) "outside" else NULL,
          textangle = if (isTRUE(completeness_label)) label_angle else NULL,
          textfont = if (isTRUE(completeness_label)) {
            list(size = label_size)
          } else {
            NULL
          },
          name = yv,
          marker = list(
            color = colors[i],
            line = list(width = 1 * line_scale, color = colors[i])
          )
        )
      }
    }

    p <- plotly::layout(
      p,
      barmode = "group",
      xaxis = list(
        title = list(standoff = 0),
        showgrid = gridx,
        showline = TRUE,
        titlefont = list(size = axis_scale * 14),
        tickfont = list(size = axis_scale * 12),
        ticks = "outside",
        ticklen = 5,
        tickwidth = 1,
        tickcolor = "black",
        categoryorder = "array",
        categoryarray = bin_levels
      ),
      yaxis = list(
        title = list(text = y_title, standoff = 10),
        showgrid = gridy,
        showline = TRUE,
        zeroline = FALSE,
        titlefont = list(size = axis_scale * 14),
        tickfont = list(size = axis_scale * 12),
        ticks = "outside",
        ticklen = 5,
        tickwidth = 1,
        tickcolor = "black"
      ),
      margin = list(b = 0, t = 40 * axis_scale, l = 50 * axis_scale),
      legend = list(
        title = list(text = "Year"),
        font = list(size = legend_scale * 12),
        orientation = legend_position
      ),
      uniformtext = list(minsize = label_size, mode = "show"),
      font = list(family = "Nunito Sans")
    )
  } else {
    if (nrow(plot_data) > 20) {
      label_angle <- -90
      label_size <- 11
    }

    p <- plotly::plot_ly(
      data = plot_data,
      x = ~bin_start,
      y = ~value,
      type = "bar",
      text = if (isTRUE(completeness_label)) ~completeness_text else NULL,
      textposition = if (isTRUE(completeness_label)) "outside" else NULL,
      textangle = if (isTRUE(completeness_label)) label_angle else NULL,
      textfont = if (isTRUE(completeness_label)) {
        list(size = label_size)
      } else {
        NULL
      },
      marker = list(
        color = "#00454e",
        line = list(width = 1 * line_scale, color = "#00454e")
      )
    ) |>
      plotly::layout(
        xaxis = list(
          title = list(standoff = 0),
          showgrid = gridx,
          showline = TRUE,
          titlefont = list(size = axis_scale * 14),
          tickfont = list(size = axis_scale * 12),
          ticks = "outside",
          ticklen = 5,
          tickwidth = 1,
          tickcolor = "black"
        ),
        yaxis = list(
          title = list(text = y_title, standoff = 10),
          showgrid = gridy,
          showline = TRUE,
          zeroline = FALSE,
          titlefont = list(size = axis_scale * 14),
          tickfont = list(size = axis_scale * 12),
          ticks = "outside",
          ticklen = 5,
          tickwidth = 1,
          tickcolor = "black"
        ),
        margin = list(b = 0, t = 40 * axis_scale, l = 50 * axis_scale),
        legend = list(
          font = list(size = legend_scale * 12),
          orientation = legend_position
        ),
        uniformtext = list(minsize = label_size, mode = "show"),
        font = list(family = "Nunito Sans")
      )
  }

  as_of_title <- format_as_of_title(as_of, tzone, lang)

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
    p <- plotly::layout(
      p,
      title = list(
        text = if (is.null(as_of_title)) {
          custom_title
        } else {
          paste0(custom_title, "<br><sup>", as_of_title, "</sup>")
        },
        x = 0.05,
        xref = "container",
        font = list(
          size = axis_scale * 18,
          family = "Nunito Sans",
          color = "#000000"
        )
      )
    )
  }
  p <- p |>
    plotly::config(locale = lang)

  if (isTRUE(data)) {
    return(list(plot = p, plot_data = plot_data, raw_data = db_data))
  }

  return(p)
}
