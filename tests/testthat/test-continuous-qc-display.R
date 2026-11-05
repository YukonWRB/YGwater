test_that("singleton QC readings get visible bands without changing source intervals", {
  t <- as.POSIXct("2024-10-16 10:00:00", tz = "America/Whitehorse")
  intervals <- data.table::data.table(
    start_dt = t + c(-3600, 0, 3600, 7200),
    end_dt = t + c(-3600, 0, 3600, 7200),
    qc_type_code = c("UNS", "ERROR", "UNS", "ERROR"),
    color_code = c("#808080", "#fe3200", "#808080", "#fe3200")
  )
  original <- data.table::copy(intervals)
  display <- continuous_qc_display_intervals(intervals, 3600, "qc_type_code")

  expect_equal(intervals, original)
  expect_equal(as.numeric(display$end_dt - display$start_dt, units = "secs"),
               rep(3600, 4))
  expect_equal(display$end_dt[-4], display$start_dt[-1])
  expect_equal(display$start_dt[2], t - 1800)
  expect_equal(display$end_dt[2], t + 1800)
})

test_that("equal QC codes merge only across adjacent readings", {
  t <- as.POSIXct("2024-10-16", tz = "UTC")
  intervals <- data.table::data.table(
    start_dt = t + c(0, 3600, 18000),
    end_dt = t + c(0, 7200, 18000),
    qc_type_code = "UNS",
    color_code = "#808080"
  )
  display <- continuous_qc_display_intervals(intervals, 3600, "qc_type_code")

  expect_equal(nrow(display), 2L)
  expect_equal(display$start_dt, t + c(-1800, 16200))
  expect_equal(display$end_dt, t + c(9000, 19800))
})

test_that("touching QC intervals retain their boundary and smaller gaps meet midway", {
  t <- as.POSIXct("2024-10-16", tz = "UTC")
  intervals <- data.table::data.table(
    start_dt = t + c(0, 3600, 5700),
    end_dt = t + c(3600, 5400, 7200),
    qc_type_code = c("UNS", "ERROR", "UNS"),
    color_code = c("#808080", "#fe3200", "#808080")
  )
  display <- continuous_qc_display_intervals(intervals, 3600, "qc_type_code")

  expect_equal(display$end_dt[1], t + 3600)
  expect_equal(display$start_dt[2], t + 3600)
  expect_equal(display$end_dt[2], t + 5550)
  expect_equal(display$start_dt[3], t + 5550)
})

test_that("empty QC and missing recording cadence are handled without inventing widths", {
  intervals <- data.table::data.table(
    start_dt = as.POSIXct("2024-10-16", tz = "UTC"),
    end_dt = as.POSIXct("2024-10-16", tz = "UTC"),
    qc_type_code = "ERROR",
    color_code = "#fe3200"
  )
  expect_equal(continuous_qc_display_intervals(intervals[0], 3600, "qc_type_code"),
               intervals[0])
  for (cadence in list(NULL, NA_real_, 0, -1, Inf)) {
    expect_equal(continuous_qc_display_intervals(intervals, cadence, "qc_type_code"),
                 intervals)
  }
})

test_that("adaptive and conventional status ribbons use the same native cadence", {
  t <- as.POSIXct("2024-10-16", tz = "UTC")
  context <- data.frame(
    timeseries_id = 1223L, location_id = 294L, parameter_id = 1202L,
    start_datetime = t, end_datetime = t + 7200,
    param_name = "wind direction", param_name_fr = "wind direction",
    plot_default_y_orientation = "normal", location_name_en = "Test station",
    location_name_fr = "Test station", units = "degrees", z = NA_real_,
    aggregation_type = "mean", record_rate_seconds = 900
  )
  trace <- data.table::data.table(datetime = t + 0:2 * 3600,
                                 value = c(1, 2, 3), imputed = FALSE)
  testthat::local_mocked_bindings(
    fetch_continuous_qc_intervals = function(..., qc_type, as_of) {
      data.table::data.table(
        start_dt = t + c(0, 3600, 4500),
        end_dt = t + c(2700, 3600, 7200),
        qc_type_code = c("UNS", "ERROR", "UNS"),
        qc_type_description = c("Unspecified", "Instrument error", "Unspecified"),
        qc_type_description_fr = c("Unspecified", "Instrument error", "Unspecified"),
        color_code = c("#808080", "#fe3200", "#808080")
      )
    },
    .package = "YGwater"
  )
  args <- list(timeseries_id = 1223L, start_date = t, end_date = t + 7200,
               tzone = "UTC", historic_range = FALSE, grades = TRUE,
               approvals = TRUE, qualifiers = TRUE, data = TRUE,
               preprocessed_trace_data = trace, preloaded_timeseries_context = context,
               con = structure(list(), class = "mock_connection"))
  adaptive <- do.call(plotTimeseries, c(args, list(build_plot = FALSE)))
  polygons <- adaptive$meta$status_bands$polygons
  for (prefix in c("grade", "approval", "qualifier")) {
    flag <- polygons[polygons$id == paste0(prefix, "_2")]
    expect_equal(range(as.numeric(flag$datetime)), as.numeric(t) + c(3150, 4050))
  }
  expect_equal(adaptive$data$trace_data$value, trace$value)

  # The standalone plot and its static exports must match adaptive ribbons.
  conventional <- do.call(plotTimeseries, c(args, list(build_plot = TRUE)))
  built <- suppressWarnings(plotly::plotly_build(conventional$plot))
  ribbons <- Filter(function(x) identical(x$fill, "toself"), built$x$data)
  flags <- Filter(function(x) any(grepl("Instrument error", x$text)), ribbons)
  expect_length(flags, 3L)
  for (flag in flags) {
    # plotly serializes POSIXct coordinates as milliseconds since the epoch.
    coords <- if (is.numeric(flag$x)) {
      as.numeric(flag$x) / 1000
    } else {
      as.numeric(as.POSIXct(flag$x, tz = "UTC"))
    }
    expect_equal(range(coords), as.numeric(t) + c(3150, 4050))
  }
})

test_that("display padding cannot extend no-use grade filtering", {
  t <- as.POSIXct("2024-10-16", tz = "UTC")
  context <- data.frame(
    timeseries_id = 1223L, location_id = 294L, parameter_id = 1202L,
    start_datetime = t, end_datetime = t + 7200,
    param_name = "wind direction", param_name_fr = "wind direction",
    plot_default_y_orientation = "normal", location_name_en = "Test station",
    location_name_fr = "Test station", units = "degrees", z = NA_real_,
    record_rate_seconds = 3600
  )
  # Readings close to both sides of the singleton are inside its display band,
  # but only the reading at the stored timestamp should be excluded.
  trace <- data.table::data.table(
    datetime = t + c(0, 3000, 3600, 4200, 7200),
    value = 1:5, imputed = FALSE
  )
  testthat::local_mocked_bindings(
    fetch_continuous_qc_intervals = function(...) data.table::data.table(
      start_dt = t + 3600, end_dt = t + 3600, qc_type_code = "N",
      qc_type_description = "No use", qc_type_description_fr = "No use",
      color_code = "#fe3200"
    ),
    fetch_hourly_trace_data = function(...) data.table::copy(trace),
    continuous_trace_uses_corrected_source = function(...) FALSE,
    .package = "YGwater"
  )
  out <- plotTimeseries(
    timeseries_id = 1223L, start_date = t, end_date = t + 7200,
    tzone = "UTC", resolution = "hour", historic_range = FALSE,
    grades = TRUE, data = TRUE, build_plot = FALSE,
    preloaded_timeseries_context = context,
    con = structure(list(), class = "mock_connection")
  )
  actual <- out$data$trace_data
  expect_equal(actual$value[match(trace$datetime, actual$datetime)],
               c(1, 2, NA, 4, 5))
  expect_equal(range(as.numeric(out$meta$status_bands$polygons$datetime)),
               as.numeric(t) + c(1800, 5400))
})
