# Note: these tests depend on installation of Python and a few libraries.
# This is taken care of in the setup.R file within the testthat folder.

con <- AquaConnect(silent = TRUE)
on.exit(DBI::dbDisconnect(con), add = TRUE)

tsid <- DBI::dbGetQuery(
  con,
  "SELECT timeseries_id FROM timeseries WHERE parameter_id = (SELECT parameter_id FROM parameters WHERE param_name = 'water level') AND location_id = (SELECT location_id FROM locations WHERE location_code = '09EA004') LIMIT 1"
)$timeseries_id

test_that("plotTimeseriesHistogram validates inputs", {
  expect_error(
    plotTimeseriesHistogram(
      timeseries_id = tsid,
      startDay = 0,
      endDay = 365
    ),
    "startDay must resolve to an integer day-of-year between 1 and 366."
  )

  expect_error(
    plotTimeseriesHistogram(
      timeseries_id = tsid,
      startDay = 1,
      endDay = 365,
      threshold = 1.1
    ),
    "between 0 and 1"
  )

  expect_error(
    plotTimeseriesHistogram(
      timeseries_id = tsid,
      startDay = 1,
      endDay = 365,
      tzone = 1.5
    ),
    "whole hour"
  )
})

test_that("plotTimeseriesHistogram returns plot/data structure", {
  skip_on_cran()

  test_year <- 2022
  out <- plotTimeseriesHistogram(
    timeseries_id = tsid,
    startDay = 31,
    endDay = 180,
    years = test_year,
    tzone = "UTC",
    data = TRUE,
    con = con
  )

  expect_type(out, "list")
  expect_named(out, c("plot", "plot_data", "raw_data"))
  expect_s3_class(out$plot, "plotly")
  expect_true(
    all(
      c(
        "plot_year",
        "bin_order",
        "bin_start",
        "bin_end",
        "value",
        "completeness",
        "bin_label"
      ) %in%
        names(out$plot_data)
    )
  )
  expect_true(all(c("datetime", "value") %in% names(out$raw_data)))
})

test_that("weekly bins start at startDay and end on full-bin boundaries", {
  skip_on_cran()

  yrs <- c(2022, 2023)
  out <- plotTimeseriesHistogram(
    timeseries_id = tsid,
    startDay = 31,
    endDay = 180,
    bin_width = 1,
    bin_width_units = "weeks",
    years = yrs,
    tzone = "UTC",
    data = TRUE,
    con = con
  )
  pd <- out$plot_data

  expect_true(all(grepl(" - ", pd$bin_label, fixed = TRUE)))

  for (y in yrs) {
    pd_y <- pd[pd$plot_year == y, ]
    expect_true(nrow(pd_y) > 0)

    expected_start <- as.POSIXct(
      sprintf("%04d-01-31 00:00:00", y),
      tz = "UTC"
    )
    observed_start <- min(pd_y$bin_start_utc)
    expect_equal(observed_start, expected_start)

    requested_end_exclusive <- as.POSIXct(
      sprintf("%04d-01-01 00:00:00", y),
      tz = "UTC"
    ) +
      lubridate::days(180)
    observed_end <- max(pd_y$bin_end_utc)
    expect_true(observed_end >= requested_end_exclusive)
    expect_true(observed_end < requested_end_exclusive + lubridate::days(7))
  }
})

test_that("overlap day-of-year mode works across year boundary", {
  skip_on_cran()

  yrs <- c(2022, 2023)
  out <- plotTimeseriesHistogram(
    timeseries_id = tsid,
    startDay = 335,
    endDay = 31,
    bin_width = 1,
    bin_width_units = "weeks",
    years = yrs,
    tzone = "UTC",
    data = TRUE,
    con = con
  )
  pd <- out$plot_data

  expect_setequal(sort(unique(pd$plot_year)), sort(yrs))
  expect_true(nrow(pd) > 0)

  for (y in yrs) {
    expected_start <- as.POSIXct(
      sprintf("%04d-01-01 00:00:00", y),
      tz = "UTC"
    ) +
      lubridate::days(334)
    observed_start <- min(pd[pd$plot_year == y, ]$bin_start_utc)
    expect_equal(observed_start, expected_start)
  }
})

test_that("integral transformation returns non-zero values", {
  skip_on_cran()

  test_year <- 2022
  out <- plotTimeseriesHistogram(
    timeseries_id = tsid,
    startDay = 31,
    endDay = 180,
    years = test_year,
    transformation = "integral",
    tzone = "UTC",
    data = TRUE,
    con = con
  )

  vals <- out$plot_data$value
  expect_true(is.numeric(vals))
  expect_true(any(!is.na(vals)))
  expect_true(any(abs(vals) > 0, na.rm = TRUE))
})

test_that("style parameters and timezone are applied", {
  skip_on_cran()

  yrs <- c(2022, 2023)
  out <- plotTimeseriesHistogram(
    timeseries_id = tsid,
    startDay = 31,
    endDay = 180,
    years = yrs,
    bin_width = 1,
    bin_width_units = "weeks",
    line_scale = 2,
    axis_scale = 1.5,
    legend_scale = 1.25,
    legend_position = "h",
    gridx = TRUE,
    gridy = TRUE,
    tzone = -7,
    data = TRUE,
    con = con
  )

  built <- plotly::plotly_build(out$plot)
  lay <- built$x$layout

  expect_true(isTRUE(lay$xaxis$showgrid))
  expect_true(isTRUE(lay$yaxis$showgrid))
  expect_equal(lay$legend$orientation, "h")
  expect_equal(as.numeric(lay$legend$font$size), 1.25 * 12, tolerance = 1e-8)
  expect_equal(as.numeric(lay$xaxis$titlefont$size), 1.5 * 14, tolerance = 1e-8)
  expect_equal(attr(out$plot_data$bin_start, "tzone"), "Etc/GMT+7")
})

test_that("threshold and completeness fields are coherent", {
  skip_on_cran()

  test_year <- 2022
  out <- plotTimeseriesHistogram(
    timeseries_id = tsid,
    startDay = 31,
    endDay = 180,
    years = test_year,
    threshold = 0.25,
    completeness_label = TRUE,
    tzone = "UTC",
    data = TRUE,
    con = con
  )
  pd <- out$plot_data

  expect_true(all(pd$completeness >= 0.25 - 1e-9))
  expect_true(all(pd$completeness >= 0 & pd$completeness <= 1))
  expect_true(all(grepl("%$", pd$completeness_text)))
})

test_that("plotTimeseriesHistogram can show data in the past", {
  skip_on_ci() # Because the CI instance would not have the necessary historical data
  skip_on_cran()

  if (
    !isTRUE(DBI::dbGetQuery(
      con,
      "SELECT has_schema_privilege(current_user, 'audit', 'USAGE') AS ok;"
    )$ok[[1]])
  ) {
    skip("Historical queries require USAGE on schema audit.")
  }

  as_of <- as.POSIXct("2026-03-30 12:00:00", tz = "UTC")
  start_dt <- as.POSIXct("2022-01-31 00:00:00", tz = "UTC")
  end_dt <- as.POSIXct("2022-02-10 00:00:00", tz = "UTC")

  out <- plotTimeseriesHistogram(
    timeseries_id = tsid,
    startDay = 31,
    endDay = 40,
    years = 2022,
    tzone = "UTC",
    data = TRUE,
    con = con,
    as_of = as_of
  )
  expect_match(plotly::plotly_build(out$plot)$x$layout$title$text, "As of")
})
