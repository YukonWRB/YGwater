# Note: these tests depend on installation of Python and a few libraries. This is taken care of in the setup.R file within the testthat folder.

con <- AquaConnect(silent = TRUE)
on.exit(DBI::dbDisconnect(con), add = TRUE)

test_that("timeseries plot is as expected for one year with no historic range or slider", {
  skip_on_cran()
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test1.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotTimeseries(
    location = "09EA004",
    parameter = "water level",
    start_date = "2022-01-01",
    end_date = "2023-01-01",
    historic_range = FALSE,
    slider = FALSE,
    con = con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("timeseries plot is as expected for one year with no historic range", {
  skip_on_cran()
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test2.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotTimeseries(
    location = "09EA004",
    parameter = "water level",
    start_date = "2022-01-01",
    end_date = "2023-01-01",
    historic_range = FALSE,
    con = con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("timeseries plot is as expected for one year with historic range", {
  skip_on_cran()
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test3.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotTimeseries(
    location = "09EA004",
    parameter = "water level",
    start_date = "2022-01-01",
    end_date = "2023-01-01",
    con = con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("French timeseries plot is as expected for one year with historic range and slider", {
  skip_on_cran()
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test4.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotTimeseries(
    location = "09EA004",
    parameter = "water level",
    start_date = "2022-01-01",
    end_date = "2023-01-01",
    lang = "fr",
    con = con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("French timeseries plot is as expected for one year with historic range and no slider", {
  skip_on_cran()
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test5.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotTimeseries(
    location = "09EA004",
    parameter = "water level",
    start_date = "2022-01-01",
    end_date = "2023-01-01",
    lang = "fr",
    slider = FALSE,
    con = con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("grades, approvals, qualifiers are displayed", {
  skip_on_cran()
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test6.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotTimeseries(
    location = "09EA004",
    parameter = "water level",
    start_date = "2022-01-01",
    end_date = "2023-01-01",
    lang = "fr",
    slider = FALSE,
    grades = TRUE,
    qualifiers = TRUE,
    approvals = TRUE,
    con = con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("one of grades, approvals, qualifiers is displayed", {
  skip_on_cran()
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test7.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotTimeseries(
    location = "09EA004",
    parameter = "water level",
    start_date = "2022-01-01",
    end_date = "2023-01-01",
    lang = "fr",
    slider = FALSE,
    grades = TRUE,
    con = con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("returned plot data is as expected", {
  skip_on_cran()

  plot <- plotTimeseries(
    location = "09EA004",
    parameter = "water level",
    start_date = "2022-01-01",
    end_date = "2023-01-01",
    lang = "fr",
    slider = FALSE,
    data = TRUE,
    con = con
  )$data
  expect_type(plot, "list")
  expect_named(plot, c("trace_data", "range_data"))
  expect_named(plot$trace_data, c("datetime", "value", "imputed"))
  expect_named(plot$range_data, c("datetime", "min", "max", "q75", "q25"))
})

test_that("returned plot data is as expected with all parameters specified", {
  skip_on_cran()

  plot <- plotTimeseries(
    location = "09EA004",
    sub_location = NULL,
    parameter = "water level",
    record_rate = 5 * 60, # 5 minutes for WSC data
    aggregation_type = "instantaneous",
    z = NULL,
    z_approx = NULL,
    start_date = "2022-01-01",
    end_date = "2023-01-01",
    lang = "fr",
    slider = FALSE,
    data = TRUE,
    datum = FALSE,
    custom_title = NULL,
    filter = 20,
    unusable = FALSE,
    grades = TRUE,
    approvals = TRUE,
    qualifiers = TRUE,
    historic_range = TRUE,
    con = con
  )$data
  expect_type(plot, "list")
  expect_named(plot, c("trace_data", "range_data"))
  expect_named(plot$trace_data, c("datetime", "value", "imputed"))
  expect_named(plot$range_data, c("datetime", "min", "max", "q75", "q25"))
})

test_that("plotTimeseries works when given only a timeseries_id", {
  skip_on_cran()

  # Find an appropriate timeseries_id
  wl <- DBI::dbGetQuery(
    con,
    "SELECT parameter_id FROM parameters WHERE param_name = 'water level' LIMIT 1;"
  )[1, 1]
  loc_id <- DBI::dbGetQuery(
    con,
    "SELECT location_id FROM locations WHERE location_code = '09EA004';"
  )[1, 1]
  tsid <- DBI::dbGetQuery(
    con,
    "SELECT timeseries_id FROM timeseries WHERE parameter_id = $1 AND location_id = $2;",
    params = list(wl, loc_id)
  )[1, 1]
  plot <- plotTimeseries(
    timeseries_id = tsid,
    start_date = "2022-01-01",
    end_date = "2023-01-01",
    historic_range = TRUE,
    slider = FALSE,
    data = TRUE,
    con = con
  )
  expect_s3_class(plot$plot, "plotly")
  expect_named(plot$data, c("trace_data", "range_data"))
  expect_named(plot$data$trace_data, c("datetime", "value", "imputed"))
  expect_named(plot$data$range_data, c("datetime", "min", "max", "q75", "q25"))

  # Skip snapshot test on CI
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test8.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plotly::save_image(plot$plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

# Test that plotTimeseries plots raw and uncorrected data
test_that("plotTimeseries plots raw and corrected data", {
  skip_on_cran()

  # Find an appropriate timeseries_id
  wl <- DBI::dbGetQuery(
    con,
    "SELECT parameter_id FROM parameters WHERE param_name = 'water level' LIMIT 1;"
  )[1, 1]
  loc_id <- DBI::dbGetQuery(
    con,
    "SELECT location_id FROM locations WHERE location_code = '09EA004';"
  )[1, 1]
  tsid <- DBI::dbGetQuery(
    con,
    "SELECT timeseries_id FROM timeseries WHERE parameter_id = $1 AND location_id = $2;",
    params = list(wl, loc_id)
  )[1, 1]
  plot <- plotTimeseries(
    timeseries_id = tsid,
    start_date = "2022-01-01",
    end_date = "2023-01-01",
    historic_range = TRUE,
    slider = FALSE,
    data = TRUE,
    raw = TRUE,
    con = con
  )
  expect_s3_class(plot$plot, "plotly")
  expect_named(plot$data, c("trace_data", "range_data"))
  expect_named(
    plot$data$trace_data,
    c("datetime", "value", "value_raw", "imputed")
  )
  expect_named(plot$data$range_data, c("datetime", "min", "max", "q75", "q25"))

  # Skip snapshot test on CI
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test9.png")
  path <- pathPrep(path)

  on.exit(unlink(path), add = TRUE)
  plotly::save_image(plot$plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("plotTimeseries hourly resolution uses timeseries aggregation logic", {
  skip_on_cran()

  wl <- DBI::dbGetQuery(
    con,
    "SELECT parameter_id FROM parameters WHERE param_name = 'water level' LIMIT 1;"
  )[1, 1]
  loc_id <- DBI::dbGetQuery(
    con,
    "SELECT location_id FROM locations WHERE location_code = '09EA004';"
  )[1, 1]
  tsid <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT ts.timeseries_id FROM timeseries ts ",
      "LEFT JOIN aggregation_types at ",
      "ON ts.aggregation_type_id = at.aggregation_type_id ",
      "WHERE ts.parameter_id = $1 AND ts.location_id = $2 ",
      "AND at.aggregation_type IN ('instantaneous', 'mean') ",
      "LIMIT 1;"
    ),
    params = list(wl, loc_id)
  )[1, 1]

  start_dt <- as.POSIXct("2022-06-01 00:00:00", tz = "UTC")
  end_dt <- as.POSIXct("2022-06-02 23:59:59", tz = "UTC")

  agg_type <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT at.aggregation_type FROM timeseries ts ",
      "LEFT JOIN aggregation_types at ",
      "ON ts.aggregation_type_id = at.aggregation_type_id ",
      "WHERE ts.timeseries_id = $1;"
    ),
    params = list(tsid)
  )[1, 1]

  source <- dbGetQueryDT(
    con,
    paste0(
      "SELECT datetime, value_corrected, imputed ",
      "FROM measurements_continuous_corrected ",
      "WHERE timeseries_id = $1 AND datetime BETWEEN $2 AND $3 ",
      "ORDER BY datetime;"
    ),
    params = list(tsid, start_dt, end_dt)
  )

  source[,
    datetime_hour := as.POSIXct(
      format(datetime, "%Y-%m-%d %H:00:00", tz = "UTC"),
      tz = "UTC"
    )
  ]
  expected <- source[,
    .(
      value = if (agg_type == "sum") {
        sum(value_corrected, na.rm = TRUE)
      } else if (agg_type == "median") {
        stats::median(value_corrected, na.rm = TRUE)
      } else if (agg_type %in% c("min", "minimum")) {
        min(value_corrected, na.rm = TRUE)
      } else if (agg_type %in% c("max", "maximum")) {
        max(value_corrected, na.rm = TRUE)
      } else if (agg_type == "(min+max)/2") {
        mean(c(
          min(value_corrected, na.rm = TRUE),
          max(value_corrected, na.rm = TRUE)
        ))
      } else {
        mean(value_corrected, na.rm = TRUE)
      },
      imputed = any(imputed)
    ),
    by = datetime_hour
  ]
  data.table::setnames(expected, "datetime_hour", "datetime")
  data.table::setorder(expected, datetime)

  out <- plotTimeseries(
    timeseries_id = tsid,
    start_date = start_dt,
    end_date = end_dt,
    resolution = "hour",
    tzone = "UTC",
    slider = FALSE,
    historic_range = FALSE,
    data = TRUE,
    con = con
  )$data$trace_data

  out <- out[
    lubridate::minute(datetime) == 0 & lubridate::second(datetime) == 0,
    .(datetime, value, imputed)
  ]
  data.table::setorder(out, datetime)

  expect_equal(out$datetime, expected$datetime)
  expect_equal(out$value, expected$value, tolerance = 1e-8)
  expect_equal(out$imputed, expected$imputed)
})
