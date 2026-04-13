# Note: these tests depend on installation of Python and a few libraries. This is taken care of in the setup.R file within the testthat folder.

skip_on_cran()

# Note: these tests depend on installation of Python and a few libraries. This is taken care of in the setup.R file within the testthat folder.

test_that("plotMultiTimeseries with all defaults is as expected", {
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test1.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotMultiTimeseries(
    locations = c("09EA004", "09EA004"),
    parameters = c(1165, 1150),
    start_date = "2021-01-01",
    end_date = "2022-01-01",
    datum = FALSE
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("plotMultiTimeseries returns data as expected", {
  plot <- plotMultiTimeseries(
    locations = c("09EA004", "09EA004"),
    parameters = c(1165, 1150),
    start_date = "2021-01-01",
    end_date = "2022-01-01",
    data = TRUE,
    datum = FALSE
  )$data
  expect_type(plot, "list")
  expect_equal(length(plot), 2) # should have two elements, for both timeseries plotted
  expect_equal(length(plot$`09EA004_1165`), 2) # Should have the trace_data and range_data data.tables
  expect_named(plot$`09EA004_1165`, c("range_data", "trace_data"))
  expect_named(plot$`09EA004_1165`$trace_data, c("datetime", "value"))
  expect_named(
    plot$`09EA004_1165`$range_data,
    c("datetime", "min", "max", "q75", "q25")
  )
})

test_that("plotMultiTimeseries accepts hourly resolution", {
  # Expect a warning about datums not being applied
  expect_warning(
    plot <- plotMultiTimeseries(
      locations = c("09EA004", "09EA004"),
      parameters = c(1165, 1150),
      start_date = "2022-06-01",
      end_date = "2022-06-03",
      resolution = "hour",
      historic_range = TRUE,
      data = TRUE
    )$data,
    "^Datum.*meters.+$"
  )

  expect_equal(length(plot), 2)
  expect_named(plot$`09EA004_1165`, c("range_data", "trace_data"))
  expect_named(plot$`09EA004_1165`$trace_data, c("datetime", "value"))
  expect_gt(nrow(plot$`09EA004_1165`$trace_data), 10)
})

test_that("plotMultiTimeseries can show data in the past", {
  skip_on_ci() # Because the CI instance would not have the necessary historical data
  con <- AquaConnect(silent = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (
    !isTRUE(DBI::dbGetQuery(
      con,
      "SELECT has_schema_privilege(current_user, 'audit', 'USAGE') AS ok;"
    )$ok[[1]])
  ) {
    skip("Historical queries require USAGE on schema audit.")
  }

  tsid <- DBI::dbGetQuery(
    con,
    "SELECT timeseries_id FROM timeseries WHERE parameter_id = (SELECT parameter_id FROM parameters WHERE param_name = 'water level') AND location_id = (SELECT location_id FROM locations WHERE location_code = '09EA004') LIMIT 1;"
  )$timeseries_id[[1]]

  as_of <- as.POSIXct("2026-03-30 12:00:00", tz = "UTC")
  start_dt <- as.POSIXct("2022-06-01 00:00:00", tz = "UTC")
  end_dt <- as.POSIXct("2022-06-02 23:59:59", tz = "UTC")

  out <- plotMultiTimeseries(
    locations = "09EA004",
    parameters = 1165,
    start_date = start_dt,
    end_date = end_dt,
    resolution = "hour",
    historic_range = TRUE,
    tzone = "UTC",
    data = TRUE,
    con = con,
    as_of = as_of
  )$data[[1]]

  expect_gt(nrow(out$trace_data), 48)
})
