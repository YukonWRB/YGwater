# Note: these tests depend on installation of Python and a few libraries. This is taken care of in the setup.R file within the testthat folder.

con <- AquaConnect(silent = TRUE)
on.exit(DBI::dbDisconnect(con), add = TRUE)

skip_on_cran()

test_that("plotOverlap with all defaults", {
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test1.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotOverlap(
    location = "09EA004",
    parameter = "water level",
    years = 2020,
    con = con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("plotOverlap with minimal elements", {
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test2.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotOverlap(
    location = "09EA004",
    parameter = "water level",
    years = 2020,
    hover = FALSE,
    slider = FALSE,
    gridx = FALSE,
    gridy = FALSE,
    con = con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("plotOverlap with multiple years and 'last' historic range", {
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test3.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotOverlap(
    location = "09AB004",
    parameter = "water level",
    years = c(2022, 2023),
    hover = FALSE,
    slider = FALSE,
    gridx = FALSE,
    gridy = FALSE,
    historic_range = "last",
    con = con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("plotOverlap with multiple years and 'last' historic range", {
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test4.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotOverlap(
    location = "09AB004",
    parameter = "water level",
    years = c(2022, 2023),
    hover = FALSE,
    slider = FALSE,
    gridx = FALSE,
    gridy = FALSE,
    historic_range = "all",
    con = con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("returned plot data is as expected", {
  plot <- plotOverlap(
    location = "09AB004",
    parameter = "water level",
    years = c(2022, 2023),
    hover = FALSE,
    slider = FALSE,
    gridx = FALSE,
    gridy = FALSE,
    historic_range = "all",
    data = TRUE,
    con = con
  )$data
  expect_type(plot, "list")
  expect_named(plot, c("trace_data", "range_data"))
  expect_named(
    plot$trace_data,
    c("datetime", "value", "year", "month", "day", "plot_year", "plot_datetime")
  )
  expect_named(
    plot$range_data,
    c("datetime", "value", "max", "min", "q75", "q25", "year", "month", "day")
  )
})

test_that("plotOverlap works when given only a timeseries_id", {
  # Find an appropriate timeseries_id
  wl <- DBI::dbGetQuery(
    con,
    "SELECT parameter_id FROM parameters WHERE param_name = 'water level' LIMIT 1;"
  )[1, 1]
  loc_id <- DBI::dbGetQuery(
    con,
    "SELECT location_id FROM locations WHERE location_code = '09AB004';"
  )[1, 1]
  tsid <- DBI::dbGetQuery(
    con,
    "SELECT timeseries_id FROM timeseries WHERE parameter_id = $1 AND location_id = $2;",
    params = list(wl, loc_id)
  )[1, 1]
  plot <- plotOverlap(
    timeseries_id = tsid,
    years = c(2022, 2023),
    hover = FALSE,
    slider = FALSE,
    gridx = FALSE,
    gridy = FALSE,
    historic_range = "all",
    data = TRUE,
    con = con
  )
  expect_s3_class(plot$plot, "plotly")
  expect_named(plot$data, c("trace_data", "range_data"))
  expect_named(
    plot$data$trace_data,
    c("datetime", "value", "year", "month", "day", "plot_year", "plot_datetime")
  )
  expect_named(
    plot$data$range_data,
    c("datetime", "value", "max", "min", "q75", "q25", "year", "month", "day")
  )

  skip_on_ci()
  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test5.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plotly::save_image(plot$plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})


test_that("plotOverlap works with no historic range", {
  # Find an appropriate timeseries_id
  wl <- DBI::dbGetQuery(
    con,
    "SELECT parameter_id FROM parameters WHERE param_name = 'water level' LIMIT 1;"
  )[1, 1]
  loc_id <- DBI::dbGetQuery(
    con,
    "SELECT location_id FROM locations WHERE location_code = '09AB004';"
  )[1, 1]
  tsid <- DBI::dbGetQuery(
    con,
    "SELECT timeseries_id FROM timeseries WHERE parameter_id = $1 AND location_id = $2;",
    params = list(wl, loc_id)
  )[1, 1]
  plot <- plotOverlap(
    timeseries_id = tsid,
    years = c(2022, 2023),
    hover = FALSE,
    slider = FALSE,
    gridx = FALSE,
    gridy = FALSE,
    historic_range = "none",
    data = TRUE,
    con = con
  )
  expect_s3_class(plot$plot, "plotly")
  expect_named(plot$data, c("trace_data", "range_data"))
  expect_null(plot$data$range_data) # range_data should be NULL

  expect_named(
    plot$data$trace_data,
    c("datetime", "value", "year", "month", "day", "plot_year", "plot_datetime")
  )

  skip_on_ci()
  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test6.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plotly::save_image(plot$plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("plotOverlap accepts hourly resolution", {
  plot <- plotOverlap(
    location = "09AB004",
    parameter = "water level",
    startDay = "2022-06-01",
    endDay = "2022-06-02",
    years = 2022,
    historic_range = "none",
    resolution = "hour",
    hover = FALSE,
    slider = FALSE,
    data = TRUE,
    con = con
  )$data

  expect_named(plot, c("trace_data", "range_data"))
  expect_null(plot$range_data)
  expect_gt(nrow(plot$trace_data), 10)
  expect_true(any(lubridate::hour(plot$trace_data$datetime) != 0))
})

test_that("plotOverlap can show data in the past", {
  skip_on_ci() # Because the CI instance would not have the necessary historical data
  if (
    !isTRUE(DBI::dbGetQuery(
      con,
      "SELECT has_schema_privilege(current_user, 'audit', 'USAGE') AS ok;"
    )$ok[[1]])
  ) {
    skip("Historical queries require USAGE on schema audit.")
  }

  wl <- DBI::dbGetQuery(
    con,
    "SELECT parameter_id FROM parameters WHERE param_name = 'water level' LIMIT 1;"
  )[1, 1]
  loc_id <- DBI::dbGetQuery(
    con,
    "SELECT location_id FROM locations WHERE location_code = '09AB004';"
  )[1, 1]
  tsid <- DBI::dbGetQuery(
    con,
    "SELECT timeseries_id FROM timeseries WHERE parameter_id = $1 AND location_id = $2 LIMIT 1;",
    params = list(wl, loc_id)
  )[1, 1]

  as_of <- as.POSIXct("2026-03-30 12:00:00", tz = "UTC")
  start_dt <- as.POSIXct("2022-06-01 00:00:00", tz = "UTC")
  end_dt <- as.POSIXct("2022-06-02 23:59:59", tz = "UTC")

  out <- plotOverlap(
    timeseries_id = tsid,
    startDay = "2022-06-01",
    endDay = "2022-06-02",
    tzone = "UTC",
    years = 2022,
    datum = FALSE,
    hover = FALSE,
    slider = FALSE,
    unusable = TRUE,
    historic_range = "none",
    resolution = "hour",
    data = TRUE,
    con = con,
    as_of = as_of
  )
  expect_match(plotly::plotly_build(out$plot)$x$layout$title$text, "As of")
})
