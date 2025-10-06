# Note: these tests depend on installation of Python and a few libraries. This is taken care of in the setup.R file within the testthat folder.

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
    slider = FALSE
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
    historic_range = FALSE
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
    end_date = "2023-01-01"
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
    lang = "fr"
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
    slider = FALSE
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
    approvals = TRUE
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
    grades = TRUE
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
    data = TRUE
  )$data
  expect_type(plot, "list")
  expect_named(plot, c("trace_data", "range_data"))
  expect_named(plot$trace_data, c("datetime", "value"))
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
    historic_range = TRUE
  )$data
  expect_type(plot, "list")
  expect_named(plot, c("trace_data", "range_data"))
  expect_named(plot$trace_data, c("datetime", "value"))
  expect_named(plot$range_data, c("datetime", "min", "max", "q75", "q25"))
})
