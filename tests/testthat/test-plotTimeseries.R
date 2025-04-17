# Tests depend on snapshots so can't be inspected on CRAN or CI
skip_on_ci()
skip_on_cran()

# Note: these tests depend on installation of Python and a few libraries. This is taken care of in the setup.R file within the testthat folder.

test_that("timeseries plot is as expected for one year with no historic range or slider", {
  skip_on_cran()
  skip_on_ci()
  dir <- paste0(tempdir(), "\\plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir)
  path <- paste0(dir, "\\test1.svg")
  path <- gsub("\\\\", "/", path)
  plot <- plotTimeseries(location = "09EA004", parameter = "water level", start_date = "2016-01-01", end_date = "2017-01-01", historic_range = FALSE, slider = FALSE)
  plotly::save_image(plot, file = path)
  expect_snapshot_file(path)
})

test_that("timeseries plot is as expected for one year with no historic range", {
  skip_on_cran()
  skip_on_ci()
  dir <- paste0(tempdir(), "\\plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir)
  path <- paste0(dir, "\\test2.svg")
  path <- gsub("\\\\", "/", path)
  plot <- plotTimeseries(location = "09EA004", parameter = "water level", start_date = "2016-01-01", end_date = "2017-01-01", historic_range = FALSE)
  plotly::save_image(plot, file = path)
  expect_snapshot_file(path)
})

test_that("timeseries plot is as expected for one year with historic range", {
  dir <- paste0(tempdir(), "\\plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir)
  path <- paste0(dir, "\\test3.svg")
  path <- gsub("\\\\", "/", path)
  plot <- plotTimeseries(location = "09EA004", parameter = "water level", start_date = "2016-01-01", end_date = "2017-01-01")
  plotly::save_image(plot, file = path)
  expect_snapshot_file(path)
})

test_that("French timeseries plot is as expected for one year with historic range and slider", {
  dir <- paste0(tempdir(), "\\plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir)
  path <- paste0(dir, "\\test4.svg")
  path <- gsub("\\\\", "/", path)
  plot <- plotTimeseries(location = "09EA004", parameter = "water level", start_date = "2016-01-01", end_date = "2017-01-01", lang = "fr")
  plotly::save_image(plot, file = path)
  expect_snapshot_file(path)
})

test_that("French timeseries plot is as expected for one year with historic range and no slider", {
  dir <- paste0(tempdir(), "\\plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir)
  path <- paste0(dir, "\\test5.svg")
  path <- gsub("\\\\", "/", path)
  plot <- plotTimeseries(location = "09EA004", parameter = "water level", start_date = "2016-01-01", end_date = "2017-01-01", lang = "fr", slider = FALSE)
  plotly::save_image(plot, file = path)
  expect_snapshot_file(path)
})

test_that("grades, approvals, qualifiers are displayed", {
  dir <- paste0(tempdir(), "\\plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir)
  path <- paste0(dir, "\\test6.svg")
  path <- gsub("\\\\", "/", path)
  plot <- plotTimeseries(location = "09EA004", parameter = "water level", start_date = "2016-01-01", end_date = "2017-01-01", lang = "fr", slider = FALSE, grades = TRUE, qualifiers = TRUE, approvals = TRUE)
  plotly::save_image(plot, file = path)
  expect_snapshot_file(path)
})

test_that("one of grades, approvals, qualifiers is displayed", {
  dir <- paste0(tempdir(), "\\plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir)
  path <- paste0(dir, "\\test7.svg")
  path <- gsub("\\\\", "/", path)
  plot <- plotTimeseries(location = "09EA004", parameter = "water level", start_date = "2016-01-01", end_date = "2017-01-01", lang = "fr", slider = FALSE, grades = TRUE)
  plotly::save_image(plot, file = path)
  expect_snapshot_file(path)
})

test_that("returned plot data is as expected", {
  plot <- plotTimeseries(location = "09EA004", parameter = "water level", start_date = "2016-01-01", end_date = "2017-01-01", lang = "fr", slider = FALSE, data = TRUE)$data
  expect_type(plot, "list")
  expect_named(plot, c("trace_data", "range_data"))
  expect_named(plot$trace_data, c("datetime", "value"))
  expect_named(plot$range_data, c("datetime", "min", "max", "q75", "q25"))
})
