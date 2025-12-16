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
    end_date = "2022-01-01"
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
    data = TRUE
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
