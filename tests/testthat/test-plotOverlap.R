# Note: these tests depend on installation of Python and a few libraries. This is taken care of in the setup.R file within the testthat folder.
skip_on_cran()

test_that("plotOverlap with all defaults", {
  dir <- paste0(tempdir(), "\\plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir)
  path <- paste0(dir, "\\test1.png")
  path <- gsub("\\\\", "/", path)
  on.exit(unlink(path), add = TRUE)
  
  plot <- plotOverlap(location = "09EA004", parameter = "water level", years = 2020)
  plotly::save_image(plot, file = path, width = 500, height = 500)
  
  expect_snapshot_file(path)
})

test_that("plotOverlap with minimal elements", {
  dir <- paste0(tempdir(), "\\plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir)
  path <- paste0(dir, "\\test2.png")
  path <- gsub("\\\\", "/", path)
  on.exit(unlink(path), add = TRUE)
  
  plot <- plotOverlap(location = "09EA004", parameter = "water level", years = 2020, hover = FALSE, slider = FALSE, gridx = FALSE, gridy = FALSE)
  plotly::save_image(plot, file = path, width = 500, height = 500)
  
  expect_snapshot_file(path)
})

test_that("plotOverlap with multiple years and 'last' historic range", {
  dir <- paste0(tempdir(), "\\plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir)
  path <- paste0(dir, "\\test3.png")
  path <- gsub("\\\\", "/", path)
  on.exit(unlink(path), add = TRUE)
  
  plot <- plotOverlap(location = "09AB004", parameter = "water level", years = c(1990, 1991), hover = FALSE, slider = FALSE, gridx = FALSE, gridy = FALSE, historic_range = "last")
  plotly::save_image(plot, file = path, width = 500, height = 500)
  
  expect_snapshot_file(path)
})

test_that("plotOverlap with multiple years and 'last' historic range", {
  dir <- paste0(tempdir(), "\\plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir)
  path <- paste0(dir, "\\test4.png")
  path <- gsub("\\\\", "/", path)
  on.exit(unlink(path), add = TRUE)
  
  plot <- plotOverlap(location = "09AB004", parameter = "water level", years = c(1990, 1991), hover = FALSE, slider = FALSE, gridx = FALSE, gridy = FALSE, historic_range = "all")
  plotly::save_image(plot, file = path, width = 500, height = 500)
  
  expect_snapshot_file(path)
})

test_that("returned plot data is as expected", {
  plot <- plotOverlap(location = "09AB004", parameter = "water level", years = c(1990, 1991), hover = FALSE, slider = FALSE, gridx = FALSE, gridy = FALSE, historic_range = "all", data = TRUE)$data
  expect_type(plot, "list")
  expect_named(plot, c("trace_data", "range_data"))
  expect_named(plot$trace_data, c("datetime", "value", "year", "month", "day", "plot_year", "plot_datetime"))
  expect_named(plot$range_data, c("datetime", "value", "max", "min", "q75", "q25", "year", "month", "day"))
})
