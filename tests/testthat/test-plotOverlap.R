# Tests depend on snapshots so can't be inspected on CRAN or CI
skip_on_ci()
skip_on_cran()

# Note: these tests depend on installation of Python and a few libraries. This is taken care of in the setup.R file within the testthat folder.

test_that("plotOverlap with all defaults", {
  dir <- paste0(tempdir(), "\\plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir)
  path <- paste0(dir, "\\test1.svg")
  path <- gsub("\\\\", "/", path)
  plot <- plotOverlap(location = "09EA004", parameter = "water level", years = 2020)
  plotly::save_image(plot, file = path)
  expect_snapshot_file(path)
})

test_that("plotOverlap with minimal elements", {
  dir <- paste0(tempdir(), "\\plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir)
  path <- paste0(dir, "\\test2.svg")
  path <- gsub("\\\\", "/", path)
  plot <- plotOverlap(location = "09EA004", parameter = "water level", years = 2020, hover = FALSE, slider = FALSE, gridx = FALSE, gridy = FALSE)
  plotly::save_image(plot, file = path)
  expect_snapshot_file(path)
})

test_that("plotOverlap with multiple years and 'last' historic range", {
  dir <- paste0(tempdir(), "\\plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir)
  path <- paste0(dir, "\\test3.svg")
  path <- gsub("\\\\", "/", path)
  plot <- plotOverlap(location = "09AB004", parameter = "water level", years = c(1990, 1991), hover = FALSE, slider = FALSE, gridx = FALSE, gridy = FALSE, historic_range = "last")
  plotly::save_image(plot, file = path)
  expect_snapshot_file(path)
})

test_that("plotOverlap with multiple years and 'last' historic range", {
  dir <- paste0(tempdir(), "\\plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir)
  path <- paste0(dir, "\\test4.svg")
  path <- gsub("\\\\", "/", path)
  plot <- plotOverlap(location = "09AB004", parameter = "water level", years = c(1990, 1991), hover = FALSE, slider = FALSE, gridx = FALSE, gridy = FALSE, historic_range = "all")
  plotly::save_image(plot, file = path)
  expect_snapshot_file(path)
})
