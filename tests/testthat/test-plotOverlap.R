# Tests depend on snapshots so can't be inspected on CRAN or CI
skip_on_cran()

test_that("plotOverlap with all defaults", {
  plot <- plotOverlap(location = "09EA004", parameter = "water level", years = 2020)
  vdiffr::expect_doppelganger("plotOverlap_default", plot)
})

test_that("plotOverlap with minimal elements", {
  plot <- plotOverlap(location = "09EA004", parameter = "water level", years = 2020, hover = FALSE, slider = FALSE, gridx = FALSE, gridy = FALSE)
  vdiffr::expect_doppelganger("plotOverlap_minimal_elements", plot)
})

test_that("plotOverlap with multiple years and 'last' historic range", {
  plot <- plotOverlap(location = "09AB004", parameter = "water level", years = c(1990, 1991), hover = FALSE, slider = FALSE, gridx = FALSE, gridy = FALSE, historic_range = "last")
  vdiffr::expect_doppelganger("plotOverlap_last_years", plot)
})

test_that("plotOverlap with multiple years and 'last' historic range", {
  plot <- plotOverlap(location = "09AB004", parameter = "water level", years = c(1990, 1991), hover = FALSE, slider = FALSE, gridx = FALSE, gridy = FALSE, historic_range = "all")
  vdiffr::expect_doppelganger("plotOverlap_all_years", plot)
})

test_that("returned plot data is as expected", {
  plot <- plotOverlap(location = "09AB004", parameter = "water level", years = c(1990, 1991), hover = FALSE, slider = FALSE, gridx = FALSE, gridy = FALSE, historic_range = "all", data = TRUE)$data
  expect_type(plot, "list")
  expect_named(plot, c("trace_data", "range_data"))
  expect_named(plot$trace_data, c("datetime", "value", "year", "month", "day", "plot_year", "plot_datetime"))
  expect_named(plot$range_data, c("datetime", "value", "max", "min", "q75", "q25", "year", "month", "day"))
})
