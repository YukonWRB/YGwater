# Note: these tests depend on installation of Python and a few libraries. This is taken care of in the setup.R file within the testthat folder.

test_that("timeseries plot is as expected for one year with no historic range or slider", {
  skip_on_cran()
  plot <- plotTimeseries(location = "09EA004", parameter = "water level", start_date = "2016-01-01", end_date = "2017-01-01", historic_range = FALSE, slider = FALSE)
  vdiffr::expect_doppelganger("plot_no_hist_range_no_slider", print(plot))
})

test_that("timeseries plot is as expected for one year with no historic range", {
  skip_on_cran()
  plot <- plotTimeseries(location = "09EA004", parameter = "water level", start_date = "2016-01-01", end_date = "2017-01-01", historic_range = FALSE)
  vdiffr::expect_doppelganger("plot_no_hist_range", plot)
})

test_that("timeseries plot is as expected for one year with historic range", {
  skip_on_cran()
  plot <- plotTimeseries(location = "09EA004", parameter = "water level", start_date = "2016-01-01", end_date = "2017-01-01")
  vdiffr::expect_doppelganger("plot_w_hist_range", plot)
})

test_that("French timeseries plot is as expected for one year with historic range and slider", {
  skip_on_cran()
  plot <- plotTimeseries(location = "09EA004", parameter = "water level", start_date = "2016-01-01", end_date = "2017-01-01", lang = "fr")
  vdiffr::expect_doppelganger("fr_plot_w_hist_rnge_and_slider", plot)
})

test_that("French timeseries plot is as expected for one year with historic range and no slider", {
  skip_on_cran()
  plot <- plotTimeseries(location = "09EA004", parameter = "water level", start_date = "2016-01-01", end_date = "2017-01-01", lang = "fr", slider = FALSE)
  vdiffr::expect_doppelganger("fr_plot_w_hist_rng_no_slider", plot)
})

test_that("grades, approvals, qualifiers are displayed", {
  skip_on_cran()
  plot <- plotTimeseries(location = "09EA004", parameter = "water level", start_date = "2016-01-01", end_date = "2017-01-01", lang = "fr", slider = FALSE, grades = TRUE, qualifiers = TRUE, approvals = TRUE)
  vdiffr::expect_doppelganger("plot_w_grades_approvals_qualifiers", plot)
})

test_that("one of grades, approvals, qualifiers is displayed", {
  # skip_on_ci()
  skip_on_cran()
  plot <- plotTimeseries(location = "09EA004", parameter = "water level", start_date = "2016-01-01", end_date = "2017-01-01", lang = "fr", slider = FALSE, grades = TRUE)
  vdiffr::expect_doppelganger("timeseries_plot_with_grades", plot)
})

test_that("returned plot data is as expected", {
  skip_on_ci()
  plot <- plotTimeseries(location = "09EA004", parameter = "water level", start_date = "2016-01-01", end_date = "2017-01-01", lang = "fr", slider = FALSE, data = TRUE)$data
  expect_type(plot, "list")
  expect_named(plot, c("trace_data", "range_data"))
  expect_named(plot$trace_data, c("datetime", "value"))
  expect_named(plot$range_data, c("datetime", "min", "max", "q75", "q25"))
})
