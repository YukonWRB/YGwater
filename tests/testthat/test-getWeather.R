skip_if_offline()
# Skip on cran and ci because the weathercan::weather_dl function used requires having an installed stations list, and currently that can't be checked for non-interactively.
skip_on_cran()
skip_on_ci()

test_that("weather is fetched and return looks as expected", {
  res <- suppressWarnings(suppressMessages(getWeather(
    "53179",
    start = "2022-01-01",
    end = "2022-01-15",
    interval = "day"
  )))
  expect_equal(nrow(res), 15)
  expect_equal(ncol(res), 37)
})
