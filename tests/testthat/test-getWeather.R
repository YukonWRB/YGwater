skip_if_offline()

test_that("weather is fetched and return looks as expected", {
  res <- getWeather("53179", start = "2022-01-01", end = "2022-01-15", interval = "day")
  expect_equal(nrow(res), 15)
  expect_equal(ncol(res), 37)
})
