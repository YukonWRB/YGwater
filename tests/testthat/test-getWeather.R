test_that("weather is fetched and return looks as expected", {
  expect_snapshot(getWeather("53179", start = "2022-01-01", end = "2022-01-15", interval = "day"))
})
