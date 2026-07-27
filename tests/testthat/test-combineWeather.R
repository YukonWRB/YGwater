test_that("combineWeather combines data-frame station inputs", {
  stn1 <- data.frame(
    datetime = as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + 0:9 * 3600,
    temp = 1:10
  )
  stn2 <- data.frame(
    datetime = as.POSIXct("2024-01-01 05:00:00", tz = "UTC") + 0:9 * 3600,
    temp = 16:25
  )

  out <- suppressMessages(combineWeather(
    list(stn1, stn2),
    datetime_col = "datetime",
    variables = "temp"
  ))

  expect_equal(nrow(out), 15L)
  expect_equal(out$value, 1:15)
  expect_equal(out$station, c(rep(1, 10), rep(2, 5)))
})
