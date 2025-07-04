test_that("calculate_period handles uniform hourly data", {
  df <- data.frame(
    datetime = as.POSIXct("2021-01-01 00:00:00", tz = "UTC") + 0:5 * 3600,
    value = 1:6
  )
  res <- calculate_period(df)
  expect_equal(unique(res$period), "P0DT1H0M0S")
})


test_that("calculate_period detects change in period", {
  dt <- data.frame(
    datetime = as.POSIXct(c(
      "2020-12-31 22:00:00",
      "2020-12-31 23:00:00",
      "2021-01-01 00:00:00",
      "2021-01-01 01:00:00",
      "2021-01-01 02:00:00",
      "2021-01-01 03:00:00",
      "2021-01-01 09:00:00",
      "2021-01-01 15:00:00",
      "2021-01-01 21:00:00",
      "2021-01-02 03:00:00"
    ), tz = "UTC"),
    value = 1:10
  )
  res <- calculate_period(dt)
  expect_true(all(res$period[1:5] == "P0DT1H0M0S"))
  expect_true(all(res$period[6:10] == "P0DT6H0M0S"))
})


test_that("calculate_period errors with too few rows", {
  df <- data.frame(
    datetime = as.POSIXct(c(
      "2021-01-01 00:00:00",
      "2021-01-01 01:00:00"
    ), tz = "UTC"),
    value = 1:2
  )
  expect_error(calculate_period(df), "too few measurements")
})


test_that("calculate_period preserves data.table class", {
  dt <- data.table::data.table(
    datetime = as.POSIXct("2021-01-01 00:00:00", tz = "UTC") + 0:5 * 3600,
    value = 1:6
  )
  res <- calculate_period(dt)
  expect_true(data.table::is.data.table(res))
  expect_equal(unique(res$period), "P0DT1H0M0S")
})
