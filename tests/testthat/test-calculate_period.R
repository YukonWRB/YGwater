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


test_that("calculate_period uses historical reconstruction when as_of is set", {
  fake_con <- structure(list(), class = "mock_connection")
  calls <- list()

  res <- testthat::with_mocked_bindings(
    calculate_period(
      data.table::data.table(
        datetime = as.POSIXct(c(
          "2021-01-01 00:00:00",
          "2021-01-01 01:00:00"
        ), tz = "UTC"),
        value = 1:2
      ),
      timeseries_id = 123,
      con = fake_con,
      as_of = as.POSIXct("2026-03-30 12:00:00", tz = "UTC")
    ),
    dbGetQueryDT = function(con, statement, params = NULL, ...) {
      calls[[length(calls) + 1]] <<- list(statement = statement, params = params)

      if (length(calls) == 1) {
        return(data.table::data.table(
          datetime = as.POSIXct(c(
            "2020-12-31 19:00:00",
            "2020-12-31 20:00:00",
            "2020-12-31 21:00:00",
            "2020-12-31 22:00:00",
            "2020-12-31 23:00:00"
          ), tz = "UTC"),
          value = -3:1
        ))
      }

      data.table::data.table(
        datetime = as.POSIXct(c(
          "2021-01-01 02:00:00",
          "2021-01-01 03:00:00",
          "2021-01-01 04:00:00",
          "2021-01-01 05:00:00",
          "2021-01-01 06:00:00"
        ), tz = "UTC"),
        value = 3:7
      )
    },
    .package = "YGwater"
  )

  expect_equal(unique(res$period), "P0DT1H0M0S")
  expect_length(calls, 2)
  expect_true(all(vapply(
    calls,
    function(call) {
      grepl(
        "continuous\\.measurements_continuous_corrected_at\\(",
        call$statement
      )
    },
    logical(1)
  )))
  expect_match(calls[[1]]$statement, "WHERE datetime < \\$5")
  expect_match(calls[[2]]$statement, "WHERE datetime > \\$5")
  expect_equal(calls[[1]]$params[[1]], as.POSIXct("2026-03-30 12:00:00", tz = "UTC"))
  expect_equal(calls[[1]]$params[[2]], 123)
})
