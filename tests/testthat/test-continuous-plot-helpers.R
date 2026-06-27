test_that("historic range export data is NULL when stats are unavailable", {
  expect_null(historic_range_data_for_export(data.frame(), "m"))

  range_data <- data.frame(
    datetime = as.POSIXct("2026-06-01", tz = "UTC"),
    min = 1,
    max = 2,
    q75 = NA_real_,
    q25 = NA_real_
  )
  expect_null(historic_range_data_for_export(range_data, "m"))
})

test_that("historic range export data is renamed when stats are available", {
  range_data <- data.frame(
    datetime = as.POSIXct("2026-06-01", tz = "UTC"),
    min = 1,
    max = 2,
    q75 = 1.75,
    q25 = 1.25
  )

  out <- historic_range_data_for_export(range_data, "m")

  expect_s3_class(out, "data.frame")
  expect_named(
    out,
    c(
      "datetime_UTC",
      "historic_min_m",
      "historic_max_m",
      "historic_Q75_m",
      "historic_Q25_m"
    )
  )
  expect_equal(nrow(out), 1L)
})
