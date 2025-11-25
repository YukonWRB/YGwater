test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("get_most_recent_date returns correct date", {
  ts <- data.frame(
    datetime = as.POSIXct(
      c("2024-01-01", "2024-01-02", "2024-01-03"),
      tz = "UTC"
    ),
    value = c(100, NA, 150)
  )
  expect_equal(get_most_recent_date(ts), as.POSIXct("2024-01-03", tz = "UTC"))
})

test_that("get_datetime returns first day of month", {
  expect_equal(get_datetime(2025, 3), as.POSIXct("2025-03-01", tz = "UTC"))
})

test_that("initialize_visualization_parameters returns expected structure", {
  viz <- initialize_visualization_parameters()
  expect_true(is.list(viz))
  expect_true("basins" %in% names(viz))
  expect_true("surveys" %in% names(viz))
  expect_true("pillows" %in% names(viz))
})
