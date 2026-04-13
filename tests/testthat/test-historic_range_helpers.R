test_that("historic range is disabled only for sub-daily cumulative series", {
  expect_true(historic_range_is_meaningless(
    aggregation_types = "sum",
    resolution = "hour"
  ))

  expect_true(historic_range_is_meaningless(
    aggregation_types = "sum",
    resolution = "max",
    record_rate_seconds = 3600
  ))

  expect_false(historic_range_is_meaningless(
    aggregation_types = "sum",
    resolution = "day",
    record_rate_seconds = 3600
  ))

  expect_false(historic_range_is_meaningless(
    aggregation_types = "sum",
    resolution = "max",
    record_rate_seconds = 24 * 60 * 60
  ))

  expect_false(historic_range_is_meaningless(
    aggregation_types = "instantaneous",
    resolution = "hour",
    record_rate_seconds = 3600
  ))
})
