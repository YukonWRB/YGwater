skip_on_ci()
skip_on_cran()

res <- suppressWarnings(snowInfo(locations = "all", inactive = FALSE, save_path = NULL, stats = FALSE, plots = FALSE))

test_that("output has correct tables", {
  expect_named(res, c("locations", "measurements"))
})

test_that("locations table has correct names", {
  expect_named(res$locations, c('location_ID', 'location_name', 'latitude', 'longitude', 'elevation', 'last_survey'))
})

test_that("measurements table has correct names", { #NOTE: names for this one are taken straight from the DB. If the test fails, check for changes to the DB first.
  expect_named(res$measurements, c('timeseries_id', 'target_datetime', 'value', 'year', 'month', 'location_id', 'end_datetime', 'param_name', 'location'))
})

rm(res)
