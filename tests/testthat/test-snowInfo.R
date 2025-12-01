skip_on_ci()
skip_on_cran()

res <- suppressWarnings(snowInfo(
  locations = "all",
  inactive = FALSE,
  save_path = NULL,
  stats = FALSE,
  plots = FALSE
))

test_that("output has correct tables", {
  expect_named(res, c("locations", "measurements"))
})

test_that("locations table has correct names", {
  expect_named(
    res$locations,
    c(
      'location_code',
      'location_name',
      'note',
      'sub_basin',
      'latitude',
      'longitude',
      'elevation_m',
      'metadata_created',
      'metadata_modified',
      'first_survey',
      'last_survey',
      'march1_surveys',
      'april1_surveys',
      'may1_surveys',
      'may15_surveys'
    )
  )
})

test_that("measurements table has correct names", {
  #NOTE: names for this one are taken straight from the DB. If the test fails, check for changes to the DB first.
  expect_named(
    res$measurements,
    c(
      'location_code',
      "location_name",
      "parameter",
      "units",
      "sample_date",
      "target_date",
      "year",
      "month",
      "result",
      "flag"
    )
  )
})

rm(res)
