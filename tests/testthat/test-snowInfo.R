
res <- suppressWarnings(snowInfo(db_path = "X:/Snow/DB/SnowDB.mdb", locations = "all", inactive = FALSE, save_path = NULL, stats = FALSE, plots = FALSE))

test_that("output has correct tables", {
  expect_named(res, c("locations", "measurements"))
})

test_that("locations table has correct names", {
  expect_named(res$locations, c("location_ID", "location_name", "active", "elevation_m", "latitude", "longitude"))
})

test_that("measurements table has correct names", { #NOTE: names for this one are taken straight from the DB. If the test fails, check for changes to the DB first.
  expect_named(res$measurements, c("SNOW_COURSE_ID", "AGENCY_ID", "DEPTH", "SNOW_WATER_EQUIV", "ESTIMATE_FLG", "SAMPLE_DATE", "SURVEY_DATE", "EXCLUDE_FLG", "SPECIAL_FLG", "NOSURVEY_FLG", "REC_CREATE_DATE", "REC_CREATE_USER", "REC_LAST_MOD_DATE", "REC_LAST_MOD_USER", "year", "month"))
})

rm(res)
