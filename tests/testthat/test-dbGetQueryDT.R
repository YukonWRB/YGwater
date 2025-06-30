library(DBI)


# Use local SQLite fixture for offline testing
sqlite_path <- testthat::test_path("fixtures", "aquacache_test.sqlite")

con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)
on.exit <- function() DBI::dbDisconnect(con)


# Test default date conversion for SQLite

test_that("sqlite date columns are converted", {
  res <- dbGetQueryDT(con, "SELECT date FROM measurements_calculated_daily LIMIT 2")
  expect_s3_class(res$date, "Date")
})

test_that("sqlite datetime columns are converted", {
  res <- dbGetQueryDT(con, "SELECT datetime, target_datetime FROM samples LIMIT 2")
  expect_s3_class(res$datetime, "POSIXct")
  expect_s3_class(res$target_datetime, "POSIXct")
})

test_that("conversion can be turned off", {
  res <- dbGetQueryDT(con, "SELECT start_dt, end_dt FROM grades LIMIT 1", sqlite_date_convert = FALSE)
  expect_type(res$start_dt, "character")
  expect_type(res$end_dt, "character")
})

DBI::dbDisconnect(con)


