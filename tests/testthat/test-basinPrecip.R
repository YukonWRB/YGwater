res <- basinPrecip("09EA004", start = Sys.time()-60*60*48, end = Sys.time()-60*60*6)
test_that("times in past only work", {
  expect_true(res$mean_precip > 0)
})
test_that("no forecast is fetched", {
  expect_true(is.null(res$forecast_time_range_UTC))
})
rm(res)

test_that("times in the future by less than 48 hours work", {
  expect_output(basinPrecip("09EA004", start = Sys.time(), end = Sys.time()+60*60*24))
})

test_that("times in the future by > 48 hours work", {
  expect_output(basinPrecip("09EA004", start = Sys.time(), end = Sys.time()+60*60*96))
})

test_that("times spanning past and future work", {
  expect_output(basinPrecip("09EA004", start = Sys.time()-60*60*48, end = Sys.time()+60*60*24))
})

test_that("requesting times in past when no forecast is issued yet yields only a forecast", {
  res <- basinPrecip("09EA004", start = Sys.time()-60*60*1, end = Sys.time())
  expect_true(is.null(res$reanalysis_time_range_UTC))
})

test_that("requesting time span < 6 hours works when in the past", {
  expect_output(basinPrecip("09EA004", start = Sys.time()-60*60*24, end = Sys.time()-60*60*23))
})

test_that("correct error is thrown if start is after end", {
  expect_error(basinPrecip("09EA004", start = Sys.time(), end = Sys.time()-60*60*24))
})
