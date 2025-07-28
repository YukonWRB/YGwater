# Tests depend on db connection and internet access
skip_on_cran()
skip_if_offline()

# Check if curl can be used (can pose problems depending on network)
test_url <- paste0("https://dd.weather.gc.ca/", gsub("-", "", Sys.Date() - 1), "/WXO-DD/model_hrdpa/2.5km/18/", gsub("-", "", Sys.Date() - 1), "T18Z_MSC_HRDPA_APCP-Accum6h_Sfc_RLatLon0.0225_PT0H.grib2")
# Check if the test URL can be downloaded
curl::curl_download(test_url, destfile = tempfile(fileext = ".grib2"), quiet = TRUE)
if (!file.exists(tempfile(fileext = ".grib2"))) {
  skip("Test URL for basinPrecip is not available, skipping tests.")
}
# Delete the file
unlink(tempfile(fileext = ".grib2"))

res <- basinPrecip("09EA004", start = Sys.time() - 60*60*48, end = Sys.time() - 60*60*6)
test_that("times in past only work", {
  expect_true(res$mean_precip > 0)
})
test_that("no forecast is fetched", {
  expect_true(is.null(res$forecast_time_range_UTC))
})
rm(res)

test_that("times in the future by less than 48 hours work", {
  expect_output(basinPrecip("09EA004", start = Sys.time(), end = Sys.time() + 60*60*24))
})

test_that("times in the future by > 48 hours work", {
  expect_output(basinPrecip("09EA004", start = Sys.time(), end = Sys.time() + 60*60*96))
})

test_that("times spanning past and future work", {
  expect_output(basinPrecip("09EA004", start = Sys.time() - 60*60*48, end = Sys.time() + 60*60*24))
})

test_that("requesting times in past when no forecast is issued yet yields only a forecast", {
  res <- basinPrecip("09EA004", start = Sys.time() - 60*60*1, end = Sys.time())
  expect_true(is.null(res$reanalysis_time_range_UTC))
})

test_that("requesting time span < 6 hours works when in the past", {
  expect_output(basinPrecip("09EA004", start = Sys.time() - 60*60*24, end = Sys.time() - 60*60*23))
})

test_that("correct error is thrown if start is after end", {
  expect_error(basinPrecip("09EA004", start = Sys.time(), end = Sys.time() - 60*60*24))
})
