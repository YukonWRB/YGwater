skip_if_offline()
skip_on_cran()

# Check if curl can be used (can pose problems depending on network)
test_url <- paste0("https://dd.weather.gc.ca/", gsub("-", "", Sys.Date() - 1), "/WXO-DD/model_hrdpa/2.5km/18/", gsub("-", "", Sys.Date() - 1), "T18Z_MSC_HRDPA_APCP-Accum6h_Sfc_RLatLon0.0225_PT0H.grib2")
# Check if the test URL can be downloaded
curl::curl_download(test_url, destfile = tempfile(fileext = ".grib2"), quiet = TRUE)
if (!file.exists(tempfile(fileext = ".grib2"))) {
  skip("Test URL for basinPrecip is not available, skipping tests.")
}
# Delete the file
unlink(tempfile(fileext = ".grib2"))

test_that("correct number of raster are saved to disc", {
  suppressWarnings(dir.create(paste0(tempdir(), "/test_hrdps")))
  getHRDPS(clip = NULL, save_path = (paste0(tempdir(), "/test_hrdps")), param = "APCP_Sfc")
  expect_equal(length(list.files(paste0(tempdir(), "/test_hrdps/APCP_Sfc"))), 144)
})

