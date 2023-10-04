test_that("correct number of raster are saved to disc", {
  suppressWarnings(dir.create(paste0(tempdir(), "/test_hrdps")))
  getHRDPS(clip = NULL, save_path = (paste0(tempdir(), "/test_hrdps")), param = "APCP_Sfc")
  expect_equal(length(list.files(paste0(tempdir(), "/test_hrdps/APCP_Sfc"))), 144)
})

