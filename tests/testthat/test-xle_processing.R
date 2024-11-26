# Test that all testing dependencies are as expected
test_that("YOWN master is as expected", {
  # Check YOWN_MASTER.xlsx
  master <- openxlsx::read.xlsx(test_path("fixtures/logger_reads/YOWN_MASTER.xlsx"), sheet = 1)
  expect_snapshot(master)
})

test_that("YOWN logger tracking is as expected", {
  # Check YOWN_Logger_Tracking.xlsx
  tracking <- openxlsx::read.xlsx(test_path("fixtures/logger_reads/YOWN_Logger_Tracking.xlsx"), sheet = 1)
  expect_snapshot(tracking)
})

test_that("xle file is as expected", {
  # Check .xle file
  xle <- xml2::read_xml(test_path("fixtures/logger_reads/1071079_YOWN-0000_TEST_2020_05_07.xle"))
  expect_snapshot(xle)
})


test_that("xle file can be read", {
  
  dir <- tempdir()
  
  # Remove files from tempdir()
  unlink(dir, recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE))
  
  res <- xle_processing(file = test_path("fixtures/logger_reads/1071079_YOWN-0000_TEST_2020_05_07.xle"),
                        aq_upload = FALSE,   # Will force function to output a data.frame instead of aquarius upload
                        master_sheet = test_path("fixtures/logger_reads/YOWN_MASTER.xlsx"),
                        logger_tracking = test_path("fixtures/logger_reads/YOWN_Logger_Tracking.xlsx"),
                        dropbox = dir,
                        repo = NULL)
  
  # Check that the returned tibble is as expected
  expect_snapshot(res)

  # check that the logger tracking file has a new row
  logger_tracking <- openxlsx::read.xlsx(test_path("fixtures/logger_reads/YOWN_Logger_Tracking.xlsx"), sheet = 1)
  expect_equal(nrow(logger_tracking), 1)

  # Delete the new row in the logger tracking file
  openxlsx::write.xlsx(logger_tracking[-1, ], test_path("fixtures/logger_reads/YOWN_Logger_Tracking.xlsx"), overwrite = TRUE)
})
