test_that("tabular report is created without error on current data", {
  
  con <- AquaConnect(silent = TRUE)
  # Check the 'timeseries' table to make sure there's data for today.
  check <- DBI::dbGetQuery(con, "SELECT COUNT(*) FROM timeseries WHERE DATE(end_datetime) = CURRENT_DATE")[1,1]
  if (check == 0) {
    skip("No data available for today, skipping test.")
  }
  path <- paste0(tempdir(), "/tabular_report_test")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  
  expect_no_error({
    res <- suppressMessages(tabularReport(precip_locations = NULL, bridge_locations = NULL, save_path = path, archive_path = NULL, past = 7))
})
  
  # Can't snapshot as the data is different day-to-day
  
  expect_true(file.exists(res))
})


test_that("tabular report is created without error on historic data", {
  
  con <- AquaConnect(silent = TRUE)
  # Check the 'timeseries' table to make sure there's data for today.
  check <- DBI::dbGetQuery(con, "SELECT COUNT(*) FROM timeseries WHERE DATE(end_datetime) = CURRENT_DATE")[1,1]
  if (check == 0) {
    skip("No data available for this day in the past, skipping test.")
  }
  path <- paste0(tempdir(), "/tabular_report_test")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  
  expect_no_error({
    res <- suppressMessages(tabularReport(precip_locations = NULL, bridge_locations = NULL, save_path = path, archive_path = NULL, past = 7, report_datetime = as.POSIXct("2020-06-01 12:00", tz = "UTC")))
  })
  
  expect_true(file.exists(res))
})
