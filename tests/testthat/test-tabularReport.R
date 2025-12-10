test_that("tabular report is created without error on current data", {
  con <- AquaConnect(silent = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  # Check the 'timeseries' table to make sure there's data for today.
  check <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) FROM timeseries WHERE DATE(end_datetime) = CURRENT_DATE"
  )[1, 1]
  if (check == 0) {
    skip("No data available for today, skipping test.")
  }
  path <- paste0(tempdir(), "/tabular_report_test")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  expect_no_error({
    res <- suppressMessages(tabularReport(
      precip_locations = NULL,
      bridge_locations = NULL,
      save_path = path,
      archive_path = NULL,
      past = 7,
      con = con
    ))
  })

  # Can't snapshot as the data is different day-to-day

  expect_true(file.exists(res))

  # Check that the file has the expected sheet names; getSheetNames returns a vector of sheet names, so expect_named doesn't work here
  names <- openxlsx::getSheetNames(res)
  expect_equal(names, c("comments", "levels", "flows", "snow"))

  # Check that levels has four header rows plus at least one data row
  levels <- openxlsx::read.xlsx(res, sheet = "levels")
  expect_true(nrow(levels) > 4)
})


test_that("tabular report is created without error on historic data", {
  skip("Skipping historic data test to reduce test time.")
  con <- AquaConnect(silent = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Check the 'timeseries' table to make sure there's data for today.
  check <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) FROM measurements_continuous_corrected WHERE DATE(datetime) = '2020-06-01'"
  )[1, 1]
  if (check == 0) {
    skip("No data available for this day in the past, skipping test.")
  }
  path <- paste0(tempdir(), "/tabular_report_test")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)

  expect_no_error({
    res <- suppressMessages(tabularReport(
      precip_locations = NULL,
      bridge_locations = NULL,
      save_path = path,
      archive_path = NULL,
      past = 7,
      report_datetime = as.POSIXct("2020-06-01 12:00", tz = "UTC"),
      con = con
    ))
  })

  expect_true(file.exists(res))

  # Check that the file has the expected sheet names; getSheetNames returns a vector of sheet names, so expect_named doesn't work here
  names <- openxlsx::getSheetNames(res)
  expect_equal(names, c("comments", "levels", "flows", "snow"))

  # Check that levels has four header rows plus at least one data row
  levels <- openxlsx::read.xlsx(res, sheet = "levels")
  expect_true(nrow(levels) > 4)

  # Ensure that no NA values are found in row 4, columns 1:10
  expect_true(all(!is.na(levels[4, 1:11])))
})
