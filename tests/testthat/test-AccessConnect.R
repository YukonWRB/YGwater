test_that("Access connection works", {
  # Look for the Access driver
  odbc_drivers <- tryCatch(odbc::odbcListDrivers(), error = function(e) {
    return(NULL)
  })

  # Check if the Microsoft Access Driver is available
  if (!any(grepl("Microsoft Access Driver", odbc_drivers$name))) {
    skip(
      "Microsoft Access Driver not found."
    )
  }

  # Test .mdb first
  con <- AccessConnect(path = test_path("fixtures/tiny.mdb"), silent = TRUE)
  tbls <- DBI::dbListTables(con)
  # Ensure that 'test_table' is listed
  expect_true("test_table" %in% tbls)
  DBI::dbDisconnect(con)

  # Test .accdb
  con <- AccessConnect(path = test_path("fixtures/tiny.accdb"), silent = TRUE)
  tbls <- DBI::dbListTables(con)
  # Ensure that 'test_table' is listed
  expect_true("test_table" %in% tbls)
  DBI::dbDisconnect(con)
})
