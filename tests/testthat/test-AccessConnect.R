test_that("Access connection works", {
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
