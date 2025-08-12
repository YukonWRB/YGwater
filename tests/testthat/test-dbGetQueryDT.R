test_that("dbGetQueryDT converts SQLite date columns", {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, "CREATE TABLE test (id INTEGER, date TEXT, datetime TEXT, start_dt TEXT)")
  DBI::dbExecute(con, "INSERT INTO test VALUES (1, '2024-01-01', '2024-01-01 12:00:00', '2024-01-01 13:00:00')")
  res <- dbGetQueryDT(con, "SELECT * FROM test")
  expect_s3_class(res, "data.table")
  DBI::dbDisconnect(con)
})
