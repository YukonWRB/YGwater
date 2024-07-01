test_that("Snow DB connection works", {
  con <- snowConnect(silent = TRUE)
  expect_true(DBI::dbIsValid(con))
  DBI::dbDisconnect(con)
})

test_that("Snow DB has 12 tables/views", {
  con <- snowConnect(silent = TRUE)
  expect_length(DBI::dbListTables(con), 12)
  DBI::dbDisconnect(con)
})
