skip_on_ci()
skip_on_cran()

test_that("Snow DB connection works", {
  con <- snowConnect(silent = TRUE)
  expect_true(DBI::dbIsValid(con))
  DBI::dbDisconnect(con)
})

test_that("Snow DB has 13 tables/views", {
  con <- snowConnect(silent = TRUE)
  expect_length(DBI::dbListTables(con), 13)
  DBI::dbDisconnect(con)
})
