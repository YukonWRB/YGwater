test_that("Hydro DB connection works", {
  con <- hydroConnect()
  locations <- DBI::dbGetQuery(con, "SELECT * FROM locations")
  DBI::dbDisconnect(con)
  expect_gt(nrow(locations), 1)
})
