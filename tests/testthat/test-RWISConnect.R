test_that("RWIS connection works", {
  con <- RWISConnect()
  stations <- DBI::dbGetQuery(con, "SELECT name FROM stations_station")
  DBI::dbDisconnect(con)
  expect_gt(nrow(stations), 1)
})
