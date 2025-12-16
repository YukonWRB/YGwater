skip_on_ci()
skip_on_cran()

test_that("RWIS connection works", {
  con <- RWISConnect()
  stations <- DBI::dbGetQuery(con, "SELECT name FROM stations_station LIMIT 5;")
  DBI::dbDisconnect(con)
  expect_gt(nrow(stations), 1)
})
