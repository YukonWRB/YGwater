test_that("Snow DB connection works", {
  con <- snowConnect_access()
  courses <- DBI::dbGetQuery(con, "SELECT * FROM SNOW_COURSE")
  DBI::dbDisconnect(con)
  expect_gt(nrow(courses), 1)
})
