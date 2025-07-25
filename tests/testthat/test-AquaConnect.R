test_that("Connection can be made", {
  skip_on_cran()
  
  expect_message({
    con <- AquaConnect()
    on.exit(DBI::dbDisconnect(con))
  })
  
  
  # a trivial query must work
  expect_identical(
    DBI::dbGetQuery(con, "SELECT 1 AS x")$x,
    1L
  )
  
})
