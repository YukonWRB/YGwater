test_that("Connection can be made", {
  skip_on_cran()
  skip_on_ci()
  
  expect_message({
    con <- AquaConnect()
    DBI::dbDisconnect(con)
  })
})
