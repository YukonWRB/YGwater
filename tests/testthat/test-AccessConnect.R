test_that("Access connection works", {
  skip_on_ci()
  skip_on_cran()
  if (file.exists("//carver/infosys/EQWin/WaterResources.mdb")) {
    con <- AccessConnect(path = "//carver/infosys/EQWin/WaterResources.mdb", silent = TRUE)
    tbls <- DBI::dbListTables(con)
    expect_gt(length(tbls), 0)
    DBI::dbDisconnect(con)
  } else {
    skip("Access database file not found, skipping test.")
  }
})
