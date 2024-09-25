test_that("Access connection works", {
  skip_on_ci()
  skip_on_cran()
  if (file.exists("//env-fs/env-data/corp/water/Data/Databases_virtual_machines/databases/EQWinDB/WaterResources.mdb")) {
    con <- AccessConnect(path = "//env-fs/env-data/corp/water/Data/Databases_virtual_machines/databases/EQWinDB/WaterResources.mdb", silent = TRUE)
    tbls <- DBI::dbListTables(con)
    expect_gt(length(tbls), 0)
    DBI::dbDisconnect(con)
  }
})
