test_that("Access connection works", {
  skip_on_ci()
  skip_on_cran()
  if (file.exists("X:/EQWin/WR/DB/Water Resources.mdb")) {
    con <- AccessConnect(path = "X:/EQWin/WR/DB/Water Resources.mdb", silent = TRUE)
    tbls <- DBI::dbListTables(con)
    expect_gt(length(tbls), 0)
  }
})
