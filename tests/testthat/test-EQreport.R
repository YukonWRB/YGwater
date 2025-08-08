# Tests depend on db connection so can't be run on CRAN or CI
skip_on_ci()
skip_on_cran()

test_that("workbook is generated", {
  if (file.exists("//carver/infosys/EQWin/WaterResources.mdb")) {
    dir <- tempdir()
    unlink(dir, recursive = TRUE)
    dir.create(dir, showWarnings = FALSE)
    # Delete files in tempdir() on exit
    on.exit(unlink(dir, recursive = TRUE))
    con  <- AccessConnect("//carver/infosys/EQWin/WaterResources.mdb", silent = TRUE)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    suppressMessages(EQWinReport("1991-02-12", stations = c("(CM)CM-u/s"), parameters = c("pH-F"), stnStds = FALSE, save_path = dir, con = con))
    list.files(dir, pattern = ".xlsx", full.names = TRUE) %>% expect_length(1)
  } else {
    skip("EQWin database not found, skipping test.")
  }
})
