# Tests depend on db connection so can't be run on CRAN or CI
skip_on_ci()
skip_on_cran()

test_that("workbook is generated", {
  if (file.exists("//carver/infosys/EQWin/WR/DB/Water Resources.mdb")) {
    dir <- tempdir()
    unlink(dir, recursive = TRUE)
    # Delete files in tempdir() on exit
    on.exit(unlink(dir, recursive = TRUE))
    suppressMessages(EQWinReport("1991-02-12", stations = c("(CM)CM-u/s"), parameters = c("pH-F"), stnStds = FALSE, save_path = dir))
    list.files(dir, pattern = ".xlsx", full.names = TRUE) %>% expect_length(1)
  }
})
