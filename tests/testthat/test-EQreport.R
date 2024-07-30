test_that("workbook is generated", {
  if (file.exists("X:/EQWin/WR/DB/Water Resources.mdb")) {
    dir <- tempdir()
    unlink(dir, recursive = TRUE)
    # Delete files in tempdir() on exit
    on.exit(unlink(dir, recursive = TRUE))
    suppressMessages(EQreport("1991-02-12", stations = c("(CM)CM-u/s"), parameters = c("pH-F"), stnStds = FALSE, save_path = dir))
    list.files(dir, pattern = ".xlsx", full.names = TRUE) %>% expect_length(1)
  }
})
