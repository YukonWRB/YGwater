test_that("error if no data", {
  if (file.exists("X:/EQWin/WR/DB/Water Resources.mdb")) {
    dir <- tempdir()
    unlink(dir, recursive = TRUE)
    # Delete files in tempdir() on exit
    on.exit(unlink(dir, recursive = TRUE))
    expect_error(EQWinData(start = "2024-07-01 00:00", end = Sys.Date(), stations = c("(CM)CM-u/s"), parameters = c("pH-F"), format = 'wide', stds = c("CCME_LT", "CCME_ST"), stnStds = TRUE, save_path = dir))
  }
})

test_that("three workbooks are produced", {
  if (file.exists("X:/EQWin/WR/DB/Water Resources.mdb")) {
    dir <- tempdir()
    unlink(dir, recursive = TRUE)
    # Delete files in tempdir() on exit
    on.exit(unlink(dir, recursive = TRUE))
    suppressMessages(EQWinData(start = "2024-01-01 00:00", end = Sys.Date(), stations = c("(EG)W23"), parameters = c("pH-F"), format = 'wide', stds = c("CCME_LT", "CCME_ST"), stnStds = TRUE, save_path = dir))
    list.files(dir, pattern = ".xlsx") %>% expect_length(3)
  }
})
