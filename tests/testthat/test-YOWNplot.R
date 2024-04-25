test_that("YOWNplot console output is as expected", {
  skip_on_cran()
  skip_on_ci()
  plot <- suppressMessages(YOWNplot(AQID = "YOWN-0101", timeSeriesID = "Wlevel_bgs.Calculated", chartXinterval = "auto", dateRange = c("2020/01/01", "2020/02/01"), stats = FALSE, smooth = FALSE, saveTo = NULL, login = Sys.getenv(c("AQUSER", "AQPASS")), server = "https://yukon.aquaticinformatics.net/AQUARIUS"))
  vdiffr::expect_doppelganger("console output ok", plot)
})

test_that("YOWNplot saved output is as expected", {
  skip_on_cran()
  skip_on_ci()
  dir <- paste0(tempdir(), "/YOWNplots")
  unlink(dir, recursive = TRUE)
  dir.create(dir)
  plot <- suppressMessages(YOWNplot(AQID = "YOWN-0101", timeSeriesID = "Wlevel_bgs.Calculated", chartXinterval = "auto", dateRange = c("2020/01/01", "2020/02/01"), stats = FALSE, smooth = FALSE, saveTo = dir, format = "png", login = Sys.getenv(c("AQUSER", "AQPASS")), server = "https://yukon.aquaticinformatics.net/AQUARIUS"))
  path <- list.files(paste0(dir, "/YOWN-0101"), full.names = TRUE)
  expect_snapshot_file(path)
  unlink(dir, recursive = TRUE)
})


