skip_if_offline()
skip_on_ci()
skip_on_cran()

test_that("YOWNplot console output is as expected", {
  plot <- suppressMessages(YOWNplot(AQID = "YOWN-0101", timeSeriesID = "Wlevel_bgs.Calculated", chartXinterval = "auto", dateRange = c("2020/01/01", "2020/02/01"), stats = FALSE, smooth = FALSE, saveTo = NULL, login = Sys.getenv(c("AQUSER", "AQPASS")), server = "https://yukon.aquaticinformatics.net/AQUARIUS"))
  vdiffr::expect_doppelganger("console output ok", plot)
})
