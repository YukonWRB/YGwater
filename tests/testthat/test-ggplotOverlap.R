# Tests depend on DB connection and create snapshots so can't be run on CRAN or CI
skip_on_ci()
skip_on_cran()

old_warn <- getOption("warn")
options(warn = -1)
on.exit(options(warn = old_warn), add = TRUE)

test_that("continuous level plot is as expected for full year with numeric startDay and endDay when saved to a file", {
  dir <- paste0(tempdir(), "/plots")
  unlink(dir, recursive = TRUE)
  dir.create(dir)
  plot <- ggplotOverlap("09EA004", "water level", startDay = 1, endDay = 365, years = "2022", save_path = dir,  historic_range = "last")
  path <- list.files(dir, full.names = TRUE)
  file.rename(path, paste0(dir, "/level1.png"))
  expect_snapshot_file(paste0(dir, "/level1.png"))
  unlink(dir, recursive = TRUE)
})

test_that("french labels work with full year", {
  plot <- ggplotOverlap("09EA004", "water level", startDay = 1, endDay = 365, years = "2022", save_path = NULL,  historic_range = "last", language = "fr")
  vdiffr::expect_doppelganger("french labels", plot)
})

test_that("continuous level plot is as expected for full year with numeric startDay and endDay when output to console", {
  plot <- suppressWarnings(ggplotOverlap("09EA004", "water level", startDay = 1, endDay = 365, years = "2022", historic_range = "last"))
  vdiffr::expect_doppelganger("full yr numeric start/end", plot)
})

test_that("continuous level plot is as expected for full year with character startDay and endDay", {
  plot <- ggplotOverlap("09EA004", "water level", startDay = "2023-01-01", endDay = "2023-12-31", years = "2022", save_path = NULL, historic_range = "last")
  vdiffr::expect_doppelganger("full yr char start/end", plot)
})

test_that("continuous flow plot is as expected for full year with numeric startDay and endDay", {
  plot <- ggplotOverlap("09EA004", "flow", startDay = 1, endDay = 365, years = "2022", save_path = NULL, historic_range = "last")
  vdiffr::expect_doppelganger("full num start/end", plot)
})

test_that("overlaping year plot throws no error when years is NULL", {
  expect_no_error(suppressWarnings(ggplotOverlap("09AA-M1", "snow water equivalent", startDay = "2023-09-01", endDay = "2023-05-31", return_months = c(4,5), historic_range = "last")))
})

test_that("SWE plot works when overlaping new year, dates as character", {
  plot <- ggplotOverlap("09AA-M1", "snow water equivalent", startDay = "2023-09-01", endDay = "2023-05-31", years = "2022", save_path = NULL, return_months = c(4,5), historic_range = "last", datum = FALSE)
  vdiffr::expect_doppelganger("swe overlaping new year chr dates", plot)
})

test_that("depth plot works when overlaping new year, dates as numeric", {
  plot <- ggplotOverlap("09AA-M1", "snow depth", startDay = 250, endDay = 150, years = "2022", save_path = NULL, return_months = c(4,5), historic_range = "last", datum = FALSE)
  vdiffr::expect_doppelganger("depth overlaping new year num dates", plot)
})

test_that("french labels work with overlaping years", {
  plot <- ggplotOverlap("09AA-M1", "snow depth", startDay = 250, endDay = 150, years = "2022", save_path = NULL, return_months = c(4,5), historic_range = "last", language = "fr")
  vdiffr::expect_doppelganger("french labels overlaping years", plot)
})

test_that("continuous level plot is as expected for multiple years when output to console", {
  plot <- suppressWarnings(ggplotOverlap("09EA004", "water level", startDay = 1, endDay = 365, years = c(2019,2020,2021,2022), historic_range = "last"))
  vdiffr::expect_doppelganger("multi yr numeric start/end", plot)
})

test_that("too big year error message happens", {
  expect_error(suppressWarnings(ggplotOverlap("09EA004", "water level", startDay = 1, endDay = 365, years = lubridate::year(Sys.Date()) + 2)))
})

#Test for historical range and return periods able to flex.
test_that("historic range can be < requested year", {
  plot <- suppressWarnings(ggplotOverlap("09EA004", "water level", startDay = 1, endDay = 365, years = c(2018), historic_range = "all"))
  vdiffr::expect_doppelganger("hist range > plotted year", plot)
})
#Test for historical range and return periods able to flex.
test_that("returns can be for yrs > requested year", {
  plot <- suppressWarnings(ggplotOverlap("09EA004", "water level", startDay = 1, endDay = 365, years = c(2018), historic_range = "all", return_max_year = 2022))
  vdiffr::expect_doppelganger("returns > yr", plot)
})
