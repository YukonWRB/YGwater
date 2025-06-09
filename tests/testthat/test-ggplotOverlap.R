# Tests depend on DB connection and create snapshots so can't be run on CRAN or CI

old_warn <- getOption("warn")
options(warn = -1)
on.exit(options(warn = old_warn), add = TRUE)

test_that("continuous level plot is as expected for full year with numeric startDay and endDay when saved to a file", {
  skip_on_ci()
  skip_on_cran()
  dir <- paste0(tempdir(), "/plots")
  unlink(dir, recursive = TRUE)
  dir.create(dir)
  plot <- ggplotOverlap("09EA004", "water level", startDay = 1, endDay = 365, years = "2022", save_path = dir,  historic_range = "last", gridx = FALSE)
  path <- list.files(dir, full.names = TRUE)
  file.rename(path, paste0(dir, "/level1.png"))
  expect_snapshot_file(paste0(dir, "/level1.png"))
  unlink(dir, recursive = TRUE)
})

test_that("continuous level plot is as expected for full year with numeric startDay and endDay when saved to a file FROM SQLITE", {
  dir <- paste0(tempdir(), "/plots")
  unlink(dir, recursive = TRUE)
  dir.create(dir)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), test_path("fixtures", "aquacache_test.sqlite"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  plot <- ggplotOverlap("09EA004", "water level", startDay = 1, endDay = 365, years = "2020", save_path = dir,  historic_range = "last", gridx = FALSE, con = con)
  path <- list.files(dir, full.names = TRUE)
  file.rename(path, paste0(dir, "/level1_sqlite.png"))
  expect_snapshot_file(paste0(dir, "/level1_sqlite.png"))
  unlink(dir, recursive = TRUE)
})

test_that("french labels work with full year", {
  skip_on_ci()
  skip_on_cran()
  plot <- ggplotOverlap("09EA004", "water level", startDay = 1, endDay = 365, years = "2022", save_path = NULL,  historic_range = "last", lang = "fr", gridx = FALSE)
  vdiffr::expect_doppelganger("french labels", plot)
})

test_that("continuous level plot is as expected for full year with numeric startDay and endDay when output to console", {
  skip_on_ci()
  skip_on_cran()
  plot <- suppressWarnings(ggplotOverlap("09EA004", "water level", startDay = 1, endDay = 365, years = "2022", historic_range = "last", gridx = TRUE, gridy = TRUE))
  vdiffr::expect_doppelganger("full yr numeric start/end", plot)
})

test_that("continuous level plot is as expected for full year with character startDay and endDay", {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), test_path("fixtures", "aquacache_test.sqlite"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  plot <- ggplotOverlap("09EA004", "water level", startDay = "2023-01-01", endDay = "2023-12-31", years = "2020", save_path = NULL, historic_range = "last", gridx = FALSE, con = con)
  vdiffr::expect_doppelganger("full yr char start/end", plot)
})

test_that("continuous flow plot is as expected for full year with numeric startDay and endDay", {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), test_path("fixtures", "aquacache_test.sqlite"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  plot <- ggplotOverlap("09EA004", "flow", startDay = 1, endDay = 365, years = "2020", save_path = NULL, historic_range = "last", gridx = FALSE, con = con)
  vdiffr::expect_doppelganger("full num start/end", plot)
})

test_that("overlaping year plot throws no error when years is NULL", {
  skip_on_ci()
  skip_on_cran()
  expect_no_error(suppressWarnings(ggplotOverlap("09AA-M1", "snow water equivalent", startDay = "2023-09-01", endDay = "2023-05-31", return_months = c(4,5), historic_range = "last", gridx = FALSE)))
})

test_that("SWE plot works when overlaping new year, dates as character", {
  skip_on_ci()
  skip_on_cran()
  plot <- ggplotOverlap("09AA-M1", "snow water equivalent", startDay = "2023-09-01", endDay = "2023-05-31", years = "2022", save_path = NULL, return_months = c(4,5), historic_range = "last", datum = FALSE, gridx = FALSE)
  vdiffr::expect_doppelganger("swe overlaping new year chr dates", plot)
})

test_that("depth plot works when overlaping new year, dates as numeric", {
  skip_on_ci()
  skip_on_cran()
  plot <- ggplotOverlap("09AA-M1", "snow depth", startDay = 250, endDay = 150, years = "2022", save_path = NULL, return_months = c(4,5), historic_range = "last", datum = FALSE, gridx = FALSE)
  vdiffr::expect_doppelganger("depth overlaping new year num dates", plot)
})

test_that("french labels work with overlaping years", {
  skip_on_ci()
  skip_on_cran()
  plot <- ggplotOverlap("09AA-M1", "snow depth", startDay = 250, endDay = 150, years = "2022", save_path = NULL, return_months = c(4,5), historic_range = "last", lang = "fr", gridx = FALSE)
  vdiffr::expect_doppelganger("french labels overlaping years", plot)
})

test_that("continuous level plot is as expected for multiple years when output to console", {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), test_path("fixtures", "aquacache_test.sqlite"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  plot <- suppressWarnings(ggplotOverlap("09EA004", "water level", startDay = 1, endDay = 365, years = c(2019,2020), historic_range = "last", gridx = FALSE, con = con))
  vdiffr::expect_doppelganger("multi yr numeric start/end", plot)
})

test_that("too big year error message happens", {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), test_path("fixtures", "aquacache_test.sqlite"))
  expect_error(suppressWarnings(ggplotOverlap("09EA004", "water level", startDay = 1, endDay = 365, years = lubridate::year(Sys.Date()) + 2, gridx = FALSE)))
})

#Test for historical range and return periods able to flex.
test_that("historic range can be < requested year", {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), test_path("fixtures", "aquacache_test.sqlite"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  plot <- suppressWarnings(ggplotOverlap("09EA004", "water level", startDay = 1, endDay = 365, years = c(2019), historic_range = "all", gridx = FALSE, con = con))
  vdiffr::expect_doppelganger("hist range > plotted year", plot)
})
#Test for historical range and return periods able to flex.
test_that("returns can be for yrs > requested year", {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), test_path("fixtures", "aquacache_test.sqlite"))
  on.exit(RSQLite::dbDisconnect(con), add = TRUE)
  plot <- suppressWarnings(ggplotOverlap("09EA004", "water level", startDay = 1, endDay = 365, years = c(2019), historic_range = "all", return_max_year = 2022, gridx = FALSE, con = con))
  vdiffr::expect_doppelganger("returns > yr", plot)
})
