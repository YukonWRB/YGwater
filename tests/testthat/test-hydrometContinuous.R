test_that("continuous level plot is as expected for full year with numeric startDay and endDay when saved to a file", {
  skip_on_cran()
  skip_on_ci()
  dir <- paste0(tempdir(), "/plots")
  unlink(dir, recursive=TRUE)
  dir.create(dir)
  plot <- suppressWarnings(hydrometContinuous("09EA004", "water level", startDay = 1, endDay = 365, years = "2022", save_path = dir,  historic_range = "last"))
  path <- list.files(dir, full.names=TRUE)
  file.rename(path, paste0(dir, "/level1.png"))
  expect_snapshot_file(paste0(dir, "/level1.png"))
  unlink(dir, recursive=TRUE)
})

test_that("continuous level plot is as expected for full year with numeric startDay and endDay when output to console", {
  skip_on_cran()
  skip_on_ci()
  plot <- suppressWarnings(hydrometContinuous("09EA004", "water level", startDay = 1, endDay = 365, years = "2022", historic_range = "last"))
  vdiffr::expect_doppelganger("full yr numeric start/end", plot)
})

test_that("continuous level plot is as expected for full year with character startDay and endDay", {
  skip_on_cran()
  skip_on_ci()
  dir <- paste0(tempdir(), "/plots")
  dir.create(dir)
  suppressWarnings(hydrometContinuous("09EA004", "water level", startDay = "2023-01-01", endDay = "2023-12-31", years = "2022", save_path = dir, historic_range = "last"))
  path <- list.files(dir, full.names = TRUE)
  file.rename(path, paste0(dir, "/level2.png"))
  expect_snapshot_file(paste0(dir, "/level2.png"))
  unlink(dir, recursive = TRUE)
})

test_that("continuous flow plot is as expected for full year with numeric startDay and endDay", {
  skip_on_cran()
  skip_on_ci()
  dir <- paste0(tempdir(), "/plots")
  dir.create(dir)
  suppressWarnings(hydrometContinuous("09EA004", "water flow", startDay = 1, endDay = 365, years = "2022", save_path = dir, historic_range = "last"))
  path <- list.files(dir, full.names = TRUE)
  file.rename(path, paste0(dir, "/flow1.png"))
  expect_snapshot_file(paste0(dir, "/flow1.png"))
  unlink(dir, recursive = TRUE)
})

test_that("SWE plot works when overlaping new year, dates as character", {
  skip_on_cran()
  skip_on_ci()
  dir <- paste0(tempdir(), "/plots")
  dir.create(dir)
  suppressWarnings(hydrometContinuous("09AA-M1", "SWE", startDay = "2023-09-01", endDay = "2023-05-31", years = "2022", save_path = dir, return_months = c(4,5), historic_range = "last"))
  path <- list.files(dir, full.names=TRUE)
  file.rename(path, paste0(dir, "/SWE1.png"))
  expect_snapshot_file(paste0(dir, "/SWE1.png"))
  unlink(dir, recursive=TRUE)
})

test_that("depth plot works when overlaping new year, dates as numeric", {
  skip_on_cran()
  skip_on_ci()
  dir <- paste0(tempdir(), "/plots")
  dir.create(dir)
  suppressWarnings(hydrometContinuous("09AA-M1", "snow depth", startDay = 250, endDay = 150, years = "2022", save_path = dir, return_months = c(4,5), historic_range = "last"))
  path <- list.files(dir, full.names=TRUE)
  file.rename(path, paste0(dir, "/depth1.png"))
  expect_snapshot_file(paste0(dir, "/depth1.png"))
  unlink(dir, recursive=TRUE)
})

test_that("continuous level plot is as expected for multiple years when output to console", {
  skip_on_cran()
  skip_on_ci()
  plot <- suppressWarnings(hydrometContinuous("09EA004", "water level", startDay = 1, endDay = 365, years = c(2019,2020,2021,2022), historic_range = "last"))
  vdiffr::expect_doppelganger("multi yr numeric start/end", plot)
})

test_that("too big year error message happens", {
  expect_error(hydrometContinuous("09EA004", "water level", startDay = 1, endDay = 365, years = lubridate::year(Sys.Date()) + 2))
})

#Test for historical range and return periods able to flex.
test_that("historic range can be < requested year", {
  skip_on_cran()
  skip_on_ci()
  plot <- suppressWarnings(hydrometContinuous("09EA004", "water level", startDay = 1, endDay = 365, years = c(2018), historic_range = "all"))
  vdiffr::expect_doppelganger("hist range > plotted year", plot)
})
#Test for historical range and return periods able to flex.
test_that("returns can be for yrs > requested year", {
  skip_on_cran()
  skip_on_ci()
  plot <- suppressWarnings(hydrometContinuous("09EA004", "water level", startDay = 1, endDay = 365, years = c(2018), historic_range = "all", return_max_year = 2022))
  vdiffr::expect_doppelganger("hist range > plotted year", plot)
})
