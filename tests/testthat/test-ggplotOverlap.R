# Tests depend on DB connection and create snapshots
skip_on_cran()

old_warn <- getOption("warn")
options(warn = -1)
on.exit(options(warn = old_warn), add = TRUE)

test_that("continuous level plot is as expected for full year with numeric startDay and endDay when saved to a file", {
  skip_on_ci()
  
  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test1.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)
  
  plot <- ggplotOverlap("09EA004", "water level", startDay = 1, endDay = 365, years = "2022", historic_range = "last", gridx = FALSE)
  # Save the plot as png
  suppressMessages(ggplot2::ggsave(filename = path, plot = plot, width = 10, height = 6, dpi = 300, device = ragg::agg_png))
  
  expect_snapshot_file(path)
})

test_that("french labels work with full year", {
  skip_on_ci()
  
  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test2.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)
  
  plot <- ggplotOverlap("09EA004", "water level", startDay = 1, endDay = 365, years = "2022", historic_range = "last", lang = "fr", gridx = FALSE)
  ggplot2::ggsave(filename = path, plot = plot, width = 10, height = 6, dpi = 300, device = ragg::agg_png)
  
  expect_snapshot_file(path)
})

test_that("continuous level plot is as expected for full year with numeric startDay and endDay when output to console", {
  skip_on_ci()
  
  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test3.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)
  
  plot <- suppressWarnings(ggplotOverlap("09EA004", "water level", startDay = 1, endDay = 365, years = "2022", historic_range = "last", gridx = TRUE, gridy = TRUE))
  ggplot2::ggsave(filename = path, plot = plot, width = 10, height = 6, dpi = 300, device = ragg::agg_png)
  
  expect_snapshot_file(path)
})

test_that("continuous level plot is as expected for full year with character startDay and endDay", {
  skip_on_ci()
  
  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test4.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)
  
  plot <- ggplotOverlap("09EA004", "water level", startDay = "2023-01-01", endDay = "2023-12-31", years = "2021", save_path = NULL, historic_range = "last", gridx = FALSE)
  ggplot2::ggsave(filename = path, plot = plot, width = 10, height = 6, dpi = 300, device = ragg::agg_png)
  
  expect_snapshot_file(path)
})

test_that("continuous flow plot is as expected for full year with numeric startDay and endDay", {
  skip_on_ci()
  
  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test5.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)
  
  plot <- ggplotOverlap("09EA004", "flow", startDay = 1, endDay = 365, years = "2020", save_path = NULL, historic_range = "last", gridx = FALSE)
  ggplot2::ggsave(filename = path, plot = plot, width = 10, height = 6, dpi = 300, device = ragg::agg_png)
  
  expect_snapshot_file(path)
})

test_that("overlaping year plot throws no error when years is NULL", {
  skip_on_ci() # The latest year is not in the test database
  
  expect_no_error(suppressWarnings(ggplotOverlap("09AA-M1", "snow water equivalent", startDay = "2023-09-01", endDay = "2023-05-31", return_months = c(4,5), historic_range = "last", gridx = FALSE)))
})

test_that("SWE plot works when overlaping new year, dates as character", {
  skip_on_ci()
  
  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test6.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)
  
  plot <- ggplotOverlap("09AA-M1", "snow water equivalent", startDay = "2023-09-01", endDay = "2023-05-31", years = "2022", save_path = NULL, return_months = c(4,5), historic_range = "last", datum = FALSE, gridx = FALSE)
  ggplot2::ggsave(filename = path, plot = plot, width = 10, height = 6, dpi = 300, device = ragg::agg_png)
  
  expect_snapshot_file(path)
})

test_that("depth plot works when overlaping new year, dates as numeric", {
  skip_on_ci()
  
  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test7.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)
  
  plot <- ggplotOverlap("09AA-M1", "snow depth", startDay = 250, endDay = 150, years = "2022", save_path = NULL, return_months = c(4,5), historic_range = "last", datum = FALSE, gridx = FALSE)
  ggplot2::ggsave(filename = path, plot = plot, width = 10, height = 6, dpi = 300, device = ragg::agg_png)
  
  expect_snapshot_file(path)
})

test_that("french labels work with overlaping years", {
  skip_on_ci()
  
  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test8.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)
  
  plot <- ggplotOverlap("09AA-M1", "snow depth", startDay = 250, endDay = 150, years = "2022", save_path = NULL, return_months = c(4,5), historic_range = "last", lang = "fr", gridx = FALSE)
  ggplot2::ggsave(filename = path, plot = plot, width = 10, height = 6, dpi = 300, device = ragg::agg_png)
  
  expect_snapshot_file(path)
})

test_that("continuous level plot is as expected for multiple years when output to console", {
  skip_on_ci()
  
  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test9.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)
  
  plot <- suppressWarnings(ggplotOverlap("09EA004", "water level", startDay = 1, endDay = 365, years = c(2021,2022), historic_range = "last", gridx = FALSE))
  ggplot2::ggsave(filename = path, plot = plot, width = 10, height = 6, dpi = 300, device = ragg::agg_png)
  
  expect_snapshot_file(path)
})

test_that("too big year error message happens", {
  expect_error(suppressWarnings(ggplotOverlap("09EA004", "water level", startDay = 1, endDay = 365, years = lubridate::year(Sys.Date()) + 2, gridx = FALSE)))
})

#Test for historical range and return periods able to flex.
test_that("historic range can be < requested year", {
  skip_on_ci()
  
  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test10.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)
  
  plot <- suppressWarnings(ggplotOverlap("09EA004", "water level", startDay = 1, endDay = 365, years = c(2022), historic_range = "all", gridx = FALSE))
  ggplot2::ggsave(filename = path, plot = plot, width = 10, height = 6, dpi = 300, device = ragg::agg_png)
  
  expect_snapshot_file(path)
})
#Test for historical range and return periods able to flex.
test_that("returns can be for yrs > requested year", {
  skip_on_ci()
  
  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test11.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)
  
  plot <- suppressWarnings(ggplotOverlap("09EA004", "water level", startDay = 1, endDay = 365, years = c(2021), historic_range = "all", return_max_year = 2022, gridx = FALSE))
  ggplot2::ggsave(filename = path, plot = plot, width = 10, height = 6, dpi = 300, device = ragg::agg_png)
  
  expect_snapshot_file(path)
})
