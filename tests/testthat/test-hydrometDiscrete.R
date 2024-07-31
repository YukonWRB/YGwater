# Tests depend on snapshots so can't be run on CRAN or CI
skip_on_ci()
skip_on_cran()

#NOTE: as of June 2023 hydrometDiscrete does not accept start/end dates other than 1 and 365. This explained in the help file.

test_that("violin plot is as expected for full year with numeric startDay and endDay", {
  skip_on_cran()
  skip_on_ci()
  dir <- paste0(tempdir(), "/plot")
  unlink(dir, recursive = TRUE)
  dir.create(dir)
  suppressWarnings(hydrometDiscrete("08AA-SC01", "snow water equivalent", startDay = 1, endDay = 365, years = "2022", save_path = dir))
  path <- list.files(dir, full.names = TRUE)
  file.rename(path, paste0(dir, "/snow.png"))
  expect_snapshot_file(paste0(dir, "/snow.png"))
  unlink(dir, recursive = TRUE)
})

test_that("console plot output is as expected", {
  skip_on_cran()
  skip_on_ci()
  plot <- suppressWarnings(hydrometDiscrete("08AA-SC01", "snow water equivalent", startDay = 1, endDay = 365, years = "2022"))
  vdiffr::expect_doppelganger("full year violin", plot)
})

test_that("violin plot is as expected for full year with Date startDay and endDay", {
  skip_on_cran()
  skip_on_ci()
  dir <- paste0(tempdir(), "/plot")
  dir.create(dir)
  suppressWarnings(hydrometDiscrete("08AA-SC01", "snow water equivalent", startDay = "2023-01-01", endDay = "2023-12-31", years = "2022", save_path = dir))
  path <- list.files(dir, full.names = TRUE)
  file.rename(path, paste0(dir, "/snow2.png"))
  expect_snapshot_file(paste0(dir, "/snow2.png"))
  unlink(dir, recursive = TRUE)
})

test_that("box plot is as expected for full year with numeric startDay and endDay", {
  skip_on_cran()
  skip_on_ci()
  dir <- paste0(tempdir(), "/plot")
  dir.create(dir)
  suppressWarnings(hydrometDiscrete("08AA-SC01", "snow water equivalent", startDay = 1, endDay = 365, years = "2022", save_path = dir,  plot_type = "boxplot"))
  path <- list.files(dir, full.names = TRUE)
  file.rename(path, paste0(dir, "/snow3.png"))
  expect_snapshot_file(paste0(dir, "/snow3.png"))
  unlink(dir, recursive = TRUE)
})

test_that("box plot is as expected for full year with Date startDay and endDay", {
  skip_on_cran()
  skip_on_ci()
  dir <- paste0(tempdir(), "/plot")
  dir.create(dir)
  suppressWarnings(hydrometDiscrete("08AA-SC01", "snow water equivalent", startDay = "2023-01-01", endDay = "2023-12-31", years = "2022", save_path = dir, plot_type = "boxplot"))
  path <- list.files(dir, full.names = TRUE)
  file.rename(path, paste0(dir, "/snow4.png"))
  expect_snapshot_file(paste0(dir, "/snow4.png"))
  unlink(dir, recursive = TRUE)
})

test_that("plot scale factor and titles works", {
  skip_on_cran()
  skip_on_ci()
  dir <- paste0(tempdir(), "/plot")
  dir.create(dir)
  suppressWarnings(hydrometDiscrete("08AA-SC01", "snow water equivalent", startDay = "2023-01-01", endDay = "2023-12-31", years = "2022", save_path = dir, plot_type = "boxplot", plot_scale = 2, title = FALSE))
  path <- list.files(dir, full.names = TRUE)
  file.rename(path, paste0(dir, "/snow5.png"))
  expect_snapshot_file(paste0(dir, "/snow5.png"))
  unlink(dir, recursive = TRUE)
})

test_that("depth plots work", {
  skip_on_cran()
  skip_on_ci()
  dir <- paste0(tempdir(), "/plot")
  dir.create(dir)
  suppressWarnings(hydrometDiscrete("08AA-SC01", "snow water equivalent", startDay = "2023-01-01", endDay = "2023-12-31", years = "2022", save_path = dir))
  path <- list.files(dir, full.names = TRUE)
  file.rename(path, paste0(dir, "/snow6.png"))
  expect_snapshot_file(paste0(dir, "/snow6.png"))
  unlink(dir, recursive = TRUE)
})

# Run swe_basin to get discrete_data for the next two tests
discrete_data <- suppressWarnings(SWE_basin(year = 2022,
                           month = c(3,4,5),
                           threshold = 6,
                           csv = FALSE,
                           summarise = FALSE))
discrete_data$datetime <- paste0(discrete_data$year, "-0", discrete_data$month, "-01")
discrete_data <- discrete_data %>% 
  dplyr::filter(location == "Upper_Yukon")

test_that("violin plot is as expected when discrete data is given", {
  skip_on_cran()
  skip_on_ci()
  dir <- paste0(tempdir(), "/plot")
  unlink(dir, recursive = TRUE)
  dir.create(dir)
  # use discrete data in hydrometDiscrete
  hydrometDiscrete(location = NULL, parameter = 'snow water equivalent', years = c(2021, 2022), title = TRUE, plot_type = "boxplot", save_path = dir, discrete_data = discrete_data)
  path <- list.files(dir, full.names = TRUE)
  file.rename(path, paste0(dir, "/snow7.png"))
  expect_snapshot_file(paste0(dir, "/snow7.png"))
  unlink(dir, recursive = TRUE)
})


test_that("linedbox plot is as expected when discrete data is given", {
  skip_on_cran()
  skip_on_ci()
  dir <- paste0(tempdir(), "/plot")
  unlink(dir, recursive = TRUE)
  dir.create(dir)
  # use discrete data in hydrometDiscrete
    hydrometDiscrete(location = "Upper Yukon", 
                     parameter = "snow water equivalent",
                     startDay = 1, 
                     tzone = "MST", 
                     years = 2022, 
                     title = TRUE,
                     custom_title = "Upper Yukon Basin Monthly Snow Course Data",
                     plot_type = "linedbox", 
                     save_path = dir,
                     discrete_data = discrete_data,
                     plot_scale = 1)
    
  path <- list.files(dir, full.names = TRUE)
  file.rename(path, paste0(dir, "/snow8.png"))
  expect_snapshot_file(paste0(dir, "/snow8.png"))
  unlink(dir, recursive = TRUE)
})

rm(discrete_data)

test_that("linedbox plot that starts and ends in different years is as expected when discrete data is given", {
  skip_on_cran()
  skip_on_ci()
  dir <- paste0(tempdir(), "/plot")
  unlink(dir, recursive = TRUE)
  dir.create(dir)
  # Get precipitation data from measurements_continuous
  con <- AquaConnect(silent = TRUE)
  discrete_data <- DBI::dbGetQuery(con, "SELECT * FROM measurements_continuous WHERE timeseries_id = 663")
  attr(discrete_data$datetime, "tzone") <- "MST"
  discrete_data$month <- format(discrete_data$datetime, "%m")
  discrete_data$year <- format(discrete_data$datetime, "%Y")
  discrete_data$units <- "mm"

  hydrometDiscrete(location = NULL, parameter = "Total precipitation",
                   tzone = "MST",
                   years = 2022,
                   startDay = "2022-10-01", # 275
                   endDay = "2023-05-01", #120
                   title = TRUE, 
                   custom_title = "Whitehorse Monthly Precipitation",
                   plot_type = "linedbox", 
                   plot_scale = 1,#params$scale,
                   save_path = dir, 
                   discrete_data = discrete_data, 
                   con = con)
  DBI::dbDisconnect(con)

  path <- list.files(dir, full.names = TRUE)
  file.rename(path, paste0(dir, "/snow9.png"))
  expect_snapshot_file(paste0(dir, "/snow9.png"))
  unlink(dir, recursive = TRUE)
})
