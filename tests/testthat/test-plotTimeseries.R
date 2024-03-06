#Initial checks to make sure things run
if (!check_miniconda_installed()) {
  reticulate::install_miniconda()
}

reticulate::use_condaenv("r-reticulate", required = TRUE)

# Check if python-kaleido is installed and install if necessary
if (!check_conda_package_installed("r-reticulate", "python-kaleido")) {
  
  reticulate::conda_install("r-reticulate", "python-kaleido")
}

# Check if plotly is installed and install if necessary
if (!check_conda_package_installed("r-reticulate", "plotly")) {
  reticulate::conda_install("r-reticulate", "plotly", channel = "plotly")
}

reticulate::use_miniconda('r-reticulate')



test_that("timeseries plot is as expected for one year with no historic range or slider", {
  skip_on_cran()
  skip_on_ci()
  dir <- paste0(tempdir(), "\\plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir)
  path <- paste0(dir, "\\test1.svg")
  path <- gsub("\\\\", "/", path)
  plot <- plotTimeseries("09EA004", "water level", start_date = "2016-01-01", end_date = "2017-01-01", historic_range = FALSE, slider = FALSE)
  plotly::save_image(plot, file = path)
  expect_snapshot_file(path)
})

test_that("timeseries plot is as expected for one year with no historic range", {
  skip_on_cran()
  skip_on_ci()
  dir <- paste0(tempdir(), "\\plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir)
  path <- paste0(dir, "\\test2.svg")
  path <- gsub("\\\\", "/", path)
  plot <- plotTimeseries("09EA004", "water level", start_date = "2016-01-01", end_date = "2017-01-01", historic_range = FALSE)
  plotly::save_image(plot, file = path)
  expect_snapshot_file(path)
})

test_that("timeseries plot is as expected for one year with historic range", {
  skip_on_cran()
  skip_on_ci()
  dir <- paste0(tempdir(), "\\plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir)
  path <- paste0(dir, "\\test3.svg")
  path <- gsub("\\\\", "/", path)
  plot <- plotTimeseries("09EA004", "water level", start_date = "2016-01-01", end_date = "2017-01-01")
  plotly::save_image(plot, file = path)
  expect_snapshot_file(path)
})

test_that("French timeseries plot is as expected for one year with historic range and slider", {
  skip_on_cran()
  skip_on_ci()
  dir <- paste0(tempdir(), "\\plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir)
  path <- paste0(dir, "\\test4.svg")
  path <- gsub("\\\\", "/", path)
  plot <- plotTimeseries("09EA004", "water level", start_date = "2016-01-01", end_date = "2017-01-01", language = "fr")
  plotly::save_image(plot, file = path)
  expect_snapshot_file(path)
})

test_that("French timeseries plot is as expected for one year with historic range and no slider", {
  skip_on_cran()
  skip_on_ci()
  dir <- paste0(tempdir(), "\\plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir)
  path <- paste0(dir, "\\test5.svg")
  path <- gsub("\\\\", "/", path)
  plot <- plotTimeseries("09EA004", "water level", start_date = "2016-01-01", end_date = "2017-01-01", language = "fr", slider = FALSE)
  plotly::save_image(plot, file = path)
  expect_snapshot_file(path)
})
