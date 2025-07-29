# Tests depend on snapshots so can't be inspected on CRAN
skip_on_cran()

# Note: these tests depend on installation of Python and a few libraries. This is taken care of in the setup.R file within the testthat folder.

# test_that("discrete plots work from EQWin DB", {
#   
#   # With locGrp and paramGrp
#   plot <- plotDiscrete(start = "2023-06-01", end = "2023-07-01", locations = NULL, locGrp = "QZ Eagle Gold HLF", parameters = NULL, paramGrp = "EG-HLF-failure", log = TRUE, facet_on = 'params', rows = 'auto', dbSource = "EQ", dbPath = "//env-fs/env-data/corp/water/Data/Databases_virtual_machines/databases/EQWinDB/WaterResources.mdb")
# })

test_that("two location, two parameter facet on parameter", {
  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test1.png")
  on.exit(unlink(path), add = TRUE)
  
  plot <- plotDiscrete(start = "2020-01-01", end = "2023-07-01", locations = c(46, 44), parameters = c("snow water equivalent", "snow depth"), dbSource = "AC", facet_on = "params")
  plotly::save_image(plot, file = path, width = 500, height = 500)
  
  expect_snapshot_file(path)
})

test_that("two location, two parameter facet on location", {
  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test2.png")
  on.exit(unlink(path), add = TRUE)
  
  plot <- plotDiscrete(start = "2020-01-01", end = "2023-07-01", locations = c(46, 44), parameters = c("snow water equivalent", "snow depth"), dbSource = "AC", facet_on = "locs")
  plotly::save_image(plot, file = path, width = 500, height = 500)
  
  expect_snapshot_file(path)
})

test_that("log scale", {
  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test3.png")
  on.exit(unlink(path), add = TRUE)
  
  plot <- plotDiscrete(start = "2020-01-01", end = "2023-07-01", locations = c(46), parameters = c("snow water equivalent", "snow depth"), dbSource = "AC", facet_on = "locs", log = TRUE)
  plotly::save_image(plot, file = path, width = 500, height = 500)
  
  expect_snapshot_file(path)
})

test_that("scale arguments", {
  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test4.png")
  on.exit(unlink(path), add = TRUE)
  
  plot <- plotDiscrete(start = "2020-01-01", end = "2023-07-01", locations = c(46), parameters = c("snow water equivalent", "snow depth"), dbSource = "AC", facet_on = "locs", point_scale = 2, axis_scale = 2, legend_scale = 2)
  plotly::save_image(plot, file = path, width = 500, height = 500)
  
  expect_snapshot_file(path)
})

test_that("legend horizontal", {
  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test5.png")
  on.exit(unlink(path), add = TRUE)
  
  plot <- plotDiscrete(start = "2020-01-01", end = "2023-07-01", locations = c(46), parameters = c("snow water equivalent", "snow depth"), dbSource = "AC", facet_on = "locs", legend_position = "h")
  plotly::save_image(plot, file = path, width = 500, height = 500)
  
  expect_snapshot_file(path)
})

test_that("xy grids", {
  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test6.png")
  on.exit(unlink(path), add = TRUE)
  
  plot <- plotDiscrete(start = "2020-01-01", end = "2023-07-01", locations = c(46), parameters = c("snow water equivalent", "snow depth"), dbSource = "AC", facet_on = "locs", gridy = TRUE, gridx = TRUE)
  plotly::save_image(plot, file = path, width = 500, height = 500)
  
  expect_snapshot_file(path)
})

test_that("french text", {
  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test7.png")
  on.exit(unlink(path), add = TRUE)
  
  plot <- plotDiscrete(start = "2020-01-01", end = "2023-07-01", locations = c(46), parameters = c("snow water equivalent", "snow depth"), dbSource = "AC", facet_on = "locs", lang = "fr")
  plotly::save_image(plot, file = path, width = 500, height = 500)
  
  expect_snapshot_file(path)
})
