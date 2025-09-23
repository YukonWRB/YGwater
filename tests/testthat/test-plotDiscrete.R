# Tests depend on snapshots so can't be inspected on CRAN
skip_on_cran()

# Note: these tests depend on installation of Python and a few libraries. This is taken care of in the setup.R file within the testthat folder.

test_that("discrete plots work from EQWin DB", {
  # Ensure that dbPath can be reached
  dbPath = "//env-fs/env-data/corp/water/Data/Databases_virtual_machines/databases/EQWinDB/WaterResourcesEG.mdb"

  if (!file.exists(dbPath)) {
    skip(paste("Database not found at", dbPath))
  }

  # With locGrp and paramGrp
  plot <- plotDiscrete(
    start = "2023-06-01",
    end = "2023-07-01",
    target_datetime = FALSE,
    locations = c("(EG)W29"),
    locGrp = NULL,
    parameters = c("CN-WAD", "Co-T", "Cu-T"),
    paramGrp = NULL,
    log = TRUE,
    facet_on = 'params',
    rows = 'auto',
    dbSource = "EQ",
    dbPath = dbPath
  )
  expect_s3_class(plot, "plotly")
  expect_snapshot(plot$x$data)
})

test_that("two location, two parameter facet on parameter", {
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test1.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotDiscrete(
    start = "2020-01-01",
    end = "2023-07-01",
    locations = c(46, 44),
    parameters = c("snow water equivalent", "snow depth"),
    dbSource = "AC",
    facet_on = "params"
  )
  expect_s3_class(plot, "plotly")
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("two location, two parameter facet on location", {
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test2.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotDiscrete(
    start = "2020-01-01",
    end = "2023-07-01",
    locations = c(46, 44),
    parameters = c("snow water equivalent", "snow depth"),
    dbSource = "AC",
    facet_on = "locs"
  )
  expect_s3_class(plot, "plotly")
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("log scale", {
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test3.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotDiscrete(
    start = "2020-01-01",
    end = "2023-07-01",
    locations = c(46),
    parameters = c("snow water equivalent", "snow depth"),
    dbSource = "AC",
    facet_on = "locs",
    log = TRUE
  )
  expect_s3_class(plot, "plotly")
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("scale arguments", {
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test4.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotDiscrete(
    start = "2020-01-01",
    end = "2023-07-01",
    locations = c(46),
    parameters = c("snow water equivalent", "snow depth"),
    dbSource = "AC",
    facet_on = "locs",
    point_scale = 2,
    axis_scale = 2,
    legend_scale = 2
  )
  expect_s3_class(plot, "plotly")
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("legend horizontal", {
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test5.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotDiscrete(
    start = "2020-01-01",
    end = "2023-07-01",
    locations = c(46),
    parameters = c("snow water equivalent", "snow depth"),
    dbSource = "AC",
    facet_on = "locs",
    legend_position = "h"
  )
  expect_s3_class(plot, "plotly")
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("xy grids", {
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test6.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotDiscrete(
    start = "2020-01-01",
    end = "2023-07-01",
    locations = c(46),
    parameters = c("snow water equivalent", "snow depth"),
    dbSource = "AC",
    facet_on = "locs",
    gridy = TRUE,
    gridx = TRUE
  )
  expect_s3_class(plot, "plotly")
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("french text", {
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test7.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotDiscrete(
    start = "2020-01-01",
    end = "2023-07-01",
    locations = c(46),
    parameters = c("snow water equivalent", "snow depth"),
    dbSource = "AC",
    facet_on = "locs",
    lang = "fr"
  )
  expect_s3_class(plot, "plotly")
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})
