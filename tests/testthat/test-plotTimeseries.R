# Note: these tests depend on installation of Python and a few libraries. This is taken care of in the setup.R file within the testthat folder.

con <- AquaConnect()
on.exit(DBI::dbDisconnect(con), add = TRUE)

test_that("timeseries plot is as expected for one year with no historic range or slider", {
  skip_on_cran()
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test1.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotTimeseries(
    location = "09EA004",
    parameter = "water level",
    start_date = "2022-01-01",
    end_date = "2023-01-01",
    historic_range = FALSE,
    slider = FALSE,
    con = con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("timeseries plot is as expected for one year with no historic range", {
  skip_on_cran()
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test2.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotTimeseries(
    location = "09EA004",
    parameter = "water level",
    start_date = "2022-01-01",
    end_date = "2023-01-01",
    historic_range = FALSE,
    con = con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("timeseries plot is as expected for one year with historic range", {
  skip_on_cran()
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test3.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotTimeseries(
    location = "09EA004",
    parameter = "water level",
    start_date = "2022-01-01",
    end_date = "2023-01-01",
    con = con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("French timeseries plot is as expected for one year with historic range and slider", {
  skip_on_cran()
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test4.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotTimeseries(
    location = "09EA004",
    parameter = "water level",
    start_date = "2022-01-01",
    end_date = "2023-01-01",
    lang = "fr",
    con = con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("French timeseries plot is as expected for one year with historic range and no slider", {
  skip_on_cran()
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test5.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotTimeseries(
    location = "09EA004",
    parameter = "water level",
    start_date = "2022-01-01",
    end_date = "2023-01-01",
    lang = "fr",
    slider = FALSE,
    con = con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("grades, approvals, qualifiers are displayed", {
  skip_on_cran()
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test6.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotTimeseries(
    location = "09EA004",
    parameter = "water level",
    start_date = "2022-01-01",
    end_date = "2023-01-01",
    lang = "fr",
    slider = FALSE,
    grades = TRUE,
    qualifiers = TRUE,
    approvals = TRUE,
    con = con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("one of grades, approvals, qualifiers is displayed", {
  skip_on_cran()
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test7.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotTimeseries(
    location = "09EA004",
    parameter = "water level",
    start_date = "2022-01-01",
    end_date = "2023-01-01",
    lang = "fr",
    slider = FALSE,
    grades = TRUE,
    con = con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("returned plot data is as expected", {
  skip_on_cran()

  plot <- plotTimeseries(
    location = "09EA004",
    parameter = "water level",
    start_date = "2022-01-01",
    end_date = "2023-01-01",
    lang = "fr",
    slider = FALSE,
    data = TRUE,
    con = con
  )$data
  expect_type(plot, "list")
  expect_named(plot, c("trace_data", "range_data"))
  expect_named(plot$trace_data, c("datetime", "value", "imputed"))
  expect_named(plot$range_data, c("datetime", "min", "max", "q75", "q25"))
})

test_that("returned plot data is as expected with all parameters specified", {
  skip_on_cran()

  plot <- plotTimeseries(
    location = "09EA004",
    sub_location = NULL,
    parameter = "water level",
    record_rate = 5 * 60, # 5 minutes for WSC data
    aggregation_type = "instantaneous",
    z = NULL,
    z_approx = NULL,
    start_date = "2022-01-01",
    end_date = "2023-01-01",
    lang = "fr",
    slider = FALSE,
    data = TRUE,
    datum = FALSE,
    custom_title = NULL,
    filter = 20,
    unusable = FALSE,
    grades = TRUE,
    approvals = TRUE,
    qualifiers = TRUE,
    historic_range = TRUE,
    con = con
  )$data
  expect_type(plot, "list")
  expect_named(plot, c("trace_data", "range_data"))
  expect_named(plot$trace_data, c("datetime", "value", "imputed"))
  expect_named(plot$range_data, c("datetime", "min", "max", "q75", "q25"))
})

test_that("plotTimeseries works when given only a timeseries_id", {
  skip_on_cran()

  # Find an appropriate timeseries_id
  wl <- DBI::dbGetQuery(
    con,
    "SELECT parameter_id FROM parameters WHERE param_name = 'water level' LIMIT 1;"
  )[1, 1]
  tsid <- DBI::dbGetQuery(
    con,
    "SELECT timeseries_id FROM timeseries WHERE parameter_id = $1 AND location = '09EA004';",
    params = list(wl)
  )[1, 1]
  plot <- plotTimeseries(
    timeseries_id = tsid,
    start_date = "2022-01-01",
    end_date = "2023-01-01",
    historic_range = TRUE,
    slider = FALSE,
    data = TRUE,
    con = con
  )
  expect_s3_class(plot$plot, "plotly")
  expect_named(plot$data, c("trace_data", "range_data"))
  expect_named(plot$data$trace_data, c("datetime", "value", "imputed"))
  expect_named(plot$data$range_data, c("datetime", "min", "max", "q75", "q25"))

  # Skip snapshot test on CI
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test8.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plotly::save_image(plot$plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

# Test that plotTimeseries plots raw and uncorrected data
test_that("plotTimeseries plots raw and corrected data", {
  skip_on_cran()

  # Find an appropriate timeseries_id
  wl <- DBI::dbGetQuery(
    con,
    "SELECT parameter_id FROM parameters WHERE param_name = 'water level' LIMIT 1;"
  )[1, 1]
  tsid <- DBI::dbGetQuery(
    con,
    "SELECT timeseries_id FROM timeseries WHERE parameter_id = $1 AND location = '09EA004';",
    params = list(wl)
  )[1, 1]
  plot <- plotTimeseries(
    timeseries_id = tsid,
    start_date = "2022-01-01",
    end_date = "2023-01-01",
    historic_range = TRUE,
    slider = FALSE,
    data = TRUE,
    raw = TRUE,
    con = con
  )
  expect_s3_class(plot$plot, "plotly")
  expect_named(plot$data, c("trace_data", "range_data"))
  expect_named(
    plot$data$trace_data,
    c("datetime", "value", "value_raw", "imputed")
  )
  expect_named(plot$data$range_data, c("datetime", "min", "max", "q75", "q25"))

  # Skip snapshot test on CI
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test9.png")
  path <- pathPrep(path)

  on.exit(unlink(path), add = TRUE)
  plotly::save_image(plot$plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})
