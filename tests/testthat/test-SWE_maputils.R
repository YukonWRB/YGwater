test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("get_most_recent_date returns correct date", {
  ts <- data.frame(
    datetime = as.POSIXct(
      c("2024-01-01", "2024-01-02", "2024-01-03"),
      tz = "UTC"
    ),
    value = c(100, NA, 150)
  )
  expect_equal(get_most_recent_date(ts), as.POSIXct("2024-01-03", tz = "UTC"))
})

test_that("get_datetime returns first day of month", {
  expect_equal(get_datetime(2025, 3), as.POSIXct("2025-03-01", tz = "UTC"))
})

test_that("initialize_visualization_parameters returns expected structure", {
  viz <- initialize_visualization_parameters()
  expect_true(is.list(viz))
  expect_true("basins" %in% names(viz))
  expect_true("surveys" %in% names(viz))
  expect_true("pillows" %in% names(viz))
})

test_that("calculate_historic_daily_median returns expected list", {
  ts <- data.frame(
    datetime = as.POSIXct(c("2020-03-01", "2021-03-01", "2022-03-01")),
    A = c(100, 110, 120),
    B = c(200, 210, 220)
  )
  result <- calculate_historic_daily_median(ts, lookback_year = 2020)
  expect_true(is.list(result))
  expect_true("historic_median" %in% names(result))
  expect_true("relative_swe" %in% names(result))
})

test_that("get_swe_state extracts values for correct date", {
  ts <- data.frame(
    datetime = as.POSIXct(c("2025-03-01", "2025-04-01")),
    "101" = c(100, 200),
    "102" = c(150, 250)
  )
  metadata <- data.frame(timeseries_id = c(101, 102))
  data <- list(
    timeseries = list(
      swe = ts,
      historic_median = ts,
      relative_swe = ts
    ),
    metadata = metadata
  )
  result <- get_swe_state(data, 2025, 3, "timeseries_id")
  expect_equal(result$swe, c(100, 150))
})

test_that("load_snowcourse_factors merges location_id", {
  metadata <- data.frame(location = "A", location_id = 1)
  factors <- data.frame(location = "A", Basin1 = 0.5, stringsAsFactors = FALSE)
  utils::write.csv(
    factors,
    file = tempfile("snowcourse_factors.csv"),
    row.names = FALSE
  )
  # Simulate system.file by copying to tempdir and patching system.file
  old_system_file <- getOption("system.file")
  options(system.file = function(...) tempfile("snowcourse_factors.csv"))
  result <- load_snowcourse_factors(metadata)
  expect_true("location_id" %in% names(result))
  options(system.file = old_system_file)
})

test_that("create_continuous_plot_popup returns HTML string", {
  ts <- data.frame(datetime = as.POSIXct("2025-03-01"), value = 100)
  html <- create_continuous_plot_popup(ts, 2025, NULL)
  expect_true(is.character(html))
  expect_true(grepl("<img", html))
})

test_that("create_discrete_plot_popup returns HTML string", {
  ts <- data.frame(datetime = as.POSIXct("2025-03-01"), value = 100)
  html <- create_discrete_plot_popup(ts)
  expect_true(is.character(html))
  expect_true(grepl("<img", html))
})
