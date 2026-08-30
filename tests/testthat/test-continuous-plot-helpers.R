test_that("historic range export data is NULL when stats are unavailable", {
  expect_null(historic_range_data_for_export(data.frame(), "m"))

  range_data <- data.frame(
    datetime = as.POSIXct("2026-06-01", tz = "UTC"),
    min = 1,
    max = 2,
    q75 = NA_real_,
    q25 = NA_real_
  )
  expect_null(historic_range_data_for_export(range_data, "m"))
})

test_that("sensor priority labels are readable and bilingual", {
  french <- names(YGwater:::data$translations)[
    vapply(
      YGwater:::data$translations,
      function(values) identical(unname(values[["titleCase"]]), "fr"),
      logical(1)
    )
  ][[1L]]

  expect_identical(
    format_sensor_priority_label(
      c(1L, 2L, 3L, NA_integer_),
      "English"
    ),
    c("Primary", "Secondary", "Tertiary", NA_character_)
  )
  expect_identical(
    format_sensor_priority_label(c("1", "2", "3"), french),
    c("Primaire", "Secondaire", "Tertiaire")
  )
})

test_that("unknown sensor priorities remain visible", {
  expect_identical(
    format_sensor_priority_label(c("4", "backup", ""), "English"),
    c("4", "backup", NA_character_)
  )
})

test_that("map location filters preserve the complete continuous table", {
  timeseries <- data.frame(
    timeseries_id = 1:4,
    location_id = c(10, 10, 20, 30)
  )
  locations <- data.table::data.table(
    location_id = c(10, 20, 30),
    name = c("Alpha", "Beta", "Gamma")
  )

  location_value <- continuous_plot_map_location_value(
    10,
    timeseries,
    locations,
    "name"
  )
  searches <- continuous_plot_location_search_columns(
    c("timeseries_id", "location", "parameter"),
    location_value
  )

  expect_identical(location_value, "Alpha")
  expect_identical(jsonlite::fromJSON(searches[[2L]]), "Alpha")
  expect_identical(searches[c(1L, 3L)], c("", ""))
  expect_equal(nrow(timeseries), 4L)
  expect_equal(timeseries$timeseries_id, 1:4)
})

test_that("map location filters reject invalid and non-continuous locations", {
  timeseries <- data.frame(
    timeseries_id = 1:2,
    location_id = c(10, 20)
  )
  locations <- data.frame(
    location_id = c(10, 20, 30),
    name = c("Alpha", "Beta", "Gamma")
  )

  expect_null(continuous_plot_map_location_value(
    30,
    timeseries,
    locations,
    "name"
  ))
  expect_null(continuous_plot_map_location_value(
    "not-an-id",
    timeseries,
    locations,
    "name"
  ))
  expect_identical(
    continuous_plot_location_search_columns(
      c("timeseries_id", "location"),
      NULL
    ),
    c("", "")
  )
})

test_that("duplicate map location names remain unambiguous", {
  timeseries <- data.frame(
    timeseries_id = 1:2,
    location_id = c(10, 20)
  )
  locations <- data.table::data.table(
    location_id = c(10, 20),
    name = c("Same name", "Same name")
  )

  expect_identical(
    continuous_plot_location_labels(locations, "name"),
    c("Same name [10]", "Same name [20]")
  )
  expect_identical(
    continuous_plot_map_location_value(
      20,
      timeseries,
      locations,
      "name"
    ),
    "Same name [20]"
  )
})

test_that("sensor priority labels support additional translation catalogues", {
  translations <- list(
    Test = c(
      sensor_priority_primary = "First",
      sensor_priority_secondary = "Second",
      sensor_priority_tertiary = "Third"
    )
  )

  expect_identical(
    format_sensor_priority_label(
      c(1L, 2L, 3L),
      "Test",
      translations = translations
    ),
    c("First", "Second", "Third")
  )
})

test_that("statistics-period labels use the translation catalogue", {
  translations <- list(
    Test = c(
      stats_period_last_30_years = "Recent baseline",
      stats_period_entire_record = "Complete baseline"
    )
  )

  expect_identical(
    format_stats_period_label(
      c("30yr", "full", "custom"),
      "Test",
      translations = translations
    ),
    c("Recent baseline", "Complete baseline", "custom")
  )
})

test_that("adaptive continuous plot displays sensor priority in both metadata views", {
  module_text <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/client/plot/continuousPlotAdaptive.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    module_text,
    'sensor_priority = tr("sensor_priority", language$language)',
    fixed = TRUE
  )
  expect_match(
    module_text,
    "format_metadata_value(sensor_priority)",
    fixed = TRUE
  )

  cache_text <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/cache_functions.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(cache_text, "ts.sensor_priority", fixed = TRUE)
})

test_that("public statistics-period controls use translated labels", {
  adaptive_text <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/client/plot/continuousPlotAdaptive.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  params_map_text <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/client/map/paramsMap.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    adaptive_text,
    'label = tr("stats_period", language$language)',
    fixed = TRUE
  )
  expect_match(
    adaptive_text,
    "YGwater:::format_stats_period_label",
    fixed = TRUE
  )
  expect_match(
    params_map_text,
    'label = tr("stats_period", language$language)',
    fixed = TRUE
  )
  expect_match(
    params_map_text,
    "YGwater:::format_stats_period_label",
    fixed = TRUE
  )
})

test_that("historic range export data is renamed when stats are available", {
  range_data <- data.frame(
    datetime = as.POSIXct("2026-06-01", tz = "UTC"),
    min = 1,
    max = 2,
    q75 = 1.75,
    q25 = 1.25
  )

  out <- historic_range_data_for_export(range_data, "m")

  expect_s3_class(out, "data.frame")
  expect_named(
    out,
    c(
      "datetime_UTC",
      "historic_min_m",
      "historic_max_m",
      "historic_Q75_m",
      "historic_Q25_m"
    )
  )
  expect_equal(nrow(out), 1L)
})

test_that("continuous QC helper uses current tables without as_of", {
  fake_con <- structure(list(), class = "mock_connection")
  call <- NULL

  out <- testthat::with_mocked_bindings(
    fetch_continuous_qc_intervals(
      fake_con,
      timeseries_id = 123L,
      start_date = as.POSIXct("2024-01-01", tz = "UTC"),
      end_date = as.POSIXct("2024-01-02", tz = "UTC"),
      qc_type = "approval"
    ),
    dbGetQueryDT = function(con, statement, params = NULL, ...) {
      call <<- list(statement = statement, params = params)
      data.table::data.table(
        start_dt = as.POSIXct(character(), tz = "UTC"),
        end_dt = as.POSIXct(character(), tz = "UTC"),
        qc_type_code = character(),
        qc_type_description = character(),
        qc_type_description_fr = character(),
        color_code = character()
      )
    },
    .package = "YGwater"
  )

  expect_s3_class(out, "data.table")
  expect_match(call$statement, "FROM continuous\\.approvals qc")
  expect_match(call$statement, "LEFT JOIN public\\.approval_types qt")
  expect_false(grepl("audit\\.", call$statement))
  expect_equal(call$params[[1]], 123L)
  expect_length(call$params, 3L)
})

test_that("continuous QC helper reconstructs intervals and types with as_of", {
  fake_con <- structure(list(), class = "mock_connection")
  call <- NULL
  as_of <- as.POSIXct("2025-06-01 12:00:00", tz = "UTC")
  start_date <- as.POSIXct("2024-01-01", tz = "UTC")
  end_date <- as.POSIXct("2024-01-02", tz = "UTC")

  testthat::with_mocked_bindings(
    fetch_continuous_qc_intervals(
      fake_con,
      timeseries_id = 456L,
      start_date = start_date,
      end_date = end_date,
      qc_type = "grade",
      as_of = as_of
    ),
    dbGetQueryDT = function(con, statement, params = NULL, ...) {
      call <<- list(statement = statement, params = params)
      data.table::data.table()
    },
    .package = "YGwater"
  )

  expect_match(
    call$statement,
    "FROM audit\\.continuous_qc_intervals_as_of\\("
  )
  expect_match(call$statement, "ARRAY\\[\\$2\\]::INTEGER\\[\\]")
  expect_match(call$statement, "ARRAY\\[\\$5\\]::TEXT\\[\\]")
  expect_identical(call$params[[1]], as_of)
  expect_equal(call$params[[2]], 456L)
  expect_identical(call$params[[3]], start_date)
  expect_identical(call$params[[4]], end_date)
  expect_identical(call$params[[5]], "grade")
})
