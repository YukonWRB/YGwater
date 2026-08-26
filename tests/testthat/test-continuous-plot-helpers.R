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
