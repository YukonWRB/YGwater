logger_fixture <- function(stem, ext) {
  test_path("fixtures", "logger_file_data", paste0(stem, ".", ext))
}

read_hoboware_csv <- function(stem) {
  data.table::fread(
    logger_fixture(stem, "csv"),
    skip = 1,
    showProgress = FALSE
  )
}

hoboware_temperature_c <- function(csv) {
  temp_col <- grep("^Temp,", names(csv), value = TRUE)
  stopifnot(length(temp_col) == 1L)
  values <- csv[[temp_col]]
  if (grepl("\u00B0F", temp_col, fixed = TRUE)) {
    values <- (values - 32) * 5 / 9
  }
  values[!is.na(values)]
}

hoboware_numeric_column <- function(csv, pattern) {
  col <- grep(pattern, names(csv), value = TRUE)
  stopifnot(length(col) == 1L)
  csv[[col]]
}

expect_utc_datetime <- function(x) {
  expect_s3_class(x, "POSIXct")
  expect_identical(attr(x, "tzone"), "UTC")
  expect_false(any(is.na(x)))
}

expect_rmse_below <- function(actual, expected, threshold) {
  n <- min(length(actual), length(expected))
  rmse <- sqrt(mean((actual[seq_len(n)] - expected[seq_len(n)])^2))
  expect_lt(rmse, threshold)
}

test_that("logger file reader converts Solinst XLE data", {
  temperature_col <- "Temperature (\u00B0C)"
  conductivity_col <- "Conductivity (\u00B5S/cm)"
  res <- read_logger_file_data(
    test_path("fixtures/logger_reads/1071079_YOWN-0000_TEST_2020_05_07.xle")
  )

  expect_s3_class(res, "data.frame")
  expect_named(res, c("datetime", "Level (m)", temperature_col, conductivity_col))
  expect_utc_datetime(res$datetime)
  expect_false(any(is.na(res$`Level (m)`)))
  expect_false(any(is.na(res[[temperature_col]])))
  expect_false(any(is.na(res[[conductivity_col]])))
  expect_match(
    attr(res, "logger_timezone_note"),
    "no offset shift was applied",
    fixed = TRUE
  )
})

test_that("logger file reader converts VuSitu HTML data with UTC offset", {
  temperature_col <- "Temperature (\u00B0C)"
  html_file <- logger_fixture(
    paste0(
      "VuSitu_Log_2025-08-25_18-00-00_YOWN-1609_Yukon_River_CG_",
      "Log_2025-08-25_YOWN-1609"
    ),
    "html"
  )

  res <- read_logger_file_data(html_file, file_type = "html")

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 4923L)
  expect_named(res, c(
    "datetime",
    "Pressure (m)",
    temperature_col,
    "Depth (m)"
  ))
  expect_utc_datetime(res$datetime)
  expect_identical(
    format(res$datetime[[1L]], "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    "2025-08-26 01:00:00"
  )
  expect_equal(res$`Pressure (m)`[[1L]], 10.3079, tolerance = 0.0001)
  expect_match(attr(res, "logger_timezone_note"), "UTC-07:00", fixed = TRUE)
})

test_that("logger file reader rejects files that do not match logger formats", {
  html_file <- tempfile(fileext = ".html")
  writeLines(
    c(
      "<html><body><table>",
      '<tr class="dataHeader"><td>Date Time</td><td>Temperature (\u00B0C)</td></tr>',
      '<tr class="data"><td>2025-08-25 18:00:00</td><td>10</td></tr>',
      "</table></body></html>"
    ),
    html_file,
    useBytes = TRUE
  )

  xle_file <- tempfile(fileext = ".xle")
  writeLines("<not_xle><Data /></not_xle>", xle_file, useBytes = TRUE)

  hobo_file <- tempfile(fileext = ".hobo")
  writeBin(charToRaw("not a HOBO file"), hobo_file)

  expect_error(
    read_logger_file_data(html_file, file_type = "html"),
    "InSitu/VuSitu HTML logger file",
    fixed = TRUE
  )
  expect_error(
    read_logger_file_data(xle_file, file_type = "xle"),
    "Solinst XLE logger file",
    fixed = TRUE
  )
  expect_error(
    read_logger_file_data(hobo_file, file_type = "hobo"),
    "Onset HOBO binary file",
    fixed = TRUE
  )
})

test_that("logger file reader converts TidbiT HOBO endpoints", {
  temperature_col <- "Temperature (\u00B0C)"
  samples <- data.table::data.table(
    stem = c(
      "2023_07_WRB_Liard_Tidbit",
      "2024_08_WRB_Liard_Tidbit",
      "WRBTidbit_Yukon_US_of_Takhini"
    )
  )

  for (i in seq_len(nrow(samples))) {
    csv <- read_hoboware_csv(samples$stem[[i]])
    expected_temperature <- hoboware_temperature_c(csv)
    res <- read_logger_file_data(
      logger_fixture(samples$stem[[i]], "hobo")
    )

    expect_s3_class(res, "data.frame")
    expect_named(res, c("datetime", temperature_col))
    expect_utc_datetime(res$datetime)
    expect_equal(nrow(res), length(expected_temperature))
    expect_equal(
      res[[temperature_col]][[1L]],
      expected_temperature[[1L]],
      tolerance = 0.02
    )
    expect_equal(
      res[[temperature_col]][[nrow(res)]],
      expected_temperature[[length(expected_temperature)]],
      tolerance = 0.02
    )
    expect_rmse_below(res[[temperature_col]], expected_temperature, 0.005)
    expect_match(attr(res, "logger_timezone_note"), "UTC-07:00", fixed = TRUE)
  }
})

test_that("logger file reader converts U24 HOBO low/full/temp data", {
  low_col <- "Low Range Conductivity (\u00B5S/cm)"
  full_col <- "Full Range Conductivity (\u00B5S/cm)"
  temperature_col <- "Temperature (\u00B0C)"
  stem <- "2023_07_WRB_Liard_U22"

  csv <- read_hoboware_csv(stem)
  res <- read_logger_file_data(logger_fixture(stem, "hobo"))

  expect_s3_class(res, "data.frame")
  expect_named(res, c(
    "datetime",
    low_col,
    full_col,
    temperature_col
  ))
  expect_utc_datetime(res$datetime)
  expect_equal(nrow(res), nrow(csv))
  expect_rmse_below(
    res[[low_col]],
    hoboware_numeric_column(csv, "^Low Range"),
    0.05
  )
  expect_rmse_below(
    res[[full_col]],
    hoboware_numeric_column(csv, "^Full Range"),
    0.05
  )
  expect_rmse_below(
    res[[temperature_col]],
    hoboware_temperature_c(csv),
    0.01
  )
})

test_that("logger file reader converts U24 HOBO full/temp data", {
  full_col <- "Full Range Conductivity (\u00B5S/cm)"
  temperature_col <- "Temperature (\u00B0C)"
  stem <- "20230614_WRB_Yukon_US_of_Takhini"

  csv <- read_hoboware_csv(stem)
  res <- read_logger_file_data(logger_fixture(stem, "hobo"))

  expect_s3_class(res, "data.frame")
  expect_named(res, c(
    "datetime",
    full_col,
    temperature_col
  ))
  expect_utc_datetime(res$datetime)
  expect_equal(nrow(res), nrow(csv))
  expect_rmse_below(
    res[[full_col]],
    hoboware_numeric_column(csv, "^Full Range"),
    0.06
  )
  expect_rmse_below(
    res[[temperature_col]],
    hoboware_temperature_c(csv),
    0.01
  )
})
