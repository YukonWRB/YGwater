test_that("api(version = 2) builds a plumber2 router without running", {
  skip_if_not_installed("plumber2")

  withr::local_envvar(list(
    aquacacheName = NA,
    aquacacheHost = NA,
    aquacachePort = NA
  ))

  pr <- api(
    version = 2,
    run = FALSE,
    server = "/water-data/api",
    dbName = "aquacache_test"
  )

  expect_s3_class(pr, "Plumber2")

  expect_equal(Sys.getenv("APIaquacacheName"), "aquacache_test")
  expect_equal(Sys.getenv("APIaquacacheHost"), Sys.getenv("aquacacheHost"))
  expect_equal(Sys.getenv("APIaquacachePort"), Sys.getenv("aquacachePort"))
})

test_that("tests for API V2 metadata endpoints", {
  skip_if_not_installed("plumber2")
  skip_if_not_installed("reqres")

  pr <- api(
    version = 2,
    run = FALSE,
    dbName = Sys.getenv("aquacacheName", "testdb"),
    dbHost = Sys.getenv("aquacacheHost", "localhost"),
    dbPort = Sys.getenv("aquacachePort", "5432"),
    dbUser = Sys.getenv("aquacacheUser", "runner"),
    dbPass = Sys.getenv("aquacachePass", "runner")
  )

  get_v2 <- function(url) {
    pr$test_request(reqres:::mock_rook(url = url, method = "get"))
  }

  get_ts <- get_v2("http://example.com/v2/timeseries")

  expect_equal(get_ts$status, 200)

  out <- read.csv(text = get_ts$body)
  out$end_datetime <- as.POSIXct(out$end_datetime, tz = "UTC")
  out$start_datetime <- as.POSIXct(out$start_datetime, tz = "UTC")

  expect_named(
    out,
    c(
      "timeseries_id",
      "location_id",
      "location_name",
      "alias_name",
      "depth_height_m",
      "latitude",
      "longitude",
      "location_elevation",
      "projects",
      "networks",
      "media_type",
      "parameter_name",
      "units",
      "aggregation_type",
      "recording_rate",
      "sensor_priority",
      "start_datetime",
      "end_datetime",
      "note"
    )
  )
  expect_gt(nrow(out), 0)

  get_locs <- get_v2("http://example.com/v2/locations")

  expect_equal(get_locs$status, 200)

  out <- read.csv(text = get_locs$body)

  expect_named(
    out,
    c(
      "location_id",
      "name",
      "alias",
      "location_code",
      "latitude",
      "longitude",
      "elevation",
      "datum",
      "note",
      "projects",
      "networks",
      "fn_names"
    )
  )
  expect_gt(nrow(out), 0)

  invalid_lang <- get_v2("http://example.com/v2/locations?lang=es")

  expect_equal(invalid_lang$status, 400)

  out <- read.csv(text = invalid_lang$body)

  expect_equal(out$status[1], "error")
  expect_match(out$message[1], "Invalid language parameter")
})
