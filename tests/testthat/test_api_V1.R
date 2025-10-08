# plumber tests use package callthat to manage the background process
# devtools::install_github("edgararuiz/callthat")

# Notably, this does not test the R function at api.R, but does test the plumber file at plumber/v1.R
# Tests for api.R are in tests/testthat/test-api.R

test_that("tests for API V1", {
  # First set some environment variables for the API to use. These are normally set when the API is launched using api().
  Sys.setenv(APIaquacacheName = Sys.getenv("aquacacheName"))
  Sys.setenv(APIaquacacheHost = Sys.getenv("aquacacheHost"))
  Sys.setenv(APIaquacachePort = Sys.getenv("aquacachePort"))

  # Set up and start the API and a test session
  expect_silent({
    # ------------- Start plumber API -------------------
    local_api <- callthat::call_that_plumber_start(
      system.file("plumber", package = "YGwater"),
      api_file = "v1.R"
    )

    # ------------- Start test session ------------------
    api_session <- callthat::call_that_session_start(local_api)
  })
  on.exit(
    # ----- Close the session and the plumber API --------
    expect_null(
      callthat::call_that_session_stop(api_session)
    )
  )

  ## Tests for /timeseries endpoint
  expect_s3_class(
    # ---------------- Make API call --------------------
    get_ts <- callthat::call_that_api_get(
      api_session,
      endpoint = "timeseries"
    ),
    "response"
  )

  # ---------- Run tests against response ---------------
  ## Test to confirm that the response was a success
  expect_equal(
    get_ts$status_code,
    200
  )

  out <- readr::read_csv(
    httr::content(
      get_ts,
      type = "text",
      encoding = "UTF-8"
    ),
    show_col_types = FALSE
  )

  ## Test to confirm the output of the API is correct
  expect_named(
    out,
    c(
      "timeseries_id",
      "location_id",
      "location_name",
      "depth_height_m",
      "media_type",
      "parameter_name",
      "units",
      "aggregation_type",
      "recording_rate",
      "start_datetime",
      "end_datetime",
      "note"
    )
  )
  # Expect more than 0 rows returned
  expect_gt(
    nrow(out),
    0
  )
  # pick a timeseries_id to test the /timeseries/{timeseries_id} endpoint later on
  test_timeseries_id <- out$timeseries_id[1]
  test_timeseries_end <- out$end_datetime[1]

  # Tests for /locations endpoint
  expect_s3_class(
    # ---------------- Make API call --------------------
    get_locs <- callthat::call_that_api_get(
      api_session,
      endpoint = "locations",
      query = list(weight = 1)
    ),
    "response"
  )
  # Test to confirm that the response was a success
  expect_equal(
    get_locs$status_code,
    200
  )
  out <- readr::read_csv(
    httr::content(
      get_locs,
      type = "text",
      encoding = "UTF-8"
    ),
    show_col_types = FALSE
  )
  # ---------- Run tests against response ---------------
  expect_gt(
    nrow(out),
    0
  )
  expect_named(
    out,
    c(
      "location_id",
      "location_code",
      "name",
      "latitude",
      "longitude",
      "elevation",
      "datum",
      "note",
      "projects",
      "networks"
    )
  )

  ## Tests for /timeseries/{timeseries_id} endpoint
  expect_s3_class(
    # ---------------- Make API call --------------------
    get_ts_id <- callthat::call_that_api_get(
      api_session,
      endpoint = sprintf("timeseries/%s/measurements", test_timeseries_id),
      query = list(
        start = test_timeseries_end - 365 * 24 * 60 * 60, # one year before end date
        end = test_timeseries_end,
        limit = 100
      )
    ),
    "response"
  )
  # Test to confirm that the response was a success
  expect_equal(
    get_ts_id$status_code,
    200
  )
  out <- readr::read_csv(
    httr::content(
      get_ts_id,
      type = "text",
      encoding = "UTF-8"
    ),
    show_col_types = FALSE
  )
  # ---------- Run tests against response ---------------
  expect_gt(
    nrow(out),
    0
  )
  expect_named(
    out,
    c(
      "timeseries_id",
      "datetime",
      "value_raw",
      "value_corrected",
      "period",
      "imputed"
    )
  )
})
