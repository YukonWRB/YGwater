# plumber tests use package callthat to manage the background process
# devtools::install_github("edgararuiz/callthat")

# Notably, this does not test the R function at api.R, but does test the plumber file at plumber/v1.R
# Tests for api.R are in tests/testthat/test-api.R

test_that("tests for API V1", {
  # Set some environment variables for the API to use. These are normally set when the API is launched using api() but are set here in the local environment.
  Sys.setenv(APIaquacacheUser = Sys.getenv("aquacacheUser", "runner"))
  Sys.setenv(APIaquacachePass = Sys.getenv("aquacachePass", "runner"))
  Sys.setenv(APIaquacacheName = Sys.getenv("aquacacheName", "testdb"))
  Sys.setenv(APIaquacacheHost = Sys.getenv("aquacacheHost", "localhost"))
  Sys.setenv(APIaquacachePort = Sys.getenv("aquacachePort", "5432"))

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
      endpoint = "timeseries",
      query = list(format = "csv")
    ),
    "response"
  )

  # ---------- Run tests against response ---------------
  ## Test to confirm that the response was a success
  expect_equal(
    get_ts$status_code,
    200
  )

  # get the content of the response as a data frame with base R if possible
  out <- read.csv(
    text = httr::content(
      get_ts,
      type = "text",
      encoding = "UTF-8"
    )
  )

  out$end_datetime <- as.POSIXct(out$end_datetime, tz = "UTC")
  out$start_datetime <- as.POSIXct(out$start_datetime, tz = "UTC")

  ## Test to confirm the output of the API is correct
  expect_named(
    out,
    c(
      "timeseries_id",
      "location_id",
      "location_name",
      "location_type",
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
      "note",
      "timeseries_type_code",
      "timeseries_type",
      "timeseries_type_description",
      "last_new_data",
      "publicly_visible",
      "active",
      "default_owner_organization_id",
      "default_owner",
      "default_owner_fr",
      "timezone_daily_calc",
      "last_daily_calculation",
      "last_synchronize",
      "matrix_state_id",
      "matrix_state_name",
      "matrix_state_name_fr",
      "sub_location_id",
      "sub_location_name",
      "sub_location_name_fr",
      "compound_expression_sql",
      "compound_member_aliases",
      "compound_member_timeseries_ids",
      "compound_member_priorities",
      "compound_member_use_from",
      "compound_member_use_to"
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
  # In case the end datetime is a numeric timestamp, convert to POSIXct (this happens on CI sometimes)
  if (suppressWarnings(!is.na(as.numeric(test_timeseries_end)))) {
    test_timeseries_end <- as.POSIXct(
      as.numeric(test_timeseries_end),
      origin = "1970-01-01",
      tz = "UTC"
    )
  }

  # Pick another timeseries same or greater end datetime to test the /timeseries/{timeseries_id} endpoint later on
  test_timeseries_id_2 <- out$timeseries_id[
    out$end_datetime >= test_timeseries_end
  ][1]

  # Tests for /locations endpoint
  expect_s3_class(
    # ---------------- Make API call --------------------
    get_locs <- callthat::call_that_api_get(
      api_session,
      endpoint = "locations",
      query = list(weight = 1, format = "csv")
    ),
    "response"
  )
  # Test to confirm that the response was a success
  expect_equal(
    get_locs$status_code,
    200
  )
  out <- read.csv(
    text = httr::content(
      get_locs,
      type = "text",
      encoding = "UTF-8"
    )
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
      "name",
      "alias",
      "location_code",
      "location_type",
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

  ## Tests for grade, approval, qualifier, and organization lookup endpoints
  lookup_endpoints <- list(
    grades = c(
      "grade_type_id",
      "grade_type_code",
      "grade_type_description",
      "grade_type_description_fr",
      "color_code"
    ),
    approvals = c(
      "approval_type_id",
      "approval_type_code",
      "approval_type_description",
      "approval_type_description_fr",
      "color_code"
    ),
    qualifiers = c(
      "qualifier_type_id",
      "qualifier_type_code",
      "qualifier_type_description",
      "qualifier_type_description_fr",
      "color_code"
    ),
    organizations = c(
      "organization_id",
      "name",
      "name_fr",
      "contact_name",
      "phone",
      "email",
      "note"
    )
  )

  for (endpoint in names(lookup_endpoints)) {
    expect_s3_class(
      get_lookup <- callthat::call_that_api_get(
        api_session,
        endpoint = endpoint,
        query = list(format = "csv")
      ),
      "response"
    )
    expect_equal(
      get_lookup$status_code,
      200
    )

    out <- read.csv(
      text = httr::content(
        get_lookup,
        type = "text",
        encoding = "UTF-8"
      )
    )

    expect_gt(
      nrow(out),
      0
    )
    expect_named(
      out,
      lookup_endpoints[[endpoint]]
    )
  }

  ## Tests for /timeseries/{timeseries_id} endpoint
  skip_on_ci()
  # Single timeseries
  expect_s3_class(
    # ---------------- Make API call --------------------
    get_ts_id <- callthat::call_that_api_get(
      api_session,
      endpoint = "timeseries/measurements",
      query = list(
        start = test_timeseries_end - 365 * 24 * 60 * 60, # one year before end date
        end = test_timeseries_end,
        limit = 100,
        id = test_timeseries_id,
        format = "csv"
      )
    ),
    "response"
  )
  # Two timeseries
  expect_s3_class(
    # ---------------- Make API call --------------------
    get_ts_id <- callthat::call_that_api_get(
      api_session,
      endpoint = "timeseries/measurements",
      query = list(
        start = test_timeseries_end - 365 * 24 * 60 * 60, # one year before end date
        end = test_timeseries_end,
        limit = 100,
        id = paste(test_timeseries_id, test_timeseries_id_2, sep = ","),
        format = "csv"
      )
    ),
    "response"
  )
  # Test to confirm that the response was a success
  expect_equal(
    get_ts_id$status_code,
    200
  )
  out <- read.csv(
    text = httr::content(
      get_ts_id,
      type = "text",
      encoding = "UTF-8"
    )
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
      "imputed",
      "created",
      "modified",
      "grade_type_id",
      "grade_type_description",
      "approval_type_id",
      "approval_type_description",
      "qualifier_type_ids",
      "qualifier_type_descriptions",
      "owner_organization_id",
      "owner",
      "contributor_organization_id",
      "contributor"
    )
  )
})
