# Notably, most tests here exercise the plumber2 router via api(run = FALSE).
# Tests for api.R are in tests/testthat/test-api.R

# Helpers to test JSON responses
content_text <- function(response) {
  if (!is.null(response$body)) {
    return(response$body)
  }

  httr::content(
    response,
    type = "text",
    encoding = "UTF-8"
  )
}

expect_content_type <- function(response, pattern) {
  headers <- if (!is.null(response$headers)) {
    response$headers
  } else {
    httr::headers(response)
  }
  content_type <- headers[[which(tolower(names(headers)) == "content-type")[1L]]]

  expect_true(
    grepl(pattern, content_type, ignore.case = TRUE),
    info = paste("Unexpected Content-Type:", content_type)
  )
}

response_status <- function(response) {
  if (!is.null(response$status_code)) {
    return(response$status_code)
  }

  response$status
}

parse_json_df <- function(response) {
  text <- content_text(response)

  expect_true(
    jsonlite::validate(text),
    info = text
  )

  out <- jsonlite::fromJSON(
    text,
    simplifyDataFrame = TRUE,
    simplifyVector = TRUE
  )

  as.data.frame(out, stringsAsFactors = FALSE)
}

expect_json_df_response <- function(response, expected_names = NULL) {
  expect_equal(response_status(response), 200)
  expect_content_type(response, "application/json")

  out <- parse_json_df(response)

  expect_gt(nrow(out), 0)

  if (!is.null(expected_names)) {
    expect_named(out, expected_names)
  }

  out
}

v2_url_with_format <- function(url, format = "json") {
  separator <- if (grepl("\\?", url)) "&" else "?"
  paste0(url, separator, "format=", format)
}

v2_route_file <- function() {
  path <- YGwater:::api_find_target(2)$path
  if (dir.exists(path)) {
    path <- file.path(path, "plumber.R")
  }
  path
}

v2_test_async <- function(expr, envir) {
  promises::promise_resolve(eval(expr, envir = envir))
}

v2_resolve_request <- function(value, max_ticks = 500L) {
  if (!promises::is.promise(value)) {
    return(value)
  }

  resolved <- FALSE
  out <- NULL
  err <- NULL
  value$then(
    function(result) {
      out <<- result
      resolved <<- TRUE
    },
    function(error) {
      err <<- error
      resolved <<- TRUE
    }
  )

  for (i in seq_len(max_ticks)) {
    if (resolved) {
      break
    }
    later::run_now(0.01)
  }

  if (!is.null(err)) {
    stop(err)
  }
  if (!resolved) {
    stop("Timed out waiting for async test request.", call. = FALSE)
  }

  out
}

test_that("api(version = 2) builds a plumber2 router without running", {
  skip_if_not_installed("plumber2")

  withr::local_options(list(plumber2.async = v2_test_async))
  withr::local_envvar(list(
    aquacacheName = NA,
    aquacacheHost = NA,
    aquacachePort = NA
  ))

  pr <- api(
    version = 2,
    run = FALSE,
    dbName = "aquacache_test"
  )

  expect_s3_class(pr, "Plumber2")
  expect_null(pr$.__enclos_env__$private$OPENAPI$servers)

  pr_with_server <- api(
    version = 2,
    run = FALSE,
    server = "/water-data/api/v2",
    dbName = "aquacache_test"
  )

  expect_equal(
    pr_with_server$.__enclos_env__$private$OPENAPI$servers,
    list(list(url = "/water-data/api/v2"))
  )
  expect_equal(
    head(names(pr$.__enclos_env__$private$OPENAPI$paths), 2L),
    c("/locations", "/timeseries")
  )

  openapi <- pr_with_server$.__enclos_env__$private$OPENAPI
  openapi$servers <- c(openapi$servers, list(list(url = "")))
  openapi_json <- jsonlite::toJSON(
    fireproof::prune_openapi(openapi),
    auto_unbox = TRUE
  )
  expect_true(jsonlite::validate(openapi_json))

  expect_equal(Sys.getenv("APIaquacacheName"), "aquacache_test")
  expect_equal(Sys.getenv("APIaquacacheHost"), Sys.getenv("aquacacheHost"))
  expect_equal(Sys.getenv("APIaquacachePort"), Sys.getenv("aquacachePort"))
})

test_that("API V2 async annotations are limited to long-running endpoints", {
  lines <- readLines(v2_route_file(), warn = FALSE)
  route_starts <- grep("^#\\* @get\\s+", lines)
  route_ends <- c(route_starts[-1L] - 1L, length(lines))
  routes <- sub("^#\\* @get\\s+", "", lines[route_starts])
  async_routes <- c(
    "/timeseries/measurements",
    "/samples",
    "/samples/results",
    "/snow-bulletin/leaflet",
    "/snow-survey/data",
    "/snow-survey/metadata",
    "/snow-survey/stats",
    "/snow-survey/trends"
  )

  expect_false(any(grepl("^/v[0-9]+(?:/|$)", routes)))
  expect_false(any(grepl("^#\\* @any\\s+/\\*\\s*$", lines)))

  is_async <- vapply(
    seq_along(route_starts),
    function(i) {
      any(grepl("^#\\* @async\\s*$", lines[route_starts[[i]]:route_ends[[i]]]))
    },
    logical(1L)
  )

  expect_true(
    all(async_routes %in% routes),
    info = paste(setdiff(async_routes, routes), collapse = ", ")
  )
  expect_equal(
    sort(routes[is_async]),
    sort(async_routes)
  )

  missing_then <- async_routes[
    !vapply(
      async_routes,
      function(route) {
        i <- match(route, routes)
        block <- lines[route_starts[[i]]:route_ends[[i]]]
        async_at <- grep("^#\\* @async\\s*$", block)
        then_at <- grep("^#\\* @then\\s*$", block)
        length(async_at) == 1L &&
          length(then_at) == 1L &&
          then_at[[1L]] > async_at[[1L]]
      },
      logical(1L)
    )
  ]

  unexpected_then <- routes[
    !is_async &
      vapply(
        seq_along(route_starts),
        function(i) {
          block <- lines[route_starts[[i]]:route_ends[[i]]]
          any(grepl("^#\\* @then\\s*$", block))
        },
        logical(1L)
      )
  ]

  expect_true(
    length(missing_then) == 0L,
    info = paste(missing_then, collapse = ", ")
  )
  expect_true(
    length(unexpected_then) == 0L,
    info = paste(unexpected_then, collapse = ", ")
  )
})

test_that("API V2 file cache reuses values and waits on in-flight work", {
  env <- new.env(parent = globalenv())
  sys.source(v2_route_file(), envir = env)

  cache_dir <- tempfile("v2-cache")
  withr::local_envvar(c(
    YGWATER_API_V2_CACHE_DIR = cache_dir,
    YGWATER_API_V2_CACHE_WAIT = "0.1",
    YGWATER_API_V2_CACHE_STALE = "10"
  ))
  withr::defer(unlink(cache_dir, recursive = TRUE, force = TRUE))

  calls <- 0L
  first <- env$v2_cache_get_or_compute(
    "single-flight",
    function() {
      calls <<- calls + 1L
      list(value = "first")
    }
  )
  second <- env$v2_cache_get_or_compute(
    "single-flight",
    function() {
      calls <<- calls + 1L
      list(value = "second")
    }
  )

  expect_equal(first$value, "first")
  expect_equal(second$value, "first")
  expect_equal(calls, 1L)

  locked <- env$v2_cache_paths("locked")
  dir.create(locked$lock_dir, recursive = TRUE)

  expect_error(
    env$v2_cache_get_or_compute("locked", function() "should not run"),
    "Timed out waiting for an in-flight cached API result"
  )
})

test_that("API V2 metadata and lookup endpoints return expected CSV and JSON", {
  skip_if_not_installed("plumber2")
  skip_if_not_installed("reqres")
  skip_if_not_installed("promises")
  skip_if_not_installed("later")

  withr::local_options(list(plumber2.async = v2_test_async))
  pr <- api(
    version = 2,
    run = FALSE,
    dbName = Sys.getenv("aquacacheName", "testdb"),
    dbHost = Sys.getenv("aquacacheHost", "localhost"),
    dbPort = Sys.getenv("aquacachePort", "5432"),
    dbUser = Sys.getenv("aquacacheUser", "runner"),
    dbPass = Sys.getenv("aquacachePass", "runner")
  )

  get_v2 <- function(url, headers = list()) {
    v2_resolve_request(pr$test_request(reqres:::mock_rook(
      url = url,
      method = "get",
      headers = headers
    )))
  }

  get_ts <- get_v2("http://example.com/timeseries")
  get_ts_json <- get_v2("http://example.com/timeseries?format=json")

  expect_equal(get_ts$status, 200)
  expect_equal(get_ts_json$status, 200)

  out <- read.csv(text = get_ts$body)
  out_json <- parse_json_df(get_ts_json)
  out$end_datetime <- as.POSIXct(out$end_datetime, tz = "UTC")
  out$start_datetime <- as.POSIXct(out$start_datetime, tz = "UTC")
  out_json$end_datetime <- as.POSIXct(out_json$end_datetime, tz = "UTC")
  out_json$start_datetime <- as.POSIXct(out_json$start_datetime, tz = "UTC")

  timeseries_names <- c(
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
  expect_named(
    out,
    timeseries_names
  )
  expect_named(
    out_json,
    timeseries_names
  )
  expect_gt(nrow(out), 0)
  expect_gt(nrow(out_json), 0)

  get_locs <- get_v2("http://example.com/locations")
  get_locs_json <- get_v2("http://example.com/locations?format=json")

  expect_equal(get_locs$status, 200)
  expect_equal(get_locs_json$status, 200)

  locs <- read.csv(text = get_locs$body)
  locs_json <- parse_json_df(get_locs_json)

  location_names <- c(
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
  expect_named(
    locs,
    location_names
  )
  expect_named(
    locs_json,
    location_names
  )
  expect_gt(nrow(locs), 0)
  expect_gt(nrow(locs_json), 0)

  get_parameters <- get_v2("http://example.com/parameters")
  get_parameters_json <- get_v2("http://example.com/parameters?format=json")

  expect_equal(get_parameters$status, 200)
  expect_equal(get_parameters_json$status, 200)

  parameters <- read.csv(text = get_parameters$body)
  parameters_json <- parse_json_df(get_parameters_json)

  parameter_names <- c(
    "parameter_id",
    "param_name",
    "param_name_fr",
    "description",
    "description_fr",
    "units"
  )
  expect_named(
    parameters,
    parameter_names
  )
  expect_named(
    parameters_json,
    parameter_names
  )
  expect_gt(nrow(parameters), 0)
  expect_gt(nrow(parameters_json), 0)

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
    res <- get_v2(sprintf("http://example.com/%s", endpoint))
    res_json <- get_v2(sprintf("http://example.com/%s?format=json", endpoint))

    expect_equal(res$status, 200)
    expect_equal(res_json$status, 200)

    lookup <- read.csv(text = res$body)
    lookup_json <- parse_json_df(res_json)

    expect_named(lookup, lookup_endpoints[[endpoint]])
    expect_named(lookup_json, lookup_endpoints[[endpoint]])
    expect_gt(nrow(lookup), 0)
    expect_gt(nrow(lookup_json), 0)
  }

  invalid_lang <- get_v2("http://example.com/locations?lang=es")

  expect_equal(invalid_lang$status, 400)

  invalid_lang_body <- read.csv(text = invalid_lang$body)

  expect_equal(invalid_lang_body$status[1], "error")
  expect_match(invalid_lang_body$message[1], "Invalid language parameter")

  invalid_auth <- get_v2(
    "http://example.com/locations",
    headers = list(Authorization = "Bearer invalid")
  )

  expect_equal(invalid_auth$status, 401)

  missing_sample_start <- get_v2("http://example.com/samples")

  expect_equal(missing_sample_start$status, 400)

  missing_sample_ids <- get_v2("http://example.com/samples/results")

  expect_equal(missing_sample_ids$status, 400)
})

test_that("API V2 sample endpoints use discrete metadata views and modifiedSince", {
  skip_if_not_installed("plumber2")
  skip_if_not_installed("reqres")
  skip_if_not_installed("promises")
  skip_if_not_installed("later")

  withr::local_options(list(plumber2.async = v2_test_async))
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
    v2_resolve_request(
      pr$test_request(reqres:::mock_rook(url = url, method = "get"))
    )
  }

  encode_time <- function(x) {
    utils::URLencode(
      format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      reserved = TRUE
    )
  }

  row_stamp <- function(x) {
    parse_stamp <- function(value) {
      value <- as.character(value)
      value[!nzchar(value)] <- NA_character_
      as.POSIXct(value, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
    }

    created <- parse_stamp(x$created)
    modified <- parse_stamp(x$modified)
    out <- created
    use_modified <- !is.na(modified) & (is.na(out) | modified > out)
    out[use_modified] <- modified[use_modified]
    out
  }

  floor_to_second <- function(x) {
    as.POSIXct(
      format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    )
  }

  sample_url <- function(modified_since = NULL) {
    suffix <- if (is.null(modified_since)) {
      ""
    } else {
      paste0("&modifiedSince=", modified_since)
    }
    paste0(
      "http://example.com/samples",
      "?start=1900-01-01&end=3000-01-01",
      suffix
    )
  }

  get_samples <- get_v2(sample_url())
  get_samples_json <- get_v2(v2_url_with_format(sample_url()))

  expect_equal(get_samples$status, 200)
  expect_equal(get_samples_json$status, 200)

  samples <- read.csv(text = get_samples$body)
  samples_json <- if (get_samples_json$status == 200L) {
    parse_json_df(get_samples_json)
  } else {
    data.frame()
  }
  if (
    identical(names(samples), c("status", "message")) &&
      identical(samples$status[1], "info")
  ) {
    skip("No samples available for API V2 sample endpoint tests")
  }

  expect_true(all(
    c(
      "sample_id",
      "location_code",
      "media_type",
      "sample_type",
      "created",
      "modified"
    ) %in%
      names(samples)
  ))
  if (get_samples_json$status == 200L) {
    expect_true(all(
      c(
        "sample_id",
        "location_code",
        "media_type",
        "sample_type",
        "created",
        "modified"
      ) %in%
        names(samples_json)
    ))
    expect_gt(nrow(samples_json), 0)
  }

  get_samples_since <- get_v2(sample_url(
    encode_time(as.POSIXct("1900-01-01", tz = "UTC"))
  ))
  get_samples_since_json <- get_v2(v2_url_with_format(sample_url(
    encode_time(as.POSIXct("1900-01-01", tz = "UTC"))
  )))

  expect_equal(get_samples_since$status, 200)
  expect_equal(get_samples_since_json$status, 200)
  expect_named(read.csv(text = get_samples_since$body), names(samples))
  if (get_samples_since_json$status == 200L) {
    expect_named(parse_json_df(get_samples_since_json), names(samples))
  }

  sample_stamps <- row_stamp(samples)
  if (any(!is.na(sample_stamps))) {
    newest_stamp <- floor_to_second(max(sample_stamps, na.rm = TRUE))
    get_samples_recent <- get_v2(sample_url(encode_time(newest_stamp)))

    expect_equal(get_samples_recent$status, 200)

    recent_samples <- read.csv(text = get_samples_recent$body)
    if (
      !identical(names(recent_samples), c("status", "message")) ||
        !identical(recent_samples$status[1], "info")
    ) {
      expect_true(all(row_stamp(recent_samples) >= newest_stamp, na.rm = TRUE))
      expect_lte(nrow(recent_samples), nrow(samples))
    }
  }

  get_samples_future <- get_v2(sample_url(
    encode_time(Sys.time() + 365 * 24 * 60 * 60)
  ))
  get_samples_future_json <- get_v2(v2_url_with_format(sample_url(
    encode_time(Sys.time() + 365 * 24 * 60 * 60)
  )))

  expect_equal(get_samples_future$status, 200)
  expect_equal(get_samples_future_json$status, 200)

  samples_future <- read.csv(text = get_samples_future$body)
  samples_future_json <- if (get_samples_future_json$status == 200L) {
    parse_json_df(get_samples_future_json)
  } else {
    data.frame()
  }

  expect_equal(samples_future$status[1], "info")
  expect_match(samples_future$message[1], "No samples found")
  if (get_samples_future_json$status == 200L) {
    expect_equal(samples_future_json$status[1], "info")
    expect_match(samples_future_json$message[1], "No samples found")
  }

  invalid_samples_since <- get_v2(paste0(
    "http://example.com/samples",
    "?start=1900-01-01&end=3000-01-01&modifiedSince=not-a-date"
  ))

  expect_equal(invalid_samples_since$status, 400)

  sample_ids <- utils::URLencode(
    paste(utils::head(samples$sample_id, 50L), collapse = ","),
    reserved = TRUE
  )
  results_url <- function(modified_since = NULL) {
    suffix <- if (is.null(modified_since)) {
      ""
    } else {
      paste0("&modifiedSince=", modified_since)
    }
    sprintf(
      "http://example.com/samples/results?sample_ids=%s%s",
      sample_ids,
      suffix
    )
  }

  get_results <- get_v2(results_url())
  get_results_json <- get_v2(v2_url_with_format(results_url()))

  expect_equal(get_results$status, 200)
  expect_equal(get_results_json$status, 200)

  results <- read.csv(text = get_results$body)
  results_json <- if (get_results_json$status == 200L) {
    parse_json_df(get_results_json)
  } else {
    data.frame()
  }
  if (
    identical(names(results), c("status", "message")) &&
      identical(results$status[1], "info")
  ) {
    skip("No sample results available for API V2 result endpoint tests")
  }

  expect_true(all(
    c(
      "result_id",
      "sample_id",
      "location_code",
      "parameter_id",
      "parameter_name",
      "result",
      "created",
      "modified"
    ) %in%
      names(results)
  ))
  if (get_results_json$status == 200L) {
    expect_true(all(
      c(
        "result_id",
        "sample_id",
        "location_code",
        "parameter_id",
        "parameter_name",
        "result",
        "created",
        "modified"
      ) %in%
        names(results_json)
    ))
    expect_gt(nrow(results_json), 0)
  }

  get_results_since <- get_v2(results_url(
    encode_time(as.POSIXct("1900-01-01", tz = "UTC"))
  ))
  get_results_since_json <- get_v2(v2_url_with_format(results_url(
    encode_time(as.POSIXct("1900-01-01", tz = "UTC"))
  )))

  expect_equal(get_results_since$status, 200)
  expect_equal(get_results_since_json$status, 200)
  expect_named(read.csv(text = get_results_since$body), names(results))
  if (get_results_since_json$status == 200L) {
    expect_named(parse_json_df(get_results_since_json), names(results))
  }

  result_stamps <- row_stamp(results)
  if (any(!is.na(result_stamps))) {
    newest_stamp <- floor_to_second(max(result_stamps, na.rm = TRUE))
    get_results_recent <- get_v2(results_url(encode_time(newest_stamp)))

    expect_equal(get_results_recent$status, 200)

    recent_results <- read.csv(text = get_results_recent$body)
    if (
      !identical(names(recent_results), c("status", "message")) ||
        !identical(recent_results$status[1], "info")
    ) {
      expect_true(all(row_stamp(recent_results) >= newest_stamp, na.rm = TRUE))
      expect_lte(nrow(recent_results), nrow(results))
    }
  }

  get_results_future <- get_v2(results_url(
    encode_time(Sys.time() + 365 * 24 * 60 * 60)
  ))
  get_results_future_json <- get_v2(v2_url_with_format(results_url(
    encode_time(Sys.time() + 365 * 24 * 60 * 60)
  )))

  expect_equal(get_results_future$status, 200)
  expect_equal(get_results_future_json$status, 200)

  results_future <- read.csv(text = get_results_future$body)
  results_future_json <- if (get_results_future_json$status == 200L) {
    parse_json_df(get_results_future_json)
  } else {
    data.frame()
  }

  expect_equal(results_future$status[1], "info")
  expect_match(results_future$message[1], "No results found")
  if (get_results_future_json$status == 200L) {
    expect_equal(results_future_json$status[1], "info")
    expect_match(results_future_json$message[1], "No results found")
  }

  invalid_results_since <- get_v2(sprintf(
    "http://example.com/samples/results?sample_ids=%s&modifiedSince=not-a-date",
    sample_ids
  ))

  expect_equal(invalid_results_since$status, 400)
})

test_that("API V2 measurements endpoint returns corrected measurements", {
  skip_if_not_installed("plumber2")
  skip_if_not_installed("reqres")
  skip_if_not_installed("promises")
  skip_if_not_installed("later")

  withr::local_options(list(plumber2.async = v2_test_async))
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
    v2_resolve_request(
      pr$test_request(reqres:::mock_rook(url = url, method = "get"))
    )
  }

  get_ts <- get_v2("http://example.com/timeseries")
  timeseries <- read.csv(text = get_ts$body)
  timeseries$end_datetime <- as.POSIXct(timeseries$end_datetime, tz = "UTC")
  timeseries <- timeseries[!is.na(timeseries$end_datetime), ]

  skip_if(nrow(timeseries) == 0, "No timeseries with end_datetime available")

  test_timeseries_id <- timeseries$timeseries_id[1]
  test_timeseries_end <- timeseries$end_datetime[1]
  test_timeseries_start <- test_timeseries_end - 365 * 24 * 60 * 60

  encode_time <- function(x) {
    utils::URLencode(
      format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      reserved = TRUE
    )
  }

  measurement_stamp <- function(x) {
    created <- as.POSIXct(x$created, tz = "UTC")
    modified <- as.POSIXct(x$modified, tz = "UTC")
    out <- created
    use_modified <- !is.na(modified) & (is.na(out) | modified > out)
    out[use_modified] <- modified[use_modified]
    out
  }

  measurement_url <- function(modified_since = NULL) {
    suffix <- if (is.null(modified_since)) {
      ""
    } else {
      paste0("&modifiedSince=", modified_since)
    }

    sprintf(
      paste0(
        "http://example.com/timeseries/measurements",
        "?id=%s&start=%s&end=%s&limit=100%s"
      ),
      test_timeseries_id,
      encode_time(test_timeseries_start),
      encode_time(test_timeseries_end),
      suffix
    )
  }

  get_measurements <- get_v2(measurement_url())
  get_measurements_json <- get_v2(v2_url_with_format(measurement_url()))

  expect_equal(get_measurements$status, 200)
  expect_equal(get_measurements_json$status, 200)

  measurements <- read.csv(text = get_measurements$body)
  measurements_json <- if (get_measurements_json$status == 200L) {
    parse_json_df(get_measurements_json)
  } else {
    data.frame()
  }
  if (
    identical(names(measurements), c("status", "message")) &&
      identical(measurements$status[1], "info")
  ) {
    skip("No measurements found for selected test timeseries")
  }

  get_measurements_since <- get_v2(measurement_url(
    encode_time(as.POSIXct("1900-01-01", tz = "UTC"))
  ))
  get_measurements_since_json <- get_v2(v2_url_with_format(measurement_url(
    encode_time(as.POSIXct("1900-01-01", tz = "UTC"))
  )))

  expect_equal(get_measurements_since$status, 200)
  expect_equal(get_measurements_since_json$status, 200)
  expect_named(
    read.csv(text = get_measurements_since$body),
    names(measurements)
  )
  if (get_measurements_since_json$status == 200L) {
    expect_named(
      parse_json_df(get_measurements_since_json),
      names(measurements)
    )
  }

  measurement_stamps <- measurement_stamp(measurements)
  if (any(!is.na(measurement_stamps))) {
    newest_stamp <- max(measurement_stamps, na.rm = TRUE)
    get_measurements_recent <- get_v2(measurement_url(encode_time(
      newest_stamp
    )))

    expect_equal(get_measurements_recent$status, 200)

    recent <- read.csv(text = get_measurements_recent$body)
    if (
      !identical(names(recent), c("status", "message")) ||
        !identical(recent$status[1], "info")
    ) {
      recent_stamps <- measurement_stamp(recent)
      expect_true(all(recent_stamps >= newest_stamp, na.rm = TRUE))
      expect_lte(nrow(recent), nrow(measurements))
    }
  }

  future_modified_since <- get_v2(measurement_url(
    encode_time(Sys.time() + 365 * 24 * 60 * 60)
  ))

  expect_equal(future_modified_since$status, 200)

  future_body <- read.csv(text = future_modified_since$body)

  expect_equal(future_body$status[1], "info")
  expect_match(future_body$message[1], "No measurements found")

  invalid_modified_since <- get_v2(sprintf(
    paste0(
      "http://example.com/timeseries/measurements",
      "?id=%s&start=%s&end=%s&modifiedSince=not-a-date"
    ),
    test_timeseries_id,
    encode_time(test_timeseries_start),
    encode_time(test_timeseries_end)
  ))

  expect_equal(invalid_modified_since$status, 400)

  measurement_names <- c(
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
  expect_named(
    measurements,
    measurement_names
  )
  if (get_measurements_json$status == 200L) {
    expect_named(
      measurements_json,
      measurement_names
    )
  }
  expect_gt(nrow(measurements), 0)
  if (get_measurements_json$status == 200L) {
    expect_gt(nrow(measurements_json), 0)
  }

  missing_id <- get_v2(
    "http://example.com/timeseries/measurements?start=2020-01-01"
  )

  expect_equal(missing_id$status, 400)
})

test_that("API V2 snow bulletin map endpoint returns HTML", {
  skip_if_not_installed("plumber2")
  skip_if_not_installed("reqres")
  skip_if_not_installed("promises")
  skip_if_not_installed("later")

  withr::local_options(list(plumber2.async = v2_test_async))
  ns <- asNamespace("YGwater")
  original <- get("create_snowbull_leaflet_html", envir = ns)

  unlockBinding("create_snowbull_leaflet_html", ns)
  assign(
    "create_snowbull_leaflet_html",
    function(year = NULL, month = NULL, ...) {
      list(html = "<html>stub map</html>", year = year, month = month)
    },
    envir = ns
  )
  lockBinding("create_snowbull_leaflet_html", ns)

  withr::defer({
    unlockBinding("create_snowbull_leaflet_html", ns)
    assign("create_snowbull_leaflet_html", original, envir = ns)
    lockBinding("create_snowbull_leaflet_html", ns)
  })
  withr::local_envvar(c(YGWATER_API_V2_CACHE_DIR = tempfile("v2-cache")))

  pr <- api(
    version = 2,
    run = FALSE,
    dbName = Sys.getenv("aquacacheName", "testdb"),
    dbHost = Sys.getenv("aquacacheHost", "localhost"),
    dbPort = Sys.getenv("aquacachePort", "5432"),
    dbUser = Sys.getenv("aquacacheUser", "runner"),
    dbPass = Sys.getenv("aquacachePass", "runner")
  )

  res <- v2_resolve_request(pr$test_request(reqres:::mock_rook(
    url = "http://example.com/snow-bulletin/leaflet?year=2024&month=5",
    method = "get"
  )))

  expect_equal(res$status, 200)
  expect_match(res$body, "stub map", fixed = TRUE)
})

test_that("leaflet map HTML renderer inlines widget dependencies", {
  skip_if_not_installed("base64enc")
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("leaflet")

  widget <- leaflet::addTiles(leaflet::leaflet())
  html <- YGwater:::render_leaflet_widget_html(widget)

  expect_match(html, "<!DOCTYPE html>", fixed = TRUE)
  expect_false(grepl("lib/", html, fixed = TRUE))
  expect_true(grepl("data:application/javascript", html, fixed = TRUE))
  expect_true(grepl("data:text/css", html, fixed = TRUE))
})
