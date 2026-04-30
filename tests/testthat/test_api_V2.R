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
    server = "/water-data/api/v2",
    dbName = "aquacache_test"
  )

  expect_s3_class(pr, "Plumber2")
  expect_equal(
    pr$.__enclos_env__$private$OPENAPI$servers,
    list(list(url = "/water-data/api/v2"))
  )

  expect_equal(Sys.getenv("APIaquacacheName"), "aquacache_test")
  expect_equal(Sys.getenv("APIaquacacheHost"), Sys.getenv("aquacacheHost"))
  expect_equal(Sys.getenv("APIaquacachePort"), Sys.getenv("aquacachePort"))
})

test_that("API V2 endpoints are marked async", {
  lines <- readLines(v2_route_file(), warn = FALSE)
  route_starts <- grep("^#\\* @get\\s+", lines)
  route_ends <- c(route_starts[-1L] - 1L, length(lines))
  routes <- sub("^#\\* @get\\s+", "", lines[route_starts])

  expect_false(any(grepl("^/v[0-9]+(?:/|$)", routes)))

  missing_async <- routes[!vapply(
    seq_along(route_starts),
    function(i) {
      any(grepl("^#\\* @async\\s*$", lines[route_starts[[i]]:route_ends[[i]]]))
    },
    logical(1L)
  )]

  expect_true(
    length(missing_async) == 0L,
    info = paste(missing_async, collapse = ", ")
  )

  missing_then <- routes[!vapply(
    seq_along(route_starts),
    function(i) {
      block <- lines[route_starts[[i]]:route_ends[[i]]]
      async_at <- grep("^#\\* @async\\s*$", block)
      then_at <- grep("^#\\* @then\\s*$", block)
      length(async_at) == 1L &&
        length(then_at) == 1L &&
        then_at[[1L]] > async_at[[1L]]
    },
    logical(1L)
  )]

  expect_true(
    length(missing_then) == 0L,
    info = paste(missing_then, collapse = ", ")
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

test_that("API V2 metadata and lookup endpoints return expected CSV", {
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
      "note",
      "timeseries_type_code",
      "timeseries_type",
      "timeseries_type_description",
      "last_new_data",
      "publicly_visible",
      "active",
      "source_fx",
      "source_fx_args",
      "share_with",
      "default_owner_organization_id",
      "default_owner",
      "default_owner_fr",
      "default_data_sharing_agreement_id",
      "private_expiry",
      "sync_remote",
      "timezone_daily_calc",
      "last_daily_calculation",
      "last_synchronize",
      "matrix_state_id",
      "matrix_state_code",
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
  expect_gt(nrow(out), 0)

  get_locs <- get_v2("http://example.com/locations")

  expect_equal(get_locs$status, 200)

  locs <- read.csv(text = get_locs$body)

  expect_named(
    locs,
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
  expect_gt(nrow(locs), 0)

  get_parameters <- get_v2("http://example.com/parameters")

  expect_equal(get_parameters$status, 200)

  parameters <- read.csv(text = get_parameters$body)

  expect_named(
    parameters,
    c(
      "parameter_id",
      "param_name",
      "param_name_fr",
      "description",
      "description_fr",
      "units"
    )
  )
  expect_gt(nrow(parameters), 0)

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

    expect_equal(res$status, 200)

    lookup <- read.csv(text = res$body)

    expect_named(lookup, lookup_endpoints[[endpoint]])
    expect_gt(nrow(lookup), 0)
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
    utils::URLencode(format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC"), reserved = TRUE)
  }

  get_measurements <- get_v2(sprintf(
    paste0(
      "http://example.com/timeseries/measurements",
      "?id=%s&start=%s&end=%s&limit=10"
    ),
    test_timeseries_id,
    encode_time(test_timeseries_start),
    encode_time(test_timeseries_end)
  ))

  expect_equal(get_measurements$status, 200)

  measurements <- read.csv(text = get_measurements$body)
  if (
    identical(names(measurements), c("status", "message")) &&
      identical(measurements$status[1], "info")
  ) {
    skip("No measurements found for selected test timeseries")
  }

  expect_named(
    measurements,
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
      "grade_type_code",
      "approval_type_id",
      "approval_type_code",
      "qualifier_type_ids",
      "qualifier_type_codes",
      "owner_organization_id",
      "owner",
      "contributor_organization_id",
      "contributor"
    )
  )
  expect_gt(nrow(measurements), 0)

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
