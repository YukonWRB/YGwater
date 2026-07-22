test_that("API request budgets accept legitimate bounded work", {
  budget <- YGwater:::api_request_budget(
    limit = 100L,
    id_groups = list(id = 1:2),
    start = as.POSIXct("2025-01-01", tz = "UTC"),
    end = as.POSIXct("2025-12-31", tz = "UTC")
  )
  continuous_budget <- YGwater:::api_request_budget(
    limit = 1000000L,
    max_rows = 1000000L
  )
  sample_result_budget <- YGwater:::api_request_budget(
    id_groups = list(sample_ids = seq_len(100000L)),
    max_ids = 100000L
  )

  expect_true(budget$valid)
  expect_equal(budget$limit, 100L)
  expect_null(budget$message)
  expect_true(continuous_budget$valid)
  expect_equal(continuous_budget$limit, 1000000L)
  expect_true(sample_result_budget$valid)
})

test_that("API request budgets reject attacker-controlled amplification", {
  too_many_rows <- YGwater:::api_request_budget(
    limit = 1000001L,
    max_rows = 1000000L
  )
  too_many_ids <- YGwater:::api_request_budget(
    id_groups = list(sample_ids = seq_len(100001L)),
    max_ids = 100000L
  )
  reversed_range <- YGwater:::api_request_budget(
    start = as.Date("2025-02-01"),
    end = as.Date("2025-01-01")
  )
  oversized_range <- YGwater:::api_request_budget(
    start = as.Date("1000-01-01"),
    end = as.Date("3000-01-01")
  )

  expect_false(too_many_rows$valid)
  expect_match(too_many_rows$message, "at most 1000000")
  expect_false(too_many_ids$valid)
  expect_match(too_many_ids$message, "sample_ids.*at most 100000")
  expect_false(reversed_range$valid)
  expect_match(reversed_range$message, "before or equal")
  expect_false(oversized_range$valid)
  expect_match(oversized_range$message, "at most 73050 days")
})

test_that("invalid non-positive limits preserve the documented default", {
  expect_equal(YGwater:::api_request_budget(limit = 0L)$limit, 100000L)
  expect_equal(YGwater:::api_request_budget(limit = "invalid")$limit, 100000L)
})

test_that("API V1 rejects oversized work before connecting to the database", {
  skip_if_not_installed("plumber")
  skip_if_not_installed("reqres")

  withr::local_envvar(list(APIaquacacheLogRequests = "FALSE"))
  pr <- api(
    version = 1,
    run = FALSE,
    dbName = "request_budget_test",
    dbHost = "127.0.0.1",
    dbPort = "1",
    publicDbUser = "request_budget_test",
    publicDbPass = "request_budget_test"
  )

  oversized_sample_ids <- paste(seq_len(100001L), collapse = ",")
  urls <- c(
    paste0(
      "http://example.com/timeseries/measurements",
      "?id=1&start=2025-01-01&end=2025-01-02&limit=1000001"
    ),
    paste0(
      "http://example.com/timeseries/measurementsDaily",
      "?id=1&start=2025-01-01&end=2025-01-02&limit=1000001"
    ),
    paste0(
      "http://example.com/samples",
      "?start=2025-01-01&end=2025-01-02&limit=100001"
    ),
    "http://example.com/samples/results?sample_ids=1&limit=100001",
    paste0(
      "http://example.com/samples/results?sample_ids=",
      oversized_sample_ids,
      "&limit=1"
    )
  )
  expected_maximum <- c(1000000L, 1000000L, 100000L, 100000L, 100000L)
  responses <- lapply(urls, function(url) {
    pr$call(reqres:::mock_rook(url = url, method = "get"))
  })

  expect_equal(
    vapply(responses, `[[`, numeric(1L), "status"),
    rep(400, length(responses))
  )
  for (i in seq_along(responses)) {
    expect_match(
      responses[[i]]$body,
      sprintf("at most %d", expected_maximum[[i]])
    )
  }

  accepted_urls <- c(
    paste0(
      "http://example.com/timeseries/measurements",
      "?id=1&start=2025-01-01&end=2025-01-02&limit=1000000"
    ),
    paste0(
      "http://example.com/samples/results?sample_ids=",
      paste(seq_len(100000L), collapse = ","),
      "&limit=1"
    )
  )
  accepted_responses <- lapply(accepted_urls, function(url) {
    pr$call(reqres:::mock_rook(url = url, method = "get"))
  })
  expect_equal(
    vapply(accepted_responses, `[[`, numeric(1L), "status"),
    c(503, 503)
  )
})

test_that("API V2 rejects oversized work before async database dispatch", {
  skip_if_not_installed("plumber2")
  skip_if_not_installed("reqres")
  skip_if_not_installed("promises")
  skip_if_not_installed("later")

  withr::local_options(list(
    plumber2.async = function(expr, envir) {
      promises::promise_resolve(eval(expr, envir = envir))
    }
  ))
  withr::local_envvar(list(
    APIaquacacheLogRequests = "FALSE",
    YGWATER_API_V2_CACHE_DIR = file.path(
      tempdir(),
      "ygwater-api-budget-v2"
    )
  ))
  pr <- api(
    version = 2,
    run = FALSE,
    dbName = "request_budget_test",
    dbHost = "127.0.0.1",
    dbPort = "1",
    publicDbUser = "request_budget_test",
    publicDbPass = "request_budget_test"
  )

  oversized_sample_ids <- paste(seq_len(100001L), collapse = ",")
  urls <- c(
    paste0(
      "http://example.com/timeseries/measurements",
      "?id=1&start=2025-01-01&end=2025-01-02&limit=1000001"
    ),
    paste0(
      "http://example.com/timeseries/measurementsDaily",
      "?id=1&start=2025-01-01&end=2025-01-02&limit=1000001"
    ),
    paste0(
      "http://example.com/samples",
      "?start=2025-01-01&end=2025-01-02&limit=100001"
    ),
    "http://example.com/samples/results?sample_ids=1&limit=100001"
  )
  expected_maximum <- c(1000000L, 1000000L, 100000L, 100000L)
  get_v2 <- function(url) {
    response <- pr$test_request(reqres:::mock_rook(url = url, method = "get"))
    if (promises::is.promise(response)) {
      result <- new.env(parent = emptyenv())
      result$resolved <- FALSE
      response$then(function(value) {
        result$response <- value
        result$resolved <- TRUE
      })
      for (i in seq_len(500L)) {
        if (result$resolved) {
          break
        }
        later::run_now(0.01)
      }
      expect_true(result$resolved)
      response <- result$response
    }
    response
  }
  responses <- lapply(urls, get_v2)

  expect_equal(
    vapply(responses, `[[`, numeric(1L), "status"),
    rep(400, length(responses))
  )
  for (i in seq_along(responses)) {
    expect_match(
      responses[[i]]$body,
      sprintf("at most %d", expected_maximum[[i]])
    )
  }

  route <- pr$request_router$get_route("plumber")
  outer_handler <- route$get_handler("get", "/samples/results")
  handler_env <- get("envir", envir = environment(outer_handler))
  sample_results_handler <- get("handler", envir = handler_env)
  oversized_ids_response <- sample_results_handler(
    client_id = "oversized-sample-ids",
    query = list(sample_ids = oversized_sample_ids, limit = 1L)
  )
  expect_equal(oversized_ids_response$status, 400L)
  expect_match(
    oversized_ids_response$body$message,
    "sample_ids.*at most 100000"
  )

  accepted_continuous <- get_v2(paste0(
    "http://example.com/timeseries/measurements",
    "?id=1&start=2025-01-01&end=2025-01-02&limit=1000000"
  ))
  accepted_ids_response <- sample_results_handler(
    client_id = "accepted-sample-ids",
    query = list(
      sample_ids = paste(seq_len(100000L), collapse = ","),
      limit = 1L
    )
  )
  expect_equal(accepted_continuous$status, 503L)
  expect_equal(accepted_ids_response$status, 503L)
})
