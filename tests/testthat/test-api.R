test_that("api() builds a router and sets OpenAPI bits without running", {
  withr::local_envvar(list(
    aquacacheName = NA,
    aquacacheHost = NA,
    aquacachePort = NA
  ))

  pr <- api(
    run = FALSE,
    server = "/water-data/api",
    dbName = "aquacache_test",
    publicDbUser = "api_public",
    publicDbPass = "api_public_pass"
  )
  expect_s3_class(pr, "Plumber")

  spec <- pr$getApiSpec()
  expect_true("BasicAuth" %in% names(spec$components$securitySchemes))
  expect_equal(spec$components$securitySchemes$BasicAuth$scheme, "basic")
  expect_equal(spec$servers[[1]]$url, "/water-data/api")

  # check env vars were set
  expect_equal(Sys.getenv("APIaquacacheName"), "aquacache_test")
  expect_equal(Sys.getenv("APIaquacacheHost"), Sys.getenv("aquacacheHost"))
  expect_equal(Sys.getenv("APIaquacachePort"), Sys.getenv("aquacachePort"))
  expect_equal(Sys.getenv("APIaquacachePublicUser"), "api_public")
  expect_equal(Sys.getenv("APIaquacachePublicPass"), "api_public_pass")
  expect_equal(Sys.getenv("APIaquacacheLogRequests"), "TRUE")
})

test_that("API logging helpers extract request metadata without authorization secrets", {
  req <- list(
    REQUEST_METHOD = "GET",
    PATH_INFO = "/locations",
    QUERY_STRING = "format=json&lang=en",
    REMOTE_ADDR = "127.0.0.1",
    HTTP_AUTHORIZATION = paste0(
      "Basic ",
      jsonlite::base64_enc(charToRaw("api_user:api_password"))
    )
  )

  expect_equal(
    api_log_request_endpoint(req, api_version = 1L),
    "v1 GET /locations"
  )
  expect_equal(api_log_auth_user(req), "api_user")
  expect_equal(api_log_user_ip(req), "127.0.0.1")

  params <- jsonlite::fromJSON(api_log_parameters_json(req))
  expect_equal(params$query$format, "json")
  expect_equal(params$query$lang, "en")
  expect_false(grepl("api_password", api_log_parameters_json(req), fixed = TRUE))
})

test_that("API logging keeps docs shell requests but skips docs assets", {
  req <- function(path) {
    list(
      REQUEST_METHOD = "GET",
      PATH_INFO = path
    )
  }

  expect_true(api_log_request_should_log(req("/__docs__")))
  expect_true(api_log_request_should_log(req("/__docs__/")))
  expect_false(api_log_request_should_log(req("/__docs__/default.min.css")))
  expect_false(api_log_request_should_log(req("/__docs__/highlight.min.js")))
  expect_false(api_log_request_should_log(req("/__docs__/s/opensans/v18/font.woff2")))
  expect_false(api_log_request_should_log(req("/openapi.json")))
  expect_false(api_log_request_should_log(req("/openapi.yaml")))
})
