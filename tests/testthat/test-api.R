test_that("api() builds a router and sets OpenAPI bits without running", {
  withr::local_envvar(list(
    aquacacheName = NA,
    aquacacheHost = NA,
    aquacachePort = NA
  ))

  pr <- api(run = FALSE, server = "/water-data/api", dbName = "aquacache_test")
  expect_s3_class(pr, "Plumber")

  spec <- pr$getApiSpec()
  expect_true("BasicAuth" %in% names(spec$components$securitySchemes))
  expect_equal(spec$components$securitySchemes$BasicAuth$scheme, "basic")
  expect_equal(spec$servers[[1]]$url, "/water-data/api")

  # check env vars were set
  expect_equal(Sys.getenv("aquacacheName"), "aquacache_test")
})
