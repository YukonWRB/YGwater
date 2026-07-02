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
})
