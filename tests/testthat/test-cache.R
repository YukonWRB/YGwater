test_that("get_cached caches values within TTL", {
  test_env <- new.env(parent = emptyenv())
  fetch_count <- 0
  fetch_fun <- function() {
    fetch_count <<- fetch_count + 1
    fetch_count
  }

  first <- get_cached("example", fetch_fun, ttl = 3600, env = test_env)
  second <- get_cached("example", fetch_fun, ttl = 3600, env = test_env)

  expect_equal(first, 1)
  expect_equal(second, 1)
  expect_equal(fetch_count, 1)
})


test_that("get_cached reuses expired cache when check_fun allows it", {
  test_env <- new.env(parent = emptyenv())
  fetch_count <- 0
  check_count <- 0
  fetch_fun <- function() {
    fetch_count <<- fetch_count + 1
    fetch_count
  }
  check_fun <- function() {
    check_count <<- check_count + 1
    TRUE
  }

  initial <- get_cached("example", fetch_fun, ttl = 10, env = test_env)
  cache_env <- get("app_cache", envir = test_env)
  cache_env$example$timestamp <- cache_env$example$timestamp - 3600

  subsequent <- get_cached("example", fetch_fun, check_fun = check_fun, ttl = 10, env = test_env)

  expect_equal(initial, 1)
  expect_equal(subsequent, 1)
  expect_equal(fetch_count, 1)
  expect_equal(check_count, 1)
})


test_that("get_cached refreshes expired cache when check_fun rejects it", {
  test_env <- new.env(parent = emptyenv())
  fetch_count <- 0
  check_count <- 0
  fetch_fun <- function() {
    fetch_count <<- fetch_count + 1
    fetch_count
  }
  check_fun <- function() {
    check_count <<- check_count + 1
    FALSE
  }

  first <- get_cached("example", fetch_fun, ttl = 10, env = test_env)
  cache_env <- get("app_cache", envir = test_env)
  cache_env$example$timestamp <- cache_env$example$timestamp - 3600

  second <- get_cached("example", fetch_fun, check_fun = check_fun, ttl = 10, env = test_env)

  expect_equal(first, 1)
  expect_equal(second, 2)
  expect_equal(fetch_count, 2)
  expect_equal(check_count, 1)
})


test_that("clear_cached removes stored values", {
  test_env <- new.env(parent = emptyenv())
  fetch_count <- 0
  fetch_fun <- function() {
    fetch_count <<- fetch_count + 1
    fetch_count
  }

  get_cached("example", fetch_fun, env = test_env)
  clear_cached("example", env = test_env)

  cache_env <- get("app_cache", envir = test_env)
  expect_false(exists("example", envir = cache_env, inherits = FALSE))

  get_cached("example", fetch_fun, env = test_env)
  expect_equal(fetch_count, 2)
})
