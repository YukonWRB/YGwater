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

  subsequent <- get_cached(
    "example",
    fetch_fun,
    check_fun = check_fun,
    ttl = 10,
    env = test_env
  )

  expect_equal(initial, 1)
  expect_equal(subsequent, 1)
  expect_equal(fetch_count, 1)
  expect_equal(check_count, 1)
})


test_that("get_cached checks unexpired cache when check_fun allows it", {
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

  first <- get_cached("example", fetch_fun, ttl = 3600, env = test_env)
  second <- get_cached(
    "example",
    fetch_fun,
    check_fun = check_fun,
    ttl = 3600,
    env = test_env
  )

  expect_equal(first, 1)
  expect_equal(second, 1)
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

  second <- get_cached(
    "example",
    fetch_fun,
    check_fun = check_fun,
    ttl = 10,
    env = test_env
  )

  expect_equal(first, 1)
  expect_equal(second, 2)
  expect_equal(fetch_count, 2)
  expect_equal(check_count, 1)
})


test_that("get_cached refreshes unexpired cache when check_fun rejects it", {
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

  first <- get_cached("example", fetch_fun, ttl = 3600, env = test_env)
  second <- get_cached(
    "example",
    fetch_fun,
    check_fun = check_fun,
    ttl = 3600,
    env = test_env
  )

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

test_that("reference edits invalidate all dependent scoped caches in both stores", {
  editor <- new.env(parent = asNamespace("YGwater"))
  sys.source(
    system.file(
      "apps/aqualink/modules/admin/metadata/manageReferenceTables.R",
      package = "YGwater"
    ),
    editor
  )
  # Exercise real cache removal without touching the running process's cache.
  editor$.GlobalEnv <- new.env(parent = emptyenv())
  session_cache <- new.env(parent = emptyenv())
  session <- list(userData = list(app_cache = session_cache))
  obsolete <- c(
    "cont_data.plot_module_data",
    "disc_data_module_data",
    "map_location_module_data",
    "map_params_module_data",
    "map_params_module_data:dev:5432:aquacache:public_reader",
    "map_params_module_data:dev:5432:aquacache:editor",
    "map_params_module_data:dev:5432:testdb:public_reader"
  )
  prefixes <- c("cont_data.plot_module_data", "disc_data_module_data",
                "disc_plot_module_data_chemistry", "map_location_module_data")
  obsolete <- c(obsolete, prefixes,
    paste0(prefixes, ":dev:5432:aquacache:public_reader"),
    paste0(prefixes, ":dev:5432:aquacache:editor"),
    paste0(prefixes, ":dev:5432:testdb:public_reader"))
  keep <- c("wwr_module_data", "wwr_module_data:dev:5432:aquacache:public_reader",
            "map_params_module_data_other", "disc_plot_module_data_chemistry_other")
  for (env in list(editor$.GlobalEnv, session_cache)) {
    for (key in c(obsolete, keep)) {
      get_cached(key, function() "cached", env = env)
    }
  }
  editor$clear_reference_caches(session)
  expect_setequal(ls(editor$.GlobalEnv$app_cache), keep)
  expect_setequal(ls(session_cache$app_cache), keep)
  # A session without an authenticated cache still clears the public copy.
  key <- obsolete[[5L]]
  get_cached(key, function() "new public entry", env = editor$.GlobalEnv)
  editor$clear_reference_caches(list(userData = list()))
  expect_false(exists(
    key,
    envir = editor$.GlobalEnv$app_cache,
    inherits = FALSE
  ))
})

test_that("scoped invalidation preserves exact-key defaults and similar cache names", {
  env <- new.env(parent = emptyenv())
  keys <- c("wwr_module_data", "wwr_module_data:dev:5432:aquacache:public_reader",
            "wwr_module_data:dev:5432:testdb:editor", "wwr_module_data_other", "unrelated")
  for (key in keys) get_cached(key, function() "cached", env = env)
  clear_cached("wwr_module_data", env = env)
  expect_setequal(ls(env$app_cache), keys[-1L])
  clear_cached("wwr_module_data", env = env, all_scopes = TRUE)
  expect_setequal(ls(env$app_cache), tail(keys, 2L))
  # Clearing an empty private store must not remove its parent's cached data.
  child <- new.env(parent = env)
  clear_cached("unrelated", env = child, all_scopes = TRUE)
  expect_true(exists("unrelated", envir = env$app_cache, inherits = FALSE))
  expect_false(exists("app_cache", envir = child, inherits = FALSE))
})

test_that("every database module cache uses connection identity on hits and validity checks", {
  module <- new.env(parent = asNamespace("YGwater"))
  sys.source(system.file("apps/aqualink/modules/cache_functions.R", package = "YGwater"), module)
  module$cont_data_cache_signature <- function(con) "stable"
  module$disc_plot_cache_signature <- function(con) "stable"
  local_mocked_bindings(
    dbGetInfo = function(dbObj, ...) dbObj,
    dbGetQuery = function(...) stop("An existing scoped cache should be reused"),
    .package = "DBI"
  )
  prefixes <- c(
    cont_data.plot_module_data = "cont_data.plot_module_data",
    disc_plot_module_data = "disc_plot_module_data_chemistry",
    disc_data_module_data = "disc_data_module_data",
    map_params_module_data = "map_params_module_data",
    map_location_module_data = "map_location_module_data",
    wwr_module_data = "wwr_module_data"
  )
  base_info <- list(host = "dev", port = 5432L, dbname = "aquacache", user = "public_reader")
  identities <- list(base_info)
  for (field in names(base_info)) {
    changed <- base_info
    changed[[field]] <- switch(field, host = "other-host", port = 5433L,
                              dbname = "testdb", user = "editor")
    identities[[length(identities) + 1L]] <- changed
  }
  env <- new.env(parent = emptyenv())
  for (info in identities) {
    suffix <- paste(info$host, info$port, info$dbname, info$user, sep = ":")
    for (prefix in prefixes) {
      get_cached(paste(prefix, suffix, sep = ":"),
        function() list(cache_signature = "stable", marker = suffix), env = env)
    }
    result <- vapply(names(prefixes), function(name) {
      module[[name]](info, env = env)$marker
    }, character(1))
    expect_identical(unname(result), rep(suffix, length(prefixes)))
  }
})
