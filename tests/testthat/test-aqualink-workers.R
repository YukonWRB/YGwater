test_that("AquaLink checks, sizes and shuts down its application-wide mirai pool", {
  for (package in c("mirai", "parallelly", "future", "shiny")) skip_if_not_installed(package)
  launcher <- new.env(parent = asNamespace("YGwater"))
  launcher$AquaLink <- YGwater::AquaLink
  environment(launcher$AquaLink) <- launcher
  checks <- list()
  pool_calls <- list()
  stop_hooks <- list()
  connects <- 0L
  cores <- 7L
  missing_package <- NULL
  launcher$AquaConnect <- function(...) {
    connects <<- connects + 1L
    "mock connection"
  }
  launcher$source <- function(...) NULL
  launcher$YGwater_globals <- function(...) NULL

  local_mocked_bindings(
    check_installed = function(pkg, ..., version = NULL) {
      checks[pkg] <<- list(version)
      if (identical(pkg, missing_package)) stop("Missing package: ", pkg)
    },
    is_installed = function(...) TRUE,
    .package = "rlang"
  )
  local_mocked_bindings(availableCores = function(omit, ...) {
    expect_identical(omit, 1L)
    cores
  }, .package = "parallelly")
  local_mocked_bindings(
    dbExistsTable = function(...) TRUE,
    dbGetQuery = function(conn, statement, ...) {
      if (grepl("can_execute", statement, fixed = TRUE)) return(data.frame(can_execute = TRUE))
      if (grepl("Last patch number", statement, fixed = TRUE)) return(data.frame(version = 66))
      stop("Unexpected launcher query: ", statement)
    },
    dbDisconnect = function(...) TRUE,
    .package = "DBI"
  )
  local_mocked_bindings(plan = function(...) NULL, .package = "future")
  local_mocked_bindings(daemons = function(n, .compute, ...) {
    pool_calls[[length(pool_calls) + 1L]] <<- list(n = n, profile = .compute)
  }, .package = "mirai")
  local_mocked_bindings(
    onStop = function(fun, session) {
      expect_null(session) # Pool belongs to the application, never one session.
      stop_hooks[[length(stop_hooks) + 1L]] <<- fun
    },
    shinyAppDir = function(...) "server app",
    runApp = function(...) "local app",
    .package = "shiny"
  )

  for (case in list(
    list(workers = NULL, cores = 7L, expected = 3L, server = TRUE),
    list(workers = NULL, cores = 1L, expected = 1L, server = TRUE),
    list(workers = 2, cores = 7L, expected = 2L, server = FALSE)
  )) {
    cores <- case$cores
    result <- launcher$AquaLink(
      dbHost = "10.250.12.154", workers = case$workers, server = case$server
    )
    expect_identical(result, if (case$server) "server app" else "local app")
    expect_identical(tail(pool_calls, 1L)[[1L]], list(n = case$expected, profile = "YGwater_aqualink"))
    tail(stop_hooks, 1L)[[1L]]()
    expect_identical(tail(pool_calls, 1L)[[1L]], list(n = 0L, profile = "YGwater_aqualink"))
  }
  expect_identical(checks$mirai, "2.0.0")
  expect_identical(checks$promises, "1.3.0")
  expect_true("parallelly" %in% names(checks))

  calls_before <- connects
  for (workers in list(0, -1, 1.5, Inf, NA_real_, numeric(), c(1, 2), "2", TRUE,
                       .Machine$integer.max + 1)) {
    expect_error(launcher$AquaLink(dbHost = "10.250.12.154", workers = workers),
                 "positive whole number", fixed = TRUE)
  }
  expect_equal(connects, calls_before) # Invalid sizing fails before DB preflight.
  for (package in c("mirai", "promises", "parallelly")) {
    missing_package <- package
    expect_error(launcher$AquaLink(dbHost = "10.250.12.154"), "Missing package", fixed = TRUE)
  }
  expect_equal(connects, calls_before)
})

test_that("the parameter map consumes the app pool without managing workers", {
  skip_if_not_installed("mirai")
  skip_if_not_installed("shiny")
  module <- new.env(parent = asNamespace("YGwater"))
  sys.source(system.file("apps/aqualink/modules/client/map/paramsMap.R", package = "YGwater"), module)
  captured <- NULL
  local_mocked_bindings(
    mirai = function(.expr, ..., .timeout, .compute) {
      captured <<- list(args = list(...), timeout = .timeout, profile = .compute)
      "submitted"
    },
    status = function(...) stop("Module must not inspect pool sizing"),
    daemons = function(...) stop("Module must not manage daemons"),
    .package = "mirai"
  )
  local_mocked_bindings(check_installed = function(...) stop("Dependencies belong to the launcher"),
                        .package = "rlang")
  local_mocked_bindings(onStop = function(...) stop("Pool cleanup belongs to the launcher"),
                        .package = "shiny")
  result <- module$params_map_start_fetch(list(target = as.Date("2026-10-21")),
                                        list(dbUser = "test reader"), identity, identity)
  expect_identical(result, "submitted")
  expect_identical(captured$profile, "YGwater_aqualink")
  expect_equal(captured$timeout, 60000)
  expect_identical(captured$args$db$dbUser, "test reader")
  expect_identical(captured$args$fetch_fun, identity)
})
