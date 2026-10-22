# Exercise the sourced module, including its actual ExtendedTask lifecycle.
# No live connections or worker processes are needed for these unit checks.
params_map_test_module <- function() {
  module <- new.env(parent = asNamespace("YGwater"))
  for (package in c("shiny", "bslib")) {
    for (name in getNamespaceExports(package)) {
      module[[name]] <- getExportedValue(package, name)
    }
  }
  sys.source(
    system.file(
      "apps/aqualink/modules/client/map/paramsMap.R",
      package = "YGwater"
    ),
    module
  )
  module$config <- list(public = FALSE)
  module$application_notifications_ui <- function(...) NULL
  module$cached <- list(
    locations = data.table::data.table(
      location_id = 1:4,
      name = paste0("Site ", 1:4),
      name_fr = paste0("Lieu ", 1:4),
      latitude = 60:63,
      longitude = -135
    ),
    timeseries = data.table::data.table(
      timeseries_id = 1:4,
      location_id = 1:4,
      parameter_id = c(41L, 41L, 41L, 42L),
      units = c("m3/s", "m3/s", "m3/s", "m"),
      matrix_state = "Liquid"
    ),
    parameters = data.table::data.table(
      parameter_id = rep(c(41L, 42L), each = 2),
      param_name = rep(c("water flow", "water level"), each = 2),
      param_name_fr = rep(c("debit", "niveau"), each = 2),
      group_id = rep(c(4L, 8L), 2)
    )
  )
  module$map_params_module_data <- function(...) module$cached
  module$params_map_start_fetch <- function(
    request,
    db,
    fetch_fun,
    stats_sql_fun
  ) {
    promises::promise_resolve(params_map_test_result(request))
  }
  module
}

params_map_test_result <- function(request) {
  ids <- vapply(request$selections, `[[`, integer(1), "parameter_id")
  rows <- merge(
    request$timeseries[parameter_id %in% ids],
    request$locations,
    by = "location_id"
  )
  rows[, `:=`(
    date = as.Date("2026-10-01"),
    datetime = as.POSIXct("2026-10-01 12:00:00", tz = "UTC"),
    record_rate_seconds = 3600,
    sensor_priority = 1L,
    value = 5,
    max = 10,
    min = 0,
    doy_count = 20L,
    percent_historic_range = 50
  )]
  if (identical(request$include_history, FALSE)) {
    rows[, c("max", "min", "doy_count", "percent_historic_range") := NULL]
  }
  list(
    data = rows,
    stats_period = request$stats_period,
    latest = request$latest
  )
}

params_map_wait <- function(session, settled) {
  for (i in seq_len(100L)) {
    later::run_now(0.01)
    session$flushReact()
    if (settled()) return(invisible(NULL))
  }
  fail("Map request did not settle")
}

test_that("parameter maps handle old caches and preserve selections in both languages", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet")
  skip_if_not_installed("shinyjs")
  module <- params_map_test_module()
  popups <- character()
  add_markers <- leaflet::addCircleMarkers
  local_mocked_bindings(
    addCircleMarkers = function(map, ...) {
      popups <<- list(...)$popup
      add_markers(map, ...)
    },
    .package = "leaflet"
  )
  session <- shiny::MockShinySession$new()
  session$userData$user_logged_in <- FALSE
  shiny::testServer(
    module$mapParams,
    args = list(
      language = shiny::reactiveValues(language = "English", abbrev = "en")
    ),
    session = session,
    {
      session$setInputs(
        mapType = "range",
        target = as.Date("2026-10-01"),
        latest = FALSE,
        stats_period = "30yr",
        map_bounds = list(north = 65)
      )
      params_map_wait(session, function() map_state$status == "success")
      expect_false(session$isClosed())
      expect_identical(moduleData$parameters$parameter_id, c(41L, 42L))
      expect_equal(nrow(module$cached$parameters), 4L)
      expect_equal(map_params$param1, 41L) # Defaults resolve names, never fixed instance IDs.
      expect_match(output$primary_param$html, "water flow", fixed = TRUE)
      session$setInputs(edit_primary_param = 1)
      session$setInputs(
        param = "41",
        yrs = 12,
        days = 2,
        save_primary_param = 1
      )
      params_map_wait(session, function() map_state$status == "success")
      expect_match(output$primary_param$html, "12", fixed = TRUE)
      session$setInputs(edit_secondary_param = 1)
      session$setInputs(param = "42", save_secondary_param = 1)
      params_map_wait(session, function() map_state$status == "success")
      expect_equal(map_params$params, 2L)
      expect_match(output$secondary_param$html, "water level", fixed = TRUE)
      session$setInputs(latest = TRUE, auto_update_latest = TRUE)
      params_map_wait(session, function() map_state$status == "success")
      expect_equal(map_params$target, as.Date("2026-10-01"))
      expect_true(map_state$request$latest)
      expect_true(all(grepl("Record rate: 1H", popups, fixed = TRUE)))
      language$language <- "Fran\u00e7ais"
      language$abbrev <- "fr"
      session$flushReact()
      expect_match(output$primary_param$html, "debit", fixed = TRUE)
      expect_match(output$secondary_param$html, "niveau", fixed = TRUE)
      expect_true(all(grepl(
        tr("record_rate", "Fran\u00e7ais"),
        popups,
        fixed = TRUE
      )))
      expect_match(
        output$sidebar_controls$html,
        'id="proxy1-latest"',
        fixed = TRUE
      )
      expect_false(session$isClosed())
      session$setInputs(remove_secondary_param = 1)
      expect_null(output$secondary_param)
      session$setInputs(mapType = "actual", latest = FALSE)
      params_map_wait(session, function() map_state$status == "success")
      expect_equal(map_params$target, as.Date("2026-10-01"))
      expect_false(session$isClosed())
    }
  )
})

test_that("pending map work leaves controls usable and coalesces to the newest request", {
  module <- params_map_test_module()
  requests <- list()
  resolve <- list()
  module$params_map_start_fetch <- function(
    request,
    db,
    fetch_fun,
    stats_sql_fun
  ) {
    index <- length(requests) + 1L
    requests[[index]] <<- request
    promises::promise(function(res, rej) resolve[[index]] <<- res)
  }
  session <- shiny::MockShinySession$new()
  shiny::testServer(
    module$mapParams,
    args = list(
      language = shiny::reactiveValues(language = "English", abbrev = "en")
    ),
    session = session,
    {
      session$setInputs(
        mapType = "range",
        latest = TRUE,
        auto_update_latest = TRUE,
        stats_period = "30yr",
        map_bounds = list(north = 65)
      )
      expect_identical(fetch_task$status(), "running")
      expect_length(requests, 1L)
      # The modal is available immediately while the first promise is unresolved.
      session$setInputs(
        edit_primary_param = 1,
        param = "42",
        yrs = 10,
        days = 1,
        save_primary_param = 1
      )
      expect_equal(map_params$param1, 42L)
      session$setInputs(days = 2, save_primary_param = 2)
      session$setInputs(days = 3, save_primary_param = 3)
      expect_equal(map_params$days1, 3L)
      expect_length(requests, 1L)
      expect_identical(map_state$status, "loading")
      resolve[[1L]](params_map_test_result(requests[[1L]]))
      params_map_wait(session, function() length(requests) == 2L)
      expect_null(map_state$data) # Old result was never applied.
      expect_equal(requests[[2L]]$selections[[1L]]$parameter_id, 42L)
      expect_equal(requests[[2L]]$selections[[1L]]$days, 3L)
      resolve[[2L]](params_map_test_result(requests[[2L]]))
      params_map_wait(session, function() map_state$status == "success")
      expect_identical(unique(map_state$data$parameter_id), 42L)
      calls <- length(requests)
      session$setInputs(mapType = "actual")
      expect_length(requests, calls + 1L)
      expect_false(tail(requests, 1L)[[1L]]$include_history)
      calls <- length(requests)
      resolve[[calls]](params_map_test_result(requests[[calls]]))
      params_map_wait(session, function() map_state$status == "success")
      language$language <- "Fran\u00e7ais"
      language$abbrev <- "fr"
      session$flushReact()
      expect_length(requests, calls)
      session$setInputs(auto_update_latest = FALSE)
      session$setInputs(auto_update_latest = TRUE)
      expect_length(requests, calls)
      # The real 15-minute timer dispatches once, without rewriting inputs.
      session$elapse(15 * 60 * 1000)
      expect_length(requests, calls + 1L)
      resolve[[calls + 1L]](params_map_test_result(requests[[calls + 1L]]))
      params_map_wait(session, function() map_state$status == "success")
      expect_equal(map_params$param1, 42L)
      expect_equal(map_params$days1, 3L)
      session$setInputs(latest = FALSE)
      calls <- length(requests)
      resolve[[calls]](params_map_test_result(requests[[calls]]))
      params_map_wait(session, function() map_state$status == "success")
      calls <- length(requests)
      session$elapse(15 * 60 * 1000)
      expect_length(requests, calls)
      expect_false(session$isClosed())
    }
  )
})

test_that("query failures, empty data and invalid selections keep the session alive", {
  module <- params_map_test_module()
  module$fail_request <- TRUE
  module$params_map_start_fetch <- function(
    request,
    db,
    fetch_fun,
    stats_sql_fun
  ) {
    if (module$fail_request) {
      return(promises::promise_reject(simpleError("statement timeout")))
    }
    result <- params_map_test_result(request)
    result$data <- result$data[0]
    promises::promise_resolve(result)
  }
  session <- shiny::MockShinySession$new()
  shiny::testServer(
    module$mapParams,
    args = list(
      language = shiny::reactiveValues(language = "English", abbrev = "en")
    ),
    session = session,
    {
      session$setInputs(
        mapType = "range",
        latest = FALSE,
        stats_period = "30yr",
        map_bounds = list(north = 65)
      )
      suppressWarnings(params_map_wait(session, function() {
        map_state$status == "error"
      }))
      expect_false(session$isClosed())
      expect_match(output$map_status$html, "Try again", fixed = TRUE)
      session$setInputs(
        param = "not an id",
        yrs = NA,
        days = -1,
        save_primary_param = 1
      )
      expect_equal(map_params$param1, 41L)
      expect_equal(map_params$days1, 1L)
      module$fail_request <- FALSE
      session$setInputs(retry_map = 1)
      params_map_wait(session, function() map_state$status == "success")
      expect_match(
        output$map_status$html,
        "No measurements match",
        fixed = TRUE
      )
      expect_false(session$isClosed())
    }
  )
})

test_that("map fetches preserve primary coverage and use real day tolerances", {
  module <- params_map_test_module()
  request <- list(
    selections = list(
      list(parameter_id = 41L, years = 10L, days = 3L),
      list(parameter_id = 42L, years = 10L, days = 0L)
    ),
    latest = TRUE,
    now = as.POSIXct("2026-10-22 03:00:00", tz = "UTC"),
    target = as.Date("2026-10-21"),
    stats_period = "30yr",
    timeseries = module$cached$timeseries,
    locations = module$cached$locations
  )
  # Put the secondary at an already-covered site, then an uncovered site.
  request$timeseries <- data.table::rbindlist(list(
    request$timeseries,
    data.table::data.table(
      timeseries_id = 5L,
      location_id = 1L,
      parameter_id = 42L,
      units = "m",
      matrix_state = "Liquid"
    )
  ))
  statements <- list()
  query_number <- 0L
  primary_empty <- FALSE
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params = NULL, ...) {
      query_number <<- query_number + 1L
      statements[[query_number]] <<- list(sql = statement, params = params)
      primary <- grepl("IN (1,2,3)", statement, fixed = TRUE)
      if (!primary) {
        expect_false(
          grepl("IN (4,5)", statement, fixed = TRUE) && !primary_empty
        )
      }
      ids <- if (primary) {
        if (primary_empty) integer() else 1:3
      } else {
        4L
      }
      data.frame(
        timeseries_id = ids,
        datetime = rep(request$now, length(ids)),
        record_rate_seconds = rep(3600, length(ids)),
        sensor_priority = rep(1L, length(ids)),
        date = rep(as.Date(request$now), length(ids)),
        value = rep(5, length(ids)),
        max = rep(10, length(ids)),
        min = rep(0, length(ids)),
        doy_count = rep(20L, length(ids)),
        percent_historic_range = rep(50, length(ids))
      )
    },
    .package = "DBI"
  )
  stats_sql <- function(...) {
    structure(
      c(
        max = 'd."max_30yr" AS max',
        min = 'd."min_30yr" AS min',
        doy_count = 'd."doy_count_30yr" AS doy_count',
        percent_historic_range = 'd."percent_historic_range_30yr" AS percent_historic_range'
      ),
      count_column = 'd."doy_count_30yr"',
      stats_period = "30yr"
    )
  }
  result <- module$params_map_fetch_data(DBI::ANSI(), request, stats_sql)
  expect_equal(nrow(result$data), 4L)
  expect_equal(result$data$percent_historic_range, rep(50, 4))
  expect_equal(
    statements[[1]]$params[[1]],
    as.POSIXct("2026-10-19", tz = "UTC")
  )
  expect_equal(
    statements[[2]]$params[[1]],
    as.POSIXct("2026-10-22", tz = "UTC")
  )
  expect_match(
    statements[[1]]$sql,
    "normalized_day_of_year((latest.datetime AT TIME ZONE 'UTC')::date)",
    fixed = TRUE
  )
  expect_match(
    statements[[1]]$sql,
    "continuous.apply_corrections",
    fixed = TRUE
  )
  expect_match(
    statements[[1]]$sql,
    "continuous.measurements_continuous_corrected(",
    fixed = TRUE
  )
  request$now <- as.POSIXct("2028-02-29 03:00:00", tz = "UTC")
  module$params_map_fetch_data(DBI::ANSI(), request, stats_sql)
  expect_match(
    statements[[length(statements)]]$sql,
    "EXTRACT(DAY FROM d.date) = 29",
    fixed = TRUE
  )
  primary_empty <- TRUE
  result <- module$params_map_fetch_data(DBI::ANSI(), request, stats_sql)
  expect_identical(result$data$parameter_id, 42L)
  # No secondary candidates must leave every primary result intact.
  request$timeseries <- request$timeseries[parameter_id == 41L]
  primary_empty <- FALSE
  result <- module$params_map_fetch_data(DBI::ANSI(), request, stats_sql)
  expect_equal(nrow(result$data), 3L)
  expect_true(all(result$data$parameter_id == 41L))
  request$latest <- FALSE
  result <- module$params_map_fetch_data(DBI::ANSI(), request, stats_sql)
  expect_match(
    statements[[length(statements)]]$sql,
    "ORDER BY ABS(d.date - $1::date), d.date DESC LIMIT 1",
    fixed = TRUE
  )
  expect_equal(nrow(result$data), 3L)
})

test_that("connection replacement discards old results and snapshots the new database identity", {
  module <- params_map_test_module()
  requests <- list()
  resolvers <- list()
  module$params_map_start_fetch <- function(
    request,
    db,
    fetch_fun,
    stats_sql_fun
  ) {
    index <- length(requests) + 1L
    requests[[index]] <<- list(query = request, db = db)
    promises::promise(function(resolve, reject) resolvers[[index]] <<- resolve)
  }
  session <- shiny::MockShinySession$new()
  session$userData$AquaCache_version <- shiny::reactiveVal(0L)
  session$userData$config <- list(dbName = "first_db", dbUser = "first_user")
  shiny::testServer(
    module$mapParams,
    args = list(
      language = shiny::reactiveValues(language = "English", abbrev = "en")
    ),
    session = session,
    {
      session$setInputs(
        mapType = "range",
        latest = FALSE,
        stats_period = "30yr",
        map_bounds = list(north = 65)
      )
      expect_length(requests, 1L)
      module$cached$parameters[, parameter_id := parameter_id + 100L]
      module$cached$timeseries[, parameter_id := parameter_id + 100L]
      session$userData$config <- list(
        dbName = "second_db",
        dbUser = "second_user"
      )
      session$userData$AquaCache_version(1L)
      session$flushReact()
      expect_equal(map_params$param1, 141L)
      expect_equal(map_params$param2, 142L)
      resolvers[[1]](params_map_test_result(requests[[1]]$query))
      params_map_wait(session, function() length(requests) == 2L)
      expect_null(map_state$data)
      expect_equal(requests[[2]]$db$dbName, "second_db")
      expect_equal(requests[[2]]$db$dbUser, "second_user")
      resolvers[[2]](params_map_test_result(requests[[2]]$query))
      params_map_wait(session, function() map_state$status == "success")
      expect_true(all(map_state$data$parameter_id == 141L))
      expect_false(session$isClosed())
    }
  )
})

test_that("map metadata caches separate database instances and users", {
  module <- new.env(parent = asNamespace("YGwater"))
  sys.source(
    system.file("apps/aqualink/modules/cache_functions.R", package = "YGwater"),
    module
  )
  keys <- character()
  module$get_cached <- function(key, ...) {
    keys <<- c(keys, key)
    list()
  }
  local_mocked_bindings(
    dbGetInfo = function(dbObj, ...) dbObj,
    .package = "DBI"
  )
  info <- list(
    host = "test-host",
    port = 5432,
    dbname = "first_db",
    user = "first_user"
  )
  module$map_params_module_data(info)
  info$dbname <- "second_db"
  module$map_params_module_data(info)
  info$user <- "second_user"
  module$map_params_module_data(info)
  expect_length(unique(keys), 3L)
})

test_that("historical statistics report the effective period and quote its count column", {
  module <- params_map_test_module()
  columns <- c("max", "min", "doy_count", "percent_historic_range")
  available <- c(columns, paste0(columns, "_30yr"))
  local_mocked_bindings(
    dbGetQuery = function(...) data.frame(column_name = available),
    .package = "DBI"
  )
  sql <- module$params_map_daily_stats_select_sql(DBI::ANSI())
  expect_identical(attr(sql, "stats_period"), "30yr")
  expect_identical(attr(sql, "count_column"), 'd."doy_count_30yr"')
  expect_equal(unname(sql[["max"]]), 'd."max_30yr" AS "max"')
  available <- columns
  sql <- module$params_map_daily_stats_select_sql(DBI::ANSI())
  expect_identical(attr(sql, "stats_period"), "full")
  expect_identical(attr(sql, "count_column"), 'd."doy_count"')
})

test_that("map legends handle negative constants, flat history and different units", {
  module <- params_map_test_module()
  module$cached$locations$name[1L] <- "<script>test</script>"
  module$params_map_start_fetch <- function(
    request,
    db,
    fetch_fun,
    stats_sql_fun
  ) {
    result <- params_map_test_result(request)
    result$data[, `:=`(
      value = -5,
      min = -5,
      max = -5,
      percent_historic_range = NA_real_
    )]
    result$data$units[1L] <- "different unit"
    promises::promise_resolve(result)
  }
  legends <- list()
  popups <- list()
  add_legend <- leaflet::addLegend
  add_markers <- leaflet::addCircleMarkers
  local_mocked_bindings(
    addLegend = function(map, ...) {
      legends[[length(legends) + 1L]] <<- list(...)
      add_legend(map, ...)
    },
    addCircleMarkers = function(map, ...) {
      popups[[length(popups) + 1L]] <<- list(...)$popup
      add_markers(map, ...)
    },
    .package = "leaflet"
  )
  session <- shiny::MockShinySession$new()
  shiny::testServer(
    module$mapParams,
    args = list(
      language = shiny::reactiveValues(language = "English", abbrev = "en")
    ),
    session = session,
    {
      session$setInputs(
        mapType = "range",
        latest = FALSE,
        stats_period = "30yr",
        map_bounds = list(north = 65)
      )
      params_map_wait(session, function() map_state$status == "success")
      expect_false(session$isClosed())
      expect_true(any(grepl(
        "&lt;script&gt;test&lt;/script&gt;",
        unlist(popups),
        fixed = TRUE
      )))
      before <- length(legends)
      session$setInputs(mapType = "actual")
      params_map_wait(session, function() {
        map_state$status == "success" && length(legends) > before
      })
      expect_false(session$isClosed())
      expect_length(legends, before + 2L)
      titles <- vapply(tail(legends, 2L), `[[`, character(1), "title")
      expect_true(any(grepl("different unit", titles, fixed = TRUE)))
      expect_true(any(grepl("m3/s", titles, fixed = TRUE)))
      expect_true(all(vapply(
        tail(legends, 2L),
        function(x) all(x$values == -5),
        logical(1)
      )))
    }
  )
})

test_that("latest maps choose the fastest eligible series at each parameter/location", {
  module <- params_map_test_module()
  # Faster intervals beat both newer slow observations and sensor priority.
  # Equal intervals use priority, then recency, then ID, independent of order.
  series <- data.table::fread(
    text = "timeseries_id location_id parameter_id record_rate_seconds sensor_priority offset value
101 1 41 86400 1 0 1
102 1 41 3600 2 -3600 2
103 1 41 60 3 -7200 3
201 2 41 3600 2 0 4
202 2 41 3600 1 -3600 5
203 2 41 3600 NA 0 6
301 3 41 3600 1 -3600 7
302 3 41 3600 1 0 8
303 3 41 3600 1 0 9
401 4 41 NA 1 0 10
402 4 41 86400 2 -3600 11
501 5 41 NA NA 0 12
502 5 41 0 NA 0 13
503 5 41 -1 NA 0 14
601 6 41 60 1 0 NA
602 6 41 3600 1 0 15
701 7 41 3600 1 0 16
702 7 41 86400 1 0 17
802 8 42 3600 1 0 18
803 8 42 86400 1 0 19
901 9 41 3600 1 0 20
902 9 42 60 1 0 21"
  )
  now <- as.POSIXct("2026-10-22 12:00:00", tz = "UTC")
  request <- list(
    selections = list(
      list(parameter_id = 41L, years = 10L, days = 1L),
      list(parameter_id = 42L, years = 10L, days = 1L)
    ),
    latest = TRUE,
    now = now,
    target = as.Date(now),
    stats_period = "30yr",
    # The metadata snapshot deliberately lacks interval/priority, as in older
    # shared caches. Selection must use the live query's metadata instead.
    timeseries = series[, .(
      timeseries_id,
      location_id,
      parameter_id,
      units = "degC",
      matrix_state = "Gas"
    )],
    locations = data.table::data.table(
      location_id = 1:9,
      name = "Station",
      name_fr = "Station",
      latitude = 60 + (1:9) / 10,
      longitude = -135
    )
  )
  reverse_rows <- FALSE
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, ...) {
      match <- regmatches(
        statement,
        regexec("timeseries_id IN \\(([^)]+)\\)", statement)
      )[[1L]]
      ids <- as.integer(strsplit(match[2L], ",", fixed = TRUE)[[1L]])
      # Simulate the SQL minimum-history filter excluding 701. Missing/invalid
      # values are also ineligible; the fastest remaining usable series wins.
      out <- series[
        timeseries_id %in% ids & timeseries_id != 701L,
        .(
          timeseries_id,
          record_rate_seconds = as.numeric(record_rate_seconds),
          sensor_priority,
          datetime = now + offset,
          date = as.Date(now),
          value = as.numeric(value),
          max = 25,
          min = -25,
          doy_count = 20L
        )
      ]
      if (reverse_rows) {
        out <- out[rev(seq_len(nrow(out)))]
      }
      out
    },
    .package = "DBI"
  )
  stats_sql <- function(...) {
    structure(
      c(
        max = "max",
        min = "min",
        doy_count = "doy_count",
        percent_historic_range = "percent_historic_range"
      ),
      count_column = "doy_count",
      stats_period = "30yr"
    )
  }
  result <- module$params_map_fetch_data(DBI::ANSI(), request, stats_sql)$data
  data.table::setorder(result, location_id, parameter_id)
  expect_identical(
    result$timeseries_id,
    c(103L, 202L, 302L, 402L, 501L, 602L, 702L, 802L, 901L)
  )
  expect_equal(nrow(result), 9L)
  expect_false(anyDuplicated(result[, c("location_id", "parameter_id")]) > 0L)
  expect_equal(result$value[1L], 3) # Minute value, not the daily mean of 1.
  expect_equal(result$parameter_id[result$location_id == 9L], 41L)
  expect_false(any(c("rate_rank", "priority_rank") %in% names(result)))
  reverse_rows <- TRUE
  reordered <- module$params_map_fetch_data(
    DBI::ANSI(),
    request,
    stats_sql
  )$data
  data.table::setorder(reordered, location_id, parameter_id)
  expect_equal(reordered, result)
  request$latest <- FALSE
  daily <- module$params_map_fetch_data(DBI::ANSI(), request, stats_sql)$data
  expect_gt(nrow(daily), nrow(result)) # Historical daily-mean selection is unchanged.
})

test_that("absolute maps omit historical criteria and preserve the relative selection", {
  module <- params_map_test_module()
  modal <- ""
  popups <- character()
  module$showModal <- function(ui, ...) modal <<- as.character(ui)
  add_markers <- leaflet::addCircleMarkers
  local_mocked_bindings(
    addCircleMarkers = function(map, ...) {
      popups <<- list(...)$popup
      add_markers(map, ...)
    },
    .package = "leaflet"
  )
  session <- shiny::MockShinySession$new()
  shiny::testServer(
    module$mapParams,
    args = list(
      language = shiny::reactiveValues(language = "English", abbrev = "en")
    ),
    session = session,
    {
      session$setInputs(
        mapType = "range",
        latest = TRUE,
        stats_period = "30yr",
        map_bounds = list(north = 65)
      )
      params_map_wait(session, function() map_state$status == "success")
      session$setInputs(
        edit_primary_param = 1,
        param = "41",
        yrs = 12,
        days = 2,
        save_primary_param = 1
      )
      params_map_wait(session, function() map_state$status == "success")
      expect_match(modal, 'id="proxy1-yrs"', fixed = TRUE)
      expect_true(all(grepl("Years of record", popups, fixed = TRUE)))

      session$setInputs(mapType = "actual")
      params_map_wait(session, function() map_state$status == "success")
      session$setInputs(edit_primary_param = 2)
      expect_false(grepl('id="proxy1-yrs"', modal, fixed = TRUE))
      expect_match(modal, 'id="proxy1-days"', fixed = TRUE)
      expect_false(grepl(
        "years of record",
        output$primary_param$html,
        fixed = TRUE
      ))
      expect_false(any(grepl(
        "Historic range|Years of record|% historic range",
        popups
      )))
      expect_true(all(grepl("Record rate: 1H", popups, fixed = TRUE)))
      expect_false(map_state$request$include_history)
      expect_null(map_state$request$stats_period)
      expect_null(map_state$request$selections[[1L]]$years)
      expect_false(any(c("max", "min", "doy_count") %in% names(map_state$data)))
      # The removed control can be absent or stale without invalidating Save.
      session$setInputs(yrs = NULL, days = 3, save_primary_param = 2)
      params_map_wait(session, function() map_state$status == "success")
      expect_equal(map_params$days1, 3L)
      expect_equal(map_params$yrs1, 12L)
      session$setInputs(days = -1, save_primary_param = 3)
      expect_equal(map_params$days1, 3L)
      language$language <- "Fran\u00e7ais"
      language$abbrev <- "fr"
      session$flushReact()
      session$setInputs(edit_primary_param = 3)
      expect_false(grepl('id="proxy1-yrs"', modal, fixed = TRUE))
      expect_false(grepl(
        tr("map_min_yrs_selected2", language$language),
        output$primary_param$html,
        fixed = TRUE
      ))
      expect_false(any(grepl(
        tr("map_actual_hist_range", language$language),
        popups,
        fixed = TRUE
      )))
      expect_true(all(grepl(
        tr("value", language$language),
        popups,
        fixed = TRUE
      )))

      session$setInputs(mapType = "range")
      params_map_wait(session, function() map_state$status == "success")
      session$setInputs(edit_primary_param = 4)
      expect_match(modal, 'id="proxy1-yrs"', fixed = TRUE)
      expect_equal(map_state$request$selections[[1L]]$years, 12L)
      expect_true(map_state$request$include_history)
      expect_true(all(grepl(
        tr("map_actual_yrs", language$language),
        popups,
        fixed = TRUE
      )))
      session$setInputs(yrs = NA, days = 4, save_primary_param = 4)
      expect_equal(map_params$days1, 3L)
      expect_false(session$isClosed())
    }
  )
})

test_that("absolute queries never require historical statistics or years of record", {
  module <- params_map_test_module()
  request <- list(
    selections = list(list(parameter_id = 41L, days = 1L)),
    include_history = FALSE,
    stats_period = NULL,
    latest = TRUE,
    now = as.POSIXct("2026-10-22 12:00:00", tz = "UTC"),
    target = as.Date("2026-10-21"),
    timeseries = data.table::data.table(
      timeseries_id = 1:2,
      location_id = 1L,
      parameter_id = 41L,
      units = "degC",
      matrix_state = "Gas"
    ),
    locations = module$cached$locations
  )
  queries <- list()
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, params, ...) {
      queries[[length(queries) + 1L]] <<- list(sql = statement, params = params)
      data.table::data.table(
        timeseries_id = 1:2,
        value = c(7, 9),
        datetime = request$now,
        date = as.Date(request$now),
        record_rate_seconds = c(86400, 60),
        sensor_priority = 1L
      )
    },
    .package = "DBI"
  )
  no_history <- function(...) {
    stop("Absolute maps must not inspect historical statistics")
  }
  result <- module$params_map_fetch_data(DBI::ANSI(), request, no_history)
  expect_identical(result$data$timeseries_id, 2L)
  expect_equal(result$data$value, 9)
  expect_null(result$stats_period)
  expect_false(grepl(
    "measurements_calculated_daily|doy_count|\\$3",
    queries[[1L]]$sql
  ))
  expect_length(queries[[1L]]$params, 2L)
  expect_false(any(
    c("max", "min", "doy_count", "percent_historic_range") %in%
      names(result$data)
  ))
  request$latest <- FALSE
  daily <- module$params_map_fetch_data(DBI::ANSI(), request, no_history)
  expect_equal(nrow(daily$data), 2L)
  expect_match(queries[[2L]]$sql, "SELECT d.date, d.value", fixed = TRUE)
  expect_false(grepl(
    "doy_count|percent_historic_range|\\$3",
    queries[[2L]]$sql
  ))
  expect_length(queries[[2L]]$params, 2L)
  expect_null(daily$stats_period)
})
