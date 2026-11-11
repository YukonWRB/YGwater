# Exercise the sourced module's synchronous queries. Mock DBI at the boundary
# so SQL generation and dbGetQueryDT run without live connections or workers.
params_map_test_module <- function() {
  module <- new.env(parent = asNamespace("YGwater"))
  for (package in c("shiny", "bslib")) {
    for (name in getNamespaceExports(package)) {
      module[[name]] <- getExportedValue(package, name)
    }
  }
  sys.source(
    system.file("apps/aqualink/modules/client/map/paramsMap.R", package = "YGwater"),
    module
  )
  module$config <- list(public = FALSE)
  module$application_notifications_ui <- function(...) NULL
  module$cached <- list(
    locations = data.table::data.table(
      location_id = 1:4, name = paste0("Site ", 1:4),
      name_fr = paste0("Lieu ", 1:4), latitude = 60:63, longitude = -135
    ),
    timeseries = data.table::data.table(
      timeseries_id = 1:4, location_id = 1:4,
      parameter_id = c(41L, 41L, 41L, 42L),
      units = c("m3/s", "m3/s", "m3/s", "m"), matrix_state = "Liquid"
    ),
    parameters = data.table::data.table(
      parameter_id = rep(c(41L, 42L), each = 2),
      param_name = rep(c("water flow", "water level"), each = 2),
      param_name_fr = rep(c("debit", "niveau"), each = 2),
      group_id = rep(c(4L, 8L), 2)
    )
  )
  module$map_params_module_data <- function(...) module$cached
  module$measurements <- data.table::data.table(
    timeseries_id = 1:4, date = as.Date("2026-10-01"),
    datetime = as.POSIXct("2026-10-01 12:00:00", tz = "UTC"),
    value = 5, max = 10, min = 0, doy_count = 20L, percent_historic_range = 50
  )
  module$queries <- character()
  module$legends <- list()
  module$modal <- ""
  module$showModal <- function(ui, ...) module$modal <- as.character(ui)
  local_mocked_bindings(
    dbGetQuery = function(conn, statement, ...) {
      if (grepl("information_schema.columns", statement, fixed = TRUE)) {
        columns <- c("max", "min", "doy_count", "percent_historic_range")
        return(data.frame(column_name = c(columns, paste0(columns, "_30yr"))))
      }
      if (grepl("FROM public.parameters", statement, fixed = TRUE)) {
        if (grepl("SELECT parameter_id", statement, fixed = TRUE)) {
          return(data.frame(parameter_id = integer())) # Cached default fallback.
        }
        id <- as.integer(sub(".*parameter_id = ([0-9]+).*", "\\1", statement))
        return(data.frame(param_name = unique(module$cached$parameters$param_name[
          module$cached$parameters$parameter_id == id
        ])))
      }
      module$queries <- c(module$queries, statement)
      ids <- regmatches(
        statement,
        regexec("timeseries_id IN \\(([0-9,]+)\\)|ARRAY\\[([0-9,]+)\\]", statement)
      )[[1L]]
      if (!length(ids)) stop("Unexpected map query: ", statement)
      ids <- as.integer(strsplit(paste0(ids[-1L], collapse = ""), ",")[[1L]])
      # The real module orders rows and adds columns by reference.
      rows <- data.table::copy(module$measurements[
        module$measurements$timeseries_id %in% ids
      ])
      if (grepl("measurements_continuous_corrected(", statement, fixed = TRUE)) {
        return(rows[, c("timeseries_id", "datetime", "value")])
      }
      if (grepl("WITH ranked_data", statement, fixed = TRUE)) {
        return(rows[, c("timeseries_id", "max", "min", "doy_count")])
      }
      rows[, c("timeseries_id", "date", "value", "max", "min", "doy_count",
               "percent_historic_range")]
    },
    .package = "DBI",
    .env = parent.frame()
  )
  add_legend <- leaflet::addLegend
  local_mocked_bindings(
    addLegend = function(map, ...) {
      module$legends[[length(module$legends) + 1L]] <- list(...)
      add_legend(map, ...)
    },
    .package = "leaflet",
    .env = parent.frame()
  )
  module
}

test_that("parameter maps wait for client bounds and preserve bilingual selections", {
  module <- params_map_test_module()
  shiny::testServer(
    module$mapParams,
    args = list(language = shiny::reactiveValues(language = "English", abbrev = "en")),
    {
      session$userData$AquaCache <- DBI::ANSI()
      session$setInputs(mapType = "range")
      expect_length(module$queries, 0L)
      session$setInputs(target = as.Date("2026-10-01"), map_bounds = list(north = 65))
      expect_false(session$isClosed())
      expect_equal(map_params$param1, 41L)
      expect_equal(nrow(module$cached$parameters), 4L) # Older caches repeat IDs.
      expect_equal(popup_markers()$timeseries_id, as.character(1:3))
      expect_match(output$primary_param$html, "water flow", fixed = TRUE)
      expect_false(grepl('id="proxy1-latest"', output$sidebar_controls$html, fixed = TRUE))
      session$setInputs(edit_primary_param = 1)
      expect_match(module$modal, 'id="proxy1-yrs"', fixed = TRUE)
      session$setInputs(param = "41", yrs = 12, days = 2, save_primary_param = 1)
      expect_equal(map_params$yrs1, 12)
      expect_equal(map_params$days1, 2)
      expect_match(output$data_availability_disclaimer$html, "12 years", fixed = TRUE)
      session$setInputs(edit_secondary_param = 1)
      session$setInputs(param = "42", save_secondary_param = 1)
      expect_equal(map_params$params, 2)
      expect_match(output$secondary_param$html, "water level", fixed = TRUE)
      expect_equal(popup_markers()$timeseries_id, as.character(1:4))
      language$language <- "Fran\u00e7ais"
      language$abbrev <- "fr"
      session$flushReact()
      expect_equal(map_params$param1, "41")
      expect_equal(map_params$param2, "42")
      expect_true(all(grepl("Lieu", popup_markers()$popup, fixed = TRUE)))
      expect_true(all(grepl(tr("value", language$language), popup_markers()$popup, fixed = TRUE)))
      session$setInputs(edit_primary_param = 2)
      expect_match(module$modal, "debit", fixed = TRUE)
      expect_match(module$modal, "niveau", fixed = TRUE)
      session$setInputs(remove_secondary_param = 1)
      expect_null(output$secondary_param)
      expect_equal(popup_markers()$timeseries_id, as.character(1:3))
      expect_false(session$isClosed())
    }
  )
})

test_that("daily queries use selected tolerances and preserve primary coverage", {
  module <- params_map_test_module()
  # Unordered results with several days per series exercise nearest-day selection.
  module$measurements <- data.table::rbindlist(list(
    module$measurements[, date := as.Date("2026-09-30")],
    data.table::copy(module$measurements)[, `:=`(date = as.Date("2026-10-01"), value = 7)]
  ))[8:1]
  shiny::testServer(
    module$mapParams,
    args = list(language = shiny::reactiveValues(language = "English", abbrev = "en")),
    {
      session$userData$AquaCache <- DBI::ANSI()
      session$setInputs(mapType = "range")
      session$setInputs(target = as.Date("2026-10-01"), map_bounds = list(north = 65))
      expect_equal(popup_markers()$timeseries_id, as.character(1:3))
      expect_true(all(grepl(": 7", popup_markers()$popup, fixed = TRUE)))
      session$setInputs(param = "41", yrs = 12, days = 3, save_primary_param = 1)
      sql <- tail(module$queries, 1L)
      expect_match(sql, "doy_count_30yr >= 12", fixed = TRUE)
      expect_match(sql, "timeseries_id IN (1,2,3)", fixed = TRUE)
      expect_match(sql, "date BETWEEN '2026-09-28' AND '2026-10-04'", fixed = TRUE)
      # A selected secondary parameter can legitimately have no series.
      moduleData$timeseries <- moduleData$timeseries[parameter_id == 41L]
      session$setInputs(param = "42", yrs = 10, days = 0, save_secondary_param = 1)
      expect_equal(popup_markers()$timeseries_id, as.character(1:3))
      expect_false(session$isClosed())
      calls <- length(module$queries)
      session$setInputs(param = "41", yrs = 2, days = 366, save_secondary_param = 2)
      expect_equal(map_params$param2, "42")
      expect_equal(map_params$yrs2, 10)
      expect_equal(map_params$days2, 0)
      expect_length(module$queries, calls)
      # Transient invalid primary inputs leave the previous map usable.
      calls <- length(module$queries)
      session$setInputs(param = "not an id", yrs = NA, days = -1, save_primary_param = 2)
      expect_length(module$queries, calls)
      expect_equal(map_params$param1, "41")
      expect_equal(map_params$yrs1, 12)
      expect_equal(map_params$days1, 3)
      expect_equal(popup_markers()$timeseries_id, as.character(1:3))
      expect_false(session$isClosed())
    }
  )
})

test_that("hourly timelines query corrected values up to the selected hour", {
  module <- params_map_test_module()
  shiny::testServer(
    module$mapParams,
    args = list(language = shiny::reactiveValues(language = "English", abbrev = "en")),
    {
      session$userData$AquaCache <- DBI::ANSI()
      session$setInputs(mapType = "range")
      session$setInputs(target = as.Date("2026-10-01"), map_bounds = list(north = 65))
      session$setInputs(timeline_days = "2")
      target <- as.POSIXct("2026-10-01 12:00:00", tz = "UTC")
      session$setInputs(map_date_slider = target)
      expect_equal(map_params$target_time, target)
      expect_equal(map_params$target, as.Date(target, tz = Sys.timezone()))
      expect_s3_class(timeline_window(), "POSIXct")
      sql <- tail(module$queries, 2L)
      expect_match(sql[[1L]], "continuous.measurements_continuous_corrected(", fixed = TRUE)
      expect_match(sql[[1L]], "2026-09-30 12:00:00", fixed = TRUE)
      expect_match(sql[[1L]], "2026-10-01 12:00:00", fixed = TRUE)
      expect_match(sql[[1L]], "ORDER BY datetime DESC", fixed = TRUE)
      expect_match(sql[[2L]], "doy_count_30yr >= 10", fixed = TRUE)
      expect_equal(popup_markers()$timeseries_id, as.character(1:3))
      # A stale daily slider echo must not overwrite an hourly target.
      session$setInputs(map_date_slider = as.Date("2026-09-30"))
      expect_equal(map_params$target_time, target)
      session$setInputs(timeline_days = "7")
      expect_s3_class(timeline_window(), "Date")
      expect_match(tail(module$queries, 1L), "date BETWEEN", fixed = TRUE)
      expect_false(session$isClosed())
    }
  )
})

test_that("latest refreshes retain selections and stop following when a date is picked", {
  module <- params_map_test_module()
  shiny::testServer(
    module$mapParams,
    args = list(language = shiny::reactiveValues(language = "English", abbrev = "en")),
    {
      session$userData$AquaCache <- DBI::ANSI()
      session$setInputs(mapType = "range")
      session$setInputs(map_bounds = list(north = 65), latest = TRUE, auto_update_latest = TRUE)
      expect_true(map_params$latest)
      expect_equal(map_params$target, as.Date(module$measurements$datetime[[1L]], tz = Sys.timezone()))
      expect_equal(popup_markers()$timeseries_id, as.character(1:3))
      calls <- length(module$queries)
      session$elapse(15 * 60 * 1000)
      expect_gt(length(module$queries), calls)
      expect_equal(map_params$param1, 41L)
      expect_equal(map_params$days1, 1)
      expect_match(tail(module$queries, 2L)[[1L]], "NULL::timestamptz", fixed = TRUE)
      session$setInputs(target = as.Date("2026-09-30"))
      expect_false(map_params$latest)
      expect_equal(map_params$target, as.Date("2026-09-30"))
      calls <- length(module$queries)
      session$elapse(15 * 60 * 1000)
      expect_length(module$queries, calls)
      expect_false(session$isClosed())
    }
  )
})

test_that("map metadata caches separate database instances and users", {
  module <- new.env(parent = asNamespace("YGwater"))
  sys.source(
    system.file("apps/aqualink/modules/cache_functions.R", package = "YGwater"), module
  )
  keys <- character()
  module$get_cached <- function(key, ...) {
    keys <<- c(keys, key)
    list()
  }
  local_mocked_bindings(dbGetInfo = function(dbObj, ...) dbObj, .package = "DBI")
  info <- list(host = "test-host", port = 5432, dbname = "first_db", user = "first_user")
  module$map_params_module_data(info)
  info$dbname <- "second_db"
  module$map_params_module_data(info)
  info$user <- "second_user"
  module$map_params_module_data(info)
  expect_length(unique(keys), 3L)
})

test_that("historical statistics use available period columns and matching count filters", {
  module <- params_map_test_module()
  columns <- c("max", "min", "doy_count", "percent_historic_range")
  available <- c(columns, paste0(columns, "_30yr"))
  local_mocked_bindings(
    dbGetQuery = function(...) data.frame(column_name = available), .package = "DBI"
  )
  sql <- module$params_map_daily_stats_select_sql(DBI::ANSI())
  expect_identical(attr(sql, "count_column"), "doy_count_30yr")
  expect_equal(unname(sql[["max"]]), 'd."max_30yr" AS "max"')
  expect_equal(unname(sql[["doy_count"]]), 'd."doy_count_30yr" AS "doy_count"')
  full <- module$params_map_daily_stats_select_sql(DBI::ANSI(), stats_period = "full")
  expect_identical(attr(full, "count_column"), "doy_count")
  expect_equal(unname(full[["max"]]), 'd."max" AS "max"')
  # A partial migration must fall back consistently for every statistic.
  available <- c(columns, "max_30yr")
  sql <- module$params_map_daily_stats_select_sql(DBI::ANSI(), table_alias = "daily")
  expect_identical(attr(sql, "count_column"), "doy_count")
  expect_equal(unname(sql[["max"]]), 'daily."max" AS "max"')
  expect_equal(unname(sql[["min"]]), 'daily."min" AS "min"')
})

test_that("map legends handle negative constants, flat history and mixed units", {
  module <- params_map_test_module()
  module$cached$locations$name[1L] <- "<script>test</script>"
  module$cached$timeseries$units[1L] <- "different unit"
  module$measurements[, `:=`(value = -5, min = -5, max = -5, percent_historic_range = NA_real_)]
  shiny::testServer(
    module$mapParams,
    args = list(language = shiny::reactiveValues(language = "English", abbrev = "en")),
    {
      session$userData$AquaCache <- DBI::ANSI()
      session$setInputs(mapType = "range")
      session$setInputs(map_bounds = list(north = 65))
      expect_false(session$isClosed())
      expect_true(any(grepl("&lt;script&gt;test&lt;/script&gt;", popup_markers()$popup, fixed = TRUE)))
      expect_true(all(is.na(tail(module$legends, 1L)[[1L]]$values)))
      session$setInputs(mapType = "actual")
      legend <- tail(module$legends, 1L)[[1L]]
      expect_match(legend$title, "different unit", fixed = TRUE)
      expect_match(legend$title, "m3/s", fixed = TRUE)
      expect_equal(legend$values, rep(-5, 3))
      expect_true(all(is.finite(slider_echo[["colour_limits"]])))
      expect_gt(diff(slider_echo[["colour_limits"]]), 0)
      expect_false(session$isClosed())
    }
  )
})

test_that("station selection prefers usable values, aggregation, record length and recency", {
  module <- params_map_test_module()
  series <- data.table::fread(text = "timeseries_id location_id aggregation_type_id start end value
101 1 6 2000 2026 1
102 1 2 2020 2026 2
201 2 1 2000 2026 NA
202 2 2 2020 2026 3
301 3 2 2000 2026 4
302 3 2 2020 2026 5
401 4 2 2000 2020 6
402 4 2 2006 2026 7
403 4 2 2006 2026 8")
  module$cached$timeseries <- series[, .(
    timeseries_id, location_id, parameter_id = 41L, aggregation_type_id,
    start_datetime = as.POSIXct(paste0(start, "-01-01"), tz = "UTC"),
    end_datetime = as.POSIXct(paste0(end, "-01-01"), tz = "UTC"),
    units = "m3/s", matrix_state = "Liquid"
  )]
  module$measurements <- series[, .(
    timeseries_id, value, date = as.Date("2026-10-01"),
    datetime = as.POSIXct("2026-10-01 12:00:00", tz = "UTC"),
    max = 10, min = 0, doy_count = 20L, percent_historic_range = value * 10
  )]
  shiny::testServer(
    module$mapParams,
    args = list(language = shiny::reactiveValues(language = "English", abbrev = "en")),
    {
      session$userData$AquaCache <- DBI::ANSI()
      session$setInputs(mapType = "range")
      session$setInputs(map_bounds = list(north = 65))
      expect_equal(popup_markers()$timeseries_id, c("102", "202", "301", "402"))
      module$measurements <- module$measurements[.N:1]
      updateMap()
      expect_equal(popup_markers()$timeseries_id, c("102", "202", "301", "402"))
      session$setInputs(mapType = "actual")
      expect_equal(popup_markers()$timeseries_id, c("102", "202", "301", "402"))
      expect_false(session$isClosed())
    }
  )
})

test_that("absolute maps retain historical criteria and parameter selections", {
  module <- params_map_test_module()
  shiny::testServer(
    module$mapParams,
    args = list(language = shiny::reactiveValues(language = "English", abbrev = "en")),
    {
      session$userData$AquaCache <- DBI::ANSI()
      session$setInputs(mapType = "range")
      session$setInputs(target = as.Date("2026-10-01"), map_bounds = list(north = 65))
      session$setInputs(param = "41", yrs = 12, days = 2, save_primary_param = 1)
      session$setInputs(param = "42", yrs = 10, days = 1, save_secondary_param = 1)
      session$setInputs(mapType = "actual")
      session$setInputs(edit_primary_param = 1)
      expect_null(output$secondary_param)
      expect_match(module$modal, 'id="proxy1-yrs"', fixed = TRUE)
      expect_match(output$data_availability_disclaimer$html, "12 years", fixed = TRUE)
      expect_match(tail(module$queries, 2L)[[1L]], "doy_count_30yr >= 12", fixed = TRUE)
      expect_equal(map_params$yrs1, 12)
      expect_equal(map_params$days1, 2)
      expect_equal(popup_markers()$timeseries_id, as.character(1:4))
      legends <- tail(module$legends, 2L)
      expect_match(legends[[1L]]$title, "m3/s", fixed = TRUE)
      expect_match(legends[[2L]]$title, "water level (m)", fixed = TRUE)
      expect_equal(legends[[1L]]$values, rep(5, 3))
      expect_equal(legends[[2L]]$values, 5)
      # Absolute queries still need valid history criteria in the current map.
      session$setInputs(param = "41", yrs = NULL, days = 3, save_primary_param = 2)
      expect_equal(map_params$yrs1, 12)
      expect_equal(map_params$days1, 2)
      session$setInputs(mapType = "range")
      expect_match(output$secondary_param$html, "water level", fixed = TRUE)
      expect_equal(map_params$param1, "41")
      expect_equal(map_params$param2, "42")
      expect_equal(map_params$yrs1, 12)
      expect_equal(map_params$days1, 2)
      expect_equal(tail(module$legends, 2L)[[1L]]$values, rep(50, 3))
      expect_false(session$isClosed())
    }
  )
})
