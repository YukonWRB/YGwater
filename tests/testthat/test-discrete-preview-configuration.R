discrete_preview_environment <- function() {
  env <- new.env(parent = asNamespace("YGwater"))
  sys.source(system.file("apps/aqualink/modules/admin/discreteData/addDiscData.R",
    package = "YGwater"), env)
  env
}

discrete_preview_profile <- function(env, timezone = "UTC") {
  profile <- env$addDiscData_empty_profiles()[NA_integer_, , drop = FALSE]
  profile$import_source_id <- 1L
  profile$import_profile_id <- 2L
  profile$source_code <- "LAB"
  profile$profile_code <- "workbook"
  profile$timezone <- timezone
  profile$active <- TRUE
  profile$column_map <- I(list(list(sample_date = "Date", result = "Value")))
  profile$wide_config <- I(list(list(first_result_column = 3L)))
  profile$defaults <- I(list(list(owner = 1L, media_id = 1L)))
  profile$sample_identity <- I(list(c("datetime", "location_id")))
  profile$result_identity <- I(list("parameter_id"))
  profile$validation_rules <- I(list(list(result_text_values = list(Present = 1))))
  profile
}

discrete_preview_fixture <- function() {
  env <- discrete_preview_environment()
  database <- new.env()
  database$snapshot <- list(
    profile = lapply(discrete_preview_profile(env, "America/Whitehorse"), `[[`, 1L),
    upload_options = list(negative_values_are_mdl = FALSE, negative_mdl_exclusions = character())
  )
  env$addDiscData_mapping_snapshot <- function(con, profile, lock = FALSE, options = list()) {
    snapshot <- database$snapshot
    snapshot$upload_options <- options
    snapshot
  }
  env$addDiscData_parse_upload <- function(path, profile, ...) {
    rows <- env$addDiscData_empty_table()[NA_integer_, , drop = FALSE]
    rows$sample_key <- "one"
    rows$source_code <- "LAB"
    rows$result <- 42
    rows$datetime <- env$addDiscData_datetime("2026-01-01", "12:00", tz = profile$timezone[[1]])
    rows
  }
  env$addDiscData_location_match <- function(rows, ...) rows
  env$addDiscData_apply_blank_sample_types <- function(rows, ...) rows
  env$addDiscData_apply_mappings <- function(rows, ...) rows
  env$addDiscData_negative_mdl_choices <- function(...) data.frame()
  list(env = env, database = database)
}

discrete_preview_server_state <- function(env, path) {
  state <- new.env(parent = env)
  state$data <- new.env()
  state$data$raw_file_path <- path
  state$data$negative_mdl_exclusions <- character()
  state$data$df <- env$addDiscData_empty_table()
  state$data$preview_base <- env$addDiscData_empty_table()
  state$data$preview_profile_key <- env$addDiscData_profile_key("LAB", "workbook")
  state$data$preview_mapping_snapshot <- "earlier preview"
  state$input <- list(file = list(datapath = path), negative_values_are_mdl = FALSE)
  state$req <- shiny::req
  state$con <- NULL
  state$locations <- function() data.frame()
  state$sample_types <- data.frame()
  state$mapping_revision <- local({
    value <- 0L
    function(new_value) {
      if (!missing(new_value)) value <<- new_value
      value
    }
  })
  state$showNotification <- function(ui, type, ...) {
    state$notification <- list(text = as.character(ui), type = type)
  }
  statements <- as.list(body(env$addDiscData)[[2]][[3]][[3]])[-1]
  for (name in c("refresh_current_preview", "apply_mapping_save_result")) {
    assignment <- Filter(function(x) is.call(x) && identical(x[[1]], quote(`<-`)) &&
      identical(x[[2]], as.name(name)), statements)[[1]]
    eval(assignment, state)
  }
  state
}

test_that("preview configurations reconstruct the current parser and preserve JSON fields", {
  fixture <- discrete_preview_fixture()
  env <- fixture$env
  cached <- discrete_preview_profile(env)
  options <- fixture$database$snapshot$upload_options

  configuration <- env$addDiscData_preview_configuration(NULL, cached, options)

  expect_identical(configuration$profile$timezone, "America/Whitehorse")
  expect_identical(cached$timezone, "UTC")
  expect_identical(configuration$snapshot, fixture$database$snapshot)
  for (field in c("column_map", "wide_config", "defaults", "sample_identity",
    "result_identity", "validation_rules")) {
    expect_true(is.list(configuration$profile[[field]]))
    expect_identical(configuration$profile[[field]][[1]], configuration$snapshot$profile[[field]])
  }
})

test_that("refreshes parse with the captured current profile and reject old worker snapshots", {
  fixture <- discrete_preview_fixture()
  env <- fixture$env
  path <- tempfile()
  writeLines("preview fixture", path)
  on.exit(unlink(path))
  state <- discrete_preview_server_state(env, path)
  local_mocked_bindings(getImportLocationMappings = function(...) data.frame(), .package = "AquaCache")
  cached <- discrete_preview_profile(env)

  expect_true(state$refresh_current_preview(cached, preserve_edits = FALSE))
  expect_equal(state$data$df$datetime, as.POSIXct("2026-01-01 19:00:00", tz = "UTC"))
  expect_identical(state$data$preview_mapping_snapshot, fixture$database$snapshot)
  before <- state$data$df
  old_snapshot <- fixture$database$snapshot
  old_snapshot$profile$timezone <- "UTC"
  old_rows <- env$addDiscData_parse_upload(path, cached)

  expect_error(state$refresh_current_preview(cached, parsed = old_rows,
    snapshot = old_snapshot), "changed while parsing")
  expect_identical(state$data$df, before)
  expect_identical(state$data$preview_mapping_snapshot, fixture$database$snapshot)
  expect_true(state$data$preview_is_stale)
  expect_error(state$refresh_current_preview(cached, parsed = old_rows), "changed while parsing")
})

test_that("the normal parse task carries its captured configuration through the result handler", {
  fixture <- discrete_preview_fixture()
  env <- fixture$env
  path <- tempfile()
  writeLines("preview fixture", path)
  on.exit(unlink(path))
  state <- discrete_preview_server_state(env, path)
  cached <- discrete_preview_profile(env)
  state$selected_profile <- function() cached
  request <- NULL
  worker_result <- NULL
  state$preview_parse_task <- list(
    invoke = function(value) request <<- value,
    result = function() worker_result
  )
  statements <- as.list(body(env$addDiscData)[[2]][[3]][[3]])[-1]
  handlers <- Filter(function(x) is.call(x) && identical(x[[1]], quote(observeEvent)), statements)
  dispatch <- Filter(function(x) identical(x[[2]], quote(input$preview_file)), handlers)[[1]][[3]]
  receive <- Filter(function(x) identical(x[[2]], quote(preview_parse_task$result())), handlers)[[1]][[3]]
  local_mocked_bindings(getImportLocationMappings = function(...) data.frame(), .package = "AquaCache")

  eval(dispatch, state)

  expect_identical(request$profile$timezone, "America/Whitehorse")
  expect_identical(request$mapping_snapshot, fixture$database$snapshot)
  worker_result <- list(ok = TRUE, request = request,
    parsed = env$addDiscData_parse_upload(path, request$profile))
  eval(receive, state)
  expect_equal(state$data$df$datetime, as.POSIXct("2026-01-01 19:00:00", tz = "UTC"))
  expect_identical(state$data$preview_mapping_snapshot, request$mapping_snapshot)
  expect_false(state$data$preview_is_stale)
  # A later result from the same file and profile key must not acquire a
  # replacement snapshot if the parser changed while its worker was running.
  before <- state$data$df
  fixture$database$snapshot$profile$timezone <- "UTC"
  eval(receive, state)
  expect_true(state$data$preview_is_stale)
  expect_identical(state$data$df, before)
  expect_identical(state$data$preview_mapping_snapshot, request$mapping_snapshot)
  expect_identical(state$notification$type, "error")
})

test_that("a configuration change during preview preparation leaves the previous rows intact", {
  fixture <- discrete_preview_fixture()
  env <- fixture$env
  path <- tempfile()
  writeLines("preview fixture", path)
  on.exit(unlink(path))
  state <- discrete_preview_server_state(env, path)
  local_mocked_bindings(getImportLocationMappings = function(...) data.frame(), .package = "AquaCache")
  env$addDiscData_apply_mappings <- function(rows, ...) {
    fixture$database$snapshot$profile$timezone <- "UTC"
    rows
  }

  expect_error(state$refresh_current_preview(discrete_preview_profile(env)), "changed while preparing")
  expect_equal(nrow(state$data$df), 0L)
  expect_identical(state$data$preview_mapping_snapshot, "earlier preview")
  expect_true(state$data$preview_is_stale)
})

test_that("mapping-save refreshes carry the snapshot used by the worker", {
  fixture <- discrete_preview_fixture()
  env <- fixture$env
  request <- list(config = list(), kind = "location", profile = discrete_preview_profile(env),
    row = data.frame(source_location_name = "Site"), location_id = 1L,
    sub_location_id = NA_integer_, refresh_preview = TRUE, path = "fixture",
    negative_values_are_mdl = FALSE, negative_mdl_exclusions = character(),
    locations = data.frame(), sample_types = data.frame())
  local_mocked_bindings(AquaConnect = function(...) "mock connection", .package = "YGwater")
  local_mocked_bindings(dbWithTransaction = function(conn, code, ...) force(code),
    dbDisconnect = function(...) TRUE, .package = "DBI")
  local_mocked_bindings(upsertImportLocationMappings = function(...) NULL,
    getImportLocationMappings = function(...) data.frame(), .package = "AquaCache")

  result <- env$addDiscData_run_mapping_save(request)

  expect_true(result$ok)
  expect_true(result$saved)
  expect_identical(result$mapping_snapshot, fixture$database$snapshot)
  expect_equal(result$parsed$datetime, as.POSIXct("2026-01-01 19:00:00", tz = "UTC"))
  parse <- env$addDiscData_parse_upload
  env$addDiscData_parse_upload <- function(...) {
    rows <- parse(...)
    fixture$database$snapshot$profile$timezone <- "UTC"
    rows
  }
  changed <- env$addDiscData_run_mapping_save(request)
  expect_false(changed$ok)
  expect_true(changed$saved)
  expect_match(changed$message, "changed while preparing")
})

test_that("late mapping-save results are rejected instead of receiving a new snapshot", {
  fixture <- discrete_preview_fixture()
  env <- fixture$env
  path <- tempfile()
  writeLines("preview fixture", path)
  on.exit(unlink(path))
  state <- discrete_preview_server_state(env, path)
  request <- list(kind = "location", path = path, profile = discrete_preview_profile(env),
    profile_key = state$data$preview_profile_key, refresh_preview = TRUE)
  snapshot <- fixture$database$snapshot
  parsed <- env$addDiscData_parse_upload(path, env$addDiscData_preview_configuration(
    NULL, request$profile, snapshot$upload_options)$profile)
  result <- list(ok = TRUE, saved = TRUE, request = request,
    parsed = parsed, mapping_snapshot = snapshot)
  fixture$database$snapshot$profile$timezone <- "UTC"

  state$apply_mapping_save_result(result)

  expect_equal(nrow(state$data$df), 0L)
  expect_identical(state$data$preview_mapping_snapshot, "earlier preview")
  expect_true(state$data$preview_is_stale)
  expect_identical(state$notification$type, "warning")
  fixture$database$snapshot <- snapshot
  state$apply_mapping_save_result(result)
  expect_identical(state$data$df, parsed)
  expect_identical(state$data$preview_mapping_snapshot, snapshot)
  expect_false(state$data$preview_is_stale)
  # Changed upload options must also invalidate a worker's old result.
  state$input$negative_values_are_mdl <- TRUE
  state$apply_mapping_save_result(result)
  expect_true(state$data$preview_is_stale)
  expect_identical(state$data$preview_mapping_snapshot, snapshot)
})
