timeseries_transmission_test_environment <- function() {
  env <- new.env(parent = globalenv())
  sys.source(
    system.file(
      "apps/YGwater/modules/admin/continuousData/transmissionTimeseriesHelpers.R",
      package = "YGwater"
    ),
    envir = env
  )
  env
}

test_that("transmission mapping input is normalized for database storage", {
  env <- timeseries_transmission_test_environment()

  mapping <- env$timeseries_normalize_transmission_mapping(
    route_id = "12",
    source_field = "  WaterLevel  ",
    value_multiplier = "0.01",
    value_offset = "-2",
    missing_values = "-9999, MISSING, -9999",
    mapping_config = '{"quality_field":"Quality"}'
  )

  expect_equal(mapping$transmission_route_id, 12L)
  expect_equal(mapping$source_field, "WaterLevel")
  expect_equal(mapping$value_multiplier, 0.01)
  expect_equal(mapping$value_offset, -2)
  expect_equal(
    jsonlite::fromJSON(mapping$missing_values),
    c("-9999", "MISSING")
  )
  expect_equal(
    jsonlite::fromJSON(mapping$mapping_config)$quality_field,
    "Quality"
  )

  expect_error(
    env$timeseries_normalize_transmission_mapping(
      route_id = 12,
      source_field = "",
      value_multiplier = 1
    ),
    "payload field"
  )
  expect_error(
    env$timeseries_normalize_transmission_mapping(
      route_id = 12,
      source_field = "value",
      value_multiplier = 0
    ),
    "non-zero"
  )
  expect_error(
    env$timeseries_normalize_transmission_mapping(
      route_id = 12,
      source_field = "value",
      mapping_config = "[]"
    ),
    "JSON object"
  )
})

test_that("source adapter lookup works with data.table registries", {
  env <- timeseries_transmission_test_environment()
  capabilities <- data.table::data.table(
    source_fx = c("downloadA", "downloadB"),
    requires_transmission_mapping = c(FALSE, TRUE)
  )

  result <- env$timeseries_source_adapter_capability(
    capabilities,
    "downloadB"
  )
  expect_equal(nrow(result), 1L)
  expect_equal(result$source_fx, "downloadB")
  expect_true(result$requires_transmission_mapping)
  expect_null(
    env$timeseries_source_adapter_capability(
      capabilities,
      "downloadMissing"
    )
  )
})

test_that("catalogued route fields are filtered and merged into route JSON", {
  env <- timeseries_transmission_test_environment()
  capability <- data.frame(source_fx = "downloadNESDIS")
  capability$ui_config <- I(list(list(route_config_fields = list(
    list(
      name = "timestamp_floor_seconds",
      path = list("parser_config", "timestamp_floor_seconds"),
      label = "Observation-time alignment interval, seconds (optional)",
      help = "Use only when the payload has no observation timestamp.",
      value_type = "integer",
      control = "numeric",
      minimum = 1,
      message_formats = list("BLM")
    )
  ))))

  expect_length(
    env$timeseries_route_config_fields(capability, "BLM"),
    1L
  )
  expect_length(
    env$timeseries_route_config_fields(capability, "SHEF"),
    0L
  )

  fields <- env$timeseries_route_config_fields(capability)
  config <- env$timeseries_route_config_with_ui_fields(
    '{"max_days":14,"parser_config":{"fields":["ta","rh"]}}',
    fields,
    list(timestamp_floor_seconds = 3600)
  )
  parsed <- jsonlite::fromJSON(config)
  expect_equal(parsed$max_days, 14L)
  expect_equal(parsed$parser_config$fields, c("ta", "rh"))
  expect_equal(parsed$parser_config$timestamp_floor_seconds, 3600L)
  expect_equal(
    env$timeseries_route_config_field_value(
      config,
      c("parser_config", "timestamp_floor_seconds")
    ),
    3600L
  )

  cleared <- env$timeseries_route_config_with_ui_fields(
    config,
    fields,
    list(timestamp_floor_seconds = NA)
  )
  expect_null(jsonlite::fromJSON(cleared)$parser_config$timestamp_floor_seconds)
  expect_error(
    env$timeseries_route_config_with_ui_fields(
      "{}",
      fields,
      list(timestamp_floor_seconds = 0)
    ),
    "must be at least 1"
  )
})

test_that("transmission routes are stored with their source assignments", {
  env <- timeseries_transmission_test_environment()

  source_args <- env$timeseries_source_args_with_transmission_route(
    '{"cache":true}',
    14
  )
  parsed <- jsonlite::fromJSON(source_args)

  expect_true(parsed$cache)
  expect_equal(parsed$transmission_route_id, 14L)
  expect_error(
    env$timeseries_source_args_with_transmission_route("{}", NA),
    "valid transmission route"
  )
})

test_that("timeseries mapping synchronization retains distinct routes", {
  env <- timeseries_transmission_test_environment()
  statements <- character()
  parameters <- list()

  local_mocked_bindings(
    dbExecute = function(con, statement, params = NULL, ...) {
      statements <<- c(statements, statement)
      parameters[[length(parameters) + 1L]] <<- params
      1L
    },
    .package = "DBI"
  )

  primary_mapping <- env$timeseries_normalize_transmission_mapping(
    route_id = 7,
    source_field = "Stage",
    missing_values = "-9999"
  )
  secondary_mapping <- env$timeseries_normalize_transmission_mapping(
    route_id = 8,
    source_field = "StageBackup"
  )
  env$timeseries_sync_transmission_mapping(
    con = structure(list(), class = "mock_con"),
    timeseries_id = 101,
    mapping = list(primary_mapping, secondary_mapping)
  )

  expect_length(statements, 3L)
  expect_match(statements[[1]], "WHERE timeseries_id = $1", fixed = TRUE)
  expect_equal(parameters[[1]], list(101L))
  expect_equal(parameters[[2]][1:3], list(7L, "Stage", 101L))
  expect_equal(parameters[[3]][1:3], list(8L, "StageBackup", 101L))

  expect_error(
    env$timeseries_sync_transmission_mapping(
      con = structure(list(), class = "mock_con"),
      timeseries_id = 101,
      mapping = list(primary_mapping, primary_mapping)
    ),
    "cannot repeat"
  )
})

test_that("route creation can reuse an existing transmission setup", {
  env <- timeseries_transmission_test_environment()
  committed <- FALSE
  rolled_back <- FALSE

  local_mocked_bindings(
    dbBegin = function(con, ...) invisible(TRUE),
    dbCommit = function(con, ...) {
      committed <<- TRUE
      invisible(TRUE)
    },
    dbRollback = function(con, ...) {
      rolled_back <<- TRUE
      invisible(TRUE)
    },
    dbGetQuery = function(con, statement, params = NULL, ...) {
      expect_match(
        statement,
        "INSERT INTO public.locations_metadata_transmission_routes",
        fixed = TRUE
      )
      expect_equal(params[[1]], 22L)
      data.frame(transmission_route_id = 44L)
    },
    .package = "DBI"
  )

  route_id <- env$timeseries_save_transmission_route(
    con = structure(list(), class = "mock_con"),
    location_id = 3,
    setup_id = 22,
    logger_metadata_id = NA,
    transmission_method_id = NA,
    provider_name = "",
    platform_identifier = "",
    setup_start_datetime = "",
    transmission_config = "{}",
    route_name = "Primary",
    endpoint_identifier = "",
    message_format = "JSON",
    schedule_reference_time_utc = "",
    transmit_interval_seconds = 3600,
    transmit_window_seconds = 30,
    payload_size_bytes = NA,
    route_config = '{"server":"example"}'
  )

  expect_equal(route_id, 44L)
  expect_true(committed)
  expect_false(rolled_back)
})

test_that("route creation can create a setup without a deployed logger", {
  env <- timeseries_transmission_test_environment()
  statements <- character()
  parameters <- list()
  committed <- FALSE

  local_mocked_bindings(
    dbBegin = function(con, ...) invisible(TRUE),
    dbCommit = function(con, ...) {
      committed <<- TRUE
      invisible(TRUE)
    },
    dbRollback = function(con, ...) invisible(TRUE),
    dbGetQuery = function(con, statement, params = NULL, ...) {
      statements <<- c(statements, statement)
      parameters[[length(parameters) + 1L]] <<- params
      if (grepl("transmission_setups", statement, fixed = TRUE)) {
        return(data.frame(transmission_setup_id = 33L))
      }
      data.frame(transmission_route_id = 45L)
    },
    .package = "DBI"
  )

  route_id <- env$timeseries_save_transmission_route(
    con = structure(list(), class = "mock_con"),
    location_id = 3,
    setup_id = NA,
    logger_metadata_id = NA,
    transmission_method_id = 5,
    provider_name = "NESDIS",
    platform_identifier = "CE123456",
    setup_start_datetime = "2026-01-02 03:04:05",
    transmission_config = '{"decoder":"json"}',
    route_name = "Primary",
    endpoint_identifier = "LRGS",
    message_format = "JSON",
    schedule_reference_time_utc = "00:00:00",
    transmit_interval_seconds = 3600,
    transmit_window_seconds = 30,
    payload_size_bytes = 1024,
    route_config = '{"server":"example"}'
  )

  expect_equal(route_id, 45L)
  expect_true(committed)
  expect_length(statements, 2L)
  expect_match(statements[[1]], "transmission_setups", fixed = TRUE)
  expect_match(statements[[2]], "transmission_routes", fixed = TRUE)
  expect_equal(
    parameters[[1]][1:5],
    list(3L, NA_integer_, 5L, "NESDIS", "CE123456")
  )
  expect_equal(parameters[[2]][[1]], 33L)
})

test_that("route editing updates only a route at the selected location", {
  env <- timeseries_transmission_test_environment()
  statement <- NULL
  parameters <- NULL
  committed <- FALSE

  local_mocked_bindings(
    dbBegin = function(con, ...) invisible(TRUE),
    dbCommit = function(con, ...) {
      committed <<- TRUE
      invisible(TRUE)
    },
    dbRollback = function(con, ...) invisible(TRUE),
    dbGetQuery = function(con, sql, params = NULL, ...) {
      statement <<- sql
      parameters <<- params
      data.frame(transmission_route_id = 44L)
    },
    .package = "DBI"
  )

  route_id <- env$timeseries_save_transmission_route(
    con = structure(list(), class = "mock_con"),
    location_id = 3,
    setup_id = NA,
    logger_metadata_id = NA,
    transmission_method_id = NA,
    provider_name = "",
    platform_identifier = "",
    setup_start_datetime = "",
    transmission_config = "{}",
    route_name = "Hourly BLM",
    endpoint_identifier = "",
    message_format = "BLM",
    schedule_reference_time_utc = "00:09:30",
    transmit_interval_seconds = 3600,
    transmit_window_seconds = NA,
    payload_size_bytes = NA,
    route_config = '{"parser_config":{"timestamp_floor_seconds":3600}}',
    route_id = 44
  )

  expect_equal(route_id, 44L)
  expect_true(committed)
  expect_match(statement, "UPDATE public.locations_metadata_transmission_routes", fixed = TRUE)
  expect_match(statement, "s.location_id = $2", fixed = TRUE)
  expect_equal(parameters[1:3], list(44L, 3L, "Hourly BLM"))
  expect_equal(
    jsonlite::fromJSON(parameters[[10]])$parser_config$timestamp_floor_seconds,
    3600L
  )
})

test_that("addTimeseries uses registry-driven transmission UI and persistence", {
  module_text <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/admin/continuousData/addTimeseries.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  globals_text <- paste(
    readLines(
      system.file(
        "apps/YGwater/YGwater_globals.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    module_text,
    "AquaCache::getSourceAdapterCapabilities",
    fixed = TRUE
  )
  expect_match(
    module_text,
    'data_domain = "continuous"',
    fixed = TRUE
  )
  expect_match(
    module_text,
    "moduleData$source_fx <- sort",
    fixed = TRUE
  )
  expect_false(grepl(
    'ls(getNamespace("AquaCache"))',
    module_text,
    fixed = TRUE
  ))
  expect_match(
    module_text,
    "Missing download function? Download functions must be",
    fixed = TRUE
  )
  expect_match(
    module_text,
    "public.source_adapter_capabilities",
    fixed = TRUE
  )
  expect_match(
    module_text,
    "output$transmission_mapping_ui <- renderUI",
    fixed = TRUE
  )
  expect_match(
    module_text,
    "output$secondary_transmission_mapping_ui <- renderUI",
    fixed = TRUE
  )
  expect_match(
    module_text,
    'ns("source_secondary_enabled")',
    fixed = TRUE
  )
  expect_match(
    module_text,
    'condition = "input.source_secondary_enabled"',
    fixed = TRUE
  )
  expect_match(
    module_text,
    "timeseries_source_args_with_transmission_route",
    fixed = TRUE
  )
  primary_mapping_position <- regexpr(
    'uiOutput(ns("transmission_mapping_ui"))',
    module_text,
    fixed = TRUE
  )[[1L]]
  secondary_toggle_position <- regexpr(
    'ns("source_secondary_enabled")',
    module_text,
    fixed = TRUE
  )[[1L]]
  secondary_mapping_position <- regexpr(
    'uiOutput(ns("secondary_transmission_mapping_ui"))',
    module_text,
    fixed = TRUE
  )[[1L]]
  expect_gt(primary_mapping_position, 0L)
  expect_lt(primary_mapping_position, secondary_toggle_position)
  expect_lt(secondary_toggle_position, secondary_mapping_position)
  expect_match(
    module_text,
    "rows[[2L]] <- if (isTRUE(input$source_secondary_enabled))",
    fixed = TRUE
  )
  expect_match(
    module_text,
    "timeseries_sync_transmission_mapping",
    fixed = TRUE
  )
  expect_match(
    module_text,
    "Create transmission route",
    fixed = TRUE
  )
  expect_match(
    module_text,
    "Edit selected route",
    fixed = TRUE
  )
  expect_match(
    module_text,
    'uiOutput(ns("new_route_config_fields"))',
    fixed = TRUE
  )
  expect_match(
    module_text,
    "Observation timing settings belong to the transmission route",
    fixed = TRUE
  )
  expect_match(
    module_text,
    "Deployed logger (optional)",
    fixed = TRUE
  )
  expect_match(
    globals_text,
    "transmissionTimeseriesHelpers.R",
    fixed = TRUE
  )
})
