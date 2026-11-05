parameter_unit_migration_test_environment <- function() {
  env <- new.env(parent = asNamespace("YGwater"))
  sys.source(
    system.file(
      "apps/aqualink/modules/admin/metadata/manageReferenceTables.R",
      package = "YGwater"
    ),
    envir = env
  )
  env
}

test_that("parameter unit changes report additions, replacements, and removals", {
  editor <- parameter_unit_migration_test_environment()
  old_units <- data.frame(
    units_liquid = 1L,
    units_solid = NA_integer_,
    units_gas = 3L,
    units_na = NA_integer_
  )
  new_units <- data.frame(
    units_liquid = 2L,
    units_solid = 4L,
    units_gas = NA_integer_,
    units_na = NA_integer_
  )

  changed <- editor$parameter_changed_assigned_unit_fields(old_units, new_units)

  expect_identical(
    changed$matrix_state_code,
    c("liquid", "solid", "gas")
  )
  expect_identical(changed$old_unit_id, c(1L, NA_integer_, 3L))
  expect_identical(changed$new_unit_id, c(2L, 4L, NA_integer_))
  expect_equal(
    nrow(editor$parameter_changed_assigned_unit_fields(old_units, old_units)),
    0L
  )
})

parameter_unit_migration_test_connection <- function() {
  skip_if(
    Sys.getenv("YGWATER_PARAMETER_UNIT_TEST_DB") != "dev",
    "set YGWATER_PARAMETER_UNIT_TEST_DB=dev to run the isolated dev integration test"
  )
  if (!nzchar(Sys.getenv("aquacacheAdminUser"))) {
    renviron_path <- path.expand("~/Documents/.Renviron")
    if (file.exists(renviron_path)) {
      readRenviron(renviron_path)
    }
  }
  skip_if(
    !nzchar(Sys.getenv("aquacacheAdminUser")) ||
      !nzchar(Sys.getenv("aquacacheAdminPass")),
    "AquaCache admin credentials are not available"
  )

  con <- AquaCache::AquaConnect(
    name = Sys.getenv("aquacacheName", "aquacache"),
    host = "10.250.12.154",
    port = Sys.getenv("aquacachePort", "5432"),
    username = Sys.getenv("aquacacheAdminUser"),
    password = Sys.getenv("aquacacheAdminPass"),
    silent = TRUE,
    check = FALSE
  )
  identity <- DBI::dbGetQuery(
    con,
    "SELECT current_database() AS database_name,
       inet_server_addr()::TEXT AS server_address,
       (SELECT version FROM information.version_info
        WHERE item = 'Last patch number') AS patch"
  )
  expect_identical(identity$database_name[[1]], "aquacache")
  expect_identical(identity$server_address[[1]], "10.250.12.154/32")
  expect_identical(identity$patch[[1]], "67")
  con
}

test_that("dev migration preserves extrema timezone and converts continuous records atomically", {
  editor <- parameter_unit_migration_test_environment()
  con <- parameter_unit_migration_test_connection()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbBegin(con)
  on.exit(DBI::dbRollback(con), add = TRUE, after = FALSE)

  units <- DBI::dbGetQuery(
    con,
    "SELECT unit_id FROM public.units ORDER BY unit_id LIMIT 2"
  )
  states <- DBI::dbGetQuery(
    con,
    "SELECT matrix_state_id, matrix_state_code
     FROM public.matrix_states WHERE matrix_state_code IN ('liquid', 'solid')"
  )
  expect_equal(nrow(units), 2L)
  liquid_id <- as.integer(states$matrix_state_id[match("liquid", states$matrix_state_code)])
  solid_id <- as.integer(states$matrix_state_id[match("solid", states$matrix_state_code)])
  expect_false(is.na(liquid_id) || is.na(solid_id))

  organization_id <- as.integer(DBI::dbGetQuery(
    con,
    "SELECT organization_id FROM public.organizations
     ORDER BY organization_id LIMIT 1"
  )$organization_id[[1]])
  parameter_id <- as.integer(DBI::dbGetQuery(
    con,
    "SELECT COALESCE(max(parameter_id), 1000)::BIGINT + 100000 AS id
     FROM public.parameters"
  )$id[[1]])
  timeseries_id <- as.integer(DBI::dbGetQuery(
    con,
    "SELECT COALESCE(max(timeseries_id), 1000)::BIGINT + 100000 AS id
     FROM continuous.timeseries"
  )$id[[1]])
  correction_id <- as.integer(DBI::dbGetQuery(
    con,
    "SELECT COALESCE(max(correction_id), 0)::BIGINT + 100000 AS id
     FROM continuous.corrections"
  )$id[[1]])
  parameter_name <- paste0(
    "__codex_unit_migration_test_",
    format(Sys.time(), "%Y%m%d%H%M%S")
  )

  invisible(DBI::dbExecute(
    con,
    "INSERT INTO public.parameters
       (parameter_id, param_name, sample_fraction,
        plot_default_y_orientation, units_liquid, reporting_basis,
        result_speciation, result_target, endpoint)
     SELECT $1, $2, sample_fraction, plot_default_y_orientation, $3,
       FALSE, FALSE, FALSE, FALSE
     FROM public.parameters ORDER BY parameter_id LIMIT 1",
    params = list(parameter_id, parameter_name, as.integer(units$unit_id[[1]]))
  ))
  invisible(DBI::dbExecute(
    con,
    "INSERT INTO continuous.timeseries
       (timeseries_id, parameter_id, media_id, aggregation_type_id,
        location_id, record_rate, active, share_with, default_owner,
        sensor_priority, sub_location_id, timezone_daily_calc, sync_remote,
        matrix_state_id, timeseries_type, default_show, created_by)
     SELECT $1, $2, ts.media_id, ts.aggregation_type_id, ts.location_id,
       ts.record_rate, TRUE, ARRAY['public_reader']::TEXT[], ts.default_owner,
       ts.sensor_priority, ts.sub_location_id, ts.timezone_daily_calc,
       FALSE, $3, 'basic', TRUE, CURRENT_USER
     FROM continuous.timeseries ts
     WHERE ts.timeseries_type = 'basic'
       AND EXISTS (
         SELECT 1
         FROM public.locations_metadata_transmission_routes route
         JOIN public.locations_metadata_transmission_setups setup
           USING (transmission_setup_id)
         WHERE setup.location_id = ts.location_id
       )
     ORDER BY ts.timeseries_id LIMIT 1",
    params = list(timeseries_id, parameter_id, liquid_id)
  ))

  route <- DBI::dbGetQuery(
    con,
    "SELECT route.transmission_route_id
     FROM public.locations_metadata_transmission_routes route
     JOIN public.locations_metadata_transmission_setups setup
       USING (transmission_setup_id)
     WHERE setup.location_id = (
       SELECT location_id FROM continuous.timeseries WHERE timeseries_id = $1
     )
     ORDER BY route.transmission_route_id LIMIT 1",
    params = list(timeseries_id)
  )
  skip_if(nrow(route) != 1L, "dev has no timeseries with a transmission route")

  extremum_timezone <- DBI::dbGetQuery(
    con,
    "SELECT timezone_daily_calc FROM continuous.timeseries
     WHERE timeseries_id = $1",
    params = list(timeseries_id)
  )$timezone_daily_calc[[1]]

  invisible(DBI::dbExecute(
    con,
    "INSERT INTO continuous.transmission_timeseries_mappings
       (transmission_route_id, source_field, timeseries_id,
        value_multiplier, value_offset, missing_values, mapping_config,
        enabled, created_by)
     VALUES ($1, $2, $3, 3, 4, '[]'::jsonb, '{}'::jsonb, FALSE, CURRENT_USER)",
    params = list(
      as.integer(route$transmission_route_id[[1]]),
      paste0("__codex_unit_test_", parameter_id),
      timeseries_id
    )
  ))
  invisible(DBI::dbExecute(
    con,
    "INSERT INTO continuous.measurements_continuous
       (timeseries_id, datetime, value, created_by)
     VALUES ($1, '2025-01-01 12:00:00+00', 7, CURRENT_USER)",
    params = list(timeseries_id)
  ))

  correction_type_id <- as.integer(DBI::dbGetQuery(
    con,
    "SELECT correction_type_id FROM continuous.correction_types
     WHERE correction_type = 'offset linear'"
  )$correction_type_id[[1]])
  invisible(DBI::dbExecute(
    con,
    "INSERT INTO continuous.corrections
       (correction_id, timeseries_id, start_dt, end_dt,
        correction_type, value1, created_by)
     VALUES ($1, $2, '2025-01-01', '2025-02-01', $3, 3, CURRENT_USER)",
    params = list(correction_id, timeseries_id, correction_type_id)
  ))
  invisible(DBI::dbExecute(
    con,
    "INSERT INTO continuous.measurements_calculated_daily
       (timeseries_id, date, value, imputed, max, min, q90, q75, q50, q25,
        q10, mean, max_30yr, min_30yr, q90_30yr, q75_30yr, q50_30yr,
        q25_30yr, q10_30yr, mean_30yr, created_by)
     VALUES ($1, '2025-01-01', 10, FALSE, 12, 8, 11, 10.5, 10, 9.5, 9,
       10, 14, 6, 13, 12, 10, 8, 7, 10, CURRENT_USER)",
    params = list(timeseries_id)
  ))
  invisible(DBI::dbExecute(
    con,
    "INSERT INTO continuous.extrema
       (timeseries_id, agency_organization_id, extremum_year, extremum_date,
        timezone_daily_calc, value, period_type_code, condition_code, extremum_type_code,
        record_source_code, uncertainty_lower, uncertainty_upper,
        calculation_method, created_by)
     VALUES ($1, $2, 2025, '2025-01-01', $3, 10, '1-day', 'open_water',
       'maximum', 'calculated', 0.5, 1.5, 'test fixture', CURRENT_USER)",
    params = list(timeseries_id, organization_id, extremum_timezone)
  ))

  # Real imported series carry review and provenance segments. These contain
  # no measurement values and must neither block migration nor be rewritten.
  for (table in c("approvals", "contributors", "grades", "owners", "qualifiers")) {
    columns <- DBI::dbListFields(con, DBI::Id(schema = "continuous", table = table))
    identity <- DBI::dbGetQuery(con,
      "SELECT column_name FROM information_schema.columns
       WHERE table_schema = 'continuous' AND table_name = $1
         AND (is_identity = 'YES' OR column_default LIKE 'nextval(%')",
      params = list(table))$column_name
    columns <- setdiff(columns, c(identity, "timeseries_id"))
    quoted <- paste(as.character(DBI::dbQuoteIdentifier(con, columns)), collapse = ", ")
    # Copy one existing segment's valid reference IDs onto the isolated series.
    DBI::dbExecute(con, sprintf(
      "INSERT INTO continuous.%s (timeseries_id, %s)
       SELECT $1, %s FROM continuous.%s LIMIT 1", table, quoted, quoted, table),
      params = list(timeseries_id))
  }
  metadata_before <- lapply(c("approvals", "contributors", "grades", "owners", "qualifiers"), function(table) {
    DBI::dbGetQuery(con, sprintf("SELECT * FROM continuous.%s WHERE timeseries_id = $1", table),
      params = list(timeseries_id))
  })
  expect_true(all(vapply(metadata_before, nrow, integer(1)) > 0L))

  old_units <- list(
    units_liquid = as.integer(units$unit_id[[1]]),
    units_solid = NA_integer_, units_gas = NA_integer_, units_na = NA_integer_
  )
  new_units <- list(
    units_liquid = NA_integer_,
    units_solid = as.integer(units$unit_id[[2]]),
    units_gas = NA_integer_, units_na = NA_integer_
  )
  changed_units <- editor$parameter_changed_assigned_unit_fields(
    as.data.frame(old_units), as.data.frame(new_units)
  )
  usage <- editor$fetch_parameter_unit_usage(
    con, parameter_id, changed_units, as.data.frame(new_units)
  )
  expect_length(usage$blockers, 0L)
  expect_true(any(usage$usage$table == "continuous.extrema"))
  expect_true(all(usage$usage$matrix_state_code == "liquid"))

  # A formula for another parameter must block changes to one of its inputs.
  consumer_rule <- DBI::dbGetQuery(con,
    "SELECT rule_id FROM criteria.guideline_value_rules
     WHERE algorithm_code IN ('linear', 'log_linear', 'sql_scalar')
     ORDER BY rule_id LIMIT 1")$rule_id[[1]]
  input_id <- DBI::dbGetQuery(con,
    "INSERT INTO criteria.guideline_rule_inputs
       (rule_id, input_code, input_source, parameter_id, matrix_state_id)
     VALUES ($1, $2, 'sample_result', $3, $4) RETURNING input_id",
    params = list(consumer_rule, paste0("unit_test_", parameter_id), parameter_id, liquid_id))$input_id[[1]]
  blocked_usage <- editor$fetch_parameter_unit_usage(
    con, parameter_id, changed_units, as.data.frame(new_units))
  expect_contains(blocked_usage$blockers, "criteria.guideline_value_rules (dependent input)")
  expect_false(blocked_usage$usage$supported[
    blocked_usage$usage$table == "criteria.guideline_value_rules (dependent input)"])
  DBI::dbExecute(con, "DELETE FROM criteria.guideline_rule_inputs WHERE input_id = $1",
    params = list(input_id))

  # Exercise the actual modal confirmation observer, rather than constructing
  # a plan with hand-picked column names that can diverge from the Shiny UI.
  editor$application_notifications_ui <- function(...) NULL
  # Sibling catalogue editors have their own modules and test environments.
  editor$referenceTableManager <- function(...) NULL
  editor$clear_reference_caches <- function(session) NULL
  session <- shiny::MockShinySession$new()
  session$userData$AquaCache <- con
  confirmation <- new.env(parent = emptyenv())
  confirmation$plan <- NULL
  confirmation$warnings <- list()
  confirmation$messages <- character()
  editor$showNotification <- function(ui, ..., type = "default", duration = 5) {
    confirmation$messages <- c(confirmation$messages, as.character(ui))
    if (identical(type, "warning")) {
      confirmation$warnings[[length(confirmation$warnings) + 1L]] <- list(text = ui, duration = duration)
    }
  }
  apply_migration <- editor$parameter_unit_apply_migration
  editor$parameter_unit_apply_migration <- function(con, parameter_id, plan, mapping_sets) {
    confirmation$plan <- plan
    apply_migration(con, parameter_id, plan, mapping_sets)
  }
  # Run the module's complete save transaction as a savepoint, keeping every
  # fixture and migration inside this test's outer rollback transaction.
  testthat::local_mocked_bindings(dbWithTransaction = function(conn, code, ...) {
    DBI::dbBegin(conn, name = "parameter_unit_save")
    tryCatch({
      result <- force(code)
      DBI::dbCommit(conn, name = "parameter_unit_save")
      result
    }, error = function(e) {
      DBI::dbRollback(conn, name = "parameter_unit_save")
      stop(e)
    })
  }, .package = "DBI")
  shiny::testServer(editor$manageParameters,
    args = list(language = shiny::reactiveValues(language = "en")),
    session = session, {
      session$flushReact()
      session$setInputs(ui_ready = "parameter-editor-bound")
      confirmation$busy <- logical()
      assign("set_parameter_save_busy", function(busy, ...) {
        confirmation$busy <- c(confirmation$busy, busy)
      }, envir = session$env)
      # Invalid form data still restores the buttons; a submission made while
      # busy is ignored rather than starting a second dependency scan.
      expect_false(save_parameter())
      expect_identical(confirmation$busy, c(TRUE, FALSE))
      expect_false(get("parameter_save_busy", envir = session$env))
      assign("parameter_save_busy", TRUE, envir = session$env)
      expect_false(save_parameter())
      expect_identical(confirmation$busy, c(TRUE, FALSE))
      assign("parameter_save_busy", FALSE, envir = session$env)
      current_record_id(as.character(parameter_id))
      defaults <- editor$parameter_unit_change_defaults(
        con, usage$changed_units, as.data.frame(new_units), usage$state_choices, usage)
      expect_identical(defaults$matrix_state_code, "liquid")
      pending_unit_change(list(
        selected_id = as.character(parameter_id), values = new_units,
        usage = blocked_usage, defaults = defaults,
        old_units = old_units,
        old_units_json = as.character(jsonlite::toJSON(old_units, auto_unbox = TRUE, na = "null")),
        changed_units = usage$changed_units,
        usage_signature = as.character(editor$parameter_unit_inventory_signature(usage))))
      editor$showModal <- function(ui) confirmation$modal <- as.character(ui)
      show_parameter_unit_change_modal(pending_unit_change())
      expect_match(confirmation$modal, "Previously staged or queued uploads")
      for (metric in c("discrete_a", "discrete_b", "continuous_a", "continuous_b",
                       "transmission_a", "transmission_b", "mapping_a", "mapping_b")) {
        expect_true(grepl(paste0('id="', session$ns(paste0(metric, "_1")), '"'),
          confirmation$modal, fixed = TRUE))
      }
      session$flushReact()
      session$setInputs(confirm_unit_change = 1L)
      expect_null(confirmation$plan)
      pending <- pending_unit_change()
      pending$usage <- usage
      pending_unit_change(pending)
      session$flushReact()
      controls <- list(confirm_unit_change = 2L)
      for (i in seq_len(nrow(defaults))) {
        controls[[paste0("unit_destination_", i)]] <- "solid"
        if (is.na(defaults$old_unit_id[[i]])) {
          controls[[paste0("unit_assumed_", i)]] <- as.character(units$unit_id[[2]])
        }
        source_id <- if (is.na(defaults$old_unit_id[[i]])) units$unit_id[[2]] else defaults$old_unit_id[[i]]
        controls[[paste0("unit_conversion_mode_", i)]] <- paste0("convert:", source_id, ":", units$unit_id[[2]])
        for (metric in c("discrete_a", "discrete_b", "continuous_a", "continuous_b",
                         "transmission_a", "transmission_b", "mapping_a", "mapping_b")) {
          controls[[paste0(metric, "_", i)]] <- switch(metric,
            continuous_a = 2, continuous_b = 1,
            transmission_a = 4, transmission_b = 5,
            if (grepl("_a$", metric)) 1 else 0)
        }
      }
      # Fail after measurements and parameter units have changed. The save
      # boundary must roll both back, together with its authorization log.
      save_suggestions <- editor$save_parameter_suggestions
      editor$save_parameter_suggestions <- function(...) stop("Injected save failure")
      do.call(session$setInputs, controls)
      expect_false(is.null(pending_unit_change()))
      expect_identical(editor$fetch_parameter_unit_values(con, parameter_id), as.data.frame(old_units))
      expect_equal(as.numeric(DBI::dbGetQuery(con,
        "SELECT value FROM continuous.measurements_continuous WHERE timeseries_id = $1",
        params = list(timeseries_id))$value), 7)
      expect_equal(DBI::dbGetQuery(con,
        "SELECT count(*)::INTEGER AS n FROM audit.parameter_unit_change_log WHERE parameter_id = $1",
        params = list(parameter_id))$n, 0L)
      editor$save_parameter_suggestions <- save_suggestions
      session$setInputs(confirm_unit_change = 3L)
      expect_false(is.null(confirmation$plan))
      expect_null(pending_unit_change(), info = paste(confirmation$messages, collapse = "\n"))
      expect_length(confirmation$warnings, 1L)
      expect_match(confirmation$warnings[[1]]$text, "reload that upload module")
      expect_null(confirmation$warnings[[1]]$duration)
    })
  confirmed_plan <- confirmation$plan
  expect_true(all(c("discrete_a", "continuous_a", "transmission_a", "mapping_a") %in% names(confirmed_plan)))

  measured <- DBI::dbGetQuery(
    con,
    "SELECT value FROM continuous.measurements_continuous WHERE timeseries_id = $1",
    params = list(timeseries_id)
  )$value[[1]]
  daily <- DBI::dbGetQuery(
    con,
    "SELECT value, max, min, mean, max_30yr, min_30yr, mean_30yr
     FROM continuous.measurements_calculated_daily WHERE timeseries_id = $1",
    params = list(timeseries_id)
  )
  extrema <- DBI::dbGetQuery(
    con,
    "SELECT value, uncertainty_lower, uncertainty_upper, timezone_daily_calc
     FROM continuous.extrema WHERE timeseries_id = $1",
    params = list(timeseries_id)
  )
  correction <- DBI::dbGetQuery(
    con,
    "SELECT value1 FROM continuous.corrections WHERE timeseries_id = $1",
    params = list(timeseries_id)
  )$value1[[1]]
  transmission <- DBI::dbGetQuery(
    con,
    "SELECT value_multiplier, value_offset
     FROM continuous.transmission_timeseries_mappings WHERE timeseries_id = $1",
    params = list(timeseries_id)
  )
  migrated_state <- DBI::dbGetQuery(
    con,
    "SELECT matrix_state_id FROM continuous.timeseries WHERE timeseries_id = $1",
    params = list(timeseries_id)
  )$matrix_state_id[[1]]

  expect_equal(as.numeric(measured), 15)
  expect_equal(
    as.numeric(unlist(daily[1, ], use.names = FALSE)),
    c(21, 25, 17, 21, 29, 13, 21)
  )
  expect_equal(as.numeric(extrema[1, 1:3]), c(21, 1, 3))
  expect_equal(extrema$timezone_daily_calc[[1]], extremum_timezone)
  expect_equal(as.numeric(correction), 6)
  expect_equal(as.numeric(transmission[1, ]), c(12, 21))
  expect_equal(as.integer(migrated_state), solid_id)
  expect_identical(editor$fetch_parameter_unit_values(con, parameter_id), as.data.frame(new_units))
  expect_equal(DBI::dbGetQuery(con,
    "SELECT count(*)::INTEGER AS n FROM audit.parameter_unit_change_log WHERE parameter_id = $1",
    params = list(parameter_id))$n, 1L)
  metadata_after <- lapply(c("approvals", "contributors", "grades", "owners", "qualifiers"), function(table) {
    DBI::dbGetQuery(con, sprintf("SELECT * FROM continuous.%s WHERE timeseries_id = $1", table),
      params = list(timeseries_id))
  })
  expect_identical(metadata_after, metadata_before)
  expect_false(DBI::dbGetQuery(
    con,
    "SELECT current_setting('continuous.skip_daily_refresh', TRUE) = 'on' AS skipped"
  )$skipped[[1]])
})

test_that("dev discrete migration converts limits and publishes a new mapping revision", {
  editor <- parameter_unit_migration_test_environment()
  con <- parameter_unit_migration_test_connection()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbBegin(con)
  on.exit(DBI::dbRollback(con), add = TRUE, after = FALSE)
  parameter_id <- DBI::dbGetQuery(con,
    "SELECT (max(parameter_id) + 100001)::INTEGER AS id FROM public.parameters")$id[[1]]
  units <- DBI::dbGetQuery(con, "SELECT unit_id FROM public.units ORDER BY unit_id LIMIT 2")$unit_id
  states <- DBI::dbGetQuery(con,
    "SELECT matrix_state_id, matrix_state_code FROM public.matrix_states
     WHERE matrix_state_code IN ('liquid', 'solid')")
  liquid_id <- states$matrix_state_id[match("liquid", states$matrix_state_code)]
  solid_id <- states$matrix_state_id[match("solid", states$matrix_state_code)]
  DBI::dbExecute(con,
    "INSERT INTO public.parameters
       (parameter_id, param_name, sample_fraction, plot_default_y_orientation, units_liquid)
     VALUES ($1, $2, FALSE, 'normal', $3)",
    params = list(parameter_id, paste0("__codex_discrete_unit_test_", Sys.getpid()), units[[1]]))
  old_units <- data.frame(units_liquid = units[[1]], units_solid = NA_integer_,
    units_gas = NA_integer_, units_na = NA_integer_)
  new_units <- data.frame(units_liquid = NA_integer_, units_solid = units[[2]],
    units_gas = NA_integer_, units_na = NA_integer_)
  changed <- editor$parameter_changed_assigned_unit_fields(old_units, new_units)
  result_id <- DBI::dbGetQuery(con,
    "INSERT INTO discrete.results (sample_id, parameter_id, matrix_state_id, result_type, result)
     SELECT min(sample_id), $1, $2, 2, 7 FROM discrete.samples RETURNING result_id",
    params = list(parameter_id, liquid_id))$result_id[[1]]
  guideline_id <- DBI::dbGetQuery(con,
    "INSERT INTO criteria.guidelines (guideline_name, guideline_code, parameter_id, matrix_state_id)
     VALUES ('Unit migration test', $1, $2, $3) RETURNING guideline_id",
    params = list(paste0("codex_unit_test_", Sys.getpid()), parameter_id, liquid_id))$guideline_id[[1]]
  rule_id <- DBI::dbGetQuery(con,
    "INSERT INTO criteria.guideline_value_rules
       (guideline_id, algorithm_code, fixed_value, min_output_value, max_output_value)
     VALUES ($1, 'constant', 7, 6, 8) RETURNING rule_id",
    params = list(guideline_id))$rule_id[[1]]
  source_id <- DBI::dbGetQuery(con,
    "INSERT INTO discrete.import_sources (source_code, source_name)
     VALUES ($1, 'Unit migration test') RETURNING import_source_id",
    params = list(paste0("codex_unit_test_", Sys.getpid())))$import_source_id[[1]]
  prepare <- getFromNamespace("import_mapping_prepare_revision", "AquaCache")
  publish <- getFromNamespace("import_mapping_publish_revision", "AquaCache")
  set_id <- prepare(con, source_id)
  DBI::dbExecute(con,
    "INSERT INTO discrete.import_parameter_mappings
       (import_mapping_set_id, source_match, parameter_id, result_type, matrix_state_id, conversion, result_offset)
     VALUES ($1, '{\"parameter\":\"test\"}', $2, 2, $3, 3, 4)",
    params = list(set_id, parameter_id, liquid_id))
  publish(con, set_id)
  open_draft <- prepare(con, source_id)
  DBI::dbExecute(con,
    "UPDATE discrete.import_parameter_mappings SET conversion = 9, result_offset = 10,
       note = 'Unfinished target edit' WHERE import_mapping_set_id = $1", params = list(open_draft))
  DBI::dbExecute(con,
    "INSERT INTO discrete.import_parameter_mappings
       (import_mapping_set_id, source_match, result_type, active, note)
     VALUES ($1, '{\"parameter\":\"unfinished\"}', 2, FALSE, 'Keep unpublished')", params = list(open_draft))
  DBI::dbExecute(con,
    "INSERT INTO discrete.import_location_mappings (import_mapping_set_id, source_location_code, location_id)
     SELECT $1, 'unfinished-location', min(location_id) FROM public.locations", params = list(open_draft))
  draft_source <- DBI::dbGetQuery(con,
    "INSERT INTO discrete.import_sources (source_code, source_name)
     VALUES ($1, 'Draft-only unit test') RETURNING import_source_id",
    params = list(paste0("codex_draft_unit_test_", Sys.getpid())))$import_source_id[[1]]
  draft_only <- prepare(con, draft_source)
  DBI::dbExecute(con,
    "INSERT INTO discrete.import_parameter_mappings
       (import_mapping_set_id, source_match, parameter_id, result_type, matrix_state_id, conversion, result_offset)
     VALUES ($1, '{\"parameter\":\"draft-only\"}', $2, 2, $3, 5, 6)",
    params = list(draft_only, parameter_id, liquid_id))
  usage <- editor$fetch_parameter_unit_usage(con, parameter_id, changed, new_units)
  expect_length(usage$blockers, 0L)
  expect_equal(nrow(usage$mapping_sets), 3L)

  # A fixed guideline's rounding is in its original unit. Keeping two decimal
  # places after a scale change can silently turn a small limit into zero.
  DBI::dbExecute(con,
    "UPDATE criteria.guideline_value_rules SET rounding_method = 'round', rounding_digits = 2 WHERE rule_id = $1",
    params = list(rule_id))
  rounded <- editor$fetch_parameter_unit_usage(con, parameter_id, changed, new_units)
  expect_contains(rounded$blockers, "criteria.guideline_value_rules (rounding)")
  DBI::dbExecute(con,
    "UPDATE criteria.guideline_value_rules SET rounding_method = 'none', rounding_digits = NULL WHERE rule_id = $1",
    params = list(rule_id))

  json <- function(x) as.character(jsonlite::toJSON(as.list(x), auto_unbox = TRUE, na = "null"))
  DBI::dbGetQuery(con,
    "SELECT public.begin_parameter_unit_change($1, $2::jsonb, $3::jsonb, '{}')",
    params = list(parameter_id, json(old_units), json(new_units)))
  DBI::dbExecute(con,
    "UPDATE public.parameters SET units_liquid = NULL, units_solid = $2 WHERE parameter_id = $1",
    params = list(parameter_id, units[[2]]))
  plan <- data.frame(matrix_state_id = liquid_id, destination_id = solid_id,
    discrete_a = 2, discrete_b = 1, continuous_a = 1, continuous_b = 0,
    transmission_a = 1, transmission_b = 0, mapping_a = 4, mapping_b = 5)
  editor$parameter_unit_apply_migration(con, parameter_id, plan, usage$mapping_sets)
  result <- DBI::dbGetQuery(con,
    "SELECT result, matrix_state_id FROM discrete.results WHERE result_id = $1", params = list(result_id))
  expect_equal(as.numeric(result$result), 15)
  expect_equal(result$matrix_state_id, solid_id)
  limits <- DBI::dbGetQuery(con,
    "SELECT fixed_value, min_output_value, max_output_value FROM criteria.guideline_value_rules WHERE rule_id = $1",
    params = list(rule_id))
  expect_equal(as.numeric(limits[1, ]), c(15, 13, 17))
  expect_equal(DBI::dbGetQuery(con,
    "SELECT matrix_state_id FROM criteria.guidelines WHERE guideline_id = $1",
    params = list(guideline_id))$matrix_state_id, solid_id)
  mappings <- DBI::dbGetQuery(con,
    "SELECT s.version, s.status, m.conversion, m.result_offset, m.matrix_state_id
     FROM discrete.import_mapping_sets s JOIN discrete.import_parameter_mappings m USING (import_mapping_set_id)
     WHERE s.import_source_id = $1 AND m.parameter_id = $2 ORDER BY s.version", params = list(source_id, parameter_id))
  expect_identical(mappings$status, c("retired", "published", "draft"))
  expect_equal(as.numeric(mappings$conversion), c(3, 12, 36))
  expect_equal(as.numeric(mappings$result_offset), c(4, 21, 45))
  expect_equal(mappings$matrix_state_id, c(liquid_id, solid_id, solid_id))
  unfinished <- DBI::dbGetQuery(con,
    "SELECT s.status FROM discrete.import_parameter_mappings m
     JOIN discrete.import_mapping_sets s USING (import_mapping_set_id)
     WHERE s.import_source_id = $1 AND m.note = 'Keep unpublished'", params = list(source_id))
  expect_identical(unfinished$status, "draft")
  unfinished_locations <- DBI::dbGetQuery(con,
    "SELECT s.status FROM discrete.import_location_mappings m
     JOIN discrete.import_mapping_sets s USING (import_mapping_set_id)
     WHERE s.import_source_id = $1 AND m.source_location_code = 'unfinished-location'", params = list(source_id))
  expect_identical(unfinished_locations$status, "draft")
  draft_result <- DBI::dbGetQuery(con,
    "SELECT s.status, m.matrix_state_id, m.conversion, m.result_offset
     FROM discrete.import_mapping_sets s JOIN discrete.import_parameter_mappings m USING (import_mapping_set_id)
     WHERE s.import_mapping_set_id = $1", params = list(draft_only))
  expect_identical(draft_result$status, "draft")
  expect_equal(draft_result$matrix_state_id, solid_id)
  expect_equal(as.numeric(draft_result[1, c("conversion", "result_offset")]), c(20, 29))

  # Relabel an already-used unit without a numeric conversion. The UI's keep
  # action generates identity conversions for every stored and future path.
  kept_units <- new_units
  kept_units$units_solid <- units[[1]]
  usage <- editor$fetch_parameter_unit_usage(con, parameter_id,
    editor$parameter_changed_assigned_unit_fields(new_units, kept_units), kept_units)
  DBI::dbGetQuery(con,
    "SELECT public.begin_parameter_unit_change($1, $2::jsonb, $3::jsonb, '{}')",
    params = list(parameter_id, json(new_units), json(kept_units)))
  DBI::dbExecute(con, "UPDATE public.parameters SET units_solid = $2 WHERE parameter_id = $1",
    params = list(parameter_id, units[[1]]))
  keep_plan <- data.frame(matrix_state_id = solid_id, destination_id = solid_id,
    conversion_action = "keep", discrete_a = 1, discrete_b = 0,
    continuous_a = 1, continuous_b = 0, transmission_a = 1, transmission_b = 0,
    mapping_a = 1, mapping_b = 0)
  editor$parameter_unit_apply_migration(con, parameter_id, keep_plan, usage$mapping_sets)
  expect_identical(editor$fetch_parameter_unit_values(con, parameter_id), kept_units)
  expect_equal(as.numeric(DBI::dbGetQuery(con,
    "SELECT result FROM discrete.results WHERE result_id = $1", params = list(result_id))$result), 15)
  expect_equal(as.numeric(DBI::dbGetQuery(con,
    "SELECT fixed_value FROM criteria.guideline_value_rules WHERE rule_id = $1", params = list(rule_id))$fixed_value), 15)
  published <- DBI::dbGetQuery(con,
    "SELECT m.conversion, m.result_offset FROM discrete.import_parameter_mappings m
     JOIN discrete.import_mapping_sets s USING (import_mapping_set_id)
     WHERE s.import_source_id = $1 AND s.status = 'published'", params = list(source_id))
  expect_equal(as.numeric(published[1, ]), c(12, 21))
})

test_that("the modal clears stale conversions and accepts an explicit keep-numbers choice", {
  editor <- parameter_unit_migration_test_environment()
  con <- parameter_unit_migration_test_connection()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  editor$referenceTableManager <- function(...) NULL
  editor$application_notifications_ui <- function(...) NULL
  # Synthetic unit pairs exercise known and unknown conversions independently
  # of the cluster's conversion catalogue. No database rows are written here.
  editor$parameter_unit_conversion <- function(con, from_unit_id, to_unit_id) {
    if (identical(to_unit_id, 2L)) data.frame(scale_a = 1000, scale_b = 0) else NULL
  }
  captured <- new.env(parent = emptyenv())
  captured$plan <- NULL
  captured$numeric <- list()
  captured$mode <- NULL
  editor$updateNumericInput <- function(session, inputId, value, ...) captured$numeric[[inputId]] <- value
  editor$updateSelectInput <- function(session, inputId, choices, selected, ...) {
    if (grepl("unit_conversion_mode_", inputId)) captured$mode <- selected
  }
  session <- shiny::MockShinySession$new()
  session$userData$AquaCache <- con
  shiny::testServer(editor$manageParameters,
    args = list(language = shiny::reactiveValues(language = "en")), session = session, {
      session$flushReact()
      session$setInputs(ui_ready = "parameter-editor-bound")
      states <- data.frame(matrix_state_id = 1:2, matrix_state_code = c("liquid", "solid"),
        matrix_state_name = c("Liquid", "Solid"))
      values <- list(units_liquid = 2L, units_solid = 3L, units_gas = NA_integer_, units_na = NA_integer_)
      changed <- data.frame(unit_column = "units_liquid", unit_label = "Liquid unit",
        matrix_state_code = "liquid", matrix_state_id = 1L, old_unit_id = 1L, new_unit_id = 2L)
      defaults <- editor$parameter_unit_change_defaults(con, changed, values, states)
      unknown <- editor$parameter_unit_change_defaults(con, changed, values, states[2, , drop = FALSE])
      expect_false(unknown$conversion_known[[1]])
      expect_true(is.na(unknown$conversion_a[[1]]) && is.na(unknown$conversion_b[[1]]))
      unit_conversion_auto_keys("1:2")
      pending_unit_change(list(values = values, defaults = defaults, selected_id = "1",
        usage = list(blockers = character(), state_choices = states,
          usage = data.frame(matrix_state_code = "liquid"))))
      assign("commit_parameter_save", function(values, selected_id, migration) {
        captured$plan <- migration$plan
      }, envir = session$env)
      controls <- list(unit_destination_1 = "liquid", unit_conversion_mode_1 = "convert:1:2")
      for (metric in c("discrete_a", "continuous_a", "transmission_a", "mapping_a")) controls[[paste0(metric, "_1")]] <- 1000
      for (metric in c("discrete_b", "continuous_b", "transmission_b", "mapping_b")) controls[[paste0(metric, "_1")]] <- 0
      do.call(session$setInputs, controls)
      captured$numeric <- list()
      session$setInputs(unit_destination_1 = "solid")
      expect_identical(captured$mode, "")
      expect_length(captured$numeric, 8L)
      expect_true(all(is.na(unlist(captured$numeric))))
      # Old values/actions still present in the browser cannot be confirmed
      # against a different unit pair while updates are in flight.
      session$setInputs(confirm_unit_change = 1L)
      expect_null(captured$plan)
      session$setInputs(unit_conversion_mode_1 = "keep:1:3", confirm_unit_change = 2L)
      expect_identical(captured$plan$conversion_action, "keep")
      expect_equal(as.numeric(captured$plan[1, c("discrete_a", "continuous_a", "transmission_a", "mapping_a")]), rep(1, 4))
      expect_equal(as.numeric(captured$plan[1, c("discrete_b", "continuous_b", "transmission_b", "mapping_b")]), rep(0, 4))
      # A user may instead supply a custom conversion for the unknown pair.
      session$setInputs(unit_conversion_mode_1 = "convert:1:3", discrete_a_1 = 5,
        discrete_b_1 = 7, confirm_unit_change = 3L)
      expect_identical(captured$plan$conversion_action, "convert")
      expect_equal(captured$plan$discrete_a, 5)
      expect_equal(captured$plan$discrete_b, 7)
      session$setInputs(unit_destination_1 = "liquid")
      expect_identical(captured$mode, "convert:1:2")
      expect_equal(captured$numeric$discrete_a_1, 1000)
    })
})
