field_visit_test_environment <- function() {
  env <- new.env(parent = asNamespace("shiny"))
  globals <- parse(system.file("apps/aqualink/YGwater_globals.R", package = "YGwater"))
  load_functions <- function(expression) {
    if (is.call(expression) && identical(expression[[1]], as.name("<<-")) &&
        is.call(expression[[3]]) && identical(expression[[3]][[1]], as.name("function"))) {
      eval(call("<-", expression[[2]], expression[[3]]), envir = env)
    } else if (is.call(expression) || is.expression(expression)) {
      for (child in as.list(expression)) load_functions(child)
    }
  }
  load_functions(globals)
  env$application_notifications_ui <- function(...) NULL
  sys.source(system.file(
    "apps/aqualink/modules/admin/field/field_visit.R", package = "YGwater"
  ), envir = env)
  env
}

field_visit_test_result <- function() {
  data.frame(
    result_id = 501L, sample_id = 101L, result_type = 1L, parameter_id = 1L,
    matrix_state_id = 1L, protocol_method = NA_integer_, sample_fraction_id = NA_integer_,
    result = 7, result_condition = NA_integer_, result_condition_value = NA_real_,
    result_value_type = 1L, reporting_basis_id = NA_integer_, laboratory = NA_integer_,
    analysis_datetime = as.POSIXct("2026-10-20 12:00:00", tz = "UTC"),
    lab_report_no = "preserve report", lab_sample_no = NA_character_, grade_type_id = 2L,
    approval_type_id = 3L, no_source_update = TRUE, note = "original note",
    share_with = "{public_reader}", result_speciation_id = NA_integer_,
    result_target_id = NA_integer_, endpoint_mode = NA_character_, endpoint_ph = NA_real_,
    analysis_conditions = "{}", stringsAsFactors = FALSE
  )
}

test_that("Field visit keys follow the full NULL-equal analytical identity", {
  module <- field_visit_test_environment()
  result <- field_visit_test_result()
  original <- module$field_visit_result_keys(result)
  expect_identical(module$field_visit_result_keys(data.frame()), character())
  same <- result
  same$result <- 8
  same$note <- "a new note"
  attr(same$analysis_datetime, "tzone") <- "America/Whitehorse"
  expect_identical(module$field_visit_result_keys(same), original)
  dimensions <- c(
    "result_type", "parameter_id", "matrix_state_id", "sample_fraction_id",
    "result_value_type", "reporting_basis_id", "result_speciation_id", "result_target_id",
    "endpoint_mode", "endpoint_ph", "analysis_conditions", "protocol_method",
    "laboratory", "analysis_datetime"
  )
  for (column in dimensions) {
    changed <- result
    changed[[column]] <- switch(column,
      endpoint_mode = "fixed_ph", analysis_conditions = '{"mass_basis":"dry"}',
      analysis_datetime = result$analysis_datetime + 60, 99L
    )
    expect_false(identical(module$field_visit_result_keys(changed), original), info = column)
  }
  result$analysis_conditions <- '{"mass_basis":"dry","nested":{"a":1,"b":2}}'
  same$analysis_conditions <- '{"nested":{"b":2.0,"a":1.0},"mass_basis":"dry"}'
  expect_identical(module$field_visit_result_keys(result), module$field_visit_result_keys(same))
})

test_that("Field visit writes require current, explicit overwrite approval", {
  module <- field_visit_test_environment()
  stored <- field_visit_test_result()
  original <- stored
  proposed <- stored
  proposed$result <- 8
  proposed$note <- "replacement note"
  queries <- list()
  insert_conflict <- FALSE
  update_denied <- FALSE
  testthat::local_mocked_bindings(
    dbGetQuery = function(conn, statement, params, ...) {
      queries[[length(queries) + 1L]] <<- list(sql = statement, params = params)
      if (grepl("SELECT *", statement, fixed = TRUE)) return(stored)
      if (grepl("UPDATE discrete.results", statement, fixed = TRUE)) {
        if (update_denied) return(data.frame(result_id = integer()))
        return(data.frame(result_id = 501L))
      }
      if (grepl("INSERT INTO discrete.results", statement, fixed = TRUE)) {
        if (insert_conflict) return(data.frame(result_id = integer()))
        return(data.frame(result_id = 502L))
      }
      stop("Unexpected result query: ", statement)
    }, .package = "DBI"
  )
  expect_error(module$field_visit_write_results(NULL, 101, proposed, data.frame()),
               "confirm its replacement")
  expect_length(queries, 1)
  expect_no_error(module$field_visit_write_results(NULL, 101, proposed, original))
  update <- queries[[length(queries)]]
  expect_match(update$sql, "WHERE result_id = $5 AND sample_id = $6", fixed = TRUE)
  expect_equal(update$params, list(8, NA_integer_, NA_real_, "replacement note", 501L, 101L))
  expect_false(grepl("share_with|grade_type|approval_type|lab_report|no_source_update", update$sql))
  expect_match(queries[[1]]$sql, "FOR UPDATE", fixed = TRUE)

  for (column in c("result", "result_condition", "result_condition_value", "note", "result_id")) {
    stored <- original
    stored[[column]] <- if (column == "note") "changed elsewhere" else 99L
    expect_error(module$field_visit_write_results(NULL, 101, proposed, original),
                 "changed after you reviewed", info = column)
  }
  stored <- original
  stored$result <- stored$result + 1e-10
  expect_error(module$field_visit_write_results(NULL, 101, proposed, original),
               "changed after you reviewed")
  stored <- original[FALSE, ]
  expect_error(module$field_visit_write_results(NULL, 101, proposed, original),
               "changed after you reviewed")
  expect_no_error(module$field_visit_write_results(NULL, 101, proposed, data.frame()))
  insert <- queries[[length(queries)]]
  expect_match(insert$sql, "ON CONFLICT DO NOTHING", fixed = TRUE)
  expect_match(insert$sql, "$20::text[]", fixed = TRUE)
  expect_match(insert$sql, "$25::jsonb", fixed = TRUE)
  insert_conflict <- TRUE
  expect_error(module$field_visit_write_results(NULL, 101, proposed, data.frame()),
               "could not be saved")
  stored <- original
  update_denied <- TRUE
  expect_error(module$field_visit_write_results(NULL, 101, proposed, original),
               "could not be saved")
  query_count <- length(queries)
  expect_error(module$field_visit_write_results(NULL, 101, rbind(proposed, proposed), original),
               "Duplicate measurements")
  expect_length(queries, query_count)
})

test_that("Sample rows reopen staged measurements and require overwrite confirmation", {
  module <- field_visit_test_environment()
  stored <- field_visit_test_result()
  sample <- data.frame(
    sample_id = 101L, location_id = 1L, sub_location_id = NA_integer_, media_id = 1L,
    datetime = stored$analysis_datetime, note = "Sample note", share_with = "{public_reader}",
    sample_type = 1L, sample_type_name = "Field observation", field_result_count = 1L
  )
  catalogues <- list(
    "public.locations" = data.frame(location_id = 1L, location = "TEST", name = "Test site"),
    "public.sub_locations" = data.frame(sub_location_id = 1L, sub_location_name = "Test", location_id = 1L),
    "public.parameters" = data.frame(
      parameter_id = 1L, param_name = "pH", unit_liquid = NA_character_,
      sample_fraction = FALSE, reporting_basis = FALSE, result_speciation = FALSE,
      result_target = FALSE, endpoint = FALSE, analysis_conditions_required_keys = "[]"
    ),
    "public.media_types" = data.frame(media_id = 1L, media_type = "Water"),
    "public.organizations" = data.frame(organization_id = 1L, name = "Test owner"),
    "discrete.sample_types" = data.frame(sample_type_id = 1L, sample_type = "Field observation",
                                          requires_location = TRUE, requires_sample_group = FALSE),
    "discrete.collection_methods" = data.frame(collection_method_id = 1L, collection_method = "Observation"),
    "discrete.result_types" = data.frame(result_type_id = 1L, result_type = "field"),
    "discrete.result_conditions" = data.frame(result_condition_id = 1L, result_condition = "Below limit"),
    "discrete.result_value_types" = data.frame(result_value_type_id = 1L, result_value_type = "actual"),
    "discrete.sample_fractions" = data.frame(sample_fraction_id = 1L, sample_fraction = "Total"),
    "discrete.reporting_bases" = data.frame(reporting_basis_id = 1L, reporting_basis = "Element"),
    "discrete.protocols_methods" = data.frame(protocol_id = 1L, protocol_name = "Test method"),
    "instruments.instruments" = data.frame(instrument_id = 1L, serial_no = "Test", make = "Test", model = "Test", type = "Test", owner = 1L),
    "field.field_visits" = data.frame(field_visit_id = integer(), location_name = character(),
                                     sub_location_name = character(), start_datetime_MST = as.POSIXct(character()), purpose = character())
  )
  writes <- list()
  transactions <- 0L
  linked_samples <- integer()
  visit <- data.frame(
    field_visit_id = 201L, location_id = 1L, sub_location_id = NA_integer_,
    start_datetime = sample$datetime, share_with = "{public_reader}"
  )
  testthat::local_mocked_bindings(
    dbGetQuery = function(conn, statement, params = NULL, ...) {
      if (grepl("INSERT INTO field.field_visits", statement, fixed = TRUE)) return(data.frame(field_visit_id = 201L))
      if (grepl("SELECT * FROM field.field_visits", gsub("[[:space:]]+", " ", statement), fixed = TRUE)) return(visit)
      if (grepl("FROM field.field_visit_images", statement, fixed = TRUE)) return(data.frame())
      if (grepl("SELECT * FROM discrete.samples", statement, fixed = TRUE)) return(sample)
      if (grepl("SELECT * FROM discrete.results", statement, fixed = TRUE)) return(stored)
      if (grepl("FROM discrete.samples AS s", statement, fixed = TRUE)) return(sample)
      if (grepl("UPDATE discrete.results", statement, fixed = TRUE)) {
        writes[[length(writes) + 1L]] <<- list(sql = statement, params = params)
        return(data.frame(result_id = 501L))
      }
      if (grepl("resolve_matrix_state_id", statement, fixed = TRUE)) return(data.frame(matrix_state_id = 1L))
      if (grepl("get_shareable_principals_for", statement, fixed = TRUE)) return(data.frame(role_name = "public_reader"))
      if (grepl("public.parameter_relationships", statement, fixed = TRUE)) return(data.frame(parameter_id = 1L))
      for (table in names(catalogues)) {
        if (grepl(paste("FROM", table), statement, fixed = TRUE)) return(catalogues[[table]])
      }
      stop("Unexpected field visit query: ", statement)
    },
    dbExecute = function(conn, statement, params, ...) {
      if (!grepl("UPDATE discrete.samples", statement, fixed = TRUE)) stop("Unexpected write: ", statement)
      linked_samples <<- c(linked_samples, params[[2]])
      1L
    },
    dbWithTransaction = function(conn, code, ...) {
      transactions <<- transactions + 1L
      force(code)
    }, .package = "DBI"
  )
  session <- shiny::MockShinySession$new()
  session$userData$AquaCache <- DBI::ANSI()
  shiny::testServer(module$visit, args = list(language = shiny::reactiveValues(language = "en")), session = session, {
    session$flushReact()
    session$setInputs(mode = "add", location = "1", share_with = "public_reader",
                      visit_datetime_start = sample$datetime, timezone = "UTC+00:00")
    session$setInputs(create_field_sample = 1)
    session$setInputs(
      field_sample_datetime = sample$datetime, field_sample_type = "1", field_sample_media = "1",
      field_sample_method = "1", field_sample_owner = "1", field_parameter = "1",
      field_result = 7, field_result_value_type = "1", field_result_condition = "",
      field_sample_fraction = "", field_reporting_basis = "", field_protocol = "",
      field_result_datetime = sample$datetime + 60, stage_field_result = 1
    )
    session$setInputs(save_field_sample = 1)
    expect_length(pending_visit_samples()$new, 1)
    pending <- pending_visit_samples()
    pending$existing <- sample
    pending_visit_samples(pending)
    session$setInputs(pending_visit_samples_rows_selected = 1L)
    expect_identical(sample_editor_target()$kind, "new")
    expect_equal(nrow(pending_field_results()), 1)
    session$setInputs(field_parameter = "1", field_result = 8, field_result_condition = "",
                      field_result_value_type = "1", field_sample_fraction = "",
                      field_reporting_basis = "", field_protocol = "",
                      field_result_datetime = sample$datetime + 120, field_result_note = "new note",
                      stage_field_result = 2)
    expect_equal(nrow(pending_field_results()), 2)
    session$setInputs(stage_field_result = 3)
    expect_equal(nrow(pending_field_results()), 2)
    session$setInputs(save_field_result = 1)
    expect_equal(nrow(pending_visit_samples()$new[[1]]$results), 2)
    expect_length(writes, 0)

    session$setInputs(open_pending_sample = 1)
    expect_equal(nrow(pending_field_results()), 2)
    session$setInputs(pending_visit_samples_rows_selected = 2L)
    expect_identical(sample_editor_target()$kind, "existing")
    expect_equal(nrow(stored_field_results()), 1)
    session$setInputs(field_result_datetime = sample$datetime, stage_field_result = 4)
    expect_equal(nrow(pending_field_results()), 0)
    expect_equal(field_result_conflict()$existing$result, 7)
    expect_match(output$field_result_conflict$html, "Use this replacement in preview", fixed = TRUE)
    session$setInputs(cancel_field_result_overwrite = 1)
    expect_null(field_result_conflict())
    session$setInputs(stage_field_result = 5, confirm_field_result_overwrite = 1)
    expect_equal(nrow(pending_field_results()), 1)
    expect_equal(field_result_overwrites()$result, 7)
    session$setInputs(save_field_result = 2)
    expect_equal(pending_visit_samples()$existing_results[["101"]]$results$result, 8)
    expect_length(writes, 0)

    session$setInputs(open_pending_sample = 2)
    expect_equal(nrow(pending_field_results()), 1)
    session$setInputs(field_result_preview_rows_selected = 1L, remove_staged_field_result = 1)
    expect_equal(nrow(field_result_overwrites()), 0)
    session$setInputs(save_field_result = 3)
    expect_equal(nrow(pending_visit_samples()$existing_results[["101"]]$results), 0)

    visitData$current_visit <- visit
    selected_visit(201L)
    visitData$samples <- sample
    session$setInputs(visit_samples_rows_selected = 1L)
    expect_identical(sample_editor_target()$kind, "saved")
    session$setInputs(stage_field_result = 6)
    session$setInputs(confirm_field_result_overwrite = 2)
    session$setInputs(save_field_result = 4)
    expect_length(writes, 1)
    expect_equal(transactions, 1)
    expect_equal(writes[[1]]$params[[1]], 8)

    selected_visit(NULL)
    visitData$current_visit <- NULL
    proposed <- stored
    proposed$result <- 9
    pending_visit_samples(list(new = list(), existing = sample, existing_results = list(
      "101" = list(results = proposed, overwrites = stored)
    )))
    session$setInputs(add_visit = 1)
    expect_equal(transactions, 2)
    expect_identical(linked_samples, 101L)
    expect_length(writes, 2)
    expect_equal(writes[[2]]$params[[1]], 9)
    expect_identical(selected_visit(), 201L)
    expect_length(pending_visit_samples()$existing_results, 0)
  })
})
