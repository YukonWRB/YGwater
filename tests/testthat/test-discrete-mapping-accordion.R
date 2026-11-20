test_that("parsed previews open mappings only when file values need attention", {
  env <- new.env(parent = asNamespace("YGwater"))
  sys.source(
    system.file(
      "apps/aqualink/modules/admin/discreteData/addDiscData.R",
      package = "YGwater"
    ),
    env
  )
  statements <- as.list(body(env$addDiscData)[[2]][[3]][[3]])[-1]
  handler <- Filter(
    function(x) {
      is.call(x) &&
        identical(x[[1]], quote(observeEvent)) &&
        identical(x[[2]], quote(list(data$preview_base, data$preview_is_stale)))
    },
    statements
  )[[1]]
  changes <- list()
  local_mocked_bindings(
    send_panel_message = function(id, session, method, values) {
      # Keep bslib's public API validation: set() rejects empty vectors and
      # close(..., TRUE) is required when no sections need attention.
      values <- if (method == "close" && isTRUE(values)) {
        character()
      } else {
        unlist(values, use.names = FALSE)
      }
      changes[[length(changes) + 1L]] <<- list(id = id, values = values)
    },
    .package = "bslib"
  )
  server <- function(input, output, session) {
    data <- reactiveValues(
      df = addDiscData_empty_table(),
      preview_base = addDiscData_empty_table(),
      preview_is_stale = FALSE,
      preview_profile_key = NULL
    )
    eval(handler)
  }
  environment(server) <- new.env(parent = env)
  environment(server)$handler <- handler
  shiny::testServer(server, {
    session$flushReact()
    expect_length(changes, 0L)
    mapped <- env$addDiscData_empty_table()[NA_integer_, ]
    mapped$source_code <- "LAB"
    mapped$parameter_id <- 1L
    mapped$source_location_name <- "Station A"
    mapped$location_id <- 1L
    # Each case completes a parse, including identical data on a later reparse.
    parse_preview <- function(rows) {
      data$preview_is_stale <- TRUE
      session$flushReact()
      data$df <- rows
      data$preview_base <- rows
      data$preview_profile_key <- "LAB/workbook"
      data$preview_is_stale <- FALSE
      session$flushReact()
      tail(changes, 1L)[[1]]$values
    }
    expect_identical(parse_preview(mapped), character())
    expect_identical(
      changes[[length(changes) - 1L]],
      list(id = "mapping_sections", values = character())
    )
    for (field in c("parameter_id", "location_id", "source_result_flag")) {
      rows <- mapped
      rows[[field]] <- if (field == "source_result_flag") "U" else NA_integer_
      expect_identical(parse_preview(rows), "mappings")
      expect_identical(tail(changes, 1L)[[1]]$id, "mapping_accordion")
      expect_identical(
        changes[[length(changes) - 1L]]$values,
        switch(
          field,
          parameter_id = "parameters",
          location_id = "locations",
          source_result_flag = "flags"
        )
      )
    }
    rows$result_flag_action <- "note_only"
    expect_identical(parse_preview(rows), character())
    # Locationless blanks, empty source values and manual sample shells are
    # intentional; none should cause the mapping panel to open.
    rows <- mapped
    rows$source_location_name <- "Field Blank"
    rows$location_id <- NA_integer_
    expect_identical(parse_preview(rows), character())
    rows$source_location_name <- ""
    expect_identical(parse_preview(rows), character())
    rows$source_code <- "YGwater-manual"
    rows$parameter_id <- NA_integer_
    expect_identical(parse_preview(rows), character())
    expect_identical(parse_preview(env$addDiscData_empty_table()), character())
    expect_identical(parse_preview(mapped), character())
    expect_identical(parse_preview(mapped), character())
    rows <- mapped
    rows$parameter_id <- NA_integer_
    rows$location_id <- NA_integer_
    rows$source_result_flag <- "NEW"
    expect_identical(parse_preview(rows), "mappings")
    expect_identical(
      changes[[length(changes) - 1L]]$values,
      c("parameters", "locations", "flags")
    )
    count <- length(changes)
    data$df$parameter_id <- NA_integer_
    session$flushReact()
    expect_length(changes, count)
    # Starting a parse does not change the panel using the previous workbook.
    data$preview_is_stale <- TRUE
    session$flushReact()
    expect_length(changes, count)
  })
})

discrete_mapping_panels_fixture <- function() {
  env <- new.env(parent = asNamespace("YGwater"))
  for (file in c("chemistry_form.R", "discrete_protocols.R", "addDiscData.R")) {
    sys.source(
      system.file(
        "apps/aqualink/modules/admin/discreteData",
        file,
        package = "YGwater"
      ),
      env
    )
  }
  statements <- as.list(body(env$addDiscData)[[2]][[3]][[3]])[-1]
  wanted <- c(
    "mapping_rows",
    "selected_mapping_row",
    "selected_parameter_mapping",
    "mapping_editor",
    "location_mapping_rows",
    "result_flag_mapping_rows",
    "selected_result_flag_mapping",
    "result_flag_mapping_editor",
    "mapping_available"
  )
  env$panel_expressions <- Filter(
    function(x) {
      is.call(x) &&
        identical(x[[1]], quote(`<-`)) &&
        tail(as.character(x[[2]]), 1L) %in% wanted
    },
    statements
  )
  env$initial_rows <- env$addDiscData_empty_table()[rep(NA_integer_, 3L), ]
  env$initial_rows$source_code <- "LAB"
  env$initial_rows$source_parameter_code <- c("P", "Q", "P")
  env$initial_rows$parameter_id <- c(1L, NA_integer_, 1L)
  env$initial_rows$mapping_status <- c("mapped", "unmapped", "mapped")
  env$initial_rows$source_location_name <- c(
    "Station A",
    "Station B",
    "Station A"
  )
  env$initial_rows$location_id <- c(1L, NA_integer_, NA_integer_)
  env$initial_rows$source_result_flag <- c("HTDC", "NEW", "HTDC")
  env$initial_rows$source_result_flag_column <- "Qual"
  env$initial_rows$result_flag_action <- c(
    "note_only",
    NA_character_,
    NA_character_
  )
  env$initial_rows$source_matching_values <- "{}"
  server <- function(input, output, session) {
    ns <- session$ns
    con <- "unusable captured connection"
    data <- reactiveValues(
      df = initial_rows,
      preview_profile_key = "LAB/workbook",
      preview_mapping_snapshot = list(mappings = list(result_flags = list()))
    )
    selected_profile <- function() {
      data.frame(source_code = "LAB", profile_code = "workbook")
    }
    params <- function() {
      data.frame(
        parameter_id = 1L,
        param_name = "Acenaphthene",
        unit_liquid = "mg/L",
        unit_solid = NA_character_,
        unit_gas = NA_character_
      )
    }
    matrix_states <- data.frame(
      matrix_state_id = 1L,
      matrix_state_name = "Liquid"
    )
    sample_fractions <- data.frame(
      sample_fraction_id = 1L,
      sample_fraction = "Total"
    )
    result_types <- data.frame(result_type_id = 2L, result_type = "Lab")
    result_value_types <- data.frame(
      result_value_type_id = 1L,
      result_value_type = "Direct"
    )
    reporting_bases <- data.frame(
      reporting_basis_id = 1L,
      reporting_basis = "As reported"
    )
    chemistry_catalogues <- list()
    protocols_methods <- function() {
      data.frame(protocol_method_id = 21L, protocol_name = "EPA 8270")
    }
    result_conditions <- data.frame(
      result_condition_id = 1L,
      result_condition = "Below limit"
    )
    for (expr in panel_expressions) {
      eval(expr)
    }
  }
  environment(server) <- env
  list(env = env, server = server)
}

test_that("each mapping table can show unresolved or already mapped values", {
  fixture <- discrete_mapping_panels_fixture()
  shiny::testServer(fixture$server, {
    session$flushReact()
    expect_identical(mapping_rows()$source_parameter_code, "Q")
    expect_setequal(
      location_mapping_rows()$source_location_name,
      c("Station A", "Station B")
    )
    expect_true(all(is.na(location_mapping_rows()$location_id)))
    expect_setequal(
      result_flag_mapping_rows()$source_result_flag,
      c("HTDC", "NEW")
    )
    expect_true(all(is.na(result_flag_mapping_rows()$result_flag_action)))
    session$setInputs(
      show_all_mappings = TRUE,
      show_all_location_mappings = TRUE,
      show_all_flag_mappings = TRUE
    )
    expect_equal(nrow(mapping_rows()), 2L)
    expect_equal(nrow(location_mapping_rows()), 2L)
    expect_equal(nrow(result_flag_mapping_rows()), 2L)
    # A mapped occurrence preceding an unresolved occurrence of the same source
    # value must not hide the unresolved one in the default view.
    data$df <- data$df[1:2, ]
    session$setInputs(
      show_all_mappings = FALSE,
      show_all_location_mappings = FALSE,
      show_all_flag_mappings = FALSE
    )
    expect_identical(location_mapping_rows()$source_location_name, "Station B")
    expect_identical(result_flag_mapping_rows()$source_result_flag, "NEW")
    session$setInputs(
      show_all_location_mappings = TRUE,
      show_all_flag_mappings = TRUE
    )
    expect_identical(location_mapping_rows()$location_id, c(1L, NA_integer_))
    expect_setequal(
      result_flag_mapping_rows()$result_flag_action,
      c("note_only", NA_character_)
    )
  })
})

test_that("parameter editors identify the selected method and default to its effective scope", {
  fixture <- discrete_mapping_panels_fixture()
  shiny::testServer(fixture$server, {
    data$df$source_parameter_code <- "Acenaphthene"
    data$df$source_unit <- c("ug/L", "ug/L", "mg/L")
    data$df$source_matching_values <- c(
      '{"source.Method":"FAKE"}',
      '{"source.Method":"E641A"}',
      '{"source.Method":"FAKE"}'
    )
    data$df$parameter_id <- 1L
    data$df$parameter_mapping_id <- c(100L, 101L, 102L)
    data$preview_mapping_snapshot$mappings$parameters <- list(
      list(
        import_mapping_id = 100L,
        profile_specific = TRUE,
        protocol_method_id = 21L
      ),
      list(import_mapping_id = 101L, profile_specific = FALSE),
      list(import_mapping_id = 102L, profile_specific = FALSE)
    )
    session$setInputs(show_all_mappings = TRUE)
    expect_equal(nrow(mapping_rows()), 3L)
    session$setInputs(
      mapping_summary_rows_selected = match(
        100L,
        mapping_rows()$parameter_mapping_id
      )
    )
    expect_match(output$mapping_editor$html, "Method = FAKE", fixed = TRUE)
    expect_match(
      output$mapping_editor$html,
      'value="profile" selected',
      fixed = TRUE
    )
    expect_match(
      output$mapping_editor$html,
      'value="21" selected',
      fixed = TRUE
    )
    expect_match(
      output$mapping_editor$html,
      'mapping_protocol" data-eval=',
      fixed = TRUE
    )
    session$setInputs(
      mapping_summary_rows_selected = match(
        101L,
        mapping_rows()$parameter_mapping_id
      )
    )
    expect_match(output$mapping_editor$html, "Method = E641A", fixed = TRUE)
    expect_match(
      output$mapping_editor$html,
      'value="source" selected',
      fixed = TRUE
    )
  })
})

test_that("saving one method overrides a broader workbook mapping for that method only", {
  env <- discrete_mapping_panels_fixture()$env
  mappings <- data.frame(
    import_mapping_id = 1:2,
    source_match = c(
      '{"parameter_code":"Acenaphthene","unit":"ug/L"}',
      '{"parameter_code":"Acenaphthene","unit":"ug/L","source.Method":"FAKE"}'
    ),
    profile_specific = c(TRUE, FALSE),
    priority = c(10L, 50L),
    parameter_id = 263L,
    protocol_method_id = NA_integer_,
    result_type_id = 2L,
    sample_fraction_id = NA_integer_,
    result_value_type_id = 1L,
    reporting_basis_id = NA_integer_,
    matrix_state_id = 1L,
    conversion = 1,
    result_offset = 0
  )
  rows <- env$initial_rows
  rows$source_parameter_code <- "Acenaphthene"
  rows$source_unit <- c("ug/L", "ug/L", "mg/L")
  rows$source_matching_values <- c(
    '{"source.Method":"FAKE"}',
    '{"source.Method":"E641A"}',
    '{"source.Method":"FAKE"}'
  )
  rows$source_result <- 2
  env$addDiscData_fetch_mappings <- function(...) mappings
  env$addDiscData_fetch_result_flag_mappings <- function(...) data.frame()
  local_mocked_bindings(
    upsertImportParameterMappings = function(
      con,
      mappings,
      match_columns,
      profile_code,
      ...
    ) {
      row <- as.list(mappings)
      row$source_match <- as.character(jsonlite::toJSON(
        row[match_columns],
        auto_unbox = TRUE
      ))
      row$profile_specific <- identical(profile_code, "xlr")
      row
    },
    .package = "AquaCache"
  )
  initial <- env$addDiscData_apply_mappings(rows, NULL, "xlr")
  expect_identical(initial$parameter_mapping_id, c(1L, 1L, NA_integer_))
  saved <- env$addDiscData_upsert_mapping(
    con = NULL,
    source_code = "ALS",
    source_name = "ALS",
    parameter_code = rows$source_parameter_code[[1]],
    unit = rows$source_unit[[1]],
    source_matching_values = rows$source_matching_values[[1]],
    parameter_id = 263L,
    protocol_method_id = 21L,
    result_type_id = 2L,
    sample_fraction_id = NA_integer_,
    result_value_type_id = 1L,
    reporting_basis_id = NA_integer_,
    matrix_state_id = 1L,
    conversion = 0.001,
    result_offset = 0,
    note = NA_character_,
    profile_code = "xlr",
    applied_mapping = as.list(mappings[1, ])
  )
  expect_true(saved$profile_specific)
  expect_identical(saved$priority, 10L)
  expect_identical(jsonlite::fromJSON(saved$source_match)$source.Method, "FAKE")
  saved$import_mapping_id <- 3L
  mappings <- rbind(mappings, as.data.frame(saved[names(mappings)]))
  refreshed <- env$addDiscData_apply_mappings(rows, NULL, "xlr")
  expect_identical(refreshed$parameter_mapping_id, c(3L, 1L, NA_integer_))
  expect_equal(refreshed$result, c(0.002, 2, 2))
  expect_identical(refreshed$protocol_method_id, c(21L, NA_integer_, NA_integer_))
})

test_that("flag selection renders from preview metadata without a connection", {
  fixture <- discrete_mapping_panels_fixture()
  local_mocked_bindings(
    getImportResultFlagMappings = function(...) {
      stop("bad_weak_ptr: selection used a database connection")
    },
    .package = "AquaCache"
  )
  shiny::testServer(fixture$server, {
    data$df <- data$df[1:2, ]
    mappings <- data.frame(
      source_flag_value = "HTDC",
      source_flag_column = c(NA, "Qual", NA, "Qual", "Qual"),
      profile_specific = c(FALSE, FALSE, TRUE, TRUE, TRUE),
      priority = c(1L, 1L, 1L, 50L, 0L),
      import_result_flag_mapping_id = 1:5,
      active = c(TRUE, TRUE, TRUE, TRUE, FALSE),
      result_action = c(
        "keep_result",
        "skip_result",
        "set_result_null",
        "note_only",
        "reject_row"
      ),
      result_condition_id = 1L,
      result_condition_value_source = "literal",
      result_condition_value_literal = 2.5,
      note_template = "Selected flag note"
    )
    data$preview_mapping_snapshot <- list(
      mappings = list(
        result_flags = lapply(1:5, function(i) as.list(mappings[i, ]))
      )
    )
    session$setInputs(show_all_flag_mappings = TRUE)
    session$setInputs(
      result_flag_mapping_summary_rows_selected = match(
        "HTDC",
        result_flag_mapping_rows()$source_result_flag
      )
    )
    row <- selected_result_flag_mapping()
    expect_identical(row$mapped_action, "note_only")
    expect_identical(row$mapped_condition, 1L)
    expect_equal(row$mapped_threshold, 2.5)
    html <- output$result_flag_mapping_editor$html
    expect_match(html, 'value="note_only" selected', fixed = TRUE)
    expect_match(html, "Selected flag note", fixed = TRUE)
    expect_match(html, "2.5", fixed = TRUE)
    session$setInputs(
      result_flag_mapping_summary_rows_selected = match(
        "NEW",
        result_flag_mapping_rows()$source_result_flag
      )
    )
    expect_true(is.na(selected_result_flag_mapping()$mapped_action))
    expect_match(
      output$result_flag_mapping_editor$html,
      'value="keep_result" selected',
      fixed = TRUE
    )
  })
})

test_that("mapping controls stay available for mapped previews and hide without source values", {
  fixture <- discrete_mapping_panels_fixture()
  shiny::testServer(fixture$server, {
    expect_identical(output$mapping_available, "true")
    data$df <- data$df[1L, ]
    session$flushReact()
    expect_identical(output$mapping_available, "true")
    data$df$source_code <- "YGwater-manual"
    session$flushReact()
    expect_identical(output$mapping_available, "false")
    data$df <- fixture$env$addDiscData_empty_table()
    session$flushReact()
    expect_identical(output$mapping_available, "false")
  })
})
