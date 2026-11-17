reference_chemistry_test_environment <- function(
  parent = asNamespace("YGwater")
) {
  env <- new.env(parent = parent)
  sys.source(
    system.file(
      "apps/aqualink/modules/admin/metadata/manageReferenceTables.R",
      package = "YGwater"
    ),
    env
  )
  env
}

test_that("reference tabs wait for their schema and render loaded empty catalogues", {
  editor <- reference_chemistry_test_environment(parent = asNamespace("shiny"))
  # Run the complete wrapper so all four sibling module namespaces participate.
  editor$application_notifications_ui <- function(...) NULL
  editor$reference_load_choices <- function(con, fields) {
    fields <- Filter(function(field) identical(field$input, "select"), fields)
    stats::setNames(
      lapply(fields, function(field) {
        if (!is.null(field$choices)) {
          field$choices
        } else {
          data.frame(value = "1", label = "Fixture unit")
        }
      }),
      vapply(fields, `[[`, character(1), "name")
    )
  }
  records <- new.env(parent = emptyenv())
  records$empty <- FALSE
  editor$reference_fetch_records <- function(con, config) {
    row <- editor$reference_empty_row(config$columns)
    row[[config$pk]] <- 1L
    row[[config$order_by]] <- config$title
    if (records$empty) row[FALSE, , drop = FALSE] else row
  }
  editor$fetch_parameter_records <- editor$reference_fetch_records
  editor$fetch_parameter_relationship_choices <- function(con) {
    list(
      groups = data.frame(value = "1", label = "Fixture group"),
      sub_groups = data.frame(value = "1", label = "Fixture subgroup")
    )
  }
  editor$update_parameter_suggestions <- function(...) NULL
  messages <- list()
  session <- shiny::MockShinySession$new()
  session$sendInputMessage <- function(inputId, message) {
    messages[[inputId]] <<- message
  }
  server <- function(input, output, session) {
    editor$manageParameters(
      "parameters",
      shiny::reactiveValues(language = "en")
    )
  }
  shiny::testServer(server, session = session, {
    ids <- c(
      "parameters",
      paste0("parameters_", names(editor$parameter_suggestion_tables()))
    )
    session$flushReact()
    for (id in ids) {
      # Before binding, never emit DT's NULL widget for a zero-column frame.
      expect_error(
        output[[paste0(id, "-records_table")]],
        class = "shiny.silent.error"
      )
    }
    expect_length(messages, 0L)
    ready <- stats::setNames(
      rep(list("bound"), length(ids)),
      paste0(ids, "-ui_ready")
    )
    do.call(session$setInputs, ready)
    for (id in ids) {
      widget <- jsonlite::fromJSON(
        output[[paste0(id, "-records_table")]],
        simplifyVector = FALSE
      )
      expect_type(widget$x, "list")
      expect_match(
        output[[paste0(id, "-table_heading")]]$html,
        "1 record(s)",
        fixed = TRUE
      )
    }
    expect_match(
      messages[["field_units_liquid"]]$options,
      "Fixture unit",
      fixed = TRUE
    )
    expect_match(
      messages[["relationship_group_id"]]$options,
      "Fixture group",
      fixed = TRUE
    )
    parameter_widget <- jsonlite::fromJSON(
      output[["parameters-records_table"]],
      simplifyVector = FALSE
    )
    expect_true(parameter_widget$x$options$serverSide)
    expect_true(parameter_widget$x$options$stateSave)

    # A legitimate empty result must still render its columns and allow inserts.
    records$empty <- TRUE
    reload <- stats::setNames(
      rep(list(1L), length(ids)),
      paste0(ids, "-reload")
    )
    do.call(session$setInputs, reload)
    for (id in ids) {
      widget <- jsonlite::fromJSON(
        output[[paste0(id, "-records_table")]],
        simplifyVector = FALSE
      )
      expect_type(widget$x, "list")
      expect_match(
        output[[paste0(id, "-table_heading")]]$html,
        "0 record(s)",
        fixed = TRUE
      )
    }
  })
})

test_that("invalid condition requirements produce recoverable form errors", {
  editor <- reference_chemistry_test_environment()
  fields <- editor$reference_table_configs()$parameters$columns
  expect_error(
    editor$reference_collect_values(
      list(field_analysis_conditions_required_keys_by_matrix = "{broken"),
      fields
    ),
    "JSON object"
  )
  expect_error(
    editor$reference_collect_values(
      list(field_analysis_conditions_required_keys = "invalid key"),
      fields
    ),
    "separated by commas"
  )
  values <- editor$reference_collect_values(
    list(
      field_analysis_conditions_required_keys = "mass_basis, gas_basis, mass_basis",
      field_analysis_conditions_required_keys_by_matrix = '{"solid":["mass_basis"]}'
    ),
    fields
  )
  expect_identical(
    values$analysis_conditions_required_keys,
    "{mass_basis,gas_basis}"
  )
  expect_identical(
    values$analysis_conditions_required_keys_by_matrix,
    '{"solid":["mass_basis"]}'
  )
})

test_that("chemistry reference edits preserve definitions and ordered advisory associations", {
  skip_if(
    Sys.getenv("YGWATER_CHEMISTRY_TEST_DB") != "testdb",
    "explicit dev testdb opt-in required"
  )
  editor <- reference_chemistry_test_environment()
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = "testdb",
    host = "10.250.12.154",
    port = Sys.getenv("aquacachePort", "5432"),
    user = Sys.getenv("aquacacheAdminUser"),
    password = Sys.getenv("aquacacheAdminPass"),
    connect_timeout = 5
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  identity <- DBI::dbGetQuery(
    con,
    "SELECT current_database() AS db, inet_server_addr()::TEXT AS host,
    (SELECT version FROM information.version_info WHERE item='Last patch number') AS patch"
  )
  stopifnot(
    identity$db == "testdb",
    identity$host == "10.250.12.154/32",
    identity$patch == "65"
  )
  DBI::dbBegin(con)
  on.exit(DBI::dbRollback(con), add = TRUE, after = FALSE)
  configs <- editor$reference_table_configs()
  selected <- list()
  for (catalogue in names(editor$parameter_suggestion_tables())) {
    config <- configs[[catalogue]]
    rows <- editor$reference_fetch_records(con, config)
    row <- rows[1, , drop = FALSE]
    values <- as.list(row[vapply(config$columns, `[[`, character(1), "name")])
    original_label <- row[[config$order_by]]
    values[[config$order_by]] <- "A different definition needs a new ID"
    values$description_fr <- "Verification temporaire"
    values$active <- FALSE
    editor$reference_save_record(
      con,
      config,
      values,
      row[[config$pk]],
      names(row)
    )
    saved <- DBI::dbGetQuery(
      con,
      sprintf("SELECT * FROM discrete.%s WHERE %s=$1", catalogue, config$pk),
      params = list(row[[config$pk]][[1]])
    )
    expect_identical(saved[[config$order_by]], original_label)
    expect_identical(saved$description_fr, "Verification temporaire")
    expect_false(saved$active)
    selected[[paste0("suggest_", catalogue)]] <- as.character(rev(head(
      rows[[config$pk]],
      2
    )))
  }
  parameter_id <- DBI::dbGetQuery(
    con,
    "SELECT parameter_id FROM public.parameters ORDER BY parameter_id LIMIT 1"
  )$parameter_id
  editor$save_parameter_suggestions(con, parameter_id, selected)
  for (catalogue in names(editor$parameter_suggestion_tables())) {
    rows <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT %s AS id, display_order FROM public.%s WHERE parameter_id=$1 ORDER BY display_order",
        configs[[catalogue]]$pk,
        editor$parameter_suggestion_tables()[[catalogue]]
      ),
      params = list(parameter_id)
    )
    expect_identical(
      as.character(rows$id),
      selected[[paste0("suggest_", catalogue)]]
    )
    expect_equal(rows$display_order, seq_len(nrow(rows)))
  }
  editor$save_parameter_suggestions(con, parameter_id, list())
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT count(*)::INTEGER AS n FROM public.parameter_target_suggestions WHERE parameter_id=$1",
      params = list(parameter_id)
    )$n,
    0L
  )
})
