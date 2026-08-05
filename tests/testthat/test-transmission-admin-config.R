transmission_admin_environment <- function() {
  module_env <- new.env(parent = asNamespace("shiny"))
  sys.source(
    testthat::test_path(
      "..",
      "..",
      "inst",
      "apps",
      "YGwater",
      "modules",
      "admin",
      "metadata",
      "manageReferenceTables.R"
    ),
    envir = module_env
  )
  module_env
}

test_that("patch 56 transmission tables have matching admin configurations", {
  module_env <- transmission_admin_environment()
  configs <- module_env$reference_table_configs()

  mapping <- configs$transmission_timeseries_mappings
  expect_equal(mapping$schema, "continuous")
  expect_equal(mapping$table, "transmission_timeseries_mappings")
  expect_false(identical(mapping$editable, FALSE))
  expect_equal(
    vapply(mapping$columns, `[[`, character(1), "name"),
    c(
      "transmission_route_id",
      "source_field",
      "timeseries_id",
      "value_multiplier",
      "value_offset",
      "missing_values",
      "mapping_config",
      "enabled",
      "note"
    )
  )
  timeseries_field <- mapping$columns[[
    match(
      "timeseries_id",
      vapply(mapping$columns, `[[`, character(1), "name")
    )
  ]]
  expect_match(
    timeseries_field$choices_query,
    "WHERE ts.timeseries_type = 'basic'",
    fixed = TRUE
  )

  history <- configs$transmission_import_runs
  expect_equal(history$schema, "continuous")
  expect_equal(history$table, "transmission_import_runs")
  expect_identical(history$editable, FALSE)
  expect_equal(history$row_limit, 1000L)
  expect_setequal(
    history$order_by_desc,
    c("started", "transmission_import_run_id")
  )
})

test_that("transmission import history renders as read-only", {
  module_env <- transmission_admin_environment()
  configs <- module_env$reference_table_configs()

  history_ui <- module_env$referenceTableManagerUI(
    "history",
    configs$transmission_import_runs
  )
  history_html <- htmltools::renderTags(history_ui)$html
  expect_match(history_html, "history-reload", fixed = TRUE)
  expect_false(grepl("history-save", history_html, fixed = TRUE))
  expect_false(grepl("history-new_record", history_html, fixed = TRUE))

  mapping_ui <- module_env$referenceTableManagerUI(
    "mapping",
    configs$transmission_timeseries_mappings
  )
  mapping_html <- htmltools::renderTags(mapping_ui)$html
  expect_match(mapping_html, "mapping-save", fixed = TRUE)
  expect_match(mapping_html, "mapping-new_record", fixed = TRUE)
})

test_that("transmission-linked app wiring includes patch 56 relationships", {
  server_text <- paste(readLines(
    testthat::test_path(
      "..",
      "..",
      "inst",
      "apps",
      "YGwater",
      "server.R"
    ),
    warn = FALSE
  ), collapse = "\n")
  deployment_text <- paste(readLines(
    testthat::test_path(
      "..",
      "..",
      "inst",
      "apps",
      "YGwater",
      "modules",
      "admin",
      "field",
      "deploy_recover.R"
    ),
    warn = FALSE
  ), collapse = "\n")

  expect_match(
    server_text,
    "continuous.transmission_timeseries_mappings",
    fixed = TRUE
  )
  expect_match(
    server_text,
    "continuous.transmission_import_runs",
    fixed = TRUE
  )
  expect_match(
    deployment_text,
    "transmission_mapping_count",
    fixed = TRUE
  )
  expect_match(
    deployment_text,
    "transmission_import_run_count",
    fixed = TRUE
  )
})

test_that("patch 56 transmission admin queries execute on testdb", {
  con <- test_AquaConnect()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  patch_objects <- DBI::dbGetQuery(
    con,
    "SELECT
       to_regclass('continuous.transmission_timeseries_mappings') IS NOT NULL
         AS has_mappings,
       to_regclass('continuous.transmission_import_runs') IS NOT NULL
         AS has_import_runs"
  )
  skip_if_not(all(unlist(patch_objects[1, ], use.names = FALSE)))

  module_env <- transmission_admin_environment()
  configs <- module_env$reference_table_configs()
  mapping <- configs$transmission_timeseries_mappings
  history <- configs$transmission_import_runs

  mapping_choices <- module_env$reference_load_choices(
    con,
    mapping$columns
  )
  expect_setequal(
    names(mapping_choices),
    c("transmission_route_id", "timeseries_id")
  )

  mapping_records <- module_env$reference_fetch_records(con, mapping)
  history_records <- module_env$reference_fetch_records(con, history)
  expect_s3_class(mapping_records, "data.frame")
  expect_s3_class(history_records, "data.frame")
  expect_lte(nrow(history_records), history$row_limit)
})
