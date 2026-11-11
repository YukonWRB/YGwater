reference_chemistry_test_environment <- function() {
  env <- new.env(parent = asNamespace("YGwater"))
  sys.source(system.file("apps/aqualink/modules/admin/metadata/manageReferenceTables.R",
    package = "YGwater"), env)
  env
}

test_that("invalid condition requirements produce recoverable form errors", {
  editor <- reference_chemistry_test_environment()
  fields <- editor$reference_table_configs()$parameters$columns
  expect_error(editor$reference_collect_values(
    list(field_analysis_conditions_required_keys_by_matrix = "{broken"), fields), "JSON object")
  expect_error(editor$reference_collect_values(
    list(field_analysis_conditions_required_keys = "invalid key"), fields), "separated by commas")
  values <- editor$reference_collect_values(list(
    field_analysis_conditions_required_keys = "mass_basis, gas_basis, mass_basis",
    field_analysis_conditions_required_keys_by_matrix = '{"solid":["mass_basis"]}'), fields)
  expect_identical(values$analysis_conditions_required_keys, "{mass_basis,gas_basis}")
  expect_identical(values$analysis_conditions_required_keys_by_matrix, '{"solid":["mass_basis"]}')
})

test_that("chemistry reference edits preserve definitions and ordered advisory associations", {
  skip_if(Sys.getenv("YGWATER_CHEMISTRY_TEST_DB") != "testdb", "explicit dev testdb opt-in required")
  editor <- reference_chemistry_test_environment()
  con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "testdb", host = "10.250.12.154",
    port = Sys.getenv("aquacachePort", "5432"), user = Sys.getenv("aquacacheAdminUser"),
    password = Sys.getenv("aquacacheAdminPass"), connect_timeout = 5)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  identity <- DBI::dbGetQuery(con, "SELECT current_database() AS db, inet_server_addr()::TEXT AS host,
    (SELECT version FROM information.version_info WHERE item='Last patch number') AS patch")
  stopifnot(identity$db == "testdb", identity$host == "10.250.12.154/32", identity$patch == "65")
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
    editor$reference_save_record(con, config, values, row[[config$pk]], names(row))
    saved <- DBI::dbGetQuery(con, sprintf("SELECT * FROM discrete.%s WHERE %s=$1", catalogue, config$pk),
      params = list(row[[config$pk]][[1]]))
    expect_identical(saved[[config$order_by]], original_label)
    expect_identical(saved$description_fr, "Verification temporaire")
    expect_false(saved$active)
    selected[[paste0("suggest_", catalogue)]] <- as.character(rev(head(rows[[config$pk]], 2)))
  }
  parameter_id <- DBI::dbGetQuery(con, "SELECT parameter_id FROM public.parameters ORDER BY parameter_id LIMIT 1")$parameter_id
  editor$save_parameter_suggestions(con, parameter_id, selected)
  for (catalogue in names(editor$parameter_suggestion_tables())) {
    rows <- DBI::dbGetQuery(con, sprintf(
      "SELECT %s AS id, display_order FROM public.%s WHERE parameter_id=$1 ORDER BY display_order",
      configs[[catalogue]]$pk, editor$parameter_suggestion_tables()[[catalogue]]), params = list(parameter_id))
    expect_identical(as.character(rows$id), selected[[paste0("suggest_", catalogue)]])
    expect_equal(rows$display_order, seq_len(nrow(rows)))
  }
  editor$save_parameter_suggestions(con, parameter_id, list())
  expect_equal(DBI::dbGetQuery(con,
    "SELECT count(*)::INTEGER AS n FROM public.parameter_target_suggestions WHERE parameter_id=$1",
    params = list(parameter_id))$n, 0L)
})
