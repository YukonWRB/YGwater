test_that("provenance distinguishes restricted workbooks from unrecorded imports", {
  skip_if(Sys.getenv("YGWATER_TEST_IMPORT_DB") != "true", "testdb integration opt-in required")
  con <- AquaCache::AquaConnect(name = "testdb", host = "10.250.12.154", check = FALSE, silent = TRUE)
  on.exit(DBI::dbDisconnect(con))
  ids <- DBI::dbGetQuery(con,
    "SELECT r.result_id FROM discrete.results r JOIN discrete.import_runs ir USING(import_run_id)
     JOIN discrete.import_sources src ON src.import_source_id=ir.import_source_id
     WHERE src.source_code='patch69_worker_review' ORDER BY result_id")$result_id
  skip_if(!length(ids), "Run dev/review-upload-integration.R to provide the browser fixtures")
  recorded <- discrete_result_provenance(con, tail(ids, 1L))
  expect_identical(recorded$provenance_state, "recorded")
  expect_identical(recorded$document_name, "review-partial.xlsx")
  expect_match(recorded$configuration_snapshot, "omitted-sample", fixed = TRUE)
  DBI::dbBegin(con)
  on.exit(DBI::dbRollback(con), add = TRUE, after = FALSE)
  DBI::dbExecute(con, "SET LOCAL ROLE public_reader")
  restricted <- discrete_result_provenance(con, tail(ids, 1L))
  expect_identical(restricted$provenance_state, "restricted")
  expect_true(is.na(restricted$configuration_snapshot))
  expect_true(is.na(restricted$document_name))
  legacy <- DBI::dbGetQuery(con,
    "SELECT r.result_id FROM discrete.results r JOIN discrete.samples s USING(sample_id)
     WHERE r.import_run_id IS NULL ORDER BY result_id LIMIT 1")$result_id
  expect_identical(discrete_result_provenance(con, legacy)$provenance_state, "unrecorded")
  DBI::dbExecute(con, "RESET ROLE")
})
