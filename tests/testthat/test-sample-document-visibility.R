test_that("document synchronization reads each linked sample and preserves public isolation", {
  skip_if(Sys.getenv("YGWATER_TEST_IMPORT_DB") != "true", "testdb integration opt-in required")
  con <- AquaCache::AquaConnect(name = "testdb", host = "10.250.12.154",
    check = FALSE, silent = TRUE)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbBegin(con)
  on.exit(DBI::dbRollback(con), add = TRUE, after = FALSE)
  groups <- DBI::dbGetQuery(con,
    "SELECT rolname FROM pg_roles WHERE NOT rolcanlogin AND rolname !~ '^pg_'
     AND NOT pg_has_role('public_reader',oid,'member') ORDER BY rolname LIMIT 2")$rolname
  stopifnot(length(groups) == 2L)
  samples <- DBI::dbGetQuery(con, "SELECT sample_id FROM discrete.samples ORDER BY sample_id LIMIT 2")$sample_id
  stopifnot(length(samples) == 2L)
  type <- DBI::dbGetQuery(con, "SELECT document_type_en FROM files.document_types LIMIT 1")$document_type_en
  file <- tempfile(fileext = ".xlsx")
  on.exit(unlink(file), add = TRUE)
  documents <- integer()
  for (i in 1:2) {
    writeBin(charToRaw(paste("Synthetic workbook visibility", i, Sys.getpid())), file)
    documents <- c(documents, AquaCache::insertACDocument(file,
      paste0("visibility_", i, "_", Sys.getpid()), type,
      "Synthetic workbook for sample document visibility test.", con = con)$new_document_id)
    if (i == 1L) {
      reused <- AquaCache::insertACDocument(file,
        paste0("visibility_duplicate_", Sys.getpid()), type,
        "The identical file uploaded under another name.", con = con)$new_document_id
      expect_equal(reused, documents[[1]])
    }
  }
  DBI::dbExecute(con, "UPDATE discrete.samples SET share_with=ARRAY[$1] WHERE sample_id=$2",
    params = list(groups[[1]], samples[[1]]))
  DBI::dbExecute(con, "UPDATE discrete.samples SET share_with=ARRAY['public_reader'] WHERE sample_id=$1",
    params = list(samples[[2]]))
  DBI::dbExecute(con,
    "INSERT INTO discrete.sample_documents(sample_id,document_id) VALUES ($1,$3),($2,$3),($2,$4)",
    params = list(samples[[1]], samples[[2]], documents[[1]], documents[[2]]))
  sync_sample_document_visibility(con, document_ids = documents)
  stored <- DBI::dbGetQuery(con,
    "SELECT document_id,array_to_json(share_with)::text AS visibility FROM files.documents
     WHERE document_id IN ($1,$2) ORDER BY document_id", params = as.list(documents))
  expect_identical(jsonlite::fromJSON(stored$visibility[[1]]), groups[[1]])
  expect_identical(jsonlite::fromJSON(stored$visibility[[2]]), "public_reader")
  DBI::dbExecute(con, "SET LOCAL ROLE public_reader")
  visible <- DBI::dbGetQuery(con,
    "SELECT document_id FROM files.documents WHERE document_id IN ($1,$2)", params = as.list(documents))
  expect_equal(visible$document_id, documents[[2]])
  DBI::dbExecute(con, "RESET ROLE")
  source <- paste0("partial_visibility_", Sys.getpid())
  profile_id <- AquaCache::upsertImportProfile(con, source, "workbook", "Visibility test workbook")
  source_id <- AquaCache::getImportProfiles(con, source)$import_source_id[[1]]
  run <- AquaCache::createImportRun(con, source_id, profile_id,
    source_document_id = documents[[1]], summary = list(omitted_samples = 1L))
  AquaCache::completeImportRun(con, run, summary = list(omitted_samples = 1L))
  DBI::dbExecute(con, "UPDATE discrete.samples SET share_with=ARRAY['public_reader'] WHERE sample_id=$1",
    params = list(samples[[1]]))
  sync_sample_document_visibility(con, sample_ids = samples)
  broadened <- DBI::dbGetQuery(con,
    "SELECT array_to_json(share_with)::text AS visibility FROM files.documents WHERE document_id=$1",
    params = list(documents[[1]]))
  expect_identical(jsonlite::fromJSON(broadened$visibility[[1]]), "public_reader")
  DBI::dbExecute(con, "UPDATE discrete.samples SET share_with=ARRAY[$1] WHERE sample_id=$2",
    params = list(groups[[1]], samples[[1]]))
  DBI::dbExecute(con, "UPDATE discrete.samples SET share_with=ARRAY[$1] WHERE sample_id=$2",
    params = list(groups[[2]], samples[[2]]))
  expect_error(sync_sample_document_visibility(con, sample_ids = samples), "no access group shared")
})
