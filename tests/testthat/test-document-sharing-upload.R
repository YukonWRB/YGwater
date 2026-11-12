test_that("workbook uploads snapshot actual sharing and roll back late failures", {
  skip_if(Sys.getenv("YGWATER_TEST_IMPORT_DB") != "true", "testdb integration opt-in required")
  env <- new.env(parent = asNamespace("YGwater"))
  sys.source(system.file("apps/aqualink/modules/admin/discreteData/addDiscData.R", package = "YGwater"), env)
  con <- AquaCache::AquaConnect(name = "testdb", host = "10.250.12.154", check = FALSE, silent = TRUE)
  on.exit(DBI::dbDisconnect(con))
  stopifnot(DBI::dbGetQuery(con, "SELECT current_database() AS db")$db == "testdb")
  prefix <- paste0("sharing_upload_", Sys.getpid())
  file <- tempfile(pattern = prefix, fileext = ".xlsx")
  writeBin(charToRaw(basename(file)), file)
  on.exit(unlink(file), add = TRUE)
  profile_id <- AquaCache::upsertImportProfile(con, prefix, "workbook", "Sharing regression workbook")
  profile <- AquaCache::getImportProfiles(con, prefix)
  profile <- profile[profile$import_profile_id == profile_id, , drop = FALSE]
  location <- sample_id <- integer()
  on.exit(DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "DELETE FROM discrete.results WHERE import_run_id IN
      (SELECT import_run_id FROM discrete.import_runs WHERE import_profile_id=$1)", params = list(profile_id))
    DBI::dbExecute(con, "DELETE FROM discrete.import_runs WHERE import_profile_id=$1", params = list(profile_id))
    DBI::dbExecute(con, "DELETE FROM files.documents WHERE name=$1", params = list(basename(file)))
    if (length(sample_id)) DBI::dbExecute(con, "DELETE FROM discrete.samples WHERE sample_id=$1", params = list(sample_id))
    if (length(location)) DBI::dbExecute(con, "DELETE FROM public.locations WHERE location_id=$1", params = list(location))
    DBI::dbExecute(con, "DELETE FROM discrete.import_profiles WHERE import_profile_id=$1", params = list(profile_id))
    DBI::dbExecute(con, "DELETE FROM discrete.import_sources WHERE source_code=$1", params = list(prefix))
  }), add = TRUE, after = FALSE)
  group <- DBI::dbGetQuery(con,
    "SELECT rolname FROM pg_roles WHERE NOT rolcanlogin AND rolname !~ '^pg_' ORDER BY rolname LIMIT 1")$rolname
  location <- DBI::dbGetQuery(con,
    "INSERT INTO public.locations(location_code,name,latitude,longitude,share_with)
     VALUES ($1,'Upload sharing regression',60,-135,ARRAY[$2]) RETURNING location_id",
    params = list(prefix, group))$location_id
  sample_id <- DBI::dbGetQuery(con,
    "INSERT INTO discrete.samples(location_id,media_id,datetime,collection_method,sample_type,owner)
     SELECT $1,media_id,'2026-01-01'::timestamptz,collection_method,sample_type,owner
     FROM discrete.samples ORDER BY sample_id LIMIT 1 RETURNING sample_id", params = list(location))$sample_id
  sample <- DBI::dbGetQuery(con,
    "SELECT *,COALESCE(modified,created)::text AS sample_version FROM discrete.samples WHERE sample_id=$1",
    params = list(sample_id))
  parameter <- DBI::dbGetQuery(con,
    "SELECT parameter_id FROM public.parameters WHERE NOT sample_fraction AND NOT reporting_basis
     AND NOT result_speciation AND NOT result_target AND NOT endpoint
     AND cardinality(analysis_conditions_required_keys)=0 AND analysis_conditions_required_keys_by_matrix='{}'::jsonb
     AND units_liquid IS NOT NULL ORDER BY parameter_id LIMIT 1")$parameter_id
  rows <- env$addDiscData_empty_table()[NA_integer_, , drop = FALSE]
  for (name in intersect(names(rows), names(sample))) rows[[name]][1] <- sample[[name]][1]
  rows$existing_sample_id <- sample_id
  rows$existing_sample_version <- sample$sample_version
  rows$sample_key <- "uploaded"
  rows$source_code <- prefix
  rows$source_sample_id <- "LAB-1"
  rows$source_row_number <- 2L
  rows$parameter_id <- parameter
  rows$result_type <- 2L
  rows$result <- 42
  rows$result_value_type <- rows$matrix_state_id <- 1L
  rows$analysis_conditions <- I(list(list()))
  rows$sample_no_source_update <- rows$result_no_source_update <- FALSE
  request <- list(
    config = list(dbName = "testdb", dbHost = "10.250.12.154", dbPort = Sys.getenv("aquacachePort", "5432"),
      dbUser = Sys.getenv("aquacacheAdminUser"), dbPass = Sys.getenv("aquacacheAdminPass")),
    file = list(name = basename(file), datapath = file), attachments = list(), df = rows, profile = profile,
    locations = DBI::dbGetQuery(con, "SELECT * FROM public.locations WHERE location_id=$1", params = list(location)),
    sub_locations = DBI::dbGetQuery(con, "SELECT * FROM public.sub_locations WHERE false"),
    field_visit_id = NA_integer_, sample_share_with = list(uploaded = "public_reader"),
    sample_qualifiers = list(), sample_observers = list(),
    mapping_snapshot = env$addDiscData_mapping_snapshot(con, profile),
    upload_coverage = list(omitted_result_rows = 1L, omitted_samples = 1L), partial_upload_acknowledged = TRUE,
    # Old/stale requests may include omitted source samples: they impose no ACL.
    source_sample_visibility = list(list(existing_sample_id = 2147483647L, share_with = "omitted"))
  )
  uploaded <- env$addDiscData_run_upload(request)
  expect_true(uploaded$ok, info = uploaded$message)
  stopifnot(isTRUE(uploaded$ok))
  run <- DBI::dbGetQuery(con,
    "SELECT import_run_id,source_document_id,configuration_snapshot::text AS snapshot
     FROM discrete.import_runs WHERE import_profile_id=$1", params = list(profile_id))
  expect_equal(nrow(run), 1L)
  snapshot <- jsonlite::fromJSON(run$snapshot, simplifyVector = FALSE)
  expect_identical(unlist(snapshot$execution$archived_document_visibility), group)
  expect_identical(jsonlite::fromJSON(DBI::dbGetQuery(con,
    "SELECT array_to_json(share_with)::text AS groups FROM files.documents WHERE document_id=$1",
    params = list(run$source_document_id))$groups), group)
  expect_equal(DBI::dbGetQuery(con,
    "SELECT count(*)::integer AS n FROM discrete.results WHERE import_run_id=$1", params = list(run$import_run_id))$n, 1L)

  DBI::dbExecute(con, "UPDATE public.locations SET share_with=ARRAY['public_reader'] WHERE location_id=$1", params = list(location))
  request$df$parameter_id <- 2147483647L
  failed <- env$addDiscData_run_upload(request)
  expect_false(failed$ok)
  expect_equal(DBI::dbGetQuery(con,
    "SELECT count(*)::integer AS n FROM discrete.import_runs WHERE import_profile_id=$1", params = list(profile_id))$n, 1L)
  # The second upload would have made the workbook public before result insertion.
  expect_identical(jsonlite::fromJSON(DBI::dbGetQuery(con,
    "SELECT array_to_json(share_with)::text AS groups FROM files.documents WHERE document_id=$1",
    params = list(run$source_document_id))$groups), group)
})
