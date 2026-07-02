suppressPackageStartupMessages({
  library(data.table)
  library(DBI)
  library(RPostgres)
})

# Seed AquaCache with ALS discrete-data import profiles and optional parameter
# mappings for the YGwater add discrete data module.
#
# Dry run against dev/aquacache:
# & 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' inst\scripts\seed_ALS_discrete_import_profiles.R --db=aquacache
#
# Apply against dev/aquacache:
# & 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' inst\scripts\seed_ALS_discrete_import_profiles.R --db=aquacache --allow-dev --apply
#
# Apply against prod only when explicitly intended:
# & 'C:\Program Files\R\R-4.6.0\bin\Rscript.exe' inst\scripts\seed_ALS_discrete_import_profiles.R --db=aquacache --host=199.247.132.26 --allow-prod --apply

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(name, default = NULL) {
  hit <- grep(paste0("^", name, "="), args, value = TRUE)
  if (!length(hit)) {
    return(default)
  }
  sub(paste0("^", name, "="), "", hit[[length(hit)]])
}

arg_flag <- function(name) {
  name %in% args || identical(tolower(arg_value(name, "false")), "true")
}

is_present <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

first_existing_path <- function(paths) {
  paths <- paths[is_present(paths)]
  hit <- paths[file.exists(paths)]
  if (!length(hit)) {
    return(NA_character_)
  }
  normalizePath(hit[[1]], winslash = "/", mustWork = TRUE)
}

json_value <- function(x, object = TRUE) {
  if (is.null(x)) {
    x <- if (object) list() else character()
  }
  if (object && length(x) == 0L) {
    return("{}")
  }
  jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null")
}

lookup_column <- function(dt, choices, default = NA) {
  hit <- choices[choices %in% names(dt)]
  if (!length(hit)) {
    return(rep(default, nrow(dt)))
  }
  dt[[hit[[1]]]]
}

as_key_logical <- function(x) {
  if (is.logical(x)) {
    return(x)
  }
  x <- trimws(tolower(as.character(x)))
  x[x %in% c("true", "t", "1", "yes", "y")] <- "TRUE"
  x[x %in% c("false", "f", "0", "no", "n")] <- "FALSE"
  suppressWarnings(as.logical(x))
}

scalar_int <- function(x, default = NA_integer_) {
  if (length(x) == 0 || is.null(x) || !is_present(x)) {
    return(default)
  }
  out <- suppressWarnings(as.integer(x))
  if (length(out) == 0 || is.na(out)) {
    return(default)
  }
  out[[1]]
}

scalar_num <- function(x, default = NA_real_) {
  if (length(x) == 0 || is.null(x) || !is_present(x)) {
    return(default)
  }
  out <- suppressWarnings(as.numeric(x))
  if (length(out) == 0 || is.na(out)) {
    return(default)
  }
  out[[1]]
}

read_mapping_input <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xlsm", "xls")) {
    return(as.data.table(openxlsx::read.xlsx(path)))
  }
  data.table::fread(path, encoding = "UTF-8")
}

upsert_source <- function(con, source_code, source_name, source_description) {
  DBI::dbGetQuery(
    con,
    "INSERT INTO discrete.import_sources
       (source_code, source_name, source_description, active)
     VALUES ($1, $2, $3, TRUE)
     ON CONFLICT (source_code) DO UPDATE
     SET source_name = EXCLUDED.source_name,
         source_description = EXCLUDED.source_description,
         active = TRUE
     RETURNING import_source_id;",
    params = list(source_code, source_name, source_description)
  )$import_source_id[[1]]
}

upsert_profile <- function(con, source_id, profile) {
  DBI::dbGetQuery(
    con,
    "INSERT INTO discrete.import_profiles (
       import_source_id,
       profile_code,
       profile_name,
       profile_description,
       file_type,
       parser_type,
       sheet_strategy,
       sheet_name,
       sheet_index,
       header_row,
       units_row,
       parameter_row,
       data_start_row,
       datetime_origin,
       timezone,
       column_map,
       wide_config,
       defaults,
       sample_identity,
       result_identity,
       validation_rules,
       active,
       note
     ) VALUES (
       $1, $2, $3, $4, $5, $6, $7, $8, $9, $10,
       $11, $12, $13, $14, $15, $16::jsonb, $17::jsonb,
       $18::jsonb, $19::jsonb, $20::jsonb, $21::jsonb, $22, $23
     )
     ON CONFLICT (import_source_id, profile_code) DO UPDATE
     SET profile_name = EXCLUDED.profile_name,
         profile_description = EXCLUDED.profile_description,
         file_type = EXCLUDED.file_type,
         parser_type = EXCLUDED.parser_type,
         sheet_strategy = EXCLUDED.sheet_strategy,
         sheet_name = EXCLUDED.sheet_name,
         sheet_index = EXCLUDED.sheet_index,
         header_row = EXCLUDED.header_row,
         units_row = EXCLUDED.units_row,
         parameter_row = EXCLUDED.parameter_row,
         data_start_row = EXCLUDED.data_start_row,
         datetime_origin = EXCLUDED.datetime_origin,
         timezone = EXCLUDED.timezone,
         column_map = EXCLUDED.column_map,
         wide_config = EXCLUDED.wide_config,
         defaults = EXCLUDED.defaults,
         sample_identity = EXCLUDED.sample_identity,
         result_identity = EXCLUDED.result_identity,
         validation_rules = EXCLUDED.validation_rules,
         active = EXCLUDED.active,
         note = EXCLUDED.note
     RETURNING import_profile_id;",
    params = list(
      source_id,
      profile$profile_code,
      profile$profile_name,
      profile$profile_description,
      profile$file_type,
      profile$parser_type,
      profile$sheet_strategy,
      profile$sheet_name,
      profile$sheet_index,
      as.integer(profile$header_row),
      profile$units_row,
      profile$parameter_row,
      as.integer(profile$data_start_row),
      profile$datetime_origin,
      profile$timezone,
      json_value(profile$column_map, object = TRUE),
      json_value(profile$wide_config, object = TRUE),
      json_value(profile$defaults, object = TRUE),
      json_value(profile$sample_identity, object = FALSE),
      json_value(profile$result_identity, object = FALSE),
      json_value(profile$validation_rules, object = TRUE),
      isTRUE(profile$active),
      profile$note
    )
  )$import_profile_id[[1]]
}

upsert_parameter_mappings <- function(con, source_id, key) {
  if (!("ignore" %in% names(key))) {
    key[, ignore := FALSE]
  }
  key[, ignore_bool := as_key_logical(ignore)]
  key[is.na(ignore_bool), ignore_bool := FALSE]
  key <- key[ignore_bool == FALSE]
  key <- key[is_present(input_param) & !is.na(suppressWarnings(as.integer(parameter_id)))]

  optional_defaults <- list(
    result_type = 2L,
    matrix_state = 1L,
    sample_fraction = NA_integer_,
    sample_fraction_AC = NA_integer_,
    result_value_type = 1L,
    result_speciation_id = NA_integer_,
    conversion = 1,
    result_offset = 0,
    FLAG_notes_combined = NA_character_
  )
  for (nm in names(optional_defaults)) {
    if (!(nm %in% names(key))) {
      key[[nm]] <- optional_defaults[[nm]]
    }
  }

  valid_parameters <- DBI::dbGetQuery(
    con,
    "SELECT parameter_id FROM public.parameters;"
  )$parameter_id
  key[, parameter_id_int := suppressWarnings(as.integer(parameter_id))]
  missing_parameter <- !(key$parameter_id_int %in% valid_parameters)
  if (any(missing_parameter)) {
    message(
      "Skipping ",
      sum(missing_parameter),
      " parameter mapping row(s) whose parameter_id is not present in public.parameters."
    )
    key <- key[!missing_parameter]
  }

  valid_sample_fractions <- DBI::dbGetQuery(
    con,
    "SELECT sample_fraction_id FROM discrete.sample_fractions;"
  )$sample_fraction_id
  valid_result_value_types <- DBI::dbGetQuery(
    con,
    "SELECT result_value_type_id FROM discrete.result_value_types;"
  )$result_value_type_id
  valid_result_speciations <- DBI::dbGetQuery(
    con,
    "SELECT result_speciation_id FROM discrete.result_speciations;"
  )$result_speciation_id

  inserted <- 0L
  for (i in seq_len(nrow(key))) {
    sample_fraction_id <- if ("sample_fraction_id" %in% names(key)) {
      scalar_int(key$sample_fraction_id[[i]])
    } else {
      NA_integer_
    }
    if (is.na(sample_fraction_id) && "sample_fraction_AC" %in% names(key)) {
      sample_fraction_id <- scalar_int(key$sample_fraction_AC[[i]])
    }
    if (!is.na(sample_fraction_id) && !(sample_fraction_id %in% valid_sample_fractions)) {
      sample_fraction_id <- NA_integer_
    }
    result_value_type <- scalar_int(key$result_value_type[[i]], 1L)
    if (!(result_value_type %in% valid_result_value_types)) {
      result_value_type <- 1L
    }
    result_speciation_id <- scalar_int(key$result_speciation_id[[i]])
    if (!is.na(result_speciation_id) && !(result_speciation_id %in% valid_result_speciations)) {
      result_speciation_id <- NA_integer_
    }

    source_match <- json_value(list(
      parameter_code = as.character(key$input_param[[i]]),
      unit = as.character(key$input_unit[[i]])
    ))
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.import_parameter_mappings (
         import_source_id,
         source_match,
         parameter_id,
         result_type,
         sample_fraction_id,
         result_value_type,
         result_speciation_id,
         matrix_state_id,
         conversion,
         result_offset,
         priority,
         active,
         note
       ) VALUES (
         $1, $2::jsonb, $3, $4, $5, $6, $7, $8, $9, $10, 100, TRUE, $11
       )
       ON CONFLICT (import_source_id, source_match) DO UPDATE
       SET parameter_id = EXCLUDED.parameter_id,
           result_type = EXCLUDED.result_type,
           sample_fraction_id = EXCLUDED.sample_fraction_id,
           result_value_type = EXCLUDED.result_value_type,
           result_speciation_id = EXCLUDED.result_speciation_id,
           matrix_state_id = EXCLUDED.matrix_state_id,
           conversion = EXCLUDED.conversion,
           result_offset = EXCLUDED.result_offset,
           active = TRUE,
           note = EXCLUDED.note;",
      params = list(
        source_id,
        source_match,
        key$parameter_id_int[[i]],
        scalar_int(key$result_type[[i]], 2L),
        sample_fraction_id,
        result_value_type,
        result_speciation_id,
        scalar_int(key$matrix_state[[i]], 1L),
        scalar_num(key$conversion[[i]], 1),
        scalar_num(key$result_offset[[i]], 0),
        as.character(key$FLAG_notes_combined[[i]])
      )
    )
    inserted <- inserted + 1L
  }
  inserted
}

upsert_qualifier_mappings <- function(con, source_id) {
  qualifiers <- data.table::data.table(
    qualifier_column = "result",
    qualifier_value = c("<", ">"),
    result_condition = c(1L, 2L),
    result_condition_value_source = "result",
    result_action = "set_result_null",
    note_template = c(
      "Result reported below detection or quantification limit.",
      "Result reported above detection or quantification limit."
    ),
    priority = c(10L, 10L),
    active = c(TRUE, TRUE),
    note = c("Seeded for ALS result imports.", "Seeded for ALS result imports.")
  )

  for (i in seq_len(nrow(qualifiers))) {
    DBI::dbExecute(
      con,
      "INSERT INTO discrete.import_qualifier_mappings (
         import_source_id,
         import_profile_id,
         qualifier_column,
         qualifier_value,
         result_condition,
         result_condition_value_source,
         result_action,
         note_template,
         priority,
         active,
         note
       ) VALUES (
         $1, NULL, $2, $3, $4, $5, $6, $7, $8, $9, $10
       )
       ON CONFLICT (
         import_source_id,
         import_profile_id,
         qualifier_column,
         qualifier_value
       ) DO UPDATE
       SET result_condition = EXCLUDED.result_condition,
           result_condition_value_source = EXCLUDED.result_condition_value_source,
           result_action = EXCLUDED.result_action,
           note_template = EXCLUDED.note_template,
           priority = EXCLUDED.priority,
           active = EXCLUDED.active,
           note = EXCLUDED.note;",
      params = list(
        source_id,
        qualifiers$qualifier_column[[i]],
        qualifiers$qualifier_value[[i]],
        qualifiers$result_condition[[i]],
        qualifiers$result_condition_value_source[[i]],
        qualifiers$result_action[[i]],
        qualifiers$note_template[[i]],
        qualifiers$priority[[i]],
        qualifiers$active[[i]],
        qualifiers$note[[i]]
      )
    )
  }
  nrow(qualifiers)
}

profile_definitions <- function() {
  sample_identity <- c(
    "source_location_name",
    "location_id",
    "sub_location_id",
    "media_id",
    "datetime",
    "sample_type",
    "collection_method",
    "source_sample_id"
  )
  result_identity <- c(
    "source_parameter_code",
    "source_unit",
    "parameter_id",
    "result_type",
    "matrix_state_id",
    "sample_fraction_id",
    "result_value_type",
    "result_speciation_id",
    "laboratory",
    "analysis_datetime"
  )

  list(
    list(
      profile_code = "als_eqwin_can_long",
      profile_name = "ALS YUKON_YG_EQWIN_CAN long export",
      profile_description = "ALS one-result-per-row EQWin-compatible workbook, sheet YUKON_YG_EQWIN_CAN.",
      file_type = "xlsx",
      parser_type = "long",
      sheet_strategy = "name_or_first",
      sheet_name = "YUKON_YG_EQWIN_CAN",
      sheet_index = NA_integer_,
      header_row = 1L,
      units_row = NA_integer_,
      parameter_row = NA_integer_,
      data_start_row = 2L,
      datetime_origin = "text",
      timezone = "America/Whitehorse",
      column_map = list(
        station_code = "Station_Code",
        sample_date = "Smpl_CollectDate",
        sample_time = "Smpl_CollectTime",
        sample_class = "Smpl_Class",
        matrix = "Smpl_Matrix",
        parameter_code = "Lab_Param_Code",
        parameter_name = "Parameter Description",
        result = "Result",
        unit = "Units",
        result_comment = "Result_Comment",
        lab_mdl = "Lab_MDL",
        lab_rdl = "Meth_Rprt_Limit_(RDL)",
        lab_name = "Lab_Name",
        lab_report_no = "Lab_Rport_No",
        lab_sample_id = "Lab_Smpl_#",
        received_date = "Lab_Date-time_Receivd.",
        analytical_method = "Analytical Method",
        analytical_method_code = "Analytical_Method_Code",
        prep_datetime = "Analyt_Prep_Date-time",
        analysis_datetime = "Lab_Analy_Date-time"
      ),
      wide_config = list(),
      defaults = list(
        media_id = 1L,
        collection_method = 27L,
        sample_type = 34L,
        owner = 1L,
        result_type = 2L,
        matrix_state_id = 1L,
        result_value_type = 1L,
        laboratory = 2L
      ),
      sample_identity = sample_identity,
      result_identity = result_identity,
      validation_rules = list(),
      active = TRUE,
      note = "Seeded by seed_ALS_discrete_import_profiles.R."
    ),
    list(
      profile_code = "als_samples_transposed",
      profile_name = "ALS Samples transposed EDD",
      profile_description = "ALS EDD workbook where columns are samples and parameter rows are results.",
      file_type = "xlsx",
      parser_type = "wide",
      sheet_strategy = "name_or_first",
      sheet_name = "Samples",
      sheet_index = NA_integer_,
      header_row = 1L,
      units_row = 15L,
      parameter_row = 15L,
      data_start_row = 15L,
      datetime_origin = "text",
      timezone = "America/Whitehorse",
      column_map = list(
        lab_report_row = 1L,
        lab_sample_row = 2L,
        sampled_by_row = 4L,
        station_code_row = 5L,
        sample_date_row = 6L,
        sample_time_row = 7L,
        matrix_row = 8L,
        lab_code_row = 9L,
        sample_session_row = 10L,
        sample_class_row = 11L,
        sample_number_row = 12L,
        comments_row = 13L,
        parameter_name_column = 1L,
        parameter_code_column = 2L,
        unit_column = 3L,
        first_sample_column = 4L
      ),
      wide_config = list(),
      defaults = list(
        media_id = 1L,
        collection_method = 27L,
        sample_type = 34L,
        owner = 1L,
        result_type = 2L,
        matrix_state_id = 1L,
        result_value_type = 1L,
        laboratory = 2L
      ),
      sample_identity = sample_identity,
      result_identity = result_identity,
      validation_rules = list(),
      active = TRUE,
      note = "Seeded by seed_ALS_discrete_import_profiles.R."
    ),
    list(
      profile_code = "als_xlr_detailed",
      profile_name = "ALS XLR Detailed Report",
      profile_description = "ALS XLR Certificate of Analysis detailed report sheet.",
      file_type = "xlsx",
      parser_type = "long",
      sheet_strategy = "name_or_first",
      sheet_name = "Detailed Report",
      sheet_index = NA_integer_,
      header_row = 9L,
      units_row = NA_integer_,
      parameter_row = NA_integer_,
      data_start_row = 10L,
      datetime_origin = "text",
      timezone = "America/Whitehorse",
      column_map = list(
        parameter_name = "Analyte",
        lab_sample_id = "ALS Sample ID",
        station_code = "Client Sample ID",
        matrix = "Matrix",
        sub_matrix = "Sub-Matrix",
        analytical_method_code = "Method",
        result = "Results",
        lab_rdl = "Detection Limit",
        unit = "Units",
        result_comment = "Qual",
        sample_date = "Date Sampled",
        sample_time = "Time Sampled",
        prep_datetime = "Prep Date",
        analysis_datetime = "Analysis Date"
      ),
      wide_config = list(),
      defaults = list(
        media_id = 1L,
        collection_method = 27L,
        sample_type = 34L,
        owner = 1L,
        result_type = 2L,
        matrix_state_id = 1L,
        result_value_type = 1L,
        laboratory = 2L
      ),
      sample_identity = sample_identity,
      result_identity = result_identity,
      validation_rules = list(),
      active = TRUE,
      note = "Seeded by seed_ALS_discrete_import_profiles.R."
    )
  )
}

if (file.exists("C:/Users/gtdelapl/Documents/.Renviron")) {
  readRenviron("C:/Users/gtdelapl/Documents/.Renviron")
}

repo_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
mapping_key <- first_existing_path(c(
  arg_value("--mapping-key", NA_character_),
  file.path(repo_root, "inst", "import_keys", "EQWin.csv"),
  "C:/Users/gtdelapl/Documents/AquaCache/inst/import_keys/EQWin.csv"
))

target_db <- arg_value("--db", Sys.getenv("aquacacheName", "aquacache"))
target_host <- arg_value("--host", Sys.getenv("aquacacheHost", "10.250.12.154"))
target_port <- arg_value("--port", Sys.getenv("aquacachePort", "5432"))
target_user <- arg_value("--user", Sys.getenv("aquacacheAdminUser", Sys.getenv("aquacacheUser")))
target_pass <- arg_value("--password", Sys.getenv("aquacacheAdminPass", Sys.getenv("aquacachePass")))
apply_changes <- arg_flag("--apply")
allow_dev <- arg_flag("--allow-dev")
allow_prod <- arg_flag("--allow-prod")
upload_mappings <- !arg_flag("--profiles-only")

if (!apply_changes) {
  message("Dry run only. Re-run with --apply to modify the database.")
}
if (apply_changes && identical(target_host, "10.250.12.154") && !allow_dev) {
  stop("Refusing to modify the dev database without --allow-dev.")
}
if (apply_changes && identical(target_host, "199.247.132.26") && !allow_prod) {
  stop("Refusing to modify production without --allow-prod.")
}

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = target_db,
  host = target_host,
  port = target_port,
  user = target_user,
  password = target_pass
)
on.exit(DBI::dbDisconnect(con), add = TRUE)

identity <- DBI::dbGetQuery(
  con,
  "SELECT current_database() AS db, inet_server_addr()::text AS host, current_user AS username"
)
message(
  "Connected to database=",
  identity$db[[1]],
  ", host=",
  identity$host[[1]],
  ", user=",
  identity$username[[1]]
)

required <- DBI::dbGetQuery(
  con,
  "SELECT
     to_regclass('discrete.import_sources') IS NOT NULL AS has_sources,
     to_regclass('discrete.import_profiles') IS NOT NULL AS has_profiles,
     to_regclass('discrete.import_parameter_mappings') IS NOT NULL AS has_parameter_mappings,
     to_regclass('discrete.import_qualifier_mappings') IS NOT NULL AS has_qualifier_mappings;"
)
if (!all(unlist(required))) {
  stop("Import mapping tables are missing. Apply AquaCache patch 49 before running this script.")
}

profiles <- profile_definitions()
message("Profiles to upsert: ", length(profiles))
if (is_present(mapping_key)) {
  message("Parameter mapping key: ", mapping_key)
} else {
  message("No EQWin/ALS parameter mapping key found. Profiles can still be seeded.")
}

DBI::dbExecute(con, "BEGIN")

tryCatch(
  {
    source_id <- upsert_source(
      con,
      source_code = "ALS",
      source_name = "ALS Environmental",
      source_description = "ALS laboratory and EQWin-compatible water-quality workbook exports."
    )

    profile_ids <- integer()
    for (profile in profiles) {
      profile_ids <- c(profile_ids, upsert_profile(con, source_id, profile))
    }

    qualifier_count <- upsert_qualifier_mappings(con, source_id)
    mapping_count <- 0L
    if (upload_mappings && is_present(mapping_key)) {
      key <- read_mapping_input(mapping_key)
      key <- data.table::as.data.table(key)
      mapping_count <- upsert_parameter_mappings(con, source_id, key)
    }

    if (apply_changes) {
      DBI::dbExecute(con, "COMMIT")
    } else {
      DBI::dbExecute(con, "ROLLBACK")
    }

    message("Profile IDs: ", paste(profile_ids, collapse = ", "))
    message("Qualifier mappings processed: ", qualifier_count)
    message("Parameter mappings processed: ", mapping_count)
  },
  error = function(e) {
    try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    stop(e)
  }
)
