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
  source <- DBI::dbGetQuery(
    con,
    "SELECT source_code, source_name, source_description
     FROM discrete.import_sources
     WHERE import_source_id = $1",
    params = list(source_id)
  )
  AquaCache::upsertImportProfile(
    con = con,
    source_code = source$source_code[[1]],
    source_name = source$source_name[[1]],
    source_description = source$source_description[[1]],
    profile_code = profile$profile_code,
    profile_name = profile$profile_name,
    profile_description = profile$profile_description,
    file_type = profile$file_type,
    parser_type = profile$parser_type,
    sheet_strategy = profile$sheet_strategy,
    sheet_name = profile$sheet_name,
    sheet_index = profile$sheet_index,
    header_row = profile$header_row,
    units_row = profile$units_row,
    parameter_row = profile$parameter_row,
    data_start_row = profile$data_start_row,
    datetime_origin = profile$datetime_origin,
    timezone = profile$timezone,
    column_map = profile$column_map,
    wide_config = profile$wide_config,
    defaults = profile$defaults,
    sample_identity = profile$sample_identity,
    result_identity = profile$result_identity,
    validation_rules = profile$validation_rules,
    active = profile$active,
    note = profile$note
  )
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

  resolved <- vector("list", nrow(key))
  for (i in seq_len(nrow(key))) {
    sample_fraction_id <- if ("sample_fraction_id" %in% names(key)) {
      scalar_int(key$sample_fraction_id[[i]])
    } else {
      NA_integer_
    }
    if (is.na(sample_fraction_id) && "sample_fraction_AC" %in% names(key)) {
      sample_fraction_id <- scalar_int(key$sample_fraction_AC[[i]])
    }
    if (is.na(sample_fraction_id) && "sample_fraction" %in% names(key)) {
      sample_fraction_id <- scalar_int(key$sample_fraction[[i]])
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

    resolved[[i]] <- data.frame(
      parameter_code = as.character(key$input_param[[i]]),
      unit = as.character(key$input_unit[[i]]),
      parameter_id = key$parameter_id_int[[i]],
      result_type = scalar_int(key$result_type[[i]], 2L),
      sample_fraction_id = sample_fraction_id,
      result_value_type = result_value_type,
      result_speciation_id = result_speciation_id,
      matrix_state_id = scalar_int(key$matrix_state[[i]], 1L),
      conversion = scalar_num(key$conversion[[i]], 1),
      result_offset = scalar_num(key$result_offset[[i]], 0),
      priority = 100L,
      active = TRUE,
      note = as.character(key$FLAG_notes_combined[[i]]),
      stringsAsFactors = FALSE
    )
  }
  resolved <- data.table::rbindlist(resolved, fill = TRUE)
  source <- DBI::dbGetQuery(
    con,
    "SELECT source_code, source_name
     FROM discrete.import_sources
     WHERE import_source_id = $1",
    params = list(source_id)
  )
  AquaCache::upsertImportParameterMappings(
    con = con,
    source_code = source$source_code[[1]],
    source_name = source$source_name[[1]],
    mappings = resolved,
    match_columns = c("parameter_code", "unit")
  )

  # The Detailed Report labels and units differ from the ECCC/EQWin source
  # vocabulary even when they describe the same target parameter. Publish
  # explicit profile-scoped rows so those differences remain inspectable in
  # the database rather than being hidden in parser code.
  target_units <- DBI::dbGetQuery(
    con,
    "SELECT p.parameter_id,
            liquid.unit_name AS unit_liquid,
            solid.unit_name AS unit_solid,
            gas.unit_name AS unit_gas
     FROM public.parameters p
     LEFT JOIN public.units liquid ON liquid.unit_id = p.units_liquid
     LEFT JOIN public.units solid ON solid.unit_id = p.units_solid
     LEFT JOIN public.units gas ON gas.unit_id = p.units_gas"
  )
  xlr <- merge(
    data.table::copy(resolved),
    data.table::as.data.table(target_units),
    by = "parameter_id",
    all.x = TRUE
  )
  source_labels <- toupper(trimws(gsub(
    "[[:space:]]+",
    " ",
    xlr$parameter_code
  )))
  xlr[, parameter_code := vapply(
    source_labels,
    function(label) {
      if (grepl("^HARDNESS DISSOLVED", label)) {
        return("Hardness (as CaCO3), dissolved")
      }
      if (
        grepl("^TOTAL DISSOLVED SOLIDS", label) ||
          identical(label, "RESIDUE FILTERABLE")
      ) {
        return("Solids, total dissolved [TDS]")
      }
      if (grepl(" DISSOLVED$", label)) {
        analyte <- sub(" DISSOLVED$", "", label)
        return(paste0(tools::toTitleCase(tolower(analyte)), ", dissolved"))
      }
      NA_character_
    },
    character(1)
  )]
  xlr[, unit := data.table::fcase(
    matrix_state_id == 1L, unit_liquid,
    matrix_state_id == 2L, unit_solid,
    matrix_state_id == 3L, unit_gas,
    default = NA_character_
  )]
  xlr <- xlr[
    is_present(parameter_code) & is_present(unit),
    .(
      parameter_code,
      unit,
      parameter_id,
      result_type,
      sample_fraction_id,
      result_value_type,
      result_speciation_id,
      matrix_state_id,
      conversion = 1,
      result_offset = 0,
      priority = 100L,
      active = TRUE,
      note = "ALS XLR Detailed Report label and target-unit mapping."
    )
  ]
  xlr_mg_l <- data.table::copy(xlr)
  target_unit_key <- tolower(gsub("[µμ]", "u", xlr_mg_l$unit))
  xlr_mg_l[, conversion := data.table::fcase(
    target_unit_key == "mg/l", 1,
    target_unit_key == "ug/l", 1000,
    target_unit_key == "ng/l", 1000000,
    target_unit_key == "g/l", 0.001,
    default = NA_real_
  )]
  xlr_mg_l[, unit := "mg/L"]
  xlr_mg_l[, note := paste(
    "ALS XLR Detailed Report mg/L source-unit conversion to the",
    "parameter target unit."
  )]
  xlr <- data.table::rbindlist(
    list(xlr, xlr_mg_l[is.finite(conversion)]),
    fill = TRUE
  )
  suspended_solids <- DBI::dbGetQuery(
    con,
    "SELECT p.parameter_id, u.unit_name
     FROM public.parameters p
     JOIN public.units u ON u.unit_id = p.units_liquid
     WHERE lower(p.param_name) = 'total suspended solids'"
  )
  if (nrow(suspended_solids) == 1L) {
    total_fraction <- if (19L %in% valid_sample_fractions) {
      19L
    } else {
      DBI::dbGetQuery(
        con,
        "SELECT sample_fraction_id
         FROM discrete.sample_fractions
         WHERE lower(sample_fraction) LIKE 'total%'
         ORDER BY sample_fraction_id
         LIMIT 1"
      )$sample_fraction_id
    }
    xlr <- data.table::rbindlist(list(
      xlr,
      data.table::data.table(
        parameter_code = "Solids, total suspended [TSS]",
        unit = suspended_solids$unit_name[[1]],
        parameter_id = suspended_solids$parameter_id[[1]],
        result_type = 2L,
        sample_fraction_id = if (length(total_fraction) == 1L) {
          total_fraction[[1]]
        } else {
          NA_integer_
        },
        result_value_type = 1L,
        result_speciation_id = NA_integer_,
        matrix_state_id = 1L,
        conversion = 1,
        result_offset = 0,
        priority = 100L,
        active = TRUE,
        note = "ALS XLR Detailed Report label and target-unit mapping."
      )
    ), fill = TRUE)
  }

  # A few XLR labels have no exact counterpart in the legacy EQWin key (or
  # use qualifiers such as "dissolved/filtered" there). Keep these aliases
  # explicit and profile-scoped so their fraction and unit treatment remains
  # visible to import-profile editors.
  dissolved_fraction <- DBI::dbGetQuery(
    con,
    "SELECT sample_fraction_id
     FROM discrete.sample_fractions
     WHERE lower(sample_fraction) = 'dissolved'"
  )$sample_fraction_id
  explicit_xlr_aliases <- data.table::data.table(
    parameter_code = c(
      "Mercury, dissolved",
      "Phosphorus, dissolved",
      "Potassium, dissolved",
      "Sodium, dissolved",
      "Sulfur, dissolved",
      "Thorium, dissolved"
    ),
    param_name = c(
      "mercury",
      "phosphorus, elemental",
      "potassium",
      "sodium",
      "sulfur",
      "thorium"
    )
  )
  explicit_targets <- data.table::as.data.table(DBI::dbGetQuery(
    con,
    "SELECT p.parameter_id, lower(p.param_name) AS param_name,
            u.unit_name AS unit
     FROM public.parameters p
     JOIN public.units u ON u.unit_id = p.units_liquid"
  ))
  explicit_targets <- explicit_targets[
    param_name %chin% explicit_xlr_aliases$param_name
  ]
  explicit_xlr_aliases <- merge(
    explicit_xlr_aliases,
    explicit_targets,
    by = "param_name",
    all.x = TRUE,
    sort = FALSE
  )
  missing_explicit <- explicit_xlr_aliases[is.na(parameter_id), param_name]
  if (length(missing_explicit)) {
    stop(
      "Could not resolve ALS XLR target parameter(s): ",
      paste(missing_explicit, collapse = ", "),
      call. = FALSE
    )
  }
  if (length(dissolved_fraction) != 1L) {
    stop("Could not uniquely resolve the dissolved sample fraction.", call. = FALSE)
  }
  explicit_xlr_aliases[, `:=`(
    result_type = 2L,
    sample_fraction_id = dissolved_fraction[[1]],
    result_value_type = 1L,
    result_speciation_id = NA_integer_,
    matrix_state_id = 1L,
    conversion = 1,
    result_offset = 0,
    priority = 100L,
    active = TRUE,
    note = "ALS XLR Detailed Report dissolved-analyte alias."
  )]
  xlr <- data.table::rbindlist(list(
    xlr,
    explicit_xlr_aliases[, setdiff(names(explicit_xlr_aliases), "param_name"), with = FALSE]
  ), fill = TRUE)
  xlr <- unique(xlr, by = c("parameter_code", "unit"))
  AquaCache::upsertImportParameterMappings(
    con = con,
    source_code = source$source_code[[1]],
    source_name = source$source_name[[1]],
    profile_code = "als_xlr_detailed",
    mappings = xlr,
    match_columns = c("parameter_code", "unit")
  )
  message(
    "Processed ", nrow(resolved), " source-wide and ", nrow(xlr),
    " ALS XLR profile-specific parameter mappings."
  )
  nrow(resolved) + nrow(xlr)
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
        method_detection_limit = "Lab_MDL",
        reporting_detection_limit = "Meth_Rprt_Limit_(RDL)",
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
      validation_rules = list(parser_family = "long"),
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
      validation_rules = list(parser_family = "transposed"),
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
        reporting_detection_limit = "Detection Limit",
        unit = "Units",
        result_flag = "Qual",
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
      validation_rules = list(parser_family = "xlr"),
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
  file.path(repo_root, "..", "AquaCache", "inst", "import_keys", "downloadECCCeq1.csv"),
  "C:/Users/gtdelapl/Documents/AquaCache/inst/import_keys/downloadECCCeq1.csv",
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
     to_regclass('discrete.import_result_flag_mappings') IS NOT NULL AS has_result_flag_mappings,
     to_regclass('discrete.import_location_mappings') IS NOT NULL AS has_location_mappings,
     EXISTS (
       SELECT 1
       FROM information_schema.columns
       WHERE table_schema = 'discrete'
         AND table_name = 'import_parameter_mappings'
         AND column_name = 'import_mapping_set_id'
     ) AS has_versioned_parameter_mappings;"
)
if (!all(unlist(required))) {
  stop(
    "The Patch 61 import mapping schema is missing. Apply AquaCache patch 61 before running this script."
  )
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
    message("Parameter mappings processed: ", mapping_count)
  },
  error = function(e) {
    try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
    stop(e)
  }
)
