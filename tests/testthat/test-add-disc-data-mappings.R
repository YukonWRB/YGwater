add_disc_data_mapping_environment <- function() {
  module_path <- system.file(
    "apps/aqualink/modules/admin/discreteData/addDiscData.R",
    package = "YGwater"
  )
  env <- new.env(parent = asNamespace("YGwater"))
  sys.source(module_path, envir = env)
  sys.source(
    system.file(
      "apps/aqualink/modules/admin/discreteData/importSetup.R",
      package = "YGwater"
    ),
    envir = env
  )
  env
}

add_disc_data_test_profile <- function(parser_family = "xlr") {
  column_map <- if (identical(parser_family, "long")) {
    list(
      station_code = "Station_Code",
      sample_date = "Smpl_CollectDate",
      sample_time = "Smpl_CollectTime",
      parameter_code = "Lab_Param_Code",
      parameter_name = "Parameter Description",
      result = "Result",
      unit = "Units",
      lab_sample_id = "Lab_Smpl_#"
    )
  } else {
    list(
      parameter_name = "Analyte",
      lab_sample_id = "ALS Sample ID",
      station_code = "Client Sample ID",
      result = "Results",
      unit = "Units",
      result_flag = "Qual",
      sample_date = "Date Sampled",
      sample_time = "Time Sampled",
      analysis_datetime = "Analysis Date"
    )
  }
  defaults <- list(
    media_id = 1L,
    collection_method = 27L,
    sample_type = 34L,
    owner = 1L,
    contributor = 9L,
    result_type = 2L,
    matrix_state_id = 1L,
    result_value_type = 1L,
    laboratory = 2L,
    grade_type_id = 3L,
    approval_type_id = 4L,
    sample_no_source_update = TRUE,
    result_no_source_update = FALSE
  )
  data.frame(
    import_profile_id = 1L,
    import_source_id = 1L,
    source_code = "ALS",
    source_name = "Test laboratory",
    profile_code = paste0("test_", parser_family),
    profile_name = paste("Test", parser_family),
    profile_description = "Test fixture",
    file_type = "xlsx",
    parser_type = if (identical(parser_family, "transposed")) {
      "wide"
    } else {
      "long"
    },
    sheet_strategy = "name_or_first",
    sheet_name = if (identical(parser_family, "xlr")) {
      "Detailed Report"
    } else {
      "Data"
    },
    sheet_index = NA_integer_,
    header_row = 1L,
    units_row = NA_integer_,
    parameter_row = NA_integer_,
    data_start_row = 2L,
    datetime_origin = "text",
    timezone = "America/Whitehorse",
    active = TRUE,
    note = NA_character_,
    stringsAsFactors = FALSE
  ) |>
    transform(
      column_map = I(list(column_map)),
      wide_config = I(list(list())),
      defaults = I(list(defaults)),
      sample_identity = I(list(character())),
      result_identity = I(list(character())),
      validation_rules = I(list(list(parser_family = parser_family)))
    )
}

test_that("typed profile defaults reach imported samples and results", {
  env <- add_disc_data_mapping_environment()
  profile <- add_disc_data_test_profile()
  rows <- env$addDiscData_common_rows(
    source_code = "ALS",
    profile = profile,
    source_location_name = "Test Station",
    sample_date = "2026-01-02",
    sample_time = "12:00",
    source_sample_id = "S1",
    source_parameter_code = "AL",
    source_parameter_name = "Aluminum",
    source_unit = "ug/L",
    result_raw = "1.2"
  )

  expect_identical(rows$contributor[[1]], 9L)
  expect_true(rows$sample_no_source_update[[1]])
  expect_false(rows$result_no_source_update[[1]])
  expect_identical(rows$grade_type_id[[1]], 3L)
  expect_identical(rows$approval_type_id[[1]], 4L)
})

test_that("unmapped parameter choices start with an explicit blank", {
  env <- add_disc_data_mapping_environment()
  parameters <- data.frame(
    parameter_id = c(654L, 10L),
    param_name = c("(r)-dichlorprop", "aluminum"),
    unit_liquid = c("mg/l", "mg/l"),
    unit_solid = c("mg/kg", "mg/kg"),
    unit_gas = NA_character_
  )
  matrix_states <- data.frame(
    matrix_state_id = c(1L, 2L),
    matrix_state_code = c("liquid", "solid")
  )

  choices <- env$addDiscData_parameter_choices(parameters)

  expect_identical(unname(choices[[1]]), "")
  expect_identical(names(choices)[[1]], "Select AquaCache parameter")
  expect_match(names(choices)[[2]], "Liquid: mg/l", fixed = TRUE)
  expect_identical(
    env$addDiscData_target_unit(
      parameters,
      c(10L, 10L),
      c(1L, 2L),
      matrix_states
    ),
    c("mg/l", "mg/kg")
  )
})

test_that("mapping conversion and offset are applied once", {
  env <- add_disc_data_mapping_environment()
  profile <- add_disc_data_test_profile("xlr")
  rows <- env$addDiscData_common_rows(
    source_code = "ALS",
    profile = profile,
    source_location_name = "Station A",
    sample_date = c("2026-09-01", "2026-09-01"),
    sample_time = c("10:00", "10:00"),
    source_sample_id = c("S1", "S1"),
    source_parameter_code = c("Aluminum, dissolved", "Aluminum, dissolved"),
    source_parameter_name = c("Aluminum, dissolved", "Aluminum, dissolved"),
    source_unit = c("ug/L", "ug/L"),
    result_raw = c("10", "<10")
  )
  env$addDiscData_fetch_mappings <- function(con, source_code, ...) {
    data.frame(
      source_code = "ALS",
      source_match = '{"parameter_code":"Aluminum, dissolved","unit":"ug/L"}',
      parameter_id = 10L,
      result_type = 2L,
      sample_fraction_id = 5L,
      result_value_type = 1L,
      reporting_basis_id = NA_integer_,
      matrix_state_id = 1L,
      conversion = 0.001,
      result_offset = 0.5
    )
  }
  env$addDiscData_fetch_result_flag_mappings <- function(...) data.frame()

  mapped <- env$addDiscData_apply_mappings(rows, con = NULL)
  mapped_again <- env$addDiscData_apply_mappings(mapped, con = NULL)

  expect_equal(mapped$result[[1]], 0.51)
  expect_equal(mapped$result_condition_value[[2]], 0.51)
  expect_equal(mapped_again$result, mapped$result)
  expect_equal(
    mapped_again$result_condition_value,
    mapped$result_condition_value
  )
  expect_identical(mapped$sample_fraction_id, c(5L, 5L))
  expect_identical(mapped$mapping_status, c("mapped", "mapped"))
})

test_that("source-specific result-flag mappings use typed detection limits", {
  env <- add_disc_data_mapping_environment()
  profile <- add_disc_data_test_profile("xlr")
  rows <- env$addDiscData_common_rows(
    source_code = "ALS",
    profile = profile,
    source_location_name = "Station A",
    sample_date = "2026-09-01",
    sample_time = "10:00",
    source_sample_id = "S1",
    source_parameter_code = "Aluminum, dissolved",
    source_parameter_name = "Aluminum, dissolved",
    source_unit = "ug/L",
    result_raw = "10",
    result_flag = "ND",
    result_flag_column = "Qual",
    reporting_detection_limit = 20,
    note = "Source note"
  )
  env$addDiscData_fetch_mappings <- function(...) {
    data.frame(
      source_code = "ALS",
      source_match = '{"parameter_code":"Aluminum, dissolved","unit":"ug/L"}',
      parameter_id = 10L,
      result_type = 2L,
      sample_fraction_id = 5L,
      result_value_type = 1L,
      reporting_basis_id = NA_integer_,
      matrix_state_id = 1L,
      conversion = 0.001,
      result_offset = 0
    )
  }
  env$addDiscData_fetch_result_flag_mappings <- function(...) {
    data.frame(
      import_result_flag_mapping_id = 1L,
      source_flag_column = "Qual",
      source_flag_value = "ND",
      result_condition_id = 1L,
      result_condition_value_source = "reporting_detection_limit",
      result_condition_value_literal = NA_real_,
      result_action = "set_result_null",
      note_template = "Not detected by the laboratory.",
      priority = 10L,
      profile_specific = TRUE
    )
  }

  mapped <- env$addDiscData_apply_mappings(rows, con = NULL)
  mapped_again <- env$addDiscData_apply_mappings(mapped, con = NULL)

  expect_true(is.na(mapped$result))
  expect_identical(mapped$result_condition, 1L)
  expect_equal(mapped$result_condition_value, 0.02)
  expect_identical(mapped$result_flag_action, "set_result_null")
  expect_identical(
    mapped$note,
    "Source note; Not detected by the laboratory."
  )
  expect_identical(mapped_again$note, mapped$note)
})

test_that("unit-specific mappings do not become ambiguous code-only fallbacks", {
  env <- add_disc_data_mapping_environment()

  specific <- env$addDiscData_mapping_keys(
    '{"parameter_code":"Aluminum","unit":"ug/L"}'
  )
  generic <- env$addDiscData_mapping_keys(
    '{"parameter_code":"Aluminum","unit":""}'
  )

  expect_identical(specific, "aluminum\rug/l")
  expect_identical(generic, "aluminum\r")
})

test_that("XLR parser excludes filtration-location metadata rows", {
  env <- add_disc_data_mapping_environment()
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)

  header <- c(
    "Analyte",
    "ALS Sample ID",
    "Client Sample ID",
    "Matrix",
    "Sub-Matrix",
    "Method",
    "Results",
    "Detection Limit",
    "Units",
    "Qual",
    "Date Sampled",
    "Time Sampled",
    "Prep Date",
    "Analysis Date"
  )
  raw <- matrix("", nrow = 10L, ncol = length(header))
  raw[1:6, 1] <- paste("Report heading", 1:6)
  raw[7, ] <- header
  raw[8, ] <- c(
    "Aluminum, dissolved",
    "S1",
    "Station A",
    "Water",
    "",
    "ME-ICP",
    "0.1",
    "0.01",
    "mg/L",
    "",
    "2026-09-01",
    "10:00",
    "",
    "2026-09-02"
  )
  raw[9, ] <- c(
    "Dissolved metals filtration location",
    "S1",
    "Station A",
    "Water",
    "",
    "",
    "Field",
    "",
    "",
    "",
    "2026-09-01",
    "10:00",
    "",
    ""
  )
  raw[10, ] <- c(
    "Dissolved mercury filtration location",
    "S1",
    "Station A",
    "Water",
    "",
    "",
    "Lab",
    "",
    "",
    "",
    "2026-09-01",
    "10:00",
    "",
    ""
  )
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Detailed Report")
  openxlsx::writeData(wb, "Detailed Report", raw, colNames = FALSE)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)

  profile <- add_disc_data_test_profile("xlr")
  profile$profile_code <- "custom_xlr_copy"
  profile$sheet_name <- "Renamed report"
  parsed <- env$addDiscData_parse_upload(path, profile)

  expect_equal(nrow(parsed), 1L)
  expect_identical(parsed$source_parameter_code, "Aluminum, dissolved")
  expect_identical(
    format(parsed$datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    "2026-09-01 17:00:00"
  )
  expect_identical(
    format(parsed$analysis_datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    "2026-09-02 07:00:00"
  )
})

test_that("Excel and textual dates are parsed without partial-year matches", {
  env <- add_disc_data_mapping_environment()
  excel_serial <- as.numeric(as.Date("2025-10-02") - as.Date("1899-12-30"))

  expect_identical(
    as.character(env$addDiscData_as_date(c(
      "02-Oct-2025",
      "2025-Oct-02",
      "2025-10-02",
      as.character(excel_serial)
    ))),
    rep("2025-10-02", 4L)
  )
  expect_true(is.na(env$addDiscData_as_date("02-Oct-2025 trailing text")))
  expect_identical(
    format(
      env$addDiscData_datetime("02-Oct-2025", "10:50", "America/Whitehorse"),
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    "2025-10-02 17:50:00"
  )
  expect_identical(
    format(
      env$addDiscData_datetime(
        "7/2/2026 10:35 AM",
        NA_character_,
        "America/Whitehorse"
      ),
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    "2026-07-02 17:35:00"
  )
  expect_identical(
    format(
      env$addDiscData_datetime(
        "2025-06-10",
        "12:30:00.0000000",
        "America/Whitehorse"
      ),
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    "2025-06-10 19:30:00"
  )
})

test_that("EHS component workbooks suggest a long layout without a unit column", {
  env <- add_disc_data_mapping_environment()
  raw <- as.data.frame(
    as.list(c(
      "Sample ID",
      "CustomerId",
      "JobName",
      "SampledDate",
      "LocationId",
      "Area",
      "SamplingPoint",
      "Analysis",
      "ComponentName",
      "ResultText"
    )),
    check.names = FALSE
  )

  suggested <- env$importSetup_suggest_layout(raw)

  expect_identical(suggested$parser_family, "long")
  expect_identical(suggested$column_map$parameter_code, "ComponentName")
  expect_identical(suggested$column_map$result, "ResultText")
  expect_null(suggested$column_map$unit)
})

test_that("Mayo-style sample reports suggest and parse transposed results", {
  env <- add_disc_data_mapping_environment()
  raw <- matrix("", nrow = 28L, ncol = 8L)
  raw[1L, 1L] <- "Report Id"
  raw[1L, 7:8] <- "3148108"
  raw[4L, 1L] <- "Sample Id"
  raw[4L, 7:8] <- c("S1", "S2")
  raw[16L, 1L] <- "Site Id"
  raw[16L, 7:8] <- c("W3", "W5")
  raw[18L, 1L] <- "Sample Date"
  raw[18L, 7:8] <- "2025-06-10"
  raw[19L, 1L] <- "Sampled Time"
  raw[19L, 7:8] <- c("12:30:00.0000000", "12:20:00.0000000")
  raw[26L, ] <- c(
    "Report Results Group",
    "Method Reference",
    "Analyte",
    "Descriptor",
    "Unit",
    "Detection Limit",
    "Result Text",
    "Result Text"
  )
  raw[27L, c(1L, 2L, 3L, 5L, 6L, 7L, 8L)] <- c(
    "Routine Water",
    "APHA [4500-H+ B]",
    "pH",
    "",
    "0.01",
    "7.2",
    "7.3"
  )
  raw[28L, c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L)] <- c(
    "Routine Water",
    "APHA [4110 B]",
    "Nitrate",
    "Dissolved",
    "mg/L",
    "0.01",
    "<0.01",
    "0.02"
  )

  suggested <- env$importSetup_suggest_layout(as.data.frame(raw))
  expect_identical(suggested$parser_family, "transposed")
  expect_identical(suggested$header_row, 26L)
  expect_identical(suggested$column_map$data_start_row, 27L)
  expect_identical(suggested$column_map$first_sample_column, 7L)
  expect_setequal(
    suggested$column_map$match_columns,
    c(
      "Report Results Group",
      "Method Reference",
      "Descriptor",
      "Detection Limit"
    )
  )

  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Test Results")
  openxlsx::writeData(wb, "Test Results", raw, colNames = FALSE)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)

  profile <- add_disc_data_test_profile("transposed")
  profile$sheet_name <- "Test Results"
  profile$header_row <- suggested$header_row
  profile$column_map[[1]] <- suggested$column_map
  parsed <- env$addDiscData_parse_upload(path, profile)

  expect_equal(nrow(parsed), 4L)
  expect_identical(
    parsed$source_parameter_name,
    c("pH", "Nitrate", "pH", "Nitrate")
  )
  expect_identical(
    parsed$result_condition,
    c(NA_integer_, 1L, NA_integer_, NA_integer_)
  )
  nitrate_context <- jsonlite::fromJSON(
    parsed$source_matching_values[[2]],
    simplifyVector = FALSE
  )
  expect_identical(nitrate_context[["source.Descriptor"]], "Dissolved")
  expect_identical(
    nitrate_context[["source.Method Reference"]],
    "APHA [4110 B]"
  )
  expect_identical(
    format(parsed$datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    c(
      "2025-06-10 19:30:00",
      "2025-06-10 19:30:00",
      "2025-06-10 19:20:00",
      "2025-06-10 19:20:00"
    )
  )
})

test_that("location choices expose name, code, and alias", {
  env <- add_disc_data_mapping_environment()
  locations <- data.frame(
    location_id = c(11L, 12L),
    location_code = c("YT001", "YT002"),
    name = c("Wolf Creek", "Marsh Lake"),
    alias = c("WC", NA_character_)
  )

  labels <- env$addDiscData_location_labels(locations)
  choices <- env$addDiscData_location_choices(locations, include_blank = TRUE)

  expect_identical(labels[[1]], "Wolf Creek | Code: YT001 | Alias: WC")
  expect_identical(labels[[2]], "Marsh Lake | Code: YT002")
  expect_identical(unname(choices[[1]]), "")
  expect_identical(unname(choices[labels]), c("11", "12"))
})

test_that("sample locations can be matched and reassigned independently", {
  env <- add_disc_data_mapping_environment()
  locations <- data.frame(
    location_id = c(11L, 12L, 13L),
    location_code = c("YT001", "YT002", "YT003"),
    name = c("Wolf Creek", "Marsh Lake", "Other"),
    alias = c("WC", "ML", "WC")
  )
  rows <- data.frame(
    sample_key = c("S1", "S1", "S2", "S2"),
    source_location_name = c("YT001", "YT001", "ML", "ML"),
    location_id = NA_integer_,
    sub_location_id = NA_integer_
  )

  matched <- env$addDiscData_location_match(rows, locations)
  assigned <- env$addDiscData_assign_sample_locations(
    matched,
    sample_keys = "S2",
    location_id = 12L,
    sub_location_id = 8L
  )

  expect_identical(matched$location_id, c(11L, 11L, 12L, 12L))
  expect_identical(assigned$location_id, c(11L, 11L, 12L, 12L))
  expect_identical(
    assigned$sub_location_id,
    c(NA_integer_, NA_integer_, 8L, 8L)
  )

  rows$source_location_name <- "WC"
  ambiguous <- env$addDiscData_location_match(rows, locations)
  expect_true(all(is.na(ambiguous$location_id)))
})

test_that("saved location mappings override incidental location labels", {
  env <- add_disc_data_mapping_environment()
  locations <- data.frame(
    location_id = c(11L, 12L),
    location_code = c("LAB-1", "YT002"),
    name = c("Incidental match", "Mapped location"),
    alias = c("", "")
  )
  rows <- data.frame(
    sample_key = "S1",
    source_location_name = "LAB-1",
    location_id = NA_integer_,
    sub_location_id = NA_integer_
  )
  mappings <- data.frame(
    source_location_code = "lab-1",
    location_id = 12L,
    sub_location_id = NA_integer_,
    profile_specific = TRUE
  )

  matched <- env$addDiscData_location_match(rows, locations, mappings)

  expect_identical(matched$location_id, 12L)
  expect_identical(matched$location_mapping_status, "profile mapping")
})

test_that("profile keys are source-specific and profile fields are human editable", {
  env <- add_disc_data_mapping_environment()
  profile <- add_disc_data_test_profile("long")

  expect_identical(
    env$addDiscData_profile_key(c("ALS", "OTHER"), c("shared", "shared")),
    c("ALS::shared", "OTHER::shared")
  )
  json <- env$addDiscData_profile_json(profile, "column_map")
  expect_true(jsonlite::validate(json))
  expect_match(json, '"parameter_code"', fixed = TRUE)

  server_code <- paste(deparse(body(env$importSetup)), collapse = "\n")
  profile_loader_code <- paste(
    deparse(body(env$addDiscData_read_profiles)),
    collapse = "\n"
  )
  expect_match(server_code, "AquaCache::upsertImportProfile", fixed = TRUE)
  expect_match(server_code, "new_profile_mapping_fields", fixed = TRUE)
  expect_match(server_code, "new_profile_default_media", fixed = TRUE)
  expect_false(grepl("Column map JSON", server_code, fixed = TRUE))
  expect_match(
    profile_loader_code,
    "AquaCache::getImportProfiles",
    fixed = TRUE
  )
  expect_false(exists("addDiscData_builtin_profiles", envir = env))
  expect_identical(env$addDiscData_parser_family(profile), "long")
})

test_that("mapped-result preview uses source text and lookup labels", {
  env <- add_disc_data_mapping_environment()
  profile <- add_disc_data_test_profile("xlr")
  rows <- env$addDiscData_common_rows(
    source_code = "ALS",
    profile = profile,
    source_location_name = "Wolf Creek",
    sample_date = "2026-09-01",
    sample_time = "10:00",
    source_sample_id = "S1",
    source_parameter_code = "Aluminum, dissolved",
    source_parameter_name = "Aluminum, dissolved",
    source_unit = "mg/L",
    result_raw = "<0.010"
  )
  rows$location_id <- 11L
  rows$parameter_id <- 22L
  rows$sample_fraction_id <- 5L

  display <- env$addDiscData_result_display(
    rows = rows,
    locations = data.frame(
      location_id = 11L,
      location_code = "YT001",
      name = "Wolf Creek",
      alias = "WC"
    ),
    sub_locations = data.frame(
      sub_location_id = integer(),
      sub_location_name = character()
    ),
    parameters = data.frame(
      parameter_id = 22L,
      param_name = "aluminum",
      unit_liquid = "mg/L",
      unit_solid = "mg/kg",
      unit_gas = NA_character_
    ),
    result_types = data.frame(result_type_id = 2L, result_type = "Laboratory"),
    result_conditions = data.frame(
      result_condition_id = 1L,
      result_condition = "Less than"
    ),
    sample_fractions = data.frame(
      sample_fraction_id = 5L,
      sample_fraction = "Dissolved"
    ),
    result_value_types = data.frame(
      result_value_type_id = 1L,
      result_value_type = "Measured"
    ),
    reporting_bases = data.frame(
      reporting_basis_id = integer(),
      reporting_basis = character()
    ),
    chemistry_catalogues = list(
      result_speciations = data.frame(
        result_speciation_id = integer(),
        result_speciation = character()
      ),
      result_targets = data.frame(
        result_target_id = integer(),
        result_target = character()
      )
    ),
    matrix_states = data.frame(
      matrix_state_id = 1L,
      matrix_state_code = "liquid",
      matrix_state_name = "Liquid"
    ),
    laboratories = data.frame(lab_id = 2L, lab_name = "ALS Environmental"),
    media = data.frame(media_id = 1L, media_type = "Water"),
    collection_methods = data.frame(
      collection_method_id = 27L,
      collection_method = "Grab"
    ),
    sample_types = data.frame(sample_type_id = 34L, sample_type = "Routine"),
    protocols_methods = data.frame(
      protocol_id = integer(),
      protocol_name = character()
    ),
    grade_types = data.frame(
      grade_type_id = integer(),
      grade_type_description = character()
    ),
    approval_types = data.frame(
      approval_type_id = integer(),
      approval_type_description = character()
    )
  )

  expect_identical(display$`Source result`, "<0.010")
  expect_identical(display$`Result value`, "")
  expect_identical(display$`Result condition`, "Less than")
  expect_identical(display$Laboratory, "ALS Environmental")
  expect_identical(display$`Sample fraction`, "Dissolved")
  expect_identical(display$`Target unit`, "mg/L")
  expect_false(any(
    c(
      "parameter_id",
      "result_condition",
      "laboratory",
      "conversion",
      "result_offset",
      "source_result_condition_value"
    ) %in%
      names(display)
  ))
})

test_that("mapping persistence accepts all editable mapping details", {
  env <- add_disc_data_mapping_environment()

  expect_true(all(
    c("conversion", "result_offset", "profile_code") %in%
      names(formals(env$addDiscData_upsert_mapping))
  ))
  code <- paste(deparse(body(env$addDiscData_upsert_mapping)), collapse = "\n")
  expect_match(code, "AquaCache::upsertImportParameterMappings", fixed = TRUE)
  expect_match(code, "conversion = as.numeric(conversion)", fixed = TRUE)
  expect_match(code, "result_offset = as.numeric(result_offset)", fixed = TRUE)
  expect_match(code, "profile_code = profile_code", fixed = TRUE)
  expect_false(grepl("INSERT INTO", code, fixed = TRUE))
})

test_that("file uploads write generalized import-run provenance", {
  env <- add_disc_data_mapping_environment()
  server_code <- paste(deparse(body(env$addDiscData)), collapse = "\n")
  upload_code <- paste(
    deparse(body(env$addDiscData_run_upload)),
    collapse = "\n"
  )

  expect_match(server_code, "addDiscData_run_upload(request)", fixed = TRUE)
  expect_match(upload_code, "AquaCache::createImportRun", fixed = TRUE)
  expect_match(upload_code, "AquaCache::appendImportRunRows", fixed = TRUE)
  expect_match(upload_code, "AquaCache::completeImportRun", fixed = TRUE)
  expect_match(
    upload_code,
    "source_adapter_function = \"addDiscData\"",
    fixed = TRUE
  )
})

test_that("negative numeric results are opt-in MDLs with source exclusions", {
  env <- add_disc_data_mapping_environment()
  parsed <- env$addDiscData_parse_result(
    c("-0.25", "-2", "-3"),
    negative_values_are_mdl = TRUE,
    negative_mdl_excluded = c(FALSE, TRUE, TRUE)
  )

  expect_true(is.na(parsed$result[[1]]))
  expect_identical(parsed$result_condition[[1]], 1L)
  expect_equal(parsed$result_condition_value[[1]], 0.25)
  expect_identical(parsed$result[2:3], c(-2, -3))
  expect_true(all(is.na(parsed$result_condition[2:3])))

  unchanged <- env$addDiscData_parse_result(
    "-0.25",
    negative_values_are_mdl = FALSE
  )
  expect_identical(unchanged$result[[1]], -0.25)
  expect_true(is.na(unchanged$result_condition[[1]]))
})

test_that("signed and non-Kelvin temperature parameters are auto-excluded", {
  env <- add_disc_data_mapping_environment()
  parameter <- c(
    "Temperature Celsius",
    "Temperature_K",
    "Temperature_K",
    "Temperature",
    "Temperature K",
    "ORP",
    "redox potential",
    "pH",
    "delta_18O",
    "water depth",
    "gauge pressure",
    "flow velocity",
    "discharge",
    "Dissolved aluminum"
  )

  expect_identical(
    env$addDiscData_negative_mdl_auto_excluded(
      parameter,
      parameter,
      c(
        "deg C", "K", "", "", "deg C", "mV", "mV", "pH", "per mil",
        "m", "kPa", "m/s", "m3/s", "ug/L"
      )
    ),
    c(
      TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
      TRUE, TRUE, FALSE
    )
  )
})

test_that("wide workbook headers retain Unicode-minus units", {
  env <- add_disc_data_mapping_environment()
  minus <- intToUtf8(0x2212)
  micro <- intToUtf8(0x00B5)
  headers <- c(
    paste0("F", minus, "-D (mg L", minus, "1)"),
    paste0("SpC ", micro, "S cm", minus, "1"),
    paste0("Alk mg L", minus, "1 as CaCO3")
  )

  parsed <- env$addDiscData_split_wide_header_units(headers)

  expect_identical(parsed$parameter, c(
    paste0("F", minus, "-D"),
    "SpC",
    "Alk"
  ))
  expect_identical(parsed$unit, c(
    paste0("mg L", minus, "1"),
    paste0(micro, "S cm", minus, "1"),
    paste0("mg L", minus, "1 as CaCO3")
  ))
})

test_that("wide workbook parsing requires its configured sample date", {
  env <- add_disc_data_mapping_environment()
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  workbook <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(workbook, "together")
  openxlsx::writeData(
    workbook,
    "together",
    data.frame(
      No. = "S1",
      datetime = NA_character_,
      ID = "ST01",
      `B-T ppb` = "-1",
      check.names = FALSE
    ),
    colNames = TRUE
  )
  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)

  profile <- add_disc_data_test_profile("wide")
  profile$sheet_name <- "together"
  profile$column_map[[1]] <- list(
    station_code = "ID",
    sample_date = "datetime",
    lab_sample_id = "No.",
    first_result_column = 4L,
    last_result_column = 4L,
    data_start_row = 2L
  )

  expect_error(
    env$addDiscData_parse_upload(path, profile),
    "Every sample row needs a valid sample date/time"
  )
})

test_that("wide workbook parsing preserves worksheet row numbers", {
  env <- add_disc_data_mapping_environment()
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  workbook <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(workbook, "Data")
  openxlsx::writeData(
    workbook,
    "Data",
    data.frame("", "", "", "Dissolved metals", check.names = FALSE),
    startRow = 1L,
    colNames = FALSE
  )
  openxlsx::writeData(
    workbook,
    "Data",
    data.frame(
      `No.` = "S1",
      datetime = "2025-06-10",
      ID = "ST01",
      Aluminum = "1.2",
      check.names = FALSE
    ),
    startRow = 2L,
    colNames = TRUE
  )
  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)

  profile <- add_disc_data_test_profile("wide")
  profile$parser_type <- "wide"
  profile$sheet_name <- "Data"
  profile$header_row <- 2L
  profile$data_start_row <- 3L
  profile$datetime_origin <- "text"
  profile$column_map[[1]] <- list(
    lab_sample_id = "No.",
    sample_date = "datetime",
    station_code = "ID",
    first_result_column = 4L,
    last_result_column = 4L,
    data_start_row = 3L,
    group_header_row = 1L
  )

  parsed <- env$addDiscData_parse_upload(path, profile)

  expect_equal(nrow(parsed), 1L)
  expect_identical(parsed$source_sample_id, "S1")
  expect_identical(parsed$source_parameter_code, "Dissolved metals / Aluminum")
})
