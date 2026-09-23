add_disc_data_mapping_environment <- function() {
  module_path <- system.file(
    "apps/YGwater/modules/admin/discreteData/addDiscData.R",
    package = "YGwater"
  )
  env <- new.env(parent = asNamespace("shiny"))
  sys.source(module_path, envir = env)
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

  choices <- env$addDiscData_parameter_choices(parameters)

  expect_identical(unname(choices[[1]]), "")
  expect_identical(names(choices)[[1]], "Select AquaCache parameter")
  expect_match(names(choices)[[2]], "Liquid: mg/l", fixed = TRUE)
  expect_identical(
    env$addDiscData_target_unit(parameters, c(10L, 10L), c(1L, 2L)),
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
      result_speciation_id = NA_integer_,
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
      result_speciation_id = NA_integer_,
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
    c("ALS\rshared", "OTHER\rshared")
  )
  json <- env$addDiscData_profile_json(profile, "column_map")
  expect_true(jsonlite::validate(json))
  expect_match(json, '"parameter_code"', fixed = TRUE)

  server_code <- paste(deparse(body(env$addDiscData)), collapse = "\n")
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
    result_speciations = data.frame(
      result_speciation_id = integer(),
      result_speciation = character()
    ),
    matrix_states = data.frame(
      matrix_state_id = 1L,
      matrix_state_name = "Liquid"
    ),
    laboratories = data.frame(lab_id = 2L, lab_name = "ALS Environmental"),
    media = data.frame(media_id = 1L, media_type = "Water"),
    collection_methods = data.frame(
      collection_method_id = 27L,
      collection_method = "Grab"
    ),
    sample_types = data.frame(sample_type_id = 34L, sample_type = "Routine")
  )

  expect_identical(display$`Source result`, "<0.010")
  expect_identical(display$Result, "")
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

  expect_match(server_code, "AquaCache::createImportRun", fixed = TRUE)
  expect_match(server_code, "AquaCache::appendImportRunRows", fixed = TRUE)
  expect_match(server_code, "AquaCache::completeImportRun", fixed = TRUE)
  expect_match(
    server_code,
    "source_adapter_function = \"addDiscData\"",
    fixed = TRUE
  )
})
