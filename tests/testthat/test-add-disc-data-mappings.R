add_disc_data_mapping_environment <- function() {
  module_path <- system.file(
    "apps/YGwater/modules/admin/discreteData/addDiscData.R",
    package = "YGwater"
  )
  env <- new.env(parent = asNamespace("shiny"))
  sys.source(module_path, envir = env)
  env
}

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
  profile <- env$addDiscData_builtin_profiles()[3, , drop = FALSE]
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
  env$addDiscData_fetch_mappings <- function(con, source_code) {
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

  mapped <- env$addDiscData_apply_mappings(rows, con = NULL)
  mapped_again <- env$addDiscData_apply_mappings(mapped, con = NULL)

  expect_equal(mapped$result[[1]], 0.51)
  expect_equal(mapped$result_condition_value[[2]], 0.51)
  expect_equal(mapped_again$result, mapped$result)
  expect_equal(mapped_again$result_condition_value, mapped$result_condition_value)
  expect_identical(mapped$sample_fraction_id, c(5L, 5L))
  expect_identical(mapped$mapping_status, c("mapped", "mapped"))
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
    "Analyte", "ALS Sample ID", "Client Sample ID", "Matrix", "Sub-Matrix",
    "Method", "Results", "Detection Limit", "Units", "Qual", "Date Sampled",
    "Time Sampled", "Prep Date", "Analysis Date"
  )
  raw <- matrix("", nrow = 10L, ncol = length(header))
  raw[1:6, 1] <- paste("Report heading", 1:6)
  raw[7, ] <- header
  raw[8, ] <- c(
    "Aluminum, dissolved", "S1", "Station A", "Water", "", "ME-ICP",
    "0.1", "0.01", "mg/L", "", "2026-09-01", "10:00", "", "2026-09-02"
  )
  raw[9, ] <- c(
    "Dissolved metals filtration location", "S1", "Station A", "Water", "",
    "", "Field", "", "", "", "2026-09-01", "10:00", "", ""
  )
  raw[10, ] <- c(
    "Dissolved mercury filtration location", "S1", "Station A", "Water", "",
    "", "Lab", "", "", "", "2026-09-01", "10:00", "", ""
  )
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Detailed Report")
  openxlsx::writeData(wb, "Detailed Report", raw, colNames = FALSE)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)

  profile <- env$addDiscData_builtin_profiles()[3, , drop = FALSE]
  profile$profile_code <- "custom_xlr_copy"
  parsed <- env$addDiscData_parse_upload(path, profile)

  expect_equal(nrow(parsed), 1L)
  expect_identical(parsed$source_parameter_code, "Aluminum, dissolved")
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
  expect_identical(assigned$sub_location_id, c(NA_integer_, NA_integer_, 8L, 8L))

  rows$source_location_name <- "WC"
  ambiguous <- env$addDiscData_location_match(rows, locations)
  expect_true(all(is.na(ambiguous$location_id)))
})

test_that("profile keys are source-specific and profile JSON is editable", {
  env <- add_disc_data_mapping_environment()
  profile <- env$addDiscData_builtin_profiles()[1, , drop = FALSE]

  expect_identical(
    env$addDiscData_profile_key(c("ALS", "OTHER"), c("shared", "shared")),
    c("ALS\rshared", "OTHER\rshared")
  )
  json <- env$addDiscData_profile_json(profile, "column_map")
  expect_true(jsonlite::validate(json))
  expect_match(json, '"parameter_code"', fixed = TRUE)

  server_code <- paste(deparse(body(env$addDiscData)), collapse = "\n")
  expect_match(server_code, "AquaCache::upsertImportProfile", fixed = TRUE)
  expect_match(server_code, "new_profile_column_map", fixed = TRUE)
})

test_that("mapped-result preview uses source text and lookup labels", {
  env <- add_disc_data_mapping_environment()
  profile <- env$addDiscData_builtin_profiles()[3, , drop = FALSE]
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
    matrix_states = data.frame(matrix_state_id = 1L, matrix_state_name = "Liquid"),
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
  expect_false(any(c(
    "parameter_id",
    "result_condition",
    "laboratory",
    "conversion",
    "result_offset",
    "source_result_condition_value"
  ) %in% names(display)))
})

test_that("mapping persistence accepts all editable mapping details", {
  env <- add_disc_data_mapping_environment()

  expect_true(all(
    c("conversion", "result_offset") %in% names(formals(env$addDiscData_upsert_mapping))
  ))
  code <- paste(deparse(body(env$addDiscData_upsert_mapping)), collapse = "\n")
  expect_match(code, "conversion = EXCLUDED.conversion", fixed = TRUE)
  expect_match(code, "result_offset = EXCLUDED.result_offset", fixed = TRUE)
  expect_false(grepl("source_name = EXCLUDED.source_name", code, fixed = TRUE))
})
