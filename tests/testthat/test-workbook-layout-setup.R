workbook_layout_environment <- function() {
  env <- new.env(parent = asNamespace("YGwater"))
  for (file in c("addDiscData.R", "importSetup.R")) sys.source(system.file(
    "apps/aqualink/modules/admin/discreteData", file, package = "YGwater"), env)
  env
}

test_that("wide samples preserve metal fractions and omit summary rows", {
  env <- workbook_layout_environment()
  raw <- rbind(c("", "", "Dissolved Metals", "Total Metals"),
    c("Client Sample ID", "Sample Collect Date", "Aluminum", "Aluminum"),
    c("River A", "45000", "<0.01", "0.2"),
    c("River B", "45001", "0", "0.3"), c("", "mean", "0.1", "0.25"))
  suggested <- env$importSetup_suggest_layout(as.data.frame(raw))
  expect_identical(suggested$parser_family, "wide")
  expect_identical(suggested$header_row, 2L)
  expect_identical(suggested$column_map$group_header_row, 1L)
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  openxlsx::write.xlsx(as.data.frame(raw), path, colNames = FALSE)
  profile <- data.frame(source_code = "test", profile_code = "wide", header_row = 2L,
    timezone = "Etc/GMT+7", sheet_strategy = "first")
  profile$column_map <- I(list(suggested$column_map))
  profile$validation_rules <- I(list(list(parser_family = "wide")))
  parsed <- env$addDiscData_parse_upload(path, profile)
  expect_equal(nrow(parsed), 4L)
  expect_equal(length(unique(parsed$sample_key)), 2L)
  expect_setequal(parsed$source_parameter_code,
    c("Dissolved Metals / Aluminum", "Total Metals / Aluminum"))
  expect_setequal(parsed$source_row_number, 3:4)
  expect_true(all(is.na(parsed$source_unit)))
  expect_true(all(!is.na(parsed$datetime)))
  expect_equal(parsed$source_result_text[parsed$source_location_name == "River B" &
    parsed$source_parameter_code == "Dissolved Metals / Aluminum"], "0")
  profile$column_map[[1]]$data_end_row <- 3L
  expect_equal(nrow(env$addDiscData_parse_upload(path, profile)), 2L)
  profile$column_map[[1]]$group_header_row <- NULL
  expect_error(env$addDiscData_parse_upload(path, profile), "same name.*group heading")
  # Worksheet row numbers must include leading blank rows, as in the example preview.
  openxlsx::write.xlsx(as.data.frame(raw), path, colNames = FALSE, startRow = 2)
  profile$header_row <- 3L
  profile$column_map[[1]] <- suggested$column_map
  profile$column_map[[1]]$group_header_row <- 2L
  profile$column_map[[1]]$data_start_row <- 4L
  parsed <- env$addDiscData_parse_upload(path, profile)
  expect_equal(nrow(parsed), 4L)
  expect_setequal(parsed$source_row_number, 4:5)
})

test_that("layout suggestions retain source headings and distinguish long and XLR", {
  env <- workbook_layout_environment()
  long <- as.data.frame(rbind(
    c("Station_Code", "Smpl_CollectDate", "Lab_Smpl_#", "Lab_Param_Code", "Result", "Units"),
    c("River", "2026-Jul-01", "L001", "Al-D", "0.2", "mg/L")))
  suggested <- env$importSetup_suggest_layout(long)
  expect_identical(suggested$parser_family, "long")
  expect_identical(suggested$column_map$parameter_code, "Lab_Param_Code")
  xlr <- as.data.frame(rbind(rep("", 7),
    c("Analyte", "ALS Sample ID", "Client Sample ID", "Date Sampled", "Units", "Results", "Qual")))
  suggested <- env$importSetup_suggest_layout(xlr)
  expect_identical(suggested$parser_family, "xlr")
  expect_identical(suggested$header_row, 2L)
  expect_identical(suggested$column_map$result_flag, "Qual")
})

test_that("workbook lookups accept source-wide EQWin parameter criteria", {
  env <- workbook_layout_environment()
  expect_identical(env$addDiscData_mapping_keys('{"ParamCode":"Al-D","input_unit":"mg/L"}'), "al-d\rmg/l")
})

test_that("multi-column criteria preserve and match independent source dimensions", {
  env <- workbook_layout_environment()
  source <- data.frame(param_name = rep("Nitrogen", 4), unit = rep("mg/L", 4),
    sample_fraction = c("dissolved", "total", "dissolved", "dissolved"),
    result_speciation = c("nitrate", "nitrate", "nitrite", "nitrate"),
    result_type = c("lab", "lab", "lab", "field"))
  fields <- names(source)
  preserved <- env$addDiscData_source_matching_values(source, fields)
  criteria <- env$importSetup_column_criteria(fields, unname(as.character(source[1, ])))
  expect_named(criteria, paste0("source.", fields))
  expect_identical(criteria$source.result_type, "lab")
  rows <- env$addDiscData_empty_table()[rep(NA_integer_, 4), ]
  rows$source_parameter_code <- "Nitrogen"
  rows$source_unit <- "mg/L"
  rows$source_matching_values <- preserved
  mappings <- data.frame(source_match = as.character(jsonlite::toJSON(criteria, auto_unbox = TRUE)),
    priority = 50L, profile_specific = FALSE)
  expect_identical(env$addDiscData_mapping_indices(rows, mappings), c(1L, NA_integer_, NA_integer_, NA_integer_))
  expect_error(env$importSetup_column_criteria(c("unit", "unit"), c("mg/L", "mg/L")), "unique source column")
  expect_error(env$addDiscData_source_matching_values(source, "absent"), "missing from the worksheet")
  source$Station <- "River A"
  source$Date <- "2026-07-01"
  source$Sample <- "sample-1"
  source$Result <- "1.25"
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  openxlsx::write.xlsx(source, path)
  profile <- data.frame(source_code = "other_cache", profile_code = "export", header_row = 1L,
    timezone = "Etc/GMT+7", sheet_strategy = "first")
  profile$column_map <- I(list(list(station_code = "Station", sample_date = "Date", lab_sample_id = "Sample",
    parameter_code = "param_name", unit = "unit", result = "Result", match_columns = fields)))
  profile$validation_rules <- I(list(list(parser_family = "long")))
  parsed <- env$addDiscData_parse_upload(path, profile)
  expect_identical(parsed$source_matching_values, preserved)
  expect_true(all(is.na(parsed$result_speciation_id)))
  expect_identical(env$addDiscData_mapping_indices(parsed, mappings), c(1L, NA_integer_, NA_integer_, NA_integer_))
  # Missing preserved source fields must never collapse to a name/unit match.
  rows$source_matching_values[[1]] <- "{}"
  expect_true(all(is.na(env$addDiscData_mapping_indices(rows, mappings))))
})

test_that("mapping scope, specificity, and ambiguous matches are explicit", {
  env <- workbook_layout_environment()
  rows <- env$addDiscData_empty_table()[NA_integer_, ]
  rows$source_parameter_code <- "pH-L"
  rows$source_unit <- "pH units"
  rows$source_matching_values <- '{}'
  mappings <- data.frame(source_match = c('{"ParamCode":"pH-L","input_unit":"pH units"}',
    '{"parameter_code":"pH-L"}'), priority = 50L, profile_specific = FALSE)
  expect_identical(env$addDiscData_mapping_indices(rows, mappings), 1L)
  mappings$profile_specific[[2]] <- TRUE
  expect_identical(env$addDiscData_mapping_indices(rows, mappings), 2L)
  mappings[2, ] <- mappings[1, ]
  expect_error(env$addDiscData_mapping_indices(rows, mappings), "Multiple parameter mappings")
  rows <- rows[rep(1L, 2L), ]
  rows$source_unit <- c("", "mg/L")
  mapping <- mappings[1, ]
  mapping$source_match <- '{"parameter_code":"pH-L","unit":""}'
  expect_identical(env$addDiscData_mapping_indices(rows, mapping), c(1L, NA_integer_))
  mapping$source_match <- '{"parameter_code":"pH-L"}'
  expect_identical(env$addDiscData_mapping_indices(rows, mapping), c(1L, 1L))
})
