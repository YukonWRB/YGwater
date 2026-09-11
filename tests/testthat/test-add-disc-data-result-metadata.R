add_disc_data_module_environment <- function() {
  module_path <- system.file(
    "apps/YGwater/modules/admin/discreteData/addDiscData.R",
    package = "YGwater"
  )

  env <- new.env(parent = asNamespace("shiny"))
  sys.source(module_path, envir = env)
  env
}

test_that("discrete import rows retain laboratory result identifiers", {
  env <- add_disc_data_module_environment()
  profile <- data.frame(timezone = "UTC", stringsAsFactors = FALSE)
  profile$defaults <- I(list(list(
    media_id = 1L,
    collection_method = 27L,
    sample_type = 34L,
    owner = 1L,
    result_type = 2L,
    matrix_state_id = 1L,
    result_value_type = 1L,
    laboratory = 2L
  )))

  row <- env$addDiscData_common_rows(
    source_code = "ALS",
    profile = profile,
    source_location_name = "Station A",
    sample_date = "2026-09-01",
    sample_time = "10:30",
    source_sample_id = "L12345-1",
    lab_report_no = "L12345",
    source_parameter_code = "AG-DIS",
    source_parameter_name = "Silver, dissolved",
    source_unit = "mg/L",
    result_raw = "0.01"
  )

  expect_identical(row$lab_report_no, "L12345")
  expect_identical(row$lab_sample_no, "L12345-1")
  expect_true(all(c("grade_type_id", "approval_type_id") %in% names(row)))
  expect_true(is.na(row$grade_type_id))
  expect_true(is.na(row$approval_type_id))
})

test_that("discrete import persists finalized Patch 60 result metadata", {
  module_code <- readLines(
    system.file(
      "apps/YGwater/modules/admin/discreteData/addDiscData.R",
      package = "YGwater"
    ),
    warn = FALSE
  )
  code <- paste(module_code, collapse = "\n")

  expect_match(code, "lab_report_no", fixed = TRUE)
  expect_match(code, "lab_sample_no", fixed = TRUE)
  expect_match(code, "grade_type_id", fixed = TRUE)
  expect_match(code, "approval_type_id", fixed = TRUE)
  expect_false(grepl("result_qualifiers", code, fixed = TRUE))
})
