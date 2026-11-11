existing_discrete_sample_module <- function() {
  env <- new.env(parent = asNamespace("YGwater"))
  sys.source(system.file("apps/aqualink/modules/admin/discreteData/addDiscData.R", package = "YGwater"), env)
  env
}

test_that("parsed workbook rows start without an existing destination sample", {
  env <- existing_discrete_sample_module()
  profile <- data.frame(timezone = "UTC")
  profile$defaults <- I(list(list(media_id = 1L, collection_method = 27L,
    sample_type = 34L, owner = 1L, result_type = 2L, matrix_state_id = 1L,
    result_value_type = 1L)))
  rows <- env$addDiscData_common_rows(source_code = "LAB", profile = profile,
    source_location_name = "Site", sample_date = "2026-11-09", sample_time = "12:00:00",
    source_sample_id = "LAB-1", source_parameter_code = "A", source_parameter_name = "Analyte",
    source_unit = "mg/L", result_raw = "12")
  expect_identical(rows$existing_sample_id, NA_integer_)
  expect_identical(rows$existing_sample_version, NA_character_)
})

test_that("selecting an existing sample preserves workbook/result identifiers", {
  env <- existing_discrete_sample_module()
  rows <- env$addDiscData_empty_table()[rep(NA_integer_, 2L), ]
  rows$sample_key <- "lab-sample"
  rows$source_sample_id <- "LAB-123"
  rows$source_row_number <- 2:3
  rows$source_code <- "Laboratory"
  rows$lab_sample_no <- "LAB-123"
  rows$result <- c(12, 34)
  rows$sample_group_id <- 99L
  original <- rows
  target <- rows[1L, ]
  target$sample_id <- 12L
  target$sample_version <- "2026-11-09 12:00:00+00"
  target$datetime <- as.POSIXct("2026-11-01 10:00:00", tz = "UTC")
  target$media_id <- 1L
  target$location_id <- 3L
  target$sample_note <- "Existing collection metadata"
  staged <- env$addDiscData_use_existing_sample(rows, "lab-sample", target)
  expect_identical(rows, original)
  expect_identical(staged$existing_sample_id, c(12L, 12L))
  expect_identical(staged$existing_sample_version, rep(target$sample_version, 2L))
  expect_identical(staged$datetime, rep(target$datetime, 2L))
  expect_identical(staged$source_sample_id, original$source_sample_id)
  expect_identical(staged$lab_sample_no, original$lab_sample_no)
  expect_identical(staged$result, original$result)
  expect_true(all(is.na(staged$sample_group_id)))
  expect_identical(staged$sample_note, rep(target$sample_note, 2L))
})

test_that("refreshing mappings retains the explicit destination sample", {
  env <- existing_discrete_sample_module()
  previous <- env$addDiscData_empty_table()[NA_integer_, ]
  previous$source_sample_id <- "LAB-123"
  previous$sample_key <- "source-key"
  previous$source_row_number <- 2L
  previous$source_parameter_code <- "A"
  previous$source_unit <- "mg/L"
  current <- previous
  current$existing_sample_id <- 12L
  current$existing_sample_version <- "version"
  current$datetime <- as.POSIXct("2026-11-01", tz = "UTC")
  refreshed <- previous
  refreshed$parameter_id <- 5L
  result <- env$addDiscData_merge_preview_edits(refreshed, current, previous)
  expect_identical(result$existing_sample_id, 12L)
  expect_identical(result$existing_sample_version, "version")
  expect_identical(result$datetime, current$datetime)
  expect_identical(result$parameter_id, 5L)
})
