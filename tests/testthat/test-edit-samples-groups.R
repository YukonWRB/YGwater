edit_samples_module_environment <- function() {
  module_path <- system.file(
    "apps/YGwater/modules/admin/discreteData/editSamples.R",
    package = "YGwater"
  )

  env <- new.env(parent = asNamespace("shiny"))
  sys.source(module_path, envir = env)
  env
}

test_that("sample association synchronization changes only selected links", {
  env <- edit_samples_module_environment()

  changes <- env$edit_samples_link_changes(
    existing_ids = c(10L, 20L),
    selected_ids = c(20L, 30L, 30L, NA_integer_)
  )

  expect_identical(changes$remove, 10L)
  expect_identical(changes$add, 30L)
})

test_that("sample editor uses normalized qualifiers and observers", {
  module_code <- readLines(
    system.file(
      "apps/YGwater/modules/admin/discreteData/editSamples.R",
      package = "YGwater"
    ),
    warn = FALSE
  )
  code <- paste(module_code, collapse = "\n")

  expect_match(code, "discrete.sample_qualifiers", fixed = TRUE)
  expect_match(code, "discrete.sample_observers", fixed = TRUE)
  expect_false(grepl(
    "s\\.sample_qualifier\\b|sample_qualifier\\s*=\\s*\\$",
    code,
    perl = TRUE
  ))
  expect_match(
    code,
    "The canonical value and analytical identity are maintained through the aggregation and its components.",
    fixed = TRUE
  )
  expect_match(code, "r.lab_report_no", fixed = TRUE)
  expect_match(code, "r.lab_sample_no", fixed = TRUE)
  expect_match(code, "r.grade_type_id", fixed = TRUE)
  expect_match(code, "r.approval_type_id", fixed = TRUE)
  expect_match(code, "save_composite_result_metadata", fixed = TRUE)
  expect_match(code, "observer.organization", fixed = TRUE)
  expect_false(grepl(
    "observer\\.organization\\s*=\\s*organization\\.organization_id",
    code,
    perl = TRUE
  ))
  expect_false(grepl("result_qualifiers", code, fixed = TRUE))
  expect_match(code, "discrete.sample_group_members", fixed = TRUE)
  expect_false(grepl("linked_with", code, fixed = TRUE))
})

test_that("duplicate averaging uses replicate-set memberships", {
  code <- paste(deparse(body(plotDiscrete)), collapse = "\n")

  expect_match(code, "replicate_set", fixed = TRUE)
  expect_match(code, "replicate_group_id", fixed = TRUE)
  expect_false(grepl("linked_with", code, fixed = TRUE))
})
