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
    "Composite results must be changed through their aggregation and component records.",
    fixed = TRUE
  )
})
