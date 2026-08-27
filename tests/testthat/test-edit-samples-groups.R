edit_samples_module_environment <- function() {
  module_path <- system.file(
    "apps/YGwater/modules/admin/discreteData/editSamples.R",
    package = "YGwater"
  )

  env <- new.env(parent = asNamespace("shiny"))
  sys.source(module_path, envir = env)
  env
}

test_that("sample-group synchronization changes only selected memberships", {
  env <- edit_samples_module_environment()

  changes <- env$edit_samples_group_link_changes(
    existing_ids = c(10L, 20L),
    selected_ids = c(20L, 30L, 30L, NA_integer_)
  )

  expect_identical(changes$remove, 10L)
  expect_identical(changes$add, 30L)
})
