test_that("Add Location offers automatic elevation lookup", {
  module_path <- system.file(
    "apps/YGwater/modules/admin/locations/addLocation.R",
    package = "YGwater"
  )
  module <- paste(readLines(module_path, warn = FALSE), collapse = "\n")

  expect_match(
    module,
    "Elevation conversion (meters; leave blank to estimate)",
    fixed = TRUE
  )
  expect_match(
    module,
    "AquaCache will estimate it from the coordinates",
    fixed = TRUE
  )
  expect_match(
    module,
    'automatic_elevation <- identical(input$mode, "add")',
    fixed = TRUE
  )
  expect_match(
    module,
    "conversion_m = if (automatic_elevation) NA_real_ else input$elev",
    fixed = TRUE
  )
  expect_match(
    module,
    "added_location <- AquaCache::addACLocation",
    fixed = TRUE
  )
})
