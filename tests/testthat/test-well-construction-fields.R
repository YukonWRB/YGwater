test_that("simplerIndex carries seal and screen construction fields to AquaCache", {
  module <- paste(
    readLines(
      testthat::test_path(
        "..",
        "..",
        "inst",
        "apps",
        "YGwater",
        "modules",
        "admin",
        "boreholes_wells",
        "simplerIndex.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  for (input_id in c(
    "seal_material",
    "seal_diameter",
    "seal_depth_from",
    "seal_depth_to",
    "screen_material",
    "screen_type"
  )) {
    expect_match(module, paste0('ns("', input_id, '")'), fixed = TRUE)
    expect_match(module, paste0("input$", input_id), fixed = TRUE)
  }
  for (argument in c(
    "seal_material",
    "seal_diameter_mm",
    "seal_depth_from",
    "seal_depth_to",
    "screen_material",
    "screen_type"
  )) {
    expect_match(module, paste0(argument, " = metadata[["), fixed = TRUE)
  }
})

test_that("WWR cache and popup expose well construction details", {
  cache_module <- paste(
    readLines(
      testthat::test_path(
        "..",
        "..",
        "inst",
        "apps",
        "YGwater",
        "modules",
        "cache_functions.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  registry_module <- paste(
    readLines(
      testthat::test_path(
        "..",
        "..",
        "inst",
        "apps",
        "YGwater",
        "modules",
        "client",
        "WWR",
        "registry_front_end.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(cache_module, "LEFT JOIN boreholes.seal_materials", fixed = TRUE)
  expect_match(cache_module, "LEFT JOIN boreholes.screen_materials", fixed = TRUE)
  expect_match(cache_module, "LEFT JOIN boreholes.screen_types", fixed = TRUE)
  expect_match(registry_module, "Seal material:", fixed = TRUE)
  expect_match(registry_module, "Screen material:", fixed = TRUE)
  expect_match(registry_module, "Screen type:", fixed = TRUE)
})
