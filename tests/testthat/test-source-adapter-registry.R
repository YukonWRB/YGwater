test_that("source-function selectors use the database registry by data domain", {
  module_paths <- c(
    continuous = testthat::test_path(
      "..", "..", "inst", "apps", "YGwater", "modules", "admin",
      "continuousData", "addTimeseries.R"
    ),
    discrete = testthat::test_path(
      "..", "..", "inst", "apps", "YGwater", "modules", "admin",
      "discreteData", "addSampleSeries.R"
    ),
    image = testthat::test_path(
      "..", "..", "inst", "apps", "YGwater", "modules", "admin",
      "imgupload", "addImgSeries.R"
    )
  )

  for (data_domain in names(module_paths)) {
    module_text <- paste(
      readLines(module_paths[[data_domain]], warn = FALSE),
      collapse = "\n"
    )

    expect_match(
      module_text,
      "AquaCache::getSourceAdapterCapabilities",
      fixed = TRUE,
      info = data_domain
    )
    expect_match(
      module_text,
      paste0('data_domain = "', data_domain, '"'),
      fixed = TRUE,
      info = data_domain
    )
    expect_false(
      grepl(
        'ls(getNamespace("AquaCache"))',
        module_text,
        fixed = TRUE
      ),
      info = data_domain
    )
    expect_match(
      module_text,
      "public.source_adapter_capabilities",
      fixed = TRUE,
      info = data_domain
    )
  }
})
