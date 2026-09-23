data_download_cache_environment <- function() {
  env <- new.env(parent = asNamespace("shiny"))
  sys.source(
    system.file(
      "apps/YGwater/modules/cache_functions.R",
      package = "YGwater"
    ),
    envir = env
  )
  env
}

test_that("all-sentinel selections normalize deterministically", {
  env <- data_download_cache_environment()

  expect_identical(env$data_filter_normalize_selection(NULL), "all")
  expect_identical(env$data_filter_normalize_selection("all"), "all")
  expect_identical(
    env$data_filter_normalize_selection(c("all", "13")),
    "13"
  )
  expect_identical(
    env$data_filter_normalize_selection(c("13", "all")),
    "all"
  )
  expect_identical(
    env$data_filter_normalize_selection(c("13", "2")),
    c("13", "2")
  )
})

test_that("select-all follows the table's real selection", {
  env <- data_download_cache_environment()

  expect_identical(env$data_table_toggle_all_rows(NULL, 3L), 1:3)
  expect_identical(env$data_table_toggle_all_rows(c(1L, 3L), 3L), 1:3)
  expect_null(env$data_table_toggle_all_rows(3:1, 3L))
  expect_null(env$data_table_toggle_all_rows(NULL, 0L))
})

test_that("continuous and discrete modules use stateless select-all", {
  module_paths <- c(
    system.file(
      "apps/YGwater/modules/client/data/continuousData.R",
      package = "YGwater"
    ),
    system.file(
      "apps/YGwater/modules/client/data/discreteData.R",
      package = "YGwater"
    )
  )

  for (path in module_paths) {
    code <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_match(code, "data_table_toggle_all_rows(", fixed = TRUE)
    expect_false(grepl("select_all <- reactiveVal", code, fixed = TRUE))
    expect_match(code, "data_filter_normalize_selection", fixed = TRUE)
  }
})

test_that("discrete downloads use current metadata contracts", {
  module_code <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/client/data/discreteData.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  helper_code <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/client/plot/discretePlot.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(module_code, "disc_sample_metadata(", fixed = TRUE)
  expect_match(module_code, "disc_result_metadata(", fixed = TRUE)
  expect_match(module_code, "disc_sample_documents(", fixed = TRUE)
  expect_match(module_code, "disc_sample_group_memberships(", fixed = TRUE)
  expect_match(helper_code, "discrete.samples_metadata_", fixed = TRUE)
  expect_match(helper_code, "discrete.results_metadata_", fixed = TRUE)
  expect_match(helper_code, "discrete.sample_documents", fixed = TRUE)
  expect_match(helper_code, "discrete.sample_group_members", fixed = TRUE)
})
