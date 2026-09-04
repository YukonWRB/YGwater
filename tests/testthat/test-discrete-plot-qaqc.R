discrete_plot_module_environment <- function() {
  module_path <-
    system.file(
      "apps/YGwater/modules/client/plot/discretePlot.R",
      package = "YGwater"
    )

  env <- new.env(parent = asNamespace("shiny"))
  sys.source(module_path, envir = env)
  env
}

test_that("QA/QC module text is sourced from the translation catalogue", {
  translations <- data$translations
  keys <- c(
    "disc_qaqc_show_data",
    "disc_qaqc_sample_data",
    "disc_qaqc_group_links",
    "disc_qaqc_retrieval_error",
    "disc_qaqc_linked_results",
    "disc_result_components"
  )
  entries <- suppressWarnings(translations$English[
    names(translations$English) %in% keys
  ])

  expect_setequal(names(entries), keys)
  expect_equal(length(entries), length(keys))

  module_paths <- c(
    system.file(
      "apps/YGwater/modules/client/plot/discretePlot.R",
      package = "YGwater"
    ),
    system.file(
      "apps/YGwater/modules/client/data/discreteData.R",
      package = "YGwater"
    )
  )
  module_code <- unlist(lapply(module_paths, readLines, warn = FALSE))
  expect_false(any(grepl("qaqc_text", module_code, fixed = TRUE)))
})

test_that("plot QA/QC tabularization preserves its input", {
  env <- discrete_plot_module_environment()
  input <- data.table::data.table(
    sample_id = 1L,
    share_with = list(c("reader", "reviewer"))
  )

  output <- env$disc_plot_tabularize(input)

  expect_type(input$share_with, "list")
  expect_identical(output$share_with, "reader, reviewer")
})

test_that("plot QA/QC uses contributing IDs for averaged rows", {
  env <- discrete_plot_module_environment()
  plot_result <- list(
    data = data.frame(sample_id = NA_integer_),
    source_sample_ids = c(7L, NA_integer_, 5L, 7L)
  )

  expect_identical(env$disc_plot_source_sample_ids(plot_result), c(5L, 7L))

  plot_result$source_result_ids <- c(12L, NA_integer_, 9L, 12L)
  expect_identical(env$disc_plot_source_result_ids(plot_result), c(9L, 12L))
})

test_that("plot downloads include applicable QA/QC tables", {
  env <- discrete_plot_module_environment()
  qaqc <- env$disc_plot_empty_qaqc_data()
  qaqc$group_links <- data.frame(
    plotted_sample_id = 1L,
    qaqc_sample_id = 2L
  )
  qaqc$samples <- data.frame(sample_id = 2L, location_id = NA_integer_)
  qaqc$results <- data.frame(result_id = 10L, sample_id = 2L)
  qaqc$result_components <- data.frame(
    result_component_id = 100L,
    result_id = 10L
  )
  components <- data.frame(
    result_component_id = 99L,
    result_id = 9L
  )

  tables <- env$disc_plot_download_tables(
    data.frame(sample_id = 1L),
    qaqc,
    components
  )

  expect_named(
    tables,
    c(
      "plot_data",
      "result_components",
      "qaqc_group_links",
      "qaqc_samples",
      "qaqc_results",
      "qaqc_result_components"
    )
  )

  workbook <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(tables, workbook)
  expect_identical(
    openxlsx::getSheetNames(workbook),
    c(
      "plot_data",
      "result_components",
      "qaqc_group_links",
      "qaqc_samples",
      "qaqc_results",
      "qaqc_result_components"
    )
  )
})

test_that("component retrieval handles an empty result selection", {
  env <- discrete_plot_module_environment()

  expect_s3_class(
    env$disc_result_components(NULL, c(NA_integer_, NA_integer_)),
    "data.frame"
  )
  expect_equal(
    nrow(env$disc_result_components(NULL, integer())),
    0L
  )
})

test_that("plot downloads omit QA/QC sheets when no blank is linked", {
  env <- discrete_plot_module_environment()

  tables <- env$disc_plot_download_tables(
    data.frame(sample_id = 1L),
    env$disc_plot_empty_qaqc_data()
  )

  expect_named(tables, "plot_data")
})
