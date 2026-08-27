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
    "disc_qaqc_linked_results"
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

  tables <- env$disc_plot_download_tables(
    data.frame(sample_id = 1L),
    qaqc
  )

  expect_named(
    tables,
    c("plot_data", "qaqc_group_links", "qaqc_samples", "qaqc_results")
  )

  workbook <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(tables, workbook)
  expect_identical(
    openxlsx::getSheetNames(workbook),
    c("plot_data", "qaqc_group_links", "qaqc_samples", "qaqc_results")
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
