add_cont_data_module_text <- function() {
  paste(
    readLines(
      testthat::test_path(
        "..",
        "..",
        "inst",
        "apps",
        "YGwater",
        "modules",
        "admin",
        "continuousData",
        "addContData.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
}

test_that("UTC offset changes preserve staged wall-clock datetimes", {
  module <- add_cont_data_module_text()

  expect_false(grepl("shift_display_datetime_strings", module, fixed = TRUE))
  expect_false(grepl("previous_data_timezone", module, fixed = TRUE))

  # Spreadsheet values and manual edits remain wall-clock text in staging.
  expect_match(module, "datetime = raw[[datetime_input]]", fixed = TRUE)
  expect_match(module, "data$df[[col]][row] <- value", fixed = TRUE)

  # The current shared offset is instead applied to each job during upload.
  expect_match(module, "for (i in seq_along(jobs))", fixed = TRUE)
  expect_match(
    module,
    "parsed_datetime <- table_datetimes_to_utc(df$datetime, input$UTC_offset)",
    fixed = TRUE
  )
})
