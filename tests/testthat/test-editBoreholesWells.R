test_that("editBoreholesWells selects filtered and sorted rows by record key", {
  module <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/admin/boreholes_wells/editBoreholesWells.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(module, 'ns("records_table_record_key")', fixed = TRUE)
  expect_match(
    module,
    "table.on('click', 'tbody tr', function() {",
    fixed = TRUE
  )
  expect_match(module, "table.row(this).data()", fixed = TRUE)
  expect_match(module, "rowData[0]", fixed = TRUE)
  expect_false(grepl("select.dt", module, fixed = TRUE))
  expect_match(
    module,
    "observeEvent(input$records_table_record_key, {",
    fixed = TRUE
  )
  expect_match(
    module,
    "record_key %in% moduleData$records$record_key",
    fixed = TRUE
  )
  expect_match(
    module,
    "update_record_selector(selected = record_key)",
    fixed = TRUE
  )
  expect_match(module, "server = TRUE", fixed = TRUE)
  expect_false(grepl(
    "moduleData$records$record_key[idx]",
    module,
    fixed = TRUE
  ))
})
