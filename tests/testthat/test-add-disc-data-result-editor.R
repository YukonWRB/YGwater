# Exercise the module's actual reactive expressions without a database. Reference
# catalogues are small fixtures; selection, rendering, validation and saving are
# the expressions used by the live module.
discrete_result_editor_fixture <- function(
  sample_form = FALSE,
  tables = FALSE
) {
  env <- new.env(parent = asNamespace("YGwater"))
  for (file in c("chemistry_form.R", "discrete_protocols.R", "addDiscData.R")) {
    sys.source(
      system.file(
        "apps/aqualink/modules/admin/discreteData",
        file,
        package = "YGwater"
      ),
      env
    )
  }
  env$page_fluid <- bslib::page_fluid
  env$tooltip <- bslib::tooltip
  statements <- as.list(body(env$addDiscData)[[2]][[3]][[3]])[-1]
  assignments <- c(
    "selected_result_index",
    "result_form_revision",
    "sample_form_revision",
    "selected_sample_key",
    "sample_location_rows",
    "selected_sample_row",
    "selected_result_rows",
    "selected_results",
    "selected_result_row",
    "parameter_requirement",
    "is_field_result_type"
  )
  outputs <- c("result_editor", "manual_lab_metadata", "sample_editor_state")
  if (sample_form) {
    outputs <- c(outputs, "sample_editor")
  }
  if (tables) {
    assignments <- c(assignments, "result_display", "result_parameter_choices")
    outputs <- c(
      outputs,
      "sample_location_summary",
      "data_table",
      "manual_target_unit"
    )
  }
  events <- list(
    quote(input$sample_location_summary_rows_selected),
    quote(input$data_table_rows_selected),
    quote(input$new_result),
    quote(input$new_sample),
    quote(data$preview_base),
    quote(selected_sample_key()),
    quote(input$add_manual_result)
  )
  if (tables) {
    events <- c(
      events,
      list(
        quote(input$data_table_cell_edit),
        quote(input$result_editor_ready)
      )
    )
  }
  env$editor_expressions <- Filter(
    function(x) {
      if (!is.call(x)) {
        return(FALSE)
      }
      if (identical(x[[1]], quote(`<-`))) {
        return(
          as.character(x[[2]])[[1]] %in%
            assignments ||
            any(vapply(
              outputs,
              function(name) {
                identical(
                  x[[2]],
                  substitute(output$NAME, list(NAME = as.name(name)))
                )
              },
              logical(1)
            ))
        )
      }
      identical(x[[1]], quote(observeEvent)) &&
        any(vapply(events, identical, logical(1), x[[2]]))
    },
    statements
  )
  rows <- env$addDiscData_empty_table()[rep(NA_integer_, 4L), ]
  rows$sample_key <- c("file-a", "file-a", "manual-b", "file-a")
  rows$source_sample_id <- c("A", "A", "B", "A")
  rows$source_code <- c("LAB", "LAB", "YGwater-manual", "LAB")
  rows$source_row_number <- c(10L, 11L, NA_integer_, 12L)
  rows$datetime <- as.POSIXct("2026-11-01 12:00:00", tz = "UTC")
  rows$parameter_id <- c(1L, 1L, NA_integer_, NA_integer_)
  rows$source_parameter_code <- c("P", "P", "", "UNMAPPED")
  rows$source_parameter_name <- c("Analyte", "Analyte", "", "Unmapped")
  rows$source_unit <- "ug/L"
  rows$source_result_text <- c("1200", "<20", "", "5")
  rows$source_result <- c(1200, NA, NA, 5)
  rows$source_result_condition <- c(NA_integer_, 1L, NA_integer_, NA_integer_)
  rows$source_result_condition_value <- c(NA, 20, NA, NA)
  rows$result <- c(1.2, NA, NA, 5)
  rows$result_condition_id <- rows$source_result_condition
  rows$result_condition_value <- c(NA, 0.02, NA, NA)
  rows$result_type_id <- 2L
  rows$matrix_state_id <- 1L
  rows$result_value_type_id <- 1L
  rows$lab_report_no <- "Report-1"
  rows$lab_sample_no <- "Lab-1"
  rows$lab_id <- 1L
  rows$conversion <- 0.001
  rows$result_offset <- 0
  rows$analysis_conditions <- '{"reference_temperature_kelvin":293.15,"custom":"keep"}'
  rows$note <- "Original note"
  rows$mapping_status <- "mapped"
  rows$result_no_source_update <- TRUE
  env$initial_rows <- rows
  server <- function(input, output, session) {
    ns <- session$ns
    data <- reactiveValues(df = initial_rows, preview_base = initial_rows)
    pending_sample_group <- reactiveVal(NULL)
    sample_qualifier_map <- reactiveVal(list())
    sample_observer_map <- reactiveVal(list())
    sample_share_map <- reactiveVal(list())
    locations <- reactiveVal(data.frame(
      location_id = 1L,
      name = "Station A",
      location_code = "A",
      alias = ""
    ))
    sub_locations <- data.frame(
      sub_location_id = 1L,
      location_id = 1L,
      sub_location_name = "Shore"
    )
    sample_groups <- reactiveVal(data.frame(
      sample_group_id = 1L,
      group_type = "trip",
      group_code = "TRIP-1",
      group_name = "Field trip"
    ))
    media <- data.frame(media_id = 1L, media_type = "Freshwater")
    collection_methods <- data.frame(
      collection_method_id = 1L,
      collection_method = "Bottle"
    )
    sample_types <- data.frame(sample_type_id = 1L, sample_type = "Routine")
    organizations <- data.frame(organization_id = 1L, name = "Organization")
    sample_qualifiers <- data.frame(
      qualifier_type_id = 1L,
      qualifier_type_description = "Qualifier"
    )
    observers <- reactiveVal(data.frame(
      observer_id = 1L,
      observer_first = "Field",
      observer_last = "Sampler",
      organization = "Organization"
    ))
    observer_choices <- function(rows) c("Observer" = "1")
    sample_share_choices <- c("All users" = "public_reader")
    params <- reactiveVal(data.frame(
      parameter_id = 1:2,
      param_name = c("Analyte", "Required descriptors"),
      sample_fraction = c(FALSE, TRUE),
      reporting_basis = c(FALSE, TRUE),
      result_speciation = FALSE,
      result_target = FALSE,
      endpoint = FALSE,
      unit_liquid = "mg/L"
    ))
    matrix_states <- data.frame(
      matrix_state_id = 1L,
      matrix_state_code = "liquid",
      matrix_state_name = "Liquid"
    )
    result_types <- data.frame(
      result_type_id = 1:2,
      result_type = c("field", "lab_id")
    )
    field_result_type_id <- 1L
    result_conditions <- data.frame(
      result_condition_id = 1L,
      result_condition = "Below detection limit"
    )
    sample_fractions <- data.frame(
      sample_fraction_id = 1L,
      sample_fraction = "Total"
    )
    reporting_bases <- data.frame(
      reporting_basis_id = 1L,
      reporting_basis = "As element"
    )
    result_value_types <- data.frame(
      result_value_type_id = 1L,
      result_value_type = "Measured"
    )
    chemistry_catalogues <- list(
      result_speciations = data.frame(
        result_speciation_id = integer(),
        result_speciation = character()
      ),
      result_targets = data.frame(
        result_target_id = integer(),
        result_target = character()
      )
    )
    protocols_methods <- reactiveVal(data.frame(
      protocol_method_id = 1L,
      protocol_name = "Method"
    ))
    laboratories <- data.frame(lab_id = 1L, lab_name = "Laboratory")
    grade_types <- data.frame(
      grade_type_id = 1L,
      grade_type_description = "Unspecified"
    )
    approval_types <- data.frame(
      approval_type_id = 1L,
      approval_type_description = "Not reviewed"
    )
    notices <- reactiveVal(character())
    showNotification <- function(ui, ...) notices(as.character(ui))
    for (expr in editor_expressions) {
      eval(expr)
    }
  }
  environment(server) <- env
  list(env = env, server = server, rows = rows)
}

test_that("selecting results prefills the editor and preserves source provenance", {
  fixture <- discrete_result_editor_fixture()
  shiny::testServer(fixture$server, {
    session$setInputs(sample_location_summary_rows_selected = 1L)
    expect_equal(selected_result_rows(), c(1L, 2L, 4L))
    session$setInputs(data_table_rows_selected = 1L)
    html <- output$result_editor$html
    expect_match(html, "Save result changes", fixed = TRUE)
    expect_match(html, 'value="1.2"', fixed = TRUE)
    expect_match(html, "293.15", fixed = TRUE)
    expect_match(output$manual_lab_metadata$html, "Report-1", fixed = TRUE)
    session$setInputs(
      manual_parameter = "1",
      manual_result = 2.5,
      manual_result_type = "2",
      manual_matrix_state = "1",
      manual_result_value_type = "1",
      manual_laboratory = "1",
      manual_lab_report = "Corrected report",
      manual_lab_sample = "Lab-2",
      manual_note = "Corrected note",
      manual_chem_analysis_conditions = '{"custom":"updated"}',
      add_manual_result = 1L
    )
    expect_equal(nrow(data$df), 4L)
    expect_equal(data$df$result, c(2.5, NA, NA, 5))
    expect_identical(
      data$df$source_result_text,
      fixture$rows$source_result_text
    )
    expect_identical(data$df$source_row_number, fixture$rows$source_row_number)
    expect_identical(data$df$conversion, fixture$rows$conversion)
    expect_identical(
      data$df$result_no_source_update,
      fixture$rows$result_no_source_update
    )
    expect_identical(data$df$lab_report_no[[1]], "Corrected report")
    expect_identical(data$df$lab_sample_no[[1]], "Lab-2")
    expect_identical(data$df$mapping_status[[1]], "preview edit")
    expect_identical(data$df$note[[2]], "Original note")
    expect_identical(data$df$analysis_conditions[[1]], '{"custom":"updated"}')
    # Edits to the newly editable metadata survive mapping refreshes.
    merged <- fixture$env$addDiscData_merge_preview_edits(
      fixture$rows,
      data$df,
      fixture$rows
    )
    expect_equal(merged$result[[1]], 2.5)
    expect_identical(merged$lab_report_no[[1]], "Corrected report")
    expect_identical(merged$lab_sample_no[[1]], "Lab-2")
    expect_identical(merged$mapping_status[[1]], "preview edit")
  })
})

test_that("New result adds to the selected sample and can then be edited in place", {
  fixture <- discrete_result_editor_fixture()
  shiny::testServer(fixture$server, {
    session$setInputs(sample_location_summary_rows_selected = 1L)
    session$setInputs(data_table_rows_selected = 2L)
    expect_match(output$result_editor$html, 'value="0.02"', fixed = TRUE)
    session$setInputs(new_result = 1L)
    expect_null(selected_result_row())
    expect_match(
      output$result_editor$html,
      "Create new result",
      fixed = TRUE
    )
    expect_false(grepl('value="0.02"', output$result_editor$html, fixed = TRUE))
    session$setInputs(
      manual_parameter = "1",
      manual_result = 7,
      manual_result_type = "1",
      manual_matrix_state = "1",
      manual_result_value_type = "1",
      manual_lab_report = "stale lab input",
      add_manual_result = 1L
    )
    expect_equal(nrow(data$df), 5L)
    expect_identical(data$df$sample_key[[5]], "file-a")
    expect_identical(data$df$source_code[[5]], "YGwater-manual")
    expect_true(is.na(data$df$lab_id[[5]]))
    expect_true(is.na(data$df$lab_report_no[[5]]))
    expect_true(is.na(data$df$lab_sample_no[[5]]))
    expect_equal(selected_result_index(), 5L)
    session$setInputs(manual_result = 8, add_manual_result = 2L)
    expect_equal(nrow(data$df), 5L)
    expect_equal(data$df$result[[5]], 8)
    expect_equal(
      fixture$env$addDiscData_upload_plan(data$df)$included_results,
      3L
    )
    session$setInputs(sample_location_summary_rows_selected = 2L)
    expect_null(selected_result_row())
    expect_equal(nrow(selected_results()), 0L)
    session$setInputs(
      new_result = 2L,
      manual_result = 9,
      add_manual_result = 3L
    )
    expect_identical(data$df$sample_key[[6]], "manual-b")
    expect_equal(data$df$result[[6]], 9)
    data$df <- fixture$env$addDiscData_empty_table()
    session$flushReact()
    expect_identical(names(sample_location_rows()), names(fixture$rows))
    expect_equal(nrow(sample_location_rows()), 0L)
  })
})

test_that("editing validates values and descriptors without changing other rows", {
  fixture <- discrete_result_editor_fixture()
  shiny::testServer(fixture$server, {
    session$setInputs(sample_location_summary_rows_selected = 1L)
    # The third displayed result is an unmapped file row, not a manual shell.
    session$setInputs(data_table_rows_selected = 3L)
    expect_equal(selected_result_index(), 4L)
    before <- data$df
    session$setInputs(
      manual_parameter = "2",
      manual_result = 3,
      manual_result_type = "2",
      manual_matrix_state = "1",
      manual_result_value_type = "1",
      add_manual_result = 1L
    )
    expect_identical(data$df, before)
    expect_match(notices(), "Sample fraction is required", fixed = TRUE)
    session$setInputs(manual_sample_fraction = "1", add_manual_result = 2L)
    expect_identical(data$df, before)
    expect_match(notices(), "Reporting basis is required", fixed = TRUE)
    session$setInputs(
      manual_descriptor = "1",
      manual_result_condition = "1",
      manual_condition_value = 0.1,
      add_manual_result = 3L
    )
    expect_identical(data$df, before)
    expect_match(notices(), "cannot be combined", fixed = TRUE)
    session$setInputs(manual_result = NA_real_, add_manual_result = 4L)
    expect_equal(data$df$parameter_id[[4]], 2L)
    expect_equal(data$df$result_condition_value[[4]], 0.1)
    expect_true(is.na(data$df$result[[4]]))
    expect_identical(data$df[1:3, ], before[1:3, ])
    # Reparse can replace/reorder rows; it must clear the result selection.
    data$preview_base <- data$df[4:1, ]
    session$flushReact()
    expect_null(selected_result_row())
  })
})

test_that("sample and result tables precede independent editor accordions", {
  fixture <- discrete_result_editor_fixture()
  html <- as.character(fixture$env$addDiscDataUI("entry"))
  doc <- xml2::read_html(html)
  position <- function(id) {
    regexpr(paste0('id="entry-', id, '"'), html, fixed = TRUE)[[1]]
  }
  expect_lt(
    position("sample_location_summary"),
    position("sample_detail_accordion")
  )
  expect_lt(position("data_table"), position("manual_result_accordion"))
  expect_lt(position("sample_location_summary"), position("new_sample"))
  expect_lt(position("new_sample"), position("sample_detail_accordion"))
  expect_lt(position("data_table"), position("new_result"))
  expect_lt(position("new_result"), position("manual_result_accordion"))
  expect_match(html, "Edit sample details", fixed = TRUE)
  expect_match(html, "Edit result details", fixed = TRUE)
  expect_false(grepl("Create or edit", html, fixed = TRUE))
  for (ids in list(
    c("sample_detail_accordion", "new_sample"),
    c("manual_result_accordion", "new_result")
  )) {
    expect_length(
      xml2::xml_find_all(
        doc,
        sprintf(
          '//*[@id="entry-%s"]//*[@id="entry-%s"]',
          ids[[1]],
          ids[[2]]
        )
      ),
      0L
    )
  }
  mappings <- xml2::xml_find_first(doc, '//*[@id="entry-mapping_accordion"]')
  expect_length(
    xml2::xml_find_all(mappings, './/*[@id="entry-preview_file"]'),
    0L
  )
  expect_length(
    xml2::xml_find_all(mappings, './/*[@id="entry-mapping_summary"]'),
    1L
  )
  expect_length(
    xml2::xml_find_all(
      doc,
      '//*[@id="entry-accordion1"]//*[@id="entry-mapping_summary"]'
    ),
    0L
  )
})

test_that("New buttons clear selections and reset repeated unsaved entries", {
  fixture <- discrete_result_editor_fixture()
  shiny::testServer(fixture$server, {
    session$flushReact()
    session$setInputs(sample_location_summary_rows_selected = 1L)
    expect_match(
      output$sample_editor_state$html,
      "Editing sample A",
      fixed = TRUE
    )
    session$setInputs(data_table_rows_selected = 1L)
    expect_equal(selected_result_row()$result, 1.2)
    session$setInputs(new_result = 1L)
    expect_match(
      output$result_editor$html,
      "Creating a new result for sample A",
      fixed = TRUE
    )
    expect_match(output$result_editor$html, "Create new result", fixed = TRUE)
    expect_null(selected_result_row())
    expect_identical(selected_sample_key(), "file-a")
    expect_false(grepl('value="1.2"', output$result_editor$html, fixed = TRUE))
    expect_false(grepl(
      "Original note",
      output$result_editor$html,
      fixed = TRUE
    ))
    expect_false(grepl(
      "Report-1",
      output$manual_lab_metadata$html,
      fixed = TRUE
    ))
    revision <- result_form_revision()
    session$setInputs(manual_note = "Unsaved draft", new_result = 2L)
    expect_gt(result_form_revision(), revision)
    expect_false(grepl(
      "Unsaved draft",
      output$result_editor$html,
      fixed = TRUE
    ))
    session$setInputs(data_table_rows_selected = 2L)
    expect_equal(selected_result_row()$result_condition_value, 0.02)
    session$setInputs(new_sample = 1L)
    expect_match(
      output$sample_editor_state$html,
      "Creating a new sample.",
      fixed = TRUE
    )
    expect_null(selected_sample_key())
    expect_null(selected_result_row())
    expect_equal(nrow(selected_results()), 0L)
    expect_match(
      output$result_editor$html,
      "Create or select a sample",
      fixed = TRUE
    )
    revision <- sample_form_revision()
    session$setInputs(new_sample = 2L)
    expect_gt(sample_form_revision(), revision)
    expect_identical(data$df, fixture$rows)
  })
})

test_that("empty result tables retain headers with review values first", {
  fixture <- discrete_result_editor_fixture()
  args <- rep(
    list(data.frame()),
    length(formals(fixture$env$addDiscData_result_display))
  )
  names(args) <- names(formals(fixture$env$addDiscData_result_display))
  args$rows <- fixture$env$addDiscData_empty_table()
  args$chemistry_catalogues <- list(
    result_speciations = data.frame(),
    result_targets = data.frame()
  )
  display <- do.call(fixture$env$addDiscData_result_display, args)
  expect_equal(nrow(display), 0L)
  expect_identical(
    names(display)[1:5],
    c(
      "Parameter",
      "Result value",
      "Result condition",
      "Condition value",
      "Target unit"
    )
  )
  expect_true(all(
    c("Source result", "Source unit", "Source result flag") %in% names(display)
  ))
  expect_false(any(
    c(
      "Source parameter",
      "Source sample",
      "Source location",
      "AquaCache location",
      "Sub-location",
      "Sample datetime (UTC)",
      "Media",
      "Collection method",
      "Sample type",
      "Analysis datetime",
      "Lab report",
      "Lab sample"
    ) %in%
      names(display)
  ))
})

test_that("simplified result columns still edit the right value and retain provenance", {
  fixture <- discrete_result_editor_fixture(tables = TRUE)
  shiny::testServer(fixture$server, {
    session$setInputs(sample_location_summary_rows_selected = 1L)
    display <- result_display()
    expect_identical(
      display$Parameter,
      c("Analyte", "Analyte", "Unmapped: Unmapped")
    )
    expect_identical(display$`Source result`, c("1200", "<20", "5"))
    expect_identical(display$`Target unit`, c("mg/L", "mg/L", ""))
    # DT sends a zero-based column index after columns are removed or reordered.
    session$setInputs(
      data_table_cell_edit = list(
        row = 1L,
        col = match("Result value", names(display)) - 1L,
        value = "2.5"
      )
    )
    expect_equal(data$df$result, c(2.5, NA, NA, 5))
    expect_identical(
      data$df$source_result_text,
      fixture$rows$source_result_text
    )
    expect_identical(data$df$lab_sample_no, fixture$rows$lab_sample_no)
    expect_identical(data$df$lab_report_no, fixture$rows$lab_report_no)
    expect_identical(data$df$analysis_datetime, fixture$rows$analysis_datetime)
    session$setInputs(
      data_table_cell_edit = list(
        row = 3L,
        col = match("Note", names(display)) - 1L,
        value = "Review this result"
      )
    )
    expect_identical(
      data$df$note,
      c(
        "Original note",
        "Original note",
        "Original note",
        "Review this result"
      )
    )
    # Source comparisons have no cell editor and cannot overwrite source values.
    session$setInputs(
      data_table_cell_edit = list(
        row = 1L,
        col = match("Source result", names(display)) - 1L,
        value = "9999"
      )
    )
    expect_identical(
      data$df$source_result_text,
      fixture$rows$source_result_text
    )
    session$setInputs(
      new_result = 1L,
      manual_parameter = "1",
      manual_result = 7,
      manual_result_type = "1",
      manual_matrix_state = "1",
      manual_result_value_type = "1",
      add_manual_result = 1L
    )
    display <- result_display()
    expect_equal(nrow(display), 4L)
    expect_identical(tail(display$`Source result`, 1L), "")
    expect_equal(tail(as.numeric(display$`Result value`), 1L), 7)
    expect_identical(tail(display$Parameter, 1L), "Analyte")
  })
})

test_that("sample save button identifies create and edit modes", {
  fixture <- discrete_result_editor_fixture()
  find_button <- function(expr) {
    if (!is.call(expr)) {
      return(list())
    }
    if (
      identical(expr[[1]], quote(actionButton)) &&
        identical(expr[[2]], quote(ns("save_sample")))
    ) {
      return(list(expr))
    }
    unlist(lapply(as.list(expr)[-1], find_button), recursive = FALSE)
  }
  button <- find_button(body(fixture$env$addDiscData))[[1]]
  state <- new.env(parent = fixture$env)
  state$ns <- identity
  for (editing in c(FALSE, TRUE)) {
    state$selected <- editing
    html <- as.character(eval(button, state))
    expect_match(
      html,
      if (editing) "Save sample changes" else "Create new sample",
      fixed = TRUE
    )
  }
})

test_that("the complete sample form renders for selected and new samples", {
  fixture <- discrete_result_editor_fixture(sample_form = TRUE)
  shiny::testServer(fixture$server, {
    session$flushReact()
    expect_match(output$sample_editor$html, "Create new sample", fixed = TRUE)
    data$df$sample_note <- "Selected sample note"
    session$setInputs(sample_location_summary_rows_selected = 1L)
    html <- output$sample_editor$html
    expect_match(html, "Save sample changes", fixed = TRUE)
    expect_match(html, "edit_sample_location", fixed = TRUE)
    expect_match(html, "edit_sample_share_with", fixed = TRUE)
    expect_match(html, "To restrict visibility", fixed = TRUE)
    expect_match(html, "Selected sample note", fixed = TRUE)
    session$setInputs(new_sample = 1L)
    html <- output$sample_editor$html
    expect_match(html, "Create new sample", fixed = TRUE)
    expect_match(html, "edit_sample_share_with", fixed = TRUE)
    expect_false(grepl("Selected sample note", html, fixed = TRUE))
  })
})

test_that("the sample location follows the selected row through catalogue refreshes", {
  fixture <- discrete_result_editor_fixture(sample_form = TRUE)
  shiny::testServer(fixture$server, {
    data$df$location_id <- c(1L, 1L, NA_integer_, 1L)
    session$setInputs(sample_location_summary_rows_selected = 1L)
    expect_match(
      output$sample_editor$html, '<option value="1" selected>Station A | Code: A',
      fixed = TRUE
    )

    session$setInputs(sample_location_summary_rows_selected = 2L)
    expect_match(
      output$sample_editor$html, '<option value="" selected>Select a location',
      fixed = TRUE
    )
    locations(rbind(locations(), data.frame(
      location_id = 2L, name = "Station B", location_code = "B", alias = ""
    )))
    data$df$location_id[3L] <- 2L
    session$flushReact()
    expect_match(
      output$sample_editor$html, '<option value="2" selected>Station B | Code: B',
      fixed = TRUE
    )

    session$setInputs(sample_location_summary_rows_selected = 1L)
    expect_match(
      output$sample_editor$html, '<option value="1" selected>Station A | Code: A',
      fixed = TRUE
    )
  })
})
