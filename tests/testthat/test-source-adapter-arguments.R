source_adapter_helper_path <- system.file(
  "apps/YGwater/modules/admin/sourceAdapterArguments.R",
  package = "YGwater"
)

test_that("source adapter capability lookup selects the requested row", {
  source(source_adapter_helper_path, local = TRUE)
  capabilities <- data.table::data.table(
    source_fx = c("downloadFirst", "downloadSecond"),
    argument_schema = list(
      list(
        schema_version = 1L,
        arguments = list(list(name = "first_argument"))
      ),
      list(
        schema_version = 1L,
        arguments = list(list(name = "second_argument"))
      )
    )
  )

  selected <- source_adapter_capability_row(
    capabilities,
    "downloadSecond"
  )

  expect_equal(nrow(selected), 1L)
  expect_identical(selected$source_fx, "downloadSecond")
  expect_identical(
    selected$argument_schema[[1]]$arguments[[1]]$name,
    "second_argument"
  )
})

test_that("source adapter editor shows user and managed arguments", {
  source(source_adapter_helper_path, local = TRUE)
  schema <- list(
    schema_version = 1L,
    arguments = list(
      list(
        name = "location",
        source = "user",
        label = "Station ID",
        help = "Provider station identifier.",
        value_type = "character",
        control = "text",
        required = TRUE
      ),
      list(
        name = "start_datetime",
        source = "runtime",
        help = "AquaCache passes the next datetime to retrieve."
      ),
      list(
        name = "password",
        source = "environment",
        help = "The function default reads the provider password."
      )
    )
  )
  capability <- data.frame(source_fx = "downloadExample")
  capability$argument_schema <- I(list(schema))

  ui <- source_adapter_argument_ui(
    shiny::NS("adapter"),
    capability,
    '{"location":"station-1"}'
  )
  html <- htmltools::renderTags(ui)$html

  expect_match(html, "Station ID", fixed = TRUE)
  expect_match(html, "Managed arguments (read-only)", fixed = TRUE)
  expect_match(html, "start_datetime", fixed = TRUE)
  expect_match(html, "AquaCache/runtime", fixed = TRUE)
  expect_match(html, "Function default/environment", fixed = TRUE)
  expect_match(html, "How it is supplied", fixed = TRUE)
  expect_match(html, "<details class=\"mt-3\" open", fixed = TRUE)
})

test_that("source adapter editor collects typed values and preserves unknown keys", {
  source(source_adapter_helper_path, local = TRUE)
  schema <- list(
    schema_version = 1L,
    arguments = list(
      list(
        name = "location",
        source = "user",
        label = "Station ID",
        help = "Provider station identifier.",
        value_type = "character",
        control = "text",
        required = TRUE
      ),
      list(
        name = "difference",
        source = "user",
        label = "Calculate differences",
        help = "Convert cumulative observations to increments.",
        value_type = "logical",
        control = "checkbox",
        required = FALSE
      )
    )
  )
  capability <- data.frame(source_fx = "downloadExample")
  capability$argument_schema <- I(list(schema))
  input <- list(
    source_arg_location = "station-1",
    source_arg_difference = TRUE,
    source_args_uncatalogued = '{"legacy_setting":42}'
  )

  args <- source_adapter_collect_args(
    input,
    capability,
    '{"location":"old","difference":"FALSE","legacy_setting":42}'
  )

  expect_identical(args$location, "station-1")
  expect_true(args$difference)
  expect_identical(args$legacy_setting, 42L)
  expect_equal(
    jsonlite::fromJSON(source_adapter_args_json(args)),
    list(
      legacy_setting = 42L,
      location = "station-1",
      difference = TRUE
    )
  )
})

test_that("source adapter tests use the requested window without writing", {
  source(source_adapter_helper_path, local = TRUE)
  received <- NULL
  test_function <- function(
    location,
    start_datetime,
    end_datetime,
    con,
    write = TRUE
  ) {
    received <<- list(
      location = location,
      start_datetime = start_datetime,
      end_datetime = end_datetime,
      con = con,
      write = write
    )
    data.frame(
      datetime = start_datetime,
      value = 12.5
    )
  }

  result <- source_adapter_test(
    source_fx = "downloadExample",
    source_fx_args = '{"location":"station-1","write":true}',
    start_datetime = "2026-08-23 12:00:00",
    end_datetime = "2026-08-24 12:00:00",
    con = "read-only-connection",
    source_function = test_function
  )

  expect_identical(received$location, "station-1")
  expect_identical(received$con, "read-only-connection")
  expect_false(received$write)
  expect_equal(
    as.numeric(difftime(
      received$end_datetime,
      received$start_datetime,
      units = "hours"
    )),
    24
  )
  expect_equal(result$value, 12.5)
  expect_error(
    source_adapter_test(
      source_fx = "downloadExample",
      source_fx_args = '{"location":"station-1"}',
      start_datetime = "2026-08-24 12:00:00",
      end_datetime = "2026-08-23 12:00:00",
      con = NULL,
      source_function = test_function
    ),
    "must precede"
  )
})

test_that("source adapter test previews are bounded and include list sections", {
  source(source_adapter_helper_path, local = TRUE)
  preview <- source_adapter_test_result_text(
    list(
      summary = data.frame(status = "success"),
      data = data.frame(datetime = seq_len(4), value = seq_len(4))
    ),
    max_rows = 2
  )

  expect_match(preview, "[summary]", fixed = TRUE)
  expect_match(preview, "[data]", fixed = TRUE)
  expect_match(preview, "4 row(s) x 2 column(s)", fixed = TRUE)
  expect_match(preview, "2 additional row(s) omitted", fixed = TRUE)
})

test_that("timeseries modules use the catalogued argument editor", {
  module_paths <- c(
    system.file(
      "apps/YGwater/modules/admin/continuousData/addTimeseries.R",
      package = "YGwater"
    ),
    system.file(
      "apps/YGwater/modules/admin/discreteData/addSampleSeries.R",
      package = "YGwater"
    ),
    system.file(
      "apps/YGwater/modules/admin/imgupload/addImgSeries.R",
      package = "YGwater"
    )
  )

  for (module_path in module_paths) {
    module_text <- paste(readLines(module_path, warn = FALSE), collapse = "\n")
    expect_match(module_text, 'uiOutput(ns("source_fx_args_ui"))', fixed = TRUE)
    expect_match(module_text, "source_adapter_collect_args", fixed = TRUE)
    expect_false(grepl('ns("source_fx_args")', module_text, fixed = TRUE))
    expect_false(grepl("format_source_args", module_text, fixed = TRUE))
  }

  timeseries_text <- paste(
    readLines(module_paths[[1L]], warn = FALSE),
    collapse = "\n"
  )
  expect_match(
    timeseries_text,
    'ns("test_primary_source")',
    fixed = TRUE
  )
  expect_match(
    timeseries_text,
    'ns("test_secondary_source")',
    fixed = TRUE
  )
  expect_match(
    timeseries_text,
    "bslib::input_task_button(",
    fixed = TRUE
  )
  expect_match(timeseries_text, 'ns("run_source_test")', fixed = TRUE)
  expect_match(
    timeseries_text,
    "source_test_task <- ExtendedTask$new(function(request)",
    fixed = TRUE
  )
  expect_match(
    timeseries_text,
    'bslib::bind_task_button("run_source_test")',
    fixed = TRUE
  )
  expect_match(
    timeseries_text,
    "bindEvent(input$run_source_test, ignoreInit = TRUE)",
    fixed = TRUE
  )
  expect_match(timeseries_text, "source_adapter_test(", fixed = TRUE)
  expect_match(
    timeseries_text,
    "SET default_transaction_read_only = on",
    fixed = TRUE
  )
})
