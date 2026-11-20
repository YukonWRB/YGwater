qc_rule_helper_path <- system.file(
  "apps/aqualink/modules/admin/sourceAdapterArguments.R",
  package = "YGwater"
)
local_qc_rule_helper_path <- file.path(
  "inst",
  "apps",
  "aqualink",
  "modules",
  "admin",
  "sourceAdapterArguments.R"
)
if (file.exists(local_qc_rule_helper_path)) {
  qc_rule_helper_path <- local_qc_rule_helper_path
}

test_that("QC rule controls follow the schema and collect conditional values", {
  source(qc_rule_helper_path, local = TRUE)
  schema <- list(
    schema_version = 2L,
    arguments = list(
      list(
        name = "zero_split",
        source = "user",
        label = "Split stuck zeros",
        help = "Use air temperature to classify stuck zero readings.",
        value_type = "logical",
        control = "checkbox",
        required = TRUE,
        default = FALSE
      ),
      list(
        name = "zero_max_air_temp",
        source = "user",
        label = "Cold threshold",
        help = "Values below the threshold are classified as frozen.",
        value_type = "numeric",
        control = "numeric",
        required = TRUE,
        default = 0,
        step = 0.1,
        units = "\u00b0C",
        visible_when = list(argument = "zero_split", equals = TRUE)
      ),
      list(
        name = "zero_qualifier",
        source = "internal",
        help = "Qualifier used for cold zero readings.",
        default = "SUS"
      )
    ),
    companions = list(list(
      role = "air_temp",
      label = "Air temperature",
      parameter_name = "temperature, air",
      selection_strategy = "same_location_lowest_sensor_priority",
      allow_explicit_override = TRUE,
      missing_behavior = "warn_and_use_na",
      help = "Used only when zero-splitting is enabled.",
      when = list(argument = "zero_split", equals = TRUE)
    ))
  )
  defaults <- qc_rule_schema_defaults(schema)
  input <- list(
    qc_1_source_arg_zero_split = TRUE,
    qc_1_source_arg_zero_max_air_temp = -4.5,
    qc_1_active = TRUE,
    qc_1_run_order = 2,
    qc_1_note = "Cold-season classification",
    qc_1_qc_companion_air_temp = "42"
  )

  ui <- qc_rule_argument_controls_ui(
    shiny::NS("qc"),
    schema,
    defaults,
    input_prefix = "qc_1_"
  )
  html <- htmltools::renderTags(ui)$html
  expect_match(html, paste0("Cold threshold (", "\u00b0C", ")"), fixed = TRUE)
  expect_match(html, "Managed rule arguments (read-only)", fixed = TRUE)
  expect_match(html, "zero_split", fixed = TRUE)

  companion_ui <- qc_rule_companion_control_ui(
    shiny::NS("qc"),
    schema$companions[[1L]],
    data.frame(
      timeseries_id = 42L,
      sensor_priority = 1L,
      record_rate = "1 hour",
      stringsAsFactors = FALSE
    ),
    list(),
    input_prefix = "qc_1_"
  )
  companion_html <- htmltools::renderTags(companion_ui)$html
  expect_match(companion_html, "Automatic (lowest sensor priority)", fixed = TRUE)
  expect_match(companion_html, "Timeseries 42", fixed = TRUE)
  expect_match(companion_html, "data-display-if", fixed = TRUE)
  expect_match(companion_html, "source_arg_zero_split", fixed = TRUE)

  arguments <- qc_rule_collect_arguments(
    input,
    schema,
    defaults,
    input_prefix = "qc_1_"
  )
  expect_identical(arguments$zero_split, TRUE)
  expect_equal(arguments$zero_max_air_temp, -4.5)

  collected <- qc_rule_collect_assignments(input, list(list(
    timeseries_qc_rule_id = NA_integer_,
    rule_fx = "qc_stuck_sensor",
    rule_version = 1L,
    argument_schema = schema,
    rule_args = defaults,
    companion_timeseries = list(),
    run_order = 1L,
    active = TRUE,
    note = NA_character_
  )))
  expect_equal(collected$run_order, 2)
  expect_identical(collected$rule_args[[1]]$zero_split, TRUE)
  expect_identical(collected$companion_timeseries[[1]], list(air_temp = 42L))

  input$qc_1_source_arg_zero_split <- FALSE
  collected <- qc_rule_collect_assignments(input, list(list(
    timeseries_qc_rule_id = NA_integer_,
    rule_fx = "qc_stuck_sensor",
    rule_version = 1L,
    argument_schema = schema,
    rule_args = defaults,
    companion_timeseries = list(air_temp = 42L),
    run_order = 1L,
    active = TRUE,
    note = NA_character_
  )))
  expect_length(collected$companion_timeseries[[1]], 0L)

  unchanged <- qc_rule_collect_assignments(list(), list(list(
    timeseries_qc_rule_id = 7L,
    rule_fx = "qc_stuck_sensor",
    rule_version = 1L,
    argument_schema = schema,
    rule_args = defaults,
    companion_timeseries = list(air_temp = 42L),
    run_order = 3L,
    active = TRUE,
    note = NA_character_
  )))
  expect_identical(unchanged$timeseries_qc_rule_id, 7L)
  expect_identical(unchanged$run_order, 3)
  expect_identical(unchanged$active, TRUE)
})

test_that("QC rule choices match target parameters and omit configured rules", {
  source(qc_rule_helper_path, local = TRUE)
  capabilities <- data.frame(
    rule_fx = c("qc_rain_cold", "qc_dir_calm", "qc_stuck_sensor"),
    rule_version = 1L,
    description = c("Cold rain", "Calm wind", "Stuck sensor"),
    enabled = TRUE,
    stringsAsFactors = FALSE
  )
  capabilities$primary_parameters <- I(list(
    "precipitation, rain",
    "wind direction (direction from, expressed 0-360 deg)",
    character()
  ))

  rain_choices <- qc_rule_capability_choices(
    capabilities,
    primary_parameter = "precipitation, rain"
  )
  expect_setequal(
    unname(rain_choices),
    c("qc_rain_cold@1", "qc_stuck_sensor@1")
  )
  wind_choices <- qc_rule_capability_choices(
    capabilities,
    primary_parameter = "wind direction (direction from, expressed 0-360 deg)",
    exclude_rule_fx = "qc_dir_calm"
  )
  expect_identical(unname(wind_choices), "qc_stuck_sensor@1")
  legacy_choices <- qc_rule_capability_choices(
    capabilities[, setdiff(names(capabilities), "primary_parameters")],
    primary_parameter = "precipitation, rain"
  )
  expect_length(legacy_choices, 3L)
})
