aqualink_chemistry_form <- new.env(parent = asNamespace("YGwater"))
sys.source(
  system.file(
    "apps/aqualink/modules/admin/discreteData/chemistry_form.R",
    package = "YGwater"
  ),
  aqualink_chemistry_form
)

test_that("SQL input references include forms, endpoints, and numeric context", {
  rows <- data.frame(
    parameter_id = 1L,
    sample_fraction_id = 2L,
    reporting_basis_id = 3L,
    result_speciation_id = 4L,
    result_target_id = 5L,
    endpoint_mode = "fixed",
    endpoint_ph = 4.5,
    analysis_conditions = '{"mass_basis":"dry","reference_pressure_kpa":101.325}'
  )
  sql <- "SELECT discrete.get_sample_val($1::INT, 1, 2, 3, 4, 5, 'fixed', 4.5,
    '{\"reference_pressure_kpa\":101.325,\"mass_basis\":\"dry\"}'::JSONB)"
  expect_identical(
    unname(chemistry_sql_accessor_refs(sql)),
    chemistry_accessor_keys(rows)
  )
  rows$endpoint_ph <- 8.3
  expect_false(identical(
    unname(chemistry_sql_accessor_refs(sql)),
    chemistry_accessor_keys(rows)
  ))
  expect_length(
    chemistry_sql_accessor_refs(
      "SELECT 'discrete.get_sample_val(1,2)' -- discrete.get_sample_val(1,2)"
    ),
    0L
  )
  expect_error(
    chemistry_sql_accessor_refs(
      "SELECT discrete.get_sample_val($1, 1, NULL, NULL, NULL, NULL, 'fixed', nope)"
    ),
    "endpoint_ph"
  )
})

test_that("common conditions are collected through scientific controls", {
  input <- list(
    test_endpoint_mode = "fixed",
    test_endpoint_ph = 4.5,
    test_mass_basis = "dry",
    test_gas_basis = "",
    test_reference_temperature_kelvin = 293.15,
    test_reference_pressure_kpa = NA_real_,
    test_analysis_conditions = '{"isotope_reference_standard":"VSMOW"}'
  )
  result <- aqualink_chemistry_form$chemistry_metadata_values(input, "test_")
  conditions <- jsonlite::fromJSON(result$analysis_conditions)
  expect_identical(conditions$mass_basis, "dry")
  expect_identical(conditions$isotope_reference_standard, "VSMOW")
  expect_equal(conditions$reference_temperature_kelvin, 293.15)
  expect_null(conditions$gas_basis)
  expect_null(conditions$reference_pressure_kpa)
  input$test_reference_temperature_kelvin <- -1
  expect_error(
    aqualink_chemistry_form$chemistry_metadata_values(input, "test_"),
    "positive"
  )
})

test_that("an explicitly unknown endpoint is available and retained", {
  ui <- htmltools::renderTags(aqualink_chemistry_form$chemistry_metadata_ui(
    shiny::NS("test"),
    "test_",
    value = list(endpoint_mode = "unknown")
  ))$html
  expect_match(ui, 'value="unknown"')
  expect_match(ui, "Unknown / not reported")

  values <- aqualink_chemistry_form$chemistry_metadata_values(
    list(
      test_endpoint_mode = "unknown",
      test_endpoint_ph = NA_real_,
      test_analysis_conditions = "{}"
    ),
    "test_"
  )
  expect_identical(values$endpoint_mode, "unknown")
  expect_true(is.na(values$endpoint_ph))
})

test_that("mapping labels and suggestion ranks follow the selected target", {
  messages <- list()
  session <- shiny::MockShinySession$new()
  session$sendInputMessage <- function(inputId, message) {
    messages[[inputId]] <<- message
  }
  session$setInputs(
    mapping_reporting_basis = "1",
    mapping_chem_result_speciation_id = "4"
  )
  fields <- c("reporting_basis", "result_speciation", "result_target")
  catalogues <- stats::setNames(
    lapply(fields, function(field) {
      rows <- data.frame(
        id = 1:4,
        label = c("Alpha", "Beta", "Gamma", "Retired"),
        active = c(TRUE, TRUE, TRUE, FALSE)
      )
      names(rows)[1:2] <- c(paste0(field, "_id"), field)
      rows
    }),
    c("reporting_bases", "result_speciations", "result_targets")
  )
  suggestions <- data.frame(
    parameter_id = 2L,
    field = rep(fields, each = 2L),
    choice_id = c(2L, 3L),
    display_order = c(2L, 1L)
  )
  parameters <- data.frame(
    parameter_id = 1:2,
    sample_fraction = c(FALSE, TRUE),
    reporting_basis = c(FALSE, TRUE),
    result_speciation = FALSE,
    result_target = c(FALSE, TRUE),
    endpoint = FALSE,
    analysis_conditions_required_keys = c("[]", '["reference_pressure_kpa"]'),
    analysis_conditions_required_keys_by_matrix = c(
      "{}",
      '{"solid":["mass_basis","extraction"]}'
    )
  )
  update <- function(parameter, matrix = "liquid") {
    shiny::isolate(aqualink_chemistry_form$update_mapping_chemistry(
      session,
      "mapping_",
      parameters[parameter, ],
      matrix,
      catalogues,
      catalogues$reporting_bases,
      suggestions
    ))
  }
  option_values <- function(message) {
    matches <- gregexpr('(?<=value=")[^"]*', message$options, perl = TRUE)
    unlist(regmatches(message$options, matches))
  }
  label <- function(id) {
    as.character(messages[[paste0("mapping_", id)]]$label$html)
  }

  update(NA_integer_)
  expect_identical(label("sample_fraction"), "Sample fraction")
  update(1L)
  expect_identical(label("sample_fraction"), "Sample fraction (optional)")
  expect_identical(
    option_values(messages$mapping_chem_result_target_id),
    c("1", "2", "3")
  )

  update(2L, "solid")
  expect_identical(label("sample_fraction"), "Sample fraction (mandatory)")
  expect_identical(label("reporting_basis"), "Reporting basis (mandatory)")
  expect_identical(
    option_values(messages$mapping_reporting_basis),
    c("", "3", "2", "1", "4")
  )
  expect_identical(
    option_values(messages$mapping_chem_result_speciation_id),
    c("3", "2", "1", "4")
  )
  expect_identical(
    option_values(messages$mapping_chem_result_target_id),
    c("3", "2", "1")
  )
  expect_identical(messages$mapping_reporting_basis$value, "1")
  expect_identical(messages$mapping_chem_result_speciation_id$value, "4")
  expect_identical(label("chem_mass_basis"), "Solid mass basis (mandatory)")
  expect_identical(
    label("chem_reference_pressure_kpa"),
    "Reference pressure (kPa) (mandatory)"
  )
  expect_identical(
    label("chem_analysis_conditions"),
    "Named conditions (mandatory) [extraction]"
  )

  update(2L)
  expect_identical(label("chem_mass_basis"), "Solid mass basis (optional)")
})
