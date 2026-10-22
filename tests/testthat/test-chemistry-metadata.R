test_that("SQL input references include forms, endpoints, and numeric context", {
  rows <- data.frame(parameter_id = 1L, sample_fraction_id = 2L, reporting_basis_id = 3L,
    result_speciation_id = 4L, result_target_id = 5L, endpoint_mode = "fixed", endpoint_ph = 4.5,
    analysis_conditions = '{"mass_basis":"dry","reference_pressure_kpa":101.325}')
  sql <- "SELECT discrete.get_sample_val($1::INT, 1, 2, 3, 4, 5, 'fixed', 4.5,
    '{\"reference_pressure_kpa\":101.325,\"mass_basis\":\"dry\"}'::JSONB)"
  expect_identical(unname(chemistry_sql_accessor_refs(sql)), chemistry_accessor_keys(rows))
  rows$endpoint_ph <- 8.3
  expect_false(identical(unname(chemistry_sql_accessor_refs(sql)), chemistry_accessor_keys(rows)))
  expect_length(chemistry_sql_accessor_refs("SELECT 'discrete.get_sample_val(1,2)' -- discrete.get_sample_val(1,2)"), 0L)
  expect_error(chemistry_sql_accessor_refs("SELECT discrete.get_sample_val($1, 1, NULL, NULL, NULL, NULL, 'fixed', nope)"), "endpoint_ph")
})

test_that("common conditions are collected through scientific controls", {
  input <- list(test_endpoint_mode = "fixed", test_endpoint_ph = 4.5,
    test_mass_basis = "dry", test_gas_basis = "", test_reference_temperature_kelvin = 293.15,
    test_reference_pressure_kpa = NA_real_, test_analysis_conditions = '{"isotope_reference_standard":"VSMOW"}')
  result <- chemistry_metadata_values(input, "test_")
  conditions <- jsonlite::fromJSON(result$analysis_conditions)
  expect_identical(conditions$mass_basis, "dry")
  expect_identical(conditions$isotope_reference_standard, "VSMOW")
  expect_equal(conditions$reference_temperature_kelvin, 293.15)
  expect_null(conditions$gas_basis)
  expect_null(conditions$reference_pressure_kpa)
  input$test_reference_temperature_kelvin <- -1
  expect_error(chemistry_metadata_values(input, "test_"), "positive")
})
