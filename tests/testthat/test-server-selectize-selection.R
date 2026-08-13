read_app_module <- function(path) {
  paste(
    readLines(
      system.file("apps", "YGwater", "modules", path, package = "YGwater"),
      warn = FALSE
    ),
    collapse = "\n"
  )
}

test_that("instrument and sensor tables refresh server-side record selectors", {
  instruments <- read_app_module("admin/instruments/manageInstruments.R")
  sensors <- read_app_module("admin/instruments/manageSensors.R")

  expect_match(
    instruments,
    "update_record_selector <- function(selected = isolate(input$record_id))",
    fixed = TRUE
  )
  expect_match(
    instruments,
    "choices = build_record_choices(instrument_data$records)",
    fixed = TRUE
  )
  expect_match(
    instruments,
    "update_record_selector(\n          as.character(instrument_data$records$instrument_id[[selected_row]])",
    fixed = TRUE
  )
  expect_match(
    instruments,
    "update_lookup_selectors(list(",
    fixed = TRUE
  )

  expect_match(
    sensors,
    "update_record_selector <- function(selected = isolate(input$record_id))",
    fixed = TRUE
  )
  expect_match(
    sensors,
    "choices = build_record_choices(sensor_data$records)",
    fixed = TRUE
  )
  expect_match(
    sensors,
    "update_record_selector(\n          as.character(sensor_data$records$sensor_id[[selected_row]])",
    fixed = TRUE
  )
  expect_match(sensors, "update_lookup_selectors(list(", fixed = TRUE)
})

test_that("maintenance tables refresh server-side selectors", {
  module <- read_app_module("admin/instruments/instrumentMaintenance.R")

  expect_match(
    module,
    "choices = build_instrument_choices(maintenance_data$instruments)",
    fixed = TRUE
  )
  expect_match(
    module,
    "choices = build_observer_choices(maintenance_data$observers)",
    fixed = TRUE
  )
  expect_match(
    module,
    "build_sensor_choices(maintenance_data$sensors)",
    fixed = TRUE
  )
  expect_match(
    module,
    "update_instrument_selector(as.character(instrument_id))",
    fixed = TRUE
  )
  expect_equal(
    lengths(regmatches(
      module,
      gregexpr(
        "update_observer_selector(as.character(record$observer[[1]]))",
        module,
        fixed = TRUE
      )
    )),
    2L
  )
  expect_match(
    module,
    "update_sensor_selector(record$sensor_id[[1]])",
    fixed = TRUE
  )
})

test_that("flood table and map refresh the server-side station selector", {
  module <- read_app_module("client/reports/floodDashboard.R")

  expect_match(
    module,
    "update_station_selector <- function(",
    fixed = TRUE
  )
  expect_match(
    module,
    "choices = build_station_choices(dat)",
    fixed = TRUE
  )
  expect_match(
    module,
    "update_station_selector(\n                    selected = selected_station,\n                    dat = community_locations()",
    fixed = TRUE
  )
  expect_match(
    module,
    "update_station_selector(selected = selected_code)",
    fixed = TRUE
  )
})
