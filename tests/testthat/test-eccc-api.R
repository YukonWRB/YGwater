test_that("observation station identifiers are classified", {
  keys <- YGwater:::eccc_observation_station_keys(c("2101310", "48168"))
  expect_equal(keys$field, c("CLIMATE_IDENTIFIER", "STN_ID"))
  expect_equal(keys$value, c("2101310", "48168"))
})

test_that("ECCC date ranges are validated", {
  expect_equal(YGwater:::eccc_date_range("2025-01-01", "2025-01-02")$end, "2025-01-02")
  expect_error(YGwater:::eccc_date_range("2025-01-02", "2025-01-01"), "on or before")
})

test_that("GeoJSON features become a data frame", {
  features <- list(list(
    properties = list(CLIMATE_IDENTIFIER = "2101310", TEMP = -5.2),
    geometry = list(type = "Point", coordinates = list(-135.1, 60.7))
  ))
  result <- YGwater:::eccc_features_data_frame(features)
  expect_equal(result$climate_identifier, "2101310")
  expect_equal(result$temp, -5.2)
  expect_equal(result$geometry[[1]]$type, "Point")
})

test_that("PROGNOS station candidates match MSC identifiers", {
  stations <- data.frame(
    MSC_ID = "WHITEHORSE",
    IATA_ID = "CYXY",
    WMO_ID = "71964",
    Data_Provider = "MSC",
    Longitude = -135.1,
    Latitude = 60.7
  )
  normalized <- YGwater:::eccc_prognos_stations(stations)
  values <- data.frame(
    station_network = "ca", ade_id = "YXY", variable = "AirTemp",
    forecast_value = 270, stringsAsFactors = FALSE
  )
  values$geometry <- list(list(type = "Point", coordinates = list(-135.1, 60.7)))
  result <- YGwater:::eccc_match_prognos(values, normalized, 10)
  expect_equal(result$location_id, "WHITEHORSE")
})

test_that("distance uses great-circle kilometres", {
  expect_equal(YGwater:::eccc_distance_km(-135, 60, -135, 60), 0)
  expect_equal(YGwater:::eccc_distance_km(0, 0, 1, 0), 111.2, tolerance = 0.2)
})
