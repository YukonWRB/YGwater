test_that("location and WWR maps defer expensive marker work", {
  locations_module <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/client/map/locationsMap.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  registry_module <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/client/WWR/registry_front_end.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_false(grepl("leaflet::addTiles()", locations_module, fixed = TRUE))
  expect_false(grepl("leaflet::addTiles()", registry_module, fixed = TRUE))
  expect_false(grepl("popup = ~popup_html", locations_module, fixed = TRUE))
  expect_false(grepl("popup = ~popup_html", registry_module, fixed = TRUE))
  expect_match(
    locations_module,
    "popupData <- function(location_id)",
    fixed = TRUE
  )
  expect_match(
    registry_module,
    "popupData <- function(registry_id)",
    fixed = TRUE
  )
  expect_match(locations_module, "chunkedLoading = TRUE", fixed = TRUE)
  expect_match(registry_module, "chunkedLoading = TRUE", fixed = TRUE)
  expect_match(locations_module, "type_map[,", fixed = TRUE)
  expect_match(registry_module, "symbol_map[,", fixed = TRUE)
})

test_that("continuous cache predicates apply to each timeseries", {
  cache_module <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/cache_functions.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    cache_module,
    '"ts.start_datetime IS NOT NULL AND ts.end_datetime IS NOT NULL"',
    fixed = TRUE
  )
  expect_false(grepl(
    "EXISTS (\n       SELECT 1\n         FROM continuous.timeseries",
    cache_module,
    fixed = TRUE
  ))
  expect_match(
    cache_module,
    "(end_datetime AT TIME ZONE 'UTC')::date::text",
    fixed = TRUE
  )
})
