skip_on_cran()

skip_if(
  !nzchar(Sys.getenv("aquacacheTestHost")),
  "Aquacache connection details are not configured"
)
skip_if(
  !nzchar(Sys.getenv("aquacacheTestName")),
  "Aquacache connection details are not configured"
)
skip_if(
  !nzchar(Sys.getenv("aquacacheTestUser")),
  "Aquacache connection details are not configured"
)
skip_if(
  !nzchar(Sys.getenv("aquacacheTestPass")),
  "Aquacache connection details are not configured"
)

skip_if_not_installed("rpostgis")
skip_if_not_installed("jsonlite")
skip_if_not_installed("terra")

con <- test_AquaConnect(silent = TRUE)
on.exit(DBI::dbDisconnect(con), add = TRUE)


test_that("getVector retrieves a single feature by name", {
  # Query table 'spatial.vectors' to find a valid feature name for layer 'Locations'
  name <- DBI::dbGetQuery(
    con,
    "SELECT feature_name, layer_name FROM spatial.vectors LIMIT 1;"
  )
  result <- getVector(
    layer_name = name$layer_name,
    feature_name = name$feature_name,
    con = con,
    silent = TRUE
  )

  expect_s4_class(result, "SpatVector")
  expect_equal(terra::nrow(result), 1)

  attrs <- terra::values(result)
  expect_true(
    all(
      c(
        "geom_id",
        "geom_type",
        "layer_name",
        "feature_name",
        "description"
      ) %in%
        names(attrs)
    )
  )
  expect_gt(nchar(attrs$feature_name), 2)
  expect_gte(nchar(attrs$description), 2)
})


test_that("getVector retrieves all features in a layer", {
  layers <- DBI::dbGetQuery(
    con,
    "SELECT DISTINCT layer_name FROM spatial.vectors LIMIT 1;"
  )
  result <- getVector(
    layer_name = layers$layer_name[1],
    con = con,
    silent = TRUE
  )

  # Find out how many features are expected via a database query
  exp <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT layer_name, feature_name, geom_id AS n FROM spatial.vectors WHERE layer_name = '",
      layers$layer_name[1],
      "';"
    )
  )

  expect_equal(terra::nrow(result), nrow(exp))

  attrs <- terra::values(result)
  expect_setequal(
    attrs$feature_name,
    exp$feature_name
  )
})


test_that("getVector errors when no results are found", {
  expect_error(
    getVector(
      layer_name = "This layer does not exist",
      con = con,
      silent = TRUE
    ),
    "returned no results"
  )
})


test_that("getVector validates requested geometry type", {
  expect_error(
    getVector(geom_type = "invalid"),
    "Parameter geom_type is not one of the possible choices"
  )
})
