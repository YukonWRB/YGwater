skip_on_cran()

skip_if(
  !nzchar(Sys.getenv("aquacacheHost")),
  "Aquacache connection details are not configured"
)
skip_if(
  !nzchar(Sys.getenv("aquacacheName")),
  "Aquacache connection details are not configured"
)
skip_if(
  !nzchar(Sys.getenv("aquacacheUser")),
  "Aquacache connection details are not configured"
)
skip_if(
  !nzchar(Sys.getenv("aquacachePass")),
  "Aquacache connection details are not configured"
)

skip_if_not_installed("rpostgis")
skip_if_not_installed("jsonlite")
skip_if_not_installed("terra")

con <- AquaConnect(silent = TRUE)
on.exit(DBI::dbDisconnect(con), add = TRUE)


test_that("getVector retrieves a single feature by name", {
  result <- getVector(
    layer_name = "Locations",
    feature_name = "09AA-SC03",
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
  expect_equal(attrs$layer_name, "Locations")
  expect_equal(attrs$feature_name, "09AA-SC03")
  expect_equal(attrs$geom_type, "ST_Point")
  expect_equal(attrs$description, "Log Cabin Snow Course")
})


test_that("getVector retrieves all features in a layer", {
  result <- getVector(
    layer_name = "Locations",
    con = con,
    silent = TRUE
  )

  # Find out how many features are expected via a database query
  exp <- DBI::dbGetQuery(
    con,
    "SELECT layer_name, feature_name, geom_id AS n FROM spatial.vectors WHERE layer_name = 'Locations';"
  )

  expect_equal(terra::nrow(result), nrow(exp))

  attrs <- terra::values(result)
  expect_true(all(attrs$geom_type == "ST_Point"))
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
