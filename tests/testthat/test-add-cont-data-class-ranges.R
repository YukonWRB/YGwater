add_cont_data_module_environment <- function() {
  env <- new.env(parent = asNamespace("YGwater"))
  sys.source(
    system.file(
      "apps/YGwater/modules/admin/continuousData/addContData.R",
      package = "YGwater"
    ),
    envir = env
  )
  env
}

test_that("blank class rows split repeated codes into separate ranges", {
  module <- add_cont_data_module_environment()
  datetime <- as.POSIXct(
    "2025-01-01 00:00:00",
    tz = "UTC"
  ) + seq.int(0, 5) * 3600
  code <- c("SUS", "", "SUS", "SUS", NA, "SUS")

  ranges <- module$add_cont_data_class_runs(datetime, code)

  expect_equal(nrow(ranges), 3)
  expect_identical(ranges$code, rep("SUS", 3))
  expect_identical(
    ranges$start_datetime,
    c(
      "2025-01-01 00:00:00",
      "2025-01-01 02:00:00",
      "2025-01-01 05:00:00"
    )
  )
  expect_identical(
    ranges$end_datetime,
    c(
      "2025-01-01 00:00:00",
      "2025-01-01 03:00:00",
      "2025-01-01 05:00:00"
    )
  )
})

test_that("preview class polygons have unique column names", {
  module <- add_cont_data_module_environment()
  ranges <- data.frame(
    id = c("qualifier_1", "qualifier_2"),
    start_dt = as.POSIXct(
      c("2025-01-01 00:00:00", "2025-01-01 02:00:00"),
      tz = "UTC"
    ),
    end_dt = as.POSIXct(
      c("2025-01-01 01:00:00", "2025-01-01 03:00:00"),
      tz = "UTC"
    ),
    code = c("SUS", "SUS"),
    description = c("Suspect", "Suspect"),
    color = c("#ff0000", "#ff0000")
  )

  polygons <- module$add_cont_data_band_polygons(
    ranges,
    c(0, 1, 1, 0),
    "Qualifier"
  )

  expect_identical(anyDuplicated(names(polygons)), 0L)
  expect_identical(names(polygons), c("id", "datetime", "y", "color", "text"))
  expect_equal(nrow(polygons), 8)
})
