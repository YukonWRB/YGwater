test_that("addTimeseries explains timeseries uniqueness conflicts", {
  module_text <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/admin/continuousData/addTimeseries.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    module_text,
    "ON CONFLICT ON CONSTRAINT timeseries_unique DO NOTHING",
    fixed = TRUE
  )
  expect_match(
    module_text,
    "if (nrow(new_timeseries) == 0L)",
    fixed = TRUE
  )
  expect_match(
    module_text,
    "DBI::dbRollback(con)",
    fixed = TRUE
  )
  expect_match(
    module_text,
    "Timeseries must be unique by location, parameter, aggregation type, media, matrix state, record rate, elevation/depth (z), sensor priority, sub-location, and timeseries type.",
    fixed = TRUE
  )
})
