continuous_cache_source_path <- function() {
  checkout_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "apps",
    "YGwater",
    "modules",
    "cache_functions.R"
  )
  if (file.exists(checkout_path)) {
    return(checkout_path)
  }
  system.file(
    "apps",
    "YGwater",
    "modules",
    "cache_functions.R",
    package = "YGwater"
  )
}

count_fixed_matches <- function(text, pattern) {
  matches <- gregexpr(pattern, text, fixed = TRUE)[[1]]
  if (identical(matches[[1]], -1L)) 0L else length(matches)
}

test_that("continuous cache predicates require an actual measurement", {
  env <- new.env(parent = baseenv())
  sys.source(continuous_cache_source_path(), envir = env)

  sql <- env$continuous_timeseries_has_measurements_sql("candidate")

  expect_match(
    gsub("\\s+", " ", trimws(sql)),
    paste(
      "EXISTS \\( SELECT 1 FROM continuous.measurements_continuous mc",
      "WHERE mc.timeseries_id = candidate.timeseries_id \\)"
    )
  )
})

test_that("every continuous cache timeseries query applies the predicate", {
  source <- paste(
    readLines(continuous_cache_source_path(), warn = FALSE),
    collapse = "\n"
  )
  source <- gsub(
    "continuous.timeseries AS ts",
    "continuous.timeseries ts",
    source,
    fixed = TRUE
  )

  expect_equal(
    count_fixed_matches(source, "continuous.timeseries ts"),
    count_fixed_matches(
      source,
      'continuous_timeseries_has_measurements_sql("ts")'
    )
  )
})
