test_that("HTML text escaping preserves text without creating markup", {
  expect_identical(
    escape_html_text("<img src=x onerror=alert(1)>&\"'"),
    "&lt;img src=x onerror=alert(1)&gt;&amp;\"'"
  )

  popup <- generate_popup_content(
    swe = 10,
    name = "<img src=x onerror=alert(1)>",
    location = "<script>alert(1)</script>"
  )
  expect_false(grepl("<img", popup, fixed = TRUE))
  expect_false(grepl("<script", popup, fixed = TRUE))
  expect_true(grepl("&lt;img", popup, fixed = TRUE))
})

test_that("numeric SQL expressions accept supported calculations", {
  expect_invisible(validate_numeric_sql_expression(
    "round((upstream + downstream) / 2, 3)",
    allowed_identifiers = c("upstream", "downstream")
  ))
  expect_invisible(validate_numeric_sql_expression(
    "$1 + (0.002 * $2)",
    allowed_placeholders = c(1L, 2L)
  ))
})

test_that("numeric SQL expressions reject executable SQL", {
  expect_error(
    validate_numeric_sql_expression(
      "(SELECT pg_sleep(10))",
      allowed_identifiers = "value"
    ),
    "unsupported"
  )
  expect_error(
    validate_numeric_sql_expression("$1; DELETE FROM continuous.corrections"),
    "only arithmetic"
  )
  expect_error(
    validate_numeric_sql_expression("pg_sleep($1)", allowed_placeholders = 1L),
    "unsupported identifier"
  )
})

test_that("guideline SQL scalar validation blocks modifying CTEs", {
  expect_invisible(validate_guideline_sql_scalar(
    "WITH vals AS (SELECT $1::integer AS sample_id)
     SELECT sample_id::numeric FROM vals"
  ))
  expect_error(
    validate_guideline_sql_scalar(
      "WITH changed AS (DELETE FROM discrete.samples RETURNING sample_id)
       SELECT count(*)::numeric FROM changed"
    ),
    "may not modify data"
  )
  expect_error(
    validate_guideline_sql_scalar("SELECT pg_sleep(10)"),
    "may not modify data"
  )
})

test_that("public app SQL and help-page sinks remain parameterized", {
  continuous <- readLines(system.file(
    "apps/YGwater/modules/client/data/continuousData.R",
    package = "YGwater"
  ))
  continuous <- continuous[!grepl("^[[:space:]]*#", continuous)]
  image_map <- readLines(system.file(
    "apps/YGwater/modules/client/images/image_map_view.R",
    package = "YGwater"
  ))
  help_page <- readLines(system.file(
    "apps/YGwater/www/html/admin_help/page_help_placeholder.html",
    package = "YGwater"
  ))
  water_info <- readLines(system.file(
    "R/waterInfo.R",
    package = "YGwater"
  ))
  wq_report <- readLines(system.file(
    "apps/YGwater/modules/client/reports/WQReport.R",
    package = "YGwater"
  ))

  expect_false(any(grepl(
    "AND date > '",
    continuous,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    "AND date >= '",
    continuous,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    "WHERE image_id = \", id",
    image_map,
    fixed = TRUE
  )))
  expect_false(any(grepl("innerHTML", help_page, fixed = TRUE)))
  expect_false(any(grepl(
    "paste(locations, collapse = \"', '\")",
    water_info,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "!value %in% configured_mdb_files",
    wq_report,
    fixed = TRUE
  )))
})
