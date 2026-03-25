test_that("round_any rounds using the supplied function", {
  expect_equal(YGwater:::round_any(c(1.1, 1.6), 1), c(1, 2))
  expect_equal(YGwater:::round_any(5.7, 2, floor), 4)
  expect_equal(YGwater:::round_any(5.1, 2, ceiling), 6)
})


test_that("iso_period converts numeric hours to ISO 8601 durations", {
  expect_equal(YGwater:::iso_period(0), "P0DT0H0M0S")
  expect_equal(YGwater:::iso_period(26.5), "P1DT2H30M0S")
  expect_equal(
    YGwater:::iso_period(c(1.5, 50.75)),
    c("P0DT1H30M0S", "P2DT2H45M0S")
  )
})

test_that("translations work correctly", {
  # Ensure package data exists
  check <- data$translations
  expect_true(!is.null(check))
  expect_true(length(check) > 0)
  expect_true(is.list(check))

  expect_equal(tr("home", "English"), "Home")
  expect_equal(tr("home", "Français"), "Accueil")
})

test_that("parameter unit SQL supports legacy text unit columns", {
  fake_con <- structure(list(), class = "mock_connection")

  expr <- testthat::with_mocked_bindings(
    ac_parameter_unit_select_sql(fake_con, "p", "unit"),
    ac_db_column_info = function(...) {
      data.frame(
        column_name = c("unit_default", "unit_solid"),
        data_type = c("text", "text")
      )
    },
    ac_db_table_exists = function(...) {
      FALSE
    },
    .package = "YGwater"
  )

  expect_equal(
    expr,
    "COALESCE(NULLIF(BTRIM(p.unit_default), ''), NULLIF(BTRIM(p.unit_solid), '')) AS unit"
  )
})

test_that("parameter unit SQL supports normalized unit id columns", {
  fake_con <- structure(list(), class = "mock_connection")

  expr <- testthat::with_mocked_bindings(
    ac_parameter_unit_select_sql(fake_con, "p", "unit"),
    ac_db_column_info = function(...) {
      data.frame(
        column_name = c("unit_default", "unit_solid"),
        data_type = c("integer", "integer")
      )
    },
    ac_db_table_exists = function(...) {
      TRUE
    },
    .package = "YGwater"
  )

  expect_equal(
    expr,
    paste0(
      "COALESCE(",
      "(SELECT u.unit_name FROM public.units u WHERE u.unit_id = p.unit_default), ",
      "(SELECT u.unit_name FROM public.units u WHERE u.unit_id = p.unit_solid)",
      ") AS unit"
    )
  )
})
