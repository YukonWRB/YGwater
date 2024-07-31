test_that("Output table has proper dimensions when querying from AquaCache db", {
  skip_on_ci()
  skip_on_cran()
  test <- SWE_basin(year = 2022,
                    month = c(3,4,5),
                    threshold = 6,
                    csv = FALSE,
                    summarise = FALSE,
                    source = "AquaCache")
  expect_equal(ncol(test), 7)
  expect_true(nrow(test) >= 1)
})

test_that("Output table has proper dimensions when querying from snow db", {
  skip_on_ci()
  skip_on_cran()
  test <- SWE_basin(year = 2022,
                    month = c(3,4,5),
                    threshold = 6,
                    csv = FALSE,
                    summarise = FALSE,
                    source = "snow")
  expect_equal(ncol(test), 7)
  expect_true(nrow(test) >= 1)
})

test_that("Output summary table has proper dimensions", {
  skip_on_ci()
  skip_on_cran()
  test <- SWE_basin(year = 2022,
                    month = c(3,4,5),
                    threshold = 6,
                    csv = FALSE,
                    summarise = TRUE,
                    source = "AquaCache")
  expect_equal(ncol(test), 9)
  expect_true(nrow(test) >= 1)
})
