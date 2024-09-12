test_that("Output (summarise = TRUE) has proper dimensions when reading from AquaCache db", {
  skip_on_ci()
  skip_on_cran()
  test <- suppressWarnings(SWE_station(year = 2022,
                                       month = 3,
                                       csv = FALSE,
                                       return_missing = FALSE,
                                       source = 'AquaCache'))
  expect_equal(ncol(test), 21)
  expect_true(nrow(test) >= 1)
})

test_that("Output table (summarise = FALSE) has proper dimensions when reading from AquaCache db", {
  skip_on_ci()
  skip_on_cran()
  test <- suppressWarnings(SWE_station(year = 2022,
                                       month = 3,
                                       csv = FALSE,
                                       return_missing = FALSE,
                                       source = 'AquaCache',
                                       summarise = FALSE))
  expect_equal(ncol(test), 13)
  expect_true(nrow(test) >= 1)
})

test_that("Output table (summarise = TRUE) has proper dimensions when reading from snow db", {
  skip_on_ci()
  skip_on_cran()
  test <- suppressWarnings(SWE_station(year = 2022,
                                       month = 3,
                                       csv = FALSE,
                                       return_missing = FALSE,
                                       source = 'snow'))
  expect_equal(ncol(test), 20)
  expect_true(nrow(test) >= 1)
})

test_that("Output table (summarise = FALSE) has proper dimensions when reading from snow db", {
  skip_on_ci()
  skip_on_cran()
  test <- suppressWarnings(SWE_station(year = 2022,
                                       month = 3,
                                       csv = FALSE,
                                       return_missing = FALSE,
                                       source = 'snow',
                                       summarise = FALSE))
  expect_equal(ncol(test), 12)
  expect_true(nrow(test) >= 1)
})
