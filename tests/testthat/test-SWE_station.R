test_that("Outputted table (summarise=TRUE) has proper dimensions when reading from hydromet db", {
  test <- suppressWarnings(SWE_station(year=2022,
                       month=3,
                       csv = FALSE,
                       return_missing = FALSE,
                       source = 'hydromet'))
  expect_equal(ncol(test), 19)
  expect_true(nrow(test) >= 1)
})

test_that("Outputted table (summarise=FALSE) has proper dimensions when reading from hydromet db", {
  test <- suppressWarnings(SWE_station(year = 2022,
                                       month = 3,
                                       csv = FALSE,
                                       return_missing = FALSE,
                                       source = 'hydromet',
                                       summarise = FALSE))
  expect_equal(ncol(test), 11)
  expect_true(nrow(test) >= 1)
})

test_that("Outputted table (summarise=TRUE) has proper dimensions when reading from snow db", {
  test <- suppressWarnings(SWE_station(year=2022,
                                       month=3,
                                       csv = FALSE,
                                       return_missing = FALSE,
                                       source = 'snow'))
  expect_equal(ncol(test), 19)
  expect_true(nrow(test) >= 1)
})

test_that("Outputted table (summarise=FALSE) has proper dimensions when reading from snow db", {
  test <- suppressWarnings(SWE_station(year = 2022,
                                       month = 3,
                                       csv = FALSE,
                                       return_missing = FALSE,
                                       source = 'snow',
                                       summarise = FALSE))
  expect_equal(ncol(test), 11)
  expect_true(nrow(test) >= 1)
})
