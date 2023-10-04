test_that("Outputted table has proper dimensions", {
  test <- suppressWarnings(SWE_station(year=2022,
                       month=3,
                       csv = FALSE, 
                       return_missing = FALSE))
  expect_equal(ncol(test), 9)
  expect_true(nrow(test) >= 1)
})
