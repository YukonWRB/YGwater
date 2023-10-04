test_that("Outputted table has proper dimensions", {
  test <- suppressWarnings(SWE_basin(year=2022,
                     month=c(3,4,5),
                     threshold = 6,
                     csv = FALSE,
                     summarise = FALSE))
  expect_equal(ncol(test), 7)
  expect_true(nrow(test) >= 1)
})

test_that("Outputted summary table has proper dimensions", {
  test <- suppressWarnings(SWE_basin(year=2022,
                    month=c(3,4,5),
                    threshold = 6,
                    csv = FALSE,
                    summarise = TRUE))
  expect_equal(ncol(test), 9)
  expect_true(nrow(test) >= 1)
})
