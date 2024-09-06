# Tests depend on parameters on the machine so can't be run on CRAN or CI
skip_on_ci()
skip_on_cran()

test_that("wolf creek curve download works", {
  res <- aq_download_curves("29AB009", "2023-01-01", "2024-09-01")
  expect_type(res, "list")
  expect_length(res, 2)
  expect_type(res[[1]], "list")
  expect_type(res[[2]], "list")
})
