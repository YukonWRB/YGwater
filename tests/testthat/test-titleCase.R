test_that("titleCase capitalizes English phrases", {
  expect_equal(
    titleCase("the quick brown fox jumps over the lazy dog"),
    "The Quick Brown Fox Jumps Over the Lazy Dog"
  )

  expect_equal(
    titleCase("swe data from eccc", language = "en"),
    "SWE Data from ECCC"
  )
})


test_that("titleCase applies French-specific rules", {
  expect_equal(
    titleCase("l'été à dawson", language = "fr"),
    "L'Été à Dawson"
  )

  expect_equal(
    titleCase("rivière grande", language = "fr"),
    "Rivière Grande"
  )
})


test_that("titleCase validates the requested language", {
  expect_error(titleCase("bonjour", language = "es"), "Language must be either 'en' or 'fr'.")
})
