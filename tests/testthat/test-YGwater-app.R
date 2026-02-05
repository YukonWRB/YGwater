library(shinytest2)

test_that("application loads home page", {
  skip_on_cran()

  app <- AppDriver$new(YGwater(), name = "ygwater")
  app$expect_values()
})
