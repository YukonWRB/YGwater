test_that("all workbooks are created when a DB connection can be made", {
  skip_on_ci()
  skip_on_cran()
  tempdir <- withr::local_tempdir()
  expect_no_error(suppressWarnings(createSnowTemplate("2023-03-01", circuits = "all", save_path = tempdir)))
  files <- list.files(tempdir)
  expect_equal(length(files), 15)
})
