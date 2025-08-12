test_that("all workbooks are created when a DB connection can be made", {
  skip_on_ci()
  skip_on_cran()
  
  # Ensure datatase connection can be made
  sk <- FALSE
  tryCatch({
    con <- snowConnect(silent = TRUE)
  }, error = function(e) {
    sk <<- TRUE
  })
  if (sk) {
    skip("Could not connect to snow survey database for snow workbook tests")
  }
  
  tempdir <- paste0(tempdir(), "/createSnowTemplateTest")
  dir.create(tempdir, recursive = TRUE)
  on.exit(unlink(tempdir, recursive = TRUE), add = TRUE)
  expect_no_error(suppressWarnings(createSnowTemplate("2023-03-01", circuits = "all", save_path = tempdir, snowCon = con)))
  files <- list.files(tempdir)
  expect_equal(length(files), 15)
})
