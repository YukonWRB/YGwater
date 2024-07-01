test_that("all workbooks are created when a DB connection can be made", {
  tempdir <- withr::local_tempdir()
  expect_no_error(suppressWarnings(createSnowTemplate("2023-03-01", circuit = "all", save_path = tempdir)))
  files <- list.files(tempdir)
  expect_equal(length(files), 14)
})

test_that("all workbooks are created when a DB connection cannot be made", {
  # Set a fake port to prevent a connection to the database
  old_port <- Sys.getenv("snowPort")
  on.exit(Sys.setenv(snowPort = old_port))
  Sys.setenv(snowPort = "9999")
  
  tempdir <- withr::local_tempdir()
  expect_warning(createSnowTemplate("2023-03-01", circuit = "all", save_path = tempdir), class = "warning")
  files <- list.files(tempdir)
  expect_equal(length(files), 14)
})
