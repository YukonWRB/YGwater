test_that("Connection can be made", {
  skip_on_cran()

  expect_message({
    con <- test_AquaConnect(silent = FALSE)
    on.exit(DBI::dbDisconnect(con))
  })

  # a trivial query must work
  expect_identical(
    DBI::dbGetQuery(con, "SELECT 1 AS x")$x,
    1L
  )
})

test_that("AquaConnect compares patch numbers numerically", {
  version <- "9"
  local_mocked_bindings(
    dbConnect = function(...) list(),
    dbExecute = function(...) 0L,
    dbGetQuery = function(con, statement, ...) {
      if (identical(statement, "SELECT 1")) data.frame(x = 1L) else data.frame(version)
    },
    .package = "DBI"
  )

  expect_warning(AquaConnect(silent = TRUE), "This database is at Patch 9")
  version <- "71"
  expect_warning(AquaConnect(silent = TRUE), "This database is at Patch 71")
  version <- "72"
  expect_no_warning(AquaConnect(silent = TRUE))
  version <- "100"
  expect_no_warning(AquaConnect(silent = TRUE))
})
