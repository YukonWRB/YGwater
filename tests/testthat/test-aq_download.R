# Tests depend on parameters on the machine so can't be run on CRAN or CI
skip_on_ci()
skip_on_cran()

dl <- aq_download(loc_id = "YOWN-0804",
                  ts_name = "Wlevel_bgs.Calculated",
                  start = "1950-01-01",
                  end = as.character(Sys.Date()),
                  login = Sys.getenv(c("AQUSER", "AQPASS")),
                  server = "https://yukon.aquaticinformatics.net/AQUARIUS"
)

test_that("timeseries download gets the expected list elements", {
  expect_named(dl, c("metadata", "timeseries", "approvals", "grades"))
})

test_that("metadata is filled out", {
  sum <- nrow(dl$metadata) + ncol(dl$metadata)
  expect_equal(sum, 11)
})

test_that("timeseries has 7 columns",{
  expect_equal(ncol(dl$timeseries), 6)
})

test_that("timeseries has no NA values", {
  expect_equal(nrow(dl$timeseries[rowSums(is.na(dl$timeseries)) > 0,]), 0)
})

test_that ("grades has 4 columns", {
  expect_equal(ncol(dl$grades), 4)
})

test_that ("grades has no NA values",{
  expect_equal(nrow(dl$grades[rowSums(is.na(dl$grades)) > 0,]), 0)
})

test_that ("approvals has 7 columns", {
  expect_equal(ncol(dl$approvals), 7)
})

test_that ("approvals has no NA values", {
  expect_equal(nrow(dl$approvals[rowSums(is.na(dl$approvals)) > 0,]), 0)
})

rm(dl)
