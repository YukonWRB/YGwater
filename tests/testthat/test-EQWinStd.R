# Tests depend on db connection so can't be run on CRAN or CI
skip_on_ci()
skip_on_cran()

test_that("EQWin standard calculations work", {
  if (file.exists("//carver/infosys/EQWin/WaterResources.mdb")) {
    con <- AccessConnect("//carver/infosys/EQWin/WaterResources.mdb", silent = TRUE)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    # Run a calculation for Pb-T at Sa Dena Hes using sampleId 97428
    res <- EQWinStd(41, 97428, con = con)[[1]]$Value
    expect_equal(round(res, 8), 0.0175511)
  } else {
    skip("EQWin database not found, skipping EQWinStd tests.")
  }
})
