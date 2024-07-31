# Tests depend on db connection so can't be run on CRAN or CI
skip_on_ci()
skip_on_cran()

test_that("EQWin standard calculations work", {
  if (file.exists("X:/EQWin/WR/DB/Water Resources.mdb")) {
    # Run a calculation for Pb-T at Sa Dena Hes using sampleId 97428
    res <- EQWinStd(41, 97428)
    expect_equal(round(res, 8), 0.0175511)
  }
})
