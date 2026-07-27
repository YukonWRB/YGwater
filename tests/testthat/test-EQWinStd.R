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

test_that("EQWin calculated standard helper uses lookup-table columns", {
  sample_cols <- c(
    "pH-F (pH units)",
    "pH-L (pH units)",
    "Hard-D (mg/L)",
    "Ca-D (mg/L)",
    "Mg-D (mg/L)",
    "Hard-T (mg/L)",
    "Ca-T (mg/L)",
    "Mg-T (mg/L)",
    "C-DOC (mg/L)",
    "Temp-F (C)",
    "Chlord (mg/L)"
  )
  sampledata <- as.data.frame(
    stats::setNames(as.list(rep(NA_real_, length(sample_cols))), sample_cols),
    check.names = FALSE
  )
  sampledata[["pH-F (pH units)"]] <- 8.2
  sampledata[["Hard-D (mg/L)"]] <- 100
  sampledata[["C-DOC (mg/L)"]] <- 2
  sampledata[["Temp-F (C)"]] <- 10
  sampledata[["Chlord (mg/L)"]] <- 3

  out <- YGwater:::eq_std_calc(
    sampledata,
    data.frame(
      MaxVal = c("CCME_Mn-D_lt", "CCME_NH4_lt", "C3AWNH4SW"),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(as.numeric(out$MaxVal), c(240, 1.04, 0.37))

  sampledata[["pH-F (pH units)"]] <- 8.6
  out <- YGwater:::eq_std_calc(
    sampledata,
    data.frame(MaxVal = "C3AWNH4SW", stringsAsFactors = FALSE)
  )
  expect_equal(as.numeric(out$MaxVal), 0.131)
})
