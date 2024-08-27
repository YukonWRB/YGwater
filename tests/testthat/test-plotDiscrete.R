# Tests depend on snapshots so can't be inspected on CRAN or CI
skip_on_ci()
skip_on_cran()

# Note: these tests depend on installation of Python and a few libraries. This is taken care of in the setup.R file within the testthat folder.

warning("tests on plotly objects weren't working in R 4.3.3 and up. These tests have been disabled.")
# 
# test_that("discrete plots work from EQWin DB", {
#   
#   # With locGrp and paramGrp
#   plot <- plotDiscrete(start = "2023-06-01", end = "2023-07-01", locations = NULL, locGrp = "QZ Eagle Gold HLF", parameters = NULL, paramGrp = "EG-HLF-failure", log = TRUE, facet_on = 'params', rows = 'auto', dbSource = "EQ", dbPath = "//carver/infosys/EQWin/WR/DB/Water Resources.mdb")
# })
