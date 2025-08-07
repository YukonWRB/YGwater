# Check and load python environment for tests on plotly objects

if (!isTRUE(getOption("YGwater_pythonSetupDone", FALSE))) {
  warning("tests on plotly objects use an outdated version of the 'kaleido' python package. You should try to reinstall kaleido periodically. See the 'setup.R' file in tests where this message originates.")

  check_miniconda_installed()

  reticulate::use_condaenv("r-reticulate", required = TRUE)

  # Check if python-kaleido is installed and install if necessary
  if (!check_conda_package_installed("r-reticulate", "python-kaleido")) {
    # !!! python-kaleido 0.1.* is old, but later versions hang when saving the object as of 2025-02-25
    reticulate::conda_install("r-reticulate", "python-kaleido==0.1.*")
  }

  # Check if plotly is installed and install if necessary
  if (!check_conda_package_installed("r-reticulate", "plotly")) {
    reticulate::conda_install("r-reticulate", "plotly", channel = "plotly")
  }

  reticulate::use_miniconda('r-reticulate')
  reticulate::py_run_string("import sys")

  # Mark the setup as done for this session.
  options(YGwater_pythonSetupDone = TRUE)
}

# If on CI, set environment variables which would otherwise be found in the user's .Renviron file.
# We're working with a micro postgres database here installed on the CI environment!
if (Sys.getenv("CI") == "true") {
  Sys.setenv(aquacacheName = "testdb",
             aquacacheHost = "localhost",
             aquacachePort = "5432",
             aquacacheUser = "runner",
             aquacachePass = "runner",
             aquacacheAdminUser = "runner",
             aquacacheAdminPass = "runner",
             AQUSER = "readonly",
             AQPASS = "WaterIsLife",
             AQSERVER = "https://yukon.aquaticinformatics.net/AQUARIUS")
  message("Running on CI, setting environment accordingly.")
}

set.seed(123) # Set seed for reproducibility in tests

# Ensure ragg is available for deterministic PNG outputs
if (!requireNamespace("ragg", quietly = TRUE)) {
  install.packages("ragg")
}

# Use a stable base font across platforms
ggplot2::theme_set(ggplot2::theme_gray(base_family = "DejaVu Sans"))

# Function to clean paths that mix \\ with /. On Windows, this poses problems for plotly::save_image.
pathPrep <- function(path) {
  x <- chartr("\\", "/", path)
  return(x)
}
