# Check and load python environment for tests on plotly objects

if (!isTRUE(getOption("YGwater_pythonSetupDone", FALSE))) {
  print(
    "Setting up python environment for tests on plotly objects. Please be patient, this takes up to a minute. This is done only once per R session."
  )
  warning(
    "tests on plotly objects use an outdated version of the 'kaleido' python package. You should try to reinstall kaleido periodically. See the 'setup.R' file in tests where this message originates."
  )

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
# We're working with a micro postgres database here installed on the CI environment; see the .github/workflows/R-CMD-check.yaml file.
# This DB is created with AquaCache::create_test_db() and default parameters, expect for the username which uses 'postgres'
if (Sys.getenv("CI") == "true") {
  Sys.setenv(
    aquacacheName = "testdb",
    aquacacheHost = "localhost",
    aquacachePort = "5432",
    aquacacheUser = "runner",
    aquacachePass = "runner",
    aquacacheAdminUser = "runner",
    aquacacheAdminPass = "runner",
    aquacacheTestHost = "localhost",
    aquacacheTestPort = "5432",
    aquacacheTestUser = "runner",
    aquacacheTestPass = "runner",
    AQUSER = "readonly",
    AQPASS = "WaterIsLife",
    AQSERVER = "https://yukon.aquaticinformatics.net/AQUARIUS"
  )
  message("Running on CI, setting environment accordingly.")
} else {
  # If not running onf CI, check that the datbase 'testdb' exists at the host and port in the .Renviron file. If not, instruct user to craete it with AquaCache::create_test_db()
  test_db_exists <- tryCatch(
    {
      con <- AquaConnect(
        name = Sys.getenv("aquacacheTestName"),
        host = Sys.getenv("aquacacheTestHost"),
        port = Sys.getenv("aquacacheTestPort"),
        user = Sys.getenv("aquacacheTestUser"),
        password = Sys.getenv("aquacacheTestPass")
      )
      DBI::dbDisconnect(con)

      message(
        "Database 'testdb' found at the host and port specified in your .Renviron file. Proceeding with tests."
      )
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )

  if (!test_db_exists) {
    stop(
      "Database 'testdb' not found at the host and port specified in your .Renviron file. Please create it with AquaCache::create_test_db() before running the tests."
    )
  }
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

# Function to override default AquaConnect() parameters with those from the .Renviron file, for use in tests. This allows us to use a test database on CI and a different one locally without changing the code of the tests.
test_con <- function(
  name = Sys.getenv("aquacacheTestName"),
  host = Sys.getenv("aquacacheTestHost"),
  port = Sys.getenv("aquacacheTestPort"),
  user = Sys.getenv("aquacacheTestUser"),
  password = Sys.getenv("aquacacheTestPass"),
  silent = FALSE
) {
  AquaConnect(
    name = name,
    host = host,
    port = port,
    username = user,
    password = password,
    silent = silent
  )
}
