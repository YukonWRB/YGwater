#Initial checks to make sure things run. These functions are defined in R/python-helpers.R
check_miniconda_installed()

reticulate::use_condaenv("r-reticulate", required = TRUE)

# Check if python-kaleido is installed and install if necessary
if (!check_conda_package_installed("r-reticulate", "python-kaleido")) {
  reticulate::conda_install("r-reticulate", "python-kaleido")
}

# Check if plotly is installed and install if necessary
if (!check_conda_package_installed("r-reticulate", "plotly")) {
  reticulate::conda_install("r-reticulate", "plotly", channel = "plotly")
}

reticulate::use_miniconda('r-reticulate')
