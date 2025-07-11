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
