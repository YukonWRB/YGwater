# Functions created to help manage reticulate, miniconda, and installed packaged in conda environments

# Function to check if reticulate package is installed
check_reticulate <- function() {
  rlang::check_installed("reticulate", "to run tests on plotly objects")
}

# Function to check if miniconda is installed
check_miniconda_installed <- function() {
  check_reticulate()
  
  # Attempt to get the version of conda installed
  conda_version_command <- "conda --version"
  
  # Capture the output and status of the command
  conda_check <- tryCatch({
    system(conda_version_command, intern = TRUE, ignore.stderr = TRUE)
  }, error = function(e) NULL)
  
  # Determine if conda is installed based on the command output
  if (!is.null(conda_check) && length(conda_check) > 0 && grepl("conda", conda_check)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Function to retrieve installed packages
conda_installed_packages <- function(env_name) {
  installed_packages <- system(paste0("conda list -n ", env_name), intern = TRUE)
  
  installed_packages <- installed_packages[grep("^[a-z]", installed_packages, ignore.case = TRUE)]
  
  installed_packages <- do.call(rbind, strsplit(installed_packages, "\\s{2,}"))
  installed_packages <- as.data.frame(installed_packages, stringsAsFactors = FALSE)

  colnames(installed_packages) <- c("Name", "Version", "Build", "Channel")
  return(installed_packages)
}

# Function to check if a package is installed in a conda environment
check_conda_package_installed <- function(env_name, package_name) {
  installed_packages <- conda_installed_packages(env_name)
  if (any(installed_packages$Name == package_name)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
