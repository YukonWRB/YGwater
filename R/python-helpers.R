# Functions created to help manage reticulate, miniconda, and installed packaged in conda environments

#' Check if reticulate package is installed
#'
#' @param silent If TRUE, reticulate is installed if missing without any user input.
#' @return Nothing.
#' @export
#'

check_reticulate <- function(silent) {
  if (!rlang::is_installed("reticulate")) {
    if (silent) {                                                           
      message("Installing dependency 'reticulate'...")
      utils::install.packages("reticulate")
      message("Installed.")
    } else {
      message("Package 'reticulate' is required to use this function. Would you like to install it now? (y/n)")
      response <- readline()
      if (tolower(response) == "y") {
        utils::install.packages("reticulate")
        message("Installed.")
      } else {                
        stop("Package 'reticulate' is required to use this function.")
      }                                                           
    }
  } else {
    if (!silent) {
      message("Package 'reticulate' is already installed.")
    }
  }
}


#' Check if miniconda is installed
#'
#' Checks for a miniconda installation and installs it if necessary using reticulate::install_miniconda.
#' 
#' @return Nothing.
#' @export
#'

check_miniconda_installed <- function() {
  check_reticulate(silent = TRUE)
  # Attempt to install Miniconda, handling errors appropriately
  installed <- FALSE
  tryCatch({
    reticulate::install_miniconda()
    message("Installed miniconda.")
  }, error = function(e) {
    # Check if the error is because Miniconda is already installed
    if (!grepl("Miniconda is already installed at", e$message)) {
     installed <<- TRUE
    }
  })
  if (installed) {
    message("miniconda is already installed.")
  }
}

#' Retrieve installed packages in a conda environment
#'
#' @param env_name The name of the version of Python to be used by `reticulate`.
#'
#' @return The installed packages in the specified conda environment.
#' @export
#'

conda_installed_packages <- function(env_name) {
  installed_packages <- system(paste0("conda list -n ", env_name), intern = TRUE)
  
  installed_packages <- installed_packages[grep("^[a-z]", installed_packages, ignore.case = TRUE)]
  
  installed_packages <- suppressWarnings(do.call(rbind, strsplit(installed_packages, "\\s{2,}")))
  installed_packages <- as.data.frame(installed_packages, stringsAsFactors = FALSE)

  colnames(installed_packages) <- c("Name", "Version", "Build", "Channel")
  return(installed_packages)
}

#' Check if a specific package is installed in a conda environment
#'
#' @param env_name The name of the version of Python to be used by `reticulate`.
#' @param package_name The name of the caonda package to check for.
#'
#' @return TRUE if installed, FALSE if not.
#' @export

check_conda_package_installed <- function(env_name, package_name) {
  installed_packages <- conda_installed_packages(env_name)
  if (any(installed_packages$Name == package_name)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
