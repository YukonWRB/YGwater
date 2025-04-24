library(httr)

XML_FILE_URL <- "https://www.weather.gov/source/aprfc/toCanada/YukonFlows.xml"
PEM_FILE <- "../weather-gov-chain.pem"

args = commandArgs(trailingOnly=TRUE)

lib_loc <- args[1] # $R_LIB
output_file <- args[2]    # for FEWS error/warnings %WORK_DIR
output_folder <- args[3]  # for IMPORT routine %REGION_HOME%\import

save_file <- paste0(output_folder, "/YukonFlows.xml")

## start message thread for FEWS error messaging
fc <- file(output_file, open='w+')
on.exit(close(fc))

writeLines("DEBUG - Initializing DownloadAPRFCData.R log", fc)

## setup library paths
.libPaths(lib_loc)
writeLines(sprintf("DEBUG - .libPaths are: %s", paste(.libPaths(), collapse="\n")), fc)
writeLines(sprintf("DEBUG - lib_loc is: %s",lib_loc), fc)
writeLines(sprintf("DEBUG - output_folder is: %s",output_folder), fc)


# Check if the PEM file exists
if (!file.exists(PEM_FILE)) {
    writeLines(sprintf("ERROR - PEM SSL certificate file does not exist: %s", PEM_FILE), fc)
    stop("PEM file does not exist: ", PEM_FILE)
}

# 200 is success
#(status_code(response) == 200)
result <- tryCatch({
    response <- GET(XML_FILE_URL, config = config(ssl_verifypeer = TRUE, cainfo = PEM_FILE))
    writeBin(content(response, "raw"), file.path(save_file))
    writeLines(sprintf("INFO - Downloaded file from %s", XML_FILE_URL), fc)
  },
  error = function(cond) {
    writeLines(sprintf("ERROR - Downloading file from %s: %s", XML_FILE_URL, as.character(cond)), fc)
    stop()
    close(fc)
    return(FALSE)
  },
  warning = function(cond) {
    writeLines(sprintf("WARNING - Downloading file from %s: %s", XML_FILE_URL, as.character(cond)), fc)
  }
)

## wrap up
writeLines(sprintf("INFO - DownloadRWISData Rscript finished"), fc)
close(fc)