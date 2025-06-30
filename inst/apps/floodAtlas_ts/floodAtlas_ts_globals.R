floodAtlas_ts_globals <- function(dbName, dbHost, dbPort = dbPort, dbUser = dbUser, dbPass = dbPass) {
  
  library(shiny)
  library(shinyjs)
  
  # Use a user-writable cache directory for sass
  cache_dir <- tools::R_user_dir("YGwater", "cache")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  options(bslib.sass.cache = cache_dir)

  config <<- list(
    dbName = dbName,
    dbHost = dbHost,
    dbPort = dbPort,
    dbUser = dbUser,
    dbPass = dbPass)

}

