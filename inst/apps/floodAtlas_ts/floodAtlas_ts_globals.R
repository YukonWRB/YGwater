floodAtlas_ts_globals <- function(dbName, dbHost, dbPort = dbPort, dbUser = dbUser, dbPass = dbPass) {
  
  library(shiny)
  library(shinyjs)
  
  options(bslib.sass.cache = tools::R_user_dir("YGwater", "cache"))

  config <<- list(
    dbName = dbName,
    dbHost = dbHost,
    dbPort = dbPort,
    dbUser = dbUser,
    dbPass = dbPass)

}

