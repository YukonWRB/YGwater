floodAtlas_ts_globals <- function(dbName, dbHost, dbPort = dbPort, dbUser = dbUser, dbPass = dbPass) {
  
  library(shiny)
  library(shinyjs)
  
  config <<- list(
    dbName = dbName,
    dbHost = dbHost,
    dbPort = dbPort,
    dbUser = dbUser,
    dbPass = dbPass)

}

