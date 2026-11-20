floodAtlas_over_globals <- function(
  dbName,
  dbHost,
  dbPort = dbPort,
  dbUser = dbUser,
  dbPass = dbPass,
  timezone = "Etc/GMT+7"
) {
  library(shiny)
  library(shinyjs)

  config <<- list(
    dbName = dbName,
    dbHost = dbHost,
    dbPort = dbPort,
    dbUser = dbUser,
    dbPass = dbPass,
    timezone = timezone
  )
}
