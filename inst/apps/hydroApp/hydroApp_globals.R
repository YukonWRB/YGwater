hydroApp_globals <- function(dev) {
  library(shiny)
  library(shinyjs)
  # Establish database connection
  if (dev) {
    if (!exists("pool")) {
      pool <<- pool::dbPool(
        drv = RPostgres::Postgres(),
        dbname = "AquaCache",
        host = Sys.getenv("AquaCacheHost"),
        port = Sys.getenv("AquaCachePort"),
        user = Sys.getenv("AquaCacheUser"),
        password = Sys.getenv("AquaCachePass")
      )
    }
  } else {
    if (!exists("pool")) {
      pool <<- pool::dbPool(
        drv = RPostgres::Postgres(),
        dbname = "hydromet",
        host = Sys.getenv("hydrometHost"),
        port = Sys.getenv("hydrometPort"),
        user = Sys.getenv("hydrometUser"),
        password = Sys.getenv("hydrometPass")
      )
    }
  }
}

