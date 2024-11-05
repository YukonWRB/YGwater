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
        dbname = "AquaCache_dev",
        host = Sys.getenv("AquaCacheHost"),
        port = Sys.getenv("AquaCachePort"),
        user = Sys.getenv("AquaCacheUser"),
        password = Sys.getenv("AquaCachePass")
      )
    }
  }
}

