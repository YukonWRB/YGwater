hydroApp_globals <- function(dev) {
  library(shiny)
  library(shinyjs)
  # Establish database connection
  if (dev) {
    if (!exists("pool")) {
      pool <<- pool::dbPool(
        drv = RPostgres::Postgres(),
        dbname = "AquaCache",
        host = Sys.getenv("aquacacheHost"),
        port = Sys.getenv("aquacachePort"),
        user = Sys.getenv("aquacacheUser"),
        password = Sys.getenv("aquacachePass")
      )
    }
  } else {
    if (!exists("pool")) {
      pool <<- pool::dbPool(
        drv = RPostgres::Postgres(),
        dbname = "AquaCache_dev",
        host = Sys.getenv("aquacacheHost"),
        port = Sys.getenv("aquacachePort"),
        user = Sys.getenv("aquacacheUser"),
        password = Sys.getenv("aquacachePass")
      )
    }
  }
}

