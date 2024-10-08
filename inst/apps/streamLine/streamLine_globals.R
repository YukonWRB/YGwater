streamLine_globals <- function(dev = FALSE) {
  library(shiny)
  library(shinyjs)
  
  source(system.file("apps/streamLine/modules/mapView.R", package = "YGwater"))
  source(system.file("apps/streamLine/modules/dataView.R", package = "YGwater"))
  source(system.file("apps/streamLine/modules/homeView.R", package = "YGwater"))
  source(system.file("apps/streamLine/modules/plotView.R", package = "YGwater"))
  source(system.file("apps/streamLine/modules/docView.R", package = "YGwater"))
  source(system.file("apps/streamLine/modules/imgView.R", package = "YGwater"))
  source(system.file("apps/streamLine/modules/aboutView.R", package = "YGwater"))
  
  translations <<- data.table::setDT(openxlsx::read.xlsx(system.file("apps/streamLine/translations.xlsx", package = "YGwater"), sheet = 1))
  
  # Function to get correct text from translations table
  translate <<- function(id, lang) {
    translations[id == id, lang]
  }
  
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
