library(shiny)
library(shinyjs)


# Establish database connection
if (!exists("pool")) {
  pool <- pool::dbPool(
    drv = RPostgres::Postgres(),
    dbname = "hydromet",
    host = Sys.getenv("hydrometHost"),
    port = Sys.getenv("hydrometPort"),
    user = Sys.getenv("hydrometUser"),
    password = Sys.getenv("hydrometPass")
  )
}
