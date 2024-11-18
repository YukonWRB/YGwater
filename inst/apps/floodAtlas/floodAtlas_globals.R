floodAtlas_globals <- function(dbName, dbHost, dbPort = dbPort, dbUser = dbUser, dbPass = dbPass) {
  
  library(shiny)
  library(shinyjs)
  
  # source(system.file("apps/streamLine/modules/mapView.R", package = "YGwater"))
  
  # Establish database connection pool
  if (!exists("pool")) {
    pool <<- pool::dbPool(
      drv = RPostgres::Postgres(),
      dbname = dbName,
      host = dbHost,
      port = dbPort,
      user = dbUser,
      password = dbPass
    )
    DBI::dbExecute(pool, "SET logged_in_user.username = 'public';")
  } else {
    # Check if pool object works
    test <- DBI::dbGetQuery(pool, "SELECT 1;")
    if (nrow(test) == 0) {
      pool <<- pool::dbPool(
        drv = RPostgres::Postgres(),
        dbname = dbName,
        host = dbHost,
        port = dbPort,
        user = dbUser,
        password = dbPass
      )
      DBI::dbExecute(pool, "SET logged_in_user.username = 'public';")
    }
  }
}

