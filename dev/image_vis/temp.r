

library(DBI)
library(RPostgres)

config <<- list(
  dbName = "aquacache",
  dbHost = Sys.getenv("aquacacheHost"),
  dbPort = Sys.getenv("aquacachePort"),
  dbUser = Sys.getenv("aquacacheAdminUser"),
  dbPass = Sys.getenv("aquacacheAdminPass")
)


con = DBI::dbConnect(dbname = config$dbName,
                     host = config$dbHost,
                     port = config$dbPort,
                     user = config$dbUser,
                     password = config$dbPass)




DBI::dbGetQuery(con, "SELECT * FROM files.images LIMIT 19")


has_insert_privileges <- function(con) {
    query <- 
    result <- DBI::dbGetQuery(con, query)
    return(result$can_insert[1])
}

can_insert <- has_insert_privileges(con)
print(can_insert)