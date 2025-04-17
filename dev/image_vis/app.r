library(shiny)
library(leaflet)
library(DT)
library(YGwater)
library(AquaCache)


config <<- list(
    dbName = "aquacache",
    dbHost = Sys.getenv("aquacacheHost"),
    dbPort = Sys.getenv("aquacachePort"),
    dbUser = Sys.getenv("aquacacheAdminUser"),
    dbPass = Sys.getenv("aquacacheAdminPass")
)


    con = AquaConnect(name = config$dbName,
                                              host = config$dbHost,
                                              port = config$dbPort,
                                              username = config$dbUser,
                                              password = config$dbPass,
                                              silent = TRUE)

    images <- DBI::dbGetQuery(con, "SELECT DISTINCT layer_name FROM spatial.vectors LIMIT 10000")
