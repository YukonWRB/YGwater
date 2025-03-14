readRenviron("~/.Renviron")

config <- list(
  dbName = "aquacache",
  dbHost = Sys.getenv("aquacacheHost"),
  dbPort = Sys.getenv("aquacachePort"),
  dbUser = Sys.getenv("aquacacheUser"),
  dbPass = Sys.getenv("aquacachePass")
)


  dbHost = Sys.getenv("aquacacheHost")
