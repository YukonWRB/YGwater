


library(AquaCache)
library(YGwater)

config <<- list(
  dbName = "aquacache",
  dbHost = Sys.getenv("aquacacheHost"),
  dbPort = Sys.getenv("aquacachePort"),
  dbUser = Sys.getenv("aquacacheAdminUser"),
  dbPass = Sys.getenv("aquacacheAdminPass")
)

con <- AquaConnect(name = config$dbName, 
                   host = config$dbHost, 
                   port = config$dbPort, 
                   user = config$dbUser, 
                   pass = config$dbPass)



filename <- "C:\\Users\\esniede\\Documents\\github\\YGwater\\dev\\img_import_gui\\data\\PXL_20240916_065110890.jpg"
datetime <- as.POSIXct(Sys.time())
location = 162
insertACImage(con=con, object=filename, datetime=datetime, location=location)


on.exit(DBI::dbDisconnect(con))


share_with = c("public")
latitude = 0
longitude = 0
extension = "jpg"
file = "okok"

