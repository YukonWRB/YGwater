



library(AquaCache)
library(YGwater)

readRenviron("~/.Renviron")


config <- list(
  dbName = "aquacache",
  dbHost = Sys.getenv("aquacacheHost"),
  dbPort = Sys.getenv("aquacachePort"),
  dbUser = Sys.getenv("aquacacheUser"),
  dbPass = Sys.getenv("aquacachePass")
)


  dbHost = Sys.getenv("aquacacheHost")


con <- AquaConnect(name = config$dbName, 
                   host = config$dbHost, 
                   port = config$dbPort, 
                   user = config$dbUser, 
                   pass = config$dbPass)



image_types <- DBI::dbGetQuery(con, "SELECT * FROM files.image_types")
rownames(types) <- types$image_type_id


image_tags <- types[type_id, "default_tag_options", drop = TRUE] %>%
  strsplit(",") %>%
  unlist() %>%
  trimws() %>%
  gsub("[{}]", "", .) %>%
  gsub('\"', "", .)

