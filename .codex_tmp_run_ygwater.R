pkgload::load_all(".", quiet = TRUE)
source(file.path("inst", "apps", "YGwater", "YGwater_globals.R"))
YGwater_globals(
  dbName = Sys.getenv("aquacacheName", "aquacache"),
  dbHost = "10.250.12.154",
  dbPort = Sys.getenv("aquacachePort", "5432"),
  dbUser = Sys.getenv("aquacacheUser"),
  dbPass = Sys.getenv("aquacachePass"),
  RLS_user = Sys.getenv("aquacacheRLSUser"),
  RLS_pass = Sys.getenv("aquacacheRLSPass"),
  network_check = FALSE,
  accessPath1 = NULL,
  accessPath2 = NULL,
  logout_timer_min = 10,
  analytics = FALSE,
  public = FALSE,
  brand = "yukon"
)
shiny::runApp(
  appDir = file.path("inst", "apps", "YGwater"),
  host = "127.0.0.1",
  port = 38497,
  launch.browser = FALSE
)
