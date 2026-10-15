test_that("YEC plot renders selected traces", {
  env <- new.env(parent = globalenv())
  sys.source(file.path("inst", "apps", "YGwater", "modules", "client", "reports", "yecDashboard.R"), env)
  env$yec_dashboard_thresholds <- function(...) list(thresholds = data.frame(), types = data.frame(), colours = data.frame())
  env$yec_dashboard_threshold_segments <- function(...) data.frame()
  env$yec_dashboard_threshold_margins <- function(dat, ...) data.frame(
    below_fsl = rep(NA_real_, nrow(dat)), above_lsl = NA_real_,
    below_alert = NA_real_, above_min_flow = NA_real_
  )
  dat <- expand.grid(
    location_code = c("09DC007", "09AB001"), parameter = c("Water level", "Discharge"),
    index = 1:3, stringsAsFactors = FALSE
  )
  dat$name <- dat$location_code
  dat$latitude <- 60 + as.numeric(dat$location_code == "09AB001")
  dat$longitude <- -135 - as.numeric(dat$location_code == "09AB001")
  dat$latest_value <- dat$index
  dat$latest_time <- as.POSIXct("2026-09-18 12:00:00", tz = "UTC")
  dat$units <- ifelse(dat$parameter == "Water level", "m", "cms")
  dat$change_24h <- 1; dat$change_48h <- 2; dat$change_1w <- 3; dat$data_age_hours <- 1
  dat$datetime <- dat$latest_time - (4 - dat$index) * 3600
  dat$value <- dat$index
  dat$start_time <- dat$latest_time - 7 * 86400
  dat$end_time <- dat$latest_time
  env$yec_dashboard_data <- function(...) dat
  shiny::testServer(env$yecDashboardMod, args = list(language = NULL), {
    session$userData$AquaCache <- TRUE
    session$setInputs(parameter_view = "Water level", record_length = "1-week")
    session$flushReact()
    expect_true(nzchar(output$stations_map))
    expect_true(nzchar(output$station_plot))
    session$setInputs(level_table_rows_selected = c(1L, 2L))
    session$flushReact()
    expect_true(nzchar(output$station_plot))
  })
})
