# Note: these tests depend on installation of Python and a few libraries. This is taken care of in the setup.R file within the testthat folder.

skip_on_cran()

# Note: these tests depend on installation of Python and a few libraries. This is taken care of in the setup.R file within the testthat folder.

test_con <- test_AquaConnect(silent = TRUE)
on.exit(DBI::dbDisconnect(test_con), add = TRUE)

wlevel <- DBI::dbGetQuery(
  test_con,
  "SELECT parameter_id FROM parameters WHERE param_name = 'water level';"
)$parameter_id[[1]]

flow <- DBI::dbGetQuery(
  test_con,
  "SELECT parameter_id FROM parameters WHERE param_name = 'water flow';"
)$parameter_id[[1]]

# Find the first water level timeseries in the DB
wlevel_ts <- DBI::dbGetQuery(
  test_con,
  paste0(
    "SELECT location_id, parameter_id, timeseries_id, EXTRACT(EPOCH FROM ts.record_rate) AS record_rate, aggregation_type_id, start_datetime, end_datetime FROM timeseries ts WHERE parameter_id = ",
    wlevel,
    " LIMIT 1;"
  )
)

flow_ts <- DBI::dbGetQuery(
  test_con,
  paste0(
    "SELECT location_id, parameter_id, timeseries_id, EXTRACT(EPOCH FROM ts.record_rate) AS record_rate, aggregation_type_id,start_datetime, end_datetime FROM timeseries ts WHERE parameter_id = ",
    flow,
    " LIMIT 1;"
  )
)

test_that("plotMultiTimeseries with all defaults is as expected", {
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test1.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotMultiTimeseries(
    con = test_con,
    locations = c(wlevel_ts$location_id[1], flow_ts$location_id[1]),
    parameters = c(wlevel, flow),
    record_rates = c(wlevel_ts$record_rate[1], flow_ts$record_rate[1]),
    aggregation_types = c(
      wlevel_ts$aggregation_type_id[1],
      flow_ts$aggregation_type_id[1]
    ),
    start_date = wlevel_ts$end_datetime[1] - lubridate::days(2),
    end_date = wlevel_ts$end_datetime[1],
    datum = FALSE
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("plotMultiTimeseries returns data as expected", {
  plot <- plotMultiTimeseries(
    con = test_con,
    locations = c(wlevel_ts$location_id[1], flow_ts$location_id[1]),
    parameters = c(wlevel, flow),
    record_rates = c(wlevel_ts$record_rate[1], flow_ts$record_rate[1]),
    aggregation_types = c(
      wlevel_ts$aggregation_type_id[1],
      flow_ts$aggregation_type_id[1]
    ),
    start_date = wlevel_ts$end_datetime[1] - lubridate::days(2),
    end_date = wlevel_ts$end_datetime[1],
    data = TRUE,
    datum = FALSE
  )$data
  expect_type(plot, "list")
  expect_equal(length(plot), 2) # should have two elements, for both timeseries plotted
  expect_equal(length(plot[[1]]), 2) # Should have the trace_data and range_data data.tables
  expect_named(plot[[1]], c("range_data", "trace_data"))
  expect_named(plot[[1]]$trace_data, c("datetime", "value"))
  expect_named(
    plot[[1]]$range_data,
    c("datetime", "min", "max", "q75", "q25")
  )
})

test_that("plotMultiTimeseries subplots preserve inverted y-axis orientation", {
  plot <- plotMultiTimeseries(
    type = "subplots",
    con = test_con,
    locations = c(wlevel_ts$location_id[1], flow_ts$location_id[1]),
    parameters = c(wlevel, flow),
    record_rates = c(wlevel_ts$record_rate[1], flow_ts$record_rate[1]),
    aggregation_types = c(
      wlevel_ts$aggregation_type_id[1],
      flow_ts$aggregation_type_id[1]
    ),
    start_date = wlevel_ts$end_datetime[1] - lubridate::days(2),
    end_date = wlevel_ts$end_datetime[1],
    datum = FALSE,
    historic_range = FALSE,
    invert = c(TRUE, FALSE),
    webgl = FALSE
  )

  expect_identical(plot$x$layout$yaxis$autorange, "reversed")
  expect_true(isTRUE(plot$x$layout$yaxis2$autorange))
})

test_that("plotMultiTimeseries accepts hourly resolution", {
  # Expect a warning about datums not being applied
  expect_warning(
    plot <- plotMultiTimeseries(
      con = test_con,
      locations = c(wlevel_ts$location_id[1], flow_ts$location_id[1]),
      parameters = c(wlevel, flow),
      record_rates = c(wlevel_ts$record_rate[1], flow_ts$record_rate[1]),
      aggregation_types = c(
        wlevel_ts$aggregation_type_id[1],
        flow_ts$aggregation_type_id[1]
      ),
      start_date = wlevel_ts$end_datetime[1] - lubridate::days(2),
      end_date = wlevel_ts$end_datetime[1],
      resolution = "hour",
      historic_range = TRUE,
      data = TRUE
    )$data,
    "^Datum.*meters.+$"
  )

  expect_equal(length(plot), 2)
  expect_named(plot[[1]], c("range_data", "trace_data"))
  expect_named(plot[[1]]$trace_data, c("datetime", "value"))
  expect_gt(nrow(plot[[1]]$trace_data), 10)
})

test_that("plotMultiTimeseries can show data in the past", {
  skip_on_ci() # Because the CI instance would not have the necessary historical data
  con <- AquaConnect(silent = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (
    !isTRUE(DBI::dbGetQuery(
      con,
      "SELECT has_schema_privilege(current_user, 'audit', 'USAGE') AS ok;"
    )$ok[[1]])
  ) {
    skip("Historical queries require USAGE on schema audit.")
  }

  tsid <- DBI::dbGetQuery(
    con,
    "SELECT timeseries_id FROM timeseries WHERE parameter_id = (SELECT parameter_id FROM parameters WHERE param_name = 'water level') AND location_id = (SELECT location_id FROM locations WHERE location_code = '09EA004') LIMIT 1;"
  )$timeseries_id[[1]]

  as_of <- as.POSIXct("2026-03-30 12:00:00", tz = "UTC")
  start_dt <- as.POSIXct("2022-06-01 00:00:00", tz = "UTC")
  end_dt <- as.POSIXct("2022-06-02 23:59:59", tz = "UTC")

  # Check if the connection can access function 'measurements_calculated_daily_at' which is used for historical queries. If not, skip the test.

  yes <- FALSE
  tryCatch(
    {
      DBI::dbGetQuery(
        con,
        paste(
          "SELECT date, value, max, min, q75, q25",
          "FROM continuous.measurements_calculated_daily_at(",
          "  $1,",
          "  ARRAY[$2]::INTEGER[],",
          "  $3::DATE,",
          "  $4::DATE",
          ")",
          "ORDER by date ASC;"
        ),
        params = list(as_of, tsid, start_dt, end_dt)
      )
      yes <- TRUE
    },
    error = function(e) {
      message(
        "Cannot access measurements_calculated_daily_at function: ",
        e$message
      )
    }
  )

  if (!yes) {
    skip(
      "Connection cannot access measurements_calculated_daily_at function, which is required for historical queries."
    )
  }

  out <- plotMultiTimeseries(
    timeseries_id = tsid,
    start_date = start_dt,
    end_date = end_dt,
    resolution = "hour",
    historic_range = TRUE,
    tzone = "UTC",
    data = TRUE,
    con = con,
    as_of = as_of
  )$data[[1]]

  expect_gt(nrow(out$trace_data), 48)
})
