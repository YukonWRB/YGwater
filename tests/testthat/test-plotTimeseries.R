# Note: these tests depend on installation of Python and a few libraries. This is taken care of in the setup.R file within the testthat folder.

test_con <- test_AquaConnect(silent = TRUE)
on.exit(DBI::dbDisconnect(test_con), add = TRUE)


wlevel <- DBI::dbGetQuery(
  test_con,
  "SELECT parameter_id FROM public.parameters WHERE param_name = 'water level';"
)$parameter_id[[1]]

# Find the first water level timeseries in the DB
wlevel_ts <- DBI::dbGetQuery(
  test_con,
  paste0(
    "SELECT location_id, parameter_id, timeseries_id, EXTRACT(EPOCH FROM ts.record_rate) AS record_rate, aggregation_type_id, start_datetime, end_datetime FROM continuous.timeseries ts WHERE parameter_id = ",
    wlevel,
    " LIMIT 1;"
  )
)

test_that("timeseries plot is as expected for one year with no historic range or slider", {
  skip_on_cran()
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test1.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotTimeseries(
    location = wlevel_ts$location_id[1],
    parameter = "water level",
    record_rate = wlevel_ts$record_rate[1],
    aggregation_type = wlevel_ts$aggregation_type_id[1],
    start_date = as.character(lubridate::date(wlevel_ts$end_datetime[1]) - 30),
    end_date = as.character(lubridate::date(wlevel_ts$end_datetime[1])),
    historic_range = FALSE,
    slider = FALSE,
    con = test_con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("timeseries plot is as expected for one year with no historic range", {
  skip_on_cran()
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test2.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotTimeseries(
    location = wlevel_ts$location_id[1],
    parameter = "water level",
    record_rate = wlevel_ts$record_rate[1],
    aggregation_type = wlevel_ts$aggregation_type_id[1],
    start_date = as.character(lubridate::date(wlevel_ts$end_datetime[1]) - 365),
    end_date = as.character(lubridate::date(wlevel_ts$end_datetime[1])),
    historic_range = FALSE,
    con = test_con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("timeseries plot is as expected for one year with historic range", {
  skip_on_cran()
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test3.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotTimeseries(
    location = wlevel_ts$location_id[1],
    parameter = "water level",
    record_rate = wlevel_ts$record_rate[1],
    aggregation_type = wlevel_ts$aggregation_type_id[1],
    start_date = as.character(lubridate::date(wlevel_ts$end_datetime[1]) - 365),
    end_date = as.character(lubridate::date(wlevel_ts$end_datetime[1])),
    con = test_con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("French timeseries plot is as expected for one year with historic range and slider", {
  skip_on_cran()
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test4.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotTimeseries(
    location = wlevel_ts$location_id[1],
    parameter = "water level",
    record_rate = wlevel_ts$record_rate[1],
    aggregation_type = wlevel_ts$aggregation_type_id[1],
    start_date = as.character(lubridate::date(wlevel_ts$end_datetime[1]) - 365),
    end_date = as.character(lubridate::date(wlevel_ts$end_datetime[1])),
    lang = "fr",
    con = test_con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("French timeseries plot is as expected for one year with historic range and no slider", {
  skip_on_cran()
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test5.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotTimeseries(
    location = wlevel_ts$location_id[1],
    parameter = "water level",
    record_rate = wlevel_ts$record_rate[1],
    aggregation_type = wlevel_ts$aggregation_type_id[1],
    start_date = as.character(lubridate::date(wlevel_ts$end_datetime[1]) - 365),
    end_date = as.character(lubridate::date(wlevel_ts$end_datetime[1])),
    lang = "fr",
    slider = FALSE,
    con = test_con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("grades, approvals, qualifiers are displayed", {
  skip_on_cran()
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test6.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotTimeseries(
    timeseries_id = 1,
    start_date = as.character(lubridate::date(wlevel_ts$end_datetime[1]) - 365),
    end_date = as.character(lubridate::date(wlevel_ts$end_datetime[1])),
    lang = "fr",
    slider = FALSE,
    grades = TRUE,
    qualifiers = TRUE,
    approvals = TRUE,
    con = test_con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("one of grades, approvals, qualifiers is displayed", {
  skip_on_cran()
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test7.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plot <- plotTimeseries(
    timeseries_id = 1,
    start_date = as.character(lubridate::date(wlevel_ts$end_datetime[1]) - 365),
    end_date = as.character(lubridate::date(wlevel_ts$end_datetime[1])),
    lang = "fr",
    slider = FALSE,
    grades = TRUE,
    con = test_con
  )
  plotly::save_image(plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("returned plot data is as expected", {
  skip_on_cran()

  plot <- plotTimeseries(
    timeseries_id = 1,
    start_date = as.character(lubridate::date(wlevel_ts$end_datetime[1]) - 30),
    end_date = as.character(lubridate::date(wlevel_ts$end_datetime[1])),
    lang = "fr",
    slider = FALSE,
    data = TRUE,
    con = test_con
  )$data
  expect_type(plot, "list")
  expect_named(plot, c("trace_data", "range_data"))
  expect_named(plot$trace_data, c("datetime", "value", "imputed"))
  expect_named(plot$range_data, c("datetime", "min", "max", "q75", "q25"))
})


test_that("plotTimeseries works when given only a timeseries_id", {
  skip_on_cran()

  plot <- plotTimeseries(
    timeseries_id = wlevel_ts$timeseries_id[1],
    start_date = as.character(lubridate::date(wlevel_ts$end_datetime[1]) - 30),
    end_date = as.character(lubridate::date(wlevel_ts$end_datetime[1])),
    historic_range = TRUE,
    slider = FALSE,
    data = TRUE,
    stats_period = "30yr",
    con = test_con
  )
  expect_s3_class(plot$plot, "plotly")
  expect_named(plot$data, c("trace_data", "range_data"))
  expect_named(plot$data$trace_data, c("datetime", "value", "imputed"))
  expect_named(plot$data$range_data, c("datetime", "min", "max", "q75", "q25"))

  # Skip snapshot test on CI
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test8.png")
  path <- pathPrep(path)
  on.exit(unlink(path), add = TRUE)

  plotly::save_image(plot$plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

# Test that plotTimeseries plots raw and uncorrected data
test_that("plotTimeseries plots raw and corrected data", {
  skip_on_cran()

  plot <- plotTimeseries(
    timeseries_id = wlevel_ts$timeseries_id[1],
    start_date = as.character(lubridate::date(wlevel_ts$end_datetime[1]) - 30),
    end_date = as.character(lubridate::date(wlevel_ts$end_datetime[1])),
    historic_range = TRUE,
    slider = FALSE,
    data = TRUE,
    raw = TRUE,
    con = test_con
  )
  expect_s3_class(plot$plot, "plotly")
  expect_named(plot$data, c("trace_data", "range_data"))
  expect_named(
    plot$data$trace_data,
    c("datetime", "value", "value_raw", "imputed")
  )
  expect_named(plot$data$range_data, c("datetime", "min", "max", "q75", "q25"))

  # Skip snapshot test on CI
  skip_on_ci()

  dir <- file.path(tempdir(), "plotly_tests")
  unlink(dir, recursive = TRUE, force = TRUE)
  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "test9.png")
  path <- pathPrep(path)

  on.exit(unlink(path), add = TRUE)
  plotly::save_image(plot$plot, file = path, width = 500, height = 500)

  expect_snapshot_file(path)
})

test_that("plotTimeseries hourly resolution uses timeseries aggregation logic", {
  skip_on_cran()
  tsid <- wlevel_ts$timeseries_id[1]

  start_dt <- wlevel_ts$end_datetime[1] - lubridate::days(30)
  end_dt <- wlevel_ts$end_datetime[1]

  agg_type <- DBI::dbGetQuery(
    test_con,
    paste0(
      "SELECT at.aggregation_type FROM continuous.timeseries ts ",
      "LEFT JOIN continuous.aggregation_types at ",
      "ON ts.aggregation_type_id = at.aggregation_type_id ",
      "WHERE ts.timeseries_id = $1;"
    ),
    params = list(tsid)
  )[1, 1]

  source <- dbGetQueryDT(
    test_con,
    paste0(
      "SELECT datetime, value_corrected, imputed ",
      "FROM continuous.measurements_continuous_corrected($1, $2, $3) ",
      "ORDER BY datetime;"
    ),
    params = list(tsid, start_dt, end_dt)
  )

  source[,
    datetime_hour := as.POSIXct(
      format(datetime, "%Y-%m-%d %H:00:00", tz = "UTC"),
      tz = "UTC"
    )
  ]
  expected <- source[,
    .(
      value = if (agg_type == "sum") {
        sum(value_corrected, na.rm = TRUE)
      } else if (agg_type == "median") {
        stats::median(value_corrected, na.rm = TRUE)
      } else if (agg_type %in% c("min", "minimum")) {
        min(value_corrected, na.rm = TRUE)
      } else if (agg_type %in% c("max", "maximum")) {
        max(value_corrected, na.rm = TRUE)
      } else if (agg_type == "(min+max)/2") {
        mean(c(
          min(value_corrected, na.rm = TRUE),
          max(value_corrected, na.rm = TRUE)
        ))
      } else {
        mean(value_corrected, na.rm = TRUE)
      },
      imputed = any(imputed)
    ),
    by = datetime_hour
  ]
  data.table::setnames(expected, "datetime_hour", "datetime")
  data.table::setorder(expected, datetime)

  out <- plotTimeseries(
    timeseries_id = tsid,
    start_date = start_dt,
    end_date = end_dt,
    resolution = "hour",
    tzone = "UTC",
    slider = FALSE,
    historic_range = FALSE,
    data = TRUE,
    con = test_con
  )$data$trace_data

  out <- out[
    lubridate::minute(datetime) == 0 & lubridate::second(datetime) == 0,
    .(datetime, value, imputed)
  ]
  data.table::setorder(out, datetime)

  expect_equal(out$datetime, expected$datetime)
  expect_equal(out$value, expected$value, tolerance = 1e-8)
  expect_equal(out$imputed, expected$imputed)
})

test_that("plotTimeseries can show data in the past", {
  skip_on_ci() # Because the CI instance would not have the necessary historical data
  skip_on_cran()

  con <- AquaConnect() # Connect to regular DB so that we can work with historical data
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (
    !isTRUE(DBI::dbGetQuery(
      con,
      "SELECT has_schema_privilege(current_user, 'audit', 'USAGE') AS ok;"
    )$ok[[1]])
  ) {
    skip("Historical queries require USAGE on schema audit.")
  }

  wl <- DBI::dbGetQuery(
    con,
    "SELECT parameter_id FROM public.parameters WHERE param_name = 'water level' LIMIT 1;"
  )[1, 1]
  loc_id <- DBI::dbGetQuery(
    con,
    "SELECT location_id FROM public.locations WHERE location_code = '09EA004';"
  )[1, 1]
  tsid <- DBI::dbGetQuery(
    con,
    "SELECT timeseries_id FROM continuous.timeseries WHERE parameter_id = $1 AND location_id = $2 LIMIT 1;",
    params = list(wl, loc_id)
  )[1, 1]

  as_of <- as.POSIXct("2026-03-30 12:00:00", tz = "UTC") # Has to be after implementation of patch_38, otherwise the old AquaCache logic was deleting rows and replacing, resulting in recent created_on timestamps - this made it appear as if there was nothing in the past.
  start_dt <- as.POSIXct("2022-06-01 00:00:00", tz = "UTC")
  end_dt <- as.POSIXct("2022-06-02 23:59:59", tz = "UTC")

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

  out_max <- plotTimeseries(
    timeseries_id = tsid,
    start_date = start_dt,
    end_date = end_dt,
    as_of = as_of,
    resolution = "max",
    tzone = "UTC",
    slider = FALSE,
    historic_range = FALSE,
    raw = TRUE,
    data = TRUE,
    con = con
  )$data$trace_data

  expect_gt(nrow(out_max), 570)

  out_day <- plotTimeseries(
    timeseries_id = tsid,
    start_date = start_dt,
    end_date = end_dt,
    as_of = as_of,
    resolution = "day",
    tzone = "UTC",
    slider = FALSE,
    historic_range = TRUE,
    data = TRUE,
    con = con
  )$data

  expect_gt(nrow(out_day), 1)
})
