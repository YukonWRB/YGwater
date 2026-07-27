test_that("date-only plot range bounds include the selected start day", {
  tzone <- "Etc/GMT+7"

  start <- normalize_plot_datetime_bound(
    as.Date("2024-01-01"),
    tzone,
    bound = "start"
  )
  end <- normalize_plot_datetime_bound(
    as.Date("2024-01-02"),
    tzone,
    bound = "end"
  )

  expect_equal(
    format(start, "%Y-%m-%d %H:%M:%S", tz = tzone, usetz = FALSE),
    "2024-01-01 00:00:00"
  )
  expect_equal(
    format(end, "%Y-%m-%d %H:%M:%S", tz = tzone, usetz = FALSE),
    "2024-01-03 00:00:00"
  )
  expect_equal(as.numeric(difftime(end, start, units = "days")), 2)
})

test_that("datetime plot range bounds keep explicit times", {
  tzone <- "Etc/GMT+7"

  start <- normalize_plot_datetime_bound(
    "2024-01-01 12:30:00",
    tzone,
    bound = "start"
  )
  end <- normalize_plot_datetime_bound(
    as.POSIXct("2024-01-02 18:45:00", tz = tzone),
    tzone,
    bound = "end"
  )

  expect_equal(
    format(start, "%Y-%m-%d %H:%M:%S", tz = tzone, usetz = FALSE),
    "2024-01-01 12:30:00"
  )
  expect_equal(
    format(end, "%Y-%m-%d %H:%M:%S", tz = tzone, usetz = FALSE),
    "2024-01-02 18:45:00"
  )
})
