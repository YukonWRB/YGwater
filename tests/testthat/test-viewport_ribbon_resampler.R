test_that("viewport ribbon resampling reduces rows while preserving gaps", {
  x <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + seq_len(12000) * 60
  dt <- data.table::data.table(
    x = x,
    y = sin(seq_len(12000) / 120) + ifelse(seq_len(12000) %% 911 == 0, 1, 0),
    lo = sin(seq_len(12000) / 120) - 0.5,
    hi = sin(seq_len(12000) / 120) + 0.5
  )
  dt <- dt[-(5000:5600)]

  out <- YGwater:::viewport_ribbon_resample(
    data = dt,
    x_col = "x",
    line_col = "y",
    bands = list(Band = c("lo", "hi")),
    n_bins = 300L
  )

  expect_equal(out$meta$total_rows, nrow(dt))
  expect_lt(out$meta$line_rows, out$meta$visible_rows)
  expect_true("Band" %in% names(out$bands))
  expect_gt(length(unique(out$line$.run)), 1)
})


test_that("viewport ribbon trace bundle returns plotly-ready traces", {
  x <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + seq_len(8000) * 60
  dt <- data.table::data.table(
    x = x,
    y = cos(seq_len(8000) / 80),
    lo = cos(seq_len(8000) / 80) - 0.25,
    hi = cos(seq_len(8000) / 80) + 0.25
  )

  summary <- YGwater:::viewport_ribbon_resample(
    data = dt,
    x_col = "x",
    line_col = "y",
    bands = list(Band = c("lo", "hi")),
    xlim = range(dt$x)[1] + c(0, 60 * 60 * 24),
    n_bins = 200L
  )

  bundle <- YGwater:::viewport_ribbon_trace_bundle(summary)

  expect_gt(bundle$trace_count, 0)
  expect_gt(bundle$client_points, 0)
  expect_true(any(vapply(bundle$traces, function(x) identical(x$fill, "toself"), logical(1))))
  expect_true(any(vapply(bundle$traces, function(x) identical(x$name, "Observed"), logical(1))))
})
