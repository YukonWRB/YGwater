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


test_that("viewport ribbon resampling supports ribbon-only summaries", {
  x <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + seq_len(2400) * 3600
  dt <- data.table::data.table(
    datetime = x,
    min = sin(seq_len(2400) / 30) - 1,
    max = sin(seq_len(2400) / 30) + 1,
    q25 = sin(seq_len(2400) / 30) - 0.3,
    q75 = sin(seq_len(2400) / 30) + 0.3
  )

  out <- YGwater:::viewport_ribbon_resample(
    data = dt,
    x_col = "datetime",
    line_col = NULL,
    bands = list(
      Historic = c("min", "max"),
      Typical = c("q25", "q75")
    ),
    n_bins = 180L
  )

  expect_equal(out$meta$line_rows, 0L)
  expect_named(out$bands, c("Historic", "Typical"))
  expect_gt(nrow(out$bands$Historic), 0)
})


test_that("viewport timeseries plot builds a compact line plus ribbon plot", {
  x <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + seq_len(12000) * 60
  trace_dt <- data.table::data.table(
    datetime = x,
    value = sin(seq_len(12000) / 80) + ifelse(seq_len(12000) %% 907 == 0, 1, 0)
  )
  range_dt <- data.table::data.table(
    datetime = seq(min(x), max(x), by = "day"),
    min = -1.5,
    max = 1.5,
    q25 = -0.5,
    q75 = 0.5
  )

  built <- YGwater:::viewport_timeseries_plot(
    trace_data = trace_dt,
    range_data = range_dt,
    meta = list(
      hover = TRUE,
      line_name = "Signal",
      line_color = "#00454e",
      line_width = 2,
      band_names = list(historic = "Historic", typical = "Typical"),
      band_styles = list(
        Historic = list(
          fillcolor = "rgba(212, 236, 239, 0.85)",
          line = list(color = "rgba(212, 236, 239, 1)", width = 0.2)
        ),
        Typical = list(
          fillcolor = "rgba(95, 157, 166, 0.45)",
          line = list(color = "rgba(95, 157, 166, 0.85)", width = 0.2)
        )
      ),
      layout = list(
        title = list(text = "Adaptive"),
        xaxis = list(showline = TRUE),
        yaxis = list(showline = TRUE),
        legend = list(orientation = "v")
      ),
      config = list(locale = "en")
    ),
    n_bins = 220L
  )

  expect_s3_class(built$plot, "plotly")
  expect_gt(built$trace_bundle$trace_count, 0)
  expect_gt(built$trace_bundle$client_points, 0)
})

test_that("viewport adaptive plot includes status bands", {
  x <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + seq_len(5000) * 60
  trace_dt <- data.table::data.table(
    datetime = x,
    value = sin(seq_len(5000) / 80)
  )
  range_dt <- data.table::data.table()
  status_bands <- list(
    polygons = data.table::data.table(
      id = rep("grade_1", 4),
      datetime = as.POSIXct(
        c(
          "2024-01-01 06:00:00",
          "2024-01-01 06:00:00",
          "2024-01-02 06:00:00",
          "2024-01-02 06:00:00"
        ),
        tz = "UTC"
      ),
      y = c(0, 1, 1, 0),
      color = "#4daf4a",
      text = "Grade: A"
    ),
    annotations = list(list(
      x = 0,
      y = 0.5,
      xref = "paper",
      yref = "y2",
      text = "Grade",
      showarrow = FALSE
    )),
    height = 0.02,
    y_range = c(0, 1),
    xaxis = "x2",
    yaxis = "y2"
  )
  layout <- YGwater:::viewport_layout_add_status_bands(
    list(xaxis = list(), yaxis = list()),
    status_bands,
    status_xaxis_name = "xaxis2"
  )
  expect_equal(layout$yaxis2$domain, c(0, 0.02))
  expect_equal(layout$xaxis$type, "date")
  expect_false(layout$xaxis$showticklabels)
  expect_equal(layout$xaxis2$type, "date")
  expect_equal(layout$xaxis2$anchor, "y2")
  expect_true(layout$xaxis2$fixedrange)

  built <- YGwater:::viewport_adaptive_plot(
    payload = list(
      series = list(list(
        trace_data = trace_dt,
        range_data = range_dt,
        meta = list(hover = TRUE),
        xaxis = "x",
        yaxis = "y"
      )),
      layout = layout,
      config = list(locale = "en"),
      xaxis_names = c("xaxis", "xaxis2"),
      status_bands = status_bands
    ),
    xlim = range(x),
    n_bins = 120L
  )

  expect_s3_class(built$plot, "plotly")
  expect_true(any(vapply(
    built$trace_bundle$traces,
    function(trace) identical(trace$yaxis, "y2"),
    logical(1)
  )))
  expect_true(any(vapply(
    built$trace_bundle$traces,
    function(trace) identical(trace$xaxis, "x2") && identical(trace$yaxis, "y2"),
    logical(1)
  )))
  status_trace <- Filter(
    function(trace) identical(trace$xaxis, "x2") && identical(trace$yaxis, "y2"),
    built$trace_bundle$traces
  )[[1]]
  expect_equal(status_trace$name, "Grade: A")
  expect_equal(status_trace$hovertemplate, "%{text}<extra></extra>")
  expect_equal(plotly::plotly_build(built$plot)$x$layout$xaxis2$type, "date")
})

test_that("viewport status bands are clipped to the visible x window", {
  xlim <- as.POSIXct(
    c("2024-01-01 00:00:00", "2024-01-02 00:00:00"),
    tz = "UTC"
  )
  status_bands <- list(
    polygons = data.table::data.table(
      id = rep("grade_1", 4),
      datetime = as.POSIXct(
        c(
          "2023-12-01 00:00:00",
          "2023-12-01 00:00:00",
          "2024-02-01 00:00:00",
          "2024-02-01 00:00:00"
        ),
        tz = "UTC"
      ),
      y = c(0, 1, 1, 0),
      color = "#4daf4a",
      text = "Grade: A"
    ),
    xaxis = "x",
    yaxis = "y2"
  )

  bundle <- YGwater:::viewport_status_band_trace_bundle(
    status_bands,
    xlim = xlim
  )

  expect_equal(bundle$trace_count, 1L)
  expect_equal(range(bundle$traces[[1]]$x), xlim)
})
