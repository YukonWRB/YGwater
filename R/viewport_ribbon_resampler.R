# Viewport-aware resampling helpers for ribbon plots
#
# These helpers keep the full-resolution data on the server and reduce the
# number of points sent to the browser based on the current x-axis viewport.
# They are intended for Shiny + plotly integrations where line traces and
# uncertainty bands need to stay responsive at very large row counts.

#' Calculate target number of bins for viewport resampling based on viewport width
#' @keywords internal
#' @noRd

viewport_ribbon_target_bins <- function(
  width_px = NULL,
  density = 1,
  min_bins = 120L,
  max_bins = 1500L,
  fallback = 700L
) {
  if (is.null(width_px) || !is.finite(width_px) || width_px <= 0) {
    return(as.integer(fallback))
  }

  bins <- as.integer(round(width_px * density))
  bins <- max(as.integer(min_bins), bins)
  bins <- min(as.integer(max_bins), bins)
  bins
}

#' Convert various x-axis inputs to POSIXct for viewport calculations
#' @keywords internal
#' @noRd
viewport_ribbon_as_posixct <- function(x, tz = "UTC") {
  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(x, tz = tz))
  }

  if (is.numeric(x)) {
    origin_x <- if (max(abs(x), na.rm = TRUE) > 1e11) x / 1000 else x
    return(as.POSIXct(origin_x, origin = "1970-01-01", tz = tz))
  }

  as.POSIXct(x, tz = tz)
}

#' Extract x-axis limits from plotly relayout data for viewport resampling
#' @keywords internal
#' @noRd
viewport_ribbon_relayout_xlim <- function(relayout, tz = "UTC") {
  if (is.null(relayout) || length(relayout) == 0) {
    return(NULL)
  }

  autorange_keys <- grep(
    "^xaxis[0-9]*\\.autorange$",
    names(relayout),
    value = TRUE
  )
  if (
    length(autorange_keys) > 0 &&
      any(vapply(
        autorange_keys,
        function(key) isTRUE(relayout[[key]]),
        logical(1)
      ))
  ) {
    return(NULL)
  }

  range0_keys <- grep(
    "^xaxis[0-9]*\\.range\\[0\\]$",
    names(relayout),
    value = TRUE
  )
  range1_keys <- grep(
    "^xaxis[0-9]*\\.range\\[1\\]$",
    names(relayout),
    value = TRUE
  )

  if (length(range0_keys) == 0 || length(range1_keys) == 0) {
    return(NULL)
  }

  axis_prefix <- sub("\\.range\\[0\\]$", "", range0_keys[[1]])
  range1_key <- paste0(axis_prefix, ".range[1]")
  if (!(range1_key %in% range1_keys)) {
    range1_key <- range1_keys[[1]]
  }

  x0 <- relayout[[range0_keys[[1]]]]
  x1 <- relayout[[range1_key]]

  if (is.null(x0) || is.null(x1)) {
    return(NULL)
  }

  out <- c(
    viewport_ribbon_as_posixct(x0, tz = tz),
    viewport_ribbon_as_posixct(x1, tz = tz)
  )

  if (any(is.na(out))) {
    return(NULL)
  }

  sort(out)
}


#' Resample line and band data for a given x-axis viewport
#' @keywords internal
#' @noRd
viewport_ribbon_resample <- function(
  data,
  x_col = "x",
  line_col = "y",
  bands = NULL,
  xlim = NULL,
  n_bins = 700L,
  pad_fraction = 0.02,
  gap_multiplier = 20
) {
  dt <- data.table::as.data.table(data)

  if (!nrow(dt)) {
    return(list(
      line = data.table::data.table(),
      bands = list(),
      meta = list(
        total_rows = 0L,
        window_rows = 0L,
        visible_rows = 0L,
        line_rows = 0L,
        band_rows = 0L,
        n_bins = 0L,
        x_window = NULL,
        x_visible = NULL
      )
    ))
  }

  x_all_num <- as.numeric(dt[[x_col]])
  if (!any(is.finite(x_all_num))) {
    stop("`data` must contain finite x values.")
  }
  if (is.unsorted(x_all_num, na.rm = TRUE)) {
    dt <- data.table::copy(dt)
    data.table::setorderv(dt, x_col)
    x_all_num <- as.numeric(dt[[x_col]])
  }

  xlim_num <- NULL
  window_rows <- nrow(dt)
  visible <- NULL
  lower_bound <- function(x, value) {
    lo <- 1L
    hi <- length(x) + 1L
    while (lo < hi) {
      mid <- floor((lo + hi) / 2L)
      if (x[[mid]] < value) {
        lo <- mid + 1L
      } else {
        hi <- mid
      }
    }
    lo
  }
  upper_bound <- function(x, value) {
    lo <- 1L
    hi <- length(x) + 1L
    while (lo < hi) {
      mid <- floor((lo + hi) / 2L)
      if (x[[mid]] <= value) {
        lo <- mid + 1L
      } else {
        hi <- mid
      }
    }
    lo - 1L
  }

  if (!is.null(xlim) && length(xlim) == 2 && all(!is.na(xlim))) {
    xlim_num <- sort(as.numeric(xlim))
    window_first_idx <- lower_bound(x_all_num, xlim_num[1])
    window_last_idx <- upper_bound(x_all_num, xlim_num[2])
    window_rows <- max(0L, window_last_idx - window_first_idx + 1L)

    span <- diff(xlim_num)
    if (!is.finite(span) || span <= 0) {
      span <- diff(range(x_all_num, na.rm = TRUE))
      if (!is.finite(span) || span <= 0) {
        span <- 1
      }
    }

    pad <- span * pad_fraction
    keep_first_idx <- lower_bound(x_all_num, xlim_num[1] - pad)
    keep_last_idx <- upper_bound(x_all_num, xlim_num[2] + pad)

    if (keep_first_idx <= keep_last_idx) {
      first_idx <- max(1L, keep_first_idx - 1L)
      last_idx <- min(nrow(dt), keep_last_idx + 1L)
      visible <- data.table::copy(dt[first_idx:last_idx])
      visible[, .row_id := first_idx:last_idx]
    } else {
      visible <- data.table::copy(dt[0])
      visible[, .row_id := integer(0)]
    }
  }

  if (is.null(visible)) {
    visible <- data.table::copy(dt)
    visible[, .row_id := .I]
  }

  if (!nrow(visible)) {
    if (!is.null(xlim_num)) {
      return(list(
        line = data.table::data.table(
          x = dt[[x_col]][0],
          y = numeric(0),
          .run = integer(0),
          .bin = integer(0),
          .row_id = integer(0)
        ),
        bands = stats::setNames(
          vector("list", length(if (is.null(bands)) list() else bands)),
          names(bands)
        ),
        meta = list(
          total_rows = nrow(dt),
          window_rows = window_rows,
          visible_rows = 0L,
          line_rows = 0L,
          band_rows = 0L,
          n_bins = 0L,
          x_window = sort(xlim),
          x_visible = NULL
        )
      ))
    }

    visible <- dt
  }

  visible_x_num <- as.numeric(visible[[x_col]])
  visible_diffs <- diff(visible_x_num)
  visible_diffs <- visible_diffs[is.finite(visible_diffs) & visible_diffs > 0]
  raw_gap_threshold <- if (length(visible_diffs) > 0) {
    stats::median(visible_diffs) * gap_multiplier
  } else {
    Inf
  }

  if (!is.finite(raw_gap_threshold) || raw_gap_threshold <= 0) {
    raw_gap_threshold <- Inf
  }

  visible[, .run := 1L]
  if (nrow(visible) > 1) {
    visible[,
      .run := cumsum(c(
        1L,
        diff(as.numeric(get(x_col))) > raw_gap_threshold
      ))
    ]
  }

  span_visible <- diff(range(visible_x_num, na.rm = TRUE))
  if (!is.finite(span_visible) || span_visible <= 0) {
    visible[, .bin := 1L]
  } else {
    n_bins <- max(1L, as.integer(n_bins))
    bin_width <- span_visible / n_bins
    x_min <- min(visible_x_num, na.rm = TRUE)
    visible[,
      .bin := pmin(
        n_bins,
        pmax(
          1L,
          as.integer(floor((as.numeric(get(x_col)) - x_min) / bin_width)) + 1L
        )
      )
    ]
  }

  line_dt <- data.table::data.table(
    x = dt[[x_col]][0],
    y = numeric(0),
    .run = integer(0),
    .bin = integer(0),
    .row_id = integer(0)
  )
  if (!is.null(line_col)) {
    if (!(line_col %in% names(visible))) {
      stop("`line_col` was not found in `data`.")
    }

    line_visible <- data.table::copy(visible)
    if (nrow(line_visible) > 1) {
      line_visible[,
        .line_run := cumsum(c(
          1L,
          diff(.run) != 0L |
            head(is.na(get(line_col)), -1L)
        ))
      ]
    } else {
      line_visible[, .line_run := 1L]
    }

    line_dt <- line_visible[
      !is.na(get(line_col)),
      {
        local <- .SD
        idx <- unique(c(
          1L,
          which.min(local[[line_col]]),
          which.max(local[[line_col]]),
          .N
        ))
        local[idx]
      },
      by = .(.run = .line_run, .bin),
      .SDcols = c(x_col, line_col, ".row_id")
    ]

    if (nrow(line_dt) > 0) {
      data.table::setorderv(line_dt, c(".row_id"))
      line_dt <- unique(line_dt, by = ".row_id")
      data.table::setnames(
        line_dt,
        old = c(x_col, line_col),
        new = c("x", "y")
      )
    }
  }

  band_list <- list()
  band_row_count <- 0L
  if (!is.null(bands) && length(bands) > 0) {
    for (band_name in names(bands)) {
      cols <- bands[[band_name]]
      if (length(cols) != 2 || !all(cols %in% names(visible))) {
        next
      }

      lower_col <- cols[[1]]
      upper_col <- cols[[2]]

      band_dt <- visible[
        !is.na(get(lower_col)) & !is.na(get(upper_col)),
        .(
          x = get(x_col)[ceiling(.N / 2)],
          ymin = min(get(lower_col), na.rm = TRUE),
          ymax = max(get(upper_col), na.rm = TRUE),
          n_raw = .N
        ),
        by = .(.run, .bin)
      ]

      if (nrow(band_dt) > 0) {
        data.table::setorderv(band_dt, c(".run", ".bin"))
      }

      band_row_count <- band_row_count + nrow(band_dt)
      band_list[[band_name]] <- band_dt
    }
  }

  list(
    line = line_dt,
    bands = band_list,
    meta = list(
      total_rows = nrow(dt),
      window_rows = window_rows,
      visible_rows = nrow(visible),
      line_rows = nrow(line_dt),
      band_rows = band_row_count,
      n_bins = if (nrow(visible) > 0) max(visible$.bin) else 0L,
      x_window = if (is.null(xlim)) range(dt[[x_col]]) else sort(xlim),
      x_visible = range(visible[[x_col]])
    )
  )
}


#' Generate plotly trace bundle for line and band data from viewport resampling
#' @keywords internal
#' @noRd
viewport_ribbon_trace_bundle <- function(
  summary,
  line_name = "Observed",
  line_color = "#00454e",
  line_width = 1.4,
  band_styles = NULL,
  hover = TRUE
) {
  if (is.null(band_styles)) {
    band_styles <- list(
      Historic = list(
        fillcolor = "rgba(212, 236, 239, 0.85)",
        line = list(color = "rgba(212, 236, 239, 1)", width = 0.3)
      ),
      Typical = list(
        fillcolor = "rgba(95, 157, 166, 0.45)",
        line = list(color = "rgba(95, 157, 166, 0.85)", width = 0.3)
      )
    )
  }

  traces <- list()
  client_points <- 0L

  for (band_name in names(summary$bands)) {
    band_dt <- summary$bands[[band_name]]
    if (is.null(band_dt)) {
      next
    }
    band_dt <- data.table::as.data.table(band_dt)
    if (
      nrow(band_dt) == 0L ||
        !all(c("x", "ymin", "ymax", ".run") %in% names(band_dt))
    ) {
      next
    }

    style <- band_styles[[band_name]]
    if (is.null(style)) {
      style <- list(
        fillcolor = "rgba(160, 160, 160, 0.30)",
        line = list(color = "rgba(120, 120, 120, 0.75)", width = 0.3)
      )
    }

    band_runs <- split(band_dt, by = ".run", keep.by = FALSE)
    showlegend <- TRUE

    for (seg in band_runs) {
      x_poly <- c(seg$x, rev(seg$x))
      y_poly <- c(seg$ymin, rev(seg$ymax))

      traces[[length(traces) + 1L]] <- list(
        x = x_poly,
        y = y_poly,
        type = "scatter",
        mode = "lines",
        fill = "toself",
        hoveron = "fills",
        fillcolor = style$fillcolor,
        line = style$line,
        name = band_name,
        legendgroup = band_name,
        showlegend = showlegend,
        hoverinfo = "skip",
        hovertemplate = "<extra></extra>"
      )

      client_points <- client_points + length(x_poly)
      showlegend <- FALSE
    }
  }

  if (nrow(summary$line) > 0) {
    line_runs <- split(summary$line, by = ".run", keep.by = FALSE)
    showlegend <- TRUE

    for (seg in line_runs) {
      traces[[length(traces) + 1L]] <- list(
        x = seg$x,
        y = seg$y,
        type = "scatter",
        mode = "lines",
        line = list(color = line_color, width = line_width),
        name = line_name,
        legendgroup = line_name,
        showlegend = showlegend,
        hoverinfo = if (hover) "text" else "skip",
        text = paste0(
          line_name,
          "<br>",
          format(seg$x, usetz = TRUE),
          "<br>Value: ",
          signif(seg$y, 5)
        )
      )

      client_points <- client_points + length(seg$x)
      showlegend <- FALSE
    }
  }

  list(
    traces = traces,
    trace_count = length(traces),
    client_points = client_points
  )
}

#' Generate plotly trace bundle for line and band data from viewport resampling
#' and create a plotly plot with appropriate layout and configuration for viewport-aware ribbon plots
#' @keywords internal
#' @noRd
viewport_ribbon_plot <- function(
  summary,
  title = NULL,
  source = NULL,
  line_name = "Observed",
  line_color = "#00454e",
  line_width = 1.4,
  band_styles = NULL,
  hover = TRUE,
  yaxis_title = NULL,
  xaxis_title = NULL
) {
  trace_bundle <- viewport_ribbon_trace_bundle(
    summary = summary,
    line_name = line_name,
    line_color = line_color,
    line_width = line_width,
    band_styles = band_styles,
    hover = hover
  )

  p <- plotly::plot_ly(source = source)

  for (trace in trace_bundle$traces) {
    p <- do.call(
      plotly::add_trace,
      c(list(p = p), trace)
    )
  }

  p <- p |>
    plotly::layout(
      title = title,
      xaxis = list(
        title = xaxis_title,
        showline = TRUE,
        zeroline = FALSE
      ),
      yaxis = list(
        title = yaxis_title,
        showline = TRUE,
        zeroline = FALSE
      ),
      hovermode = if (hover) "closest" else FALSE,
      legend = list(orientation = "h"),
      font = list(family = "Nunito Sans")
    ) |>
    plotly::config(displayModeBar = TRUE)
  p <- viewport_event_register(p)

  list(
    plot = p,
    trace_bundle = trace_bundle
  )
}

#' Generate plotly trace bundle for line and band data from viewport resampling and create a plotly plot with appropriate layout and configuration for viewport-aware ribbon plots, using raw input data and resampling within the function, with additional helper functions for axis naming and event registration
#' @keywords internal
#' @noRd
viewport_timeseries_plot <- function(
  trace_data,
  range_data = NULL,
  meta,
  source = NULL,
  xlim = NULL,
  n_bins = 700L,
  legend_orientation = NULL
) {
  trace_dt <- data.table::copy(data.table::as.data.table(trace_data))
  if (!nrow(trace_dt)) {
    stop("`trace_data` must contain at least one row.")
  }
  if (!all(c("datetime", "value") %in% names(trace_dt))) {
    stop("`trace_data` must contain `datetime` and `value` columns.")
  }

  range_dt <- data.table::copy(data.table::as.data.table(range_data))
  if (!all(c("datetime", "min", "max", "q25", "q75") %in% names(range_dt))) {
    range_dt <- data.table::data.table()
  }

  hover <- isTRUE(meta$hover)
  line_name <- if (!is.null(meta$line_name)) {
    meta$line_name
  } else {
    "Observed"
  }
  line_color <- if (!is.null(meta$line_color)) {
    meta$line_color
  } else {
    "#00454e"
  }
  line_width <- if (!is.null(meta$line_width)) {
    meta$line_width
  } else {
    1.4
  }

  line_summary <- viewport_ribbon_resample(
    data = trace_dt,
    x_col = "datetime",
    line_col = "value",
    xlim = xlim,
    n_bins = n_bins
  )
  line_bundle <- viewport_ribbon_trace_bundle(
    summary = line_summary,
    line_name = line_name,
    line_color = line_color,
    line_width = line_width,
    band_styles = list(),
    hover = hover
  )

  band_summary <- list(
    line = data.table::data.table(),
    bands = list(),
    meta = NULL
  )
  band_bundle <- list(
    traces = list(),
    trace_count = 0L,
    client_points = 0L
  )

  if (nrow(range_dt) > 0) {
    band_names <- meta$band_names
    if (is.null(band_names)) {
      band_names <- list(
        historic = "Historic",
        typical = "Typical"
      )
    }

    band_defs <- list()
    band_defs[[band_names$historic]] <- c("min", "max")
    band_defs[[band_names$typical]] <- c("q25", "q75")

    band_summary <- viewport_ribbon_resample(
      data = range_dt,
      x_col = "datetime",
      line_col = NULL,
      bands = band_defs,
      xlim = xlim,
      n_bins = n_bins
    )
    band_bundle <- viewport_ribbon_trace_bundle(
      summary = band_summary,
      line_name = NULL,
      band_styles = meta$band_styles,
      hover = hover
    )
  }

  traces <- c(band_bundle$traces, line_bundle$traces)

  p <- plotly::plot_ly(source = source)
  for (trace in traces) {
    p <- do.call(
      plotly::add_trace,
      c(list(p = p), trace)
    )
  }

  layout_args <- meta$layout
  if (is.null(layout_args)) {
    layout_args <- list()
  }
  if (!is.null(legend_orientation)) {
    if (is.null(layout_args$legend)) {
      layout_args$legend <- list()
    }
    layout_args$legend$orientation <- legend_orientation
  }

  if (length(layout_args) > 0) {
    p <- do.call(
      plotly::layout,
      c(list(p = p), layout_args)
    )
  }

  config_args <- meta$config
  if (is.null(config_args)) {
    config_args <- list()
  }
  p <- do.call(
    plotly::config,
    c(list(p = p), config_args)
  )
  p <- viewport_event_register(p)

  list(
    plot = p,
    trace_bundle = list(
      traces = traces,
      trace_count = length(traces),
      client_points = band_bundle$client_points + line_bundle$client_points
    ),
    summaries = list(
      line = line_summary,
      bands = band_summary
    )
  )
}

#' Safe helper for generating plotly axis references based on index, returning base prefix for index 1 or NULL/NA, and appending index for higher values, used for dynamic axis assignment in viewport-aware ribbon plots
#' @keywords internal
#' @noRd
viewport_axis_ref <- function(prefix, index) {
  if (is.null(index) || is.na(index) || index <= 1) {
    return(prefix)
  }

  paste0(prefix, as.integer(index))
}

#' Safe helper for generating plotly axis names based on index, returning base prefix for index 1 or NULL/NA, and appending index for higher values, used for dynamic axis assignment in viewport-aware ribbon plots
#' @keywords internal
#' @noRd
viewport_layout_axis_name <- function(prefix, index) {
  if (is.null(index) || is.na(index) || index <= 1) {
    return(paste0(prefix, "axis"))
  }

  paste0(prefix, "axis", as.integer(index))
}

#' Helper for applying x-axis and y-axis references to plotly trace definitions, used for dynamic axis assignment in viewport-aware ribbon plots
#' @keywords internal
#' @noRd
viewport_trace_apply_axes <- function(trace, xaxis = NULL, yaxis = NULL) {
  if (!is.null(xaxis)) {
    trace$xaxis <- xaxis
  }
  if (!is.null(yaxis)) {
    trace$yaxis <- yaxis
  }
  trace
}

#' Helper for registering plotly events on a plot, used for enabling viewport-aware resampling in Shiny + plotly integrations
#' @keywords internal
#' @noRd
viewport_event_register <- function(plot, events = "plotly_relayout") {
  for (event in events) {
    plot <- plotly::event_register(plot, event)
  }
  plot
}

#' Generate plotly trace bundle for status bands based on input data, with optional x-axis limits for filtering and clipping, used for adding status bands to viewport-aware ribbon plots
#' @keywords internal
#' @noRd
viewport_status_band_trace_bundle <- function(status_bands, xlim = NULL) {
  datetime <- y <- id <- starts_before_end <- ends_after_start <- NULL
  empty_bundle <- list(
    traces = list(),
    trace_count = 0L,
    client_points = 0L
  )

  if (is.null(status_bands)) {
    return(empty_bundle)
  }

  if (is.null(status_bands$polygons) && length(status_bands) > 0) {
    bundles <- lapply(
      status_bands,
      viewport_status_band_trace_bundle,
      xlim = xlim
    )
    traces <- unlist(lapply(bundles, `[[`, "traces"), recursive = FALSE)
    return(list(
      traces = traces,
      trace_count = length(traces),
      client_points = sum(vapply(
        bundles,
        function(bundle) bundle$client_points,
        integer(1)
      ))
    ))
  }

  if (is.null(status_bands$polygons)) {
    return(empty_bundle)
  }

  polygons <- data.table::copy(data.table::as.data.table(
    status_bands$polygons
  ))
  required_cols <- c("datetime", "y", "color", "text", "id")
  if (!nrow(polygons) || !all(required_cols %in% names(polygons))) {
    return(empty_bundle)
  }

  polygons[, datetime := viewport_ribbon_as_posixct(datetime, tz = "UTC")]
  polygons <- polygons[!is.na(datetime) & !is.na(y)]
  if (!nrow(polygons)) {
    return(empty_bundle)
  }

  if (!is.null(xlim) && length(xlim) == 2 && all(!is.na(xlim))) {
    xlim_num <- sort(as.numeric(xlim))
    keep_ids <- polygons[,
      .(
        starts_before_end = min(as.numeric(datetime), na.rm = TRUE) <=
          xlim_num[2],
        ends_after_start = max(as.numeric(datetime), na.rm = TRUE) >=
          xlim_num[1]
      ),
      by = id
    ][starts_before_end & ends_after_start, id]
    polygons <- polygons[id %in% keep_ids]
    if (!nrow(polygons)) {
      return(empty_bundle)
    }
    polygons[,
      datetime := as.POSIXct(
        pmin(pmax(as.numeric(datetime), xlim_num[1]), xlim_num[2]),
        origin = "1970-01-01",
        tz = "UTC"
      )
    ]
  }

  xaxis <- if (!is.null(status_bands$xaxis)) status_bands$xaxis else "x"
  yaxis <- if (!is.null(status_bands$yaxis)) status_bands$yaxis else "y2"
  hover <- if (!is.null(status_bands$hover)) {
    isTRUE(status_bands$hover)
  } else {
    TRUE
  }

  traces <- list()
  client_points <- 0L
  for (status_id in unique(polygons$id)) {
    seg <- polygons[id == status_id]
    if (nrow(seg) < 3) {
      next
    }
    status_text <- as.character(seg$text[[1]])

    traces[[length(traces) + 1L]] <- list(
      x = seg$datetime,
      y = seg$y,
      type = "scatter",
      mode = "lines",
      fill = "toself",
      hoveron = "fills",
      fillcolor = as.character(seg$color[[1]]),
      line = list(width = 1, color = "black"),
      name = status_text,
      legendgroup = status_text,
      hoverinfo = if (hover) "text" else "skip",
      hovertemplate = if (hover) "%{text}<extra></extra>" else NULL,
      text = rep(status_text, nrow(seg)),
      showlegend = FALSE,
      xaxis = xaxis,
      yaxis = yaxis
    )

    client_points <- client_points + nrow(seg)
  }

  list(
    traces = traces,
    trace_count = length(traces),
    client_points = client_points
  )
}

#' Add status band traces and corresponding layout adjustments to a plotly layout, based on input status band definitions and optional axis naming, used for integrating status bands into viewport-aware ribbon plots
#' @keywords internal
#' @noRd
viewport_layout_add_status_bands <- function(
  layout,
  status_bands,
  main_xaxis_name = "xaxis",
  main_yaxis_name = "yaxis",
  status_xaxis_name = NULL,
  status_yaxis_name = "yaxis2"
) {
  if (is.null(status_bands)) {
    return(layout)
  }

  if (is.null(layout)) {
    layout <- list()
  }

  status_height <- status_bands$height
  if (
    is.null(status_height) ||
      length(status_height) != 1 ||
      is.na(status_height)
  ) {
    status_height <- 0.04
  }
  status_height <- min(0.20, max(0.02, as.numeric(status_height)))

  if (is.null(layout[[main_xaxis_name]])) {
    layout[[main_xaxis_name]] <- list()
  }
  if (is.null(layout[[main_yaxis_name]])) {
    layout[[main_yaxis_name]] <- list()
  }

  main_xaxis <- layout[[main_xaxis_name]]
  main_yaxis <- layout[[main_yaxis_name]]
  main_xaxis_ref <- sub("axis", "", main_xaxis_name, fixed = TRUE)
  main_yaxis_ref <- sub("axis", "", main_yaxis_name, fixed = TRUE)
  status_xaxis_ref <- if (!is.null(status_xaxis_name)) {
    sub("axis", "", status_xaxis_name, fixed = TRUE)
  } else {
    main_xaxis_ref
  }
  status_yaxis_ref <- sub("axis", "", status_yaxis_name, fixed = TRUE)
  panel_domain <- main_yaxis$domain
  if (
    is.null(panel_domain) ||
      length(panel_domain) != 2 ||
      any(is.na(panel_domain))
  ) {
    panel_domain <- c(0, 1)
  }
  panel_domain <- sort(as.numeric(panel_domain))
  panel_span <- diff(panel_domain)
  status_domain_height <- panel_span * status_height
  status_domain_top <- panel_domain[1] + status_domain_height

  original_showticklabels <- main_xaxis$showticklabels
  if (is.null(original_showticklabels)) {
    original_showticklabels <- TRUE
  }

  main_yaxis$domain <- c(status_domain_top, panel_domain[2])
  main_yaxis$anchor <- main_xaxis_ref
  layout[[main_yaxis_name]] <- main_yaxis

  if (is.null(main_xaxis$type)) {
    main_xaxis$type <- "date"
  }
  if (!is.null(main_xaxis$rangeslider)) {
    main_xaxis$rangeslider$visible <- FALSE
  }

  if (!is.null(status_xaxis_name)) {
    main_xaxis$anchor <- main_yaxis_ref
    main_xaxis$showticklabels <- FALSE
    layout[[main_xaxis_name]] <- main_xaxis

    status_xaxis <- main_xaxis
    status_xaxis$anchor <- status_yaxis_ref
    status_xaxis$matches <- main_xaxis_ref
    status_xaxis$showticklabels <- original_showticklabels
    status_xaxis$showgrid <- FALSE
    status_xaxis$type <- "date"
    status_xaxis$fixedrange <- FALSE
    status_xaxis$title <- list(standoff = 0)
    if (!is.null(status_xaxis$rangeslider)) {
      status_xaxis$rangeslider$visible <- FALSE
    }
    layout[[status_xaxis_name]] <- status_xaxis
  } else {
    main_xaxis$anchor <- status_yaxis_ref
    main_xaxis$showticklabels <- original_showticklabels
    layout[[main_xaxis_name]] <- main_xaxis
  }

  y_range <- status_bands$y_range
  if (is.null(y_range) || length(y_range) != 2 || any(is.na(y_range))) {
    y_range <- c(0, 1)
  }

  layout[[status_yaxis_name]] <- list(
    domain = c(panel_domain[1], status_domain_top),
    anchor = status_xaxis_ref,
    range = y_range,
    fixedrange = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE,
    zeroline = FALSE
  )

  annotations <- status_bands$annotations
  if (!is.null(annotations) && length(annotations) > 0) {
    layout$annotations <- c(layout$annotations, annotations)
  }

  layout
}

#' Apply x-axis limits to plotly layout for specified axis names, used for ensuring consistent axis ranges in viewport-aware ribbon plots when x-axis limits are updated
#' @keywords internal
#' @noRd
viewport_layout_apply_xlim <- function(
  layout,
  xlim = NULL,
  xaxis_names = NULL
) {
  if (is.null(xlim) || length(xlim) != 2 || any(is.na(xlim))) {
    return(layout)
  }

  if (is.null(xaxis_names) || length(xaxis_names) == 0) {
    xaxis_names <- "xaxis"
  }

  for (axis_name in xaxis_names) {
    if (is.null(layout[[axis_name]])) {
      layout[[axis_name]] <- list()
    }
    layout[[axis_name]]$range <- xlim
  }

  layout
}

#' Generate plotly trace bundle for line and band data from viewport resampling and create a plotly plot with appropriate layout and configuration for viewport-aware ribbon plots, using raw input data and resampling within the function, with additional helper functions for axis naming and event registration, and support for multiple series with individual metadata and trace definitions, used for creating complex viewport-aware ribbon plots with multiple series and status bands in Shiny + plotly integrations
#' @keywords internal
#' @noRd
viewport_adaptive_plot <- function(
  payload,
  source = NULL,
  xlim = NULL,
  n_bins = 700L,
  legend_orientation = NULL
) {
  series <- payload$series
  if (is.null(series) || length(series) == 0) {
    stop("`payload$series` must contain at least one series.")
  }

  traces <- list()
  summaries <- vector("list", length(series))
  client_points <- 0L

  for (i in seq_along(series)) {
    item <- series[[i]]
    trace_dt <- data.table::as.data.table(item$trace_data)
    range_dt <- data.table::as.data.table(item$range_data)
    meta <- item$meta
    if (is.null(meta)) {
      meta <- list()
    }
    hover <- if (is.null(meta$hover)) TRUE else isTRUE(meta$hover)

    x_col <- if (!is.null(item$x_col)) item$x_col else "datetime"
    y_col <- if (!is.null(item$y_col)) item$y_col else "value"
    range_x_col <- if (!is.null(item$range_x_col)) {
      item$range_x_col
    } else {
      x_col
    }

    line_bundle <- list(
      traces = list(),
      trace_count = 0L,
      client_points = 0L
    )
    line_summary <- NULL
    if (
      nrow(trace_dt) > 0 &&
        all(c(x_col, y_col) %in% names(trace_dt))
    ) {
      line_summary <- viewport_ribbon_resample(
        data = trace_dt,
        x_col = x_col,
        line_col = y_col,
        xlim = xlim,
        n_bins = n_bins
      )
      line_bundle <- viewport_ribbon_trace_bundle(
        summary = line_summary,
        line_name = if (!is.null(item$line_name)) {
          item$line_name
        } else if (!is.null(meta$line_name)) {
          meta$line_name
        } else {
          paste("Series", i)
        },
        line_color = if (!is.null(item$line_color)) {
          item$line_color
        } else if (!is.null(meta$line_color)) {
          meta$line_color
        } else {
          "#00454e"
        },
        line_width = if (!is.null(item$line_width)) {
          item$line_width
        } else if (!is.null(meta$line_width)) {
          meta$line_width
        } else {
          1.4
        },
        band_styles = list(),
        hover = hover
      )
    }

    band_bundle <- list(
      traces = list(),
      trace_count = 0L,
      client_points = 0L
    )
    band_summary <- NULL
    if (
      nrow(range_dt) > 0 &&
        all(c(range_x_col, "min", "max", "q25", "q75") %in% names(range_dt))
    ) {
      band_names <- meta$band_names
      if (is.null(band_names)) {
        band_names <- list(historic = "Historic", typical = "Typical")
      }

      band_defs <- list()
      band_defs[[band_names$historic]] <- c("min", "max")
      band_defs[[band_names$typical]] <- c("q25", "q75")

      band_summary <- viewport_ribbon_resample(
        data = range_dt,
        x_col = range_x_col,
        line_col = NULL,
        bands = band_defs,
        xlim = xlim,
        n_bins = n_bins
      )
      band_bundle <- viewport_ribbon_trace_bundle(
        summary = band_summary,
        line_name = NULL,
        band_styles = meta$band_styles,
        hover = hover
      )
    }

    xaxis <- item$xaxis
    yaxis <- item$yaxis
    item_traces <- c(band_bundle$traces, line_bundle$traces)
    item_traces <- lapply(
      item_traces,
      viewport_trace_apply_axes,
      xaxis = xaxis,
      yaxis = yaxis
    )

    traces <- c(traces, item_traces)
    client_points <- client_points +
      band_bundle$client_points +
      line_bundle$client_points
    summaries[[i]] <- list(line = line_summary, bands = band_summary)
  }

  status_bundle <- viewport_status_band_trace_bundle(
    payload$status_bands,
    xlim = xlim
  )
  if (status_bundle$trace_count > 0) {
    traces <- c(traces, status_bundle$traces)
    client_points <- client_points + status_bundle$client_points
  }

  layout_args <- payload$layout
  if (is.null(layout_args)) {
    layout_args <- list()
  }
  if (!is.null(legend_orientation)) {
    if (is.null(layout_args$legend)) {
      layout_args$legend <- list()
    }
    layout_args$legend$orientation <- legend_orientation
  }
  layout_args <- viewport_layout_apply_xlim(
    layout_args,
    xlim = xlim,
    xaxis_names = payload$xaxis_names
  )

  p <- plotly::plot_ly(source = source)
  for (trace in traces) {
    p <- do.call(plotly::add_trace, c(list(p = p), trace))
  }
  if (length(layout_args) > 0) {
    p <- do.call(plotly::layout, c(list(p = p), layout_args))
  }

  config_args <- payload$config
  if (is.null(config_args)) {
    config_args <- list()
  }
  p <- do.call(plotly::config, c(list(p = p), config_args))
  p <- viewport_event_register(p)

  list(
    plot = p,
    trace_bundle = list(
      traces = traces,
      trace_count = length(traces),
      client_points = client_points
    ),
    summaries = summaries
  )
}
