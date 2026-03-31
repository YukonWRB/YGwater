#' Viewport-aware resampling helpers for ribbon plots
#'
#' These helpers keep the full-resolution data on the server and reduce the
#' number of points sent to the browser based on the current x-axis viewport.
#' They are intended for Shiny + plotly integrations where line traces and
#' uncertainty bands need to stay responsive at very large row counts.
#'
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


viewport_ribbon_relayout_xlim <- function(relayout, tz = "UTC") {
  if (is.null(relayout) || length(relayout) == 0) {
    return(NULL)
  }

  if (isTRUE(relayout[["xaxis.autorange"]])) {
    return(NULL)
  }

  x0 <- relayout[["xaxis.range[0]"]]
  x1 <- relayout[["xaxis.range[1]"]]

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
  dt <- data.table::copy(data.table::as.data.table(data))

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

  data.table::setorderv(dt, x_col)
  dt[, .row_id := .I]

  x_all_num <- as.numeric(dt[[x_col]])
  if (!any(is.finite(x_all_num))) {
    stop("`data` must contain finite x values.")
  }

  xlim_num <- NULL
  window_rows <- nrow(dt)
  visible <- dt

  if (!is.null(xlim) && length(xlim) == 2 && all(!is.na(xlim))) {
    xlim_num <- sort(as.numeric(xlim))
    window_rows <- sum(
      x_all_num >= xlim_num[1] & x_all_num <= xlim_num[2],
      na.rm = TRUE
    )

    span <- diff(xlim_num)
    if (!is.finite(span) || span <= 0) {
      span <- diff(range(x_all_num, na.rm = TRUE))
      if (!is.finite(span) || span <= 0) {
        span <- 1
      }
    }

    pad <- span * pad_fraction
    keep <- x_all_num >= (xlim_num[1] - pad) & x_all_num <= (xlim_num[2] + pad)

    if (any(keep)) {
      keep_idx <- which(keep)
      first_idx <- max(1L, keep_idx[1] - 1L)
      last_idx <- min(nrow(dt), keep_idx[length(keep_idx)] + 1L)
      visible <- dt[first_idx:last_idx]
    } else {
      visible <- dt[0]
    }
  }

  if (!nrow(visible)) {
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
    visible[, .run := cumsum(c(
      1L,
      diff(as.numeric(get(x_col))) > raw_gap_threshold
    ))]
  }

  span_visible <- diff(range(visible_x_num, na.rm = TRUE))
  if (!is.finite(span_visible) || span_visible <= 0) {
    visible[, .bin := 1L]
  } else {
    n_bins <- max(1L, as.integer(n_bins))
    bin_width <- span_visible / n_bins
    x_min <- min(visible_x_num, na.rm = TRUE)
    visible[, .bin := pmin(
      n_bins,
      pmax(
        1L,
        as.integer(floor((as.numeric(get(x_col)) - x_min) / bin_width)) + 1L
      )
    )]
  }

  line_dt <- visible[!is.na(get(line_col)), {
    local <- .SD
    idx <- unique(c(
      1L,
      which.min(local[[line_col]]),
      which.max(local[[line_col]]),
      .N
    ))
    local[idx]
  },
  by = .(.run, .bin),
  .SDcols = c(x_col, line_col, ".row_id")]

  if (nrow(line_dt) > 0) {
    data.table::setorderv(line_dt, c(".row_id"))
    line_dt <- unique(line_dt, by = ".row_id")
    data.table::setnames(
      line_dt,
      old = c(x_col, line_col),
      new = c("x", "y")
    )
  } else {
    line_dt <- data.table::data.table(
      x = dt[[x_col]][0],
      y = numeric(0),
      .run = integer(0),
      .bin = integer(0),
      .row_id = integer(0)
    )
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
    if (!nrow(band_dt)) {
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
      hover_text <- paste0(
        band_name,
        "<br>",
        format(seg$x, usetz = TRUE),
        "<br>Lower: ",
        signif(seg$ymin, 5),
        "<br>Upper: ",
        signif(seg$ymax, 5),
        "<br>Rows aggregated: ",
        prettyNum(seg$n_raw, big.mark = ",")
      )

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
        hoverinfo = if (hover) "text" else "skip",
        text = c(hover_text, rev(hover_text))
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

  list(
    plot = p,
    trace_bundle = trace_bundle
  )
}
