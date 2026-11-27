# This module pairs a station photo viewer with a hydrograph. Users pick a
# hydrometric station that has an image series; photo times are marked along
# the hydrograph and clicking either plot shows the photo closest in time. A
# second plot below the hydrograph shows stage, discharge, air temperature,
# precipitation or degree days from a nearby station, with its x-axis linked
# to the hydrograph (www/js/linked_plots.js).

imgTsViewUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(tags$script(src = "js/linked_plots.js")),
    tags$style(HTML(sprintf(
      "
      /* Compact inputs in card headers */
      #%s .card-header .form-group { margin-bottom: 0; }
      #%s .card-header .selectize-input { padding-top: .2rem; padding-bottom: .2rem; }
      /* Keep the chosen item on one line (longer in French); the dropdown
         still shows the full text */
      #%s .card-header .selectize-input {
        display: flex !important; align-items: center; flex-wrap: nowrap; overflow: hidden;
      }
      #%s .card-header .selectize-input > .item {
        display: block; min-width: 0; white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
      }
      #%s .card-header .selectize-input > input { flex: none; }
      /* Switch label stays on one line next to the date range */
      #%s .form-switch .form-check-label { white-space: nowrap; }
      ",
      ns("page"),
      ns("page"),
      ns("page"),
      ns("page"),
      ns("page"),
      ns("page")
    ))),
    # Banner notification
    uiOutput(ns("banner")),
    # All UI elements are rendered in the server function to allow multi-language functionality
    uiOutput(ns("page"), fill = TRUE)
  )
}

imgTsView <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # The app replaces session$userData$AquaCache on login/logout, so `con`
    # always resolves to the current connection (as in the flood dashboard).
    # Reactive reads also subscribe to the connection version, so queries
    # re-run on the new connection instead of using a closed one.
    connection_version <- session$userData$AquaCache_version
    makeActiveBinding(
      "con",
      function() {
        tryCatch(
          connection_version(),
          error = function(e) isolate(connection_version())
        )
        session$userData$AquaCache
      },
      environment()
    )

    # Times are shown in fixed UTC-7, like the rest of the image modules
    tz <- "Etc/GMT+7"
    # Above this many days the plots use daily means instead of raw data
    daily_threshold_days <- 180
    # Only offer secondary-plot stations within this distance of the station
    nearby_max_km <- 150
    # Degree-sign unit label, built at runtime to keep this file ASCII
    deg_c <- paste0(intToUtf8(176L), "C")
    # Plot colours, also used for the station markers on the map
    hydro_colour <- "#244C5A"
    secondary_colour <- "#0097A9"

    # Semi-transparent version of a hex colour, for climate-normal bands
    with_alpha <- function(hex, alpha) {
      rgb <- grDevices::col2rgb(hex)
      sprintf("rgba(%d,%d,%d,%s)", rgb[1L], rgb[2L], rgb[3L], alpha)
    }

    # Helpers ##################################################################

    format_time <- function(x) {
      format(x, "%Y-%m-%d %H:%M", tz = tz)
    }

    # Plotly gets local-time strings so the axis shows times in `tz`
    plot_time <- function(x) {
      format(x, "%Y-%m-%d %H:%M:%S", tz = tz)
    }

    haversine_km <- function(lat1, lon1, lat2, lon2) {
      rad <- pi / 180
      a <- sin((lat2 - lat1) * rad / 2)^2 +
        cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
      2 * 6371 * asin(pmin(1, sqrt(a)))
    }

    # Parameter name in the current language, with units when known
    param_label <- function(row) {
      name <- row[[tr("param_name_col", language$language)]][[1L]]
      units <- row$units[[1L]]
      if (is.na(units) || !nzchar(units)) {
        name
      } else {
        paste0(name, " (", units, ")")
      }
    }

    # Hydrograph value at the given times, used to place photo markers on the line
    value_at <- function(hd, times) {
      if (length(times) == 0L) {
        return(numeric())
      }
      stats::approx(
        as.numeric(hd$datetime),
        hd$value,
        xout = as.numeric(times),
        rule = 2,
        ties = mean
      )$y
    }

    # x/y of the highlight marker for the photo at `idx` (NA when none)
    highlight_xy <- function(idx, ph, hd) {
      if (is.null(idx) || idx > nrow(ph)) {
        return(list(x = NA_character_, y = NA_real_))
      }
      list(
        x = plot_time(ph$datetime[[idx]]),
        y = value_at(hd, ph$datetime[[idx]])
      )
    }

    # Data loaded once per module ##############################################

    unit_sql <- YGwater::ac_parameter_unit_select_sql(con, "p", "units")

    # One timeseries per station and parameter (water level / flow), preferring
    # the most recently updated, restricted to stations with an image series.
    stations <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT DISTINCT ON (l.location_id, p.parameter_id)
           l.location_id,
           l.location_code,
           l.name,
           l.name_fr,
           l.latitude,
           l.longitude,
           p.param_name,
           p.param_name_fr,
           %s,
           ts.timeseries_id
         FROM public.locations l
         JOIN continuous.timeseries ts
           ON ts.location_id = l.location_id
         JOIN public.parameters p
           ON p.parameter_id = ts.parameter_id
         WHERE p.param_name IN ('water level', 'water flow')
           AND EXISTS (
             SELECT 1 FROM files.image_series s
             WHERE s.location_id = l.location_id
           )
         ORDER BY l.location_id, p.parameter_id,
           ts.end_datetime DESC NULLS LAST, ts.timeseries_id",
        unit_sql
      )
    )

    # Candidate series for the secondary plot, from any station: stage and
    # discharge from hydrometric stations, and (since hydrometric stations
    # rarely record weather) temperature, precipitation and degree days from
    # met stations. One timeseries per station and parameter: daily totals for
    # precipitation and mean-type aggregations (not daily min/max) for air
    # temperature.
    nearby_series <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT DISTINCT ON (l.location_id, p.param_name)
           l.location_id,
           l.location_code,
           l.name,
           l.name_fr,
           l.latitude,
           l.longitude,
           p.param_name,
           p.param_name_fr,
           %s,
           ts.timeseries_id,
           ts.end_datetime
         FROM continuous.timeseries ts
         JOIN public.locations l
           ON l.location_id = ts.location_id
         JOIN public.parameters p
           ON p.parameter_id = ts.parameter_id
         JOIN continuous.aggregation_types a
           ON a.aggregation_type_id = ts.aggregation_type_id
         WHERE l.latitude IS NOT NULL
           AND l.longitude IS NOT NULL
           AND (
             p.param_name IN ('water level', 'water flow')
             OR (p.param_name = 'precipitation, total' AND a.aggregation_type = 'sum')
             OR (p.param_name = 'temperature, air'
               AND a.aggregation_type IN ('instantaneous', '(min+max)/2', 'mean', 'average'))
           )
         ORDER BY l.location_id, p.param_name,
           ts.end_datetime DESC NULLS LAST, ts.timeseries_id",
        unit_sql
      )
    )
    attr(nearby_series$end_datetime, "tzone") <- tz

    # Secondary-plot variables: label translation key and source parameter
    secondary_vars <- list(
      stage = list(key = "img_ts_stage", param = "water level"),
      discharge = list(key = "img_ts_discharge", param = "water flow"),
      temperature = list(key = "img_ts_air_temp", param = "temperature, air"),
      precip = list(key = "precipitation", param = "precipitation, total"),
      cfdd = list(key = "img_ts_cfdd", param = "temperature, air"),
      cddt = list(key = "img_ts_cddt", param = "temperature, air")
    )

    # UI #######################################################################

    output$banner <- renderUI({
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = con,
        module_id = "imgTsView"
      )
    })

    # Re-rendered on language change; current selections are kept
    output$page <- renderUI({
      lang <- language$language
      name_col <- tr("generic_name_col", lang)
      station_choices <- unique(stations[, c(
        "location_id",
        "location_code",
        name_col
      )])
      station_choices <- station_choices[order(station_choices$location_code), ]

      date_range <- isolate(input$date_range)
      if (is.null(date_range)) {
        date_range <- c(Sys.Date() - 30, Sys.Date())
      }

      page_fillable(
        padding = 0,
        gap = "1rem",
        layout_columns(
          col_widths = c(4, 3, 3, 2),
          fill = FALSE,
          selectizeInput(
            ns("location"),
            label = tr("img_ts_station_lab", lang),
            choices = stats::setNames(
              station_choices$location_id,
              paste0(
                station_choices$location_code,
                " - ",
                station_choices[[name_col]]
              )
            ),
            selected = isolate(input$location),
            width = "100%"
          ),
          selectizeInput(
            ns("parameter"),
            label = tr("parameter", lang),
            choices = NULL,
            width = "100%"
          ),
          dateRangeInput(
            ns("date_range"),
            label = tr("date_range_lab", lang),
            start = date_range[[1L]],
            end = date_range[[2L]],
            format = "yyyy-mm-dd",
            language = language$abbrev,
            separator = tr("date_sep", lang),
            width = "100%"
          ),
          div(
            class = "d-flex align-items-end h-100 pb-3",
            input_switch(
              ns("normals"),
              label = tr("img_ts_normals", lang),
              value = isTRUE(isolate(input$normals))
            )
          )
        ),
        # Photo viewer and station map (left), linked plots (right)
        layout_columns(
          col_widths = c(5, 7),
          min_height = "600px",
          layout_columns(
            col_widths = 12,
            row_heights = c(3, 1.4),
            card(
              full_screen = TRUE,
              card_header(uiOutput(ns("photo_header"))),
              card_body(
                class = "d-flex align-items-center justify-content-center",
                uiOutput(ns("photo"))
              ),
              card_footer(
                # Centred so the card's full-screen button (bottom right) stays clear
                div(
                  class = "d-flex justify-content-center align-items-center gap-3",
                  actionButton(
                    ns("prev_photo"),
                    label = NULL,
                    icon = icon("chevron-left"),
                    class = "btn-sm"
                  ),
                  textOutput(ns("photo_count"), inline = TRUE),
                  actionButton(
                    ns("next_photo"),
                    label = NULL,
                    icon = icon("chevron-right"),
                    class = "btn-sm"
                  )
                )
              )
            ),
            # Where the hydrograph and lower-plot stations are, in plot colours
            card(
              full_screen = TRUE,
              card_body(
                padding = 0,
                leaflet::leafletOutput(ns("station_map"), height = "100%")
              )
            )
          ),
          layout_columns(
            col_widths = 12,
            row_heights = c(3, 2),
            card(
              full_screen = TRUE,
              card_header(
                tr("img_ts_hydrograph", lang),
                tags$small(
                  class = "text-muted ms-2",
                  tr("img_ts_click_hint", lang)
                )
              ),
              # Plain plotlyOutput: spinner wrappers stop it filling the card.
              # The app's busy indicator shows while plots recalculate.
              card_body(
                padding = c(0, 8, 8, 0),
                plotly::plotlyOutput(ns("hydrograph"), height = "100%")
              )
            ),
            card(
              full_screen = TRUE,
              card_header(
                class = "d-flex align-items-center gap-2 py-1",
                div(
                  style = "width: 12rem; flex: none;",
                  selectizeInput(
                    ns("secondary"),
                    label = NULL,
                    choices = stats::setNames(
                      names(secondary_vars),
                      vapply(
                        secondary_vars,
                        function(v) tr(v$key, lang),
                        character(1L)
                      )
                    ),
                    selected = if (is.null(isolate(input$secondary))) {
                      "precip"
                    } else {
                      isolate(input$secondary)
                    },
                    width = "100%"
                  )
                ),
                div(
                  style = "flex: 1; min-width: 0;",
                  selectizeInput(
                    ns("series_station"),
                    label = NULL,
                    choices = NULL,
                    width = "100%"
                  )
                )
              ),
              card_body(
                padding = c(0, 8, 8, 0),
                plotly::plotlyOutput(ns("secondary_plot"), height = "100%")
              )
            )
          )
        )
      )
    }) %>%
      bindEvent(language$language)

    # Station, parameter and date window #######################################

    selected_station <- reactive({
      req(input$location)
      st <- stations[stations$location_id == as.integer(input$location), ]
      req(nrow(st) > 0L)
      st
    })

    # Parameter choices for the station; re-run on language change so the
    # re-rendered input gets (translated) choices again
    observe({
      st <- selected_station()
      lang <- language$language
      # Default to water level when both are available
      st <- st[order(st$param_name != "water level"), ]
      current <- isolate(input$parameter)
      labels <- vapply(
        seq_len(nrow(st)),
        function(i) param_label(st[i, ]),
        character(1L)
      )
      updateSelectizeInput(
        session,
        "parameter",
        choices = stats::setNames(st$timeseries_id, labels),
        selected = if (!is.null(current) && current %in% st$timeseries_id) {
          current
        } else {
          st$timeseries_id[[1L]]
        }
      )
    })

    date_window <- reactive({
      req(input$date_range)
      validate(need(
        !anyNA(input$date_range) &&
          input$date_range[[2L]] >= input$date_range[[1L]],
        tr("img_ts_invalid_dates", language$language)
      ))
      list(
        from_date = input$date_range[[1L]],
        to_date = input$date_range[[2L]],
        start = as.POSIXct(format(input$date_range[[1L]]), tz = tz),
        end = as.POSIXct(format(input$date_range[[2L]] + 1L), tz = tz),
        days = as.numeric(diff(input$date_range)) + 1
      )
    })

    # Secondary-plot station choices ###########################################

    # Stations with the needed parameter near the selected station, nearest
    # first (for stage and discharge the selected station itself, at 0 km)
    nearby_stations <- reactive({
      req(input$secondary)
      st <- selected_station()
      cand <- nearby_series[
        nearby_series$param_name == secondary_vars[[input$secondary]]$param,
      ]
      cand$km <- haversine_km(
        st$latitude[[1L]],
        st$longitude[[1L]],
        cand$latitude,
        cand$longitude
      )
      cand <- cand[!is.na(cand$km) & cand$km <= nearby_max_km, ]
      cand[order(cand$km), ]
    })

    # Station + variable the list was last built for: a new station or
    # variable resets the choice, while a date or language change keeps it
    last_station_key <- NULL

    # Default to the nearest station with data reaching into the window
    observe({
      cand <- nearby_stations()
      win <- date_window()
      lang <- language$language
      key <- paste(selected_station()$location_id[[1L]], input$secondary)
      labels <- sprintf(
        "%s - %s (%.0f km, %s %s)",
        cand$location_code,
        cand[[tr("generic_name_col", lang)]],
        cand$km,
        tr("img_ts_data_to", lang),
        ifelse(
          is.na(cand$end_datetime),
          "?",
          format(cand$end_datetime, "%Y-%m-%d", tz = tz)
        )
      )
      current <- isolate(input$series_station)
      keep_current <- identical(key, last_station_key) &&
        !is.null(current) &&
        current %in% cand$timeseries_id
      last_station_key <<- key
      has_recent <- !is.na(cand$end_datetime) & cand$end_datetime >= win$start
      selected <- if (keep_current) {
        current
      } else if (any(has_recent)) {
        cand$timeseries_id[which(has_recent)[[1L]]]
      } else if (nrow(cand) > 0L) {
        cand$timeseries_id[[1L]]
      }
      updateSelectizeInput(
        session,
        "series_station",
        choices = stats::setNames(cand$timeseries_id, labels),
        selected = selected
      )
    })

    # Data #####################################################################

    # Local-noon datetimes for dates (format() keeps zero rows as zero rows;
    # paste() would yield " 12:00:00")
    noon <- function(dates) {
      as.POSIXct(format(dates), tz = tz) + 12 * 60 * 60
    }

    # Daily values (dated at local noon) for dates from_date to to_date inclusive
    fetch_daily <- function(tsid, from_date, to_date) {
      out <- DBI::dbGetQuery(
        con,
        "SELECT date, value
         FROM continuous.measurements_calculated_daily
         WHERE timeseries_id = $1 AND date >= $2 AND date <= $3
         ORDER BY date",
        params = list(tsid, from_date, to_date)
      )
      out$date <- as.Date(out$date)
      out$datetime <- noon(out$date)
      out
    }

    # Climate normals #########################################################

    # Normals are worked out here from daily values by day of year, rather than
    # taken from the per-date statistics in measurements_calculated_daily:
    # those only exist on dates the current year has data, and are often
    # incomplete (NA percentiles, min = max) or out of order, which made the
    # bands break up, pinch and cross.
    normal_years <- 30 # Years of record before the window used for normals
    normal_min_years <- 5 # Fewest years of data a day of year needs
    normal_max_gap <- 7 # Longest run of missing days filled by interpolation

    # Day of year on a 365-day calendar (Feb 29 shares Feb 28's normals)
    noleap_doy <- function(dates) {
      j <- as.integer(format(dates, "%j"))
      y <- as.integer(format(dates, "%Y"))
      leap <- (y %% 4L == 0L & y %% 100L != 0L) | y %% 400L == 0L
      ifelse(leap & j > 59L, j - 1L, j)
    }

    # Fill runs of up to `max_gap` missing days by linear interpolation,
    # wrapping around the year end; longer runs stay NA (drawn as breaks)
    fill_short_gaps <- function(x, max_gap) {
      ok <- !is.na(x)
      if (sum(ok) < 2L || all(ok)) {
        return(x)
      }
      n <- length(x)
      pos <- c(which(ok) - n, which(ok), which(ok) + n)
      filled <- stats::approx(pos, rep(x[ok], 3L), xout = seq_len(n))$y
      runs <- rle(!ok)
      run_end <- cumsum(runs$lengths)
      run_start <- run_end - runs$lengths + 1L
      long <- runs$values & runs$lengths > max_gap
      # A run touching both ends continues across the year end
      if (!ok[[1L]] && !ok[[n]] && length(runs$lengths) > 1L) {
        wrap <- runs$lengths[[1L]] + runs$lengths[[length(runs$lengths)]]
        if (wrap > max_gap) {
          long[c(1L, length(long))] <- TRUE
        }
      }
      for (i in which(long)) {
        filled[run_start[[i]]:run_end[[i]]] <- NA_real_
      }
      filled
    }

    # Normal statistics for each day of the year (rows 1-365) from the
    # `normal_years` years before `before_date`. Cached for the session.
    normals_cache <- new.env(parent = emptyenv())
    doy_normals <- function(tsid, before_date) {
      key <- paste(tsid, before_date)
      if (!is.null(normals_cache[[key]])) {
        return(normals_cache[[key]])
      }
      hist <- DBI::dbGetQuery(
        con,
        "SELECT date, value
         FROM continuous.measurements_calculated_daily
         WHERE timeseries_id = $1 AND date >= $2 AND date < $3
           AND value IS NOT NULL",
        params = list(
          tsid,
          seq(before_date, by = paste0("-", normal_years, " years"), length.out = 2L)[[2L]],
          before_date
        )
      )
      hist$date <- as.Date(hist$date)
      hist$doy <- noleap_doy(hist$date)
      hist$year <- as.integer(format(hist$date, "%Y"))

      stat <- function(f) {
        out <- rep(NA_real_, 365L)
        if (nrow(hist) == 0L) {
          return(out)
        }
        by_doy <- split(hist$value, hist$doy)
        years <- vapply(
          split(hist$year, hist$doy),
          function(y) length(unique(y)),
          integer(1L)
        )
        keep <- names(by_doy)[years >= normal_min_years]
        out[as.integer(keep)] <- vapply(by_doy[keep], f, numeric(1L))
        fill_short_gaps(out, normal_max_gap)
      }
      q <- function(p) function(v) stats::quantile(v, p, names = FALSE)
      out <- data.frame(
        norm_min = stat(min),
        norm_q25 = stat(q(0.25)),
        norm_q75 = stat(q(0.75)),
        norm_max = stat(max),
        norm_mean = stat(mean)
      )
      # Interpolation runs per column, so keep the bands nested
      out$norm_q25 <- pmax(out$norm_q25, out$norm_min)
      out$norm_q75 <- pmin(pmax(out$norm_q75, out$norm_q25), out$norm_max)
      normals_cache[[key]] <- out
      out
    }

    # Normals for every date from from_date to to_date (dated at local noon),
    # whether or not the current year has data on those dates
    daily_normals <- function(tsid, from_date, to_date, before_date = from_date) {
      dates <- seq(from_date, to_date, by = "day")
      out <- doy_normals(tsid, before_date)[noleap_doy(dates), ]
      out$date <- dates
      out$datetime <- noon(dates)
      rownames(out) <- NULL
      out
    }

    # Raw corrected values, or daily means for long windows
    fetch_series <- function(tsid, win) {
      if (win$days > daily_threshold_days) {
        out <- fetch_daily(tsid, win$from_date, win$to_date)
        return(out[, c("datetime", "value")])
      }
      out <- DBI::dbGetQuery(
        con,
        "SELECT datetime, value_corrected AS value
         FROM continuous.measurements_continuous_corrected($1, $2, $3)
         ORDER BY datetime",
        params = list(tsid, win$start, win$end)
      )
      attr(out$datetime, "tzone") <- tz
      out
    }

    # Cumulative degree days, reset each season: freezing (CFDD) from Oct 1,
    # thawing (CDDT) from Apr 1, matching the flood dashboard.
    degree_days <- function(tsid, win, freezing) {
      season_month <- if (freezing) 10L else 4L
      season_start <- function(date) {
        y <- as.integer(format(date, "%Y"))
        s <- as.Date(sprintf("%d-%02d-01", y, season_month))
        if (date < s) as.Date(sprintf("%d-%02d-01", y - 1L, season_month)) else s
      }
      # Start at the season start so the accumulation is correct inside the window
      first <- season_start(win$from_date)
      season_of <- function(dates) {
        month <- as.integer(format(dates, "%m"))
        year <- as.integer(format(dates, "%Y"))
        ifelse(month >= season_month, year, year - 1L)
      }
      contribution <- function(temp) {
        if (freezing) pmax(-temp, 0) else pmax(temp, 0)
      }

      out <- fetch_daily(tsid, first, win$to_date)
      if (nrow(out) > 0L) {
        add <- contribution(out$value)
        add[is.na(add)] <- 0
        out$value <- stats::ave(add, season_of(out$date), FUN = cumsum)
      }
      out <- out[out$date >= win$from_date, c("datetime", "value")]

      # Normal: the same accumulation over each day's normal mean temperature,
      # on every day of the window. A long gap in the normals stops the curve
      # there rather than under-counting.
      normal <- NULL
      if (isTRUE(input$normals)) {
        nd <- daily_normals(tsid, first, win$to_date, before_date = win$from_date)
        nd$normal <- stats::ave(
          contribution(nd$norm_mean),
          season_of(nd$date),
          FUN = cumsum
        )
        normal <- nd[nd$date >= win$from_date, c("datetime", "normal")]
      }
      list(data = out, normal = normal)
    }

    # Climate normals for a plot, or NULL when the toggle is off. "range"
    # draws typical (25th-75th percentile) and historic (min-max) bands,
    # "mean" a dashed line at the normal daily mean, and "line" a dashed line
    # at pre-computed normal values (degree days).
    plot_normals <- function(kind, tsid, win, data = NULL) {
      if (!isTRUE(input$normals)) {
        return(NULL)
      }
      if (identical(kind, "line")) {
        return(list(kind = kind, data = data))
      }
      list(kind = kind, data = daily_normals(tsid, win$from_date, win$to_date))
    }

    # Split row indices into runs where `ok` holds, so bands break cleanly at
    # missing normals instead of being joined across the gap
    ok_runs <- function(ok) {
      runs <- rle(ok)
      ends <- cumsum(runs$lengths)
      starts <- ends - runs$lengths + 1L
      lapply(which(runs$values), function(i) starts[[i]]:ends[[i]])
    }

    # One filled band per run of available normals, sharing a legend entry
    add_band <- function(p, x, lo, hi, name, fill) {
      runs <- ok_runs(!is.na(lo) & !is.na(hi))
      for (i in seq_along(runs)) {
        r <- runs[[i]]
        p <- plotly::add_ribbons(
          p,
          x = x[r],
          ymin = lo[r],
          ymax = hi[r],
          name = name,
          legendgroup = name,
          showlegend = i == 1L,
          fillcolor = fill,
          line = list(width = 0),
          hoverinfo = "skip"
        )
      }
      list(plot = p, n = length(runs))
    }

    # Add climate-normal traces to a plot (drawn before the data so they sit
    # underneath it). Returns the plot and how many traces were added.
    add_normals <- function(p, normals, colour) {
      lang <- language$language
      if (is.null(normals)) {
        return(list(plot = p, n = 0L))
      }
      d <- normals$data
      values <- if (identical(normals$kind, "range")) {
        c(d$norm_q25, d$norm_q75, d$norm_min, d$norm_max)
      } else if (identical(normals$kind, "mean")) {
        d$norm_mean
      } else {
        d$normal
      }
      # Normals are switched on but there's too little history for any of
      # these dates: say so, rather than silently drawing nothing (which
      # looks like the switch stopped working). An annotation is part of the
      # layout, so trace indices are unaffected.
      if (nrow(d) == 0L || all(is.na(values))) {
        p <- plotly::layout(
          p,
          annotations = list(list(
            text = sprintf(tr("img_ts_no_normals", lang), normal_min_years),
            # Bottom right inside the plot, clear of the legend above it
            xref = "paper",
            yref = "paper",
            x = 1,
            y = 0,
            xanchor = "right",
            yanchor = "bottom",
            showarrow = FALSE,
            bgcolor = "rgba(255,255,255,0.8)",
            font = list(size = 11, color = "#6c757d")
          ))
        )
        return(list(plot = p, n = 0L))
      }
      x <- plot_time(d$datetime)
      if (identical(normals$kind, "range")) {
        historic <- add_band(
          p,
          x,
          d$norm_min,
          d$norm_max,
          tr("img_ts_historic_range", lang),
          with_alpha(colour, 0.12)
        )
        typical <- add_band(
          historic$plot,
          x,
          d$norm_q25,
          d$norm_q75,
          tr("img_ts_typical_range", lang),
          with_alpha(colour, 0.28)
        )
        return(list(plot = typical$plot, n = historic$n + typical$n))
      }
      # Lines break at NA on their own (plotly doesn't join across gaps)
      y <- if (identical(normals$kind, "mean")) d$norm_mean else d$normal
      p <- plotly::add_lines(
        p,
        x = x,
        y = y,
        name = tr("img_ts_normal", lang),
        line = list(color = colour, dash = "dash", width = 1.5),
        hovertemplate = paste0(
          tr("img_ts_normal", lang),
          ": %{y:.1f}<extra></extra>"
        )
      )
      list(plot = p, n = 1L)
    }

    hydro_data <- reactive({
      req(input$parameter)
      tsid <- as.integer(input$parameter)
      req(tsid %in% selected_station()$timeseries_id)
      fetch_series(tsid, date_window())
    })

    secondary_data <- reactive({
      var <- input$secondary
      req(var %in% names(secondary_vars))
      lang <- language$language
      win <- date_window()
      cand <- nearby_stations()
      validate(need(
        nrow(cand) > 0L,
        sprintf(tr("img_ts_no_nearby", lang), nearby_max_km)
      ))
      req(input$series_station)
      tsid <- as.integer(input$series_station)
      req(tsid %in% cand$timeseries_id)
      row <- cand[cand$timeseries_id == tsid, ]
      deg_day <- paste0("(", deg_c, " ", tr("img_ts_day", lang), ")")

      if (var %in% c("cfdd", "cddt")) {
        dd <- degree_days(tsid, win, freezing = identical(var, "cfdd"))
        return(list(
          data = dd$data,
          type = "line",
          y_label = paste(tr(secondary_vars[[var]]$key, lang), deg_day),
          normals = if (!is.null(dd$normal)) {
            plot_normals("line", tsid, win, data = dd$normal)
          }
        ))
      }

      if (identical(var, "precip")) {
        return(list(
          data = fetch_daily(tsid, win$from_date, win$to_date),
          type = "bar",
          y_label = tr("img_ts_daily_precip", lang),
          normals = plot_normals("mean", tsid, win)
        ))
      }

      list(
        data = fetch_series(tsid, win),
        type = "line",
        y_label = if (identical(var, "temperature")) {
          paste0(tr("img_ts_air_temp", lang), " (", deg_c, ")")
        } else {
          param_label(row)
        },
        normals = plot_normals("range", tsid, win)
      )
    })

    photos <- reactive({
      st <- selected_station()
      win <- date_window()
      out <- DBI::dbGetQuery(
        con,
        "SELECT i.image_id, i.datetime
         FROM files.images i
         JOIN files.image_series s
           ON s.img_series_id = i.img_series_id
         WHERE s.location_id = $1
           AND i.datetime >= $2 AND i.datetime < $3
           AND i.file IS NOT NULL
         ORDER BY i.datetime, i.image_id",
        params = list(st$location_id[[1L]], win$start, win$end)
      )
      attr(out$datetime, "tzone") <- tz
      out
    })

    # Photo selection ##########################################################

    # Index into photos() of the photo being shown
    selected_photo <- reactiveVal(NULL)

    # Show the latest photo whenever the set of photos changes. Runs before the
    # hydrograph re-renders so the redrawn plot highlights the right photo.
    observeEvent(
      photos(),
      {
        n <- nrow(photos())
        selected_photo(if (n > 0L) n else NULL)
      },
      priority = 10
    )

    observeEvent(input$prev_photo, {
      idx <- selected_photo()
      req(idx, idx > 1L)
      selected_photo(idx - 1L)
    })

    observeEvent(input$next_photo, {
      idx <- selected_photo()
      req(idx, idx < nrow(photos()))
      selected_photo(idx + 1L)
    })

    # Clicking either plot selects the photo closest in time to the click.
    # Plotly sends clicks as an app-level input ("plotly_click-<source>"), not
    # a module input, so read it from the root session; `input[[...]]` here
    # would add the module prefix again and never fire. Reads the raw input
    # rather than plotly::event_data(), which warns when called before the
    # plots have rendered.
    click_source <- ns("plots")
    click_input <- paste0("plotly_click-", click_source)
    root_input <- session$rootScope()$input
    observeEvent(root_input[[click_input]], {
      click <- jsonlite::fromJSON(root_input[[click_input]])
      ph <- photos()
      clicked_epoch <- suppressWarnings(as.numeric(click$customdata))
      clicked_epoch <- clicked_epoch[!is.na(clicked_epoch)]
      req(nrow(ph) > 0L, length(clicked_epoch) > 0L)
      clicked_epoch <- clicked_epoch[[1L]]
      selected_photo(which.min(abs(as.numeric(ph$datetime) - clicked_epoch)))
    })

    # Plots ####################################################################

    # Shared layout for the two stacked plots: both span the selected window
    # with the same side margins so their x-axes line up, and
    # www/js/linked_plots.js mirrors zoom, pan, reset and the hover line
    # between them.
    linked_x_layout <- function(p, y_label, partner) {
      win <- isolate(date_window())
      p |>
        plotly::layout(
          xaxis = list(
            title = "",
            type = "date",
            range = plot_time(c(win$start, win$end))
          ),
          yaxis = list(title = y_label, automargin = FALSE),
          # Snap to the nearest time so a click anywhere on the plot registers
          hovermode = "x unified",
          margin = list(l = 70, r = 20)
        ) |>
        plotly::config(displaylogo = FALSE, scrollZoom = TRUE) |>
        plotly::event_register("plotly_click") |>
        htmlwidgets::onRender(
          "function(el, x, data) { window.linkedPlotsSyncX(el, data); }",
          data = list(
            partner = partner,
            key = paste(win$from_date, win$to_date),
            range = plot_time(c(win$start, win$end))
          )
        )
    }

    output$hydrograph <- plotly::renderPlotly({
      lang <- language$language
      hd <- hydro_data()
      ph <- photos()
      st <- selected_station()
      ts_row <- st[st$timeseries_id == as.integer(input$parameter), ]
      validate(need(nrow(hd) > 0L, tr("img_ts_no_hydro_data", lang)))

      highlight <- highlight_xy(isolate(selected_photo()), ph, hd)
      normals <- add_normals(
        plotly::plot_ly(source = click_source),
        plot_normals("range", ts_row$timeseries_id[[1L]], isolate(date_window())),
        hydro_colour
      )
      # Normal bands come first, then the line, photo and highlight traces;
      # keep an (empty) photo trace so the highlight index is predictable
      highlight_trace <<- normals$n + 2L
      if (nrow(ph) == 0L) {
        ph_x <- NA_character_
        ph_y <- NA_real_
        ph_epoch <- NA_real_
      } else {
        ph_x <- plot_time(ph$datetime)
        ph_y <- value_at(hd, ph$datetime)
        ph_epoch <- as.numeric(ph$datetime)
      }

      normals$plot |>
        plotly::add_lines(
          x = plot_time(hd$datetime),
          y = hd$value,
          customdata = as.numeric(hd$datetime),
          name = ts_row[[tr("param_name_col", lang)]][[1L]],
          line = list(color = hydro_colour),
          hovertemplate = "%{x|%Y-%m-%d %H:%M}<br>%{y:.3f}<extra></extra>"
        ) |>
        plotly::add_markers(
          x = ph_x,
          y = ph_y,
          customdata = ph_epoch,
          name = tr("img_ts_photo", lang),
          marker = list(
            symbol = "triangle-down",
            size = 9,
            color = "#DC4405",
            line = list(color = "white", width = 1)
          ),
          hovertemplate = paste0(
            tr("img_ts_photo", lang),
            " %{x|%Y-%m-%d %H:%M}<extra></extra>"
          )
        ) |>
        plotly::add_markers(
          x = highlight$x,
          y = highlight$y,
          name = tr("img_ts_shown_photo", lang),
          marker = list(
            symbol = "circle-open",
            size = 18,
            color = "#F2A900",
            line = list(width = 3)
          ),
          hoverinfo = "skip"
        ) |>
        plotly::layout(
          legend = list(orientation = "h", x = 0, y = 1.08),
          margin = list(t = 40)
        ) |>
        linked_x_layout(param_label(ts_row), partner = ns("secondary_plot"))
    })

    # Index (0-based) of the "shown photo" trace in the drawn hydrograph
    highlight_trace <- 2L

    # Move the "shown photo" highlight without redrawing (keeps the user's zoom)
    observeEvent(selected_photo(), ignoreNULL = FALSE, {
      hd <- isolate(hydro_data())
      req(nrow(hd) > 0L)
      highlight <- highlight_xy(selected_photo(), isolate(photos()), hd)
      plotly::plotlyProxy("hydrograph", session) |>
        plotly::plotlyProxyInvoke(
          "restyle",
          list(x = list(list(highlight$x)), y = list(list(highlight$y))),
          list(highlight_trace)
        )
    })

    output$secondary_plot <- plotly::renderPlotly({
      lang <- language$language
      sd <- secondary_data()
      dat <- sd$data
      validate(need(nrow(dat) > 0L, tr("img_ts_no_secondary_data", lang)))
      label <- tr(secondary_vars[[input$secondary]]$key, lang)

      normals <- add_normals(
        plotly::plot_ly(source = click_source),
        sd$normals,
        secondary_colour
      )
      p <- normals$plot
      p <- if (identical(sd$type, "bar")) {
        plotly::add_bars(
          p,
          x = plot_time(dat$datetime),
          y = dat$value,
          customdata = as.numeric(dat$datetime),
          name = label,
          marker = list(color = secondary_colour),
          hovertemplate = "%{x|%Y-%m-%d}<br>%{y:.1f}<extra></extra>"
        )
      } else {
        plotly::add_lines(
          p,
          x = plot_time(dat$datetime),
          y = dat$value,
          customdata = as.numeric(dat$datetime),
          name = label,
          line = list(color = secondary_colour),
          hovertemplate = "%{x|%Y-%m-%d %H:%M}<br>%{y:.2f}<extra></extra>"
        )
      }

      # A legend is only needed to explain the normals
      p |>
        plotly::layout(
          showlegend = normals$n > 0L,
          legend = list(orientation = "h", x = 0, y = 1.12),
          margin = list(t = if (normals$n > 0L) 30 else 10)
        ) |>
        linked_x_layout(sd$y_label, partner = ns("hydrograph"))
    })

    # Station map ##############################################################

    # The hydrograph station and the lower plot's station, coloured like their
    # plots. Redrawn whole on change: it's small, and leafletProxy updates
    # would be lost when the page re-renders on a language change.
    output$station_map <- leaflet::renderLeaflet({
      lang <- language$language
      name_col <- tr("generic_name_col", lang)
      stations_here <- selected_station()
      st <- stations_here[1L, ]
      hydro_ts <- stations_here[
        stations_here$timeseries_id == suppressWarnings(as.integer(input$parameter)),
      ]
      # Legend: "<station code> - <parameter plotted>" for each plot
      legend_colours <- hydro_colour
      legend_labels <- paste0(
        st$location_code,
        " - ",
        if (nrow(hydro_ts) > 0L) {
          hydro_ts[[tr("param_name_col", lang)]][[1L]]
        } else {
          tr("img_ts_hydrograph", lang)
        }
      )
      pts <- data.frame(
        lat = st$latitude,
        lng = st$longitude,
        label = paste0(st$location_code, " - ", st[[name_col]]),
        colour = hydro_colour,
        radius = 9
      )
      cand <- nearby_stations()
      sec <- cand[cand$timeseries_id == suppressWarnings(as.integer(input$series_station)), ]
      if (nrow(sec) > 0L) {
        sec <- sec[1L, ]
        legend_colours <- c(legend_colours, secondary_colour)
        legend_labels <- c(
          legend_labels,
          paste0(
            sec$location_code,
            " - ",
            tr(secondary_vars[[input$secondary]]$key, lang)
          )
        )
        # Drawn second and smaller, so it shows inside the hydrograph marker
        # when both plots use the same station
        pts <- rbind(pts, data.frame(
          lat = sec$latitude,
          lng = sec$longitude,
          label = paste0(sec$location_code, " - ", sec[[name_col]]),
          colour = secondary_colour,
          radius = 5
        ))
      }
      # Some stations have no coordinates; show what can be placed
      pts <- pts[!is.na(pts$lat) & !is.na(pts$lng), ]

      map <- leaflet::leaflet(
        options = leaflet::leafletOptions(maxZoom = 13, attributionControl = FALSE)
      ) |>
        leaflet::addProviderTiles("Esri.WorldTopoMap") |>
        leaflet::addCircleMarkers(
          lng = pts$lng,
          lat = pts$lat,
          radius = pts$radius,
          color = "white",
          weight = 2,
          fillColor = pts$colour,
          fillOpacity = 1,
          label = pts$label
        ) |>
        leaflet::addLegend(
          position = "bottomright",
          colors = legend_colours,
          labels = legend_labels,
          opacity = 1
        )
      if (nrow(pts) == 0L) {
        # Nothing to place: show the territory
        leaflet::setView(map, lng = -135.0, lat = 64.0, zoom = 4)
      } else if (
        nrow(pts) > 1L &&
          (diff(range(pts$lat)) > 0 || diff(range(pts$lng)) > 0)
      ) {
        pad <- 0.05
        leaflet::fitBounds(
          map,
          min(pts$lng) - pad,
          min(pts$lat) - pad,
          max(pts$lng) + pad,
          max(pts$lat) + pad
        )
      } else {
        leaflet::setView(map, lng = pts$lng[[1L]], lat = pts$lat[[1L]], zoom = 9)
      }
    })

    # Photo viewer #############################################################

    output$photo_header <- renderUI({
      st <- selected_station()
      idx <- selected_photo()
      ph <- photos()
      title <- paste0(
        st$location_code[[1L]],
        " - ",
        st[[tr("generic_name_col", language$language)]][[1L]]
      )
      if (is.null(idx) || idx > nrow(ph)) {
        return(title)
      }
      tagList(
        title,
        tags$small(class = "text-muted ms-2", format_time(ph$datetime[[idx]]))
      )
    })

    output$photo_count <- renderText({
      n <- nrow(photos())
      idx <- selected_photo()
      if (n == 0L || is.null(idx)) {
        return(tr("img_ts_no_photos_short", language$language))
      }
      sprintf(tr("img_ts_photo_n_of_m", language$language), idx, n)
    })

    output$photo <- renderUI({
      lang <- language$language
      idx <- selected_photo()
      ph <- photos()
      if (is.null(idx) || idx > nrow(ph)) {
        return(tags$p(class = "text-muted", tr("img_ts_no_photos", lang)))
      }

      img <- DBI::dbGetQuery(
        con,
        "SELECT format, file FROM files.images WHERE image_id = $1",
        params = list(ph$image_id[[idx]])
      )
      src <- if (nrow(img) > 0L) {
        application_image_data_url(img$file[[1L]], img$format[[1L]])
      }
      if (is.null(src)) {
        return(tags$p(class = "text-muted", tr("image_content_not_found", lang)))
      }

      tags$img(
        src = src,
        alt = sprintf(tr("img_ts_photo_alt", lang), format_time(ph$datetime[[idx]])),
        style = "max-width: 100%; max-height: 100%; object-fit: contain;"
      )
    })
  }) # End of moduleServer
}
