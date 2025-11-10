# UI and server code for imputing missing values in continuous data

imputeMissingUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(
      HTML(sprintf(
        "
     /* Add colors to the accordion. Using ns() makes it specific to each accordion */
      #%s.accordion {
        /* body background */
        --bs-accordion-bg:          #FFFCF5;
        /* collapsed header */
        --bs-accordion-btn-bg:      #FBE5B2;
        /* expanded header */
        --bs-accordion-active-bg:   #FBE5B2;
      }
    ",
        ns("accordion1")
      )),
      HTML(sprintf(
        "
     /* Add colors to the accordion. Using ns() makes it specific to each accordion */
      #%s.accordion {
        /* body background */
        --bs-accordion-bg:          #E5F4F6;
        /* collapsed header */
        --bs-accordion-btn-bg:      #0097A9;
        /* expanded header */
        --bs-accordion-active-bg:   #0097A9;
      }
    ",
        ns("accordion2")
      )),
      HTML(sprintf(
        "
     /* Add colors to the accordion. Using ns() makes it specific to each accordion */
      #%s.accordion {
        /* body background */
        --bs-accordion-bg:          #F1F4E5;
        /* collapsed header */
        --bs-accordion-btn-bg:      #7A9A01;
        /* expanded header */
        --bs-accordion-active-bg:   #7A9A01;
      }
    ",
        ns("accordion3")
      ))
    ),

    page_fluid(
      accordion(
        id = ns("accordion1"),
        open = "ts_panel",
        accordion_panel(
          id = ns("ts_panel"),
          title = "Timeseries selection",
          DT::DTOutput(ns("ts_table")),
          input_task_button(ns("plot_ts_pre"), "Plot timeseries"),
          plotly::plotlyOutput(ns("ts_plot_pre"))
        )
      ),
      accordion(
        id = ns("accordion2"),
        open = "options_panel",
        accordion_panel(
          id = ns("options_panel"),
          title = "Imputation options",
          textInput(
            ns("start"),
            "Start datetime (UTC)",
            placeholder = "YYYY-MM-DD HH:MM:SS"
          ),
          textInput(
            ns("end"),
            "End datetime (UTC)",
            placeholder = "YYYY-MM-DD HH:MM:SS"
          ),
          numericInput(ns("radius"), "Search radius (km)", value = 10, min = 0),
          selectInput(
            ns("method"),
            "Interpolation method",
            choices = c(
              "Direct (use other timeseries with calculated offset)" = "direct",
              "Linear (direct point to point)" = "linear",
              "Spline (curve fitted with surrounding data)" = "spline"
            )
          ),
          actionButton(ns("load"), "Load data")
        )
      ),
      accordion(
        id = ns("accordion3"),
        open = FALSE,
        accordion_panel(
          id = ns("impute_panel"),
          title = "Impute",
          textOutput(ns("direct_impute_selection")),
          DT::DTOutput(ns("candidates")),
          plotly::plotlyOutput(ns("plot")),
          actionButton(ns("impute"), "Impute"),
          actionButton(ns("commit"), "Commit to DB")
        )
      )
    )
  )
}


imputeMissing <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ts_meta <- reactive({
      dbGetQueryDT(
        session$userData$AquaCache,
        paste(
          "SELECT tm.timeseries_id, tm.location_name AS location, tm.parameter_name AS parameter,",
          "tm.media_type AS media, tm.aggregation_type AS aggregation, tm.recording_rate,",
          "l.latitude, l.longitude FROM continuous.timeseries_metadata_en tm JOIN locations l ON tm.location_id = l.location_id"
        )
      )
    })

    output$ts_table <- DT::renderDT({
      # Convert some data types to factors for better filtering in DT
      df <- ts_meta()
      display <- as.data.frame(df)
      display$recording_rate <- as.factor(display$recording_rate)
      display$media <- as.factor(display$media)
      display$aggregation <- as.factor(display$aggregation)
      display$parameter <- as.factor(display$parameter)
      display$latitude <- NULL
      display$longitude <- NULL

      DT::datatable(
        display,
        selection = 'single',
        options = list(
          columnDefs = list(
            list(targets = 0, visible = FALSE)
          ),
          scrollX = TRUE,
          initComplete = htmlwidgets::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({",
            "  'background-color': '#079',",
            "  'color': '#fff',",
            "  'font-size': '100%',",
            "});",
            "$(this.api().table().body()).css({",
            "  'font-size': '90%',",
            "});",
            "}"
          )
        ),
        filter = 'top'
      )
    })

    selected_ts <- reactiveVal()

    observeEvent(input$ts_table_rows_selected, {
      req(input$ts_table_rows_selected)
      selected_ts(ts_meta()[["timeseries_id"]][[input$ts_table_rows_selected]])
    })

    selected_meta <- reactive({
      req(selected_ts())
      meta <- as.data.frame(ts_meta())
      meta[meta$timeseries_id == selected_ts(), , drop = FALSE]
    })

    ts_plot_pre_task <- ExtendedTask$new(function(df) {
      promises::future_promise({
        plot <- plotly::plot_ly(
          data = df,
          x = ~datetime,
          y = ~value_raw,
          type = 'scatter',
          mode = 'lines',
          name = 'Original'
        ) %>%
          plotly::add_lines(
            y = ~value_corrected,
            name = 'Corrected',
            line = list(color = 'red')
          ) %>%
          plotly::layout(
            title = NULL,
            xaxis = list(title = "Datetime"),
            yaxis = list(title = "Value")
          )
        return(plot)
      })
    }) |>
      bind_task_button("plot_ts_pre")

    observeEvent(input$plot_ts_pre, {
      req(selected_ts())
      query <- sprintf(
        "SELECT datetime, value_raw, value_corrected FROM continuous.measurements_continuous_corrected WHERE timeseries_id = %s ORDER BY datetime",
        selected_ts()
      )
      df <- DBI::dbGetQuery(session$userData$AquaCache, query)
      ts_plot_pre_task$invoke(df)
    })

    output$ts_plot_pre <- plotly::renderPlotly({
      ts_plot_pre_task$result()
    })

    raw_data <- reactiveVal(NULL)
    full_data <- reactiveVal(NULL)
    candidates <- reactiveVal(NULL)
    imputed_data <- reactiveVal(NULL)

    parse_datetime <- function(x) {
      if (is.null(x) || !nzchar(x)) {
        return(NA_real_)
      }
      parsed <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
      if (is.na(parsed)) {
        return(NA_real_)
      }
      parsed
    }

    record_rate_seconds <- function(rate) {
      if (is.null(rate) || is.na(rate) || !nzchar(rate)) {
        return(NA_real_)
      }
      suppressWarnings(
        tryCatch(
          lubridate::period_to_seconds(lubridate::period(rate)),
          error = function(e) NA_real_
        )
      )
    }

    haversine_km <- function(lat1, lon1, lat2, lon2) {
      if (any(is.na(c(lat1, lon1, lat2, lon2)))) {
        return(NA_real_)
      }
      rad <- pi / 180
      dlat <- (lat2 - lat1) * rad
      dlon <- (lon2 - lon1) * rad
      a <- sin(dlat / 2)^2 + cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
      c <- 2 * atan2(sqrt(a), sqrt(1 - a))
      6371 * c
    }

    fetch_series <- function(tsid, start_dt, end_dt) {
      con <- session$userData$AquaCache
      query <- DBI::sqlInterpolate(
        con,
        paste(
          "SELECT datetime, value FROM continuous.measurements_continuous",
          "WHERE timeseries_id = $1 AND datetime >= $2 AND datetime <= $3",
          "ORDER BY datetime"
        ),
        tsid = tsid,
        start_dt = start_dt,
        end_dt = end_dt
      )
      res <- DBI::dbGetQuery(con, query)
      if (nrow(res) > 0) {
        res$datetime <- as.POSIXct(res$datetime, tz = "UTC")
        res$value <- as.numeric(res$value)
      }
      res
    }

    observeEvent(input$load, {
      req(selected_ts())
      start_dt <- parse_datetime(input$start)
      end_dt <- parse_datetime(input$end)
      if (is.na(start_dt) || is.na(end_dt)) {
        showNotification(
          "Please provide valid start and end datetimes in UTC.",
          type = "error"
        )
        return()
      }
      if (start_dt >= end_dt) {
        showNotification(
          "The start datetime must be before the end datetime.",
          type = "error"
        )
        return()
      }

      df <- fetch_series(selected_ts(), start_dt, end_dt)
      if (nrow(df) == 0) {
        showNotification(
          "No measurements were found for the selected period.",
          type = "error"
        )
        raw_data(NULL)
        full_data(NULL)
        imputed_data(NULL)
        candidates(NULL)
        return()
      }

      df <- df[order(df$datetime), ]
      raw_data(df)

      meta_row <- selected_meta()
      step <- record_rate_seconds(meta_row$recording_rate[[1]])
      if (is.na(step) || step <= 0) {
        if (nrow(df) > 1) {
          diffs <- diff(df$datetime)
          step <- stats::median(as.numeric(diffs, units = "secs"))
        } else {
          step <- 3600
        }
      }

      seq_start <- min(start_dt, min(df$datetime, na.rm = TRUE))
      seq_end <- max(end_dt, max(df$datetime, na.rm = TRUE))
      full_dt <- data.frame(
        datetime = seq(seq_start, seq_end, by = step)
      )
      df_full <- merge(full_dt, df, by = "datetime", all.x = TRUE)
      df_full <- df_full[order(df_full$datetime), ]
      full_data(df_full)
      imputed_data(NULL)

      if (input$method == "direct") {
        meta <- as.data.frame(ts_meta())
        selected_row <- meta[
          meta$timeseries_id == selected_ts(),
          ,
          drop = FALSE
        ]
        if (nrow(selected_row) > 0) {
          meta <- meta[meta$timeseries_id != selected_ts(), , drop = FALSE]
          meta <- meta[meta$parameter == selected_row$parameter, , drop = FALSE]
          meta$distance_km <- mapply(
            haversine_km,
            selected_row$latitude[[1]],
            selected_row$longitude[[1]],
            meta$latitude,
            meta$longitude
          )
          meta <- meta[
            is.na(meta$distance_km) | meta$distance_km <= input$radius,
            ,
            drop = FALSE
          ]
          meta <- meta[order(meta$distance_km), ]
          candidates(meta[, c(
            "timeseries_id",
            "location",
            "parameter",
            "recording_rate",
            "distance_km"
          )])
        } else {
          candidates(NULL)
        }
      } else {
        candidates(NULL)
      }
    })

    observeEvent(
      {
        list(full_data(), input$radius, input$method)
      },
      {
        if (is.null(full_data()) || input$method != "direct") {
          candidates(NULL)
          return()
        }
        meta <- as.data.frame(ts_meta())
        selected_row <- meta[
          meta$timeseries_id == selected_ts(),
          ,
          drop = FALSE
        ]
        if (nrow(selected_row) == 0) {
          candidates(NULL)
          return()
        }
        meta <- meta[meta$timeseries_id != selected_ts(), , drop = FALSE]
        meta <- meta[meta$parameter == selected_row$parameter, , drop = FALSE]
        meta$distance_km <- mapply(
          haversine_km,
          selected_row$latitude[[1]],
          selected_row$longitude[[1]],
          meta$latitude,
          meta$longitude
        )
        meta <- meta[
          is.na(meta$distance_km) | meta$distance_km <= input$radius,
          ,
          drop = FALSE
        ]
        meta <- meta[order(meta$distance_km), ]
        if (nrow(meta) == 0) {
          candidates(NULL)
        } else {
          candidates(meta[, c(
            "timeseries_id",
            "location",
            "parameter",
            "recording_rate",
            "distance_km"
          )])
        }
      },
      ignoreNULL = FALSE
    )

    output$direct_impute_selection <- renderText({
      req(input$method == "direct")
      if (is.null(candidates()) || nrow(candidates()) == 0) {
        return("No nearby timeseries were found within the selected radius.")
      }
      "Select a timeseries below to impute missing values using the direct method."
    })

    output$candidates <- DT::renderDT({
      req(input$method == "direct")
      req(candidates())
      DT::datatable(
        candidates(),
        selection = "single",
        options = list(scrollX = TRUE),
        filter = "top",
        rownames = FALSE
      )
    })

    perform_impute <- function(df, method, cand = NULL) {
      df <- df[order(df$datetime), ]
      df$imputed <- FALSE
      missing_idx <- which(is.na(df$value))
      if (!length(missing_idx)) {
        return(df)
      }

      if (method == "direct") {
        if (is.null(cand) || nrow(cand) == 0) {
          return(NULL)
        }
        cand <- cand[order(cand$datetime), ]
        names(cand)[names(cand) == "value"] <- "reference_value"
        merged <- merge(df, cand, by = "datetime", all.x = TRUE)
        overlap <- !is.na(merged$value) & !is.na(merged$reference_value)
        if (!any(overlap)) {
          return(NULL)
        }
        offset <- mean(
          merged$value[overlap] - merged$reference_value[overlap],
          na.rm = TRUE
        )
        fill_idx <- which(is.na(merged$value) & !is.na(merged$reference_value))
        if (!length(fill_idx)) {
          return(NULL)
        }
        df$value[fill_idx] <- merged$reference_value[fill_idx] + offset
        df$imputed[fill_idx] <- TRUE
        return(df)
      }

      available <- which(!is.na(df$value))
      if (method == "linear" && length(available) < 2) {
        return(NULL)
      }
      if (method == "spline" && length(available) < 3) {
        return(NULL)
      }

      x <- as.numeric(df$datetime[available])
      y <- df$value[available]
      xout <- as.numeric(df$datetime)
      if (method == "linear") {
        interp <- stats::approx(x = x, y = y, xout = xout, rule = 1)$y
      } else {
        interp <- stats::spline(x = x, y = y, xout = xout, method = "natural")$y
      }
      filled_idx <- missing_idx[!is.na(interp[missing_idx])]
      if (!length(filled_idx)) {
        return(NULL)
      }
      df$value[filled_idx] <- interp[filled_idx]
      df$imputed[filled_idx] <- TRUE
      df
    }

    observeEvent(input$impute, {
      req(full_data())
      method <- input$method
      df <- full_data()
      if (method == "direct") {
        sel <- input$candidates_rows_selected
        if (length(sel) != 1) {
          showNotification(
            "Select a timeseries for direct imputation.",
            type = "error"
          )
          return()
        }
        ref_meta <- candidates()
        if (is.null(ref_meta) || nrow(ref_meta) < sel) {
          showNotification(
            "Unable to determine the selected timeseries.",
            type = "error"
          )
          return()
        }
        tsid2 <- ref_meta[sel, "timeseries_id"]
        ref_df <- fetch_series(tsid2, min(df$datetime), max(df$datetime))
        if (nrow(ref_df) == 0) {
          showNotification(
            "The selected reference timeseries has no data in the requested period.",
            type = "error"
          )
          return()
        }
        ref_full <- merge(
          data.frame(datetime = df$datetime),
          ref_df,
          by = "datetime",
          all.x = TRUE
        )
        imp <- perform_impute(df, "direct", ref_full)
        if (is.null(imp)) {
          showNotification(
            "Unable to impute values with the selected timeseries. Ensure there is overlapping data to compute an offset.",
            type = "error"
          )
          return()
        }
      } else {
        imp <- perform_impute(df, method)
        if (is.null(imp)) {
          if (method == "linear") {
            showNotification(
              "At least two existing measurements are required for linear interpolation.",
              type = "error"
            )
          } else {
            showNotification(
              "At least three existing measurements are required for spline interpolation.",
              type = "error"
            )
          }
          return()
        }
      }

      if (!any(imp$imputed)) {
        showNotification(
          "No missing values met the criteria for imputation.",
          type = "warning"
        )
      }
      imputed_data(imp)
    })

    output$plot <- plotly::renderPlotly({
      req(imputed_data())
      df <- imputed_data()
      existing <- df
      existing$value[df$imputed] <- NA
      imputed_only <- df
      imputed_only$value[!df$imputed] <- NA
      plotly::plot_ly() %>%
        plotly::add_lines(
          data = existing,
          x = ~datetime,
          y = ~value,
          name = "Existing",
          line = list(color = "#0072B2")
        ) %>%
        plotly::add_markers(
          data = existing,
          x = ~datetime,
          y = ~value,
          name = "Existing",
          marker = list(color = "#0072B2", size = 4),
          showlegend = FALSE
        ) %>%
        plotly::add_lines(
          data = imputed_only,
          x = ~datetime,
          y = ~value,
          name = "Imputed",
          line = list(color = "#D55E00")
        ) %>%
        plotly::add_markers(
          data = imputed_only,
          x = ~datetime,
          y = ~value,
          name = "Imputed",
          marker = list(color = "#D55E00", size = 6)
        ) %>%
        plotly::layout(
          xaxis = list(title = "Datetime"),
          yaxis = list(title = "Value")
        )
    })

    observeEvent(input$commit, {
      req(imputed_data())
      df <- imputed_data()
      to_push <- df[df$imputed, c("datetime", "value")]
      if (nrow(to_push) == 0) {
        showNotification(
          "There are no imputed values to commit.",
          type = "error"
        )
        return()
      }
      to_push$timeseries_id <- selected_ts()
      to_push$imputed <- TRUE

      tryCatch(
        {
          AquaCache::addNewContinuous(
            tsid = selected_ts(),
            df = to_push,
            con = session$userData$AquaCache,
            target = "realtime",
            overwrite = "conflict"
          )
          showNotification(
            "Imputed values saved to the database.",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(paste("Commit failed:", e$message), type = "error")
        }
      )
    })
  })
}
