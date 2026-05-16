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
      uiOutput(ns("banner")),
      accordion(
        id = ns("accordion1"),
        open = "ts_panel",
        accordion_panel(
          id = ns("ts_panel"),
          title = "Timeseries selection",
          DT::DTOutput(ns("ts_table")),
          dateRangeInput(
            ns("dt_range_pre"),
            "Date range (for viewing only)",
            start = Sys.Date() - 365,
            end = Sys.Date()
          ),
          bslib::input_task_button(ns("plot_ts_pre"), "Plot timeseries"),
          plotly::plotlyOutput(ns("ts_plot_pre"))
        )
      ),
      accordion(
        id = ns("accordion2"),
        open = "options_panel",
        accordion_panel(
          id = ns("options_panel"),
          title = "Imputation options",
          fluidRow(
            column(
              3,
              selectizeInput(
                ns("timezone"),
                "Input timezone",
                choices = input_timezone_choices(),
                selected = default_input_timezone(),
                multiple = FALSE,
                width = "100%"
              )
            ),
            column(
              9,
              shinyWidgets::airDatepickerInput(
                ns("dt_range_impute"),
                label = "Imputation datetime range",
                value = c(
                  .POSIXct(Sys.time() - 365 * 24 * 3600, tz = "UTC"),
                  .POSIXct(Sys.time(), tz = "UTC")
                ),
                range = TRUE,
                multiple = FALSE,
                timepicker = TRUE,
                maxDate = Sys.Date() + 1,
                startView = Sys.Date(),
                update_on = "change",
                tz = air_datetime_widget_timezone(default_input_timezone()),
                timepickerOpts = shinyWidgets::timepickerOptions(
                  minutesStep = 15,
                  timeFormat = "HH:mm"
                ),
                width = "100%"
              )
            )
          ),
          fluidRow(
            column(
              4,
              selectInput(
                ns("method"),
                "Imputation method",
                choices = c(
                  "Direct (nearby timeseries with offset)" = "direct",
                  "Linear interpolation" = "linear",
                  "Spline interpolation" = "spline"
                ),
                width = "100%"
              )
            ),
            column(
              4,
              numericInput(
                ns("min_gap"),
                "Minimum gap length (points)",
                value = 1,
                min = 1,
                step = 1,
                width = "100%"
              )
            ),
            column(
              4,
              numericInput(
                ns("max_gap"),
                "Maximum gap length (points)",
                value = NA,
                min = 1,
                step = 1,
                width = "100%"
              )
            )
          ),
          checkboxInput(
            ns("reimpute_existing"),
            "Treat existing imputed values as missing",
            value = TRUE
          ),
          conditionalPanel(
            condition = "input.method == 'direct'",
            ns = ns,
            fluidRow(
              column(
                4,
                numericInput(
                  ns("radius"),
                  "Search radius (km)",
                  value = 10,
                  min = 0,
                  step = 1,
                  width = "100%"
                )
              ),
              column(
                8,
                selectizeInput(
                  ns("extra_params"),
                  "Additional candidate parameters",
                  choices = NULL,
                  multiple = TRUE,
                  width = "100%"
                )
              )
            )
          ),
          bslib::input_task_button(ns("load"), "Load data")
        )
      ),
      accordion(
        id = ns("accordion3"),
        open = FALSE,
        accordion_panel(
          id = ns("impute_panel"),
          title = "Impute",
          uiOutput(ns("load_summary")),
          DT::DTOutput(ns("gaps")),
          conditionalPanel(
            condition = "input.method == 'direct'",
            ns = ns,
            textOutput(ns("direct_impute_selection")),
            DT::DTOutput(ns("candidates"))
          ),
          bslib::input_task_button(ns("impute"), "Impute"),
          plotly::plotlyOutput(ns("plot"), height = "430px"),
          DT::DTOutput(ns("imputed_values")),
          bslib::input_task_button(ns("commit"), "Commit to DB")
        )
      )
    )
  )
}


imputeMissing <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shift_impute_range_timezone <- function(tz_name) {
      current_value <- coerce_utc_datetime(input$dt_range_impute)
      if (
        is.null(current_value) ||
          !length(current_value) ||
          all(is.na(current_value))
      ) {
        return(invisible(NULL))
      }
      shinyWidgets::updateAirDateInput(
        session,
        "dt_range_impute",
        value = current_value,
        tz = air_datetime_widget_timezone(tz_name)
      )
    }

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "imputeMissing"
      )
    })

    commit_allowed <- reactiveVal(FALSE)
    observe({
      req(session$userData$AquaCache)
      allowed <- tryCatch(
        {
          DBI::dbGetQuery(
            session$userData$AquaCache,
            "
            SELECT
              has_table_privilege(
                current_user,
                'continuous.measurements_continuous',
                'INSERT'
              ) AS can_insert,
              has_table_privilege(
                current_user,
                'continuous.measurements_continuous',
                'DELETE'
              ) AS can_delete
            "
          )
        },
        error = function(e) data.frame(can_insert = FALSE, can_delete = FALSE)
      )
      can_commit <- isTRUE(allowed$can_insert[[1]]) &&
        isTRUE(allowed$can_delete[[1]])
      commit_allowed(can_commit)
      if (!can_commit) {
        shinyjs::disable("commit")
      }
    })

    ts_meta <- reactive({
      dbGetQueryDT(
        session$userData$AquaCache,
        "
        SELECT
          timeseries_id,
          location_id,
          location_name AS location,
          parameter_name AS parameter,
          media_type AS media,
          aggregation_type AS aggregation,
          recording_rate,
          latitude,
          longitude,
          start_datetime,
          end_datetime,
          timeseries_type_code,
          timeseries_type
        FROM continuous.timeseries_metadata_en
        ORDER BY location_name, parameter_name, aggregation_type, timeseries_id
        "
      )
    })

    observeEvent(ts_meta(), {
      params <- sort(unique(ts_meta()[["parameter"]]))
      updateSelectizeInput(
        session,
        "extra_params",
        choices = stats::setNames(params, params),
        server = TRUE
      )
    })

    output$ts_table <- DT::renderDT({
      df <- data.table::copy(ts_meta())
      df[, recording_rate := as.factor(as.character(recording_rate))]
      df[, media := as.factor(media)]
      df[, aggregation := as.factor(aggregation)]
      df[, parameter := as.factor(parameter)]
      display_cols <- setdiff(names(df), c(
        "location_id",
        "latitude",
        "longitude",
        "start_datetime",
        "end_datetime",
        "timeseries_type_code"
      ))
      display <- as.data.frame(df[, ..display_cols])

      DT::datatable(
        display,
        selection = "single",
        rownames = FALSE,
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
        filter = "top"
      )
    })

    selected_ts <- reactiveVal(NULL)

    observeEvent(input$ts_table_rows_selected, {
      req(input$ts_table_rows_selected)
      selected_ts(ts_meta()[["timeseries_id"]][[input$ts_table_rows_selected]])
      full_data(NULL)
      gap_data(NULL)
      candidates(NULL)
      candidate_reference_data(NULL)
      imputed_data(NULL)
    })

    selected_meta <- reactive({
      req(selected_ts())
      ts_meta()[timeseries_id == selected_ts()]
    })

    ts_plot_pre_task <- ExtendedTask$new(function(tsid, range, config) {
      promises::future_promise({
        con <- AquaConnect(
          name = config$dbName,
          host = config$dbHost,
          port = config$dbPort,
          username = config$dbUser,
          password = config$dbPass,
          silent = TRUE
        )
        on.exit(DBI::dbDisconnect(con))

        plot <- plotTimeseries(
          timeseries_id = tsid,
          raw = TRUE,
          con = con,
          start_date = range[1],
          end_date = range[2],
          grades = TRUE,
          approvals = TRUE,
          qualifiers = TRUE,
          resolution = "max",
          imputed = TRUE,
          data = TRUE,
          datum = FALSE
        )
        plot$plot
      })
    }) |>
      bslib::bind_task_button("plot_ts_pre")

    observeEvent(input$plot_ts_pre, {
      req(selected_ts())
      ts_plot_pre_task$invoke(
        tsid = selected_ts(),
        range = input$dt_range_pre,
        config = session$userData$config
      )
    })

    observeEvent(
      input$timezone,
      {
        shift_impute_range_timezone(normalize_input_timezone(input$timezone))
      },
      ignoreInit = TRUE
    )

    output$ts_plot_pre <- plotly::renderPlotly({
      ts_plot_pre_task$result()
    })

    full_data <- reactiveVal(NULL)
    gap_data <- reactiveVal(NULL)
    candidates <- reactiveVal(NULL)
    candidate_reference_data <- reactiveVal(NULL)
    imputed_data <- reactiveVal(NULL)
    loaded_range <- reactiveVal(NULL)
    loaded_step <- reactiveVal(NULL)

    empty_posixct_utc <- function(n = 1) {
      structure(
        rep(NA_real_, n),
        class = c("POSIXct", "POSIXt"),
        tzone = "UTC"
      )
    }

    as_posixct_utc <- function(value) {
      if (is.null(value) || !length(value)) {
        return(empty_posixct_utc())
      }
      out <- suppressWarnings(as.POSIXct(value, tz = "UTC"))
      attr(out, "tzone") <- "UTC"
      out
    }

    connect_from_config <- function(config) {
      AquaConnect(
        name = config$dbName,
        host = config$dbHost,
        port = config$dbPort,
        username = config$dbUser,
        password = config$dbPass,
        silent = TRUE
      )
    }

    finite_to_na <- function(value) {
      if (length(value) == 0 || is.infinite(value) || is.nan(value)) {
        return(NA_real_)
      }
      as.numeric(value)
    }

    interval_seconds <- function(value) {
      if (is.null(value) || !length(value) || all(is.na(value))) {
        return(NA_real_)
      }
      value <- value[[1]]
      if (inherits(value, "difftime")) {
        return(as.numeric(value, units = "secs"))
      }
      if (inherits(value, "Period")) {
        return(lubridate::period_to_seconds(value))
      }
      if (inherits(value, "Duration")) {
        return(as.numeric(value))
      }
      if (is.numeric(value)) {
        return(as.numeric(value))
      }

      txt <- trimws(as.character(value))
      if (!nzchar(txt)) {
        return(NA_real_)
      }
      if (grepl("^[0-9]+(\\.[0-9]+)?$", txt)) {
        return(as.numeric(txt))
      }
      if (grepl("^[0-9]{1,2}:[0-9]{2}:[0-9]{2}(\\.[0-9]+)?$", txt)) {
        parts <- as.numeric(strsplit(txt, ":", fixed = TRUE)[[1]])
        return(parts[[1]] * 3600 + parts[[2]] * 60 + parts[[3]])
      }
      suppressWarnings(
        tryCatch(
          lubridate::period_to_seconds(lubridate::period(txt)),
          error = function(e) NA_real_
        )
      )
    }

    infer_step_seconds <- function(dat, meta_row) {
      step <- interval_seconds(meta_row$recording_rate)
      if (is.na(step) || step <= 0) {
        if (!is.null(dat) && nrow(dat) > 1) {
          diffs <- as.numeric(diff(sort(dat$datetime)), units = "secs")
          diffs <- diffs[is.finite(diffs) & diffs > 0]
          if (length(diffs)) {
            step <- stats::median(diffs, na.rm = TRUE)
          }
        }
      }
      if (is.na(step) || step <= 0) {
        step <- 3600
      }
      as.numeric(step)
    }

    align_grid_start <- function(start_dt, origin, step_seconds) {
      if (is.na(origin)) {
        origin <- start_dt
      }
      delta <- as.numeric(difftime(start_dt, origin, units = "secs"))
      origin + ceiling(delta / step_seconds) * step_seconds
    }

    align_grid_end <- function(end_dt, origin, step_seconds) {
      if (is.na(origin)) {
        origin <- end_dt
      }
      delta <- as.numeric(difftime(end_dt, origin, units = "secs"))
      origin + floor(delta / step_seconds) * step_seconds
    }

    make_datetime_grid <- function(start_dt, end_dt, origin, step_seconds) {
      grid_start <- align_grid_start(start_dt, origin, step_seconds)
      grid_end <- align_grid_end(end_dt, origin, step_seconds)
      if (is.na(grid_start) || is.na(grid_end) || grid_start > grid_end) {
        return(empty_posixct_utc(0))
      }
      seq.POSIXt(grid_start, grid_end, by = step_seconds)
    }

    gap_limits <- function() {
      min_gap <- suppressWarnings(as.integer(input$min_gap))
      max_gap <- suppressWarnings(as.integer(input$max_gap))
      if (is.na(min_gap) || min_gap < 1L) {
        min_gap <- 1L
      }
      if (is.na(max_gap) || max_gap < min_gap) {
        max_gap <- Inf
      }
      list(min_gap = min_gap, max_gap = max_gap)
    }

    eligible_missing_indices <- function(mask, min_gap, max_gap) {
      mask[is.na(mask)] <- FALSE
      if (!any(mask)) {
        return(integer())
      }
      runs <- rle(mask)
      ends <- cumsum(runs$lengths)
      starts <- ends - runs$lengths + 1L
      idx <- integer()
      for (i in seq_along(runs$lengths)) {
        if (
          isTRUE(runs$values[[i]]) &&
            runs$lengths[[i]] >= min_gap &&
            runs$lengths[[i]] <= max_gap
        ) {
          idx <- c(idx, seq.int(starts[[i]], ends[[i]]))
        }
      }
      idx
    }

    build_gap_table <- function(dat, min_gap, max_gap) {
      if (is.null(dat) || nrow(dat) == 0) {
        return(data.table::data.table())
      }
      mask <- is.na(dat$value) & dat$in_impute_range
      mask[is.na(mask)] <- FALSE
      if (!any(mask)) {
        return(data.table::data.table())
      }

      runs <- rle(mask)
      ends <- cumsum(runs$lengths)
      starts <- ends - runs$lengths + 1L
      keep <- which(runs$values)
      if (!length(keep)) {
        return(data.table::data.table())
      }

      gaps <- data.table::data.table(
        gap = seq_along(keep),
        start_datetime = dat$datetime[starts[keep]],
        end_datetime = dat$datetime[ends[keep]],
        missing_points = runs$lengths[keep]
      )
      gaps[, impute := missing_points >= min_gap & missing_points <= max_gap]
      gaps
    }

    fetch_series_for_con <- function(con, tsid, start_dt, end_dt) {
      res <- DBI::dbGetQuery(
        con,
        "
        SELECT
          datetime,
          value_raw,
          value_corrected AS value,
          period,
          imputed
        FROM continuous.measurements_continuous_corrected($1, $2, $3)
        ORDER BY datetime
        ",
        params = list(as.integer(tsid), start_dt, end_dt)
      )
      data.table::setDT(res)
      if (nrow(res) == 0) {
        return(data.table::data.table(
          datetime = empty_posixct_utc(0),
          value_raw = numeric(),
          value = numeric(),
          period = character(),
          imputed = logical()
        ))
      }
      res[, datetime := as_posixct_utc(datetime)]
      res[, value := as.numeric(value)]
      res[, value_raw := as.numeric(value_raw)]
      res[, imputed := as.logical(imputed)]
      res[is.na(imputed), imputed := FALSE]
      res
    }

    fetch_series <- function(tsid, start_dt, end_dt) {
      fetch_series_for_con(
        session$userData$AquaCache,
        tsid,
        start_dt,
        end_dt
      )
    }

    prepare_target_data_for_con <- function(
      con,
      tsid,
      meta_row,
      start_dt,
      end_dt,
      reimpute_existing
    ) {
      meta_row <- data.table::as.data.table(meta_row)
      context_padding <- 50 * infer_step_seconds(NULL, meta_row)
      first_fetch_start <- start_dt - context_padding
      first_fetch_end <- end_dt + context_padding
      raw <- fetch_series_for_con(con, tsid, first_fetch_start, first_fetch_end)

      step <- infer_step_seconds(raw, meta_row)
      context_padding <- max(50 * step, 20 * step)
      context_start <- start_dt - context_padding
      context_end <- end_dt + context_padding
      raw <- fetch_series_for_con(con, tsid, context_start, context_end)

      origin <- if (nrow(raw) > 0) {
        raw$datetime[[1]]
      } else {
        as_posixct_utc(meta_row$start_datetime)[[1]]
      }
      if (is.na(origin)) {
        origin <- start_dt
      }

      grid <- make_datetime_grid(context_start, context_end, origin, step)
      full <- data.table::data.table(datetime = grid)
      full <- merge(full, raw, by = "datetime", all.x = TRUE)
      data.table::setorder(full, datetime)

      full[, source_imputed := as.logical(imputed)]
      full[is.na(source_imputed), source_imputed := FALSE]
      full[, imputed := FALSE]
      full[, value_existing := value]
      full[, in_impute_range := datetime >= start_dt & datetime <= end_dt]
      if (isTRUE(reimpute_existing)) {
        full[source_imputed == TRUE, value := NA_real_]
      }
      full[, missing_for_impute := is.na(value) & in_impute_range]
      list(data = full, step = step)
    }

    prepare_target_data <- function(start_dt, end_dt) {
      prepare_target_data_for_con(
        con = session$userData$AquaCache,
        tsid = selected_ts(),
        meta_row = selected_meta(),
        start_dt = start_dt,
        end_dt = end_dt,
        reimpute_existing = input$reimpute_existing
      )
    }

    haversine_km <- function(lat1, lon1, lat2, lon2) {
      if (any(is.na(c(lat1, lon1, lat2, lon2)))) {
        return(NA_real_)
      }
      rad <- pi / 180
      dlat <- (lat2 - lat1) * rad
      dlon <- (lon2 - lon1) * rad
      a <- sin(dlat / 2)^2 +
        cos(lat1 * rad) * cos(lat2 * rad) * sin(dlon / 2)^2
      6371 * 2 * atan2(sqrt(a), sqrt(1 - a))
    }

    aggregate_values <- function(values, aggregation) {
      values <- values[!is.na(values)]
      if (!length(values)) {
        return(NA_real_)
      }
      aggregation <- tolower(as.character(aggregation[[1]]))
      out <- switch(
        aggregation,
        "sum" = sum(values, na.rm = TRUE),
        "minimum" = min(values, na.rm = TRUE),
        "min" = min(values, na.rm = TRUE),
        "maximum" = max(values, na.rm = TRUE),
        "max" = max(values, na.rm = TRUE),
        "median" = stats::median(values, na.rm = TRUE),
        "(min+max)/2" = mean(c(
          min(values, na.rm = TRUE),
          max(values, na.rm = TRUE)
        )),
        mean(values, na.rm = TRUE)
      )
      finite_to_na(out)
    }

    summarize_reference_to_grid <- function(
      ref,
      grid_datetimes,
      step_seconds,
      aggregation
    ) {
      grid <- data.table::data.table(datetime = grid_datetimes)
      if (nrow(grid) == 0) {
        return(data.table::data.table(
          datetime = empty_posixct_utc(0),
          reference_value = numeric()
        ))
      }

      out <- data.table::copy(grid)
      out[, reference_value := NA_real_]
      if (is.null(ref) || nrow(ref) == 0) {
        return(out)
      }

      ref <- data.table::as.data.table(ref)
      ref <- ref[!is.na(value), .(datetime, value = as.numeric(value))]
      if (nrow(ref) == 0) {
        return(out)
      }

      ref_interval <- ref[, .(
        ref_start = as.numeric(datetime),
        ref_end = as.numeric(datetime),
        value
      )]
      grid_interval <- grid[, .(
        datetime,
        grid_start = as.numeric(datetime) - step_seconds + 1e-6,
        grid_end = as.numeric(datetime)
      )]
      data.table::setkey(ref_interval, ref_start, ref_end)
      data.table::setkey(grid_interval, grid_start, grid_end)
      joined <- data.table::foverlaps(
        ref_interval,
        grid_interval,
        by.x = c("ref_start", "ref_end"),
        by.y = c("grid_start", "grid_end"),
        type = "within",
        nomatch = 0L
      )
      if (nrow(joined) == 0) {
        return(out)
      }

      summary <- joined[, .(
        reference_value = aggregate_values(value, aggregation)
      ), by = datetime]
      out <- merge(out[, .(datetime)], summary, by = "datetime", all.x = TRUE)
      data.table::setorder(out, datetime)
      out
    }

    direct_candidate_metadata_for_meta <- function(
      meta,
      target,
      tsid,
      radius,
      extra_params = NULL
    ) {
      meta <- data.table::as.data.table(meta)
      target <- data.table::as.data.table(target)
      wanted_params <- unique(c(target$parameter[[1]], extra_params))

      meta <- meta[
        timeseries_id != tsid &
          parameter %in% wanted_params
      ]
      if (nrow(meta) == 0) {
        return(meta)
      }

      meta[, distance_km := mapply(
        haversine_km,
        target$latitude[[1]],
        target$longitude[[1]],
        latitude,
        longitude
      )]
      radius <- suppressWarnings(as.numeric(radius))
      if (!length(radius) || is.na(radius) || radius < 0) {
        radius <- 0
      }
      meta <- meta[!is.na(distance_km) & distance_km <= radius]
      if (nrow(meta) == 0) {
        return(meta)
      }

      target_step <- interval_seconds(target$recording_rate)
      if (!is.na(target_step) && target_step > 0) {
        meta[, candidate_step := vapply(
          recording_rate,
          interval_seconds,
          numeric(1)
        )]
        meta <- meta[
          is.na(candidate_step) |
            candidate_step <= target_step
        ]
      }
      data.table::setorder(meta, distance_km)
      meta
    }

    direct_candidate_metadata <- function() {
      req(selected_ts(), selected_meta())
      direct_candidate_metadata_for_meta(
        meta = ts_meta(),
        target = selected_meta(),
        tsid = selected_ts(),
        radius = input$radius,
        extra_params = input$extra_params
      )
    }

    score_direct_candidates_for_con <- function(
      con,
      target_dat,
      meta,
      target,
      tsid,
      radius,
      extra_params,
      step,
      min_gap,
      max_gap
    ) {
      target_dat <- data.table::as.data.table(target_dat)
      if (is.null(target_dat) || nrow(target_dat) == 0) {
        return(list(
          table = data.table::data.table(),
          reference = list()
        ))
      }

      fill_idx <- eligible_missing_indices(
        is.na(target_dat$value) & target_dat$in_impute_range,
        min_gap,
        max_gap
      )
      if (!length(fill_idx)) {
        return(list(
          table = data.table::data.table(),
          reference = list()
        ))
      }

      candidate_meta <- direct_candidate_metadata_for_meta(
        meta = meta,
        target = target,
        tsid = tsid,
        radius = radius,
        extra_params = extra_params
      )
      if (nrow(candidate_meta) == 0) {
        return(list(
          table = data.table::data.table(),
          reference = list()
        ))
      }

      target <- data.table::as.data.table(target)
      target_aggregation <- target$aggregation[[1]]
      context_start <- min(target_dat$datetime)
      context_end <- max(target_dat$datetime)
      rows <- list()
      refs <- list()

      for (i in seq_len(nrow(candidate_meta))) {
        candidate <- candidate_meta[i]
        ref <- fetch_series_for_con(
          con,
          candidate$timeseries_id[[1]],
          context_start,
          context_end
        )
        if (nrow(ref) == 0) {
          next
        }

        reference_grid <- summarize_reference_to_grid(
          ref = ref,
          grid_datetimes = target_dat$datetime,
          step_seconds = step,
          aggregation = target_aggregation
        )
        merged <- merge(
          target_dat[, .(datetime, target_value = value)],
          reference_grid,
          by = "datetime",
          all.x = TRUE
        )
        overlap <- !is.na(merged$target_value) &
          !is.na(merged$reference_value)
        if (!any(overlap)) {
          next
        }

        usable <- !is.na(reference_grid$reference_value[fill_idx])
        if (!any(usable)) {
          next
        }

        offset <- mean(
          merged$target_value[overlap] - merged$reference_value[overlap],
          na.rm = TRUE
        )
        sd_offset <- stats::sd(
          merged$target_value[overlap] - merged$reference_value[overlap],
          na.rm = TRUE
        )
        missing_candidate <- sum(is.na(reference_grid$reference_value[fill_idx]))

        rows[[length(rows) + 1L]] <- data.table::data.table(
          timeseries_id = candidate$timeseries_id[[1]],
          location = candidate$location[[1]],
          parameter = candidate$parameter[[1]],
          aggregation = candidate$aggregation[[1]],
          recording_rate = as.character(candidate$recording_rate[[1]]),
          distance_km = round(candidate$distance_km[[1]], 3),
          offset = round(offset, 5),
          sd_offset = round(sd_offset, 5),
          overlap_points = sum(overlap),
          imputed_points_available = sum(usable),
          imputed_points_missing = missing_candidate,
          missing_data_for_impute = missing_candidate > 0
        )
        refs[[as.character(candidate$timeseries_id[[1]])]] <- reference_grid
      }

      if (!length(rows)) {
        return(list(
          table = data.table::data.table(),
          reference = list()
        ))
      }

      table <- data.table::rbindlist(rows, fill = TRUE)
      data.table::setorder(
        table,
        missing_data_for_impute,
        distance_km,
        sd_offset
      )
      list(table = table, reference = refs)
    }

    score_direct_candidates <- function() {
      limits <- gap_limits()
      score_direct_candidates_for_con(
        con = session$userData$AquaCache,
        target_dat = full_data(),
        meta = ts_meta(),
        target = selected_meta(),
        tsid = selected_ts(),
        radius = input$radius,
        extra_params = input$extra_params,
        step = loaded_step(),
        min_gap = limits$min_gap,
        max_gap = limits$max_gap
      )
    }

    task_signature <- function(...) {
      paste(vapply(list(...), function(value) {
        if (is.null(value)) {
          return("")
        }
        paste(as.character(value), collapse = "|")
      }, character(1)), collapse = "\r")
    }

    load_task_signature <- reactiveVal(NULL)
    candidate_task_signature <- reactiveVal(NULL)
    impute_task_signature <- reactiveVal(NULL)
    commit_task_signature <- reactiveVal(NULL)

    load_task <- ExtendedTask$new(function(req) {
      promises::future_promise({
        tryCatch(
          {
            con <- connect_from_config(req$config)
            on.exit(DBI::dbDisconnect(con), add = TRUE)

            prepared <- prepare_target_data_for_con(
              con = con,
              tsid = req$timeseries_id,
              meta_row = req$target_meta,
              start_dt = req$start_dt,
              end_dt = req$end_dt,
              reimpute_existing = req$reimpute_existing
            )
            gaps <- build_gap_table(
              prepared$data,
              req$min_gap,
              req$max_gap
            )

            scored <- list(
              table = data.table::data.table(),
              reference = list()
            )
            if (identical(req$method, "direct")) {
              scored <- score_direct_candidates_for_con(
                con = con,
                target_dat = prepared$data,
                meta = req$timeseries_meta,
                target = req$target_meta,
                tsid = req$timeseries_id,
                radius = req$radius,
                extra_params = req$extra_params,
                step = prepared$step,
                min_gap = req$min_gap,
                max_gap = req$max_gap
              )
            }

            list(
              ok = TRUE,
              signature = req$signature,
              full_data = prepared$data,
              step = prepared$step,
              range = c(req$start_dt, req$end_dt),
              gaps = gaps,
              candidates = scored$table,
              candidate_reference_data = scored$reference,
              missing_count = sum(prepared$data$missing_for_impute)
            )
          },
          error = function(e) {
            list(
              ok = FALSE,
              signature = req$signature,
              message = conditionMessage(e)
            )
          }
        )
      })
    }) |>
      bslib::bind_task_button("load")

    candidate_task <- ExtendedTask$new(function(req) {
      promises::future_promise({
        tryCatch(
          {
            con <- connect_from_config(req$config)
            on.exit(DBI::dbDisconnect(con), add = TRUE)
            scored <- score_direct_candidates_for_con(
              con = con,
              target_dat = req$full_data,
              meta = req$timeseries_meta,
              target = req$target_meta,
              tsid = req$timeseries_id,
              radius = req$radius,
              extra_params = req$extra_params,
              step = req$step,
              min_gap = req$min_gap,
              max_gap = req$max_gap
            )
            list(
              ok = TRUE,
              signature = req$signature,
              candidates = scored$table,
              candidate_reference_data = scored$reference
            )
          },
          error = function(e) {
            list(
              ok = FALSE,
              signature = req$signature,
              message = conditionMessage(e)
            )
          }
        )
      })
    })

    invoke_candidate_task <- function() {
      if (!identical(input$method, "direct") || is.null(full_data())) {
        candidates(NULL)
        candidate_reference_data(NULL)
        return(invisible(NULL))
      }

      limits <- gap_limits()
      signature <- task_signature(
        selected_ts(),
        loaded_range(),
        input$radius,
        input$extra_params,
        limits$min_gap,
        limits$max_gap
      )
      candidate_task_signature(signature)
      candidate_task$invoke(list(
        signature = signature,
        config = session$userData$config,
        timeseries_id = selected_ts(),
        timeseries_meta = as.data.frame(ts_meta()),
        target_meta = as.data.frame(selected_meta()),
        full_data = as.data.frame(full_data()),
        step = loaded_step(),
        radius = input$radius,
        extra_params = input$extra_params,
        min_gap = limits$min_gap,
        max_gap = limits$max_gap
      ))
      invisible(NULL)
    }

    observeEvent(load_task$result(), {
      result <- load_task$result()
      if (!identical(result$signature, load_task_signature())) {
        return()
      }
      if (!isTRUE(result$ok)) {
        full_data(NULL)
        gap_data(NULL)
        imputed_data(NULL)
        candidates(NULL)
        candidate_reference_data(NULL)
        showNotification(result$message, type = "error")
        return()
      }

      full_data(data.table::as.data.table(result$full_data))
      loaded_step(result$step)
      loaded_range(result$range)
      gap_data(data.table::as.data.table(result$gaps))
      imputed_data(NULL)
      candidates(data.table::as.data.table(result$candidates))
      candidate_reference_data(result$candidate_reference_data)

      if (!isTRUE(result$missing_count > 0)) {
        showNotification(
          "No missing values were found in the selected range.",
          type = "warning"
        )
      }
    })

    observeEvent(candidate_task$result(), {
      result <- candidate_task$result()
      if (!identical(result$signature, candidate_task_signature())) {
        return()
      }
      if (!isTRUE(result$ok)) {
        candidates(NULL)
        candidate_reference_data(NULL)
        showNotification(result$message, type = "error")
        return()
      }
      candidates(data.table::as.data.table(result$candidates))
      candidate_reference_data(result$candidate_reference_data)
    })

    update_direct_candidates <- function() {
      invoke_candidate_task()
    }

    observeEvent(
      list(input$radius, input$extra_params, input$method),
      {
        imputed_data(NULL)
        update_direct_candidates()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      list(input$min_gap, input$max_gap),
      {
        req(full_data())
        limits <- gap_limits()
        gap_data(build_gap_table(
          full_data(),
          limits$min_gap,
          limits$max_gap
        ))
        imputed_data(NULL)
        update_direct_candidates()
      },
      ignoreInit = TRUE
    )

    output$load_summary <- renderUI({
      req(full_data())
      dat <- full_data()
      gaps <- gap_data()
      selected <- selected_meta()
      in_range <- dat[in_impute_range == TRUE]
      missing_count <- sum(is.na(in_range$value))
      eligible_count <- if (is.null(gaps) || nrow(gaps) == 0) {
        0L
      } else {
        sum(gaps[impute == TRUE]$missing_points)
      }

      tags$div(
        class = "mb-3",
        tags$strong(paste0(
          selected$location[[1]],
          " | ",
          selected$parameter[[1]],
          " | timeseries_id ",
          selected_ts()
        )),
        tags$br(),
        sprintf(
          "%s expected points in range; %s missing; %s eligible for the current gap limits.",
          nrow(in_range),
          missing_count,
          eligible_count
        )
      )
    })

    output$gaps <- DT::renderDT({
      req(gap_data())
      gaps <- gap_data()
      if (nrow(gaps) == 0) {
        gaps <- data.table::data.table(
          message = "No missing values in the selected range."
        )
      }
      DT::datatable(
        gaps,
        rownames = FALSE,
        selection = "none",
        options = list(scrollX = TRUE, pageLength = 8)
      )
    })

    output$direct_impute_selection <- renderText({
      req(input$method == "direct")
      if (is.null(candidates()) || nrow(candidates()) == 0) {
        return("No suitable nearby timeseries were found for the current radius, parameters, and gap limits.")
      }
      "Select a candidate timeseries below for direct imputation."
    })

    output$candidates <- DT::renderDT({
      req(input$method == "direct")
      req(candidates())
      DT::datatable(
        candidates(),
        selection = "single",
        rownames = FALSE,
        filter = "top",
        options = list(scrollX = TRUE, pageLength = 8)
      )
    })

    perform_impute_for_limits <- function(
      dat,
      method,
      min_gap,
      max_gap,
      reference = NULL,
      offset = NULL
    ) {
      dat <- data.table::as.data.table(dat)
      out <- data.table::copy(dat)
      out[, imputed := FALSE]
      fill_idx <- eligible_missing_indices(
        is.na(out$value) & out$in_impute_range,
        min_gap,
        max_gap
      )
      if (!length(fill_idx)) {
        return(out)
      }

      if (identical(method, "direct")) {
        if (is.null(reference) || is.null(offset) || nrow(reference) == 0) {
          return(NULL)
        }
        reference <- data.table::as.data.table(reference)
        ref <- reference[match(out$datetime, reference$datetime)]
        values <- ref$reference_value + offset
        usable_idx <- fill_idx[!is.na(values[fill_idx])]
        if (!length(usable_idx)) {
          return(NULL)
        }
        out$value[usable_idx] <- values[usable_idx]
        out$imputed[usable_idx] <- TRUE
        return(out)
      }

      available <- which(!is.na(out$value))
      if (identical(method, "linear") && length(available) < 2) {
        return(NULL)
      }
      if (identical(method, "spline") && length(available) < 3) {
        return(NULL)
      }

      x <- as.numeric(out$datetime[available])
      y <- out$value[available]
      xout <- as.numeric(out$datetime)
      if (identical(method, "linear")) {
        interp <- stats::approx(x = x, y = y, xout = xout, rule = 1)$y
      } else {
        interp <- stats::spline(
          x = x,
          y = y,
          xout = xout,
          method = "natural"
        )$y
        interp[xout < min(x) | xout > max(x)] <- NA_real_
      }

      usable_idx <- fill_idx[!is.na(interp[fill_idx])]
      if (!length(usable_idx)) {
        return(NULL)
      }
      out$value[usable_idx] <- interp[usable_idx]
      out$imputed[usable_idx] <- TRUE
      out
    }

    perform_impute <- function(dat, method, reference = NULL, offset = NULL) {
      limits <- gap_limits()
      perform_impute_for_limits(
        dat = dat,
        method = method,
        min_gap = limits$min_gap,
        max_gap = limits$max_gap,
        reference = reference,
        offset = offset
      )
    }

    impute_task <- ExtendedTask$new(function(req) {
      promises::future_promise({
        tryCatch(
          {
            imp <- perform_impute_for_limits(
              dat = req$full_data,
              method = req$method,
              min_gap = req$min_gap,
              max_gap = req$max_gap,
              reference = req$reference,
              offset = req$offset
            )
            if (is.null(imp)) {
              message <- switch(
                req$method,
                direct = "The selected candidate cannot fill any eligible missing values.",
                linear = "Linear interpolation could not fill any eligible points. It needs existing measurements bracketing the gaps.",
                spline = "Spline interpolation could not fill any eligible points. It needs at least three existing measurements bracketing the gaps.",
                "The selected imputation method could not fill any eligible missing values."
              )
              return(list(
                ok = FALSE,
                signature = req$signature,
                message = message
              ))
            }
            list(
              ok = TRUE,
              signature = req$signature,
              imputed_data = imp,
              imputed_count = sum(imp$imputed)
            )
          },
          error = function(e) {
            list(
              ok = FALSE,
              signature = req$signature,
              message = conditionMessage(e)
            )
          }
        )
      })
    }) |>
      bslib::bind_task_button("impute")

    commit_task <- ExtendedTask$new(function(req) {
      promises::future_promise({
        tryCatch(
          {
            con <- connect_from_config(req$config)
            on.exit(DBI::dbDisconnect(con), add = TRUE)

            allowed <- DBI::dbGetQuery(
              con,
              "
              SELECT
                has_table_privilege(
                  current_user,
                  'continuous.measurements_continuous',
                  'INSERT'
                ) AS can_insert,
                has_table_privilege(
                  current_user,
                  'continuous.measurements_continuous',
                  'DELETE'
                ) AS can_delete
              "
            )
            if (
              !isTRUE(allowed$can_insert[[1]]) ||
                !isTRUE(allowed$can_delete[[1]])
            ) {
              stop(
                "This database user cannot insert and delete continuous measurements.",
                call. = FALSE
              )
            }

            if (!identical(req$timeseries_type_code, "basic")) {
              stop(
                "Imputed values can only be committed to basic timeseries.",
                call. = FALSE
              )
            }

            withCallingHandlers(
              AquaCache::addNewContinuous(
                tsid = req$timeseries_id,
                df = req$data,
                con = con,
                target = "realtime",
                overwrite = "conflict"
              ),
              warning = function(w) {
                stop(conditionMessage(w), call. = FALSE)
              }
            )
            list(
              ok = TRUE,
              signature = req$signature,
              saved_count = nrow(req$data)
            )
          },
          error = function(e) {
            list(
              ok = FALSE,
              signature = req$signature,
              message = conditionMessage(e)
            )
          }
        )
      })
    }) |>
      bslib::bind_task_button("commit")

    observeEvent(input$load, {
      req(selected_ts())
      datetime_range_utc <- coerce_utc_datetime(input$dt_range_impute)
      if (length(datetime_range_utc) < 2) {
        showNotification(
          "Please provide valid start and end datetimes.",
          type = "error"
        )
        return()
      }
      start_dt <- datetime_range_utc[[1]]
      end_dt <- datetime_range_utc[[2]]
      if (is.na(start_dt) || is.na(end_dt)) {
        showNotification(
          "Please provide valid start and end datetimes.",
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

      limits <- gap_limits()
      signature <- task_signature(
        selected_ts(),
        start_dt,
        end_dt,
        input$method,
        input$radius,
        input$extra_params,
        limits$min_gap,
        limits$max_gap,
        input$reimpute_existing
      )
      load_task_signature(signature)
      full_data(NULL)
      gap_data(NULL)
      imputed_data(NULL)
      candidates(NULL)
      candidate_reference_data(NULL)
      load_task$invoke(list(
        signature = signature,
        config = session$userData$config,
        timeseries_id = selected_ts(),
        timeseries_meta = as.data.frame(ts_meta()),
        target_meta = as.data.frame(selected_meta()),
        start_dt = start_dt,
        end_dt = end_dt,
        method = input$method,
        radius = input$radius,
        extra_params = input$extra_params,
        reimpute_existing = input$reimpute_existing,
        min_gap = limits$min_gap,
        max_gap = limits$max_gap
      ))
    })

    observeEvent(input$impute, {
      req(full_data())
      method <- input$method
      reference <- NULL
      offset <- NULL
      selected_candidate_id <- NULL

      if (identical(method, "direct")) {
        cand <- candidates()
        refs <- candidate_reference_data()
        sel <- input$candidates_rows_selected
        if (is.null(cand) || nrow(cand) == 0 || length(sel) != 1) {
          showNotification(
            "Select a candidate timeseries for direct imputation.",
            type = "error"
          )
          return()
        }
        if (sel > nrow(cand)) {
          showNotification(
            "Unable to determine the selected candidate timeseries.",
            type = "error"
          )
          return()
        }
        selected_candidate <- cand[sel]
        selected_candidate_id <- selected_candidate$timeseries_id[[1]]
        key <- as.character(selected_candidate_id)
        reference <- refs[[key]]
        if (is.null(reference) || nrow(reference) == 0) {
          showNotification(
            "Unable to load the selected candidate reference data.",
            type = "error"
          )
          return()
        }
        offset <- selected_candidate$offset[[1]]
      }

      limits <- gap_limits()
      signature <- task_signature(
        load_task_signature(),
        selected_ts(),
        method,
        selected_candidate_id,
        offset,
        limits$min_gap,
        limits$max_gap
      )
      impute_task_signature(signature)
      imputed_data(NULL)
      impute_task$invoke(list(
        signature = signature,
        full_data = as.data.frame(full_data()),
        method = method,
        min_gap = limits$min_gap,
        max_gap = limits$max_gap,
        reference = if (is.null(reference)) NULL else as.data.frame(reference),
        offset = offset
      ))
    })

    observeEvent(impute_task$result(), {
      result <- impute_task$result()
      if (!identical(result$signature, impute_task_signature())) {
        return()
      }
      if (!isTRUE(result$ok)) {
        showNotification(result$message, type = "error")
        return()
      }

      imp <- data.table::as.data.table(result$imputed_data)
      if (!any(imp$imputed)) {
        showNotification(
          "No missing values met the current imputation criteria.",
          type = "warning"
        )
      } else {
        showNotification(
          sprintf("Prepared %s imputed values.", sum(imp$imputed)),
          type = "message"
        )
      }
      imputed_data(imp)
    })

    output$plot <- plotly::renderPlotly({
      req(full_data())
      dat <- if (!is.null(imputed_data())) imputed_data() else full_data()
      dt_range <- loaded_range()
      step <- loaded_step()
      plot_start <- dt_range[[1]] - 5 * step
      plot_end <- dt_range[[2]] + 5 * step
      dat <- dat[datetime >= plot_start & datetime <= plot_end]

      y_values <- dat$value
      if ("value_existing" %in% names(dat)) {
        y_values <- c(y_values, dat$value_existing)
      }
      y_values <- y_values[!is.na(y_values)]
      if (!length(y_values)) {
        y_values <- c(0, 1)
      }
      y_span <- diff(range(y_values, na.rm = TRUE))
      if (!is.finite(y_span) || y_span == 0) {
        y_span <- 1
      }
      y_min <- min(y_values, na.rm = TRUE) - 0.05 * y_span
      y_max <- max(y_values, na.rm = TRUE) + 0.05 * y_span

      existing <- data.table::copy(dat)
      existing[imputed == TRUE, value := NA_real_]
      imputed_only <- data.table::copy(dat)
      imputed_only[imputed != TRUE, value := NA_real_]

      gaps <- gap_data()
      shapes <- list()
      if (!is.null(gaps) && nrow(gaps) > 0) {
        gaps <- gaps[
          end_datetime >= plot_start &
            start_datetime <= plot_end
        ]
        for (i in seq_len(nrow(gaps))) {
          shapes[[length(shapes) + 1L]] <- list(
            type = "rect",
            xref = "x",
            yref = "y",
            x0 = gaps$start_datetime[[i]],
            x1 = gaps$end_datetime[[i]],
            y0 = y_min,
            y1 = y_max,
            fillcolor = if (isTRUE(gaps$impute[[i]])) {
              "rgba(213, 94, 0, 0.15)"
            } else {
              "rgba(0, 114, 178, 0.10)"
            },
            line = list(width = 0),
            layer = "below"
          )
        }
      }

      p <- plotly::plot_ly() |>
        plotly::add_lines(
          data = existing,
          x = ~datetime,
          y = ~value,
          name = "Existing",
          line = list(color = "#0072B2")
        ) |>
        plotly::add_markers(
          data = existing,
          x = ~datetime,
          y = ~value,
          name = "Existing",
          marker = list(color = "#0072B2", size = 4),
          showlegend = FALSE
        )

      if (!is.null(imputed_data()) && any(dat$imputed)) {
        p <- p |>
          plotly::add_lines(
            data = imputed_only,
            x = ~datetime,
            y = ~value,
            name = "Imputed",
            line = list(color = "#D55E00")
          ) |>
          plotly::add_markers(
            data = imputed_only,
            x = ~datetime,
            y = ~value,
            name = "Imputed",
            marker = list(color = "#D55E00", size = 6)
          )
      }

      p |>
        plotly::layout(
          xaxis = list(title = "Datetime"),
          yaxis = list(title = "Value", range = c(y_min, y_max)),
          shapes = shapes
        )
    })

    output$imputed_values <- DT::renderDT({
      req(imputed_data())
      out <- imputed_data()[
        imputed == TRUE,
        .(datetime, value)
      ]
      DT::datatable(
        out,
        rownames = FALSE,
        selection = "none",
        options = list(scrollX = TRUE, pageLength = 10)
      )
    })

    observeEvent(input$commit, {
      req(imputed_data(), selected_ts())
      if (!isTRUE(commit_allowed())) {
        showNotification(
          "This database user cannot insert and delete continuous measurements.",
          type = "error"
        )
        return()
      }
      meta_row <- selected_meta()
      if (!identical(meta_row$timeseries_type_code[[1]], "basic")) {
        showNotification(
          "Imputed values can only be committed to basic timeseries.",
          type = "error"
        )
        return()
      }

      df <- imputed_data()
      to_push <- df[
        imputed == TRUE,
        .(datetime, value, imputed)
      ]
      if (nrow(to_push) == 0) {
        showNotification(
          "There are no imputed values to commit.",
          type = "error"
        )
        return()
      }

      signature <- task_signature(
        impute_task_signature(),
        selected_ts(),
        nrow(to_push),
        min(to_push$datetime),
        max(to_push$datetime)
      )
      commit_task_signature(signature)
      commit_task$invoke(list(
        signature = signature,
        config = session$userData$config,
        timeseries_id = selected_ts(),
        timeseries_type_code = meta_row$timeseries_type_code[[1]],
        data = as.data.frame(to_push)
      ))
    })

    observeEvent(commit_task$result(), {
      result <- commit_task$result()
      if (!identical(result$signature, commit_task_signature())) {
        return()
      }
      if (!isTRUE(result$ok)) {
        showNotification(
          paste("Commit failed:", result$message),
          type = "error"
        )
        return()
      }

      showNotification(
        sprintf(
          "%s imputed values were saved to the database.",
          result$saved_count
        ),
        type = "message"
      )
      full_data(NULL)
      gap_data(NULL)
      candidates(NULL)
      candidate_reference_data(NULL)
      imputed_data(NULL)
    })
  })
}
