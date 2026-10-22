params_map_daily_stats_select_sql <- function(
  con,
  stats_period = "30yr",
  columns = c("percent_historic_range", "max", "min", "doy_count"),
  table_alias = "d"
) {
  if (!(stats_period %in% c("30yr", "full"))) {
    stats_period <- "30yr"
  }

  available <- DBI::dbGetQuery(
    con,
    "SELECT column_name
     FROM information_schema.columns
     WHERE table_schema = 'continuous'
       AND table_name = 'measurements_calculated_daily'"
  )$column_name

  suffix <- ""
  if (
    identical(stats_period, "30yr") &&
      all(paste0(columns, "_30yr") %in% available)
  ) {
    suffix <- "_30yr"
  }

  out <- vapply(
    columns,
    function(column) {
      source_column <- paste0(column, suffix)
      paste0(
        table_alias,
        ".",
        DBI::dbQuoteIdentifier(con, source_column),
        " AS ",
        DBI::dbQuoteIdentifier(con, column)
      )
    },
    character(1)
  )
  attr(out, "count_column") <- paste0(table_alias, ".\"doy_count", suffix, "\"")
  attr(out, "stats_period") <- if (nzchar(suffix)) "30yr" else "full"
  out
}

# Read measurements on a worker-owned connection. Only plain snapshots cross
# the process boundary; neither a DBI connection nor reactiveValues may do so.
# The same implementation handles both parameters, with secondary values used
# only at locations without an eligible primary value.
params_map_fetch_data <- function(con, request, stats_sql_fun) {
  include_history <- !identical(request$include_history, FALSE)
  stats_sql <- if (include_history) {
    stats_sql_fun(con, request$stats_period)
  } else {
    character()
  }
  count_sql <- attr(stats_sql, "count_column")
  # Absolute maps have no historical prerequisites, including for new series.
  history_sql <- if (include_history) {
    paste(stats_sql[c("max", "min", "doy_count")], collapse = ", ")
  }
  timeseries <- data.table::as.data.table(request$timeseries)
  locations <- data.table::as.data.table(request$locations)
  collected <- list()
  covered <- integer()

  for (selection in request$selections) {
    candidates <- timeseries[
      timeseries$parameter_id == selection$parameter_id &
        !timeseries$location_id %in% covered
    ]
    ids <- unique(as.integer(candidates$timeseries_id))
    ids <- ids[!is.na(ids) & ids > 0L]
    if (!length(ids)) {
      next
    }
    ids_sql <- paste(ids, collapse = ",")

    if (request$latest) {
      # Date tolerance uses UTC calendar days, including today when days = 0.
      # Select the indexed newest basic row BEFORE applying its correction.
      # Compound series still use the canonical corrected resolver so derived
      # series, member precedence, and corrections retain database semantics.
      from <- as.POSIXct(
        as.Date(request$now, tz = "UTC") - selection$days,
        tz = "UTC"
      )
      # The database's normalized calendar excludes February 29. Match its own
      # daily rows explicitly when the requested window can contain that date.
      leap_sql <- if (
        include_history &&
          any(
            format(
              seq(as.Date(from), as.Date(request$now, tz = "UTC"), by = "day"),
              "%m-%d"
            ) ==
              "02-29"
          )
      ) {
        " OR (continuous.normalized_day_of_year((latest.datetime AT TIME ZONE 'UTC')::date) IS NULL
                AND EXTRACT(MONTH FROM d.date) = 2 AND EXTRACT(DAY FROM d.date) = 29)"
      } else {
        ""
      }
      sql <- paste0(
        "WITH requested AS MATERIALIZED (
           SELECT timeseries_id, timeseries_type,
                  EXTRACT(EPOCH FROM record_rate)::double precision AS record_rate_seconds,
                  sensor_priority
           FROM continuous.timeseries
           WHERE timeseries_id IN (",
        ids_sql,
        ")
             AND start_datetime <= $2::timestamptz
             AND end_datetime >= $1::timestamptz
         ), latest AS MATERIALIZED (
           SELECT t.timeseries_id, t.record_rate_seconds, t.sensor_priority, m.datetime,
                  continuous.apply_corrections(t.timeseries_id, m.datetime, m.value) AS value
           FROM requested t
           CROSS JOIN LATERAL (
             SELECT datetime, value
             FROM continuous.measurements_continuous
             WHERE timeseries_id = t.timeseries_id
               AND datetime BETWEEN $1::timestamptz AND $2::timestamptz
               AND value IS NOT NULL
             ORDER BY datetime DESC LIMIT 1
           ) m
           WHERE t.timeseries_type = 'basic'
           UNION ALL
           SELECT t.timeseries_id, t.record_rate_seconds, t.sensor_priority,
                  m.datetime, m.value_corrected AS value
           FROM requested t
           CROSS JOIN LATERAL (
             SELECT datetime, value_corrected
             FROM continuous.measurements_continuous_corrected(
               t.timeseries_id, $1::timestamptz, $2::timestamptz
             )
             WHERE value_corrected IS NOT NULL
             ORDER BY datetime DESC LIMIT 1
           ) m
           WHERE t.timeseries_type <> 'basic'
         )
         SELECT latest.*, (latest.datetime AT TIME ZONE 'UTC')::date AS date"
      )
      params <- list(from, request$now)
      if (include_history) {
        sql <- paste0(
          sql,
          ", history.max, history.min, history.doy_count
         FROM latest
         CROSS JOIN LATERAL (
           SELECT ",
          history_sql,
          "
           FROM continuous.measurements_calculated_daily d
           WHERE d.timeseries_id = latest.timeseries_id
             AND (continuous.normalized_day_of_year(d.date) =
                 continuous.normalized_day_of_year((latest.datetime AT TIME ZONE 'UTC')::date)",
          leap_sql,
          ")
             AND d.date <= (latest.datetime AT TIME ZONE 'UTC')::date
           ORDER BY d.date DESC LIMIT 1
         ) history
         WHERE history.doy_count >= $3::integer"
        )
        params[[3L]] <- selection$years
      } else {
        sql <- paste0(sql, " FROM latest")
      }
      rows <- data.table::as.data.table(DBI::dbGetQuery(
        con,
        sql,
        params = params
      ))
      if (include_history) {
        rows[,
          percent_historic_range := data.table::fifelse(
            max > min,
            100 * (value - min) / (max - min),
            NA_real_
          )
        ]
      }
    } else {
      # Bounded index lookups avoid sorting an entire timeseries history.
      # A date tie prefers the more recent daily mean, deterministically.
      sql <- paste0(
        "SELECT t.timeseries_id, daily.*
         FROM continuous.timeseries t
         CROSS JOIN LATERAL (
           SELECT d.date, d.value",
        if (include_history) paste0(", ", paste(stats_sql, collapse = ", ")),
        "
           FROM continuous.measurements_calculated_daily d
           WHERE d.timeseries_id = t.timeseries_id
             AND d.date BETWEEN $1::date - $2::integer AND $1::date + $2::integer
             ",
        if (include_history) paste0("AND ", count_sql, " >= $3::integer"),
        "
             AND d.value IS NOT NULL
           ORDER BY ABS(d.date - $1::date), d.date DESC LIMIT 1
         ) daily
         WHERE t.timeseries_id IN (",
        ids_sql,
        ")"
      )
      params <- list(request$target, selection$days)
      if (include_history) {
        params[[3L]] <- selection$years
      }
      rows <- data.table::as.data.table(DBI::dbGetQuery(
        con,
        sql,
        params = params
      ))
    }
    if (!nrow(rows)) {
      next
    }

    rows <- merge(rows, candidates, by = "timeseries_id")
    rows <- merge(rows, locations, by = "location_id")
    rows <- rows[
      is.finite(rows$value) &
        is.finite(rows$latitude) &
        is.finite(rows$longitude) &
        abs(rows$latitude) <= 90 &
        abs(rows$longitude) <= 180
    ]
    if (!nrow(rows)) {
      next
    }
    if (request$latest) {
      # Different recording intervals otherwise draw overlapping markers at
      # the same station. Prefer the most frequent eligible measurement, not
      # whichever series happens to be drawn last. Read interval/priority from
      # the live query so older metadata caches cannot change this selection.
      # Unknown/non-positive intervals rank after known intervals; priority,
      # newest timestamp and stable ID resolve ties without changing filters.
      rows[, `:=`(
        rate_rank = data.table::fifelse(
          is.finite(record_rate_seconds) & record_rate_seconds > 0,
          record_rate_seconds,
          Inf
        ),
        priority_rank = data.table::fifelse(
          is.finite(sensor_priority) & sensor_priority > 0,
          as.numeric(sensor_priority),
          Inf
        )
      )]
      data.table::setorderv(
        rows,
        c(
          "location_id",
          "parameter_id",
          "rate_rank",
          "priority_rank",
          "datetime",
          "timeseries_id"
        ),
        c(1L, 1L, 1L, 1L, -1L, 1L),
        na.last = TRUE
      )
      rows <- unique(rows, by = c("location_id", "parameter_id"))
      rows[, c("rate_rank", "priority_rank") := NULL]
    }
    if (include_history) {
      rows[
        !is.finite(rows$percent_historic_range),
        percent_historic_range := NA_real_
      ]
    }
    collected[[length(collected) + 1L]] <- rows
    covered <- unique(c(covered, rows$location_id))
  }
  list(
    data = data.table::rbindlist(collected, fill = TRUE),
    stats_period = attr(stats_sql, "stats_period"),
    latest = request$latest
  )
}

# Use AquaLink's application-wide mirai pool. Transport these sourced functions
# explicitly so a worker cannot silently use an older installed module copy.
params_map_start_fetch <- function(request, db, fetch_fun, stats_sql_fun) {
  mirai::mirai(
    {
      con <- DBI::dbConnect(
        RPostgres::Postgres(),
        dbname = db$dbName,
        host = db$dbHost,
        port = db$dbPort,
        user = db$dbUser,
        password = db$dbPass,
        connect_timeout = 5,
        options = "-c default_transaction_read_only=on -c statement_timeout=15000 -c timezone=UTC"
      )
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      fetch_fun(con, request, stats_sql_fun)
    },
    request = request,
    db = db,
    fetch_fun = fetch_fun,
    stats_sql_fun = stats_sql_fun,
    .timeout = 60000,
    .compute = "YGwater_aqualink"
  )
}

mapParamsUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(
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
        ns("accordion")
      ))
    ),
    # All UI elements rendered in server function to allow multi-language functionality
    uiOutput(ns("banner")),
    uiOutput(ns("top")),
    # This module is embedded in an existing page. A full page_sidebar() adds
    # page-level sizing/theme rules and clips Leaflet controls on narrow views.
    layout_sidebar(
      border = FALSE,
      border_radius = FALSE,
      sidebar = sidebar(
        title = NULL,
        bg = config$sidebar_bg,
        open = list(mobile = "always-above"),
        uiOutput(ns("sidebar_controls"))
      ),
      tagList(
        tags$div(
          role = "status",
          `aria-live` = "polite",
          uiOutput(ns("map_status"))
        ),
        leaflet::leafletOutput(ns("map"), height = "80vh")
      )
    )
  )
} # End of mapParamsUI

mapParams <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    load_catalogue <- function() {
      if (isTRUE(session$userData$user_logged_in)) {
        map_params_module_data(
          con = session$userData$AquaCache,
          env = session$userData$app_cache
        )
      } else {
        map_params_module_data(con = session$userData$AquaCache)
      }
    }
    cached <- load_catalogue()
    parameters <- unique(cached$parameters, by = "parameter_id")
    moduleData <- reactiveValues(
      locations = cached$locations,
      timeseries = cached$timeseries,
      # Older shared caches can contain one row per catalogue group. Map
      # labels and selectors need exactly one row per parameter instead.
      parameters = parameters
    )

    mapCreated <- reactiveVal(FALSE)

    # Catalogue IDs differ between database instances. Prefer hydrometric
    # defaults by name, then fall back to parameters actually in this cache.
    parameter_ids <- parameters$parameter_id
    parameter_names <- tolower(trimws(parameters$param_name))
    primary <- parameter_ids[match("water flow", parameter_names)]
    if (!length(primary) || is.na(primary)) {
      primary <- parameter_ids[1L]
    }
    secondary <- parameter_ids[match("water level", parameter_names)]
    if (!length(secondary) || is.na(secondary)) {
      secondary <- setdiff(parameter_ids, primary)[1L]
    }
    map_params <- reactiveValues(
      param1 = primary,
      param2 = secondary,
      yrs1 = 10L,
      yrs2 = 10L,
      days1 = 1L,
      days2 = 1L,
      latest = FALSE,
      target = Sys.Date(),
      stats_period = "30yr",
      params = 1L,
      bins = c(-Inf, 0, 20, 40, 60, 80, 100, Inf),
      colors = c(
        "#8c510a",
        "#d8b365",
        "#FEE090",
        "#74ADD1",
        "#4575D2",
        "#313695",
        "#A50026"
      )
    )

    # Login/logout may replace the connection (and the target database).
    # Refresh scoped metadata before the next worker request, retaining the
    # selected parameter by name when catalogue IDs differ between instances.
    observeEvent(
      if (is.function(session$userData$AquaCache_version)) {
        session$userData$AquaCache_version()
      },
      {
        request_id <<- request_id + 1L
        pending_request <<- NULL
        desired_key <<- NULL
        map_state$data <- NULL
        map_state$status <- "idle"
        if (inherits(active_fetch, "mirai")) {
          mirai::stop_mirai(active_fetch)
        }
        selected_names <- moduleData$parameters$param_name[
          match(
            c(map_params$param1, map_params$param2),
            moduleData$parameters$parameter_id
          )
        ]
        next_cache <- load_catalogue()
        parameters <- unique(next_cache$parameters, by = "parameter_id")
        selected_ids <- parameters$parameter_id[match(
          tolower(trimws(selected_names)),
          tolower(trimws(parameters$param_name))
        )]
        moduleData$locations <- next_cache$locations
        moduleData$timeseries <- next_cache$timeseries
        moduleData$parameters <- parameters
        map_params$param1 <- if (is.na(selected_ids[1L])) {
          parameters$parameter_id[1L]
        } else {
          selected_ids[1L]
        }
        map_params$param2 <- if (is.na(selected_ids[2L])) {
          setdiff(parameters$parameter_id, map_params$param1)[1L]
        } else {
          selected_ids[2L]
        }
      },
      ignoreInit = TRUE,
      priority = 20
    )

    output$banner <- renderUI({
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "mapParams"
      )
    })
    output$top <- renderUI({
      tagList(
        accordion(
          id = ns("accordion"),
          open = TRUE,
          accordion_panel(
            title = tr("instructions", language$language),
            tags$p(HTML(tr(
              "params_map_module_instructions",
              language$language
            ))),
            tags$div(style = "height: 10px;")
          )
        )
      )
    }) |> # End of renderUI for instructions
      bindEvent(language$language)

    output$sidebar_controls <- renderUI({
      req(language$language)
      target_input <- dateInput(
        ns("target"),
        label = tr("map_target_date", language$language),
        value = map_params$target,
        max = Sys.Date(),
        format = "yyyy-mm-dd",
        language = language$abbrev
      )
      if (map_params$latest) {
        target_input <- shinyjs::disabled(target_input)
      }
      tagList(
        selectizeInput(
          ns("mapType"),
          label = tr("map_mapType", language$language),
          choices = stats::setNames(
            c("range", "actual"),
            c(
              tr("map_relative", language$language),
              tr("map_actual_values", language$language)
            )
          ),
          selected = if (is.null(input$mapType)) "range" else input$mapType,
          multiple = FALSE
        ),
        conditionalPanel(
          condition = "input.mapType != 'actual'",
          ns = ns,
          selectizeInput(
            ns("stats_period"),
            label = tr("stats_period", language$language),
            choices = stats::setNames(
              c("30yr", "full"),
              YGwater:::format_stats_period_label(
                c("30yr", "full"),
                language$language
              )
            ),
            selected = map_params$stats_period,
            multiple = FALSE
          )
        ),
        target_input,
        checkboxInput(
          ns("latest"),
          tooltip(
            trigger = tagList(
              tr("map_latest_measurements", language$language),
              " ",
              bsicons::bs_icon("info-circle-fill")
            ),
            tr("map_latest_measurements_modal", language$language)
          ),
          value = map_params$latest
        ),
        conditionalPanel(
          condition = "input.latest == true",
          ns = ns,
          checkboxInput(
            ns("auto_update_latest"),
            tr("map_latest_auto_update", language$language),
            value = isTRUE(input$auto_update_latest)
          )
        ),
        htmlOutput(ns("primary_param")), # Primary parameter information, rendered separately as it needs to update if selections change
        actionButton(
          ns("edit_primary_param"),
          if (config$public) {
            tr("map_edit_primary_param_solo", language$language)
          } else {
            tr("map_edit_primary_param", language$language)
          },
          style = "display: block; width: 100%;",
          class = "btn btn-primary"
        ),
        if (!config$public) {
          htmlOutput(ns("secondary_param"))
        },
        if (!config$public) {
          actionButton(
            ns("edit_secondary_param"),
            tr(
              if (map_params$params == 2) {
                "map_edit_second_param"
              } else {
                "map_add_second_param"
              },
              language$language
            ),
            style = "display: block; width: 100%",
            class = "btn btn-primary"
          )
        }
      )
    }) |>
      bindEvent(language$language)

    output$primary_param <- renderUI({
      req(map_params, language, input$mapType)
      tagList(
        h4(
          if (config$public) {
            tr("parameter", language$language)
          } else {
            tr("map_primary_param", language$language)
          }
        ), # Text for primary parameter
        p(
          moduleData$parameters[
            moduleData$parameters$parameter_id == map_params$param1,
            get(tr("param_name_col", language$language))
          ]
        ), # Name of primary parameter
        p(
          if (input$mapType != "actual") {
            tagList(
              tr("map_min_yrs_selected1", language$language),
              " ",
              map_params$yrs1,
              " ",
              tr("map_min_yrs_selected2", language$language)
            )
          },
          if (map_params$latest) {
            sprintf(
              tr(
                if (input$mapType == "actual") {
                  "map_latest_within_only"
                } else {
                  "map_latest_within_selected"
                },
                language$language
              ),
              map_params$days1
            )
          } else if (input$mapType == "actual") {
            sprintf(
              tr("map_date_within_only", language$language),
              map_params$days1
            )
          } else {
            paste(
              tr("map_date_within_selected1", language$language),
              map_params$days1,
              tr("map_date_within_selected2", language$language)
            )
          }
        ) # Text for within x days
      )
    }) |>
      bindEvent(
        map_params$param1,
        map_params$yrs1,
        map_params$days1,
        map_params$latest,
        input$mapType,
        language$language
      )

    output$secondary_param <- renderUI({
      req(map_params, language, input$mapType)
      if (
        config$public || map_params$params == 1 || input$mapType == "actual"
      ) {
        return(NULL)
      } else {
        tagList(
          h4(tr("map_second_param", language$language)), # Text for secondary parameter
          p(
            moduleData$parameters[
              moduleData$parameters$parameter_id == map_params$param2,
              get(tr("param_name_col", language$language))
            ]
          ), # Name of secondary parameter
          p(
            tr("map_min_yrs_selected1", language$language),
            " ",
            map_params$yrs2,
            " ",
            tr("map_min_yrs_selected2", language$language), # Text for min years selected
            if (map_params$latest) {
              sprintf(
                tr("map_latest_within_selected", language$language),
                map_params$days2
              )
            } else {
              paste(
                tr("map_date_within_selected1", language$language),
                map_params$days2,
                tr("map_date_within_selected2", language$language)
              )
            }
          ) # Text for within x days
        )
      }
    }) |>
      bindEvent(
        map_params$params,
        map_params$param2,
        map_params$yrs2,
        map_params$days2,
        map_params$latest,
        input$mapType,
        language$language
      )

    observeEvent(input$stats_period, {
      if (
        is.null(input$stats_period) ||
          !(input$stats_period %in% c("30yr", "full"))
      ) {
        map_params$stats_period <- "30yr"
      } else {
        map_params$stats_period <- input$stats_period
      }
    })

    # Latest mode preserves the user's historical date for switching back.
    observeEvent(input$latest, {
      map_params$latest <- isTRUE(input$latest)
      shinyjs::toggleState("target", condition = !map_params$latest)
      if (!map_params$latest) {
        updateCheckboxInput(session, "auto_update_latest", value = FALSE)
      }
    })

    # edit primary and secondary parameter options in a modal
    observeEvent(input$edit_primary_param, {
      # Order the parameters alphabetically by name (in the current language)
      tmp <- moduleData$parameters[
        order(moduleData$parameters[[tr("param_name_col", language$language)]]),
      ]

      showModal(modalDialog(
        title = NULL,
        selectizeInput(
          ns("param"),
          label = tr("parameter", language$language),
          choices = stats::setNames(
            tmp$parameter_id,
            tmp[[tr("param_name_col", language$language)]]
          ),
          selected = map_params$param1,
          multiple = FALSE
        ),
        if (input$mapType != "actual") {
          numericInput(
            ns("yrs"),
            label = tooltip(
              trigger = list(
                tr("map_min_yrs", language$language),
                bsicons::bs_icon("info-circle-fill")
              ),
              tr("tooltip_params_map_min_yrs", language$language),
            ),
            value = map_params$yrs1,
            min = 3,
            max = 100,
            step = 1
          )
        },
        numericInput(
          ns("days"),
          label = tooltip(
            trigger = list(
              tr("map_date_within", language$language),
              bsicons::bs_icon("info-circle-fill")
            ),
            tr("tooltip_params_map_date_within", language$language),
          ),
          value = map_params$days1,
          min = 0,
          max = 365,
          step = 1
        ),
        footer = tagList(
          actionButton(ns("save_primary_param"), tr("save", language$language)),
          actionButton(ns("close"), tr("close", language$language))
        )
      ))
    })
    observeEvent(input$save_primary_param, {
      if (!valid_selection()) {
        return()
      }
      map_params$param1 <- as.integer(input$param)
      if (input$mapType != "actual") {
        map_params$yrs1 <- as.integer(input$yrs)
      }
      map_params$days1 <- as.integer(input$days)
      removeModal()
    })

    observeEvent(input$edit_secondary_param, {
      # Order the parameters alphabetically by name (in the current language)
      tmp <- moduleData$parameters[
        order(moduleData$parameters[[tr("param_name_col", language$language)]]),
      ]

      showModal(modalDialog(
        title = NULL,
        selectizeInput(
          ns("param"),
          label = tr("parameter", language$language),
          choices = stats::setNames(
            tmp$parameter_id,
            tmp[[tr("param_name_col", language$language)]]
          ),
          selected = map_params$param2,
          multiple = FALSE
        ),
        if (input$mapType != "actual") {
          numericInput(
            ns("yrs"),
            label = tr("map_min_yrs", language$language),
            value = map_params$yrs2,
            min = 3,
            max = 100,
            step = 1
          )
        },
        numericInput(
          ns("days"),
          label = tr("map_date_within", language$language),
          value = map_params$days2,
          min = 0,
          max = 365,
          step = 1
        ),
        footer = tagList(
          actionButton(
            ns("save_secondary_param"),
            tr("save", language$language)
          ),
          if (map_params$params == 2) {
            actionButton(
              ns("remove_secondary_param"),
              tr("map_rm_second_param", language$language)
            )
          },
          actionButton(ns("close"), tr("close", language$language))
        )
      ))
    })

    observeEvent(input$save_secondary_param, {
      if (!valid_selection()) {
        return()
      }
      if (map_params$params == 1) {
        map_params$params <- 2
      }
      updateActionButton(
        session,
        "edit_secondary_param",
        tr("map_edit_second_param", language$language)
      )
      map_params$param2 <- as.integer(input$param)
      if (input$mapType != "actual") {
        map_params$yrs2 <- as.integer(input$yrs)
      }
      map_params$days2 <- as.integer(input$days)
      removeModal()
    })
    observeEvent(input$remove_secondary_param, {
      map_params$params <- 1
      updateActionButton(
        session,
        "edit_secondary_param",
        tr("map_add_second_param", language$language)
      )
      removeModal()
    })

    # remove any modal
    observeEvent(input$close, {
      removeModal()
    })

    observeEvent(
      input$mapType,
      {
        if (input$mapType == "actual" || config$public) {
          shinyjs::hide("secondary_param")
          shinyjs::hide("edit_secondary_param")
        } else {
          shinyjs::show("secondary_param")
          shinyjs::show("edit_secondary_param")
        }
      },
      ignoreInit = FALSE
    )

    observeEvent(input$target, {
      target <- tryCatch(as.Date(input$target), error = function(e) as.Date(NA))
      if (length(target) == 1L && !is.na(target) && target <= Sys.Date()) {
        map_params$target <- target
      }
    })

    # Keep invalid/transient modal inputs out of both state and SQL requests.
    valid_selection <- function() {
      values <- suppressWarnings(as.numeric(c(
        input$param,
        input$days
      )))
      valid <- length(values) == 2L &&
        all(is.finite(values)) &&
        all(values == floor(values)) &&
        values[1L] %in% moduleData$parameters$parameter_id &&
        values[2L] >= 0L &&
        values[2L] <= 365L
      if (input$mapType != "actual") {
        years <- suppressWarnings(as.numeric(input$yrs))
        valid <- valid &&
          length(years) == 1L &&
          is.finite(years) &&
          years == floor(years) &&
          years >= 3L &&
          years <= 100L
      }
      if (!valid) {
        showNotification(
          tr(
            if (input$mapType == "actual") {
              "map_invalid_absolute_selection"
            } else {
              "map_invalid_selection"
            },
            language$language
          ),
          type = "error"
        )
      }
      valid
    }

    refresh_tick <- reactiveVal(0L)
    map_state <- reactiveValues(
      status = "idle",
      data = NULL,
      request = NULL,
      stats_period = NULL,
      updated = NULL
    )
    pending_request <- NULL
    active_request <- NULL
    active_fetch <- NULL
    desired_key <- NULL
    request_id <- 0L

    # ExtendedTask detaches the promise from Shiny's session busy cycle. A
    # regular async observer would still prevent that session receiving input.
    fetch_task <- ExtendedTask$new(function(request, db) {
      active_fetch <<- params_map_start_fetch(
        request,
        db,
        params_map_fetch_data,
        params_map_daily_stats_select_sql
      )
      active_fetch
    })
    start_pending <- function() {
      active_request <<- pending_request
      pending_request <<- NULL
      fetch_task$invoke(active_request$query, active_request$db)
    }

    map_request <- reactive({
      req(mapCreated(), input$mapType, map_params$param1, language$language)
      # Language changes redraw the returned data locally. Absolute maps
      # request values without historical coverage or statistics queries.
      # Snapshot only data needed by the worker; never send reactiveValues.
      selections <- list(list(
        parameter_id = map_params$param1,
        years = if (input$mapType != "actual") map_params$yrs1 else NULL,
        days = map_params$days1
      ))
      if (
        !config$public && input$mapType == "range" && map_params$params == 2L
      ) {
        selections[[2L]] <- list(
          parameter_id = map_params$param2,
          years = map_params$yrs2,
          days = map_params$days2
        )
      }
      revision <- if (is.function(session$userData$AquaCache_version)) {
        session$userData$AquaCache_version()
      } else {
        0L
      }
      list(
        query = list(
          selections = selections,
          include_history = input$mapType != "actual",
          latest = map_params$latest,
          target = if (map_params$latest) NULL else map_params$target,
          stats_period = if (input$mapType != "actual") {
            map_params$stats_period
          } else {
            NULL
          },
          refresh = refresh_tick(),
          revision = revision,
          timeseries = data.table::copy(moduleData$timeseries[, c(
            "timeseries_id",
            "location_id",
            "parameter_id",
            "units",
            "matrix_state"
          )]),
          locations = data.table::copy(moduleData$locations)
        ),
        db = session$userData$config[c(
          "dbName",
          "dbHost",
          "dbPort",
          "dbUser",
          "dbPass"
        )]
      )
    })

    observeEvent(
      map_request(),
      {
        next_request <- map_request()
        # Compare scalar selections/identity, not data.table's mutable internal
        # attributes. Metadata changes are represented by the connection revision.
        next_key <- next_request
        next_key$query$timeseries <- NULL
        next_key$query$locations <- NULL
        if (identical(next_key, desired_key)) {
          return()
        }
        desired_key <<- next_key
        request_id <<- request_id + 1L
        next_request$query$request_id <- request_id
        next_request$query$now <- Sys.time()
        pending_request <<- next_request
        map_state$status <- "loading"
        map_state$data <- NULL
        if (identical(fetch_task$status(), "running")) {
          # Coalesce rapid edits to a single newest request. Interrupted or late
          # results can never replace the current selection's markers or labels.
          if (inherits(active_fetch, "mirai")) mirai::stop_mirai(active_fetch)
        } else {
          start_pending()
        }
      },
      priority = 10
    )

    observeEvent(
      fetch_task$status(),
      {
        status <- fetch_task$status()
        if (!status %in% c("success", "error")) {
          return()
        }
        if (!is.null(pending_request)) {
          start_pending()
          return()
        }
        if (
          is.null(active_request) ||
            active_request$query$request_id != request_id
        ) {
          return()
        }
        result <- tryCatch(fetch_task$result(), error = function(e) {
          message("Parameter map request failed: ", conditionMessage(e))
          NULL
        })
        if (is.null(result)) {
          map_state$status <- "error"
          map_state$data <- NULL
        } else {
          map_state$request <- active_request$query
          map_state$data <- result$data
          map_state$stats_period <- result$stats_period
          map_state$updated <- Sys.time()
          map_state$status <- "success"
        }
      },
      ignoreInit = TRUE
    )

    session$onSessionEnded(function() {
      pending_request <<- NULL
      if (inherits(active_fetch, "mirai")) mirai::stop_mirai(active_fetch)
    })
    observeEvent(input$retry_map, refresh_tick(refresh_tick() + 1L))

    output$map_status <- renderUI({
      req(language$language)
      if (map_state$status == "loading") {
        return(tags$p(
          class = "text-muted",
          tr("map_loading", language$language)
        ))
      }
      if (map_state$status == "error") {
        return(tags$p(
          class = "text-danger",
          tr("map_load_error", language$language),
          " ",
          actionLink(ns("retry_map"), tr("map_retry", language$language))
        ))
      }
      if (!nrow(moduleData$parameters)) {
        return(tags$p(tr("map_no_measurements", language$language)))
      }
      if (map_state$status != "success") {
        return(NULL)
      }
      tagList(
        if (!nrow(map_state$data)) {
          tags$p(tr("map_no_measurements", language$language))
        },
        tags$p(
          class = "small text-muted",
          tr("map_auto_update_notification", language$language),
          ": ",
          format(map_state$updated, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
        ),
        if (
          isTRUE(map_state$request$include_history) &&
            !identical(map_state$stats_period, map_state$request$stats_period)
        ) {
          tags$p(class = "small", tr("map_stats_fallback", language$language))
        }
      )
    })

    legend_ids <- character()
    observe({
      req(mapCreated(), language$language, input$mapType)
      proxy <- leaflet::leafletProxy("map", session) |>
        leaflet::clearGroup("measurements")
      for (legend_id in legend_ids) {
        proxy <- leaflet::removeControl(proxy, legend_id)
      }
      legend_ids <<- character()
      if (map_state$status != "success" || !nrow(map_state$data)) {
        return()
      }
      # Switching back to relative values needs a fresh historical result.
      if (
        input$mapType != "actual" && !isTRUE(map_state$request$include_history)
      ) {
        return()
      }
      mapping_data <- data.table::copy(map_state$data)
      if (input$mapType == "actual") {
        mapping_data <- mapping_data[
          mapping_data$parameter_id == map_params$param1
        ]
        if (!nrow(mapping_data)) return()
      }
      param_names <- moduleData$parameters[[tr(
        "param_name_col",
        language$language
      )]]
      mapping_data[,
        param_name := param_names[match(
          parameter_id,
          moduleData$parameters$parameter_id
        )]
      ]
      mapping_data[, param_unit := data.table::fifelse(is.na(units), "", units)]
      esc <- YGwater:::escape_html_text
      names <- mapping_data[[tr("generic_name_col", language$language)]]
      timestamp <- if (map_state$request$latest) {
        format(mapping_data$datetime, "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
      } else {
        paste(mapping_data$date, tr("map_daily_mean", language$language))
      }
      rate_text <- rep("", nrow(mapping_data))
      if (
        map_state$request$latest &&
          "record_rate_seconds" %in% names(mapping_data)
      ) {
        known_rate <- is.finite(mapping_data$record_rate_seconds) &
          mapping_data$record_rate_seconds > 0
        rate_text[known_rate] <- paste0(
          tr("record_rate", language$language),
          ": ",
          as.character(lubridate::seconds_to_period(mapping_data$record_rate_seconds[
            known_rate
          ])),
          "<br/>"
        )
      }
      history_text <- rep("", nrow(mapping_data))
      if (input$mapType != "actual") {
        history_text <- paste0(
          "<br/>",
          tr("map_relative", language$language),
          ": ",
          ifelse(
            is.na(mapping_data$percent_historic_range),
            "—",
            paste0(round(mapping_data$percent_historic_range, 2), "%")
          ),
          "<br/>",
          tr("map_actual_hist_range", language$language),
          ": ",
          round(mapping_data$min, 2),
          " ",
          tr("to", language$language),
          " ",
          round(mapping_data$max, 2),
          " ",
          esc(mapping_data$param_unit),
          "<br/>",
          tr("map_actual_yrs", language$language),
          ": ",
          mapping_data$doy_count
        )
      }
      popup <- paste0(
        "<strong>",
        esc(names),
        "</strong><br/>",
        esc(mapping_data$param_name),
        "<br/>",
        rate_text,
        ifelse(
          is.na(mapping_data$matrix_state) | !nzchar(mapping_data$matrix_state),
          "",
          paste0(esc(mapping_data$matrix_state), "<br/>")
        ),
        tr("date", language$language),
        ": ",
        timestamp,
        "<br/>",
        tr("value", language$language),
        ": ",
        round(mapping_data$value, 2),
        " ",
        esc(mapping_data$param_unit),
        history_text
      )
      # Different matrix units must not share an absolute numeric scale.
      groups <- if (input$mapType == "actual") {
        unique(mapping_data$param_unit)
      } else {
        "relative"
      }
      for (i in seq_along(groups)) {
        keep <- if (input$mapType == "actual") {
          mapping_data$param_unit == groups[i]
        } else {
          rep(TRUE, nrow(mapping_data))
        }
        rows <- mapping_data[keep]
        if (input$mapType == "actual") {
          values <- rows$value
          limits <- range(values)
          if (limits[1L] == limits[2L]) {
            delta <- max(abs(limits[1L]) * 0.01, 0.1)
            limits <- limits + c(-delta, delta)
          }
          bins <- seq(
            limits[1L],
            limits[2L],
            length.out = length(map_params$colors) + 1L
          )
          title <- unique(rows$param_name)[1L]
          if (nzchar(groups[i])) {
            title <- paste0(title, " (", groups[i], ")")
          }
          digits <- if (max(abs(values)) >= 100) {
            0L
          } else if (max(abs(values)) >= 10) {
            1L
          } else {
            2L
          }
          lab_format <- leaflet::labelFormat(digits = digits)
        } else {
          values <- rows$percent_historic_range
          bins <- map_params$bins
          title <- tr("map_relative", language$language)
          lab_format <- leaflet::labelFormat(digits = 0, suffix = "%")
        }
        palette <- leaflet::colorBin(
          map_params$colors,
          domain = values,
          bins = bins,
          pretty = FALSE,
          na.color = "#808080"
        )
        legend_id <- paste0("measurement-legend-", i)
        proxy <- proxy |>
          leaflet::addCircleMarkers(
            lng = rows$longitude,
            lat = rows$latitude,
            group = "measurements",
            layerId = as.character(rows$timeseries_id),
            fillColor = palette(values),
            color = palette(values),
            fillOpacity = 1,
            stroke = TRUE,
            weight = 1,
            radius = 8,
            options = leaflet::pathOptions(pane = "overlay"),
            popup = popup[keep]
          ) |>
          leaflet::addLegend(
            position = "bottomright",
            pal = palette,
            values = values,
            title = esc(title),
            labFormat = lab_format,
            opacity = 1,
            layerId = legend_id
          )
        legend_ids <<- c(legend_ids, legend_id)
      }
    })

    # Create the basic map
    output$map <- leaflet::renderLeaflet({
      map <- leaflet::leaflet(
        options = leaflet::leafletOptions(
          maxZoom = 15,
          zoomSnap = 0.5,
          zoomDelta = 0.5,
          zoomPxPerZoomLevel = 120
        )
      ) %>%
        leaflet::addMapPane("basemap", zIndex = 1) %>%
        leaflet::addMapPane("overlay", zIndex = 420) %>%
        leaflet::addTiles(options = leaflet::tileOptions(pane = "basemap")) %>%
        leaflet::addProviderTiles(
          "Esri.WorldTopoMap",
          group = "Topographic",
          options = leaflet::providerTileOptions(pane = "basemap")
        ) %>%
        leaflet::addProviderTiles(
          "Esri.WorldImagery",
          group = "Satellite",
          options = leaflet::providerTileOptions(pane = "basemap")
        ) %>%
        leaflet::addLayersControl(
          baseGroups = c("Topographic", "Satellite")
        ) %>%
        leaflet::addScaleBar(
          position = "bottomleft",
          options = leaflet::scaleBarOptions(imperial = FALSE)
        ) %>%
        leaflet::setView(lng = -135.05, lat = 64.00, zoom = 5)
      map
    })

    # Wait for Leaflet's first client bounds event before sending proxy calls.
    observeEvent(input$map_bounds, mapCreated(TRUE), once = TRUE)

    # Auto-update for latest measurements every 15 minutes
    autoUpdateTimer <- reactiveTimer(15 * 60 * 1000, session = session)

    observeEvent(
      autoUpdateTimer(),
      {
        req(mapCreated())
        if (!map_params$latest || !isTRUE(input$auto_update_latest)) {
          return()
        }
        # One dispatch path handles both input changes and refresh ticks. Enabling
        # the checkbox itself does not run an extra query or reset any controls.
        if (fetch_task$status() != "running") refresh_tick(refresh_tick() + 1L)
      },
      ignoreInit = TRUE
    )
  }) # End of moduleServer
} # End of mapParams server function
