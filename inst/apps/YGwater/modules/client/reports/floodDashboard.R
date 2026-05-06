floodDashboardUIMod <- function(id) {
    ns <- shiny::NS(id)

    placeholder_block <- function(text, height = "240px") {
        shiny::tags$div(
            style = paste(
                "height:",
                height,
                ";",
                "border: 1px dashed #94a3b8;",
                "border-radius: 0.5rem;",
                "background: #f8fafc;",
                "display: flex;",
                "align-items: center;",
                "justify-content: center;",
                "color: #475569;",
                "font-size: 0.95rem;",
                "padding: 0.75rem;",
                "text-align: center;"
            ),
            text
        )
    }

    bslib::page_fillable(
        shiny::tags$style(shiny::HTML(paste(
            paste0(
                "#",
                ns("dashboard-panels"),
                " { display:grid; gap:1rem; align-items:start; }"
            ),
            paste0(
                "#",
                ns("dashboard-panels"),
                ".compact { grid-template-columns: repeat(2, minmax(0, 1fr)); }"
            ),
            paste0(
                "#",
                ns("dashboard-panels"),
                ".comfortable { grid-template-columns: 1fr; }"
            ),
            paste0(
                "#",
                ns("controls"),
                " .dashboard-controls-wrap { display:flex; flex-direction:column; gap:0.3rem; }"
            ),
            paste0(
                "#",
                ns("controls"),
                " .dashboard-controls-row { display:grid; gap:0.35rem; align-items:start; justify-items:stretch; }"
            ),
            paste0(
                "#",
                ns("controls"),
                " .dashboard-controls-row.dashboard-controls-top { grid-template-columns: 12rem 12rem 7rem 9rem; justify-content:start; align-items:end; }"
            ),
            paste0(
                "#",
                ns("controls"),
                " .dashboard-controls-row.dashboard-controls-plot { grid-template-columns: repeat(6, minmax(0, 1fr)); }"
            ),
            paste0(
                "#",
                ns("controls"),
                " .dashboard-controls-cell { min-width:0; display:flex; flex-direction:column; justify-content:flex-start; }"
            ),
            paste0(
                "#",
                ns("controls"),
                " .dashboard-controls-cell > * { width:100%; }"
            ),
            paste0(
                "#",
                ns("controls"),
                " .dashboard-controls-row.dashboard-controls-top .dashboard-controls-cell > .shiny-input-container { display:flex; flex-direction:column; justify-content:flex-start; height:100%; }"
            ),
            paste0(
                "#",
                ns("controls"),
                " .dashboard-controls-row .shiny-input-container { margin-bottom: 0; }"
            ),
            paste0(
                "#",
                ns("controls"),
                " .dashboard-controls-row .control-label { margin-bottom: 0.05rem; font-size: 0.62rem; line-height: 1; letter-spacing: 0.01em; }"
            ),
            paste0(
                "#",
                ns("controls"),
                " .dashboard-controls-row .form-control { min-height: calc(1.05rem + 2px); padding: 0.02rem 0.2rem; font-size: 0.68rem; line-height: 1.05; }"
            ),
            paste0(
                "#",
                ns("controls"),
                " .dashboard-controls-row input[type='datetime-local'].form-control { height: calc(1.05rem + 2px); }"
            ),
            paste0(
                "#",
                ns("controls"),
                " .dashboard-controls-row .selectize-control { margin-bottom: 0; }"
            ),
            paste0(
                "#",
                ns("controls"),
                " .dashboard-controls-row .selectize-control.single .selectize-input, #",
                ns("controls"),
                " .dashboard-controls-row .selectize-control.multi .selectize-input { min-height: calc(1.05rem + 2px); padding: 0.02rem 0.2rem; font-size: 0.68rem; line-height: 1.05; }"
            ),
            paste0(
                "#",
                ns("controls"),
                " .dashboard-controls-row .selectize-input > input { font-size: 0.68rem; line-height: 1.05; }"
            ),
            paste0(
                "#",
                ns("controls"),
                " .dashboard-controls-row .selectize-dropdown { font-size: 0.68rem; }"
            ),
            paste0(
                "#",
                ns("controls"),
                " .dashboard-controls-row .btn { min-height: calc(1.05rem + 2px); padding: 0.08rem 0.28rem; font-size: 0.68rem; line-height: 1.05; }"
            ),
            paste0(
                "#",
                ns("controls"),
                " .dashboard-controls-row .form-group { margin-bottom: 0; }"
            ),
            ".station-plot-header { display:flex; flex-direction:column; align-items:stretch; gap:0.5rem; width:100%; }",
            ".station-plot-title { width:100%; min-width:24rem; }",
            ".station-plot-actions { display:flex; align-items:center; flex-wrap:wrap; gap:0.75rem; width:100%; }",
            ".station-plot-actions .shiny-input-container { margin-bottom:0; }",
            ".station-plot-actions .checkbox { margin:0; }",
            ".station-plot-actions .btn { white-space:nowrap; }",
            ".station-plot-shell { position: relative; }",
            paste0(
                ".station-plot-shell.is-stale .js-plotly-plot { opacity: 0.45; filter: grayscale(1); }"
            ),
            paste0(
                ".station-plot-stale-banner { position: absolute; top: 0.75rem; left: 50%; transform: translateX(-50%); z-index: 10; padding: 0.35rem 0.75rem; border: 1px solid #64748b; border-radius: 999px; background: rgba(255,255,255,0.92); color: #334155; font-size: 0.9rem; font-weight: 600; pointer-events: none; }"
            ),
            paste0(
                "@media (max-width: 1100px) { #",
                ns("controls"),
                " .dashboard-controls-row.dashboard-controls-top, #",
                ns("controls"),
                " .dashboard-controls-row.dashboard-controls-plot { grid-template-columns: repeat(2, minmax(0, 1fr)); } }"
            ),
            paste0(
                "@media (max-width: 700px) { #",
                ns("controls"),
                " .dashboard-controls-row.dashboard-controls-top, #",
                ns("controls"),
                " .dashboard-controls-row.dashboard-controls-plot { grid-template-columns: 1fr; } }"
            ),
            paste0(
                "@media (max-width: 992px) { #",
                ns("dashboard-panels"),
                ".compact { grid-template-columns: 1fr; } }"
            ),
            sep = "\n"
        ))),
        shiny::tags$script(shiny::HTML(paste(
            sprintf(
                "Shiny.addCustomMessageHandler('%s', function(mode) { var el = document.getElementById('%s'); if (!el) return; el.classList.remove('compact', 'comfortable'); el.classList.add(mode === 'comfortable' ? 'comfortable' : 'compact'); });",
                ns("set-dashboard-mode"),
                ns("dashboard-panels")
            ),
            sprintf(
                "Shiny.addCustomMessageHandler('%s', function(isStale) { var el = document.getElementById('%s'); if (!el) return; el.classList.toggle('is-stale', !!isStale); });",
                ns("set-station-plot-stale"),
                ns("station-plot-shell")
            ),
            sep = "\n"
        ))),
        bslib::accordion(
            id = ns("controls"),
            open = TRUE,
            bslib::accordion_panel(
                "",
                shiny::tags$div(
                    class = "dashboard-controls-wrap",
                    shiny::tags$div(
                        class = "dashboard-controls-row dashboard-controls-top",
                        shiny::tags$div(
                            class = "dashboard-controls-cell",
                            shiny::selectInput(
                                inputId = ns("community"),
                                label = "Community",
                                choices = character(0),
                                selected = NULL
                            )
                        ),
                        shiny::tags$div(
                            class = "dashboard-controls-cell",
                            shiny::tags$div(
                                class = "form-group shiny-input-container",
                                style = "margin-bottom:0;",
                                shiny::tags$label(
                                    `for` = ns("time0"),
                                    "Time 0",
                                    class = "control-label"
                                ),
                                shiny::tags$input(
                                    id = ns("time0"),
                                    type = "datetime-local",
                                    class = "form-control",
                                    value = format(
                                        Sys.time(),
                                        "%Y-%m-%dT%H:%M"
                                    ),
                                    max = format(Sys.time(), "%Y-%m-%dT%H:%M"),
                                    oninput = paste0(
                                        "if(this.max && this.value > this.max){ this.value = this.max; }",
                                        " Shiny.setInputValue('",
                                        ns("time0"),
                                        "', this.value, {priority: 'event'});"
                                    )
                                )
                            )
                        ),
                        shiny::tags$div(
                            class = "dashboard-controls-cell",
                            shiny::selectInput(
                                inputId = ns("view_mode"),
                                label = "View",
                                choices = c(
                                    "Compact" = "compact",
                                    "Detailed" = "comfortable"
                                ),
                                selected = "compact"
                            )
                        ),
                        shiny::tags$div(
                            class = "dashboard-controls-cell",
                            shiny::tags$div(
                                class = "form-group shiny-input-container",
                                style = "margin-bottom:0;",
                                shiny::tags$label(
                                    `for` = ns("export_html_report"),
                                    "Export",
                                    class = "control-label"
                                ),
                                shiny::downloadButton(
                                    ns("export_html_report"),
                                    "Export to HTML",
                                    style = "width:100%;"
                                )
                            )
                        )
                    ),
                    shiny::tags$div(
                        class = "dashboard-controls-row dashboard-controls-plot",
                        shiny::tags$div(
                            class = "dashboard-controls-cell",
                            shiny::selectizeInput(
                                inputId = ns("parameter"),
                                label = "Primary",
                                choices = c(
                                    "water level",
                                    "water flow",
                                    "precipitation (1wk)",
                                    "precipitation (24hr)",
                                    "temperature, air",
                                    "FDD",
                                    "DDT",
                                    "snow water eq (pillow)",
                                    "snow depth (pillow)",
                                    "snow water eq (survey)",
                                    "snow depth (survey)"
                                ),
                                selected = "water level",
                                options = list(placeholder = "Param")
                            )
                        ),
                        shiny::tags$div(
                            class = "dashboard-controls-cell",
                            shiny::selectizeInput(
                                inputId = ns("primary_historical_years"),
                                label = "Hist years",
                                choices = character(0),
                                selected = character(0),
                                multiple = TRUE,
                                options = list(
                                    placeholder = "Years",
                                    plugins = list("remove_button")
                                )
                            )
                        ),
                        shiny::tags$div(
                            class = "dashboard-controls-cell",
                            shiny::selectizeInput(
                                inputId = ns("station"),
                                label = "Station",
                                choices = character(0),
                                selected = character(0),
                                multiple = FALSE,
                                options = list(placeholder = "Station")
                            )
                        ),
                        shiny::tags$div(
                            class = "dashboard-controls-cell",
                            shiny::selectizeInput(
                                inputId = ns("secondary_parameter"),
                                label = "Secondary",
                                choices = character(0),
                                selected = character(0),
                                options = list(placeholder = "Param")
                            )
                        ),
                        shiny::tags$div(
                            class = "dashboard-controls-cell",
                            shiny::selectizeInput(
                                inputId = ns("secondary_historical_years"),
                                label = "Hist years",
                                choices = character(0),
                                selected = character(0),
                                multiple = TRUE,
                                options = list(
                                    placeholder = "Years",
                                    plugins = list("remove_button")
                                )
                            )
                        ),
                        shiny::tags$div(
                            class = "dashboard-controls-cell",
                            shiny::selectizeInput(
                                inputId = ns("secondary_station"),
                                label = "Station",
                                choices = character(0),
                                selected = character(0),
                                options = list(placeholder = "Station")
                            )
                        )
                    )
                )
            )
        ),
        shiny::tags$div(
            id = ns("dashboard-panels"),
            class = "compact",
            bslib::card(
                bslib::card_header(
                    shiny::tags$div(
                        style = "display:flex; align-items:center; justify-content:space-between; gap:1rem; width:100%;",
                        shiny::tags$span("Summary"),
                        shiny::checkboxInput(
                            inputId = ns("summary_relative"),
                            label = "Relative changes",
                            value = FALSE,
                            width = NULL
                        )
                    )
                ),
                DT::DTOutput(ns("summary_table"))
            ),
            bslib::card(
                bslib::card_header(
                    shiny::tags$div(
                        class = "station-plot-header",
                        shiny::tags$div(
                            class = "station-plot-title",
                            shiny::textOutput(
                                ns("station_plot_title"),
                                inline = TRUE
                            )
                        ),
                        shiny::tags$div(
                            class = "station-plot-actions",
                            shiny::checkboxInput(
                                inputId = ns("station_plot_show_legend"),
                                label = "Show legend",
                                value = FALSE,
                                width = NULL
                            ),
                            shiny::checkboxInput(
                                inputId = ns("station_plot_label_traces"),
                                label = "Label traces",
                                value = TRUE,
                                width = NULL
                            ),
                            shiny::actionButton(
                                inputId = ns("create_plot"),
                                label = "Create plot",
                                class = "btn btn-primary btn-sm"
                            )
                        )
                    )
                ),
                shiny::tags$div(
                    id = ns("station-plot-shell"),
                    class = "station-plot-shell",
                    plotly::plotlyOutput(ns("station_plot")),
                    shiny::uiOutput(ns("station_plot_stale_banner"))
                )
            ),
            bslib::card(
                bslib::card_header(
                    "Drainage area and hydrometeorological station map"
                ),
                leaflet::leafletOutput(ns("stations_map"), height = "300px")
            ),
            bslib::card(
                bslib::card_header(shiny::uiOutput(ns("image_series_header"))),
                bslib::card_body(
                    shiny::uiOutput(ns("image_series_navigation")),
                    shiny::uiOutput(ns("station_image"))
                )
            )
        )
    )
}

floodDashboardMod <- function(id, language, inputs = NULL) {
    read_fva_json <- function() {
        jsonlite::fromJSON(
            system.file(
                "extdata/flood_vulnerable_gauges_encoded.json",
                package = "YGwater"
            ),
            simplifyVector = FALSE
        )
    }

    `%||%` <- function(a, b) if (!is.null(a)) a else b

    normalize_selected_historical_years <- function(years) {
        if (is.null(years) || length(years) == 0) {
            return(integer(0))
        }
        valid <- suppressWarnings(as.integer(years))
        sort(unique(valid[!is.na(valid)]), decreasing = TRUE)
    }

    parameter_query_name <- function(parameter) {
        aliases <- c(
            "FDD" = "temperature, air",
            "DDT" = "temperature, air",
            "precipitation (1wk)" = "precipitation, total",
            "precipitation (24hr)" = "precipitation, total",
            "snow water eq (pillow)" = "snow water equivalent",
            "snow depth (pillow)" = "snow depth",
            "snow water eq (survey)" = "snow water equivalent",
            "snow depth (survey)" = "snow depth"
        )

        if (parameter %in% names(aliases)) {
            aliases[[parameter]]
        } else {
            parameter
        }
    }

    daily_temperature_aggregation_name <- function() {
        "(min+max)/2"
    }

    #' Encode gauge metadata from community JSON data
    #'
    #' Extracts gauge names, location codes, location IDs, and encodings from the
    #' nested community configuration list and returns them as a flat data frame.
    #'
    #' @param community_data Named list of gauge configuration entries. Each
    #'   top-level name is a gauge name, and each element is a list of entries with
    #'   fields `location_code`, `location_id`, and `encoding`.
    #'
    #' @return A data frame with columns `gauge_name` (character),
    #'   `location_code` (character), `location_id` (integer), and
    #'   `encoding` (numeric). Rows with missing `location_code` and duplicate
    #'   `location_code` values are removed. Returns an empty data frame when
    #'   `community_data` is `NULL` or has length zero.
    get_encoded_gauge_metadata <- function(community_data) {
        if (is.null(community_data) || length(community_data) == 0) {
            return(data.frame())
        }

        extract_scalar <- function(entry, field, default = NA_character_) {
            value <- entry[[field]]
            if (is.null(value) || length(value) == 0) {
                return(default)
            }
            as.character(value[[1]])
        }

        gauge_names <- names(community_data)
        rows <- lapply(gauge_names, function(gauge_name) {
            entries <- community_data[[gauge_name]]
            if (is.null(entries) || length(entries) == 0) {
                return(NULL)
            }

            data.frame(
                gauge_name = rep(gauge_name, length(entries)),
                location_code = vapply(
                    entries,
                    extract_scalar,
                    field = "location_code",
                    FUN.VALUE = character(1),
                    default = NA_character_
                ),
                location_id = suppressWarnings(as.integer(vapply(
                    entries,
                    extract_scalar,
                    field = "location_id",
                    FUN.VALUE = character(1),
                    default = NA_character_
                ))),
                encoding = suppressWarnings(as.numeric(vapply(
                    entries,
                    extract_scalar,
                    field = "encoding",
                    FUN.VALUE = character(1),
                    default = NA_character_
                ))),
                stringsAsFactors = FALSE
            )
        })

        rows <- rows[vapply(rows, Negate(is.null), logical(1))]
        if (length(rows) == 0) {
            return(data.frame())
        }

        out <- do.call(rbind, rows)
        out <- out[
            !is.na(out$location_code) & nzchar(out$location_code),
            ,
            drop = FALSE
        ]
        out[!duplicated(out$location_code), , drop = FALSE]
    }

    get_location_ids_with_parameter <- function(location_ids, parameter, con) {
        location_ids <- unique(stats::na.omit(as.integer(location_ids)))
        if (length(location_ids) == 0) {
            return(integer(0))
        }

        param_sql <- DBI::dbQuoteString(con, parameter_query_name(parameter))

        dat <- YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "SELECT DISTINCT ts.location_id",
                    "FROM timeseries ts",
                    "JOIN parameters p",
                    "  ON p.parameter_id = ts.parameter_id",
                    "WHERE ts.location_id IN (%s)",
                    "  AND p.param_name = %s"
                ),
                paste(location_ids, collapse = ","),
                param_sql
            ),
            con = con
        )

        unique(stats::na.omit(as.integer(dat$location_id)))
    }

    dashboard_recent_cutoff_timestamp_sql <- function(reference_time = NULL) {
        base_sql <- if (is.null(reference_time)) {
            "NOW()"
        } else {
            paste0(
                "'",
                format(as.POSIXct(reference_time), "%Y-%m-%d %H:%M:%S"),
                "'::timestamp"
            )
        }

        paste0("(", base_sql, " - INTERVAL '14 days')")
    }

    dashboard_recent_cutoff_date_sql <- function(reference_time = NULL) {
        base_sql <- if (is.null(reference_time)) {
            "CURRENT_DATE"
        } else {
            paste0(
                "'",
                format(as.POSIXct(reference_time), "%Y-%m-%d"),
                "'::date"
            )
        }

        paste0("(", base_sql, " - INTERVAL '14 days')::date")
    }

    interpret_loaded_times_as_local <- function(
        dat,
        time_columns = c("datetime", "latest_time")
    ) {
        fixed_timezone <- "Etc/GMT+7"

        if (is.null(dat) || nrow(dat) == 0) {
            return(dat)
        }

        for (col_name in intersect(time_columns, names(dat))) {
            local_text <- if (
                inherits(dat[[col_name]], c("POSIXct", "POSIXt"))
            ) {
                format(
                    dat[[col_name]],
                    "%Y-%m-%d %H:%M:%S",
                    tz = fixed_timezone
                )
            } else {
                as.character(dat[[col_name]])
            }

            x <- suppressWarnings(as.POSIXct(
                local_text,
                tz = fixed_timezone,
                format = "%Y-%m-%d %H:%M:%S"
            ))
            if (all(is.na(x))) {
                next
            }

            attr(x, "tzone") <- fixed_timezone
            dat[[col_name]] <- x
        }

        dat
    }

    get_latest_parameter_summary <- function(
        location_codes,
        parameter,
        con,
        reference_time = NULL
    ) {
        location_codes <- unique(stats::na.omit(unlist(
            location_codes,
            use.names = FALSE
        )))
        if (length(location_codes) == 0) {
            return(data.frame())
        }

        location_codes_sql <- paste(
            DBI::dbQuoteString(con, location_codes),
            collapse = ","
        )
        parameter_sql <- DBI::dbQuoteString(
            con,
            parameter_query_name(parameter)
        )

        ref_ts_sql <- if (is.null(reference_time)) {
            "NOW()"
        } else {
            paste0(
                "'",
                format(as.POSIXct(reference_time), "%Y-%m-%d %H:%M:%S"),
                "'::timestamp"
            )
        }
        ref_date_sql <- if (is.null(reference_time)) {
            "CURRENT_DATE"
        } else {
            paste0(
                "'",
                format(as.POSIXct(reference_time), "%Y-%m-%d"),
                "'::date"
            )
        }
        recent_cutoff_ts_sql <- dashboard_recent_cutoff_timestamp_sql(
            reference_time
        )
        recent_cutoff_date_sql <- dashboard_recent_cutoff_date_sql(
            reference_time
        )

        dat <- YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "WITH target_locations AS (",
                    "    SELECT UNNEST(ARRAY[%s]::text[]) AS location_code",
                    "),",
                    "target_timeseries AS (",
                    "    SELECT ts.location_id, ts.timeseries_id",
                    "    FROM timeseries ts",
                    "    JOIN locations l ON l.location_id = ts.location_id",
                    "    JOIN target_locations tl ON tl.location_code = l.location_code",
                    "    JOIN parameters p ON p.parameter_id = ts.parameter_id",
                    "    WHERE p.param_name = %s",
                    "),",
                    "continuous_latest AS (",
                    "    SELECT DISTINCT ON (tt.location_id)",
                    "        tt.location_id,",
                    "        tt.timeseries_id,",
                    "        mc.datetime AS latest_time,",
                    "        mc.value AS current_value",
                    "    FROM target_timeseries tt",
                    "    JOIN measurements_continuous mc ON mc.timeseries_id = tt.timeseries_id",
                    "    WHERE mc.value IS NOT NULL",
                    paste0("      AND mc.datetime >= ", recent_cutoff_ts_sql),
                    paste0("      AND mc.datetime <= ", ref_ts_sql),
                    "    ORDER BY tt.location_id, mc.datetime DESC, tt.timeseries_id",
                    "),",
                    "continuous_change AS (",
                    "    SELECT",
                    "        cl.location_id, cl.timeseries_id, cl.latest_time, cl.current_value,",
                    "        cl.current_value - prev.value AS change_24h,",
                    "        cl.current_value - prev48.value AS change_48h,",
                    "        cl.current_value - prev1w.value AS change_1w,",
                    "        1 AS source_priority",
                    "    FROM continuous_latest cl",
                    "    LEFT JOIN LATERAL (",
                    "        SELECT mc.value FROM measurements_continuous mc",
                    "        WHERE mc.timeseries_id = cl.timeseries_id",
                    "          AND mc.value IS NOT NULL",
                    "          AND mc.datetime <= cl.latest_time - INTERVAL '24 hours'",
                    "        ORDER BY mc.datetime DESC LIMIT 1",
                    "    ) prev ON TRUE",
                    "    LEFT JOIN LATERAL (",
                    "        SELECT mc.value FROM measurements_continuous mc",
                    "        WHERE mc.timeseries_id = cl.timeseries_id",
                    "          AND mc.value IS NOT NULL",
                    "          AND mc.datetime <= cl.latest_time - INTERVAL '48 hours'",
                    "        ORDER BY mc.datetime DESC LIMIT 1",
                    "    ) prev48 ON TRUE",
                    "    LEFT JOIN LATERAL (",
                    "        SELECT mc.value FROM measurements_continuous mc",
                    "        WHERE mc.timeseries_id = cl.timeseries_id",
                    "          AND mc.value IS NOT NULL",
                    "          AND mc.datetime <= cl.latest_time - INTERVAL '7 days'",
                    "        ORDER BY mc.datetime DESC LIMIT 1",
                    "    ) prev1w ON TRUE",
                    "),",
                    "daily_latest AS (",
                    "    SELECT DISTINCT ON (tt.location_id)",
                    "        tt.location_id, tt.timeseries_id, mcd.date::timestamp AS latest_time, mcd.value AS current_value",
                    "    FROM target_timeseries tt",
                    "    JOIN measurements_calculated_daily mcd ON mcd.timeseries_id = tt.timeseries_id",
                    "    WHERE mcd.value IS NOT NULL",
                    paste0("      AND mcd.date >= ", recent_cutoff_date_sql),
                    paste0("      AND mcd.date <= ", ref_date_sql),
                    "    ORDER BY tt.location_id, mcd.date DESC, tt.timeseries_id",
                    "),",
                    "daily_change AS (",
                    "    SELECT",
                    "        dl.location_id, dl.timeseries_id, dl.latest_time, dl.current_value,",
                    "        dl.current_value - prev.value AS change_24h,",
                    "        dl.current_value - prev48.value AS change_48h,",
                    "        dl.current_value - prev1w.value AS change_1w,",
                    "        2 AS source_priority",
                    "    FROM daily_latest dl",
                    "    LEFT JOIN LATERAL (",
                    "        SELECT mcd.value FROM measurements_calculated_daily mcd",
                    "        WHERE mcd.timeseries_id = dl.timeseries_id",
                    "          AND mcd.value IS NOT NULL",
                    "          AND mcd.date::timestamp <= dl.latest_time - INTERVAL '1 day'",
                    "        ORDER BY mcd.date DESC LIMIT 1",
                    "    ) prev ON TRUE",
                    "    LEFT JOIN LATERAL (",
                    "        SELECT mcd.value FROM measurements_calculated_daily mcd",
                    "        WHERE mcd.timeseries_id = dl.timeseries_id",
                    "          AND mcd.value IS NOT NULL",
                    "          AND mcd.date::timestamp <= dl.latest_time - INTERVAL '2 days'",
                    "        ORDER BY mcd.date DESC LIMIT 1",
                    "    ) prev48 ON TRUE",
                    "    LEFT JOIN LATERAL (",
                    "        SELECT mcd.value FROM measurements_calculated_daily mcd",
                    "        WHERE mcd.timeseries_id = dl.timeseries_id",
                    "          AND mcd.value IS NOT NULL",
                    "          AND mcd.date::timestamp <= dl.latest_time - INTERVAL '7 days'",
                    "        ORDER BY mcd.date DESC LIMIT 1",
                    "    ) prev1w ON TRUE",
                    "),",
                    "latest AS (",
                    "    SELECT location_id, timeseries_id, latest_time, current_value, change_24h, change_48h, change_1w, source_priority FROM continuous_change",
                    "    UNION ALL",
                    "    SELECT location_id, timeseries_id, latest_time, current_value, change_24h, change_48h, change_1w, source_priority FROM daily_change",
                    "),",
                    "best_latest AS (",
                    "    SELECT l.*,",
                    "        ROW_NUMBER() OVER (PARTITION BY l.location_id ORDER BY l.latest_time DESC, l.source_priority, l.timeseries_id) AS rn",
                    "    FROM latest l",
                    ")",
                    "SELECT loc.location_id, loc.location_code, loc.name, b.timeseries_id,",
                    "    b.latest_time, b.current_value, b.change_24h, b.change_48h, b.change_1w,",
                    paste0(
                        "    EXTRACT(EPOCH FROM (",
                        ref_ts_sql,
                        " - b.latest_time)) / 3600.0 AS last_data_age_hours"
                    ),
                    "FROM best_latest b",
                    "JOIN locations loc ON loc.location_id = b.location_id",
                    "WHERE b.rn = 1",
                    "ORDER BY loc.location_code"
                ),
                location_codes_sql,
                parameter_sql
            ),
            con = con
        )

        interpret_loaded_times_as_local(dat, time_columns = c("latest_time"))
    }

    get_fdd_summary <- function(location_codes, con, reference_time = NULL) {
        location_codes <- unique(stats::na.omit(unlist(
            location_codes,
            use.names = FALSE
        )))
        if (length(location_codes) == 0) {
            return(data.frame())
        }

        location_codes_sql <- paste(
            DBI::dbQuoteString(con, location_codes),
            collapse = ","
        )
        aggregation_sql <- DBI::dbQuoteString(
            con,
            daily_temperature_aggregation_name()
        )

        ref_ts_sql <- if (is.null(reference_time)) {
            "NOW()"
        } else {
            paste0(
                "'",
                format(as.POSIXct(reference_time), "%Y-%m-%d %H:%M:%S"),
                "'::timestamp"
            )
        }
        ref_date_sql <- if (is.null(reference_time)) {
            "CURRENT_DATE"
        } else {
            paste0(
                "'",
                format(as.POSIXct(reference_time), "%Y-%m-%d"),
                "'::date"
            )
        }
        recent_cutoff_date_sql <- dashboard_recent_cutoff_date_sql(
            reference_time
        )

        dat <- YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "WITH target_locations AS (",
                    "    SELECT UNNEST(ARRAY[%s]::text[]) AS location_code",
                    "),",
                    "target_timeseries AS (",
                    "    SELECT ts.location_id, ts.timeseries_id",
                    "    FROM timeseries ts",
                    "    JOIN locations l",
                    "      ON l.location_id = ts.location_id",
                    "    JOIN target_locations tl",
                    "      ON tl.location_code = l.location_code",
                    "    JOIN parameters p",
                    "      ON p.parameter_id = ts.parameter_id",
                    "    JOIN aggregation_types a",
                    "      ON a.aggregation_type_id = ts.aggregation_type_id",
                    "    WHERE p.param_name = 'temperature, air'",
                    "      AND a.aggregation_type = %s",
                    "),",
                    "daily_temp AS (",
                    "    SELECT",
                    "        tt.location_id,",
                    "        tt.timeseries_id,",
                    "        mcd.date AS date,",
                    "        mcd.value AS mean_temp",
                    "    FROM target_timeseries tt",
                    "    JOIN measurements_calculated_daily mcd",
                    "      ON mcd.timeseries_id = tt.timeseries_id",
                    "    WHERE mcd.value IS NOT NULL",
                    paste0("      AND mcd.date <= ", ref_date_sql),
                    "),",
                    "daily_fdd AS (",
                    "    SELECT",
                    "        dt.location_id,",
                    "        dt.timeseries_id,",
                    "        dt.date,",
                    "        GREATEST(-dt.mean_temp, 0) AS fdd_day,",
                    "        CASE",
                    "            WHEN EXTRACT(MONTH FROM dt.date) >= 10",
                    "            THEN EXTRACT(YEAR FROM dt.date)::int",
                    "            ELSE EXTRACT(YEAR FROM dt.date)::int - 1",
                    "        END AS season_start_year",
                    "    FROM daily_temp dt",
                    "    WHERE dt.date <= ",
                    ref_date_sql,
                    "),",
                    "seasonal_fdd AS (",
                    "    SELECT",
                    "        df.location_id,",
                    "        df.timeseries_id,",
                    "        df.date,",
                    "        1 +",
                    "            SUM(df.fdd_day) OVER (",
                    "                PARTITION BY df.location_id, df.timeseries_id, df.season_start_year",
                    "                ORDER BY df.date",
                    "                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW",
                    "            ) -",
                    "            FIRST_VALUE(df.fdd_day) OVER (",
                    "                PARTITION BY df.location_id, df.timeseries_id, df.season_start_year",
                    "                ORDER BY df.date",
                    "                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW",
                    "            ) AS fdd",
                    "    FROM daily_fdd df",
                    "),",
                    "latest_by_location AS (",
                    "    SELECT DISTINCT ON (location_id)",
                    "        location_id,",
                    "        timeseries_id,",
                    "        date,",
                    "        fdd",
                    "    FROM seasonal_fdd",
                    "    ORDER BY location_id, date DESC, timeseries_id",
                    ")",
                    "SELECT",
                    "    loc.location_id,",
                    "    loc.location_code,",
                    "    loc.name,",
                    "    l.timeseries_id,",
                    "    l.date::timestamp AS latest_time,",
                    "    l.fdd AS current_value,",
                    "    l.fdd - prev24.fdd AS change_24h,",
                    "    l.fdd - prev48.fdd AS change_48h,",
                    "    l.fdd - prev1w.fdd AS change_1w,",
                    paste0(
                        "    EXTRACT(EPOCH FROM (",
                        ref_ts_sql,
                        " - l.date::timestamp)) / 3600.0 AS last_data_age_hours"
                    ),
                    "FROM latest_by_location l",
                    "LEFT JOIN LATERAL (",
                    "    SELECT sf.fdd",
                    "    FROM seasonal_fdd sf",
                    "    WHERE sf.location_id = l.location_id",
                    "      AND sf.timeseries_id = l.timeseries_id",
                    "      AND sf.date <= l.date - INTERVAL '1 day'",
                    "    ORDER BY sf.date DESC",
                    "    LIMIT 1",
                    ") prev24 ON TRUE",
                    "LEFT JOIN LATERAL (",
                    "    SELECT sf.fdd",
                    "    FROM seasonal_fdd sf",
                    "    WHERE sf.location_id = l.location_id",
                    "      AND sf.timeseries_id = l.timeseries_id",
                    "      AND sf.date <= l.date - INTERVAL '2 days'",
                    "    ORDER BY sf.date DESC",
                    "    LIMIT 1",
                    ") prev48 ON TRUE",
                    "LEFT JOIN LATERAL (",
                    "    SELECT sf.fdd",
                    "    FROM seasonal_fdd sf",
                    "    WHERE sf.location_id = l.location_id",
                    "      AND sf.timeseries_id = l.timeseries_id",
                    "      AND sf.date <= l.date - INTERVAL '7 days'",
                    "    ORDER BY sf.date DESC",
                    "    LIMIT 1",
                    ") prev1w ON TRUE",
                    "JOIN locations loc",
                    "  ON loc.location_id = l.location_id",
                    paste0("WHERE l.date >= ", recent_cutoff_date_sql),
                    "ORDER BY loc.location_code"
                ),
                location_codes_sql,
                aggregation_sql
            ),
            con = con
        )

        interpret_loaded_times_as_local(dat, time_columns = c("latest_time"))
    }

    get_ddt_summary <- function(location_codes, con, reference_time = NULL) {
        location_codes <- unique(stats::na.omit(unlist(
            location_codes,
            use.names = FALSE
        )))
        if (length(location_codes) == 0) {
            return(data.frame())
        }

        location_codes_sql <- paste(
            DBI::dbQuoteString(con, location_codes),
            collapse = ","
        )
        aggregation_sql <- DBI::dbQuoteString(
            con,
            daily_temperature_aggregation_name()
        )

        ref_ts_sql <- if (is.null(reference_time)) {
            "NOW()"
        } else {
            paste0(
                "'",
                format(as.POSIXct(reference_time), "%Y-%m-%d %H:%M:%S"),
                "'::timestamp"
            )
        }
        ref_date_sql <- if (is.null(reference_time)) {
            "CURRENT_DATE"
        } else {
            paste0(
                "'",
                format(as.POSIXct(reference_time), "%Y-%m-%d"),
                "'::date"
            )
        }
        recent_cutoff_date_sql <- dashboard_recent_cutoff_date_sql(
            reference_time
        )

        dat <- YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "WITH target_locations AS (",
                    "    SELECT UNNEST(ARRAY[%s]::text[]) AS location_code",
                    "),",
                    "target_timeseries AS (",
                    "    SELECT ts.location_id, ts.timeseries_id",
                    "    FROM timeseries ts",
                    "    JOIN locations l",
                    "      ON l.location_id = ts.location_id",
                    "    JOIN target_locations tl",
                    "      ON tl.location_code = l.location_code",
                    "    JOIN parameters p",
                    "      ON p.parameter_id = ts.parameter_id",
                    "    JOIN aggregation_types a",
                    "      ON a.aggregation_type_id = ts.aggregation_type_id",
                    "    WHERE p.param_name = 'temperature, air'",
                    "      AND a.aggregation_type = %s",
                    "),",
                    "daily_temp AS (",
                    "    SELECT",
                    "        tt.location_id,",
                    "        tt.timeseries_id,",
                    "        mcd.date AS date,",
                    "        mcd.value AS mean_temp",
                    "    FROM target_timeseries tt",
                    "    JOIN measurements_calculated_daily mcd",
                    "      ON mcd.timeseries_id = tt.timeseries_id",
                    "    WHERE mcd.value IS NOT NULL",
                    paste0("      AND mcd.date <= ", ref_date_sql),
                    "),",
                    "daily_ddt AS (",
                    "    SELECT",
                    "        dt.location_id,",
                    "        dt.timeseries_id,",
                    "        dt.date,",
                    "        GREATEST(dt.mean_temp, 0) AS ddt_day,",
                    "        CASE",
                    "            WHEN EXTRACT(MONTH FROM dt.date) >= 4",
                    "            THEN EXTRACT(YEAR FROM dt.date)::int",
                    "            ELSE EXTRACT(YEAR FROM dt.date)::int - 1",
                    "        END AS season_start_year",
                    "    FROM daily_temp dt",
                    "    WHERE dt.date <= ",
                    ref_date_sql,
                    "),",
                    "seasonal_ddt AS (",
                    "    SELECT",
                    "        df.location_id,",
                    "        df.timeseries_id,",
                    "        df.date,",
                    "        1 +",
                    "            SUM(df.ddt_day) OVER (",
                    "                PARTITION BY df.location_id, df.timeseries_id, df.season_start_year",
                    "                ORDER BY df.date",
                    "                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW",
                    "            ) -",
                    "            FIRST_VALUE(df.ddt_day) OVER (",
                    "                PARTITION BY df.location_id, df.timeseries_id, df.season_start_year",
                    "                ORDER BY df.date",
                    "                ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW",
                    "            ) AS ddt",
                    "    FROM daily_ddt df",
                    "),",
                    "latest_by_location AS (",
                    "    SELECT DISTINCT ON (location_id)",
                    "        location_id,",
                    "        timeseries_id,",
                    "        date,",
                    "        ddt",
                    "    FROM seasonal_ddt",
                    "    ORDER BY location_id, date DESC, timeseries_id",
                    ")",
                    "SELECT",
                    "    loc.location_id,",
                    "    loc.location_code,",
                    "    loc.name,",
                    "    l.timeseries_id,",
                    "    l.date::timestamp AS latest_time,",
                    "    l.ddt AS current_value,",
                    "    l.ddt - prev24.ddt AS change_24h,",
                    "    l.ddt - prev48.ddt AS change_48h,",
                    "    l.ddt - prev1w.ddt AS change_1w,",
                    paste0(
                        "    EXTRACT(EPOCH FROM (",
                        ref_ts_sql,
                        " - l.date::timestamp)) / 3600.0 AS last_data_age_hours"
                    ),
                    "FROM latest_by_location l",
                    "LEFT JOIN LATERAL (",
                    "    SELECT sd.ddt",
                    "    FROM seasonal_ddt sd",
                    "    WHERE sd.location_id = l.location_id",
                    "      AND sd.timeseries_id = l.timeseries_id",
                    "      AND sd.date <= l.date - INTERVAL '1 day'",
                    "    ORDER BY sd.date DESC",
                    "    LIMIT 1",
                    ") prev24 ON TRUE",
                    "LEFT JOIN LATERAL (",
                    "    SELECT sd.ddt",
                    "    FROM seasonal_ddt sd",
                    "    WHERE sd.location_id = l.location_id",
                    "      AND sd.timeseries_id = l.timeseries_id",
                    "      AND sd.date <= l.date - INTERVAL '2 days'",
                    "    ORDER BY sd.date DESC",
                    "    LIMIT 1",
                    ") prev48 ON TRUE",
                    "LEFT JOIN LATERAL (",
                    "    SELECT sd.ddt",
                    "    FROM seasonal_ddt sd",
                    "    WHERE sd.location_id = l.location_id",
                    "      AND sd.timeseries_id = l.timeseries_id",
                    "      AND sd.date <= l.date - INTERVAL '7 days'",
                    "    ORDER BY sd.date DESC",
                    "    LIMIT 1",
                    ") prev1w ON TRUE",
                    "JOIN locations loc",
                    "  ON loc.location_id = l.location_id",
                    paste0("WHERE l.date >= ", recent_cutoff_date_sql),
                    "ORDER BY loc.location_code"
                ),
                location_codes_sql,
                aggregation_sql
            ),
            con = con
        )

        interpret_loaded_times_as_local(dat, time_columns = c("latest_time"))
    }

    get_precipitation_timeseries_summary <- function(
        location_codes,
        con,
        reference_time = NULL
    ) {
        location_codes <- unique(stats::na.omit(unlist(
            location_codes,
            use.names = FALSE
        )))
        if (length(location_codes) == 0) {
            return(data.frame())
        }

        location_codes_sql <- paste(
            DBI::dbQuoteString(con, location_codes),
            collapse = ","
        )

        ref_ts_sql <- if (is.null(reference_time)) {
            "NOW()"
        } else {
            paste0(
                "'",
                format(as.POSIXct(reference_time), "%Y-%m-%d %H:%M:%S"),
                "'::timestamp"
            )
        }
        ref_date_sql <- if (is.null(reference_time)) {
            "CURRENT_DATE"
        } else {
            paste0(
                "'",
                format(as.POSIXct(reference_time), "%Y-%m-%d"),
                "'::date"
            )
        }
        recent_cutoff_date_sql <- dashboard_recent_cutoff_date_sql(
            reference_time
        )

        dat <- YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "WITH target_locations AS (",
                    "    SELECT UNNEST(ARRAY[%s]::text[]) AS location_code",
                    "),",
                    "target_timeseries AS (",
                    "    SELECT ts.location_id, ts.timeseries_id",
                    "    FROM timeseries ts",
                    "    JOIN locations loc ON loc.location_id = ts.location_id",
                    "    JOIN target_locations tl ON tl.location_code = loc.location_code",
                    "    JOIN parameters p ON p.parameter_id = ts.parameter_id",
                    "    WHERE p.param_name = 'precipitation, total'",
                    "),",
                    "filtered AS MATERIALIZED (",
                    "    SELECT t.location_id, t.timeseries_id, mc.date, mc.value",
                    "    FROM measurements_calculated_daily mc",
                    "    JOIN target_timeseries t ON t.timeseries_id = mc.timeseries_id",
                    "    WHERE mc.value IS NOT NULL",
                    paste0(
                        "      AND mc.date >= ",
                        ref_date_sql,
                        " - INTERVAL '6 months'"
                    ),
                    paste0(
                        "      AND mc.date < ",
                        ref_date_sql,
                        " + INTERVAL '1 day'"
                    ),
                    "),",
                    "latest_by_location AS (",
                    "    SELECT DISTINCT ON (location_id)",
                    "        location_id, timeseries_id, date AS last_date",
                    "    FROM filtered",
                    "    ORDER BY location_id, date DESC, timeseries_id",
                    "),",
                    "current_accumulations AS (",
                    "    SELECT",
                    "        l.location_id,",
                    "        SUM(mc.value) FILTER (WHERE mc.date > l.last_date - INTERVAL '7 days' AND mc.date <= l.last_date) AS precipitation_1w_accumulation,",
                    "        SUM(mc.value) FILTER (WHERE mc.date > l.last_date - INTERVAL '1 month' AND mc.date <= l.last_date) AS precipitation_1m_accumulation,",
                    "        SUM(mc.value) FILTER (WHERE mc.date > l.last_date - INTERVAL '6 months' AND mc.date <= l.last_date) AS precipitation_6m_accumulation",
                    "    FROM latest_by_location l",
                    "    JOIN measurements_calculated_daily mc ON mc.timeseries_id = l.timeseries_id",
                    "    WHERE mc.value IS NOT NULL",
                    "      AND mc.date > l.last_date - INTERVAL '6 months'",
                    "      AND mc.date <= l.last_date",
                    "    GROUP BY l.location_id",
                    ")",
                    "SELECT",
                    "    loc.location_id, loc.location_code, loc.name,",
                    "    l.last_date::timestamp AS latest_time,",
                    "    ca.precipitation_1w_accumulation,",
                    "    ca.precipitation_1m_accumulation,",
                    "    ca.precipitation_6m_accumulation,",
                    paste0(
                        "    EXTRACT(EPOCH FROM (",
                        ref_ts_sql,
                        " - l.last_date::timestamp)) / 3600.0 AS last_data_age_hours"
                    ),
                    "FROM target_locations tl",
                    "JOIN locations loc ON loc.location_code = tl.location_code",
                    "LEFT JOIN latest_by_location l ON l.location_id = loc.location_id",
                    "LEFT JOIN current_accumulations ca ON ca.location_id = loc.location_id",
                    "WHERE l.last_date IS NOT NULL",
                    paste0("  AND l.last_date >= ", recent_cutoff_date_sql),
                    "ORDER BY loc.location_code"
                ),
                location_codes_sql
            ),
            con = con
        )

        interpret_loaded_times_as_local(dat, time_columns = c("latest_time"))
    }

    get_snow_survey_summary <- function(
        location_codes,
        parameter,
        con,
        reference_time = NULL
    ) {
        location_codes <- unique(stats::na.omit(unlist(
            location_codes,
            use.names = FALSE
        )))
        if (length(location_codes) == 0) {
            return(data.frame())
        }

        location_codes_sql <- paste(
            DBI::dbQuoteString(con, location_codes),
            collapse = ","
        )
        parameter_sql <- DBI::dbQuoteString(
            con,
            parameter_query_name(parameter)
        )

        ref_ts <- if (is.null(reference_time)) {
            Sys.time()
        } else {
            as.POSIXct(reference_time)
        }
        ref_ts_sql <- paste0(
            "'",
            format(ref_ts, "%Y-%m-%d %H:%M:%S"),
            "'::timestamp"
        )
        ref_year <- as.integer(format(ref_ts, "%Y"))

        dat <- YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "WITH target_locations AS (",
                    "    SELECT UNNEST(ARRAY[%s]::text[]) AS location_code",
                    "),",
                    "survey_values AS (",
                    "    SELECT l.location_id, l.location_code, s.sample_id,",
                    "        s.target_datetime, dr.result AS value",
                    "    FROM target_locations tl",
                    "    JOIN locations l ON l.location_code = tl.location_code",
                    "    JOIN samples s ON s.location_id = l.location_id",
                    "    JOIN discrete.results dr ON dr.sample_id = s.sample_id",
                    "    JOIN parameters p ON p.parameter_id = dr.parameter_id",
                    "    WHERE p.param_name = %s",
                    "      AND dr.result IS NOT NULL",
                    "      AND s.target_datetime IS NOT NULL",
                    paste0("      AND s.target_datetime <= ", ref_ts_sql),
                    "      AND EXTRACT(MONTH FROM s.target_datetime) IN (3, 4, 5)",
                    "      AND EXTRACT(DAY FROM s.target_datetime) = 1",
                    "),",
                    "latest_by_location AS (",
                    "    SELECT DISTINCT ON (location_id)",
                    "        location_id, sample_id, target_datetime AS latest_time, value AS current_value",
                    "    FROM survey_values",
                    "    ORDER BY location_id, target_datetime DESC, sample_id DESC",
                    "),",
                    "current_year_values AS (",
                    "    SELECT location_id,",
                    sprintf(
                        "        MAX(value) FILTER (WHERE EXTRACT(YEAR FROM target_datetime)::int = %d AND EXTRACT(MONTH FROM target_datetime) = 3) AS march_1_value,",
                        ref_year
                    ),
                    sprintf(
                        "        MAX(value) FILTER (WHERE EXTRACT(YEAR FROM target_datetime)::int = %d AND EXTRACT(MONTH FROM target_datetime) = 4) AS april_1_value,",
                        ref_year
                    ),
                    sprintf(
                        "        MAX(value) FILTER (WHERE EXTRACT(YEAR FROM target_datetime)::int = %d AND EXTRACT(MONTH FROM target_datetime) = 5) AS may_1_value",
                        ref_year
                    ),
                    "    FROM survey_values GROUP BY location_id",
                    ")",
                    "SELECT loc.location_id, loc.location_code, loc.name,",
                    "    l.latest_time, l.current_value,",
                    "    cy.march_1_value, cy.april_1_value, cy.may_1_value,",
                    paste0(
                        "    EXTRACT(EPOCH FROM (",
                        ref_ts_sql,
                        " - l.latest_time)) / 3600.0 AS last_data_age_hours"
                    ),
                    "FROM latest_by_location l",
                    "JOIN locations loc ON loc.location_id = l.location_id",
                    "LEFT JOIN current_year_values cy ON cy.location_id = l.location_id",
                    "ORDER BY loc.location_code"
                ),
                location_codes_sql,
                parameter_sql
            ),
            con = con
        )

        interpret_loaded_times_as_local(dat, time_columns = c("latest_time"))
    }

    get_latest_parameter_summary_by_location_id <- function(
        location_ids,
        parameter,
        con,
        reference_time = NULL
    ) {
        location_ids <- unique(stats::na.omit(as.integer(location_ids)))
        if (length(location_ids) == 0) {
            return(data.frame())
        }

        parameter_sql <- DBI::dbQuoteString(
            con,
            parameter_query_name(parameter)
        )
        reference_ts <- if (is.null(reference_time)) {
            "NOW()"
        } else {
            paste0(
                "'",
                format(as.POSIXct(reference_time), "%Y-%m-%d %H:%M:%S"),
                "'::timestamp"
            )
        }
        reference_date <- if (is.null(reference_time)) {
            "CURRENT_DATE"
        } else {
            paste0(
                "'",
                format(as.POSIXct(reference_time), "%Y-%m-%d"),
                "'::date"
            )
        }

        YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "WITH target_locations AS (",
                    "    SELECT UNNEST(ARRAY[%s]::int[]) AS location_id",
                    "),",
                    "target_timeseries AS (",
                    "    SELECT ts.location_id, ts.timeseries_id",
                    "    FROM timeseries ts",
                    "    JOIN target_locations tl ON tl.location_id = ts.location_id",
                    "    JOIN parameters p ON p.parameter_id = ts.parameter_id",
                    "    WHERE p.param_name = %s",
                    "),",
                    "continuous_latest AS (",
                    "    SELECT DISTINCT ON (tt.location_id)",
                    "        tt.location_id,",
                    "        tt.timeseries_id,",
                    "        mc.datetime AS latest_time,",
                    "        mc.value AS current_value",
                    "    FROM target_timeseries tt",
                    "    JOIN measurements_continuous mc ON mc.timeseries_id = tt.timeseries_id",
                    "    WHERE mc.value IS NOT NULL",
                    paste0("      AND mc.datetime <= ", reference_ts),
                    "    ORDER BY tt.location_id, mc.datetime DESC, tt.timeseries_id",
                    "),",
                    "continuous_change AS (",
                    "    SELECT",
                    "        cl.location_id,",
                    "        cl.latest_time,",
                    paste0(
                        "        EXTRACT(EPOCH FROM (",
                        reference_ts,
                        " - cl.latest_time)) / 3600.0 AS last_data_age_hours,"
                    ),
                    "        cl.current_value,",
                    "        cl.current_value - prev24.value AS change_24h,",
                    "        cl.current_value - prev48.value AS change_48h,",
                    "        cl.current_value - prev1w.value AS change_1w,",
                    "        1 AS source_priority",
                    "    FROM continuous_latest cl",
                    "    LEFT JOIN LATERAL (",
                    "        SELECT mc.value",
                    "        FROM measurements_continuous mc",
                    "        WHERE mc.timeseries_id = cl.timeseries_id",
                    "          AND mc.value IS NOT NULL",
                    "          AND mc.datetime <= cl.latest_time - INTERVAL '24 hours'",
                    "        ORDER BY mc.datetime DESC",
                    "        LIMIT 1",
                    "    ) prev24 ON TRUE",
                    "    LEFT JOIN LATERAL (",
                    "        SELECT mc.value",
                    "        FROM measurements_continuous mc",
                    "        WHERE mc.timeseries_id = cl.timeseries_id",
                    "          AND mc.value IS NOT NULL",
                    "          AND mc.datetime <= cl.latest_time - INTERVAL '48 hours'",
                    "        ORDER BY mc.datetime DESC",
                    "        LIMIT 1",
                    "    ) prev48 ON TRUE",
                    "    LEFT JOIN LATERAL (",
                    "        SELECT mc.value",
                    "        FROM measurements_continuous mc",
                    "        WHERE mc.timeseries_id = cl.timeseries_id",
                    "          AND mc.value IS NOT NULL",
                    "          AND mc.datetime <= cl.latest_time - INTERVAL '7 days'",
                    "        ORDER BY mc.datetime DESC",
                    "        LIMIT 1",
                    "    ) prev1w ON TRUE",
                    "),",
                    "daily_latest AS (",
                    "    SELECT DISTINCT ON (tt.location_id)",
                    "        tt.location_id,",
                    "        tt.timeseries_id,",
                    "        mcd.date::timestamp AS latest_time,",
                    "        mcd.value AS current_value",
                    "    FROM target_timeseries tt",
                    "    JOIN measurements_calculated_daily mcd ON mcd.timeseries_id = tt.timeseries_id",
                    "    WHERE mcd.value IS NOT NULL",
                    paste0("      AND mcd.date <= ", reference_date),
                    "    ORDER BY tt.location_id, mcd.date DESC, tt.timeseries_id",
                    "),",
                    "daily_change AS (",
                    "    SELECT",
                    "        dl.location_id,",
                    "        dl.latest_time,",
                    paste0(
                        "        EXTRACT(EPOCH FROM (",
                        reference_ts,
                        " - dl.latest_time)) / 3600.0 AS last_data_age_hours,"
                    ),
                    "        dl.current_value,",
                    "        dl.current_value - prev24.value AS change_24h,",
                    "        dl.current_value - prev48.value AS change_48h,",
                    "        dl.current_value - prev1w.value AS change_1w,",
                    "        2 AS source_priority",
                    "    FROM daily_latest dl",
                    "    LEFT JOIN LATERAL (",
                    "        SELECT mcd.value",
                    "        FROM measurements_calculated_daily mcd",
                    "        WHERE mcd.timeseries_id = dl.timeseries_id",
                    "          AND mcd.value IS NOT NULL",
                    "          AND mcd.date::timestamp <= dl.latest_time - INTERVAL '1 day'",
                    "        ORDER BY mcd.date DESC",
                    "        LIMIT 1",
                    "    ) prev24 ON TRUE",
                    "    LEFT JOIN LATERAL (",
                    "        SELECT mcd.value",
                    "        FROM measurements_calculated_daily mcd",
                    "        WHERE mcd.timeseries_id = dl.timeseries_id",
                    "          AND mcd.value IS NOT NULL",
                    "          AND mcd.date::timestamp <= dl.latest_time - INTERVAL '2 day'",
                    "        ORDER BY mcd.date DESC",
                    "        LIMIT 1",
                    "    ) prev48 ON TRUE",
                    "    LEFT JOIN LATERAL (",
                    "        SELECT mcd.value",
                    "        FROM measurements_calculated_daily mcd",
                    "        WHERE mcd.timeseries_id = dl.timeseries_id",
                    "          AND mcd.value IS NOT NULL",
                    "          AND mcd.date::timestamp <= dl.latest_time - INTERVAL '7 day'",
                    "        ORDER BY mcd.date DESC",
                    "        LIMIT 1",
                    "    ) prev1w ON TRUE",
                    "),",
                    "combined AS (",
                    "    SELECT location_id, latest_time, last_data_age_hours, current_value, change_24h, change_48h, change_1w, source_priority FROM continuous_change",
                    "    UNION ALL",
                    "    SELECT location_id, latest_time, last_data_age_hours, current_value, change_24h, change_48h, change_1w, source_priority FROM daily_change",
                    "),",
                    "ranked AS (",
                    "    SELECT",
                    "        c.*,",
                    "        ROW_NUMBER() OVER (",
                    "            PARTITION BY c.location_id",
                    "            ORDER BY c.latest_time DESC, c.source_priority",
                    "        ) AS rn",
                    "    FROM combined c",
                    ")",
                    "SELECT",
                    "    location_id,",
                    "    latest_time,",
                    "    last_data_age_hours,",
                    "    current_value,",
                    "    change_24h,",
                    "    change_48h,",
                    "    change_1w",
                    "FROM ranked",
                    "WHERE rn = 1"
                ),
                paste(location_ids, collapse = ","),
                parameter_sql
            ),
            con = con
        )
    }

    #' Fetch a spatial vector layer from the database as an sf object
    #'
    #' Queries the `spatial.vectors` table, optionally filtering by feature name,
    #' reprojects geometries to the requested CRS, and returns an `sf` object.
    #'
    #' @param feature_name Character vector of feature names to include. When
    #'   `NULL` (default) all features in the matching layer(s) are returned.
    #' @param layer_name Character vector of layer names to query (matched against
    #'   the `layer_name` column of `spatial.vectors`).
    #' @param con A DBI database connection.
    #' @param epsg Integer EPSG code for the output CRS. Defaults to `4326`
    #'   (WGS 84).
    #'
    #' @return An `sf` object with all columns from `spatial.vectors` plus the
    #'   well-known-text geometry reprojected to `epsg`, or `NULL` when no rows
    #'   match the supplied filters.
    get_spatial_layer_as_sf <- function(
        feature_name = NULL,
        layer_name,
        con,
        epsg = 4326
    ) {
        layer_values <- unlist(layer_name, use.names = FALSE)
        if (length(layer_values) == 0) {
            return(NULL)
        }

        layer_sql <- paste(
            DBI::dbQuoteString(con, layer_values),
            collapse = ","
        )
        where_clauses <- c(sprintf("layer_name IN (%s)", layer_sql))

        if (!is.null(feature_name)) {
            feature_values <- unique(stats::na.omit(unlist(
                feature_name,
                use.names = FALSE
            )))
            feature_values <- feature_values[nzchar(feature_values)]

            if (length(feature_values) > 0) {
                feature_sql <- paste(
                    DBI::dbQuoteString(con, feature_values),
                    collapse = ","
                )
                where_clauses <- c(
                    where_clauses,
                    sprintf("feature_name IN (%s)", feature_sql)
                )
            }
        }

        query <- sprintf(
            paste(
                "SELECT",
                "    *,",
                "    ST_AsText(ST_Transform(geom, %d)) AS geom_wkt",
                "FROM spatial.vectors",
                "WHERE %s"
            ),
            epsg,
            paste(where_clauses, collapse = " AND ")
        )

        dat <- YGwater::dbGetQueryDT(query, con = con)
        if (is.null(dat) || nrow(dat) == 0) {
            return(NULL)
        }

        sf::st_as_sf(dat, wkt = "geom_wkt", crs = epsg)
    }

    #' Look up location records for gauges in a community configuration
    #'
    #' Resolves location metadata (name, code, ID, latitude, longitude) from the
    #' database for all gauges described in a community JSON configuration. The
    #' lookup first attempts to match by explicit `location_id` / `location_code`
    #' values found in the configuration entries; if no rows are found it falls
    #' back to matching by gauge name.
    #'
    #' @param community_data Named list of gauge configuration entries (same
    #'   structure as accepted by `get_encoded_gauge_metadata`).
    #' @param con A DBI database connection.
    #'
    #' @return A data frame with columns `name`, `location_code`, `location_id`,
    #'   `latitude`, and `longitude` (one row per unique matching location), or an
    #'   empty data frame when no matches are found or `community_data` is `NULL`.
    location_lookup_for_community <- function(community_data, con) {
        if (is.null(community_data) || length(community_data) == 0) {
            return(data.frame())
        }

        location_names <- unique(stats::na.omit(unlist(
            names(community_data),
            use.names = FALSE
        )))

        entry_list <- unlist(
            community_data,
            recursive = FALSE,
            use.names = FALSE
        )

        extract_entry_scalar <- function(entry, field) {
            value <- entry[[field]]
            if (is.null(value) || length(value) == 0) {
                return(NA_character_)
            }
            as.character(value[[1]])
        }

        location_ids <- integer(0)
        location_codes <- character(0)

        if (length(entry_list) > 0) {
            location_ids <- suppressWarnings(as.integer(vapply(
                entry_list,
                extract_entry_scalar,
                field = "location_id",
                FUN.VALUE = character(1)
            )))
            location_ids <- unique(stats::na.omit(location_ids))

            location_codes <- vapply(
                entry_list,
                extract_entry_scalar,
                field = "location_code",
                FUN.VALUE = character(1)
            )
            location_codes <- unique(stats::na.omit(location_codes))
            location_codes <- location_codes[nzchar(location_codes)]
        }

        dat <- data.frame()

        # Prefer strict DB matches by explicit IDs/codes from JSON metadata.
        where_clauses <- character(0)
        if (length(location_ids) > 0) {
            where_clauses <- c(
                where_clauses,
                sprintf(
                    "location_id IN (%s)",
                    paste(location_ids, collapse = ",")
                )
            )
        }
        if (length(location_codes) > 0) {
            location_codes_sql <- paste(
                DBI::dbQuoteString(con, location_codes),
                collapse = ","
            )
            where_clauses <- c(
                where_clauses,
                sprintf("location_code IN (%s)", location_codes_sql)
            )
        }

        if (length(where_clauses) > 0) {
            dat <- YGwater::dbGetQueryDT(
                paste(
                    "SELECT DISTINCT",
                    "    name,",
                    "    location_code,",
                    "    location_id,",
                    "    latitude,",
                    "    longitude",
                    "FROM locations",
                    sprintf(
                        "WHERE %s",
                        paste(where_clauses, collapse = " OR ")
                    ),
                    "ORDER BY name"
                ),
                con = con
            )
        }

        # Fallback: if no strict ID/code hits, try matching by location name.
        if ((is.null(dat) || nrow(dat) == 0) && length(location_names) > 0) {
            location_names_sql <- paste(
                DBI::dbQuoteString(con, location_names),
                collapse = ","
            )
            dat <- YGwater::dbGetQueryDT(
                paste(
                    "SELECT DISTINCT",
                    "    name,",
                    "    location_code,",
                    "    location_id,",
                    "    latitude,",
                    "    longitude",
                    "FROM locations",
                    sprintf("WHERE name IN (%s)", location_names_sql),
                    "ORDER BY name"
                ),
                con = con
            )
        }

        if (is.null(dat) || nrow(dat) == 0) {
            return(data.frame())
        }

        dat$location_code <- as.character(dat$location_code)
        dat$name <- as.character(dat$name)
        dat
    }

    #' Extract location codes from a community JSON configuration
    #'
    #' Walks the nested community configuration list and collects every unique,
    #' non-empty `location_code` string found in the leaf entries.
    #'
    #' @param community_data Named list of gauge configuration entries (same
    #'   structure as accepted by `get_encoded_gauge_metadata`).
    #'
    #' @return A character vector of unique, non-empty location codes, or
    #'   `character(0)` when `community_data` is `NULL`, empty, or contains no
    #'   valid codes.
    community_location_codes_from_json <- function(community_data) {
        if (is.null(community_data) || length(community_data) == 0) {
            return(character(0))
        }

        entry_list <- unlist(
            community_data,
            recursive = FALSE,
            use.names = FALSE
        )
        if (length(entry_list) == 0) {
            return(character(0))
        }

        extract_code <- function(entry) {
            value <- entry[["location_code"]]
            if (is.null(value) || length(value) == 0) {
                return(NA_character_)
            }
            as.character(value[[1]])
        }

        codes <- vapply(entry_list, extract_code, FUN.VALUE = character(1))
        codes <- unique(stats::na.omit(codes))
        codes[nzchar(codes)]
    }

    # ---------------------------------------------------------------------------
    # Plot helpers (ported from dev/freshet_forecasting/dashboard.r)
    # ---------------------------------------------------------------------------

    normalize_historical_start_year <- function(historical_start_year) {
        current_year <- as.integer(format(Sys.Date(), "%Y"))
        historical_start_year <- suppressWarnings(as.integer(
            historical_start_year[[1]]
        ))
        if (
            length(historical_start_year) != 1 ||
                is.na(historical_start_year) ||
                historical_start_year < 1980 ||
                historical_start_year > current_year
        ) {
            return(2020L)
        }
        historical_start_year
    }

    parameter_axis_title <- function(parameter) {
        axis_titles <- c(
            "precipitation (1wk)" = "Precipitation (1wk accumulation, mm)",
            "precipitation (24hr)" = "Precipitation (24hr, mm)",
            "FDD" = "Freezing Degree Days (FDD)",
            "DDT" = "Thawing Degree Days (DDT)",
            "snow water eq (pillow)" = "Snow Water Equivalent (Pillow)",
            "snow depth (pillow)" = "Snow Depth (Pillow)",
            "snow water eq (survey)" = "Snow Water Equivalent (Survey)",
            "snow depth (survey)" = "Snow Depth (Survey)"
        )
        if (parameter %in% names(axis_titles)) {
            axis_titles[[parameter]]
        } else {
            parameter
        }
    }

    sanitize_loaded_series_values <- function(dat, parameter) {
        if (
            is.null(dat) ||
                nrow(dat) == 0 ||
                !"value" %in% names(dat) ||
                identical(parameter, "temperature, air")
        ) {
            return(dat)
        }
        dat$value[!is.na(dat$value) & dat$value < 0] <- NA_real_
        dat
    }

    empty_plotly_widget <- function(title = NULL, annotations = NULL) {
        widget <- plotly::plot_ly(
            x = numeric(0),
            y = numeric(0),
            type = "scatter",
            mode = "lines",
            hoverinfo = "skip",
            showlegend = FALSE
        )
        plotly::layout(widget, title = title, annotations = annotations)
    }

    get_return_period_discharge <- function(
        location_codes,
        parameter = "water flow",
        return_periods = c(2, 5, 10, 25, 50, 100),
        con
    ) {
        location_codes_str <- paste(
            sprintf("'%s'", location_codes),
            collapse = ","
        )
        percentiles <- 1 - (1 / return_periods)
        percentile_cols <- paste(
            sprintf(
                "percentile_cont(%.4f) WITHIN GROUP (ORDER BY annual_peak) AS rp_%d",
                percentiles,
                return_periods
            ),
            collapse = ", "
        )
        YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "WITH annual_peaks AS (",
                    "    SELECT",
                    "        ts.location_id,",
                    "        l.location_code,",
                    "        EXTRACT(YEAR FROM mc.date)::int AS year,",
                    "        MAX(mc.value) AS annual_peak",
                    "    FROM measurements_calculated_daily mc",
                    "    JOIN timeseries ts ON mc.timeseries_id = ts.timeseries_id",
                    "    JOIN locations l ON ts.location_id = l.location_id",
                    "    JOIN parameters p ON ts.parameter_id = p.parameter_id",
                    "    WHERE l.location_code IN (%s)",
                    "      AND p.param_name = '%s'",
                    "    GROUP BY ts.location_id, l.location_code, EXTRACT(YEAR FROM mc.date)",
                    ")",
                    "SELECT location_id, location_code, COUNT(*) AS n_years, %s",
                    "FROM annual_peaks",
                    "GROUP BY location_id, location_code"
                ),
                location_codes_str,
                parameter,
                percentile_cols
            ),
            con = con
        )
    }

    get_daily_percentiles <- function(
        location_codes,
        parameter = "water flow",
        con,
        historical_start_year = 2020
    ) {
        query_parameter <- parameter_query_name(parameter)
        location_codes_str <- paste(
            sprintf("'%s'", location_codes),
            collapse = ","
        )
        historical_start_year <- normalize_historical_start_year(
            historical_start_year
        )
        historical_start_date_sql <- paste0(
            "MAKE_DATE(",
            historical_start_year,
            ", 1, 1)"
        )

        if (identical(parameter, "FDD")) {
            return(YGwater::dbGetQueryDT(
                sprintf(
                    paste(
                        "WITH daily_temp AS (",
                        "    SELECT l.location_code,",
                        "        mcd.date AS date,",
                        "        mcd.value AS mean_temp",
                        "    FROM measurements_calculated_daily mcd",
                        "    JOIN timeseries ts ON mcd.timeseries_id = ts.timeseries_id",
                        "    JOIN locations l ON ts.location_id = l.location_id",
                        "    JOIN parameters p ON ts.parameter_id = p.parameter_id",
                        "    WHERE l.location_code IN (%s)",
                        "      AND p.param_name = 'temperature, air'",
                        "      AND mcd.value IS NOT NULL",
                        paste0(
                            "      AND mcd.date >= ",
                            historical_start_date_sql
                        ),
                        "),",
                        "daily_fdd AS (",
                        "    SELECT dt.location_code, dt.date,",
                        "        GREATEST(-dt.mean_temp, 0) AS fdd_day,",
                        "        CASE WHEN EXTRACT(MONTH FROM dt.date) >= 10",
                        "            THEN EXTRACT(YEAR FROM dt.date)::int",
                        "            ELSE EXTRACT(YEAR FROM dt.date)::int - 1",
                        "        END AS season_start_year",
                        "    FROM daily_temp dt",
                        "),",
                        "seasonal_fdd AS (",
                        "    SELECT df.location_code, df.date,",
                        "        1 + SUM(df.fdd_day) OVER (PARTITION BY df.location_code, df.season_start_year ORDER BY df.date ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)",
                        "          - FIRST_VALUE(df.fdd_day) OVER (PARTITION BY df.location_code, df.season_start_year ORDER BY df.date ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS value",
                        "    FROM daily_fdd df",
                        "),",
                        "daily_values AS (",
                        "    SELECT location_code,",
                        "        CASE WHEN EXTRACT(MONTH FROM date) > 2 AND ((EXTRACT(YEAR FROM date)::int %% 4 = 0 AND EXTRACT(YEAR FROM date)::int %% 100 <> 0) OR EXTRACT(YEAR FROM date)::int %% 400 = 0)",
                        "            THEN EXTRACT(DOY FROM date)::int - 1 ELSE EXTRACT(DOY FROM date)::int END AS doy,",
                        "        value FROM seasonal_fdd",
                        "    WHERE NOT (EXTRACT(MONTH FROM date) = 2 AND EXTRACT(DAY FROM date) = 29) AND value IS NOT NULL",
                        "),",
                        "by_doy AS (",
                        "    SELECT location_code, doy, COUNT(*) AS n_values,",
                        "        percentile_cont(0.00) WITHIN GROUP (ORDER BY value) AS p0,",
                        "        percentile_cont(0.10) WITHIN GROUP (ORDER BY value) AS p10,",
                        "        percentile_cont(0.25) WITHIN GROUP (ORDER BY value) AS p25,",
                        "        percentile_cont(0.50) WITHIN GROUP (ORDER BY value) AS p50,",
                        "        percentile_cont(0.75) WITHIN GROUP (ORDER BY value) AS p75,",
                        "        percentile_cont(0.90) WITHIN GROUP (ORDER BY value) AS p90,",
                        "        percentile_cont(1.00) WITHIN GROUP (ORDER BY value) AS p100",
                        "    FROM daily_values GROUP BY location_code, doy",
                        ")",
                        "SELECT lc.location_code, gs.doy, b.n_values, b.p0, b.p10, b.p25, b.p50, b.p75, b.p90, b.p100",
                        "FROM (SELECT DISTINCT location_code FROM daily_values) lc",
                        "CROSS JOIN generate_series(1, 365) AS gs(doy)",
                        "LEFT JOIN by_doy b ON b.location_code = lc.location_code AND b.doy = gs.doy",
                        "ORDER BY lc.location_code, gs.doy"
                    ),
                    location_codes_str
                ),
                con = con
            ))
        }

        if (identical(parameter, "DDT")) {
            return(YGwater::dbGetQueryDT(
                sprintf(
                    paste(
                        "WITH daily_temp AS (",
                        "    SELECT l.location_code,",
                        "        mcd.date AS date,",
                        "        mcd.value AS mean_temp",
                        "    FROM measurements_calculated_daily mcd",
                        "    JOIN timeseries ts ON mcd.timeseries_id = ts.timeseries_id",
                        "    JOIN locations l ON ts.location_id = l.location_id",
                        "    JOIN parameters p ON ts.parameter_id = p.parameter_id",
                        "    WHERE l.location_code IN (%s)",
                        "      AND p.param_name = 'temperature, air'",
                        "      AND mcd.value IS NOT NULL",
                        paste0(
                            "      AND mcd.date >= ",
                            historical_start_date_sql
                        ),
                        "),",
                        "daily_ddt AS (",
                        "    SELECT dt.location_code, dt.date,",
                        "        GREATEST(dt.mean_temp, 0) AS ddt_day,",
                        "        CASE WHEN EXTRACT(MONTH FROM dt.date) >= 4",
                        "            THEN EXTRACT(YEAR FROM dt.date)::int",
                        "            ELSE EXTRACT(YEAR FROM dt.date)::int - 1",
                        "        END AS season_start_year",
                        "    FROM daily_temp dt",
                        "),",
                        "seasonal_ddt AS (",
                        "    SELECT df.location_code, df.date,",
                        "        1 + SUM(df.ddt_day) OVER (PARTITION BY df.location_code, df.season_start_year ORDER BY df.date ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)",
                        "          - FIRST_VALUE(df.ddt_day) OVER (PARTITION BY df.location_code, df.season_start_year ORDER BY df.date ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS value",
                        "    FROM daily_ddt df",
                        "),",
                        "daily_values AS (",
                        "    SELECT location_code,",
                        "        CASE WHEN EXTRACT(MONTH FROM date) > 2 AND ((EXTRACT(YEAR FROM date)::int %% 4 = 0 AND EXTRACT(YEAR FROM date)::int %% 100 <> 0) OR EXTRACT(YEAR FROM date)::int %% 400 = 0)",
                        "            THEN EXTRACT(DOY FROM date)::int - 1 ELSE EXTRACT(DOY FROM date)::int END AS doy,",
                        "        value FROM seasonal_ddt",
                        "    WHERE NOT (EXTRACT(MONTH FROM date) = 2 AND EXTRACT(DAY FROM date) = 29) AND value IS NOT NULL",
                        "),",
                        "by_doy AS (",
                        "    SELECT location_code, doy, COUNT(*) AS n_values,",
                        "        percentile_cont(0.00) WITHIN GROUP (ORDER BY value) AS p0,",
                        "        percentile_cont(0.10) WITHIN GROUP (ORDER BY value) AS p10,",
                        "        percentile_cont(0.25) WITHIN GROUP (ORDER BY value) AS p25,",
                        "        percentile_cont(0.50) WITHIN GROUP (ORDER BY value) AS p50,",
                        "        percentile_cont(0.75) WITHIN GROUP (ORDER BY value) AS p75,",
                        "        percentile_cont(0.90) WITHIN GROUP (ORDER BY value) AS p90,",
                        "        percentile_cont(1.00) WITHIN GROUP (ORDER BY value) AS p100",
                        "    FROM daily_values GROUP BY location_code, doy",
                        ")",
                        "SELECT lc.location_code, gs.doy, b.n_values, b.p0, b.p10, b.p25, b.p50, b.p75, b.p90, b.p100",
                        "FROM (SELECT DISTINCT location_code FROM daily_values) lc",
                        "CROSS JOIN generate_series(1, 365) AS gs(doy)",
                        "LEFT JOIN by_doy b ON b.location_code = lc.location_code AND b.doy = gs.doy",
                        "ORDER BY lc.location_code, gs.doy"
                    ),
                    location_codes_str
                ),
                con = con
            ))
        }

        if (identical(parameter, "precipitation (1wk)")) {
            return(YGwater::dbGetQueryDT(
                sprintf(
                    paste(
                        "WITH daily_values AS (",
                        "    SELECT l.location_code, mc.date, mc.value",
                        "    FROM measurements_calculated_daily mc",
                        "    JOIN timeseries ts ON mc.timeseries_id = ts.timeseries_id",
                        "    JOIN locations l ON ts.location_id = l.location_id",
                        "    JOIN parameters p ON ts.parameter_id = p.parameter_id",
                        "    WHERE l.location_code IN (%s)",
                        "      AND p.param_name = 'precipitation, total'",
                        paste0(
                            "      AND mc.date >= ",
                            historical_start_date_sql
                        ),
                        "      AND NOT (EXTRACT(MONTH FROM mc.date) = 2 AND EXTRACT(DAY FROM mc.date) = 29)",
                        "      AND mc.value IS NOT NULL",
                        "),",
                        "rolling_week AS (",
                        "    SELECT location_code, date,",
                        "        SUM(value) OVER (PARTITION BY location_code ORDER BY date ROWS BETWEEN 6 PRECEDING AND CURRENT ROW) AS weekly_accumulation",
                        "    FROM daily_values",
                        "),",
                        "daily_weekly AS (",
                        "    SELECT location_code,",
                        "        CASE WHEN EXTRACT(MONTH FROM date) > 2 AND ((EXTRACT(YEAR FROM date)::int %% 4 = 0 AND EXTRACT(YEAR FROM date)::int %% 100 <> 0) OR EXTRACT(YEAR FROM date)::int %% 400 = 0)",
                        "            THEN EXTRACT(DOY FROM date)::int - 1 ELSE EXTRACT(DOY FROM date)::int END AS doy,",
                        "        weekly_accumulation AS value FROM rolling_week",
                        "),",
                        "by_doy AS (",
                        "    SELECT location_code, doy, COUNT(*) AS n_values,",
                        "        percentile_cont(0.00) WITHIN GROUP (ORDER BY value) AS p0,",
                        "        percentile_cont(0.10) WITHIN GROUP (ORDER BY value) AS p10,",
                        "        percentile_cont(0.25) WITHIN GROUP (ORDER BY value) AS p25,",
                        "        percentile_cont(0.50) WITHIN GROUP (ORDER BY value) AS p50,",
                        "        percentile_cont(0.75) WITHIN GROUP (ORDER BY value) AS p75,",
                        "        percentile_cont(0.90) WITHIN GROUP (ORDER BY value) AS p90,",
                        "        percentile_cont(1.00) WITHIN GROUP (ORDER BY value) AS p100",
                        "    FROM daily_weekly GROUP BY location_code, doy",
                        ")",
                        "SELECT lc.location_code, gs.doy, b.n_values, b.p0, b.p10, b.p25, b.p50, b.p75, b.p90, b.p100",
                        "FROM (SELECT DISTINCT location_code FROM daily_weekly) lc",
                        "CROSS JOIN generate_series(1, 365) AS gs(doy)",
                        "LEFT JOIN by_doy b ON b.location_code = lc.location_code AND b.doy = gs.doy",
                        "ORDER BY lc.location_code, gs.doy"
                    ),
                    location_codes_str
                ),
                con = con
            ))
        }

        if (parameter %in% c("snow water eq (survey)", "snow depth (survey)")) {
            return(tryCatch(
                YGwater::dbGetQueryDT(
                    sprintf(
                        paste(
                            "WITH survey_values AS (",
                            "    SELECT l.location_code,",
                            "        CASE WHEN EXTRACT(MONTH FROM s.target_datetime) > 2",
                            "            AND ((EXTRACT(YEAR FROM s.target_datetime)::int %% 4 = 0",
                            "                 AND EXTRACT(YEAR FROM s.target_datetime)::int %% 100 <> 0)",
                            "                 OR EXTRACT(YEAR FROM s.target_datetime)::int %% 400 = 0)",
                            "            THEN EXTRACT(DOY FROM s.target_datetime)::int - 1",
                            "            ELSE EXTRACT(DOY FROM s.target_datetime)::int",
                            "        END AS doy,",
                            "        dr.result AS value",
                            "    FROM samples s",
                            "    JOIN discrete.results dr ON dr.sample_id = s.sample_id",
                            "    JOIN parameters p ON p.parameter_id = dr.parameter_id",
                            "    JOIN locations l ON l.location_id = s.location_id",
                            "    WHERE l.location_code IN (%s)",
                            "      AND p.param_name = '%s'",
                            "      AND dr.result IS NOT NULL",
                            "      AND s.target_datetime IS NOT NULL",
                            paste0(
                                "      AND s.target_datetime >= ",
                                historical_start_date_sql
                            ),
                            "      AND NOT (EXTRACT(MONTH FROM s.target_datetime) = 2",
                            "               AND EXTRACT(DAY FROM s.target_datetime) = 29)",
                            "),",
                            "by_doy AS (",
                            "    SELECT location_code, doy, COUNT(*) AS n_values,",
                            "        percentile_cont(0.00) WITHIN GROUP (ORDER BY value) AS p0,",
                            "        percentile_cont(0.10) WITHIN GROUP (ORDER BY value) AS p10,",
                            "        percentile_cont(0.25) WITHIN GROUP (ORDER BY value) AS p25,",
                            "        percentile_cont(0.50) WITHIN GROUP (ORDER BY value) AS p50,",
                            "        percentile_cont(0.75) WITHIN GROUP (ORDER BY value) AS p75,",
                            "        percentile_cont(0.90) WITHIN GROUP (ORDER BY value) AS p90,",
                            "        percentile_cont(1.00) WITHIN GROUP (ORDER BY value) AS p100",
                            "    FROM survey_values GROUP BY location_code, doy",
                            ")",
                            "SELECT lc.location_code, gs.doy, b.n_values, b.p0, b.p10, b.p25, b.p50, b.p75, b.p90, b.p100",
                            "FROM (SELECT DISTINCT location_code FROM survey_values) lc",
                            "CROSS JOIN generate_series(1, 365) AS gs(doy)",
                            "LEFT JOIN by_doy b ON b.location_code = lc.location_code AND b.doy = gs.doy",
                            "ORDER BY lc.location_code, gs.doy"
                        ),
                        location_codes_str,
                        query_parameter
                    ),
                    con = con
                ),
                error = function(e) data.frame()
            ))
        }

        YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "WITH daily_values AS (",
                    "    SELECT l.location_code,",
                    "        CASE WHEN EXTRACT(MONTH FROM mc.date) > 2 AND ((EXTRACT(YEAR FROM mc.date)::int %% 4 = 0 AND EXTRACT(YEAR FROM mc.date)::int %% 100 <> 0) OR EXTRACT(YEAR FROM mc.date)::int %% 400 = 0)",
                    "            THEN EXTRACT(DOY FROM mc.date)::int - 1 ELSE EXTRACT(DOY FROM mc.date)::int END AS doy,",
                    "        mc.value",
                    "    FROM measurements_calculated_daily mc",
                    "    JOIN timeseries ts ON mc.timeseries_id = ts.timeseries_id",
                    "    JOIN locations l ON ts.location_id = l.location_id",
                    "    JOIN parameters p ON ts.parameter_id = p.parameter_id",
                    "    WHERE l.location_code IN (%s)",
                    "      AND p.param_name = '%s'",
                    paste0("      AND mc.date >= ", historical_start_date_sql),
                    "      AND NOT (EXTRACT(MONTH FROM mc.date) = 2 AND EXTRACT(DAY FROM mc.date) = 29)",
                    "      AND mc.value IS NOT NULL",
                    "),",
                    "by_doy AS (",
                    "    SELECT location_code, doy, COUNT(*) AS n_values,",
                    "        percentile_cont(0.00) WITHIN GROUP (ORDER BY value) AS p0,",
                    "        percentile_cont(0.10) WITHIN GROUP (ORDER BY value) AS p10,",
                    "        percentile_cont(0.25) WITHIN GROUP (ORDER BY value) AS p25,",
                    "        percentile_cont(0.50) WITHIN GROUP (ORDER BY value) AS p50,",
                    "        percentile_cont(0.75) WITHIN GROUP (ORDER BY value) AS p75,",
                    "        percentile_cont(0.90) WITHIN GROUP (ORDER BY value) AS p90,",
                    "        percentile_cont(1.00) WITHIN GROUP (ORDER BY value) AS p100",
                    "    FROM daily_values GROUP BY location_code, doy",
                    ")",
                    "SELECT lc.location_code, gs.doy, b.n_values, b.p0, b.p10, b.p25, b.p50, b.p75, b.p90, b.p100",
                    "FROM (SELECT DISTINCT location_code FROM daily_values) lc",
                    "CROSS JOIN generate_series(1, 365) AS gs(doy)",
                    "LEFT JOIN by_doy b ON b.location_code = lc.location_code AND b.doy = gs.doy",
                    "ORDER BY lc.location_code, gs.doy"
                ),
                location_codes_str,
                query_parameter
            ),
            con = con
        )
    }

    get_station_timeseries <- function(
        location_code,
        parameter,
        con,
        reference_time = NULL,
        load_entire_record = FALSE,
        historical_start_year = 2020
    ) {
        if (is.na(location_code) || length(location_code) == 0) {
            return(data.frame(
                datetime = as.POSIXct(character()),
                value = numeric()
            ))
        }

        location_sql <- DBI::dbQuoteString(con, location_code)
        parameter_sql <- DBI::dbQuoteString(
            con,
            parameter_query_name(parameter)
        )
        historical_start_year <- normalize_historical_start_year(
            historical_start_year
        )

        finalize_series <- function(dat) {
            dat <- interpret_loaded_times_as_local(
                dat,
                time_columns = c("datetime")
            )
            if (
                !is.null(dat) &&
                    nrow(dat) > 0 &&
                    "datetime" %in% names(dat)
            ) {
                dat <- dat[order(dat$datetime), , drop = FALSE]
            }
            dat
        }

        ref_ts_sql <- if (is.null(reference_time)) {
            "NOW()"
        } else {
            paste0(
                "'",
                format(as.POSIXct(reference_time), "%Y-%m-%d %H:%M:%S"),
                "'::timestamp"
            )
        }
        ref_date_sql <- if (is.null(reference_time)) {
            "CURRENT_DATE"
        } else {
            paste0(
                "'",
                format(as.POSIXct(reference_time), "%Y-%m-%d"),
                "'::date"
            )
        }
        recent_cutoff_ts_sql <- dashboard_recent_cutoff_timestamp_sql(
            reference_time
        )
        recent_cutoff_date_sql <- dashboard_recent_cutoff_date_sql(
            reference_time
        )
        historical_daily_min_date_sql <- paste0(
            "MAKE_DATE(",
            historical_start_year,
            ", 1, 1)"
        )

        recent_parameter_sql <- if (parameter %in% c("FDD", "DDT")) {
            DBI::dbQuoteString(con, "temperature, air")
        } else if (
            parameter %in% c("precipitation (1wk)", "precipitation (24hr)")
        ) {
            DBI::dbQuoteString(con, "precipitation, total")
        } else {
            parameter_sql
        }

        has_recent_data <- YGwater::dbGetQueryDT(
            sprintf(
                paste(
                    "WITH recent_continuous AS (",
                    "    SELECT 1 FROM measurements_continuous mc",
                    "    JOIN timeseries ts ON ts.timeseries_id = mc.timeseries_id",
                    "    JOIN parameters p ON p.parameter_id = ts.parameter_id",
                    "    JOIN locations l ON l.location_id = ts.location_id",
                    "    WHERE l.location_code = %s AND p.param_name = %s AND mc.value IS NOT NULL",
                    paste0("      AND mc.datetime >= ", recent_cutoff_ts_sql),
                    paste0("      AND mc.datetime <= ", ref_ts_sql),
                    "    LIMIT 1",
                    "),",
                    "recent_daily AS (",
                    "    SELECT 1 FROM measurements_calculated_daily mcd",
                    "    JOIN timeseries ts ON ts.timeseries_id = mcd.timeseries_id",
                    "    JOIN parameters p ON p.parameter_id = ts.parameter_id",
                    "    JOIN locations l ON l.location_id = ts.location_id",
                    "    WHERE l.location_code = %s AND p.param_name = %s AND mcd.value IS NOT NULL",
                    paste0("      AND mcd.date >= ", recent_cutoff_date_sql),
                    paste0("      AND mcd.date <= ", ref_date_sql),
                    "    LIMIT 1",
                    ")",
                    "SELECT EXISTS(SELECT 1 FROM recent_continuous) OR EXISTS(SELECT 1 FROM recent_daily) AS has_recent_data"
                ),
                location_sql,
                recent_parameter_sql,
                location_sql,
                recent_parameter_sql
            ),
            con = con
        )

        if (
            nrow(has_recent_data) == 0 ||
                !isTRUE(has_recent_data$has_recent_data[[1]])
        ) {
            if (
                parameter %in%
                    c(
                        "temperature, air",
                        "precipitation (1wk)",
                        "precipitation (24hr)",
                        "FDD",
                        "DDT"
                    )
            ) {
                # Continue; these parameters are queried from daily series and may be older.
            } else {
                return(data.frame(
                    datetime = as.POSIXct(character()),
                    value = numeric()
                ))
            }
        }

        precip_start_filter_sql <- if (isTRUE(load_entire_record)) {
            paste0("  AND mcd.date >= ", historical_daily_min_date_sql)
        } else {
            paste0("  AND mcd.date >= ", ref_date_sql, " - INTERVAL '2 months'")
        }
        continuous_start_filter_sql <- paste0(
            "  AND mc.datetime >= ",
            ref_ts_sql,
            " - INTERVAL '7 days'"
        )
        daily_fallback_start_filter_sql <- if (isTRUE(load_entire_record)) {
            paste0("  AND mcd.date >= ", historical_daily_min_date_sql)
        } else {
            paste0("  AND mcd.date >= ", ref_date_sql, " - INTERVAL '2 months'")
        }

        if (identical(parameter, "temperature, air")) {
            temperature_daily <- YGwater::dbGetQueryDT(
                sprintf(
                    paste(
                        "SELECT mcd.date::timestamp AS datetime, mcd.value AS value",
                        "FROM measurements_calculated_daily mcd",
                        "JOIN timeseries ts ON ts.timeseries_id = mcd.timeseries_id",
                        "JOIN parameters p ON p.parameter_id = ts.parameter_id",
                        "JOIN locations l ON l.location_id = ts.location_id",
                        "WHERE l.location_code = %s",
                        "  AND p.param_name = 'temperature, air'",
                        "  AND mcd.value IS NOT NULL",
                        daily_fallback_start_filter_sql,
                        paste0("  AND mcd.date <= ", ref_date_sql),
                        "ORDER BY mcd.date"
                    ),
                    location_sql
                ),
                con = con
            )
            temperature_daily <- sanitize_loaded_series_values(
                temperature_daily,
                parameter
            )
            temperature_daily$trace_source <- "observed"
            return(finalize_series(temperature_daily))
        }

        if (identical(parameter, "FDD")) {
            fdd_start_filter_sql <- if (isTRUE(load_entire_record)) {
                paste0("  AND dt.date >= ", historical_daily_min_date_sql)
            } else {
                paste0(
                    "  AND dt.date >= MAKE_DATE(",
                    "CASE WHEN EXTRACT(MONTH FROM ",
                    ref_date_sql,
                    ") >= 10 ",
                    "THEN EXTRACT(YEAR FROM ",
                    ref_date_sql,
                    ")::int ",
                    "ELSE EXTRACT(YEAR FROM ",
                    ref_date_sql,
                    ")::int - 1 END, ",
                    "10, 1)"
                )
            }
            fdd <- YGwater::dbGetQueryDT(
                sprintf(
                    paste(
                        "WITH daily_temp AS (",
                        "    SELECT mcd.date AS date, mcd.value AS mean_temp",
                        "    FROM measurements_calculated_daily mcd",
                        "    JOIN timeseries ts ON ts.timeseries_id = mcd.timeseries_id",
                        "    JOIN parameters p ON p.parameter_id = ts.parameter_id",
                        "    JOIN locations l ON l.location_id = ts.location_id",
                        "    WHERE l.location_code = %s AND p.param_name = 'temperature, air' AND mcd.value IS NOT NULL",
                        paste0("      AND mcd.date <= ", ref_date_sql),
                        "),",
                        "daily_fdd AS (",
                        "    SELECT dt.date, GREATEST(-dt.mean_temp, 0) AS fdd_day,",
                        "        CASE WHEN EXTRACT(MONTH FROM dt.date) >= 10 THEN EXTRACT(YEAR FROM dt.date)::int ELSE EXTRACT(YEAR FROM dt.date)::int - 1 END AS season_start_year",
                        "    FROM daily_temp dt WHERE dt.date <=",
                        ref_date_sql,
                        fdd_start_filter_sql,
                        "),",
                        "seasonal_fdd AS (",
                        "    SELECT df.date::timestamp AS datetime,",
                        "        1 + SUM(df.fdd_day) OVER (PARTITION BY df.season_start_year ORDER BY df.date ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)",
                        "          - FIRST_VALUE(df.fdd_day) OVER (PARTITION BY df.season_start_year ORDER BY df.date ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS value",
                        "    FROM daily_fdd df",
                        ")",
                        "SELECT datetime, value FROM seasonal_fdd ORDER BY datetime"
                    ),
                    location_sql
                ),
                con = con
            )
            fdd$trace_source <- "observed"
            return(finalize_series(fdd))
        }

        if (identical(parameter, "DDT")) {
            ddt_start_filter_sql <- if (isTRUE(load_entire_record)) {
                paste0("  AND dt.date >= ", historical_daily_min_date_sql)
            } else {
                paste0(
                    "  AND dt.date >= MAKE_DATE(",
                    "CASE WHEN EXTRACT(MONTH FROM ",
                    ref_date_sql,
                    ") >= 4 ",
                    "THEN EXTRACT(YEAR FROM ",
                    ref_date_sql,
                    ")::int ",
                    "ELSE EXTRACT(YEAR FROM ",
                    ref_date_sql,
                    ")::int - 1 END, ",
                    "4, 1)"
                )
            }
            ddt <- YGwater::dbGetQueryDT(
                sprintf(
                    paste(
                        "WITH daily_temp AS (",
                        "    SELECT mcd.date AS date, mcd.value AS mean_temp",
                        "    FROM measurements_calculated_daily mcd",
                        "    JOIN timeseries ts ON ts.timeseries_id = mcd.timeseries_id",
                        "    JOIN parameters p ON p.parameter_id = ts.parameter_id",
                        "    JOIN locations l ON l.location_id = ts.location_id",
                        "    WHERE l.location_code = %s AND p.param_name = 'temperature, air' AND mcd.value IS NOT NULL",
                        paste0("      AND mcd.date <= ", ref_date_sql),
                        "),",
                        "daily_ddt AS (",
                        "    SELECT dt.date, GREATEST(dt.mean_temp, 0) AS ddt_day,",
                        "        CASE WHEN EXTRACT(MONTH FROM dt.date) >= 4 THEN EXTRACT(YEAR FROM dt.date)::int ELSE EXTRACT(YEAR FROM dt.date)::int - 1 END AS season_start_year",
                        "    FROM daily_temp dt WHERE dt.date <=",
                        ref_date_sql,
                        ddt_start_filter_sql,
                        "),",
                        "seasonal_ddt AS (",
                        "    SELECT df.date::timestamp AS datetime,",
                        "        1 + SUM(df.ddt_day) OVER (PARTITION BY df.season_start_year ORDER BY df.date ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)",
                        "          - FIRST_VALUE(df.ddt_day) OVER (PARTITION BY df.season_start_year ORDER BY df.date ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS value",
                        "    FROM daily_ddt df",
                        ")",
                        "SELECT datetime, value FROM seasonal_ddt ORDER BY datetime"
                    ),
                    location_sql
                ),
                con = con
            )
            ddt$trace_source <- "observed"
            return(finalize_series(ddt))
        }

        if (parameter %in% c("precipitation (1wk)", "precipitation (24hr)")) {
            precip_db_sql <- DBI::dbQuoteString(con, "precipitation, total")

            if (identical(parameter, "precipitation (1wk)")) {
                precip_sql <- paste(
                    "SELECT mcd.date::timestamp AS datetime,",
                    "    SUM(mcd.value) OVER (ORDER BY mcd.date ROWS BETWEEN 6 PRECEDING AND CURRENT ROW) AS value",
                    "FROM measurements_calculated_daily mcd",
                    "JOIN timeseries ts ON ts.timeseries_id = mcd.timeseries_id",
                    "JOIN parameters p ON p.parameter_id = ts.parameter_id",
                    "JOIN locations l ON l.location_id = ts.location_id",
                    "WHERE l.location_code = %s AND p.param_name = %s AND mcd.value IS NOT NULL",
                    precip_start_filter_sql,
                    paste0("  AND mcd.date <= ", ref_date_sql),
                    "ORDER BY mcd.date"
                )
            } else {
                precip_sql <- paste(
                    "SELECT mcd.date::timestamp AS datetime, mcd.value AS value",
                    "FROM measurements_calculated_daily mcd",
                    "JOIN timeseries ts ON ts.timeseries_id = mcd.timeseries_id",
                    "JOIN parameters p ON p.parameter_id = ts.parameter_id",
                    "JOIN locations l ON l.location_id = ts.location_id",
                    "WHERE l.location_code = %s AND p.param_name = %s AND mcd.value IS NOT NULL",
                    precip_start_filter_sql,
                    paste0("  AND mcd.date <= ", ref_date_sql),
                    "ORDER BY mcd.date"
                )
            }

            precipitation <- YGwater::dbGetQueryDT(
                sprintf(precip_sql, location_sql, precip_db_sql),
                con = con
            )
            precipitation <- sanitize_loaded_series_values(
                precipitation,
                parameter
            )
            precipitation$trace_source <- "observed"
            return(finalize_series(precipitation))
        }

        continuous <- tryCatch(
            YGwater::dbGetQueryDT(
                sprintf(
                    paste(
                        "SELECT mc.datetime, mc.value AS value",
                        "FROM measurements_continuous mc",
                        "JOIN timeseries ts ON ts.timeseries_id = mc.timeseries_id",
                        "JOIN parameters p ON p.parameter_id = ts.parameter_id",
                        "JOIN locations l ON l.location_id = ts.location_id",
                        "WHERE l.location_code = %s AND p.param_name = %s AND mc.value IS NOT NULL",
                        continuous_start_filter_sql,
                        paste0("  AND mc.datetime <= ", ref_ts_sql),
                        "ORDER BY mc.datetime"
                    ),
                    location_sql,
                    parameter_sql
                ),
                con = con
            ),
            error = function(e) data.frame()
        )

        daily_fallback <- tryCatch(
            YGwater::dbGetQueryDT(
                sprintf(
                    paste(
                        "SELECT mcd.date::timestamp AS datetime, mcd.value",
                        "FROM measurements_calculated_daily mcd",
                        "JOIN timeseries ts ON ts.timeseries_id = mcd.timeseries_id",
                        "JOIN parameters p ON p.parameter_id = ts.parameter_id",
                        "JOIN locations l ON l.location_id = ts.location_id",
                        "WHERE l.location_code = %s AND p.param_name = %s AND mcd.value IS NOT NULL",
                        daily_fallback_start_filter_sql,
                        paste0("  AND mcd.date <= ", ref_date_sql),
                        "ORDER BY mcd.date"
                    ),
                    location_sql,
                    parameter_sql
                ),
                con = con
            ),
            error = function(e) data.frame()
        )

        continuous <- sanitize_loaded_series_values(continuous, parameter)
        daily_fallback <- sanitize_loaded_series_values(
            daily_fallback,
            parameter
        )
        continuous$trace_source <- "realtime_continuous"
        daily_fallback$trace_source <- "historical_daily"

        if (isTRUE(load_entire_record)) {
            if (nrow(continuous) == 0) {
                return(finalize_series(daily_fallback))
            }
            if (nrow(daily_fallback) == 0) {
                return(finalize_series(continuous))
            }
            first_continuous_time <- min(continuous$datetime, na.rm = TRUE)
            first_continuous_date <- as.POSIXct(
                format(first_continuous_time, "%Y-%m-%d"),
                tz = attr(first_continuous_time, "tzone") %||%
                    "America/Whitehorse"
            )
            daily_history <- daily_fallback[
                daily_fallback$datetime <= first_continuous_date,
            ]
            merged_series <- rbind(
                daily_history[, c("datetime", "value", "trace_source")],
                continuous[, c("datetime", "value", "trace_source")]
            )
            merged_series <- merged_series[order(merged_series$datetime), ]
            return(finalize_series(merged_series))
        }

        if (nrow(continuous) == 0) {
            daily_fallback$trace_source <- "observed"
            return(finalize_series(daily_fallback))
        }

        if (nrow(daily_fallback) == 0) {
            continuous$trace_source <- "observed"
            return(finalize_series(continuous))
        }

        latest_continuous <- max(continuous$datetime, na.rm = TRUE)
        latest_daily <- max(daily_fallback$datetime, na.rm = TRUE)

        selected_series <- if (
            is.finite(latest_daily) &&
                (!is.finite(latest_continuous) ||
                    latest_daily > latest_continuous)
        ) {
            daily_fallback
        } else {
            continuous
        }

        selected_series$trace_source <- "observed"
        finalize_series(selected_series)
    }

    empty_historical_overlay_data <- function() {
        data.frame(
            location_code = character(),
            datetime = as.POSIXct(character(), tz = "America/Whitehorse"),
            value = numeric(),
            overlay_year = integer(),
            trace_source = character(),
            trace_label = character(),
            stringsAsFactors = FALSE
        )
    }

    empty_historical_overlay_traces <- function() {
        list()
    }

    get_historical_overlay_timeseries <- function(
        location_code,
        parameter,
        years,
        con,
        reference_time = NULL
    ) {
        if (
            is.na(location_code) ||
                length(location_code) == 0 ||
                !nzchar(location_code)
        ) {
            return(empty_historical_overlay_traces())
        }
        plot_timezone <- "Etc/GMT+7"
        target_time <- if (is.null(reference_time)) {
            as.POSIXct(Sys.time(), tz = plot_timezone)
        } else {
            suppressWarnings(as.POSIXct(reference_time, tz = plot_timezone))
        }
        if (is.na(target_time)) {
            target_time <- as.POSIXct(Sys.time(), tz = plot_timezone)
        }
        target_year <- as.integer(format(target_time, "%Y", tz = plot_timezone))

        selected_years <- normalize_selected_historical_years(years)
        selected_years <- selected_years[selected_years < target_year]
        if (length(selected_years) == 0) {
            return(empty_historical_overlay_traces())
        }

        location_sql <- DBI::dbQuoteString(con, location_code)
        parameter_sql <- DBI::dbQuoteString(
            con,
            parameter_query_name(parameter)
        )

        shift_dates_to_target_year <- function(dat, overlay_year) {
            if (is.null(dat) || nrow(dat) == 0) {
                return(empty_historical_overlay_data())
            }
            dat <- interpret_loaded_times_as_local(
                dat,
                time_columns = c("datetime")
            )
            dat <- sanitize_loaded_series_values(dat, parameter)
            source_dates <- as.Date(dat$datetime, tz = plot_timezone)
            source_month <- as.integer(format(source_dates, "%m"))
            source_day <- as.integer(format(source_dates, "%d"))

            # Align overlays by calendar day in the target year to avoid leap-year
            # DOY drift that shifts post-February values and clips fills.
            target_date_str <- sprintf(
                "%04d-%02d-%02d",
                target_year,
                source_month,
                source_day
            )
            shifted_dates <- as.Date(target_date_str)
            shifted_dates[source_month == 2 & source_day == 29] <- as.Date(NA)
            shifted_datetimes <- as.POSIXct(shifted_dates, tz = plot_timezone)
            dat$datetime <- shifted_datetimes
            dat <- dat[stats::complete.cases(dat[, c("datetime", "value")]), ]
            if (nrow(dat) == 0) {
                return(empty_historical_overlay_data())
            }
            dat$location_code <- location_code
            dat$overlay_year <- as.integer(overlay_year)
            dat$trace_source <- "selected_historical_year"
            dat$trace_label <- sprintf("%d", overlay_year)
            dat[, c(
                "location_code",
                "datetime",
                "value",
                "overlay_year",
                "trace_source",
                "trace_label"
            )]
        }

        overlay_frames <- lapply(selected_years, function(overlay_year) {
            year_start_sql <- sprintf("MAKE_DATE(%d, 1, 1)", overlay_year)
            year_end_sql <- sprintf("MAKE_DATE(%d, 12, 31)", overlay_year)

            overlay_data <- if (
                parameter %in%
                    c("snow water eq (survey)", "snow depth (survey)")
            ) {
                tryCatch(
                    YGwater::dbGetQueryDT(
                        sprintf(
                            paste(
                                "SELECT s.target_datetime AS datetime,",
                                "  dr.result AS value",
                                "FROM samples s",
                                "JOIN discrete.results dr",
                                "  ON dr.sample_id = s.sample_id",
                                "JOIN parameters p",
                                "  ON p.parameter_id = dr.parameter_id",
                                "JOIN locations l",
                                "  ON l.location_id = s.location_id",
                                "WHERE l.location_code = %s",
                                "  AND p.param_name = %s",
                                "  AND dr.result IS NOT NULL",
                                "  AND s.target_datetime IS NOT NULL",
                                "  AND EXTRACT(YEAR FROM s.target_datetime) = %d",
                                "ORDER BY s.target_datetime"
                            ),
                            location_sql,
                            parameter_sql,
                            overlay_year
                        ),
                        con = con
                    ),
                    error = function(e) NULL
                )
            } else if (identical(parameter, "FDD")) {
                season_start_sql <- sprintf(
                    "MAKE_DATE(%d, 10, 1)",
                    overlay_year - 1L
                )
                YGwater::dbGetQueryDT(
                    sprintf(
                        paste(
                            "WITH daily_temp AS (",
                            "    SELECT mcd.date AS date, mcd.value AS mean_temp",
                            "    FROM measurements_calculated_daily mcd",
                            "    JOIN timeseries ts ON ts.timeseries_id = mcd.timeseries_id",
                            "    JOIN parameters p ON p.parameter_id = ts.parameter_id",
                            "    JOIN locations l ON l.location_id = ts.location_id",
                            "    WHERE l.location_code = %s AND p.param_name = 'temperature, air' AND mcd.value IS NOT NULL",
                            paste0(
                                "      AND mcd.date >= ",
                                season_start_sql
                            ),
                            paste0(
                                "      AND mcd.date <= ",
                                year_end_sql
                            ),
                            "),",
                            "daily_fdd AS (",
                            "    SELECT dt.date, GREATEST(-dt.mean_temp, 0) AS fdd_day,",
                            "        CASE WHEN EXTRACT(MONTH FROM dt.date) >= 10 THEN EXTRACT(YEAR FROM dt.date)::int ELSE EXTRACT(YEAR FROM dt.date)::int - 1 END AS season_start_year",
                            "    FROM daily_temp dt",
                            "),",
                            "seasonal_fdd AS (",
                            "    SELECT df.date::timestamp AS datetime,",
                            "        1 + SUM(df.fdd_day) OVER (PARTITION BY df.season_start_year ORDER BY df.date ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)",
                            "          - FIRST_VALUE(df.fdd_day) OVER (PARTITION BY df.season_start_year ORDER BY df.date ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS value",
                            "    FROM daily_fdd df",
                            ")",
                            "SELECT datetime, value FROM seasonal_fdd",
                            paste0("WHERE datetime::date >= ", year_start_sql),
                            paste0("  AND datetime::date <= ", year_end_sql),
                            "ORDER BY datetime"
                        ),
                        location_sql
                    ),
                    con = con
                )
            } else if (identical(parameter, "DDT")) {
                season_start_sql <- sprintf(
                    "MAKE_DATE(%d, 4, 1)",
                    overlay_year - 1L
                )
                YGwater::dbGetQueryDT(
                    sprintf(
                        paste(
                            "WITH daily_temp AS (",
                            "    SELECT mcd.date AS date, mcd.value AS mean_temp",
                            "    FROM measurements_calculated_daily mcd",
                            "    JOIN timeseries ts ON ts.timeseries_id = mcd.timeseries_id",
                            "    JOIN parameters p ON p.parameter_id = ts.parameter_id",
                            "    JOIN locations l ON l.location_id = ts.location_id",
                            "    WHERE l.location_code = %s AND p.param_name = 'temperature, air' AND mcd.value IS NOT NULL",
                            paste0(
                                "      AND mcd.date >= ",
                                season_start_sql
                            ),
                            paste0(
                                "      AND mcd.date <= ",
                                year_end_sql
                            ),
                            "),",
                            "daily_ddt AS (",
                            "    SELECT dt.date, GREATEST(dt.mean_temp, 0) AS ddt_day,",
                            "        CASE WHEN EXTRACT(MONTH FROM dt.date) >= 4 THEN EXTRACT(YEAR FROM dt.date)::int ELSE EXTRACT(YEAR FROM dt.date)::int - 1 END AS season_start_year",
                            "    FROM daily_temp dt",
                            "),",
                            "seasonal_ddt AS (",
                            "    SELECT df.date::timestamp AS datetime,",
                            "        1 + SUM(df.ddt_day) OVER (PARTITION BY df.season_start_year ORDER BY df.date ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)",
                            "          - FIRST_VALUE(df.ddt_day) OVER (PARTITION BY df.season_start_year ORDER BY df.date ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS value",
                            "    FROM daily_ddt df",
                            ")",
                            "SELECT datetime, value FROM seasonal_ddt",
                            paste0("WHERE datetime::date >= ", year_start_sql),
                            paste0("  AND datetime::date <= ", year_end_sql),
                            "ORDER BY datetime"
                        ),
                        location_sql
                    ),
                    con = con
                )
            } else if (identical(parameter, "precipitation (1wk)")) {
                lookback_start_sql <- sprintf(
                    "MAKE_DATE(%d, 1, 1) - INTERVAL '6 days'",
                    overlay_year
                )
                precip_sql_q <- DBI::dbQuoteString(con, "precipitation, total")
                YGwater::dbGetQueryDT(
                    sprintf(
                        paste(
                            "WITH daily_values AS (",
                            "    SELECT mcd.date, mcd.value",
                            "    FROM measurements_calculated_daily mcd",
                            "    JOIN timeseries ts ON ts.timeseries_id = mcd.timeseries_id",
                            "    JOIN parameters p ON p.parameter_id = ts.parameter_id",
                            "    JOIN locations l ON l.location_id = ts.location_id",
                            "    WHERE l.location_code = %s AND p.param_name = %s AND mcd.value IS NOT NULL",
                            paste0(
                                "      AND mcd.date >= ",
                                lookback_start_sql
                            ),
                            paste0("      AND mcd.date <= ", year_end_sql),
                            "),",
                            "rolling_week AS (",
                            "    SELECT date::timestamp AS datetime,",
                            "        SUM(value) OVER (ORDER BY date ROWS BETWEEN 6 PRECEDING AND CURRENT ROW) AS value",
                            "    FROM daily_values",
                            ")",
                            "SELECT datetime, value FROM rolling_week",
                            paste0("WHERE datetime::date >= ", year_start_sql),
                            paste0("  AND datetime::date <= ", year_end_sql),
                            "ORDER BY datetime"
                        ),
                        location_sql,
                        precip_sql_q
                    ),
                    con = con
                )
            } else {
                q_param_sql <- if (
                    identical(parameter, "precipitation (24hr)")
                ) {
                    DBI::dbQuoteString(con, "precipitation, total")
                } else {
                    parameter_sql
                }
                YGwater::dbGetQueryDT(
                    sprintf(
                        paste(
                            "SELECT mcd.date::timestamp AS datetime, mcd.value AS value",
                            "FROM measurements_calculated_daily mcd",
                            "JOIN timeseries ts ON ts.timeseries_id = mcd.timeseries_id",
                            "JOIN parameters p ON p.parameter_id = ts.parameter_id",
                            "JOIN locations l ON l.location_id = ts.location_id",
                            "WHERE l.location_code = %s AND p.param_name = %s AND mcd.value IS NOT NULL",
                            paste0("  AND mcd.date >= ", year_start_sql),
                            paste0("  AND mcd.date <= ", year_end_sql),
                            "ORDER BY mcd.date"
                        ),
                        location_sql,
                        q_param_sql
                    ),
                    con = con
                )
            }

            shift_dates_to_target_year(overlay_data, overlay_year)
        })

        overlay_frames <- overlay_frames[
            vapply(overlay_frames, nrow, integer(1)) > 0
        ]
        if (length(overlay_frames) == 0) {
            return(empty_historical_overlay_traces())
        }
        names(overlay_frames) <- vapply(
            overlay_frames,
            function(dat) as.character(dat$overlay_year[[1]]),
            character(1)
        )
        lapply(overlay_frames, function(dat) {
            dat[order(dat$datetime), , drop = FALSE]
        })
    }

    simple_continuous_plotly_widget <- function(
        location_code,
        parameter,
        continuous_data,
        title_prefix = NULL
    ) {
        if (is.null(continuous_data) || nrow(continuous_data) == 0) {
            return(empty_plotly_widget(
                title = title_prefix,
                annotations = list(list(
                    text = sprintf("No data available for %s.", location_code),
                    x = 0.5,
                    y = 0.5,
                    xref = "paper",
                    yref = "paper",
                    showarrow = FALSE
                ))
            ))
        }
        series <- continuous_data
        if ("location_code" %in% names(series)) {
            series <- series[
                series$location_code == location_code,
                ,
                drop = FALSE
            ]
        }
        if (
            nrow(series) == 0 || !all(c("datetime", "value") %in% names(series))
        ) {
            return(empty_plotly_widget(
                title = title_prefix,
                annotations = list(list(
                    text = sprintf("No plottable data for %s.", location_code),
                    x = 0.5,
                    y = 0.5,
                    xref = "paper",
                    yref = "paper",
                    showarrow = FALSE
                ))
            ))
        }
        plot_timezone <- attr(series$datetime, "tzone") %||%
            "America/Whitehorse"
        series$datetime <- suppressWarnings(as.POSIXct(
            series$datetime,
            tz = plot_timezone
        ))
        series$value <- suppressWarnings(as.numeric(series$value))
        series <- series[
            stats::complete.cases(series[, c("datetime", "value")]),
            ,
            drop = FALSE
        ]
        series <- series[order(series$datetime), , drop = FALSE]
        if (nrow(series) == 0) {
            return(empty_plotly_widget(title = title_prefix))
        }
        title_text <- if (!is.null(title_prefix) && nzchar(title_prefix)) {
            title_prefix
        } else {
            sprintf("%s [%s]", location_code, parameter)
        }
        plotly::plot_ly(
            data = series,
            x = ~datetime,
            y = ~value,
            type = "scatter",
            mode = "lines",
            name = "Observed",
            line = list(color = "#000000", width = 1.5),
            hovertemplate = "Observed: %{y:.3f}<extra></extra>"
        ) %>%
            plotly::layout(
                title = title_text,
                xaxis = list(title = "Date"),
                yaxis = list(title = parameter_axis_title(parameter)),
                hovermode = "x unified",
                showlegend = FALSE
            )
    }

    plot_continuous_with_percentiles_and_return_periods <- function(
        location_code,
        continuous_data = NULL,
        percentiles = NULL,
        return_periods = NULL,
        parameter = "water flow",
        return_period_values = c(2, 5, 10, 25, 50, 100),
        start_date = NULL,
        end_date = NULL,
        reference_time = NULL,
        load_entire_record = FALSE,
        con = NULL,
        historical_start_year = 2020
    ) {
        include_return_periods <- parameter %in% c("water flow", "water level")
        include_percentiles <- parameter %in%
            c(
                "water flow",
                "water level",
                "precipitation (1wk)",
                "precipitation (24hr)",
                "temperature, air",
                "FDD",
                "DDT",
                "snow water equivalent",
                "snow depth",
                "snow water eq (pillow)",
                "snow depth (pillow)"
            )
        historical_start_year <- normalize_historical_start_year(
            historical_start_year
        )

        safe_as_posix <- function(x, tz) {
            tryCatch(
                suppressWarnings(as.POSIXct(x, tz = tz)),
                error = function(e) {
                    as.POSIXct(NA, tz = tz)
                }
            )
        }

        plot_timezone <- "America/Whitehorse"
        now_ts <- if (is.null(reference_time)) {
            Sys.time()
        } else {
            safe_as_posix(reference_time, plot_timezone)
        }
        if (is.na(now_ts)) {
            now_ts <- Sys.time()
        }

        default_view_start <- now_ts - as.difftime(7, units = "days")
        default_view_end <- now_ts + as.difftime(28, units = "days")

        view_start <- if (!is.null(start_date)) {
            safe_as_posix(start_date, plot_timezone)
        } else {
            default_view_start
        }
        view_end <- if (!is.null(end_date)) {
            safe_as_posix(end_date, plot_timezone)
        } else {
            default_view_end
        }
        if (is.na(view_start)) {
            view_start <- default_view_start
        }
        if (is.na(view_end)) {
            view_end <- default_view_end
        }

        if (is.null(continuous_data) && !is.null(con)) {
            continuous_data <- get_station_timeseries(
                location_code = location_code,
                parameter = parameter,
                reference_time = now_ts,
                load_entire_record = load_entire_record,
                con = con,
                historical_start_year = historical_start_year
            )
        }
        if (!"location_code" %in% names(continuous_data)) {
            continuous_data$location_code <- location_code
        }

        series <- continuous_data[
            continuous_data$location_code == location_code,
        ]
        if (nrow(series) == 0) {
            return(empty_plotly_widget(
                title = sprintf("No data for %s (%s)", location_code, parameter)
            ))
        }

        tz_attr <- attr(series$datetime, "tzone")
        if (
            length(tz_attr) == 0 || is.na(tz_attr[[1]]) || !nzchar(tz_attr[[1]])
        ) {
            plot_timezone <- "America/Whitehorse"
        } else {
            plot_timezone <- tz_attr[[1]]
        }

        coerce_tz <- function(x) {
            x <- safe_as_posix(x, tz = plot_timezone)
            attr(x, "tzone") <- plot_timezone
            x
        }
        series$datetime <- coerce_tz(series$datetime)
        series <- series[order(series$datetime), , drop = FALSE]
        now_ts <- coerce_tz(now_ts)
        view_start <- coerce_tz(view_start)
        view_end <- coerce_tz(view_end)

        latest_obs <- max(series$datetime, na.rm = TRUE)
        fixed_view_end <- now_ts + as.difftime(28, units = "days")
        if (is.finite(latest_obs)) {
            view_start <- min(view_start, latest_obs, now_ts)
            view_end <- fixed_view_end
        } else {
            view_start <- min(view_start, now_ts)
            view_end <- fixed_view_end
        }

        pct <- data.frame()
        envelope_xlim <- NULL
        if (include_percentiles) {
            if (is.null(percentiles) && !is.null(con)) {
                percentiles <- get_daily_percentiles(
                    location_codes = c(location_code),
                    parameter = parameter,
                    con = con,
                    historical_start_year = historical_start_year
                )
            }
            if (!is.null(percentiles) && nrow(percentiles) > 0) {
                pct <- percentiles[percentiles$location_code == location_code, ]
            }
        }

        p <- plotly::plot_ly()

        if (include_percentiles && nrow(pct) > 0) {
            ref_year <- suppressWarnings(as.integer(format(
                now_ts,
                "%Y",
                tz = plot_timezone
            )))
            if (is.na(ref_year)) {
                ref_year <- as.integer(format(
                    Sys.time(),
                    "%Y",
                    tz = plot_timezone
                ))
            }
            # Build a padded envelope around the visible window, but keep it
            # bounded to the configured historical period to avoid oversized
            # ribbon polygons that can trigger plot clipping artifacts.
            historical_start_date <- as.Date(sprintf(
                "%d-01-01",
                historical_start_year
            ))
            historical_end_date <- as.Date(sprintf("%d-12-31", ref_year + 1L))
            padded_view_start <- as.Date(view_start, tz = plot_timezone) - 366
            padded_view_end <- as.Date(view_end, tz = plot_timezone) + 366

            pct_cycle_start <- max(historical_start_date, padded_view_start)
            pct_cycle_end <- min(historical_end_date, padded_view_end)
            if (pct_cycle_end < pct_cycle_start) {
                pct_cycle_start <- historical_start_date
                pct_cycle_end <- min(
                    historical_end_date,
                    historical_start_date + 366
                )
            }
            pct_dates <- seq(pct_cycle_start, pct_cycle_end, by = "day")
            pct_years <- as.integer(format(pct_dates, "%Y"))
            pct_months <- as.integer(format(pct_dates, "%m"))
            pct_days <- as.integer(format(pct_dates, "%d"))
            pct_doys <- as.integer(format(pct_dates, "%j"))
            pct_is_leap <- (pct_years %% 4 == 0 & pct_years %% 100 != 0) |
                (pct_years %% 400 == 0)

            pct_plot <- data.frame(
                location_code = location_code,
                datetime = as.POSIXct(pct_dates, tz = plot_timezone),
                doy = ifelse(
                    pct_months > 2 & pct_is_leap,
                    pct_doys - 1L,
                    pct_doys
                ),
                stringsAsFactors = FALSE
            )
            pct_plot$is_feb29 <- pct_months == 2 & pct_days == 29

            pct_lookup <- pct[, c(
                "location_code",
                "doy",
                "p0",
                "p10",
                "p25",
                "p50",
                "p75",
                "p90",
                "p100"
            )]
            pct_key <- paste(pct_lookup$location_code, pct_lookup$doy)
            plot_key <- paste(pct_plot$location_code, pct_plot$doy)
            match_idx <- match(plot_key, pct_key)
            for (col in c("p0", "p10", "p25", "p50", "p75", "p90", "p100")) {
                pct_plot[[col]] <- pct_lookup[[col]][match_idx]
            }

            # Percentile queries intentionally exclude Feb 29; synthesize leap-day
            # values by linear interpolation between Feb 28 (DOY 59) and Mar 1 (DOY 60).
            feb29_rows <- which(pct_plot$is_feb29)
            if (length(feb29_rows) > 0) {
                feb28_key <- paste(location_code, 59L)
                mar01_key <- paste(location_code, 60L)
                feb28_idx <- match(feb28_key, pct_key)
                mar01_idx <- match(mar01_key, pct_key)

                for (col in c(
                    "p0",
                    "p10",
                    "p25",
                    "p50",
                    "p75",
                    "p90",
                    "p100"
                )) {
                    v0 <- pct_lookup[[col]][feb28_idx]
                    v1 <- pct_lookup[[col]][mar01_idx]
                    if (is.finite(v0) && is.finite(v1)) {
                        pct_plot[feb29_rows, col] <- (v0 + v1) / 2
                    }
                }
            }
            pct_plot <- pct_plot[order(pct_plot$datetime), ]

            # Prevent ribbon self-clipping caused by NA gaps in percentile bands.
            pct_plot <- pct_plot[
                stats::complete.cases(pct_plot[, c(
                    "datetime",
                    "p0",
                    "p10",
                    "p25",
                    "p50",
                    "p75",
                    "p90",
                    "p100"
                )]),
            ]
            pct_plot$is_feb29 <- NULL

            if (nrow(pct_plot) > 0) {
                envelope_xlim <- range(pct_plot$datetime, na.rm = TRUE)
                if (any(!is.finite(envelope_xlim))) {
                    envelope_xlim <- NULL
                }

                p <- p %>%
                    plotly::add_ribbons(
                        data = pct_plot,
                        x = ~datetime,
                        ymin = ~p0,
                        ymax = ~p10,
                        name = "P0-P10",
                        legendrank = 80,
                        hovertemplate = "P0-P10<extra></extra>",
                        fillcolor = "rgba(127, 29, 29, 0.3)",
                        line = list(color = "transparent")
                    ) %>%
                    plotly::add_ribbons(
                        data = pct_plot,
                        x = ~datetime,
                        ymin = ~p10,
                        ymax = ~p25,
                        name = "P10-P25",
                        legendrank = 70,
                        hovertemplate = "P10-P25<extra></extra>",
                        fillcolor = "rgba(180, 83, 9, 0.3)",
                        line = list(color = "transparent")
                    ) %>%
                    plotly::add_ribbons(
                        data = pct_plot,
                        x = ~datetime,
                        ymin = ~p25,
                        ymax = ~p50,
                        name = "P25-P50",
                        legendrank = 60,
                        hovertemplate = "P25-P50<extra></extra>",
                        fillcolor = "rgba(245, 158, 11, 0.3)",
                        line = list(color = "transparent")
                    ) %>%
                    plotly::add_ribbons(
                        data = pct_plot,
                        x = ~datetime,
                        ymin = ~p50,
                        ymax = ~p75,
                        name = "P50-P75",
                        legendrank = 50,
                        hovertemplate = "P50-P75<extra></extra>",
                        fillcolor = "rgba(163, 230, 53, 0.3)",
                        line = list(color = "transparent")
                    ) %>%
                    plotly::add_ribbons(
                        data = pct_plot,
                        x = ~datetime,
                        ymin = ~p75,
                        ymax = ~p90,
                        name = "P75-P90",
                        legendrank = 40,
                        hovertemplate = "P75-P90<extra></extra>",
                        fillcolor = "rgba(34, 197, 94, 0.3)",
                        line = list(color = "transparent")
                    ) %>%
                    plotly::add_ribbons(
                        data = pct_plot,
                        x = ~datetime,
                        ymin = ~p90,
                        ymax = ~p100,
                        name = "P90-P100",
                        legendrank = 30,
                        hovertemplate = "P90-P100<extra></extra>",
                        fillcolor = "rgba(15, 118, 110, 0.3)",
                        line = list(color = "transparent")
                    )
            }
        }

        observed_name <- if (identical(parameter, "precipitation (1wk)")) {
            "Observed 1-week accumulation"
        } else if (identical(parameter, "precipitation (24hr)")) {
            "Observed 24-hour"
        } else {
            "Observed"
        }

        if (!"trace_source" %in% names(series)) {
            series$trace_source <- "observed"
        }

        if (isTRUE(load_entire_record)) {
            hist_series <- series[series$trace_source == "historical_daily", ]
            rt_series <- series[series$trace_source == "realtime_continuous", ]
            if (nrow(hist_series) > 0) {
                p <- p %>%
                    plotly::add_lines(
                        data = hist_series,
                        x = ~datetime,
                        y = ~value,
                        name = "Daily observed",
                        legendrank = 20,
                        hovertemplate = "Daily observed: %{y:.3f}<extra></extra>",
                        line = list(color = "#4b5563", width = 1)
                    )
            }
            if (nrow(rt_series) > 0) {
                p <- p %>%
                    plotly::add_lines(
                        data = rt_series,
                        x = ~datetime,
                        y = ~value,
                        name = observed_name,
                        legendrank = 10,
                        hovertemplate = paste0(
                            observed_name,
                            ": %{y:.3f}<extra></extra>"
                        ),
                        line = list(color = "#000000", width = 2)
                    )
            }
            if (nrow(hist_series) == 0 && nrow(rt_series) == 0) {
                p <- p %>%
                    plotly::add_lines(
                        data = series,
                        x = ~datetime,
                        y = ~value,
                        name = observed_name,
                        legendrank = 10,
                        hovertemplate = paste0(
                            observed_name,
                            ": %{y:.3f}<extra></extra>"
                        ),
                        line = list(color = "#000000", width = 2)
                    )
            }
        } else {
            p <- p %>%
                plotly::add_lines(
                    data = series,
                    x = ~datetime,
                    y = ~value,
                    name = observed_name,
                    legendrank = 10,
                    hovertemplate = paste0(
                        observed_name,
                        ": %{y:.3f}<extra></extra>"
                    ),
                    line = list(color = "#000000", width = 1)
                )
        }

        rp_shapes <- list()
        rp_annotations <- list()
        if (include_return_periods) {
            if (is.null(return_periods) && !is.null(con)) {
                return_periods <- tryCatch(
                    get_return_period_discharge(
                        location_codes = c(location_code),
                        parameter = parameter,
                        con = con
                    ),
                    error = function(e) data.frame()
                )
            }
            if (!is.null(return_periods) && nrow(return_periods) > 0) {
                rp <- return_periods[
                    return_periods$location_code == location_code,
                ]
                if (nrow(rp) > 0) {
                    rp_cols <- grep("^rp_", names(rp), value = TRUE)
                    if (length(rp_cols) > 0) {
                        rp_x0 <- if (!is.null(envelope_xlim)) {
                            envelope_xlim[[1]]
                        } else {
                            min(series$datetime, na.rm = TRUE)
                        }
                        rp_x1 <- if (!is.null(envelope_xlim)) {
                            envelope_xlim[[2]]
                        } else {
                            max(series$datetime, na.rm = TRUE)
                        }
                        rp_label_x <- 0.995 - c(0, 0.03, 0.06, 0.09)
                        for (i in seq_along(rp_cols)) {
                            rp_years <- sub("^rp_", "", rp_cols[i])
                            rp_val <- rp[[rp_cols[i]]][1]
                            rp_shapes[[i]] <- list(
                                type = "line",
                                x0 = rp_x0,
                                x1 = rp_x1,
                                y0 = rp_val,
                                y1 = rp_val,
                                line = list(color = "#dc2626", width = 1)
                            )
                            rp_annotations[[i]] <- list(
                                x = rp_label_x[
                                    ((i - 1) %% length(rp_label_x)) + 1
                                ],
                                y = rp_val,
                                xref = "paper",
                                yref = "y",
                                text = sprintf("RP %s", rp_years),
                                showarrow = FALSE,
                                xanchor = "right",
                                bgcolor = "rgba(255,255,255,0.85)",
                                bordercolor = "rgba(220,38,38,0.35)",
                                borderwidth = 1
                            )
                        }
                    }
                }
            }
        }

        now_line <- list(
            type = "line",
            x0 = now_ts,
            x1 = now_ts,
            xref = "x",
            y0 = 0,
            y1 = 1,
            yref = "paper",
            line = list(color = "#9ca3af", dash = "dash")
        )

        p %>%
            plotly::layout(
                xaxis = list(
                    title = "Date",
                    range = c(
                        format(view_start, "%Y-%m-%d %H:%M:%S"),
                        format(view_end, "%Y-%m-%d %H:%M:%S")
                    ),
                    hoverformat = "%Y-%m-%d"
                ),
                yaxis = list(title = parameter_axis_title(parameter)),
                hovermode = "x unified",
                showlegend = FALSE,
                shapes = c(list(now_line), rp_shapes),
                annotations = rp_annotations,
                legend = list(orientation = "v", x = 0.01, y = 0.99),
                margin = list(r = 80, t = 60)
            )
    }

    build_station_plot <- function(
        location_code,
        parameter,
        reference_time,
        con,
        snow_survey_parameters,
        load_entire_record = FALSE,
        continuous_data = NULL,
        percentiles = NULL,
        return_periods = NULL
    ) {
        tryCatch(
            suppressWarnings({
                if (is.null(continuous_data)) {
                    continuous_data <- get_station_timeseries(
                        location_code = location_code,
                        parameter = parameter,
                        reference_time = reference_time,
                        load_entire_record = load_entire_record,
                        con = con,
                        historical_start_year = 2020L
                    )
                }
                if (is.null(continuous_data) || nrow(continuous_data) == 0) {
                    return(simple_continuous_plotly_widget(
                        location_code = location_code,
                        parameter = parameter,
                        continuous_data = continuous_data,
                        title_prefix = sprintf(
                            "%s [%s]",
                            location_code,
                            parameter
                        )
                    ))
                }
                if (!"location_code" %in% names(continuous_data)) {
                    continuous_data$location_code <- location_code
                }
                tryCatch(
                    plot_continuous_with_percentiles_and_return_periods(
                        location_code = location_code,
                        continuous_data = continuous_data,
                        percentiles = percentiles,
                        return_periods = return_periods,
                        parameter = parameter,
                        reference_time = as.character(reference_time),
                        load_entire_record = load_entire_record,
                        con = con,
                        historical_start_year = 2020L
                    ),
                    error = function(e) {
                        simple_continuous_plotly_widget(
                            location_code = location_code,
                            parameter = parameter,
                            continuous_data = continuous_data,
                            title_prefix = sprintf(
                                "%s [%s]",
                                location_code,
                                parameter
                            )
                        )
                    }
                )
            }),
            error = function(e) {
                simple_continuous_plotly_widget(
                    location_code = location_code,
                    parameter = parameter,
                    continuous_data = NULL,
                    title_prefix = sprintf("%s [%s]", location_code, parameter)
                )
            }
        )
    }

    # End plot helpers -----------------------------------------------------------

    shiny::moduleServer(id, function(input, output, session) {
        shiny::req(language$language)

        con <- session$userData$AquaCache
        shiny::req(con)

        fva <- read_fva_json()
        communities <- names(fva)
        current_calendar_year <- as.integer(format(Sys.Date(), "%Y"))
        historical_year_choices <- if (current_calendar_year > 2000L) {
            as.character(seq(current_calendar_year - 1L, 2000L))
        } else {
            character(0)
        }

        shiny::observeEvent(
            TRUE,
            {
                shiny::req(length(communities) > 0)

                current <- input$community
                selected <- if (!is.null(current) && current %in% communities) {
                    current
                } else {
                    communities[[1]]
                }

                shiny::updateSelectInput(
                    session,
                    inputId = "community",
                    choices = communities,
                    selected = selected
                )

                shiny::updateSelectizeInput(
                    session,
                    inputId = "primary_historical_years",
                    choices = historical_year_choices,
                    selected = character(0),
                    server = TRUE
                )

                shiny::updateSelectizeInput(
                    session,
                    inputId = "secondary_historical_years",
                    choices = historical_year_choices,
                    selected = character(0),
                    server = TRUE
                )
            },
            once = TRUE
        )

        community_locations <- shiny::reactive({
            shiny::req(input$community)

            community_data <- fva[[input$community]]
            dat <- location_lookup_for_community(community_data, con)
            encoded <- get_encoded_gauge_metadata(community_data)

            if (is.null(dat) || nrow(dat) == 0) {
                return(dat)
            }

            if (!is.null(encoded) && nrow(encoded) > 0) {
                dat$encoding <- encoded$encoding[
                    match(dat$location_code, encoded$location_code)
                ]
                dat$gauge_name <- encoded$gauge_name[
                    match(dat$location_code, encoded$location_code)
                ]
            } else {
                dat$encoding <- NA_real_
                dat$gauge_name <- NA_character_
            }

            dat <- dat[
                !is.na(dat$longitude) & !is.na(dat$latitude),
                ,
                drop = FALSE
            ]
            dat
        })

        snow_survey_parameters <- c(
            "snow water eq (survey)",
            "snow depth (survey)"
        )
        precipitation_parameters <- c(
            "precipitation (1wk)",
            "precipitation (24hr)"
        )

        summary_data <- shiny::reactive({
            dat <- community_locations()
            if (is.null(dat) || nrow(dat) == 0) {
                return(data.frame())
            }

            selected_parameter <- input$parameter
            if (is.null(selected_parameter) || !nzchar(selected_parameter)) {
                selected_parameter <- "water level"
            }

            reference_time <- if (
                !is.null(input$time0) && nzchar(input$time0)
            ) {
                suppressWarnings(as.POSIXct(
                    input$time0,
                    format = "%Y-%m-%dT%H:%M"
                ))
            } else {
                NULL
            }

            location_codes <- unique(stats::na.omit(as.character(
                dat$location_code
            )))

            if (selected_parameter %in% precipitation_parameters) {
                summary_metrics <- get_precipitation_timeseries_summary(
                    location_codes = location_codes,
                    con = con,
                    reference_time = reference_time
                )
                if (is.null(summary_metrics) || nrow(summary_metrics) == 0) {
                    return(data.frame())
                }
                idx <- match(dat$location_code, summary_metrics$location_code)
                dat$current_value <- summary_metrics$precipitation_1w_accumulation[
                    idx
                ]
                dat$precip_1w <- summary_metrics$precipitation_1w_accumulation[
                    idx
                ]
                dat$precip_1m <- summary_metrics$precipitation_1m_accumulation[
                    idx
                ]
                dat$precip_6m <- summary_metrics$precipitation_6m_accumulation[
                    idx
                ]
                dat$latest_time <- summary_metrics$latest_time[idx]
                dat$last_data_age_hours <- summary_metrics$last_data_age_hours[
                    idx
                ]
                dat <- dat[!is.na(dat$latest_time), , drop = FALSE]
                dat$summary_type <- "precipitation"
                return(dat)
            }

            if (selected_parameter %in% snow_survey_parameters) {
                summary_metrics <- get_snow_survey_summary(
                    location_codes = location_codes,
                    parameter = selected_parameter,
                    con = con,
                    reference_time = reference_time
                )
                if (is.null(summary_metrics) || nrow(summary_metrics) == 0) {
                    return(data.frame())
                }
                idx <- match(dat$location_code, summary_metrics$location_code)
                dat$current_value <- summary_metrics$current_value[idx]
                dat$march_1_value <- summary_metrics$march_1_value[idx]
                dat$april_1_value <- summary_metrics$april_1_value[idx]
                dat$may_1_value <- summary_metrics$may_1_value[idx]
                dat$latest_time <- summary_metrics$latest_time[idx]
                dat$last_data_age_hours <- summary_metrics$last_data_age_hours[
                    idx
                ]
                dat <- dat[!is.na(dat$current_value), , drop = FALSE]
                dat$summary_type <- "snow_survey"
                return(dat)
            }

            summary_metrics <- if (identical(selected_parameter, "FDD")) {
                get_fdd_summary(
                    location_codes = location_codes,
                    con = con,
                    reference_time = reference_time
                )
            } else if (identical(selected_parameter, "DDT")) {
                get_ddt_summary(
                    location_codes = location_codes,
                    con = con,
                    reference_time = reference_time
                )
            } else {
                get_latest_parameter_summary(
                    location_codes = location_codes,
                    parameter = selected_parameter,
                    con = con,
                    reference_time = reference_time
                )
            }

            if (is.null(summary_metrics) || nrow(summary_metrics) == 0) {
                return(data.frame())
            }

            idx <- match(dat$location_code, summary_metrics$location_code)
            dat$current_value <- summary_metrics$current_value[idx]
            dat$change_24h <- summary_metrics$change_24h[idx]
            dat$change_48h <- summary_metrics$change_48h[idx]
            dat$change_1w <- summary_metrics$change_1w[idx]
            dat$latest_time <- summary_metrics$latest_time[idx]
            dat$last_data_age_hours <- summary_metrics$last_data_age_hours[idx]

            dat <- dat[
                !is.na(dat$current_value) | !is.na(dat$latest_time),
                ,
                drop = FALSE
            ]
            dat$summary_type <- "standard"
            dat
        })

        available_primary_stations <- shiny::reactive({
            dat <- community_locations()
            if (is.null(dat) || nrow(dat) == 0) {
                return(data.frame())
            }

            selected_parameter <- input$parameter
            if (is.null(selected_parameter) || !nzchar(selected_parameter)) {
                selected_parameter <- "water level"
            }

            # Snow survey data lives in the discrete samples tables, not in
            # timeseries. Use summary_data() which already queries that path.
            if (selected_parameter %in% snow_survey_parameters) {
                sd <- summary_data()
                if (is.null(sd) || nrow(sd) == 0) {
                    return(data.frame())
                }
                return(dat[
                    dat$location_id %in% sd$location_id,
                    ,
                    drop = FALSE
                ])
            }

            eligible_ids <- tryCatch(
                get_location_ids_with_parameter(
                    location_ids = dat$location_id,
                    parameter = selected_parameter,
                    con = con
                ),
                error = function(e) integer(0)
            )

            dat[
                dat$location_id %in% eligible_ids,
                ,
                drop = FALSE
            ]
        })

        shiny::observe({
            dat <- available_primary_stations()

            if (is.null(dat) || nrow(dat) == 0) {
                shiny::updateSelectizeInput(
                    session,
                    inputId = "station",
                    choices = c("No stations available" = ""),
                    selected = "",
                    server = TRUE
                )
                return()
            }

            station_choices <- stats::setNames(
                dat$location_code,
                ifelse(
                    !is.na(dat$name) & nzchar(dat$name),
                    paste0(dat$name, " (", dat$location_code, ")"),
                    dat$location_code
                )
            )

            shiny::updateSelectizeInput(
                session,
                inputId = "station",
                choices = station_choices,
                selected = shiny::isolate({
                    if (
                        !is.null(input$station) &&
                            nzchar(input$station) &&
                            input$station %in% dat$location_code
                    ) {
                        input$station
                    } else {
                        dat$location_code[[1]]
                    }
                }),
                server = TRUE
            )
        })

        # Update secondary_parameter choices from community stations.
        shiny::observe({
            dat <- community_locations()

            parameter_candidates <- c(
                "water level",
                "water flow",
                "precipitation (1wk)",
                "precipitation (24hr)",
                "temperature, air",
                "FDD",
                "DDT",
                "snow water eq (pillow)",
                "snow depth (pillow)",
                "snow water eq (survey)",
                "snow depth (survey)"
            )

            if (is.null(dat) || nrow(dat) == 0) {
                shiny::updateSelectizeInput(
                    session,
                    inputId = "secondary_parameter",
                    choices = character(0),
                    selected = character(0),
                    server = TRUE
                )
                return()
            }

            current <- shiny::isolate(input$secondary_parameter)
            new_selected <- if (
                !is.null(current) &&
                    nzchar(current) &&
                    current %in% parameter_candidates
            ) {
                current
            } else {
                character(0)
            }

            shiny::updateSelectizeInput(
                session,
                inputId = "secondary_parameter",
                choices = parameter_candidates,
                selected = new_selected,
                server = TRUE
            )
        })

        # Update secondary_station choices when secondary_parameter changes.
        shiny::observe({
            sec_param <- input$secondary_parameter
            all_locs <- community_locations()
            primary <- input$station %||% ""

            if (
                is.null(sec_param) ||
                    !nzchar(sec_param) ||
                    is.null(all_locs) ||
                    nrow(all_locs) == 0
            ) {
                shiny::updateSelectizeInput(
                    session,
                    inputId = "secondary_station",
                    choices = c("No stations available" = ""),
                    selected = "",
                    server = TRUE
                )
                return()
            }

            # Filter to stations that actually have this parameter.
            # Snow survey data is in discrete tables, not timeseries.
            eligible <- if (sec_param %in% snow_survey_parameters) {
                sd <- tryCatch(
                    get_snow_survey_summary(
                        location_codes = all_locs$location_code,
                        parameter = sec_param,
                        con = con
                    ),
                    error = function(e) data.frame()
                )
                if (is.null(sd) || nrow(sd) == 0) {
                    all_locs[integer(0), , drop = FALSE]
                } else {
                    all_locs[
                        all_locs$location_id %in% sd$location_id,
                        ,
                        drop = FALSE
                    ]
                }
            } else {
                eligible_ids <- tryCatch(
                    get_location_ids_with_parameter(
                        location_ids = all_locs$location_id,
                        parameter = sec_param,
                        con = con
                    ),
                    error = function(e) integer(0)
                )
                all_locs[
                    all_locs$location_id %in% eligible_ids,
                    ,
                    drop = FALSE
                ]
            }

            # Exclude the currently selected primary station.
            eligible <- eligible[
                eligible$location_code != primary,
                ,
                drop = FALSE
            ]

            if (nrow(eligible) == 0) {
                shiny::updateSelectizeInput(
                    session,
                    inputId = "secondary_station",
                    choices = c("No stations available" = ""),
                    selected = "",
                    server = TRUE
                )
                return()
            }

            sec_choices <- stats::setNames(
                eligible$location_code,
                ifelse(
                    !is.na(eligible$name) & nzchar(eligible$name),
                    paste0(eligible$name, " (", eligible$location_code, ")"),
                    eligible$location_code
                )
            )

            current <- shiny::isolate(input$secondary_station)
            new_selected <- if (
                !is.null(current) &&
                    nzchar(current) &&
                    current %in% eligible$location_code
            ) {
                current
            } else {
                character(0)
            }

            shiny::updateSelectizeInput(
                session,
                inputId = "secondary_station",
                choices = sec_choices,
                selected = new_selected,
                server = TRUE
            )
        })

        community_basins <- shiny::reactive({
            shiny::req(input$community)

            community_data <- fva[[input$community]]
            basin_codes <- community_location_codes_from_json(community_data)

            if (length(basin_codes) == 0) {
                return(NULL)
            }

            get_spatial_layer_as_sf(
                layer_name = "Drainage basins",
                feature_name = basin_codes,
                con = con,
                epsg = 4326
            )
        })

        community_roads <- shiny::reactive({
            roads <- get_spatial_layer_as_sf(
                layer_name = "Roads",
                con = con,
                epsg = 4326
            )

            if (is.null(roads) || nrow(roads) == 0) {
                return(NULL)
            }

            roads
        })

        community_communities <- shiny::reactive({
            communities <- get_spatial_layer_as_sf(
                layer_name = "Communities",
                con = con,
                epsg = 4326
            )

            if (is.null(communities) || nrow(communities) == 0) {
                return(NULL)
            }

            communities
        })

        normalize_image_extension <- function(format_value) {
            ext <- tolower(trimws(as.character(format_value[[1]])))
            if (is.na(ext) || !nzchar(ext)) {
                ext <- "jpg"
            }
            ext <- gsub("^\\.", "", ext)

            if (ext %in% c("jpg", "jpeg")) {
                return("jpeg")
            }
            if (ext %in% c("tif", "tiff")) {
                return("tiff")
            }
            if (ext == "svg") {
                return("svg+xml")
            }

            ext
        }

        coerce_image_blob_to_raw <- function(blob) {
            if (is.null(blob)) {
                return(NULL)
            }

            if (is.list(blob)) {
                if (length(blob) == 0) {
                    return(NULL)
                }
                return(coerce_image_blob_to_raw(blob[[1]]))
            }

            if (is.raw(blob)) {
                return(blob)
            }

            if (is.character(blob) && length(blob) == 1 && nzchar(blob)) {
                hex <- blob[[1]]
                if (startsWith(hex, "\\\\x")) {
                    hex <- substr(hex, 3, nchar(hex))
                }
                is_hex <- grepl("^[0-9A-Fa-f]+$", hex)
                if (is_hex && (nchar(hex) %% 2) == 0) {
                    bytes <- substring(
                        hex,
                        first = seq(1, nchar(hex), by = 2),
                        last = seq(2, nchar(hex), by = 2)
                    )
                    return(as.raw(strtoi(bytes, base = 16L)))
                }
            }

            if (is.numeric(blob) || is.integer(blob)) {
                vals <- as.integer(blob)
                vals <- vals[!is.na(vals)]
                if (length(vals) == 0) {
                    return(NULL)
                }
                vals <- pmin(255L, pmax(0L, vals))
                return(as.raw(vals))
            }

            stop("Unsupported image byte format")
        }

        image_blob_to_data_uri <- function(image_blob, image_format) {
            raw_bytes <- coerce_image_blob_to_raw(image_blob)
            if (is.null(raw_bytes) || length(raw_bytes) == 0) {
                return(NULL)
            }

            mime <- paste0("image/", normalize_image_extension(image_format))
            base64enc::dataURI(data = raw_bytes, mime = mime)
        }

        get_image_series_locations_as_sf <- function(con) {
            image_series_locations <- YGwater::dbGetQueryDT(
                paste(
                    "SELECT",
                    "    s.img_series_id,",
                    "    s.location_id,",
                    "    l.location_code,",
                    "    l.name,",
                    "    l.latitude,",
                    "    l.longitude",
                    "FROM image_series s",
                    "JOIN locations l",
                    "  ON l.location_id = s.location_id",
                    "WHERE l.latitude IS NOT NULL",
                    "  AND l.longitude IS NOT NULL",
                    "ORDER BY l.name, s.img_series_id"
                ),
                con = con
            )

            sf::st_as_sf(
                image_series_locations,
                coords = c("longitude", "latitude"),
                crs = 4326,
                remove = FALSE
            )
        }

        image_series_locations_sf <- shiny::reactive({
            tryCatch(
                get_image_series_locations_as_sf(con),
                error = function(e) {
                    sf::st_as_sf(
                        data.frame(
                            img_series_id = integer(),
                            location_id = integer(),
                            location_code = character(),
                            name = character(),
                            latitude = numeric(),
                            longitude = numeric(),
                            stringsAsFactors = FALSE
                        ),
                        coords = c("longitude", "latitude"),
                        crs = 4326,
                        remove = FALSE
                    )
                }
            )
        })

        selected_img_series_id <- shiny::reactiveVal(NULL)
        selected_image_ts_index <- shiny::reactiveVal(NA_integer_)

        community_image_series <- shiny::reactive({
            basins <- community_basins()
            image_series_sf <- image_series_locations_sf()

            if (
                is.null(basins) ||
                    nrow(basins) == 0 ||
                    is.null(image_series_sf) ||
                    nrow(image_series_sf) == 0
            ) {
                return(image_series_sf[0, , drop = FALSE])
            }

            basins_projected <- sf::st_transform(basins, 3578)
            image_series_projected <- sf::st_transform(image_series_sf, 3578)

            basin_cols <- intersect(
                c("feature_name", "layer_name"),
                names(
                    basins_projected
                )
            )

            basins_buffered <- sf::st_buffer(
                basins_projected[basin_cols],
                25000
            )

            series_in_basins <- sf::st_join(
                image_series_projected,
                basins_buffered,
                join = sf::st_intersects,
                left = FALSE
            )

            if (nrow(series_in_basins) == 0) {
                return(series_in_basins)
            }

            series_in_basins <- series_in_basins[
                order(series_in_basins$name, series_in_basins$img_series_id),
            ]
            series_in_basins <- series_in_basins[
                !duplicated(series_in_basins$img_series_id),
            ]

            sf::st_transform(series_in_basins, 4326)
        })

        selected_image_series <- shiny::reactive({
            series <- community_image_series()
            shiny::req(nrow(series) > 0)

            sid <- selected_img_series_id()
            if (!is.null(sid) && sid %in% series$img_series_id) {
                series[series$img_series_id == sid, , drop = FALSE][
                    1,
                    ,
                    drop = FALSE
                ]
            } else {
                series[1, , drop = FALSE]
            }
        })

        available_image_timestamps <- shiny::reactive({
            image_series <- selected_image_series()
            shiny::req(!is.null(image_series) && nrow(image_series) > 0)

            img_series_id <- image_series$img_series_id[[1]]

            time0 <- if (!is.null(input$time0) && nzchar(input$time0)) {
                parsed <- suppressWarnings(as.POSIXct(
                    input$time0,
                    format = "%Y-%m-%dT%H:%M",
                    tz = "Etc/GMT+7"
                ))
                if (is.na(parsed)) Sys.time() else parsed
            } else {
                Sys.time()
            }

            img_series_id_int <- as.integer(img_series_id)
            if (is.na(img_series_id_int)) {
                return(data.frame(
                    image_id = integer(0),
                    datetime = character(0)
                ))
            }

            time0_sql <- DBI::dbQuoteString(
                con,
                format(as.POSIXct(time0), "%Y-%m-%d %H:%M:%S")
            )
            time_min_sql <- DBI::dbQuoteString(
                con,
                format(as.POSIXct(time0) - 7 * 24 * 3600, "%Y-%m-%d %H:%M:%S")
            )

            DBI::dbGetQuery(
                con,
                paste0(
                    "SELECT image_id, datetime FROM images WHERE img_series_id = ",
                    img_series_id_int,
                    " AND datetime <= ",
                    time0_sql,
                    " AND datetime >= ",
                    time_min_sql,
                    " ORDER BY datetime ASC"
                )
            )
        })

        shiny::observeEvent(
            input$community,
            {
                selected_img_series_id(NULL)
                selected_image_ts_index(NA_integer_)
            },
            ignoreInit = FALSE
        )

        shiny::observeEvent(
            community_image_series(),
            {
                series <- community_image_series()
                if (is.null(series) || nrow(series) == 0) {
                    selected_img_series_id(NULL)
                    selected_image_ts_index(NA_integer_)
                    return()
                }

                sid <- selected_img_series_id()
                if (is.null(sid) || !(sid %in% series$img_series_id)) {
                    selected_img_series_id(series$img_series_id[[1]])
                }
                selected_image_ts_index(NA_integer_)
            },
            ignoreInit = FALSE
        )

        shiny::observeEvent(
            input$image_location_select,
            {
                selected_img_series_id(as.integer(input$image_location_select))
                selected_image_ts_index(NA_integer_)
            },
            ignoreNULL = TRUE
        )

        shiny::observeEvent(input$previous_image_series, {
            timestamps <- available_image_timestamps()
            shiny::req(nrow(timestamps) > 0)

            idx <- selected_image_ts_index()
            if (
                length(idx) != 1 ||
                    is.na(idx) ||
                    idx < 1 ||
                    idx > nrow(timestamps)
            ) {
                idx <- nrow(timestamps)
            }

            selected_image_ts_index(
                if (idx <= 1L) 1L else idx - 1L
            )
        })

        shiny::observeEvent(input$next_image_series, {
            timestamps <- available_image_timestamps()
            shiny::req(nrow(timestamps) > 0)

            idx <- selected_image_ts_index()
            if (
                length(idx) != 1 ||
                    is.na(idx) ||
                    idx < 1 ||
                    idx > nrow(timestamps)
            ) {
                idx <- nrow(timestamps)
            }

            selected_image_ts_index(
                if (idx >= nrow(timestamps)) nrow(timestamps) else idx + 1L
            )
        })

        output$image_series_header <- shiny::renderUI({
            series <- community_image_series()

            timestamps <- tryCatch(
                available_image_timestamps(),
                error = function(e) NULL
            )
            ts_str <- if (!is.null(timestamps) && nrow(timestamps) > 0) {
                idx <- selected_image_ts_index()
                if (
                    length(idx) != 1 ||
                        is.na(idx) ||
                        idx < 1 ||
                        idx > nrow(timestamps)
                ) {
                    idx <- nrow(timestamps)
                }
                format(
                    as.POSIXct(timestamps$datetime[[idx]], tz = "Etc/GMT+7"),
                    "%d-%b-%y %H:%M",
                    tz = "Etc/GMT+7"
                )
            } else {
                "No images"
            }

            if (is.null(series) || nrow(series) == 0) {
                return(shiny::tags$span(paste("Images \u2014", ts_str)))
            }

            choices <- stats::setNames(
                as.character(series$img_series_id),
                ifelse(
                    !is.na(series$name) & nzchar(as.character(series$name)),
                    as.character(series$name),
                    as.character(series$img_series_id)
                )
            )

            sid <- selected_img_series_id()
            selected_val <- if (
                !is.null(sid) &&
                    as.character(sid) %in% as.character(series$img_series_id)
            ) {
                as.character(sid)
            } else {
                as.character(series$img_series_id[[1]])
            }

            shiny::tags$div(
                style = "display:flex; align-items:center; gap:0.4rem; flex-wrap:wrap;",
                shiny::selectInput(
                    inputId = session$ns("image_location_select"),
                    label = NULL,
                    choices = choices,
                    selected = selected_val,
                    width = "auto",
                    selectize = FALSE
                ),
                shiny::actionButton(
                    inputId = session$ns("previous_image_series"),
                    label = "\u25c0",
                    style = "padding:0.15rem 0.4rem; font-size:0.85rem; height:auto;"
                ),
                shiny::tags$span(
                    if (!is.null(timestamps) && nrow(timestamps) > 0) {
                        n <- nrow(timestamps)
                        i <- selected_image_ts_index()
                        if (length(i) != 1 || is.na(i) || i < 1 || i > n) {
                            i <- n
                        }
                        sprintf("%d / %d", i, n)
                    } else {
                        "0 / 0"
                    },
                    style = "font-size:0.85rem; min-width:2.5rem; text-align:center; white-space:nowrap;"
                ),
                shiny::actionButton(
                    inputId = session$ns("next_image_series"),
                    label = "\u25b6",
                    style = "padding:0.15rem 0.4rem; font-size:0.85rem; height:auto;"
                ),
                shiny::tags$span(
                    ts_str,
                    style = "white-space:nowrap; font-size:0.85rem; color:#6b7280;"
                )
            )
        })

        output$image_series_navigation <- shiny::renderUI({
            NULL
        })

        output$station_image <- shiny::renderUI({
            timestamps <- available_image_timestamps()
            if (is.null(timestamps) || nrow(timestamps) == 0) {
                return(shiny::tags$div(
                    style = "color:#6b7280;",
                    "No images available in the last 7 days for this location."
                ))
            }

            idx <- selected_image_ts_index()
            if (
                length(idx) != 1 ||
                    is.na(idx) ||
                    idx < 1 ||
                    idx > nrow(timestamps)
            ) {
                idx <- nrow(timestamps)
            }

            image_id <- timestamps$image_id[[idx]]
            image <- DBI::dbGetQuery(
                con,
                paste0(
                    "SELECT format, file FROM images WHERE image_id = ",
                    image_id
                )
            )

            if (nrow(image) == 0 || is.null(image$file[[1]])) {
                return(shiny::tags$div("Image bytes not available"))
            }

            img_src <- tryCatch(
                image_blob_to_data_uri(
                    image_blob = image$file[[1]],
                    image_format = image$format[[1]]
                ),
                error = function(e) NULL
            )

            if (is.null(img_src)) {
                return(shiny::tags$div("Unable to render image"))
            }

            shiny::tags$img(
                src = img_src,
                style = "max-width:100%; max-height:420px; object-fit:contain;"
            )
        })

        output$stations_map <- leaflet::renderLeaflet({
            dat <- community_locations()
            basins <- community_basins()
            roads <- community_roads()
            communities <- community_communities()
            eligible_stations <- available_primary_stations()

            if (
                (is.null(dat) || nrow(dat) == 0) &&
                    (is.null(basins) || nrow(basins) == 0) &&
                    (is.null(roads) || nrow(roads) == 0) &&
                    (is.null(communities) || nrow(communities) == 0)
            ) {
                map <- leaflet::leaflet()
                map <- leaflet::addProviderTiles(map, "CartoDB.Positron")
                map <- leaflet::setView(map, lng = -135, lat = 64, zoom = 5)
                return(
                    map
                )
            }

            dat$longitude <- suppressWarnings(as.numeric(dat$longitude))
            dat$latitude <- suppressWarnings(as.numeric(dat$latitude))
            dat <- dat[
                !is.na(dat$longitude) & !is.na(dat$latitude),
                ,
                drop = FALSE
            ]

            if (nrow(dat) == 0) {
                map <- leaflet::leaflet()
                map <- leaflet::addProviderTiles(map, "CartoDB.Positron")
                map <- leaflet::setView(map, lng = -135, lat = 64, zoom = 5)
            } else {
                eligible_codes <- if (
                    !is.null(eligible_stations) && nrow(eligible_stations) > 0
                ) {
                    unique(as.character(eligible_stations$location_code))
                } else {
                    character(0)
                }
                dat$has_selected_parameter <- dat$location_code %in%
                    eligible_codes
                dat$point_color <- ifelse(
                    dat$has_selected_parameter,
                    "#2563eb",
                    "#16a34a"
                )

                dat$station_name <- ifelse(
                    !is.na(dat$name) & nzchar(dat$name),
                    as.character(dat$name),
                    "Unknown station"
                )
                dat$station_code <- ifelse(
                    !is.na(dat$location_code) & nzchar(dat$location_code),
                    as.character(dat$location_code),
                    ""
                )

                popup_parameter_order <- c(
                    "water level",
                    "water flow",
                    "precipitation (1wk)",
                    "precipitation (24hr)",
                    "temperature, air",
                    "FDD",
                    "DDT",
                    "snow water eq (pillow)",
                    "snow depth (pillow)",
                    "snow water eq (survey)",
                    "snow depth (survey)"
                )

                parameter_available_by_code <- setNames(
                    vector("list", nrow(dat)),
                    dat$location_code
                )

                location_ids_all <- unique(stats::na.omit(as.integer(
                    dat$location_id
                )))
                location_codes_all <- unique(stats::na.omit(as.character(
                    dat$location_code
                )))

                append_available_parameter <- function(code_vec, param_label) {
                    if (length(code_vec) == 0) {
                        return(invisible(NULL))
                    }
                    for (code_i in code_vec) {
                        if (!isTRUE(nzchar(code_i))) {
                            next
                        }
                        existing <- parameter_available_by_code[[code_i]]
                        parameter_available_by_code[[code_i]] <<- unique(c(
                            existing,
                            param_label
                        ))
                    }
                    invisible(NULL)
                }

                timeseries_parameter_candidates <- c(
                    "water level",
                    "water flow",
                    "precipitation (1wk)",
                    "precipitation (24hr)",
                    "temperature, air",
                    "snow water eq (pillow)",
                    "snow depth (pillow)"
                )

                for (param_i in timeseries_parameter_candidates) {
                    eligible_ids_i <- tryCatch(
                        get_location_ids_with_parameter(
                            location_ids = location_ids_all,
                            parameter = param_i,
                            con = con
                        ),
                        error = function(e) integer(0)
                    )
                    if (length(eligible_ids_i) == 0) {
                        next
                    }
                    eligible_codes_i <- dat$location_code[
                        dat$location_id %in% eligible_ids_i
                    ]
                    append_available_parameter(
                        unique(as.character(eligible_codes_i)),
                        param_i
                    )
                }

                temp_codes <- names(parameter_available_by_code)[vapply(
                    parameter_available_by_code,
                    function(x) "temperature, air" %in% x,
                    logical(1)
                )]
                append_available_parameter(temp_codes, "FDD")
                append_available_parameter(temp_codes, "DDT")

                for (survey_param in c(
                    "snow water eq (survey)",
                    "snow depth (survey)"
                )) {
                    survey_summary <- tryCatch(
                        get_snow_survey_summary(
                            location_codes = location_codes_all,
                            parameter = survey_param,
                            con = con
                        ),
                        error = function(e) data.frame()
                    )
                    if (nrow(survey_summary) == 0) {
                        next
                    }
                    append_available_parameter(
                        unique(as.character(survey_summary$location_code)),
                        survey_param
                    )
                }

                popup_params <- vapply(
                    dat$location_code,
                    function(code_i) {
                        vals <- parameter_available_by_code[[code_i]]
                        vals <- vals[vals %in% popup_parameter_order]
                        vals <- popup_parameter_order[
                            popup_parameter_order %in% vals
                        ]
                        if (length(vals) == 0) {
                            return("None")
                        }
                        paste(vals, collapse = "|")
                    },
                    character(1)
                )

                popup_params_html <- vapply(
                    popup_params,
                    function(param_text) {
                        if (identical(param_text, "None")) {
                            return("None")
                        }
                        param_items <- strsplit(
                            param_text,
                            "|",
                            fixed = TRUE
                        )[[1]]
                        paste0(
                            "<ul style='margin:0.25rem 0 0 1rem; padding:0;'>",
                            paste0(
                                "<li>",
                                htmltools::htmlEscape(param_items),
                                "</li>",
                                collapse = ""
                            ),
                            "</ul>"
                        )
                    },
                    character(1)
                )

                map <- leaflet::leaflet(dat)
                map <- leaflet::addProviderTiles(map, "CartoDB.Positron")

                dat$popup_html <- sprintf(
                    paste0(
                        "<strong>Station name:</strong> %s",
                        "<br/><strong>Location code:</strong> %s",
                        "<br/><strong>Available parameters:</strong> %s"
                    ),
                    htmltools::htmlEscape(dat$station_name),
                    htmltools::htmlEscape(dat$station_code),
                    popup_params_html
                )
                dat$tooltip_text <- paste0(
                    dat$station_name,
                    " (",
                    dat$station_code,
                    ")"
                )
            }

            if (!is.null(basins) && nrow(basins) > 0) {
                map <- leaflet::addPolygons(
                    map,
                    data = basins,
                    color = "#334155",
                    weight = 1,
                    fillColor = "#475569",
                    fillOpacity = 0.08,
                    smoothFactor = 0.3,
                    options = leaflet::pathOptions(interactive = FALSE)
                )
            }

            if (!is.null(roads) && nrow(roads) > 0) {
                map <- leaflet::addPolylines(
                    map,
                    data = roads,
                    color = "#64748b",
                    weight = 1.5,
                    opacity = 0.8,
                    options = leaflet::pathOptions(interactive = FALSE)
                )
            }

            if (!is.null(communities) && nrow(communities) > 0) {
                communities <- sf::st_transform(communities, 4326)
                community_coords <- sf::st_coordinates(sf::st_geometry(
                    communities
                ))
                communities$longitude <- community_coords[, 1]
                communities$latitude <- community_coords[, 2]
                communities$feature_name <- if (
                    "feature_name" %in% names(communities)
                ) {
                    as.character(communities$feature_name)
                } else {
                    ""
                }

                map <- leaflet::addCircleMarkers(
                    map,
                    data = communities,
                    lng = ~longitude,
                    lat = ~latitude,
                    radius = 3,
                    stroke = TRUE,
                    weight = 1,
                    color = "#111827",
                    fillColor = "#111827",
                    fillOpacity = 0.9,
                    popup = ~feature_name
                )
            }

            if (!is.null(dat) && nrow(dat) > 0) {
                map <- leaflet::addCircleMarkers(
                    map,
                    data = dat,
                    lng = ~longitude,
                    lat = ~latitude,
                    radius = 5,
                    stroke = TRUE,
                    weight = 1,
                    color = ~point_color,
                    fillColor = ~point_color,
                    fillOpacity = 0.8,
                    popup = ~popup_html
                )
            }

            if (
                (!is.null(dat) && nrow(dat) > 0) ||
                    (!is.null(communities) && nrow(communities) > 0) ||
                    (!is.null(roads) && nrow(roads) > 0)
            ) {
                legend_html <- paste0(
                    "<div style='background:rgba(255,255,255,0.95); padding:8px 10px; border-radius:6px; border:1px solid #d1d5db; font-size:12px; line-height:1.35;'>",
                    "<div style='font-weight:600; margin-bottom:6px;'>Legend</div>",
                    "<div style='display:flex; align-items:center; margin-bottom:4px;'><span style='display:inline-block; width:10px; height:10px; border-radius:50%; background:#2563eb; border:1px solid #1e3a8a; margin-right:6px;'></span>Hydrometric station</div>",
                    "<div style='display:flex; align-items:center; margin-bottom:4px;'><span style='display:inline-block; width:10px; height:10px; border-radius:50%; background:#16a34a; border:1px solid #166534; margin-right:6px;'></span>Meteorological station</div>",
                    "<div style='display:flex; align-items:center; margin-bottom:4px;'><span style='display:inline-block; width:10px; height:10px; border-radius:50%; background:#111827; border:1px solid #111827; margin-right:6px;'></span>Communities</div>",
                    "<div style='display:flex; align-items:center;'><span style='display:inline-block; width:14px; height:0; border-top:2px solid #64748b; margin-right:6px;'></span>Roads</div>",
                    "</div>"
                )

                map <- leaflet::addControl(
                    map,
                    html = legend_html,
                    position = "bottomright"
                )
            }

            if (!is.null(basins) && nrow(basins) > 0) {
                basin_bbox <- sf::st_bbox(sf::st_transform(basins, 4326))
                map <- leaflet::fitBounds(
                    map,
                    lng1 = basin_bbox[["xmin"]],
                    lat1 = basin_bbox[["ymin"]],
                    lng2 = basin_bbox[["xmax"]],
                    lat2 = basin_bbox[["ymax"]]
                )
            } else if (nrow(dat) > 0) {
                map <- leaflet::fitBounds(
                    map,
                    lng1 = min(dat$longitude, na.rm = TRUE),
                    lat1 = min(dat$latitude, na.rm = TRUE),
                    lng2 = max(dat$longitude, na.rm = TRUE),
                    lat2 = max(dat$latitude, na.rm = TRUE)
                )
            }

            map
        })

        output$summary_table <- DT::renderDT({
            dat <- summary_data()
            use_relative <- isTRUE(input$summary_relative)

            if (is.null(dat) || nrow(dat) == 0) {
                return(DT::datatable(
                    data.frame(message = "No stations for current filters"),
                    rownames = FALSE,
                    selection = "single"
                ))
            }

            dt_options <- list(
                pageLength = 8,
                scrollX = TRUE,
                dom = "tip",
                searching = FALSE,
                lengthChange = FALSE
            )

            station_col <- ifelse(
                !is.na(dat$name) & nzchar(dat$name),
                dat$name,
                dat$location_code
            )

            summary_type <- dat$summary_type[[1]] %||% "standard"

            if (summary_type == "precipitation") {
                view <- data.frame(
                    Station = station_col,
                    `Last reading (UTC-7)` = format(
                        dat$latest_time,
                        "%Y-%m-%d %H:%M",
                        tz = "Etc/GMT+7"
                    ),
                    `7-day (mm)` = round(as.numeric(dat$precip_1w), 1),
                    `1-month (mm)` = round(as.numeric(dat$precip_1m), 1),
                    `6-month (mm)` = round(as.numeric(dat$precip_6m), 1),
                    `Data age (h)` = round(
                        as.numeric(dat$last_data_age_hours),
                        1
                    ),
                    check.names = FALSE,
                    stringsAsFactors = FALSE
                )
                return(DT::datatable(
                    view,
                    rownames = FALSE,
                    selection = "single",
                    options = dt_options
                ))
            }

            if (summary_type == "snow_survey") {
                view <- data.frame(
                    Station = station_col,
                    `Last reading (UTC-7)` = format(
                        dat$latest_time,
                        "%Y-%m-%d %H:%M",
                        tz = "Etc/GMT+7"
                    ),
                    `Latest` = round(as.numeric(dat$current_value), 1),
                    `Mar 1` = round(as.numeric(dat$march_1_value), 1),
                    `Apr 1` = round(as.numeric(dat$april_1_value), 1),
                    `May 1` = round(as.numeric(dat$may_1_value), 1),
                    `Data age (h)` = round(
                        as.numeric(dat$last_data_age_hours),
                        1
                    ),
                    check.names = FALSE,
                    stringsAsFactors = FALSE
                )
                return(DT::datatable(
                    view,
                    rownames = FALSE,
                    selection = "single",
                    options = dt_options
                ))
            }

            # Standard: water level, water flow, temperature, FDD, DDT, snow pillows
            current_value <- as.numeric(dat$current_value)
            delta_24h <- as.numeric(dat$change_24h)
            delta_48h <- as.numeric(dat$change_48h)
            delta_1wk <- as.numeric(dat$change_1w)

            if (isTRUE(use_relative)) {
                # Relative change as percent of current value.
                denom <- ifelse(
                    !is.na(current_value) & current_value != 0,
                    current_value,
                    NA_real_
                )
                delta_24h <- (delta_24h / denom) * 100
                delta_48h <- (delta_48h / denom) * 100
                delta_1wk <- (delta_1wk / denom) * 100
            }

            view <- data.frame(
                Station = station_col,
                `Last reading (UTC-7)` = format(
                    dat$latest_time,
                    "%Y-%m-%d %H:%M",
                    tz = "Etc/GMT+7"
                ),
                Value = round(current_value, 3),
                change_24h = round(delta_24h, 3),
                change_48h = round(delta_48h, 3),
                change_1wk = round(delta_1wk, 3),
                `Data age (h)` = round(as.numeric(dat$last_data_age_hours), 2),
                check.names = FALSE,
                stringsAsFactors = FALSE
            )

            if (isTRUE(use_relative)) {
                names(view)[
                    names(view) == "change_24h"
                ] <- "\u0394 24 h (\u0025)"
                names(view)[
                    names(view) == "change_48h"
                ] <- "\u0394 48 h (\u0025)"
                names(view)[
                    names(view) == "change_1wk"
                ] <- "\u0394 1 wk (\u0025)"
            } else {
                names(view)[names(view) == "change_24h"] <- "\u0394 24 h"
                names(view)[names(view) == "change_48h"] <- "\u0394 48 h"
                names(view)[names(view) == "change_1wk"] <- "\u0394 1 wk"
            }

            result_table <- DT::datatable(
                view,
                rownames = FALSE,
                selection = "single",
                options = dt_options
            )

            change_positions <- grep("\u005E\u0394", names(view))
            if (length(change_positions) > 0) {
                result_table <- DT::formatStyle(
                    result_table,
                    columns = change_positions,
                    backgroundColor = DT::styleInterval(
                        c(-0.5, 0.5),
                        c("#86efac", "#fef08a", "#fca5a5")
                    )
                )
            }

            result_table
        })

        # Table row selection -> update station selectize
        shiny::observe({
            row_idx <- input$summary_table_rows_selected
            dat <- summary_data()
            if (is.null(dat) || nrow(dat) == 0 || is.null(row_idx)) {
                return()
            }
            if (row_idx < 1L || row_idx > nrow(dat)) {
                return()
            }
            selected_code <- dat$location_code[[row_idx]]
            if (!is.null(selected_code) && nzchar(selected_code)) {
                shiny::updateSelectizeInput(
                    session,
                    inputId = "station",
                    selected = selected_code
                )
            }
        })

        # Station selectize -> highlight matching table row
        shiny::observe({
            code <- input$station
            dat <- summary_data()
            if (
                is.null(dat) || nrow(dat) == 0 || is.null(code) || !nzchar(code)
            ) {
                DT::selectRows(
                    DT::dataTableProxy("summary_table", session = session),
                    NULL
                )
                return()
            }
            row_idx <- which(dat$location_code == code)
            if (length(row_idx) == 0) {
                row_idx <- NULL
            }
            DT::selectRows(
                DT::dataTableProxy("summary_table", session = session),
                row_idx
            )
        })

        # -----------------------------------------------------------------------
        # Plot server
        # -----------------------------------------------------------------------

        time_zero <- shiny::reactive({
            if (!is.null(input$time0) && nzchar(input$time0)) {
                suppressWarnings(as.POSIXct(
                    input$time0,
                    format = "%Y-%m-%dT%H:%M",
                    tz = "America/Whitehorse"
                ))
            } else {
                Sys.time()
            }
        })

        station_label_for_code <- function(code) {
            if (is.null(code) || !nzchar(code %||% "")) {
                return("")
            }
            dat <- community_locations()
            if (is.null(dat) || nrow(dat) == 0) {
                return(code)
            }
            idx <- match(code, dat$location_code)
            if (is.na(idx)) {
                return(code)
            }
            nm <- dat$name[[idx]]
            if (!is.na(nm) && nzchar(nm)) nm else code
        }

        current_station_plot_request <- shiny::reactive({
            dat <- available_primary_stations()
            if (is.null(dat) || nrow(dat) == 0) {
                return(NULL)
            }

            primary_station_code <- input$station
            if (
                is.null(primary_station_code) ||
                    !nzchar(primary_station_code) ||
                    !primary_station_code %in% dat$location_code
            ) {
                primary_station_code <- dat$location_code[[1]]
            }

            secondary_station_code <- input$secondary_station
            secondary_parameter <- input$secondary_parameter
            all_locations <- community_locations()
            if (
                is.null(secondary_station_code) ||
                    !nzchar(secondary_station_code) ||
                    identical(secondary_station_code, primary_station_code) ||
                    is.null(all_locations) ||
                    nrow(all_locations) == 0 ||
                    !secondary_station_code %in% all_locations$location_code
            ) {
                secondary_station_code <- NULL
            }

            selected_parameter <- input$parameter
            if (is.null(selected_parameter) || !nzchar(selected_parameter)) {
                selected_parameter <- "water level"
            }

            list(
                primary_station_code = primary_station_code,
                secondary_station_code = secondary_station_code,
                secondary_parameter = secondary_parameter,
                parameter = selected_parameter,
                primary_historical_years = normalize_selected_historical_years(
                    input$primary_historical_years
                ),
                secondary_historical_years = normalize_selected_historical_years(
                    input$secondary_historical_years
                ),
                show_legend = isTRUE(input$station_plot_show_legend),
                label_traces = isTRUE(input$station_plot_label_traces),
                reference_time = time_zero(),
                historical_start_year = 2020L,
                primary_station_label = station_label_for_code(
                    primary_station_code
                ),
                secondary_station_label = station_label_for_code(
                    secondary_station_code
                )
            )
        })

        normalize_plot_request <- function(request) {
            if (is.null(request)) {
                return(NULL)
            }
            request$primary_historical_years <- normalize_selected_historical_years(
                request$primary_historical_years
            )
            request$secondary_historical_years <- normalize_selected_historical_years(
                request$secondary_historical_years
            )
            request$reference_time <- if (is.null(request$reference_time)) {
                NULL
            } else {
                format(
                    as.POSIXct(
                        request$reference_time,
                        tz = "America/Whitehorse"
                    ),
                    "%Y-%m-%d %H:%M:%S",
                    tz = "America/Whitehorse"
                )
            }
            # Exclude display-only fields from staleness comparison so
            # changing them never triggers the "create plot" gate.
            request$show_legend <- NULL
            request$label_traces <- NULL
            request$primary_historical_years <- NULL
            request$secondary_historical_years <- NULL
            request
        }

        create_plot_pending <- shiny::reactiveVal(FALSE)
        station_plot_request <- shiny::reactiveVal(NULL)

        shiny::observeEvent(
            input$create_plot,
            {
                create_plot_pending(TRUE)
            },
            ignoreInit = TRUE
        )

        shiny::observe({
            if (!isTRUE(create_plot_pending())) {
                return()
            }

            summary_data()

            request <- current_station_plot_request()

            if (
                is.null(request) ||
                    !nzchar(request$primary_station_code %||% "")
            ) {
                create_plot_pending(FALSE)
                return()
            }

            station_plot_request(request)
            create_plot_pending(FALSE)
        })

        # Auto-refresh the stored request when soft inputs change (no need
        # to hit Create Plot again for these).
        # NOTE: show_legend is intentionally excluded here, it is handled
        # via plotlyProxy (relayout) so the plot is never re-rendered just
        # because the legend is toggled.
        shiny::observe({
            # Take a reactive dependency on each soft input.
            lbl <- input$station_plot_label_traces
            prim_yrs <- input$primary_historical_years
            sec_yrs <- input$secondary_historical_years

            stored <- station_plot_request()
            if (is.null(stored)) {
                return()
            }

            updated <- stored
            updated$label_traces <- isTRUE(lbl)
            updated$primary_historical_years <-
                normalize_selected_historical_years(prim_yrs)
            updated$secondary_historical_years <-
                normalize_selected_historical_years(sec_yrs)

            station_plot_request(updated)
        })

        station_plot_is_stale <- shiny::reactive({
            current_request <- normalize_plot_request(current_station_plot_request())
            stored_request <- normalize_plot_request(station_plot_request())

            !is.null(current_request) &&
                !is.null(stored_request) &&
                !isTRUE(identical(current_request, stored_request))
        })

        output$station_plot_title <- shiny::renderText({
            request <- station_plot_request()
            if (is.null(request)) {
                return("Station Plot")
            }

            if (
                !is.null(request$secondary_station_label) &&
                    nzchar(request$secondary_station_label)
            ) {
                sprintf(
                    "%s: %s vs %s",
                    request$parameter,
                    request$primary_station_label %||%
                        request$primary_station_code,
                    request$secondary_station_label
                )
            } else {
                sprintf(
                    "%s: %s",
                    request$parameter,
                    request$primary_station_label %||%
                        request$primary_station_code
                )
            }
        })

        # Push showlegend into the live Plotly widget without re-rendering.
        shiny::observe({
            shiny::req(!is.null(station_plot_request()))
            show <- isTRUE(input$station_plot_show_legend)
            plotly::plotlyProxy("station_plot", session) %>%
                plotly::plotlyProxyInvoke(
                    "relayout",
                    list(showlegend = show)
                )
        })

        output$station_plot_stale_banner <- shiny::renderUI({
            if (!isTRUE(station_plot_is_stale())) {
                return(NULL)
            }

            shiny::tags$div(
                class = "station-plot-stale-banner",
                "Plot inputs changed \u2014 click Create plot to refresh."
            )
        })

        output$station_plot <- plotly::renderPlotly({
            request <- station_plot_request()
            if (is.null(request)) {
                return(
                    empty_plotly_widget(
                        annotations = list(list(
                            text = "Choose parameters/stations, then click Create plot.",
                            x = 0.5,
                            y = 0.5,
                            xref = "paper",
                            yref = "paper",
                            showarrow = FALSE
                        ))
                    ) %>%
                        plotly::layout(
                            xaxis = list(visible = FALSE),
                            yaxis = list(visible = FALSE)
                        )
                )
            }

            code <- request$primary_station_code
            param <- request$parameter
            sec_code <- request$secondary_station_code
            sec_param <- request$secondary_parameter

            derived_parameters <- c(
                "FDD",
                "DDT",
                "temperature, air",
                "precipitation (1wk)",
                "precipitation (24hr)"
            )

            snow_survey_params_local <- c(
                "snow water eq (survey)",
                "snow depth (survey)"
            )

            fetch_plot_series <- function(location_code, parameter_name) {
                if (parameter_name %in% derived_parameters) {
                    return(tryCatch(
                        get_station_timeseries(
                            location_code = location_code,
                            parameter = parameter_name,
                            reference_time = request$reference_time,
                            load_entire_record = FALSE,
                            con = con
                        ),
                        error = function(e) NULL
                    ))
                }

                # Discrete snow survey data: query samples/discrete.results.
                if (parameter_name %in% snow_survey_params_local) {
                    code_sql <- DBI::dbQuoteString(con, location_code)
                    param_sql <- DBI::dbQuoteString(
                        con,
                        parameter_query_name(parameter_name)
                    )
                    return(tryCatch(
                        interpret_loaded_times_as_local(
                            YGwater::dbGetQueryDT(
                                sprintf(
                                    paste(
                                        "SELECT s.target_datetime AS datetime,",
                                        "  dr.result AS value",
                                        "FROM samples s",
                                        "JOIN discrete.results dr",
                                        "  ON dr.sample_id = s.sample_id",
                                        "JOIN parameters p",
                                        "  ON p.parameter_id = dr.parameter_id",
                                        "JOIN locations l",
                                        "  ON l.location_id = s.location_id",
                                        "WHERE l.location_code = %s",
                                        "  AND p.param_name = %s",
                                        "  AND dr.result IS NOT NULL",
                                        "  AND s.target_datetime IS NOT NULL",
                                        "  AND s.target_datetime >= NOW() - INTERVAL '1 year'",
                                        "ORDER BY s.target_datetime"
                                    ),
                                    code_sql,
                                    param_sql
                                ),
                                con = con
                            ),
                            time_columns = "datetime"
                        ),
                        error = function(e) NULL
                    ))
                }

                code_sql <- DBI::dbQuoteString(con, location_code)
                param_sql <- DBI::dbQuoteString(
                    con,
                    parameter_query_name(parameter_name)
                )

                history_interval <- if (
                    identical(parameter_name, "water flow")
                ) {
                    "6 months"
                } else {
                    "30 days"
                }
                history_days <- if (identical(parameter_name, "water flow")) {
                    183L
                } else {
                    30L
                }

                d <- tryCatch(
                    YGwater::dbGetQueryDT(
                        sprintf(
                            paste(
                                "SELECT",
                                "  DATE_TRUNC('hour', mc.datetime) +",
                                "    (FLOOR(EXTRACT(MINUTE FROM mc.datetime) / 15) * 15 || ' minutes')::interval AS datetime,",
                                "  AVG(mc.value) AS value",
                                "FROM measurements_continuous mc",
                                "JOIN timeseries ts ON ts.timeseries_id = mc.timeseries_id",
                                "JOIN parameters p ON p.parameter_id = ts.parameter_id",
                                "JOIN locations l ON l.location_id = ts.location_id",
                                "WHERE l.location_code = %s",
                                "  AND p.param_name = %s",
                                "  AND mc.datetime >= NOW() - INTERVAL '%s'",
                                "  AND mc.datetime <= NOW()",
                                "  AND mc.value IS NOT NULL",
                                "GROUP BY 1",
                                "ORDER BY 1"
                            ),
                            code_sql,
                            param_sql,
                            history_interval
                        ),
                        con = con
                    ),
                    error = function(e) NULL
                )

                if (is.null(d) || nrow(d) == 0) {
                    d <- tryCatch(
                        YGwater::dbGetQueryDT(
                            sprintf(
                                paste(
                                    "SELECT mcd.date::timestamp AS datetime, mcd.value",
                                    "FROM measurements_calculated_daily mcd",
                                    "JOIN timeseries ts ON ts.timeseries_id = mcd.timeseries_id",
                                    "JOIN parameters p ON p.parameter_id = ts.parameter_id",
                                    "JOIN locations l ON l.location_id = ts.location_id",
                                    "WHERE l.location_code = %s",
                                    "  AND p.param_name = %s",
                                    "  AND mcd.date >= CURRENT_DATE - %s",
                                    "  AND mcd.value IS NOT NULL",
                                    "ORDER BY 1"
                                ),
                                code_sql,
                                param_sql,
                                history_days
                            ),
                            con = con
                        ),
                        error = function(e) NULL
                    )
                }

                if (!is.null(d) && nrow(d) > 0) {
                    d <- interpret_loaded_times_as_local(
                        d,
                        time_columns = "datetime"
                    )
                }
                d
            }

            dat <- fetch_plot_series(code, param)

            if (is.null(dat) || nrow(dat) == 0) {
                return(
                    empty_plotly_widget(
                        annotations = list(list(
                            text = "No data available for the selected station and parameter.",
                            x = 0.5,
                            y = 0.5,
                            xref = "paper",
                            yref = "paper",
                            showarrow = FALSE
                        ))
                    ) %>%
                        plotly::layout(
                            xaxis = list(visible = FALSE),
                            yaxis = list(visible = FALSE)
                        )
                )
            }

            y_title <- parameter_axis_title(param)

            is_survey_param <- param %in%
                c(
                    "snow water eq (survey)",
                    "snow depth (survey)"
                )

            include_percentiles <- param %in%
                c(
                    "water flow",
                    "water level",
                    "precipitation (1wk)",
                    "precipitation (24hr)",
                    "temperature, air",
                    "FDD",
                    "DDT",
                    "snow water equivalent",
                    "snow depth",
                    "snow water eq (pillow)",
                    "snow depth (pillow)",
                    "snow water eq (survey)",
                    "snow depth (survey)"
                )

            p <- plotly::plot_ly(type = "scatter", mode = "lines")

            if (isTRUE(include_percentiles)) {
                pct <- tryCatch(
                    get_daily_percentiles(
                        location_codes = c(code),
                        parameter = param,
                        con = con,
                        historical_start_year = request$historical_start_year %||%
                            2020L
                    ),
                    error = function(e) data.frame()
                )

                if (!is.null(pct) && nrow(pct) > 0) {
                    pct <- pct[pct$location_code == code, , drop = FALSE]
                }

                if (!is.null(pct) && nrow(pct) > 0) {
                    pct <- as.data.frame(pct, stringsAsFactors = FALSE)
                    dat <- as.data.frame(dat, stringsAsFactors = FALSE)

                    plot_timezone <- attr(dat$datetime, "tzone")
                    if (
                        length(plot_timezone) == 0 ||
                            is.na(plot_timezone[[1]]) ||
                            !nzchar(plot_timezone[[1]])
                    ) {
                        plot_timezone <- "America/Whitehorse"
                    } else {
                        plot_timezone <- plot_timezone[[1]]
                    }

                    reference_ts <- suppressWarnings(as.POSIXct(
                        request$reference_time,
                        tz = plot_timezone
                    ))
                    if (is.na(reference_ts)) {
                        reference_ts <- as.POSIXct(
                            Sys.time(),
                            tz = plot_timezone
                        )
                    }

                    historical_start_year <- normalize_historical_start_year(
                        request$historical_start_year %||% 2020L
                    )
                    ref_year <- suppressWarnings(as.integer(format(
                        reference_ts,
                        "%Y",
                        tz = plot_timezone
                    )))
                    if (is.na(ref_year)) {
                        ref_year <- as.integer(format(
                            Sys.time(),
                            "%Y",
                            tz = plot_timezone
                        ))
                    }

                    pct_bands <- c(
                        "p0",
                        "p10",
                        "p25",
                        "p50",
                        "p75",
                        "p90",
                        "p100"
                    )

                    pct_lookup <- pct[, c(
                        "location_code",
                        "doy",
                        "n_values",
                        pct_bands
                    )]
                    pct_lookup <- as.data.frame(
                        pct_lookup,
                        stringsAsFactors = FALSE
                    )
                    pct_lookup <- pct_lookup[
                        !is.na(pct_lookup$doy) &
                            !is.na(pct_lookup$n_values) &
                            pct_lookup$n_values > 0,
                        ,
                        drop = FALSE
                    ]

                    pct_plot <- data.frame()

                    if (is_survey_param) {
                        survey_anchor_dates <- as.POSIXct(
                            sprintf(
                                "%04d-%02d-01",
                                ref_year,
                                c(3L, 4L, 5L)
                            ),
                            tz = plot_timezone
                        )

                        survey_points <- dat[
                            !is.na(dat$datetime),
                            c("datetime"),
                            drop = FALSE
                        ]
                        survey_points <- rbind(
                            survey_points,
                            data.frame(
                                datetime = survey_anchor_dates,
                                stringsAsFactors = FALSE
                            )
                        )
                        survey_points <- as.data.frame(
                            survey_points,
                            stringsAsFactors = FALSE
                        )
                        survey_points <- survey_points[
                            order(survey_points$datetime),
                            ,
                            drop = FALSE
                        ]
                        survey_points <- survey_points[
                            !duplicated(survey_points$datetime),
                            ,
                            drop = FALSE
                        ]

                        if (nrow(survey_points) > 0 && nrow(pct_lookup) > 0) {
                            sp_years <- as.integer(format(
                                survey_points$datetime,
                                "%Y",
                                tz = plot_timezone
                            ))
                            sp_months <- as.integer(format(
                                survey_points$datetime,
                                "%m",
                                tz = plot_timezone
                            ))
                            sp_days <- as.integer(format(
                                survey_points$datetime,
                                "%d",
                                tz = plot_timezone
                            ))
                            sp_doys <- as.integer(format(
                                survey_points$datetime,
                                "%j",
                                tz = plot_timezone
                            ))
                            sp_is_leap <- (sp_years %% 4 == 0 &
                                sp_years %% 100 != 0) |
                                (sp_years %% 400 == 0)

                            survey_points$doy <- ifelse(
                                sp_months > 2 & sp_is_leap,
                                sp_doys - 1L,
                                sp_doys
                            )
                            survey_points$doy[
                                sp_months == 2 & sp_days == 29
                            ] <- NA_integer_

                            pct_key <- paste(
                                pct_lookup$location_code,
                                pct_lookup$doy
                            )
                            sample_key <- paste(
                                rep(code, nrow(survey_points)),
                                survey_points$doy
                            )
                            match_idx <- match(sample_key, pct_key)

                            sample_pct <- data.frame(
                                location_code = rep(code, nrow(survey_points)),
                                datetime = survey_points$datetime,
                                doy = survey_points$doy,
                                stringsAsFactors = FALSE
                            )
                            for (col in pct_bands) {
                                sample_pct[[col]] <- pct_lookup[[col]][
                                    match_idx
                                ]
                            }
                            sample_pct <- sample_pct[
                                stats::complete.cases(sample_pct[, c(
                                    "datetime",
                                    pct_bands
                                )]),
                                ,
                                drop = FALSE
                            ]

                            if (nrow(sample_pct) > 0) {
                                pct_plot <- sample_pct
                            }
                        }
                    } else {
                        pct_dates <- seq(
                            as.Date(sprintf("%d-01-01", historical_start_year)),
                            as.Date(sprintf("%d-12-31", ref_year + 1L)),
                            by = "day"
                        )

                        pct_years <- as.integer(format(pct_dates, "%Y"))
                        pct_months <- as.integer(format(pct_dates, "%m"))
                        pct_days <- as.integer(format(pct_dates, "%d"))
                        pct_doys <- as.integer(format(pct_dates, "%j"))
                        pct_is_leap <- (pct_years %% 4 == 0 &
                            pct_years %% 100 != 0) |
                            (pct_years %% 400 == 0)

                        pct_plot <- data.frame(
                            location_code = code,
                            datetime = as.POSIXct(
                                pct_dates,
                                tz = plot_timezone
                            ),
                            doy = ifelse(
                                pct_months > 2 & pct_is_leap,
                                pct_doys - 1L,
                                pct_doys
                            ),
                            stringsAsFactors = FALSE
                        )
                        pct_plot$is_feb29 <-
                            pct_months == 2 & pct_days == 29

                        pct_key <- paste(
                            pct_lookup$location_code,
                            pct_lookup$doy
                        )
                        plot_key <- paste(pct_plot$location_code, pct_plot$doy)
                        match_idx <- match(plot_key, pct_key)
                        for (col in pct_bands) {
                            pct_plot[[col]] <- pct_lookup[[col]][match_idx]
                        }

                        # Percentile queries exclude Feb 29 from DB; for leap
                        # years, synthesize Feb 29 from Feb 28 and Mar 1 bands.
                        feb29_rows <- which(pct_plot$is_feb29)
                        if (length(feb29_rows) > 0) {
                            feb28_key <- paste(code, 59L)
                            mar01_key <- paste(code, 60L)
                            feb28_idx <- match(feb28_key, pct_key)
                            mar01_idx <- match(mar01_key, pct_key)

                            for (col in pct_bands) {
                                v0 <- pct_lookup[[col]][feb28_idx]
                                v1 <- pct_lookup[[col]][mar01_idx]
                                if (is.finite(v0) && is.finite(v1)) {
                                    pct_plot[feb29_rows, col] <- (v0 + v1) / 2
                                }
                            }
                        }

                        pct_plot$is_feb29 <- NULL
                    }

                    pct_plot <- pct_plot[
                        stats::complete.cases(pct_plot[, c(
                            "datetime",
                            pct_bands
                        )]),
                        ,
                        drop = FALSE
                    ]
                    pct_plot <- pct_plot[
                        order(pct_plot$datetime),
                        ,
                        drop = FALSE
                    ]
                    if (nrow(pct_plot) > 0) {
                        if (is_survey_param) {
                            band_specs <- list(
                                list(
                                    ymin = "p0",
                                    ymax = "p10",
                                    name = "P0-P10",
                                    rank = 80,
                                    color = "rgba(127, 29, 29, 0.3)"
                                ),
                                list(
                                    ymin = "p10",
                                    ymax = "p25",
                                    name = "P10-P25",
                                    rank = 70,
                                    color = "rgba(180, 83, 9, 0.3)"
                                ),
                                list(
                                    ymin = "p25",
                                    ymax = "p50",
                                    name = "P25-P50",
                                    rank = 60,
                                    color = "rgba(245, 158, 11, 0.3)"
                                ),
                                list(
                                    ymin = "p50",
                                    ymax = "p75",
                                    name = "P50-P75",
                                    rank = 50,
                                    color = "rgba(163, 230, 53, 0.3)"
                                ),
                                list(
                                    ymin = "p75",
                                    ymax = "p90",
                                    name = "P75-P90",
                                    rank = 40,
                                    color = "rgba(34, 197, 94, 0.3)"
                                ),
                                list(
                                    ymin = "p90",
                                    ymax = "p100",
                                    name = "P90-P100",
                                    rank = 30,
                                    color = "rgba(15, 118, 110, 0.3)"
                                )
                            )

                            for (.i in seq_len(nrow(pct_plot))) {
                                box_start <- pct_plot$datetime[[.i]]
                                box_end <- box_start +
                                    as.difftime(28, units = "days")
                                for (.band in band_specs) {
                                    y0 <- pct_plot[[.band$ymin]][[.i]]
                                    y1 <- pct_plot[[.band$ymax]][[.i]]
                                    if (!is.finite(y0) || !is.finite(y1)) {
                                        next
                                    }
                                    ribbon_box <- data.frame(
                                        datetime = c(box_start, box_end),
                                        ymin = c(y0, y0),
                                        ymax = c(y1, y1),
                                        stringsAsFactors = FALSE
                                    )
                                    p <- p %>%
                                        plotly::add_ribbons(
                                            data = ribbon_box,
                                            x = ~datetime,
                                            ymin = ~ymin,
                                            ymax = ~ymax,
                                            yaxis = "y",
                                            name = .band$name,
                                            legendrank = .band$rank,
                                            hovertemplate = paste0(
                                                .band$name,
                                                "<extra></extra>"
                                            ),
                                            fillcolor = .band$color,
                                            line = list(color = "transparent"),
                                            connectgaps = FALSE,
                                            showlegend = FALSE,
                                            inherit = FALSE
                                        )
                                }
                            }
                        } else {
                            p <- p %>%
                                plotly::add_ribbons(
                                    data = pct_plot,
                                    x = ~datetime,
                                    ymin = ~p0,
                                    ymax = ~p10,
                                    yaxis = "y",
                                    name = "P0-P10",
                                    legendrank = 80,
                                    hovertemplate = "P0-P10<extra></extra>",
                                    fillcolor = "rgba(127, 29, 29, 0.3)",
                                    line = list(color = "transparent"),
                                    connectgaps = FALSE,
                                    inherit = FALSE
                                ) %>%
                                plotly::add_ribbons(
                                    data = pct_plot,
                                    x = ~datetime,
                                    ymin = ~p10,
                                    ymax = ~p25,
                                    yaxis = "y",
                                    name = "P10-P25",
                                    legendrank = 70,
                                    hovertemplate = "P10-P25<extra></extra>",
                                    fillcolor = "rgba(180, 83, 9, 0.3)",
                                    line = list(color = "transparent"),
                                    connectgaps = FALSE,
                                    inherit = FALSE
                                ) %>%
                                plotly::add_ribbons(
                                    data = pct_plot,
                                    x = ~datetime,
                                    ymin = ~p25,
                                    ymax = ~p50,
                                    yaxis = "y",
                                    name = "P25-P50",
                                    legendrank = 60,
                                    hovertemplate = "P25-P50<extra></extra>",
                                    fillcolor = "rgba(245, 158, 11, 0.3)",
                                    line = list(color = "transparent"),
                                    connectgaps = FALSE,
                                    inherit = FALSE
                                ) %>%
                                plotly::add_ribbons(
                                    data = pct_plot,
                                    x = ~datetime,
                                    ymin = ~p50,
                                    ymax = ~p75,
                                    yaxis = "y",
                                    name = "P50-P75",
                                    legendrank = 50,
                                    hovertemplate = "P50-P75<extra></extra>",
                                    fillcolor = "rgba(163, 230, 53, 0.3)",
                                    line = list(color = "transparent"),
                                    connectgaps = FALSE,
                                    inherit = FALSE
                                ) %>%
                                plotly::add_ribbons(
                                    data = pct_plot,
                                    x = ~datetime,
                                    ymin = ~p75,
                                    ymax = ~p90,
                                    yaxis = "y",
                                    name = "P75-P90",
                                    legendrank = 40,
                                    hovertemplate = "P75-P90<extra></extra>",
                                    fillcolor = "rgba(34, 197, 94, 0.3)",
                                    line = list(color = "transparent"),
                                    connectgaps = FALSE,
                                    inherit = FALSE
                                ) %>%
                                plotly::add_ribbons(
                                    data = pct_plot,
                                    x = ~datetime,
                                    ymin = ~p90,
                                    ymax = ~p100,
                                    yaxis = "y",
                                    name = "P90-P100",
                                    legendrank = 30,
                                    hovertemplate = "P90-P100<extra></extra>",
                                    fillcolor = "rgba(15, 118, 110, 0.3)",
                                    line = list(color = "transparent"),
                                    connectgaps = FALSE,
                                    inherit = FALSE
                                )
                        }
                    }
                }
            }

            p <- if (is_survey_param) {
                dat$datetime_end <- dat$datetime +
                    as.difftime(28, units = "days")
                p %>%
                    plotly::add_segments(
                        data = dat,
                        x = ~datetime,
                        xend = ~datetime_end,
                        y = ~value,
                        yend = ~value,
                        yaxis = "y",
                        name = param,
                        line = list(color = "#2563eb", width = 3),
                        hovertemplate = paste0(
                            param,
                            " (",
                            code,
                            "): %{y:.3f}<extra></extra>"
                        ),
                        inherit = FALSE
                    )
            } else {
                p %>%
                    plotly::add_lines(
                        data = dat,
                        x = ~datetime,
                        y = ~value,
                        yaxis = "y",
                        name = param,
                        line = list(color = "#2563eb", width = 2),
                        hovertemplate = paste0(
                            param,
                            " (",
                            code,
                            "): %{y:.3f}<extra></extra>"
                        ),
                        inherit = FALSE
                    )
            }

            has_secondary_trace <- FALSE
            sec_dat <- NULL

            if (
                !is.null(sec_code) &&
                    nzchar(sec_code) &&
                    !identical(sec_code, code) &&
                    !is.null(sec_param) &&
                    nzchar(sec_param)
            ) {
                sec_dat <- fetch_plot_series(sec_code, sec_param)
                if (!is.null(sec_dat) && nrow(sec_dat) > 0) {
                    has_secondary_trace <- TRUE
                    sec_is_survey_param <- sec_param %in%
                        c(
                            "snow water eq (survey)",
                            "snow depth (survey)"
                        )
                    p <- if (sec_is_survey_param) {
                        sec_dat$datetime_end <- sec_dat$datetime +
                            as.difftime(28, units = "days")
                        p %>%
                            plotly::add_segments(
                                data = sec_dat,
                                x = ~datetime,
                                xend = ~datetime_end,
                                y = ~value,
                                yend = ~value,
                                yaxis = "y2",
                                name = sec_param,
                                line = list(color = "#f97316", width = 3),
                                hovertemplate = paste0(
                                    sec_param,
                                    " (",
                                    sec_code,
                                    "): %{y:.3f}<extra></extra>"
                                ),
                                inherit = FALSE
                            )
                    } else {
                        p %>%
                            plotly::add_lines(
                                data = sec_dat,
                                x = ~datetime,
                                y = ~value,
                                yaxis = "y2",
                                name = sec_param,
                                line = list(color = "#f97316", width = 2),
                                hovertemplate = paste0(
                                    sec_param,
                                    " (",
                                    sec_code,
                                    "): %{y:.3f}<extra></extra>"
                                ),
                                inherit = FALSE
                            )
                    }
                }
            }

            # ------------------------------------------------------------------
            # Historical overlay traces (primary station, selected years)
            # ------------------------------------------------------------------
            primary_overlays <- tryCatch(
                get_historical_overlay_timeseries(
                    location_code = code,
                    parameter = param,
                    years = request$primary_historical_years,
                    con = con,
                    reference_time = request$reference_time
                ),
                error = function(e) list()
            )

            show_legend <- FALSE
            label_traces <- isTRUE(request$label_traces)
            trace_label_annotations <- list()

            # Compute view bounds now so label positions can be clamped.
            # Must use "Etc/GMT+7" to match the tzone stamped by
            # get_historical_overlay_timeseries; mixing with "America/Whitehorse"
            # (same offset, different string) triggers the tzone warning.
            plot_tz_label <- "Etc/GMT+7"
            plot_ref_ts <- suppressWarnings(as.POSIXct(
                request$reference_time,
                tz = plot_tz_label
            ))
            if (is.null(plot_ref_ts) || is.na(plot_ref_ts)) {
                plot_ref_ts <- as.POSIXct(
                    format(Sys.time(), tz = plot_tz_label),
                    tz = plot_tz_label
                )
            }
            attr(plot_ref_ts, "tzone") <- plot_tz_label
            view_x_min <- structure(
                plot_ref_ts - as.difftime(14, units = "days"),
                tzone = plot_tz_label
            )
            view_x_max <- structure(
                plot_ref_ts + as.difftime(28, units = "days"),
                tzone = plot_tz_label
            )

            # Dark, visually distinct colours readable against white/light bg.
            dark_overlay_palette <- c(
                "#1b4f9e", # dark blue
                "#a33000", # dark red-orange
                "#1a7a1a", # dark green
                "#7b2fa8", # dark purple
                "#8a6800", # dark gold
                "#0a7a7a", # dark teal
                "#8b1a4a", # dark rose
                "#2c5f2e", # forest green
                "#4b3000", # dark brown
                "#003d6b" # navy
            )

            if (length(primary_overlays) > 0) {
                overlay_years <- names(primary_overlays)
                n_ov <- length(overlay_years)
                overlay_colors <- dark_overlay_palette[
                    ((seq_len(n_ov) - 1L) %% length(dark_overlay_palette)) + 1L
                ]
                for (index in seq_along(overlay_years)) {
                    overlay_series <- primary_overlays[[index]]
                    if (is.null(overlay_series) || nrow(overlay_series) == 0) {
                        next
                    }
                    overlay_year <- overlay_years[[index]]
                    if (is_survey_param) {
                        overlay_series$datetime_end <-
                            overlay_series$datetime +
                            as.difftime(28, units = "days")
                    }
                    p <- if (is_survey_param) {
                        p %>%
                            plotly::add_segments(
                                data = overlay_series,
                                x = ~datetime,
                                xend = ~datetime_end,
                                y = ~value,
                                yend = ~value,
                                yaxis = "y",
                                name = sprintf("%s (%s)", overlay_year, param),
                                legendrank = 15L + index,
                                hovertemplate = paste0(
                                    overlay_year,
                                    ": %{y:.3f}<extra></extra>"
                                ),
                                line = list(
                                    color = overlay_colors[[index]],
                                    width = 2,
                                    dash = "dot"
                                ),
                                inherit = FALSE
                            )
                    } else {
                        p %>%
                            plotly::add_lines(
                                data = overlay_series,
                                x = overlay_series$datetime,
                                y = overlay_series$value,
                                yaxis = "y",
                                name = sprintf("%s (%s)", overlay_year, param),
                                legendrank = 15L + index,
                                hovertemplate = paste0(
                                    overlay_year,
                                    ": %{y:.3f}<extra></extra>"
                                ),
                                line = list(
                                    color = overlay_colors[[index]],
                                    width = 1.5,
                                    dash = "dot"
                                ),
                                inherit = FALSE
                            )
                    }
                    if (label_traces) {
                        # Use the last point that falls within the visible
                        # window; fall back to the last point in the series.
                        in_view <- overlay_series$datetime >= view_x_min &
                            overlay_series$datetime <= view_x_max
                        cand_series <- if (any(in_view)) {
                            overlay_series[in_view, , drop = FALSE]
                        } else {
                            overlay_series
                        }
                        latest_idx <- which.max(cand_series$datetime)
                        label_x <- cand_series$datetime[[latest_idx]]
                        label_y <- cand_series$value[[latest_idx]]
                        # Clamp x to just inside the right edge of the window.
                        label_x <- min(label_x, view_x_max)
                        if (
                            length(label_x) == 1 &&
                                is.finite(label_x) &&
                                is.finite(label_y)
                        ) {
                            trace_label_annotations <- c(
                                trace_label_annotations,
                                list(list(
                                    x = format(
                                        label_x,
                                        "%Y-%m-%d %H:%M:%S"
                                    ),
                                    y = label_y,
                                    xref = "x",
                                    yref = "y",
                                    text = as.character(overlay_year),
                                    showarrow = FALSE,
                                    xanchor = "right",
                                    yanchor = "middle",
                                    font = list(
                                        size = 11,
                                        color = overlay_colors[[index]]
                                    ),
                                    bgcolor = "rgba(255,255,255,0.7)",
                                    bordercolor = overlay_colors[[index]],
                                    borderwidth = 1,
                                    borderpad = 3
                                ))
                            )
                        }
                    }
                }
            }

            if (length(trace_label_annotations) > 1) {
                ann_y <- vapply(
                    trace_label_annotations,
                    function(x) suppressWarnings(as.numeric(x$y)),
                    numeric(1)
                )
                valid_idx <- which(is.finite(ann_y))

                if (length(valid_idx) > 1) {
                    ann_y_valid <- ann_y[valid_idx]
                    y_min_bound <- suppressWarnings(min(
                        c(
                            dat$value,
                            ann_y_valid
                        ),
                        na.rm = TRUE
                    ))
                    y_max_bound <- suppressWarnings(max(
                        c(
                            dat$value,
                            ann_y_valid
                        ),
                        na.rm = TRUE
                    ))

                    if (
                        !is.finite(y_min_bound) ||
                            !is.finite(y_max_bound) ||
                            y_max_bound <= y_min_bound
                    ) {
                        y_min_bound <- min(ann_y_valid, na.rm = TRUE)
                        y_max_bound <- max(ann_y_valid, na.rm = TRUE)
                    }

                    y_span <- y_max_bound - y_min_bound
                    min_sep <- if (is.finite(y_span) && y_span > 0) {
                        0.02 * y_span
                    } else {
                        0.1
                    }

                    ord <- order(ann_y_valid)
                    ann_y_adj <- ann_y_valid

                    for (i in 2:length(ord)) {
                        cur <- ord[[i]]
                        prev <- ord[[i - 1]]
                        if ((ann_y_adj[[cur]] - ann_y_adj[[prev]]) < min_sep) {
                            ann_y_adj[[cur]] <- ann_y_adj[[prev]] + min_sep
                        }
                    }

                    overflow <- max(ann_y_adj, na.rm = TRUE) - y_max_bound
                    if (is.finite(overflow) && overflow > 0) {
                        ann_y_adj <- ann_y_adj - overflow
                    }

                    underflow <- y_min_bound - min(ann_y_adj, na.rm = TRUE)
                    if (is.finite(underflow) && underflow > 0) {
                        ann_y_adj <- ann_y_adj + underflow
                    }

                    for (i in 2:length(ord)) {
                        cur <- ord[[i]]
                        prev <- ord[[i - 1]]
                        if ((ann_y_adj[[cur]] - ann_y_adj[[prev]]) < min_sep) {
                            ann_y_adj[[cur]] <- ann_y_adj[[prev]] + min_sep
                        }
                    }

                    for (i in seq_along(valid_idx)) {
                        trace_label_annotations[[valid_idx[[i]]]]$y <-
                            ann_y_adj[[i]]
                    }
                }
            }

            # ------------------------------------------------------------------
            # Historical overlay traces (secondary station, selected years)
            # ------------------------------------------------------------------
            secondary_overlays <- if (
                !is.null(sec_code) &&
                    nzchar(sec_code) &&
                    !identical(sec_code, code) &&
                    !is.null(sec_param) &&
                    nzchar(sec_param)
            ) {
                tryCatch(
                    get_historical_overlay_timeseries(
                        location_code = sec_code,
                        parameter = sec_param,
                        years = request$secondary_historical_years,
                        con = con,
                        reference_time = request$reference_time
                    ),
                    error = function(e) list()
                )
            } else {
                list()
            }

            if (length(secondary_overlays) > 0) {
                sec_overlay_years <- names(secondary_overlays)
                n_sec_ov <- length(sec_overlay_years)
                sec_overlay_colors <- dark_overlay_palette[
                    ((seq_len(n_sec_ov) - 1L) %% length(dark_overlay_palette)) +
                        1L
                ]
                sec_is_survey_param_ov <- !is.null(sec_param) &&
                    sec_param %in%
                        c(
                            "snow water eq (survey)",
                            "snow depth (survey)"
                        )
                for (index in seq_along(sec_overlay_years)) {
                    sec_ov_series <- secondary_overlays[[index]]
                    if (is.null(sec_ov_series) || nrow(sec_ov_series) == 0) {
                        next
                    }
                    sec_ov_year <- sec_overlay_years[[index]]
                    if (sec_is_survey_param_ov) {
                        sec_ov_series$datetime_end <-
                            sec_ov_series$datetime +
                            as.difftime(28, units = "days")
                    }
                    p <- if (sec_is_survey_param_ov) {
                        p %>%
                            plotly::add_segments(
                                data = sec_ov_series,
                                x = ~datetime,
                                xend = ~datetime_end,
                                y = ~value,
                                yend = ~value,
                                yaxis = "y2",
                                name = sprintf(
                                    "%s (%s)",
                                    sec_ov_year,
                                    sec_param
                                ),
                                legendrank = 100L + index,
                                hovertemplate = paste0(
                                    sec_ov_year,
                                    ": %{y:.3f}<extra></extra>"
                                ),
                                line = list(
                                    color = sec_overlay_colors[[index]],
                                    width = 2,
                                    dash = "dot"
                                ),
                                inherit = FALSE
                            )
                    } else {
                        p %>%
                            plotly::add_lines(
                                data = sec_ov_series,
                                x = sec_ov_series$datetime,
                                y = sec_ov_series$value,
                                yaxis = "y2",
                                name = sprintf(
                                    "%s (%s)",
                                    sec_ov_year,
                                    sec_param
                                ),
                                legendrank = 100L + index,
                                hovertemplate = paste0(
                                    sec_ov_year,
                                    ": %{y:.3f}<extra></extra>"
                                ),
                                line = list(
                                    color = sec_overlay_colors[[index]],
                                    width = 1.5,
                                    dash = "dot"
                                ),
                                inherit = FALSE
                            )
                    }
                }
            }

            # ------------------------------------------------------------------
            # X-axis: start at earliest real-time data, end at now + 4 weeks.
            # Percentile ribbons span many years but are clipped to this window.
            # ------------------------------------------------------------------
            ref_ts <- plot_ref_ts

            x_min <- min(dat$datetime, na.rm = TRUE)
            if (
                !is.null(sec_dat) &&
                    nrow(sec_dat) > 0 &&
                    "datetime" %in% names(sec_dat)
            ) {
                x_min <- min(x_min, min(sec_dat$datetime, na.rm = TRUE))
            }
            x_range <- if (is.finite(x_min)) {
                if (is_survey_param) {
                    survey_start <- as.POSIXct(
                        sprintf(
                            "%04d-02-15 00:00:00",
                            as.integer(format(ref_ts, "%Y", tz = plot_tz_label))
                        ),
                        tz = plot_tz_label
                    )

                    list(
                        format(
                            survey_start,
                            "%Y-%m-%d %H:%M:%S"
                        ),
                        format(
                            ref_ts + as.difftime(28, units = "days"),
                            "%Y-%m-%d %H:%M:%S"
                        )
                    )
                } else {
                    list(
                        format(
                            ref_ts - as.difftime(14, units = "days"),
                            "%Y-%m-%d %H:%M:%S"
                        ),
                        format(
                            ref_ts + as.difftime(28, units = "days"),
                            "%Y-%m-%d %H:%M:%S"
                        )
                    )
                }
            } else {
                NULL
            }

            t0_line <- list(
                type = "line",
                xref = "x",
                yref = "paper",
                x0 = format(ref_ts, "%Y-%m-%d %H:%M:%S"),
                x1 = format(ref_ts, "%Y-%m-%d %H:%M:%S"),
                y0 = 0,
                y1 = 1,
                line = list(
                    color = "#b91c1c",
                    width = 1.5,
                    dash = "dash"
                )
            )

            p %>%
                plotly::layout(
                    xaxis = list(
                        title = "",
                        range = x_range
                    ),
                    yaxis = list(title = y_title),
                    yaxis2 = if (has_secondary_trace) {
                        list(
                            title = parameter_axis_title(sec_param),
                            overlaying = "y",
                            side = "right",
                            showgrid = FALSE
                        )
                    } else {
                        NULL
                    },
                    showlegend = show_legend,
                    shapes = list(t0_line),
                    annotations = if (length(trace_label_annotations) > 0) {
                        trace_label_annotations
                    } else {
                        NULL
                    },
                    margin = list(l = 60, r = 60, t = 60, b = 40)
                )
        })
    })
}
