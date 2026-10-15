#' YEC Dashboard UI Module
#'
#' @param id Character module id.
#' @return A Shiny UI definition.
#' @keywords internal
#' @noRd
yecDashboardUIMod <- function(id) {
    ns <- shiny::NS(id)
    panels_id <- ns("station_panels")
    map_id <- ns("stations_map")
    divider_click <- sprintf(
        paste0(
            "var panels=document.getElementById('%s');",
            "var collapsed=panels.classList.toggle('map-collapsed');",
            "this.textContent=collapsed ? '▶' : '◀';",
            "this.setAttribute('aria-label',collapsed ? 'Show map' : 'Hide map');",
            "if(!collapsed){setTimeout(function(){",
            "var widget=window.HTMLWidgets && HTMLWidgets.find('#%s');",
            "if(widget && widget.getMap){widget.getMap().invalidateSize();}",
            "},250);}"
        ),
        panels_id,
        map_id
    )
    bslib::page_fillable(
        shiny::tags$style(shiny::HTML(
            ".yec-station-panels { display:flex; gap:.5rem; min-height:0; }\n",
            ".yec-map-pane { flex:0 0 calc(50% - .75rem); min-width:0; }\n",
            ".yec-plot-pane { flex:1 1 0; min-width:0; }\n",
            ".yec-map-divider { flex:0 0 1.25rem; padding:0; border:0; ",
            "background:linear-gradient(90deg,transparent 45%,#adb5bd 45%,#adb5bd 55%,transparent 55%); ",
            "cursor:pointer; color:#495057; font-size:.75rem; }\n",
            ".yec-map-divider:hover { background-color:#e9ecef; }\n",
            ".yec-station-panels.map-collapsed .yec-map-pane { display:none; }\n"
        )),
        bslib::card(
            bslib::card_header("Latest readings"),
            bslib::layout_columns(
                shiny::selectizeInput(
                    ns("parameter_view"),
                    "Parameter",
                    choices = c(
                        "Level" = "Water level",
                        "Discharge" = "Discharge"
                    ),
                    selected = "Water level",
                    multiple = FALSE
                ),
                shiny::selectizeInput(
                    ns("record_length"),
                    "Record length",
                    choices = c(
                        "24-hour" = "24-hour",
                        "1-week" = "1-week",
                        "1-month" = "1-month",
                        "6-months" = "6-months",
                        "1-year" = "1-year",
                        "Entire record" = "entire-record"
                    ),
                    selected = "1-week",
                    multiple = FALSE
                ),
                col_widths = c(6, 6)
            ),
            shiny::tags$p(
                "Select a station in the table or click its map marker to plot."
            ),
            shiny::conditionalPanel(
                "input.parameter_view === 'Water level'",
                DT::DTOutput(ns("level_table")),
                ns = ns
            ),
            shiny::conditionalPanel(
                "input.parameter_view === 'Discharge'",
                DT::DTOutput(ns("flow_table")),
                ns = ns
            ),
            shiny::tags$small(
                "Changes use the last available reading at or before each interval",
                "prior to the latest reading, in the station's displayed units."
            ),
            shiny::tags$small(
                "Threshold margins use limits valid at the latest reading.",
                "Negative margins indicate the reading is beyond the limit."
            ),
            fill = FALSE
        ),
        shiny::tags$div(
            id = panels_id,
            class = "yec-station-panels",
            shiny::tags$div(
                class = "yec-map-pane",
                bslib::card(
                    bslib::card_header("Stations"),
                    leaflet::leafletOutput(ns("stations_map"), height = "440px")
                )
            ),
            shiny::tags$button(
                type = "button",
                class = "yec-map-divider",
                `aria-label` = "Hide map",
                title = "Hide or show map",
                onclick = divider_click,
                "◀"
            ),
            shiny::tags$div(
                class = "yec-plot-pane",
                bslib::card(
                    bslib::card_header("Hydrograph"),
                    shiny::textOutput(ns("record_resolution")),
                    plotly::plotlyOutput(ns("station_plot"), height = "440px")
                )
            )
        )
    )
}

#' Load fixed YEC stations and the selected history of water level and discharge
#'
#' @param con An AquaCache database connection.
#' @param query Database query function.
#' @param record_length One of the supported history windows.
#' @return Station metadata, latest readings and history.
#' @keywords internal
#' @noRd
yec_dashboard_data <- function(
    con,
    query = DBI::dbGetQuery,
    record_length = "1-week"
) {
    record_length <- match.arg(
        record_length,
        c(
            "24-hour",
            "1-week",
            "1-month",
            "6-months",
            "1-year",
            "entire-record"
        )
    )
    interval <- c(
        "24-hour" = "24 hours",
        "1-week" = "7 days",
        "1-month" = "1 month",
        "6-months" = "6 months",
        "1-year" = "1 year",
        "entire-record" = NA_character_
    )[[record_length]]
    # Only whitelisted intervals enter SQL; calendar months/years stay calendar based.
    start_sql <- if (identical(record_length, "entire-record")) {
        "TIMESTAMP '1900-01-01 00:00:00'"
    } else {
        paste0("clock.end_time - INTERVAL '", interval, "'")
    }
    daily <- record_length %in% c("6-months", "1-year", "entire-record")
    if (daily) {
        # Calculate local daily averages from the selected series. Some stations
        # do not have a matching stored calculated-daily series for every period.
        history_columns <- "history.datetime, history.value,"
        history_join <- paste(
            "LEFT JOIN LATERAL (",
            "  SELECT (date_trunc('day', mc.datetime AT TIME ZONE 'Etc/GMT+7')",
            "          AT TIME ZONE 'Etc/GMT+7') AS datetime,",
            "         AVG(mc.value) AS value",
            "  FROM continuous.measurements_continuous mc",
            "  WHERE mc.timeseries_id = latest.timeseries_id",
            "    AND mc.value IS NOT NULL",
            "    AND mc.datetime >=",
            start_sql,
            "    AND mc.datetime <= clock.end_time",
            "  GROUP BY 1",
            ") history ON TRUE"
        )
    } else {
        history_columns <- "history.datetime, history.value,"
        history_join <- paste(
            "LEFT JOIN continuous.measurements_continuous history",
            "  ON history.timeseries_id = latest.timeseries_id",
            " AND history.datetime >=",
            start_sql,
            " AND history.datetime <= clock.end_time"
        )
    }
    # Select one series per station and parameter so table and plot agree and
    # multiple series cannot create overlapping or interleaved traces.
    query(
        con,
        paste(
            "WITH requested(location_code, station_order) AS (",
            "    VALUES ('09DC007', 1), ('09AB001', 2),",
            "           ('08AA010', 3), ('08AA009', 4),",
            "           ('08AA005', 5), ('09AB004', 6), ('09DC005', 7)",
            "), requested_parameters(param_name, parameter, parameter_order) AS (",
            "    VALUES ('water level', 'Water level', 1),",
            "           ('water flow', 'Discharge', 2)",
            "), clock AS (SELECT NOW() AS end_time)",
            "SELECT r.location_code, loc.name, loc.latitude, loc.longitude,",
            "       rp.parameter,",
            "       latest.latest_value, latest.latest_time, latest.units,",
            "       latest.latest_value - previous.day_value AS change_24h,",
            "       latest.latest_value - previous.two_day_value AS change_48h,",
            "       latest.latest_value - previous.week_value AS change_1w,",
            "       EXTRACT(EPOCH FROM (clock.end_time - latest.latest_time))",
            "           / 3600.0 AS data_age_hours,",
            history_columns,
            paste0("       ", start_sql, " AS start_time,"),
            "       clock.end_time",
            "FROM requested r CROSS JOIN requested_parameters rp CROSS JOIN clock",
            "LEFT JOIN LATERAL (",
            "    SELECT l.location_id, l.name, l.latitude, l.longitude",
            "    FROM public.locations l WHERE l.location_code = r.location_code",
            "    ORDER BY l.location_id LIMIT 1",
            ") loc ON TRUE",
            "LEFT JOIN LATERAL (",
            "    SELECT ts.timeseries_id, reading.value AS latest_value,",
            "           reading.datetime AS latest_time, u.unit_name AS units",
            "    FROM continuous.timeseries ts",
            "    JOIN public.parameters p ON p.parameter_id = ts.parameter_id",
            "    LEFT JOIN public.units u ON u.unit_id = p.units_liquid",
            "    CROSS JOIN LATERAL (",
            "        SELECT mc.datetime, mc.value",
            "        FROM continuous.measurements_continuous mc",
            "        WHERE mc.timeseries_id = ts.timeseries_id",
            "          AND mc.value IS NOT NULL AND mc.datetime <= clock.end_time",
            "        ORDER BY mc.datetime DESC LIMIT 1",
            "    ) reading",
            "    WHERE ts.location_id = loc.location_id",
            "      AND p.param_name = rp.param_name",
            "    ORDER BY reading.datetime DESC, ts.timeseries_id LIMIT 1",
            ") latest ON TRUE",
            "LEFT JOIN LATERAL (",
            "    SELECT",
            "      (SELECT mc.value FROM continuous.measurements_continuous mc",
            "       WHERE mc.timeseries_id = latest.timeseries_id",
            "         AND mc.value IS NOT NULL",
            "         AND mc.datetime <= latest.latest_time - INTERVAL '24 hours'",
            "       ORDER BY mc.datetime DESC LIMIT 1) AS day_value,",
            "      (SELECT mc.value FROM continuous.measurements_continuous mc",
            "       WHERE mc.timeseries_id = latest.timeseries_id",
            "         AND mc.value IS NOT NULL",
            "         AND mc.datetime <= latest.latest_time - INTERVAL '48 hours'",
            "       ORDER BY mc.datetime DESC LIMIT 1) AS two_day_value,",
            "      (SELECT mc.value FROM continuous.measurements_continuous mc",
            "       WHERE mc.timeseries_id = latest.timeseries_id",
            "         AND mc.value IS NOT NULL",
            "         AND mc.datetime <= latest.latest_time - INTERVAL '7 days'",
            "       ORDER BY mc.datetime DESC LIMIT 1) AS week_value",
            ") previous ON TRUE",
            history_join,
            paste(
                "ORDER BY r.station_order, rp.parameter_order,",
                "history.datetime"
            )
        )
    )
}

#' Create the empty result shape used when station data cannot be loaded.
#' @keywords internal
#' @noRd
yec_dashboard_empty_data <- function() {
    data.frame(
        location_code = character(),
        name = character(),
        latitude = numeric(),
        longitude = numeric(),
        parameter = character(),
        latest_value = numeric(),
        latest_time = as.POSIXct(character(), tz = "UTC"),
        units = character(),
        change_24h = numeric(),
        change_48h = numeric(),
        change_1w = numeric(),
        data_age_hours = numeric(),
        datetime = as.POSIXct(character(), tz = "UTC"),
        value = numeric(),
        start_time = as.POSIXct(character(), tz = "UTC"),
        end_time = as.POSIXct(character(), tz = "UTC"),
        stringsAsFactors = FALSE
    )
}

#' Add the Aishihik River minus Giltana Creek discharge series.
#' @keywords internal
#' @noRd
yec_dashboard_add_aishihik_giltana_difference <- function(dat) {
    required <- c("location_code", "parameter", "datetime", "value")
    if (!all(required %in% names(dat))) {
        return(dat)
    }
    aishihik <- dat[
        dat$location_code == "08AA010" &
            dat$parameter == "Discharge" &
            is.finite(dat$value) &
            !is.na(dat$datetime),
        ,
        drop = FALSE
    ]
    giltana <- dat[
        dat$location_code == "08AA009" &
            dat$parameter == "Discharge" &
            is.finite(dat$value) &
            !is.na(dat$datetime),
        ,
        drop = FALSE
    ]
    if (!nrow(aishihik) || !nrow(giltana)) {
        return(dat)
    }

    aishihik <- aishihik[order(aishihik$datetime), , drop = FALSE]
    aishihik <- aishihik[
        !duplicated(aishihik$datetime, fromLast = TRUE),
        ,
        drop = FALSE
    ]
    giltana <- giltana[order(giltana$datetime), , drop = FALSE]
    giltana <- giltana[
        !duplicated(giltana$datetime, fromLast = TRUE),
        ,
        drop = FALSE
    ]
    matched <- findInterval(
        as.numeric(giltana$datetime),
        as.numeric(aishihik$datetime)
    )
    giltana <- giltana[matched > 0L, , drop = FALSE]
    matched <- matched[matched > 0L]
    if (!nrow(giltana)) {
        return(dat)
    }

    difference <- giltana
    difference$location_code <- "08AA010-08AA009"
    difference$name <- "Aishihik River minus Giltana Creek"
    difference$latitude <- NA_real_
    difference$longitude <- NA_real_
    difference$parameter <- "Discharge"
    difference$units <- aishihik$units[matched]
    difference$value <- aishihik$value[matched] - giltana$value
    latest_index <- which.max(difference$datetime)
    latest_time <- difference$datetime[[latest_index]]
    latest_value <- difference$value[[latest_index]]
    previous_value <- function(hours) {
        candidates <- difference$value[
            difference$datetime <= latest_time - hours * 60 * 60
        ]
        if (length(candidates)) tail(candidates, 1L) else NA_real_
    }
    difference$latest_value <- latest_value
    difference$latest_time <- latest_time
    difference$change_24h <- latest_value - previous_value(24)
    difference$change_48h <- latest_value - previous_value(48)
    difference$change_1w <- latest_value - previous_value(24 * 7)
    difference$data_age_hours <- as.numeric(
        difftime(difference$end_time, latest_time, units = "hours")
    )
    rbind(dat, difference)
}

#' Read YEC threshold definitions and display metadata
#' @keywords internal
#' @noRd
yec_dashboard_thresholds <- function(directory = NULL) {
    if (is.null(directory)) {
        directory <- if (dir.exists("data-raw/yec")) {
            "data-raw/yec"
        } else {
            system.file("extdata", "yec", package = "YGwater")
        }
    }
    read_file <- function(name) {
        utils::read.csv(
            file.path(directory, name),
            stringsAsFactors = FALSE,
            check.names = FALSE,
            strip.white = TRUE
        )
    }
    list(
        thresholds = read_file("thresholds.csv"),
        types = read_file("threshold_types.csv"),
        colours = read_file("threshold_colours.csv")
    )
}

#' Match thresholds and clip their annual validity windows to the plot period
#' @keywords internal
#' @noRd
yec_dashboard_threshold_segments <- function(
    config,
    station,
    parameter,
    units,
    start_time,
    end_time
) {
    normalize_units <- function(x) {
        x <- tolower(gsub("[[:space:]]", "", x))
        x[x %in% c("cms", "m3/s", "m^3/s", "m³/s")] <- "cms"
        x[x %in% c("metres", "meters", "metre", "meter")] <- "m"
        x
    }
    parameter_code <- c("Water level" = "level", "Discharge" = "flow")[[
        parameter
    ]]
    timezone <- "Etc/GMT+7"
    start_time <- as.POSIXct(start_time, tz = timezone)
    end_time <- as.POSIXct(end_time, tz = timezone)
    attr(start_time, "tzone") <- timezone
    attr(end_time, "tzone") <- timezone

    # Giltana Creek is displayed without a standalone flow threshold. Its
    # operational minimum is evaluated on the Aishihik-minus-Giltana series.
    if (identical(station, "08AA009") && identical(parameter, "Discharge")) {
        return(data.frame())
    }
    if (
        identical(station, "08AA010-08AA009") &&
            identical(parameter, "Discharge")
    ) {
        return(data.frame(
            start = start_time,
            end = end_time,
            value = 1.146,
            code = "Min flow",
            description = "Minimum flow",
            units = "cms",
            notes = paste(
                "Aishihik River (08AA010) discharge minus",
                "Giltana Creek (08AA009) discharge."
            ),
            colour = "#dc2626",
            stringsAsFactors = FALSE
        ))
    }

    thresholds <- config$thresholds
    keep <- thresholds$station == station &
        thresholds$parameter == parameter_code &
        normalize_units(thresholds$units) %in% normalize_units(units) &
        thresholds$threshold != "Datum" &
        is.finite(thresholds$value)
    thresholds <- thresholds[which(keep), , drop = FALSE]
    result <- data.frame()
    years <- seq.int(
        as.integer(format(start_time, "%Y", tz = timezone)) - 1L,
        as.integer(format(end_time, "%Y", tz = timezone))
    )
    for (i in seq_len(nrow(thresholds))) {
        threshold <- thresholds[i, ]
        for (year in years) {
            end_year <- year +
                as.integer(threshold$valid_to < threshold$valid_from)
            from <- as.POSIXct(
                paste(year, threshold$valid_from, sep = "-"),
                format = "%Y-%m-%d",
                tz = timezone
            )
            # valid_to includes the entire local calendar day.
            to <- as.POSIXct(
                paste(end_year, threshold$valid_to, sep = "-"),
                format = "%Y-%m-%d",
                tz = timezone
            ) +
                86400
            if (
                is.na(from) || is.na(to) || from > end_time || to <= start_time
            ) {
                next
            }
            description <- config$types$description[
                match(threshold$threshold, config$types$code)
            ]
            if (is.na(description)) {
                description <- threshold$threshold
            }
            # Use the configured colour for crossing the actual limit (lt 0).
            rules <- config$colours
            rules <- rules[
                rules$station %in%
                    c("all", station) &
                    rules$parameter == parameter_code &
                    rules$threshold == threshold$threshold &
                    rules$type == "lt" &
                    rules$value == 0,
                ,
                drop = FALSE
            ]
            rules <- rules[
                order(rules$station == station, decreasing = TRUE),
                ,
                drop = FALSE
            ]
            colour <- if (nrow(rules)) rules$colour[[1]] else "#666666"
            result <- rbind(
                result,
                data.frame(
                    start = max(from, start_time),
                    end = min(to, end_time),
                    value = threshold$value,
                    code = threshold$threshold,
                    description = description,
                    units = threshold$units,
                    notes = ifelse(is.na(threshold$notes), "", threshold$notes),
                    colour = colour,
                    stringsAsFactors = FALSE
                )
            )
        }
    }
    result
}

#' Keep station-parameter rows with a valid reading in the last 365 days
#' @keywords internal
#' @noRd
yec_dashboard_recent_rows <- function(dat) {
    keep <- is.finite(dat$latest_value) &
        !is.na(dat$latest_time) &
        is.finite(dat$data_age_hours) &
        dat$data_age_hours < 365 * 24
    dat[keep, , drop = FALSE]
}

#' Calculate signed margins against thresholds valid at each latest reading
#' @keywords internal
#' @noRd
yec_dashboard_threshold_margins <- function(dat, config) {
    result <- data.frame(
        below_fsl = rep(NA_real_, nrow(dat)),
        above_lsl = rep(NA_real_, nrow(dat)),
        below_alert = rep(NA_real_, nrow(dat)),
        above_min_flow = rep(NA_real_, nrow(dat))
    )
    for (i in seq_len(nrow(dat))) {
        if (!is.finite(dat$latest_value[[i]]) || is.na(dat$latest_time[[i]])) {
            next
        }
        thresholds <- yec_dashboard_threshold_segments(
            config,
            dat$location_code[[i]],
            dat$parameter[[i]],
            dat$units[[i]],
            dat$latest_time[[i]],
            dat$latest_time[[i]]
        )
        if (!nrow(thresholds)) {
            next
        }
        limit <- function(code) {
            values <- unique(thresholds$value[thresholds$code == code])
            # A station may have threshold segments without every margin
            # limit (for example, no Alert or Min flow row).  Returning a
            # scalar keeps the table renderer from failing on a zero-length
            # replacement.
            if (length(values) == 1L) values else NA_real_
        }
        value <- dat$latest_value[[i]]
        if (dat$parameter[[i]] == "Water level") {
            result$below_fsl[[i]] <- limit("FSL") - value
            result$above_lsl[[i]] <- value - limit("LSL")
            result$below_alert[[i]] <- limit("Alert") - value
        } else if (dat$parameter[[i]] == "Discharge") {
            result$above_min_flow[[i]] <- value - limit("Min flow")
        }
    }
    result
}

#' Find the configured cell colour for each threshold margin
#'
#' Station-specific rules take precedence over `all` rules.  Where more than
#' one rule is met, use the closest more-severe boundary (the lowest `lt`
#' boundary or highest `gt` boundary).
#' @keywords internal
#' @noRd
yec_dashboard_threshold_margin_colours <- function(dat, margins, config) {
    definitions <- data.frame(
        margin = c(
            "below_fsl",
            "above_lsl",
            "below_alert",
            "above_min_flow"
        ),
        threshold = c("FSL", "LSL", "Alert", "Min flow"),
        parameter = c("level", "level", "level", "flow"),
        stringsAsFactors = FALSE
    )
    result <- as.data.frame(
        lapply(definitions$margin, function(x) rep(NA_character_, nrow(dat))),
        stringsAsFactors = FALSE
    )
    names(result) <- definitions$margin
    for (i in seq_len(nrow(dat))) {
        parameter <- if (dat$parameter[[i]] == "Water level") {
            "level"
        } else {
            "flow"
        }
        for (j in seq_len(nrow(definitions))) {
            definition <- definitions[j, ]
            value <- margins[[definition$margin]][[i]]
            if (
                !identical(parameter, definition$parameter) || !is.finite(value)
            ) {
                next
            }
            rules <- config$colours
            rules <- rules[
                rules$parameter == definition$parameter &
                    rules$threshold == definition$threshold &
                    rules$station %in% c("all", dat$location_code[[i]]),
                ,
                drop = FALSE
            ]
            station_rules <- rules[
                rules$station == dat$location_code[[i]],
                ,
                drop = FALSE
            ]
            if (nrow(station_rules)) {
                rules <- station_rules
            }
            matched <- rules[
                (rules$type == "lt" & value < rules$value) |
                    (rules$type == "gt" & value > rules$value),
                ,
                drop = FALSE
            ]
            if (!nrow(matched)) {
                next
            }
            lt <- matched[matched$type == "lt", , drop = FALSE]
            gt <- matched[matched$type == "gt", , drop = FALSE]
            chosen <- if (nrow(lt)) {
                lt[which.min(lt$value), , drop = FALSE]
            } else {
                gt[which.max(gt$value), , drop = FALSE]
            }
            result[[definition$margin]][[i]] <- chosen$colour[[1]]
        }
    }
    result
}

#' Render a threshold margin with its configured background colour
#' @keywords internal
#' @noRd
yec_dashboard_coloured_margin <- function(values, colours) {
    palette <- c(
        green = "#b7e1cd",
        yellow = "#ffe599",
        orange = "#f9cb9c",
        red = "#e06666"
    )
    value_text <- ifelse(
        is.finite(values),
        format(round(values, 3), trim = TRUE, nsmall = 3),
        "No data"
    )
    vapply(
        seq_along(value_text),
        function(i) {
            colour <- unname(palette[colours[[i]]])
            text <- htmltools::htmlEscape(value_text[[i]])
            if (is.na(colour)) {
                return(text)
            }
            text_colour <- if (identical(colours[[i]], "red")) {
                "#ffffff"
            } else {
                "#000000"
            }
            paste0(
                "<span style=\"display:block;padding:2px 4px;",
                "background-color:",
                colour,
                ";color:",
                text_colour,
                ";\">",
                text,
                "</span>"
            )
        },
        character(1)
    )
}

#' Build hydrograph background bands from the threshold colour rules
#' @keywords internal
#' @noRd
yec_dashboard_threshold_background <- function(
    segments,
    parameter,
    y_range
) {
    if (
        !nrow(segments) ||
            length(y_range) != 2L ||
            !is.finite(y_range[1]) ||
            !is.finite(y_range[2])
    ) {
        return(list())
    }
    palette <- c(
        green = "#b7e1cd",
        yellow = "#ffe599",
        orange = "#f9cb9c",
        red = "#e06666"
    )
    plot_time <- function(value) {
        format(
            as.POSIXct(value, tz = "Etc/GMT+7"),
            "%Y-%m-%d %H:%M:%S",
            tz = "Etc/GMT+7"
        )
    }
    rectangle <- function(start, end, y0, y1, colour) {
        if (!is.finite(y0) || !is.finite(y1) || y1 <= y0 || is.na(colour)) {
            return(NULL)
        }
        list(
            type = "rect",
            xref = "x",
            yref = "y",
            x0 = plot_time(start),
            x1 = plot_time(end),
            y0 = y0,
            y1 = y1,
            fillcolor = colour,
            opacity = 0.28,
            line = list(width = 0),
            layer = "below"
        )
    }
    result <- list()
    boundaries <- sort(unique(c(segments$start, segments$end)))
    for (i in seq_len(length(boundaries) - 1L)) {
        start <- boundaries[[i]]
        end <- boundaries[[i + 1L]]
        active <- segments[
            segments$start <= start & segments$end >= end,
            ,
            drop = FALSE
        ]
        limit <- function(code) {
            values <- active$value[active$code == code]
            if (length(values)) values[[1]] else NA_real_
        }
        fsl <- limit("FSL")
        lsl <- limit("LSL")
        alert <- limit("Alert")
        min_flow <- limit("Min flow")
        if (identical(parameter, "Water level")) {
            # Level zones are intentionally finite: they show the operating
            # bands immediately around the limits without colouring the rest
            # of the y-axis.  Draw green first, then the warning/breach bands.
            if (is.finite(alert)) {
                result[[length(result) + 1L]] <- rectangle(
                    start,
                    end,
                    y_range[1],
                    alert - 0.1,
                    palette[["green"]]
                )
                result[[length(result) + 1L]] <- rectangle(
                    start,
                    end,
                    alert - 0.1,
                    alert,
                    palette[["yellow"]]
                )
                result[[length(result) + 1L]] <- rectangle(
                    start,
                    end,
                    alert,
                    alert + 10,
                    palette[["red"]]
                )
            }
            if (is.finite(fsl) && is.finite(lsl)) {
                result[[length(result) + 1L]] <- rectangle(
                    start,
                    end,
                    lsl + 0.1,
                    fsl - 0.1,
                    palette[["green"]]
                )
            }
            if (is.finite(lsl)) {
                result[[length(result) + 1L]] <- rectangle(
                    start,
                    end,
                    0,
                    lsl,
                    palette[["red"]]
                )
                result[[length(result) + 1L]] <- rectangle(
                    start,
                    end,
                    lsl,
                    lsl + 0.1,
                    palette[["yellow"]]
                )
            }
            if (is.finite(fsl)) {
                result[[length(result) + 1L]] <- rectangle(
                    start,
                    end,
                    fsl - 0.1,
                    fsl,
                    palette[["yellow"]]
                )
                result[[length(result) + 1L]] <- rectangle(
                    start,
                    end,
                    fsl,
                    fsl + 10,
                    palette[["red"]]
                )
            }
        } else if (is.finite(min_flow)) {
            # Keep discharge zones simple and independent of the CSV rules.
            warning_width <- 0.1
            result[[length(result) + 1L]] <- rectangle(
                start,
                end,
                min_flow + warning_width,
                y_range[2],
                palette[["green"]]
            )
            result[[length(result) + 1L]] <- rectangle(
                start,
                end,
                min_flow,
                min_flow + warning_width,
                palette[["yellow"]]
            )
            result[[length(result) + 1L]] <- rectangle(
                start,
                end,
                y_range[1],
                min_flow,
                palette[["red"]]
            )
        }
    }
    for (boundary in boundaries) {
        result[[length(result) + 1L]] <- list(
            type = "line",
            xref = "x",
            yref = "paper",
            x0 = plot_time(boundary),
            x1 = plot_time(boundary),
            y0 = 0,
            y1 = 1,
            line = list(
                color = "rgba(80, 80, 80, 0.35)",
                width = 1,
                dash = "dot"
            ),
            layer = "below"
        )
    }
    Filter(Negate(is.null), result)
}

#' Compute plot limits from observations only, ignoring thresholds
#' @keywords internal
#' @noRd
yec_dashboard_trace_range <- function(values) {
    values <- values[is.finite(values)]
    if (!length(values)) {
        return(NULL)
    }
    limits <- range(values)
    padding <- if (diff(limits) > 0) {
        diff(limits) * 0.05
    } else {
        max(abs(limits[1]) * 0.001, 0.01)
    }
    limits + c(-padding, padding)
}

#' YEC Dashboard Server Module
#'
#' @param id Character module id.
#' @param language Reactive language selection (for app compatibility).
#' @param inputs Optional module inputs.
#' @keywords internal
#' @noRd
yecDashboardMod <- function(id, language, inputs = NULL) {
    shiny::moduleServer(id, function(input, output, session) {
        station_codes <- c(
            "09DC007",
            "09AB001",
            "08AA010",
            "08AA009",
            "08AA010-08AA009",
            "08AA005",
            "09AB004",
            "09DC005"
        )
        discharge_station_codes <- c(
            "09DC007",
            "09AB001",
            "08AA010",
            "08AA009",
            "08AA010-08AA009"
        )
        level_station_codes <- c("09AB001", "08AA005", "09AB004", "09DC005")
        station_colors <- stats::setNames(
            c(
                "#0072B2",
                "#D55E00",
                "#009E73",
                "#CC79A7",
                "#984EA3",
                "#E69F00",
                "#56B4E9",
                "#333333"
            ),
            station_codes
        )
        local_timezone <- "Etc/GMT+7"
        threshold_config <- yec_dashboard_thresholds()

        readings <- shiny::reactive({
            shiny::invalidateLater(5 * 60 * 1000, session)
            con <- session$userData$AquaCache
            shiny::req(con)
            record_length <- input$record_length
            if (is.null(record_length) || !nzchar(record_length)) {
                record_length <- "1-week"
            }
            tryCatch(
                {
                    dat <- as.data.frame(yec_dashboard_data(
                        con,
                        record_length = record_length
                    ))
                    yec_dashboard_add_aishihik_giltana_difference(dat)
                },
                error = function(e) {
                    error_message <- paste(
                        "YEC dashboard query failed:",
                        conditionMessage(e)
                    )
                    message(error_message)
                    yec_dashboard_empty_data()
                }
            )
        })

        station_parameters <- shiny::reactive({
            dat <- readings()
            allowed <- (dat$location_code %in%
                discharge_station_codes &
                dat$parameter == "Discharge") |
                (dat$location_code %in%
                    level_station_codes &
                    dat$parameter == "Water level")
            dat <- dat[allowed, , drop = FALSE]
            dat[
                !duplicated(dat[c("location_code", "parameter")]),
                ,
                drop = FALSE
            ]
        })

        visible_station_parameters <- shiny::reactive({
            yec_dashboard_recent_rows(station_parameters())
        })
        series_key <- function(x) {
            paste(x$location_code, x$parameter, sep = " / ")
        }
        # The level and discharge tables retain their own station selection.
        bind_parameter_table <- function(table_id, parameter) {
            displayed_series <- shiny::reactiveVal(character())
            selected_series <- shiny::reactiveVal(character())
            selection_input <- paste0(table_id, "_rows_selected")
            shiny::observeEvent(
                input[[selection_input]],
                {
                    rows <- input[[selection_input]]
                    keys <- displayed_series()
                    rows <- rows[
                        !is.na(rows) & rows >= 1L & rows <= length(keys)
                    ]
                    if (length(rows)) {
                        selected_series(keys[rows])
                    }
                },
                ignoreNULL = FALSE
            )

            # Default to row one, retaining valid multiple selections.
            shiny::observe({
                dat <- visible_station_parameters()
                dat <- dat[dat$parameter == parameter, , drop = FALSE]
                keys <- series_key(dat)
                current <- selected_series()
                if (!length(keys)) {
                    selected_series(character())
                } else if (!length(current) || !all(current %in% keys)) {
                    selected_series(keys[[1]])
                }
            })

            # A map marker selects that station in the table currently shown.
            # Markers identify stations, while a table row identifies both the
            # station and its active parameter.
            shiny::observeEvent(
                input$stations_map_marker_click,
                {
                    if (!identical(input$parameter_view, parameter)) {
                        return()
                    }
                    station_code <- input$stations_map_marker_click$id
                    dat <- visible_station_parameters()
                    dat <- dat[
                        dat$parameter == parameter &
                            dat$location_code == station_code,
                        ,
                        drop = FALSE
                    ]
                    if (nrow(dat)) {
                        selected_series(series_key(dat)[[1]])
                    }
                },
                ignoreNULL = TRUE
            )

            output[[table_id]] <- DT::renderDT(
                {
                    dat <- visible_station_parameters()
                    dat <- dat[dat$parameter == parameter, , drop = FALSE]
                    keys <- series_key(dat)
                    selected_rows <- which(keys %in% selected_series())
                    displayed_series(keys)
                    table <- data.frame(
                        station = dat$location_code,
                        name = ifelse(
                            is.na(dat$name),
                            "Station unavailable",
                            dat$name
                        ),
                        value = ifelse(
                            is.na(dat$latest_value),
                            "No data",
                            format(
                                round(dat$latest_value, 3),
                                trim = TRUE,
                                nsmall = 3
                            )
                        ),
                        timestamp = ifelse(
                            is.na(dat$latest_time),
                            "No data",
                            format(
                                as.POSIXct(dat$latest_time, tz = "UTC"),
                                "%Y-%m-%d %H:%M:%S",
                                tz = local_timezone
                            )
                        ),
                        change_24h = round(dat$change_24h, 3),
                        change_48h = round(dat$change_48h, 3),
                        change_1w = round(dat$change_1w, 3),
                        data_age_hours = round(dat$data_age_hours, 1)
                    )
                    names(table) <- c(
                        "Station",
                        "Name",
                        "Latest value",
                        "Latest timestamp (UTC-7)",
                        "24 hour change",
                        "48 hour change",
                        "1 week change",
                        "Data age (hours)"
                    )
                    margins <- yec_dashboard_threshold_margins(
                        dat,
                        threshold_config
                    )
                    margin_colours <- yec_dashboard_threshold_margin_colours(
                        dat,
                        margins,
                        threshold_config
                    )
                    if (parameter == "Water level") {
                        table[["m below FSL"]] <- yec_dashboard_coloured_margin(
                            margins$below_fsl,
                            margin_colours$below_fsl
                        )
                        table[["m above LSL"]] <- yec_dashboard_coloured_margin(
                            margins$above_lsl,
                            margin_colours$above_lsl
                        )
                        table[[
                            "m below alert"
                        ]] <- yec_dashboard_coloured_margin(
                            margins$below_alert,
                            margin_colours$below_alert
                        )
                    } else {
                        table[[
                            "cms above min flow"
                        ]] <- yec_dashboard_coloured_margin(
                            margins$above_min_flow,
                            margin_colours$above_min_flow
                        )
                    }
                    margin_columns <- intersect(
                        names(table),
                        c(
                            "m below FSL",
                            "m above LSL",
                            "m below alert",
                            "cms above min flow"
                        )
                    )
                    DT::datatable(
                        table,
                        rownames = FALSE,
                        # Margin cells contain only escaped values generated by
                        # yec_dashboard_coloured_margin(); retain escaping for
                        # every database-supplied table column.
                        escape = which(!names(table) %in% margin_columns),
                        selection = list(
                            mode = "single",
                            target = "row",
                            selected = selected_rows
                        ),
                        options = list(
                            dom = "t",
                            paging = FALSE,
                            scrollX = TRUE,
                            ordering = FALSE,
                            language = list(
                                emptyTable = "No readings within the last 365 days."
                            ),
                            columnDefs = list(list(
                                targets = "_all",
                                defaultContent = "No data"
                            ))
                        )
                    )
                },
                server = FALSE
            )
            selected_series
        }
        level_selection <- bind_parameter_table("level_table", "Water level")
        flow_selection <- bind_parameter_table("flow_table", "Discharge")
        selected_series <- shiny::reactive({
            if (identical(input$parameter_view, "Discharge")) {
                flow_selection()
            } else {
                level_selection()
            }
        })

        output$stations_map <- leaflet::renderLeaflet({
            dat <- station_parameters()
            dat <- dat[!duplicated(dat$location_code), , drop = FALSE]
            dat <- dat[
                is.finite(dat$longitude) & is.finite(dat$latitude),
                ,
                drop = FALSE
            ]
            map <- leaflet::addProviderTiles(
                leaflet::leaflet(),
                leaflet::providers$Esri.WorldTopoMap
            )
            if (nrow(dat) == 0) {
                return(leaflet::addControl(
                    leaflet::setView(map, lng = -135, lat = 62, zoom = 5),
                    "Station coordinates unavailable."
                ))
            }
            map <- leaflet::addCircleMarkers(
                map,
                lng = dat$longitude,
                lat = dat$latitude,
                layerId = dat$location_code,
                label = paste(dat$location_code, dat$name, sep = " — "),
                color = unname(station_colors[dat$location_code]),
                radius = 7,
                fillOpacity = 0.9,
                weight = 2
            )
            if (nrow(dat) == 1) {
                return(leaflet::setView(
                    map,
                    lng = dat$longitude[[1]],
                    lat = dat$latitude[[1]],
                    zoom = 9
                ))
            }
            leaflet::fitBounds(
                map,
                min(dat$longitude),
                min(dat$latitude),
                max(dat$longitude),
                max(dat$latitude)
            )
        })

        output$record_resolution <- shiny::renderText({
            if (
                isTRUE(
                    input$record_length %in%
                        c(
                            "6-months",
                            "1-year",
                            "entire-record"
                        )
                )
            ) {
                "Daily averages"
            } else {
                "Continuous observations"
            }
        })

        output$station_plot <- plotly::renderPlotly({
            dat <- readings()
            if (!nrow(dat)) {
                return(plotly::layout(
                    plotly::plot_ly(),
                    annotations = list(list(
                        text = "YEC station data are unavailable.",
                        x = 0.5,
                        y = 0.5,
                        xref = "paper",
                        yref = "paper",
                        showarrow = FALSE
                    )),
                    xaxis = list(visible = FALSE),
                    yaxis = list(visible = FALSE)
                ))
            }
            available <- visible_station_parameters()
            selected <- available[
                series_key(available) %in% selected_series(),
                ,
                drop = FALSE
            ]
            selected_keys <- series_key(selected)
            selected_data <- dat[
                series_key(dat) %in% selected_keys,
                ,
                drop = FALSE
            ]
            mixed_parameters <- length(unique(selected$parameter)) > 1
            # Explicit local timestamps keep the axis and hover text in UTC-7.
            local_time <- function(x) {
                format(
                    as.POSIXct(x, tz = "UTC"),
                    "%Y-%m-%d %H:%M:%S",
                    tz = local_timezone
                )
            }
            observed <- selected_data[
                is.finite(selected_data$value) & !is.na(selected_data$datetime),
                ,
                drop = FALSE
            ]
            record_length <- input$record_length
            if (is.null(record_length) || !nzchar(record_length)) {
                record_length <- "1-week"
            }
            x_range <- c(dat$start_time[[1]], dat$end_time[[1]])
            if (identical(record_length, "entire-record") && nrow(observed)) {
                x_range[[1]] <- min(observed$datetime)
            }
            # Keep a one-week context for threshold display and plot reset.
            threshold_context <- x_range + c(-7, 7) * 24 * 60 * 60
            plot <- plotly::plot_ly()
            for (key in selected_keys) {
                trace <- selected_data[
                    series_key(selected_data) == key &
                        !is.na(selected_data$datetime),
                    ,
                    drop = FALSE
                ]
                if (nrow(trace) == 0) {
                    next
                }
                code <- trace$location_code[[1]]
                discharge <- trace$parameter[[1]] == "Discharge"
                plot <- plotly::add_trace(
                    plot,
                    x = local_time(trace$datetime),
                    y = trace$value,
                    type = "scatter",
                    mode = "lines+markers",
                    name = key,
                    yaxis = if (mixed_parameters && discharge) "y2" else "y",
                    line = list(
                        color = station_colors[[code]],
                        dash = if (discharge) "dash" else "solid"
                    ),
                    marker = list(color = station_colors[[code]], size = 3),
                    connectgaps = FALSE,
                    hovertemplate = paste0(
                        key,
                        "<br>%{x}<br>%{y}<extra></extra>"
                    )
                )
                thresholds <- yec_dashboard_threshold_segments(
                    threshold_config,
                    code,
                    trace$parameter[[1]],
                    trace$units[[1]],
                    threshold_context[[1]],
                    threshold_context[[2]]
                )
                for (i in seq_len(nrow(thresholds))) {
                    threshold <- thresholds[i, ]
                    label <- paste(key, threshold$code, sep = " / ")
                    hover <- paste0(
                        htmltools::htmlEscape(label),
                        "<br>",
                        htmltools::htmlEscape(threshold$description),
                        "<br>",
                        threshold$value,
                        " ",
                        htmltools::htmlEscape(threshold$units),
                        "<br>",
                        htmltools::htmlEscape(threshold$notes)
                    )
                    plot <- plotly::add_trace(
                        plot,
                        x = local_time(c(threshold$start, threshold$end)),
                        y = rep(threshold$value, 2),
                        type = "scatter",
                        mode = "lines",
                        name = label,
                        legendgroup = label,
                        showlegend = !threshold$code %in%
                            thresholds$code[seq_len(i - 1L)],
                        yaxis = if (mixed_parameters && discharge) {
                            "y2"
                        } else {
                            "y"
                        },
                        line = list(
                            color = threshold$colour,
                            dash = "dot",
                            width = 2
                        ),
                        text = rep(hover, 2),
                        hovertemplate = "%{text}<extra></extra>"
                    )
                }
            }
            axis_title <- function(parameter) {
                units <- unique(stats::na.omit(
                    selected_data$units[selected_data$parameter == parameter]
                ))
                if (length(units) == 1) {
                    paste0(parameter, " (", units, ")")
                } else {
                    parameter
                }
            }
            primary_parameter <- if (mixed_parameters || nrow(selected) == 0) {
                "Water level"
            } else {
                selected$parameter[[1]]
            }
            y_range <- yec_dashboard_trace_range(observed$value)
            background_shapes <- list()
            if (
                !mixed_parameters && nrow(selected) == 1L && !is.null(y_range)
            ) {
                thresholds <- yec_dashboard_threshold_segments(
                    threshold_config,
                    selected$location_code[[1]],
                    selected$parameter[[1]],
                    selected$units[[1]],
                    threshold_context[[1]],
                    threshold_context[[2]]
                )
                background_shapes <- yec_dashboard_threshold_background(
                    thresholds,
                    selected$parameter[[1]],
                    y_range
                )
            }
            plot <- plotly::layout(
                plot,
                xaxis = list(
                    title = "Time (UTC-7)",
                    type = "date",
                    range = local_time(x_range),
                    autorange = FALSE
                ),
                yaxis = list(
                    title = axis_title(primary_parameter),
                    range = y_range,
                    # Explicit trace-only limits prevent background zones and
                    # threshold lines from changing the plotted scale.
                    autorange = FALSE
                ),
                legend = list(orientation = "h", y = -0.25),
                margin = list(b = 100, r = if (mixed_parameters) 80 else 30),
                shapes = background_shapes
            )
            if (mixed_parameters) {
                plot <- plotly::layout(
                    plot,
                    yaxis2 = list(
                        title = axis_title("Discharge"),
                        overlaying = "y",
                        side = "right",
                        showgrid = FALSE
                    )
                )
            }
            if (!is.null(y_range)) {
                reset_x <- jsonlite::toJSON(
                    local_time(threshold_context),
                    auto_unbox = TRUE
                )
                reset_y <- jsonlite::toJSON(unname(y_range), auto_unbox = TRUE)
                plot <- htmlwidgets::onRender(
                    plot,
                    paste0(
                        "function(el) { el.on('plotly_doubleclick', function() {",
                        "Plotly.relayout(el, {'xaxis.range': ",
                        reset_x,
                        ", 'yaxis.range': ",
                        reset_y,
                        ", 'yaxis.autorange': false}); return false; }); }"
                    )
                )
            }
            if (
                !any(
                    is.finite(selected_data$value) &
                        !is.na(selected_data$datetime)
                )
            ) {
                plot <- plotly::layout(
                    plot,
                    annotations = list(list(
                        text = if (nrow(selected) == 0) {
                            "Select one or more rows in the table."
                        } else {
                            "No data in the selected record length for the selected rows."
                        },
                        x = 0.5,
                        y = 0.5,
                        xref = "paper",
                        yref = "paper",
                        showarrow = FALSE
                    ))
                )
            }
            plotly::config(
                plot,
                displaylogo = FALSE,
                doubleClick = FALSE
            )
        })
    })
}
