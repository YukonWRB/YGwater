params_map_daily_stats_select_sql <- function(
  con,
  stats_period = "30yr",
  columns = c("percent_historic_range", "max", "min", "doy_count")
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
      source_sql <- as.character(DBI::dbQuoteIdentifier(con, source_column))
      if (identical(source_column, column)) {
        source_sql
      } else {
        paste0(
          source_sql,
          " AS ",
          as.character(DBI::dbQuoteIdentifier(con, column))
        )
      }
    },
    character(1)
  )
  attr(out, "count_column") <- paste0("doy_count", suffix)
  out
}

# Small inline-SVG line plot for a station's map popup. `x` and `x_range` are
# numeric (days for dates, seconds for date-times); `x_labels` are the two
# labels shown under the start and end of `x_range`; `target`, if inside the
# range, is drawn as a dashed vertical line.
params_map_popup_plot <- function(
  x,
  y,
  x_range,
  x_labels,
  target = NULL,
  width = 260,
  height = 120,
  colour = "#0097A9"
) {
  ok <- is.finite(x) &
    is.finite(y) &
    x >= x_range[[1]] &
    x <= x_range[[2]]
  x <- x[ok]
  y <- y[ok]
  if (length(x) < 2) {
    return(paste0(
      '<div style="color: #666; font-size: 0.85em; padding-top: 4px;">',
      "No data for this station in the timeline.</div>"
    ))
  }
  order_x <- order(x)
  x <- x[order_x]
  y <- y[order_x]

  left <- 44
  right <- 8
  top <- 8
  bottom <- 22
  plot_w <- width - left - right
  plot_h <- height - top - bottom
  y_range <- range(y)
  if (diff(y_range) == 0) {
    y_range <- y_range + c(-1, 1)
  }
  if (diff(x_range) <= 0) {
    x_range <- x_range + c(-1, 1)
  }
  sx <- function(v) left + (v - x_range[[1]]) / diff(x_range) * plot_w
  sy <- function(v) top + (1 - (v - y_range[[1]]) / diff(y_range)) * plot_h

  # Break the line where there is a gap of more than 3x the usual spacing
  gaps <- diff(x)
  breaks <- c(0, which(gaps > 3 * stats::median(gaps)), length(x))
  segments <- vapply(
    seq_len(length(breaks) - 1),
    function(i) {
      idx <- (breaks[[i]] + 1):breaks[[i + 1]]
      if (length(idx) == 1) {
        sprintf(
          '<circle cx="%.1f" cy="%.1f" r="1.5" fill="%s"/>',
          sx(x[idx]),
          sy(y[idx]),
          colour
        )
      } else {
        sprintf(
          '<polyline fill="none" stroke="%s" stroke-width="1.5" points="%s"/>',
          colour,
          paste(sprintf("%.1f,%.1f", sx(x[idx]), sy(y[idx])), collapse = " ")
        )
      }
    },
    character(1)
  )

  format_number <- function(v) {
    format(signif(v, 3), big.mark = ",", scientific = FALSE, trim = TRUE)
  }
  text <- function(x_pos, y_pos, anchor, label) {
    sprintf(
      '<text x="%.1f" y="%.1f" text-anchor="%s">%s</text>',
      x_pos,
      y_pos,
      anchor,
      htmltools::htmlEscape(label)
    )
  }
  target_line <- if (
    length(target) == 1L &&
      is.finite(target) &&
      target >= x_range[[1]] &&
      target <= x_range[[2]]
  ) {
    sprintf(
      '<line x1="%1$.1f" y1="%2$.1f" x2="%1$.1f" y2="%3$.1f" stroke="#888" stroke-dasharray="3,3"/>',
      sx(target),
      top,
      top + plot_h
    )
  } else {
    ""
  }

  paste0(
    sprintf(
      '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d" style="display: block; margin-top: 6px; font: 10px sans-serif; fill: #444;">',
      width,
      height,
      width,
      height
    ),
    sprintf(
      '<path d="M%1$.1f %2$.1f V%3$.1f H%4$.1f" fill="none" stroke="#bbb"/>',
      left,
      top,
      top + plot_h,
      left + plot_w
    ),
    text(left - 4, top + 7, "end", format_number(y_range[[2]])),
    text(left - 4, top + plot_h, "end", format_number(y_range[[1]])),
    text(left, height - 6, "start", x_labels[[1]]),
    text(left + plot_w, height - 6, "end", x_labels[[2]]),
    target_line,
    paste(segments, collapse = ""),
    "</svg>"
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
    page_sidebar(
      sidebar = sidebar(
        title = NULL,
        bg = config$sidebar_bg,
        open = list(mobile = "always-above"),
        uiOutput(ns("sidebar_controls"))
      ),
      tags$div(
        style = "height: 80vh; display: flex; flex-direction: column;",
        leaflet::leafletOutput(ns("map"), height = "calc(100% - 105px)"),
        tags$div(
          # Tall enough for the label, slider and tick labels. Extra side
          # padding leaves room for the wider hourly labels at the ends, and
          # overflow-x: clip (unlike 'hidden') trims them sideways without
          # turning the vertical overflow into a scroll bar.
          style = "height: 105px; padding: 0.4rem 3.5rem 0; overflow-x: clip;",
          sliderInput(
            ns("map_date_slider"),
            label = "Target date",
            min = Sys.Date() - 30,
            max = Sys.Date(),
            value = Sys.Date(),
            timeFormat = "%Y-%m-%d",
            width = "100%"
          )
        )
      )
    )
  )
} # End of mapParamsUI

mapParams <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (session$userData$user_logged_in) {
      cached <- map_params_module_data(
        con = session$userData$AquaCache,
        env = session$userData$app_cache
      )
    } else {
      cached <- map_params_module_data(con = session$userData$AquaCache)
    }

    moduleData <- reactiveValues(
      locations = cached$locations,
      timeseries = cached$timeseries,
      parameters = cached$parameters
    )

    mapCreated <- reactiveVal(FALSE) # Track whether the map has been initialized on the client

    parameter_label <- function(parameter_id) {
      parameter_id <- suppressWarnings(as.integer(parameter_id))
      if (length(parameter_id) == 1L && !is.na(parameter_id)) {
        canonical_name <- tryCatch(
          DBI::dbGetQuery(
            session$userData$AquaCache,
            paste0(
              "SELECT param_name FROM public.parameters WHERE parameter_id = ",
              parameter_id,
              " LIMIT 1"
            )
          )$param_name,
          error = function(e) character()
        )
        canonical_name <- as.character(canonical_name)
        canonical_name <- canonical_name[!is.na(canonical_name) & nzchar(canonical_name)]
        if (length(canonical_name)) return(canonical_name[[1]])
      }
      values <- parameter_names(parameter_id)
      values <- values[!is.na(values) & nzchar(values)]
      if (length(values)) values[[1]] else "Unknown parameter"
    }
    # Plain vector indexing on purpose: inside a data.table's [ ], a bare
    # 'parameter_id' would mean the column, not this function's argument.
    parameter_names <- function(parameter_id) {
      ids <- as.character(moduleData$parameters$parameter_id)
      as.character(moduleData$parameters$param_name[
        ids %in% as.character(parameter_id)
      ])
    }
    parameter_internal_name <- function(parameter_id) {
      values <- parameter_names(parameter_id)
      values <- values[!is.na(values) & nzchar(values)]
      if (length(values)) tolower(values[[1]]) else ""
    }

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
      req(moduleData, language)
      # The new slider reports its initial value; don't treat that as an edit.
      slider_echo[["colour_limits"]] <- c(
        map_params$colour_lower,
        map_params$colour_upper
      )
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
          selected = "range",
          multiple = FALSE
        ),
        tags$div(
          style = "margin-bottom: 0.4rem;",
          dateInput(
            ns("target"),
            label = tr("map_target_date", language$language),
            value = Sys.Date(),
            max = Sys.Date(),
            format = "yyyy-mm-dd",
            language = language$abbrev
          )
        ),
        tags$div(
          style = "margin-bottom: 0.4rem;",
          selectizeInput(
            ns("timeline_days"),
            label = tooltip(
              trigger = list(
                "Timeline",
                bsicons::bs_icon("info-circle-fill")
              ),
              paste(
                "Timelines of 7 days or more step through days and map daily",
                "means. The 24 and 48 hour timelines step through hours and map",
                "the most recent measurement at or before the selected hour,",
                "compared with the historic range for that day."
              )
            ),
            choices = c(
              "2 years" = 730,
              "1 year" = 365,
              "6 months" = 183,
              "30 days" = 30,
              "7 days" = 7,
              "48 hours" = 2,
              "24 hours" = 1
            ),
            selected = map_params$timeline_days,
            multiple = FALSE
          )
        ),
        # 'Latest measurements' and its 15-minute auto-update checkbox are
        # hidden for now. Without them map_params$latest stays FALSE, so the
        # latest-mode code below never runs. Restore these two inputs to bring
        # them back.
        # checkboxInput(
        #   ns("latest"),
        #   tr("map_latest_measurements", language$language),
        #   value = FALSE
        # ),
        # conditionalPanel(
        #   condition = "input.latest == true",
        #   ns = ns,
        #   checkboxInput(
        #     ns("auto_update_latest"),
        #     tr("map_latest_auto_update", language$language),
        #     value = FALSE
        #   )
        # ),
        htmlOutput(ns("primary_param")), # Primary parameter information, rendered separately as it needs to update if selections change
        actionButton(
          ns("edit_primary_param"),
          if (config$public) {
            tr("map_edit_primary_param_solo", language$language)
          } else {
            tr("map_edit_primary_param", language$language)
          },
          style = "display: block; width: 100%; margin-bottom: 0.4rem;",
          class = "btn btn-primary"
        ),
        accordion(
          id = ns("primary_advanced_options"),
          open = FALSE,
          accordion_panel(
            title = "Advanced options",
            selectizeInput(
              ns("colour_palette"),
              "Colour palette",
              choices = c(
                "Default" = "default",
                "Turbo" = "turbo",
                "Blues" = "blues",
                "Viridis" = "viridis",
                "Plasma" = "plasma",
                "Spectral" = "spectral",
                "Brewer YlGnBu" = "ylgnbu"
              ),
              selected = map_params$colour_palette,
              multiple = FALSE
            ),
            sliderInput(
              ns("colour_limits"),
              "Colour scale limits",
              min = -100,
              max = 200,
              value = c(map_params$colour_lower, map_params$colour_upper),
              step = 1
            ),
            checkboxInput(
              ns("reverse_colormap"),
              "Reverse colormap",
              value = FALSE
            ),
            checkboxInput(
              ns("show_value_annotations"),
              "Label values",
              value = FALSE
            )
          )
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
            style = "display: block; width: 100%; margin-top: 0.4rem;",
            class = "btn btn-primary"
          )
        },
        if (!config$public) uiOutput(ns("secondary_advanced_options")),
        uiOutput(ns("data_availability_disclaimer"))
      )
    }) |>
      bindEvent(language$language)

    output$secondary_advanced_options <- renderUI({
      if (map_params$params != 2) {
        return(NULL)
      }
      # The new slider reports its initial value; don't treat that as an edit.
      slider_echo[["secondary_colour_limits"]] <- c(
        map_params$colour_lower2,
        map_params$colour_upper2
      )
      accordion(
        id = ns("secondary_advanced_options_accordion"),
        open = FALSE,
        accordion_panel(
          title = "Advanced options",
          selectizeInput(
            ns("secondary_colour_palette"),
            "Colour palette",
            choices = c(
              "Default" = "default",
              "Turbo" = "turbo",
              "Blues" = "blues",
              "Viridis" = "viridis",
              "Plasma" = "plasma",
              "Spectral" = "spectral",
              "Brewer YlGnBu" = "ylgnbu"
            ),
            selected = map_params$colour_palette2,
            multiple = FALSE
          ),
          sliderInput(
            ns("secondary_colour_limits"),
            "Colour scale limits",
            min = -100,
            max = 200,
            value = c(map_params$colour_lower2, map_params$colour_upper2),
            step = 1
          ),
          checkboxInput(
            ns("secondary_reverse_colormap"),
            "Reverse colormap",
            value = map_params$reverse_colormap2
          )
        )
      )
    }) |>
      bindEvent(map_params$params, language$language)

    output$data_availability_disclaimer <- renderUI({
      criteria <- paste0(
        "Stations are shown only when they have at least ",
        map_params$yrs1,
        " years of record and data within ",
        map_params$days1,
        " day(s) of the target date."
      )
      accordion(
        id = ns("data_availability_disclaimer"),
        open = FALSE,
        accordion_panel(title = "Data availability", tags$p(criteria))
      )
    }) |>
      bindEvent(map_params$yrs1, map_params$days1)

    output$primary_param <- renderUI({
      req(map_params, language)
      tagList(
        h4(
          if (config$public) {
            tr("parameter", language$language)
          } else {
            tr("map_primary_param", language$language)
          }
        ), # Text for primary parameter
        p(paste0(
          parameter_label(map_params$param1),
          aggregation_suffix(map_params$agg1)
        ))
      )
    }) |>
      bindEvent(map_params$param1, map_params$agg1, language$language)

    output$secondary_param <- renderUI({
      req(map_params, language)
      if (config$public || map_params$params == 1) {
        return(NULL)
      } else {
        tagList(
          h4(tr("map_second_param", language$language)), # Text for secondary parameter
          p(paste0(
            parameter_label(map_params$param2),
            aggregation_suffix(map_params$agg2)
          ))
        )
      }
    }) |>
      bindEvent(map_params$param2, map_params$agg2, language$language)

    default_primary_parameter <- {
      matched <- tryCatch(
        DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT parameter_id FROM public.parameters WHERE param_name = 'temperature, air' LIMIT 1"
        )$parameter_id,
        error = function(e) integer()
      )
      if (!length(matched)) {
        matched <- moduleData$parameters$parameter_id[
          as.character(moduleData$parameters$param_name) == "temperature, air"
        ]
      }
      if (length(matched)) {
        matched[[1]]
      } else {
        moduleData$parameters$parameter_id[[1]]
      }
    }

    # Create the filter inputs ############################################################################
    map_params <- reactiveValues(
      param1 = default_primary_parameter, # Temperature, air
      param2 = 1165, # Water level
      yrs1 = 10,
      yrs2 = 10,
      days1 = 1,
      days2 = 1,
      latest = FALSE,
      target = Sys.Date(),
      # Target hour, used instead of 'target' on the hourly (24/48 h) timelines
      target_time = local({
        now <- as.POSIXct(trunc(Sys.time(), "hours"))
        attr(now, "tzone") <- "UTC"
        now
      }),
      stats_period = "30yr",
      colour_lower = 0,
      colour_upper = 100,
      colour_palette = "default",
      colour_lower2 = 0,
      colour_upper2 = 100,
      colour_palette2 = "default",
      reverse_colormap2 = FALSE,
      timeline_days = 30,
      precip_hours1 = 24,
      precip_hours2 = 24,
      # Which daily statistic to map, for parameters stored as several
      # aggregations (e.g. air temperature min, max and (min+max)/2)
      agg1 = "average",
      agg2 = "average",
      # Colour limits used for 'actual values' maps; NULL scales to the data.
      actual_limits1 = NULL,
      actual_limits2 = NULL,
      params = 1,
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

    is_precipitation <- function(parameter_id) {
      startsWith(parameter_internal_name(parameter_id), "precipitation")
    }

    precip_hour_choices <- c(
      "1 hour" = 1,
      "3 hours" = 3,
      "6 hours" = 6,
      "12 hours" = 12,
      "24 hours" = 24,
      "48 hours" = 48,
      "72 hours" = 72,
      "5 days" = 120,
      "7 days" = 168
    )

    # Which parameter (1 or 2) the open parameter modal is editing
    editing_param <- reactiveVal(1)

    # Aggregation choices. 'average' uses a station's instantaneous or mean
    # series, falling back to (min+max)/2, and never its minimum or maximum.
    # Ids are continuous.aggregation_types.
    aggregation_choices <- c(
      "Average" = "average",
      "Meteorological (min+max)/2" = "meteorological",
      "Minimum" = "minimum",
      "Maximum" = "maximum"
    )
    aggregation_type_ids <- c(meteorological = 6L, minimum = 4L, maximum = 5L)

    # Aggregation type ids of each timeseries (NA if not recorded)
    timeseries_aggregation <- function(index = TRUE) {
      ids <- moduleData$timeseries$aggregation_type_id
      if (is.null(ids)) {
        return(rep(NA_integer_, nrow(moduleData$timeseries))[index])
      }
      as.integer(ids)[index]
    }

    # The aggregation choices a parameter's timeseries offer; only worth
    # showing when it has a minimum, maximum or (min+max)/2 series.
    parameter_aggregation_choices <- function(parameter_id) {
      matches <- as.character(moduleData$timeseries$parameter_id) ==
        as.character(parameter_id)
      ids <- timeseries_aggregation(matches)
      if (!any(ids %in% aggregation_type_ids)) {
        return(character())
      }
      available <- c(
        average = any(!ids %in% aggregation_type_ids[c("minimum", "maximum")]),
        vapply(aggregation_type_ids, function(id) any(ids %in% id), logical(1))
      )
      aggregation_choices[aggregation_choices %in% names(available)[available]]
    }

    # Which of `ids` (aggregation type ids) fit the chosen aggregation
    aggregation_allowed <- function(ids, choice) {
      if (identical(choice, "average") || !choice %in% names(aggregation_type_ids)) {
        !ids %in% aggregation_type_ids[c("minimum", "maximum")]
      } else {
        ids %in% aggregation_type_ids[[choice]]
      }
    }

    # " (minimum)" etc. to add to labels; nothing for the default average
    aggregation_suffix <- function(choice) {
      if (identical(choice, "average") || !choice %in% aggregation_choices) {
        return("")
      }
      paste0(" (", tolower(names(aggregation_choices)[aggregation_choices == choice]), ")")
    }

    # Aggregation choice, shown in the parameter modal when it applies
    output$aggregation_ui <- renderUI({
      req(input$param)
      choices <- parameter_aggregation_choices(input$param)
      if (!length(choices)) {
        return(NULL)
      }
      current <- isolate(map_params[[paste0("agg", editing_param())]])
      selectizeInput(
        ns("aggregation"),
        label = "Aggregation",
        choices = choices,
        selected = if (current %in% choices) current else choices[[1]],
        multiple = FALSE
      )
    })

    # The aggregation to save for `parameter_id` from the modal's inputs
    saved_aggregation <- function(parameter_id) {
      choices <- parameter_aggregation_choices(parameter_id)
      if (length(choices) && isTRUE(input$aggregation %in% choices)) {
        input$aggregation
      } else if (length(choices)) {
        choices[[1]]
      } else {
        "average"
      }
    }

    # Aggregation length, shown in the parameter modal only for precipitation
    output$precip_hours_ui <- renderUI({
      req(input$param)
      if (!is_precipitation(input$param)) {
        return(NULL)
      }
      selectizeInput(
        ns("precip_hours"),
        label = "Accumulation",
        choices = precip_hour_choices,
        selected = isolate(map_params[[paste0("precip_hours", editing_param())]]),
        multiple = FALSE
      )
    })

    # Values last pushed to each colour-limit slider by the server. The slider
    # echoes these back as input changes, which must not count as user edits.
    slider_echo <- new.env()
    is_slider_echo <- function(id, limits) {
      echo <- slider_echo[[id]]
      !is.null(echo) && isTRUE(all.equal(as.numeric(echo), limits))
    }

    # Step size for an 'actual values' colour slider spanning `span` units
    actual_slider_step <- function(span) {
      if (!is.finite(span) || span <= 0) {
        return(1)
      }
      10^floor(log10(span / 100))
    }

    # Point a colour-limit slider at the range of the mapped actual values
    sync_actual_slider <- function(id, legend) {
      vals <- legend$values[is.finite(legend$values)]
      bounds <- range(c(vals, legend$limits))
      span <- diff(bounds)
      if (span == 0) {
        span <- max(abs(bounds), 1)
      }
      # Same step as group_legend() used to round the automatic limits
      step <- actual_slider_step(
        if (length(vals) > 1L) diff(range(vals)) else span
      )
      pad <- span * 0.25
      slider_echo[[id]] <- legend$limits
      updateSliderInput(
        session,
        id,
        min = floor((bounds[[1]] - pad) / step) * step,
        max = ceiling((bounds[[2]] + pad) / step) * step,
        value = legend$limits,
        step = step
      )
    }

    # Put a colour-limit slider back on the relative (percent) scale
    reset_range_slider <- function(id, lower, upper) {
      slider_echo[[id]] <- c(lower, upper)
      updateSliderInput(
        session,
        id,
        min = -100,
        max = 200,
        value = c(lower, upper),
        step = 1
      )
    }

    observeEvent(
      input$mapType,
      {
        if (input$mapType != "actual") {
          reset_range_slider(
            "colour_limits",
            map_params$colour_lower,
            map_params$colour_upper
          )
          if (map_params$params == 2) {
            reset_range_slider(
              "secondary_colour_limits",
              map_params$colour_lower2,
              map_params$colour_upper2
            )
          }
        }
        # In 'actual' mode updateMap() sets the sliders once it has the data.
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$colour_limits,
      {
        limits <- suppressWarnings(as.numeric(input$colour_limits))
        if (length(limits) != 2L || !all(is.finite(limits))) {
          return()
        }
        if (is_slider_echo("colour_limits", limits)) {
          return()
        }
        if (identical(input$mapType, "actual")) {
          map_params$actual_limits1 <- limits
        } else {
          map_params$colour_lower <- limits[[1]]
          map_params$colour_upper <- limits[[2]]
        }
      },
      ignoreInit = TRUE
    )
    palette_colours <- list(
      default = c(
        "#8c510a",
        "#d8b365",
        "#FEE090",
        "#74ADD1",
        "#4575D2",
        "#313695",
        "#A50026"
      ),
      turbo = c(
        "#30123b",
        "#4145ab",
        "#2a8bd2",
        "#29c7ac",
        "#7ad151",
        "#f9e721",
        "#a61c2b"
      ),
      blues = c(
        "#f7fbff",
        "#deebf7",
        "#c6dbef",
        "#9ecae1",
        "#6baed6",
        "#3182bd",
        "#08519c"
      ),
      viridis = c(
        "#440154",
        "#443983",
        "#31688e",
        "#21918c",
        "#35b779",
        "#90d743",
        "#fde725"
      ),
      plasma = c(
        "#0d0887",
        "#5b02a3",
        "#9a179b",
        "#cb4679",
        "#ed7953",
        "#fdb32e",
        "#f0f921"
      ),
      spectral = c(
        "#9e0142",
        "#d53e4f",
        "#f46d43",
        "#fee08b",
        "#e6f598",
        "#66c2a5",
        "#3288bd"
      ),
      ylgnbu = c(
        "#ffffd9",
        "#edf8b1",
        "#c7e9b4",
        "#7fcdbb",
        "#41b6c4",
        "#1d91c0",
        "#225ea8"
      )
    )
    apply_palette <- function() {
      selected <- map_params$colour_palette
      if (selected %in% names(palette_colours)) {
        colours <- palette_colours[[selected]]
        if (isTRUE(input$reverse_colormap)) {
          colours <- rev(colours)
        }
        map_params$colors <- colours
      }
    }
    observeEvent(
      input$colour_palette,
      {
        selected <- input$colour_palette
        if (length(selected) == 1L && selected %in% names(palette_colours)) {
          map_params$colour_palette <- selected
          apply_palette()
        }
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$secondary_colour_palette,
      {
        selected <- input$secondary_colour_palette
        if (length(selected) == 1L && selected %in% names(palette_colours)) {
          map_params$colour_palette2 <- selected
        }
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$secondary_colour_limits,
      {
        limits <- suppressWarnings(as.numeric(input$secondary_colour_limits))
        if (length(limits) != 2L || !all(is.finite(limits))) {
          return()
        }
        if (is_slider_echo("secondary_colour_limits", limits)) {
          return()
        }
        if (identical(input$mapType, "actual")) {
          map_params$actual_limits2 <- limits
        } else {
          map_params$colour_lower2 <- limits[[1]]
          map_params$colour_upper2 <- limits[[2]]
        }
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$reverse_colormap,
      {
        apply_palette()
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$secondary_reverse_colormap,
      {
        map_params$reverse_colormap2 <- isTRUE(input$secondary_reverse_colormap)
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$show_value_annotations,
      {
        if (isTRUE(mapCreated())) updateMap()
      },
      ignoreInit = TRUE
    )

    # Observe 'map_latest_measurements'. If TRUE, 'target' is adjusted to Sys.Date()
    observeEvent(input$latest, {
      if (input$latest) {
        # Show the user a modal that explains that the most recent values will be compared against daily means
        showModal(modalDialog(
          HTML(tr("map_latest_measurements_modal", language$language)),
          easyClose = TRUE,
          footer = modalButton(tr("close", language$language))
        ))
        map_params$latest <- TRUE
        set_target_now()
        if (isTRUE(mapCreated())) updateMap()
      } else {
        # The target already tracks the date input and slider, so keep it.
        updateCheckboxInput(session, "auto_update_latest", value = FALSE)
        map_params$latest <- FALSE
      }
    })

    # edit primary and secondary parameter options in a modal
    observeEvent(input$edit_primary_param, {
      # Order the parameters alphabetically by name (in the current language)
      tmp <- moduleData$parameters[
        order(moduleData$parameters[[tr("param_name_col", language$language)]]),
      ]

      editing_param(1)
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
        uiOutput(ns("aggregation_ui")),
        uiOutput(ns("precip_hours_ui")),
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
        ),
        numericInput(
          ns("days"),
          label = tooltip(
            trigger = list(
              tr("map_date_within", language$language),
              bsicons::bs_icon("info-circle-fill")
            ),
            tr("tooltip_params_map_date_within", language$language)
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
      if (!identical(as.character(map_params$param1), input$param)) {
        map_params$actual_limits1 <- NULL # rescale to the new parameter
      }
      if (is_precipitation(input$param) && !is.null(input$precip_hours)) {
        map_params$precip_hours1 <- as.numeric(input$precip_hours)
      }
      map_params$agg1 <- saved_aggregation(input$param)
      map_params$param1 <- input$param
      map_params$yrs1 <- input$yrs
      map_params$days1 <- input$days
      removeModal()
    })

    observeEvent(input$edit_secondary_param, {
      # Order the parameters alphabetically by name (in the current language)
      tmp <- moduleData$parameters[
        order(moduleData$parameters[[tr("param_name_col", language$language)]]),
      ]

      editing_param(2)
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
        uiOutput(ns("aggregation_ui")),
        uiOutput(ns("precip_hours_ui")),
        numericInput(
          ns("yrs"),
          label = tr("map_min_yrs", language$language),
          value = map_params$yrs2,
          min = 3,
          max = 100,
          step = 1
        ),
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
      if (map_params$params == 1) {
        map_params$params <- 2
      }
      updateActionButton(
        session,
        "edit_secondary_param",
        tr("map_edit_second_param", language$language)
      )
      if (!identical(as.character(map_params$param2), input$param)) {
        map_params$actual_limits2 <- NULL # rescale to the new parameter
      }
      if (is_precipitation(input$param) && !is.null(input$precip_hours)) {
        map_params$precip_hours2 <- as.numeric(input$precip_hours)
      }
      map_params$agg2 <- saved_aggregation(input$param)
      map_params$param2 <- input$param
      map_params$yrs2 <- input$yrs
      map_params$days2 <- input$days
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

    # The 24 and 48 hour timelines step through hours instead of days.
    # Hourly times are kept in UTC so combining them never mixes time zones.
    utc_time <- function(x) {
      x <- as.POSIXct(x)
      attr(x, "tzone") <- "UTC"
      x
    }
    current_hour <- function() {
      utc_time(trunc(Sys.time(), "hours"))
    }
    is_hourly_timeline <- function() {
      isTRUE(as.numeric(map_params$timeline_days) <= 2)
    }

    # Values last pushed to the date input and date slider by the server, so
    # their echoes can be told apart from the user picking a date or hour.
    is_echo <- function(id, value) {
      echo <- slider_echo[[id]]
      !is.null(echo) &&
        inherits(value, class(echo)[[1]]) &&
        isTRUE(as.numeric(value) == as.numeric(echo))
    }
    push_date_input <- function(date) {
      slider_echo[["target"]] <- as.Date(date)
      updateDateInput(session, "target", value = date)
    }
    # Set the date slider to `value`. With `recenter = TRUE` (or when `value`
    # falls outside the current range) the timeline is re-placed so `value`
    # sits 80% of the way along it: 80% of the timeline before it and 20%
    # after, cut off at the current hour (hourly) or today (daily) so no
    # future times are shown. Otherwise the current range is kept.
    push_date_slider <- function(value, recenter = FALSE) {
      days <- as.numeric(isolate(map_params$timeline_days))
      window <- slider_echo[["date_slider_window"]]
      if (isolate(is_hourly_timeline())) {
        now <- current_hour()
        value <- min(utc_time(value), now)
        span <- days * 24 * 3600 # seconds
        hour <- function(x) utc_time(round(as.numeric(x) / 3600) * 3600)
        new_window <- c(hour(value - 0.8 * span), min(hour(value + 0.2 * span), now))
        step <- 3600 * 1000 # milliseconds
        time_format <- "%Y-%m-%d %H:%M"
      } else {
        value <- min(as.Date(value), Sys.Date())
        new_window <- c(
          value - round(0.8 * days),
          min(value + round(0.2 * days), Sys.Date())
        )
        step <- 24 * 3600 * 1000 # milliseconds
        time_format <- "%Y-%m-%d"
      }
      if (
        recenter ||
          !inherits(window, class(value)[[1]]) ||
          value < window[[1]] ||
          value > window[[2]]
      ) {
        window <- new_window
        slider_echo[["date_slider_window"]] <- window
      }
      slider_echo[["map_date_slider"]] <- value
      updateSliderInput(
        session,
        "map_date_slider",
        min = window[[1]],
        max = window[[2]],
        value = value,
        step = step,
        timeFormat = time_format
      )
      value
    }

    # Picking a date or hour yourself means you no longer want the latest
    # measurements, so stop following them.
    stop_following_latest <- function() {
      if (isTRUE(map_params$latest)) {
        map_params$latest <- FALSE
        updateCheckboxInput(session, "latest", value = FALSE)
      }
    }

    # The selected hour of day moved onto `date`, but never past this hour
    target_hour_on <- function(date) {
      date <- as.Date(date)
      target_time <- isolate(map_params$target_time)
      if (identical(as.Date(target_time, tz = Sys.timezone()), date)) {
        return(min(target_time, current_hour()))
      }
      hour_of_day <- as.numeric(format(target_time, "%H", tz = Sys.timezone()))
      min(
        utc_time(
          as.POSIXct(as.character(date), tz = Sys.timezone()) +
            hour_of_day * 3600
        ),
        current_hour()
      )
    }

    # Move the target (and the date and slider inputs) to today / this hour
    set_target_now <- function() {
      map_params$target <- Sys.Date()
      map_params$target_time <- current_hour()
      push_date_input(Sys.Date())
      push_date_slider(
        if (isolate(is_hourly_timeline())) current_hour() else Sys.Date(),
        recenter = TRUE
      )
    }

    observeEvent(
      input$target,
      {
        req(input$target)
        selected_date <- as.Date(input$target)
        # A date the user picked (not one the slider pushed here) re-places
        # the timeline around it.
        picked_by_user <- !is_echo("target", selected_date)
        if (picked_by_user) {
          stop_following_latest()
        }
        if (is_hourly_timeline()) {
          if (
            identical(
              as.Date(map_params$target_time, tz = Sys.timezone()),
              selected_date
            )
          ) {
            return()
          }
          target_time <- target_hour_on(selected_date)
          map_params$target <- selected_date
          map_params$target_time <- push_date_slider(
            target_time,
            recenter = picked_by_user
          )
          return()
        }
        map_params$target <- selected_date
        if (
          picked_by_user ||
            is.null(input$map_date_slider) ||
            as.Date(input$map_date_slider) != selected_date
        ) {
          push_date_slider(selected_date, recenter = picked_by_user)
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$map_date_slider,
      {
        req(input$map_date_slider)
        if (is_hourly_timeline()) {
          # A daily value can arrive while the slider switches to hourly
          if (!inherits(input$map_date_slider, "POSIXct")) {
            return()
          }
          target_time <- utc_time(input$map_date_slider)
          if (!is_echo("map_date_slider", target_time)) {
            stop_following_latest()
          }
          map_params$target_time <- target_time
          selected_date <- as.Date(target_time, tz = Sys.timezone())
        } else {
          selected_date <- as.Date(input$map_date_slider)
          if (!is_echo("map_date_slider", selected_date)) {
            stop_following_latest()
          }
        }
        map_params$target <- selected_date
        if (is.null(input$target) || as.Date(input$target) != selected_date) {
          push_date_input(selected_date)
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$timeline_days,
      {
        timeline_days <- suppressWarnings(as.numeric(input$timeline_days))
        if (
          length(timeline_days) != 1L ||
            !is.finite(timeline_days) ||
            timeline_days <= 0
        ) {
          return()
        }

        map_params$timeline_days <- timeline_days
        if (timeline_days <= 2) {
          # Hourly timeline over the last 24 or 48 hours
          target_time <- if (map_params$latest) {
            current_hour()
          } else {
            target_hour_on(map_params$target)
          }
          target_time <- push_date_slider(target_time, recenter = TRUE)
          map_params$target_time <- target_time
          selected_date <- as.Date(target_time, tz = Sys.timezone())
        } else {
          selected_date <- as.Date(map_params$target)
          if (is.na(selected_date)) {
            selected_date <- Sys.Date()
          }
          selected_date <- push_date_slider(selected_date, recenter = TRUE)
        }
        if (!identical(as.Date(map_params$target), selected_date)) {
          map_params$target <- selected_date
          push_date_input(selected_date)
        }
      },
      ignoreInit = TRUE
    )

    # Listen for input changes and update the map ########################################################
    precipitation_aggregate <- function(timeseries_ids, target_end, hours) {
      if (!length(timeseries_ids)) {
        return(data.table::data.table())
      }
      dbGetQueryDT(
        session$userData$AquaCache,
        paste0(
          "WITH requested_timeseries(timeseries_id) AS (SELECT unnest(ARRAY[",
          paste(as.integer(timeseries_ids), collapse = ","),
          "]::integer[])) ",
          "SELECT r.timeseries_id, max(m.datetime) AS datetime, sum(m.value_corrected) AS value ",
          "FROM requested_timeseries r JOIN LATERAL continuous.measurements_continuous_corrected(",
          "r.timeseries_id, '",
          format(
            target_end - as.numeric(hours) * 3600,
            tz = "UTC",
            usetz = TRUE
          ),
          "'::timestamptz, '",
          format(target_end, tz = "UTC", usetz = TRUE),
          "'::timestamptz) m ON TRUE GROUP BY r.timeseries_id;"
        )
      )
    }

    updateMap <- function() {
      map_type <- req(input$mapType)
      stats_select <- params_map_daily_stats_select_sql(
        session$userData$AquaCache,
        stats_period = map_params$stats_period
      )
      stats_filter_count <- attr(stats_select, "count_column")

      # Inputs can briefly be NULL while a parameter dialog is being saved.
      # Only update once every required value is a usable scalar.
      valid_number <- function(x) {
        length(x) == 1L && is.finite(suppressWarnings(as.numeric(x)))
      }
      has_second_parameter <- isTRUE(map_params$params == 2)
      if (!valid_number(map_params$yrs1) || !valid_number(map_params$days1)) {
        return()
      }
      if (
        has_second_parameter &&
          (!valid_number(map_params$yrs2) || !valid_number(map_params$days2))
      ) {
        return()
      }

      # Stop if a parameter is temporarily absent or does not exist.
      if (!isTRUE(map_params$param1 %in% moduleData$parameters$parameter_id)) {
        return()
      }
      if (
        has_second_parameter &&
          !isTRUE(map_params$param2 %in% moduleData$parameters$parameter_id)
      ) {
        return()
      }

      # Deal with parameter 1 (only its timeseries for the chosen aggregation)
      tsids1 <- moduleData$timeseries$timeseries_id[
        moduleData$timeseries$parameter_id == map_params$param1 &
          aggregation_allowed(timeseries_aggregation(), map_params$agg1)
      ]
      if (length(tsids1) == 0) {
        return()
      }

      # Daily timelines map daily means near the target date. 'Latest
      # measurements' and the hourly (24/48 h) timelines instead map the most
      # recent measurement at or before an end time: now, or the target hour.
      hourly <- is_hourly_timeline() && !isTRUE(map_params$latest)
      point_in_time <- isTRUE(map_params$latest) || hourly
      point_end <- if (hourly) map_params$target_time else Sys.time()

      point_measurements <- function(timeseries_ids, days, yrs) {
        end_sql <- if (hourly) {
          paste0("'", format(point_end, tz = "UTC", usetz = TRUE), "'")
        } else {
          "NULL"
        }
        measurements <- dbGetQueryDT(
          session$userData$AquaCache,
          paste0(
            "WITH requested_timeseries(timeseries_id) AS (
              SELECT unnest(ARRAY[",
            paste(timeseries_ids, collapse = ","),
            "]::integer[])
            )
            SELECT m.timeseries_id, m.datetime, m.value
            FROM requested_timeseries r
            JOIN LATERAL (
              SELECT timeseries_id, datetime, value_corrected AS value
              FROM continuous.measurements_continuous_corrected(
                r.timeseries_id,
                '",
            format(
              point_end - as.numeric(days) * 24 * 60 * 60,
              tz = "UTC",
              usetz = TRUE
            ),
            "'::timestamptz,
                ",
            end_sql,
            "::timestamptz
              )
              ORDER BY datetime DESC
              LIMIT 1
            ) m ON TRUE;"
          )
        )
        if (nrow(measurements) == 0) {
          return(measurements)
        }

        # Historic range for the most recent day at or before the end time
        range <- dbGetQueryDT(
          session$userData$AquaCache,
          paste0(
            "WITH ranked_data AS (
              SELECT timeseries_id, ",
            paste(stats_select[c("max", "min", "doy_count")], collapse = ", "),
            ", ROW_NUMBER() OVER (PARTITION BY timeseries_id ORDER BY date DESC) AS row_num
              FROM continuous.measurements_calculated_daily
              WHERE ",
            stats_filter_count,
            " >= ",
            as.numeric(yrs),
            " AND date <= '",
            as.Date(point_end, tz = Sys.timezone()),
            "' AND timeseries_id IN (",
            paste(measurements$timeseries_id, collapse = ","),
            ")
            )
            SELECT timeseries_id, max, min, doy_count
            FROM ranked_data
            WHERE row_num = 1;"
          )
        )

        # Merge on timeseries_id to add the historic range (this drops
        # timeseries without enough years of record)
        measurements <- merge(
          measurements,
          range,
          by = "timeseries_id",
          all.y = TRUE
        )
        measurements[,
          percent_historic_range := 100 * (value - min) / (max - min)
        ]
        measurements
      }

      # Precipitation totals over the accumulation window ending at the end
      # time above, or at the end of the target date. There is no historic
      # range for these.
      precipitation_measurements <- function(timeseries_ids, hours) {
        precipitation_end <- if (point_in_time) {
          point_end
        } else {
          as.POSIXct(
            as.character(as.Date(map_params$target) + 1),
            tz = Sys.timezone()
          )
        }
        out <- precipitation_aggregate(timeseries_ids, precipitation_end, hours)
        if (!nrow(out)) {
          return(out)
        }
        out[, `:=`(
          max = NA_real_,
          min = NA_real_,
          doy_count = NA_real_,
          percent_historic_range = NA_real_
        )]
        out
      }

      if (is_precipitation(map_params$param1)) {
        closest_measurements1 <- precipitation_measurements(
          tsids1,
          map_params$precip_hours1
        )
        if (nrow(closest_measurements1) == 0) {
          leaflet::leafletProxy("map", session) %>%
            leaflet::clearMarkers() %>%
            leaflet::clearShapes() %>%
            leaflet::clearControls()
          return()
        }
      } else if (point_in_time) {
        closest_measurements1 <- point_measurements(
          tsids1,
          map_params$days1,
          map_params$yrs1
        )

        # IF there are no closest measurements, stop and show a notification
        if (nrow(closest_measurements1) == 0) {
          leaflet::leafletProxy("map", session) %>%
            leaflet::clearMarkers() %>%
            leaflet::clearShapes() %>%
            leaflet::clearControls()
          return()
        }
      } else {
        # not requesting latest measurements
        range1 <- dbGetQueryDT(
          session$userData$AquaCache,
          paste0(
            "SELECT ",
            paste(
              c("timeseries_id", "date", "value", stats_select),
              collapse = ", "
            ),
            " FROM continuous.measurements_calculated_daily WHERE ",
            stats_filter_count,
            " >= ",
            as.numeric(map_params$yrs1),
            " AND timeseries_id IN (",
            paste(tsids1, collapse = ","),
            ") AND date BETWEEN '",
            map_params$target - as.numeric(map_params$days1),
            "' AND '",
            map_params$target + as.numeric(map_params$days1),
            "';"
          )
        )

        # Calculate the absolute difference in days between each date and the target date
        range1[, date_diff := abs(date - map_params$target)]

        # Order the data by 'timeseries_id' and 'date_diff'
        data.table::setorder(range1, timeseries_id, date_diff)

        # For each 'timeseries_id', select the row with the smallest 'date_diff'
        closest_measurements1 <- range1[, .SD[1], by = timeseries_id]
      }

      locs_tsids1 <- merge(
        moduleData$locations[, c(
          "latitude",
          "longitude",
          "location_id",
          "name",
          "name_fr"
        )],
        moduleData$timeseries[
          moduleData$timeseries$timeseries_id %in%
            closest_measurements1$timeseries_id,
          c("timeseries_id", "location_id", "units", "matrix_state")
        ],
        by = "location_id"
      )

      locs_tsids1$param_name <- paste0(
        parameter_label(map_params$param1),
        aggregation_suffix(map_params$agg1)
      )
      locs_tsids1$param_unit <- locs_tsids1$units

      # Now if the user has selected two parameters, repeat the process for the
      # second parameter. Stations that have both are shown as a cluster.
      if (map_params$params == 2) {
        tsids2 <- moduleData$timeseries$timeseries_id[
          moduleData$timeseries$parameter_id == map_params$param2 &
            aggregation_allowed(timeseries_aggregation(), map_params$agg2)
        ]
        if (length(tsids2) == 0) {
          # A secondary parameter with no eligible locations must not stop the
          # primary map from updating or leave its controls in a stale state.
          locs_tsids <- locs_tsids1
          closest_measurements <- closest_measurements1
        } else {
          if (is_precipitation(map_params$param2)) {
            closest_measurements2 <- precipitation_measurements(
              tsids2,
              map_params$precip_hours2
            )
          } else if (point_in_time) {
            closest_measurements2 <- point_measurements(
              tsids2,
              map_params$days2,
              map_params$yrs2
            )
          } else {
            range2 <- dbGetQueryDT(
              session$userData$AquaCache,
              paste0(
                "SELECT ",
                paste(
                  c("timeseries_id", "date", "value", stats_select),
                  collapse = ", "
                ),
                " FROM continuous.measurements_calculated_daily WHERE ",
                stats_filter_count,
                " >= ",
                as.numeric(map_params$yrs2),
                " AND timeseries_id IN (",
                paste(tsids2, collapse = ","),
                ") AND date BETWEEN '",
                map_params$target - as.numeric(map_params$days2),
                "' AND '",
                map_params$target + as.numeric(map_params$days2),
                "';"
              )
            )

            # Calculate the absolute difference in days between each date and the target date
            range2[, date_diff := abs(date - map_params$target)]

            # Order the data by 'timeseries_id' and 'date_diff'
            data.table::setorder(range2, timeseries_id, date_diff)

            # For each 'timeseries_id', select the row with the smallest 'date_diff'
            closest_measurements2 <- range2[, .SD[1], by = timeseries_id]
          }

          locs_tsids2 <- merge(
            moduleData$locations[, c(
              "latitude",
              "longitude",
              "location_id",
              "name",
              "name_fr"
            )],
            moduleData$timeseries[
              moduleData$timeseries$timeseries_id %in%
                closest_measurements2$timeseries_id,
              c("timeseries_id", "location_id", "units", "matrix_state")
            ],
            by = "location_id"
          )

          locs_tsids2$param_name <- paste0(
            parameter_label(map_params$param2),
            aggregation_suffix(map_params$agg2)
          )
          locs_tsids2$param_unit <- locs_tsids2$units

          # Merge the two sets of locations and timeseries IDs
          locs_tsids <- rbind(locs_tsids1, locs_tsids2)
          # The two sets can have different columns (e.g. precipitation
          # totals vs daily means), so match by name and fill the gaps.
          closest_measurements <- rbind(
            closest_measurements1,
            closest_measurements2,
            use.names = TRUE,
            fill = TRUE
          )
        }
      } else {
        locs_tsids <- locs_tsids1
        closest_measurements <- closest_measurements1
      }

      if (
        isTRUE(map_params$latest) && "datetime" %in% names(closest_measurements)
      ) {
        times <- suppressWarnings(as.POSIXct(
          closest_measurements$datetime,
          tz = "UTC"
        ))
        times <- times[is.finite(times)]
        if (length(times)) {
          # Use the local date and never go past today: the date inputs are
          # capped at Sys.Date(), and a UTC date of 'tomorrow' would be pushed
          # back by them, re-triggering this update in an endless loop.
          latest_date <- min(as.Date(max(times), tz = Sys.timezone()), Sys.Date())
          # isolate() so writing the target here doesn't re-trigger updateMap.
          if (isolate(is_hourly_timeline())) {
            latest_time <- min(
              utc_time(trunc(max(times), "hours")),
              current_hour()
            )
            if (
              !isTRUE(
                as.numeric(isolate(map_params$target_time)) ==
                  as.numeric(latest_time)
              )
            ) {
              map_params$target_time <- push_date_slider(latest_time)
            }
          }
          if (!identical(as.Date(isolate(map_params$target)), latest_date)) {
            map_params$target <- latest_date
            push_date_input(latest_date)
            if (!isolate(is_hourly_timeline())) {
              push_date_slider(latest_date)
            }
          }
        }
      }

      mapping_data <- merge(
        closest_measurements,
        locs_tsids,
        by = "timeseries_id",
        all.x = TRUE
      )
      mapping_data[, percent_historic_range_capped := percent_historic_range]
      mapping_data$is_secondary <- FALSE
      if (
        isTRUE(map_params$params == 2) && exists("tsids2", inherits = FALSE)
      ) {
        mapping_data$is_secondary <- mapping_data$timeseries_id %in% tsids2
      }

      # A station can have several timeseries for one parameter (e.g. different
      # sensor heights, sources or aggregation types). Show one per station and
      # parameter: one with a value to show, then by aggregation (instantaneous
      # or mean before (min+max)/2, for 'Average'), then the longest record,
      # then the most recently updated.
      ts_index <- match(
        mapping_data$timeseries_id,
        moduleData$timeseries$timeseries_id
      )
      aggregation_rank <- match(
        timeseries_aggregation(ts_index),
        c(1L, 2L, 6L, 3L, 7L) # instantaneous, mean, (min+max)/2, median, sum
      )
      ts_time <- function(column) {
        values <- moduleData$timeseries[[column]]
        if (is.null(values)) {
          return(rep(NA_real_, length(ts_index)))
        }
        suppressWarnings(as.numeric(as.POSIXct(values[ts_index])))
      }
      ts_start <- ts_time("start_datetime")
      ts_end <- ts_time("end_datetime")
      shown_value <- suppressWarnings(as.numeric(unlist(
        if (map_type == "actual") {
          mapping_data$value
        } else {
          mapping_data$percent_historic_range
        }
      )))
      mapping_data <- mapping_data[order(
        mapping_data$location_id,
        mapping_data$is_secondary,
        !is.finite(shown_value),
        aggregation_rank,
        -(ts_end - ts_start),
        -ts_end,
        mapping_data$timeseries_id,
        na.last = TRUE
      )]
      duplicate_station <- duplicated(
        mapping_data[, c("location_id", "is_secondary")]
      ) &
        !is.na(mapping_data$location_id)
      mapping_data <- mapping_data[!duplicate_station]

      # Secondary parameter colours (reverse flag applied here; the primary
      # colours are kept in map_params$colors by apply_palette()).
      secondary_colours <- palette_colours[[
        if (map_params$colour_palette2 %in% names(palette_colours)) {
          map_params$colour_palette2
        } else {
          "default"
        }
      ]]
      if (isTRUE(map_params$reverse_colormap2)) {
        secondary_colours <- rev(secondary_colours)
      }

      legend_digits <- function(vals) {
        if (length(vals) == 0 || all(!is.finite(vals))) {
          return(0)
        }
        max_val <- max(abs(vals), na.rm = TRUE)
        if (max_val >= 100) {
          0
        } else if (max_val >= 10) {
          1
        } else {
          2
        }
      }

      # Build the palette, legend title and label format for one parameter's
      # rows, using that parameter's own colours and colour limits. Actual
      # values use `actual_limits`, or the range of the data when NULL.
      group_legend <- function(
        rows,
        parameter_id,
        colours,
        lower,
        upper,
        actual_limits,
        label_suffix = ""
      ) {
        vals <- if (map_type == "actual") {
          rows$value
        } else {
          rows$percent_historic_range_capped
        }
        vals <- suppressWarnings(as.numeric(unlist(vals)))
        if (map_type == "actual") {
          finite_vals <- vals[is.finite(vals)]
          if (length(actual_limits) == 2L) {
            lower <- actual_limits[[1]]
            upper <- actual_limits[[2]]
          } else if (length(finite_vals)) {
            # Round outwards to the slider step so the slider can show them.
            step <- actual_slider_step(diff(range(finite_vals)))
            lower <- floor(min(finite_vals) / step) * step
            upper <- ceiling(max(finite_vals) / step) * step
          }
        }
        lower_limit <- min(lower, upper)
        upper_limit <- max(lower, upper)
        if (identical(lower_limit, upper_limit)) {
          upper_limit <- lower_limit + 1
        }
        # Keep values outside the selected range in the end colours.
        bins <- c(-Inf, seq(lower_limit, upper_limit, length.out = 6), Inf)
        pal <- leaflet::colorBin(
          palette = colours,
          domain = vals,
          bins = bins,
          pretty = FALSE,
          na.color = "#808080"
        )
        label <- paste0(parameter_label(parameter_id), label_suffix)
        if (map_type == "actual") {
          units <- unique(na.omit(rows$param_unit))
          units <- units[nzchar(units)]
          title <- if (length(units)) {
            sprintf("%s (%s)", label, paste(units, collapse = ", "))
          } else {
            label
          }
          lab_format <- leaflet::labelFormat(digits = legend_digits(vals))
        } else {
          title <- paste0(
            htmltools::htmlEscape(label),
            "<br>",
            tr("map_relative", language$language)
          )
          lab_format <- leaflet::labelFormat(digits = 0, suffix = "%")
        }
        list(
          pal = pal,
          values = vals,
          title = title,
          lab_format = lab_format,
          limits = c(lower_limit, upper_limit)
        )
      }

      # Leaflet requires atomic numeric vectors. Some database backends return
      # list columns after the map-data merge, so normalize them before drawing.
      for (column in c(
        "longitude",
        "latitude",
        "value",
        "percent_historic_range_capped"
      )) {
        if (column %in% names(mapping_data)) {
          mapping_data[[
            column
          ]] <- suppressWarnings(as.numeric(unlist(mapping_data[[column]])))
        }
      }
      mapping_data <- mapping_data[
        is.finite(mapping_data$longitude) & is.finite(mapping_data$latitude),
        ,
        drop = FALSE
      ]
      if (!nrow(mapping_data)) {
        leaflet::leafletProxy("map", session) %>%
          leaflet::clearMarkers() %>%
          leaflet::clearShapes() %>%
          leaflet::clearControls()
        return()
      }

      mapping_data$map_annotation <- if (isTRUE(input$show_value_annotations)) {
        as.character(round(mapping_data$value, 2))
      } else {
        NA_character_
      }
      is_secondary_row <- mapping_data$is_secondary
      primary_data <- mapping_data[which(!is_secondary_row), , drop = FALSE]
      secondary_data <- mapping_data[which(is_secondary_row), , drop = FALSE]
      # Each parameter gets its own palette, reverse flag and colour limits.
      primary_legend <- group_legend(
        primary_data,
        map_params$param1,
        map_params$colors,
        map_params$colour_lower,
        map_params$colour_upper,
        map_params$actual_limits1,
        label_suffix = aggregation_suffix(map_params$agg1)
      )
      secondary_legend <- if (nrow(secondary_data)) {
        group_legend(
          secondary_data,
          map_params$param2,
          secondary_colours,
          map_params$colour_lower2,
          map_params$colour_upper2,
          map_params$actual_limits2,
          label_suffix = aggregation_suffix(map_params$agg2)
        )
      }
      if (map_type == "actual") {
        sync_actual_slider("colour_limits", primary_legend)
        if (!is.null(secondary_legend)) {
          sync_actual_slider("secondary_colour_limits", secondary_legend)
        }
      }
      marker_lng <- as.vector(
        unlist(mapping_data$longitude, recursive = TRUE, use.names = FALSE),
        mode = "numeric"
      )
      marker_lat <- as.vector(
        unlist(mapping_data$latitude, recursive = TRUE, use.names = FALSE),
        mode = "numeric"
      )
      marker_colours <- character(nrow(mapping_data))
      marker_colours[which(!is_secondary_row)] <- as.character(unlist(
        primary_legend$pal(primary_legend$values),
        use.names = FALSE
      ))
      if (!is.null(secondary_legend)) {
        marker_colours[which(is_secondary_row)] <- as.character(unlist(
          secondary_legend$pal(secondary_legend$values),
          use.names = FALSE
        ))
      }
      marker_text <- function(column) {
        vapply(
          mapping_data[[column]],
          function(x) as.character(x)[[1]],
          character(1)
        )
      }
      marker_names <- marker_text(tr("generic_name_col", language$language))
      marker_parameters <- marker_text("param_name")
      marker_popup <- paste0(
        "<strong>",
        YGwater:::escape_html_text(marker_names),
        "</strong><br/>",
        YGwater:::escape_html_text(marker_parameters),
        "<br/>",
        tr("value", language$language),
        ": ",
        round(mapping_data$value, 2)
      )
      marker_popup <- as.character(unlist(
        marker_popup,
        recursive = TRUE,
        use.names = FALSE
      ))
      marker_labels <- as.character(unlist(
        mapping_data$map_annotation,
        recursive = TRUE,
        use.names = FALSE
      ))
      marker_data <- data.frame(
        timeseries_id = as.character(unlist(
          mapping_data$timeseries_id,
          use.names = FALSE
        )),
        longitude = marker_lng,
        latitude = marker_lat,
        colour = marker_colours,
        label = marker_labels,
        popup = marker_popup,
        stringsAsFactors = FALSE
      )
      # Popups (with the station's plot) are built when a marker is clicked
      popup_markers(marker_data)

      # Primary-parameter stations are circles, secondary ones squares. Both
      # go in one cluster group whose radius is so small that only markers
      # drawn on top of each other (e.g. a station measuring both parameters)
      # are grouped; clicking a group fans its markers out.
      circle_data <- marker_data[which(!is_secondary_row), , drop = FALSE]
      square_data <- marker_data[which(is_secondary_row), , drop = FALSE]
      cluster_options <- leaflet::markerClusterOptions(
        maxClusterRadius = 4,
        spiderfyOnMaxZoom = TRUE,
        showCoverageOnHover = FALSE,
        zoomToBoundsOnClick = TRUE,
        # The group icon is split into its members' colours, with a count
        iconCreateFunction = htmlwidgets::JS(
          "function(cluster) {
            var colours = cluster.getAllChildMarkers().map(function(m) {
              if (m.options.fillColor) return m.options.fillColor;
              var url = m.options.icon && m.options.icon.options.iconUrl;
              var match = url && /fill=\"(#[0-9A-Fa-f]{3,8})\"/.exec(decodeURIComponent(url));
              return match ? match[1] : '#808080';
            });
            var n = colours.length;
            var stops = colours.map(function(c, i) {
              return c + ' ' + (100 * i / n) + '% ' + (100 * (i + 1) / n) + '%';
            }).join(', ');
            return L.divIcon({
              html: '<div style=\"width: 22px; height: 22px; border-radius: 50%; border: 2px solid #222; box-sizing: border-box; background: conic-gradient(' + stops + '); display: flex; align-items: center; justify-content: center;\"><span style=\"font: bold 10px sans-serif; color: #fff; text-shadow: 0 0 2px #000;\">' + n + '</span></div>',
              className: 'map-colocated-cluster',
              iconSize: L.point(22, 22)
            });
          }"
        )
      )

      map_proxy <- leaflet::leafletProxy("map", session)
      # leafletProxy can retain a malformed bounds list from an earlier
      # data-table render. Reset it before adding the freshly atomic markers.
      map_proxy$x$limits <- NULL
      map_proxy <- map_proxy %>%
        leaflet::clearMarkers() %>%
        leaflet::clearMarkerClusters() %>%
        leaflet::clearShapes() %>%
        leaflet::clearControls() %>%
        leaflet::addMapPane("overlay", zIndex = 420) %>%
        leaflet::clearPopups()
      if (nrow(circle_data)) {
        map_proxy <- map_proxy %>% leaflet::addCircleMarkers(
          data = circle_data,
          layerId = ~timeseries_id,
          clusterOptions = cluster_options,
          clusterId = "stations",
          lng = ~longitude,
          lat = ~latitude,
          fillColor = ~colour,
          color = ~colour,
          fillOpacity = 1,
          stroke = TRUE,
          weight = 1,
          radius = 8,
          options = leaflet::pathOptions(pane = "overlay"),
          label = ~label,
          labelOptions = leaflet::labelOptions(
            noHide = isTRUE(input$show_value_annotations),
            direction = "top",
            offset = c(0, -10),
            textOnly = TRUE,
            className = "map-value-annotation"
          )
        )
      }
      map_proxy %>%
        leaflet::addLegend(
          position = "bottomright",
          pal = primary_legend$pal,
          values = primary_legend$values,
          title = primary_legend$title,
          labFormat = primary_legend$lab_format,
          opacity = 1,
          layerId = "primary_legend"
        )

      if (nrow(square_data)) {
        square_svg <- vapply(
          square_data$colour,
          function(colour) {
            paste0(
              "data:image/svg+xml,",
              utils::URLencode(
                sprintf(
                  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"16\" height=\"16\"><rect x=\"1\" y=\"1\" width=\"14\" height=\"14\" fill=\"%s\" stroke=\"#222\"/></svg>",
                  colour
                ),
                reserved = TRUE
              )
            )
          },
          character(1)
        )
        square_icons <- leaflet::icons(
          iconUrl = square_svg,
          iconWidth = 16,
          iconHeight = 16,
          iconAnchorX = 8,
          iconAnchorY = 8
        )
        leaflet::leafletProxy("map", session) %>%
          leaflet::addMarkers(
            data = square_data,
            layerId = ~timeseries_id,
            lng = ~longitude,
            lat = ~latitude,
            icon = square_icons,
            clusterOptions = cluster_options,
            clusterId = "stations",
            label = ~label,
            labelOptions = leaflet::labelOptions(
              noHide = isTRUE(input$show_value_annotations),
              direction = "top",
              offset = c(0, -10),
              textOnly = TRUE,
              className = "map-value-annotation"
            )
          ) %>%
          leaflet::addLegend(
            position = "bottomright",
            pal = secondary_legend$pal,
            values = secondary_legend$values,
            title = secondary_legend$title,
            labFormat = secondary_legend$lab_format,
            opacity = 1,
            layerId = "secondary_legend"
          )
      }
    }

    # Station popups with a plot ##############################################
    # Markers currently on the map (timeseries_id, position, popup text)
    popup_markers <- reactiveVal(NULL)

    # The timeline's current range, as Dates (daily) or UTC times (hourly)
    timeline_window <- function() {
      window <- slider_echo[["date_slider_window"]]
      hourly <- isolate(is_hourly_timeline())
      if (
        length(window) == 2L &&
          inherits(window, if (hourly) "POSIXct" else "Date")
      ) {
        return(window)
      }
      days <- as.numeric(isolate(map_params$timeline_days))
      if (hourly) {
        c(current_hour() - days * 24 * 3600, current_hour())
      } else {
        c(Sys.Date() - days, Sys.Date())
      }
    }

    # One station's values within `window`: daily means for daily timelines,
    # continuous measurements for the hourly ones.
    timeline_series <- function(timeseries_id, window) {
      timeseries_id <- as.integer(timeseries_id)
      if (inherits(window, "POSIXct")) {
        dbGetQueryDT(
          session$userData$AquaCache,
          paste0(
            "SELECT datetime AS x, value_corrected AS value
             FROM continuous.measurements_continuous_corrected(",
            timeseries_id,
            ", '",
            format(window[[1]], tz = "UTC", usetz = TRUE),
            "'::timestamptz, '",
            format(window[[2]], tz = "UTC", usetz = TRUE),
            "'::timestamptz) ORDER BY datetime;"
          )
        )
      } else {
        dbGetQueryDT(
          session$userData$AquaCache,
          paste0(
            "SELECT date AS x, value
             FROM continuous.measurements_calculated_daily
             WHERE timeseries_id = ",
            timeseries_id,
            " AND date BETWEEN '",
            window[[1]],
            "' AND '",
            window[[2]],
            "' ORDER BY date;"
          )
        )
      }
    }

    observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      markers <- popup_markers()
      if (is.null(click$id) || is.null(markers)) {
        return()
      }
      marker <- markers[markers$timeseries_id == as.character(click$id), ]
      if (!nrow(marker)) {
        return()
      }
      marker <- marker[1, ]

      window <- timeline_window()
      series <- tryCatch(
        timeline_series(marker$timeseries_id, window),
        error = function(e) NULL
      )
      hourly <- inherits(window, "POSIXct")
      label_format <- if (hourly) "%m-%d %H:%M" else "%Y-%m-%d"
      target <- if (hourly) map_params$target_time else map_params$target
      plot <- if (is.null(series)) {
        '<div style="color: #666; font-size: 0.85em; padding-top: 4px;">Could not load data for this station.</div>'
      } else {
        params_map_popup_plot(
          x = as.numeric(series$x),
          y = suppressWarnings(as.numeric(series$value)),
          x_range = as.numeric(window),
          x_labels = format(window, label_format, tz = Sys.timezone()),
          target = as.numeric(target)
        )
      }

      leaflet::leafletProxy("map", session) %>%
        leaflet::clearPopups() %>%
        leaflet::addPopups(
          lng = marker$longitude,
          lat = marker$latitude,
          popup = paste0(marker$popup, plot),
          options = leaflet::popupOptions(minWidth = 270, maxWidth = 300)
        )
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
      mapCreated(TRUE)
      map
    })

    # Auto-update for latest measurements every 15 minutes
    autoUpdateTimer <- reactiveTimer(15 * 60 * 1000) # 15 minutes in milliseconds

    observeEvent(autoUpdateTimer(), {
      req(mapCreated())
      if (!map_params$latest || !isTRUE(input$auto_update_latest)) {
        return()
      }
      set_target_now()
      updateMap()
      showNotification(
        tr("map_auto_update_notification", language$language),
        type = "message",
        duration = 5
      )
    })

    # Observe the map being created and update it when the parameters change
    observe({
      req(mapCreated(), map_params, language$language) # Ensure the map has been created before updating
      updateMap() # Call the updateMap function to refresh the map with the current parameters
    })
  }) # End of moduleServer
} # End of mapParams server function
