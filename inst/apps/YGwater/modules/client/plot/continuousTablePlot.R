contTablePlotUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("main"))
}

contTablePlot <- function(id, language, inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (session$userData$user_logged_in) {
      cached <- cont_data.plot_module_data(
        con = session$userData$AquaCache,
        env = session$userData$app_cache
      )
    } else {
      cached <- cont_data.plot_module_data(con = session$userData$AquaCache)
    }

    moduleData <- reactiveValues(
      locs = cached$locs,
      sub_locs = cached$sub_locs,
      params = cached$params,
      media = cached$media,
      aggregation_types = cached$aggregation_types,
      locations_projects = cached$locations_projects,
      projects = cached$projects,
      locations_networks = cached$locations_networks,
      networks = cached$networks,
      range = cached$range,
      timeseries = cached$timeseries
    )

    selected_timeseries_id <- reactiveVal(
      if (!is.null(inputs$timeseries_id)) {
        as.numeric(inputs$timeseries_id)
      } else {
        NULL
      }
    )

    table_range <- reactive({
      start_val <- as.Date(moduleData$range$min_datetime)
      end_val <- as.Date(moduleData$range$max_datetime)

      if (is.na(start_val)) {
        start_val <- Sys.Date() - 365
      }
      if (is.na(end_val)) {
        end_val <- Sys.Date()
      }

      list(start = start_val, end = end_val)
    })

    location_network_filter <- reactive({
      loc_ids <- moduleData$locs$location_id

      if (!is.null(input$network_filter) && length(input$network_filter) > 0) {
        loc_ids <- intersect(
          loc_ids,
          moduleData$locations_networks$location_id[
            moduleData$locations_networks$network_id %in% input$network_filter
          ]
        )
      }

      if (!is.null(input$project_filter) && length(input$project_filter) > 0) {
        loc_ids <- intersect(
          loc_ids,
          moduleData$locations_projects$location_id[
            moduleData$locations_projects$project_id %in% input$project_filter
          ]
        )
      }

      loc_ids
    })

    timeseries_table <- reactive({
      lang <- language$language
      loc_name_col <- if (lang == "fr") "name_fr" else "name"
      sub_loc_col <- if (lang == "fr") {
        "sub_location_name_fr"
      } else {
        "sub_location_name"
      }
      param_col <- if (lang == "fr") "param_name_fr" else "param_name"
      media_col <- if (lang == "fr") "media_type_fr" else "media_type"
      agg_col <- if (lang == "fr") "aggregation_type_fr" else "aggregation_type"
      network_col <- if (lang == "fr") "name_fr" else "name"
      project_col <- if (lang == "fr") "name_fr" else "name"

      as_dt <- function(x) data.table::as.data.table(data.table::copy(x))

      ts <- as_dt(moduleData$timeseries)
      loc_filter_ids <- location_network_filter()
      ts <- ts[location_id %in% loc_filter_ids]

      locs <- unique(as_dt(moduleData$locs), by = "location_id")
      sub_locs <- unique(as_dt(moduleData$sub_locs), by = "sub_location_id")
      params <- unique(as_dt(moduleData$params), by = "parameter_id")
      media <- unique(as_dt(moduleData$media), by = "media_id")
      aggregation_types <- unique(as_dt(moduleData$aggregation_types), by = "aggregation_type_id")

      networks <- as_dt(moduleData$locations_networks)
      if (nrow(networks) > 0 && nrow(moduleData$networks) > 0) {
        networks <- merge(
          networks,
          as_dt(moduleData$networks),
          by = "network_id",
          all.x = TRUE,
          allow.cartesian = TRUE
        )[, .(networks = paste(unique(stats::na.omit(.SD[[network_col]])), collapse = ", ")), .SDcols = network_col, by = location_id]
      } else {
        networks <- data.table::data.table(location_id = numeric(), networks = character())
      }

      projects <- as_dt(moduleData$locations_projects)
      if (nrow(projects) > 0 && nrow(moduleData$projects) > 0) {
        projects <- merge(
          projects,
          as_dt(moduleData$projects),
          by = "project_id",
          all.x = TRUE,
          allow.cartesian = TRUE
        )[, .(projects = paste(unique(stats::na.omit(.SD[[project_col]])), collapse = ", ")), .SDcols = project_col, by = location_id]
      } else {
        projects <- data.table::data.table(location_id = numeric(), projects = character())
      }

      ts <- merge(ts, locs[, .(location_id, location = .SD[[loc_name_col]]), .SDcols = loc_name_col], by = "location_id", all.x = TRUE)
      ts <- merge(ts, sub_locs[, .(sub_location_id, sub_location = .SD[[sub_loc_col]]), .SDcols = sub_loc_col], by = "sub_location_id", all.x = TRUE)
      ts <- merge(ts, params[, .(parameter_id, parameter = .SD[[param_col]]), .SDcols = param_col], by = "parameter_id", all.x = TRUE)
      ts <- merge(ts, media[, .(media_id, media = .SD[[media_col]]), .SDcols = media_col], by = "media_id", all.x = TRUE)
      ts <- merge(ts, aggregation_types[, .(aggregation_type_id, aggregation = .SD[[agg_col]]), .SDcols = agg_col], by = "aggregation_type_id", all.x = TRUE)
      ts <- merge(ts, networks, by = "location_id", all.x = TRUE)
      ts <- merge(ts, projects, by = "location_id", all.x = TRUE)

      ts[, `:=`(
        record_rate = as.character(lubridate::seconds_to_period(record_rate)),
        start_date = as.Date(start_datetime),
        end_date = as.Date(end_datetime),
        parameter = as.factor(parameter),
        media = as.factor(media),
        aggregation = as.factor(aggregation)
      )]

      ts <- ts[, .(
        timeseries_id,
        location,
        sub_location,
        parameter,
        media,
        aggregation,
        record_rate,
        z,
        networks,
        projects,
        start_date,
        end_date
      )]

      is_empty_col <- function(col) {
        if (is.character(col) || is.factor(col)) {
          all(is.na(col) | col == "")
        } else {
          all(is.na(col))
        }
      }

      empty_cols <- setdiff(names(ts)[vapply(ts, is_empty_col, logical(1))], "timeseries_id")
      if (length(empty_cols) > 0) {
        ts[, (empty_cols) := NULL]
      }

      data.table::setorder(ts, location, parameter, record_rate)
      ts
    })

    output$main <- renderUI({
      date_range <- table_range()
      network_col <- if (language$language == "fr") "name_fr" else "name"
      project_col <- if (language$language == "fr") "name_fr" else "name"
      network_choices <- if (
        nrow(moduleData$networks) > 0 && network_col %in% names(moduleData$networks)
      ) {
        stats::setNames(
          moduleData$networks$network_id,
          moduleData$networks[[network_col]]
        )
      } else {
        NULL
      }
      project_choices <- if (
        nrow(moduleData$projects) > 0 && project_col %in% names(moduleData$projects)
      ) {
        stats::setNames(
          moduleData$projects$project_id,
          moduleData$projects[[project_col]]
        )
      } else {
        NULL
      }

      tagList(
        h4(tr("cont_table_heading", language$language)),
        bslib::accordion(
          id = ns("filters"),
          bslib::accordion_panel(
            "Filters",
            fluidRow(
              column(
                width = 6,
                selectizeInput(
                  ns("network_filter"),
                  label = "Networks",
                  choices = network_choices,
                  multiple = TRUE,
                  options = list(placeholder = "Select network(s)")
                )
              ),
              column(
                width = 6,
                selectizeInput(
                  ns("project_filter"),
                  label = "Projects",
                  choices = project_choices,
                  multiple = TRUE,
                  options = list(placeholder = "Select project(s)")
                )
              )
            )
          ),
          bslib::accordion_panel(
            "Database",
            div(
              class = "mb-3",
              DT::dataTableOutput(ns("timeseries_table"))
            )
          )
        ),
        h4(tr("cont_table_plot_heading", language$language)),
        bslib::card(
          class = "mb-3",
          bslib::card_body(
            fluidRow(
              column(
                width = 6,
                dateRangeInput(
                  ns("date_range"),
                  tr("date_range_lab", language$language),
                  start = date_range$start,
                  end = date_range$end,
                  format = "yyyy-mm-dd",
                  startview = "month",
                  min = date_range$start,
                  max = date_range$end,
                  language = language$abbrev,
                  separator = tr("date_sep", language$language)
                )
              ),
              column(
                width = 6,
                checkboxInput(
                  ns("show_hist"),
                  tr("plot_hist_range", language$language),
                  value = TRUE
                ),
                checkboxInput(
                  ns("show_unusable"),
                  tr("plot_show_unusable", language$language),
                  value = FALSE
                ),
                checkboxInput(
                  ns("show_grades"),
                  tr("plot_show_grades", language$language),
                  value = FALSE
                ),
                checkboxInput(
                  ns("show_approvals"),
                  tr("plot_show_approvals", language$language),
                  value = FALSE
                ),
                checkboxInput(
                  ns("show_qualifiers"),
                  tr("plot_show_qualifiers", language$language),
                  value = FALSE
                ),
                actionButton(
                  ns("render_plot"),
                  label = "Render plot",
                  class = "btn-primary"
                )
              )
            )
          )
        ),
        plotly::plotlyOutput(ns("timeseries_plot"), height = "600px")
      )
    })

    observeEvent(timeseries_table(), {
      ts <- timeseries_table()
      current <- selected_timeseries_id()
      if (!is.null(current) && current %in% ts$timeseries_id) {
        return()
      }
      if (nrow(ts) > 0) {
        selected_timeseries_id(ts$timeseries_id[1])
      } else {
        selected_timeseries_id(NULL)
      }
    }, ignoreNULL = FALSE)

    observeEvent(input$timeseries_table_rows_selected, {
      ts <- timeseries_table()
      if (!is.null(input$timeseries_table_rows_selected) &&
        length(input$timeseries_table_rows_selected) == 1 &&
        nrow(ts) >= input$timeseries_table_rows_selected) {
        selected_timeseries_id(ts$timeseries_id[input$timeseries_table_rows_selected])
      }
    })

    output$timeseries_table <- DT::renderDataTable({
      ts <- timeseries_table()
      selected_id <- selected_timeseries_id()
      selected_row <- if (!is.null(selected_id)) {
        which(ts$timeseries_id == selected_id)
      } else {
        NULL
      }
      if (length(selected_row) == 0) {
        selected_row <- NULL
      }

      column_labels <- c(
        timeseries_id = "timeseries_id",
        location = tr("cont_table_col_location", language$language),
        sub_location = tr("cont_table_col_sub_location", language$language),
        parameter = tr("cont_table_col_parameter", language$language),
        media = tr("cont_table_col_media", language$language),
        aggregation = tr("cont_table_col_aggregation", language$language),
        record_rate = tr("cont_table_col_record_rate", language$language),
        z = "z",
        networks = "Networks",
        projects = "Projects",
        start_date = tr("cont_table_col_start_date", language$language),
        end_date = tr("cont_table_col_end_date", language$language)
      )

      visible_cols <- names(ts)
        col_names <- unname(column_labels[visible_cols[-1]])

      order_columns <- list()
      if ("location" %in% visible_cols) {
        order_columns[[length(order_columns) + 1]] <- list(match("location", visible_cols) - 1, "asc")
      }
      if ("parameter" %in% visible_cols) {
        order_columns[[length(order_columns) + 1]] <- list(match("parameter", visible_cols) - 1, "asc")
      }

        dt <- DT::datatable(
          ts,
          rownames = FALSE,
          selection = list(mode = "single", selected = selected_row),
          options = list(
            pageLength = 10,
          columnDefs = list(list(visible = FALSE, targets = 0)),
          order = order_columns
          ),
          colnames = c("timeseries_id", col_names),
          filter = "top"
        )

        date_cols <- intersect(c("start_date", "end_date"), names(ts))
        if (length(date_cols) > 0) {
          dt <- DT::formatDate(
            dt,
            columns = date_cols,
            method = "toDateString"
          )
        }

        dt
      })

    plot_request <- eventReactive(input$render_plot, {
      ts <- timeseries_table()
      validate(need(nrow(ts) > 0, tr("no_data", language$language)))

      selected_id <- selected_timeseries_id()
      selected_row <- if (!is.null(selected_id)) {
        which(ts$timeseries_id == selected_id)
      } else {
        NULL
      }
      if (length(selected_row) == 0) {
        selected_row <- 1
      }

      req(input$date_range)

      list(
        timeseries_id = ts$timeseries_id[selected_row],
        start_date = input$date_range[1],
        end_date = input$date_range[2]
      )
    })

    output$timeseries_plot <- plotly::renderPlotly({
      req(plot_request())

      tryCatch(
        {
          plotTimeseries(
            timeseries_id = plot_request()$timeseries_id,
            start_date = plot_request()$start_date,
            end_date = plot_request()$end_date,
            historic_range = input$show_hist,
            unusable = input$show_unusable,
            grades = input$show_grades,
            approvals = input$show_approvals,
            qualifiers = input$show_qualifiers,
            lang = language$abbrev,
            webgl = TRUE
          )
        },
        error = function(e) {
          validate(need(FALSE, e$message))
        }
      )
    })
  })
}
