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

    moduleInputs <- reactiveValues(
      timeseries_id = if (!is.null(inputs$timeseries_id)) {
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

      ts <- moduleData$timeseries
      loc_filter_ids <- location_network_filter()
      ts <- ts[ts$location_id %in% loc_filter_ids, ]

      networks <- moduleData$locations_networks |>
        dplyr::left_join(moduleData$networks, by = "network_id") |>
        dplyr::group_by(location_id) |>
        dplyr::summarise(
          networks = paste(
            unique(stats::na.omit(.data[[network_col]])),
            collapse = ", "
          ),
          .groups = "drop"
        )

      projects <- moduleData$locations_projects |>
        dplyr::left_join(moduleData$projects, by = "project_id") |>
        dplyr::group_by(location_id) |>
        dplyr::summarise(
          projects = paste(
            unique(stats::na.omit(.data[[project_col]])),
            collapse = ", "
          ),
          .groups = "drop"
        )

      ts |>
        dplyr::left_join(moduleData$locs, by = "location_id") |>
        dplyr::left_join(moduleData$sub_locs, by = "sub_location_id") |>
        dplyr::left_join(moduleData$params, by = "parameter_id") |>
        dplyr::left_join(moduleData$media, by = "media_id") |>
        dplyr::left_join(
          moduleData$aggregation_types,
          by = "aggregation_type_id"
        ) |>
        dplyr::left_join(networks, by = "location_id") |>
        dplyr::left_join(projects, by = "location_id") |>
        dplyr::mutate(
          record_rate = as.character(lubridate::seconds_to_period(record_rate)),
          start_date = as.Date(start_datetime),
          end_date = as.Date(end_datetime)
        ) |>
        dplyr::transmute(
          timeseries_id,
          location = .data[[loc_name_col]],
          sub_location = .data[[sub_loc_col]],
          parameter = .data[[param_col]],
          media = .data[[media_col]],
          aggregation = .data[[agg_col]],
          record_rate,
          z,
          networks,
          projects,
          start_date,
          end_date
        ) |>
        dplyr::mutate(
          parameter = as.factor(parameter),
          media = as.factor(media),
          aggregation = as.factor(aggregation)
        ) |>
        dplyr::arrange(location, parameter, record_rate)
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

    output$timeseries_table <- DT::renderDataTable({
      ts <- timeseries_table()
      selected_id <- moduleInputs$timeseries_id
      if (!is.null(input$timeseries_table_rows_selected) &&
        length(input$timeseries_table_rows_selected) == 1 &&
        nrow(ts) >= input$timeseries_table_rows_selected) {
        selected_id <- ts$timeseries_id[input$timeseries_table_rows_selected]
      }
      if (!is.null(selected_id) && !selected_id %in% ts$timeseries_id && nrow(ts) > 0) {
        selected_id <- ts$timeseries_id[1]
      }
      if (is.null(selected_id) && nrow(ts) > 0) {
        selected_id <- ts$timeseries_id[1]
      }
      selected_row <- if (!is.null(selected_id)) {
        which(ts$timeseries_id == selected_id)
      } else {
        NULL
      }
      if (length(selected_row) == 0) {
        selected_row <- NULL
      }
      moduleInputs$timeseries_id <- selected_id

      col_names <- c(
        tr("cont_table_col_location", language$language),
        tr("cont_table_col_sub_location", language$language),
        tr("cont_table_col_parameter", language$language),
        tr("cont_table_col_media", language$language),
        tr("cont_table_col_aggregation", language$language),
        tr("cont_table_col_record_rate", language$language),
        "z",
        "Networks",
        "Projects",
        tr("cont_table_col_start_date", language$language),
        tr("cont_table_col_end_date", language$language)
      )

      DT::datatable(
        ts,
        rownames = FALSE,
        selection = list(mode = "single", selected = selected_row),
        options = list(
          pageLength = 10,
          columnDefs = list(list(visible = FALSE, targets = 0)),
          order = list(list(1, "asc"), list(3, "asc"))
        ),
        colnames = c("timeseries_id", col_names),
        filter = "top"
      ) |>
        DT::formatDate(
          columns = c("start_date", "end_date"),
          method = "toDateString"
        )
    })

    plot_request <- eventReactive(input$render_plot, {
      ts <- timeseries_table()
      validate(need(nrow(ts) > 0, tr("no_data", language$language)))

      selected_row <- input$timeseries_table_rows_selected
      if (is.null(selected_row) || length(selected_row) == 0) {
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
