contTablePlotUI <- function(id) {
  ns <- NS(id)

  page_sidebar(
    sidebar = sidebar(
      title = NULL,
      width = 350,
      bg = config$sidebar_bg,
      open = list(mobile = "always-above"),
      uiOutput(ns("sidebar"))
    ),
    uiOutput(ns("main"))
  )
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
      range = cached$range,
      timeseries = cached$timeseries
    )

    moduleInputs <- reactiveValues(
      location_id = if (!is.null(inputs$location_id)) {
        as.numeric(inputs$location_id)
      } else {
        NULL
      },
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

      ts <- moduleData$timeseries
      if (
        !is.null(input$location_filter) && length(input$location_filter) > 0
      ) {
        ts <- ts[ts$location_id %in% as.numeric(input$location_filter), ]
      }
      if (
        !is.null(input$parameter_filter) && length(input$parameter_filter) > 0
      ) {
        ts <- ts[ts$parameter_id %in% as.numeric(input$parameter_filter), ]
      }

      ts |>
        dplyr::left_join(moduleData$locs, by = "location_id") |>
        dplyr::left_join(moduleData$sub_locs, by = "sub_location_id") |>
        dplyr::left_join(moduleData$params, by = "parameter_id") |>
        dplyr::left_join(moduleData$media, by = "media_id") |>
        dplyr::left_join(
          moduleData$aggregation_types,
          by = "aggregation_type_id"
        ) |>
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
          start_date,
          end_date
        ) |>
        dplyr::arrange(location, parameter, record_rate)
    })

    selected_timeseries <- reactiveVal(moduleInputs$timeseries_id)

    observeEvent(language$language, {
      ts <- timeseries_table()
      if (is.null(selected_timeseries()) && nrow(ts) > 0) {
        selected_timeseries(ts$timeseries_id[1])
      }
    })

    observeEvent(input$timeseries_table_rows_selected, {
      ts <- timeseries_table()
      if (
        !is.null(input$timeseries_table_rows_selected) &&
          length(input$timeseries_table_rows_selected) == 1 &&
          nrow(ts) >= input$timeseries_table_rows_selected
      ) {
        selected_timeseries(ts$timeseries_id[
          input$timeseries_table_rows_selected
        ])
      }
    })

    observeEvent(
      timeseries_table(),
      {
        ts <- timeseries_table()
        if (
          !is.null(moduleInputs$timeseries_id) &&
            moduleInputs$timeseries_id %in% ts$timeseries_id
        ) {
          selected_timeseries(moduleInputs$timeseries_id)
          moduleInputs$timeseries_id <- NULL
        } else if (is.null(selected_timeseries()) && nrow(ts) > 0) {
          selected_timeseries(ts$timeseries_id[1])
        }
      },
      ignoreNULL = FALSE
    )

    output$sidebar <- renderUI({
      date_range <- table_range()
      tagList(
        h4(tr("cont_table_intro", language$language)),
        selectizeInput(
          ns("location_filter"),
          tr("loc(s)", language$language),
          choices = stats::setNames(
            moduleData$locs$location_id,
            moduleData$locs$name
          ),
          selected = moduleInputs$location_id,
          multiple = TRUE,
          options = list(placeholder = tr("select_locs", language$language))
        ),
        selectizeInput(
          ns("parameter_filter"),
          tr("parameters", language$language),
          choices = stats::setNames(
            moduleData$params$parameter_id,
            moduleData$params$param_name
          ),
          multiple = TRUE,
          options = list(placeholder = tr("select_params", language$language))
        ),
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
        ),
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
        )
      )
    })

    output$main <- renderUI({
      tagList(
        h4(tr("cont_table_heading", language$language)),
        div(
          class = "mb-3",
          DT::dataTableOutput(ns("timeseries_table"))
        ),
        h4(tr("cont_table_plot_heading", language$language)),
        plotly::plotlyOutput(ns("timeseries_plot"), height = "600px")
      )
    })

    output$timeseries_table <- DT::renderDataTable({
      ts <- timeseries_table()
      selected_row <- NULL
      if (!is.null(selected_timeseries())) {
        selected_row <- which(ts$timeseries_id == selected_timeseries())
        if (length(selected_row) == 0) {
          selected_row <- NULL
        }
      }

      col_names <- c(
        tr("cont_table_col_location", language$language),
        tr("cont_table_col_sub_location", language$language),
        tr("cont_table_col_parameter", language$language),
        tr("cont_table_col_media", language$language),
        tr("cont_table_col_aggregation", language$language),
        tr("cont_table_col_record_rate", language$language),
        "z",
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
        colnames = c("timeseries_id", col_names)
      ) |>
        DT::formatDate(
          columns = c("start_date", "end_date"),
          method = "toDateString"
        )
    })

    output$timeseries_plot <- plotly::renderPlotly({
      req(input$date_range)
      req(selected_timeseries())

      tryCatch(
        {
          plotTimeseries(
            timeseries_id = selected_timeseries(),
            start_date = input$date_range[1],
            end_date = input$date_range[2],
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
