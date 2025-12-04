contTablePlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(sprintf(
      "
    /* container id */
    #%s.accordion {
      /* optional default for all panels */
    }
    
    /* Panel: filters_panel */
    #%s .accordion-item[data-value='%s'] {
      --bs-accordion-bg:        #FFFCF5;
      --bs-accordion-btn-bg:    #FBE5B2;
      --bs-accordion-active-bg: #FBE5B2;
    }
    
    /* Panel: table_panel */
    #%s .accordion-item[data-value='%s'] {
      --bs-accordion-bg:        #E5F4F6;
      --bs-accordion-btn-bg:    #0097A9;
      --bs-accordion-active-bg: #0097A9;
    }
    
    /* Panel: plot_panel */
    #%s .accordion-item[data-value='%s'] {
      --bs-accordion-bg:        #F1F4E5;
      --bs-accordion-btn-bg:    #7A9A01;
      --bs-accordion-active-bg: #7A9A01;
    }
    ",
      ns("accordion_panels"),
      ns("accordion_panels"),
      ns("filters_panel"),
      ns("accordion_panels"),
      ns("table_panel"),
      ns("accordion_panels"),
      ns("plot_panel")
    ))),
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
      param_groups = cached$param_groups,
      param_sub_groups = cached$param_sub_groups,
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
      loc_name_col <- tr("generic_name_col", language$language)
      sub_loc_col <- tr("sub_location_col", language$language)
      param_col <- tr("param_name_col", language$language)
      media_col <- tr("media_type_col", language$language)
      agg_col <- tr("aggregation_type_col", language$language)
      network_col <- tr("generic_name_col", language$language)
      project_col <- tr("generic_name_col", language$language)

      ts <- moduleData$timeseries
      loc_filter_ids <- location_network_filter()
      ts <- ts[location_id %in% loc_filter_ids]

      locs <- unique(moduleData$locs, by = "location_id")
      sub_locs <- unique(moduleData$sub_locs, by = "sub_location_id")
      params <- unique(moduleData$params, by = "parameter_id")
      media <- unique(moduleData$media, by = "media_id")
      aggregation_types <- unique(
        moduleData$aggregation_types,
        by = "aggregation_type_id"
      )

      networks <- moduleData$locations_networks
      if (nrow(networks) > 0 && nrow(moduleData$networks) > 0) {
        networks <- merge(
          networks,
          moduleData$networks,
          by = "network_id",
          all.x = TRUE,
          allow.cartesian = TRUE
        )[,
          .(
            networks = paste(
              unique(stats::na.omit(.SD[[network_col]])),
              collapse = ", "
            )
          ),
          .SDcols = network_col,
          by = location_id
        ]
      } else {
        networks <- data.table::data.table(
          location_id = numeric(),
          networks = character()
        )
      }

      projects <- moduleData$locations_projects
      if (nrow(projects) > 0 && nrow(moduleData$projects) > 0) {
        projects <- merge(
          projects,
          moduleData$projects,
          by = "project_id",
          all.x = TRUE,
          allow.cartesian = TRUE
        )[,
          .(
            projects = paste(
              unique(stats::na.omit(.SD[[project_col]])),
              collapse = ", "
            )
          ),
          .SDcols = project_col,
          by = location_id
        ]
      } else {
        projects <- data.table::data.table(
          location_id = numeric(),
          projects = character()
        )
      }

      ts <- merge(
        ts,
        locs[,
          .(location_id, location = .SD[[loc_name_col]]),
          .SDcols = loc_name_col
        ],
        by = "location_id",
        all.x = TRUE
      )
      ts <- merge(
        ts,
        sub_locs[,
          .(sub_location_id, sub_location = .SD[[sub_loc_col]]),
          .SDcols = sub_loc_col
        ],
        by = "sub_location_id",
        all.x = TRUE
      )
      ts <- merge(
        ts,
        params[,
          .(parameter_id, parameter = .SD[[param_col]]),
          .SDcols = param_col
        ],
        by = "parameter_id",
        all.x = TRUE
      )
      ts <- merge(
        ts,
        media[, .(media_id, media = .SD[[media_col]]), .SDcols = media_col],
        by = "media_id",
        all.x = TRUE
      )
      ts <- merge(
        ts,
        aggregation_types[,
          .(aggregation_type_id, aggregation = .SD[[agg_col]]),
          .SDcols = agg_col
        ],
        by = "aggregation_type_id",
        all.x = TRUE
      )
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

      # Helper function to identify empty columns
      is_empty_col <- function(col) {
        if (is.character(col) || is.factor(col)) {
          all(is.na(col) | col == "")
        } else {
          all(is.na(col))
        }
      }

      empty_cols <- setdiff(
        names(ts)[vapply(ts, is_empty_col, logical(1))],
        "timeseries_id"
      )
      if (length(empty_cols) > 0) {
        ts[, (empty_cols) := NULL]
      }

      data.table::setorder(ts, location, parameter, record_rate)
      ts
    })

    output$main <- renderUI({
      req(moduleData, language$language)
      date_range <- table_range()
      network_col <- tr("generic_name_col", language$language)
      project_col <- tr("generic_name_col", language$language)
      param_grp_col <- tr("param_group_col", language$language)
      network_choices <- if (
        nrow(moduleData$networks) > 0 &&
          network_col %in% names(moduleData$networks)
      ) {
        stats::setNames(
          moduleData$networks$network_id[order(moduleData$networks[[
            network_col
          ]])],
          moduleData$networks[[network_col]][order(moduleData$networks[[
            network_col
          ]])]
        )
      } else {
        NULL
      }
      project_choices <- if (
        nrow(moduleData$projects) > 0 &&
          project_col %in% names(moduleData$projects)
      ) {
        stats::setNames(
          moduleData$projects$project_id[order(moduleData$projects[[
            project_col
          ]])],
          moduleData$projects[[project_col]][order(moduleData$projects[[
            project_col
          ]])]
        )
      } else {
        NULL
      }
      param_grp_choices <- if (
        nrow(moduleData$param_groups) > 0 &&
          param_grp_col %in% names(moduleData$param_groups)
      ) {
        stats::setNames(
          moduleData$param_groups$group_id[order(moduleData$param_groups[[
            param_grp_col
          ]])],
          moduleData$param_groups[[
            param_grp_col
          ]][order(moduleData$param_groups[[
            param_grp_col
          ]])]
        )
      } else {
        NULL
      }
      tagList(
        bslib::accordion(
          id = ns("accordion_panels"),
          open = c(ns("table_panel")),
          bslib::accordion_panel(
            title = tr("filters", language$language),
            value = ns("filters_panel"),
            fluidRow(
              column(
                width = 4,
                selectizeInput(
                  ns("network_filter"),
                  label = tr("networks", language$language),
                  choices = network_choices,
                  multiple = TRUE,
                  options = list(
                    placeholder = tr("select_network", language$language)
                  )
                )
              ),
              column(
                width = 4,
                selectizeInput(
                  ns("project_filter"),
                  label = tr("projects", language$language),
                  choices = project_choices,
                  multiple = TRUE,
                  options = list(
                    placeholder = tr("select_project", language$language)
                  )
                )
              ),
              column(
                width = 4,
                selectizeInput(
                  ns("param_grp_filter"),
                  label = tr("param_groups", language$language),
                  choices = param_grp_choices,
                  multiple = TRUE,
                  options = list(
                    placeholder = tr("select_param_group", language$language)
                  )
                )
              )
            )
          ),
          bslib::accordion_panel(
            title = tr("cont_table_heading", language$language),
            value = ns("table_panel"),
            div(
              DT::dataTableOutput(ns("timeseries_table"))
            )
          )
        ),
        h4(tr("cont_table_plot_heading", language$language)),
        bslib::card(
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
                input_task_button(
                  ns("make_plot"),
                  label = tr("create_plot", language$language),
                  style = "display: block; width: 100%;",
                  class = "btn btn-primary"
                )
              )
            )
          )
        ),
        plotly::plotlyOutput(ns("timeseries_plot"), height = "600px")
      )
    }) %>% # End renderUI
      bindEvent(language$language) # Re-render the UI if the language or moduleData changes

    observeEvent(
      timeseries_table(),
      {
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
      },
      ignoreNULL = FALSE
    )

    observeEvent(input$timeseries_table_rows_selected, {
      ts <- timeseries_table()
      if (
        !is.null(input$timeseries_table_rows_selected) &&
          length(input$timeseries_table_rows_selected) == 1 &&
          nrow(ts) >= input$timeseries_table_rows_selected
      ) {
        selected_timeseries_id(ts$timeseries_id[
          input$timeseries_table_rows_selected
        ])
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
        z = tr("z", language$language),
        networks = tr("networks", language$language),
        projects = tr("projects", language$language),
        start_date = tr("cont_table_col_start_date", language$language),
        end_date = tr("cont_table_col_end_date", language$language)
      )

      visible_cols <- names(ts)
      col_names <- unname(column_labels[visible_cols[-1]])

      order_columns <- list()
      if ("location" %in% visible_cols) {
        order_columns[[length(order_columns) + 1]] <- list(
          match("location", visible_cols) - 1,
          "asc"
        )
      }
      if ("parameter" %in% visible_cols) {
        order_columns[[length(order_columns) + 1]] <- list(
          match("parameter", visible_cols) - 1,
          "asc"
        )
      }

      dt <- DT::datatable(
        ts,
        rownames = FALSE,
        selection = list(mode = "single", selected = selected_row),
        options = list(
          pageLength = 5,
          lengthMenu = c(5, 10, 20),
          columnDefs = list(list(visible = FALSE, targets = 0)), # Hide timeseries_id
          scrollX = TRUE,
          initComplete = htmlwidgets::JS(
            "function(settings, json) {
             $(this.api().table().header()).css({
              'font-size': '90%'
             });
             $(this.api().table().body()).css({
              'font-size': '80%'
             });
            }"
          ),
          language = list(
            info = tr("tbl_info", language$language),
            infoEmpty = tr("tbl_info_empty", language$language),
            paginate = list(previous = "", `next` = ""),
            search = tr("tbl_search", language$language),
            lengthMenu = tr("tbl_length", language$language),
            infoFiltered = tr("tbl_filtered", language$language),
            zeroRecords = tr("tbl_zero", language$language)
          ),
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

    plot_request <- reactive({
      ts <- timeseries_table()
      validate(need(nrow(ts) > 0, tr("error", language$language)))

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
        end_date = input$date_range[2],
        historic_range = input$show_hist,
        unusable = input$show_unusable,
        grades = input$show_grades,
        approvals = input$show_approvals,
        qualifiers = input$show_qualifiers,
        lang = language$abbrev
      )
    })

    # ExtendedTask that does the heavy plotting work
    plot_task <- ExtendedTask$new(function(req) {
      # req is the list returned by plot_request()
      plotTimeseries(
        timeseries_id = req$timeseries_id,
        start_date = req$start_date,
        end_date = req$end_date,
        historic_range = req$historic_range,
        unusable = req$unusable,
        grades = req$grades,
        approvals = req$approvals,
        qualifiers = req$qualifiers,
        lang = req$lang,
        webgl = session$userData$use_webgl
      )
    }) |>
      bind_task_button("make_plot")

    # Kick off task on button click
    observeEvent(input$make_plot, {
      plot_task$invoke(plot_request())
    })

    # Render from the task result
    output$timeseries_plot <- plotly::renderPlotly({
      plot_task$result()
    })
  })
}
