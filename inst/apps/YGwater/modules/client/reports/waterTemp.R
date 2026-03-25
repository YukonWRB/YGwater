waterTempUIMod <- function(id) {
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
      --bs-accordion-btn-bg:    #bacc81ff;
      --bs-accordion-active-bg: #bacc81ff;
    }

    /* Panel: metadata_panel */
    #%s .accordion-item[data-value='%s'] {
      --bs-accordion-bg:        #FFFCF5;
      --bs-accordion-btn-bg:    #FBE5B2;
      --bs-accordion-active-bg: #FBE5B2;
    }
    ",
      ns("accordion1"),
      ns("accordion1"),
      ns("filters_panel"),
      ns("accordion1"),
      ns("table_panel"),
      ns("accordion1"),
      ns("plot_options_panel"),
      ns("accordion2"),
      ns("metadata_panel")
    ))),

    # Adjust table background so they don't blend into the accordion
    tags$style(HTML(sprintf(
      "
      /* Force DT table backgrounds to white for these module tables */
      #%s table.dataTable,
      #%s table.dataTable tbody,
      #%s table.dataTable tbody tr,
      #%s table.dataTable tbody td,
    
      #%s table.dataTable,
      #%s table.dataTable tbody,
      #%s table.dataTable tbody tr,
      #%s table.dataTable tbody td,
    
      #%s table.dataTable,
      #%s table.dataTable tbody,
      #%s table.dataTable tbody tr,
      #%s table.dataTable tbody td,
    
      #%s table.dataTable,
      #%s table.dataTable tbody,
      #%s table.dataTable tbody tr,
      #%s table.dataTable tbody td {
        background-color: #FFFFFF !important;
      }
    
      /* Only timeseries_table has the filter row in the header */
      #%s table.dataTable thead tr,
      #%s table.dataTable thead th,
      #%s table.dataTable thead td {
        background-color: #FFFFFF !important;
      }
      ",
      ns("timeseries_table"),
      ns("timeseries_table"),
      ns("timeseries_table"),
      ns("timeseries_table"),
      ns("location_metadata_table"),
      ns("location_metadata_table"),
      ns("location_metadata_table"),
      ns("location_metadata_table"),
      ns("timeseries_metadata_table"),
      ns("timeseries_metadata_table"),
      ns("timeseries_metadata_table"),
      ns("timeseries_metadata_table"),
      ns("timeseries_ownership_table"),
      ns("timeseries_ownership_table"),
      ns("timeseries_ownership_table"),
      ns("timeseries_ownership_table"),
      ns("timeseries_table"),
      ns("timeseries_table"),
      ns("timeseries_table")
    ))),

    uiOutput(ns("banner")),
    uiOutput(ns("main"))
  )
}

waterTempMod <- function(id, language, inputs) {
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

    moduleInputs <- reactiveValues(
      location_id = if (!is.null(inputs$location_id)) {
        as.numeric(inputs$location_id)
      } else {
        NULL
      }
    )

    initial_timeseries_ids <- if (!is.null(inputs$timeseries_id)) {
      utils::head(unique(as.numeric(inputs$timeseries_id)), 4)
    } else {
      numeric(0)
    }
    selected_timeseries_slots <- reactiveVal(
      if (length(initial_timeseries_ids) > 0) {
        initial_timeseries_ids
      } else {
        NA_real_
      }
    )
    active_timeseries_slot <- reactiveVal(1L)

    selected_timeseries_id_active <- reactive({
      slots <- selected_timeseries_slots()
      if (length(slots) == 0) {
        return(NULL)
      }
      idx <- active_timeseries_slot()
      if (
        is.null(idx) ||
          is.na(idx) ||
          idx < 1 ||
          idx > length(slots) ||
          is.na(slots[[idx]])
      ) {
        filled_idx <- which(!is.na(slots))
        if (length(filled_idx) == 0) {
          return(NULL)
        }
        idx <- filled_idx[[1]]
      }
      as.numeric(slots[[idx]])
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

    location_filter_value <- reactive({
      if (is.null(moduleInputs$location_id)) {
        return(NULL)
      }

      loc_name_col <- tr("generic_name_col", language$language)
      locs <- unique(moduleData$locs, by = "location_id")

      available_values <- locs[
        location_id %in% moduleInputs$location_id,
        ..loc_name_col
      ][[loc_name_col]]

      if (length(available_values) == 0) {
        return(NULL)
      }

      stats::na.omit(available_values)[1]
    })

    timeseries_table_reactive <- reactive({
      loc_name_col <- tr("generic_name_col", language$language)
      sub_loc_col <- tr("sub_location_col", language$language)
      param_col <- tr("param_name_col", language$language)
      media_col <- tr("media_type_col", language$language)
      agg_col <- tr("aggregation_type_col", language$language)
      network_col <- tr("generic_name_col", language$language)
      project_col <- tr("generic_name_col", language$language)

      ts <- moduleData$timeseries
      loc_filter_ids <- location_network_filter()
      locs <- unique(moduleData$locs, by = "location_id")
      sub_locs <- unique(moduleData$sub_locs, by = "sub_location_id")
      params <- unique(moduleData$params, by = "parameter_id")
      temp_parameter_ids <- unique(params[
        trimws(tolower(param_name)) == "temperature, water",
        parameter_id
      ])
      ts <- ts[
        location_id %in% loc_filter_ids & parameter_id %in% temp_parameter_ids
      ]
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
        start_date = as.Date(start_datetime),
        end_date = as.Date(end_datetime),
        parameter = as.factor(parameter),
        media = as.factor(media),
        aggregation = as.factor(aggregation),
        z = as.numeric(z),
        location = as.factor(location),
        sub_location = as.factor(sub_location),
        loc_code = as.factor(loc_code),
        networks = as.factor(networks),
        projects = as.factor(projects)
      )]

      # Convert to periods
      ts[,
        record_rate := ifelse(
          !is.na(record_rate),
          as.character(lubridate::seconds_to_period(record_rate)),
          NA_character_
        )
      ]

      ts <- ts[, .(
        timeseries_id,
        location,
        sub_location,
        loc_code,
        parameter,
        media,
        aggregation,
        z,
        record_rate,
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

      if (nrow(ts) > 0) {
        empty_cols <- setdiff(
          names(ts)[vapply(ts, is_empty_col, logical(1))],
          "timeseries_id"
        )
        if (length(empty_cols) > 0) {
          ts[, (empty_cols) := NULL]
        }
      }

      sort_cols <- intersect(c("location", "parameter"), names(ts))
      if (length(sort_cols) > 0) {
        data.table::setorderv(ts, sort_cols)
      }
      ts
    })

    output$banner <- renderUI({
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "contPlot"
      )
    })

    output$main <- renderUI({
      req(moduleData, language$language, language$abbrev)
      network_col <- tr("generic_name_col", language$language)
      project_col <- tr("generic_name_col", language$language)
      param_grp_col <- tr("param_group_col", language$language)
      intro_text <- HTML(
        tr("gen_waterTemp_info", language$language)
      )
      tags <- tagList(
        div(
          style = paste(
            "background-color: #F7FAFC;",
            "border-left: 4px solid #0097A9;",
            "border-radius: 6px;",
            "padding: 12px 16px;",
            "margin-bottom: 12px;"
          ),
          tags$p(style = "margin-bottom: 0;", intro_text)
        ),
        bslib::accordion(
          id = ns("accordion1"),
          open = c(ns("table_panel"), ns("plot_options_panel")),

          bslib::accordion_panel(
            title = tr("cont_table_heading", language$language),
            value = ns("table_panel"),
            div(
              DT::dataTableOutput(ns("timeseries_table")),
              uiOutput(ns("selected_timeseries_output")) #,
            ) # End div
          ), # End table_panel
          bslib::accordion_panel(
            title = tr("cont_table_plot_heading", language$language),
            value = ns("plot_options_panel"),
            fluidRow(
              column(
                width = 6,
                dateRangeInput(
                  ns("date_range"),
                  tr("date_range_lab", language$language),
                  start = Sys.Date() - 365,
                  end = Sys.Date(),
                  format = "yyyy-mm-dd",
                  startview = "month",
                  language = language$abbrev,
                  separator = tr("date_sep", language$language)
                ),
                actionButton(
                  ns("entire_record"),
                  tr("plot_all_record", language$language)
                ),
                hr(),
                dateRangeInput(
                  ns("highlight_range"),
                  "Highlight Range",
                  start = Sys.Date() - 365,
                  end = Sys.Date(),
                  format = "yyyy-mm-dd",
                  startview = "month",
                  language = language$abbrev,
                  separator = tr("date_sep", language$language)
                ),
                actionButton(
                  ns("recent_year"),
                  "Highlight Most Recent Year"
                )
              ),
              column(
                width = 6,
                selectizeInput(
                  ns("thresholds_select"),
                  label = "Thresholds",
                  choices = c("20 °C" = 20, "18 °C" = 18, "13 °C" = 13),
                  multiple = TRUE
                ),
                div(br()),
                div(strong("Plot Description:")),
                div("Blue: Highlighted Day of the Year Mean"),
                div("Grey Line: Non-Highlighted Day of the Year Mean"),
                div("Grey Ribbon: Non-Highlighted Day of the Year Range")
              )
            ),
            hr(),
            fluidRow(
              column(
                12,
                align = "center",
                bslib::input_task_button(
                  ns("make_plot"),
                  label = "Create Report Figures",
                  label_busy = "Creating Figures..."
                )
              )
            )
          ) # End plot_options_panel
        ), # End accordion 1
        uiOutput(ns("report_figures_ui")),

        tags$div(style = "height: 10px;"),

        # Start accordion 2
        bslib::accordion(
          id = ns("accordion2"),
          open = FALSE,
          bslib::accordion_panel(
            title = tr("metadata", language$language),
            value = ns("metadata_panel"),
            uiOutput(ns("metadata_message")),
            fluidRow(
              column(
                width = 6,
                tags$h5(tr("location_metadata_heading", language$language)),
                DT::dataTableOutput(ns("location_metadata_table"))
              ),
              column(
                width = 6,
                tags$h5(tr("timeseries_metadata_heading", language$language)),
                DT::dataTableOutput(ns("timeseries_metadata_table"))
              )
            ),
            tags$div(style = "height: 10px;"),
            tags$h5(tr("ownership_history_heading", language$language)),
            DT::dataTableOutput(ns("timeseries_ownership_table"))
          )
        ) # End accordion2 accordion
      ) # End tagList
      return(tags)
    }) # End renderUI

    output$report_figures_ui <- renderUI({
      req(plot_bundle())

      figures_heading <- if (identical(language$abbrev, "fr")) {
        "Figures du rapport"
      } else {
        "Report Figures"
      }

      tagList(
        tags$div(style = "height: 10px;"),
        tags$h4(figures_heading),
        fluidRow(
          column(
            12,
            align = "center",
            bslib::input_task_button(
              ns("generate_report"),
              "Generate Report",
              label_busy = "Generating Report..."
            ),
            shinyjs::hidden(downloadButton(
              ns("download_report"),
              "Download Report"
            ))
          )
        ),
        tags$div(style = "height: 10px;"),
        fluidRow(
          column(
            6,
            wellPanel(
              plotOutput(ns("daily_mean_plot"))
            )
          ),
          column(
            6,
            wellPanel(
              plotOutput(ns("daily_max_plot"))
            )
          )
        ),
        fluidRow(
          column(
            6,
            wellPanel(
              plotOutput(ns("seven_day_max_avg_plot"))
            )
          )
        )
      )
    })

    # Download Handler ----
    output$download_report <- downloadHandler(
      filename = function() {
        req(report_bundle())
        report_bundle()$filename
      },

      content = function(file) {
        bundle <- report_bundle()
        req(bundle)

        if (!file.exists(bundle$zipfile)) {
          stop("Generated report archive could not be found for download.")
        }

        copied <- file.copy(bundle$zipfile, file, overwrite = TRUE)
        if (!isTRUE(copied)) {
          stop(
            "Unable to copy the generated report archive to the download location."
          )
        }

        unlink(dirname(bundle$zipfile), recursive = TRUE, force = TRUE)
        report_bundle(NULL)
      },
      contentType = "application/zip"
    )
    outputOptions(output, "download_report", suspendWhenHidden = FALSE)

    # OBSERVERS ----

    # Keep selected slots valid when table rows change
    observeEvent(
      timeseries_table_reactive(),
      {
        ts <- timeseries_table_reactive()
        slots <- selected_timeseries_slots()

        if (nrow(ts) == 0) {
          selected_timeseries_slots(NA_real_)
          active_timeseries_slot(1L)
          return()
        }

        valid_slots <- slots[!is.na(slots) & slots %in% ts$timeseries_id]
        if (length(valid_slots) == 0) {
          selected_timeseries_slots(NA_real_)
          active_timeseries_slot(1L)
          return()
        }

        selected_timeseries_slots(as.numeric(valid_slots))
        active_timeseries_slot(
          min(active_timeseries_slot(), length(valid_slots))
        )
      },
      ignoreNULL = FALSE
    )

    # Update active slot when a row is selected in the table
    observeEvent(input$timeseries_table_rows_selected, {
      ts <- timeseries_table_reactive()
      if (
        !is.null(input$timeseries_table_rows_selected) &&
          length(input$timeseries_table_rows_selected) == 1 &&
          nrow(ts) >= input$timeseries_table_rows_selected
      ) {
        selected_id <- ts$timeseries_id[input$timeseries_table_rows_selected]
        slots <- selected_timeseries_slots()
        idx <- active_timeseries_slot()

        if (is.null(idx) || is.na(idx) || idx < 1 || idx > length(slots)) {
          idx <- 1L
        }

        duplicate_idx <- which(!is.na(slots) & slots == selected_id)
        if (length(duplicate_idx) > 0 && duplicate_idx[[1]] != idx) {
          active_timeseries_slot(duplicate_idx[[1]])
        } else {
          slots[[idx]] <- as.numeric(selected_id)
          selected_timeseries_slots(as.numeric(slots))
          active_timeseries_slot(idx)
        }
      }

      # Update the date range input to reflect the active selected timeseries
      selected_id <- selected_timeseries_id_active()
      if (is.null(selected_id)) {
        return()
      }
      # updateDateRangeInput(
      #   session,
      #   "date_range",
      #   start = ts$end_date[ts$timeseries_id == selected_id] - 365,
      #   end = ts$end_date[ts$timeseries_id == selected_id],
      #   min = ts$start_date[ts$timeseries_id == selected_id],
      #   max = ts$end_date[ts$timeseries_id == selected_id]
      # )
    })

    # Render the timeseries table
    output$timeseries_table <- DT::renderDataTable({
      ts <- timeseries_table_reactive()

      column_labels <- c(
        timeseries_id = "timeseries_id",
        location = tr("loc", language$language),
        sub_location = tr("sub_loc", language$language),
        loc_code = tr("code", language$language),
        parameter = tr("parameter", language$language),
        media = tr("media", language$language),
        aggregation = tr("aggregation", language$language),
        z = tr("z", language$language),
        record_rate = tr("record_rate", language$language),
        networks = tr("network", language$language),
        projects = tr("project", language$language),
        start_date = tr("start_date", language$language),
        end_date = tr("end_date", language$language)
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

      search_cols <- vector("list", ncol(ts))
      if (!is.null(location_filter_value()) && "location" %in% names(ts)) {
        search_cols[[match("location", names(ts))]] <- list(
          search = location_filter_value()
        )
      }

      date_targets <- which(names(ts) %in% c("start_date", "end_date")) - 1
      dt <- DT::datatable(
        ts,
        rownames = FALSE,
        selection = list(mode = "single"),
        options = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 20),
          columnDefs = list(
            list(visible = FALSE, targets = 0),
            list(
              targets = date_targets,
              render = htmlwidgets::JS(
                "function(data, type, row){
                  if (!data) return '';
                  var d = new Date(data);
                  if (type === 'display') return d.toISOString().substring(0,10);
                  return data;
                }"
              )
            )
          ),
          searchCols = search_cols,
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
          order = order_columns,
          stateSave = FALSE
        ), # End options
        colnames = c("timeseries_id", col_names),
        filter = "top",
        callback = htmlwidgets::JS(
          sprintf(
            "
          var tips = {
            4: '%s',
            6: '%s'
          };
        
          var applyTips = function() {
            var $thead = $(table.table().header());
            var $rows  = $thead.find('tr');
        
            var $labelRow = $rows.filter(function() {
              return $(this).find('th').filter(function() {
                return $.trim($(this).text()).length > 0;
              }).length > 0;
            }).first();
        
            $labelRow.find('th').each(function(i) {
              if (!(i in tips)) return;
        
              var $th   = $(this);
              var $icon = $th.find('.dt-info-icon');
        
              if ($icon.length === 0) {
                $icon = $('<i class=\"fa fa-info-circle dt-info-icon\" style=\"font-size:100%%; margin-left:5px; cursor:help;\"></i>')
                  .appendTo($th);
        
                // Make it focusable (Tab) and clickable (mobile)
                $icon.attr({ tabindex: 0, role: 'button', 'aria-label': 'Info' });
              }
        
              // Bootstrap 5 tooltip attributes (title is read by BS)
              $icon.attr({
                title: tips[i],
                'data-bs-toggle': 'tooltip',
                'data-bs-trigger': 'hover focus click',
                'data-bs-placement': 'top'
              });
        
              // Initialize (or update) Bootstrap tooltip instance
              var el = $icon.get(0);
              if (window.bootstrap && bootstrap.Tooltip) {
                var inst = bootstrap.Tooltip.getInstance(el);
                if (inst) {
                  inst.setContent({ '.tooltip-inner': tips[i] });
                } else {
                  new bootstrap.Tooltip(el, { trigger: 'hover focus click', delay: { show: 0, hide: 0 } });
                }
              }
            });
          };
        
          applyTips();
          table.on('draw.dt', applyTips);
          ",
            tr("cont_table_tooltip1", language$language),
            tr("cont_table_tooltip2", language$language)
          )
        )
      )
      dt
    })

    output$selected_timeseries_output <- renderUI({
      slots <- selected_timeseries_slots()
      ts <- timeseries_table_reactive()
      selected_count <- sum(!is.na(slots))
      show_delete <- selected_count > 1

      if (length(slots) == 0) {
        return(NULL)
      }

      rows <- lapply(seq_along(slots), function(i) {
        ts_id <- slots[[i]]
        label <- tr("metadata_select_prompt", language$language)
        if (!is.na(ts_id)) {
          row_match <- ts[timeseries_id == ts_id]
          if (nrow(row_match) > 0) {
            row_match <- row_match[1]
            label_parts <- c(
              as.character(row_match$location),
              as.character(row_match$sub_location),
              as.character(row_match$parameter),
              paste0("ID: ", row_match$timeseries_id)
            )
            label_parts <- label_parts[
              !is.na(label_parts) & nzchar(label_parts)
            ]
            label <- paste(label_parts, collapse = " | ")
          } else {
            label <- paste0("ID: ", ts_id)
          }
        }

        row_style <- if (active_timeseries_slot() == i) {
          "border-left: 4px solid #0097A9; padding-left: 10px;"
        } else {
          "padding-left: 14px;"
        }

        fluidRow(
          style = paste0("margin-top: 8px; ", row_style),
          column(
            width = 10,
            tags$div(
              tags$strong(paste0("Timeseries ", i, ": ")),
              label
            )
          ),
          column(
            width = 2,
            if (show_delete && !is.na(ts_id)) {
              actionButton(
                ns(paste0("delete_timeseries_", i)),
                label = "Delete",
                class = "btn btn-outline-danger btn-sm"
              )
            }
          )
        )
      })

      do.call(tagList, rows)
    })

    proxy <- DT::dataTableProxy(ns("timeseries_table"))

    # Keep table highlight in sync with active selected timeseries slot
    observeEvent(
      list(
        selected_timeseries_slots(),
        active_timeseries_slot(),
        timeseries_table_reactive()
      ),
      {
        ts <- timeseries_table_reactive()
        id <- selected_timeseries_id_active()

        if (is.null(id) || nrow(ts) == 0) {
          DT::selectRows(proxy, NULL)
          return()
        }

        row <- which(ts$timeseries_id == id)
        if (length(row) == 1) {
          DT::selectRows(proxy, row)
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(input$entire_record, {
      ts <- timeseries_table_reactive()
      selected_id <- selected_timeseries_id_active()
      if (is.null(selected_id)) {
        return()
      }
      selected_row <- which(ts$timeseries_id == selected_id)
      if (length(selected_row) == 0) {
        return()
      }
      start_date <- ts$start_date[selected_row]
      end_date <- ts$end_date[selected_row]
      updateDateRangeInput(
        session,
        "date_range",
        start = start_date,
        end = end_date
      )
    })

    observeEvent(input$recent_year, {
      ts <- timeseries_table_reactive()
      selected_id <- selected_timeseries_id_active()
      if (is.null(selected_id)) {
        return()
      }
      selected_row <- which(ts$timeseries_id == selected_id)
      if (length(selected_row) == 0) {
        return()
      }

      end_date <- ts$end_date[selected_row]
      updateDateRangeInput(
        session,
        "highlight_range",
        start = end_date - 365,
        end = end_date
      )
    })

    # plot_request <- reactive({
    #   ts <- timeseries_table_reactive()
    #   validate(need(nrow(ts) > 0, tr("error", language$language)))
    #
    #   selected_id <- selected_timeseries_ids()
    #   selected_row <- if (!is.null(selected_id)) {
    #     which(ts$timeseries_id == selected_id)
    #   } else {
    #     NULL
    #   }
    #   validate(need(
    #     !is.null(selected_row) && length(selected_row) > 0,
    #     "Please select a row in the table before creating a plot."
    #   ))
    #
    #   req(input$date_range)
    #
    #   list(
    #     timeseries_id = ts$timeseries_id[selected_row],
    #     start_date = input$date_range[1],
    #     end_date = input$date_range[2],
    #     historic_range = input$show_hist,
    #     unusable = input$show_unusable,
    #     grades = input$show_grades,
    #     approvals = input$show_approvals,
    #     qualifiers = input$show_qualifiers,
    #     lang = language$abbrev,
    #     plot_timezone = input$plot_timezone
    #   )
    # })

    format_metadata_value <- function(value) {
      if (is.null(value) || length(value) == 0) {
        return(NA_character_)
      }
      if (is.list(value)) {
        value <- unlist(value, use.names = FALSE)
      }
      if (length(value) > 1) {
        return(paste(stats::na.omit(value), collapse = ", "))
      }
      value <- as.character(value)
      if (!is.na(value) && grepl("^\\{.*\\}$", value)) {
        value <- gsub("^\\{|\\}$", "", value)
        value <- gsub(",", ", ", value)
      }
      if (identical(value, "")) {
        return(NA_character_)
      }
      value
    }

    metadata_base_table <- function(attributes, values) {
      metadata <- data.frame(
        attribute = attributes,
        value = values,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      metadata <- metadata[!is.na(metadata$value) & metadata$value != "", ]
      metadata
    }

    selected_location_id <- reactive({
      ts_id <- selected_timeseries_id_active()
      if (is.null(ts_id)) {
        return(NULL)
      }
      location_id <- moduleData$timeseries[
        timeseries_id == ts_id,
        location_id
      ]
      if (length(location_id) == 0) {
        return(NULL)
      }
      location_id[[1]]
    })

    selected_sub_location <- reactive({
      ts_id <- selected_timeseries_id_active()
      if (is.null(ts_id)) {
        return(NA_character_)
      }
      selected_sub_location_id <- moduleData$timeseries[
        timeseries_id == ts_id,
        sub_location_id
      ]
      if (length(selected_sub_location_id) == 0) {
        return(NA_character_)
      }
      selected_sub_location_id <- selected_sub_location_id[[1]]
      if (
        is.null(selected_sub_location_id) || is.na(selected_sub_location_id)
      ) {
        return(NA_character_)
      }
      sub_loc_col <- tr("sub_location_col", language$language)
      sub_loc <- moduleData$sub_locs[
        sub_location_id == selected_sub_location_id,
        ..sub_loc_col
      ][[sub_loc_col]]
      if (length(sub_loc) == 0) {
        return(NA_character_)
      }
      sub_loc[[1]]
    })

    location_metadata <- reactive({
      req(selected_location_id())
      loc_tbl <- DBI::dbGetQuery(
        session$userData$AquaCache,
        paste0(
          "SELECT * FROM ",
          if (language$abbrev == "fr") {
            "location_metadata_fr"
          } else {
            "location_metadata_en"
          },
          " WHERE location_id = ",
          selected_location_id(),
          ";"
        )
      )
      if (nrow(loc_tbl) == 0) {
        return(NULL)
      }
      location_name <- if (language$abbrev == "fr") {
        loc_tbl$nom
      } else {
        loc_tbl$name
      }
      location_code <- if (language$abbrev == "fr") {
        loc_tbl$code_de_site
      } else {
        loc_tbl$location_code
      }
      projects <- if (language$abbrev == "fr") {
        loc_tbl$projets
      } else {
        loc_tbl$projects
      }
      networks <- if (language$abbrev == "fr") {
        loc_tbl$`réseaux`
      } else {
        loc_tbl$networks
      }
      elevation <- if (language$abbrev == "fr") {
        loc_tbl$altitude
      } else {
        loc_tbl$elevation
      }
      metadata_base_table(
        attributes = c(
          tr("loc", language$language),
          tr("location_code_label", language$language),
          tr("sub_loc", language$language),
          tr("latitude", language$language),
          tr("longitude", language$language),
          tr("elevation_m", language$language),
          tr("datum", language$language),
          tr("projects", language$language),
          tr("networks", language$language)
        ),
        values = c(
          format_metadata_value(location_name),
          format_metadata_value(location_code),
          format_metadata_value(selected_sub_location()),
          format_metadata_value(loc_tbl$latitude),
          format_metadata_value(loc_tbl$longitude),
          format_metadata_value(elevation),
          format_metadata_value(loc_tbl$datum),
          format_metadata_value(projects),
          format_metadata_value(networks)
        )
      )
    })

    timeseries_metadata <- reactive({
      ts_id <- selected_timeseries_id_active()
      if (is.null(ts_id)) {
        return(NULL)
      }

      ts_tbl <- DBI::dbGetQuery(
        session$userData$AquaCache,
        paste0(
          "SELECT * FROM ",
          if (language$abbrev == "fr") {
            "continuous.timeseries_metadata_fr"
          } else {
            "continuous.timeseries_metadata_en"
          },
          " WHERE timeseries_id = ",
          ts_id,
          ";"
        )
      )

      if (nrow(ts_tbl) == 0) {
        return(NULL)
      }

      if (language$abbrev == "fr") {
        parameter <- ts_tbl$`nom_paramètre`
        media <- ts_tbl$`type_de_média`
        aggregation <- ts_tbl$`type_agrégation`
        record_rate <- ts_tbl$`fréquence_enregistrement`
        depth_height <- ts_tbl$`profondeur_hauteur_m`
        start_dt <- ts_tbl$`début`
        end_dt <- ts_tbl$fin
      } else {
        parameter <- ts_tbl$parameter_name
        media <- ts_tbl$media_type
        aggregation <- ts_tbl$aggregation_type
        record_rate <- ts_tbl$recording_rate
        depth_height <- ts_tbl$depth_height_m
        start_dt <- ts_tbl$start_datetime
        end_dt <- ts_tbl$end_datetime
      }
      record_rate_seconds <- suppressWarnings(as.numeric(record_rate))
      record_rate_display <- if (!is.na(record_rate_seconds)) {
        as.character(lubridate::seconds_to_period(record_rate_seconds))
      } else {
        format_metadata_value(record_rate)
      }
      metadata_base_table(
        attributes = c(
          tr("parameter", language$language),
          tr("media", language$language),
          tr("aggregation", language$language),
          tr("nominal_rate", language$language),
          tr("depth_height_m", language$language),
          tr("start_date", language$language),
          tr("end_date", language$language),
          tr("note", language$language)
        ),
        values = c(
          format_metadata_value(parameter),
          format_metadata_value(media),
          format_metadata_value(aggregation),
          format_metadata_value(record_rate_display),
          format_metadata_value(depth_height),
          format_metadata_value(start_dt),
          format_metadata_value(end_dt),
          format_metadata_value(ts_tbl$note)
        )
      )
    })

    timeseries_ownership <- reactive({
      ts_id <- selected_timeseries_id_active()
      if (is.null(ts_id)) {
        return(NULL)
      }
      ownership_tbl <- DBI::dbGetQuery(
        session$userData$AquaCache,
        paste0(
          "SELECT o.start_dt, o.end_dt, ",
          if (language$abbrev == "fr") {
            "COALESCE(org.name_fr, org.name) AS owner"
          } else {
            "org.name AS owner"
          },
          " FROM continuous.owners o ",
          "JOIN public.organizations org ",
          "ON o.organization_id = org.organization_id ",
          "WHERE o.timeseries_id = ",
          ts_id,
          " ORDER BY o.start_dt;"
        )
      )
      if (nrow(ownership_tbl) == 0) {
        return(NULL)
      }
      ownership_tbl
    })

    plot_bundle <- reactiveVal(NULL)
    report_bundle <- reactiveVal(NULL)

    show_validation_modal <- function(title, messages) {
      messages <- unique(messages[!is.na(messages) & nzchar(messages)])
      if (!length(messages)) {
        return(invisible(FALSE))
      }

      showModal(modalDialog(
        title = title,
        tags$p("Please correct the following before continuing:"),
        tags$ul(lapply(messages, function(msg) tags$li(msg))),
        easyClose = TRUE,
        footer = modalButton(tr("close", language$language))
      ))

      invisible(TRUE)
    }

    validate_plot_request <- function() {
      issues <- character()

      if (length(selected_timeseries_ids()) == 0) {
        issues <- c(
          issues,
          "Select a water temperature timeseries from the table before creating report figures."
        )
      }

      date_range <- as.Date(input$date_range)
      if (
        length(date_range) != 2 ||
          any(is.na(date_range)) ||
          date_range[[1]] > date_range[[2]]
      ) {
        issues <- c(issues, "Provide a valid report date range.")
      }

      highlight_range <- as.Date(input$highlight_range)
      if (
        length(highlight_range) != 2 ||
          any(is.na(highlight_range)) ||
          highlight_range[[1]] > highlight_range[[2]]
      ) {
        issues <- c(issues, "Provide a valid highlight date range.")
      }

      if (
        length(date_range) == 2 &&
          !any(is.na(date_range)) &&
          length(highlight_range) == 2 &&
          !any(is.na(highlight_range)) &&
          (highlight_range[[1]] < date_range[[1]] ||
            highlight_range[[2]] > date_range[[2]])
      ) {
        issues <- c(
          issues,
          "The highlight date range must fall within the report date range."
        )
      }

      unique(issues)
    }

    build_report_filename <- function(site_name) {
      site_name <- as.character(site_name)
      if (
        length(site_name) == 0 ||
          is.na(site_name[[1]]) ||
          !nzchar(site_name[[1]])
      ) {
        site_name <- "Selected site"
      } else {
        site_name <- site_name[[1]]
      }
      site_name <- gsub("[\\\\/:*?\"<>|]+", " ", site_name)
      site_name <- gsub("\\s+", " ", site_name)
      paste0(
        trimws(site_name),
        " Temperature Report Bundle ",
        Sys.Date(),
        ".zip"
      )
    }

    cleanup_report_bundle <- function(bundle) {
      if (is.null(bundle) || is.null(bundle$zipfile)) {
        return(invisible(NULL))
      }
      bundle_dir <- dirname(bundle$zipfile)
      if (dir.exists(bundle_dir)) {
        unlink(bundle_dir, recursive = TRUE, force = TRUE)
      }
      invisible(NULL)
    }

    complete_fake_dates <- function(data) {
      if (nrow(data) == 0) {
        return(data)
      }
      tidyr::complete(
        data,
        fake_date = seq(
          min(data$fake_date, na.rm = TRUE),
          max(data$fake_date, na.rm = TRUE),
          by = "day"
        )
      )
    }

    safe_min <- function(x) {
      if (all(is.na(x))) {
        return(NA_real_)
      }
      min(x, na.rm = TRUE)
    }

    safe_max <- function(x) {
      if (all(is.na(x))) {
        return(NA_real_)
      }
      max(x, na.rm = TRUE)
    }

    safe_mean <- function(x) {
      if (all(is.na(x))) {
        return(NA_real_)
      }
      mean(x, na.rm = TRUE)
    }

    summarise_fake_date <- function(data, value_col) {
      if (nrow(data) == 0) {
        return(data.frame(
          fake_date = as.Date(character()),
          tmin = numeric(),
          tmax = numeric(),
          tmean = numeric()
        ))
      }

      out <- data |>
        dplyr::group_by(fake_date) |>
        dplyr::summarise(
          tmin = safe_min(.data[[value_col]]),
          tmax = safe_max(.data[[value_col]]),
          tmean = safe_mean(.data[[value_col]]),
          .groups = "drop"
        )

      complete_fake_dates(out)
    }

    add_threshold_lines <- function(plot_obj, thresholds) {
      thresholds <- as.character(thresholds)
      threshold_cols <- c(
        "20" = "#8b0000",
        "18" = "#fe0100",
        "13" = "#ffa405"
      )

      for (threshold in names(threshold_cols)) {
        if (threshold %in% thresholds) {
          plot_obj <- plot_obj +
            ggplot2::geom_hline(
              yintercept = as.numeric(threshold),
              color = threshold_cols[[threshold]],
              linetype = "dashed"
            )
        }
      }

      plot_obj
    }

    build_water_temp_plot_bundle <- function(
      ts_data,
      highlight_range,
      thresholds,
      language_abbrev
    ) {
      if (is.null(ts_data) || nrow(ts_data) == 0) {
        stop(
          "No temperature data found for the selected timeseries and date range."
        )
      }

      highlight_range <- as.Date(highlight_range)
      if (length(highlight_range) != 2 || any(is.na(highlight_range))) {
        stop("Please provide a valid highlight date range.")
      }

      site_col <- if (
        identical(language_abbrev, "fr") &&
          "name_fr" %in% names(ts_data)
      ) {
        "name_fr"
      } else {
        "name"
      }

      display_name <- as.character(ts_data[[site_col]])
      if ("name" %in% names(ts_data)) {
        fallback_name <- as.character(ts_data[["name"]])
        display_name[is.na(display_name) | !nzchar(display_name)] <-
          fallback_name[is.na(display_name) | !nzchar(display_name)]
      }

      ts_data <- ts_data |>
        dplyr::mutate(
          datetime = as.POSIXct(datetime, tz = "UTC"),
          value_corrected = as.numeric(value_corrected),
          name = display_name
        )

      daily_stats <- ts_data |>
        dplyr::mutate(date = as.Date(datetime)) |>
        dplyr::group_by(date, name) |>
        dplyr::summarise(
          daily_mean = safe_mean(value_corrected),
          daily_max = safe_max(value_corrected),
          daily_min = safe_min(value_corrected),
          .groups = "drop"
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          fake_date = as.Date(
            paste0(
              "1996-",
              lubridate::month(date),
              "-",
              lubridate::day(date)
            ),
            "%Y-%m-%d"
          ),
          seven_day_max_avg = zoo::rollmean(
            daily_max,
            7,
            align = "right",
            na.pad = TRUE
          )
        )

      site_name <- stats::na.omit(unique(as.character(daily_stats$name)))
      if (length(site_name) == 0) {
        site_name <- "Selected site"
      } else {
        site_name <- site_name[[1]]
      }

      historic <- daily_stats |>
        dplyr::filter(
          date < highlight_range[[1]] | date > highlight_range[[2]]
        )
      historic_mean <- summarise_fake_date(historic, "daily_mean")
      historic_max <- summarise_fake_date(historic, "daily_max")
      historic_seven_day <- summarise_fake_date(historic, "seven_day_max_avg")

      highlight <- daily_stats |>
        dplyr::filter(
          date >= highlight_range[[1]] & date <= highlight_range[[2]]
        )
      highlight <- complete_fake_dates(highlight)

      daily_mean_plot <- ggplot2::ggplot() +
        ggplot2::geom_ribbon(
          data = historic_mean,
          ggplot2::aes(x = fake_date, ymin = tmin, ymax = tmax),
          fill = "grey80"
        ) +
        ggplot2::geom_line(
          data = historic_mean,
          ggplot2::aes(x = fake_date, y = tmean),
          color = "darkgrey",
          linewidth = 0.7
        ) +
        ggplot2::geom_line(
          data = highlight,
          ggplot2::aes(x = fake_date, y = daily_mean),
          color = "blue",
          linewidth = 1.2
        ) +
        ggplot2::labs(
          title = paste0(site_name, "\n - Daily Mean Temperature"),
          x = "Date",
          y = "Temperature (°C)"
        ) +
        ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 45,
            hjust = 1,
            size = 13
          ),
          axis.text.y = ggplot2::element_text(size = 13),
          axis.title = ggplot2::element_text(size = 16),
          plot.title = ggplot2::element_text(size = 18),
          axis.line = ggplot2::element_line()
        )
      daily_mean_plot <- add_threshold_lines(daily_mean_plot, thresholds)

      daily_max_plot <- ggplot2::ggplot() +
        ggplot2::geom_ribbon(
          data = historic_max,
          ggplot2::aes(x = fake_date, ymin = tmin, ymax = tmax),
          fill = "grey80"
        ) +
        ggplot2::geom_line(
          data = historic_max,
          ggplot2::aes(x = fake_date, y = tmean),
          color = "darkgrey",
          linewidth = 0.7
        ) +
        ggplot2::geom_line(
          data = highlight,
          ggplot2::aes(x = fake_date, y = daily_max),
          color = "blue",
          linewidth = 1.2
        ) +
        ggplot2::labs(
          title = paste0(site_name, "\n - Daily Max Temperature"),
          x = "Date",
          y = "Temperature (°C)"
        ) +
        ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 45,
            hjust = 1,
            size = 13
          ),
          axis.text.y = ggplot2::element_text(size = 13),
          axis.title = ggplot2::element_text(size = 16),
          plot.title = ggplot2::element_text(size = 18),
          axis.line = ggplot2::element_line()
        )
      daily_max_plot <- add_threshold_lines(daily_max_plot, thresholds)

      seven_day_max_plot <- ggplot2::ggplot() +
        ggplot2::geom_ribbon(
          data = historic_seven_day,
          ggplot2::aes(x = fake_date, ymin = tmin, ymax = tmax),
          fill = "grey80"
        ) +
        ggplot2::geom_line(
          data = historic_seven_day,
          ggplot2::aes(x = fake_date, y = tmean),
          color = "darkgrey",
          linewidth = 0.7
        ) +
        ggplot2::geom_line(
          data = highlight,
          ggplot2::aes(x = fake_date, y = seven_day_max_avg),
          color = "blue",
          linewidth = 1.2
        ) +
        ggplot2::labs(
          title = paste0(
            site_name,
            "\n - 7-Day Rolling Avg of Max Temperature"
          ),
          x = "Date",
          y = "Temperature (°C)"
        ) +
        ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 45,
            hjust = 1,
            size = 13
          ),
          axis.text.y = ggplot2::element_text(size = 13),
          axis.title = ggplot2::element_text(size = 16),
          plot.title = ggplot2::element_text(size = 18),
          axis.line = ggplot2::element_line()
        )
      seven_day_max_plot <- add_threshold_lines(seven_day_max_plot, thresholds)

      list(
        raw_ts = ts_data,
        daily_stats = daily_stats,
        site_name = site_name,
        highlight_range = highlight_range,
        daily_mean_plot = daily_mean_plot,
        daily_max_plot = daily_max_plot,
        seven_day_max_plot = seven_day_max_plot
      )
    }

    plot_figures_task <- ExtendedTask$new(function(req, config) {
      promises::future_promise({
        tryCatch(
          {
            con <- AquaConnect(
              name = config$dbName,
              host = config$dbHost,
              port = config$dbPort,
              username = config$dbUser,
              password = config$dbPass,
              silent = TRUE
            )
            on.exit(DBI::dbDisconnect(con), add = TRUE)

            timeseries_ids <- as.numeric(req$timeseries_ids)
            timeseries_ids <- timeseries_ids[!is.na(timeseries_ids)]
            if (length(timeseries_ids) == 0) {
              stop("Please select a timeseries before creating report figures.")
            }

            date_range <- as.Date(req$date_range)
            if (
              length(date_range) != 2 ||
                any(is.na(date_range)) ||
                date_range[[1]] > date_range[[2]]
            ) {
              stop("Please provide a valid date range.")
            }

            ts_data <- DBI::dbGetQuery(
              con,
              sprintf(
                paste0(
                  "SELECT datetime, ",
                  "measurements_continuous_corrected.value_corrected, ",
                  "locations.name, locations.name_fr ",
                  "FROM measurements_continuous_corrected ",
                  "JOIN continuous.timeseries ",
                  "ON measurements_continuous_corrected.timeseries_id = ",
                  "continuous.timeseries.timeseries_id ",
                  "JOIN locations ",
                  "ON timeseries.location_id = locations.location_id ",
                  "WHERE measurements_continuous_corrected.timeseries_id IN (%s) ",
                  "AND datetime >= $1 AND datetime < $2 ",
                  "ORDER BY datetime;"
                ),
                paste(timeseries_ids, collapse = ", ")
              ),
              params = list(
                as.POSIXct(date_range[[1]], tz = "UTC"),
                as.POSIXct(date_range[[2]] + 1, tz = "UTC")
              )
            )

            build_water_temp_plot_bundle(
              ts_data = ts_data,
              highlight_range = req$highlight_range,
              thresholds = req$thresholds_select,
              language_abbrev = req$lang
            )
          },
          error = function(e) {
            e$message
          }
        )
      })
    }) |>
      bind_task_button("make_plot")

    report_task <- ExtendedTask$new(function(plot_data, highlight_range) {
      promises::future_promise({
        tryCatch(
          {
            report_template <- system.file(
              "rmd",
              "water_temperature_report.Rmd",
              package = "YGwater"
            )
            if (!nzchar(report_template)) {
              stop("Water temperature report template not found.")
            }

            highlight_range <- as.Date(highlight_range)
            if (length(highlight_range) != 2 || any(is.na(highlight_range))) {
              stop("Please provide a valid highlight date range.")
            }

            work_dir <- tempfile("water_temp_report_")
            dir.create(work_dir, recursive = TRUE)
            bundle_dir <- file.path(work_dir, "bundle")
            dir.create(bundle_dir, recursive = TRUE)
            zip_path <- file.path(work_dir, "report.zip")
            render_template <- file.path(work_dir, basename(report_template))

            copied_template <- file.copy(
              report_template,
              render_template,
              overwrite = TRUE
            )
            if (!isTRUE(copied_template)) {
              stop(
                "Unable to stage the water temperature report template in the temporary workspace."
              )
            }

            ggplot2::ggsave(
              filename = file.path(bundle_dir, "daily_mean_plot.png"),
              plot = plot_data$daily_mean_plot,
              height = 8,
              width = 8
            )
            ggplot2::ggsave(
              filename = file.path(bundle_dir, "daily_max_plot.png"),
              plot = plot_data$daily_max_plot,
              height = 8,
              width = 8
            )
            ggplot2::ggsave(
              filename = file.path(bundle_dir, "seven_day_avg_max_plot.png"),
              plot = plot_data$seven_day_max_plot,
              height = 8,
              width = 8
            )

            saveRDS(
              plot_data$daily_mean_plot,
              file.path(bundle_dir, "daily_mean_plot.rds")
            )
            saveRDS(
              plot_data$daily_max_plot,
              file.path(bundle_dir, "daily_max_plot.rds")
            )
            saveRDS(
              plot_data$seven_day_max_plot,
              file.path(bundle_dir, "seven_day_avg_max_plot.rds")
            )

            utils::write.csv(
              plot_data$daily_stats,
              file.path(bundle_dir, "daily_summary.csv"),
              row.names = FALSE
            )

            params <- list(
              daily_mean_plot = plot_data$daily_mean_plot,
              daily_max_plot = plot_data$daily_max_plot,
              seven_day_max_plot = plot_data$seven_day_max_plot,
              all_ts = plot_data$raw_ts,
              site_name = plot_data$site_name,
              highlight_range = highlight_range
            )

            rmarkdown::render(
              input = render_template,
              output_file = "report.html",
              output_dir = bundle_dir,
              intermediates_dir = work_dir,
              knit_root_dir = work_dir,
              params = params,
              envir = new.env(parent = globalenv()),
              quiet = TRUE
            )

            bundle_files <- list.files(bundle_dir, full.names = FALSE)
            if (!length(bundle_files)) {
              stop("No report files were generated.")
            }

            zip::zip(
              zipfile = zip_path,
              files = bundle_files,
              mode = "cherry-pick",
              include_directories = FALSE,
              root = bundle_dir
            )

            list(
              zipfile = zip_path,
              filename = build_report_filename(plot_data$site_name)
            )
          },
          error = function(e) {
            e$message
          }
        )
      })
    }) |>
      bind_task_button("generate_report")

    # output renders ----

    output$daily_mean_plot <- renderPlot({
      req(plot_bundle())
      plot_bundle()$daily_mean_plot
    })

    output$daily_max_plot <- renderPlot({
      req(plot_bundle())
      plot_bundle()$daily_max_plot
    })

    output$seven_day_max_avg_plot <- renderPlot({
      req(plot_bundle())
      plot_bundle()$seven_day_max_plot
    })

    selected_timeseries_ids <- reactive({
      timeseries_table_reactive()$timeseries_id[
        input$timeseries_table_rows_selected
      ]
    })

    observeEvent(input$make_plot, {
      if (
        show_validation_modal(
          title = "Cannot Create Water Temperature Figures",
          messages = validate_plot_request()
        )
      ) {
        return()
      }

      cleanup_report_bundle(report_bundle())
      report_bundle(NULL)
      plot_bundle(NULL)

      plot_figures_task$invoke(
        req = list(
          timeseries_ids = selected_timeseries_ids(),
          date_range = as.Date(input$date_range),
          highlight_range = as.Date(input$highlight_range),
          thresholds_select = as.character(input$thresholds_select),
          lang = language$abbrev
        ),
        config = session$userData$config
      )
    })

    observeEvent(plot_figures_task$result(), {
      result <- plot_figures_task$result()

      if (inherits(result, "character")) {
        showNotification(
          paste("Error generating water temperature plots:", result),
          type = "error",
          duration = NULL,
          closeButton = TRUE
        )
        return()
      }

      cleanup_report_bundle(report_bundle())
      report_bundle(NULL)
      plot_bundle(result)
    })

    observeEvent(input$generate_report, {
      req(plot_bundle())

      report_task$invoke(
        plot_data = plot_bundle(),
        highlight_range = plot_bundle()$highlight_range
      )
    })

    observeEvent(report_task$result(), {
      result <- report_task$result()

      if (inherits(result, "character")) {
        showNotification(
          paste("Error generating water temperature report:", result),
          type = "error",
          duration = NULL,
          closeButton = TRUE
        )
        return()
      }

      if (is.null(result$zipfile) || !file.exists(result$zipfile)) {
        showNotification(
          "Report was generated, but the zip archive could not be found for download.",
          type = "error",
          duration = NULL,
          closeButton = TRUE
        )
        return()
      }

      cleanup_report_bundle(report_bundle())
      report_bundle(result)
      shinyjs::click("download_report")
    })

    output$metadata_message <- renderUI({
      if (is.null(selected_timeseries_id_active())) {
        return(
          tags$em(tr("metadata_select_prompt", language$language))
        )
      }
      NULL
    })

    output$location_metadata_table <- DT::renderDataTable({
      metadata <- location_metadata()
      req(metadata)
      DT::datatable(
        metadata,
        rownames = FALSE,
        selection = "none",
        options = list(
          dom = "t",
          pageLength = 10,
          ordering = FALSE,
          searching = FALSE,
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
          )
        ),
        colnames = c(
          tr("attribute", language$language),
          tr("value", language$language)
        )
      )
    })

    output$timeseries_metadata_table <- DT::renderDataTable({
      metadata <- timeseries_metadata()
      req(metadata)
      DT::datatable(
        metadata,
        rownames = FALSE,
        selection = "none",
        options = list(
          dom = "t",
          pageLength = 10,
          ordering = FALSE,
          searching = FALSE,
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
          )
        ),
        colnames = c(
          tr("attribute", language$language),
          tr("value", language$language)
        )
      )
    })

    output$timeseries_ownership_table <- DT::renderDataTable({
      ownership <- timeseries_ownership()
      date_targets <- c(0, 1)
      req(ownership)
      DT::datatable(
        ownership,
        rownames = FALSE,
        selection = "none",
        options = list(
          paging = TRUE,
          pageLength = 5,
          ordering = TRUE,
          searching = FALSE,
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
          )
        ),
        colnames = c(
          tr("start_date", language$language),
          tr("end_date", language$language),
          tr("owner", language$language)
        )
      )
    })
  }) # End moduleServer
}
