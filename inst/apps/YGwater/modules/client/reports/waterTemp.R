
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


waterTempMod <- function(id, language, windowDims, inputs){
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
      
      ts <- ts[ts$parameter == "temperature, water"]
      
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
      
      empty_cols <- setdiff(
        names(ts)[vapply(ts, is_empty_col, logical(1))],
        "timeseries_id"
      )
      if (length(empty_cols) > 0) {
        ts[, (empty_cols) := NULL]
      }
      
      data.table::setorder(ts, location, parameter)
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
      req(moduleData, language$language)
      network_col <- tr("generic_name_col", language$language)
      project_col <- tr("generic_name_col", language$language)
      param_grp_col <- tr("param_group_col", language$language)
      
      tags <- tagList(
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
                ),
                hr()
              ),
              column(
                width = 6,
                selectizeInput(ns("thresholds_select"), label = "Thresholds", choices = c("20 °C" = 20, "18 °C" = 18, "13 °C" = 13),
                               multiple = TRUE)
              )
            ),
            fluidRow(
              column(12, align = "center",
                     actionButton(
                       ns("make_plot"),
                       label = "Create Report Figures"
                     )
              )
              
            ),
            fluidRow(
              column(6,
                     wellPanel(
                       plotOutput(ns("daily_mean_plot"))
                     ) 
              ),
              column(6,
                     wellPanel(
                       plotOutput(ns("daily_max_plot"))
                     )
              )
            ),
            fluidRow(
              column(6,
                     wellPanel(
                       plotOutput(ns("seven_day_max_avg_plot"))
                     )
              )
            )
          ), # End plot_options_panel
          fluidRow(
            column(12, hr()),
            column(12, align = "center",
                   uiOutput(ns("generate_report_ui")))
          )
        ), # End accordion 1
        tags$div(style = "height: 10px;"),
        plotly::plotlyOutput(ns("plot"), height = "600px", inline = TRUE),
        uiOutput(ns("full_screen_ui")),
        
        # Space so the table and plot aren't in each other's faces
        tags$div(style = "height: 15px;"),
        
        # Start accordion 2
        bslib::accordion(
          id = ns("accordion2"),
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
    
    output$generate_report_ui <- renderUI({
      validate(need(!is.null(save_plots$daily_mean_plot), "Create plots first to export report"))
      downloadButton(ns("generate_report"), "Generate Report")
    })
    
    # Download Handler ----
    output$generate_report <- downloadHandler(
      filename = function() {
        paste0("temperature_report_bundle", Sys.Date(), ".zip")
      },
      
      content = function(file) {
        report_template <- system.file("rmd", "water_temperature_report.Rmd", package = "YGwater")
        
        # Create temporary directory to store report directory to be zipped
        tmpdir <- tempdir()
        bundle_dir <- file.path(tmpdir, paste0("report_", Sys.time()))
        dir.create(bundle_dir)
        
        withProgress({
          # Save Figures as .pngs
          ggplot2::ggsave(paste0(bundle_dir, "/daily_mean_plot.png"), save_plots$daily_mean_plot, height = 8, width = 8)
          incProgress(1/10)
          ggplot2::ggsave(paste0(bundle_dir, "/daily_max_plot.png"), save_plots$daily_max_plot, height = 8, width = 8)
          incProgress(2/10)
          ggplot2::ggsave(paste0(bundle_dir, "/seven_day_avg_max_plot.png"), save_plots$seven_day_max_plot, height = 8, width = 8)
          incProgress(3/10)
          
          # Save .rds objects of plots for future user modification
          saveRDS(save_plots$daily_mean_plot, paste0(bundle_dir, "/daily_mean_plot.rds"))
          incProgress(4/10)
          saveRDS(save_plots$daily_max_plot, paste0(bundle_dir, "/daily_max_plot.rds"))
          incProgress(5/10)
          saveRDS(save_plots$seven_day_max_plot, paste0(bundle_dir, "/seven_day_avg_max_plot.rds"))
          incProgress(6/10)
          
          
          # Store parameters to R markdown as list
          params <- list(
            daily_mean_plot = save_plots$daily_mean_plot,
            daily_max_plot = save_plots$daily_max_plot,
            seven_day_max_plot = save_plots$seven_day_max_plot,
            all_ts = raw_ts(),
            site_name = unique(historic()$name)
          )
          
          
          # Render R markdown report
          rmarkdown::render(
            report_template,
            output_file = paste0(bundle_dir, "/report.html"),
            params = params,
            envir = new.env(parent = globalenv())
          )
        },
        message = "Generating report...")
        

        
        # Store files to be zipped
        files <- list.files(bundle_dir)
        
        # Zip file
        zip::zip(zipfile = file,
                 files = files, root = bundle_dir)
        
        
      }
    )
    
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
    
    raw_ts <- reactiveVal(NULL)
    
    highlight_cutoff <- reactiveVal(c(as.Date("2025-01-01"), as.Date("2025-12-31")))
    threshold <- reactiveVal(18)
    
    # Output container to hold plots as they update such that we may write them out easily
    save_plots <- reactiveValues(
      daily_mean_plot = NULL,
      daily_max_plot = NULL,
      seven_day_max_plot = NULL
    )
    
    # plot data reactives----
    
    ## historic ----
    historic <- reactive({
      if(is.null(daily_stats())){
        return(NULL)
      }
      
      out <- daily_stats() |> 
        dplyr::filter(date < input$highlight_range[1] | date > input$highlight_range[2])
      
    })
    
    ## historic_max ----
    historic_max <- reactive({
      if(is.null(historic())){
        return(NULL)
      }
      
      out <- historic() |> 
        dplyr::group_by(fake_date) |> 
        dplyr::summarise(tmin = min(daily_max, na.rm = TRUE),
                         tmax = max(daily_max, na.rm = TRUE),
                         tmean = mean(daily_max, na.rm = TRUE),
                         .groups = "drop")
      
      # If data exists, pad out gaps in dates with NAs, otherwise return the empty data.frame
      if(nrow(out) > 0){
        return(tidyr::complete(out, fake_date = seq(min(fake_date, na.rm = TRUE), max(fake_date, na.rm = TRUE), by = "day")))
      } else {
        return(out)
      }
      
    })
    
    ## historic_mean ----
    historic_mean <- reactive({
      if(is.null(historic())){
        return(NULL)
      }
      
      out <- historic() |> 
        dplyr::group_by(fake_date) |> 
        dplyr::summarise(tmin = min(daily_mean, na.rm = TRUE),
                         tmax = max(daily_mean, na.rm = TRUE),
                         tmean = mean(daily_mean, na.rm = TRUE),
                         .groups = "drop")
      
      if(nrow(out) > 0){
        return(tidyr::complete(out, fake_date = seq(min(fake_date, na.rm = TRUE), max(fake_date, na.rm = TRUE), by = "day")))
      } else {
        return(out)
      }
    })
    
    ## historic_seven_day ----
    historic_seven_day <- reactive({
      if(is.null(historic())){
        return(NULL)
      }
      
      out <- historic() |> 
        dplyr::group_by(fake_date) |> 
        dplyr::summarise(tmin = min(seven_day_max_avg, na.rm = TRUE),
                         tmax = max(seven_day_max_avg, na.rm = TRUE),
                         tmean = mean(seven_day_max_avg, na.rm = TRUE),
                         .groups = "drop")
      
      if(nrow(out) > 0){
        return(tidyr::complete(out, fake_date = seq(min(fake_date, na.rm = TRUE), max(fake_date, na.rm = TRUE), by = "day")))
      } else {
        return(out)
      }
    })
    
    ## highlight ----
    highlight <- reactive({
      if(is.null(daily_stats())){
        return(NULL)
      }
      
      out <- daily_stats() |> 
        dplyr::filter(date >= input$highlight_range[1] & date <= input$highlight_range[2]) #|> 
      
      # If data exists, pad out gaps in dates with NAs, otherwise return the empty data.frame
      if(nrow(out) > 0){
        return(tidyr::complete(out, fake_date = seq(min(fake_date, na.rm = TRUE), max(fake_date, na.rm = TRUE), by = "day")))
      } else {
        return(out)
      }
      
    })
    
    
    
    ## daily_stats----
    daily_stats <- reactive({
      if(is.null(raw_ts())){
        return(NULL)
      }
      
      out <- raw_ts() |> 
        dplyr::mutate(date = as.Date(datetime)) |> 
        dplyr::group_by(date, name) |> 
        dplyr::summarise(daily_mean = mean(value_corrected, na.rm = TRUE),
                         daily_max = max(value_corrected, na.rm = TRUE),
                         daily_min = min(value_corrected, na.rm = TRUE)) |> 
        dplyr::ungroup() |> 
        dplyr::mutate(fake_date = as.Date(paste0("1996-", lubridate::month(date), "-", lubridate::day(date)), "%Y-%m-%d"),
                      seven_day_max_avg = zoo::rollmean(daily_max, 7, align = "right", na.pad = TRUE))
      return(out)
    })
    
    # seven_day_stats <- reactive({
    #   if(is.null(daily_stats())){
    #     return(NULL)
    #   }
    #   
    # out <- daily_stats() |> 
    #   zoo::
    #     
    #   
    # })
    
    # output renders ----
    
    
    ## daily_mean_plot ----
    output$daily_mean_plot <- renderPlot({
      validate(need(!is.null(daily_stats()), ''))
      
      library(ggplot2)
      
      out <- ggplot() +
        geom_ribbon(data = historic_mean(), 
                    aes(x = fake_date, ymin = tmin, ymax = tmax), 
                    fill = "grey80") +
        
        # Historical mean line
        geom_line(data = historic_mean(),
                  aes(x = fake_date, y = tmean),
                  color = "darkgrey",
                  size = 0.7) +
        
        # Highlight year line
        geom_line(data = highlight(), aes(x = fake_date, y = daily_mean),
                  color = "blue",
                  size = 1.2) +
        
        labs(
          title = paste0(unique(historic()$name), "\n"," - Mean Temperature"),
          x = "Date",
          y = "Temperature (°C)"
        ) +
        
        scale_x_date(date_labels = "%b", date_breaks = "1 month") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
              axis.text.y = element_text(size = 13),
              axis.title = element_text(size = 16),
              plot.title = element_text(size = 18))
      
      if("20" %in% input$thresholds_select){
        out <- out +
          geom_hline(aes(yintercept = 20), color = "#8b0000",
                     linetype = "dashed")
      }
      if("18" %in% input$thresholds_select){
        out <- out + 
          geom_hline(aes(yintercept = 18), color = "#fe0100",
                     linetype = "dashed")
      }
      if("13" %in% input$thresholds_select){
        out <- out + 
          geom_hline(aes(yintercept = 13), color = "#ffa405",
                     linetype = "dashed")
      }
      # Store plot to be written out later
      save_plots$daily_mean_plot <- out
      
      return(out)
      
    })
    
    ## daily_max_plot -----
    output$daily_max_plot <- renderPlot({
      validate(need(!is.null(daily_stats()), ''))
      
      out <- ggplot() +
        geom_ribbon(data = historic_max(), 
                    aes(x = fake_date, ymin = tmin, ymax = tmax), 
                    fill = "grey80") +
        
        # Historical mean line
        geom_line(data = historic_max(),
                  aes(x = fake_date, y = tmean),
                  color = "darkgrey",
                  size = 0.7) +
        
        # Highlight year line
        geom_line(data = highlight(), aes(x = fake_date, y = daily_max),
                  color = "blue",
                  size = 1.2) +
        labs(
          title = paste0(unique(historic()$name), "\n"," - Max Temperature"),
          x = "Date",
          y = "Temperature (°C)"
        ) +
        
        scale_x_date(date_labels = "%b", date_breaks = "1 month") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
              axis.text.y = element_text(size = 13),
              axis.title = element_text(size = 16),
              plot.title = element_text(size = 18))
      
      if("20" %in% input$thresholds_select){
        out <- out +
          geom_hline(aes(yintercept = 20), color = "#8b0000",
                     linetype = "dashed")
      }
      if("18" %in% input$thresholds_select){
        out <- out + 
          geom_hline(aes(yintercept = 18), color = "#fe0100",
                     linetype = "dashed")
      }
      if("13" %in% input$thresholds_select){
        out <- out + 
          geom_hline(aes(yintercept = 13), color = "#ffa405",
                     linetype = "dashed")
      }
      # Store plot to be written out later
      save_plots$daily_max_plot <- out
      
      return(out)
      
    })
    
    ## seven_day_max_avg_plot ----
    output$seven_day_max_avg_plot <- renderPlot({
      validate(need(!is.null(daily_stats()), ''))
      
      out <- ggplot() +
        geom_ribbon(data = historic_seven_day(), 
                    aes(x = fake_date, ymin = tmin, ymax = tmax), 
                    fill = "grey80") +
        
        # Historical mean line
        geom_line(data = historic_seven_day(),
                  aes(x = fake_date, y = tmean),
                  color = "darkgrey",
                  size = 0.7) +
        
        # Highlight year line
        geom_line(data = highlight(), aes(x = fake_date, y = seven_day_max_avg),
                  color = "blue",
                  size = 1.2) +
        labs(
          title = paste0(unique(historic()$name), "\n"," - 7-Day Rolling Avg of Max Temperature"),
          x = "Date",
          y = "Temperature (°C)"
        ) +
        
        scale_x_date(date_labels = "%b", date_breaks = "1 month") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
              axis.text.y = element_text(size = 13),
              axis.title = element_text(size = 16),
              plot.title = element_text(size = 18))
      
      if("20" %in% input$thresholds_select){
        out <- out +
          geom_hline(aes(yintercept = 20), color = "#8b0000",
                     linetype = "dashed")
      }
      if("18" %in% input$thresholds_select){
        out <- out + 
          geom_hline(aes(yintercept = 18), color = "#fe0100",
                     linetype = "dashed")
      }
      if("13" %in% input$thresholds_select){
        out <- out + 
          geom_hline(aes(yintercept = 13), color = "#ffa405",
                     linetype = "dashed")
      }
      
      # Store plot to be written out later
      save_plots$seven_day_max_plot <- out
      
      return(out)
    })
    
    
    selected_timeseries_ids <- reactive({
      timeseries_table_reactive()$timeseries_id[input$timeseries_table_rows_selected]
    })
    
    observeEvent(input$make_plot, {
      print(selected_timeseries_ids())
      if (is.null(selected_timeseries_ids())) {
        showModal(modalDialog(
          title = tr("error", language$language),
          tr("cont_table_intro", language$language),
          easyClose = TRUE,
          footer = tagList(
            modalButton(tr("close", language$language))
          )
        ))
        return()
      }
      
      # req is the list returned by plot_request()
      tryCatch(
        {
          # New connection necessary because the task gets farmed out to another core
          con <- AquaConnect(
            name = config$dbName,
            host = config$dbHost,
            port = config$dbPort,
            username = config$dbUser,
            password = config$dbPass,
            silent = TRUE
          )
          # Auto close the connection - important!!!
          on.exit(DBI::dbDisconnect(con))
          
          ts_data <- DBI::dbGetQuery(con, sprintf("
            SELECT datetime, measurements_continuous_corrected.value_corrected, locations.name, locations.name_fr 
	            FROM measurements_continuous_corrected 
              JOIN continuous.timeseries 
              ON measurements_continuous_corrected.timeseries_id = continuous.timeseries.timeseries_id
              JOIN locations
	            ON timeseries.location_id = locations.location_id
              WHERE measurements_continuous_corrected.timeseries_id IN (%s) AND datetime >= $1 AND datetime <= $2;",
                                                  paste(selected_timeseries_ids(), collapse = ", ")),
                                     params = list(input$date_range[1], input$date_range[2])
          )
          
          
          raw_ts(ts_data)
          
        }, error = function(e){
          showNotification(sprintf("Request failed with: %s", e), type = "error")
        })
      
    })
    
    plot_created <- reactiveVal(FALSE) # Flag to determine if a plot has been created
    
    # Kick off task on button click
    observeEvent(input$make_plot, {
      
      # 
      # if (plot_created()) {
      #   shinyjs::hide("full_screen_ui")
      # }
      # long_ts_plot$invoke(plot_request())
    })
    
    observeEvent(input$cancel, {
      removeModal()
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
    
    # Observe changes to the windowDims reactive value and update the legend position using plotlyProxy
    # The js function takes care of debouncing the window resize event and also reacts to a change in orientation or full screen event
    observeEvent(
      windowDims(),
      {
        req(plot_created())
        if (is.null(windowDims())) {
          return()
        }
        if (windowDims()$width > windowDims()$height) {
          plotly::plotlyProxy("plot", session) %>%
            plotly::plotlyProxyInvoke(
              "relayout",
              legend = list(orientation = "v")
            )
        } else {
          plotly::plotlyProxy("plot", session) %>%
            plotly::plotlyProxyInvoke(
              "relayout",
              legend = list(orientation = "h")
            )
        }
      },
      ignoreNULL = TRUE
    )
    
    # Observe the full screen button and run the javascript function to make the plot full screen
    observeEvent(
      input$full_screen,
      {
        shinyjs::runjs(paste0("toggleFullScreen('", ns("plot"), "');"))
        # Manually trigger a window resize event after some delay
        shinyjs::runjs(
          "
                      setTimeout(function() {
                        sendWindowSizeToShiny();
                      }, 700);
                    "
        )
      },
      ignoreInit = TRUE
    )
    
  }) # End moduleServer
}
