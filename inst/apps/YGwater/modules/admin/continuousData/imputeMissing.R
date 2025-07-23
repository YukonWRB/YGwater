# UI and server code for imputing missing values in continuous data

imputeMissingUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(
      HTML(sprintf("
     /* Add colors to the accordion. Using ns() makes it specific to each accordion */
      #%s.accordion {
        /* body background */
        --bs-accordion-bg:          #FFFCF5;
        /* collapsed header */
        --bs-accordion-btn-bg:      #FBE5B2;
        /* expanded header */
        --bs-accordion-active-bg:   #FBE5B2;
      }
    ", ns("accordion1"))),
      HTML(sprintf("
     /* Add colors to the accordion. Using ns() makes it specific to each accordion */
      #%s.accordion {
        /* body background */
        --bs-accordion-bg:          #E5F4F6;
        /* collapsed header */
        --bs-accordion-btn-bg:      #0097A9;
        /* expanded header */
        --bs-accordion-active-bg:   #0097A9;
      }
    ", ns("accordion2"))),
      HTML(sprintf("
     /* Add colors to the accordion. Using ns() makes it specific to each accordion */
      #%s.accordion {
        /* body background */
        --bs-accordion-bg:          #F1F4E5;
        /* collapsed header */
        --bs-accordion-btn-bg:      #7A9A01;
        /* expanded header */
        --bs-accordion-active-bg:   #7A9A01;
      }
    ", ns("accordion3")))
      ),
    
    page_fluid(
      accordion(
        id = ns("accordion1"),
        open = "ts_panel",
        accordion_panel(
          id = ns("ts_panel"),
          title = "Timeseries selection",
          DT::DTOutput(ns("ts_table")),
          input_task_button(ns("plot_ts_pre"), "Plot timeseries"),
          plotly::plotlyOutput(ns("ts_plot_pre"))
        )
      ),
      accordion(
        id = ns("accordion2"),
        open = "options_panel",
        accordion_panel(
          id = ns("options_panel"),
          title = "Imputation options",
          textInput(ns("start"), "Start datetime (UTC)", placeholder = "YYYY-MM-DD HH:MM:SS"),
          textInput(ns("end"), "End datetime (UTC)", placeholder = "YYYY-MM-DD HH:MM:SS"),
          numericInput(ns("radius"), "Search radius (km)", value = 10, min = 0),
          selectInput(ns("method"), "Interpolation method",
                      choices = c("Direct (use other timeseries with calculated offset)" = "direct",
                                  "Linear (direct point to point)" = "linear",
                                  "Spline (curve fitted with surrounding data)" = "spline")),
          actionButton(ns("load"), "Load data")
        )
      ),
      accordion(
        id = ns("accordion3"),
        open = FALSE,
        accordion_panel(
          id = ns("impute_panel"),
          title = "Impute",
          textOutput(ns("direct_impute_selection")),
          DT::DTOutput(ns("candidates")),
          plotly::plotlyOutput(ns("plot")),
          actionButton(ns("impute"), "Impute"),
          actionButton(ns("commit"), "Commit to DB")
        )
      )
    )
  )
}


imputeMissing <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ts_meta <- reactive({
      dbGetQueryDT(session$userData$AquaCache,
                   "SELECT timeseries_id, location_name AS location, parameter_name AS parameter, media_type AS media, aggregation_type AS aggregation, recording_rate AS nominal_record_rate FROM continuous.timeseries_metadata_en")
    })
    
    output$ts_table <- DT::renderDT({
      DT::datatable(ts_meta(), selection = 'single',
                    options = list(
                      columnDefs = list(
                        list(targets = 0, 
                             visible = FALSE)),
                      scrollX = TRUE,
                      initComplete = htmlwidgets::JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({",
                        "  'background-color': '#079',",
                        "  'color': '#fff',",
                        "  'font-size': '100%',",
                        "});",
                        "$(this.api().table().body()).css({",
                        "  'font-size': '90%',",
                        "});",
                        "}"
                      )
                    )
      )
    })
    
    selected_ts <- reactiveVal()
    
    observeEvent(input$ts_table_rows_selected, {
      req(input$ts_table_rows_selected)
      selected_ts(ts_meta()[input$ts_table_rows_selected, "timeseries_id"])
    })
    
    ts_plot_pre_task <- ExtendedTask$new(function(df) {
      promises::future_promise({
        plot <- plotly::plot_ly(data = df, x = ~datetime, y = ~value_raw, type = 'scatter', mode = 'lines', name = 'Original') %>%
          plotly::add_lines(y = ~value_corrected, name = 'Corrected', line = list(color = 'red')) %>%
          plotly::layout(title = NULL, xaxis = list(title = "Datetime"), yaxis = list(title = "Value"))
        return(plot)
      })
    }) |> bind_task_button("plot_ts_pre")
    
    observeEvent(input$plot_ts_pre, {
      req(selected_ts())
      query <- sprintf(
        "SELECT datetime, value_raw, value_corrected FROM continuous.measurements_continuous_corrected WHERE timeseries_id = %s ORDER BY datetime",
        selected_ts()
      )
      df <- DBI::dbGetQuery(session$userData$AquaCache, query)
      ts_plot_pre_task$invoke(df)
    })
    
    output$ts_plot_pre <- plotly::renderPlotly({
      ts_plot_pre_task$result()
    })
    
    
    raw_data <- reactiveVal(NULL)
    full_data <- reactiveVal(NULL)
    candidates <- reactiveVal(NULL)
    imputed_data <- reactiveVal(NULL)
    
    observeEvent(input$load, {
      print(seleted_ts())
      print(input$start)
      print(input$end)
      req(selected_ts(), input$start, input$end)
      query <- sprintf(
        "SELECT datetime, value FROM continuous.measurements_continuous WHERE timeseries_id = %s AND datetime >= '%s' AND datetime <= '%s'",
        selected_ts(), input$start, input$end
      )
      df <- DBI::dbGetQuery(session$userData$AquaCache, query)
      df$datetime <- as.POSIXct(df$datetime, tz = 'UTC')
      raw_data(df)
      if (nrow(df) > 1) {
        period <- stats::median(diff(df$datetime))
      } else {
        period <- 3600
      }
      full_dt <- data.frame(datetime = seq(min(df$datetime), max(df$datetime), by = period))
      full_dt <- merge(full_dt, df, by = 'datetime', all.x = TRUE)
      full_data(full_dt)
      loc <- ts_meta()[input$ts_table_rows_selected, 'location']
      param <- ts_meta()[input$ts_table_rows_selected, 'parameter']
      cand_q <- sprintf(
        "SELECT timeseries_id, location_name AS location, parameter_name AS parameter FROM continuous.timeseries_metadata_en WHERE parameter_name = '%s' AND location_name != '%s'",
        param, loc
      )
      candidates(DBI::dbGetQuery(session$userData$AquaCache, cand_q))
    })
    
    output$direct_impute_selection <- renderText({
      "Select a timeseries below to impute missing values using the direct method."
    })
    output$candidates <- DT::renderDT({
      req(candidates())
      DT::datatable(candidates(), selection = 'single', options = list(scrollX = TRUE))
    })
    
    perform_impute <- function(df, method, cand = NULL) {
      if (method == 'direct' && !is.null(cand)) {
        df$imputed <- is.na(df$value)
        merged <- merge(df, cand, by = 'datetime', all.x = TRUE, suffixes = c('', '.cand'))
        offset <- mean(merged$value - merged$value.cand, na.rm = TRUE)
        fill_idx <- which(is.na(merged$value))
        merged$value[fill_idx] <- merged$value.cand[fill_idx] + offset
        df$value <- merged$value
        return(df)
      }
      df$imputed <- FALSE
      na_run <- rle(is.na(df$value))
      pos <- cumsum(na_run$lengths)
      for (i in seq_along(na_run$lengths)) {
        if (na_run$values[i]) {
          start_pos <- pos[i] - na_run$lengths[i] + 1
          end_pos <- pos[i + 1] - na_run$lengths[i + 1]
          if (method == 'linear') {
            y <- stats::approx(x = c(start_pos - 1, end_pos + 1),
                               y = df$value[c(start_pos - 1, end_pos + 1)],
                               xout = start_pos:end_pos)$y
          } else {
            y <- stats::spline(x = c(start_pos - 1, end_pos + 1),
                               y = df$value[c(start_pos - 1, end_pos + 1)],
                               xout = start_pos:end_pos)$y
          }
          df$value[start_pos:end_pos] <- y
          df$imputed[start_pos:end_pos] <- TRUE
        }
      }
      df
    }
    
    observeEvent(input$impute, {
      req(full_data())
      df <- full_data()
      method <- input$method
      if (method == 'direct') {
        sel <- input$candidates_rows_selected
        if (length(sel) != 1) {
          showNotification('Select a timeseries for direct imputation.', type = 'error')
          return()
        }
        tsid2 <- candidates()[sel, 'timeseries_id']
        q <- sprintf(
          "SELECT datetime, value FROM continuous.measurements_continuous WHERE timeseries_id = %s AND datetime >= '%s' AND datetime <= '%s'",
          tsid2, min(df$datetime), max(df$datetime)
        )
        cand <- DBI::dbGetQuery(session$userData$AquaCache, q)
        cand$datetime <- as.POSIXct(cand$datetime, tz = 'UTC')
        cand_full <- merge(data.frame(datetime = df$datetime), cand, by = 'datetime', all.x = TRUE)
        imp <- perform_impute(df, 'direct', cand_full)
      } else {
        imp <- perform_impute(df, method)
      }
      imputed_data(imp)
      output$plot <- plotly::renderPlotly({
        dat <- imp
        dat$existing <- dat$value
        dat$existing[dat$imputed] <- NA
        dat$impute <- imp$value
        dat$impute[!imp$imputed] <- NA
        plotly::plot_ly() %>%
          plotly::add_lines(data = dat, x = ~datetime, y = ~existing, name = 'existing', line = list(color = 'blue')) %>%
          plotly::add_lines(data = dat, x = ~datetime, y = ~impute, name = 'imputed', line = list(color = 'red'))
      })
    })
    
    observeEvent(input$commit, {
      req(imputed_data())
      tryCatch({
        df <- imputed_data()
        to_push <- df[df$imputed, c('datetime', 'value')]
        to_push$timeseries_id <- selected_ts()
        to_push$imputed <- TRUE
        AquaCache::addNewContinuous(tsid = selected_ts(), df = to_push, con = session$userData$AquaCache)
        showNotification('Data imputed and saved.', type = 'message')
      }, error = function(e) {
        showNotification(paste('Commit failed:', e$message), type = 'error')
      })
    })
  })
}
