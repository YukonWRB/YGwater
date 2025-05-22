contDataUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(
      HTML(
        "/* Make the ...preparing download... notification stand out more */
        .shiny-notification {
          font-size: 24px;
          font-weight: bold;
          background-color: #f9f9f9;
          color: #333;
          padding: 15px;
          border-left: 5px solid #007BFF;
          border-right: 5px solid #007BFF;
          border-top: 5px solid #007BFF;
          border-bottom: 5px solid #007BFF;
          border-radius: 10px;
        }"
      )
    ),
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
  )
}

contData <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ------------------------------------------------------------------------
    # Load values used for filters
    # ------------------------------------------------------------------------
    moduleData <- reactive({
      list(
        locs  = DBI::dbGetQuery(session$userData$AquaCache,
          "SELECT DISTINCT loc.location_id, loc.name, loc.name_fr
           FROM locations loc
           JOIN continuous.timeseries ts ON loc.location_id = ts.location_id
           ORDER BY loc.name"),
        sub_locs = DBI::dbGetQuery(session$userData$AquaCache,
          "SELECT DISTINCT sl.sub_location_id, sl.sub_location_name, sl.sub_location_name_fr
           FROM sub_locations sl
           JOIN continuous.timeseries ts ON sl.sub_location_id = ts.sub_location_id
           ORDER BY sl.sub_location_name"),
        params = DBI::dbGetQuery(session$userData$AquaCache,
          "SELECT DISTINCT p.parameter_id, p.param_name,
                  COALESCE(p.param_name_fr, p.param_name) AS param_name_fr
           FROM parameters p
           JOIN continuous.timeseries ts ON p.parameter_id = ts.parameter_id
           ORDER BY p.param_name"),
        media = DBI::dbGetQuery(session$userData$AquaCache,
          "SELECT DISTINCT m.media_id, m.media_type, m.media_type_fr
           FROM media_types m
           JOIN continuous.timeseries ts ON m.media_id = ts.media_id
           ORDER BY m.media_type"),
        aggs = DBI::dbGetQuery(session$userData$AquaCache,
          "SELECT DISTINCT a.aggregation_type_id, a.aggregation_type, a.aggregation_type_fr
           FROM continuous.aggregation_types a
           JOIN continuous.timeseries ts ON a.aggregation_type_id = ts.aggregation_type_id
           ORDER BY a.aggregation_type"),
        rates = DBI::dbGetQuery(session$userData$AquaCache,
          "SELECT DISTINCT record_rate FROM continuous.timeseries ORDER BY record_rate"),
        zs = DBI::dbGetQuery(session$userData$AquaCache,
          "SELECT DISTINCT z FROM continuous.timeseries WHERE z IS NOT NULL ORDER BY z"),
        range = DBI::dbGetQuery(session$userData$AquaCache,
          "SELECT MIN(start_datetime) AS min_date, MAX(end_datetime) AS max_date FROM continuous.timeseries")
      )
    })

    table_data <- reactiveVal(data.frame())

    # ----------------------------------------------------------------------
    # Sidebar UI
    # ----------------------------------------------------------------------
    output$sidebar <- renderUI({
      data <- moduleData()
      tagList(
        dateRangeInput(ns("date_range"),
                       tr("date_range_select", language$language),
                       start = as.Date(data$range$min_date),
                       end   = as.Date(data$range$max_date),
                       min   = as.Date(data$range$min_date),
                       max   = as.Date(data$range$max_date),
                       format = "yyyy-mm-dd"),
        selectizeInput(ns("locations"),
                       label = tr("loc(s)", language$language),
                       choices = stats::setNames(c("all", data$locs$location_id),
                                               c(tr("all", language$language), data$locs[, tr("generic_name_col", language$language)])),
                       selected = "all", multiple = TRUE),
        selectizeInput(ns("sub_locations"),
                       label = tr("sub_loc(s)", language$language),
                       choices = stats::setNames(c("all", data$sub_locs$sub_location_id),
                                               c(tr("all", language$language), data$sub_locs[, tr("sub_location_col", language$language)])),
                       selected = "all", multiple = TRUE),
        selectizeInput(ns("media"),
                       label = tr("media_type(s)", language$language),
                       choices = stats::setNames(c("all", data$media$media_id),
                                               c(tr("all", language$language), data$media[, tr("media_type_col", language$language)])),
                       selected = "all", multiple = TRUE),
        selectizeInput(ns("aggs"),
                       label = tr("aggregation_type", language$language),
                       choices = stats::setNames(c("all", data$aggs$aggregation_type_id),
                                               c(tr("all", language$language), data$aggs[, if (language$abbrev == 'fr') 'aggregation_type_fr' else 'aggregation_type'])),
                       selected = "all", multiple = TRUE),
        selectizeInput(ns("rate"),
                       label = tr("rate", language$language),
                       choices = stats::setNames(c("all", data$rates$record_rate),
                                               c(tr("all", language$language), data$rates$record_rate)),
                       selected = "all", multiple = TRUE),
        selectizeInput(ns("z"),
                       label = tr("z", language$language),
                       choices = stats::setNames(c("all", data$zs$z),
                                               c(tr("all", language$language), data$zs$z)),
                       selected = "all", multiple = TRUE),
        selectizeInput(ns("params"),
                       label = tr("parameter(s)", language$language),
                       choices = stats::setNames(c("all", data$params$parameter_id),
                                               c(tr("all", language$language), data$params[, tr("param_name_col", language$language)])),
                       selected = "all", multiple = TRUE),
        tags$div(style = "height: 15px;"),
        splitLayout(
          cellWidths = c("50%", "50%"),
          actionButton(ns("reset"), label = tr("reset_filters", language$language), width = "100%"),
          actionButton(ns("filter"), label = tr("view_samples", language$language), width = "100%")
        )
      )
    })

    output$main <- renderUI({
      tagList(
        tags$p(HTML(tr("view_data_instructions", language$language))),
        tags$div(style = "height: 10px;"),
        DT::DTOutput(ns("tbl")),
        actionButton(ns("select_all"), tr("select_all", language$language), style = "display: none;"),
        actionButton(ns("view_data"), tr("view_data1", language$language), style = "display: none;")
      )
    })

    # ----------------------------------------------------------------------
    # Reset filters
    # ----------------------------------------------------------------------
    observeEvent(input$reset, {
      data <- moduleData()
      updateDateRangeInput(session, "date_range",
                           start = as.Date(data$range$min_date),
                           end   = as.Date(data$range$max_date),
                           min   = as.Date(data$range$min_date),
                           max   = as.Date(data$range$max_date))
      updateSelectizeInput(session, "locations",
                           choices = stats::setNames(c("all", data$locs$location_id),
                                                   c(tr("all", language$language), data$locs[, tr("generic_name_col", language$language)])),
                           selected = "all")
      updateSelectizeInput(session, "sub_locations",
                           choices = stats::setNames(c("all", data$sub_locs$sub_location_id),
                                                   c(tr("all", language$language), data$sub_locs[, tr("sub_location_col", language$language)])),
                           selected = "all")
      updateSelectizeInput(session, "media",
                           choices = stats::setNames(c("all", data$media$media_id),
                                                   c(tr("all", language$language), data$media[, tr("media_type_col", language$language)])),
                           selected = "all")
      updateSelectizeInput(session, "aggs",
                           choices = stats::setNames(c("all", data$aggs$aggregation_type_id),
                                                   c(tr("all", language$language), data$aggs[, if (language$abbrev == 'fr') 'aggregation_type_fr' else 'aggregation_type'])),
                           selected = "all")
      updateSelectizeInput(session, "rate",
                           choices = stats::setNames(c("all", data$rates$record_rate),
                                                   c(tr("all", language$language), data$rates$record_rate)),
                           selected = "all")
      updateSelectizeInput(session, "z",
                           choices = stats::setNames(c("all", data$zs$z),
                                                   c(tr("all", language$language), data$zs$z)),
                           selected = "all")
      updateSelectizeInput(session, "params",
                           choices = stats::setNames(c("all", data$params$parameter_id),
                                                   c(tr("all", language$language), data$params[, tr("param_name_col", language$language)])),
                           selected = "all")
      table_data(data.frame())
      output$tbl <- DT::renderDT(data.frame())
      shinyjs::hide("select_all")
    })

    # ----------------------------------------------------------------------
    # Filtering action - query timeseries
    # ----------------------------------------------------------------------
    observeEvent(input$filter, {
      data <- moduleData()
      cond <- c(
        sprintf("ts.start_datetime <= '%s'", as.character(input$date_range[2])),
        sprintf("ts.end_datetime >= '%s'", as.character(input$date_range[1]))
      )
      if (!("all" %in% input$locations))
        cond <- c(cond, sprintf("ts.location_id IN (%s)", paste(input$locations, collapse = ",")))
      if (!("all" %in% input$sub_locations))
        cond <- c(cond, sprintf("ts.sub_location_id IN (%s)", paste(input$sub_locations, collapse = ",")))
      if (!("all" %in% input$media))
        cond <- c(cond, sprintf("ts.media_id IN (%s)", paste(input$media, collapse = ",")))
      if (!("all" %in% input$aggs))
        cond <- c(cond, sprintf("ts.aggregation_type_id IN (%s)", paste(input$aggs, collapse = ",")))
      if (!("all" %in% input$rate))
        cond <- c(cond, sprintf("ts.record_rate IN (%s)", paste(sprintf("'%s'", input$rate), collapse = ",")))
      if (!("all" %in% input$z))
        cond <- c(cond, sprintf("ts.z IN (%s)", paste(input$z, collapse = ",")))
      if (!("all" %in% input$params))
        cond <- c(cond, sprintf("ts.parameter_id IN (%s)", paste(input$params, collapse = ",")))
      where <- paste(cond, collapse = " AND ")

      query <- paste0(
        "SELECT ts.timeseries_id, loc.name AS location, sl.sub_location_name,",
        " p.param_name AS parameter, m.media_type AS media,",
        " ag.aggregation_type, ts.record_rate, ts.z,",
        " ts.start_datetime, ts.end_datetime, loc.location_id",
        " FROM continuous.timeseries ts",
        " JOIN locations loc ON ts.location_id = loc.location_id",
        " LEFT JOIN sub_locations sl ON ts.sub_location_id = sl.sub_location_id",
        " JOIN parameters p ON ts.parameter_id = p.parameter_id",
        " JOIN media_types m ON ts.media_id = m.media_id",
        " JOIN continuous.aggregation_types ag ON ts.aggregation_type_id = ag.aggregation_type_id",
        " WHERE ", where,
        " ORDER BY loc.name, p.param_name")

      res <- DBI::dbGetQuery(session$userData$AquaCache, query)
      table_data(res)

      out_tbl <- DT::datatable(res,
        rownames = FALSE,
        selection = "multiple",
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          language = list(
            info = tr("tbl_info", language$language),
            infoEmpty = tr("tbl_info_empty", language$language),
            paginate = list(previous = "", `next` = ""),
            search = tr("tbl_search", language$language),
            lengthMenu = tr("tbl_length", language$language),
            infoFiltered = tr("tbl_filtered", language$language),
            zeroRecords = tr("tbl_zero", language$language)
          )
        )
      )
      output$tbl <- DT::renderDT(out_tbl)
      if (nrow(res) == 0) {
        shinyjs::hide("select_all")
      } else {
        shinyjs::show("select_all")
      }
    })

    proxy <- DT::dataTableProxy("tbl")
    select_all <- reactiveVal(FALSE)
    observeEvent(input$select_all, {
      if (select_all()) {
        DT::selectRows(proxy, NULL)
        select_all(FALSE)
      } else {
        DT::selectRows(proxy, seq_len(nrow(table_data())))
        select_all(TRUE)
      }
    })

    observe({
      if (is.null(input$tbl_rows_selected)) {
        shinyjs::hide("view_data")
      } else {
        shinyjs::show("view_data")
        updateActionButton(session, "view_data",
                           label = paste0(tr("view_data1", language$language),
                                          " ", length(input$tbl_rows_selected),
                                          " ", tr("view_data2", language$language)))
      }
    })

    # ----------------------------------------------------------------------
    # Modal showing subset of data and download option
    # ----------------------------------------------------------------------
    observeEvent(input$view_data, {
      req(input$tbl_rows_selected)
      ids <- table_data()[input$tbl_rows_selected, timeseries_id]
      id_str <- paste(ids, collapse = ",")
      subset_q <- paste0(
        "SELECT * FROM (SELECT timeseries_id, datetime, value_corrected,",
        " ROW_NUMBER() OVER (PARTITION BY timeseries_id ORDER BY datetime) rn",
        " FROM continuous.measurements_continuous_corrected",
        " WHERE timeseries_id IN (", id_str, ")",
        " AND datetime BETWEEN '", input$date_range[1], "' AND '", input$date_range[2], "')",
        ") sub WHERE rn <= 3")
      subset <- DBI::dbGetQuery(session$userData$AquaCache, subset_q)
      subset$rn <- NULL

      output$modal_subset <- DT::renderDT({
        DT::datatable(subset, rownames = FALSE, selection = "none",
                       options = list(scrollX = TRUE, dom = 'rt'))
      })

      output$num_rows <- renderText({
        n <- DBI::dbGetQuery(session$userData$AquaCache,
          paste0("SELECT COUNT(*) FROM continuous.measurements_continuous_corrected WHERE timeseries_id IN (", id_str,
                 ") AND datetime BETWEEN '", input$date_range[1], "' AND '", input$date_range[2], "'"))[[1]]
        paste0(tr("dl_num_results", language$language), " ", n)
      })

      showModal(modalDialog(
        h4(tr("continuous_subset_msg", language$language)),
        DT::DTOutput(ns("modal_subset")),
        textOutput(ns("num_rows")),
        selectizeInput(ns("modal_format"), label = tr("dl_format", language$language),
                       choices = stats::setNames(c("xlsx", "csv"),
                                               c(tr("dl_format_xlsx", language$language), tr("dl_format_csv", language$language))),
                       selected = "xlsx"),
        downloadButton(ns("download"), tr("dl_data", language$language)),
        size = "l"
      ))
    })

    output$download <- downloadHandler(
      filename = function() {
        paste0("timeseries_", format(Sys.time(), "%Y%m%d_%H%M%S%Z"), ".", if (input$modal_format == "csv") "zip" else input$modal_format)
      },
      content = function(file) {
        ids <- table_data()[input$tbl_rows_selected, timeseries_id]
        id_str <- paste(ids, collapse = ",")
        data <- DBI::dbGetQuery(session$userData$AquaCache,
          paste0("SELECT * FROM continuous.measurements_continuous_corrected WHERE timeseries_id IN (", id_str,
                 ") AND datetime BETWEEN '", input$date_range[1], "' AND '", input$date_range[2], "' ORDER BY timeseries_id, datetime"))
        meta <- DBI::dbGetQuery(session$userData$AquaCache,
          paste0("SELECT * FROM continuous.timeseries_metadata_", language$abbrev,
                 " WHERE timeseries_id IN (", id_str, ")"))
        out <- list(metadata = meta, measurements = data)
        if (input$modal_format == "xlsx") {
          openxlsx::write.xlsx(out, file)
        } else {
          temp_dir <- tempdir()
          meta_file <- file.path(temp_dir, "metadata.csv")
          meas_file <- file.path(temp_dir, "measurements.csv")
          data.table::fwrite(meta, meta_file)
          data.table::fwrite(data, meas_file)
          utils::zip(file, c(meta_file, meas_file))
        }
      }
    )
  })
}

