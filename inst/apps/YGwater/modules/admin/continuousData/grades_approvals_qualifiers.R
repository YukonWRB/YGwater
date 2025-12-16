# UI and server code for adding/modifying grades, approvals, qualifiers

grades_approvals_qualifiersUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(
      HTML(sprintf(
        "\n     /* Add colors to the accordion. Using ns() makes it specific to each accordion */\n      #%s.accordion {\n        /* body background */\n        --bs-accordion-bg:          #FFFCF5;\n        /* collapsed header */\n        --bs-accordion-btn-bg:      #FBE5B2;\n        /* expanded header */\n        --bs-accordion-active-bg:   #FBE5B2;\n      }\n    ",
        ns("accordion_ts")
      )),
      HTML(sprintf(
        "\n     /* Add colors to the accordion. Using ns() makes it specific to each accordion */\n      #%s.accordion {\n        /* body background */\n        --bs-accordion-bg:          #E5F4F6;\n        /* collapsed header */\n        --bs-accordion-btn-bg:      #0097A9;\n        /* expanded header */\n        --bs-accordion-active-bg:   #0097A9;\n      }\n    ",
        ns("accordion_review")
      )),
      HTML(sprintf(
        "\n     /* Add colors to the accordion. Using ns() makes it specific to each accordion */\n      #%s.accordion {\n        /* body background */\n        --bs-accordion-bg:          #F1F4E5;\n        /* collapsed header */\n        --bs-accordion-btn-bg:      #7A9A01;\n        /* expanded header */\n        --bs-accordion-active-bg:   #7A9A01;\n      }\n    ",
        ns("accordion_manage")
      ))
    ),
    page_fluid(
      accordion(
        id = ns("accordion_ts"),
        open = "ts_panel",
        accordion_panel(
          id = ns("ts_panel"),
          title = "Timeseries selection",
          p(
            class = "text-muted",
            "Select a timeseries to review raw and corrected values before applying grades, approvals, or qualifiers."
          ),
          DT::DTOutput(ns("ts_table"))
        )
      ),
      accordion(
        id = ns("accordion_review"),
        open = FALSE,
        accordion_panel(
          id = ns("review_panel"),
          title = "Review data and choose a date range",
          fluidRow(
            column(
              width = 4,
              textInput(
                ns("start_dt"),
                "Start datetime (UTC)",
                placeholder = "YYYY-MM-DD HH:MM:SS"
              ),
              textInput(
                ns("end_dt"),
                "End datetime (UTC)",
                placeholder = "YYYY-MM-DD HH:MM:SS"
              ),
              div(
                class = "text-muted small",
                textOutput(ns("range_feedback"))
              ),
              div(
                class = "text-muted small mt-2",
                textOutput(ns("click_feedback"))
              )
            ),
            column(
              width = 8,
              plotly::plotlyOutput(ns("ts_plot"), height = "420px")
            )
          )
        )
      ),
      accordion(
        id = ns("accordion_manage"),
        open = FALSE,
        accordion_panel(
          id = ns("manage_panel"),
          title = "Manage grades, approvals, and qualifiers",
          fluidRow(
            column(
              width = 4,
              radioButtons(
                ns("attribute_kind"),
                "Attribute to modify",
                choices = c(
                  "Grades" = "grade",
                  "Approvals" = "approval",
                  "Qualifiers" = "qualifier"
                ),
                inline = FALSE
              ),
              selectizeInput(
                ns("attribute_value"),
                "Attribute value",
                choices = NULL,
                multiple = FALSE,
                options = list(placeholder = "Select a value")
              ),
              actionButton(ns("clear_selection"), "Clear selection"),
              br(),
              br(),
              actionButton(
                ns("apply_attribute"),
                "Add attribute",
                class = "btn-primary w-100"
              ),
              actionButton(
                ns("delete_attribute"),
                "Delete selected",
                class = "btn-danger w-100 mt-2"
              )
            ),
            column(
              width = 8,
              h5(textOutput(ns("active_table_title"))),
              DT::DTOutput(ns("active_assignments")),
              br(),
              accordion(
                id = ns("existing_overview"),
                open = NULL,
                accordion_panel(
                  id = ns("existing_grades"),
                  title = "Existing grades",
                  DT::DTOutput(ns("grades_table"))
                ),
                accordion_panel(
                  id = ns("existing_approvals"),
                  title = "Existing approvals",
                  DT::DTOutput(ns("approvals_table"))
                ),
                accordion_panel(
                  id = ns("existing_qualifiers"),
                  title = "Existing qualifiers",
                  DT::DTOutput(ns("qualifiers_table"))
                )
              )
            )
          )
        )
      )
    )
  )
}

grades_approvals_qualifiers <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    con <- session$userData$AquaCache
    req(con)

    parse_datetime <- function(value) {
      if (is.null(value) || !nzchar(value)) {
        return(NA)
      }
      suppressWarnings({
        parsed <- as.POSIXct(value, tz = "UTC")
      })
      if (is.na(parsed)) {
        suppressWarnings({
          parsed <- as.POSIXct(strptime(value, "%Y-%m-%d", tz = "UTC"))
        })
      }
      parsed
    }

    format_datetime <- function(value) {
      if (is.null(value) || is.na(value)) {
        return("")
      }
      value <- as.POSIXct(value, tz = "UTC")
      format(value, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    }

    to_posix_from_event <- function(value) {
      if (inherits(value, "POSIXt")) {
        return(as.POSIXct(value, tz = "UTC"))
      }
      if (is.numeric(value)) {
        if (abs(value) > 1e12) {
          value <- value / 1000
        }
        return(as.POSIXct(value, origin = "1970-01-01", tz = "UTC"))
      }
      if (is.character(value)) {
        suppressWarnings({
          parsed <- as.POSIXct(value, tz = "UTC")
        })
        if (!is.na(parsed)) {
          return(parsed)
        }
      }
      NA
    }

    load_privileges <- function() {
      query <- "SELECT \
        has_table_privilege(current_user, 'continuous.grades', 'INSERT') AS grade_insert, \
        has_table_privilege(current_user, 'continuous.grades', 'UPDATE') AS grade_update, \
        has_table_privilege(current_user, 'continuous.grades', 'DELETE') AS grade_delete, \
        has_table_privilege(current_user, 'continuous.approvals', 'INSERT') AS approval_insert, \
        has_table_privilege(current_user, 'continuous.approvals', 'UPDATE') AS approval_update, \
        has_table_privilege(current_user, 'continuous.approvals', 'DELETE') AS approval_delete, \
        has_table_privilege(current_user, 'continuous.qualifiers', 'INSERT') AS qualifier_insert, \
        has_table_privilege(current_user, 'continuous.qualifiers', 'UPDATE') AS qualifier_update, \
        has_table_privilege(current_user, 'continuous.qualifiers', 'DELETE') AS qualifier_delete"
      DBI::dbGetQuery(con, query)
    }

    privileges_row <- load_privileges()
    privileges <- as.list(privileges_row[1, , drop = FALSE])

    if (!any(unlist(privileges))) {
      showModal(
        modalDialog(
          title = "Insufficient privileges",
          "You do not have write privileges for the grades, approvals, or qualifiers tables.",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    }

    safe_query <- function(expr) {
      tryCatch(expr, error = function(e) {
        warning(conditionMessage(e))
        data.frame()
      })
    }

    module_data <- reactiveValues(
      grade_types = safe_query(
        DBI::dbGetQuery(
          con,
          "SELECT grade_type_id, grade_type_description, COALESCE(color_code, '#4E79A7') AS color_code FROM grade_types ORDER BY grade_type_description"
        )
      ),
      approval_types = safe_query(
        DBI::dbGetQuery(
          con,
          "SELECT approval_type_id, approval_type_description, COALESCE(color_code, '#F28E2B') AS color_code FROM approval_types ORDER BY approval_type_description"
        )
      ),
      qualifier_types = safe_query(
        DBI::dbGetQuery(
          con,
          "SELECT qualifier_type_id, qualifier_type_description, COALESCE(color_code, '#E15759') AS color_code FROM qualifier_types ORDER BY qualifier_type_description"
        )
      ),
      privileges = privileges
    )

    ts_meta <- reactive({
      dbGetQueryDT(
        con,
        paste(
          "SELECT timeseries_id, location_name AS location, parameter_name AS parameter,",
          "media_type AS media, aggregation_type AS aggregation, recording_rate,",
          "note FROM continuous.timeseries_metadata_en"
        )
      )
    })

    output$ts_table <- DT::renderDT({
      df <- as.data.frame(ts_meta())
      if (!nrow(df)) {
        return(DT::datatable(
          data.frame(Message = "No timeseries available."),
          options = list(dom = 't'),
          selection = 'none'
        ))
      }
      df$recording_rate <- as.factor(df$recording_rate)
      df$media <- as.factor(df$media)
      df$aggregation <- as.factor(df$aggregation)
      df$parameter <- as.factor(df$parameter)
      DT::datatable(
        df,
        selection = 'single',
        options = list(
          columnDefs = list(list(targets = 0, visible = FALSE)),
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
        ),
        filter = 'top'
      )
    })

    selected_ts <- reactiveVal(NULL)
    selected_record <- reactiveVal(NULL)
    assignment_refresh <- reactiveVal(0)
    next_edge <- reactiveVal("start")

    observeEvent(
      input$ts_table_rows_selected,
      {
        selection <- input$ts_table_rows_selected
        if (length(selection)) {
          tsid <- ts_meta()[selection, "timeseries_id"]
          selected_ts(tsid)
          assignment_refresh(assignment_refresh() + 1)
          selected_record(NULL)
          next_edge("start")

          range_info <- safe_query(
            DBI::dbGetQuery(
              con,
              "SELECT MIN(datetime) AS min_dt, MAX(datetime) AS max_dt FROM continuous.measurements_continuous_corrected WHERE timeseries_id = $1",
              params = list(tsid)
            )
          )

          default_end <- if (nrow(range_info) && !is.na(range_info$max_dt[1])) {
            as.POSIXct(range_info$max_dt[1], tz = "UTC")
          } else {
            Sys.time()
          }
          default_start <- default_end - 7 * 24 * 3600
          if (nrow(range_info) && !is.na(range_info$min_dt[1])) {
            min_dt <- as.POSIXct(range_info$min_dt[1], tz = "UTC")
            if (!is.na(min_dt) && default_start < min_dt) {
              default_start <- min_dt
            }
          }
          if (
            !is.finite(as.numeric(default_start)) ||
              !is.finite(as.numeric(default_end))
          ) {
            default_start <- Sys.time() - 7 * 24 * 3600
            default_end <- Sys.time()
          }
          if (default_start >= default_end) {
            default_start <- default_end - 1 * 3600
          }
          updateTextInput(
            session,
            "start_dt",
            value = format_datetime(default_start)
          )
          updateTextInput(
            session,
            "end_dt",
            value = format_datetime(default_end)
          )
        } else {
          selected_ts(NULL)
        }
      },
      ignoreNULL = FALSE
    )

    range_error <- reactive({
      if (is.null(selected_ts())) {
        return(NULL)
      }
      start_str <- input$start_dt
      end_str <- input$end_dt
      if (is.null(start_str) || !nzchar(start_str)) {
        return("Enter a start datetime.")
      }
      start_dt <- parse_datetime(start_str)
      if (is.na(start_dt)) {
        return("Start datetime is invalid. Use format YYYY-MM-DD HH:MM:SS.")
      }
      if (is.null(end_str) || !nzchar(end_str)) {
        return("Enter an end datetime.")
      }
      end_dt <- parse_datetime(end_str)
      if (is.na(end_dt)) {
        return("End datetime is invalid. Use format YYYY-MM-DD HH:MM:SS.")
      }
      if (start_dt >= end_dt) {
        return("Start datetime must be before end datetime.")
      }
      NULL
    })

    output$range_feedback <- renderText({
      msg <- range_error()
      if (is.null(msg)) "" else msg
    })

    output$click_feedback <- renderText({
      if (is.null(selected_ts())) {
        return("")
      }
      paste0(
        "Click on the plot to set the ",
        next_edge(),
        " datetime. Drag across the plot to fill both start and end."
      )
    })

    load_assignments <- function(kind, tsid) {
      if (is.null(tsid)) {
        return(data.frame())
      }
      query <- switch(
        kind,
        grade = "SELECT g.grade_id AS record_id, g.grade_type_id AS type_id, gt.grade_type_description AS description, COALESCE(gt.color_code, '#4E79A7') AS color_code, g.start_dt, g.end_dt, g.created, g.updated FROM continuous.grades g LEFT JOIN grade_types gt ON g.grade_type_id = gt.grade_type_id WHERE g.timeseries_id = $1 ORDER BY g.start_dt",
        approval = "SELECT a.approval_id AS record_id, a.approval_type_id AS type_id, at.approval_type_description AS description, COALESCE(at.color_code, '#F28E2B') AS color_code, a.start_dt, a.end_dt, a.created, a.updated FROM continuous.approvals a LEFT JOIN approval_types at ON a.approval_type_id = at.approval_type_id WHERE a.timeseries_id = $1 ORDER BY a.start_dt",
        qualifier = "SELECT q.qualifier_id AS record_id, q.qualifier_type_id AS type_id, qt.qualifier_type_description AS description, COALESCE(qt.color_code, '#E15759') AS color_code, q.start_dt, q.end_dt, q.created, q.updated FROM continuous.qualifiers q LEFT JOIN qualifier_types qt ON q.qualifier_type_id = qt.qualifier_type_id WHERE q.timeseries_id = $1 ORDER BY q.start_dt"
      )
      df <- safe_query(DBI::dbGetQuery(con, query, params = list(tsid)))
      if (!nrow(df)) {
        return(df)
      }
      cols <- intersect(
        c("start_dt", "end_dt", "created", "updated"),
        names(df)
      )
      for (col in cols) {
        df[[col]] <- as.POSIXct(df[[col]], tz = "UTC")
      }
      df
    }

    assignments <- reactive({
      req(selected_ts())
      assignment_refresh()
      tsid <- selected_ts()
      list(
        grades = load_assignments("grade", tsid),
        approvals = load_assignments("approval", tsid),
        qualifiers = load_assignments("qualifier", tsid)
      )
    })

    ts_data <- reactive({
      if (is.null(selected_ts())) {
        return(data.frame())
      }
      if (!is.null(range_error())) {
        return(data.frame())
      }
      start_dt <- parse_datetime(input$start_dt)
      end_dt <- parse_datetime(input$end_dt)
      if (is.na(start_dt) || is.na(end_dt) || start_dt >= end_dt) {
        return(data.frame())
      }
      df <- safe_query(
        DBI::dbGetQuery(
          con,
          "SELECT datetime, value_raw, value_corrected FROM continuous.measurements_continuous_corrected WHERE timeseries_id = $1 AND datetime BETWEEN $2 AND $3 ORDER BY datetime",
          params = list(selected_ts(), start_dt, end_dt)
        )
      )
      if (nrow(df)) {
        df$datetime <- as.POSIXct(df$datetime, tz = "UTC")
      }
      df
    })

    active_kind <- reactive({
      kind <- input$attribute_kind
      if (is.null(kind) || !nzchar(kind)) {
        "grade"
      } else {
        kind
      }
    })

    observe({
      kind <- active_kind()
      types <- switch(
        kind,
        grade = module_data$grade_types,
        approval = module_data$approval_types,
        qualifier = module_data$qualifier_types
      )
      if (is.null(types) || !nrow(types)) {
        updateSelectizeInput(
          session,
          "attribute_value",
          choices = character(0),
          selected = character(0)
        )
      } else {
        id_col <- switch(
          kind,
          grade = "grade_type_id",
          approval = "approval_type_id",
          qualifier = "qualifier_type_id"
        )
        label_col <- switch(
          kind,
          grade = "grade_type_description",
          approval = "approval_type_description",
          qualifier = "qualifier_type_description"
        )
        choices <- stats::setNames(
          as.character(types[[id_col]]),
          types[[label_col]]
        )
        current <- selected_record()
        selected_value <- if (!is.null(current) && !is.null(current$type_id)) {
          as.character(current$type_id)
        } else {
          character(0)
        }
        updateSelectizeInput(
          session,
          "attribute_value",
          choices = choices,
          selected = selected_value,
          server = TRUE
        )
      }
    })

    observeEvent(
      active_kind(),
      {
        selected_record(NULL)
        next_edge("start")
        proxy <- DT::dataTableProxy("active_assignments", session = session)
        DT::selectRows(proxy, NULL)
      },
      ignoreNULL = FALSE
    )

    output$active_table_title <- renderText({
      switch(
        active_kind(),
        grade = "Selected grade assignments",
        approval = "Selected approval assignments",
        qualifier = "Selected qualifier assignments"
      )
    })

    active_assignments_data <- reactive({
      data <- assignments()
      switch(
        active_kind(),
        grade = data$grades,
        approval = data$approvals,
        qualifier = data$qualifiers
      )
    })

    render_active_table <- function(df, label) {
      if (is.null(df) || !nrow(df)) {
        msg <- sprintf("No %s have been recorded for this timeseries.", label)
        return(DT::datatable(
          data.frame(Message = msg),
          options = list(dom = 't'),
          selection = 'none'
        ))
      }
      display <- df
      display$Start <- vapply(display$start_dt, format_datetime, character(1))
      display$End <- vapply(display$end_dt, format_datetime, character(1))
      display$Created <- vapply(display$created, format_datetime, character(1))
      display$Updated <- vapply(display$updated, format_datetime, character(1))
      display$Color <- sprintf(
        '<span style="display:inline-block;width:18px;height:18px;border:1px solid #444;background-color:%s;"></span>',
        display$color_code
      )
      display <- display[, c(
        "record_id",
        "type_id",
        "description",
        "Start",
        "End",
        "Created",
        "Updated",
        "Color"
      )]
      names(display) <- c(
        "record_id",
        "type_id",
        "Description",
        "Start",
        "End",
        "Created",
        "Updated",
        "Color"
      )
      DT::datatable(
        display,
        selection = 'single',
        options = list(
          columnDefs = list(
            list(targets = c(0, 1), visible = FALSE),
            list(targets = 7, orderable = FALSE)
          ),
          pageLength = 5,
          lengthChange = FALSE,
          scrollX = TRUE
        ),
        escape = FALSE
      )
    }

    render_overview_table <- function(df, label) {
      if (is.null(df) || !nrow(df)) {
        msg <- sprintf("No %s have been recorded for this timeseries.", label)
        return(DT::datatable(
          data.frame(Message = msg),
          options = list(dom = 't'),
          selection = 'none'
        ))
      }
      display <- data.frame(
        Description = df$description,
        Start = vapply(df$start_dt, format_datetime, character(1)),
        End = vapply(df$end_dt, format_datetime, character(1)),
        Created = vapply(df$created, format_datetime, character(1)),
        Updated = vapply(df$updated, format_datetime, character(1)),
        Color = sprintf(
          '<span style="display:inline-block;width:18px;height:18px;border:1px solid #444;background-color:%s;"></span>',
          df$color_code
        ),
        stringsAsFactors = FALSE
      )
      DT::datatable(
        display,
        selection = 'none',
        options = list(
          columnDefs = list(list(targets = 5, orderable = FALSE)),
          pageLength = 5,
          lengthChange = FALSE,
          scrollX = TRUE
        ),
        escape = FALSE
      )
    }

    output$active_assignments <- DT::renderDT({
      req(selected_ts())
      label <- switch(
        active_kind(),
        grade = "grades",
        approval = "approvals",
        qualifier = "qualifiers"
      )
      render_active_table(active_assignments_data(), label)
    })

    output$grades_table <- DT::renderDT({
      req(selected_ts())
      render_overview_table(assignments()$grades, "grades")
    })

    output$approvals_table <- DT::renderDT({
      req(selected_ts())
      render_overview_table(assignments()$approvals, "approvals")
    })

    output$qualifiers_table <- DT::renderDT({
      req(selected_ts())
      render_overview_table(assignments()$qualifiers, "qualifiers")
    })

    observeEvent(
      input$active_assignments_rows_selected,
      {
        idx <- input$active_assignments_rows_selected
        df <- active_assignments_data()
        if (length(idx) == 1 && nrow(df) >= idx) {
          row <- df[idx, ]
          selected_record(list(
            id = row$record_id,
            type_id = row$type_id,
            start = row$start_dt,
            end = row$end_dt
          ))
          updateSelectizeInput(
            session,
            "attribute_value",
            selected = as.character(row$type_id)
          )
          updateTextInput(
            session,
            "start_dt",
            value = format_datetime(row$start_dt)
          )
          updateTextInput(
            session,
            "end_dt",
            value = format_datetime(row$end_dt)
          )
        }
      },
      ignoreNULL = TRUE
    )

    observeEvent(input$clear_selection, {
      selected_record(NULL)
      proxy <- DT::dataTableProxy("active_assignments", session = session)
      DT::selectRows(proxy, NULL)
      updateSelectizeInput(session, "attribute_value", selected = character(0))
    })

    observe({
      label <- if (is.null(selected_record())) {
        "Add attribute"
      } else {
        "Update attribute"
      }
      shiny::updateActionButton(session, "apply_attribute", label = label)
    })

    observeEvent(input$apply_attribute, {
      req(selected_ts())
      kind <- active_kind()
      record <- selected_record()
      type_id <- input$attribute_value
      if (!length(type_id)) {
        showNotification(
          "Select an attribute value before applying.",
          type = "warning"
        )
        return()
      }
      err <- range_error()
      if (!is.null(err)) {
        showNotification(err, type = "error")
        return()
      }
      start_dt <- parse_datetime(input$start_dt)
      end_dt <- parse_datetime(input$end_dt)
      if (is.na(start_dt) || is.na(end_dt)) {
        showNotification("Start or end datetime is invalid.", type = "error")
        return()
      }
      table_name <- switch(
        kind,
        grade = "continuous.grades",
        approval = "continuous.approvals",
        qualifier = "continuous.qualifiers"
      )
      id_col <- switch(
        kind,
        grade = "grade_id",
        approval = "approval_id",
        qualifier = "qualifier_id"
      )
      type_col <- switch(
        kind,
        grade = "grade_type_id",
        approval = "approval_type_id",
        qualifier = "qualifier_type_id"
      )
      if (is.null(record)) {
        if (!isTRUE(module_data$privileges[[paste0(kind, "_insert")]])) {
          showNotification(
            "You do not have permission to add records to this table.",
            type = "error"
          )
          return()
        }
        query <- sprintf(
          "INSERT INTO %s (timeseries_id, %s, start_dt, end_dt) VALUES ($1, $2, $3, $4)",
          table_name,
          type_col
        )
        res <- tryCatch(
          DBI::dbExecute(
            con,
            query,
            params = list(selected_ts(), as.integer(type_id), start_dt, end_dt)
          ),
          error = function(e) {
            showNotification(conditionMessage(e), type = "error")
            NULL
          }
        )
        if (!is.null(res)) {
          showNotification("Attribute added successfully.", type = "message")
          assignment_refresh(assignment_refresh() + 1)
          selected_record(NULL)
          proxy <- DT::dataTableProxy("active_assignments", session = session)
          DT::selectRows(proxy, NULL)
        }
      } else {
        if (!isTRUE(module_data$privileges[[paste0(kind, "_update")]])) {
          showNotification(
            "You do not have permission to update this record.",
            type = "error"
          )
          return()
        }
        query <- sprintf(
          "UPDATE %s SET %s = $1, start_dt = $2, end_dt = $3 WHERE %s = $4",
          table_name,
          type_col,
          id_col
        )
        res <- tryCatch(
          DBI::dbExecute(
            con,
            query,
            params = list(as.integer(type_id), start_dt, end_dt, record$id)
          ),
          error = function(e) {
            showNotification(conditionMessage(e), type = "error")
            NULL
          }
        )
        if (!is.null(res)) {
          showNotification("Attribute updated successfully.", type = "message")
          assignment_refresh(assignment_refresh() + 1)
          selected_record(NULL)
          proxy <- DT::dataTableProxy("active_assignments", session = session)
          DT::selectRows(proxy, NULL)
        }
      }
    })

    observeEvent(input$delete_attribute, {
      req(selected_ts())
      record <- selected_record()
      if (is.null(record)) {
        showNotification("Select a record to delete.", type = "warning")
        return()
      }
      kind <- active_kind()
      if (!isTRUE(module_data$privileges[[paste0(kind, "_delete")]])) {
        showNotification(
          "You do not have permission to delete this record.",
          type = "error"
        )
        return()
      }
      table_name <- switch(
        kind,
        grade = "continuous.grades",
        approval = "continuous.approvals",
        qualifier = "continuous.qualifiers"
      )
      id_col <- switch(
        kind,
        grade = "grade_id",
        approval = "approval_id",
        qualifier = "qualifier_id"
      )
      query <- sprintf("DELETE FROM %s WHERE %s = $1", table_name, id_col)
      res <- tryCatch(
        DBI::dbExecute(con, query, params = list(record$id)),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
      if (!is.null(res)) {
        showNotification("Attribute deleted successfully.", type = "message")
        assignment_refresh(assignment_refresh() + 1)
        selected_record(NULL)
        proxy <- DT::dataTableProxy("active_assignments", session = session)
        DT::selectRows(proxy, NULL)
      }
    })

    observeEvent(
      plotly::event_data("plotly_selected", source = ns("ts_plot")),
      {
        req(selected_ts())
        selection <- plotly::event_data(
          "plotly_selected",
          source = ns("ts_plot")
        )
        if (is.null(selection) || !nrow(selection)) {
          return()
        }
        times <- vapply(selection$x, to_posix_from_event, as.POSIXct(NA))
        times <- times[!is.na(times)]
        if (!length(times)) {
          return()
        }
        times <- sort(times)
        updateTextInput(session, "start_dt", value = format_datetime(times[1]))
        updateTextInput(
          session,
          "end_dt",
          value = format_datetime(times[length(times)])
        )
        next_edge("start")
      },
      ignoreNULL = TRUE
    )

    observeEvent(
      plotly::event_data("plotly_click", source = ns("ts_plot")),
      {
        req(selected_ts())
        click <- plotly::event_data("plotly_click", source = ns("ts_plot"))
        if (is.null(click) || is.null(click$x)) {
          return()
        }
        dt <- to_posix_from_event(click$x)
        if (is.na(dt)) {
          return()
        }
        if (identical(next_edge(), "start")) {
          updateTextInput(session, "start_dt", value = format_datetime(dt))
          next_edge("end")
        } else {
          updateTextInput(session, "end_dt", value = format_datetime(dt))
          next_edge("start")
        }
      },
      ignoreNULL = TRUE
    )

    output$ts_plot <- plotly::renderPlotly({
      req(selected_ts())
      err <- range_error()
      validate(need(is.null(err), err))
      df <- ts_data()
      plot_source <- ns("ts_plot")
      assignments_list <- assignments()
      if (!nrow(df)) {
        return(
          plotly::plotly_empty(
            type = "scatter",
            mode = "lines",
            source = plot_source
          ) %>%
            plotly::layout(
              title = NULL,
              xaxis = list(title = "Datetime"),
              yaxis = list(title = "Value"),
              dragmode = "select"
            )
        )
      }
      p <- plotly::plot_ly(
        data = df,
        x = ~datetime,
        y = ~value_raw,
        type = "scatter",
        mode = "lines",
        name = "Raw",
        source = plot_source,
        line = list(color = "#6C757D")
      )
      if ("value_corrected" %in% names(df)) {
        p <- plotly::add_lines(
          p,
          y = ~value_corrected,
          name = "Corrected",
          line = list(color = "#0072B2")
        )
      }
      y_vals <- c(df$value_raw, df$value_corrected)
      y_range <- range(y_vals, na.rm = TRUE)
      if (!all(is.finite(y_range))) {
        y_range <- c(0, 1)
      }
      if (diff(y_range) == 0) {
        y_range <- y_range + c(-0.5, 0.5)
      }
      shapes <- list()
      add_shapes <- function(data, opacity = 0.2) {
        if (is.null(data) || !nrow(data)) {
          return()
        }
        for (i in seq_len(nrow(data))) {
          row <- data[i, ]
          if (is.na(row$start_dt) || is.na(row$end_dt)) {
            next
          }
          fill_col <- tryCatch(
            grDevices::adjustcolor(row$color_code, alpha.f = opacity),
            error = function(e) {
              grDevices::adjustcolor("#cccccc", alpha.f = opacity)
            }
          )
          shapes[[length(shapes) + 1]] <- list(
            type = "rect",
            x0 = row$start_dt,
            x1 = row$end_dt,
            y0 = y_range[1],
            y1 = y_range[2],
            xref = "x",
            yref = "y",
            fillcolor = fill_col,
            line = list(width = 0)
          )
        }
      }
      add_shapes(assignments_list$grades, 0.12)
      add_shapes(assignments_list$approvals, 0.18)
      add_shapes(assignments_list$qualifiers, 0.24)
      if (length(shapes)) {
        p <- plotly::layout(p, shapes = shapes)
      }
      p <- plotly::layout(
        p,
        title = NULL,
        xaxis = list(title = "Datetime"),
        yaxis = list(title = "Value"),
        dragmode = "select",
        legend = list(orientation = "h", yanchor = "bottom", y = 1.02)
      )
      p
    })
  }) # End of moduleServer
}
