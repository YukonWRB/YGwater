# UI and server code for reviewing and editing continuous data attributes

continuousDataReviewUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(
      HTML(sprintf(
        "\n      #%s.accordion {\n        --bs-accordion-bg:          #FFFCF5;\n        --bs-accordion-btn-bg:      #FBE5B2;\n        --bs-accordion-active-bg:   #FBE5B2;\n      }\n    ",
        ns("accordion_ts")
      )),
      HTML(sprintf(
        "\n      #%s.accordion {\n        --bs-accordion-bg:          #E5F4F6;\n        --bs-accordion-btn-bg:      #0097A9;\n        --bs-accordion-active-bg:   #0097A9;\n      }\n    ",
        ns("accordion_review")
      )),
      HTML(sprintf(
        "\n      #%s.accordion {\n        --bs-accordion-bg:          #F1F4E5;\n        --bs-accordion-btn-bg:      #7A9A01;\n        --bs-accordion-active-bg:   #7A9A01;\n      }\n    ",
        ns("accordion_manage")
      ))
    ),
    page_fluid(
      uiOutput(ns("banner")),
      accordion(
        id = ns("accordion_ts"),
        open = "ts_panel",
        accordion_panel(
          id = ns("ts_panel"),
          title = "Timeseries selection",
          p(
            class = "text-muted",
            "Select a timeseries to review raw and corrected values before applying grades, approvals, qualifiers, or corrections."
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
              selectizeInput(
                ns("timezone"),
                "Input timezone",
                choices = input_timezone_choices(),
                selected = default_input_timezone(),
                multiple = FALSE,
                width = "100%"
              ),
              shinyWidgets::airDatepickerInput(
                ns("start_dt"),
                "Start datetime",
                value = NULL,
                range = FALSE,
                multiple = FALSE,
                timepicker = TRUE,
                update_on = "change",
                tz = air_datetime_widget_timezone(default_input_timezone()),
                timepickerOpts = shinyWidgets::timepickerOptions(
                  minutesStep = 15,
                  timeFormat = "HH:mm"
                )
              ),
              shinyWidgets::airDatepickerInput(
                ns("end_dt"),
                "End datetime",
                value = NULL,
                range = FALSE,
                multiple = FALSE,
                timepicker = TRUE,
                update_on = "change",
                tz = air_datetime_widget_timezone(default_input_timezone()),
                timepickerOpts = shinyWidgets::timepickerOptions(
                  minutesStep = 15,
                  timeFormat = "HH:mm"
                )
              ),
              div(
                class = "text-muted small",
                textOutput(ns("range_feedback"))
              ),
              div(
                class = "text-muted small mt-2",
                textOutput(ns("click_feedback"))
              ),
              hr(),
              checkboxInput(
                ns("show_attribute_bands"),
                "Show grades, approvals, qualifiers, and corrections",
                value = TRUE
              ),
              checkboxInput(
                ns("show_field_readings"),
                "Show field visit readings",
                value = TRUE
              ),
              checkboxInput(
                ns("show_instrument_events"),
                "Show instrument checks / calibrations",
                value = TRUE
              ),
              hr(),
              actionButton(ns("last_year"), label = "Most Recent Year"),
              actionButton(ns("entire_ts"), label = "Full Timeseries"),
              hr(),
              h5("Snap range"),
              DT::DTOutput(ns("snap_events")),
              fluidRow(
                column(4, actionButton(ns("snap_start"), "Set start")),
                column(4, actionButton(ns("snap_end"), "Set end")),
                column(4, actionButton(ns("snap_range"), "Set range"))
              )
            ),
            column(
              width = 8,
              plotly::plotlyOutput(ns("ts_plot"), height = "520px")
            )
          )
        )
      ),
      accordion(
        id = ns("accordion_manage"),
        open = FALSE,
        accordion_panel(
          id = ns("manage_panel"),
          title = "Manage grades, approvals, qualifiers, and corrections",
          fluidRow(
            column(
              width = 4,
              radioButtons(
                ns("attribute_kind"),
                "Attribute to modify",
                choices = c(
                  "Grades" = "grade",
                  "Approvals" = "approval",
                  "Qualifiers" = "qualifier",
                  "Corrections" = "correction"
                ),
                inline = FALSE
              ),
              conditionalPanel(
                ns = ns,
                condition = "input.attribute_kind != 'correction'",
                selectizeInput(
                  ns("attribute_value"),
                  "Attribute value",
                  choices = NULL,
                  multiple = FALSE,
                  options = list(placeholder = "Select a value")
                )
              ),
              conditionalPanel(
                ns = ns,
                condition = "input.attribute_kind == 'correction'",
                selectizeInput(
                  ns("correction_type"),
                  "Correction type",
                  choices = NULL,
                  multiple = FALSE,
                  options = list(placeholder = "Select a correction")
                ),
                numericInput(
                  ns("correction_value1"),
                  "Value 1",
                  value = NA,
                  width = "100%"
                ),
                numericInput(
                  ns("correction_value2"),
                  "Value 2",
                  value = NA,
                  width = "100%"
                ),
                numericInput(
                  ns("correction_window"),
                  "Time window (seconds)",
                  value = NA,
                  width = "100%"
                ),
                textInput(
                  ns("correction_equation"),
                  "Equation",
                  width = "100%"
                )
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
              h5("Records overlapping selected range"),
              DT::DTOutput(ns("selected_range_records")),
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
                ),
                accordion_panel(
                  id = ns("existing_corrections"),
                  title = "Existing corrections",
                  DT::DTOutput(ns("corrections_table"))
                )
              )
            )
          )
        )
      )
    )
  )
}

continuousDataReview <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "grades_approvals_qualifiers"
      )
    })

    format_datetime <- function(value) {
      if (is.null(value) || !length(value) || is.na(value)) {
        return("")
      }
      value <- as.POSIXct(value, tz = "UTC")
      format(value, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    }

    format_number <- function(value) {
      if (is.null(value) || !length(value) || is.na(value)) {
        return("")
      }
      format(value, trim = TRUE, scientific = FALSE)
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

    safe_query <- function(expr) {
      tryCatch(expr, error = function(e) {
        warning(conditionMessage(e))
        data.frame()
      })
    }

    shift_range_datetime_inputs <- function(tz_name) {
      shift_air_datetime_input_timezone(session, input, "start_dt", tz_name)
      shift_air_datetime_input_timezone(session, input, "end_dt", tz_name)
    }

    set_datetime_input <- function(input_id, value) {
      if (is.null(value) || !length(value) || is.na(value)) {
        return(invisible(NULL))
      }
      shinyWidgets::updateAirDateInput(
        session,
        input_id,
        value = coerce_utc_datetime(value),
        tz = air_datetime_widget_timezone(input$timezone)
      )
    }

    set_range_inputs <- function(start_dt, end_dt) {
      set_datetime_input("start_dt", start_dt)
      set_datetime_input("end_dt", end_dt)
      next_edge("start")
    }

    load_privileges <- function() {
      query <- "SELECT
        has_table_privilege(current_user, 'continuous.grades', 'INSERT') AS grade_insert,
        has_table_privilege(current_user, 'continuous.grades', 'UPDATE') AS grade_update,
        has_table_privilege(current_user, 'continuous.grades', 'DELETE') AS grade_delete,
        has_table_privilege(current_user, 'continuous.approvals', 'INSERT') AS approval_insert,
        has_table_privilege(current_user, 'continuous.approvals', 'UPDATE') AS approval_update,
        has_table_privilege(current_user, 'continuous.approvals', 'DELETE') AS approval_delete,
        has_table_privilege(current_user, 'continuous.qualifiers', 'INSERT') AS qualifier_insert,
        has_table_privilege(current_user, 'continuous.qualifiers', 'UPDATE') AS qualifier_update,
        has_table_privilege(current_user, 'continuous.qualifiers', 'DELETE') AS qualifier_delete,
        has_table_privilege(current_user, 'continuous.corrections', 'INSERT') AS correction_insert,
        has_table_privilege(current_user, 'continuous.corrections', 'UPDATE') AS correction_update,
        has_table_privilege(current_user, 'continuous.corrections', 'DELETE') AS correction_delete"
      DBI::dbGetQuery(session$userData$AquaCache, query)
    }

    privileges_row <- safe_query(load_privileges())
    privileges <- if (nrow(privileges_row)) {
      as.list(privileges_row[1, , drop = FALSE])
    } else {
      list()
    }

    if (!any(unlist(privileges))) {
      showModal(
        modalDialog(
          title = "Insufficient privileges",
          "You do not have write privileges for the grades, approvals, qualifiers, or corrections tables.",
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    }

    module_data <- reactiveValues(
      grade_types = safe_query(
        DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT grade_type_id, grade_type_description,
                  COALESCE(color_code, '#4E79A7') AS color_code
           FROM public.grade_types
           ORDER BY grade_type_description"
        )
      ),
      approval_types = safe_query(
        DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT approval_type_id, approval_type_description,
                  COALESCE(color_code, '#F28E2B') AS color_code
           FROM public.approval_types
           ORDER BY approval_type_description"
        )
      ),
      qualifier_types = safe_query(
        DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT qualifier_type_id, qualifier_type_description,
                  COALESCE(color_code, '#E15759') AS color_code
           FROM public.qualifier_types
           ORDER BY qualifier_type_description"
        )
      ),
      correction_types = safe_query(
        DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT correction_type_id, correction_type, description, priority,
                  value1, value1_description, value2, value2_description,
                  timestep_window, equation
           FROM continuous.correction_types
           ORDER BY priority, correction_type"
        )
      ),
      privileges = privileges
    )

    ts_meta <- reactive({
      dbGetQueryDT(
        session$userData$AquaCache,
        paste(
          "SELECT",
          "  tm.timeseries_id,",
          "  ts.location_id,",
          "  ts.sub_location_id,",
          "  ts.parameter_id,",
          "  ts.media_id,",
          "  tm.location_name AS location,",
          "  tm.parameter_name AS parameter,",
          "  tm.media_type AS media,",
          "  tm.aggregation_type AS aggregation,",
          "  tm.recording_rate,",
          "  tm.start_datetime,",
          "  tm.end_datetime,",
          "  tm.note",
          "FROM continuous.timeseries_metadata_en AS tm",
          "INNER JOIN continuous.timeseries AS ts",
          "  ON ts.timeseries_id = tm.timeseries_id"
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
          columnDefs = list(list(targets = 0:4, visible = FALSE)),
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
    pending_action <- reactiveVal(NULL)

    observeEvent(
      input$ts_table_rows_selected,
      {
        selection <- input$ts_table_rows_selected
        if (length(selection)) {
          row <- ts_meta()[selection, ]
          tsid <- row$timeseries_id[[1]]
          selected_ts(tsid)
          assignment_refresh(assignment_refresh() + 1)
          selected_record(NULL)
          next_edge("start")

          default_end <- if (!is.na(row$end_datetime[[1]])) {
            as.POSIXct(row$end_datetime[[1]], tz = "UTC")
          } else {
            Sys.time()
          }
          default_start <- default_end - 60 * 24 * 3600
          if (!is.na(row$start_datetime[[1]])) {
            min_dt <- as.POSIXct(row$start_datetime[[1]], tz = "UTC")
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
          set_range_inputs(default_start, default_end)
        } else {
          selected_ts(NULL)
        }
      },
      ignoreNULL = FALSE
    )

    observeEvent(
      input$timezone,
      {
        shift_range_datetime_inputs(normalize_input_timezone(input$timezone))
      },
      ignoreInit = TRUE
    )

    observeEvent(input$entire_ts, {
      req(selected_ts())
      row <- ts_meta()[ts_meta()$timeseries_id == selected_ts(), ]
      if (!nrow(row)) {
        return()
      }
      ts_start <- if (!is.na(row$start_datetime[[1]])) {
        as.POSIXct(row$start_datetime[[1]], tz = "UTC")
      } else {
        Sys.time() - 60 * 24 * 3600
      }
      ts_end <- if (!is.na(row$end_datetime[[1]])) {
        as.POSIXct(row$end_datetime[[1]], tz = "UTC")
      } else {
        Sys.time()
      }
      if (
        !is.finite(as.numeric(ts_start)) ||
          !is.finite(as.numeric(ts_end))
      ) {
        ts_start <- Sys.time() - 7 * 24 * 3600
        ts_end <- Sys.time()
      }
      if (ts_start >= ts_end) {
        ts_start <- ts_end - 1 * 3600
      }
      set_range_inputs(ts_start, ts_end)
    })

    observeEvent(input$last_year, {
      req(selected_ts())
      row <- ts_meta()[ts_meta()$timeseries_id == selected_ts(), ]
      if (!nrow(row)) {
        return()
      }
      ts_end <- if (!is.na(row$end_datetime[[1]])) {
        as.POSIXct(row$end_datetime[[1]], tz = "UTC")
      } else {
        Sys.time()
      }
      year_start <- ts_end - 365 * 24 * 3600
      if (!is.na(row$start_datetime[[1]])) {
        min_dt <- as.POSIXct(row$start_datetime[[1]], tz = "UTC")
        if (!is.na(min_dt) && year_start < min_dt) {
          year_start <- min_dt
          showNotification(sprintf(
            "Earliest entry in time series displayed (%s)",
            min_dt
          ))
        }
      }
      if (
        !is.finite(as.numeric(year_start)) ||
          !is.finite(as.numeric(ts_end))
      ) {
        year_start <- Sys.time() - 7 * 24 * 3600
        ts_end <- Sys.time()
      }
      if (year_start >= ts_end) {
        year_start <- ts_end - 1 * 3600
      }
      set_range_inputs(year_start, ts_end)
    })

    range_error <- reactive({
      if (is.null(selected_ts())) {
        return(NULL)
      }
      start_dt <- scalar_utc_datetime(input$start_dt)
      if (is.na(start_dt)) {
        return("Select a start datetime.")
      }
      end_dt <- scalar_utc_datetime(input$end_dt)
      if (is.na(end_dt)) {
        return("Select an end datetime.")
      }
      if (start_dt >= end_dt) {
        return("Start datetime must be before end datetime.")
      }
      NULL
    })

    selected_range <- reactive({
      err <- range_error()
      if (!is.null(err)) {
        return(NULL)
      }
      list(
        start = scalar_utc_datetime(input$start_dt),
        end = scalar_utc_datetime(input$end_dt)
      )
    })

    plot_range <- reactive({
      rng <- selected_range()
      if (is.null(rng)) {
        return(NULL)
      }
      span <- as.numeric(difftime(rng$end, rng$start, units = "secs"))
      if (!is.finite(span) || span <= 0) {
        span <- 24 * 3600
      }
      pad <- max(0.2 * span, 3600)
      view_start <- rng$start - pad
      view_end <- rng$end + pad

      meta_row <- ts_meta()[ts_meta()$timeseries_id == selected_ts(), ]
      if (nrow(meta_row)) {
        if (!is.na(meta_row$start_datetime[[1]])) {
          ts_start <- as.POSIXct(meta_row$start_datetime[[1]], tz = "UTC")
          if (!is.na(ts_start) && view_start < ts_start) {
            view_start <- ts_start
          }
        }
        if (!is.na(meta_row$end_datetime[[1]])) {
          ts_end <- as.POSIXct(meta_row$end_datetime[[1]], tz = "UTC")
          if (!is.na(ts_end) && view_end > ts_end) {
            view_end <- ts_end
          }
        }
      }
      if (view_start >= view_end) {
        view_start <- rng$start
        view_end <- rng$end
      }
      list(start = view_start, end = view_end)
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
        " datetime. Drag across the plot to fill both start and end. The shaded band is the selected range."
      )
    })

    load_assignments <- function(kind, tsid) {
      if (is.null(tsid)) {
        return(data.frame())
      }
      query <- switch(
        kind,
        grade = "SELECT g.grade_id AS record_id,
                        g.grade_type_id AS type_id,
                        gt.grade_type_description AS description,
                        COALESCE(gt.color_code, '#4E79A7') AS color_code,
                        g.start_dt, g.end_dt, g.created, g.modified AS updated
                 FROM continuous.grades AS g
                 LEFT JOIN public.grade_types AS gt
                   ON g.grade_type_id = gt.grade_type_id
                 WHERE g.timeseries_id = $1
                 ORDER BY g.start_dt",
        approval = "SELECT a.approval_id AS record_id,
                           a.approval_type_id AS type_id,
                           at.approval_type_description AS description,
                           COALESCE(at.color_code, '#F28E2B') AS color_code,
                           a.start_dt, a.end_dt, a.created, a.modified AS updated
                    FROM continuous.approvals AS a
                    LEFT JOIN public.approval_types AS at
                      ON a.approval_type_id = at.approval_type_id
                    WHERE a.timeseries_id = $1
                    ORDER BY a.start_dt",
        qualifier = "SELECT q.qualifier_id AS record_id,
                            q.qualifier_type_id AS type_id,
                            qt.qualifier_type_description AS description,
                            COALESCE(qt.color_code, '#E15759') AS color_code,
                            q.start_dt, q.end_dt, q.created, q.modified AS updated
                     FROM continuous.qualifiers AS q
                     LEFT JOIN public.qualifier_types AS qt
                       ON q.qualifier_type_id = qt.qualifier_type_id
                     WHERE q.timeseries_id = $1
                     ORDER BY q.start_dt",
        correction = "SELECT c.correction_id AS record_id,
                             c.correction_type AS type_id,
                             ct.correction_type AS description,
                             '#7A9A01' AS color_code,
                             c.start_dt, c.end_dt,
                             c.created, c.modified AS updated,
                             ct.priority, c.value1, c.value2,
                             EXTRACT(EPOCH FROM c.timestep_window)::integer
                               AS timestep_window_seconds,
                             c.equation
                      FROM continuous.corrections AS c
                      LEFT JOIN continuous.correction_types AS ct
                        ON ct.correction_type_id = c.correction_type
                      WHERE c.timeseries_id = $1
                      ORDER BY c.start_dt, ct.priority, c.correction_id"
      )
      df <- safe_query(DBI::dbGetQuery(
        session$userData$AquaCache,
        query,
        params = list(tsid)
      ))
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
        qualifiers = load_assignments("qualifier", tsid),
        corrections = load_assignments("correction", tsid)
      )
    })

    ts_data <- reactive({
      if (is.null(selected_ts()) || !is.null(range_error())) {
        return(data.frame())
      }
      rng <- plot_range()
      if (is.null(rng)) {
        return(data.frame())
      }
      payload <- tryCatch(
        plotTimeseries(
          timeseries_id = selected_ts(),
          start_date = rng$start,
          end_date = rng$end,
          historic_range = FALSE,
          raw = TRUE,
          unusable = TRUE,
          slider = FALSE,
          title = FALSE,
          tzone = "UTC",
          data = TRUE,
          build_plot = FALSE,
          con = session$userData$AquaCache
        ),
        error = function(e) {
          warning(conditionMessage(e))
          NULL
        }
      )
      if (is.null(payload) || is.null(payload$data$trace_data)) {
        return(data.frame())
      }
      df <- as.data.frame(payload$data$trace_data)
      if (!nrow(df)) {
        return(df)
      }
      df$datetime <- as.POSIXct(df$datetime, tz = "UTC")
      if ("value" %in% names(df)) {
        names(df)[names(df) == "value"] <- "value_corrected"
      }
      if (!"value_raw" %in% names(df)) {
        df$value_raw <- NA_real_
      }
      df
    })

    field_visits <- reactive({
      if (is.null(selected_ts()) || !isTRUE(input$show_field_readings)) {
        return(data.frame())
      }
      rng <- plot_range()
      if (is.null(rng)) {
        return(data.frame())
      }
      df <- safe_query(DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT v.field_visit_id AS record_id,
                v.start_datetime AS start_dt,
                COALESCE(v.end_datetime, v.start_datetime) AS end_dt,
                COALESCE(NULLIF(v.purpose, ''), 'Field visit') AS description
         FROM field.field_visits AS v
         INNER JOIN continuous.timeseries AS ts
           ON ts.timeseries_id = $1
         WHERE v.location_id = ts.location_id
           AND v.sub_location_id IS NOT DISTINCT FROM ts.sub_location_id
           AND v.start_datetime <= $3
           AND COALESCE(v.end_datetime, v.start_datetime) >= $2
         ORDER BY v.start_datetime",
        params = list(selected_ts(), rng$start, rng$end)
      ))
      if (nrow(df)) {
        df$start_dt <- as.POSIXct(df$start_dt, tz = "UTC")
        df$end_dt <- as.POSIXct(df$end_dt, tz = "UTC")
      }
      df
    })

    field_readings <- reactive({
      if (is.null(selected_ts()) || !isTRUE(input$show_field_readings)) {
        return(data.frame())
      }
      rng <- plot_range()
      if (is.null(rng)) {
        return(data.frame())
      }
      df <- safe_query(DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT s.sample_id AS record_id,
                s.field_visit_id,
                s.datetime,
                r.result::numeric AS value,
                COALESCE(NULLIF(s.note, ''), 'Field visit reading') AS note
         FROM discrete.samples AS s
         INNER JOIN discrete.results AS r
           ON r.sample_id = s.sample_id
         INNER JOIN continuous.timeseries AS ts
           ON ts.timeseries_id = $1
         WHERE s.field_visit_id IS NOT NULL
           AND s.location_id = ts.location_id
           AND s.sub_location_id IS NOT DISTINCT FROM ts.sub_location_id
           AND s.media_id = ts.media_id
           AND r.parameter_id = ts.parameter_id
           AND s.datetime BETWEEN $2 AND $3
           AND r.result IS NOT NULL
         ORDER BY s.datetime",
        params = list(selected_ts(), rng$start, rng$end)
      ))
      if (nrow(df)) {
        df$datetime <- as.POSIXct(df$datetime, tz = "UTC")
      }
      df
    })

    instrument_events <- reactive({
      if (is.null(selected_ts()) || !isTRUE(input$show_instrument_events)) {
        return(data.frame())
      }
      rng <- plot_range()
      if (is.null(rng)) {
        return(data.frame())
      }
      df <- safe_query(DBI::dbGetQuery(
        session$userData$AquaCache,
        "WITH deployments AS (
           SELECT DISTINCT
                  lmi.metadata_id,
                  lmi.instrument_id,
                  lmi.start_datetime,
                  lmi.end_datetime
           FROM public.locations_metadata_instruments AS lmi
           WHERE EXISTS (
             SELECT 1
             FROM public.locations_metadata_instrument_timeseries AS lmit
             WHERE lmit.metadata_id = lmi.metadata_id
               AND lmit.timeseries_id = $1
           )
           OR EXISTS (
             SELECT 1
             FROM public.locations_metadata_instrument_connections AS c
             INNER JOIN public.locations_metadata_instrument_connection_signals AS s
               ON s.connection_id = c.connection_id
             WHERE c.instrument_metadata_id = lmi.metadata_id
               AND s.timeseries_id = $1
           )
         )
         SELECT c.calibration_id AS record_id,
                c.obs_datetime AS datetime,
                CASE
                  WHEN BOOL_OR(COALESCE(parts.check_only, FALSE))
                    THEN 'Instrument check'
                  ELSE 'Instrument calibration'
                END AS event_type,
                COALESCE(NULLIF(c.purpose, ''), 'No purpose') AS purpose,
                COALESCE(i.serial_no, i.asset_tag, c.id_sensor_holder::text)
                  AS instrument,
                STRING_AGG(DISTINCT parts.component, ', ' ORDER BY parts.component)
                  AS components
         FROM instruments.calibrations AS c
         INNER JOIN deployments AS d
           ON c.id_sensor_holder = d.instrument_id
              OR c.id_handheld_meter = d.instrument_id
         LEFT JOIN instruments.instruments AS i
           ON i.instrument_id = d.instrument_id
         LEFT JOIN LATERAL (
           SELECT 'temperature' AS component, FALSE AS check_only
           FROM instruments.calibrate_temperature AS x
           WHERE x.calibration_id = c.calibration_id
           UNION ALL
           SELECT 'specific conductance', COALESCE(x.check_only, FALSE)
           FROM instruments.calibrate_specific_conductance AS x
           WHERE x.calibration_id = c.calibration_id
           UNION ALL
           SELECT 'pH', COALESCE(x.check_only, FALSE)
           FROM instruments.calibrate_ph AS x
           WHERE x.calibration_id = c.calibration_id
           UNION ALL
           SELECT 'ORP', COALESCE(x.check_only, FALSE)
           FROM instruments.calibrate_orp AS x
           WHERE x.calibration_id = c.calibration_id
           UNION ALL
           SELECT 'turbidity', COALESCE(x.check_only, FALSE)
           FROM instruments.calibrate_turbidity AS x
           WHERE x.calibration_id = c.calibration_id
           UNION ALL
           SELECT 'dissolved oxygen', COALESCE(x.check_only, FALSE)
           FROM instruments.calibrate_dissolved_oxygen AS x
           WHERE x.calibration_id = c.calibration_id
           UNION ALL
           SELECT 'depth', TRUE
           FROM instruments.calibrate_depth AS x
           WHERE x.calibration_id = c.calibration_id
         ) AS parts ON TRUE
         WHERE c.complete IS TRUE
           AND c.obs_datetime BETWEEN $2 AND $3
           AND d.start_datetime <= $3
           AND (d.end_datetime IS NULL OR d.end_datetime >= $2)
         GROUP BY c.calibration_id, c.obs_datetime, c.purpose,
                  i.serial_no, i.asset_tag, c.id_sensor_holder
         ORDER BY c.obs_datetime",
        params = list(selected_ts(), rng$start, rng$end)
      ))
      if (nrow(df)) {
        df$datetime <- as.POSIXct(df$datetime, tz = "UTC")
        df$components[is.na(df$components)] <- ""
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

    correction_type_row <- reactive({
      req(active_kind() == "correction")
      type_id <- input$correction_type
      types <- module_data$correction_types
      if (is.null(type_id) || !length(type_id) || !nrow(types)) {
        return(NULL)
      }
      types[types$correction_type_id == as.integer(type_id), , drop = FALSE]
    })

    observe({
      kind <- active_kind()
      if (identical(kind, "correction")) {
        types <- module_data$correction_types
        choices <- if (!is.null(types) && nrow(types)) {
          stats::setNames(
            as.character(types$correction_type_id),
            paste0(types$correction_type, " (priority ", types$priority, ")")
          )
        } else {
          character(0)
        }
        current <- selected_record()
        selected_value <- if (!is.null(current) && !is.null(current$type_id)) {
          as.character(current$type_id)
        } else {
          character(0)
        }
        updateSelectizeInput(
          session,
          "correction_type",
          choices = choices,
          selected = selected_value,
          server = TRUE
        )
        return()
      }

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

    update_correction_inputs <- function(record = NULL) {
      row <- correction_type_row()
      if (is.null(row) || !nrow(row)) {
        return(invisible(NULL))
      }
      show_input <- function(input_id, show) {
        if (isTRUE(show)) {
          shinyjs::show(input_id)
        } else {
          shinyjs::hide(input_id)
        }
      }

      show_input("correction_value1", isTRUE(row$value1[[1]]))
      show_input(
        "correction_value2",
        isTRUE(row$value2[[1]]) || is.na(row$value2[[1]])
      )
      show_input("correction_window", isTRUE(row$timestep_window[[1]]))
      show_input("correction_equation", isTRUE(row$equation[[1]]))

      if (isTRUE(row$value1[[1]])) {
        updateNumericInput(
          session,
          "correction_value1",
          label = row$value1_description[[1]]
        )
      }
      if (isTRUE(row$value2[[1]]) || is.na(row$value2[[1]])) {
        label <- row$value2_description[[1]]
        if (is.na(row$value2[[1]])) {
          label <- paste0(label, " (optional)")
        }
        updateNumericInput(session, "correction_value2", label = label)
      }
      updateNumericInput(session, "correction_window", label = "Time window (seconds)")

      if (!is.null(record)) {
        updateNumericInput(session, "correction_value1", value = record$value1)
        updateNumericInput(session, "correction_value2", value = record$value2)
        updateNumericInput(
          session,
          "correction_window",
          value = record$timestep_window_seconds
        )
        updateTextInput(
          session,
          "correction_equation",
          value = if (is.na(record$equation)) "" else record$equation
        )
      }
      invisible(NULL)
    }

    observeEvent(input$correction_type, {
      update_correction_inputs()
    }, ignoreNULL = TRUE)

    observeEvent(
      active_kind(),
      {
        selected_record(NULL)
        next_edge("start")
        proxy <- DT::dataTableProxy("active_assignments")
        DT::selectRows(proxy, NULL)
      },
      ignoreNULL = FALSE
    )

    output$active_table_title <- renderText({
      switch(
        active_kind(),
        grade = "Selected grade assignments",
        approval = "Selected approval assignments",
        qualifier = "Selected qualifier assignments",
        correction = "Selected corrections"
      )
    })

    active_assignments_data <- reactive({
      data <- assignments()
      switch(
        active_kind(),
        grade = data$grades,
        approval = data$approvals,
        qualifier = data$qualifiers,
        correction = data$corrections
      )
    })

    render_active_table <- function(df, label, kind) {
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

      if (identical(kind, "correction")) {
        display$Value1 <- vapply(display$value1, format_number, character(1))
        display$Value2 <- vapply(display$value2, format_number, character(1))
        display$Window_seconds <- vapply(
          display$timestep_window_seconds,
          format_number,
          character(1)
        )
        display$Equation <- ifelse(is.na(display$equation), "", display$equation)
        display <- display[, c(
          "record_id",
          "type_id",
          "description",
          "priority",
          "Start",
          "End",
          "Value1",
          "Value2",
          "Window_seconds",
          "Equation",
          "Created",
          "Updated"
        )]
        names(display) <- c(
          "record_id",
          "type_id",
          "Correction",
          "Priority",
          "Start",
          "End",
          "Value 1",
          "Value 2",
          "Window seconds",
          "Equation",
          "Created",
          "Updated"
        )
        return(DT::datatable(
          display,
          selection = 'single',
          options = list(
            columnDefs = list(list(targets = c(0, 1), visible = FALSE)),
            pageLength = 5,
            lengthChange = FALSE,
            scrollX = TRUE
          ),
          escape = TRUE
        ))
      }

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

    render_overview_table <- function(df, label, kind) {
      if (is.null(df) || !nrow(df)) {
        msg <- sprintf("No %s have been recorded for this timeseries.", label)
        return(DT::datatable(
          data.frame(Message = msg),
          options = list(dom = 't'),
          selection = 'none'
        ))
      }

      if (identical(kind, "correction")) {
        display <- data.frame(
          Correction = df$description,
          Priority = df$priority,
          Start = vapply(df$start_dt, format_datetime, character(1)),
          End = vapply(df$end_dt, format_datetime, character(1)),
          Value1 = vapply(df$value1, format_number, character(1)),
          Value2 = vapply(df$value2, format_number, character(1)),
          Window_seconds = vapply(
            df$timestep_window_seconds,
            format_number,
            character(1)
          ),
          Equation = ifelse(is.na(df$equation), "", df$equation),
          stringsAsFactors = FALSE
        )
        return(DT::datatable(
          display,
          selection = 'none',
          options = list(
            pageLength = 5,
            lengthChange = FALSE,
            scrollX = TRUE
          )
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
      kind <- active_kind()
      label <- switch(
        kind,
        grade = "grades",
        approval = "approvals",
        qualifier = "qualifiers",
        correction = "corrections"
      )
      render_active_table(active_assignments_data(), label, kind)
    })

    output$grades_table <- DT::renderDT({
      req(selected_ts())
      render_overview_table(assignments()$grades, "grades", "grade")
    })

    output$approvals_table <- DT::renderDT({
      req(selected_ts())
      render_overview_table(assignments()$approvals, "approvals", "approval")
    })

    output$qualifiers_table <- DT::renderDT({
      req(selected_ts())
      render_overview_table(assignments()$qualifiers, "qualifiers", "qualifier")
    })

    output$corrections_table <- DT::renderDT({
      req(selected_ts())
      render_overview_table(assignments()$corrections, "corrections", "correction")
    })

    overlap_records <- function(df, start_dt, end_dt, exclude_id = NULL) {
      if (is.null(df) || !nrow(df)) {
        return(df)
      }
      keep <- df$start_dt <= end_dt & df$end_dt >= start_dt
      keep[is.na(keep)] <- FALSE
      if (!is.null(exclude_id) && "record_id" %in% names(df)) {
        keep <- keep & df$record_id != exclude_id
      }
      df[keep, , drop = FALSE]
    }

    selected_range_records_data <- reactive({
      req(selected_ts())
      rng <- selected_range()
      if (is.null(rng)) {
        return(data.frame())
      }
      data <- assignments()
      bind_rows <- list(
        grade = overlap_records(data$grades, rng$start, rng$end),
        approval = overlap_records(data$approvals, rng$start, rng$end),
        qualifier = overlap_records(data$qualifiers, rng$start, rng$end),
        correction = overlap_records(data$corrections, rng$start, rng$end)
      )
      out <- data.frame()
      for (kind in names(bind_rows)) {
        df <- bind_rows[[kind]]
        if (is.null(df) || !nrow(df)) {
          next
        }
        tmp <- data.frame(
          Type = kind,
          Description = df$description,
          Start = vapply(df$start_dt, format_datetime, character(1)),
          End = vapply(df$end_dt, format_datetime, character(1)),
          stringsAsFactors = FALSE
        )
        out <- rbind(out, tmp)
      }
      out
    })

    output$selected_range_records <- DT::renderDT({
      df <- selected_range_records_data()
      if (!nrow(df)) {
        return(DT::datatable(
          data.frame(Message = "No records overlap the selected range."),
          options = list(dom = 't'),
          selection = 'none'
        ))
      }
      DT::datatable(
        df,
        selection = 'none',
        options = list(pageLength = 5, lengthChange = FALSE, scrollX = TRUE)
      )
    })

    observeEvent(
      input$active_assignments_rows_selected,
      {
        idx <- input$active_assignments_rows_selected
        df <- active_assignments_data()
        if (length(idx) == 1 && nrow(df) >= idx) {
          row <- df[idx, , drop = FALSE]
          record <- as.list(row)
          selected_record(record)

          if (identical(active_kind(), "correction")) {
            updateSelectizeInput(
              session,
              "correction_type",
              selected = as.character(row$type_id)
            )
            update_correction_inputs(record)
          } else {
            updateSelectizeInput(
              session,
              "attribute_value",
              selected = as.character(row$type_id)
            )
          }
          set_range_inputs(row$start_dt, row$end_dt)
        }
      },
      ignoreNULL = TRUE
    )

    observeEvent(input$clear_selection, {
      selected_record(NULL)
      proxy <- DT::dataTableProxy("active_assignments")
      DT::selectRows(proxy, NULL)
      updateSelectizeInput(session, "attribute_value", selected = character(0))
      updateSelectizeInput(session, "correction_type", selected = character(0))
      updateNumericInput(session, "correction_value1", value = NA)
      updateNumericInput(session, "correction_value2", value = NA)
      updateNumericInput(session, "correction_window", value = NA)
      updateTextInput(session, "correction_equation", value = "")
    })

    observe({
      label <- if (is.null(selected_record())) {
        if (identical(active_kind(), "correction")) "Add correction" else "Add attribute"
      } else {
        if (identical(active_kind(), "correction")) "Update correction" else "Update attribute"
      }
      shiny::updateActionButton(session, "apply_attribute", label = label)
    })

    correction_form_values <- function() {
      row <- correction_type_row()
      if (is.null(row) || !nrow(row)) {
        stop("Select a correction type before applying.")
      }

      value1 <- if (isTRUE(row$value1[[1]])) input$correction_value1 else NA_real_
      value2 <- if (isTRUE(row$value2[[1]]) || is.na(row$value2[[1]])) {
        input$correction_value2
      } else {
        NA_real_
      }
      window <- if (isTRUE(row$timestep_window[[1]])) {
        input$correction_window
      } else {
        NA_integer_
      }
      equation <- if (isTRUE(row$equation[[1]]) && nzchar(input$correction_equation)) {
        input$correction_equation
      } else {
        NA_character_
      }

      if (isTRUE(row$value1[[1]]) && is.na(value1)) {
        stop("Value 1 is required for this correction type.")
      }
      if (isTRUE(row$value2[[1]]) && is.na(value2)) {
        stop("Value 2 is required for this correction type.")
      }
      if (isTRUE(row$timestep_window[[1]]) && is.na(window)) {
        stop("Time window is required for this correction type.")
      }
      if (isTRUE(row$equation[[1]]) && is.na(equation)) {
        stop("Equation is required for this correction type.")
      }

      list(
        type_id = as.integer(input$correction_type),
        priority = row$priority[[1]],
        type_label = row$correction_type[[1]],
        value1 = value1,
        value2 = value2,
        window = if (is.na(window)) NA_integer_ else as.integer(window),
        equation = equation
      )
    }

    describe_overlaps <- function(df) {
      if (is.null(df) || !nrow(df)) {
        return(character())
      }
      shown <- utils::head(df, 5)
      lines <- paste0(
        shown$description,
        " (",
        vapply(shown$start_dt, format_datetime, character(1)),
        " to ",
        vapply(shown$end_dt, format_datetime, character(1)),
        ")"
      )
      if (nrow(df) > nrow(shown)) {
        lines <- c(lines, sprintf("...and %d more.", nrow(df) - nrow(shown)))
      }
      lines
    }

    confirmation_message <- function(action) {
      data <- assignments()
      current <- switch(
        action$kind,
        grade = data$grades,
        approval = data$approvals,
        qualifier = data$qualifiers,
        correction = data$corrections
      )
      exclude_id <- if (!is.null(action$record)) action$record$record_id else NULL
      overlaps <- overlap_records(
        current,
        action$start_dt,
        action$end_dt,
        exclude_id = exclude_id
      )

      if (action$kind %in% c("grade", "approval") && nrow(overlaps)) {
        return(tagList(
          p(sprintf(
            "The selected range overlaps existing %s records. Applying this change may overwrite records or adjust their bounds.",
            paste0(action$kind, "s")
          )),
          tags$ul(lapply(describe_overlaps(overlaps), tags$li))
        ))
      }

      if (
        identical(action$kind, "qualifier") &&
          is.null(action$record) &&
          nrow(overlaps)
      ) {
        return(tagList(
          p("Qualifiers can overlap. This will add another qualifier over a range that already has qualifiers."),
          tags$ul(lapply(describe_overlaps(overlaps), tags$li))
        ))
      }

      if (identical(action$kind, "correction")) {
        overlap_text <- if (nrow(overlaps)) {
          tags$ul(lapply(describe_overlaps(overlaps), tags$li))
        } else {
          p("No existing corrections overlap this range.")
        }
        return(tagList(
          p(sprintf(
            "Corrections are additive and are evaluated by priority. This correction type has priority %s.",
            action$correction$priority
          )),
          overlap_text
        ))
      }

      NULL
    }

    commit_action <- function(action) {
      kind <- action$kind
      record <- action$record

      if (is.null(record)) {
        if (!isTRUE(module_data$privileges[[paste0(kind, "_insert")]])) {
          showNotification(
            "You do not have permission to add records to this table.",
            type = "error"
          )
          return(FALSE)
        }
      } else {
        if (!isTRUE(module_data$privileges[[paste0(kind, "_update")]])) {
          showNotification(
            "You do not have permission to update this record.",
            type = "error"
          )
          return(FALSE)
        }
      }

      if (identical(kind, "correction")) {
        correction <- action$correction
        if (is.null(record)) {
          query <- paste(
            "INSERT INTO continuous.corrections",
            "(timeseries_id, start_dt, end_dt, correction_type, value1,",
            " value2, timestep_window, equation)",
            "VALUES ($1, $2, $3, $4, $5, $6,",
            " CASE WHEN $7::integer IS NULL THEN NULL",
            "      ELSE make_interval(secs => $7::integer) END,",
            " $8)"
          )
          params <- list(
            selected_ts(),
            action$start_dt,
            action$end_dt,
            correction$type_id,
            correction$value1,
            correction$value2,
            correction$window,
            correction$equation
          )
        } else {
          query <- paste(
            "UPDATE continuous.corrections",
            "SET correction_type = $1, start_dt = $2, end_dt = $3,",
            "    value1 = $4, value2 = $5,",
            "    timestep_window = CASE WHEN $6::integer IS NULL THEN NULL",
            "      ELSE make_interval(secs => $6::integer) END,",
            "    equation = $7",
            "WHERE correction_id = $8"
          )
          params <- list(
            correction$type_id,
            action$start_dt,
            action$end_dt,
            correction$value1,
            correction$value2,
            correction$window,
            correction$equation,
            record$record_id
          )
        }
      } else {
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
          query <- sprintf(
            "INSERT INTO %s (timeseries_id, %s, start_dt, end_dt) VALUES ($1, $2, $3, $4)",
            table_name,
            type_col
          )
          params <- list(
            selected_ts(),
            as.integer(action$type_id),
            action$start_dt,
            action$end_dt
          )
        } else {
          query <- sprintf(
            "UPDATE %s SET %s = $1, start_dt = $2, end_dt = $3 WHERE %s = $4",
            table_name,
            type_col,
            id_col
          )
          params <- list(
            as.integer(action$type_id),
            action$start_dt,
            action$end_dt,
            record$record_id
          )
        }
      }

      res <- tryCatch(
        DBI::dbExecute(
          session$userData$AquaCache,
          query,
          params = params
        ),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
      if (is.null(res)) {
        return(FALSE)
      }

      showNotification(
        if (is.null(record)) "Record added successfully." else "Record updated successfully.",
        type = "message"
      )
      assignment_refresh(assignment_refresh() + 1)
      selected_record(NULL)
      proxy <- DT::dataTableProxy(ns("active_assignments"), session = session)
      DT::selectRows(proxy, NULL)
      TRUE
    }

    observeEvent(input$apply_attribute, {
      req(selected_ts())
      err <- range_error()
      if (!is.null(err)) {
        showNotification(err, type = "error")
        return()
      }
      start_dt <- scalar_utc_datetime(input$start_dt)
      end_dt <- scalar_utc_datetime(input$end_dt)
      if (is.na(start_dt) || is.na(end_dt)) {
        showNotification("Start or end datetime is invalid.", type = "error")
        return()
      }

      kind <- active_kind()
      record <- selected_record()
      action <- list(
        kind = kind,
        record = record,
        start_dt = start_dt,
        end_dt = end_dt
      )

      if (identical(kind, "correction")) {
        correction <- tryCatch(
          correction_form_values(),
          error = function(e) {
            showNotification(conditionMessage(e), type = "error")
            NULL
          }
        )
        if (is.null(correction)) {
          return()
        }
        action$correction <- correction
      } else {
        type_id <- input$attribute_value
        if (!length(type_id)) {
          showNotification(
            "Select an attribute value before applying.",
            type = "warning"
          )
          return()
        }
        action$type_id <- type_id
      }

      msg <- confirmation_message(action)
      if (!is.null(msg)) {
        pending_action(action)
        showModal(modalDialog(
          title = "Confirm change",
          msg,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_apply_attribute"), "Apply")
          )
        ))
        return()
      }

      commit_action(action)
    })

    observeEvent(input$confirm_apply_attribute, {
      action <- pending_action()
      removeModal()
      pending_action(NULL)
      if (!is.null(action)) {
        commit_action(action)
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
        qualifier = "continuous.qualifiers",
        correction = "continuous.corrections"
      )
      id_col <- switch(
        kind,
        grade = "grade_id",
        approval = "approval_id",
        qualifier = "qualifier_id",
        correction = "correction_id"
      )
      query <- sprintf("DELETE FROM %s WHERE %s = $1", table_name, id_col)
      res <- tryCatch(
        DBI::dbExecute(
          session$userData$AquaCache,
          query,
          params = list(record$record_id)
        ),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error")
          NULL
        }
      )
      if (!is.null(res)) {
        showNotification("Record deleted successfully.", type = "message")
        assignment_refresh(assignment_refresh() + 1)
        selected_record(NULL)
        proxy <- DT::dataTableProxy("active_assignments")
        DT::selectRows(proxy, NULL)
      }
    })

    snap_events <- reactive({
      req(selected_ts())
      data <- assignments()
      out <- data.frame()

      add_interval_events <- function(df, source) {
        if (is.null(df) || !nrow(df)) {
          return(data.frame())
        }
        data.frame(
          Source = source,
          Description = df$description,
          start_dt = df$start_dt,
          end_dt = df$end_dt,
          stringsAsFactors = FALSE
        )
      }

      out <- rbind(
        out,
        add_interval_events(data$grades, "Grade"),
        add_interval_events(data$approvals, "Approval"),
        add_interval_events(data$qualifiers, "Qualifier"),
        add_interval_events(data$corrections, "Correction"),
        add_interval_events(field_visits(), "Field visit")
      )

      readings <- field_readings()
      if (!is.null(readings) && nrow(readings)) {
        out <- rbind(
          out,
          data.frame(
            Source = "Field reading",
            Description = readings$note,
            start_dt = readings$datetime,
            end_dt = as.POSIXct(NA, tz = "UTC"),
            stringsAsFactors = FALSE
          )
        )
      }

      events <- instrument_events()
      if (!is.null(events) && nrow(events)) {
        out <- rbind(
          out,
          data.frame(
            Source = events$event_type,
            Description = paste(events$instrument, events$components),
            start_dt = events$datetime,
            end_dt = as.POSIXct(NA, tz = "UTC"),
            stringsAsFactors = FALSE
          )
        )
      }

      if (!nrow(out)) {
        return(out)
      }
      out$start_dt <- as.POSIXct(out$start_dt, tz = "UTC")
      out$end_dt <- as.POSIXct(out$end_dt, tz = "UTC")
      out <- out[order(out$start_dt, out$Source), , drop = FALSE]
      row.names(out) <- NULL
      out
    })

    output$snap_events <- DT::renderDT({
      df <- snap_events()
      if (is.null(df) || !nrow(df)) {
        return(DT::datatable(
          data.frame(Message = "No snap points available for this selection."),
          options = list(dom = 't'),
          selection = 'none'
        ))
      }
      display <- data.frame(
        Source = df$Source,
        Description = df$Description,
        Start = vapply(df$start_dt, format_datetime, character(1)),
        End = vapply(df$end_dt, format_datetime, character(1)),
        stringsAsFactors = FALSE
      )
      DT::datatable(
        display,
        selection = 'single',
        options = list(
          pageLength = 5,
          lengthChange = FALSE,
          scrollX = TRUE
        )
      )
    })

    selected_snap_event <- reactive({
      idx <- input$snap_events_rows_selected
      df <- snap_events()
      if (!length(idx) || is.null(df) || !nrow(df) || idx > nrow(df)) {
        return(NULL)
      }
      df[idx, , drop = FALSE]
    })

    observeEvent(input$snap_start, {
      event <- selected_snap_event()
      if (is.null(event)) {
        showNotification("Select a snap row first.", type = "warning")
        return()
      }
      set_datetime_input("start_dt", event$start_dt)
      next_edge("end")
    })

    observeEvent(input$snap_end, {
      event <- selected_snap_event()
      if (is.null(event)) {
        showNotification("Select a snap row first.", type = "warning")
        return()
      }
      end_value <- if (!is.na(event$end_dt)) event$end_dt else event$start_dt
      set_datetime_input("end_dt", end_value)
      next_edge("start")
    })

    observeEvent(input$snap_range, {
      event <- selected_snap_event()
      if (is.null(event)) {
        showNotification("Select a snap row first.", type = "warning")
        return()
      }
      if (is.na(event$end_dt) || event$start_dt >= event$end_dt) {
        showNotification(
          "The selected snap row is a point event. Use Set start or Set end.",
          type = "warning"
        )
        return()
      }
      set_range_inputs(event$start_dt, event$end_dt)
    })

    handle_plot_selection <- function(selection) {
      if (is.null(selection) || !length(selection) || is.null(selection$x)) {
        return()
      }
      times <- vapply(selection$x, to_posix_from_event, as.POSIXct(NA))
      times <- times[!is.na(times)]
      if (!length(times)) {
        return()
      }
      times <- sort(times)
      set_range_inputs(times[1], times[length(times)])
    }

    observeEvent(
      plotly::event_data("plotly_brushed", source = ns("ts_plot")),
      {
        req(selected_ts())
        handle_plot_selection(
          plotly::event_data("plotly_brushed", source = ns("ts_plot"))
        )
      },
      ignoreNULL = TRUE
    )

    observeEvent(
      plotly::event_data("plotly_selected", source = ns("ts_plot")),
      {
        req(selected_ts())
        handle_plot_selection(
          plotly::event_data("plotly_selected", source = ns("ts_plot"))
        )
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
          set_datetime_input("start_dt", dt)
          next_edge("end")
        } else {
          set_datetime_input("end_dt", dt)
          next_edge("start")
        }
      },
      ignoreNULL = TRUE
    )

    add_interval_shapes <- function(shapes, df, opacity = 0.12) {
      if (is.null(df) || !nrow(df)) {
        return(shapes)
      }
      for (i in seq_len(nrow(df))) {
        row <- df[i, ]
        if (is.na(row$start_dt) || is.na(row$end_dt)) {
          next
        }
        fill_col <- tryCatch(
          grDevices::adjustcolor(row$color_code, alpha.f = opacity),
          error = function(e) grDevices::adjustcolor("#cccccc", alpha.f = opacity)
        )
        shapes[[length(shapes) + 1]] <- list(
          type = "rect",
          x0 = row$start_dt,
          x1 = row$end_dt,
          y0 = 0,
          y1 = 1,
          xref = "x",
          yref = "paper",
          fillcolor = fill_col,
          line = list(width = 0),
          layer = "below"
        )
      }
      shapes
    }

    output$ts_plot <- plotly::renderPlotly({
      req(selected_ts())
      err <- range_error()
      validate(need(is.null(err), err))
      rng <- selected_range()
      view_rng <- plot_range()
      df <- ts_data()
      plot_source <- ns("ts_plot")

      if (!nrow(df)) {
        return(
          plotly::plotly_empty(
            type = "scatter",
            mode = "lines",
            source = plot_source
          ) |>
            plotly::layout(
              title = NULL,
              xaxis = list(title = "Datetime"),
              yaxis = list(title = "Value"),
              dragmode = "select"
            )
        )
      }

      readings <- field_readings()
      events <- instrument_events()
      visits <- field_visits()
      y_vals <- c(df$value_raw, df$value_corrected, readings$value)
      y_range <- range(y_vals, na.rm = TRUE)
      if (!all(is.finite(y_range))) {
        y_range <- c(0, 1)
      }
      if (diff(y_range) == 0) {
        y_range <- y_range + c(-0.5, 0.5)
      }
      event_y <- y_range[1] + 0.04 * diff(y_range)

      shapes <- list(list(
        type = "rect",
        x0 = rng$start,
        x1 = rng$end,
        y0 = 0,
        y1 = 1,
        xref = "x",
        yref = "paper",
        fillcolor = grDevices::adjustcolor("#FBE5B2", alpha.f = 0.35),
        line = list(color = "#A66F00", width = 1, dash = "dot"),
        layer = "below"
      ))

      if (isTRUE(input$show_attribute_bands)) {
        assignments_list <- assignments()
        shapes <- add_interval_shapes(shapes, assignments_list$grades, 0.09)
        shapes <- add_interval_shapes(shapes, assignments_list$approvals, 0.12)
        shapes <- add_interval_shapes(shapes, assignments_list$qualifiers, 0.15)
        shapes <- add_interval_shapes(shapes, assignments_list$corrections, 0.08)
      }

      if (isTRUE(input$show_field_readings) && nrow(visits)) {
        visit_shapes <- visits
        visit_shapes$color_code <- "#6C757D"
        shapes <- add_interval_shapes(shapes, visit_shapes, 0.08)
      }

      if (isTRUE(input$show_instrument_events) && nrow(events)) {
        for (i in seq_len(nrow(events))) {
          shapes[[length(shapes) + 1]] <- list(
            type = "line",
            x0 = events$datetime[i],
            x1 = events$datetime[i],
            y0 = 0,
            y1 = 1,
            xref = "x",
            yref = "paper",
            line = list(color = "#7A9A01", width = 1, dash = "dash"),
            layer = "below"
          )
        }
      }

      p <- plotly::plot_ly(source = plot_source)
      if ("value_raw" %in% names(df) && any(!is.na(df$value_raw))) {
        p <- plotly::add_lines(
          p,
          data = df,
          x = ~datetime,
          y = ~value_raw,
          name = "Raw",
          line = list(color = "#6C757D"),
          hoverinfo = "text",
          text = ~paste0("Raw: ", round(value_raw, 4), " (", datetime, ")")
        )
      }
      p <- plotly::add_lines(
        p,
        data = df,
        x = ~datetime,
        y = ~value_corrected,
        name = "Corrected",
        line = list(color = "#0072B2"),
        hoverinfo = "text",
        text = ~paste0(
          "Corrected: ",
          round(value_corrected, 4),
          " (",
          datetime,
          ")"
        )
      )

      if (isTRUE(input$show_field_readings) && nrow(readings)) {
        p <- plotly::add_markers(
          p,
          data = readings,
          x = ~datetime,
          y = ~value,
          name = "Field readings",
          marker = list(
            symbol = "diamond",
            size = 9,
            color = "#D55E00",
            line = list(width = 1, color = "#FFFFFF")
          ),
          hoverinfo = "text",
          text = ~paste0("Field reading: ", value, " (", datetime, ")")
        )
      }

      if (isTRUE(input$show_instrument_events) && nrow(events)) {
        events$plot_y <- event_y
        p <- plotly::add_markers(
          p,
          data = events,
          x = ~datetime,
          y = ~plot_y,
          name = "Instrument checks / calibrations",
          marker = list(
            symbol = "triangle-up",
            size = 10,
            color = "#7A9A01",
            line = list(width = 1, color = "#FFFFFF")
          ),
          hoverinfo = "text",
          text = ~paste0(
            event_type,
            ": ",
            instrument,
            " ",
            components,
            " (",
            datetime,
            ")"
          )
        )
      }

      plotly::layout(
        p,
        title = NULL,
        shapes = shapes,
        xaxis = list(
          title = "Datetime",
          range = c(view_rng$start, view_rng$end)
        ),
        yaxis = list(title = "Value"),
        dragmode = "select",
        legend = list(orientation = "h", yanchor = "bottom", y = 1.02)
      )
    })
  })
}
