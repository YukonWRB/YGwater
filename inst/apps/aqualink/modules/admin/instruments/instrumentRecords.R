# Read-only instrument calibration, maintenance, and sensor record viewer.

instrumentRecordsUI <- function(id) {
  ns <- NS(id)
  detail_tables <- c(
    temperature = "Temperature checks",
    specific_conductance = "Conductivity checks / calibrations",
    ph = "pH checks / calibrations",
    orp = "ORP checks / calibrations",
    turbidity = "Turbidity checks / calibrations",
    dissolved_oxygen = "Dissolved oxygen checks / calibrations",
    depth = "Depth checks"
  )

  bslib::page_sidebar(
    YGwater:::shiny_ui_ready(ns("ui_ready")),
    title = NULL,
    sidebar = bslib::sidebar(
      width = 360,
      title = "Instrument records",
      p("Review calibration, maintenance, and sensor association records."),
      selectizeInput(
        ns("instrument_id"),
        "Instrument",
        choices = NULL,
        multiple = FALSE,
        options = list(
          placeholder = "Search by serial, make, model, owner, asset tag, or type",
          allowEmptyOption = TRUE
        )
      ),
      div(
        class = "d-flex gap-2 flex-wrap",
        actionButton(ns("reload"), "Reload records"),
        uiOutput(ns("download_button_ui"), inline = TRUE)
      ),
      tags$hr(),
      p(class = "text-muted", "This page does not edit equipment records.")
    ),
    uiOutput(ns("instrument_summary")),
    bslib::navset_tab(
      id = ns("records_view"),
      bslib::nav_panel(
        "Calibrations",
        br(),
        tags$h5("Calibration records"),
        DT::DTOutput(ns("calibrations_table")),
        br(),
        do.call(
          bslib::accordion,
          c(
            list(id = ns("calibration_details"), open = FALSE),
            lapply(names(detail_tables), function(table) {
              bslib::accordion_panel(
                detail_tables[[table]],
                DT::DTOutput(ns(paste0("calibration_", table)))
              )
            })
          )
        )
      ),
      bslib::nav_panel(
        "Maintenance",
        br(),
        tags$h5("Maintenance history"),
        DT::DTOutput(ns("maintenance_table")),
        br(),
        tags$h5("Maintenance due items"),
        DT::DTOutput(ns("due_table"))
      ),
      bslib::nav_panel(
        "Sensor associations",
        br(),
        tags$h5("Current sensor associations"),
        DT::DTOutput(ns("current_sensors_table")),
        br(),
        tags$h5("Sensor association history"),
        DT::DTOutput(ns("sensor_history_table"))
      )
    )
  )
}

instrumentRecords <- function(id, language = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    calibration_detail_tables <- c(
      temperature = "calibrate_temperature",
      specific_conductance = "calibrate_specific_conductance",
      ph = "calibrate_ph",
      orp = "calibrate_orp",
      turbidity = "calibrate_turbidity",
      dissolved_oxygen = "calibrate_dissolved_oxygen",
      depth = "calibrate_depth"
    )
    detail_labels <- c(
      temperature = "Temperature checks",
      specific_conductance = "Conductivity checks / calibrations",
      ph = "pH checks / calibrations",
      orp = "ORP checks / calibrations",
      turbidity = "Turbidity checks / calibrations",
      dissolved_oxygen = "Dissolved oxygen checks / calibrations",
      depth = "Depth checks"
    )
    records <- reactiveValues(
      instruments = data.frame(),
      instrument = data.frame(),
      calibrations = data.frame(),
      calibration_details = stats::setNames(
        lapply(calibration_detail_tables, function(x) data.frame()),
        names(calibration_detail_tables)
      ),
      maintenance = data.frame(),
      due = data.frame(),
      current_sensors = data.frame(),
      sensor_history = data.frame()
    )

    con <- function() session$userData$AquaCache

    render_empty_table <- function(message) {
      DT::datatable(
        data.frame(message, check.names = FALSE),
        rownames = FALSE,
        options = list(dom = "t")
      )
    }

    read_instruments <- function() {
      DBI::dbGetQuery(
        con(),
        "SELECT i.instrument_id,
                i.serial_no,
                i.asset_tag,
                mk.make,
                mdl.model,
                tp.type,
                org.name AS owner,
                i.date_retired
         FROM instruments.instruments AS i
         LEFT JOIN instruments.instrument_makes AS mk
           ON i.instrument_make_id = mk.make_id
         LEFT JOIN instruments.instrument_models AS mdl
           ON i.instrument_model_id = mdl.model_id
         LEFT JOIN instruments.instrument_types AS tp
           ON i.instrument_type_id = tp.type_id
         LEFT JOIN public.organizations AS org
           ON i.owner = org.organization_id
         ORDER BY i.date_retired NULLS FIRST, mk.make, mdl.model, i.serial_no, i.instrument_id"
      )
    }

    instrument_choices <- function(instruments) {
      if (is.null(instruments) || nrow(instruments) == 0L) {
        return(stats::setNames(character(), character()))
      }
      piece <- function(x) {
        is_blank <- is.na(x) | !nzchar(trimws(as.character(x)))
        x[is_blank] <- ""
        trimws(as.character(x))
      }
      labels <- vapply(seq_len(nrow(instruments)), function(i) {
        make_model <- paste(
          piece(instruments$make[i]),
          piece(instruments$model[i])
        )
        parts <- c(
          piece(instruments$serial_no[i]),
          trimws(make_model),
          piece(instruments$type[i]),
          if (!is.na(instruments$owner[i]) && nzchar(instruments$owner[i])) {
            instruments$owner[i]
          },
          if (!is.na(instruments$asset_tag[i]) && nzchar(instruments$asset_tag[i])) {
            paste0("Asset ", instruments$asset_tag[i])
          },
          if (!is.na(instruments$date_retired[i])) "(retired)"
        )
        parts <- parts[!is.na(parts) & nzchar(parts)]
        paste(parts, collapse = " | ")
      }, character(1))
      stats::setNames(as.character(instruments$instrument_id), labels)
    }

    clear_records <- function() {
      records$instrument <- data.frame()
      records$calibrations <- data.frame()
      records$calibration_details <- stats::setNames(
        lapply(calibration_detail_tables, function(x) data.frame()),
        names(calibration_detail_tables)
      )
      records$maintenance <- data.frame()
      records$due <- data.frame()
      records$current_sensors <- data.frame()
      records$sensor_history <- data.frame()
      invisible(NULL)
    }

    read_records <- function(instrument_id) {
      instrument_id <- as.integer(instrument_id)
      instrument_row <- match(instrument_id, records$instruments$instrument_id)
      if (is.na(instrument_row)) {
        stop("The selected instrument is no longer in the catalogue.")
      }
      selected_instrument <- records$instruments[
        instrument_row,
        ,
        drop = FALSE
      ]

      calibrations <- DBI::dbGetQuery(
        con(),
        "SELECT c.calibration_id,
                c.obs_datetime,
                c.observer_id,
                CONCAT(o.observer_first, ' ', o.observer_last, ' (', o.organization, ')') AS observer,
                c.purpose,
                c.complete,
                c.sensor_holder_instrument_id,
                holder.serial_no AS sensor_holder_serial,
                c.handheld_meter_instrument_id,
                handheld.serial_no AS handheld_meter_serial
         FROM instruments.calibrations AS c
         LEFT JOIN instruments.observers AS o ON c.observer_id = o.observer_id
         LEFT JOIN instruments.instruments AS holder
           ON c.sensor_holder_instrument_id = holder.instrument_id
         LEFT JOIN instruments.instruments AS handheld
           ON c.handheld_meter_instrument_id = handheld.instrument_id
         WHERE c.sensor_holder_instrument_id = $1
            OR c.handheld_meter_instrument_id = $1
         ORDER BY c.obs_datetime DESC, c.calibration_id DESC",
        params = list(instrument_id)
      )

      details <- lapply(calibration_detail_tables, function(table_name) {
        quoted_table <- DBI::dbQuoteIdentifier(
          con(),
          DBI::Id(schema = "instruments", table = table_name)
        )
        DBI::dbGetQuery(
          con(),
          paste0(
            "SELECT detail.*, sensor.sensor_serial, sensor_type.sensor_type AS probe_type ",
            "FROM ", as.character(quoted_table), " AS detail ",
            "LEFT JOIN instruments.sensors AS sensor ON detail.sensor_id = sensor.sensor_id ",
            "LEFT JOIN instruments.sensor_types AS sensor_type ",
            "ON sensor.sensor_type = sensor_type.sensor_type_id ",
            "WHERE detail.calibration_id IN (",
            "SELECT calibration_id FROM instruments.calibrations ",
            "WHERE sensor_holder_instrument_id = $1 ",
            "OR handheld_meter_instrument_id = $1) ",
            "ORDER BY detail.calibration_id"
          ),
          params = list(instrument_id)
        )
      })
      names(details) <- names(calibration_detail_tables)

      maintenance <- DBI::dbGetQuery(
        con(),
        "SELECT im.event_id,
                im.instrument_id,
                im.obs_datetime,
                CONCAT(o.observer_first, ' ', o.observer_last, ' (', o.organization, ')') AS observer,
                im.note,
                im.field_visit_id,
                im.firmware_version,
                im.logger_program_revision,
                completed_due.completed_due
         FROM instruments.instrument_maintenance AS im
         LEFT JOIN instruments.observers AS o ON im.observer = o.observer_id
         LEFT JOIN LATERAL (
           SELECT string_agg(
             to_char(due.date_maintenance_due, 'YYYY-MM-DD') || ' | ' ||
               due.maintenance_due_note,
             '; ' ORDER BY due.date_maintenance_due, due.maintenance_due_id
           ) AS completed_due
           FROM instruments.instrument_maintenance_due AS due
           WHERE due.completed_event_id = im.event_id
         ) AS completed_due ON TRUE
         WHERE im.instrument_id = $1
         ORDER BY im.obs_datetime DESC, im.event_id DESC",
        params = list(instrument_id)
      )

      due <- DBI::dbGetQuery(
        con(),
        "SELECT due.maintenance_due_id,
                due.instrument_id,
                due.obs_datetime AS scheduled_datetime,
                due.date_maintenance_due,
                due.maintenance_due_note,
                due.completed_event_id,
                completed.obs_datetime AS completed_datetime,
                completed.note AS completion_note,
                CONCAT(o.observer_first, ' ', o.observer_last, ' (', o.organization, ')') AS scheduled_by,
                CASE
                  WHEN due.completed_event_id IS NOT NULL THEN 'Completed'
                  WHEN due.date_maintenance_due < CURRENT_DATE THEN 'Overdue'
                  WHEN due.date_maintenance_due = CURRENT_DATE THEN 'Due today'
                  ELSE 'Upcoming'
                END AS due_status
         FROM instruments.instrument_maintenance_due AS due
         LEFT JOIN instruments.observers AS o ON due.observer = o.observer_id
         LEFT JOIN instruments.instrument_maintenance AS completed
           ON due.completed_event_id = completed.event_id
         WHERE due.instrument_id = $1
         ORDER BY due.completed_event_id IS NOT NULL,
                  due.date_maintenance_due, due.maintenance_due_id",
        params = list(instrument_id)
      )

      current_sensors <- DBI::dbGetQuery(
        con(),
        "SELECT cur.slot_number,
                cur.slot_state,
                COALESCE(sensor.sensor_serial, cur.sensor_serial) AS sensor_serial,
                sensor_type.sensor_type,
                cur.note,
                CONCAT(o.observer_first, ' ', o.observer_last, ' (', o.organization, ')') AS recorded_by,
                cur.obs_datetime
         FROM instruments.instrument_sensor_current AS cur
         LEFT JOIN instruments.sensors AS sensor ON cur.sensor_id = sensor.sensor_id
         LEFT JOIN instruments.sensor_types AS sensor_type
           ON sensor.sensor_type = sensor_type.sensor_type_id
         LEFT JOIN instruments.observers AS o ON cur.observer = o.observer_id
         WHERE cur.instrument_id = $1
         ORDER BY cur.slot_number",
        params = list(instrument_id)
      )

      sensor_history <- DBI::dbGetQuery(
        con(),
        "SELECT e.event_id,
                e.obs_datetime,
                CONCAT(o.observer_first, ' ', o.observer_last, ' (', o.organization, ')') AS recorded_by,
                e.event_note,
                slots.slot_number,
                slots.slot_state,
                slots.note AS slot_note,
                slots.sensor_id,
                sensor.sensor_serial,
                sensor_type.sensor_type
         FROM instruments.instrument_sensor_events AS e
         LEFT JOIN instruments.observers AS o ON e.observer = o.observer_id
         LEFT JOIN instruments.instrument_sensor_event_slots AS slots
           ON e.event_id = slots.event_id
         LEFT JOIN instruments.sensors AS sensor ON slots.sensor_id = sensor.sensor_id
         LEFT JOIN instruments.sensor_types AS sensor_type
           ON sensor.sensor_type = sensor_type.sensor_type_id
         WHERE e.instrument_id = $1
         ORDER BY e.obs_datetime DESC, e.event_id DESC, slots.slot_number",
        params = list(instrument_id)
      )

      list(
        instrument = selected_instrument,
        calibrations = calibrations,
        calibration_details = details,
        maintenance = maintenance,
        due = due,
        current_sensors = current_sensors,
        sensor_history = sensor_history
      )
    }

    output$instrument_summary <- renderUI({
      if (nrow(records$instrument) == 0L) {
        return(tags$div(
          class = "alert alert-info",
          "Choose an instrument to view its equipment records."
        ))
      }
      instrument <- records$instrument[1L, , drop = FALSE]
      status <- if (is.na(instrument$date_retired)) {
        "Active"
      } else {
        paste("Retired", instrument$date_retired)
      }
      bslib::card(
        class = "mb-3",
        bslib::card_body(
          tags$strong(paste(instrument$make, instrument$model, instrument$type)),
          tags$span(paste0(" · Serial ", instrument$serial_no)),
          if (!is.na(instrument$asset_tag) && nzchar(instrument$asset_tag)) {
            tags$span(paste0(" · Asset ", instrument$asset_tag))
          },
          tags$span(paste0(" · ", status)),
          if (!is.na(instrument$owner) && nzchar(instrument$owner)) {
            tags$span(paste0(" · Owner: ", instrument$owner))
          }
        )
      )
    })

    output$download_button_ui <- renderUI({
      if (nrow(records$instrument) != 1L) {
        return(tags$button(
          type = "button",
          class = "btn btn-primary",
          disabled = "disabled",
          "Export .xlsx"
        ))
      }
      downloadButton(
        ns("download_workbook"),
        "Export .xlsx",
        class = "btn-primary"
      )
    })

    render_records <- function(data, empty_message, page_length = 10L) {
      if (is.null(data) || nrow(data) == 0L) {
        return(render_empty_table(empty_message))
      }
      DT::datatable(
        data,
        filter = "top",
        selection = "none",
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          autoWidth = TRUE,
          pageLength = page_length
        )
      )
    }

    output$calibrations_table <- DT::renderDT({
      df <- records$calibrations
      if (is.null(df) || nrow(df) == 0L) {
        return(render_empty_table("No calibration records found for this instrument."))
      }
      display <- df[, c(
        "calibration_id", "obs_datetime", "observer", "purpose", "complete",
        "sensor_holder_serial", "handheld_meter_serial"
      ), drop = FALSE]
      names(display) <- c(
        "Calibration ID", "Date/time UTC", "Recorded by", "Purpose", "Complete",
        "Sensor holder", "Handheld meter"
      )
      render_records(display)
    })

    for (detail_name in names(calibration_detail_tables)) {
      local({
        detail_name <- detail_name
        output[[paste0("calibration_", detail_name)]] <- DT::renderDT({
          detail <- records$calibration_details[[detail_name]]
          render_records(
            detail,
            paste("No", detail_labels[[detail_name]], "recorded.")
          )
        })
      })
    }

    output$maintenance_table <- DT::renderDT({
      df <- records$maintenance
      if (is.null(df) || nrow(df) == 0L) {
        return(render_empty_table("No maintenance history found for this instrument."))
      }
      display <- df[, c(
        "obs_datetime", "observer", "note", "field_visit_id", "firmware_version",
        "logger_program_revision", "completed_due"
      ), drop = FALSE]
      names(display) <- c(
        "Date/time UTC", "Recorded by", "Maintenance performed", "Field visit ID",
        "Firmware installed", "Logger program installed", "Completed due items"
      )
      render_records(display)
    })

    output$due_table <- DT::renderDT({
      df <- records$due
      if (is.null(df) || nrow(df) == 0L) {
        return(render_empty_table("No maintenance due items found for this instrument."))
      }
      display <- df[, c(
        "date_maintenance_due", "due_status", "maintenance_due_note", "scheduled_by",
        "scheduled_datetime", "completed_datetime", "completion_note"
      ), drop = FALSE]
      names(display) <- c(
        "Due date", "Status", "Maintenance due", "Scheduled by", "Scheduled at UTC",
        "Completed at UTC", "Completion note"
      )
      render_records(display)
    })

    output$current_sensors_table <- DT::renderDT({
      df <- records$current_sensors
      if (is.null(df) || nrow(df) == 0L) {
        return(render_empty_table("No current sensor associations found."))
      }
      display <- df[, c(
        "slot_number", "slot_state", "sensor_serial", "sensor_type", "note",
        "recorded_by", "obs_datetime"
      ), drop = FALSE]
      names(display) <- c(
        "Slot", "State", "Sensor serial", "Sensor type", "Slot note",
        "Recorded by", "Date/time UTC"
      )
      render_records(display)
    })

    output$sensor_history_table <- DT::renderDT({
      df <- records$sensor_history
      if (is.null(df) || nrow(df) == 0L) {
        return(render_empty_table("No sensor association history found."))
      }
      display <- df[, c(
        "obs_datetime", "recorded_by", "slot_number", "slot_state", "sensor_serial",
        "sensor_type", "slot_note", "event_note"
      ), drop = FALSE]
      names(display) <- c(
        "Date/time UTC", "Recorded by", "Slot", "State", "Sensor serial",
        "Sensor type", "Slot note", "Event note"
      )
      render_records(display)
    })

    refresh_instrument_choices <- function(selected = "") {
      records$instruments <- read_instruments()
      shiny::updateSelectizeInput(
        session,
        "instrument_id",
        choices = instrument_choices(records$instruments),
        selected = selected,
        server = TRUE
      )
      invisible(NULL)
    }

    refresh_selected_records <- function() {
      instrument_id <- suppressWarnings(as.integer(input$instrument_id))
      if (!length(instrument_id) || is.na(instrument_id)) {
        clear_records()
        return(invisible(NULL))
      }
      tryCatch(
        {
          loaded <- read_records(instrument_id)
          records$instrument <- loaded$instrument
          records$calibrations <- loaded$calibrations
          records$calibration_details <- loaded$calibration_details
          records$maintenance <- loaded$maintenance
          records$due <- loaded$due
          records$current_sensors <- loaded$current_sensors
          records$sensor_history <- loaded$sensor_history
        },
        error = function(e) {
          clear_records()
          shiny::showNotification(
            paste("Unable to load instrument records:", conditionMessage(e)),
            type = "error",
            duration = NULL
          )
        }
      )
      invisible(NULL)
    }

    observeEvent(input$ui_ready, {
      tryCatch(
        refresh_instrument_choices(),
        error = function(e) {
          shiny::showNotification(
            paste("Unable to load the instrument catalogue:", conditionMessage(e)),
            type = "error",
            duration = NULL
          )
        }
      )
    }, once = TRUE, ignoreInit = TRUE)

    observeEvent(input$instrument_id, {
      refresh_selected_records()
    }, ignoreInit = TRUE)

    observeEvent(input$reload, {
      selected <- if (is.null(input$instrument_id)) "" else input$instrument_id
      tryCatch(
        refresh_instrument_choices(selected = selected),
        error = function(e) {
          shiny::showNotification(
            paste("Unable to reload the instrument catalogue:", conditionMessage(e)),
            type = "error",
            duration = NULL
          )
        }
      )
      refresh_selected_records()
    }, ignoreInit = TRUE)

    output$download_workbook <- downloadHandler(
      filename = function() {
        req(nrow(records$instrument) == 1L)
        serial <- records$instrument$serial_no[[1L]]
        if (is.na(serial) || !nzchar(serial)) {
          serial <- paste0("instrument_", records$instrument$instrument_id[[1L]])
        }
        serial <- gsub("[^A-Za-z0-9_-]+", "_", serial)
        paste0(
          "instrument_records_",
          serial,
          "_",
          format(Sys.Date(), "%Y%m%d"),
          ".xlsx"
        )
      },
      content = function(file) {
        req(nrow(records$instrument) == 1L)
        sheets <- c(
          list(
            instrument = records$instrument,
            calibrations = records$calibrations
          ),
          stats::setNames(
            unname(records$calibration_details),
            unname(calibration_detail_tables)
          ),
          list(
            maintenance_history = records$maintenance,
            maintenance_due = records$due,
            sensor_current = records$current_sensors,
            sensor_history = records$sensor_history
          )
        )
        names(sheets) <- make.unique(substr(names(sheets), 1L, 31L))
        workbook <- openxlsx::createWorkbook()
        header_style <- openxlsx::createStyle(
          fgFill = "#244C5A",
          fontColour = "#FFFFFF",
          textDecoration = "bold",
          border = "bottom"
        )
        for (sheet_name in names(sheets)) {
          data <- sheets[[sheet_name]]
          openxlsx::addWorksheet(workbook, sheet_name)
          openxlsx::writeData(
            workbook,
            sheet_name,
            data,
            withFilter = nrow(data) > 0L
          )
          if (ncol(data) > 0L) {
            openxlsx::addStyle(
              workbook,
              sheet_name,
              header_style,
              rows = 1L,
              cols = seq_len(ncol(data)),
              gridExpand = TRUE
            )
            openxlsx::freezePane(workbook, sheet_name, firstActiveRow = 2L)
            openxlsx::setColWidths(
              workbook,
              sheet_name,
              cols = seq_len(ncol(data)),
              widths = "auto"
            )
          }
        }
        openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
      }
    )
  })
}
