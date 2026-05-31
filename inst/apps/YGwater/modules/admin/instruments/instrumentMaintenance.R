# UI and server code for recording instrument maintenance

instrumentMaintenanceUI <- function(id) {
  ns <- NS(id)

  bslib::page_sidebar(
    title = NULL,
    sidebar = bslib::sidebar(
      width = 440,
      title = "Instrument maintenance",
      actionButton(
        ns("show_all_upcoming"),
        "Show upcoming maintenance for all",
        class = "btn-warning w-100 mb-3"
      ),
      selectizeInput(
        ns("instrument_id"),
        "Instrument",
        choices = NULL,
        multiple = TRUE,
        options = list(
          maxItems = 1,
          placeholder = "Search by serial, make, model, type, or owner"
        )
      ),
      div(
        class = "d-flex gap-2 flex-wrap mb-3",
        actionButton(ns("choose_instrument_table"), "Find in table"),
        actionButton(ns("clear"), "Clear"),
        actionButton(ns("reload"), "Reload")
      ),
      tags$hr(),
      tags$h5("Record metadata"),
      selectizeInput(
        ns("observer"),
        "Observer (type to add new)",
        choices = NULL,
        multiple = TRUE,
        options = list(
          maxItems = 1,
          placeholder = "Required",
          create = TRUE
        )
      ),
      bslib::layout_columns(
        col_widths = c(6, 6),
        selectizeInput(
          ns("timezone"),
          "Display time zone",
          choices = input_timezone_choices(),
          selected = default_input_timezone(),
          multiple = TRUE,
          options = list(maxItems = 1)
        ),
        shinyWidgets::airDatepickerInput(
          ns("obs_datetime"),
          "Record date/time",
          value = .POSIXct(Sys.time(), tz = "UTC"),
          range = FALSE,
          multiple = FALSE,
          timepicker = TRUE,
          update_on = "change",
          tz = air_datetime_widget_timezone(default_input_timezone()),
          timepickerOpts = shinyWidgets::timepickerOptions(
            minutesStep = 15,
            timeFormat = "HH:mm"
          )
        )
      ),
      tags$hr(),
      conditionalPanel(
        ns = ns,
        condition = "input.view == 'Maintenance history'",
        tags$h5("Maintenance event"),
        textAreaInput(
          ns("maintenance_note"),
          "Maintenance performed",
          value = "",
          width = "100%",
          height = "120px"
        ),
        selectizeInput(
          ns("completed_due_ids"),
          "Due items completed",
          choices = NULL,
          multiple = TRUE,
          options = list(placeholder = "Optional")
        ),
        div(
          class = "d-flex gap-2 flex-wrap mb-3",
          actionButton(
            ns("save_maintenance"),
            "Save event",
            class = "btn-primary"
          ),
          actionButton(ns("clear_maintenance_selection"), "New event")
        )
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.view == 'Next due'",
        tags$h5("Scheduled maintenance due"),
        dateInput(ns("date_maintenance_due"), "Due date", value = NULL),
        textAreaInput(
          ns("maintenance_due_note"),
          "Maintenance due note",
          value = "",
          width = "100%",
          height = "100px"
        ),
        div(
          class = "d-flex gap-2 flex-wrap mb-3",
          actionButton(
            ns("save_due"),
            "Schedule due item",
            class = "btn-primary"
          ),
          actionButton(ns("new_due"), "New due item"),
          actionButton(ns("delete_due"), "Delete due item")
        )
      ),
      conditionalPanel(
        ns = ns,
        condition = "input.view == 'Sensors'",
        tags$h5("Sensor slot event"),
        numericInput(
          ns("slot_number"),
          "Slot number",
          value = 1,
          min = 1,
          step = 1
        ),
        br(),
        actionButton(ns("register_sensor"), "Register new sensor"),
        selectizeInput(
          ns("sensor_id"),
          "Sensor",
          choices = NULL,
          multiple = TRUE,
          options = list(maxItems = 1, placeholder = "Optional")
        ),
        textAreaInput(
          ns("slot_note"),
          "Slot note",
          value = "",
          width = "100%",
          height = "90px"
        ),
        textAreaInput(
          ns("sensor_event_note"),
          "Event note",
          value = "",
          width = "100%",
          height = "80px"
        ),
        div(
          class = "d-flex gap-2 flex-wrap mb-3",
          actionButton(
            ns("save_slot_event"),
            "Record slot event",
            class = "btn-primary"
          )
        )
      )
    ),
    uiOutput(ns("banner")),
    uiOutput(ns("instrument_summary")),
    bslib::navset_tab(
      id = ns("view"),
      bslib::nav_panel(
        "Maintenance history",
        br(),
        tags$h5(
          "Add new maintenance events in the sidebar. Click on existing events in the table to edit."
        ),
        DT::DTOutput(ns("maintenance_table"))
      ),
      bslib::nav_panel(
        "Next due",
        uiOutput(ns("due_summary")),
        DT::DTOutput(ns("due_table"))
      ),
      bslib::nav_panel(
        "Sensors",
        tags$h5("Current sensors"),
        DT::DTOutput(ns("current_slots_table")),
        bslib::accordion(
          id = ns("sensor_history_accordion"),
          open = FALSE,
          bslib::accordion_panel(
            "Sensor event history",
            DT::DTOutput(ns("sensor_history_table"))
          )
        )
      )
    )
  )
}

instrumentMaintenance <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- session$userData$AquaCache

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = con,
        module_id = "instrumentMaintenance"
      )
    })

    maintenance_data <- reactiveValues(
      instruments = NULL,
      observers = NULL,
      sensors = NULL,
      sensor_types = NULL,
      sensor_makes = NULL,
      sensor_models = NULL,
      owners = NULL,
      suppliers = NULL,
      maintenance = data.table::data.table(),
      due = data.table::data.table(),
      upcoming_due = data.table::data.table(),
      current_slots = data.table::data.table(),
      sensor_history = data.table::data.table(),
      pending_sensor_move = NULL,
      pending_sensor_unassign = NULL,
      selected_event_id = NA_integer_,
      selected_due_id = NA_integer_
    )

    `%||%` <- function(x, y) {
      if (is.null(x) || !length(x) || all(is.na(x))) y else x
    }

    blank_to_na <- function(value) {
      if (is.null(value) || !length(value) || all(is.na(value))) {
        return(NA_character_)
      }
      value <- trimws(as.character(value[[1]]))
      if (!nzchar(value)) {
        return(NA_character_)
      }
      value
    }

    int_or_na <- function(value) {
      value <- blank_to_na(value)
      if (is.na(value)) {
        return(NA_integer_)
      }
      suppressWarnings(as.integer(value))
    }

    date_or_na <- function(value) {
      if (is.null(value) || !length(value) || all(is.na(value))) {
        return(as.Date(NA))
      }
      as.Date(value[[1]])
    }

    safe_text <- function(value, fallback = "") {
      value <- blank_to_na(value)
      if (is.na(value)) {
        return(fallback)
      }
      value
    }

    label_piece <- function(x, fallback = "Not specified") {
      out <- as.character(x)
      out[is.na(out) | !nzchar(trimws(out))] <- fallback
      out
    }

    normalize_select_value <- function(value) {
      value <- blank_to_na(value)
      if (is.na(value)) {
        return(character(0))
      }
      as.character(value)
    }

    selected_instrument_id <- function() {
      value <- input$instrument_id
      if (is.null(value) || !length(value) || all(is.na(value))) {
        return(NA_integer_)
      }
      value <- trimws(as.character(value))
      value <- value[nzchar(value)]
      if (!length(value)) {
        return(NA_integer_)
      }
      suppressWarnings(as.integer(value[[length(value)]]))
    }

    selected_observer_id <- function() {
      value <- int_or_na(input$observer)
      if (is.na(value)) {
        return(NA_integer_)
      }
      value
    }

    selected_due_ids <- function(value = input$completed_due_ids) {
      if (is.null(value) || !length(value) || all(is.na(value))) {
        return(integer())
      }
      value <- suppressWarnings(as.integer(as.character(value)))
      value[!is.na(value)]
    }

    lookup_id_from_text <- function(value, df, id_col, text_col) {
      value <- blank_to_na(value)
      if (is.na(value) || is.null(df) || nrow(df) == 0) {
        return(NA_integer_)
      }
      target <- tolower(trimws(value))
      values <- tolower(trimws(as.character(df[[text_col]])))
      match_row <- match(target, values)
      if (is.na(match_row)) {
        return(NA_integer_)
      }
      as.integer(df[[id_col]][[match_row]])
    }

    is_new_lookup_value <- function(value, df, id_col) {
      value <- normalize_select_value(value)
      if (!length(value)) {
        return(FALSE)
      }
      if (is.null(df) || nrow(df) == 0) {
        return(TRUE)
      }
      id_value <- suppressWarnings(as.integer(value))
      is.na(id_value) || !id_value %in% as.integer(df[[id_col]])
    }

    with_db_feedback <- function(expr, success = NULL) {
      tryCatch(
        {
          result <- force(expr)
          if (!is.null(success)) {
            showNotification(success, type = "message")
          }
          result
        },
        error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = NULL)
          NULL
        }
      )
    }

    build_instrument_choices <- function(df) {
      if (is.null(df) || nrow(df) == 0) {
        return(character())
      }
      dt <- unique(data.table::as.data.table(df), by = "instrument_id")
      make_model <- trimws(paste(
        label_piece(dt$make_name, ""),
        label_piece(dt$model_name, "")
      ))
      make_model[!nzchar(make_model)] <- "Not specified"
      labels <- paste0(
        "[",
        dt$instrument_id,
        "] ",
        label_piece(dt$serial_no, "No serial"),
        " | ",
        make_model,
        " | ",
        label_piece(dt$type_name),
        " | ",
        label_piece(dt$owner_name)
      )
      stats::setNames(as.character(dt$instrument_id), labels)
    }

    instrument_picker_data <- function() {
      if (
        is.null(maintenance_data$instruments) ||
          nrow(maintenance_data$instruments) == 0
      ) {
        return(data.table::data.table())
      }

      dt <- data.table::copy(maintenance_data$instruments)
      dt <- unique(dt, by = "instrument_id")
      dt[,
        status := data.table::fifelse(
          is.na(date_retired),
          "Active",
          paste("Retired", as.character(date_retired))
        )
      ]
      dt[,
        make_model := trimws(paste(
          label_piece(make_name, ""),
          label_piece(model_name, "")
        ))
      ]
      dt[!nzchar(make_model), make_model := "Not specified"]

      dt[
        order(!is.na(date_retired), serial_no, instrument_id),
        .(
          instrument_id,
          Serial = label_piece(serial_no, "No serial"),
          `Asset tag` = label_piece(asset_tag, ""),
          Make = label_piece(make_name, ""),
          Model = label_piece(model_name, ""),
          Type = label_piece(type_name),
          Owner = label_piece(owner_name),
          Status = status,
          `Next due` = data.table::fifelse(
            is.na(date_maintenance_due),
            "",
            as.character(date_maintenance_due)
          )
        )
      ]
    }

    build_observer_choices <- function(df) {
      if (is.null(df) || nrow(df) == 0) {
        return(character())
      }
      stats::setNames(as.character(df$observer_id), df$observer_label)
    }

    build_sensor_type_choices <- function(df) {
      if (is.null(df) || nrow(df) == 0) {
        return(character())
      }
      stats::setNames(as.character(df$sensor_type_id), df$sensor_type)
    }

    build_sensor_make_choices <- function(df) {
      if (is.null(df) || nrow(df) == 0) {
        return(character())
      }
      stats::setNames(as.character(df$make_id), df$make)
    }

    build_sensor_model_choices <- function(df) {
      if (is.null(df) || nrow(df) == 0) {
        return(character())
      }
      stats::setNames(as.character(df$model_id), df$model)
    }

    build_owner_choices <- function(df) {
      if (is.null(df) || nrow(df) == 0) {
        return(character())
      }
      stats::setNames(as.character(df$organization_id), df$name)
    }

    build_supplier_choices <- function(df) {
      if (is.null(df) || nrow(df) == 0) {
        return(character())
      }
      stats::setNames(as.character(df$supplier_id), df$supplier_name)
    }

    build_sensor_choices <- function(df) {
      if (is.null(df) || nrow(df) == 0) {
        return(character())
      }
      dt <- data.table::as.data.table(df)
      labels <- paste0(
        "[",
        dt$sensor_id,
        "] ",
        label_piece(dt$sensor_serial, "No serial"),
        " | ",
        label_piece(dt$sensor_type_label),
        " | ",
        trimws(paste(
          label_piece(dt$sensor_make, ""),
          label_piece(dt$sensor_model, "")
        ))
      )
      labels <- trimws(gsub("\\|\\s*$", "", labels))
      stats::setNames(as.character(dt$sensor_id), labels)
    }

    sensor_label <- function(sensor_id) {
      sid <- suppressWarnings(as.integer(sensor_id))
      sensors <- maintenance_data$sensors
      if (is.na(sid) || is.null(sensors) || nrow(sensors) == 0) {
        return("selected sensor")
      }
      sensor <- data.table::as.data.table(sensors)[sensor_id == sid]
      if (nrow(sensor) == 0) {
        return(paste("sensor", sid))
      }
      paste0(
        label_piece(sensor$sensor_serial[[1]], paste("sensor", sid)),
        " (",
        label_piece(sensor$sensor_type_label[[1]], "type not specified"),
        ")"
      )
    }

    is_blank_sensor <- function(sensor_id) {
      sid <- suppressWarnings(as.integer(sensor_id))
      sensors <- maintenance_data$sensors
      if (is.na(sid) || is.null(sensors) || nrow(sensors) == 0) {
        return(FALSE)
      }
      sensor <- data.table::as.data.table(sensors)[sensor_id == sid]
      if (nrow(sensor) == 0) {
        return(FALSE)
      }
      identical(toupper(trimws(sensor$sensor_serial[[1]])), "BLANK")
    }

    current_sensor_conflicts <- function(
      sensor_id,
      instrument_id,
      slot_number
    ) {
      if (
        is.na(sensor_id) ||
          is.na(instrument_id) ||
          is.na(slot_number) ||
          is_blank_sensor(sensor_id)
      ) {
        return(data.table::data.table())
      }

      data.table::as.data.table(DBI::dbGetQuery(
        con,
        "SELECT cur.instrument_id,
                cur.slot_number,
                cur.event_id,
                cur.obs_datetime,
                i.serial_no,
                i.asset_tag,
                mk.make AS make,
                mdl.model AS model,
                tp.type AS type,
                s.sensor_serial,
                st.sensor_type
         FROM instruments.instrument_sensor_current AS cur
         INNER JOIN instruments.instruments AS i
           ON cur.instrument_id = i.instrument_id
         LEFT JOIN instruments.instrument_makes AS mk ON i.make = mk.make_id
         LEFT JOIN instruments.instrument_models AS mdl ON i.model = mdl.model_id
         LEFT JOIN instruments.instrument_types AS tp ON i.type = tp.type_id
         LEFT JOIN instruments.sensors AS s ON cur.sensor_id = s.sensor_id
         LEFT JOIN instruments.sensor_types AS st
           ON s.sensor_type = st.sensor_type_id
         WHERE cur.sensor_id = $1
           AND NOT (
             cur.instrument_id = $2 AND
             cur.slot_number = $3
           )
         ORDER BY cur.instrument_id, cur.slot_number",
        params = list(sensor_id, instrument_id, slot_number)
      ))
    }

    current_slot_sensor <- function(slot_number) {
      slot_number <- suppressWarnings(as.integer(slot_number))
      current <- maintenance_data$current_slots
      if (is.na(slot_number) || is.null(current) || nrow(current) == 0) {
        return(NULL)
      }
      current <- data.table::as.data.table(current)
      out <- current[current$slot_number == slot_number]
      if (nrow(out) == 0 || is.na(out$sensor_id[[1]])) {
        return(NULL)
      }
      out[1]
    }

    sensor_conflict_table_ui <- function(conflicts) {
      if (is.null(conflicts) || nrow(conflicts) == 0) {
        return(NULL)
      }
      rows <- lapply(seq_len(nrow(conflicts)), function(i) {
        x <- conflicts[i]
        tags$tr(
          tags$td(x$instrument_id),
          tags$td(label_piece(x$serial_no, "No serial")),
          tags$td(label_piece(x$make, "")),
          tags$td(label_piece(x$model, "")),
          tags$td(x$slot_number),
          tags$td(as.character(x$obs_datetime))
        )
      })
      tags$table(
        class = "table table-sm table-striped",
        tags$thead(
          tags$tr(
            tags$th("Instrument ID"),
            tags$th("Serial no"),
            tags$th("Make"),
            tags$th("Model"),
            tags$th("Slot"),
            tags$th("Recorded UTC")
          )
        ),
        tags$tbody(rows)
      )
    }

    build_due_choices <- function(selected_event_id = NA_integer_) {
      due <- maintenance_data$due
      if (is.null(due) || nrow(due) == 0) {
        return(character())
      }
      due <- data.table::copy(due)
      if (is.na(selected_event_id)) {
        due <- due[is.na(completed_event_id)]
      } else {
        due <- due[
          is.na(completed_event_id) | completed_event_id == selected_event_id
        ]
      }
      if (nrow(due) == 0) {
        return(character())
      }
      due[,
        label := paste0(
          as.character(date_maintenance_due),
          " | ",
          label_piece(maintenance_due_note, "No note")
        )
      ]
      due <- due[order(date_maintenance_due, maintenance_due_id)]
      stats::setNames(as.character(due$maintenance_due_id), due$label)
    }

    update_due_choices <- function(selected = character()) {
      updateSelectizeInput(
        session,
        "completed_due_ids",
        choices = build_due_choices(maintenance_data$selected_event_id),
        selected = as.character(selected),
        server = TRUE
      )
    }

    current_report_user <- function() {
      config_user <- session$userData$config$dbUser
      if (!is.null(config_user) && length(config_user) && nzchar(config_user)) {
        return(as.character(config_user[[1]]))
      }
      shiny_user <- session$user
      if (!is.null(shiny_user) && length(shiny_user) && nzchar(shiny_user)) {
        return(as.character(shiny_user[[1]]))
      }
      db_user <- tryCatch(
        DBI::dbGetQuery(con, "SELECT CURRENT_USER AS user_name")$user_name[[1]],
        error = function(e) NA_character_
      )
      label_piece(db_user, "Unknown user")
    }

    fetch_upcoming_maintenance <- function() {
      data.table::as.data.table(DBI::dbGetQuery(
        con,
        "SELECT due.maintenance_due_id,
                due.instrument_id,
                i.serial_no,
                i.asset_tag,
                mk.make AS make,
                mdl.model AS model,
                tp.type AS type,
                org.name AS owner,
                CASE
                  WHEN i.date_retired IS NULL THEN 'Active'
                  ELSE 'Retired ' || i.date_retired::text
                END AS instrument_status,
                due.date_maintenance_due,
                due.date_maintenance_due - CURRENT_DATE AS days_until_due,
                CASE
                  WHEN due.date_maintenance_due < CURRENT_DATE THEN 'Overdue'
                  WHEN due.date_maintenance_due = CURRENT_DATE THEN 'Due today'
                  ELSE 'Upcoming'
                END AS due_status,
                due.maintenance_due_note,
                CONCAT(o.observer_first, ' ', o.observer_last, ' (', o.organization, ')') AS scheduled_by,
                due.obs_datetime AS scheduled_datetime,
                due.created,
                due.modified
         FROM instruments.instrument_maintenance_due AS due
         INNER JOIN instruments.instruments AS i
           ON due.instrument_id = i.instrument_id
         LEFT JOIN instruments.instrument_makes AS mk ON i.make = mk.make_id
         LEFT JOIN instruments.instrument_models AS mdl ON i.model = mdl.model_id
         LEFT JOIN instruments.instrument_types AS tp ON i.type = tp.type_id
         LEFT JOIN public.organizations AS org ON i.owner = org.organization_id
         LEFT JOIN instruments.observers AS o ON due.observer = o.observer_id
         WHERE due.completed_event_id IS NULL
         ORDER BY
           due.date_maintenance_due,
           i.date_retired NULLS FIRST,
           i.serial_no,
           due.maintenance_due_id"
      ))
    }

    upcoming_report_table <- function() {
      df <- maintenance_data$upcoming_due
      if (is.null(df) || nrow(df) == 0) {
        return(data.table::data.table(
          `Due date` = as.Date(character()),
          `Due status` = character(),
          `Days until due` = integer(),
          `Instrument ID` = integer(),
          `Serial no` = character(),
          `Asset tag` = character(),
          Make = character(),
          Model = character(),
          Type = character(),
          Owner = character(),
          `Instrument status` = character(),
          `Maintenance due` = character(),
          `Scheduled by` = character(),
          `Record created` = character()
        ))
      }
      display <- data.table::copy(df)
      display[,
        `Scheduled UTC` := format(
          coerce_utc_datetime(scheduled_datetime),
          "%Y-%m-%d %H:%M %Z"
        )
      ]
      display[, list(
        `Due date` = date_maintenance_due,
        `Due status` = due_status,
        `Days until due` = days_until_due,
        `Instrument ID` = instrument_id,
        `Serial no` = serial_no,
        `Asset tag` = asset_tag,
        Make = make,
        Model = model,
        Type = type,
        Owner = owner,
        `Instrument status` = instrument_status,
        `Maintenance due` = maintenance_due_note,
        `Scheduled by` = scheduled_by,
        `Scheduled UTC`
      )]
    }

    write_upcoming_maintenance_workbook <- function(file) {
      report <- upcoming_report_table()
      generated_at <- scalar_utc_datetime(Sys.time())
      generated_at_text <- format(generated_at, "%Y-%m-%d %H:%M:%S %Z")
      generated_by <- current_report_user()
      metadata <- data.frame(
        `Generated by` = c("Created at", "Open maintenance items"),
        placeholder = c(
          generated_at_text,
          as.character(nrow(report))
        ),
        check.names = FALSE
      )
      names(metadata)[names(metadata) == "placeholder"] <- generated_by

      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "upcoming_maintenance")
      openxlsx::writeData(
        wb,
        "upcoming_maintenance",
        "Upcoming instrument maintenance",
        startRow = 1,
        startCol = 1
      )
      openxlsx::writeData(
        wb,
        "upcoming_maintenance",
        metadata,
        startRow = 3,
        startCol = 1
      )
      start_row <- nrow(metadata) + 6L
      openxlsx::writeData(
        wb,
        "upcoming_maintenance",
        report,
        startRow = start_row,
        startCol = 1,
        withFilter = TRUE
      )
      openxlsx::freezePane(
        wb,
        "upcoming_maintenance",
        firstActiveRow = start_row + 1L
      )
      openxlsx::setColWidths(
        wb,
        "upcoming_maintenance",
        cols = seq_len(max(1L, ncol(report))),
        widths = "auto"
      )
      openxlsx::addWorksheet(wb, "metadata")
      openxlsx::writeData(wb, "metadata", metadata)
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }

    refresh_lookups <- function(
      instrument_selected = isolate(input$instrument_id),
      observer_selected = isolate(input$observer),
      sensor_selected = isolate(input$sensor_id)
    ) {
      maintenance_data$instruments <- data.table::as.data.table(DBI::dbGetQuery(
        con,
        "SELECT i.instrument_id,
                i.serial_no,
                i.asset_tag,
                i.holds_replaceable_sensors,
                i.date_retired,
                mk.make AS make_name,
                mdl.model AS model_name,
                tp.type AS type_name,
                org.name AS owner_name,
                due.date_maintenance_due,
                due.maintenance_due_note
         FROM instruments.instruments AS i
         LEFT JOIN instruments.instrument_makes AS mk ON i.make = mk.make_id
         LEFT JOIN instruments.instrument_models AS mdl ON i.model = mdl.model_id
         LEFT JOIN instruments.instrument_types AS tp ON i.type = tp.type_id
         LEFT JOIN public.organizations AS org ON i.owner = org.organization_id
         LEFT JOIN LATERAL (
           SELECT date_maintenance_due,
                  maintenance_due_note
           FROM instruments.instrument_maintenance_due
           WHERE instrument_id = i.instrument_id
             AND completed_event_id IS NULL
           ORDER BY date_maintenance_due ASC, obs_datetime DESC
           LIMIT 1
         ) AS due ON TRUE
         ORDER BY i.date_retired NULLS FIRST, i.serial_no, i.instrument_id"
      ))

      maintenance_data$observers <- data.table::as.data.table(DBI::dbGetQuery(
        con,
        "SELECT observer_id,
                CONCAT(observer_first, ' ', observer_last, ' (', organization, ')') AS observer_label
         FROM instruments.observers
         ORDER BY observer_first, observer_last, organization"
      ))

      maintenance_data$sensor_types <- data.table::as.data.table(DBI::dbGetQuery(
        con,
        "SELECT sensor_type_id,
                sensor_type,
                sensor_type_description
         FROM instruments.sensor_types
         ORDER BY sensor_type"
      ))

      maintenance_data$sensor_makes <- data.table::as.data.table(DBI::dbGetQuery(
        con,
        "SELECT make_id,
                make,
                description
         FROM instruments.sensor_makes
         ORDER BY make"
      ))

      maintenance_data$sensor_models <- data.table::as.data.table(DBI::dbGetQuery(
        con,
        "SELECT model_id,
                model,
                description
         FROM instruments.sensor_models
         ORDER BY model"
      ))

      maintenance_data$owners <- data.table::as.data.table(DBI::dbGetQuery(
        con,
        "SELECT organization_id, name
         FROM public.organizations
         ORDER BY name"
      ))

      maintenance_data$suppliers <- data.table::as.data.table(DBI::dbGetQuery(
        con,
        "SELECT supplier_id, supplier_name
         FROM instruments.suppliers
         ORDER BY supplier_name"
      ))

      maintenance_data$sensors <- data.table::as.data.table(DBI::dbGetQuery(
        con,
        "SELECT s.sensor_id,
                s.sensor_type,
                st.sensor_type AS sensor_type_label,
                s.sensor_serial,
                s.sensor_make AS sensor_make_id,
                sm.make AS sensor_make,
                s.sensor_model AS sensor_model_id,
                smdl.model AS sensor_model,
                s.owner,
                org.name AS owner_name,
                s.supplier_id,
                sup.supplier_name,
                s.sensor_asset_tag,
                s.date_retired
         FROM instruments.sensors AS s
         LEFT JOIN instruments.sensor_types AS st ON s.sensor_type = st.sensor_type_id
         LEFT JOIN instruments.sensor_makes AS sm ON s.sensor_make = sm.make_id
         LEFT JOIN instruments.sensor_models AS smdl ON s.sensor_model = smdl.model_id
         LEFT JOIN public.organizations AS org ON s.owner = org.organization_id
         LEFT JOIN instruments.suppliers AS sup ON s.supplier_id = sup.supplier_id
         ORDER BY s.date_retired NULLS FIRST, st.sensor_type, s.sensor_serial, s.sensor_id"
      ))

      updateSelectizeInput(
        session,
        "instrument_id",
        choices = build_instrument_choices(maintenance_data$instruments),
        selected = normalize_select_value(instrument_selected),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "observer",
        choices = build_observer_choices(maintenance_data$observers),
        selected = normalize_select_value(observer_selected),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "sensor_id",
        choices = c(
          "No sensor / remove sensor" = "",
          build_sensor_choices(maintenance_data$sensors)
        ),
        selected = normalize_select_value(sensor_selected),
        server = TRUE
      )
    }

    refresh_instrument_details <- function(
      instrument_id = selected_instrument_id()
    ) {
      if (is.na(instrument_id)) {
        maintenance_data$maintenance <- data.table::data.table()
        maintenance_data$due <- data.table::data.table()
        maintenance_data$current_slots <- data.table::data.table()
        maintenance_data$sensor_history <- data.table::data.table()
        maintenance_data$selected_event_id <- NA_integer_
        maintenance_data$selected_due_id <- NA_integer_
        updateTextAreaInput(session, "maintenance_note", value = "")
        updateSelectizeInput(
          session,
          "completed_due_ids",
          choices = character(),
          selected = character(0)
        )
        updateDateInput(session, "date_maintenance_due", value = NULL)
        updateTextAreaInput(session, "maintenance_due_note", value = "")
        return(invisible(NULL))
      }

      maintenance_data$maintenance <- data.table::as.data.table(DBI::dbGetQuery(
        con,
        "SELECT im.event_id,
                im.instrument_id,
                im.observer,
                CONCAT(o.observer_first, ' ', o.observer_last, ' (', o.organization, ')') AS observer_name,
                im.obs_datetime,
                im.note,
                completed_due.completed_due,
                im.created_by,
                im.modified_by,
                im.created,
                im.modified
         FROM instruments.instrument_maintenance AS im
         LEFT JOIN instruments.observers AS o ON im.observer = o.observer_id
         LEFT JOIN LATERAL (
           SELECT string_agg(
             to_char(due.date_maintenance_due, 'YYYY-MM-DD') || ' | ' ||
               due.maintenance_due_note,
             '; '
             ORDER BY due.date_maintenance_due, due.maintenance_due_id
           ) AS completed_due
           FROM instruments.instrument_maintenance_due AS due
           WHERE due.completed_event_id = im.event_id
         ) AS completed_due ON TRUE
         WHERE im.instrument_id = $1
         ORDER BY im.obs_datetime DESC, im.event_id DESC",
        params = list(instrument_id)
      ))

      maintenance_data$due <- data.table::as.data.table(DBI::dbGetQuery(
        con,
        "SELECT due.maintenance_due_id,
                due.instrument_id,
                due.observer,
                CONCAT(o.observer_first, ' ', o.observer_last, ' (', o.organization, ')') AS observer_name,
                due.obs_datetime,
                due.date_maintenance_due,
                due.maintenance_due_note,
                due.completed_event_id,
                completed.obs_datetime AS completed_obs_datetime,
                completed.note AS completed_note,
                due.created_by,
                due.modified_by,
                due.created,
                due.modified
         FROM instruments.instrument_maintenance_due AS due
         LEFT JOIN instruments.observers AS o ON due.observer = o.observer_id
         LEFT JOIN instruments.instrument_maintenance AS completed
           ON due.completed_event_id = completed.event_id
         WHERE due.instrument_id = $1
         ORDER BY
           due.completed_event_id IS NOT NULL,
           due.date_maintenance_due,
           due.maintenance_due_id",
        params = list(instrument_id)
      ))

      maintenance_data$current_slots <- data.table::as.data.table(DBI::dbGetQuery(
        con,
        "SELECT cur.slot_number,
                cur.sensor_id,
                COALESCE(s.sensor_serial, cur.sensor_serial) AS sensor_serial,
                st.sensor_type AS sensor_type,
                cur.note,
                cur.event_id,
                cur.observer,
                CONCAT(o.observer_first, ' ', o.observer_last, ' (', o.organization, ')') AS observer_name,
                cur.obs_datetime
         FROM instruments.instrument_sensor_current AS cur
         LEFT JOIN instruments.sensors AS s ON cur.sensor_id = s.sensor_id
         LEFT JOIN instruments.sensor_types AS st ON s.sensor_type = st.sensor_type_id
         LEFT JOIN instruments.observers AS o ON cur.observer = o.observer_id
         WHERE cur.instrument_id = $1
         ORDER BY cur.slot_number",
        params = list(instrument_id)
      ))

      maintenance_data$sensor_history <- data.table::as.data.table(DBI::dbGetQuery(
        con,
        "SELECT e.event_id,
                e.instrument_id,
                e.observer,
                CONCAT(o.observer_first, ' ', o.observer_last, ' (', o.organization, ')') AS observer_name,
                e.obs_datetime,
                e.event_note,
                slots.slot_number,
                slots.sensor_id,
                s.sensor_serial,
                st.sensor_type,
                slots.note AS slot_note
         FROM instruments.instrument_sensor_events AS e
         LEFT JOIN instruments.observers AS o ON e.observer = o.observer_id
         LEFT JOIN instruments.instrument_sensor_event_slots AS slots
           ON e.event_id = slots.event_id
         LEFT JOIN instruments.sensors AS s ON slots.sensor_id = s.sensor_id
         LEFT JOIN instruments.sensor_types AS st ON s.sensor_type = st.sensor_type_id
         WHERE e.instrument_id = $1
         ORDER BY e.obs_datetime DESC, e.event_id DESC, slots.slot_number",
        params = list(instrument_id)
      ))

      clear_due_form()
      update_due_choices()

      invisible(NULL)
    }

    clear_due_form <- function() {
      maintenance_data$selected_due_id <- NA_integer_
      updateDateInput(session, "date_maintenance_due", value = NULL)
      updateTextAreaInput(session, "maintenance_due_note", value = "")
      updateActionButton(session, "save_due", label = "Schedule due item")
      proxy <- DT::dataTableProxy("due_table", session = session)
      DT::selectRows(proxy, NULL)
      invisible(NULL)
    }

    clear_maintenance_form <- function() {
      maintenance_data$selected_event_id <- NA_integer_
      updateTextAreaInput(session, "maintenance_note", value = "")
      update_due_choices(character())
      updateActionButton(session, "save_maintenance", label = "Save event")
      proxy <- DT::dataTableProxy("maintenance_table", session = session)
      DT::selectRows(proxy, NULL)
      invisible(NULL)
    }

    render_empty_table <- function(message) {
      DT::datatable(
        data.frame(message, check.names = FALSE),
        rownames = FALSE,
        options = list(dom = "t")
      )
    }

    output$instrument_picker_table <- DT::renderDT({
      df <- instrument_picker_data()
      if (is.null(df) || nrow(df) == 0) {
        return(render_empty_table("No instruments found."))
      }

      df$Make <- as.factor(df$Make)
      df$Model <- as.factor(df$Model)
      df$Type <- as.factor(df$Type)
      df$Owner <- as.factor(df$Owner)
      df$Status <- as.factor(df$Status)
      DT::datatable(
        df,
        filter = "top",
        selection = "single",
        rownames = FALSE,
        options = list(
          columnDefs = list(list(targets = 0, visible = FALSE)),
          scrollX = TRUE,
          autoWidth = TRUE,
          pageLength = 10
        )
      )
    })

    output$instrument_summary <- renderUI({
      instrument_id <- selected_instrument_id()
      if (is.na(instrument_id) || is.null(maintenance_data$instruments)) {
        return(tags$h5("Select an instrument."))
      }
      instrument_row <- match(
        instrument_id,
        maintenance_data$instruments$instrument_id
      )
      if (is.na(instrument_row)) {
        return(tags$h5("Select an instrument."))
      }
      instrument <- maintenance_data$instruments[instrument_row, ]
      status <- if (is.na(instrument$date_retired[[1]])) {
        "Active"
      } else {
        paste("Retired", as.character(instrument$date_retired[[1]]))
      }
      tags$div(
        class = "mb-3",
        tags$h4(label_piece(instrument$serial_no[[1]], "No serial")),
        tags$p(
          class = "text-muted",
          paste(
            label_piece(instrument$make_name[[1]], ""),
            label_piece(instrument$model_name[[1]], ""),
            "|",
            label_piece(instrument$type_name[[1]]),
            "|",
            label_piece(instrument$owner_name[[1]]),
            "|",
            status
          )
        )
      )
    })

    output$due_summary <- renderUI({
      due <- maintenance_data$due
      if (is.null(due) || nrow(due) == 0) {
        return(tags$p(
          class = "text-muted",
          "No maintenance due items are currently logged."
        ))
      }
      open_due <- due[is.na(completed_event_id)]
      if (nrow(open_due) == 0) {
        return(tags$p(
          class = "text-muted",
          paste(nrow(due), "completed due item(s) logged; none are open.")
        ))
      }
      data.table::setorder(open_due, date_maintenance_due, maintenance_due_id)
      due_date <- open_due$date_maintenance_due[[1]]
      days_until_due <- as.integer(due_date - Sys.Date())
      status <- if (is.na(days_until_due)) {
        ""
      } else if (days_until_due < 0) {
        paste(abs(days_until_due), "days overdue")
      } else if (days_until_due == 0) {
        "Due today"
      } else {
        paste(days_until_due, "days from today")
      }
      tags$div(
        tags$h4(as.character(due_date)),
        tags$p(
          class = "text-muted",
          paste(status, "|", nrow(open_due), "open due item(s)")
        ),
        tags$p(safe_text(open_due$maintenance_due_note[[1]])),
        tags$p(
          class = "text-muted",
          paste(
            "Recorded",
            format(
              coerce_utc_datetime(open_due$obs_datetime[[1]]),
              "%Y-%m-%d %H:%M %Z"
            ),
            "by",
            label_piece(open_due$observer_name[[1]])
          )
        )
      )
    })

    output$due_table <- DT::renderDT({
      df <- maintenance_data$due
      if (is.null(df) || nrow(df) == 0) {
        return(render_empty_table("No maintenance due items found."))
      }
      display <- data.table::copy(df)
      display[,
        Status := ifelse(
          is.na(completed_event_id),
          "Open",
          "Completed"
        )
      ]
      completed_time <- format(
        coerce_utc_datetime(display$completed_obs_datetime),
        "%Y-%m-%d %H:%M %Z"
      )
      completed_time[is.na(display$completed_obs_datetime)] <- ""
      display <- display[, list(
        maintenance_due_id,
        Status,
        `Due date` = date_maintenance_due,
        `Due note` = maintenance_due_note,
        `Completed UTC` = completed_time,
        `Recorded by` = observer_name,
        `Recorded UTC` = format(
          coerce_utc_datetime(obs_datetime),
          "%Y-%m-%d %H:%M %Z"
        )
      )]
      display$Status <- as.factor(display$Status)
      display$`Recorded by` <- as.factor(display$`Recorded by`)
      DT::datatable(
        display,
        filter = "top",
        selection = "single",
        rownames = FALSE,
        options = list(
          columnDefs = list(list(targets = 0, visible = FALSE)),
          scrollX = TRUE,
          autoWidth = TRUE
        )
      )
    })

    output$upcoming_maintenance_table <- DT::renderDT({
      display <- upcoming_report_table()
      if (nrow(display) == 0) {
        return(render_empty_table("No upcoming maintenance due items found."))
      }
      display$`Due status` <- as.factor(display$`Due status`)
      display$Make <- as.factor(display$Make)
      display$Model <- as.factor(display$Model)
      display$Type <- as.factor(display$Type)
      display$Owner <- as.factor(display$Owner)
      display$`Instrument status` <- as.factor(display$`Instrument status`)
      DT::datatable(
        display,
        filter = "top",
        selection = "none",
        rownames = FALSE,
        options = list(
          scrollX = TRUE,
          autoWidth = TRUE,
          pageLength = 15
        )
      )
    })

    output$maintenance_table <- DT::renderDT({
      df <- maintenance_data$maintenance
      if (is.null(df) || nrow(df) == 0) {
        return(render_empty_table("No maintenance events found."))
      }
      display <- data.table::copy(df)[, list(
        `Date/time UTC` = obs_datetime,
        `Recorded by` = observer_name,
        Note = note,
        `Completed task` = completed_due
      )]
      DT::datatable(
        display,
        filter = "top",
        selection = "single",
        rownames = FALSE,
        options = list(scrollX = TRUE, autoWidth = TRUE)
      )
    })

    output$current_slots_table <- DT::renderDT({
      df <- maintenance_data$current_slots
      if (is.null(df) || nrow(df) == 0) {
        return(render_empty_table("No current sensor slots found."))
      }
      display <- data.table::copy(df)[, list(
        Slot = slot_number,
        `Sensor serial` = sensor_serial,
        `Sensor type` = sensor_type,
        Note = note,
        `Recorded by` = observer_name,
        `Date/time UTC` = obs_datetime
      )]
      display$Slot <- as.integer(display$Slot)
      display$`Sensor type` <- as.factor(display$`Sensor type`)
      display$`Recorded by` <- as.factor(display$`Recorded by`)
      DT::datatable(
        display,
        filter = "top",
        selection = "single",
        rownames = FALSE,
        options = list(scrollX = TRUE, autoWidth = TRUE)
      )
    })

    output$sensor_history_table <- DT::renderDT({
      df <- maintenance_data$sensor_history
      if (is.null(df) || nrow(df) == 0) {
        return(render_empty_table("No sensor slot events found."))
      }
      display <- data.table::copy(df)[, list(
        `Date/time UTC` = obs_datetime,
        `Recorded by` = observer_name,
        Slot = slot_number,
        `Sensor serial` = sensor_serial,
        `Sensor type` = sensor_type,
        `Slot note` = slot_note,
        `Event note` = event_note
      )]
      display$Slot <- as.integer(display$Slot)
      display$`Sensor type` <- as.factor(display$`Sensor type`)
      display$`Recorded by` <- as.factor(display$`Recorded by`)
      DT::datatable(
        display,
        filter = "top",
        selection = "single",
        rownames = FALSE,
        options = list(scrollX = TRUE, autoWidth = TRUE)
      )
    })

    upcoming_maintenance_filename <- function() {
      paste0(
        "upcoming_instrument_maintenance_",
        format(Sys.Date(), "%Y%m%d"),
        ".xlsx"
      )
    }

    output$download_upcoming_maintenance_top <- downloadHandler(
      filename = upcoming_maintenance_filename,
      content = write_upcoming_maintenance_workbook
    )

    output$download_upcoming_maintenance_bottom <- downloadHandler(
      filename = upcoming_maintenance_filename,
      content = write_upcoming_maintenance_workbook
    )

    refresh_lookups()
    shinyWidgets::updateAirDateInput(
      session,
      "obs_datetime",
      value = scalar_utc_datetime(Sys.time()),
      tz = air_datetime_widget_timezone(default_input_timezone())
    )

    observeEvent(
      input$timezone,
      {
        shift_air_datetime_input_timezone(
          session = session,
          input = input,
          input_id = "obs_datetime",
          tz_name = input$timezone %||% default_input_timezone()
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$reload,
      {
        refresh_lookups()
        refresh_instrument_details()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$show_all_upcoming,
      {
        upcoming <- with_db_feedback(fetch_upcoming_maintenance())
        if (is.null(upcoming)) {
          return()
        }
        maintenance_data$upcoming_due <- upcoming
        showModal(modalDialog(
          title = "Upcoming maintenance for all instruments",
          div(
            class = "d-flex justify-content-between align-items-center mb-3",
            tags$p(
              class = "text-muted mb-0",
              paste(nrow(upcoming), "open maintenance due item(s)")
            ),
            downloadButton(
              ns("download_upcoming_maintenance_top"),
              "Export Excel",
              class = "btn-warning"
            )
          ),
          DT::DTOutput(ns("upcoming_maintenance_table")),
          footer = tagList(
            downloadButton(
              ns("download_upcoming_maintenance_bottom"),
              "Export Excel",
              class = "btn-warning"
            ),
            modalButton("Close")
          ),
          size = "xl",
          easyClose = TRUE
        ))
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$clear,
      {
        updateSelectizeInput(session, "instrument_id", selected = character(0))
        clear_maintenance_form()
        clear_due_form()
        refresh_instrument_details(NA_integer_)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$choose_instrument_table,
      {
        showModal(modalDialog(
          title = "Find instrument",
          DT::DTOutput(ns("instrument_picker_table")),
          footer = modalButton("Cancel"),
          size = "xl",
          easyClose = TRUE
        ))
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$instrument_picker_table_rows_selected,
      {
        selected <- input$instrument_picker_table_rows_selected
        if (!length(selected)) {
          return()
        }
        df <- instrument_picker_data()
        if (nrow(df) < selected[[1]]) {
          return()
        }
        instrument_id <- df$instrument_id[[selected[[1]]]]
        updateSelectizeInput(
          session,
          "instrument_id",
          selected = as.character(instrument_id)
        )
        removeModal()
        clear_maintenance_form()
        clear_due_form()
        refresh_instrument_details(instrument_id)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$instrument_id,
      {
        clear_maintenance_form()
        clear_due_form()
        refresh_instrument_details()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$maintenance_table_rows_selected,
      {
        selected <- input$maintenance_table_rows_selected
        if (!length(selected)) {
          return()
        }
        df <- maintenance_data$maintenance
        if (nrow(df) < selected[[1]]) {
          return()
        }
        record <- df[selected[[1]], ]
        maintenance_data$selected_event_id <- record$event_id[[1]]
        completed_due_ids <- maintenance_data$due[
          !is.na(completed_event_id) &
            completed_event_id == record$event_id[[1]],
          as.character(maintenance_due_id)
        ]
        updateTextAreaInput(
          session,
          "maintenance_note",
          value = safe_text(record$note[[1]])
        )
        updateSelectizeInput(
          session,
          "observer",
          selected = as.character(record$observer[[1]])
        )
        shinyWidgets::updateAirDateInput(
          session,
          "obs_datetime",
          value = coerce_utc_datetime(record$obs_datetime[[1]]),
          tz = air_datetime_widget_timezone(
            input$timezone %||% default_input_timezone()
          )
        )
        update_due_choices(completed_due_ids)
        updateActionButton(session, "save_maintenance", label = "Update event")
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$clear_maintenance_selection,
      {
        clear_maintenance_form()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$due_table_rows_selected,
      {
        selected <- input$due_table_rows_selected
        if (!length(selected)) {
          return()
        }
        df <- maintenance_data$due
        if (nrow(df) < selected[[1]]) {
          return()
        }
        record <- df[selected[[1]], ]
        maintenance_data$selected_due_id <- record$maintenance_due_id[[1]]
        updateDateInput(
          session,
          "date_maintenance_due",
          value = record$date_maintenance_due[[1]]
        )
        updateTextAreaInput(
          session,
          "maintenance_due_note",
          value = safe_text(record$maintenance_due_note[[1]])
        )
        updateSelectizeInput(
          session,
          "observer",
          selected = as.character(record$observer[[1]])
        )
        shinyWidgets::updateAirDateInput(
          session,
          "obs_datetime",
          value = coerce_utc_datetime(record$obs_datetime[[1]]),
          tz = air_datetime_widget_timezone(
            input$timezone %||% default_input_timezone()
          )
        )
        updateActionButton(session, "save_due", label = "Update due item")
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$new_due,
      {
        clear_due_form()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$current_slots_table_rows_selected,
      {
        selected <- input$current_slots_table_rows_selected
        if (!length(selected)) {
          return()
        }
        df <- maintenance_data$current_slots
        if (nrow(df) < selected[[1]]) {
          return()
        }
        record <- df[selected[[1]], ]
        updateNumericInput(
          session,
          "slot_number",
          value = record$slot_number[[1]]
        )
        updateSelectizeInput(
          session,
          "sensor_id",
          selected = normalize_select_value(record$sensor_id[[1]])
        )
        updateTextAreaInput(
          session,
          "slot_note",
          value = safe_text(record$note[[1]])
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$observer,
      {
        value <- normalize_select_value(input$observer)
        if (
          !length(value) ||
            value %in% as.character(maintenance_data$observers$observer_id)
        ) {
          return()
        }
        updateSelectizeInput(session, "observer", selected = character(0))
        showModal(modalDialog(
          title = "Add new observer",
          textInput(ns("new_observer_first"), "First name", value = value),
          textInput(ns("new_observer_last"), "Last name"),
          textInput(ns("new_observer_org"), "Organization"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("add_new_observer"), "Add observer")
          ),
          easyClose = TRUE
        ))
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$add_new_observer,
      {
        if (
          !nzchar(trimws(input$new_observer_first)) ||
            !nzchar(trimws(input$new_observer_last)) ||
            !nzchar(trimws(input$new_observer_org))
        ) {
          showNotification(
            "Observer first name, last name, and organization are required.",
            type = "error"
          )
          return()
        }
        new_id <- with_db_feedback(
          DBI::dbGetQuery(
            con,
            "INSERT INTO instruments.observers
           (observer_first, observer_last, organization)
           VALUES ($1, $2, $3)
           RETURNING observer_id",
            params = list(
              trimws(input$new_observer_first),
              trimws(input$new_observer_last),
              trimws(input$new_observer_org)
            )
          )$observer_id[[1]],
          success = "Observer added."
        )
        if (is.null(new_id)) {
          return()
        }
        refresh_lookups(observer_selected = as.character(new_id))
        removeModal()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$save_maintenance,
      {
        instrument_id <- selected_instrument_id()
        observer_id <- selected_observer_id()
        obs_datetime <- scalar_utc_datetime(input$obs_datetime)
        note <- blank_to_na(input$maintenance_note)

        missing_fields <- c()
        if (is.na(instrument_id)) {
          missing_fields <- c(missing_fields, "Instrument")
        }
        if (is.na(observer_id)) {
          missing_fields <- c(missing_fields, "Observer")
        }
        if (!length(obs_datetime) || is.na(obs_datetime)) {
          missing_fields <- c(missing_fields, "Record date/time")
        }
        if (is.na(note)) {
          missing_fields <- c(missing_fields, "Maintenance performed")
        }
        if (length(missing_fields) > 0) {
          showNotification(
            paste("Required fields:", paste(missing_fields, collapse = ", ")),
            type = "error",
            duration = NULL
          )
          return()
        }

        is_new_event <- is.na(maintenance_data$selected_event_id)
        completed_due_ids <- selected_due_ids()
        if (length(completed_due_ids)) {
          due <- maintenance_data$due
          if (is.null(due) || nrow(due) == 0) {
            valid_due_ids <- integer()
          } else if (is_new_event) {
            valid_due_ids <- due[is.na(completed_event_id), maintenance_due_id]
          } else {
            valid_due_ids <- due[
              is.na(completed_event_id) |
                completed_event_id == maintenance_data$selected_event_id,
              maintenance_due_id
            ]
          }
          if (any(!completed_due_ids %in% valid_due_ids)) {
            showNotification(
              "Select only open due items for this instrument.",
              type = "error",
              duration = NULL
            )
            return()
          }
        }

        result <- with_db_feedback(
          {
            DBI::dbBegin(con)
            tryCatch(
              {
                if (is_new_event) {
                  event_id <- DBI::dbGetQuery(
                    con,
                    "INSERT INTO instruments.instrument_maintenance
                 (instrument_id, observer, obs_datetime, note)
                 VALUES ($1, $2, $3, $4)
                 RETURNING event_id",
                    params = list(
                      instrument_id,
                      observer_id,
                      obs_datetime,
                      note
                    )
                  )$event_id[[1]]
                } else {
                  event_id <- maintenance_data$selected_event_id
                  DBI::dbExecute(
                    con,
                    "UPDATE instruments.instrument_maintenance
                 SET instrument_id = $1,
                     observer = $2,
                     obs_datetime = $3,
                     note = $4
                 WHERE event_id = $5",
                    params = list(
                      instrument_id,
                      observer_id,
                      obs_datetime,
                      note,
                      event_id
                    )
                  )
                }
                DBI::dbExecute(
                  con,
                  "UPDATE instruments.instrument_maintenance_due
               SET completed_event_id = NULL
               WHERE completed_event_id = $1",
                  params = list(event_id)
                )
                for (due_id in completed_due_ids) {
                  DBI::dbExecute(
                    con,
                    "UPDATE instruments.instrument_maintenance_due
                 SET completed_event_id = $1
                 WHERE maintenance_due_id = $2
                   AND instrument_id = $3",
                    params = list(event_id, due_id, instrument_id)
                  )
                }
                DBI::dbCommit(con)
                event_id
              },
              error = function(e) {
                DBI::dbRollback(con)
                stop(e)
              }
            )
          },
          success = if (is_new_event) {
            "Maintenance event saved."
          } else {
            "Maintenance event updated."
          }
        )
        if (is.null(result)) {
          return()
        }
        clear_maintenance_form()
        refresh_instrument_details(instrument_id)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$save_due,
      {
        instrument_id <- selected_instrument_id()
        observer_id <- selected_observer_id()
        obs_datetime <- scalar_utc_datetime(input$obs_datetime)
        due_date <- date_or_na(input$date_maintenance_due)
        due_note <- blank_to_na(input$maintenance_due_note)

        missing_fields <- c()
        if (is.na(instrument_id)) {
          missing_fields <- c(missing_fields, "Instrument")
        }
        if (is.na(observer_id)) {
          missing_fields <- c(missing_fields, "Observer")
        }
        if (!length(obs_datetime) || is.na(obs_datetime)) {
          missing_fields <- c(missing_fields, "Record date/time")
        }
        if (is.na(due_date)) {
          missing_fields <- c(missing_fields, "Due date")
        }
        if (is.na(due_note)) {
          missing_fields <- c(missing_fields, "Maintenance due note")
        }
        if (length(missing_fields) > 0) {
          showNotification(
            paste("Required fields:", paste(missing_fields, collapse = ", ")),
            type = "error",
            duration = NULL
          )
          return()
        }

        is_new_due <- is.na(maintenance_data$selected_due_id)
        if (is_new_due) {
          result <- with_db_feedback(
            DBI::dbGetQuery(
              con,
              "INSERT INTO instruments.instrument_maintenance_due
           (instrument_id, observer, obs_datetime, date_maintenance_due, maintenance_due_note)
           VALUES ($1, $2, $3, $4, $5)
           RETURNING maintenance_due_id",
              params = list(
                instrument_id,
                observer_id,
                obs_datetime,
                due_date,
                due_note
              )
            )$maintenance_due_id[[1]],
            success = "Maintenance due item scheduled."
          )
        } else {
          result <- with_db_feedback(
            DBI::dbExecute(
              con,
              "UPDATE instruments.instrument_maintenance_due
           SET observer = $1,
               obs_datetime = $2,
               date_maintenance_due = $3,
               maintenance_due_note = $4
           WHERE maintenance_due_id = $5
             AND instrument_id = $6",
              params = list(
                observer_id,
                obs_datetime,
                due_date,
                due_note,
                maintenance_data$selected_due_id,
                instrument_id
              )
            ),
            success = "Maintenance due item updated."
          )
        }
        if (is.null(result)) {
          return()
        }
        clear_due_form()
        refresh_lookups()
        refresh_instrument_details(instrument_id)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$delete_due,
      {
        instrument_id <- selected_instrument_id()
        if (is.na(instrument_id)) {
          showNotification("Select an instrument first.", type = "error")
          return()
        }
        if (is.na(maintenance_data$selected_due_id)) {
          showNotification("Select a due item first.", type = "error")
          return()
        }
        result <- with_db_feedback(
          DBI::dbExecute(
            con,
            "DELETE FROM instruments.instrument_maintenance_due
           WHERE maintenance_due_id = $1
             AND instrument_id = $2
             AND completed_event_id IS NULL",
            params = list(maintenance_data$selected_due_id, instrument_id)
          )
        )
        if (is.null(result)) {
          return()
        }
        if (result < 1) {
          showNotification(
            "Only open due items can be deleted.",
            type = "error",
            duration = NULL
          )
          return()
        }
        showNotification("Maintenance due item deleted.", type = "message")
        clear_due_form()
        refresh_lookups()
        refresh_instrument_details(instrument_id)
      },
      ignoreInit = TRUE
    )

    save_sensor_slot_event <- function(
      instrument_id,
      observer_id,
      obs_datetime,
      slot_number,
      sensor_id,
      slot_note,
      event_note,
      move_conflicts = data.table::data.table(),
      retire_sensor_id = NA_integer_,
      retired_by = NA_character_,
      date_retired = as.Date(NA)
    ) {
      DBI::dbBegin(con)
      tryCatch(
        {
          if (!is.null(move_conflicts) && nrow(move_conflicts) > 0) {
            move_conflicts <- data.table::as.data.table(move_conflicts)
            move_note <- paste0(
              "Sensor moved to instrument ",
              instrument_id,
              " slot ",
              slot_number,
              "."
            )
            for (old_instrument_id in unique(move_conflicts$instrument_id)) {
              event_id <- DBI::dbGetQuery(
                con,
                "INSERT INTO instruments.instrument_sensor_events
                 (instrument_id, observer, obs_datetime, event_note)
                 VALUES ($1, $2, $3, $4)
                 RETURNING event_id",
                params = list(
                  old_instrument_id,
                  observer_id,
                  obs_datetime,
                  move_note
                )
              )$event_id[[1]]

              old_slots <- move_conflicts[instrument_id == old_instrument_id]
              for (i in seq_len(nrow(old_slots))) {
                DBI::dbExecute(
                  con,
                  "INSERT INTO instruments.instrument_sensor_event_slots
                   (event_id, slot_number, sensor_id, note)
                   VALUES ($1, $2, NULL, $3)",
                  params = list(
                    event_id,
                    old_slots$slot_number[[i]],
                    move_note
                  )
                )
              }
            }
          }

          event_id <- DBI::dbGetQuery(
            con,
            "INSERT INTO instruments.instrument_sensor_events
             (instrument_id, observer, obs_datetime, event_note)
             VALUES ($1, $2, $3, $4)
             RETURNING event_id",
            params = list(
              instrument_id,
              observer_id,
              obs_datetime,
              event_note
            )
          )$event_id[[1]]

          DBI::dbExecute(
            con,
            "INSERT INTO instruments.instrument_sensor_event_slots
             (event_id, slot_number, sensor_id, note)
             VALUES ($1, $2, $3, $4)",
            params = list(event_id, slot_number, sensor_id, slot_note)
          )
          retire_sensor_id <- suppressWarnings(as.integer(retire_sensor_id))
          if (!is.na(retire_sensor_id)) {
            DBI::dbExecute(
              con,
              "UPDATE instruments.sensors
               SET retired_by = $1,
                   date_retired = $2
               WHERE sensor_id = $3",
              params = list(
                blank_to_na(retired_by),
                date_or_na(date_retired),
                retire_sensor_id
              )
            )
          }
          DBI::dbCommit(con)
          event_id
        },
        error = function(e) {
          DBI::dbRollback(con)
          stop(e)
        }
      )
    }

    show_sensor_move_modal <- function(pending, conflicts) {
      showModal(modalDialog(
        title = "Sensor already assigned",
        size = "l",
        easyClose = FALSE,
        p(
          sensor_label(pending$sensor_id),
          " is currently assigned to another instrument slot."
        ),
        p(
          "To record this event, first remove the old current assignment and ",
          "then assign the sensor to the selected instrument."
        ),
        sensor_conflict_table_ui(conflicts),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("confirm_sensor_move"),
            "Remove old assignment and save",
            class = "btn-warning"
          )
        )
      ))
    }

    show_sensor_unassign_modal <- function(pending, current_sensor) {
      showModal(modalDialog(
        title = "Remove sensor from slot",
        size = "m",
        easyClose = FALSE,
        p(
          sensor_label(current_sensor$sensor_id[[1]]),
          " is being removed from slot ",
          current_sensor$slot_number[[1]],
          "."
        ),
        p("Do you also want to retire this sensor record?"),
        bslib::layout_columns(
          col_widths = c(6, 6),
          dateInput(
            ns("retire_unassigned_sensor_date"),
            "Retirement date",
            value = Sys.Date()
          ),
          textInput(
            ns("retire_unassigned_sensor_by"),
            "Retired by",
            value = label_piece(pending$observer_label, "")
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("confirm_sensor_unassign_only"),
            "Unassign only"
          ),
          actionButton(
            ns("confirm_sensor_unassign_retire"),
            "Retire and unassign",
            class = "btn-warning"
          )
        )
      ))
    }

    observeEvent(
      input$save_slot_event,
      {
        instrument_id <- selected_instrument_id()
        observer_id <- selected_observer_id()
        obs_datetime <- scalar_utc_datetime(input$obs_datetime)
        slot_number <- int_or_na(input$slot_number)
        sensor_id <- int_or_na(input$sensor_id)
        slot_note <- blank_to_na(input$slot_note)
        event_note <- blank_to_na(input$sensor_event_note)

        missing_fields <- c()
        if (is.na(instrument_id)) {
          missing_fields <- c(missing_fields, "Instrument")
        }
        if (is.na(observer_id)) {
          missing_fields <- c(missing_fields, "Observer")
        }
        if (!length(obs_datetime) || is.na(obs_datetime)) {
          missing_fields <- c(missing_fields, "Record date/time")
        }
        if (is.na(slot_number) || slot_number < 1L) {
          missing_fields <- c(missing_fields, "Slot number")
        }
        if (is.na(sensor_id) && is.na(slot_note)) {
          missing_fields <- c(missing_fields, "Sensor or slot note")
        }
        if (length(missing_fields) > 0) {
          showNotification(
            paste("Required fields:", paste(missing_fields, collapse = ", ")),
            type = "error",
            duration = NULL
          )
          return()
        }

        if (is.na(sensor_id)) {
          current_sensor <- current_slot_sensor(slot_number)
          if (
            !is.null(current_sensor) &&
              !is_blank_sensor(current_sensor$sensor_id[[1]])
          ) {
            observer_match <- maintenance_data$observers[
              maintenance_data$observers$observer_id == observer_id
            ]
            observer_label <- if (nrow(observer_match) > 0) {
              observer_match$observer_label[[1]]
            } else {
              ""
            }
            maintenance_data$pending_sensor_unassign <- list(
              instrument_id = instrument_id,
              observer_id = observer_id,
              observer_label = observer_label,
              obs_datetime = obs_datetime,
              slot_number = slot_number,
              sensor_id = sensor_id,
              slot_note = slot_note,
              event_note = event_note,
              current_sensor_id = current_sensor$sensor_id[[1]]
            )
            show_sensor_unassign_modal(
              maintenance_data$pending_sensor_unassign,
              current_sensor
            )
            return()
          }
        }

        conflicts <- with_db_feedback(
          current_sensor_conflicts(sensor_id, instrument_id, slot_number)
        )
        if (is.null(conflicts)) {
          return()
        }
        if (nrow(conflicts) > 0) {
          maintenance_data$pending_sensor_move <- list(
            instrument_id = instrument_id,
            observer_id = observer_id,
            obs_datetime = obs_datetime,
            slot_number = slot_number,
            sensor_id = sensor_id,
            slot_note = slot_note,
            event_note = event_note,
            conflicts = conflicts
          )
          show_sensor_move_modal(
            maintenance_data$pending_sensor_move,
            conflicts
          )
          return()
        }

        result <- with_db_feedback(
          save_sensor_slot_event(
            instrument_id,
            observer_id,
            obs_datetime,
            slot_number,
            sensor_id,
            slot_note,
            event_note
          ),
          success = "Sensor slot event saved."
        )
        if (is.null(result)) {
          return()
        }
        updateTextAreaInput(session, "slot_note", value = "")
        updateTextAreaInput(session, "sensor_event_note", value = "")
        refresh_instrument_details(instrument_id)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$confirm_sensor_move,
      {
        pending <- maintenance_data$pending_sensor_move
        if (is.null(pending)) {
          removeModal()
          return()
        }
        conflicts <- with_db_feedback(current_sensor_conflicts(
          pending$sensor_id,
          pending$instrument_id,
          pending$slot_number
        ))
        if (is.null(conflicts)) {
          return()
        }
        if (nrow(conflicts) == 0) {
          showNotification(
            "The old sensor assignment no longer exists. Saving normally.",
            type = "message"
          )
        }

        result <- with_db_feedback(
          save_sensor_slot_event(
            pending$instrument_id,
            pending$observer_id,
            pending$obs_datetime,
            pending$slot_number,
            pending$sensor_id,
            pending$slot_note,
            pending$event_note,
            move_conflicts = conflicts
          ),
          success = "Sensor moved and slot event saved."
        )
        if (is.null(result)) {
          return()
        }

        removeModal()
        maintenance_data$pending_sensor_move <- NULL
        updateTextAreaInput(session, "slot_note", value = "")
        updateTextAreaInput(session, "sensor_event_note", value = "")
        refresh_instrument_details(pending$instrument_id)
      },
      ignoreInit = TRUE
    )

    save_pending_sensor_unassign <- function(retire = FALSE) {
      pending <- maintenance_data$pending_sensor_unassign
      if (is.null(pending)) {
        removeModal()
        return(invisible(NULL))
      }
      result <- with_db_feedback(
        save_sensor_slot_event(
          pending$instrument_id,
          pending$observer_id,
          pending$obs_datetime,
          pending$slot_number,
          pending$sensor_id,
          pending$slot_note,
          pending$event_note,
          retire_sensor_id = if (isTRUE(retire)) {
            pending$current_sensor_id
          } else {
            NA_integer_
          },
          retired_by = if (isTRUE(retire)) {
            input$retire_unassigned_sensor_by
          } else {
            NA_character_
          },
          date_retired = if (isTRUE(retire)) {
            input$retire_unassigned_sensor_date
          } else {
            as.Date(NA)
          }
        ),
        success = if (isTRUE(retire)) {
          "Sensor unassigned and retired."
        } else {
          "Sensor unassigned."
        }
      )
      if (is.null(result)) {
        return(invisible(NULL))
      }
      removeModal()
      maintenance_data$pending_sensor_unassign <- NULL
      updateTextAreaInput(session, "slot_note", value = "")
      updateTextAreaInput(session, "sensor_event_note", value = "")
      refresh_lookups(sensor_selected = character(0))
      refresh_instrument_details(pending$instrument_id)
      invisible(result)
    }

    observeEvent(
      input$confirm_sensor_unassign_only,
      {
        save_pending_sensor_unassign(retire = FALSE)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$confirm_sensor_unassign_retire,
      {
        if (is.na(date_or_na(input$retire_unassigned_sensor_date))) {
          showNotification("Retirement date is required.", type = "error")
          return()
        }
        save_pending_sensor_unassign(retire = TRUE)
      },
      ignoreInit = TRUE
    )

    output$new_sensor_type_details <- renderUI({
      if (
        !is_new_lookup_value(
          input$new_sensor_type,
          maintenance_data$sensor_types,
          "sensor_type_id"
        )
      ) {
        return(NULL)
      }
      textAreaInput(
        ns("new_sensor_type_description"),
        "New sensor type description",
        width = "100%",
        height = "70px"
      )
    })

    output$new_sensor_make_details <- renderUI({
      if (
        !is_new_lookup_value(
          input$new_sensor_make,
          maintenance_data$sensor_makes,
          "make_id"
        )
      ) {
        return(NULL)
      }
      textAreaInput(
        ns("new_sensor_make_description"),
        "New make description",
        width = "100%",
        height = "70px"
      )
    })

    output$new_sensor_model_details <- renderUI({
      if (
        !is_new_lookup_value(
          input$new_sensor_model,
          maintenance_data$sensor_models,
          "model_id"
        )
      ) {
        return(NULL)
      }
      textAreaInput(
        ns("new_sensor_model_description"),
        "New model description",
        width = "100%",
        height = "70px"
      )
    })

    observeEvent(
      input$register_sensor,
      {
        showModal(modalDialog(
          title = "Register sensor",
          textInput(
            ns("new_sensor_serial"),
            "Serial no",
            placeholder = "Required"
          ),
          selectizeInput(
            ns("new_sensor_type"),
            "Sensor type (type to add new)",
            choices = build_sensor_type_choices(maintenance_data$sensor_types),
            multiple = TRUE,
            options = list(
              maxItems = 1,
              placeholder = "Required",
              create = TRUE
            )
          ),
          uiOutput(ns("new_sensor_type_details")),
          selectizeInput(
            ns("new_sensor_make"),
            "Make (type to add new)",
            choices = build_sensor_make_choices(maintenance_data$sensor_makes),
            multiple = TRUE,
            options = list(
              maxItems = 1,
              placeholder = "Required",
              create = TRUE
            )
          ),
          uiOutput(ns("new_sensor_make_details")),
          selectizeInput(
            ns("new_sensor_model"),
            "Model (type to add new)",
            choices = build_sensor_model_choices(
              maintenance_data$sensor_models
            ),
            multiple = TRUE,
            options = list(
              maxItems = 1,
              placeholder = "Required",
              create = TRUE
            )
          ),
          uiOutput(ns("new_sensor_model_details")),
          selectizeInput(
            ns("new_sensor_owner"),
            "Owner",
            choices = build_owner_choices(maintenance_data$owners),
            multiple = TRUE,
            options = list(maxItems = 1, placeholder = "Required")
          ),
          selectizeInput(
            ns("new_sensor_supplier_id"),
            "Supplier",
            choices = build_supplier_choices(maintenance_data$suppliers),
            multiple = TRUE,
            options = list(maxItems = 1, placeholder = "Optional")
          ),
          textInput(
            ns("new_sensor_asset_tag"),
            "Asset tag",
            placeholder = "Optional"
          ),
          bslib::layout_columns(
            col_widths = c(6, 6),
            dateInput(
              ns("new_sensor_date_purchased"),
              "Date purchased",
              value = NULL
            ),
            dateInput(
              ns("new_sensor_date_in_service"),
              "Date in service",
              value = NULL
            )
          ),
          textAreaInput(ns("new_sensor_notes"), "Notes", width = "100%"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("add_sensor"), "Register sensor")
          ),
          easyClose = TRUE
        ))
      },
      ignoreInit = TRUE
    )

    resolve_sensor_type_id <- function(value) {
      id <- int_or_na(value)
      if (!is.na(id) && id %in% maintenance_data$sensor_types$sensor_type_id) {
        return(id)
      }
      sensor_type <- safe_text(value)
      existing_id <- lookup_id_from_text(
        sensor_type,
        maintenance_data$sensor_types,
        "sensor_type_id",
        "sensor_type"
      )
      if (!is.na(existing_id)) {
        return(existing_id)
      }
      DBI::dbGetQuery(
        con,
        "INSERT INTO instruments.sensor_types
         (sensor_type, sensor_type_description)
         VALUES ($1, $2)
         RETURNING sensor_type_id",
        params = list(
          sensor_type,
          blank_to_na(input$new_sensor_type_description)
        )
      )$sensor_type_id[[1]]
    }

    resolve_sensor_make_id <- function(value) {
      id <- int_or_na(value)
      if (!is.na(id) && id %in% maintenance_data$sensor_makes$make_id) {
        return(id)
      }
      make <- safe_text(value)
      existing_id <- lookup_id_from_text(
        make,
        maintenance_data$sensor_makes,
        "make_id",
        "make"
      )
      if (!is.na(existing_id)) {
        return(existing_id)
      }
      DBI::dbGetQuery(
        con,
        "INSERT INTO instruments.sensor_makes
         (make, description)
         VALUES ($1, $2)
         RETURNING make_id",
        params = list(
          make,
          blank_to_na(input$new_sensor_make_description)
        )
      )$make_id[[1]]
    }

    resolve_sensor_model_id <- function(value) {
      id <- int_or_na(value)
      if (!is.na(id) && id %in% maintenance_data$sensor_models$model_id) {
        return(id)
      }
      model <- safe_text(value)
      existing_id <- lookup_id_from_text(
        model,
        maintenance_data$sensor_models,
        "model_id",
        "model"
      )
      if (!is.na(existing_id)) {
        return(existing_id)
      }
      DBI::dbGetQuery(
        con,
        "INSERT INTO instruments.sensor_models
         (model, description)
         VALUES ($1, $2)
         RETURNING model_id",
        params = list(
          model,
          blank_to_na(input$new_sensor_model_description)
        )
      )$model_id[[1]]
    }

    observeEvent(
      input$add_sensor,
      {
        missing_fields <- c()
        if (is.na(blank_to_na(input$new_sensor_type))) {
          missing_fields <- c(missing_fields, "Sensor type")
        }
        if (!nzchar(safe_text(input$new_sensor_serial))) {
          missing_fields <- c(missing_fields, "Serial no")
        }
        if (!nzchar(safe_text(input$new_sensor_make))) {
          missing_fields <- c(missing_fields, "Make")
        }
        if (!nzchar(safe_text(input$new_sensor_model))) {
          missing_fields <- c(missing_fields, "Model")
        }
        if (is.na(int_or_na(input$new_sensor_owner))) {
          missing_fields <- c(missing_fields, "Owner")
        }
        if (length(missing_fields) > 0) {
          showNotification(
            paste("Required fields:", paste(missing_fields, collapse = ", ")),
            type = "error",
            duration = NULL
          )
          return()
        }

        new_id <- with_db_feedback(
          {
            DBI::dbBegin(con)
            tryCatch(
              {
                sensor_type_id <- resolve_sensor_type_id(input$new_sensor_type)
                sensor_make_id <- resolve_sensor_make_id(input$new_sensor_make)
                sensor_model_id <- resolve_sensor_model_id(
                  input$new_sensor_model
                )
                owner_id <- int_or_na(input$new_sensor_owner)
                supplier_id <- int_or_na(input$new_sensor_supplier_id)
                sensor_id <- DBI::dbGetQuery(
                  con,
                  "INSERT INTO instruments.sensors
               (sensor_type, sensor_serial, sensor_make, sensor_model,
                owner, supplier_id, sensor_asset_tag, date_purchased,
                date_in_service, sensor_notes)
               VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)
               RETURNING sensor_id",
                  params = list(
                    sensor_type_id,
                    safe_text(input$new_sensor_serial),
                    sensor_make_id,
                    sensor_model_id,
                    owner_id,
                    supplier_id,
                    blank_to_na(input$new_sensor_asset_tag),
                    date_or_na(input$new_sensor_date_purchased),
                    date_or_na(input$new_sensor_date_in_service),
                    blank_to_na(input$new_sensor_notes)
                  )
                )$sensor_id[[1]]
                DBI::dbCommit(con)
                sensor_id
              },
              error = function(e) {
                DBI::dbRollback(con)
                stop(e)
              }
            )
          },
          success = "Sensor registered."
        )
        if (is.null(new_id)) {
          return()
        }
        refresh_lookups(sensor_selected = as.character(new_id))
        removeModal()
      },
      ignoreInit = TRUE
    )
  })
}
