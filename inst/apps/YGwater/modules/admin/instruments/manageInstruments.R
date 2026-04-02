# UI and server code for creating or modifying instrument records

manageInstrumentsUI <- function(id) {
  ns <- NS(id)

  bslib::page_sidebar(
    title = NULL,
    sidebar = bslib::sidebar(
      width = 440,
      title = "Create / modify instruments",
      selectizeInput(
        ns("record_id"),
        "Instrument record",
        choices = NULL,
        multiple = TRUE,
        options = list(
          maxItems = 1,
          placeholder = "Search for an existing instrument"
        )
      ),
      div(
        class = "d-flex gap-2 flex-wrap mb-3",
        actionButton(ns("save"), "Save instrument", class = "btn-primary"),
        actionButton(ns("clear"), "New record"),
        actionButton(ns("reload"), "Reload")
      ),
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
          "Observation date/time",
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
        )
      ),
      tags$hr(),
      tags$h5("Identity"),
      bslib::layout_columns(
        col_widths = c(6, 6),
        textInput(ns("serial_no"), "Serial no"),
        textInput(ns("asset_tag"), "Asset tag"),
        selectizeInput(
          ns("make"),
          "Make (type to add new)",
          choices = NULL,
          multiple = TRUE,
          options = list(maxItems = 1, placeholder = "Required", create = TRUE)
        ),
        selectizeInput(
          ns("model"),
          "Model (type to add new)",
          choices = NULL,
          multiple = TRUE,
          options = list(maxItems = 1, placeholder = "Required", create = TRUE)
        ),
        selectizeInput(
          ns("type"),
          "Type (type to add new)",
          choices = NULL,
          multiple = TRUE,
          options = list(maxItems = 1, placeholder = "Required", create = TRUE)
        ),
        selectizeInput(
          ns("owner"),
          "Owner (type to add new)",
          choices = NULL,
          multiple = TRUE,
          options = list(maxItems = 1, placeholder = "Required", create = TRUE)
        ),
        selectizeInput(
          ns("supplier_id"),
          "Supplier (type to add new)",
          choices = NULL,
          multiple = TRUE,
          options = list(maxItems = 1, placeholder = "Optional", create = TRUE)
        ),
        checkboxInput(
          ns("holds_replaceable_sensors"),
          "Holds replaceable sensors",
          value = FALSE
        ),
        checkboxInput(
          ns("takes_measurements"),
          "Takes measurements",
          value = FALSE
        ),
        checkboxInput(
          ns("can_be_logger"),
          "Can be logger",
          value = FALSE
        ),
        checkboxInput(
          ns("can_be_telemetry_component"),
          "Can be telemetry component",
          value = FALSE
        )
      ),
      tags$hr(),
      tags$h5("Lifecycle"),
      bslib::layout_columns(
        col_widths = c(6, 6),
        dateInput(ns("date_in_service"), "Date in service", value = NULL),
        dateInput(ns("date_purchased"), "Date purchased", value = NULL),
        numericInput(
          ns("purchase_price"),
          "Purchase price dollars",
          value = NA_real_,
          min = 0,
          step = 0.01
        ),
        textInput(ns("retired_by"), "Retired by"),
        dateInput(ns("date_retired"), "Date retired", value = NULL),
        dateInput(
          ns("date_end_of_life"),
          "Expected end of life",
          value = NULL
        )
      ),
      tags$hr(),
      tags$h5("Maintenance due"),
      dateInput(
        ns("date_maintenance_due"),
        "Next maintenance due",
        value = NULL
      ),
      textAreaInput(
        ns("maintenance_due_note"),
        "What maintenance is due?",
        value = "",
        width = "100%",
        height = "110px"
      ),
      tags$hr(),
      tags$h5("Technical details"),
      bslib::layout_columns(
        col_widths = c(6, 6),
        numericInput(
          ns("cable_length_m"),
          "Cable length (m)",
          value = NA_real_,
          min = 0,
          step = 0.1
        ),
        textInput(ns("firmware_version"), "Firmware version"),
        numericInput(
          ns("voltage"),
          "Input voltage",
          value = NA_real_,
          step = 0.01
        ),
        numericInput(
          ns("power_active_ma"),
          "Active power (mA)",
          value = NA_real_,
          step = 0.01
        ),
        numericInput(
          ns("power_quiescent_ma"),
          "Quiescent power (mA)",
          value = NA_real_,
          step = 0.01
        )
      )
    ),
    uiOutput(ns("banner")),
    tags$div(
      class = "mb-3",
      tags$h4("Existing instrument records"),
      tags$p(class = "text-muted", "Select a row to load it into the form.")
    ),
    DT::DTOutput(ns("records_table"))
  )
}

manageInstruments <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- session$userData$AquaCache
    table_proxy <- DT::dataTableProxy("records_table", session = session)

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = con,
        module_id = "manageInstruments"
      )
    })

    instrument_data <- reactiveValues(
      records = NULL,
      observers = NULL,
      makes = NULL,
      models = NULL,
      types = NULL,
      owners = NULL,
      suppliers = NULL
    )

    `%||%` <- function(x, y) {
      if (is.null(x) || !length(x) || all(is.na(x))) y else x
    }

    default_obs_datetime <- function() {
      out <- as.POSIXct(Sys.time(), tz = "UTC")
      attr(out, "tzone") <- "UTC"
      out
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
      as.integer(value)
    }

    num_or_na <- function(value) {
      if (is.null(value) || !length(value) || all(is.na(value))) {
        return(NA_real_)
      }
      as.numeric(value[[1]])
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

    build_choices <- function(df, id_col, label_col) {
      ids <- if (nrow(df) > 0) as.character(df[[id_col]]) else character()
      labels <- if (nrow(df) > 0) {
        label_piece(df[[label_col]], "(blank)")
      } else {
        character()
      }
      stats::setNames(ids, labels)
    }

    has_lookup_id <- function(value, df, id_col) {
      value <- blank_to_na(value)
      if (is.na(value)) {
        return(FALSE)
      }
      value %in% as.character(df[[id_col]])
    }

    lookup_id_from_label <- function(value, df, id_col, label_col) {
      match_lookup_id_by_label(value, df[[id_col]], df[[label_col]])
    }

    build_record_choices <- function(df) {
      if (is.null(df) || nrow(df) == 0) {
        return(character())
      }
      make_model <- trimws(paste(
        label_piece(df$make_name, ""),
        label_piece(df$model_name, "")
      ))
      make_model[!nzchar(make_model)] <- "Not specified"
      labels <- paste0(
        "[",
        df$instrument_id,
        "] ",
        label_piece(df$serial_no, "No serial"),
        " | ",
        make_model,
        " | ",
        label_piece(df$type_name),
        " | ",
        label_piece(df$owner_name)
      )
      stats::setNames(
        as.character(df$instrument_id),
        labels
      )
    }

    truncate_with_tooltip_js <- function(visible_chars = 24) {
      htmlwidgets::JS(sprintf(
        paste(
          "function(data, type, row, meta) {",
          "  if (type !== 'display') { return data; }",
          "  if (data === null || data === undefined) { return ''; }",
          "  var text = String(data);",
          "  var esc = $('<div/>').text(text).html();",
          "  var short = text.length > %d ? text.substring(0, %d) + '...' : text;",
          "  return '<span title=\"' + esc + '\">' + $('<div/>').text(short).html() + '</span>';",
          "}"
        ),
        visible_chars,
        visible_chars
      ))
    }

    current_lookup_selection <- function() {
      list(
        observer = normalize_select_value(isolate(input$observer)),
        make = normalize_select_value(isolate(input$make)),
        model = normalize_select_value(isolate(input$model)),
        type = normalize_select_value(isolate(input$type)),
        owner = normalize_select_value(isolate(input$owner)),
        supplier_id = normalize_select_value(isolate(input$supplier_id))
      )
    }

    refresh_lookups <- function(selected = current_lookup_selection()) {
      instrument_data$observers <- DBI::dbGetQuery(
        con,
        paste(
          "SELECT observer_id,",
          "CONCAT(observer_first, ' ', observer_last, ' (', organization, ')') AS observer_label",
          "FROM instruments.observers",
          "ORDER BY observer_first, observer_last, organization"
        )
      )
      instrument_data$makes <- DBI::dbGetQuery(
        con,
        "SELECT make_id, make FROM instruments.instrument_make ORDER BY make"
      )
      instrument_data$models <- DBI::dbGetQuery(
        con,
        "SELECT model_id, model FROM instruments.instrument_model ORDER BY model"
      )
      instrument_data$types <- DBI::dbGetQuery(
        con,
        "SELECT type_id, type FROM instruments.instrument_type ORDER BY type"
      )
      instrument_data$owners <- DBI::dbGetQuery(
        con,
        "SELECT organization_id, name FROM public.organizations ORDER BY name"
      )
      instrument_data$suppliers <- DBI::dbGetQuery(
        con,
        paste(
          "SELECT supplier_id, supplier_name",
          "FROM instruments.suppliers",
          "ORDER BY supplier_name"
        )
      )

      updateSelectizeInput(
        session,
        "observer",
        choices = build_choices(
          instrument_data$observers,
          "observer_id",
          "observer_label"
        ),
        selected = normalize_select_value(selected$observer),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "make",
        choices = build_choices(instrument_data$makes, "make_id", "make"),
        selected = normalize_select_value(selected$make),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "model",
        choices = build_choices(instrument_data$models, "model_id", "model"),
        selected = normalize_select_value(selected$model),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "type",
        choices = build_choices(instrument_data$types, "type_id", "type"),
        selected = normalize_select_value(selected$type),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "owner",
        choices = build_choices(
          instrument_data$owners,
          "organization_id",
          "name"
        ),
        selected = normalize_select_value(selected$owner),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "supplier_id",
        choices = build_choices(
          instrument_data$suppliers,
          "supplier_id",
          "supplier_name"
        ),
        selected = normalize_select_value(selected$supplier_id),
        server = TRUE
      )
    }

    refresh_records <- function(selected = isolate(input$record_id)) {
      instrument_data$records <- DBI::dbGetQuery(
        con,
        paste(
          "SELECT i.instrument_id, i.obs_datetime, i.observer,",
          "CONCAT(o.observer_first, ' ', o.observer_last, ' (', o.organization, ')') AS observer_name,",
          "i.make, mk.make AS make_name,",
          "i.model, mdl.model AS model_name,",
          "i.type, tp.type AS type_name,",
          "i.holds_replaceable_sensors, i.serial_no, i.asset_tag,",
          "i.date_in_service, i.date_purchased, i.retired_by, i.date_retired,",
          "i.owner, org.name AS owner_name, i.date_end_of_life,",
          "i.created_by, i.modified_by, i.created, i.modified,",
          "i.supplier_id, s.supplier_name, i.purchase_price,",
          "due.maintenance_event_id, due.date_maintenance_due, due.maintenance_due_note,",
          "i.takes_measurements, i.cable_length_m, i.firmware_version,",
          "i.voltage, i.power_active_ma, i.power_quiescent_ma,",
          "i.can_be_logger, i.can_be_telemetry_component",
          "FROM instruments.instruments AS i",
          "LEFT JOIN instruments.observers AS o ON i.observer = o.observer_id",
          "LEFT JOIN instruments.instrument_make AS mk ON i.make = mk.make_id",
          "LEFT JOIN instruments.instrument_model AS mdl ON i.model = mdl.model_id",
          "LEFT JOIN instruments.instrument_type AS tp ON i.type = tp.type_id",
          "LEFT JOIN public.organizations AS org ON i.owner = org.organization_id",
          "LEFT JOIN instruments.suppliers AS s ON i.supplier_id = s.supplier_id",
          "LEFT JOIN (",
          "  SELECT DISTINCT ON (instrument_id)",
          "    event_id AS maintenance_event_id,",
          "    instrument_id,",
          "    date_maintenance_due,",
          "    note AS maintenance_due_note",
          "  FROM instruments.instrument_maintenance",
          "  WHERE date_maintenance_due IS NOT NULL",
          "  ORDER BY instrument_id, obs_datetime DESC, event_id DESC",
          ") AS due ON i.instrument_id = due.instrument_id",
          "ORDER BY i.date_retired NULLS FIRST, i.serial_no, i.instrument_id"
        )
      )

      selected <- normalize_select_value(selected)
      valid_choices <- as.character(instrument_data$records$instrument_id)
      if (!length(selected) || !(selected %in% valid_choices)) {
        selected <- character(0)
      }

      updateSelectizeInput(
        session,
        "record_id",
        choices = build_record_choices(instrument_data$records),
        selected = selected,
        server = TRUE
      )
    }

    refresh_all <- function(
      record_selected = isolate(input$record_id),
      lookup_selected = current_lookup_selection()
    ) {
      refresh_lookups(selected = lookup_selected)
      refresh_records(selected = record_selected)
    }

    normalize_record_id <- function(record_id = input$record_id) {
      record_id <- blank_to_na(record_id)
      if (is.na(record_id)) {
        return(NA_integer_)
      }
      suppressWarnings(as.integer(record_id))
    }

    selected_record <- function(record_id = input$record_id) {
      record_id <- normalize_record_id(record_id)
      if (is.na(record_id)) {
        return(NULL)
      }
      record <- instrument_data$records[
        instrument_data$records$instrument_id == record_id,
        ,
        drop = FALSE
      ]
      if (nrow(record) == 0) {
        return(NULL)
      }
      record[1, , drop = FALSE]
    }

    clear_form <- function(reset_record = TRUE, clear_table = TRUE) {
      if (reset_record) {
        updateSelectizeInput(session, "record_id", selected = character(0))
      }
      if (clear_table) {
        DT::selectRows(table_proxy, NULL)
      }

      updateSelectizeInput(session, "observer", selected = character(0))
      updateSelectizeInput(session, "make", selected = character(0))
      updateSelectizeInput(session, "model", selected = character(0))
      updateSelectizeInput(session, "type", selected = character(0))
      updateSelectizeInput(session, "owner", selected = character(0))
      updateSelectizeInput(session, "supplier_id", selected = character(0))
      updateSelectizeInput(
        session,
        "timezone",
        selected = default_input_timezone()
      )
      shinyWidgets::updateAirDateInput(
        session,
        "obs_datetime",
        value = default_obs_datetime(),
        tz = air_datetime_widget_timezone(default_input_timezone())
      )
      updateTextInput(session, "serial_no", value = "")
      updateTextInput(session, "asset_tag", value = "")
      updateTextInput(session, "retired_by", value = "")
      updateDateInput(session, "date_in_service", value = NULL)
      updateDateInput(session, "date_purchased", value = NULL)
      updateDateInput(session, "date_retired", value = NULL)
      updateDateInput(session, "date_end_of_life", value = NULL)
      updateDateInput(session, "date_maintenance_due", value = NULL)
      updateTextAreaInput(session, "maintenance_due_note", value = "")
      updateNumericInput(session, "purchase_price", value = NA_real_)
      updateNumericInput(session, "cable_length_m", value = NA_real_)
      updateTextInput(session, "firmware_version", value = "")
      updateNumericInput(session, "voltage", value = NA_real_)
      updateNumericInput(session, "power_active_ma", value = NA_real_)
      updateNumericInput(session, "power_quiescent_ma", value = NA_real_)
      updateCheckboxInput(session, "holds_replaceable_sensors", value = FALSE)
      updateCheckboxInput(session, "takes_measurements", value = FALSE)
      updateCheckboxInput(session, "can_be_logger", value = FALSE)
      updateCheckboxInput(
        session,
        "can_be_telemetry_component",
        value = FALSE
      )
    }

    populate_form <- function(record_id) {
      record <- selected_record(record_id)
      if (is.null(record)) {
        clear_form(reset_record = FALSE, clear_table = FALSE)
        return(invisible(NULL))
      }

      updateSelectizeInput(
        session,
        "observer",
        selected = normalize_select_value(record$observer)
      )
      updateSelectizeInput(
        session,
        "make",
        selected = normalize_select_value(record$make)
      )
      updateSelectizeInput(
        session,
        "model",
        selected = normalize_select_value(record$model)
      )
      updateSelectizeInput(
        session,
        "type",
        selected = normalize_select_value(record$type)
      )
      updateSelectizeInput(
        session,
        "owner",
        selected = normalize_select_value(record$owner)
      )
      updateSelectizeInput(
        session,
        "supplier_id",
        selected = normalize_select_value(record$supplier_id)
      )
      shinyWidgets::updateAirDateInput(
        session,
        "obs_datetime",
        value = coerce_utc_datetime(record$obs_datetime),
        tz = air_datetime_widget_timezone(
          input$timezone %||% default_input_timezone()
        )
      )
      updateTextInput(session, "serial_no", value = safe_text(record$serial_no))
      updateTextInput(session, "asset_tag", value = safe_text(record$asset_tag))
      updateTextInput(
        session,
        "retired_by",
        value = safe_text(record$retired_by)
      )
      updateDateInput(
        session,
        "date_in_service",
        value = record$date_in_service
      )
      updateDateInput(session, "date_purchased", value = record$date_purchased)
      updateDateInput(session, "date_retired", value = record$date_retired)
      updateDateInput(
        session,
        "date_end_of_life",
        value = record$date_end_of_life
      )
      updateDateInput(
        session,
        "date_maintenance_due",
        value = record$date_maintenance_due
      )
      updateTextAreaInput(
        session,
        "maintenance_due_note",
        value = safe_text(record$maintenance_due_note)
      )
      updateNumericInput(
        session,
        "purchase_price",
        value = record$purchase_price
      )
      updateNumericInput(
        session,
        "cable_length_m",
        value = record$cable_length_m
      )
      updateTextInput(
        session,
        "firmware_version",
        value = safe_text(record$firmware_version)
      )
      updateNumericInput(session, "voltage", value = record$voltage)
      updateNumericInput(
        session,
        "power_active_ma",
        value = record$power_active_ma
      )
      updateNumericInput(
        session,
        "power_quiescent_ma",
        value = record$power_quiescent_ma
      )
      updateCheckboxInput(
        session,
        "holds_replaceable_sensors",
        value = isTRUE(record$holds_replaceable_sensors)
      )
      updateCheckboxInput(
        session,
        "takes_measurements",
        value = isTRUE(record$takes_measurements)
      )
      updateCheckboxInput(
        session,
        "can_be_logger",
        value = isTRUE(record$can_be_logger)
      )
      updateCheckboxInput(
        session,
        "can_be_telemetry_component",
        value = isTRUE(record$can_be_telemetry_component)
      )
      invisible(NULL)
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

    output$records_table <- DT::renderDT({
      req(!is.null(instrument_data$records))

      display <- instrument_data$records[, c(
        "serial_no",
        "make_name",
        "model_name",
        "type_name",
        "owner_name",
        "supplier_name",
        "observer_name",
        "date_maintenance_due",
        "obs_datetime",
        "date_in_service",
        "date_purchased",
        "purchase_price",
        "holds_replaceable_sensors",
        "takes_measurements",
        "can_be_logger",
        "can_be_telemetry_component",
        "cable_length_m",
        "firmware_version",
        "voltage",
        "power_active_ma",
        "power_quiescent_ma",
        "asset_tag",
        "retired_by",
        "date_retired",
        "date_end_of_life",
        "created",
        "modified"
      )]

      factor_cols <- c(
        "make_name",
        "model_name",
        "type_name",
        "owner_name",
        "supplier_name",
        "observer_name"
      )
      display[factor_cols] <- lapply(display[factor_cols], factor)

      names(display) <- c(
        "Serial no",
        "Make",
        "Model",
        "Type",
        "Owner",
        "Supplier",
        "Observer",
        "Next maintenance due",
        "Observed at",
        "In service",
        "Purchased",
        "Purchase price dollars",
        "Replaceable sensors",
        "Takes measurements",
        "Can be logger",
        "Can be telemetry component",
        "Cable length (m)",
        "Firmware",
        "Input voltage",
        "Active power (mA)",
        "Quiescent power (mA)",
        "Asset tag",
        "Retired by",
        "Date retired",
        "End of life",
        "Created",
        "Modified"
      )

      DT::datatable(
        display,
        filter = "top",
        selection = "single",
        rownames = FALSE,
        escape = FALSE,
        options = list(
          scrollX = TRUE,
          autoWidth = TRUE,
          columnDefs = list(
            list(
              targets = c(1, 2, 3, 4, 5, 6),
              render = truncate_with_tooltip_js(24)
            )
          )
        )
      )
    })

    refresh_all(
      record_selected = character(0),
      lookup_selected = list(
        observer = character(0),
        make = character(0),
        model = character(0),
        type = character(0),
        owner = character(0),
        supplier_id = character(0)
      )
    )
    clear_form(reset_record = FALSE, clear_table = FALSE)

    observeEvent(
      input$timezone,
      {
        shift_air_datetime_input_timezone(
          session = session,
          input = input,
          input_id = "obs_datetime",
          tz_name = input$timezone
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$record_id,
      {
        record_id <- normalize_record_id(input$record_id)
        if (is.na(record_id)) {
          clear_form(reset_record = FALSE)
        } else {
          populate_form(record_id)
          row_id <- match(
            record_id,
            instrument_data$records$instrument_id
          )
          if (!is.na(row_id)) {
            DT::selectRows(table_proxy, row_id)
          }
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$records_table_rows_selected,
      {
        req(length(input$records_table_rows_selected) == 1)
        selected_row <- input$records_table_rows_selected[[1]]
        updateSelectizeInput(
          session,
          "record_id",
          selected = as.character(instrument_data$records$instrument_id[[
            selected_row
          ]])
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$reload,
      {
        refresh_all(
          record_selected = isolate(input$record_id),
          lookup_selected = current_lookup_selection()
        )
        record_id <- normalize_record_id(isolate(input$record_id))
        if (!is.na(record_id)) {
          populate_form(record_id)
        }
        showNotification("Instrument data reloaded.", type = "message")
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$clear,
      {
        clear_form(reset_record = TRUE)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$observer,
      {
        created_value <- blank_to_na(input$observer)
        if (
          is.na(created_value) ||
            has_lookup_id(
              created_value,
              instrument_data$observers,
              "observer_id"
            )
        ) {
          return()
        }
        existing_id <- lookup_id_from_label(
          created_value,
          instrument_data$observers,
          "observer_id",
          "observer_label"
        )
        if (length(existing_id)) {
          updateSelectizeInput(session, "observer", selected = existing_id[[1]])
          return()
        }
        updateSelectizeInput(session, "observer", selected = character(0))
        showModal(modalDialog(
          title = "Add new observer",
          textInput(ns("new_observer_first"), "First name"),
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
        existing_id <- lookup_id_from_label(
          sprintf(
            "%s %s (%s)",
            trimws(input$new_observer_first),
            trimws(input$new_observer_last),
            trimws(input$new_observer_org)
          ),
          instrument_data$observers,
          "observer_id",
          "observer_label"
        )
        if (length(existing_id)) {
          refresh_lookups(
            modifyList(
              current_lookup_selection(),
              list(observer = existing_id[[1]])
            )
          )
          removeModal()
          showNotification("Existing observer selected.", type = "message")
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
          )$observer_id[[1]]
        )
        if (is.null(new_id)) {
          return()
        }
        refresh_lookups(
          modifyList(
            current_lookup_selection(),
            list(observer = as.character(new_id))
          )
        )
        removeModal()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$make,
      {
        created_value <- blank_to_na(input$make)
        if (
          is.na(created_value) ||
            has_lookup_id(created_value, instrument_data$makes, "make_id")
        ) {
          return()
        }
        existing_id <- lookup_id_from_label(
          created_value,
          instrument_data$makes,
          "make_id",
          "make"
        )
        if (length(existing_id)) {
          updateSelectizeInput(session, "make", selected = existing_id[[1]])
          return()
        }
        updateSelectizeInput(session, "make", selected = character(0))
        showModal(modalDialog(
          title = "Add new make",
          textInput(ns("new_make"), "Make", value = created_value),
          textInput(ns("new_make_desc"), "Description"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("add_new_make"), "Add make")
          ),
          easyClose = TRUE
        ))
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$add_new_make,
      {
        if (!nzchar(trimws(input$new_make))) {
          showNotification("Make is required.", type = "error")
          return()
        }
        existing_id <- lookup_id_from_label(
          input$new_make,
          instrument_data$makes,
          "make_id",
          "make"
        )
        if (length(existing_id)) {
          refresh_lookups(
            modifyList(
              current_lookup_selection(),
              list(make = existing_id[[1]])
            )
          )
          removeModal()
          showNotification("Existing make selected.", type = "message")
          return()
        }
        new_id <- with_db_feedback(
          DBI::dbGetQuery(
            con,
            "INSERT INTO instruments.instrument_make
              (make, description)
              VALUES ($1, $2)
              RETURNING make_id",
            params = list(
              trimws(input$new_make),
              blank_to_na(input$new_make_desc)
            )
          )$make_id[[1]]
        )
        if (is.null(new_id)) {
          return()
        }
        refresh_lookups(
          modifyList(
            current_lookup_selection(),
            list(make = as.character(new_id))
          )
        )
        removeModal()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$model,
      {
        created_value <- blank_to_na(input$model)
        if (
          is.na(created_value) ||
            has_lookup_id(created_value, instrument_data$models, "model_id")
        ) {
          return()
        }
        existing_id <- lookup_id_from_label(
          created_value,
          instrument_data$models,
          "model_id",
          "model"
        )
        if (length(existing_id)) {
          updateSelectizeInput(session, "model", selected = existing_id[[1]])
          return()
        }
        updateSelectizeInput(session, "model", selected = character(0))
        showModal(modalDialog(
          title = "Add new model",
          textInput(ns("new_model"), "Model", value = created_value),
          textInput(ns("new_model_desc"), "Description"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("add_new_model"), "Add model")
          ),
          easyClose = TRUE
        ))
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$add_new_model,
      {
        if (!nzchar(trimws(input$new_model))) {
          showNotification("Model is required.", type = "error")
          return()
        }
        existing_id <- lookup_id_from_label(
          input$new_model,
          instrument_data$models,
          "model_id",
          "model"
        )
        if (length(existing_id)) {
          refresh_lookups(
            modifyList(
              current_lookup_selection(),
              list(model = existing_id[[1]])
            )
          )
          removeModal()
          showNotification("Existing model selected.", type = "message")
          return()
        }
        new_id <- with_db_feedback(
          DBI::dbGetQuery(
            con,
            "INSERT INTO instruments.instrument_model
              (model, description)
              VALUES ($1, $2)
              RETURNING model_id",
            params = list(
              trimws(input$new_model),
              blank_to_na(input$new_model_desc)
            )
          )$model_id[[1]]
        )
        if (is.null(new_id)) {
          return()
        }
        refresh_lookups(
          modifyList(
            current_lookup_selection(),
            list(model = as.character(new_id))
          )
        )
        removeModal()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$type,
      {
        created_value <- blank_to_na(input$type)
        if (
          is.na(created_value) ||
            has_lookup_id(created_value, instrument_data$types, "type_id")
        ) {
          return()
        }
        existing_id <- lookup_id_from_label(
          created_value,
          instrument_data$types,
          "type_id",
          "type"
        )
        if (length(existing_id)) {
          updateSelectizeInput(session, "type", selected = existing_id[[1]])
          return()
        }
        updateSelectizeInput(session, "type", selected = character(0))
        showModal(modalDialog(
          title = "Add new type",
          textInput(ns("new_type"), "Type", value = created_value),
          textInput(ns("new_type_desc"), "Description"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("add_new_type"), "Add type")
          ),
          easyClose = TRUE
        ))
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$add_new_type,
      {
        if (
          !nzchar(trimws(input$new_type)) ||
            !nzchar(trimws(input$new_type_desc))
        ) {
          showNotification(
            "Type and description are required.",
            type = "error"
          )
          return()
        }
        existing_id <- lookup_id_from_label(
          input$new_type,
          instrument_data$types,
          "type_id",
          "type"
        )
        if (length(existing_id)) {
          refresh_lookups(
            modifyList(
              current_lookup_selection(),
              list(type = existing_id[[1]])
            )
          )
          removeModal()
          showNotification("Existing type selected.", type = "message")
          return()
        }
        new_id <- with_db_feedback(
          DBI::dbGetQuery(
            con,
            "INSERT INTO instruments.instrument_type
              (type, description)
              VALUES ($1, $2)
              RETURNING type_id",
            params = list(
              trimws(input$new_type),
              trimws(input$new_type_desc)
            )
          )$type_id[[1]]
        )
        if (is.null(new_id)) {
          return()
        }
        refresh_lookups(
          modifyList(
            current_lookup_selection(),
            list(type = as.character(new_id))
          )
        )
        removeModal()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$owner,
      {
        created_value <- blank_to_na(input$owner)
        if (
          is.na(created_value) ||
            has_lookup_id(
              created_value,
              instrument_data$owners,
              "organization_id"
            )
        ) {
          return()
        }
        existing_id <- lookup_id_from_label(
          created_value,
          instrument_data$owners,
          "organization_id",
          "name"
        )
        if (length(existing_id)) {
          updateSelectizeInput(session, "owner", selected = existing_id[[1]])
          return()
        }
        updateSelectizeInput(session, "owner", selected = character(0))
        showModal(modalDialog(
          title = "Add new organization",
          textInput(ns("new_org_name"), "Name", value = created_value),
          textInput(ns("new_org_name_fr"), "Name (French)"),
          textInput(ns("new_org_contact_name"), "Contact name"),
          textInput(ns("new_org_contact_phone"), "Contact phone"),
          textInput(ns("new_org_contact_email"), "Contact email"),
          textAreaInput(ns("new_org_note"), "Note", width = "100%"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("add_new_org"), "Add organization")
          ),
          easyClose = TRUE
        ))
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$add_new_org,
      {
        if (
          !nzchar(trimws(input$new_org_name)) ||
            !nzchar(trimws(input$new_org_name_fr))
        ) {
          showNotification(
            "Organization name and French name are required.",
            type = "error"
          )
          return()
        }
        existing_id <- lookup_id_from_label(
          input$new_org_name,
          instrument_data$owners,
          "organization_id",
          "name"
        )
        if (length(existing_id)) {
          refresh_lookups(
            modifyList(
              current_lookup_selection(),
              list(owner = existing_id[[1]])
            )
          )
          removeModal()
          showNotification("Existing organization selected.", type = "message")
          return()
        }
        new_id <- with_db_feedback(
          DBI::dbGetQuery(
            con,
            "INSERT INTO public.organizations
              (name, name_fr, contact_name, phone, email, note)
              VALUES ($1, $2, $3, $4, $5, $6)
              RETURNING organization_id",
            params = list(
              trimws(input$new_org_name),
              trimws(input$new_org_name_fr),
              blank_to_na(input$new_org_contact_name),
              blank_to_na(input$new_org_contact_phone),
              blank_to_na(input$new_org_contact_email),
              blank_to_na(input$new_org_note)
            )
          )$organization_id[[1]]
        )
        if (is.null(new_id)) {
          return()
        }
        refresh_lookups(
          modifyList(
            current_lookup_selection(),
            list(owner = as.character(new_id))
          )
        )
        removeModal()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$supplier_id,
      {
        created_value <- blank_to_na(input$supplier_id)
        if (
          is.na(created_value) ||
            has_lookup_id(
              created_value,
              instrument_data$suppliers,
              "supplier_id"
            )
        ) {
          return()
        }
        existing_id <- lookup_id_from_label(
          created_value,
          instrument_data$suppliers,
          "supplier_id",
          "supplier_name"
        )
        if (length(existing_id)) {
          updateSelectizeInput(session, "supplier_id", selected = existing_id[[1]])
          return()
        }
        updateSelectizeInput(session, "supplier_id", selected = character(0))
        showModal(modalDialog(
          title = "Add new supplier",
          textInput(
            ns("new_supplier_name"),
            "Supplier name",
            value = created_value
          ),
          textInput(ns("new_supplier_contact_name"), "Contact name"),
          textInput(ns("new_supplier_contact_phone"), "Contact phone"),
          textInput(ns("new_supplier_contact_email"), "Contact email"),
          textAreaInput(ns("new_supplier_note"), "Note", width = "100%"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("add_new_supplier"), "Add supplier")
          ),
          easyClose = TRUE
        ))
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$add_new_supplier,
      {
        if (!nzchar(trimws(input$new_supplier_name))) {
          showNotification("Supplier name is required.", type = "error")
          return()
        }
        existing_id <- lookup_id_from_label(
          input$new_supplier_name,
          instrument_data$suppliers,
          "supplier_id",
          "supplier_name"
        )
        if (length(existing_id)) {
          refresh_lookups(
            modifyList(
              current_lookup_selection(),
              list(supplier_id = existing_id[[1]])
            )
          )
          removeModal()
          showNotification("Existing supplier selected.", type = "message")
          return()
        }
        new_id <- with_db_feedback(
          DBI::dbGetQuery(
            con,
            "INSERT INTO instruments.suppliers
            (supplier_name, contact_name, contact_phone, contact_email, note)
            VALUES ($1, $2, $3, $4, $5)
            RETURNING supplier_id",
            params = list(
              trimws(input$new_supplier_name),
              blank_to_na(input$new_supplier_contact_name),
              blank_to_na(input$new_supplier_contact_phone),
              blank_to_na(input$new_supplier_contact_email),
              blank_to_na(input$new_supplier_note)
            )
          )$supplier_id[[1]]
        )
        if (is.null(new_id)) {
          return()
        }
        refresh_lookups(
          modifyList(
            current_lookup_selection(),
            list(supplier_id = as.character(new_id))
          )
        )
        removeModal()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$save,
      {
        current_id <- normalize_record_id(input$record_id)
        obs_datetime <- scalar_utc_datetime(input$obs_datetime)
        serial_no <- safe_text(input$serial_no)
        maintenance_due_date <- date_or_na(input$date_maintenance_due)
        maintenance_due_note <- blank_to_na(input$maintenance_due_note)
        current_record <- selected_record(current_id)
        current_due_event_id <- if (
          is.null(current_record) ||
            all(is.na(current_record$maintenance_event_id))
        ) {
          NA_integer_
        } else {
          as.integer(current_record$maintenance_event_id[[1]])
        }

        missing_fields <- c()
        if (!nzchar(serial_no)) {
          missing_fields <- c(missing_fields, "Serial no")
        }
        if (is.na(int_or_na(input$observer))) {
          missing_fields <- c(missing_fields, "Observer")
        }
        if (is.na(int_or_na(input$make))) {
          missing_fields <- c(missing_fields, "Make")
        }
        if (is.na(int_or_na(input$model))) {
          missing_fields <- c(missing_fields, "Model")
        }
        if (is.na(int_or_na(input$type))) {
          missing_fields <- c(missing_fields, "Type")
        }
        if (is.na(int_or_na(input$owner))) {
          missing_fields <- c(missing_fields, "Owner")
        }
        if (!length(obs_datetime) || is.na(obs_datetime)) {
          missing_fields <- c(missing_fields, "Observation date/time")
        }
        if (length(missing_fields) > 0) {
          showNotification(
            paste("Required fields:", paste(missing_fields, collapse = ", ")),
            type = "error",
            duration = NULL
          )
          return()
        }
        if (xor(is.na(maintenance_due_date), is.na(maintenance_due_note))) {
          showNotification(
            "Next maintenance due date and due note must be filled together.",
            type = "error",
            duration = NULL
          )
          return()
        }

        duplicate_serial <- with_db_feedback(
          DBI::dbGetQuery(
            con,
            "SELECT instrument_id
              FROM instruments.instruments
              WHERE serial_no = $1
              AND ($2::integer IS NULL OR instrument_id <> $2)",
            params = list(serial_no, current_id)
          )
        )
        if (is.null(duplicate_serial)) {
          return()
        }
        if (nrow(duplicate_serial) > 0) {
          showNotification(
            "That serial number already exists on another record.",
            type = "error",
            duration = NULL
          )
          return()
        }

        params <- list(
          obs_datetime,
          int_or_na(input$observer),
          int_or_na(input$make),
          int_or_na(input$model),
          int_or_na(input$type),
          isTRUE(input$holds_replaceable_sensors),
          serial_no,
          blank_to_na(input$asset_tag),
          date_or_na(input$date_in_service),
          date_or_na(input$date_purchased),
          blank_to_na(input$retired_by),
          date_or_na(input$date_retired),
          int_or_na(input$owner),
          date_or_na(input$date_end_of_life),
          int_or_na(input$supplier_id),
          num_or_na(input$purchase_price),
          isTRUE(input$takes_measurements),
          isTRUE(input$can_be_logger),
          isTRUE(input$can_be_telemetry_component),
          num_or_na(input$cable_length_m),
          blank_to_na(input$firmware_version),
          num_or_na(input$voltage),
          num_or_na(input$power_active_ma),
          num_or_na(input$power_quiescent_ma)
        )

        saved_id <- if (is.na(current_id)) {
          with_db_feedback(
            DBI::dbGetQuery(
              con,
              "INSERT INTO instruments.instruments 
                  (obs_datetime, observer, make, model, type, holds_replaceable_sensors, serial_no, asset_tag, date_in_service, date_purchased, retired_by, date_retired, owner, date_end_of_life, supplier_id, purchase_price, takes_measurements, can_be_logger, can_be_telemetry_component, cable_length_m, firmware_version, voltage, power_active_ma, power_quiescent_ma)
                VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24)
                RETURNING instrument_id",
              params = params
            )$instrument_id[[1]],
            success = "Instrument record created."
          )
        } else {
          result <- with_db_feedback(
            DBI::dbExecute(
              con,
              "UPDATE instruments.instruments
                SET 
                  obs_datetime = $1, 
                  observer = $2, 
                  make = $3, 
                  model = $4,
                  type = $5, 
                  holds_replaceable_sensors = $6, 
                  serial_no = $7,
                  asset_tag = $8, 
                  date_in_service = $9, 
                  date_purchased = $10,
                  retired_by = $11, 
                  date_retired = $12, 
                  owner = $13,
                  date_end_of_life = $14, 
                  supplier_id = $15, 
                  purchase_price = $16,
                  takes_measurements = $17, 
                  can_be_logger = $18,
                  can_be_telemetry_component = $19,
                  cable_length_m = $20,
                  firmware_version = $21,
                  voltage = $22,
                  power_active_ma = $23,
                  power_quiescent_ma = $24
                WHERE instrument_id = $25",
              params = c(params, current_id)
            ),
            success = "Instrument record updated."
          )
          if (!is.null(result)) current_id else NULL
        }

        if (is.null(saved_id)) {
          return()
        }

        if (!is.na(maintenance_due_date) && !is.na(maintenance_due_note)) {
          if (is.na(current_due_event_id)) {
            with_db_feedback(
              DBI::dbExecute(
                con,
                "INSERT INTO instruments.instrument_maintenance
                  (instrument_id, observer, obs_datetime, note, date_maintenance_due)
                  VALUES ($1, $2, $3, $4, $5)",
                params = list(
                  saved_id,
                  int_or_na(input$observer),
                  obs_datetime,
                  maintenance_due_note,
                  maintenance_due_date
                )
              )
            )
          } else {
            with_db_feedback(
              DBI::dbExecute(
                con,
                "UPDATE instruments.instrument_maintenance
                  SET instrument_id = $1, observer = $2, obs_datetime = $3,
                  note = $4, date_maintenance_due = $5
                  WHERE event_id = $6",
                params = list(
                  saved_id,
                  int_or_na(input$observer),
                  obs_datetime,
                  maintenance_due_note,
                  maintenance_due_date,
                  current_due_event_id
                )
              )
            )
          }
        } else if (!is.na(current_due_event_id)) {
          with_db_feedback(
            DBI::dbExecute(
              con,
              "UPDATE instruments.instrument_maintenance
                SET date_maintenance_due = NULL, note = $1
                WHERE event_id = $2",
              params = list(
                safe_text(
                  current_record$maintenance_due_note,
                  "Maintenance due cleared"
                ),
                current_due_event_id
              )
            )
          )
        }

        refresh_all(
          record_selected = saved_id,
          lookup_selected = current_lookup_selection()
        )
        populate_form(saved_id)
        selected_row <- match(saved_id, instrument_data$records$instrument_id)
        if (!is.na(selected_row)) {
          DT::selectRows(table_proxy, selected_row)
        }
      },
      ignoreInit = TRUE
    )
  })
}
