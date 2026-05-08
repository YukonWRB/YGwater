# UI and server code for creating or modifying sensor records

manageSensorsUI <- function(id) {
  ns <- NS(id)

  bslib::page_sidebar(
    title = NULL,
    sidebar = bslib::sidebar(
      width = 440,
      title = "Create / modify sensors",
      selectizeInput(
        ns("record_id"),
        "Sensor record",
        choices = NULL,
        multiple = TRUE,
        options = list(
          maxItems = 1,
          placeholder = "Search for an existing sensor"
        )
      ),
      div(
        class = "d-flex gap-2 flex-wrap mb-3",
        actionButton(ns("clear"), "New record"),
        actionButton(ns("reload"), "Reload")
      ),
      tags$hr(),
      tags$h5("Identity"),
      textInput(ns("sensor_serial"), "Serial no", placeholder = "Required"),
      bslib::layout_columns(
        col_widths = c(6, 6),
        selectizeInput(
          ns("sensor_type"),
          "Type (type to add new)",
          choices = NULL,
          multiple = TRUE,
          options = list(maxItems = 1, placeholder = "Required", create = TRUE)
        ),
        selectizeInput(
          ns("sensor_make"),
          "Make (type to add new)",
          choices = NULL,
          multiple = TRUE,
          options = list(maxItems = 1, placeholder = "Required", create = TRUE)
        ),
        selectizeInput(
          ns("sensor_model"),
          "Model (type to add new)",
          choices = NULL,
          multiple = TRUE,
          options = list(maxItems = 1, placeholder = "Required", create = TRUE)
        ),
        textInput(ns("sensor_asset_tag"), "Asset tag")
      ),
      uiOutput(ns("new_sensor_type_details")),
      uiOutput(ns("new_sensor_make_details")),
      uiOutput(ns("new_sensor_model_details")),
      tags$hr(),
      tags$h5("Ownership"),
      bslib::layout_columns(
        col_widths = c(6, 6),
        selectizeInput(
          ns("owner"),
          "Owner",
          choices = NULL,
          multiple = TRUE,
          options = list(maxItems = 1, placeholder = "Required")
        ),
        selectizeInput(
          ns("supplier_id"),
          "Supplier",
          choices = NULL,
          multiple = TRUE,
          options = list(maxItems = 1, placeholder = "Optional")
        )
      ),
      tags$hr(),
      tags$h5("Lifecycle"),
      bslib::layout_columns(
        col_widths = c(6, 6),
        dateInput(ns("date_in_service"), "Date in service", value = NULL),
        dateInput(ns("date_purchased"), "Date purchased", value = NULL),
        textInput(ns("retired_by"), "Retired by"),
        dateInput(ns("date_retired"), "Date retired", value = NULL),
        dateInput(
          ns("date_maintenance_due"),
          "Maintenance due",
          value = NULL
        )
      ),
      textAreaInput(ns("sensor_notes"), "Notes", width = "100%"),
      actionButton(
        ns("save"),
        "Save sensor",
        class = "btn-primary",
        width = "100%"
      )
    ),
    uiOutput(ns("banner")),
    tags$div(
      class = "mb-3",
      tags$h4("Existing sensor records"),
      tags$p(class = "text-muted", "Select a row to load it into the form.")
    ),
    DT::DTOutput(ns("records_table"))
  )
}

manageSensors <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- session$userData$AquaCache
    table_proxy <- DT::dataTableProxy("records_table", session = session)
    sensor_instrument_notice <- paste(
      "<strong>What's the difference between a sensor and an instrument?</strong>",
      "A sensor <strong>must</strong> be connected to an instrument to take measurements;",
      "it is not usable on its own.",
      "Instruments may or may not use sensors and are stand-alone pieces of",
      "equipment capable of displaying and/or recording measurements."
    )

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = con,
        module_id = "manageSensors",
        text = sensor_instrument_notice
      )
    })

    sensor_data <- reactiveValues(
      records = NULL,
      types = NULL,
      makes = NULL,
      models = NULL,
      owners = NULL,
      suppliers = NULL
    )

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

    build_choices <- function(df, id_col, label_col) {
      if (is.null(df) || nrow(df) == 0) {
        return(character())
      }
      stats::setNames(as.character(df[[id_col]]), df[[label_col]])
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

    build_record_choices <- function(df) {
      if (is.null(df) || nrow(df) == 0) {
        return(character())
      }
      labels <- paste0(
        "[",
        df$sensor_id,
        "] ",
        label_piece(df$sensor_serial, "No serial"),
        " | ",
        label_piece(df$sensor_type_label),
        " | ",
        trimws(paste(label_piece(df$sensor_make, ""), label_piece(df$sensor_model, ""))),
        " | ",
        label_piece(df$owner_name)
      )
      stats::setNames(as.character(df$sensor_id), labels)
    }

    current_lookup_selection <- function() {
      list(
        sensor_type = normalize_select_value(isolate(input$sensor_type)),
        sensor_make = normalize_select_value(isolate(input$sensor_make)),
        sensor_model = normalize_select_value(isolate(input$sensor_model)),
        owner = normalize_select_value(isolate(input$owner)),
        supplier_id = normalize_select_value(isolate(input$supplier_id))
      )
    }

    refresh_lookups <- function(selected = current_lookup_selection()) {
      sensor_data$types <- data.table::as.data.table(DBI::dbGetQuery(
        con,
        "SELECT sensor_type_id, sensor_type, sensor_type_description
         FROM instruments.sensor_types
         ORDER BY sensor_type"
      ))
      sensor_data$makes <- data.table::as.data.table(DBI::dbGetQuery(
        con,
        "SELECT make_id, make, description
         FROM instruments.sensor_makes
         ORDER BY make"
      ))
      sensor_data$models <- data.table::as.data.table(DBI::dbGetQuery(
        con,
        "SELECT model_id, model, description
         FROM instruments.sensor_models
         ORDER BY model"
      ))
      sensor_data$owners <- data.table::as.data.table(DBI::dbGetQuery(
        con,
        "SELECT organization_id, name
         FROM public.organizations
         ORDER BY name"
      ))
      sensor_data$suppliers <- data.table::as.data.table(DBI::dbGetQuery(
        con,
        "SELECT supplier_id, supplier_name
         FROM instruments.suppliers
         ORDER BY supplier_name"
      ))

      updateSelectizeInput(
        session,
        "sensor_type",
        choices = build_choices(sensor_data$types, "sensor_type_id", "sensor_type"),
        selected = normalize_select_value(selected$sensor_type),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "sensor_make",
        choices = build_choices(sensor_data$makes, "make_id", "make"),
        selected = normalize_select_value(selected$sensor_make),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "sensor_model",
        choices = build_choices(sensor_data$models, "model_id", "model"),
        selected = normalize_select_value(selected$sensor_model),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "owner",
        choices = build_choices(sensor_data$owners, "organization_id", "name"),
        selected = normalize_select_value(selected$owner),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "supplier_id",
        choices = build_choices(sensor_data$suppliers, "supplier_id", "supplier_name"),
        selected = normalize_select_value(selected$supplier_id),
        server = TRUE
      )
    }

    refresh_records <- function(selected = isolate(input$record_id)) {
      sensor_data$records <- data.table::as.data.table(DBI::dbGetQuery(
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
                s.date_in_service,
                s.date_purchased,
                s.retired_by,
                s.date_retired,
                s.sensor_asset_tag,
                s.sensor_notes,
                s.date_maintenance_due,
                s.created_by,
                s.modified_by,
                s.created,
                s.modified
         FROM instruments.sensors AS s
         LEFT JOIN instruments.sensor_types AS st ON s.sensor_type = st.sensor_type_id
         LEFT JOIN instruments.sensor_makes AS sm ON s.sensor_make = sm.make_id
         LEFT JOIN instruments.sensor_models AS smdl ON s.sensor_model = smdl.model_id
         LEFT JOIN public.organizations AS org ON s.owner = org.organization_id
         LEFT JOIN instruments.suppliers AS sup ON s.supplier_id = sup.supplier_id
         ORDER BY s.date_retired NULLS FIRST, st.sensor_type, s.sensor_serial, s.sensor_id"
      ))

      selected <- normalize_select_value(selected)
      valid_choices <- as.character(sensor_data$records$sensor_id)
      if (!length(selected) || !(selected %in% valid_choices)) {
        selected <- character(0)
      }
      updateSelectizeInput(
        session,
        "record_id",
        choices = build_record_choices(sensor_data$records),
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
      record <- sensor_data$records[sensor_data$records$sensor_id == record_id]
      if (nrow(record) == 0) {
        return(NULL)
      }
      record[1]
    }

    clear_form <- function(reset_record = TRUE, clear_table = TRUE) {
      if (reset_record) {
        updateSelectizeInput(session, "record_id", selected = character(0))
      }
      if (clear_table) {
        DT::selectRows(table_proxy, NULL)
      }
      updateTextInput(session, "sensor_serial", value = "")
      updateSelectizeInput(session, "sensor_type", selected = character(0))
      updateSelectizeInput(session, "sensor_make", selected = character(0))
      updateSelectizeInput(session, "sensor_model", selected = character(0))
      updateSelectizeInput(session, "owner", selected = character(0))
      updateSelectizeInput(session, "supplier_id", selected = character(0))
      updateTextInput(session, "sensor_asset_tag", value = "")
      updateDateInput(session, "date_in_service", value = NULL)
      updateDateInput(session, "date_purchased", value = NULL)
      updateTextInput(session, "retired_by", value = "")
      updateDateInput(session, "date_retired", value = NULL)
      updateDateInput(session, "date_maintenance_due", value = NULL)
      updateTextAreaInput(session, "sensor_notes", value = "")
    }

    populate_form <- function(record_id) {
      record <- selected_record(record_id)
      if (is.null(record)) {
        clear_form(reset_record = FALSE, clear_table = FALSE)
        return(invisible(NULL))
      }
      updateTextInput(session, "sensor_serial", value = safe_text(record$sensor_serial))
      updateSelectizeInput(session, "sensor_type", selected = normalize_select_value(record$sensor_type))
      updateSelectizeInput(session, "sensor_make", selected = normalize_select_value(record$sensor_make_id))
      updateSelectizeInput(session, "sensor_model", selected = normalize_select_value(record$sensor_model_id))
      updateSelectizeInput(session, "owner", selected = normalize_select_value(record$owner))
      updateSelectizeInput(session, "supplier_id", selected = normalize_select_value(record$supplier_id))
      updateTextInput(session, "sensor_asset_tag", value = safe_text(record$sensor_asset_tag))
      updateDateInput(session, "date_in_service", value = record$date_in_service)
      updateDateInput(session, "date_purchased", value = record$date_purchased)
      updateTextInput(session, "retired_by", value = safe_text(record$retired_by))
      updateDateInput(session, "date_retired", value = record$date_retired)
      updateDateInput(session, "date_maintenance_due", value = record$date_maintenance_due)
      updateTextAreaInput(session, "sensor_notes", value = safe_text(record$sensor_notes))
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

    resolve_sensor_type_id <- function(value) {
      id <- int_or_na(value)
      if (!is.na(id) && id %in% sensor_data$types$sensor_type_id) {
        return(id)
      }
      sensor_type <- safe_text(value)
      existing_id <- lookup_id_from_text(
        sensor_type,
        sensor_data$types,
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
        params = list(sensor_type, blank_to_na(input$new_sensor_type_description))
      )$sensor_type_id[[1]]
    }

    resolve_sensor_make_id <- function(value) {
      id <- int_or_na(value)
      if (!is.na(id) && id %in% sensor_data$makes$make_id) {
        return(id)
      }
      make <- safe_text(value)
      existing_id <- lookup_id_from_text(make, sensor_data$makes, "make_id", "make")
      if (!is.na(existing_id)) {
        return(existing_id)
      }
      DBI::dbGetQuery(
        con,
        "INSERT INTO instruments.sensor_makes
         (make, description)
         VALUES ($1, $2)
         RETURNING make_id",
        params = list(make, blank_to_na(input$new_sensor_make_description))
      )$make_id[[1]]
    }

    resolve_sensor_model_id <- function(value) {
      id <- int_or_na(value)
      if (!is.na(id) && id %in% sensor_data$models$model_id) {
        return(id)
      }
      model <- safe_text(value)
      existing_id <- lookup_id_from_text(model, sensor_data$models, "model_id", "model")
      if (!is.na(existing_id)) {
        return(existing_id)
      }
      DBI::dbGetQuery(
        con,
        "INSERT INTO instruments.sensor_models
         (model, description)
         VALUES ($1, $2)
         RETURNING model_id",
        params = list(model, blank_to_na(input$new_sensor_model_description))
      )$model_id[[1]]
    }

    output$new_sensor_type_details <- renderUI({
      if (!is_new_lookup_value(input$sensor_type, sensor_data$types, "sensor_type_id")) {
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
      if (!is_new_lookup_value(input$sensor_make, sensor_data$makes, "make_id")) {
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
      if (!is_new_lookup_value(input$sensor_model, sensor_data$models, "model_id")) {
        return(NULL)
      }
      textAreaInput(
        ns("new_sensor_model_description"),
        "New model description",
        width = "100%",
        height = "70px"
      )
    })

    output$records_table <- DT::renderDT({
      req(!is.null(sensor_data$records))
      display <- sensor_data$records[, list(
        `Serial no` = sensor_serial,
        Type = sensor_type_label,
        Make = sensor_make,
        Model = sensor_model,
        Owner = owner_name,
        Supplier = supplier_name,
        `Asset tag` = sensor_asset_tag,
        `In service` = date_in_service,
        Purchased = date_purchased,
        `Retired by` = retired_by,
        `Date retired` = date_retired,
        `Maintenance due` = date_maintenance_due,
        Notes = sensor_notes,
        Created = created,
        Modified = modified
      )]
      factor_cols <- c("Type", "Make", "Model", "Owner", "Supplier")
      display[, (factor_cols) := lapply(.SD, factor), .SDcols = factor_cols]
      DT::datatable(
        display,
        filter = "top",
        selection = "single",
        rownames = FALSE,
        options = list(scrollX = TRUE, autoWidth = TRUE)
      )
    })

    refresh_all(record_selected = character(0))
    clear_form(reset_record = FALSE, clear_table = FALSE)

    observeEvent(
      input$record_id,
      {
        record_id <- normalize_record_id(input$record_id)
        if (is.na(record_id)) {
          clear_form(reset_record = FALSE)
        } else {
          populate_form(record_id)
          row_id <- match(record_id, sensor_data$records$sensor_id)
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
          selected = as.character(sensor_data$records$sensor_id[[selected_row]])
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
        showNotification("Sensor data reloaded.", type = "message")
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
      input$save,
      {
        record_id <- normalize_record_id()
        missing_fields <- c()
        if (!nzchar(safe_text(input$sensor_serial))) {
          missing_fields <- c(missing_fields, "Serial no")
        }
        if (is.na(blank_to_na(input$sensor_type))) {
          missing_fields <- c(missing_fields, "Type")
        }
        if (is.na(blank_to_na(input$sensor_make))) {
          missing_fields <- c(missing_fields, "Make")
        }
        if (is.na(blank_to_na(input$sensor_model))) {
          missing_fields <- c(missing_fields, "Model")
        }
        if (is.na(int_or_na(input$owner))) {
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

        saved_id <- with_db_feedback(
          {
            DBI::dbBegin(con)
            tryCatch(
              {
                sensor_type_id <- resolve_sensor_type_id(input$sensor_type)
                sensor_make_id <- resolve_sensor_make_id(input$sensor_make)
                sensor_model_id <- resolve_sensor_model_id(input$sensor_model)
                params <- list(
                  sensor_type_id,
                  safe_text(input$sensor_serial),
                  sensor_make_id,
                  sensor_model_id,
                  int_or_na(input$owner),
                  int_or_na(input$supplier_id),
                  blank_to_na(input$sensor_asset_tag),
                  date_or_na(input$date_in_service),
                  date_or_na(input$date_purchased),
                  blank_to_na(input$retired_by),
                  date_or_na(input$date_retired),
                  date_or_na(input$date_maintenance_due),
                  blank_to_na(input$sensor_notes)
                )
                if (is.na(record_id)) {
                  out <- DBI::dbGetQuery(
                    con,
                    "INSERT INTO instruments.sensors
                     (sensor_type, sensor_serial, sensor_make, sensor_model,
                      owner, supplier_id, sensor_asset_tag, date_in_service,
                      date_purchased, retired_by, date_retired,
                      date_maintenance_due, sensor_notes)
                     VALUES
                     ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)
                     RETURNING sensor_id",
                    params = params
                  )$sensor_id[[1]]
                } else {
                  DBI::dbExecute(
                    con,
                    "UPDATE instruments.sensors
                     SET sensor_type = $1,
                         sensor_serial = $2,
                         sensor_make = $3,
                         sensor_model = $4,
                         owner = $5,
                         supplier_id = $6,
                         sensor_asset_tag = $7,
                         date_in_service = $8,
                         date_purchased = $9,
                         retired_by = $10,
                         date_retired = $11,
                         date_maintenance_due = $12,
                         sensor_notes = $13
                     WHERE sensor_id = $14",
                    params = c(params, list(record_id))
                  )
                  out <- record_id
                }
                DBI::dbCommit(con)
                out
              },
              error = function(e) {
                DBI::dbRollback(con)
                stop(e)
              }
            )
          },
          success = "Sensor saved."
        )
        if (is.null(saved_id)) {
          return()
        }
        refresh_all(
          record_selected = as.character(saved_id),
          lookup_selected = current_lookup_selection()
        )
        populate_form(saved_id)
      },
      ignoreInit = TRUE
    )
  })
}
