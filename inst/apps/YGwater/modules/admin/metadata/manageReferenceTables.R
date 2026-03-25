reference_default <- function(x, y) {
  if (is.null(x) || !length(x) || all(is.na(x))) y else x
}

reference_form_input_id <- function(field_name) {
  paste0("field_", field_name)
}

reference_text_field <- function(name, label, required = FALSE) {
  list(
    name = name,
    label = label,
    input = "text",
    required = required
  )
}

reference_textarea_field <- function(
  name,
  label,
  required = FALSE,
  rows = 4
) {
  list(
    name = name,
    label = label,
    input = "textarea",
    required = required,
    rows = rows
  )
}

reference_select_field <- function(
  name,
  label,
  choices_query = NULL,
  choices = NULL,
  required = FALSE,
  storage = "integer"
) {
  list(
    name = name,
    label = label,
    input = "select",
    required = required,
    storage = storage,
    choices_query = choices_query,
    choices = choices
  )
}

reference_numeric_field <- function(
  name,
  label,
  required = FALSE,
  step = "any"
) {
  list(
    name = name,
    label = label,
    input = "numeric",
    required = required,
    step = step
  )
}

reference_checkbox_field <- function(name, label) {
  list(
    name = name,
    label = label,
    input = "checkbox",
    required = FALSE
  )
}

reference_table_configs <- function() {
  audit_cols <- c("created", "created_by", "modified", "modified_by")

  list(
    organizations = list(
      key = "organizations",
      title = "Organizations",
      description = paste(
        "Create and update organizations used across locations,",
        "timeseries ownership, samples, documents, images, and instruments."
      ),
      schema = "public",
      table = "organizations",
      pk = "organization_id",
      id_label = "Organization ID",
      order_by = "name",
      audit_columns = audit_cols,
      columns = list(
        reference_text_field("name", "Name", required = TRUE),
        reference_text_field("name_fr", "Name (French)"),
        reference_text_field("contact_name", "Contact name"),
        reference_text_field("phone", "Phone"),
        reference_text_field("email", "Email"),
        reference_textarea_field("note", "Note")
      )
    ),
    networks = list(
      key = "networks",
      title = "Networks",
      description = paste(
        "Manage the named monitoring networks used for location grouping",
        "and filtering throughout the app."
      ),
      schema = "public",
      table = "networks",
      pk = "network_id",
      id_label = "Network ID",
      order_by = "name",
      audit_columns = audit_cols,
      columns = list(
        reference_text_field("name", "Name", required = TRUE),
        reference_text_field("name_fr", "Name (French)"),
        reference_select_field(
          "type",
          "Type",
          choices_query = paste(
            "SELECT id AS value, name AS label",
            "FROM public.network_project_types",
            "ORDER BY name"
          ),
          required = TRUE
        ),
        reference_textarea_field(
          "description",
          "Description",
          required = TRUE
        ),
        reference_textarea_field(
          "description_fr",
          "Description (French)"
        )
      )
    ),
    projects = list(
      key = "projects",
      title = "Projects",
      description = paste(
        "Manage the named projects used for location grouping and",
        "filtering throughout the app."
      ),
      schema = "public",
      table = "projects",
      pk = "project_id",
      id_label = "Project ID",
      order_by = "name",
      audit_columns = audit_cols,
      columns = list(
        reference_text_field("name", "Name", required = TRUE),
        reference_text_field("name_fr", "Name (French)"),
        reference_select_field(
          "type",
          "Type",
          choices_query = paste(
            "SELECT id AS value, name AS label",
            "FROM public.network_project_types",
            "ORDER BY name"
          ),
          required = TRUE
        ),
        reference_textarea_field(
          "description",
          "Description",
          required = TRUE
        ),
        reference_textarea_field(
          "description_fr",
          "Description (French)"
        )
      )
    ),
    network_project_types = list(
      key = "network_project_types",
      title = "Network / Project Types",
      description = paste(
        "Manage the shared type list used by both networks and projects."
      ),
      schema = "public",
      table = "network_project_types",
      pk = "id",
      id_label = "Type ID",
      order_by = "name",
      audit_columns = audit_cols,
      columns = list(
        reference_text_field("name", "Name", required = TRUE),
        reference_text_field("name_fr", "Name (French)"),
        reference_textarea_field("description", "Description"),
        reference_textarea_field("description_fr", "Description (French)")
      )
    ),
    location_types = list(
      key = "location_types",
      title = "Location Types",
      description = paste(
        "Manage the location type list used when creating and editing",
        "monitoring locations."
      ),
      schema = "public",
      table = "location_types",
      pk = "type_id",
      id_label = "Type ID",
      order_by = "type",
      audit_columns = audit_cols,
      columns = list(
        reference_text_field("type", "Type", required = TRUE),
        reference_text_field("type_fr", "Type (French)"),
        reference_text_field("type_suffix", "Type suffix")
      )
    ),
    media_types = list(
      key = "media_types",
      title = "Media Types",
      description = paste(
        "Manage the media categories referenced by samples, timeseries,",
        "guidelines, rasters, and related filters."
      ),
      schema = "public",
      table = "media_types",
      pk = "media_id",
      id_label = "Media ID",
      order_by = "media_type",
      audit_columns = audit_cols,
      columns = list(
        reference_text_field("media_type", "Media type", required = TRUE),
        reference_text_field(
          "media_type_fr",
          "Media type (French)",
          required = TRUE
        ),
        reference_textarea_field("description", "Description"),
        reference_textarea_field("description_fr", "Description (French)")
      )
    ),
    parameter_groups = list(
      key = "parameter_groups",
      title = "Parameter Groups",
      description = paste(
        "Manage the higher-level parameter groups used by mapping and",
        "data-filtering modules."
      ),
      schema = "public",
      table = "parameter_groups",
      pk = "group_id",
      id_label = "Group ID",
      order_by = "group_name",
      audit_columns = audit_cols,
      columns = list(
        reference_text_field("group_name", "Group name", required = TRUE),
        reference_text_field("group_name_fr", "Group name (French)"),
        reference_textarea_field("description", "Description"),
        reference_textarea_field("description_fr", "Description (French)")
      )
    ),
    parameter_sub_groups = list(
      key = "parameter_sub_groups",
      title = "Parameter Sub-Groups",
      description = paste(
        "Manage the parameter sub-groups used by mapping and",
        "data-filtering modules."
      ),
      schema = "public",
      table = "parameter_sub_groups",
      pk = "sub_group_id",
      id_label = "Sub-group ID",
      order_by = "sub_group_name",
      audit_columns = audit_cols,
      columns = list(
        reference_text_field(
          "sub_group_name",
          "Sub-group name",
          required = TRUE
        ),
        reference_text_field(
          "sub_group_name_fr",
          "Sub-group name (French)"
        ),
        reference_textarea_field("description", "Description"),
        reference_textarea_field("description_fr", "Description (French)")
      )
    ),
    parameters = list(
      key = "parameters",
      title = "Parameters",
      description = paste(
        "Manage parameter definitions and the parameter group / sub-group",
        "links that drive map and filter groupings."
      ),
      schema = "public",
      table = "parameters",
      pk = "parameter_id",
      id_label = "Parameter ID",
      order_by = "param_name",
      audit_columns = audit_cols,
      display_columns = "relationship_summary",
      column_labels = c(relationship_summary = "Group links"),
      columns = list(
        reference_text_field("param_name", "Parameter name", required = TRUE),
        reference_text_field("param_name_fr", "Parameter name (French)"),
        reference_textarea_field("description", "Description"),
        reference_textarea_field("description_fr", "Description (French)"),
        reference_text_field(
          "unit_default",
          "Default unit",
          required = TRUE
        ),
        reference_text_field("unit_solid", "Solid unit"),
        reference_text_field("cas_number", "CAS number"),
        reference_checkbox_field(
          "result_speciation",
          "Result speciation required"
        ),
        reference_checkbox_field(
          "sample_fraction",
          "Sample fraction required"
        ),
        reference_select_field(
          "plot_default_y_orientation",
          "Default plot y-axis orientation",
          choices = data.frame(
            value = c("normal", "inverted"),
            label = c("Normal", "Inverted"),
            stringsAsFactors = FALSE
          ),
          required = TRUE,
          storage = "character"
        ),
        reference_numeric_field("plot_default_floor", "Default plot floor"),
        reference_numeric_field(
          "plot_default_ceiling",
          "Default plot ceiling"
        )
      )
    )
  )
}

reference_quote_identifier_chr <- function(con, value) {
  as.character(DBI::dbQuoteIdentifier(con, value))
}

reference_qualified_table_chr <- function(con, config) {
  as.character(DBI::dbQuoteIdentifier(
    con,
    DBI::Id(schema = config$schema, table = config$table)
  ))
}

reference_choice_df <- function(field, con = NULL) {
  choice_df <- if (!is.null(field$choices_query)) {
    DBI::dbGetQuery(con, field$choices_query)
  } else {
    field$choices
  }

  if (!all(c("value", "label") %in% names(choice_df))) {
    stop(
      "Select field choices must provide columns named 'value' and 'label'."
    )
  }

  choice_df$value <- as.character(choice_df$value)
  choice_df$label <- as.character(choice_df$label)
  missing_label <- is.na(choice_df$label) | !nzchar(choice_df$label)
  choice_df$label[missing_label] <- choice_df$value[missing_label]
  choice_df
}

reference_load_choices <- function(con, fields) {
  out <- list()

  for (field in fields) {
    if (!identical(field$input, "select")) {
      next
    }

    out[[field$name]] <- reference_choice_df(field, con = con)
  }

  out
}

reference_make_select_choices <- function(choice_df) {
  if (is.null(choice_df) || !nrow(choice_df)) {
    return(character(0))
  }

  stats::setNames(choice_df$value, choice_df$label)
}

reference_empty_row <- function(fields) {
  empty <- as.data.frame(
    setNames(
      replicate(length(fields), list(NA), simplify = FALSE),
      vapply(fields, `[[`, character(1), "name")
    ),
    stringsAsFactors = FALSE
  )

  for (field_name in names(empty)) {
    empty[[field_name]] <- NA
  }

  empty
}

reference_render_fields_ui <- function(ns, fields, choices, values = NULL) {
  tagList(lapply(fields, function(field) {
    input_id <- ns(reference_form_input_id(field$name))
    field_value <- if (
      !is.null(values) &&
        field$name %in% names(values) &&
        length(values[[field$name]])
    ) {
      values[[field$name]][1]
    } else {
      NA
    }

    if (identical(field$input, "text")) {
      return(textInput(
        input_id,
        field$label,
        value = if (is.na(field_value)) "" else as.character(field_value)
      ))
    }

    if (identical(field$input, "textarea")) {
      return(textAreaInput(
        input_id,
        field$label,
        value = if (is.na(field_value)) "" else as.character(field_value),
        width = "100%",
        height = paste0(reference_default(field$rows, 4) * 22, "px")
      ))
    }

    if (identical(field$input, "select")) {
      return(selectizeInput(
        input_id,
        field$label,
        choices = reference_make_select_choices(choices[[field$name]]),
        selected = if (is.na(field_value)) character(0) else as.character(field_value),
        multiple = FALSE,
        options = list(maxItems = 1)
      ))
    }

    if (identical(field$input, "numeric")) {
      return(numericInput(
        input_id,
        field$label,
        value = if (is.na(field_value)) NA_real_ else as.numeric(field_value),
        step = reference_default(field$step, "any")
      ))
    }

    if (identical(field$input, "checkbox")) {
      return(checkboxInput(
        input_id,
        field$label,
        value = isTRUE(field_value)
      ))
    }

    NULL
  }))
}

reference_normalize_value <- function(value, field) {
  if (identical(field$input, "checkbox")) {
    return(isTRUE(value))
  }

  if (identical(field$input, "numeric")) {
    if (is.null(value) || !length(value) || is.na(value[[1]])) {
      return(NA_real_)
    }
    return(as.numeric(value[[1]]))
  }

  first_value <- if (is.null(value) || !length(value)) {
    NA
  } else {
    value[[1]]
  }

  if (identical(field$input, "select")) {
    selected_value <- as.character(reference_default(first_value, NA_character_))
    if (is.na(selected_value) || !nzchar(selected_value)) {
      return(
        if (identical(field$storage, "integer")) {
          NA_integer_
        } else {
          NA_character_
        }
      )
    }

    return(
      if (identical(field$storage, "integer")) {
        as.integer(selected_value)
      } else {
        selected_value
      }
    )
  }

  if (identical(field$input, "text") || identical(field$input, "textarea")) {
    text_value <- as.character(reference_default(first_value, NA_character_))
    text_value <- trimws(text_value)
    if (is.na(text_value) || !nzchar(text_value)) {
      return(NA_character_)
    }
    return(text_value)
  }

  first_value
}

reference_is_missing_value <- function(value) {
  if (is.logical(value)) {
    return(FALSE)
  }
  if (is.numeric(value) || is.integer(value)) {
    return(is.na(value))
  }
  is.na(value) || (is.character(value) && !nzchar(value))
}

reference_update_fields <- function(session, fields, row, choices) {
  for (field in fields) {
    input_id <- reference_form_input_id(field$name)
    value <- row[[field$name]][1]

    if (identical(field$input, "text")) {
      updateTextInput(
        session,
        input_id,
        value = if (is.na(value)) "" else as.character(value)
      )
      next
    }

    if (identical(field$input, "textarea")) {
      updateTextAreaInput(
        session,
        input_id,
        value = if (is.na(value)) "" else as.character(value)
      )
      next
    }

    if (identical(field$input, "select")) {
      updateSelectizeInput(
        session,
        input_id,
        choices = reference_make_select_choices(choices[[field$name]]),
        selected = if (is.na(value)) character(0) else as.character(value)
      )
      next
    }

    if (identical(field$input, "numeric")) {
      updateNumericInput(
        session,
        input_id,
        value = if (is.na(value)) NA_real_ else as.numeric(value)
      )
      next
    }

    if (identical(field$input, "checkbox")) {
      updateCheckboxInput(
        session,
        input_id,
        value = isTRUE(value)
      )
    }
  }
}

reference_clear_fields <- function(session, fields, choices) {
  empty_row <- as.data.frame(setNames(
    replicate(length(fields), NA, simplify = FALSE),
    vapply(fields, `[[`, character(1), "name")
  ))
  for (field_name in names(empty_row)) {
    empty_row[[field_name]] <- NA
  }
  reference_update_fields(session, fields, empty_row, choices)
}

reference_field_label <- function(config, column_name) {
  if (
    !is.null(config$column_labels) &&
      column_name %in% names(config$column_labels)
  ) {
    return(unname(config$column_labels[[column_name]]))
  }

  field_names <- vapply(config$columns, `[[`, character(1), "name")
  field_idx <- match(column_name, field_names)
  if (!is.na(field_idx)) {
    return(config$columns[[field_idx]]$label)
  }

  switch(
    column_name,
    created = "Created",
    created_by = "Created by",
    modified = "Modified",
    modified_by = "Modified by",
    column_name
  )
}

reference_display_labels <- function(config, column_names) {
  vapply(column_names, function(column_name) {
    if (identical(column_name, config$pk)) {
      return(reference_default(config$id_label, column_name))
    }
    reference_field_label(config, column_name)
  }, character(1))
}

reference_build_display_data <- function(raw, config, choices) {
  display_cols <- unique(c(
    config$pk,
    vapply(config$columns, `[[`, character(1), "name"),
    reference_default(config$display_columns, character(0)),
    reference_default(config$audit_columns, character(0))
  ))
  display_cols <- intersect(display_cols, names(raw))

  if (!length(display_cols)) {
    return(data.frame())
  }

  display <- raw[, display_cols, drop = FALSE]

  for (field in config$columns) {
    if (
      !identical(field$input, "select") ||
        !field$name %in% names(display)
    ) {
      next
    }

    choice_df <- choices[[field$name]]
    if (is.null(choice_df) || !nrow(choice_df)) {
      next
    }

    display[[field$name]] <- choice_df$label[match(
      as.character(display[[field$name]]),
      choice_df$value
    )]
  }

  display
}

reference_prepare_for_dt <- function(df) {
  if (!ncol(df)) {
    return(df)
  }

  out <- df

  for (col_name in names(out)) {
    column <- out[[col_name]]

    if (inherits(column, c("POSIXct", "POSIXt", "Date"))) {
      out[[col_name]] <- as.character(column)
      next
    }

    if (is.logical(column)) {
      out[[col_name]] <- ifelse(
        is.na(column),
        "",
        ifelse(column, "TRUE", "FALSE")
      )
      next
    }

    if (inherits(column, "integer64")) {
      out[[col_name]] <- as.character(column)
      next
    }

    if (is.list(column)) {
      out[[col_name]] <- vapply(column, function(value) {
        if (length(value) == 0 || all(is.na(value))) {
          ""
        } else {
          paste(as.character(value), collapse = ", ")
        }
      }, character(1))
    }
  }

  out
}

reference_fetch_records <- function(con, config) {
  order_cols <- reference_default(config$order_by, config$pk)
  order_sql <- paste(reference_quote_identifier_chr(con, order_cols), collapse = ", ")

  DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT * FROM %s ORDER BY %s",
      reference_qualified_table_chr(con, config),
      order_sql
    )
  )
}

reference_save_record <- function(
  con,
  config,
  values,
  selected_id = NULL,
  existing_columns = character(0)
) {
  column_names <- names(values)
  quoted_columns <- reference_quote_identifier_chr(con, column_names)

  if (is.null(selected_id)) {
    insert_sql <- sprintf(
      "INSERT INTO %s (%s) VALUES (%s) RETURNING %s",
      reference_qualified_table_chr(con, config),
      paste(quoted_columns, collapse = ", "),
      paste0("$", seq_along(column_names), collapse = ", "),
      reference_quote_identifier_chr(con, config$pk)
    )

    inserted <- DBI::dbGetQuery(con, insert_sql, params = values)
    return(inserted[[config$pk]][1])
  }

  set_sql <- paste(
    sprintf("%s = $%d", quoted_columns, seq_along(column_names)),
    collapse = ", "
  )

  if ("modified" %in% existing_columns) {
    set_sql <- paste(
      set_sql,
      sprintf("%s = CURRENT_TIMESTAMP", reference_quote_identifier_chr(con, "modified")),
      sep = ", "
    )
  }
  if ("modified_by" %in% existing_columns) {
    set_sql <- paste(
      set_sql,
      sprintf("%s = CURRENT_USER", reference_quote_identifier_chr(con, "modified_by")),
      sep = ", "
    )
  }

  update_sql <- sprintf(
    "UPDATE %s SET %s WHERE %s = $%d",
    reference_qualified_table_chr(con, config),
    set_sql,
    reference_quote_identifier_chr(con, config$pk),
    length(column_names) + 1
  )

  DBI::dbExecute(
    con,
    update_sql,
    params = c(values, list(type.convert(as.character(selected_id), as.is = TRUE)))
  )

  type.convert(as.character(selected_id), as.is = TRUE)
}

clear_reference_caches <- function(session) {
  cache_keys <- c(
    "cont_data.plot_module_data",
    "disc_data_module_data",
    "map_params_module_data",
    "map_location_module_data"
  )

  for (cache_key in cache_keys) {
    clear_cached(cache_key)

    if (
      !is.null(session$userData$app_cache) &&
        is.environment(session$userData$app_cache)
    ) {
      clear_cached(cache_key, env = session$userData$app_cache)
    }
  }
}

referenceTableManagerUI <- function(id, title = "Reference data") {
  ns <- NS(id)

  bslib::page_sidebar(
    title = NULL,
    sidebar = bslib::sidebar(
      width = 430,
      title = title,
      uiOutput(ns("panel_intro")),
      uiOutput(ns("record_summary")),
      div(
        class = "d-flex gap-2 flex-wrap mb-3",
        actionButton(ns("save"), "Save", class = "btn-primary"),
        actionButton(ns("new_record"), "New record"),
        actionButton(ns("reload"), "Reload")
      ),
      uiOutput(ns("form_ui"))
    ),
    uiOutput(ns("banner")),
    uiOutput(ns("table_heading")),
    DT::DTOutput(ns("records_table"))
  )
}

referenceTableManager <- function(
  id,
  language,
  config,
  notification_module_id
) {
  moduleServer(id, function(input, output, session) {
    con <- session$userData$AquaCache
    table_proxy <- DT::dataTableProxy("records_table", session = session)

    editor_data <- reactiveValues(
      choices = list(),
      records_raw = data.frame(),
      records_display = data.frame()
    )
    current_record_id <- reactiveVal(NULL)
    current_row <- reactiveVal(reference_empty_row(config$columns))

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = session$ns,
        lang = language$language,
        con = con,
        module_id = notification_module_id
      )
    })

    refresh_editor <- function(selected_id = NULL) {
      selected_id <- if (is.null(selected_id) || !length(selected_id)) {
        NULL
      } else {
        as.character(selected_id)[1]
      }

      choices <- reference_load_choices(con, config$columns)
      raw <- reference_fetch_records(con, config)
      display <- reference_build_display_data(raw, config, choices)

      editor_data$choices <- choices
      editor_data$records_raw <- raw
      editor_data$records_display <- display

      session$onFlushed(function() {
        if (is.null(selected_id)) {
          current_record_id(NULL)
          current_row(reference_empty_row(config$columns))
          DT::selectRows(table_proxy, NULL)
          return()
        }

        selected_idx <- match(
          as.character(selected_id),
          as.character(raw[[config$pk]])
        )

        if (is.na(selected_idx)) {
          current_record_id(NULL)
          current_row(reference_empty_row(config$columns))
          DT::selectRows(table_proxy, NULL)
          return()
        }

        current_record_id(as.character(selected_id))
        current_row(raw[selected_idx, , drop = FALSE])
        DT::selectRows(table_proxy, selected_idx)
      }, once = TRUE)
    }

    output$panel_intro <- renderUI({
      tagList(
        tags$p(class = "text-muted", config$description),
        tags$p(
          class = "text-muted small",
          paste(
            "This editor supports adds and updates.",
            "Deletes are intentionally excluded because these tables are",
            "widely referenced elsewhere in the application."
          )
        )
      )
    })

    output$record_summary <- renderUI({
      record_id <- current_record_id()

      tags$p(
        tags$strong("Mode: "),
        if (is.null(record_id)) {
          paste("New", tolower(config$title), "record")
        } else {
          paste("Editing", reference_default(config$id_label, config$pk), record_id)
        }
      )
    })

    output$form_ui <- renderUI({
      reference_render_fields_ui(
        session$ns,
        config$columns,
        editor_data$choices,
        values = current_row()
      )
    })

    output$table_heading <- renderUI({
      row_count <- nrow(editor_data$records_display)

      tags$div(
        class = "mb-3",
        tags$h4(paste(config$title, "records")),
        tags$p(
          class = "text-muted",
          paste(
            "Select a row to load it into the form.",
            row_count,
            "record(s) currently available."
          )
        )
      )
    })

    output$records_table <- DT::renderDT({
      display <- reference_prepare_for_dt(editor_data$records_display)
      DT::datatable(
        display,
        rownames = FALSE,
        selection = "single",
        filter = "top",
        colnames = unname(reference_display_labels(config, names(display))),
        options = list(
          pageLength = 10,
          lengthChange = FALSE,
          autoWidth = TRUE,
          scrollX = TRUE
        )
      )
    }, server = FALSE)

    observeEvent(input$reload, {
      selected_id <- current_record_id()
      refresh_editor(selected_id = selected_id)
    })

    observeEvent(input$new_record, {
      current_record_id(NULL)
      current_row(reference_empty_row(config$columns))
      DT::selectRows(table_proxy, NULL)
    })

    observeEvent(
      input$records_table_rows_selected,
      {
        req(length(input$records_table_rows_selected) == 1)
        selected_row <- input$records_table_rows_selected[[1]]
        req(nrow(editor_data$records_raw) >= selected_row)

        selected_id <- as.character(
          editor_data$records_raw[[config$pk]][selected_row]
        )

        current_record_id(selected_id)
        current_row(editor_data$records_raw[selected_row, , drop = FALSE])
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$save,
      {
        values <- lapply(config$columns, function(field) {
          reference_normalize_value(
            input[[reference_form_input_id(field$name)]],
            field
          )
        })
        names(values) <- vapply(config$columns, `[[`, character(1), "name")

        validation_errors <- character(0)
        for (field in config$columns) {
          if (
            isTRUE(field$required) &&
              reference_is_missing_value(values[[field$name]])
          ) {
            validation_errors <- c(
              validation_errors,
              paste(field$label, "is required.")
            )
          }
        }

        if (length(validation_errors)) {
          showNotification(
            paste(validation_errors, collapse = " "),
            type = "error"
          )
          return()
        }

        tryCatch(
          {
            saved_id <- reference_save_record(
              con = con,
              config = config,
              values = values,
              selected_id = current_record_id(),
              existing_columns = names(editor_data$records_raw)
            )

            clear_reference_caches(session)
            refresh_editor(selected_id = saved_id)
            showNotification(
              paste(config$title, "saved."),
              type = "message"
            )
          },
          error = function(e) {
            showNotification(
              paste("Save failed:", conditionMessage(e)),
              type = "error",
              duration = 10
            )
          }
        )
      },
      ignoreInit = TRUE
    )

    refresh_editor()
  })
}

parameter_relationships_empty <- function() {
  data.frame(
    relationship_id = integer(0),
    group_id = integer(0),
    group_name = character(0),
    sub_group_id = integer(0),
    sub_group_name = character(0),
    stringsAsFactors = FALSE
  )
}

parameter_relationship_keys <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(character(0))
  }

  paste(
    as.character(df$group_id),
    ifelse(is.na(df$sub_group_id), "NA", as.character(df$sub_group_id)),
    sep = "::"
  )
}

fetch_parameter_records <- function(con, config) {
  raw <- reference_fetch_records(con, config)
  raw$relationship_summary <- ""

  relationships <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT",
      "  pr.parameter_id,",
      "  pg.group_name,",
      "  sg.sub_group_name",
      "FROM public.parameter_relationships pr",
      "JOIN public.parameter_groups pg ON pg.group_id = pr.group_id",
      "LEFT JOIN public.parameter_sub_groups sg",
      "  ON sg.sub_group_id = pr.sub_group_id",
      "ORDER BY pr.parameter_id, pg.group_name, sg.sub_group_name"
    )
  )

  if (!nrow(raw) || !nrow(relationships)) {
    return(raw)
  }

  relationship_labels <- ifelse(
    is.na(relationships$sub_group_name),
    relationships$group_name,
    paste(relationships$group_name, relationships$sub_group_name, sep = " / ")
  )

  relationship_summary <- tapply(
    relationship_labels,
    relationships$parameter_id,
    function(values) paste(unique(values), collapse = "; ")
  )

  matched_summary <- relationship_summary[match(
    as.character(raw$parameter_id),
    names(relationship_summary)
  )]
  raw$relationship_summary <- ifelse(
    is.na(matched_summary),
    "",
    as.character(matched_summary)
  )

  raw
}

fetch_parameter_relationships <- function(con, parameter_id) {
  if (is.null(parameter_id) || !length(parameter_id) || is.na(parameter_id)) {
    return(parameter_relationships_empty())
  }

  DBI::dbGetQuery(
    con,
    paste(
      "SELECT",
      "  pr.relationship_id,",
      "  pr.group_id,",
      "  pg.group_name,",
      "  pr.sub_group_id,",
      "  sg.sub_group_name",
      "FROM public.parameter_relationships pr",
      "JOIN public.parameter_groups pg ON pg.group_id = pr.group_id",
      "LEFT JOIN public.parameter_sub_groups sg",
      "  ON sg.sub_group_id = pr.sub_group_id",
      "WHERE pr.parameter_id = $1",
      "ORDER BY pg.group_name, sg.sub_group_name"
    ),
    params = list(as.integer(parameter_id))
  )
}

fetch_parameter_relationship_choices <- function(con) {
  list(
    groups = DBI::dbGetQuery(
      con,
      paste(
        "SELECT group_id AS value, group_name AS label",
        "FROM public.parameter_groups",
        "ORDER BY group_name"
      )
    ),
    sub_groups = DBI::dbGetQuery(
      con,
      paste(
        "SELECT sub_group_id AS value, sub_group_name AS label",
        "FROM public.parameter_sub_groups",
        "ORDER BY sub_group_name"
      )
    )
  )
}

parameterManagerUI <- function(id) {
  ns <- NS(id)
  config <- reference_table_configs()[["parameters"]]

  bslib::page_sidebar(
    title = NULL,
    sidebar = bslib::sidebar(
      width = 430,
      title = config$title,
      uiOutput(ns("panel_intro")),
      uiOutput(ns("record_summary")),
      div(
        class = "d-flex gap-2 flex-wrap mb-3",
        actionButton(ns("save"), "Save", class = "btn-primary"),
        actionButton(ns("new_record"), "New record"),
        actionButton(ns("reload"), "Reload")
      ),
      uiOutput(ns("form_ui")),
      tags$hr(class = "my-3"),
      tags$h5("Group links"),
      tags$p(
        class = "text-muted small",
        paste(
          "Add one or more parameter group / sub-group combinations.",
          "Sub-group is optional."
        )
      ),
      selectizeInput(
        ns("relationship_group_id"),
        "Parameter group",
        choices = character(0),
        selected = character(0),
        multiple = FALSE,
        options = list(maxItems = 1)
      ),
      selectizeInput(
        ns("relationship_sub_group_id"),
        "Parameter sub-group",
        choices = character(0),
        selected = character(0),
        multiple = FALSE,
        options = list(maxItems = 1, placeholder = "Optional")
      ),
      div(
        class = "d-flex gap-2 flex-wrap mb-2",
        actionButton(ns("add_relationship"), "Add group link"),
        actionButton(ns("remove_relationship"), "Remove selected")
      ),
      DT::DTOutput(ns("relationships_table"))
    ),
    uiOutput(ns("banner")),
    uiOutput(ns("table_heading")),
    DT::DTOutput(ns("records_table"))
  )
}

manageParameters <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    con <- session$userData$AquaCache
    config <- reference_table_configs()[["parameters"]]
    table_proxy <- DT::dataTableProxy("records_table", session = session)
    relationship_proxy <- DT::dataTableProxy(
      "relationships_table",
      session = session
    )

    editor_data <- reactiveValues(
      field_choices = list(),
      relationship_choices = list(),
      records_raw = data.frame(),
      records_display = data.frame()
    )
    current_record_id <- reactiveVal(NULL)
    current_relationships <- reactiveVal(parameter_relationships_empty())
    current_row <- reactiveVal(reference_empty_row(config$columns))

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = session$ns,
        lang = language$language,
        con = con,
        module_id = "manageParameters"
      )
    })

    update_relationship_selectors <- function(relationship_choices) {
      updateSelectizeInput(
        session,
        "relationship_group_id",
        choices = reference_make_select_choices(relationship_choices$groups),
        selected = character(0)
      )
      updateSelectizeInput(
        session,
        "relationship_sub_group_id",
        choices = reference_make_select_choices(relationship_choices$sub_groups),
        selected = character(0)
      )
    }

    clear_parameter_editor <- function(field_choices, relationship_choices) {
      current_record_id(NULL)
      current_row(reference_empty_row(config$columns))
      current_relationships(parameter_relationships_empty())
      update_relationship_selectors(relationship_choices)
      DT::selectRows(table_proxy, NULL)
      DT::selectRows(relationship_proxy, NULL)
    }

    populate_parameter_editor <- function(
      selected_id,
      raw_row,
      field_choices,
      relationship_choices
    ) {
      current_record_id(as.character(selected_id))
      current_row(raw_row)
      current_relationships(fetch_parameter_relationships(con, selected_id))
      update_relationship_selectors(relationship_choices)
      DT::selectRows(relationship_proxy, NULL)
    }

    refresh_editor <- function(selected_id = NULL) {
      selected_id <- if (is.null(selected_id) || !length(selected_id)) {
        NULL
      } else {
        as.character(selected_id)[1]
      }

      field_choices <- reference_load_choices(con, config$columns)
      relationship_choices <- fetch_parameter_relationship_choices(con)
      raw <- fetch_parameter_records(con, config)
      display <- reference_build_display_data(raw, config, field_choices)

      editor_data$field_choices <- field_choices
      editor_data$relationship_choices <- relationship_choices
      editor_data$records_raw <- raw
      editor_data$records_display <- display

      session$onFlushed(function() {
        update_relationship_selectors(relationship_choices)

        if (is.null(selected_id)) {
          clear_parameter_editor(field_choices, relationship_choices)
          return()
        }

        selected_idx <- match(
          as.character(selected_id),
          as.character(raw[[config$pk]])
        )

        if (is.na(selected_idx)) {
          clear_parameter_editor(field_choices, relationship_choices)
          return()
        }

        populate_parameter_editor(
          selected_id = selected_id,
          raw_row = raw[selected_idx, , drop = FALSE],
          field_choices = field_choices,
          relationship_choices = relationship_choices
        )
        DT::selectRows(table_proxy, selected_idx)
      }, once = TRUE)
    }

    output$panel_intro <- renderUI({
      tagList(
        tags$p(class = "text-muted", config$description),
        tags$p(
          class = "text-muted small",
          paste(
            "Save updates the selected parameter and synchronizes its",
            "parameter group / sub-group links."
          )
        )
      )
    })

    output$record_summary <- renderUI({
      record_id <- current_record_id()

      tags$p(
        tags$strong("Mode: "),
        if (is.null(record_id)) {
          "New parameter record"
        } else {
          paste("Editing", config$id_label, record_id)
        }
      )
    })

    output$form_ui <- renderUI({
      reference_render_fields_ui(
        session$ns,
        config$columns,
        editor_data$field_choices,
        values = current_row()
      )
    })

    output$table_heading <- renderUI({
      row_count <- nrow(editor_data$records_display)

      tags$div(
        class = "mb-3",
        tags$h4("Parameter records"),
        tags$p(
          class = "text-muted",
          paste(
            "Select a row to load it into the form.",
            row_count,
            "record(s) currently available."
          )
        )
      )
    })

    output$records_table <- DT::renderDT({
      display <- reference_prepare_for_dt(editor_data$records_display)
      DT::datatable(
        display,
        rownames = FALSE,
        selection = "single",
        filter = "top",
        colnames = unname(reference_display_labels(config, names(display))),
        options = list(
          pageLength = 10,
          lengthChange = FALSE,
          autoWidth = TRUE,
          scrollX = TRUE
        )
      )
    }, server = FALSE)

    output$relationships_table <- DT::renderDT({
      relationship_display <- current_relationships()[, c(
        "group_name",
        "sub_group_name"
      ), drop = FALSE]

      names(relationship_display) <- c(
        "Parameter group",
        "Parameter sub-group"
      )

      DT::datatable(
        relationship_display,
        rownames = FALSE,
        selection = "single",
        options = list(
          dom = "t",
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          autoWidth = TRUE,
          scrollX = TRUE
        )
      )
    }, server = FALSE)

    observeEvent(input$reload, {
      selected_id <- current_record_id()
      refresh_editor(selected_id = selected_id)
    })

    observeEvent(input$new_record, {
      clear_parameter_editor(
        editor_data$field_choices,
        editor_data$relationship_choices
      )
    })

    observeEvent(
      input$records_table_rows_selected,
      {
        req(length(input$records_table_rows_selected) == 1)
        selected_row <- input$records_table_rows_selected[[1]]
        req(nrow(editor_data$records_raw) >= selected_row)

        selected_id <- as.character(
          editor_data$records_raw[[config$pk]][selected_row]
        )

        populate_parameter_editor(
          selected_id = selected_id,
          raw_row = editor_data$records_raw[selected_row, , drop = FALSE],
          field_choices = editor_data$field_choices,
          relationship_choices = editor_data$relationship_choices
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$add_relationship,
      {
        relationship_choices <- editor_data$relationship_choices
        group_id <- suppressWarnings(as.integer(input$relationship_group_id))
        sub_group_id <- suppressWarnings(as.integer(input$relationship_sub_group_id))

        if (is.na(group_id)) {
          showNotification(
            "Select a parameter group before adding a link.",
            type = "error"
          )
          return()
        }

        new_relationship <- data.frame(
          relationship_id = NA_integer_,
          group_id = group_id,
          group_name = relationship_choices$groups$label[match(
            as.character(group_id),
            relationship_choices$groups$value
          )],
          sub_group_id = if (is.na(sub_group_id)) NA_integer_ else sub_group_id,
          sub_group_name = if (is.na(sub_group_id)) {
            NA_character_
          } else {
            relationship_choices$sub_groups$label[match(
              as.character(sub_group_id),
              relationship_choices$sub_groups$value
            )]
          },
          stringsAsFactors = FALSE
        )

        existing_relationships <- current_relationships()
        existing_keys <- parameter_relationship_keys(existing_relationships)
        new_key <- parameter_relationship_keys(new_relationship)

        if (new_key %in% existing_keys) {
          showNotification(
            "That group link is already present for this parameter.",
            type = "warning"
          )
          return()
        }

        updated_relationships <- rbind(existing_relationships, new_relationship)
        order_idx <- order(
          tolower(updated_relationships$group_name),
          tolower(ifelse(
            is.na(updated_relationships$sub_group_name),
            "",
            updated_relationships$sub_group_name
          ))
        )
        current_relationships(updated_relationships[order_idx, , drop = FALSE])
        update_relationship_selectors(relationship_choices)
        DT::selectRows(relationship_proxy, NULL)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$remove_relationship,
      {
        selected_row <- input$relationships_table_rows_selected
        if (!length(selected_row)) {
          showNotification(
            "Select a group link to remove.",
            type = "warning"
          )
          return()
        }

        relationships <- current_relationships()
        relationships <- relationships[-selected_row, , drop = FALSE]
        current_relationships(relationships)
        DT::selectRows(relationship_proxy, NULL)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$save,
      {
        values <- lapply(config$columns, function(field) {
          reference_normalize_value(
            input[[reference_form_input_id(field$name)]],
            field
          )
        })
        names(values) <- vapply(config$columns, `[[`, character(1), "name")

        validation_errors <- character(0)
        for (field in config$columns) {
          if (
            isTRUE(field$required) &&
              reference_is_missing_value(values[[field$name]])
          ) {
            validation_errors <- c(
              validation_errors,
              paste(field$label, "is required.")
            )
          }
        }

        if (
          !is.na(values$plot_default_floor) &&
            !is.na(values$plot_default_ceiling) &&
            values$plot_default_floor > values$plot_default_ceiling
        ) {
          validation_errors <- c(
            validation_errors,
            "Default plot floor cannot be greater than the default plot ceiling."
          )
        }

        if (length(validation_errors)) {
          showNotification(
            paste(validation_errors, collapse = " "),
            type = "error"
          )
          return()
        }

        tryCatch(
          {
            saved_id <- DBI::dbWithTransaction(con, {
              saved_parameter_id <- reference_save_record(
                con = con,
                config = config,
                values = values,
                selected_id = current_record_id(),
                existing_columns = names(editor_data$records_raw)
              )

              existing_relationships <- fetch_parameter_relationships(
                con,
                saved_parameter_id
              )
              desired_relationships <- current_relationships()[, c(
                "group_id",
                "sub_group_id"
              ), drop = FALSE]

              existing_keys <- parameter_relationship_keys(existing_relationships)
              desired_keys <- parameter_relationship_keys(desired_relationships)

              delete_ids <- existing_relationships$relationship_id[
                !(existing_keys %in% desired_keys)
              ]
              if (length(delete_ids)) {
                for (relationship_id in delete_ids) {
                  DBI::dbExecute(
                    con,
                    paste(
                      "DELETE FROM public.parameter_relationships",
                      "WHERE relationship_id = $1"
                    ),
                    params = list(as.integer(relationship_id))
                  )
                }
              }

              new_relationships <- desired_relationships[
                !(desired_keys %in% existing_keys),
                ,
                drop = FALSE
              ]
              if (nrow(new_relationships)) {
                for (row_idx in seq_len(nrow(new_relationships))) {
                  DBI::dbExecute(
                    con,
                    paste(
                      "INSERT INTO public.parameter_relationships",
                      "(parameter_id, group_id, sub_group_id)",
                      "VALUES ($1, $2, $3)"
                    ),
                    params = list(
                      as.integer(saved_parameter_id),
                      as.integer(new_relationships$group_id[row_idx]),
                      if (is.na(new_relationships$sub_group_id[row_idx])) {
                        NA_integer_
                      } else {
                        as.integer(new_relationships$sub_group_id[row_idx])
                      }
                    )
                  )
                }
              }

              saved_parameter_id
            })

            clear_reference_caches(session)
            refresh_editor(selected_id = saved_id)
            showNotification(
              "Parameters saved.",
              type = "message"
            )
          },
          error = function(e) {
            showNotification(
              paste("Save failed:", conditionMessage(e)),
              type = "error",
              duration = 10
            )
          }
        )
      },
      ignoreInit = TRUE
    )

    refresh_editor()
  })
}

manageOrganizationsUI <- function(id) {
  referenceTableManagerUI(id, title = "Organizations")
}

manageOrganizations <- function(id, language) {
  config <- reference_table_configs()[["organizations"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageOrganizations"
  )
}

manageNetworksUI <- function(id) {
  referenceTableManagerUI(id, title = "Networks")
}

manageNetworks <- function(id, language) {
  config <- reference_table_configs()[["networks"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageNetworks"
  )
}

manageProjectsUI <- function(id) {
  referenceTableManagerUI(id, title = "Projects")
}

manageProjects <- function(id, language) {
  config <- reference_table_configs()[["projects"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageProjects"
  )
}

manageNetworkProjectTypesUI <- function(id) {
  referenceTableManagerUI(id, title = "Network / Project Types")
}

manageNetworkProjectTypes <- function(id, language) {
  config <- reference_table_configs()[["network_project_types"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageNetworkProjectTypes"
  )
}

manageLocationTypesUI <- function(id) {
  referenceTableManagerUI(id, title = "Location Types")
}

manageLocationTypes <- function(id, language) {
  config <- reference_table_configs()[["location_types"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageLocationTypes"
  )
}

manageMediaTypesUI <- function(id) {
  referenceTableManagerUI(id, title = "Media Types")
}

manageMediaTypes <- function(id, language) {
  config <- reference_table_configs()[["media_types"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageMediaTypes"
  )
}

manageParameterGroupsUI <- function(id) {
  referenceTableManagerUI(id, title = "Parameter Groups")
}

manageParameterGroups <- function(id, language) {
  config <- reference_table_configs()[["parameter_groups"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageParameterGroups"
  )
}

manageParameterSubGroupsUI <- function(id) {
  referenceTableManagerUI(id, title = "Parameter Sub-Groups")
}

manageParameterSubGroups <- function(id, language) {
  config <- reference_table_configs()[["parameter_sub_groups"]]
  referenceTableManager(
    id = id,
    language = language,
    config = config,
    notification_module_id = "manageParameterSubGroups"
  )
}

manageParametersUI <- function(id) {
  parameterManagerUI(id)
}
