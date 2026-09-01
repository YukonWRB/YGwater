# UI and server code for water quality guidelines management module.
#
# This version targets the patch-47 AquaCache guideline engine. The UI keeps
# common edits compact while writing the normalized guideline/rule/input tables
# that the database uses for validation and application.

addGuidelinesUI <- function(id) {
  ns <- NS(id)
  single_selectize_options <- function(placeholder, create = FALSE) {
    options <- list(maxItems = 1, placeholder = placeholder)
    if (isTRUE(create)) {
      options$create <- TRUE
    }
    options
  }

  tagList(
    tags$head(
      tags$script(HTML(
        "
Shiny.addCustomMessageHandler('insertAtCursor', function(msg) {
  const el = document.getElementById(msg.target);
  if (!el) return;

  const isCodeEditor = el.tagName.toLowerCase() === 'bslib-code-editor';
  const ta = isCodeEditor ? el.querySelector('textarea') : el;
  if (!ta) return;

  ta.focus();

  const currentValue = isCodeEditor ? el.value : ta.value;
  const start = ta.selectionStart ?? 0;
  const end = ta.selectionEnd ?? 0;
  const before = currentValue.slice(0, start);
  const after = currentValue.slice(end);
  const insert = msg.text || '';
  let newValue = before + insert + after;
  const caret = start + insert.length;

  if (msg.eolComment && msg.eolComment.trim().length > 0) {
    const lineStart = newValue.lastIndexOf('\\n', caret - 1) + 1;
    const lineEndIdx = newValue.indexOf('\\n', caret);
    const lineEnd = (lineEndIdx === -1) ? newValue.length : lineEndIdx;
    const lineText = newValue.slice(lineStart, lineEnd);
    const commentIdx = lineText.indexOf('--');
    const norm = s => s.replace(/\\s+/g, ' ').trim().toLowerCase();

    if (commentIdx === -1) {
      const left = newValue.slice(0, lineEnd).replace(/[ \\t]+$/, '');
      const right = newValue.slice(lineEnd);
      newValue = left + '  -- ' + msg.eolComment.trim() + right;
    } else {
      const codePart = lineText.slice(0, commentIdx).replace(/[ \\t]+$/, '');
      const commentPart = lineText.slice(commentIdx + 2);
      const tokens = commentPart.split(';').map(t => t.trim()).filter(t => t.length > 0);
      const hasAlready = tokens.some(t => norm(t) === norm(msg.eolComment));
      const newTokens = hasAlready ? tokens : tokens.concat([msg.eolComment.trim()]);
      const newLine = codePart + '  -- ' + newTokens.join(' ; ');
      newValue = newValue.slice(0, lineStart) + newLine + newValue.slice(lineEnd);
    }
  }

  if (isCodeEditor) {
    el.value = newValue;
  } else {
    ta.value = newValue;
  }

  ta.setSelectionRange(caret, caret);
  if (isCodeEditor) {
    el.dispatchEvent(new CustomEvent('bslibCodeEditorUpdate', { bubbles: true }));
  } else {
    ta.dispatchEvent(new Event('input', { bubbles: true }));
  }
});

Shiny.addCustomMessageHandler('openGuidelineHelp', function(url) {
  window.open(url, '_blank', 'noopener');
});
"
      )),
      tags$style(HTML(
        "
.guideline-sql-editor .code-editor,
.guideline-sql-editor .code-editor .prism-code-editor,
.guideline-sql-editor .code-editor textarea,
.guideline-sql-editor .code-editor pre,
.guideline-sql-editor .code-editor code {
  font-family: Consolas, 'Courier New', monospace !important;
  font-variant-ligatures: none;
  font-feature-settings: 'liga' 0, 'calt' 0;
  letter-spacing: 0 !important;
}

.guideline-sql-editor .code-editor textarea {
  resize: none;
}
"
      ))
    ),
    uiOutput(ns("banner")),
    page_sidebar(
      sidebar = sidebar(
        title = NULL,
        position = "right",
        width = "50%",
        bg = config$sidebar_bg,
        open = list(mobile = "always-above"),

        h4("Guideline"),
        textInput(ns("guideline_code"), "Code", width = "100%"),
        textInput(ns("guideline_name"), "Name", width = "100%"),
        selectizeInput(
          ns("publisher"),
          "Publisher",
          choices = NULL,
          width = "100%",
          selected = character(0),
          multiple = TRUE,
          options = single_selectize_options(
            "Select or type to add publisher",
            create = TRUE
          )
        ),
        selectizeInput(
          ns("series"),
          "Series",
          choices = NULL,
          width = "100%",
          selected = character(0),
          multiple = TRUE,
          options = single_selectize_options(
            "Select or type to add series",
            create = TRUE
          )
        ),
        selectizeInput(
          ns("parameter_id"),
          "Parameter",
          choices = NULL,
          width = "100%",
          selected = character(0),
          multiple = TRUE,
          options = single_selectize_options("Select a parameter")
        ),
        selectizeInput(
          ns("matrix_state"),
          "Matrix state",
          choices = NULL,
          width = "100%",
          selected = character(0),
          multiple = TRUE,
          options = single_selectize_options("Select a matrix state")
        ),
        uiOutput(ns("parameter_unit_status")),
        shinyjs::hidden(
          div(
            id = ns("result_speciation_section"),
            selectizeInput(
              ns("result_speciation"),
              "Result speciation",
              choices = NULL,
              width = "100%",
              selected = character(0),
              multiple = TRUE,
              options = single_selectize_options(
                "Leave empty to apply to all speciations"
              )
            )
          )
        ),
        selectizeInput(
          ns("sample_fraction"),
          "Guideline fractions",
          choices = NULL,
          width = "100%",
          selected = character(0),
          multiple = TRUE,
          options = list(placeholder = "Leave empty to apply to all fractions")
        ),
        uiOutput(ns("fraction_speciation_applicability_note")),
        selectizeInput(
          ns("media_type"),
          "Guideline media types",
          choices = NULL,
          width = "100%",
          selected = character(0),
          multiple = TRUE,
          options = list(placeholder = "Select one or more media types")
        ),

        h4("Applicability"),
        selectInput(ns("comparison_operator"), "Comparison", choices = NULL),
        selectizeInput(
          ns("jurisdiction"),
          "Jurisdiction",
          choices = NULL,
          width = "100%",
          selected = character(0),
          multiple = TRUE,
          options = single_selectize_options(
            "Select or type to add a jurisdiction",
            create = TRUE
          )
        ),
        selectizeInput(
          ns("protection_goal"),
          "Protection goal",
          choices = NULL,
          width = "100%",
          selected = character(0),
          multiple = TRUE,
          options = single_selectize_options(
            "Select or type to add a protection goal",
            create = TRUE
          )
        ),
        selectizeInput(
          ns("exposure_duration"),
          "Exposure duration",
          choices = NULL,
          width = "100%",
          selected = character(0),
          multiple = TRUE,
          options = single_selectize_options(
            "Select or type to add an exposure duration",
            create = TRUE
          )
        ),
        selectizeInput(
          ns("averaging_period"),
          "Averaging period",
          choices = NULL,
          width = "100%",
          selected = character(0),
          multiple = TRUE,
          options = single_selectize_options(
            "Select or type to add an averaging period",
            create = TRUE
          )
        ),
        selectizeInput(
          ns("specific_locations"),
          bslib::tooltip(
            tags$span("Specific locations"),
            "Optional. Leave blank for a generally applicable guideline; choose one or more locations when the guideline is location-specific."
          ),
          choices = NULL,
          width = "100%",
          selected = character(0),
          multiple = TRUE,
          options = list(
            placeholder = "Applies at all locations unless selected"
          )
        ),
        fluidRow(
          column(
            6,
            dateInput(ns("valid_from"), "Valid from", value = Sys.Date())
          ),
          column(6, dateInput(ns("valid_to"), "Valid to", value = as.Date(NA)))
        ),
        fluidRow(
          column(
            6,
            selectInput(
              ns("review_status"),
              "Review status",
              choices = c(
                "draft",
                "reviewed",
                "approved",
                "retired",
                "superseded"
              )
            )
          ),
          column(
            6,
            checkboxInput(ns("active"), "Active", value = TRUE)
          )
        ),

        h4("Source"),
        textInput(ns("reference"), "Reference", width = "100%"),
        textInput(
          ns("source_document_title"),
          "Source document",
          width = "100%"
        ),
        textInput(ns("source_url"), "Source URL", width = "100%"),
        fluidRow(
          column(4, textInput(ns("source_page"), "Page", width = "100%")),
          column(4, textInput(ns("source_table"), "Table", width = "100%")),
          column(4, textInput(ns("source_section"), "Section", width = "100%"))
        ),
        fluidRow(
          column(
            6,
            dateInput(
              ns("source_effective_date"),
              "Effective",
              value = as.Date(NA)
            )
          ),
          column(
            6,
            dateInput(
              ns("source_retrieved_date"),
              "Retrieved",
              value = Sys.Date()
            )
          )
        ),
        textAreaInput(
          ns("general_notes"),
          "General notes",
          width = "100%",
          height = "70px"
        ),
        textAreaInput(
          ns("applicability_notes"),
          "Applicability notes",
          width = "100%",
          height = "70px"
        ),

        h4("Value Rule"),
        selectInput(
          ns("guideline_type"),
          "Type",
          choices = c(
            "Fixed upper value" = "constant_upper",
            "Fixed lower value" = "constant_lower",
            "Fixed range" = "constant_range",
            "One-input formula" = "single_input_formula",
            "Narrative/site-specific" = "narrative",
            "Advanced SQL scalar" = "sql_scalar"
          )
        ),
        uiOutput(ns("value_rule_hint")),
        uiOutput(ns("calculation_source_ui")),
        uiOutput(ns("guideline_value_units")),
        shinyjs::hidden(
          div(
            id = ns("fixed_value_section"),
            numericInput(ns("fixed_value"), "Value", value = NA_real_)
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("range_value_section"),
            fluidRow(
              column(
                6,
                numericInput(ns("lower_value"), "Lower", value = NA_real_)
              ),
              column(
                6,
                numericInput(ns("upper_value"), "Upper", value = NA_real_)
              )
            )
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("bound_section"),
            selectInput(
              ns("bound_code"),
              "Bound",
              choices = c("Upper limit" = "upper", "Lower limit" = "lower")
            )
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("formula_algorithm_section"),
            selectInput(
              ns("formula_algorithm"),
              "Formula",
              choices = c("Linear" = "linear", "Log-linear" = "log_linear")
            )
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("advanced_rule_options_section"),
            tags$details(
              open = FALSE,
              tags$summary(
                bslib::tooltip(
                  tags$span("Advanced rule handling"),
                  "Leave these alone unless the source guideline says to round, clamp, or order multiple rules for the same bound."
                )
              ),
              fluidRow(
                column(
                  6,
                  numericInput(
                    ns("rounding_digits"),
                    bslib::tooltip(
                      tags$span("Rounding digits"),
                      "Number of decimal places applied after the rule value is derived."
                    ),
                    value = NA_integer_
                  )
                ),
                column(
                  6,
                  selectInput(
                    ns("rounding_method"),
                    bslib::tooltip(
                      tags$span("Rounding method"),
                      "How the derived value is rounded when rounding digits are supplied."
                    ),
                    choices = c(
                      "None" = "none",
                      "Round" = "round",
                      "Floor" = "floor",
                      "Ceiling" = "ceiling"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  4,
                  selectInput(
                    ns("missing_input_policy"),
                    bslib::tooltip(
                      tags$span("Missing input"),
                      "Whether missing chemistry inputs should return no guideline value or stop with an error."
                    ),
                    choices = c("No value" = "no_value", "Error" = "error")
                  )
                ),
                column(
                  4,
                  numericInput(
                    ns("min_output_value"),
                    "Minimum output",
                    value = NA_real_
                  )
                ),
                column(
                  4,
                  numericInput(
                    ns("max_output_value"),
                    "Maximum output",
                    value = NA_real_
                  )
                )
              ),
              textInput(
                ns("precision_note"),
                bslib::tooltip(
                  tags$span("Precision note"),
                  "Optional note about rounding, significant figures, or source-document precision."
                ),
                width = "100%"
              )
            )
          )
        ),
        textAreaInput(
          ns("rule_note"),
          "Rule note",
          width = "100%",
          height = "60px"
        ),

        shinyjs::hidden(
          div(
            id = ns("rule_inputs_section"),
            tags$details(
              open = TRUE,
              tags$summary(
                bslib::tooltip(
                  tags$span("Rule inputs"),
                  "Rows identify sample results used to derive a calculated guideline. Formula and SQL scalar rules can use one or more chemistry inputs."
                )
              ),
              uiOutput(ns("rule_inputs_ui")),
              div(
                style = "display:flex; gap:8px; margin:8px 0;",
                bslib::tooltip(
                  actionButton(
                    ns("add_rule_input"),
                    "Add input",
                    class = "btn-sm"
                  ),
                  "Add another sample result that this rule needs."
                ),
                bslib::tooltip(
                  actionButton(
                    ns("remove_rule_input"),
                    "Remove last",
                    class = "btn-sm"
                  ),
                  "Remove the last sample input row."
                )
              )
            )
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("coefficients_section"),
            tags$details(
              open = TRUE,
              tags$summary("Coefficients"),
              div(
                style = "display:flex; gap:8px; margin:8px 0;",
                actionButton(
                  ns("fill_coefficients_template"),
                  "Template",
                  class = "btn-sm"
                )
              ),
              textAreaInput(
                ns("coefficients_text"),
                NULL,
                width = "100%",
                height = "100px"
              )
            )
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("narrative_values_section"),
            tags$details(
              open = TRUE,
              tags$summary(
                bslib::tooltip(
                  tags$span("Narrative values"),
                  "Structured non-numeric guidance, such as allowed change from background or condition-specific narrative limits."
                )
              ),
              textAreaInput(
                ns("narrative_values_text"),
                NULL,
                width = "100%",
                height = "150px",
                placeholder = paste(
                  "Paste rows from a spreadsheet. Required columns:",
                  paste(
                    c(
                      "value_code",
                      "condition_label",
                      "max_change_value",
                      "max_change_percent",
                      "change_unit",
                      "background_lower_bound",
                      "background_upper_bound",
                      "background_unit",
                      "duration_label",
                      "flow_condition",
                      "sort_order",
                      "note"
                    ),
                    collapse = ", "
                  )
                )
              )
            )
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("sql_scalar_section"),
            tags$details(
              open = TRUE,
              tags$summary(
                bslib::tooltip(
                  tags$span("SQL scalar"),
                  "Advanced rule type. The SQL must return one numeric value; use $1::integer as the sample_id when the value depends on sample chemistry."
                )
              ),
              fluidRow(
                column(
                  9,
                  bslib::input_code_editor(
                    ns("formula_sql"),
                    NULL,
                    width = "100%",
                    height = "350px",
                    language = "sql",
                    line_numbers = TRUE,
                    word_wrap = TRUE
                  ) |>
                    tagAppendAttributes(class = "guideline-sql-editor")
                ),
                column(
                  3,
                  div(
                    style = "display:flex; flex-direction:column; gap:8px;",
                    bslib::tooltip(
                      selectizeInput(
                        ns("sql_template"),
                        "Template",
                        choices = c(
                          "Fixed value" = "fixed",
                          "Sample chemistry" = "sample_inputs",
                          "Hardness helper" = "hardness"
                        ),
                        multiple = TRUE,
                        selected = character(0),
                        options = list(
                          maxItems = 1,
                          placeholder = "Choose a template..."
                        ),
                        width = "100%"
                      ),
                      "Replace the SQL editor contents with a starting template."
                    ),
                    bslib::tooltip(
                      actionButton(
                        ns("insert_sql_parameter"),
                        "Insert input",
                        class = "btn-sm"
                      ),
                      "Insert a SQL expression for one of the Rule inputs already listed above."
                    ),
                    bslib::tooltip(
                      actionButton(
                        ns("open_guideline_sql_help"),
                        "SQL helper",
                        class = "btn-sm"
                      ),
                      "Open examples and rules for writing database-driven SQL scalar guideline rules."
                    )
                  )
                )
              )
            )
          )
        ),

        splitLayout(
          cellWidths = c("50%", "50%"),
          bslib::input_task_button(
            ns("save_guideline"),
            label = "Save guideline"
          ),
          bslib::input_task_button(
            ns("test_guideline"),
            label = "Test saved guideline"
          )
        )
      ),

      div(
        DT::DTOutput(ns("guidelines_table")),
        br(),
        splitLayout(
          cellWidths = c("50%", "50%"),
          actionButton(
            ns("add_guideline"),
            "Add new guideline",
            class = "btn btn-primary"
          ),
          actionButton(
            ns("delete_guideline"),
            "Delete selected guideline",
            class = "btn btn-primary"
          )
        ),
        br(),
        h4("Selected Guideline Rules"),
        DT::DTOutput(ns("selected_rules_table")),
        br(),
        DT::DTOutput(ns("selected_inputs_table")),
        br(),
        uiOutput(ns("test_guideline_result"))
      )
    )
  )
}

addGuidelines <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- session$userData$AquaCache

    `%||%` <- function(x, y) {
      if (is.null(x) || !length(x) || all(is.na(x))) y else x
    }
    is_blank <- function(x) {
      is.null(x) ||
        !length(x) ||
        all(is.na(x)) ||
        !nzchar(trimws(as.character(x[[1]])))
    }
    text_value <- function(x) {
      if (is_blank(x)) "" else trimws(as.character(x[[1]]))
    }
    text_or_na <- function(x) {
      val <- text_value(x)
      if (nzchar(val)) val else NA_character_
    }
    text_default <- function(x, default) {
      val <- text_value(x)
      if (nzchar(val)) val else default
    }
    sql_text_value <- function(x) {
      if (is.null(x) || !length(x) || all(is.na(x))) {
        ""
      } else {
        as.character(x[[1]])
      }
    }
    clean_sql_scalar <- function(x) {
      gsub("(?m)[[:blank:]]+$", "", sql_text_value(x), perl = TRUE)
    }
    update_formula_sql <- function(value = "") {
      bslib::update_code_editor(
        "formula_sql",
        value = clean_sql_scalar(value),
        session = session
      )
    }
    normalize_code <- function(x) {
      val <- toupper(gsub("[^A-Za-z0-9]+", "-", text_value(x)))
      val <- gsub("(^-+|-+$)", "", val)
      if (!nzchar(val)) {
        paste0("GUIDELINE-", format(Sys.time(), "%Y%m%d%H%M%S"))
      } else {
        val
      }
    }
    normalize_input_code <- function(x, fallback = "input") {
      val <- tolower(gsub("[^A-Za-z0-9]+", "_", text_value(x)))
      val <- gsub("(^_+|_+$)", "", val)
      if (nzchar(val)) val else fallback
    }
    integer_or_na <- function(x) {
      if (is_blank(x)) {
        return(NA_integer_)
      }
      out <- suppressWarnings(as.integer(as.character(x[[1]])))
      if (is.na(out)) NA_integer_ else out
    }
    numeric_or_na <- function(x) {
      if (is_blank(x)) {
        return(NA_real_)
      }
      out <- suppressWarnings(as.numeric(as.character(x[[1]])))
      if (is.na(out)) NA_real_ else out
    }
    date_or_na <- function(x) {
      if (is.null(x) || !length(x) || is.na(x[[1]])) {
        return(as.Date(NA))
      }
      as.Date(x[[1]])
    }
    update_nullable_date_input <- function(input_id, value, default = NULL) {
      value <- date_or_na(value)
      if (is.na(value)) {
        if (is.null(default)) {
          session$sendInputMessage(input_id, list(value = ""))
        } else {
          updateDateInput(session, input_id, value = default)
        }
      } else {
        updateDateInput(session, input_id, value = value)
      }
    }
    bool_value <- function(x, default = FALSE) {
      if (is_blank(x)) {
        return(default)
      }
      val <- tolower(trimws(as.character(x[[1]])))
      if (val %in% c("true", "t", "1", "yes", "y")) {
        return(TRUE)
      }
      if (val %in% c("false", "f", "0", "no", "n")) {
        return(FALSE)
      }
      default
    }
    parse_id_vector <- function(x) {
      vals <- suppressWarnings(as.integer(as.character(x %||% character(0))))
      unique(vals[!is.na(vals)])
    }
    parse_id_csv <- function(x) {
      if (is_blank(x)) {
        return(integer(0))
      }
      vals <- unlist(
        strsplit(as.character(x[[1]]), ",", fixed = TRUE),
        use.names = FALSE
      )
      parse_id_vector(trimws(vals))
    }
    collapse_id_vector <- function(x) paste(parse_id_vector(x), collapse = ",")
    selected_or_empty_vector <- function(x) {
      vals <- parse_id_csv(x)
      if (length(vals)) as.character(vals) else character(0)
    }
    choice_values <- function(df, id_col, label_col) {
      if (is.null(df) || !nrow(df)) {
        return(character(0))
      }
      stats::setNames(as.character(df[[id_col]]), df[[label_col]])
    }
    lookup_id_from_text <- function(value, df, id_col, text_col) {
      value <- text_or_na(value)
      if (is.na(value) || is.null(df) || !nrow(df)) {
        return(NA_integer_)
      }
      match_row <- match(
        tolower(trimws(value)),
        tolower(trimws(as.character(df[[text_col]])))
      )
      if (is.na(match_row)) {
        NA_integer_
      } else {
        as.integer(df[[id_col]][[match_row]])
      }
    }
    resolve_lookup_id <- function(
      value,
      df,
      id_col,
      text_col,
      table_name,
      code_col,
      code_prefix
    ) {
      id <- integer_or_na(value)
      if (!is.na(id) && !is.null(df) && id %in% as.integer(df[[id_col]])) {
        return(id)
      }
      label <- text_or_na(value)
      if (is.na(label)) {
        return(NA_integer_)
      }
      existing_id <- lookup_id_from_text(label, df, id_col, text_col)
      if (!is.na(existing_id)) {
        return(existing_id)
      }
      resolved <- DBI::dbGetQuery(
        con,
        sprintf(
          "WITH existing AS (
             SELECT %1$s
             FROM %2$s
             WHERE lower(btrim(%3$s)) = lower(btrim($1))
             LIMIT 1
           ),
           inserted AS (
             INSERT INTO %2$s (%4$s, %3$s, sort_order)
             SELECT $2 || '_' || upper(left(md5($1), 10)), btrim($1), 800
             WHERE NOT EXISTS (SELECT 1 FROM existing)
             ON CONFLICT DO NOTHING
             RETURNING %1$s
           )
           SELECT %1$s FROM inserted
           UNION ALL
           SELECT %1$s FROM existing
           LIMIT 1",
          id_col,
          table_name,
          text_col,
          code_col
        ),
        params = list(label, code_prefix)
      )
      if (nrow(resolved)) {
        return(as.integer(resolved[[id_col]][[1]]))
      }

      resolved <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT %1$s
           FROM %2$s
           WHERE lower(btrim(%3$s)) = lower(btrim($1))
           LIMIT 1",
          id_col,
          table_name,
          text_col
        ),
        params = list(label)
      )
      if (nrow(resolved)) {
        return(as.integer(resolved[[id_col]][[1]]))
      }
      stop("Could not resolve reference value '", label, "'.", call. = FALSE)
    }
    resolve_jurisdiction_id <- function(value) {
      resolve_lookup_id(
        value,
        moduleData$jurisdictions,
        "jurisdiction_id",
        "jurisdiction_name",
        "criteria.guideline_jurisdictions",
        "jurisdiction_code",
        "JUR"
      )
    }
    resolve_protection_goal_id <- function(value) {
      resolve_lookup_id(
        value,
        moduleData$protection_goals,
        "protection_goal_id",
        "protection_goal_name",
        "criteria.guideline_protection_goals",
        "protection_goal_code",
        "GOAL"
      )
    }
    resolve_exposure_duration_id <- function(value) {
      resolve_lookup_id(
        value,
        moduleData$exposure_durations,
        "exposure_duration_id",
        "exposure_duration_name",
        "criteria.guideline_exposure_durations",
        "exposure_duration_code",
        "EXPOSURE"
      )
    }
    resolve_averaging_period_id <- function(value) {
      resolve_lookup_id(
        value,
        moduleData$averaging_periods,
        "averaging_period_id",
        "averaging_period_name",
        "criteria.guideline_averaging_periods",
        "averaging_period_code",
        "AVG"
      )
    }

    guideline_type_choices_for_operator <- function(operator) {
      switch(
        operator %||% "lte",
        lte = c(
          "Fixed upper value" = "constant_upper",
          "One-input formula" = "single_input_formula",
          "Advanced SQL scalar" = "sql_scalar"
        ),
        gte = c(
          "Fixed lower value" = "constant_lower",
          "One-input formula" = "single_input_formula",
          "Advanced SQL scalar" = "sql_scalar"
        ),
        range = c("Fixed range" = "constant_range"),
        eq = c("Fixed exact value" = "constant_range"),
        narrative = c("Narrative/site-specific" = "narrative"),
        c("Fixed upper value" = "constant_upper")
      )
    }
    default_type_for_operator <- function(operator) {
      unname(guideline_type_choices_for_operator(operator)[[1]])
    }
    bound_for_operator <- function(operator) {
      switch(operator %||% "lte", gte = "lower", "upper")
    }

    input_cols <- c(
      "input_code",
      "input_name",
      "input_source",
      "parameter_id",
      "matrix_state_id",
      "sample_fraction_id",
      "result_speciation_id",
      "result_type",
      "result_type_preference",
      "aggregate_method",
      "required",
      "allow_condition_value",
      "lower_calibrated_bound",
      "upper_calibrated_bound",
      "bounds_action",
      "note"
    )
    coef_cols <- c("coefficient_name", "coefficient_value", "note")
    narrative_cols <- c(
      "value_code",
      "condition_label",
      "max_change_value",
      "max_change_percent",
      "change_unit",
      "background_lower_bound",
      "background_upper_bound",
      "background_unit",
      "duration_label",
      "flow_condition",
      "sort_order",
      "note"
    )

    empty_table_text <- function(cols) paste(cols, collapse = "\t")
    format_table_text <- function(df, cols) {
      if (is.null(df) || !nrow(df)) {
        return(empty_table_text(cols))
      }
      for (col in cols) {
        if (!col %in% names(df)) df[[col]] <- NA
      }
      rows <- apply(df[, cols, drop = FALSE], 1, function(row) {
        paste(
          vapply(
            row,
            function(value) {
              if (is.na(value)) "" else as.character(value)
            },
            character(1)
          ),
          collapse = "\t"
        )
      })
      paste(c(paste(cols, collapse = "\t"), rows), collapse = "\n")
    }
    parse_table_text <- function(x, required_cols, label) {
      txt <- text_value(x)
      if (!nzchar(txt)) {
        return(data.frame(stringsAsFactors = FALSE))
      }
      sep <- if (grepl("\t", txt, fixed = TRUE)) "\t" else ","
      df <- tryCatch(
        utils::read.table(
          text = txt,
          header = TRUE,
          sep = sep,
          stringsAsFactors = FALSE,
          na.strings = c("", "NA", "NULL"),
          quote = "\"",
          comment.char = "",
          fill = TRUE
        ),
        error = function(e) {
          stop(
            "Could not read ",
            label,
            ": ",
            conditionMessage(e),
            call. = FALSE
          )
        }
      )
      names(df) <- trimws(names(df))
      missing_cols <- setdiff(required_cols, names(df))
      if (length(missing_cols)) {
        stop(
          label,
          " is missing column(s): ",
          paste(missing_cols, collapse = ", "),
          call. = FALSE
        )
      }
      df
    }

    moduleData <- reactiveValues()
    selected_guideline_id <- reactiveVal(NA_integer_)
    rule_input_count <- reactiveVal(1L)
    rule_input_seed <- reactiveVal(NULL)

    selectize_single_options <- list(maxItems = 1)
    selected_or_empty <- function(x) {
      if (
        is.null(x) ||
          !length(x) ||
          is.na(x[[1]]) ||
          !nzchar(as.character(x[[1]]))
      ) {
        character(0)
      } else {
        as.character(x[[1]])
      }
    }
    choices_with_any <- function(choices, any_label = "Any") {
      c(stats::setNames("", any_label), choices)
    }

    parameter_choice_labels <- function() {
      if (is.null(moduleData$parameters) || !nrow(moduleData$parameters)) {
        return(character(0))
      }
      labels <- moduleData$parameters$param_name
      units <- moduleData$parameters$unit_default
      has_units <- !is.na(units) & nzchar(units)
      labels[has_units] <- paste0(
        labels[has_units],
        " (",
        units[has_units],
        ")"
      )
      labels
    }
    parameter_choices <- function() {
      if (is.null(moduleData$parameters) || !nrow(moduleData$parameters)) {
        return(character(0))
      }
      stats::setNames(
        as.character(moduleData$parameters$parameter_id),
        parameter_choice_labels()
      )
    }
    hardness_helper_choice_value <- "helper:hardness_preferred"
    parameter_choices_with_helpers <- function() {
      c(
        "Hardness helper (preferred hardness as CaCO3)" = hardness_helper_choice_value,
        parameter_choices()
      )
    }
    is_hardness_helper_choice <- function(value) {
      identical(text_value(value), hardness_helper_choice_value)
    }
    hardness_parameter_id <- function() {
      find_parameter_id("^hardness$")
    }
    is_hardness_parameter <- function(parameter_id) {
      parameter_name <- tolower(parameter_name_for_id(parameter_id))
      nzchar(parameter_name) && grepl("\\bhardness\\b", parameter_name)
    }
    is_hardness_rule_input <- function(row) {
      identical(
        text_default(row$input_source[[1]], "sample_result"),
        "hardness_helper"
      ) ||
        is_hardness_parameter(row$parameter_id[[1]])
    }
    matrix_state_choices <- function() {
      choice_values(
        moduleData$matrix_states,
        "matrix_state_id",
        "matrix_state_name"
      )
    }
    sample_fraction_choices <- function() {
      choice_values(
        moduleData$sample_fractions,
        "sample_fraction_id",
        "sample_fraction"
      )
    }
    speciation_choices <- function() {
      choice_values(
        moduleData$result_speciations,
        "result_speciation_id",
        "result_speciation"
      )
    }
    result_type_choices <- function() {
      choice_values(moduleData$result_types, "result_type_id", "result_type")
    }
    parameter_name_for_id <- function(parameter_id) {
      parameter_id <- integer_or_na(parameter_id)
      if (
        is.na(parameter_id) ||
          is.null(moduleData$parameters) ||
          !nrow(moduleData$parameters)
      ) {
        return("")
      }
      row <- moduleData$parameters[
        moduleData$parameters$parameter_id == parameter_id,
        ,
        drop = FALSE
      ]
      if (nrow(row)) as.character(row$param_name[[1]]) else ""
    }
    default_rule_input_name <- function(
      parameter_id,
      input_source = "sample_result"
    ) {
      if (identical(text_value(input_source), "hardness_helper")) {
        return("Hardness as CaCO3 (preferred)")
      }
      parameter_name_for_id(parameter_id)
    }
    default_rule_input_code <- function(
      parameter_id,
      input_source = "sample_result",
      fallback = "input"
    ) {
      if (identical(text_value(input_source), "hardness_helper")) {
        return("hardness_mg_l_caco3")
      }
      parameter_name <- parameter_name_for_id(parameter_id)
      parameter_key <- tolower(parameter_name)
      if (grepl("dissolved organic carbon|\\bdoc\\b", parameter_key)) {
        return("doc_mg_l")
      }
      if (grepl("^ph$|\\bpH\\b", parameter_name, ignore.case = FALSE)) {
        return("ph")
      }
      if (grepl("temperature", parameter_key)) {
        return("temperature_c")
      }
      if (grepl("hardness", parameter_key)) {
        return("hardness_mg_l_caco3")
      }
      normalize_input_code(parameter_name, fallback = fallback)
    }
    make_unique_input_codes <- function(codes) {
      out <- character(length(codes))
      seen <- integer(0)
      names(seen) <- character(0)
      for (i in seq_along(codes)) {
        base <- normalize_input_code(codes[[i]], fallback = paste0("input_", i))
        count <- if (base %in% names(seen)) seen[[base]] else 0L
        count <- count + 1L
        seen[[base]] <- count
        out[[i]] <- if (count == 1L) base else paste0(base, "_", count)
      }
      out
    }
    parameter_unit_for_id <- function(
      parameter_id,
      matrix_state_id = input$matrix_state
    ) {
      parameter_id <- integer_or_na(parameter_id)
      matrix_state_id <- integer_or_na(matrix_state_id)
      if (is.na(parameter_id)) {
        return("")
      }
      unit_value <- tryCatch(
        DBI::dbGetQuery(
          con,
          "SELECT public.get_parameter_unit_name($1, $2) AS unit_name",
          params = list(parameter_id, matrix_state_id)
        )$unit_name[[1]],
        error = function(e) NA_character_
      )
      if (!is.na(unit_value) && nzchar(unit_value)) {
        return(as.character(unit_value))
      }
      row <- moduleData$parameters[
        moduleData$parameters$parameter_id == parameter_id,
        ,
        drop = FALSE
      ]
      if (nrow(row) && !is.na(row$unit_default[[1]])) {
        as.character(row$unit_default[[1]])
      } else {
        ""
      }
    }
    matrix_state_row_for_id <- function(matrix_state_id) {
      matrix_state_id <- integer_or_na(matrix_state_id)
      if (
        is.na(matrix_state_id) ||
          is.null(moduleData$matrix_states) ||
          !nrow(moduleData$matrix_states)
      ) {
        return(data.frame())
      }
      moduleData$matrix_states[
        moduleData$matrix_states$matrix_state_id == matrix_state_id,
        ,
        drop = FALSE
      ]
    }
    matrix_state_code_for_id <- function(matrix_state_id) {
      row <- matrix_state_row_for_id(matrix_state_id)
      if (nrow(row)) as.character(row$matrix_state_code[[1]]) else NA_character_
    }
    matrix_state_name_for_id <- function(matrix_state_id) {
      row <- matrix_state_row_for_id(matrix_state_id)
      if (nrow(row)) as.character(row$matrix_state_name[[1]]) else ""
    }
    parameter_unit_column_for_matrix <- function(matrix_state_id) {
      code <- matrix_state_code_for_id(matrix_state_id)
      if (is.na(code)) {
        return(NA_character_)
      }
      switch(
        code,
        liquid = "units_liquid",
        solid = "units_solid",
        gas = "units_gas",
        NA_character_
      )
    }
    parameter_unit_id_for_id <- function(parameter_id, matrix_state_id) {
      parameter_id <- integer_or_na(parameter_id)
      matrix_state_id <- integer_or_na(matrix_state_id)
      if (is.na(parameter_id) || is.na(matrix_state_id)) {
        return(NA_integer_)
      }
      value <- tryCatch(
        DBI::dbGetQuery(
          con,
          "SELECT public.get_parameter_unit_id($1, $2) AS unit_id",
          params = list(parameter_id, matrix_state_id)
        )$unit_id[[1]],
        error = function(e) NA_integer_
      )
      if (is.na(value)) NA_integer_ else as.integer(value)
    }
    parameter_unit_name_exact <- function(parameter_id, matrix_state_id) {
      unit_id <- parameter_unit_id_for_id(parameter_id, matrix_state_id)
      if (
        is.na(unit_id) || is.null(moduleData$units) || !nrow(moduleData$units)
      ) {
        return("")
      }
      row <- moduleData$units[
        moduleData$units$unit_id == unit_id,
        ,
        drop = FALSE
      ]
      if (nrow(row)) as.character(row$unit_name[[1]]) else ""
    }
    selected_parameter_unit_status <- function() {
      parameter_id <- integer_or_na(input$parameter_id)
      matrix_state_id <- integer_or_na(input$matrix_state)
      parameter_name <- parameter_name_for_id(parameter_id)
      matrix_state_name <- matrix_state_name_for_id(matrix_state_id)
      unit_id <- parameter_unit_id_for_id(parameter_id, matrix_state_id)
      unit_name <- parameter_unit_name_exact(parameter_id, matrix_state_id)
      list(
        parameter_id = parameter_id,
        matrix_state_id = matrix_state_id,
        parameter_name = parameter_name,
        matrix_state_name = matrix_state_name,
        unit_id = unit_id,
        unit_name = unit_name,
        has_selection = !is.na(parameter_id) && !is.na(matrix_state_id),
        has_unit = !is.na(unit_id)
      )
    }
    parameter_group_unit_distribution <- function(
      parameter_id,
      matrix_state_id
    ) {
      parameter_id <- integer_or_na(parameter_id)
      matrix_state_id <- integer_or_na(matrix_state_id)
      if (is.na(parameter_id) || is.na(matrix_state_id)) {
        return(data.frame())
      }
      DBI::dbGetQuery(
        con,
        "WITH selected_groups AS (
           SELECT DISTINCT group_id
           FROM public.parameter_relationships
           WHERE parameter_id = $1
         ),
         group_parameters AS (
           SELECT DISTINCT p.parameter_id
           FROM public.parameters p
           JOIN public.parameter_relationships pr
             ON pr.parameter_id = p.parameter_id
           JOIN selected_groups sg
             ON sg.group_id = pr.group_id
         )
         SELECT
           COALESCE(u.unit_name, 'No unit assigned') AS unit_name,
           count(*)::integer AS parameter_count
         FROM group_parameters gp
         JOIN public.parameters p
           ON p.parameter_id = gp.parameter_id
         LEFT JOIN public.units u
           ON u.unit_id = public.get_parameter_unit_id(p.parameter_id, $2)
         GROUP BY COALESCE(u.unit_name, 'No unit assigned')
         ORDER BY parameter_count DESC, unit_name",
        params = list(parameter_id, matrix_state_id)
      )
    }
    parameter_requires_speciation <- function(parameter_id) {
      if (is_hardness_helper_choice(parameter_id)) {
        return(FALSE)
      }
      parameter_id <- integer_or_na(parameter_id)
      if (
        is.na(parameter_id) ||
          is.null(moduleData$parameters) ||
          !nrow(moduleData$parameters) ||
          !"result_speciation" %in% names(moduleData$parameters)
      ) {
        return(FALSE)
      }
      row <- moduleData$parameters[
        moduleData$parameters$parameter_id == parameter_id,
        ,
        drop = FALSE
      ]
      nrow(row) && isTRUE(row$result_speciation[[1]])
    }
    parameter_requires_fraction <- function(parameter_id) {
      if (is_hardness_helper_choice(parameter_id)) {
        return(FALSE)
      }
      parameter_id <- integer_or_na(parameter_id)
      if (
        is.na(parameter_id) ||
          is.null(moduleData$parameters) ||
          !nrow(moduleData$parameters) ||
          !"sample_fraction" %in% names(moduleData$parameters)
      ) {
        return(FALSE)
      }
      row <- moduleData$parameters[
        moduleData$parameters$parameter_id == parameter_id,
        ,
        drop = FALSE
      ]
      nrow(row) && isTRUE(row$sample_fraction[[1]])
    }
    parameter_flag_value <- function(parameter_id, column) {
      parameter_id <- integer_or_na(parameter_id)
      if (
        is.na(parameter_id) ||
          is.null(moduleData$parameters) ||
          !nrow(moduleData$parameters) ||
          !(column %in% names(moduleData$parameters))
      ) {
        return(FALSE)
      }
      row <- moduleData$parameters[
        moduleData$parameters$parameter_id == parameter_id,
        ,
        drop = FALSE
      ]
      nrow(row) && isTRUE(row[[column]][[1]])
    }
    update_result_speciation_visibility <- function(
      parameter_id = input$parameter_id
    ) {
      if (parameter_requires_speciation(parameter_id)) {
        shinyjs::show(id = "result_speciation_section", anim = FALSE)
      } else {
        updateSelectizeInput(
          session,
          "result_speciation",
          selected = character(0)
        )
        shinyjs::hide(id = "result_speciation_section", anim = FALSE)
      }
    }
    blank_rule_input_rows <- function(n = 1L) {
      data.frame(
        input_code = rep("", n),
        input_name = rep("", n),
        input_source = rep("sample_result", n),
        parameter_id = rep(NA_integer_, n),
        matrix_state_id = rep(NA_integer_, n),
        sample_fraction_id = rep(NA_integer_, n),
        result_speciation_id = rep(NA_integer_, n),
        result_type = rep(NA_integer_, n),
        result_type_preference = rep("", n),
        aggregate_method = rep("single", n),
        required = rep(TRUE, n),
        allow_condition_value = rep(FALSE, n),
        lower_calibrated_bound = rep(NA_real_, n),
        upper_calibrated_bound = rep(NA_real_, n),
        bounds_action = rep("flag", n),
        note = rep("", n),
        stringsAsFactors = FALSE
      )
    }
    normalize_rule_input_rows <- function(df) {
      if (is.null(df) || !nrow(df)) {
        return(blank_rule_input_rows(1L))
      }
      for (col in input_cols) {
        if (!col %in% names(df)) {
          df[[col]] <- blank_rule_input_rows(1L)[[col]][[1]]
        }
      }
      df <- df[, input_cols, drop = FALSE]
      df$input_code <- as.character(df$input_code %||% "")
      df$input_name <- as.character(df$input_name %||% "")
      df$input_source <- as.character(df$input_source %||% "sample_result")
      df$input_source[
        is.na(df$input_source) | !nzchar(df$input_source)
      ] <- "sample_result"
      df$result_type_preference <- as.character(
        df$result_type_preference %||% ""
      )
      df$aggregate_method[
        is.na(df$aggregate_method) | !nzchar(df$aggregate_method)
      ] <- "single"
      df$bounds_action[
        is.na(df$bounds_action) | !nzchar(df$bounds_action)
      ] <- "flag"
      df$required <- vapply(df$required, bool_value, logical(1), default = TRUE)
      df$allow_condition_value <- vapply(
        df$allow_condition_value,
        bool_value,
        logical(1),
        default = FALSE
      )
      df$note <- as.character(df$note %||% "")
      df
    }
    set_rule_input_rows <- function(df) {
      df <- normalize_rule_input_rows(df)
      rule_input_seed(df)
      rule_input_count(nrow(df))
    }
    rule_input_id <- function(field, row) paste0("rule_input_", field, "_", row)
    collect_rule_input_rows <- function(include_empty = FALSE) {
      n <- rule_input_count()
      if (is.null(n) || !n) {
        return(blank_rule_input_rows(0L))
      }
      rows <- lapply(seq_len(n), function(i) {
        parameter_choice <- input[[rule_input_id("parameter", i)]]
        input_source <- if (is_hardness_helper_choice(parameter_choice)) {
          "hardness_helper"
        } else {
          "sample_result"
        }
        parameter_id <- if (identical(input_source, "hardness_helper")) {
          hardness_parameter_id()
        } else {
          integer_or_na(parameter_choice)
        }
        matrix_state_id <- integer_or_na(input[[rule_input_id(
          "matrix_state",
          i
        )]])
        if (is.na(matrix_state_id)) {
          matrix_state_id <- integer_or_na(input$matrix_state)
        }
        sample_fraction_id <- if (
          identical(input_source, "hardness_helper") ||
            !parameter_requires_fraction(parameter_choice)
        ) {
          NA_integer_
        } else {
          integer_or_na(input[[rule_input_id("sample_fraction", i)]])
        }
        result_speciation_id <- if (
          identical(input_source, "hardness_helper") ||
            !parameter_requires_speciation(parameter_choice)
        ) {
          NA_integer_
        } else {
          integer_or_na(input[[rule_input_id("speciation", i)]])
        }
        input_name <- default_rule_input_name(parameter_id, input_source)
        input_code <- default_rule_input_code(
          parameter_id,
          input_source,
          fallback = paste0("input_", i)
        )
        data.frame(
          input_code = input_code,
          input_name = input_name,
          input_source = input_source,
          parameter_id = parameter_id,
          matrix_state_id = matrix_state_id,
          sample_fraction_id = sample_fraction_id,
          result_speciation_id = result_speciation_id,
          result_type = NA_integer_,
          result_type_preference = collapse_id_vector(
            input[[rule_input_id("result_type_preference", i)]]
          ),
          aggregate_method = text_default(
            input[[rule_input_id("aggregate_method", i)]],
            "single"
          ),
          required = isTRUE(input[[rule_input_id("required", i)]]),
          allow_condition_value = isTRUE(input[[rule_input_id(
            "allow_condition_value",
            i
          )]]),
          lower_calibrated_bound = numeric_or_na(input[[rule_input_id(
            "lower_calibrated_bound",
            i
          )]]),
          upper_calibrated_bound = numeric_or_na(input[[rule_input_id(
            "upper_calibrated_bound",
            i
          )]]),
          bounds_action = text_default(
            input[[rule_input_id("bounds_action", i)]],
            "flag"
          ),
          note = text_value(input[[rule_input_id("note", i)]]),
          stringsAsFactors = FALSE
        )
      })
      df <- do.call(rbind, rows)
      df$input_code <- make_unique_input_codes(df$input_code)
      if (include_empty) {
        return(df)
      }
      has_content <- !is.na(df$parameter_id)
      df[has_content, , drop = FALSE]
    }

    load_guidelines <- function() {
      DBI::dbGetQuery(
        con,
        "WITH rule_summary AS (
           SELECT guideline_id,
                  count(*)::integer AS rule_count,
                  string_agg(COALESCE(bound_code, 'no-bound') || ':' || algorithm_code, ', ' ORDER BY rule_priority, rule_id) AS rules
           FROM criteria.guideline_value_rules
           GROUP BY guideline_id
         ),
         fr AS (
           SELECT gf.guideline_id,
                  string_agg(sf.sample_fraction, ', ' ORDER BY sf.sample_fraction) AS fractions,
                  string_agg(gf.fraction_id::text, ',' ORDER BY gf.fraction_id::text) AS fraction_ids
           FROM criteria.guidelines_fractions gf
           JOIN discrete.sample_fractions sf ON sf.sample_fraction_id = gf.fraction_id
           GROUP BY gf.guideline_id
         ),
         mt AS (
           SELECT gm.guideline_id,
                  string_agg(mt.media_type, ', ' ORDER BY mt.media_type) AS media_types,
                  string_agg(gm.media_id::text, ',' ORDER BY gm.media_id::text) AS media_ids
           FROM criteria.guidelines_media_types gm
           JOIN public.media_types mt ON mt.media_id = gm.media_id
           GROUP BY gm.guideline_id
         ),
         loc AS (
           SELECT gl.guideline_id,
                  string_agg(
                    concat_ws(' - ', l.location_code, l.name),
                    ', ' ORDER BY l.location_code, l.name
                  ) AS locations,
                  string_agg(gl.location_id::text, ',' ORDER BY l.location_code, l.name) AS location_ids
           FROM criteria.guideline_locations gl
           JOIN public.locations l ON l.location_id = gl.location_id
           WHERE gl.active
           GROUP BY gl.guideline_id
         )
         SELECT g.guideline_id, g.guideline_code, g.guideline_name,
                gp.publisher_name AS publisher, gs.series_name AS series,
                p.param_name AS parameter,
                public.get_parameter_unit_name(g.parameter_id, g.matrix_state_id) AS units,
                ms.matrix_state_name AS matrix_state, fr.fractions, mt.media_types,
                loc.locations,
                rs.result_speciation AS speciation, g.comparison_operator_code,
                gj.jurisdiction_name AS jurisdiction,
                gpg.protection_goal_name AS protection_goal,
                ged.exposure_duration_name AS exposure_duration,
                gap.averaging_period_name AS averaging_period,
                g.valid_from, g.valid_to, g.review_status, g.active,
                COALESCE(rule_summary.rule_count, 0) AS rule_count,
                rule_summary.rules, g.publisher_id, g.series_id, g.parameter_id,
                g.matrix_state_id, g.result_speciation_id, fr.fraction_ids,
                mt.media_ids, loc.location_ids, g.reference, g.general_notes, g.applicability_notes,
                gjl.jurisdiction_level_name AS jurisdiction_level,
                g.jurisdiction_id,
                g.protection_goal_id, g.exposure_duration_id, g.averaging_period_id,
                g.source_document_title, g.source_url,
                g.source_page, g.source_table, g.source_section,
                g.source_effective_date, g.source_retrieved_date
         FROM criteria.guidelines g
         LEFT JOIN criteria.guideline_publishers gp ON gp.publisher_id = g.publisher_id
         LEFT JOIN criteria.guideline_series gs ON gs.series_id = g.series_id
         LEFT JOIN criteria.guideline_jurisdictions gj ON gj.jurisdiction_id = g.jurisdiction_id
         LEFT JOIN criteria.guideline_jurisdiction_levels gjl ON gjl.jurisdiction_level_id = gj.jurisdiction_level_id
         LEFT JOIN criteria.guideline_protection_goals gpg ON gpg.protection_goal_id = g.protection_goal_id
         LEFT JOIN criteria.guideline_exposure_durations ged ON ged.exposure_duration_id = g.exposure_duration_id
         LEFT JOIN criteria.guideline_averaging_periods gap ON gap.averaging_period_id = g.averaging_period_id
         JOIN public.parameters p ON p.parameter_id = g.parameter_id
         LEFT JOIN public.matrix_states ms ON ms.matrix_state_id = g.matrix_state_id
         LEFT JOIN discrete.result_speciations rs ON rs.result_speciation_id = g.result_speciation_id
         LEFT JOIN rule_summary ON rule_summary.guideline_id = g.guideline_id
         LEFT JOIN fr ON fr.guideline_id = g.guideline_id
         LEFT JOIN mt ON mt.guideline_id = g.guideline_id
         LEFT JOIN loc ON loc.guideline_id = g.guideline_id
         ORDER BY g.guideline_code, g.guideline_name"
      )
    }

    load_reference_data <- function() {
      moduleData$guidelines <- load_guidelines()
      moduleData$publishers <- DBI::dbGetQuery(
        con,
        "SELECT publisher_id, publisher_name FROM criteria.guideline_publishers ORDER BY publisher_name"
      )
      moduleData$series <- DBI::dbGetQuery(
        con,
        "SELECT series_id, series_name, publisher_id FROM criteria.guideline_series ORDER BY series_name"
      )
      moduleData$matrix_states <- DBI::dbGetQuery(
        con,
        "SELECT matrix_state_id, matrix_state_code, matrix_state_name FROM public.matrix_states ORDER BY matrix_state_name"
      )
      moduleData$media_types <- DBI::dbGetQuery(
        con,
        "SELECT media_id, media_type, default_matrix_state_id FROM public.media_types ORDER BY media_type"
      )
      moduleData$locations <- DBI::dbGetQuery(
        con,
        "SELECT location_id,
                concat_ws(' - ', location_code, name) AS location_label
         FROM public.locations
         ORDER BY location_code, name"
      )
      moduleData$parameters <- DBI::dbGetQuery(
        con,
        "SELECT p.parameter_id, p.param_name,
                public.get_parameter_unit_name(p.parameter_id, NULL::integer) AS unit_default,
                p.units_liquid, ul.unit_name AS unit_liquid_name,
                p.units_solid, us.unit_name AS unit_solid_name,
                p.units_gas, ug.unit_name AS unit_gas_name,
                p.result_speciation, p.sample_fraction
         FROM public.parameters p
         LEFT JOIN public.units ul ON ul.unit_id = p.units_liquid
         LEFT JOIN public.units us ON us.unit_id = p.units_solid
         LEFT JOIN public.units ug ON ug.unit_id = p.units_gas
         ORDER BY p.param_name"
      )
      moduleData$units <- DBI::dbGetQuery(
        con,
        "SELECT unit_id, unit_name
         FROM public.units
         ORDER BY unit_name"
      )
      moduleData$sample_fractions <- DBI::dbGetQuery(
        con,
        "SELECT sample_fraction_id, sample_fraction FROM discrete.sample_fractions ORDER BY sample_fraction"
      )
      moduleData$result_speciations <- DBI::dbGetQuery(
        con,
        "SELECT result_speciation_id, result_speciation FROM discrete.result_speciations ORDER BY result_speciation"
      )
      moduleData$result_types <- DBI::dbGetQuery(
        con,
        "SELECT result_type_id, result_type FROM discrete.result_types ORDER BY result_type"
      )
      moduleData$jurisdictions <- DBI::dbGetQuery(
        con,
        "SELECT jurisdiction_id, jurisdiction_name
         FROM criteria.guideline_jurisdictions
         WHERE active
         ORDER BY sort_order, jurisdiction_name"
      )
      moduleData$protection_goals <- DBI::dbGetQuery(
        con,
        "SELECT protection_goal_id, protection_goal_name
         FROM criteria.guideline_protection_goals
         WHERE active
         ORDER BY sort_order, protection_goal_name"
      )
      moduleData$exposure_durations <- DBI::dbGetQuery(
        con,
        "SELECT exposure_duration_id, exposure_duration_name
         FROM criteria.guideline_exposure_durations
         WHERE active
         ORDER BY sort_order, exposure_duration_name"
      )
      moduleData$averaging_periods <- DBI::dbGetQuery(
        con,
        "SELECT averaging_period_id, averaging_period_name
         FROM criteria.guideline_averaging_periods
         WHERE active
         ORDER BY sort_order, averaging_period_name"
      )
      moduleData$operators <- DBI::dbGetQuery(
        con,
        "SELECT operator_code, operator_name FROM criteria.guideline_comparison_operators ORDER BY operator_code"
      )
    }

    update_choices <- function(clear = FALSE) {
      selected <- if (isTRUE(clear)) character(0) else NULL
      shiny::isolate({
        updateSelectizeInput(
          session,
          "publisher",
          choices = choice_values(
            moduleData$publishers,
            "publisher_id",
            "publisher_name"
          ),
          selected = selected,
          server = TRUE
        )
        updateSelectizeInput(
          session,
          "series",
          choices = choice_values(
            moduleData$series,
            "series_id",
            "series_name"
          ),
          selected = selected,
          server = TRUE
        )
        updateSelectizeInput(
          session,
          "parameter_id",
          choices = parameter_choices(),
          selected = selected,
          server = TRUE
        )
        updateSelectizeInput(
          session,
          "matrix_state",
          choices = choice_values(
            moduleData$matrix_states,
            "matrix_state_id",
            "matrix_state_name"
          ),
          selected = selected,
          server = TRUE
        )
        updateSelectizeInput(
          session,
          "result_speciation",
          choices = choice_values(
            moduleData$result_speciations,
            "result_speciation_id",
            "result_speciation"
          ),
          selected = selected,
          server = TRUE
        )
        updateSelectizeInput(
          session,
          "sample_fraction",
          choices = choice_values(
            moduleData$sample_fractions,
            "sample_fraction_id",
            "sample_fraction"
          ),
          selected = selected,
          server = TRUE
        )
        updateSelectizeInput(
          session,
          "media_type",
          choices = choice_values(
            moduleData$media_types,
            "media_id",
            "media_type"
          ),
          selected = selected,
          server = TRUE
        )
        updateSelectizeInput(
          session,
          "specific_locations",
          choices = choice_values(
            moduleData$locations,
            "location_id",
            "location_label"
          ),
          selected = selected,
          server = TRUE
        )
        updateSelectizeInput(
          session,
          "jurisdiction",
          choices = choice_values(
            moduleData$jurisdictions,
            "jurisdiction_id",
            "jurisdiction_name"
          ),
          selected = selected,
          server = TRUE
        )
        updateSelectizeInput(
          session,
          "protection_goal",
          choices = choice_values(
            moduleData$protection_goals,
            "protection_goal_id",
            "protection_goal_name"
          ),
          selected = selected,
          server = TRUE
        )
        updateSelectizeInput(
          session,
          "exposure_duration",
          choices = choice_values(
            moduleData$exposure_durations,
            "exposure_duration_id",
            "exposure_duration_name"
          ),
          selected = selected,
          server = TRUE
        )
        updateSelectizeInput(
          session,
          "averaging_period",
          choices = choice_values(
            moduleData$averaging_periods,
            "averaging_period_id",
            "averaging_period_name"
          ),
          selected = selected,
          server = TRUE
        )
        updateSelectInput(
          session,
          "comparison_operator",
          choices = stats::setNames(
            moduleData$operators$operator_code,
            paste(
              moduleData$operators$operator_code,
              moduleData$operators$operator_name,
              sep = " - "
            )
          ),
          selected = if (isTRUE(clear)) {
            "lte"
          } else {
            input$comparison_operator %||% "lte"
          }
        )
      })
    }
    update_guideline_type_choices <- function(operator, selected = NULL) {
      choices <- guideline_type_choices_for_operator(operator)
      selected <- selected %||% default_type_for_operator(operator)
      if (!selected %in% unname(choices)) {
        selected <- default_type_for_operator(operator)
      }
      updateSelectInput(
        session,
        "guideline_type",
        choices = choices,
        selected = selected
      )
      if (operator %in% c("lte", "gte", "eq")) {
        updateSelectInput(
          session,
          "bound_code",
          selected = bound_for_operator(operator)
        )
      }
      update_rule_field_visibility(selected)
      selected
    }

    load_reference_data()
    session$onFlushed(
      function() {
        update_choices(clear = TRUE)
        set_rule_input_rows(blank_rule_input_rows(1L))
        update_result_speciation_visibility(character(0))
        updateTextAreaInput(
          session,
          "coefficients_text",
          value = empty_table_text(coef_cols)
        )
        updateTextAreaInput(
          session,
          "narrative_values_text",
          value = empty_table_text(narrative_cols)
        )
        update_guideline_type_choices("lte", "constant_upper")
      },
      once = TRUE
    )

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = con,
        module_id = "addGuidelines"
      )
    })

    output$parameter_unit_status <- renderUI({
      status <- selected_parameter_unit_status()
      if (!isTRUE(status$has_selection)) {
        return(NULL)
      }

      if (isTRUE(status$has_unit)) {
        return(tags$div(
          class = "text-muted",
          style = "font-size:0.9em; margin-top:-8px; margin-bottom:8px;",
          paste0(
            "Units for ",
            status$parameter_name,
            ", ",
            status$matrix_state_name,
            ": ",
            status$unit_name
          )
        ))
      }

      tags$div(
        class = "alert alert-warning",
        style = "padding:8px; margin-top:-4px; margin-bottom:8px;",
        tags$div(
          paste0(
            "No units are assigned for ",
            status$parameter_name,
            " in ",
            status$matrix_state_name,
            ". Add units before saving this guideline."
          )
        ),
        actionButton(
          ns("open_add_parameter_unit"),
          "Add units",
          class = "btn btn-sm btn-warning",
          style = "margin-top:6px;"
        )
      )
    })

    output$guideline_value_units <- renderUI({
      status <- selected_parameter_unit_status()
      if (!isTRUE(status$has_selection)) {
        return(NULL)
      }

      if (isTRUE(status$has_unit)) {
        return(tags$div(
          class = "text-muted",
          style = "margin-bottom:8px;",
          tags$strong("Guideline value units: "),
          status$unit_name
        ))
      }

      tags$div(
        class = "alert alert-warning",
        style = "padding:8px; margin-bottom:8px;",
        paste0(
          "Guideline values cannot be saved until units are assigned for ",
          status$parameter_name,
          " in ",
          status$matrix_state_name,
          "."
        )
      )
    })

    observeEvent(
      input$open_add_parameter_unit,
      {
        status <- selected_parameter_unit_status()
        req(status$has_selection)

        distribution <- parameter_group_unit_distribution(
          status$parameter_id,
          status$matrix_state_id
        )
        distribution_ui <- if (nrow(distribution)) {
          tags$table(
            class = "table table-sm",
            tags$thead(tags$tr(tags$th("Unit"), tags$th("Parameters"))),
            tags$tbody(lapply(seq_len(nrow(distribution)), function(i) {
              tags$tr(
                tags$td(distribution$unit_name[[i]]),
                tags$td(distribution$parameter_count[[i]])
              )
            }))
          )
        } else {
          tags$p(
            class = "text-muted",
            "No parameter-group peers were found for this parameter."
          )
        }

        unit_choices <- choice_values(moduleData$units, "unit_id", "unit_name")
        showModal(modalDialog(
          title = "Add parameter units",
          tags$p(
            paste0(
              "Assign units for ",
              status$parameter_name,
              ", ",
              status$matrix_state_name,
              "."
            )
          ),
          tags$p(
            class = "text-muted",
            "For parameters in the same parameter group and matrix state, current unit usage is:"
          ),
          distribution_ui,
          selectizeInput(
            ns("parameter_unit_modal_unit"),
            "Unit (if missing, ask a DB admin to create a new unit)",
            choices = unit_choices,
            selected = character(0),
            multiple = TRUE,
            options = list(maxItems = 1, placeholder = "Select units...")
          ),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              ns("confirm_add_parameter_unit"),
              "Save units",
              class = "btn-primary"
            )
          )
        ))
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$confirm_add_parameter_unit,
      {
        status <- selected_parameter_unit_status()
        unit_id <- integer_or_na(input$parameter_unit_modal_unit)
        unit_column <- parameter_unit_column_for_matrix(status$matrix_state_id)

        if (
          !isTRUE(status$has_selection) || is.na(unit_id) || is.na(unit_column)
        ) {
          showModal(modalDialog(
            title = "Units not saved",
            "Select a parameter, matrix state, and unit before saving.",
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          return()
        }

        DBI::dbExecute(
          con,
          sprintf(
            "UPDATE public.parameters
           SET %s = $1
           WHERE parameter_id = $2",
            DBI::dbQuoteIdentifier(con, unit_column)
          ),
          params = list(unit_id, status$parameter_id)
        )
        removeModal()
        load_reference_data()
        update_choices(clear = FALSE)
      },
      ignoreInit = TRUE
    )

    output$value_rule_hint <- renderUI({
      hint <- switch(
        input$guideline_type %||% "constant_upper",
        constant_upper = "Enter the single numeric upper limit.",
        constant_lower = "Enter the single numeric lower limit.",
        constant_range = if (identical(input$comparison_operator, "eq")) {
          "Enter the same number in Lower and Upper for an exact-value comparison."
        } else {
          "Enter the lower and upper numeric limits."
        },
        single_input_formula = "Add one sample input row, then provide coefficients for the limit chosen by Comparison.",
        narrative = "Use the note fields when the guideline does not produce a numeric value.",
        sql_scalar = "Enter a database-owned scalar SQL expression for the selected bound. Add Rule inputs when the SQL depends on sample chemistry so the dependency is visible and validated as metadata.",
        "Choose the rule type that matches how the guideline value is derived."
      )
      tags$p(class = "text-muted", style = "margin-bottom:8px;", hint)
    })

    output$calculation_source_ui <- renderUI({
      value_text <- function(x, fallback = "Not set") {
        if (
          is.null(x) ||
            !length(x) ||
            is.na(x[[1]]) ||
            !nzchar(as.character(x[[1]]))
        ) {
          fallback
        } else {
          as.character(x[[1]])
        }
      }
      item <- function(label, value) {
        tags$div(
          style = "margin-bottom:3px;",
          tags$strong(label),
          ": ",
          value_text(value)
        )
      }
      guideline_id <- selected_guideline_id()
      rules <- if (is.na(guideline_id)) {
        data.frame()
      } else {
        load_rules(guideline_id)
      }
      primary <- if (nrow(rules)) rules[1, , drop = FALSE] else NULL
      type <- input$guideline_type %||% "constant_upper"
      algorithm <- if (!is.null(primary)) {
        primary$algorithm_code[[1]]
      } else {
        switch(
          type,
          constant_upper = "constant",
          constant_lower = "constant",
          constant_range = "constant",
          single_input_formula = input$formula_algorithm %||% "linear",
          narrative = "narrative",
          sql_scalar = "sql_scalar",
          "constant"
        )
      }

      summary_text <- switch(
        algorithm,
        constant = "The guideline value is stored directly on the value rule.",
        linear = "The guideline value is calculated separately for each sample from the listed input result.",
        log_linear = "The guideline value is calculated separately for each sample from the listed input result.",
        narrative = "This guideline is stored as narrative or site-specific guidance, without a numeric calculation.",
        sql_scalar = "The database executes the stored scalar SQL expression for this rule.",
        "The database evaluates the stored value rule for this guideline."
      )

      details <- list(item("Stored as", algorithm))
      if (algorithm %in% c("linear", "log_linear")) {
        coef_count <- if (!is.null(primary)) {
          nrow(load_coefficients(primary$rule_id[[1]]))
        } else {
          NA_integer_
        }
        rule_text <- if (!is.null(primary)) format_value_rule(primary) else ""
        details <- c(
          details,
          list(
            item("Value rule", rule_text),
            item(
              "Computed value",
              "Shown as lower_guideline_value or upper_guideline_value after testing a sample/result."
            ),
            item("Coefficient rows", coef_count)
          )
        )
      } else if (algorithm == "narrative") {
        narrative_count <- if (is.na(guideline_id)) {
          NA_integer_
        } else {
          nrow(load_narrative_values(guideline_id))
        }
        details <- c(
          details,
          list(
            item("Structured rows", narrative_count),
            item("Value table", "criteria.guideline_narrative_values")
          )
        )
      } else if (algorithm == "sql_scalar") {
        details <- c(
          details,
          list(item("SQL source", "Stored on this value rule"))
        )
      }

      tags$div(
        style = paste(
          "border:1px solid #d7d7d7;",
          "padding:10px;",
          "margin-bottom:10px;",
          "background:#fff;"
        ),
        tags$strong("Calculation source"),
        tags$p(
          class = "text-muted",
          style = "margin:4px 0 8px 0;",
          summary_text
        ),
        details
      )
    })

    output$rule_inputs_ui <- renderUI({
      n <- rule_input_count()
      if (is.null(n) || n < 1L) {
        return(NULL)
      }
      seed <- normalize_rule_input_rows(rule_input_seed())
      if (nrow(seed) < n) {
        seed <- rbind(seed, blank_rule_input_rows(n - nrow(seed)))
      }
      tagList(lapply(seq_len(n), function(i) {
        row <- seed[i, , drop = FALSE]
        selected_parameter <- if (
          identical(row$input_source[[1]], "hardness_helper")
        ) {
          hardness_helper_choice_value
        } else {
          selected_or_empty(row$parameter_id)
        }
        result_type_seed <- if (
          nzchar(text_value(row$result_type_preference))
        ) {
          row$result_type_preference[[1]]
        } else {
          row$result_type[[1]]
        }
        tags$div(
          style = paste(
            "border-top:1px solid #d7d7d7;",
            "padding-top:10px;",
            "margin-top:10px;"
          ),
          tags$h5(paste("Input", i)),
          fluidRow(
            column(
              4,
              selectizeInput(
                ns(rule_input_id("parameter", i)),
                bslib::tooltip(
                  tags$span("Parameter"),
                  "Choose the sample result to use. The hardness helper is a database resolver that picks the preferred hardness value for the sample."
                ),
                choices = parameter_choices_with_helpers(),
                selected = selected_parameter,
                multiple = TRUE,
                options = selectize_single_options,
                width = "100%"
              )
            ),
            column(
              4,
              selectizeInput(
                ns(rule_input_id("matrix_state", i)),
                "Matrix state",
                choices = choices_with_any(matrix_state_choices()),
                selected = selected_or_empty(row$matrix_state_id),
                multiple = TRUE,
                options = selectize_single_options,
                width = "100%"
              )
            ),
            column(
              4,
              selectInput(
                ns(rule_input_id("aggregate_method", i)),
                bslib::tooltip(
                  tags$span("Multiple results"),
                  "What to do if the selected preference level has more than one matching result for the sample."
                ),
                choices = c(
                  "Require one result" = "single",
                  "Average" = "avg",
                  "Minimum" = "min",
                  "Maximum" = "max"
                ),
                selected = row$aggregate_method[[1]] %||% "single"
              )
            )
          ),
          fluidRow(
            column(
              4,
              shinyjs::hidden(
                div(
                  id = ns(rule_input_id("fraction_section", i)),
                  selectizeInput(
                    ns(rule_input_id("sample_fraction", i)),
                    "Fraction",
                    choices = choices_with_any(sample_fraction_choices()),
                    selected = selected_or_empty(row$sample_fraction_id),
                    multiple = TRUE,
                    options = selectize_single_options,
                    width = "100%"
                  )
                )
              )
            ),
            column(
              4,
              shinyjs::hidden(
                div(
                  id = ns(rule_input_id("speciation_section", i)),
                  selectizeInput(
                    ns(rule_input_id("speciation", i)),
                    "Speciation",
                    choices = choices_with_any(speciation_choices()),
                    selected = selected_or_empty(row$result_speciation_id),
                    multiple = TRUE,
                    options = selectize_single_options,
                    width = "100%"
                  )
                )
              )
            ),
            column(
              4,
              selectizeInput(
                ns(rule_input_id("result_type_preference", i)),
                bslib::tooltip(
                  tags$span("Result type preference"),
                  "Optional fallback order. For example, choose field first and lab second to use field results when present, otherwise lab results."
                ),
                choices = choices_with_any(result_type_choices()),
                selected = selected_or_empty_vector(result_type_seed),
                multiple = TRUE,
                options = list(
                  placeholder = "Any result type, or select fallback order..."
                ),
                width = "100%"
              )
            )
          ),
          tags$details(
            open = FALSE,
            tags$summary("Input matching options"),
            fluidRow(
              column(
                3,
                checkboxInput(
                  ns(rule_input_id("required", i)),
                  bslib::tooltip(
                    tags$span("Required"),
                    "If checked, the guideline returns a missing-input status when this value cannot be resolved."
                  ),
                  value = isTRUE(row$required[[1]])
                )
              ),
              column(
                3,
                checkboxInput(
                  ns(rule_input_id("allow_condition_value", i)),
                  bslib::tooltip(
                    tags$span("Use condition value"),
                    "Use a numeric condition value for censored/qualified results when the primary result value is blank. Leave unchecked unless the source method explicitly allows it."
                  ),
                  value = isTRUE(row$allow_condition_value[[1]])
                )
              ),
              column(
                3,
                numericInput(
                  ns(rule_input_id("lower_calibrated_bound", i)),
                  bslib::tooltip(
                    tags$span("Calibration low"),
                    "Optional lower domain limit for an external model or calibrated formula. Out-of-range handling is set below."
                  ),
                  value = row$lower_calibrated_bound[[1]]
                )
              ),
              column(
                3,
                numericInput(
                  ns(rule_input_id("upper_calibrated_bound", i)),
                  bslib::tooltip(
                    tags$span("Calibration high"),
                    "Optional upper domain limit for an external model or calibrated formula. Out-of-range handling is set below."
                  ),
                  value = row$upper_calibrated_bound[[1]]
                )
              )
            ),
            fluidRow(
              column(
                3,
                selectInput(
                  ns(rule_input_id("bounds_action", i)),
                  bslib::tooltip(
                    tags$span("Out-of-range input"),
                    "For calibration limits: flag the value, clamp it to the limit, or reject the guideline calculation."
                  ),
                  choices = c(
                    "Flag" = "flag",
                    "Clamp" = "clamp",
                    "Reject" = "reject"
                  ),
                  selected = row$bounds_action[[1]] %||% "flag"
                )
              ),
              column(
                9,
                textInput(
                  ns(rule_input_id("note", i)),
                  "Input note",
                  value = row$note[[1]] %||% "",
                  width = "100%"
                )
              )
            )
          )
        )
      }))
    })

    update_rule_field_visibility <- function(type = "constant_upper") {
      type <- type %||% "constant_upper"
      sections <- c(
        "fixed_value_section",
        "range_value_section",
        "bound_section",
        "formula_algorithm_section",
        "advanced_rule_options_section",
        "rule_inputs_section",
        "coefficients_section",
        "narrative_values_section",
        "sql_scalar_section"
      )
      invisible(lapply(sections, function(id) {
        shinyjs::hide(id = id, anim = FALSE)
      }))

      if (type %in% c("constant_upper", "constant_lower")) {
        shinyjs::show(id = "fixed_value_section", anim = FALSE)
      }
      if (identical(type, "constant_range")) {
        shinyjs::show(id = "range_value_section", anim = FALSE)
      }
      if (type %in% c("single_input_formula", "sql_scalar")) {
        shinyjs::show(id = "advanced_rule_options_section", anim = FALSE)
      }
      if (identical(type, "single_input_formula")) {
        shinyjs::show(id = "formula_algorithm_section", anim = FALSE)
        shinyjs::show(id = "rule_inputs_section", anim = FALSE)
        shinyjs::show(id = "coefficients_section", anim = FALSE)
      }
      if (identical(type, "narrative")) {
        shinyjs::show(id = "narrative_values_section", anim = FALSE)
      }
      if (identical(type, "sql_scalar")) {
        shinyjs::show(id = "rule_inputs_section", anim = FALSE)
        shinyjs::show(id = "sql_scalar_section", anim = FALSE)
      }
    }

    last_comparison_operator <- reactiveVal(NULL)

    observeEvent(
      input$guideline_type,
      {
        type <- input$guideline_type %||% "constant_upper"
        update_rule_field_visibility(type)
        operator <- input$comparison_operator %||% "lte"
        if (operator %in% c("lte", "gte", "eq")) {
          updateSelectInput(
            session,
            "bound_code",
            selected = bound_for_operator(operator)
          )
        }
      },
      ignoreInit = FALSE
    )

    observeEvent(
      input$comparison_operator,
      {
        operator <- input$comparison_operator %||% "lte"
        previous_operator <- last_comparison_operator()
        selected <- update_guideline_type_choices(
          operator,
          input$guideline_type
        )
        if (
          !is.null(previous_operator) && !identical(previous_operator, operator)
        ) {
          updateNumericInput(session, "fixed_value", value = NA_real_)
          updateNumericInput(session, "lower_value", value = NA_real_)
          updateNumericInput(session, "upper_value", value = NA_real_)
        }
        update_rule_field_visibility(selected)
        last_comparison_operator(operator)
      },
      ignoreInit = FALSE
    )

    observeEvent(
      input$parameter_id,
      {
        update_result_speciation_visibility(input$parameter_id)
      },
      ignoreInit = FALSE
    )

    output$fraction_speciation_applicability_note <- renderUI({
      parameter_id <- integer_or_na(input$parameter_id)
      if (is.na(parameter_id)) {
        return(tags$small(
          class = "text-muted",
          "Leaving guideline fractions empty applies the guideline to all fractions, including results where fraction is empty. Leaving result speciation empty applies the guideline to all speciations, including results where speciation is empty."
        ))
      }

      fraction_required <- parameter_flag_value(parameter_id, "sample_fraction")
      speciation_required <- parameter_flag_value(
        parameter_id,
        "result_speciation"
      )

      notes <- c(
        "Leave guideline fractions empty unless the source guideline applies only to named fractions. Empty means all fractions, including results where fraction is empty.",
        "Leave result speciation empty unless the source guideline applies only to a named speciation. Empty means all speciations, including results where speciation is empty."
      )
      if (!fraction_required) {
        notes <- c(
          notes,
          "This parameter is not marked as requiring sample fraction in public.parameters, so results for it commonly have no fraction."
        )
      }
      if (!speciation_required) {
        notes <- c(
          notes,
          "This parameter is not marked as requiring result speciation in public.parameters, so results for it commonly have no speciation."
        )
      }

      tags$div(
        class = "small text-muted",
        style = paste(
          "margin-top:-0.35rem;",
          "margin-bottom:0.75rem;",
          "line-height:1.35;"
        ),
        lapply(notes, tags$p)
      )
    })

    observe({
      n <- rule_input_count()
      if (is.null(n) || n < 1L) {
        return()
      }
      for (i in seq_len(n)) {
        parameter_id <- input[[rule_input_id("parameter", i)]]
        if (parameter_requires_fraction(parameter_id)) {
          shinyjs::show(id = rule_input_id("fraction_section", i), anim = FALSE)
        } else {
          updateSelectizeInput(
            session,
            rule_input_id("sample_fraction", i),
            selected = character(0)
          )
          shinyjs::hide(id = rule_input_id("fraction_section", i), anim = FALSE)
        }
        if (parameter_requires_speciation(parameter_id)) {
          shinyjs::show(
            id = rule_input_id("speciation_section", i),
            anim = FALSE
          )
        } else {
          updateSelectizeInput(
            session,
            rule_input_id("speciation", i),
            selected = character(0)
          )
          shinyjs::hide(
            id = rule_input_id("speciation_section", i),
            anim = FALSE
          )
        }
      }
    })

    observeEvent(input$add_rule_input, {
      df <- collect_rule_input_rows(include_empty = TRUE)
      set_rule_input_rows(rbind(df, blank_rule_input_rows(1L)))
    })

    observeEvent(input$remove_rule_input, {
      df <- collect_rule_input_rows(include_empty = TRUE)
      if (nrow(df) <= 1L) {
        set_rule_input_rows(blank_rule_input_rows(1L))
      } else {
        set_rule_input_rows(df[-nrow(df), , drop = FALSE])
      }
    })

    output$guidelines_table <- DT::renderDT(
      {
        cols <- c(
          "guideline_id",
          "guideline_code",
          "guideline_name",
          "publisher",
          "series",
          "parameter",
          "units",
          "matrix_state",
          "fractions",
          "media_types",
          "locations",
          "comparison_operator_code",
          "exposure_duration",
          "averaging_period",
          "review_status",
          "active",
          "rule_count",
          "rules"
        )
        df <- moduleData$guidelines
        for (col in cols) {
          if (!col %in% names(df)) df[[col]] <- NA
        }
        DT::datatable(
          df[, cols, drop = FALSE],
          selection = "single",
          rownames = FALSE,
          filter = "top",
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            columnDefs = list(list(targets = 0, visible = FALSE)),
            layout = list(bottomStart = "info", bottomEnd = "paging")
          )
        )
      },
      server = FALSE
    )

    load_rules <- function(guideline_id) {
      DBI::dbGetQuery(
        con,
        "SELECT * FROM criteria.guideline_value_rules
         WHERE guideline_id = $1
         ORDER BY rule_priority, rule_id",
        params = list(guideline_id)
      )
    }
    load_inputs <- function(rule_id) {
      DBI::dbGetQuery(
        con,
        "SELECT gri.input_code, gri.input_name,
                COALESCE(gri.input_source, 'sample_result') AS input_source,
                gri.parameter_id,
                gri.matrix_state_id, gri.sample_fraction_id,
                gri.result_speciation_id, gri.result_type,
                array_to_string(gri.result_type_preference, ',') AS result_type_preference,
                gri.aggregate_method, gri.required, gri.allow_condition_value,
                gri.lower_calibrated_bound,
                gri.upper_calibrated_bound,
                COALESCE(gri.bounds_action, 'flag') AS bounds_action,
                gri.note
         FROM criteria.guideline_rule_inputs gri
         WHERE gri.rule_id = $1
         ORDER BY gri.input_code",
        params = list(rule_id)
      )
    }
    load_coefficients <- function(rule_id) {
      DBI::dbGetQuery(
        con,
        "SELECT coefficient_name, coefficient_value, note
         FROM criteria.guideline_rule_coefficients
         WHERE rule_id = $1
         ORDER BY coefficient_name",
        params = list(rule_id)
      )
    }
    coefficient_value <- function(coefficients, name) {
      if (is.null(coefficients) || !nrow(coefficients)) {
        return(NA_real_)
      }
      hit <- coefficients$coefficient_name == name
      if (!any(hit)) {
        return(NA_real_)
      }
      suppressWarnings(as.numeric(coefficients$coefficient_value[hit][[1]]))
    }
    formula_input_label <- function(inputs) {
      if (is.null(inputs) || !nrow(inputs)) {
        return("input_value")
      }
      label <- inputs$input_name[[1]]
      if (is.na(label) || !nzchar(label)) {
        label <- inputs$input_code[[1]]
      }
      if (is.na(label) || !nzchar(label)) {
        label <- "input_value"
      }
      label
    }
    format_value_rule <- function(rule) {
      if (is.null(rule) || !nrow(rule)) {
        return("")
      }
      algorithm <- rule$algorithm_code[[1]]
      bound <- rule$bound_code[[1]]
      if (is.na(bound) || !nzchar(bound)) {
        bound <- "value"
      }
      if (identical(algorithm, "constant")) {
        return(paste0(bound, " = ", rule$fixed_value[[1]]))
      }
      if (algorithm %in% c("linear", "log_linear")) {
        coefficients <- load_coefficients(rule$rule_id[[1]])
        inputs <- load_inputs(rule$rule_id[[1]])
        input_label <- formula_input_label(inputs)
        intercept <- coefficient_value(coefficients, "intercept")
        slope <- coefficient_value(coefficients, "slope")
        if (identical(algorithm, "linear")) {
          return(paste0(
            bound,
            " = ",
            intercept,
            " + ",
            slope,
            " * ",
            input_label
          ))
        }
        return(paste0(
          bound,
          " = exp(",
          intercept,
          " + ",
          slope,
          " * ln(",
          input_label,
          "))"
        ))
      }
      if (identical(algorithm, "sql_scalar")) {
        return(paste0(bound, " = SQL scalar result"))
      }
      if (identical(algorithm, "narrative")) {
        return("narrative or site-specific value")
      }
      algorithm
    }
    load_narrative_values <- function(guideline_id) {
      if (is.na(integer_or_na(guideline_id))) {
        return(data.frame(stringsAsFactors = FALSE))
      }
      DBI::dbGetQuery(
        con,
        "SELECT value_code, condition_label, max_change_value,
                max_change_percent, change_unit, background_lower_bound,
                background_upper_bound, background_unit, duration_label,
                flow_condition, sort_order, note
         FROM criteria.guideline_narrative_values
         WHERE guideline_id = $1
         ORDER BY sort_order, narrative_value_id",
        params = list(integer_or_na(guideline_id))
      )
    }

    output$selected_rules_table <- DT::renderDT({
      guideline_id <- selected_guideline_id()
      if (is.na(guideline_id)) {
        return(DT::datatable(data.frame(), rownames = FALSE))
      }
      rules <- load_rules(guideline_id)
      if (nrow(rules)) {
        rules$value_rule <- vapply(
          seq_len(nrow(rules)),
          function(i) format_value_rule(rules[i, , drop = FALSE]),
          character(1)
        )
      }
      DT::datatable(rules, rownames = FALSE, options = list(scrollX = TRUE))
    })
    output$selected_inputs_table <- DT::renderDT({
      guideline_id <- selected_guideline_id()
      if (is.na(guideline_id)) {
        return(DT::datatable(data.frame(), rownames = FALSE))
      }
      rules <- load_rules(guideline_id)
      if (!nrow(rules)) {
        return(DT::datatable(data.frame(), rownames = FALSE))
      }
      inputs <- do.call(
        rbind,
        lapply(rules$rule_id, function(rule_id) {
          out <- load_inputs(rule_id)
          if (nrow(out)) {
            out$rule_id <- rule_id
          }
          out
        })
      )
      if (is.null(inputs)) {
        inputs <- data.frame()
      }
      DT::datatable(inputs, rownames = FALSE, options = list(scrollX = TRUE))
    })

    infer_guideline_type <- function(rules) {
      if (!nrow(rules)) {
        return("constant_upper")
      }
      if (
        nrow(rules) == 2 &&
          all(rules$algorithm_code == "constant") &&
          all(c("lower", "upper") %in% rules$bound_code)
      ) {
        return("constant_range")
      }
      first <- rules[1, , drop = FALSE]
      if (first$algorithm_code == "constant" && first$bound_code == "lower") {
        return("constant_lower")
      }
      if (first$algorithm_code == "constant") {
        return("constant_upper")
      }
      if (first$algorithm_code %in% c("linear", "log_linear")) {
        return("single_input_formula")
      }
      first$algorithm_code
    }

    clear_form <- function() {
      selected_guideline_id(NA_integer_)
      updateTextInput(session, "guideline_code", value = "")
      updateTextInput(session, "guideline_name", value = "")
      updateSelectizeInput(session, "publisher", selected = character(0))
      updateSelectizeInput(session, "series", selected = character(0))
      updateSelectizeInput(session, "parameter_id", selected = character(0))
      updateSelectizeInput(session, "matrix_state", selected = character(0))
      updateSelectizeInput(
        session,
        "result_speciation",
        selected = character(0)
      )
      updateSelectizeInput(session, "sample_fraction", selected = character(0))
      updateSelectizeInput(session, "media_type", selected = character(0))
      updateSelectInput(session, "comparison_operator", selected = "lte")
      updateSelectizeInput(session, "jurisdiction", selected = character(0))
      updateSelectizeInput(session, "protection_goal", selected = character(0))
      updateSelectizeInput(
        session,
        "exposure_duration",
        selected = character(0)
      )
      updateSelectizeInput(session, "averaging_period", selected = character(0))
      updateSelectizeInput(
        session,
        "specific_locations",
        selected = character(0)
      )
      updateDateInput(session, "valid_from", value = Sys.Date())
      update_nullable_date_input("valid_to", NA)
      updateSelectInput(session, "review_status", selected = "draft")
      updateCheckboxInput(session, "active", value = TRUE)
      updateTextInput(session, "reference", value = "")
      updateTextInput(session, "source_document_title", value = "")
      updateTextInput(session, "source_url", value = "")
      updateTextInput(session, "source_page", value = "")
      updateTextInput(session, "source_table", value = "")
      updateTextInput(session, "source_section", value = "")
      update_nullable_date_input("source_effective_date", NA)
      updateDateInput(session, "source_retrieved_date", value = Sys.Date())
      updateTextAreaInput(session, "general_notes", value = "")
      updateTextAreaInput(session, "applicability_notes", value = "")
      update_guideline_type_choices("lte", "constant_upper")
      updateNumericInput(session, "fixed_value", value = NA_real_)
      updateNumericInput(session, "lower_value", value = NA_real_)
      updateNumericInput(session, "upper_value", value = NA_real_)
      updateSelectInput(session, "bound_code", selected = "upper")
      updateSelectInput(session, "formula_algorithm", selected = "linear")
      updateNumericInput(session, "rounding_digits", value = NA_integer_)
      updateSelectInput(session, "rounding_method", selected = "none")
      updateSelectInput(session, "missing_input_policy", selected = "no_value")
      updateNumericInput(session, "min_output_value", value = NA_real_)
      updateNumericInput(session, "max_output_value", value = NA_real_)
      updateTextInput(session, "precision_note", value = "")
      updateTextAreaInput(session, "rule_note", value = "")
      set_rule_input_rows(blank_rule_input_rows(1L))
      updateTextAreaInput(
        session,
        "coefficients_text",
        value = empty_table_text(coef_cols)
      )
      updateTextAreaInput(
        session,
        "narrative_values_text",
        value = empty_table_text(narrative_cols)
      )
      update_formula_sql("")
      update_result_speciation_visibility(character(0))
    }

    load_form <- function(guideline_id) {
      g <- moduleData$guidelines[
        moduleData$guidelines$guideline_id == guideline_id,
        ,
        drop = FALSE
      ]
      if (!nrow(g)) {
        return()
      }
      selected_guideline_id(guideline_id)
      rules <- load_rules(guideline_id)
      type <- infer_guideline_type(rules)
      primary <- if (nrow(rules)) rules[1, , drop = FALSE] else NULL

      updateTextInput(session, "guideline_code", value = g$guideline_code[[1]])
      updateTextInput(session, "guideline_name", value = g$guideline_name[[1]])
      updateSelectizeInput(
        session,
        "publisher",
        choices = choice_values(
          moduleData$publishers,
          "publisher_id",
          "publisher_name"
        ),
        selected = as.character(g$publisher_id[[1]]),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "series",
        choices = choice_values(moduleData$series, "series_id", "series_name"),
        selected = if (is.na(g$series_id[[1]])) {
          character(0)
        } else {
          as.character(g$series_id[[1]])
        },
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "parameter_id",
        choices = parameter_choices(),
        selected = as.character(g$parameter_id[[1]]),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "matrix_state",
        choices = choice_values(
          moduleData$matrix_states,
          "matrix_state_id",
          "matrix_state_name"
        ),
        selected = as.character(g$matrix_state_id[[1]]),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "result_speciation",
        choices = choice_values(
          moduleData$result_speciations,
          "result_speciation_id",
          "result_speciation"
        ),
        selected = if (is.na(g$result_speciation_id[[1]])) {
          character(0)
        } else {
          as.character(g$result_speciation_id[[1]])
        },
        server = TRUE
      )
      update_result_speciation_visibility(g$parameter_id[[1]])
      updateSelectizeInput(
        session,
        "sample_fraction",
        choices = choice_values(
          moduleData$sample_fractions,
          "sample_fraction_id",
          "sample_fraction"
        ),
        selected = if (is.na(g$fraction_ids[[1]])) {
          character(0)
        } else {
          strsplit(g$fraction_ids[[1]], ",", fixed = TRUE)[[1]]
        },
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "media_type",
        choices = choice_values(
          moduleData$media_types,
          "media_id",
          "media_type"
        ),
        selected = if (is.na(g$media_ids[[1]])) {
          character(0)
        } else {
          strsplit(g$media_ids[[1]], ",", fixed = TRUE)[[1]]
        },
        server = TRUE
      )
      updateSelectInput(
        session,
        "comparison_operator",
        selected = g$comparison_operator_code[[1]]
      )
      update_guideline_type_choices(g$comparison_operator_code[[1]], type)
      last_comparison_operator(g$comparison_operator_code[[1]])
      updateSelectizeInput(
        session,
        "jurisdiction",
        choices = choice_values(
          moduleData$jurisdictions,
          "jurisdiction_id",
          "jurisdiction_name"
        ),
        selected = if (is.na(g$jurisdiction_id[[1]])) {
          character(0)
        } else {
          as.character(g$jurisdiction_id[[1]])
        },
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "protection_goal",
        choices = choice_values(
          moduleData$protection_goals,
          "protection_goal_id",
          "protection_goal_name"
        ),
        selected = if (is.na(g$protection_goal_id[[1]])) {
          character(0)
        } else {
          as.character(g$protection_goal_id[[1]])
        },
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "exposure_duration",
        choices = choice_values(
          moduleData$exposure_durations,
          "exposure_duration_id",
          "exposure_duration_name"
        ),
        selected = if (is.na(g$exposure_duration_id[[1]])) {
          character(0)
        } else {
          as.character(g$exposure_duration_id[[1]])
        },
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "averaging_period",
        choices = choice_values(
          moduleData$averaging_periods,
          "averaging_period_id",
          "averaging_period_name"
        ),
        selected = if (is.na(g$averaging_period_id[[1]])) {
          character(0)
        } else {
          as.character(g$averaging_period_id[[1]])
        },
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "specific_locations",
        choices = choice_values(
          moduleData$locations,
          "location_id",
          "location_label"
        ),
        selected = if (is.na(g$location_ids[[1]])) {
          character(0)
        } else {
          strsplit(g$location_ids[[1]], ",", fixed = TRUE)[[1]]
        },
        server = TRUE
      )
      update_nullable_date_input(
        "valid_from",
        g$valid_from[[1]],
        default = Sys.Date()
      )
      update_nullable_date_input("valid_to", g$valid_to[[1]])
      updateSelectInput(
        session,
        "review_status",
        selected = g$review_status[[1]]
      )
      updateCheckboxInput(
        session,
        "active",
        value = bool_value(g$active[[1]], TRUE)
      )
      updateTextInput(session, "reference", value = g$reference[[1]] %||% "")
      updateTextInput(
        session,
        "source_document_title",
        value = g$source_document_title[[1]] %||% ""
      )
      updateTextInput(session, "source_url", value = g$source_url[[1]] %||% "")
      updateTextInput(
        session,
        "source_page",
        value = g$source_page[[1]] %||% ""
      )
      updateTextInput(
        session,
        "source_table",
        value = g$source_table[[1]] %||% ""
      )
      updateTextInput(
        session,
        "source_section",
        value = g$source_section[[1]] %||% ""
      )
      update_nullable_date_input(
        "source_effective_date",
        g$source_effective_date[[1]]
      )
      update_nullable_date_input(
        "source_retrieved_date",
        g$source_retrieved_date[[1]],
        default = Sys.Date()
      )
      updateTextAreaInput(
        session,
        "general_notes",
        value = g$general_notes[[1]] %||% ""
      )
      updateTextAreaInput(
        session,
        "applicability_notes",
        value = g$applicability_notes[[1]] %||% ""
      )

      updateNumericInput(session, "fixed_value", value = NA_real_)
      updateNumericInput(session, "lower_value", value = NA_real_)
      updateNumericInput(session, "upper_value", value = NA_real_)
      if (type == "constant_range") {
        updateNumericInput(
          session,
          "lower_value",
          value = rules$fixed_value[rules$bound_code == "lower"][[1]]
        )
        updateNumericInput(
          session,
          "upper_value",
          value = rules$fixed_value[rules$bound_code == "upper"][[1]]
        )
      } else if (!is.null(primary) && primary$algorithm_code == "constant") {
        updateNumericInput(
          session,
          "fixed_value",
          value = primary$fixed_value[[1]]
        )
      }

      if (!is.null(primary)) {
        updateSelectInput(
          session,
          "bound_code",
          selected = if (is.na(primary$bound_code[[1]])) {
            "upper"
          } else {
            primary$bound_code[[1]]
          }
        )
        updateSelectInput(
          session,
          "formula_algorithm",
          selected = if (
            primary$algorithm_code[[1]] %in% c("linear", "log_linear")
          ) {
            primary$algorithm_code[[1]]
          } else {
            "linear"
          }
        )
        updateNumericInput(
          session,
          "rounding_digits",
          value = primary$rounding_digits[[1]]
        )
        updateSelectInput(
          session,
          "rounding_method",
          selected = primary$rounding_method[[1]]
        )
        updateSelectInput(
          session,
          "missing_input_policy",
          selected = primary$missing_input_policy[[1]]
        )
        updateNumericInput(
          session,
          "min_output_value",
          value = primary$min_output_value[[1]]
        )
        updateNumericInput(
          session,
          "max_output_value",
          value = primary$max_output_value[[1]]
        )
        updateTextInput(
          session,
          "precision_note",
          value = primary$precision_note[[1]] %||% ""
        )
        updateTextAreaInput(
          session,
          "rule_note",
          value = primary$note[[1]] %||% ""
        )
        update_formula_sql(primary$formula_sql[[1]] %||% "")
        set_rule_input_rows(load_inputs(primary$rule_id[[1]]))
        updateTextAreaInput(
          session,
          "coefficients_text",
          value = format_table_text(
            load_coefficients(primary$rule_id[[1]]),
            coef_cols
          )
        )
        updateTextAreaInput(
          session,
          "narrative_values_text",
          value = format_table_text(
            load_narrative_values(guideline_id),
            narrative_cols
          )
        )
      } else {
        set_rule_input_rows(blank_rule_input_rows(1L))
        updateTextAreaInput(
          session,
          "narrative_values_text",
          value = empty_table_text(narrative_cols)
        )
      }
    }

    observeEvent(input$guidelines_table_rows_selected, {
      row <- input$guidelines_table_rows_selected
      if (is.null(row) || !length(row)) {
        return()
      }
      load_form(moduleData$guidelines$guideline_id[[row]])
    })
    observeEvent(input$add_guideline, {
      clear_form()
    })

    find_parameter_id <- function(pattern) {
      hits <- grep(
        pattern,
        moduleData$parameters$param_name,
        ignore.case = TRUE
      )
      if (length(hits)) {
        moduleData$parameters$parameter_id[[hits[[1]]]]
      } else {
        NA_integer_
      }
    }
    find_fraction_id <- function(pattern) {
      hits <- grep(
        pattern,
        moduleData$sample_fractions$sample_fraction,
        ignore.case = TRUE
      )
      if (length(hits)) {
        moduleData$sample_fractions$sample_fraction_id[[hits[[1]]]]
      } else {
        NA_integer_
      }
    }
    find_speciation_id <- function(pattern) {
      hits <- grep(
        pattern,
        moduleData$result_speciations$result_speciation,
        ignore.case = TRUE
      )
      if (length(hits)) {
        moduleData$result_speciations$result_speciation_id[[hits[[1]]]]
      } else {
        NA_integer_
      }
    }
    first_id_from_csv <- function(x) {
      vals <- parse_id_csv(x)
      if (length(vals)) vals[[1]] else NA_integer_
    }
    first_result_type_preference <- function(x) {
      vals <- parse_id_csv(x)
      if (length(vals)) vals[[1]] else NA_integer_
    }
    default_result_type_id <- function(patterns = c("^lab$", "^field$")) {
      if (is.null(moduleData$result_types) || !nrow(moduleData$result_types)) {
        return(NA_integer_)
      }
      for (pattern in patterns) {
        hits <- grep(
          pattern,
          moduleData$result_types$result_type,
          ignore.case = TRUE
        )
        if (length(hits)) {
          return(moduleData$result_types$result_type_id[[hits[[1]]]])
        }
      }
      moduleData$result_types$result_type_id[[1]]
    }
    result_value_actual_id <- function() {
      value_type <- DBI::dbGetQuery(
        con,
        "SELECT result_value_type_id
         FROM discrete.result_value_types
         WHERE lower(result_value_type) = 'actual'
         ORDER BY result_value_type_id
         LIMIT 1"
      )
      if (nrow(value_type)) {
        value_type$result_value_type_id[[1]]
      } else {
        NA_integer_
      }
    }
    test_value_label <- function(parameter_id, matrix_state_id, suffix) {
      name <- text_default(parameter_name_for_id(parameter_id), "Parameter")
      units <- parameter_unit_name_exact(parameter_id, matrix_state_id)
      if (nzchar(units)) {
        paste0(name, " (", units, ") ", suffix)
      } else {
        paste0(name, " ", suffix)
      }
    }
    temporary_input_controls <- function(rule_inputs) {
      if (is.null(rule_inputs) || !nrow(rule_inputs)) {
        return(tags$div(
          class = "text-muted",
          "This guideline rule has no sample-input parameters."
        ))
      }
      tagList(lapply(seq_len(nrow(rule_inputs)), function(i) {
        row <- rule_inputs[i, , drop = FALSE]
        numericInput(
          ns(paste0("test_input_value_", i)),
          test_value_label(
            row$parameter_id[[1]],
            row$matrix_state_id[[1]],
            "input value"
          ),
          value = NA_real_,
          width = "100%"
        )
      }))
    }
    collect_temporary_input_results <- function(rule_inputs) {
      if (is.null(rule_inputs) || !nrow(rule_inputs)) {
        return(blank_rule_input_rows(0L))
      }
      df <- rule_inputs
      df$value <- vapply(
        seq_len(nrow(df)),
        function(i) numeric_or_na(input[[paste0("test_input_value_", i)]]),
        numeric(1)
      )
      df$result_type_id <- vapply(
        df$result_type_preference,
        first_result_type_preference,
        integer(1)
      )
      missing_result_type <- is.na(df$result_type_id)
      df$result_type_id[
        missing_result_type
      ] <- suppressWarnings(as.integer(df$result_type[missing_result_type]))
      df$result_type_id[is.na(df$result_type_id)] <- default_result_type_id()
      hardness_rows <- df$input_source %in% "hardness_helper"
      if (any(hardness_rows)) {
        df$sample_fraction_id[
          hardness_rows & is.na(df$sample_fraction_id)
        ] <- find_fraction_id("^total$")
        df$result_speciation_id[
          hardness_rows & is.na(df$result_speciation_id)
        ] <- find_speciation_id("CaCO3|CaCO3 equivalent")
      }
      df
    }
    liquid_matrix_state_id <- function() {
      hits <- which(moduleData$matrix_states$matrix_state_code == "liquid")
      if (length(hits)) {
        moduleData$matrix_states$matrix_state_id[[hits[[1]]]]
      } else {
        NA_integer_
      }
    }

    observeEvent(input$fill_coefficients_template, {
      df <- data.frame(
        coefficient_name = c("intercept", "slope"),
        coefficient_value = c(0, 1),
        note = ""
      )
      updateTextAreaInput(
        session,
        "coefficients_text",
        value = format_table_text(df, coef_cols)
      )
    })

    sql_insert_text <- function(text, eol_comment = NULL) {
      session$sendCustomMessage(
        "insertAtCursor",
        list(
          target = ns("formula_sql"),
          text = text,
          eolComment = eol_comment %||% ""
        )
      )
    }
    normalize_declared_rule_inputs <- function(df) {
      if (is.null(df) || !nrow(df)) {
        return(blank_rule_input_rows(0L))
      }
      for (i in seq_len(nrow(df))) {
        df$input_source[i] <- text_default(df$input_source[i], "sample_result")
        df$input_name[i] <- default_rule_input_name(
          df$parameter_id[i],
          df$input_source[i]
        )
        df$input_code[i] <- default_rule_input_code(
          df$parameter_id[i],
          df$input_source[i],
          fallback = paste0("input_", i)
        )
      }
      df$input_code <- make_unique_input_codes(df$input_code)
      df[!is.na(df$parameter_id), , drop = FALSE]
    }
    null_or_int_sql <- function(x) {
      value <- integer_or_na(x)
      if (is.na(value)) "NULL::INT" else paste0(value, "::INT")
    }
    sql_input_expression <- function(row) {
      input_code <- normalize_input_code(row$input_code[[1]], "input_value")
      if (
        identical(
          text_default(row$input_source[[1]], "sample_result"),
          "hardness_helper"
        )
      ) {
        return(paste0(
          "criteria.get_sample_hardness($1::INT) AS ",
          input_code
        ))
      }
      paste0(
        "discrete.get_sample_val(\n",
        "  sample_id := $1::INT,\n",
        "  parameter_id := ",
        null_or_int_sql(row$parameter_id[[1]]),
        ",\n",
        "  sample_fraction_id := ",
        null_or_int_sql(row$sample_fraction_id[[1]]),
        ",\n",
        "  result_speciation_id := ",
        null_or_int_sql(row$result_speciation_id[[1]]),
        "\n",
        ") AS ",
        input_code
      )
    }
    declared_sql_input_choices <- function() {
      df <- normalize_declared_rule_inputs(collect_rule_input_rows())
      if (!nrow(df)) {
        return(character(0))
      }
      labels <- vapply(
        seq_len(nrow(df)),
        function(i) {
          row <- df[i, , drop = FALSE]
          name <- text_value(row$input_name[[1]])
          code <- normalize_input_code(row$input_code[[1]], paste0("input_", i))
          if (
            identical(
              text_default(row$input_source[[1]], "sample_result"),
              "hardness_helper"
            )
          ) {
            return(paste0(name, " [", code, "]"))
          }
          meta <- paste0(
            "parameter_id ",
            integer_or_na(row$parameter_id[[1]]),
            ", fraction ",
            ifelse(
              is.na(integer_or_na(row$sample_fraction_id[[1]])),
              "NULL",
              integer_or_na(row$sample_fraction_id[[1]])
            ),
            ", speciation ",
            ifelse(
              is.na(integer_or_na(row$result_speciation_id[[1]])),
              "NULL",
              integer_or_na(row$result_speciation_id[[1]])
            )
          )
          paste0(name, " [", code, "; ", meta, "]")
        },
        character(1)
      )
      stats::setNames(as.character(seq_len(nrow(df))), labels)
    }
    observeEvent(
      input$insert_sql_parameter,
      {
        choices <- declared_sql_input_choices()
        if (!length(choices)) {
          showModal(modalDialog(
            title = "No rule inputs",
            "Add at least one Rule input before inserting sample chemistry into SQL.",
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          return()
        }
        showModal(modalDialog(
          size = "m",
          title = "Choose a rule input",
          selectizeInput(
            ns("pick_sql_input"),
            NULL,
            choices = choices,
            options = list(placeholder = "Type to search...")
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              ns("confirm_sql_input"),
              "Insert",
              class = "btn-primary"
            )
          )
        ))
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$confirm_sql_input,
      {
        req(input$pick_sql_input)
        removeModal()
        df <- normalize_declared_rule_inputs(collect_rule_input_rows())
        row_number <- integer_or_na(input$pick_sql_input)
        if (is.na(row_number) || row_number < 1L || row_number > nrow(df)) {
          return()
        }
        row <- df[row_number, , drop = FALSE]
        sql_insert_text(
          paste0(sql_input_expression(row), "\n"),
          paste0("rule input: ", text_value(row$input_name[[1]]))
        )
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$open_guideline_sql_help,
      {
        session$sendCustomMessage(
          "openGuidelineHelp",
          "html/guidelines_help.html"
        )
      },
      ignoreInit = TRUE
    )
    sql_scalar_template <- function(template_code) {
      switch(
        template_code,
        fixed = "-- Return one numeric guideline value in the database units for the guideline parameter.
SELECT 0::numeric",
        sample_inputs = "WITH vals AS (
  SELECT
    discrete.get_sample_val(
      sample_id := $1::INT,
      ***!parameter_id := xxx::INT!***,
      ***!sample_fraction_id := xxx::INT!***,
      ***!result_speciation_id := xxx::INT!***
    ) AS v1

    -- Add more sample inputs here if needed, separated by commas.
)
SELECT CASE
  WHEN v1 IS NULL THEN NULL::numeric
  ELSE ***!equation_or_value!***::numeric
END
FROM vals",
        hardness = "WITH vals AS (
  SELECT
    criteria.get_sample_hardness($1::INT) AS h

    -- Add more sample inputs here if needed, separated by commas.
    -- , discrete.get_sample_val(
    --     sample_id := $1::INT,
    --     ***!parameter_id := xxx::INT!***,
    --     ***!sample_fraction_id := xxx::INT!***,
    --     ***!result_speciation_id := xxx::INT!***
    --   ) AS v1
)
SELECT CASE
  WHEN h IS NULL THEN NULL::numeric
  WHEN h <= ***!hardness_breakpoint!*** THEN ***!value_or_equation!***::numeric
  ELSE ***!fallback_value_or_equation!***::numeric
END
FROM vals",
        ""
      )
    }
    add_hardness_template_input <- function() {
      df <- collect_rule_input_rows(include_empty = TRUE)
      df <- normalize_rule_input_rows(df)
      has_hardness <- vapply(
        seq_len(nrow(df)),
        function(i) is_hardness_rule_input(df[i, , drop = FALSE]),
        logical(1)
      )
      if (any(has_hardness)) {
        return(FALSE)
      }

      new_row <- blank_rule_input_rows(1L)
      new_row$input_source <- "hardness_helper"
      new_row$parameter_id <- hardness_parameter_id()
      new_row$matrix_state_id <- integer_or_na(input$matrix_state)
      if (is.na(new_row$matrix_state_id)) {
        new_row$matrix_state_id <- liquid_matrix_state_id()
      }
      new_row$input_name <- default_rule_input_name(
        new_row$parameter_id,
        new_row$input_source
      )
      new_row$input_code <- default_rule_input_code(
        new_row$parameter_id,
        new_row$input_source
      )

      has_existing_input <- !is.na(df$parameter_id)
      if (!any(has_existing_input)) {
        df <- new_row
      } else {
        df <- rbind(df[has_existing_input, , drop = FALSE], new_row)
      }
      set_rule_input_rows(df)
      TRUE
    }
    pending_sql_template <- reactiveVal(NULL)
    observeEvent(
      input$sql_template,
      {
        req(input$sql_template)
        pending_sql_template(input$sql_template)
        updateSelectizeInput(session, "sql_template", selected = character(0))
        showModal(modalDialog(
          "Inserting a template will overwrite the SQL scalar editor. You can edit the template after inserting it.",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              ns("confirm_sql_template"),
              "Insert template",
              class = "btn-primary"
            )
          )
        ))
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$confirm_sql_template,
      {
        req(pending_sql_template())
        removeModal()
        template_code <- pending_sql_template()
        added_input <- FALSE
        if (identical(template_code, "hardness")) {
          added_input <- add_hardness_template_input()
        }
        update_formula_sql(sql_scalar_template(template_code))
        pending_sql_template(NULL)
        showModal(modalDialog(
          paste(
            "Template inserted. Replace placeholders marked with ***!...!*** and make sure the final query returns one numeric value.",
            if (isTRUE(added_input)) {
              "A matching hardness Rule input was also added."
            } else {
              ""
            }
          ),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      },
      ignoreInit = TRUE
    )

    resolve_publisher_id <- function(value) {
      if (is_blank(value)) {
        stop("Publisher is required.", call. = FALSE)
      }
      value <- text_value(value)
      existing <- suppressWarnings(as.integer(value))
      if (
        !is.na(existing) && existing %in% moduleData$publishers$publisher_id
      ) {
        return(existing)
      }

      existing <- DBI::dbGetQuery(
        con,
        "SELECT publisher_id
         FROM criteria.guideline_publishers
         WHERE lower(btrim(publisher_name)) = lower(btrim($1))
         ORDER BY publisher_id
         LIMIT 1",
        params = list(value)
      )
      if (nrow(existing)) {
        return(existing$publisher_id[[1]])
      }

      inserted <- DBI::dbGetQuery(
        con,
        "WITH inserted AS (
           INSERT INTO criteria.guideline_publishers (
             publisher_code, publisher_name
           )
           VALUES ('PUB_' || upper(left(md5($1), 10)), btrim($1))
           ON CONFLICT DO NOTHING
           RETURNING publisher_id
         )
         SELECT publisher_id FROM inserted
         UNION ALL
         SELECT publisher_id
         FROM criteria.guideline_publishers
         WHERE lower(btrim(publisher_name)) = lower(btrim($1))
         ORDER BY publisher_id
         LIMIT 1",
        params = list(value)
      )
      if (nrow(inserted)) {
        return(inserted$publisher_id[[1]])
      }
      DBI::dbGetQuery(
        con,
        "SELECT publisher_id
         FROM criteria.guideline_publishers
         WHERE lower(btrim(publisher_name)) = lower(btrim($1))
         ORDER BY publisher_id
         LIMIT 1",
        params = list(value)
      )$publisher_id[[1]]
    }
    resolve_series_id <- function(value, publisher_id) {
      if (is_blank(value)) {
        return(NA_integer_)
      }
      value <- text_value(value)
      existing <- suppressWarnings(as.integer(value))
      if (!is.na(existing) && existing %in% moduleData$series$series_id) {
        return(existing)
      }

      existing <- DBI::dbGetQuery(
        con,
        "SELECT series_id
         FROM criteria.guideline_series
         WHERE lower(btrim(series_name)) = lower(btrim($1))
           AND publisher_id IS NOT DISTINCT FROM $2
         ORDER BY series_id
         LIMIT 1",
        params = list(value, publisher_id)
      )
      if (nrow(existing)) {
        return(existing$series_id[[1]])
      }

      inserted <- DBI::dbGetQuery(
        con,
        "WITH inserted AS (
           INSERT INTO criteria.guideline_series (
             series_code, series_name, publisher_id
           )
           VALUES ('SER_' || upper(left(md5($1), 10)), btrim($1), $2)
           ON CONFLICT DO NOTHING
           RETURNING series_id
         )
         SELECT series_id FROM inserted
         UNION ALL
         SELECT series_id
         FROM criteria.guideline_series
         WHERE lower(btrim(series_name)) = lower(btrim($1))
         ORDER BY series_id
         LIMIT 1",
        params = list(value, publisher_id)
      )
      if (nrow(inserted)) {
        return(inserted$series_id[[1]])
      }
      DBI::dbGetQuery(
        con,
        "SELECT series_id
         FROM criteria.guideline_series
         WHERE lower(btrim(series_name)) = lower(btrim($1))
         ORDER BY series_id
         LIMIT 1",
        params = list(value)
      )$series_id[[1]]
    }
    target_operator <- function(type) {
      switch(
        type,
        constant_upper = "lte",
        constant_lower = "gte",
        constant_range = if (identical(input$comparison_operator, "eq")) {
          "eq"
        } else {
          "range"
        },
        narrative = "narrative",
        input$comparison_operator %||% "lte"
      )
    }

    insert_rule <- function(
      guideline_id,
      bound_code,
      algorithm_code,
      fixed_value = NA_real_,
      formula_sql = NA_character_,
      priority = 100L
    ) {
      DBI::dbGetQuery(
        con,
        "INSERT INTO criteria.guideline_value_rules (
           guideline_id, bound_code, algorithm_code, fixed_value,
           formula_sql, min_output_value, max_output_value, rounding_digits,
           rounding_method, missing_input_policy, rule_priority,
           precision_note, note
         )
         VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)
         RETURNING rule_id",
        params = list(
          guideline_id,
          text_or_na(bound_code),
          algorithm_code,
          fixed_value,
          text_or_na(formula_sql),
          numeric_or_na(input$min_output_value),
          numeric_or_na(input$max_output_value),
          integer_or_na(input$rounding_digits),
          input$rounding_method %||% "none",
          input$missing_input_policy %||% "no_value",
          if (is.na(priority)) 100L else priority,
          text_or_na(input$precision_note),
          text_or_na(input$rule_note)
        )
      )$rule_id[[1]]
    }

    strip_sql_for_input_scan <- function(sql) {
      scan <- clean_sql_scalar(sql)
      scan <- gsub("(?s)\\$[^$]*\\$.*?\\$[^$]*\\$", " ", scan, perl = TRUE)
      scan <- gsub("'([^'\\\\]|\\\\.)*'", " ", scan, perl = TRUE)
      scan <- gsub("--.*?(\\r?\\n|$)", " ", scan, perl = TRUE)
      scan <- gsub("/\\*.*?\\*/", " ", scan, perl = TRUE)
      scan
    }
    sql_int_literal <- function(x) {
      if (is.null(x) || !length(x) || is.na(x)) {
        return(NA_integer_)
      }
      x <- trimws(as.character(x[[1]]))
      x <- gsub(
        "::\\s*(integer|int)\\b",
        "",
        x,
        ignore.case = TRUE,
        perl = TRUE
      )
      x <- trimws(x)
      if (!nzchar(x) || grepl("^null$", x, ignore.case = TRUE)) {
        return(NA_integer_)
      }
      out <- suppressWarnings(as.integer(x))
      if (is.na(out)) NA_integer_ else out
    }
    sql_named_int_arg <- function(args, name) {
      pattern <- paste0(
        "\\b",
        name,
        "\\s*:=\\s*(NULL|-?[0-9]+\\s*(?:::?\\s*(?:integer|int))?)"
      )
      hit <- regexec(pattern, args, perl = TRUE, ignore.case = TRUE)
      match <- regmatches(args, hit)[[1]]
      if (length(match) < 2L) {
        return(NULL)
      }
      match[[2]]
    }
    sql_arg_value <- function(args, name, position) {
      named <- sql_named_int_arg(args, name)
      if (!is.null(named)) {
        return(sql_int_literal(named))
      }
      parts <- trimws(strsplit(args, ",", fixed = TRUE)[[1]])
      if (length(parts) < position) {
        return(NA_integer_)
      }
      sql_int_literal(parts[[position]])
    }
    sql_ref_key <- function(
      parameter_id,
      sample_fraction_id,
      result_speciation_id
    ) {
      paste(
        ifelse(
          is.na(parameter_id),
          "NULL",
          as.character(as.integer(parameter_id))
        ),
        ifelse(
          is.na(sample_fraction_id),
          "NULL",
          as.character(as.integer(sample_fraction_id))
        ),
        ifelse(
          is.na(result_speciation_id),
          "NULL",
          as.character(as.integer(result_speciation_id))
        ),
        sep = "|"
      )
    }
    format_sql_ref_key <- function(key) {
      parts <- strsplit(key, "|", fixed = TRUE)[[1]]
      paste0(
        "parameter_id=",
        parts[[1]],
        ", sample_fraction_id=",
        parts[[2]],
        ", result_speciation_id=",
        parts[[3]]
      )
    }
    sql_scalar_input_refs <- function(sql) {
      scan <- strip_sql_for_input_scan(sql)
      call_hits <- gregexpr(
        "\\bdiscrete\\.get_sample_val\\s*\\(([^()]*)\\)",
        scan,
        perl = TRUE,
        ignore.case = TRUE
      )
      calls <- regmatches(scan, call_hits)[[1]]
      if (length(calls) == 1L && identical(calls, "-1")) {
        calls <- character(0)
      }
      sample_refs <- character(0)
      if (length(calls)) {
        sample_refs <- vapply(
          calls,
          function(call) {
            args <- sub("^.*?\\((.*)\\)\\s*$", "\\1", call, perl = TRUE)
            sql_ref_key(
              sql_arg_value(args, "parameter_id", 2L),
              sql_arg_value(args, "sample_fraction_id", 3L),
              sql_arg_value(args, "result_speciation_id", 4L)
            )
          },
          character(1)
        )
      }
      list(
        sample_refs = unique(sample_refs),
        uses_hardness = grepl(
          "\\bcriteria\\.get_sample_hardness\\s*\\(",
          scan,
          perl = TRUE,
          ignore.case = TRUE
        ),
        uses_sample_id = grepl("\\$1\\b", scan, perl = TRUE)
      )
    }
    validate_sql_scalar_inputs <- function(sql, inputs) {
      refs <- sql_scalar_input_refs(sql)
      has_inputs <- !is.null(inputs) && nrow(inputs) > 0L
      helper_count <- length(refs$sample_refs) + as.integer(refs$uses_hardness)

      if (!has_inputs) {
        if (helper_count > 0L || refs$uses_sample_id) {
          stop(
            "SQL scalar rules that use sample data must list matching Rule inputs.",
            call. = FALSE
          )
        }
        return(invisible(NULL))
      }

      input_is_hardness <- vapply(
        seq_len(nrow(inputs)),
        function(i) is_hardness_rule_input(inputs[i, , drop = FALSE]),
        logical(1)
      )
      declared_hardness <- any(input_is_hardness)
      declared_hardness_helper <- any(
        inputs$input_source %in% "hardness_helper"
      )
      declared_samples <- inputs[
        inputs$input_source %in% "sample_result",
        ,
        drop = FALSE
      ]
      if (refs$uses_hardness && nrow(declared_samples)) {
        sample_hardness <- vapply(
          seq_len(nrow(declared_samples)),
          function(i) {
            is_hardness_rule_input(declared_samples[i, , drop = FALSE])
          },
          logical(1)
        )
        declared_samples <- declared_samples[!sample_hardness, , drop = FALSE]
      }
      declared_refs <- unique(sql_ref_key(
        declared_samples$parameter_id,
        declared_samples$sample_fraction_id,
        declared_samples$result_speciation_id
      ))
      declared_refs <- declared_refs[!grepl("^NULL\\|", declared_refs)]

      if (refs$uses_sample_id && helper_count == 0L) {
        stop(
          "SQL scalar rules with Rule inputs must fetch sample chemistry with discrete.get_sample_val() or criteria.get_sample_hardness().",
          call. = FALSE
        )
      }
      if (refs$uses_hardness && !declared_hardness) {
        stop(
          "SQL scalar uses criteria.get_sample_hardness(), but no hardness Rule input is listed.",
          call. = FALSE
        )
      }
      if (declared_hardness_helper && !refs$uses_hardness) {
        stop(
          "A hardness helper Rule input is listed, but the SQL scalar does not call criteria.get_sample_hardness().",
          call. = FALSE
        )
      }

      extra_refs <- setdiff(refs$sample_refs, declared_refs)
      if (length(extra_refs)) {
        stop(
          "SQL scalar uses sample input(s) that are not listed as Rule inputs: ",
          paste(
            vapply(extra_refs, format_sql_ref_key, character(1)),
            collapse = "; "
          ),
          call. = FALSE
        )
      }

      missing_refs <- setdiff(declared_refs, refs$sample_refs)
      if (length(missing_refs)) {
        stop(
          "Rule input(s) are listed but not used by the SQL scalar: ",
          paste(
            vapply(missing_refs, format_sql_ref_key, character(1)),
            collapse = "; "
          ),
          call. = FALSE
        )
      }
      invisible(NULL)
    }

    save_rule_inputs <- function(
      rule_id,
      require_inputs = FALSE,
      formula_sql = NULL
    ) {
      df <- collect_rule_input_rows()
      if (!nrow(df)) {
        if (!is.null(formula_sql)) {
          validate_sql_scalar_inputs(formula_sql, df)
        }
        if (require_inputs) {
          stop("At least one rule input is required.", call. = FALSE)
        }
        return(df)
      }
      for (i in seq_len(nrow(df))) {
        df$input_source[i] <- text_default(df$input_source[i], "sample_result")
        df$input_name[i] <- default_rule_input_name(
          df$parameter_id[i],
          df$input_source[i]
        )
        df$input_code[i] <- default_rule_input_code(
          df$parameter_id[i],
          df$input_source[i],
          fallback = paste0("input_", i)
        )
      }
      df$input_code <- make_unique_input_codes(df$input_code)
      df <- df[!is.na(df$parameter_id), , drop = FALSE]
      if (!nrow(df)) {
        if (!is.null(formula_sql)) {
          validate_sql_scalar_inputs(formula_sql, df)
        }
        if (require_inputs) {
          stop("At least one rule input is required.", call. = FALSE)
        }
        return(df)
      }
      if (!is.null(formula_sql)) {
        validate_sql_scalar_inputs(formula_sql, df)
      }
      for (i in seq_len(nrow(df))) {
        is_helper <- identical(
          text_default(df$input_source[i], "sample_result"),
          "hardness_helper"
        )
        if (
          !is_helper &&
            parameter_requires_fraction(df$parameter_id[i]) &&
            is.na(integer_or_na(df$sample_fraction_id[i]))
        ) {
          stop(
            "Fraction is required for input '",
            df$input_name[i],
            "'.",
            call. = FALSE
          )
        }
        if (
          !is_helper &&
            parameter_requires_speciation(df$parameter_id[i]) &&
            is.na(integer_or_na(df$result_speciation_id[i]))
        ) {
          stop(
            "Speciation is required for input '",
            df$input_name[i],
            "'.",
            call. = FALSE
          )
        }
        DBI::dbExecute(
          con,
          "INSERT INTO criteria.guideline_rule_inputs (
             rule_id, input_code, input_name, input_source,
             parameter_id, matrix_state_id, sample_fraction_id,
             result_speciation_id, result_type, result_type_preference,
             aggregate_method, required, allow_condition_value,
             lower_calibrated_bound, upper_calibrated_bound,
             bounds_action, note
           )
           VALUES (
             $1, $2, $3, $4, $5, $6, $7, $8, $9,
             CASE
               WHEN NULLIF($10, '') IS NULL THEN NULL::integer[]
               ELSE string_to_array($10, ',')::integer[]
             END,
             $11, $12, $13, $14, $15, $16, $17
           )",
          params = list(
            rule_id,
            text_value(df$input_code[i]),
            text_or_na(df$input_name[i]),
            text_default(df$input_source[i], "sample_result"),
            integer_or_na(df$parameter_id[i]),
            integer_or_na(df$matrix_state_id[i]),
            integer_or_na(df$sample_fraction_id[i]),
            integer_or_na(df$result_speciation_id[i]),
            integer_or_na(df$result_type[i]),
            text_value(df$result_type_preference[i]),
            text_default(df$aggregate_method[i], "single"),
            bool_value(df$required[i], TRUE),
            bool_value(df$allow_condition_value[i], FALSE),
            numeric_or_na(df$lower_calibrated_bound[i]),
            numeric_or_na(df$upper_calibrated_bound[i]),
            text_default(df$bounds_action[i], "flag"),
            text_or_na(df$note[i])
          )
        )
      }
      df
    }

    save_coefficients <- function(rule_id, required = FALSE) {
      df <- parse_table_text(input$coefficients_text, coef_cols, "Coefficients")
      if (!nrow(df)) {
        if (required) {
          stop("This rule requires coefficients.", call. = FALSE)
        }
        return(invisible(NULL))
      }
      for (i in seq_len(nrow(df))) {
        if (!nzchar(text_value(df$coefficient_name[i]))) {
          next
        }
        DBI::dbExecute(
          con,
          "INSERT INTO criteria.guideline_rule_coefficients (
             rule_id, coefficient_name, coefficient_value, note
           )
           VALUES ($1, $2, $3, $4)",
          params = list(
            rule_id,
            text_value(df$coefficient_name[i]),
            numeric_or_na(df$coefficient_value[i]),
            text_or_na(df$note[i])
          )
        )
      }
    }

    save_narrative_values <- function(guideline_id) {
      df <- parse_table_text(
        input$narrative_values_text,
        narrative_cols,
        "Narrative values"
      )
      DBI::dbExecute(
        con,
        "DELETE FROM criteria.guideline_narrative_values WHERE guideline_id = $1",
        params = list(guideline_id)
      )
      if (!nrow(df)) {
        return(invisible(NULL))
      }
      for (i in seq_len(nrow(df))) {
        value_code <- text_value(df$value_code[i])
        condition_label <- text_value(df$condition_label[i])
        if (!nzchar(value_code) && !nzchar(condition_label)) {
          next
        }
        if (!nzchar(value_code) || !nzchar(condition_label)) {
          stop(
            "Narrative value rows require both value_code and condition_label.",
            call. = FALSE
          )
        }
        DBI::dbExecute(
          con,
          "INSERT INTO criteria.guideline_narrative_values (
             guideline_id, value_code, condition_label, max_change_value,
             max_change_percent, change_unit, background_lower_bound,
             background_upper_bound, background_unit, duration_label,
             flow_condition, sort_order, note
           )
           VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)",
          params = list(
            guideline_id,
            value_code,
            condition_label,
            numeric_or_na(df$max_change_value[i]),
            numeric_or_na(df$max_change_percent[i]),
            text_or_na(df$change_unit[i]),
            numeric_or_na(df$background_lower_bound[i]),
            numeric_or_na(df$background_upper_bound[i]),
            text_or_na(df$background_unit[i]),
            text_or_na(df$duration_label[i]),
            text_or_na(df$flow_condition[i]),
            integer_or_na(df$sort_order[i]) %||% (i * 10L),
            text_or_na(df$note[i])
          )
        )
      }
      invisible(NULL)
    }

    save_rules <- function(guideline_id, operator) {
      type <- input$guideline_type %||% "constant_upper"
      rule_bound <- bound_for_operator(operator)
      DBI::dbExecute(
        con,
        "DELETE FROM criteria.guideline_value_rules WHERE guideline_id = $1",
        params = list(guideline_id)
      )
      if (!identical(type, "narrative")) {
        DBI::dbExecute(
          con,
          "DELETE FROM criteria.guideline_narrative_values WHERE guideline_id = $1",
          params = list(guideline_id)
        )
      }
      if (type == "constant_upper") {
        value <- numeric_or_na(input$fixed_value)
        if (is.na(value)) {
          stop("Fixed value is required.", call. = FALSE)
        }
        insert_rule(guideline_id, "upper", "constant", fixed_value = value)
      } else if (type == "constant_lower") {
        value <- numeric_or_na(input$fixed_value)
        if (is.na(value)) {
          stop("Fixed value is required.", call. = FALSE)
        }
        insert_rule(guideline_id, "lower", "constant", fixed_value = value)
      } else if (type == "constant_range") {
        lower <- numeric_or_na(input$lower_value)
        upper <- numeric_or_na(input$upper_value)
        if (is.na(lower) || is.na(upper)) {
          stop("Lower and upper values are required.", call. = FALSE)
        }
        insert_rule(
          guideline_id,
          "lower",
          "constant",
          fixed_value = lower,
          priority = 10L
        )
        insert_rule(
          guideline_id,
          "upper",
          "constant",
          fixed_value = upper,
          priority = 20L
        )
      } else if (type == "narrative") {
        insert_rule(guideline_id, NA_character_, "narrative")
        save_narrative_values(guideline_id)
      } else if (type == "sql_scalar") {
        if (!nzchar(text_value(input$formula_sql))) {
          stop("SQL scalar text is required.", call. = FALSE)
        }
        if (identical(operator, "eq")) {
          lower_rule_id <- insert_rule(
            guideline_id,
            "lower",
            "sql_scalar",
            formula_sql = input$formula_sql,
            priority = 10L
          )
          upper_rule_id <- insert_rule(
            guideline_id,
            "upper",
            "sql_scalar",
            formula_sql = input$formula_sql,
            priority = 20L
          )
          save_rule_inputs(
            lower_rule_id,
            require_inputs = FALSE,
            formula_sql = input$formula_sql
          )
          save_rule_inputs(
            upper_rule_id,
            require_inputs = FALSE,
            formula_sql = input$formula_sql
          )
        } else {
          rule_id <- insert_rule(
            guideline_id,
            rule_bound,
            "sql_scalar",
            formula_sql = input$formula_sql
          )
          save_rule_inputs(
            rule_id,
            require_inputs = FALSE,
            formula_sql = input$formula_sql
          )
        }
      } else if (type == "single_input_formula") {
        rule_id <- insert_rule(
          guideline_id,
          rule_bound,
          input$formula_algorithm %||% "linear"
        )
        save_rule_inputs(rule_id, require_inputs = TRUE)
        save_coefficients(rule_id, required = TRUE)
      } else {
        stop("Unsupported guideline type: ", type, call. = FALSE)
      }
    }

    save_rule_inputs_snapshot <- function(formula_sql = NULL) {
      df <- collect_rule_input_rows()
      if (!nrow(df)) {
        if (!is.null(formula_sql)) {
          validate_sql_scalar_inputs(formula_sql, df)
        }
        return(df)
      }
      for (i in seq_len(nrow(df))) {
        df$input_source[i] <- text_default(df$input_source[i], "sample_result")
        df$input_name[i] <- default_rule_input_name(
          df$parameter_id[i],
          df$input_source[i]
        )
        df$input_code[i] <- default_rule_input_code(
          df$parameter_id[i],
          df$input_source[i],
          fallback = paste0("input_", i)
        )
      }
      df$input_code <- make_unique_input_codes(df$input_code)
      df <- df[!is.na(df$parameter_id), , drop = FALSE]
      if (!is.null(formula_sql)) {
        validate_sql_scalar_inputs(formula_sql, df)
      }
      for (i in seq_len(nrow(df))) {
        is_helper <- identical(
          text_default(df$input_source[i], "sample_result"),
          "hardness_helper"
        )
        if (
          !is_helper &&
            parameter_requires_fraction(df$parameter_id[i]) &&
            is.na(integer_or_na(df$sample_fraction_id[i]))
        ) {
          stop(
            "Fraction is required for input '",
            df$input_name[i],
            "'.",
            call. = FALSE
          )
        }
        if (
          !is_helper &&
            parameter_requires_speciation(df$parameter_id[i]) &&
            is.na(integer_or_na(df$result_speciation_id[i]))
        ) {
          stop(
            "Speciation is required for input '",
            df$input_name[i],
            "'.",
            call. = FALSE
          )
        }
      }
      df
    }
    build_save_guideline_request <- function() {
      parameter_id <- integer_or_na(input$parameter_id)
      matrix_state_id <- integer_or_na(input$matrix_state)
      if (is.na(parameter_id)) {
        stop("Parameter is required.", call. = FALSE)
      }
      if (is.na(matrix_state_id)) {
        stop("Matrix state is required.", call. = FALSE)
      }
      if (is.na(parameter_unit_id_for_id(parameter_id, matrix_state_id))) {
        stop(
          "Units are required for ",
          parameter_name_for_id(parameter_id),
          " in ",
          matrix_state_name_for_id(matrix_state_id),
          " before this guideline can be saved.",
          call. = FALSE
        )
      }
      if (!nzchar(text_value(input$guideline_name))) {
        stop("Guideline name is required.", call. = FALSE)
      }
      type <- input$guideline_type %||% "constant_upper"
      operator <- target_operator(type)
      formula_sql <- clean_sql_scalar(input$formula_sql)
      if (identical(type, "sql_scalar")) {
        YGwater:::validate_guideline_sql_scalar(formula_sql)
      }
      rule_inputs <- if (type %in% c("sql_scalar", "single_input_formula")) {
        save_rule_inputs_snapshot(
          formula_sql = if (identical(type, "sql_scalar")) formula_sql else NULL
        )
      } else {
        blank_rule_input_rows(0L)
      }
      coefficients <- if (identical(type, "single_input_formula")) {
        parse_table_text(input$coefficients_text, coef_cols, "Coefficients")
      } else {
        data.frame()
      }
      narrative_values <- if (identical(type, "narrative")) {
        parse_table_text(
          input$narrative_values_text,
          narrative_cols,
          "Narrative values"
        )
      } else {
        data.frame()
      }
      guideline_code <- text_value(input$guideline_code)
      if (!nzchar(guideline_code)) {
        guideline_code <- normalize_code(input$guideline_name)
      }
      list(
        config = session$userData$config,
        guideline_id = selected_guideline_id(),
        guideline_code = guideline_code,
        guideline_name = text_value(input$guideline_name),
        publisher = input$publisher,
        series = input$series,
        reference = text_or_na(input$reference),
        general_notes = text_or_na(input$general_notes),
        applicability_notes = text_or_na(input$applicability_notes),
        parameter_id = parameter_id,
        matrix_state_id = matrix_state_id,
        result_speciation_id = integer_or_na(input$result_speciation),
        comparison_operator_code = operator,
        jurisdiction = input$jurisdiction,
        protection_goal = input$protection_goal,
        exposure_duration = input$exposure_duration,
        averaging_period = input$averaging_period,
        source_document_title = text_or_na(input$source_document_title),
        source_url = text_or_na(input$source_url),
        source_page = text_or_na(input$source_page),
        source_table = text_or_na(input$source_table),
        source_section = text_or_na(input$source_section),
        source_effective_date = date_or_na(input$source_effective_date),
        source_retrieved_date = date_or_na(input$source_retrieved_date),
        valid_from = date_or_na(input$valid_from),
        valid_to = date_or_na(input$valid_to),
        review_status = input$review_status %||% "draft",
        active = bool_value(input$active, TRUE),
        fraction_ids = parse_id_vector(input$sample_fraction),
        media_type_ids = parse_id_vector(input$media_type),
        location_ids = parse_id_vector(input$specific_locations),
        guideline_type = type,
        rule_bound = bound_for_operator(operator),
        fixed_value = numeric_or_na(input$fixed_value),
        lower_value = numeric_or_na(input$lower_value),
        upper_value = numeric_or_na(input$upper_value),
        formula_algorithm = input$formula_algorithm %||% "linear",
        formula_sql = formula_sql,
        min_output_value = numeric_or_na(input$min_output_value),
        max_output_value = numeric_or_na(input$max_output_value),
        rounding_digits = integer_or_na(input$rounding_digits),
        rounding_method = input$rounding_method %||% "none",
        missing_input_policy = input$missing_input_policy %||% "no_value",
        precision_note = text_or_na(input$precision_note),
        rule_note = text_or_na(input$rule_note),
        rule_inputs = rule_inputs,
        coefficients = coefficients,
        narrative_values = narrative_values
      )
    }
    save_guideline_task <- ExtendedTask$new(function(req) {
      promises::future_promise(seed = TRUE, expr = {
        con <- NULL
        tryCatch(
          {
            con <- DBI::dbConnect(
              RPostgres::Postgres(),
              dbname = req$config$dbName,
              host = req$config$dbHost,
              port = req$config$dbPort,
              user = req$config$dbUser,
              password = req$config$dbPass
            )
            on.exit(DBI::dbDisconnect(con), add = TRUE)

            is_empty <- function(x) {
              is.null(x) ||
                !length(x) ||
                all(is.na(x)) ||
                !nzchar(trimws(as.character(x[[1]])))
            }
            int_or_na <- function(x) {
              if (is_empty(x)) {
                return(NA_integer_)
              }
              out <- suppressWarnings(as.integer(as.character(x[[1]])))
              if (is.na(out)) NA_integer_ else out
            }
            num_or_na <- function(x) {
              if (is_empty(x)) {
                return(NA_real_)
              }
              out <- suppressWarnings(as.numeric(as.character(x[[1]])))
              if (is.na(out)) NA_real_ else out
            }
            txt_or_na <- function(x) {
              if (is_empty(x)) {
                NA_character_
              } else {
                trimws(as.character(x[[1]]))
              }
            }
            txt_value <- function(x) {
              out <- txt_or_na(x)
              if (is.na(out)) "" else out
            }
            bool_value_worker <- function(x, default = FALSE) {
              if (is_empty(x)) {
                return(default)
              }
              value <- tolower(trimws(as.character(x[[1]])))
              if (value %in% c("true", "t", "1", "yes", "y")) {
                return(TRUE)
              }
              if (value %in% c("false", "f", "0", "no", "n")) {
                return(FALSE)
              }
              default
            }
            resolve_lookup <- function(
              value,
              table_name,
              id_col,
              text_col,
              code_col,
              code_prefix
            ) {
              id <- int_or_na(value)
              if (!is.na(id)) {
                return(id)
              }
              label <- txt_or_na(value)
              if (is.na(label)) {
                return(NA_integer_)
              }
              resolved <- DBI::dbGetQuery(
                con,
                sprintf(
                  "WITH existing AS (
                     SELECT %1$s
                     FROM %2$s
                     WHERE lower(btrim(%3$s)) = lower(btrim($1))
                     LIMIT 1
                   ),
                   inserted AS (
                     INSERT INTO %2$s (%4$s, %3$s, sort_order)
                     SELECT $2 || '_' || upper(left(md5($1), 10)), btrim($1), 800
                     WHERE NOT EXISTS (SELECT 1 FROM existing)
                     ON CONFLICT DO NOTHING
                     RETURNING %1$s
                   )
                   SELECT %1$s FROM inserted
                   UNION ALL
                   SELECT %1$s FROM existing
                   LIMIT 1",
                  id_col,
                  table_name,
                  text_col,
                  code_col
                ),
                params = list(label, code_prefix)
              )
              if (nrow(resolved)) {
                return(as.integer(resolved[[id_col]][[1]]))
              }
              stop("Could not resolve reference value '", label, "'.")
            }
            resolve_publisher <- function(value) {
              id <- int_or_na(value)
              if (!is.na(id)) {
                return(id)
              }
              label <- txt_or_na(value)
              if (is.na(label)) {
                stop("Publisher is required.")
              }
              resolved <- DBI::dbGetQuery(
                con,
                "WITH existing AS (
                   SELECT publisher_id
                   FROM criteria.guideline_publishers
                   WHERE lower(btrim(publisher_name)) = lower(btrim($1))
                   LIMIT 1
                 ),
                 inserted AS (
                   INSERT INTO criteria.guideline_publishers (
                     publisher_code, publisher_name
                   )
                   SELECT 'PUB_' || upper(left(md5($1), 10)), btrim($1)
                   WHERE NOT EXISTS (SELECT 1 FROM existing)
                   ON CONFLICT DO NOTHING
                   RETURNING publisher_id
                 )
                 SELECT publisher_id FROM inserted
                 UNION ALL
                 SELECT publisher_id FROM existing
                 LIMIT 1",
                params = list(label)
              )
              resolved$publisher_id[[1]]
            }
            resolve_series <- function(value, publisher_id) {
              id <- int_or_na(value)
              if (!is.na(id)) {
                return(id)
              }
              label <- txt_or_na(value)
              if (is.na(label)) {
                return(NA_integer_)
              }
              resolved <- DBI::dbGetQuery(
                con,
                "WITH existing AS (
                   SELECT series_id
                   FROM criteria.guideline_series
                   WHERE lower(btrim(series_name)) = lower(btrim($1))
                     AND publisher_id IS NOT DISTINCT FROM $2
                   LIMIT 1
                 ),
                 inserted AS (
                   INSERT INTO criteria.guideline_series (
                     series_code, series_name, publisher_id
                   )
                   SELECT 'SER_' || upper(left(md5($1), 10)), btrim($1), $2
                   WHERE NOT EXISTS (SELECT 1 FROM existing)
                   ON CONFLICT DO NOTHING
                   RETURNING series_id
                 )
                 SELECT series_id FROM inserted
                 UNION ALL
                 SELECT series_id FROM existing
                 LIMIT 1",
                params = list(label, publisher_id)
              )
              if (nrow(resolved)) resolved$series_id[[1]] else NA_integer_
            }
            insert_rule <- function(
              guideline_id,
              bound_code,
              algorithm_code,
              fixed_value = NA_real_,
              formula_sql = NA_character_,
              priority = 100L
            ) {
              DBI::dbGetQuery(
                con,
                "INSERT INTO criteria.guideline_value_rules (
                   guideline_id, bound_code, algorithm_code, fixed_value,
                   formula_sql, min_output_value, max_output_value,
                   rounding_digits, rounding_method, missing_input_policy,
                   rule_priority, precision_note, note
                 )
                 VALUES (
                   $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13
                 )
                 RETURNING rule_id",
                params = list(
                  guideline_id,
                  txt_or_na(bound_code),
                  algorithm_code,
                  fixed_value,
                  txt_or_na(formula_sql),
                  req$min_output_value,
                  req$max_output_value,
                  req$rounding_digits,
                  req$rounding_method,
                  req$missing_input_policy,
                  if (is.na(priority)) 100L else priority,
                  req$precision_note,
                  req$rule_note
                )
              )$rule_id[[1]]
            }
            save_inputs <- function(rule_id) {
              df <- req$rule_inputs
              if (is.null(df) || !nrow(df)) {
                return(invisible(NULL))
              }
              for (i in seq_len(nrow(df))) {
                DBI::dbExecute(
                  con,
                  "INSERT INTO criteria.guideline_rule_inputs (
                     rule_id, input_code, input_name, input_source,
                     parameter_id, matrix_state_id, sample_fraction_id,
                     result_speciation_id, result_type,
                     result_type_preference, aggregate_method, required,
                     allow_condition_value, lower_calibrated_bound,
                     upper_calibrated_bound, bounds_action, note
                   )
                   VALUES (
                     $1, $2, $3, $4, $5, $6, $7, $8, $9,
                     CASE
                       WHEN NULLIF($10, '') IS NULL THEN NULL::integer[]
                       ELSE string_to_array($10, ',')::integer[]
                     END,
                     $11, $12, $13, $14, $15, $16, $17
                   )",
                  params = list(
                    rule_id,
                    txt_value(df$input_code[i]),
                    txt_or_na(df$input_name[i]),
                    if (is_empty(df$input_source[i])) {
                      "sample_result"
                    } else {
                      txt_value(df$input_source[i])
                    },
                    int_or_na(df$parameter_id[i]),
                    int_or_na(df$matrix_state_id[i]),
                    int_or_na(df$sample_fraction_id[i]),
                    int_or_na(df$result_speciation_id[i]),
                    int_or_na(df$result_type[i]),
                    txt_value(df$result_type_preference[i]),
                    if (is_empty(df$aggregate_method[i])) {
                      "single"
                    } else {
                      txt_value(df$aggregate_method[i])
                    },
                    bool_value_worker(df$required[i], TRUE),
                    bool_value_worker(df$allow_condition_value[i], FALSE),
                    num_or_na(df$lower_calibrated_bound[i]),
                    num_or_na(df$upper_calibrated_bound[i]),
                    if (is_empty(df$bounds_action[i])) {
                      "flag"
                    } else {
                      txt_value(df$bounds_action[i])
                    },
                    txt_or_na(df$note[i])
                  )
                )
              }
            }
            save_coefficients <- function(rule_id) {
              df <- req$coefficients
              if (is.null(df) || !nrow(df)) {
                return(invisible(NULL))
              }
              for (i in seq_len(nrow(df))) {
                if (!nzchar(txt_value(df$coefficient_name[i]))) {
                  next
                }
                DBI::dbExecute(
                  con,
                  "INSERT INTO criteria.guideline_rule_coefficients (
                     rule_id, coefficient_name, coefficient_value, note
                   )
                   VALUES ($1, $2, $3, $4)",
                  params = list(
                    rule_id,
                    txt_value(df$coefficient_name[i]),
                    num_or_na(df$coefficient_value[i]),
                    txt_or_na(df$note[i])
                  )
                )
              }
            }
            save_narrative <- function(guideline_id) {
              df <- req$narrative_values
              if (is.null(df) || !nrow(df)) {
                return(invisible(NULL))
              }
              for (i in seq_len(nrow(df))) {
                value_code <- txt_value(df$value_code[i])
                condition_label <- txt_value(df$condition_label[i])
                if (!nzchar(value_code) && !nzchar(condition_label)) {
                  next
                }
                if (!nzchar(value_code) || !nzchar(condition_label)) {
                  stop(
                    "Narrative value rows require both value_code and condition_label."
                  )
                }
                DBI::dbExecute(
                  con,
                  "INSERT INTO criteria.guideline_narrative_values (
                     guideline_id, value_code, condition_label,
                     max_change_value, max_change_percent, change_unit,
                     background_lower_bound, background_upper_bound,
                     background_unit, duration_label, flow_condition,
                     sort_order, note
                   )
                   VALUES (
                     $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13
                   )",
                  params = list(
                    guideline_id,
                    value_code,
                    condition_label,
                    num_or_na(df$max_change_value[i]),
                    num_or_na(df$max_change_percent[i]),
                    txt_or_na(df$change_unit[i]),
                    num_or_na(df$background_lower_bound[i]),
                    num_or_na(df$background_upper_bound[i]),
                    txt_or_na(df$background_unit[i]),
                    txt_or_na(df$duration_label[i]),
                    txt_or_na(df$flow_condition[i]),
                    int_or_na(df$sort_order[i]),
                    txt_or_na(df$note[i])
                  )
                )
              }
            }

            DBI::dbBegin(con)
            committed <- FALSE
            on.exit(
              {
                if (!committed && !is.null(con)) {
                  try(DBI::dbRollback(con), silent = TRUE)
                }
              },
              add = TRUE
            )

            publisher_id <- resolve_publisher(req$publisher)
            series_id <- resolve_series(req$series, publisher_id)
            jurisdiction_id <- resolve_lookup(
              req$jurisdiction,
              "criteria.guideline_jurisdictions",
              "jurisdiction_id",
              "jurisdiction_name",
              "jurisdiction_code",
              "JUR"
            )
            protection_goal_id <- resolve_lookup(
              req$protection_goal,
              "criteria.guideline_protection_goals",
              "protection_goal_id",
              "protection_goal_name",
              "protection_goal_code",
              "GOAL"
            )
            exposure_duration_id <- resolve_lookup(
              req$exposure_duration,
              "criteria.guideline_exposure_durations",
              "exposure_duration_id",
              "exposure_duration_name",
              "exposure_duration_code",
              "EXPOSURE"
            )
            averaging_period_id <- resolve_lookup(
              req$averaging_period,
              "criteria.guideline_averaging_periods",
              "averaging_period_id",
              "averaging_period_name",
              "averaging_period_code",
              "AVG"
            )

            guideline_id <- int_or_na(req$guideline_id)
            guideline_params <- list(
              req$guideline_code,
              req$guideline_name,
              publisher_id,
              series_id,
              req$reference,
              req$general_notes,
              req$applicability_notes,
              req$parameter_id,
              req$matrix_state_id,
              req$result_speciation_id,
              req$comparison_operator_code,
              jurisdiction_id,
              protection_goal_id,
              exposure_duration_id,
              averaging_period_id,
              req$source_document_title,
              req$source_url,
              req$source_page,
              req$source_table,
              req$source_section,
              req$source_effective_date,
              req$source_retrieved_date,
              req$valid_from,
              req$valid_to,
              req$review_status,
              req$active
            )
            if (is.na(guideline_id)) {
              guideline_id <- DBI::dbGetQuery(
                con,
                "INSERT INTO criteria.guidelines (
                   guideline_code, guideline_name, publisher_id, series_id,
                   reference, general_notes, applicability_notes,
                   parameter_id, matrix_state_id, result_speciation_id,
                   comparison_operator_code, jurisdiction_id,
                   protection_goal_id, exposure_duration_id,
                   averaging_period_id, source_document_title, source_url,
                   source_page, source_table, source_section,
                   source_effective_date, source_retrieved_date, valid_from,
                   valid_to, review_status, active
                 )
                 VALUES (
                   $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12,
                   $13, $14, $15, $16, $17, $18, $19, $20, $21, $22,
                   $23, $24, $25, $26
                 )
                 RETURNING guideline_id",
                params = guideline_params
              )$guideline_id[[1]]
            } else {
              DBI::dbExecute(
                con,
                "UPDATE criteria.guidelines
                 SET guideline_code = $1, guideline_name = $2,
                     publisher_id = $3, series_id = $4, reference = $5,
                     general_notes = $6, applicability_notes = $7,
                     parameter_id = $8, matrix_state_id = $9,
                     result_speciation_id = $10,
                     comparison_operator_code = $11, jurisdiction_id = $12,
                     protection_goal_id = $13, exposure_duration_id = $14,
                     averaging_period_id = $15,
                     source_document_title = $16, source_url = $17,
                     source_page = $18, source_table = $19,
                     source_section = $20, source_effective_date = $21,
                     source_retrieved_date = $22, valid_from = $23,
                     valid_to = $24, review_status = $25, active = $26
                 WHERE guideline_id = $27",
                params = c(guideline_params, list(guideline_id))
              )
            }

            DBI::dbExecute(
              con,
              "DELETE FROM criteria.guidelines_fractions WHERE guideline_id = $1",
              params = list(guideline_id)
            )
            for (fraction_id in req$fraction_ids) {
              DBI::dbExecute(
                con,
                "INSERT INTO criteria.guidelines_fractions (
                   guideline_id, fraction_id
                 )
                 VALUES ($1, $2)",
                params = list(guideline_id, fraction_id)
              )
            }
            DBI::dbExecute(
              con,
              "DELETE FROM criteria.guidelines_media_types WHERE guideline_id = $1",
              params = list(guideline_id)
            )
            for (media_id in req$media_type_ids) {
              DBI::dbExecute(
                con,
                "INSERT INTO criteria.guidelines_media_types (
                   guideline_id, media_id
                 )
                 VALUES ($1, $2)",
                params = list(guideline_id, media_id)
              )
            }
            DBI::dbExecute(
              con,
              "DELETE FROM criteria.guideline_locations WHERE guideline_id = $1",
              params = list(guideline_id)
            )
            for (location_id in req$location_ids) {
              DBI::dbExecute(
                con,
                "INSERT INTO criteria.guideline_locations (
                   guideline_id, location_id
                 )
                 VALUES ($1, $2)",
                params = list(guideline_id, location_id)
              )
            }

            DBI::dbExecute(
              con,
              "DELETE FROM criteria.guideline_value_rules WHERE guideline_id = $1",
              params = list(guideline_id)
            )
            DBI::dbExecute(
              con,
              "DELETE FROM criteria.guideline_narrative_values WHERE guideline_id = $1",
              params = list(guideline_id)
            )

            type <- req$guideline_type
            if (identical(type, "constant_upper")) {
              if (is.na(req$fixed_value)) {
                stop("Fixed value is required.")
              }
              insert_rule(guideline_id, "upper", "constant", req$fixed_value)
            } else if (identical(type, "constant_lower")) {
              if (is.na(req$fixed_value)) {
                stop("Fixed value is required.")
              }
              insert_rule(guideline_id, "lower", "constant", req$fixed_value)
            } else if (identical(type, "constant_range")) {
              if (is.na(req$lower_value) || is.na(req$upper_value)) {
                stop("Lower and upper values are required.")
              }
              insert_rule(
                guideline_id,
                "lower",
                "constant",
                req$lower_value,
                priority = 10L
              )
              insert_rule(
                guideline_id,
                "upper",
                "constant",
                req$upper_value,
                priority = 20L
              )
            } else if (identical(type, "narrative")) {
              insert_rule(guideline_id, NA_character_, "narrative")
              save_narrative(guideline_id)
            } else if (identical(type, "sql_scalar")) {
              if (!nzchar(req$formula_sql)) {
                stop("SQL scalar text is required.")
              }
              if (identical(req$comparison_operator_code, "eq")) {
                lower_id <- insert_rule(
                  guideline_id,
                  "lower",
                  "sql_scalar",
                  formula_sql = req$formula_sql,
                  priority = 10L
                )
                upper_id <- insert_rule(
                  guideline_id,
                  "upper",
                  "sql_scalar",
                  formula_sql = req$formula_sql,
                  priority = 20L
                )
                save_inputs(lower_id)
                save_inputs(upper_id)
              } else {
                rule_id <- insert_rule(
                  guideline_id,
                  req$rule_bound,
                  "sql_scalar",
                  formula_sql = req$formula_sql
                )
                save_inputs(rule_id)
              }
            } else if (identical(type, "single_input_formula")) {
              rule_id <- insert_rule(
                guideline_id,
                req$rule_bound,
                req$formula_algorithm
              )
              if (is.null(req$rule_inputs) || !nrow(req$rule_inputs)) {
                stop("At least one rule input is required.")
              }
              if (is.null(req$coefficients) || !nrow(req$coefficients)) {
                stop("This rule requires coefficients.")
              }
              save_inputs(rule_id)
              save_coefficients(rule_id)
            } else {
              stop("Unsupported guideline type: ", type)
            }

            DBI::dbCommit(con)
            committed <- TRUE
            list(ok = TRUE, guideline_id = guideline_id)
          },
          error = function(e) {
            if (!is.null(con)) {
              try(DBI::dbRollback(con), silent = TRUE)
            }
            list(ok = FALSE, error = conditionMessage(e))
          }
        )
      })
    }) |>
      bslib::bind_task_button("save_guideline")

    observeEvent(input$save_guideline, {
      req <- tryCatch(
        build_save_guideline_request(),
        error = function(e) {
          conditionMessage(e)
        }
      )
      if (is.character(req)) {
        showModal(modalDialog(
          title = "Guideline not saved",
          req,
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return()
      }
      save_guideline_task$invoke(req)
    })
    observeEvent(save_guideline_task$result(), {
      result <- save_guideline_task$result()
      if (is.null(result)) {
        return()
      }
      if (!isTRUE(result$ok)) {
        showModal(modalDialog(
          title = "Guideline not saved",
          result$error,
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return()
      }
      saved_id <- result$guideline_id
      load_reference_data()
      update_choices()
      selected_guideline_id(saved_id)
      load_form(saved_id)
      showModal(modalDialog(
        title = "Guideline saved",
        "The database accepted the guideline and value rules.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })

    observeEvent(input$delete_guideline, {
      guideline_id <- selected_guideline_id()
      if (is.na(guideline_id)) {
        return()
      }
      g <- moduleData$guidelines[
        moduleData$guidelines$guideline_id == guideline_id,
        ,
        drop = FALSE
      ]
      showModal(modalDialog(
        title = "Delete guideline",
        paste("Delete", g$guideline_code[[1]], "?"),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("confirm_delete_guideline"),
            "Delete",
            class = "btn-danger"
          )
        )
      ))
    })
    observeEvent(input$confirm_delete_guideline, {
      removeModal()
      guideline_id <- selected_guideline_id()
      if (is.na(guideline_id)) {
        return()
      }
      tryCatch(
        {
          DBI::dbExecute(
            con,
            "DELETE FROM criteria.guidelines WHERE guideline_id = $1",
            params = list(guideline_id)
          )
          load_reference_data()
          update_choices()
          clear_form()
        },
        error = function(e) {
          showModal(modalDialog(
            title = "Delete failed",
            conditionMessage(e),
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
        }
      )
    })

    guideline_test_query <- function(result_id, guideline_id) {
      DBI::dbGetQuery(
        con,
        "SELECT guideline_code, guideline_name, result_value,
                lower_guideline_value, upper_guideline_value,
                output_status, comparison_status, derivation_inputs, message
         FROM criteria.applicable_guidelines_for_result($1, CURRENT_DATE, TRUE, TRUE)
         WHERE guideline_id = $2",
        params = list(result_id, guideline_id)
      )
    }
    guideline_input_values_for_sample <- function(sample_id, rule_id) {
      sample_id <- integer_or_na(sample_id)
      rule_id <- integer_or_na(rule_id)
      if (is.na(sample_id) || is.na(rule_id)) {
        return(data.frame())
      }
      DBI::dbGetQuery(
        con,
        "SELECT gri.input_code, gri.input_name,
                v.input_value, v.source_result_id, v.status, v.message
         FROM criteria.guideline_rule_inputs gri
         CROSS JOIN LATERAL criteria.guideline_get_input_value(
           $1, gri.input_id
         ) v
         WHERE gri.rule_id = $2
         ORDER BY gri.input_code",
        params = list(sample_id, rule_id)
      )
    }
    format_input_value_summary <- function(values) {
      if (is.null(values) || !nrow(values)) {
        return("")
      }
      parts <- vapply(
        seq_len(nrow(values)),
        function(i) {
          label <- text_default(values$input_name[[i]], values$input_code[[i]])
          if (identical(values$status[[i]], "value")) {
            paste0(label, " = ", values$input_value[[i]])
          } else {
            paste0(label, " = ", values$status[[i]])
          }
        },
        character(1)
      )
      paste(parts, collapse = "; ")
    }
    target_result_for_sample <- function(sample_id, guideline_row) {
      sample_id <- integer_or_na(sample_id)
      if (is.na(sample_id) || is.null(guideline_row) || !nrow(guideline_row)) {
        return(data.frame())
      }
      params <- list(
        sample_id,
        as.integer(guideline_row$parameter_id[[1]]),
        as.integer(guideline_row$matrix_state_id[[1]])
      )
      conditions <- c(
        "r.sample_id = $1",
        "r.parameter_id = $2",
        "r.matrix_state_id = $3"
      )
      result_speciation_id <- integer_or_na(
        guideline_row$result_speciation_id[[1]]
      )
      if (!is.na(result_speciation_id)) {
        params <- c(params, list(result_speciation_id))
        conditions <- c(
          conditions,
          paste0(
            "r.result_speciation_id IS NOT DISTINCT FROM $",
            length(params)
          )
        )
      }
      fraction_ids <- parse_id_csv(guideline_row$fraction_ids[[1]])
      if (length(fraction_ids)) {
        conditions <- c(
          conditions,
          paste0(
            "r.sample_fraction_id IN (",
            paste(fraction_ids, collapse = ","),
            ")"
          )
        )
      }
      DBI::dbGetQuery(
        con,
        paste0(
          "SELECT r.result_id, r.result AS guideline_parameter_result
           FROM discrete.results r
           WHERE ",
          paste(conditions, collapse = "\n             AND "),
          "
           ORDER BY r.result_id DESC
           LIMIT 1"
        ),
        params = params
      )
    }
    existing_sample_choices_for_guideline <- function(
      guideline_id = NA_integer_,
      guideline_row = NULL,
      primary_rule_id = NULL,
      connection = con,
      candidate_limit = 50L,
      choice_limit = 10L
    ) {
      if (is.null(guideline_row)) {
        guideline_row <- moduleData$guidelines[
          moduleData$guidelines$guideline_id == guideline_id,
          ,
          drop = FALSE
        ]
      }
      if (!nrow(guideline_row)) {
        return(character(0))
      }
      if (is.null(primary_rule_id)) {
        rules <- load_rules(guideline_id)
        primary_rule_id <- if (nrow(rules)) {
          integer_or_na(rules$rule_id[[1]])
        } else {
          NA_integer_
        }
      }
      params <- list(
        as.integer(guideline_row$parameter_id[[1]]),
        as.integer(guideline_row$matrix_state_id[[1]])
      )
      target_conditions <- c(
        "r.parameter_id = $1",
        "r.matrix_state_id = $2"
      )
      sample_conditions <- character(0)
      result_speciation_id <- integer_or_na(
        guideline_row$result_speciation_id[[1]]
      )
      if (!is.na(result_speciation_id)) {
        params <- c(params, list(result_speciation_id))
        target_conditions <- c(
          target_conditions,
          paste0(
            "r.result_speciation_id IS NOT DISTINCT FROM $",
            length(params)
          )
        )
      }
      fraction_ids <- parse_id_csv(guideline_row$fraction_ids[[1]])
      if (length(fraction_ids)) {
        target_conditions <- c(
          target_conditions,
          paste0(
            "r.sample_fraction_id IN (",
            paste(fraction_ids, collapse = ","),
            ")"
          )
        )
      }
      if (!is.na(primary_rule_id)) {
        params <- c(params, list(primary_rule_id))
        sample_conditions <- c(
          sample_conditions,
          paste0(
            "NOT EXISTS (
               SELECT 1
               FROM criteria.guideline_rule_inputs gri
               CROSS JOIN LATERAL criteria.guideline_get_input_value(
                 s.sample_id, gri.input_id
               ) v
               WHERE gri.rule_id = $",
            length(params),
            "
                 AND COALESCE(gri.required, TRUE)
                 AND v.status <> 'value'
             )"
          )
        )
      }
      candidate_limit <- max(10L, as.integer(candidate_limit))
      choice_limit <- max(1L, as.integer(choice_limit))
      sample_where <- if (length(sample_conditions)) {
        paste0(
          "WHERE ",
          paste(sample_conditions, collapse = "\n             AND ")
        )
      } else {
        ""
      }
      choices <- DBI::dbGetQuery(
        connection,
        paste0(
          "WITH target_candidates AS (
             SELECT DISTINCT ON (s.sample_id)
                    s.sample_id, s.datetime, s.location_id
             FROM discrete.samples s
             JOIN discrete.results r ON r.sample_id = s.sample_id
             WHERE ",
          paste(target_conditions, collapse = "\n               AND "),
          "
             ORDER BY s.sample_id, s.datetime DESC NULLS LAST
             LIMIT ",
          candidate_limit,
          "
           )
           SELECT s.sample_id,
                  concat_ws(
                    ' | ',
                    'Sample ' || s.sample_id::text,
                    to_char(s.datetime AT TIME ZONE 'UTC',
                            'YYYY-MM-DD HH24:MI UTC'),
                    concat_ws(' - ', l.location_code, l.name)
                  ) AS sample_label
           FROM target_candidates s
           LEFT JOIN public.locations l ON l.location_id = s.location_id
           ",
          sample_where,
          "
           GROUP BY s.sample_id, s.datetime, l.location_code, l.name
           ORDER BY s.datetime DESC NULLS LAST, s.sample_id DESC
           LIMIT ",
          choice_limit
        ),
        params = params
      )
      if (!nrow(choices)) {
        return(character(0))
      }
      stats::setNames(as.character(choices$sample_id), choices$sample_label)
    }
    run_existing_sample_guideline_test <- function(guideline_id, sample_id) {
      guideline_row <- moduleData$guidelines[
        moduleData$guidelines$guideline_id == guideline_id,
        ,
        drop = FALSE
      ]
      rules <- load_rules(guideline_id)
      primary_rule_id <- if (nrow(rules)) {
        integer_or_na(rules$rule_id[[1]])
      } else {
        NA_integer_
      }
      target <- target_result_for_sample(sample_id, guideline_row)
      if (!nrow(target)) {
        return(data.frame(
          error = "No matching result for the guideline parameter was found on the selected sample.",
          stringsAsFactors = FALSE
        ))
      }
      result <- guideline_test_query(target$result_id[[1]], guideline_id)
      if (!nrow(result)) {
        result <- data.frame(
          message = "No applicable row returned for the selected sample.",
          stringsAsFactors = FALSE
        )
      }
      input_values <- guideline_input_values_for_sample(
        sample_id,
        primary_rule_id
      )
      result$selected_sample_id <- sample_id
      result$selected_result_id <- target$result_id[[1]]
      result$guideline_parameter_result <- target$guideline_parameter_result[[
        1
      ]]
      result$input_values <- format_input_value_summary(input_values)
      result
    }
    existing_sample_choices_state <- reactiveVal(NULL)
    output$existing_sample_picker <- renderUI({
      state <- existing_sample_choices_state()
      if (is.null(state)) {
        return(tags$div(
          class = "text-muted",
          "Select Existing sample to load a short list of matching samples."
        ))
      }
      if (isTRUE(state$loading)) {
        return(tags$div(class = "text-muted", "Loading matching samples..."))
      }
      choices <- state$choices %||% character(0)
      tagList(
        selectizeInput(
          ns("test_sample_id"),
          "Sample with guideline parameter and required inputs",
          choices = choices,
          selected = if (length(choices)) {
            unname(choices[[1]])
          } else {
            character(0)
          },
          width = "100%",
          options = list(placeholder = "Select a sample")
        ),
        if (!length(choices)) {
          tags$div(
            class = "alert alert-info",
            style = "padding:8px; margin-top:8px;",
            "No existing samples were found with both the guideline parameter result and all required rule inputs."
          )
        }
      )
    })
    load_existing_samples_task <- ExtendedTask$new(function(req) {
      promises::future_promise(seed = TRUE, expr = {
        tryCatch(
          {
            con <- DBI::dbConnect(
              RPostgres::Postgres(),
              dbname = req$config$dbName,
              host = req$config$dbHost,
              port = req$config$dbPort,
              user = req$config$dbUser,
              password = req$config$dbPass
            )
            on.exit(DBI::dbDisconnect(con), add = TRUE)

            guideline_row <- req$guideline_row
            params <- list(
              as.integer(guideline_row$parameter_id[[1]]),
              as.integer(guideline_row$matrix_state_id[[1]])
            )
            target_conditions <- c(
              "r.parameter_id = $1",
              "r.matrix_state_id = $2"
            )
            result_speciation_id <- suppressWarnings(as.integer(
              guideline_row$result_speciation_id[[1]]
            ))
            if (!is.na(result_speciation_id)) {
              params <- c(params, list(result_speciation_id))
              target_conditions <- c(
                target_conditions,
                paste0(
                  "r.result_speciation_id IS NOT DISTINCT FROM $",
                  length(params)
                )
              )
            }
            fraction_ids <- suppressWarnings(as.integer(strsplit(
              as.character(guideline_row$fraction_ids[[1]] %||% ""),
              ",",
              fixed = TRUE
            )[[1]]))
            fraction_ids <- fraction_ids[!is.na(fraction_ids)]
            if (length(fraction_ids)) {
              target_conditions <- c(
                target_conditions,
                paste0(
                  "r.sample_fraction_id IN (",
                  paste(fraction_ids, collapse = ","),
                  ")"
                )
              )
            }
            sample_conditions <- character(0)
            primary_rule_id <- suppressWarnings(as.integer(req$primary_rule_id))
            if (!is.na(primary_rule_id)) {
              params <- c(params, list(primary_rule_id))
              sample_conditions <- c(
                sample_conditions,
                paste0(
                  "NOT EXISTS (
                     SELECT 1
                     FROM criteria.guideline_rule_inputs gri
                     CROSS JOIN LATERAL criteria.guideline_get_input_value(
                       s.sample_id, gri.input_id
                     ) v
                     WHERE gri.rule_id = $",
                  length(params),
                  "
                       AND COALESCE(gri.required, TRUE)
                       AND v.status <> 'value'
                   )"
                )
              )
            }
            sample_where <- if (length(sample_conditions)) {
              paste0(
                "WHERE ",
                paste(sample_conditions, collapse = "\n             AND ")
              )
            } else {
              ""
            }
            choices <- DBI::dbGetQuery(
              con,
              paste0(
                "WITH target_candidates AS (
                   SELECT s.sample_id, s.datetime, s.location_id
                   FROM discrete.samples s
                   JOIN discrete.results r ON r.sample_id = s.sample_id
                   WHERE ",
                paste(
                  target_conditions,
                  collapse = "\n                     AND "
                ),
                "
                   GROUP BY s.sample_id, s.datetime, s.location_id
                   ORDER BY s.datetime DESC NULLS LAST, s.sample_id DESC
                   LIMIT 50
                 )
                 SELECT s.sample_id,
                        concat_ws(
                          ' | ',
                          'Sample ' || s.sample_id::text,
                          to_char(s.datetime AT TIME ZONE 'UTC',
                                  'YYYY-MM-DD HH24:MI UTC'),
                          concat_ws(' - ', l.location_code, l.name)
                        ) AS sample_label
                 FROM target_candidates s
                 LEFT JOIN public.locations l ON l.location_id = s.location_id
                 ",
                sample_where,
                "
                 ORDER BY s.datetime DESC NULLS LAST, s.sample_id DESC
                 LIMIT 10"
              ),
              params = params
            )
            out <- if (nrow(choices)) {
              stats::setNames(
                as.character(choices$sample_id),
                choices$sample_label
              )
            } else {
              character(0)
            }
            list(ok = TRUE, choices = out)
          },
          error = function(e) {
            list(ok = FALSE, error = conditionMessage(e))
          }
        )
      })
    })
    run_guideline_test_task <- ExtendedTask$new(function(req) {
      promises::future_promise(seed = TRUE, expr = {
        tryCatch(
          {
            con <- DBI::dbConnect(
              RPostgres::Postgres(),
              dbname = req$config$dbName,
              host = req$config$dbHost,
              port = req$config$dbPort,
              user = req$config$dbUser,
              password = req$config$dbPass
            )
            on.exit(DBI::dbDisconnect(con), add = TRUE)

            int_or_na <- function(x) {
              if (is.null(x) || !length(x) || is.na(x[[1]])) {
                return(NA_integer_)
              }
              out <- suppressWarnings(as.integer(as.character(x[[1]])))
              if (is.na(out)) NA_integer_ else out
            }
            parse_ids <- function(x) {
              if (is.null(x) || !length(x) || all(is.na(x))) {
                return(integer(0))
              }
              vals <- unlist(strsplit(as.character(x[[1]]), ",", fixed = TRUE))
              vals <- suppressWarnings(as.integer(trimws(vals)))
              vals[!is.na(vals)]
            }
            first_id <- function(x) {
              vals <- parse_ids(x)
              if (length(vals)) vals[[1]] else NA_integer_
            }
            default_result_type_id <- function() {
              out <- DBI::dbGetQuery(
                con,
                "SELECT result_type_id
                 FROM discrete.result_types
                 ORDER BY
                   CASE
                     WHEN result_type ~* '^lab$' THEN 0
                     WHEN result_type ~* '^field$' THEN 1
                     ELSE 2
                   END,
                   result_type_id
                 LIMIT 1"
              )
              if (nrow(out)) out$result_type_id[[1]] else NA_integer_
            }
            actual_value_type_id <- function() {
              out <- DBI::dbGetQuery(
                con,
                "SELECT result_value_type_id
                 FROM discrete.result_value_types
                 WHERE lower(result_value_type) = 'actual'
                 ORDER BY result_value_type_id
                 LIMIT 1"
              )
              if (nrow(out)) out$result_value_type_id[[1]] else NA_integer_
            }
            guideline_query <- function(result_id) {
              DBI::dbGetQuery(
                con,
                "SELECT guideline_code, guideline_name, result_value,
                        lower_guideline_value, upper_guideline_value,
                        output_status, comparison_status,
                        derivation_inputs, message
                 FROM criteria.applicable_guidelines_for_result(
                   $1, CURRENT_DATE, TRUE, TRUE
                 )
                 WHERE guideline_id = $2",
                params = list(result_id, req$guideline_id)
              )
            }
            input_values <- function(sample_id) {
              rule_id <- int_or_na(req$primary_rule_id)
              if (is.na(rule_id)) {
                return(data.frame())
              }
              DBI::dbGetQuery(
                con,
                "SELECT gri.input_code, gri.input_name,
                        v.input_value, v.source_result_id,
                        v.status, v.message
                 FROM criteria.guideline_rule_inputs gri
                 CROSS JOIN LATERAL criteria.guideline_get_input_value(
                   $1, gri.input_id
                 ) v
                 WHERE gri.rule_id = $2
                 ORDER BY gri.input_code",
                params = list(sample_id, rule_id)
              )
            }
            input_summary <- function(values) {
              if (is.null(values) || !nrow(values)) {
                return("")
              }
              paste(
                vapply(
                  seq_len(nrow(values)),
                  function(i) {
                    label <- values$input_name[[i]]
                    if (is.na(label) || !nzchar(label)) {
                      label <- values$input_code[[i]]
                    }
                    if (identical(values$status[[i]], "value")) {
                      paste0(label, " = ", values$input_value[[i]])
                    } else {
                      paste0(label, " = ", values$status[[i]])
                    }
                  },
                  character(1)
                ),
                collapse = "; "
              )
            }
            target_result_for_sample <- function(sample_id) {
              guideline_row <- req$guideline_row
              params <- list(
                int_or_na(sample_id),
                as.integer(guideline_row$parameter_id[[1]]),
                as.integer(guideline_row$matrix_state_id[[1]])
              )
              conditions <- c(
                "r.sample_id = $1",
                "r.parameter_id = $2",
                "r.matrix_state_id = $3"
              )
              result_speciation_id <- int_or_na(
                guideline_row$result_speciation_id[[1]]
              )
              if (!is.na(result_speciation_id)) {
                params <- c(params, list(result_speciation_id))
                conditions <- c(
                  conditions,
                  paste0(
                    "r.result_speciation_id IS NOT DISTINCT FROM $",
                    length(params)
                  )
                )
              }
              fraction_ids <- parse_ids(guideline_row$fraction_ids[[1]])
              if (length(fraction_ids)) {
                conditions <- c(
                  conditions,
                  paste0(
                    "r.sample_fraction_id IN (",
                    paste(fraction_ids, collapse = ","),
                    ")"
                  )
                )
              }
              DBI::dbGetQuery(
                con,
                paste0(
                  "SELECT r.result_id,
                          r.result AS guideline_parameter_result
                   FROM discrete.results r
                   WHERE ",
                  paste(conditions, collapse = "\n                     AND "),
                  "
                   ORDER BY r.result_id DESC
                   LIMIT 1"
                ),
                params = params
              )
            }
            insert_temp_result <- function(
              sample_id,
              parameter_id,
              matrix_state_id,
              sample_fraction_id,
              result_speciation_id,
              value,
              result_type_id,
              analysis_datetime
            ) {
              result_type_id <- if (is.na(int_or_na(result_type_id))) {
                default_result_type_id()
              } else {
                int_or_na(result_type_id)
              }
              query <- DBI::sqlInterpolate(
                con,
                "INSERT INTO discrete.results (
                   sample_id, result_type, parameter_id, sample_fraction_id,
                   result, result_condition, result_condition_value,
                   result_value_type, result_speciation_id,
                   analysis_datetime, share_with, no_source_update, matrix_state_id
                 )
                 VALUES (
                   ?sample_id, ?result_type_id, ?parameter_id,
                   ?sample_fraction_id, ?value, NULL, NULL,
                   ?result_value_type_id, ?result_speciation_id,
                   CAST(?analysis_datetime AS timestamptz),
                   ARRAY['public_reader'], false, ?matrix_state_id
                 )
                 RETURNING result_id",
                sample_id = sample_id,
                result_type_id = result_type_id,
                parameter_id = parameter_id,
                sample_fraction_id = sample_fraction_id,
                value = value,
                result_value_type_id = actual_value_type_id(),
                result_speciation_id = result_speciation_id,
                analysis_datetime = analysis_datetime,
                matrix_state_id = matrix_state_id
              )
              DBI::dbGetQuery(con, query)$result_id[[1]]
            }

            if (identical(req$mode, "existing")) {
              target <- target_result_for_sample(req$sample_id)
              if (!nrow(target)) {
                return(list(
                  ok = TRUE,
                  result = data.frame(
                    error = "No matching result for the guideline parameter was found on the selected sample.",
                    stringsAsFactors = FALSE
                  )
                ))
              }
              result <- guideline_query(target$result_id[[1]])
              if (!nrow(result)) {
                result <- data.frame(
                  message = "No applicable row returned for the selected sample.",
                  stringsAsFactors = FALSE
                )
              }
              values <- input_values(req$sample_id)
              result$selected_sample_id <- req$sample_id
              result$selected_result_id <- target$result_id[[1]]
              result$guideline_parameter_result <-
                target$guideline_parameter_result[[1]]
              result$input_values <- input_summary(values)
              return(list(ok = TRUE, result = result))
            }

            guideline_row <- req$guideline_row
            sample_datetime <- Sys.time()
            DBI::dbBegin(con)
            active <- TRUE
            on.exit(
              {
                if (isTRUE(active)) {
                  try(DBI::dbRollback(con), silent = TRUE)
                }
              },
              add = TRUE
            )
            template <- DBI::dbGetQuery(
              con,
              "SELECT COALESCE($1::integer, s.media_id) AS media_id,
                      COALESCE($2::integer, s.location_id) AS location_id,
                      s.sub_location_id, s.collection_method, s.sample_type,
                      s.sample_grade, s.sample_approval, s.owner,
                      s.contributor, s.sampling_org
               FROM discrete.samples s
               WHERE ($1::integer IS NULL OR s.media_id = $1)
               ORDER BY
                 CASE
                   WHEN $2::integer IS NOT NULL
                     AND s.location_id = $2 THEN 0
                   ELSE 1
                 END,
                 s.sample_id
               LIMIT 1",
              params = list(
                first_id(guideline_row$media_ids[[1]]),
                first_id(guideline_row$location_ids[[1]])
              )
            )
            if (!nrow(template)) {
              stop(
                "No existing sample was available to use as metadata for a temporary test sample.",
                call. = FALSE
              )
            }
            source_id <- paste0(
              "guideline_test_",
              format(Sys.time(), "%Y%m%d%H%M%S"),
              "_",
              sample.int(999999L, 1L)
            )
            sample_id <- DBI::dbGetQuery(
              con,
              "INSERT INTO discrete.samples (
                 location_id, sub_location_id, media_id, z, datetime,
                 target_datetime, collection_method, sample_type,
                 sample_volume_ml, sample_grade, sample_approval,
                 owner, contributor, sampling_org, share_with, import_source,
                 no_source_update, note, import_source_id
               )
               VALUES (
                 $1, $2, $3, 0, $4::timestamptz, $4::timestamptz,
                 $5, $6, 1000, $7, $8, $9, $10, $11,
                 ARRAY['public_reader'], 'ygwater_guideline_test',
                 false,
                 'Temporary guideline test sample; transaction rolled back.',
                 $12
               )
               RETURNING sample_id",
              params = list(
                template$location_id[[1]],
                template$sub_location_id[[1]],
                template$media_id[[1]],
                sample_datetime,
                template$collection_method[[1]],
                template$sample_type[[1]],
                template$sample_grade[[1]],
                template$sample_approval[[1]],
                template$owner[[1]],
                template$contributor[[1]],
                template$sampling_org[[1]],
                source_id
              )
            )$sample_id[[1]]
            target_result_id <- insert_temp_result(
              sample_id = sample_id,
              parameter_id = as.integer(guideline_row$parameter_id[[1]]),
              matrix_state_id = as.integer(guideline_row$matrix_state_id[[1]]),
              sample_fraction_id = first_id(guideline_row$fraction_ids[[1]]),
              result_speciation_id = int_or_na(
                guideline_row$result_speciation_id[[1]]
              ),
              value = req$target_value,
              result_type_id = NA_integer_,
              analysis_datetime = sample_datetime
            )
            input_results <- req$input_results
            if (!is.null(input_results) && nrow(input_results)) {
              input_results <- input_results[
                !is.na(input_results$value),
                ,
                drop = FALSE
              ]
              for (i in seq_len(nrow(input_results))) {
                insert_temp_result(
                  sample_id = sample_id,
                  parameter_id = input_results$parameter_id[[i]],
                  matrix_state_id = input_results$matrix_state_id[[i]],
                  sample_fraction_id = input_results$sample_fraction_id[[i]],
                  result_speciation_id = input_results$result_speciation_id[[
                    i
                  ]],
                  value = input_results$value[[i]],
                  result_type_id = input_results$result_type_id[[i]],
                  analysis_datetime = sample_datetime
                )
              }
            }
            result <- guideline_query(target_result_id)
            if (!nrow(result)) {
              result <- data.frame(
                message = "No applicable row returned for the temporary test result.",
                stringsAsFactors = FALSE
              )
            }
            values <- input_values(sample_id)
            result$temporary_sample_id <- sample_id
            result$temporary_result_id <- target_result_id
            result$guideline_parameter_result <- req$target_value
            result$temporary_input_results <- nrow(req$input_results)
            result$input_values <- input_summary(values)
            DBI::dbRollback(con)
            active <- FALSE
            list(ok = TRUE, result = result)
          },
          error = function(e) {
            list(
              ok = FALSE,
              result = data.frame(
                error = conditionMessage(e),
                stringsAsFactors = FALSE
              )
            )
          }
        )
      })
    }) |>
      bslib::bind_task_button("run_guideline_test")
    open_test_guideline_task <- ExtendedTask$new(function(req) {
      promises::future_promise(seed = TRUE, expr = {
        req
      })
    }) |>
      bslib::bind_task_button("test_guideline")
    test_sample_template <- function(guideline_id) {
      g <- moduleData$guidelines[
        moduleData$guidelines$guideline_id == guideline_id,
        ,
        drop = FALSE
      ]
      media_id <- if (nrow(g)) {
        first_id_from_csv(g$media_ids[[1]])
      } else {
        NA_integer_
      }
      location_id <- if (nrow(g)) {
        first_id_from_csv(g$location_ids[[1]])
      } else {
        NA_integer_
      }
      template <- DBI::dbGetQuery(
        con,
        "SELECT COALESCE($1::integer, s.media_id) AS media_id,
                COALESCE($2::integer, s.location_id) AS location_id,
                s.sub_location_id, s.collection_method, s.sample_type,
                s.sample_grade, s.sample_approval, s.owner, s.contributor,
                s.sampling_org
         FROM discrete.samples s
         WHERE ($1::integer IS NULL OR s.media_id = $1)
         ORDER BY
           CASE
             WHEN $2::integer IS NOT NULL AND s.location_id = $2 THEN 0
             ELSE 1
           END,
           s.sample_id
         LIMIT 1",
        params = list(media_id, location_id)
      )
      if (!nrow(template)) {
        template <- DBI::dbGetQuery(
          con,
          "SELECT s.media_id, COALESCE($1::integer, s.location_id) AS location_id,
                  s.sub_location_id, s.collection_method, s.sample_type,
                  s.sample_grade, s.sample_approval, s.owner, s.contributor,
                  s.sampling_org
           FROM discrete.samples s
           ORDER BY
             CASE
               WHEN $1::integer IS NOT NULL AND s.location_id = $1 THEN 0
               ELSE 1
             END,
             s.sample_id
           LIMIT 1",
          params = list(location_id)
        )
      }
      if (!nrow(template)) {
        stop(
          "No existing sample was available to use as metadata for a temporary test sample.",
          call. = FALSE
        )
      }
      template
    }
    insert_temp_sample <- function(guideline_id, sample_datetime) {
      template <- test_sample_template(guideline_id)
      source_id <- paste0(
        "guideline_test_",
        format(Sys.time(), "%Y%m%d%H%M%S"),
        "_",
        sample.int(999999L, 1L)
      )
      DBI::dbGetQuery(
        con,
        "INSERT INTO discrete.samples (
           location_id, sub_location_id, media_id, z, datetime,
           target_datetime, collection_method, sample_type,
           sample_volume_ml, sample_grade, sample_approval,
           owner, contributor, sampling_org, share_with, import_source,
           no_source_update, note, import_source_id
         )
         VALUES (
           $1, $2, $3, 0, $4::timestamptz, $4::timestamptz,
           $5, $6, 1000, $7, $8, $9, $10, $11,
           ARRAY['public_reader'], 'ygwater_guideline_test',
           false, 'Temporary guideline test sample; transaction rolled back.', $12
         )
         RETURNING sample_id",
        params = list(
          template$location_id[[1]],
          template$sub_location_id[[1]],
          template$media_id[[1]],
          sample_datetime,
          template$collection_method[[1]],
          template$sample_type[[1]],
          template$sample_grade[[1]],
          template$sample_approval[[1]],
          template$owner[[1]],
          template$contributor[[1]],
          template$sampling_org[[1]],
          source_id
        )
      )$sample_id[[1]]
    }
    insert_temp_result <- function(
      sample_id,
      parameter_id,
      matrix_state_id,
      sample_fraction_id,
      result_speciation_id,
      result_value,
      result_type_id = NA_integer_,
      analysis_datetime = Sys.time()
    ) {
      if (
        is.na(parameter_id) || is.na(matrix_state_id) || is.na(result_value)
      ) {
        stop(
          "Temporary results require parameter_id, matrix_state_id, and value.",
          call. = FALSE
        )
      }
      result_type_id <- if (is.na(result_type_id)) {
        default_result_type_id()
      } else {
        result_type_id
      }
      result_value_type <- result_value_actual_id()
      if (is.na(result_type_id) || is.na(result_value_type)) {
        stop(
          "Could not resolve result type or result value type for temporary results.",
          call. = FALSE
        )
      }
      DBI::dbGetQuery(
        con,
        "INSERT INTO discrete.results (
           sample_id, result_type, parameter_id, sample_fraction_id,
           result, result_condition, result_condition_value,
           result_value_type, result_speciation_id, analysis_datetime,
           share_with, no_source_update, matrix_state_id
         )
         VALUES (
           $1, $2, $3, $4, $5, NULL, NULL, $6, $7, $8::timestamptz,
           ARRAY['public_reader'], false, $9
         )
         RETURNING result_id",
        params = list(
          sample_id,
          result_type_id,
          parameter_id,
          sample_fraction_id,
          result_value,
          result_value_type,
          result_speciation_id,
          analysis_datetime,
          matrix_state_id
        )
      )$result_id[[1]]
    }
    run_fake_guideline_test <- function(guideline_id) {
      g <- moduleData$guidelines[
        moduleData$guidelines$guideline_id == guideline_id,
        ,
        drop = FALSE
      ]
      if (!nrow(g)) {
        stop("No selected guideline was found.", call. = FALSE)
      }
      sample_datetime <- Sys.time()
      target_value <- numeric_or_na(input$test_target_value)
      if (is.na(target_value)) {
        stop(
          "Enter the value against which to test the guideline.",
          call. = FALSE
        )
      }
      rules <- load_rules(guideline_id)
      primary_inputs <- if (nrow(rules)) {
        load_inputs(rules$rule_id[[1]])
      } else {
        data.frame()
      }
      input_results <- collect_temporary_input_results(primary_inputs)
      input_results <- input_results[
        !is.na(input_results$value),
        ,
        drop = FALSE
      ]
      active <- FALSE
      tryCatch(
        {
          DBI::dbBegin(con)
          active <- TRUE
          sample_id <- insert_temp_sample(guideline_id, sample_datetime)
          target_result_id <- insert_temp_result(
            sample_id = sample_id,
            parameter_id = as.integer(g$parameter_id[[1]]),
            matrix_state_id = as.integer(g$matrix_state_id[[1]]),
            sample_fraction_id = first_id_from_csv(g$fraction_ids[[1]]),
            result_speciation_id = if (is.na(g$result_speciation_id[[1]])) {
              NA_integer_
            } else {
              as.integer(g$result_speciation_id[[1]])
            },
            result_value = target_value,
            result_type_id = default_result_type_id(c("^lab$", "^field$")),
            analysis_datetime = sample_datetime
          )
          if (nrow(input_results)) {
            for (i in seq_len(nrow(input_results))) {
              insert_temp_result(
                sample_id = sample_id,
                parameter_id = input_results$parameter_id[[i]],
                matrix_state_id = input_results$matrix_state_id[[i]],
                sample_fraction_id = input_results$sample_fraction_id[[i]],
                result_speciation_id = input_results$result_speciation_id[[i]],
                result_value = input_results$value[[i]],
                result_type_id = input_results$result_type_id[[i]],
                analysis_datetime = sample_datetime
              )
            }
          }
          result <- guideline_test_query(target_result_id, guideline_id)
          if (!nrow(result)) {
            result <- data.frame(
              message = "No applicable row returned for the temporary test result.",
              stringsAsFactors = FALSE
            )
          }
          primary_rule_id <- if (nrow(rules)) {
            integer_or_na(rules$rule_id[[1]])
          } else {
            NA_integer_
          }
          input_values <- guideline_input_values_for_sample(
            sample_id,
            primary_rule_id
          )
          result$temporary_sample_id <- sample_id
          result$temporary_result_id <- target_result_id
          result$guideline_parameter_result <- target_value
          result$temporary_input_results <- nrow(input_results)
          result$input_values <- format_input_value_summary(input_values)
          DBI::dbRollback(con)
          active <- FALSE
          result
        },
        error = function(e) {
          if (active) {
            try(DBI::dbRollback(con), silent = TRUE)
          }
          data.frame(error = conditionMessage(e), stringsAsFactors = FALSE)
        }
      )
    }

    observeEvent(input$test_guideline, {
      guideline_id <- selected_guideline_id()
      if (is.na(guideline_id)) {
        showModal(modalDialog(
          title = "Save first",
          "Save the guideline before testing it against a result.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return()
      }
      rules <- load_rules(guideline_id)
      primary_inputs <- if (nrow(rules)) {
        load_inputs(rules$rule_id[[1]])
      } else {
        data.frame()
      }
      g <- moduleData$guidelines[
        moduleData$guidelines$guideline_id == guideline_id,
        ,
        drop = FALSE
      ]
      open_test_guideline_task$invoke(list(
        guideline_id = guideline_id,
        guideline_row = g,
        primary_inputs = primary_inputs
      ))
    })
    observeEvent(open_test_guideline_task$result(), {
      modal_req <- open_test_guideline_task$result()
      if (is.null(modal_req)) {
        return()
      }
      g <- modal_req$guideline_row
      primary_inputs <- modal_req$primary_inputs
      existing_sample_choices_state(NULL)
      showModal(modalDialog(
        title = "Test saved guideline",
        radioButtons(
          ns("test_mode"),
          NULL,
          choices = c(
            "Temporary sample/results" = "fake",
            "Existing sample" = "existing"
          ),
          selected = "fake",
          inline = TRUE
        ),
        conditionalPanel(
          condition = "input.test_mode == 'fake'",
          ns = ns,
          tags$h4("Temporary sample/results"),
          helpText(
            "Rows entered here are inserted only inside the test transaction and rolled back after the guideline is evaluated."
          ),
          fluidRow(
            column(
              4,
              numericInput(
                ns("test_target_value"),
                test_value_label(
                  g$parameter_id[[1]],
                  g$matrix_state_id[[1]],
                  "value against which to test the guideline"
                ),
                value = 0
              )
            )
          ),
          helpText(
            "This is the measured result for the guideline parameter being evaluated."
          ),
          temporary_input_controls(primary_inputs),
          helpText(
            "Input values are inserted only for this rollback test."
          )
        ),
        conditionalPanel(
          condition = "input.test_mode == 'existing'",
          ns = ns,
          tags$h4("Existing database sample"),
          uiOutput(ns("existing_sample_picker"))
        ),
        bslib::input_task_button(ns("run_guideline_test"), label = "Run"),
        br(),
        br(),
        DT::DTOutput(ns("test_guideline_modal_table")),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "l"
      ))
    })
    observeEvent(
      input$test_mode,
      {
        if (!identical(input$test_mode, "existing")) {
          return()
        }
        guideline_id <- selected_guideline_id()
        if (is.na(guideline_id)) {
          return()
        }
        rules <- load_rules(guideline_id)
        guideline_row <- moduleData$guidelines[
          moduleData$guidelines$guideline_id == guideline_id,
          ,
          drop = FALSE
        ]
        existing_sample_choices_state(list(loading = TRUE))
        load_existing_samples_task$invoke(list(
          config = session$userData$config,
          guideline_row = guideline_row,
          primary_rule_id = if (nrow(rules)) rules$rule_id[[1]] else NA_integer_
        ))
      },
      ignoreInit = TRUE
    )
    observeEvent(load_existing_samples_task$result(), {
      result <- load_existing_samples_task$result()
      if (is.null(result)) {
        return()
      }
      if (!isTRUE(result$ok)) {
        existing_sample_choices_state(list(
          loading = FALSE,
          choices = character(0)
        ))
        showNotification(result$error, type = "error", duration = 8)
        return()
      }
      existing_sample_choices_state(list(
        loading = FALSE,
        choices = result$choices
      ))
    })
    observeEvent(input$run_guideline_test, {
      guideline_id <- selected_guideline_id()
      if (is.na(guideline_id)) {
        return()
      }
      guideline_row <- moduleData$guidelines[
        moduleData$guidelines$guideline_id == guideline_id,
        ,
        drop = FALSE
      ]
      rules <- load_rules(guideline_id)
      primary_inputs <- if (nrow(rules)) {
        load_inputs(rules$rule_id[[1]])
      } else {
        data.frame()
      }
      if (identical(input$test_mode, "existing")) {
        sample_id <- integer_or_na(input$test_sample_id)
        if (is.na(sample_id)) {
          showNotification(
            "Choose a sample before running the existing-sample test.",
            type = "error",
            duration = 6
          )
          return()
        }
        req <- list(
          config = session$userData$config,
          mode = "existing",
          guideline_id = guideline_id,
          guideline_row = guideline_row,
          primary_rule_id = if (nrow(rules)) {
            rules$rule_id[[1]]
          } else {
            NA_integer_
          },
          sample_id = sample_id
        )
      } else {
        target_value <- numeric_or_na(input$test_target_value)
        if (is.na(target_value)) {
          showNotification(
            "Enter the value against which to test the guideline.",
            type = "error",
            duration = 6
          )
          return()
        }
        req <- list(
          config = session$userData$config,
          mode = "fake",
          guideline_id = guideline_id,
          guideline_row = guideline_row,
          primary_rule_id = if (nrow(rules)) {
            rules$rule_id[[1]]
          } else {
            NA_integer_
          },
          target_value = target_value,
          input_results = collect_temporary_input_results(primary_inputs)
        )
      }
      run_guideline_test_task$invoke(req)
    })
    observeEvent(run_guideline_test_task$result(), {
      task_result <- run_guideline_test_task$result()
      if (is.null(task_result)) {
        return()
      }
      result <- task_result$result
      output$test_guideline_modal_table <- DT::renderDT({
        DT::datatable(result, rownames = FALSE, options = list(scrollX = TRUE))
      })
      output$test_guideline_result <- renderUI({
        if (!nrow(result)) {
          div("No applicable row returned for the last test.")
        } else {
          tagList(
            h4("Last Test Result"),
            DT::DTOutput(ns("test_guideline_last_table"))
          )
        }
      })
      output$test_guideline_last_table <- DT::renderDT({
        DT::datatable(result, rownames = FALSE, options = list(scrollX = TRUE))
      })
    })
  })
}
