# UI and server code for water quality guidelines management module.
#
# This version targets the patch-47 AquaCache guideline engine. The UI keeps
# common edits compact while writing the normalized guideline/rule/input tables
# that the database uses for validation and application.

addGuidelinesUI <- function(id) {
  ns <- NS(id)

  tagList(
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
          ns("publisher"), "Publisher", choices = NULL, width = "100%",
          multiple = TRUE, options = list(maxItems = 1, create = TRUE)
        ),
        selectizeInput(
          ns("series"), "Series", choices = NULL, width = "100%",
          multiple = TRUE, options = list(maxItems = 1, create = TRUE)
        ),
        selectizeInput(
          ns("parameter_id"), "Parameter", choices = NULL, width = "100%",
          multiple = TRUE, options = list(maxItems = 1)
        ),
        selectizeInput(
          ns("matrix_state"), "Matrix state", choices = NULL, width = "100%",
          multiple = TRUE, options = list(maxItems = 1)
        ),
        shinyjs::hidden(
          div(
            id = ns("result_speciation_section"),
            selectizeInput(
              ns("result_speciation"), "Result speciation", choices = NULL,
              width = "100%", multiple = TRUE, options = list(maxItems = 1)
            )
          )
        ),
        selectizeInput(
          ns("sample_fraction"), "Guideline fractions", choices = NULL,
          width = "100%", multiple = TRUE
        ),
        selectizeInput(
          ns("media_type"), "Guideline media types", choices = NULL,
          width = "100%", multiple = TRUE
        ),

        h4("Applicability"),
        selectInput(ns("comparison_operator"), "Comparison", choices = NULL),
        selectizeInput(
          ns("jurisdiction"), "Jurisdiction", choices = NULL, width = "100%",
          multiple = TRUE, options = list(maxItems = 1, create = TRUE)
        ),
        selectizeInput(
          ns("jurisdiction_level"), "Jurisdiction level", choices = NULL,
          width = "100%", multiple = TRUE, options = list(maxItems = 1, create = TRUE)
        ),
        selectizeInput(
          ns("protection_goal"), "Protection goal", choices = NULL,
          width = "100%", multiple = TRUE, options = list(maxItems = 1, create = TRUE)
        ),
        selectizeInput(
          ns("exposure_duration"), "Exposure duration", choices = NULL,
          width = "100%", multiple = TRUE, options = list(maxItems = 1, create = TRUE)
        ),
        selectizeInput(
          ns("averaging_period"), "Averaging period", choices = NULL,
          width = "100%", multiple = TRUE, options = list(maxItems = 1, create = TRUE)
        ),
        fluidRow(
          column(6, dateInput(ns("valid_from"), "Valid from", value = Sys.Date())),
          column(6, dateInput(ns("valid_to"), "Valid to", value = NA))
        ),
        selectInput(
          ns("review_status"), "Review status",
          choices = c("draft", "reviewed", "approved", "retired", "superseded")
        ),

        h4("Source"),
        textInput(ns("reference"), "Reference", width = "100%"),
        textInput(ns("source_document_title"), "Source document", width = "100%"),
        textInput(ns("source_url"), "Source URL", width = "100%"),
        fluidRow(
          column(4, textInput(ns("source_page"), "Page", width = "100%")),
          column(4, textInput(ns("source_table"), "Table", width = "100%")),
          column(4, textInput(ns("source_section"), "Section", width = "100%"))
        ),
        fluidRow(
          column(
            6,
            dateInput(ns("source_effective_date"), "Effective", value = NA)
          ),
          column(
            6,
            dateInput(ns("source_retrieved_date"), "Retrieved", value = Sys.Date())
          )
        ),
        textAreaInput(
          ns("general_notes"), "General notes", width = "100%", height = "70px"
        ),
        textAreaInput(
          ns("applicability_notes"), "Applicability notes",
          width = "100%", height = "70px"
        ),

        h4("Value Rule"),
        selectInput(
          ns("guideline_type"), "Type",
          choices = c(
            "Fixed upper value" = "constant_upper",
            "Fixed lower value" = "constant_lower",
            "Fixed range" = "constant_range",
            "Lookup by sample input" = "lookup_range",
            "Multi-input lookup table" = "lookup_grid",
            "One-input formula" = "single_input_formula",
            "Database function" = "db_function",
            "External model result" = "model_result_cache",
            "Narrative/site-specific" = "narrative",
            "Advanced SQL scalar" = "sql_scalar"
          )
        ),
        uiOutput(ns("value_rule_hint")),
        uiOutput(ns("calculation_source_ui")),
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
              column(6, numericInput(ns("lower_value"), "Lower", value = NA_real_)),
              column(6, numericInput(ns("upper_value"), "Upper", value = NA_real_))
            )
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("bound_section"),
            selectInput(
              ns("bound_code"), "Bound",
              choices = c("Upper limit" = "upper", "Lower limit" = "lower")
            )
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("formula_algorithm_section"),
            selectInput(
              ns("formula_algorithm"), "Formula",
              choices = c("Linear" = "linear", "Log-linear" = "log_linear", "Power" = "power")
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
                    choices = c("None" = "none", "Round" = "round", "Floor" = "floor", "Ceiling" = "ceiling")
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
                column(4, numericInput(ns("min_output_value"), "Minimum output", value = NA_real_)),
                column(4, numericInput(ns("max_output_value"), "Maximum output", value = NA_real_))
              ),
              textInput(
                ns("precision_note"),
                bslib::tooltip(
                  tags$span("Precision note"),
                  "Optional note about rounding, significant figures, or source-document precision."
                )
                , width = "100%"
              )
            )
          )
        ),
        textAreaInput(ns("rule_note"), "Rule note", width = "100%", height = "60px"),

        shinyjs::hidden(
          div(
            id = ns("rule_inputs_section"),
            tags$details(
              open = TRUE,
              tags$summary(
                bslib::tooltip(
                  tags$span("Rule inputs"),
                  "Rows identify sample results used to derive a guideline. One-input lookup and formula rules require one input row; multi-input lookup, function, and model rules can require several chemistry inputs."
                )
              ),
              uiOutput(ns("rule_inputs_ui")),
              div(
                style = "display:flex; gap:8px; margin:8px 0;",
                bslib::tooltip(
                  actionButton(ns("add_rule_input"), "Add input", class = "btn-sm"),
                  "Add another sample result that this rule needs."
                ),
                bslib::tooltip(
                  actionButton(ns("remove_rule_input"), "Remove last", class = "btn-sm"),
                  "Remove the last sample input row."
                ),
                bslib::tooltip(
                  actionButton(ns("fill_hardness_doc_inputs"), "Hardness + DOC", class = "btn-sm"),
                  "Use common hardness and dissolved organic carbon inputs."
                )
              )
            )
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("lookup_values_section"),
            tags$details(
              open = TRUE,
              tags$summary(
                bslib::tooltip(
                  tags$span("Lookup values"),
                  "Only for Lookup by sample input. Each row maps an input interval to the guideline value returned for that interval."
                )
              ),
              div(
                style = "display:flex; gap:8px; margin:8px 0;",
                bslib::tooltip(
                  actionButton(ns("fill_lookup_template"), "Template", class = "btn-sm"),
                  "Insert an example lookup table with open-ended and closed input ranges."
                )
              ),
              textAreaInput(
                ns("lookup_values_text"), NULL, width = "100%", height = "130px",
                placeholder = paste(
                  "Example: for sulphate by hardness, one row might cover hardness 30-75 mg/L and return 218.",
                  "Leave lower_bound or upper_bound blank for an open-ended interval."
                )
              )
            )
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("lookup_grid_values_section"),
            tags$details(
              open = TRUE,
              tags$summary(
                bslib::tooltip(
                  tags$span("Lookup grid cells"),
                  "Only for Multi-input lookup table. Each row maps a combination of input intervals to one returned guideline value."
                )
              ),
              div(
                style = "display:flex; gap:8px; margin:8px 0;",
                bslib::tooltip(
                  actionButton(ns("fill_lookup_grid_template"), "Template", class = "btn-sm"),
                  "Insert an example multi-input lookup grid using the current Rule inputs."
                )
              ),
              textAreaInput(
                ns("lookup_grid_values_text"), NULL, width = "100%", height = "170px",
                placeholder = paste(
                  "Columns are based on Rule input codes. For input hardness_mg_l_caco3, use",
                  "hardness_mg_l_caco3_lower_bound and hardness_mg_l_caco3_upper_bound."
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
                actionButton(ns("fill_coefficients_template"), "Template", class = "btn-sm")
              ),
              textAreaInput(ns("coefficients_text"), NULL, width = "100%", height = "100px")
            )
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("model_fields_section"),
            tags$details(
              open = TRUE,
              tags$summary(
                bslib::tooltip(
                  tags$span("Derivation output metadata"),
                  "Stable database keys for the function or external model output that produces this guideline value."
                )
              ),
              fluidRow(
                column(6, textInput(ns("model_code"), "Derivation code", width = "100%")),
                column(6, textInput(ns("model_output_code"), "Output code", width = "100%"))
              ),
              textInput(ns("model_name"), "Display name", width = "100%")
            )
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("function_fields_section"),
            tags$details(
              open = TRUE,
              tags$summary("Database function"),
              fluidRow(
                column(
                  6,
                  textInput(ns("function_schema"), "Function schema", value = "discrete")
                ),
                column(6, textInput(ns("function_name"), "Function name"))
              )
            )
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("sql_scalar_section"),
            tags$details(
              open = TRUE,
              tags$summary("SQL scalar"),
              textAreaInput(ns("formula_sql"), NULL, width = "100%", height = "160px")
            )
          )
        ),

        splitLayout(
          cellWidths = c("50%", "50%"),
          actionButton(ns("save_guideline"), "Save guideline", class = "btn btn-primary"),
          actionButton(ns("test_guideline"), "Test saved guideline", class = "btn btn-primary")
        )
      ),

      div(
        DT::DTOutput(ns("guidelines_table")),
        br(),
        splitLayout(
          cellWidths = c("50%", "50%"),
          actionButton(ns("add_guideline"), "Add new guideline", class = "btn btn-primary"),
          actionButton(ns("delete_guideline"), "Delete selected guideline", class = "btn btn-primary")
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
      is.null(x) || !length(x) || all(is.na(x)) ||
        !nzchar(trimws(as.character(x[[1]])))
    }
    text_value <- function(x) if (is_blank(x)) "" else trimws(as.character(x[[1]]))
    text_or_na <- function(x) {
      val <- text_value(x)
      if (nzchar(val)) val else NA_character_
    }
    text_default <- function(x, default) {
      val <- text_value(x)
      if (nzchar(val)) val else default
    }
    normalize_code <- function(x) {
      val <- toupper(gsub("[^A-Za-z0-9]+", "-", text_value(x)))
      val <- gsub("(^-+|-+$)", "", val)
      if (!nzchar(val)) paste0("GUIDELINE-", format(Sys.time(), "%Y%m%d%H%M%S")) else val
    }
    normalize_input_code <- function(x, fallback = "input") {
      val <- tolower(gsub("[^A-Za-z0-9]+", "_", text_value(x)))
      val <- gsub("(^_+|_+$)", "", val)
      if (nzchar(val)) val else fallback
    }
    integer_or_na <- function(x) {
      if (is_blank(x)) return(NA_integer_)
      out <- suppressWarnings(as.integer(as.character(x[[1]])))
      if (is.na(out)) NA_integer_ else out
    }
    numeric_or_na <- function(x) {
      if (is_blank(x)) return(NA_real_)
      out <- suppressWarnings(as.numeric(as.character(x[[1]])))
      if (is.na(out)) NA_real_ else out
    }
    date_or_na <- function(x) {
      if (is.null(x) || !length(x) || is.na(x[[1]])) return(as.Date(NA))
      as.Date(x[[1]])
    }
    bool_value <- function(x, default = FALSE) {
      if (is_blank(x)) return(default)
      val <- tolower(trimws(as.character(x[[1]])))
      if (val %in% c("true", "t", "1", "yes", "y")) return(TRUE)
      if (val %in% c("false", "f", "0", "no", "n")) return(FALSE)
      default
    }
    parse_id_vector <- function(x) {
      vals <- suppressWarnings(as.integer(as.character(x %||% character(0))))
      unique(vals[!is.na(vals)])
    }
    choice_values <- function(df, id_col, label_col) {
      if (is.null(df) || !nrow(df)) return(character(0))
      stats::setNames(as.character(df[[id_col]]), df[[label_col]])
    }
    lookup_id_from_text <- function(value, df, id_col, text_col) {
      value <- text_or_na(value)
      if (is.na(value) || is.null(df) || !nrow(df)) return(NA_integer_)
      match_row <- match(
        tolower(trimws(value)),
        tolower(trimws(as.character(df[[text_col]])))
      )
      if (is.na(match_row)) NA_integer_ else as.integer(df[[id_col]][[match_row]])
    }
    resolve_lookup_id <- function(
      value, df, id_col, text_col, table_name, code_col, code_prefix
    ) {
      id <- integer_or_na(value)
      if (!is.na(id) && !is.null(df) && id %in% as.integer(df[[id_col]])) {
        return(id)
      }
      label <- text_or_na(value)
      if (is.na(label)) return(NA_integer_)
      existing_id <- lookup_id_from_text(label, df, id_col, text_col)
      if (!is.na(existing_id)) return(existing_id)
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
          id_col, table_name, text_col, code_col
        ),
        params = list(label, code_prefix)
      )
      if (nrow(resolved)) return(as.integer(resolved[[id_col]][[1]]))

      resolved <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT %1$s
           FROM %2$s
           WHERE lower(btrim(%3$s)) = lower(btrim($1))
           LIMIT 1",
          id_col, table_name, text_col
        ),
        params = list(label)
      )
      if (nrow(resolved)) return(as.integer(resolved[[id_col]][[1]]))
      stop("Could not resolve reference value '", label, "'.", call. = FALSE)
    }
    resolve_jurisdiction_id <- function(value) {
      resolve_lookup_id(
        value, moduleData$jurisdictions, "jurisdiction_id", "jurisdiction_name",
        "discrete.guideline_jurisdictions", "jurisdiction_code", "JUR"
      )
    }
    resolve_jurisdiction_level_id <- function(value) {
      resolve_lookup_id(
        value, moduleData$jurisdiction_levels, "jurisdiction_level_id", "jurisdiction_level_name",
        "discrete.guideline_jurisdiction_levels", "jurisdiction_level_code", "LEVEL"
      )
    }
    resolve_protection_goal_id <- function(value) {
      resolve_lookup_id(
        value, moduleData$protection_goals, "protection_goal_id", "protection_goal_name",
        "discrete.guideline_protection_goals", "protection_goal_code", "GOAL"
      )
    }
    resolve_exposure_duration_id <- function(value) {
      resolve_lookup_id(
        value, moduleData$exposure_durations, "exposure_duration_id", "exposure_duration_name",
        "discrete.guideline_exposure_durations", "exposure_duration_code", "EXPOSURE"
      )
    }
    resolve_averaging_period_id <- function(value) {
      resolve_lookup_id(
        value, moduleData$averaging_periods, "averaging_period_id", "averaging_period_name",
        "discrete.guideline_averaging_periods", "averaging_period_code", "AVG"
      )
    }

    guideline_type_choices_for_operator <- function(operator) {
      switch(
        operator %||% "lte",
        lte = c(
          "Fixed upper value" = "constant_upper",
          "Lookup by sample input" = "lookup_range",
          "Multi-input lookup table" = "lookup_grid",
          "One-input formula" = "single_input_formula",
          "Database function" = "db_function",
          "External model result" = "model_result_cache",
          "Advanced SQL scalar" = "sql_scalar"
        ),
        gte = c(
          "Fixed lower value" = "constant_lower",
          "Lookup by sample input" = "lookup_range",
          "Multi-input lookup table" = "lookup_grid",
          "One-input formula" = "single_input_formula",
          "Database function" = "db_function",
          "External model result" = "model_result_cache",
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
      "input_code", "input_name", "parameter_id", "matrix_state_id",
      "sample_fraction_id", "result_speciation_id", "result_type",
      "aggregate_method", "required", "allow_condition_value",
      "lower_calibrated_bound", "upper_calibrated_bound", "bounds_action",
      "note"
    )
    lookup_cols <- c(
      "input_code", "lower_bound", "upper_bound", "lower_inclusive",
      "upper_inclusive", "output_value", "output_status", "output_label",
      "sort_order", "note"
    )
    lookup_grid_base_cols <- c(
      "output_value", "output_status", "output_label", "sort_order", "note"
    )
    coef_cols <- c("coefficient_name", "coefficient_value", "note")

    empty_table_text <- function(cols) paste(cols, collapse = "\t")
    lookup_grid_cols_from_codes <- function(input_codes) {
      input_codes <- input_codes[!is.na(input_codes) & nzchar(trimws(input_codes))]
      if (!length(input_codes)) input_codes <- c("input_1", "input_2")
      range_cols <- unlist(lapply(input_codes, function(input_code) {
        paste0(
          input_code,
          c("_lower_bound", "_upper_bound", "_lower_inclusive", "_upper_inclusive")
        )
      }), use.names = FALSE)
      c(lookup_grid_base_cols, range_cols)
    }
    lookup_grid_cols_for_inputs <- function(inputs) {
      if (is.null(inputs) || !nrow(inputs)) {
        return(lookup_grid_cols_from_codes(c("input_1", "input_2")))
      }
      input_codes <- as.character(inputs$input_code %||% "")
      input_codes <- vapply(seq_along(input_codes), function(i) {
        normalize_input_code(input_codes[[i]], fallback = paste0("input_", i))
      }, character(1))
      lookup_grid_cols_from_codes(input_codes)
    }
    empty_lookup_grid_text <- function(inputs = NULL) {
      empty_table_text(lookup_grid_cols_for_inputs(inputs))
    }
    format_table_text <- function(df, cols) {
      if (is.null(df) || !nrow(df)) return(empty_table_text(cols))
      for (col in cols) if (!col %in% names(df)) df[[col]] <- NA
      rows <- apply(df[, cols, drop = FALSE], 1, function(row) {
        paste(vapply(row, function(value) {
          if (is.na(value)) "" else as.character(value)
        }, character(1)), collapse = "\t")
      })
      paste(c(paste(cols, collapse = "\t"), rows), collapse = "\n")
    }
    parse_table_text <- function(x, required_cols, label) {
      txt <- text_value(x)
      if (!nzchar(txt)) return(data.frame(stringsAsFactors = FALSE))
      sep <- if (grepl("\t", txt, fixed = TRUE)) "\t" else ","
      df <- tryCatch(
        utils::read.table(
          text = txt, header = TRUE, sep = sep, stringsAsFactors = FALSE,
          na.strings = c("", "NA", "NULL"), quote = "\"",
          comment.char = "", fill = TRUE
        ),
        error = function(e) stop("Could not read ", label, ": ", conditionMessage(e), call. = FALSE)
      )
      names(df) <- trimws(names(df))
      missing_cols <- setdiff(required_cols, names(df))
      if (length(missing_cols)) {
        stop(label, " is missing column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
      }
      df
    }

    moduleData <- reactiveValues()
    selected_guideline_id <- reactiveVal(NA_integer_)
    rule_input_count <- reactiveVal(1L)
    rule_input_seed <- reactiveVal(NULL)

    selectize_single_options <- list(maxItems = 1)
    selected_or_empty <- function(x) {
      if (is.null(x) || !length(x) || is.na(x[[1]]) || !nzchar(as.character(x[[1]]))) {
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
      labels[has_units] <- paste0(labels[has_units], " (", units[has_units], ")")
      labels
    }
    parameter_choices <- function() {
      if (is.null(moduleData$parameters) || !nrow(moduleData$parameters)) {
        return(character(0))
      }
      stats::setNames(as.character(moduleData$parameters$parameter_id), parameter_choice_labels())
    }
    matrix_state_choices <- function() {
      choice_values(moduleData$matrix_states, "matrix_state_id", "matrix_state_name")
    }
    sample_fraction_choices <- function() {
      choice_values(moduleData$sample_fractions, "sample_fraction_id", "sample_fraction")
    }
    speciation_choices <- function() {
      choice_values(moduleData$result_speciations, "result_speciation_id", "result_speciation")
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
    parameter_requires_speciation <- function(parameter_id) {
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
    update_result_speciation_visibility <- function(parameter_id = input$parameter_id) {
      if (parameter_requires_speciation(parameter_id)) {
        shinyjs::show(id = "result_speciation_section", anim = FALSE)
      } else {
        updateSelectizeInput(session, "result_speciation", selected = character(0))
        shinyjs::hide(id = "result_speciation_section", anim = FALSE)
      }
    }
    blank_rule_input_rows <- function(n = 1L) {
      data.frame(
        input_code = rep("", n),
        input_name = rep("", n),
        parameter_id = rep(NA_integer_, n),
        matrix_state_id = rep(NA_integer_, n),
        sample_fraction_id = rep(NA_integer_, n),
        result_speciation_id = rep(NA_integer_, n),
        result_type = rep(NA_integer_, n),
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
      if (is.null(df) || !nrow(df)) return(blank_rule_input_rows(1L))
      for (col in input_cols) {
        if (!col %in% names(df)) df[[col]] <- blank_rule_input_rows(1L)[[col]][[1]]
      }
      df <- df[, input_cols, drop = FALSE]
      df$input_code <- as.character(df$input_code %||% "")
      df$input_name <- as.character(df$input_name %||% "")
      df$aggregate_method[is.na(df$aggregate_method) | !nzchar(df$aggregate_method)] <- "single"
      df$bounds_action[is.na(df$bounds_action) | !nzchar(df$bounds_action)] <- "flag"
      df$required <- vapply(df$required, bool_value, logical(1), default = TRUE)
      df$allow_condition_value <- vapply(df$allow_condition_value, bool_value, logical(1), default = FALSE)
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
      if (is.null(n) || !n) return(blank_rule_input_rows(0L))
      rows <- lapply(seq_len(n), function(i) {
        data.frame(
          input_code = text_value(input[[rule_input_id("code", i)]]),
          input_name = text_value(input[[rule_input_id("name", i)]]),
          parameter_id = integer_or_na(input[[rule_input_id("parameter", i)]]),
          matrix_state_id = integer_or_na(input[[rule_input_id("matrix_state", i)]]),
          sample_fraction_id = integer_or_na(input[[rule_input_id("sample_fraction", i)]]),
          result_speciation_id = integer_or_na(input[[rule_input_id("speciation", i)]]),
          result_type = integer_or_na(input[[rule_input_id("result_type", i)]]),
          aggregate_method = text_default(input[[rule_input_id("aggregate_method", i)]], "single"),
          required = isTRUE(input[[rule_input_id("required", i)]]),
          allow_condition_value = isTRUE(input[[rule_input_id("allow_condition_value", i)]]),
          lower_calibrated_bound = numeric_or_na(input[[rule_input_id("lower_calibrated_bound", i)]]),
          upper_calibrated_bound = numeric_or_na(input[[rule_input_id("upper_calibrated_bound", i)]]),
          bounds_action = text_default(input[[rule_input_id("bounds_action", i)]], "flag"),
          note = text_value(input[[rule_input_id("note", i)]]),
          stringsAsFactors = FALSE
        )
      })
      df <- do.call(rbind, rows)
      if (include_empty) return(df)
      has_content <- nzchar(trimws(df$input_code)) |
        nzchar(trimws(df$input_name)) |
        !is.na(df$parameter_id)
      df[has_content, , drop = FALSE]
    }

    load_guidelines <- function() {
      DBI::dbGetQuery(
        con,
        "WITH rule_summary AS (
           SELECT guideline_id,
                  count(*)::integer AS rule_count,
                  string_agg(COALESCE(bound_code, 'no-bound') || ':' || algorithm_code, ', ' ORDER BY rule_priority, rule_id) AS rules
           FROM discrete.guideline_value_rules
           GROUP BY guideline_id
         ),
         fr AS (
           SELECT gf.guideline_id,
                  string_agg(sf.sample_fraction, ', ' ORDER BY sf.sample_fraction) AS fractions,
                  string_agg(gf.fraction_id::text, ',' ORDER BY gf.fraction_id::text) AS fraction_ids
           FROM discrete.guidelines_fractions gf
           JOIN discrete.sample_fractions sf ON sf.sample_fraction_id = gf.fraction_id
           GROUP BY gf.guideline_id
         ),
         mt AS (
           SELECT gm.guideline_id,
                  string_agg(mt.media_type, ', ' ORDER BY mt.media_type) AS media_types,
                  string_agg(gm.media_id::text, ',' ORDER BY gm.media_id::text) AS media_ids
           FROM discrete.guidelines_media_types gm
           JOIN public.media_types mt ON mt.media_id = gm.media_id
           GROUP BY gm.guideline_id
         )
         SELECT g.guideline_id, g.guideline_code, g.guideline_name,
                gp.publisher_name AS publisher, gs.series_name AS series,
                p.param_name AS parameter,
                public.get_parameter_unit_name(g.parameter_id, g.matrix_state_id) AS units,
                ms.matrix_state_name AS matrix_state, fr.fractions, mt.media_types,
                rs.result_speciation AS speciation, g.comparison_operator_code,
                gj.jurisdiction_name AS jurisdiction,
                gpg.protection_goal_name AS protection_goal,
                ged.exposure_duration_name AS exposure_duration,
                gap.averaging_period_name AS averaging_period,
                g.valid_from, g.valid_to, g.review_status,
                COALESCE(rule_summary.rule_count, 0) AS rule_count,
                rule_summary.rules, g.publisher_id, g.series_id, g.parameter_id,
                g.matrix_state_id, g.result_speciation_id, fr.fraction_ids,
                mt.media_ids, g.reference, g.general_notes, g.applicability_notes,
                gjl.jurisdiction_level_name AS jurisdiction_level,
                g.jurisdiction_id, g.jurisdiction_level_id,
                g.protection_goal_id, g.exposure_duration_id, g.averaging_period_id,
                g.source_document_title, g.source_url,
                g.source_page, g.source_table, g.source_section,
                g.source_effective_date, g.source_retrieved_date
         FROM discrete.guidelines g
         LEFT JOIN discrete.guideline_publishers gp ON gp.publisher_id = g.publisher_id
         LEFT JOIN discrete.guideline_series gs ON gs.series_id = g.series_id
         LEFT JOIN discrete.guideline_jurisdictions gj ON gj.jurisdiction_id = g.jurisdiction_id
         LEFT JOIN discrete.guideline_jurisdiction_levels gjl ON gjl.jurisdiction_level_id = g.jurisdiction_level_id
         LEFT JOIN discrete.guideline_protection_goals gpg ON gpg.protection_goal_id = g.protection_goal_id
         LEFT JOIN discrete.guideline_exposure_durations ged ON ged.exposure_duration_id = g.exposure_duration_id
         LEFT JOIN discrete.guideline_averaging_periods gap ON gap.averaging_period_id = g.averaging_period_id
         JOIN public.parameters p ON p.parameter_id = g.parameter_id
         LEFT JOIN public.matrix_states ms ON ms.matrix_state_id = g.matrix_state_id
         LEFT JOIN discrete.result_speciations rs ON rs.result_speciation_id = g.result_speciation_id
         LEFT JOIN rule_summary ON rule_summary.guideline_id = g.guideline_id
         LEFT JOIN fr ON fr.guideline_id = g.guideline_id
         LEFT JOIN mt ON mt.guideline_id = g.guideline_id
         ORDER BY g.guideline_code, g.guideline_name"
      )
    }

    load_reference_data <- function() {
      moduleData$guidelines <- load_guidelines()
      moduleData$publishers <- DBI::dbGetQuery(
        con,
        "SELECT publisher_id, publisher_name FROM discrete.guideline_publishers ORDER BY publisher_name"
      )
      moduleData$series <- DBI::dbGetQuery(
        con,
        "SELECT series_id, series_name, publisher_id FROM discrete.guideline_series ORDER BY series_name"
      )
      moduleData$matrix_states <- DBI::dbGetQuery(
        con,
        "SELECT matrix_state_id, matrix_state_code, matrix_state_name FROM public.matrix_states ORDER BY matrix_state_name"
      )
      moduleData$media_types <- DBI::dbGetQuery(
        con,
        "SELECT media_id, media_type, default_matrix_state_id FROM public.media_types ORDER BY media_type"
      )
      moduleData$parameters <- DBI::dbGetQuery(
        con,
        "SELECT p.parameter_id, p.param_name,
                public.get_parameter_unit_name(p.parameter_id, NULL::integer) AS unit_default,
                p.result_speciation, p.sample_fraction
         FROM public.parameters p
         ORDER BY p.param_name"
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
         FROM discrete.guideline_jurisdictions
         WHERE active
         ORDER BY sort_order, jurisdiction_name"
      )
      moduleData$jurisdiction_levels <- DBI::dbGetQuery(
        con,
        "SELECT jurisdiction_level_id, jurisdiction_level_name
         FROM discrete.guideline_jurisdiction_levels
         WHERE active
         ORDER BY sort_order, jurisdiction_level_name"
      )
      moduleData$protection_goals <- DBI::dbGetQuery(
        con,
        "SELECT protection_goal_id, protection_goal_name
         FROM discrete.guideline_protection_goals
         WHERE active
         ORDER BY sort_order, protection_goal_name"
      )
      moduleData$exposure_durations <- DBI::dbGetQuery(
        con,
        "SELECT exposure_duration_id, exposure_duration_name
         FROM discrete.guideline_exposure_durations
         WHERE active
         ORDER BY sort_order, exposure_duration_name"
      )
      moduleData$averaging_periods <- DBI::dbGetQuery(
        con,
        "SELECT averaging_period_id, averaging_period_name
         FROM discrete.guideline_averaging_periods
         WHERE active
         ORDER BY sort_order, averaging_period_name"
      )
      moduleData$operators <- DBI::dbGetQuery(
        con,
        "SELECT operator_code, operator_name FROM discrete.guideline_comparison_operators ORDER BY operator_code"
      )
    }

    update_choices <- function() {
      shiny::isolate({
        updateSelectizeInput(session, "publisher", choices = choice_values(moduleData$publishers, "publisher_id", "publisher_name"), server = TRUE)
        updateSelectizeInput(session, "series", choices = choice_values(moduleData$series, "series_id", "series_name"), server = TRUE)
        updateSelectizeInput(session, "parameter_id", choices = parameter_choices(), server = TRUE)
        updateSelectizeInput(session, "matrix_state", choices = choice_values(moduleData$matrix_states, "matrix_state_id", "matrix_state_name"), server = TRUE)
        updateSelectizeInput(session, "result_speciation", choices = choice_values(moduleData$result_speciations, "result_speciation_id", "result_speciation"), server = TRUE)
        updateSelectizeInput(session, "sample_fraction", choices = choice_values(moduleData$sample_fractions, "sample_fraction_id", "sample_fraction"), server = TRUE)
        updateSelectizeInput(session, "media_type", choices = choice_values(moduleData$media_types, "media_id", "media_type"), server = TRUE)
        updateSelectizeInput(session, "jurisdiction", choices = choice_values(moduleData$jurisdictions, "jurisdiction_id", "jurisdiction_name"), server = TRUE)
        updateSelectizeInput(session, "jurisdiction_level", choices = choice_values(moduleData$jurisdiction_levels, "jurisdiction_level_id", "jurisdiction_level_name"), server = TRUE)
        updateSelectizeInput(session, "protection_goal", choices = choice_values(moduleData$protection_goals, "protection_goal_id", "protection_goal_name"), server = TRUE)
        updateSelectizeInput(session, "exposure_duration", choices = choice_values(moduleData$exposure_durations, "exposure_duration_id", "exposure_duration_name"), server = TRUE)
        updateSelectizeInput(session, "averaging_period", choices = choice_values(moduleData$averaging_periods, "averaging_period_id", "averaging_period_name"), server = TRUE)
        updateSelectInput(
          session, "comparison_operator",
          choices = stats::setNames(
            moduleData$operators$operator_code,
            paste(moduleData$operators$operator_code, moduleData$operators$operator_name, sep = " - ")
          ),
          selected = "lte"
        )
      })
    }
    update_guideline_type_choices <- function(operator, selected = NULL) {
      choices <- guideline_type_choices_for_operator(operator)
      selected <- selected %||% default_type_for_operator(operator)
      if (!selected %in% unname(choices)) {
        selected <- default_type_for_operator(operator)
      }
      updateSelectInput(session, "guideline_type", choices = choices, selected = selected)
      if (operator %in% c("lte", "gte", "eq")) {
        updateSelectInput(session, "bound_code", selected = bound_for_operator(operator))
      }
      update_rule_field_visibility(selected)
      selected
    }

    load_reference_data()
    session$onFlushed(function() {
      update_choices()
      set_rule_input_rows(blank_rule_input_rows(1L))
      update_result_speciation_visibility(character(0))
      updateTextAreaInput(session, "lookup_values_text", value = empty_table_text(lookup_cols))
      updateTextAreaInput(session, "lookup_grid_values_text", value = empty_lookup_grid_text())
      updateTextAreaInput(session, "coefficients_text", value = empty_table_text(coef_cols))
      update_guideline_type_choices("lte", "constant_upper")
    }, once = TRUE)

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns, lang = language$language, con = con, module_id = "addGuidelines"
      )
    })

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
        lookup_range = "Add one sample input row, then map input ranges to the upper or lower limit chosen by Comparison.",
        lookup_grid = "Add two or more sample input rows, then map combinations of input ranges to the upper or lower limit chosen by Comparison.",
        single_input_formula = "Add one sample input row, then provide coefficients for the limit chosen by Comparison.",
        db_function = "Add sample input rows and the database function that returns the limit chosen by Comparison.",
        model_result_cache = "Add sample input rows used to match a stored external model result for the limit chosen by Comparison.",
        narrative = "Use the note fields when the guideline does not produce a numeric value.",
        sql_scalar = "Enter a database-owned scalar SQL expression for the selected bound.",
        "Choose the rule type that matches how the guideline value is derived."
      )
      tags$p(class = "text-muted", style = "margin-bottom:8px;", hint)
    })

    output$calculation_source_ui <- renderUI({
      value_text <- function(x, fallback = "Not set") {
        if (is.null(x) || !length(x) || is.na(x[[1]]) || !nzchar(as.character(x[[1]]))) {
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
      rules <- if (is.na(guideline_id)) data.frame() else load_rules(guideline_id)
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
          lookup_range = "lookup_range",
          lookup_grid = "lookup_grid",
          single_input_formula = input$formula_algorithm %||% "linear",
          db_function = "db_function",
          model_result_cache = "model_result_cache",
          narrative = "narrative",
          sql_scalar = "sql_scalar",
          "constant"
        )
      }

      model_code <- if (!is.null(primary)) primary$model_code[[1]] else text_value(input$model_code)
      output_code <- if (!is.null(primary)) primary$model_output_code[[1]] else text_value(input$model_output_code)
      summary_text <- switch(
        algorithm,
        constant = "The guideline value is stored directly on the value rule.",
        lookup_range = "The selected sample input is compared with lookup rows; the matching row supplies the guideline value.",
        lookup_grid = "Multiple sample inputs are compared with lookup-grid cells; the matching cell supplies the guideline value.",
        linear = "The selected sample input and coefficients are evaluated by the database formula rule.",
        log_linear = "The selected sample input and coefficients are evaluated by the database formula rule.",
        power = "The selected sample input and coefficients are evaluated by the database formula rule.",
        db_function = "The database resolves the sample inputs and passes them to the registered database function.",
        model_result_cache = "The database resolves the sample inputs, computes an input hash, and reads the matching stored model result.",
        narrative = "This guideline is stored as narrative or site-specific guidance, without a numeric calculation.",
        sql_scalar = "The database executes the stored scalar SQL expression for this rule.",
        "The database evaluates the stored value rule for this guideline."
      )

      details <- list(item("Stored as", algorithm))
      if (algorithm == "model_result_cache") {
        if (nzchar(value_text(model_code, "")) && nzchar(value_text(output_code, ""))) {
          model_summary <- DBI::dbGetQuery(
            con,
            "SELECT gm.model_name, gm.model_type, gmo.output_name,
                    count(gmr.model_result_id)::integer AS stored_values,
                    max(gmr.model_run_datetime) AS latest_run
             FROM discrete.guideline_models gm
             LEFT JOIN discrete.guideline_model_outputs gmo
               ON gmo.model_code = gm.model_code
              AND gmo.output_code = $2
             LEFT JOIN discrete.guideline_model_results gmr
               ON gmr.model_code = gm.model_code
              AND gmr.model_output_code = $2
             WHERE gm.model_code = $1
             GROUP BY gm.model_name, gm.model_type, gmo.output_name",
            params = list(model_code, output_code)
          )
        } else {
          model_summary <- data.frame()
        }
        details <- c(
          details,
          list(
            item("Derivation code", model_code),
            item("Output code", output_code),
            item("Value table", "discrete.guideline_model_results"),
            item("Match key", "sample_id + input hash from Rule inputs")
          )
        )
        if (nrow(model_summary)) {
          details <- c(
            details,
            list(
              item("Display name", model_summary$model_name[[1]]),
              item("Stored values", model_summary$stored_values[[1]]),
              item("Latest run", model_summary$latest_run[[1]])
            )
          )
        }
      } else if (algorithm == "db_function") {
        details <- c(
          details,
          list(
            item("Function", paste(value_text(if (!is.null(primary)) primary$function_schema[[1]] else input$function_schema), value_text(if (!is.null(primary)) primary$function_name[[1]] else input$function_name), sep = ".")),
            item("Inputs passed as", "JSONB from Rule inputs")
          )
        )
      } else if (algorithm %in% c("linear", "log_linear", "power")) {
        coef_count <- if (!is.null(primary)) nrow(load_coefficients(primary$rule_id[[1]])) else NA_integer_
        details <- c(
          details,
          list(
            item("Formula", algorithm),
            item("Coefficient rows", coef_count)
          )
        )
      } else if (algorithm == "lookup_range") {
        lookup_count <- if (!is.null(primary)) nrow(load_lookup(primary$rule_id[[1]])) else NA_integer_
        details <- c(
          details,
          list(
            item("Lookup rows", lookup_count),
            item("Lookup table", "discrete.guideline_lookup_values")
          )
        )
      } else if (algorithm == "lookup_grid") {
        grid_count <- if (!is.null(primary)) nrow(load_lookup_grid(primary$rule_id[[1]])) else NA_integer_
        details <- c(
          details,
          list(
            item("Lookup cells", grid_count),
            item("Lookup header", "discrete.guideline_lookup_tables"),
            item("Dimensions", "discrete.guideline_lookup_dimensions"),
            item("Cell ranges", "discrete.guideline_lookup_cell_ranges")
          )
        )
      } else if (algorithm == "sql_scalar") {
        details <- c(details, list(item("SQL source", "Stored on this value rule")))
      }

      tags$div(
        style = paste(
          "border:1px solid #d7d7d7;",
          "padding:10px;",
          "margin-bottom:10px;",
          "background:#fff;"
        ),
        tags$strong("Calculation source"),
        tags$p(class = "text-muted", style = "margin:4px 0 8px 0;", summary_text),
        details
      )
    })

    output$rule_inputs_ui <- renderUI({
      n <- rule_input_count()
      if (is.null(n) || n < 1L) return(NULL)
      seed <- normalize_rule_input_rows(rule_input_seed())
      if (nrow(seed) < n) {
        seed <- rbind(seed, blank_rule_input_rows(n - nrow(seed)))
      }
      tagList(lapply(seq_len(n), function(i) {
        row <- seed[i, , drop = FALSE]
        tags$div(
          style = paste(
            "border-top:1px solid #d7d7d7;",
            "padding-top:10px;",
            "margin-top:10px;"
          ),
          tags$h5(paste("Input", i)),
          fluidRow(
            column(
              3,
              textInput(
                ns(rule_input_id("code", i)),
                "Input code",
                value = row$input_code[[1]] %||% "",
                width = "100%"
              )
            ),
            column(
              3,
              textInput(
                ns(rule_input_id("name", i)),
                "Input name",
                value = row$input_name[[1]] %||% "",
                width = "100%"
              )
            ),
            column(
              3,
              selectizeInput(
                ns(rule_input_id("parameter", i)),
                "Parameter",
                choices = parameter_choices(),
                selected = selected_or_empty(row$parameter_id),
                multiple = TRUE,
                options = selectize_single_options,
                width = "100%"
              )
            ),
            column(
              3,
              selectizeInput(
                ns(rule_input_id("matrix_state", i)),
                "Matrix state",
                choices = choices_with_any(matrix_state_choices()),
                selected = selected_or_empty(row$matrix_state_id),
                multiple = TRUE,
                options = selectize_single_options,
                width = "100%"
              )
            )
          ),
          fluidRow(
            column(
              3,
              selectizeInput(
                ns(rule_input_id("sample_fraction", i)),
                "Fraction",
                choices = choices_with_any(sample_fraction_choices()),
                selected = selected_or_empty(row$sample_fraction_id),
                multiple = TRUE,
                options = selectize_single_options,
                width = "100%"
              )
            ),
            column(
              3,
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
              3,
              selectInput(
                ns(rule_input_id("aggregate_method", i)),
                "Multiple results",
                choices = c(
                  "Require one result" = "single",
                  "Average" = "avg",
                  "Minimum" = "min",
                  "Maximum" = "max"
                ),
                selected = row$aggregate_method[[1]] %||% "single"
              )
            ),
            column(
              3,
              selectizeInput(
                ns(rule_input_id("result_type", i)),
                "Result type",
                choices = choices_with_any(result_type_choices()),
                selected = selected_or_empty(row$result_type),
                multiple = TRUE,
                options = selectize_single_options,
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
                  "Required",
                  value = isTRUE(row$required[[1]])
                )
              ),
              column(
                3,
                checkboxInput(
                  ns(rule_input_id("allow_condition_value", i)),
                  "Use condition value",
                  value = isTRUE(row$allow_condition_value[[1]])
                )
              ),
              column(
                3,
                numericInput(
                  ns(rule_input_id("lower_calibrated_bound", i)),
                  "Calibration low",
                  value = row$lower_calibrated_bound[[1]]
                )
              ),
              column(
                3,
                numericInput(
                  ns(rule_input_id("upper_calibrated_bound", i)),
                  "Calibration high",
                  value = row$upper_calibrated_bound[[1]]
                )
              )
            ),
            fluidRow(
              column(
                3,
                selectInput(
                  ns(rule_input_id("bounds_action", i)),
                  "Out-of-range input",
                  choices = c("Flag" = "flag", "Clamp" = "clamp", "Reject" = "reject"),
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
        "fixed_value_section", "range_value_section", "bound_section",
        "formula_algorithm_section", "advanced_rule_options_section",
        "rule_inputs_section", "lookup_values_section", "lookup_grid_values_section",
        "coefficients_section", "model_fields_section", "function_fields_section",
        "sql_scalar_section"
      )
      invisible(lapply(sections, function(id) shinyjs::hide(id = id, anim = FALSE)))

      if (type %in% c("constant_upper", "constant_lower")) {
        shinyjs::show(id = "fixed_value_section", anim = FALSE)
      }
      if (identical(type, "constant_range")) {
        shinyjs::show(id = "range_value_section", anim = FALSE)
      }
      if (type %in% c("lookup_range", "lookup_grid", "single_input_formula", "db_function", "model_result_cache", "sql_scalar")) {
        shinyjs::show(id = "advanced_rule_options_section", anim = FALSE)
      }
      if (identical(type, "single_input_formula")) {
        shinyjs::show(id = "formula_algorithm_section", anim = FALSE)
        shinyjs::show(id = "rule_inputs_section", anim = FALSE)
        shinyjs::show(id = "coefficients_section", anim = FALSE)
      }
      if (identical(type, "lookup_range")) {
        shinyjs::show(id = "rule_inputs_section", anim = FALSE)
        shinyjs::show(id = "lookup_values_section", anim = FALSE)
      }
      if (identical(type, "lookup_grid")) {
        shinyjs::show(id = "rule_inputs_section", anim = FALSE)
        shinyjs::show(id = "lookup_grid_values_section", anim = FALSE)
      }
      if (type %in% c("db_function", "model_result_cache")) {
        shinyjs::show(id = "rule_inputs_section", anim = FALSE)
        shinyjs::show(id = "model_fields_section", anim = FALSE)
      }
      if (identical(type, "db_function")) {
        shinyjs::show(id = "function_fields_section", anim = FALSE)
      }
      if (identical(type, "sql_scalar")) {
        shinyjs::show(id = "sql_scalar_section", anim = FALSE)
      }
    }

    last_comparison_operator <- reactiveVal(NULL)

    observeEvent(input$guideline_type, {
      type <- input$guideline_type %||% "constant_upper"
      update_rule_field_visibility(type)
      operator <- input$comparison_operator %||% "lte"
      if (operator %in% c("lte", "gte", "eq")) {
        updateSelectInput(session, "bound_code", selected = bound_for_operator(operator))
      }
    }, ignoreInit = FALSE)

    observeEvent(input$comparison_operator, {
      operator <- input$comparison_operator %||% "lte"
      previous_operator <- last_comparison_operator()
      selected <- update_guideline_type_choices(operator, input$guideline_type)
      if (!is.null(previous_operator) && !identical(previous_operator, operator)) {
        updateNumericInput(session, "fixed_value", value = NA_real_)
        updateNumericInput(session, "lower_value", value = NA_real_)
        updateNumericInput(session, "upper_value", value = NA_real_)
      }
      update_rule_field_visibility(selected)
      last_comparison_operator(operator)
    }, ignoreInit = FALSE)

    observeEvent(input$parameter_id, {
      update_result_speciation_visibility(input$parameter_id)
    }, ignoreInit = FALSE)

    observe({
      n <- rule_input_count()
      if (is.null(n) || n < 1L) return()
      for (i in seq_len(n)) {
        parameter_id <- input[[rule_input_id("parameter", i)]]
        if (parameter_requires_speciation(parameter_id)) {
          shinyjs::show(id = rule_input_id("speciation_section", i), anim = FALSE)
        } else {
          updateSelectizeInput(
            session,
            rule_input_id("speciation", i),
            selected = character(0)
          )
          shinyjs::hide(id = rule_input_id("speciation_section", i), anim = FALSE)
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

    output$guidelines_table <- DT::renderDT({
      cols <- c(
        "guideline_id", "guideline_code", "guideline_name", "publisher",
        "series", "parameter", "units", "matrix_state", "fractions",
        "media_types", "comparison_operator_code", "exposure_duration",
        "averaging_period", "review_status", "rule_count", "rules"
      )
      df <- moduleData$guidelines
      for (col in cols) if (!col %in% names(df)) df[[col]] <- NA
      DT::datatable(
        df[, cols, drop = FALSE], selection = "single", rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 10, scrollX = TRUE,
          columnDefs = list(list(targets = 0, visible = FALSE)),
          layout = list(bottomStart = "info", bottomEnd = "paging")
        )
      )
    }, server = TRUE)

    load_rules <- function(guideline_id) {
      DBI::dbGetQuery(
        con,
        "SELECT * FROM discrete.guideline_value_rules
         WHERE guideline_id = $1
         ORDER BY rule_priority, rule_id",
        params = list(guideline_id)
      )
    }
    load_inputs <- function(rule_id) {
      DBI::dbGetQuery(
        con,
        "SELECT gri.input_code, gri.input_name, gri.parameter_id,
                gri.matrix_state_id, gri.sample_fraction_id,
                gri.result_speciation_id, gri.result_type,
                gri.aggregate_method, gri.required, gri.allow_condition_value,
                gmi.lower_calibrated_bound, gmi.upper_calibrated_bound,
                COALESCE(gmi.bounds_action, 'flag') AS bounds_action, gri.note
         FROM discrete.guideline_rule_inputs gri
         JOIN discrete.guideline_value_rules gr ON gr.rule_id = gri.rule_id
         LEFT JOIN discrete.guideline_model_inputs gmi
           ON gmi.model_code = gr.model_code AND gmi.input_code = gri.input_code
         WHERE gri.rule_id = $1
         ORDER BY gri.input_code",
        params = list(rule_id)
      )
    }
    load_lookup <- function(rule_id) {
      DBI::dbGetQuery(
        con,
        "SELECT input_code, lower_bound, upper_bound, lower_inclusive,
                upper_inclusive, output_value, output_status, output_label,
                sort_order, note
         FROM discrete.guideline_lookup_values
         WHERE rule_id = $1
         ORDER BY sort_order, lookup_id",
        params = list(rule_id)
      )
    }
    load_lookup_grid <- function(rule_id) {
      rows <- DBI::dbGetQuery(
        con,
        "SELECT glc.cell_id, glc.output_value, glc.output_status,
                glc.output_label, glc.sort_order, glc.note,
                gld.input_code, glcr.lower_bound, glcr.upper_bound,
                glcr.lower_inclusive, glcr.upper_inclusive,
                gld.sort_order AS dimension_sort_order
         FROM discrete.guideline_lookup_tables glt
         JOIN discrete.guideline_lookup_cells glc
           ON glc.lookup_table_id = glt.lookup_table_id
         JOIN discrete.guideline_lookup_dimensions gld
           ON gld.lookup_table_id = glt.lookup_table_id
         LEFT JOIN discrete.guideline_lookup_cell_ranges glcr
           ON glcr.lookup_table_id = glt.lookup_table_id
          AND glcr.cell_id = glc.cell_id
          AND glcr.dimension_id = gld.dimension_id
         WHERE glt.rule_id = $1
         ORDER BY glc.sort_order, glc.cell_id, gld.sort_order, gld.dimension_id",
        params = list(rule_id)
      )
      if (!nrow(rows)) return(data.frame(stringsAsFactors = FALSE))
      input_codes <- unique(rows$input_code[order(rows$dimension_sort_order, rows$input_code)])
      cell_groups <- split(rows, rows$cell_id)
      out <- do.call(rbind, lapply(cell_groups, function(cell_rows) {
        cell_rows <- cell_rows[order(cell_rows$dimension_sort_order, cell_rows$input_code), , drop = FALSE]
        out_row <- as.list(cell_rows[1, c(
          "output_value", "output_status", "output_label", "sort_order", "note"
        )])
        for (i in seq_len(nrow(cell_rows))) {
          prefix <- cell_rows$input_code[[i]]
          out_row[[paste0(prefix, "_lower_bound")]] <- cell_rows$lower_bound[[i]]
          out_row[[paste0(prefix, "_upper_bound")]] <- cell_rows$upper_bound[[i]]
          out_row[[paste0(prefix, "_lower_inclusive")]] <- cell_rows$lower_inclusive[[i]]
          out_row[[paste0(prefix, "_upper_inclusive")]] <- cell_rows$upper_inclusive[[i]]
        }
        as.data.frame(out_row, stringsAsFactors = FALSE, check.names = FALSE)
      }))
      rownames(out) <- NULL
      attr(out, "input_codes") <- input_codes
      out
    }
    format_lookup_grid_text <- function(rule_id) {
      df <- load_lookup_grid(rule_id)
      input_codes <- attr(df, "input_codes")
      cols <- if (is.null(input_codes) || !length(input_codes)) {
        lookup_grid_cols_for_inputs(collect_rule_input_rows())
      } else {
        lookup_grid_cols_from_codes(input_codes)
      }
      format_table_text(df, cols)
    }
    load_coefficients <- function(rule_id) {
      DBI::dbGetQuery(
        con,
        "SELECT coefficient_name, coefficient_value, note
         FROM discrete.guideline_rule_coefficients
         WHERE rule_id = $1
         ORDER BY coefficient_name",
        params = list(rule_id)
      )
    }

    output$selected_rules_table <- DT::renderDT({
      guideline_id <- selected_guideline_id()
      if (is.na(guideline_id)) return(DT::datatable(data.frame(), rownames = FALSE))
      DT::datatable(load_rules(guideline_id), rownames = FALSE, options = list(scrollX = TRUE))
    })
    output$selected_inputs_table <- DT::renderDT({
      guideline_id <- selected_guideline_id()
      if (is.na(guideline_id)) return(DT::datatable(data.frame(), rownames = FALSE))
      rules <- load_rules(guideline_id)
      if (!nrow(rules)) return(DT::datatable(data.frame(), rownames = FALSE))
      inputs <- do.call(rbind, lapply(rules$rule_id, function(rule_id) {
        out <- load_inputs(rule_id)
        if (nrow(out)) out$rule_id <- rule_id
        out
      }))
      if (is.null(inputs)) inputs <- data.frame()
      DT::datatable(inputs, rownames = FALSE, options = list(scrollX = TRUE))
    })

    infer_guideline_type <- function(rules) {
      if (!nrow(rules)) return("constant_upper")
      if (
        nrow(rules) == 2 &&
          all(rules$algorithm_code == "constant") &&
          all(c("lower", "upper") %in% rules$bound_code)
      ) {
        return("constant_range")
      }
      first <- rules[1, , drop = FALSE]
      if (first$algorithm_code == "constant" && first$bound_code == "lower") return("constant_lower")
      if (first$algorithm_code == "constant") return("constant_upper")
      if (first$algorithm_code %in% c("linear", "log_linear", "power")) return("single_input_formula")
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
      updateSelectizeInput(session, "result_speciation", selected = character(0))
      updateSelectizeInput(session, "sample_fraction", selected = character(0))
      updateSelectizeInput(session, "media_type", selected = character(0))
      updateSelectInput(session, "comparison_operator", selected = "lte")
      updateSelectizeInput(session, "jurisdiction", selected = character(0))
      updateSelectizeInput(session, "jurisdiction_level", selected = character(0))
      updateSelectizeInput(session, "protection_goal", selected = character(0))
      updateSelectizeInput(session, "exposure_duration", selected = character(0))
      updateSelectizeInput(session, "averaging_period", selected = character(0))
      updateDateInput(session, "valid_from", value = Sys.Date())
      updateDateInput(session, "valid_to", value = NA)
      updateSelectInput(session, "review_status", selected = "draft")
      updateTextInput(session, "reference", value = "")
      updateTextInput(session, "source_document_title", value = "")
      updateTextInput(session, "source_url", value = "")
      updateTextInput(session, "source_page", value = "")
      updateTextInput(session, "source_table", value = "")
      updateTextInput(session, "source_section", value = "")
      updateDateInput(session, "source_effective_date", value = NA)
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
      updateTextAreaInput(session, "lookup_values_text", value = empty_table_text(lookup_cols))
      updateTextAreaInput(session, "lookup_grid_values_text", value = empty_lookup_grid_text())
      updateTextAreaInput(session, "coefficients_text", value = empty_table_text(coef_cols))
      updateTextInput(session, "model_code", value = "")
      updateTextInput(session, "model_output_code", value = "")
      updateTextInput(session, "model_name", value = "")
      updateTextInput(session, "function_schema", value = "discrete")
      updateTextInput(session, "function_name", value = "")
      updateTextAreaInput(session, "formula_sql", value = "")
      update_result_speciation_visibility(character(0))
    }

    load_form <- function(guideline_id) {
      g <- moduleData$guidelines[moduleData$guidelines$guideline_id == guideline_id, , drop = FALSE]
      if (!nrow(g)) return()
      selected_guideline_id(guideline_id)
      rules <- load_rules(guideline_id)
      type <- infer_guideline_type(rules)
      primary <- if (nrow(rules)) rules[1, , drop = FALSE] else NULL

      updateTextInput(session, "guideline_code", value = g$guideline_code[[1]])
      updateTextInput(session, "guideline_name", value = g$guideline_name[[1]])
      updateSelectizeInput(session, "publisher", selected = as.character(g$publisher_id[[1]]))
      updateSelectizeInput(session, "series", selected = if (is.na(g$series_id[[1]])) character(0) else as.character(g$series_id[[1]]))
      updateSelectizeInput(session, "parameter_id", selected = as.character(g$parameter_id[[1]]))
      updateSelectizeInput(session, "matrix_state", selected = as.character(g$matrix_state_id[[1]]))
      updateSelectizeInput(session, "result_speciation", selected = if (is.na(g$result_speciation_id[[1]])) character(0) else as.character(g$result_speciation_id[[1]]))
      update_result_speciation_visibility(g$parameter_id[[1]])
      updateSelectizeInput(session, "sample_fraction", selected = if (is.na(g$fraction_ids[[1]])) character(0) else strsplit(g$fraction_ids[[1]], ",", fixed = TRUE)[[1]])
      updateSelectizeInput(session, "media_type", selected = if (is.na(g$media_ids[[1]])) character(0) else strsplit(g$media_ids[[1]], ",", fixed = TRUE)[[1]])
      updateSelectInput(session, "comparison_operator", selected = g$comparison_operator_code[[1]])
      update_guideline_type_choices(g$comparison_operator_code[[1]], type)
      last_comparison_operator(g$comparison_operator_code[[1]])
      updateSelectizeInput(session, "jurisdiction", selected = if (is.na(g$jurisdiction_id[[1]])) character(0) else as.character(g$jurisdiction_id[[1]]))
      updateSelectizeInput(session, "jurisdiction_level", selected = if (is.na(g$jurisdiction_level_id[[1]])) character(0) else as.character(g$jurisdiction_level_id[[1]]))
      updateSelectizeInput(session, "protection_goal", selected = if (is.na(g$protection_goal_id[[1]])) character(0) else as.character(g$protection_goal_id[[1]]))
      updateSelectizeInput(session, "exposure_duration", selected = if (is.na(g$exposure_duration_id[[1]])) character(0) else as.character(g$exposure_duration_id[[1]]))
      updateSelectizeInput(session, "averaging_period", selected = if (is.na(g$averaging_period_id[[1]])) character(0) else as.character(g$averaging_period_id[[1]]))
      updateDateInput(session, "valid_from", value = as.Date(g$valid_from[[1]]))
      updateDateInput(session, "valid_to", value = if (is.na(g$valid_to[[1]])) NA else as.Date(g$valid_to[[1]]))
      updateSelectInput(session, "review_status", selected = g$review_status[[1]])
      updateTextInput(session, "reference", value = g$reference[[1]] %||% "")
      updateTextInput(session, "source_document_title", value = g$source_document_title[[1]] %||% "")
      updateTextInput(session, "source_url", value = g$source_url[[1]] %||% "")
      updateTextInput(session, "source_page", value = g$source_page[[1]] %||% "")
      updateTextInput(session, "source_table", value = g$source_table[[1]] %||% "")
      updateTextInput(session, "source_section", value = g$source_section[[1]] %||% "")
      updateDateInput(session, "source_effective_date", value = if (is.na(g$source_effective_date[[1]])) NA else as.Date(g$source_effective_date[[1]]))
      updateDateInput(session, "source_retrieved_date", value = if (is.na(g$source_retrieved_date[[1]])) Sys.Date() else as.Date(g$source_retrieved_date[[1]]))
      updateTextAreaInput(session, "general_notes", value = g$general_notes[[1]] %||% "")
      updateTextAreaInput(session, "applicability_notes", value = g$applicability_notes[[1]] %||% "")

      updateNumericInput(session, "fixed_value", value = NA_real_)
      updateNumericInput(session, "lower_value", value = NA_real_)
      updateNumericInput(session, "upper_value", value = NA_real_)
      if (type == "constant_range") {
        updateNumericInput(session, "lower_value", value = rules$fixed_value[rules$bound_code == "lower"][[1]])
        updateNumericInput(session, "upper_value", value = rules$fixed_value[rules$bound_code == "upper"][[1]])
      } else if (!is.null(primary) && primary$algorithm_code == "constant") {
        updateNumericInput(session, "fixed_value", value = primary$fixed_value[[1]])
      }

      if (!is.null(primary)) {
        updateSelectInput(
          session, "bound_code",
          selected = if (is.na(primary$bound_code[[1]])) "upper" else primary$bound_code[[1]]
        )
        updateSelectInput(session, "formula_algorithm", selected = if (primary$algorithm_code[[1]] %in% c("linear", "log_linear", "power")) primary$algorithm_code[[1]] else "linear")
        updateNumericInput(session, "rounding_digits", value = primary$rounding_digits[[1]])
        updateSelectInput(session, "rounding_method", selected = primary$rounding_method[[1]])
        updateSelectInput(session, "missing_input_policy", selected = primary$missing_input_policy[[1]])
        updateNumericInput(session, "min_output_value", value = primary$min_output_value[[1]])
        updateNumericInput(session, "max_output_value", value = primary$max_output_value[[1]])
        updateTextInput(session, "precision_note", value = primary$precision_note[[1]] %||% "")
        updateTextAreaInput(session, "rule_note", value = primary$note[[1]] %||% "")
        updateTextInput(session, "model_code", value = primary$model_code[[1]] %||% "")
        updateTextInput(session, "model_output_code", value = primary$model_output_code[[1]] %||% "")
        updateTextInput(session, "function_schema", value = primary$function_schema[[1]] %||% "discrete")
        updateTextInput(session, "function_name", value = primary$function_name[[1]] %||% "")
        updateTextAreaInput(session, "formula_sql", value = primary$formula_sql[[1]] %||% "")
        set_rule_input_rows(load_inputs(primary$rule_id[[1]]))
        updateTextAreaInput(session, "lookup_values_text", value = format_table_text(load_lookup(primary$rule_id[[1]]), lookup_cols))
        updateTextAreaInput(session, "lookup_grid_values_text", value = format_lookup_grid_text(primary$rule_id[[1]]))
        updateTextAreaInput(session, "coefficients_text", value = format_table_text(load_coefficients(primary$rule_id[[1]]), coef_cols))
      } else {
        set_rule_input_rows(blank_rule_input_rows(1L))
      }
    }

    observeEvent(input$guidelines_table_rows_selected, {
      row <- input$guidelines_table_rows_selected
      if (is.null(row) || !length(row)) return()
      load_form(moduleData$guidelines$guideline_id[[row]])
    })
    observeEvent(input$add_guideline, {
      clear_form()
      updateTextInput(session, "guideline_name", value = "<new guideline>")
    })

    find_parameter_id <- function(pattern) {
      hits <- grep(pattern, moduleData$parameters$param_name, ignore.case = TRUE)
      if (length(hits)) moduleData$parameters$parameter_id[[hits[[1]]]] else NA_integer_
    }
    find_fraction_id <- function(pattern) {
      hits <- grep(pattern, moduleData$sample_fractions$sample_fraction, ignore.case = TRUE)
      if (length(hits)) moduleData$sample_fractions$sample_fraction_id[[hits[[1]]]] else NA_integer_
    }
    find_speciation_id <- function(pattern) {
      hits <- grep(pattern, moduleData$result_speciations$result_speciation, ignore.case = TRUE)
      if (length(hits)) moduleData$result_speciations$result_speciation_id[[hits[[1]]]] else NA_integer_
    }
    liquid_matrix_state_id <- function() {
      hits <- which(moduleData$matrix_states$matrix_state_code == "liquid")
      if (length(hits)) moduleData$matrix_states$matrix_state_id[[hits[[1]]]] else NA_integer_
    }

    observeEvent(input$fill_hardness_doc_inputs, {
      df <- data.frame(
        input_code = c("hardness_mg_l_caco3", "doc_mg_l"),
        input_name = c("Hardness as CaCO3", "Dissolved organic carbon"),
        parameter_id = c(find_parameter_id("^hardness$"), find_parameter_id("dissolved organic carbon|organic carbon|^doc$")),
        matrix_state_id = liquid_matrix_state_id(),
        sample_fraction_id = c(NA_integer_, find_fraction_id("dissolved")),
        result_speciation_id = c(find_speciation_id("caco3"), NA_integer_),
        result_type = NA_integer_,
        aggregate_method = "single",
        required = TRUE,
        allow_condition_value = FALSE,
        lower_calibrated_bound = NA_real_,
        upper_calibrated_bound = NA_real_,
        bounds_action = "flag",
        note = "",
        stringsAsFactors = FALSE
      )
      set_rule_input_rows(df)
    })
    observeEvent(input$fill_lookup_template, {
      df <- data.frame(
        input_code = "input_1",
        lower_bound = c(NA, 0, 100),
        upper_bound = c(0, 100, NA),
        lower_inclusive = c(TRUE, TRUE, TRUE),
        upper_inclusive = c(FALSE, FALSE, TRUE),
        output_value = c(NA, 1, 2),
        output_status = c("site_specific", "value", "value"),
        output_label = c("Below range", "Range 1", "Range 2"),
        sort_order = c(10, 20, 30),
        note = "",
        stringsAsFactors = FALSE
      )
      updateTextAreaInput(session, "lookup_values_text", value = format_table_text(df, lookup_cols))
    })
    observeEvent(input$fill_lookup_grid_template, {
      inputs <- collect_rule_input_rows()
      if (nrow(inputs) < 2L) {
        inputs <- data.frame(input_code = c("input_1", "input_2"), stringsAsFactors = FALSE)
      }
      codes <- vapply(seq_len(nrow(inputs)), function(i) {
        normalize_input_code(inputs$input_code[[i]], fallback = paste0("input_", i))
      }, character(1))
      cols <- lookup_grid_cols_from_codes(codes)
      df <- data.frame(
        output_value = c(1, 2, 3, 4),
        output_status = "value",
        output_label = c("low/low", "low/high", "high/low", "high/high"),
        sort_order = c(10, 20, 30, 40),
        note = "",
        stringsAsFactors = FALSE
      )
      for (code in codes) {
        df[[paste0(code, "_lower_bound")]] <- NA_real_
        df[[paste0(code, "_upper_bound")]] <- NA_real_
        df[[paste0(code, "_lower_inclusive")]] <- TRUE
        df[[paste0(code, "_upper_inclusive")]] <- FALSE
      }
      df[[paste0(codes[[1]], "_lower_bound")]] <- c(NA, NA, 100, 100)
      df[[paste0(codes[[1]], "_upper_bound")]] <- c(100, 100, NA, NA)
      df[[paste0(codes[[2]], "_lower_bound")]] <- c(NA, 5, NA, 5)
      df[[paste0(codes[[2]], "_upper_bound")]] <- c(5, NA, 5, NA)
      df[[paste0(codes[[1]], "_upper_inclusive")]] <- c(FALSE, FALSE, TRUE, TRUE)
      df[[paste0(codes[[2]], "_upper_inclusive")]] <- c(FALSE, TRUE, FALSE, TRUE)
      updateTextAreaInput(session, "lookup_grid_values_text", value = format_table_text(df, cols))
    })
    observeEvent(input$fill_coefficients_template, {
      df <- if ((input$formula_algorithm %||% "linear") %in% c("linear", "log_linear")) {
        data.frame(coefficient_name = c("intercept", "slope"), coefficient_value = c(0, 1), note = "")
      } else {
        data.frame(coefficient_name = c("factor", "exponent"), coefficient_value = c(1, 1), note = "")
      }
      updateTextAreaInput(session, "coefficients_text", value = format_table_text(df, coef_cols))
    })

    resolve_publisher_id <- function(value) {
      if (is_blank(value)) stop("Publisher is required.", call. = FALSE)
      value <- text_value(value)
      existing <- suppressWarnings(as.integer(value))
      if (!is.na(existing) && existing %in% moduleData$publishers$publisher_id) return(existing)
      DBI::dbGetQuery(
        con,
        "INSERT INTO discrete.guideline_publishers (publisher_name)
         VALUES ($1)
         ON CONFLICT (publisher_name) DO UPDATE
         SET publisher_name = EXCLUDED.publisher_name
         RETURNING publisher_id",
        params = list(value)
      )$publisher_id[[1]]
    }
    resolve_series_id <- function(value, publisher_id) {
      if (is_blank(value)) return(NA_integer_)
      value <- text_value(value)
      existing <- suppressWarnings(as.integer(value))
      if (!is.na(existing) && existing %in% moduleData$series$series_id) return(existing)
      DBI::dbGetQuery(
        con,
        "INSERT INTO discrete.guideline_series (series_name, publisher_id)
         VALUES ($1, $2)
         ON CONFLICT (series_name) DO UPDATE
         SET publisher_id = COALESCE(discrete.guideline_series.publisher_id, EXCLUDED.publisher_id)
         RETURNING series_id",
        params = list(value, publisher_id)
      )$series_id[[1]]
    }
    target_operator <- function(type) {
      switch(
        type,
        constant_upper = "lte",
        constant_lower = "gte",
        constant_range = if (identical(input$comparison_operator, "eq")) "eq" else "range",
        narrative = "narrative",
        input$comparison_operator %||% "lte"
      )
    }

    insert_rule <- function(
      guideline_id, bound_code, algorithm_code, fixed_value = NA_real_,
      formula_sql = NA_character_, model_code = NA_character_,
      model_output_code = NA_character_, function_schema = NA_character_,
      function_name = NA_character_, priority = 100L
    ) {
      DBI::dbGetQuery(
        con,
        "INSERT INTO discrete.guideline_value_rules (
           guideline_id, model_code, model_output_code, function_schema,
           function_name, bound_code, algorithm_code, fixed_value,
           formula_sql, min_output_value, max_output_value, rounding_digits,
           rounding_method, missing_input_policy, rule_priority,
           precision_note, note
         )
         VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17)
         RETURNING rule_id",
        params = list(
          guideline_id, text_or_na(model_code), text_or_na(model_output_code),
          text_or_na(function_schema), text_or_na(function_name), text_or_na(bound_code),
          algorithm_code, fixed_value, text_or_na(formula_sql),
          numeric_or_na(input$min_output_value), numeric_or_na(input$max_output_value),
          integer_or_na(input$rounding_digits), input$rounding_method %||% "none",
          input$missing_input_policy %||% "no_value",
          if (is.na(priority)) 100L else priority,
          text_or_na(input$precision_note), text_or_na(input$rule_note)
        )
      )$rule_id[[1]]
    }

    ensure_model_output <- function(model_type, publisher_id, parameter_id, matrix_state_id, operator) {
      model_code <- text_value(input$model_code)
      output_code <- text_value(input$model_output_code)
      if (!nzchar(model_code)) stop("Derivation code is required.", call. = FALSE)
      if (!nzchar(output_code)) stop("Output code is required.", call. = FALSE)
      model_name <- text_value(input$model_name)
      if (!nzchar(model_name)) model_name <- paste(text_value(input$guideline_name), "model")
      exposure_duration_id <- resolve_exposure_duration_id(input$exposure_duration)
      averaging_period_id <- resolve_averaging_period_id(input$averaging_period)
      DBI::dbExecute(
        con,
        "INSERT INTO discrete.guideline_models (
           model_code, model_name, publisher_id, model_type,
           source_document_title, source_url, description
         )
         VALUES ($1, $2, $3, $4, $5, $6, $7)
         ON CONFLICT (model_code) DO NOTHING",
        params = list(
          model_code, model_name, publisher_id, model_type,
          text_or_na(input$source_document_title), text_or_na(input$source_url),
          text_or_na(input$rule_note)
        )
      )
      DBI::dbExecute(
        con,
        "INSERT INTO discrete.guideline_model_outputs (
           model_code, output_code, output_name, comparison_operator_code,
           output_units, exposure_duration_id, averaging_period_id, note
         )
         VALUES ($1, $2, $3, $4, public.get_parameter_unit_id($5, $6), $7, $8, $9)
         ON CONFLICT (model_code, output_code) DO NOTHING",
        params = list(
          model_code, output_code, paste(text_value(input$guideline_name), output_code),
          operator, parameter_id, matrix_state_id, exposure_duration_id,
          averaging_period_id, text_or_na(input$rule_note)
        )
      )
      list(model_code = model_code, output_code = output_code)
    }

    save_rule_inputs <- function(rule_id, model_code = NA_character_, require_inputs = FALSE) {
      df <- collect_rule_input_rows()
      if (!nrow(df)) {
        if (require_inputs) stop("At least one rule input is required.", call. = FALSE)
        return(df)
      }
      for (i in seq_len(nrow(df))) {
        parameter_name <- parameter_name_for_id(df$parameter_id[i])
        if (!nzchar(trimws(df$input_name[i])) && nzchar(parameter_name)) {
          df$input_name[i] <- parameter_name
        }
        if (!nzchar(trimws(df$input_code[i]))) {
          df$input_code[i] <- normalize_input_code(
            text_default(df$input_name[i], parameter_name),
            fallback = paste0("input_", i)
          )
        }
      }
      df <- df[!is.na(df$parameter_id), , drop = FALSE]
      if (!nrow(df)) {
        if (require_inputs) stop("At least one rule input is required.", call. = FALSE)
        return(df)
      }
      duplicate_codes <- unique(df$input_code[duplicated(df$input_code)])
      if (length(duplicate_codes)) {
        stop(
          "Input codes must be unique. Duplicate: ",
          paste(duplicate_codes, collapse = ", "),
          call. = FALSE
        )
      }
      for (i in seq_len(nrow(df))) {
        if (parameter_requires_speciation(df$parameter_id[i]) &&
            is.na(integer_or_na(df$result_speciation_id[i]))) {
          stop(
            "Speciation is required for input '",
            df$input_code[i],
            "'.",
            call. = FALSE
          )
        }
        DBI::dbExecute(
          con,
          "INSERT INTO discrete.guideline_rule_inputs (
             rule_id, input_code, input_name, parameter_id, matrix_state_id,
             sample_fraction_id, result_speciation_id, result_type,
             aggregate_method, required, allow_condition_value, note
           )
           VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)",
          params = list(
            rule_id, text_value(df$input_code[i]), text_or_na(df$input_name[i]),
            integer_or_na(df$parameter_id[i]), integer_or_na(df$matrix_state_id[i]),
            integer_or_na(df$sample_fraction_id[i]),
            integer_or_na(df$result_speciation_id[i]), integer_or_na(df$result_type[i]),
            text_default(df$aggregate_method[i], "single"),
            bool_value(df$required[i], TRUE),
            bool_value(df$allow_condition_value[i], FALSE),
            text_or_na(df$note[i])
          )
        )
        if (!is.na(model_code) && nzchar(model_code)) {
          DBI::dbExecute(
            con,
            "INSERT INTO discrete.guideline_model_inputs (
               model_code, input_code, input_name, parameter_id,
               matrix_state_id, sample_fraction_id, result_speciation_id,
               input_units, lower_calibrated_bound, upper_calibrated_bound,
               bounds_action, required, sort_order, note
             )
             VALUES (
               $1, $2, $3, $4, $5, $6, $7,
               public.get_parameter_unit_id($4, $5), $8, $9, $10, $11, $12, $13
             )
             ON CONFLICT (model_code, input_code) DO UPDATE
             SET input_name = EXCLUDED.input_name,
                 parameter_id = EXCLUDED.parameter_id,
                 matrix_state_id = EXCLUDED.matrix_state_id,
                 sample_fraction_id = EXCLUDED.sample_fraction_id,
                 result_speciation_id = EXCLUDED.result_speciation_id,
                 input_units = EXCLUDED.input_units,
                 lower_calibrated_bound = EXCLUDED.lower_calibrated_bound,
                 upper_calibrated_bound = EXCLUDED.upper_calibrated_bound,
                 bounds_action = EXCLUDED.bounds_action,
                 required = EXCLUDED.required,
                 note = EXCLUDED.note",
            params = list(
              model_code, text_value(df$input_code[i]),
              text_default(df$input_name[i], text_value(df$input_code[i])),
              integer_or_na(df$parameter_id[i]), integer_or_na(df$matrix_state_id[i]),
              integer_or_na(df$sample_fraction_id[i]),
              integer_or_na(df$result_speciation_id[i]),
              numeric_or_na(df$lower_calibrated_bound[i]),
              numeric_or_na(df$upper_calibrated_bound[i]),
              text_default(df$bounds_action[i], "flag"),
              bool_value(df$required[i], TRUE), i * 10L, text_or_na(df$note[i])
            )
          )
        }
      }
      df
    }

    save_lookup_values <- function(rule_id, default_input_code) {
      df <- parse_table_text(input$lookup_values_text, lookup_cols, "Lookup values")
      if (!nrow(df)) stop("Lookup rules require at least one lookup row.", call. = FALSE)
      for (i in seq_len(nrow(df))) {
        input_code <- text_value(df$input_code[i])
        if (!nzchar(input_code)) input_code <- default_input_code
        DBI::dbExecute(
          con,
          "INSERT INTO discrete.guideline_lookup_values (
             rule_id, input_code, lower_bound, upper_bound,
             lower_inclusive, upper_inclusive, output_value,
             output_status, output_label, sort_order, note
           )
           VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)",
          params = list(
            rule_id, input_code, numeric_or_na(df$lower_bound[i]),
            numeric_or_na(df$upper_bound[i]), bool_value(df$lower_inclusive[i], TRUE),
            bool_value(df$upper_inclusive[i], TRUE), numeric_or_na(df$output_value[i]),
            text_default(df$output_status[i], "value"), text_or_na(df$output_label[i]),
            integer_or_na(df$sort_order[i]) %||% (i * 10L), text_or_na(df$note[i])
          )
        )
      }
    }

    save_lookup_grid_values <- function(rule_id, inputs) {
      if (nrow(inputs) < 2L) {
        stop("Multi-input lookup tables require at least two rule inputs.", call. = FALSE)
      }
      expected_cols <- lookup_grid_cols_for_inputs(inputs)
      df <- parse_table_text(input$lookup_grid_values_text, expected_cols, "Lookup grid cells")
      if (!nrow(df)) {
        stop("Multi-input lookup tables require at least one lookup cell.", call. = FALSE)
      }
      table_code <- normalize_code(paste(text_default(input$guideline_code, input$guideline_name), "grid"))
      table_name <- paste(text_default(input$guideline_name, "Guideline"), "lookup grid")
      lookup_table_id <- DBI::dbGetQuery(
        con,
        "INSERT INTO discrete.guideline_lookup_tables (
           rule_id, table_code, table_name, no_match_status, note
         )
         VALUES ($1, $2, $3, 'no_matching_cell', $4)
         RETURNING lookup_table_id",
        params = list(
          rule_id, table_code, table_name,
          text_or_na("Multidimensional lookup table maintained through the guideline admin module.")
        )
      )$lookup_table_id[[1]]

      dimension_ids <- setNames(integer(nrow(inputs)), inputs$input_code)
      for (i in seq_len(nrow(inputs))) {
        dimension_ids[[inputs$input_code[[i]]]] <- DBI::dbGetQuery(
          con,
          "INSERT INTO discrete.guideline_lookup_dimensions (
             lookup_table_id, rule_id, input_code, sort_order, note
           )
           VALUES ($1, $2, $3, $4, $5)
           RETURNING dimension_id",
          params = list(
            lookup_table_id, rule_id, inputs$input_code[[i]], i * 10L,
            text_or_na(inputs$note[i])
          )
        )$dimension_id[[1]]
      }

      valid_statuses <- c("value", "no_recommended_guideline", "site_specific")
      for (i in seq_len(nrow(df))) {
        output_status <- text_default(df$output_status[i], "value")
        if (!output_status %in% valid_statuses) {
          stop(
            "Lookup grid output_status must be one of: ",
            paste(valid_statuses, collapse = ", "),
            call. = FALSE
          )
        }
        output_value <- numeric_or_na(df$output_value[i])
        if (identical(output_status, "value") && is.na(output_value)) {
          stop("Lookup grid rows with output_status 'value' require output_value.", call. = FALSE)
        }
        cell_id <- DBI::dbGetQuery(
          con,
          "INSERT INTO discrete.guideline_lookup_cells (
             lookup_table_id, output_value, output_status,
             output_label, sort_order, note
           )
           VALUES ($1, $2, $3, $4, $5, $6)
           RETURNING cell_id",
          params = list(
            lookup_table_id, output_value, output_status,
            text_or_na(df$output_label[i]),
            integer_or_na(df$sort_order[i]) %||% (i * 10L),
            text_or_na(df$note[i])
          )
        )$cell_id[[1]]

        for (input_code in inputs$input_code) {
          lower_col <- paste0(input_code, "_lower_bound")
          upper_col <- paste0(input_code, "_upper_bound")
          lower_inclusive_col <- paste0(input_code, "_lower_inclusive")
          upper_inclusive_col <- paste0(input_code, "_upper_inclusive")
          DBI::dbExecute(
            con,
            "INSERT INTO discrete.guideline_lookup_cell_ranges (
               cell_id, lookup_table_id, dimension_id,
               lower_bound, upper_bound, lower_inclusive, upper_inclusive
             )
             VALUES ($1, $2, $3, $4, $5, $6, $7)",
            params = list(
              cell_id, lookup_table_id, dimension_ids[[input_code]],
              numeric_or_na(df[[lower_col]][i]), numeric_or_na(df[[upper_col]][i]),
              bool_value(df[[lower_inclusive_col]][i], TRUE),
              bool_value(df[[upper_inclusive_col]][i], FALSE)
            )
          )
        }
      }
      invisible(NULL)
    }

    save_coefficients <- function(rule_id, required = FALSE) {
      df <- parse_table_text(input$coefficients_text, coef_cols, "Coefficients")
      if (!nrow(df)) {
        if (required) stop("This rule requires coefficients.", call. = FALSE)
        return(invisible(NULL))
      }
      for (i in seq_len(nrow(df))) {
        if (!nzchar(text_value(df$coefficient_name[i]))) next
        DBI::dbExecute(
          con,
          "INSERT INTO discrete.guideline_rule_coefficients (
             rule_id, coefficient_name, coefficient_value, note
           )
           VALUES ($1, $2, $3, $4)",
          params = list(
            rule_id, text_value(df$coefficient_name[i]),
            numeric_or_na(df$coefficient_value[i]), text_or_na(df$note[i])
          )
        )
      }
    }

    save_rules <- function(guideline_id, publisher_id, parameter_id, matrix_state_id, operator) {
      type <- input$guideline_type %||% "constant_upper"
      rule_bound <- bound_for_operator(operator)
      DBI::dbExecute(
        con,
        "DELETE FROM discrete.guideline_value_rules WHERE guideline_id = $1",
        params = list(guideline_id)
      )
      if (type == "constant_upper") {
        value <- numeric_or_na(input$fixed_value)
        if (is.na(value)) stop("Fixed value is required.", call. = FALSE)
        insert_rule(guideline_id, "upper", "constant", fixed_value = value)
      } else if (type == "constant_lower") {
        value <- numeric_or_na(input$fixed_value)
        if (is.na(value)) stop("Fixed value is required.", call. = FALSE)
        insert_rule(guideline_id, "lower", "constant", fixed_value = value)
      } else if (type == "constant_range") {
        lower <- numeric_or_na(input$lower_value)
        upper <- numeric_or_na(input$upper_value)
        if (is.na(lower) || is.na(upper)) stop("Lower and upper values are required.", call. = FALSE)
        insert_rule(guideline_id, "lower", "constant", fixed_value = lower, priority = 10L)
        insert_rule(guideline_id, "upper", "constant", fixed_value = upper, priority = 20L)
      } else if (type == "narrative") {
        insert_rule(guideline_id, NA_character_, "narrative")
      } else if (type == "sql_scalar") {
        if (!nzchar(text_value(input$formula_sql))) stop("SQL scalar text is required.", call. = FALSE)
        rule_id <- insert_rule(guideline_id, rule_bound, "sql_scalar", formula_sql = input$formula_sql)
        save_rule_inputs(rule_id, require_inputs = FALSE)
      } else if (type == "single_input_formula") {
        rule_id <- insert_rule(guideline_id, rule_bound, input$formula_algorithm %||% "linear")
        save_rule_inputs(rule_id, require_inputs = TRUE)
        save_coefficients(rule_id, required = TRUE)
      } else if (type == "lookup_range") {
        rule_id <- insert_rule(guideline_id, rule_bound, "lookup_range")
        inputs <- save_rule_inputs(rule_id, require_inputs = TRUE)
        if (nrow(inputs) != 1) stop("Lookup rules require exactly one input.", call. = FALSE)
        save_lookup_values(rule_id, inputs$input_code[[1]])
      } else if (type == "lookup_grid") {
        rule_id <- insert_rule(guideline_id, rule_bound, "lookup_grid")
        inputs <- save_rule_inputs(rule_id, require_inputs = TRUE)
        if (nrow(inputs) < 2L) {
          stop("Multi-input lookup tables require at least two inputs.", call. = FALSE)
        }
        save_lookup_grid_values(rule_id, inputs)
      } else if (type == "db_function") {
        model <- ensure_model_output("database_function", publisher_id, parameter_id, matrix_state_id, operator)
        rule_id <- insert_rule(
          guideline_id, rule_bound, "db_function",
          model_code = model$model_code, model_output_code = model$output_code,
          function_schema = input$function_schema, function_name = input$function_name
        )
        save_rule_inputs(rule_id, model_code = model$model_code, require_inputs = TRUE)
      } else if (type == "model_result_cache") {
        model <- ensure_model_output("external_software", publisher_id, parameter_id, matrix_state_id, operator)
        rule_id <- insert_rule(
          guideline_id, rule_bound, "model_result_cache",
          model_code = model$model_code, model_output_code = model$output_code
        )
        save_rule_inputs(rule_id, model_code = model$model_code, require_inputs = TRUE)
      } else {
        stop("Unsupported guideline type: ", type, call. = FALSE)
      }
    }

    observeEvent(input$save_guideline, {
      err <- NULL
      saved_id <- NA_integer_
      tryCatch(
        {
          parameter_id <- integer_or_na(input$parameter_id)
          matrix_state_id <- integer_or_na(input$matrix_state)
          if (is.na(parameter_id)) stop("Parameter is required.", call. = FALSE)
          if (is.na(matrix_state_id)) stop("Matrix state is required.", call. = FALSE)
          result_speciation_id <- if (parameter_requires_speciation(parameter_id)) {
            integer_or_na(input$result_speciation)
          } else {
            NA_integer_
          }
          if (parameter_requires_speciation(parameter_id) && is.na(result_speciation_id)) {
            stop("Result speciation is required for the selected parameter.", call. = FALSE)
          }
          if (!nzchar(text_value(input$guideline_name))) stop("Guideline name is required.", call. = FALSE)
          operator <- target_operator(input$guideline_type)
          jurisdiction_id <- resolve_jurisdiction_id(input$jurisdiction)
          jurisdiction_level_id <- resolve_jurisdiction_level_id(input$jurisdiction_level)
          protection_goal_id <- resolve_protection_goal_id(input$protection_goal)
          exposure_duration_id <- resolve_exposure_duration_id(input$exposure_duration)
          averaging_period_id <- resolve_averaging_period_id(input$averaging_period)
          guideline_code <- text_value(input$guideline_code)
          if (!nzchar(guideline_code)) guideline_code <- normalize_code(input$guideline_name)
          guideline_id <- selected_guideline_id()

          DBI::dbWithTransaction(con, {
            publisher_id <- resolve_publisher_id(input$publisher)
            series_id <- resolve_series_id(input$series, publisher_id)

            if (is.na(guideline_id)) {
              saved_id <- DBI::dbGetQuery(
                con,
                "INSERT INTO discrete.guidelines (
                   guideline_code, guideline_name, publisher_id, series_id,
                   reference, general_notes, applicability_notes,
                   parameter_id, matrix_state_id, result_speciation_id,
                   comparison_operator_code, jurisdiction_id, jurisdiction_level_id,
                   protection_goal_id, exposure_duration_id, averaging_period_id,
                   source_document_title, source_url, source_page,
                   source_table, source_section, source_effective_date,
                   source_retrieved_date, valid_from, valid_to, review_status
                 )
                 VALUES (
                   $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13,
                   $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24,
                   $25, $26
                 )
                 RETURNING guideline_id",
                params = list(
                  guideline_code, text_value(input$guideline_name), publisher_id,
                  series_id, text_or_na(input$reference), text_or_na(input$general_notes),
                  text_or_na(input$applicability_notes), parameter_id, matrix_state_id,
                  result_speciation_id, operator,
                  jurisdiction_id, jurisdiction_level_id, protection_goal_id,
                  exposure_duration_id, averaging_period_id, text_or_na(input$source_document_title),
                  text_or_na(input$source_url), text_or_na(input$source_page),
                  text_or_na(input$source_table), text_or_na(input$source_section),
                  date_or_na(input$source_effective_date),
                  date_or_na(input$source_retrieved_date), date_or_na(input$valid_from),
                  date_or_na(input$valid_to), input$review_status %||% "draft"
                )
              )$guideline_id[[1]]
            } else {
              saved_id <- guideline_id
              DBI::dbExecute(
                con,
                "UPDATE discrete.guidelines
                 SET guideline_code = $1, guideline_name = $2,
                     publisher_id = $3, series_id = $4, reference = $5,
                     general_notes = $6, applicability_notes = $7,
                     parameter_id = $8, matrix_state_id = $9,
                     result_speciation_id = $10, comparison_operator_code = $11,
                     jurisdiction_id = $12, jurisdiction_level_id = $13,
                     protection_goal_id = $14, exposure_duration_id = $15,
                     averaging_period_id = $16, source_document_title = $17,
                     source_url = $18, source_page = $19, source_table = $20,
                     source_section = $21, source_effective_date = $22,
                     source_retrieved_date = $23, valid_from = $24,
                     valid_to = $25, review_status = $26
                 WHERE guideline_id = $27",
                params = list(
                  guideline_code, text_value(input$guideline_name), publisher_id,
                  series_id, text_or_na(input$reference), text_or_na(input$general_notes),
                  text_or_na(input$applicability_notes), parameter_id, matrix_state_id,
                  result_speciation_id, operator,
                  jurisdiction_id, jurisdiction_level_id, protection_goal_id,
                  exposure_duration_id, averaging_period_id, text_or_na(input$source_document_title),
                  text_or_na(input$source_url), text_or_na(input$source_page),
                  text_or_na(input$source_table), text_or_na(input$source_section),
                  date_or_na(input$source_effective_date),
                  date_or_na(input$source_retrieved_date), date_or_na(input$valid_from),
                  date_or_na(input$valid_to), input$review_status %||% "draft",
                  saved_id
                )
              )
            }

            DBI::dbExecute(con, "DELETE FROM discrete.guidelines_fractions WHERE guideline_id = $1", params = list(saved_id))
            for (fraction_id in parse_id_vector(input$sample_fraction)) {
              DBI::dbExecute(con, "INSERT INTO discrete.guidelines_fractions (guideline_id, fraction_id) VALUES ($1, $2)", params = list(saved_id, fraction_id))
            }
            DBI::dbExecute(con, "DELETE FROM discrete.guidelines_media_types WHERE guideline_id = $1", params = list(saved_id))
            for (media_id in parse_id_vector(input$media_type)) {
              DBI::dbExecute(con, "INSERT INTO discrete.guidelines_media_types (guideline_id, media_id) VALUES ($1, $2)", params = list(saved_id, media_id))
            }
            save_rules(saved_id, publisher_id, parameter_id, matrix_state_id, operator)
          })
        },
        error = function(e) {
          err <<- conditionMessage(e)
          message("Guideline save failed: ", err)
        }
      )
      if (!is.null(err)) {
        showModal(modalDialog(
          title = "Guideline not saved", err, easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return()
      }
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
      if (is.na(guideline_id)) return()
      g <- moduleData$guidelines[moduleData$guidelines$guideline_id == guideline_id, , drop = FALSE]
      showModal(modalDialog(
        title = "Delete guideline",
        paste("Delete", g$guideline_code[[1]], "?"),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_delete_guideline"), "Delete", class = "btn-danger")
        )
      ))
    })
    observeEvent(input$confirm_delete_guideline, {
      removeModal()
      guideline_id <- selected_guideline_id()
      if (is.na(guideline_id)) return()
      tryCatch(
        {
          DBI::dbExecute(
            con,
            "DELETE FROM discrete.guidelines WHERE guideline_id = $1",
            params = list(guideline_id)
          )
          load_reference_data()
          update_choices()
          clear_form()
        },
        error = function(e) {
          showModal(modalDialog(
            title = "Delete failed", conditionMessage(e), easyClose = TRUE,
            footer = modalButton("Close")
          ))
        }
      )
    })

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
      showModal(modalDialog(
        title = "Test saved guideline",
        numericInput(ns("test_result_id"), "Result ID", value = NA_integer_, min = 1),
        actionButton(ns("run_guideline_test"), "Run", class = "btn-primary"),
        br(), br(),
        DT::DTOutput(ns("test_guideline_modal_table")),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "l"
      ))
    })
    observeEvent(input$run_guideline_test, {
      guideline_id <- selected_guideline_id()
      result_id <- integer_or_na(input$test_result_id)
      if (is.na(guideline_id) || is.na(result_id)) return()
      result <- tryCatch(
        DBI::dbGetQuery(
          con,
          "SELECT guideline_code, guideline_name, result_value,
                  lower_guideline_value, upper_guideline_value,
                  output_status, comparison_status, derivation_inputs, message
           FROM discrete.applicable_guidelines_for_result($1, CURRENT_DATE, TRUE)
           WHERE guideline_id = $2",
          params = list(result_id, guideline_id)
        ),
        error = function(e) data.frame(error = conditionMessage(e), stringsAsFactors = FALSE)
      )
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
