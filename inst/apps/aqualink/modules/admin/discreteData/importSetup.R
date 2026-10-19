# Standalone administration of the Patch 65 import catalogue. Uses the existing
# source/profile mapping scopes and immutable published revisions; no additional
# storage is needed. Inspection uses SELECT privileges. Every write stages a
# draft through AquaCache, and publication rechecks the reviewed scope under the
# same advisory lock as package writers. Shared revision state invalidates open
# workbook previews so their saved provenance agrees with the imported values.
# These helpers keep matching criteria and publication review independent of
# Shiny inputs, so workbook and package-download mappings use the same path.
importSetup_match_values <- function(text) {
  value <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e)
    stop("Enter a JSON object of source field names and text values.", call. = FALSE))
  if (!is.list(value) || !length(value) || is.null(names(value)) ||
      any(!nzchar(trimws(names(value)))) || anyDuplicated(names(value)) ||
      !all(vapply(value, function(x) is.character(x) && length(x) == 1L && !is.na(x), logical(1)))) {
    stop("Source criteria must be a non-empty JSON object with unique field names and text values.", call. = FALSE)
  }
  # These names are the target columns consumed by the existing upsert API.
  reserved <- c("parameter_id","parameter","param_name","result_type","result_type_id",
    "sample_fraction_id","sample_fraction","result_value_type","result_value_type_id",
    "reporting_basis_id","reporting_basis","reporting_basis_code","matrix_state_id","matrix_state",
    "result_speciation_id","result_speciation","result_speciation_code","result_target_id",
    "result_target","result_target_code","endpoint_mode","endpoint_ph","analysis_conditions",
    "conversion","result_offset","priority","active","note","ignore")
  if (any(names(value) %in% reserved)) stop("A source criterion uses a reserved AquaCache target field name.", call. = FALSE)
  if (!any(nzchar(trimws(unlist(value))))) stop("Enter at least one non-empty source criterion.", call. = FALSE)
  value
}

importSetup_changes <- function(rows, labels = list()) {
  fields <- c(parameter_id="Parameter", result_type="Result type", sample_fraction_id="Sample fraction",
    result_value_type="Value type", reporting_basis_id="Reporting basis", result_speciation_id="Chemical form",
    result_target_id="Analytical target", endpoint_mode="Endpoint method", endpoint_ph="Endpoint pH",
    analysis_conditions="Analysis conditions", matrix_state_id="Matrix", location_id="Location",
    sub_location_id="Sub-location", active="Active", conversion="Conversion multiplier", result_offset="Offset",
    priority="Priority", note="Note", result_action="Result handling", result_condition_id="Result condition")
  before <- rows[rows$status == "published", , drop=FALSE]
  after <- rows[rows$status == "draft", , drop=FALSE]
  old_key <- paste(before$mapping_type,before$source_match,sep="\r")
  new_key <- paste(after$mapping_type,after$source_match,sep="\r")
  format_value <- function(value,field) {
    if (is.null(value)) return("")
    if (is.list(value)) return(as.character(jsonlite::toJSON(value,auto_unbox=TRUE,null="null")))
    display <- as.character(value)
    lookup <- labels[[field]]
    if (!is.null(lookup) && length(display)==1L && display %in% names(lookup)) return(paste0(unname(lookup[[display]]), " [", display, "]"))
    paste(display,collapse=", ")
  }
  changes <- list()
  for (key in union(old_key,new_key)) {
    old <- match(key,old_key); new <- match(key,new_key)
    old_def <- if (!is.na(old)) jsonlite::fromJSON(before$definition[[old]],simplifyVector=FALSE) else list()
    new_def <- if (!is.na(new)) jsonlite::fromJSON(after$definition[[new]],simplifyVector=FALSE) else list()
    row <- if (!is.na(new)) after[new,,drop=FALSE] else before[old,,drop=FALSE]
    for (field in union(names(old_def),names(new_def))) {
      old_value <- format_value(old_def[[field]],field)
      new_value <- format_value(new_def[[field]],field)
      if (identical(old_def[[field]],new_def[[field]])) next
      changes[[length(changes)+1L]] <- data.frame(Type=row$mapping_type,Criteria=row$source_match,
        Change=if (is.na(old)) "New mapping" else if (is.na(new)) "Removed mapping" else "Changed",
        Field=if (field %in% names(fields)) unname(fields[[field]]) else gsub("_"," ",field),
        Before=old_value,After=new_value,check.names=FALSE)
    }
  }
  if (!length(changes)) return(data.frame(Type=character(),Criteria=character(),Change=character(),
    Field=character(),Before=character(),After=character()))
  as.data.frame(data.table::rbindlist(changes))
}

importSetupUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    tags$h3("Import setup"),
    uiOutput(ns("banner")),
    actionButton(ns("refresh_setup"), "Refresh saved setup"),
    tabsetPanel(id = ns("setup_tabs"),
      tabPanel("Mapping overview", value = "overview",
        helpText("Review saved mappings across all sources and formats. Published issues can cause results to be omitted; draft issues prevent publication. Retired revisions are preserved as history."),
        uiOutput(ns("review_summary")),
        fluidRow(column(4, selectizeInput(ns("review_source"), "Source", choices = c("All sources" = ""))),
          column(4, selectInput(ns("review_state"), "Revision", c("Current published and draft"="current", "Published"="published", "Draft"="draft", "All revisions, including retired"="history"))),
          column(4, selectInput(ns("review_kind"), "Mapping type", c("All types"="", "Parameter"="parameter", "Location"="location", "Result flag"="result_flag")))),
        checkboxInput(ns("needs_attention"), "Needs attention only", TRUE),
        checkboxInput(ns("review_inactive"), "Include inactive mappings", FALSE),
        DT::DTOutput(ns("mapping_review")),
        actionButton(ns("open_review_mapping"), "Edit selected mapping"),
        helpText("Select a row above to open its saved definition in the editor. Saving creates or updates a draft; it does not change the published mapping.")),
        tabPanel(
          "Formats and mappings",
          value = "editor",
          helpText(
            "Manage reusable sources, workbook layouts and mapping revisions. Source-wide mappings are used by AquaCache download functions; workbook mappings override them for that format. Save incomplete work as a draft, then review and publish a complete revision. Reparse any open workbook after changing its setup."
          ),
          uiOutput(ns("editing_mapping")),
          fluidRow(column(6, selectizeInput(ns("manage_source"), "Source", choices = NULL)),
            column(6, actionButton(ns("new_source"), "Create source"))),
          fluidRow(
            column(
              6,
              selectizeInput(
                ns("manage_import_profile"),
                "Scope",
                choices = NULL,
                options = list(placeholder = "Source-wide mappings")
              )
            ),
            column(
              4,
              tags$div(
                style = "padding-top: 25px;",
                actionButton(
                  ns("new_import_profile"),
                  "Create new format"
                )
              )
            ),
            column(
              4,
              tags$div(
                style = "padding-top: 25px;",
                actionButton(
                  ns("copy_import_profile"),
                  "Copy selected format"
                )
              )
            ),
            column(
              4,
              tags$div(
                style = "padding-top: 25px;",
                actionButton(
                  ns("edit_import_profile"),
                  "Edit selected format"
                )
              )
            )
          ),
          uiOutput(ns("manage_import_profile_status")),
          tags$hr(),
          fluidRow(
            column(
              4,
              selectInput(
                ns("manage_mapping_type"),
                "Mapping type",
                choices = c(
                  "Parameter" = "parameter",
                  "Location" = "location",
                  "Result flag" = "result_flag"
                )
              )
            ),
            column(
              4,
              checkboxInput(
                ns("manage_show_inactive"),
                "Include inactive mappings",
                value = TRUE
              )
            ),
            column(
              4,
              tags$div(
                style = "padding-top: 25px;",
                actionButton(ns("manage_new_mapping"), "Create mapping")
              )
            )
          ),
          DT::DTOutput(ns("manage_mapping_table")),
          uiOutput(ns("manage_mapping_permissions")),
          uiOutput(ns("manage_mapping_completeness")),
          tags$p(
            class = "text-muted small",
            "Save mappings as drafts while repairing a set. Publish each scope once its active parameter mappings are complete."
          ),
          selectInput(
            ns("manage_publish_scope"),
            "Draft scope to publish",
            choices = c(
              "This workbook format" = "profile",
              "Source-wide / package downloads" = "source"
            )
          ),
          actionButton(ns("manage_publish_mappings"), "Review draft for publication"),
          conditionalPanel(
            condition = "input.manage_mapping_type == 'parameter'",
            ns = ns,
            wellPanel(
              tags$h5("Parameter mapping"),
              textAreaInput(ns("manage_source_match"), "Source matching criteria (JSON)", rows = 3,
                value = '{"parameter_code":"", "unit":""}'),
              helpText('Keep the source field names unchanged. Workbook: {"parameter_code":"Al-T","unit":"mg/L"}; EQWin: {"ParamCode":"Al-T","input_unit":"mg/L"}; ECCC: {"input_param":"ALUMINUM","input_unit":"mg/L"}.'),
              selectInput(ns("manage_mapping_scope"), "Applies to", choices = c(
                "Source-wide / package downloads" = "source", "This workbook format" = "profile")),
              fluidRow(
                column(
                  4,
                  selectizeInput(
                    ns("manage_parameter_id"),
                    "AquaCache parameter",
                    choices = NULL
                  )
                ),
                column(
                  4,
                  selectizeInput(
                    ns("manage_result_type"),
                    "Result type",
                    choices = NULL
                  )
                ),
                column(
                  4,
                  selectizeInput(
                    ns("manage_matrix_state"),
                    "Matrix state",
                    choices = NULL
                  )
                )
              ),
              fluidRow(
                column(
                  3,
                  selectizeInput(
                    ns("manage_sample_fraction"),
                    "Sample fraction",
                    choices = NULL
                  )
                ),
                column(
                  3,
                  selectizeInput(
                    ns("manage_value_type"),
                    "Result value type",
                    choices = NULL
                  )
                ),
                column(
                  3,
                  selectizeInput(
                    ns("manage_descriptor"),
                    "Reporting basis",
                    choices = NULL
                  )
                ),
                column(
                  3,
                  numericInput(
                    ns("manage_priority"),
                    "Priority",
                    value = 50,
                    min = 1,
                    step = 1
                  )
                )
              ),
              uiOutput(ns("manage_chemistry")),
              helpText(
                "Each analytical dimension is independently required by the selected parameter."
              ),
              fluidRow(
                column(
                  3,
                  numericInput(
                    ns("manage_conversion"),
                    "Conversion multiplier",
                    value = 1
                  )
                ),
                column(
                  3,
                  numericInput(
                    ns("manage_result_offset"),
                    "Result offset",
                    value = 0
                  )
                ),
                column(
                  3,
                  checkboxInput(ns("manage_active"), "Active", value = TRUE)
                )
              ),
              textAreaInput(
                ns("manage_mapping_note"),
                "Mapping note",
                rows = 2
              ),
              actionButton(
                ns("manage_save_parameter_mapping"),
                "Save parameter mapping as draft"
              )
            )
          ),
          conditionalPanel(
            condition = "input.manage_mapping_type == 'location'",
            ns = ns,
            wellPanel(
              tags$h5("Location mapping"),
              fluidRow(
                column(
                  4,
                  textInput(ns("manage_location_code"), "Source location code")
                ),
                column(
                  4,
                  textInput(
                    ns("manage_location_name"),
                    "Source location name (optional)"
                  )
                ),
                column(
                  4,
                  selectInput(
                    ns("manage_location_scope"),
                    "Applies to",
                    choices = c(
                      "This workbook format" = "profile",
                      "Source-wide / package downloads" = "source"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  6,
                  selectizeInput(
                    ns("manage_location_id"),
                    "AquaCache location",
                    choices = NULL
                  )
                ),
                column(
                  6,
                  selectizeInput(
                    ns("manage_sub_location_id"),
                    "Sub-location (optional)",
                    choices = NULL
                  )
                )
              ),
              fluidRow(
                column(
                  3,
                  numericInput(
                    ns("manage_location_priority"),
                    "Priority",
                    value = 50,
                    min = 1,
                    step = 1
                  )
                ),
                column(
                  3,
                  checkboxInput(
                    ns("manage_location_active"),
                    "Active",
                    value = TRUE
                  )
                )
              ),
              textAreaInput(
                ns("manage_location_note"),
                "Mapping note",
                rows = 2
              ),
              actionButton(
                ns("manage_save_location_mapping"),
                "Save location mapping as draft"
              )
            )
          ),
          conditionalPanel(
            condition = "input.manage_mapping_type == 'result_flag'",
            ns = ns,
            wellPanel(
              tags$h5("Result-flag mapping"),
              fluidRow(
                column(
                  4,
                  textInput(
                    ns("manage_flag_column"),
                    "Source flag column (optional)"
                  )
                ),
                column(
                  4,
                  textInput(ns("manage_flag_value"), "Source flag value")
                ),
                column(
                  4,
                  selectInput(
                    ns("manage_flag_scope"),
                    "Applies to",
                    choices = c(
                      "This workbook format" = "profile",
                      "Source-wide / package downloads" = "source"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  4,
                  selectInput(
                    ns("manage_flag_action"),
                    "When this flag appears",
                    choices = c(
                      "Keep the result" = "keep_result",
                      "Keep flag and clear numeric result" = "set_result_null",
                      "Skip this result during import" = "skip_result",
                      "Reject the result row" = "reject_row",
                      "Add a note only" = "note_only"
                    )
                  )
                ),
                column(
                  4,
                  selectInput(
                    ns("manage_flag_condition"),
                    "Result condition (optional)",
                    choices = NULL
                  )
                ),
                column(
                  4,
                  selectInput(
                    ns("manage_flag_threshold_source"),
                    "Condition value from",
                    choices = c(
                      "None" = "none",
                      "Result" = "result",
                      "Method detection limit" = "method_detection_limit",
                      "Reporting detection limit" = "reporting_detection_limit",
                      "Enter a value" = "literal"
                    )
                  )
                )
              ),
              fluidRow(
                column(
                  4,
                  numericInput(
                    ns("manage_flag_threshold"),
                    "Literal condition value",
                    value = NA_real_
                  )
                ),
                column(
                  4,
                  numericInput(
                    ns("manage_flag_priority"),
                    "Priority",
                    value = 50,
                    min = 1,
                    step = 1
                  )
                ),
                column(
                  4,
                  checkboxInput(
                    ns("manage_flag_active"),
                    "Active",
                    value = TRUE
                  )
                )
              ),
              textInput(
                ns("manage_flag_note_template"),
                "Optional note to add"
              ),
              textAreaInput(ns("manage_flag_note"), "Mapping note", rows = 2),
              actionButton(
                ns("manage_save_flag_mapping"),
                "Save result-flag mapping as draft"
              )
            )
          )
        )
    )
  )
}

importSetup <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- session$userData$AquaCache
    mapping_revision <- reactiveVal(0L)
    import_profiles <- reactiveVal(addDiscData_empty_profiles())
    profile_load_error <- reactiveVal(NULL)
    sources <- reactiveVal(data.frame())
    pending_open <- reactiveVal(NULL)
    opened_review_mapping <- reactiveVal(NULL)
    pending_profile_selection <- reactiveVal(NULL)
    pending_publication <- reactiveVal(NULL)
    if (is.null(session$userData$import_setup_revision)) session$userData$import_setup_revision <- reactiveVal(0L)
    shared_revision <- session$userData$import_setup_revision
    bump_revision <- function() {
      mapping_revision(mapping_revision() + 1L)
      shared_revision(isolate(shared_revision()) + 1L)
    }
    output$banner <- renderUI(application_notifications_ui(ns=ns, lang=language$language, con=con, module_id="importSetup"))
    new_profile_template <- reactiveVal(NULL)
    new_profile_mode <- reactiveVal("create")
    manage_mapping_selected <- reactiveVal(NULL)
    manager_selection_from_review <- reactiveVal(NULL)
    manage_mapping_error <- reactiveVal(NULL)
    profile_permissions <- tryCatch(
      DBI::dbGetQuery(
        con,
        "SELECT
           has_table_privilege(
             current_user,
             'discrete.import_profiles',
             'INSERT'
           ) AND has_table_privilege(
             current_user,
             'discrete.import_profiles',
             'UPDATE'
           ) AS can_write_profiles,
           has_table_privilege(
             current_user,
             'discrete.import_sources',
             'INSERT'
           ) AND has_table_privilege(
             current_user,
             'discrete.import_sources',
             'UPDATE'
           ) AS can_write_sources,
           has_sequence_privilege(
             current_user,
             pg_get_serial_sequence(
               'discrete.import_profiles',
               'import_profile_id'
             ),
             'USAGE'
           ) AS can_use_profile_sequence,
           has_sequence_privilege(
             current_user,
             pg_get_serial_sequence(
               'discrete.import_sources',
               'import_source_id'
             ),
             'USAGE'
           ) AS can_use_source_sequence;"
      ),
      error = function(e) {
        data.frame(
          can_write_profiles = FALSE,
          can_write_sources = FALSE,
          can_use_profile_sequence = FALSE,
          can_use_source_sequence = FALSE
        )
      }
    )
    can_manage_profiles <- isTRUE(profile_permissions$can_write_profiles[[
      1
    ]]) &&
      isTRUE(profile_permissions$can_write_sources[[1]]) &&
      isTRUE(profile_permissions$can_use_profile_sequence[[1]]) &&
      isTRUE(profile_permissions$can_use_source_sequence[[1]])
    mapping_permissions <- tryCatch(
      DBI::dbGetQuery(
        con,
        "SELECT
           has_table_privilege(current_user, 'discrete.import_sources', 'INSERT')
             AND has_table_privilege(current_user, 'discrete.import_sources', 'UPDATE')
             AND has_table_privilege(current_user, 'discrete.import_mapping_sets', 'INSERT')
             AND has_table_privilege(current_user, 'discrete.import_mapping_sets', 'UPDATE')
             AND has_table_privilege(current_user, 'discrete.import_parameter_mappings', 'INSERT')
             AND has_table_privilege(current_user, 'discrete.import_parameter_mappings', 'UPDATE')
             AND has_table_privilege(current_user, 'discrete.import_location_mappings', 'INSERT')
             AND has_table_privilege(current_user, 'discrete.import_location_mappings', 'UPDATE')
             AND has_table_privilege(current_user, 'discrete.import_result_flag_mappings', 'INSERT')
             AND has_table_privilege(current_user, 'discrete.import_result_flag_mappings', 'UPDATE')
             AND has_sequence_privilege(
               current_user,
               pg_get_serial_sequence('discrete.import_mapping_sets', 'import_mapping_set_id'),
               'USAGE'
             )
             AND has_sequence_privilege(
               current_user,
               pg_get_serial_sequence('discrete.import_sources', 'import_source_id'),
               'USAGE'
             ) AS can_write_mappings;"
      ),
      error = function(e) data.frame(can_write_mappings = FALSE)
    )
    can_manage_mappings <- isTRUE(mapping_permissions$can_write_mappings[[1]])
    clear_manager_mapping_selection <- function() {
      manage_mapping_selected(NULL)
      proxy <- DT::dataTableProxy("manage_mapping_table", session = session)
      try(DT::selectRows(proxy, NULL), silent = TRUE)
      invisible(NULL)
    }
    select_manager_mapping_row <- function(index, row) {
      index <- addDiscData_int(index)
      if (is.na(index) || index < 1L || is.null(row) || !nrow(row)) {
        return(invisible(NULL))
      }
      table_id <- as.character(jsonlite::toJSON(
        session$ns("manage_mapping_table"),
        auto_unbox = TRUE
      ))
      selection_input_id <- as.character(jsonlite::toJSON(
        session$ns("manager_mapping_table_selection"),
        auto_unbox = TRUE
      ))
      scope <- if (isTRUE(as.logical(row$profile_specific[[1]]))) {
        "This workbook format"
      } else {
        "All formats for this source"
      }
      expected_values <- switch(
        input$manage_mapping_type,
        parameter = c(scope, row$source_match[[1]], ""),
        location = c(
          scope,
          manager_text(row$source_location_code[[1]]),
          manager_text(row$source_location_name[[1]])
        ),
        result_flag = c(
          scope,
          if (addDiscData_present(row$source_flag_column[[1]])) {
            row$source_flag_column[[1]]
          } else {
            "Any column"
          },
          manager_text(row$source_flag_value[[1]])
        )
      )
      expected_values <- as.character(expected_values)
      expected_values[is.na(expected_values)] <- ""
      expected <- as.character(jsonlite::toJSON(
        expected_values,
        auto_unbox = FALSE
      ))
      script <- sprintf(
        "(function() {
          var outputId = %s;
          var expected = %s;
          var rowIndex = %d;
          var selectionInputId = %s;
          var attempts = 0;
          function selectWhenReady() {
            var output = document.getElementById(outputId);
            var element = output && (output.matches('table') ? output : output.querySelector('table[id]'));
            if (element && $.fn.dataTable.isDataTable(element)) {
              var table = $(element).DataTable();
              var row = table.row(rowIndex);
              var data = row.data();
              if (row.any() && data && String(data[0]) === expected[0] &&
                  String(data[1]) === expected[1] &&
                  (!expected[2] || String(data[2]) === expected[2])) {
                table.search('').columns().search('').draw();
                var ordered = table.rows({order: 'applied', search: 'applied'}).indexes().toArray();
                var position = ordered.indexOf(row.index());
                if (position >= 0) {
                  table.page(Math.floor(position / table.page.len())).draw('page');
                  Shiny.setInputValue(selectionInputId, rowIndex + 1, {priority: 'event'});
                  return;
                }
              }
            }
            if (++attempts < 50) window.setTimeout(selectWhenReady, 100);
          }
          selectWhenReady();
        })();",
        table_id,
        expected,
        as.integer(index) - 1L,
        selection_input_id
      )
      shinyjs::runjs(script)
      invisible(index)
    }
    # Catalogue choices do not change when a mapping draft is saved. Reloading
    # them would reset the editor and discard an asynchronously loaded selection.
    params <- reactive({
      dbGetQueryDT(
        con,
        "SELECT p.parameter_id,
                p.param_name,
                p.sample_fraction,
                p.reporting_basis, p.result_speciation, p.result_target, p.endpoint, to_jsonb(p.analysis_conditions_required_keys) AS analysis_conditions_required_keys, p.analysis_conditions_required_keys_by_matrix,
                ul.unit_name AS unit_liquid,
                us.unit_name AS unit_solid,
                ug.unit_name AS unit_gas,
                una.unit_name AS unit_na
           FROM public.parameters p
           LEFT JOIN public.units ul ON ul.unit_id = p.units_liquid
           LEFT JOIN public.units us ON us.unit_id = p.units_solid
           LEFT JOIN public.units ug ON ug.unit_id = p.units_gas
           LEFT JOIN public.units una ON una.unit_id = p.units_na
          ORDER BY p.param_name"
      )
    })
    result_types <- DBI::dbGetQuery(
      con,
      "SELECT result_type_id, result_type
         FROM discrete.result_types
        ORDER BY result_type"
    )
    result_conditions <- DBI::dbGetQuery(
      con,
      "SELECT result_condition_id, result_condition
         FROM discrete.result_conditions
        ORDER BY result_condition"
    )
    matrix_states <- DBI::dbGetQuery(
      con,
      "SELECT matrix_state_id, matrix_state_code, matrix_state_name
         FROM public.matrix_states
        ORDER BY matrix_state_id"
    )
    sample_fractions <- DBI::dbGetQuery(
      con,
      "SELECT sample_fraction_id, sample_fraction
         FROM discrete.sample_fractions
        ORDER BY sample_fraction"
    )
    result_value_types <- DBI::dbGetQuery(
      con,
      "SELECT result_value_type_id, result_value_type
         FROM discrete.result_value_types
        ORDER BY result_value_type"
    )
    reporting_bases <- DBI::dbGetQuery(
      con,
      "SELECT reporting_basis_id, reporting_basis
         FROM discrete.reporting_bases
        ORDER BY reporting_basis"
    )
    chemistry_catalogues <- YGwater:::chemistry_metadata_catalogues(con)
    output$manage_chemistry <- renderUI({
      YGwater:::chemistry_metadata_ui(ns, "manage_chem_", chemistry_catalogues)
    })
    laboratories <- DBI::dbGetQuery(
      con,
      "SELECT lab_id, lab_name
         FROM discrete.laboratories
        ORDER BY lab_name"
    )
    read_locations <- function() {
      DBI::dbGetQuery(
        con,
        "SELECT location_id, location_code, name, alias, latitude, longitude
           FROM public.locations
          ORDER BY name, location_code"
      )
    }
    locations <- reactiveVal(read_locations())
    sub_locations <- DBI::dbGetQuery(
      con,
      "SELECT sub_location_id, sub_location_name, location_id
         FROM public.sub_locations
        ORDER BY sub_location_name"
    )
    media <- DBI::dbGetQuery(
      con,
      "SELECT media_id, media_type FROM public.media_types ORDER BY media_type"
    )
    collection_methods <- DBI::dbGetQuery(
      con,
      "SELECT collection_method_id, collection_method FROM discrete.collection_methods ORDER BY collection_method"
    )
    sample_types <- DBI::dbGetQuery(
      con,
      "SELECT sample_type_id, sample_type, requires_location, requires_sample_group
       FROM discrete.sample_types
       ORDER BY sample_type"
    )
    organizations <- DBI::dbGetQuery(
      con,
      "SELECT organization_id, name
         FROM public.organizations
        ORDER BY name"
    )

    # Catalogue IDs vary between AquaCache instances. Resolve the suggested
    # scientific defaults by meaning; ownership and laboratory need a user choice.
    default_metadata <- list(
      media_id = media$media_id[match("freshwater (surface)", tolower(trimws(media$media_type)))],
      collection_method = collection_methods$collection_method_id[match("water bottle (direct fill)", tolower(trimws(collection_methods$collection_method)))],
      sample_type = sample_types$sample_type_id[match("sample-routine", tolower(trimws(sample_types$sample_type)))],
      result_type = result_types$result_type_id[match("lab", tolower(trimws(result_types$result_type)))],
      matrix_state_id = matrix_states$matrix_state_id[match("liquid", tolower(trimws(matrix_states$matrix_state_code)))],
      result_value_type = result_value_types$result_value_type_id[match("actual", tolower(trimws(result_value_types$result_value_type)))],
      owner = NA_integer_, laboratory = NA_integer_)

    update_manage_formats <- function(selected = NULL) {
      profiles <- import_profiles()
      profiles <- profiles[profiles$source_code == input$manage_source, , drop = FALSE]
      if (is.null(selected) || !selected %in% profiles$profile_key) selected <- ""
      choices <- c("Source-wide / package downloads" = "")
      if (nrow(profiles) && "profile_key" %in% names(profiles)) {
        choices <- c(choices, stats::setNames(profiles$profile_key,
          paste0(profiles$profile_name, ifelse(profiles$active,""," (inactive)"))))
      }
      updateSelectizeInput(session,"manage_import_profile",choices=choices,selected=selected)
    }
    reload_profiles <- function(selected = NULL) {
      tryCatch({
        src <- DBI::dbGetQuery(con,"SELECT import_source_id,source_code,source_name,source_description,active
          FROM discrete.import_sources ORDER BY source_code")
        sources(src)
        profiles <- AquaCache::getImportProfiles(con,active=NULL,parse_json=TRUE)
        profiles$profile_key <- addDiscData_profile_key(profiles$source_code,profiles$profile_code)
        import_profiles(profiles)
        current <- addDiscData_first(input$manage_source,"")
        if (!is.null(selected) && selected %in% profiles$profile_key)
          current <- profiles$source_code[match(selected,profiles$profile_key)]
        if (!current %in% src$source_code) current <- if (nrow(src)) src$source_code[[1]] else ""
        updateSelectizeInput(session,"manage_source",choices=stats::setNames(src$source_code,
          paste0(src$source_code," — ",src$source_name,ifelse(src$active,""," (inactive)"))),selected=current)
        updateSelectizeInput(session,"review_source",choices=c("All sources"="",stats::setNames(src$source_code,src$source_code)),
          selected=addDiscData_first(input$review_source,""))
        if (identical(current,input$manage_source)) {
          update_manage_formats(selected)
        } else if (!is.null(selected)) {
          pending_profile_selection(selected)
        }
        profile_load_error(NULL)
      },error=function(e) profile_load_error(conditionMessage(e)))
    }
    reload_profiles()
    observeEvent(input$manage_source, {
      if (is.null(pending_open())) opened_review_mapping(NULL)
      clear_manager_mapping_selection()
      pending <- pending_open()
      selected <- if (!is.null(pending) && identical(input$manage_source,pending$source_code[[1]]) &&
        !is.na(pending$profile_code[[1]])) addDiscData_profile_key(pending$source_code[[1]],pending$profile_code[[1]]) else ""
      requested <- pending_profile_selection()
      if (!is.null(requested)) {
        selected <- requested
        pending_profile_selection(NULL)
      }
      update_manage_formats(selected)
    })
    observeEvent(input$manage_import_profile, {
      if (is.null(pending_open())) opened_review_mapping(NULL)
      clear_manager_mapping_selection()
      has_profile <- nzchar(addDiscData_first(input$manage_import_profile,""))
      choices <- c("Source-wide / package downloads"="source")
      if (has_profile) choices <- c(choices,"This workbook format"="profile")
      for (id in c("manage_mapping_scope","manage_location_scope","manage_flag_scope","manage_publish_scope"))
        updateSelectInput(session,id,choices=choices,selected=if (has_profile) "profile" else "source")
    },ignoreNULL=FALSE)
    observeEvent(input$refresh_setup, {
      reload_profiles(selected=input$manage_import_profile)
      bump_revision()
    })
    output$editing_mapping <- renderUI({
      row <- manage_mapping_selected()
      context <- opened_review_mapping()
      if (is.null(row)) {
        if (is.null(context)) {
          return(tags$div(class="alert alert-info",
            tags$strong("No mapping selected."),
            tags$p("Select a saved mapping in the table below, or choose Create mapping. Saving stages a draft; publication is a separate review step.")))
        }
        return(tags$div(class="alert alert-warning",
          tags$strong("Opening the selected mapping…"),
          tags$p(sprintf("%s | %s | %s",context$source_code[[1]],
            if (is.na(context$profile_code[[1]])) "Source-wide / package downloads" else context$profile_name[[1]],
            context$mapping_type[[1]])),
          tags$p(tags$strong("Needs attention: "),context$issue[[1]])))
      }
      id_field <- switch(input$manage_mapping_type,parameter="import_mapping_id",
        location="import_location_mapping_id",result_flag="import_result_flag_mapping_id")
      mapping_id <- row[[id_field]][[1]]
      if (!is.null(context)) {
        scope <- if (is.na(context$profile_code[[1]])) "Source-wide / package downloads" else
          addDiscData_first(context$profile_name[[1]],context$profile_code[[1]])
        issue <- addDiscData_first(context$issue[[1]],"Review this mapping's saved definition.")
        save_note <- if (identical(context$status[[1]],"published"))
          "Saving your correction stages it in a draft. The published mapping stays in use until you review and publish the whole scope." else
          "Saving updates the draft. It remains unpublished until you review and publish the whole scope."
        return(tags$div(class="alert alert-warning",
          tags$strong(sprintf("Editing %s mapping %s",context$mapping_type[[1]],mapping_id)),
          tags$p(sprintf("%s | %s | %s revision %s",context$source_code[[1]],scope,
            context$status[[1]],context$version[[1]])),
          tags$p(tags$strong("Needs attention: "),issue),
          tags$p(tags$strong("Source criteria: "),tags$code(context$source_match[[1]])),
          tags$p(save_note)))
      }
      status <- row$mapping_set_status[[1]]
      save_note <- if (identical(status,"published"))
        "Saving stages your change in a draft. This published revision stays unchanged until you review and publish the scope." else
        "Saving updates this draft. It remains unpublished until you review and publish the scope."
      tags$div(class="alert alert-info",
        tags$strong(sprintf("Editing %s mapping %s",input$manage_mapping_type,mapping_id)),
        tags$p(sprintf("%s revision %s",status,row$mapping_set_version[[1]])),
        tags$p(save_note))
    })
    observeEvent(input$new_source, {
      req(can_manage_mappings)
      showModal(modalDialog(title="Create import source",
        textInput(ns("source_code"),"Stable source code"),textInput(ns("source_name"),"Source name"),
        textAreaInput(ns("source_description"),"Description",rows=2),
        footer=tagList(modalButton("Cancel"),actionButton(ns("save_source"),"Create source"))))
    })
    observeEvent(input$save_source, {
      tryCatch({
        if (!can_manage_mappings) stop("Your database role cannot create import sources.")
        code <- trimws(addDiscData_first(input$source_code,""))
        name <- trimws(addDiscData_first(input$source_name,""))
        if (!nzchar(code) || !nzchar(name)) stop("Enter a source code and name.")
        DBI::dbExecute(con,"INSERT INTO discrete.import_sources (source_code,source_name,source_description)
          VALUES ($1,$2,$3)",params=list(code,name,addDiscData_first(input$source_description,NA_character_)))
        removeModal()
        reload_profiles()
        updateSelectizeInput(session,"manage_source",selected=code)
        bump_revision()
      },error=function(e) showNotification(conditionMessage(e),type="error"))
    })
    if (!can_manage_mappings) shinyjs::disable("new_source")

    output$new_profile_mapping_fields <- renderUI({
      parser_family <- addDiscData_first(
        input$new_profile_parser_family,
        "long"
      )
      specs <- addDiscData_profile_column_specs(parser_family)
      profile <- new_profile_template()
      column_map <- addDiscData_profile_value(profile, "column_map", list())
      fields <- lapply(names(specs), function(name) {
        value <- column_map[[name]]
        control <- if (identical(specs[[name]][[2]], "integer")) {
          numericInput(
            ns(paste0("new_profile_col_", name)),
            specs[[name]][[1]],
            value = addDiscData_int(value),
            min = 1,
            step = 1
          )
        } else {
          textInput(
            ns(paste0("new_profile_col_", name)),
            specs[[name]][[1]],
            value = addDiscData_first(value, "")
          )
        }
        column(6, control)
      })
      tagList(
        tags$h5(
          if (identical(parser_family, "transposed")) {
            "Source rows and columns"
          } else {
            "Source columns and layout"
          }
        ),
        helpText(
          if (identical(parser_family, "transposed")) {
            "Enter the worksheet row number (starting at 1) for each sample detail. Enter the worksheet column number (starting at 1) for the parameter name, code, unit, and first sample."
          } else {
            "Enter each column heading exactly as it appears in the worksheet."
          }
        ),
        do.call(fluidRow, fields)
      )
    })
    output$new_profile_default_fields <- renderUI({
      profile <- new_profile_template()
      defaults <- if (is.null(profile)) {
        default_metadata
      } else {
        addDiscData_defaults(profile)
      }
      tagList(
        tags$h5("Imported data defaults"),
        helpText(
          "These values fill fields that are not supplied by the workbook. They remain editable in the preview."
        ),
        fluidRow(
          column(
            4,
            selectizeInput(
              ns("new_profile_default_media"),
              "Media",
              choices = stats::setNames(media$media_id, media$media_type),
              selected = defaults$media_id
            )
          ),
          column(
            4,
            selectizeInput(
              ns("new_profile_default_method"),
              "Collection method",
              choices = stats::setNames(
                collection_methods$collection_method_id,
                collection_methods$collection_method
              ),
              selected = defaults$collection_method
            )
          ),
          column(
            4,
            selectizeInput(
              ns("new_profile_default_sample_type"),
              "Sample type",
              choices = stats::setNames(
                sample_types$sample_type_id,
                sample_types$sample_type
              ),
              selected = defaults$sample_type
            )
          )
        ),
        fluidRow(
          column(
            4,
            selectizeInput(
              ns("new_profile_default_owner"),
              "Owner",
              choices = c("Choose an owner" = "", stats::setNames(
                organizations$organization_id,
                organizations$name
              )),
              selected = if (is.na(defaults$owner)) "" else defaults$owner
            )
          ),
          column(
            4,
            selectizeInput(
              ns("new_profile_default_lab"),
              "Laboratory",
              choices = c("None" = "", stats::setNames(
                laboratories$lab_id,
                laboratories$lab_name
              )),
              selected = if (is.na(defaults$laboratory)) "" else defaults$laboratory
            )
          ),
          column(
            4,
            selectizeInput(
              ns("new_profile_default_result_type"),
              "Result type",
              choices = stats::setNames(
                result_types$result_type_id,
                result_types$result_type
              ),
              selected = defaults$result_type
            )
          )
        ),
        fluidRow(
          column(
            6,
            selectizeInput(
              ns("new_profile_default_matrix"),
              "Matrix state",
              choices = stats::setNames(
                matrix_states$matrix_state_id,
                matrix_states$matrix_state_name
              ),
              selected = defaults$matrix_state_id
            )
          ),
          column(
            6,
            selectizeInput(
              ns("new_profile_default_value_type"),
              "Result value type",
              choices = stats::setNames(
                result_value_types$result_value_type_id,
                result_value_types$result_value_type
              ),
              selected = defaults$result_value_type
            )
          )
        )
      )
    })
    open_profile_editor <- function(mode = "create") {
      mode <- match.arg(mode, c("create", "copy", "edit"))
      edit <- identical(mode, "edit")
      copy <- identical(mode, "copy")
      if (!can_manage_profiles) {
        showNotification(
          "Your database role cannot modify import profiles.",
          type = "error"
        )
        return()
      }
      profiles <- import_profiles()
      selected_key <- addDiscData_first(input$manage_import_profile, "")
      selected_row <- which(profiles$profile_key == selected_key)
      profile <- if (!identical(mode, "create") && length(selected_row) == 1L) {
        profiles[selected_row, , drop = FALSE]
      } else {
        NULL
      }
      if (!identical(mode, "create") && is.null(profile)) {
        showNotification("Select a workbook format first.", type = "warning")
        return()
      }
      timezone_choices <- input_timezone_choices()
      profile_timezone <- addDiscData_profile_value(profile, "timezone")
      if (
        is.null(profile_timezone) ||
          !length(profile_timezone) ||
          is.na(profile_timezone[[1]]) ||
          !nzchar(profile_timezone[[1]]) ||
          !(profile_timezone[[1]] %in% timezone_choices)
      ) {
        profile_timezone <- default_input_timezone()
      }
      new_profile_template(profile)
      new_profile_mode(mode)
      suggested_code <- if (edit) {
        profile$profile_code[[1]]
      } else if (copy) {
        paste0(profile$profile_code[[1]], "_copy")
      } else {
        ""
      }
      showModal(modalDialog(
        title = if (edit) {
          "Edit workbook format"
        } else if (copy) {
          "Create a copy of this workbook format"
        } else {
          "Create workbook format"
        },
        helpText(
          if (edit) {
            "Changes are saved to this workbook format and will be used by future previews. Reparse an open workbook to apply these changes. The source and format codes identify the saved format; create a copy if you need different codes."
          } else if (copy) {
            "This creates a new workbook format using the selected format's layout and defaults as a starting point. Give the copy a new code."
          } else {
            "Describe how the workbook is arranged. Saving creates a new format that will be available in the workbook selector."
          }
        ),
        helpText(
          "Use a short, stable code for the lab and this workbook format. For example: ALS and tatchun_samples."
        ),
        fluidRow(
          column(
            4,
            textInput(
              ns("new_profile_source_code"),
              "Lab/source short code",
              value = addDiscData_profile_value(profile, "source_code", addDiscData_first(input$manage_source,""))
            )
          ),
          column(
            8,
            textInput(
              ns("new_profile_source_name"),
              "Lab/source name",
              value = addDiscData_profile_value(profile, "source_name", sources()$source_name[match(input$manage_source,sources()$source_code)])
            )
          )
        ),
        fluidRow(
          column(
            4,
            textInput(
              ns("new_profile_code"),
              "Workbook format code",
              value = suggested_code
            )
          ),
          column(
            8,
            textInput(
              ns("new_profile_name"),
              "Workbook format name",
              value = if (copy) {
                paste(profile$profile_name[[1]], "copy")
              } else if (edit) {
                profile$profile_name[[1]]
              } else {
                ""
              }
            )
          )
        ),
        fluidRow(
          column(
            4,
            selectInput(
              ns("new_profile_parser_family"),
              "Workbook layout",
              choices = c(
                "Long table — one result per row" = "long",
                "Transposed table — one sample per column, one row per analyte" = "transposed",
                "XLR Detailed Report — lab export" = "xlr"
              ),
              selected = if (copy || edit) {
                addDiscData_parser_family(profile)
              } else {
                "long"
              }
            )
          ),
          column(
            4,
            textInput(
              ns("new_profile_sheet"),
              "Worksheet name (optional)",
              value = addDiscData_profile_value(profile, "sheet_name", "")
            )
          ),
          column(
            4,
            selectizeInput(
              ns("new_profile_timezone"),
              "Sample time zone (UTC offset)",
              choices = timezone_choices,
              selected = profile_timezone
            ),
            helpText(
              "This offset is used to convert the workbook's sample times to UTC. Yukon local time is UTC-07:00."
            )
          )
        ),
        fluidRow(
          column(
            4,
            numericInput(
              ns("new_profile_header_row"),
              "Column header row (long-table layout)",
              value = addDiscData_int(
                addDiscData_profile_value(profile, "header_row", 1L),
                1L
              ),
              min = 1,
              step = 1
            )
          )
        ),
        helpText(
          "For long-table workbooks, this row contains the column headings; rows above it are skipped. Row 1 is the default."
        ),
        helpText(
          "Choose the layout from the worksheet's shape, not from the lab name. ",
          tags$a(
            "Open the visual workbook layout guide",
            href = "html/discrete-workbook-layouts.html",
            target = "_blank",
            rel = "noopener noreferrer"
          )
        ),
        uiOutput(ns("new_profile_mapping_fields")),
        uiOutput(ns("new_profile_default_fields")),
        textAreaInput(
          ns("new_profile_description"),
          "Description",
          value = addDiscData_profile_value(
            profile,
            "profile_description",
            ""
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("save_import_profile"),
            if (edit) "Save workbook format" else "Create workbook format"
          )
        ),
        size = "l",
        easyClose = FALSE
      ))
    }
    observeEvent(input$new_import_profile, open_profile_editor("create"))
    observeEvent(input$copy_import_profile, open_profile_editor("copy"))
    observeEvent(input$edit_import_profile, open_profile_editor("edit"))
    observeEvent(input$save_import_profile, {
      tryCatch(
        {
          if (!can_manage_profiles) stop("Your database role cannot modify workbook formats.")
          profile <- new_profile_template()
          source_code <- trimws(addDiscData_first(
            input$new_profile_source_code,
            ""
          ))
          source_name <- trimws(addDiscData_first(
            input$new_profile_source_name,
            ""
          ))
          profile_code <- tolower(trimws(addDiscData_first(
            input$new_profile_code,
            ""
          )))
          profile_name <- trimws(addDiscData_first(input$new_profile_name, ""))
          profile_mode <- new_profile_mode()
          if (!grepl("^[a-z0-9][a-z0-9_]*$", profile_code)) {
            stop(
              "Workbook format code can contain lowercase letters, numbers, and underscores only."
            )
          }
          if (
            !nzchar(source_code) ||
              !nzchar(source_name) ||
              !nzchar(profile_name)
          ) {
            stop("Source code, source name, and profile name are required.")
          }
          new_profile_key <- addDiscData_profile_key(source_code, profile_code)
          original_profile_key <- if (identical(profile_mode, "edit")) {
            addDiscData_profile_key(
              addDiscData_profile_value(profile, "source_code", ""),
              addDiscData_profile_value(profile, "profile_code", "")
            )
          } else {
            character()
          }
          if (
            identical(profile_mode, "edit") &&
              !identical(new_profile_key, original_profile_key)
          ) {
            stop(
              "The source and format codes identify this saved format. Create a copy to use different codes."
            )
          }
          if (
            !identical(profile_mode, "edit") &&
              new_profile_key %in% import_profiles()$profile_key
          ) {
            stop(
              "That lab/source already has a workbook format with this code."
            )
          }
          parser_family <- addDiscData_first(
            input$new_profile_parser_family,
            "long"
          )
          if (!(parser_family %in% c("long", "transposed", "xlr"))) {
            stop("Select a supported workbook layout.")
          }
          header_row_default <- addDiscData_int(
            addDiscData_profile_value(profile, "header_row", 1L),
            1L
          )
          header_row <- addDiscData_int(
            input$new_profile_header_row,
            header_row_default
          )
          if (is.na(header_row) || header_row < 1L) {
            stop("Column header row must be 1 or greater.")
          }
          column_map <- addDiscData_profile_column_map(
            input,
            parser_family
          )
          required_mapping_fields <- if (
            identical(parser_family, "transposed")
          ) {
            c(
              "lab_sample_row",
              "station_code_row",
              "sample_date_row",
              "parameter_code_column",
              "unit_column",
              "first_sample_column"
            )
          } else if (identical(parser_family, "xlr")) {
            c(
              "station_code",
              "sample_date",
              "lab_sample_id",
              "parameter_name",
              "unit",
              "result"
            )
          } else {
            c(
              "station_code",
              "sample_date",
              "lab_sample_id",
              "parameter_code",
              "unit",
              "result"
            )
          }
          missing_mapping_fields <- setdiff(
            required_mapping_fields,
            names(column_map)
          )
          if (length(missing_mapping_fields)) {
            stop(
              "Complete the required source layout fields: ",
              paste(missing_mapping_fields, collapse = ", "),
              "."
            )
          }
          defaults <- list(
            media_id = addDiscData_int(input$new_profile_default_media, default_metadata$media_id),
            collection_method = addDiscData_int(
              input$new_profile_default_method,
              default_metadata$collection_method
            ),
            sample_type = addDiscData_int(
              input$new_profile_default_sample_type,
              default_metadata$sample_type
            ),
            owner = addDiscData_int(input$new_profile_default_owner, default_metadata$owner),
            result_type = addDiscData_int(
              input$new_profile_default_result_type,
              default_metadata$result_type
            ),
            matrix_state_id = addDiscData_int(
              input$new_profile_default_matrix,
              default_metadata$matrix_state_id
            ),
            result_value_type = addDiscData_int(
              input$new_profile_default_value_type,
              default_metadata$result_value_type
            ),
            laboratory = addDiscData_int(input$new_profile_default_lab, default_metadata$laboratory)
          )
          if (is.na(defaults$owner)) stop("Choose the owner for samples imported with this format.")
          validation_rules <- addDiscData_profile_value(
            profile,
            "validation_rules",
            list()
          )
          validation_rules$parser_family <- parser_family
          DBI::dbWithTransaction(con, {
            AquaCache::upsertImportProfile(
              con = con,
              source_code = source_code,
              source_name = source_name,
              source_description = addDiscData_profile_value(
                profile,
                "source_description"
              ),
              profile_code = profile_code,
              profile_name = profile_name,
              profile_description = trimws(addDiscData_first(
                input$new_profile_description,
                ""
              )),
              file_type = addDiscData_profile_value(
                profile,
                "file_type",
                "xlsx"
              ),
              parser_type = if (identical(parser_family, "transposed")) {
                "wide"
              } else {
                "long"
              },
              sheet_strategy = addDiscData_profile_value(
                profile,
                "sheet_strategy",
                "name_or_first"
              ),
              sheet_name = trimws(addDiscData_first(
                input$new_profile_sheet,
                ""
              )),
              sheet_index = addDiscData_profile_value(profile, "sheet_index"),
              header_row = header_row,
              units_row = addDiscData_profile_value(profile, "units_row"),
              parameter_row = addDiscData_profile_value(
                profile,
                "parameter_row"
              ),
              data_start_row = addDiscData_profile_value(
                profile,
                "data_start_row",
                2L
              ),
              datetime_origin = addDiscData_profile_value(
                profile,
                "datetime_origin",
                "excel_1900"
              ),
              timezone = addDiscData_first(
                input$new_profile_timezone,
                "America/Whitehorse"
              ),
              column_map = column_map,
              wide_config = addDiscData_profile_value(
                profile,
                "wide_config",
                list()
              ),
              defaults = defaults,
              sample_identity = addDiscData_profile_value(
                profile,
                "sample_identity",
                c(
                  "location_id",
                  "sub_location_id",
                  "media_id",
                  "z",
                  "datetime",
                  "sample_type",
                  "collection_method"
                )
              ),
              result_identity = addDiscData_profile_value(
                profile,
                "result_identity",
                c(
                  "result_type",
                  "parameter_id",
                  "matrix_state_id",
                  "sample_fraction_id",
                  "result_value_type",
                  "reporting_basis_id",
                  "protocol_method",
                  "laboratory",
                  "analysis_datetime"
                )
              ),
              validation_rules = validation_rules,
              active = if (identical(profile_mode, "edit")) {
                isTRUE(addDiscData_profile_value(profile, "active", TRUE))
              } else {
                TRUE
              },
              note = "Saved from AquaLink Import setup."
            )
          })
          reload_profiles(selected = new_profile_key)
          bump_revision()
          new_profile_template(NULL)
          removeModal()
          showNotification(
            if (identical(profile_mode, "edit")) "Workbook format updated." else "Workbook format created.",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Saving workbook format failed:", e$message),
            type = "error"
          )
        }
      )
    })
    selected_manage_profile <- reactive({
      src <- sources()
      src <- src[src$source_code == input$manage_source, , drop = FALSE]
      req(nrow(src) == 1L)
      profiles <- import_profiles()
      selected <- profiles[profiles$profile_key == input$manage_import_profile, , drop = FALSE]
      if (nrow(selected) == 1L && selected$source_code[[1]] == src$source_code[[1]]) return(selected)
      data.frame(source_code = src$source_code, source_name = src$source_name,
        import_source_id = src$import_source_id, profile_code = NA_character_,
        profile_name = NA_character_, profile_key = "", stringsAsFactors = FALSE)
    })
    manager_text <- function(value, default = "") {
      if (is.null(value) || !length(value) || is.na(value[[1]])) {
        return(default)
      }
      as.character(value[[1]])
    }
    output$manage_import_profile_status <- renderUI({
      error <- profile_load_error()
      if (!is.null(error)) {
        return(tags$div(class = "alert alert-danger", error))
      }
      if (!can_manage_profiles) {
        return(tags$div(
          class = "text-muted",
          "Your database role can use workbook formats but cannot create or edit them."
        ))
      }
      if (!nrow(import_profiles())) {
        return(tags$div(
          class = "alert alert-warning",
          "No workbook formats are saved. Source-wide mappings can still be managed without a format."
        ))
      }
      NULL
    })
    output$manage_mapping_permissions <- renderUI({
      error <- manage_mapping_error()
      if (!is.null(error)) {
        return(tags$div(class = "alert alert-danger", error))
      }
      if (!can_manage_mappings) {
        return(tags$div(
          class = "text-muted",
          "You can inspect saved mappings, but your database role cannot create or change them."
        ))
      }
      NULL
    })
    output$manage_mapping_completeness <- renderUI({
      req(identical(input$manage_mapping_type, "parameter"))
      rows <- manage_mapping_rows()
      if (!nrow(rows)) {
        return(NULL)
      }
      invalid <- tryCatch(
        DBI::dbGetQuery(
          con,
          "SELECT m.import_mapping_id, discrete.import_parameter_mapping_error(m) AS issue
         FROM discrete.import_parameter_mappings m
         WHERE m.import_mapping_id IN (SELECT jsonb_array_elements_text($1::JSONB)::INTEGER)
           AND m.active AND discrete.import_parameter_mapping_error(m) IS NOT NULL
         ORDER BY m.import_mapping_id",
          params = list(jsonlite::toJSON(
            rows$import_mapping_id,
            auto_unbox = FALSE
          ))
        ),
        error = function(e) e
      )
      if (inherits(invalid, "error")) {
        return(tags$div(
          class = "alert alert-danger",
          paste(
            "Could not check mapping completeness:",
            conditionMessage(invalid)
          )
        ))
      }
      if (!nrow(invalid)) {
        return(NULL)
      }
      tags$div(
        class = "alert alert-warning",
        tags$strong("Incomplete active parameter mappings: "),
        paste(
          sprintf("%s (%s)", invalid$import_mapping_id, invalid$issue),
          collapse = "; "
        ),
        tags$p(
          "Complete or deactivate these mappings in the draft before publishing. Imports skip incomplete mappings."
        )
      )
    })
    observe({
      has_profile <- nzchar(addDiscData_first(input$manage_import_profile, ""))
      has_source <- nzchar(addDiscData_first(input$manage_source, ""))
      if (can_manage_profiles && has_profile) {
        shinyjs::enable("copy_import_profile")
        shinyjs::enable("edit_import_profile")
      } else {
        shinyjs::disable("copy_import_profile")
        shinyjs::disable("edit_import_profile")
      }
      if (can_manage_mappings && has_source) {
        shinyjs::enable("manage_publish_mappings")
        shinyjs::enable("manage_new_mapping")
        shinyjs::enable("manage_save_parameter_mapping")
        shinyjs::enable("manage_save_location_mapping")
        shinyjs::enable("manage_save_flag_mapping")
      } else {
        shinyjs::disable("manage_publish_mappings")
        shinyjs::disable("manage_new_mapping")
        shinyjs::disable("manage_save_parameter_mapping")
        shinyjs::disable("manage_save_location_mapping")
        shinyjs::disable("manage_save_flag_mapping")
      }
    })
    observe({
      parameter_rows <- params()
      updateSelectizeInput(
        session,
        "manage_parameter_id",
        choices = addDiscData_parameter_choices(parameter_rows),
        selected = "", server = TRUE
      )
      updateSelectizeInput(
        session,
        "manage_result_type",
        choices = stats::setNames(
          as.character(result_types$result_type_id),
          result_types$result_type
        ),
        selected = as.character(default_metadata$result_type)
      )
      updateSelectizeInput(
        session,
        "manage_matrix_state",
        choices = stats::setNames(
          as.character(matrix_states$matrix_state_id),
          matrix_states$matrix_state_name
        ),
        selected = as.character(default_metadata$matrix_state_id)
      )
      updateSelectizeInput(
        session,
        "manage_sample_fraction",
        choices = c(
          "None" = "",
          stats::setNames(
            as.character(sample_fractions$sample_fraction_id),
            sample_fractions$sample_fraction
          )
        ),
        selected = ""
      )
      updateSelectizeInput(
        session,
        "manage_value_type",
        choices = stats::setNames(
          as.character(result_value_types$result_value_type_id),
          result_value_types$result_value_type
        ),
        selected = as.character(default_metadata$result_value_type)
      )
      updateSelectizeInput(
        session,
        "manage_descriptor",
        choices = c(
          "None" = "",
          stats::setNames(
            as.character(reporting_bases$reporting_basis_id),
            reporting_bases$reporting_basis
          )
        ),
        selected = ""
      )
      updateSelectInput(
        session,
        "manage_flag_condition",
        choices = c(
          "None" = "",
          stats::setNames(
            as.character(result_conditions$result_condition_id),
            result_conditions$result_condition
          )
        )
      )
      updateSelectizeInput(
        session,
        "manage_location_id",
        choices = addDiscData_location_choices(
          locations(),
          include_blank = TRUE
        ),
        selected = ""
      )
      updateSelectizeInput(
        session,
        "manage_sub_location_id",
        choices = c("None" = ""),
        selected = ""
      )
    })
    observeEvent(
      input$manage_location_id,
      {
        location_id <- addDiscData_int(input$manage_location_id)
        available <- if (is.na(location_id)) {
          sub_locations[FALSE, , drop = FALSE]
        } else {
          sub_locations[
            sub_locations$location_id == location_id,
            ,
            drop = FALSE
          ]
        }
        updateSelectizeInput(
          session,
          "manage_sub_location_id",
          choices = c(
            "None" = "",
            stats::setNames(
              as.character(available$sub_location_id),
              available$sub_location_name
            )
          )
        )
      },
      ignoreInit = TRUE
    )
    manage_mapping_rows <- reactive({
      mapping_revision()
      manage_mapping_error(NULL)
      profile <- tryCatch(
        selected_manage_profile(),
        error = function(e) NULL
      )
      if (is.null(profile) || !nrow(profile)) {
        return(data.frame())
      }
      rows <- tryCatch(
        {
          switch(
            input$manage_mapping_type,
            parameter = AquaCache::getImportParameterMappings(
              con,
              source_code = profile$source_code[[1]],
              profile_code = if (is.na(profile$profile_code[[1]])) NULL else profile$profile_code[[1]],
              active = NULL,
              include_draft = TRUE
            ),
            location = AquaCache::getImportLocationMappings(
              con,
              source_code = profile$source_code[[1]],
              profile_code = if (is.na(profile$profile_code[[1]])) NULL else profile$profile_code[[1]],
              active = NULL,
              include_draft = TRUE
            ),
            result_flag = AquaCache::getImportResultFlagMappings(
              con,
              source_code = profile$source_code[[1]],
              profile_code = if (is.na(profile$profile_code[[1]])) NULL else profile$profile_code[[1]],
              active = NULL,
              include_draft = TRUE
            ),
            data.frame()
          )
        },
        error = function(e) {
          manage_mapping_error(e$message)
          data.frame()
        }
      )
      if (!isTRUE(input$manage_show_inactive) && nrow(rows)) {
        active <- as.logical(rows$active)
        rows <- rows[!is.na(active) & active, , drop = FALSE]
      }
      rows
    })
    output$manage_mapping_table <- DT::renderDT(
      {
        rows <- manage_mapping_rows()
        if (!nrow(rows)) {
          message <- if (!is.null(manage_mapping_error())) {
            "Mappings could not be loaded. See the message below."
          } else {
            "No mappings are saved for this workbook format yet."
          }
          return(DT::datatable(
            data.frame(Message = message),
            rownames = FALSE,
            selection = "none",
            options = list(
              dom = "t"
            )
          ))
        }

        scope <- ifelse(
          as.logical(rows$profile_specific),
          "This workbook format",
          "All formats for this source"
        )
        active <- ifelse(as.logical(rows$active), "Active", "Inactive")
        if (identical(input$manage_mapping_type, "parameter")) {
          source_match <- lapply(rows$source_match, function(value) {
            tryCatch(
              jsonlite::fromJSON(value, simplifyVector = FALSE),
              error = function(e) list()
            )
          })
          parameter_index <- match(rows$parameter_id, params()$parameter_id)
          summary <- data.frame(
            Scope = scope,
            `Source matching criteria` = rows$source_match,
            `AquaCache parameter` = params()$param_name[parameter_index],
            Conversion = rows$conversion,
            Offset = rows$result_offset,
            Priority = rows$priority,
            Status = active,
            check.names = FALSE
          )
        } else if (identical(input$manage_mapping_type, "location")) {
          location_index <- match(rows$location_id, locations()$location_id)
          sublocation_index <- match(
            rows$sub_location_id,
            sub_locations$sub_location_id
          )
          summary <- data.frame(
            Scope = scope,
            `Source location code` = rows$source_location_code,
            `Source location name` = rows$source_location_name,
            `AquaCache location` = addDiscData_location_labels(locations())[
              location_index
            ],
            `Sub-location` = sub_locations$sub_location_name[sublocation_index],
            Priority = rows$priority,
            Status = active,
            check.names = FALSE
          )
        } else {
          summary <- data.frame(
            Scope = scope,
            `Source flag column` = ifelse(
              addDiscData_present(rows$source_flag_column),
              rows$source_flag_column,
              "Any column"
            ),
            `Source flag value` = rows$source_flag_value,
            `Result condition` = rows$result_condition_name,
            `Handling` = rows$result_action,
            Priority = rows$priority,
            Status = active,
            check.names = FALSE
          )
        }
        summary$Revision <- paste(
          rows$mapping_set_status,
          rows$mapping_set_version
        )
        summary[is.na(summary)] <- ""
        addDiscData_style_origin_columns(
          DT::datatable(
            summary,
            rownames = FALSE,
            selection = "single",
            options = list(
              pageLength = 10,
              scrollX = TRUE,
              initComplete = htmlwidgets::JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'font-size': '90%'});",
                "$(this.api().table().body()).css({'font-size': '80%'});",
                "}"
              )
            )
          ),
          source_columns = if (
            identical(input$manage_mapping_type, "parameter")
          ) {
            "Source matching criteria"
          } else if (identical(input$manage_mapping_type, "location")) {
            c("Source location code", "Source location name")
          } else {
            c("Source flag column", "Source flag value")
          },
          target_columns = if (
            identical(input$manage_mapping_type, "parameter")
          ) {
            c(
              "AquaCache parameter",
              "Conversion",
              "Offset"
            )
          } else if (identical(input$manage_mapping_type, "location")) {
            c("AquaCache location", "Sub-location")
          } else {
            c("Result condition", "Handling")
          }
        )
      },
      server = FALSE
    )
    selected_manage_mapping <- reactive({
      rows <- manage_mapping_rows()
      selected <- input$manage_mapping_table_rows_selected
      if (!nrow(rows) || length(selected) != 1L || selected > nrow(rows)) {
        return(NULL)
      }
      rows[selected, , drop = FALSE]
    })
    observeEvent(
      input$manage_mapping_type,
      {
        if (is.null(pending_open())) opened_review_mapping(NULL)
        clear_manager_mapping_selection()
      },
      ignoreInit = TRUE
    )
    load_manager_mapping <- function(row) {
        if (is.null(row)) {
          return()
        }
        manage_mapping_selected(row)
        scope <- if (isTRUE(as.logical(row$profile_specific[[1]]))) {
          "profile"
        } else {
          "source"
        }
        active <- isTRUE(as.logical(row$active[[1]]))
        priority <- addDiscData_int(row$priority[[1]], 50L)
        note <- manager_text(row$note[[1]])
        if (identical(input$manage_mapping_type, "parameter")) {
          source_match <- tryCatch(
            jsonlite::fromJSON(row$source_match[[1]], simplifyVector = FALSE),
            error = function(e) list()
          )
          updateTextAreaInput(session, "manage_source_match", value = row$source_match[[1]])
          updateSelectInput(session, "manage_mapping_scope", selected = scope)
          updateSelectizeInput(
            session,
            "manage_parameter_id",
            choices = addDiscData_parameter_choices(params()),
            selected = if (is.na(row$parameter_id[[1]])) "" else as.character(row$parameter_id[[1]]),
            server = TRUE
          )
          updateSelectizeInput(
            session,
            "manage_result_type",
            selected = as.character(row$result_type[[1]])
          )
          updateSelectizeInput(
            session,
            "manage_matrix_state",
            selected = as.character(row$matrix_state_id[[1]])
          )
          updateSelectizeInput(
            session,
            "manage_sample_fraction",
            selected = if (is.na(row$sample_fraction_id[[1]])) {
              ""
            } else {
              as.character(row$sample_fraction_id[[1]])
            }
          )
          updateSelectizeInput(
            session,
            "manage_value_type",
            selected = as.character(row$result_value_type[[1]])
          )
          updateSelectizeInput(
            session,
            "manage_descriptor",
            selected = if (is.na(row$reporting_basis_id[[1]])) {
              ""
            } else {
              as.character(row$reporting_basis_id[[1]])
            }
          )
          output$manage_chemistry <- renderUI({
            YGwater:::chemistry_metadata_ui(
              ns,
              "manage_chem_",
              chemistry_catalogues,
              row
            )
          })
          updateNumericInput(session, "manage_priority", value = priority)
          updateNumericInput(
            session,
            "manage_conversion",
            value = addDiscData_num(row$conversion[[1]], 1)
          )
          updateNumericInput(
            session,
            "manage_result_offset",
            value = addDiscData_num(row$result_offset[[1]], 0)
          )
          updateCheckboxInput(session, "manage_active", value = active)
          updateTextAreaInput(session, "manage_mapping_note", value = note)
        } else if (identical(input$manage_mapping_type, "location")) {
          updateTextInput(
            session,
            "manage_location_code",
            value = row$source_location_code[[1]]
          )
          updateTextInput(
            session,
            "manage_location_name",
            value = manager_text(row$source_location_name[[1]])
          )
          updateSelectInput(session, "manage_location_scope", selected = scope)
          updateSelectizeInput(
            session,
            "manage_location_id",
            selected = as.character(row$location_id[[1]])
          )
          updateSelectizeInput(
            session,
            "manage_sub_location_id",
            selected = if (is.na(row$sub_location_id[[1]])) {
              ""
            } else {
              as.character(row$sub_location_id[[1]])
            }
          )
          updateNumericInput(
            session,
            "manage_location_priority",
            value = priority
          )
          updateCheckboxInput(session, "manage_location_active", value = active)
          updateTextAreaInput(session, "manage_location_note", value = note)
        } else {
          updateTextInput(
            session,
            "manage_flag_column",
            value = manager_text(row$source_flag_column[[1]])
          )
          updateTextInput(
            session,
            "manage_flag_value",
            value = row$source_flag_value[[1]]
          )
          updateSelectInput(session, "manage_flag_scope", selected = scope)
          updateSelectInput(
            session,
            "manage_flag_action",
            selected = row$result_action[[1]]
          )
          updateSelectInput(
            session,
            "manage_flag_condition",
            selected = if (is.na(row$result_condition_id[[1]])) {
              ""
            } else {
              as.character(row$result_condition_id[[1]])
            }
          )
          updateSelectInput(
            session,
            "manage_flag_threshold_source",
            selected = row$result_condition_value_source[[1]]
          )
          updateNumericInput(
            session,
            "manage_flag_threshold",
            value = row$result_condition_value_literal[[1]]
          )
          updateNumericInput(session, "manage_flag_priority", value = priority)
          updateCheckboxInput(session, "manage_flag_active", value = active)
          updateTextInput(
            session,
            "manage_flag_note_template",
            value = manager_text(row$note_template[[1]])
          )
          updateTextAreaInput(session, "manage_flag_note", value = note)
        }
    }
    observeEvent(input$manage_mapping_table_rows_selected, {
      selected_index <- addDiscData_int(input$manage_mapping_table_rows_selected)
      from_review <- !is.null(manager_selection_from_review()) &&
        identical(selected_index, manager_selection_from_review())
      if (!from_review) opened_review_mapping(NULL)
      manager_selection_from_review(NULL)
      load_manager_mapping(selected_manage_mapping())
    })
    observeEvent(input$manager_mapping_table_selection, {
      selected <- addDiscData_int(input$manager_mapping_table_selection)
      if (is.na(selected) || selected < 1L) return()
      proxy <- DT::dataTableProxy("manage_mapping_table", session = session)
      DT::selectRows(proxy, selected)
    }, ignoreInit = TRUE)

    observeEvent(input$manage_new_mapping, {
      opened_review_mapping(NULL)
      clear_manager_mapping_selection()
      updateTextAreaInput(session, "manage_source_match", value = '{"parameter_code":"", "unit":""}')
      updateSelectInput(session, "manage_mapping_scope", selected = if (nzchar(addDiscData_first(input$manage_import_profile, ""))) "profile" else "source")
      updateSelectizeInput(session, "manage_parameter_id", selected = "")
      updateSelectizeInput(session, "manage_result_type", selected = as.character(default_metadata$result_type))
      updateSelectizeInput(session, "manage_matrix_state", selected = as.character(default_metadata$matrix_state_id))
      updateSelectizeInput(session, "manage_sample_fraction", selected = "")
      updateSelectizeInput(session, "manage_value_type", selected = as.character(default_metadata$result_value_type))
      updateSelectizeInput(session, "manage_descriptor", selected = "")
      output$manage_chemistry <- renderUI({
        YGwater:::chemistry_metadata_ui(
          ns,
          "manage_chem_",
          chemistry_catalogues
        )
      })
      updateNumericInput(session, "manage_priority", value = 50)
      updateNumericInput(session, "manage_conversion", value = 1)
      updateNumericInput(session, "manage_result_offset", value = 0)
      updateCheckboxInput(session, "manage_active", value = TRUE)
      updateTextAreaInput(session, "manage_mapping_note", value = "")
      updateTextInput(session, "manage_location_code", value = "")
      updateTextInput(session, "manage_location_name", value = "")
      updateSelectInput(session, "manage_location_scope", selected = if (nzchar(addDiscData_first(input$manage_import_profile, ""))) "profile" else "source")
      updateSelectizeInput(session, "manage_location_id", selected = "")
      updateSelectizeInput(session, "manage_sub_location_id", selected = "")
      updateNumericInput(session, "manage_location_priority", value = 50)
      updateCheckboxInput(session, "manage_location_active", value = TRUE)
      updateTextAreaInput(session, "manage_location_note", value = "")
      updateTextInput(session, "manage_flag_column", value = "")
      updateTextInput(session, "manage_flag_value", value = "")
      updateSelectInput(session, "manage_flag_scope", selected = if (nzchar(addDiscData_first(input$manage_import_profile, ""))) "profile" else "source")
      updateSelectInput(session, "manage_flag_action", selected = "keep_result")
      updateSelectInput(session, "manage_flag_condition", selected = "")
      updateSelectInput(
        session,
        "manage_flag_threshold_source",
        selected = "none"
      )
      updateNumericInput(session, "manage_flag_threshold", value = NA_real_)
      updateNumericInput(session, "manage_flag_priority", value = 50)
      updateCheckboxInput(session, "manage_flag_active", value = TRUE)
      updateTextInput(session, "manage_flag_note_template", value = "")
      updateTextAreaInput(session, "manage_flag_note", value = "")
    })
    save_manager_mapping <- function(kind) {
      if (!can_manage_mappings) {
        stop("Your database role cannot modify import mappings.")
      }
      profile <- selected_manage_profile()
      selected <- manage_mapping_selected()
      if (!is.null(selected) && !identical(selected$source_code[[1]], profile$source_code[[1]]))
        stop("Select the mapping again after changing sources.")
      scope_input <- switch(
        kind,
        parameter = "manage_mapping_scope",
        location = "manage_location_scope",
        result_flag = "manage_flag_scope"
      )
      scope <- addDiscData_first(input[[scope_input]], "source")
      profile_code <- if (identical(scope, "profile")) {
        value <- profile$profile_code[[1]]
        if (is.na(value) || !nzchar(value)) stop("Select a workbook format for a profile-specific mapping.")
        value
      } else if (identical(scope, "source")) {
        NULL
      } else {
        stop("Choose where this mapping should apply.")
      }
      if (!is.null(selected) && (!identical(!is.null(profile_code), isTRUE(selected$profile_specific[[1]])) ||
          (!is.null(profile_code) && !identical(profile_code, selected$profile_code[[1]]))))
        stop("The scope identifies the saved mapping. Create a new mapping to use a different scope.")
      src <- sources()[sources()$source_code == profile$source_code[[1]], , drop = FALSE]
      if (!nrow(src) || !isTRUE(src$active[[1]])) stop("This import source is inactive; inspect its mappings without changing them.")
      source_name <- manager_text(
        profile$source_name[[1]],
        profile$source_code[[1]]
      )
      DBI::dbWithTransaction(con, {
      # Package upserts lock the source row before the scope. Use that same
      # order to avoid deadlocks and preserve concurrently edited source names.
      locked_source <- DBI::dbGetQuery(con,"SELECT source_name,active FROM discrete.import_sources
        WHERE import_source_id=$1 FOR UPDATE",params=list(profile$import_source_id[[1]]))
      if (nrow(locked_source)!=1L || !isTRUE(locked_source$active[[1]])) stop("The import source is missing or inactive.")
      source_name <- locked_source$source_name[[1]]
      scope_id <- if (is.null(profile_code)) "source" else profile$import_profile_id[[1]]
      scope_key <- paste(profile$import_source_id[[1]],scope_id,sep=":")
      DBI::dbGetQuery(con,"SELECT pg_advisory_xact_lock(hashtextextended($1,0))",params=list(scope_key))
      fresh <- switch(kind,
        parameter=AquaCache::getImportParameterMappings(con,source_code=profile$source_code[[1]],profile_code=profile_code,active=NULL,include_draft=TRUE),
        location=AquaCache::getImportLocationMappings(con,source_code=profile$source_code[[1]],profile_code=profile_code,active=NULL,include_draft=TRUE),
        result_flag=AquaCache::getImportResultFlagMappings(con,source_code=profile$source_code[[1]],profile_code=profile_code,active=NULL,include_draft=TRUE))
      if (!is.null(selected)) {
        id_field <- switch(kind,parameter="import_mapping_id",location="import_location_mapping_id",result_flag="import_result_flag_mapping_id")
        current <- fresh[fresh[[id_field]]==selected[[id_field]][[1]],,drop=FALSE]
        if (nrow(current)!=1L || !identical(as.list(current),as.list(selected)))
          stop("The saved mapping changed after it was selected. Refresh and select it again before saving.")
      }
      if (identical(kind, "parameter")) {
        criteria <- importSetup_match_values(input$manage_source_match)
        if (!is.null(selected)) {
          original <- importSetup_match_values(selected$source_match[[1]])
          if (!identical(criteria[sort(names(criteria))], original[sort(names(original))]))
            stop("Matching criteria identify the saved mapping. Create a new mapping to use different criteria.")
        }
        if (is.null(selected)) {
          canonical <- criteria[sort(names(criteria))]
          existing <- vapply(fresh$source_match, function(value) {
            parsed <- jsonlite::fromJSON(value, simplifyVector = FALSE)
            identical(parsed[sort(names(parsed))], canonical)
          }, logical(1))
          if (any(existing & fresh$profile_specific == !is.null(profile_code)))
            stop("A mapping with these criteria already exists in this scope. Select it in the table before editing.")
        }
        parameter_id <- addDiscData_int(input$manage_parameter_id)
        sample_fraction_id <- addDiscData_int(input$manage_sample_fraction)
        reporting_basis_id <- addDiscData_int(input$manage_descriptor)
        chemistry <- YGwater:::chemistry_metadata_values(input, "manage_chem_")
        conversion <- addDiscData_num(input$manage_conversion, 1)
        result_offset <- addDiscData_num(input$manage_result_offset, 0)
        if (!is.finite(conversion) || !is.finite(result_offset)) {
          stop("Enter finite conversion and offset values.")
        }
        AquaCache::upsertImportParameterMappings(
          con = con,
          source_code = profile$source_code[[1]],
          source_name = source_name,
          profile_code = profile_code,
          mappings = data.frame(
            chemistry,
            as.data.frame(criteria, stringsAsFactors = FALSE, check.names = FALSE),
            parameter_id = parameter_id,
            result_type = addDiscData_int(input$manage_result_type, default_metadata$result_type),
            sample_fraction_id = sample_fraction_id,
            result_value_type = addDiscData_int(input$manage_value_type, default_metadata$result_value_type),
            reporting_basis_id = reporting_basis_id,
            matrix_state_id = addDiscData_int(input$manage_matrix_state, default_metadata$matrix_state_id),
            conversion = conversion,
            result_offset = result_offset,
            priority = addDiscData_int(input$manage_priority, 50L),
            active = isTRUE(input$manage_active),
            note = addDiscData_first(input$manage_mapping_note, NA_character_),
            stringsAsFactors = FALSE, check.names = FALSE
          ),
          match_columns = names(criteria),
          publish = FALSE
        )
      } else if (identical(kind, "location")) {
        location_code <- trimws(addDiscData_first(
          input$manage_location_code,
          ""
        ))
        location_id <- addDiscData_int(input$manage_location_id)
        sub_location_id <- addDiscData_int(input$manage_sub_location_id)
        if (!is.null(selected) && !identical(location_code, selected$source_location_code[[1]]))
          stop("Create a new mapping to use a different source location code.")
        if (!nzchar(location_code)) {
          stop("Enter a source location code.")
        }
        if (is.null(selected) && any(fresh$source_location_code == location_code &
            fresh$profile_specific == !is.null(profile_code)))
          stop("A mapping for this source location already exists in this scope. Select it in the table before editing.")
        if (is.na(location_id)) {
          stop("Choose an AquaCache location.")
        }
        if (
          !is.na(sub_location_id) &&
            !any(
              sub_locations$sub_location_id == sub_location_id &
                sub_locations$location_id == location_id
            )
        ) {
          stop("Choose a sub-location that belongs to the selected location.")
        }
        source_location_name <- trimws(addDiscData_first(
          input$manage_location_name,
          ""
        ))
        if (!nzchar(source_location_name)) {
          source_location_name <- NA_character_
        }
        AquaCache::upsertImportLocationMappings(
          con = con,
          source_code = profile$source_code[[1]],
          source_name = source_name,
          profile_code = profile_code,
          mappings = data.frame(
            source_location_code = location_code,
            source_location_name = source_location_name,
            location_id = location_id,
            sub_location_id = sub_location_id,
            priority = addDiscData_int(input$manage_location_priority, 50L),
            active = isTRUE(input$manage_location_active),
            note = addDiscData_first(input$manage_location_note, NA_character_),
            stringsAsFactors = FALSE
          ),
          publish = FALSE
        )
      } else {
        flag_value <- trimws(addDiscData_first(input$manage_flag_value, ""))
        if (!is.null(selected) && !identical(flag_value, selected$source_flag_value[[1]]))
          stop("Create a new mapping to use a different source flag value.")
        if (!nzchar(flag_value)) {
          stop("Enter a source result-flag value.")
        }
        threshold_source <- addDiscData_first(
          input$manage_flag_threshold_source,
          "none"
        )
        threshold <- if (identical(threshold_source, "literal")) {
          addDiscData_num(input$manage_flag_threshold)
        } else {
          NA_real_
        }
        if (identical(threshold_source, "literal") && !is.finite(threshold)) {
          stop("Enter a finite condition value.")
        }
        flag_column <- trimws(addDiscData_first(input$manage_flag_column, ""))
        if (!is.null(selected) && !identical(flag_column, manager_text(selected$source_flag_column)))
          stop("Create a new mapping to use a different source flag column.")
        if (is.null(selected) && any(fresh$source_flag_value == flag_value &
            ifelse(is.na(fresh$source_flag_column), "", fresh$source_flag_column) == flag_column &
            fresh$profile_specific == !is.null(profile_code)))
          stop("A mapping for this source flag already exists in this scope. Select it in the table before editing.")
        if (!nzchar(flag_column)) {
          flag_column <- NA_character_
        }
        flag_note <- trimws(addDiscData_first(
          input$manage_flag_note_template,
          ""
        ))
        if (!nzchar(flag_note)) {
          flag_note <- NA_character_
        }
        AquaCache::upsertImportResultFlagMappings(
          con = con,
          source_code = profile$source_code[[1]],
          source_name = source_name,
          profile_code = profile_code,
          mappings = data.frame(
            source_flag_column = flag_column,
            source_flag_value = flag_value,
            result_condition_id = addDiscData_int(input$manage_flag_condition),
            result_condition_value_source = threshold_source,
            result_condition_value_literal = threshold,
            result_action = addDiscData_first(
              input$manage_flag_action,
              "keep_result"
            ),
            note_template = flag_note,
            priority = addDiscData_int(input$manage_flag_priority, 50L),
            active = isTRUE(input$manage_flag_active),
            note = addDiscData_first(input$manage_flag_note, NA_character_),
            stringsAsFactors = FALSE
          ),
          publish = FALSE
        )
      }

      })
      bump_revision()
      clear_manager_mapping_selection()
      invisible(NULL)
    }
    observeEvent(input$manage_save_parameter_mapping, {
      tryCatch(
        {
          save_manager_mapping("parameter")
          opened_review_mapping(NULL)
          showNotification(
            "Parameter mapping saved as a draft. Publish this scope when all active mappings are complete.",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Saving parameter mapping failed:", conditionMessage(e)),
            type = "error",
            duration = 15
          )
        }
      )
    })
    observeEvent(input$manage_save_location_mapping, {
      tryCatch(
        {
          save_manager_mapping("location")
          opened_review_mapping(NULL)
          showNotification(
            "Location mapping saved as a draft. Publish this scope when all active mappings are complete.",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Saving location mapping failed:", conditionMessage(e)),
            type = "error",
            duration = 15
          )
        }
      )
    })
    observeEvent(input$manage_save_flag_mapping, {
      tryCatch(
        {
          save_manager_mapping("result_flag")
          opened_review_mapping(NULL)
          showNotification(
            "Result-flag mapping saved as a draft. Publish this scope when all active mappings are complete.",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Saving result-flag mapping failed:", conditionMessage(e)),
            type = "error",
            duration = 15
          )
        }
      )
    })

    review_rows <- reactive({
      mapping_revision()
      tryCatch(AquaCache::getImportMappingReview(con,include_history=identical(input$review_state,"history")),
        error=function(e) e)
    })
    output$review_summary <- renderUI({
      rows <- review_rows()
      if (inherits(rows,"error")) return(tags$div(class="alert alert-danger",conditionMessage(rows)))
      bad <- rows$active & !is.na(rows$issue) & nzchar(rows$issue)
      tags$div(class=if (any(bad & rows$status=="published")) "alert alert-warning" else "alert alert-info",
        sprintf("%s incomplete published mappings; %s incomplete draft mappings. %s source(s) have saved mappings.",
          sum(bad & rows$status=="published"),sum(bad & rows$status=="draft"),length(unique(rows$source_code))),
        tags$p("Only active mappings must be complete for publication. Deactivate an unwanted analyte with an explanatory note; retain retired revisions as history."))
    })
    filtered_review <- reactive({
      rows <- review_rows()
      if (inherits(rows,"error")) return(data.frame())
      source <- addDiscData_first(input$review_source,"")
      kind <- addDiscData_first(input$review_kind,"")
      if (nzchar(source)) rows <- rows[rows$source_code==source,,drop=FALSE]
      if (nzchar(kind)) rows <- rows[rows$mapping_type==kind,,drop=FALSE]
      if (addDiscData_first(input$review_state,"current") %in% c("published","draft")) rows <- rows[rows$status==input$review_state,,drop=FALSE]
      if (!isTRUE(input$review_inactive)) rows <- rows[rows$active,,drop=FALSE]
      if (isTRUE(input$needs_attention)) rows <- rows[rows$active & rows$status!="retired" &
        !is.na(rows$issue) & nzchar(rows$issue),,drop=FALSE]
      rows
    })
    output$mapping_review <- DT::renderDT({
      rows <- filtered_review()
      if (!nrow(rows)) return(DT::datatable(data.frame(Message="No saved mappings match these filters."),rownames=FALSE,options=list(dom="t")))
      display <- data.frame(Source=rows$source_code,Scope=ifelse(is.na(rows$profile_code),
        "Source-wide / package downloads",rows$profile_name),Type=rows$mapping_type,
        Criteria=rows$source_match,Target=rows$target,Issue=rows$issue,Revision=paste(rows$status,rows$version),
        Active=rows$active,`Source active`=rows$source_active,`Changed at`=ifelse(is.na(rows$modified),
          as.character(rows$created),as.character(rows$modified)),
        `Changed by`=ifelse(is.na(rows$modified_by),rows$created_by,rows$modified_by),Note=rows$note,check.names=FALSE)
      display[is.na(display)] <- ""
      DT::datatable(display,rownames=FALSE,selection="single",options=list(pageLength=10,scrollX=TRUE))
    },server=FALSE)
    observeEvent(input$open_review_mapping, {
      rows <- filtered_review()
      selected <- input$mapping_review_rows_selected
      req(length(selected)==1L,selected>=1L,selected<=nrow(rows))
      row <- rows[selected,,drop=FALSE]
      if (identical(row$status[[1]],"retired")) {
        showModal(modalDialog(title="Retired mapping revision — read only",size="l",
          tags$p(paste(row$source_code[[1]],if (is.na(row$profile_code[[1]]))
            "— source-wide / package downloads —" else row$profile_name[[1]],"revision",row$version[[1]])),
          tags$pre(as.character(jsonlite::prettify(row$definition[[1]]))),footer=modalButton("Close")))
        return()
      }
      clear_manager_mapping_selection()
      pending_open(row)
      opened_review_mapping(row)
      if (!isTRUE(as.logical(row$active[[1]]))) {
        updateCheckboxInput(session,"manage_show_inactive",value=TRUE)
      }
      updateSelectizeInput(session,"manage_source",selected=row$source_code[[1]])
      updateSelectizeInput(session,"manage_import_profile",selected=if (is.na(row$profile_code[[1]])) "" else
        addDiscData_profile_key(row$source_code[[1]],row$profile_code[[1]]))
      updateSelectInput(session,"manage_mapping_type",selected=row$mapping_type[[1]])
      updateTabsetPanel(session,"setup_tabs",selected="editor")
    })
    observe({
      pending <- pending_open()
      req(!is.null(pending),identical(input$manage_source,pending$source_code[[1]]),
        identical(input$manage_mapping_type,pending$mapping_type[[1]]))
      key <- if (is.na(pending$profile_code[[1]])) "" else
        addDiscData_profile_key(pending$source_code[[1]],pending$profile_code[[1]])
      req(identical(addDiscData_first(input$manage_import_profile,""),key))
      rows <- manage_mapping_rows()
      criteria <- if (identical(input$manage_mapping_type,"parameter")) rows$source_match else if (identical(input$manage_mapping_type,"location"))
        vapply(rows$source_location_code,function(x) as.character(jsonlite::toJSON(list(source_location_code=x),auto_unbox=TRUE)),character(1)) else
        vapply(seq_len(nrow(rows)),function(i) as.character(jsonlite::toJSON(list(source_flag_column=rows$source_flag_column[[i]],
          source_flag_value=rows$source_flag_value[[i]]),auto_unbox=TRUE,na="null")),character(1))
      wanted <- jsonlite::fromJSON(pending$source_match[[1]],simplifyVector=FALSE)
      matches <- vapply(criteria,function(value) {
        candidate <- jsonlite::fromJSON(value,simplifyVector=FALSE)
        identical(candidate[sort(names(candidate))],wanted[sort(names(wanted))])
      },logical(1))
      matches <- matches & if (is.na(pending$profile_code[[1]])) !rows$profile_specific else rows$profile_specific
      if (any(matches)) {
        mapping_index <- which(matches)[[1]]
        mapping_row <- rows[mapping_index,,drop=FALSE]
        load_manager_mapping(mapping_row)
        manager_selection_from_review(mapping_index)
        select_manager_mapping_row(mapping_index, mapping_row)
        shinyjs::runjs("window.scrollTo(0, 0);")
        pending_open(NULL)
      }
    })

    # Review the whole selected scope, including all three mapping types. The
    # fingerprint excludes labels/audit times but includes every saved setting.
    read_scope_review <- function(source,profile_code) {
      rows <- AquaCache::getImportMappingReview(con,source_code=source)
      rows <- rows[if (is.null(profile_code)) is.na(rows$profile_code) else
        !is.na(rows$profile_code) & rows$profile_code==profile_code,,drop=FALSE]
      rows <- rows[,c("import_mapping_set_id","import_source_id","import_profile_id","status","version",
        "mapping_type","source_match","target","active","issue","definition"),drop=FALSE]
      rownames(rows) <- NULL
      rows
    }
    output$publication_changes <- DT::renderDT({
      pending <- pending_publication()
      req(!is.null(pending))
      DT::datatable(pending$changes,rownames=FALSE,selection="none",
        options=list(pageLength=5,lengthChange=FALSE,scrollX=TRUE))
    },server=FALSE)
    observeEvent(input$manage_publish_mappings, {
      tryCatch({
        if (!can_manage_mappings) stop("Your database role cannot publish mappings.")
        profile <- selected_manage_profile()
        profile_code <- if (identical(input$manage_publish_scope,"profile")) profile$profile_code[[1]] else NULL
        if (!is.null(profile_code) && (is.na(profile_code) || !nzchar(profile_code))) stop("Select a workbook format first.")
        rows <- read_scope_review(profile$source_code[[1]],profile_code)
        if (!any(rows$status=="draft")) stop("This scope has no unpublished draft.")
        bad <- rows$status=="draft" & rows$active & !is.na(rows$issue) & nzchar(rows$issue)
        labels <- list(parameter_id=stats::setNames(params()$param_name,params()$parameter_id),
          sample_fraction_id=stats::setNames(sample_fractions$sample_fraction,sample_fractions$sample_fraction_id),
          reporting_basis_id=stats::setNames(reporting_bases$reporting_basis,reporting_bases$reporting_basis_id),
          matrix_state_id=stats::setNames(matrix_states$matrix_state_name,matrix_states$matrix_state_id),
          result_type=stats::setNames(result_types$result_type,result_types$result_type_id),
          result_value_type=stats::setNames(result_value_types$result_value_type,result_value_types$result_value_type_id),
          location_id=stats::setNames(locations()$name,locations()$location_id),
          result_condition_id=stats::setNames(result_conditions$result_condition,result_conditions$result_condition_id))
        changes <- importSetup_changes(rows,labels)
        pending_publication(list(source=profile$source_code[[1]],profile_code=profile_code,rows=rows,changes=changes))
        showModal(modalDialog(title="Review mapping publication",size="l",easyClose=FALSE,
          tags$p(paste("Source:",profile$source_code[[1]],"—",if (is.null(profile_code)) "source-wide / package downloads" else profile$profile_name[[1]])),
          tags$p("Publishing replaces the current snapshot for this scope. Retired revisions remain available as history. All mapping types in this scope are included."),
          if (any(bad)) tags$div(class="alert alert-danger",paste("Complete or deactivate these draft mappings before publication:",paste(unique(rows$issue[bad]),collapse="; "))),
          if (!nrow(changes)) tags$p("The draft has no definition changes from the current published revision."),
          DT::DTOutput(ns("publication_changes")),
          footer=tagList(actionButton(ns("cancel_publication"),"Cancel"),
            if (any(bad)) shinyjs::disabled(actionButton(ns("confirm_publication"),"Publish revision",class="btn-primary")) else
              actionButton(ns("confirm_publication"),"Publish revision",class="btn-primary"))))
      },error=function(e) showNotification(conditionMessage(e),type="error",duration=15))
    })
    observeEvent(input$cancel_publication,{pending_publication(NULL);removeModal()})
    observeEvent(input$confirm_publication, {
      tryCatch({
        if (!can_manage_mappings) stop("Your database role cannot publish mappings.")
        pending <- pending_publication()
        req(!is.null(pending))
        DBI::dbWithTransaction(con,{
          scope <- DBI::dbGetQuery(con,"SELECT s.import_source_id,p.import_profile_id,s.active
            FROM discrete.import_sources s LEFT JOIN discrete.import_profiles p
              ON p.import_source_id=s.import_source_id AND p.profile_code=$2
            WHERE s.source_code=$1",params=list(pending$source,
              if (is.null(pending$profile_code)) NA_character_ else pending$profile_code))
          if (nrow(scope)!=1L || !isTRUE(scope$active[[1]])) stop("The import source is missing or inactive.")
          scope_key <- paste(scope$import_source_id[[1]],if (is.null(pending$profile_code)) "source" else scope$import_profile_id[[1]],sep=":")
          DBI::dbGetQuery(con,"SELECT pg_advisory_xact_lock(hashtextextended($1,0))",params=list(scope_key))
          current <- read_scope_review(pending$source,pending$profile_code)
          if (!identical(current,pending$rows)) stop("The mapping revision changed after review. Cancel and review it again before publishing.")
          AquaCache::publishImportMappings(con,pending$source,profile_code=pending$profile_code)
        })
        pending_publication(NULL)
        removeModal()
        bump_revision()
        clear_manager_mapping_selection()
        showNotification("Mapping revision published. Reparse any open workbook to use it.",type="message")
      },error=function(e) showNotification(paste("Publication failed:",conditionMessage(e)),type="error",duration=15))
    })

  })
}
