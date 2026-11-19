# Build catalogue choices in display order, retaining a saved inactive choice.
# Suggested choices with the same rank keep the catalogue's alphabetical order.
chemistry_catalogue_choices <- function(
  rows,
  field,
  selected = NULL,
  lang = "en",
  suggestions = NULL,
  active_only = TRUE
) {
  if (is.null(rows) || !nrow(rows)) {
    return(character())
  }
  id <- paste0(field, "_id")
  if (active_only) {
    rows <- rows[rows$active | rows[[id]] %in% selected, , drop = FALSE]
  }
  if (!is.null(suggestions) && nrow(suggestions)) {
    rank <- suggestions$display_order[match(rows[[id]], suggestions$choice_id)]
    rows <- rows[order(rank), , drop = FALSE]
  }
  labels <- rows[[field]]
  fr <- paste0(field, "_fr")
  if (identical(lang, "fr") && fr %in% names(rows)) {
    labels <- ifelse(is.na(rows[[fr]]), labels, rows[[fr]])
  }
  if (active_only) {
    labels <- ifelse(rows$active, labels, paste0(labels, " (inactive)"))
  }
  stats::setNames(as.character(rows[[id]]), labels)
}

# Fetch advisory ranks together for reuse while editing several mappings.
mapping_chemistry_suggestions <- function(con) {
  DBI::dbGetQuery(
    con,
    "SELECT 'reporting_basis' AS field, parameter_id,
            reporting_basis_id AS choice_id, display_order
       FROM public.parameter_reporting_basis_suggestions
     UNION ALL
     SELECT 'result_speciation', parameter_id, result_speciation_id, display_order
       FROM public.parameter_speciation_suggestions
     UNION ALL
     SELECT 'result_target', parameter_id, result_target_id, display_order
       FROM public.parameter_target_suggestions"
  )
}

# Refresh only the controls that depend on the target parameter. Retain the
# current values, including saved inactive forms and targets, as choices move.
update_mapping_chemistry <- function(
  session,
  prefix,
  parameter,
  matrix_code,
  catalogues,
  reporting_bases,
  suggestions,
  reporting_basis_input = paste0(prefix, "reporting_basis")
) {
  has_parameter <- isTRUE(!is.na(parameter$parameter_id))
  label <- function(text, required) {
    if (has_parameter) {
      paste0(text, if (isTRUE(required)) " (mandatory)" else " (optional)")
    } else {
      text
    }
  }
  suggestions <- suggestions[
    suggestions$parameter_id %in% parameter$parameter_id,
    ,
    drop = FALSE
  ]
  update_catalogue <- function(
    input_id,
    rows,
    field,
    text,
    allow_none = FALSE
  ) {
    selected <- as.character(session$input[[input_id]])
    choices <- chemistry_catalogue_choices(
      rows,
      field,
      selected,
      suggestions = suggestions[suggestions$field == field, , drop = FALSE],
      active_only = !allow_none
    )
    if (allow_none) {
      choices <- c("None" = "", choices)
    }
    shiny::updateSelectizeInput(
      session,
      input_id,
      label = label(text, parameter[[field]] %in% TRUE),
      choices = choices,
      selected = selected
    )
  }
  shiny::updateSelectizeInput(
    session,
    paste0(prefix, "sample_fraction"),
    label = label("Sample fraction", parameter$sample_fraction %in% TRUE)
  )
  update_catalogue(
    reporting_basis_input,
    reporting_bases,
    "reporting_basis",
    "Reporting basis",
    allow_none = TRUE
  )
  update_catalogue(
    paste0(prefix, "chem_result_speciation_id"),
    catalogues$result_speciations,
    "result_speciation",
    "Chemical form / species scope"
  )
  update_catalogue(
    paste0(prefix, "chem_result_target_id"),
    catalogues$result_targets,
    "result_target",
    "Analytical target / material"
  )
  shiny::updateSelectInput(
    session,
    paste0(prefix, "chem_endpoint_mode"),
    label = label("Endpoint determination", parameter$endpoint %in% TRUE)
  )

  # Parameter-wide and matrix-specific keys are additive in AquaCache.
  condition_keys <- parameter$analysis_conditions_required_keys[[1L]]
  matrix_requirements <- parameter$analysis_conditions_required_keys_by_matrix[[
    1L
  ]]
  if (
    is.character(condition_keys) &&
      length(condition_keys) == 1L &&
      !is.na(condition_keys) &&
      startsWith(condition_keys, "[")
  ) {
    condition_keys <- jsonlite::fromJSON(condition_keys)
  }
  if (is.character(matrix_requirements) && !is.na(matrix_requirements)) {
    matrix_requirements <- jsonlite::fromJSON(
      matrix_requirements,
      simplifyVector = FALSE
    )
  }
  if (is.list(matrix_requirements) && isTRUE(!is.na(matrix_code))) {
    condition_keys <- c(condition_keys, matrix_requirements[[matrix_code]])
  }
  condition_keys <- unique(unlist(condition_keys, use.names = FALSE))
  common <- c(
    mass_basis = "Solid mass basis",
    gas_basis = "Gas moisture basis",
    reference_temperature_kelvin = "Reference temperature (K)",
    reference_pressure_kpa = "Reference pressure (kPa)"
  )
  for (field in names(common)) {
    input_id <- paste0(prefix, "chem_", field)
    field_label <- label(common[[field]], field %in% condition_keys)
    if (field %in% c("mass_basis", "gas_basis")) {
      shiny::updateSelectInput(session, input_id, label = field_label)
    } else {
      shiny::updateNumericInput(session, input_id, label = field_label)
    }
  }
  additional <- setdiff(condition_keys, names(common))
  additional_label <- label("Named conditions", length(additional) > 0L)
  shiny::updateTextAreaInput(
    session,
    paste0(prefix, "chem_analysis_conditions"),
    label = if (length(additional)) {
      paste0(additional_label, " [", paste(additional, collapse = ", "), "]")
    } else {
      additional_label
    }
  )
  invisible(NULL)
}

#' Build shared analytical metadata controls
#'
#' Render chemical form, analytical target, endpoint, solid/gas basis, reference
#' temperature/pressure, and additional-condition inputs for AquaLink editors.
#' @param ns A Shiny namespace function, normally `session$ns` or `shiny::NS(id)`.
#' @param prefix A character prefix prepended to each field name before applying
#'   `ns`. Pass the same prefix to [chemistry_metadata_values()].
#' @param catalogues A list from [chemistry_metadata_catalogues()]. An empty list
#'   renders controls without chemical-form or analytical-target choices.
#' @param value A named list or single-row data.frame of existing metadata.
#'   Missing fields are displayed as unspecified. `analysis_conditions` may be a
#'   JSON object string or a named list accepted by
#'   [AquaCache::normalizeAnalysisConditions()].
#' @param lang Label language: `"en"` or `"fr"`.
#' @return A Shiny tag list. Input IDs are `ns(paste0(prefix, field))`, where
#'   `field` is `result_speciation_id`, `result_target_id`, `endpoint_mode`,
#'   `endpoint_ph`, `mass_basis`, `gas_basis`, `reference_temperature_kelvin`,
#'   `reference_pressure_kpa`, or `analysis_conditions`.
#' @details Active catalogue entries are offered for new selections. An existing
#'   retired entry remains visible and is labelled inactive. Common condition
#'   keys use separate controls; the JSON text area contains only additional
#'   keys. Reporting basis, sample fraction, laboratory, and method controls
#'   belong to the calling form.
#' @noRd
#' @keywords internal
chemistry_metadata_ui <- function(
  ns,
  prefix,
  catalogues = list(),
  value = list(),
  lang = "en"
) {
  french <- identical(lang, "fr")
  label <- function(en, fr) if (french) fr else en
  selected <- function(field) {
    x <- value[[field]]
    if (is.null(x) || !length(x) || is.na(x[[1]])) {
      return(NULL)
    }
    x[[1]]
  }
  raw_conditions <- value$analysis_conditions
  if (inherits(value, "data.frame") && length(raw_conditions)) {
    raw_conditions <- raw_conditions[[1]]
  }
  conditions <- AquaCache::normalizeAnalysisConditions(raw_conditions)
  context <- jsonlite::fromJSON(conditions, simplifyVector = FALSE)
  common <- c(
    "mass_basis",
    "gas_basis",
    "reference_temperature_kelvin",
    "reference_pressure_kpa"
  )
  additional <- context[setdiff(names(context), common)]
  additional <- AquaCache::normalizeAnalysisConditions(additional)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::selectizeInput(
          ns(paste0(prefix, "result_speciation_id")),
          label("Chemical form / species scope", "Forme chimique / especes"),
          choices = chemistry_catalogue_choices(
            catalogues$result_speciations,
            "result_speciation",
            selected("result_speciation_id"),
            lang
          ),
          selected = selected("result_speciation_id"),
          multiple = TRUE,
          options = list(
            maxItems = 1,
            placeholder = label("Unspecified", "Non precise")
          )
        )
      ),
      shiny::column(
        3,
        shiny::selectizeInput(
          ns(paste0(prefix, "result_target_id")),
          label("Analytical target / material", "Cible analytique / matiere"),
          choices = chemistry_catalogue_choices(
            catalogues$result_targets,
            "result_target",
            selected("result_target_id"),
            lang
          ),
          selected = selected("result_target_id"),
          multiple = TRUE,
          options = list(
            maxItems = 1,
            placeholder = label("Unspecified", "Non precise")
          )
        )
      ),
      shiny::column(
        3,
        shiny::selectInput(
          ns(paste0(prefix, "endpoint_mode")),
          label("Endpoint determination", "Determination du point final"),
          choices = c(
            "Unspecified" = "",
            stats::setNames(
              "unknown",
              label("Unknown / not reported", "Inconnu / non indique")
            ),
            "Fixed pH" = "fixed",
            "Inflection" = "inflection",
            "Gran" = "gran",
            "Other" = "other"
          ),
          selected = selected("endpoint_mode")
        )
      ),
      shiny::column(
        3,
        shiny::numericInput(
          ns(paste0(prefix, "endpoint_ph")),
          label("Endpoint pH", "pH du point final"),
          value = if (is.null(selected("endpoint_ph"))) {
            NA_real_
          } else {
            selected("endpoint_ph")
          }
        )
      )
    ),
    shiny::helpText(label(
      "Fixed endpoints require a pH. Use Unknown / not reported when the source does not state the endpoint method, and leave the pH blank.",
      "Un point final fixe exige un pH. Utiliser Inconnu / non indique si la methode n'est pas precisee et laisser le pH vide."
    )),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::selectInput(
          ns(paste0(prefix, "mass_basis")),
          label("Solid mass basis", "Base de masse solide"),
          choices = c(
            "Unspecified" = "",
            "Dry" = "dry",
            "Wet" = "wet",
            "Ash" = "ash",
            "Other" = "other"
          ),
          selected = context$mass_basis
        )
      ),
      shiny::column(
        3,
        shiny::selectInput(
          ns(paste0(prefix, "gas_basis")),
          label("Gas moisture basis", "Base d'humidite du gaz"),
          choices = c(
            "Unspecified" = "",
            "Dry" = "dry",
            "Wet" = "wet",
            "Other" = "other"
          ),
          selected = context$gas_basis
        )
      ),
      shiny::column(
        3,
        shiny::numericInput(
          ns(paste0(prefix, "reference_temperature_kelvin")),
          label("Reference temperature (K)", "Temperature de reference (K)"),
          value = if (is.null(context$reference_temperature_kelvin)) {
            NA_real_
          } else {
            context$reference_temperature_kelvin
          }
        )
      ),
      shiny::column(
        3,
        shiny::numericInput(
          ns(paste0(prefix, "reference_pressure_kpa")),
          label("Reference pressure (kPa)", "Pression de reference (kPa)"),
          value = if (is.null(context$reference_pressure_kpa)) {
            NA_real_
          } else {
            context$reference_pressure_kpa
          }
        )
      )
    ),
    shiny::tags$details(
      shiny::tags$summary(label(
        "Additional analysis conditions",
        "Autres conditions analytiques"
      )),
      shiny::textAreaInput(
        ns(paste0(prefix, "analysis_conditions")),
        label("Named conditions", "Conditions nommees"),
        value = additional,
        rows = 2,
        width = "100%"
      ),
      shiny::helpText(label(
        'Additional conditions use a JSON object, for example {"isotope_reference_standard":"VSMOW"}. Use {} when unspecified. Laboratory and method belong in their own fields.',
        'Les autres conditions utilisent un objet JSON, par exemple {"isotope_reference_standard":"VSMOW"}. Utiliser {} si les conditions ne sont pas precisees.'
      ))
    )
  )
}

#' Collect analytical metadata from shared Shiny controls
#'
#' Read inputs produced by [chemistry_metadata_ui()] and combine common
#' scientific conditions with the additional-condition JSON object.
#' @param input A Shiny module input object or named list of input values.
#' @param prefix The prefix passed to [chemistry_metadata_ui()]. Module input
#'   names are already stripped of the Shiny namespace; do not apply `ns` again.
#' @return A named list containing integer `result_speciation_id` and
#'   `result_target_id`, character `endpoint_mode`, numeric `endpoint_ph`, and
#'   normalized
#'   JSON `analysis_conditions`. Unspecified IDs, mode, and pH become `NA`.
#'   Unspecified conditions become `{}`.
#' @details Catalogue selectors must contain one positive integer ID. Common
#'   condition controls override the corresponding additional JSON keys; blank
#'   controls remove those keys. Invalid IDs or conditions raise an error.
#'   Endpoint combinations and parameter-specific requirements must be checked
#'   separately with [AquaCache::validateResultChemistry()] before writing.
#' @noRd
#' @keywords internal
chemistry_metadata_values <- function(input, prefix) {
  fields <- c(
    "result_speciation_id",
    "result_target_id",
    "endpoint_mode",
    "endpoint_ph",
    "analysis_conditions"
  )
  out <- stats::setNames(
    lapply(fields, function(field) input[[paste0(prefix, field)]]),
    fields
  )
  for (field in c("result_speciation_id", "result_target_id")) {
    value <- out[[field]]
    if (
      is.null(value) ||
        !length(value) ||
        (length(value) == 1L && (is.na(value) || !nzchar(value)))
    ) {
      out[[field]] <- NA_integer_
    } else {
      number <- suppressWarnings(as.numeric(value))
      if (
        length(number) != 1L ||
          !is.finite(number) ||
          number < 1 ||
          number != trunc(number) ||
          number > .Machine$integer.max
      ) {
        stop("Choose one valid ", field, ".")
      }
      out[[field]] <- as.integer(number)
    }
  }
  mode <- out$endpoint_mode
  out$endpoint_mode <- if (
    is.null(mode) || !length(mode) || !nzchar(mode[[1]])
  ) {
    NA_character_
  } else {
    mode[[1]]
  }
  ph <- out$endpoint_ph
  out$endpoint_ph <- if (is.null(ph) || !length(ph)) NA_real_ else ph[[1]]
  context <- jsonlite::fromJSON(
    AquaCache::normalizeAnalysisConditions(out$analysis_conditions),
    simplifyVector = FALSE
  )
  for (field in c(
    "mass_basis",
    "gas_basis",
    "reference_temperature_kelvin",
    "reference_pressure_kpa"
  )) {
    value <- input[[paste0(prefix, field)]]
    if (is.null(value)) {
      next
    }
    if (!length(value) || is.na(value[[1]]) || identical(value[[1]], "")) {
      context[[field]] <- NULL
    } else {
      context[[field]] <- value[[1]]
    }
  }
  out$analysis_conditions <- AquaCache::normalizeAnalysisConditions(context)
  out
}

# Columns used for averaging, trends, report rows, and import identity. Keep
# numeric context in these keys even when an interface does not display it.
