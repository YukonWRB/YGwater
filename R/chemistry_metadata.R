# Analytical metadata shared by result entry, import mappings, and guideline
# editors. The controls use typed catalogues; provenance remains in existing
# laboratory, protocol, source, and calculation fields.
#' Fetch the analytical form and target catalogues
#'
#' Query the revised AquaCache chemistry catalogues for shared result, import
#' mapping, and guideline editors. Reporting bases and sample fractions are
#' supplied separately by the calling module.
#' @param con An open DBI connection to an AquaCache database with the revised
#'   Patch 65 schema.
#' @return A named list containing `result_speciations` and `result_targets`
#'   data.frames, ordered by their English labels. All catalogue columns and
#'   both active and retired entries are retained.
#' @details No data are modified. [chemistry_metadata_ui()] filters choices for
#'   new selections while retaining an existing retired selection for display.
#' @family analytical metadata helpers
#' @keywords internal
chemistry_metadata_catalogues <- function(con) {
  list(
    result_speciations = DBI::dbGetQuery(con, "SELECT * FROM discrete.result_speciations ORDER BY result_speciation"),
    result_targets = DBI::dbGetQuery(con, "SELECT * FROM discrete.result_targets ORDER BY result_target")
  )
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
#'   belong to the calling form. Use `YGwater:::chemistry_metadata_ui()` in
#'   application modules sourced outside the package namespace.
#' @family analytical metadata helpers
#' @keywords internal
chemistry_metadata_ui <- function(ns, prefix, catalogues = list(), value = list(), lang = "en") {
  french <- identical(lang, "fr")
  label <- function(en, fr) if (french) fr else en
  selected <- function(field) {
    x <- value[[field]]
    if (is.null(x) || !length(x) || is.na(x[[1]])) return(NULL)
    x[[1]]
  }
  choices <- function(table, field) {
    rows <- catalogues[[table]]
    if (is.null(rows) || !nrow(rows)) return(NULL)
    id <- paste0(field, "_id")
    rows <- rows[rows$active | rows[[id]] %in% selected(id), , drop = FALSE]
    labels <- rows[[field]]
    fr <- paste0(field, "_fr")
    if (french && fr %in% names(rows)) labels <- ifelse(is.na(rows[[fr]]), labels, rows[[fr]])
    stats::setNames(as.character(rows[[id]]), ifelse(rows$active, labels, paste0(labels, " (inactive)")))
  }
  raw_conditions <- value$analysis_conditions
  if (inherits(value, "data.frame") && length(raw_conditions)) raw_conditions <- raw_conditions[[1]]
  conditions <- AquaCache::normalizeAnalysisConditions(raw_conditions)
  context <- jsonlite::fromJSON(conditions, simplifyVector = FALSE)
  common <- c("mass_basis", "gas_basis", "reference_temperature_kelvin", "reference_pressure_kpa")
  additional <- context[setdiff(names(context), common)]
  additional <- AquaCache::normalizeAnalysisConditions(additional)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(3, shiny::selectizeInput(ns(paste0(prefix, "result_speciation_id")),
        label("Chemical form / species scope", "Forme chimique / especes"),
        choices = choices("result_speciations", "result_speciation"), selected = selected("result_speciation_id"),
        multiple = TRUE, options = list(maxItems = 1, placeholder = label("Unspecified", "Non precise")))),
      shiny::column(3, shiny::selectizeInput(ns(paste0(prefix, "result_target_id")),
        label("Analytical target / material", "Cible analytique / matiere"),
        choices = choices("result_targets", "result_target"), selected = selected("result_target_id"),
        multiple = TRUE, options = list(maxItems = 1, placeholder = label("Unspecified", "Non precise")))),
      shiny::column(3, shiny::selectInput(ns(paste0(prefix, "endpoint_mode")),
        label("Endpoint determination", "Determination du point final"),
        choices = c("Unspecified" = "", "Fixed pH" = "fixed", "Inflection" = "inflection", "Gran" = "gran", "Other" = "other"),
        selected = selected("endpoint_mode"))),
      shiny::column(3, shiny::numericInput(ns(paste0(prefix, "endpoint_ph")),
        label("Endpoint pH", "pH du point final"), value = if (is.null(selected("endpoint_ph"))) NA_real_ else selected("endpoint_ph")))
    ),
    shiny::fluidRow(
      shiny::column(3, shiny::selectInput(ns(paste0(prefix, "mass_basis")),
        label("Solid mass basis", "Base de masse solide"),
        choices = c("Unspecified" = "", "Dry" = "dry", "Wet" = "wet", "Ash" = "ash", "Other" = "other"),
        selected = context$mass_basis)),
      shiny::column(3, shiny::selectInput(ns(paste0(prefix, "gas_basis")),
        label("Gas moisture basis", "Base d'humidite du gaz"),
        choices = c("Unspecified" = "", "Dry" = "dry", "Wet" = "wet", "Other" = "other"), selected = context$gas_basis)),
      shiny::column(3, shiny::numericInput(ns(paste0(prefix, "reference_temperature_kelvin")),
        label("Reference temperature (K)", "Temperature de reference (K)"),
        value = if (is.null(context$reference_temperature_kelvin)) NA_real_ else context$reference_temperature_kelvin)),
      shiny::column(3, shiny::numericInput(ns(paste0(prefix, "reference_pressure_kpa")),
        label("Reference pressure (kPa)", "Pression de reference (kPa)"),
        value = if (is.null(context$reference_pressure_kpa)) NA_real_ else context$reference_pressure_kpa))
    ),
    shiny::tags$details(shiny::tags$summary(label("Additional analysis conditions", "Autres conditions analytiques")),
      shiny::textAreaInput(ns(paste0(prefix, "analysis_conditions")),
        label("Named conditions", "Conditions nommees"), value = additional, rows = 2, width = "100%"),
      shiny::helpText(label(
        'Additional conditions use a JSON object, for example {"isotope_reference_standard":"VSMOW"}. Use {} when unspecified. Laboratory and method belong in their own fields.',
        'Les autres conditions utilisent un objet JSON, par exemple {"isotope_reference_standard":"VSMOW"}. Utiliser {} si les conditions ne sont pas precisees.'))
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
#' @examples
#' values <- YGwater:::chemistry_metadata_values(
#'   list(lab_endpoint_mode = "fixed", lab_endpoint_ph = 4.5,
#'        lab_mass_basis = "dry", lab_analysis_conditions = "{}"),
#'   prefix = "lab_"
#' )
#' values$analysis_conditions
#' @family analytical metadata helpers
#' @keywords internal
chemistry_metadata_values <- function(input, prefix) {
  fields <- c("result_speciation_id", "result_target_id", "endpoint_mode", "endpoint_ph", "analysis_conditions")
  out <- stats::setNames(lapply(fields, function(field) input[[paste0(prefix, field)]]), fields)
  for (field in c("result_speciation_id", "result_target_id")) {
    value <- out[[field]]
    if (is.null(value) || !length(value) || (length(value) == 1L && (is.na(value) || !nzchar(value)))) {
      out[[field]] <- NA_integer_
    } else {
      number <- suppressWarnings(as.numeric(value))
      if (length(number) != 1L || !is.finite(number) || number < 1 || number != trunc(number) || number > .Machine$integer.max) {
        stop("Choose one valid ", field, ".")
      }
      out[[field]] <- as.integer(number)
    }
  }
  mode <- out$endpoint_mode
  out$endpoint_mode <- if (is.null(mode) || !length(mode) || !nzchar(mode[[1]])) NA_character_ else mode[[1]]
  ph <- out$endpoint_ph
  out$endpoint_ph <- if (is.null(ph) || !length(ph)) NA_real_ else ph[[1]]
  context <- jsonlite::fromJSON(AquaCache::normalizeAnalysisConditions(out$analysis_conditions), simplifyVector = FALSE)
  for (field in c("mass_basis", "gas_basis", "reference_temperature_kelvin", "reference_pressure_kpa")) {
    value <- input[[paste0(prefix, field)]]
    if (is.null(value)) next
    if (!length(value) || is.na(value[[1]]) || identical(value[[1]], "")) context[[field]] <- NULL else context[[field]] <- value[[1]]
  }
  out$analysis_conditions <- AquaCache::normalizeAnalysisConditions(context)
  out
}

# Columns used for averaging, trends, report rows, and import identity. Keep
# numeric context in these keys even when an interface does not display it.
#' Return analytical columns used to distinguish results
#'
#' Supply the chemistry portion of grouping keys for plots, report rows,
#' averaging, trends, and import identity.
#' @return A character vector containing `reporting_basis_id`,
#'   `result_speciation_id`, `result_target_id`, `endpoint_mode`, `endpoint_ph`,
#'   and `analysis_conditions_key`.
#' @details These columns complement the caller's sample, parameter, matrix,
#'   fraction, and result-type keys. `analysis_conditions_key` is the database's
#'   canonical conditions hash; context must remain in the key even when an
#'   interface does not display it. This function does not group or validate data.
#' @family analytical metadata helpers
#' @keywords internal
chemistry_identity_columns <- function() c("reporting_basis_id", "result_speciation_id",
  "result_target_id", "endpoint_mode", "endpoint_ph", "analysis_conditions_key")

#' Format analytical metadata as readable result labels
#'
#' Combine available reporting-basis, chemical-form, target, endpoint, and
#' analysis-condition labels for plots and report matrices.
#' @param rows A data.frame or data.table of results. Optional display columns
#'   are `reporting_basis`, `result_speciation`, `result_target`, `endpoint_mode`,
#'   `endpoint_ph`, and JSON `analysis_conditions`.
#' @return One plain-text character label per input row, joined with semicolons.
#'   Missing fields, empty values, and `{}` are omitted. A row without analytical
#'   metadata receives an empty string.
#' @details Labels describe metadata; they are not unique scientific keys. Use
#'   [chemistry_identity_columns()] for grouping, and escape labels before
#'   inserting them into HTML.
#' @examples
#' YGwater:::chemistry_result_labels(data.frame(
#'   reporting_basis = "as CaCO3",
#'   endpoint_mode = "fixed", endpoint_ph = c(4.5, 8.3)
#' ))
#' @family analytical metadata helpers
#' @keywords internal
chemistry_result_labels <- function(rows) {
  labels <- rep("", nrow(rows))
  for (field in c("reporting_basis", "result_speciation", "result_target", "endpoint_mode", "endpoint_ph", "analysis_conditions")) {
    if (!field %in% names(rows)) next
    values <- as.character(rows[[field]])
    present <- !is.na(values) & nzchar(values) & values != "{}"
    if (field == "endpoint_ph") values <- paste0("endpoint pH ", values)
    labels[present] <- ifelse(nzchar(labels[present]), paste0(labels[present], "; ", values[present]), values[present])
  }
  labels
}

# Stable references shared by SQL-scalar declarations and their Rule inputs.
# Include every scientific selector; equal parameter/basis IDs alone cannot
# establish that two inputs describe the same measurement.
#' Build comparable analytical selectors for guideline inputs
#'
#' Serialize guideline-input selectors consistently so SQL declarations can be
#' compared with the rule's declared inputs.
#' @param rows A data.frame or data.table with analytical selector columns:
#'   `parameter_id`, `matrix_state_id`, `sample_fraction_id`, `reporting_basis_id`,
#'   `result_speciation_id`, `result_target_id`, `endpoint_mode`, `endpoint_ph`,
#'   and `analysis_conditions`. Missing selectors default to `NA`, and missing
#'   conditions default to `{}`.
#' @return One JSON key per input row in input order; `character()` for no rows.
#'   IDs are stored as integers, endpoint pH as numeric, missing selectors as
#'   JSON null, and conditions as a normalized object.
#' @details Input rows must already have validated selectors. The key contains
#'   every scientific selector but excludes sample ID, which is supplied by the
#'   rule's evaluation context. Matching parameter/basis IDs alone is insufficient
#'   to establish equivalent inputs. These keys are separate from the database's
#'   `analysis_conditions_key` hash.
#' @family analytical metadata helpers
#' @keywords internal
chemistry_accessor_keys <- function(rows) {
  if (!nrow(rows)) return(character())
  fields <- c("parameter_id", "matrix_state_id", "sample_fraction_id", "reporting_basis_id",
    "result_speciation_id", "result_target_id", "endpoint_mode", "endpoint_ph", "analysis_conditions")
  rows <- as.data.frame(rows)
  for (field in setdiff(fields, names(rows))) rows[[field]] <- if (field == "analysis_conditions") "{}" else NA
  vapply(seq_len(nrow(rows)), function(i) {
    record <- as.list(rows[i, fields, drop = FALSE])
    for (field in c("parameter_id", "matrix_state_id", "sample_fraction_id", "reporting_basis_id", "result_speciation_id", "result_target_id")) {
      record[[field]] <- as.integer(record[[field]])
    }
    record$endpoint_ph <- as.numeric(record$endpoint_ph)
    record$analysis_conditions <- jsonlite::fromJSON(AquaCache::normalizeAnalysisConditions(record$analysis_conditions), simplifyVector = FALSE)
    as.character(jsonlite::toJSON(record, auto_unbox = TRUE, na = "null", null = "null", digits = NA))
  }, character(1))
}

# Scan literal selectors in supported get_sample_val calls. Quotes and comments
# are handled separately so commas in a conditions object do not split an
# argument, and text mentioning a function is not mistaken for a real call.
#' Extract literal analytical selectors from guideline SQL
#'
#' Scan supported `discrete.get_sample_val()` calls without executing SQL, and
#' return keys comparable with [chemistry_accessor_keys()].
#' @param sql A single SQL expression as a character string.
#' @return One JSON selector key per recognized accessor call in source order,
#'   or `character()` when no supported calls are found. Sample ID is excluded.
#' @details Quoted text, line comments, and block comments do not create accessor
#'   references. Positional arguments may precede named arguments using `:=` or
#'   `=>`. IDs, pH, endpoint mode, and conditions must be literals or NULL; trailing
#'   integer/int/numeric/text/jsonb casts are supported. JSON commas and escaped
#'   single quotes are handled within string literals. Unknown, repeated, or
#'   invalid selectors in recognized calls raise errors.
#'
#'   This is a limited selector scanner, not a general SQL parser or permission
#'   validator. Nested argument expressions are unsupported and may not be
#'   recognized. The caller must validate allowed SQL syntax and independently
#'   compare the extracted keys with declared guideline inputs.
#' @examples
#' rows <- data.frame(parameter_id = 1L, endpoint_mode = "fixed",
#'                    endpoint_ph = 4.5)
#' sql <- "SELECT discrete.get_sample_val($1, parameter_id => 1,
#'   endpoint_mode => 'fixed', endpoint_ph => 4.5)"
#' identical(unname(YGwater:::chemistry_sql_accessor_refs(sql)),
#'           YGwater:::chemistry_accessor_keys(rows))
#' @family analytical metadata helpers
#' @keywords internal
chemistry_sql_accessor_refs <- function(sql) {
  sql <- as.character(sql)
  tokens <- gregexpr("'(?:[^']|'')*'|--[^\\r\\n]*|/\\*(?s:.*?)\\*/", sql, perl = TRUE)
  pieces <- regmatches(sql, tokens)[[1]]
  positions <- tokens[[1]]
  lengths <- attr(positions, "match.length")
  if (length(pieces)) {
    comments <- startsWith(pieces, "--") | startsWith(pieces, "/*")
    pieces[comments] <- vapply(lengths[comments], function(n) strrep(" ", n), character(1))
    regmatches(sql, tokens) <- list(pieces)
  }
  quoted <- if (length(pieces)) positions[startsWith(pieces, "'")] else integer()
  quote_lengths <- if (length(pieces)) lengths[startsWith(pieces, "'")] else integer()
  hits <- gregexpr("(?i)\\bdiscrete\\.get_sample_val\\s*\\((?:'(?:[^']|'')*'|[^'()])*\\)", sql, perl = TRUE)
  calls <- regmatches(sql, hits)[[1]]
  if (!length(calls)) return(character())
  starts <- hits[[1]]
  calls <- calls[!vapply(starts, function(pos) any(pos >= quoted & pos < quoted + quote_lengths), logical(1))]
  fields <- c("sample_id", "parameter_id", "sample_fraction_id", "reporting_basis_id",
    "result_speciation_id", "result_target_id", "endpoint_mode", "endpoint_ph", "analysis_conditions", "matrix_state_id")
  vapply(calls, function(call) {
    args <- sub("(?s)^.*?\\((.*)\\)$", "\\1", call, perl = TRUE)
    chars <- strsplit(args, "", fixed = TRUE)[[1]]
    quote <- FALSE
    separators <- integer()
    i <- 1L
    while (i <= length(chars)) {
      if (chars[[i]] == "'") {
        if (quote && i < length(chars) && chars[[i + 1L]] == "'") {
          i <- i + 2L
          next
        }
        quote <- !quote
      } else if (!quote && chars[[i]] == ",") separators <- c(separators, i)
      i <- i + 1L
    }
    begins <- c(1L, separators + 1L)
    ends <- c(separators - 1L, length(chars))
    parts <- trimws(vapply(seq_along(begins), function(j) paste0(chars[begins[[j]]:ends[[j]]], collapse = ""), character(1)))
    values <- stats::setNames(rep("NULL", length(fields)), fields)
    assigned <- character()
    named_seen <- FALSE
    for (j in seq_along(parts)) {
      part <- parts[[j]]
      named <- grepl("^[a-z_]+\\s*(?::=|=>)", part, perl = TRUE, ignore.case = TRUE)
      if (named) {
        named_seen <- TRUE
        field <- tolower(sub("^([a-z_]+)\\s*(?::=|=>).*$", "\\1", part, perl = TRUE, ignore.case = TRUE))
        value <- sub("^[a-z_]+\\s*(?::=|=>)\\s*", "", part, perl = TRUE, ignore.case = TRUE)
      } else {
        if (named_seen || j > length(fields)) stop("Review sample accessor argument order.")
        field <- fields[[j]]
        value <- part
      }
      if (!field %in% fields || field %in% assigned) stop("Unknown or repeated sample accessor argument: ", field)
      assigned <- c(assigned, field)
      values[[field]] <- trimws(sub("::\\s*(integer|int|numeric|text|jsonb)\\s*$", "", value, perl = TRUE, ignore.case = TRUE))
    }
    record <- list()
    for (field in fields[-1L]) {
      literal <- values[[field]]
      if (grepl("^NULL$", literal, ignore.case = TRUE)) {
        record[[field]] <- if (field == "analysis_conditions") "{}" else NA
      } else if (field %in% c("endpoint_mode", "analysis_conditions")) {
        if (!grepl("^'(?:[^']|'')*'$", literal, perl = TRUE)) stop("Use a literal ", field, " in a sample accessor.")
        record[[field]] <- gsub("''", "'", substr(literal, 2L, nchar(literal) - 1L), fixed = TRUE)
      } else {
        number <- suppressWarnings(as.numeric(literal))
        if (!is.finite(number) || (field != "endpoint_ph" && (number < 1 || number != trunc(number)))) stop("Use a literal valid ", field, " in a sample accessor.")
        record[[field]] <- number
      }
    }
    chemistry_accessor_keys(as.data.frame(record, stringsAsFactors = FALSE))[[1]]
  }, character(1))
}
