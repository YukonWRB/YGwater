#' Create an AquaCache water quality report
#'
#' @description
#' Creates an Excel report from AquaCache discrete results. The report
#' worksheet layout can place dates, locations, or parameters on separate tabs;
#' filterable result details and a guideline audit sheet follow the report
#' tabs. When requested, a separate HTML map is written for report locations.
#'
#' @details
#' Approximate sample selection is performed separately for each target date
#' and location. If this selects one sample for multiple dates, the function
#' warns and reports the reuse count; each result detail row includes both its
#' requested and actual sample dates. Optional matrix-state and sample-fraction
#' filters are applied when finding the closest eligible sample, then retained
#' in the report matrix and result details. Result speciation is shown in the
#' report whenever it is recorded for a measurement.
#'
#' @param date One or more target sample dates, supplied as `Date` values or
#'   `YYYY-MM-DD` strings. The report layout controls whether dates are written
#'   to separate worksheets or to columns.
#' @param location_ids AquaCache location IDs to include.
#' @param parameter_ids AquaCache parameter IDs to include.
#' @param output_path Full path for the `.xlsx` output file. Parent directories
#'   are created when needed.
#' @param guideline_ids Optional AquaCache guideline IDs to evaluate. Only
#'   active, approved guideline versions valid on each sample date are applied.
#' @param date_approx Maximum days before or after each target date to search
#'   when a location has no eligible sample on that date. Supply one value to
#'   use it for all dates, or one value per `date`. The closest sample date is
#'   selected for each location and target date; ties prefer the later date.
#' @param include_blanks Include samples whose sample type contains "blank".
#' @param include_duplicates Include samples whose sample type contains
#'   "duplicate" or "replicate".
#' @param sd_multiplier Optional number of sample standard deviations from the
#'   mean used to flag unusually high or low results. Only numeric result values
#'   are used; censored and missing results are excluded. Groups are calculated
#'   separately by location, parameter, matrix, fraction, and speciation.
#' @param sd_start Optional first date included in the SD calculation.
#' @param sd_end Optional last date included in the SD calculation.
#' @param sd_day_of_year Optional day-of-year values included in the SD
#'   calculation, from 1 to 366.
#' @param include_map Write an HTML map of locations with report results.
#' @param format Workbook layout. One of `"by_date"` (locations as columns,
#'   parameters as rows, one worksheet per date), `"by_location"` (parameters
#'   as rows, dates as columns, one worksheet per location), or
#'   `"by_parameter"` (locations as rows, dates as columns, one worksheet per
#'   parameter).
#' @param map_path Optional full path for the map HTML. Defaults to a sibling
#'   file named from `output_path`.
#' @param lang Language for location and parameter labels (`"en"` or `"fr"`).
#' @param con Optional AquaCache DBI connection. A connection created by this
#'   function is closed on exit; a caller-supplied connection remains open.
#' @param matrix_state_ids Optional matrix-state IDs to include. `NULL` or an
#'   empty vector includes every matrix state.
#' @param sample_fraction_ids Optional sample-fraction IDs to include. `NULL` or
#'   an empty vector includes every sample fraction.
#'
#' @return An invisible list with `xlsx_path`, `map_path`, `map_assets_path`
#'   (when a non-self-contained map is written), the number of included result
#'   rows, and `reused_sample_count` for samples selected for multiple target
#'   dates.
#' @export
#'
#' @examples
#' \dontrun{
#' AquaCacheReport(
#'   date = "2026-09-07",
#'   location_ids = c(101L, 102L),
#'   parameter_ids = c(15L, 16L),
#'   guideline_ids = c(3L, 4L),
#'   output_path = file.path(tempdir(), "water-quality-report.xlsx"),
#'   include_map = TRUE
#' )
#' }
AquaCacheReport <- function(
  date,
  location_ids,
  parameter_ids,
  output_path,
  guideline_ids = NULL,
  date_approx = 0L,
  include_blanks = FALSE,
  include_duplicates = TRUE,
  sd_multiplier = NULL,
  sd_start = NULL,
  sd_end = NULL,
  sd_day_of_year = NULL,
  include_map = FALSE,
  map_path = NULL,
  lang = c("en", "fr"),
  con = NULL,
  format = c("by_date", "by_location", "by_parameter"),
  matrix_state_ids = NULL,
  sample_fraction_ids = NULL
) {
  lang <- match.arg(lang)
  report_format <- match.arg(format)
  if (inherits(date, "Date")) {
    date <- as.Date(date)
  } else if (is.character(date) && length(date) && !anyNA(date)) {
    date <- tryCatch(as.Date(date), error = function(e) as.Date(rep(NA, length(date))))
  } else {
    stop("'date' must contain Date values or YYYY-MM-DD strings.", call. = FALSE)
  }
  if (!length(date) || anyNA(date)) {
    stop("'date' must contain one or more valid dates.", call. = FALSE)
  }
  if (anyDuplicated(date)) {
    stop("'date' must not contain duplicate dates.", call. = FALSE)
  }
  validate_ids <- function(x, name, optional = FALSE) {
    if (optional && (is.null(x) || length(x) == 0L)) return(integer())
    if (is.character(x) && !anyNA(x) && all(grepl("^[0-9]+$", x))) {
      x <- suppressWarnings(as.numeric(x))
    }
    if (!is.numeric(x) || length(x) == 0L || anyNA(x) ||
        any(!is.finite(x)) || any(x < 1) || any(x != trunc(x)) ||
        any(x > .Machine$integer.max)) {
      stop("'", name, "' must contain positive integer IDs.", call. = FALSE)
    }
    unique(as.integer(x))
  }
  location_ids <- validate_ids(location_ids, "location_ids")
  parameter_ids <- validate_ids(parameter_ids, "parameter_ids")
  matrix_state_ids <- validate_ids(
    matrix_state_ids, "matrix_state_ids", optional = TRUE
  )
  sample_fraction_ids <- validate_ids(
    sample_fraction_ids, "sample_fraction_ids", optional = TRUE
  )
  guideline_ids <- validate_ids(guideline_ids, "guideline_ids", optional = TRUE)
  if (!is.numeric(date_approx) || !length(date_approx) ||
      anyNA(date_approx) || any(!is.finite(date_approx)) ||
      any(date_approx < 0) || any(date_approx > .Machine$integer.max) ||
      any(date_approx != trunc(date_approx)) ||
      !(length(date_approx) %in% c(1L, length(date)))) {
    stop("'date_approx' must be one non-negative integer or one per date.", call. = FALSE)
  }
  date_approx <- rep(as.integer(date_approx), length.out = length(date))
  for (flag in c("include_blanks", "include_duplicates", "include_map")) {
    value <- get(flag)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop("'", flag, "' must be TRUE or FALSE.", call. = FALSE)
    }
  }
  if (!is.null(sd_multiplier) &&
      (!is.numeric(sd_multiplier) || length(sd_multiplier) != 1L ||
       is.na(sd_multiplier) || !is.finite(sd_multiplier) || sd_multiplier < 0)) {
    stop("'sd_multiplier' must be NULL or one non-negative number.", call. = FALSE)
  }
  parse_optional_date <- function(x, name) {
    if (is.null(x)) return(NULL)
    if (inherits(x, "Date") && length(x) == 1L && !is.na(x)) return(x)
    if (!is.character(x) || length(x) != 1L) {
      stop("'", name, "' must be NULL or one Date/YYYY-MM-DD value.", call. = FALSE)
    }
    value <- tryCatch(as.Date(x), error = function(e) as.Date(NA))
    if (is.na(value)) stop("'", name, "' is not a valid date.", call. = FALSE)
    value
  }
  sd_start <- parse_optional_date(sd_start, "sd_start")
  sd_end <- parse_optional_date(sd_end, "sd_end")
  if (!is.null(sd_start) && !is.null(sd_end) && sd_start > sd_end) {
    stop("'sd_start' must be on or before 'sd_end'.", call. = FALSE)
  }
  if (!is.null(sd_day_of_year) &&
      (!is.numeric(sd_day_of_year) || length(sd_day_of_year) == 0L ||
       anyNA(sd_day_of_year) || any(sd_day_of_year < 1 | sd_day_of_year > 366) ||
       any(sd_day_of_year != trunc(sd_day_of_year)))) {
    stop("'sd_day_of_year' must contain integers from 1 to 366.", call. = FALSE)
  }
  if (!is.character(output_path) || length(output_path) != 1L ||
      is.na(output_path) || !nzchar(output_path) ||
      !grepl("\\.xlsx$", output_path, ignore.case = TRUE)) {
    stop("'output_path' must be a full path ending in .xlsx.", call. = FALSE)
  }
  if (!is.null(map_path) &&
      (!is.character(map_path) || length(map_path) != 1L || is.na(map_path) ||
       !nzchar(map_path) || !grepl("\\.html?$", map_path, ignore.case = TRUE))) {
    stop("'map_path' must be NULL or a path ending in .html/.htm.", call. = FALSE)
  }
  if (!include_map && !is.null(map_path)) {
    stop("Set 'include_map = TRUE' when supplying 'map_path'.", call. = FALSE)
  }
  if (include_map) {
    rlang::check_installed(c("leaflet", "htmlwidgets", "htmltools"))
    if (is.null(map_path)) {
      map_path <- paste0(tools::file_path_sans_ext(output_path), "_locations.html")
    }
  }
  rlang::check_installed("jsonlite")

  made_connection <- is.null(con)
  if (made_connection) {
    con <- AquaConnect(silent = TRUE)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  } else if (!DBI::dbIsValid(con)) {
    stop("The supplied AquaCache connection is not valid.", call. = FALSE)
  }

  id_json <- function(x) as.character(jsonlite::toJSON(
    as.character(x), auto_unbox = FALSE
  ))
  location_json <- id_json(location_ids)
  parameter_json <- id_json(parameter_ids)
  matrix_state_json <- id_json(matrix_state_ids)
  sample_fraction_json <- id_json(sample_fraction_ids)
  date_request_json <- as.character(jsonlite::toJSON(
    data.frame(
      requested_date = format(date, "%Y-%m-%d"),
      date_approx = date_approx
    ),
    dataframe = "rows",
    auto_unbox = TRUE
  ))
  requested_locations <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT l.location_id, l.location_code FROM public.locations l ",
      "WHERE l.location_id IN (SELECT value::integer ",
      "FROM jsonb_array_elements_text($1::jsonb))"
    ),
    params = list(location_json)
  )
  if (nrow(requested_locations) == 0L) {
    stop("None of the supplied location IDs were found or visible.", call. = FALSE)
  }
  missing_locations <- setdiff(location_ids, requested_locations$location_id)
  if (length(missing_locations)) {
    warning(
      "Location IDs not found or visible: ", paste(missing_locations, collapse = ", "),
      call. = FALSE
    )
  }
  requested_parameters <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT parameter_id, param_name, param_name_fr FROM public.parameters ",
      "WHERE parameter_id IN (SELECT value::integer ",
      "FROM jsonb_array_elements_text($1::jsonb))"
    ),
    params = list(parameter_json)
  )
  if (nrow(requested_parameters) == 0L) {
    stop("None of the supplied parameter IDs were found or visible.", call. = FALSE)
  }
  missing_parameters <- setdiff(parameter_ids, requested_parameters$parameter_id)
  if (length(missing_parameters)) {
    warning(
      "Parameter IDs not found or visible: ", paste(missing_parameters, collapse = ", "),
      call. = FALSE
    )
  }

  unit_sql <- ac_parameter_unit_select_sql(
    con,
    parameter_alias = "p",
    output_alias = "units",
    matrix_state_alias = "r"
  )
  result_sql <- paste0(
    "WITH date_requests AS (\n",
    "  SELECT requested_date, date_approx\n",
    "  FROM jsonb_to_recordset($3::jsonb) AS d(requested_date date, date_approx integer)\n",
    "), candidates AS (\n",
    "  SELECT d.requested_date, d.date_approx, s.location_id,\n",
    "    s.datetime::date AS sample_date\n",
    "  FROM date_requests d\n",
    "  JOIN discrete.samples s ON s.datetime::date BETWEEN\n",
    "    d.requested_date - d.date_approx AND d.requested_date + d.date_approx\n",
    "  LEFT JOIN discrete.sample_types st ON st.sample_type_id = s.sample_type\n",
    "  WHERE s.location_id IN (SELECT value::integer FROM jsonb_array_elements_text($1::jsonb))\n",
    "    AND ($4::boolean OR st.sample_type IS NULL OR st.sample_type !~* 'blank')\n",
    "    AND ($5::boolean OR st.sample_type IS NULL OR st.sample_type !~* '(duplicate|replicate)')\n",
    "    AND EXISTS (SELECT 1 FROM discrete.results er\n",
    "      WHERE er.sample_id = s.sample_id\n",
    "        AND er.parameter_id IN (SELECT value::integer FROM jsonb_array_elements_text($2::jsonb))\n",
    "        AND (jsonb_array_length($6::jsonb) = 0 OR er.matrix_state_id IN\n",
    "          (SELECT value::integer FROM jsonb_array_elements_text($6::jsonb)))\n",
    "        AND (jsonb_array_length($7::jsonb) = 0 OR er.sample_fraction_id IN\n",
    "          (SELECT value::integer FROM jsonb_array_elements_text($7::jsonb))))\n",
    "), best_dates AS (\n",
    "  SELECT DISTINCT ON (requested_date, location_id)\n",
    "    requested_date, date_approx, location_id, sample_date\n",
    "  FROM candidates\n",
    "  ORDER BY requested_date, location_id,\n",
    "    abs(sample_date - requested_date),\n",
    "    (sample_date >= requested_date) DESC, sample_date\n",
    "), selected_samples AS (\n",
    "  SELECT s.*, b.requested_date, b.date_approx FROM discrete.samples s\n",
    "  JOIN best_dates b ON b.location_id = s.location_id\n",
    "    AND b.sample_date = s.datetime::date\n",
    "  LEFT JOIN discrete.sample_types st ON st.sample_type_id = s.sample_type\n",
    "  WHERE ($4::boolean OR st.sample_type IS NULL OR st.sample_type !~* 'blank')\n",
    "    AND ($5::boolean OR st.sample_type IS NULL OR st.sample_type !~* '(duplicate|replicate)')\n",
    ")\n",
    "SELECT r.result_id, s.sample_id, s.location_id, l.location_code AS location,\n",
    "  l.alias, l.name AS location_name, l.name_fr AS location_name_fr,\n",
    "  l.latitude, l.longitude, s.sub_location_id, sl.sub_location_name,\n",
    "  sl.sub_location_name_fr, s.requested_date, s.date_approx,\n",
    "  s.datetime::date AS sample_date, s.datetime,\n",
    "  s.target_datetime, s.media_id, mt.media_type, mt.media_type_fr,\n",
    "  s.sample_type AS sample_type_id, st.sample_type, s.collection_method AS collection_method_id,\n",
    "  cm.collection_method, r.parameter_id, p.param_name, p.param_name_fr,\n",
    "  r.matrix_state_id, ms.matrix_state_name AS matrix_state,\n",
    "  r.sample_fraction_id, sf.sample_fraction, r.result_speciation_id, rs.result_speciation,\n",
    "  r.result_type AS result_type_id, rt.result_type,\n",
    "  r.result_value_type AS result_value_type_id, rvt.result_value_type,\n",
    "  r.result, r.result_condition, rc.result_condition AS result_condition_label,\n",
    "  r.result_condition_value, ", unit_sql, ",\n",
    "  r.grade_type_id AS result_grade_id, gt.grade_type_code AS result_grade_code,\n",
    "  gt.grade_type_description AS result_grade, r.approval_type_id AS result_approval_id,\n",
    "  at.approval_type_code AS result_approval_code,\n",
    "  at.approval_type_description AS result_approval, r.lab_report_no, r.lab_sample_no\n",
    "FROM selected_samples s\n",
    "JOIN discrete.results r ON r.sample_id = s.sample_id\n",
    "JOIN public.locations l ON l.location_id = s.location_id\n",
    "JOIN public.parameters p ON p.parameter_id = r.parameter_id\n",
    "LEFT JOIN public.matrix_states ms ON ms.matrix_state_id = r.matrix_state_id\n",
    "LEFT JOIN public.media_types mt ON mt.media_id = s.media_id\n",
    "LEFT JOIN public.sub_locations sl ON sl.sub_location_id = s.sub_location_id\n",
    "LEFT JOIN discrete.sample_types st ON st.sample_type_id = s.sample_type\n",
    "LEFT JOIN discrete.collection_methods cm ON cm.collection_method_id = s.collection_method\n",
    "LEFT JOIN discrete.sample_fractions sf ON sf.sample_fraction_id = r.sample_fraction_id\n",
    "LEFT JOIN discrete.result_speciations rs ON rs.result_speciation_id = r.result_speciation_id\n",
    "LEFT JOIN discrete.result_types rt ON rt.result_type_id = r.result_type\n",
    "LEFT JOIN discrete.result_value_types rvt ON rvt.result_value_type_id = r.result_value_type\n",
    "LEFT JOIN discrete.result_conditions rc ON rc.result_condition_id = r.result_condition\n",
    "LEFT JOIN public.grade_types gt ON gt.grade_type_id = r.grade_type_id\n",
    "LEFT JOIN public.approval_types at ON at.approval_type_id = r.approval_type_id\n",
    "WHERE r.parameter_id IN (SELECT value::integer FROM jsonb_array_elements_text($2::jsonb))\n",
    "  AND (jsonb_array_length($6::jsonb) = 0 OR r.matrix_state_id IN\n",
    "    (SELECT value::integer FROM jsonb_array_elements_text($6::jsonb)))\n",
    "  AND (jsonb_array_length($7::jsonb) = 0 OR r.sample_fraction_id IN\n",
    "    (SELECT value::integer FROM jsonb_array_elements_text($7::jsonb)))\n",
    "ORDER BY s.requested_date, l.location_code, s.datetime, s.sample_id, p.param_name, r.result_id;"
  )
  results <- DBI::dbGetQuery(
    con,
    result_sql,
    params = list(
      location_json, parameter_json, date_request_json,
      include_blanks, include_duplicates, matrix_state_json,
      sample_fraction_json
    )
  )
  if (nrow(results) == 0L) {
    stop("No results matched the selected locations, parameters, and target dates.", call. = FALSE)
  }
  results$requested_date <- as.Date(results$requested_date)
  selected_date_pairs <- unique(results[c("sample_id", "requested_date")])
  repeated_samples <- unique(
    selected_date_pairs$sample_id[duplicated(selected_date_pairs$sample_id)]
  )
  if (length(repeated_samples)) {
    warning(
      "Closest-date matching selected the same sample for multiple requested dates; it will appear in ",
      if (report_format == "by_date") "multiple date worksheets." else "multiple date columns.",
      call. = FALSE
    )
  }
  results$location_label <- if (lang == "fr") {
    ifelse(is.na(results$location_name_fr), results$location_name, results$location_name_fr)
  } else {
    results$location_name
  }
  results$parameter_label <- if (lang == "fr") {
    ifelse(is.na(results$param_name_fr), results$param_name, results$param_name_fr)
  } else {
    results$param_name
  }
  results$media_label <- if (lang == "fr") {
    ifelse(is.na(results$media_type_fr), results$media_type, results$media_type_fr)
  } else {
    results$media_type
  }
  results$matrix_label <- results$matrix_state
  results$sub_location_label <- if (lang == "fr") {
    results$sub_location_name_fr
  } else {
    results$sub_location_name
  }
  is_less_than <- results$result_condition %in% 1L |
    grepl("below detection|less than", results$result_condition_label, ignore.case = TRUE)
  is_greater_than <- results$result_condition %in% 2L |
    grepl("above detection|greater than", results$result_condition_label, ignore.case = TRUE)
  results$result_relation <- ifelse(
    !is.na(results$result), "actual",
    ifelse(is_less_than, "less_than", ifelse(is_greater_than, "greater_than", "none"))
  )
  results$result_display <- ifelse(
    !is.na(results$result),
    format(results$result, digits = 12, trim = TRUE, scientific = FALSE),
    ifelse(
      results$result_relation == "less_than" & !is.na(results$result_condition_value),
      paste0("< ", format(results$result_condition_value, digits = 12, trim = TRUE, scientific = FALSE)),
      ifelse(
        results$result_relation == "greater_than" & !is.na(results$result_condition_value),
        paste0("> ", format(results$result_condition_value, digits = 12, trim = TRUE, scientific = FALSE)),
        ifelse(is.na(results$result_condition_label), "", results$result_condition_label)
      )
    )
  )

  if (length(guideline_ids)) {
    guideline_engine <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT to_regprocedure(",
        "'criteria.applicable_guideline_rules_for_result(integer,date,boolean,boolean)'",
        ") IS NOT NULL AS available"
      )
    )$available[[1]]
    if (!isTRUE(guideline_engine)) {
      stop(
        "AquaCache's criteria.applicable_guideline_rules_for_result() is not installed; this report requires AquaCache Patch 61 or later.",
        call. = FALSE
      )
    }
    guideline_json <- id_json(guideline_ids)
    guideline_catalog <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT g.guideline_id, g.guideline_code, g.guideline_name, g.version_label,\n",
        "  g.parent_guideline_code, gp.publisher_name, gs.series_name,\n",
        "  g.parameter_id, p.param_name AS parameter_name,\n",
        "  g.matrix_state_id, ms.matrix_state_code, ms.matrix_state_name,\n",
        "  g.result_speciation_id, rs.result_speciation,\n",
        "  g.comparison_operator_code, op.operator_symbol, op.operator_name,\n",
        "  op.description AS comparison_description, gj.jurisdiction_name AS jurisdiction,\n",
        "  gpg.protection_goal_name AS protection_goal,\n",
        "  ged.exposure_duration_name AS exposure_duration,\n",
        "  gap.averaging_period_name AS averaging_period,\n",
        "  COALESCE(NULLIF(g.source_document_title, ''), NULLIF(g.reference, '')) AS source_document_title,\n",
        "  COALESCE(NULLIF(g.source_url, ''), NULLIF(g.reference, '')) AS reference_url,\n",
        "  g.source_page, g.source_table, g.source_section, g.source_effective_date,\n",
        "  g.source_retrieved_date, g.source_revision, g.valid_from, g.valid_to,\n",
        "  g.active, g.review_status, g.general_notes, g.applicability_notes,\n",
        "  COALESCE((SELECT jsonb_agg(mt.media_type ORDER BY mt.media_type)\n",
        "    FROM criteria.guidelines_media_types gm\n",
        "    JOIN public.media_types mt ON mt.media_id = gm.media_id\n",
        "    WHERE gm.guideline_id = g.guideline_id), '[]'::jsonb) AS media_applicability,\n",
        "  COALESCE((SELECT jsonb_agg(sf.sample_fraction ORDER BY sf.sample_fraction)\n",
        "    FROM criteria.guidelines_fractions gf\n",
        "    JOIN discrete.sample_fractions sf ON sf.sample_fraction_id = gf.fraction_id\n",
        "    WHERE gf.guideline_id = g.guideline_id), '[]'::jsonb) AS fraction_applicability,\n",
        "  COALESCE((SELECT jsonb_agg(jsonb_build_object(\n",
        "      'location_id', gl.location_id, 'location_code', l.location_code, 'note', gl.note)\n",
        "      ORDER BY l.location_code)\n",
        "    FROM criteria.guideline_locations gl\n",
        "    JOIN public.locations l ON l.location_id = gl.location_id\n",
        "    WHERE gl.guideline_id = g.guideline_id AND gl.active), '[]'::jsonb) AS location_applicability\n",
        "FROM criteria.guidelines g\n",
        "LEFT JOIN criteria.guideline_publishers gp ON gp.publisher_id = g.publisher_id\n",
        "LEFT JOIN criteria.guideline_series gs ON gs.series_id = g.series_id\n",
        "LEFT JOIN public.parameters p ON p.parameter_id = g.parameter_id\n",
        "LEFT JOIN public.matrix_states ms ON ms.matrix_state_id = g.matrix_state_id\n",
        "LEFT JOIN discrete.result_speciations rs ON rs.result_speciation_id = g.result_speciation_id\n",
        "LEFT JOIN criteria.guideline_comparison_operators op ON op.operator_code = g.comparison_operator_code\n",
        "LEFT JOIN criteria.guideline_jurisdictions gj ON gj.jurisdiction_id = g.jurisdiction_id\n",
        "LEFT JOIN criteria.guideline_protection_goals gpg ON gpg.protection_goal_id = g.protection_goal_id\n",
        "LEFT JOIN criteria.guideline_exposure_durations ged ON ged.exposure_duration_id = g.exposure_duration_id\n",
        "LEFT JOIN criteria.guideline_averaging_periods gap ON gap.averaging_period_id = g.averaging_period_id\n",
        "WHERE g.guideline_id IN (SELECT value::integer FROM jsonb_array_elements_text($1::jsonb))\n",
        "ORDER BY g.guideline_code, g.guideline_id;"
      ),
      params = list(guideline_json)
    )
    missing_guidelines <- setdiff(guideline_ids, guideline_catalog$guideline_id)
    if (length(missing_guidelines)) {
      warning(
        "Guideline IDs not found or visible: ", paste(missing_guidelines, collapse = ", "),
        call. = FALSE
      )
    }

    request_json <- as.character(jsonlite::toJSON(
      data.frame(
        result_id = as.integer(results$result_id),
        sample_date = format(as.Date(results$sample_date), "%Y-%m-%d")
      ) |> unique(),
      dataframe = "rows",
      auto_unbox = TRUE,
      na = "null"
    ))
    guideline_rule_sql <- paste0(
      "WITH requested AS (\n",
      "  SELECT result_id, sample_date\n",
      "  FROM jsonb_to_recordset($1::jsonb) AS r(result_id integer, sample_date date)\n",
      ")\n",
      "SELECT ar.result_id, ar.sample_id, s.location_id, l.location_code AS location,\n",
      "  s.datetime::date AS sample_date, ar.parameter_id, ar.parameter_name,\n",
      "  ar.matrix_state_id, ar.matrix_state_code, ar.units, r.result AS raw_result,\n",
      "  ar.result_value, ar.result_value_relation, r.result_condition,\n",
      "  rc.result_condition AS result_condition_label, r.result_condition_value,\n",
      "  ar.guideline_id, ar.guideline_code, ar.guideline_name, ar.publisher_name,\n",
      "  ar.series_name, ar.jurisdiction, ar.protection_goal, ar.exposure_duration,\n",
      "  ar.averaging_period, ar.comparison_operator_code, ar.comparison_symbol,\n",
      "  ar.rule_id, ar.bound_code, ar.guideline_value, ar.output_status,\n",
      "  ar.comparison_status, ar.derivation_inputs::text AS derivation_inputs,\n",
      "  ar.message, COALESCE(NULLIF(g.source_url, ''), NULLIF(g.reference, '')) AS reference_url,\n",
      "  g.source_document_title, g.source_page, g.source_table, g.source_section,\n",
      "  g.valid_from, g.valid_to, g.review_status, g.active,\n",
      "  g.general_notes, g.applicability_notes, gr.algorithm_code,\n",
      "  ga.algorithm_name, ga.description AS algorithm_description, gr.fixed_value,\n",
      "  gr.formula_sql, gr.min_output_value, gr.max_output_value,\n",
      "  gr.rounding_method, gr.rounding_digits, gr.missing_input_policy,\n",
      "  gr.precision_note, gr.note AS rule_note,\n",
      "  COALESCE(inputs.input_definitions, '[]'::jsonb)::text AS input_definitions,\n",
      "  COALESCE(coefficients.coefficients, '[]'::jsonb)::text AS coefficients,\n",
      "  CASE WHEN EXISTS (SELECT 1 FROM criteria.guidelines_media_types gm\n",
      "    WHERE gm.guideline_id = g.guideline_id) THEN\n",
      "    COALESCE((SELECT jsonb_agg(mt.media_type ORDER BY mt.media_type)\n",
      "      FROM criteria.guidelines_media_types gm\n",
      "      JOIN public.media_types mt ON mt.media_id = gm.media_id\n",
      "      WHERE gm.guideline_id = g.guideline_id), '[]'::jsonb)::text\n",
      "    ELSE 'All media (unrestricted)' END AS media_applicability,\n",
      "  CASE WHEN EXISTS (SELECT 1 FROM criteria.guidelines_fractions gf\n",
      "    WHERE gf.guideline_id = g.guideline_id) THEN\n",
      "    COALESCE((SELECT jsonb_agg(sf.sample_fraction ORDER BY sf.sample_fraction)\n",
      "      FROM criteria.guidelines_fractions gf\n",
      "      JOIN discrete.sample_fractions sf ON sf.sample_fraction_id = gf.fraction_id\n",
      "      WHERE gf.guideline_id = g.guideline_id), '[]'::jsonb)::text\n",
      "    ELSE 'All fractions (unrestricted)' END AS fraction_applicability,\n",
      "  CASE WHEN EXISTS (SELECT 1 FROM criteria.guideline_locations gl\n",
      "    WHERE gl.guideline_id = g.guideline_id AND gl.active) THEN\n",
      "    COALESCE((SELECT jsonb_agg(l2.location_code ORDER BY l2.location_code)\n",
      "      FROM criteria.guideline_locations gl\n",
      "      JOIN public.locations l2 ON l2.location_id = gl.location_id\n",
      "      WHERE gl.guideline_id = g.guideline_id AND gl.active), '[]'::jsonb)::text\n",
      "    ELSE 'All locations (unrestricted)' END AS location_applicability\n",
      "FROM requested q\n",
      "CROSS JOIN LATERAL criteria.applicable_guideline_rules_for_result(\n",
      "  q.result_id, q.sample_date, TRUE, FALSE) ar\n",
      "JOIN discrete.results r ON r.result_id = ar.result_id\n",
      "JOIN discrete.samples s ON s.sample_id = ar.sample_id\n",
      "JOIN public.locations l ON l.location_id = s.location_id\n",
      "JOIN criteria.guidelines g ON g.guideline_id = ar.guideline_id\n",
      "JOIN criteria.guideline_value_rules gr ON gr.rule_id = ar.rule_id\n",
      "LEFT JOIN criteria.guideline_value_algorithms ga ON ga.algorithm_code = gr.algorithm_code\n",
      "LEFT JOIN discrete.result_conditions rc ON rc.result_condition_id = r.result_condition\n",
      "LEFT JOIN LATERAL (\n",
      "  SELECT jsonb_agg(jsonb_build_object(\n",
      "    'input_code', gri.input_code, 'input_name', gri.input_name,\n",
      "    'input_source', gri.input_source, 'parameter_id', gri.parameter_id,\n",
      "    'parameter_name', ip.param_name, 'matrix_state_id', gri.matrix_state_id,\n",
      "    'matrix_state', ims.matrix_state_name, 'sample_fraction_id', gri.sample_fraction_id,\n",
      "    'sample_fraction', isf.sample_fraction, 'result_speciation_id', gri.result_speciation_id,\n",
      "    'result_speciation', irs.result_speciation, 'result_type_id', gri.result_type,\n",
      "    'result_type', irt.result_type, 'result_type_preference', gri.result_type_preference,\n",
      "    'input_search_scope', gri.search_scope, 'aggregate_method', gri.aggregate_method,\n",
      "    'allow_condition_value', gri.allow_condition_value,\n",
      "    'lower_calibrated_bound', gri.lower_calibrated_bound,\n",
      "    'upper_calibrated_bound', gri.upper_calibrated_bound,\n",
      "    'bounds_action', gri.bounds_action, 'required', gri.required, 'note', gri.note\n",
      "  ) ORDER BY gri.input_code) AS input_definitions\n",
      "  FROM criteria.guideline_rule_inputs gri\n",
      "  LEFT JOIN public.parameters ip ON ip.parameter_id = gri.parameter_id\n",
      "  LEFT JOIN public.matrix_states ims ON ims.matrix_state_id = gri.matrix_state_id\n",
      "  LEFT JOIN discrete.sample_fractions isf ON isf.sample_fraction_id = gri.sample_fraction_id\n",
      "  LEFT JOIN discrete.result_speciations irs ON irs.result_speciation_id = gri.result_speciation_id\n",
      "  LEFT JOIN discrete.result_types irt ON irt.result_type_id = gri.result_type\n",
      "  WHERE gri.rule_id = gr.rule_id\n",
      ") inputs ON TRUE\n",
      "LEFT JOIN LATERAL (\n",
      "  SELECT jsonb_agg(jsonb_build_object(\n",
      "    'coefficient_name', grc.coefficient_name,\n",
      "    'coefficient_value', grc.coefficient_value, 'note', grc.note\n",
      "  ) ORDER BY grc.coefficient_name) AS coefficients\n",
      "  FROM criteria.guideline_rule_coefficients grc\n",
      "  WHERE grc.rule_id = gr.rule_id\n",
      ") coefficients ON TRUE\n",
      "WHERE ar.guideline_id IN (SELECT value::integer FROM jsonb_array_elements_text($2::jsonb))\n",
      "ORDER BY ar.result_id, ar.guideline_id, ar.rule_id;"
    )
    guideline_rules <- DBI::dbGetQuery(
      con,
      guideline_rule_sql,
      params = list(request_json, guideline_json)
    )
  } else {
    guideline_catalog <- data.frame()
    guideline_rules <- data.frame()
  }

  results$result_row_key <- do.call(paste, c(
    lapply(results[c(
      "parameter_id", "matrix_state_id", "sample_fraction_id",
      "result_speciation_id", "units"
    )], function(x) {
      value <- as.character(x)
      value[is.na(value)] <- "<NA>"
      value
    }),
    sep = "\034"
  ))

  guideline_summary <- data.frame()
  if (nrow(guideline_rules)) {
    summary_groups <- split(
      seq_len(nrow(guideline_rules)),
      paste(guideline_rules$result_id, guideline_rules$guideline_id, sep = "\034")
    )
    guideline_summary <- do.call(rbind, lapply(summary_groups, function(idx) {
      rows <- guideline_rules[idx, , drop = FALSE]
      statuses <- unique(stats::na.omit(rows$comparison_status))
      failures <- intersect(statuses, c("exceeds", "below", "does_not_equal"))
      assessment <- if (length(failures)) {
        paste(failures, collapse = "; ")
      } else if (length(statuses) && all(statuses == "meets")) {
        "meets"
      } else if (length(statuses)) {
        paste(statuses, collapse = "; ")
      } else {
        "not evaluated"
      }
      valid_values <- rows$output_status == "value" & !is.na(rows$guideline_value)
      lower <- rows$guideline_value[valid_values & rows$bound_code == "lower"]
      upper <- rows$guideline_value[valid_values & rows$bound_code == "upper"]
      lower <- if (length(lower)) max(lower) else NA_real_
      upper <- if (length(upper)) max(upper) else NA_real_
      op <- rows$comparison_operator_code[[1]]
      value_text <- function(x) format(x, digits = 12, trim = TRUE, scientific = FALSE)
      limit <- switch(
        op,
        lte = if (!is.na(upper)) paste0("<= ", value_text(upper)) else "unresolved",
        gte = if (!is.na(lower)) paste0(">= ", value_text(lower)) else "unresolved",
        range = if (!is.na(lower) && !is.na(upper)) {
          paste0(value_text(lower), " to ", value_text(upper))
        } else "unresolved",
        eq = if (!is.na(upper)) paste0("= ", value_text(upper)) else "unresolved",
        narrative = "narrative",
        "unresolved"
      )
      data.frame(
        result_id = rows$result_id[[1]],
        guideline_id = rows$guideline_id[[1]],
        lower_value = lower,
        upper_value = upper,
        limit_display = limit,
        assessment = assessment,
        has_failure = length(failures) > 0L,
        stringsAsFactors = FALSE
      )
    }))
    rownames(guideline_summary) <- NULL
    guideline_summary$guideline_label <- guideline_catalog$guideline_code[
      match(guideline_summary$guideline_id, guideline_catalog$guideline_id)
    ]
    missing_codes <- is.na(guideline_summary$guideline_label) |
      !nzchar(guideline_summary$guideline_label)
    guideline_summary$guideline_label[missing_codes] <- paste0(
      "Guideline ", guideline_summary$guideline_id[missing_codes]
    )
    guideline_summary$row_key <- results$result_row_key[
      match(guideline_summary$result_id, results$result_id)
    ]
  }

  results$sd_exceedance <- FALSE
  if (!is.null(sd_multiplier)) {
    sd_where <- c(
      "s.location_id IN (SELECT value::integer FROM jsonb_array_elements_text($1::jsonb))",
      "r.parameter_id IN (SELECT value::integer FROM jsonb_array_elements_text($2::jsonb))",
      "r.result IS NOT NULL",
      "($3::boolean OR st.sample_type IS NULL OR st.sample_type !~* 'blank')",
      "($4::boolean OR st.sample_type IS NULL OR st.sample_type !~* '(duplicate|replicate)')"
    )
    sd_params <- list(location_json, parameter_json, include_blanks, include_duplicates)
    if (!is.null(sd_start)) {
      sd_params[[length(sd_params) + 1L]] <- format(sd_start, "%Y-%m-%d")
      sd_where <- c(sd_where, paste0("s.datetime::date >= $", length(sd_params), "::date"))
    }
    if (!is.null(sd_end)) {
      sd_params[[length(sd_params) + 1L]] <- format(sd_end, "%Y-%m-%d")
      sd_where <- c(sd_where, paste0("s.datetime::date <= $", length(sd_params), "::date"))
    }
    if (!is.null(sd_day_of_year)) {
      sd_params[[length(sd_params) + 1L]] <- id_json(unique(as.integer(sd_day_of_year)))
      sd_where <- c(
        sd_where,
        paste0(
          "EXTRACT(DOY FROM s.datetime)::integer IN (SELECT value::integer ",
          "FROM jsonb_array_elements_text($", length(sd_params), "::jsonb))"
        )
      )
    }
    sd_history <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT s.location_id, r.parameter_id, r.matrix_state_id,\n",
        "  r.sample_fraction_id, r.result_speciation_id, r.result AS result\n",
        "FROM discrete.results r\n",
        "JOIN discrete.samples s ON s.sample_id = r.sample_id\n",
        "LEFT JOIN discrete.sample_types st ON st.sample_type_id = s.sample_type\n",
        "WHERE ", paste(sd_where, collapse = " AND "), ";"
      ),
      params = sd_params
    )
    key_columns <- c(
      "location_id", "parameter_id", "matrix_state_id",
      "sample_fraction_id", "result_speciation_id"
    )
    if (nrow(sd_history)) {
      sd_dt <- data.table::as.data.table(sd_history)
      sd_stats <- sd_dt[, .(
        sd_n = .N,
        sd_mean = mean(result),
        sd_value = if (.N > 1L) stats::sd(result) else NA_real_
      ), by = key_columns]
      sd_stats[, sd_lower := sd_mean - sd_multiplier * sd_value]
      sd_stats[, sd_upper := sd_mean + sd_multiplier * sd_value]
      make_group_key <- function(x) do.call(paste, c(
        lapply(x[key_columns], function(value) {
          value <- as.character(value)
          value[is.na(value)] <- "<NA>"
          value
        }),
        sep = "\034"
      ))
      sd_index <- match(make_group_key(results), make_group_key(sd_stats))
      results$sd_n <- sd_stats$sd_n[sd_index]
      results$sd_mean <- sd_stats$sd_mean[sd_index]
      results$sd_value <- sd_stats$sd_value[sd_index]
      results$sd_lower <- sd_stats$sd_lower[sd_index]
      results$sd_upper <- sd_stats$sd_upper[sd_index]
      results$sd_exceedance <- !is.na(results$result) &
        !is.na(results$sd_lower) &
        (results$result < results$sd_lower | results$result > results$sd_upper)
    } else {
      results$sd_n <- NA_integer_
      results$sd_mean <- results$sd_value <- results$sd_lower <- results$sd_upper <- NA_real_
      warning("No numeric historical results were available for the SD calculation.", call. = FALSE)
    }
  }

  if (nrow(guideline_summary)) {
    assessment_groups <- split(
      seq_len(nrow(guideline_summary)), guideline_summary$result_id
    )
    results$guideline_assessments <- vapply(seq_len(nrow(results)), function(i) {
      idx <- assessment_groups[[as.character(results$result_id[[i]])]]
      if (is.null(idx)) return("")
      rows <- guideline_summary[idx, , drop = FALSE]
      labels <- paste0(rows$guideline_label, ": ", rows$limit_display, " (", rows$assessment, ")")
      paste(labels, collapse = "; ")
    }, character(1))
  } else {
    results$guideline_assessments <- ""
  }

  # Assemble the matrix sheets, with one row per parameter/result context or
  # location/result context, depending on the selected layout.
  param_key_columns <- c(
    "parameter_id", "matrix_state_id", "sample_fraction_id",
    "result_speciation_id", "units"
  )
  make_matrix_key <- function(x, columns) do.call(paste, c(
    lapply(x[columns], function(value) {
      value <- as.character(value)
      value[is.na(value)] <- "<NA>"
      value
    }),
    sep = "\034"
  ))
  all_param_groups <- unique(results[c(
    param_key_columns, "parameter_label", "matrix_label",
    "sample_fraction", "result_speciation"
  )])
  all_param_groups <- all_param_groups[order(
    match(all_param_groups$parameter_id, parameter_ids),
    all_param_groups$matrix_state_id,
    all_param_groups$sample_fraction_id,
    all_param_groups$result_speciation_id,
    na.last = TRUE
  ), , drop = FALSE]
  all_param_groups$row_key <- make_matrix_key(all_param_groups, param_key_columns)
  results$result_row_key <- make_matrix_key(results, param_key_columns)

  report_tabs <- if (report_format == "by_date") {
    lapply(seq_along(date), function(i) list(
      id = date[[i]],
      base_name = if (length(date) == 1L) "Report" else format(date[[i]], "%Y-%m-%d"),
      result_rows = which(results$requested_date == date[[i]])
    ))
  } else if (report_format == "by_location") {
    lapply(location_ids, function(id) {
      meta <- requested_locations[requested_locations$location_id == id, , drop = FALSE]
      location <- if (nrow(meta)) as.character(meta$location_code[[1]]) else paste0("Location ", id)
      list(
        id = id,
        base_name = if (length(location_ids) == 1L) "Report" else location,
        result_rows = which(results$location_id == id)
      )
    })
  } else {
    lapply(parameter_ids, function(id) {
      meta <- requested_parameters[requested_parameters$parameter_id == id, , drop = FALSE]
      parameter <- if (nrow(meta)) as.character(meta$param_name[[1]]) else paste0("Parameter ", id)
      if (nrow(meta) && lang == "fr" && !is.na(meta$param_name_fr[[1]]) &&
          nzchar(meta$param_name_fr[[1]])) {
        parameter <- as.character(meta$param_name_fr[[1]])
      }
      list(
        id = id,
        base_name = if (length(parameter_ids) == 1L) "Report" else parameter,
        result_rows = which(results$parameter_id == id)
      )
    })
  }
  make_sheet_names <- function(values, reserved = character()) {
    for (invalid in c("/", "\\", "?", "*", "[", "]", ":")) {
      values <- gsub(invalid, " ", values, fixed = TRUE)
    }
    values <- trimws(substr(values, 1L, 31L))
    values[!nzchar(values)] <- "Report"
    used <- reserved
    for (i in seq_along(values)) {
      candidate <- values[[i]]
      suffix <- 1L
      while (tolower(candidate) %in% tolower(used)) {
        suffix <- suffix + 1L
        ending <- paste0(" (", suffix, ")")
        candidate <- paste0(substr(values[[i]], 1L, 31L - nchar(ending)), ending)
      }
      values[[i]] <- candidate
      used <- c(used, candidate)
    }
    values
  }
  report_sheet_names <- make_sheet_names(
    vapply(report_tabs, `[[`, character(1), "base_name"),
    reserved = c("Result details", "Guideline details")
  )

  # Format the two filterable detail tables with user-facing labels.
  result_details <- data.frame(
    `Result ID` = results$result_id,
    `Sample ID` = results$sample_id,
    `Location ID` = results$location_id,
    `Location code` = results$location,
    `Location name` = results$location_label,
    `Sub-location` = results$sub_location_label,
    `Requested date` = results$requested_date,
    `Date tolerance (days)` = results$date_approx,
    `Sample date (UTC)` = results$sample_date,
    `Sample datetime (UTC)` = results$datetime,
    `Target datetime (UTC)` = results$target_datetime,
    `Parameter ID` = results$parameter_id,
    Parameter = results$parameter_label,
    Matrix = results$matrix_state,
    `Sample fraction` = results$sample_fraction,
    Speciation = results$result_speciation,
    Unit = results$units,
    `Numeric result` = results$result,
    `Result display` = results$result_display,
    `Result relation` = results$result_relation,
    `Result condition` = results$result_condition_label,
    `Condition value` = results$result_condition_value,
    Media = results$media_label,
    `Sample type` = results$sample_type,
    `Collection method` = results$collection_method,
    `Result type` = results$result_type,
    `Result value type` = results$result_value_type,
    `Result grade` = results$result_grade,
    `Result approval` = results$result_approval,
    `Lab report number` = results$lab_report_no,
    `Lab sample number` = results$lab_sample_no,
    `Guideline assessments` = results$guideline_assessments,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  if (!is.null(sd_multiplier)) {
    result_details$`SD sample count` <- results$sd_n
    result_details$`SD mean` <- results$sd_mean
    result_details$`SD` <- results$sd_value
    result_details$`SD lower bound` <- results$sd_lower
    result_details$`SD upper bound` <- results$sd_upper
    result_details$`SD exceedance` <- results$sd_exceedance
  }

  # Prepare the report worksheets in the EQWin-style matrix layout.
  wb <- openxlsx::createWorkbook(title = "Water Quality Report")
  generated <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
  title_style <- openxlsx::createStyle(
    fgFill = "turquoise2", textDecoration = "bold", fontSize = 12,
    valign = "center"
  )
  note_style <- openxlsx::createStyle(fgFill = "orchid", wrapText = TRUE, valign = "center")
  group_style <- openxlsx::createStyle(
    fgFill = "azure3", textDecoration = "bold", halign = "center"
  )
  sample_style <- openxlsx::createStyle(
    fgFill = "lemonchiffon2", textDecoration = "bold", wrapText = TRUE,
    halign = "center", valign = "center"
  )
  parameter_style <- openxlsx::createStyle(fgFill = "goldenrod1")
  limit_style <- openxlsx::createStyle(fgFill = "azure1", wrapText = TRUE)
  sample_data_style <- openxlsx::createStyle(fgFill = "lemonchiffon")
  exceed_style <- openxlsx::createStyle(
    fontColour = "black", border = "TopBottomLeftRight",
    borderColour = "red2", borderStyle = "medium"
  )
  format_note <- switch(
    report_format,
    by_date = "Each requested date has a separate worksheet; locations are columns.",
    by_location = "Each location has a separate worksheet; target dates are columns.",
    by_parameter = "Each parameter has a separate worksheet; locations are rows and target dates are columns."
  )
  flag_note <- if (length(guideline_ids)) {
    paste0(
      "Run with guideline IDs: ", paste(guideline_ids, collapse = ", "),
      ". Guideline applicability is evaluated separately for each result."
    )
  } else {
    "Run with no guideline flags."
  }
  if (!is.null(sd_multiplier)) {
    flag_note <- paste0(
      flag_note, " SD exceedances above/below ", sd_multiplier,
      " SD from mean (from ",
      if (is.null(sd_start)) "start of records" else format(sd_start, "%Y-%m-%d"),
      " to ",
      if (is.null(sd_end)) "end of records" else format(sd_end, "%Y-%m-%d"),
      if (is.null(sd_day_of_year)) " for all days of year" else paste0(
        " on days of year ", paste(sd_day_of_year, collapse = ", ")
      ),
      "). Numeric results only; red outline marks an SD exceedance or guideline failure."
    )
  } else if (length(guideline_ids)) {
    flag_note <- paste0(flag_note, " Red outline marks a guideline failure.")
  }
  if (length(repeated_samples)) {
    flag_note <- paste0(flag_note, " A sample may appear for more than one target date.")
  }
  failed_result_ids <- if (nrow(guideline_summary)) {
    unique(guideline_summary$result_id[guideline_summary$has_failure])
  } else integer()

  for (tab_index in seq_along(report_tabs)) {
    tab <- report_tabs[[tab_index]]
    sheet_name <- report_sheet_names[[tab_index]]
    sheet_results <- results[tab$result_rows, , drop = FALSE]
    if (report_format == "by_parameter") {
      row_results <- results[results$parameter_id == tab$id, , drop = FALSE]
      row_groups <- unique(row_results[c(
        "location_id", "location", "location_label", "parameter_id",
        "matrix_state_id", "sample_fraction_id", "result_speciation_id",
        "units", "matrix_label", "sample_fraction", "result_speciation"
      )])
      row_groups <- row_groups[order(
        match(row_groups$location_id, location_ids),
        row_groups$matrix_state_id, row_groups$sample_fraction_id,
        row_groups$result_speciation_id, na.last = TRUE
      ), , drop = FALSE]
      row_groups$row_key <- make_matrix_key(row_groups, param_key_columns)
    } else {
      row_results <- sheet_results
      row_groups <- all_param_groups
    }
    row_groups$cell_row_key <- if (report_format == "by_parameter") {
      paste(row_groups$location_id, row_groups$row_key, sep = "\034")
    } else row_groups$row_key
    row_results$cell_row_key <- if (report_format == "by_parameter") {
      paste(row_results$location_id, row_results$result_row_key, sep = "\034")
    } else row_results$result_row_key
    guide_limit_map <- list()
    if (nrow(guideline_summary) && nrow(row_results) && nrow(row_groups)) {
      result_groups <- unique(row_results[c("result_id", "cell_row_key")])
      result_groups$row_index <- match(result_groups$cell_row_key, row_groups$cell_row_key)
      result_groups <- result_groups[!is.na(result_groups$row_index), , drop = FALSE]
      guide_values <- merge(
        result_groups,
        guideline_summary[c("result_id", "guideline_id", "limit_display")],
        by = "result_id",
        all = FALSE,
        sort = FALSE
      )
      if (nrow(guide_values)) {
        guide_limit_map <- split(
          guide_values$limit_display,
          paste(guide_values$row_index, guide_values$guideline_id, sep = "\034")
        )
        guide_limit_map <- lapply(guide_limit_map, function(value) {
          unique(value[!is.na(value) & nzchar(value)])
        })
      }
    }

    guideline_column_names <- if (length(guideline_ids)) vapply(
      seq_len(nrow(guideline_catalog)), function(i) {
        meta <- guideline_catalog[i, , drop = FALSE]
        code <- if (is.na(meta$guideline_code) || !nzchar(meta$guideline_code)) {
          paste0("Guideline ", meta$guideline_id)
        } else as.character(meta$guideline_code)
        paste0(code, " - ", meta$guideline_name, " (limit)")
      }, character(1)
    ) else character()

    if (report_format == "by_date") {
      sample_meta <- unique(sheet_results[c(
        "sample_id", "location", "sub_location_label", "sample_date", "datetime"
      )])
      sample_meta <- sample_meta[order(sample_meta$location, sample_meta$datetime), , drop = FALSE]
      if (!nrow(sample_meta)) {
        date_headers <- "No eligible samples"
      } else {
        date_headers <- paste0(
          sample_meta$location,
          ifelse(
            is.na(sample_meta$sub_location_label) | !nzchar(sample_meta$sub_location_label),
            "", paste0(" - ", sample_meta$sub_location_label)
          ),
          " (", format(sample_meta$datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC"), " UTC)"
        )
        date_headers <- make.unique(date_headers, sep = " #")
      }
    } else {
      sample_meta <- data.frame()
      date_headers <- base::format(date, "%Y-%m-%d")
    }

    if (report_format == "by_parameter") {
      matrix_report <- data.frame(
        Location = row_groups$location,
        `Location name` = row_groups$location_label,
        Unit = row_groups$units,
        Matrix = row_groups$matrix_label,
        `Sample fraction` = row_groups$sample_fraction,
        Speciation = row_groups$result_speciation,
        `Parameter ID` = row_groups$parameter_id,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    } else {
      matrix_report <- data.frame(
        Parameter = row_groups$parameter_label,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }
    if (length(guideline_column_names)) {
      for (g in seq_along(guideline_column_names)) {
        meta <- guideline_catalog[g, , drop = FALSE]
        values <- rep("No applicable result", nrow(row_groups))
        if (length(guide_limit_map) && nrow(row_groups)) {
          for (j in seq_len(nrow(row_groups))) {
            limits <- guide_limit_map[[paste(j, meta$guideline_id, sep = "\034")]]
            if (is.null(limits)) next
            if (length(limits) == 1L) values[[j]] <- limits
            if (length(limits) > 1L) values[[j]] <- "Varies by sample; see Guideline details"
          }
        }
        matrix_report[[guideline_column_names[[g]]]] <- values
      }
    }
    if (report_format != "by_parameter") {
      matrix_report$Unit <- row_groups$units
      matrix_report$Matrix <- row_groups$matrix_label
      matrix_report$`Sample fraction` <- row_groups$sample_fraction
      matrix_report$Speciation <- row_groups$result_speciation
      matrix_report$`Parameter ID` <- row_groups$parameter_id
    }
    for (header in date_headers) {
      matrix_report[[header]] <- rep("", nrow(row_groups))
    }
    flagged_row_index <- flagged_column_index <- integer()
    if (nrow(row_results) && nrow(row_groups)) {
      cell_table <- data.table::as.data.table(row_results[c(
        "result_id", "sample_id", "requested_date", "result_display",
        "sd_exceedance", "cell_row_key"
      )])
      cell_table[, cell_column_index := if (report_format == "by_date") {
        match(sample_id, sample_meta$sample_id)
      } else match(requested_date, date)]
      cell_table[, cell_flagged := (!is.na(sd_exceedance) & sd_exceedance) |
        result_id %in% failed_result_ids]
      cell_values <- cell_table[, .(
        display_value = paste(unique(result_display), collapse = "; "),
        flagged = any(cell_flagged)
      ), by = .(cell_row_key, cell_column_index)]
      cell_values[, row_index := match(cell_row_key, row_groups$cell_row_key)]
      cell_values <- cell_values[
        !is.na(row_index) & !is.na(cell_column_index)
      ]
      for (i in seq_along(date_headers)) {
        cell_idx <- which(cell_values$cell_column_index == i)
        if (length(cell_idx)) {
          matrix_report[[date_headers[[i]]]][cell_values$row_index[cell_idx]] <-
            cell_values$display_value[cell_idx]
        }
      }
      flagged_cells <- cell_values[cell_values$flagged == TRUE]
      flagged_row_index <- flagged_cells$row_index
      flagged_column_index <- flagged_cells$cell_column_index
    }
    if (!nrow(matrix_report)) {
      empty_row <- stats::setNames(rep(list(NA), ncol(matrix_report)), names(matrix_report))
      empty_row[[1]] <- "No eligible results"
      matrix_report <- as.data.frame(empty_row, check.names = FALSE, stringsAsFactors = FALSE)
    }

    report_last_col <- ncol(matrix_report)
    guide_count <- length(guideline_column_names)
    if (report_format == "by_parameter") {
      location_end <- 7L
      guideline_start <- 8L
      guideline_end <- 7L + guide_count
      date_start <- 8L + guide_count
      date_end <- date_start + length(date_headers) - 1L
      context_start <- 1L
      context_end <- location_end
    } else {
      guideline_start <- 2L
      guideline_end <- 1L + guide_count
      context_start <- 2L + guide_count
      context_end <- 6L + guide_count
      date_start <- 7L + guide_count
      date_end <- date_start + length(date_headers) - 1L
    }
    openxlsx::addWorksheet(wb, sheet_name, gridLines = FALSE)
    report_title <- if (report_format == "by_parameter") {
      param_name <- requested_parameters$param_name[
        match(tab$id, requested_parameters$parameter_id)
      ]
      if (!length(param_name) || is.na(param_name) || !nzchar(param_name)) {
        param_name <- paste0("Parameter ", tab$id)
      }
      if (lang == "fr") {
        french_name <- requested_parameters$param_name_fr[
          match(tab$id, requested_parameters$parameter_id)
        ]
        if (length(french_name) && !is.na(french_name) && nzchar(french_name)) {
          param_name <- french_name
        }
      }
      paste0("WQ report for AquaCache parameter ", param_name)
    } else if (report_format == "by_location") {
      location <- requested_locations$location_code[
        match(tab$id, requested_locations$location_id)
      ]
      if (is.na(location)) location <- paste0("Location ", tab$id)
      paste0("WQ report for AquaCache location ", location)
    } else {
      paste0("WQ report for AquaCache locations  ", paste(unique(results$location), collapse = ", "))
    }
    if (report_format == "by_date") {
      target_date <- tab$id
      target_tolerance <- date_approx[match(target_date, date)]
      date_note <- paste0("Target date: ", format(target_date, "%Y-%m-%d"))
      if (target_tolerance > 0L) {
        date_note <- paste0(date_note, ". Closest eligible samples may be within ", target_tolerance, " day(s).")
      }
    } else {
      date_note <- paste0(
        "Target dates: ", format(min(date), "%Y-%m-%d"), " to ",
        format(max(date), "%Y-%m-%d"), " (", length(date), " selected)."
      )
      if (length(unique(date_approx)) == 1L && date_approx[[1]] > 0L) {
        date_note <- paste0(date_note, " Closest eligible samples may be within ", date_approx[[1]], " day(s).")
      } else if (length(unique(date_approx)) > 1L) {
        date_note <- paste0(date_note, " The tolerance varies by target date; see Result details.")
      }
    }
    openxlsx::writeData(wb, sheet_name, report_title, startRow = 1, colNames = FALSE)
    openxlsx::writeData(
      wb, sheet_name, paste0("Issued at ", generated),
      startCol = report_last_col, startRow = 1, colNames = FALSE
    )
    openxlsx::writeData(wb, sheet_name, date_note, startRow = 2, colNames = FALSE)
    openxlsx::writeData(
      wb, sheet_name,
      paste0("Created with R package YGwater ", utils::packageVersion("YGwater")),
      startCol = report_last_col, startRow = 2, colNames = FALSE
    )
    openxlsx::writeData(
      wb, sheet_name, paste(flag_note, format_note, sep = " "),
      startRow = 3, colNames = FALSE
    )
    if (guide_count) {
      openxlsx::writeData(wb, sheet_name, "Guidelines", startCol = guideline_start, startRow = 4, colNames = FALSE)
    }
    context_title <- if (report_format == "by_parameter") "Location and result details" else "Parameter details"
    date_title <- if (report_format == "by_date") "Samples (date-time UTC)" else "Target dates"
    openxlsx::writeData(
      wb, sheet_name, context_title,
      startCol = context_start, startRow = 4, colNames = FALSE
    )
    openxlsx::writeData(
      wb, sheet_name, date_title,
      startCol = date_start, startRow = 4, colNames = FALSE
    )
    openxlsx::writeData(
      wb, sheet_name, matrix_report,
      startRow = 5, withFilter = TRUE, keepNA = FALSE
    )
    for (row in 1:3) {
      if (row < 3L) {
        openxlsx::mergeCells(wb, sheet_name, cols = 1:context_end, rows = row)
      } else {
        openxlsx::mergeCells(wb, sheet_name, cols = seq_len(report_last_col), rows = row)
      }
    }
    if (guide_count > 1L) {
      openxlsx::mergeCells(wb, sheet_name, cols = guideline_start:guideline_end, rows = 4)
    }
    if (context_end > context_start) {
      openxlsx::mergeCells(wb, sheet_name, cols = context_start:context_end, rows = 4)
    }
    if (date_end > date_start) {
      openxlsx::mergeCells(wb, sheet_name, cols = date_start:date_end, rows = 4)
    }
    openxlsx::addStyle(wb, sheet_name, title_style, rows = 1:2, cols = seq_len(report_last_col), gridExpand = TRUE)
    openxlsx::addStyle(wb, sheet_name, note_style, rows = 3, cols = seq_len(report_last_col), gridExpand = TRUE)
    data_rows <- 6:(5L + nrow(matrix_report))
    openxlsx::addStyle(wb, sheet_name, parameter_style, rows = 4:5, cols = 1, gridExpand = TRUE)
    openxlsx::addStyle(wb, sheet_name, parameter_style, rows = data_rows, cols = 1)
    if (guide_count) {
      openxlsx::addStyle(
        wb, sheet_name, group_style, rows = 4:5,
        cols = guideline_start:guideline_end, gridExpand = TRUE
      )
      openxlsx::addStyle(
        wb, sheet_name, limit_style, rows = data_rows,
        cols = guideline_start:guideline_end, gridExpand = TRUE
      )
    }
    openxlsx::addStyle(
      wb, sheet_name, group_style, rows = 4:5,
      cols = context_start:context_end, gridExpand = TRUE
    )
    openxlsx::addStyle(
      wb, sheet_name, limit_style, rows = data_rows,
      cols = context_start:context_end, gridExpand = TRUE
    )
    openxlsx::addStyle(
      wb, sheet_name, sample_style, rows = 4:5,
      cols = date_start:date_end, gridExpand = TRUE
    )
    openxlsx::addStyle(
      wb, sheet_name, sample_data_style, rows = data_rows,
      cols = date_start:date_end, gridExpand = TRUE
    )
    if (report_format == "by_parameter") {
      openxlsx::setColWidths(wb, sheet_name, cols = 1, widths = 16)
      openxlsx::setColWidths(wb, sheet_name, cols = 2, widths = 26)
      openxlsx::setColWidths(wb, sheet_name, cols = 3:7, widths = 16)
    } else {
      openxlsx::setColWidths(wb, sheet_name, cols = 1, widths = 28)
      openxlsx::setColWidths(wb, sheet_name, cols = context_start:context_end, widths = 15)
    }
    if (guide_count) {
      openxlsx::setColWidths(wb, sheet_name, cols = guideline_start:guideline_end, widths = 24)
    }
    openxlsx::setColWidths(wb, sheet_name, cols = date_start:date_end, widths = if (report_format == "by_date") 23 else 16)
    openxlsx::setRowHeights(wb, sheet_name, rows = 1, heights = 24)
    openxlsx::setRowHeights(wb, sheet_name, rows = 3, heights = 34)
    openxlsx::setRowHeights(wb, sheet_name, rows = 4:5, heights = 32)
    openxlsx::freezePane(wb, sheet_name, firstActiveRow = 6, firstActiveCol = date_start)

    exceed_rows <- 5L + flagged_row_index
    exceed_cols <- date_start + flagged_column_index - 1L
    if (length(exceed_rows)) {
      openxlsx::addStyle(
        wb, sheet_name, exceed_style,
        rows = exceed_rows, cols = exceed_cols,
        gridExpand = FALSE, stack = TRUE
      )
    }
  }

  openxlsx::addWorksheet(wb, "Result details", gridLines = FALSE)
  openxlsx::writeDataTable(
    wb, "Result details", result_details,
    tableStyle = "TableStyleMedium2", withFilter = TRUE
  )
  openxlsx::freezePane(wb, "Result details", firstActiveRow = 2, firstActiveCol = 5)
  openxlsx::setColWidths(wb, "Result details", cols = 1:ncol(result_details), widths = "auto")

  openxlsx::addWorksheet(wb, "Guideline details", gridLines = FALSE)
  openxlsx::writeData(
    wb, "Guideline details", "Guideline catalogue and application",
    startRow = 1, colNames = FALSE
  )
  openxlsx::writeData(
    wb, "Guideline details",
    paste0(
      "Guidelines are evaluated at each sample date. Only active, approved versions are included; unresolved inputs are retained with their status."
    ),
    startRow = 2, colNames = FALSE
  )
  if (!length(guideline_ids)) {
    openxlsx::writeData(
      wb, "Guideline details", "No guideline IDs were selected.",
      startRow = 4, colNames = FALSE
    )
  } else if (!nrow(guideline_catalog)) {
    openxlsx::writeData(
      wb, "Guideline details", "No selected guideline IDs were visible in the catalogue.",
      startRow = 4, colNames = FALSE
    )
  } else {
    guideline_catalog$report_result_count <- vapply(
      guideline_catalog$guideline_id,
      function(id) {
        if (!nrow(guideline_rules)) return(0L)
        length(unique(guideline_rules$result_id[guideline_rules$guideline_id == id]))
      },
      integer(1)
    )
    guideline_catalog$application_status <- ifelse(
      guideline_catalog$report_result_count > 0L,
      "Applied to report results",
      "No applicable result in this report"
    )
    openxlsx::writeDataTable(
      wb, "Guideline details", guideline_catalog,
      startRow = 4, tableStyle = "TableStyleMedium2", withFilter = TRUE
    )
    rules_start <- 6L + nrow(guideline_catalog)
    if (nrow(guideline_rules)) {
      openxlsx::writeData(
        wb, "Guideline details", "Per-result rule evaluation",
        startRow = rules_start - 1L, colNames = FALSE
      )
      openxlsx::writeDataTable(
        wb, "Guideline details", guideline_rules,
        startRow = rules_start, tableStyle = "TableStyleMedium4", withFilter = TRUE
      )
    } else {
      openxlsx::writeData(
        wb, "Guideline details",
        "No selected guideline rules matched the included results.",
        startRow = rules_start, colNames = FALSE
      )
    }
  }
  title_detail_style <- openxlsx::createStyle(
    fgFill = "turquoise2", textDecoration = "bold", fontSize = 12
  )
  openxlsx::addStyle(wb, "Guideline details", title_detail_style, rows = 1, cols = 1)
  openxlsx::addStyle(wb, "Guideline details", note_style, rows = 2, cols = 1)
  openxlsx::setColWidths(wb, "Guideline details", cols = 1:60, widths = "auto")
  openxlsx::freezePane(wb, "Guideline details", firstActiveRow = 5, firstActiveCol = 1)

  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir) && !dir.create(output_dir, recursive = TRUE)) {
    stop("Could not create the output directory: ", output_dir, call. = FALSE)
  }
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)

  written_map <- NULL
  map_assets_path <- NULL
  if (include_map) {
    sites <- unique(results[c("location", "location_label", "latitude", "longitude")])
    sites <- sites[!is.na(sites$latitude) & !is.na(sites$longitude), , drop = FALSE]
    if (!nrow(sites)) {
      warning("No report locations had coordinates; the HTML map was not created.", call. = FALSE)
    } else {
      sites$popup <- paste0(
        "<strong>", htmltools::htmlEscape(as.character(sites$location)), "</strong><br>",
        htmltools::htmlEscape(as.character(sites$location_label))
      )
      map <- leaflet::leaflet(sites) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addCircleMarkers(
          lng = ~longitude, lat = ~latitude,
          label = ~location, popup = ~popup,
          radius = 5, stroke = TRUE, weight = 1, fillOpacity = 0.8
        )
      longitude_range <- range(sites$longitude)
      latitude_range <- range(sites$latitude)
      if (nrow(sites) == 1L ||
          (diff(longitude_range) == 0 && diff(latitude_range) == 0)) {
        map <- leaflet::setView(
          map, lng = sites$longitude[[1]], lat = sites$latitude[[1]], zoom = 9
        )
      } else {
        map <- leaflet::fitBounds(
          map,
          lng1 = longitude_range[[1]], lat1 = latitude_range[[1]],
          lng2 = longitude_range[[2]], lat2 = latitude_range[[2]]
        )
      }
      map_dir <- dirname(map_path)
      if (!dir.exists(map_dir) && !dir.create(map_dir, recursive = TRUE)) {
        stop("Could not create the map output directory: ", map_dir, call. = FALSE)
      }
      saved_self_contained <- tryCatch({
        htmlwidgets::saveWidget(map, map_path, selfcontained = TRUE)
        TRUE
      }, error = function(e) FALSE)
      if (!saved_self_contained) {
        warning(
          "Could not embed the map dependencies in one HTML file; saving the map with an adjacent assets directory.",
          call. = FALSE
        )
        htmlwidgets::saveWidget(map, map_path, selfcontained = FALSE)
        map_assets_path <- paste0(tools::file_path_sans_ext(map_path), "_files")
      }
      written_map <- map_path
    }
  }

  invisible(list(
    xlsx_path = normalizePath(output_path, winslash = "/", mustWork = TRUE),
    map_path = if (is.null(written_map)) NULL else normalizePath(written_map, winslash = "/", mustWork = TRUE),
    map_assets_path = if (is.null(map_assets_path) || !dir.exists(map_assets_path)) {
      NULL
    } else {
      normalizePath(map_assets_path, winslash = "/", mustWork = TRUE)
    },
    result_count = nrow(results),
    reused_sample_count = length(repeated_samples)
  ))
}
