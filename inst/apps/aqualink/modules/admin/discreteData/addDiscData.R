# UI and server code for adding discrete samples and results.

addDiscData_empty_table <- function() {
  data.frame(
    sample_key = character(),
    source_location_name = character(),
    location_mapping_status = character(),
    location_id = integer(),
    sub_location_id = integer(),
    datetime = as.POSIXct(character(), tz = "UTC"),
    target_datetime = as.POSIXct(character(), tz = "UTC"),
    z = numeric(),
    media_id = integer(),
    collection_method = integer(),
    sample_type = integer(),
    sample_group_id = integer(),
    sample_volume_ml = numeric(),
    purge_volume_l = numeric(),
    purge_time_min = numeric(),
    flow_rate_l_min = numeric(),
    wave_hgt_m = numeric(),
    sample_grade = integer(),
    sample_approval = integer(),
    sample_qualifier = integer(),
    commissioning_org = integer(),
    sampling_org = integer(),
    linked_with = integer(),
    sample_note = character(),
    owner = integer(),
    contributor = integer(),
    sample_no_source_update = logical(),
    result_no_source_update = logical(),
    source_sample_id = character(),
    lab_report_no = character(),
    lab_sample_no = character(),
    source_parameter_code = character(),
    source_parameter_name = character(),
    source_unit = character(),
    parameter_id = integer(),
    result_type = integer(),
    protocol_method = integer(),
    matrix_state_id = integer(),
    sample_fraction_id = integer(),
    result_value_type = integer(),
    result_speciation_id = integer(),
    source_result_text = character(),
    source_result = numeric(),
    source_result_condition = integer(),
    source_result_condition_value = numeric(),
    source_result_flag = character(),
    source_result_flag_column = character(),
    source_method_detection_limit = numeric(),
    source_reporting_detection_limit = numeric(),
    result = numeric(),
    result_condition = integer(),
    result_condition_value = numeric(),
    result_flag_action = character(),
    conversion = numeric(),
    result_offset = numeric(),
    laboratory = integer(),
    grade_type_id = integer(),
    approval_type_id = integer(),
    analysis_datetime = as.POSIXct(character(), tz = "UTC"),
    source_note = character(),
    note = character(),
    mapping_status = character(),
    source_code = character(),
    source_row_number = integer(),
    stringsAsFactors = FALSE
  )
}

addDiscData_empty_profiles <- function() {
  data.frame(
    import_profile_id = integer(),
    import_source_id = integer(),
    source_code = character(),
    source_name = character(),
    profile_code = character(),
    profile_name = character(),
    profile_description = character(),
    file_type = character(),
    parser_type = character(),
    sheet_strategy = character(),
    sheet_name = character(),
    sheet_index = integer(),
    header_row = integer(),
    units_row = integer(),
    parameter_row = integer(),
    data_start_row = integer(),
    datetime_origin = character(),
    timezone = character(),
    column_map = I(list()),
    wide_config = I(list()),
    defaults = I(list()),
    sample_identity = I(list()),
    result_identity = I(list()),
    validation_rules = I(list()),
    active = logical(),
    note = character(),
    stringsAsFactors = FALSE
  )
}

addDiscData_merge_preview_edits <- function(recalculated, current, previous) {
  if (!nrow(current) || !nrow(previous)) {
    return(recalculated)
  }

  key_fields <- intersect(
    c(
      "source_row_number",
      "source_sample_id",
      "source_parameter_code",
      "source_unit",
      "source_result_flag"
    ),
    names(recalculated)
  )
  if (!length(key_fields)) {
    return(recalculated)
  }
  make_keys <- function(rows, fields = key_fields) {
    values <- lapply(fields, function(name) {
      value <- as.character(rows[[name]])
      value[is.na(value)] <- ""
      value
    })
    key <- do.call(paste, c(values, sep = "\r"))
    occurrence <- ave(seq_along(key), key, FUN = seq_along)
    paste(key, occurrence, sep = "\r")
  }

  current_keys <- make_keys(current)
  previous_keys <- make_keys(previous)
  recalculated_keys <- make_keys(recalculated)
  previous_index <- match(current_keys, previous_keys)
  recalculated_index <- match(current_keys, recalculated_keys)
  fallback_fields <- intersect(
    c("source_row_number", "source_sample_id"),
    key_fields
  )
  if (length(fallback_fields) && anyNA(recalculated_index)) {
    fallback_current <- make_keys(current, fallback_fields)
    fallback_previous <- make_keys(previous, fallback_fields)
    fallback_recalculated <- make_keys(recalculated, fallback_fields)
    missing_match <- which(is.na(recalculated_index))
    fallback_old <- match(fallback_current[missing_match], fallback_previous)
    fallback_new <- match(
      fallback_current[missing_match],
      fallback_recalculated
    )
    use_fallback <- !is.na(fallback_old) & !is.na(fallback_new)
    previous_index[missing_match[use_fallback]] <- fallback_old[use_fallback]
    recalculated_index[missing_match[use_fallback]] <- fallback_new[
      use_fallback
    ]
  }
  edit_columns <- intersect(
    c(
      "location_id",
      "sub_location_id",
      "location_mapping_status",
      "datetime",
      "target_datetime",
      "z",
      "media_id",
      "collection_method",
      "sample_type",
      "sample_group_id",
      "sample_volume_ml",
      "purge_volume_l",
      "purge_time_min",
      "flow_rate_l_min",
      "wave_hgt_m",
      "sample_grade",
      "sample_approval",
      "sample_qualifier",
      "commissioning_org",
      "sampling_org",
      "linked_with",
      "sample_note",
      "owner",
      "contributor",
      "sample_no_source_update",
      "result_no_source_update",
      "parameter_id",
      "result_type",
      "protocol_method",
      "matrix_state_id",
      "sample_fraction_id",
      "result_value_type",
      "result_speciation_id",
      "result",
      "result_condition",
      "result_condition_value",
      "laboratory",
      "grade_type_id",
      "approval_type_id",
      "analysis_datetime",
      "note"
    ),
    intersect(names(current), names(previous))
  )
  rows_to_keep <- integer()

  for (i in seq_len(nrow(current))) {
    old_i <- previous_index[[i]]
    new_i <- recalculated_index[[i]]
    if (is.na(old_i)) {
      rows_to_keep <- c(rows_to_keep, i)
      next
    }

    changed <- edit_columns[vapply(
      edit_columns,
      function(name) {
        !isTRUE(all.equal(
          current[[name]][i],
          previous[[name]][old_i],
          check.attributes = FALSE
        ))
      },
      logical(1)
    )]
    if (!length(changed)) {
      next
    }
    if (is.na(new_i)) {
      rows_to_keep <- c(rows_to_keep, i)
      next
    }
    for (name in changed) {
      recalculated[[name]][new_i] <- current[[name]][i]
    }
  }

  if (length(rows_to_keep)) {
    preserved <- current[rows_to_keep, names(recalculated), drop = FALSE]
    recalculated <- data.table::rbindlist(
      list(recalculated, preserved),
      fill = TRUE,
      use.names = TRUE
    ) |>
      as.data.frame()
  }
  rownames(recalculated) <- NULL
  recalculated
}

addDiscData_sample_group_labels <- function(sample_groups) {
  if (!nrow(sample_groups)) {
    return(character())
  }
  group_code <- as.character(sample_groups$group_code)
  group_name <- as.character(sample_groups$group_name)
  group_code[is.na(group_code)] <- ""
  group_name[is.na(group_name)] <- ""
  group_code <- trimws(group_code)
  group_name <- trimws(group_name)
  identifier <- ifelse(
    nzchar(group_code),
    ifelse(
      nzchar(group_name),
      paste(group_code, group_name, sep = " — "),
      group_code
    ),
    group_name
  )
  paste(sample_groups$group_type, identifier, sep = ": ")
}

addDiscData_observer_labels <- function(observers) {
  if (!nrow(observers)) {
    return(character())
  }
  first <- trimws(as.character(observers$observer_first))
  last <- trimws(as.character(observers$observer_last))
  organization <- trimws(as.character(observers$organization))
  first[is.na(first)] <- ""
  last[is.na(last)] <- ""
  organization[is.na(organization)] <- ""
  ifelse(
    nzchar(organization),
    paste0(first, " ", last, " (", organization, ")"),
    paste(first, last)
  )
}

addDiscData_read_profiles <- function(con) {
  available <- DBI::dbGetQuery(
    con,
    "SELECT
       to_regclass('discrete.import_profiles') IS NOT NULL
       AND to_regclass('discrete.import_mapping_sets') IS NOT NULL
       AND to_regclass('discrete.import_location_mappings') IS NOT NULL
       AND EXISTS (
         SELECT 1
         FROM information_schema.columns
         WHERE table_schema = 'discrete'
           AND table_name = 'import_parameter_mappings'
           AND column_name = 'import_mapping_set_id'
       ) AS available;"
  )$available[[1]]
  if (!isTRUE(available)) {
    stop(
      "This database does not have the coherent import mapping schema. ",
      "Apply AquaCache patch 61 before using file imports.",
      call. = FALSE
    )
  }
  profiles <- AquaCache::getImportProfiles(
    con = con,
    active = TRUE,
    parse_json = TRUE
  )
  if (!nrow(profiles)) {
    return(addDiscData_empty_profiles())
  }
  profiles
}

addDiscData_profile_value <- function(profile, name, default = NULL) {
  if (!(name %in% names(profile)) || !length(profile[[name]])) {
    return(default)
  }
  value <- profile[[name]][[1]]
  if (is.null(value) || (length(value) == 1L && is.na(value))) {
    return(default)
  }
  value
}

addDiscData_profile_key <- function(source_code, profile_code) {
  # Keep option values free of carriage returns, which HTML select controls normalize.
  paste(source_code, profile_code, sep = "::")
}

addDiscData_profile_json <- function(profile, name, default = list()) {
  value <- addDiscData_profile_value(profile, name, default)
  jsonlite::toJSON(
    value,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    na = "null"
  )
}

addDiscData_profile_column_specs <- function(parser_family) {
  if (identical(parser_family, "transposed")) {
    return(list(
      lab_report_row = c("Lab report row", "integer"),
      lab_sample_row = c("Lab sample row", "integer"),
      station_code_row = c("Station/location code row", "integer"),
      sample_date_row = c("Sample date row", "integer"),
      sample_time_row = c("Sample time row", "integer"),
      comments_row = c("Comments row", "integer"),
      parameter_name_column = c("Parameter name column", "integer"),
      parameter_code_column = c("Parameter code column", "integer"),
      unit_column = c("Unit column", "integer"),
      first_sample_column = c("First sample column", "integer")
    ))
  }
  specs <- list(
    station_code = c("Station/location code column", "text"),
    sample_date = c("Sample date column", "text"),
    sample_time = c("Sample time column", "text"),
    lab_sample_id = c("Lab sample ID column", "text"),
    lab_report_no = c("Lab report number column", "text"),
    parameter_name = c("Parameter/analyte name column", "text"),
    unit = c("Unit column", "text"),
    result = c("Result column", "text"),
    result_flag = c("Source result flag/code column", "text"),
    method_detection_limit = c("Method detection limit column", "text"),
    reporting_detection_limit = c("Reporting detection limit column", "text"),
    result_comment = c("Result comment column", "text"),
    analysis_datetime = c("Analysis datetime column", "text")
  )
  if (identical(parser_family, "long")) {
    specs <- append(
      specs,
      list(parameter_code = c("Parameter code column", "text")),
      after = 6L
    )
  }
  specs
}

addDiscData_profile_column_map <- function(input, parser_family) {
  specs <- addDiscData_profile_column_specs(parser_family)
  values <- lapply(names(specs), function(name) {
    value <- input[[paste0("new_profile_col_", name)]]
    if (identical(specs[[name]][[2]], "integer")) {
      return(addDiscData_int(value))
    }
    trimws(addDiscData_first(value, ""))
  })
  names(values) <- names(specs)
  values[
    !vapply(
      values,
      function(x) {
        length(x) == 1L && (is.na(x) || identical(x, ""))
      },
      logical(1)
    )
  ]
}

addDiscData_parser_family <- function(profile) {
  validation_rules <- addDiscData_profile_value(
    profile,
    "validation_rules",
    list()
  )
  configured <- validation_rules$parser_family
  if (
    length(configured) == 1L &&
      configured %in% c("long", "transposed", "xlr")
  ) {
    return(configured)
  }

  column_map <- addDiscData_profile_value(profile, "column_map", list())
  parser_type <- addDiscData_profile_value(profile, "parser_type", "long")
  if (
    identical(parser_type, "wide") ||
      any(
        c("first_sample_column", "parameter_code_column") %in% names(column_map)
      )
  ) {
    return("transposed")
  }
  if (
    all(
      c("parameter_name", "lab_sample_id", "result") %in% names(column_map)
    ) &&
      !("parameter_code" %in% names(column_map))
  ) {
    return("xlr")
  }
  if ("parameter_code" %in% names(column_map)) {
    return("long")
  }
  NA_character_
}

addDiscData_clean_colnames <- function(x) {
  names(x) <- trimws(names(x))
  names(x)
}

addDiscData_col <- function(x, col, default = NA_character_) {
  if (is.null(col) || !nzchar(col) || !(col %in% names(x))) {
    norm <- function(value) {
      tolower(gsub("[^a-z0-9]", "", as.character(value)))
    }
    hit <- which(norm(names(x)) == norm(col))
    if (!length(hit)) {
      return(rep(default, nrow(x)))
    }
    col <- names(x)[[hit[[1]]]]
  }
  x[[col]]
}

addDiscData_cell <- function(x, row, col, default = NA_character_) {
  row <- suppressWarnings(as.integer(row))
  col <- suppressWarnings(as.integer(col))
  if (
    length(row) != 1L ||
      length(col) != 1L ||
      is.na(row) ||
      is.na(col) ||
      row < 1L ||
      col < 1L ||
      row > nrow(x) ||
      col > ncol(x)
  ) {
    return(default)
  }
  value <- x[row, col][[1]]
  if (length(value) == 0 || is.na(value)) {
    return(default)
  }
  as.character(value)
}

addDiscData_present <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

addDiscData_share_choices <- function(con, relation) {
  groups <- tryCatch(
    DBI::dbGetQuery(
      con,
      "SELECT shareable.role_name
         FROM public.get_shareable_principals_for($1::regclass) AS shareable
         JOIN pg_catalog.pg_roles AS role_catalog
           ON role_catalog.rolname = shareable.role_name
        WHERE shareable.role_name <> 'public_reader'
          AND NOT role_catalog.rolcanlogin
          AND role_catalog.rolname <> 'public'
          AND role_catalog.rolname !~ '^pg_'
        ORDER BY shareable.role_name",
      params = list(relation)
    )$role_name,
    error = function(e) character()
  )
  groups <- unique(as.character(groups))
  groups <- groups[!is.na(groups) & nzchar(groups)]
  stats::setNames(
    c("public_reader", groups),
    c("All users", groups)
  )
}

addDiscData_share_selection <- function(selected, choices) {
  if (is.null(selected) || !length(selected)) {
    return("public_reader")
  }
  selected <- unique(as.character(selected))
  selected <- selected[!is.na(selected) & nzchar(selected)]
  if (!length(selected)) {
    return("public_reader")
  }
  invalid <- setdiff(selected, unname(choices))
  if (length(invalid)) {
    stop("Choose only an available access group.", call. = FALSE)
  }
  if ("public_reader" %in% selected && length(selected) > 1L) {
    stop(
      "Remove All users before selecting access groups.",
      call. = FALSE
    )
  }
  if ("public_reader" %in% selected) {
    return("public_reader")
  }
  selected
}

addDiscData_int <- function(x, default = NA_integer_) {
  if (length(x) == 0 || is.null(x) || !addDiscData_present(x)) {
    return(default)
  }
  out <- suppressWarnings(as.integer(x))
  if (length(out) == 0 || is.na(out)) {
    return(default)
  }
  out[[1]]
}

addDiscData_num <- function(x, default = NA_real_) {
  if (length(x) == 0 || is.null(x) || !addDiscData_present(x)) {
    return(default)
  }
  out <- suppressWarnings(as.numeric(x))
  if (length(out) == 0 || is.na(out)) {
    return(default)
  }
  out[[1]]
}

addDiscData_first <- function(x, default = NA_character_) {
  if (length(x) == 0 || is.null(x)) {
    return(default)
  }
  x[[1]]
}

addDiscData_as_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(as.Date(x, tz = "UTC"))
  }
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }
  x <- trimws(as.character(x))
  out <- as.Date(rep(NA_character_, length(x)))
  format_groups <- list(
    list("^\\d{4}-\\d{1,2}-\\d{1,2}$", c("%Y-%m-%d")),
    list("^\\d{4}/\\d{1,2}/\\d{1,2}$", c("%Y/%m/%d")),
    list("^\\d{4}-[A-Za-z]{3,9}-\\d{1,2}$", c("%Y-%b-%d", "%Y-%B-%d")),
    list("^\\d{4}/[A-Za-z]{3,9}/\\d{1,2}$", c("%Y/%b/%d", "%Y/%B/%d")),
    list("^\\d{1,2}-[A-Za-z]{3,9}-\\d{4}$", c("%d-%b-%Y", "%d-%B-%Y")),
    list("^\\d{1,2}/\\d{1,2}/\\d{4}$", c("%m/%d/%Y", "%d/%m/%Y"))
  )
  for (group in format_groups) {
    candidates <- is.na(out) & grepl(group[[1]], x)
    for (fmt in group[[2]]) {
      missing <- candidates & is.na(out)
      if (!any(missing)) {
        next
      }
      out[missing] <- suppressWarnings(as.Date(x[missing], format = fmt))
    }
  }
  serial <- is.na(out) & grepl("^\\d+(?:\\.\\d+)?$", x)
  serial_value <- suppressWarnings(as.numeric(x[serial]))
  plausible <- !is.na(serial_value) &
    serial_value >= 20000 &
    serial_value <= 100000
  if (any(plausible)) {
    serial_rows <- which(serial)[plausible]
    out[serial_rows] <- as.Date(serial_value[plausible], origin = "1899-12-30")
  }
  out
}

addDiscData_datetime <- function(date, time = NA, tz = "America/Whitehorse") {
  parsed_date <- addDiscData_as_date(date)
  if (all(is.na(parsed_date))) {
    return(as.POSIXct(
      rep(NA_real_, length(parsed_date)),
      origin = "1970-01-01",
      tz = "UTC"
    ))
  }
  embedded_time <- rep("00:00:00", length(parsed_date))
  if (inherits(date, "POSIXt")) {
    embedded_time <- format(date, "%H:%M:%S", tz = "UTC")
  } else if (is.numeric(date)) {
    seconds <- round((date %% 1) * 86400) %% 86400
    embedded_time <- sprintf(
      "%02d:%02d:%02d",
      seconds %/% 3600,
      (seconds %% 3600) %/% 60,
      seconds %% 60
    )
  }

  time <- rep_len(time, length(parsed_date))
  if (inherits(time, "POSIXt")) {
    time <- format(time, "%H:%M:%S", tz = "UTC")
  } else if (is.numeric(time)) {
    seconds <- round((time %% 1) * 86400) %% 86400
    time <- sprintf(
      "%02d:%02d:%02d",
      seconds %/% 3600,
      (seconds %% 3600) %/% 60,
      seconds %% 60
    )
  } else {
    time <- trimws(as.character(time))
  }
  missing_time <- !addDiscData_present(time)
  time[missing_time] <- embedded_time[missing_time]
  value <- paste(format(parsed_date, "%Y-%m-%d"), time)
  tz <- trimws(as.character(tz[[1]]))
  offset_match <- regexec("^UTC([+-])(\\d{2}):(\\d{2})$", tz)
  offset_parts <- regmatches(tz, offset_match)[[1]]
  offset_minutes <- NA_integer_
  parse_tz <- tz
  if (length(offset_parts) == 4L) {
    offset_sign <- if (identical(offset_parts[[2]], "-")) -1L else 1L
    offset_minutes <- offset_sign *
      (as.integer(offset_parts[[3]]) * 60L + as.integer(offset_parts[[4]]))
    parse_tz <- "UTC"
  }
  parsed <- suppressWarnings(as.POSIXct(
    value,
    tz = parse_tz,
    tryFormats = c(
      "%Y-%m-%d %H:%M:%S",
      "%Y-%m-%d %H:%M",
      "%Y-%m-%d %I:%M:%S %p",
      "%Y-%m-%d %I:%M %p"
    )
  ))
  if (!is.na(offset_minutes)) {
    parsed <- parsed - offset_minutes * 60
  }
  attr(parsed, "tzone") <- "UTC"
  parsed
}

addDiscData_parse_result <- function(x) {
  raw <- trimws(as.character(x))
  condition <- rep(NA_integer_, length(raw))
  condition_value <- rep(NA_real_, length(raw))
  result <- suppressWarnings(as.numeric(raw))

  below <- grepl("^<", raw)
  above <- grepl("^>", raw)
  qualified <- below | above
  numeric_part <- suppressWarnings(as.numeric(gsub("[^0-9eE+.-]", "", raw)))
  condition[below] <- 1L
  condition[above] <- 2L
  condition_value[qualified] <- numeric_part[qualified]
  result[qualified] <- NA_real_

  data.frame(
    result = result,
    result_condition = condition,
    result_condition_value = condition_value
  )
}

addDiscData_defaults <- function(profile) {
  defaults <- profile$defaults[[1]]
  if (is.null(defaults) || !length(defaults)) {
    defaults <- list()
  }
  list(
    media_id = addDiscData_int(defaults$media_id, 1L),
    collection_method = addDiscData_int(defaults$collection_method, 27L),
    sample_type = addDiscData_int(defaults$sample_type, 34L),
    owner = addDiscData_int(defaults$owner, 1L),
    contributor = addDiscData_int(defaults$contributor),
    result_type = addDiscData_int(defaults$result_type, 2L),
    matrix_state_id = addDiscData_int(defaults$matrix_state_id, 1L),
    result_value_type = addDiscData_int(defaults$result_value_type, 1L),
    laboratory = addDiscData_int(defaults$laboratory, 2L),
    grade_type_id = addDiscData_int(defaults$grade_type_id),
    approval_type_id = addDiscData_int(defaults$approval_type_id),
    sample_no_source_update = isTRUE(defaults$sample_no_source_update),
    result_no_source_update = isTRUE(defaults$result_no_source_update)
  )
}

addDiscData_common_rows <- function(
  source_code,
  profile,
  source_location_name,
  sample_date,
  sample_time,
  source_sample_id,
  source_parameter_code,
  source_parameter_name,
  source_unit,
  result_raw,
  result_flag = NA_character_,
  result_flag_column = NA_character_,
  method_detection_limit = NA_real_,
  reporting_detection_limit = NA_real_,
  lab_report_no = NA_character_,
  note = NA_character_,
  analysis_datetime = NA,
  source_row_number = NA_integer_
) {
  defaults <- addDiscData_defaults(profile)
  tz <- profile$timezone[[1]]
  if (!addDiscData_present(tz)) {
    tz <- "America/Whitehorse"
  }
  parsed_result <- addDiscData_parse_result(result_raw)
  datetime <- addDiscData_datetime(sample_date, sample_time, tz = tz)
  analysis_datetime <- addDiscData_datetime(analysis_datetime, NA, tz = tz)
  sample_key <- ifelse(
    addDiscData_present(source_sample_id),
    as.character(source_sample_id),
    paste(source_location_name, format(datetime, "%Y-%m-%d %H:%M:%S"))
  )

  out <- data.frame(
    sample_key = sample_key,
    source_location_name = as.character(source_location_name),
    location_mapping_status = "unmapped",
    location_id = NA_integer_,
    sub_location_id = NA_integer_,
    datetime = datetime,
    media_id = defaults$media_id,
    collection_method = defaults$collection_method,
    sample_type = defaults$sample_type,
    sample_group_id = NA_integer_,
    target_datetime = as.POSIXct(NA),
    z = NA_real_,
    sample_volume_ml = NA_real_,
    purge_volume_l = NA_real_,
    purge_time_min = NA_real_,
    flow_rate_l_min = NA_real_,
    wave_hgt_m = NA_real_,
    sample_grade = NA_integer_,
    sample_approval = NA_integer_,
    sample_qualifier = NA_integer_,
    commissioning_org = NA_integer_,
    sampling_org = NA_integer_,
    linked_with = NA_integer_,
    sample_note = NA_character_,
    owner = defaults$owner,
    contributor = defaults$contributor,
    sample_no_source_update = defaults$sample_no_source_update,
    result_no_source_update = defaults$result_no_source_update,
    source_sample_id = sample_key,
    lab_report_no = as.character(lab_report_no),
    lab_sample_no = as.character(source_sample_id),
    source_parameter_code = as.character(source_parameter_code),
    source_parameter_name = as.character(source_parameter_name),
    source_unit = as.character(source_unit),
    parameter_id = NA_integer_,
    result_type = defaults$result_type,
    protocol_method = addDiscData_int(defaults$protocol_method),
    matrix_state_id = defaults$matrix_state_id,
    sample_fraction_id = NA_integer_,
    result_value_type = defaults$result_value_type,
    result_speciation_id = NA_integer_,
    source_result_text = as.character(result_raw),
    source_result = parsed_result$result,
    source_result_condition = parsed_result$result_condition,
    source_result_condition_value = parsed_result$result_condition_value,
    source_result_flag = as.character(result_flag),
    source_result_flag_column = as.character(result_flag_column),
    source_method_detection_limit = suppressWarnings(as.numeric(
      method_detection_limit
    )),
    source_reporting_detection_limit = suppressWarnings(as.numeric(
      reporting_detection_limit
    )),
    result = parsed_result$result,
    result_condition = parsed_result$result_condition,
    result_condition_value = parsed_result$result_condition_value,
    result_flag_action = NA_character_,
    conversion = 1,
    result_offset = 0,
    laboratory = defaults$laboratory,
    grade_type_id = defaults$grade_type_id,
    approval_type_id = defaults$approval_type_id,
    analysis_datetime = analysis_datetime,
    source_note = as.character(note),
    note = as.character(note),
    mapping_status = "unmapped",
    stringsAsFactors = FALSE
  )
  out$source_code <- source_code
  out$source_row_number <- source_row_number
  out
}

addDiscData_excel_sheet <- function(path, profile) {
  sheets <- readxl::excel_sheets(path)
  strategy <- addDiscData_profile_value(
    profile,
    "sheet_strategy",
    "name_or_first"
  )
  sheet_name <- addDiscData_profile_value(profile, "sheet_name", "")
  sheet_index <- addDiscData_profile_value(profile, "sheet_index")

  if (
    strategy %in%
      c("name", "name_or_first") &&
      addDiscData_present(sheet_name) &&
      sheet_name %in% sheets
  ) {
    return(sheet_name)
  }
  if (identical(strategy, "name")) {
    stop(
      "Import profile worksheet '",
      sheet_name,
      "' was not found in the workbook.",
      call. = FALSE
    )
  }
  if (identical(strategy, "index")) {
    sheet_index <- addDiscData_int(sheet_index)
    if (
      is.na(sheet_index) || sheet_index < 1L || sheet_index > length(sheets)
    ) {
      stop(
        "Import profile worksheet index is outside the workbook.",
        call. = FALSE
      )
    }
    return(sheet_index)
  }
  if (strategy %in% c("first", "name_or_first")) {
    return(1L)
  }
  stop(
    "The add discrete data module does not support sheet_strategy '",
    strategy,
    "'.",
    call. = FALSE
  )
}

addDiscData_parse_als_eqwin <- function(path, profile) {
  cmap <- profile$column_map[[1]]
  sheet <- addDiscData_excel_sheet(path, profile)
  x <- readxl::read_excel(
    path,
    sheet = sheet,
    col_names = TRUE,
    .name_repair = "minimal"
  ) |>
    as.data.frame(check.names = FALSE)
  addDiscData_clean_colnames(x)

  out <- addDiscData_common_rows(
    source_code = profile$source_code[[1]],
    profile = profile,
    source_location_name = addDiscData_col(x, cmap$station_code),
    sample_date = addDiscData_col(x, cmap$sample_date),
    sample_time = addDiscData_col(x, cmap$sample_time),
    source_sample_id = addDiscData_col(x, cmap$lab_sample_id),
    lab_report_no = addDiscData_col(x, cmap$lab_report_no),
    source_parameter_code = addDiscData_col(x, cmap$parameter_code),
    source_parameter_name = addDiscData_col(x, cmap$parameter_name),
    source_unit = addDiscData_col(x, cmap$unit),
    result_raw = addDiscData_col(x, cmap$result),
    result_flag = addDiscData_col(x, cmap$result_flag),
    result_flag_column = addDiscData_first(
      cmap$result_flag,
      NA_character_
    ),
    method_detection_limit = addDiscData_col(
      x,
      addDiscData_first(cmap$method_detection_limit, cmap$lab_mdl)
    ),
    reporting_detection_limit = addDiscData_col(
      x,
      addDiscData_first(cmap$reporting_detection_limit, cmap$lab_rdl)
    ),
    note = addDiscData_col(x, cmap$result_comment),
    analysis_datetime = addDiscData_col(x, cmap$analysis_datetime),
    source_row_number = seq_len(nrow(x)) + 1L
  )
  out[addDiscData_present(out$source_parameter_code), , drop = FALSE]
}

addDiscData_parse_als_samples <- function(path, profile) {
  cmap <- profile$column_map[[1]]
  sheet <- addDiscData_excel_sheet(path, profile)
  x <- readxl::read_excel(
    path,
    sheet = sheet,
    col_names = FALSE,
    .name_repair = "minimal"
  ) |>
    as.data.frame(check.names = FALSE)

  find_row <- function(label, fallback) {
    label <- tolower(label)
    search_cols <- seq_len(min(5L, ncol(x)))
    hit <- which(vapply(
      seq_len(nrow(x)),
      function(i) {
        any(tolower(trimws(as.character(unlist(x[i, search_cols])))) == label)
      },
      logical(1)
    ))
    if (length(hit)) {
      return(hit[[1]])
    }
    addDiscData_int(fallback)
  }
  find_col <- function(row, label, fallback) {
    label <- tolower(label)
    values <- tolower(trimws(as.character(unlist(x[row, ]))))
    hit <- which(values == label)
    if (length(hit)) {
      return(hit[[1]])
    }
    addDiscData_int(fallback)
  }

  header_row <- find_row(
    "Parameter Code",
    addDiscData_int(cmap$data_start_row, 16L) - 1L
  )
  first_sample_col <- addDiscData_int(cmap$first_sample_column, 4L)
  data_start_row <- header_row + 1L
  parameter_name_col <- find_col(
    header_row,
    "Parameter Name",
    cmap$parameter_name_column
  )
  parameter_code_col <- find_col(
    header_row,
    "Parameter Code",
    cmap$parameter_code_column
  )
  unit_col <- find_col(header_row, "Units", cmap$unit_column)
  lab_sample_row <- find_row("Lab Sample #", cmap$lab_sample_row)
  lab_report_row <- find_row("Lab Report #", cmap$lab_report_row)
  station_code_row <- find_row("Station Code", cmap$station_code_row)
  sample_date_row <- find_row("Sample Date", cmap$sample_date_row)
  sample_time_row <- find_row("Sample Time", cmap$sample_time_row)
  comments_row <- find_row("Comments", cmap$comments_row)

  rows <- list()
  sample_cols <- first_sample_col:ncol(x)
  for (col in sample_cols) {
    source_sample_id <- addDiscData_cell(x, lab_sample_row, col)
    if (!addDiscData_present(source_sample_id)) {
      next
    }
    station <- addDiscData_cell(x, station_code_row, col)
    sample_date <- addDiscData_cell(x, sample_date_row, col)
    sample_time <- addDiscData_cell(x, sample_time_row, col)
    sample_note <- addDiscData_cell(x, comments_row, col)
    lab_report_no <- addDiscData_cell(x, lab_report_row, col)

    for (row in data_start_row:nrow(x)) {
      result_raw <- addDiscData_cell(x, row, col)
      parameter_code <- addDiscData_cell(x, row, parameter_code_col)
      if (
        !addDiscData_present(result_raw) || !addDiscData_present(parameter_code)
      ) {
        next
      }
      rows[[length(rows) + 1L]] <- addDiscData_common_rows(
        source_code = profile$source_code[[1]],
        profile = profile,
        source_location_name = station,
        sample_date = sample_date,
        sample_time = sample_time,
        source_sample_id = source_sample_id,
        lab_report_no = lab_report_no,
        source_parameter_code = parameter_code,
        source_parameter_name = addDiscData_cell(x, row, parameter_name_col),
        source_unit = addDiscData_cell(x, row, unit_col),
        result_raw = result_raw,
        note = sample_note,
        analysis_datetime = NA,
        source_row_number = row
      )
    }
  }

  if (!length(rows)) {
    return(addDiscData_empty_table())
  }
  data.table::rbindlist(rows, fill = TRUE) |>
    as.data.frame()
}

addDiscData_parse_als_xlr <- function(path, profile) {
  cmap <- profile$column_map[[1]]
  sheet <- addDiscData_excel_sheet(path, profile)
  raw <- readxl::read_excel(
    path,
    sheet = sheet,
    col_names = FALSE,
    .name_repair = "minimal"
  ) |>
    as.data.frame(check.names = FALSE)
  header_row <- which(
    trimws(as.character(raw[[1]])) == "Analyte" &
      trimws(as.character(raw[[2]])) == "ALS Sample ID"
  )[1]
  if (is.na(header_row)) {
    stop("Could not find the Detailed Report header row.", call. = FALSE)
  }
  x <- raw[(header_row + 1L):nrow(raw), , drop = FALSE]
  names(x) <- as.character(unlist(raw[header_row, ], use.names = FALSE))
  addDiscData_clean_colnames(x)
  x <- x[
    addDiscData_present(addDiscData_col(x, cmap$lab_sample_id)),
    ,
    drop = FALSE
  ]
  x <- x[addDiscData_present(addDiscData_col(x, cmap$result)), , drop = FALSE]
  analyte <- addDiscData_col(x, cmap$parameter_name)
  x <- x[
    !grepl("\\(Matrix:", analyte) &
      !grepl("filtration location$", analyte, ignore.case = TRUE),
    ,
    drop = FALSE
  ]

  out <- addDiscData_common_rows(
    source_code = profile$source_code[[1]],
    profile = profile,
    source_location_name = addDiscData_col(x, cmap$station_code),
    sample_date = addDiscData_col(x, cmap$sample_date),
    sample_time = addDiscData_col(x, cmap$sample_time),
    source_sample_id = addDiscData_col(x, cmap$lab_sample_id),
    source_parameter_code = addDiscData_col(x, cmap$parameter_name),
    source_parameter_name = addDiscData_col(x, cmap$parameter_name),
    source_unit = addDiscData_col(x, cmap$unit),
    result_raw = addDiscData_col(x, cmap$result),
    result_flag = addDiscData_col(x, cmap$result_flag),
    result_flag_column = addDiscData_first(
      cmap$result_flag,
      NA_character_
    ),
    method_detection_limit = addDiscData_col(
      x,
      addDiscData_first(cmap$method_detection_limit, cmap$lab_mdl)
    ),
    reporting_detection_limit = addDiscData_col(
      x,
      addDiscData_first(cmap$reporting_detection_limit, cmap$lab_rdl)
    ),
    note = addDiscData_col(x, cmap$result_comment),
    analysis_datetime = addDiscData_col(x, cmap$analysis_datetime),
    source_row_number = seq_len(nrow(x)) + header_row
  )
  out[addDiscData_present(out$source_parameter_code), , drop = FALSE]
}

addDiscData_parse_upload <- function(path, profile) {
  code <- profile$profile_code[[1]]
  parser_family <- addDiscData_parser_family(profile)

  if (identical(parser_family, "long")) {
    return(addDiscData_parse_als_eqwin(path, profile))
  }
  if (identical(parser_family, "transposed")) {
    return(addDiscData_parse_als_samples(path, profile))
  }
  if (identical(parser_family, "xlr")) {
    return(addDiscData_parse_als_xlr(path, profile))
  }
  stop(
    "Import profile '",
    code,
    "' does not describe a supported long, transposed, or XLR layout.",
    call. = FALSE
  )
}

addDiscData_clean_location_text <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x)] <- ""
  x
}

addDiscData_location_labels <- function(locations) {
  name <- addDiscData_clean_location_text(locations$name)
  code <- if ("location_code" %in% names(locations)) {
    addDiscData_clean_location_text(locations$location_code)
  } else {
    rep("", nrow(locations))
  }
  alias <- if ("alias" %in% names(locations)) {
    addDiscData_clean_location_text(locations$alias)
  } else {
    rep("", nrow(locations))
  }
  vapply(
    seq_len(nrow(locations)),
    function(i) {
      details <- c(
        if (nzchar(code[[i]])) paste0("Code: ", code[[i]]),
        if (nzchar(alias[[i]])) paste0("Alias: ", alias[[i]])
      )
      label <- if (nzchar(name[[i]])) {
        name[[i]]
      } else {
        paste0("Location ", locations$location_id[[i]])
      }
      if (length(details)) {
        paste(label, paste(details, collapse = " | "), sep = " | ")
      } else {
        label
      }
    },
    character(1)
  )
}

addDiscData_location_choices <- function(locations, include_blank = FALSE) {
  choices <- stats::setNames(
    as.character(locations$location_id),
    addDiscData_location_labels(locations)
  )
  if (isTRUE(include_blank)) c("Select a location" = "", choices) else choices
}

addDiscData_blank_sample_kind <- function(source_location_name) {
  normalized <- tolower(gsub(
    "[^[:alnum:]]+",
    " ",
    trimws(as.character(source_location_name)),
    perl = TRUE
  ))
  kind <- rep(NA_character_, length(normalized))
  kind[grepl("\\bfield\\s+blank\\b", normalized, perl = TRUE)] <- "field"
  kind[grepl("\\btrip\\s+blank\\b", normalized, perl = TRUE)] <- "trip"
  kind
}

addDiscData_apply_blank_sample_types <- function(rows, sample_types) {
  if (!nrow(rows)) {
    return(rows)
  }
  if (!"sample_type" %in% names(rows)) {
    rows$sample_type <- NA_integer_
  }
  kind <- addDiscData_blank_sample_kind(rows$source_location_name)
  for (blank_kind in c("field", "trip")) {
    selected <- which(!is.na(kind) & kind == blank_kind)
    if (!length(selected)) {
      next
    }
    sample_type_name <- paste0("qc-sample-", blank_kind, " blank")
    sample_type_index <- match(
      sample_type_name,
      tolower(trimws(sample_types$sample_type))
    )
    if (is.na(sample_type_index)) {
      stop(
        "AquaCache is missing sample type '",
        sample_type_name,
        "'. Refresh the sample type reference data before previewing this file.",
        call. = FALSE
      )
    }
    rows$sample_type[selected] <- as.integer(
      sample_types$sample_type_id[[sample_type_index]]
    )
    rows$location_id[selected] <- NA_integer_
    rows$sub_location_id[selected] <- NA_integer_
    rows$location_mapping_status[selected] <-
      "blank sample; location not required"
  }
  rows
}

addDiscData_location_match <- function(
  rows,
  locations,
  mappings = data.frame(),
  selected_location = NULL
) {
  if (!nrow(rows)) {
    return(rows)
  }
  match_columns <- intersect(
    c("name", "location_code", "alias"),
    names(locations)
  )
  location_values <- lapply(
    locations[match_columns],
    function(x) tolower(addDiscData_clean_location_text(x))
  )
  source_names <- tolower(addDiscData_clean_location_text(
    rows$source_location_name
  ))
  blank_samples <- !is.na(addDiscData_blank_sample_kind(
    rows$source_location_name
  ))

  rows$location_id <- NA_integer_
  rows$sub_location_id <- NA_integer_
  rows$location_mapping_status <- "unmapped"
  if (nrow(mappings)) {
    if (!("profile_specific" %in% names(mappings))) {
      mappings$profile_specific <- FALSE
    }
    mapping_keys <- tolower(addDiscData_clean_location_text(
      mappings$source_location_code
    ))
    for (i in seq_along(source_names)) {
      if (blank_samples[[i]]) {
        next
      }
      hit <- which(mapping_keys == source_names[[i]])
      if (length(hit)) {
        rows$location_id[[i]] <- addDiscData_int(mappings$location_id[[hit[[
          1
        ]]]])
        rows$sub_location_id[[i]] <- addDiscData_int(
          mappings$sub_location_id[[hit[[1]]]]
        )
        rows$location_mapping_status[[i]] <- if (
          isTRUE(mappings$profile_specific[[hit[[1]]]])
        ) {
          "profile mapping"
        } else {
          "source mapping"
        }
      }
    }
  }
  for (i in seq_along(source_names)) {
    if (
      blank_samples[[i]] ||
        !nzchar(source_names[[i]]) ||
        !is.na(rows$location_id[[i]])
    ) {
      next
    }
    hit <- unique(unlist(lapply(location_values, function(values) {
      which(values == source_names[[i]])
    })))
    if (length(hit) == 1L) {
      rows$location_id[[i]] <- locations$location_id[[hit[[1]]]]
      rows$location_mapping_status[[i]] <- "name/code/alias match"
    }
  }
  if (!is.null(selected_location) && length(selected_location)) {
    fallback <- addDiscData_int(selected_location[[1]])
    default_rows <- is.na(rows$location_id) & !blank_samples
    rows$location_id[default_rows] <- fallback
    rows$location_mapping_status[
      rows$location_id == fallback & rows$location_mapping_status == "unmapped"
    ] <- "manual default"
  }
  rows$location_mapping_status[blank_samples] <-
    "blank sample; location not required"
  rows
}

addDiscData_assign_sample_locations <- function(
  rows,
  sample_keys,
  location_id = NA_integer_,
  sub_location_id = NA_integer_
) {
  selected <- rows$sample_key %in% sample_keys
  rows$location_id[selected] <- addDiscData_int(location_id)
  rows$sub_location_id[selected] <- addDiscData_int(sub_location_id)
  rows
}

addDiscData_fetch_mappings <- function(con, source_code, profile_code = NULL) {
  available <- DBI::dbGetQuery(
    con,
    "SELECT to_regclass('discrete.import_parameter_mappings') IS NOT NULL AS available;"
  )$available[[1]]
  if (!isTRUE(available) || !addDiscData_present(source_code)) {
    return(data.frame())
  }

  AquaCache::getImportParameterMappings(
    con = con,
    source_code = source_code,
    profile_code = if (addDiscData_present(profile_code)) {
      profile_code
    } else {
      NULL
    },
    active = TRUE,
    include_draft = TRUE
  )
}

addDiscData_fetch_result_flag_mappings <- function(
  con,
  source_code,
  profile_code = NULL
) {
  available <- DBI::dbGetQuery(
    con,
    "SELECT to_regclass('discrete.import_result_flag_mappings') IS NOT NULL AS available;"
  )$available[[1]]
  if (!isTRUE(available) || !addDiscData_present(source_code)) {
    return(data.frame())
  }

  AquaCache::getImportResultFlagMappings(
    con = con,
    source_code = source_code,
    profile_code = if (addDiscData_present(profile_code)) {
      profile_code
    } else {
      NULL
    },
    active = TRUE,
    include_draft = TRUE
  )
}

addDiscData_apply_result_flag_mapping <- function(rows, row, mapping) {
  threshold_source <- as.character(mapping$result_condition_value_source[[1]])
  threshold <- switch(
    threshold_source,
    result = if (!is.na(rows$result[[row]])) {
      rows$result[[row]]
    } else {
      rows$result_condition_value[[row]]
    },
    method_detection_limit = rows$source_method_detection_limit[[row]] *
      rows$conversion[[row]] +
      rows$result_offset[[row]],
    reporting_detection_limit = rows$source_reporting_detection_limit[[row]] *
      rows$conversion[[row]] +
      rows$result_offset[[row]],
    literal = suppressWarnings(as.numeric(
      mapping$result_condition_value_literal[[1]]
    )),
    none = NA_real_,
    NA_real_
  )
  if (!length(threshold) || !is.finite(threshold)) {
    threshold <- NA_real_
  }

  action <- as.character(mapping$result_action[[1]])
  rows$result_condition[[row]] <- addDiscData_int(
    mapping$result_condition_id[[1]],
    rows$result_condition[[row]]
  )
  rows$result_condition_value[[row]] <- threshold
  rows$result_flag_action[[row]] <- action
  if (action %in% c("set_result_null", "skip_result")) {
    rows$result[[row]] <- NA_real_
  }
  note_template <- trimws(as.character(mapping$note_template[[1]]))
  if (addDiscData_present(note_template)) {
    current_note <- trimws(as.character(rows$note[[row]]))
    rows$note[[row]] <- if (addDiscData_present(current_note)) {
      paste(current_note, note_template, sep = "; ")
    } else {
      note_template
    }
  }
  rows
}

addDiscData_mapping_keys <- function(source_match) {
  x <- jsonlite::fromJSON(source_match, simplifyVector = FALSE)
  code <- x$parameter_code
  if (is.null(code)) {
    code <- x$input_param
  }
  unit <- x$unit
  if (is.null(unit)) {
    unit <- x$input_unit
  }
  code <- trimws(as.character(addDiscData_first(code, "")))
  unit <- trimws(as.character(addDiscData_first(unit, "")))
  key <- paste(tolower(code), tolower(unit), sep = "\r")
  if (!addDiscData_present(unit)) {
    key <- c(key, paste(tolower(code), "", sep = "\r"))
  }
  unique(key)
}

addDiscData_apply_mappings <- function(rows, con, profile_code = NULL) {
  if (!nrow(rows) || !("source_code" %in% names(rows))) {
    return(rows)
  }
  file_rows <- addDiscData_present(rows$source_code) &
    addDiscData_present(rows$source_parameter_code)
  rows$mapping_status <- "manual"
  rows$mapping_status[file_rows] <- "unmapped"
  if ("source_result" %in% names(rows)) {
    rows$result[file_rows] <- rows$source_result[file_rows]
  }
  if ("source_result_condition_value" %in% names(rows)) {
    rows$result_condition_value[file_rows] <-
      rows$source_result_condition_value[file_rows]
  }
  if ("source_result_condition" %in% names(rows)) {
    rows$result_condition[file_rows] <- rows$source_result_condition[file_rows]
  }
  if ("source_note" %in% names(rows)) {
    rows$note[file_rows] <- rows$source_note[file_rows]
  }
  rows$conversion[file_rows] <- 1
  rows$result_offset[file_rows] <- 0
  rows$result_flag_action[file_rows] <- NA_character_

  for (source_code in unique(rows$source_code[file_rows])) {
    mappings <- addDiscData_fetch_mappings(con, source_code, profile_code)
    if (!nrow(mappings)) {
      next
    }
    mapping_list <- list()
    for (i in seq_len(nrow(mappings))) {
      keys <- addDiscData_mapping_keys(mappings$source_match[[i]])
      for (key in keys) {
        if (!nzchar(sub("\r$", "", key))) {
          next
        }
        if (is.null(mapping_list[[key]])) {
          mapping_list[[key]] <- mappings[i, , drop = FALSE]
        }
      }
    }

    hit_rows <- which(rows$source_code == source_code)
    for (i in hit_rows) {
      code <- tolower(trimws(rows$source_parameter_code[[i]]))
      unit <- tolower(trimws(rows$source_unit[[i]]))
      hit <- mapping_list[[paste(code, unit, sep = "\r")]]
      if (is.null(hit)) {
        hit <- mapping_list[[paste(code, "", sep = "\r")]]
      }
      if (is.null(hit)) {
        next
      }
      rows$parameter_id[[i]] <- addDiscData_int(hit$parameter_id[[1]])
      rows$result_type[[i]] <- addDiscData_int(
        hit$result_type[[1]],
        rows$result_type[[i]]
      )
      rows$sample_fraction_id[[i]] <- addDiscData_int(hit$sample_fraction_id[[
        1
      ]])
      rows$result_value_type[[i]] <- addDiscData_int(
        hit$result_value_type[[1]],
        rows$result_value_type[[i]]
      )
      rows$result_speciation_id[[
        i
      ]] <- addDiscData_int(hit$result_speciation_id[[1]])
      rows$matrix_state_id[[i]] <- addDiscData_int(
        hit$matrix_state_id[[1]],
        rows$matrix_state_id[[i]]
      )
      conversion <- addDiscData_num(hit$conversion[[1]], 1)
      result_offset <- addDiscData_num(hit$result_offset[[1]], 0)
      rows$conversion[[i]] <- conversion
      rows$result_offset[[i]] <- result_offset
      if (!is.na(rows$result[[i]])) {
        rows$result[[i]] <- rows$result[[i]] * conversion + result_offset
      }
      if (!is.na(rows$result_condition_value[[i]])) {
        rows$result_condition_value[[i]] <-
          rows$result_condition_value[[i]] * conversion + result_offset
      }
      rows$mapping_status[[i]] <- "mapped"
    }

    result_flag_mappings <- addDiscData_fetch_result_flag_mappings(
      con,
      source_code,
      profile_code
    )
    if (!nrow(result_flag_mappings)) {
      next
    }
    result_flag_rows <- hit_rows[
      addDiscData_present(rows$source_result_flag[hit_rows])
    ]
    for (i in result_flag_rows) {
      value_key <- tolower(trimws(rows$source_result_flag[[i]]))
      column_key <- tolower(trimws(rows$source_result_flag_column[[i]]))
      mapped_value <- tolower(trimws(result_flag_mappings$source_flag_value))
      mapped_column <- tolower(trimws(result_flag_mappings$source_flag_column))
      hit <- which(
        mapped_value == value_key &
          (is.na(result_flag_mappings$source_flag_column) |
            !nzchar(mapped_column) |
            mapped_column == column_key)
      )
      if (!length(hit)) {
        next
      }
      candidates <- result_flag_mappings[hit, , drop = FALSE]
      exact_column <- !is.na(candidates$source_flag_column) &
        nzchar(trimws(candidates$source_flag_column)) &
        tolower(trimws(candidates$source_flag_column)) == column_key
      candidates <- candidates[
        order(
          -as.integer(candidates$profile_specific),
          -as.integer(exact_column),
          candidates$priority,
          candidates$import_result_flag_mapping_id
        ),
        ,
        drop = FALSE
      ]
      rows <- addDiscData_apply_result_flag_mapping(rows, i, candidates[1, ])
    }
  }
  rows
}

addDiscData_upsert_mapping <- function(
  con,
  source_code,
  source_name,
  parameter_code,
  unit,
  parameter_id,
  result_type,
  sample_fraction_id,
  result_value_type,
  result_speciation_id,
  matrix_state_id,
  conversion,
  result_offset,
  note,
  profile_code = NULL
) {
  mappings <- data.frame(
    parameter_code = as.character(parameter_code),
    unit = as.character(unit),
    parameter_id = as.integer(parameter_id),
    result_type = as.integer(result_type),
    sample_fraction_id = as.integer(sample_fraction_id),
    result_value_type = as.integer(result_value_type),
    result_speciation_id = as.integer(result_speciation_id),
    matrix_state_id = as.integer(matrix_state_id),
    conversion = as.numeric(conversion),
    result_offset = as.numeric(result_offset),
    priority = 50L,
    active = TRUE,
    note = as.character(note),
    stringsAsFactors = FALSE
  )
  AquaCache::upsertImportParameterMappings(
    con = con,
    source_code = source_code,
    source_name = source_name,
    source_description = "Created from YGwater add discrete data.",
    profile_code = profile_code,
    mappings = mappings,
    match_columns = c("parameter_code", "unit"),
    publish = FALSE
  )
}

addDiscData_run_mapping_save <- function(request) {
  config <- request$config
  con <- YGwater::AquaConnect(
    name = config$dbName,
    host = config$dbHost,
    port = config$dbPort,
    username = config$dbUser,
    password = config$dbPass,
    silent = TRUE
  )
  if (is.null(con)) {
    stop("Could not connect to the dev AquaCache database.")
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  saved <- FALSE
  tryCatch(
    {
      DBI::dbWithTransaction(con, {
        if (identical(request$kind, "parameter")) {
          addDiscData_upsert_mapping(
            con = con,
            source_code = request$row$source_code[[1]],
            source_name = request$row$source_code[[1]],
            parameter_code = request$row$source_parameter_code[[1]],
            unit = request$row$source_unit[[1]],
            parameter_id = request$parameter_id,
            result_type = request$result_type,
            sample_fraction_id = request$sample_fraction_id,
            result_value_type = request$result_value_type,
            result_speciation_id = request$result_speciation_id,
            matrix_state_id = request$matrix_state_id,
            conversion = request$conversion,
            result_offset = request$result_offset,
            note = "Saved from YGwater add discrete data mapping editor.",
            profile_code = request$profile$profile_code[[1]]
          )
        } else {
          AquaCache::upsertImportLocationMappings(
            con = con,
            source_code = request$profile$source_code[[1]],
            source_name = request$profile$source_code[[1]],
            profile_code = request$profile$profile_code[[1]],
            mappings = data.frame(
              source_location_code = request$row$source_location_name[[1]],
              source_location_name = request$row$source_location_name[[1]],
              location_id = request$location_id,
              sub_location_id = request$sub_location_id,
              priority = 50L,
              active = TRUE,
              note = "Saved from YGwater add discrete data location mapping editor."
            ),
            publish = FALSE
          )
        }
      })
      saved <- TRUE
      parsed <- NULL
      if (isTRUE(request$refresh_preview)) {
        parsed <- addDiscData_parse_upload(request$path, request$profile)
        location_mappings <- AquaCache::getImportLocationMappings(
          con = con,
          source_code = request$profile$source_code[[1]],
          profile_code = request$profile$profile_code[[1]],
          active = TRUE,
          include_draft = TRUE
        )
        parsed <- addDiscData_location_match(
          parsed,
          request$locations,
          location_mappings
        )
        parsed <- addDiscData_apply_blank_sample_types(
          parsed,
          request$sample_types
        )
        parsed <- addDiscData_apply_mappings(
          parsed,
          con,
          profile_code = request$profile$profile_code[[1]]
        )
        parsed <- parsed[names(addDiscData_empty_table())]
      }
      list(
        ok = TRUE,
        saved = TRUE,
        request = request,
        parsed = parsed
      )
    },
    error = function(e) {
      list(
        ok = FALSE,
        saved = saved,
        request = request,
        message = conditionMessage(e)
      )
    }
  )
}

addDiscData_run_location_create <- function(request) {
  config <- request$config
  con <- YGwater::AquaConnect(
    name = config$dbName,
    host = config$dbHost,
    port = config$dbPort,
    username = config$dbUser,
    password = config$dbPass,
    silent = TRUE
  )
  if (is.null(con)) {
    stop("Could not connect to the dev AquaCache database.")
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  location_rows <- request$location_rows
  latitude <- location_rows$latitude
  longitude <- location_rows$longitude
  elevation_details <- lapply(seq_len(nrow(location_rows)), function(i) {
    tryCatch(
      AquaCache::get_elevation(
        lat = latitude[[i]],
        lon = longitude[[i]],
        details = TRUE
      ),
      error = function(e) NULL
    )
  })
  elevation_available <- vapply(
    elevation_details,
    function(details) {
      if (!is.list(details)) {
        return(FALSE)
      }
      elevation <- suppressWarnings(as.numeric(details$elevation))
      vertical_datum <- details$vertical_datum
      length(elevation) == 1L &&
        is.finite(elevation) &&
        length(vertical_datum) == 1L &&
        !is.na(vertical_datum) &&
        nzchar(trimws(as.character(vertical_datum)))
    },
    logical(1)
  )
  elevation_fallback <- !elevation_available
  location_rows$conversion_m <- ifelse(elevation_fallback, 0, NA_real_)
  location_rows$datum_id_from <- NA_integer_
  location_rows$datum_id_to <- NA_integer_
  location_rows$elevation_details <- I(elevation_details)

  active <- FALSE
  tryCatch(
    {
      active <- AquaCache::dbTransBegin(con)
      tryCatch(
        {
          added <- AquaCache::addACLocation(
            con = con,
            df = location_rows
          )
          for (i in seq_along(request$sources)) {
            if (length(request$networks[[i]]) > 1L) {
              for (network_id in request$networks[[i]][-1L]) {
                DBI::dbExecute(
                  con,
                  "INSERT INTO public.locations_networks (location_id, network_id) VALUES ($1, $2)",
                  params = list(added$location_id[[i]], network_id)
                )
              }
            }
            if (length(request$projects[[i]]) > 1L) {
              for (project_id in request$projects[[i]][-1L]) {
                DBI::dbExecute(
                  con,
                  "INSERT INTO public.locations_projects (location_id, project_id) VALUES ($1, $2)",
                  params = list(added$location_id[[i]], project_id)
                )
              }
            }
          }
          mapping_rows <- data.frame(
            source_location_code = request$sources,
            source_location_name = request$sources,
            location_id = as.integer(added$location_id),
            stringsAsFactors = FALSE
          )
          AquaCache::upsertImportLocationMappings(
            con = con,
            source_code = request$profile$source_code[[1]],
            profile_code = request$profile$profile_code[[1]],
            mappings = mapping_rows,
            publish = FALSE
          )
          if (active) {
            DBI::dbExecute(con, "COMMIT")
          }
          active <- FALSE
          list(
            ok = TRUE,
            added = added,
            profile = request$profile,
            names = request$names,
            elevation_fallback = elevation_fallback
          )
        },
        error = function(e) {
          if (active) {
            try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
            active <- FALSE
          }
          stop(e)
        }
      )
    },
    error = function(e) {
      list(ok = FALSE, message = conditionMessage(e))
    }
  )
}

addDiscData_run_upload <- function(request) {
  config <- request$config
  con <- YGwater::AquaConnect(
    name = config$dbName,
    host = config$dbHost,
    port = config$dbPort,
    username = config$dbUser,
    password = config$dbPass,
    silent = TRUE
  )
  if (is.null(con)) {
    stop("Could not connect to the dev AquaCache database.")
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  active <- FALSE
  tryCatch(
    {
      DBI::dbExecute(con, "BEGIN")
      active <- TRUE

      default_document_type <- function() {
        type <- DBI::dbGetQuery(
          con,
          "SELECT document_type_en
           FROM files.document_types
           ORDER BY
             CASE WHEN document_type_id = 1 THEN 0 ELSE 1 END,
             document_type_en
           LIMIT 1;"
        )
        if (!nrow(type)) {
          stop("No document types are available for uploaded documents.")
        }
        type$document_type_en[[1]]
      }

      insert_document <- function(file) {
        document <- readBin(
          file$datapath,
          "raw",
          as.integer(file.info(file$datapath)$size)
        )
        existing <- DBI::dbGetQuery(
          con,
          "SELECT document_id, name
           FROM files.documents
           WHERE file_hash = md5(encode($1::bytea, 'hex'))
           LIMIT 1;",
          params = list(list(document))
        )
        if (nrow(existing)) {
          return(as.integer(existing$document_id[[1]]))
        }
        result <- AquaCache::insertACDocument(
          path = file$datapath,
          name = file$name,
          type = default_document_type(),
          description = sprintf(
            "Uploaded %s from the discrete data upload workflow.",
            file$name
          ),
          tags = c("discrete sample", "discrete upload"),
          share_with = "public_reader",
          geoms = NULL,
          con = con
        )
        as.integer(result$new_document_id)
      }

      sample_document_fields <- DBI::dbListFields(
        con,
        DBI::Id(schema = "discrete", table = "sample_documents")
      )
      sample_document_has_link_source <-
        "link_source" %in% sample_document_fields

      link_sample_documents <- function(sample_id, document_ids) {
        document_ids <- unique(as.integer(document_ids))
        document_ids <- document_ids[!is.na(document_ids)]
        for (document_id in document_ids) {
          if (sample_document_has_link_source) {
            DBI::dbExecute(
              con,
              "INSERT INTO discrete.sample_documents (
                 sample_id,
                 document_id,
                 document_role,
                 link_source
               ) VALUES ($1, $2, 'supporting', 'addDiscData')
               ON CONFLICT (sample_id, document_id) DO NOTHING;",
              params = list(as.integer(sample_id), document_id)
            )
          } else {
            DBI::dbExecute(
              con,
              "INSERT INTO discrete.sample_documents (
                 sample_id,
                 document_id,
                 document_role
               ) VALUES ($1, $2, 'supporting')
               ON CONFLICT (sample_id, document_id) DO NOTHING;",
              params = list(as.integer(sample_id), document_id)
            )
          }
        }
        invisible(NULL)
      }

      doc_ids <- integer()
      if (!is.null(request$file)) {
        doc_ids <- c(doc_ids, insert_document(request$file))
      }
      if (length(request$attachments)) {
        for (file in request$attachments) {
          doc_ids <- c(doc_ids, insert_document(file))
        }
      }

      df <- request$df
      inserted_samples <- 0L
      inserted_results <- 0L
      sample_lookup <- list()
      samples <- unique(df[, c(
        "sample_key",
        "location_id",
        "sub_location_id",
        "datetime",
        "target_datetime",
        "z",
        "media_id",
        "collection_method",
        "sample_type",
        "sample_group_id",
        "linked_with",
        "sample_volume_ml",
        "purge_volume_l",
        "purge_time_min",
        "flow_rate_l_min",
        "wave_hgt_m",
        "sample_grade",
        "sample_approval",
        "commissioning_org",
        "sampling_org",
        "sample_note",
        "owner",
        "contributor",
        "sample_no_source_update",
        "source_sample_id",
        "source_code"
      )])
      field_visit_id <- addDiscData_int(request$field_visit_id)
      samples$field_visit_id <- rep(NA_integer_, nrow(samples))
      if (!is.na(field_visit_id)) {
        visits <- DBI::dbGetQuery(
          con,
          "SELECT field_visit_id, location_id, sub_location_id
           FROM field.field_visits"
        )
        visit_index <- match(field_visit_id, visits$field_visit_id)
        if (is.na(visit_index)) {
          stop(
            "The selected field visit is no longer available. Refresh the visit list and choose it again.",
            call. = FALSE
          )
        }
        visit <- visits[visit_index, , drop = FALSE]
        invalid_location <- is.na(samples$location_id) |
          samples$location_id != visit$location_id[[1]]
        if (any(invalid_location)) {
          stop(
            "Every sample linked to a field visit must use the visit's location.",
            call. = FALSE
          )
        }
        if (!is.na(visit$sub_location_id[[1]])) {
          invalid_sub_location <- is.na(samples$sub_location_id) |
            samples$sub_location_id != visit$sub_location_id[[1]]
          if (any(invalid_sub_location)) {
            stop(
              "Every sample linked to this field visit must use the visit's sub-location.",
              call. = FALSE
            )
          }
        }
        samples$field_visit_id[] <- field_visit_id
      }
      df$field_visit_id <- samples$field_visit_id[
        match(df$sample_key, samples$sample_key)
      ]
      sample_table_fields <- DBI::dbListFields(
        con,
        DBI::Id(schema = "discrete", table = "samples")
      )
      sample_has_linked_with <- "linked_with" %in% sample_table_fields
      sample_has_field_visit_id <- "field_visit_id" %in% sample_table_fields
      sample_has_share_with <- "share_with" %in% sample_table_fields
      if (!sample_has_share_with) {
        stop(
          "The discrete.samples table does not support sharing. Apply the current AquaCache schema before uploading samples.",
          call. = FALSE
        )
      }
      if (!is.na(field_visit_id) && !sample_has_field_visit_id) {
        stop(
          "The discrete.samples table does not support field visit links. Apply the AquaCache field visit schema patch before linking samples.",
          call. = FALSE
        )
      }
      sample_insert_fields <- c(
        "location_id",
        "sub_location_id",
        "media_id",
        "z",
        "datetime",
        "target_datetime",
        "collection_method",
        "sample_type",
        if (sample_has_linked_with) "linked_with",
        "sample_volume_ml",
        "purge_volume_l",
        "purge_time_min",
        "flow_rate_l_min",
        "wave_hgt_m",
        "sample_grade",
        "sample_approval",
        "owner",
        "contributor",
        "comissioning_org",
        "sampling_org",
        "source_adapter_function",
        "external_sample_id",
        "import_source_id",
        "no_source_update",
        "note",
        if (sample_has_share_with) "share_with",
        if (sample_has_field_visit_id) "field_visit_id"
      )
      sample_insert_values <- sprintf(
        "$%d",
        seq_along(sample_insert_fields)
      )
      share_with_index <- match("share_with", sample_insert_fields)
      if (!is.na(share_with_index)) {
        sample_insert_values[[share_with_index]] <- paste0(
          sample_insert_values[[share_with_index]],
          "::text[]"
        )
      }
      sample_insert_sql <- paste0(
        "INSERT INTO discrete.samples (",
        paste(sample_insert_fields, collapse = ", "),
        ") VALUES (",
        paste(sample_insert_values, collapse = ", "),
        ") RETURNING sample_id"
      )
      import_sources <- DBI::dbGetQuery(
        con,
        "SELECT import_source_id, source_code
         FROM discrete.import_sources"
      )
      samples$import_source_id <- import_sources$import_source_id[
        match(samples$source_code, import_sources$source_code)
      ]
      file_sample <- samples$source_code != "YGwater-manual"
      if (any(file_sample & is.na(samples$import_source_id))) {
        stop(
          "Every uploaded sample must resolve to a database import source.",
          call. = FALSE
        )
      }
      import_run_id <- NULL
      profile <- request$profile
      if (any(file_sample)) {
        if (is.null(profile) || !nrow(profile)) {
          stop("Choose a workbook format before uploading file samples.")
        }
        run_source_ids <- unique(samples$import_source_id[file_sample])
        if (length(run_source_ids) != 1L) {
          stop(
            "A file upload must resolve to exactly one import source.",
            call. = FALSE
          )
        }
        run_source_code <- import_sources$source_code[
          match(run_source_ids[[1]], import_sources$import_source_id)
        ]
        AquaCache::publishImportMappings(
          con = con,
          source_code = run_source_code
        )
        AquaCache::publishImportMappings(
          con = con,
          source_code = run_source_code,
          profile_code = profile$profile_code[[1]]
        )
        import_run_id <- AquaCache::createImportRun(
          con = con,
          import_source_id = run_source_ids[[1]],
          import_profile_id = profile$import_profile_id[[1]],
          source_adapter_function = "addDiscData",
          adapter_version = as.character(utils::packageVersion("YGwater")),
          source_file_name = request$file$name,
          source_file_size = request$file$size,
          summary = list(
            source_rows = length(unique(df$source_row_number)),
            result_rows = nrow(df)
          ),
          note = "Created by the YGwater discrete-data import workflow."
        )
      }

      for (i in seq_len(nrow(samples))) {
        sid <- DBI::dbGetQuery(
          con,
          sample_insert_sql,
          params = c(
            list(
              as.integer(samples$location_id[[i]]),
              addDiscData_int(samples$sub_location_id[[i]]),
              as.integer(samples$media_id[[i]]),
              addDiscData_num(samples$z[[i]]),
              as.POSIXct(samples$datetime[[i]], tz = "UTC"),
              as.POSIXct(samples$target_datetime[[i]], tz = "UTC"),
              as.integer(samples$collection_method[[i]]),
              as.integer(samples$sample_type[[i]])
            ),
            if (sample_has_linked_with) {
              list(addDiscData_int(samples$linked_with[[i]]))
            },
            list(
              addDiscData_num(samples$sample_volume_ml[[i]]),
              addDiscData_num(samples$purge_volume_l[[i]]),
              addDiscData_num(samples$purge_time_min[[i]]),
              addDiscData_num(samples$flow_rate_l_min[[i]]),
              addDiscData_num(samples$wave_hgt_m[[i]]),
              addDiscData_int(samples$sample_grade[[i]]),
              addDiscData_int(samples$sample_approval[[i]]),
              as.integer(samples$owner[[i]]),
              addDiscData_int(samples$contributor[[i]]),
              addDiscData_int(samples$commissioning_org[[i]]),
              addDiscData_int(samples$sampling_org[[i]]),
              if (file_sample[[i]]) "addDiscData" else NA_character_,
              if (file_sample[[i]]) {
                samples$source_sample_id[[i]]
              } else {
                NA_character_
              },
              addDiscData_int(samples$import_source_id[[i]]),
              isTRUE(samples$sample_no_source_update[[i]]),
              if (addDiscData_present(samples$sample_note[[i]])) {
                samples$sample_note[[i]]
              } else if (file_sample[[i]]) {
                paste("Imported source sample:", samples$source_sample_id[[i]])
              } else {
                NA_character_
              }
            ),
            if (sample_has_share_with) {
              share_with <- request$sample_share_with[[
                as.character(samples$sample_key[[i]])
              ]]
              if (is.null(share_with) || !length(share_with)) {
                share_with <- "public_reader"
              }
              list(share_with_to_array(share_with))
            },
            if (sample_has_field_visit_id) {
              list(addDiscData_int(samples$field_visit_id[[i]]))
            }
          )
        )$sample_id[[1]]
        link_sample_documents(sid, doc_ids)
        if (!is.na(samples$sample_group_id[[i]])) {
          DBI::dbExecute(
            con,
            "INSERT INTO discrete.sample_group_members (
               sample_group_id, sample_id, sequence_in_group
             ) VALUES ($1, $2, $3)",
            params = list(
              as.integer(samples$sample_group_id[[i]]),
              as.integer(sid),
              as.integer(i)
            )
          )
        }
        qualifier_ids <- request$sample_qualifiers[[samples$sample_key[[i]]]]
        qualifier_ids <- unique(as.integer(qualifier_ids))
        qualifier_ids <- qualifier_ids[!is.na(qualifier_ids)]
        for (qualifier_id in qualifier_ids) {
          DBI::dbExecute(
            con,
            "INSERT INTO discrete.sample_qualifiers (sample_id, qualifier_type_id)
             VALUES ($1, $2)
             ON CONFLICT (sample_id, qualifier_type_id) DO NOTHING",
            params = list(as.integer(sid), qualifier_id)
          )
        }
        observer_ids <- unique(as.integer(
          request$sample_observers[[samples$sample_key[[i]]]]
        ))
        observer_ids <- observer_ids[!is.na(observer_ids)]
        for (observer_id in observer_ids) {
          DBI::dbExecute(
            con,
            "INSERT INTO discrete.sample_observers (
               sample_id, observer_id, observer_role
             ) VALUES ($1, $2, 'sampler')
             ON CONFLICT (sample_id, observer_id, observer_role) DO NOTHING",
            params = list(as.integer(sid), observer_id)
          )
        }
        sample_lookup[[samples$sample_key[[i]]]] <- sid
        inserted_samples <- inserted_samples + 1L
      }

      result_ids <- integer(nrow(df))
      for (j in seq_len(nrow(df))) {
        sid <- sample_lookup[[df$sample_key[[j]]]]
        result_ids[[j]] <- DBI::dbGetQuery(
          con,
          "INSERT INTO discrete.results (
             sample_id,
             result_type,
             parameter_id,
             protocol_method,
             sample_fraction_id,
             result,
             result_condition,
             result_condition_value,
             result_value_type,
             result_speciation_id,
             laboratory,
             analysis_datetime,
             lab_report_no,
             lab_sample_no,
             grade_type_id,
             approval_type_id,
             matrix_state_id,
             no_source_update,
             note
           ) VALUES (
             $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13,
             $14, $15, $16, $17, $18, $19
           ) RETURNING result_id",
          params = list(
            sid,
            as.integer(df$result_type[[j]]),
            as.integer(df$parameter_id[[j]]),
            addDiscData_int(df$protocol_method[[j]]),
            addDiscData_int(df$sample_fraction_id[[j]]),
            addDiscData_num(df$result[[j]]),
            addDiscData_int(df$result_condition[[j]]),
            addDiscData_num(df$result_condition_value[[j]]),
            addDiscData_int(df$result_value_type[[j]]),
            addDiscData_int(df$result_speciation_id[[j]]),
            addDiscData_int(df$laboratory[[j]]),
            if (is.na(df$analysis_datetime[[j]])) {
              as.POSIXct(NA)
            } else {
              as.POSIXct(df$analysis_datetime[[j]], tz = "UTC")
            },
            if (addDiscData_present(df$lab_report_no[[j]])) {
              as.character(df$lab_report_no[[j]])
            } else {
              NA_character_
            },
            if (addDiscData_present(df$lab_sample_no[[j]])) {
              as.character(df$lab_sample_no[[j]])
            } else {
              NA_character_
            },
            addDiscData_int(df$grade_type_id[[j]]),
            addDiscData_int(df$approval_type_id[[j]]),
            as.integer(df$matrix_state_id[[j]]),
            isTRUE(df$result_no_source_update[[j]]),
            df$note[[j]]
          )
        )$result_id[[1]]
        inserted_results <- inserted_results + 1L
      }

      if (!is.null(import_run_id)) {
        source_columns <- c(
          "source_location_name",
          "source_sample_id",
          "source_parameter_code",
          "source_parameter_name",
          "source_unit",
          "source_result_text",
          "lab_report_no",
          "lab_sample_no",
          "analysis_datetime"
        )
        sample_columns <- c(
          "sample_key",
          "location_id",
          "sub_location_id",
          "datetime",
          "target_datetime",
          "z",
          "media_id",
          "collection_method",
          "sample_type",
          "sample_group_id",
          "linked_with",
          "sample_volume_ml",
          "purge_volume_l",
          "purge_time_min",
          "flow_rate_l_min",
          "wave_hgt_m",
          "sample_grade",
          "sample_approval",
          "commissioning_org",
          "sampling_org",
          "sample_note",
          "owner",
          "contributor",
          "sample_no_source_update",
          "field_visit_id"
        )
        result_columns <- c(
          "parameter_id",
          "result_type",
          "protocol_method",
          "matrix_state_id",
          "sample_fraction_id",
          "result_value_type",
          "result_speciation_id",
          "result",
          "result_condition",
          "result_condition_value",
          "conversion",
          "result_offset",
          "laboratory",
          "result_no_source_update",
          "grade_type_id",
          "approval_type_id"
        )
        source_group <- interaction(
          df$source_row_number,
          drop = TRUE,
          lex.order = TRUE
        )
        run_rows <- data.frame(
          sheet_name = rep(
            addDiscData_first(profile$sheet_name, NA_character_),
            nrow(df)
          ),
          source_row_number = as.integer(df$source_row_number),
          result_index = as.integer(ave(
            seq_len(nrow(df)),
            source_group,
            FUN = seq_along
          )),
          validation_status = rep("committed", nrow(df)),
          sample_id = as.integer(unlist(sample_lookup[df$sample_key])),
          result_id = result_ids,
          stringsAsFactors = FALSE
        )
        run_rows$source_record <- lapply(
          seq_len(nrow(df)),
          function(i) as.list(df[i, source_columns, drop = FALSE])
        )
        run_rows$normalized_sample <- lapply(
          seq_len(nrow(df)),
          function(i) as.list(df[i, sample_columns, drop = FALSE])
        )
        run_rows$normalized_result <- lapply(
          seq_len(nrow(df)),
          function(i) as.list(df[i, result_columns, drop = FALSE])
        )
        run_rows$validation_messages <- replicate(
          nrow(df),
          list(),
          simplify = FALSE
        )
        AquaCache::appendImportRunRows(con, import_run_id, run_rows)
        AquaCache::completeImportRun(
          con = con,
          import_run_id = import_run_id,
          status = "committed",
          summary = list(
            samples = inserted_samples,
            results = inserted_results
          ),
          validation_summary = list(committed = inserted_results, errors = 0L)
        )
      }

      location_labels <- addDiscData_location_labels(request$locations)
      location_index <- match(
        samples$location_id,
        request$locations$location_id
      )
      destination <- location_labels[location_index]
      destination[is.na(samples$location_id)] <-
        "No location (locationless sample)"
      sub_location_index <- match(
        samples$sub_location_id,
        request$sub_locations$sub_location_id
      )
      sub_location <- request$sub_locations$sub_location_name[
        sub_location_index
      ]
      sample_ids <- as.integer(vapply(
        samples$sample_key,
        function(key) sample_lookup[[key]],
        numeric(1)
      ))
      summary <- data.frame(
        sample_id = sample_ids,
        source_code = as.character(samples$source_code),
        source_sample_id = as.character(samples$source_sample_id),
        sample_datetime = format(
          as.POSIXct(samples$datetime, tz = "UTC"),
          "%Y-%m-%d %H:%M:%S UTC",
          tz = "UTC"
        ),
        AquaCache_location = as.character(destination),
        AquaCache_sub_location = as.character(sub_location),
        stringsAsFactors = FALSE
      )
      summary$AquaCache_sub_location[is.na(
        summary$AquaCache_sub_location
      )] <- ""
      DBI::dbExecute(con, "COMMIT")
      active <- FALSE
      list(
        ok = TRUE,
        inserted_samples = inserted_samples,
        inserted_results = inserted_results,
        summary = summary
      )
    },
    error = function(e) {
      if (isTRUE(active)) {
        try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
      }
      list(ok = FALSE, message = conditionMessage(e))
    }
  )
}

addDiscData_target_unit <- function(
  parameters,
  parameter_id,
  matrix_state_id,
  matrix_states
) {
  parameter_id <- suppressWarnings(as.integer(parameter_id))
  matrix_state_id <- suppressWarnings(as.integer(matrix_state_id))
  out <- rep(NA_character_, max(length(parameter_id), length(matrix_state_id)))
  parameter_id <- rep_len(parameter_id, length(out))
  matrix_state_id <- rep_len(matrix_state_id, length(out))
  for (i in seq_along(out)) {
    row <- match(parameter_id[[i]], parameters$parameter_id)
    state_row <- match(matrix_state_id[[i]], matrix_states$matrix_state_id)
    unit_column <- if (!is.na(state_row)) {
      state_code <- matrix_states$matrix_state_code[[state_row]]
      paste0("unit_", if (state_code == "not_applicable") "na" else state_code)
    } else {
      NA_character_
    }
    if (
      !is.na(row) && length(unit_column) && unit_column %in% names(parameters)
    ) {
      out[[i]] <- as.character(parameters[[unit_column]][[row]])
    }
  }
  out[!addDiscData_present(out)] <- NA_character_
  out
}

addDiscData_parameter_choices <- function(parameters) {
  labels <- vapply(
    seq_len(nrow(parameters)),
    function(i) {
      unit_labels <- character()
      for (spec in list(
        c("Liquid", "unit_liquid"),
        c("Solid", "unit_solid"),
        c("Gas", "unit_gas"),
        c("Not applicable", "unit_na")
      )) {
        if (spec[[2]] %in% names(parameters)) {
          unit <- parameters[[spec[[2]]]][[i]]
          if (addDiscData_present(unit)) {
            unit_labels <- c(unit_labels, paste0(spec[[1]], ": ", unit))
          }
        }
      }
      if (!length(unit_labels)) {
        return(as.character(parameters$param_name[[i]]))
      }
      paste0(
        parameters$param_name[[i]],
        " [",
        paste(unit_labels, collapse = "; "),
        "]"
      )
    },
    character(1)
  )

  c(
    "Select AquaCache parameter" = "",
    stats::setNames(as.character(parameters$parameter_id), labels)
  )
}

addDiscData_lookup_label <- function(ids, lookup, id_column, label_column) {
  if (!nrow(lookup)) {
    return(rep(NA_character_, length(ids)))
  }
  as.character(lookup[[label_column]][match(ids, lookup[[id_column]])])
}

addDiscData_result_display <- function(
  rows,
  locations,
  sub_locations,
  parameters,
  result_types,
  result_conditions,
  sample_fractions,
  result_value_types,
  result_speciations,
  matrix_states,
  laboratories,
  media,
  collection_methods,
  sample_types,
  protocols_methods,
  grade_types,
  approval_types
) {
  if (!nrow(rows)) {
    return(data.frame())
  }
  source_result <- if ("source_result_text" %in% names(rows)) {
    addDiscData_clean_location_text(rows$source_result_text)
  } else {
    rep("", nrow(rows))
  }
  missing_source_result <- !addDiscData_present(source_result)
  source_result[
    missing_source_result & !is.na(rows$source_result)
  ] <- as.character(
    rows$source_result[missing_source_result & !is.na(rows$source_result)]
  )
  condition_prefix <- c("1" = "<", "2" = ">")
  rebuild <- missing_source_result & !is.na(rows$source_result_condition_value)
  if (any(rebuild)) {
    prefix <- unname(condition_prefix[as.character(rows$result_condition[
      rebuild
    ])])
    prefix[is.na(prefix)] <- ""
    source_result[rebuild] <- paste0(
      prefix,
      rows$source_result_condition_value[rebuild]
    )
  }

  location_index <- match(rows$location_id, locations$location_id)
  parameter_name <- addDiscData_lookup_label(
    rows$parameter_id,
    parameters,
    "parameter_id",
    "param_name"
  )
  parameter_name[
    !addDiscData_present(parameter_name)
  ] <- rows$source_parameter_name[
    !addDiscData_present(parameter_name)
  ]
  out <- data.frame(
    `Source sample` = rows$source_sample_id,
    `Source location` = rows$source_location_name,
    `Source parameter` = rows$source_parameter_code,
    `Source unit` = rows$source_unit,
    `Source result` = source_result,
    `Source result flag` = rows$source_result_flag,
    `AquaCache location` = addDiscData_location_labels(locations)[
      location_index
    ],
    `Sub-location` = addDiscData_lookup_label(
      rows$sub_location_id,
      sub_locations,
      "sub_location_id",
      "sub_location_name"
    ),
    `Sample datetime (UTC)` = format(
      rows$datetime,
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    Parameter = parameter_name,
    `Result value` = rows$result,
    `Result condition` = addDiscData_lookup_label(
      rows$result_condition,
      result_conditions,
      "result_condition_id",
      "result_condition"
    ),
    `Condition value` = rows$result_condition_value,
    `Target unit` = addDiscData_target_unit(
      parameters,
      rows$parameter_id,
      rows$matrix_state_id,
      matrix_states
    ),
    `Sample fraction` = addDiscData_lookup_label(
      rows$sample_fraction_id,
      sample_fractions,
      "sample_fraction_id",
      "sample_fraction"
    ),
    `Result type` = addDiscData_lookup_label(
      rows$result_type,
      result_types,
      "result_type_id",
      "result_type"
    ),
    `Value type` = addDiscData_lookup_label(
      rows$result_value_type,
      result_value_types,
      "result_value_type_id",
      "result_value_type"
    ),
    Speciation = addDiscData_lookup_label(
      rows$result_speciation_id,
      result_speciations,
      "result_speciation_id",
      "result_speciation"
    ),
    Matrix = addDiscData_lookup_label(
      rows$matrix_state_id,
      matrix_states,
      "matrix_state_id",
      "matrix_state_name"
    ),
    Laboratory = addDiscData_lookup_label(
      rows$laboratory,
      laboratories,
      "lab_id",
      "lab_name"
    ),
    Protocol = addDiscData_lookup_label(
      rows$protocol_method,
      protocols_methods,
      "protocol_id",
      "protocol_name"
    ),
    Grade = addDiscData_lookup_label(
      rows$grade_type_id,
      grade_types,
      "grade_type_id",
      "grade_type_description"
    ),
    Approval = addDiscData_lookup_label(
      rows$approval_type_id,
      approval_types,
      "approval_type_id",
      "approval_type_description"
    ),
    Media = addDiscData_lookup_label(
      rows$media_id,
      media,
      "media_id",
      "media_type"
    ),
    `Collection method` = addDiscData_lookup_label(
      rows$collection_method,
      collection_methods,
      "collection_method_id",
      "collection_method"
    ),
    `Sample type` = addDiscData_lookup_label(
      rows$sample_type,
      sample_types,
      "sample_type_id",
      "sample_type"
    ),
    `Analysis datetime` = format(
      rows$analysis_datetime,
      "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ),
    `Lab report` = rows$lab_report_no,
    `Lab sample` = rows$lab_sample_no,
    Note = rows$note,
    `Import action` = rows$result_flag_action,
    `Mapping status` = rows$mapping_status,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  out[is.na(out)] <- ""
  out
}

addDiscData_result_edit_columns <- function() {
  c(
    "Result value" = "result",
    "Result condition" = "result_condition",
    "Condition value" = "result_condition_value",
    "Result type" = "result_type",
    "Sample fraction" = "sample_fraction_id",
    "Value type" = "result_value_type",
    "Speciation" = "result_speciation_id",
    "Matrix" = "matrix_state_id",
    "Protocol" = "protocol_method",
    "Laboratory" = "laboratory",
    "Analysis datetime" = "analysis_datetime",
    "Lab report" = "lab_report_no",
    "Lab sample" = "lab_sample_no",
    "Grade" = "grade_type_id",
    "Approval" = "approval_type_id",
    "Note" = "note"
  )
}

addDiscData_style_origin_columns <- function(
  table,
  source_columns,
  target_columns
) {
  column_defs <- table$x$options$columnDefs
  if (is.null(column_defs)) {
    column_defs <- list()
  }
  table_columns <- names(table$x$data)
  source_targets <- match(source_columns, table_columns) - 1L
  source_targets <- source_targets[!is.na(source_targets)]
  target_targets <- match(target_columns, table_columns) - 1L
  target_targets <- target_targets[!is.na(target_targets)]
  neutral_targets <- setdiff(
    seq_along(table_columns) - 1L,
    c(source_targets, target_targets)
  )

  if (length(source_targets)) {
    column_defs[[length(column_defs) + 1L]] <- list(
      className = "addDiscData-source-column",
      targets = as.integer(source_targets)
    )
  }
  if (length(target_targets)) {
    column_defs[[length(column_defs) + 1L]] <- list(
      className = "addDiscData-target-column",
      targets = as.integer(target_targets)
    )
  }
  if (length(neutral_targets)) {
    column_defs[[length(column_defs) + 1L]] <- list(
      className = "addDiscData-neutral-column",
      targets = as.integer(neutral_targets)
    )
  }
  table$x$options$columnDefs <- column_defs
  table
}

addDiscDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    page_fluid(
      tags$head(tags$style(HTML(
        "html body table.dataTable.dataTable.dataTable tbody tr:not(.selected) > td.addDiscData-source-column,
        table.dataTable thead tr > th.addDiscData-source-column {
          --bs-table-bg: #e8f1fb !important;
          --bs-table-bg-type: #e8f1fb !important;
          --bs-table-bg-state: #e8f1fb !important;
          --bs-table-accent-bg: #e8f1fb !important;
          background-color: #e8f1fb !important;
          box-shadow: inset 0 0 0 9999px #e8f1fb !important;
          color: #212529 !important;
        }
        html body table.dataTable.dataTable.dataTable tbody tr:not(.selected) > td.addDiscData-target-column,
        table.dataTable thead tr > th.addDiscData-target-column {
          --bs-table-bg: #e9f5ec !important;
          --bs-table-bg-type: #e9f5ec !important;
          --bs-table-bg-state: #e9f5ec !important;
          --bs-table-accent-bg: #e9f5ec !important;
          background-color: #e9f5ec !important;
          box-shadow: inset 0 0 0 9999px #e9f5ec !important;
          color: #212529 !important;
        }
        html body table.dataTable.dataTable tbody tr.selected > td.addDiscData-source-column {
          --bs-table-bg: #c6dbee !important;
          --bs-table-bg-type: #c6dbee !important;
          --bs-table-bg-state: #c6dbee !important;
          --bs-table-accent-bg: #c6dbee !important;
          background-color: #c6dbee !important;
          box-shadow: inset 0 0 0 9999px #c6dbee !important;
          color: #212529 !important;
        }
        html body table.dataTable.dataTable tbody tr.selected > td.addDiscData-target-column {
          --bs-table-bg: #cce3d1 !important;
          --bs-table-bg-type: #cce3d1 !important;
          --bs-table-bg-state: #cce3d1 !important;
          --bs-table-accent-bg: #cce3d1 !important;
          background-color: #cce3d1 !important;
          box-shadow: inset 0 0 0 9999px #cce3d1 !important;
          color: #212529 !important;
        }
        html body table.dataTable.dataTable tbody tr.selected > td.addDiscData-neutral-column {
          --bs-table-bg: #e2e5e8 !important;
          --bs-table-bg-type: #e2e5e8 !important;
          --bs-table-bg-state: #e2e5e8 !important;
          --bs-table-accent-bg: #e2e5e8 !important;
          background-color: #e2e5e8 !important;
          box-shadow: inset 0 0 0 9999px #e2e5e8 !important;
          color: #212529 !important;
        }"
      ))),
      uiOutput(ns("banner")),
      tabsetPanel(
        id = ns("workflow_tabs"),
        tabPanel(
          "Add samples/results",
          value = "upload_samples",
          bslib::accordion(
            id = ns("accordion1"),
            open = "data_panel",
            bslib::accordion_panel(
              id = ns("data_panel"),
              value = "data_panel",
              title = "Add samples and results",
              radioButtons(
                ns("entry_mode"),
                "Input method",
                choices = c(File = "file", Manual = "manual"),
                inline = TRUE
              ),
              conditionalPanel(
                condition = "input.entry_mode == 'file'",
                ns = ns,
                fileInput(
                  ns("file"),
                  "Upload .csv or Excel",
                  accept = c(".csv", ".xls", ".xlsx")
                ),
                helpText(
                  "The file selected here is attached to every sample created by this upload. If its exact file hash is already in AquaCache, the existing document is reused."
                ),
                fluidRow(
                  column(
                    9,
                    selectizeInput(
                      ns("import_profile"),
                      "Workbook format",
                      choices = NULL
                    )
                  ),
                  column(
                    3,
                    tags$div(
                      style = "padding-top: 25px;",
                      actionButton(
                        ns("open_import_setup"),
                        "Manage formats and mappings"
                      )
                    )
                  )
                ),
                uiOutput(ns("import_profile_status")),
                bslib::input_task_button(
                  ns("preview_file"),
                  "Parse file"
                ),
                br(),
                conditionalPanel(
                  condition = "input.entry_mode == 'file' && input.preview_file && input.preview_file.value > 0",
                  ns = ns,
                  tags$h5("Map source parameter names and units to AquaCache"),
                  helpText(
                    "After previewing, select any item that needs a mapping, choose what it means in AquaCache, then save it. These preview edits are staged for this workbook format and published when the upload succeeds. Use Manage import formats and mappings to create or edit mappings outside an upload and publish them immediately."
                  ),
                  checkboxInput(
                    ns("show_all_mappings"),
                    "Show already mapped parameters",
                    value = FALSE
                  ),
                  DT::DTOutput(ns("mapping_summary")),
                  uiOutput(ns("mapping_editor")),
                  bslib::input_task_button(
                    ns("save_parameter_mappings"),
                    "Save selected parameter mapping",
                    label_busy = "Saving parameter mapping..."
                  ),
                  tags$hr(),
                  tags$h5("Map source locations to AquaCache locations"),
                  helpText(
                    "Choose a source row, select its AquaCache location, then save the mapping. Field and trip blanks are assigned their QC sample type automatically and are kept locationless; assign them to a trip or QC group in Sample details before uploading."
                  ),
                  DT::DTOutput(ns("location_mapping_summary")),
                  uiOutput(ns("location_mapping_editor")),
                  bslib::input_task_button(
                    ns("save_location_mapping"),
                    "Save selected location mapping",
                    label_busy = "Saving location mapping..."
                  ),
                  actionButton(
                    ns("open_create_locations"),
                    "Create missing locations"
                  ),
                  tags$hr(),
                  tags$h5("Map result flags"),
                  helpText(
                    "Choose a source flag and tell AquaCache how to interpret it. Leave the condition and threshold empty when the flag is only descriptive."
                  ),
                  DT::DTOutput(ns("result_flag_mapping_summary")),
                  uiOutput(ns("result_flag_mapping_editor")),
                  actionButton(
                    ns("save_result_flag_mapping"),
                    "Save selected result-flag mapping"
                  )
                )
              ),
            ),
          ),
          bslib::accordion(
            id = ns("sample_detail_accordion"),
            open = FALSE,
            bslib::accordion_panel(
              id = ns("sample_detail_panel"),
              title = "Create or edit sample details",
              fluidRow(
                column(
                  9,
                  selectizeInput(
                    ns("field_visit_id"),
                    "Field visit for new samples (optional)",
                    choices = c("No field visit" = ""),
                    options = list(
                      placeholder = "Leave blank for samples without a visit",
                      maxItems = 1
                    )
                  )
                ),
                column(
                  3,
                  tags$div(
                    style = "padding-top: 25px;",
                    actionButton(ns("refresh_field_visits"), "Refresh visits")
                  )
                )
              ),
              helpText(
                "The selected visit will be linked to every sample created by this upload. To use more than one visit, upload each visit's samples separately."
              ),
              helpText(
                "Select a sample in the table below to edit it. Use New sample to add a sample alongside file samples or start another manual sample. Field and trip blanks are assigned a matching QC sample type and must be assigned to a trip or QC group, but they will not be assigned a location."
              ),
              actionButton(ns("new_sample"), "New sample"),
              uiOutput(ns("sample_editor"))
            )
          ),
          DT::DTOutput(ns("sample_location_summary")),
          tags$hr(),
          conditionalPanel(
            condition = paste(
              "input.entry_mode == 'manual' ||",
              "(input.entry_mode == 'file' && input.preview_file && input.preview_file.value > 0)"
            ),
            ns = ns,
            bslib::accordion(
              id = ns("manual_result_accordion"),
              open = FALSE,
              bslib::accordion_panel(
                id = ns("manual_result_panel"),
                title = "Add a result to the selected sample",
                uiOutput(ns("manual_result_sample_label")),
                conditionalPanel(
                  condition = "input.entry_mode == 'file'",
                  ns = ns,
                  helpText(
                    "Use this form for additional results, such as field measurements. Set Result type to field when appropriate. Results remain in this preview until you upload the reviewed data."
                  )
                ),

                # WIP
                fluidRow(
                  column(
                    3,
                    selectizeInput(
                      ns("manual_parameter"),
                      "Parameter",
                      choices = NULL,
                      multiple = TRUE,
                      options = list(
                        maxItems = 1,
                        placeholder = "Select a parameter"
                      ),
                      width = "100%"
                    )
                  ),
                  column(
                    3,
                    selectizeInput(
                      ns("manual_sample_fraction"),
                      "Sample fraction",
                      choices = NULL,
                      multiple = TRUE,
                      options = list(
                        maxItems = 1,
                        placeholder = "Select a parameter first"
                      ),
                      width = "100%"
                    )
                  ),
                  column(
                    2,
                    selectizeInput(
                      ns("manual_speciation"),
                      "Speciation",
                      choices = NULL,
                      multiple = TRUE,
                      options = list(
                        maxItems = 1,
                        placeholder = "Select a parameter first"
                      ),
                      width = "100%"
                    )
                  ),
                  column(
                    2,
                    selectizeInput(
                      ns("manual_matrix_state"),
                      "Matrix state",
                      choices = NULL,
                      multiple = TRUE,
                      options = list(
                        maxItems = 1,
                        placeholder = "Mandatory"
                      ),
                      width = "100%"
                    )
                  ),
                  column(
                    2,
                    selectizeInput(
                      ns("manual_result_type"),
                      "Result type",
                      choices = NULL,
                      multiple = TRUE,
                      options = list(
                        maxItems = 1,
                        placeholder = "Mandatory"
                      ),
                      width = "100%"
                    )
                  )
                ),
                # WIP
                tags$hr(),
                fluidRow(
                  column(
                    3,
                    numericInput(
                      ns("manual_result"),
                      "Result value - leave blank if result condition applies",
                      value = NA_real_,
                      width = "100%"
                    )
                  ),
                  column(
                    3,
                    selectizeInput(
                      ns("manual_result_value_type"),
                      "Result value type",
                      choices = NULL,
                      multiple = TRUE,
                      options = list(
                        maxItems = 1,
                        placeholder = "Mandatory"
                      ),
                      width = "100%"
                    )
                  ),
                  column(
                    3,
                    selectizeInput(
                      ns("manual_result_condition"),
                      "Result condition",
                      choices = NULL,
                      multiple = TRUE,
                      options = list(
                        maxItems = 1,
                        placeholder = "Only use if result value is blank"
                      ),
                      width = "100%"
                    )
                  ),
                  column(
                    3,
                    numericInput(
                      ns("manual_condition_value"),
                      "Result condition value",
                      value = NA_real_,
                      width = "100%"
                    )
                  )
                ),
                tags$hr(),
                fluidRow(
                  column(
                    3,
                    selectizeInput(
                      ns("manual_protocol"),
                      "Protocol/method",
                      choices = NULL,
                      multiple = TRUE,
                      options = list(
                        maxItems = 1,
                        placeholder = "Enter if applicable"
                      ),
                      width = "100%"
                    )
                  ),
                  column(9, uiOutput(ns("manual_lab_metadata")))
                ),
                shinyWidgets::airDatepickerInput(
                  ns("manual_analysis_datetime"),
                  "Analysis datetime (optional)",
                  value = NULL,
                  timepicker = TRUE,
                  update_on = "change",
                  tz = "UTC",
                  timepickerOpts = shinyWidgets::timepickerOptions(
                    minutesStep = 15,
                    timeFormat = "HH:mm"
                  )
                ),
                tags$hr(),
                fluidRow(
                  column(
                    2,
                    selectizeInput(
                      ns("manual_grade"),
                      "Grade",
                      choices = NULL,
                      width = "100%"
                    )
                  ),
                  column(
                    2,
                    selectizeInput(
                      ns("manual_approval"),
                      "Approval",
                      choices = NULL,
                      width = "100%"
                    )
                  ),
                  column(
                    8,
                    textAreaInput(
                      ns("manual_note"),
                      "Result note",
                      placeholder = "Optional result-specific note",
                      width = "100%"
                    )
                  )
                ),
                actionButton(
                  ns("add_manual_result"),
                  "Add result to selected sample"
                )
              )
            )
          ),
          tags$h5("Results for selected sample"),
          DT::DTOutput(ns("data_table")),
          helpText(
            "Any additional documents selected below will also be attached to every sample created by this upload."
          ),
          fileInput(
            ns("attach_docs"),
            "Additional documents to attach (optional)",
            multiple = TRUE
          ),
          bslib::input_task_button(
            ns("upload"),
            "Upload to AquaCache",
            label_busy = "Uploading to AquaCache..."
          )
        ),
        tabPanel(
          "Create missing locations",
          value = "create_locations",
          helpText(
            "Create one AquaCache location for each unmapped source location in the current file preview. The English name, location type, latitude, and longitude are required. Alias and note are optional. Select any number of networks and projects; leave either field empty for none. Location codes are generated automatically; elevations are fetched from web services. If no service returns a usable elevation, it is saved as 0 m using the assumed datum and called out in the summary. New locations are visible to all users by default; remove All users before selecting access groups to restrict visibility. Creating locations also saves source mappings for this workbook format."
          ),
          fluidRow(
            column(
              8,
              selectizeInput(
                ns("new_location_map_target"),
                "Location row to update from the map",
                choices = NULL,
                options = list(
                  placeholder = "Choose an unmapped source location"
                )
              )
            ),
            column(
              4,
              tags$div(
                style = "padding-top: 25px;",
                actionButton(
                  ns("open_new_location_map"),
                  "Choose coordinates on map"
                )
              )
            )
          ),
          uiOutput(ns("new_location_rows")),
          bslib::input_task_button(
            ns("create_new_locations"),
            "Create locations and map source names",
            label_busy = "Creating new locations..."
          )
        ),
        tabPanel(
          "Manage import formats and mappings",
          value = "manage_import_setup",
          helpText(
            "Create or edit workbook formats and manage parameter, location, and result-flag mappings here. You can use this page without selecting a file. Saved mappings are published immediately and applied to future uploads. Profile-specific mappings take precedence over source-wide mappings. If a matching file is already previewed, its preview is refreshed while retaining your edits."
          ),
          fluidRow(
            column(
              6,
              selectizeInput(
                ns("manage_import_profile"),
                "Workbook format",
                choices = NULL,
                options = list(placeholder = "Select a workbook format")
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
          conditionalPanel(
            condition = "input.manage_mapping_type == 'parameter'",
            ns = ns,
            wellPanel(
              tags$h5("Parameter mapping"),
              fluidRow(
                column(
                  4,
                  textInput(
                    ns("manage_parameter_code"),
                    "Source parameter code"
                  )
                ),
                column(
                  4,
                  textInput(
                    ns("manage_parameter_unit"),
                    "Source unit (optional)"
                  )
                ),
                column(
                  4,
                  selectInput(
                    ns("manage_mapping_scope"),
                    "Applies to",
                    choices = c(
                      "This workbook format" = "profile",
                      "All formats for this source" = "source"
                    )
                  )
                )
              ),
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
                    ns("manage_speciation"),
                    "Result speciation",
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
              helpText(
                "A sample fraction or speciation is required when the selected AquaCache parameter requires it."
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
                "Save parameter mapping"
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
                      "All formats for this source" = "source"
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
                "Save location mapping"
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
                      "All formats for this source" = "source"
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
                "Save result-flag mapping"
              )
            )
          )
        )
      )
    )
  )
}

addDiscData <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "addDiscData"
      )
    })

    outputs <- reactiveValues()
    data <- reactiveValues(
      df = addDiscData_empty_table(),
      preview_base = addDiscData_empty_table(),
      raw_file_path = NULL,
      preview_profile_key = NULL,
      preview_is_stale = FALSE
    )
    sample_qualifier_map <- reactiveVal(list())
    sample_observer_map <- reactiveVal(list())
    sample_share_map <- reactiveVal(list())
    current_manual_sample <- reactiveVal(0L)
    manual_upload_id <- paste0(
      format(Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC"),
      "-",
      Sys.getpid()
    )
    import_profiles <- reactiveVal(addDiscData_empty_profiles())
    profile_load_error <- reactiveVal(NULL)
    new_profile_template <- reactiveVal(NULL)
    new_profile_mode <- reactiveVal("create")
    mapping_revision <- reactiveVal(0L)
    manage_mapping_selected <- reactiveVal(NULL)
    manage_mapping_error <- reactiveVal(NULL)

    con <- session$userData$AquaCache
    read_observers <- function() {
      tryCatch(
        DBI::dbGetQuery(
          con,
          "SELECT observer_id, observer_first, observer_last, organization
             FROM instruments.observers
            ORDER BY observer_first, observer_last, organization"
        ),
        error = function(e) {
          data.frame(
            observer_id = integer(),
            observer_first = character(),
            observer_last = character(),
            organization = character()
          )
        }
      )
    }
    observers <- reactiveVal(read_observers())
    observer_choices <- function(rows = observers()) {
      stats::setNames(
        as.character(rows$observer_id),
        addDiscData_observer_labels(rows)
      )
    }
    read_field_visits <- function() {
      tryCatch(
        DBI::dbGetQuery(
          con,
          "SELECT v.field_visit_id,
                  v.location_id,
                  v.sub_location_id,
                  v.start_datetime,
                  l.name AS location_name,
                  sl.sub_location_name,
                  v.purpose
             FROM field.field_visits AS v
             JOIN public.locations AS l USING (location_id)
             LEFT JOIN public.sub_locations AS sl USING (sub_location_id)
            ORDER BY v.start_datetime DESC, v.field_visit_id DESC"
        ),
        error = function(e) {
          data.frame(
            field_visit_id = integer(),
            location_id = integer(),
            sub_location_id = integer(),
            start_datetime = as.POSIXct(character(), tz = "UTC"),
            location_name = character(),
            sub_location_name = character(),
            purpose = character()
          )
        }
      )
    }
    field_visits <- reactiveVal(read_field_visits())
    update_field_visit_choices <- function() {
      visits <- field_visits()
      labels <- if (nrow(visits)) {
        visit_time <- format(
          as.POSIXct(visits$start_datetime, tz = "UTC"),
          "%Y-%m-%d %H:%M UTC",
          tz = "UTC"
        )
        paste(
          visit_time,
          visits$location_name,
          ifelse(
            addDiscData_present(visits$sub_location_name),
            visits$sub_location_name,
            ""
          ),
          ifelse(addDiscData_present(visits$purpose), visits$purpose, ""),
          sep = " · "
        )
      } else {
        character()
      }
      updateSelectizeInput(
        session,
        "field_visit_id",
        choices = c(
          "No field visit" = "",
          stats::setNames(as.character(visits$field_visit_id), labels)
        )
      )
    }
    update_field_visit_choices()
    observeEvent(
      input$refresh_field_visits,
      {
        field_visits(read_field_visits())
        update_field_visit_choices()
        showNotification("Field visit choices refreshed.", type = "message")
      },
      ignoreInit = TRUE
    )
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
    check_results <- DBI::dbGetQuery(
      con,
      "SELECT has_table_privilege(current_user, 'discrete.results', 'INSERT') AS can_insert"
    )
    check_samples <- DBI::dbGetQuery(
      con,
      "SELECT has_table_privilege(current_user, 'discrete.samples', 'INSERT') AS can_insert"
    )
    check_groups <- DBI::dbGetQuery(
      con,
      "SELECT
         has_table_privilege(
           current_user,
           'discrete.sample_groups',
           'INSERT'
         ) AS can_create_group,
         has_table_privilege(
           current_user,
           'discrete.sample_group_members',
           'INSERT'
         ) AS can_assign_group"
    )
    observer_permissions <- tryCatch(
      DBI::dbGetQuery(
        con,
        "SELECT
           has_table_privilege(
             current_user,
             'instruments.observers',
             'INSERT'
           ) AS can_create_observer,
           has_table_privilege(
             current_user,
             'discrete.sample_observers',
             'INSERT'
           ) AS can_assign_observers"
      ),
      error = function(e) {
        data.frame(
          can_create_observer = FALSE,
          can_assign_observers = FALSE
        )
      }
    )
    if (!check_results$can_insert || !check_samples$can_insert) {
      showModal(modalDialog(
        title = "Insufficient Privileges",
        "You do not have write privileges to add samples or results to the database. Please contact your database administrator.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      shinyjs::disable("upload")
    }

    params <- reactive({
      dbGetQueryDT(
        con,
        "SELECT p.parameter_id,
                p.param_name,
                p.sample_fraction,
                p.result_speciation,
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
    field_result_type_ids <- result_types$result_type_id[
      tolower(trimws(result_types$result_type)) == "field"
    ]
    field_result_type_id <- if (length(field_result_type_ids) == 1L) {
      as.integer(field_result_type_ids[[1]])
    } else {
      NA_integer_
    }
    is_field_result_type <- function(result_type_id) {
      result_type_id <- addDiscData_int(result_type_id)
      !is.na(field_result_type_id) &&
        !is.na(result_type_id) &&
        result_type_id == field_result_type_id
    }
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
    result_speciations <- DBI::dbGetQuery(
      con,
      "SELECT result_speciation_id, result_speciation
         FROM discrete.result_speciations
        ORDER BY result_speciation"
    )
    sample_qualifiers <- DBI::dbGetQuery(
      con,
      "SELECT qualifier_type_id, qualifier_type_description
         FROM public.qualifier_types
        ORDER BY qualifier_type_description"
    )
    protocols_methods <- DBI::dbGetQuery(
      con,
      "SELECT protocol_id, protocol_name
         FROM discrete.protocols_methods
        ORDER BY protocol_name"
    )
    grade_types <- DBI::dbGetQuery(
      con,
      "SELECT grade_type_id, grade_type_description
         FROM public.grade_types
        ORDER BY grade_type_description"
    )
    approval_types <- DBI::dbGetQuery(
      con,
      "SELECT approval_type_id, approval_type_description
         FROM public.approval_types
        ORDER BY approval_type_description"
    )
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
    location_networks <- DBI::dbGetQuery(
      con,
      "SELECT network_id, name FROM public.networks ORDER BY name"
    )
    location_projects <- DBI::dbGetQuery(
      con,
      "SELECT project_id, name FROM public.projects ORDER BY name"
    )
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
    location_types <- DBI::dbGetQuery(
      con,
      "SELECT type_id, type
         FROM public.location_types
        ORDER BY type"
    )
    location_share_choices <- addDiscData_share_choices(
      con,
      "public.locations"
    )
    sample_group_share_choices <- addDiscData_share_choices(
      con,
      "discrete.sample_groups"
    )
    sample_share_choices <- addDiscData_share_choices(
      con,
      "discrete.samples"
    )
    observe_share_selection <- function(input_id) {
      observeEvent(
        input[[input_id]],
        {
          selected <- as.character(input[[input_id]])
          if (length(selected) > 1L && "public_reader" %in% selected) {
            updateSelectizeInput(
              session,
              input_id,
              selected = "public_reader"
            )
          }
        },
        ignoreInit = TRUE
      )
    }
    observe_share_selection("new_sample_group_share_with")
    observe_share_selection("edit_sample_share_with")
    quick_elevation_datum <- DBI::dbGetQuery(
      con,
      "SELECT datum_id
         FROM public.datum_list
        WHERE datum_name_en = 'CGVD2013:2010'
        ORDER BY datum_id
        LIMIT 1"
    )$datum_id
    read_sample_groups <- function() {
      DBI::dbGetQuery(
        con,
        "SELECT sample_group_id, group_type, group_code, group_name
         FROM discrete.sample_groups
         WHERE active
         ORDER BY start_datetime DESC NULLS LAST, sample_group_id DESC"
      )
    }
    initial_sample_groups <- read_sample_groups()
    sample_groups <- reactiveVal(initial_sample_groups)
    sample_group_types <- DBI::dbGetQuery(
      con,
      "SELECT group_type, group_type_name
       FROM discrete.sample_group_types
       WHERE active
       ORDER BY sort_order"
    )
    pending_sample_group <- reactiveVal(NULL)

    pending_location_selection <- reactiveVal(character(0))
    pending_location_new <- reactiveVal(NULL)
    pending_sublocation_selection <- reactiveVal(character(0))
    pending_sublocation_new <- reactiveVal(NULL)

    update_location_selectize <- function(selected = NULL) {
      args <- list(
        session = session,
        inputId = "edit_sample_location",
        choices = addDiscData_location_choices(locations())
      )
      if (!is.null(selected)) {
        args$selected <- normalize_selectize_values(selected)
      }
      do.call(updateSelectizeInput, args)
    }

    update_sublocation_selectize <- function(selected = NULL) {
      args <- list(
        session = session,
        inputId = "edit_sample_sublocation",
        choices = stats::setNames(
          sub_locations$sub_location_id,
          sub_locations$sub_location_name
        )
      )
      if (!is.null(selected)) {
        args$selected <- normalize_selectize_values(selected)
      }
      do.call(updateSelectizeInput, args)
    }

    update_location_selectize()
    update_sublocation_selectize()
    group_rows <- initial_sample_groups
    updateSelectizeInput(
      session,
      "edit_sample_group",
      choices = stats::setNames(
        as.character(group_rows$sample_group_id),
        addDiscData_sample_group_labels(group_rows)
      )
    )
    updateSelectizeInput(
      session,
      "manual_result_condition",
      choices = stats::setNames(
        result_conditions$result_condition_id,
        result_conditions$result_condition
      )
    )
    updateSelectizeInput(
      session,
      "manual_result_type",
      choices = stats::setNames(
        result_types$result_type_id,
        result_types$result_type
      ),
      selected = 2L
    )
    updateSelectizeInput(
      session,
      "manual_sample_fraction",
      choices = stats::setNames(
        sample_fractions$sample_fraction_id,
        sample_fractions$sample_fraction
      )
    )
    updateSelectizeInput(
      session,
      "manual_result_value_type",
      choices = stats::setNames(
        result_value_types$result_value_type_id,
        result_value_types$result_value_type
      ),
      selected = 1L
    )
    updateSelectizeInput(
      session,
      "manual_speciation",
      choices = stats::setNames(
        result_speciations$result_speciation_id,
        result_speciations$result_speciation
      )
    )
    parameter_requirement <- function(parameter_id, requirement) {
      parameter_id <- addDiscData_int(parameter_id)
      parameter_rows <- params()
      parameter_index <- match(parameter_id, parameter_rows$parameter_id)
      if (
        is.na(parameter_id) ||
          is.na(parameter_index) ||
          !requirement %in% names(parameter_rows)
      ) {
        return(FALSE)
      }
      isTRUE(as.logical(parameter_rows[[requirement]][[parameter_index]]))
    }
    validate_parameter_mapping_descriptors <- function(
      parameter_id,
      sample_fraction_id,
      result_speciation_id
    ) {
      parameter_id <- addDiscData_int(parameter_id)
      if (
        is.na(parameter_id) ||
          !parameter_id %in% params()$parameter_id
      ) {
        stop("Choose a valid AquaCache parameter.", call. = FALSE)
      }
      if (
        parameter_requirement(parameter_id, "sample_fraction") &&
          is.na(addDiscData_int(sample_fraction_id))
      ) {
        stop(
          "Sample fraction is required for the selected parameter.",
          call. = FALSE
        )
      }
      if (
        parameter_requirement(parameter_id, "result_speciation") &&
          is.na(addDiscData_int(result_speciation_id))
      ) {
        stop(
          "Speciation is required for the selected parameter.",
          call. = FALSE
        )
      }
      invisible(TRUE)
    }
    observeEvent(
      input$manual_parameter,
      {
        parameter_id <- addDiscData_int(input$manual_parameter)
        if (is.na(parameter_id)) {
          fraction_placeholder <- "Select a parameter first"
          speciation_placeholder <- "Select a parameter first"
        } else {
          fraction_placeholder <- if (
            parameter_requirement(
              parameter_id,
              "sample_fraction"
            )
          ) {
            "Required for selected parameter"
          } else {
            "Optional"
          }
          speciation_placeholder <- if (
            parameter_requirement(
              parameter_id,
              "result_speciation"
            )
          ) {
            "Required for selected parameter"
          } else {
            "Optional"
          }
        }
        updateSelectizeInput(
          session,
          "manual_sample_fraction",
          options = list(placeholder = fraction_placeholder)
        )
        updateSelectizeInput(
          session,
          "manual_speciation",
          options = list(placeholder = speciation_placeholder)
        )
      },
      ignoreInit = FALSE
    )
    updateSelectizeInput(
      session,
      "manual_matrix_state",
      choices = stats::setNames(
        matrix_states$matrix_state_id,
        matrix_states$matrix_state_name
      ),
      selected = 1L
    )
    updateSelectizeInput(
      session,
      "manual_protocol",
      choices = stats::setNames(
        protocols_methods$protocol_id,
        protocols_methods$protocol_name
      )
    )
    updateSelectizeInput(
      session,
      "manual_laboratory",
      choices = stats::setNames(
        laboratories$lab_id,
        laboratories$lab_name
      )
    )
    updateSelectizeInput(
      session,
      "manual_grade",
      choices = stats::setNames(
        grade_types$grade_type_id,
        grade_types$grade_type_description
      ),
      selected = grade_types[
        grade_types$grade_type_description == "Unspecified",
        "grade_type_id"
      ]
    )
    updateSelectizeInput(
      session,
      "manual_approval",
      choices = stats::setNames(
        approval_types$approval_type_id,
        approval_types$approval_type_description
      ),
      selected = approval_types[
        approval_types$approval_type_description == "Not reviewed",
        "approval_type_id"
      ]
    )

    observeEvent(
      input$edit_sample_location,
      {
        resolved <- resolve_selectize_lookup_values(
          input$edit_sample_location,
          locations()$location_id,
          locations()$name
        )
        pending_location_selection(resolved$existing_selection)
        if (!length(resolved$new_values)) {
          pending_location_new(NULL)
          if (resolved$used_label_match) {
            update_location_selectize(resolved$existing_selection)
          }
          return()
        }
        pending_location_new(resolved$last_new_value)
        showModal(modalDialog(
          sprintf("Add location '%s'?", pending_location_new()),
          footer = tagList(
            actionButton(ns("cancel_add_location_prompt"), "No"),
            actionButton(ns("goto_add_loc"), "Yes")
          ),
          easyClose = FALSE
        ))
      },
      ignoreInit = TRUE
    )

    observeEvent(input$cancel_add_location_prompt, {
      update_location_selectize(pending_location_selection())
      pending_location_new(NULL)
      removeModal()
    })

    observeEvent(input$goto_add_loc, {
      new_location <- pending_location_new()
      update_location_selectize(pending_location_selection())
      pending_location_new(NULL)
      removeModal()
      outputs$change_tab <- "addLocation"
      outputs$location <- new_location
    })

    observeEvent(
      input$edit_sample_sublocation,
      {
        resolved <- resolve_selectize_lookup_values(
          input$edit_sample_sublocation,
          sub_locations$sub_location_id,
          sub_locations$sub_location_name
        )
        pending_sublocation_selection(resolved$existing_selection)
        if (!length(resolved$new_values)) {
          pending_sublocation_new(NULL)
          if (resolved$used_label_match) {
            update_sublocation_selectize(resolved$existing_selection)
          }
          return()
        }
        pending_sublocation_new(resolved$last_new_value)
        showModal(modalDialog(
          sprintf("Add sub-location '%s'?", pending_sublocation_new()),
          footer = tagList(
            actionButton(ns("cancel_add_sublocation_prompt"), "No"),
            actionButton(ns("goto_add_subloc"), "Yes")
          ),
          easyClose = FALSE
        ))
      },
      ignoreInit = TRUE
    )

    observeEvent(input$cancel_add_sublocation_prompt, {
      update_sublocation_selectize(pending_sublocation_selection())
      pending_sublocation_new(NULL)
      removeModal()
    })

    observeEvent(input$goto_add_subloc, {
      new_sublocation <- pending_sublocation_new()
      update_sublocation_selectize(pending_sublocation_selection())
      pending_sublocation_new(NULL)
      removeModal()
      outputs$change_tab <- "addSubLocation"
      outputs$sub_location <- new_sublocation
    })

    reload_profiles <- function(selected = NULL) {
      profile_load_error(NULL)
      profiles <- tryCatch(
        addDiscData_read_profiles(con),
        error = function(e) {
          profile_load_error(e$message)
          addDiscData_empty_profiles()
        }
      )
      profiles$profile_key <- addDiscData_profile_key(
        profiles$source_code,
        profiles$profile_code
      )
      import_profiles(profiles)
      if (is.null(selected)) {
        current <- addDiscData_first(input$import_profile, "")
        selected <- if (current %in% profiles$profile_key) {
          current
        } else if (nrow(profiles)) {
          profiles$profile_key[[1]]
        } else {
          character()
        }
      }
      choices <- stats::setNames(
        profiles$profile_key,
        paste(profiles$source_code, profiles$profile_name, sep = " - ")
      )
      updateSelectizeInput(
        session,
        "import_profile",
        choices = choices,
        selected = selected
      )
      updateSelectizeInput(
        session,
        "manage_import_profile",
        choices = choices,
        selected = selected
      )
    }
    reload_profiles()

    output$import_profile_status <- renderUI({
      error <- profile_load_error()
      if (!is.null(error)) {
        return(tags$div(class = "alert alert-danger", error))
      }
      if (!can_manage_profiles) {
        return(tags$div(
          class = "text-muted",
          "Your database role can use workbook formats but cannot create or edit them. Ask an administrator to grant profile-management access."
        ))
      }
      if (!nrow(import_profiles())) {
        return(tags$div(
          class = "alert alert-warning",
          "No workbook formats are saved yet. Use Manage import formats and mappings to create one."
        ))
      }
      NULL
    })

    if (!can_manage_profiles) {
      shinyjs::disable("new_import_profile")
      shinyjs::disable("copy_import_profile")
      shinyjs::disable("edit_import_profile")
    }
    if (!can_manage_mappings) {
      shinyjs::disable("manage_new_mapping")
      shinyjs::disable("manage_save_parameter_mapping")
      shinyjs::disable("manage_save_location_mapping")
      shinyjs::disable("manage_save_flag_mapping")
    }
    observeEvent(
      input$import_profile,
      {
        if (!identical(input$manage_import_profile, input$import_profile)) {
          updateSelectizeInput(
            session,
            "manage_import_profile",
            selected = input$import_profile
          )
        }
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$manage_import_profile,
      {
        if (!identical(input$import_profile, input$manage_import_profile)) {
          updateSelectizeInput(
            session,
            "import_profile",
            selected = input$manage_import_profile
          )
        }
        clear_manager_mapping_selection()
      },
      ignoreInit = TRUE
    )
    observeEvent(input$open_import_setup, {
      updateTabsetPanel(session, "workflow_tabs", "manage_import_setup")
    })
    observe({
      updateSelectizeInput(
        session,
        "manual_parameter",
        choices = stats::setNames(params()$parameter_id, params()$param_name)
      )
    })

    selected_profile <- reactive({
      profiles <- import_profiles()
      req(nrow(profiles), input$import_profile)
      hit <- profiles[
        profiles$profile_key == input$import_profile,
        ,
        drop = FALSE
      ]
      validate(need(nrow(hit) == 1L, "Select an import profile."))
      hit
    })

    preview_parse_task <- ExtendedTask$new(function(request) {
      promises::future_promise(seed = TRUE, expr = {
        tryCatch(
          list(
            ok = TRUE,
            request = request,
            parsed = addDiscData_parse_upload(request$path, request$profile)
          ),
          error = function(e) {
            list(ok = FALSE, message = conditionMessage(e))
          }
        )
      })
    }) |>
      bslib::bind_task_button("preview_file")

    parameter_mapping_save_task <- ExtendedTask$new(function(request) {
      promises::future_promise(seed = TRUE, expr = {
        tryCatch(
          addDiscData_run_mapping_save(request),
          error = function(e) {
            list(
              ok = FALSE,
              saved = FALSE,
              request = request,
              message = conditionMessage(e)
            )
          }
        )
      })
    }) |>
      bslib::bind_task_button("save_parameter_mappings")

    location_mapping_save_task <- ExtendedTask$new(function(request) {
      promises::future_promise(seed = TRUE, expr = {
        tryCatch(
          addDiscData_run_mapping_save(request),
          error = function(e) {
            list(
              ok = FALSE,
              saved = FALSE,
              request = request,
              message = conditionMessage(e)
            )
          }
        )
      })
    }) |>
      bslib::bind_task_button("save_location_mapping")

    location_create_task <- ExtendedTask$new(function(request) {
      promises::future_promise(seed = TRUE, expr = {
        tryCatch(
          addDiscData_run_location_create(request),
          error = function(e) {
            list(ok = FALSE, message = conditionMessage(e))
          }
        )
      })
    }) |>
      bslib::bind_task_button("create_new_locations")

    upload_task <- ExtendedTask$new(function(request) {
      promises::future_promise(seed = TRUE, expr = {
        tryCatch(
          addDiscData_run_upload(request),
          error = function(e) {
            list(ok = FALSE, message = conditionMessage(e))
          }
        )
      })
    }) |>
      bslib::bind_task_button("upload")
    upload_summary <- reactiveVal(data.frame())

    refresh_current_preview <- function(
      profile,
      preserve_edits = TRUE,
      parsed = NULL
    ) {
      path <- data$raw_file_path
      if (
        is.null(path) ||
          !length(path) ||
          is.na(path) ||
          !nzchar(path) ||
          !file.exists(path)
      ) {
        if (!is.null(data$preview_profile_key)) {
          data$preview_is_stale <- TRUE
        }
        return(invisible(FALSE))
      }
      data$preview_is_stale <- TRUE
      if (is.null(parsed)) {
        parsed <- addDiscData_parse_upload(path, profile)
      }
      location_mappings <- AquaCache::getImportLocationMappings(
        con = con,
        source_code = profile$source_code[[1]],
        profile_code = profile$profile_code[[1]],
        active = TRUE,
        include_draft = TRUE
      )
      parsed <- addDiscData_location_match(
        parsed,
        locations(),
        location_mappings
      )
      parsed <- addDiscData_apply_blank_sample_types(parsed, sample_types)
      parsed <- addDiscData_apply_mappings(
        parsed,
        con,
        profile_code = profile$profile_code[[1]]
      )
      parsed <- parsed[names(addDiscData_empty_table())]
      data$df <- if (preserve_edits) {
        addDiscData_merge_preview_edits(
          parsed,
          data$df,
          data$preview_base
        )
      } else {
        parsed
      }
      data$df <- addDiscData_apply_blank_sample_types(data$df, sample_types)
      data$preview_base <- parsed
      data$preview_profile_key <- addDiscData_profile_key(
        profile$source_code[[1]],
        profile$profile_code[[1]]
      )
      data$preview_is_stale <- !is.null(input$file) &&
        !identical(input$file$datapath, path)
      invisible(TRUE)
    }

    apply_mapping_save_result <- function(result) {
      request <- result$request
      label <- if (identical(request$kind, "parameter")) {
        "parameter mapping"
      } else {
        "location mapping"
      }
      if (isTRUE(result$saved)) {
        mapping_revision(mapping_revision() + 1L)
      }
      if (!isTRUE(result$ok)) {
        if (isTRUE(result$saved) && isTRUE(request$refresh_preview)) {
          data$preview_is_stale <- TRUE
        }
        showNotification(
          if (isTRUE(result$saved)) {
            paste("Saved", label, "but preview refresh failed:", result$message)
          } else {
            paste("Saving", label, "failed:", result$message)
          },
          type = if (isTRUE(result$saved)) "warning" else "error"
        )
        return(invisible(NULL))
      }

      refreshed <- FALSE
      if (!is.null(result$parsed)) {
        current_path <- if (is.null(input$file)) {
          NULL
        } else {
          input$file$datapath
        }
        still_current <- identical(current_path, request$path) &&
          identical(data$raw_file_path, request$path) &&
          identical(data$preview_profile_key, request$profile_key)
        if (still_current) {
          data$df <- addDiscData_merge_preview_edits(
            result$parsed,
            data$df,
            data$preview_base
          )
          data$preview_base <- result$parsed
          data$preview_profile_key <- request$profile_key
          data$preview_is_stale <- FALSE
          refreshed <- TRUE
        } else if (!is.null(data$preview_profile_key)) {
          data$preview_is_stale <- TRUE
        }
      }

      message <- if (!isTRUE(request$refresh_preview)) {
        paste("Saved", label, ". Preview a workbook to apply it.")
      } else if (refreshed) {
        paste(
          "Saved",
          label,
          "and refreshed the preview while retaining your sample edits."
        )
      } else {
        paste(
          "Saved",
          label,
          "but the selected workbook changed before refresh completed."
        )
      }
      showNotification(
        message,
        type = if (refreshed || !request$refresh_preview) {
          "message"
        } else {
          "warning"
        }
      )
      invisible(NULL)
    }

    observeEvent(parameter_mapping_save_task$result(), {
      apply_mapping_save_result(parameter_mapping_save_task$result())
    })

    observeEvent(location_mapping_save_task$result(), {
      apply_mapping_save_result(location_mapping_save_task$result())
    })

    observeEvent(location_create_task$result(), {
      result <- location_create_task$result()
      if (!isTRUE(result$ok)) {
        showNotification(
          paste("Creating locations failed:", result$message),
          type = "error"
        )
        return()
      }

      mapping_revision(mapping_revision() + 1L)
      locations(read_locations())
      update_location_selectize()
      refresh_error <- tryCatch(
        {
          if (
            isTRUE(refresh_current_preview(
              result$profile,
              preserve_edits = TRUE
            ))
          ) {
            NULL
          } else {
            "The locations and mappings were saved. Preview the file again to apply them."
          }
        },
        error = function(e) e$message
      )
      summary_rows <- lapply(seq_len(nrow(result$added)), function(i) {
        tags$tr(
          tags$td(result$added$name[[i]]),
          tags$td(result$added$location_code[[i]])
        )
      })
      showModal(modalDialog(
        title = "New locations created",
        tags$p(
          "Location codes are generated automatically. Elevations are fetched from web services when a usable value is available."
        ),
        tags$table(
          class = "table table-sm",
          tags$thead(tags$tr(
            tags$th("Location name"),
            tags$th("Location code")
          )),
          tags$tbody(summary_rows)
        ),
        if (any(result$elevation_fallback)) {
          tags$p(
            class = "text-warning",
            paste0(
              "Elevation services could not provide an elevation for ",
              paste(result$names[result$elevation_fallback], collapse = ", "),
              ". Elevation was set to 0 m using the assumed datum. Review these locations under Locations -> Add/modify locations."
            )
          )
        },
        if (is.null(refresh_error)) {
          tags$p("The new source mappings are ready in this preview.")
        } else {
          tags$p(
            class = "text-warning",
            paste("The locations were created, but:", refresh_error)
          )
        },
        tags$p(
          "For further changes, go to Locations -> Add/modify locations."
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }, ignoreInit = TRUE)

    observeEvent(
      input$file,
      {
        if (
          !is.null(data$preview_profile_key) &&
            !identical(input$file$datapath, data$raw_file_path)
        ) {
          data$preview_is_stale <- TRUE
        }
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$import_profile,
      {
        if (
          identical(input$entry_mode, "file") &&
            !is.null(data$raw_file_path) &&
            !identical(input$import_profile, data$preview_profile_key)
        ) {
          profile <- tryCatch(selected_profile(), error = function(e) NULL)
          if (!is.null(profile) && nrow(profile)) {
            tryCatch(
              {
                refreshed <- refresh_current_preview(
                  profile,
                  preserve_edits = TRUE
                )
                if (!isTRUE(refreshed)) {
                  showNotification(
                    "The earlier upload is no longer available. Select the file again and preview it.",
                    type = "warning"
                  )
                }
              },
              error = function(e) {
                showNotification(
                  paste("Refreshing the preview failed:", e$message),
                  type = "error"
                )
              }
            )
          }
        }
      },
      ignoreInit = TRUE
    )

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
        list(
          media_id = 1L,
          collection_method = 27L,
          sample_type = 34L,
          owner = 1L,
          result_type = 2L,
          matrix_state_id = 1L,
          result_value_type = 1L,
          laboratory = 2L
        )
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
              choices = stats::setNames(
                organizations$organization_id,
                organizations$name
              ),
              selected = defaults$owner
            )
          ),
          column(
            4,
            selectizeInput(
              ns("new_profile_default_lab"),
              "Laboratory",
              choices = stats::setNames(
                laboratories$lab_id,
                laboratories$lab_name
              ),
              selected = defaults$laboratory
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
            "Changes are saved to this workbook format and will be used by future previews. If its file is already previewed, YGwater will refresh the preview and retain your edits. The source and format codes identify the saved format; create a copy if you need different codes."
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
              value = addDiscData_profile_value(profile, "source_code", "")
            )
          ),
          column(
            8,
            textInput(
              ns("new_profile_source_name"),
              "Lab/source name",
              value = addDiscData_profile_value(profile, "source_name", "")
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
              tags$span(
                "Workbook layout",
                title = paste(
                  "Choose the layout that matches the worksheet. Long table: one result per row. ",
                  "Transposed table: each sample is a column and each analyte is a row. ",
                  "XLR Detailed Report: the lab's standard detailed report export."
                )
              ),
              choices = c(
                "Long table — one result per row" = "long",
                "Transposed table — one sample per column" = "transposed",
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
          profile <- new_profile_template()
          source_code <- toupper(trimws(addDiscData_first(
            input$new_profile_source_code,
            ""
          )))
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
            media_id = addDiscData_int(input$new_profile_default_media, 1L),
            collection_method = addDiscData_int(
              input$new_profile_default_method,
              27L
            ),
            sample_type = addDiscData_int(
              input$new_profile_default_sample_type,
              34L
            ),
            owner = addDiscData_int(input$new_profile_default_owner, 1L),
            result_type = addDiscData_int(
              input$new_profile_default_result_type,
              2L
            ),
            matrix_state_id = addDiscData_int(
              input$new_profile_default_matrix,
              1L
            ),
            result_value_type = addDiscData_int(
              input$new_profile_default_value_type,
              1L
            ),
            laboratory = addDiscData_int(input$new_profile_default_lab, 2L)
          )
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
              header_row = addDiscData_profile_value(profile, "header_row", 1L),
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
                  "result_speciation_id",
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
              note = "Created from YGwater add discrete data."
            )
          })
          selected_after_save <- if (identical(profile_mode, "edit")) {
            new_profile_key
          } else {
            addDiscData_first(input$import_profile, new_profile_key)
          }
          reload_profiles(selected = selected_after_save)
          updated_profile <- import_profiles()[
            import_profiles()$profile_key == new_profile_key,
            ,
            drop = FALSE
          ]
          preview_refresh_error <- NULL
          if (
            identical(data$preview_profile_key, new_profile_key) &&
              nrow(updated_profile) &&
              !is.null(data$raw_file_path) &&
              identical(input$entry_mode, "file")
          ) {
            preview_refresh_error <- tryCatch(
              {
                refreshed <- refresh_current_preview(
                  updated_profile,
                  preserve_edits = TRUE
                )
                if (isTRUE(refreshed)) {
                  NULL
                } else {
                  "The earlier upload is no longer available. Select it again and preview it."
                }
              },
              error = function(e) e$message
            )
          }
          new_profile_template(NULL)
          removeModal()
          showNotification(
            if (!is.null(preview_refresh_error)) {
              paste(
                "Workbook format saved, but the current preview could not be refreshed:",
                preview_refresh_error
              )
            } else if (identical(profile_mode, "edit")) {
              "Workbook format updated."
            } else {
              "Workbook format created."
            },
            type = if (is.null(preview_refresh_error)) "message" else "warning"
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
      profiles <- import_profiles()
      req(nrow(profiles), input$manage_import_profile)
      hit <- profiles[
        profiles$profile_key == input$manage_import_profile,
        ,
        drop = FALSE
      ]
      validate(need(nrow(hit) == 1L, "Select a workbook format."))
      hit
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
          "No workbook formats are saved. Create one here before managing mappings."
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

    observe({
      has_profile <- nrow(import_profiles()) > 0L
      if (can_manage_profiles && has_profile) {
        shinyjs::enable("copy_import_profile")
        shinyjs::enable("edit_import_profile")
      } else {
        shinyjs::disable("copy_import_profile")
        shinyjs::disable("edit_import_profile")
      }
      if (can_manage_mappings && has_profile) {
        shinyjs::enable("manage_new_mapping")
        shinyjs::enable("manage_save_parameter_mapping")
        shinyjs::enable("manage_save_location_mapping")
        shinyjs::enable("manage_save_flag_mapping")
      } else {
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
        selected = ""
      )
      updateSelectizeInput(
        session,
        "manage_result_type",
        choices = stats::setNames(
          as.character(result_types$result_type_id),
          result_types$result_type
        ),
        selected = "2"
      )
      updateSelectizeInput(
        session,
        "manage_matrix_state",
        choices = stats::setNames(
          as.character(matrix_states$matrix_state_id),
          matrix_states$matrix_state_name
        ),
        selected = "1"
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
        selected = "1"
      )
      updateSelectizeInput(
        session,
        "manage_speciation",
        choices = c(
          "None" = "",
          stats::setNames(
            as.character(result_speciations$result_speciation_id),
            result_speciations$result_speciation
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
              profile_code = profile$profile_code[[1]],
              active = NULL,
              include_draft = TRUE
            ),
            location = AquaCache::getImportLocationMappings(
              con,
              source_code = profile$source_code[[1]],
              profile_code = profile$profile_code[[1]],
              active = NULL,
              include_draft = TRUE
            ),
            result_flag = AquaCache::getImportResultFlagMappings(
              con,
              source_code = profile$source_code[[1]],
              profile_code = profile$profile_code[[1]],
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
          source_code <- vapply(
            source_match,
            function(value) {
              as.character(addDiscData_first(
                value$parameter_code,
                addDiscData_first(value$input_param, "")
              ))
            },
            character(1)
          )
          source_unit <- vapply(
            source_match,
            function(value) {
              as.character(addDiscData_first(
                value$unit,
                addDiscData_first(value$input_unit, "")
              ))
            },
            character(1)
          )
          parameter_index <- match(rows$parameter_id, params()$parameter_id)
          summary <- data.frame(
            Scope = scope,
            `Source parameter code` = source_code,
            `Source unit` = source_unit,
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
            c("Source parameter code", "Source unit")
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
        clear_manager_mapping_selection()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$manage_mapping_table_rows_selected,
      {
        row <- selected_manage_mapping()
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
          updateTextInput(
            session,
            "manage_parameter_code",
            value = manager_text(
              source_match$parameter_code,
              manager_text(source_match$input_param)
            )
          )
          updateTextInput(
            session,
            "manage_parameter_unit",
            value = manager_text(
              source_match$unit,
              manager_text(source_match$input_unit)
            )
          )
          updateSelectInput(session, "manage_mapping_scope", selected = scope)
          updateSelectizeInput(
            session,
            "manage_parameter_id",
            selected = as.character(row$parameter_id[[1]])
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
            "manage_speciation",
            selected = if (is.na(row$result_speciation_id[[1]])) {
              ""
            } else {
              as.character(row$result_speciation_id[[1]])
            }
          )
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
      },
      ignoreInit = TRUE
    )

    observeEvent(input$manage_new_mapping, {
      clear_manager_mapping_selection()
      updateTextInput(session, "manage_parameter_code", value = "")
      updateTextInput(session, "manage_parameter_unit", value = "")
      updateSelectInput(session, "manage_mapping_scope", selected = "profile")
      updateSelectizeInput(session, "manage_parameter_id", selected = "")
      updateSelectizeInput(session, "manage_result_type", selected = "2")
      updateSelectizeInput(session, "manage_matrix_state", selected = "1")
      updateSelectizeInput(session, "manage_sample_fraction", selected = "")
      updateSelectizeInput(session, "manage_value_type", selected = "1")
      updateSelectizeInput(session, "manage_speciation", selected = "")
      updateNumericInput(session, "manage_priority", value = 50)
      updateNumericInput(session, "manage_conversion", value = 1)
      updateNumericInput(session, "manage_result_offset", value = 0)
      updateCheckboxInput(session, "manage_active", value = TRUE)
      updateTextAreaInput(session, "manage_mapping_note", value = "")
      updateTextInput(session, "manage_location_code", value = "")
      updateTextInput(session, "manage_location_name", value = "")
      updateSelectInput(session, "manage_location_scope", selected = "profile")
      updateSelectizeInput(session, "manage_location_id", selected = "")
      updateSelectizeInput(session, "manage_sub_location_id", selected = "")
      updateNumericInput(session, "manage_location_priority", value = 50)
      updateCheckboxInput(session, "manage_location_active", value = TRUE)
      updateTextAreaInput(session, "manage_location_note", value = "")
      updateTextInput(session, "manage_flag_column", value = "")
      updateTextInput(session, "manage_flag_value", value = "")
      updateSelectInput(session, "manage_flag_scope", selected = "profile")
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
      scope_input <- switch(
        kind,
        parameter = "manage_mapping_scope",
        location = "manage_location_scope",
        result_flag = "manage_flag_scope"
      )
      scope <- addDiscData_first(input[[scope_input]], "profile")
      profile_code <- if (identical(scope, "profile")) {
        profile$profile_code[[1]]
      } else if (identical(scope, "source")) {
        NULL
      } else {
        stop("Choose where this mapping should apply.")
      }
      source_name <- manager_text(
        profile$source_name[[1]],
        profile$source_code[[1]]
      )
      if (identical(kind, "parameter")) {
        parameter_code <- trimws(addDiscData_first(
          input$manage_parameter_code,
          ""
        ))
        unit <- trimws(addDiscData_first(input$manage_parameter_unit, ""))
        parameter_id <- addDiscData_int(input$manage_parameter_id)
        if (!nzchar(parameter_code)) {
          stop("Enter a source parameter code.")
        }
        if (is.na(parameter_id)) {
          stop("Choose an AquaCache parameter.")
        }
        sample_fraction_id <- addDiscData_int(input$manage_sample_fraction)
        result_speciation_id <- addDiscData_int(input$manage_speciation)
        validate_parameter_mapping_descriptors(
          parameter_id,
          sample_fraction_id,
          result_speciation_id
        )
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
            parameter_code = parameter_code,
            unit = unit,
            parameter_id = parameter_id,
            result_type = addDiscData_int(input$manage_result_type, 2L),
            sample_fraction_id = sample_fraction_id,
            result_value_type = addDiscData_int(input$manage_value_type, 1L),
            result_speciation_id = result_speciation_id,
            matrix_state_id = addDiscData_int(input$manage_matrix_state, 1L),
            conversion = conversion,
            result_offset = result_offset,
            priority = addDiscData_int(input$manage_priority, 50L),
            active = isTRUE(input$manage_active),
            note = addDiscData_first(input$manage_mapping_note, NA_character_),
            stringsAsFactors = FALSE
          ),
          match_columns = c("parameter_code", "unit"),
          publish = TRUE
        )
      } else if (identical(kind, "location")) {
        location_code <- trimws(addDiscData_first(
          input$manage_location_code,
          ""
        ))
        location_id <- addDiscData_int(input$manage_location_id)
        sub_location_id <- addDiscData_int(input$manage_sub_location_id)
        if (!nzchar(location_code)) {
          stop("Enter a source location code.")
        }
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
          publish = TRUE
        )
      } else {
        flag_value <- trimws(addDiscData_first(input$manage_flag_value, ""))
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
          publish = TRUE
        )
      }

      mapping_revision(mapping_revision() + 1L)
      clear_manager_mapping_selection()
      current_profile <- import_profiles()[
        import_profiles()$profile_key == data$preview_profile_key,
        ,
        drop = FALSE
      ]
      applies_to_preview <- nrow(current_profile) &&
        identical(current_profile$source_code[[1]], profile$source_code[[1]]) &&
        (identical(scope, "source") ||
          identical(
            current_profile$profile_code[[1]],
            profile$profile_code[[1]]
          ))
      preview_refresh_error <- NULL
      if (isTRUE(applies_to_preview) && identical(input$entry_mode, "file")) {
        preview_refresh_error <- tryCatch(
          {
            refreshed <- refresh_current_preview(
              current_profile,
              preserve_edits = TRUE
            )
            if (isTRUE(refreshed)) {
              NULL
            } else {
              "The earlier upload is no longer available. Select it again and preview it."
            }
          },
          error = function(e) e$message
        )
      }
      preview_refresh_error
    }

    observeEvent(input$manage_save_parameter_mapping, {
      tryCatch(
        {
          refresh_error <- save_manager_mapping("parameter")
          showNotification(
            if (is.null(refresh_error)) {
              "Parameter mapping saved and published."
            } else {
              paste(
                "Parameter mapping saved, but the current preview could not be refreshed:",
                refresh_error
              )
            },
            type = if (is.null(refresh_error)) "message" else "warning"
          )
        },
        error = function(e) {
          showNotification(
            paste("Saving parameter mapping failed:", e$message),
            type = "error"
          )
        }
      )
    })
    observeEvent(input$manage_save_location_mapping, {
      tryCatch(
        {
          refresh_error <- save_manager_mapping("location")
          showNotification(
            if (is.null(refresh_error)) {
              "Location mapping saved and published."
            } else {
              paste(
                "Location mapping saved, but the current preview could not be refreshed:",
                refresh_error
              )
            },
            type = if (is.null(refresh_error)) "message" else "warning"
          )
        },
        error = function(e) {
          showNotification(
            paste("Saving location mapping failed:", e$message),
            type = "error"
          )
        }
      )
    })
    observeEvent(input$manage_save_flag_mapping, {
      tryCatch(
        {
          refresh_error <- save_manager_mapping("result_flag")
          showNotification(
            if (is.null(refresh_error)) {
              "Result-flag mapping saved and published."
            } else {
              paste(
                "Result-flag mapping saved, but the current preview could not be refreshed:",
                refresh_error
              )
            },
            type = if (is.null(refresh_error)) "message" else "warning"
          )
        },
        error = function(e) {
          showNotification(
            paste("Saving result-flag mapping failed:", e$message),
            type = "error"
          )
        }
      )
    })

    observeEvent(input$preview_file, {
      req(input$file)
      profile <- selected_profile()
      path <- input$file$datapath
      profile_key <- addDiscData_profile_key(
        profile$source_code[[1]],
        profile$profile_code[[1]]
      )
      same_file <- identical(data$raw_file_path, path) &&
        identical(data$preview_profile_key, profile_key)
      data$raw_file_path <- path
      data$preview_is_stale <- TRUE
      if (!same_file) {
        data$df <- addDiscData_empty_table()
        data$preview_base <- addDiscData_empty_table()
        sample_qualifier_map(list())
        sample_observer_map(list())
        sample_share_map(list())
        selected_sample_key(NULL)
        pending_sample_group(NULL)
      }
      preview_parse_task$invoke(list(
        path = path,
        profile = profile,
        profile_key = profile_key,
        same_file = same_file
      ))
    })

    observeEvent(preview_parse_task$result(), {
      result <- preview_parse_task$result()
      if (!isTRUE(result$ok)) {
        showNotification(
          paste("Preview failed:", result$message),
          type = "error"
        )
        return()
      }

      request <- result$request
      current_path <- if (is.null(input$file)) {
        NULL
      } else {
        input$file$datapath
      }
      current_profile <- tryCatch(selected_profile(), error = function(e) NULL)
      current_profile_key <- if (
        !is.null(current_profile) && nrow(current_profile)
      ) {
        addDiscData_profile_key(
          current_profile$source_code[[1]],
          current_profile$profile_code[[1]]
        )
      } else {
        NULL
      }
      if (
        !identical(current_path, request$path) ||
          !identical(current_profile_key, request$profile_key)
      ) {
        showNotification(
          paste(
            "The selected file or workbook format changed while the file was being parsed. ",
            "Click 'Parse file' to parse the current selection again."
          ),
          type = "warning"
        )
        return()
      }

      tryCatch(
        {
          refresh_current_preview(
            request$profile,
            preserve_edits = request$same_file,
            parsed = result$parsed
          )
          showNotification(
            sprintf(
              "Parsed %s result rows from %s sample(s).",
              nrow(data$df),
              length(unique(data$df$sample_key))
            )
          )
        },
        error = function(e) {
          showNotification(
            paste("Preview failed:", e$message),
            type = "error"
          )
        }
      )
    })

    selected_sample_key <- reactiveVal(NULL)
    observeEvent(
      input$sample_location_summary_rows_selected,
      {
        rows <- sample_location_rows()
        index <- input$sample_location_summary_rows_selected
        if (length(index) == 1L && index >= 1L && index <= nrow(rows)) {
          selected_sample_key(rows$sample_key[[index]])
        } else {
          selected_sample_key(NULL)
        }
      },
      ignoreInit = FALSE
    )

    selected_sample_row <- reactive({
      rows <- sample_location_rows()
      if (!nrow(rows)) {
        return(NULL)
      }
      key <- selected_sample_key()
      if (is.null(key)) {
        return(NULL)
      }
      index <- which(rows$sample_key == key)
      if (!length(index)) {
        return(NULL)
      }
      rows[index[[1]], , drop = FALSE]
    })

    observeEvent(
      input$new_sample,
      {
        selected_sample_key(NULL)
        pending_sample_group(NULL)
        DT::selectRows(
          DT::dataTableProxy("sample_location_summary", session = session),
          NULL
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(input$open_create_observer, {
      showModal(modalDialog(
        title = "Add new observer",
        textInput(ns("new_observer_first"), "First name"),
        textInput(ns("new_observer_last"), "Last name"),
        textInput(ns("new_observer_org"), "Organization"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_new_observer"), "Add observer")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(
      input$save_new_observer,
      {
        if (!isTRUE(observer_permissions$can_create_observer[[1]])) {
          showNotification(
            "Your database role cannot create observers.",
            type = "error"
          )
          return()
        }
        first <- trimws(addDiscData_first(input$new_observer_first, ""))
        last <- trimws(addDiscData_first(input$new_observer_last, ""))
        organization <- trimws(addDiscData_first(input$new_observer_org, ""))
        if (!nzchar(first) || !nzchar(last) || !nzchar(organization)) {
          showNotification(
            "Observer first name, last name, and organization are required.",
            type = "error"
          )
          return()
        }
        current_observers <- observers()
        label <- paste0(first, " ", last, " (", organization, ")")
        existing <- which(
          addDiscData_observer_labels(current_observers) == label
        )
        if (length(existing)) {
          observer_id <- current_observers$observer_id[[existing[[1]]]]
          message <- "Existing observer selected."
        } else {
          observer_id <- tryCatch(
            DBI::dbGetQuery(
              con,
              "INSERT INTO instruments.observers (
                 observer_first, observer_last, organization
               ) VALUES ($1, $2, $3)
               RETURNING observer_id",
              params = list(first, last, organization)
            )$observer_id[[1]],
            error = function(e) e
          )
          if (inherits(observer_id, "error")) {
            showNotification(
              paste("Creating observer failed:", conditionMessage(observer_id)),
              type = "error"
            )
            return()
          }
          current_observers <- rbind(
            current_observers,
            data.frame(
              observer_id = as.integer(observer_id),
              observer_first = first,
              observer_last = last,
              organization = organization,
              stringsAsFactors = FALSE
            )
          )
          observers(current_observers)
          message <- "Observer created."
        }
        selected_ids <- unique(c(
          as.character(input$edit_sample_observers),
          as.character(observer_id)
        ))
        updateSelectizeInput(
          session,
          "edit_sample_observers",
          choices = observer_choices(current_observers),
          selected = selected_ids,
          server = TRUE
        )
        removeModal()
        showNotification(message, type = "message")
      },
      ignoreInit = TRUE
    )

    output$manual_result_sample_label <- renderUI({
      row <- selected_sample_row()
      if (is.null(row)) {
        return(tags$div(
          class = "alert alert-info",
          "Create or select a sample above before adding results."
        ))
      }
      tags$div(
        class = "alert alert-info",
        paste("Adding a result to sample", row$source_sample_id[[1]])
      )
    })

    output$manual_lab_metadata <- renderUI({
      if (is_field_result_type(input$manual_result_type)) {
        return(NULL)
      }
      fluidRow(
        column(
          4,
          selectizeInput(
            ns("manual_laboratory"),
            "Laboratory",
            choices = stats::setNames(
              laboratories$lab_id,
              laboratories$lab_name
            ),
            multiple = TRUE,
            options = list(
              maxItems = 1,
              placeholder = "Enter if applicable"
            ),
            width = "100%"
          )
        ),
        column(
          4,
          textInput(
            ns("manual_lab_report"),
            "Lab report number",
            placeholder = "Optional",
            width = "100%"
          )
        ),
        column(
          4,
          textInput(
            ns("manual_lab_sample"),
            "Lab sample number",
            placeholder = "Optional",
            width = "100%"
          )
        )
      )
    })

    observeEvent(
      list(input$manual_result_condition, input$manual_condition_value),
      {
        result_value <- addDiscData_num(input$manual_result)
        condition_id <- addDiscData_int(input$manual_result_condition)
        condition_value <- addDiscData_num(input$manual_condition_value)
        if (
          !is.na(result_value) &&
            (!is.na(condition_id) || !is.na(condition_value))
        ) {
          showNotification(
            "A result value cannot be combined with a result condition. The condition and its value were cleared.",
            type = "warning"
          )
          updateSelectizeInput(
            session,
            "manual_result_condition",
            selected = character()
          )
          updateNumericInput(
            session,
            "manual_condition_value",
            value = NA_real_
          )
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$manual_result,
      {
        result_value <- addDiscData_num(input$manual_result)
        condition_id <- addDiscData_int(input$manual_result_condition)
        condition_value <- addDiscData_num(input$manual_condition_value)
        if (
          !is.na(result_value) &&
            (!is.na(condition_id) || !is.na(condition_value))
        ) {
          showNotification(
            "A result value cannot be combined with a result condition. The result value was cleared.",
            type = "warning"
          )
          updateNumericInput(
            session,
            "manual_result",
            value = NA_real_
          )
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$create_sample_group,
      {
        if (!isTRUE(check_groups$can_create_group[[1]])) {
          showNotification(
            "Your database role cannot create sample groups.",
            type = "error"
          )
          return()
        }
        if (!nrow(sample_group_types)) {
          showNotification(
            "No active sample-group types are available.",
            type = "error"
          )
          return()
        }
        existing <- selected_sample_row()
        owner_id <- if (is.null(existing)) {
          addDiscData_int(input$edit_sample_owner)
        } else {
          addDiscData_int(existing$owner[[1]])
        }
        if (is.na(owner_id) && nrow(organizations)) {
          owner_id <- as.integer(organizations$organization_id[[1]])
        }
        default_type <- if ("trip" %in% sample_group_types$group_type) {
          "trip"
        } else {
          sample_group_types$group_type[[1]]
        }
        showModal(modalDialog(
          title = "Create sample group",
          helpText(
            "Use a group for samples connected by a trip, field event, cooler, shipment, lab batch, or quality-control set. Create it once, then assign each sample to the right group in Sample details."
          ),
          selectizeInput(
            ns("new_sample_group_type"),
            "Group type",
            choices = stats::setNames(
              sample_group_types$group_type,
              sample_group_types$group_type_name
            ),
            selected = default_type,
            options = list(placeholder = "Choose a group type", maxItems = 1),
            width = "100%"
          ),
          textInput(
            ns("new_sample_group_name"),
            "Group name",
            placeholder = "For example: Tatchun Creek trip, 2026-07-23",
            width = "100%"
          ),
          textInput(
            ns("new_sample_group_code"),
            "Group code (optional)",
            placeholder = "Use the trip, cooler, shipment, or batch code if available",
            width = "100%"
          ),
          selectizeInput(
            ns("new_sample_group_owner"),
            "Owner",
            choices = stats::setNames(
              as.character(organizations$organization_id),
              organizations$name
            ),
            selected = as.character(owner_id),
            options = list(placeholder = "Choose an owner", maxItems = 1),
            width = "100%"
          ),
          textAreaInput(
            ns("new_sample_group_note"),
            "Note (optional)",
            rows = 2,
            placeholder = "Add context that will help identify this group later",
            width = "100%"
          ),
          selectizeInput(
            ns("new_sample_group_share_with"),
            "Visible to",
            choices = sample_group_share_choices,
            selected = "public_reader",
            multiple = TRUE,
            options = list(
              placeholder = "All users",
              plugins = list("remove_button"),
              dropdownParent = "body"
            )
          ),
          helpText(
            "New sample groups are visible to all users by default. To restrict visibility, remove All users and then select one or more access groups."
          ),
          footer = tagList(
            actionButton(ns("cancel_new_sample_group"), "Cancel"),
            actionButton(
              ns("save_new_sample_group"),
              "Create group and select it",
              class = "btn-primary"
            )
          ),
          easyClose = FALSE,
          size = "l",
        ))
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$cancel_new_sample_group,
      removeModal(),
      ignoreInit = TRUE
    )

    observeEvent(
      input$save_new_sample_group,
      {
        group_type <- trimws(addDiscData_first(
          input$new_sample_group_type,
          ""
        ))
        group_name <- trimws(addDiscData_first(
          input$new_sample_group_name,
          ""
        ))
        group_code <- trimws(addDiscData_first(
          input$new_sample_group_code,
          ""
        ))
        group_owner <- addDiscData_int(input$new_sample_group_owner)
        group_note <- trimws(addDiscData_first(
          input$new_sample_group_note,
          ""
        ))
        group_share_with <- tryCatch(
          addDiscData_share_selection(
            input$new_sample_group_share_with,
            sample_group_share_choices
          ),
          error = function(e) e
        )
        if (inherits(group_share_with, "error")) {
          showNotification(
            paste("Invalid sample-group sharing selection:", group_share_with$message),
            type = "error"
          )
          return()
        }
        if (!group_type %in% sample_group_types$group_type) {
          showNotification("Choose a group type.", type = "error")
          return()
        }
        if (!nzchar(group_name) && !nzchar(group_code)) {
          showNotification(
            "Enter a group name or group code.",
            type = "error"
          )
          return()
        }
        if (is.na(group_owner)) {
          showNotification("Choose an owner for this group.", type = "error")
          return()
        }
        tryCatch(
          {
            inserted <- DBI::dbGetQuery(
              con,
              "INSERT INTO discrete.sample_groups (
             group_type, group_code, group_name, owner, note, share_with
           ) VALUES (
             $1, NULLIF($2::TEXT, ''), NULLIF($3::TEXT, ''), $4,
             NULLIF($5::TEXT, ''),
             $6::TEXT[]
           )
           RETURNING sample_group_id",
              params = list(
                group_type,
                group_code,
                group_name,
                group_owner,
                group_note,
                share_with_to_array(group_share_with)
              )
            )
            group_id <- as.integer(inserted$sample_group_id[[1]])
            existing <- selected_sample_row()
            pending_sample_group(list(
              sample_key = if (is.null(existing)) {
                NA_character_
              } else {
                as.character(existing$sample_key[[1]])
              },
              sample_group_id = group_id
            ))
            group_rows <- read_sample_groups()
            sample_groups(group_rows)
            updateSelectizeInput(
              session,
              "edit_sample_group",
              choices = c(
                "None" = "",
                stats::setNames(
                  as.character(group_rows$sample_group_id),
                  addDiscData_sample_group_labels(group_rows)
                )
              ),
              selected = as.character(group_id)
            )
            removeModal()
            showNotification(
              "Group created and selected. Save the sample details to keep the assignment.",
              type = "message",
              duration = 8
            )
          },
          error = function(e) {
            message_text <- if (
              grepl(
                "duplicate|unique",
                conditionMessage(e),
                ignore.case = TRUE
              )
            ) {
              "A group with that owner, type, and code already exists. Select it in Sample group or use a different code."
            } else {
              paste("Creating the sample group failed:", conditionMessage(e))
            }
            showNotification(message_text, type = "error", duration = 10)
          }
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(input$save_sample, {
      existing <- selected_sample_row()
      location_id <- addDiscData_int(input$edit_sample_location)
      sub_location_id <- addDiscData_int(input$edit_sample_sublocation)
      sample_type_id <- addDiscData_int(input$edit_sample_type)
      sample_type_index <- match(sample_type_id, sample_types$sample_type_id)
      sample_type_name <- if (is.na(sample_type_index)) {
        ""
      } else {
        tolower(trimws(sample_types$sample_type[[sample_type_index]]))
      }
      is_blank_sample <- sample_type_name %in%
        c(
          "qc-sample-field blank",
          "qc-sample-trip blank"
        )
      if (is_blank_sample) {
        location_id <- NA_integer_
        sub_location_id <- NA_integer_
      }
      if (
        !is.na(sub_location_id) &&
          !any(
            sub_locations$sub_location_id == sub_location_id &
              sub_locations$location_id == location_id
          )
      ) {
        showNotification(
          "The sub-location does not belong to the selected location.",
          type = "error"
        )
        return()
      }
      sample_datetime <- as.POSIXct(
        addDiscData_first(input$edit_sample_datetime, NA_character_),
        tz = "UTC"
      )
      if (is.na(sample_datetime)) {
        showNotification("Enter a sample datetime.", type = "error")
        return()
      }
      share_values <- tryCatch(
        addDiscData_share_selection(
          input$edit_sample_share_with,
          sample_share_choices
        ),
        error = function(e) e
      )
      if (inherits(share_values, "error")) {
        showNotification(
          paste("Invalid sample sharing selection:", share_values$message),
          type = "error"
        )
        return()
      }
      sample_values <- list(
        location_id = location_id,
        sub_location_id = sub_location_id,
        datetime = sample_datetime,
        media_id = addDiscData_int(input$edit_sample_media),
        collection_method = addDiscData_int(input$edit_sample_method),
        sample_type = sample_type_id,
        sample_group_id = addDiscData_int(input$edit_sample_group),
        owner = addDiscData_int(input$edit_sample_owner),
        contributor = addDiscData_int(input$edit_sample_contributor),
        commissioning_org = addDiscData_int(
          input$edit_sample_commissioning_org
        ),
        sampling_org = addDiscData_int(input$edit_sample_sampling_org),
        target_datetime = as.POSIXct(
          addDiscData_first(
            input$edit_sample_target_datetime,
            NA_character_
          ),
          tz = "UTC"
        ),
        z = addDiscData_num(input$edit_sample_z),
        sample_volume_ml = addDiscData_num(input$edit_sample_volume),
        purge_volume_l = addDiscData_num(input$edit_sample_purge_volume),
        purge_time_min = addDiscData_num(input$edit_sample_purge_time),
        flow_rate_l_min = addDiscData_num(input$edit_sample_flow_rate),
        wave_hgt_m = addDiscData_num(input$edit_sample_wave_height),
        sample_grade = addDiscData_int(input$edit_sample_grade),
        sample_approval = addDiscData_int(input$edit_sample_approval),
        sample_note = addDiscData_first(input$edit_sample_note, "")
      )
      qualifier_values <- suppressWarnings(as.integer(
        input$edit_sample_qualifiers
      ))
      qualifier_values <- unique(qualifier_values[!is.na(qualifier_values)])
      observer_values <- suppressWarnings(as.integer(
        input$edit_sample_observers
      ))
      observer_values <- unique(observer_values[!is.na(observer_values)])
      if (is.null(existing)) {
        current_manual_sample(current_manual_sample() + 1L)
        row <- addDiscData_empty_table()
        row[1, ] <- NA
        row$sample_key <- paste0(manual_upload_id, "-", current_manual_sample())
        row$source_location_name <- ""
        row$location_mapping_status <- if (is_blank_sample) {
          "blank sample; location not required"
        } else {
          "manual"
        }
        row$source_sample_id <- row$sample_key
        row$sample_no_source_update <- FALSE
        row$result_no_source_update <- FALSE
        row$source_code <- "YGwater-manual"
        row$mapping_status <- "manual"
        row$result_type <- addDiscData_int(input$manual_result_type, 2L)
        row$result_value_type <- addDiscData_int(
          input$manual_result_value_type,
          1L
        )
        row$matrix_state_id <- addDiscData_int(input$manual_matrix_state, 1L)
        row$conversion <- 1
        row$result_offset <- 0
        row$note <- ""
        for (nm in names(sample_values)) {
          row[[nm]] <- sample_values[[nm]]
        }
        data$df <- rbind(data$df, row)
        selected_sample_key(row$sample_key[[1]])
        key <- row$sample_key[[1]]
        showNotification(
          sprintf("Created sample %s.", current_manual_sample()),
          type = "message"
        )
      } else {
        key <- existing$sample_key[[1]]
        ix <- which(data$df$sample_key == key)
        for (nm in names(sample_values)) {
          data$df[[nm]][ix] <- sample_values[[nm]]
        }
        location_changed <- !identical(
          location_id,
          addDiscData_int(existing$location_id[[1]])
        ) || !identical(
          sub_location_id,
          addDiscData_int(existing$sub_location_id[[1]])
        )
        data$df$location_mapping_status[ix] <- if (is_blank_sample) {
          "blank sample; location not required"
        } else if (location_changed) {
          "sample location override"
        } else {
          as.character(existing$location_mapping_status[[1]])
        }
        showNotification("Sample metadata updated.", type = "message")
      }
      qualifier_state <- sample_qualifier_map()
      qualifier_state[[key]] <- qualifier_values
      sample_qualifier_map(qualifier_state)
      observer_state <- sample_observer_map()
      observer_state[[key]] <- observer_values
      sample_observer_map(observer_state)
      share_state <- sample_share_map()
      share_state[[key]] <- share_values
      sample_share_map(share_state)
      pending_sample_group(NULL)
    })

    observeEvent(
      input$apply_sample_share_with,
      {
        share_values <- tryCatch(
          addDiscData_share_selection(
            input$edit_sample_share_with,
            sample_share_choices
          ),
          error = function(e) e
        )
        if (inherits(share_values, "error")) {
          showNotification(
            paste("Invalid sample sharing selection:", share_values$message),
            type = "error"
          )
          return()
        }
        sample_keys <- unique(as.character(data$df$sample_key))
        sample_keys <- sample_keys[!is.na(sample_keys) & nzchar(sample_keys)]
        if (!length(sample_keys)) {
          showNotification(
            "There are no samples in the current upload to update.",
            type = "warning"
          )
          return()
        }
        share_state <- sample_share_map()
        for (key in sample_keys) {
          share_state[[key]] <- share_values
        }
        sample_share_map(share_state)
        showNotification(
          sprintf("Applied sharing to %s sample(s).", length(sample_keys)),
          type = "message"
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(input$add_manual_result, {
      req(input$manual_parameter)
      selected <- selected_sample_row()
      if (is.null(selected) || !nrow(selected)) {
        showNotification(
          "Create and select a sample before adding a result.",
          type = "warning"
        )
        return()
      }
      result_value <- addDiscData_num(input$manual_result)
      condition_id <- addDiscData_int(input$manual_result_condition)
      condition_value <- addDiscData_num(input$manual_condition_value)
      if (
        !is.na(result_value) &&
          (!is.na(condition_id) || !is.na(condition_value))
      ) {
        showNotification(
          "A numeric result value cannot be combined with a result condition or condition value.",
          type = "error"
        )
        return()
      }
      if (is.na(result_value) && is.na(condition_id)) {
        showNotification(
          "Enter a numeric result value or choose a result condition.",
          type = "error"
        )
        return()
      }
      if (
        parameter_requirement(input$manual_parameter, "sample_fraction") &&
          is.na(addDiscData_int(input$manual_sample_fraction))
      ) {
        showNotification(
          "Sample fraction is required for the selected parameter.",
          type = "error"
        )
        return()
      }
      if (
        parameter_requirement(input$manual_parameter, "result_speciation") &&
          is.na(addDiscData_int(input$manual_speciation))
      ) {
        showNotification(
          "Speciation is required for the selected parameter.",
          type = "error"
        )
        return()
      }
      row <- addDiscData_empty_table()
      row[1, ] <- NA
      sample_fields <- c(
        "sample_key",
        "source_location_name",
        "location_mapping_status",
        "location_id",
        "sub_location_id",
        "datetime",
        "target_datetime",
        "z",
        "media_id",
        "collection_method",
        "sample_type",
        "sample_group_id",
        "sample_volume_ml",
        "purge_volume_l",
        "purge_time_min",
        "flow_rate_l_min",
        "wave_hgt_m",
        "sample_grade",
        "sample_approval",
        "sample_qualifier",
        "commissioning_org",
        "sampling_org",
        "linked_with",
        "sample_note",
        "owner",
        "contributor",
        "sample_no_source_update",
        "source_sample_id",
        "source_code"
      )
      row[1, sample_fields] <- selected[1, sample_fields]
      row$source_parameter_code <- ""
      row$source_parameter_name <- ""
      row$source_unit <- ""
      row$parameter_id <- addDiscData_int(input$manual_parameter)
      row$result_type <- addDiscData_int(input$manual_result_type, 2L)
      row$matrix_state_id <- addDiscData_int(input$manual_matrix_state, 1L)
      row$sample_fraction_id <- addDiscData_int(input$manual_sample_fraction)
      row$result_value_type <- addDiscData_int(
        input$manual_result_value_type,
        1L
      )
      row$result_speciation_id <- addDiscData_int(input$manual_speciation)
      row$protocol_method <- addDiscData_int(input$manual_protocol)
      row$laboratory <- if (is_field_result_type(row$result_type)) {
        NA_integer_
      } else {
        addDiscData_int(input$manual_laboratory)
      }
      row$grade_type_id <- addDiscData_int(input$manual_grade)
      row$approval_type_id <- addDiscData_int(input$manual_approval)
      row$source_result_text <- if (is.na(result_value)) {
        ""
      } else {
        as.character(result_value)
      }
      row$source_result <- result_value
      row$source_result_condition <- condition_id
      row$source_result_condition_value <- condition_value
      row$result <- result_value
      row$result_condition <- condition_id
      row$result_condition_value <- condition_value
      if (is_field_result_type(row$result_type)) {
        row$lab_report_no <- NA_character_
        row$lab_sample_no <- NA_character_
      } else {
        row$lab_report_no <- trimws(addDiscData_first(
          input$manual_lab_report,
          ""
        ))
        row$lab_sample_no <- trimws(addDiscData_first(
          input$manual_lab_sample,
          ""
        ))
      }
      row$conversion <- 1
      row$result_offset <- 0
      row$analysis_datetime <- as.POSIXct(
        addDiscData_first(input$manual_analysis_datetime, NA_character_),
        tz = "UTC"
      )
      row$note <- addDiscData_first(input$manual_note, "")
      row$source_code <- "YGwater-manual"
      row$mapping_status <- "manual"
      row$source_row_number <- NA_integer_
      data$df <- rbind(data$df, row)
    })

    sample_location_rows <- reactive({
      df <- data$df
      if (!nrow(df)) {
        return(data.frame())
      }
      df <- df[!duplicated(df$sample_key), , drop = FALSE]
      df[order(df$datetime, df$source_sample_id), , drop = FALSE]
    })

    output$sample_location_summary <- DT::renderDT(
      {
        df <- sample_location_rows()
        if (!nrow(df)) {
          return(DT::datatable(
            data.frame(
              Message = "Add or preview data to assign sample locations."
            ),
            rownames = FALSE,
            selection = "none",
            options = list(dom = "t")
          ))
        }
        current_locations <- locations()
        group_rows <- sample_groups()
        location_index <- match(df$location_id, current_locations$location_id)
        group_index <- match(
          df$sample_group_id,
          group_rows$sample_group_id
        )
        sub_location_index <- match(
          df$sub_location_id,
          sub_locations$sub_location_id
        )
        observer_state <- sample_observer_map()
        observer_rows <- observers()
        observer_labels <- addDiscData_observer_labels(observer_rows)
        observer_summary <- vapply(
          df$sample_key,
          function(key) {
            if (is.na(key) || !nzchar(as.character(key))) {
              return("")
            }
            ids <- observer_state[[as.character(key)]]
            if (!length(ids)) {
              return("")
            }
            index <- match(as.integer(ids), observer_rows$observer_id)
            paste(observer_labels[index[!is.na(index)]], collapse = ", ")
          },
          character(1)
        )
        summary <- data.frame(
          `Source sample` = df$source_sample_id,
          `Source location` = df$source_location_name,
          `Sample datetime` = format(
            df$datetime,
            "%Y-%m-%d %H:%M:%S",
            tz = "UTC"
          ),
          `AquaCache location` = addDiscData_location_labels(current_locations)[
            location_index
          ],
          Media = addDiscData_lookup_label(
            df$media_id,
            media,
            "media_id",
            "media_type"
          ),
          `Sample group` = addDiscData_sample_group_labels(group_rows)[
            group_index
          ],
          `Sampler(s)` = observer_summary,
          `Collection method` = addDiscData_lookup_label(
            df$collection_method,
            collection_methods,
            "collection_method_id",
            "collection_method"
          ),
          `Sample type` = addDiscData_lookup_label(
            df$sample_type,
            sample_types,
            "sample_type_id",
            "sample_type"
          ),
          `Sub-location` = sub_locations$sub_location_name[sub_location_index],
          `Location match` = df$location_mapping_status,
          check.names = FALSE
        )
        summary[is.na(summary)] <- ""
        addDiscData_style_origin_columns(
          DT::datatable(
            summary,
            rownames = FALSE,
            selection = "single",
            options = list(
              scrollX = TRUE,
              pageLength = 10,
              initComplete = htmlwidgets::JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'font-size': '90%'});",
                "$(this.api().table().body()).css({'font-size': '80%'});",
                "}"
              )
            )
          ),
          source_columns = c("Source sample", "Source location"),
          target_columns = c(
            "Sample datetime",
            "AquaCache location",
            "Media",
            "Sample group",
            "Collection method",
            "Sample type",
            "Sub-location"
          )
        )
      },
      server = FALSE
    )

    output$sample_editor <- renderUI({
      row <- selected_sample_row()
      selected <- !is.null(row)
      if (!selected) {
        row <- addDiscData_empty_table()
        row[1, ] <- NA
        row$datetime <- as.POSIXct(Sys.time(), tz = "UTC")
        row$media_id <- 1L
        row$collection_method <- 27L
        row$sample_type <- 34L
        row$owner <- 1L
      }
      current_sample_key <- as.character(row$sample_key[[1]])
      sample_key_present <- length(current_sample_key) == 1L &&
        !is.na(current_sample_key) &&
        nzchar(current_sample_key)
      selected_qualifiers <- if (sample_key_present) {
        sample_qualifier_map()[[current_sample_key]]
      } else {
        integer()
      }
      selected_observers <- if (sample_key_present) {
        sample_observer_map()[[current_sample_key]]
      } else {
        integer()
      }
      selected_share <- if (sample_key_present) {
        sample_share_map()[[current_sample_key]]
      } else {
        "public_reader"
      }
      if (is.null(selected_share) || !length(selected_share)) {
        selected_share <- "public_reader"
      }
      location_choices <- addDiscData_location_choices(
        locations(),
        include_blank = TRUE
      )
      group_rows <- sample_groups()
      pending_group <- pending_sample_group()
      selected_group <- if (is.na(row$sample_group_id[[1]])) {
        ""
      } else {
        as.character(row$sample_group_id[[1]])
      }
      pending_matches <- !is.null(pending_group) &&
        if (is.na(pending_group$sample_key)) {
          !selected
        } else {
          selected &&
            identical(
              as.character(row$sample_key[[1]]),
              as.character(pending_group$sample_key)
            )
        }
      if (pending_matches) {
        selected_group <- as.character(pending_group$sample_group_id)
      }
      tagList(
        tags$h5(
          if (selected) {
            paste("Edit sample", row$source_sample_id[[1]])
          } else {
            "New sample details"
          }
        ),
        fluidRow(
          column(
            3,
            selectizeInput(
              ns("edit_sample_location"),
              "Location",
              choices = location_choices,
              selected = ifelse(
                is.na(row$location_id[[1]]),
                "",
                as.character(row$location_id[[1]])
              ),
              options = list(
                create = TRUE,
                placeholder = "Select a location",
                maxItems = 1
              )
            )
          ),
          column(
            1,
            tags$div(
              style = "padding-top: 25px;",
              actionButton(
                ns("find_sample_location_map"),
                label = NULL,
                icon = icon("map-location-dot"),
                title = "Find this sample location on a map"
              )
            )
          ),
          column(
            4,
            selectizeInput(
              ns("edit_sample_sublocation"),
              "Sub-location",
              choices = c(
                "None" = "",
                stats::setNames(
                  as.character(sub_locations$sub_location_id),
                  sub_locations$sub_location_name
                )
              ),
              selected = ifelse(
                is.na(row$sub_location_id[[1]]),
                "",
                as.character(row$sub_location_id[[1]])
              ),
              options = list(
                create = TRUE,
                placeholder = "Optional",
                maxItems = 1
              )
            )
          ),
          column(
            4,
            shinyWidgets::airDatepickerInput(
              ns("edit_sample_datetime"),
              "Sample datetime",
              value = row$datetime[[1]],
              timepicker = TRUE,
              update_on = "change",
              tz = "UTC",
              timepickerOpts = shinyWidgets::timepickerOptions(
                minutesStep = 15,
                timeFormat = "HH:mm"
              )
            )
          )
        ),
        helpText(
          "Changing this location updates this sample only. To reuse a source label mapping, save it deliberately in the Location mappings editor."
        ),
        fluidRow(
          column(
            3,
            selectizeInput(
              ns("edit_sample_media"),
              "Media",
              choices = stats::setNames(media$media_id, media$media_type),
              selected = row$media_id[[1]]
            )
          ),
          column(
            3,
            selectizeInput(
              ns("edit_sample_method"),
              "Collection method",
              choices = stats::setNames(
                collection_methods$collection_method_id,
                collection_methods$collection_method
              ),
              selected = row$collection_method[[1]]
            )
          ),
          column(
            3,
            selectizeInput(
              ns("edit_sample_type"),
              "Sample type",
              choices = stats::setNames(
                sample_types$sample_type_id,
                sample_types$sample_type
              ),
              selected = row$sample_type[[1]]
            )
          ),
          column(
            3,
            tagList(
              selectizeInput(
                ns("edit_sample_group"),
                "Sample group",
                choices = c(
                  "None" = "",
                  stats::setNames(
                    as.character(group_rows$sample_group_id),
                    addDiscData_sample_group_labels(group_rows)
                  )
                ),
                selected = selected_group
              ),
              actionButton(
                ns("create_sample_group"),
                "Create group",
                icon = icon("plus"),
                title = "Create a trip, field event, cooler, shipment, batch, or quality-control group"
              ),
              helpText(
                "Create groups here, then assign each sample to the group it belongs to."
              )
            )
          )
        ),
        fluidRow(
          column(
            3,
            selectizeInput(
              ns("edit_sample_owner"),
              "Owner",
              choices = stats::setNames(
                organizations$organization_id,
                organizations$name
              ),
              selected = row$owner[[1]]
            )
          ),
          column(
            3,
            selectizeInput(
              ns("edit_sample_contributor"),
              "Contributor",
              choices = c(
                "None" = "",
                stats::setNames(
                  as.character(organizations$organization_id),
                  organizations$name
                )
              ),
              selected = ifelse(
                is.na(row$contributor[[1]]),
                "",
                as.character(row$contributor[[1]])
              )
            )
          ),
          column(
            3,
            selectizeInput(
              ns("edit_sample_commissioning_org"),
              "Commissioning organization",
              choices = c(
                "None" = "",
                stats::setNames(
                  as.character(organizations$organization_id),
                  organizations$name
                )
              ),
              selected = ifelse(
                is.na(row$commissioning_org[[1]]),
                "",
                as.character(row$commissioning_org[[1]])
              )
            )
          ),
          column(
            3,
            selectizeInput(
              ns("edit_sample_sampling_org"),
              "Sampling organization",
              choices = c(
                "None" = "",
                stats::setNames(
                  as.character(organizations$organization_id),
                  organizations$name
                )
              ),
              selected = ifelse(
                is.na(row$sampling_org[[1]]),
                "",
                as.character(row$sampling_org[[1]])
              )
            )
          )
        ),
        fluidRow(
          column(
            3,
            shinyWidgets::airDatepickerInput(
              ns("edit_sample_target_datetime"),
              "Target datetime",
              value = row$target_datetime[[1]],
              timepicker = TRUE,
              update_on = "change",
              tz = "UTC"
            )
          ),
          column(
            2,
            numericInput(
              ns("edit_sample_z"),
              "Elevation/depth (m) from ground or reference",
              value = row$z[[1]]
            )
          ),
          column(
            2,
            numericInput(
              ns("edit_sample_volume"),
              "Sample volume (mL)",
              value = row$sample_volume_ml[[1]]
            )
          ),
          column(
            2,
            numericInput(
              ns("edit_sample_purge_volume"),
              "Purge volume (L)",
              value = row$purge_volume_l[[1]]
            )
          ),
          column(
            3,
            numericInput(
              ns("edit_sample_purge_time"),
              "Purge time (min)",
              value = row$purge_time_min[[1]]
            )
          )
        ),
        fluidRow(
          column(
            3,
            numericInput(
              ns("edit_sample_flow_rate"),
              "Flow rate (L/min)",
              value = row$flow_rate_l_min[[1]]
            )
          ),
          column(
            3,
            numericInput(
              ns("edit_sample_wave_height"),
              "Wave height (m)",
              value = row$wave_hgt_m[[1]]
            )
          ),
          column(
            3,
            selectizeInput(
              ns("edit_sample_grade"),
              "Sample grade",
              choices = c(
                stats::setNames(
                  as.character(grade_types$grade_type_id),
                  grade_types$grade_type_description
                )
              ),
              selected = grade_types[
                grade_types$grade_type_description == "Unspecified",
                "grade_type_id"
              ]
            )
          ),
          column(
            3,
            selectizeInput(
              ns("edit_sample_approval"),
              "Sample approval",
              choices = c(
                stats::setNames(
                  as.character(approval_types$approval_type_id),
                  approval_types$approval_type_description
                )
              ),
              selected = approval_types[
                approval_types$approval_type_description == "Not reviewed",
                "approval_type_id"
              ]
            )
          )
        ),
        textAreaInput(
          ns("edit_sample_note"),
          "Sample note",
          value = addDiscData_first(row$sample_note, ""),
          width = "100%"
        ),
        selectizeInput(
          ns("edit_sample_qualifiers"),
          "Sample qualifiers",
          multiple = TRUE,
          choices = stats::setNames(
            sample_qualifiers$qualifier_type_id,
            sample_qualifiers$qualifier_type_description
          ),
          selected = as.character(selected_qualifiers)
        ),
        selectizeInput(
          ns("edit_sample_observers"),
          "Sampler(s)",
          multiple = TRUE,
          choices = observer_choices(shiny::isolate(observers())),
          selected = as.character(selected_observers),
          options = list(
            placeholder = "Select one or more samplers",
            plugins = list("remove_button"),
            dropdownParent = "body"
          )
        ),
        actionButton(
          ns("open_create_observer"),
          "Create new observer"
        ),
        fluidRow(
          column(
            8,
            selectizeInput(
              ns("edit_sample_share_with"),
              "Visible to",
              choices = sample_share_choices,
              selected = selected_share,
              multiple = TRUE,
              options = list(
                placeholder = "All users",
                plugins = list("remove_button"),
                dropdownParent = "body"
              )
            )
          ),
          column(
            4,
            tags$div(
              style = "padding-top: 25px;",
              actionButton(
                ns("apply_sample_share_with"),
                "Apply to all samples",
                title = "Use this sharing selection for every sample in the current upload"
              )
            )
          )
        ),
        helpText(
          "New samples are visible to all users by default. To restrict visibility, remove All users and select access groups, then save this sample. Use Apply to all samples to set the same sharing for the current upload."
        ),
        actionButton(
          ns("save_sample"),
          if (selected) "Update sample" else "Create sample"
        )
      )
    })

    map_location_target <- reactiveVal("default")
    map_location_selected <- reactiveVal(NA_integer_)

    show_location_map <- function(target) {
      map_location_target(target)
      selected <- addDiscData_int(input$edit_sample_location)
      map_location_selected(selected)
      showModal(modalDialog(
        title = "Find a location",
        selectizeInput(
          ns("map_location_search"),
          "Search by location name, code, or alias",
          choices = addDiscData_location_choices(
            locations(),
            include_blank = TRUE
          ),
          selected = if (is.na(selected)) "" else as.character(selected)
        ),
        leaflet::leafletOutput(ns("location_search_map"), height = "520px"),
        textOutput(ns("map_location_selected_label")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("use_map_location"), "Use selected location")
        ),
        size = "l",
        easyClose = TRUE
      ))
    }

    observeEvent(input$find_sample_location_map, show_location_map("sample"))

    output$location_search_map <- leaflet::renderLeaflet({
      current_locations <- locations()
      mapped <- current_locations[
        is.finite(current_locations$latitude) &
          is.finite(current_locations$longitude),
        ,
        drop = FALSE
      ]
      map <- leaflet::leaflet(mapped) |>
        leaflet::addTiles()
      if (!nrow(mapped)) {
        return(map)
      }
      map <- map |>
        leaflet::addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          layerId = ~location_id,
          label = addDiscData_location_labels(mapped),
          radius = 6,
          stroke = TRUE,
          weight = 1,
          fillOpacity = 0.8,
          clusterOptions = leaflet::markerClusterOptions()
        )
      if (nrow(mapped) == 1L) {
        map |>
          leaflet::setView(
            mapped$longitude[[1]],
            mapped$latitude[[1]],
            zoom = 11
          )
      } else {
        map |>
          leaflet::fitBounds(
            min(mapped$longitude),
            min(mapped$latitude),
            max(mapped$longitude),
            max(mapped$latitude)
          )
      }
    })

    observeEvent(
      input$map_location_search,
      {
        location_id <- addDiscData_int(input$map_location_search)
        map_location_selected(location_id)
        current_locations <- locations()
        row <- match(location_id, current_locations$location_id)
        if (
          !is.na(row) &&
            is.finite(current_locations$latitude[[row]]) &&
            is.finite(current_locations$longitude[[row]])
        ) {
          leaflet::leafletProxy("location_search_map", session = session) |>
            leaflet::setView(
              lng = current_locations$longitude[[row]],
              lat = current_locations$latitude[[row]],
              zoom = 12
            )
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(input$location_search_map_marker_click, {
      location_id <- addDiscData_int(input$location_search_map_marker_click$id)
      map_location_selected(location_id)
      updateSelectizeInput(
        session,
        "map_location_search",
        selected = as.character(location_id)
      )
    })

    output$map_location_selected_label <- renderText({
      current_locations <- locations()
      row <- match(map_location_selected(), current_locations$location_id)
      if (is.na(row)) {
        "No location selected."
      } else {
        addDiscData_location_labels(current_locations)[[row]]
      }
    })

    observeEvent(input$use_map_location, {
      location_id <- map_location_selected()
      if (is.na(location_id)) {
        showNotification(
          "Select a location on the map first.",
          type = "warning"
        )
        return()
      }
      if (identical(map_location_target(), "sample")) {
        updateSelectizeInput(
          session,
          "edit_sample_location",
          selected = as.character(location_id)
        )
      } else {
        updateSelectizeInput(
          session,
          "edit_sample_location",
          selected = as.character(location_id)
        )
      }
      removeModal()
    })

    mapping_rows <- reactive({
      df <- data$df
      if (!nrow(df) || !("source_parameter_code" %in% names(df))) {
        return(data.frame())
      }
      df <- df[addDiscData_present(df$source_parameter_code), , drop = FALSE]
      if (!isTRUE(input$show_all_mappings)) {
        df <- df[df$mapping_status != "mapped", , drop = FALSE]
      }
      if (!nrow(df)) {
        return(data.frame())
      }
      key <- paste(
        df$source_code,
        df$source_parameter_code,
        df$source_unit,
        sep = "\r"
      )
      out <- df[!duplicated(key), , drop = FALSE]
      out[order(out$source_parameter_code, out$source_unit), , drop = FALSE]
    })

    output$mapping_summary <- DT::renderDT(
      {
        df <- mapping_rows()
        if (!nrow(df)) {
          return(DT::datatable(
            data.frame(Message = "No parameter mappings need review."),
            rownames = FALSE,
            selection = "none",
            options = list(dom = "t")
          ))
        }
        parameter_index <- match(df$parameter_id, params()$parameter_id)
        fraction_index <- match(
          df$sample_fraction_id,
          sample_fractions$sample_fraction_id
        )
        result_type_index <- match(df$result_type, result_types$result_type_id)
        value_type_index <- match(
          df$result_value_type,
          result_value_types$result_value_type_id
        )
        speciation_index <- match(
          df$result_speciation_id,
          result_speciations$result_speciation_id
        )
        matrix_index <- match(df$matrix_state_id, matrix_states$matrix_state_id)
        source_parameter_label <- ifelse(
          addDiscData_present(df$source_parameter_name) &
            tolower(trimws(df$source_parameter_name)) !=
              tolower(trimws(df$source_parameter_code)),
          paste0(
            df$source_parameter_name,
            " [",
            df$source_parameter_code,
            "]"
          ),
          df$source_parameter_code
        )

        summary <- data.frame(
          `Source parameter` = source_parameter_label,
          `Source unit` = df$source_unit,
          `AquaCache parameter` = params()$param_name[parameter_index],
          `Target unit` = addDiscData_target_unit(
            params(),
            df$parameter_id,
            df$matrix_state_id,
            matrix_states
          ),
          `Sample fraction` = sample_fractions$sample_fraction[fraction_index],
          Conversion = df$conversion,
          Offset = df$result_offset,
          `Result type` = result_types$result_type[result_type_index],
          `Value type` = result_value_types$result_value_type[value_type_index],
          Speciation = result_speciations$result_speciation[speciation_index],
          Matrix = matrix_states$matrix_state_name[matrix_index],
          Status = df$mapping_status,
          check.names = FALSE
        )
        summary[is.na(summary)] <- ""
        addDiscData_style_origin_columns(
          DT::datatable(
            summary,
            rownames = FALSE,
            selection = "single",
            options = list(
              scrollX = TRUE,
              pageLength = 10,
              initComplete = htmlwidgets::JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'font-size': '90%'});",
                "$(this.api().table().body()).css({'font-size': '80%'});",
                "}"
              )
            )
          ),
          source_columns = c("Source parameter", "Source unit"),
          target_columns = c(
            "AquaCache parameter",
            "Target unit",
            "Sample fraction",
            "Conversion",
            "Offset",
            "Result type",
            "Value type",
            "Speciation",
            "Matrix"
          )
        )
      },
      server = FALSE
    )

    selected_mapping_row <- reactive({
      rows <- mapping_rows()
      selected <- input$mapping_summary_rows_selected
      if (!nrow(rows) || length(selected) != 1L || selected > nrow(rows)) {
        return(NULL)
      }
      rows[selected, , drop = FALSE]
    })

    output$mapping_editor <- renderUI({
      row <- selected_mapping_row()
      if (is.null(row)) {
        return(tags$div(
          class = "text-muted",
          "Select a mapping-summary row to edit."
        ))
      }
      selected_value <- function(value, default = "") {
        value <- addDiscData_first(value, default)
        if (!addDiscData_present(value)) default else as.character(value)
      }
      optional_choices <- function(values, labels) {
        c("None" = "", stats::setNames(as.character(values), labels))
      }
      source_name <- as.character(row$source_parameter_name[[1]])
      source_code <- as.character(row$source_parameter_code[[1]])
      if (!addDiscData_present(source_name)) {
        source_name <- source_code
      }
      label <- paste0(
        source_name,
        if (tolower(trimws(source_name)) != tolower(trimws(source_code))) {
          paste0(" [", source_code, "]")
        } else {
          ""
        },
        if (addDiscData_present(row$source_unit[[1]])) {
          paste0(" [", row$source_unit[[1]], "]")
        } else {
          ""
        }
      )

      wellPanel(
        tags$h5(label),
        fluidRow(
          column(
            6,
            selectizeInput(
              ns("mapping_parameter"),
              "AquaCache parameter and target units",
              choices = addDiscData_parameter_choices(params()),
              selected = selected_value(row$parameter_id[[1]]),
              options = list(placeholder = "Select AquaCache parameter")
            )
          ),
          column(
            3,
            selectizeInput(
              ns("mapping_matrix_state"),
              "Matrix state",
              choices = stats::setNames(
                matrix_states$matrix_state_id,
                matrix_states$matrix_state_name
              ),
              selected = selected_value(row$matrix_state_id[[1]], "1")
            )
          ),
          column(
            3,
            selectizeInput(
              ns("mapping_sample_fraction"),
              "Sample fraction",
              choices = optional_choices(
                sample_fractions$sample_fraction_id,
                sample_fractions$sample_fraction
              ),
              selected = selected_value(row$sample_fraction_id[[1]])
            )
          )
        ),
        fluidRow(
          column(
            4,
            selectizeInput(
              ns("mapping_result_type"),
              "Result type",
              choices = stats::setNames(
                result_types$result_type_id,
                result_types$result_type
              ),
              selected = selected_value(row$result_type[[1]], "2")
            )
          ),
          column(
            4,
            selectizeInput(
              ns("mapping_result_value_type"),
              "Result value type",
              choices = stats::setNames(
                result_value_types$result_value_type_id,
                result_value_types$result_value_type
              ),
              selected = selected_value(row$result_value_type[[1]], "1")
            )
          ),
          column(
            4,
            selectizeInput(
              ns("mapping_result_speciation"),
              "Result speciation",
              choices = optional_choices(
                result_speciations$result_speciation_id,
                result_speciations$result_speciation
              ),
              selected = selected_value(row$result_speciation_id[[1]])
            )
          )
        ),
        helpText(
          "A sample fraction or speciation is required when the selected AquaCache parameter requires it."
        ),
        fluidRow(
          column(
            4,
            numericInput(
              ns("mapping_conversion"),
              paste0(
                "Multiply source value [",
                selected_value(row$source_unit[[1]], "unitless"),
                "] by"
              ),
              value = addDiscData_num(row$conversion[[1]], 1)
            )
          ),
          column(
            4,
            numericInput(
              ns("mapping_result_offset"),
              "Then add",
              value = addDiscData_num(row$result_offset[[1]], 0)
            )
          ),
          column(
            4,
            tags$strong(textOutput(ns("mapping_target_unit"), inline = TRUE))
          )
        )
      )
    })

    output$mapping_target_unit <- renderText({
      unit <- addDiscData_target_unit(
        params(),
        addDiscData_int(input$mapping_parameter),
        addDiscData_int(input$mapping_matrix_state, 1L),
        matrix_states
      )
      if (!length(unit) || !addDiscData_present(unit[[1]])) {
        return("AquaCache target unit: not configured")
      }
      paste(
        "AquaCache target unit:",
        unit[[1]],
        ". MAKE SURE THIS MATCHES THE SOURCE UNIT!"
      )
    })

    observeEvent(input$save_parameter_mappings, {
      row <- selected_mapping_row()
      if (is.null(row)) {
        showNotification(
          "Select a mapping-summary row first.",
          type = "warning"
        )
        return()
      }
      request <- tryCatch(
        {
          parameter_id <- addDiscData_int(input$mapping_parameter)
          if (is.na(parameter_id)) {
            stop("Select an AquaCache parameter before saving.")
          }
          sample_fraction_id <- addDiscData_int(
            input$mapping_sample_fraction
          )
          result_speciation_id <- addDiscData_int(
            input$mapping_result_speciation
          )
          validate_parameter_mapping_descriptors(
            parameter_id,
            sample_fraction_id,
            result_speciation_id
          )
          conversion <- addDiscData_num(input$mapping_conversion)
          result_offset <- addDiscData_num(input$mapping_result_offset)
          if (is.na(conversion) || !is.finite(conversion)) {
            stop("Enter a finite conversion multiplier.")
          }
          if (is.na(result_offset) || !is.finite(result_offset)) {
            stop("Enter a finite result offset.")
          }
          list(
            kind = "parameter",
            row = row,
            profile = selected_profile(),
            config = session$userData$config,
            parameter_id = parameter_id,
            result_type = addDiscData_int(input$mapping_result_type, 2L),
            sample_fraction_id = sample_fraction_id,
            result_value_type = addDiscData_int(
              input$mapping_result_value_type,
              1L
            ),
            result_speciation_id = result_speciation_id,
            matrix_state_id = addDiscData_int(input$mapping_matrix_state, 1L),
            conversion = conversion,
            result_offset = result_offset
          )
        },
        error = function(e) e
      )
      if (inherits(request, "error")) {
        showNotification(
          paste("Saving mapping failed:", conditionMessage(request)),
          type = "error"
        )
        return()
      }

      profile_key <- addDiscData_profile_key(
        request$profile$source_code[[1]],
        request$profile$profile_code[[1]]
      )
      path <- data$raw_file_path
      has_current_preview <- length(path) == 1L &&
        !is.na(path) &&
        nzchar(path) &&
        file.exists(path) &&
        !is.null(input$file) &&
        identical(input$file$datapath, path) &&
        identical(data$preview_profile_key, profile_key)
      request$profile_key <- profile_key
      request$refresh_preview <- has_current_preview
      request$path <- if (has_current_preview) path else NULL
      request$locations <- locations()
      request$sample_types <- sample_types
      data$preview_is_stale <- has_current_preview
      invoke_error <- tryCatch(
        {
          parameter_mapping_save_task$invoke(request)
          NULL
        },
        error = function(e) e
      )
      if (inherits(invoke_error, "error")) {
        data$preview_is_stale <- FALSE
        showNotification(
          paste("Could not start saving the mapping:", invoke_error$message),
          type = "error"
        )
      }
    })

    location_mapping_rows <- reactive({
      rows <- data$df
      profile <- tryCatch(selected_profile(), error = function(e) NULL)
      if (!nrow(rows) || is.null(profile) || !nrow(profile)) {
        return(data.frame())
      }
      rows <- rows[
        addDiscData_present(rows$source_location_name) &
          is.na(addDiscData_blank_sample_kind(rows$source_location_name)),
        ,
        drop = FALSE
      ]
      if (!nrow(rows)) {
        return(data.frame())
      }
      key <- tolower(trimws(rows$source_location_name))
      rows <- rows[!duplicated(key), , drop = FALSE]
      rows[order(tolower(rows$source_location_name)), , drop = FALSE]
    })

    output$location_mapping_summary <- DT::renderDT(
      {
        rows <- location_mapping_rows()
        if (!nrow(rows)) {
          return(DT::datatable(
            data.frame(Message = "No source locations are available to map."),
            rownames = FALSE,
            selection = "none",
            options = list(dom = "t")
          ))
        }
        current_locations <- locations()
        loc_ix <- match(rows$location_id, current_locations$location_id)
        summary <- data.frame(
          `Source location name` = rows$source_location_name,
          `Current AquaCache location` = addDiscData_location_labels(
            current_locations
          )[loc_ix],
          Status = rows$location_mapping_status,
          check.names = FALSE
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
          source_columns = c("Source location name"),
          target_columns = c("Current AquaCache location")
        )
      },
      server = FALSE
    )

    selected_location_mapping <- reactive({
      rows <- location_mapping_rows()
      selected <- input$location_mapping_summary_rows_selected
      if (!nrow(rows) || length(selected) != 1L || selected > nrow(rows)) {
        return(NULL)
      }
      rows[selected, , drop = FALSE]
    })

    observeEvent(
      input$location_mapping_summary_rows_selected,
      {
        row <- selected_location_mapping()
        if (is.null(row)) {
          return()
        }
        current_location <- if (
          identical(as.character(row$location_mapping_status[[1]]), "unmapped")
        ) {
          NA_integer_
        } else {
          addDiscData_int(row$location_id[[1]])
        }
        current_sub <- if ("sub_location_id" %in% names(row)) {
          addDiscData_int(row$sub_location_id[[1]])
        } else {
          NA_integer_
        }
        current_locations <- locations()
        updateSelectizeInput(
          session,
          "location_mapping_target",
          choices = addDiscData_location_choices(
            current_locations,
            include_blank = TRUE
          ),
          selected = if (is.na(current_location)) {
            ""
          } else {
            as.character(current_location)
          }
        )
        updateSelectizeInput(
          session,
          "location_mapping_sublocation",
          selected = if (is.na(current_sub)) "" else as.character(current_sub)
        )
      },
      ignoreInit = TRUE
    )

    output$location_mapping_editor <- renderUI({
      row <- selected_location_mapping()
      if (is.null(row)) {
        return(tags$div(
          class = "text-muted",
          "Select a source location above."
        ))
      }
      current_locations <- locations()
      current_location <- if (
        identical(as.character(row$location_mapping_status[[1]]), "unmapped")
      ) {
        NA_integer_
      } else {
        addDiscData_int(row$location_id[[1]])
      }
      current_sub <- if ("sub_location_id" %in% names(row)) {
        addDiscData_int(row$sub_location_id[[1]])
      } else {
        NA_integer_
      }
      target_location <- addDiscData_int(
        input$location_mapping_target,
        current_location
      )
      available_sub <- if (is.na(target_location)) {
        sub_locations[FALSE, , drop = FALSE]
      } else {
        sub_locations[
          sub_locations$location_id == target_location,
          ,
          drop = FALSE
        ]
      }
      selected_sub <- addDiscData_int(
        input$location_mapping_sublocation,
        current_sub
      )
      if (!selected_sub %in% available_sub$sub_location_id) {
        selected_sub <- NA_integer_
      }
      wellPanel(
        tags$strong(row$source_location_name[[1]]),
        fluidRow(
          column(
            6,
            selectizeInput(
              ns("location_mapping_target"),
              "AquaCache location",
              choices = addDiscData_location_choices(
                current_locations,
                include_blank = TRUE
              ),
              selected = if (is.na(target_location)) {
                ""
              } else {
                as.character(target_location)
              },
              options = list(placeholder = "Choose a location", maxItems = 1)
            )
          ),
          column(
            6,
            selectizeInput(
              ns("location_mapping_sublocation"),
              "Sub-location (optional)",
              choices = c(
                "None" = "",
                stats::setNames(
                  as.character(available_sub$sub_location_id),
                  available_sub$sub_location_name
                )
              ),
              selected = if (is.na(selected_sub)) {
                ""
              } else {
                as.character(selected_sub)
              },
              options = list(placeholder = "None", maxItems = 1)
            )
          )
        )
      )
    })

    observeEvent(input$save_location_mapping, {
      row <- selected_location_mapping()
      if (is.null(row)) {
        showNotification("Select a source location first.", type = "warning")
        return()
      }
      request <- tryCatch(
        {
          location_id <- addDiscData_int(input$location_mapping_target)
          if (is.na(location_id)) {
            stop("Choose an AquaCache location.")
          }
          profile <- selected_profile()
          profile_key <- addDiscData_profile_key(
            profile$source_code[[1]],
            profile$profile_code[[1]]
          )
          path <- data$raw_file_path
          has_current_preview <- length(path) == 1L &&
            !is.na(path) &&
            nzchar(path) &&
            file.exists(path) &&
            !is.null(input$file) &&
            identical(input$file$datapath, path) &&
            identical(data$preview_profile_key, profile_key)
          list(
            kind = "location",
            row = row,
            profile = profile,
            config = session$userData$config,
            location_id = location_id,
            sub_location_id = addDiscData_int(
              input$location_mapping_sublocation
            ),
            profile_key = profile_key,
            refresh_preview = has_current_preview,
            path = if (has_current_preview) path else NULL,
            locations = locations(),
            sample_types = sample_types
          )
        },
        error = function(e) e
      )
      if (inherits(request, "error")) {
        showNotification(
          paste("Saving location mapping failed:", conditionMessage(request)),
          type = "error"
        )
        return()
      }
      data$preview_is_stale <- request$refresh_preview
      invoke_error <- tryCatch(
        {
          location_mapping_save_task$invoke(request)
          NULL
        },
        error = function(e) e
      )
      if (inherits(invoke_error, "error")) {
        data$preview_is_stale <- FALSE
        showNotification(
          paste(
            "Could not start saving the location mapping:",
            conditionMessage(invoke_error)
          ),
          type = "error"
        )
      }
    })

    result_flag_mapping_rows <- reactive({
      rows <- data$df
      profile <- tryCatch(selected_profile(), error = function(e) NULL)
      if (!nrow(rows) || is.null(profile) || !nrow(profile)) {
        return(data.frame())
      }
      rows <- rows[addDiscData_present(rows$source_result_flag), , drop = FALSE]
      if (!nrow(rows)) {
        return(data.frame())
      }
      keys <- paste(
        tolower(ifelse(
          is.na(rows$source_result_flag_column),
          "",
          rows$source_result_flag_column
        )),
        tolower(rows$source_result_flag),
        sep = "\r"
      )
      rows[!duplicated(keys), , drop = FALSE]
    })

    output$result_flag_mapping_summary <- DT::renderDT(
      {
        rows <- result_flag_mapping_rows()
        if (!nrow(rows)) {
          return(DT::datatable(
            data.frame(
              Message = "No source result flags are available to map."
            ),
            rownames = FALSE,
            selection = "none",
            options = list(dom = "t")
          ))
        }
        summary <- data.frame(
          `Source flag column` = ifelse(
            addDiscData_present(rows$source_result_flag_column),
            rows$source_result_flag_column,
            "Any column"
          ),
          `Source flag value` = rows$source_result_flag,
          Status = ifelse(
            addDiscData_present(rows$result_flag_action),
            "Mapped",
            "Needs mapping"
          ),
          `Current handling` = unname(c(
            keep_result = "Keep result",
            set_result_null = "Clear numeric result",
            skip_result = "Skip result",
            reject_row = "Reject row",
            note_only = "Add note only"
          )[rows$result_flag_action]),
          check.names = FALSE
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
            ),
          ),
          source_columns = c("Source flag column", "Source flag value"),
          target_columns = c("Current handling")
        )
      },
      server = FALSE
    )

    selected_result_flag_mapping <- reactive({
      rows <- result_flag_mapping_rows()
      selected <- input$result_flag_mapping_summary_rows_selected
      if (!nrow(rows) || length(selected) != 1L || selected > nrow(rows)) {
        return(NULL)
      }
      row <- rows[selected, , drop = FALSE]
      row$mapped_action <- NA_character_
      row$mapped_condition <- NA_integer_
      row$mapped_threshold_source <- NA_character_
      row$mapped_threshold <- NA_real_
      row$mapped_note <- NA_character_
      profile <- selected_profile()
      mappings <- AquaCache::getImportResultFlagMappings(
        con,
        profile$source_code[[1]],
        profile$profile_code[[1]],
        active = TRUE,
        include_draft = TRUE
      )
      if (nrow(mappings)) {
        value_match <- tolower(trimws(mappings$source_flag_value)) ==
          tolower(trimws(row$source_result_flag[[1]]))
        mapped_column <- tolower(trimws(mappings$source_flag_column))
        source_column <- tolower(trimws(row$source_result_flag_column[[1]]))
        column_match <- is.na(mapped_column) |
          !nzchar(mapped_column) |
          mapped_column == source_column
        hit <- which(value_match & column_match)
        if (length(hit)) {
          exact <- !is.na(mapped_column[hit]) &
            nzchar(mapped_column[hit]) &
            mapped_column[hit] == source_column
          profile_specific <- if ("profile_specific" %in% names(mappings)) {
            mappings$profile_specific[hit]
          } else {
            rep(FALSE, length(hit))
          }
          chosen <- hit[order(
            -as.integer(profile_specific),
            -as.integer(exact),
            mappings$priority[hit],
            mappings$import_result_flag_mapping_id[hit]
          )[[1]]]
          row$mapped_action <- mappings$result_action[[chosen]]
          row$mapped_condition <- mappings$result_condition_id[[chosen]]
          row$mapped_threshold_source <- mappings$result_condition_value_source[[
            chosen
          ]]
          row$mapped_threshold <- mappings$result_condition_value_literal[[
            chosen
          ]]
          row$mapped_note <- mappings$note_template[[chosen]]
        }
      }
      row
    })

    output$result_flag_mapping_editor <- renderUI({
      row <- selected_result_flag_mapping()
      if (is.null(row)) {
        return(tags$div(class = "text-muted", "Select a source flag above."))
      }
      wellPanel(
        tags$strong(paste0(
          if (addDiscData_present(row$source_result_flag_column[[1]])) {
            paste0(row$source_result_flag_column[[1]], ": ")
          },
          row$source_result_flag[[1]]
        )),
        fluidRow(
          column(
            4,
            selectizeInput(
              ns("flag_mapping_action"),
              "When this flag appears",
              choices = c(
                "Keep the result" = "keep_result",
                "Keep flag and clear numeric result" = "set_result_null",
                "Skip this result during import" = "skip_result",
                "Reject the result row" = "reject_row",
                "Add a note only" = "note_only"
              ),
              selected = if (addDiscData_present(row$mapped_action[[1]])) {
                row$mapped_action[[1]]
              } else {
                "keep_result"
              }
            )
          ),
          column(
            4,
            selectizeInput(
              ns("flag_mapping_condition"),
              "Result condition (optional)",
              choices = c(
                "None" = "",
                stats::setNames(
                  as.character(result_conditions$result_condition_id),
                  result_conditions$result_condition
                )
              ),
              selected = if (is.na(row$mapped_condition[[1]])) {
                ""
              } else {
                as.character(row$mapped_condition[[1]])
              }
            )
          ),
          column(
            4,
            selectizeInput(
              ns("flag_mapping_threshold_source"),
              "Condition value from",
              choices = c(
                "None" = "none",
                "Result" = "result",
                "Method detection limit" = "method_detection_limit",
                "Reporting detection limit" = "reporting_detection_limit",
                "Enter a value" = "literal"
              ),
              selected = if (
                addDiscData_present(row$mapped_threshold_source[[1]])
              ) {
                row$mapped_threshold_source[[1]]
              } else {
                "none"
              }
            )
          )
        ),
        conditionalPanel(
          condition = "input.flag_mapping_threshold_source == 'literal'",
          ns = ns,
          numericInput(
            ns("flag_mapping_threshold"),
            "Condition value",
            value = row$mapped_threshold[[1]]
          )
        ),
        textInput(
          ns("flag_mapping_note"),
          "Optional note to add",
          value = addDiscData_first(row$mapped_note[[1]], "")
        )
      )
    })

    observeEvent(
      input$open_create_locations,
      {
        updateTabsetPanel(
          session,
          "workflow_tabs",
          selected = "create_locations"
        )
      },
      ignoreInit = TRUE
    )

    unmapped_location_sources <- reactive({
      rows <- data$df
      if (
        !nrow(rows) ||
          !all(c("source_location_name", "location_id") %in% names(rows))
      ) {
        return(character())
      }
      source <- trimws(as.character(rows$source_location_name))
      pending <- is.na(rows$location_id)
      if ("location_mapping_status" %in% names(rows)) {
        pending <- pending |
          (!is.na(rows$location_mapping_status) &
            rows$location_mapping_status == "unmapped")
      }
      source <- source[
        pending &
          !is.na(source) &
          nzchar(source) &
          is.na(addDiscData_blank_sample_kind(source))
      ]
      source <- source[!duplicated(tolower(source))]
      source[order(tolower(source))]
    })

    observe({
      sources <- unmapped_location_sources()
      updateSelectizeInput(
        session,
        "new_location_map_target",
        choices = stats::setNames(sources, sources),
        selected = if (length(sources)) sources[[1]] else character()
      )
    })

    output$new_location_rows <- renderUI({
      sources <- unmapped_location_sources()
      if (!length(sources)) {
        return(tags$p(
          class = "text-muted",
          if (is.null(data$raw_file_path)) {
            "Select a file and preview it to list unmapped source locations."
          } else {
            "There are no unmapped source locations in this preview."
          }
        ))
      }

      input_id <- function(field, row) {
        ns(paste0("new_location_", field, "_", row))
      }
      type_choices <- c(
        "Choose a type" = "",
        stats::setNames(
          as.character(location_types$type_id),
          location_types$type
        )
      )
      network_choices <- stats::setNames(
        as.character(location_networks$network_id),
        location_networks$name
      )
      project_choices <- stats::setNames(
        as.character(location_projects$project_id),
        location_projects$name
      )

      tags$div(
        class = "table-responsive new-location-table",
        tags$table(
          class = "table table-sm table-striped align-middle",
          tags$thead(tags$tr(
            tags$th("Unmapped source location"),
            tags$th("English name *"),
            tags$th(
              "Alias",
              title = "An alternate location name. Import matching can use this value when a source location name differs from the English name.",
              tabindex = "0"
            ),
            tags$th("Location type *"),
            tags$th("Latitude *"),
            tags$th("Longitude *"),
            tags$th("Network(s)"),
            tags$th("Project(s)"),
            tags$th("Visible to"),
            tags$th(
              "Note",
              title = "Optional location-level context, such as access or site details. Use a sample or result note for observations about collected data.",
              tabindex = "0"
            )
          )),
          tags$tbody(lapply(seq_along(sources), function(i) {
            tags$tr(
              tags$th(scope = "row", sources[[i]]),
              tags$td(textInput(
                input_id("name", i),
                NULL,
                value = sources[[i]],
                width = "190px"
              )),
              tags$td(textInput(
                input_id("alias", i),
                NULL,
                value = "",
                width = "140px"
              )),
              tags$td(selectizeInput(
                input_id("type", i),
                NULL,
                choices = type_choices,
                selected = "",
                options = list(dropdownParent = "body"),
                width = "175px"
              )),
              tags$td(numericInput(
                input_id("latitude", i),
                NULL,
                value = NA,
                min = -90,
                max = 90,
                step = 0.000001,
                width = "125px"
              )),
              tags$td(numericInput(
                input_id("longitude", i),
                NULL,
                value = NA,
                min = -180,
                max = 180,
                step = 0.000001,
                width = "125px"
              )),
              tags$td(selectizeInput(
                input_id("network", i),
                NULL,
                choices = network_choices,
                selected = NULL,
                multiple = TRUE,
                options = list(
                  dropdownParent = "body",
                  placeholder = "Optional; select any"
                ),
                width = "180px"
              )),
              tags$td(selectizeInput(
                input_id("project", i),
                NULL,
                choices = project_choices,
                selected = NULL,
                multiple = TRUE,
                options = list(
                  dropdownParent = "body",
                  placeholder = "Optional; select any"
                ),
                width = "180px"
              )),
              tags$td(selectizeInput(
                input_id("share_with", i),
                NULL,
                choices = location_share_choices,
                selected = "public_reader",
                multiple = TRUE,
                options = list(
                  dropdownParent = "body",
                  placeholder = "All users",
                  plugins = list("remove_button")
                ),
                width = "180px"
              )),
              tags$td(textAreaInput(
                input_id("note", i),
                NULL,
                value = "",
                rows = 2,
                width = "190px"
              ))
            )
          }))
        )
      )
    })

    new_location_map_row <- reactiveVal(NA_integer_)
    new_location_map_center <- reactiveVal(
      list(lat = 64, lon = -135, zoom = 4)
    )
    new_location_map_selection <- reactiveVal(NULL)
    new_location_map_zoom <- reactiveVal(NA_real_)

    existing_location_map_data <- function() {
      locs <- locations()
      if (is.null(locs) || nrow(locs) == 0) {
        return(data.frame())
      }
      locs$latitude <- suppressWarnings(as.numeric(locs$latitude))
      locs$longitude <- suppressWarnings(as.numeric(locs$longitude))
      locs <- locs[
        is.finite(locs$latitude) &
          is.finite(locs$longitude) &
          locs$latitude >= -90 &
          locs$latitude <= 90 &
          locs$longitude >= -180 &
          locs$longitude <= 180,
        ,
        drop = FALSE
      ]
      if (!nrow(locs)) {
        return(locs)
      }

      clean_text <- function(x) {
        x <- as.character(x)
        x[is.na(x)] <- ""
        x
      }
      name <- clean_text(locs$name)
      code <- clean_text(locs$location_code)
      alias <- clean_text(locs$alias)
      locs$map_label <- sprintf("%s (%s)", name, code)
      locs$map_popup <- sprintf(
        "<strong>%s</strong><br/>Code: %s<br/>Alias: %s",
        htmltools::htmlEscape(name),
        htmltools::htmlEscape(code),
        htmltools::htmlEscape(alias)
      )
      locs
    }

    output$new_location_picker_map <- leaflet::renderLeaflet({
      center <- new_location_map_center()
      selection <- isolate(new_location_map_selection())
      existing_locs <- existing_location_map_data()
      map <- leaflet::leaflet(
        options = leaflet::leafletOptions(maxZoom = 19)
      ) %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery,
          group = "Satellite"
        ) %>%
        leaflet::addLayersControl(
          baseGroups = c("Esri.WorldTopoMap", "Satellite"),
          overlayGroups = "Existing locations",
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) %>%
        leaflet::addScaleBar(
          options = leaflet::scaleBarOptions(imperial = FALSE)
        ) %>%
        leaflet::setView(
          lng = center$lon,
          lat = center$lat,
          zoom = center$zoom
        )

      if (nrow(existing_locs)) {
        map <- map %>%
          leaflet::addCircleMarkers(
            data = existing_locs,
            lng = ~longitude,
            lat = ~latitude,
            radius = 5,
            color = "#DC4405",
            weight = 2,
            opacity = 0.9,
            fillColor = "#F2A900",
            fillOpacity = 0.65,
            group = "Existing locations",
            label = ~map_label,
            popup = ~map_popup,
            layerId = ~ paste0("existing_location_", location_id),
            labelOptions = leaflet::labelOptions(direction = "auto")
          )
      }
      if (!is.null(selection)) {
        map <- map %>%
          leaflet::addCircleMarkers(
            lng = selection$lon,
            lat = selection$lat,
            radius = 6,
            color = "#007B8A",
            fillOpacity = 0.9,
            group = "selected_point"
          )
      }
      map %>%
        leaflet::addLegend(
          position = "bottomright",
          colors = c("#DC4405", "#007B8A"),
          labels = c("Existing locations", "Selected location"),
          opacity = 1
        )
    }) %>%
      bindEvent(input$open_new_location_map)

    draw_new_location_selected_point <- function() {
      selection <- isolate(new_location_map_selection())
      if (is.null(selection)) {
        return(invisible(NULL))
      }
      leaflet::leafletProxy(
        ns("new_location_picker_map"),
        session = session
      ) %>%
        leaflet::clearGroup("selected_point") %>%
        leaflet::addCircleMarkers(
          lng = selection$lon,
          lat = selection$lat,
          radius = 6,
          color = "#007B8A",
          fillOpacity = 0.9,
          group = "selected_point"
        )
    }

    output$new_location_map_zoom_note <- renderUI({
      zoom <- new_location_map_zoom()
      if (is.null(zoom) || !is.finite(zoom) || zoom < 14) {
        return(div(
          style = "color: #b42318; font-size: 14px; margin-top: 8px;",
          "Zoom in to level 14 or higher before using the selected coordinates."
        ))
      }
      div(
        style = "color: #027a48; font-size: 14px; margin-top: 8px;",
        "Zoom level is sufficient. Select a point, then choose Use selected location."
      )
    })

    output$new_location_map_selection_note <- renderUI({
      selection <- new_location_map_selection()
      if (is.null(selection)) {
        return(tags$p("Click the map to place the selected-location marker."))
      }
      tags$p(sprintf(
        "Selected coordinates: %.8f, %.8f",
        selection$lat,
        selection$lon
      ))
    })

    observeEvent(
      input$open_new_location_map,
      {
        sources <- unmapped_location_sources()
        source <- as.character(input$new_location_map_target)
        row <- match(source, sources)
        if (is.na(row)) {
          showNotification(
            "Choose an unmapped source location before opening the map.",
            type = "warning"
          )
          return()
        }

        latitude <- addDiscData_num(
          input[[paste0("new_location_latitude_", row)]]
        )
        longitude <- addDiscData_num(
          input[[paste0("new_location_longitude_", row)]]
        )
        has_coordinates <- is.finite(latitude) && is.finite(longitude)
        center <- if (has_coordinates) {
          list(lat = latitude, lon = longitude, zoom = 12)
        } else {
          list(lat = 64, lon = -135, zoom = 4)
        }
        new_location_map_row(row)
        new_location_map_center(center)
        new_location_map_zoom(center$zoom)
        new_location_map_selection(
          if (has_coordinates) {
            list(lat = latitude, lon = longitude)
          } else {
            NULL
          }
        )
        showModal(modalDialog(
          title = paste("Choose coordinates for", source),
          tags$p(
            "Click to place or move the selected-location marker. Existing AquaCache locations appear in orange."
          ),
          leaflet::leafletOutput(
            ns("new_location_picker_map"),
            height = "60vh"
          ),
          uiOutput(ns("new_location_map_zoom_note")),
          uiOutput(ns("new_location_map_selection_note")),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              ns("new_location_use_selected"),
              "Use selected location"
            )
          ),
          size = "l"
        ))
        shinyjs::disable("new_location_use_selected")
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$new_location_picker_map_click,
      {
        click <- input$new_location_picker_map_click
        if (
          is.null(click$lat) ||
            is.null(click$lng) ||
            !is.finite(click$lat) ||
            !is.finite(click$lng)
        ) {
          return()
        }
        new_location_map_selection(list(lat = click$lat, lon = click$lng))
        draw_new_location_selected_point()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$new_location_picker_map_zoom,
      {
        zoom <- suppressWarnings(as.numeric(input$new_location_picker_map_zoom))
        if (!length(zoom) || !is.finite(zoom[[1]])) {
          return()
        }
        new_location_map_zoom(zoom[[1]])
        if (zoom[[1]] < 14) {
          shinyjs::disable("new_location_use_selected")
        } else {
          shinyjs::enable("new_location_use_selected")
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$new_location_use_selected,
      {
        zoom <- new_location_map_zoom()
        if (is.null(zoom) || !is.finite(zoom) || zoom < 14) {
          showNotification(
            "Zoom in to level 14 or higher before using the selected coordinates.",
            type = "warning"
          )
          return()
        }
        selection <- new_location_map_selection()
        if (is.null(selection)) {
          showNotification("Click a point on the map to select coordinates.")
          return()
        }
        row <- new_location_map_row()
        if (is.na(row)) {
          showNotification(
            "Choose an unmapped source location before opening the map.",
            type = "warning"
          )
          return()
        }
        updateNumericInput(
          session,
          paste0("new_location_latitude_", row),
          value = selection$lat
        )
        updateNumericInput(
          session,
          paste0("new_location_longitude_", row),
          value = selection$lon
        )
        removeModal()
        showNotification(
          sprintf(
            "Coordinates set to %.8f, %.8f.",
            selection$lat,
            selection$lon
          ),
          type = "message"
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$create_new_locations,
      {
        sources <- unmapped_location_sources()
        if (!length(sources)) {
          showNotification(
            "There are no unmapped source locations to create.",
            type = "warning"
          )
          return()
        }
        if (is.null(data$raw_file_path) || isTRUE(data$preview_is_stale)) {
          showNotification(
            "Preview the current file again before creating locations.",
            type = "warning"
          )
          return()
        }
        read_text <- function(field, row, default = "") {
          value <- input[[paste0("new_location_", field, "_", row)]]
          if (is.null(value) || !length(value) || is.na(value[[1]])) {
            return(default)
          }
          trimws(as.character(value[[1]]))
        }
        read_ids <- function(field, row) {
          value <- input[[paste0("new_location_", field, "_", row)]]
          if (is.null(value) || !length(value)) {
            return(integer())
          }
          value <- trimws(as.character(value))
          value <- value[!is.na(value) & nzchar(value)]
          ids <- suppressWarnings(as.integer(value))
          if (anyNA(ids)) {
            stop("A selected network or project has an invalid ID.")
          }
          unique(ids)
        }
        read_share <- function(row) {
          addDiscData_share_selection(
            input[[paste0("new_location_share_with_", row)]],
            location_share_choices
          )
        }
        names <- vapply(
          seq_along(sources),
          function(i) {
            read_text("name", i)
          },
          character(1)
        )
        aliases <- vapply(
          seq_along(sources),
          function(i) {
            value <- read_text("alias", i)
            if (nzchar(value)) value else NA_character_
          },
          character(1)
        )
        types <- vapply(
          seq_along(sources),
          function(i) {
            addDiscData_int(read_text("type", i))
          },
          integer(1)
        )
        latitude <- vapply(
          seq_along(sources),
          function(i) {
            addDiscData_num(input[[paste0("new_location_latitude_", i)]])
          },
          numeric(1)
        )
        longitude <- vapply(
          seq_along(sources),
          function(i) {
            addDiscData_num(input[[paste0("new_location_longitude_", i)]])
          },
          numeric(1)
        )
        networks <- lapply(
          seq_along(sources),
          function(i) {
            read_ids("network", i)
          }
        )
        projects <- lapply(
          seq_along(sources),
          function(i) {
            read_ids("project", i)
          }
        )
        shares <- tryCatch(
          lapply(seq_along(sources), read_share),
          error = function(e) e
        )
        if (inherits(shares, "error")) {
          showNotification(
            paste("Invalid location sharing selection:", shares$message),
            type = "error"
          )
          return()
        }
        primary_networks <- vapply(
          networks,
          function(ids) if (length(ids)) ids[[1]] else NA_integer_,
          integer(1)
        )
        primary_projects <- vapply(
          projects,
          function(ids) if (length(ids)) ids[[1]] else NA_integer_,
          integer(1)
        )
        notes <- vapply(
          seq_along(sources),
          function(i) {
            value <- read_text("note", i)
            if (nzchar(value)) value else NA_character_
          },
          character(1)
        )

        if (any(!nzchar(names))) {
          showNotification(
            "Enter an English name for every new location.",
            type = "error"
          )
          return()
        }
        if (anyNA(types)) {
          showNotification(
            "Choose a location type for every new location.",
            type = "error"
          )
          return()
        }
        if (
          any(!is.finite(latitude)) ||
            any(!is.finite(longitude)) ||
            any(latitude < -90 | latitude > 90) ||
            any(longitude < -180 | longitude > 180)
        ) {
          showNotification(
            "Set a valid latitude and longitude for every new location. Use the map picker if helpful.",
            type = "error"
          )
          return()
        }
        if (anyDuplicated(tolower(names))) {
          showNotification(
            "Each new location must have a unique English name.",
            type = "error"
          )
          return()
        }
        coordinates <- paste(
          format(latitude, digits = 12),
          format(longitude, digits = 12)
        )
        if (anyDuplicated(coordinates)) {
          showNotification(
            "Each new location must have unique coordinates.",
            type = "error"
          )
          return()
        }

        profile <- selected_profile()
        if (is.null(profile) || !nrow(profile)) {
          showNotification(
            "Choose a workbook format before creating locations.",
            type = "error"
          )
          return()
        }
        location_rows <- data.frame(
          name = names,
          alias = aliases,
          location_code = rep(NA_character_, length(sources)),
          latitude = latitude,
          longitude = longitude,
          share_with = vapply(
            shares,
            function(groups) paste(groups, collapse = ","),
            character(1)
          ),
          location_type = types,
          note = notes,
          contact = rep(NA_character_, length(sources)),
          network = primary_networks,
          project = primary_projects,
          stringsAsFactors = FALSE
        )

        request <- list(
          config = session$userData$config,
          location_rows = location_rows,
          sources = sources,
          names = names,
          networks = networks,
          projects = projects,
          profile = profile
        )
        invoke_error <- tryCatch(
          {
            location_create_task$invoke(request)
            NULL
          },
          error = function(e) e
        )
        if (inherits(invoke_error, "error")) {
          showNotification(
            paste("Could not start creating locations:", invoke_error$message),
            type = "error"
          )
        }
      },
      ignoreInit = TRUE
    )
    observeEvent(input$save_result_flag_mapping, {
      row <- selected_result_flag_mapping()
      if (is.null(row)) {
        showNotification("Select a source result flag first.", type = "warning")
        return()
      }
      tryCatch(
        {
          threshold_source <- input$flag_mapping_threshold_source
          threshold <- if (identical(threshold_source, "literal")) {
            addDiscData_num(input$flag_mapping_threshold)
          } else {
            NA_real_
          }
          if (
            identical(threshold_source, "literal") &&
              (is.na(threshold) || !is.finite(threshold))
          ) {
            stop("Enter a finite condition value.")
          }
          profile <- selected_profile()
          AquaCache::upsertImportResultFlagMappings(
            con = con,
            source_code = profile$source_code[[1]],
            source_name = profile$source_code[[1]],
            profile_code = profile$profile_code[[1]],
            mappings = data.frame(
              source_flag_column = if (
                addDiscData_present(row$source_result_flag_column[[1]])
              ) {
                row$source_result_flag_column[[1]]
              } else {
                NA_character_
              },
              source_flag_value = row$source_result_flag[[1]],
              result_condition_id = addDiscData_int(
                input$flag_mapping_condition
              ),
              result_condition_value_source = threshold_source,
              result_condition_value_literal = threshold,
              result_action = input$flag_mapping_action,
              note_template = if (
                addDiscData_present(input$flag_mapping_note)
              ) {
                input$flag_mapping_note
              } else {
                NA_character_
              },
              priority = 50L,
              active = TRUE,
              note = "Saved from YGwater add discrete data result-flag mapping editor."
            ),
            publish = FALSE
          )
          mapping_revision(mapping_revision() + 1L)
          preview_error <- tryCatch(
            {
              refreshed <- refresh_current_preview(
                profile,
                preserve_edits = TRUE
              )
              if (isTRUE(refreshed)) {
                NULL
              } else {
                "The earlier upload is no longer available. Select it again and preview it."
              }
            },
            error = function(e) e$message
          )
          showNotification(
            if (is.null(preview_error)) {
              "Saved result-flag mapping and refreshed the preview while retaining your result edits."
            } else {
              paste(
                "Result-flag mapping saved, but the current preview could not be refreshed:",
                preview_error
              )
            },
            type = if (is.null(preview_error)) "message" else "warning"
          )
        },
        error = function(e) {
          showNotification(
            paste("Saving result-flag mapping failed:", e$message),
            type = "error"
          )
        }
      )
    })

    selected_results <- reactive({
      row <- selected_sample_row()
      if (is.null(row)) {
        return(addDiscData_empty_table())
      }
      data$df[
        data$df$sample_key == row$sample_key[[1]] &
          !is.na(data$df$parameter_id),
        ,
        drop = FALSE
      ]
    })

    result_display <- reactive({
      addDiscData_result_display(
        rows = selected_results(),
        locations = locations(),
        sub_locations = sub_locations,
        parameters = params(),
        result_types = result_types,
        result_conditions = result_conditions,
        sample_fractions = sample_fractions,
        result_value_types = result_value_types,
        result_speciations = result_speciations,
        matrix_states = matrix_states,
        laboratories = laboratories,
        media = media,
        collection_methods = collection_methods,
        sample_types = sample_types,
        protocols_methods = protocols_methods,
        grade_types = grade_types,
        approval_types = approval_types
      )
    })

    output$data_table <- DT::renderDT(
      {
        display <- result_display()
        if (!nrow(display)) {
          return(DT::datatable(
            data.frame(
              Message = "Select a sample to review its results."
            ),
            rownames = FALSE,
            selection = "none",
            options = list(dom = "t")
          ))
        }
        editable_columns <- match(
          names(addDiscData_result_edit_columns()),
          names(display)
        ) -
          1L
        disabled_columns <- setdiff(
          seq_len(ncol(display)) - 1L,
          editable_columns
        )
        addDiscData_style_origin_columns(
          DT::datatable(
            display,
            editable = list(
              target = "cell",
              disable = list(columns = disabled_columns)
            ),
            selection = "single",
            rownames = FALSE,
            options = list(
              scrollX = TRUE,
              pageLength = 10,
              initComplete = htmlwidgets::JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'font-size': '90%'});",
                "$(this.api().table().body()).css({'font-size': '80%'});",
                "}"
              )
            )
          ),
          source_columns = c(
            "Source sample",
            "Source location",
            "Source parameter",
            "Source unit",
            "Source result",
            "Source result flag"
          ),
          target_columns = c(
            "AquaCache location",
            "Sub-location",
            "Parameter",
            "Result value",
            "Result condition",
            "Condition value",
            "Target unit",
            "Sample fraction",
            "Result type",
            "Value type",
            "Speciation",
            "Matrix",
            "Laboratory",
            "Protocol",
            "Grade",
            "Approval",
            "Media",
            "Collection method",
            "Sample type",
            "Sample datetime (UTC)",
            "Analysis datetime",
            "Lab report",
            "Lab sample",
            "Note"
          )
        )
      },
      server = FALSE
    )

    observeEvent(input$data_table_cell_edit, {
      info <- input$data_table_cell_edit
      display <- result_display()
      display_column <- as.integer(info$col) + 1L
      if (display_column < 1L || display_column > ncol(display)) {
        return()
      }
      source_column <- unname(
        addDiscData_result_edit_columns()[names(display)[[display_column]]]
      )
      if (
        !length(source_column) ||
          is.na(source_column) ||
          info$row > nrow(selected_results())
      ) {
        return()
      }
      selected_row <- selected_sample_row()
      if (is.null(selected_row)) {
        return()
      }
      target_rows <- which(
        data$df$sample_key == selected_row$sample_key[[1]] &
          !is.na(data$df$parameter_id)
      )
      target_row <- target_rows[[info$row]]
      value <- trimws(as.character(info$value))
      if (identical(source_column, "result_condition")) {
        if (!nzchar(value)) {
          new_value <- NA_integer_
        } else {
          hit <- which(
            tolower(result_conditions$result_condition) == tolower(value)
          )
          if (length(hit) != 1L) {
            showNotification(
              "Enter a result-condition name exactly as shown in the lookup table.",
              type = "error"
            )
            data$df <- data$df
            return()
          }
          new_value <- result_conditions$result_condition_id[[hit[[1]]]]
        }
      } else if (source_column %in% c("result", "result_condition_value")) {
        new_value <- if (nzchar(value)) {
          suppressWarnings(as.numeric(value))
        } else {
          NA_real_
        }
        if (nzchar(value) && is.na(new_value)) {
          showNotification(
            "Enter a numeric value or leave the cell blank.",
            type = "error"
          )
          data$df <- data$df
          return()
        }
      } else if (
        source_column %in%
          c(
            "result_type",
            "sample_fraction_id",
            "result_value_type",
            "result_speciation_id",
            "matrix_state_id",
            "protocol_method",
            "laboratory",
            "grade_type_id",
            "approval_type_id"
          )
      ) {
        lookup <- switch(
          source_column,
          result_type = result_types,
          sample_fraction_id = sample_fractions,
          result_value_type = result_value_types,
          result_speciation_id = result_speciations,
          matrix_state_id = matrix_states,
          protocol_method = protocols_methods,
          laboratory = laboratories,
          grade_type_id = grade_types,
          approval_type_id = approval_types
        )
        id_col <- switch(
          source_column,
          result_type = "result_type_id",
          sample_fraction_id = "sample_fraction_id",
          result_value_type = "result_value_type_id",
          result_speciation_id = "result_speciation_id",
          matrix_state_id = "matrix_state_id",
          protocol_method = "protocol_id",
          laboratory = "lab_id",
          grade_type_id = "grade_type_id",
          approval_type_id = "approval_type_id"
        )
        label_col <- switch(
          source_column,
          result_type = "result_type",
          sample_fraction_id = "sample_fraction",
          result_value_type = "result_value_type",
          result_speciation_id = "result_speciation",
          matrix_state_id = "matrix_state_name",
          protocol_method = "protocol_name",
          laboratory = "lab_name",
          grade_type_id = "grade_type_description",
          approval_type_id = "approval_type_description"
        )
        if (!nzchar(value)) {
          new_value <- NA_integer_
        } else {
          hit <- which(
            tolower(as.character(lookup[[label_col]])) == tolower(value)
          )
          if (length(hit) != 1L) {
            showNotification(
              "Enter a value exactly as shown in the database lookup.",
              type = "error"
            )
            return()
          }
          new_value <- as.integer(lookup[[id_col]][[hit[[1]]]])
        }
      } else if (identical(source_column, "analysis_datetime")) {
        new_value <- if (nzchar(value)) {
          as.POSIXct(value, tz = "UTC")
        } else {
          as.POSIXct(NA)
        }
        if (nzchar(value) && is.na(new_value)) {
          showNotification(
            "Enter analysis datetime as YYYY-MM-DD HH:MM:SS.",
            type = "error"
          )
          return()
        }
      } else {
        new_value <- value
      }
      if (
        identical(source_column, "sample_fraction_id") &&
          is.na(new_value) &&
          parameter_requirement(
            data$df$parameter_id[[target_row]],
            "sample_fraction"
          )
      ) {
        showNotification(
          "Sample fraction is required for this parameter.",
          type = "error"
        )
        return()
      }
      if (
        identical(source_column, "result_speciation_id") &&
          is.na(new_value) &&
          parameter_requirement(
            data$df$parameter_id[[target_row]],
            "result_speciation"
          )
      ) {
        showNotification(
          "Speciation is required for this parameter.",
          type = "error"
        )
        return()
      }
      data$df[[source_column]][[target_row]] <- new_value
    })

    validate_upload_rows <- function(df) {
      if (!nrow(df)) {
        stop("Empty data table.", call. = FALSE)
      }
      rejected <- which(df$result_flag_action == "reject_row")
      if (length(rejected)) {
        stop(
          "A source result flag rejects row(s): ",
          paste(rejected, collapse = ", "),
          ". Correct the source data or its result-flag mapping before upload.",
          call. = FALSE
        )
      }
      required <- list(
        datetime = "sample datetime",
        media_id = "media",
        collection_method = "collection method",
        sample_type = "sample type",
        owner = "owner",
        parameter_id = "parameter",
        result_type = "result type",
        matrix_state_id = "matrix state"
      )
      for (nm in names(required)) {
        missing <- is.na(df[[nm]]) | !addDiscData_present(df[[nm]])
        if (any(missing)) {
          stop(
            "Missing ",
            required[[nm]],
            " in row(s): ",
            paste(which(missing), collapse = ", "),
            call. = FALSE
          )
        }
      }
      type_index <- match(df$sample_type, sample_types$sample_type_id)
      if (anyNA(type_index)) {
        stop(
          "Unknown sample type in row(s): ",
          paste(which(is.na(type_index)), collapse = ", "),
          call. = FALSE
        )
      }
      missing_location <- is.na(df$location_id)
      requires_location <- sample_types$requires_location[type_index]
      if (any(missing_location & requires_location)) {
        stop(
          "The selected sample type requires a location in row(s): ",
          paste(which(missing_location & requires_location), collapse = ", "),
          call. = FALSE
        )
      }
      is_blank_sample <- tolower(trimws(sample_types$sample_type[
        type_index
      ])) %in%
        c("qc-sample-field blank", "qc-sample-trip blank")
      if (any(is_blank_sample & !missing_location)) {
        stop(
          "Field and trip blank samples must not be assigned to a location. Remove the location in Sample details and assign a trip or QC group.",
          call. = FALSE
        )
      }
      if (any(missing_location & !is.na(df$sub_location_id))) {
        stop(
          "A sub-location cannot be supplied without a location in row(s): ",
          paste(
            which(missing_location & !is.na(df$sub_location_id)),
            collapse = ", "
          ),
          call. = FALSE
        )
      }
      requires_group <- missing_location |
        sample_types$requires_sample_group[type_index]
      if (any(requires_group & is.na(df$sample_group_id))) {
        stop(
          "Every locationless sample and any sample type that requires a group must be assigned to a sample group. For field or trip blanks, select or create the related trip or QC group in Sample details.",
          call. = FALSE
        )
      }
      invalid_group <- !is.na(df$sample_group_id) &
        !(df$sample_group_id %in% sample_groups()$sample_group_id)
      if (any(invalid_group)) {
        stop("A sample has an unknown sample group.", call. = FALSE)
      }
      if (
        any(missing_location) &&
          any(
            !addDiscData_present(df$source_code[missing_location]) |
              !addDiscData_present(df$source_sample_id[missing_location])
          )
      ) {
        stop(
          "Locationless samples require both an import source and source sample ID.",
          call. = FALSE
        )
      }
      no_result <- is.na(df$result) & is.na(df$result_condition)
      if (any(no_result)) {
        stop(
          "Missing result or result condition in row(s): ",
          paste(which(no_result), collapse = ", "),
          call. = FALSE
        )
      }
      needs_fraction <- vapply(
        df$parameter_id,
        parameter_requirement,
        logical(1),
        requirement = "sample_fraction"
      )
      missing_fraction <- needs_fraction & is.na(df$sample_fraction_id)
      if (any(missing_fraction)) {
        stop(
          "Sample fraction is required for the parameter in result row(s): ",
          paste(which(missing_fraction), collapse = ", "),
          call. = FALSE
        )
      }
      needs_speciation <- vapply(
        df$parameter_id,
        parameter_requirement,
        logical(1),
        requirement = "result_speciation"
      )
      missing_speciation <- needs_speciation & is.na(df$result_speciation_id)
      if (any(missing_speciation)) {
        stop(
          "Speciation is required for the parameter in result row(s): ",
          paste(which(missing_speciation), collapse = ", "),
          call. = FALSE
        )
      }
      invisible(TRUE)
    }

    observeEvent(input$upload, {
      request <- tryCatch(
        {
          df <- data$df
          manual_sample_shells <- df$source_code == "YGwater-manual" &
            is.na(df$parameter_id)
          df <- df[!manual_sample_shells, , drop = FALSE]
          skipped <- which(df$result_flag_action == "skip_result")
          if (length(skipped)) {
            df <- df[-skipped, , drop = FALSE]
          }
          if (
            identical(input$entry_mode, "file") &&
              isTRUE(data$preview_is_stale)
          ) {
            stop(
              "The selected file or workbook format changed after this preview. Preview the file again before uploading.",
              call. = FALSE
            )
          }
          validate_upload_rows(df)
          has_groups <- any(!is.na(df$sample_group_id))
          if (has_groups && !isTRUE(check_groups$can_assign_group[[1]])) {
            stop(
              "You do not have permission to assign samples to groups.",
              call. = FALSE
            )
          }
          observer_state <- sample_observer_map()
          sample_keys <- unique(as.character(df$sample_key))
          has_observers <- any(vapply(
            sample_keys,
            function(key) length(observer_state[[key]]) > 0L,
            logical(1)
          ))
          if (
            has_observers &&
              !isTRUE(observer_permissions$can_assign_observers[[1]])
          ) {
            stop(
              "Your database role cannot assign sample observers.",
              call. = FALSE
            )
          }
          sample_share_state <- sample_share_map()
          sample_keys <- unique(as.character(df$sample_key))
          sample_keys <- sample_keys[!is.na(sample_keys) & nzchar(sample_keys)]
          sample_share_with <- lapply(sample_keys, function(key) {
            addDiscData_share_selection(
              sample_share_state[[key]],
              sample_share_choices
            )
          })
          names(sample_share_with) <- sample_keys
          file <- if (is.null(input$file)) {
            NULL
          } else {
            list(
              name = input$file$name[[1]],
              datapath = input$file$datapath[[1]],
              size = input$file$size[[1]]
            )
          }
          attachments <- if (is.null(input$attach_docs)) {
            list()
          } else {
            lapply(seq_len(nrow(input$attach_docs)), function(i) {
              list(
                name = input$attach_docs$name[[i]],
                datapath = input$attach_docs$datapath[[i]]
              )
            })
          }
          profile <- if (
            any(df$source_code != "YGwater-manual", na.rm = TRUE)
          ) {
            selected_profile()
          } else {
            NULL
          }
          list(
            df = df,
            config = session$userData$config,
            file = file,
            attachments = attachments,
            profile = profile,
            field_visit_id = addDiscData_int(input$field_visit_id),
            sample_qualifiers = sample_qualifier_map(),
            sample_observers = sample_observer_map(),
            sample_share_with = sample_share_with,
            locations = locations(),
            sub_locations = sub_locations
          )
        },
        error = function(e) e
      )
      if (inherits(request, "error")) {
        showNotification(
          paste("Upload could not start:", conditionMessage(request)),
          type = "error"
        )
        return()
      }

      invoke_error <- tryCatch(
        {
          upload_task$invoke(request)
          NULL
        },
        error = function(e) e
      )
      if (inherits(invoke_error, "error")) {
        showNotification(
          paste("Upload could not start:", conditionMessage(invoke_error)),
          type = "error"
        )
      }
    })

    observeEvent(upload_task$result(), {
      result <- upload_task$result()
      if (!isTRUE(result$ok)) {
        showNotification(
          paste("Upload failed:", result$message),
          type = "error"
        )
        return()
      }

      summary <- result$summary
      names(summary) <- c(
        "New sample ID",
        "Source",
        "Source sample ID",
        "Sample date/time (UTC)",
        "AquaCache location",
        "AquaCache sub-location"
      )
      upload_summary(summary)
      data$df <- addDiscData_empty_table()
      data$preview_base <- addDiscData_empty_table()
      data$raw_file_path <- NULL
      data$preview_profile_key <- NULL
      data$preview_is_stale <- FALSE
      sample_qualifier_map(list())
      sample_observer_map(list())
      sample_share_map(list())
      selected_sample_key(NULL)
      showModal(
        modalDialog(
          title = "Upload complete",
          size = "l",
          easyClose = TRUE,
          tags$p(
            sprintf(
              "Added %s sample(s) and %s result(s). New sample IDs and destination locations are listed below.",
              result$inserted_samples,
              result$inserted_results
            )
          ),
          fluidRow(
            column(
              6,
              downloadButton(ns("download_upload_summary_txt"), "Download .txt")
            ),
            column(
              6,
              downloadButton(
                ns("download_upload_summary_xlsx"),
                "Download .xlsx"
              )
            )
          ),
          DT::DTOutput(ns("upload_summary_table")),
          footer = modalButton("Close")
        )
      )
    })

    output$upload_summary_table <- DT::renderDT({
      DT::datatable(
        upload_summary(),
        rownames = FALSE,
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
      )
    })

    output$download_upload_summary_txt <- downloadHandler(
      filename = function() {
        paste0("discrete-upload-summary-", Sys.Date(), ".txt")
      },
      content = function(file) {
        write.table(
          upload_summary(),
          file = file,
          sep = "\t",
          row.names = FALSE,
          quote = TRUE,
          na = ""
        )
      }
    )

    output$download_upload_summary_xlsx <- downloadHandler(
      filename = function() {
        paste0("discrete-upload-summary-", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        openxlsx::write.xlsx(
          upload_summary(),
          file = file,
          asTable = TRUE,
          overwrite = TRUE
        )
      }
    )

    return(outputs)
  })
}
