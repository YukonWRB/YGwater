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
    ifelse(nzchar(group_name), paste(group_code, group_name, sep = " — "), group_code),
    group_name
  )
  paste(sample_groups$group_type, identifier, sep = ": ")
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
  paste(source_code, profile_code, sep = "\r")
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
    offset_minutes <- offset_sign * (
      as.integer(offset_parts[[3]]) * 60L +
        as.integer(offset_parts[[4]])
    )
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
    if (!nzchar(source_names[[i]]) || !is.na(rows$location_id[[i]])) {
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
    rows$location_id[is.na(rows$location_id)] <- fallback
    rows$location_mapping_status[
      rows$location_id == fallback & rows$location_mapping_status == "unmapped"
    ] <- "manual default"
  }
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
    `Source parameter` = rows$source_parameter_code,
    `Source unit` = rows$source_unit,
    `Source result` = source_result,
    `Source result flag` = rows$source_result_flag,
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

addDiscDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    page_fluid(
      uiOutput(ns("banner")),
      accordion(
        id = ns("accordion1"),
        open = "data_panel",
        accordion_panel(
          id = ns("data_panel"),
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
                    ns("new_import_profile"),
                    "Set up workbook format"
                  )
                )
              )
            ),
            uiOutput(ns("import_profile_status")),
            actionButton(ns("preview_file"), "Preview file"),
            conditionalPanel(
              condition = "input.entry_mode == 'file' && input.preview_file > 0",
              ns = ns,
              tags$h5("Review new names and values"),
              helpText(
                "After previewing, select any item that needs a mapping, choose what it means in AquaCache, then save it. Each saved mapping is kept for this workbook format and applied to future previews. Saved mappings are published automatically when an upload succeeds."
              ),
              checkboxInput(
                ns("show_all_mappings"),
                "Show already mapped parameters",
                value = FALSE
              ),
              DT::DTOutput(ns("mapping_summary")),
              uiOutput(ns("mapping_editor")),
              actionButton(
                ns("save_parameter_mappings"),
                "Save selected parameter mapping"
              ),
              tags$hr(),
              tags$h5("Map source locations"),
              helpText(
                "Choose a source location and the AquaCache location it means. The saved match will be reused for future files with this profile."
              ),
              DT::DTOutput(ns("location_mapping_summary")),
              uiOutput(ns("location_mapping_editor")),
              actionButton(
                ns("save_location_mapping"),
                "Save selected location mapping"
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
          tags$h5("Sample details"),
          helpText(
            "Select a sample to update it. Deselect all rows to create a new sample."
          ),
          uiOutput(ns("sample_editor")),
          DT::DTOutput(ns("sample_location_summary")),
          tags$hr(),
          conditionalPanel(
            condition = "input.entry_mode == 'manual'",
            ns = ns,
            tags$h5("Add a result to the selected sample"),
            uiOutput(ns("manual_result_sample_label")),

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
              column(
                3,
                selectizeInput(
                  ns("manual_laboratory"),
                  "Laboratory",
                  choices = NULL,
                  multiple = TRUE,
                  options = list(
                    maxItems = 1,
                    placeholder = "Enter if applicable"
                  ),
                  width = "100%"
                )
              ),
              column(
                3,
                textInput(
                  ns("manual_lab_report"),
                  "Lab report number",
                  placeholder = "Optional",
                  width = "100%"
                )
              ),
              column(
                3,
                textInput(
                  ns("manual_lab_sample"),
                  "Lab sample number",
                  placeholder = "Optional",
                  width = "100%"
                )
              )
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
          ),
          tags$h5("Results for selected sample"),
          DT::DTOutput(ns("data_table")),
          fileInput(ns("attach_docs"), "Attach documents", multiple = TRUE),
          actionButton(ns("upload"), "Upload to AquaCache")
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
    data <- reactiveValues(df = addDiscData_empty_table())
    sample_qualifier_map <- reactiveVal(list())
    current_manual_sample <- reactiveVal(0L)
    manual_upload_id <- paste0(
      format(Sys.time(), "%Y%m%dT%H%M%OS6", tz = "UTC"),
      "-",
      Sys.getpid()
    )
    import_profiles <- reactiveVal(addDiscData_empty_profiles())
    profile_load_error <- reactiveVal(NULL)
    new_profile_template <- reactiveVal(NULL)

    con <- session$userData$AquaCache
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
    shareable_location_roles <- tryCatch(
      DBI::dbGetQuery(
        con,
        "SELECT role_name
           FROM public.get_shareable_principals_for('public.locations')
          ORDER BY role_name"
      )$role_name,
      error = function(e) "public_reader"
    )
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
    sample_group_share_roles <- tryCatch(
      DBI::dbGetQuery(
        con,
        "SELECT role_name
         FROM public.get_shareable_principals_for('discrete.sample_groups')
         WHERE role_name <> 'public_reader'
           AND pg_has_role(current_user, role_name, 'member')
         ORDER BY CASE WHEN lower(role_name) LIKE '%admin%' THEN 0 ELSE 1 END,
                  role_name
         LIMIT 1"
      )$role_name,
      error = function(e) character()
    )
    sample_group_share_role <- if (length(sample_group_share_roles)) {
      sample_group_share_roles[[1]]
    } else {
      NA_character_
    }
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
        selected <- if (nrow(profiles)) {
          profiles$profile_key[[1]]
        } else {
          character()
        }
      }
      updateSelectizeInput(
        session,
        "import_profile",
        choices = stats::setNames(
          profiles$profile_key,
          paste(profiles$source_code, profiles$profile_name, sep = " - ")
        ),
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
          "Your database role can use import profiles but cannot create them. Ask an administrator to grant profile-management access."
        ))
      }
      if (!nrow(import_profiles())) {
        return(tags$div(
          class = "alert alert-warning",
          "No workbook formats are saved yet. Select Set up workbook format to get started."
        ))
      }
      NULL
    })

    if (!can_manage_profiles) {
      shinyjs::disable("new_import_profile")
    }
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
        tags$h5(if (identical(parser_family, "transposed")) {
          "Source rows and columns"
        } else {
          "Source columns and layout"
        }),
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

    observeEvent(input$new_import_profile, {
      if (!can_manage_profiles) {
        showNotification(
          "Your database role cannot create import profiles.",
          type = "error"
        )
        return()
      }
      profiles <- import_profiles()
      selected_key <- addDiscData_first(input$import_profile, "")
      selected_row <- which(profiles$profile_key == selected_key)
      profile <- if (length(selected_row) == 1L) {
        profiles[selected_row, , drop = FALSE]
      } else {
        NULL
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
      clone_profile <- !is.null(profile)
      suggested_code <- if (clone_profile) {
        paste0(profile$profile_code[[1]], "_copy")
      } else {
        ""
      }
      showModal(modalDialog(
        title = if (clone_profile) {
          "Copy selected workbook format"
        } else {
          "Set up workbook format"
        },
        helpText(
          if (clone_profile) {
            "The selected format supplies the layout and defaults. Change the source columns to match the new file."
          } else {
            "Describe how this workbook is arranged. The saved format will be available in the selector."
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
              value = if (clone_profile) {
                paste(profile$profile_name[[1]], "copy")
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
                "One result per row" = "long",
                "Samples in columns" = "transposed",
                "XLR detailed report" = "xlr"
              ),
              selected = if (clone_profile) {
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
          actionButton(ns("save_import_profile"), "Create profile")
        ),
        size = "l",
        easyClose = FALSE
      ))
    })

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
          if (new_profile_key %in% import_profiles()$profile_key) {
            stop("That lab/source already has a workbook format with this code.")
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
              note = "Created from YGwater add discrete data."
            )
          })
          reload_profiles(selected = new_profile_key)
          removeModal()
          showNotification(
            "Workbook format saved.",
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

    observeEvent(input$preview_file, {
      req(input$file)
      tryCatch(
        {
          profile <- selected_profile()
          parsed <- addDiscData_parse_upload(
            input$file$datapath,
            profile
          )
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
            mappings = location_mappings
          )
          parsed <- addDiscData_apply_mappings(
            parsed,
            con,
            profile_code = profile$profile_code[[1]]
          )
          data$df <- parsed[names(addDiscData_empty_table())]
          showNotification(
            sprintf(
              "Parsed %s result rows from %s sample(s).",
              nrow(data$df),
              length(unique(data$df$sample_key))
            ),
            type = "message"
          )
        },
        error = function(e) {
          showNotification(paste("Preview failed:", e$message), type = "error")
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

    observeEvent(input$create_sample_group, {
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
      if (!addDiscData_present(sample_group_share_role)) {
        showNotification(
          "Your account has no private access group for a new sample group. Ask a database administrator to configure access.",
          type = "error",
          duration = 10
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
          options = list(placeholder = "Choose a group type", maxItems = 1)
        ),
        textInput(
          ns("new_sample_group_name"),
          "Group name",
          placeholder = "For example: Tatchun Creek trip, 2026-07-23"
        ),
        textInput(
          ns("new_sample_group_code"),
          "Group code (optional)",
          placeholder = "Use the trip, cooler, shipment, or batch code if available"
        ),
        selectizeInput(
          ns("new_sample_group_owner"),
          "Owner",
          choices = stats::setNames(
            as.character(organizations$organization_id),
            organizations$name
          ),
          selected = as.character(owner_id),
          options = list(placeholder = "Choose an owner", maxItems = 1)
        ),
        textAreaInput(
          ns("new_sample_group_note"),
          "Note (optional)",
          rows = 2,
          placeholder = "Add context that will help identify this group later"
        ),
        tags$p(
          class = "text-muted",
          paste("Visible to your access group:", sample_group_share_role)
        ),
        footer = tagList(
          actionButton(ns("cancel_new_sample_group"), "Cancel"),
          actionButton(
            ns("save_new_sample_group"),
            "Create group and select it",
            class = "btn-primary"
          )
        ),
        easyClose = FALSE
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$cancel_new_sample_group, removeModal(), ignoreInit = TRUE)

    observeEvent(input$save_new_sample_group, {
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
      if (!addDiscData_present(sample_group_share_role)) {
        showNotification(
          "Your account has no private access group for a new sample group.",
          type = "error"
        )
        return()
      }
      tryCatch({
        inserted <- DBI::dbGetQuery(
          con,
          "INSERT INTO discrete.sample_groups (
             group_type, group_code, group_name, owner, note, share_with
           ) VALUES (
             $1, NULLIF($2::TEXT, ''), NULLIF($3::TEXT, ''), $4,
             NULLIF($5::TEXT, ''),
             ARRAY[$6]::TEXT[]
           )
           RETURNING sample_group_id",
          params = list(
            group_type,
            group_code,
            group_name,
            group_owner,
            group_note,
            sample_group_share_role
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
      }, error = function(e) {
        message_text <- if (grepl(
          "duplicate|unique",
          conditionMessage(e),
          ignore.case = TRUE
        )) {
          "A group with that owner, type, and code already exists. Select it in Sample group or use a different code."
        } else {
          paste("Creating the sample group failed:", conditionMessage(e))
        }
        showNotification(message_text, type = "error", duration = 10)
      })
    }, ignoreInit = TRUE)

    observeEvent(input$save_sample, {
      existing <- selected_sample_row()
      location_id <- addDiscData_int(input$edit_sample_location)
      sub_location_id <- addDiscData_int(input$edit_sample_sublocation)
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
      sample_values <- list(
        location_id = location_id,
        sub_location_id = sub_location_id,
        datetime = sample_datetime,
        media_id = addDiscData_int(input$edit_sample_media),
        collection_method = addDiscData_int(input$edit_sample_method),
        sample_type = addDiscData_int(input$edit_sample_type),
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
      if (is.null(existing)) {
        current_manual_sample(current_manual_sample() + 1L)
        row <- addDiscData_empty_table()
        row[1, ] <- NA
        row$sample_key <- paste0(manual_upload_id, "-", current_manual_sample())
        row$source_location_name <- ""
        row$location_mapping_status <- "manual"
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
        data$df$location_mapping_status[ix] <- "edited in preview"
        showNotification("Sample metadata updated.", type = "message")
      }
      qualifier_state <- sample_qualifier_map()
      qualifier_state[[key]] <- qualifier_values
      sample_qualifier_map(qualifier_state)
      pending_sample_group(NULL)
    })

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
      row$laboratory <- addDiscData_int(input$manual_laboratory)
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
      row$lab_report_no <- trimws(addDiscData_first(
        input$manual_lab_report,
        ""
      ))
      row$lab_sample_no <- trimws(addDiscData_first(
        input$manual_lab_sample,
        ""
      ))
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
        DT::datatable(
          summary,
          rownames = FALSE,
          selection = "single",
          options = list(scrollX = TRUE, pageLength = 10)
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
          selected && identical(
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
          selected = as.character(sample_qualifier_map()[[row$sample_key[[1]]]])
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
        DT::datatable(
          summary,
          rownames = FALSE,
          selection = "single",
          options = list(scrollX = TRUE, pageLength = 15)
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
        if (
          tolower(trimws(source_name)) != tolower(trimws(source_code))
        ) {
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
        return("Target unit: not configured")
      }
      paste("Target unit:", unit[[1]])
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
      tryCatch(
        {
          parameter_id <- addDiscData_int(input$mapping_parameter)
          if (is.na(parameter_id)) {
            stop("Select an AquaCache parameter before saving.")
          }
          conversion <- addDiscData_num(input$mapping_conversion)
          result_offset <- addDiscData_num(input$mapping_result_offset)
          if (is.na(conversion) || !is.finite(conversion)) {
            stop("Enter a finite conversion multiplier.")
          }
          if (is.na(result_offset) || !is.finite(result_offset)) {
            stop("Enter a finite result offset.")
          }
          profile <- selected_profile()
          DBI::dbWithTransaction(con, {
            addDiscData_upsert_mapping(
              con = con,
              source_code = row$source_code[[1]],
              source_name = row$source_code[[1]],
              parameter_code = row$source_parameter_code[[1]],
              unit = row$source_unit[[1]],
              parameter_id = parameter_id,
              result_type = addDiscData_int(input$mapping_result_type, 2L),
              sample_fraction_id = addDiscData_int(
                input$mapping_sample_fraction
              ),
              result_value_type = addDiscData_int(
                input$mapping_result_value_type,
                1L
              ),
              result_speciation_id = addDiscData_int(
                input$mapping_result_speciation
              ),
              matrix_state_id = addDiscData_int(input$mapping_matrix_state, 1L),
              conversion = conversion,
              result_offset = result_offset,
              note = "Saved from YGwater add discrete data mapping editor.",
              profile_code = profile$profile_code[[1]]
            )
          })
          data$df <- addDiscData_apply_mappings(
            data$df,
            con,
            profile_code = profile$profile_code[[1]]
          )[names(addDiscData_empty_table())]
          showNotification("Saved mapping.", type = "message")
        },
        error = function(e) {
          showNotification(
            paste("Saving mappings failed:", e$message),
            type = "error"
          )
        }
      )
    })

    location_mapping_rows <- reactive({
      rows <- data$df
      profile <- tryCatch(selected_profile(), error = function(e) NULL)
      if (!nrow(rows) || is.null(profile) || !nrow(profile)) {
        return(data.frame())
      }
      rows <- rows[addDiscData_present(rows$source_location_name), , drop = FALSE]
      if (!nrow(rows)) return(data.frame())
      key <- tolower(trimws(rows$source_location_name))
      rows <- rows[!duplicated(key), , drop = FALSE]
      rows[order(tolower(rows$source_location_name)), , drop = FALSE]
    })

    output$location_mapping_summary <- DT::renderDT({
      rows <- location_mapping_rows()
      if (!nrow(rows)) {
        return(DT::datatable(
          data.frame(Message = "No source locations are available to map."),
          rownames = FALSE, selection = "none", options = list(dom = "t")
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
      DT::datatable(summary, rownames = FALSE, selection = "single",
                    options = list(pageLength = 10, scrollX = TRUE))
    }, server = FALSE)

    selected_location_mapping <- reactive({
      rows <- location_mapping_rows()
      selected <- input$location_mapping_summary_rows_selected
      if (!nrow(rows) || length(selected) != 1L || selected > nrow(rows)) {
        return(NULL)
      }
      rows[selected, , drop = FALSE]
    })

    observeEvent(input$location_mapping_summary_rows_selected, {
      row <- selected_location_mapping()
      if (is.null(row)) return()
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
        selected = if (is.na(current_location)) "" else
          as.character(current_location)
      )
      updateSelectizeInput(
        session,
        "location_mapping_sublocation",
        selected = if (is.na(current_sub)) "" else as.character(current_sub)
      )
    }, ignoreInit = TRUE)

    output$location_mapping_editor <- renderUI({
      row <- selected_location_mapping()
      if (is.null(row)) return(tags$div(class = "text-muted", "Select a source location above."))
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
          , drop = FALSE
        ]
      }
      if (!current_sub %in% available_sub$sub_location_id) {
        current_sub <- NA_integer_
      }
      wellPanel(
        tags$strong(row$source_location_name[[1]]),
        fluidRow(
          column(6, selectizeInput(
            ns("location_mapping_target"), "AquaCache location",
            choices = addDiscData_location_choices(
              current_locations,
              include_blank = TRUE
            ),
            selected = if (is.na(current_location)) "" else
              as.character(current_location),
            options = list(placeholder = "Choose a location", maxItems = 1)
          )),
          column(6, selectizeInput(
            ns("location_mapping_sublocation"), "Sub-location (optional)",
            choices = c("None" = "", stats::setNames(
              as.character(available_sub$sub_location_id),
              available_sub$sub_location_name
            )),
            selected = if (is.na(current_sub)) "" else as.character(current_sub),
            options = list(placeholder = "None", maxItems = 1)
          ))
        )
      )
    })

    observeEvent(input$save_location_mapping, {
      row <- selected_location_mapping()
      if (is.null(row)) {
        showNotification("Select a source location first.", type = "warning")
        return()
      }
      tryCatch({
        location_id <- addDiscData_int(input$location_mapping_target)
        if (is.na(location_id)) stop("Choose an AquaCache location.")
        profile <- selected_profile()
        AquaCache::upsertImportLocationMappings(
          con = con,
          source_code = profile$source_code[[1]],
          source_name = profile$source_code[[1]],
          profile_code = profile$profile_code[[1]],
          mappings = data.frame(
            source_location_code = row$source_location_name[[1]],
            source_location_name = row$source_location_name[[1]],
            location_id = location_id,
            sub_location_id = addDiscData_int(input$location_mapping_sublocation),
            priority = 50L,
            active = TRUE,
            note = "Saved from YGwater add discrete data location mapping editor."
          ),
          publish = FALSE
        )
        mappings <- AquaCache::getImportLocationMappings(
          con, profile$source_code[[1]], profile$profile_code[[1]],
          active = TRUE, include_draft = TRUE
        )
        data$df <- addDiscData_location_match(data$df, locations(), mappings)
        showNotification(
          "Saved location mapping for this profile. Review any sample-specific location edits before importing.",
          type = "message"
        )
      }, error = function(e) {
        showNotification(paste("Saving location mapping failed:", e$message), type = "error")
      })
    })

    result_flag_mapping_rows <- reactive({
      rows <- data$df
      profile <- tryCatch(selected_profile(), error = function(e) NULL)
      if (!nrow(rows) || is.null(profile) || !nrow(profile)) {
        return(data.frame())
      }
      rows <- rows[addDiscData_present(rows$source_result_flag), , drop = FALSE]
      if (!nrow(rows)) return(data.frame())
      keys <- paste(
        tolower(ifelse(is.na(rows$source_result_flag_column), "", rows$source_result_flag_column)),
        tolower(rows$source_result_flag), sep = "\r"
      )
      rows[!duplicated(keys), , drop = FALSE]
    })

    output$result_flag_mapping_summary <- DT::renderDT({
      rows <- result_flag_mapping_rows()
      if (!nrow(rows)) {
        return(DT::datatable(
          data.frame(Message = "No source result flags are available to map."),
          rownames = FALSE, selection = "none", options = list(dom = "t")
        ))
      }
      summary <- data.frame(
        `Source flag column` = ifelse(
          addDiscData_present(rows$source_result_flag_column),
          rows$source_result_flag_column, "Any column"
        ),
        `Source flag value` = rows$source_result_flag,
        Status = ifelse(
          addDiscData_present(rows$result_flag_action), "Mapped", "Needs mapping"
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
      DT::datatable(summary, rownames = FALSE, selection = "single",
                    options = list(pageLength = 10, scrollX = TRUE))
    }, server = FALSE)

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
        con, profile$source_code[[1]], profile$profile_code[[1]],
        active = TRUE, include_draft = TRUE
      )
      if (nrow(mappings)) {
        value_match <- tolower(trimws(mappings$source_flag_value)) ==
          tolower(trimws(row$source_result_flag[[1]]))
        mapped_column <- tolower(trimws(mappings$source_flag_column))
        source_column <- tolower(trimws(row$source_result_flag_column[[1]]))
        column_match <- is.na(mapped_column) | !nzchar(mapped_column) |
          mapped_column == source_column
        hit <- which(value_match & column_match)
        if (length(hit)) {
          exact <- !is.na(mapped_column[hit]) & nzchar(mapped_column[hit]) &
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
          row$mapped_threshold_source <- mappings$result_condition_value_source[[chosen]]
          row$mapped_threshold <- mappings$result_condition_value_literal[[chosen]]
          row$mapped_note <- mappings$note_template[[chosen]]
        }
      }
      row
    })

    output$result_flag_mapping_editor <- renderUI({
      row <- selected_result_flag_mapping()
      if (is.null(row)) return(tags$div(class = "text-muted", "Select a source flag above."))
      wellPanel(
        tags$strong(paste0(
          if (addDiscData_present(row$source_result_flag_column[[1]])) {
            paste0(row$source_result_flag_column[[1]], ": ")
          },
          row$source_result_flag[[1]]
        )),
        fluidRow(
          column(4, selectizeInput(
            ns("flag_mapping_action"), "When this flag appears",
            choices = c(
              "Keep the result" = "keep_result",
              "Keep flag and clear numeric result" = "set_result_null",
              "Skip this result during import" = "skip_result",
              "Reject the result row" = "reject_row",
              "Add a note only" = "note_only"
            ), selected = if (addDiscData_present(row$mapped_action[[1]])) {
              row$mapped_action[[1]]
            } else "keep_result"
          )),
          column(4, selectizeInput(
            ns("flag_mapping_condition"), "Result condition (optional)",
            choices = c("None" = "", stats::setNames(
              as.character(result_conditions$result_condition_id),
              result_conditions$result_condition
            )), selected = if (is.na(row$mapped_condition[[1]])) "" else
              as.character(row$mapped_condition[[1]])
          )),
          column(4, selectizeInput(
            ns("flag_mapping_threshold_source"), "Condition value from",
            choices = c(
              "None" = "none", "Result" = "result",
              "Method detection limit" = "method_detection_limit",
              "Reporting detection limit" = "reporting_detection_limit",
              "Enter a value" = "literal"
            ), selected = if (
              addDiscData_present(row$mapped_threshold_source[[1]])
            ) row$mapped_threshold_source[[1]] else "none"
          ))
        ),
        conditionalPanel(
          condition = "input.flag_mapping_threshold_source == 'literal'",
          ns = ns,
          numericInput(
            ns("flag_mapping_threshold"), "Condition value",
            value = row$mapped_threshold[[1]]
          )
        ),
        textInput(
          ns("flag_mapping_note"), "Optional note to add",
          value = addDiscData_first(row$mapped_note[[1]], "")
        )
      )
    })

    observeEvent(input$save_result_flag_mapping, {
      row <- selected_result_flag_mapping()
      if (is.null(row)) {
        showNotification("Select a source result flag first.", type = "warning")
        return()
      }
      tryCatch({
        threshold_source <- input$flag_mapping_threshold_source
        threshold <- if (identical(threshold_source, "literal")) {
          addDiscData_num(input$flag_mapping_threshold)
        } else {
          NA_real_
        }
        if (identical(threshold_source, "literal") &&
            (is.na(threshold) || !is.finite(threshold))) {
          stop("Enter a finite condition value.")
        }
        profile <- selected_profile()
        AquaCache::upsertImportResultFlagMappings(
          con = con,
          source_code = profile$source_code[[1]],
          source_name = profile$source_code[[1]],
          profile_code = profile$profile_code[[1]],
          mappings = data.frame(
            source_flag_column = if (addDiscData_present(row$source_result_flag_column[[1]])) {
              row$source_result_flag_column[[1]]
            } else NA_character_,
            source_flag_value = row$source_result_flag[[1]],
            result_condition_id = addDiscData_int(input$flag_mapping_condition),
            result_condition_value_source = threshold_source,
            result_condition_value_literal = threshold,
            result_action = input$flag_mapping_action,
            note_template = if (addDiscData_present(input$flag_mapping_note)) {
              input$flag_mapping_note
            } else NA_character_,
            priority = 50L,
            active = TRUE,
            note = "Saved from YGwater add discrete data result-flag mapping editor."
          ),
          publish = FALSE
        )
        data$df <- addDiscData_apply_mappings(
          data$df, con, profile_code = profile$profile_code[[1]]
        )[names(addDiscData_empty_table())]
        showNotification(
          "Saved result-flag mapping for this profile. The preview was recalculated from the uploaded values; review result edits before importing.",
          type = "message"
        )
      }, error = function(e) {
        showNotification(paste("Saving result-flag mapping failed:", e$message), type = "error")
      })
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
        DT::datatable(
          display,
          editable = list(
            target = "cell",
            disable = list(columns = disabled_columns)
          ),
          selection = "single",
          rownames = FALSE,
          options = list(scrollX = TRUE, pageLength = 15)
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

    find_existing_document <- function(file) {
      document <- readBin(file$datapath, "raw", file.info(file$datapath)$size)
      DBI::dbGetQuery(
        con,
        "SELECT document_id, name
         FROM files.documents
         WHERE file_hash = md5(encode($1::bytea, 'hex'))
         LIMIT 1;",
        params = list(list(document))
      )
    }

    insertDoc <- function(file) {
      existing <- find_existing_document(file)
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

    link_sample_documents <- function(sample_id, document_ids) {
      document_ids <- unique(as.integer(document_ids))
      document_ids <- document_ids[!is.na(document_ids)]
      if (!length(document_ids)) {
        return(invisible(NULL))
      }

      for (document_id in document_ids) {
        DBI::dbExecute(
          con,
          "INSERT INTO discrete.sample_documents (
             sample_id,
             document_id,
             document_role,
             link_source
           ) VALUES ($1, $2, 'supporting', 'addDiscData')
           ON CONFLICT (sample_id, document_id) DO NOTHING;",
          params = list(as.integer(sample_id), as.integer(document_id))
        )
      }

      invisible(NULL)
    }

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
          "One or more samples require a sample group. Assign a group in that sample's metadata.",
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
      df <- data$df
      manual_sample_shells <- df$source_code == "YGwater-manual" &
        is.na(df$parameter_id)
      df <- df[!manual_sample_shells, , drop = FALSE]
      skipped <- which(df$result_flag_action == "skip_result")
      if (length(skipped)) {
        df <- df[-skipped, , drop = FALSE]
      }
      active <- FALSE
      tryCatch(
        {
          validate_upload_rows(df)
          has_groups <- any(!is.na(df$sample_group_id))
          if (has_groups && !isTRUE(check_groups$can_assign_group[[1]])) {
            stop(
              "You do not have permission to assign samples to groups.",
              call. = FALSE
            )
          }

          DBI::dbExecute(con, "BEGIN")
          active <- TRUE

          doc_ids <- integer()
          if (!is.null(input$file)) {
            doc_ids <- c(doc_ids, insertDoc(input$file))
          }
          if (!is.null(input$attach_docs)) {
            for (ii in seq_len(nrow(input$attach_docs))) {
              doc_ids <- c(
                doc_ids,
                insertDoc(list(
                  name = input$attach_docs$name[ii],
                  datapath = input$attach_docs$datapath[ii]
                ))
              )
            }
          }
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
          if (any(file_sample)) {
            run_source_ids <- unique(samples$import_source_id[file_sample])
            if (length(run_source_ids) != 1L) {
              stop(
                "A file upload must resolve to exactly one import source.",
                call. = FALSE
              )
            }
            profile <- selected_profile()
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
              source_file_name = input$file$name,
              source_file_size = input$file$size,
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
              "INSERT INTO discrete.samples (
                 location_id,
                 sub_location_id,
                 media_id,
                 z,
                  datetime,
                  target_datetime,
                  collection_method,
                  sample_type,
                  linked_with,
                  sample_volume_ml,
                  purge_volume_l,
                  purge_time_min,
                  flow_rate_l_min,
                  wave_hgt_m,
                  sample_grade,
                  sample_approval,
                  owner,
                  contributor,
                  comissioning_org,
                  sampling_org,
                  source_adapter_function,
                  external_sample_id,
                  import_source_id,
                  no_source_update,
                  note
                ) VALUES (
                  $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13,
                  $14, $15, $16, $17, $18, $19, $20, $21, $22, $23, $24, $25
                )
               RETURNING sample_id",
              params = list(
                as.integer(samples$location_id[[i]]),
                addDiscData_int(samples$sub_location_id[[i]]),
                as.integer(samples$media_id[[i]]),
                addDiscData_num(samples$z[[i]]),
                as.POSIXct(samples$datetime[[i]], tz = "UTC"),
                as.POSIXct(samples$target_datetime[[i]], tz = "UTC"),
                as.integer(samples$collection_method[[i]]),
                as.integer(samples$sample_type[[i]]),
                addDiscData_int(samples$linked_with[[i]]),
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
                  paste(
                    "Imported source sample:",
                    samples$source_sample_id[[i]]
                  )
                } else {
                  NA_character_
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
            qualifier_ids <- sample_qualifier_map()[[samples$sample_key[[i]]]]
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
               )
               RETURNING result_id",
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
              "sample_no_source_update"
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
              validation_summary = list(
                committed = inserted_results,
                errors = 0L
              )
            )
          }

          DBI::dbExecute(con, "COMMIT")
          active <- FALSE
          showNotification(
            sprintf(
              "Added %s sample(s) and %s result(s).",
              inserted_samples,
              inserted_results
            ),
            type = "message"
          )
          data$df <- addDiscData_empty_table()
        },
        error = function(e) {
          if (isTRUE(active)) {
            try(DBI::dbExecute(con, "ROLLBACK"), silent = TRUE)
          }
          showNotification(paste("Upload failed:", e$message), type = "error")
        }
      )
    })

    return(outputs)
  })
}
