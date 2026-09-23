# UI and server code for adding discrete samples and results.

addDiscData_empty_table <- function() {
  data.frame(
    sample_key = character(),
    source_location_name = character(),
    location_mapping_status = character(),
    location_id = integer(),
    sub_location_id = integer(),
    datetime = as.POSIXct(character(), tz = "UTC"),
    media_id = integer(),
    collection_method = integer(),
    sample_type = integer(),
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
  values[!vapply(values, function(x) {
    length(x) == 1L && (is.na(x) || identical(x, ""))
  }, logical(1))]
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
      any(c("first_sample_column", "parameter_code_column") %in% names(column_map))
  ) {
    return("transposed")
  }
  if (
    all(c("parameter_name", "lab_sample_id", "result") %in% names(column_map)) &&
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
  parsed <- suppressWarnings(as.POSIXct(
    value,
    tz = tz,
    tryFormats = c(
      "%Y-%m-%d %H:%M:%S",
      "%Y-%m-%d %H:%M",
      "%Y-%m-%d %I:%M:%S %p",
      "%Y-%m-%d %I:%M %p"
    )
  ))
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
    strategy %in% c("name", "name_or_first") &&
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
    if (is.na(sheet_index) || sheet_index < 1L || sheet_index > length(sheets)) {
      stop("Import profile worksheet index is outside the workbook.", call. = FALSE)
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
        rows$location_id[[i]] <- addDiscData_int(mappings$location_id[[hit[[1]]]])
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
    active = TRUE
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
    active = TRUE
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
      rows$conversion[[row]] + rows$result_offset[[row]],
    reporting_detection_limit = rows$source_reporting_detection_limit[[row]] *
      rows$conversion[[row]] + rows$result_offset[[row]],
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
          (
            is.na(result_flag_mappings$source_flag_column) |
              !nzchar(mapped_column) |
              mapped_column == column_key
          )
      )
      if (!length(hit)) {
        next
      }
      candidates <- result_flag_mappings[hit, , drop = FALSE]
      exact_column <- !is.na(candidates$source_flag_column) &
        nzchar(trimws(candidates$source_flag_column)) &
        tolower(trimws(candidates$source_flag_column)) == column_key
      candidates <- candidates[order(
        -as.integer(candidates$profile_specific),
        -as.integer(exact_column),
        candidates$priority,
        candidates$import_result_flag_mapping_id
      ), , drop = FALSE]
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
    match_columns = c("parameter_code", "unit")
  )
}

addDiscData_target_unit <- function(parameters, parameter_id, matrix_state_id) {
  parameter_id <- suppressWarnings(as.integer(parameter_id))
  matrix_state_id <- suppressWarnings(as.integer(matrix_state_id))
  out <- rep(NA_character_, max(length(parameter_id), length(matrix_state_id)))
  parameter_id <- rep_len(parameter_id, length(out))
  matrix_state_id <- rep_len(matrix_state_id, length(out))
  unit_columns <- c("1" = "unit_liquid", "2" = "unit_solid", "3" = "unit_gas")

  for (i in seq_along(out)) {
    row <- match(parameter_id[[i]], parameters$parameter_id)
    unit_column <- unname(unit_columns[as.character(matrix_state_id[[i]])])
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
        c("Gas", "unit_gas")
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
  sample_types
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
    Result = rows$result,
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
      rows$matrix_state_id
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
    `Analysis datetime (UTC)` = format(
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
    "Result" = "result",
    "Result condition" = "result_condition",
    "Condition value" = "result_condition_value",
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
        open = c("sample_defaults_panel", "data_panel"),
        accordion_panel(
          id = ns("sample_defaults_panel"),
          title = "Manual sample defaults and optional group",
          helpText(
            "Location, sub-location, media, method, type, and datetime below apply only to manually entered samples. File imports can contain many locations and are assigned per source sample in the table below. The optional sample group applies to either input method."
          ),
          fluidRow(
            column(
              5,
              selectizeInput(
                ns("location"),
                "Manual sample location",
                multiple = TRUE,
                choices = NULL,
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
                  ns("find_default_location_map"),
                  label = NULL,
                  icon = icon("map-location-dot"),
                  title = "Find the default location on a map"
                )
              )
            ),
            column(
              6,
              selectizeInput(
                ns("sublocation"),
                "Manual sample sub-location",
                multiple = TRUE,
                choices = NULL,
                options = list(
                  create = TRUE,
                  placeholder = "Optional",
                  maxItems = 1
                )
              )
            )
          ),
          fluidRow(
            column(3, selectizeInput(ns("media_id"), "Media", choices = NULL)),
            column(
              3,
              selectizeInput(
                ns("collection_method"),
                "Collection method",
                choices = NULL
              )
            ),
            column(
              3,
              selectizeInput(ns("sample_type"), "Sample type", choices = NULL)
            ),
            column(
              3,
              selectizeInput(
                ns("timezone"),
                "Input timezone",
                choices = input_timezone_choices(),
                selected = default_input_timezone()
              )
            )
          ),
          shinyWidgets::airDatepickerInput(
            ns("sample_datetime"),
            "Manual sample datetime",
            value = Sys.time(),
            timepicker = TRUE,
            update_on = "change",
            tz = air_datetime_widget_timezone(default_input_timezone()),
            timepickerOpts = shinyWidgets::timepickerOptions(
              minutesStep = 15,
              timeFormat = "HH:mm"
            )
          ),
          radioButtons(
            ns("sample_group_mode"),
            "Sample group",
            choices = c(
              "None" = "none",
              "Existing" = "existing",
              "Create new" = "new"
            ),
            selected = "none",
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.sample_group_mode == 'existing'",
            ns = ns,
            selectizeInput(
              ns("sample_group_id"),
              "Existing sample group",
              choices = NULL
            )
          ),
          conditionalPanel(
            condition = "input.sample_group_mode == 'new'",
            ns = ns,
            fluidRow(
              column(
                4,
                selectizeInput(
                  ns("sample_group_type"),
                  "Group type",
                  choices = NULL
                )
              ),
              column(4, textInput(ns("sample_group_code"), "Group code")),
              column(4, textInput(ns("sample_group_name"), "Group name"))
            ),
            textAreaInput(
              ns("sample_group_note"),
              "Group notes",
              width = "100%"
            )
          )
        ),
        accordion_panel(
          id = ns("data_panel"),
          title = "New data",
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
                  "Import profile",
                  choices = NULL
                )
              ),
              column(
                3,
                tags$div(
                  style = "padding-top: 25px;",
                  actionButton(ns("new_import_profile"), "Create profile")
                )
              )
            ),
            uiOutput(ns("import_profile_status")),
            actionButton(ns("preview_file"), "Preview file"),
            checkboxInput(
              ns("show_all_mappings"),
              "Show mapped parameters in mapping editor",
              value = FALSE
            ),
            helpText(
              "Select one row below, complete its mapping details, then save it. Incomplete rows are never written."
            ),
            DT::DTOutput(ns("mapping_summary")),
            uiOutput(ns("mapping_editor")),
            actionButton(ns("save_parameter_mappings"), "Save selected mapping")
          ),
          conditionalPanel(
            condition = "input.entry_mode == 'manual'",
            ns = ns,
            fluidRow(
              column(
                5,
                selectizeInput(
                  ns("manual_parameter"),
                  "Parameter",
                  choices = NULL
                )
              ),
              column(3, textInput(ns("manual_result"), "Result")),
              column(2, actionButton(ns("add_manual_result"), "Add result")),
              column(2, actionButton(ns("new_manual_sample"), "New sample"))
            )
          ),
          tags$hr(),
          tags$h5("Imported and manual sample locations"),
          helpText(
            "Each row represents one sample. File locations are resolved from saved source-code mappings first, then exact name/code/alias matches. Select rows to correct or confirm a location."
          ),
          DT::DTOutput(ns("sample_location_summary")),
          fluidRow(
            column(
              5,
              selectizeInput(
                ns("sample_location"),
                "Location for selected samples",
                choices = NULL,
                options = list(placeholder = "Search name, code, or alias")
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
                  title = "Find a sample location on a map"
                )
              )
            ),
            column(
              3,
              selectizeInput(
                ns("sample_sub_location"),
                "Sub-location",
                choices = c("None" = "")
              )
            ),
            column(
              3,
              tags$div(
                checkboxInput(
                  ns("remember_location_mapping"),
                  "Remember lab code mapping",
                  value = TRUE
                ),
                actionButton(ns("apply_sample_location"), "Apply to selected"),
                actionButton(ns("clear_sample_location"), "Clear"),
                actionButton(ns("create_sample_location"), "Create location")
              )
            )
          ),
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
    current_manual_sample <- reactiveVal(1L)
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
    can_manage_profiles <- isTRUE(profile_permissions$can_write_profiles[[1]]) &&
      isTRUE(profile_permissions$can_write_sources[[1]]) &&
      isTRUE(profile_permissions$can_use_profile_sequence[[1]]) &&
      isTRUE(profile_permissions$can_use_source_sequence[[1]])
    location_create_permissions <- tryCatch(
      DBI::dbGetQuery(
        con,
        "SELECT
           has_table_privilege(
             current_user,
             'public.locations',
             'INSERT'
           ) AND has_table_privilege(
             current_user,
             'public.datum_conversions',
             'INSERT'
           ) AND has_table_privilege(
             current_user,
             'discrete.import_location_mappings',
             'INSERT'
           ) AND has_sequence_privilege(
             current_user,
             pg_get_serial_sequence('public.locations', 'location_id'),
             'USAGE'
           ) AND has_sequence_privilege(
             current_user,
             pg_get_serial_sequence(
               'discrete.import_location_mappings',
               'import_location_mapping_id'
             ),
             'USAGE'
           ) AS can_create"
      )$can_create[[1]],
      error = function(e) FALSE
    )
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
                ul.unit_name AS unit_liquid,
                us.unit_name AS unit_solid,
                ug.unit_name AS unit_gas
           FROM public.parameters p
           LEFT JOIN public.units ul ON ul.unit_id = p.units_liquid
           LEFT JOIN public.units us ON us.unit_id = p.units_solid
           LEFT JOIN public.units ug ON ug.unit_id = p.units_gas
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
      "SELECT matrix_state_id, matrix_state_name
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
    sample_group_types <- DBI::dbGetQuery(
      con,
      "SELECT group_type, group_type_name
       FROM discrete.sample_group_types
       WHERE active
       ORDER BY sort_order, group_type_name"
    )
    sample_groups <- DBI::dbGetQuery(
      con,
      "SELECT sample_group_id, group_type, group_code, group_name
       FROM discrete.sample_groups
       WHERE active
       ORDER BY start_datetime DESC NULLS LAST, sample_group_id DESC"
    )

    pending_location_selection <- reactiveVal(character(0))
    pending_location_new <- reactiveVal(NULL)
    pending_sublocation_selection <- reactiveVal(character(0))
    pending_sublocation_new <- reactiveVal(NULL)

    update_location_selectize <- function(selected = NULL) {
      args <- list(
        session = session,
        inputId = "location",
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
        inputId = "sublocation",
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
    updateSelectizeInput(
      session,
      "sample_location",
      choices = addDiscData_location_choices(locations(), include_blank = TRUE)
    )
    updateSelectizeInput(
      session,
      "media_id",
      choices = stats::setNames(media$media_id, media$media_type),
      selected = 1L
    )
    updateSelectizeInput(
      session,
      "collection_method",
      choices = stats::setNames(
        collection_methods$collection_method_id,
        collection_methods$collection_method
      ),
      selected = 27L
    )
    updateSelectizeInput(
      session,
      "sample_type",
      choices = stats::setNames(
        sample_types$sample_type_id,
        sample_types$sample_type
      ),
      selected = 34L
    )
    updateSelectizeInput(
      session,
      "sample_group_type",
      choices = stats::setNames(
        sample_group_types$group_type,
        sample_group_types$group_type_name
      )
    )
    sample_group_labels <- sprintf(
      "%s: %s",
      sample_groups$group_type,
      ifelse(
        nzchar(ifelse(
          is.na(sample_groups$group_code),
          "",
          sample_groups$group_code
        )),
        sample_groups$group_code,
        sample_groups$group_name
      )
    )
    updateSelectizeInput(
      session,
      "sample_group_id",
      choices = stats::setNames(
        sample_groups$sample_group_id,
        sample_group_labels
      )
    )

    observeEvent(
      input$timezone,
      {
        shift_air_datetime_input_timezone(
          session,
          input,
          "sample_datetime",
          input$timezone
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$location,
      {
        resolved <- resolve_selectize_lookup_values(
          input$location,
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
      input$sublocation,
      {
        resolved <- resolve_selectize_lookup_values(
          input$sublocation,
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
        selected <- if (nrow(profiles)) profiles$profile_key[[1]] else character()
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
      if (!nrow(import_profiles())) {
        return(tags$div(
          class = "alert alert-warning",
          "This database has no active import profiles. Create one here or run an approved profile seed script."
        ))
      }
      if (!can_manage_profiles) {
        return(tags$div(
          class = "text-muted",
          "Import profiles are database-managed. Your database role can use them but cannot create them."
        ))
      }
      NULL
    })

    if (!can_manage_profiles) {
      shinyjs::disable("new_import_profile")
    }
    if (!isTRUE(location_create_permissions)) {
      shinyjs::disable("create_sample_location")
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
        tags$h5("Source columns and layout"),
        helpText(
          "Enter the column headings exactly as they appear in the workbook. Transposed layouts use 1-based row and column numbers."
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
      new_profile_template(profile)
      clone_profile <- !is.null(profile)
      suggested_code <- if (clone_profile) {
        paste0(profile$profile_code[[1]], "_copy")
      } else {
        ""
      }
      showModal(modalDialog(
        title = if (clone_profile) {
          "Create import profile from selected"
        } else {
          "Create import profile"
        },
        helpText(
          if (clone_profile) {
            "The selected database profile supplies the layout and defaults. Change the source columns to match the new file."
          } else {
            "Define the first database-backed profile. The profile becomes available immediately after it is saved."
          }
        ),
        fluidRow(
          column(
            4,
            textInput(
              ns("new_profile_source_code"),
              "Source code",
              value = addDiscData_profile_value(profile, "source_code", "")
            )
          ),
          column(
            8,
            textInput(
              ns("new_profile_source_name"),
              "Source name",
              value = addDiscData_profile_value(profile, "source_name", "")
            )
          )
        ),
        fluidRow(
          column(
            4,
            textInput(
              ns("new_profile_code"),
              "Profile code",
              value = suggested_code
            )
          ),
          column(
            8,
            textInput(
              ns("new_profile_name"),
              "Profile name",
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
              "Source timezone",
              choices = input_timezone_choices(),
              selected = addDiscData_profile_value(
                profile,
                "timezone",
                "America/Whitehorse"
              )
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
              "Profile code must contain only lowercase letters, numbers, and underscores."
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
            stop("That source already has a profile with this profile code.")
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
          required_mapping_fields <- if (identical(parser_family, "transposed")) {
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
          profile_id <- DBI::dbWithTransaction(con, {
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
            sprintf("Created import profile %s.", profile_id),
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Creating profile failed:", e$message),
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
            active = TRUE
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

    observeEvent(input$new_manual_sample, {
      current_manual_sample(current_manual_sample() + 1L)
      showNotification(
        sprintf("Manual sample %s is active.", current_manual_sample()),
        type = "message"
      )
    })

    observeEvent(input$add_manual_result, {
      req(input$manual_parameter)
      parsed_result <- addDiscData_parse_result(input$manual_result)
      row <- addDiscData_empty_table()
      row[1, ] <- NA
      row$sample_key <- paste0(manual_upload_id, "-", current_manual_sample())
      row$source_location_name <- ""
      row$location_mapping_status <- "manual"
      row$location_id <- addDiscData_int(addDiscData_first(normalize_selectize_values(
        input$location
      )))
      row$sub_location_id <- addDiscData_int(addDiscData_first(normalize_selectize_values(
        input$sublocation
      )))
      row$datetime <- as.POSIXct(input$sample_datetime, tz = "UTC")
      row$media_id <- addDiscData_int(input$media_id, 1L)
      row$collection_method <- addDiscData_int(input$collection_method, 27L)
      row$sample_type <- addDiscData_int(input$sample_type, 34L)
      row$owner <- 1L
      row$source_sample_id <- row$sample_key
      row$source_parameter_code <- ""
      row$source_parameter_name <- ""
      row$source_unit <- ""
      row$parameter_id <- addDiscData_int(input$manual_parameter)
      row$result_type <- 3L
      row$matrix_state_id <- 1L
      row$sample_fraction_id <- NA_integer_
      row$result_value_type <- 1L
      row$result_speciation_id <- NA_integer_
      row$source_result_text <- as.character(input$manual_result)
      row$source_result <- parsed_result$result[[1]]
      row$source_result_condition_value <- parsed_result$result_condition_value[[
        1
      ]]
      row$result <- parsed_result$result[[1]]
      row$result_condition <- parsed_result$result_condition[[1]]
      row$result_condition_value <- parsed_result$result_condition_value[[1]]
      row$conversion <- 1
      row$result_offset <- 0
      row$laboratory <- NA_integer_
      row$analysis_datetime <- as.POSIXct(NA)
      row$note <- ""
      row$mapping_status <- "manual"
      row$source_code <- "YGwater-manual"
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
        location_index <- match(df$location_id, current_locations$location_id)
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
          `Sub-location` = sub_locations$sub_location_name[sub_location_index],
          `Location match` = df$location_mapping_status,
          check.names = FALSE
        )
        summary[is.na(summary)] <- ""
        DT::datatable(
          summary,
          rownames = FALSE,
          selection = list(mode = "multiple", target = "row"),
          options = list(scrollX = TRUE, pageLength = 10)
        )
      },
      server = FALSE
    )

    observeEvent(
      input$sample_location,
      {
        location_id <- addDiscData_int(input$sample_location)
        available <- sub_locations[
          !is.na(location_id) & sub_locations$location_id == location_id,
        ]
        updateSelectizeInput(
          session,
          "sample_sub_location",
          choices = c(
            "None" = "",
            stats::setNames(
              as.character(available$sub_location_id),
              available$sub_location_name
            )
          ),
          selected = ""
        )
      },
      ignoreInit = TRUE
    )

    selected_sample_keys <- reactive({
      rows <- sample_location_rows()
      selected <- input$sample_location_summary_rows_selected
      if (!nrow(rows) || !length(selected)) {
        return(character())
      }
      rows$sample_key[selected[selected <= nrow(rows)]]
    })

    observeEvent(input$apply_sample_location, {
      keys <- selected_sample_keys()
      location_id <- addDiscData_int(input$sample_location)
      if (!length(keys)) {
        showNotification("Select at least one sample row.", type = "warning")
        return()
      }
      if (is.na(location_id)) {
        showNotification("Select a location to apply.", type = "warning")
        return()
      }
      sub_location_id <- addDiscData_int(input$sample_sub_location)
      if (!is.na(sub_location_id)) {
        valid_sub_location <- any(
          sub_locations$sub_location_id == sub_location_id &
            sub_locations$location_id == location_id
        )
        if (!valid_sub_location) {
          showNotification(
            "The sub-location does not belong to that location.",
            type = "error"
          )
          return()
        }
      }
      data$df <- addDiscData_assign_sample_locations(
        data$df,
        sample_keys = keys,
        location_id = location_id,
        sub_location_id = sub_location_id
      )
      selected <- data$df$sample_key %in% keys
      data$df$location_mapping_status[selected] <- "assigned in preview"
      if (isTRUE(input$remember_location_mapping)) {
        source_rows <- unique(data$df[
          selected &
            addDiscData_present(data$df$source_location_name) &
            addDiscData_present(data$df$source_code) &
            data$df$source_code != "YGwater-manual",
          c("source_code", "source_location_name"),
          drop = FALSE
        ])
        if (nrow(source_rows)) {
          profile <- selected_profile()
          for (source_code in unique(source_rows$source_code)) {
            source_locations <- source_rows[
              source_rows$source_code == source_code,
              ,
              drop = FALSE
            ]
            AquaCache::upsertImportLocationMappings(
              con = con,
              source_code = source_code,
              source_name = source_code,
              profile_code = if (
                identical(source_code, profile$source_code[[1]])
              ) profile$profile_code[[1]] else NULL,
              mappings = data.frame(
                source_location_code = source_locations$source_location_name,
                source_location_name = source_locations$source_location_name,
                location_id = location_id,
                sub_location_id = sub_location_id,
                note = "Saved from YGwater add discrete data location assignment."
              )
            )
          }
          data$df$location_mapping_status[selected] <- "profile mapping saved"
        }
      }
      showNotification(
        sprintf("Assigned %s sample(s).", length(keys)),
        type = "message"
      )
    })

    observeEvent(input$clear_sample_location, {
      keys <- selected_sample_keys()
      if (!length(keys)) {
        showNotification("Select at least one sample row.", type = "warning")
        return()
      }
      data$df <- addDiscData_assign_sample_locations(
        data$df,
        sample_keys = keys
      )
      data$df$location_mapping_status[data$df$sample_key %in% keys] <-
        "unmapped"
    })

    observeEvent(input$create_sample_location, {
      keys <- selected_sample_keys()
      if (!length(keys)) {
        showNotification(
          "Select at least one source sample before creating its location.",
          type = "warning"
        )
        return()
      }
      selected_rows <- data$df[data$df$sample_key %in% keys, , drop = FALSE]
      source_codes <- unique(selected_rows$source_code[
        addDiscData_present(selected_rows$source_code) &
          selected_rows$source_code != "YGwater-manual"
      ])
      source_locations <- unique(selected_rows$source_location_name[
        addDiscData_present(selected_rows$source_location_name)
      ])
      if (length(source_codes) != 1L || length(source_locations) != 1L) {
        showNotification(
          "Select samples with one import source and one source location code.",
          type = "warning"
        )
        return()
      }
      showModal(modalDialog(
        title = "Create and map a location",
        helpText(
          "This creates the AquaCache location and immediately maps the lab location code for the selected import profile. Click the map or enter coordinates."
        ),
        fluidRow(
          column(
            6,
            textInput(
              ns("quick_source_location_code"),
              "Lab/source location code",
              value = source_locations[[1]]
            )
          ),
          column(
            6,
            textInput(ns("quick_location_code"), "AquaCache location code (optional)")
          )
        ),
        fluidRow(
          column(6, textInput(ns("quick_location_name"), "Location name")),
          column(6, textInput(ns("quick_location_alias"), "Alias (optional)"))
        ),
        fluidRow(
          column(
            4,
            numericInput(ns("quick_location_latitude"), "Latitude", value = NA)
          ),
          column(
            4,
            numericInput(ns("quick_location_longitude"), "Longitude", value = NA)
          ),
          column(
            4,
            selectizeInput(
              ns("quick_location_type"),
              "Location type",
              choices = stats::setNames(
                location_types$type_id,
                location_types$type
              )
            )
          )
        ),
        fluidRow(
          column(
            6,
            numericInput(
              ns("quick_location_elevation"),
              "Elevation (m, optional; CGVD2013:2010)",
              value = NA
            )
          ),
          column(
            6,
            selectizeInput(
              ns("quick_location_share_with"),
              "Share with",
              choices = shareable_location_roles,
              selected = "public_reader",
              multiple = TRUE
            )
          )
        ),
        textAreaInput(ns("quick_location_note"), "Note (optional)"),
        leaflet::leafletOutput(ns("quick_location_map"), height = "360px"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_quick_location"), "Create and map")
        ),
        size = "l",
        easyClose = FALSE
      ))
    })

    output$quick_location_map <- leaflet::renderLeaflet({
      mapped <- locations()
      mapped <- mapped[
        is.finite(mapped$latitude) & is.finite(mapped$longitude),
        ,
        drop = FALSE
      ]
      map <- leaflet::leaflet(mapped) |>
        leaflet::addTiles()
      if (nrow(mapped)) {
        map <- map |>
          leaflet::addCircleMarkers(
            lng = ~longitude,
            lat = ~latitude,
            label = addDiscData_location_labels(mapped),
            radius = 4,
            fillOpacity = 0.55,
            stroke = FALSE,
            clusterOptions = leaflet::markerClusterOptions()
          ) |>
          leaflet::fitBounds(
            min(mapped$longitude),
            min(mapped$latitude),
            max(mapped$longitude),
            max(mapped$latitude)
          )
      }
      map
    })

    observeEvent(input$quick_location_map_click, {
      updateNumericInput(
        session,
        "quick_location_latitude",
        value = input$quick_location_map_click$lat
      )
      updateNumericInput(
        session,
        "quick_location_longitude",
        value = input$quick_location_map_click$lng
      )
    })

    observeEvent(input$save_quick_location, {
      tryCatch(
        {
          keys <- selected_sample_keys()
          req(length(keys))
          selected_rows <- data$df[data$df$sample_key %in% keys, , drop = FALSE]
          source_code <- unique(selected_rows$source_code[
            addDiscData_present(selected_rows$source_code) &
              selected_rows$source_code != "YGwater-manual"
          ])
          if (length(source_code) != 1L) {
            stop("The selected samples no longer have one import source.")
          }
          source_location_code <- trimws(addDiscData_first(
            input$quick_source_location_code,
            ""
          ))
          location_name <- trimws(addDiscData_first(
            input$quick_location_name,
            ""
          ))
          if (!nzchar(source_location_code) || !nzchar(location_name)) {
            stop("Source location code and location name are required.")
          }
          latitude <- addDiscData_num(input$quick_location_latitude)
          longitude <- addDiscData_num(input$quick_location_longitude)
          if (
            !is.finite(latitude) || latitude < -90 || latitude > 90 ||
              !is.finite(longitude) || longitude < -180 || longitude > 180
          ) {
            stop("Enter valid decimal-degree latitude and longitude.")
          }
          location_type <- addDiscData_int(input$quick_location_type)
          if (is.na(location_type)) {
            stop("Select a location type.")
          }
          elevation <- addDiscData_num(input$quick_location_elevation)
          if (!is.na(elevation) && !length(quick_elevation_datum)) {
            stop("The database does not contain the CGVD2013:2010 datum.")
          }
          profile <- selected_profile()
          location_code <- trimws(addDiscData_first(
            input$quick_location_code,
            ""
          ))
          alias <- trimws(addDiscData_first(input$quick_location_alias, ""))
          note <- trimws(addDiscData_first(input$quick_location_note, ""))
          share_with <- input$quick_location_share_with
          if (!length(share_with)) {
            share_with <- "public_reader"
          }

          DBI::dbWithTransaction(con, {
            AquaCache::addACLocation(
              name = location_name,
              name_fr = "Traduction requise!",
              alias = if (nzchar(alias)) alias else NA_character_,
              location_code = if (nzchar(location_code)) {
                location_code
              } else {
                NA_character_
              },
              latitude = latitude,
              longitude = longitude,
              share_with = paste(share_with, collapse = ","),
              location_type = location_type,
              note = if (nzchar(note)) note else NA_character_,
              contact = NA_character_,
              datum_id_from = 10L,
              datum_id_to = if (is.na(elevation)) {
                10L
              } else {
                quick_elevation_datum[[1]]
              },
              conversion_m = if (is.na(elevation)) 0 else elevation,
              current = TRUE,
              network = NA_integer_,
              project = NA_integer_,
              con = con
            )
            new_location <- DBI::dbGetQuery(
              con,
              "SELECT location_id
                 FROM public.locations
                WHERE name = $1
                ORDER BY location_id DESC
                LIMIT 1",
              params = list(location_name)
            )
            if (nrow(new_location) != 1L) {
              stop("The newly created location could not be read back.")
            }
            AquaCache::upsertImportLocationMappings(
              con = con,
              source_code = source_code[[1]],
              source_name = source_code[[1]],
              profile_code = profile$profile_code[[1]],
              mappings = data.frame(
                source_location_code = source_location_code,
                source_location_name = source_location_code,
                location_id = new_location$location_id[[1]],
                note = "Created and mapped from YGwater add discrete data."
              )
            )
            data$df <- addDiscData_assign_sample_locations(
              data$df,
              sample_keys = keys,
              location_id = new_location$location_id[[1]]
            )
            data$df$location_mapping_status[
              data$df$sample_key %in% keys
            ] <- "profile mapping saved"
          })

          locations(read_locations())
          update_location_selectize()
          updateSelectizeInput(
            session,
            "sample_location",
            choices = addDiscData_location_choices(
              locations(),
              include_blank = TRUE
            )
          )
          removeModal()
          showNotification(
            "Created the location and saved its source-code mapping.",
            type = "message"
          )
        },
        error = function(e) {
          showNotification(
            paste("Creating the location failed:", e$message),
            type = "error"
          )
        }
      )
    })

    map_location_target <- reactiveVal("default")
    map_location_selected <- reactiveVal(NA_integer_)

    show_location_map <- function(target) {
      map_location_target(target)
      selected <- if (identical(target, "sample")) {
        addDiscData_int(input$sample_location)
      } else {
        addDiscData_int(addDiscData_first(normalize_selectize_values(
          input$location
        )))
      }
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

    observeEvent(input$find_default_location_map, show_location_map("default"))
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
          "sample_location",
          selected = as.character(location_id)
        )
      } else {
        update_location_selectize(location_id)
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

        summary <- data.frame(
          `Source parameter` = df$source_parameter_code,
          `Source unit` = df$source_unit,
          `AquaCache parameter` = params()$param_name[parameter_index],
          `Target unit` = addDiscData_target_unit(
            params(),
            df$parameter_id,
            df$matrix_state_id
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
      label <- paste0(
        row$source_parameter_code[[1]],
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
        addDiscData_int(input$mapping_matrix_state, 1L)
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

    result_display <- reactive({
      addDiscData_result_display(
        rows = data$df,
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
        sample_types = sample_types
      )
    })

    output$data_table <- DT::renderDT(
      {
        display <- result_display()
        if (!nrow(display)) {
          return(DT::datatable(
            data.frame(
              Message = "Add or preview data to review mapped results."
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
          info$row > nrow(data$df)
      ) {
        return()
      }
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
      } else {
        new_value <- value
      }
      data$df[[source_column]][[info$row]] <- new_value
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

    validate_upload_rows <- function(df, group_mode) {
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
      if (any(requires_group) && identical(group_mode, "none")) {
        stop(
          "One or more samples require a sample group. Select an existing group or create a new one.",
          call. = FALSE
        )
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
      invisible(TRUE)
    }

    observeEvent(input$upload, {
      df <- data$df
      skipped <- which(df$result_flag_action == "skip_result")
      if (length(skipped)) {
        df <- df[-skipped, , drop = FALSE]
      }
      active <- FALSE
      tryCatch(
        {
          group_mode <- addDiscData_first(input$sample_group_mode, "none")
          validate_upload_rows(df, group_mode)

          if (
            !identical(group_mode, "none") &&
              !isTRUE(check_groups$can_assign_group[[1]])
          ) {
            stop(
              "You do not have permission to assign samples to groups.",
              call. = FALSE
            )
          }
          if (
            identical(group_mode, "new") &&
              !isTRUE(check_groups$can_create_group[[1]])
          ) {
            stop(
              "You do not have permission to create sample groups.",
              call. = FALSE
            )
          }

          if (identical(group_mode, "existing")) {
            group_id <- addDiscData_int(input$sample_group_id)
            if (
              is.na(group_id) || !(group_id %in% sample_groups$sample_group_id)
            ) {
              stop("Select an existing sample group.", call. = FALSE)
            }
          } else {
            group_id <- NA_integer_
          }
          if (identical(group_mode, "new")) {
            group_type <- addDiscData_first(input$sample_group_type)
            group_code <- trimws(addDiscData_first(input$sample_group_code, ""))
            group_name <- trimws(addDiscData_first(input$sample_group_name, ""))
            if (!addDiscData_present(group_type)) {
              stop("Select a sample group type.", call. = FALSE)
            }
            if (!nzchar(group_code) && !nzchar(group_name)) {
              stop("Enter a group code or group name.", call. = FALSE)
            }
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
            "media_id",
            "collection_method",
            "sample_type",
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

          if (identical(group_mode, "new")) {
            group_owners <- unique(as.integer(samples$owner))
            group_owners <- group_owners[!is.na(group_owners)]
            if (length(group_owners) != 1L) {
              stop(
                "All samples must have the same owner when creating a group.",
                call. = FALSE
              )
            }
            group_id <- DBI::dbGetQuery(
              con,
              "INSERT INTO discrete.sample_groups (
                 group_type, group_code, group_name, start_datetime,
                 end_datetime, owner, note, share_with
               ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8::text[])
               RETURNING sample_group_id",
              params = list(
                group_type,
                if (nzchar(group_code)) group_code else NA_character_,
                if (nzchar(group_name)) group_name else NA_character_,
                min(as.POSIXct(samples$datetime, tz = "UTC"), na.rm = TRUE),
                max(as.POSIXct(samples$datetime, tz = "UTC"), na.rm = TRUE),
                group_owners[[1]],
                if (isTruthy(input$sample_group_note)) {
                  input$sample_group_note
                } else {
                  NA_character_
                },
                "{public_reader}"
              )
            )$sample_group_id[[1]]
          }

          for (i in seq_len(nrow(samples))) {
            sid <- DBI::dbGetQuery(
              con,
              "INSERT INTO discrete.samples (
                 location_id,
                 sub_location_id,
                 media_id,
                  datetime,
                  collection_method,
                  sample_type,
                  owner,
                  contributor,
                  source_adapter_function,
                  external_sample_id,
                  import_source_id,
                  no_source_update,
                  note
                ) VALUES (
                  $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13
                )
               RETURNING sample_id",
              params = list(
                as.integer(samples$location_id[[i]]),
                addDiscData_int(samples$sub_location_id[[i]]),
                as.integer(samples$media_id[[i]]),
                as.POSIXct(samples$datetime[[i]], tz = "UTC"),
                as.integer(samples$collection_method[[i]]),
                as.integer(samples$sample_type[[i]]),
                as.integer(samples$owner[[i]]),
                addDiscData_int(samples$contributor[[i]]),
                if (file_sample[[i]]) "addDiscData" else NA_character_,
                if (file_sample[[i]]) {
                  samples$source_sample_id[[i]]
                } else {
                  NA_character_
                },
                addDiscData_int(samples$import_source_id[[i]]),
                isTRUE(samples$sample_no_source_update[[i]]),
                paste("Imported source sample:", samples$source_sample_id[[i]])
              )
            )$sample_id[[1]]
            link_sample_documents(sid, doc_ids)
            if (!is.na(group_id)) {
              DBI::dbExecute(
                con,
                "INSERT INTO discrete.sample_group_members (
                   sample_group_id, sample_id, sequence_in_group
                 ) VALUES ($1, $2, $3)",
                params = list(
                  as.integer(group_id),
                  as.integer(sid),
                  as.integer(i)
                )
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
                  $14, $15, $16, $17, $18
               )
               RETURNING result_id",
              params = list(
                sid,
                as.integer(df$result_type[[j]]),
                as.integer(df$parameter_id[[j]]),
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
              "media_id",
              "collection_method",
              "sample_type",
              "owner",
              "contributor",
              "sample_no_source_update"
            )
            result_columns <- c(
              "parameter_id",
              "result_type",
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
