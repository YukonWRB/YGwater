# UI and server code for adding discrete samples and results.

addDiscData_empty_table <- function() {
  data.frame(
    sample_key = character(),
    source_location_name = character(),
    location_id = integer(),
    sub_location_id = integer(),
    datetime = as.POSIXct(character(), tz = "UTC"),
    media_id = integer(),
    collection_method = integer(),
    sample_type = integer(),
    owner = integer(),
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
    source_result_condition_value = numeric(),
    result = numeric(),
    result_condition = integer(),
    result_condition_value = numeric(),
    conversion = numeric(),
    result_offset = numeric(),
    laboratory = integer(),
    grade_type_id = integer(),
    approval_type_id = integer(),
    analysis_datetime = as.POSIXct(character(), tz = "UTC"),
    note = character(),
    mapping_status = character(),
    source_code = character(),
    source_row_number = integer(),
    stringsAsFactors = FALSE
  )
}

addDiscData_builtin_profiles <- function() {
  defaults <- list(
    media_id = 1L,
    collection_method = 27L,
    sample_type = 34L,
    owner = 1L,
    result_type = 2L,
    matrix_state_id = 1L,
    result_value_type = 1L,
    laboratory = 2L
  )

  data.frame(
    import_profile_id = c(NA_integer_, NA_integer_, NA_integer_),
    source_code = c("ALS", "ALS", "ALS"),
    source_name = c("ALS Environmental", "ALS Environmental", "ALS Environmental"),
    profile_code = c(
      "als_eqwin_can_long",
      "als_samples_transposed",
      "als_xlr_detailed"
    ),
    profile_name = c(
      "ALS YUKON_YG_EQWIN_CAN long export",
      "ALS Samples transposed EDD",
      "ALS XLR Detailed Report"
    ),
    sheet_name = c("YUKON_YG_EQWIN_CAN", "Samples", "Detailed Report"),
    parser_type = c("long", "wide", "long"),
    timezone = c(
      "America/Whitehorse",
      "America/Whitehorse",
      "America/Whitehorse"
    ),
    stringsAsFactors = FALSE
  ) |>
    transform(
      defaults = I(replicate(3L, defaults, simplify = FALSE)),
      column_map = I(list(
        list(
          station_code = "Station_Code",
          sample_date = "Smpl_CollectDate",
          sample_time = "Smpl_CollectTime",
          parameter_code = "Lab_Param_Code",
          parameter_name = "Parameter Description",
          result = "Result",
          unit = "Units",
          result_comment = "Result_Comment",
          lab_mdl = "Lab_MDL",
          lab_rdl = "Meth_Rprt_Limit_(RDL)",
          lab_name = "Lab_Name",
          lab_report_no = "Lab_Rport_No",
          lab_sample_id = "Lab_Smpl_#",
          analysis_datetime = "Lab_Analy_Date-time"
        ),
        list(
          lab_report_row = 1L,
          lab_sample_row = 2L,
          station_code_row = 5L,
          sample_date_row = 6L,
          sample_time_row = 7L,
          matrix_row = 8L,
          lab_code_row = 9L,
          sample_class_row = 11L,
          sample_number_row = 12L,
          parameter_name_column = 1L,
          parameter_code_column = 2L,
          unit_column = 3L,
          first_sample_column = 4L,
          data_start_row = 15L
        ),
        list(
          parameter_name = "Analyte",
          lab_sample_id = "ALS Sample ID",
          station_code = "Client Sample ID",
          analytical_method_code = "Method",
          result = "Results",
          lab_rdl = "Detection Limit",
          unit = "Units",
          result_comment = "Qual",
          sample_date = "Date Sampled",
          sample_time = "Time Sampled",
          analysis_datetime = "Analysis Date"
        )
      ))
    )
}

addDiscData_read_profiles <- function(con) {
  available <- DBI::dbGetQuery(
    con,
    "SELECT to_regclass('discrete.import_profiles') IS NOT NULL AS available;"
  )$available[[1]]
  if (!isTRUE(available)) {
    return(addDiscData_builtin_profiles())
  }

  profiles <- DBI::dbGetQuery(
    con,
    "SELECT
       p.import_profile_id,
       s.source_code,
       s.source_name,
       p.profile_code,
       p.profile_name,
       p.profile_description,
       p.file_type,
       p.sheet_name,
       p.parser_type,
       p.sheet_strategy,
       p.sheet_index,
       p.header_row,
       p.units_row,
       p.parameter_row,
       p.data_start_row,
       p.datetime_origin,
       p.timezone,
       p.column_map::text AS column_map,
       p.wide_config::text AS wide_config,
       p.defaults::text AS defaults,
       p.sample_identity::text AS sample_identity,
       p.result_identity::text AS result_identity,
       p.validation_rules::text AS validation_rules,
       p.note
     FROM discrete.import_profiles p
     JOIN discrete.import_sources s
       ON s.import_source_id = p.import_source_id
     WHERE p.active
     ORDER BY s.source_code, p.profile_name;"
  )
  if (!nrow(profiles)) {
    return(addDiscData_builtin_profiles())
  }
  for (column in c(
    "column_map",
    "wide_config",
    "defaults",
    "sample_identity",
    "result_identity",
    "validation_rules"
  )) {
    profiles[[column]] <- lapply(
      profiles[[column]],
      jsonlite::fromJSON,
      simplifyVector = FALSE
    )
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
    return(as.Date(x))
  }
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }
  x <- trimws(as.character(x))
  out <- as.Date(rep(NA_character_, length(x)))
  formats <- c(
    "%Y-%m-%d",
    "%Y-%b-%d",
    "%Y/%b/%d",
    "%d-%b-%Y",
    "%d-%B-%Y",
    "%m/%d/%Y",
    "%d/%m/%Y"
  )
  for (fmt in formats) {
    missing <- is.na(out) & addDiscData_present(x)
    if (!any(missing)) {
      break
    }
    out[missing] <- suppressWarnings(as.Date(x[missing], format = fmt))
  }
  out
}

addDiscData_datetime <- function(date, time = NA, tz = "America/Whitehorse") {
  if (inherits(date, "POSIXt")) {
    return(as.POSIXct(date, tz = "UTC"))
  }
  parsed_date <- addDiscData_as_date(date)
  if (all(is.na(parsed_date))) {
    return(as.POSIXct(rep(NA_real_, length(parsed_date)), origin = "1970-01-01", tz = "UTC"))
  }
  time <- trimws(as.character(time))
  time[!addDiscData_present(time)] <- "00:00"
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
  as.POSIXct(parsed, tz = "UTC")
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
    result_type = addDiscData_int(defaults$result_type, 2L),
    matrix_state_id = addDiscData_int(defaults$matrix_state_id, 1L),
    result_value_type = addDiscData_int(defaults$result_value_type, 1L),
    laboratory = addDiscData_int(defaults$laboratory, 2L)
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
    location_id = NA_integer_,
    sub_location_id = NA_integer_,
    datetime = datetime,
    media_id = defaults$media_id,
    collection_method = defaults$collection_method,
    sample_type = defaults$sample_type,
    owner = defaults$owner,
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
    source_result_condition_value = parsed_result$result_condition_value,
    result = parsed_result$result,
    result_condition = parsed_result$result_condition,
    result_condition_value = parsed_result$result_condition_value,
    conversion = 1,
    result_offset = 0,
    laboratory = defaults$laboratory,
    grade_type_id = NA_integer_,
    approval_type_id = NA_integer_,
    analysis_datetime = analysis_datetime,
    note = as.character(note),
    mapping_status = "unmapped",
    stringsAsFactors = FALSE
  )
  out$source_code <- source_code
  out$source_row_number <- source_row_number
  out
}

addDiscData_parse_als_eqwin <- function(path, profile) {
  cmap <- profile$column_map[[1]]
  sheet <- profile$sheet_name[[1]]
  x <- openxlsx::read.xlsx(path, sheet = sheet, colNames = TRUE)
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
    note = addDiscData_col(x, cmap$result_comment),
    analysis_datetime = addDiscData_col(x, cmap$analysis_datetime),
    source_row_number = seq_len(nrow(x)) + 1L
  )
  out[addDiscData_present(out$source_parameter_code), , drop = FALSE]
}

addDiscData_parse_als_samples <- function(path, profile) {
  cmap <- profile$column_map[[1]]
  sheet <- profile$sheet_name[[1]]
  x <- openxlsx::read.xlsx(path, sheet = sheet, colNames = FALSE)

  find_row <- function(label, fallback) {
    label <- tolower(label)
    search_cols <- seq_len(min(5L, ncol(x)))
    hit <- which(vapply(seq_len(nrow(x)), function(i) {
      any(tolower(trimws(as.character(unlist(x[i, search_cols])))) == label)
    }, logical(1)))
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

  header_row <- find_row("Parameter Code", addDiscData_int(cmap$data_start_row, 16L) - 1L)
  first_sample_col <- addDiscData_int(cmap$first_sample_column, 4L)
  data_start_row <- header_row + 1L
  parameter_name_col <- find_col(header_row, "Parameter Name", cmap$parameter_name_column)
  parameter_code_col <- find_col(header_row, "Parameter Code", cmap$parameter_code_column)
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
      if (!addDiscData_present(result_raw) || !addDiscData_present(parameter_code)) {
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
  sheet <- profile$sheet_name[[1]]
  raw <- openxlsx::read.xlsx(path, sheet = sheet, colNames = FALSE)
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
  x <- x[addDiscData_present(addDiscData_col(x, cmap$lab_sample_id)), , drop = FALSE]
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
    note = addDiscData_col(x, cmap$result_comment),
    analysis_datetime = addDiscData_col(x, cmap$analysis_datetime),
    source_row_number = seq_len(nrow(x)) + header_row
  )
  out[addDiscData_present(out$source_parameter_code), , drop = FALSE]
}

addDiscData_parse_upload <- function(path, profile) {
  code <- profile$profile_code[[1]]
  cmap <- profile$column_map[[1]]
  parser_type <- addDiscData_profile_value(profile, "parser_type", "long")
  parser_family <- if (identical(code, "als_eqwin_can_long")) {
    "long"
  } else if (identical(code, "als_samples_transposed")) {
    "transposed"
  } else if (identical(code, "als_xlr_detailed")) {
    "xlr"
  } else if (
    identical(parser_type, "wide") ||
      any(c("first_sample_column", "parameter_code_column") %in% names(cmap))
  ) {
    "transposed"
  } else if ("parameter_code" %in% names(cmap)) {
    "long"
  } else if (all(c("parameter_name", "lab_sample_id", "result") %in% names(cmap))) {
    "xlr"
  } else {
    NA_character_
  }

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
  vapply(seq_len(nrow(locations)), function(i) {
    details <- c(
      if (nzchar(code[[i]])) paste0("Code: ", code[[i]]),
      if (nzchar(alias[[i]])) paste0("Alias: ", alias[[i]])
    )
    label <- if (nzchar(name[[i]])) name[[i]] else paste0("Location ", locations$location_id[[i]])
    if (length(details)) paste(label, paste(details, collapse = " | "), sep = " | ") else label
  }, character(1))
}

addDiscData_location_choices <- function(locations, include_blank = FALSE) {
  choices <- stats::setNames(
    as.character(locations$location_id),
    addDiscData_location_labels(locations)
  )
  if (isTRUE(include_blank)) c("Select a location" = "", choices) else choices
}

addDiscData_location_match <- function(rows, locations, selected_location = NULL) {
  if (!nrow(rows)) {
    return(rows)
  }
  match_columns <- intersect(c("name", "location_code", "alias"), names(locations))
  location_values <- lapply(
    locations[match_columns],
    function(x) tolower(addDiscData_clean_location_text(x))
  )
  source_names <- tolower(addDiscData_clean_location_text(rows$source_location_name))

  rows$location_id <- NA_integer_
  for (i in seq_along(source_names)) {
    if (!nzchar(source_names[[i]])) {
      next
    }
    hit <- unique(unlist(lapply(location_values, function(values) {
      which(values == source_names[[i]])
    })))
    if (length(hit) == 1L) {
      rows$location_id[[i]] <- locations$location_id[[hit[[1]]]]
    }
  }
  if (!is.null(selected_location) && length(selected_location)) {
    fallback <- addDiscData_int(selected_location[[1]])
    rows$location_id[is.na(rows$location_id)] <- fallback
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

addDiscData_fetch_mappings <- function(con, source_code) {
  available <- DBI::dbGetQuery(
    con,
    "SELECT to_regclass('discrete.import_parameter_mappings') IS NOT NULL AS available;"
  )$available[[1]]
  if (!isTRUE(available) || !addDiscData_present(source_code)) {
    return(data.frame())
  }

  DBI::dbGetQuery(
    con,
    "SELECT
       s.source_code,
       m.source_match::text AS source_match,
       m.parameter_id,
       m.result_type,
       m.sample_fraction_id,
       m.result_value_type,
       m.result_speciation_id,
       m.matrix_state_id,
       m.conversion,
       m.result_offset,
       m.priority,
       m.import_mapping_id
     FROM discrete.import_parameter_mappings m
     JOIN discrete.import_sources s
       ON s.import_source_id = m.import_source_id
     WHERE s.source_code = $1
       AND m.active
     ORDER BY m.priority DESC, m.import_mapping_id;",
    params = list(source_code)
  )
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

addDiscData_apply_mappings <- function(rows, con) {
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
  rows$conversion[file_rows] <- 1
  rows$result_offset[file_rows] <- 0

  for (source_code in unique(rows$source_code[file_rows])) {
    mappings <- addDiscData_fetch_mappings(con, source_code)
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
      rows$result_type[[i]] <- addDiscData_int(hit$result_type[[1]], rows$result_type[[i]])
      rows$sample_fraction_id[[i]] <- addDiscData_int(hit$sample_fraction_id[[1]])
      rows$result_value_type[[i]] <- addDiscData_int(hit$result_value_type[[1]], rows$result_value_type[[i]])
      rows$result_speciation_id[[i]] <- addDiscData_int(hit$result_speciation_id[[1]])
      rows$matrix_state_id[[i]] <- addDiscData_int(hit$matrix_state_id[[1]], rows$matrix_state_id[[i]])
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
  note
) {
  source_id <- DBI::dbGetQuery(
    con,
    "INSERT INTO discrete.import_sources
       (source_code, source_name, source_description, active)
     VALUES ($1, $2, $3, TRUE)
     ON CONFLICT (source_code) DO UPDATE
     SET active = TRUE
     RETURNING import_source_id;",
    params = list(source_code, source_name, "Created from YGwater add discrete data.")
  )$import_source_id[[1]]
  source_match <- jsonlite::toJSON(
    list(parameter_code = parameter_code, unit = unit),
    auto_unbox = TRUE,
    null = "null"
  )
  DBI::dbExecute(
    con,
    "INSERT INTO discrete.import_parameter_mappings (
       import_source_id,
       source_match,
       parameter_id,
       result_type,
       sample_fraction_id,
       result_value_type,
       result_speciation_id,
       matrix_state_id,
       conversion,
       result_offset,
       priority,
       active,
       note
    ) VALUES (
      $1, $2::jsonb, $3, $4, $5, $6, $7, $8, $9, $10, 50, TRUE, $11
     )
     ON CONFLICT (import_source_id, source_match) DO UPDATE
     SET parameter_id = EXCLUDED.parameter_id,
         result_type = EXCLUDED.result_type,
         sample_fraction_id = EXCLUDED.sample_fraction_id,
         result_value_type = EXCLUDED.result_value_type,
         result_speciation_id = EXCLUDED.result_speciation_id,
         matrix_state_id = EXCLUDED.matrix_state_id,
         conversion = EXCLUDED.conversion,
         result_offset = EXCLUDED.result_offset,
         active = TRUE,
         note = EXCLUDED.note;",
    params = list(
      source_id,
      source_match,
      parameter_id,
      result_type,
      sample_fraction_id,
      result_value_type,
      result_speciation_id,
      matrix_state_id,
      conversion,
      result_offset,
      note
    )
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
    if (!is.na(row) && length(unit_column) && unit_column %in% names(parameters)) {
      out[[i]] <- as.character(parameters[[unit_column]][[row]])
    }
  }
  out[!addDiscData_present(out)] <- NA_character_
  out
}

addDiscData_parameter_choices <- function(parameters) {
  labels <- vapply(seq_len(nrow(parameters)), function(i) {
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
    paste0(parameters$param_name[[i]], " [", paste(unit_labels, collapse = "; "), "]")
  }, character(1))

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
  source_result[missing_source_result & !is.na(rows$source_result)] <- as.character(
    rows$source_result[missing_source_result & !is.na(rows$source_result)]
  )
  condition_prefix <- c("1" = "<", "2" = ">")
  rebuild <- missing_source_result & !is.na(rows$source_result_condition_value)
  if (any(rebuild)) {
    prefix <- unname(condition_prefix[as.character(rows$result_condition[rebuild])])
    prefix[is.na(prefix)] <- ""
    source_result[rebuild] <- paste0(prefix, rows$source_result_condition_value[rebuild])
  }

  location_index <- match(rows$location_id, locations$location_id)
  parameter_name <- addDiscData_lookup_label(
    rows$parameter_id,
    parameters,
    "parameter_id",
    "param_name"
  )
  parameter_name[!addDiscData_present(parameter_name)] <- rows$source_parameter_name[
    !addDiscData_present(parameter_name)
  ]
  out <- data.frame(
    `Source sample` = rows$source_sample_id,
    `Source location` = rows$source_location_name,
    Location = addDiscData_location_labels(locations)[location_index],
    `Sub-location` = addDiscData_lookup_label(
      rows$sub_location_id,
      sub_locations,
      "sub_location_id",
      "sub_location_name"
    ),
    `Sample datetime (UTC)` = format(rows$datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    Parameter = parameter_name,
    `Source parameter` = rows$source_parameter_code,
    `Source unit` = rows$source_unit,
    `Source result` = source_result,
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
    Media = addDiscData_lookup_label(rows$media_id, media, "media_id", "media_type"),
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
          title = "Sample defaults",
          fluidRow(
            column(
              5,
              selectizeInput(
                ns("location"),
                "Default location",
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
                "Default sub-location",
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
            column(3, selectizeInput(ns("sample_type"), "Sample type", choices = NULL)),
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
            "Manual/default sample datetime",
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
            choices = c("None" = "none", "Existing" = "existing", "Create new" = "new"),
            selected = "none",
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.sample_group_mode == 'existing'",
            ns = ns,
            selectizeInput(ns("sample_group_id"), "Existing sample group", choices = NULL)
          ),
          conditionalPanel(
            condition = "input.sample_group_mode == 'new'",
            ns = ns,
            fluidRow(
              column(4, selectizeInput(ns("sample_group_type"), "Group type", choices = NULL)),
              column(4, textInput(ns("sample_group_code"), "Group code")),
              column(4, textInput(ns("sample_group_name"), "Group name"))
            ),
            textAreaInput(ns("sample_group_note"), "Group notes", width = "100%")
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
              column(9, selectizeInput(ns("import_profile"), "Import profile", choices = NULL)),
              column(
                3,
                tags$div(
                  style = "padding-top: 25px;",
                  actionButton(ns("new_import_profile"), "Create from selected")
                )
              )
            ),
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
              column(5, selectizeInput(ns("manual_parameter"), "Parameter", choices = NULL)),
              column(3, textInput(ns("manual_result"), "Result")),
              column(2, actionButton(ns("add_manual_result"), "Add result")),
              column(2, actionButton(ns("new_manual_sample"), "New sample"))
            )
          ),
          tags$hr(),
          tags$h5("Sample locations"),
          helpText(
            "Each row represents one sample. Select one or more samples, choose a location, and apply it. Location searches include name, code, and alias."
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
                style = "padding-top: 25px;",
                actionButton(ns("apply_sample_location"), "Apply to selected"),
                actionButton(ns("clear_sample_location"), "Clear")
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
    import_profiles <- reactiveVal(data.frame())

    con <- session$userData$AquaCache
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
    locations <- DBI::dbGetQuery(
      con,
      "SELECT location_id, location_code, name, alias, latitude, longitude
         FROM public.locations
        ORDER BY name, location_code"
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
        choices = addDiscData_location_choices(locations)
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
      choices = addDiscData_location_choices(locations, include_blank = TRUE)
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
      choices = stats::setNames(sample_types$sample_type_id, sample_types$sample_type),
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
        nzchar(ifelse(is.na(sample_groups$group_code), "", sample_groups$group_code)),
        sample_groups$group_code,
        sample_groups$group_name
      )
    )
    updateSelectizeInput(
      session,
      "sample_group_id",
      choices = stats::setNames(sample_groups$sample_group_id, sample_group_labels)
    )

    observeEvent(
      input$timezone,
      {
        shift_air_datetime_input_timezone(session, input, "sample_datetime", input$timezone)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$location,
      {
        resolved <- resolve_selectize_lookup_values(
          input$location,
          locations$location_id,
          locations$name
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

    reload_profiles <- function() {
      profiles <- addDiscData_read_profiles(con)
      profiles$profile_key <- addDiscData_profile_key(
        profiles$source_code,
        profiles$profile_code
      )
      import_profiles(profiles)
      updateSelectizeInput(
        session,
        "import_profile",
        choices = stats::setNames(
          profiles$profile_key,
          paste(profiles$source_code, profiles$profile_name, sep = " - ")
        ),
        selected = profiles$profile_key[[1]]
      )
    }
    reload_profiles()

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
      hit <- profiles[profiles$profile_key == input$import_profile, , drop = FALSE]
      validate(need(nrow(hit) == 1L, "Select an import profile."))
      hit
    })

    observeEvent(input$new_import_profile, {
      profile <- selected_profile()
      suggested_code <- paste0(profile$profile_code[[1]], "_copy")
      showModal(modalDialog(
        title = "Create import profile from selected",
        helpText(
          "The selected profile supplies the parser layout and defaults. Change the source columns in Column map JSON to match the new file."
        ),
        fluidRow(
          column(
            4,
            textInput(
              ns("new_profile_source_code"),
              "Source code",
              value = profile$source_code[[1]]
            )
          ),
          column(
            8,
            textInput(
              ns("new_profile_source_name"),
              "Source name",
              value = profile$source_name[[1]]
            )
          )
        ),
        fluidRow(
          column(
            4,
            textInput(ns("new_profile_code"), "Profile code", value = suggested_code)
          ),
          column(
            8,
            textInput(
              ns("new_profile_name"),
              "Profile name",
              value = paste(profile$profile_name[[1]], "copy")
            )
          )
        ),
        fluidRow(
          column(
            6,
            textInput(
              ns("new_profile_sheet"),
              "Worksheet name",
              value = addDiscData_profile_value(profile, "sheet_name", "")
            )
          ),
          column(
            6,
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
        textAreaInput(
          ns("new_profile_column_map"),
          "Column map JSON",
          value = addDiscData_profile_json(profile, "column_map"),
          width = "100%",
          height = "260px"
        ),
        textAreaInput(
          ns("new_profile_defaults"),
          "Defaults JSON",
          value = addDiscData_profile_json(profile, "defaults"),
          width = "100%",
          height = "180px"
        ),
        textAreaInput(
          ns("new_profile_description"),
          "Description",
          value = ""
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
          profile <- selected_profile()
          source_code <- toupper(trimws(addDiscData_first(input$new_profile_source_code, "")))
          source_name <- trimws(addDiscData_first(input$new_profile_source_name, ""))
          profile_code <- tolower(trimws(addDiscData_first(input$new_profile_code, "")))
          profile_name <- trimws(addDiscData_first(input$new_profile_name, ""))
          if (!grepl("^[a-z0-9][a-z0-9_]*$", profile_code)) {
            stop("Profile code must contain only lowercase letters, numbers, and underscores.")
          }
          if (!nzchar(source_code) || !nzchar(source_name) || !nzchar(profile_name)) {
            stop("Source code, source name, and profile name are required.")
          }
          new_profile_key <- addDiscData_profile_key(source_code, profile_code)
          if (new_profile_key %in% import_profiles()$profile_key) {
            stop("That source already has a profile with this profile code.")
          }
          parse_json_object <- function(value, label) {
            parsed <- jsonlite::fromJSON(value, simplifyVector = FALSE)
            if (!is.list(parsed) || is.null(names(parsed))) {
              stop(label, " must be a JSON object.")
            }
            parsed
          }
          column_map <- parse_json_object(input$new_profile_column_map, "Column map")
          defaults <- parse_json_object(input$new_profile_defaults, "Defaults")
          profile_id <- DBI::dbWithTransaction(con, {
            AquaCache::upsertImportProfile(
              con = con,
              source_code = source_code,
              source_name = source_name,
              source_description = addDiscData_profile_value(profile, "source_description"),
              profile_code = profile_code,
              profile_name = profile_name,
              profile_description = trimws(addDiscData_first(input$new_profile_description, "")),
              file_type = addDiscData_profile_value(profile, "file_type", "xlsx"),
              parser_type = addDiscData_profile_value(profile, "parser_type", "long"),
              sheet_strategy = addDiscData_profile_value(
                profile,
                "sheet_strategy",
                "name_or_first"
              ),
              sheet_name = trimws(addDiscData_first(input$new_profile_sheet, "")),
              sheet_index = addDiscData_profile_value(profile, "sheet_index"),
              header_row = addDiscData_profile_value(profile, "header_row", 1L),
              units_row = addDiscData_profile_value(profile, "units_row"),
              parameter_row = addDiscData_profile_value(profile, "parameter_row"),
              data_start_row = addDiscData_profile_value(profile, "data_start_row", 2L),
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
              wide_config = addDiscData_profile_value(profile, "wide_config", list()),
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
              validation_rules = addDiscData_profile_value(
                profile,
                "validation_rules",
                list()
              ),
              note = "Created from YGwater add discrete data."
            )
          })
          reload_profiles()
          updateSelectizeInput(
            session,
            "import_profile",
            selected = new_profile_key
          )
          removeModal()
          showNotification(
            sprintf("Created import profile %s.", profile_id),
            type = "message"
          )
        },
        error = function(e) {
          showNotification(paste("Creating profile failed:", e$message), type = "error")
        }
      )
    })

    observeEvent(input$preview_file, {
      req(input$file)
      tryCatch(
        {
          parsed <- addDiscData_parse_upload(input$file$datapath, selected_profile())
          parsed <- addDiscData_location_match(
            parsed,
            locations,
            selected_location = normalize_selectize_values(input$location)
          )
          parsed <- addDiscData_apply_mappings(parsed, con)
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
      row$location_id <- addDiscData_int(addDiscData_first(normalize_selectize_values(input$location)))
      row$sub_location_id <- addDiscData_int(addDiscData_first(normalize_selectize_values(input$sublocation)))
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
      row$source_result_condition_value <- parsed_result$result_condition_value[[1]]
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

    output$sample_location_summary <- DT::renderDT({
      df <- sample_location_rows()
      if (!nrow(df)) {
        return(DT::datatable(
          data.frame(Message = "Add or preview data to assign sample locations."),
          rownames = FALSE,
          selection = "none",
          options = list(dom = "t")
        ))
      }
      location_index <- match(df$location_id, locations$location_id)
      sub_location_index <- match(df$sub_location_id, sub_locations$sub_location_id)
      summary <- data.frame(
        `Source sample` = df$source_sample_id,
        `Source location` = df$source_location_name,
        `Sample datetime` = format(df$datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
        Location = addDiscData_location_labels(locations)[location_index],
        `Sub-location` = sub_locations$sub_location_name[sub_location_index],
        check.names = FALSE
      )
      summary[is.na(summary)] <- ""
      DT::datatable(
        summary,
        rownames = FALSE,
        selection = list(mode = "multiple", target = "row"),
        options = list(scrollX = TRUE, pageLength = 10)
      )
    }, server = FALSE)

    observeEvent(input$sample_location, {
      location_id <- addDiscData_int(input$sample_location)
      available <- sub_locations[!is.na(location_id) & sub_locations$location_id == location_id, ]
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
    }, ignoreInit = TRUE)

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
          showNotification("The sub-location does not belong to that location.", type = "error")
          return()
        }
      }
      data$df <- addDiscData_assign_sample_locations(
        data$df,
        sample_keys = keys,
        location_id = location_id,
        sub_location_id = sub_location_id
      )
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
    })

    map_location_target <- reactiveVal("default")
    map_location_selected <- reactiveVal(NA_integer_)

    show_location_map <- function(target) {
      map_location_target(target)
      selected <- if (identical(target, "sample")) {
        addDiscData_int(input$sample_location)
      } else {
        addDiscData_int(addDiscData_first(normalize_selectize_values(input$location)))
      }
      map_location_selected(selected)
      showModal(modalDialog(
        title = "Find a location",
        selectizeInput(
          ns("map_location_search"),
          "Search by location name, code, or alias",
          choices = addDiscData_location_choices(locations, include_blank = TRUE),
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
      mapped <- locations[
        is.finite(locations$latitude) & is.finite(locations$longitude),
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
        map |> leaflet::setView(mapped$longitude[[1]], mapped$latitude[[1]], zoom = 11)
      } else {
        map |> leaflet::fitBounds(
          min(mapped$longitude),
          min(mapped$latitude),
          max(mapped$longitude),
          max(mapped$latitude)
        )
      }
    })

    observeEvent(input$map_location_search, {
      location_id <- addDiscData_int(input$map_location_search)
      map_location_selected(location_id)
      row <- match(location_id, locations$location_id)
      if (!is.na(row) && is.finite(locations$latitude[[row]]) && is.finite(locations$longitude[[row]])) {
        leaflet::leafletProxy("location_search_map", session = session) |>
          leaflet::setView(
            lng = locations$longitude[[row]],
            lat = locations$latitude[[row]],
            zoom = 12
          )
      }
    }, ignoreInit = TRUE)

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
      row <- match(map_location_selected(), locations$location_id)
      if (is.na(row)) "No location selected." else addDiscData_location_labels(locations)[[row]]
    })

    observeEvent(input$use_map_location, {
      location_id <- map_location_selected()
      if (is.na(location_id)) {
        showNotification("Select a location on the map first.", type = "warning")
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
      key <- paste(df$source_code, df$source_parameter_code, df$source_unit, sep = "\r")
      out <- df[!duplicated(key), , drop = FALSE]
      out[order(out$source_parameter_code, out$source_unit), , drop = FALSE]
    })

    output$mapping_summary <- DT::renderDT({
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
      fraction_index <- match(df$sample_fraction_id, sample_fractions$sample_fraction_id)
      result_type_index <- match(df$result_type, result_types$result_type_id)
      value_type_index <- match(df$result_value_type, result_value_types$result_value_type_id)
      speciation_index <- match(df$result_speciation_id, result_speciations$result_speciation_id)
      matrix_index <- match(df$matrix_state_id, matrix_states$matrix_state_id)

      summary <- data.frame(
        `Source parameter` = df$source_parameter_code,
        `Source unit` = df$source_unit,
        `AquaCache parameter` = params()$param_name[parameter_index],
        `Target unit` = addDiscData_target_unit(params(), df$parameter_id, df$matrix_state_id),
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
    }, server = FALSE)

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
        return(tags$div(class = "text-muted", "Select a mapping-summary row to edit."))
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
          column(4, tags$strong(textOutput(ns("mapping_target_unit"), inline = TRUE)))
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
        showNotification("Select a mapping-summary row first.", type = "warning")
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
          DBI::dbWithTransaction(con, {
            addDiscData_upsert_mapping(
              con = con,
              source_code = row$source_code[[1]],
              source_name = row$source_code[[1]],
              parameter_code = row$source_parameter_code[[1]],
              unit = row$source_unit[[1]],
              parameter_id = parameter_id,
              result_type = addDiscData_int(input$mapping_result_type, 2L),
              sample_fraction_id = addDiscData_int(input$mapping_sample_fraction),
              result_value_type = addDiscData_int(input$mapping_result_value_type, 1L),
              result_speciation_id = addDiscData_int(input$mapping_result_speciation),
              matrix_state_id = addDiscData_int(input$mapping_matrix_state, 1L),
              conversion = conversion,
              result_offset = result_offset,
              note = "Saved from YGwater add discrete data mapping editor."
            )
          })
          data$df <- addDiscData_apply_mappings(data$df, con)[names(addDiscData_empty_table())]
          showNotification("Saved mapping.", type = "message")
        },
        error = function(e) {
          showNotification(paste("Saving mappings failed:", e$message), type = "error")
        }
      )
    })

    result_display <- reactive({
      addDiscData_result_display(
        rows = data$df,
        locations = locations,
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

    output$data_table <- DT::renderDT({
      display <- result_display()
      if (!nrow(display)) {
        return(DT::datatable(
          data.frame(Message = "Add or preview data to review mapped results."),
          rownames = FALSE,
          selection = "none",
          options = list(dom = "t")
        ))
      }
      editable_columns <- match(
        names(addDiscData_result_edit_columns()),
        names(display)
      ) - 1L
      disabled_columns <- setdiff(seq_len(ncol(display)) - 1L, editable_columns)
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
    }, server = FALSE)

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
      if (!length(source_column) || is.na(source_column) || info$row > nrow(data$df)) {
        return()
      }
      value <- trimws(as.character(info$value))
      if (identical(source_column, "result_condition")) {
        if (!nzchar(value)) {
          new_value <- NA_integer_
        } else {
          hit <- which(tolower(result_conditions$result_condition) == tolower(value))
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
        new_value <- if (nzchar(value)) suppressWarnings(as.numeric(value)) else NA_real_
        if (nzchar(value) && is.na(new_value)) {
          showNotification("Enter a numeric value or leave the cell blank.", type = "error")
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
          paste(which(missing_location & !is.na(df$sub_location_id)), collapse = ", "),
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
            if (is.na(group_id) || !(group_id %in% sample_groups$sample_group_id)) {
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
            "source_sample_id",
            "source_code"
          )])

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
                  import_source,
                  import_source_id,
                  note
                ) VALUES (
                  $1, $2, $3, $4, $5, $6, $7, $8, $9, $10
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
                samples$source_code[[i]],
                samples$source_sample_id[[i]],
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
                params = list(as.integer(group_id), as.integer(sid), as.integer(i))
              )
            }
            sample_lookup[[samples$sample_key[[i]]]] <- sid
            inserted_samples <- inserted_samples + 1L
          }

          for (j in seq_len(nrow(df))) {
            sid <- sample_lookup[[df$sample_key[[j]]]]
            DBI::dbExecute(
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
                 note
               ) VALUES (
                 $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13,
                 $14, $15, $16, $17
               )",
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
                df$note[[j]]
              )
            )
            inserted_results <- inserted_results + 1L
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
