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
    source_parameter_code = character(),
    source_parameter_name = character(),
    source_unit = character(),
    parameter_id = integer(),
    result_type = integer(),
    matrix_state_id = integer(),
    sample_fraction_id = integer(),
    result_value_type = integer(),
    result_speciation_id = integer(),
    result = numeric(),
    result_condition = integer(),
    result_condition_value = numeric(),
    laboratory = integer(),
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
       p.sheet_name,
       p.parser_type,
       p.timezone,
       p.column_map::text AS column_map,
       p.defaults::text AS defaults
     FROM discrete.import_profiles p
     JOIN discrete.import_sources s
       ON s.import_source_id = p.import_source_id
     WHERE p.active
     ORDER BY s.source_code, p.profile_name;"
  )
  if (!nrow(profiles)) {
    return(addDiscData_builtin_profiles())
  }
  profiles$column_map <- lapply(
    profiles$column_map,
    jsonlite::fromJSON,
    simplifyVector = FALSE
  )
  profiles$defaults <- lapply(
    profiles$defaults,
    jsonlite::fromJSON,
    simplifyVector = FALSE
  )
  profiles
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
    source_parameter_code = as.character(source_parameter_code),
    source_parameter_name = as.character(source_parameter_name),
    source_unit = as.character(source_unit),
    parameter_id = NA_integer_,
    result_type = defaults$result_type,
    matrix_state_id = defaults$matrix_state_id,
    sample_fraction_id = NA_integer_,
    result_value_type = defaults$result_value_type,
    result_speciation_id = NA_integer_,
    result = parsed_result$result,
    result_condition = parsed_result$result_condition,
    result_condition_value = parsed_result$result_condition_value,
    laboratory = defaults$laboratory,
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
  x <- x[!grepl("\\(Matrix:", addDiscData_col(x, cmap$parameter_name)), , drop = FALSE]

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
  if (identical(code, "als_eqwin_can_long")) {
    return(addDiscData_parse_als_eqwin(path, profile))
  }
  if (identical(code, "als_samples_transposed")) {
    return(addDiscData_parse_als_samples(path, profile))
  }
  if (identical(code, "als_xlr_detailed")) {
    return(addDiscData_parse_als_xlr(path, profile))
  }
  stop("Unsupported import profile: ", code, call. = FALSE)
}

addDiscData_location_match <- function(rows, locations, selected_location = NULL) {
  if (!nrow(rows)) {
    return(rows)
  }
  loc_names <- tolower(trimws(locations$name))
  loc_alias <- tolower(trimws(if ("alias" %in% names(locations)) locations$alias else NA))
  source_names <- tolower(trimws(rows$source_location_name))

  rows$location_id <- NA_integer_
  for (i in seq_along(source_names)) {
    hit <- which(loc_names == source_names[[i]] | loc_alias == source_names[[i]])
    if (length(hit)) {
      rows$location_id[[i]] <- locations$location_id[[hit[[1]]]]
    }
  }
  if (!is.null(selected_location) && length(selected_location)) {
    fallback <- addDiscData_int(selected_location[[1]])
    rows$location_id[is.na(rows$location_id)] <- fallback
  }
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
       m.result_offset
     FROM discrete.import_parameter_mappings m
     JOIN discrete.import_sources s
       ON s.import_source_id = m.import_source_id
     WHERE s.source_code = $1
       AND m.active;",
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
  code <- trimws(as.character(code))
  unit <- trimws(as.character(unit))
  c(
    paste(tolower(code), tolower(unit), sep = "\r"),
    paste(tolower(code), "", sep = "\r")
  )
}

addDiscData_apply_mappings <- function(rows, con) {
  if (!nrow(rows) || !("source_code" %in% names(rows))) {
    return(rows)
  }
  rows$mapping_status <- "manual"
  file_rows <- addDiscData_present(rows$source_code)
  rows$mapping_status[file_rows] <- "unmapped"

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
        mapping_list[[key]] <- mappings[i, , drop = FALSE]
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
      rows$result[[i]] <- rows$result[[i]] * addDiscData_num(hit$conversion[[1]], 1)
      rows$result_condition_value[[i]] <- rows$result_condition_value[[i]] * addDiscData_num(hit$conversion[[1]], 1)
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
  note
) {
  source_id <- DBI::dbGetQuery(
    con,
    "INSERT INTO discrete.import_sources
       (source_code, source_name, source_description, active)
     VALUES ($1, $2, $3, TRUE)
     ON CONFLICT (source_code) DO UPDATE
     SET source_name = EXCLUDED.source_name,
         active = TRUE
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
       $1, $2::jsonb, $3, $4, $5, $6, $7, $8, 1, 0, 50, TRUE, $9
     )
     ON CONFLICT (import_source_id, source_match) DO UPDATE
     SET parameter_id = EXCLUDED.parameter_id,
         result_type = EXCLUDED.result_type,
         sample_fraction_id = EXCLUDED.sample_fraction_id,
         result_value_type = EXCLUDED.result_value_type,
         result_speciation_id = EXCLUDED.result_speciation_id,
         matrix_state_id = EXCLUDED.matrix_state_id,
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
      note
    )
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
              6,
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
            selectizeInput(ns("import_profile"), "Import profile", choices = NULL),
            actionButton(ns("preview_file"), "Preview file"),
            checkboxInput(
              ns("show_all_mappings"),
              "Show mapped parameters in mapping editor",
              value = FALSE
            ),
            uiOutput(ns("mapping_editor")),
            actionButton(ns("save_parameter_mappings"), "Save parameter mappings")
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
        "SELECT parameter_id, param_name FROM public.parameters ORDER BY param_name"
      )
    })
    locations <- DBI::dbGetQuery(
      con,
      "SELECT location_id, name, alias FROM public.locations ORDER BY name"
    )
    sub_locations <- DBI::dbGetQuery(
      con,
      "SELECT sub_location_id, sub_location_name FROM public.sub_locations ORDER BY sub_location_name"
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
        choices = stats::setNames(locations$location_id, locations$name)
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
      import_profiles(profiles)
      updateSelectizeInput(
        session,
        "import_profile",
        choices = stats::setNames(
          profiles$profile_code,
          paste(profiles$source_code, profiles$profile_name, sep = " - ")
        ),
        selected = profiles$profile_code[[1]]
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
      hit <- profiles[profiles$profile_code == input$import_profile, , drop = FALSE]
      validate(need(nrow(hit) == 1L, "Select an import profile."))
      hit
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
      row$result <- parsed_result$result[[1]]
      row$result_condition <- parsed_result$result_condition[[1]]
      row$result_condition_value <- parsed_result$result_condition_value[[1]]
      row$laboratory <- NA_integer_
      row$analysis_datetime <- as.POSIXct(NA)
      row$note <- ""
      row$mapping_status <- "manual"
      row$source_code <- "YGwater-manual"
      row$source_row_number <- NA_integer_
      data$df <- rbind(data$df, row)
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

    output$mapping_editor <- renderUI({
      rows <- mapping_rows()
      if (!nrow(rows)) {
        return(tags$div(class = "text-muted", "No parameter mappings need review."))
      }
      tagList(lapply(seq_len(nrow(rows)), function(i) {
        id <- paste0("mapping_", i)
        label <- paste0(
          rows$source_parameter_code[[i]],
          if (addDiscData_present(rows$source_unit[[i]])) {
            paste0(" [", rows$source_unit[[i]], "]")
          } else {
            ""
          },
          if (addDiscData_present(rows$source_parameter_name[[i]])) {
            paste0(" - ", rows$source_parameter_name[[i]])
          } else {
            ""
          }
        )
        selectizeInput(
          ns(id),
          label,
          choices = stats::setNames(params()$parameter_id, params()$param_name),
          selected = rows$parameter_id[[i]],
          options = list(placeholder = "Select AquaCache parameter")
        )
      }))
    })

    observeEvent(input$save_parameter_mappings, {
      rows <- mapping_rows()
      if (!nrow(rows)) {
        showNotification("No mappings to save.", type = "message")
        return()
      }
      saved <- 0L
      tryCatch(
        {
          for (i in seq_len(nrow(rows))) {
            parameter_id <- addDiscData_int(input[[paste0("mapping_", i)]])
            if (is.na(parameter_id)) {
              next
            }
            addDiscData_upsert_mapping(
              con = con,
              source_code = rows$source_code[[i]],
              source_name = rows$source_code[[i]],
              parameter_code = rows$source_parameter_code[[i]],
              unit = rows$source_unit[[i]],
              parameter_id = parameter_id,
              result_type = addDiscData_int(rows$result_type[[i]], 2L),
              sample_fraction_id = addDiscData_int(rows$sample_fraction_id[[i]]),
              result_value_type = addDiscData_int(rows$result_value_type[[i]], 1L),
              result_speciation_id = addDiscData_int(rows$result_speciation_id[[i]]),
              matrix_state_id = addDiscData_int(rows$matrix_state_id[[i]], 1L),
              note = "Saved from YGwater add discrete data mapping editor."
            )
            saved <- saved + 1L
          }
          data$df <- addDiscData_apply_mappings(data$df, con)[names(addDiscData_empty_table())]
          showNotification(sprintf("Saved %s mapping(s).", saved), type = "message")
        },
        error = function(e) {
          showNotification(paste("Saving mappings failed:", e$message), type = "error")
        }
      )
    })

    output$data_table <- DT::renderDT(
      {
        DT::datatable(
          data$df,
          editable = TRUE,
          selection = "single",
          rownames = FALSE,
          options = list(scrollX = TRUE, pageLength = 15)
        )
      },
      server = FALSE
    )

    observeEvent(input$data_table_cell_edit, {
      info <- input$data_table_cell_edit
      col <- info$col
      if (col < 1L) {
        col <- col + 1L
      }
      if (col >= 1L && col <= ncol(data$df)) {
        data$df[info$row, col] <- DT::coerceValue(info$value, data$df[info$row, col])
      }
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
                 matrix_state_id,
                 note
               ) VALUES (
                 $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13
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
