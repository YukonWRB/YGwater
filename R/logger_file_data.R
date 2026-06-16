#' Read logger files to tabular continuous data
#'
#' @description
#' Converts Solinst `.xle` and In-Situ/VuSitu `.html` logger exports to a
#' tabular data frame for continuous data upload. The returned `datetime`
#' column is converted to UTC. This function does not upload, move, copy, or
#' track files.
#'
#' @param file Full path to a `.xle` or `.html` logger file.
#' @param file_type Optional file extension override. This is useful for Shiny
#'   uploads where the temporary `datapath` may not keep the original extension.
#' @param default_tz Time zone used when the logger file does not include an
#'   explicit UTC offset. Use either a single R time zone name from
#'   `OlsonNames()` (for example, `"America/Whitehorse"` or `"UTC"`) or a fixed
#'   UTC offset in hours (for example, `-7` for UTC-07:00). Fixed offsets may
#'   also be supplied as strings like `"UTC-07:00"`, `"-07:00"`, or `"GMT-07:00"`.
#'   Defaults to UTC.
#'
#' @return A data frame with `datetime` in UTC and one column per logger
#'   measurement.
#' @export
read_logger_file_data <- function(file, file_type = NULL, default_tz = "UTC") {
  if (!file.exists(file)) {
    stop("Logger file not found: ", file, call. = FALSE)
  }

  ext <- if (is.null(file_type) || is.na(file_type) || !nzchar(file_type)) {
    tools::file_ext(file)
  } else {
    file_type
  }
  ext <- tolower(ext)
  if (identical(ext, "xle")) {
    return(read_solinst_xle_data(file, default_tz = default_tz))
  }
  if (identical(ext, "html") || identical(ext, "htm")) {
    return(read_insitu_html_data(file, default_tz = default_tz))
  }

  stop("Unsupported logger file extension: .", ext, call. = FALSE)
}

read_solinst_xle_data <- function(file, default_tz = "UTC") {
  xml_file <- xml2::read_xml(file)

  channel_nodes <- xml2::xml_find_all(
    xml_file,
    "//Ch1_data_header|//Ch2_data_header|//Ch3_data_header|//Ch4_data_header"
  )
  if (length(channel_nodes) == 0) {
    stop("No channel data headers found in XLE file.", call. = FALSE)
  }

  channel_info <- data.table::rbindlist(lapply(channel_nodes, function(node) {
    data.table::data.table(
      channel = sub("_data_header$", "", xml2::xml_name(node)),
      parameter = xle_node_text(node, "Identification"),
      unit = xle_node_text(node, "Unit")
    )
  }))
  channel_info[, channel_num := as.integer(sub("^Ch", "", channel))]
  data.table::setorder(channel_info, channel_num)

  log_nodes <- xml2::xml_find_all(xml_file, ".//Data/Log")
  if (length(log_nodes) == 0) {
    stop("No log rows found in XLE file.", call. = FALSE)
  }

  dates <- xml2::xml_text(xml2::xml_find_all(xml_file, ".//Data/Log/Date"))
  times <- xml2::xml_text(xml2::xml_find_all(xml_file, ".//Data/Log/Time"))
  if (length(dates) != length(log_nodes) || length(times) != length(log_nodes)) {
    stop("XLE log rows must each contain Date and Time fields.", call. = FALSE)
  }

  out <- data.table::data.table(
    datetime = parse_logger_local_datetime(
      paste(dates, times),
      default_tz = default_tz
    )
  )

  for (i in seq_len(nrow(channel_info))) {
    channel <- tolower(channel_info$channel[[i]])
    values <- xml2::xml_text(
      xml2::xml_find_all(xml_file, paste0(".//Data/Log/", channel))
    )
    if (length(values) != length(log_nodes)) {
      stop(
        "XLE channel ",
        channel_info$channel[[i]],
        " does not have one value per log row.",
        call. = FALSE
      )
    }

    converted <- convert_logger_values(
      suppressWarnings(as.numeric(values)),
      parameter = channel_info$parameter[[i]],
      unit = channel_info$unit[[i]]
    )
    out[, (converted$name) := converted$values]
  }

  out <- as.data.frame(out)
  attr(out, "logger_timezone_note") <- logger_timezone_note(
    offset_seconds = NA_real_,
    default_tz = default_tz,
    file_has_offset = FALSE
  )
  out
}

read_insitu_html_data <- function(file, default_tz = "UTC") {
  html <- xml2::read_html(file)

  header_nodes <- rvest::html_elements(html, "tr.dataHeader td")
  headers <- rvest::html_text(header_nodes, trim = TRUE)
  headers <- headers[nzchar(headers)]
  if (length(headers) < 2) {
    stop("No data table headers found in HTML logger file.", call. = FALSE)
  }

  row_nodes <- rvest::html_elements(html, "tr.data")
  if (length(row_nodes) == 0) {
    stop("No data rows found in HTML logger file.", call. = FALSE)
  }

  rows <- lapply(row_nodes, function(node) {
    rvest::html_text(rvest::html_elements(node, "td"), trim = TRUE)
  })
  row_lengths <- lengths(rows)
  if (any(row_lengths != length(headers))) {
    stop("HTML logger data rows do not match the header column count.", call. = FALSE)
  }

  mat <- matrix(unlist(rows, use.names = FALSE), ncol = length(headers), byrow = TRUE)
  parsed_headers <- parse_logger_headers(headers)
  offset_seconds <- html_time_offset_seconds(html)

  out <- data.table::data.table(
    datetime = parse_logger_local_datetime(
      mat[, 1],
      default_tz = default_tz,
      offset_seconds = offset_seconds
    )
  )

  for (i in seq.int(2L, ncol(mat))) {
    converted <- convert_logger_values(
      suppressWarnings(as.numeric(mat[, i])),
      parameter = parsed_headers$parameter[[i]],
      unit = parsed_headers$unit[[i]]
    )
    out[, (converted$name) := converted$values]
  }

  if ("Pressure (m)" %in% names(out)) {
    out[, `Pressure (m)` := `Pressure (m)` * 1.001]
  } else if ("Depth (m)" %in% names(out)) {
    out[, `Pressure (m)` := `Depth (m)` * 0.999]
  }

  out <- as.data.frame(out)
  attr(out, "logger_timezone_note") <- logger_timezone_note(
    offset_seconds = offset_seconds,
    default_tz = default_tz,
    file_has_offset = !is.na(offset_seconds)
  )
  out
}

xle_node_text <- function(node, name) {
  child <- xml2::xml_find_first(node, name)
  if (inherits(child, "xml_missing")) {
    return(NA_character_)
  }
  xml2::xml_text(child)
}

parse_logger_headers <- function(headers) {
  data.table::rbindlist(lapply(headers, function(header) {
    header <- trimws(header)
    unit_match <- regmatches(header, regexpr("\\([^()]+\\)", header))
    unit <- if (length(unit_match) == 0) {
      NA_character_
    } else {
      sub("^\\((.*)\\)$", "\\1", unit_match[[1]])
    }

    data.table::data.table(
      parameter = trimws(sub("\\s*\\(.*$", "", header)),
      unit = unit
    )
  }))
}

parse_logger_local_datetime <- function(x, default_tz, offset_seconds = NA_real_) {
  x <- trimws(as.character(x))
  x <- sub("\\.\\d+$", "", x)

  if (!is.na(offset_seconds)) {
    parsed <- as.POSIXct(x, tz = "UTC", tryFormats = logger_datetime_formats())
    parsed <- parsed - offset_seconds
  } else {
    tz_info <- normalize_logger_default_tz(default_tz)
    if (!is.na(tz_info$offset_seconds)) {
      parsed <- as.POSIXct(x, tz = "UTC", tryFormats = logger_datetime_formats())
      parsed <- parsed - tz_info$offset_seconds
    } else {
      parsed <- as.POSIXct(x, tz = tz_info$tz, tryFormats = logger_datetime_formats())
    }
  }

  attr(parsed, "tzone") <- "UTC"
  parsed
}

normalize_logger_default_tz <- function(default_tz) {
  if (length(default_tz) != 1L || is.na(default_tz)) {
    stop("default_tz must be a single time zone string or numeric UTC offset.", call. = FALSE)
  }

  if (is.numeric(default_tz)) {
    return(list(tz = "UTC", offset_seconds = utc_offset_hours_to_seconds(default_tz)))
  }

  if (!is.character(default_tz)) {
    stop("default_tz must be a single time zone string or numeric UTC offset.", call. = FALSE)
  }

  default_tz <- trimws(default_tz)
  if (!nzchar(default_tz)) {
    stop("default_tz cannot be blank.", call. = FALSE)
  }

  offset_seconds <- parse_logger_utc_offset(default_tz)
  if (!is.na(offset_seconds)) {
    return(list(tz = "UTC", offset_seconds = offset_seconds))
  }

  valid_tz <- OlsonNames()
  if (default_tz %in% valid_tz || identical(default_tz, "UTC")) {
    return(list(tz = default_tz, offset_seconds = NA_real_))
  }

  stop(
    "default_tz must be a valid R time zone name from OlsonNames() ",
    "or a fixed UTC offset such as -7, 'UTC-07:00', or 'GMT-07:00'.",
    call. = FALSE
  )
}

utc_offset_hours_to_seconds <- function(offset_hours) {
  offset_hours <- as.numeric(offset_hours)
  if (!is.finite(offset_hours) || abs(offset_hours) > 24) {
    stop("Numeric default_tz must be a finite UTC offset in hours.", call. = FALSE)
  }
  offset_hours * 3600
}

parse_logger_utc_offset <- function(value) {
  value <- trimws(as.character(value))
  if (toupper(value) %in% c("UTC", "GMT")) {
    return(0)
  }
  value <- sub("^(UTC|GMT)\\s*", "", value, ignore.case = TRUE)
  if (identical(toupper(value), "Z")) {
    return(0)
  }

  match <- regexec("^([+-]?)(\\d{1,2})(?::?(\\d{2}))?$", value)
  parts <- regmatches(value, match)[[1]]
  if (length(parts) == 0) {
    return(NA_real_)
  }

  sign <- if (identical(parts[[2]], "-")) -1 else 1
  hours <- as.numeric(parts[[3]])
  minutes <- ifelse(length(parts) >= 4 && nzchar(parts[[4]]), as.numeric(parts[[4]]), 0)

  if (hours > 24 || minutes >= 60 || (hours == 24 && minutes > 0)) {
    return(NA_real_)
  }

  sign * (hours * 3600 + minutes * 60)
}

logger_timezone_note <- function(offset_seconds, default_tz, file_has_offset) {
  if (isTRUE(file_has_offset)) {
    offset_label <- format_logger_utc_offset(offset_seconds)
    if (isTRUE(offset_seconds == 0)) {
      return(paste0(
        "The logger file contains UTC offset ",
        offset_label,
        ". Datetimes were already UTC, so no offset shift was applied. ",
        "The UTC offset of data control has been set to UTC+00:00."
      ))
    }

    return(paste0(
      "The logger file contains UTC offset ",
      offset_label,
      ". Datetimes were shifted to UTC for upload. ",
      "The UTC offset of data control has been set to UTC+00:00."
    ))
  }

  tz_info <- normalize_logger_default_tz(default_tz)
  if (!is.na(tz_info$offset_seconds)) {
    offset_label <- format_logger_utc_offset(tz_info$offset_seconds)
    if (isTRUE(tz_info$offset_seconds == 0)) {
      return(paste0(
        "The logger file does not contain an explicit UTC offset. ",
        "Datetimes were treated as UTC, so no offset shift was applied. ",
        "The UTC offset of data control has been set to UTC+00:00."
      ))
    }

    return(paste0(
      "The logger file does not contain an explicit UTC offset. ",
      "Datetimes were interpreted as ",
      offset_label,
      " and shifted to UTC for upload. ",
      "The UTC offset of data control has been set to UTC+00:00."
    ))
  }

  paste0(
    "The logger file does not contain an explicit UTC offset. ",
    "Datetimes were interpreted in the ",
    tz_info$tz,
    " time zone and converted to UTC for upload. ",
    "The UTC offset of data control has been set to UTC+00:00."
  )
}

format_logger_utc_offset <- function(offset_seconds) {
  sign <- if (offset_seconds < 0) "-" else "+"
  abs_seconds <- abs(offset_seconds)
  hours <- floor(abs_seconds / 3600)
  minutes <- floor((abs_seconds %% 3600) / 60)
  sprintf("UTC%s%02d:%02d", sign, hours, minutes)
}

logger_datetime_formats <- function() {
  c(
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%Y/%m/%d %H:%M:%S",
    "%Y/%m/%d %H:%M",
    "%m/%d/%Y %H:%M:%S",
    "%m/%d/%Y %H:%M"
  )
}

html_time_offset_seconds <- function(html) {
  offset_node <- rvest::html_element(
    html,
    xpath = "//*[@isi-property='TimeOffset']//*[@isi-value]"
  )
  if (inherits(offset_node, "xml_missing") || length(offset_node) == 0) {
    return(NA_real_)
  }

  offset <- rvest::html_text(offset_node, trim = TRUE)
  match <- regexec("^([+-]?)(\\d{2}):(\\d{2})(?::(\\d{2}))?$", offset)
  parts <- regmatches(offset, match)[[1]]
  if (length(parts) == 0) {
    return(NA_real_)
  }

  sign <- if (identical(parts[[2]], "-")) -1 else 1
  sign * (
    as.numeric(parts[[3]]) * 3600 +
      as.numeric(parts[[4]]) * 60 +
      ifelse(length(parts) >= 5 && nzchar(parts[[5]]), as.numeric(parts[[5]]), 0)
  )
}

convert_logger_values <- function(values, parameter, unit) {
  parameter_label <- trimws(as.character(parameter))
  if (is.na(parameter_label) || !nzchar(parameter_label)) {
    parameter_label <- "value"
  }
  parameter_norm <- tolower(trimws(parameter))
  unit_norm <- logger_unit_key(unit)

  if (parameter_norm %in% c("level", "pressure")) {
    parameter_name <- if (identical(parameter_norm, "pressure")) {
      "Pressure"
    } else {
      "Level"
    }
    return(list(
      name = paste0(parameter_name, " (m)"),
      values = convert_logger_level(values, unit_norm)
    ))
  }

  if (identical(parameter_norm, "depth")) {
    return(list(
      name = "Depth (m)",
      values = convert_logger_depth(values, unit_norm)
    ))
  }

  if (parameter_norm %in% c("temperature", "temp")) {
    return(list(
      name = "Temperature (\u00B0C)",
      values = convert_logger_temperature(values, unit_norm)
    ))
  }

  if (parameter_norm %in% c("conductivity", "specific conductance")) {
    return(list(
      name = "Conductivity (\u00B5S/cm)",
      values = convert_logger_conductivity(values, unit_norm)
    ))
  }

  unit_label <- if (!is.na(unit) && nzchar(unit)) paste0(" (", unit, ")") else ""
  list(name = paste0(parameter_label, unit_label), values = values)
}

logger_unit_key <- function(unit) {
  unit <- trimws(as.character(unit))
  unit <- gsub("\u00B5", "u", unit, fixed = TRUE)
  unit <- gsub("\u00B0", "deg ", unit, fixed = TRUE)
  unit <- gsub("^[^A-Za-z0-9u]+", "", unit)
  tolower(unit)
}

convert_logger_level <- function(values, unit) {
  factors <- c(
    "m" = 1,
    "m h2o" = 1,
    "psi" = 0.7030700000,
    "kpa" = 0.1019716213,
    "bar" = 10.1971621298,
    "mbar" = 0.0101971621,
    "mm hg" = 0.0135950982,
    "in hg" = 0.3453187748,
    "cm h2o" = 0.0101971621,
    "in h2o" = 0.0254000000
  )
  convert_logger_by_factor(values, unit, factors, "level/pressure")
}

convert_logger_depth <- function(values, unit) {
  factors <- c(
    "m" = 1,
    "mm" = 0.001,
    "cm" = 0.01,
    "in" = 0.0254,
    "ft" = 0.3048
  )
  convert_logger_by_factor(values, unit, factors, "depth")
}

convert_logger_temperature <- function(values, unit) {
  if (unit %in% c("deg c", "c", "celsius", "degc")) {
    return(values)
  }
  if (unit %in% c("deg f", "f", "fahrenheit", "degf")) {
    return((values - 32) * 5 / 9)
  }
  stop("Unrecognized temperature unit: ", unit, call. = FALSE)
}

convert_logger_conductivity <- function(values, unit) {
  if (unit %in% c("us/cm", "us/cm.", "umhos/cm")) {
    return(values)
  }
  if (unit %in% c("ms/cm", "millisiemens/cm")) {
    return(values * 1000)
  }
  stop("Unrecognized conductivity unit: ", unit, call. = FALSE)
}

convert_logger_by_factor <- function(values, unit, factors, label) {
  if (unit %in% names(factors)) {
    return(values * unname(factors[[unit]]))
  }
  stop("Unrecognized ", label, " unit: ", unit, call. = FALSE)
}
