#' Read logger files to tabular continuous data
#'
#' @description
#' Converts Solinst `.xle`, In-Situ/VuSitu `.html`, and supported Onset HOBO
#' `.hobo` logger files to a tabular data frame for continuous data upload. The
#' returned `datetime` column is converted to UTC. This function does not upload,
#' move, copy, or track files.
#'
#' @param file Full path to a `.xle`, `.html`, or `.hobo` logger file.
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

  ext <- if (
    is.null(file_type) ||
      length(file_type) == 0L ||
      is.na(file_type[[1]]) ||
      !nzchar(file_type[[1]])
  ) {
    tools::file_ext(file)
  } else {
    file_type[[1]]
  }
  ext <- tolower(ext)
  if (identical(ext, "xle")) {
    return(read_solinst_xle_data(file, default_tz = default_tz))
  }
  if (identical(ext, "html") || identical(ext, "htm")) {
    return(read_insitu_html_data(file, default_tz = default_tz))
  }
  if (identical(ext, "hobo")) {
    return(read_hobo_data(file, default_tz = default_tz))
  }

  stop("Unsupported logger file extension: .", ext, call. = FALSE)
}

read_solinst_xle_data <- function(file, default_tz = "UTC") {
  xml_file <- xml2::read_xml(file)
  if (!identical(xml2::xml_name(xml2::xml_root(xml_file)), "Body_xle")) {
    stop("File does not appear to be a Solinst XLE logger file.", call. = FALSE)
  }

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
  if (!is_insitu_html_logger_file(html)) {
    stop(
      "File does not appear to be an InSitu/VuSitu HTML logger file.",
      call. = FALSE
    )
  }

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

is_insitu_html_logger_file <- function(html) {
  has_data_table <- length(rvest::html_elements(html, "[isi-data-table]")) > 0L
  has_data_rows <- length(rvest::html_elements(html, "[isi-data-row]")) > 0L
  has_time_offset <- length(rvest::html_elements(
    html,
    "[isi-group-member='ReportProperties'][isi-property='TimeOffset']"
  )) > 0L

  has_data_table && has_data_rows && has_time_offset
}

read_hobo_data <- function(file, default_tz = "UTC") {
  hobo <- parse_hobo_binary(file)
  model <- hobo_tag_text(hobo, 0x05)

  if (grepl("U24-001 Conductivity", model, fixed = TRUE)) {
    out <- read_hobo_u24_data(hobo)
  } else if (grepl("UTBI-001 TidbiT Temp", model, fixed = TRUE)) {
    out <- read_hobo_tidbit_data(hobo)
  } else {
    stop(
      "Unsupported HOBO logger model: ",
      ifelse(nzchar(model), model, "unknown"),
      call. = FALSE
    )
  }

  offset_seconds <- hobo_utc_offset_seconds(hobo)
  attr(out, "logger_timezone_note") <- logger_timezone_note(
    offset_seconds = offset_seconds,
    default_tz = default_tz,
    file_has_offset = !is.na(offset_seconds)
  )
  out
}

read_hobo_u24_data <- function(hobo) {
  active <- hobo_u24_active_channels(hobo)
  if (!any(active)) {
    stop(
      "No active U24 conductivity channels found in HOBO file.",
      call. = FALSE
    )
  }

  channel_widths <- c(low = 4L, full = 4L, temp = 3L)[active]
  raw_channels <- hobo_decode_packed_records(
    hobo = hobo,
    channel_widths = channel_widths,
    marker_width = 6L
  )
  calibrations <- hobo_tag_floats(hobo, 0x31)
  if (length(calibrations) < 3L) {
    stop(
      "U24 HOBO file does not contain the expected calibration blocks.",
      call. = FALSE
    )
  }

  out <- data.table::data.table(
    datetime = hobo_datetimes(hobo, nrow(raw_channels))
  )
  temp_c <- if ("temp" %in% names(raw_channels)) {
    calibrations[[3L]][[1L]] + calibrations[[3L]][[2L]] * raw_channels$temp
  } else {
    rep(NA_real_, nrow(raw_channels))
  }

  if ("low" %in% names(raw_channels)) {
    out[,
      ("Low Range Conductivity (\u00B5S/cm)") := hobo_u24_conductivity(
        raw_channels$low,
        temp_c,
        calibrations[[1L]]
      )
    ]
  }
  if ("full" %in% names(raw_channels)) {
    out[,
      ("Full Range Conductivity (\u00B5S/cm)") := hobo_u24_conductivity(
        raw_channels$full,
        temp_c,
        calibrations[[2L]]
      )
    ]
  }
  if ("temp" %in% names(raw_channels)) {
    out[, ("Temperature (\u00B0C)") := temp_c]
  }

  as.data.frame(out)
}

read_hobo_tidbit_data <- function(hobo) {
  raw_values <- hobo_tidbit_raw_values(hobo)
  if (length(raw_values) == 0L) {
    stop("No TidbiT temperature records found in HOBO file.", call. = FALSE)
  }

  out <- data.table::data.table(
    datetime = hobo_datetimes(hobo, length(raw_values))
  )
  out[, ("Temperature (\u00B0C)") := hobo_tidbit_temperature(raw_values)]
  as.data.frame(out)
}

parse_hobo_binary <- function(file) {
  bytes <- readBin(file, "raw", n = file.info(file)$size)
  int_bytes <- as.integer(bytes)
  if (length(int_bytes) < 8L || !identical(rawToChar(bytes[1:4]), "HOBO")) {
    stop("File does not appear to be an Onset HOBO binary file.", call. = FALSE)
  }

  tags <- list()
  pos <- 5L
  while (pos <= length(int_bytes) - 2L && int_bytes[[pos]] == 0x88) {
    tag <- int_bytes[[pos + 1L]]
    len <- int_bytes[[pos + 2L]]
    if (pos + 2L + len > length(int_bytes)) {
      break
    }
    value <- if (len == 0L) {
      integer()
    } else {
      int_bytes[(pos + 3L):(pos + 2L + len)]
    }
    tags[[length(tags) + 1L]] <- list(
      tag = tag,
      len = len,
      value = value,
      offset = pos - 1L
    )
    pos <- pos + 3L + len
  }

  list(bytes = int_bytes, tags = tags)
}

hobo_tag_values <- function(hobo, tag) {
  lapply(
    hobo$tags[vapply(hobo$tags, function(x) x$tag == tag, logical(1))],
    `[[`,
    "value"
  )
}

hobo_tag_value <- function(hobo, tag, required = TRUE) {
  values <- hobo_tag_values(hobo, tag)
  if (length(values) == 0L) {
    if (required) {
      stop(
        sprintf("HOBO file is missing required tag 0x%02X.", tag),
        call. = FALSE
      )
    }
    return(integer())
  }
  values[[1L]]
}

hobo_tag_text <- function(hobo, tag) {
  value <- hobo_tag_value(hobo, tag, required = FALSE)
  if (length(value) == 0L) {
    return("")
  }
  rawToChar(as.raw(value))
}

hobo_tag_int <- function(hobo, tag) {
  hobo_big_endian_int(hobo_tag_value(hobo, tag))
}

hobo_tag_floats <- function(hobo, tag) {
  lapply(hobo_tag_values(hobo, tag), function(value) {
    readBin(
      as.raw(value),
      "numeric",
      n = length(value) / 4L,
      size = 4L,
      endian = "big"
    )
  })
}

hobo_big_endian_int <- function(value) {
  sum(as.integer(value) * 256^rev(seq_along(value) - 1L))
}

hobo_utc_offset_seconds <- function(hobo) {
  offset <- hobo_tag_text(hobo, 0x14)
  parse_logger_utc_offset(offset)
}

hobo_datetimes <- function(hobo, n, first_row_offset = 0L) {
  launch_info <- hobo_tag_value(hobo, 0x22)
  if (length(launch_info) < 11L) {
    stop(
      "HOBO file does not contain the expected launch-time metadata.",
      call. = FALSE
    )
  }

  launch_seconds <- hobo_big_endian_int(launch_info[5:8])
  first_log_delay <- hobo_tag_int(hobo, 0x09)
  interval_seconds <- hobo_tag_int(hobo, 0x08)
  start_time <- as.POSIXct(
    launch_seconds + first_log_delay + first_row_offset * interval_seconds,
    origin = "2000-01-01",
    tz = "UTC"
  )
  start_time + seq.int(0L, length.out = n) * interval_seconds
}

hobo_data_offset <- function(hobo) {
  hobo_tag_int(hobo, 0x0D)
}

hobo_payload_nibbles <- function(hobo) {
  offset <- hobo_data_offset(hobo)
  bytes <- hobo$bytes[(offset + 1L):length(hobo$bytes)]
  as.vector(rbind(floor(bytes / 16L), bytes %% 16L))
}

hobo_decode_packed_records <- function(hobo, channel_widths, marker_width) {
  nibbles <- hobo_payload_nibbles(hobo)
  record_width <- sum(channel_widths)
  starts <- c(
    1L,
    seq.int(
      record_width + marker_width + 1L,
      length(nibbles) - record_width + 1L,
      by = record_width
    )
  )

  records <- lapply(starts, function(start) {
    pos <- start
    values <- integer(length(channel_widths))
    for (i in seq_along(channel_widths)) {
      width <- channel_widths[[i]]
      values[[i]] <- hobo_nibbles_to_int(nibbles[pos:(pos + width - 1L)])
      pos <- pos + width
    }
    values
  })
  out <- data.table::as.data.table(do.call(rbind, records))
  data.table::setnames(out, names(channel_widths))
  out
}

hobo_nibbles_to_int <- function(nibbles) {
  sum(as.integer(nibbles) * 16^rev(seq_along(nibbles) - 1L))
}

hobo_u24_active_channels <- function(hobo) {
  channel_flags <- hobo_tag_values(hobo, 0x0B)
  channel_flags <- vapply(
    channel_flags,
    function(x) length(x) == 1L && x[[1L]] == 1L,
    logical(1)
  )
  if (length(channel_flags) < 3L) {
    stop(
      "U24 HOBO file does not contain the expected channel flags.",
      call. = FALSE
    )
  }
  out <- channel_flags[1:3]
  names(out) <- c("low", "full", "temp")
  out
}

hobo_u24_conductivity <- function(raw_values, temp_c, calibration) {
  conductivity <- rowSums(vapply(
    seq_len(6L),
    function(i) {
      calibration[[i]] * raw_values^(i - 1L)
    },
    numeric(length(raw_values))
  ))

  # HOBOware applies a small temperature compensation after the conductivity
  # calibration. These constants were checked against paired HOBOware CSV
  # exports from U24-001 files with Full Range + Temp and Low/Full + Temp
  # channel configurations.
  conductivity * (1.0323 - 0.001064 * temp_c)
}

hobo_tidbit_raw_values <- function(hobo) {
  values <- hobo_decode_packed_records(
    hobo = hobo,
    channel_widths = c(temp = 3L),
    marker_width = 5L
  )$temp
  hobo_tidbit_clean_raw_values(values[values > 0x500 & values < 0xF00])
}

hobo_tidbit_clean_raw_values <- function(values) {
  if (length(values) < 3L) {
    return(values)
  }

  removed_tail <- FALSE
  while (length(values) >= 3L) {
    n <- length(values)
    last_to_third <- abs(values[[n]] - values[[n - 2L]])
    second_to_third <- abs(values[[n - 1L]] - values[[n - 2L]])
    last_to_second <- abs(values[[n]] - values[[n - 1L]])

    if (
      last_to_third <= 100 &&
        second_to_third > 100 &&
        last_to_second > 100
    ) {
      values <- values[-n]
      next
    }

    if (last_to_third > 100 && second_to_third > 100) {
      values <- values[-n]
      removed_tail <- TRUE
      next
    }

    if (removed_tail && last_to_second > 100 && second_to_third <= 100) {
      values <- values[-n]
      next
    }

    break
  }

  keep <- rep(TRUE, length(values))
  for (i in seq.int(2L, length(values) - 1L)) {
    keep[[i]] <- !(abs(values[[i]] - values[[i - 1L]]) > 100 &&
      abs(values[[i]] - values[[i + 1L]]) > 100 &&
      abs(values[[i - 1L]] - values[[i + 1L]]) <= 100)
  }

  values[keep]
}

hobo_tidbit_temperature <- function(raw_values) {
  coefficients <- c(
    108.1188,
    -0.06771122,
    0.00001865007,
    -0.000000002638962
  )
  rowSums(vapply(
    seq_along(coefficients),
    function(i) {
      coefficients[[i]] * raw_values^(i - 1L)
    },
    numeric(length(raw_values))
  ))
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

parse_logger_local_datetime <- function(
  x,
  default_tz,
  offset_seconds = NA_real_
) {
  x <- trimws(as.character(x))
  x <- sub("\\.\\d+$", "", x)

  if (!is.na(offset_seconds)) {
    parsed <- as.POSIXct(x, tz = "UTC", tryFormats = logger_datetime_formats())
    parsed <- parsed - offset_seconds
  } else {
    tz_info <- normalize_logger_default_tz(default_tz)
    if (!is.na(tz_info$offset_seconds)) {
      parsed <- as.POSIXct(
        x,
        tz = "UTC",
        tryFormats = logger_datetime_formats()
      )
      parsed <- parsed - tz_info$offset_seconds
    } else {
      parsed <- as.POSIXct(
        x,
        tz = tz_info$tz,
        tryFormats = logger_datetime_formats()
      )
    }
  }

  attr(parsed, "tzone") <- "UTC"
  parsed
}

normalize_logger_default_tz <- function(default_tz) {
  if (length(default_tz) != 1L || is.na(default_tz)) {
    stop(
      "default_tz must be a single time zone string or numeric UTC offset.",
      call. = FALSE
    )
  }

  if (is.numeric(default_tz)) {
    return(list(
      tz = "UTC",
      offset_seconds = utc_offset_hours_to_seconds(default_tz)
    ))
  }

  if (!is.character(default_tz)) {
    stop(
      "default_tz must be a single time zone string or numeric UTC offset.",
      call. = FALSE
    )
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
    stop(
      "Numeric default_tz must be a finite UTC offset in hours.",
      call. = FALSE
    )
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
  minutes <- ifelse(
    length(parts) >= 4 && nzchar(parts[[4]]),
    as.numeric(parts[[4]]),
    0
  )

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
  sign *
    (as.numeric(parts[[3]]) *
      3600 +
      as.numeric(parts[[4]]) * 60 +
      ifelse(
        length(parts) >= 5 && nzchar(parts[[5]]),
        as.numeric(parts[[5]]),
        0
      ))
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

  unit_label <- if (!is.na(unit) && nzchar(unit)) {
    paste0(" (", unit, ")")
  } else {
    ""
  }
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
