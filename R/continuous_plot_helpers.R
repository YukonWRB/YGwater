# Helpers for continuous trace plotting

#' Format continuous-timeseries sensor priorities for display
#' @param sensor_priority Integer or character sensor-priority values.
#' @param language Language name used by [tr()].
#' @param translations Translation catalogue passed to [tr()].
#' @return A character vector containing readable priority labels.
#' @noRd
#' @keywords internal
format_sensor_priority_label <- function(
  sensor_priority,
  language,
  translations = data$translations
) {
  priority <- as.character(sensor_priority)
  priority_number <- suppressWarnings(as.integer(priority))
  labels <- vapply(
    c(
      "sensor_priority_primary",
      "sensor_priority_secondary",
      "sensor_priority_tertiary"
    ),
    tr,
    character(1),
    lang = language,
    translations = translations
  )

  out <- rep(NA_character_, length(priority))
  matched <- !is.na(priority_number) & priority_number %in% seq_along(labels)
  out[matched] <- labels[priority_number[matched]]

  fallback <- !matched & !is.na(priority) & nzchar(priority)
  out[fallback] <- priority[fallback]
  out
}

#' Format the historical-statistics period for display
#' @param stats_period Character statistics-period values.
#' @param language Language name used by [tr()].
#' @param translations Translation catalogue passed to [tr()].
#' @return A character vector containing translated period labels.
#' @noRd
#' @keywords internal
format_stats_period_label <- function(
  stats_period,
  language,
  translations = data$translations
) {
  keys <- c(
    `30yr` = "stats_period_last_30_years",
    full = "stats_period_entire_record"
  )
  stats_period <- as.character(stats_period)
  matched <- stats_period %in% names(keys)
  out <- stats_period
  out[matched] <- vapply(
    unname(keys[stats_period[matched]]),
    tr,
    character(1),
    lang = language,
    translations = translations
  )
  out
}

#' Build unambiguous location labels for the continuous table
#' @param locations Location metadata containing `location_id` and `name_col`.
#' @param name_col Name of the localized location-name column.
#' @return Character labels. Duplicate display names include their location ID.
#' @noRd
#' @keywords internal
continuous_plot_location_labels <- function(locations, name_col) {
  if (!all(c("location_id", name_col) %in% names(locations))) {
    return(character())
  }

  labels <- as.character(locations[[name_col]])
  duplicate_labels <-
    !is.na(labels) &
    (duplicated(labels) | duplicated(labels, fromLast = TRUE))
  labels[duplicate_labels] <- sprintf(
    "%s [%s]",
    labels[duplicate_labels],
    locations[["location_id"]][duplicate_labels]
  )
  labels
}

#' Resolve a map location to the continuous table's display value
#' @param location_id Location identifier supplied by the map module.
#' @param timeseries Continuous-timeseries metadata containing `location_id`.
#' @param locations Location metadata containing `location_id` and `name_col`.
#' @param name_col Name of the localized location-name column.
#' @return The scalar location label used by the table, or `NULL` when the
#'   request is invalid or has no continuous timeseries.
#' @noRd
#' @keywords internal
continuous_plot_map_location_value <- function(
  location_id,
  timeseries,
  locations,
  name_col
) {
  location_id <- suppressWarnings(as.numeric(location_id))
  location_id <- unique(location_id[!is.na(location_id)])
  if (
    length(location_id) != 1L ||
      !"location_id" %in% names(timeseries) ||
      !location_id %in% timeseries$location_id ||
      !all(c("location_id", name_col) %in% names(locations))
  ) {
    return(NULL)
  }

  location_labels <- continuous_plot_location_labels(locations, name_col)
  location_value <- location_labels[
    locations[["location_id"]] %in% location_id
  ]
  location_value <- unique(as.character(stats::na.omit(location_value)))
  location_value <- location_value[nzchar(location_value)]
  if (length(location_value) == 0L) {
    return(NULL)
  }

  location_value[[1L]]
}

#' Build DataTables column searches for a map location
#' @param column_names Names of the complete table columns.
#' @param location_value Scalar location factor label, or `NULL` for no filter.
#' @return A character vector suitable for [DT::updateSearch()]. Factor values
#'   are JSON encoded so DataTables performs an exact factor match.
#' @noRd
#' @keywords internal
continuous_plot_location_search_columns <- function(
  column_names,
  location_value = NULL
) {
  searches <- rep("", length(column_names))
  location_column <- match("location", column_names)
  if (
    !is.na(location_column) &&
      length(location_value) == 1L &&
      !is.na(location_value) &&
      nzchar(location_value)
  ) {
    searches[[location_column]] <- as.character(jsonlite::toJSON(
      as.character(location_value),
      auto_unbox = FALSE
    ))
  }
  searches
}

#' @title Check if corrected source should be used for continuous trace
#' @description Determines whether the continuous trace should use the corrected source based on the presence of applicable corrections in the database.
#' @param con A DBI database connection object.
#' @param timeseries_id The ID of the timeseries being plotted.
#' @param start_date The start date of the plot range (POSIXct or character).
#' @param end_date The end date of the plot range (POSIXct or character).
#' @return TRUE if the corrected source should be used, FALSE otherwise.
#' @noRd
#' @keywords internal
continuous_trace_uses_corrected_source <- function(
  con,
  timeseries_id,
  start_date,
  end_date
) {
  ts_type <- DBI::dbGetQuery(
    con,
    "SELECT timeseries_type FROM continuous.timeseries WHERE timeseries_id = $1",
    params = list(timeseries_id)
  )
  if (
    nrow(ts_type) > 0 &&
      !is.na(ts_type$timeseries_type[1]) &&
      ts_type$timeseries_type[1] != "basic"
  ) {
    return(TRUE)
  }

  if (!DBI::dbExistsTable(
    con,
    DBI::Id(schema = "continuous", table = "measurements_continuous")
  )) {
    return(TRUE)
  }

  corrections_apply <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT correction_id FROM continuous.corrections ",
      "WHERE timeseries_id = $1 AND start_dt <= $2 AND end_dt >= $3 ",
      "LIMIT 1;"
    ),
    params = list(timeseries_id, end_date, start_date)
  )

  nrow(corrections_apply) > 0
}

#' Normalize the daily-statistics period selector
#' @param stats_period One of "full" or "30yr".
#' @return A normalized period string.
#' @noRd
#' @keywords internal
normalize_daily_stats_period <- function(stats_period) {
  stats_period <- match.arg(stats_period, c("full", "30yr"))
  stats_period
}

#' Resolve the historic-statistics window shown in plot-data exports
#' @param stats_period One of "full" or "30yr".
#' @param trace_data Exported trace data with a datetime column.
#' @param range_data Exported range data with a datetime column.
#' @param timeseries_start Full record start datetime.
#' @param timeseries_end Full record end datetime.
#' @return A list with formatted start and end values.
#' @noRd
#' @keywords internal
historic_stats_export_window <- function(
  stats_period = "full",
  trace_data = NULL,
  range_data = NULL,
  timeseries_start = NULL,
  timeseries_end = NULL
) {
  stats_period <- normalize_daily_stats_period(stats_period)
  na_posix <- as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")

  as_utc_posix <- function(x) {
    if (is.null(x) || length(x) == 0) {
      return(na_posix)
    }
    x <- x[!is.na(x)]
    if (length(x) == 0) {
      return(na_posix)
    }
    out <- suppressWarnings(as.POSIXct(x, tz = "UTC"))
    attr(out, "tzone") <- "UTC"
    out[!is.na(out)]
  }

  min_datetime <- function(x) {
    x <- as_utc_posix(x)
    if (length(x) == 0 || all(is.na(x))) {
      return(na_posix)
    }
    min(x)
  }

  max_datetime <- function(x) {
    x <- as_utc_posix(x)
    if (length(x) == 0 || all(is.na(x))) {
      return(na_posix)
    }
    max(x)
  }

  full_start <- min_datetime(timeseries_start)
  full_end <- max_datetime(timeseries_end)
  export_start <- min_datetime(trace_data$datetime)
  export_end <- max_datetime(trace_data$datetime)

  if (is.data.frame(range_data) && "datetime" %in% names(range_data)) {
    range_start <- min_datetime(range_data$datetime)
    range_end <- max_datetime(range_data$datetime)
    if (!is.na(range_start)) {
      export_start <- range_start
    }
    if (!is.na(range_end)) {
      export_end <- range_end
    }
  }

  if (is.na(full_start)) {
    full_start <- export_start
  }
  if (is.na(full_end)) {
    full_end <- export_end
  }

  stats_start <- full_start
  stats_end <- full_end
  if (identical(stats_period, "30yr")) {
    stats_start <- if (is.na(export_start)) {
      full_start
    } else {
      lubridate::add_with_rollback(export_start, -lubridate::years(30))
    }
    if (!is.na(full_start) && !is.na(stats_start) && stats_start < full_start) {
      stats_start <- full_start
    }

    stats_end <- export_end
    if (!is.na(full_end) && !is.na(stats_end) && stats_end > full_end) {
      stats_end <- full_end
    }
  }

  start_text <- if (is.na(stats_start)) {
    NA_character_
  } else {
    format(stats_start, "%Y-%m-%d %H:%M")
  }
  end_text <- if (is.na(stats_end)) {
    NA_character_
  } else {
    format(stats_end, "%Y-%m-%d %H:%M")
  }

  list(
    start = start_text,
    end = end_text,
    start_year = if (is.na(stats_start)) {
      NA_character_
    } else {
      format(stats_start, "%Y")
    },
    end_year = if (is.na(stats_end)) {
      NA_character_
    } else {
      format(stats_end, "%Y")
    }
  )
}

#' Prepare historic-range data for plot-data exports
#' @param range_data Plot range data returned by continuous plotting helpers.
#' @param units Unit label for exported historic-stat columns.
#' @return A data frame ready for XLSX export, or NULL when no range exists.
#' @noRd
#' @keywords internal
historic_range_data_for_export <- function(range_data, units) {
  expected_cols <- c("datetime", "min", "max", "q75", "q25")
  if (
    !is.data.frame(range_data) ||
      nrow(range_data) == 0L ||
      !all(expected_cols %in% names(range_data))
  ) {
    return(NULL)
  }

  stat_cols <- c("min", "max", "q75", "q25")
  has_complete_stats <- Reduce(
    `&`,
    lapply(stat_cols, function(col) !is.na(range_data[[col]]))
  )
  if (!any(has_complete_stats)) {
    return(NULL)
  }

  range_data <- data.table::as.data.table(range_data)[, ..expected_cols]
  data.table::setnames(
    range_data,
    expected_cols,
    c(
      "datetime_UTC",
      paste0("historic_min_", units),
      paste0("historic_max_", units),
      paste0("historic_Q75_", units),
      paste0("historic_Q25_", units)
    )
  )
  as.data.frame(range_data)
}

#' Build CSV tables for continuous plot data downloads
#' @param req Plot request list created by the Shiny module.
#' @param out Plot data returned by the plotting task.
#' @param module_data Module lookup data used for metadata labels.
#' @param language Current application language object.
#' @return A named list of data frames ready for CSV export.
#' @noRd
#' @keywords internal
continuous_plot_export_tables <- function(req, out, module_data, language) {
  safe_first_value <- function(data, key_col, key_value, value_col) {
    if (
      is.null(data) ||
        !all(c(key_col, value_col) %in% names(data)) ||
        is.null(key_value) ||
        length(key_value) == 0L ||
        all(is.na(key_value))
    ) {
      return(NA)
    }

    rows <- which(data[[key_col]] %in% key_value)
    if (length(rows) == 0L) {
      return(NA)
    }

    value <- data[[value_col]][[rows[[1L]]]]
    if (length(value) == 0L || is.null(value)) {
      return(NA)
    }
    value
  }

  format_range_datetime <- function(x, range_fn) {
    if (is.null(x) || length(x) == 0L) {
      return(NA_character_)
    }
    x <- x[!is.na(x)]
    if (length(x) == 0L) {
      return(NA_character_)
    }
    format(range_fn(x), "%Y-%m-%d %H:%M")
  }

  base_metadata <- data.frame(
    Attribute = c(
      "Generated on:",
      "Plot type:",
      "Timeseries IDs:",
      "Plot language:",
      "Plot timezone:",
      "Plot resolution:"
    ),
    Value = c(
      paste0(substr(.POSIXct(Sys.time(), tz = "UTC"), 1, 16), " UTC"),
      req$plot_type,
      paste(req$timeseries_ids, collapse = ", "),
      req$lang,
      req$plot_timezone,
      if (!is.null(req$plot_resolution)) {
        req$plot_resolution
      } else {
        NA_character_
      }
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  tables <- list()
  used_names <- character(0)
  add_table <- function(name, data) {
    if (is.null(data) || !is.data.frame(data)) {
      return(invisible(NULL))
    }

    clean_name <- gsub("[^A-Za-z0-9._-]+", "_", name)
    clean_name <- gsub("_+", "_", clean_name)
    clean_name <- gsub("^_+|_+$", "", clean_name)
    if (!nzchar(clean_name)) {
      clean_name <- "data"
    }
    clean_name <- sub("\\.csv$", "", clean_name, ignore.case = TRUE)

    candidate <- clean_name
    suffix <- 1L
    while (candidate %in% used_names) {
      candidate <- paste0(clean_name, "_", suffix)
      suffix <- suffix + 1L
    }

    used_names <<- c(used_names, candidate)
    tables[[candidate]] <<- as.data.frame(data)
    invisible(NULL)
  }

  add_data_recursive <- function(x, prefix = "data") {
    if (is.null(x)) {
      return(invisible(NULL))
    }

    if (is.data.frame(x)) {
      add_table(prefix, x)
      return(invisible(NULL))
    }

    if (is.list(x)) {
      nm <- names(x)
      if (is.null(nm) || any(!nzchar(nm))) {
        nm <- paste0("item", seq_along(x))
      }
      for (i in seq_along(x)) {
        add_data_recursive(x[[i]], prefix = paste(prefix, nm[[i]], sep = "_"))
      }
    }

    invisible(NULL)
  }

  if (
    length(req$timeseries_ids) == 1L &&
      is.list(out) &&
      is.data.frame(out$trace_data)
  ) {
    timeseries <- req$timeseries_id
    loc_id <- safe_first_value(
      module_data$timeseries,
      "timeseries_id",
      timeseries,
      "location_id"
    )
    location <- safe_first_value(
      module_data$locs,
      "location_id",
      loc_id,
      tr("generic_name_col", language$language)
    )
    sloc_id <- safe_first_value(
      module_data$timeseries,
      "timeseries_id",
      timeseries,
      "sub_location_id"
    )
    sub_location <- if (!is.na(sloc_id)) {
      safe_first_value(
        module_data$sub_locs,
        "sub_location_id",
        sloc_id,
        tr("sub_location_col", language$language)
      )
    } else {
      NA
    }
    pid <- safe_first_value(
      module_data$timeseries,
      "timeseries_id",
      timeseries,
      "parameter_id"
    )
    parameter <- safe_first_value(
      module_data$params,
      "parameter_id",
      pid,
      tr("param_name_col", language$language)
    )
    units <- safe_first_value(module_data$params, "parameter_id", pid, "unit")
    date_range_start <- format_range_datetime(out$trace_data$datetime, min)
    date_range_end <- format_range_datetime(out$trace_data$datetime, max)
    range_data <- historic_range_data_for_export(out$range_data, units)

    if (is.null(range_data)) {
      hist_range_start <- NA_character_
      hist_range_end <- NA_character_
    } else {
      hist_window <- historic_stats_export_window(
        stats_period = if (is.null(req$stats_period)) {
          "full"
        } else {
          req$stats_period
        },
        trace_data = out$trace_data,
        range_data = out$range_data,
        timeseries_start = safe_first_value(
          module_data$timeseries,
          "timeseries_id",
          timeseries,
          "start_datetime"
        ),
        timeseries_end = safe_first_value(
          module_data$timeseries,
          "timeseries_id",
          timeseries,
          "end_datetime"
        )
      )
      hist_range_start <- hist_window$start
      hist_range_end <- hist_window$end
    }

    metadata <- rbind(
      base_metadata,
      data.frame(
        Attribute = c(
          "Location:",
          "Sub-location:",
          "Parameter:",
          "Units:",
          "Start of exported data:",
          "End of exported data:",
          "Start historic record for stats calculations:",
          "End historic record for stats calculations:"
        ),
        Value = c(
          location,
          sub_location,
          parameter,
          units,
          date_range_start,
          date_range_end,
          hist_range_start,
          hist_range_end
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    )

    trace_data <- as.data.frame(out$trace_data)
    if (ncol(trace_data) >= 2L) {
      names(trace_data)[1:2] <- c(
        "datetime_UTC",
        paste0(parameter, "_", units)
      )
    }

    add_table("metadata", metadata)
    add_table("trace_data", trace_data)
    add_table("historic_range_data", range_data)
    return(tables)
  }

  add_table("metadata", base_metadata)
  add_data_recursive(out)
  tables
}

#' Write continuous plot export tables to a zipped CSV bundle
#' @param tables Named list of data frames.
#' @param file Output zip file path supplied by Shiny.
#' @return Invisibly returns the output file path.
#' @noRd
#' @keywords internal
write_continuous_plot_csv_zip <- function(tables, file) {
  rlang::check_installed("zip", reason = "to download continuous plot data")

  if (!is.list(tables) || length(tables) == 0L) {
    stop("No continuous plot data tables are available for download.")
  }

  file <- file.path(
    normalizePath(dirname(file), winslash = "/", mustWork = FALSE),
    basename(file)
  )
  export_dir <- tempfile("continuous_plot_export_")
  dir.create(export_dir)
  on.exit(unlink(export_dir, recursive = TRUE, force = TRUE), add = TRUE)

  csv_files <- character(0)
  for (name in names(tables)) {
    table <- tables[[name]]
    if (is.null(table) || !is.data.frame(table)) {
      next
    }

    csv_file <- file.path(export_dir, paste0(name, ".csv"))
    data.table::fwrite(table, csv_file, na = "")
    csv_files <- c(csv_files, csv_file)
  }

  if (length(csv_files) == 0L) {
    stop("No continuous plot data tables are available for download.")
  }

  if (file.exists(file)) {
    unlink(file)
  }
  zip::zipr(zipfile = file, files = basename(csv_files), root = export_dir)
  invisible(file)
}

#' Build the historic-statistics caption used below plot legends
#' @param stats_period One of "full" or "30yr".
#' @param plot_data Plot data returned by continuous plotting helpers.
#' @param timeseries_ids Timeseries IDs included in the plot.
#' @param timeseries_table Timeseries metadata with start/end datetimes.
#' @param lang Plot language.
#' @return Caption text, or NULL when no usable year range is available.
#' @noRd
#' @keywords internal
historic_stats_caption_for_plot_data <- function(
  stats_period = "full",
  plot_data = NULL,
  timeseries_ids = NULL,
  timeseries_table = NULL,
  lang = "en"
) {
  if (is.null(timeseries_ids) || length(timeseries_ids) == 0) {
    return(NULL)
  }
  if (
    is.null(timeseries_table) || !"timeseries_id" %in% names(timeseries_table)
  ) {
    return(NULL)
  }

  plot_data_for_id <- function(index, timeseries_id) {
    if (
      is.list(plot_data) &&
        all(c("trace_data", "range_data") %in% names(plot_data))
    ) {
      return(plot_data)
    }
    if (!is.list(plot_data) || length(plot_data) == 0) {
      return(list(trace_data = NULL, range_data = NULL))
    }

    plot_data_names <- names(plot_data)
    if (
      !is.null(plot_data_names) &&
        as.character(timeseries_id) %in% plot_data_names
    ) {
      return(plot_data[[as.character(timeseries_id)]])
    }
    if (length(plot_data) >= index) {
      return(plot_data[[index]])
    }
    list(trace_data = NULL, range_data = NULL)
  }

  windows <- lapply(seq_along(timeseries_ids), function(i) {
    timeseries_id <- timeseries_ids[[i]]
    row_index <- match(
      as.character(timeseries_id),
      as.character(timeseries_table[["timeseries_id"]])
    )
    if (is.na(row_index)) {
      return(NULL)
    }

    item_data <- plot_data_for_id(i, timeseries_id)
    historic_stats_export_window(
      stats_period = stats_period,
      trace_data = item_data$trace_data,
      range_data = item_data$range_data,
      timeseries_start = timeseries_table[["start_datetime"]][[row_index]],
      timeseries_end = timeseries_table[["end_datetime"]][[row_index]]
    )
  })
  windows <- Filter(Negate(is.null), windows)
  if (length(windows) == 0) {
    return(NULL)
  }

  start_years <- suppressWarnings(as.integer(vapply(
    windows,
    function(window) window$start_year,
    character(1)
  )))
  end_years <- suppressWarnings(as.integer(vapply(
    windows,
    function(window) window$end_year,
    character(1)
  )))
  start_years <- start_years[!is.na(start_years)]
  end_years <- end_years[!is.na(end_years)]
  if (length(start_years) == 0 || length(end_years) == 0) {
    return(NULL)
  }

  start_year <- min(start_years)
  end_year <- max(end_years)
  varies <- length(unique(start_years)) > 1 || length(unique(end_years)) > 1

  if (identical(lang, "fr")) {
    caption <- if (start_year == end_year) {
      sprintf(
        "Plage historique calcul\u00E9e avec les donn\u00E9es de %s.",
        start_year
      )
    } else {
      sprintf(
        "Plage historique calcul\u00E9e avec les donn\u00E9es de %s \u00E0 %s.",
        start_year,
        end_year
      )
    }
    if (isTRUE(varies)) {
      caption <- paste(
        caption,
        "La plage varie selon la s\u00E9rie chronologique."
      )
    }
    return(caption)
  }

  caption <- if (start_year == end_year) {
    sprintf("Historic range calculated with data from %s.", start_year)
  } else {
    sprintf(
      "Historic range calculated with data from %s to %s.",
      start_year,
      end_year
    )
  }
  if (isTRUE(varies)) {
    caption <- paste(caption, "Range varies by timeseries.")
  }
  caption
}

#' Build stable daily-statistics SELECT expressions
#' @param con A DBI database connection object.
#' @param stats_period One of "full" or "30yr".
#' @param columns Base daily-statistics columns to select.
#' @param table_alias Optional table alias to prefix column references.
#' @return A character vector of SQL SELECT expressions.
#' @noRd
#' @keywords internal
daily_stats_select_sql <- function(
  con,
  stats_period = "full",
  columns = c("min", "max", "q75", "q25"),
  table_alias = NULL
) {
  stats_period <- normalize_daily_stats_period(stats_period)
  available <- DBI::dbGetQuery(
    con,
    "SELECT column_name
     FROM information_schema.columns
     WHERE table_schema = 'continuous'
       AND table_name = 'measurements_calculated_daily'"
  )$column_name

  suffix <- ""
  if (
    identical(stats_period, "30yr") &&
      all(paste0(columns, "_30yr") %in% available)
  ) {
    suffix <- "_30yr"
  }

  prefix <- if (is.null(table_alias) || !nzchar(table_alias)) {
    ""
  } else {
    paste0(table_alias, ".")
  }

  vapply(
    columns,
    function(column) {
      source_column <- paste0(column, suffix)
      source_sql <- paste0(
        prefix,
        as.character(DBI::dbQuoteIdentifier(con, source_column))
      )
      if (identical(source_column, column)) {
        source_sql
      } else {
        paste0(
          source_sql,
          " AS ",
          as.character(DBI::dbQuoteIdentifier(con, column))
        )
      }
    },
    character(1)
  )
}

#' Normalize the as_of input to a POSIXct object in UTC timezone
#' @param as_of The input value for as_of, which can be NULL, character, Date, or POSIXct.
#' @param tzone The timezone to use for parsing character inputs, which can be a string or numeric offset.
#' @return A POSIXct object in UTC timezone, or NULL if as_of is NULL.
#' @noRd
#' @keywords internal
normalize_as_of_input <- function(as_of, tzone) {
  if (is.null(as_of)) {
    return(NULL)
  }

  if (length(as_of) != 1) {
    stop("`as_of` must be NULL or a single date/datetime value.")
  }

  if (is.character(tzone)) {
    numeric_tzone <- suppressWarnings(as.numeric(tzone))
    if (!is.na(numeric_tzone) && grepl("^[-+]?\\d+$", trimws(tzone))) {
      tzone <- numeric_tzone
    }
  }

  if (is.numeric(tzone)) {
    if (length(tzone) != 1 || is.na(tzone) || tzone %% 1 != 0) {
      stop("Numeric timezone offsets must be a single whole hour value.")
    }
    if (tzone == 0) {
      tzone <- "UTC"
    } else {
      tzone <- sprintf("Etc/GMT%+d", -as.integer(tzone))
    }
  }

  if (inherits(as_of, "character")) {
    as_of_text <- trimws(as.character(as_of[[1]]))
    has_time_component <- grepl("[ T]\\d{1,2}:\\d{2}", as_of_text) ||
      grepl("(Z|[+-]\\d{2}:?\\d{2})$", as_of_text)

    if (has_time_component) {
      as_of <- suppressWarnings(as.POSIXct(as_of_text, tz = tzone))
    } else {
      as_of <- suppressWarnings(as.POSIXct(as.Date(as_of_text), tz = tzone))
      as_of <- as_of + 24 * 60 * 60
    }
  } else if (inherits(as_of, "Date") && !inherits(as_of, "POSIXt")) {
    as_of <- as.POSIXct(as_of, tz = tzone)
    as_of <- as_of + 24 * 60 * 60
  } else if (inherits(as_of, "POSIXt")) {
    as_of <- as.POSIXct(as_of, tz = tzone)
  }

  if (!inherits(as_of, "POSIXt") || is.na(as_of)) {
    stop(
      "`as_of` must be NULL or a single character, Date, or POSIXct value."
    )
  }

  attr(as_of, "tzone") <- "UTC"
  as_of
}

#' Normalize a plot date or datetime range bound
#' @param value Single character, Date, or POSIXct range bound.
#' @param tzone Timezone used to interpret date-only values.
#' @param bound Either "start" or "end". Date-only start bounds resolve to the
#'   start of the selected day; date-only end bounds resolve to the start of
#'   the following day so the selected end day is included.
#' @param arg_name Argument name used in error messages.
#' @return A POSIXct datetime with UTC timezone metadata.
#' @noRd
#' @keywords internal
normalize_plot_datetime_bound <- function(
  value,
  tzone,
  bound = c("start", "end"),
  arg_name = NULL
) {
  bound <- match.arg(bound)
  if (is.null(arg_name)) {
    arg_name <- paste0(bound, "_date")
  }

  if (is.null(value) || length(value) != 1L || is.na(value[[1L]])) {
    stop("`", arg_name, "` must be a single date/datetime value.")
  }

  date_only <- FALSE
  if (inherits(value, "character")) {
    value_text <- trimws(as.character(value[[1L]]))
    has_time_component <- grepl("[ T]\\d{1,2}:\\d{2}", value_text) ||
      grepl("(Z|[+-]\\d{2}:?\\d{2})$", value_text)

    if (has_time_component) {
      value <- suppressWarnings(as.POSIXct(value_text, tz = tzone))
    } else {
      value <- suppressWarnings(as.Date(value_text))
      if (!is.na(value)) {
        date_only <- TRUE
        value <- as.POSIXct(
          paste(value, "00:00:00"),
          tz = tzone
        )
      }
    }
  } else if (inherits(value, "Date") && !inherits(value, "POSIXt")) {
    date_only <- TRUE
    value <- as.POSIXct(
      paste(value, "00:00:00"),
      tz = tzone
    )
  } else if (inherits(value, "POSIXt")) {
    value <- as.POSIXct(value, tz = tzone)
  }

  if (!inherits(value, "POSIXt") || is.na(value)) {
    stop(
      "`",
      arg_name,
      "` must be a single character, Date, or POSIXct value."
    )
  }

  if (date_only && bound == "end") {
    value <- value + 24 * 60 * 60
  }

  attr(value, "tzone") <- "UTC"
  value
}

#' Format the as_of value for use in plot titles, respecting the specified timezone and language
#' @param as_of A POSIXct object representing the as_of datetime, or NULL.
#' @param tzone The timezone to use for formatting the datetime, which can be a string or numeric offset.
#' @param lang The language code for formatting the title, either "en" for English or "fr" for French.
#' @return A formatted string for the plot title, or NULL if as_of is NULL.
#' @noRd
#' @keywords internal
format_as_of_title <- function(as_of, tzone, lang = "en") {
  if (is.null(as_of)) {
    return(NULL)
  }

  if (lang == "fr") {
    paste0("Donn\u00e9es au ", format(as_of, tz = tzone, usetz = TRUE))
  } else {
    paste0("As of ", format(as_of, tz = tzone, usetz = TRUE))
  }
}

#' Fetch continuous quality-control intervals
#'
#' @description
#' Returns grade, approval, or qualifier intervals for one timeseries. When
#' `as_of` is supplied, both the interval assignments and their type metadata
#' are reconstructed at that timestamp by the database audit function.
#'
#' @param con A DBI database connection.
#' @param timeseries_id Integer timeseries identifier.
#' @param start_date,end_date Datetime bounds used to limit overlapping
#'   intervals.
#' @param qc_type One of `"grade"`, `"approval"`, or `"qualifier"`.
#' @param as_of Optional point-in-time timestamp.
#'
#' @return A data.table with standardized QC type columns.
#' @noRd
#' @keywords internal
fetch_continuous_qc_intervals <- function(
  con,
  timeseries_id,
  start_date,
  end_date,
  qc_type = c("grade", "approval", "qualifier"),
  as_of = NULL
) {
  qc_type <- match.arg(qc_type)

  if (is.null(as_of)) {
    qc_config <- switch(
      qc_type,
      grade = list(
        interval_table = "continuous.grades",
        type_table = "public.grade_types",
        type_id = "grade_type_id",
        type_code = "grade_type_code",
        type_description = "grade_type_description",
        type_description_fr = "grade_type_description_fr"
      ),
      approval = list(
        interval_table = "continuous.approvals",
        type_table = "public.approval_types",
        type_id = "approval_type_id",
        type_code = "approval_type_code",
        type_description = "approval_type_description",
        type_description_fr = "approval_type_description_fr"
      ),
      qualifier = list(
        interval_table = "continuous.qualifiers",
        type_table = "public.qualifier_types",
        type_id = "qualifier_type_id",
        type_code = "qualifier_type_code",
        type_description = "qualifier_type_description",
        type_description_fr = "qualifier_type_description_fr"
      )
    )

    statement <- sprintf(
      "SELECT
         qc.start_dt,
         qc.end_dt,
         qt.%s AS qc_type_code,
         qt.%s AS qc_type_description,
         qt.%s AS qc_type_description_fr,
         qt.color_code
       FROM %s qc
       LEFT JOIN %s qt
         ON qt.%s = qc.%s
       WHERE qc.timeseries_id = $1
         AND qc.end_dt >= $2
         AND qc.start_dt <= $3
       ORDER BY qc.start_dt, qc.end_dt",
      qc_config$type_code,
      qc_config$type_description,
      qc_config$type_description_fr,
      qc_config$interval_table,
      qc_config$type_table,
      qc_config$type_id,
      qc_config$type_id
    )
    params <- list(timeseries_id, start_date, end_date)
  } else {
    statement <- "SELECT
         start_dt,
         end_dt,
         type_code AS qc_type_code,
         type_description AS qc_type_description,
         type_description_fr AS qc_type_description_fr,
         color_code
       FROM audit.continuous_qc_intervals_as_of(
         $1,
         ARRAY[$2]::INTEGER[],
         $3,
         $4,
         ARRAY[$5]::TEXT[]
       )
       ORDER BY start_dt, end_dt"
    params <- list(as_of, timeseries_id, start_date, end_date, qc_type)
  }

  dbGetQueryDT(con, statement, params = params)
}

#' @title Fetch hourly trace data
#' @description Fetches hourly aggregated trace data for a given timeseries and date range, with options for using corrected values and specifying an as_of datetime.
#' @param con A DBI database connection object.
#' @param timeseries_id The ID of the timeseries to fetch data for.
#' @param start_date The start date of the range to fetch data for (POSIXct or character).
#' @param end_date The end date of the range to fetch data for (POSIXct or character).
#' @param raw Logical indicating whether to include raw values in the output.
#' @param use_corrected_source Logical indicating whether to use the corrected source table for fetching data.
#' @param as_of An optional datetime (POSIXct, character, or Date) to fetch data as of a specific point in time, which will override the use_corrected_source parameter if provided.
#' @return A data.table containing the hourly aggregated trace data, with columns for datetime, value, and optionally value_raw and imputed.
#' @noRd
#' @keywords internal
fetch_hourly_trace_data <- function(
  con,
  timeseries_id,
  start_date,
  end_date,
  raw = FALSE,
  use_corrected_source = TRUE,
  as_of = NULL
) {
  if (is.null(as_of)) {
    if (isTRUE(use_corrected_source)) {
      source_table <- "continuous.measurements_continuous_corrected($1, $2, $3)"
      value_col <- "value_corrected"
      raw_value_col <- "value_raw"
      where_sql <- ""
      params <- list(timeseries_id, start_date, end_date)
    } else {
      source_table <- "measurements_continuous"
      value_col <- "value"
      raw_value_col <- "value"
      where_sql <- "WHERE m.timeseries_id = $1 AND m.datetime BETWEEN $2 AND $3 "
      params <- list(timeseries_id, start_date, end_date)
    }
  } else {
    source_table <- paste0(
      "continuous.measurements_continuous_corrected_at(",
      "$1, $2, $3, $4",
      ")"
    )
    value_col <- "value_corrected"
    raw_value_col <- "value_raw"
    where_sql <- ""
    params <- list(as_of, timeseries_id, start_date, end_date)
  }

  agg_sql <- function(column_name) {
    paste0(
      "CASE ",
      "WHEN at.aggregation_type = 'sum' THEN SUM(m.",
      column_name,
      ") ",
      "WHEN at.aggregation_type = 'median' THEN percentile_cont(0.5) ",
      "WITHIN GROUP (ORDER BY m.",
      column_name,
      ") ",
      "WHEN at.aggregation_type IN ('min', 'minimum') THEN MIN(m.",
      column_name,
      ") ",
      "WHEN at.aggregation_type IN ('max', 'maximum') THEN MAX(m.",
      column_name,
      ") ",
      "WHEN at.aggregation_type = '(min+max)/2' THEN ",
      "(MIN(m.",
      column_name,
      ") + MAX(m.",
      column_name,
      ")) / 2.0 ",
      "ELSE AVG(m.",
      column_name,
      ") END"
    )
  }

  query <- paste0(
    "SELECT ",
    "date_trunc('hour', m.datetime) AS datetime, ",
    agg_sql(value_col),
    " AS value, ",
    if (isTRUE(raw)) {
      paste0(agg_sql(raw_value_col), " AS value_raw, ")
    } else {
      ""
    },
    "BOOL_OR(COALESCE(m.imputed, FALSE)) AS imputed ",
    "FROM ",
    source_table,
    " m ",
    "LEFT JOIN continuous.timeseries ts ON m.timeseries_id = ts.timeseries_id ",
    "LEFT JOIN continuous.aggregation_types at ",
    "ON ts.aggregation_type_id = at.aggregation_type_id ",
    where_sql,
    "GROUP BY date_trunc('hour', m.datetime), at.aggregation_type ",
    "ORDER BY datetime ASC;"
  )

  hourly <- dbGetQueryDT(
    con,
    query,
    params = params
  )

  if (nrow(hourly) == 0) {
    if (isTRUE(raw)) {
      return(data.table::data.table(
        datetime = as.POSIXct(character(), tz = "UTC"),
        value = numeric(),
        value_raw = numeric(),
        imputed = logical()
      ))
    }

    return(data.table::data.table(
      datetime = as.POSIXct(character(), tz = "UTC"),
      value = numeric(),
      imputed = logical()
    ))
  }

  hourly$datetime <- as.POSIXct(hourly$datetime, tz = "UTC")
  attr(hourly$datetime, "tzone") <- "UTC"
  hourly
}

#' @title Add gap markers to trace data
#' @description Inserts rows with NA values into the trace data where there are gaps in the datetime sequence that exceed a specified period, which helps to visually indicate gaps in the data when plotting.
#' @param trace_data A data.table containing the trace data with a datetime column.
#' @param period_seconds The threshold in seconds for identifying gaps in the datetime sequence. If the difference between consecutive datetimes exceeds this threshold, a gap marker will be inserted.
#' @return A data.table with gap markers (rows with NA values) inserted where there are gaps in the datetime sequence.
#' @noRd
#' @keywords internal
add_gap_markers <- function(trace_data, period_seconds) {
  if (nrow(trace_data) == 0 || !("datetime" %in% names(trace_data))) {
    return(trace_data)
  }

  trace_data <- data.table::as.data.table(trace_data)
  data.table::setorder(trace_data, "datetime")

  trace_data[,
    "next_datetime" := data.table::shift(trace_data$datetime, type = "lead")
  ]
  gap_indices <- which(
    !is.na(trace_data$next_datetime) &
      as.numeric(
        trace_data$next_datetime - trace_data$datetime,
        units = "secs"
      ) >
        period_seconds
  )

  if (length(gap_indices) == 0) {
    trace_data[, "next_datetime" := NULL]
    return(trace_data)
  }

  na_rows <- data.table::data.table(
    datetime = trace_data$datetime[gap_indices] + 1
  )
  other_cols <- setdiff(names(trace_data), c("datetime", "next_datetime"))
  for (col in other_cols) {
    na_rows[[col]] <- NA
  }
  if ("imputed" %in% names(na_rows)) {
    na_rows[, "imputed" := FALSE]
  }

  trace_data[, "next_datetime" := NULL]
  out <- data.table::rbindlist(
    list(trace_data, na_rows),
    use.names = TRUE,
    fill = TRUE
  )
  data.table::setorder(out, "datetime")
  out
}
