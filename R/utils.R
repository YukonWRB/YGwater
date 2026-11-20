#' Find the translation for a given placeholder
#'
#' @description
#' This function retrieves the translation for a specified key and language
#' from a provided translations list. Allows for referencing only the key in
#' function code, with the language specified at runtime.
#'
#' @param key A string representing the translation key. Must be a name in the 'id' column of the translations data.frame, else an error will be generated.
#' @param lang A string representing the target language code. Must be a name in the translations list, else an error will be generated.
#' @param translations A named list (with names representing the 'lang') of named character vectors (with names as the 'key') containing translations for different languages. Defaults to `data$translations`, package internal data but can be overridden with a custom translations list.
#' @return A string representing the translated text for the specified key and language.
#' @export

tr <- function(key, lang, translations = data$translations) {
  # Ensure that 'lang' is a name in the translations list
  if (is.na(lang) || !nzchar(lang)) {
    stop("Language code is NA or empty.")
  }
  if (!lang %in% names(translations)) {
    stop(
      "Language ",
      lang,
      " not found in translations data."
    )
  }
  if (is.na(key) || !nzchar(key)) {
    stop("Translation key is NA or empty.")
  }

  # Ensure that 'key' is a value in the 'id' column of the translations data.frame
  if (!key %in% names(translations[[lang]])) {
    stop(
      "Translation key ",
      key,
      " not found in translations data."
    )
  }
  return(translations[[lang]][[key]]) # list 'lang', item 'key'
}

#' Request a yes-or-no answer at the console
#'
#' @param ... Prompt text passed to `cli::cat_bullet()`.
#' @return `TRUE` if Yes was selected, otherwise `FALSE`.
#' @keywords internal
#' @noRd
ask <- function(...) {
  choices <- c("Yes", "No")
  cli::cat_bullet(..., bullet = "arrow", col = "darkgreen")
  utils::menu(choices) == which(choices == "Yes")
}

#' Round to a specified accuracy
#'
#' @param x Numeric vector to round.
#' @param accuracy Number to round to.
#' @param f Rounding function such as floor, ceiling, or round.
#' @return `x` rounded to multiples of `accuracy` using `f`.
#' @keywords internal
#' @noRd
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

#' Replace infinite and NaN values with NA
#'
#' Utility function to replace `Inf`, `-Inf`, and `NaN` values with `NA`.
#'
#' @param x Numeric vector, data.frame, or data.table. If data.frame or data.table, will only work on 'numeric' class columns.
#' @return Numeric vector with infinite values converted to `NA`.
#' @export
inf_to_na <- function(x) {
  # data.table
  if (data.table::is.data.table(x)) {
    num_cols <- names(x)[vapply(x, is.numeric, logical(1))]
    if (length(num_cols)) {
      x[,
        (num_cols) := lapply(.SD, function(col) {
          col[!is.finite(col)] <- NA
          col
        }),
        .SDcols = num_cols
      ]
    }
    return(x)
  }

  # data.frame / tibble
  if (is.data.frame(x)) {
    # TRUE for tibbles or data.frames (and data.tables, but these are dealt with differently)
    numeric_cols <- sapply(x, is.numeric)
    x[numeric_cols] <- lapply(x[numeric_cols], function(col) {
      col[!is.finite(col)] <- NA
      col
    })
    return(x)
  }

  # vector
  if (is.numeric(x)) {
    x[!is.finite(x)] <- NA
    return(x)
  }

  # If x is not numeric, return it unchanged
  warning("Input is not numeric. Returning unchanged.")
  return(x)
}

#' Convert hours to ISO 8601 duration format
#'
#' Converts a numeric value representing hours into an ISO 8601 duration string.
#'
#' @param x Numeric value representing hours.
#' @return A string in ISO 8601 duration format (e.g., "P1DT2H30M0S").
#' @noRd
#' @examples
#' iso_period(26.5)  # Returns "P1DT2H30M0S"

iso_period <- function(x) {
  if (length(x) == 0) {
    return(character())
  }

  total_seconds <- round(x * 3600)
  days <- total_seconds %/% (24 * 3600)
  remainder <- total_seconds %% (24 * 3600)
  hours <- remainder %/% 3600
  remainder <- remainder %% 3600
  minutes <- remainder %/% 60
  seconds <- remainder %% 60

  result <- sprintf("P%dDT%dH%dM%dS", days, hours, minutes, seconds)
  invalid <- is.na(total_seconds) | !is.finite(total_seconds)
  result[invalid] <- NA_character_
  result
}

#' Lookup a location_id from user input
#'
#' Prefers location_id when the input is numeric or coercible to numeric.
#' Otherwise searches name, name_fr, location_code, and alias.
#'
#' @param con A DBI connection.
#' @param location A location identifier (numeric or character).
#' @return A single location_id, or NA if not found.
#' @noRd
lookup_location_id <- function(con, location) {
  location_txt <- as.character(location)
  location_num <- suppressWarnings(as.integer(location_txt))
  if (!is.na(location_num) && nzchar(location_txt)) {
    result <- DBI::dbGetQuery(
      con,
      "SELECT location_id FROM public.locations WHERE location_id = $1 LIMIT 1;",
      params = list(location_num)
    )
    return(result[1, 1])
  }

  result <- DBI::dbGetQuery(
    con,
    "SELECT location_id FROM public.locations WHERE location_code = $1 OR alias = $1 OR name = $1 OR name_fr = $1 LIMIT 1;",
    params = list(location_txt)
  )
  result[1, 1]
}

#' Check if a table exists in the database
#' Checks for the existence of a table in the database, handling any errors that may occur during the check and returning FALSE if the table does not exist or if an error occurs.
#' @param con A DBI connection to the database.
#' @param schema A string representing the schema name where the table is expected to be found.
#' @param table A string representing the table name to check for existence.
#' @return A logical value indicating whether the specified table exists in the database.
#' @noRd

ac_db_table_exists <- function(con, schema, table) {
  tryCatch(
    {
      DBI::dbExistsTable(con, DBI::Id(schema = schema, table = table))
    },
    error = function(...) {
      FALSE
    }
  )
}

#' Generate a SQL column reference with optional table alias
#' Constructs a SQL column reference string, optionally prefixed with a table alias. If a table alias is provided and is not empty, the column reference will be in the format "alias.column_name". If no alias is provided, the column reference will simply be "column_name".
#' @param table_alias An optional string representing the table alias to prefix the column name with. If NULL or an empty string is provided, the column name will be returned without a prefix.
#' @param column_name A string representing the name of the column to reference in the SQL query.
#' @return A string representing the SQL column reference, optionally prefixed with the table alias.
#' @noRd
ac_sql_column_ref <- function(table_alias = NULL, column_name) {
  if (is.null(table_alias) || !nzchar(table_alias)) {
    return(column_name)
  }

  paste0(table_alias, ".", column_name)
}

#' Build SQL to select a parameter unit
#'
#' With matrix or media context, resolve the unit for that context. Otherwise
#' use the preferred unit from the parameter's matrix-specific unit IDs.
#' @param con A DBI connection for the query that will use the expression.
#' @param parameter_alias Optional alias for the parameters table.
#' @param output_alias Column name for the selected unit.
#' @param prefer Which unit to prefer when no matrix context is supplied.
#' @param ... Optional matrix state and media aliases and column names.
#' @return A SQL selection expression for the unit name.
#' @export
ac_parameter_unit_select_sql <- function(
  con,
  parameter_alias = NULL,
  output_alias = "unit_default",
  prefer = c("default_or_solid", "default", "solid", "solid_or_default"),
  ...
) {
  prefer <- match.arg(prefer)
  dots <- list(...)
  parameter_id <- ac_sql_column_ref(parameter_alias, "parameter_id")
  matrix_state <- "NULL"
  if (length(dots$matrix_state_alias) && nzchar(dots$matrix_state_alias)) {
    column <- if (is.null(dots$matrix_state_column)) "matrix_state_id" else dots$matrix_state_column
    matrix_state <- ac_sql_column_ref(
      dots$matrix_state_alias, column
    )
  }
  if (length(dots$media_alias) && nzchar(dots$media_alias)) {
    column <- if (is.null(dots$media_column)) "media_id" else dots$media_column
    media <- ac_sql_column_ref(dots$media_alias, column)
    matrix_state <- sprintf(
      "public.resolve_matrix_state_id(%s, %s, %s)", media, parameter_id, matrix_state
    )
  }
  if (matrix_state != "NULL") {
    return(sprintf(
      "public.get_parameter_unit_name(%s, %s) AS %s",
      parameter_id, matrix_state, output_alias
    ))
  }

  columns <- c("liquid_unit_id", "solid_unit_id", "gas_unit_id", "not_applicable_unit_id")
  if (prefer %in% c("solid", "solid_or_default")) {
    columns <- columns[c(2L, 1L, 3L, 4L)]
  }
  references <- vapply(columns, function(column) {
    ac_sql_column_ref(parameter_alias, column)
  }, character(1))
  units <- sprintf("(SELECT u.unit_name FROM public.units u WHERE u.unit_id = %s)", references)
  sprintf("COALESCE(%s) AS %s", paste(units, collapse = ", "), output_alias)
}

#' Get the unit for a parameter from the database
#' Retrieves the unit associated with a given parameter_id from the database, using the specified preference for which unit to return if multiple are available. The preference can be set to prioritize the default unit, the solid unit, or a combination of both. If no unit is found, returns NA.
#' @param con A DBI connection to the database containing parameter and unit information.
#' @param parameter_id An integer representing the parameter_id for which to retrieve the unit.
#' @param prefer A string indicating the preference for which unit to return if multiple are available. Options are "default_or_solid" (default), "default", "solid", and "solid_or_default".
#' @return A string representing the unit associated with the specified parameter_id, or NA if no unit is found.
#' @export

ac_get_parameter_unit <- function(
  con,
  parameter_id,
  prefer = c("default_or_solid", "default", "solid", "solid_or_default")
) {
  prefer <- match.arg(prefer)
  unit_sql <- ac_parameter_unit_select_sql(
    con = con,
    parameter_alias = "p",
    output_alias = "unit_name",
    prefer = prefer
  )

  out <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT ",
      unit_sql,
      " FROM public.parameters p WHERE p.parameter_id = $1;"
    ),
    params = list(parameter_id)
  )

  if (!nrow(out)) {
    return(NA_character_)
  }

  out$unit_name[[1]]
}

#' Get the unit for a timeseries from the database
#' @param con A DBI connection to the database containing parameter and
#'   timeseries information.
#' @param timeseries_id An integer representing the timeseries_id for which to
#'   retrieve the resolved unit.
#' @return A string representing the unit associated with the specified
#'   timeseries_id, or NA if no unit is found.
#' @noRd

ac_get_timeseries_unit <- function(con, timeseries_id) {
  unit_sql <- ac_parameter_unit_select_sql(
    con = con,
    parameter_alias = "p",
    output_alias = "unit_name",
    matrix_state_alias = "ts",
    media_alias = "ts"
  )

  out <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT",
      unit_sql,
      "FROM public.parameters p",
      "JOIN continuous.timeseries ts",
      "ON ts.parameter_id = p.parameter_id",
      "WHERE ts.timeseries_id = $1;"
    ),
    params = list(timeseries_id)
  )

  if (!nrow(out)) {
    return(NA_character_)
  }

  out$unit_name[[1]]
}

#' Get unit conversion parameters from the database
#' Retrieves the conversion parameters (conversion type, scale_a, and scale_b) for converting values from 'from_unit' to 'to_unit' from the database. If no conversion is found, returns NULL. If 'from_unit' and 'to_unit' are identical or empty, returns a conversion with a factor of 1 and an offset of 0.
#' @param con A DBI connection to the database containing unit conversion information.
#' @param from_unit A string representing the unit  of the input values.
#' @param to_unit A string representing the desired unit for the output values.
#' @return A data.frame with columns 'conversion_type', 'scale_a', and 'scale_b' containing the conversion parameters, or NULL if no conversion is found.
#' @noRd

ac_get_unit_conversion <- function(con, from_unit, to_unit) {
  from_unit <- trimws(as.character(from_unit)[1])
  to_unit <- trimws(as.character(to_unit)[1])

  if (
    is.na(from_unit) ||
      is.na(to_unit) ||
      !nzchar(from_unit) ||
      !nzchar(to_unit) ||
      identical(from_unit, to_unit)
  ) {
    return(
      data.frame(
        conversion_type = "factor",
        scale_a = 1,
        scale_b = 0
      )
    )
  }

  if (
    !ac_db_table_exists(con, "public", "units") ||
      !ac_db_table_exists(con, "public", "unit_conversions")
  ) {
    return(NULL)
  }

  out <- DBI::dbGetQuery(
    con,
    "SELECT uc.conversion_type, uc.scale_a, uc.scale_b
     FROM public.unit_conversions uc
     JOIN public.units u_from
       ON u_from.unit_id = uc.from_unit_id
     JOIN public.units u_to
       ON u_to.unit_id = uc.to_unit_id
     WHERE u_from.unit_name = $1
       AND u_to.unit_name = $2
     LIMIT 1;",
    params = list(from_unit, to_unit)
  )

  if (!nrow(out)) {
    return(NULL)
  }

  return(out)
}

#' Convert values from one unit to another using database-defined conversions
#' This function retrieves the conversion parameters from the database and applies the appropriate transformation to convert values from the specified 'from_unit' to the 'to_unit'. If no conversion is found and 'strict' is TRUE, an error is raised. If 'strict' is FALSE, the original values are returned unchanged.
#' @param con A DBI connection to the database containing unit conversion information.
#' @param values A numeric vector of values to be converted.
#' @param from_unit A string representing the unit of the input values.
#' @param to_unit A string representing the desired unit for the output values.
#' @param strict A logical value indicating whether to raise an error if no conversion is found (default is TRUE). If FALSE, the original values will be returned unchanged when no conversion is found.
#' @return A numeric vector of converted values, or the original values if no conversion is found and 'strict' is FALSE.
#' @export

ac_convert_units <- function(
  con,
  values,
  from_unit,
  to_unit,
  strict = TRUE
) {
  conversion <- ac_get_unit_conversion(con, from_unit, to_unit)

  if (is.null(conversion)) {
    if (strict) {
      stop(
        "No unit conversion was found for '",
        from_unit,
        "' to '",
        to_unit,
        "'."
      )
    }

    return(values)
  }

  as.numeric(conversion$scale_a[[1]]) *
    values +
    as.numeric(conversion$scale_b[[1]])
}

# Helpers for historic ranges in plots

#' Normalize aggregation types
#' Converts various representations of aggregation types to a standardized form.
#' For example, "minimum" becomes "min", "maximum" becomes "max", and empty strings or "na" become NA.
#' @param aggregation_types A character vector of aggregation types to normalize.
#' @return A character vector of normalized aggregation types, with invalid entries removed.
#' @noRd
#' @examples
#' normalize_aggregation_types(c("Minimum", "maximum", "", "NA", "sum"))
#' # Returns: c("min", "max")

normalize_aggregation_types <- function(aggregation_types) {
  if (is.null(aggregation_types) || length(aggregation_types) == 0) {
    return(character())
  }

  aggregation_types <- trimws(tolower(as.character(aggregation_types)))
  aggregation_types[aggregation_types == "minimum"] <- "min"
  aggregation_types[aggregation_types == "maximum"] <- "max"
  aggregation_types[aggregation_types %in% c("", "na")] <- NA_character_
  stats::na.omit(aggregation_types)
  return(aggregation_types)
}

#' Check if historic range is meaningless for given aggregation types and resolution
#' Determines if historic ranges should be considered meaningless based on the aggregation types, resolution, and record rate. Specifically, if the resolution is sub-daily (hourly or maximum with sub-daily record rate) and the aggregation type is "sum", then historic ranges are considered meaningless.
#' @param aggregation_types A character vector of aggregation types (e.g., "sum", "min", "max").
#' @param resolution A character string indicating the resolution (e.g., "hour", "day", "max").
#' @param record_rate_seconds Optional numeric value indicating the record rate in seconds, used to determine if "max" resolution is sub-daily.
#' @return A logical value indicating whether the historic range is considered meaningless.
#' @export

historic_range_is_meaningless <- function(
  aggregation_types,
  resolution,
  record_rate_seconds = NULL
) {
  historic_range_uses_subdaily_values <- function(
    resolution,
    record_rate_seconds = NULL
  ) {
    if (
      is.null(resolution) || length(resolution) == 0 || is.na(resolution[[1]])
    ) {
      resolution <- "day"
    } else {
      resolution <- tolower(as.character(resolution[[1]]))
      if (!(resolution %in% c("max", "hour", "day"))) {
        resolution <- "day"
      }
    }

    if (identical(resolution, "hour")) {
      return(TRUE)
    }
    if (!identical(resolution, "max")) {
      return(FALSE)
    }

    if (is.null(record_rate_seconds) || length(record_rate_seconds) == 0) {
      return(FALSE)
    }

    record_rate_seconds <- suppressWarnings(as.numeric(record_rate_seconds))
    record_rate_seconds <- record_rate_seconds[!is.na(record_rate_seconds)]
    if (length(record_rate_seconds) == 0) {
      return(FALSE)
    }

    any(record_rate_seconds < 24 * 60 * 60)
  }

  if (
    !historic_range_uses_subdaily_values(
      resolution = resolution,
      record_rate_seconds = record_rate_seconds
    )
  ) {
    return(FALSE)
  }

  aggregation_types <- normalize_aggregation_types(aggregation_types)
  any(aggregation_types %in% c("sum"))
}

#' Fetch metadata for historic range timeseries
#' Retrieves metadata for a set of timeseries IDs, including the aggregation type and record rate in seconds. This information is used to determine if historic ranges are meaningful for the given timeseries based on their aggregation types and record rates.
#' @param con A DBI connection to the database containing timeseries metadata.
#' @param timeseries_ids A vector of timeseries IDs for which to fetch metadata. Non-numeric and NA values will be ignored.
#' @return A data.frame containing the timeseries_id, aggregation_type, and record_rate_seconds for the specified timeseries IDs. If no valid timeseries IDs are provided, returns an empty data.frame with the appropriate columns.
#' @noRd
fetch_historic_range_timeseries_metadata <- function(con, timeseries_ids) {
  timeseries_ids <- unique(stats::na.omit(as.integer(timeseries_ids)))
  if (length(timeseries_ids) == 0) {
    return(data.frame(
      timeseries_id = integer(),
      aggregation_type = character(),
      record_rate_seconds = numeric()
    ))
  }

  DBI::dbGetQuery(
    con,
    paste0(
      "SELECT ts.timeseries_id, at.aggregation_type, ",
      "EXTRACT(EPOCH FROM ts.record_rate) AS record_rate_seconds ",
      "FROM continuous.timeseries ts ",
      "LEFT JOIN continuous.aggregation_types at ",
      "ON ts.aggregation_type_id = at.aggregation_type_id ",
      "WHERE ts.timeseries_id IN (",
      paste(timeseries_ids, collapse = ", "),
      ");"
    )
  )
}

# this set of functions standardizes inputs for various functions in the package

#' Standardize Language Input to short form
#'
#' @description
#' Standardizes the input language to either "en" (English) or "fr" (French).
#' Accepts various representations of English and French, including common abbreviations
#' and translations. If the input is `NULL`, defaults to English. If the input is not
#' recognized, a warning is issued and the function defaults to English.
#'
#' @param language Character string specifying the language. Accepts values such as
#'   "en", "english", "anglais" for English, and "fr", "french", "français", "francais",
#'   "français" for French. Case-insensitive.
#'
#' @return A character string: "en" for English or "fr" for French.
#' @export
#'
#' @examples
#' shortenLanguage("EN")        # returns "en"
#' shortenLanguage("français")  # returns "fr"
#' shortenLanguage(NULL)        # returns "en"
#' shortenLanguage("de")        # returns "en" with a warning

shortenLanguage <- function(language) {
  english_variants <- c("en", "english", "anglais")
  french_variants <- c(
    "fr",
    "french",
    "fran\u00e7ais",
    "francais",
    "fran\u00e7ais"
  )
  # standardize language input to "English" or "French"
  if (is.null(language)) {
    return("en")
  }

  language <- tolower(language)
  if (language %in% english_variants) {
    return("en")
  } else if (language %in% french_variants) {
    return("fr")
  } else {
    warning("Unrecognized language input. Defaulting to English.")
    return("en")
  }
}

#' Standardize Language Input to Full Name
#' @description
#' Standardizes the input language to either "English" or "Français".
#' Accepts various representations of English and French, including common abbreviations
#' and translations. If the input is `NULL`, defaults to English. If the input is not
#' recognized, a warning is issued and the function defaults to English.
#'
#' @param language Character string specifying the language. Accepts values such as "en", "english", "anglais" for English, and "fr", "french", "français", "francais", "français" for French. Case-insensitive.
#' @return A character string: "English" for English or "Français" for French.
#' @export
#'
#' @examples
#' lengthenLanguage("EN")        # returns "English"
#' lengthenLanguage("fr")  # returns "Fran\u00e7ais"
#' lengthenLanguage(NULL)        # returns "English"

lengthenLanguage <- function(language) {
  english_variants <- c("en", "english", "anglais")
  french_variants <- c(
    "fr",
    "french",
    "fran\u00e7ais",
    "francais",
    "fran\u00e7ais"
  )
  # standardize language input to "English" or "French"
  if (is.null(language)) {
    return("English")
  }

  language <- tolower(language)
  if (language %in% english_variants) {
    return("English")
  } else if (language %in% french_variants) {
    return("Fran\u00e7ais")
  } else {
    warning("Unrecognized language input. Defaulting to English.")
    return("English")
  }
}


#' Get the default EQWin database path
#'
#' @return The network path to the WRB EQWin Access database.
#' @keywords internal
#' @noRd
eqwin_db_path <- function() {
  "//carver/infosys/EQWin/WaterResources.mdb"
}

#' Get the default YOWN master workbook path
#'
#' @return The network path to the YOWN master workbook.
#' @keywords internal
#' @noRd
yown_master_path <- function() {
  "//env-fs/env-data/corp/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/2_SPREADSHEETS/1_YOWN_MASTER_TABLE/YOWN_MASTER.xlsx"
}

#' Get the default YOWN logger tracking workbook path
#'
#' @return The network path to the YOWN logger tracking workbook.
#' @keywords internal
#' @noRd
yown_tracking_path <- function() {
  "//env-fs/env-data/corp/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/2_SPREADSHEETS/3_OTHER/YOWN_Logger_Tracking.xlsx"
}

#' Get the default YOWN logger dropbox path
#'
#' @return The network directory for incoming logger files.
#' @keywords internal
#' @noRd
yown_dropbox_path <- function() {
  "//env-fs/env-data/corp/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/9_LOGGER_FILE_DROPBOX"
}

#' Get the default active YOWN wells directory
#'
#' @return The network directory for active well files.
#' @keywords internal
#' @noRd
yown_active_path <- function() {
  "//env-fs/env-data/corp/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/1_YOWN_SITES/1_ACTIVE_WELLS"
}

#' Get the default YOWN Water Data Catalogue directory
#'
#' @return The network directory for YOWN catalogue data.
#' @keywords internal
#' @noRd
yown_wdc_path <- function() {
  "\\\\envgeoserver\\share\\WaterResources\\Groundwater\\YOWN_DATA\\"
}
