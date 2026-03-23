normalize_plot_resolution <- function(resolution) {
  if (
    is.null(resolution) || length(resolution) == 0 || is.na(resolution[[1]])
  ) {
    return("day")
  }

  resolution <- tolower(as.character(resolution[[1]]))
  if (!(resolution %in% c("max", "hour", "day"))) {
    return("day")
  }

  resolution
}

normalize_aggregation_types <- function(aggregation_types) {
  if (is.null(aggregation_types) || length(aggregation_types) == 0) {
    return(character())
  }

  aggregation_types <- trimws(tolower(as.character(aggregation_types)))
  aggregation_types[aggregation_types == "minimum"] <- "min"
  aggregation_types[aggregation_types == "maximum"] <- "max"
  aggregation_types[aggregation_types %in% c("", "na")] <- NA_character_
  stats::na.omit(aggregation_types)
}

historic_range_uses_subdaily_values <- function(
  resolution,
  record_rate_seconds = NULL
) {
  resolution <- normalize_plot_resolution(resolution)

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

historic_range_is_meaningless <- function(
  aggregation_types,
  resolution,
  record_rate_seconds = NULL
) {
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

historic_range_meaningless_note <- function(
  aggregation_types,
  resolution,
  record_rate_seconds = NULL,
  lang = "en"
) {
  if (
    !historic_range_is_meaningless(
      aggregation_types = aggregation_types,
      resolution = resolution,
      record_rate_seconds = record_rate_seconds
    )
  ) {
    return(NULL)
  }

  if (identical(lang, "fr")) {
    return(
      paste(
        "Les plages historiques sont basees sur des valeurs quotidiennes.",
        "Elles sont desactivees pour les series cumulatives a pas de temps",
        "sous-quotidien, comme les precipitations, parce qu'un ruban",
        "quotidien serait trompeur sur un trace horaire ou a resolution",
        "maximale."
      )
    )
  }

  paste(
    "Historic ranges are based on daily values.",
    "They are disabled for sub-daily cumulative series such as",
    "precipitation, because a daily ribbon would be misleading on",
    "hourly or maximum-resolution plots."
  )
}

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
      "FROM timeseries ts ",
      "LEFT JOIN aggregation_types at ",
      "ON ts.aggregation_type_id = at.aggregation_type_id ",
      "WHERE ts.timeseries_id IN (",
      paste(timeseries_ids, collapse = ", "),
      ");"
    )
  )
}
