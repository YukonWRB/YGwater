#' Download ECCC station observations from GeoMet
#'
#' Downloads historical climate observations directly from ECCC's official
#' GeoMet OGC API. This is a dependency-light replacement for the download
#' portion of `weathercan`; station selection and persistence can be managed by
#' AquaCache.
#'
#' @param stations One or more ECCC climate identifiers (for example
#'   `"2101310"`) or numeric ECCC station IDs. A data frame is also accepted;
#'   it must contain `climate_id` or `station_id`.
#' @param start,end Start and end dates (inclusive).
#' @param interval One of `"hour"`, `"day"`, or `"month"`.
#' @param api_url Base URL for the ECCC GeoMet OGC API.
#' @param page_size Number of features requested per API page (maximum 10,000).
#' @param attempts Number of attempts for each HTTP request.
#' @param quiet Suppress progress messages.
#'
#' @return A data frame containing the fields published by ECCC, with names
#'   converted to lower case. The `geometry` list-column contains GeoJSON point
#'   geometry when supplied by the API.
#' @export
downloadECCCObservations <- function(
  stations,
  start,
  end = Sys.Date(),
  interval = c("hour", "day", "month"),
  api_url = "https://api.weather.gc.ca",
  page_size = 10000L,
  attempts = 4L,
  quiet = FALSE
) {
  interval <- match.arg(interval)
  dates <- eccc_date_range(start, end)
  station_keys <- eccc_observation_station_keys(stations)
  collection <- paste0("climate-", c(hour = "hourly", day = "daily", month = "monthly")[[interval]])
  endpoint <- paste0(sub("/$", "", api_url), "/collections/", collection, "/items")

  result <- lapply(seq_len(nrow(station_keys)), function(i) {
    key <- station_keys[i, , drop = FALSE]
    if (!quiet) {
      message("Downloading ECCC ", interval, " observations for ", key$value)
    }
    query <- list(
      f = "json",
      datetime = paste0(dates$start, "/", dates$end),
      limit = as.integer(page_size)
    )
    query[[key$field]] <- key$value
    features <- eccc_get_features(endpoint, query, page_size, attempts)
    eccc_features_data_frame(features)
  })
  eccc_bind_rows(result)
}

#' Download ECCC PROGNOS station forecasts from GeoMet
#'
#' Retrieves the latest complete PROGNOS run at or before `reference_time` for
#' each requested model. Forecasts are matched to the supplied stations using
#' their MSC/Transport Canada, WMO, and PROGNOS identifiers.
#'
#' @param stations A data frame with station identifiers. It must include a
#'   location column (`location_id`, `MSC_ID`, or `location`) and may include
#'   `prognos_station_id`, `iata_id`/`TC_IDENTIFIER`, `wmo_id`/`WMO_IDENTIFIER`,
#'   `data_provider`, `longitude`/`lon`, and `latitude`/`lat`.
#' @param reference_time Latest acceptable model run time. Defaults to now UTC.
#' @param models Any of `"HRDPS"`, `"RDPS"`, and `"GDPS"`.
#' @param max_run_age_hours How far back to search for a complete model run.
#' @param max_distance_km Reject an identifier match farther than this from the
#'   supplied station coordinates. Set to `Inf` when coordinates are absent.
#' @param api_url Base URL for the ECCC GeoMet OGC API.
#' @param page_size Number of features requested per API page.
#' @param attempts Number of attempts for each HTTP request.
#' @param quiet Suppress progress messages.
#'
#' @return A data frame with AquaCache `location_id`, model, model run time,
#'   forecast time, variable, value, unit, lead time, and ECCC station fields.
#' @export
downloadECCCPrognos <- function(
  stations,
  reference_time = Sys.time(),
  models = c("HRDPS", "RDPS", "GDPS"),
  max_run_age_hours = 36,
  max_distance_km = 10,
  api_url = "https://api.weather.gc.ca",
  page_size = 10000L,
  attempts = 4L,
  quiet = FALSE
) {
  model_info <- list(
    HRDPS = list(cycles = c(0L, 6L, 12L, 18L), final_lead = "PT048H"),
    RDPS = list(cycles = c(0L, 6L, 12L, 18L), final_lead = "PT084H"),
    GDPS = list(cycles = c(0L, 12L), final_lead = "PT168H")
  )
  models <- toupper(models)
  unknown <- setdiff(models, names(model_info))
  if (length(unknown)) stop("Unsupported PROGNOS model(s): ", paste(unknown, collapse = ", "))
  stations <- eccc_prognos_stations(stations)
  reference_time <- as.POSIXct(reference_time, tz = "UTC")
  if (is.na(reference_time)) stop("`reference_time` must be a valid date-time.")

  output <- lapply(models, function(model) {
    endpoint <- paste0(sub("/$", "", api_url), "/collections/prognos-", tolower(model), "-realtime/items")
    run <- eccc_latest_prognos_run(
      endpoint, model_info[[model]], reference_time,
      max_run_age_hours, stations, attempts
    )
    if (is.null(run)) {
      warning(model, ": no complete PROGNOS run found within ", max_run_age_hours, " hours.")
      return(data.frame())
    }
    if (!quiet) message("Downloading ECCC PROGNOS ", model, " run ", format(run, tz = "UTC"))
    query <- list(
      f = "json",
      reference_datetime = format(run, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      limit = as.integer(page_size)
    )
    bbox <- eccc_station_bbox(stations)
    if (!is.null(bbox)) query$bbox <- bbox
    features <- eccc_get_features(endpoint, query, page_size, attempts)
    values <- eccc_features_data_frame(features)
    if (!nrow(values)) return(values)
    matched <- eccc_match_prognos(values, stations, max_distance_km)
    if (!nrow(matched)) return(matched)
    matched$model <- model
    matched$reference_datetime <- as.POSIXct(run, tz = "UTC")
    preferred <- c(
      "location_id", "model", "reference_datetime", "forecast_datetime",
      "forecast_leadtime", "variable", "forecast_value", "unit",
      "station_network", "ade_id", "prognos_station_id"
    )
    matched[c(intersect(preferred, names(matched)), setdiff(names(matched), preferred))]
  })
  eccc_bind_rows(output)
}

eccc_date_range <- function(start, end) {
  start <- as.Date(start)
  end <- as.Date(end)
  if (length(start) != 1L || is.na(start) || length(end) != 1L || is.na(end)) {
    stop("`start` and `end` must each be one valid date.")
  }
  if (start > end) stop("`start` must be on or before `end`.")
  list(start = format(start), end = format(end))
}

eccc_observation_station_keys <- function(stations) {
  if (is.data.frame(stations)) {
    climate <- eccc_column(stations, c("climate_id", "CLIMATE_IDENTIFIER"), required = FALSE)
    station <- eccc_column(stations, c("station_id", "STN_ID"), required = FALSE)
    out <- data.frame(field = character(), value = character())
    if (!is.null(climate)) {
      keep <- !is.na(climate) & nzchar(trimws(as.character(climate)))
      out <- rbind(out, data.frame(field = "CLIMATE_IDENTIFIER", value = as.character(climate[keep])))
    }
    if (!is.null(station)) {
      # Prefer the stable climate identifier when a row supplies both keys.
      has_climate <- if (is.null(climate)) rep(FALSE, length(station)) else
        !is.na(climate) & nzchar(trimws(as.character(climate)))
      keep <- !has_climate & !is.na(station) & nzchar(trimws(as.character(station)))
      out <- rbind(out, data.frame(field = "STN_ID", value = as.character(station[keep])))
    }
  } else {
    values <- unique(as.character(stations))
    values <- values[!is.na(values) & nzchar(trimws(values))]
    is_climate <- grepl("^[0-9]{4}[0-9A-Za-z]{3}$", values)
    out <- data.frame(
      field = ifelse(is_climate, "CLIMATE_IDENTIFIER", "STN_ID"),
      value = values
    )
  }
  out <- unique(out)
  if (!nrow(out)) stop("No ECCC climate or station identifiers were supplied.")
  out
}

eccc_get_features <- function(endpoint, query, page_size, attempts) {
  page_size <- as.integer(page_size)
  if (is.na(page_size) || page_size < 1L || page_size > 10000L) {
    stop("`page_size` must be between 1 and 10,000.")
  }
  features <- list()
  offset <- 0L
  repeat {
    query$offset <- offset
    response <- eccc_get_json(endpoint, query, attempts)
    batch <- response$features
    if (is.null(batch)) batch <- list()
    features <- c(features, batch)
    if (length(batch) < page_size) break
    offset <- offset + length(batch)
  }
  features
}

eccc_get_json <- function(endpoint, query, attempts = 4L) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package `jsonlite` is required to download ECCC API data.")
  }
  encode <- function(x) utils::URLencode(as.character(x), reserved = TRUE)
  query_string <- paste0(encode(names(query)), "=", vapply(query, encode, character(1)), collapse = "&")
  request_url <- paste0(endpoint, "?", query_string)
  attempts <- max(1L, as.integer(attempts))
  for (attempt in seq_len(attempts)) {
    answer <- tryCatch(
      jsonlite::fromJSON(request_url, simplifyVector = FALSE),
      error = identity
    )
    if (!inherits(answer, "error")) return(answer)
    if (attempt == attempts) {
      stop("ECCC API request failed after ", attempts, " attempt(s): ", request_url,
           "\n", conditionMessage(answer), call. = FALSE)
    }
    Sys.sleep(min(5 * attempt, 30))
  }
}

eccc_features_data_frame <- function(features) {
  if (!length(features)) return(data.frame())
  rows <- lapply(features, function(feature) {
    properties <- feature$properties
    row <- as.data.frame(properties, stringsAsFactors = FALSE, optional = TRUE)
    row$geometry <- I(list(feature$geometry))
    row
  })
  out <- eccc_bind_rows(rows)
  names(out) <- tolower(names(out))
  out
}

eccc_bind_rows <- function(rows) {
  rows <- Filter(function(x) is.data.frame(x) && nrow(x), rows)
  if (!length(rows)) return(data.frame())
  columns <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(x) {
    missing <- setdiff(columns, names(x))
    for (name in missing) x[[name]] <- NA
    x[columns]
  })
  rownames_out <- NULL
  out <- do.call(rbind, rows)
  rownames(out) <- rownames_out
  out
}

eccc_column <- function(data, candidates, required = TRUE) {
  key <- match(tolower(candidates), tolower(names(data)), nomatch = 0L)
  key <- key[key > 0L]
  if (length(key)) return(data[[key[[1L]]]])
  if (required) stop("Missing required station column; expected one of: ", paste(candidates, collapse = ", "))
  NULL
}

eccc_prognos_stations <- function(stations) {
  if (!is.data.frame(stations) || !nrow(stations)) stop("`stations` must be a non-empty data frame.")
  optional_column <- function(candidates, mode = "character") {
    value <- eccc_column(stations, candidates, FALSE)
    if (is.null(value)) value <- rep(NA, nrow(stations))
    if (mode == "numeric") suppressWarnings(as.numeric(value)) else as.character(value)
  }
  out <- data.frame(
    location_id = as.character(eccc_column(stations, c("location_id", "MSC_ID", "location"))),
    prognos_station_id = optional_column(c("prognos_station_id")),
    iata_id = toupper(optional_column(c("iata_id", "IATA_ID", "TC_IDENTIFIER"))),
    wmo_id = optional_column(c("wmo_id", "WMO_ID", "WMO_IDENTIFIER")),
    data_provider = toupper(optional_column(c("data_provider", "Data_Provider"))),
    lon = optional_column(c("longitude", "Longitude", "lon"), "numeric"),
    lat = optional_column(c("latitude", "Latitude", "lat"), "numeric"),
    stringsAsFactors = FALSE
  )
  if (anyNA(out$location_id) || any(!nzchar(out$location_id))) stop("Every station needs a location identifier.")
  out
}

eccc_station_bbox <- function(stations, pad = 0.5) {
  keep <- is.finite(stations$lon) & is.finite(stations$lat)
  if (!any(keep)) return(NULL)
  paste(c(
    min(stations$lon[keep]) - pad, min(stations$lat[keep]) - pad,
    max(stations$lon[keep]) + pad, max(stations$lat[keep]) + pad
  ), collapse = ",")
}

eccc_latest_prognos_run <- function(endpoint, model, reference_time, max_age_hours, stations, attempts) {
  candidate <- as.POSIXct(format(reference_time, "%Y-%m-%d %H:00:00", tz = "UTC"), tz = "UTC")
  oldest <- reference_time - as.difftime(max_age_hours, units = "hours")
  bbox <- eccc_station_bbox(stations)
  while (candidate >= oldest) {
    hour <- as.integer(format(candidate, "%H", tz = "UTC"))
    if (hour %in% model$cycles) {
      query <- list(
        f = "json", reference_datetime = format(candidate, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        variable = "AirTemp", forecast_leadtime = model$final_lead,
        limit = 1L, skipGeometry = "true", properties = "reference_datetime"
      )
      if (!is.null(bbox)) query$bbox <- bbox
      response <- eccc_get_json(endpoint, query, attempts)
      if (length(response$features)) return(candidate)
    }
    candidate <- candidate - 3600
  }
  NULL
}

eccc_match_prognos <- function(values, stations, max_distance_km) {
  needed <- c("station_network", "ade_id")
  if (!all(needed %in% names(values))) stop("PROGNOS response is missing station identifiers.")
  values$station_network <- tolower(as.character(values$station_network))
  values$ade_id <- toupper(as.character(values$ade_id))
  values$location_id <- NA_character_

  for (i in seq_len(nrow(stations))) {
    row <- stations[i, ]
    short <- if (!is.na(row$iata_id) && grepl("^C[A-Z0-9]{3}$", row$iata_id)) substring(row$iata_id, 2L) else NA_character_
    keys <- rbind(
      c("prognos", toupper(row$prognos_station_id)),
      c(ifelse(identical(row$data_provider, "MSC"), "ca", "metar"), ifelse(identical(row$data_provider, "MSC"), short, row$iata_id)),
      c(ifelse(identical(row$data_provider, "MSC"), "metar", "ca"), ifelse(identical(row$data_provider, "MSC"), row$iata_id, short)),
      c("synop", row$wmo_id)
    )
    for (j in seq_len(nrow(keys))) {
      if (is.na(keys[j, 2L]) || !nzchar(keys[j, 2L])) next
      if (keys[j, 1L] == "prognos" && "prognos_station_id" %in% names(values)) {
        hit <- toupper(values$prognos_station_id) == keys[j, 2L]
      } else {
        hit <- values$station_network == keys[j, 1L] & values$ade_id == keys[j, 2L]
      }
      hit[is.na(hit)] <- FALSE
      if (!any(hit)) next
      if (is.finite(row$lon) && is.finite(row$lat) && is.finite(max_distance_km)) {
        geometry <- values$geometry[which(hit)[1L]][[1L]]
        if (!is.null(geometry$coordinates)) {
          distance <- eccc_distance_km(row$lon, row$lat, geometry$coordinates[[1L]], geometry$coordinates[[2L]])
          if (distance > max_distance_km) next
        }
      }
      values$location_id[hit] <- row$location_id
      break
    }
  }
  values[!is.na(values$location_id), , drop = FALSE]
}

eccc_distance_km <- function(lon1, lat1, lon2, lat2) {
  radians <- pi / 180
  phi1 <- lat1 * radians
  phi2 <- lat2 * radians
  dphi <- (lat2 - lat1) * radians
  dlambda <- (lon2 - lon1) * radians
  a <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlambda / 2)^2
  2 * 6371 * asin(sqrt(a))
}
