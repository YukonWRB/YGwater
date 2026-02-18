# !IMPORTANT! Some helper functions are located at the bottom of this file.

#' @apiTitle AquaCache API version 1
#' @apiDescription API for programmatic access to the aquacache database. Should usually be launched using function api(), in the R directory. Many endpoints can make use of authentication via HTTP Basic Auth. See the documentation for details. In addition, memoisation is used in multiple endpoints to cache results for improved performance.
#' @apiVersion 1.0.0

# Basic authentication filter. TLS is terminated by upstream NGINX.
#* @filter auth
function(req, res) {
  # Allow anonymous for read-only GET on these routes (uses public_reader account)
  public_paths <- c(
    "^/locations$",
    "^/timeseries$",
    "^/parameters$",
    "^/samples(?:$|/)",
    "^/snow-survey(?:$|/)",
    "^/snow-bulletin/leaflet$",
    "^/__docs__/", # Swagger UI shell & assets
    "^/openapi.json$", # <-- needed for the UI to load without auth
    "^/openapi.yaml$", # <-- sometimes used
    "^/timeseries/measurements$",
    "^/samples/results$/"
  )
  is_public_ok <- identical(req$REQUEST_METHOD, "GET") &&
    any(grepl(paste(public_paths, collapse = "|"), req$PATH_INFO))

  hdr <- req$HTTP_AUTHORIZATION %||% ""
  if (hdr == "") {
    if (!is_public_ok) {
      res$status <- 401
      res$setHeader("WWW-Authenticate", 'Basic realm="AquaCache"')
      return(list(error = "Authentication required"))
    }
    req$user <- Sys.getenv("APIaquacacheAnonUser", "public_reader") # default to public_reader if not set. Set on CI to run with test database, otherwise uses public_reader.
    req$password <- Sys.getenv("APIaquacacheAnonPass", "aquacache")
    return(plumber::forward())
  }

  # Parse Basic user:pass (split on first :)
  b64 <- sub("^Basic\\s+", "", hdr)
  cred <- rawToChar(jsonlite::base64_dec(b64))
  i <- regexpr(":", cred, fixed = TRUE)
  if (i < 1) {
    res$status <- 401
    return(list(error = "Invalid authentication header"))
  }
  req$user <- substr(cred, 1, i - 1)
  req$password <- substr(cred, i + 1, nchar(cred))
  plumber::forward()
}

#' List available locations
#* @param lang Language for location names and descriptions ("en" or "fr").
#* @get /locations
#* @serializer csv
function(req, res, lang = "en") {
  con <- try(
    YGwater::AquaConnect(
      username = req$user,
      password = req$password,
      name = Sys.getenv("APIaquacacheName"),
      host = Sys.getenv("APIaquacacheHost"),
      port = Sys.getenv("APIaquacachePort"),
      silent = TRUE
    ),
    silent = TRUE
  )

  if (inherits(con, "try-error")) {
    res$status <- 503
    return(data.frame(
      status = "error",
      message = "Database connection failed, check your credentials.",
      stringsAsFactors = FALSE
    ))
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  sql <- if (lang == "en") {
    "SELECT * FROM public.location_metadata_en ORDER BY location_id"
  } else if (lang == "fr") {
    "SELECT * FROM public.location_metadata_fr ORDER BY location_id"
  } else {
    res$headers[["X-Status"]] <- "error"
    return(data.frame(
      status = "error",
      message = "Invalid language parameter. Use 'en' or 'fr'.",
      stringsAsFactors = FALSE
    ))
  }

  out <- DBI::dbGetQuery(con, sql)

  if (nrow(out) == 0) {
    res$headers[["X-Status"]] <- "info"
    return(data.frame(
      status = "info",
      message = "No locations found in the database.",
      stringsAsFactors = FALSE
    ))
  }

  return(out)
}

#' List available timeseries
#* @param lang Language for timeseries names and descriptions ("en" or "fr").
#* @get /timeseries
#* @serializer csv
function(req, res, lang = "en") {
  con <- try(
    YGwater::AquaConnect(
      username = req$user,
      password = req$password,
      name = Sys.getenv("APIaquacacheName"),
      host = Sys.getenv("APIaquacacheHost"),
      port = Sys.getenv("APIaquacachePort"),
      silent = TRUE
    ),
    silent = TRUE
  )

  if (inherits(con, "try-error")) {
    res$status <- 503
    return(data.frame(
      status = "error",
      message = "Database connection failed, check your credentials.",
      stringsAsFactors = FALSE
    ))
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (lang == "en") {
    sql <- "SELECT * FROM continuous.timeseries_metadata_en ORDER BY timeseries_id"
  } else if (lang == "fr") {
    sql <- "SELECT * FROM continuous.timeseries_metadata_fr ORDER BY timeseries_id"
  } else {
    res$headers[["X-Status"]] <- "error"
    return(data.frame(
      status = "error",
      message = "Invalid language parameter. Use 'en' or 'fr'.",
      stringsAsFactors = FALSE
    ))
  }
  out <- DBI::dbGetQuery(con, sql)

  if (nrow(out) == 0) {
    res$headers[["X-Status"]] <- "info"
    return(data.frame(
      status = "info",
      message = "No timeseries found in the database.",
      stringsAsFactors = FALSE
    ))
  }

  return(out)
}


#' Return measurements for a timeseries
#* @param id Timeseries ID (required).
#* @param start Start date/time (required, ISO 8601 i.e. 2025-01-01 00:00).
#* @param end End date/time (optional; defaults to now, ISO 8601).
#* @param limit Maximum number of records to return (optional; defaults to 100000).
#* @get /timeseries/measurements
#* @serializer csv
function(req, res, id, start, end = NA, limit = 100000) {
  if (missing(id)) {
    res$headers[["X-Status"]] <- "error"
    return(data.frame(
      status = "error",
      message = "Missing required 'id' parameter.",
      stringsAsFactors = FALSE
    ))
  }

  # Ensure start and end are provided and can be converted to POSIXct
  if (missing(start)) {
    res$headers[["X-Status"]] <- "error"
    return(data.frame(
      status = "error",
      message = "Missing required 'start' parameter.",
      stringsAsFactors = FALSE
    ))
  }
  if (missing(end)) {
    end <- Sys.time()
  }
  start <- try(as.POSIXct(start, tz = "UTC"), silent = TRUE)
  end <- try(as.POSIXct(end, tz = "UTC"), silent = TRUE)

  if (inherits(start, "try-error") || is.na(start)) {
    res$headers[["X-Status"]] <- "error"
    return(data.frame(
      status = "error",
      message = "Invalid 'start' parameter. Must be in ISO 8601 format.",
      stringsAsFactors = FALSE
    ))
  }
  if (inherits(end, "try-error") || is.na(end)) {
    res$headers[["X-Status"]] <- "error"
    return(data.frame(
      status = "error",
      message = "Invalid 'end' parameter. Must be in ISO 8601 format.",
      stringsAsFactors = FALSE
    ))
  }

  lim <- suppressWarnings(as.integer(limit))
  if (is.na(lim) || lim <= 0) {
    lim <- 100000
  }

  con <- try(
    YGwater::AquaConnect(
      username = req$user,
      password = req$password,
      name = Sys.getenv("APIaquacacheName"),
      host = Sys.getenv("APIaquacacheHost"),
      port = Sys.getenv("APIaquacachePort"),
      silent = TRUE
    ),
    silent = TRUE
  )

  if (inherits(con, "try-error")) {
    res$status <- 503
    return(data.frame(
      status = "error",
      message = "Database connection failed, check your credentials.",
      stringsAsFactors = FALSE
    ))
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  sql <- "SELECT * FROM continuous.measurements_continuous_corrected 
  WHERE timeseries_id = $1 
  AND datetime >= $2 
  AND datetime <= $3 
  ORDER BY datetime DESC
  LIMIT $4"
  out <- DBI::dbGetQuery(
    con,
    sql,
    params = list(as.integer(id), start, end, lim)
  )

  if (nrow(out) == 0) {
    res$headers[["X-Status"]] <- "info"
    return(data.frame(
      status = "info",
      message = "No measurements found for the specified timeseries and date range.",
      stringsAsFactors = FALSE
    ))
  }
  return(out)
}

#' Return available parameters in the database
#* @get /parameters
#* @serializer csv
function(req, res) {
  con <- try(
    YGwater::AquaConnect(
      username = req$user,
      password = req$password,
      name = Sys.getenv("APIaquacacheName"),
      host = Sys.getenv("APIaquacacheHost"),
      port = Sys.getenv("APIaquacachePort"),
      silent = TRUE
    ),
    silent = TRUE
  )

  if (inherits(con, "try-error")) {
    res$status <- 503
    return(data.frame(
      status = "error",
      message = "Database connection failed, check your credentials.",
      stringsAsFactors = FALSE
    ))
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  sql <- "SELECT parameter_id, param_name, param_name_fr, description, description_fr, unit_default AS units FROM public.parameters ORDER BY parameter_id"
  out <- DBI::dbGetQuery(con, sql)

  if (nrow(out) == 0) {
    res$headers[["X-Status"]] <- "info"
    return(data.frame(
      status = "info",
      message = "No parameters found in the database.",
      stringsAsFactors = FALSE
    ))
  }

  return(out)
}

#' Return sample metadata
#* @param start Start date (required, ISO 8601 format).
#* @param end End date (optional, ISO 8601 format, defaults to current date/time).
#* @param locations Location ID (optional, integer string separated by commas). If provided, filters samples to these locations.
#* @param parameters Parameter ID (optional, integer string separated by commas). If provided, filters samples to only those which include these parameters.
#* @get /samples
#* @serializer csv
function(req, res, start, end = NA, locations = NA, parameters = NA) {
  # Ensure start and end are provided and can be converted to POSIXct
  if (missing(start)) {
    res$headers[["X-Status"]] <- "error"
    return(data.frame(
      status = "error",
      message = "Missing required 'start' parameter.",
      stringsAsFactors = FALSE
    ))
  }
  if (missing(end)) {
    end <- Sys.time()
  }
  start <- try(as.POSIXct(start, tz = "UTC"), silent = TRUE)
  end <- try(as.POSIXct(end, tz = "UTC"), silent = TRUE)

  if (inherits(start, "try-error") || is.na(start)) {
    res$headers[["X-Status"]] <- "error"
    return(data.frame(
      status = "error",
      message = "Invalid 'start' parameter. Must be in ISO 8601 format.",
      stringsAsFactors = FALSE
    ))
  }
  if (inherits(end, "try-error") || is.na(end)) {
    res$headers[["X-Status"]] <- "error"
    return(data.frame(
      status = "error",
      message = "Invalid 'end' parameter. Must be in ISO 8601 format.",
      stringsAsFactors = FALSE
    ))
  }

  con <- try(
    YGwater::AquaConnect(
      username = req$user,
      password = req$password,
      name = Sys.getenv("APIaquacacheName"),
      host = Sys.getenv("APIaquacacheHost"),
      port = Sys.getenv("APIaquacachePort"),
      silent = TRUE
    ),
    silent = TRUE
  )

  if (inherits(con, "try-error")) {
    res$status <- 503
    return(data.frame(
      status = "error",
      message = "Database connection failed, check your credentials.",
      stringsAsFactors = FALSE
    ))
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  sql <- "SELECT sample_id, location_id, sub_location_id, mt.media_type AS media, z AS depth_height, datetime, target_datetime, cm.collection_method, st.sample_type, sample_volume_ml, purge_volume_l, purge_time_min, flow_rate_l_min, wave_hgt_m, g.grade_type_description AS sample_grade, a.approval_type_description AS sample_approval, q.qualifier_type_description AS sample_qualifier, o1.name AS owner, o2.name AS contributor, field_visit_id, samples.note
  FROM discrete.samples 
  LEFT JOIN media_types mt ON samples.media_id = mt.media_id 
  LEFT JOIN collection_methods cm ON samples.collection_method = cm.collection_method_id
  LEFT JOIN sample_types st ON samples.sample_type = st.sample_type_id
  LEFT JOIN public.grade_types g ON samples.sample_grade = g.grade_type_id
  LEFT JOIN public.approval_types a ON samples.sample_approval = a.approval_type_id
  LEFT JOIN public.qualifier_types q ON samples.sample_qualifier = q.qualifier_type_id
  LEFT JOIN public.organizations o1 ON samples.owner = o1.organization_id
  LEFT JOIN public.organizations o2 ON samples.contributor = o2.organization_id
  WHERE datetime >= $1 AND datetime <= $2"

  if (!is.na(locations[1])) {
    sql <- paste0(
      sql,
      " AND location_id IN (",
      paste(as.integer(strsplit(locations, ",")[[1]]), collapse = ","),
      ")"
    )
  }
  if (!is.na(parameters[1])) {
    sql <- paste0(
      sql,
      " AND sample_id IN (SELECT DISTINCT sample_id FROM discrete.results WHERE parameter_id IN (",
      paste(as.integer(strsplit(parameters, ",")[[1]]), collapse = ","),
      "))"
    )
  }
  sql <- paste0(sql, " ORDER BY datetime DESC")

  out <- DBI::dbGetQuery(
    con,
    sql,
    params = list(start, end)
  )

  if (nrow(out) == 0) {
    res$headers[["X-Status"]] <- "info"
    return(data.frame(
      status = "info",
      message = "No samples found for the specified criteria.",
      stringsAsFactors = FALSE
    ))
  }
  return(out)
}

#' Return sample results
#* @param sample_ids Sample ID (required, integer string separated by commas).
#* @param parameters Parameter ID (optional, integer string separated by commas). If provided, filters results to these parameters.
#* @get /samples/results
#* @serializer csv
function(req, res, sample_ids, parameters = NA) {
  if (missing(sample_ids)) {
    res$headers[["X-Status"]] <- "error"
    return(data.frame(
      status = "error",
      message = "Missing required 'sample_ids' parameter.",
      stringsAsFactors = FALSE
    ))
  }
  con <- try(
    YGwater::AquaConnect(
      username = req$user,
      password = req$password,
      name = Sys.getenv("APIaquacacheName"),
      host = Sys.getenv("APIaquacacheHost"),
      port = Sys.getenv("APIaquacachePort"),
      silent = TRUE
    ),
    silent = TRUE
  )

  if (inherits(con, "try-error")) {
    res$status <- 503
    return(data.frame(
      status = "error",
      message = "Database connection failed, check your credentials.",
      stringsAsFactors = FALSE
    ))
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  sids <- as.integer(strsplit(sample_ids, ",")[[1]])

  sql <- paste0(
    "SELECT r.sample_id, rt.result_type, sf.sample_fraction, r.parameter_id, p.param_name, rs.result_speciation, r.result, rc.result_condition, r.result_condition_value, rvt.result_value_type, r.analysis_datetime, pm.protocol_name AS protocol, l.lab_name AS laboratory
  FROM discrete.results r
  LEFT JOIN discrete.result_types rt ON r.result_type = rt.result_type_id
  LEFT JOIN discrete.sample_fractions sf ON r.sample_fraction_id = sf.sample_fraction_id
  LEFT JOIN public.parameters p ON r.parameter_id = p.parameter_id
  LEFT JOIN discrete.result_speciations rs ON r.result_speciation_id = rs.result_speciation_id
  LEFT JOIN discrete.result_conditions rc ON r.result_condition = rc.result_condition_id
  LEFT JOIN discrete.result_value_types rvt ON r.result_value_type = rvt.result_value_type_id
  LEFT JOIN discrete.protocols_methods pm ON r.protocol_method = pm.protocol_id
  LEFT JOIN discrete.laboratories l ON r.laboratory = l.lab_id
  WHERE r.sample_id IN (",
    paste(sids, collapse = ","),
    ")"
  )

  if (!is.na(parameters[1])) {
    sql <- paste0(
      sql,
      " AND r.parameter_id IN (",
      paste(as.integer(strsplit(parameters, ",")[[1]]), collapse = ","),
      ")"
    )
  }
  sql <- paste0(sql, " ORDER BY r.parameter_id")

  out <- DBI::dbGetQuery(
    con,
    sql
  )

  if (nrow(out) == 0) {
    res$headers[["X-Status"]] <- "info"
    return(data.frame(
      status = "info",
      message = "No results found for the specified sample ID and parameters.",
      stringsAsFactors = FALSE
    ))
  }
  return(out)
}


# Functions and endpoints for snow bulletin map ########################
# Memoised version of snow bulletin leaflet HTML to improve performance.
snowbull_leaflet_mem <- memoise::memoise(
  function(
    stamp,
    year = NULL,
    month = NULL,
    statistic = "relative_to_med",
    language = "English",
    param_name = "snow water equivalent",
    con = NULL
  ) {
    YGwater:::create_snowbull_leaflet_html(
      year = year,
      month = month,
      param_name = param_name,
      statistic = statistic,
      language = language,
      con = con
    )
  },
  cache = cachem::cache_mem(),
  omit_args = c("con")
)

get_snowbull_stamp <- function(con, year = NULL, month = NULL) {
  param_id <- DBI::dbGetQuery(
    con,
    "SELECT parameter_id FROM public.parameters WHERE param_name = 'snow water equivalent'"
  )[1, 1]

  if (is.na(param_id) | is.null(year) | is.null(month)) {
    return("none")
  }

  continuous_stamp <- DBI::dbGetQuery(
    con,
    sprintf(
      "
      SELECT MAX(COALESCE(m.created, m.modified)::date) AS stamp
      FROM continuous.measurements_calculated_daily_corrected m
      JOIN continuous.timeseries t ON m.timeseries_id = t.timeseries_id
      WHERE t.parameter_id = %s AND DATE(m.date) <= CAST('%s' AS date) AND DATE(m.date) >= CAST('1990-10-01' AS date)
      ",
      as.integer(param_id),
      as.character(as.Date(sprintf("%04d-%02d-%02d", year, month, 1)))
    )
  )[1, 1]

  discrete_stamp <- DBI::dbGetQuery(
    con,
    sprintf(
      "
      SELECT MAX(COALESCE(s.created, s.modified)::date) AS stamp
      FROM discrete.samples s
      JOIN discrete.results r ON s.sample_id = r.sample_id
      WHERE r.parameter_id = %s
        AND r.result IS NOT NULL AND DATE(s.target_datetime) < DATE('%s') AND DATE(s.target_datetime) >= DATE('1990-10-01')
      ",
      as.integer(param_id),
      as.character(as.Date(sprintf("%04d-%02d-%02d", year, month, 1)))
    )
  )[1, 1]

  stamp_parts <- c(
    if (!is.na(continuous_stamp)) as.character(continuous_stamp) else "none",
    if (!is.na(discrete_stamp)) as.character(discrete_stamp) else "none"
  )

  paste(stamp_parts, collapse = "|")
}

#' Return SWE snow bulletin leaflet map HTML
#* @param year Bulletin year (optional; defaults to latest available).
#* @param month Bulletin month (optional; defaults to latest available).
#* @param statistic Statistic to display. One of "data", "relative_to_med", "percentile", "anomalies" (default "relative_to_med").
#* @param language Language for labels: 'French' or 'English' (default "English").
#* @get /snow-bulletin/leaflet
#* @serializer contentType list(type = "text/html")
function(
  req,
  res,
  year = NA,
  month = NA,
  statistic = "relative_to_med",
  language = "English"
) {
  con <- try(
    YGwater::AquaConnect(
      username = req$user,
      password = req$password,
      name = Sys.getenv("APIaquacacheName"),
      host = Sys.getenv("APIaquacacheHost"),
      port = Sys.getenv("APIaquacachePort"),
      silent = TRUE
    ),
    silent = TRUE
  )

  if (inherits(con, "try-error")) {
    res$status <- 503
    return("<p>Database connection failed, check your credentials.</p>")
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  if (xor(is.na(year), is.na(month))) {
    res$status <- 400
    return(
      "<p>Both 'year' and 'month' must be provided together or not at all.</p>"
    )
  }

  if (!is.na(year)) {
    year <- as.integer(year)
    if (is.na(year)) {
      res$status <- 400
      return("<p>Invalid 'year' parameter.</p>")
    }
  } else {
    year <- NULL
  }

  if (!is.na(month)) {
    month <- as.integer(month)
    if (is.na(month) || month < 1 || month > 12) {
      res$status <- 400
      return("<p>Invalid 'month' parameter. Use 1-12.</p>")
    }
  } else {
    month <- NULL
  }

  latest_stamp <- get_snowbull_stamp(con, year, month)
  map_payload <- snowbull_leaflet_mem(
    stamp = latest_stamp,
    year = year,
    month = month,
    statistic = statistic,
    language = language,
    param_name = "snow water equivalent",
    con = con
  )

  res$headers[["X-Map-Year"]] <- as.character(map_payload$year)
  res$headers[["X-Map-Month"]] <- as.character(map_payload$month)

  map_payload$html
}

# Functions and endpoints for snow survey data ########################
# Memoised version of snowInfo to improve performance. Re-used for multiple endpoints.
snowInfo_mem <- memoise::memoise(
  # Stamp argument to force cache invalidation only when needed, using a cheap DB check for more recent samples than last cache time.
  function(stamp, ...) {
    YGwater::snowInfo(...)
  },
  cache = cachem::cache_mem(), # no time limit, only invalidates when stamp changes
  omit_args = c("con") # Omit the connection as can changes each time yet is irrelevant
)

# Function to check for latest snow sample/result timestamp for cache invalidation
get_snow_stamp <- function(con) {
  rows <- DBI::dbGetQuery(
    con,
    "SELECT count(*) FROM discrete.samples WHERE import_source = 'downloadSnowCourse'"
  )[1, 1]

  stamp <- DBI::dbGetQuery(
    con,
    "
  SELECT GREATEST(
    (SELECT MAX(COALESCE(s.modified, s.created))
       FROM discrete.samples s
      WHERE s.import_source = 'downloadSnowCourse'),

    (SELECT MAX(COALESCE(r.modified, r.created))
       FROM discrete.results r
       JOIN discrete.samples s ON s.sample_id = r.sample_id
      WHERE s.import_source = 'downloadSnowCourse'
        AND r.parameter_id IN (21, 1220))
  ) AS stamp
"
  )[1, 1]

  if (is.na(stamp) | is.na(rows)) {
    "none"
  } else {
    as.character(paste0(stamp, " ", rows))
  } # stabilize type for hashing
}

#' Return basic snow survey data
#* @get /snow-survey/data
#* @serializer contentType list(type = "text/csv")
function(req, res) {
  con <- try(
    YGwater::AquaConnect(
      username = req$user,
      password = req$password,
      name = Sys.getenv("APIaquacacheName"),
      host = Sys.getenv("APIaquacacheHost"),
      port = Sys.getenv("APIaquacachePort"),
      silent = TRUE
    ),
    silent = TRUE
  )

  if (inherits(con, "try-error")) {
    res$status <- 503
    return(data.frame(
      status = "error",
      message = "Database connection failed, check your credentials.",
      stringsAsFactors = FALSE
    ))
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Find the latest snow sample datetime to use for cache invalidation
  latest <- get_snow_stamp(con)

  out <- snowInfo_mem(
    stamp = latest,
    con = con,
    complete_yrs = FALSE,
    inactive = TRUE,
    plots = FALSE,
    quiet = TRUE,
    stats = FALSE,
    save_path = NULL,
    headers = "object"
  )

  csv_with_header(
    out$measurements,
    header_lines = out$headers$measurements[[1]]
  )
}

#' Return basic snow survey metadata
#* @get /snow-survey/metadata
#* @serializer contentType list(type = "text/csv")

function(req, res) {
  con <- try(
    YGwater::AquaConnect(
      username = req$user,
      password = req$password,
      name = Sys.getenv("APIaquacacheName"),
      host = Sys.getenv("APIaquacacheHost"),
      port = Sys.getenv("APIaquacachePort"),
      silent = TRUE
    ),
    silent = TRUE
  )

  if (inherits(con, "try-error")) {
    res$status <- 503
    return(data.frame(
      status = "error",
      message = "Database connection failed, check your credentials.",
      stringsAsFactors = FALSE
    ))
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Find the latest snow sample datetime to use for cache invalidation
  latest <- get_snow_stamp(con)

  out <- snowInfo_mem(
    stamp = latest,
    con = con,
    complete_yrs = FALSE,
    inactive = TRUE,
    plots = FALSE,
    quiet = TRUE,
    stats = FALSE,
    save_path = NULL,
    headers = "object"
  )

  csv_with_header(
    out$locations,
    header_lines = out$headers$locations[[1]]
  )
}

#' Return snow survey statistics
#* @get /snow-survey/stats
#* @serializer contentType list(type = "text/csv")

function(req, res) {
  con <- try(
    YGwater::AquaConnect(
      username = req$user,
      password = req$password,
      name = Sys.getenv("APIaquacacheName"),
      host = Sys.getenv("APIaquacacheHost"),
      port = Sys.getenv("APIaquacachePort"),
      silent = TRUE
    ),
    silent = TRUE
  )

  if (inherits(con, "try-error")) {
    res$status <- 503
    return(data.frame(
      status = "error",
      message = "Database connection failed, check your credentials.",
      stringsAsFactors = FALSE
    ))
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Find the latest snow sample datetime to use for cache invalidation
  latest <- get_snow_stamp(con)

  out <- snowInfo_mem(
    stamp = latest,
    con = con,
    complete_yrs = TRUE,
    inactive = TRUE,
    plots = FALSE,
    quiet = TRUE,
    stats = TRUE,
    save_path = NULL,
    headers = "object"
  )

  csv_with_header(
    out$stats,
    header_lines = out$headers$stats[[1]]
  )
}

#' Return basic snow survey trends
#* @get /snow-survey/trends
#* @serializer contentType list(type = "text/csv")

function(req, res) {
  con <- try(
    YGwater::AquaConnect(
      username = req$user,
      password = req$password,
      name = Sys.getenv("APIaquacacheName"),
      host = Sys.getenv("APIaquacacheHost"),
      port = Sys.getenv("APIaquacachePort"),
      silent = TRUE
    ),
    silent = TRUE
  )

  if (inherits(con, "try-error")) {
    res$status <- 503
    return(data.frame(
      status = "error",
      message = "Database connection failed, check your credentials.",
      stringsAsFactors = FALSE
    ))
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Find the latest snow sample datetime to use for cache invalidation
  latest <- get_snow_stamp(con)

  out <- snowInfo_mem(
    stamp = latest,
    con = con,
    complete_yrs = TRUE,
    inactive = TRUE,
    plots = FALSE,
    quiet = TRUE,
    stats = TRUE,
    save_path = NULL,
    headers = "object"
  )

  csv_with_header(
    out$trends,
    header_lines = out$headers$trends[[1]]
  )
}


# Helper function to serialize data.frame to CSV with optional header lines
csv_with_header <- function(df, header_lines = NULL) {
  csv_lines <- capture.output(utils::write.csv(df, row.names = FALSE, na = ""))

  if (is.null(header_lines) || length(header_lines) == 0) {
    return(paste(csv_lines, collapse = "\n"))
  }

  header_lines <- paste0("# ", header_lines)
  # Add a blank line between header and CSV
  header_lines <- c(header_lines, "")

  # Enclose all lines in quotes to prevent commas from separating text into columns
  header_lines <- paste0('"', header_lines, '"')

  paste(c(header_lines, csv_lines), collapse = "\n")
}
