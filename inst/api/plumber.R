#' @apiTitle AquaCache API
#' @apiDescription API for programmatic access to the aquacache database.

# Basic authentication filter. TLS is terminated by upstream NGINX.
#* @filter auth
function(req, res) {
  # Allow anonymous only for read-only GET on these routes
  public_paths <- c(
    "^/locations$",
    "^/timeseries$",
    "^/parameters$",
    "^/samples(?:$|/)",
    "^/snow-survey(?:$|/)",
    "^/__docs__/"
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
    req$user <- "public_reader"
    req$password <- "aquacache"
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
  con <- YGwater::AquaConnect(
    username = req$user,
    password = req$password,
    silent = TRUE
  )
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
  con <- YGwater::AquaConnect(
    username = req$user,
    password = req$password,
    silent = TRUE
  )
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
#* @param start Start date (required, ISO 8601 format).
#* @param end End date (optional, ISO 8601 format, defaults to current date/time).
#* @get /timeseries/<id>/measurements
#* @serializer csv
function(req, res, id, start, end = NA) {
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

  con <- YGwater::AquaConnect(
    username = req$user,
    password = req$password,
    silent = TRUE
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  sql <- "SELECT * FROM continuous.measurements_continuous_corrected WHERE timeseries_id = $1 AND datetime >= $2 AND datetime <= $3 ORDER BY datetime DESC"
  out <- DBI::dbGetQuery(
    con,
    sql,
    params = list(as.integer(id), start, end)
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
  con <- YGwater::AquaConnect(
    username = req$user,
    password = req$password,
    silent = TRUE
  )
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

  con <- YGwater::AquaConnect(
    username = req$user,
    password = req$password,
    silent = TRUE
  )
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
#* @get /samples/<sample_ids>/results
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
  con <- YGwater::AquaConnect(
    username = req$user,
    password = req$password,
    silent = TRUE
  )
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


#' Return basic snow survey data
#* @get /snow-survey/data
#* @serializer csv

function(req, res) {
  con <- YGwater::AquaConnect(
    username = req$user,
    password = req$password,
    silent = TRUE
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  res <- YGwater::snowInfo(
    con = con,
    complete_yrs = FALSE,
    plots = FALSE,
    quiet = TRUE,
    stats = FALSE,
    save_path = NULL
  )$measurements
  return(res)
}

#' Return basic snow survey metadata
#* @get /snow-survey/metadata
#* @serializer csv

function(req, res) {
  con <- YGwater::AquaConnect(
    username = req$user,
    password = req$password,
    silent = TRUE
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  res <- YGwater::snowInfo(
    con = con,
    complete_yrs = FALSE,
    plots = FALSE,
    quiet = TRUE,
    stats = FALSE,
    save_path = NULL
  )$locations
  return(res)
}

#' Return snow survey statistics
#* @get /snow-survey/stats
#* @serializer csv

function(req, res) {
  con <- YGwater::AquaConnect(
    username = req$user,
    password = req$password,
    silent = TRUE
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  res <- YGwater::snowInfo(
    con = con,
    complete_yrs = TRUE,
    plots = FALSE,
    quiet = TRUE,
    stats = TRUE,
    save_path = NULL
  )$stats
  return(res)
}

#' Return basic snow survey trends
#* @get /snow-survey/trends
#* @serializer csv

function(req, res) {
  con <- YGwater::AquaConnect(
    username = req$user,
    password = req$password,
    silent = TRUE
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  res <- YGwater::snowInfo(
    con = con,
    complete_yrs = TRUE,
    plots = FALSE,
    quiet = TRUE,
    stats = TRUE,
    save_path = NULL
  )$trends

  return(res)
}
