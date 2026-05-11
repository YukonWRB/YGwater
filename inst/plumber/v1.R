# !IMPORTANT! Some helper functions are located at the bottom of this file.

#' @apiTitle AquaCache API version 1
#' @apiDescription API for programmatic access to the aquacache database.
#' @apiVersion 1.0.1

# Basic authentication filter. TLS is terminated by upstream NGINX.
#* @filter auth
function(req, res) {
  # Allow anonymous for read-only GET on these routes (uses public_reader account)
  public_paths <- c(
    "^/locations$",
    "^/timeseries$",
    "^/parameters$",
    "^/grades$",
    "^/approvals$",
    "^/qualifiers$",
    "^/organizations$",
    "^/samples(?:$|/)",
    "^/snow-survey(?:$|/)",
    "^/snow-bulletin/leaflet$",
    "^/__docs__/", # Swagger UI shell & assets
    "^/openapi.json$", # <-- needed for the UI to load without auth
    "^/openapi.yaml$", # <-- sometimes used
    "^/timeseries/measurements$",
    "^/samples/results$/",
    "^/csw-layer$"
  )
  is_public_ok <- identical(req$REQUEST_METHOD, "GET") &&
    any(grepl(paste(public_paths, collapse = "|"), req$PATH_INFO))

  hdr <- req$HTTP_AUTHORIZATION %||% ""
  req$is_authenticated <- nzchar(hdr)
  if (hdr == "") {
    if (!is_public_ok) {
      res$status <- 401
      res$setHeader("WWW-Authenticate", 'Basic realm="AquaCache"')
      return(list(error = "Authentication required"))
    }
    req$user <- Sys.getenv("APIaquacacheUser", "public_reader") # default to public_reader if not set. Set on CI to run with test database, otherwise uses public_reader.
    req$password <- Sys.getenv("APIaquacachePass", "aquacache")
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

request_cache_allowed <- function(req) {
  !isTRUE(req$is_authenticated)
}

#' List available locations
#* @param lang Language for location names and descriptions ("en" or "fr").
#* @param format Response format, either "csv" (default) or "json". Defaults to "csv" unless Accept: application/json is sent in the request header.
#* @get /locations
#* @serializer contentType list(type = "text/plain; charset=UTF-8")
function(req, res, lang = "en", format = NULL) {
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
    response <- data.frame(
      status = "error",
      message = "Database connection failed, check your credentials.",
      stringsAsFactors = FALSE
    )
    return(serialize_tabular(response, req, res, format))
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  sql <- if (lang == "en") {
    "SELECT * FROM public.location_metadata_en ORDER BY location_id"
  } else if (lang == "fr") {
    "SELECT * FROM public.location_metadata_fr ORDER BY location_id"
  } else {
    res$setHeader("X-Status", "error")
    response <- data.frame(
      status = "error",
      message = "Invalid language parameter. Use 'en' or 'fr'.",
      stringsAsFactors = FALSE
    )
    return(serialize_tabular(response, req, res, format))
  }

  out <- DBI::dbGetQuery(con, sql)

  if (nrow(out) == 0) {
    res$setHeader("X-Status", "info")
    response <- data.frame(
      status = "info",
      message = "No locations found in the database.",
      stringsAsFactors = FALSE
    )
    return(serialize_tabular(response, req, res, format))
  }

  return(serialize_tabular(out, req, res, format))
}

#' List available timeseries
#* @param lang Language for timeseries names and descriptions ("en" or "fr").
#* @param format Response format, either "csv" (default) or "json". Defaults to "csv" unless Accept: application/json is sent in the request header.
#* @get /timeseries
#* @serializer contentType list(type = "text/plain; charset=UTF-8")
function(req, res, lang = "en", format = NULL) {
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
    response <- data.frame(
      status = "error",
      message = "Database connection failed, check your credentials.",
      stringsAsFactors = FALSE
    )
    return(serialize_tabular(response, req, res, format))
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  visibility_sql <- if (request_cache_allowed(req)) {
    "WHERE ts.publicly_visible = TRUE"
  } else {
    ""
  }

  compound_sql <- "
    SELECT
      m.timeseries_id,
      string_agg(
        m.member_alias,
        ',' ORDER BY m.member_priority, m.member_alias
      ) AS compound_member_aliases,
      string_agg(
        m.member_timeseries_id::text,
        ',' ORDER BY m.member_priority, m.member_alias
      ) AS compound_member_timeseries_ids,
      string_agg(
        m.member_priority::text,
        ',' ORDER BY m.member_priority, m.member_alias
      ) AS compound_member_priorities,
      string_agg(
        COALESCE(m.use_from::text, ''),
        ',' ORDER BY m.member_priority, m.member_alias
      ) AS compound_member_use_from,
      string_agg(
        COALESCE(m.use_to::text, ''),
        ',' ORDER BY m.member_priority, m.member_alias
      ) AS compound_member_use_to
    FROM continuous.timeseries_compound_members m
    GROUP BY m.timeseries_id
  "

  metadata_sql <- if (lang == "en") {
    "continuous.timeseries_metadata_en"
  } else if (lang == "fr") {
    "continuous.timeseries_metadata_fr"
  } else {
    res$setHeader("X-Status", "error")
    response <- data.frame(
      status = "error",
      message = "Invalid language parameter. Use 'en' or 'fr'.",
      stringsAsFactors = FALSE
    )
    return(serialize_tabular(response, req, res, format))
  }

  sql <- sprintf(
    "SELECT
       tm.*,
       ts.publicly_visible,
       ts.active,
       ts.default_owner AS default_owner_organization_id,
       org.name AS default_owner,
       org.name_fr AS default_owner_fr,
       ts.timezone_daily_calc,
       ts.last_daily_calculation,
       ts.last_synchronize,
       ts.matrix_state_id,
       ms.matrix_state_name,
       ms.matrix_state_name_fr,
       ts.sub_location_id,
       sl.sub_location_name,
       sl.sub_location_name_fr,
       tc.expression_sql AS compound_expression_sql,
       cm.compound_member_aliases,
       cm.compound_member_timeseries_ids,
       cm.compound_member_priorities,
       cm.compound_member_use_from,
       cm.compound_member_use_to
     FROM %s tm
     JOIN continuous.timeseries ts
       ON tm.timeseries_id = ts.timeseries_id
     LEFT JOIN public.organizations org
       ON ts.default_owner = org.organization_id
     LEFT JOIN public.matrix_states ms
       ON ts.matrix_state_id = ms.matrix_state_id
     LEFT JOIN public.sub_locations sl
       ON ts.sub_location_id = sl.sub_location_id
     LEFT JOIN continuous.timeseries_compounds tc
       ON ts.timeseries_id = tc.timeseries_id
     LEFT JOIN (%s) cm
       ON ts.timeseries_id = cm.timeseries_id
     %s
     ORDER BY tm.timeseries_id",
    metadata_sql,
    compound_sql,
    visibility_sql
  )
  out <- DBI::dbGetQuery(con, sql)

  if (nrow(out) == 0) {
    res$setHeader("X-Status", "info")
    response <- data.frame(
      status = "info",
      message = "No timeseries found in the database.",
      stringsAsFactors = FALSE
    )
    return(serialize_tabular(response, req, res, format))
  }

  return(serialize_tabular(out, req, res, format))
}


#' Return measurements for a timeseries
#* @param id Timeseries IDs to target, separated by commas (required).
#* @param start Start date/time, inclusive (required, ISO 8601 i.e. 2025-01-01 00:00).
#* @param end End date/time, inclusive (optional; defaults to now, ISO 8601).
#* @param limit Maximum number of records to return (optional; defaults to 100000).
#* @param modifiedSince Only return measurements created or modified since this ISO 8601 date/time.
#* @param format Response format, either "csv" (default) or "json". Defaults to "csv" unless Accept: application/json is sent in the request header.
#* @get /timeseries/measurements
#* @serializer contentType list(type = "text/plain; charset=UTF-8")
function(
  req,
  res,
  id,
  start,
  end = NA,
  limit = 100000,
  modifiedSince = NA,
  format = NULL
) {
  if (missing(id)) {
    res$setHeader("X-Status", "error")
    response <- data.frame(
      status = "error",
      message = "Missing required 'id' parameter.",
      stringsAsFactors = FALSE
    )
    return(serialize_tabular(response, req, res, format))
  }

  # Ensure start and end are provided and can be converted to POSIXct
  if (missing(start)) {
    res$setHeader("X-Status", "error")
    response <- data.frame(
      status = "error",
      message = "Missing required 'start' parameter.",
      stringsAsFactors = FALSE
    )
    return(serialize_tabular(response, req, res, format))
  }
  if (missing(end)) {
    end <- Sys.time()
  }
  start <- try(as.POSIXct(start, tz = "UTC"), silent = TRUE)
  end <- try(as.POSIXct(end, tz = "UTC"), silent = TRUE)

  if (inherits(start, "try-error") || is.na(start)) {
    res$setHeader("X-Status", "error")
    response <- data.frame(
      status = "error",
      message = "Invalid 'start' parameter. Must be in ISO 8601 format.",
      stringsAsFactors = FALSE
    )
    return(serialize_tabular(response, req, res, format))
  }
  if (inherits(end, "try-error") || is.na(end)) {
    res$setHeader("X-Status", "error")
    response <- data.frame(
      status = "error",
      message = "Invalid 'end' parameter. Must be in ISO 8601 format.",
      stringsAsFactors = FALSE
    )
    return(serialize_tabular(response, req, res, format))
  }

  modified_since_missing <- missing(modifiedSince) ||
    length(modifiedSince) == 0L ||
    is.na(modifiedSince[1])
  if (!modified_since_missing) {
    modifiedSince <- try(as.POSIXct(modifiedSince, tz = "UTC"), silent = TRUE)
    if (inherits(modifiedSince, "try-error") || is.na(modifiedSince)) {
      res$setHeader("X-Status", "error")
      response <- data.frame(
        status = "error",
        message = "Invalid 'modifiedSince' parameter. Must be in ISO 8601 format.",
        stringsAsFactors = FALSE
      )
      return(serialize_tabular(response, req, res, format))
    }
  } else {
    modifiedSince <- NULL
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
    response <- data.frame(
      status = "error",
      message = "Database connection failed, check your credentials.",
      stringsAsFactors = FALSE
    )
    return(serialize_tabular(response, req, res, format))
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Break id apart by comma and convert to integer vector
  id <- unlist(strsplit(id, ","))
  id <- as.integer(id)
  id <- id[!is.na(id)]
  if (length(id) == 0) {
    res$setHeader("X-Status", "error")
    response <- data.frame(
      status = "error",
      message = "Invalid 'id' parameter. Must contain at least one integer timeseries_id.",
      stringsAsFactors = FALSE
    )
    return(serialize_tabular(response, req, res, format))
  }
  include_private <- !request_cache_allowed(req)
  measurement_join_sql <- "
    LEFT JOIN LATERAL (
      SELECT
        g.grade_type_id,
        gt.grade_type_code,
        gt.grade_type_description
      FROM continuous.grades g
      LEFT JOIN public.grade_types gt
        ON g.grade_type_id = gt.grade_type_id
      WHERE g.timeseries_id = m.timeseries_id
        AND g.start_dt <= m.datetime
        AND g.end_dt >= m.datetime
      ORDER BY g.start_dt DESC, g.grade_id DESC
      LIMIT 1
    ) grade ON TRUE
    LEFT JOIN LATERAL (
      SELECT
        a.approval_type_id,
        at.approval_type_code,
        at.approval_type_description
      FROM continuous.approvals a
      LEFT JOIN public.approval_types at
        ON a.approval_type_id = at.approval_type_id
      WHERE a.timeseries_id = m.timeseries_id
        AND a.start_dt <= m.datetime
        AND a.end_dt >= m.datetime
      ORDER BY a.start_dt DESC, a.approval_id DESC
      LIMIT 1
    ) approval ON TRUE
    LEFT JOIN LATERAL (
      SELECT
        string_agg(
          q.qualifier_type_id::text,
          ',' ORDER BY q.qualifier_type_id
        ) AS qualifier_type_ids,
        string_agg(
          q.qualifier_type_code,
          ',' ORDER BY q.qualifier_type_id
        ) AS qualifier_type_codes,
        string_agg(
          q.qualifier_type_description,
          ',' ORDER BY q.qualifier_type_id
        ) AS qualifier_type_descriptions
      FROM (
        SELECT DISTINCT ON (q.qualifier_type_id)
          q.qualifier_type_id,
          qt.qualifier_type_code,
          qt.qualifier_type_description
        FROM continuous.qualifiers q
        LEFT JOIN public.qualifier_types qt
          ON q.qualifier_type_id = qt.qualifier_type_id
        WHERE q.timeseries_id = m.timeseries_id
          AND q.start_dt <= m.datetime
          AND q.end_dt >= m.datetime
        ORDER BY q.qualifier_type_id, q.start_dt DESC, q.qualifier_id DESC
      ) q
    ) qualifier ON TRUE
    LEFT JOIN LATERAL (
      SELECT o.organization_id AS owner_organization_id
      FROM continuous.owners o
      WHERE o.timeseries_id = m.timeseries_id
        AND o.start_dt <= m.datetime
        AND o.end_dt >= m.datetime
      ORDER BY o.start_dt DESC, o.owner_id DESC
      LIMIT 1
    ) owner_range ON TRUE
    LEFT JOIN public.organizations owner_org
      ON owner_range.owner_organization_id = owner_org.organization_id
    LEFT JOIN LATERAL (
      SELECT c.organization_id AS contributor_organization_id
      FROM continuous.contributors c
      WHERE c.timeseries_id = m.timeseries_id
        AND c.start_dt <= m.datetime
        AND c.end_dt >= m.datetime
      ORDER BY c.start_dt DESC, c.contributor_id DESC
      LIMIT 1
    ) contributor_range ON TRUE
    LEFT JOIN public.organizations contributor_org
      ON contributor_range.contributor_organization_id =
        contributor_org.organization_id
  "

  measurement_select_sql <- "
    SELECT
      m.timeseries_id,
      m.datetime,
      m.value_raw,
      m.value_corrected,
      m.period,
      m.imputed,
      m.created,
      m.modified,
      grade.grade_type_id,
      grade.grade_type_code,
      grade.grade_type_description,
      approval.approval_type_id,
      approval.approval_type_code,
      approval.approval_type_description,
      qualifier.qualifier_type_ids,
      qualifier.qualifier_type_codes,
      qualifier.qualifier_type_descriptions,
      owner_range.owner_organization_id,
      owner_org.name AS owner,
      contributor_range.contributor_organization_id,
      contributor_org.name AS contributor
  "

  basic_modified_filter_sql <- ""
  compound_modified_filter_sql <- ""
  query_params <- list(start, end, include_private, lim)
  limit_param <- "$4"
  if (!is.null(modifiedSince)) {
    basic_modified_filter_sql <- "
       AND (
         mc.created >= $4
         OR mc.modified >= $4
       )"
    compound_modified_filter_sql <- "
       AND (
         source_stamp.created >= $4
         OR source_stamp.modified >= $4
       )"
    query_params <- list(start, end, include_private, modifiedSince, lim)
    limit_param <- "$5"
  }

  # timeseries_id passed in via sprintf, but no injection potential because converted to integer first.
  out <- DBI::dbGetQuery(
    con,
    sprintf(
      paste0(
        "WITH RECURSIVE requested_timeseries(timeseries_id) AS (
           SELECT unnest(ARRAY[%s]::integer[])
         ),
         selected_timeseries AS (
           SELECT
             ts.timeseries_id,
             ts.timeseries_type
           FROM requested_timeseries r
           JOIN continuous.timeseries ts
             ON r.timeseries_id = ts.timeseries_id
           WHERE ($3::boolean OR ts.publicly_visible)
         ),
         timeseries_tree(
           root_timeseries_id,
           source_timeseries_id,
           source_type,
           path
         ) AS (
           SELECT
             st.timeseries_id,
             st.timeseries_id,
             st.timeseries_type,
             ARRAY[st.timeseries_id]
           FROM selected_timeseries st

           UNION ALL

           SELECT
             tt.root_timeseries_id,
             cm.member_timeseries_id,
             member_ts.timeseries_type,
             tt.path || cm.member_timeseries_id
           FROM timeseries_tree tt
           JOIN continuous.timeseries_compound_members cm
             ON tt.source_timeseries_id = cm.timeseries_id
           JOIN continuous.timeseries member_ts
             ON cm.member_timeseries_id = member_ts.timeseries_id
           WHERE tt.source_type = 'compound'
             AND NOT cm.member_timeseries_id = ANY(tt.path)
         ),
         timeseries_sources AS (
           SELECT root_timeseries_id, source_timeseries_id
           FROM timeseries_tree
           WHERE source_type = 'basic'
         ),
         measurement_rows AS (
           SELECT
             mc.timeseries_id,
             mc.datetime,
             mc.value AS value_raw,
             continuous.apply_corrections(
               mc.timeseries_id,
               mc.datetime,
               mc.value
             ) AS value_corrected,
             mc.period,
             mc.imputed,
             mc.created,
             mc.modified
           FROM selected_timeseries st
           JOIN continuous.measurements_continuous mc
             ON st.timeseries_id = mc.timeseries_id
           WHERE st.timeseries_type = 'basic'
             AND mc.datetime >= $1
             AND mc.datetime <= $2",
        basic_modified_filter_sql,
        "
           UNION ALL

           SELECT
             m.timeseries_id,
             m.datetime,
             m.value_raw,
             m.value_corrected,
             m.period,
             m.imputed,
             source_stamp.created,
             source_stamp.modified
           FROM selected_timeseries st
           JOIN LATERAL continuous.measurements_continuous_corrected(
             st.timeseries_id,
             $1,
             $2
           ) m ON TRUE
           LEFT JOIN LATERAL (
             SELECT
               MAX(mc.created) AS created,
               MAX(mc.modified) AS modified
             FROM timeseries_sources src
             JOIN continuous.measurements_continuous mc
               ON src.source_timeseries_id = mc.timeseries_id
              AND mc.datetime = m.datetime
             WHERE src.root_timeseries_id = m.timeseries_id
           ) source_stamp ON TRUE
           WHERE st.timeseries_type <> 'basic'",
        compound_modified_filter_sql,
        "
         )
         ",
        measurement_select_sql,
        "
         FROM measurement_rows m",
        measurement_join_sql,
        "ORDER BY m.datetime ASC
         LIMIT %s"
      ),
      paste(id, collapse = ","),
      limit_param
    ),
    params = query_params
  )

  if (nrow(out) == 0) {
    res$setHeader("X-Status", "info")
    response <- data.frame(
      status = "info",
      message = "No measurements found for the specified timeseries and date range.",
      stringsAsFactors = FALSE
    )
    return(serialize_tabular(response, req, res, format))
  }

  return(serialize_tabular(out, req, res, format))
}

#' Return available parameters in the database
#* @param format Response format, either "csv" (default) or "json". Defaults to "csv" unless Accept: application/json is sent in the request header.
#* @get /parameters
#* @serializer contentType list(type = "text/plain; charset=UTF-8")
function(req, res, format = NULL) {
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
    response <- data.frame(
      status = "error",
      message = "Database connection failed, check your credentials.",
      stringsAsFactors = FALSE
    )
    return(serialize_tabular(response, req, res, format))
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  sql <- paste(
    "SELECT p.parameter_id, p.param_name, p.param_name_fr,",
    "p.description, p.description_fr,",
    ac_parameter_unit_select_sql(con, "p", "units"),
    "FROM public.parameters p ORDER BY p.parameter_id"
  )
  out <- DBI::dbGetQuery(con, sql)

  if (nrow(out) == 0) {
    res$setHeader("X-Status", "info")
    response <- data.frame(
      status = "info",
      message = "No parameters found in the database.",
      stringsAsFactors = FALSE
    )
    return(serialize_tabular(response, req, res, format))
  }

  return(serialize_tabular(out, req, res, format))
}

#' Return grade types in the database
#* @param format Response format, either "csv" (default) or "json". Defaults to "csv" unless Accept: application/json is sent in the request header.
#* @get /grades
#* @serializer contentType list(type = "text/plain; charset=UTF-8")
function(req, res, format = NULL) {
  out <- api_lookup_query(
    req,
    res,
    "SELECT
       grade_type_id,
       grade_type_code,
       grade_type_description,
       grade_type_description_fr,
       color_code
     FROM public.grade_types
     ORDER BY grade_type_id",
    "No grade types found in the database."
  )

  return(serialize_tabular(out, req, res, format))
}

#' Return approval types in the database
#* @param format Response format, either "csv" (default) or "json". Defaults to "csv" unless Accept: application/json is sent in the request header.
#* @get /approvals
#* @serializer contentType list(type = "text/plain; charset=UTF-8")
function(req, res, format = NULL) {
  out <- api_lookup_query(
    req,
    res,
    "SELECT
       approval_type_id,
       approval_type_code,
       approval_type_description,
       approval_type_description_fr,
       color_code
     FROM public.approval_types
     ORDER BY approval_type_id",
    "No approval types found in the database."
  )
  return(serialize_tabular(out, req, res, format))
}

#' Return qualifier types in the database
#* @param format Response format, either "csv" (default) or "json". Defaults to "csv" unless Accept: application/json is sent in the request header.
#* @get /qualifiers
#* @serializer contentType list(type = "text/plain; charset=UTF-8")
function(req, res, format = NULL) {
  out <- api_lookup_query(
    req,
    res,
    "SELECT
       qualifier_type_id,
       qualifier_type_code,
       qualifier_type_description,
       qualifier_type_description_fr,
       color_code
     FROM public.qualifier_types
     ORDER BY qualifier_type_id",
    "No qualifier types found in the database."
  )

  return(serialize_tabular(out, req, res, format))
}

#' Return organizations in the database
#* @param format Response format, either "csv" (default) or "json". Defaults to "csv" unless Accept: application/json is sent in the request header.
#* @get /organizations
#* @serializer contentType list(type = "text/plain; charset=UTF-8")
function(req, res, format = NULL) {
  out <- api_lookup_query(
    req,
    res,
    "SELECT
       organization_id,
       name,
       name_fr,
       contact_name,
       phone,
       email,
       note
     FROM public.organizations
     ORDER BY name, organization_id",
    "No organizations found in the database."
  )

  return(serialize_tabular(out, req, res, format))
}

#' Return sample metadata
#* @param start Start date (required, ISO 8601 format).
#* @param end End date (optional, ISO 8601 format, defaults to current date/time).
#* @param locations Location ID (optional, integer string separated by commas). If provided, filters samples to these locations.
#* @param parameters Parameter ID (optional, integer string separated by commas). If provided, filters samples to only those which include these parameters.
#* @param modifiedSince Only return samples created or modified since this ISO 8601 date/time.
#* @param format Response format, either "csv" (default) or "json". Defaults to "csv" unless Accept: application/json is sent in the request header.
#* @get /samples
#* @serializer contentType list(type = "text/plain; charset=UTF-8")
function(
  req,
  res,
  start,
  end = NA,
  locations = NA,
  parameters = NA,
  modifiedSince = NA,
  format = NULL
) {
  # Ensure start and end are provided and can be converted to POSIXct
  if (missing(start)) {
    res$setHeader("X-Status", "error")
    response <- data.frame(
      status = "error",
      message = "Missing required 'start' parameter.",
      stringsAsFactors = FALSE
    )
    return(serialize_tabular(response, req, res, format))
  }
  if (missing(end)) {
    end <- Sys.time()
  }
  start <- try(as.POSIXct(start, tz = "UTC"), silent = TRUE)
  end <- try(as.POSIXct(end, tz = "UTC"), silent = TRUE)

  if (inherits(start, "try-error") || is.na(start)) {
    res$setHeader("X-Status", "error")
    response <- data.frame(
      status = "error",
      message = "Invalid 'start' parameter. Must be in ISO 8601 format.",
      stringsAsFactors = FALSE
    )
    return(serialize_tabular(response, req, res, format))
  }
  if (inherits(end, "try-error") || is.na(end)) {
    res$setHeader("X-Status", "error")
    response <- data.frame(
      status = "error",
      message = "Invalid 'end' parameter. Must be in ISO 8601 format.",
      stringsAsFactors = FALSE
    )
    return(serialize_tabular(response, req, res, format))
  }
  modified_since_missing <- missing(modifiedSince) ||
    length(modifiedSince) == 0L ||
    is.na(modifiedSince[1])
  if (!modified_since_missing) {
    modifiedSince <- try(as.POSIXct(modifiedSince, tz = "UTC"), silent = TRUE)
    if (inherits(modifiedSince, "try-error") || is.na(modifiedSince)) {
      res$setHeader("X-Status", "error")
      response <- data.frame(
        status = "error",
        message = "Invalid 'modifiedSince' parameter. Must be in ISO 8601 format.",
        stringsAsFactors = FALSE
      )
      return(serialize_tabular(response, req, res, format))
    }
  } else {
    modifiedSince <- NULL
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
    response <- data.frame(
      status = "error",
      message = "Database connection failed, check your credentials.",
      stringsAsFactors = FALSE
    )
    return(serialize_tabular(response, req, res, format))
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  sql <- "SELECT
    sm.*
  FROM discrete.samples_metadata_en sm
  WHERE sm.datetime >= $1 AND sm.datetime <= $2"

  if (!is.na(locations[1])) {
    sql <- paste0(
      sql,
      " AND sm.location_id IN (",
      paste(as.integer(strsplit(locations, ",")[[1]]), collapse = ","),
      ")"
    )
  }
  if (!is.na(parameters[1])) {
    sql <- paste0(
      sql,
      " AND EXISTS (
        SELECT 1
        FROM discrete.results r
        WHERE r.sample_id = sm.sample_id
          AND r.parameter_id IN (",
      paste(as.integer(strsplit(parameters, ",")[[1]]), collapse = ","),
      "))"
    )
  }
  query_params <- list(start, end)
  if (!is.null(modifiedSince)) {
    sql <- paste0(
      sql,
      " AND (sm.created >= $3 OR sm.modified >= $3)"
    )
    query_params <- list(start, end, modifiedSince)
  }
  sql <- paste0(sql, " ORDER BY sm.datetime ASC")

  out <- DBI::dbGetQuery(
    con,
    sql,
    params = query_params
  )

  if (nrow(out) == 0) {
    res$setHeader("X-Status", "info")
    response <- data.frame(
      status = "info",
      message = "No samples found for the specified criteria.",
      stringsAsFactors = FALSE
    )
    return(serialize_tabular(response, req, res, format))
  }
  return(serialize_tabular(out, req, res, format))
}

#' Return sample results
#* @param sample_ids Sample ID (required, integer string separated by commas).
#* @param parameters Parameter ID (optional, integer string separated by commas). If provided, filters results to these parameters.
#* @param modifiedSince Only return results created or modified since this ISO 8601 date/time.
#* @param format Response format, either "csv" (default) or "json". Defaults to "csv" unless Accept: application/json is sent in the request header.
#* @get /samples/results
#* @serializer contentType list(type = "text/plain; charset=UTF-8")
function(
  req,
  res,
  sample_ids,
  parameters = NA,
  modifiedSince = NA,
  format = NULL
) {
  if (missing(sample_ids)) {
    res$setHeader("X-Status", "error")
    response <- data.frame(
      status = "error",
      message = "Missing required 'sample_ids' parameter.",
      stringsAsFactors = FALSE
    )
    return(serialize_tabular(response, req, res, format))
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
    response <- data.frame(
      status = "error",
      message = "Database connection failed, check your credentials.",
      stringsAsFactors = FALSE
    )
    return(serialize_tabular(response, req, res, format))
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  sids <- as.integer(strsplit(sample_ids, ",")[[1]])
  modified_since_missing <- missing(modifiedSince) ||
    length(modifiedSince) == 0L ||
    is.na(modifiedSince[1])
  if (!modified_since_missing) {
    modifiedSince <- try(as.POSIXct(modifiedSince, tz = "UTC"), silent = TRUE)
    if (inherits(modifiedSince, "try-error") || is.na(modifiedSince)) {
      res$setHeader("X-Status", "error")
      response <- data.frame(
        status = "error",
        message = "Invalid 'modifiedSince' parameter. Must be in ISO 8601 format.",
        stringsAsFactors = FALSE
      )
      return(serialize_tabular(response, req, res, format))
    }
  } else {
    modifiedSince <- NULL
  }

  sql <- paste0(
    "SELECT
    r.*
  FROM discrete.results_metadata_en r
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
  query_params <- list()
  if (!is.null(modifiedSince)) {
    sql <- paste0(
      sql,
      " AND (r.created >= $1 OR r.modified >= $1)"
    )
    query_params <- list(modifiedSince)
  }
  sql <- paste0(sql, " ORDER BY r.parameter_id")

  if (is.null(modifiedSince)) {
    out <- DBI::dbGetQuery(con, sql)
  } else {
    out <- DBI::dbGetQuery(con, sql, params = query_params)
  }

  if (nrow(out) == 0) {
    res$setHeader("X-Status", "info")
    response <- data.frame(
      status = "info",
      message = "No results found for the specified sample ID and parameters.",
      stringsAsFactors = FALSE
    )
    return(serialize_tabular(response, req, res, format))
  }
  return(serialize_tabular(out, req, res, format))
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

get_snowbull_stamp <- function(
  con,
  year = NULL,
  month = NULL,
  continuous = FALSE,
  discrete = TRUE
) {
  param_id <- DBI::dbGetQuery(
    con,
    "SELECT parameter_id FROM public.parameters WHERE param_name = 'snow water equivalent'"
  )[1, 1]

  if (is.na(param_id) | is.null(year) | is.null(month)) {
    return("none")
  }

  if (continuous) {
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
  } else {
    continuous_stamp <- NA
  }

  if (discrete) {
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
  } else {
    discrete_stamp <- NA
  }

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
#* @param continuous If year and/or month are not provided, whether to consider continuous data for determining the latest available bulletin (default FALSE).
#* @param discrete If year and/or month are not provided, whether to consider discrete data for determining the latest available bulletin (default TRUE).
#* @get /snow-bulletin/leaflet
#* @serializer contentType list(type = "text/html")
function(
  req,
  res,
  year = NA,
  month = NA,
  statistic = "relative_to_med",
  language = "English",
  continuous = FALSE,
  discrete = TRUE
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
    # Find the most recent year for which there is data available
    if (discrete) {
      year_disc <- DBI::dbGetQuery(
        con,
        "
      SELECT MAX(EXTRACT(YEAR FROM s.target_datetime)) AS latest_year
      FROM discrete.samples s
      JOIN discrete.results r ON s.sample_id = r.sample_id
      WHERE r.parameter_id = (SELECT parameter_id FROM public.parameters WHERE param_name = 'snow water equivalent')
        AND r.result IS NOT NULL AND DATE(s.target_datetime) >= DATE('1990-10-01')"
      )[1, 1]
    } else {
      year_disc <- NA
    }
    if (continuous) {
      year_cont <- DBI::dbGetQuery(
        con,
        "
      SELECT MAX(EXTRACT(YEAR FROM m.date)) AS latest_year
      FROM continuous.measurements_calculated_daily_corrected m
      JOIN continuous.timeseries t ON m.timeseries_id = t.timeseries_id
      WHERE t.parameter_id = (SELECT parameter_id FROM public.parameters WHERE param_name = 'snow water equivalent')
        AND DATE(m.date) >= DATE('1990-10-01')"
      )[1, 1]
    } else {
      year_cont <- NA
    }
    if (all(is.na(c(year_disc, year_cont)))) {
      year <- NULL
    } else {
      year <- max(c(year_disc, year_cont), na.rm = TRUE)
    }
  }

  if (!is.na(month)) {
    month <- as.integer(month)
    if (is.na(month) || month < 1 || month > 12) {
      res$status <- 400
      return("<p>Invalid 'month' parameter. Use 1-12.</p>")
    }
  } else {
    if (!is.null(year)) {
      # Need the year to determine the latest month, otherwise leave as NULL to get the latest overall regardless of month
      if (discrete) {
        month_disc <- DBI::dbGetQuery(
          con,
          "
      SELECT MAX(EXTRACT(MONTH FROM s.target_datetime)) AS latest_month
      FROM discrete.samples s
      JOIN discrete.results r ON s.sample_id = r.sample_id
      WHERE r.parameter_id = (SELECT parameter_id FROM public.parameters WHERE param_name = 'snow water equivalent')
        AND r.result IS NOT NULL AND DATE(s.target_datetime) >= DATE('1990-10-01')
        AND EXTRACT(YEAR FROM s.target_datetime) = $1",
          params = list(year)
        )[1, 1]
      } else {
        month_disc <- NA
      }
      if (continuous) {
        month_cont <- DBI::dbGetQuery(
          con,
          "SELECT MAX(EXTRACT(MONTH FROM date)) AS latest_month
        FROM continuous.measurements_calculated_daily_corrected m
        JOIN continuous.timeseries t ON m.timeseries_id = t.timeseries_id
        WHERE t.parameter_id = (SELECT parameter_id FROM public.parameters WHERE param_name = 'snow water equivalent')
          AND DATE(date) >= DATE('1990-10-01')
          AND EXTRACT(YEAR FROM date) = $1",
          params = list(year)
        )[1, 1]
      } else {
        month_cont <- NA
      }
      if (all(is.na(c(month_disc, month_cont)))) {
        month <- NULL
      } else {
        month <- max(c(month_disc, month_cont), na.rm = TRUE)
      }
    } else {
      month <- NULL
    }
  }

  latest_stamp <- get_snowbull_stamp(con, year, month)
  message("Year =", year, " Month=", month, " Stamp=", latest_stamp)
  if (request_cache_allowed(req)) {
    map_payload <- snowbull_leaflet_mem(
      stamp = latest_stamp,
      year = year,
      month = month,
      statistic = statistic,
      language = language,
      param_name = "snow water equivalent",
      con = con
    )
  } else {
    res$setHeader("Cache-Control", "no-store")
    map_payload <- YGwater:::create_snowbull_leaflet_html(
      year = year,
      month = month,
      param_name = "snow water equivalent",
      statistic = statistic,
      language = language,
      con = con
    )
  }

  res$setHeader("X-Map-Year", as.character(map_payload$year))
  res$setHeader("X-Map-Month", as.character(map_payload$month))

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

  if (request_cache_allowed(req)) {
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
  } else {
    res$setHeader("Cache-Control", "no-store")
    out <- YGwater::snowInfo(
      con = con,
      complete_yrs = FALSE,
      inactive = TRUE,
      plots = FALSE,
      quiet = TRUE,
      stats = FALSE,
      save_path = NULL,
      headers = "object"
    )
  }

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

  if (request_cache_allowed(req)) {
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
  } else {
    res$setHeader("Cache-Control", "no-store")
    out <- YGwater::snowInfo(
      con = con,
      complete_yrs = FALSE,
      inactive = TRUE,
      plots = FALSE,
      quiet = TRUE,
      stats = FALSE,
      save_path = NULL,
      headers = "object"
    )
  }

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

  if (request_cache_allowed(req)) {
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
  } else {
    res$setHeader("Cache-Control", "no-store")
    out <- YGwater::snowInfo(
      con = con,
      complete_yrs = TRUE,
      inactive = TRUE,
      plots = FALSE,
      quiet = TRUE,
      stats = TRUE,
      save_path = NULL,
      headers = "object"
    )
  }

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

  if (request_cache_allowed(req)) {
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
  } else {
    res$setHeader("Cache-Control", "no-store")
    out <- YGwater::snowInfo(
      con = con,
      complete_yrs = TRUE,
      inactive = TRUE,
      plots = FALSE,
      quiet = TRUE,
      stats = TRUE,
      save_path = NULL,
      headers = "object"
    )
  }

  csv_with_header(
    out$trends,
    header_lines = out$headers$trends[[1]]
  )
}

# End point to pass postgres function get_csw_layer() output directly as CSV, as it comes out of the DB.
#' Return CSW layer data (Geomatics Yukon specific)
#* @get /csw-layer
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

  out <- DBI::dbGetQuery(con, "SELECT * FROM public.get_csw_layer()")

  if (nrow(out) == 0) {
    res$setHeader("X-Status", "info")
    return(data.frame(
      status = "info",
      message = "No CSW layer data found in the database.",
      stringsAsFactors = FALSE
    ))
  }
  return(out)
}


# Helper function to serialize data.frame to CSV with optional header lines
csv_with_header <- function(df, header_lines = NULL) {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  data.table::fwrite(
    df,
    file = tmp,
    na = "",
    quote = "auto",
    dateTimeAs = "ISO"
  )

  csv_lines <- readLines(tmp, warn = FALSE)

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

# Helper function to perform a database query for lookup tables (e.g. parameters, grade types) and handle connection and empty results uniformly
api_lookup_query <- function(req, res, sql, empty_message) {
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

  out <- DBI::dbGetQuery(con, sql)

  if (nrow(out) == 0) {
    res$setHeader("X-Status", "info")
    return(data.frame(
      status = "info",
      message = empty_message,
      stringsAsFactors = FALSE
    ))
  }

  out
}

# Helper function to determine API response format based on query parameter or Accept header, defaulting to CSV for backward compatibility
api_format <- function(req, res = NULL, format = NULL) {
  format_supplied <- !is.null(format) && nzchar(format)
  format <- tolower(format %||% "")

  if (format_supplied && !format %in% c("json", "csv")) {
    if (!is.null(res)) {
      res$status <- 400
    }
    stop("Invalid format parameter. Use 'csv' or 'json'.", call. = FALSE)
  }

  if (format %in% c("json", "csv")) {
    return(format)
  }

  accept <- req$HTTP_ACCEPT %||% ""

  if (grepl("application/json", accept, ignore.case = TRUE)) {
    return("json")
  }

  "csv"
}

# Clean up classes 'pq_text' and 'pq_jsonb' from RPostgres to ensure they serialize properly to JSON, converting arrays to lists and parsing JSON strings as needed
clean_for_json <- function(x) {
  if (!is.data.frame(x)) {
    return(x)
  }

  x[] <- lapply(x, function(col) {
    cls <- class(col)

    # PostgreSQL array columns from RPostgres, e.g. pq__text
    if (any(grepl("^pq__", cls))) {
      return(lapply(col, function(v) {
        if (is.null(v) || length(v) == 0L || all(is.na(v))) {
          return(character(0))
        }

        as.character(v)
      }))
    }

    # PostgreSQL json/jsonb columns from RPostgres
    if (inherits(col, "pq_jsonb") || inherits(col, "pq_json")) {
      return(lapply(col, function(v) {
        if (is.null(v) || length(v) == 0L || all(is.na(v))) {
          return(NULL)
        }

        if (!is.character(v)) {
          return(v)
        }

        tryCatch(
          jsonlite::fromJSON(v, simplifyVector = FALSE),
          error = function(e) as.character(v)
        )
      }))
    }

    col
  })

  x
}

# Helper function to serialize tabular data to JSON or CSV based on request parameters and set appropriate Content-Type header
serialize_tabular <- function(x, req, res, format = NULL) {
  format <- tryCatch(
    api_format(req = req, res = res, format = format),
    error = function(e) {
      res$status <- 400
      res$setHeader("Content-Type", "application/json")
      return("format_error")
    }
  )

  if (identical(format, "format_error")) {
    return(jsonlite::toJSON(
      data.frame(
        status = "error",
        message = "Invalid format parameter. Use 'csv' or 'json'.",
        stringsAsFactors = FALSE
      ),
      dataframe = "rows",
      auto_unbox = TRUE
    ))
  }
  if (format == "json") {
    res$setHeader("Content-Type", "application/json")

    x <- clean_for_json(x)

    return(jsonlite::toJSON(
      x,
      dataframe = "rows",
      na = "null",
      null = "null",
      POSIXt = "ISO8601",
      pretty = FALSE,
      auto_unbox = TRUE
    ))
  }

  res$setHeader("Content-Type", "text/csv; charset=UTF-8")

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  data.table::fwrite(
    x,
    file = tmp,
    na = "",
    quote = "auto",
    dateTimeAs = "ISO"
  )

  paste(readLines(tmp, warn = FALSE), collapse = "\n")
}
