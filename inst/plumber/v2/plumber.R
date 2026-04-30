#* AquaCache API version 2
#*
#* API for programmatic access to AquaCache using plumber2.
#*
#* @version 2.0.0
"_API"

v2_csv_serializer <- function(...) {
  function(value) {
    if (is.null(value)) {
      return("")
    }

    paste(
      capture.output(utils::write.csv(value, row.names = FALSE, na = "")),
      collapse = "\n"
    )
  }
}

v2_text_serializer <- function(...) {
  function(value) {
    if (is.null(value)) {
      return("")
    }

    paste(value, collapse = "\n")
  }
}

v2_csv_with_header <- function(df, header_lines = NULL) {
  csv_lines <- capture.output(utils::write.csv(df, row.names = FALSE, na = ""))

  if (is.null(header_lines) || length(header_lines) == 0L) {
    return(paste(csv_lines, collapse = "\n"))
  }

  header_lines <- paste0("# ", header_lines)
  header_lines <- c(header_lines, "")
  header_lines <- paste0('"', header_lines, '"')

  paste(c(header_lines, csv_lines), collapse = "\n")
}

v2_query_missing <- function(value) {
  is.null(value) ||
    length(value) == 0L ||
    identical(value[[1]], "") ||
    is.na(value[[1]])
}

v2_query_value <- function(query, name, default = NULL) {
  value <- query[[name]]

  if (v2_query_missing(value)) {
    return(default)
  }

  value[[1]]
}

v2_error_df <- function(message, status = "error") {
  data.frame(
    status = status,
    message = message,
    stringsAsFactors = FALSE
  )
}

v2_resolve_credentials <- function(request) {
  hdr <- request$get_header("Authorization")

  if (is.null(hdr) || length(hdr) == 0L || !nzchar(hdr)) {
    return(list(
      user = Sys.getenv("APIaquacacheUser", "public_reader"),
      password = Sys.getenv("APIaquacachePass", "aquacache"),
      authenticated = FALSE
    ))
  }

  if (!grepl("^Basic\\s+", hdr)) {
    return(list(error = "Invalid authentication header"))
  }

  encoded <- sub("^Basic\\s+", "", hdr)
  decoded <- try(rawToChar(jsonlite::base64_dec(encoded)), silent = TRUE)

  if (inherits(decoded, "try-error")) {
    return(list(error = "Invalid authentication header"))
  }

  separator <- regexpr(":", decoded, fixed = TRUE)
  if (separator < 1) {
    return(list(error = "Invalid authentication header"))
  }

  list(
    user = substr(decoded, 1L, separator - 1L),
    password = substr(decoded, separator + 1L, nchar(decoded)),
    authenticated = TRUE
  )
}

v2_open_connection <- function(credentials) {
  try(
    YGwater::AquaConnect(
      username = credentials$user,
      password = credentials$password,
      name = Sys.getenv("APIaquacacheName"),
      host = Sys.getenv("APIaquacacheHost"),
      port = Sys.getenv("APIaquacachePort"),
      silent = TRUE
    ),
    silent = TRUE
  )
}

v2_context <- function(request, response) {
  credentials <- v2_resolve_credentials(request)
  if (!is.null(credentials$error)) {
    response$status <- 401L
    response$set_header("WWW-Authenticate", 'Basic realm="AquaCache"')
    response$set_header("X-Status", "error")
    return(list(error = v2_error_df(credentials$error)))
  }

  con <- v2_open_connection(credentials)
  if (inherits(con, "try-error")) {
    response$status <- 503L
    response$set_header("X-Status", "error")
    return(list(
      error = v2_error_df("Database connection failed, check your credentials.")
    ))
  }

  list(con = con, credentials = credentials)
}

v2_request_cache_allowed <- function(credentials) {
  !isTRUE(credentials$authenticated)
}

v2_validate_lang <- function(lang) {
  lang <- tolower(as.character(lang[[1]]))

  if (!lang %in% c("en", "fr")) {
    return(NULL)
  }

  lang
}

v2_parse_datetime <- function(value) {
  out <- try(as.POSIXct(value, tz = "UTC"), silent = TRUE)

  if (inherits(out, "try-error") || is.na(out)) {
    return(NULL)
  }

  out
}

v2_parse_integer_csv <- function(value) {
  if (v2_query_missing(value)) {
    return(integer())
  }

  values <- trimws(strsplit(as.character(value[[1]]), ",", fixed = TRUE)[[1L]])
  out <- suppressWarnings(as.integer(values))
  out[!is.na(out)]
}

v2_parse_logical <- function(value, default = FALSE) {
  if (v2_query_missing(value)) {
    return(default)
  }

  value <- tolower(as.character(value[[1]]))
  if (value %in% c("true", "t", "1", "yes", "y")) {
    return(TRUE)
  }
  if (value %in% c("false", "f", "0", "no", "n")) {
    return(FALSE)
  }

  default
}

v2_lookup_query <- function(request, response, sql, empty_message) {
  ctx <- v2_context(request, response)
  if (!is.null(ctx$error)) {
    return(ctx$error)
  }
  on.exit(DBI::dbDisconnect(ctx$con), add = TRUE)

  out <- DBI::dbGetQuery(ctx$con, sql)

  if (nrow(out) == 0L) {
    response$set_header("X-Status", "info")
    return(v2_error_df(empty_message, status = "info"))
  }

  out
}

v2_cache_missing <- new.env(parent = emptyenv())

v2_cache_dir <- function() {
  path <- Sys.getenv("YGWATER_API_V2_CACHE_DIR", unset = NA_character_)
  if (is.na(path) || !nzchar(path)) {
    path <- file.path(tools::R_user_dir("YGwater", "cache"), "api-v2")
  }

  normalizePath(path, winslash = "/", mustWork = FALSE)
}

v2_cache_key <- function(prefix, ...) {
  values <- list(...)
  values <- vapply(
    values,
    function(value) {
      if (is.null(value) || length(value) == 0L) {
        return("NULL")
      }
      paste(as.character(value), collapse = ",")
    },
    character(1L)
  )

  key <- paste(c(prefix, values), collapse = "|")
  key <- gsub("[^A-Za-z0-9_.=-]+", "_", key)
  key <- gsub("^_+|_+$", "", key)

  if (nchar(key, type = "bytes") <= 180L) {
    return(key)
  }

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  writeLines(key, tmp, useBytes = TRUE)
  paste0(prefix, "_", unname(tools::md5sum(tmp)))
}

v2_cache_paths <- function(key) {
  cache_dir <- v2_cache_dir()
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  safe_key <- v2_cache_key("cache", key)
  cache_file <- file.path(cache_dir, paste0(safe_key, ".rds"))
  list(
    cache_file = cache_file,
    lock_dir = paste0(cache_file, ".lock")
  )
}

v2_cache_read <- function(cache_file) {
  if (!file.exists(cache_file)) {
    return(v2_cache_missing)
  }

  out <- try(readRDS(cache_file), silent = TRUE)
  if (inherits(out, "try-error")) {
    unlink(cache_file, force = TRUE)
    return(v2_cache_missing)
  }

  out
}

v2_cache_numeric_env <- function(name, default) {
  out <- suppressWarnings(as.numeric(Sys.getenv(name, default)))
  if (length(out) == 0L || is.na(out) || out <= 0) {
    return(default)
  }
  out[[1L]]
}

v2_cache_get_or_compute <- function(key, compute) {
  paths <- v2_cache_paths(key)
  wait_seconds <- v2_cache_numeric_env("YGWATER_API_V2_CACHE_WAIT", 900)
  stale_seconds <- v2_cache_numeric_env("YGWATER_API_V2_CACHE_STALE", 1800)
  started <- Sys.time()

  repeat {
    cached <- v2_cache_read(paths$cache_file)
    if (!identical(cached, v2_cache_missing)) {
      return(cached)
    }

    if (dir.create(paths$lock_dir, showWarnings = FALSE)) {
      on.exit(unlink(paths$lock_dir, recursive = TRUE, force = TRUE), add = TRUE)

      cached <- v2_cache_read(paths$cache_file)
      if (!identical(cached, v2_cache_missing)) {
        return(cached)
      }

      out <- compute()
      tmp <- tempfile(tmpdir = dirname(paths$cache_file), fileext = ".rds")
      on.exit(unlink(tmp, force = TRUE), add = TRUE)
      saveRDS(out, tmp)
      if (!file.rename(tmp, paths$cache_file)) {
        file.copy(tmp, paths$cache_file, overwrite = TRUE)
        unlink(tmp, force = TRUE)
      }
      return(out)
    }

    lock_info <- file.info(paths$lock_dir)
    if (
      !is.na(lock_info$mtime) &&
        difftime(Sys.time(), lock_info$mtime, units = "secs") > stale_seconds
    ) {
      unlink(paths$lock_dir, recursive = TRUE, force = TRUE)
      next
    }

    if (difftime(Sys.time(), started, units = "secs") > wait_seconds) {
      stop(
        "Timed out waiting for an in-flight cached API result to finish.",
        call. = FALSE
      )
    }

    Sys.sleep(0.25)
  }
}

v2_snowbull_leaflet_cached <- function(
  stamp,
  year = NULL,
  month = NULL,
  statistic = "relative_to_med",
  language = "English",
  param_name = "snow water equivalent",
  con = NULL
) {
  key <- v2_cache_key(
    "snowbull_leaflet",
    stamp,
    year,
    month,
    statistic,
    language,
    param_name
  )

  v2_cache_get_or_compute(
    key,
    function() {
      YGwater:::create_snowbull_leaflet_html(
        year = year,
        month = month,
        param_name = param_name,
        statistic = statistic,
        language = language,
        con = con
      )
    }
  )
}

v2_snow_info_cached <- function(
  stamp,
  con = NULL,
  complete_yrs = FALSE,
  stats = FALSE
) {
  key <- v2_cache_key("snow_info", stamp, complete_yrs, stats)

  v2_cache_get_or_compute(
    key,
    function() {
      YGwater::snowInfo(
        con = con,
        complete_yrs = complete_yrs,
        inactive = TRUE,
        plots = FALSE,
        quiet = TRUE,
        stats = stats,
        save_path = NULL,
        headers = "object"
      )
    }
  )
}

v2_snowbull_stamp <- function(
  con,
  year = NULL,
  month = NULL,
  continuous = FALSE,
  discrete = TRUE
) {
  param_id <- DBI::dbGetQuery(
    con,
    "SELECT parameter_id FROM public.parameters WHERE param_name = 'snow water equivalent'"
  )[1L, 1L]

  if (is.na(param_id) || is.null(year) || is.null(month)) {
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
      WHERE t.parameter_id = %s
        AND DATE(m.date) <= CAST('%s' AS date)
        AND DATE(m.date) >= CAST('1990-10-01' AS date)
      ",
        as.integer(param_id),
        as.character(as.Date(sprintf("%04d-%02d-%02d", year, month, 1L)))
      )
    )[1L, 1L]
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
        AND r.result IS NOT NULL
        AND DATE(s.target_datetime) < DATE('%s')
        AND DATE(s.target_datetime) >= DATE('1990-10-01')
      ",
        as.integer(param_id),
        as.character(as.Date(sprintf("%04d-%02d-%02d", year, month, 1L)))
      )
    )[1L, 1L]
  } else {
    discrete_stamp <- NA
  }

  paste(
    c(
      if (!is.na(continuous_stamp)) as.character(continuous_stamp) else "none",
      if (!is.na(discrete_stamp)) as.character(discrete_stamp) else "none"
    ),
    collapse = "|"
  )
}

v2_snow_stamp <- function(con) {
  rows <- DBI::dbGetQuery(
    con,
    "SELECT count(*) FROM discrete.samples WHERE import_source = 'downloadSnowCourse'"
  )[1L, 1L]

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
  )[1L, 1L]

  if (is.na(stamp) || is.na(rows)) {
    "none"
  } else {
    as.character(paste0(stamp, " ", rows))
  }
}

v2_snow_info_endpoint <- function(
  request,
  response,
  output,
  complete_yrs = FALSE,
  stats = FALSE
) {
  ctx <- v2_context(request, response)
  if (!is.null(ctx$error)) {
    return(ctx$error)
  }
  on.exit(DBI::dbDisconnect(ctx$con), add = TRUE)

  latest <- v2_snow_stamp(ctx$con)

  if (v2_request_cache_allowed(ctx$credentials)) {
    out <- v2_snow_info_cached(
      stamp = latest,
      con = ctx$con,
      complete_yrs = complete_yrs,
      stats = stats
    )
  } else {
    response$set_header("Cache-Control", "no-store")
    out <- YGwater::snowInfo(
      con = ctx$con,
      complete_yrs = complete_yrs,
      inactive = TRUE,
      plots = FALSE,
      quiet = TRUE,
      stats = stats,
      save_path = NULL,
      headers = "object"
    )
  }

  v2_csv_with_header(
    out[[output]],
    header_lines = out$headers[[output]][[1L]]
  )
}

#* List available locations
#* @get /v2/locations
#* @query lang:string("en") Language for location names and descriptions ("en" or "fr").
#* @serializer text/csv v2_csv_serializer()
#* @async
function(request, response, query) {
  lang <- v2_validate_lang(v2_query_value(query, "lang", "en"))
  if (is.null(lang)) {
    response$status <- 400L
    response$set_header("X-Status", "error")
    return(v2_error_df("Invalid language parameter. Use 'en' or 'fr'."))
  }

  ctx <- v2_context(request, response)
  if (!is.null(ctx$error)) {
    return(ctx$error)
  }
  on.exit(DBI::dbDisconnect(ctx$con), add = TRUE)

  sql <- if (lang == "en") {
    "SELECT * FROM public.location_metadata_en ORDER BY location_id"
  } else {
    "SELECT * FROM public.location_metadata_fr ORDER BY location_id"
  }

  out <- DBI::dbGetQuery(ctx$con, sql)

  if (nrow(out) == 0L) {
    response$set_header("X-Status", "info")
    return(v2_error_df("No locations found in the database.", status = "info"))
  }

  out
}

#* List available timeseries
#* @get /v2/timeseries
#* @query lang:string("en") Language for timeseries names and descriptions ("en" or "fr").
#* @serializer text/csv v2_csv_serializer()
#* @async
function(request, response, query) {
  lang <- v2_validate_lang(v2_query_value(query, "lang", "en"))
  if (is.null(lang)) {
    response$status <- 400L
    response$set_header("X-Status", "error")
    return(v2_error_df("Invalid language parameter. Use 'en' or 'fr'."))
  }

  ctx <- v2_context(request, response)
  if (!is.null(ctx$error)) {
    return(ctx$error)
  }
  on.exit(DBI::dbDisconnect(ctx$con), add = TRUE)

  visibility_sql <- if (v2_request_cache_allowed(ctx$credentials)) {
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
  } else {
    "continuous.timeseries_metadata_fr"
  }

  sql <- sprintf(
    "SELECT
       tm.*,
       ts.publicly_visible,
       ts.active,
       ts.source_fx,
       ts.source_fx_args::text AS source_fx_args,
       ts.share_with,
       ts.default_owner AS default_owner_organization_id,
       org.name AS default_owner,
       org.name_fr AS default_owner_fr,
       ts.default_data_sharing_agreement_id,
       ts.private_expiry,
       ts.sync_remote,
       ts.timezone_daily_calc,
       ts.last_daily_calculation,
       ts.last_synchronize,
       ts.matrix_state_id,
       ms.matrix_state_code,
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

  out <- DBI::dbGetQuery(ctx$con, sql)

  if (nrow(out) == 0L) {
    response$set_header("X-Status", "info")
    return(v2_error_df("No timeseries found in the database.", status = "info"))
  }

  out
}

#* Return measurements for a timeseries
#* @get /v2/timeseries/measurements
#* @query id:string* Timeseries IDs to target, separated by commas.
#* @query start:string* Start date/time, inclusive, in ISO 8601 format.
#* @query end:string End date/time, inclusive, in ISO 8601 format.
#* @query limit:integer(100000) Maximum number of records to return.
#* @serializer text/csv v2_csv_serializer()
#* @async
function(request, response, query) {
  id <- v2_query_value(query, "id")
  if (is.null(id)) {
    response$status <- 400L
    response$set_header("X-Status", "error")
    return(v2_error_df("Missing required 'id' parameter."))
  }

  start <- v2_query_value(query, "start")
  if (is.null(start)) {
    response$status <- 400L
    response$set_header("X-Status", "error")
    return(v2_error_df("Missing required 'start' parameter."))
  }

  start <- v2_parse_datetime(start)
  if (is.null(start)) {
    response$status <- 400L
    response$set_header("X-Status", "error")
    return(v2_error_df(
      "Invalid 'start' parameter. Must be in ISO 8601 format."
    ))
  }

  end <- v2_query_value(query, "end", Sys.time())
  end <- v2_parse_datetime(end)
  if (is.null(end)) {
    response$status <- 400L
    response$set_header("X-Status", "error")
    return(v2_error_df(
      "Invalid 'end' parameter. Must be in ISO 8601 format."
    ))
  }

  lim <- suppressWarnings(as.integer(v2_query_value(query, "limit", "100000")))
  if (is.na(lim) || lim <= 0L) {
    lim <- 100000L
  }

  ids <- v2_parse_integer_csv(id)
  if (length(ids) == 0L) {
    response$status <- 400L
    response$set_header("X-Status", "error")
    return(v2_error_df(
      "Invalid 'id' parameter. Must contain at least one integer timeseries_id."
    ))
  }

  ctx <- v2_context(request, response)
  if (!is.null(ctx$error)) {
    return(ctx$error)
  }
  on.exit(DBI::dbDisconnect(ctx$con), add = TRUE)

  include_private <- !v2_request_cache_allowed(ctx$credentials)
  measurement_join_sql <- "
    LEFT JOIN LATERAL (
      SELECT
        g.grade_type_id,
        gt.grade_type_code
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
        at.approval_type_code
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
        ) AS qualifier_type_codes
      FROM (
        SELECT DISTINCT ON (q.qualifier_type_id)
          q.qualifier_type_id,
          qt.qualifier_type_code
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
      m.*,
      grade.grade_type_id,
      grade.grade_type_code,
      approval.approval_type_id,
      approval.approval_type_code,
      qualifier.qualifier_type_ids,
      qualifier.qualifier_type_codes,
      owner_range.owner_organization_id,
      owner_org.name AS owner,
      contributor_range.contributor_organization_id,
      contributor_org.name AS contributor
  "

  if (length(ids) == 1L) {
    out <- DBI::dbGetQuery(
      ctx$con,
      paste0(
        measurement_select_sql,
        "FROM continuous.measurements_continuous_corrected($1, $2, $3) m
         JOIN continuous.timeseries ts
           ON m.timeseries_id = ts.timeseries_id",
        measurement_join_sql,
        "WHERE ($4::boolean OR ts.publicly_visible)
         ORDER BY m.datetime DESC
         LIMIT $5"
      ),
      params = list(as.integer(ids[[1L]]), start, end, include_private, lim)
    )
  } else {
    out <- DBI::dbGetQuery(
      ctx$con,
      sprintf(
        paste0(
          "WITH requested_timeseries(timeseries_id) AS (
           SELECT unnest(ARRAY[%s]::integer[])
         )
         ",
          measurement_select_sql,
          "
         FROM requested_timeseries r
         JOIN LATERAL continuous.measurements_continuous_corrected(
           r.timeseries_id,
           $1,
           $2
         ) m ON TRUE
         JOIN continuous.timeseries ts
           ON m.timeseries_id = ts.timeseries_id",
          measurement_join_sql,
          "WHERE ($3::boolean OR ts.publicly_visible)
         ORDER BY m.datetime DESC
         LIMIT $4"
        ),
        paste(ids, collapse = ",")
      ),
      params = list(start, end, include_private, lim)
    )
  }

  if (nrow(out) == 0L) {
    response$set_header("X-Status", "info")
    return(v2_error_df(
      "No measurements found for the specified timeseries and date range.",
      status = "info"
    ))
  }

  out
}

#* Return available parameters in the database
#* @get /v2/parameters
#* @serializer text/csv v2_csv_serializer()
#* @async
function(request, response, query) {
  ctx <- v2_context(request, response)
  if (!is.null(ctx$error)) {
    return(ctx$error)
  }
  on.exit(DBI::dbDisconnect(ctx$con), add = TRUE)

  unit_sql <- YGwater::ac_parameter_unit_select_sql(ctx$con, "p", "units")
  sql <- sprintf(
    "SELECT
       p.parameter_id,
       p.param_name,
       p.param_name_fr,
       p.description,
       p.description_fr,
       %s
     FROM public.parameters p
     ORDER BY p.parameter_id",
    unit_sql
  )

  out <- DBI::dbGetQuery(ctx$con, sql)

  if (nrow(out) == 0L) {
    response$set_header("X-Status", "info")
    return(v2_error_df("No parameters found in the database.", status = "info"))
  }

  out
}

#* Return grade types in the database
#* @get /v2/grades
#* @serializer text/csv v2_csv_serializer()
#* @async
function(request, response, query) {
  v2_lookup_query(
    request,
    response,
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
}

#* Return approval types in the database
#* @get /v2/approvals
#* @serializer text/csv v2_csv_serializer()
#* @async
function(request, response, query) {
  v2_lookup_query(
    request,
    response,
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
}

#* Return qualifier types in the database
#* @get /v2/qualifiers
#* @serializer text/csv v2_csv_serializer()
#* @async
function(request, response, query) {
  v2_lookup_query(
    request,
    response,
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
}

#* Return organizations in the database
#* @get /v2/organizations
#* @serializer text/csv v2_csv_serializer()
#* @async
function(request, response, query) {
  v2_lookup_query(
    request,
    response,
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
}

#* Return sample metadata
#* @get /v2/samples
#* @query start:string* Start date/time, inclusive, in ISO 8601 format.
#* @query end:string End date/time, inclusive, in ISO 8601 format.
#* @query locations:string Location IDs to target, separated by commas.
#* @query parameters:string Parameter IDs to target, separated by commas.
#* @serializer text/csv v2_csv_serializer()
#* @async
function(request, response, query) {
  start <- v2_query_value(query, "start")
  if (is.null(start)) {
    response$status <- 400L
    response$set_header("X-Status", "error")
    return(v2_error_df("Missing required 'start' parameter."))
  }

  start <- v2_parse_datetime(start)
  if (is.null(start)) {
    response$status <- 400L
    response$set_header("X-Status", "error")
    return(v2_error_df(
      "Invalid 'start' parameter. Must be in ISO 8601 format."
    ))
  }

  end <- v2_query_value(query, "end", Sys.time())
  end <- v2_parse_datetime(end)
  if (is.null(end)) {
    response$status <- 400L
    response$set_header("X-Status", "error")
    return(v2_error_df(
      "Invalid 'end' parameter. Must be in ISO 8601 format."
    ))
  }

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

  locations <- v2_query_value(query, "locations")
  if (!is.null(locations)) {
    location_ids <- v2_parse_integer_csv(locations)
    if (length(location_ids) == 0L) {
      response$status <- 400L
      response$set_header("X-Status", "error")
      return(v2_error_df(
        "Invalid 'locations' parameter. Must contain integer location_id values."
      ))
    }
    sql <- paste0(
      sql,
      " AND location_id IN (",
      paste(location_ids, collapse = ","),
      ")"
    )
  }

  parameters <- v2_query_value(query, "parameters")
  if (!is.null(parameters)) {
    parameter_ids <- v2_parse_integer_csv(parameters)
    if (length(parameter_ids) == 0L) {
      response$status <- 400L
      response$set_header("X-Status", "error")
      return(v2_error_df(
        "Invalid 'parameters' parameter. Must contain integer parameter_id values."
      ))
    }
    sql <- paste0(
      sql,
      " AND sample_id IN (
          SELECT DISTINCT sample_id
          FROM discrete.results
          WHERE parameter_id IN (",
      paste(parameter_ids, collapse = ","),
      "))"
    )
  }
  sql <- paste0(sql, " ORDER BY datetime DESC")

  ctx <- v2_context(request, response)
  if (!is.null(ctx$error)) {
    return(ctx$error)
  }
  on.exit(DBI::dbDisconnect(ctx$con), add = TRUE)

  out <- DBI::dbGetQuery(ctx$con, sql, params = list(start, end))

  if (nrow(out) == 0L) {
    response$set_header("X-Status", "info")
    return(v2_error_df(
      "No samples found for the specified criteria.",
      status = "info"
    ))
  }

  out
}

#* Return sample results
#* @get /v2/samples/results
#* @query sample_ids:string* Sample IDs to target, separated by commas.
#* @query parameters:string Parameter IDs to target, separated by commas.
#* @serializer text/csv v2_csv_serializer()
#* @async
function(request, response, query) {
  sample_ids <- v2_query_value(query, "sample_ids")
  if (is.null(sample_ids)) {
    response$status <- 400L
    response$set_header("X-Status", "error")
    return(v2_error_df("Missing required 'sample_ids' parameter."))
  }

  sample_ids <- v2_parse_integer_csv(sample_ids)
  if (length(sample_ids) == 0L) {
    response$status <- 400L
    response$set_header("X-Status", "error")
    return(v2_error_df(
      "Invalid 'sample_ids' parameter. Must contain integer sample_id values."
    ))
  }

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
    paste(sample_ids, collapse = ","),
    ")"
  )

  parameters <- v2_query_value(query, "parameters")
  if (!is.null(parameters)) {
    parameter_ids <- v2_parse_integer_csv(parameters)
    if (length(parameter_ids) == 0L) {
      response$status <- 400L
      response$set_header("X-Status", "error")
      return(v2_error_df(
        "Invalid 'parameters' parameter. Must contain integer parameter_id values."
      ))
    }
    sql <- paste0(
      sql,
      " AND r.parameter_id IN (",
      paste(parameter_ids, collapse = ","),
      ")"
    )
  }
  sql <- paste0(sql, " ORDER BY r.parameter_id")

  ctx <- v2_context(request, response)
  if (!is.null(ctx$error)) {
    return(ctx$error)
  }
  on.exit(DBI::dbDisconnect(ctx$con), add = TRUE)

  out <- DBI::dbGetQuery(ctx$con, sql)

  if (nrow(out) == 0L) {
    response$set_header("X-Status", "info")
    return(v2_error_df(
      "No results found for the specified sample ID and parameters.",
      status = "info"
    ))
  }

  out
}

#* Return SWE snow bulletin leaflet map HTML
#* @get /v2/snow-bulletin/leaflet
#* @query year:integer Bulletin year.
#* @query month:integer Bulletin month.
#* @query statistic:string("relative_to_med") Statistic to display.
#* @query language:string("English") Language for labels.
#* @query continuous:boolean(false) Consider continuous data for latest bulletin.
#* @query discrete:boolean(true) Consider discrete data for latest bulletin.
#* @serializer text/html v2_text_serializer()
#* @async
function(request, response, query) {
  ctx <- v2_context(request, response)
  if (!is.null(ctx$error)) {
    response$status <- 503L
    return("<p>Database connection failed, check your credentials.</p>")
  }
  on.exit(DBI::dbDisconnect(ctx$con), add = TRUE)

  year_value <- v2_query_value(query, "year")
  month_value <- v2_query_value(query, "month")
  year_given <- !is.null(year_value)
  month_given <- !is.null(month_value)

  if (xor(year_given, month_given)) {
    response$status <- 400L
    return(
      "<p>Both 'year' and 'month' must be provided together or not at all.</p>"
    )
  }

  if (year_given) {
    year <- suppressWarnings(as.integer(year_value))
    if (is.na(year)) {
      response$status <- 400L
      return("<p>Invalid 'year' parameter.</p>")
    }
  } else {
    continuous <- v2_parse_logical(v2_query_value(query, "continuous"), FALSE)
    discrete <- v2_parse_logical(v2_query_value(query, "discrete"), TRUE)

    if (discrete) {
      year_disc <- DBI::dbGetQuery(
        ctx$con,
        "
      SELECT MAX(EXTRACT(YEAR FROM s.target_datetime)) AS latest_year
      FROM discrete.samples s
      JOIN discrete.results r ON s.sample_id = r.sample_id
      WHERE r.parameter_id = (
          SELECT parameter_id
          FROM public.parameters
          WHERE param_name = 'snow water equivalent'
        )
        AND r.result IS NOT NULL
        AND DATE(s.target_datetime) >= DATE('1990-10-01')"
      )[1L, 1L]
    } else {
      year_disc <- NA
    }

    if (continuous) {
      year_cont <- DBI::dbGetQuery(
        ctx$con,
        "
      SELECT MAX(EXTRACT(YEAR FROM m.date)) AS latest_year
      FROM continuous.measurements_calculated_daily_corrected m
      JOIN continuous.timeseries t ON m.timeseries_id = t.timeseries_id
      WHERE t.parameter_id = (
          SELECT parameter_id
          FROM public.parameters
          WHERE param_name = 'snow water equivalent'
        )
        AND DATE(m.date) >= DATE('1990-10-01')"
      )[1L, 1L]
    } else {
      year_cont <- NA
    }

    if (all(is.na(c(year_disc, year_cont)))) {
      year <- NULL
    } else {
      year <- max(c(year_disc, year_cont), na.rm = TRUE)
    }
  }

  if (month_given) {
    month <- suppressWarnings(as.integer(month_value))
    if (is.na(month) || month < 1L || month > 12L) {
      response$status <- 400L
      return("<p>Invalid 'month' parameter. Use 1-12.</p>")
    }
  } else if (!is.null(year)) {
    continuous <- v2_parse_logical(v2_query_value(query, "continuous"), FALSE)
    discrete <- v2_parse_logical(v2_query_value(query, "discrete"), TRUE)

    if (discrete) {
      month_disc <- DBI::dbGetQuery(
        ctx$con,
        "
      SELECT MAX(EXTRACT(MONTH FROM s.target_datetime)) AS latest_month
      FROM discrete.samples s
      JOIN discrete.results r ON s.sample_id = r.sample_id
      WHERE r.parameter_id = (
          SELECT parameter_id
          FROM public.parameters
          WHERE param_name = 'snow water equivalent'
        )
        AND r.result IS NOT NULL
        AND DATE(s.target_datetime) >= DATE('1990-10-01')
        AND EXTRACT(YEAR FROM s.target_datetime) = $1",
        params = list(year)
      )[1L, 1L]
    } else {
      month_disc <- NA
    }

    if (continuous) {
      month_cont <- DBI::dbGetQuery(
        ctx$con,
        "
      SELECT MAX(EXTRACT(MONTH FROM date)) AS latest_month
      FROM continuous.measurements_calculated_daily_corrected m
      JOIN continuous.timeseries t ON m.timeseries_id = t.timeseries_id
      WHERE t.parameter_id = (
          SELECT parameter_id
          FROM public.parameters
          WHERE param_name = 'snow water equivalent'
        )
        AND DATE(date) >= DATE('1990-10-01')
        AND EXTRACT(YEAR FROM date) = $1",
        params = list(year)
      )[1L, 1L]
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

  statistic <- v2_query_value(query, "statistic", "relative_to_med")
  language <- v2_query_value(query, "language", "English")
  continuous <- v2_parse_logical(v2_query_value(query, "continuous"), FALSE)
  discrete <- v2_parse_logical(v2_query_value(query, "discrete"), TRUE)
  latest_stamp <- v2_snowbull_stamp(ctx$con, year, month, continuous, discrete)

  if (v2_request_cache_allowed(ctx$credentials)) {
    map_payload <- v2_snowbull_leaflet_cached(
      stamp = latest_stamp,
      year = year,
      month = month,
      statistic = statistic,
      language = language,
      param_name = "snow water equivalent",
      con = ctx$con
    )
  } else {
    response$set_header("Cache-Control", "no-store")
    map_payload <- YGwater:::create_snowbull_leaflet_html(
      year = year,
      month = month,
      param_name = "snow water equivalent",
      statistic = statistic,
      language = language,
      con = ctx$con
    )
  }

  response$set_header("X-Map-Year", as.character(map_payload$year))
  response$set_header("X-Map-Month", as.character(map_payload$month))

  map_payload$html
}

#* Return basic snow survey data
#* @get /v2/snow-survey/data
#* @serializer text/csv v2_text_serializer()
#* @async
function(request, response, query) {
  v2_snow_info_endpoint(
    request,
    response,
    output = "measurements",
    complete_yrs = FALSE,
    stats = FALSE
  )
}

#* Return basic snow survey metadata
#* @get /v2/snow-survey/metadata
#* @serializer text/csv v2_text_serializer()
#* @async
function(request, response, query) {
  v2_snow_info_endpoint(
    request,
    response,
    output = "locations",
    complete_yrs = FALSE,
    stats = FALSE
  )
}

#* Return snow survey statistics
#* @get /v2/snow-survey/stats
#* @serializer text/csv v2_text_serializer()
#* @async
function(request, response, query) {
  v2_snow_info_endpoint(
    request,
    response,
    output = "stats",
    complete_yrs = TRUE,
    stats = TRUE
  )
}

#* Return basic snow survey trends
#* @get /v2/snow-survey/trends
#* @serializer text/csv v2_text_serializer()
#* @async
function(request, response, query) {
  v2_snow_info_endpoint(
    request,
    response,
    output = "trends",
    complete_yrs = TRUE,
    stats = TRUE
  )
}

#* Return CSW layer data
#* @get /v2/csw-layer
#* @serializer text/csv v2_csv_serializer()
#* @async
function(request, response, query) {
  v2_lookup_query(
    request,
    response,
    "SELECT * FROM public.get_csw_layer()",
    "No CSW layer data found in the database."
  )
}
