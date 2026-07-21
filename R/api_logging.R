api_log_requests_enabled <- function() {
  value <- tolower(Sys.getenv("APIaquacacheLogRequests", "TRUE"))
  !value %in% c("false", "f", "0", "no", "n", "off")
}

api_log_connect <- function() {
  user <- Sys.getenv("APIaquacachePublicUser", Sys.getenv("APIaquacacheUser"))
  pass <- Sys.getenv("APIaquacachePublicPass", Sys.getenv("APIaquacachePass"))

  if (!nzchar(user) || !nzchar(pass)) {
    return(NULL)
  }

  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = Sys.getenv("APIaquacacheName"),
    host = Sys.getenv("APIaquacacheHost"),
    port = Sys.getenv("APIaquacachePort"),
    user = user,
    password = pass
  )
}

api_log_request_value <- function(request, names) {
  for (name in names) {
    value <- tryCatch(request[[name]], error = function(e) NULL)
    if (is.function(value)) {
      next
    }
    if (!is.null(value) && length(value) > 0L && !is.na(value[[1L]])) {
      value <- as.character(value[[1L]])
      if (nzchar(value)) {
        return(value)
      }
    }
  }

  NULL
}

api_log_request_header <- function(request, header) {
  get_header <- tryCatch(request$get_header, error = function(e) NULL)
  if (is.function(get_header)) {
    value <- tryCatch(get_header(header), error = function(e) NULL)
    if (!is.null(value) && length(value) > 0L && nzchar(value[[1L]])) {
      return(as.character(value[[1L]]))
    }
  }

  rook_name <- paste0("HTTP_", toupper(gsub("-", "_", header)))
  api_log_request_value(request, rook_name)
}

api_log_auth_user <- function(request) {
  user <- api_log_request_value(request, "user")
  if (!is.null(user)) {
    return(user)
  }

  hdr <- api_log_request_header(request, "Authorization")
  if (is.null(hdr) || !grepl("^Basic\\s+", hdr)) {
    return(Sys.getenv("APIaquacachePublicUser", "public_reader"))
  }

  decoded <- try(
    rawToChar(jsonlite::base64_dec(sub("^Basic\\s+", "", hdr))),
    silent = TRUE
  )
  if (inherits(decoded, "try-error")) {
    return(NULL)
  }

  separator <- regexpr(":", decoded, fixed = TRUE)
  if (separator < 1L) {
    return(NULL)
  }

  substr(decoded, 1L, separator - 1L)
}

api_log_user_ip <- function(request) {
  forwarded <- api_log_request_header(request, "X-Forwarded-For")
  if (!is.null(forwarded)) {
    forwarded <- trimws(strsplit(forwarded, ",", fixed = TRUE)[[1L]][[1L]])
    if (nzchar(forwarded)) {
      return(forwarded)
    }
  }

  api_log_request_value(request, c("REMOTE_ADDR", "remote_addr"))
}

api_log_query_string <- function(request) {
  query <- api_log_request_value(request, "QUERY_STRING")
  if (!is.null(query)) {
    return(query)
  }

  query <- tryCatch(request$query, error = function(e) NULL)
  if (is.null(query) || length(query) == 0L) {
    return(NULL)
  }

  if (is.character(query) && length(query) == 1L) {
    return(query)
  }

  paste(
    paste0(
      utils::URLencode(names(query), reserved = TRUE),
      "=",
      utils::URLencode(as.character(query), reserved = TRUE)
    ),
    collapse = "&"
  )
}

api_log_parameters_json <- function(request) {
  query <- api_log_query_string(request)
  if (is.null(query) || !nzchar(query)) {
    return(NULL)
  }

  parsed <- utils::URLdecode(strsplit(query, "&", fixed = TRUE)[[1L]])
  parsed <- stats::setNames(
    sub("^[^=]*=?", "", parsed),
    sub("=.*$", "", parsed)
  )

  jsonlite::toJSON(
    list(query = as.list(parsed), raw_query = query),
    auto_unbox = TRUE,
    null = "null"
  )
}

api_log_request_endpoint <- function(request, api_version) {
  method <- toupper(
    api_log_request_value(request, c("REQUEST_METHOD", "method")) %||% ""
  )
  path <- api_log_request_path(request) %||% ""

  trimws(sprintf("v%s %s %s", api_version, method, path))
}

api_log_request_path <- function(request) {
  api_log_request_value(request, c("PATH_INFO", "path"))
}

api_log_request_should_log <- function(request) {
  path <- api_log_request_path(request)
  if (is.null(path)) {
    return(TRUE)
  }

  path <- sub("/+$", "", path)
  if (path %in% c("/openapi.json", "/openapi.yaml")) {
    return(FALSE)
  }

  if (grepl("^/__docs__/.+", path)) {
    return(FALSE)
  }

  TRUE
}

api_log_quote_nullable <- function(con, value, cast = NULL) {
  if (is.null(value) || length(value) == 0L || is.na(value[[1L]])) {
    return("NULL")
  }

  value <- as.character(value[[1L]])
  if (!nzchar(value)) {
    return("NULL")
  }

  out <- as.character(DBI::dbQuoteLiteral(con, value))
  if (!is.null(cast)) {
    out <- paste0(out, "::", cast)
  }

  out
}

api_log_insert_sql <- function(con, request, api_version, include_id = FALSE) {
  endpoint <- api_log_request_endpoint(request, api_version)
  parameters <- api_log_parameters_json(request)
  user_id <- api_log_auth_user(request)
  user_ip <- api_log_user_ip(request)

  columns <- c(
    if (include_id) "id",
    "endpoint",
    "parameters",
    "user_id",
    "user_ip"
  )
  values <- c(
    if (include_id) "(SELECT COALESCE(MAX(id), 0) + 1 FROM application.api_requests)",
    api_log_quote_nullable(con, endpoint),
    api_log_quote_nullable(con, parameters, cast = "jsonb"),
    api_log_quote_nullable(con, user_id),
    api_log_quote_nullable(con, user_ip, cast = "inet")
  )

  sprintf(
    "INSERT INTO application.api_requests (%s) VALUES (%s) RETURNING id",
    paste(columns, collapse = ", "),
    paste(values, collapse = ", ")
  )
}

api_log_request_start <- function(request, api_version) {
  if (!api_log_requests_enabled()) {
    return(NULL)
  }

  if (!api_log_request_should_log(request)) {
    return(NULL)
  }

  tryCatch(
    {
      con <- api_log_connect()
      if (is.null(con)) {
        return(NULL)
      }
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      started <- proc.time()[["elapsed"]]
      inserted <- try(DBI::dbGetQuery(
        con,
        api_log_insert_sql(con, request, api_version)
      ), silent = TRUE)

      if (inherits(inserted, "try-error")) {
        DBI::dbBegin(con)
        ok <- FALSE
        on.exit(if (!ok) try(DBI::dbRollback(con), silent = TRUE), add = TRUE)

        DBI::dbGetQuery(
          con,
          "SELECT pg_advisory_xact_lock(hashtext('application.api_requests'))"
        )
        inserted <- DBI::dbGetQuery(
          con,
          api_log_insert_sql(con, request, api_version, include_id = TRUE)
        )
        DBI::dbCommit(con)
        ok <- TRUE
      }

      list(
        id = inserted$id[[1L]],
        started = started
      )
    },
    error = function(e) NULL
  )
}

api_log_status_code <- function(response) {
  status <- api_log_request_value(response, c("status", "status_code"))
  status <- suppressWarnings(as.integer(status))
  if (length(status) != 1L || is.na(status)) {
    status <- 200L
  }

  status
}

api_log_response_time_writable_cache <- new.env(parent = emptyenv())

api_log_response_time_writable <- function(con) {
  key <- paste(
    Sys.getenv("APIaquacacheHost"),
    Sys.getenv("APIaquacachePort"),
    Sys.getenv("APIaquacacheName"),
    sep = ":"
  )

  if (exists(key, envir = api_log_response_time_writable_cache, inherits = FALSE)) {
    return(api_log_response_time_writable_cache[[key]])
  }

  writable <- tryCatch(
    {
      generated <- DBI::dbGetQuery(con, "
        SELECT is_generated
        FROM information_schema.columns
        WHERE table_schema = 'application'
          AND table_name = 'api_requests'
          AND column_name = 'response_time_ms'
      ")

      !identical(generated$is_generated[[1L]], "ALWAYS")
    },
    error = function(e) TRUE
  )

  api_log_response_time_writable_cache[[key]] <- writable
  writable
}

api_log_request_end <- function(log, response) {
  if (is.null(log) || is.null(log$id)) {
    return(invisible(FALSE))
  }

  tryCatch(
    {
      con <- api_log_connect()
      if (is.null(con)) {
        return(invisible(FALSE))
      }
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      status_code <- api_log_status_code(response)
      response_time_ms <- as.integer(round(
        (proc.time()[["elapsed"]] - log$started) * 1000
      ))
      success <- !is.na(status_code) && status_code < 400L

      response_time_sql <- if (api_log_response_time_writable(con)) {
        sprintf("    response_time_ms = %d", max(response_time_ms, 0L))
      } else {
        NULL
      }

      DBI::dbExecute(
        con,
        sprintf(
          paste(
            "UPDATE application.api_requests",
            "SET session_end = now(),",
            "    status_code = %d,",
            "    success = %s%s",
            "WHERE id = %d"
          ),
          status_code,
          if (success) "TRUE" else "FALSE",
          if (!is.null(response_time_sql)) {
            paste0(",", "\n", response_time_sql)
          } else {
            ""
          },
          as.integer(log$id)
        )
      )
    },
    error = function(e) NULL
  )

  invisible(TRUE)
}

api_configure_v1_logging <- function(pr, api_version) {
  logs <- new.env(parent = emptyenv())
  logs$pending <- list()

  pr$registerHook("preroute", function(req, res) {
    logs$pending[[length(logs$pending) + 1L]] <- api_log_request_start(
      req,
      api_version
    )
    NULL
  })

  pr$registerHook("postserialize", function(req, res, value) {
    if (length(logs$pending) > 0L) {
      api_log_request_end(logs$pending[[1L]], res)
      logs$pending <- logs$pending[-1L]
    }
    value
  })

  pr
}

api_configure_v2_logging <- function(pr, api_version) {
  logs <- new.env(parent = emptyenv())

  pr$on("before-request", function(id, request, ...) {
    logs[[id]] <- api_log_request_start(request, api_version)
    NULL
  }, id = "YGwater_api_request_log_start")

  pr$on("after-request", function(id, response, ...) {
    if (exists(id, envir = logs, inherits = FALSE)) {
      api_log_request_end(logs[[id]], response)
      rm(list = id, envir = logs)
    }
    NULL
  }, id = "YGwater_api_request_log_end")

  pr
}
