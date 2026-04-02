#* AquaCache API version 2
#*
#* API for programmatic access to AquaCache metadata using plumber2.
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

v2_query_value <- function(query, name, default = NULL) {
  value <- query[[name]]

  if (is.null(value) || length(value) == 0L || identical(value, "")) {
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
      password = Sys.getenv("APIaquacachePass", "aquacache")
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
    user = substr(decoded, 1, separator - 1),
    password = substr(decoded, separator + 1, nchar(decoded))
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

v2_validate_lang <- function(lang) {
  lang <- tolower(lang)

  if (!lang %in% c("en", "fr")) {
    return(NULL)
  }

  lang
}

#* List available locations
#* @get /v2/locations
#* @query lang:string("en") Language for location names and descriptions ("en" or "fr").
#* @serializer text/csv v2_csv_serializer()
function(request, response, query) {
  lang <- v2_validate_lang(v2_query_value(query, "lang", "en"))
  if (is.null(lang)) {
    response$status <- 400L
    response$set_header("X-Status", "error")
    return(v2_error_df("Invalid language parameter. Use 'en' or 'fr'."))
  }

  credentials <- v2_resolve_credentials(request)
  if (!is.null(credentials$error)) {
    response$status <- 401L
    response$set_header("WWW-Authenticate", 'Basic realm="AquaCache"')
    response$set_header("X-Status", "error")
    return(v2_error_df(credentials$error))
  }

  con <- v2_open_connection(credentials)
  if (inherits(con, "try-error")) {
    response$status <- 503L
    return(v2_error_df("Database connection failed, check your credentials."))
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  sql <- if (lang == "en") {
    "SELECT * FROM public.location_metadata_en ORDER BY location_id"
  } else {
    "SELECT * FROM public.location_metadata_fr ORDER BY location_id"
  }

  out <- DBI::dbGetQuery(con, sql)

  if (nrow(out) == 0) {
    response$set_header("X-Status", "info")
    return(v2_error_df("No locations found in the database.", status = "info"))
  }

  out
}

#* List available timeseries
#* @get /v2/timeseries
#* @query lang:string("en") Language for timeseries names and descriptions ("en" or "fr").
#* @serializer text/csv v2_csv_serializer()
function(request, response, query) {
  lang <- v2_validate_lang(v2_query_value(query, "lang", "en"))
  if (is.null(lang)) {
    response$status <- 400L
    response$set_header("X-Status", "error")
    return(v2_error_df("Invalid language parameter. Use 'en' or 'fr'."))
  }

  credentials <- v2_resolve_credentials(request)
  if (!is.null(credentials$error)) {
    response$status <- 401L
    response$set_header("WWW-Authenticate", 'Basic realm="AquaCache"')
    response$set_header("X-Status", "error")
    return(v2_error_df(credentials$error))
  }

  con <- v2_open_connection(credentials)
  if (inherits(con, "try-error")) {
    response$status <- 503L
    return(v2_error_df("Database connection failed, check your credentials."))
  }
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  sql <- if (lang == "en") {
    "SELECT * FROM continuous.timeseries_metadata_en ORDER BY timeseries_id"
  } else {
    "SELECT * FROM continuous.timeseries_metadata_fr ORDER BY timeseries_id"
  }

  out <- DBI::dbGetQuery(con, sql)

  if (nrow(out) == 0) {
    response$set_header("X-Status", "info")
    return(v2_error_df("No timeseries found in the database.", status = "info"))
  }

  out
}
