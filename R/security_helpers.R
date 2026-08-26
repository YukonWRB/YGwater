#' Escape untrusted text for inclusion in generated HTML
#'
#' @param x A value to render as text.
#'
#' @return A character vector with HTML metacharacters escaped.
#' @noRd
escape_html_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  as.character(htmltools::htmlEscape(x))
}

#' Validate a stored numeric SQL expression
#'
#' Stored correction and compound-timeseries expressions are inserted into
#' database-generated SQL. This validator limits them to arithmetic, approved
#' scalar functions, declared identifiers, and declared parameter placeholders.
#'
#' @param expression A scalar SQL expression.
#' @param allowed_identifiers Identifiers that may be referenced by the expression.
#' @param allowed_placeholders Integer parameter numbers that may be referenced.
#' @param label User-facing name for error messages.
#'
#' @return The trimmed expression, invisibly.
#' @noRd
validate_numeric_sql_expression <- function(
  expression,
  allowed_identifiers = character(),
  allowed_placeholders = integer(),
  label = "SQL expression"
) {
  if (
    length(expression) != 1L ||
      is.na(expression) ||
      !nzchar(trimws(expression))
  ) {
    stop(label, " must be a non-empty character value.", call. = FALSE)
  }

  expression <- trimws(expression)
  token_pattern <- paste0(
    "\\$[0-9]+|",
    "[A-Za-z_][A-Za-z0-9_]*|",
    "(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?|",
    "::|<=|>=|<>|!=|[-+*/%^(),<>=]"
  )
  hits <- gregexpr(token_pattern, expression, perl = TRUE)[[1]]
  tokens <- if (identical(hits[[1]], -1L)) {
    character()
  } else {
    regmatches(expression, list(hits))[[1]]
  }
  remainder <- gsub(token_pattern, "", expression, perl = TRUE)
  if (nzchar(gsub("[[:space:]]", "", remainder))) {
    stop(
      label,
      " may contain only arithmetic, comparisons, approved functions, and declared values.",
      call. = FALSE
    )
  }

  placeholders <- tokens[grepl("^\\$[0-9]+$", tokens)]
  placeholder_numbers <- as.integer(sub("^\\$", "", placeholders))
  if (length(setdiff(placeholder_numbers, allowed_placeholders))) {
    stop(label, " contains an unsupported parameter placeholder.", call. = FALSE)
  }

  identifiers <- unique(tokens[grepl("^[A-Za-z_]", tokens)])
  approved_words <- c(
    "abs", "case", "ceil", "ceiling", "coalesce", "double", "else",
    "end", "exp", "floor", "greatest", "least", "ln", "log", "null",
    "nullif", "numeric", "power", "precision", "real", "round", "sign",
    "sqrt", "then", "when"
  )
  allowed <- unique(tolower(c(approved_words, allowed_identifiers)))
  unknown <- identifiers[!tolower(identifiers) %in% allowed]
  if (length(unknown)) {
    stop(
      label,
      " contains unsupported identifier(s): ",
      paste(unknown, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  invisible(expression)
}

#' Validate an advanced guideline SQL scalar statement
#'
#' @param sql A SELECT statement that returns one numeric value.
#'
#' @return The trimmed SQL, invisibly.
#' @noRd
validate_guideline_sql_scalar <- function(sql) {
  if (length(sql) != 1L || is.na(sql) || !nzchar(trimws(sql))) {
    stop("SQL scalar text is required.", call. = FALSE)
  }

  scan <- trimws(sql)
  scan <- gsub("(?s)\\$[^$]*\\$.*?\\$[^$]*\\$", " ", scan, perl = TRUE)
  scan <- gsub("'([^'\\\\]|\\\\.)*'", " ", scan, perl = TRUE)
  scan <- gsub("--.*?(\\r?\\n|$)", " ", scan, perl = TRUE)
  scan <- gsub("/\\*.*?\\*/", " ", scan, perl = TRUE)

  if (grepl(";", scan, fixed = TRUE)) {
    stop("SQL scalar text must contain exactly one statement.", call. = FALSE)
  }
  if (!grepl("(?s)^\\s*(?:select|with\\b.*\\bselect)\\b", scan, perl = TRUE, ignore.case = TRUE)) {
    stop("SQL scalar text must begin with SELECT or WITH ... SELECT.", call. = FALSE)
  }
  if (grepl("\\$[2-9][0-9]*", scan, perl = TRUE)) {
    stop("Only $1 may be used as a SQL scalar parameter.", call. = FALSE)
  }

  forbidden <- paste0(
    "\\b(?:alter|analyze|call|checkpoint|cluster|copy|create|deallocate|delete|",
    "discard|do|drop|execute|grant|insert|listen|lock|merge|notify|prepare|",
    "refresh|reindex|reset|revoke|set|truncate|unlisten|update|vacuum)\\b|",
    "\\b(?:dblink|lo_export|lo_import|pg_cancel_backend|pg_sleep|",
    "pg_terminate_backend)\\s*\\("
  )
  if (grepl(forbidden, scan, perl = TRUE, ignore.case = TRUE)) {
    stop(
      "SQL scalar text may not modify data, database objects, sessions, or server processes.",
      call. = FALSE
    )
  }

  invisible(trimws(sql))
}
