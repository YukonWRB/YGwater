#' Validate the work requested from a public API endpoint
#'
#' Applies a common upper bound to response rows, identifier cardinality, and
#' date range before a request opens a database connection or enters an async
#' worker. Invalid or non-positive row limits retain the API's historical
#' default; explicit limits above the maximum are rejected.
#'
#' @param limit Requested maximum response rows.
#' @param id_groups Named list of parsed integer identifier vectors.
#' @param start,end Optional parsed date or date-time range.
#' @param max_rows Maximum response rows allowed.
#' @param max_ids Maximum number of values allowed in each identifier group.
#' @param max_span_days Maximum inclusive date-range span in days.
#'
#' @return A list containing `valid`, the normalized `limit`, and `message`.
#' @noRd
api_request_budget <- function(
  limit = 100000L,
  id_groups = list(),
  start = NULL,
  end = NULL,
  max_rows = 100000L,
  max_ids = 100L,
  max_span_days = 73050
) {
  limit <- suppressWarnings(as.integer(limit[[1L]]))
  if (is.na(limit) || limit <= 0L) {
    limit <- max_rows
  }

  if (limit > max_rows) {
    return(list(
      valid = FALSE,
      limit = limit,
      message = sprintf("'limit' must be at most %d.", max_rows)
    ))
  }

  if (length(id_groups) > 0L) {
    group_sizes <- lengths(id_groups)
    oversized <- which(group_sizes > max_ids)
    if (length(oversized) > 0L) {
      group_name <- names(id_groups)[oversized[[1L]]]
      if (is.null(group_name) || is.na(group_name) || !nzchar(group_name)) {
        group_name <- "identifier list"
      }
      return(list(
        valid = FALSE,
        limit = limit,
        message = sprintf("'%s' must contain at most %d values.", group_name, max_ids)
      ))
    }
  }

  if (!is.null(start) && !is.null(end)) {
    if (end < start) {
      return(list(
        valid = FALSE,
        limit = limit,
        message = "'start' must be before or equal to 'end'."
      ))
    }

    span_days <- as.numeric(difftime(end, start, units = "days"))
    if (span_days > max_span_days) {
      return(list(
        valid = FALSE,
        limit = limit,
        message = sprintf(
          "The requested date range must be at most %d days.",
          max_span_days
        )
      ))
    }
  }

  list(valid = TRUE, limit = limit, message = NULL)
}

#' Bound PostgreSQL work for a public API request
#'
#' Connections are opened per request, so the session-level timeout is scoped
#' to the request and is discarded when its connection closes.
#'
#' @param con An open DBI connection.
#' @param timeout_ms Maximum PostgreSQL statement duration in milliseconds.
#'
#' @return `con`, invisibly.
#' @noRd
api_set_query_timeout <- function(con, timeout_ms = 60000L) {
  timeout_ms <- as.integer(timeout_ms[[1L]])
  if (is.na(timeout_ms) || timeout_ms <= 0L) {
    stop("`timeout_ms` must be a positive integer.", call. = FALSE)
  }

  DBI::dbExecute(con, sprintf("SET statement_timeout = %d", timeout_ms))
  invisible(con)
}
