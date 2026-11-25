# Shared persistence and overlap guards for instrument deployment records.

instrument_deployment_ids <- function(x) {
  if (is.null(x) || !length(x)) {
    return(integer())
  }
  if (is.list(x) && length(x) == 1L) {
    x <- x[[1L]]
  }
  if (is.character(x) && length(x) == 1L && grepl("^\\{.*\\}$", x)) {
    contents <- sub("^\\{(.*)\\}$", "\\1", x)
    x <- if (nzchar(contents)) strsplit(contents, ",", fixed = TRUE)[[1L]] else character()
  }
  value <- suppressWarnings(as.integer(x))
  sort(unique(value[!is.na(value)]))
}

instrument_deployment_plan_from_values <- function(values, plan_id = 1L) {
  data.frame(
    plan_id = as.integer(plan_id),
    instrument_id = as.integer(values$instrument_id),
    start_datetime = as.POSIXct(values$start_datetime, tz = "UTC"),
    end_datetime = as.POSIXct(values$end_datetime, tz = "UTC"),
    timeseries_ids = I(list(instrument_deployment_ids(values$timeseries_ids))),
    stringsAsFactors = FALSE
  )
}

instrument_deployment_placeholder_clause <- function(values, first_index) {
  if (!length(values)) {
    return(NULL)
  }
  paste0("$", seq.int(first_index, length.out = length(values)), collapse = ", ")
}

instrument_deployment_existing_records <- function(con, plan) {
  instrument_ids <- sort(unique(as.integer(plan$instrument_id)))
  timeseries_ids <- sort(unique(unlist(plan$timeseries_ids, use.names = FALSE)))
  timeseries_ids <- instrument_deployment_ids(timeseries_ids)
  if (!length(instrument_ids) && !length(timeseries_ids)) {
    return(data.frame(
      metadata_id = integer(),
      instrument_id = integer(),
      serial_no = character(),
      start_datetime = as.POSIXct(character(), tz = "UTC"),
      end_datetime = as.POSIXct(character(), tz = "UTC"),
      timeseries_id = integer(),
      association_start_datetime = as.POSIXct(character(), tz = "UTC"),
      association_end_datetime = as.POSIXct(character(), tz = "UTC"),
      stringsAsFactors = FALSE
    ))
  }

  params <- list()
  predicates <- character()
  if (length(instrument_ids)) {
    instrument_clause <- instrument_deployment_placeholder_clause(
      instrument_ids,
      length(params) + 1L
    )
    predicates <- c(predicates, paste0("lmi.instrument_id IN (", instrument_clause, ")"))
    params <- c(params, as.list(instrument_ids))
  }
  if (length(timeseries_ids)) {
    timeseries_clause <- instrument_deployment_placeholder_clause(
      timeseries_ids,
      length(params) + 1L
    )
    predicates <- c(
      predicates,
      paste0(
        "EXISTS (SELECT 1 FROM deployment_timeseries AS relevant ",
        "WHERE relevant.metadata_id = lmi.metadata_id ",
        "AND relevant.timeseries_id IN (",
        timeseries_clause,
        "))"
      )
    )
    params <- c(params, as.list(timeseries_ids))
  }

  DBI::dbGetQuery(
    con,
    paste0(
      "WITH deployment_timeseries AS (",
      " SELECT lmit.metadata_id, lmit.timeseries_id,",
      "        lmi.start_datetime AS association_start_datetime,",
      "        lmi.end_datetime AS association_end_datetime",
      " FROM public.locations_metadata_instrument_timeseries AS lmit",
      " INNER JOIN public.locations_metadata_instruments AS lmi",
      "   ON lmi.metadata_id = lmit.metadata_id",
      " UNION",
      " SELECT lmi.metadata_id, s.timeseries_id,",
      "        GREATEST(lmi.start_datetime, c.start_datetime) AS association_start_datetime,",
      "        CASE WHEN lmi.end_datetime IS NULL THEN c.end_datetime",
      "             WHEN c.end_datetime IS NULL THEN lmi.end_datetime",
      "             ELSE LEAST(lmi.end_datetime, c.end_datetime) END AS association_end_datetime",
      " FROM public.locations_metadata_instrument_connections AS c",
      " INNER JOIN public.locations_metadata_instrument_connection_signals AS s",
      "   ON s.connection_id = c.connection_id",
      " INNER JOIN public.locations_metadata_instruments AS lmi",
      "   ON lmi.metadata_id = c.instrument_metadata_id",
      " WHERE c.instrument_metadata_id IS NOT NULL AND s.timeseries_id IS NOT NULL",
      " UNION",
      " SELECT lmi.metadata_id, s.timeseries_id,",
      "        GREATEST(lmi.start_datetime, c.start_datetime) AS association_start_datetime,",
      "        CASE WHEN lmi.end_datetime IS NULL THEN c.end_datetime",
      "             WHEN c.end_datetime IS NULL THEN lmi.end_datetime",
      "             ELSE LEAST(lmi.end_datetime, c.end_datetime) END AS association_end_datetime",
      " FROM public.locations_metadata_instrument_connections AS c",
      " INNER JOIN public.locations_metadata_instrument_connection_signals AS s",
      "   ON s.connection_id = c.connection_id",
      " INNER JOIN public.locations_metadata_instruments AS lmi",
      "   ON lmi.metadata_id = c.logger_metadata_id",
      " WHERE c.logger_metadata_id IS NOT NULL AND s.timeseries_id IS NOT NULL",
      ") ",
      "SELECT lmi.metadata_id, lmi.instrument_id, i.serial_no,",
      "       lmi.start_datetime, lmi.end_datetime,",
      "       linked.timeseries_id, linked.association_start_datetime,",
      "       linked.association_end_datetime ",
      "FROM public.locations_metadata_instruments AS lmi ",
      "LEFT JOIN instruments.instruments AS i",
      "  ON i.instrument_id = lmi.instrument_id ",
      "LEFT JOIN deployment_timeseries AS linked",
      "  ON linked.metadata_id = lmi.metadata_id ",
      "WHERE ",
      paste(predicates, collapse = " OR "),
      " ORDER BY lmi.metadata_id, linked.timeseries_id, linked.association_start_datetime"
    ),
    params = params
  )
}

instrument_deployment_ranges_overlap <- function(a_start, a_end, b_start, b_end) {
  a_start <- suppressWarnings(as.POSIXct(a_start, tz = "UTC"))
  b_start <- suppressWarnings(as.POSIXct(b_start, tz = "UTC"))
  a_end <- suppressWarnings(as.POSIXct(a_end, tz = "UTC"))
  b_end <- suppressWarnings(as.POSIXct(b_end, tz = "UTC"))
  if (
    length(a_start) != 1L || length(b_start) != 1L ||
      is.na(a_start) || is.na(b_start)
  ) {
    return(FALSE)
  }
  a_start_num <- as.numeric(a_start)
  b_start_num <- as.numeric(b_start)
  a_end_num <- if (is.na(a_end)) Inf else as.numeric(a_end)
  b_end_num <- if (is.na(b_end)) Inf else as.numeric(b_end)
  a_start_num < b_end_num && b_start_num < a_end_num
}

instrument_deployment_find_conflicts <- function(
  con,
  plan,
  exclude_metadata_ids = integer()
) {
  if (is.null(plan) || nrow(plan) == 0L) {
    return(data.frame())
  }
  required <- c(
    "plan_id", "instrument_id", "start_datetime", "end_datetime", "timeseries_ids"
  )
  missing <- setdiff(required, names(plan))
  if (length(missing)) {
    stop("Deployment plan is missing: ", paste(missing, collapse = ", "))
  }

  existing <- instrument_deployment_existing_records(con, plan)
  if (length(exclude_metadata_ids) && nrow(existing)) {
    existing <- existing[
      !existing$metadata_id %in% as.integer(exclude_metadata_ids),
      ,
      drop = FALSE
    ]
  }
  if (!nrow(existing)) {
    return(data.frame())
  }

  conflicts <- list()
  for (plan_row in seq_len(nrow(plan))) {
    proposed_id <- as.integer(plan$instrument_id[[plan_row]])
    proposed_timeseries <- instrument_deployment_ids(
      plan$timeseries_ids[[plan_row]]
    )
    proposed_start <- plan$start_datetime[[plan_row]]
    proposed_end <- plan$end_datetime[[plan_row]]

    for (existing_row in seq_len(nrow(existing))) {
      existing_id <- as.integer(existing$instrument_id[[existing_row]])
      same_instrument <- !is.na(proposed_id) &&
        !is.na(existing_id) &&
        proposed_id == existing_id
      deployment_overlap <- instrument_deployment_ranges_overlap(
        proposed_start,
        proposed_end,
        existing$start_datetime[[existing_row]],
        existing$end_datetime[[existing_row]]
      )
      if (!deployment_overlap) {
        next
      }

      existing_timeseries_id <- suppressWarnings(as.integer(
        existing$timeseries_id[[existing_row]]
      ))
      shared_timeseries <- if (
        !is.na(existing_timeseries_id) &&
          existing_timeseries_id %in% proposed_timeseries &&
          instrument_deployment_ranges_overlap(
            proposed_start,
            proposed_end,
            existing$association_start_datetime[[existing_row]],
            existing$association_end_datetime[[existing_row]]
          )
      ) {
        existing_timeseries_id
      } else {
        integer()
      }
      if (!same_instrument && !length(shared_timeseries)) {
        next
      }

      reason <- if (same_instrument && length(shared_timeseries)) {
        "The instrument and timeseries already have an overlapping deployment."
      } else if (length(shared_timeseries)) {
        "The timeseries is already assigned to another instrument during this period."
      } else {
        "The instrument already has an overlapping deployment record."
      }
      conflicts[[length(conflicts) + 1L]] <- data.frame(
        plan_id = as.integer(plan$plan_id[[plan_row]]),
        proposed_instrument_id = proposed_id,
        proposed_start_datetime = as.POSIXct(proposed_start, tz = "UTC"),
        proposed_end_datetime = as.POSIXct(proposed_end, tz = "UTC"),
        metadata_id = as.integer(existing$metadata_id[[existing_row]]),
        existing_instrument_id = existing_id,
        existing_serial_no = as.character(existing$serial_no[[existing_row]]),
        existing_start_datetime = as.POSIXct(
          existing$start_datetime[[existing_row]],
          tz = "UTC"
        ),
        existing_end_datetime = as.POSIXct(
          existing$end_datetime[[existing_row]],
          tz = "UTC"
        ),
        timeseries_id = if (length(shared_timeseries)) {
          shared_timeseries[[1L]]
        } else {
          NA_integer_
        },
        reason = reason,
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(conflicts)) {
    return(data.frame())
  }
  result <- do.call(rbind, conflicts)
  conflict_key <- paste(
    result$plan_id,
    result$metadata_id,
    result$timeseries_id,
    result$reason,
    sep = ":"
  )
  result[!duplicated(conflict_key), , drop = FALSE]
}

instrument_deployment_conflict_message <- function(conflicts) {
  if (is.null(conflicts) || !nrow(conflicts)) {
    return("")
  }
  details <- vapply(seq_len(nrow(conflicts)), function(i) {
    end <- conflicts$existing_end_datetime[[i]]
    end_text <- if (is.na(end)) "still deployed" else paste(
      "recovered",
      format(end, "%Y-%m-%d %H:%M UTC", tz = "UTC")
    )
    timeseries_text <- if (is.na(conflicts$timeseries_id[[i]])) {
      ""
    } else {
      paste0(" Timeseries #", conflicts$timeseries_id[[i]], ".")
    }
    paste0(
      "Deployment #", conflicts$metadata_id[[i]], " (",
      ifelse(
        is.na(conflicts$existing_serial_no[[i]]) || !nzchar(conflicts$existing_serial_no[[i]]),
        paste("instrument", conflicts$existing_instrument_id[[i]]),
        conflicts$existing_serial_no[[i]]
      ),
      ") started ",
      format(
        conflicts$existing_start_datetime[[i]],
        "%Y-%m-%d %H:%M UTC",
        tz = "UTC"
      ),
      " and is ", end_text, ".",
      timeseries_text,
      " ", conflicts$reason[[i]]
    )
  }, character(1))
  paste(
    "Deployment was not saved because an existing deployment overlaps:",
    paste0("- ", details, collapse = "\n"),
    "Recover the earlier deployment before starting a different instrument, or use the existing deployment record instead of creating a duplicate.",
    sep = "\n"
  )
}

instrument_deployment_conflict_condition <- function(conflicts) {
  structure(
    list(
      message = instrument_deployment_conflict_message(conflicts),
      call = NULL,
      conflicts = conflicts
    ),
    class = c("instrument_deployment_conflict", "error", "condition")
  )
}

instrument_deployment_stop_for_conflicts <- function(conflicts) {
  stop(instrument_deployment_conflict_condition(conflicts))
}

instrument_deployment_acquire_locks <- function(
  con,
  plan,
  instrument_ids = integer(),
  timeseries_ids = integer()
) {
  resource_keys <- c(
    paste0(
      "YGwater:instrument-deployment:instrument:",
      sort(unique(c(as.integer(plan$instrument_id), as.integer(instrument_ids))))
    ),
    paste0(
      "YGwater:instrument-deployment:timeseries:",
      sort(unique(c(
        unlist(plan$timeseries_ids, use.names = FALSE),
        as.integer(timeseries_ids)
      )))
    )
  )
  resource_keys <- sort(unique(resource_keys[nzchar(resource_keys)]))
  resource_keys <- resource_keys[!grepl(":NA$", resource_keys)]
  for (resource_key in resource_keys) {
    DBI::dbGetQuery(
      con,
      "SELECT pg_advisory_xact_lock(hashtext($1))",
      params = list(resource_key)
    )
  }
  invisible(NULL)
}

instrument_deployment_assert_no_conflicts <- function(
  con,
  plan,
  exclude_metadata_ids = integer(),
  lock_instrument_ids = integer(),
  lock_timeseries_ids = integer()
) {
  instrument_deployment_acquire_locks(
    con,
    plan,
    instrument_ids = lock_instrument_ids,
    timeseries_ids = lock_timeseries_ids
  )
  conflicts <- instrument_deployment_find_conflicts(
    con,
    plan,
    exclude_metadata_ids = exclude_metadata_ids
  )
  if (nrow(conflicts)) {
    instrument_deployment_stop_for_conflicts(conflicts)
  }
  invisible(NULL)
}

instrument_deployment_with_guard <- function(
  con,
  plan,
  action,
  exclude_metadata_ids = integer(),
  lock_instrument_ids = integer(),
  lock_timeseries_ids = integer()
) {
  DBI::dbWithTransaction(con, {
    instrument_deployment_assert_no_conflicts(
      con,
      plan,
      exclude_metadata_ids = exclude_metadata_ids,
      lock_instrument_ids = lock_instrument_ids,
      lock_timeseries_ids = lock_timeseries_ids
    )
    action()
  })
}

instrument_deployment_insert <- function(con, values) {
  DBI::dbGetQuery(
    con,
    "INSERT INTO public.locations_metadata_instruments (
       location_id, sub_location_id, instrument_id, start_datetime, end_datetime,
       note, z_id, deployment_visit_id, recovery_visit_id, firmware_version,
       logger_program_revision, record_unit_id, record_interval_seconds,
       record_duration_seconds, record_aggregation_type_id
     ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15)
     RETURNING metadata_id",
    params = unname(c(
      values[c(
        "location_id", "sub_location_id", "instrument_id", "start_datetime",
        "end_datetime", "note", "z_id"
      )],
      values$metadata
    ))
  )$metadata_id[[1L]]
}

instrument_deployment_replace_timeseries <- function(
  con,
  metadata_id,
  timeseries_ids
) {
  metadata_id <- as.integer(metadata_id)
  if (length(metadata_id) != 1L || is.na(metadata_id)) {
    stop("A deployment metadata_id is required to update timeseries links.")
  }
  timeseries_ids <- instrument_deployment_ids(timeseries_ids)
  DBI::dbExecute(
    con,
    "DELETE FROM public.locations_metadata_instrument_timeseries
     WHERE metadata_id = $1",
    params = list(metadata_id)
  )
  for (timeseries_id in timeseries_ids) {
    DBI::dbExecute(
      con,
      "INSERT INTO public.locations_metadata_instrument_timeseries (
         metadata_id, timeseries_id
       ) VALUES ($1, $2)",
      params = list(metadata_id, timeseries_id)
    )
  }
  invisible(NULL)
}

instrument_deployment_add_timeseries <- function(
  con,
  metadata_id,
  timeseries_ids
) {
  metadata_id <- as.integer(metadata_id)
  timeseries_ids <- instrument_deployment_ids(timeseries_ids)
  for (timeseries_id in timeseries_ids) {
    DBI::dbExecute(
      con,
      "INSERT INTO public.locations_metadata_instrument_timeseries (
         metadata_id, timeseries_id
       ) VALUES ($1, $2)
       ON CONFLICT (metadata_id, timeseries_id) DO NOTHING",
      params = list(metadata_id, timeseries_id)
    )
  }
  invisible(NULL)
}
