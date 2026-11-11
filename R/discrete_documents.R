#' Apply sample visibility to linked documents within the caller's transaction
#'
#' Uses AquaCache's privileged, read-only sharing calculation to include linked
#' samples hidden by row-level security and their parent location restrictions.
#' The database function authorizes and locks each requested document and its
#' dependencies until commit. Document updates still use the caller's ordinary
#' privileges. Only linked database samples constrain sharing; omitted workbook
#' content does not. The caller must roll back sample and document changes
#' together if this function fails. Requires the Patch 69 sharing function.
#' @param con AquaCache connection within an active transaction.
#' @param document_ids Documents to recalculate.
#' @param sample_ids Alternatively, recalculate documents linked to these samples.
#' @return Invisibly returns document IDs with the applied groups as a
#'   `visibility` attribute (a named list), for the import configuration snapshot.
#' @keywords internal
#' @noRd
sync_sample_document_visibility <- function(con, document_ids = integer(), sample_ids = integer()) {
  sample_ids <- unique(as.integer(sample_ids))
  sample_ids <- sample_ids[!is.na(sample_ids)]
  if (length(sample_ids)) {
    linked <- DBI::dbGetQuery(con,
      "SELECT DISTINCT document_id FROM discrete.sample_documents
       WHERE sample_id IN (SELECT jsonb_array_elements_text($1::jsonb)::integer)",
      params = list(jsonlite::toJSON(sample_ids, auto_unbox = FALSE)))$document_id
    document_ids <- c(document_ids, linked)
  }
  document_ids <- sort(unique(as.integer(document_ids)))
  document_ids <- document_ids[!is.na(document_ids)]
  if (!length(document_ids)) return(invisible(integer()))
  ids_json <- jsonlite::toJSON(document_ids, auto_unbox = FALSE)
  sharing <- DBI::dbGetQuery(con,
    "SELECT document_id, array_to_json(allowed_groups)::text AS visibility
     FROM discrete.get_document_sharing_groups(
       ARRAY(SELECT jsonb_array_elements_text($1::jsonb)::integer)
     )", params = list(ids_json))
  if (anyDuplicated(sharing$document_id) || !setequal(sharing$document_id, document_ids)) {
    stop("A linked document is unavailable or cannot be modified.", call. = FALSE)
  }
  visibility <- lapply(sharing$visibility, jsonlite::fromJSON)
  names(visibility) <- as.character(sharing$document_id)
  if (any(lengths(visibility) == 0L)) stop("Document sharing could not be established.", call. = FALSE)
  for (i in seq_len(nrow(sharing))) {
    updated <- DBI::dbExecute(con,
      "UPDATE files.documents
       SET share_with = ARRAY(SELECT jsonb_array_elements_text($1::jsonb))
       WHERE document_id=$2", params = list(sharing$visibility[[i]], sharing$document_id[[i]]))
    if (updated != 1L) stop("A linked document could not be updated.", call. = FALSE)
  }
  invisible(structure(document_ids, visibility = visibility))
}

#' Lock document rows before changing their samples or associations
#'
#' All cooperating save paths acquire these locks first, in document ID order.
#' This avoids lock upgrades after FK checks and sample/document lock cycles
#' between concurrent saves of the same workbook. Caller RLS remains active.
#' @param con AquaCache connection within the caller's transaction.
#' @param document_ids Documents whose links or calculated visibility will change.
#' @return Invisibly returns the normalized document IDs.
#' @keywords internal
#' @noRd
lock_sample_documents <- function(con, document_ids) {
  document_ids <- sort(unique(as.integer(document_ids)))
  document_ids <- document_ids[!is.na(document_ids)]
  if (!length(document_ids)) return(invisible(integer()))
  locked <- DBI::dbGetQuery(con,
    "SELECT document_id FROM files.documents
     WHERE document_id IN (SELECT jsonb_array_elements_text($1::jsonb)::integer)
     ORDER BY document_id FOR UPDATE",
    params = list(jsonlite::toJSON(document_ids, auto_unbox = FALSE)))$document_id
  if (!setequal(locked, document_ids)) {
    stop("A linked document is unavailable or cannot be modified.", call. = FALSE)
  }
  invisible(document_ids)
}

