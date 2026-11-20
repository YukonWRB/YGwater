#' Read the original import provenance for one visible result
#'
#' The result and sample must both be readable. Run and document joins respect
#' their database sharing policies, so a public result cannot disclose the other
#' samples in a restricted workbook or its configuration snapshot.
#' @param con AquaCache connection.
#' @param result_id One result ID.
#' @return One row, or zero rows if the result/sample is unavailable. The
#'   `provenance_state` is `recorded`, `restricted`, or `unrecorded`.
#' @keywords internal
#' @noRd
discrete_result_provenance <- function(con, result_id) {
  if (length(result_id) != 1L || is.na(result_id) || !is.finite(result_id) || result_id < 1) {
    stop("Select a result to view its import provenance.", call. = FALSE)
  }
  row <- DBI::dbGetQuery(con,
    "SELECT r.result_id,r.sample_id,r.import_run_id,r.lab_report_no,r.lab_sample_no,
            ir.import_run_id AS readable_run_id,ir.status,ir.started_at,ir.completed_at,
            ir.created_by,ir.source_adapter_function,ir.adapter_version,ir.source_uri,
            ir.configuration_snapshot::text AS configuration_snapshot,ir.summary::text AS summary,
            src.source_code,src.source_name,p.profile_code,p.profile_name,
            d.document_id,d.name AS document_name,d.format AS document_format,
            octet_length(d.document) AS document_size,d.file_hash,
            s.source_adapter_function AS sample_source_adapter,
            s.external_sample_id AS sample_external_id
     FROM discrete.results r JOIN discrete.samples s USING(sample_id)
     LEFT JOIN discrete.import_runs ir ON ir.import_run_id=r.import_run_id
     LEFT JOIN discrete.import_sources src ON src.import_source_id=ir.import_source_id
     LEFT JOIN discrete.import_profiles p ON p.import_profile_id=ir.import_profile_id
     LEFT JOIN files.documents d ON d.document_id=ir.source_document_id
     WHERE r.result_id=$1", params = list(as.integer(result_id)))
  if (!nrow(row)) return(row)
  row$provenance_state <- if (is.na(row$import_run_id[[1]])) {
    "unrecorded"
  } else if (is.na(row$readable_run_id[[1]])) {
    "restricted"
  } else {
    "recorded"
  }
  row
}
