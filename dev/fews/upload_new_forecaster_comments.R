# Incremental forecaster-comment upload; run only after the backfill is complete.
# This script never creates, drops, or truncates database objects.
#
# Windows Task Scheduler: run Rscript.exe with this script's absolute path as
# its argument. Set "Start in" to the directory containing the .Renviron used
# for the job, and run as an account with access to the report network share.
# Install a current YGwater package containing the backfill comment extractor.
# Required environment variables (same development database as the backfill):
#   aquacacheHostDev, aquacachePortDev, aquacachePostgresPass
# Optional: FORECASTER_COMMENTS_REPORTS_DIR, FORECASTER_COMMENTS_DB_NAME,
#   FORECASTER_COMMENTS_DB_USER (defaults below).
# Schedule after report editing, rather than while a workbook is being saved.
# All dated folders are rescanned deliberately: missed runs and late edits are
# recovered without a timestamp watermark. Only previously unseen comment keys
# are appended; revised comments are retained alongside earlier versions.
# Nonzero exit status signals a failed job; failed workbooks are retried next run.

find_forecaster_comment_workbooks <- function(reports_dir) {
    if (
        length(reports_dir) != 1L ||
            is.na(reports_dir) ||
            !nzchar(reports_dir) ||
            !dir.exists(reports_dir)
    ) {
        stop("Report directory is unavailable: ", reports_dir)
    }

    roots <- reports_dir
    archive_dir <- file.path(reports_dir, "Archive")
    if (dir.exists(archive_dir)) {
        roots <- c(roots, archive_dir)
    }

    folders <- unlist(
        lapply(roots, function(root) {
            list.dirs(root, full.names = TRUE, recursive = FALSE)
        }),
        use.names = FALSE
    )
    folders <- folders[grepl("^\\d{4}-\\d{2}-\\d{2}$", basename(folders))]
    report_dates <- as.Date(basename(folders), format = "%Y-%m-%d")
    valid_dates <- !is.na(report_dates) &
        format(report_dates, "%Y-%m-%d") == basename(folders)
    folders <- folders[valid_dates]
    report_dates <- report_dates[valid_dates]
    paths <- file.path(
        folders,
        paste0("HydrometricReport_", basename(folders), ".xlsx")
    )
    found <- file.exists(paths)
    workbooks <- data.frame(
        workbook_path = paths[found],
        report_date = report_dates[found],
        stringsAsFactors = FALSE
    )
    workbooks <- unique(workbooks)
    workbooks[
        order(workbooks$report_date, workbooks$workbook_path),
        ,
        drop = FALSE
    ]
}

# Can also be sourced and called with an existing DBI connection. The caller
# owns that connection; the command-line entry point disconnects its own.
upload_new_forecaster_comments <- function(con, reports_dir) {
    workbooks <- find_forecaster_comment_workbooks(reports_dir)
    if (!nrow(workbooks)) {
        stop("No dated HydrometricReport workbooks found in: ", reports_dir)
    }

    # Use the exact key/extraction logic used by backfill, including UTC dates
    # and up to four parsed authors. These helpers are internal to YGwater.
    build_key <- getFromNamespace("build_comment_key", "YGwater")
    extract_comments <- getFromNamespace(
        "extract_and_upload_comments",
        "YGwater"
    )

    # Prevent overlapping runs of this scheduled uploader from inserting the
    # same comments. The lock is released on failure or connection termination.
    lock_sql <- "SELECT pg_try_advisory_lock(hashtext('YGwater.forecaster_comments_upload')) AS acquired;"
    acquired <- DBI::dbGetQuery(con, lock_sql)$acquired[[1L]]
    if (!isTRUE(acquired)) {
        stop("Another scheduled forecaster-comment upload is already running.")
    }
    on.exit(
        DBI::dbGetQuery(
            con,
            "SELECT pg_advisory_unlock(hashtext('YGwater.forecaster_comments_upload'));"
        ),
        add = TRUE
    )
    DBI::dbExecute(con, "SET timezone = 'UTC';")

    comment_category_lookup <- DBI::dbGetQuery(
        con,
        "SELECT id, lower(name) AS category_key FROM commentary.comment_categories;"
    )
    comment_category_lookup$category_key <- as.character(
        comment_category_lookup$category_key
    )
    # Match the backfill's document-type selection to preserve deduplication.
    document_type_lookup <- DBI::dbGetQuery(
        con,
        "SELECT document_type_id
         FROM files.document_types
         WHERE lower(document_type_en) LIKE '%hydrometric%'
            OR lower(document_type_en) LIKE '%report%'
         ORDER BY CASE
             WHEN lower(document_type_en) LIKE '%hydrometric%' THEN 1
             WHEN lower(document_type_en) LIKE '%report%' THEN 2
             ELSE 3
         END, document_type_id
         LIMIT 1;"
    )
    if (!nrow(document_type_lookup)) {
        stop("Unable to find a document type for HydrometricReport comments.")
    }
    document_type_id <- document_type_lookup$document_type_id[[1L]]
    author_lookup <- DBI::dbGetQuery(
        con,
        "SELECT author_id,
                trim(concat_ws(' ', first_name, last_name)) AS author,
                first_name, last_name
         FROM commentary.authors
         ORDER BY author_id;"
    )
    author_lookup$author <- as.character(author_lookup$author)
    existing_comments <- DBI::dbGetQuery(
        con,
        "SELECT text_en, document_type_id, comment_category_id, public,
                author_id, second_author_id, third_author_id, fourth_author_id,
                timestamp
         FROM commentary.comments
         WHERE document_type_id = $1;",
        params = list(document_type_id)
    )
    existing_keys <- if (nrow(existing_comments)) {
        build_key(existing_comments)
    } else {
        character(0)
    }

    uploaded_comments <- 0L
    refreshed_comment_authors <- 0L
    processed_workbooks <- 0L
    failed_workbooks <- character(0)
    for (i in seq_len(nrow(workbooks))) {
        workbook_path <- workbooks$workbook_path[[i]]
        result <- tryCatch(
            # A workbook either uploads completely or rolls back; a failed
            # batch cannot leave partial inserts missing from existing_keys.
            DBI::dbWithTransaction(con, {
                extract_comments(
                    workbook_path = workbook_path,
                    report_date = format(
                        workbooks$report_date[[i]],
                        "%Y-%m-%d"
                    ),
                    con = con,
                    comment_category_lookup = comment_category_lookup,
                    document_type_id = document_type_id,
                    author_lookup = author_lookup,
                    existing_keys = existing_keys,
                    remaining_upload_slots = Inf
                )
            }),
            error = function(e) {
                message("FAILED: ", workbook_path, " -- ", conditionMessage(e))
                NULL
            }
        )
        if (is.null(result)) {
            failed_workbooks <- c(failed_workbooks, workbook_path)
            next
        }
        new_count <- if (!is.null(result$inserted_comments)) {
            result$inserted_comments
        } else {
            length(result$existing_keys) - length(existing_keys)
        }
        updated_count <- if (!is.null(result$updated_comments)) {
            result$updated_comments
        } else {
            0L
        }
        existing_keys <- result$existing_keys
        uploaded_comments <- uploaded_comments + new_count
        refreshed_comment_authors <- refreshed_comment_authors + updated_count
        processed_workbooks <- processed_workbooks + 1L
        message(
            "Processed: ",
            workbook_path,
            " (",
            new_count,
            " new comments, ",
            updated_count,
            " existing authors refreshed)"
        )
    }

    message(
        "Finished: ",
        processed_workbooks,
        " workbooks processed, ",
        uploaded_comments,
        " comments uploaded, ",
        refreshed_comment_authors,
        " existing comments had authors refreshed, ",
        length(failed_workbooks),
        " workbooks failed."
    )
    if (length(failed_workbooks)) {
        stop("Some workbooks failed; see the log above. Rerun to retry them.")
    }
    invisible(list(
        processed_workbooks = processed_workbooks,
        uploaded_comments = uploaded_comments,
        refreshed_comment_authors = refreshed_comment_authors
    ))
}

run_new_forecaster_comment_upload <- function() {
    required_vars <- c(
        "aquacacheHostDev",
        "aquacachePortDev",
        "aquacachePostgresPass"
    )
    missing_vars <- required_vars[!nzchar(Sys.getenv(required_vars))]
    if (length(missing_vars)) {
        stop(
            "Missing environment variables: ",
            paste(missing_vars, collapse = ", ")
        )
    }
    reports_dir <- Sys.getenv(
        "FORECASTER_COMMENTS_REPORTS_DIR",
        "//env-fs/env-data/corp/water/Hydrology/03_Reporting/Conditions/tabular_internal_reports"
    )
    # Validate the share before opening a database connection.
    if (!dir.exists(reports_dir)) {
        stop("Report directory is unavailable: ", reports_dir)
    }
    con <- YGwater::AquaConnect(
        name = Sys.getenv("FORECASTER_COMMENTS_DB_NAME", "aquacache"),
        host = Sys.getenv("aquacacheHostDev"),
        port = Sys.getenv("aquacachePortDev"),
        username = Sys.getenv("FORECASTER_COMMENTS_DB_USER", "postgres"),
        password = Sys.getenv("aquacachePostgresPass"),
        silent = TRUE
    )
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    upload_new_forecaster_comments(con, reports_dir)
}

# Sourcing defines functions without uploading; Rscript executes the job once.
if (sys.nframe() == 0L) {
    tryCatch(
        run_new_forecaster_comment_upload(),
        error = function(e) {
            message("Forecaster-comment upload failed: ", conditionMessage(e))
            quit(save = "no", status = 1L)
        }
    )
}
