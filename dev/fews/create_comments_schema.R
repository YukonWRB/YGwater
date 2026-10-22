#' Create the database objects used for forecaster commentary.
#'
#' The caller owns the supplied connection.  This function is idempotent and
#' can safely be rerun to apply missing tables, columns, triggers, grants, and
#' seeded reference records.
#'
#' @param con A DBI connection to the target AquaCache database.
#' @param overwrite When TRUE, drop and recreate the commentary schema.
#'
#' @return Invisibly TRUE after the schema objects have been created.
#' @export
create_commentary_schema <- function(con, overwrite = FALSE) {
    if (!inherits(con, "DBIConnection")) {
        stop("`con` must be an open DBI connection.", call. = FALSE)
    }

    if (isTRUE(overwrite)) {
        DBI::dbExecute(con, "DROP SCHEMA IF EXISTS commentary CASCADE;")
    }
    # Create new schema 'commentary' in the database.
    DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS commentary;")

    DBI::dbExecute(
        con,
        "COMMENT ON SCHEMA commentary IS 'Schema to hold commentary-related data, such as comments, annotations, and related metadata.';"
    )

    # Keep the search-path change limited to this setup connection. Do not
    # change the database-wide default used by other applications.
    DBI::dbExecute(
        con,
        "SET search_path TO public, continuous, discrete, spatial, files, instruments, boreholes, audit, information, application, commentary;"
    )

    create_trigger_if_missing <- function(trigger_name, table_name, statement) {
        existing <- DBI::dbGetQuery(
            con,
            "SELECT EXISTS (
                 SELECT 1
                 FROM pg_trigger
                 WHERE tgrelid = $1::regclass
                   AND tgname = $2
                   AND NOT tgisinternal
             ) AS trigger_exists;",
            params = list(paste0("commentary.", table_name), trigger_name)
        )$trigger_exists[[1]]

        if (!isTRUE(existing)) {
            DBI::dbExecute(con, statement)
        }
    }

    # Grant usage to all
    DBI::dbExecute(con, "GRANT USAGE ON SCHEMA commentary TO yg_reader_group;")
    DBI::dbExecute(
        con,
        "ALTER DEFAULT PRIVILEGES IN SCHEMA commentary GRANT SELECT ON TABLES TO yg_reader_group;"
    )

    DBI::dbExecute(con, "GRANT USAGE ON SCHEMA commentary TO yg_editor_group;")
    DBI::dbExecute(
        con,
        "ALTER DEFAULT PRIVILEGES IN SCHEMA commentary GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO yg_editor_group;"
    )

    # Create table to hold text for the application
    DBI::dbExecute(
        con,
        "CREATE TABLE IF NOT EXISTS commentary.comment_categories (
    id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
    name TEXT NOT NULL UNIQUE,
    description TEXT,
    created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    created_by TEXT DEFAULT CURRENT_USER NOT NULL,
    modified TIMESTAMP WITH TIME ZONE,
    modified_by TEXT
    );"
    )

    create_trigger_if_missing(
        "trg_user_audit",
        "comment_categories",
        "CREATE TRIGGER trg_user_audit BEFORE UPDATE ON commentary.comment_categories FOR EACH ROW EXECUTE FUNCTION user_modified()"
    )
    create_trigger_if_missing(
        "update_modify_time",
        "comment_categories",
        "CREATE TRIGGER update_modify_time BEFORE UPDATE ON commentary.comment_categories FOR EACH ROW EXECUTE FUNCTION update_modified()"
    )

    # Insert sample comment categories
    DBI::dbExecute(
        con,
        "INSERT INTO commentary.comment_categories (name, description) VALUES
    ('Current conditions', 'Comments related to current conditions'),
    ('Future conditions', 'Comments related to future conditions'),
    ('Levels', 'Comments related to current and forecasted water levels'),
    ('Flows', 'Comments related to current and forecasted water flows'),
    ('Bridges', 'Comments related to bridges'),
    ('Snow', 'Comments related to current and forecasted snow conditions'),
    ('Precipitation', 'Comments related to current and forecasted precipitation levels'),
    ('Temperature', 'Comments related to current and forecasted temperature levels'),
    ('River ice', 'Comments related to current and forecasted river ice conditions')
    ON CONFLICT DO NOTHING;"
    )

    # Comment on the table
    DBI::dbExecute(
        con,
        "COMMENT ON TABLE commentary.comment_categories IS 'Table to hold categories for comments, used to classify and organize comments.';"
    )

    # Create table to hold author names and titles
    DBI::dbExecute(
        con,
        "CREATE TABLE IF NOT EXISTS commentary.authors (
                author_id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
                first_name TEXT NOT NULL,
                last_name TEXT NOT NULL,
                title TEXT
                );"
    )

    DBI::dbExecute(
        con,
        "ALTER TABLE commentary.authors
     ADD COLUMN IF NOT EXISTS share_with TEXT[] NOT NULL
     DEFAULT ARRAY['yg_reader_group']::TEXT[];"
    )
    DBI::dbExecute(
        con,
        "UPDATE commentary.authors
     SET share_with = ARRAY['yg_reader_group']::TEXT[]
     WHERE share_with IS DISTINCT FROM ARRAY['yg_reader_group']::TEXT[];"
    )

    DBI::dbExecute(
        con,
        "COMMENT ON TABLE commentary.authors IS 'Table to hold author names and titles for commentary content.';"
    )
    DBI::dbExecute(
        con,
        "COMMENT ON COLUMN commentary.authors.author_id IS 'Unique identifier for each author.';"
    )
    DBI::dbExecute(
        con,
        "COMMENT ON COLUMN commentary.authors.first_name IS 'Author first name or preferred first-name form.';"
    )
    DBI::dbExecute(
        con,
        "COMMENT ON COLUMN commentary.authors.last_name IS 'Author last name or surname.';"
    )
    DBI::dbExecute(
        con,
        "COMMENT ON COLUMN commentary.authors.title IS 'Author title or role.';"
    )
    DBI::dbExecute(
        con,
        "CREATE UNIQUE INDEX IF NOT EXISTS idx_commentary_authors_first_last_unique ON commentary.authors (first_name, last_name);"
    )
    DBI::dbExecute(
        con,
        "INSERT INTO commentary.authors (first_name, last_name, title) VALUES
    ('Anthony', 'Bier', 'Hydrologist'),
    ('EJ', 'Bercier', 'Hydrologist'),
    ('Holly', 'Goulding', 'Senior Scientist, Hydrology'),
    ('Tyler', 'Williams', 'Water Resources Scientist'),
    ('Everett', 'Snieder', 'Water and Climate Data Scientist'),
    ('Ghislain', 'de Laplante', 'Water and Climate Data Scientist')
    ON CONFLICT (first_name, last_name) DO NOTHING;"
    )

    # Create table to hold text for the application
    DBI::dbExecute(
        con,
        "CREATE TABLE IF NOT EXISTS commentary.comments (
                id INTEGER PRIMARY KEY GENERATED ALWAYS AS IDENTITY,
                text_en TEXT NOT NULL,
                text_fr TEXT,
                link TEXT,
                document_type_id INTEGER NOT NULL REFERENCES files.document_types(document_type_id),
                location_id INTEGER REFERENCES public.locations(location_id),
                comment_category_id INTEGER REFERENCES commentary.comment_categories(id),
                public BOOLEAN NOT NULL DEFAULT FALSE,
                raw_author TEXT,
                author_id INTEGER REFERENCES commentary.authors(author_id),
                second_author_id INTEGER REFERENCES commentary.authors(author_id),
                third_author_id INTEGER REFERENCES commentary.authors(author_id),
                fourth_author_id INTEGER REFERENCES commentary.authors(author_id),
                timestamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                created_by TEXT DEFAULT CURRENT_USER NOT NULL,
                modified TIMESTAMP WITH TIME ZONE,
                modified_by TEXT
                );"
    )

    DBI::dbExecute(
        con,
        "ALTER TABLE commentary.comments
     ADD COLUMN IF NOT EXISTS share_with TEXT[] NOT NULL
     DEFAULT ARRAY['yg_reader_group']::TEXT[];"
    )
    DBI::dbExecute(
        con,
        "UPDATE commentary.comments
     SET share_with = ARRAY['yg_reader_group']::TEXT[]
     WHERE share_with IS DISTINCT FROM ARRAY['yg_reader_group']::TEXT[];"
    )

    # poly on table restrict share_with

    create_trigger_if_missing(
        "trg_user_audit",
        "comments",
        "CREATE TRIGGER trg_user_audit BEFORE UPDATE ON commentary.comments FOR EACH ROW EXECUTE FUNCTION user_modified()"
    )
    create_trigger_if_missing(
        "update_modify_time",
        "comments",
        "CREATE TRIGGER update_modify_time BEFORE UPDATE ON commentary.comments FOR EACH ROW EXECUTE FUNCTION update_modified()"
    )

    # Comment on the table
    DBI::dbExecute(
        con,
        "COMMENT ON TABLE commentary.comments IS 'Table to hold frequently changed comments on documents and locations, made by forecasters and other users.';"
    )
    DBI::dbExecute(
        con,
        "COMMENT ON COLUMN commentary.comments.id IS 'Unique identifier for the comments; this is referenced in the application to select the correct entry.';"
    )

    DBI::dbExecute(
        con,
        "COMMENT ON COLUMN commentary.comments.raw_author IS 'Raw, unparsed author string as read from the source workbook cell.';"
    )
    DBI::dbExecute(
        con,
        "COMMENT ON COLUMN commentary.comments.timestamp IS 'Timestamp for the comments; might differ from the created timestamp.';"
    )

    invisible(TRUE)
}

#' Scrape forecaster comments from dated tabular reports.
#'
#' Author aliases are constructed locally so variants such as `A. Bier`,
#' `ABier`, and `Anthony Bier` resolve to the same author record without
#' changing the package-wide report parser.
#'
#' @param con An open DBI connection with the commentary schema installed.
#' @param tabular_report_dir Root directory containing dated report folders and
#'   optionally an `Archive` subdirectory.
#' @param nDays Number of calendar days to include; use `-1` for all reports.
#' @param show_progress Display a text progress bar when TRUE.
#'
#' @return Invisibly, a list containing counts of processed workbooks and
#'   inserted comments.
#' @export
scrape_tabular_report_comments <- function(
    con,
    tabular_report_dir,
    nDays = -1L,
    show_progress = TRUE
) {
    if (!inherits(con, "DBIConnection")) {
        stop("`con` must be an open DBI connection.", call. = FALSE)
    }
    if (!dir.exists(tabular_report_dir)) {
        stop(
            "`tabular_report_dir` does not exist: ",
            tabular_report_dir,
            call. = FALSE
        )
    }
    if (
        length(nDays) != 1L ||
            is.na(nDays) ||
            nDays < -1 ||
            nDays != as.integer(nDays)
    ) {
        stop(
            "`nDays` must be -1 or a non-negative whole number.",
            call. = FALSE
        )
    }

    report_roots <- tabular_report_dir
    archive_dir <- file.path(tabular_report_dir, "Archive")
    if (dir.exists(archive_dir)) {
        report_roots <- c(report_roots, archive_dir)
    }
    folders <- unique(unlist(
        lapply(report_roots, function(root) {
            list.dirs(root, full.names = TRUE, recursive = FALSE)
        }),
        use.names = FALSE
    ))
    folder_names <- basename(folders)
    report_dates <- as.Date(folder_names, format = "%Y-%m-%d")
    valid <- !is.na(report_dates) &
        format(report_dates, "%Y-%m-%d") == folder_names
    folders <- folders[valid]
    report_dates <- report_dates[valid]
    if (nDays != -1L) {
        keep <- report_dates >= Sys.Date() - as.integer(nDays)
        folders <- folders[keep]
        report_dates <- report_dates[keep]
    }
    workbook_paths <- file.path(
        folders,
        paste0("HydrometricReport_", basename(folders), ".xlsx")
    )
    present <- file.exists(workbook_paths)
    workbook_paths <- workbook_paths[present]
    report_dates <- report_dates[present]
    ordering <- order(report_dates, workbook_paths)
    workbook_paths <- workbook_paths[ordering]
    report_dates <- report_dates[ordering]
    if (!length(workbook_paths)) {
        return(invisible(list(
            processed_workbooks = 0L,
            inserted_comments = 0L
        )))
    }

    comment_category_lookup <- DBI::dbGetQuery(
        con,
        "SELECT id, lower(name) AS category_key FROM commentary.comment_categories;"
    )
    comment_category_lookup$category_key <- as.character(
        comment_category_lookup$category_key
    )
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

    authors <- DBI::dbGetQuery(
        con,
        "SELECT author_id, first_name, last_name FROM commentary.authors
         ORDER BY author_id;"
    )
    if (!nrow(authors)) {
        stop("No commentary authors are configured.")
    }
    build_author_name_aliases <- function(first_name, last_name) {
        first_name <- trimws(as.character(first_name))
        last_name <- trimws(as.character(last_name))
        first_initial <- substr(first_name, 1L, 1L)
        last_initial <- substr(last_name, 1L, 1L)
        first_name_words <- strsplit(first_name, "[[:space:]-]+")[[1L]]
        first_name_initials <- paste0(
            substr(first_name_words[nzchar(first_name_words)], 1L, 1L),
            collapse = ""
        )
        compact_first_name <- gsub("[^[:alpha:]]", "", first_name)
        if (
            nchar(compact_first_name) > 1L &&
                nchar(compact_first_name) <= 3L &&
                identical(compact_first_name, toupper(compact_first_name))
        ) {
            first_name_initials <- compact_first_name
        }
        initials <- paste0(first_name_initials, last_initial)
        dotted_initials <- paste(
            strsplit(initials, "")[[1L]],
            collapse = "."
        )

        unique(c(
            paste(first_name, last_name),
            paste(first_initial, last_name),
            paste0(first_initial, ". ", last_name),
            paste0(first_initial, ".", last_name),
            paste0(first_initial, last_name),
            paste0(first_name, last_name),
            paste(last_name, first_name, sep = ", "),
            paste0(first_initial, ".", last_initial),
            paste0(first_initial, last_initial),
            initials,
            paste(first_name_initials, last_initial),
            paste0(first_name_initials, ".", last_initial),
            dotted_initials,
            paste0(dotted_initials, ".")
        ))
    }
    author_lookup <- do.call(
        rbind,
        lapply(seq_len(nrow(authors)), function(i) {
            first_name <- trimws(authors$first_name[[i]])
            last_name <- trimws(authors$last_name[[i]])
            aliases <- build_author_name_aliases(first_name, last_name)
            data.frame(
                author_id = rep(authors$author_id[[i]], length(aliases)),
                author = aliases,
                stringsAsFactors = FALSE
            )
        })
    )
    # A shared short alias must not silently select the first author.
    duplicated_alias <- duplicated(author_lookup$author) |
        duplicated(author_lookup$author, fromLast = TRUE)
    author_lookup <- author_lookup[!duplicated_alias, , drop = FALSE]

    build_key <- getFromNamespace("build_comment_key", "YGwater")
    extract_comments <- getFromNamespace(
        "extract_and_upload_comments",
        "YGwater"
    )
    existing_comments <- DBI::dbGetQuery(
        con,
        "SELECT text_en, document_type_id, comment_category_id, public,
                author_id, second_author_id, third_author_id, fourth_author_id,
                timestamp
         FROM commentary.comments
         WHERE document_type_id = $1;",
        params = list(document_type_lookup$document_type_id[[1L]])
    )
    existing_keys <- if (nrow(existing_comments)) {
        build_key(existing_comments)
    } else {
        character(0)
    }

    progress <- if (isTRUE(show_progress)) {
        utils::txtProgressBar(min = 0, max = length(workbook_paths), style = 3)
    } else {
        NULL
    }
    on.exit(if (!is.null(progress)) close(progress), add = TRUE)
    inserted_comments <- 0L
    for (i in seq_along(workbook_paths)) {
        result <- DBI::dbWithTransaction(
            con,
            extract_comments(
                workbook_path = workbook_paths[[i]],
                report_date = format(report_dates[[i]], "%Y-%m-%d"),
                con = con,
                comment_category_lookup = comment_category_lookup,
                document_type_id = document_type_lookup$document_type_id[[1L]],
                author_lookup = author_lookup,
                existing_keys = existing_keys,
                remaining_upload_slots = Inf
            )
        )
        new_count <- if (!is.null(result$inserted_comments)) {
            result$inserted_comments
        } else {
            length(result$existing_keys) - length(existing_keys)
        }
        existing_keys <- result$existing_keys
        inserted_comments <- inserted_comments + new_count
        if (!is.null(progress)) {
            utils::setTxtProgressBar(progress, i)
        }
    }

    invisible(list(
        processed_workbooks = length(workbook_paths),
        inserted_comments = inserted_comments
    ))
}


# Run the schema setup and the recent-comment scrape when this file is run
# directly. Sourcing the file only defines the two functions above.
if (sys.nframe() == 0L) {
    required_vars <- c(
        "aquacacheHostDev",
        "aquacachePortDev",
        "aquacachePostgresPass"
    )
    missing_vars <- required_vars[!nzchar(Sys.getenv(required_vars))]
    if (length(missing_vars)) {
        stop(
            "Missing environment variables: ",
            paste(missing_vars, collapse = ", "),
            call. = FALSE
        )
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

    tabular_report_dir <- Sys.getenv(
        "FORECASTER_COMMENTS_REPORTS_DIR",
        "//env-fs/env-data/corp/water/Hydrology/03_Reporting/Conditions/tabular_internal_reports"
    )
    create_commentary_schema(con, overwrite = TRUE)
    scrape_tabular_report_comments(
        con,
        tabular_report_dir = tabular_report_dir,
        nDays = 1000
    )
}
