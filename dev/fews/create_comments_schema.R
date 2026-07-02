con <- YGwater::AquaConnect(
    name = "aquacache",
    host = Sys.getenv("aquacacheHostDev"),
    port = Sys.getenv("aquacachePortDev"),
    user = "postgres",
    password = Sys.getenv("aquacachePostgresPass"),
)

# ITERATIVE RUN ONLY: remove this block when you no longer want each run to
# rebuild the commentary schema from scratch.
DBI::dbExecute(con, "DROP SCHEMA IF EXISTS commentary CASCADE;")

# Create new schema 'commentary' in the database
DBI::dbExecute(con, "CREATE SCHEMA IF NOT EXISTS commentary;")

DBI::dbExecute(
    con,
    "COMMENT ON SCHEMA commentary IS 'Schema to hold commentary-related data, such as comments, annotations, and related metadata.';"
)

# modify the seach path to include the new schema
DBI::dbExecute(
    con,
    "ALTER DATABASE aquacache SET search_path TO public, continuous, discrete, spatial, files, instruments, boreholes, audit, information, application, commentary;"
)

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

DBI::dbExecute(
    con,
    "DROP TRIGGER IF EXISTS trg_user_audit ON commentary.comment_categories;"
)
DBI::dbExecute(
    con,
    "create trigger trg_user_audit before update on commentary.comment_categories for each row execute function user_modified()"
)
DBI::dbExecute(
    con,
    "DROP TRIGGER IF EXISTS update_modify_time ON commentary.comment_categories;"
)
DBI::dbExecute(
    con,
    "create trigger update_modify_time before update on commentary.comment_categories for each row execute function update_modified()"
)

DBI::dbExecute(
    con,
    "DROP TRIGGER IF EXISTS update_comments_modified ON commentary.comment_categories;"
)
DBI::dbExecute(
    con,
    "CREATE TRIGGER update_comments_modified BEFORE UPDATE ON commentary.comment_categories FOR EACH ROW EXECUTE FUNCTION public.update_modified()"
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

# Migrate existing commentary.authors schema from full-name field to split name fields
DBI::dbExecute(
    con,
    "ALTER TABLE commentary.authors ADD COLUMN IF NOT EXISTS first_name TEXT;"
)
DBI::dbExecute(
    con,
    "ALTER TABLE commentary.authors ADD COLUMN IF NOT EXISTS last_name TEXT;"
)
DBI::dbExecute(
    con,
    "ALTER TABLE commentary.authors DROP COLUMN IF EXISTS author;"
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
    "DROP INDEX IF EXISTS commentary.idx_commentary_authors_author_unique;"
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

# Migrate existing commentary.comments schema from text authors to FK authors
DBI::dbExecute(
    con,
    "ALTER TABLE commentary.comments DROP COLUMN IF EXISTS author;"
)
DBI::dbExecute(
    con,
    "ALTER TABLE commentary.comments ADD COLUMN IF NOT EXISTS raw_author TEXT;"
)
DBI::dbExecute(
    con,
    "ALTER TABLE commentary.comments ADD COLUMN IF NOT EXISTS author_id INTEGER REFERENCES commentary.authors(author_id);"
)
DBI::dbExecute(
    con,
    "ALTER TABLE commentary.comments ADD COLUMN IF NOT EXISTS second_author_id INTEGER REFERENCES commentary.authors(author_id);"
)
DBI::dbExecute(
    con,
    "ALTER TABLE commentary.comments ADD COLUMN IF NOT EXISTS third_author_id INTEGER REFERENCES commentary.authors(author_id);"
)
DBI::dbExecute(
    con,
    "ALTER TABLE commentary.comments ADD COLUMN IF NOT EXISTS fourth_author_id INTEGER REFERENCES commentary.authors(author_id);"
)

# poly on table restrict share_with

DBI::dbExecute(
    con,
    "DROP TRIGGER IF EXISTS trg_user_audit ON commentary.comments;"
)
DBI::dbExecute(
    con,
    "create trigger trg_user_audit before update on commentary.comments for each row execute function user_modified()"
)
DBI::dbExecute(
    con,
    "DROP TRIGGER IF EXISTS update_modify_time ON commentary.comments;"
)
DBI::dbExecute(
    con,
    "create trigger update_modify_time before update on commentary.comments for each row execute function update_modified()"
)

DBI::dbExecute(
    con,
    "DROP TRIGGER IF EXISTS update_comments_modified ON commentary.comments;"
)
DBI::dbExecute(
    con,
    "CREATE TRIGGER update_comments_modified BEFORE UPDATE ON commentary.comments FOR EACH ROW EXECUTE FUNCTION public.update_modified()"
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

archive_dir <- "//env-fs/env-data/corp/water/Hydrology/03_Reporting/Conditions/tabular_internal_reports/Archive"


#' Run archive comment upload workflow
#'
#' @param con DBI connection.
#' @param archive_dir Character path to archive root directory.
#' @param remaining_upload_slots Integer or Inf maximum rows to upload.
#' @param show_progress Logical; display progress bar when TRUE.
#'
#' @return List containing scrape workflow results.
#' @noRd
run_archived_comment_upload_workflow <- function(
    con,
    archive_dir,
    remaining_upload_slots = Inf,
    show_progress = TRUE
) {
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
         END,
         document_type_id
         LIMIT 1;"
    )

    if (nrow(document_type_lookup) == 0) {
        stop(
            "Unable to find a matching document type for HydrometricReport comments."
        )
    }

    author_lookup <- DBI::dbGetQuery(
        con,
        "SELECT
            author_id,
            trim(concat_ws(' ', first_name, last_name)) AS author,
            first_name,
            last_name
         FROM commentary.authors
         ORDER BY author_id;"
    )
    author_lookup$author <- as.character(author_lookup$author)

    existing_comments <- DBI::dbGetQuery(
        con,
        paste0(
            "SELECT text_en, document_type_id, comment_category_id, public, ",
            "author_id, second_author_id, third_author_id, fourth_author_id, timestamp ",
            "FROM commentary.comments ",
            "WHERE document_type_id = ",
            document_type_lookup$document_type_id[1],
            ";"
        )
    )
    existing_keys <- if (nrow(existing_comments) > 0) {
        build_comment_key(existing_comments)
    } else {
        character(0)
    }

    scrape_comments_from_archived_conditions(
        archive_dir = archive_dir,
        con = con,
        comment_category_lookup = comment_category_lookup,
        document_type_id = document_type_lookup$document_type_id[1],
        author_lookup = author_lookup,
        existing_keys = existing_keys,
        remaining_upload_slots = remaining_upload_slots,
        show_progress = show_progress
    )
}


run_archived_comment_upload_workflow(
    con = con,
    archive_dir = archive_dir,
    remaining_upload_slots = Inf,
    show_progress = TRUE
)
