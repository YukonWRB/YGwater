con <- YGwater::AquaConnect(
    name = "aquacache",
    host = Sys.getenv("aquacacheHostDev"),
    port = Sys.getenv("aquacachePortDev"),
    user = Sys.getenv("aquacacheAdminUser"),
    password = Sys.getenv("aquacacheAdminPass"),
)

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
    name TEXT NOT NULL,
    description TEXT,
    created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    created_by TEXT DEFAULT CURRENT_USER NOT NULL,
    modified TIMESTAMP WITH TIME ZONE,
    modified_by TEXT
    );"
)

DBI::dbExecute(
    con,
    "create trigger trg_user_audit before update on commentary.comment_categories for each row execute function user_modified()"
)
DBI::dbExecute(
    con,
    "create trigger update_modify_time before update on commentary.comment_categories for each row execute function update_modified()"
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
                author TEXT,
                timestamp TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                created TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
                created_by TEXT DEFAULT CURRENT_USER NOT NULL,
                modified TIMESTAMP WITH TIME ZONE,
                modified_by TEXT
                );"
)

# poly on table restrict share_with

DBI::dbExecute(
    con,
    "create trigger trg_user_audit before update on commentary.comments for each row execute function user_modified()"
)
DBI::dbExecute(
    con,
    "create trigger update_modify_time before update on commentary.comments for each row execute function update_modified()"
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
    "COMMENT ON COLUMN commentary.comments.timestamp IS 'Timestamp for the comments; might differ from the created timestamp.';"
)


archive_dir <- "//env-fs/env-data/corp/water/Hydrology/03_Reporting/Conditions/tabular_internal_reports/Archive"

folders <- list.dirs(archive_dir, full.names = FALSE, recursive = FALSE)


standardize_param_name <- function(name) {
    # replace spaces with underscores and convert to lowercase
    if (name %in% c("bridges", "bridge")) {
        ret <- "bridges"
    } else {
        ret <- name
    }
    return(ret)
}


comments <- data.frame(
    timestamp = as.POSIXct(character()),
    author = character(),
    comment = character(),
    category = character(),
    stringsAsFactors = FALSE
)

for (j in folders) {
    # open workbook
    workbook <- openxlsx::loadWorkbook(
        paste(
            archive_dir,
            j,
            paste0("HydrometricReport_", j, ".xlsx"),
            sep = "/"
        )
    )
    # for each sheet in workbook
    for (k in names(workbook)) {
        # standardize param name to handle variations in sheet naming (e.g. "precipitation" vs "precip", "bridges" vs "bridge", etc.)

        param_name <- standardize_param_name(k)

        # get author name - often newer sheets only have author name in 'comments' sheet, so we'll check there if we don't find it in the current sheet
        author_name <- as.character(openxlsx::read.xlsx(
            workbook,
            sheet = k,
            rows = 1,
            cols = 5,
            colNames = FALSE
        ))

        # if no author name is found, check the "comments" sheet (available in newer reports) for an author name
        if (length(author_name) == 0 && "comments" %in% names(workbook)) {
            author_name <- as.character(openxlsx::read.xlsx(
                workbook,
                sheet = "comments",
                rows = 1,
                cols = 5,
                colNames = FALSE
            ))
        }

        if (length(author_name) == 0) {
            author_name <- NA_character_
        }

        # if the sheet is "comments", look for comments in the standard location (row 3, column 13) for forecast conditions and (row 12, column 2) for current conditions
        if (param_name == "comments") {
            comment_text <- as.character(openxlsx::read.xlsx(
                workbook,
                sheet = k,
                rows = 12,
                cols = 2,
                colNames = FALSE
            ))
            if (length(comment_text) > 0 && !all(is.na(comment_text))) {
                comments <- rbind(
                    comments,
                    data.frame(
                        timestamp = j,
                        author = author_name,
                        comment = comment_text,
                        category = "current conditions",
                        stringsAsFactors = FALSE
                    )
                )
            }

            comment_text <- as.character(openxlsx::read.xlsx(
                workbook,
                sheet = k,
                rows = 3,
                cols = 13,
                colNames = FALSE
            ))
            if (length(comment_text) > 0 && !all(is.na(comment_text))) {
                comments <- rbind(
                    comments,
                    data.frame(
                        timestamp = j,
                        author = author_name,
                        comment = comment_text,
                        category = "forecast conditions",
                        stringsAsFactors = FALSE
                    )
                )
            }
        } else {
            # if the sheet is not "comments", look for comments in the standard location (row 3, column 2)
            comment_text <- as.character(openxlsx::read.xlsx(
                workbook,
                sheet = k,
                rows = 3,
                cols = 2,
                colNames = FALSE
            ))

            if (length(comment_text) == 0) {
                comment_text <- NA_character_
            }

            if (length(comment_text) > 0 && !all(is.na(comment_text))) {
                comments <- rbind(
                    comments,
                    data.frame(
                        timestamp = j,
                        author = author_name,
                        comment = comment_text,
                        category = param_name,
                        stringsAsFactors = FALSE
                    )
                )
            }
        }
    }
}

author_counts <- as.data.frame(
    sort(table(comments$author), decreasing = TRUE),
    stringsAsFactors = FALSE
)
names(author_counts) <- c("author", "n")
author_counts <- author_counts[
    !is.na(author_counts$author) & author_counts$author != "",
]

author_histogram <- ggplot2::ggplot(
    author_counts,
    ggplot2::aes(x = stats::reorder(author, n), y = n)
) +
    ggplot2::geom_col(fill = "#2C7FB8") +
    ggplot2::coord_flip() +
    ggplot2::labs(
        title = "Comments by author",
        x = "Author",
        y = "Number of comments"
    ) +
    ggplot2::theme_minimal()
