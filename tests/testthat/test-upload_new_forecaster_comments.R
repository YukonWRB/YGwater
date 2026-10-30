load_forecaster_upload_script <- function() {
    env <- new.env(parent = globalenv())
    sys.source(
        testthat::test_path(
            "..",
            "..",
            "dev",
            "fews",
            "upload_new_forecaster_comments.R"
        ),
        envir = env
    )
    env
}

test_that("scheduled upload discovers current and archived dated reports", {
    env <- load_forecaster_upload_script()
    root <- tempfile("forecaster_reports_")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    folders <- c(
        "2026-10-08",
        "Archive/2026-10-07",
        "not-a-date",
        "2026-02-30",
        "2026-10-06"
    )
    for (folder in folders) {
        path <- file.path(root, folder)
        dir.create(path, recursive = TRUE)
        if (folder != "2026-10-06") {
            file.create(file.path(
                path,
                paste0("HydrometricReport_", basename(folder), ".xlsx")
            ))
        }
    }
    result <- env$find_forecaster_comment_workbooks(root)
    expect_equal(
        as.character(result$report_date),
        c("2026-10-07", "2026-10-08")
    )
    expect_equal(nrow(result), 2L)
    expect_true(all(file.exists(result$workbook_path)))
    expect_error(
        env$find_forecaster_comment_workbooks(file.path(root, "missing")),
        "unavailable"
    )
})

test_that("an empty report directory fails rather than appearing successful", {
    env <- load_forecaster_upload_script()
    root <- tempfile("forecaster_reports_")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    expect_equal(nrow(env$find_forecaster_comment_workbooks(root)), 0L)
    expect_error(env$upload_new_forecaster_comments(NULL, root), "No dated")
})

test_that("scheduled uploads propagate deduplication keys and release their lock", {
    env <- load_forecaster_upload_script()
    state <- new.env()
    state$keys <- "backfilled"
    state$unlocks <- 0L
    state$transactions <- 0L
    state$fail <- FALSE
    state$locked <- FALSE
    env$find_forecaster_comment_workbooks <- function(...) {
        data.frame(
            workbook_path = c("report_a.xlsx", "report_b.xlsx"),
            report_date = as.Date(c("2026-10-07", "2026-10-08"))
        )
    }
    env$getFromNamespace <- function(name, ...) {
        if (name == "build_comment_key") {
            return(function(data) data$text_en)
        }
        function(workbook_path, existing_keys, remaining_upload_slots, ...) {
            if (state$fail && workbook_path == "report_a.xlsx") {
                stop("Workbook unavailable")
            }
            expect_true("backfilled" %in% existing_keys)
            if (!state$fail && workbook_path == "report_b.xlsx") {
                expect_true("report_a.xlsx" %in% existing_keys)
            }
            list(
                existing_keys = unique(c(existing_keys, workbook_path)),
                remaining_upload_slots = remaining_upload_slots
            )
        }
    }
    local_mocked_bindings(
        dbGetQuery = function(conn, statement, ...) {
            if (grepl("pg_try_advisory_lock", statement, fixed = TRUE)) {
                return(data.frame(acquired = !state$locked))
            }
            if (grepl("pg_advisory_unlock", statement, fixed = TRUE)) {
                state$unlocks <- state$unlocks + 1L
                return(data.frame(unlocked = TRUE))
            }
            if (
                grepl(
                    "FROM commentary.comment_categories",
                    statement,
                    fixed = TRUE
                )
            ) {
                return(data.frame(id = 1L, category_key = "current conditions"))
            }
            if (grepl("FROM files.document_types", statement, fixed = TRUE)) {
                return(data.frame(document_type_id = 1L))
            }
            if (grepl("FROM commentary.authors", statement, fixed = TRUE)) {
                return(data.frame(author_id = 1L, author = "Test Author"))
            }
            data.frame(text_en = state$keys)
        },
        dbExecute = function(...) 0L,
        dbWithTransaction = function(conn, code, ...) {
            state$transactions <- state$transactions + 1L
            result <- eval(substitute(code), parent.frame())
            state$keys <- result$existing_keys
            result
        },
        .package = "DBI"
    )

    result <- suppressMessages(env$upload_new_forecaster_comments(
        NULL,
        "reports"
    ))
    expect_equal(result$uploaded_comments, 2L)
    expect_equal(result$processed_workbooks, 2L)
    expect_equal(state$transactions, 2L)
    expect_equal(state$unlocks, 1L)
    result <- suppressMessages(env$upload_new_forecaster_comments(
        NULL,
        "reports"
    ))
    expect_equal(result$uploaded_comments, 0L)
    expect_equal(state$unlocks, 2L)

    state$fail <- TRUE
    expect_error(
        suppressMessages(env$upload_new_forecaster_comments(NULL, "reports")),
        "Some workbooks failed"
    )
    expect_equal(state$transactions, 6L)
    expect_equal(state$unlocks, 3L)

    state$locked <- TRUE
    expect_error(
        env$upload_new_forecaster_comments(NULL, "reports"),
        "already running"
    )
    expect_equal(state$unlocks, 3L)
})
