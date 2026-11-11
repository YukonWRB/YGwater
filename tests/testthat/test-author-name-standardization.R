test_that("raw author preserves the workbook cell text", {
    author_cell <- data.frame(
        X1 = "A. Bier & EJ Bercier",
        stringsAsFactors = FALSE
    )
    author_lookup <- data.frame(
        author = c("A. Bier", "EJ Bercier"),
        author_id = c(1L, 2L),
        stringsAsFactors = FALSE
    )

    author_details <- resolve_author_details(author_cell, author_lookup)

    expect_identical(author_details$raw_author, "A. Bier & EJ Bercier")
    expect_identical(author_details$author, "A. Bier and EJ Bercier")
    expect_identical(
        author_details$author_ids,
        c(1L, 2L, NA_integer_, NA_integer_)
    )
})

test_that("author aliases include full initialisms", {
    aliases <- build_author_name_aliases("EJ", "Bercier")
    lookup <- data.frame(
        author = aliases,
        author_id = rep(2L, length(aliases)),
        stringsAsFactors = FALSE
    )

    expect_true(all(c("EJB", "EJ B", "E.J.B", "E.J.B.") %in% aliases))
    expect_identical(map_author_ids("EJB", lookup)[[1L]], 2L)
})

test_that("author matching ignores periods and spaces", {
    lookup <- data.frame(
        author = c("A Bier", "EJ Bercier"),
        author_id = c(1L, 2L),
        stringsAsFactors = FALSE
    )

    expect_identical(map_author_ids("A.Bier", lookup)[[1L]], 1L)
    expect_identical(map_author_ids("A. Bier", lookup)[[1L]], 1L)
})

test_that("ambiguous normalized aliases are not assigned", {
    lookup <- data.frame(
        author = c("A.Bier", "A Bier"),
        author_id = c(1L, 2L),
        stringsAsFactors = FALSE
    )

    expect_true(is.na(map_author_ids("A Bier", lookup)[[1L]]))
})

test_that("reruns refresh existing comment author metadata", {
    captured <- new.env(parent = emptyenv())
    local_mocked_bindings(
        dbGetQuery = function(conn, statement, params = NULL, ...) {
            captured$sql <- statement
            captured$params <- params
            data.frame(id = 42L)
        },
        .package = "DBI"
    )

    comment <- data.frame(
        raw_author = "AB",
        author_id = 1L,
        second_author_id = NA_integer_,
        third_author_id = NA_integer_,
        fourth_author_id = NA_integer_,
        text_en = "River conditions are stable.",
        document_type_id = 2L,
        comment_category_id = 3L,
        public = FALSE,
        timestamp = as.POSIXct("2026-10-08", tz = "UTC"),
        location_id = NA_integer_,
        stringsAsFactors = FALSE
    )

    result <- refresh_existing_comment_authors(NULL, comment)
    expect_true(result$matched)
    expect_true(result$updated)
    expect_match(captured$sql, "UPDATE commentary\\.comments")
    expect_match(captured$sql, "WHERE id = \\$6")
    expect_false(grepl("author_id = \\$", captured$sql))
    expect_equal(captured$params[[1]], "AB")
    expect_equal(captured$params[[2]], 1L)
    expect_equal(captured$params[[6]], 42L)
})

test_that("author refresh skips ambiguous duplicate comment identities", {
    queries <- 0L
    local_mocked_bindings(
        dbGetQuery = function(conn, statement, params = NULL, ...) {
            queries <<- queries + 1L
            data.frame(id = c(42L, 43L))
        },
        .package = "DBI"
    )

    comment <- data.frame(
        raw_author = "AB",
        author_id = 1L,
        second_author_id = NA_integer_,
        third_author_id = NA_integer_,
        fourth_author_id = NA_integer_,
        text_en = "River conditions are stable.",
        document_type_id = 2L,
        comment_category_id = 3L,
        public = FALSE,
        timestamp = as.POSIXct("2026-10-08", tz = "UTC"),
        location_id = NA_integer_,
        stringsAsFactors = FALSE
    )

    result <- refresh_existing_comment_authors(NULL, comment)
    expect_true(result$matched)
    expect_false(result$updated)
    expect_equal(queries, 1L)
})
