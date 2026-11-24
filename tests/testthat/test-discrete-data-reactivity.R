# Synthetic fixtures and real mirai transport; no database access is required.
disc_data_test_module <- function() {
    module <- new.env(parent = asNamespace("YGwater"))
    for (file in c(
        "cache_functions.R",
        "client/plot/discretePlot.R",
        "client/data/discreteData.R"
    )) {
        sys.source(
            system.file("apps/aqualink/modules", file, package = "YGwater"),
            module
        )
    }
    module$config <- list(sidebar_bg = "#ffffff")
    module$application_notifications_ui <- function(...) NULL
    module
}

disc_data_fixture <- function() {
    list(
        locs = data.frame(
            location_id = 1:2,
            name = c("One", "Two"),
            name_fr = c("Un", "Deux")
        ),
        sub_locs = data.frame(
            location_id = 1L,
            sub_location_id = 10L,
            sub_location_name = "Sub",
            sub_location_name_fr = "Sous"
        ),
        params = data.frame(
            parameter_id = 101:102,
            param_name = c("A", "B"),
            param_name_fr = c("A", "B")
        ),
        media = data.frame(
            media_id = 1:2,
            media_type = c("Water", "Soil"),
            media_type_fr = c("Eau", "Sol")
        ),
        sample_types = data.frame(
            sample_type_id = 1:2,
            sample_type = c("Regular", "Blank"),
            sample_type_fr = c("Normal", "Blanc")
        ),
        samples = data.frame(
            sample_id = 1:4,
            location_id = c(1L, 1L, 2L, 2L),
            sub_location_id = c(10L, NA, NA, NA),
            media_id = c(1L, 2L, 1L, 1L),
            sample_type = c(1L, 2L, 1L, 1L),
            datetime = as.POSIXct(
                c(
                    "2026-10-01 12:00:00",
                    "2026-10-02 23:59:59",
                    "2026-10-02 23:59:59",
                    "2026-10-03 00:00:00"
                ),
                tz = "UTC"
            )
        ),
        range = data.frame(
            min_date = as.POSIXct("2026-10-01", tz = "UTC"),
            max_date = as.POSIXct("2026-10-03", tz = "UTC")
        ),
        parameter_relationships = data.frame(
            parameter_id = 101:102,
            group_id = 1:2,
            sub_group_id = 1:2
        ),
        param_groups = data.frame(
            group_id = 1:2,
            group_name = c("A", "B"),
            group_name_fr = c("A", "B")
        ),
        param_sub_groups = data.frame(
            sub_group_id = 1:2,
            sub_group_name = c("A", "B"),
            sub_group_name_fr = c("A", "B")
        ),
        locations_projects = data.frame(location_id = 1:2, project_id = 1:2),
        projects = data.frame(
            project_id = 1:2,
            name = c("A", "B"),
            name_fr = c("A", "B")
        ),
        locations_networks = data.frame(location_id = 1:2, network_id = 1:2),
        networks = data.frame(
            network_id = 1:2,
            name = c("A", "B"),
            name_fr = c("A", "B")
        )
    )
}

disc_data_wait_task <- function(session, task) {
    until <- Sys.time() + 15
    repeat {
        later::run_now(0.05)
        session$flushReact()
        if (shiny::isolate(task$status()) != "running") {
            break
        }
        if (Sys.time() > until) {
            stop("Task timed out")
        }
    }
    session$flushReact()
}

test_that("discrete catalogue filtering combines constraints and includes the last UTC day", {
    module <- disc_data_test_module()
    cached <- disc_data_fixture()
    state <- list(
        date_range = as.Date(c("2026-10-01", "2026-10-02")),
        locations = "all",
        sub_locations = "all",
        media = "all",
        sample_types = "all",
        params = "all"
    )
    result <- module$disc_data_filter_catalogue(cached, state)
    expect_identical(result$data$samples$sample_id, 1:3)
    state$locations <- "1"
    state$media <- "2"
    state$sample_types <- "2"
    result <- module$disc_data_filter_catalogue(cached, state)
    expect_identical(result$data$samples$sample_id, 2L)
    expect_identical(result$data$locs$location_id, 1:2)
    state$locations <- "all"
    state$media <- "all"
    state$sample_types <- "all"
    expect_equal(
        nrow(module$disc_data_filter_catalogue(cached, state)$data$samples),
        3L
    )
    state$locations <- character()
    expect_equal(
        nrow(module$disc_data_filter_catalogue(cached, state)$data$samples),
        0L
    )
    expect_equal(nrow(cached$samples), 4L)
})

test_that("empty and invalid discrete ID predicates cannot produce invalid SQL", {
    module <- disc_data_test_module()
    expect_identical(
        module$disc_data_id_predicate("s.location_id", "all"),
        "TRUE"
    )
    expect_identical(
        module$disc_data_id_predicate("s.location_id", character()),
        "FALSE"
    )
    expect_identical(
        module$disc_data_id_predicate("s.location_id", c(NA, "bad", -1)),
        "FALSE"
    )
    expect_identical(
        module$disc_data_id_predicate("s.location_id", c(1, 2, 2)),
        "s.location_id IN (1,2)"
    )
})

test_that("sample workers preserve null sub-locations and close read-only transactions on failure", {
    module <- disc_data_test_module()
    calls <- character()
    sql <- NULL
    local_mocked_bindings(
        AquaConnect = function(...) list(),
        .package = "YGwater"
    )
    local_mocked_bindings(
        dbBegin = function(...) {
            calls <<- c(calls, "begin")
            TRUE
        },
        dbExecute = function(conn, statement, ...) {
            calls <<- c(calls, statement)
            0L
        },
        dbRollback = function(...) {
            calls <<- c(calls, "rollback")
            TRUE
        },
        dbDisconnect = function(...) {
            calls <<- c(calls, "disconnect")
            TRUE
        },
        dbQuoteLiteral = function(conn, x, ...) paste0("'", x, "'"),
        dbGetQuery = function(conn, statement, ...) {
            sql <<- statement
            data.frame(
                sample_id = integer(),
                location_id = integer(),
                note = character()
            )
        },
        .package = "DBI"
    )
    request <- list(
        kind = "samples",
        config = list(),
        parameter_ids = 101L,
        state = list(
            date_range = as.Date(c("2026-10-01", "2026-10-02")),
            locations = "all",
            sub_locations = "all",
            media = "all",
            sample_types = "all"
        )
    )
    data <- module$disc_data_run(request)
    expect_equal(nrow(data), 0L)
    expect_named(data, c("sample_id", "location_id", "note"))
    expect_false(grepl("s.sub_location_id IN", sql, fixed = TRUE))
    expect_match(sql, "s.datetime < '2026-10-03'::date", fixed = TRUE)
    expect_match(
        sql,
        "AND EXISTS (SELECT 1 FROM discrete.results r",
        fixed = TRUE
    )
    expect_identical(
        calls,
        c("begin", "SET TRANSACTION READ ONLY", "rollback", "disconnect")
    )
    calls <- character()
    local_mocked_bindings(
        dbGetQuery = function(...) stop("query failed"),
        .package = "DBI"
    )
    expect_error(module$disc_data_run(request), "query failed")
    expect_identical(tail(calls, 2L), c("rollback", "disconnect"))
})

test_that("discrete selectors and task results remain consistent during background work", {
    skip_if_not_installed("mirai")
    skip_if_not_installed("bslib")
    module <- disc_data_test_module()
    module$disc_data_run <- eval(substitute(
        function(request) {
            cached <- FIXTURE
            if (request$kind == "catalogue") {
                return(cached)
            }
            if (request$kind == "parameters") {
                return(
                    if (length(request$sample_ids)) c(101L, 102L) else integer()
                )
            }
            if (request$kind == "samples") {
                Sys.sleep(0.1)
                return(data.table::data.table(
                    sample_id = 1L,
                    location_id = 1L,
                    value = "test"
                ))
            }
            if (request$kind == "preview") {
                return(list(
                    subset = data.table::data.table(sample_id = 1L, result = 5),
                    location = data.frame(
                        location_id = 1L,
                        latitude = 60,
                        longitude = -135
                    ),
                    qaqc = disc_plot_empty_qaqc_data(),
                    rows = 1L,
                    qaqc_error = NULL
                ))
            }
            if (request$kind == "download") {
                writeLines("prepared", request$file)
                return(request$file)
            }
            stop("Unexpected kind")
        },
        list(FIXTURE = disc_data_fixture())
    ))
    updates <- list()
    module$updateSelectizeInput <- function(
        session,
        inputId,
        ...,
        server = FALSE
    ) {
        updates[[length(updates) + 1L]] <<- list(id = inputId, server = server)
        shiny::updateSelectizeInput(session, inputId, ..., server = server)
    }
    mirai::daemons(1L, .compute = "YGwater_aqualink")
    on.exit(mirai::daemons(0L, .compute = "YGwater_aqualink"), add = TRUE)
    shiny::testServer(
        module$discData,
        args = list(
            language = shiny::reactiveValues(
                language = "English",
                abbrev = "en"
            ),
            inputs = shiny::reactiveValues(
                location_target = NULL,
                location_id = NULL,
                location_request_id = NULL
            )
        ),
        {
            disc_data_wait_task(session, catalogue_task)
            disc_data_wait_task(session, parameter_task)
            session$setInputs(locations = "1")
            expect_identical(filter_state()$locations, "1")
            session$setInputs(locations = c("1", "all"))
            expect_identical(filter_state()$locations, "all")
            session$setInputs(locations = "all")
            expect_equal(nrow(filteredData$samples), 4L)
            stable_revision <- revision()
            updates_before_echo <- length(updates)
            session$setInputs(locations = "all")
            expect_identical(revision(), stable_revision)
            expect_identical(length(updates), updates_before_echo)
            expect_true(all(vapply(
                updates,
                function(update) !update$server,
                logical(1)
            )))
            for (id in c("sub_locations", "media", "sample_types")) {
                value <- switch(
                    id,
                    sub_locations = "10",
                    media = "1",
                    sample_types = "1"
                )
                do.call(session$setInputs, stats::setNames(list(value), id))
                expect_identical(filter_state()[[id]], value)
                do.call(
                    session$setInputs,
                    stats::setNames(list(c(value, "all")), id)
                )
                expect_identical(filter_state()[[id]], "all")
                do.call(session$setInputs, stats::setNames(list("all"), id))
            }
            session$setInputs(params = "101")
            session$setInputs(params = "all")
            expect_equal(nrow(filteredData$params), 2L)
            session$setInputs(filter = 1)
            expect_identical(samples_task$status(), "running")
            session$setInputs(locations = "2")
            expect_identical(filter_state()$locations, "2")
            disc_data_wait_task(session, samples_task)
            expect_null(table_data())
            session$setInputs(filter = 2)
            disc_data_wait_task(session, samples_task)
            expect_equal(nrow(table_data()), 1L)
            session$setInputs(tbl_rows_selected = 1L, view_data = 1)
            disc_data_wait_task(session, preview_task)
            expect_equal(preview_data()$rows, 1L)
            session$setInputs(modal_format = "csv", prepare_download = 1)
            disc_data_wait_task(session, download_task)
            expect_true(file.exists(download_file()))
            expect_identical(readLines(download_file()), "prepared")
            session$setInputs(modal_format = "xlsx")
            expect_null(download_file())
            session$setInputs(reset = 1)
            expect_identical(filter_state()$locations, "all")
            expect_null(table_data())
            session$setInputs(
                networks = "1",
                projects = "all",
                loc_modal_filter = 1
            )
            expect_identical(filter_state()$locations, "1")
            session$setInputs(
                networks = "1",
                projects = "2",
                loc_modal_filter = 2
            )
            expect_length(filter_state()$locations, 0L)
            session$setInputs(
                networks = "all",
                projects = "all",
                loc_modal_filter = 3
            )
            expect_identical(filter_state()$locations, "all")
            disc_data_wait_task(session, parameter_task)
            session$setInputs(
                pGrps = "2",
                pSubGrps = "all",
                param_modal_filter = 1
            )
            expect_identical(filter_state()$params, "102")
            session$setInputs(
                pGrps = "all",
                pSubGrps = "all",
                param_modal_filter = 2
            )
            expect_identical(filter_state()$params, "all")
            inputs$location_target <- "discData"
            inputs$location_id <- 2L
            inputs$location_request_id <- 1L
            session$flushReact()
            expect_identical(filter_state()$locations, "2")
            set_filter("locations", character())
            session$setInputs(locations = character())
            expect_length(filter_state()$locations, 0L)
            expect_equal(nrow(filteredData$samples), 0L)
        }
    )
})
