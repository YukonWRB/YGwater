test_that("continuous plot note windows follow the requested dates", {
  timeseries_windows <- continuous_plot_note_windows(list(
    plot_type = "timeseries",
    plot_timezone = "UTC",
    start_date = as.Date("2026-01-01"),
    end_date = as.Date("2026-01-02")
  ))
  expect_equal(
    timeseries_windows$start_dt,
    as.POSIXct("2026-01-01", tz = "UTC")
  )
  expect_equal(
    timeseries_windows$end_dt,
    as.POSIXct("2026-01-03", tz = "UTC")
  )

  overlap_windows <- continuous_plot_note_windows(list(
    plot_type = "overlap_yrs",
    plot_timezone = "UTC",
    years = c(2024L, 2025L),
    start_day = as.Date("2026-11-01"),
    end_day = as.Date("2027-04-30")
  ))
  expect_equal(nrow(overlap_windows), 2L)
  expect_equal(
    overlap_windows$start_dt,
    as.POSIXct(c("2024-11-01", "2025-11-01"), tz = "UTC")
  )
  expect_equal(
    overlap_windows$end_dt,
    as.POSIXct(c("2025-05-01", "2026-05-01"), tz = "UTC")
  )
})

test_that("continuous plot exports always include the fetched notes table", {
  request <- list(
    plot_type = "timeseries_all",
    timeseries_ids = c(1L, 2L),
    plot_timezone = "UTC",
    plot_resolution = "daily",
    lang = "en"
  )
  notes <- data.frame(
    note_id = 9L,
    timeseries_id = 1L,
    location = "Test location",
    parameter = "Water level",
    note = "Ice affected",
    start_datetime_utc = as.POSIXct("2026-01-01", tz = "UTC"),
    end_datetime_utc = as.POSIXct("2026-01-15", tz = "UTC")
  )

  tables <- continuous_plot_export_tables(
    req = request,
    out = list(trace_data = data.frame(value = 1)),
    module_data = list(),
    language = list(language = "English"),
    notes = notes
  )

  expect_named(tables, c("metadata", "data_trace_data", "notes"))
  expect_equal(tables$notes$note, "Ice affected")
})

test_that("continuous plot notes are filtered to the exact plot windows", {
  query_count <- 0L
  fake_db_get_query <- function(conn, statement, params = NULL, ...) {
    query_count <<- query_count + 1L
    data.frame(
      note_id = 1:4,
      timeseries_id = rep(17L, 4L),
      location = rep("Test location", 4L),
      parameter = rep("Water level", 4L),
      note = c(
        "First winter",
        "Summer gap",
        "Second winter",
        "Begins at exclusive boundary"
      ),
      start_datetime_utc = as.POSIXct(
        c("2025-01-01", "2025-06-01", "2026-01-01", "2025-05-01"),
        tz = "UTC"
      ),
      end_datetime_utc = as.POSIXct(
        c("2025-01-31", "2025-06-30", "2026-01-31", "2025-10-31"),
        tz = "UTC"
      )
    )
  }

  notes <- testthat::with_mocked_bindings(
    fetch_continuous_plot_notes(
      structure(list(), class = "test_connection"),
      list(
        plot_type = "overlap_yrs",
        plot_timezone = "UTC",
        timeseries_ids = 17L,
        years = c(2024L, 2025L),
        start_day = as.Date("2026-11-01"),
        end_day = as.Date("2027-04-30")
      )
    ),
    dbGetQuery = fake_db_get_query,
    .package = "DBI"
  )

  expect_equal(query_count, 1L)
  expect_equal(notes$note_id, c(1L, 3L))
})

test_that("continuous Shiny modules expose notes in alerts and downloads", {
  plot_module <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/client/plot/continuousPlotAdaptive.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  data_module <- paste(
    readLines(
      system.file(
        "apps/YGwater/modules/client/data/continuousData.R",
        package = "YGwater"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(plot_module, 'uiOutput(ns("notes_alert"))', fixed = TRUE)
  expect_match(plot_module, 'ns("show_plot_notes")', fixed = TRUE)
  expect_match(
    plot_module,
    "YGwater:::fetch_continuous_plot_notes",
    fixed = TRUE
  )
  expect_match(plot_module, "notes = isolate(plot_notes())", fixed = TRUE)
  expect_match(data_module, "FROM continuous.notes n", fixed = TRUE)
  expect_match(data_module, "notes = dbGetQueryDT(", fixed = TRUE)
  expect_false(grepl("notes_available", data_module, fixed = TRUE))
})
