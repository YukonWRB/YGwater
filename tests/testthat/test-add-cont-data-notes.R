add_cont_data_notes_module <- function() {
  path <- system.file(
    "apps/YGwater/modules/admin/continuousData/addContData.R",
    package = "YGwater"
  )
  env <- new.env(parent = globalenv())
  sys.source(path, env)
  list(path = path, env = env)
}

test_that("upload note ranges preserve free-hand text and allow overlap", {
  module <- add_cont_data_notes_module()
  ranges <- data.frame(
    note = c("  Ice affected  ", "Logger serviced"),
    start_datetime = c("2026-01-01 00:00:00", "2026-01-05 00:00:00"),
    end_datetime = c("2026-01-10 00:00:00", "2026-01-06 00:00:00")
  )

  notes <- module$env$add_cont_data_prepare_notes(
    ranges,
    no_source_update = TRUE
  )

  expect_equal(nrow(notes), 2L)
  expect_equal(notes$note, c("Ice affected", "Logger serviced"))
  expect_s3_class(notes$start_dt, "POSIXct")
  expect_s3_class(notes$end_dt, "POSIXct")
  expect_true(all(notes$no_source_update))
})

test_that("upload note ranges reject blank notes and invalid datetimes", {
  module <- add_cont_data_notes_module()
  expect_error(
    module$env$add_cont_data_prepare_notes(data.frame(
      note = " ",
      start_datetime = "2026-01-01 00:00:00",
      end_datetime = "2026-01-02 00:00:00"
    )),
    "row(s): 1",
    fixed = TRUE
  )
  expect_error(
    module$env$add_cont_data_prepare_notes(data.frame(
      note = "Reversed",
      start_datetime = "2026-01-02 00:00:00",
      end_datetime = "2026-01-01 00:00:00"
    )),
    "row(s): 1",
    fixed = TRUE
  )
})

test_that("addContData exposes note range controls and atomic persistence", {
  module <- add_cont_data_notes_module()
  text <- paste(readLines(module$path, warn = FALSE), collapse = "\n")

  expect_match(text, 'id = ns("notes_panel")', fixed = TRUE)
  expect_match(text, 'ns("add_note_range")', fixed = TRUE)
  expect_match(text, 'ns("note_modal_use_data_start")', fixed = TRUE)
  expect_match(text, 'ns("note_modal_use_data_end")', fixed = TRUE)
  expect_match(text, 'textAreaInput(', fixed = TRUE)
  expect_match(text, "AquaCache::adjust_note(", fixed = TRUE)
  expect_match(text, "DBI::dbBegin(con)", fixed = TRUE)
  expect_match(text, "DBI::dbCommit(con)", fixed = TRUE)
  expect_match(text, "note_ranges(empty_note_range_df())", fixed = TRUE)
})
