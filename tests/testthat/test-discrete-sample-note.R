test_that("optional sample notes stay empty in the form and missing on upload", {
  env <- new.env(parent = asNamespace("YGwater"))
  sys.source(
    system.file(
      "apps/aqualink/modules/admin/discreteData/addDiscData.R",
      package = "YGwater"
    ),
    envir = env
  )

  # Evaluate the actual form, save, and SQL-parameter expressions without a
  # Shiny session or database connection.
  find_calls <- function(expr, predicate) {
    if (!is.call(expr) && !is.expression(expr) && !is.pairlist(expr)) {
      return(list())
    }
    hits <- if (is.call(expr) && predicate(expr)) list(expr) else list()
    c(
      hits,
      unlist(lapply(as.list(expr), find_calls, predicate), recursive = FALSE)
    )
  }
  note_input <- find_calls(body(env$addDiscData), function(x) {
    identical(x[[1]], quote(textAreaInput)) &&
      identical(x[[2]], quote(ns("edit_sample_note")))
  })
  expect_length(note_input, 1L)
  env$ns <- identity
  env$textAreaInput <- shiny::textAreaInput
  save_values <- find_calls(body(env$addDiscData), function(x) {
    identical(x[[1]], quote(list)) && "sample_note" %in% names(x)
  })
  expect_length(save_values, 1L)
  note_parameter <- find_calls(body(env$addDiscData_run_upload), function(x) {
    identical(x[[1]], quote(`if`)) &&
      identical(
        x[[2]],
        quote(addDiscData_present(samples$sample_note[[i]]))
      )
  })
  expect_length(note_parameter, 1L)

  for (note in list(NA_character_, "", " \t\n", "Collected under ice")) {
    env$row <- data.frame(sample_note = note)
    widget <- eval(note_input[[1]], env)
    textarea <- widget$children[[2]]
    expected <- if (env$addDiscData_present(note)) note else ""
    expect_identical(textarea$children[[1]], expected)
    expect_identical(textarea$attribs$placeholder, "Optional sample note")

    env$input <- list(edit_sample_note = expected)
    saved <- eval(save_values[[1]]$sample_note, env)
    expect_identical(saved, if (nzchar(expected)) expected else NA_character_)
    env$samples <- data.frame(sample_note = saved, source_sample_id = "lab-1")
    env$i <- 1L
    env$file_sample <- FALSE
    # DBI binds NA_character_ as SQL NULL; no literal "NA" is generated.
    expect_identical(eval(note_parameter[[1]], env), saved)
  }

  # Retain import provenance when a file sample has no note.
  env$samples$sample_note <- NA_character_
  env$file_sample <- TRUE
  expect_identical(
    eval(note_parameter[[1]], env),
    "Imported source sample: lab-1"
  )
})
