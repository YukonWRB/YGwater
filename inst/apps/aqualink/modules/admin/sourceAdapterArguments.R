source_adapter_capability_row <- function(adapters, source_fx) {
  if (
    is.null(adapters) ||
      !nrow(adapters) ||
      is.null(source_fx) ||
      !length(source_fx) ||
      is.na(source_fx[[1]]) ||
      !nzchar(source_fx[[1]])
  ) {
    return(NULL)
  }
  selected_source_fx <- as.character(source_fx[[1]])
  keep <- adapters[["source_fx"]] == selected_source_fx
  row <- adapters[which(keep), , drop = FALSE]
  if (nrow(row) != 1L) {
    return(NULL)
  }
  row
}

source_adapter_argument_schema <- function(capability) {
  if (is.null(capability) || nrow(capability) != 1L) {
    return(list(schema_version = 1L, arguments = list()))
  }
  schema <- capability$argument_schema[[1]]
  if (is.null(schema$arguments)) {
    schema$arguments <- list()
  }
  schema
}

source_adapter_decode_args <- function(value) {
  if (
    is.null(value) || !length(value) || all(is.na(value)) || !nzchar(value[[1]])
  ) {
    return(list())
  }
  parsed <- jsonlite::fromJSON(value[[1]], simplifyVector = TRUE)
  if (!is.list(parsed) || is.null(names(parsed))) {
    stop("Stored source-function arguments must be a JSON object.")
  }
  parsed
}

source_adapter_argument_input_id <- function(argument_name, input_prefix = "") {
  paste0(
    input_prefix,
    "source_arg_",
    gsub("[^A-Za-z0-9_]", "_", argument_name)
  )
}

source_adapter_logical_value <- function(value) {
  if (is.logical(value)) {
    return(isTRUE(value[[1]]))
  }
  if (is.numeric(value)) {
    return(length(value) > 0L && !is.na(value[[1]]) && value[[1]] != 0)
  }
  length(value) > 0L &&
    tolower(trimws(as.character(value[[1]]))) %in%
      c("true", "t", "1", "yes", "y")
}

source_adapter_argument_control <- function(
  ns,
  argument,
  existing_args,
  input_prefix = ""
) {
  name <- argument$name
  input_id <- ns(source_adapter_argument_input_id(name, input_prefix))
  label <- paste0(argument$label, if (isTRUE(argument$required)) " *" else "")
  value <- if (name %in% names(existing_args)) {
    existing_args[[name]]
  } else if (!is.null(argument$default)) {
    argument$default
  } else {
    NULL
  }
  choices <- if (is.null(argument$choices)) {
    NULL
  } else {
    unlist(argument$choices, recursive = TRUE, use.names = FALSE)
  }

  control <- switch(
    argument$control,
    text = shiny::textInput(
      input_id,
      label,
      value = if (is.null(value)) "" else paste(value, collapse = ", "),
      width = "100%"
    ),
    password = shiny::passwordInput(
      input_id,
      label,
      value = if (is.null(value)) "" else as.character(value[[1]]),
      width = "100%"
    ),
    numeric = shiny::numericInput(
      input_id,
      label,
      value = if (is.null(value) || !length(value)) {
        NA
      } else {
        as.numeric(value[[1]])
      },
      min = if (is.null(argument$minimum)) NA else argument$minimum,
      max = if (is.null(argument$maximum)) NA else argument$maximum,
      step = if (is.null(argument$step)) NA else argument$step,
      width = "100%"
    ),
    checkbox = shiny::checkboxInput(
      input_id,
      label,
      value = if (is.null(value) || !length(value)) {
        FALSE
      } else {
        source_adapter_logical_value(value)
      }
    ),
    select = shiny::selectizeInput(
      input_id,
      label,
      choices = choices,
      selected = if (is.null(value)) character() else as.character(value[[1]]),
      multiple = FALSE,
      options = list(placeholder = "Select a value"),
      width = "100%"
    ),
    multiselect = shiny::selectizeInput(
      input_id,
      label,
      choices = choices,
      selected = if (is.null(value)) {
        character()
      } else {
        as.character(unlist(value))
      },
      multiple = TRUE,
      width = "100%"
    ),
    stop("Unsupported source-adapter control: ", argument$control)
  )

  shiny::tagList(
    control,
    if (!is.null(argument$help) && nzchar(argument$help)) {
      shiny::tags$p(class = "text-muted small", argument$help)
    }
  )
}

source_adapter_argument_ui <- function(
  ns,
  capability,
  stored_args = NA_character_,
  input_prefix = ""
) {
  schema <- source_adapter_argument_schema(capability)
  existing_args <- tryCatch(
    source_adapter_decode_args(stored_args),
    error = function(e) list()
  )
  arguments <- schema$arguments
  user_arguments <- Filter(
    function(argument) identical(argument$source, "user"),
    arguments
  )
  normal_arguments <- Filter(
    function(argument) !isTRUE(argument$advanced),
    user_arguments
  )
  advanced_arguments <- Filter(
    function(argument) isTRUE(argument$advanced),
    user_arguments
  )
  managed_arguments <- Filter(
    function(argument) !identical(argument$source, "user"),
    arguments
  )
  catalogued_names <- vapply(arguments, `[[`, character(1), "name")
  unknown_names <- setdiff(names(existing_args), catalogued_names)
  unknown_args <- existing_args[unknown_names]

  managed_labels <- c(
    runtime = "AquaCache/runtime",
    environment = "Function default/environment",
    internal = "Internal/default"
  )
  managed_ui <- if (length(managed_arguments)) {
    shiny::tags$details(
      class = "mt-3",
      open = NA,
      shiny::tags$summary("Managed arguments (read-only)"),
      shiny::tags$p(
        class = "text-muted small mt-2",
        "These function parameters are intentionally not user-defined."
      ),
      shiny::tags$table(
        class = "table table-sm table-striped",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th("Argument"),
          shiny::tags$th("Provided by"),
          shiny::tags$th("How it is supplied")
        )),
        shiny::tags$tbody(lapply(managed_arguments, function(argument) {
          shiny::tags$tr(
            shiny::tags$td(shiny::tags$code(argument$name)),
            shiny::tags$td(unname(managed_labels[[argument$source]])),
            shiny::tags$td(argument$help)
          )
        }))
      )
    )
  }

  shiny::tagList(
    if (length(normal_arguments)) {
      lapply(
        normal_arguments,
        source_adapter_argument_control,
        ns = ns,
        existing_args = existing_args,
        input_prefix = input_prefix
      )
    } else {
      shiny::tags$div(
        class = "alert alert-secondary",
        "This adapter has no user-defined source arguments."
      )
    },
    if (length(advanced_arguments) || length(unknown_args)) {
      shiny::tags$details(
        class = "mt-3",
        shiny::tags$summary("Advanced arguments"),
        if (length(advanced_arguments)) {
          lapply(
            advanced_arguments,
            source_adapter_argument_control,
            ns = ns,
            existing_args = existing_args,
            input_prefix = input_prefix
          )
        },
        if (length(unknown_args)) {
          shiny::textAreaInput(
            ns(paste0(input_prefix, "source_args_uncatalogued")),
            "Uncatalogued stored arguments",
            value = jsonlite::toJSON(
              unknown_args,
              auto_unbox = TRUE,
              pretty = TRUE,
              null = "null"
            ),
            rows = 5,
            width = "100%"
          )
        },
        if (length(unknown_args)) {
          shiny::tags$p(
            class = "text-muted small",
            "These keys are preserved because they are not present in the current catalogue."
          )
        }
      )
    },
    managed_ui
  )
}

source_adapter_collect_args <- function(
  input,
  capability,
  stored_args = NA_character_,
  input_prefix = ""
) {
  schema <- source_adapter_argument_schema(capability)
  arguments <- schema$arguments
  existing_args <- source_adapter_decode_args(stored_args)
  catalogued_names <- vapply(arguments, `[[`, character(1), "name")
  output <- existing_args[setdiff(names(existing_args), catalogued_names)]

  uncatalogued <- if (length(output)) {
    input[[paste0(input_prefix, "source_args_uncatalogued")]]
  } else {
    NULL
  }
  if (!is.null(uncatalogued) && nzchar(trimws(uncatalogued))) {
    parsed_unknown <- jsonlite::fromJSON(
      uncatalogued,
      simplifyVector = TRUE
    )
    if (!is.list(parsed_unknown) || is.null(names(parsed_unknown))) {
      stop("Uncatalogued arguments must be a JSON object.")
    }
    output <- parsed_unknown
  }

  user_arguments <- Filter(
    function(argument) identical(argument$source, "user"),
    arguments
  )
  for (argument in user_arguments) {
    name <- argument$name
    value <- input[[source_adapter_argument_input_id(name, input_prefix)]]
    missing <- is.null(value) ||
      !length(value) ||
      (is.character(value) && !any(nzchar(trimws(value)))) ||
      (is.numeric(value) && all(is.na(value)))
    if (missing) {
      if (isTRUE(argument$required)) {
        stop("Source argument '", argument$label, "' is required.")
      }
      next
    }

    value <- switch(
      argument$value_type,
      character = as.character(value[[1]]),
      numeric = as.numeric(value[[1]]),
      integer = as.integer(value[[1]]),
      logical = source_adapter_logical_value(value),
      character_vector = {
        if (length(value) == 1L && grepl(",", value, fixed = TRUE)) {
          trimws(strsplit(value, ",", fixed = TRUE)[[1]])
        } else {
          as.character(value)
        }
      },
      numeric_vector = {
        if (length(value) == 1L && grepl(",", value, fixed = TRUE)) {
          value <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
        }
        as.numeric(value)
      },
      stop("Unsupported value_type for source argument '", name, "'.")
    )
    if (anyNA(value)) {
      stop("Source argument '", argument$label, "' has an invalid value.")
    }
    choices <- if (is.null(argument$choices)) NULL else unlist(argument$choices)
    if (
      !is.null(choices) && any(!as.character(value) %in% as.character(choices))
    ) {
      stop(
        "Source argument '",
        argument$label,
        "' is outside its allowed choices."
      )
    }
    if (!is.null(argument$minimum) && any(value < argument$minimum)) {
      stop("Source argument '", argument$label, "' is below its minimum.")
    }
    if (!is.null(argument$maximum) && any(value > argument$maximum)) {
      stop("Source argument '", argument$label, "' is above its maximum.")
    }
    output[[name]] <- value
  }
  output
}

source_adapter_args_json <- function(args) {
  if (is.null(args) || !length(args)) {
    return(NA_character_)
  }
  as.character(jsonlite::toJSON(
    args,
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  ))
}

source_adapter_args_equal <- function(left, right) {
  normalize <- function(value) {
    parsed <- source_adapter_decode_args(value)
    if (!length(parsed)) {
      return(list())
    }
    parsed[sort(names(parsed))]
  }
  identical(normalize(left), normalize(right))
}

# Run a source adapter over an explicit, read-only test window. The caller is
# responsible for supplying a read-only database connection because adapters
# may use it to resolve metadata such as transmission routes and mappings.
source_adapter_test <- function(
  source_fx,
  source_fx_args,
  start_datetime,
  end_datetime,
  con,
  source_function = NULL
) {
  source_fx <- trimws(as.character(source_fx))
  if (length(source_fx) != 1L || is.na(source_fx) || !nzchar(source_fx)) {
    stop("Select a source function to test.")
  }

  parse_datetime <- function(value, label) {
    if (inherits(value, "POSIXt")) {
      value <- as.POSIXct(value, tz = "UTC")
    } else {
      value <- suppressWarnings(as.POSIXct(
        trimws(as.character(value)),
        tz = "UTC"
      ))
    }
    if (length(value) != 1L || is.na(value)) {
      stop(label, " must be a valid UTC datetime.")
    }
    value
  }

  start_datetime <- parse_datetime(start_datetime, "Start datetime")
  end_datetime <- parse_datetime(end_datetime, "End datetime")
  if (start_datetime >= end_datetime) {
    stop("Start datetime must precede end datetime.")
  }

  if (is.null(source_function)) {
    source_function <- get(
      source_fx,
      envir = asNamespace("AquaCache"),
      inherits = FALSE
    )
  }
  if (!is.function(source_function)) {
    stop("The selected source adapter is not callable.")
  }

  args <- source_adapter_decode_args(source_fx_args)
  function_args <- names(formals(source_function))
  accepts_dots <- "..." %in% function_args
  add_runtime_arg <- function(name, value) {
    if (accepts_dots || name %in% function_args) {
      args[[name]] <<- value
    }
  }
  add_runtime_arg("start_datetime", start_datetime)
  add_runtime_arg("end_datetime", end_datetime)
  add_runtime_arg("con", con)

  # Transmission import functions can record import runs and measurements.
  # A test must always override that behaviour, including for legacy stored
  # arguments that may contain a write flag.
  if (accepts_dots || "write" %in% function_args) {
    args$write <- FALSE
  }

  do.call(source_function, args)
}

# Produce a bounded, human-readable preview suitable for a Shiny modal. Large
# provider responses are summarized and truncated so they do not overwhelm the
# browser or the future-process serialization boundary.
source_adapter_test_result_text <- function(result, max_rows = 100L) {
  max_rows <- suppressWarnings(as.integer(max_rows))
  if (length(max_rows) != 1L || is.na(max_rows) || max_rows < 1L) {
    stop("max_rows must be a positive integer.")
  }

  table_text <- function(value) {
    value <- as.data.frame(value)
    heading <- paste0(nrow(value), " row(s) x ", ncol(value), " column(s)")
    preview <- utils::head(value, max_rows)
    output <- c(
      heading,
      capture.output(print(preview, row.names = FALSE))
    )
    if (nrow(value) > max_rows) {
      output <- c(
        output,
        paste0("... ", nrow(value) - max_rows, " additional row(s) omitted")
      )
    }
    output
  }

  if (inherits(result, "data.frame")) {
    return(paste(table_text(result), collapse = "\n"))
  }
  if (is.list(result) && !is.null(names(result))) {
    sections <- lapply(names(result), function(name) {
      value <- result[[name]]
      body <- if (inherits(value, "data.frame")) {
        table_text(value)
      } else {
        capture.output(str(value, max.level = 3L, give.attr = FALSE))
      }
      c(paste0("[", name, "]"), body)
    })
    return(paste(unlist(sections, use.names = FALSE), collapse = "\n"))
  }

  paste(capture.output(str(result, max.level = 3L)), collapse = "\n")
}
