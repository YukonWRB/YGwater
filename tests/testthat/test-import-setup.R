import_setup_environment <- function() {
  env <- new.env(parent = asNamespace("YGwater"))
  for (file in c("addDiscData.R", "importSetup.R")) {
    sys.source(
      system.file(
        "apps/aqualink/modules/admin/discreteData",
        file,
        package = "YGwater"
      ),
      env
    )
  }
  env
}

test_that("the mapping manager permits repeated names in distinct projects", {
  env <- import_setup_environment()
  state <- new.env(parent = env)
  profile <- data.frame(
    source_code = "ALS",
    source_name = "ALS",
    import_source_id = 1L,
    import_profile_id = 1L,
    profile_code = "xlr",
    profile_name = "XLR"
  )
  state$can_manage_mappings <- TRUE
  state$selected_manage_profile <- function() profile
  state$manage_mapping_selected <- function() NULL
  state$sources <- function() data.frame(source_code = "ALS", active = TRUE)
  state$manager_text <- function(value, default) value[[1]]
  state$input <- list(
    manage_location_scope = "source",
    manage_location_project = "2",
    manage_location_code = "SW01",
    manage_location_id = "12",
    manage_location_active = TRUE
  )
  state$con <- NULL
  state$bump_revision <- function() NULL
  state$clear_manager_mapping_selection <- function() NULL
  existing <- data.frame(
    source_location_code = "SW01",
    project_id = 1L,
    profile_specific = FALSE,
    location_id = 11L
  )
  saved <- NULL
  local_mocked_bindings(
    dbWithTransaction = function(conn, code, ...) force(code),
    dbGetQuery = function(conn, statement, ...) {
      if (grepl("SELECT source_name", statement, fixed = TRUE)) {
        data.frame(source_name = "ALS", active = TRUE)
      } else {
        data.frame()
      }
    },
    .package = "DBI"
  )
  local_mocked_bindings(
    getImportLocationMappings = function(...) existing,
    upsertImportLocationMappings = function(con, mappings, ...) {
      saved <<- mappings
    },
    .package = "AquaCache"
  )
  statements <- as.list(body(env$importSetup)[[2]][[3]][[3]])[-1]
  assignment <- Filter(
    function(x) {
      is.call(x) &&
        identical(x[[1]], quote(`<-`)) &&
        identical(x[[2]], quote(save_manager_mapping))
    },
    statements
  )[[1]]
  eval(assignment, state)
  state$save_manager_mapping("location")
  expect_identical(saved$project_id, 2L)
  expect_identical(saved$location_id, 12L)
  state$input$manage_location_project <- "1"
  expect_error(state$save_manager_mapping("location"), "already exists")
})

test_that("source criteria preserve package field names, case and blank units", {
  env <- import_setup_environment()
  eqwin <- env$importSetup_match_values(
    '{"ParamCode":"Al-D","input_unit":"mg/L"}'
  )
  expect_identical(eqwin, list(ParamCode = "Al-D", input_unit = "mg/L"))
  expect_identical(
    env$importSetup_match_values(
      '{"input_param":"ALKALINITY","input_unit":""}'
    ),
    list(input_param = "ALKALINITY", input_unit = "")
  )
  expect_identical(
    env$importSetup_match_values('{"parameter_code":"Al-D","unit":"mg/L"}'),
    list(parameter_code = "Al-D", unit = "mg/L")
  )
  expect_identical(
    names(env$importSetup_match_values('{"Source parameter":"value"}')),
    "Source parameter"
  )
})

test_that("malformed and empty criteria cannot become catch-all mappings", {
  env <- import_setup_environment()
  for (text in c(
    "{",
    "{}",
    "[]",
    '{"code":null}',
    '{"code":["A","B"]}',
    '{"code":12}',
    '{"code":"A","code":"B"}',
    '{"code":" ","unit":""}'
  )) {
    expect_error(env$importSetup_match_values(text), "criterion|criteria|JSON")
  }
  expect_error(
    env$importSetup_match_values('{"parameter_id":"12"}'),
    "reserved"
  )
})

test_that("profile text result mappings validate labels and numbers", {
  env <- import_setup_environment()
  expect_identical(
    env$importSetup_parse_result_text_values("Absent = 0\nPresent = 1"),
    list(Absent = 0, Present = 1)
  )
  expect_identical(env$importSetup_parse_result_text_values(" \n"), list())
  expect_error(
    env$importSetup_parse_result_text_values("Present = 1\npresent = 0"),
    "unique labels"
  )
  expect_error(
    env$importSetup_parse_result_text_values("Present = yes"),
    "unique labels and finite numeric values"
  )
})


test_that("an unspecified laboratory stays unassigned in workbook defaults", {
  env <- import_setup_environment()
  profile <- data.frame(profile_code = "no_lab")
  profile$defaults <- I(list(list(owner = 1L, media_id = 1L)))
  expect_true(is.na(env$addDiscData_defaults(profile)$lab_id))
  profile$defaults[[1]]$lab_id <- 7L
  expect_identical(env$addDiscData_defaults(profile)$lab_id, 7L)
})

test_that("all workbook metadata defaults can remain unset", {
  env <- import_setup_environment()
  fields <- c(
    "media_id",
    "collection_method_id",
    "sample_type_id",
    "owner",
    "result_type_id",
    "matrix_state_id",
    "result_value_type_id",
    "lab_id",
    "protocol_method_id"
  )
  profile <- data.frame(profile_code = "optional_defaults")
  for (value in list(
    NULL,
    list(),
    stats::setNames(rep(list(NA_integer_), length(fields)), fields)
  )) {
    profile$defaults <- I(list(value))
    defaults <- env$addDiscData_defaults(profile)
    expect_true(all(vapply(
      defaults[fields],
      function(x) identical(x, NA_integer_),
      logical(1)
    )))
  }
  expect_true(all(is.na(unlist(env$addDiscData_defaults(NULL)[fields]))))
  specified <- stats::setNames(as.list(seq_along(fields)), fields)
  profile$defaults <- I(list(specified))
  expect_identical(env$addDiscData_defaults(profile)[fields], specified)
})

test_that("workbook default selectors preserve blank and existing values", {
  env <- import_setup_environment()
  statements <- as.list(body(env$importSetup)[[2]][[3]][[3]])[-1]
  assignment <- Filter(
    function(x) {
      is.call(x) &&
        identical(x[[1]], quote(`<-`)) &&
        identical(x[[2]], quote(output$new_profile_default_fields))
    },
    statements
  )[[1]]
  render <- function() NULL
  body(render) <- assignment[[3]][[2]]
  state <- new.env(parent = env)
  for (name in c(
    "tagList",
    "tags",
    "helpText",
    "fluidRow",
    "column",
    "selectizeInput"
  )) {
    state[[name]] <- getExportedValue("shiny", name)
  }
  state$ns <- identity
  state$new_profile_template <- function() NULL
  state$media <- data.frame(media_id = 11L, media_type = "Water")
  state$collection_methods <- data.frame(
    collection_method_id = 12L,
    collection_method = "Bottle"
  )
  state$sample_types <- data.frame(
    sample_type_id = 13L,
    sample_type = "Routine"
  )
  state$organizations <- data.frame(organization_id = 14L, name = "Owner")
  state$laboratories <- data.frame(lab_id = 15L, lab_name = "Lab")
  state$result_types <- data.frame(result_type_id = 16L, result_type = "Lab")
  state$matrix_states <- data.frame(
    matrix_state_id = 17L,
    matrix_state_name = "Liquid"
  )
  state$result_value_types <- data.frame(
    result_value_type_id = 18L,
    result_value_type = "Actual"
  )
  state$protocol_methods <- data.frame(
    protocol_method_id = 19L,
    protocol_name = "EPA method"
  )
  environment(render) <- state
  html <- as.character(render())
  expect_equal(
    length(gregexpr(
      '<option value="" selected>None</option>',
      html,
      fixed = TRUE
    )[[1]]),
    9L
  )
  profile <- data.frame(profile_code = "existing")
  profile$defaults <- I(list(list(
    media_id = 11L,
    owner = 14L,
    protocol_method_id = 19L
  )))
  state$new_profile_template <- function() profile
  html <- as.character(render())
  expect_match(html, '<option value="11" selected>Water</option>', fixed = TRUE)
  expect_match(html, '<option value="14" selected>Owner</option>', fixed = TRUE)
  expect_match(
    html,
    '<option value="19" selected>EPA method</option>',
    fixed = TRUE
  )

  collect_defaults <- Filter(
    function(x) {
      is.call(x) &&
        identical(x[[1L]], quote(`<-`)) &&
        identical(x[[2L]], quote(profile_defaults))
    },
    statements
  )[[1L]]
  eval(collect_defaults, state)
  profile$defaults[[1L]]$grade_type_id <- 3L
  state$input <- list(new_profile_default_protocol = "20")
  expect_identical(state$profile_defaults(profile)$protocol_method_id, 20L)
  expect_identical(state$profile_defaults(profile)$grade_type_id, 3L)
  state$input$new_profile_default_protocol <- ""
  expect_identical(
    state$profile_defaults(profile)$protocol_method_id,
    NA_integer_
  )
})

test_that("required record metadata is checked before upload", {
  env <- import_setup_environment()
  state <- new.env(parent = env)
  statements <- as.list(body(env$addDiscData)[[2]][[3]][[3]])[-1]
  assignment <- Filter(
    function(x) {
      is.call(x) &&
        identical(x[[1]], quote(`<-`)) &&
        identical(x[[2]], quote(validate_upload_rows))
    },
    statements
  )[[1]]
  eval(assignment, state)
  state$sample_types <- data.frame(
    sample_type_id = 34L,
    sample_type = "Routine",
    requires_location = TRUE,
    requires_sample_group = FALSE
  )
  state$sample_groups <- function() data.frame(sample_group_id = integer())
  state$parameter_requirement <- function(...) FALSE
  state$params <- function() data.frame()
  state$matrix_states <- data.frame()
  local_mocked_bindings(
    validateResultChemistry = function(...) invisible(TRUE),
    .package = "AquaCache"
  )
  df <- env$addDiscData_empty_table()
  df[1, ] <- NA
  df$datetime <- as.POSIXct("2026-11-16", tz = "UTC")
  for (field in c(
    "media_id",
    "collection_method_id",
    "owner",
    "parameter_id",
    "result_type_id",
    "matrix_state_id",
    "location_id"
  )) {
    df[[field]] <- 1L
  }
  df$sample_type_id <- 34L
  df$result <- 1
  df$result_flag_action <- "keep"
  expect_true(state$validate_upload_rows(df))
  for (field in c(
    "datetime",
    "media_id",
    "collection_method_id",
    "sample_type_id",
    "owner",
    "parameter_id",
    "result_type_id",
    "matrix_state_id"
  )) {
    missing <- df
    missing[[field]] <- NA
    expect_error(
      state$validate_upload_rows(missing),
      "Complete the required fields in the preview"
    )
    missing[[field]] <- NULL
    expect_error(
      state$validate_upload_rows(missing),
      "Complete the required fields in the preview"
    )
  }
})


test_that("review context does not label a complete mapping as needing attention", {
  env <- import_setup_environment()
  statements <- as.list(body(env$importSetup)[[2]][[3]][[3]])[-1]
  assignment <- Filter(
    function(x) {
      is.call(x) &&
        identical(x[[1]], quote(`<-`)) &&
        identical(x[[2]], quote(output$editing_mapping))
    },
    statements
  )[[1]]
  render <- function() NULL
  body(render) <- assignment[[3]][[2]]
  state <- new.env(parent = env)
  state$tags <- htmltools::tags
  state$input <- list(manage_mapping_type = "parameter")
  state$row <- data.frame(
    import_mapping_id = 4L,
    source_code = "ALS",
    profile_code = "xlr",
    active = TRUE
  )
  state$context <- data.frame(
    source_code = "ALS",
    profile_code = "xlr",
    profile_name = "Detailed Report",
    mapping_type = "parameter",
    issue = NA_character_,
    status = "published",
    version = 3L,
    import_mapping_id = 4L,
    source_match = '{"source.Analyte":"Conductivity"}'
  )
  state$review_rows <- function() state$context
  state$manage_mapping_selected <- function() state$row
  state$opened_review_mapping <- function() state$context
  environment(render) <- state
  complete <- as.character(render())
  expect_false(grepl("Needs attention", complete, fixed = TRUE))
  expect_match(complete, "alert-info", fixed = TRUE)
  expect_match(
    complete,
    "Saving updates this mapping immediately",
    fixed = TRUE
  )
  state$context$issue <- "Unit missing"
  incomplete <- as.character(render())
  expect_match(incomplete, "needs attention", fixed = TRUE)
  expect_match(incomplete, "Unit missing", fixed = TRUE)
  expect_match(incomplete, "alert-warning", fixed = TRUE)
})

test_that("source-wide mapping summaries count real rows and name the source", {
  env <- import_setup_environment()
  statements <- as.list(body(env$importSetup)[[2]][[3]][[3]])[-1]
  assignment <- Filter(
    function(x) {
      is.call(x) &&
        identical(x[[1]], quote(`<-`)) &&
        identical(x[[2]], quote(output$mapping_status))
    },
    statements
  )[[1]]
  render <- function() NULL
  body(render) <- assignment[[3]][[2]]
  state <- new.env(parent = env)
  state$tags <- htmltools::tags
  state$selected_manage_profile <- function() {
    data.frame(
      source_code = "LAB",
      profile_code = NA_character_,
      profile_name = NA_character_
    )
  }
  state$review_rows <- function() {
    data.frame(
      source_code = "LAB",
      profile_code = c(NA_character_, NA_character_, "workbook"),
      active = TRUE,
      issue = NA_character_
    )
  }
  environment(render) <- state
  html <- as.character(render())
  expect_match(
    html,
    "2 current mapping(s) for LAB (source-wide).",
    fixed = TRUE
  )
  expect_false(grepl("for NA", html, fixed = TRUE))
})
