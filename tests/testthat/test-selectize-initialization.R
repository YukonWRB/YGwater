# Exercise the actual initialization observers without opening a database.
selectize_module_expressions <- function(path, module) {
  env <- new.env(parent = asNamespace("shiny"))
  sys.source(system.file("apps", "aqualink", "modules", path, package = "YGwater"), env)
  callbacks <- Filter(function(expr) {
    is.call(expr) &&
      (identical(expr[[1L]], quote(moduleServer)) ||
        identical(expr[[1L]], quote(shiny::moduleServer)))
  }, as.list(body(env[[module]]))[-1L])
  stopifnot(length(callbacks) == 1L)
  as.list(callbacks[[1L]][[3L]][[3L]])[-1L]
}

selectize_ready_observers <- function(expressions, input_id = "ui_ready") {
  Filter(function(expr) {
    is.call(expr) && length(expr) >= 3L &&
      (identical(expr[[1L]], quote(observeEvent)) ||
        identical(expr[[1L]], quote(shiny::observeEvent))) &&
      identical(expr[[2L]], substitute(input$id, list(id = as.name(input_id))))
  }, expressions)
}

selectize_initialization_code <- function(path, module, qualified = FALSE) {
  expressions <- selectize_module_expressions(path, module)
  matches <- selectize_ready_observers(expressions)
  stopifnot(length(matches) == 1L)
  matches[[1L]]
}

test_that("guideline defaults wait for browser binding and run once", {
  code <- selectize_initialization_code(
    "admin/discreteData/addGuidelines.R", "addGuidelines"
  )
  server <- function(input, output, session) {
    observeEvent <- shiny::observeEvent
    updateTextAreaInput <- shiny::updateTextAreaInput
    initialize_count <- shiny::reactiveVal(0L)
    update_choices <- function(clear) {
      expect_true(clear)
      initialize_count(initialize_count() + 1L)
    }
    set_rule_input_rows <- update_reporting_basis_visibility <- function(...) NULL
    blank_rule_input_rows <- empty_table_text <- function(...) ""
    update_guideline_type_choices <- function(...) NULL
    coef_cols <- narrative_cols <- character()
    eval(code, environment())
    session$userData$initialize_count <- initialize_count
  }
  shiny::testServer(server, {
    session$flushReact()
    expect_equal(session$userData$initialize_count(), 0L)
    # Other early server flushes and inputs cannot initialize the form.
    session$setInputs(review_status = "draft")
    expect_equal(session$userData$initialize_count(), 0L)
    session$setInputs(ui_ready = "first-render")
    expect_equal(session$userData$initialize_count(), 1L)
    session$setInputs(review_status = "approved", ui_ready = "another-render")
    expect_equal(session$userData$initialize_count(), 1L)
  })
})

test_that("community choices wait for binding and return after UI recreation", {
  expressions <- selectize_module_expressions("client/reports/floodDashboard.R", "floodDashboardMod")
  code <- selectize_initialization_code(
    "client/reports/floodDashboard.R", "floodDashboardMod", qualified = TRUE
  )
  messages <- list()
  session <- shiny::MockShinySession$new()
  session$sendInputMessage <- function(inputId, message) {
    messages[[length(messages) + 1L]] <<- list(id = inputId, message = message)
  }
  server <- function(input, output, session) {
    communities <- c("Teslin", "Whitehorse")
    community_selection <- shiny::reactiveVal(NULL)
    remember <- Filter(function(expr) {
      is.call(expr) && identical(expr[[1L]], quote(shiny::observeEvent)) &&
        identical(expr[[2L]], quote(input$community)) &&
        "community_selection" %in% all.names(expr)
    }, expressions)[[1L]]
    eval(remember, environment())
    eval(code, environment())
  }
  shiny::testServer(server, session = session, {
    session$flushReact()
    expect_length(messages, 0L)
    session$setInputs(ui_ready = "english-render")
    expect_length(messages, 1L)
    expect_identical(messages[[1L]]$message$value, "Teslin")
    expect_match(messages[[1L]]$message$options, "Whitehorse", fixed = TRUE)
    session$setInputs(community = "Whitehorse", ui_ready = "french-render")
    expect_length(messages, 2L)
    expect_identical(messages[[2L]]$message$value, "Whitehorse")
    # Recreating an initially empty select must not forget the last selection.
    session$setInputs(community = "", ui_ready = "another-language-render")
    expect_length(messages, 3L)
    expect_identical(messages[[3L]]$message$value, "Whitehorse")
  })
})

test_that("readiness markers are unique without depending on clock ticks or RNG", {
  withr::local_seed(47)
  rng <- .Random.seed
  tokens <- replicate(1000L, YGwater:::shiny_ui_ready_token())
  expect_length(unique(tokens), 1000L)
  expect_identical(.Random.seed, rng)
  html <- htmltools::renderTags(YGwater:::shiny_ui_ready("module-ready", "token"))$html
  input <- xml2::xml_find_first(xml2::read_html(html), "//input")
  expect_identical(xml2::xml_attr(input, "id"), "module-ready")
  expect_identical(xml2::xml_attr(input, "value"), "token")
})

test_that("startup refreshes wait for a fresh marker, including module replacement", {
  cases <- list(
    c("admin/instruments/manageInstruments.R", "manageInstruments"),
    c("admin/instruments/manageSensors.R", "manageSensors"),
    c("admin/instruments/instrumentMaintenance.R", "instrumentMaintenance"),
    c("admin/applicationTasks/manageNewsContent.R", "manageNewsContent"),
    c("admin/users/manageUsers.R", "manageUsers"),
    c("admin/discreteData/importSetup.R", "importSetup"),
    c("admin/discreteData/addDiscData.R", "addDiscData"),
    c("admin/metadata/manageReferenceTables.R", "referenceTableManager"),
    c("admin/metadata/manageReferenceTables.R", "manageParameters"),
    c("admin/imgupload/addImgs.R", "addImgs"),
    c("client/reports/basinDashboard.R", "basinDashboardMod"),
    c("admin/instruments/calibrate.R", "calibrate")
  )
  for (case in cases) {
    code <- selectize_ready_observers(selectize_module_expressions(case[[1L]], case[[2L]]))
    expect_true(length(code) > 0L, info = case[[2L]])
    calls <- character()
    server <- function(input, output, session) {
      observeEvent <- shiny::observeEvent
      record_call <- function(...) calls <<- c(calls, "updated")
      session$sendInputMessage <- function(...) record_call()
      refresh_all <- clear_form <- refresh_lookups <- load_pages <- load_roles <-
        reload_profiles <- refresh_editor <- update_location_selectize <-
        update_sublocation_selectize <- updateSelectizeInput <- updateSelectInput <-
        sync_spc_labels <- sync_spc_post_visibility <- updateRadioButtons <- record_call
      scalar_utc_datetime <- function(x) x
      air_datetime_widget_timezone <- default_input_timezone <- function(...) "UTC"
      latest_bulletin <- NULL
      swe_basins <- list(polygons = data.frame(basin_name = "Fixture basin"))
      spc_supports_extended_schema <- FALSE
      session$userData$register <- function() {
        for (expr in code) eval(expr, environment())
      }
    }
    shiny::testServer(server, {
      # Simulate logging in after the old module's UI was already bound.
      session$setInputs(ui_ready = "previous-module")
      shiny::isolate(session$userData$register())
      session$flushReact()
      expect_true(length(calls) == 0L, info = case[[2L]])
      session$setInputs(ui_ready = "replacement-module")
      expect_true(length(calls) > 0L, info = case[[2L]])
      count <- length(calls)
      session$setInputs(ui_ready = "later-render")
      expect_true(length(calls) == count, info = case[[2L]])
    })
  }
})

test_that("dynamic guideline rows populate only after their own binding", {
  code <- selectize_ready_observers(selectize_module_expressions(
    "admin/discreteData/addGuidelines.R", "addGuidelines"
  ), "rule_inputs_ready")[[1L]]
  messages <- list()
  session <- shiny::MockShinySession$new()
  session$sendInputMessage <- function(inputId, message) {
    messages[[length(messages) + 1L]] <<- list(id = inputId, message = message)
  }
  server <- function(input, output, session) {
    observeEvent <- shiny::observeEvent
    updateSelectizeInput <- shiny::updateSelectizeInput
    rows <- shiny::reactiveVal(data.frame(input_source = "parameter", parameter_id = 1L))
    rule_input_seed <- rows
    rule_input_count <- function() nrow(rows())
    normalize_rule_input_rows <- identity
    parameter_choices_with_helpers <- function() c("Parameter" = "1", "Hardness" = "helper")
    selected_or_empty <- function(x) as.character(x)
    rule_input_id <- function(field, i) sprintf("rule_%s_%d", field, i)
    hardness_helper_choice_value <- "helper"
    eval(code, environment())
    session$userData$rows <- rows
  }
  shiny::testServer(server, session = session, {
    session$flushReact()
    expect_length(messages, 0L)
    session$setInputs(rule_inputs_ready = "first")
    expect_length(messages, 1L)
    expect_identical(messages[[1L]]$message$value, "1")
    session$userData$rows(data.frame(
      input_source = c("parameter", "hardness_helper"), parameter_id = c(1L, NA_integer_)
    ))
    session$flushReact()
    expect_length(messages, 1L)
    session$setInputs(rule_inputs_ready = "two-rows")
    expect_length(messages, 3L)
    expect_identical(messages[[3L]]$id, "rule_parameter_2")
    expect_identical(messages[[3L]]$message$value, "helper")
  })
})

test_that("data-driven lookup observers also wait for the rendered controls", {
  cases <- list(
    c("admin/boreholes_wells/editBoreholesWells.R", "editBoreholesWells"),
    c("admin/boreholes_wells/manageBoreholeDocuments.R", "manageBoreholeDocuments"),
    c("admin/continuousData/addContData.R", "addContData"),
    c("admin/discreteData/importSetup.R", "importSetup"),
    c("admin/continuousData/continuousDataReview.R", "continuousDataReview"),
    c("admin/documents/viewForecasterComments.R", "viewForecasterComments"),
    c("client/reports/WQReport.R", "WQReport", "main_ready"),
    c("admin/continuousData/addTimeseries.R", "addTimeseries", "main_ready"),
    c("client/plot/discretePlot.R", "discPlot", "sidebar_ready")
  )
  for (case in cases) {
    ready_name <- if (length(case) > 2L) case[[3L]] else "ui_ready"
    expressions <- lapply(selectize_module_expressions(case[[1L]], case[[2L]]), function(expr) {
      if (is.call(expr) && identical(expr[[1L]], quote(`<-`)) &&
          is.call(expr[[3L]]) && identical(expr[[3L]][[1L]], quote(shiny::observeEvent))) {
        expr[[3L]]
      } else {
        expr
      }
    })
    code <- Filter(function(expr) {
      is.call(expr) && length(expr) >= 2L && ready_name %in% all.names(expr) &&
        (identical(expr[[1L]], quote(observe)) ||
          identical(expr[[1L]], quote(observeEvent)) ||
          identical(expr[[1L]], quote(shiny::observeEvent)))
    }, expressions)
    expect_true(length(code) > 0L, info = case[[2L]])
    messages <- character()
    server <- function(input, output, session) {
      observe <- shiny::observe
      observeEvent <- shiny::observeEvent
      req <- shiny::req
      session$sendInputMessage <- function(inputId, message) messages <<- c(messages, inputId)
      updateSelectizeInput <- updateSelectInput <- function(session, inputId, ...) {
        messages <<- c(messages, inputId)
      }
      update_record_selector <- function(...) messages <<- c(messages, "record_id")
      moduleData <- shiny::reactiveValues(
        records = data.frame(record_key = "1"),
        boreholes = data.frame(borehole_name = "Fixture", is_well = TRUE, borehole_id = 1L),
        approvals = data.frame(approval_type_id = 1L, approval_type_description = "Fixture"),
        organizations = data.frame(organization_id = 1L, name = "Fixture"),
        EQ_params = data.frame(ParamCode = "P", ParamDesc = "Fixture"),
        EQ_param_grps = data.frame(groupname = "Fixture"),
        EQ_locs = data.frame(StnCode = "S", StnDesc = "Fixture"),
        EQ_loc_grps = data.frame(groupname = "Fixture"),
        EQ_stds = data.frame(StdCode = "G", StdName = "Fixture")
      )
      module_data <- list(grade_types = data.frame(
        grade_type_id = 1L, grade_type_description = "Fixture"
      ))
      params <- function() data.frame(parameter_id = 1L, param_name = "Fixture")
      reload_profiles <- function(...) NULL
      qc_assignments_draft <- function() list()
      active_kind <- function() "grade"
      selected_data_source <- function() "EQ"
      available_qc_rule_choices <- function() c("Fixture rule" = "rule")
      nullable_text <- function(x) if (length(x)) as.character(x[[1L]]) else NA_character_
      available_instrument_deployments <- current_timeseries_association <- function() data.frame(metadata_id = 1L)
      current_timeseries_id_for_association <- function() 1L
      build_instrument_association_choices <- function(...) c("Fixture instrument" = "1")
      instrument_association_cleared <- function() FALSE
      selected_record <- function() NULL
      comments_data <- function() data.frame(
        author = "Fixture author", category = "Fixture category",
        location_name = "Fixture location", location = "FIXTURE"
      )
      for (expr in code) eval(expr, environment())
    }
    shiny::testServer(server, {
      session$flushReact()
      expect_true(length(messages) == 0L, info = case[[2L]])
      session$setInputs(ui_ready = "bound", main_ready = "bound", sidebar_ready = "bound", data_source = "EQ")
      expect_true(length(messages) > 0L, info = case[[2L]])
    })
  }
})

test_that("late acknowledgements cannot populate another result modal", {
  expressions <- selectize_module_expressions("admin/discreteData/editSamples.R", "editSamples")
  show_modal <- Filter(function(expr) {
    is.call(expr) && identical(expr[[1L]], quote(`<-`)) &&
      identical(expr[[2L]], quote(show_result_modal))
  }, expressions)[[1L]]
  code <- selectize_ready_observers(as.list(show_modal[[3L]][[3L]])[-1L], "result_modal_ready")[[1L]]
  messages <- list()
  session <- shiny::MockShinySession$new()
  session$sendInputMessage <- function(inputId, message) {
    messages[[length(messages) + 1L]] <<- message
  }
  server <- function(input, output, session) {
    observeEvent <- shiny::observeEvent
    req <- shiny::req
    updateSelectizeInput <- shiny::updateSelectizeInput
    session$userData$register <- function(modal_token, selected_parameter) {
      force(modal_token)
      force(selected_parameter)
      parameter_choices <- c("First" = "1", "Second" = "2")
      eval(code, environment())
    }
  }
  shiny::testServer(server, session = session, {
    session$setInputs(result_modal_ready = "previous")
    shiny::isolate(session$userData$register("closed", "1"))
    session$flushReact()
    shiny::isolate(session$userData$register("current", "2"))
    session$flushReact()
    expect_length(messages, 0L)
    session$setInputs(result_modal_ready = "current")
    expect_length(messages, 1L)
    expect_identical(messages[[1L]]$value, "2")
  })
})

test_that("floodAtlas overlap keeps available and requested years across language redraws", {
  env <- new.env(parent = asNamespace("shiny"))
  sys.source(system.file("apps", "floodAtlas_over", "server.R", package = "YGwater"), env)
  expressions <- as.list(body(env$app_server))[-1L]
  render <- Filter(function(expr) {
    is.call(expr) && identical(expr[[1L]], quote(`<-`)) &&
      identical(expr[[2L]], quote(output$visible_buttons))
  }, expressions)[[1L]]
  server <- function(input, output, session) {
    scope <- new.env(parent = asNamespace("shiny"))
    scope$input <- input
    scope$output <- output
    scope$session <- session
    scope$params <- shiny::reactiveValues(yrs = c("2024", "2022"))
    scope$available_years <- shiny::reactiveVal(c("2024", "2023", "2022"))
    eval(render, scope)
    session$userData$params <- scope$params
  }
  shiny::testServer(server, {
    session$setInputs(lang = "en")
    html <- xml2::read_html(output$visible_buttons$html)
    expect_identical(xml2::xml_attr(xml2::xml_find_all(html, "//select/option"), "value"),
      c("2024", "2023", "2022"))
    session$setInputs(lang = "fr")
    html <- xml2::read_html(output$visible_buttons$html)
    expect_identical(xml2::xml_attr(xml2::xml_find_all(html, "//select/option[@selected]"), "value"),
      c("2024", "2022"))
    expect_match(xml2::xml_text(html), "Re-dessiner", fixed = TRUE)
    session$userData$params$yrs <- "2023"
    session$setInputs(lang = "en")
    html <- xml2::read_html(output$visible_buttons$html)
    expect_identical(xml2::xml_attr(xml2::xml_find_all(html, "//select/option[@selected]"), "value"), "2023")
  })
})
