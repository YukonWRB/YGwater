ygwater_app_test_path <- function(...) {
  system.file("apps", "YGwater", ..., package = "YGwater")
}

shiny_server_test_environment <- function() {
  env <- new.env(parent = asNamespace("shiny"))
  sys.source(
    ygwater_app_test_path("server.R"),
    envir = env
  )
  env
}

test_that("module tab-change bridges handle each request once", {
  env <- shiny_server_test_environment()

  test_server <- function(input, output, session) {
    module_outputs <- shiny::reactiveValues(change_tab = NULL)
    selected_tabs <- shiny::reactiveVal(character())
    nav_select_fun <- function(session, id, selected) {
      selected_tabs(c(selected_tabs(), selected))
    }

    env$observe_module_tab_change(
      module_outputs,
      session,
      nav_select_fun
    )

    session$userData$module_outputs <- module_outputs
    session$userData$selected_tabs <- selected_tabs
  }

  shiny::testServer(test_server, {
    session$userData$module_outputs$change_tab <- "discData"
    session$flushReact()

    expect_equal(session$userData$selected_tabs(), "discData")
    expect_null(session$userData$module_outputs$change_tab)

    session$userData$module_outputs$change_tab <- "contData"
    session$flushReact()

    expect_equal(
      session$userData$selected_tabs(),
      c("discData", "contData")
    )
  })
})

test_that("main-server module lifecycle remains bounded", {
  server <- readLines(
    ygwater_app_test_path("server.R"),
    warn = FALSE
  )

  expect_equal(
    sum(grepl("observe_module_tab_change\\(", server)),
    4L
  )
  expect_equal(
    sum(grepl("reset_ui_loaded\\(\\)", server)),
    2L
  )
  expect_true(any(grepl("session$reload()", server, fixed = TRUE)))
  expect_equal(sum(grepl("ui_loaded\\$discData <- FALSE", server)), 1L)
  expect_equal(sum(grepl("ui_loaded\\$contData <- FALSE", server)), 1L)
})

test_that("map download requests refresh existing module instances", {
  module_paths <- c(
    ygwater_app_test_path("modules", "client", "data", "discreteData.R"),
    ygwater_app_test_path("modules", "client", "data", "continuousData.R")
  )

  for (path in module_paths) {
    code <- readLines(path, warn = FALSE)
    expect_true(any(grepl("inputs$location_request_id", code, fixed = TRUE)))
    expect_true(any(grepl("applyLocationInput", code, fixed = TRUE)))
  }
})

test_that("map plot requests filter both new and existing continuous plot modules", {
  path <- ygwater_app_test_path(
    "modules",
    "client",
    "plot",
    "continuousPlotAdaptive.R"
  )
  code <- readLines(path, warn = FALSE)

  expect_true(any(grepl(
    "identical(isolate(inputs$location_target), \"contPlot\")",
    code,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "inputs$location_request_id",
    code,
    fixed = TRUE
  )))
  expect_true(any(grepl("DT::updateSearch(", code, fixed = TRUE)))
  expect_true(any(grepl("searchCols = search_cols", code, fixed = TRUE)))
  expect_true(any(grepl(
    '"network_filter",',
    code,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    '"project_filter",',
    code,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    "ts <- ts[location_id %in% moduleInputs$location_id]",
    code,
    fixed = TRUE
  )))
})
