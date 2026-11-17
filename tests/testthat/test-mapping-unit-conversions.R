mapping_conversion_fixture <- function() {
  data.frame(
    source_unit = c("ug/L", "degF", "mg/L", "mg/L"),
    target_unit = c("mg/L", "degC", "g/L", "g/L"),
    scale_a = c(0.001, 5 / 9, 0.001, 0.002),
    scale_b = c(0, -160 / 9, 0, 0)
  )
}

test_that("unit suggestions use explicit, unambiguous conversions", {
  conversions <- mapping_conversion_fixture()
  expect_equal(
    ac_mapping_conversion(" ug/l ", "MG/L", conversions),
    list(scale_a = 0.001, scale_b = 0)
  )
  expect_equal(
    ac_mapping_conversion("\u00b5g/L", "mg/L", conversions),
    list(scale_a = 0.001, scale_b = 0)
  )
  expect_equal(
    ac_mapping_conversion("\u03bcg/L", "mg/L", conversions),
    list(scale_a = 0.001, scale_b = 0)
  )
  expect_equal(
    ac_mapping_conversion("degF", "degC", conversions),
    list(scale_a = 5 / 9, scale_b = -160 / 9)
  )
  expect_equal(
    ac_mapping_conversion("mg/L", "mg/l", conversions),
    list(scale_a = 1, scale_b = 0)
  )
  expect_null(ac_mapping_conversion("", "mg/L", conversions))
  expect_null(ac_mapping_conversion(NA_character_, "mg/L", conversions))
  expect_null(ac_mapping_conversion("ppm", "mg/L", conversions))
  expect_null(ac_mapping_conversion("mg/L", "ug/L", conversions))
  expect_null(ac_mapping_conversion("mg/L", "g/L", conversions))
  conversions$scale_a[[1]] <- Inf
  expect_null(ac_mapping_conversion("ug/L", "mg/L", conversions))
})

test_that("both mapping editors suggest conversions and preserve saved overrides", {
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
  for (manager in c(FALSE, TRUE)) {
    module <- if (manager) env$importSetup else env$addDiscData
    statements <- as.list(body(module)[[2]][[3]][[3]])[-1]
    wanted <- if (manager) {
      quote(manager_conversion_context())
    } else {
      quote(list(
        input$mapping_editor_ready,
        input$mapping_parameter,
        input$mapping_matrix_state
      ))
    }
    handler <- Filter(
      function(x) {
        is.call(x) &&
          identical(x[[1]], quote(observeEvent)) &&
          identical(x[[2]], wanted)
      },
      statements
    )[[1]]
    server <- function(input, output, session) {
      row <- data.frame(
        parameter_id = 1L,
        matrix_state_id = 1L,
        conversion = 42,
        result_offset = 9,
        source_unit = "ug/L",
        source_match = '{"parameter_code":"P","unit":"ug/L"}'
      )
      selected_mapping_row <- function() row
      manage_mapping_selected <- function() row
      mapping_unit_conversions <- mapping_conversion_fixture()
      params <- function() {
        data.frame(
          parameter_id = 1:3,
          unit_liquid = c("mg/L", "mg/L", "unknown")
        )
      }
      matrix_states <- data.frame(
        matrix_state_id = 1L,
        matrix_state_code = "liquid"
      )
      updates <- reactiveVal(list())
      updateNumericInput <- function(session, inputId, value) {
        values <- updates()
        values[[inputId]] <- value
        updates(values)
      }
      manager_conversion_context <- reactive(list(
        source_unit = input$unit,
        parameter_id = as.integer(input$parameter),
        matrix_state_id = 1L
      ))
      eval(handler)
    }
    environment(server) <- new.env(parent = env)
    environment(server)$handler <- handler
    environment(server)$mapping_conversion_fixture <- mapping_conversion_fixture
    shiny::testServer(server, {
      session$flushReact()
      # Loading a saved mapping retains its custom values, even when a
      # catalogue conversion exists for the same unit pair.
      session$setInputs(
        manage_mapping_type = "parameter",
        unit = "ug/L",
        parameter = 1L,
        mapping_editor_ready = "one",
        mapping_parameter = "1",
        mapping_matrix_state = "1"
      )
      prefix <- if (manager) "manage_" else "mapping_"
      expect_equal(updates()[[paste0(prefix, "conversion")]], 42)
      session$setInputs(parameter = 2L, mapping_parameter = "2")
      expect_equal(updates()[[paste0(prefix, "conversion")]], 0.001)
      expect_equal(updates()[[paste0(prefix, "result_offset")]], 0)
      before <- updates()
      session$setInputs(manage_conversion = 7, mapping_conversion = 7)
      expect_identical(updates(), before)
      session$setInputs(parameter = 3L, mapping_parameter = "3")
      expect_identical(updates(), before)
      session$setInputs(parameter = 1L, mapping_parameter = "1")
      expect_equal(updates()[[paste0(prefix, "conversion")]], 42)
    })
  }
  expect_identical(env$importSetup_mapping_unit(list(unit = "mg/L")), "mg/L")
  expect_identical(
    env$importSetup_mapping_unit(list(input_unit = "ug/L")),
    "ug/L"
  )
  expect_identical(
    env$importSetup_mapping_unit(list(source.Units = "ug/L")),
    "ug/L"
  )
  expect_true(is.na(env$importSetup_mapping_unit(list(
    unit = c("mg/L", "g/L")
  ))))
})

test_that("parameter-mapping worker saves the requested scope", {
  env <- new.env(parent = asNamespace("YGwater"))
  sys.source(
    system.file(
      "apps/aqualink/modules/admin/discreteData/addDiscData.R",
      package = "YGwater"
    ),
    env
  )
  # Exercise the actual upsert call without opening or writing a database.
  find_upsert <- function(expr) {
    if (!is.call(expr)) {
      return(list())
    }
    if (identical(expr[[1]], quote(addDiscData_upsert_mapping))) {
      return(list(expr))
    }
    unlist(lapply(as.list(expr)[-1], find_upsert), recursive = FALSE)
  }
  call <- find_upsert(body(env$addDiscData_run_mapping_save))[[1]]
  state <- new.env(parent = env)
  state$con <- NULL
  state$addDiscData_upsert_mapping <- function(...) list(...)
  for (scope in c("source", "profile")) {
    state$request <- list(
      mapping_scope = scope,
      row = data.frame(
        source_code = "LAB",
        source_parameter_code = "P",
        source_unit = "ug/L"
      ),
      profile = data.frame(profile_code = "workbook")
    )
    saved <- eval(call, state)
    expect_identical(
      saved$profile_code,
      if (scope == "source") NULL else "workbook"
    )
  }
})
