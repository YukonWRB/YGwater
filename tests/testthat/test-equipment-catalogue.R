test_that("equipment models are filtered in an app environment with data.table", {
  app <- new.env(parent = globalenv())
  sys.source(
    system.file(
      "apps/aqualink/modules/admin/instruments/equipmentCatalogue.R",
      package = "YGwater"
    ),
    envir = app
  )
  models <- data.table::data.table(
    model_id = 1:2,
    make_id = 3:4,
    model = c("EXO", "PLS-C")
  )
  expect_identical(
    app$equipment_model_choices(app$equipment_models_for_make(models, 4L)),
    c("PLS-C" = "2")
  )
  expect_length(app$equipment_models_for_make(models, NA_integer_)$model_id, 0)
})

test_that("AquaLink scopes equipment models and records explicit slot states", {
  skip_if(
    Sys.getenv("AQUACACHE_TEST_EQUIPMENT") != "true",
    "Patch 74 testdb opt-in required"
  )
  con <- AquaConnect(
    name = "testdb",
    host = "10.250.12.154",
    user = Sys.getenv("aquacacheAdminUser"),
    password = Sys.getenv("aquacacheAdminPass"),
    silent = TRUE
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  scalar <- function(sql, ...) {
    params <- list(...)
    if (!length(params)) {
      params <- NULL
    }
    DBI::dbGetQuery(con, sql, params = params)[[1L]]
  }
  stopifnot(scalar("SELECT current_database()") == "testdb")
  DBI::dbBegin(con)
  rollback <- DBI::dbRollback
  on.exit(rollback(con), add = TRUE, after = FALSE)

  module <- new.env(parent = asNamespace("shiny"))
  load_app_functions <- function(expr) {
    if (
      is.call(expr) &&
        identical(expr[[1L]], as.name("<<-")) &&
        is.call(expr[[3L]]) &&
        identical(expr[[3L]][[1L]], as.name("function"))
    ) {
      eval(call("<-", expr[[2L]], expr[[3L]]), envir = module)
    } else if (is.call(expr) || is.expression(expr)) {
      for (child in as.list(expr)) {
        load_app_functions(child)
      }
    }
  }
  load_app_functions(parse(system.file(
    "apps/aqualink/YGwater_globals.R",
    package = "YGwater"
  )))
  module$application_notifications_ui <- function(...) NULL
  for (file in c(
    "equipmentCatalogue.R",
    "manageInstruments.R",
    "manageSensors.R",
    "instrumentMaintenance.R"
  )) {
    sys.source(
      system.file(
        "apps/aqualink/modules/admin/instruments",
        file,
        package = "YGwater"
      ),
      envir = module
    )
  }

  changes <- new.env()
  real_update <- shiny::updateSelectizeInput
  module$updateSelectizeInput <- function(
    session,
    inputId,
    ...,
    choices = NULL,
    selected = NULL
  ) {
    if (!is.null(choices)) {
      changes[[inputId]] <- list(choices = choices, selected = selected)
    }
    real_update(session, inputId, ..., choices = choices, selected = selected)
  }
  mock_session <- function() {
    session <- shiny::MockShinySession$new()
    session$userData$AquaCache <- con
    session$userData$config <- list(dbUser = "postgres")
    session
  }
  language <- shiny::reactiveValues(language = "en")
  ysi <- as.character(scalar(
    "SELECT make_id FROM instruments.instrument_makes WHERE make = 'YSI'"
  ))
  ott <- as.character(scalar(
    "SELECT make_id FROM instruments.instrument_makes WHERE make = 'OTT'"
  ))
  exo <- as.character(scalar(
    "SELECT model_id FROM instruments.instrument_models WHERE model = 'EXO' AND make_id = $1",
    ysi
  ))
  owner <- as.character(scalar(
    "SELECT min(organization_id) FROM public.organizations"
  ))
  observer <- as.character(scalar(
    "SELECT min(observer_id) FROM instruments.observers"
  ))
  instrument_type <- as.character(scalar(
    "SELECT min(type_id) FROM instruments.instrument_types"
  ))
  sensor_type <- as.character(scalar(
    "SELECT min(sensor_type_id) FROM instruments.sensor_types WHERE sensor_type <> 'BLANK'"
  ))
  tag <- basename(tempfile("equipment-ui-"))
  observed <- as.POSIXct("2026-08-04 14:00:00", tz = "UTC")

  DBI::dbExecute(con, "SET LOCAL ROLE yg_editor_group")
  # Let the module transactions run within the fixture's rollback boundary.
  local_mocked_bindings(
    dbBegin = function(conn, ...) {
      DBI::dbExecute(conn, "SAVEPOINT equipment_form")
    },
    dbCommit = function(conn, ...) {
      DBI::dbExecute(conn, "RELEASE SAVEPOINT equipment_form")
    },
    dbRollback = function(conn, ...) {
      DBI::dbExecute(conn, "ROLLBACK TO SAVEPOINT equipment_form")
      DBI::dbExecute(conn, "RELEASE SAVEPOINT equipment_form")
    },
    .package = "DBI"
  )

  shiny::testServer(
    module$manageInstruments,
    args = list(language = language),
    session = mock_session(),
    {
      session$flushReact()
      session$setInputs(ui_ready = "ready")
    expect_false("[Unmapped manufacturer]" %in% names(changes$make$choices))
    session$setInputs(make = paste(tag, "instrument manufacturer"))
    session$setInputs(new_make = paste(tag, "instrument manufacturer"), add_new_make = 1)
    expect_length(scalar("SELECT make_id FROM instruments.instrument_makes WHERE make = $1",
                         paste(tag, "instrument manufacturer")), 1L)
      expect_length(changes$model$choices, 0L)
      session$setInputs(make = ysi, model = exo)
      expect_true("EXO" %in% names(changes$model$choices))
      expect_false("PLS-C" %in% names(changes$model$choices))
      session$setInputs(make = ott)
      expect_true("PLS-C" %in% names(changes$model$choices))
      expect_length(changes$model$selected, 0L)
      session$setInputs(
        serial_no = tag,
        observer = observer,
        owner = owner,
        type = instrument_type,
        obs_datetime = observed
      )
      session$setInputs(save = 1)
      expect_length(
        scalar(
          "SELECT instrument_id FROM instruments.instruments WHERE serial_no = $1",
          tag
        ),
        0L
      )
      session$setInputs(model = paste(tag, "instrument model"))
      session$setInputs(
        new_model = paste(tag, "instrument model"),
        add_new_model = 1
      )
      model <- scalar(
        "SELECT model_id FROM instruments.instrument_models WHERE make_id = $1 AND model = $2",
        ott,
        paste(tag, "instrument model")
      )
      expect_length(model, 1L)
      session$setInputs(
        serial_no = tag,
        model = as.character(model),
        observer = observer,
        owner = owner,
        type = instrument_type,
        obs_datetime = observed
      )
      session$setInputs(save = 2)
      expect_identical(
        scalar(
          "SELECT instrument_model_id FROM instruments.instruments WHERE serial_no = $1",
          tag
        ),
        model
      )
    }
  )
  holder <- scalar(
    "SELECT instrument_id FROM instruments.instruments WHERE serial_no = $1",
    tag
  )

  shiny::testServer(
    module$manageSensors,
    args = list(language = language),
    session = mock_session(),
    {
      session$flushReact()
      session$setInputs(ui_ready = "ready")
    expect_false("[Unmapped manufacturer]" %in% names(changes$sensor_make$choices))
      expect_false("BLANK" %in% names(changes$sensor_type$choices))
      session$setInputs(sensor_make = ysi)
      expect_true("EXO" %in% names(changes$sensor_model$choices))
      expect_false("PLS-C" %in% names(changes$sensor_model$choices))
      session$setInputs(
        sensor_model = sensor_data$models$model_id[
          sensor_data$models$model == "EXO"
        ]
      )
      session$setInputs(sensor_make = ott)
      expect_equal(names(changes$sensor_model$choices), "PLS-C")
      expect_length(changes$sensor_model$selected, 0L)
      session$setInputs(sensor_make = paste(tag, "manufacturer"))
      session$setInputs(
        sensor_model = paste(tag, "sensor model"),
        sensor_serial = tag,
        sensor_type = sensor_type,
        owner = owner
      )
      session$setInputs(save = 1)
      expect_identical(
        scalar(
          "SELECT m.make FROM instruments.sensors s JOIN instruments.sensor_models sm ON sm.model_id = s.sensor_model
       JOIN instruments.instrument_makes m ON m.make_id = sm.make_id WHERE s.sensor_serial = $1",
          tag
        ),
        paste(tag, "manufacturer")
      )
    }
  )

  sensor <- as.character(scalar(
    "SELECT sensor_id FROM instruments.sensors WHERE sensor_serial = $1",
    tag
  ))
  shiny::testServer(
    module$instrumentMaintenance,
    args = list(language = language),
    session = mock_session(),
    {
      session$flushReact()
      session$setInputs(ui_ready = "ready", view = "Sensors")
      session$setInputs(
        instrument_id = as.character(holder),
        observer = observer,
        obs_datetime = observed,
        slot_number = 1,
        slot_state = "empty",
        sensor_id = sensor
      )
      session$setInputs(save_slot_event = 1)
      session$setInputs(
        slot_number = 2,
        slot_state = "installed",
        obs_datetime = observed + 10
      )
      session$setInputs(save_slot_event = 2)
      expect_identical(
        scalar(
          "SELECT slot_state FROM instruments.instrument_sensor_current WHERE instrument_id = $1 AND slot_number = 2",
          holder
        ),
        "installed"
      )
      session$setInputs(slot_state = "unknown", obs_datetime = observed + 20)
      session$setInputs(save_slot_event = 3)
      expect_null(maintenance_data$pending_sensor_unassign)
      expect_identical(
        scalar(
          "SELECT slot_state FROM instruments.instrument_sensor_current WHERE instrument_id = $1 ORDER BY slot_number",
          holder
        ),
        c("empty", "unknown")
      )
      expect_identical(
        scalar(
          "SELECT sensor_id FROM instruments.instrument_sensor_current WHERE instrument_id = $1 ORDER BY slot_number",
          holder
        ),
        c(NA_integer_, NA_integer_)
      )
      expect_true(is.na(scalar(
        "SELECT date_retired FROM instruments.sensors WHERE sensor_id = $1",
        sensor
      )))
      session$setInputs(
        slot_number = 3,
        slot_state = "installed",
        obs_datetime = observed + 30
      )
      session$setInputs(save_slot_event = 4)
      session$setInputs(slot_number = 4, obs_datetime = observed + 40)
      session$setInputs(save_slot_event = 5)
      expect_false(is.null(maintenance_data$pending_sensor_move))
      session$setInputs(confirm_sensor_move = 1)
      expect_identical(
        scalar(
          "SELECT slot_state FROM instruments.instrument_sensor_current WHERE instrument_id = $1 ORDER BY slot_number",
          holder
        ),
        c("empty", "unknown", "empty", "installed")
      )

      session$setInputs(register_sensor = 1, new_sensor_make = ysi)
      expect_setequal(
        names(changes$new_sensor_model$choices),
        c("EXO", "ProDSS")
      )
      session$setInputs(new_sensor_make = ott)
      expect_equal(names(changes$new_sensor_model$choices), "PLS-C")
      session$setInputs(
        new_sensor_type = sensor_type,
        new_sensor_serial = paste(tag, "modal"),
        new_sensor_owner = owner,
        new_sensor_model = as.character(
          scalar(
            "SELECT model_id FROM instruments.sensor_models WHERE make_id = $1 AND model = 'PLS-C'",
            ott
          )
        )
      )
      session$setInputs(add_sensor = 1)
      expect_equal(
        scalar(
          "SELECT make_id FROM instruments.sensors WHERE sensor_serial = $1",
          paste(tag, "modal")
        ),
        as.integer(ott)
      )
    }
  )
})
