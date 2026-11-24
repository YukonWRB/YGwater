test_that("calibration sections record and restore the selected probe in testdb", {
  skip_if(
    Sys.getenv("AQUACACHE_TEST_CALIBRATION") != "true",
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
  expect_identical(
    DBI::dbGetQuery(con, "SELECT current_database()")[[1]],
    "testdb"
  )
  stopifnot(
    DBI::dbGetQuery(
      con,
      "SELECT version FROM information.version_info WHERE item = 'Last patch number'"
    )$version ==
      "74"
  )
  DBI::dbBegin(con)
  on.exit(DBI::dbRollback(con), add = TRUE, after = FALSE)

  module <- new.env(parent = asNamespace("shiny"))
  load_app_functions <- function(expr) {
    if (
      is.call(expr) &&
        identical(expr[[1]], as.name("<<-")) &&
        is.call(expr[[3]]) &&
        identical(expr[[3]][[1]], as.name("function"))
    ) {
      eval(call("<-", expr[[2]], expr[[3]]), envir = module)
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
  sys.source(
    system.file(
      "apps/aqualink/modules/admin/instruments/calibrate.R",
      package = "YGwater"
    ),
    envir = module
  )

  fixture <- basename(tempfile("calibration-test-"))
  serial <- paste(fixture, "sonde")
  holder <- DBI::dbGetQuery(
    con,
    "INSERT INTO instruments.instruments (
       obs_datetime, observer, instrument_make_id, instrument_model_id,
       instrument_type_id, serial_no, owner, holds_replaceable_sensors,
       takes_measurements, can_be_logger, can_be_telemetry_component
     )
     SELECT now(), (SELECT min(observer_id) FROM instruments.observers),
            model.make_id, model.model_id, type_id, $1,
            (SELECT min(organization_id) FROM public.organizations),
            TRUE, TRUE, FALSE, FALSE
     FROM instruments.instrument_types CROSS JOIN (
       SELECT make_id, model_id FROM instruments.instrument_models ORDER BY model_id LIMIT 1
     ) model
     WHERE type = 'Sonde'
     RETURNING instrument_id, observer",
    params = list(serial)
  )
  probe_serials <- paste(fixture, c("old", "new"))
  probes <- DBI::dbGetQuery(
    con,
    "WITH make AS (
       INSERT INTO instruments.instrument_makes (make) VALUES ($3) RETURNING make_id
     ), model AS (
       INSERT INTO instruments.sensor_models (make_id, model)
       SELECT make_id, $3 FROM make RETURNING model_id
     )
     INSERT INTO instruments.sensors (sensor_serial, make_id, sensor_model, sensor_type)
     SELECT probe_serial,
            (SELECT make_id FROM make),
            (SELECT model_id FROM model),
            (SELECT min(sensor_type_id) FROM instruments.sensor_types WHERE sensor_type <> 'BLANK')
     FROM unnest(ARRAY[$1::text, $2::text]) AS probes(probe_serial)
     RETURNING sensor_id, sensor_serial",
    params = c(as.list(probe_serials), list(fixture))
  )
  probe_ids <- probes$sensor_id[match(probe_serials, probes$sensor_serial)]
  observed <- as.POSIXct("2025-07-15 15:00:00", tz = "UTC")
  for (index in seq_along(probe_ids)) {
    DBI::dbExecute(
      con,
      "WITH event AS (
         INSERT INTO instruments.instrument_sensor_events (instrument_id, observer, obs_datetime)
         VALUES ($1, $2, $3) RETURNING event_id
       )
       INSERT INTO instruments.instrument_sensor_event_slots (event_id, slot_number, sensor_id, slot_state)
       SELECT event_id, 1, $4, 'installed' FROM event",
      params = list(
        holder$instrument_id,
        holder$observer,
        observed + c(-600, 600)[index],
        probe_ids[index]
      )
    )
  }
  invisible(module$calibrateUI("calibrate"))
  DBI::dbExecute(con, "SET LOCAL ROLE yg_editor_group")
  mock_session <- function() {
    session <- shiny::MockShinySession$new()
    session$userData$AquaCache <- con
    session
  }
  language <- shiny::reactiveValues(language = "en")
  set_basic_inputs <- function(session, purpose) {
    session$setInputs(
      ui_ready = TRUE,
      ID_sensor_holder = serial,
      ID_handheld_meter = "NA",
      observer = as.character(holder$observer),
      obs_datetime = observed,
      timezone = "UTC+00:00",
      calibration_purpose = purpose,
      selection = "Basic calibration info",
      tab_panel = "Checks / calibrations"
    )
    session$setInputs(save_basic_info = 1)
  }

  find_draft <- function(purpose) {
    DBI::dbGetQuery(
      con,
      "SELECT calibration_id FROM instruments.calibrations
       WHERE sensor_holder_instrument_id = $1 AND purpose = $2",
      params = list(holder$instrument_id, purpose)
    )$calibration_id
  }
  stored_sensor <- function(section, id) {
    table <- DBI::dbQuoteIdentifier(
      con,
      DBI::Id(
        schema = "instruments",
        table = paste0("calibrate_", section)
      )
    )
    DBI::dbGetQuery(
      con,
      paste("SELECT sensor_id FROM", table, "WHERE calibration_id = $1"),
      params = list(id)
    )$sensor_id
  }

  shiny::testServer(
    module$calibrate,
    args = list(language = language),
    session = mock_session(),
    {
      set_basic_inputs(session, "first draft")
      expect_true(complete$basic)
      just_registered <- paste(fixture, "new sonde")
      DBI::dbExecute(
        con,
        "INSERT INTO instruments.instruments (
           obs_datetime, observer, instrument_make_id, instrument_model_id,
           instrument_type_id, serial_no, owner, takes_measurements, holds_replaceable_sensors
         )
         SELECT now(), observer, instrument_make_id, instrument_model_id,
                instrument_type_id, $1, owner, TRUE, TRUE
         FROM instruments.instruments WHERE instrument_id = $2",
        params = list(just_registered, holder$instrument_id)
      )
      expect_false(just_registered %in% instruments_data$others$serial_no)
      session$setInputs(reload_instruments = 1)
      expect_match(output$ID_sensor_holder$html, just_registered, fixed = TRUE)
      expect_match(
        output$ID_sensor_holder$html,
        paste0('value="', serial, '" selected'),
        fixed = TRUE
      )
      first_id <- find_draft("first draft")
      expect_identical(calibration_data$next_id, first_id)
      expect_identical(
        probe_catalogue()$slot_number[match(
          probe_ids,
          probe_catalogue()$sensor_id
        )],
        c(1L, NA_integer_)
      )
      expect_match(
        output$probe_temperature$html,
        "In the holder at the calibration time"
      )
      expect_match(output$probe_temperature$html, "Other registered probes")
      expect_match(output$probe_temperature$html, 'value="none" selected')
      choose_probe <- function(section, sensor) {
        do.call(
          session$setInputs,
          setNames(list(as.character(sensor)), probe_input_id(section))
        )
      }
      choose_probe("temperature", probe_ids[1])
      session$setInputs(
        temp_reference_desc = "Reference thermometer",
        temp_reference = 20,
        temp_observed = 20
      )
      session$setInputs(save_cal_temp = 1)
      expect_true(complete$temperature)
      expect_identical(stored_sensor("temperature", first_id), probe_ids[1])

      for (section in c("ph", "spc", "orp", "turbidity", "do", "depth")) {
        choose_probe(section, probe_ids[2])
      }
      session$setInputs(
        ph_entry_mode = "check",
        ph1_std = 4,
        ph2_std = 7,
        ph3_std = 10,
        ph1_pre_val = 4,
        ph2_pre_val = 7,
        ph3_pre_val = 10,
        ph1_mv = 173,
        ph2_mv = 0,
        ph3_mv = -173,
        spc_entry_mode = "check",
        spc_points = "3",
        spc_or_not = FALSE,
        spc1_std = 0,
        spc2_std = 1413,
        spc3_std = 12880,
        spc1_pre = 0,
        spc2_pre = 1413,
        spc3_pre = 12880,
        orp_entry_mode = "check",
        orp_std = 220,
        orp_pre_mv = 220,
        turb_entry_mode = "check",
        turb1_std = 0,
        turb2_std = 100,
        turb1_pre = 0,
        turb2_pre = 100,
        do_entry_mode = "check",
        baro_press_pre = 1,
        do_pre = 8,
        depth_check_ok = "TRUE",
        depth_changes_ok = "TRUE"
      )
      session$setInputs(
        save_cal_ph = 1,
        save_cal_spc = 1,
        save_cal_orp = 1,
        save_cal_turb = 1,
        save_cal_do = 1,
        save_cal_depth = 1
      )
      # Exercise the regular confirmation callbacks for validation prompts.
      session$setInputs(
        ok_check_ph = 1,
        ok_check_spc = 1,
        ok_check_orp = 1,
        ok_check_turb = 1,
        ok_check_do = 1
      )
      spc <- DBI::dbGetQuery(
        con,
        "SELECT calibration_points, check_only, spc3_pre, spc3_post
         FROM instruments.calibrate_specific_conductance WHERE calibration_id = $1",
        params = list(first_id)
      )
      expect_identical(spc$calibration_points, 3L)
      expect_true(spc$check_only)
      expect_equal(spc$spc3_pre, 12880)
      expect_true(is.na(spc$spc3_post))
      for (table in c(
        "depth",
        "dissolved_oxygen",
        "orp",
        "ph",
        "specific_conductance",
        "turbidity"
      )) {
        expect_identical(
          stored_sensor(table, first_id),
          probe_ids[2],
          info = table
        )
      }
    }
  )
  first_id <- find_draft("first draft")

  shiny::testServer(
    module$calibrate,
    args = list(language = language),
    session = mock_session(),
    {
      set_basic_inputs(session, "second draft")
      second_id <- find_draft("second draft")
      expect_identical(calibration_data$next_id, second_id)
      expect_false(identical(second_id, first_id))

      session$setInputs(
        tab_panel = "View unfinished calibrations",
        incomplete_table_rows_selected = match(
          first_id,
          calibrations$incomplete_calibrations$calibration_id
        )
      )
      session$setInputs(restart_calibration = 1)
      expect_identical(calibration_data$next_id, first_id)
      expect_equal(calibration_data$basic$obs_datetime, observed)
      expect_match(
        output$obs_datetime_ui$html,
        as.character(as.numeric(observed)),
        fixed = TRUE
      )
      expect_match(
        output$probe_temperature$html,
        sprintf('value="%d" selected', probe_ids[1])
      )
      expect_match(
        output$probe_ph$html,
        sprintf('value="%d" selected', probe_ids[2])
      )

      do.call(session$setInputs, setNames(list("none"), probe_input_id("ph")))
      session$setInputs(
        ph_entry_mode = "check",
        ph1_std = 4,
        ph2_std = 7,
        ph3_std = 10,
        ph1_pre_val = 4,
        ph2_pre_val = 7,
        ph3_pre_val = 10,
        ph1_mv = 173,
        ph2_mv = 0,
        ph3_mv = -173
      )
      session$setInputs(save_cal_ph = 1)
      session$setInputs(ok_check_ph = 1)
      expect_identical(stored_sensor("ph", first_id), NA_integer_)
      expect_length(stored_sensor("ph", second_id), 0L)
      expect_identical(stored_sensor("temperature", first_id), probe_ids[1])

      session$setInputs(submit_btn = 1)
      session$setInputs(confirm_finalize = 1)
      expect_true(
        DBI::dbGetQuery(
          con,
          "SELECT complete FROM instruments.calibrations WHERE calibration_id = $1",
          params = list(first_id)
        )$complete
      )
      expect_identical(stored_sensor("temperature", first_id), probe_ids[1])
    }
  )
})
