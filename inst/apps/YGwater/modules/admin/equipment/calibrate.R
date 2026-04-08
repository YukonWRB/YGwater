# UI and server code for main calibrations module

calibrateUI <- function(id) {
  ns <- NS(id)

  # Set up for background color when validating calibrations
  instrSelectBGCol <- '
    shinyjs.backgroundCol = function(params) {
      var defaultParams = {
        id : null,
        col : "red"
      };
      params = shinyjs.getParams(params, defaultParams);

      var el = $("#" + params.id);
      el.css("background-color", params.col);
    }'

  tagList(
    div(
      tags$head(
        tags$meta(
          name = "viewport",
          content = "width=device-width, initial-scale=1"
        ),
        tags$link(rel = "apple-touch-icon", href = "app-icon.png"),
        tags$link(rel = "icon", type = "image/png", href = "app-icon.png"),
        tags$style(
          type = 'text/css',
          ".selectize-dropdown-content {max-height: 400px; }"
        ),
        tags$style(
          HTML(
            ".load_sensors_btn .btn {
              display: block !important;
              margin-bottom: 10px;
            }"
          )
        ),
        tags$style(
          HTML(
            ".show_sensors_btns .btn:not(.hidden) {
              display: block !important;
            }"
          ),
          HTML(
            "/* Make the notifications stand out more */
        .shiny-notification {
          font-size: 24px;
          font-weight: bold;
          background-color: #f9f9f9;  /* Light grey background */
          color: #000000;  /* Dark grey text */
          padding: 15px;  /* Larger padding for more space */
          border-left: 5px solid #FF0000;
          border-right: 5px solid #FF0000;
          border-top: 5px solid #FF0000;
          border-bottom: 5px solid #FF0000;
          border-radius: 10px;  /* Rounded corners */
        }"
          )
        ),
        tags$script(
          src = "https://cdnjs.cloudflare.com/ajax/libs/js-cookie/2.2.1/js.cookie.min.js"
        ) # Cookies library
      ),
      shinyjs::extendShinyjs(
        text = instrSelectBGCol,
        functions = c("backgroundCol")
      ),
      uiOutput(ns("banner")),
      # Tabs
      tabsetPanel(
        id = ns("tab_panel"),
        tabPanel(
          "Checks / calibrations",
          sidebarLayout(
            sidebarPanel(
              selectizeInput(
                ns("selection"),
                label = "Select a parameter",
                choices = c(
                  "Basic record info" = "Basic calibration info",
                  "Temperature check" = "Temperature calibration",
                  "Conductivity check / calibration" = "Conductivity calibration",
                  "pH check / calibration" = "pH calibration",
                  "ORP check / calibration" = "ORP calibration",
                  "Turbidity check / calibration" = "Turbidity calibration",
                  "DO check / calibration" = "DO calibration",
                  "Depth check" = "Depth calibration"
                )
              ),
              conditionalPanel(
                ns = ns,
                condition = "input.selection == 'Basic calibration info'",
                uiOutput(ns("observer")),
                fluidRow(
                  column(
                    4,
                    selectizeInput(
                      ns("timezone"),
                      "Input timezone",
                      choices = input_timezone_choices(),
                      selected = default_input_timezone(),
                      multiple = FALSE
                    )
                  ),
                  column(
                    8,
                    shinyWidgets::airDatepickerInput(
                      ns("obs_datetime"),
                      label = "Record date/time",
                      value = .POSIXct(Sys.time(), tz = "UTC"),
                      range = FALSE,
                      multiple = FALSE,
                      timepicker = TRUE,
                      maxDate = Sys.Date() + 1,
                      startView = Sys.Date(),
                      update_on = "change",
                      tz = air_datetime_widget_timezone(default_input_timezone()),
                      timepickerOpts = shinyWidgets::timepickerOptions(
                        minutesStep = 15,
                        timeFormat = "HH:mm"
                      )
                    )
                  )
                ),
                textOutput(ns("instrument_reminder")),
                uiOutput(ns("ID_sensor_holder")),
                uiOutput(ns("ID_handheld_meter")),
                textInput(
                  ns("calibration_purpose"),
                  "Record purpose",
                  value = ""
                ),
                actionButton(ns("save_basic_info"), "Save this section"),
              ),
              conditionalPanel(
                ns = ns,
                condition = "input.selection == 'pH calibration'",
                uiOutput(ns("ph_entry_mode_ui")),
                numericInput(
                  ns("ph1_std"),
                  label = "Low pH solution value",
                  value = 4
                ),
                numericInput(
                  ns("ph2_std"),
                  label = "Neutral pH solution value",
                  value = 7
                ),
                numericInput(
                  ns("ph3_std"),
                  label = "High pH solution value",
                  value = 10
                ),
                numericInput(
                  ns("ph1_pre_val"),
                  label = "pH 4 Observed / as-found value",
                  value = NA
                ),
                numericInput(
                  ns("ph2_pre_val"),
                  label = "pH 7 Observed / as-found value",
                  value = NA
                ),
                numericInput(
                  ns("ph3_pre_val"),
                  label = "pH 10 Observed / as-found value",
                  value = NA
                ),
                numericInput(ns("ph1_mv"), label = "pH 4 mV", value = NA),
                numericInput(ns("ph2_mv"), label = "pH 7 mV", value = NA),
                numericInput(ns("ph3_mv"), label = "pH 10 mV", value = NA),
                actionButton(
                  ns("show_post_ph"),
                  "Show as-left fields",
                  style = "display:block; margin: 8px 0 12px 0;"
                ),
                numericInput(
                  ns("ph1_post_val"),
                  label = "pH 4 As-left value",
                  value = 4
                ),
                numericInput(
                  ns("ph2_post_val"),
                  label = "pH 7 As-left value",
                  value = 7
                ),
                numericInput(
                  ns("ph3_post_val"),
                  label = "pH 10 As-left value",
                  value = 10
                ),
                actionButton(ns("save_cal_ph"), "Save this section"),
                actionButton(ns("delete_ph"), "Delete this section")
              ),
              conditionalPanel(
                ns = ns,
                condition = "input.selection == 'Temperature calibration'",
                textInput(
                  ns("temp_reference_desc"),
                  label = "Temp Reference Type",
                  value = "Lab thermometer"
                ),
                numericInput(
                  ns("temp_reference"),
                  label = "Reference Temp",
                  value = NA
                ),
                numericInput(
                  ns("temp_observed"),
                  label = "Sensor Temp",
                  value = NA
                ),
                actionButton(ns("save_cal_temp"), "Save this section"),
                actionButton(ns("delete_temp"), "Delete this section")
              ),
              conditionalPanel(
                ns = ns,
                condition = "input.selection == 'Conductivity calibration'",
                uiOutput(ns("spc_entry_mode_ui")),
                radioButtons(
                  ns("spc_points"),
                  label = "Standard points",
                  choices = c("1 point" = 1, "2 point" = 2, "3 point" = 3),
                  selected = 2,
                  inline = TRUE
                ),
                uiOutput(ns("spc_schema_notice")),
                numericInput(
                  ns("spc1_std"),
                  label = "SpC Low-Range Standard",
                  value = 0
                ),
                conditionalPanel(
                  ns = ns,
                  condition = "parseInt(input.spc_points || '2', 10) >= 2",
                  numericInput(
                    ns("spc2_std"),
                    label = "SpC High-Range Standard",
                    value = 1413
                  )
                ),
                conditionalPanel(
                  ns = ns,
                  condition = "parseInt(input.spc_points || '2', 10) >= 3",
                  numericInput(
                    ns("spc3_std"),
                    label = "SpC High-Range Standard",
                    value = 12880
                  )
                ),
                checkboxInput(
                  ns("spc_or_not"),
                  "Enter non-specific conductivity instead?",
                  value = FALSE
                ),
                numericInput(
                  ns("spc1_pre"),
                  label = "SpC Low-Range Observed / as-found value",
                  value = NA
                ),
                conditionalPanel(
                  ns = ns,
                  condition = "parseInt(input.spc_points || '2', 10) >= 2",
                  numericInput(
                    ns("spc2_pre"),
                    label = "SpC High-Range Observed / as-found value",
                    value = NA
                  )
                ),
                conditionalPanel(
                  ns = ns,
                  condition = "parseInt(input.spc_points || '2', 10) >= 3",
                  numericInput(
                    ns("spc3_pre"),
                    label = "SpC High-Range Observed / as-found value",
                    value = NA
                  )
                ),
                actionButton(
                  ns("show_post_spc"),
                  "Show as-left fields",
                  style = "display:block; margin: 8px 0 12px 0;"
                ),
                numericInput(
                  ns("spc1_post"),
                  label = "SpC Low-Range As-left value",
                  value = 0
                ),
                conditionalPanel(
                  ns = ns,
                  condition = "parseInt(input.spc_points || '2', 10) >= 2",
                  numericInput(
                    ns("spc2_post"),
                    label = "SpC High-Range As-left value",
                    value = 1413
                  )
                ),
                conditionalPanel(
                  ns = ns,
                  condition = "parseInt(input.spc_points || '2', 10) >= 3",
                  numericInput(
                    ns("spc3_post"),
                    label = "SpC High-Range As-left value",
                    value = 12880
                  )
                ),
                actionButton(ns("save_cal_spc"), "Save this section"),
                actionButton(ns("delete_spc"), "Delete this section")
              ),
              conditionalPanel(
                ns = ns,
                condition = "input.selection == 'ORP calibration'",
                uiOutput(ns("orp_entry_mode_ui")),
                numericInput(
                  ns("orp_std"),
                  label = "ORP Standard solution mV",
                  value = NA
                ),
                numericInput(
                  ns("orp_pre_mv"),
                  label = "ORP Observed / as-found mV",
                  value = NA
                ),
                actionButton(
                  ns("show_post_orp"),
                  "Show as-left fields",
                  style = "display:block; margin: 8px 0 12px 0;"
                ),
                numericInput(
                  ns("orp_post_mv"),
                  label = "ORP As-left mV",
                  value = NA
                ),
                actionButton(ns("save_cal_orp"), "Save this section"),
                actionButton(ns("delete_orp"), "Delete this section")
              ),
              conditionalPanel(
                ns = ns,
                condition = "input.selection == 'Turbidity calibration'",
                uiOutput(ns("turb_entry_mode_ui")),
                numericInput(
                  ns("turb1_std"),
                  label = "Low Turb Standard Value",
                  value = 0
                ),
                numericInput(
                  ns("turb2_std"),
                  label = "High Turb Standard Value",
                  value = 124
                ),
                numericInput(
                  ns("turb1_pre"),
                  label = "Low Turb Observed / as-found value",
                  value = NA
                ),
                numericInput(
                  ns("turb2_pre"),
                  label = "High Turb Observed / as-found value",
                  value = NA
                ),
                actionButton(
                  ns("show_post_turb"),
                  "Show as-left fields",
                  style = "display:block; margin: 8px 0 12px 0;"
                ),
                numericInput(
                  ns("turb1_post"),
                  label = "Low Turb As-left value",
                  value = 0
                ),
                numericInput(
                  ns("turb2_post"),
                  label = "High Turb As-left value",
                  value = 124
                ),
                actionButton(ns("save_cal_turb"), "Save this section"),
                actionButton(ns("delete_turb"), "Delete this section")
              ),
              conditionalPanel(
                ns = ns,
                condition = "input.selection == 'DO calibration'",
                uiOutput(ns("do_entry_mode_ui")),
                numericInput(
                  ns("baro_press_pre"),
                  label = "Baro Pressure Observed / as-found (mmHg)",
                  value = NA
                ),
                numericInput(
                  ns("baro_press_post"),
                  label = "Baro Pressure As-left (mmHg)",
                  value = NA
                ),
                numericInput(
                  ns("do_pre_prct"),
                  label = "DO Observed / as-found % LOCAL",
                  value = NA
                ),
                numericInput(
                  ns("do_post_prct"),
                  label = "DO As-left % LOCAL",
                  value = NA
                ),
                actionButton(ns("calc_abs_do"), "Calculate mg/l values"),
                actionButton(ns("calc_prct_do"), "Calculate % values"),
                numericInput(
                  ns("do_pre"),
                  label = "DO Observed / as-found mg/l",
                  value = NA
                ),
                numericInput(
                  ns("do_post"),
                  label = "DO As-left mg/l",
                  value = NA
                ),
                actionButton(
                  ns("show_post_do"),
                  "Show as-left fields",
                  style = "display:block; margin: 8px 0 12px 0;"
                ),
                actionButton(ns("save_cal_do"), "Save this section"),
                actionButton(ns("delete_do"), "Delete this section")
              ),
              conditionalPanel(
                ns = ns,
                condition = "input.selection == 'Depth calibration'",
                radioButtons(
                  inputId = ns("depth_check_ok"),
                  label = "Depth sensor output near 0 or as expected in air?",
                  choiceNames = c("FALSE", "TRUE"),
                  choiceValues = c("FALSE", "TRUE")
                ),
                radioButtons(
                  inputId = ns("depth_changes_ok"),
                  label = "Depth sensor output changes as expected with depth?",
                  choiceNames = c("Not Checked", "FALSE", "TRUE"),
                  choiceValues = c("Not Checked", "FALSE", "TRUE")
                ),
                actionButton(ns("save_cal_depth"), "Save this section"),
                actionButton(ns("delete_depth"), "Delete this section")
              ),
              actionButton(
                ns("submit_btn"),
                "Finalize + submit record",
                style = c("margin-top: 10px;")
              )
            ),
            mainPanel(
              DT::dataTableOutput(ns("calibration_instruments_table")),
              tableOutput(ns("restart_table")),
              tableOutput(ns("saved")),
              htmlOutput(ns("ph_mv_note")),
              htmlOutput(ns("ORP_molarity_note"))
            )
          )
        ),
        tabPanel(
          "Maintain instruments",
          sidebarLayout(
            sidebarPanel(
              DT::dataTableOutput(ns("maintain_instr_table"))
            ),
            mainPanel(
              DT::dataTableOutput(ns("past_instr_maintenance")),
              actionButton(ns("clear_selection"), "Clear selection"),
              textAreaInput(
                ns("maintain_comment"),
                "Describe the new maintenance performed",
                "",
                height = "100px",
                width = "800px"
              ),
              uiOutput(ns("maintain_recorder")),
              actionButton(
                ns("submit_instr_maintain"),
                "Save new maintenance event"
              )
            )
          )
        ),
        tabPanel(
          "Change/maintain sensors",
          sidebarLayout(
            sidebarPanel(
              textOutput(ns("sensors_reminder")),
              selectizeInput(
                ns("maintain_serial"),
                "Select your instrument",
                choices = "loading choices..."
              ),
              div(
                class = "load_sensors_btn",
                actionButton(ns("load_sensors"), "Show sensors")
              ),
              div(
                class = "show_sensors_btns",
                actionButton(
                  ns("sensor1_show"),
                  "Slot 1",
                  style = c("margin-bottom: 5px;")
                ),
                actionButton(
                  ns("sensor2_show"),
                  "Slot 2",
                  style = c("margin-bottom: 5px;")
                ),
                actionButton(
                  ns("sensor3_show"),
                  "Slot 3",
                  style = c("margin-bottom: 5px;")
                ),
                actionButton(
                  ns("sensor4_show"),
                  "Slot 4",
                  style = c("margin-bottom: 5px;")
                ),
                actionButton(
                  ns("sensor5_show"),
                  "Slot 5",
                  style = c("margin-bottom: 5px;")
                ),
                actionButton(
                  ns("sensor6_show"),
                  "Slot 6",
                  style = c("margin-bottom: 5px;")
                ),
                actionButton(
                  ns("sensor7_show"),
                  "Slot 7",
                  style = c("margin-bottom: 5px;")
                ),
                actionButton(
                  ns("sensor8_show"),
                  "Slot 8",
                  style = c("margin-bottom: 5px;")
                )
              ),
              selectizeInput(
                ns("add_sensor_type_dropdown"),
                "ADD a slot w/ sensor (CHANGE assigned sensor to the right)",
                choices = "placeholder",
                options = list(
                  placeholder = "Not specified",
                  onInitialize = I('function() { this.setValue(""); }')
                )
              ),
              selectizeInput(
                ns("new_sensor_serial"),
                "Serial number (type your own if not in yet)",
                choices = NULL,
                options = list(create = TRUE)
              ),
              actionButton(
                ns("add_new_sensor_serial"),
                "Add new sensor to database"
              ),
              uiOutput(ns("add_sensor_name")),
              textOutput(ns("add_sensor_note")),
              actionButton(ns("add_sensor_slot"), "Submit")
            ),
            mainPanel(
              DT::dataTableOutput(ns("manage_sensors_table")),
              DT::dataTableOutput(ns("sensor1_details")),
              DT::dataTableOutput(ns("sensor2_details")),
              DT::dataTableOutput(ns("sensor3_details")),
              DT::dataTableOutput(ns("sensor4_details")),
              DT::dataTableOutput(ns("sensor5_details")),
              DT::dataTableOutput(ns("sensor6_details")),
              DT::dataTableOutput(ns("sensor7_details")),
              DT::dataTableOutput(ns("sensor8_details")),
              htmlOutput(ns("sensor_change_note")),
              selectizeInput(
                ns("change_sensor"),
                "Assign a new sensor or leave as-is to log only maintenance note",
                choices = "placeholder",
                width = "500px",
                options = list(
                  placeholder = "Not specified",
                  onInitialize = I('function() { this.setValue(""); }')
                )
              ),
              selectizeInput(
                ns("add_sensor_serial"),
                "Serial number (type your own if not in yet)",
                choices = NULL,
                options = list(create = TRUE)
              ),
              actionButton(
                ns("add_new_sensor_serial2"),
                "Add new sensor to database"
              ),
              textAreaInput(
                ns("add_comment"),
                "Add a note (date is already captured)",
                "",
                height = "100px",
                width = "800px"
              ),
              uiOutput(ns("sensor_change_name")),
              actionButton(ns("submit_sensor_change"), "Submit new record")
            )
          )
        ),
        tabPanel(
          "View unfinished calibrations",
          sidebarLayout(
            sidebarPanel(
              helpText(
                "Select a row from the table, then restart or delete it."
              ),
              br(),
              actionButton(
                ns("restart_calibration"),
                "Restart selected calibration"
              ),
              actionButton(
                ns("delete_calibration"),
                "Delete selected calibration"
              )
            ),
            mainPanel(
              DT::dataTableOutput(ns("incomplete_table"))
            )
          )
        )
      ) # End of tabsetPanel
    ) # End of div
  ) # End of tagList
}

calibrate <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "calibrate"
      )
    })

    # Custom alert function using Shiny's showNotification (replaces old shinyAlert dependencies)
    alert <- function(title, text = NULL, type = NULL, timer = 2000) {
      msg <- if (is.null(text)) title else paste(title, text, sep = ": ")
      if (!is.null(type) && type %in% c("success", "info")) {
        type <- "message"
      }
      duration <- if (is.null(timer)) NULL else timer / 1000
      id <- paste0("alert-", substr(gsub("[^A-Za-z0-9]", "-", msg), 1, 50))
      removeNotification(id = id, session = session)
      showNotification(
        msg,
        type = ifelse(is.null(type), "default", type),
        duration = duration,
        id = id,
        session = session
      )
    }

    table_reset <- sprintf(
      '
var colors = ["blue", "green"];
var stack = [];
table.on("click", "tr", function() {
  var $rows = $("#%s tbody tr"); // change the name of the table here
  var $row = $(this);
  var idx = $row.index();
  if($row.hasClass("selected")) {
    stack.push(idx);
    for(var i = 0; i < stack.length; i++) {
      $rows.eq(stack[i]).find("td").css(
        "box-shadow", "inset 0 0 0 9999px " + colors[i]
      );
    }
  } else {
    var i0 = stack.indexOf(idx);
    $rows.eq(stack[i0]).find("td").css(
      "box-shadow", ""
    );
    stack.splice(i0, 1);
    for(var i = 0; i < stack.length; i++) {
      $rows.eq(stack[i]).find("td").css(
        "box-shadow", "inset 0 0 0 9999px " + colors[i]
      );
    }
  }
});',
      ns("calibration_instruments_table")
    )

    # Initial show/hide and creation of containers ################################################
    # create a few containers
    validation_check <- reactiveValues()
    calibration_data <- reactiveValues(restarted_id = 0)
    restarted <- reactiveValues(initialized = FALSE, restarted = FALSE)
    instruments_data <- reactiveValues()
    select_data <- reactiveValues() # Holds data to populate select menus
    sensors_data <- reactiveValues(datetime = .POSIXct(Sys.time(), tz = "UTC")) #get the time here so that multiple maintenance events are on same line
    default_obs_datetime <- function() {
      .POSIXct(Sys.time(), tz = "UTC")
    }
    db_table_exists <- function(schema, table) {
      DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT EXISTS (
          SELECT 1
          FROM information_schema.tables
          WHERE table_schema = $1 AND table_name = $2
        ) AS present",
        params = list(schema, table)
      )$present[[1]]
    }
    db_table_fields <- function(schema, table) {
      DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT column_name
         FROM information_schema.columns
         WHERE table_schema = $1 AND table_name = $2",
        params = list(schema, table)
      )$column_name
    }
    entry_mode_choices <- c(
      "Check only" = "check",
      "Calibration performed" = "calibration"
    )
    entry_mode_input_ids <- c(
      ph = "ph_entry_mode",
      spc = "spc_entry_mode",
      orp = "orp_entry_mode",
      turbidity = "turb_entry_mode",
      do = "do_entry_mode"
    )
    entry_display_labels <- list(
      basic = "Basic record info",
      temperature = "Temperature check",
      spc = "Conductivity check / calibration",
      ph = "pH check / calibration",
      orp = "ORP check / calibration",
      turbidity = "Turbidity check / calibration",
      do = "DO check / calibration",
      depth = "Depth check"
    )
    calibration_table_fields <- list(
      ph = db_table_fields("instruments", "calibrate_ph"),
      spc = db_table_fields("instruments", "calibrate_specific_conductance"),
      orp = db_table_fields("instruments", "calibrate_orp"),
      turbidity = db_table_fields("instruments", "calibrate_turbidity"),
      do = db_table_fields("instruments", "calibrate_dissolved_oxygen")
    )
    normalized_entry_mode <- function(value, default = "check") {
      if (
        is.null(value) ||
          !length(value) ||
          all(is.na(value)) ||
          !(value[[1]] %in% c("check", "calibration"))
      ) {
        return(default)
      }
      value[[1]]
    }
    parameter_entry_mode <- function(parameter_key, isolate_input = FALSE) {
      input_id <- if (parameter_key %in% names(entry_mode_input_ids)) {
        entry_mode_input_ids[[parameter_key]]
      } else {
        paste0(parameter_key, "_entry_mode")
      }
      input_value <- if (isTRUE(isolate_input)) {
        isolate(input[[input_id]])
      } else {
        input[[input_id]]
      }
      normalized_entry_mode(
        input_value,
        default = "check"
      )
    }
    is_check_only_mode <- function(parameter_key, isolate_input = FALSE) {
      identical(
        parameter_entry_mode(parameter_key, isolate_input = isolate_input),
        "check"
      )
    }
    show_requested <- function(input_id) {
      !is.null(input[[input_id]]) && (input[[input_id]] %% 2) == 1
    }
    sheet_check_only <- function(sheet) {
      "check_only" %in% colnames(sheet) && isTRUE(sheet$check_only[[1]])
    }
    selected_incomplete_row <- function() {
      selected_row <- input$incomplete_table_rows_selected[1]
      if (
        !length(selected_row) ||
          is.na(selected_row) ||
          selected_row < 1 ||
          selected_row > nrow(calibrations$incomplete_calibrations)
      ) {
        return(NA_integer_)
      }
      as.integer(selected_row)
    }
    sync_optional_post_fields <- function(
      button_id,
      field_ids,
      allow_post,
      show_fields = FALSE
    ) {
      if (!allow_post) {
        shinyjs::hide(button_id)
        for (field_id in field_ids) {
          shinyjs::hide(field_id)
        }
        return(invisible(FALSE))
      }

      shinyjs::show(button_id)
      if (show_fields) {
        for (field_id in field_ids) {
          shinyjs::show(field_id)
        }
        updateActionButton(
          session,
          button_id,
          label = "Hide as-left fields"
        )
      } else {
        for (field_id in field_ids) {
          shinyjs::hide(field_id)
        }
        updateActionButton(
          session,
          button_id,
          label = "Show as-left fields"
        )
      }
      invisible(show_fields)
    }
    ph_pre_label <- function(standard_value) {
      paste0(
        "pH ",
        standard_value,
        if (is_check_only_mode("ph")) {
          " Observed value"
        } else {
          " As-found value"
        }
      )
    }
    ph_post_label <- function(standard_value) {
      paste0("pH ", standard_value, " As-left value")
    }
    orp_pre_label <- function() {
      if (is_check_only_mode("orp")) {
        "ORP Observed mV"
      } else {
        "ORP As-found mV"
      }
    }
    turb_pre_label <- function(range_label) {
      paste(
        range_label,
        if (is_check_only_mode("turbidity")) {
          "Observed value"
        } else {
          "As-found value"
        }
      )
    }
    do_pre_label <- function(base_label) {
      if (is_check_only_mode("do")) {
        paste("Observed", base_label)
      } else {
        paste("As-found", base_label)
      }
    }
    spc_table_fields <- db_table_fields(
      "instruments",
      "calibrate_specific_conductance"
    )
    spc_supports_extended_schema <- all(
      c("calibration_points", "spc3_std", "spc3_pre", "spc3_post") %in%
        spc_table_fields
    )
    empty_string_to_na <- function(value) {
      if (is.null(value) || !length(value) || all(is.na(value))) {
        return(NA_character_)
      }
      value <- trimws(as.character(value[[1]]))
      if (!nzchar(value)) {
        return(NA_character_)
      }
      value
    }
    empty_integer_to_na <- function(value) {
      value <- empty_string_to_na(value)
      if (is.na(value)) {
        return(NA_integer_)
      }
      as.integer(value)
    }
    empty_numeric_to_na <- function(value) {
      if (is.null(value) || !length(value) || all(is.na(value))) {
        return(NA_real_)
      }
      as.numeric(value[[1]])
    }
    empty_date_to_na <- function(value) {
      if (is.null(value) || !length(value) || all(is.na(value))) {
        return(as.Date(NA))
      }
      as.Date(value[[1]])
    }
    instrument_fields <- db_table_fields("instruments", "instruments")
    suppliers_table_exists <- db_table_exists("instruments", "suppliers")
    has_supplier_column <- suppliers_table_exists &&
      "supplier_id" %in% instrument_fields
    load_instruments_sheet <- function() {
      purchase_price_select <- if ("purchase_price" %in% instrument_fields) {
        "i.purchase_price"
      } else {
        "NULL::numeric AS purchase_price"
      }
      takes_measurements_select <- if (
        "takes_measurements" %in% instrument_fields
      ) {
        "COALESCE(i.takes_measurements, FALSE) AS takes_measurements"
      } else {
        "NULL::boolean AS takes_measurements"
      }
      supplier_join <- if (has_supplier_column) {
        " LEFT JOIN instruments.suppliers suppliers ON i.supplier_id = suppliers.supplier_id"
      } else {
        ""
      }
      supplier_id_select <- if (has_supplier_column) {
        "i.supplier_id"
      } else {
        "NULL::integer AS supplier_id"
      }
      supplier_name_select <- if (has_supplier_column) {
        "suppliers.supplier_name AS supplier"
      } else {
        "NULL::text AS supplier"
      }
      DBI::dbGetQuery(
        session$userData$AquaCache,
        paste0(
          "SELECT i.instrument_id,
                  i.obs_datetime,
                  CONCAT(observers.observer_first, ' ', observers.observer_last, ' (', observers.organization, ')') AS observer,
                  i.holds_replaceable_sensors,
                  i.serial_no,
                  i.asset_tag,
                  i.date_in_service,
                  i.date_purchased,
                  ",
          purchase_price_select,
          ",
                  ",
          takes_measurements_select,
          ",
                  i.retired_by,
                  i.date_retired,
                  i.date_end_of_life,
                  instrument_make.make,
                  instrument_model.model,
                  instrument_type.type,
                  i.owner AS owner_id,
                  organizations.name AS owner,
                  ",
          supplier_id_select,
          ",
                  ",
          supplier_name_select,
          "
           FROM instruments.instruments AS i
           LEFT JOIN instruments.instrument_make ON i.make = instrument_make.make_id
           LEFT JOIN instruments.instrument_model ON i.model = instrument_model.model_id
           LEFT JOIN instruments.instrument_type ON i.type = instrument_type.type_id
           LEFT JOIN public.organizations ON i.owner = organizations.organization_id
           LEFT JOIN instruments.observers ON i.observer = observers.observer_id",
          supplier_join,
          "
           ORDER BY i.instrument_id"
        )
      )
    }
    refresh_instruments_sheet <- function() {
      instruments_data$sheet <- load_instruments_sheet()
      instruments_data$handhelds <- instruments_data$sheet[
        instruments_data$sheet$type == "Handheld" &
          is.na(instruments_data$sheet$date_retired),
        ,
        drop = FALSE
      ]
      instruments_data$others <- instruments_data$sheet[
        instruments_data$sheet$type != "Handheld" &
          is.na(instruments_data$sheet$date_retired),
        ,
        drop = FALSE
      ]
      instruments_data$maintainable_sensors <- instruments_data$sheet[
        !is.na(instruments_data$sheet$holds_replaceable_sensors) &
          instruments_data$sheet$holds_replaceable_sensors,
        ,
        drop = FALSE
      ]
      instruments_data$calibrate_instruments <- instruments_data$sheet[
        is.na(instruments_data$sheet$date_retired),
        !colnames(instruments_data$sheet) %in%
          c(
            "instrument_id",
            "observer",
            "obs_datetime",
            "owner_id",
            "retired_by",
            "date_retired",
            "date_purchased",
            "date_in_service",
            "date_end_of_life",
            "purchase_price",
            "supplier_id"
          ),
        drop = FALSE
      ]
      instruments_data$sheet
    }

    shift_obs_datetime_timezone <- function(tz_name) {
      current_value <- coerce_utc_datetime(input$obs_datetime)
      if (
        is.null(current_value) ||
          !length(current_value) ||
          all(is.na(current_value))
      ) {
        return(invisible(NULL))
      }
      shinyWidgets::updateAirDateInput(
        session,
        "obs_datetime",
        value = current_value,
        tz = tz_name
      )
    }

    send_table <- reactiveValues()
    messages <- reactiveValues()
    complete <- reactiveValues(
      basic = FALSE,
      temperature = FALSE,
      spc = FALSE,
      ph = FALSE,
      orp = FALSE,
      turbidity = FALSE,
      do = FALSE,
      depth = FALSE
    )
    reset_check <- reactiveValues(sensors = FALSE)
    modal_action_button <- function(id, label, class = "btn-primary") {
      tags$button(
        id = ns(id),
        type = "button",
        class = paste("btn", class, "action-button"),
        `data-dismiss` = "modal",
        `data-bs-dismiss` = "modal",
        label
      )
    }
    selectize_empty_options <- function(maxItems = NULL, clear = TRUE) {
      opts <- list(placeholder = "Not specified")
      if (clear) {
        opts$onInitialize <- I('function() { this.setValue(""); }')
      }
      if (!is.null(maxItems)) {
        opts$maxItems <- maxItems
      }
      opts
    }
    abbreviate_with_tooltip <- function(values, visible_chars = 10) {
      if (length(values) == 0) {
        return(values)
      }
      vapply(
        values,
        function(value) {
          if (is.na(value) || !nzchar(as.character(value))) {
            return("")
          }
          value <- as.character(value)
          short <- if (nchar(value) > visible_chars) {
            paste0(substr(value, 1, visible_chars), "...")
          } else {
            value
          }
          sprintf(
            "<span title=\"%s\">%s</span>",
            htmltools::htmlEscape(value, attribute = TRUE),
            htmltools::htmlEscape(short)
          )
        },
        character(1)
      )
    }
    format_owner_column_for_dt <- function(df, visible_chars = 10) {
      if (!("owner" %in% colnames(df))) {
        return(df)
      }
      df$owner <- abbreviate_with_tooltip(
        df$owner,
        visible_chars = visible_chars
      )
      df
    }

    ### Hide a bunch of buttons until they can be used
    # Delete buttons to remove a calibration sheet
    shinyjs::hide("delete_ph")
    shinyjs::hide("delete_turb")
    shinyjs::hide("delete_spc")
    shinyjs::hide("delete_temp")
    shinyjs::hide("delete_orp")
    shinyjs::hide("delete_do")
    shinyjs::hide("delete_depth")
    # Buttons to show sensor information
    shinyjs::hide("sensor1_show")
    shinyjs::addClass("sensor1_show", "hidden")
    shinyjs::hide("sensor2_show")
    shinyjs::addClass("sensor2_show", "hidden")
    shinyjs::hide("sensor3_show")
    shinyjs::addClass("sensor3_show", "hidden")
    shinyjs::hide("sensor4_show")
    shinyjs::addClass("sensor4_show", "hidden")
    shinyjs::hide("sensor5_show")
    shinyjs::addClass("sensor5_show", "hidden")
    shinyjs::hide("sensor6_show")
    shinyjs::addClass("sensor6_show", "hidden")
    shinyjs::hide("sensor7_show")
    shinyjs::addClass("sensor7_show", "hidden")
    shinyjs::hide("sensor8_show")
    shinyjs::addClass("sensor8_show", "hidden")
    shinyjs::hide("add_sensor_slot")
    shinyjs::hide("new_sensor_serial")
    shinyjs::hide("add_sensor_note")
    shinyjs::hide("sensor_change_note")
    shinyjs::hide("add_sensor_type_dropdown")
    shinyjs::hide("add_sensor_name")
    shinyjs::hide("load_sensors")
    shinyjs::hide("sensor1_details")
    shinyjs::hide("sensor2_details")
    shinyjs::hide("sensor3_details")
    shinyjs::hide("sensor4_details")
    shinyjs::hide("sensor5_details")
    shinyjs::hide("sensor6_details")
    shinyjs::hide("sensor7_details")
    shinyjs::hide("sensor8_details")
    shinyjs::hide("add_sensor_serial")
    shinyjs::hide("change_sensor")
    shinyjs::hide("add_comment")
    shinyjs::hide("sensor_change_name")
    shinyjs::hide("submit_sensor_change")
    shinyjs::hide("restart_table")

    # Hide optional as-left fields until a calibration mode explicitly asks for them.
    shinyjs::hide("show_post_ph")
    shinyjs::hide("show_post_orp")
    shinyjs::hide("show_post_turb")
    shinyjs::hide("show_post_spc")
    shinyjs::hide("show_post_do")
    shinyjs::hide("ph1_post_val")
    shinyjs::hide("ph2_post_val")
    shinyjs::hide("ph3_post_val")
    shinyjs::hide("orp_post_mv")
    shinyjs::hide("turb1_post")
    shinyjs::hide("turb2_post")
    shinyjs::hide("spc1_post")
    shinyjs::hide("spc2_post")
    shinyjs::hide("spc3_post")
    shinyjs::hide("baro_press_post")
    shinyjs::hide("do_post_prct")
    shinyjs::hide("do_post")

    output$spc_schema_notice <- renderUI({
      if (!spc_supports_extended_schema) {
        helpText(
          paste(
            "Database currently supports only 2-point conductivity entries.",
            "Apply the schema patch to enable 1-point, 3-point, and",
            "check-only saves."
          )
        )
      }
    })
    output$ph_entry_mode_ui <- renderUI({
      radioButtons(
        ns("ph_entry_mode"),
        label = "Entry type",
        choices = entry_mode_choices,
        selected = "check",
        inline = TRUE
      )
    })
    output$spc_entry_mode_ui <- renderUI({
      radioButtons(
        ns("spc_entry_mode"),
        label = "Entry type",
        choices = entry_mode_choices,
        selected = "check",
        inline = TRUE
      )
    })
    output$orp_entry_mode_ui <- renderUI({
      radioButtons(
        ns("orp_entry_mode"),
        label = "Entry type",
        choices = entry_mode_choices,
        selected = "check",
        inline = TRUE
      )
    })
    output$turb_entry_mode_ui <- renderUI({
      radioButtons(
        ns("turb_entry_mode"),
        label = "Entry type",
        choices = entry_mode_choices,
        selected = "check",
        inline = TRUE
      )
    })
    output$do_entry_mode_ui <- renderUI({
      radioButtons(
        ns("do_entry_mode"),
        label = "Entry type",
        choices = entry_mode_choices,
        selected = "check",
        inline = TRUE
      )
    })

    # Get the data from the database, make initial tables, populate UI elements ########################################
    refresh_instruments_sheet()

    calibrations <- reactiveValues()
    calibrations$calibrations <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT * FROM calibrations"
    ) # This will be used to check if there are any incomplete calibrations

    instruments_data$observers <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT * FROM observers"
    )
    instruments_data$instrument_maintenance <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT * FROM instrument_maintenance"
    )

    sensors_data$sensors <- DBI::dbGetQuery(
      session$userData$AquaCache,
      paste0("SELECT * FROM sensors")
    )
    sensors_data$sensor_types <- DBI::dbGetQuery(
      session$userData$AquaCache,
      paste0("SELECT * FROM sensor_types")
    )

    output$calibration_instruments_table <- DT::renderDT(
      {
        DT::datatable(
          format_owner_column_for_dt(instruments_data$calibrate_instruments),
          rownames = FALSE,
          filter = 'top',
          selection = "multiple",
          callback = htmlwidgets::JS(table_reset),
          escape = FALSE
        )
      },
      server = TRUE
    )

    observeEvent(input$last_observer_id, {
      # This is used to set the last observer ID to the last observer selected, recovered from cookies
      try({
        output$observer <- renderUI({
          selectizeInput(
            ns("observer"),
            label = "Calibrator name",
            choices = select_data$recorder,
            options = selectize_empty_options(clear = FALSE),
            selected = input$last_observer_id
          )
        })
        output$add_sensor_name <- renderUI({
          selectizeInput(
            ns("add_sensor_name"),
            label = "What's your name?",
            choices = select_data$recorder,
            options = selectize_empty_options(clear = FALSE),
            selected = input$last_observer_id
          )
        })
        output$sensor_change_name <- renderUI({
          selectizeInput(
            ns("sensor_change_name"),
            label = "Observer name",
            choices = select_data$recorder,
            options = selectize_empty_options(clear = FALSE),
            selected = input$last_observer_id
          )
        })
        output$maintain_recorder <- renderUI({
          selectizeInput(
            ns("maintain_recorder"),
            label = "Maintenance recorder",
            choices = select_data$recorder,
            options = selectize_empty_options(clear = FALSE),
            selected = input$last_observer_id
          )
        })
      })
    })

    observe({
      if (!restarted$initialized) {
        instruments_data$observers$observer_string <- paste0(
          instruments_data$observers$observer_first,
          " ",
          instruments_data$observers$observer_last,
          " (",
          instruments_data$observers$organization,
          ")"
        )
        select_data$recorder <- stats::setNames(
          c("new", instruments_data$observers$observer_id),
          c("Add new observer", instruments_data$observers$observer_string)
        )

        # look for a cookie with the last observer ID
        shinyjs::runjs(sprintf(
          "var lastObserverId = Cookies.get('last_observer_id');
      Shiny.setInputValue('%s', lastObserverId);",
          ns('last_observer_id')
        ))

        output$observer <- renderUI({
          selectizeInput(
            ns("observer"),
            label = "Calibrator name",
            choices = select_data$recorder,
            options = selectize_empty_options()
          )
        })
        output$add_sensor_name <- renderUI({
          selectizeInput(
            ns("add_sensor_name"),
            label = "What's your name?",
            choices = select_data$recorder,
            options = selectize_empty_options()
          )
        })
        output$sensor_change_name <- renderUI({
          selectizeInput(
            ns("sensor_change_name"),
            label = "Observer name",
            choices = select_data$recorder,
            options = selectize_empty_options()
          )
        })
        output$maintain_recorder <- renderUI({
          selectizeInput(
            ns("maintain_recorder"),
            label = "Maintenance recorder",
            choices = select_data$recorder,
            options = selectize_empty_options()
          )
        })
        output$ID_sensor_holder <- renderUI({
          div(
            selectizeInput(
              ns("ID_sensor_holder"),
              label = "Logger/bulkhead/sonde serial #",
              choices = c("", instruments_data$others$serial_no)
            ),
            style = "color: white; background-color: blue;"
          )
        })
        output$ID_handheld_meter <- renderUI({
          div(
            selectizeInput(
              ns("ID_handheld_meter"),
              label = "Handheld serial # (if applicable)",
              choices = c("NA", instruments_data$handhelds$serial_no)
            ),
            style = "color: white; background-color: green;"
          )
        })

        tmp <- stats::setNames(
          c("new", sensors_data$sensor_types$sensor_type_id),
          c("Add new type", sensors_data$sensor_types$sensor_type)
        )
        updateSelectizeInput(
          session,
          "change_sensor",
          choices = tmp,
          selected = character(0)
        )
        updateSelectizeInput(
          session,
          "add_sensor_type_dropdown",
          choices = tmp,
          selected = character(0)
        )

        # Create initial tables for managing incomplete calibrations
        incomplete_calibrations <- calibrations$calibrations[
          calibrations$calibrations$complete == FALSE,
        ] # find out if any calibrations are labelled as incomplete
        calibrations$incomplete_calibrations <- incomplete_calibrations
        if (nrow(incomplete_calibrations) > 0) {
          alert(
            title = "Incomplete calibrations found!",
            text = "Go to to the page 'View unfinished calibrations' to restart or delete them.",
            type = "warning",
            timer = 4000
          )
          incomplete_calibrations <- dplyr::left_join(
            incomplete_calibrations,
            instruments_data$observers[, c("observer_id", "observer_string")],
            by = dplyr::join_by(observer == observer_id)
          )
          complete$incomplete <- data.frame(
            "Calibrator" = as.vector(incomplete_calibrations$observer_string),
            "Date/time UTC" = incomplete_calibrations$obs_datetime,
            "Purpose" = incomplete_calibrations$purpose,
            check.names = FALSE
          )
        } else {
          # Make a data.frame with no calibrations
          complete$incomplete <- data.frame(
            "Calibrator" = "No unsaved records!",
            "Date/Time UTC" = "No unsaved records!",
            "Purpose" = "No unsaved records!",
            check.names = FALSE
          )
        }
        restarted$initialized <- TRUE
      }
    })

    # Modals and observers to add new observers and sensor types ########################################
    ## Add new observers  ################################################
    observer_inputs <- c(
      "observer",
      "sensor_change_name",
      "add_sensor_name"
    )
    lapply(observer_inputs, function(inp) {
      observeEvent(
        input[[inp]],
        {
          if (input[[inp]] == "new") {
            showModal(modalDialog(
              title = "Add new observer",
              textInput(ns("new_observer_first"), "First name"),
              textInput(ns("new_observer_last"), "Last name"),
              textInput(ns("new_observer_org"), "Organization"),
              actionButton(ns("add_new_observer"), "Add new observer")
            ))
          } else {
            selected_id <- input[[inp]]
            shinyjs::runjs(paste0(
              "Cookies.set('last_observer_id', '",
              selected_id,
              "', { expires: 30 });"
            ))
          }
        },
        ignoreInit = TRUE,
        ignoreNULL = TRUE
      )
    })

    observeEvent(
      input$add_new_observer,
      {
        # Ensure that a first and last name are entered
        if (
          input$new_observer_first == "" |
            input$new_observer_last == "" |
            input$new_observer_org == ""
        ) {
          alert(
            title = "Error",
            text = "Please enter a first, last name, and organization.",
            type = "error",
            timer = 4000
          )
          return()
        }
        # Add the new observer to the database
        DBI::dbExecute(
          session$userData$AquaCache,
          "INSERT INTO observers (observer_first, observer_last, organization) VALUES ($1, $2, $3)",
          params = list(
            input$new_observer_first,
            input$new_observer_last,
            input$new_observer_org
          )
        )
        # Update the observers data.frame and selectizeInputs
        instruments_data$observers <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT * FROM observers"
        )
        instruments_data$observers$observer_string <- paste0(
          instruments_data$observers$observer_first,
          " ",
          instruments_data$observers$observer_last,
          " (",
          instruments_data$observers$organization,
          ")"
        )
        select_data$recorder <- stats::setNames(
          c("new", instruments_data$observers$observer_id),
          c("Add new observer", instruments_data$observers$observer_string)
        )

        selected_id <- max(instruments_data$observers$observer_id)

        output$observer <- renderUI({
          selectizeInput(
            ns("observer"),
            label = "Calibrator name",
            choices = select_data$recorder,
            options = selectize_empty_options(clear = FALSE),
            selected = selected_id
          )
        })
        output$add_sensor_name <- renderUI({
          selectizeInput(
            ns("add_sensor_name"),
            label = "What's your name?",
            choices = select_data$recorder,
            options = selectize_empty_options(clear = FALSE),
            selected = selected_id
          )
        })
        output$sensor_change_name <- renderUI({
          selectizeInput(
            ns("sensor_change_name"),
            label = "Observer name",
            choices = select_data$recorder,
            options = selectize_empty_options(clear = FALSE),
            selected = selected_id
          )
        })
        shinyjs::runjs(paste0(
          "Cookies.set('last_observer_id', '",
          selected_id,
          "', { expires: 30 });"
        ))
        removeModal()
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    ## Add new sensor type ################################################
    observeEvent(
      input$add_new_sensor_type,
      {
        # Ensure that a sensor type is entered
        if (input$new_sensor_type == "") {
          alert(
            title = "Error",
            text = "Please enter a sensor type, description optional.",
            type = "error",
            timer = 4000
          )
          return()
        }
        DBI::dbExecute(
          session$userData$AquaCache,
          "INSERT INTO sensor_types (sensor_type, sensor_type_description) VALUES ($1, $2)",
          params = list(
            input$new_sensor_type,
            if (input$new_sensor_desc == "") NA else input$new_sensor_desc
          )
        )
        sensors_data$sensor_types <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT * FROM sensor_types"
        )
        tmp <- stats::setNames(
          c("new", sensors_data$sensor_types$sensor_type_id),
          c("Add new type", sensors_data$sensor_types$sensor_type)
        )
        updateSelectizeInput(
          session,
          "change_sensor",
          choices = tmp,
          selected = unname(tmp[names(tmp) == input$new_sensor_type])
        )
        updateSelectizeInput(
          session,
          "add_sensor_type_dropdown",
          choices = tmp,
          selected = unname(tmp[names(tmp) == input$new_sensor_type])
        )
        removeModal()
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    # Show/hide optional as-left fields for each calibration type #########################################
    observe({
      sync_optional_post_fields(
        button_id = "show_post_ph",
        field_ids = c("ph1_post_val", "ph2_post_val", "ph3_post_val"),
        allow_post = !is_check_only_mode("ph"),
        show_fields = show_requested("show_post_ph")
      )
    })
    observe({
      sync_optional_post_fields(
        button_id = "show_post_orp",
        field_ids = c("orp_post_mv"),
        allow_post = !is_check_only_mode("orp"),
        show_fields = show_requested("show_post_orp")
      )
    })
    observe({
      sync_optional_post_fields(
        button_id = "show_post_turb",
        field_ids = c("turb1_post", "turb2_post"),
        allow_post = !is_check_only_mode("turbidity"),
        show_fields = show_requested("show_post_turb")
      )
    })
    observe({
      sync_optional_post_fields(
        button_id = "show_post_do",
        field_ids = c("baro_press_post", "do_post_prct", "do_post"),
        allow_post = !is_check_only_mode("do"),
        show_fields = show_requested("show_post_do")
      )
    })
    observe({
      sync_spc_labels(
        point_count = current_spc_point_count(),
        non_specific = isTRUE(input$spc_or_not)
      )
      sync_spc_post_visibility(
        point_count = current_spc_point_count(),
        show_posts = show_requested("show_post_spc")
      )
    })

    empty_restarted_cal_table <- function() {
      data.frame(
        "Saved records (recovered session)" = "",
        check.names = FALSE
      )
    }
    remove_calibration_entry <- function(tbl, label) {
      if (is.null(tbl) || nrow(tbl) == 0) {
        return(tbl)
      }
      tbl[tbl[, 1] != label, , drop = FALSE]
    }
    latest_sensor_event_matches <- function(
      obs_datetimes,
      reference_datetime,
      tolerance_secs = 5
    ) {
      if (length(obs_datetimes) == 0 || all(is.na(obs_datetimes))) {
        return(FALSE)
      }
      latest_obs_datetime <- max(obs_datetimes, na.rm = TRUE)
      abs(
        as.numeric(
          difftime(latest_obs_datetime, reference_datetime, units = "secs")
        )
      ) <=
        tolerance_secs
    }
    current_temp_reference <- function() {
      if (
        !is.null(calibration_data$temp) &&
          nrow(calibration_data$temp) > 0 &&
          !is.na(calibration_data$temp$temp_reference[1])
      ) {
        return(calibration_data$temp$temp_reference[1])
      }
      input$temp_reference
    }
    current_spc_point_count <- function() {
      point_value <- input$spc_points
      point_count <- suppressWarnings(as.integer(point_value[[1]]))
      if (
        !length(point_count) || is.na(point_count) || !(point_count %in% 1:3)
      ) {
        point_count <- 2L
      }
      if (!spc_supports_extended_schema) {
        point_count <- 2L
      }
      point_count
    }
    spc_point_role <- function(
      point_index,
      point_count = current_spc_point_count()
    ) {
      point_roles <- switch(
        as.character(point_count),
        "1" = c("Standard"),
        "2" = c("Low-Range", "High-Range"),
        "3" = c("Low-Range", "Mid-Range", "High-Range")
      )
      if (length(point_roles) < point_index) {
        return(paste("Point", point_index))
      }
      point_roles[[point_index]]
    }
    spc_standard_label <- function(
      point_index,
      point_count = current_spc_point_count()
    ) {
      point_role <- spc_point_role(point_index, point_count)
      if (identical(point_role, "Standard")) {
        return("SpC Standard")
      }
      paste("SpC", point_role, "Standard")
    }
    spc_measurement_label <- function(
      point_index,
      suffix,
      point_count = current_spc_point_count(),
      non_specific = isTRUE(input$spc_or_not)
    ) {
      point_role <- spc_point_role(point_index, point_count)
      prefix <- if (non_specific) {
        "Conductivity"
      } else {
        "SpC"
      }
      if (identical(point_role, "Standard")) {
        return(paste(prefix, suffix))
      }
      paste(prefix, point_role, suffix)
    }
    spc_pre_label <- function(
      point_index,
      point_count = current_spc_point_count(),
      non_specific = isTRUE(input$spc_or_not),
      isolate_input = FALSE
    ) {
      spc_measurement_label(
        point_index,
        if (is_check_only_mode("spc", isolate_input = isolate_input)) {
          "Observed value"
        } else {
          "As-found value"
        },
        point_count = point_count,
        non_specific = non_specific
      )
    }
    spc_post_label <- function(
      point_index,
      point_count = current_spc_point_count(),
      non_specific = isTRUE(input$spc_or_not)
    ) {
      spc_measurement_label(
        point_index,
        "As-left value",
        point_count = point_count,
        non_specific = non_specific
      )
    }
    sync_spc_labels <- function(
      point_count = current_spc_point_count(),
      non_specific = isTRUE(input$spc_or_not),
      isolate_input = FALSE
    ) {
      for (i in 1:3) {
        updateNumericInput(
          session,
          paste0("spc", i, "_std"),
          label = spc_standard_label(i, point_count)
        )
        updateNumericInput(
          session,
          paste0("spc", i, "_pre"),
          label = spc_pre_label(
            i,
            point_count = point_count,
            non_specific = non_specific,
            isolate_input = isolate_input
          )
        )
        updateNumericInput(
          session,
          paste0("spc", i, "_post"),
          label = spc_post_label(
            i,
            point_count = point_count,
            non_specific = non_specific
          )
        )
      }
    }
    convert_to_spc <- function(value) {
      temp_reference <- current_temp_reference()
      if (length(temp_reference) == 0 || is.na(temp_reference)) {
        stop(
          "Temperature check must be saved first when entering non-specific conductivity."
        )
      }
      value / (1 + 0.02 * (temp_reference - 25))
    }
    default_spc_post_value <- function(value) {
      if (isTRUE(input$spc_or_not)) {
        return(round(convert_to_spc(value), 0))
      }
      value
    }
    current_spc_post_input <- function(point_index) {
      value <- input[[paste0("spc", point_index, "_post")]]
      if (!length(value) || all(is.na(value))) {
        return(default_spc_post_value(input[[paste0(
          "spc",
          point_index,
          "_std"
        )]]))
      }
      value
    }
    sync_spc_post_visibility <- function(
      point_count = 2L,
      show_posts = FALSE,
      isolate_input = FALSE
    ) {
      sync_optional_post_fields(
        button_id = "show_post_spc",
        field_ids = paste0("spc", 1:3, "_post"),
        allow_post = !is_check_only_mode("spc", isolate_input = isolate_input),
        show_fields = isTRUE(show_posts)
      )
      if (
        isTRUE(show_posts) &&
          !is_check_only_mode("spc", isolate_input = isolate_input)
      ) {
        for (id in paste0("spc", seq_len(point_count), "_post")) {
          shinyjs::show(id)
        }
        for (id in paste0("spc", setdiff(1:3, seq_len(point_count)), "_post")) {
          shinyjs::hide(id)
        }
      }
    }
    saved_spc_measurement <- function(point_index, measurement) {
      input_name <- paste0("spc", point_index, "_", measurement)
      value <- if (identical(measurement, "post")) {
        current_spc_post_input(point_index)
      } else {
        input[[input_name]]
      }
      if (isTRUE(input$spc_or_not)) {
        return(convert_to_spc(value))
      }
      value
    }
    build_spc_record <- function() {
      point_count <- current_spc_point_count()
      spc_check_only <- is_check_only_mode("spc")
      if (spc_supports_extended_schema) {
        record <- data.frame(
          calibration_id = calibration_data$next_id,
          calibration_points = point_count,
          spc1_std = input$spc1_std,
          spc2_std = if (point_count >= 2) input$spc2_std else NA_real_,
          spc1_pre = saved_spc_measurement(1, "pre"),
          spc2_pre = if (point_count >= 2) {
            saved_spc_measurement(2, "pre")
          } else {
            NA_real_
          },
          spc1_post = if (spc_check_only) {
            NA_real_
          } else {
            saved_spc_measurement(1, "post")
          },
          spc2_post = if (point_count >= 2) {
            if (spc_check_only) {
              NA_real_
            } else {
              saved_spc_measurement(2, "post")
            }
          } else {
            NA_real_
          },
          spc3_std = if (point_count >= 3) input$spc3_std else NA_real_,
          spc3_pre = if (point_count >= 3) {
            saved_spc_measurement(3, "pre")
          } else {
            NA_real_
          },
          spc3_post = if (point_count >= 3) {
            if (spc_check_only) {
              NA_real_
            } else {
              saved_spc_measurement(3, "post")
            }
          } else {
            NA_real_
          }
        )
        record$check_only <- spc_check_only

        return(record)
      }
      data.frame(
        calibration_id = calibration_data$next_id,
        spc1_std = input$spc1_std,
        spc1_pre = saved_spc_measurement(1, "pre"),
        spc1_post = if (spc_check_only) {
          saved_spc_measurement(1, "pre")
        } else {
          saved_spc_measurement(1, "post")
        },
        spc2_std = input$spc2_std,
        spc2_pre = saved_spc_measurement(2, "pre"),
        spc2_post = if (spc_check_only) {
          saved_spc_measurement(2, "pre")
        } else {
          saved_spc_measurement(2, "post")
        }
      )
    }
    sql_string_or_null <- function(value) {
      if (length(value) == 0 || all(is.na(value))) {
        return("NULL")
      }
      as.character(
        DBI::dbQuoteString(
          session$userData$AquaCache,
          as.character(value[1])
        )
      )
    }
    sql_numeric_or_null <- function(value) {
      if (length(value) == 0 || all(is.na(value))) {
        return("NULL")
      }
      as.character(as.numeric(value[[1]]))
    }
    sql_boolean_literal <- function(value) {
      if (isTRUE(value)) {
        "TRUE"
      } else {
        "FALSE"
      }
    }
    session$onFlushed(
      function() {
        if (!spc_supports_extended_schema) {
          updateRadioButtons(
            session,
            "spc_points",
            choices = c("2 point" = 2),
            selected = 2
          )
        }
        sync_spc_labels(
          point_count = 2L,
          non_specific = FALSE,
          isolate_input = TRUE
        )
        sync_spc_post_visibility(
          point_count = 2L,
          isolate_input = TRUE
        )
      },
      once = TRUE
    )
    sync_ph_labels <- function() {
      std1 <- if (!is.null(input$ph1_std) && length(input$ph1_std)) {
        input$ph1_std
      } else {
        4
      }
      std2 <- if (!is.null(input$ph2_std) && length(input$ph2_std)) {
        input$ph2_std
      } else {
        7
      }
      std3 <- if (!is.null(input$ph3_std) && length(input$ph3_std)) {
        input$ph3_std
      } else {
        10
      }
      updateNumericInput(session, "ph1_pre_val", label = ph_pre_label(std1))
      updateNumericInput(session, "ph2_pre_val", label = ph_pre_label(std2))
      updateNumericInput(session, "ph3_pre_val", label = ph_pre_label(std3))
      updateNumericInput(session, "ph1_post_val", label = ph_post_label(std1))
      updateNumericInput(session, "ph2_post_val", label = ph_post_label(std2))
      updateNumericInput(session, "ph3_post_val", label = ph_post_label(std3))
      updateNumericInput(session, "ph1_mv", label = paste0("pH ", std1, " mV"))
      updateNumericInput(session, "ph2_mv", label = paste0("pH ", std2, " mV"))
      updateNumericInput(session, "ph3_mv", label = paste0("pH ", std3, " mV"))
    }
    sync_orp_labels <- function() {
      updateNumericInput(session, "orp_pre_mv", label = orp_pre_label())
      updateNumericInput(session, "orp_post_mv", label = "ORP As-left mV")
    }
    sync_turb_labels <- function() {
      updateNumericInput(
        session,
        "turb1_pre",
        label = turb_pre_label("Low Turb")
      )
      updateNumericInput(
        session,
        "turb2_pre",
        label = turb_pre_label("High Turb")
      )
      updateNumericInput(
        session,
        "turb1_post",
        label = "Low Turb As-left value"
      )
      updateNumericInput(
        session,
        "turb2_post",
        label = "High Turb As-left value"
      )
    }
    sync_do_labels <- function() {
      updateNumericInput(
        session,
        "baro_press_pre",
        label = do_pre_label("baro pressure (mmHg)")
      )
      updateNumericInput(
        session,
        "baro_press_post",
        label = "As-left baro pressure (mmHg)"
      )
      updateNumericInput(
        session,
        "do_pre_prct",
        label = do_pre_label("DO % LOCAL")
      )
      updateNumericInput(
        session,
        "do_post_prct",
        label = "DO As-left % LOCAL"
      )
      updateNumericInput(
        session,
        "do_pre",
        label = do_pre_label("DO mg/l")
      )
      updateNumericInput(
        session,
        "do_post",
        label = "DO As-left mg/l"
      )
    }

    # Render messages and notes, show/hide messages based on selection ################################################
    # Initiate data.frame to populate with saved calibrations later
    send_table$saved <- data.frame(
      "Saved records" = "Nothing saved yet",
      check.names = FALSE
    ) #Title is modified later for clarity if user want to restart a cal
    send_table$restarted_cal <- empty_restarted_cal_table()
    # Initiate data.frame for instrument details when maintaining
    sensors_data$instrument_table <- data.frame(
      "Select an instrument first" = NA,
      check.names = FALSE
    )
    sensors_data$sensor1_details <- data.frame(
      "Nothing to show yet!" = NA,
      check.names = FALSE
    )
    output$sensor1_details <- renderTable({
      sensors_data$sensor1_details
    })

    messages$instrument_reminder <- paste(
      "Add your instrument in the separate",
      "'Create/modify instruments' module if it is not listed here."
    )
    output$instrument_reminder <- renderText({
      messages$instrument_reminder
    })
    output$sensors_reminder <- renderText({
      messages$instrument_reminder
    })
    messages$add_sensor_note <- "Caution: ADDS new sensor slot. To CHANGE sensor type, click on the sensor button for new options."
    output$add_sensor_note <- renderText({
      messages$add_sensor_note
    })
    messages$sensor_change_note <- "<br>Only modify the fields required:<br>If logging maintenance without changing a sensor, only add a descriptive note.<br>If changing sensors while keeping same type, change serial number and add a note only.<br>If changing sensor type, edit all three fields.<br><br>"
    output$sensor_change_note <- renderUI({
      HTML(messages$sensor_change_note)
    })
    messages$ph_mV_note <- "<b><br><br>pH mV standards:&nbsp;&nbsp;&nbsp;pH7 = +- 50;&nbsp;&nbsp;&nbsp;pH4 = pH 7 value + 165 to 180;&nbsp;&nbsp;&nbsp;pH 10 = pH 7 value - 165 to 180</b>"
    output$ph_mV_note <- renderUI({
      HTML(messages$ph_mV_note)
    })
    messages$ORP_molarity_note <- "<b><br><br>If using combination pH/ORP electrode adjust pH first.<br><br>Use proper standard scale: YSI Pro Series use 3.5M KCl scale, YSI sondes use 4M KCl scale.</b>"
    output$ORP_molarity_note <- renderUI({
      HTML(messages$ORP_molarity_note)
    })

    observeEvent(input$selection, {
      if (input$selection != "Basic calibration info" & !complete$basic) {
        alert(
          title = "Stop! You must save basic record info first or load an incomplete record!",
          type = "error",
          timer = 2000
        )
        updateSelectizeInput(
          session,
          "selection",
          selected = "Basic calibration info"
        )
        return()
      }
      if (input$selection == "Basic calibration info") {
        shinyjs::show("calibration_instruments_table")
      } else {
        shinyjs::hide("calibration_instruments_table")
      }
      if (
        input$selection == "pH calibration" &
          input$tab_panel == "Checks / calibrations"
      ) {
        shinyjs::show("ph_mV_note")
      } else {
        shinyjs::hide("ph_mV_note")
      }
      if (
        input$selection == "ORP calibration" &
          input$tab_panel == "Checks / calibrations"
      ) {
        shinyjs::show("ORP_molarity_note")
      } else {
        shinyjs::hide("ORP_molarity_note")
      }
      if (input$selection == "pH calibration") {
        sync_ph_labels()
        sync_optional_post_fields(
          button_id = "show_post_ph",
          field_ids = c("ph1_post_val", "ph2_post_val", "ph3_post_val"),
          allow_post = !is_check_only_mode("ph"),
          show_fields = show_requested("show_post_ph")
        )
      } else if (input$selection == "ORP calibration") {
        sync_orp_labels()
        sync_optional_post_fields(
          button_id = "show_post_orp",
          field_ids = c("orp_post_mv"),
          allow_post = !is_check_only_mode("orp"),
          show_fields = show_requested("show_post_orp")
        )
      } else if (input$selection == "Conductivity calibration") {
        sync_spc_labels(
          point_count = current_spc_point_count(),
          non_specific = isTRUE(input$spc_or_not)
        )
        sync_spc_post_visibility(
          point_count = current_spc_point_count(),
          show_posts = show_requested("show_post_spc")
        )
      } else if (input$selection == "Turbidity calibration") {
        sync_turb_labels()
        sync_optional_post_fields(
          button_id = "show_post_turb",
          field_ids = c("turb1_post", "turb2_post"),
          allow_post = !is_check_only_mode("turbidity"),
          show_fields = show_requested("show_post_turb")
        )
      } else if (input$selection == "DO calibration") {
        sync_do_labels()
        sync_optional_post_fields(
          button_id = "show_post_do",
          field_ids = c("baro_press_post", "do_post_prct", "do_post"),
          allow_post = !is_check_only_mode("do"),
          show_fields = show_requested("show_post_do")
        )
      }
    })

    observeEvent(
      input$timezone,
      {
        shift_obs_datetime_timezone(normalize_input_timezone(input$timezone))
      },
      ignoreInit = TRUE
    )

    # Create reset functions for each calibration type ################################################
    reset_basic <- function(keep_observer = FALSE) {
      if (!keep_observer) {
        output$observer <- renderUI({
          selectizeInput(
            ns("observer"),
            label = "Calibrator name",
            choices = select_data$recorder,
            options = selectize_empty_options()
          )
        })
      }
      shinyWidgets::updateAirDateInput(
        session,
        "obs_datetime",
        value = default_obs_datetime(),
        tz = air_datetime_widget_timezone(input$timezone)
      )
      output$ID_sensor_holder <- renderUI({
        div(
          selectizeInput(
            ns("ID_sensor_holder"),
            label = "Logger/bulkhead/sonde serial #",
            choices = c("", instruments_data$others$serial_no)
          ),
          style = "color: white; background-color: blue;"
        )
      })
      output$ID_handheld_meter <- renderUI({
        div(
          selectizeInput(
            ns("ID_handheld_meter"),
            label = "Handheld serial # (if applicable)",
            choices = c("NA", instruments_data$handhelds$serial_no)
          ),
          style = "color: white; background-color: green;"
        )
      })
    }
    reset_ph <- function() {
      updateRadioButtons(session, "ph_entry_mode", selected = "check")

      updateNumericInput(
        session,
        "ph1_std",
        label = "Low pH solution value",
        value = 4
      )
      updateNumericInput(
        session,
        "ph2_std",
        label = "Neutral pH solution value",
        value = 7
      )
      updateNumericInput(
        session,
        "ph3_std",
        label = "High pH solution value",
        value = 10
      )
      updateNumericInput(
        session,
        "ph1_pre_val",
        label = "pH 4 Observed value",
        value = NA
      )
      updateNumericInput(
        session,
        "ph2_pre_val",
        label = "pH 7 Observed value",
        value = NA
      )
      updateNumericInput(
        session,
        "ph3_pre_val",
        label = "pH 10 Observed value",
        value = NA
      )
      updateNumericInput(
        session,
        "ph1_post_val",
        label = "pH 4 As-left value",
        value = 4
      )
      updateNumericInput(session, "ph1_mv", label = "pH 4 mV", value = NA)
      updateNumericInput(
        session,
        "ph2_post_val",
        label = "pH 7 As-left value",
        value = 7
      )
      updateNumericInput(session, "ph2_mv", label = "pH 7 mV", value = NA)
      updateNumericInput(
        session,
        "ph3_post_val",
        label = "pH 10 As-left value",
        value = 10
      )
      updateNumericInput(session, "ph3_mv", label = "pH 10 mV", value = NA)
      sync_ph_labels()
      shinyjs::hide("delete_ph")
    }
    reset_temp <- function() {
      updateTextInput(
        session,
        "temp_reference_desc",
        label = "Temp Reference Type",
        value = "Lab thermometer"
      )
      updateNumericInput(
        session,
        "temp_reference",
        label = "Reference Temp",
        value = NA
      )
      updateNumericInput(
        session,
        "temp_observed",
        label = "Sensor Temp",
        value = NA
      )
      calibration_data$temp <- NULL
      shinyjs::hide("delete_temp")
    }
    reset_orp <- function() {
      updateRadioButtons(session, "orp_entry_mode", selected = "check")

      updateNumericInput(
        session,
        "orp_std",
        label = "ORP Standard solution mV",
        value = NA
      )
      updateNumericInput(
        session,
        "orp_pre_mv",
        label = "ORP Observed mV",
        value = NA
      )
      updateNumericInput(
        session,
        "orp_post_mv",
        label = "ORP As-left mV",
        value = NA
      )
      sync_orp_labels()
      shinyjs::hide("delete_orp")
    }
    reset_spc <- function() {
      updateCheckboxInput(session, "spc_or_not", value = FALSE)
      updateRadioButtons(session, "spc_points", selected = 2)
      updateRadioButtons(session, "spc_entry_mode", selected = "check")

      updateNumericInput(
        session,
        "spc1_std",
        label = "SpC Low-Range Standard",
        value = "0"
      )
      updateNumericInput(
        session,
        "spc1_pre",
        label = "SpC Low-Range Observed value",
        value = NA
      )
      updateNumericInput(
        session,
        "spc1_post",
        label = "SpC Low-Range As-left value",
        value = "0"
      )
      updateNumericInput(
        session,
        "spc2_std",
        label = "SpC High-Range Standard",
        value = "1413"
      )
      updateNumericInput(
        session,
        "spc2_pre",
        label = "SpC High-Range Observed value",
        value = NA
      )
      updateNumericInput(
        session,
        "spc2_post",
        label = "SpC High-Range As-left value",
        value = "1413"
      )
      updateNumericInput(
        session,
        "spc3_std",
        label = "SpC High-Range Standard",
        value = "12880"
      )
      updateNumericInput(
        session,
        "spc3_pre",
        label = "SpC High-Range Observed value",
        value = NA
      )
      updateNumericInput(
        session,
        "spc3_post",
        label = "SpC High-Range As-left value",
        value = "12880"
      )
      sync_spc_labels(point_count = 2L, non_specific = FALSE)
      sync_spc_post_visibility(point_count = 2L)
      shinyjs::hide("delete_spc")
    }
    reset_turb <- function() {
      updateRadioButtons(session, "turb_entry_mode", selected = "check")

      updateNumericInput(
        session,
        "turb1_std",
        label = "Low Turb Standard Value",
        value = "0"
      )
      updateNumericInput(
        session,
        "turb2_std",
        label = "High Turb Standard Value",
        value = "124"
      )
      updateNumericInput(
        session,
        "turb1_pre",
        label = "Low Turb Observed value",
        value = NA
      )
      updateNumericInput(
        session,
        "turb2_pre",
        label = "High Turb Observed value",
        value = NA
      )
      updateNumericInput(
        session,
        "turb1_post",
        label = "Low Turb As-left value",
        value = "0"
      )
      updateNumericInput(
        session,
        "turb2_post",
        label = "High Turb As-left value",
        value = "124"
      )
      sync_turb_labels()
      calibration_data$turb <- NULL
      shinyjs::hide("delete_turb")
    }
    reset_do <- function() {
      updateRadioButtons(session, "do_entry_mode", selected = "check")

      updateNumericInput(
        session,
        "baro_press_pre",
        label = "Observed baro pressure (mmHg)",
        value = NA
      )
      updateNumericInput(
        session,
        "baro_press_post",
        label = "As-left baro pressure (mmHg)",
        value = NA
      )
      updateNumericInput(
        session,
        "do_pre_prct",
        label = "Observed DO %",
        value = NA
      )
      updateNumericInput(
        session,
        "do_post_prct",
        label = "DO As-left %",
        value = NA
      )
      updateNumericInput(
        session,
        "do_pre",
        label = "Observed DO mg/l",
        value = NA
      )
      updateNumericInput(
        session,
        "do_post",
        label = "DO As-left mg/l",
        value = NA
      )
      sync_do_labels()
      shinyjs::hide("delete_do")
    }
    reset_depth <- function() {
      updateRadioButtons(
        session,
        inputId = "depth_check_ok",
        selected = "FALSE"
      )
      updateRadioButtons(
        session,
        inputId = "depth_changes_ok",
        selected = "FALSE"
      )
      shinyjs::hide("delete_depth")
    }

    # observeEvents to translate rows clicked into updated inputs, applies to several tables. ############################
    ## Calibration instrument(s) selection table
    observeEvent(input$calibration_instruments_table_rows_selected, {
      selected_rows <- input$calibration_instruments_table_rows_selected
      selected_rows <- selected_rows[selected_rows != 0]

      if (length(selected_rows) > 2) {
        proxy <- DT::dataTableProxy(
          "calibration_instruments_table",
          session = session
        )
        DT::selectRows(proxy, NULL)
        alert(
          "Choose at most two instruments.",
          text = "Select one logger/bulkhead/sonde and, if needed, one handheld meter.",
          type = "error",
          timer = 2500
        )
        selected_rows <- integer(0)
      }

      selected_serials <- character(0)
      if (length(selected_rows) > 0) {
        selected_serials <- as.character(
          instruments_data$calibrate_instruments[selected_rows, "serial_no"]
        )
      }

      output$ID_sensor_holder <- renderUI({
        div(
          selectizeInput(
            ns("ID_sensor_holder"),
            label = "Logger/bulkhead/sonde serial #",
            choices = c("", instruments_data$others$serial_no),
            selected = if (length(selected_serials) >= 1) {
              selected_serials[[1]]
            } else {
              ""
            }
          ),
          style = "color: white; background-color: blue;"
        )
      })
      output$ID_handheld_meter <- renderUI({
        div(
          selectizeInput(
            ns("ID_handheld_meter"),
            label = "Handheld serial # (if applicable)",
            choices = c("NA", instruments_data$handhelds$serial_no),
            selected = if (length(selected_serials) >= 2) {
              selected_serials[[2]]
            } else {
              "NA"
            }
          ),
          style = "color: white; background-color: green;"
        )
      })
    })

    ## Incomplete calibrations selection table
    observeEvent(input$incomplete_table_rows_selected, {
      invisible(selected_incomplete_row())
    })

    ## Sensor/array maintenance and changes instrument selection table
    observeEvent(input$manage_sensors_table_rows_selected, {
      updateSelectizeInput(
        session,
        "maintain_serial",
        selected = instruments_data$maintainable_sensors[
          input$manage_sensors_table_rows_selected[1],
          "serial_no"
        ]
      )
    })

    ## Instrument maintenance table
    observeEvent(
      input$maintain_instr_table_rows_selected,
      {
        # Get the maintenance data for that instrument and render the table
        instr_id <- instruments_data$maintain_instruments[
          input$maintain_instr_table_rows_selected[1],
          "instrument_id"
        ]
        instruments_data$instrument_maintenance_selected_id <- instr_id
        temp <- instruments_data$instrument_maintenance[
          instruments_data$instrument_maintenance$instrument_id == instr_id,
          !colnames(instruments_data$instrument_maintenance) %in%
            c("instrument_id", "event_id")
        ]
        names(temp) <- c("Observer", "Date/Time", "Maintenance note")
        output$past_instr_maintenance <- DT::renderDT(
          temp,
          rownames = FALSE,
          selection = "single"
        ) #Table is in the main panel, for maintenance history
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    # Events for recording instrument maintenance (not sensors) ########################################
    instruments_data$instrument_maintenance_update <- FALSE
    observeEvent(
      input$past_instr_maintenance_rows_selected,
      {
        instruments_data$instrument_maintenance_update <- TRUE
        # Load the past maintenance note into the "maintain_comment" textAreaInput, the observer to "maintain_recorder", and the date/time to "maintain_datetime". Modify the "submit_instr_maintain" button to "Update maintenance note".
        temp <- instruments_data$instrument_maintenance[
          instruments_data$instrument_maintenance$instrument_id == instr_id,
        ]
        obs_datetime <- temp[
          input$past_instr_maintenance_rows_selected[1],
          "obs_datetime"
        ]
        instruments_data$instrument_maintenance_update_datetime <- obs_datetime
        observer <- temp[
          input$past_instr_maintenance_rows_selected[1],
          "observer"
        ]
        instrument_id <- temp[
          input$past_instr_maintenance_rows_selected[1],
          "instrument_id"
        ]
        selected_event <- temp[
          temp$obs_datetime == obs_datetime &
            temp$observer == observer &
            temp$instrument_id == instrument_id,
        ]
        updateTextAreaInput(
          session,
          "maintain_comment",
          value = selected_event$note
        )
        updateSelectizeInput(
          session,
          "maintain_recorder",
          selected = selected_event$observer
        )
        updateActionButton(
          session,
          "submit_instr_maintain",
          label = "UPDATE maintenance event (use 'Clear selection' button above to cancel changes)"
        )
        shinyjs::show("clear_selection")
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$clear_selection,
      {
        updateTextAreaInput(session, "maintain_comment", value = "")
        updateSelectizeInput(
          session,
          "maintain_recorder",
          selected = input$last_observer_id
        )
        updateActionButton(
          session,
          "submit_instr_maintain",
          label = "Save new maintenance event"
        )
        shinyjs::hide("clear_selection")
        instruments_data$instrument_maintenance_update <- FALSE

        # Re-render the table to clear the selection
        temp <- instruments_data$instrument_maintenance[
          instruments_data$instrument_maintenance$instrument_id ==
            instruments_data$instrument_maintenance_selected_id,
          !colnames(instruments_data$instrument_maintenance) %in%
            c("instrument_id", "event_id")
        ]
        names(temp) <- c("Observer", "Date/Time", "Maintenance note")
        output$past_instr_maintenance <- DT::renderDT(
          temp,
          rownames = FALSE,
          selection = "single"
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$submit_instr_maintain,
      {
        if (instruments_data$instrument_maintenance_update) {
          #Edit an existing record
          DBI::dbExecute(
            session$userData$AquaCache,
            paste0(
              "UPDATE instrument_maintenance SET observer = '",
              input$maintain_recorder,
              "', note = '",
              input$maintain_comment,
              "' WHERE instrument_id = ",
              instruments_data$instrument_maintenance_selected_id,
              " AND obs_datetime = '",
              instruments_data$instrument_maintenance_update_datetime,
              "'"
            )
          )
          alert(
            title = "Success",
            text = "Maintenance record updated successfully.",
            timer = 2000,
            type = "success"
          )
        } else {
          # Create a new record
          temp <- data.frame(
            instrument_id = instruments_data$instrument_maintenance_selected_id,
            obs_datetime = .POSIXct(Sys.time(), tz = "UTC"),
            observer = input$maintain_recorder,
            note = input$maintain_comment
          )
          DBI::dbWriteTable(
            session$userData$AquaCache,
            "instrument_maintenance",
            temp,
            append = TRUE
          )
          alert(
            title = "Success",
            text = "Maintenance record saved successfully.",
            timer = 2000,
            type = "success"
          )
        }
        updateTextAreaInput(session, "maintain_comment", value = "")
        instruments_data$instrument_maintenance <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT * FROM instrument_maintenance"
        )
        temp <- instruments_data$instrument_maintenance[
          instruments_data$instrument_maintenance$instrument_id ==
            instruments_data$instrument_maintenance_selected_id,
          !colnames(instruments_data$instrument_maintenance) %in%
            c("instrument_id", "event_id")
        ]
        names(temp) <- c("Observer", "Date/Time", "Maintenance note")
        output$past_instr_maintenance <- DT::renderDT(
          temp,
          rownames = FALSE,
          selection = "single"
        )
        shinyjs::hide("clear_selection")
        updateActionButton(
          session,
          "submit_instr_maintain",
          label = "Save new maintenance event"
        )
        instruments_data$instrument_maintenance_update <- FALSE
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    # Update pH and ORP fields based on standard solution selected ###################################
    observeEvent(
      input$ph1_std,
      {
        sync_ph_labels()
        updateNumericInput(
          session,
          "ph1_post_val",
          label = ph_post_label(input$ph1_std),
          value = input$ph1_std
        )
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$ph2_std,
      {
        sync_ph_labels()
        updateNumericInput(
          session,
          "ph2_post_val",
          label = ph_post_label(input$ph2_std),
          value = input$ph2_std
        )
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$ph3_std,
      {
        sync_ph_labels()
        updateNumericInput(
          session,
          "ph3_post_val",
          label = ph_post_label(input$ph3_std),
          value = input$ph3_std
        )
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$ph_entry_mode,
      {
        sync_ph_labels()
        sync_optional_post_fields(
          button_id = "show_post_ph",
          field_ids = c("ph1_post_val", "ph2_post_val", "ph3_post_val"),
          allow_post = !is_check_only_mode("ph"),
          show_fields = show_requested("show_post_ph")
        )
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$orp_std,
      {
        updateNumericInput(session, "orp_post_mv", value = input$orp_std)
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$orp_entry_mode,
      {
        sync_orp_labels()
        sync_optional_post_fields(
          button_id = "show_post_orp",
          field_ids = c("orp_post_mv"),
          allow_post = !is_check_only_mode("orp"),
          show_fields = show_requested("show_post_orp")
        )
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$turb1_std,
      {
        updateNumericInput(session, "turb1_post", value = input$turb1_std)
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$turb2_std,
      {
        updateNumericInput(session, "turb2_post", value = input$turb2_std)
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$turb_entry_mode,
      {
        sync_turb_labels()
        sync_optional_post_fields(
          button_id = "show_post_turb",
          field_ids = c("turb1_post", "turb2_post"),
          allow_post = !is_check_only_mode("turbidity"),
          show_fields = show_requested("show_post_turb")
        )
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$spc1_std,
      {
        updateNumericInput(
          session,
          "spc1_post",
          value = default_spc_post_value(input$spc1_std)
        )
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$spc2_std,
      {
        updateNumericInput(
          session,
          "spc2_post",
          value = default_spc_post_value(input$spc2_std)
        )
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$spc3_std,
      {
        updateNumericInput(
          session,
          "spc3_post",
          value = default_spc_post_value(input$spc3_std)
        )
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$spc_entry_mode,
      {
        sync_spc_labels(
          point_count = current_spc_point_count(),
          non_specific = isTRUE(input$spc_or_not)
        )
        sync_spc_post_visibility(
          point_count = current_spc_point_count(),
          show_posts = show_requested("show_post_spc")
        )
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$do_entry_mode,
      {
        sync_do_labels()
        sync_optional_post_fields(
          button_id = "show_post_do",
          field_ids = c("baro_press_post", "do_post_prct", "do_post"),
          allow_post = !is_check_only_mode("do"),
          show_fields = show_requested("show_post_do")
        )
      },
      ignoreInit = TRUE
    )

    #observeEvents for when the user selects a particular page ########################################
    observeEvent(input$tab_panel, {
      if (input$tab_panel == "Checks / calibrations") {
        shinyjs::show("calibration_instruments_table")
        shinyjs::show("submit_btn")
        shinyjs::hide("load_sensors")
        shinyjs::hide("sensor1_show")
        shinyjs::addClass("sensor1_show", "hidden")
        shinyjs::hide("sensor2_show")
        shinyjs::addClass("sensor2_show", "hidden")
        shinyjs::hide("sensor3_show")
        shinyjs::addClass("sensor3_show", "hidden")
        shinyjs::hide("sensor4_show")
        shinyjs::addClass("sensor4_show", "hidden")
        shinyjs::hide("sensor5_show")
        shinyjs::addClass("sensor5_show", "hidden")
        shinyjs::hide("sensor6_show")
        shinyjs::addClass("sensor6_show", "hidden")
        shinyjs::hide("sensor7_show")
        shinyjs::addClass("sensor7_show", "hidden")
        shinyjs::hide("sensor8_show")
        shinyjs::addClass("sensor8_show", "hidden")
        shinyjs::hide("add_sensor_slot")
        shinyjs::hide("add_sensor_note")
        shinyjs::hide("sensor_change_note")
        shinyjs::hide("add_sensor_type_dropdown")
        shinyjs::hide("add_sensor_name")
        shinyjs::hide("sensor1_details")
        shinyjs::hide("sensor2_details")
        shinyjs::hide("sensor3_details")
        shinyjs::hide("sensor4_details")
        shinyjs::hide("sensor5_details")
        shinyjs::hide("sensor6_details")
        shinyjs::hide("sensor7_details")
        shinyjs::hide("sensor8_details")
        shinyjs::hide("change_sensor")
        shinyjs::hide("add_sensor_serial")
        shinyjs::hide("new_sensor_serial")
        shinyjs::hide("add_comment")
        shinyjs::hide("sensor_change_name")
        shinyjs::hide("submit_sensor_change")
        output$observer <- renderUI({
          selectizeInput(
            ns("observer"),
            label = "Calibrator name",
            choices = select_data$recorder,
            options = selectize_empty_options()
          )
        })
      } else if (input$tab_panel == "Maintain instruments") {
        instruments_data$maintain_instruments <- instruments_data$sheet[,
          !colnames(instruments_data$sheet) %in%
            c(
              "observer",
              "obs_datetime",
              "retired_by",
              "date_retired",
              "asset_tag",
              "date_in_service",
              "date_purchased",
              "holds_replaceable_sensors",
              "date_end_of_life",
              "purchase_price",
              "takes_measurements",
              "supplier_id",
              "supplier"
            ),
          drop = FALSE
        ]
        temp_maintain <- instruments_data$maintain_instruments[, c(
          "make",
          "model",
          "type",
          "serial_no",
          "owner"
        )]
        output$maintain_instr_table <- DT::renderDT(
          DT::datatable(
            format_owner_column_for_dt(temp_maintain),
            rownames = FALSE,
            selection = "single",
            escape = FALSE
          )
        ) #Table is in the sidebar, for instrument selection
        temp <- data.frame(
          "Observer" = "No one",
          "Date/Time" = "Never",
          "Maintenance note" = "Select an instrument first",
          check.names = FALSE
        )
        output$past_instr_maintenance <- DT::renderDT(
          temp,
          rownames = FALSE,
          selection = "none"
        ) #Table is in the main panel, for maintenance history
        shinyjs::hide("clear_selection")
      } else if (input$tab_panel == "Change/maintain sensors") {
        #reload instruments_data$sheet to mitigate conflicts
        refresh_instruments_sheet()
        temp_table <- instruments_data$maintainable_sensors[, c(
          "make",
          "model",
          "type",
          "serial_no",
          "owner"
        )]
        output$manage_sensors_table <- DT::renderDT(
          DT::datatable(
            format_owner_column_for_dt(temp_table),
            rownames = FALSE,
            selection = "single",
            escape = FALSE
          )
        )
        updateSelectizeInput(
          session,
          "maintain_serial",
          choices = c("", instruments_data$maintainable_sensors$serial_no)
        )
        shinyjs::hide("submit_btn")
        shinyjs::hide("add_sensor_slot")
        shinyjs::hide("add_sensor_name")
        shinyjs::hide("add_sensor_note")
        shinyjs::hide("sensor_change_note")
        shinyjs::hide("new_sensor_serial")
        shinyjs::hide("submit_sensor_change")
        shinyjs::show("load_sensors")
        shinyjs::show("manage_sensors_table")
        updateSelectizeInput(
          session,
          "add_sensor_name",
          choices = select_data$observer
        )
      } else if (input$tab_panel == "View unfinished calibrations") {
        output$incomplete_table <- DT::renderDT(
          complete$incomplete,
          rownames = FALSE,
          selection = "single"
        )
        shinyjs::hide("submit_btn")
        shinyjs::hide("load_sensors")
        shinyjs::hide("sensor1_show")
        shinyjs::addClass("sensor1_show", "hidden")
        shinyjs::hide("sensor2_show")
        shinyjs::addClass("sensor2_show", "hidden")
        shinyjs::hide("sensor3_show")
        shinyjs::addClass("sensor3_show", "hidden")
        shinyjs::hide("sensor4_show")
        shinyjs::addClass("sensor4_show", "hidden")
        shinyjs::hide("sensor5_show")
        shinyjs::addClass("sensor5_show", "hidden")
        shinyjs::hide("sensor6_show")
        shinyjs::addClass("sensor6_show", "hidden")
        shinyjs::hide("sensor7_show")
        shinyjs::addClass("sensor7_show", "hidden")
        shinyjs::hide("sensor8_show")
        shinyjs::addClass("sensor8_show", "hidden")
        shinyjs::hide("add_sensor_slot")
        shinyjs::hide("add_sensor_note")
        shinyjs::hide("sensor_change_note")
        shinyjs::hide("add_sensor_type_dropdown")
        shinyjs::hide("add_sensor_name")
        shinyjs::hide("sensor1_details")
        shinyjs::hide("sensor2_details")
        shinyjs::hide("sensor3_details")
        shinyjs::hide("sensor4_details")
        shinyjs::hide("sensor5_details")
        shinyjs::hide("sensor6_details")
        shinyjs::hide("sensor7_details")
        shinyjs::hide("sensor8_details")
        shinyjs::hide("change_sensor")
        shinyjs::hide("add_sensor_serial")
        shinyjs::hide("new_sensor_serial")
        shinyjs::hide("add_comment")
        shinyjs::hide("sensor_change_name")
        shinyjs::hide("submit_sensor_change")
      }
    })

    # observeEvents related to maintenance of instruments ################################################
    observeEvent(
      input$maintain_serial,
      {
        shinyjs::show("load_sensors")
        shinyjs::hide("sensor1_show")
        shinyjs::addClass("sensor1_show", "hidden")
        shinyjs::hide("sensor2_show")
        shinyjs::addClass("sensor2_show", "hidden")
        shinyjs::hide("sensor3_show")
        shinyjs::addClass("sensor3_show", "hidden")
        shinyjs::hide("sensor4_show")
        shinyjs::addClass("sensor4_show", "hidden")
        shinyjs::hide("sensor5_show")
        shinyjs::addClass("sensor5_show", "hidden")
        shinyjs::hide("sensor6_show")
        shinyjs::addClass("sensor6_show", "hidden")
        shinyjs::hide("sensor7_show")
        shinyjs::addClass("sensor7_show", "hidden")
        shinyjs::hide("sensor8_show")
        shinyjs::addClass("sensor8_show", "hidden")
        shinyjs::hide("add_sensor_slot")
        shinyjs::hide("add_sensor_note")
        shinyjs::hide("sensor_change_note")
        shinyjs::hide("add_sensor_type_dropdown")
        shinyjs::hide("add_sensor_name")
        shinyjs::hide("sensor1_details")
        shinyjs::hide("sensor2_details")
        shinyjs::hide("sensor3_details")
        shinyjs::hide("sensor4_details")
        shinyjs::hide("sensor5_details")
        shinyjs::hide("sensor6_details")
        shinyjs::hide("sensor7_details")
        shinyjs::hide("sensor8_details")
        shinyjs::hide("change_sensor")
        shinyjs::hide("add_sensor_serial")
        shinyjs::hide("new_sensor_serial")
        shinyjs::hide("add_comment")
        shinyjs::hide("sensor_change_name")
        shinyjs::hide("submit_sensor_change")
      },
      ignoreInit = TRUE
    )

    ## Load sensors table and buttons #############
    observeEvent(
      input$load_sensors,
      {
        if ((input$load_sensors %% 2) == 0) {
          # this part runs on second and subsequent even numbered clicks
          shinyjs::show("manage_sensors_table")
          #Hide the extra buttons
          shinyjs::hide("sensor1_show")
          shinyjs::addClass("sensor1_show", "hidden")
          shinyjs::hide("sensor2_show")
          shinyjs::addClass("sensor2_show", "hidden")
          shinyjs::hide("sensor3_show")
          shinyjs::addClass("sensor3_show", "hidden")
          shinyjs::hide("sensor4_show")
          shinyjs::addClass("sensor4_show", "hidden")
          shinyjs::hide("sensor5_show")
          shinyjs::addClass("sensor5_show", "hidden")
          shinyjs::hide("sensor6_show")
          shinyjs::addClass("sensor6_show", "hidden")
          shinyjs::hide("sensor7_show")
          shinyjs::addClass("sensor7_show", "hidden")
          shinyjs::hide("sensor8_show")
          shinyjs::addClass("sensor8_show", "hidden")
          shinyjs::hide("add_sensor_slot")
          shinyjs::hide("add_sensor_note")
          shinyjs::hide("add_sensor_type_dropdown")
          shinyjs::hide("sensor_change_note")
          shinyjs::hide("add_sensor_name")
          #Hide the sensor-specific tables in case any were opened
          shinyjs::hide("sensor1_details")
          shinyjs::hide("sensor2_details")
          shinyjs::hide("sensor3_details")
          shinyjs::hide("sensor4_details")
          shinyjs::hide("sensor5_details")
          shinyjs::hide("sensor6_details")
          shinyjs::hide("sensor7_details")
          shinyjs::hide("sensor8_details")
          #Hide the extra buttons related to sensor changes/maintenance
          shinyjs::hide("change_sensor")
          shinyjs::hide("add_sensor_serial")
          shinyjs::hide("new_sensor_serial")
          shinyjs::hide("add_comment")
          shinyjs::hide("sensor_change_name")
          shinyjs::hide("submit_sensor_change")
          updateActionButton(session, "load_sensors", label = "Show sensors")
        } else {
          # This part runs on first and subsequent odd numbered clicks
          if (input$maintain_serial != "loading choices...") {
            shinyjs::hide("manage_sensors_table")
            # Reset the button colors
            shinyjs::runjs(sprintf(
              'document.getElementById("%s").style.color = "#000000";',
              ns("sensor1_show")
            ))
            shinyjs::runjs(sprintf(
              'document.getElementById("%s").style.color = "#000000";',
              ns("sensor2_show")
            ))
            shinyjs::runjs(sprintf(
              'document.getElementById("%s").style.color = "#000000";',
              ns("sensor3_show")
            ))
            shinyjs::runjs(sprintf(
              'document.getElementById("%s").style.color = "#000000";',
              ns("sensor4_show")
            ))
            shinyjs::runjs(sprintf(
              'document.getElementById("%s").style.color = "#000000";',
              ns("sensor5_show")
            ))
            shinyjs::runjs(sprintf(
              'document.getElementById("%s").style.color = "#000000";',
              ns("sensor6_show")
            ))
            shinyjs::runjs(sprintf(
              'document.getElementById("%s").style.color = "#000000";',
              ns("sensor7_show")
            ))
            shinyjs::runjs(sprintf(
              'document.getElementById("%s").style.color = "#000000";',
              ns("sensor8_show")
            ))
            updateActionButton(
              session,
              "load_sensors",
              label = "Show instruments table again"
            )

            #Find the instrument_id associated with this instrument
            sensors_data$instrument_id <- instruments_data$maintainable_sensors[
              instruments_data$maintainable_sensors$serial_no ==
                input$maintain_serial,
              "instrument_id"
            ]

            #Load the array_maintenance_changes table subset for that instrument
            sensors_data$instrument <- DBI::dbGetQuery(
              session$userData$AquaCache,
              paste0(
                "SELECT * FROM array_maintenance_changes WHERE instrument_id = ",
                sensors_data$instrument_id
              )
            )

            shinyjs::show("add_sensor_type_dropdown")
            #Find out the max number of sensors ever assigned to the instrument and what they currently are
            sensor_columns <- grep(
              "sensor\\d+_id$",
              names(sensors_data$instrument),
              value = TRUE
            )
            sensors_data$number <- sum(
              !is.na(sensors_data$instrument[
                nrow(sensors_data$instrument),
                sensor_columns
              ])
            )

            if (sensors_data$number > 0) {
              for (i in 1:sensors_data$number) {
                #show the right number of sensors
                shinyjs::show(paste0("sensor", i, "_show"))
                shinyjs::removeClass(ns(paste0("sensor", i, "_show")), "hidden")
                sensor_id <- sensors_data$sensors[
                  sensors_data$sensors$sensor_id ==
                    sensors_data$instrument[
                      nrow(sensors_data$instrument),
                      paste0("sensor", i, "_id")
                    ],
                  "sensor_id"
                ]
                type_id <- sensors_data$sensors[
                  sensors_data$sensors$sensor_id == sensor_id,
                  "sensor_type"
                ]
                sensor_type <- sensors_data$sensor_types[
                  sensors_data$sensor_types$sensor_type_id == type_id,
                  "sensor_type"
                ]

                updateActionButton(
                  session,
                  paste0("sensor", i, "_show"),
                  label = HTML(paste0("Slot ", i, "<br>", sensor_type))
                )
                if (i == 8) {
                  shinyjs::hide("add_sensor_type_dropdown")
                }
              }
            } else {
              shinyjs::show("add_sensor_slot")
              shinyjs::show("new_sensor_serial")
              shinyjs::show("add_sensor_note")
              shinyjs::show("add_sensor_name")
            }
          }
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$add_sensor_type_dropdown,
      {
        if (input$tab_panel != "Change/maintain sensors") {
          return()
        }
        if (input$add_sensor_type_dropdown == "new") {
          showModal(modalDialog(
            title = "Add new sensor type",
            textInput(ns("new_sensor_type"), "Sensor type"),
            textInput(ns("new_sensor_desc"), "Description"),
            actionButton(ns("add_new_sensor_type"), "Add new sensor type")
          ))
        }
        shinyjs::show("add_sensor_slot")
        shinyjs::show("new_sensor_serial")
        shinyjs::show("add_sensor_note")
        shinyjs::show("add_sensor_name")
        serial_choices <- sensors_data$sensors[
          sensors_data$sensors$sensor_type == input$add_sensor_type_dropdown,
          "sensor_serial"
        ]
        updateSelectizeInput(
          session,
          "new_sensor_serial",
          choices = serial_choices
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$change_sensor,
      {
        if (input$tab_panel != "Change/maintain sensors") {
          return()
        }
        if (input$change_sensor == "new") {
          showModal(modalDialog(
            title = "Add new sensor type",
            textInput(ns("new_sensor_type"), "Sensor type"),
            textInput(ns("new_sensor_desc"), "Description"),
            actionButton(ns("add_new_sensor_type"), "Add new sensor type")
          ))
        } else {
          serial_choices <- sensors_data$sensors[
            sensors_data$sensors$sensor_type == input$change_sensor,
            "sensor_serial"
          ]
          updateSelectizeInput(
            session,
            "add_sensor_serial",
            choices = serial_choices
          )
        }
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    ## If the user inputs a non-existent sensor, add it ############################
    observeEvent(
      input$new_sensor_serial,
      {
        if (input$new_sensor_serial %in% sensors_data$sensors$sensor_serial) {
          shinyjs::hide("add_new_sensor_serial")
        } else {
          shinyjs::show("add_new_sensor_serial")
        }
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$add_sensor_serial,
      {
        if (input$add_sensor_serial %in% sensors_data$sensors$sensor_serial) {
          shinyjs::hide("add_new_sensor_serial2")
        } else {
          shinyjs::show("add_new_sensor_serial2")
        }
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$add_new_sensor_serial,
      {
        # adding sensor via left hand side menu
        # Add the sensor to the sensors table
        # Show a modal so the user can input the make, model, date in service, date purchased, asset tag, and notes
        showModal(modalDialog(
          title = "Add a new sensor",
          selectizeInput(
            ns("new_sensor_make"),
            "Make (type your own if not in yet)",
            choices = unique(sensors_data$sensors$sensor_make),
            options = list(create = TRUE)
          ),
          selectizeInput(
            ns("new_sensor_model"),
            "Model (type your own if not in yet)",
            choices = unique(sensors_data$sensors$sensor_model),
            options = list(create = TRUE)
          ),
          textInput(ns("new_sensor_asset_tag"), "Asset tag (optional)"),
          dateInput(
            ns("new_sensor_date_purchased"),
            "Date purchased (if known)",
            value = ""
          ),
          dateInput(
            ns("new_sensor_date_in_service"),
            "Date in service (if known)",
            value = ""
          ),
          textAreaInput(ns("new_sensor_notes"), "Notes (optional)"),
          actionButton(ns("add_sensor_to_db"), "Add sensor")
        ))
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$add_new_sensor_serial2,
      {
        # adding sensor via instrument maintenance view table
        # Add the sensor to the sensors table
        # Show a modal so the user can input the make, model, date in service, date purchased, asset tag, and notes
        showModal(modalDialog(
          title = "Add a new sensor",
          selectizeInput(
            ns("new_sensor_make"),
            "Make (type your own if not in yet)",
            choices = unique(sensors_data$sensors$sensor_make),
            options = list(create = TRUE)
          ),
          selectizeInput(
            ns("new_sensor_model"),
            "Model (type your own if not in yet)",
            choices = unique(sensors_data$sensors$sensor_model),
            options = list(create = TRUE)
          ),
          textInput(ns("new_sensor_asset_tag"), "Asset tag (optional)"),
          dateInput(
            ns("new_sensor_date_purchased"),
            "Date purchased",
            value = ""
          ),
          dateInput(
            ns("new_sensor_date_in_service"),
            "Date in service",
            value = ""
          ),
          textAreaInput(ns("new_sensor_notes"), "Notes (optional)"),
          actionButton(ns("add_sensor_to_db2"), "Add sensor")
        ))
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$add_sensor_to_db,
      {
        # adding sensor via left hand side menu
        if (input$new_sensor_make == "" || input$new_sensor_model == "") {
          showNotification(
            "Make and Model are required fields.",
            type = "error"
          )
          return()
        }

        tbl <- data.frame(
          "sensor_type" = input$add_sensor_type_dropdown,
          "sensor_serial" = input$new_sensor_serial,
          "sensor_make" = input$new_sensor_make,
          "sensor_model" = input$new_sensor_model,
          "sensor_asset_tag" = if (nchar(input$new_sensor_asset_tag) > 0) {
            input$new_sensor_asset_tag
          } else {
            NA
          },
          "date_purchased" = if (length(input$new_sensor_date_purchased) > 0) {
            input$new_sensor_date_purchased
          } else {
            NA
          },
          "date_in_service" = if (
            length(input$new_sensor_date_in_service) > 0
          ) {
            input$new_sensor_date_in_service
          } else {
            NA
          },
          "sensor_notes" = if (nchar(input$new_sensor_notes) > 0) {
            input$new_sensor_notes
          } else {
            NA
          }
        )
        DBI::dbAppendTable(session$userData$AquaCache, "sensors", tbl)

        removeModal()
        shinyjs::hide("add_new_sensor_serial")

        # load the sensors table again
        sensors_data$sensors <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT * FROM sensors"
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$add_sensor_to_db2,
      {
        # adding sensor via instrument maintenance view table
        if (input$new_sensor_make == "" || input$new_sensor_model == "") {
          showNotification(
            "Make and Model are required fields.",
            type = "error"
          )
          return()
        }

        tbl <- data.frame(
          "sensor_type" = input$change_sensor,
          "sensor_serial" = input$add_sensor_serial,
          "sensor_make" = input$new_sensor_make,
          "sensor_model" = input$new_sensor_model,
          "sensor_asset_tag" = if (nchar(input$new_sensor_asset_tag) > 0) {
            input$new_sensor_asset_tag
          } else {
            NA
          },
          "date_purchased" = if (length(input$new_sensor_date_purchased) > 0) {
            input$new_sensor_date_purchased
          } else {
            NA
          },
          "date_in_service" = if (
            length(input$new_sensor_date_in_service) > 0
          ) {
            input$new_sensor_date_in_service
          } else {
            NA
          },
          "sensor_notes" = if (nchar(input$new_sensor_notes) > 0) {
            input$new_sensor_notes
          } else {
            NA
          }
        )
        DBI::dbAppendTable(session$userData$AquaCache, "sensors", tbl)

        removeModal()
        shinyjs::hide("add_new_sensor_serial2")

        # load the sensors table again
        sensors_data$sensors <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT * FROM sensors"
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    ## Add a sensor to a new slot, or modify a sensor added in this same session ############################
    observeEvent(
      input$add_sensor_slot,
      {
        #find out if an entry already has the timestamp sensors_data$datetime for this same instrument (sensors_data$instrument is already subset to the instrument_id)
        if (length(sensors_data$instrument$obs_datetime) == 0) {
          sensors_data$datetime_exists <- FALSE
        } else {
          sensors_data$datetime_exists <- latest_sensor_event_matches(
            sensors_data$instrument$obs_datetime,
            sensors_data$datetime
          )
        }

        sensor_id <- sensors_data$sensors[
          sensors_data$sensors$sensor_serial == input$new_sensor_serial,
          "sensor_id"
        ]
        type_id <- sensors_data$sensors[
          sensors_data$sensors$sensor_id == sensor_id,
          "sensor_type"
        ]
        type <- sensors_data$sensor_types[
          sensors_data$sensor_types$sensor_type_id == type_id,
          "sensor_type"
        ]

        comment <- paste0(
          "Added a new sensor via app: ",
          type,
          ", serial no ",
          input$new_sensor_serial,
          " added."
        )

        col_id <- paste0("sensor", sensors_data$number + 1, "_id")
        col_comment <- paste0("sensor", sensors_data$number + 1, "_notes")

        if (sensors_data$datetime_exists) {
          #modifying an existing entry
          DBI::dbExecute(
            session$userData$AquaCache,
            paste0(
              "UPDATE array_maintenance_changes SET ",
              col_id,
              " = ",
              sensor_id,
              ", ",
              col_comment,
              " = '",
              comment,
              "' WHERE obs_datetime BETWEEN '",
              sensors_data$datetime - 5,
              "' AND '",
              sensors_data$datetime + 5,
              "' AND instrument_id = ",
              sensors_data$instrument_id,
              ";"
            )
          )
        } else {
          # Creating a new entry, starting out with the last entry in the array_maintenance_changes table so that sensor changes are noted
          # Take the final row of the array_maintenance_changes table and replace fields
          if (nrow(sensors_data$instrument) == 0) {
            # Nothing to go off of, so make a new df to append
            df <- data.frame(instrument_id = sensors_data$instrument_id)
          } else {
            df <- sensors_data$instrument[
              nrow(sensors_data$instrument),
              c(
                "instrument_id",
                "sensor1_id",
                "sensor2_id",
                "sensor3_id",
                "sensor4_id",
                "sensor5_id",
                "sensor6_id",
                "sensor7_id",
                "sensor8_id"
              )
            ]
          }
          df$obs_datetime <- sensors_data$datetime
          df$observer <- input$add_sensor_name

          df[[col_id]] <- sensor_id
          df[[col_comment]] <- comment

          DBI::dbAppendTable(
            session$userData$AquaCache,
            "array_maintenance_changes",
            df
          )
        }
        #Load the array_maintenance_changes table again
        sensors_data$instrument <- DBI::dbGetQuery(
          session$userData$AquaCache,
          paste0(
            "SELECT * FROM array_maintenance_changes WHERE instrument_id = ",
            sensors_data$instrument_id
          )
        )
        # Increment the number of sensors
        sensors_data$number <- sensors_data$number + 1

        shinyjs::show(paste0("sensor", sensors_data$number, "_show")) # show the new sensor button
        shinyjs::removeClass(
          ns(paste0("sensor", sensors_data$number, "_show")),
          "hidden"
        )
        shinyjs::hide("add_sensor_name")
        shinyjs::hide("add_sensor_slot")
        shinyjs::hide("new_sensor_serial")
        shinyjs::hide("add_sensor_note")

        lab <- paste0("Slot ", sensors_data$number, "<br>", type)
        updateActionButton(
          session,
          paste0("sensor", sensors_data$number, "_show"),
          label = HTML(lab)
        )
        sensors_data$datetime_exists <- TRUE
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    ## Show table for individual sensors when user clicks on _show button ########################################
    # Make a function to reduce repetitive code
    showSensorDetails <- function(sensor_index) {
      sensor_id_col <- paste0("sensor", sensor_index, "_id")
      sensor_notes_col <- paste0("sensor", sensor_index, "_notes")
      sensor_details_name <- paste0("sensor", sensor_index, "_details")
      sensors_data$selected <- paste0("sensor", sensor_index)

      sub <- merge(
        sensors_data$sensors[
          sensors_data$sensors$sensor_id ==
            sensors_data$instrument[, sensor_id_col],
          c("sensor_type", "sensor_id", "sensor_serial")
        ],
        sensors_data$sensor_types[, c("sensor_type_id", "sensor_type")],
        by.x = "sensor_type",
        by.y = "sensor_type_id",
        sort = FALSE
      )
      sub <- merge(
        sub,
        sensors_data$instrument[, c(
          "obs_datetime",
          sensor_notes_col,
          sensor_id_col,
          "observer"
        )],
        by.x = "sensor_id",
        by.y = sensor_id_col,
        sort = FALSE
      )
      sub <- merge(
        sub,
        instruments_data$observers[, c("observer_id", "observer_string")],
        by.x = "observer",
        by.y = "observer_id",
        sort = FALSE
      )

      sensors_data$datetime_exists <- latest_sensor_event_matches(
        sensors_data$instrument$obs_datetime,
        sensors_data$datetime
      )

      df <- data.frame(
        "Date/time" = substr(sub$obs_datetime, 1, 16),
        "Type" = sub$sensor_type.y,
        "Serial" = sub$sensor_serial,
        "Notes" = sub[, sensor_notes_col],
        "Observer" = sub$observer_string,
        check.names = FALSE
      )
      df <- df[!is.na(df$Notes), ]

      sensors_data[[sensor_details_name]] <- df

      output[[sensor_details_name]] <- DT::renderDT(df, rownames = FALSE)

      # Update inputs with the sensor's *current* details
      updateSelectizeInput(
        session,
        "change_sensor",
        selected = sub[nrow(sub), "sensor_type"]
      )
      serial_choices <- sensors_data$sensors[
        sensors_data$sensors$sensor_type == input$change_sensor,
        "sensor_serial"
      ]

      if (sensors_data$datetime_exists) {
        # Determine if the user is making an edit (i.e. they visited this sensor already in this session) or if they're saving anew
        if (
          sensors_data$instrument[
            sensors_data$instrument$obs_datetime == sensors_data$datetime,
            "instrument_id"
          ] ==
            sensors_data$instrument_id
        ) {
          updateSelectizeInput(
            session,
            "add_sensor_serial",
            choices = serial_choices,
            selected = sub[nrow(sub), "sensor_serial"]
          )
          updateTextAreaInput(
            session,
            "add_comment",
            value = sensors_data$instrument[
              nrow(sensors_data$instrument),
              sensor_notes_col
            ]
          )
          output$sensor_change_name <- renderUI({
            selectizeInput(
              ns("sensor_change_name"),
              label = "Observer name",
              choices = select_data$recorder,
              selected = sensors_data$instrument[
                nrow(sensors_data$instrument),
                "observer"
              ]
            )
          })
          updateActionButton(
            session,
            "submit_sensor_change",
            label = "Save edits"
          )
        }
      } else {
        updateSelectizeInput(
          session,
          "add_sensor_serial",
          choices = serial_choices,
          selected = ""
        )
        updateTextAreaInput(session, "add_comment", value = "")
        updateActionButton(
          session,
          "submit_sensor_change",
          label = "Submit New Record"
        )
      }
      # Hide other sensor details and show the current sensor details
      for (i in 1:8) {
        if (i == sensor_index) {
          shinyjs::show(paste0("sensor", i, "_details"))
          shinyjs::runjs(sprintf(
            'document.getElementById("%s").style.color = "#00BFFF";',
            ns(paste0("sensor", i, "_show"))
          ))
        } else {
          shinyjs::hide(paste0("sensor", i, "_details"))
          shinyjs::runjs(sprintf(
            'document.getElementById("%s").style.color = "#000000";',
            ns(paste0("sensor", i, "_show"))
          ))
        }
      }
      shinyjs::show("change_sensor")
      shinyjs::show("add_comment")
      shinyjs::show("sensor_change_name")
      shinyjs::show("add_sensor_serial")
      shinyjs::hide("manage_sensors_table")
      shinyjs::show("submit_sensor_change")
      shinyjs::show("sensor_change_note")
    }

    # Apply function to each sensor button
    observeEvent(
      input$sensor1_show,
      {
        showSensorDetails(1)
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$sensor2_show,
      {
        showSensorDetails(2)
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$sensor3_show,
      {
        showSensorDetails(3)
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$sensor4_show,
      {
        showSensorDetails(4)
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$sensor5_show,
      {
        showSensorDetails(5)
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$sensor6_show,
      {
        showSensorDetails(6)
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$sensor7_show,
      {
        showSensorDetails(7)
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$sensor8_show,
      {
        showSensorDetails(8)
      },
      ignoreInit = TRUE
    )

    ## Save changes to a sensor's details ########################################
    observeEvent(
      input$submit_sensor_change,
      {
        comment_text <- trimws(
          if (is.null(input$add_comment)) "" else input$add_comment
        )
        sensor_serial_text <- trimws(
          if (is.null(input$add_sensor_serial)) "" else input$add_sensor_serial
        )
        if (is.null(input$sensor_change_name)) {
          alert(
            "Please fill in all fields!",
            "You need a few more characters if you've already written something. Come on, make us a useful note!",
            type = "error",
            timer = 3000
          )
          return()
        } else if (
          input$sensor_change_name == "" ||
            nchar(comment_text) < 5 ||
            nchar(sensor_serial_text) < 2
        ) {
          alert(
            "Please fill in all fields!",
            "You need a few more characters if you've already written something. Come on, make us a useful note!",
            type = "error",
            timer = 3000
          )
          return()
        } else {
          #add the data to the array_maintenance_changes table
          # Check if the datetime exists in the instrument table, which means we're editing an entry from this same session
          sensors_data$datetime_exists <- latest_sensor_event_matches(
            sensors_data$instrument$obs_datetime,
            sensors_data$datetime
          )

          sensor_id <- sensors_data$sensors[
            sensors_data$sensors$sensor_serial == input$add_sensor_serial,
            "sensor_id"
          ]

          col_id <- paste0(sensors_data$selected, "_id")
          col_comment <- paste0(sensors_data$selected, "_notes")

          if (sensors_data$datetime_exists) {
            # Update the existing row
            DBI::dbExecute(
              session$userData$AquaCache,
              paste0(
                "UPDATE array_maintenance_changes SET ",
                col_id,
                " = ",
                sensor_id,
                ", ",
                col_comment,
                " = ",
                sql_string_or_null(comment_text),
                " WHERE obs_datetime = ",
                sql_string_or_null(as.character(sensors_data$datetime)),
                " AND instrument_id = ",
                sensors_data$instrument_id,
                ";"
              )
            )
            run_else <- FALSE
          } else {
            #append to array_maintenance_changes with a new row
            # Take the final row of the array_maintenance_changes table and replace fields. Remove notes from all other sensors; they don't share a datetime with the current session.
            df <- sensors_data$instrument[
              nrow(sensors_data$instrument),
              c(
                "instrument_id",
                "sensor1_id",
                "sensor2_id",
                "sensor3_id",
                "sensor4_id",
                "sensor5_id",
                "sensor6_id",
                "sensor7_id",
                "sensor8_id"
              )
            ]
            df$obs_datetime <- sensors_data$datetime
            df$observer <- input$sensor_change_name

            df[[col_id]] <- sensor_id
            df[[col_comment]] <- input$add_comment

            DBI::dbAppendTable(
              session$userData$AquaCache,
              "array_maintenance_changes",
              df
            )
          }
          #Load the array_maintenance_changes table again
          sensors_data$instrument <- DBI::dbGetQuery(
            session$userData$AquaCache,
            paste0(
              "SELECT * FROM array_maintenance_changes WHERE instrument_id = ",
              sensors_data$instrument_id
            )
          )

          #render table again and update buttons
          showSensorDetails(gsub("\\D", "", sensors_data$selected))

          type_id <- sensors_data$sensors[
            sensors_data$sensors$sensor_id == sensor_id,
            "sensor_type"
          ]
          type <- sensors_data$sensor_types[
            sensors_data$sensor_types$sensor_type_id == type_id,
            "sensor_type"
          ]
          lab <- paste0(
            "Slot ",
            gsub("\\D", "", sensors_data$selected),
            "<br>",
            type
          )

          updateActionButton(
            session,
            paste0("sensor", gsub("\\D", "", sensors_data$selected), "_show"),
            label = HTML(lab)
          )

          updateActionButton(
            session,
            "submit_sensor_change",
            label = "Save edits"
          )
        }
      },
      ignoreInit = TRUE
    )

    # Restart a calibration ##############################################################################
    observeEvent(
      input$restart_calibration,
      {
        restart_value <- selected_incomplete_row()
        if (is.na(restart_value)) {
          alert(
            "Select a row from the unfinished calibrations table first.",
            type = "error",
            timer = 3000
          )
        } else {
          shinyjs::show("restart_table")
          send_table$restarted_cal <- data.frame(
            "Saved records (recovered session)" = entry_display_labels$basic,
            check.names = FALSE
          ) #Set/reset here for if the user selects a different calibration in the same session
          complete$basic <- TRUE
          incomplete_ID <- as.numeric(calibrations$incomplete_calibrations[
            restart_value,
            "calibration_id"
          ])
          calibration_data$next_id <- incomplete_ID
          calibration_data$restarted_id <- incomplete_ID

          # Search for entries in parameter-specific sheets with the same calibration_id and update the fields
          all_sheets <- DBI::dbListTables(session$userData$AquaCache)
          calibration_sheets <- all_sheets[grepl("^calibrate_", all_sheets)]
          for (i in calibration_sheets) {
            sheet <- DBI::dbGetQuery(
              session$userData$AquaCache,
              paste0(
                "SELECT * FROM ",
                i,
                " WHERE calibration_id = ",
                incomplete_ID
              )
            )
            if (nrow(sheet) == 1) {
              if (i == "calibrate_temperature") {
                output_name <- entry_display_labels$temperature
                complete$temperature <- TRUE
                calibration_data$temp <- data.frame(
                  calibration_id = incomplete_ID,
                  temp_reference_desc = sheet$temp_reference_desc,
                  temp_reference = sheet$temp_reference,
                  temp_observed = sheet$temp_observed
                )
                updateTextInput(
                  session,
                  "temp_reference_desc",
                  value = sheet$temp_reference_desc
                )
                updateNumericInput(
                  session,
                  "temp_reference",
                  value = sheet$temp_reference
                )
                updateNumericInput(
                  session,
                  "temp_observed",
                  value = sheet$temp_observed
                )
                shinyjs::show("delete_temp")
              } else if (i == "calibrate_specific_conductance") {
                output_name <- entry_display_labels$spc
                complete$spc <- TRUE
                updateRadioButtons(
                  session,
                  "spc_entry_mode",
                  selected = if (sheet_check_only(sheet)) {
                    "check"
                  } else {
                    "calibration"
                  }
                )

                spc_point_count <- if (
                  "calibration_points" %in%
                    colnames(sheet) &&
                    !is.na(sheet$calibration_points[1])
                ) {
                  as.integer(sheet$calibration_points[1])
                } else if (
                  "spc3_std" %in% colnames(sheet) && !is.na(sheet$spc3_std[1])
                ) {
                  3L
                } else {
                  2L
                }
                updateRadioButtons(
                  session,
                  "spc_points",
                  selected = spc_point_count
                )
                updateNumericInput(session, "spc1_std", value = sheet$spc1_std)
                updateNumericInput(session, "spc1_pre", value = sheet$spc1_pre)
                updateNumericInput(
                  session,
                  "spc1_post",
                  value = sheet$spc1_post
                )
                updateNumericInput(session, "spc2_std", value = sheet$spc2_std)
                updateNumericInput(session, "spc2_pre", value = sheet$spc2_pre)
                updateNumericInput(
                  session,
                  "spc2_post",
                  value = sheet$spc2_post
                )
                updateNumericInput(
                  session,
                  "spc3_std",
                  value = if ("spc3_std" %in% colnames(sheet)) {
                    sheet$spc3_std
                  } else {
                    NA_real_
                  }
                )
                updateNumericInput(
                  session,
                  "spc3_pre",
                  value = if ("spc3_pre" %in% colnames(sheet)) {
                    sheet$spc3_pre
                  } else {
                    NA_real_
                  }
                )
                updateNumericInput(
                  session,
                  "spc3_post",
                  value = if ("spc3_post" %in% colnames(sheet)) {
                    sheet$spc3_post
                  } else {
                    NA_real_
                  }
                )
                sync_spc_labels(
                  point_count = spc_point_count,
                  non_specific = FALSE
                )
                sync_spc_post_visibility(
                  point_count = spc_point_count,
                  show_posts = !sheet_check_only(sheet)
                )
                shinyjs::show("delete_spc")
              } else if (i == "calibrate_ph") {
                output_name <- entry_display_labels$ph
                complete$ph <- TRUE
                updateRadioButtons(
                  session,
                  "ph_entry_mode",
                  selected = if (sheet_check_only(sheet)) {
                    "check"
                  } else {
                    "calibration"
                  }
                )

                updateNumericInput(session, "ph1_std", value = sheet$ph1_std)
                updateNumericInput(session, "ph2_std", value = sheet$ph2_std)
                updateNumericInput(session, "ph3_std", value = sheet$ph3_std)
                updateNumericInput(
                  session,
                  "ph1_pre_val",
                  label = ph_pre_label(sheet$ph1_std),
                  value = sheet$ph1_pre_val
                )
                updateNumericInput(
                  session,
                  "ph1_mv",
                  label = paste0("pH ", sheet$ph1_std, " mV"),
                  value = sheet$ph1_mv
                )
                updateNumericInput(
                  session,
                  "ph2_pre_val",
                  label = ph_pre_label(sheet$ph2_std),
                  value = sheet$ph2_pre_val
                )
                updateNumericInput(
                  session,
                  "ph2_mv",
                  label = paste0("pH ", sheet$ph2_std, " mV"),
                  value = sheet$ph2_mv
                )
                updateNumericInput(
                  session,
                  "ph3_pre_val",
                  label = ph_pre_label(sheet$ph3_std),
                  value = sheet$ph3_pre_val
                )
                updateNumericInput(
                  session,
                  "ph3_mv",
                  label = paste0("pH ", sheet$ph3_std, " mV"),
                  value = sheet$ph3_mv
                )
                updateNumericInput(
                  session,
                  "ph1_post_val",
                  label = ph_post_label(sheet$ph1_std),
                  value = sheet$ph1_post_val
                )
                updateNumericInput(
                  session,
                  "ph2_post_val",
                  label = ph_post_label(sheet$ph2_std),
                  value = sheet$ph2_post_val
                )
                updateNumericInput(
                  session,
                  "ph3_post_val",
                  label = ph_post_label(sheet$ph3_std),
                  value = sheet$ph3_post_val
                )
                sync_ph_labels()
                shinyjs::show("delete_ph")
              } else if (i == "calibrate_orp") {
                output_name <- entry_display_labels$orp
                complete$orp <- TRUE
                updateRadioButtons(
                  session,
                  "orp_entry_mode",
                  selected = if (sheet_check_only(sheet)) {
                    "check"
                  } else {
                    "calibration"
                  }
                )

                updateNumericInput(session, "orp_std", value = sheet$orp_std)
                updateNumericInput(
                  session,
                  "orp_pre_mv",
                  value = sheet$orp_pre_mv
                )
                updateNumericInput(
                  session,
                  "orp_post_mv",
                  value = sheet$orp_post_mv
                )
                sync_orp_labels()
                shinyjs::show("delete_orp")
              } else if (i == "calibrate_turbidity") {
                output_name <- entry_display_labels$turbidity
                complete$turbidity <- TRUE
                updateRadioButtons(
                  session,
                  "turb_entry_mode",
                  selected = if (sheet_check_only(sheet)) {
                    "check"
                  } else {
                    "calibration"
                  }
                )

                updateNumericInput(
                  session,
                  "turb1_std",
                  value = sheet$turb1_std
                )
                updateNumericInput(
                  session,
                  "turb2_std",
                  value = sheet$turb2_std
                )
                updateNumericInput(
                  session,
                  "turb1_pre",
                  value = sheet$turb1_pre
                )
                updateNumericInput(
                  session,
                  "turb2_pre",
                  value = sheet$turb2_pre
                )
                updateNumericInput(
                  session,
                  "turb1_post",
                  value = sheet$turb1_post
                )
                updateNumericInput(
                  session,
                  "turb2_post",
                  value = sheet$turb2_post
                )
                sync_turb_labels()
                shinyjs::show("delete_turb")
              } else if (i == "calibrate_dissolved_oxygen") {
                output_name <- entry_display_labels$do
                complete$do <- TRUE
                updateRadioButtons(
                  session,
                  "do_entry_mode",
                  selected = if (sheet_check_only(sheet)) {
                    "check"
                  } else {
                    "calibration"
                  }
                )

                updateNumericInput(
                  session,
                  "baro_press_pre",
                  value = sheet$baro_press_pre
                )
                updateNumericInput(
                  session,
                  "baro_press_post",
                  value = sheet$baro_press_post
                )
                updateNumericInput(session, "do_pre", value = sheet$do_pre_mgl)
                updateNumericInput(
                  session,
                  "do_post",
                  value = sheet$do_post_mgl
                )
                sync_do_labels()
                shinyjs::show("delete_do")
              } else if (i == "calibrate_depth") {
                output_name <- entry_display_labels$depth
                complete$depth <- TRUE
                updateRadioButtons(
                  session,
                  inputId = "depth_check_ok",
                  selected = sheet$depth_check_ok
                )
                updateRadioButtons(
                  session,
                  inputId = "depth_changes_ok",
                  selected = sheet$depth_changes_ok
                )
                shinyjs::show("delete_depth")
              }
              send_table$restarted_cal[
                nrow(send_table$restarted_cal) + 1,
                1
              ] <- output_name
            }
          }
          # Reset the basic fields according to recovered info
          output$observer <- renderUI({
            selectizeInput(
              ns("observer"),
              label = "Calibrator name",
              choices = select_data$recorder,
              selected = select_data$recorder[calibrations$incomplete_calibrations[
                restart_value,
                "observer"
              ]]
            )
          })
          shinyWidgets::updateAirDateInput(
            session,
            "obs_datetime",
            value = coerce_utc_datetime(
              calibrations$incomplete_calibrations[
                restart_value,
                "obs_datetime"
              ]
            ),
            tz = air_datetime_widget_timezone(input$timezone)
          )
          output$ID_sensor_holder <- renderUI({
            div(
              selectizeInput(
                ns("ID_sensor_holder"),
                label = "Logger/bulkhead/sonde serial #",
                choices = c("", instruments_data$others$serial_no),
                selected = if (
                  !is.na(calibrations$incomplete_calibrations[
                    calibrations$incomplete_calibrations$calibration_id ==
                      incomplete_ID,
                    "id_sensor_holder"
                  ])
                ) {
                  instruments_data$others[
                    instruments_data$others$instrument_id ==
                      calibrations$incomplete_calibrations[
                        calibrations$incomplete_calibrations$calibration_id ==
                          incomplete_ID,
                        "id_sensor_holder"
                      ],
                    "serial_no"
                  ]
                } else {
                  "NA"
                }
              ),
              style = "color: white; background-color: blue;"
            )
          })
          output$ID_handheld_meter <- renderUI({
            div(
              selectizeInput(
                ns("ID_handheld_meter"),
                label = "Handheld serial # (if applicable)",
                choices = c("NA", instruments_data$handhelds$serial_no),
                selected = if (
                  !is.na(calibrations$incomplete_calibrations[
                    calibrations$incomplete_calibrations$calibration_id ==
                      incomplete_ID,
                    "id_handheld_meter"
                  ])
                ) {
                  instruments_data$handhelds[
                    instruments_data$handhelds$instrument_id ==
                      calibrations$incomplete_calibrations[
                        calibrations$incomplete_calibrations$calibration_id ==
                          incomplete_ID,
                        "id_handheld_meter"
                      ],
                    "serial_no"
                  ]
                } else {
                  "NA"
                }
              ),
              style = "color: white; background-color: green;"
            )
          })
          updateTextInput(
            session,
            "calibration_purpose",
            value = calibrations$incomplete_calibrations[
              restart_value,
              "purpose"
            ]
          )
          colnames(send_table$saved) <- "Saved records (this session)" #Update the name for clarity since we're restarting a calibration
          output$saved <- renderTable({
            # Display local calibrations tables with new name
            send_table$saved
          })
          output$restart_table <- renderTable({
            # Display remotely saved calibrations tables
            send_table$restarted_cal
          })
          restarted$restarted <- TRUE
          updateCheckboxInput(session, "spc_or_not", value = FALSE) #Reset to FALSE since values are stored as spc; this makes it clear to the user.
          updateTabsetPanel(
            session,
            "tab_panel",
            selected = "Checks / calibrations"
          ) # Changing this selection brings the user right to the calibration page
        }
      },
      ignoreInit = TRUE
    )

    # Delete a calibration ##############################################################################
    observeEvent(
      input$delete_calibration,
      {
        delete_value <- selected_incomplete_row()
        if (is.na(delete_value)) {
          alert(
            "Select a row from the unfinished calibrations table first.",
            type = "error",
            timer = 2000
          )
        } else {
          delete_ID <- as.numeric(calibrations$incomplete_calibrations[
            delete_value,
            "calibration_id"
          ])
          all_sheets <- DBI::dbListTables(session$userData$AquaCache)
          calibration_sheets <- all_sheets[grepl("^calibrate_", all_sheets)]

          DBI::dbExecute(
            session$userData$AquaCache,
            paste0(
              "DELETE FROM calibrations WHERE calibration_id = ",
              delete_ID
            )
          ) # Cascades to all other sheets where the id is referenced

          # Reload the calibrations table and re-create incomplete_calibrations
          calibrations$calibrations <- DBI::dbGetQuery(
            session$userData$AquaCache,
            "SELECT * FROM calibrations"
          ) # This will be used to check if there are any incomplete calibrations
          calibrations$incomplete_calibrations <- calibrations$calibrations[
            calibrations$calibrations$complete == FALSE,
          ] # find out if any calibrations are labelled as incomplete

          calibrations$incomplete_calibrations <- dplyr::left_join(
            calibrations$incomplete_calibrations,
            instruments_data$observers[, c("observer_id", "observer_string")],
            by = dplyr::join_by(observer == observer_id)
          )

          complete$incomplete <- data.frame(
            "Calibrator" = as.vector(
              calibrations$incomplete_calibrations$observer_string
            ),
            "Date/time UTC" = calibrations$incomplete_calibrations$obs_datetime,
            "Purpose" = calibrations$incomplete_calibrations$purpose,
            check.names = FALSE
          )

          if (nrow(complete$incomplete) == 0) {
            complete$incomplete <- data.frame(
              "Calibrator" = "No unsaved records!",
              "Date/time UTC" = "No unsaved records!",
              "Purpose" = "No unsaved records!",
              check.names = FALSE
            )
          }
          output$incomplete_table <- DT::renderDT(
            complete$incomplete,
            rownames = FALSE,
            selection = "single"
          )

          calibrations$incomplete_calibrations <- calibrations$incomplete_calibrations[
            !calibrations$incomplete_calibrations$calibration_id == delete_ID,
          ]
          #reset internal markers of completeness
          complete$basic <- FALSE
          complete$temperature <- FALSE
          complete$spc <- FALSE
          complete$ph <- FALSE
          complete$orp <- FALSE
          complete$turbidity <- FALSE
          complete$do <- FALSE
          complete$depth <- FALSE
          # reset fields previously loaded if loaded calibration is the one being deleted
          if (delete_ID == calibration_data$restarted_id) {
            reset_basic()
            reset_ph()
            reset_temp()
            reset_orp()
            reset_spc()
            reset_turb()
            reset_do()
            reset_depth()
            send_table$saved <- data.frame(
              "Saved records" = "Nothing saved yet",
              check.names = FALSE
            )
            send_table$restarted_cal <- empty_restarted_cal_table()
            output$saved <- renderTable({
              send_table$saved
            })
            output$restart_table <- renderTable({
              send_table$restarted_cal
            })
            shinyjs::hide("restart_table")
            restarted$restarted <- FALSE
            calibration_data$restarted_id <- 0
            calibration_data$next_id <- NULL
          }
          alert("Deleted", type = "success", timer = 2000)
        }
      },
      ignoreInit = TRUE
    )

    # Update the spc and DO fields ##########################################
    observeEvent(
      input$spc_or_not,
      {
        if (
          !identical(input$tab_panel, "Checks / calibrations") ||
            !identical(input$selection, "Conductivity calibration")
        ) {
          return()
        }
        if (input$spc_or_not) {
          temp_reference <- current_temp_reference()
          if (
            !isTRUE(complete$temperature) ||
              length(temp_reference) == 0 ||
              is.na(temp_reference)
          ) {
            alert(
              "Save temperature check first!",
              "You must save a temperature check before entering non-specific conductivity.",
              type = "error",
              timer = 3000
            )
            updateCheckboxInput(session, "spc_or_not", value = FALSE)
            return()
          }
        } else {
          # Labels and post defaults are restored below.
        }
        sync_spc_labels(non_specific = isTRUE(input$spc_or_not))
        for (i in seq_len(current_spc_point_count())) {
          updateNumericInput(
            session,
            paste0("spc", i, "_post"),
            value = default_spc_post_value(input[[paste0("spc", i, "_std")]])
          )
        }
      },
      ignoreInit = TRUE
    )

    #Function to simplify DO calculated fields later on
    DO_calc <- function(pre_post, prct_abs, messages = TRUE) {
      trigger_name <- if (pre_post == "pre" & prct_abs == "prct") {
        "do_pre_prct"
      } else if (pre_post == "pre" & prct_abs == "abs") {
        "do_pre"
      } else if (pre_post == "post" & prct_abs == "prct") {
        "do_post_prct"
      } else if (pre_post == "post" & prct_abs == "abs") {
        "do_post"
      }
      update_name <- if (pre_post == "pre" & prct_abs == "prct") {
        "do_pre"
      } else if (pre_post == "pre" & prct_abs == "abs") {
        "do_pre_prct"
      } else if (pre_post == "post" & prct_abs == "prct") {
        "do_post"
      } else if (pre_post == "post" & prct_abs == "abs") {
        "do_post_prct"
      }
      baro_press <- if (pre_post == "pre") {
        input$baro_press_pre
      } else {
        input$baro_press_post
      }
      meas <- input[[trigger_name]]
      temp <- input$temp_observed
      go_baro <- FALSE
      go_temp <- FALSE

      if (!is.na(baro_press)) {
        if (baro_press < 600 | baro_press > 850) {
          if (messages && pre_post == "pre") {
            alert(
              if (is_check_only_mode("do")) {
                "Observed baro pressure out of range"
              } else {
                "As-found baro pressure out of range"
              },
              "Enter pressure in mmHg only",
              type = "error",
              timer = 3000
            )
          } else if (messages) {
            alert(
              "As-left baro pressure out of range",
              "Enter pressure in mmHg only",
              type = "error",
              timer = 3000
            )
          }
        } else {
          go_baro <- TRUE
        }
      } else {
        if (messages && pre_post == "pre") {
          alert(
            if (is_check_only_mode("do")) {
              "Enter observed baro pressure in mmHg first!"
            } else {
              "Enter as-found baro pressure in mmHg first!"
            },
            type = "error",
            timer = 3000
          )
        } else if (messages) {
          alert(
            "Enter as-left baro pressure in mmHg first!",
            type = "error",
            timer = 3000
          )
        }
      }
      if (!is.na(temp)) {
        if (temp < 0 | temp > 30) {
          if (messages) {
            alert(
              "Temperature out of range",
              "Temp should be between 0 and 30 degrees C; review the temperature check",
              type = "error",
              timer = 3000
            )
          }
        } else {
          go_temp <- TRUE
        }
      } else {
        if (messages) {
          alert(
            "Enter temperature data first!",
            "Go to the temperature check",
            type = "error",
            timer = 3000
          )
        }
      }
      if (!is.na(meas) & meas > 0 & go_baro & go_temp) {
        res <- suppressWarnings(respR::convert_DO(
          meas,
          from = if (grepl("prct", trigger_name)) "%Air" else "mg/l",
          to = if (grepl("prct", trigger_name)) "mg/l" else "%Air",
          S = 0,
          t = temp,
          P = baro_press / 750.06156130264
        ))
        updateNumericInput(session, update_name, value = round(res, 2))
        if (messages) {
          alert(
            "You MUST recalculate if updating baro pressures or temperature",
            "Cannot auto-update without knowing which value (mg/l or %) to update",
            timer = 4000,
            type = "warning"
          )
        }
        updateActionButton(session, "calc_abs_do", "Recalc mg/l values")
        updateActionButton(session, "calc_prct_do", "Recalc % values")
      }
    }

    observeEvent(
      input$calc_abs_do,
      {
        DO_calc(pre_post = "pre", prct_abs = "prct")
        if (!is_check_only_mode("do")) {
          DO_calc(pre_post = "post", prct_abs = "prct", messages = FALSE)
        }
      },
      ignoreInit = T
    )
    observeEvent(
      input$calc_prct_do,
      {
        DO_calc(pre_post = "pre", prct_abs = "abs")
        if (!is_check_only_mode("do")) {
          DO_calc(pre_post = "post", prct_abs = "abs", messages = FALSE)
        }
      },
      ignoreInit = T
    )

    observeEvent(
      input$ID_sensor_holder,
      {
        if (
          nrow(instruments_data$others[
            instruments_data$others$serial_no == input$ID_sensor_holder,
          ]) >
            0
        ) {
          if (
            instruments_data$others[
              instruments_data$others$serial_no == input$ID_sensor_holder,
              "make"
            ] ==
              "Solinst"
          ) {
            updateCheckboxInput(session, "spc_or_not", value = TRUE)
          } else {
            updateCheckboxInput(session, "spc_or_not", value = FALSE)
          }
        }
      },
      ignoreInit = TRUE
    )

    # Calibration data saving, updating, and deleting #########################################################
    ## Save basic info ##############################################################################
    observeEvent(
      input$save_basic_info,
      {
        if (nchar(input$ID_sensor_holder) < 1) {
          alert(
            title = "Fill in the logger/bulkhead serial #",
            type = "error",
            timer = 2000
          )
          return()
        }
        if (
          input$ID_handheld_meter == "NA" &
            instruments_data$others[
              instruments_data$others$serial_no == input$ID_sensor_holder,
              "type"
            ] ==
              "Bulkhead"
        ) {
          alert(
            title = "Error: no handheld specified",
            text = "Handheld unit is integral to bulkhead calibrations and must be entered.",
            type = "error",
            timer = 4000
          )
          return()
        }
        if (
          input$ID_handheld_meter != "NA" &
            !(instruments_data$others[
              instruments_data$others$serial_no == input$ID_sensor_holder,
              "type"
            ] %in%
              c("Bulkhead", "Sonde"))
        ) {
          alert(
            title = "Error: You specified a handheld meter but your sensor holder is not a bulkhead or sonde.",
            text = "Only bulkheads and sondes should have associated handheld units.",
            type = "error",
            timer = 4000
          )
          return()
        }
        if (nchar(input$observer) < 1) {
          alert(
            title = "Fill in the observer name",
            type = "error",
            timer = 2000
          )
          return()
        }

        dt <- scalar_utc_datetime(input$obs_datetime)

        id_sensor_holder <- instruments_data$sheet[
          instruments_data$sheet$serial_no == input$ID_sensor_holder,
          "instrument_id"
        ]
        id_handheld_meter <- if (input$ID_handheld_meter == "NA") {
          NA
        } else {
          instruments_data$sheet[
            instruments_data$sheet$serial_no == input$ID_handheld_meter,
            "instrument_id"
          ]
        }
        purpose_text <- if (nchar(input$calibration_purpose) < 1) {
          NA
        } else {
          input$calibration_purpose
        }

        calibration_data$basic <- data.frame(
          observer = input$observer,
          obs_datetime = dt,
          id_sensor_holder = id_sensor_holder,
          id_handheld_meter = id_handheld_meter,
          purpose = purpose_text,
          complete = FALSE
        )
        if (!complete$basic) {
          # New entry
          DBI::dbAppendTable(
            session$userData$AquaCache,
            "calibrations",
            calibration_data$basic
          )
          calibration_data$next_id <- DBI::dbGetQuery(
            session$userData$AquaCache,
            "SELECT MAX(calibration_id) FROM calibrations"
          )[[1]]
          complete$basic <- TRUE
        } else {
          # Modify an existing entry
          DBI::dbExecute(
            session$userData$AquaCache,
            paste0(
              "UPDATE calibrations SET observer = ",
              sql_string_or_null(input$observer),
              ", obs_datetime = ",
              sql_string_or_null(as.character(dt)),
              ", id_sensor_holder = ",
              id_sensor_holder,
              ", id_handheld_meter = ",
              if (is.na(id_handheld_meter)) {
                "NULL"
              } else {
                id_handheld_meter
              },
              ", purpose = ",
              sql_string_or_null(purpose_text),
              " WHERE calibration_id = ",
              calibration_data$next_id,
              ";"
            )
          )
        }
        if (
          entry_display_labels$basic %in%
            send_table$saved[, 1] |
            entry_display_labels$basic %in% send_table$restarted_cal[, 1]
        ) {
          alert(
            title = "Basic record info overwritten",
            type = "success",
            timer = 2000
          )
        } else {
          alert(
            title = "Basic record info saved",
            type = "success",
            timer = 2000
          )
        }
        if (send_table$saved[1, 1] == "Nothing saved yet") {
          send_table$saved[1, 1] <- entry_display_labels$basic
        } else if (!(entry_display_labels$basic %in% send_table$saved[, 1])) {
          send_table$saved[nrow(send_table$saved) + 1, 1] <-
            entry_display_labels$basic
        }
        output$saved <- renderTable({
          # Display local calibrations table
          send_table$saved
        })
      },
      ignoreInit = TRUE
    )

    ## Validate/Save/Delete pH ##############################################################################
    observeEvent(
      input$save_cal_ph,
      {
        validation_check$ph <- FALSE
        tryCatch(
          {
            ph_check_only <- is_check_only_mode("ph")
            ph_value_ids <- if (ph_check_only) {
              c("ph1_pre_val", "ph2_pre_val", "ph3_pre_val")
            } else {
              c("ph1_post_val", "ph2_post_val", "ph3_post_val")
            }
            #Check the standard values entered
            std1 <- as.numeric(input$ph1_std)
            std2 <- as.numeric(input$ph2_std)
            std3 <- as.numeric(input$ph3_std)
            warn_ph_std <- FALSE
            warn_ph_post <- FALSE
            warn_mv_post <- FALSE
            if (std1 != 4) {
              shinyjs::js$backgroundCol("ph1_std", "lemonchiffon")
              warn_ph_std <- TRUE
            } else {
              shinyjs::js$backgroundCol("ph1_std", "white")
            }
            if (std2 != 7) {
              shinyjs::js$backgroundCol("ph2_std", "lemonchiffon")
              warn_ph_std <- TRUE
            } else {
              shinyjs::js$backgroundCol("ph2_std", "white")
            }
            if (std3 != 10) {
              shinyjs::js$backgroundCol("ph3_std", "lemonchiffon")
              warn_ph_std <- TRUE
            } else {
              shinyjs::js$backgroundCol("ph3_std", "white")
            }
            #Validate the pH measurements vs the standards
            value1 <- as.numeric(input[[ph_value_ids[[1]]]])
            if (
              value1 < (std1 - 0.1) | value1 > (std1 + 0.1) | is.null(value1)
            ) {
              #tolerance of 0.1 pH units from the stated calibration standard value
              shinyjs::js$backgroundCol(ph_value_ids[[1]], "red")
              warn_ph_post <- TRUE
            } else {
              shinyjs::js$backgroundCol(ph_value_ids[[1]], "white")
            }
            value2 <- as.numeric(input[[ph_value_ids[[2]]]])
            if (
              value2 < (std2 - 0.1) | value2 > (std2 + 0.1) | is.null(value2)
            ) {
              #tolerance of 0.1 pH units from the stated calibration standard value
              shinyjs::js$backgroundCol(ph_value_ids[[2]], "red")
              warn_ph_post <- TRUE
            } else {
              shinyjs::js$backgroundCol(ph_value_ids[[2]], "white")
            }
            value3 <- as.numeric(input[[ph_value_ids[[3]]]])
            if (
              value3 < (std3 - 0.1) | value3 > (std3 + 0.1) | is.null(value3)
            ) {
              #tolerance of 0.1 pH units from the stated calibration standard value
              shinyjs::js$backgroundCol(ph_value_ids[[3]], "red")
              warn_ph_post <- TRUE
            } else {
              shinyjs::js$backgroundCol(ph_value_ids[[3]], "white")
            }
            # Validate the mV readings
            ph1_mv <- as.numeric(input$ph1_mv)
            ph2_mv <- as.numeric(input$ph2_mv)
            ph3_mv <- as.numeric(input$ph3_mv)
            if (
              (ph1_mv < (165 + ph2_mv)) |
                (ph1_mv > (180 + ph2_mv)) & (std1 > 3.9 & std1 < 4.1)
            ) {
              shinyjs::js$backgroundCol("ph1_mv", "red")
              warn_mv_post <- TRUE
            } else if (std1 != 4) {
              shinyjs::js$backgroundCol("ph1_mv", "lemonchiffon")
            } else {
              shinyjs::js$backgroundCol("ph1_mv", "white")
            }
            if ((ph2_mv > 50 | ph2_mv < -50) & (std2 > 6.9 & std2 < 7.1)) {
              shinyjs::js$backgroundCol("ph2_mv", "red")
              warn_mv_post <- TRUE
            } else if (std2 != 7) {
              shinyjs::js$backgroundCol("ph2_mv", "lemonchiffon")
            } else {
              shinyjs::js$backgroundCol("ph2_mv", "white")
            }
            if (
              (ph3_mv > (ph2_mv - 165)) |
                (ph3_mv < (ph2_mv - 180)) & (std3 > 9.9 & std3 < 10.1)
            ) {
              shinyjs::js$backgroundCol("ph3_mv", "red")
              warn_mv_post <- TRUE
            } else if (std3 != 10) {
              shinyjs::js$backgroundCol("ph3_mv", "lemonchiffon")
            } else {
              shinyjs::js$backgroundCol("ph3_mv", "white")
            }
            if (warn_ph_std | warn_ph_post | warn_mv_post) {
              warnings <- paste0(
                if (warn_ph_std) {
                  "Are you sure your standards are correct? If yes, checks on mV outputs will be invalid; use your judgement.<br><br>"
                } else {
                  ""
                },
                if (warn_ph_post) {
                  paste0(
                    "Some of your ",
                    if (ph_check_only) {
                      "observed check"
                    } else {
                      "as-left"
                    },
                    " pH values are > 0.1 units from their standards!",
                    " Check your inputs.<br><br>"
                  )
                } else {
                  ""
                },
                if (warn_mv_post) {
                  paste(
                    if (ph_check_only) {
                      "Observed"
                    } else {
                      "As-left"
                    },
                    "mV values are outside of the valid range;",
                    "consider replacing the electrode or sensor."
                  )
                }
              )
              # Show a modal to the user to confirm that they are sure about their entries
              showModal(modalDialog(
                title = "Are you sure?",
                HTML(warnings),
                footer = tagList(
                  modal_action_button("ok_check_ph", "Yes, I'm sure"),
                  modalButton("Cancel")
                )
              ))
            } else {
              validation_check$ph <- TRUE
            }
          },
          error = function(e) {
            alert(
              title = "You have unfilled mandatory entries",
              text = "If doing a 2-point calibration enter 0 for the third solution values to pass this check.",
              type = "error",
              timer = 4000
            )
          }
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$ok_check_ph,
      {
        validation_check$ph <- TRUE
      },
      ignoreInit = TRUE
    )

    observeEvent(
      validation_check$ph,
      {
        if (!validation_check$ph) {
          return()
        } else {
          ph_check_only <- is_check_only_mode("ph")
          calibration_data$ph <- data.frame(
            calibration_id = calibration_data$next_id,
            ph1_std = input$ph1_std,
            ph2_std = input$ph2_std,
            ph3_std = input$ph3_std,
            ph1_pre_val = input$ph1_pre_val,
            ph2_pre_val = input$ph2_pre_val,
            ph3_pre_val = input$ph3_pre_val,
            ph1_mv = input$ph1_mv,
            ph2_mv = input$ph2_mv,
            ph3_mv = input$ph3_mv,
            ph1_post_val = if (ph_check_only) NA_real_ else input$ph1_post_val,
            ph2_post_val = if (ph_check_only) NA_real_ else input$ph2_post_val,
            ph3_post_val = if (ph_check_only) NA_real_ else input$ph3_post_val
          )
          calibration_data$ph$check_only <- ph_check_only

          if (!complete$ph) {
            DBI::dbAppendTable(
              session$userData$AquaCache,
              "calibrate_ph",
              calibration_data$ph
            )

            complete$ph <- TRUE
            shinyjs::show("delete_ph")
          } else {
            DBI::dbExecute(
              session$userData$AquaCache,
              paste0(
                "UPDATE calibrate_ph SET ph1_std = ",
                input$ph1_std,
                ", ph2_std = ",
                input$ph2_std,
                ", ph3_std = ",
                input$ph3_std,
                ", ph1_pre_val = ",
                input$ph1_pre_val,
                ", ph2_pre_val = ",
                input$ph2_pre_val,
                ", ph3_pre_val = ",
                input$ph3_pre_val,
                ", ph1_mv = ",
                input$ph1_mv,
                ", ph2_mv = ",
                input$ph2_mv,
                ", ph3_mv = ",
                input$ph3_mv,
                ", ph1_post_val = ",
                sql_numeric_or_null(calibration_data$ph$ph1_post_val),
                ", ph2_post_val = ",
                sql_numeric_or_null(calibration_data$ph$ph2_post_val),
                ", ph3_post_val = ",
                sql_numeric_or_null(calibration_data$ph$ph3_post_val),
                paste0(
                  ", check_only = ",
                  sql_boolean_literal(ph_check_only)
                ),
                " WHERE calibration_id = ",
                calibration_data$next_id
              )
            )
          }
          if (
            entry_display_labels$ph %in%
              send_table$saved[, 1] |
              entry_display_labels$ph %in% send_table$restarted_cal[, 1]
          ) {
            alert(
              title = "pH record overwritten",
              type = "success",
              timer = 2000
            )
          } else {
            alert(
              title = "pH record saved",
              type = "success",
              timer = 2000
            )
          }
          if (send_table$saved[1, 1] == "Nothing saved yet") {
            send_table$saved[1, 1] <- entry_display_labels$ph
          } else if (!(entry_display_labels$ph %in% send_table$saved[, 1])) {
            send_table$saved[nrow(send_table$saved) + 1, 1] <-
              entry_display_labels$ph
          }
          output$saved <- renderTable({
            # Display local calibrations table
            send_table$saved
          })
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$delete_ph,
      {
        #delete on remote sheet
        DBI::dbExecute(
          session$userData$AquaCache,
          paste0(
            "DELETE FROM calibrate_ph WHERE calibration_id = ",
            calibration_data$next_id
          )
        )
        #reset the fields
        reset_ph()
        #remove from display tables
        send_table$saved <- remove_calibration_entry(
          send_table$saved,
          entry_display_labels$ph
        )
        send_table$restarted_cal <- remove_calibration_entry(
          send_table$restarted_cal,
          entry_display_labels$ph
        )
        if (nrow(send_table$saved) == 0) {
          if (!restarted$restarted) {
            send_table$saved <- data.frame(
              "Saved records" = "Nothing saved yet",
              check.names = FALSE
            )
          } else {
            send_table$saved <- data.frame(
              "Saved records (this session)" = "Nothing saved yet",
              check.names = FALSE
            )
          }
        }
        alert("Deleted", type = "success", timer = 2000)
        output$saved <- renderTable({
          # Display local calibrations table
          send_table$saved
        })
        output$restart_table <- renderTable({
          # Display remotely saved calibrations tables
          send_table$restarted_cal
        })
        complete$ph <- FALSE
      },
      ignoreInit = TRUE
    )

    ### Validate/Save/Delete temperature ##############################################################################
    observeEvent(
      input$save_cal_temp,
      {
        validation_check$temp <- FALSE
        tryCatch(
          {
            temp_ref <- input$temp_reference
            temp_meas <- input$temp_observed
            temp_diff <- abs(temp_ref - temp_meas)
            message <- character(0)
            if (temp_diff > 0.2) {
              shinyjs::js$backgroundCol("temp_observed", "red")
              shinyjs::js$backgroundCol("temp_reference", "red")
              message <- "Temperature difference is > 0.2 degrees C!"
            } else if (temp_diff > 0.1) {
              shinyjs::js$backgroundCol("temp_observed", "lemonchiffon")
              shinyjs::js$backgroundCol("temp_reference", "lemonchiffon")
              message <- "Temperature difference is > 0.1 degrees C!"
            } else {
              shinyjs::js$backgroundCol("temp_observed", "white")
              shinyjs::js$backgroundCol("temp_reference", "white")
            }
            if (length(message) > 0) {
              # Show a modal to the user to confirm that they are sure about their entries
              showModal(modalDialog(
                title = "Are you sure?",
                message,
                footer = tagList(
                  modal_action_button("ok_check_temp", "Yes, I'm sure"),
                  modalButton("Cancel")
                )
              ))
            } else {
              validation_check$temp <- TRUE
            }
          },
          error = function(e) {
            alert(
              title = "You have unfilled mandatory entries",
              type = "error",
              timer = 2000
            )
          }
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$ok_check_temp,
      {
        validation_check$temp <- TRUE
      },
      ignoreInit = TRUE
    )

    observeEvent(
      validation_check$temp,
      {
        if (!validation_check$temp) {
          return()
        } else {
          calibration_data$temp <- data.frame(
            calibration_id = calibration_data$next_id,
            temp_reference_desc = input$temp_reference_desc,
            temp_reference = input$temp_reference,
            temp_observed = input$temp_observed
          )
          if (!complete$temperature) {
            DBI::dbAppendTable(
              session$userData$AquaCache,
              "calibrate_temperature",
              calibration_data$temp
            )
            complete$temperature <- TRUE
            shinyjs::show("delete_temp")
          } else {
            DBI::dbExecute(
              session$userData$AquaCache,
              paste0(
                "UPDATE calibrate_temperature SET temp_reference_desc = ",
                sql_string_or_null(input$temp_reference_desc),
                ", temp_reference = ",
                input$temp_reference,
                ", temp_observed = ",
                input$temp_observed,
                " WHERE calibration_id = ",
                calibration_data$next_id
              )
            )
          }
          if (
            entry_display_labels$temperature %in%
              send_table$saved[, 1] |
              entry_display_labels$temperature %in%
                send_table$restarted_cal[, 1]
          ) {
            alert(
              title = "Temperature check overwritten",
              type = "success",
              timer = 2000
            )
          } else {
            alert(
              title = "Temperature check saved",
              type = "success",
              timer = 2000
            )
          }
          if (send_table$saved[1, 1] == "Nothing saved yet") {
            send_table$saved[1, 1] <- entry_display_labels$temperature
          } else if (
            !(entry_display_labels$temperature %in% send_table$saved[, 1])
          ) {
            send_table$saved[
              nrow(send_table$saved) + 1,
              1
            ] <- entry_display_labels$temperature
          }
          output$saved <- renderTable({
            # Display local calibrations table
            send_table$saved
          })
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$delete_temp,
      {
        DBI::dbExecute(
          session$userData$AquaCache,
          paste0(
            "DELETE FROM calibrate_temperature WHERE calibration_id = ",
            calibration_data$next_id
          )
        )
        #reset the fields
        reset_temp()
        #remove from display tables
        send_table$saved <- remove_calibration_entry(
          send_table$saved,
          entry_display_labels$temperature
        )
        send_table$restarted_cal <- remove_calibration_entry(
          send_table$restarted_cal,
          entry_display_labels$temperature
        )
        if (nrow(send_table$saved) == 0) {
          if (!restarted$restarted) {
            send_table$saved <- data.frame(
              "Saved records" = "Nothing saved yet",
              check.names = FALSE
            )
          } else {
            send_table$saved <- data.frame(
              "Saved records (this session)" = "Nothing saved yet",
              check.names = FALSE
            )
          }
        }
        alert("Deleted", type = "success", timer = 2000)
        output$saved <- renderTable({
          # Display local calibrations table
          send_table$saved
        })
        output$restart_table <- renderTable({
          # Display remotely saved calibrations tables
          send_table$restarted_cal
        })
        complete$temperature <- FALSE
      },
      ignoreInit = TRUE
    )

    ## Validate/Save/Delete ORP ##############################################################################
    observeEvent(
      input$save_cal_orp,
      {
        validation_check$orp <- FALSE
        tryCatch(
          {
            orp_check_only <- is_check_only_mode("orp")
            orp_value_id <- if (orp_check_only) "orp_pre_mv" else "orp_post_mv"
            orp_std <- input$orp_std
            orp_post <- input[[orp_value_id]]
            orp_diff <- abs(orp_std - orp_post)
            if (orp_diff > 10) {
              shinyjs::js$backgroundCol("orp_std", "red")
              shinyjs::js$backgroundCol(orp_value_id, "red")
            } else if (orp_diff > 5) {
              shinyjs::js$backgroundCol("orp_std", "lemonchiffon")
              shinyjs::js$backgroundCol(orp_value_id, "lemonchiffon")
            } else {
              shinyjs::js$backgroundCol("orp_std", "white")
              shinyjs::js$backgroundCol(orp_value_id, "white")
            }
            if (orp_diff > 5) {
              # Show a modal to the user to confirm that they are sure about their entries
              showModal(modalDialog(
                title = "Are you sure?",
                paste(
                  "ORP difference is > 5 mV for the",
                  if (orp_check_only) {
                    "observed check value;"
                  } else {
                    "as-left value;"
                  },
                  "are you sure about your entries?"
                ),
                footer = tagList(
                  modal_action_button("ok_check_orp", "Yes, I'm sure"),
                  modalButton("Cancel")
                )
              ))
            } else {
              validation_check$orp <- TRUE
            }
          },
          error = function(e) {
            alert(
              title = "You have unfilled mandatory entries",
              type = "error",
              timer = 2000
            )
          }
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$ok_check_orp,
      {
        validation_check$orp <- TRUE
      },
      ignoreInit = TRUE
    )

    observeEvent(
      validation_check$orp,
      {
        if (!validation_check$orp) {
          return()
        } else {
          orp_check_only <- is_check_only_mode("orp")
          calibration_data$orp <- data.frame(
            calibration_id = calibration_data$next_id,
            orp_std = input$orp_std,
            orp_pre_mv = input$orp_pre_mv,
            orp_post_mv = if (orp_check_only) NA_real_ else input$orp_post_mv
          )
          calibration_data$orp$check_only <- orp_check_only

          if (!complete$orp) {
            DBI::dbAppendTable(
              session$userData$AquaCache,
              "calibrate_orp",
              calibration_data$orp
            )
            complete$orp <- TRUE
            shinyjs::show("delete_orp")
          } else {
            DBI::dbExecute(
              session$userData$AquaCache,
              paste0(
                "UPDATE calibrate_orp SET orp_std = ",
                input$orp_std,
                ", orp_pre_mv = ",
                input$orp_pre_mv,
                ", orp_post_mv = ",
                sql_numeric_or_null(calibration_data$orp$orp_post_mv),
                paste0(
                  ", check_only = ",
                  sql_boolean_literal(orp_check_only)
                ),
                " WHERE calibration_id = ",
                calibration_data$next_id
              )
            )
          }
          if (
            entry_display_labels$orp %in%
              send_table$saved[, 1] |
              entry_display_labels$orp %in% send_table$restarted_cal[, 1]
          ) {
            alert(
              title = "ORP record overwritten",
              type = "success",
              timer = 2000
            )
          } else {
            alert(
              title = "ORP record saved",
              type = "success",
              timer = 2000
            )
          }
          if (send_table$saved[1, 1] == "Nothing saved yet") {
            send_table$saved[1, 1] <- entry_display_labels$orp
          } else if (!(entry_display_labels$orp %in% send_table$saved[, 1])) {
            send_table$saved[nrow(send_table$saved) + 1, 1] <-
              entry_display_labels$orp
          }
          output$saved <- renderTable({
            # Display local calibrations table
            send_table$saved
          })
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$delete_orp,
      {
        DBI::dbExecute(
          session$userData$AquaCache,
          paste0(
            "DELETE FROM calibrate_orp WHERE calibration_id = ",
            calibration_data$next_id
          )
        )
        #reset the fields
        reset_orp()
        #remove from display tables
        send_table$saved <- remove_calibration_entry(
          send_table$saved,
          entry_display_labels$orp
        )
        send_table$restarted_cal <- remove_calibration_entry(
          send_table$restarted_cal,
          entry_display_labels$orp
        )
        if (nrow(send_table$saved) == 0) {
          if (!restarted$restarted) {
            send_table$saved <- data.frame(
              "Saved records" = "Nothing saved yet",
              check.names = FALSE
            )
          } else {
            send_table$saved <- data.frame(
              "Saved records (this session)" = "Nothing saved yet",
              check.names = FALSE
            )
          }
        }
        alert("Deleted", type = "success", timer = 2000)
        output$saved <- renderTable({
          # Display local calibrations table
          send_table$saved
        })
        output$restart_table <- renderTable({
          # Display remotely saved calibrations tables
          send_table$restarted_cal
        })
        complete$orp <- FALSE
      },
      ignoreInit = TRUE
    )

    ## Validate/Save/Delete SpC ##############################################################################
    observeEvent(
      input$save_cal_spc,
      {
        validation_check$spc <- FALSE
        point_count <- current_spc_point_count()
        spc_check_only <- is_check_only_mode("spc")
        if (isTRUE(input$spc_or_not) && !isTRUE(complete$temperature)) {
          alert(
            "Save temperature check first!",
            "You must save the temperature check first if entering non-specific conductivity.",
            type = "error",
            timer = 3000
          )
          updateSelectizeInput(
            session,
            "selection",
            selected = "Temperature calibration"
          )
          return()
        }
        if (!spc_supports_extended_schema && point_count != 2L) {
          alert(
            "Apply the conductivity schema patch first.",
            paste(
              "This database currently supports only 2-point conductivity entries.",
              "Run the schema patch before saving 1-point, 3-point, or check-only conductivity records."
            ),
            type = "error",
            timer = 4000
          )
          updateRadioButtons(session, "spc_points", selected = 2)
          return()
        }
        temp_reference <- if (isTRUE(input$spc_or_not)) {
          current_temp_reference()
        } else {
          NA_real_
        }
        if (
          isTRUE(input$spc_or_not) &&
            (length(temp_reference) == 0 || is.na(temp_reference))
        ) {
          alert(
            "Save temperature check first!",
            "Temperature check data is missing. Re-save the temperature check before entering non-specific conductivity.",
            type = "error",
            timer = 3000
          )
          updateSelectizeInput(
            session,
            "selection",
            selected = "Temperature calibration"
          )
          return()
        }
        for (i in seq_len(point_count)) {
          if (!spc_check_only && is.na(input[[paste0("spc", i, "_post")]])) {
            updateNumericInput(
              session,
              paste0("spc", i, "_post"),
              value = default_spc_post_value(input[[paste0("spc", i, "_std")]])
            )
          }
          if (is.na(input[[paste0("spc", i, "_pre")]])) {
            point_text <- if (point_count == 1L) {
              "the conductivity/conductance standard"
            } else {
              paste(
                "the",
                tolower(spc_point_role(i, point_count)),
                "conductivity/conductance point"
              )
            }
            alert(
              paste(
                "You must enter",
                if (spc_check_only) {
                  "an observed check value for"
                } else {
                  "an as-found value for"
                },
                point_text
              ),
              type = "error",
              timer = 3000
            )
            return()
          }
        }

        tryCatch(
          {
            confirmation_messages <- character(0)
            compare_stage <- if (spc_check_only) "pre" else "post"
            compare_label <- if (spc_check_only) {
              "observed check value"
            } else {
              "as-left value"
            }
            for (i in 1:3) {
              std_id <- paste0("spc", i, "_std")
              value_id <- paste0("spc", i, "_", compare_stage)
              if (i > point_count) {
                shinyjs::js$backgroundCol(std_id, "white")
                shinyjs::js$backgroundCol(value_id, "white")
                next
              }
              spc_ref <- input[[std_id]]
              spc_value <- saved_spc_measurement(i, compare_stage)
              spc_diff <- abs(spc_ref - spc_value)
              point_name <- if (point_count == 1L) {
                "single-point"
              } else {
                tolower(spc_point_role(i, point_count))
              }
              if (spc_diff > 10) {
                shinyjs::js$backgroundCol(std_id, "red")
                shinyjs::js$backgroundCol(value_id, "red")
              } else if (spc_diff > 5) {
                shinyjs::js$backgroundCol(std_id, "lemonchiffon")
                shinyjs::js$backgroundCol(value_id, "lemonchiffon")
              } else {
                shinyjs::js$backgroundCol(std_id, "white")
                shinyjs::js$backgroundCol(value_id, "white")
              }
              if (spc_diff > 5) {
                if (input$spc_or_not) {
                  confirmation_messages <- c(
                    confirmation_messages,
                    paste0(
                      "Double check your values: your ",
                      point_name,
                      " ",
                      compare_label,
                      " converts to an SpC of ",
                      round(spc_value, 0),
                      " versus the expected ",
                      spc_ref
                    )
                  )
                } else {
                  confirmation_messages <- c(
                    confirmation_messages,
                    paste(
                      "Warning: double check your",
                      point_name,
                      compare_label,
                      "."
                    )
                  )
                }
              }
            }
            if (length(confirmation_messages) > 0) {
              showModal(modalDialog(
                title = "Are you sure?",
                HTML(paste(confirmation_messages, collapse = "<br><br>")),
                footer = tagList(
                  modal_action_button("ok_check_spc", "Yes, I'm sure"),
                  modalButton("Cancel")
                )
              ))
            } else {
              validation_check$spc <- TRUE
            }
          },
          error = function(e) {
            alert(
              title = "You have unfilled mandatory entries",
              type = "error",
              timer = 2000
            )
          }
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$ok_check_spc,
      {
        validation_check$spc <- TRUE
      },
      ignoreInit = TRUE
    )

    observeEvent(
      validation_check$spc,
      {
        if (!validation_check$spc) {
          return()
        } else {
          calibration_data$spc <- build_spc_record()

          if (!complete$spc) {
            tryCatch(
              {
                DBI::dbAppendTable(
                  session$userData$AquaCache,
                  "calibrate_specific_conductance",
                  calibration_data$spc
                )

                if (
                  entry_display_labels$spc %in%
                    send_table$saved[, 1] |
                    entry_display_labels$spc %in%
                      send_table$restarted_cal[, 1]
                ) {
                  alert(
                    title = "Conductivity record overwritten",
                    type = "success",
                    timer = 2000
                  )
                } else {
                  alert(
                    title = "Conductivity record saved",
                    type = "success",
                    timer = 2000
                  )
                }
                if (send_table$saved[1, 1] == "Nothing saved yet") {
                  send_table$saved[1, 1] <- entry_display_labels$spc
                } else if (
                  !(entry_display_labels$spc %in% send_table$saved[, 1])
                ) {
                  send_table$saved[
                    nrow(send_table$saved) + 1,
                    1
                  ] <- entry_display_labels$spc
                }
                output$saved <- renderTable({
                  # Display local calibrations table
                  send_table$saved
                })

                complete$spc <- TRUE
                shinyjs::show("delete_spc")
              },
              error = function(e) {
                alert(
                  title = "Failed to make new entry to database... are you sure all entries are correct and that you are entering specific or non-specific conductivity as required by your instrument?",
                  type = "error",
                  timer = 4000
                )
              }
            )
          } else {
            tryCatch(
              {
                if (spc_supports_extended_schema) {
                  DBI::dbExecute(
                    session$userData$AquaCache,
                    paste(
                      "UPDATE calibrate_specific_conductance",
                      "SET calibration_points = $1,",
                      "    spc1_std = $2,",
                      "    spc2_std = $3,",
                      "    spc1_pre = $4,",
                      "    spc2_pre = $5,",
                      "    spc1_post = $6,",
                      "    spc2_post = $7,",
                      "    spc3_std = $8,",
                      "    spc3_pre = $9,",
                      "    spc3_post = $10",
                      "    , check_only = $11",
                      paste0(
                        "WHERE calibration_id = $12"
                      )
                    ),
                    params = unname(as.list(c(
                      calibration_data$spc$calibration_points[1],
                      calibration_data$spc$spc1_std[1],
                      calibration_data$spc$spc2_std[1],
                      calibration_data$spc$spc1_pre[1],
                      calibration_data$spc$spc2_pre[1],
                      calibration_data$spc$spc1_post[1],
                      calibration_data$spc$spc2_post[1],
                      calibration_data$spc$spc3_std[1],
                      calibration_data$spc$spc3_pre[1],
                      calibration_data$spc$spc3_post[1],
                      calibration_data$spc$check_only[1],
                      calibration_data$next_id
                    )))
                  )
                } else {
                  DBI::dbExecute(
                    session$userData$AquaCache,
                    paste(
                      "UPDATE calibrate_specific_conductance",
                      "SET spc1_std = $1,",
                      "    spc1_pre = $2,",
                      "    spc1_post = $3,",
                      "    spc2_std = $4,",
                      "    spc2_pre = $5,",
                      "    spc2_post = $6",
                      "WHERE calibration_id = $7"
                    ),
                    params = unname(as.list(c(
                      calibration_data$spc$spc1_std[1],
                      calibration_data$spc$spc1_pre[1],
                      calibration_data$spc$spc1_post[1],
                      calibration_data$spc$spc2_std[1],
                      calibration_data$spc$spc2_pre[1],
                      calibration_data$spc$spc2_post[1],
                      calibration_data$next_id
                    )))
                  )
                }

                if (
                  entry_display_labels$spc %in%
                    send_table$saved[, 1] |
                    entry_display_labels$spc %in%
                      send_table$restarted_cal[, 1]
                ) {
                  alert(
                    title = "Conductivity record overwritten",
                    type = "success",
                    timer = 2000
                  )
                } else {
                  alert(
                    title = "Conductivity record saved",
                    type = "success",
                    timer = 2000
                  )
                }
                if (send_table$saved[1, 1] == "Nothing saved yet") {
                  send_table$saved[1, 1] <- entry_display_labels$spc
                } else if (
                  !(entry_display_labels$spc %in% send_table$saved[, 1])
                ) {
                  send_table$saved[
                    nrow(send_table$saved) + 1,
                    1
                  ] <- entry_display_labels$spc
                }
                output$saved <- renderTable({
                  # Display local calibrations table
                  send_table$saved
                })
              },
              error = function(e) {
                alert(
                  title = "Failed to edit existing database entry.",
                  text = e$message,
                  type = "error",
                  timer = 4000
                )
              }
            )
          }
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$delete_spc,
      {
        DBI::dbExecute(
          session$userData$AquaCache,
          paste0(
            "DELETE FROM calibrate_specific_conductance WHERE calibration_id = ",
            calibration_data$next_id
          )
        )
        #reset the fields
        reset_spc()
        #remove from display tables
        send_table$saved <- remove_calibration_entry(
          send_table$saved,
          entry_display_labels$spc
        )
        send_table$restarted_cal <- remove_calibration_entry(
          send_table$restarted_cal,
          entry_display_labels$spc
        )
        if (nrow(send_table$saved) == 0) {
          if (!restarted$restarted) {
            send_table$saved <- data.frame(
              "Saved records" = "Nothing saved yet",
              check.names = FALSE
            )
          } else {
            send_table$saved <- data.frame(
              "Saved records (this session)" = "Nothing saved yet",
              check.names = FALSE
            )
          }
        }
        alert("Deleted", type = "success", timer = 2000)
        output$saved <- renderTable({
          # Display local calibrations table
          send_table$saved
        })
        output$restart_table <- renderTable({
          # Display remotely saved calibrations tables
          send_table$restarted_cal
        })
        complete$spc <- FALSE
      },
      ignoreInit = TRUE
    )

    ### Save/Delete turbidity ##############################################################################
    observeEvent(
      input$save_cal_turb,
      {
        validation_check$turb <- FALSE
        turb_check_only <- is_check_only_mode("turbidity")
        tryCatch(
          {
            turb1_ref <- input$turb1_std
            turb2_ref <- input$turb2_std
            turb_value_ids <- c(
              if (turb_check_only) "turb1_pre" else "turb1_post",
              if (turb_check_only) "turb2_pre" else "turb2_post"
            )
            turb1_value <- input[[turb_value_ids[1]]]
            turb2_value <- input[[turb_value_ids[2]]]
            turb1_diff <- abs(turb1_ref - turb1_value)
            turb2_diff <- abs(turb2_ref - turb2_value)
            gtg1 <- FALSE
            gtg2 <- FALSE
            if (turb1_diff > 10) {
              shinyjs::js$backgroundCol("turb1_std", "red")
              shinyjs::js$backgroundCol(turb_value_ids[1], "red")
            } else if (turb1_diff > 5) {
              shinyjs::js$backgroundCol("turb1_std", "lemonchiffon")
              shinyjs::js$backgroundCol(turb_value_ids[1], "lemonchiffon")
            } else {
              shinyjs::js$backgroundCol("turb1_std", "white")
              shinyjs::js$backgroundCol(turb_value_ids[1], "white")
              gtg1 <- TRUE
            }
            if (turb2_diff > 10) {
              shinyjs::js$backgroundCol("turb2_std", "red")
              shinyjs::js$backgroundCol(turb_value_ids[2], "red")
            } else if (turb2_diff > 5) {
              shinyjs::js$backgroundCol("turb2_std", "lemonchiffon")
              shinyjs::js$backgroundCol(turb_value_ids[2], "lemonchiffon")
            } else {
              shinyjs::js$backgroundCol("turb2_std", "white")
              shinyjs::js$backgroundCol(turb_value_ids[2], "white")
              gtg2 <- TRUE
            }
            if (!gtg1 | !gtg2) {
              # Show a modal to the user to confirm that they are sure about their entries
              showModal(modalDialog(
                title = "Are you sure?",
                if (turb_check_only) {
                  "Your observed check values are a bit off from the standard. Please check your entries before moving on."
                } else {
                  "Your as-left values are a bit off from the standard. Please check your entries before moving on."
                },
                footer = tagList(
                  modal_action_button("ok_check_turb", "Yes, I'm sure"),
                  modalButton("Cancel")
                )
              ))
            } else {
              validation_check$turb <- TRUE
            }
          },
          error = function(e) {
            alert(
              title = "You have unfilled mandatory entries",
              type = "error",
              timer = 2000
            )
          }
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(input$ok_check_turb, {
      validation_check$turb <- TRUE
    })

    observeEvent(
      validation_check$turb,
      {
        if (!validation_check$turb) {
          return()
        } else {
          turb_check_only <- is_check_only_mode("turbidity")
          calibration_data$turb <- data.frame(
            calibration_id = calibration_data$next_id,
            turb1_std = input$turb1_std,
            turb1_pre = input$turb1_pre,
            turb1_post = if (turb_check_only) NA_real_ else input$turb1_post,
            turb2_std = input$turb2_std,
            turb2_pre = input$turb2_pre,
            turb2_post = if (turb_check_only) NA_real_ else input$turb2_post
          )
          calibration_data$turb$check_only <- turb_check_only

          if (!complete$turbidity) {
            DBI::dbAppendTable(
              session$userData$AquaCache,
              "calibrate_turbidity",
              calibration_data$turb
            )
            complete$turbidity <- TRUE
            shinyjs::show("delete_turb")
          } else {
            DBI::dbExecute(
              session$userData$AquaCache,
              paste0(
                "UPDATE calibrate_turbidity SET turb1_std = ",
                input$turb1_std,
                ", turb1_pre = ",
                input$turb1_pre,
                ", turb1_post = ",
                sql_numeric_or_null(calibration_data$turb$turb1_post),
                ", turb2_std = ",
                input$turb2_std,
                ", turb2_pre = ",
                input$turb2_pre,
                ", turb2_post = ",
                sql_numeric_or_null(calibration_data$turb$turb2_post),
                paste0(
                  ", check_only = ",
                  sql_boolean_literal(turb_check_only)
                ),
                " WHERE calibration_id = ",
                calibration_data$next_id
              )
            )
          }
          if (
            entry_display_labels$turbidity %in%
              send_table$saved[, 1] |
              entry_display_labels$turbidity %in% send_table$restarted_cal[, 1]
          ) {
            alert(
              title = "Turbidity record overwritten",
              type = "success",
              timer = 2000
            )
          } else {
            alert(
              title = "Turbidity record saved",
              type = "success",
              timer = 2000
            )
          }
          if (send_table$saved[1, 1] == "Nothing saved yet") {
            send_table$saved[1, 1] <- entry_display_labels$turbidity
          } else if (
            !(entry_display_labels$turbidity %in% send_table$saved[, 1])
          ) {
            send_table$saved[
              nrow(send_table$saved) + 1,
              1
            ] <- entry_display_labels$turbidity
          }
          output$saved <- renderTable({
            # Display local calibrations table
            send_table$saved
          })
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$delete_turb,
      {
        DBI::dbExecute(
          session$userData$AquaCache,
          paste0(
            "DELETE FROM calibrate_turbidity WHERE calibration_id = ",
            calibration_data$next_id
          )
        )
        #reset the fields
        reset_turb()
        #remove from display tables
        send_table$saved <- remove_calibration_entry(
          send_table$saved,
          entry_display_labels$turbidity
        )
        send_table$restarted_cal <- remove_calibration_entry(
          send_table$restarted_cal,
          entry_display_labels$turbidity
        )
        if (nrow(send_table$saved) == 0) {
          if (!restarted$restarted) {
            send_table$saved <- data.frame(
              "Saved records" = "Nothing saved yet",
              check.names = FALSE
            )
          } else {
            send_table$saved <- data.frame(
              "Saved records (this session)" = "Nothing saved yet",
              check.names = FALSE
            )
          }
        }
        alert("Deleted", type = "success", timer = 2000)
        output$saved <- renderTable({
          # Display local calibrations table
          send_table$saved
        })
        output$restart_table <- renderTable({
          # Display remotely saved calibrations tables
          send_table$restarted_cal
        })
        complete$turbidity <- FALSE
      },
      ignoreInit = TRUE
    )

    ## Validate/Save/Delete DO ##############################################################################
    observeEvent(
      input$save_cal_do,
      {
        validation_check$do <- FALSE
        do_check_only <- is_check_only_mode("do")
        tryCatch(
          {
            baro_value_id <- if (do_check_only) {
              "baro_press_pre"
            } else {
              "baro_press_post"
            }
            do_value_id <- if (do_check_only) "do_pre" else "do_post"
            baro_value <- input[[baro_value_id]]
            do_value <- input[[do_value_id]]
            message1 <- character(0)
            message2 <- character(0)
            if (baro_value < 600 | baro_value > 800) {
              message1 <- paste(
                if (do_check_only) {
                  "Observed"
                } else {
                  "As-left"
                },
                "baro pressure is not in range, are you sure?"
              )
            }
            if (do_value < 1 | do_value > 15) {
              message2 <- paste(
                if (do_check_only) {
                  "Observed"
                } else {
                  "As-left"
                },
                "DO mg/l is not in range; are you sure you entered % and mg/l in the right boxes? Only mg/l is saved, use the Fill/recalculate button."
              )
            }
            if (length(message1) > 0 | length(message2) > 0) {
              # Show a modal to the user to confirm that they are sure about their entries
              message <- paste0(
                if (length(message1) > 0) paste0(message1, "<br><br>") else "",
                message2
              )
              showModal(modalDialog(
                title = "Are you sure?",
                message,
                footer = tagList(
                  modal_action_button("ok_check_do", "Yes, I'm sure"),
                  modalButton("Cancel")
                )
              ))
            } else {
              validation_check$do <- TRUE
            }
          },
          error = function(e) {
            alert(
              title = "You have unfilled mandatory entries",
              if (do_check_only) {
                "Observed baro pressure and DO in mg/l are mandatory."
              } else {
                "As-left baro pressure and DO in mg/l are mandatory."
              },
              type = "error",
              timer = 4000
            )
          }
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(input$ok_check_do, {
      validation_check$do <- TRUE
    })

    observeEvent(
      validation_check$do,
      {
        if (!validation_check$do) {
          return()
        } else {
          do_check_only <- is_check_only_mode("do")
          calibration_data$do <- data.frame(
            calibration_id = calibration_data$next_id,
            baro_press_pre = input$baro_press_pre,
            baro_press_post = if (do_check_only) {
              NA_real_
            } else {
              input$baro_press_post
            },
            do_pre_mgl = input$do_pre,
            do_post_mgl = if (do_check_only) NA_real_ else input$do_post
          )
          calibration_data$do$check_only <- do_check_only

          if (!complete$do) {
            DBI::dbAppendTable(
              session$userData$AquaCache,
              "calibrate_dissolved_oxygen",
              calibration_data$do
            )

            complete$do <- TRUE
            shinyjs::show("delete_do")
          } else {
            DBI::dbExecute(
              session$userData$AquaCache,
              paste0(
                "UPDATE calibrate_dissolved_oxygen SET baro_press_pre = ",
                input$baro_press_pre,
                ", baro_press_post = ",
                sql_numeric_or_null(calibration_data$do$baro_press_post),
                ", do_pre_mgl = ",
                input$do_pre,
                ", do_post_mgl = ",
                sql_numeric_or_null(calibration_data$do$do_post_mgl),
                paste0(
                  ", check_only = ",
                  sql_boolean_literal(do_check_only)
                ),
                " WHERE calibration_id = ",
                calibration_data$next_id
              )
            )
          }
          if (
            entry_display_labels$do %in%
              send_table$saved[, 1] |
              entry_display_labels$do %in% send_table$restarted_cal[, 1]
          ) {
            alert(
              title = "DO record overwritten",
              type = "success",
              timer = 2000
            )
          } else {
            alert(
              title = "DO record saved",
              type = "success",
              timer = 2000
            )
          }
          if (send_table$saved[1, 1] == "Nothing saved yet") {
            send_table$saved[1, 1] <- entry_display_labels$do
          } else if (!(entry_display_labels$do %in% send_table$saved[, 1])) {
            send_table$saved[
              nrow(send_table$saved) + 1,
              1
            ] <- entry_display_labels$do
          }
          output$saved <- renderTable({
            # Display local calibrations table
            send_table$saved
          })
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$delete_do,
      {
        DBI::dbExecute(
          session$userData$AquaCache,
          paste0(
            "DELETE FROM calibrate_dissolved_oxygen WHERE calibration_id = ",
            calibration_data$next_id
          )
        )
        #reset the fields
        reset_do()
        #remove from display tables
        send_table$saved <- remove_calibration_entry(
          send_table$saved,
          entry_display_labels$do
        )
        send_table$restarted_cal <- remove_calibration_entry(
          send_table$restarted_cal,
          entry_display_labels$do
        )
        if (nrow(send_table$saved) == 0) {
          if (!restarted$restarted) {
            send_table$saved <- data.frame(
              "Saved records" = "Nothing saved yet",
              check.names = FALSE
            )
          } else {
            send_table$saved <- data.frame(
              "Saved records (this session)" = "Nothing saved yet",
              check.names = FALSE
            )
          }
        }
        alert("Deleted", type = "success", timer = 2000)
        output$saved <- renderTable({
          # Display local calibrations table
          send_table$saved
        })
        output$restart_table <- renderTable({
          # Display remotely saved calibrations tables
          send_table$restarted_cal
        })
        complete$do <- FALSE
      },
      ignoreInit = TRUE
    )

    ## Validate/Save/Delete depth ##############################################################################
    observeEvent(
      input$save_cal_depth,
      {
        validation_check$depth <- FALSE
        tryCatch(
          {
            depth_check <- input$depth_check_ok
            depth_change <- input$depth_changes_ok
            if (depth_check == "TRUE" & depth_change == "TRUE") {
              validation_check$depth <- TRUE
            } else {
              # Show a modal to the user to confirm that they are sure about their entries
              showModal(modalDialog(
                title = "Are you sure?",
                "You indicated FALSE or 'Not Checked' for one of the values. Are you sure about that? Should you be using a different sensor?",
                footer = tagList(
                  modal_action_button("ok_check_depth", "Yes, I'm sure"),
                  modalButton("Cancel")
                )
              ))
            }
          },
          error = function(e) {
            alert(
              title = "You have unfilled mandatory entries",
              type = "error",
              timer = 2000
            )
          }
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(input$ok_check_depth, {
      validation_check$depth <- TRUE
    })

    observeEvent(
      validation_check$depth,
      {
        if (!validation_check$depth) {
          return()
        } else {
          calibration_data$depth <- data.frame(
            calibration_id = calibration_data$next_id,
            depth_check_ok = input$depth_check_ok,
            depth_changes_ok = if (input$depth_changes_ok != "Not Checked") {
              input$depth_changes_ok
            } else {
              NA
            }
          )
          if (!complete$depth) {
            DBI::dbAppendTable(
              session$userData$AquaCache,
              "calibrate_depth",
              calibration_data$depth
            )
            complete$depth <- TRUE
            shinyjs::show("delete_depth")
          } else {
            if (input$depth_changes_ok != "Not Checked") {
              DBI::dbExecute(
                session$userData$AquaCache,
                paste0(
                  "UPDATE calibrate_depth SET depth_check_ok = '",
                  input$depth_check_ok,
                  "', depth_changes_ok ='",
                  input$depth_changes_ok,
                  "' WHERE calibration_id = ",
                  calibration_data$next_id
                )
              )
            } else {
              DBI::dbExecute(
                session$userData$AquaCache,
                paste0(
                  "UPDATE calibrate_depth SET depth_check_ok = '",
                  input$depth_check_ok,
                  "', depth_changes_ok = NULL WHERE calibration_id = ",
                  calibration_data$next_id
                )
              )
            }
          }
          if (
            entry_display_labels$depth %in%
              send_table$saved[, 1] |
              entry_display_labels$depth %in% send_table$restarted_cal[, 1]
          ) {
            alert(
              title = "Depth check overwritten",
              type = "success",
              timer = 2000
            )
          } else {
            alert(
              title = "Depth check saved",
              type = "success",
              timer = 2000
            )
          }
          if (send_table$saved[1, 1] == "Nothing saved yet") {
            send_table$saved[1, 1] <- entry_display_labels$depth
          } else if (!(entry_display_labels$depth %in% send_table$saved[, 1])) {
            send_table$saved[
              nrow(send_table$saved) + 1,
              1
            ] <- entry_display_labels$depth
          }
          output$saved <- renderTable({
            # Display local calibrations table
            send_table$saved
          })
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$delete_depth,
      {
        DBI::dbExecute(
          session$userData$AquaCache,
          paste0(
            "DELETE FROM calibrate_depth WHERE calibration_id = ",
            calibration_data$next_id
          )
        )

        #reset the fields
        reset_depth()
        #remove from display tables
        send_table$saved <- remove_calibration_entry(
          send_table$saved,
          entry_display_labels$depth
        )
        send_table$restarted_cal <- remove_calibration_entry(
          send_table$restarted_cal,
          entry_display_labels$depth
        )
        if (nrow(send_table$saved) == 0) {
          if (!restarted$restarted) {
            send_table$saved <- data.frame(
              "Saved records" = "Nothing saved yet",
              check.names = FALSE
            )
          } else {
            send_table$saved <- data.frame(
              "Saved records (this session)" = "Nothing saved yet",
              check.names = FALSE
            )
          }
        }
        alert("Deleted", type = "success", timer = 2000)
        output$saved <- renderTable({
          # Display local calibrations table
          send_table$saved
        })
        output$restart_table <- renderTable({
          # Display remotely saved calibrations tables
          send_table$restarted_cal
        })
        complete$depth <- FALSE
      },
      ignoreInit = TRUE
    )

    ### Finalize record and submit ##########################################
    observeEvent(
      {
        input$submit_btn
      },
      {
        if (
          !(entry_display_labels$basic %in%
            send_table$saved[, 1] |
            entry_display_labels$basic %in% send_table$restarted_cal[, 1])
        ) {
          alert(
            title = "There is no basic record info yet.",
            text = "Fill in your name, record time and date, and required serial numbers.",
            type = "error"
          )
        } else if (
          !(entry_display_labels$temperature %in%
            send_table$saved[, 1] |
            entry_display_labels$temperature %in% send_table$restarted_cal[, 1])
        ) {
          alert(
            title = "Temperature check is mandatory",
            text = "If you've filled it in already you probably forgot to save it!",
            type = "error"
          )
        } else {
          showModal(
            modalDialog(
              title = "Are you sure?",
              "Finalized records cannot be edited.",
              footer = tagList(
                modalButton("Cancel"),
                modal_action_button("confirm_finalize", "OK")
              )
            )
          )
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$confirm_finalize,
      {
        DBI::dbExecute(
          session$userData$AquaCache,
          paste0(
            "UPDATE calibrations SET complete = 'TRUE' WHERE calibration_id = ",
            calibration_data$next_id
          )
        )
        calibrations$calibrations <- DBI::dbReadTable(
          session$userData$AquaCache,
          "calibrations"
        )

        calibrations$calibrations[
          calibrations$calibrations$calibration_id == calibration_data$next_id,
          "complete"
        ] <- TRUE #mark as complete in the local df as well
        alert(title = "Record finalized.", type = "success")
        #Reset fields
        reset_basic(keep_observer = TRUE)
        reset_ph()
        reset_temp()
        reset_orp()
        reset_spc()
        reset_turb()
        reset_do()
        reset_depth()
        # Reset complete flags
        complete$basic <- FALSE
        complete$temperature <- FALSE
        complete$spc <- FALSE
        complete$ph <- FALSE
        complete$orp <- FALSE
        complete$turbidity <- FALSE
        complete$do <- FALSE
        complete$depth <- FALSE
        # Reset data.frames
        calibration_data$basic <- NULL
        calibration_data$temp <- NULL
        calibration_data$spc <- NULL
        calibration_data$ph <- NULL
        calibration_data$orp <- NULL
        calibration_data$turb <- NULL
        calibration_data$do <- NULL
        calibration_data$depth <- NULL
        calibration_data$restarted_id <- 0
        # Reset tables
        send_table$saved <- data.frame(
          "Saved records" = "Nothing saved yet",
          check.names = FALSE
        ) #Title is modified later for clarity if user want to restart a cal
        send_table$restarted_cal <- empty_restarted_cal_table()
        output$saved <- renderTable({
          # Display local calibrations table
          send_table$saved
        })
        output$restart_table <- renderTable({
          send_table$restarted_cal
        })
        shinyjs::hide("restart_table")
        restarted$restarted <- FALSE
        updateTabsetPanel(
          session,
          "tab_panel",
          selected = "Checks / calibrations"
        )
        updateSelectizeInput(
          session,
          "selection",
          selected = "Basic calibration info"
        )
        #Get the next ID in case user is calibrating/working in app again
        calibration_data$next_id <- NULL
      },
      ignoreInit = TRUE
    )
  }) # End of moduleServer
}
