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

      # Title
      titlePanel("Instrument Calibration and Tracking"),

      # Tabs
      tabsetPanel(
        id = ns("tab_panel"),
        tabPanel(
          "Calibrate",
          sidebarLayout(
            sidebarPanel(
              selectizeInput(
                ns("selection"),
                label = "Select a parameter",
                choices = c(
                  "Basic calibration info",
                  "Temperature calibration",
                  "Conductivity calibration",
                  "pH calibration",
                  "ORP calibration",
                  "Turbidity calibration",
                  "DO calibration",
                  "Depth calibration"
                )
              ),
              conditionalPanel(
                ns = ns,
                condition = "input.selection == 'Basic calibration info'",
                uiOutput(ns("observer")),
                shinyWidgets::airDatepickerInput(
                  ns("obs_datetime"),
                  label = "Calibration date/time (modify with the menu only)",
                  value = .POSIXct(Sys.time(), tz = "MST"),
                  range = FALSE,
                  multiple = FALSE,
                  timepicker = TRUE,
                  maxDate = Sys.Date() + 1,
                  startView = Sys.Date(),
                  update_on = "change",
                  timepickerOpts = shinyWidgets::timepickerOptions(
                    minutesStep = 15,
                    timeFormat = "HH:mm"
                  )
                ),
                textOutput(ns("instrument_reminder")),
                uiOutput(ns("ID_sensor_holder")),
                uiOutput(ns("ID_handheld_meter")),
                textInput(
                  ns("calibration_purpose"),
                  "Calibration purpose",
                  value = ""
                ),
                actionButton(ns("save_basic_info"), "Save this parameter info"),
              ),
              conditionalPanel(
                ns = ns,
                condition = "input.selection == 'pH calibration'",
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
                  label = "pH 4 Pre-Cal Value",
                  value = NA
                ),
                numericInput(
                  ns("ph2_pre_val"),
                  label = "pH 7 Pre-Cal Value",
                  value = NA
                ),
                numericInput(
                  ns("ph3_pre_val"),
                  label = "pH 10 Pre-Cal Value",
                  value = NA
                ),
                numericInput(ns("ph1_mv"), label = "pH 4 mV", value = NA),
                numericInput(ns("ph2_mv"), label = "pH 7 mV", value = NA),
                numericInput(ns("ph3_mv"), label = "pH 10 mV", value = NA),
                actionButton(ns("show_post_ph"), "Show post-cal fields"),
                numericInput(
                  ns("ph1_post_val"),
                  label = "pH 4 Post-Cal Value",
                  value = 4
                ),
                numericInput(
                  ns("ph2_post_val"),
                  label = "pH 7 Post-Cal Value",
                  value = 7
                ),
                numericInput(
                  ns("ph3_post_val"),
                  label = "pH 10 Post-Cal Value",
                  value = 10
                ),
                actionButton(ns("save_cal_ph"), "Save this sheet"),
                actionButton(ns("delete_ph"), "Delete this sheet")
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
                actionButton(ns("save_cal_temp"), "Save this sheet"),
                actionButton(ns("delete_temp"), "Delete this sheet")
              ),
              conditionalPanel(
                ns = ns,
                condition = "input.selection == 'Conductivity calibration'",
                numericInput(
                  ns("spc1_std"),
                  label = "SpC Low-Range Standard",
                  value = 0
                ),
                numericInput(
                  ns("spc2_std"),
                  label = "SpC High-Range Standard",
                  value = 1413
                ),
                checkboxInput(
                  ns("spc_or_not"),
                  "Enter non-specific conductivity instead?",
                  value = FALSE
                ),
                numericInput(
                  ns("spc1_pre"),
                  label = "SpC Low-Range Pre-Cal Value",
                  value = NA
                ),
                numericInput(
                  ns("spc2_pre"),
                  label = "SpC High-Range Pre-Cal Value",
                  value = NA
                ),
                actionButton(ns("show_post_spc"), "Show post-cal fields"),
                numericInput(
                  ns("spc1_post"),
                  label = "SpC Low-Range Post-Cal Value",
                  value = 0
                ),
                numericInput(
                  ns("spc2_post"),
                  label = "SpC High-Range Post-Cal Value",
                  value = 1413
                ),
                actionButton(ns("save_cal_spc"), "Save this sheet"),
                actionButton(ns("delete_spc"), "Delete this sheet")
              ),
              conditionalPanel(
                ns = ns,
                condition = "input.selection == 'ORP calibration'",
                numericInput(
                  ns("orp_std"),
                  label = "ORP Standard solution mV",
                  value = NA
                ),
                numericInput(
                  ns("orp_pre_mv"),
                  label = "ORP mV Pre-Cal Value",
                  value = NA
                ),
                actionButton(ns("show_post_orp"), "Show post-cal fields"),
                numericInput(
                  ns("orp_post_mv"),
                  label = "ORP mV Post-Cal Value",
                  value = NA
                ),
                actionButton(ns("save_cal_orp"), "Save this sheet"),
                actionButton(ns("delete_orp"), "Delete this sheet")
              ),
              conditionalPanel(
                ns = ns,
                condition = "input.selection == 'Turbidity calibration'",
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
                  label = "Low Turb Pre-cal Value",
                  value = NA
                ),
                numericInput(
                  ns("turb2_pre"),
                  label = "High Turb Pre-cal Value",
                  value = NA
                ),
                actionButton(ns("show_post_turb"), "Show post-cal fields"),
                numericInput(
                  ns("turb1_post"),
                  label = "Low Turb Post-cal Value",
                  value = 0
                ),
                numericInput(
                  ns("turb2_post"),
                  label = "High Turb Post-cal Value",
                  value = 124
                ),
                actionButton(ns("save_cal_turb"), "Save this sheet"),
                actionButton(ns("delete_turb"), "Delete this sheet")
              ),
              conditionalPanel(
                ns = ns,
                condition = "input.selection == 'DO calibration'",
                numericInput(
                  ns("baro_press_pre"),
                  label = "Baro Pressure Pre-Cal (mmHg)",
                  value = NA
                ),
                numericInput(
                  ns("baro_press_post"),
                  label = "Baro Pressure Post-Cal (mmHg)",
                  value = NA
                ),
                numericInput(
                  ns("do_pre_prct"),
                  label = "DO Pre-Cal % LOCAL",
                  value = NA
                ),
                numericInput(
                  ns("do_post_prct"),
                  label = "DO Post-Cal % LOCAL",
                  value = NA
                ),
                actionButton(ns("calc_abs_do"), "Calculate mg/l values"),
                actionButton(ns("calc_prct_do"), "Calculate % values"),
                numericInput(
                  ns("do_pre"),
                  label = "DO Pre-Cal mg/l",
                  value = NA
                ),
                numericInput(
                  ns("do_post"),
                  label = "DO Post-Cal mg/l",
                  value = NA
                ),
                actionButton(ns("save_cal_do"), "Save this sheet"),
                actionButton(ns("delete_do"), "Delete this sheet")
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
                actionButton(ns("save_cal_depth"), "Save this sheet"),
                actionButton(ns("delete_depth"), "Delete this sheet")
              ),
              actionButton(
                ns("submit_btn"),
                "Finalize + submit calibration",
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
          "Add/modify instruments",
          sidebarLayout(
            sidebarPanel(
              selectizeInput(
                ns("existing_serial_no"),
                "Search serial numbers or 'New record' for a new instrument",
                choices = "New record"
              ),
              textInput(
                ns("serial_no"),
                "New serial no (add alias by appending to serial #, e.g. 012345Blue)",
                value = "Search first!"
              ),
              uiOutput(ns("recorder")),
              selectizeInput(
                ns("make"),
                label = "Instrument make",
                choices = "placeholder"
              ),
              selectizeInput(
                ns("model"),
                label = "Instrument model",
                choices = "placeholder"
              ),
              selectizeInput(
                ns("type"),
                label = "Instrument type",
                choices = "placeholder"
              ),
              selectizeInput(
                ns("instrument_owner"),
                "Instrument owner",
                choices = NULL,
                multiple = TRUE,
                options(
                  maxItems = 1
                )
              ),
              checkboxInput(
                ns("replaceableSensors"),
                "Replaceable sensors?",
                value = FALSE
              ),
              textInput(
                ns("asset_tag"),
                "Asset tag number (if exists)",
                value = ""
              ),
              dateInput(
                ns("date_in_service"),
                label = "Date in service (if known)"
              ),
              dateInput(
                ns("date_purchased"),
                label = "Date purchased (if known)"
              ),
              dateInput(ns("date_retired"), label = "Date retired (if known)"),
              uiOutput(ns("retired_by")),
              actionButton(ns("save_cal_instrument"), "Save new instrument")
            ),
            mainPanel(
              DT::dataTableOutput(ns("manage_instruments_table"))
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
                choices = "placeholder"
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
                width = "500px"
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
              numericInput(
                ns("restart_index"),
                label = "Select a calibration by index number",
                value = 0
              ),
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

calibrate <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
    initial_instr_table <- reactiveValues(value = TRUE)

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

    # Hide the pH, ORP, and turbidity post-cal fields. They're still in the UI and in the code below in case ever needed, but hidden from view. Their values reflect the standard solution values, though a user could modify the fields if they become visible
    shinyjs::hide("ph1_post_val")
    shinyjs::hide("ph2_post_val")
    shinyjs::hide("ph3_post_val")
    shinyjs::hide("orp_post_mv")
    shinyjs::hide("turb1_post")
    shinyjs::hide("turb2_post")
    shinyjs::hide("spc1_post")
    shinyjs::hide("spc2_post")

    # Get the data from the database, make initial tables, populate UI elements ########################################
    instruments_sheet <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT i.instrument_id, i.obs_datetime,  CONCAT(observers.observer_first, ' ', observers.observer_last, '(', observers.organization, ')') AS observer, i.holds_replaceable_sensors, i.serial_no, i.asset_tag, i.date_in_service, i.date_purchased, i.retired_by, i.date_retired, instrument_make.make, instrument_model.model, instrument_type.type, i.owner FROM instruments AS i LEFT JOIN instrument_make ON i.make = instrument_make.make_id LEFT JOIN instrument_model ON i.model = instrument_model.model_id LEFT JOIN instrument_type ON i.type = instrument_type.type_id LEFT JOIN observers ON i.observer = observers.observer_id ORDER BY i.instrument_id"
    )

    calibrations <- reactiveValues()
    calibrations$calibrations <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT * FROM calibrations"
    ) # This will be used to check if there are any incomplete calibrations

    instruments_data$sheet <- instruments_sheet
    instruments_data$observers <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT * FROM observers"
    )
    instruments_data$makes <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT * FROM instrument_make"
    )
    instruments_data$models <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT * FROM instrument_model"
    )
    instruments_data$types <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT * FROM instrument_type"
    )
    instruments_data$handhelds <- instruments_sheet[
      instruments_sheet$type == "Handheld" &
        is.na(instruments_sheet$date_retired),
    ]
    instruments_data$others <- instruments_sheet[
      instruments_sheet$type != "Handheld" &
        is.na(instruments_sheet$date_retired),
    ]
    instruments_data$maintainable_sensors <- instruments_sheet[
      instruments_sheet$holds_replaceable_sensors,
    ]
    instruments_data$instrument_maintenance <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT * FROM instrument_maintenance"
    )
    instruments_data$organizations <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT * FROM organizations"
    )
    select_data$organizations <- setNames(
      c(instruments_data$organizations$organization_id, "new"),
      c(instruments_data$organizations$name, "Add new organization")
    )
    updateSelectizeInput(
      session,
      "instrument_owner",
      choices = select_data$organizations,
      selected = NULL
    )

    sensors_data$sensors <- DBI::dbGetQuery(
      session$userData$AquaCache,
      paste0("SELECT * FROM sensors")
    )
    sensors_data$sensor_types <- DBI::dbGetQuery(
      session$userData$AquaCache,
      paste0("SELECT * FROM sensor_types")
    )

    # Create initial tables for managing instruments
    initial_manage_instruments_table <- instruments_sheet[,
      !colnames(instruments_sheet) %in%
        c("instrument_id", "observer", "obs_datetime")
    ]
    initial_calibrate_instruments_table <- instruments_sheet[
      is.na(instruments_sheet$date_retired),
      !colnames(instruments_sheet) %in%
        c(
          "instrument_id",
          "observer",
          "obs_datetime",
          "retired_by",
          "date_retired",
          "date_purchased",
          "date_in_service"
        )
    ]
    output$manage_instruments_table <- DT::renderDT(
      initial_manage_instruments_table,
      rownames = FALSE,
      selection = "single"
    )
    output$calibration_instruments_table <- DT::renderDT(
      {
        DT::datatable(
          initial_calibrate_instruments_table,
          rownames = FALSE,
          filter = 'top',
          selection = "multiple",
          callback = htmlwidgets::JS(table_reset)
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
            selected = input$last_observer_id
          )
        })
        output$add_sensor_name <- renderUI({
          selectizeInput(
            ns("add_sensor_name"),
            label = "What's your name?",
            choices = select_data$recorder,
            selected = input$last_observer_id
          )
        })
        output$sensor_change_name <- renderUI({
          selectizeInput(
            ns("sensor_change_name"),
            label = "Observer name",
            choices = select_data$recorder,
            selected = input$last_observer_id
          )
        })
        output$maintain_recorder <- renderUI({
          selectizeInput(
            ns("maintain_recorder"),
            label = "Maintenance recorder",
            choices = select_data$recorder,
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
        select_data$recorder <- setNames(
          c(instruments_data$observers$observer_id, "new"),
          c(instruments_data$observers$observer_string, "Add new observer")
        )
        select_data$makes <- setNames(
          c(instruments_data$makes$make_id, "new"),
          c(instruments_data$makes$make, "Add new make")
        )
        select_data$models <- setNames(
          c(instruments_data$models$model_id, "new"),
          c(instruments_data$models$model, "Add new model")
        )
        select_data$types <- setNames(
          c(instruments_data$types$type_id, "new"),
          c(instruments_data$types$type, "Add new type")
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
            choices = select_data$recorder
          )
        })
        output$add_sensor_name <- renderUI({
          selectizeInput(
            ns("add_sensor_name"),
            label = "What's your name?",
            choices = select_data$recorder
          )
        })
        output$sensor_change_name <- renderUI({
          selectizeInput(
            ns("sensor_change_name"),
            label = "Observer name",
            choices = select_data$recorder
          )
        })
        output$maintain_recorder <- renderUI({
          selectizeInput(
            ns("maintain_recorder"),
            label = "Maintenance recorder",
            choices = select_data$recorder
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

        tmp <- setNames(
          c(sensors_data$sensor_types$sensor_type_id, "new"),
          c(sensors_data$sensor_types$sensor_type, "Add new type")
        )
        updateSelectizeInput(session, "change_sensor", choices = tmp)
        updateSelectizeInput(session, "add_sensor_type_dropdown", choices = tmp)

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
            "Index" = seq_len(nrow(incomplete_calibrations)),
            "Calibrator" = as.vector(incomplete_calibrations$observer_string),
            "Date/time UTC" = incomplete_calibrations$obs_datetime,
            "Purpose" = incomplete_calibrations$purpose,
            check.names = FALSE
          )
        } else {
          # Make a data.frame with no calibrations
          complete$incomplete <- data.frame(
            "Index" = 0,
            "Calibrator" = "No unsaved calibrations!",
            "Date/Time UTC" = "No unsaved calibrations!",
            "Purpose" = "No unsaved calibrations!",
            check.names = FALSE
          )
        }
        restarted$initialized <- TRUE
      }
    })

    # Modals and observers to add new observers, instrument makes, models, types, and sensor types to the DB ########################################
    ## Add new observers  ################################################
    observer_inputs <- c(
      "observer",
      "recorder",
      "retired_by",
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
          paste0(
            "INSERT INTO observers (observer_first, observer_last, organization) VALUES ('",
            input$new_observer_first,
            "', '",
            input$new_observer_last,
            "', '",
            input$new_observer_org,
            "')"
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
        select_data$recorder <- setNames(
          c(instruments_data$observers$observer_id, "new"),
          c(instruments_data$observers$observer_string, "Add new observer")
        )

        selected_id <- max(instruments_data$observers$observer_id)

        output$observer <- renderUI({
          selectizeInput(
            ns("observer"),
            label = "Calibrator name",
            choices = select_data$recorder,
            selected = selected_id
          )
        })
        output$recorder <- renderUI({
          selectizeInput(
            ns("recorder"),
            label = "Observer name",
            choices = select_data$recorder,
            selected = selected_id
          )
        })
        output$add_sensor_name <- renderUI({
          selectizeInput(
            ns("add_sensor_name"),
            label = "What's your name?",
            choices = select_data$recorder,
            selected = selected_id
          )
        })
        output$sensor_change_name <- renderUI({
          selectizeInput(
            ns("sensor_change_name"),
            label = "Observer name",
            choices = select_data$recorder,
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

    ## Add new instrument make ################################################
    observeEvent(
      input$make,
      {
        if (input$make == "new") {
          # Add a new make using a modal dialog
          showModal(modalDialog(
            title = "Add new make",
            textInput(ns("new_make"), "Make"),
            textInput(ns("new_make_desc"), "Description"),
            actionButton(ns("add_new_make"), "Add new make")
          ))
        }
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )
    observeEvent(
      input$add_new_make,
      {
        # Ensure that a make and description are entered
        if (input$new_make == "") {
          alert(
            title = "Error",
            text = "Please enter a make, description optional.",
            type = "error",
            timer = 4000
          )
          return()
        }
        DBI::dbExecute(
          session$userData$AquaCache,
          paste0(
            "INSERT INTO instrument_make (make, description) VALUES ('",
            input$new_make,
            "', '",
            input$new_make_desc,
            "')"
          )
        )
        instruments_data$makes <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT * FROM instrument_make"
        )
        select_data$makes <- setNames(
          c(instruments_data$makes$make_id, "new"),
          c(instruments_data$makes$make, "Add new make")
        )
        updateSelectizeInput(
          session,
          "make",
          choices = select_data$makes,
          selected = max(instruments_data$makes$make_id)
        )
        removeModal()
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    ## Add new instrument model ################################################
    observeEvent(
      input$model,
      {
        if (input$model == "new") {
          # Add a new model using a modal dialog
          showModal(modalDialog(
            title = "Add new model",
            textInput(ns("new_model"), "Model"),
            textInput(ns("new_model_desc"), "Description"),
            actionButton(ns("add_new_model"), "Add new model")
          ))
        }
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )
    observeEvent(
      input$add_new_model,
      {
        # Ensure that a model and description are entered
        if (input$new_model == "") {
          alert(
            title = "Error",
            text = "Please enter a model, description optional.",
            type = "error",
            timer = 4000
          )
          return()
        }
        DBI::dbExecute(
          session$userData$AquaCache,
          paste0(
            "INSERT INTO instrument_model (model, description) VALUES ('",
            input$new_model,
            "', '",
            input$new_model_desc,
            "')"
          )
        )
        instruments_data$models <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT * FROM instrument_model"
        )
        select_data$models <- setNames(
          c(instruments_data$models$model_id, "new"),
          c(instruments_data$models$model, "Add new model")
        )
        updateSelectizeInput(
          session,
          "model",
          choices = select_data$models,
          selected = max(instruments_data$models$model_id)
        )
        removeModal()
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    ## Add new instrument owner ###############################################
    observeEvent(
      input$instrument_owner,
      {
        if (input$instrument_owner == "new") {
          # Add a new owner using a modal dialog
          showModal(modalDialog(
            title = "Add new organization",
            textInput(ns("new_org_name"), "Name"),
            textInput(ns("new_org_name_fr"), "Name (French)"),
            textInput(ns("new_org_contact_name"), "Contact name (optional)"),
            textInput(ns("new_org_contact_email"), "Contact email (optional)"),
            textInput(ns("new_org_contact_phone"), "Contact phone (optional)"),
            textInput(ns("new_org_note"), "Note (optional)"),
            actionButton(ns("add_new_org"), "Add new organization")
          ))
        }
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )
    observeEvent(
      input$add_new_org,
      {
        # Ensure that a name is entered
        if (input$new_org_name == "" || input$new_org_name_fr == "") {
          alert(
            title = "Error",
            text = "Please enter an organization name, english and french.",
            type = "error",
            timer = 4000
          )
          return()
        }
        DBI::dbExecute(
          session$userData$AquaCache,
          "INSERT INTO public.organizations (name, name_fr, contact_name, email, phone, note) VALUES ($1, $2, $3, $4, $5, $6)",
          params = list(
            input$new_org_name,
            input$new_org_name_fr,
            ifelse(
              input$new_org_contact_name == "",
              NA,
              input$new_org_contact_name
            ),
            ifelse(
              input$new_org_contact_email == "",
              NA,
              input$new_org_contact_email
            ),
            ifelse(
              input$new_org_contact_phone == "",
              NA,
              input$new_org_contact_phone
            ),
            ifelse(input$new_org_note == "", NA, input$new_org_note)
          )
        )
        instruments_data$organizations <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT * FROM organizations"
        )
        select_data$organizations <- setNames(
          c(instruments_data$organizations$organization_id, "new"),
          c(instruments_data$organizations$name, "Add new organization")
        )
        updateSelectizeInput(
          session,
          "instrument_owner",
          choices = select_data$organizations,
          selected = max(instruments_data$organizations$organization_id)
        )
        removeModal()
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    ## Add new instrument type ################################################
    observeEvent(
      input$type,
      {
        if (input$type == "new") {
          # Add a new type using a modal dialog
          showModal(modalDialog(
            title = "Add new type",
            textInput(ns("new_type"), "Type"),
            textInput(ns("new_type_desc"), "Description"),
            actionButton(ns("add_new_type"), "Add new type")
          ))
        }
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )
    observeEvent(
      input$add_new_type,
      {
        # Ensure that a type and description are entered
        if (input$new_type == "") {
          alert(
            title = "Error",
            text = "Please enter a type, description optional.",
            type = "error",
            timer = 4000
          )
          return()
        }
        DBI::dbExecute(
          session$userData$AquaCache,
          paste0(
            "INSERT INTO instrument_type (type, description) VALUES ('",
            input$new_type,
            "', '",
            input$new_type_desc,
            "')"
          )
        )
        instruments_data$types <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT * FROM instrument_type"
        )
        select_data$types <- setNames(
          c(instruments_data$types$type_id, "new"),
          c(instruments_data$types$type, "Add new type")
        )
        updateSelectizeInput(
          session,
          "type",
          choices = select_data$types,
          selected = max(instruments_data$types$type_id)
        )
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
        if (input$new_sensor_desc == "") {
          DBI::dbExecute(
            session$userData$AquaCache,
            paste0(
              "INSERT INTO sensor_types (sensor_type) VALUES ('",
              input$new_sensor_type,
              "')"
            )
          )
        } else {
          DBI::dbExecute(
            session$userData$AquaCache,
            paste0(
              "INSERT INTO sensor_types (sensor_type, sensor_type_description) VALUES ('",
              input$new_sensor_type,
              "', '",
              input$new_sensor_desc,
              "')"
            )
          )
        }
        sensors_data$sensor_types <- DBI::dbGetQuery(
          session$userData$AquaCache,
          paste0("SELECT * FROM sensor_types")
        )
        tmp <- setNames(
          c(sensors_data$sensor_types$sensor_type_id, "new"),
          c(sensors_data$sensor_types$sensor_type, "Add new type")
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

    # Show/hide post-cal fields for each calibration type ################################################
    observeEvent(
      input$show_post_ph,
      {
        if ((input$show_post_ph %% 2) == 0) {
          shinyjs::hide("ph1_post_val")
          shinyjs::hide("ph2_post_val")
          shinyjs::hide("ph3_post_val")
          updateActionButton(
            session,
            "show_post_ph",
            label = "Show post-cal fields"
          )
        } else {
          shinyjs::show("ph1_post_val")
          shinyjs::show("ph2_post_val")
          shinyjs::show("ph3_post_val")
          updateActionButton(
            session,
            "show_post_ph",
            label = "Hide post-cal fields"
          )
        }
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$show_post_orp,
      {
        if ((input$show_post_orp %% 2) == 0) {
          shinyjs::hide("orp_post_mv")
          updateActionButton(
            session,
            "show_post_orp",
            label = "Show post-cal fields"
          )
        } else {
          shinyjs::show("orp_post_mv")
          updateActionButton(
            session,
            "show_post_orp",
            label = "Hide post-cal fields"
          )
        }
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$show_post_turb,
      {
        if ((input$show_post_turb %% 2) == 0) {
          shinyjs::hide("turb1_post")
          shinyjs::hide("turb2_post")
          updateActionButton(
            session,
            "show_post_turb",
            label = "Show post-cal fields"
          )
        } else {
          shinyjs::show("turb1_post")
          shinyjs::show("turb2_post")
          updateActionButton(
            session,
            "show_post_turb",
            label = "Hide post-cal fields"
          )
        }
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$show_post_spc,
      {
        if ((input$show_post_spc %% 2) == 0) {
          shinyjs::hide("spc1_post")
          shinyjs::hide("spc2_post")
          updateActionButton(
            session,
            "show_post_spc",
            label = "Show post-cal fields"
          )
        } else {
          shinyjs::show("spc1_post")
          shinyjs::show("spc2_post")
          updateActionButton(
            session,
            "show_post_spc",
            label = "Hide post-cal fields"
          )
        }
      },
      ignoreInit = TRUE
    )

    # Render messages and notes, show/hide messages based on selection ################################################
    # Initiate data.frame to populate with saved calibrations later
    send_table$saved <- data.frame(
      "Saved calibrations" = "Nothing saved yet",
      check.names = FALSE
    ) #Title is modified later for clarity if user want to restart a cal
    send_table$restarted_cal <- data.frame(
      "Saved calibrations (recovered session)" = "",
      check.names = FALSE
    )
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

    messages$instrument_reminder <- "Add your instrument if not listed via the 'Add/modify instruments' tab"
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
          title = "Stop! You must save basic info first or load an incomplete calibration!",
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
        input$selection == "pH calibration" & input$tab_panel == "Calibrate"
      ) {
        shinyjs::show("ph_mV_note")
      } else {
        shinyjs::hide("ph_mV_note")
      }
      if (
        input$selection == "ORP calibration" & input$tab_panel == "Calibrate"
      ) {
        shinyjs::show("ORP_molarity_note")
      } else {
        shinyjs::hide("ORP_molarity_note")
      }
    })

    # Create reset functions for each calibration type ################################################
    reset_basic <- function(keep_observer = FALSE) {
      if (!keep_observer) {
        output$observer <- renderUI({
          selectizeInput(
            ns("observer"),
            label = "Calibrator name",
            choices = select_data$recorder
          )
        })
      }
      shinyWidgets::updateAirDateInput(
        session,
        "obs_datetime",
        value = .POSIXct(Sys.time(), tz = "MST")
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
        label = "pH 4 Pre-Cal Value",
        value = NA
      )
      updateNumericInput(
        session,
        "ph2_pre_val",
        label = "pH 7 Pre-Cal Value",
        value = NA
      )
      updateNumericInput(
        session,
        "ph3_pre_val",
        label = "pH 10 Pre-Cal Value",
        value = NA
      )
      updateNumericInput(
        session,
        "ph1_post_val",
        label = "pH 4 Post-Cal Value",
        value = 4
      )
      updateNumericInput(session, "ph1_mv", label = "pH 4 mV", value = NA)
      updateNumericInput(
        session,
        "ph2_post_val",
        label = "pH 7 Post-Cal Value",
        value = 7
      )
      updateNumericInput(session, "ph2_mv", label = "pH 7 mV", value = NA)
      updateNumericInput(
        session,
        "ph3_post_val",
        label = "pH 10 Post-Cal Value",
        value = 10
      )
      updateNumericInput(session, "ph3_mv", label = "pH 10 mV", value = NA)
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
      shinyjs::hide("delete_temp")
    }
    reset_orp <- function() {
      updateNumericInput(
        session,
        "orp_std",
        label = "ORP Standard solution mV",
        value = NA
      )
      updateNumericInput(
        session,
        "orp_pre_mv",
        label = "ORP mV Pre-Cal Value",
        value = NA
      )
      updateNumericInput(
        session,
        "orp_post_mv",
        label = "ORP mV Post-Cal Value",
        value = NA
      )
      shinyjs::hide("delete_orp")
    }
    reset_spc <- function() {
      updateNumericInput(
        session,
        "spc1_std",
        label = "SpC Low-Range Standard",
        value = "0"
      )
      updateNumericInput(
        session,
        "spc1_pre",
        label = "SpC Low-Range Pre-Cal Value",
        value = NA
      )
      updateNumericInput(
        session,
        "spc1_post",
        label = "SpC Low-Range Post-Cal Value",
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
        label = "SpC High-Range Pre-Cal Value",
        value = NA
      )
      updateNumericInput(
        session,
        "spc2_post",
        label = "SpC High-Range Post-Cal Value",
        value = "1413"
      )
      shinyjs::hide("delete_spc")
    }
    reset_turb <- function() {
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
        label = "Low Turb Pre-cal Value",
        value = NA
      )
      updateNumericInput(
        session,
        "turb2_pre",
        label = "High Turb Pre-cal Value",
        value = NA
      )
      updateNumericInput(
        session,
        "turb1_post",
        label = "Low Turb Post-cal Value",
        value = "0"
      )
      updateNumericInput(
        session,
        "turb2_post",
        label = "High Turb Post-cal Value",
        value = "124"
      )
      shinyjs::hide("delete_turb")
    }
    reset_do <- function() {
      updateNumericInput(
        session,
        "baro_press_pre",
        label = "Baro Pressure Pre-Cal",
        value = NA
      )
      updateNumericInput(
        session,
        "baro_press_post",
        label = "Baro Pressure Post-Cal",
        value = NA
      )
      updateNumericInput(
        session,
        "do_pre_prct",
        label = "DO Pre-Cal %",
        value = NA
      )
      updateNumericInput(
        session,
        "do_post_prct",
        label = "DO Post-Cal %",
        value = NA
      )
      updateNumericInput(
        session,
        "do_pre",
        label = "DO Pre-Cal mg/L",
        value = NA
      )
      updateNumericInput(
        session,
        "do_post",
        label = "DO Post-Cal mg/L",
        value = NA
      )
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
    click_count_maintain_select <- reactiveValues(value = 0)
    observeEvent(input$calibration_instruments_table_rows_selected, {
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
      if (click_count_maintain_select$value <= 2) {
        if (initial_instr_table$value) {
          output$ID_sensor_holder <- renderUI({
            div(
              selectizeInput(
                ns("ID_sensor_holder"),
                label = "Logger/bulkhead/sonde serial #",
                choices = c("", instruments_data$others$serial_no),
                selected = initial_manage_instruments_table[
                  input$calibration_instruments_table_rows_selected[1],
                  "serial_no"
                ]
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
                selected = initial_manage_instruments_table[
                  input$calibration_instruments_table_rows_selected[2],
                  "serial_no"
                ]
              ),
              style = "color: white; background-color: green;"
            )
          })
        } else {
          output$ID_sensor_holder <- renderUI({
            div(
              selectizeInput(
                ns("ID_sensor_holder"),
                label = "Logger/bulkhead/sonde serial #",
                choices = c("", instruments_data$others$serial_no),
                selected = instruments_data$manage_instruments[
                  input$calibration_instruments_table_rows_selected[1],
                  "serial_no"
                ]
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
                selected = instruments_data$manage_instruments[
                  input$calibration_instruments_table_rows_selected[2],
                  "serial_no"
                ]
              ),
              style = "color: white; background-color: green;"
            )
          })
        }
        click_count_maintain_select$value <- click_count_maintain_select$value +
          1
      } else {
        if (initial_instr_table$value) {
          output$ID_sensor_holder <- renderUI({
            div(
              selectizeInput(
                ns("ID_sensor_holder"),
                label = "Logger/bulkhead/sonde serial #",
                choices = c("", instruments_data$others$serial_no),
                selected = initial_manage_instruments_table[
                  input$calibration_instruments_table_rows_selected[1],
                  "serial_no"
                ]
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
                selected = initial_manage_instruments_table[
                  input$calibration_instruments_table_rows_selected[2],
                  "serial_no"
                ]
              ),
              style = "color: white; background-color: green;"
            )
          })
        } else {
          output$ID_sensor_holder <- renderUI({
            div(
              selectizeInput(
                ns("ID_sensor_holder"),
                label = "Logger/bulkhead/sonde serial #",
                choices = c("", instruments_data$others$serial_no),
                selected = instruments_data$manage_instruments[
                  input$calibration_instruments_table_rows_selected[1],
                  "serial_no"
                ]
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
                selected = instruments_data$manage_instruments[
                  input$calibration_instruments_table_rows_selected[2],
                  "serial_no"
                ]
              ),
              style = "color: white; background-color: green;"
            )
          })
        }
      }
      if (
        length(input$calibration_instruments_table_rows_selected[
          input$calibration_instruments_table_rows_selected != 0
        ]) >
          2
      ) {
        selection <- NULL
        proxy <- DT::dataTableProxy("my_table")
        DT::selectRows(proxy, selection, selected = FALSE)
        if (initial_instr_table$value) {
          output$calibration_instruments_table <- DT::renderDT(
            {
              DT::datatable(
                initial_calibrate_instruments_table,
                rownames = FALSE,
                filter = 'top',
                selection = "multiple",
                callback = htmlwidgets::JS(table_reset)
              )
            },
            server = TRUE
          )
        } else {
          output$calibration_instruments_table <- DT::renderDT(
            {
              DT::datatable(
                instruments_data$calibrate_instruments,
                rownames = FALSE,
                filter = 'top',
                selection = "multiple",
                callback = htmlwidgets::JS(table_reset)
              )
            },
            server = TRUE
          )
        }
      }
    })

    ## Incomplete calibrations selection table
    observeEvent(input$incomplete_table_rows_selected, {
      reset_value <- complete$incomplete[
        input$incomplete_table_rows_selected[1],
        "Index"
      ]
      updateNumericInput(session, "restart_index", value = reset_value)
    })

    ## Instrument management table
    observeEvent(input$manage_instruments_table_rows_selected, {
      updateSelectizeInput(
        session,
        "existing_serial_no",
        selected = instruments_data$manage_instruments[
          input$manage_instruments_table_rows_selected[1],
          "serial_no"
        ]
      )
      # Everything else on the page is updated based on the serial number selected
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
        updateNumericInput(
          session,
          "ph1_pre_val",
          label = paste0("pH ", input$ph1_std, " Pre-Cal Value")
        )
        updateNumericInput(
          session,
          "ph1_mv",
          label = paste0("pH ", input$ph1_std, " mV")
        )
        updateNumericInput(
          session,
          "ph1_post_val",
          label = paste0("pH ", input$ph1_std, " Post-Cal Value"),
          value = input$ph1_std
        )
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$ph2_std,
      {
        updateNumericInput(
          session,
          "ph2_pre_val",
          label = paste0("pH ", input$ph2_std, " Pre-Cal Value")
        )
        updateNumericInput(
          session,
          "ph2_mv",
          label = paste0("pH ", input$ph2_std, " mV")
        )
        updateNumericInput(
          session,
          "ph2_post_val",
          label = paste0("pH ", input$ph2_std, " Post-Cal Value"),
          value = input$ph2_std
        )
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$ph3_std,
      {
        updateNumericInput(
          session,
          "ph3_pre_val",
          label = paste0("pH ", input$ph3_std, " Pre-Cal Value")
        )
        updateNumericInput(
          session,
          "ph3_mv",
          label = paste0("pH ", input$ph3_std, " mV")
        )
        updateNumericInput(
          session,
          "ph3_post_val",
          label = paste0("pH ", input$ph3_std, " Post-Cal Value"),
          value = input$ph3_std
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
      input$spc1_std,
      {
        updateNumericInput(session, "spc1_post", value = input$spc1_std)
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$spc2_std,
      {
        updateNumericInput(session, "spc2_post", value = input$spc2_std)
      },
      ignoreInit = TRUE
    )

    #observeEvents for when the user selects a particular page ########################################
    observeEvent(input$tab_panel, {
      if (input$tab_panel == "Calibrate") {
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
            choices = select_data$recorder
          )
        })
      } else if (input$tab_panel == "Add/modify instruments") {
        shinyjs::hide("submit_btn")
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
        shinyjs::hide("date_retired")
        shinyjs::show("retired_by")
        output$retired_by <- renderUI({
          selectizeInput(
            ns("retired_by"),
            label = "Retired by",
            choices = select_data$recorder,
            options = list(
              onInitialize = I('function() { this.setValue(""); }')
            )
          )
        })
        updateSelectizeInput(
          session,
          "existing_serial_no",
          choices = c("New record", instruments_data$sheet$serial_no),
          selected = ""
        )
        updateSelectizeInput(
          session,
          "make",
          choices = select_data$makes,
          selected = ""
        )
        updateSelectizeInput(
          session,
          "model",
          choices = select_data$models,
          selected = ""
        )
        updateSelectizeInput(
          session,
          "type",
          choices = select_data$types,
          selected = ""
        )
        updateSelectizeInput(
          session,
          "instrument_owner",
          choices = select_data$organizations,
          selected = NULL
        )
        updateCheckboxInput(session, "replaceableSensors", value = FALSE)
        updateDateInput(session, "date_retired", value = NA) #Reset the retired date to NA
        updateDateInput(session, "date_in_service", value = NA)
        updateDateInput(session, "date_purchased", value = NA)
        instruments_data$manage_instruments <- instruments_data$sheet[,
          !colnames(instruments_data$sheet) %in%
            c("instrument_id", "observer", "obs_datetime")
        ]
        output$manage_instruments_table <- DT::renderDT(
          instruments_data$manage_instruments,
          rownames = FALSE,
          selection = "single"
        )
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
              "holds_replaceable_sensors"
            )
        ]
        temp_maintain <- instruments_data$maintain_instruments[, c(
          "make",
          "model",
          "type",
          "serial_no",
          "owner"
        )]
        output$maintain_instr_table <- DT::renderDT(
          temp_maintain,
          rownames = FALSE,
          selection = "single"
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
        instruments_data$sheet <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT i.instrument_id, i.obs_datetime, CONCAT(observers.observer_first, ' ', observers.observer_last, '(', observers.organization, ')') AS observer, i.holds_replaceable_sensors, i.serial_no, i.asset_tag, i.date_in_service, i.date_purchased, i.retired_by, i.date_retired, instrument_make.make, instrument_model.model, instrument_type.type, i.owner FROM instruments AS i LEFT JOIN instrument_make ON i.make = instrument_make.make_id LEFT JOIN instrument_model ON i.model = instrument_model.model_id LEFT JOIN instrument_type ON i.type = instrument_type.type_id LEFT JOIN observers ON i.observer = observers.observer_id ORDER BY i.instrument_id"
        )
        instruments_data$maintainable_sensors <- instruments_data$sheet[
          instruments_data$sheet$holds_replaceable_sensors,
        ]
        temp_table <- instruments_data$maintainable_sensors[, c(
          "make",
          "model",
          "type",
          "serial_no",
          "owner"
        )]
        output$manage_sensors_table <- DT::renderDT(
          temp_table,
          rownames = FALSE,
          selection = "single"
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
        updateNumericInput(
          session,
          "restart_index",
          min = 0,
          max = nrow(calibrations$incomplete_calibrations)
        )
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
          sensors_data$datetime_exists <- if (
            max(sensors_data$instrument$obs_datetime) >
              (sensors_data$datetime - 5) |
              max(sensors_data$instrument$obs_datetime) >
                (sensors_data$datetime + 5)
          ) {
            TRUE
          } else {
            FALSE
          }
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

      sensors_data$datetime_exists <- if (
        max(sensors_data$instrument$obs_datetime) >
          (sensors_data$datetime - 5) |
          max(sensors_data$instrument$obs_datetime) >
            (sensors_data$datetime + 5)
      ) {
        TRUE
      } else {
        FALSE
      }

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
        if (
          nchar(input$sensor_change_name) <= 1 |
            nchar(input$add_comment) < 5 |
            nchar(input$add_sensor_serial) < 2
        ) {
          alert(
            "Please fill in all fields!",
            "You need a few more characters if you've already written something. Come on, make us a useful note!",
            type = "error",
            timer = 3000
          )
        } else {
          #add the data to the array_maintenance_changes table
          # Check if the datetime exists in the instrument table, which means we're editing an entry from this same session
          sensors_data$datetime_exists <- if (
            max(sensors_data$instrument$obs_datetime) >
              (sensors_data$datetime - 5) |
              max(sensors_data$instrument$obs_datetime) >
                (sensors_data$datetime + 5)
          ) {
            TRUE
          } else {
            FALSE
          }

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
                " = '",
                input$add_comment,
                "', WHERE obs_datetime = '",
                sensors_data$datetime,
                "' AND instrument_id = ",
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

    # Make a new instrument record or modify an existing one ##############################################
    observeEvent(
      input$existing_serial_no,
      {
        #populate fields as required
        if (
          input$existing_serial_no != "New record" &
            input$existing_serial_no != ""
        ) {
          modify_record <- instruments_data$sheet[
            instruments_data$sheet$serial_no == input$existing_serial_no,
          ]
          updateTextInput(session, "serial_no", value = modify_record$serial_no)

          recorder <- select_data$recorder[
            names(select_data$recorder) == modify_record$observer
          ]
          output$recorder <- renderUI({
            selectizeInput(
              ns("recorder"),
              label = "Observer name",
              choices = select_data$recorder,
              selected = recorder
            )
          })
          make <- instruments_data$makes[
            instruments_data$makes$make == modify_record$make,
            "make_id"
          ]
          updateSelectizeInput(session, "make", selected = make)
          model <- instruments_data$models[
            instruments_data$models$model == modify_record$model,
            "model_id"
          ]
          updateSelectizeInput(session, "model", selected = model)
          type <- instruments_data$types[
            instruments_data$types$type == modify_record$type,
            "type_id"
          ]
          updateSelectizeInput(session, "type", selected = type)
          updateTextInput(session, "asset_tag", value = modify_record$asset_tag)
          updateCheckboxInput(
            session,
            "replaceableSensors",
            value = modify_record$holds_replaceable_sensors
          )
          updateSelectizeInput(
            session,
            "instrument_owner",
            selected = modify_record$owner
          )
          updateDateInput(
            session,
            "date_in_service",
            value = modify_record$date_in_service
          )
          updateDateInput(
            session,
            "date_purchased",
            value = modify_record$date_purchased
          )
          updateDateInput(
            session,
            "date_retired",
            value = modify_record$date_retired
          )
          if (is.na(modify_record$retired_by)) {
            output$retired_by <- renderUI({
              selectizeInput(
                ns("retired_by"),
                label = "Retired by",
                choices = select_data$recorder,
                options = list(
                  onInitialize = I('function() { this.setValue(""); }')
                )
              )
            })
          } else {
            output$retired_by <- renderUI({
              selectizeInput(
                ns("retired_by"),
                label = "Retired by",
                choices = select_data$recorder,
                selected = modify_record$retired_by
              )
            })
          }

          updateActionButton(session, "save_cal_instrument", "Save edits")
          shinyjs::show("serial_no")
          shinyjs::show("date_retired")
          shinyjs::show("retired_by")
        } else if (input$existing_serial_no == "New record") {
          updateTextInput(session, "serial_no", value = "")
          output$recorder <- renderUI({
            selectizeInput(
              ns("recorder"),
              label = "Observer name",
              choices = select_data$recorder,
              selected = ""
            )
          })
          updateSelectizeInput(session, "make", selected = "")
          updateSelectInput(session, "model", selected = "")
          updateSelectizeInput(session, "type", selected = "")
          updateTextInput(session, "asset_tag", value = "")
          updateCheckboxInput(session, "replaceableSensors", value = FALSE)
          output$retired_by <- renderUI({
            selectizeInput(
              ns("retired_by"),
              label = "Retired by",
              choices = select_data$recorder,
              options = list(
                onInitialize = I('function() { this.setValue(""); }')
              )
            )
          })
          updateDateInput(session, "date_retired", value = NA)
          updateActionButton(
            session,
            "save_cal_instrument",
            "Save new instrument"
          )
          shinyjs::show("serial_no")
          shinyjs::hide("date_retired")
          shinyjs::hide("retired_by")
        }
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$save_cal_instrument,
      {
        #save the new record or the changes to existing record
        #reload instruments_data$sheet to mitigate conflicts
        instruments_data$sheet <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT i.instrument_id, i.obs_datetime,  CONCAT(observers.observer_first, ' ', observers.observer_last, '(', observers.organization, ')') AS observer, i.holds_replaceable_sensors, i.serial_no, i.asset_tag, i.date_in_service, i.date_purchased, i.retired_by, i.date_retired, instrument_make.make, instrument_model.model, instrument_type.type, i.owner FROM instruments AS i LEFT JOIN instrument_make ON i.make = instrument_make.make_id LEFT JOIN instrument_model ON i.model = instrument_model.model_id LEFT JOIN instrument_type ON i.type = instrument_type.type_id LEFT JOIN observers ON i.observer = observers.observer_id ORDER BY i.instrument_id"
        )

        if (nrow(instruments_data$sheet) == 0) {
          #if there are no instruments listed yet...
          check <- FALSE #definitely does not exist yet
        } else {
          check <- input$serial_no %in% instruments_data$sheet$serial_no #check to make sure the serial no does not already exist
        }
        if (input$existing_serial_no == "New record") {
          #add a new row with the next instrument_id
          if (check) {
            alert(
              "Serial number already exists!",
              text = "You selected 'New record' and then entered an existing serial number.",
              type = "error",
              timer = 3000
            )
            new_entry <- FALSE
          } else if (
            nchar(input$recorder) < 1 |
              nchar(input$make) < 1 |
              nchar(input$model) < 1 |
              nchar(input$type) < 1 |
              (nchar(input$serial_no) < 4 |
                (grepl("Search", input$serial_no, ignore.case = TRUE)))
          ) {
            #long statement to check if all entries are satisfactorily filled in.
            alert(
              "Unfilled mandatory entries!",
              text = "You need to provide your name, instrument make, model, type, and serial number of minimum 2 characters.",
              type = "error",
              timer = 3000
            )
            new_entry <- FALSE
          } else {
            #Make a new entry
            instrument.df <- data.frame(
              observer = input$recorder,
              obs_datetime = as.character(.POSIXct(Sys.time(), tz = "UTC")),
              make = input$make,
              model = input$model,
              type = input$type,
              holds_replaceable_sensors = input$replaceableSensors,
              serial_no = gsub("[^[:alnum:]]", "", input$serial_no),
              asset_tag = gsub("[^[:alnum:]]", "", input$asset_tag),
              date_in_service = if (length(input$date_in_service) < 1) {
                NA
              } else {
                as.character(input$date_in_service)
              },
              date_purchased = if (length(input$date_purchased) < 1) {
                NA
              } else {
                as.character(input$date_purchased)
              },
              owner = input$instrument_owner,
              retired_by = input$retired_by,
              date_retired = if (length(input$date_retired) < 1) {
                NA
              } else {
                as.character(input$date_retired)
              }
            )
            DBI::dbAppendTable(
              session$userData$AquaCache,
              "instruments",
              instrument.df
            )

            alert(
              paste0("Serial number ", input$serial_no, " added"),
              type = "success",
              timer = 2000
            )
          }
        } else {
          #Modify an existing entry
          if (
            nchar(input$recorder) < 1 |
              nchar(input$make) < 1 |
              nchar(input$model) < 1 |
              nchar(input$type) < 1 |
              (nchar(input$serial_no) < 4 |
                nchar(input$instrument_owner) < 1 |
                (grepl("Search", input$serial_no, ignore.case = TRUE)))
          ) {
            #long statement to check if all entries are satisfactorily filled in.
            alert(
              "Unfilled mandatory entries!",
              text = "You need to provide your name, instrument make, model, type, and serial number of minimum 2 characters.",
              type = "error",
              timer = 4000
            )
          } else {
            DBI::dbExecute(
              session$userData$AquaCache,
              paste0(
                "UPDATE instruments SET observer = ",
                input$recorder,
                ", obs_datetime = '",
                as.character(.POSIXct(Sys.time(), tz = "UTC")),
                "', make = ",
                input$make,
                ", model = ",
                input$model,
                ", type = ",
                input$type,
                ", owner = '",
                input$instrument_owner,
                "', holds_replaceable_sensors = ",
                input$replaceableSensors,
                " WHERE serial_no = '",
                input$existing_serial_no,
                "'"
              )
            )

            if (nchar(input$asset_tag) > 0) {
              DBI::dbExecute(
                session$userData$AquaCache,
                paste0(
                  "UPDATE instruments SET asset_tag = '",
                  gsub("[^[:alnum:]]", "", input$asset_tag),
                  "' WHERE serial_no = '",
                  input$existing_serial_no,
                  "'"
                )
              )
            }
            if (length(input$date_in_service) > 0) {
              DBI::dbExecute(
                session$userData$AquaCache,
                paste0(
                  "UPDATE instruments SET date_in_service = '",
                  input$date_in_service,
                  "' WHERE serial_no = '",
                  input$existing_serial_no,
                  "'"
                )
              )
            }
            if (length(input$date_purchased) > 0) {
              DBI::dbExecute(
                session$userData$AquaCache,
                paste0(
                  "UPDATE instruments SET date_purchased = '",
                  input$date_purchased,
                  "' WHERE serial_no = '",
                  input$existing_serial_no,
                  "'"
                )
              )
            }
            if (nchar(input$retired_by) > 0) {
              DBI::dbExecute(
                session$userData$AquaCache,
                paste0(
                  "UPDATE instruments SET retired_by = '",
                  input$retired_by,
                  "' WHERE serial_no = '",
                  input$existing_serial_no,
                  "'"
                )
              )
            }
            if (length(input$date_retired) > 0) {
              DBI::dbExecute(
                session$userData$AquaCache,
                paste0(
                  "UPDATE instruments SET date_retired = '",
                  input$date_retired,
                  "' WHERE serial_no = '",
                  input$existing_serial_no,
                  "'"
                )
              )
            }

            alert(
              paste0("Serial number ", input$existing_serial_no, " modified"),
              type = "success",
              timer = 2000
            )
            new_entry <- TRUE
          }
        }

        #Reload the instruments sheet to integrate modifications
        instruments_sheet <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT i.instrument_id, i.obs_datetime,  CONCAT(observers.observer_first, ' ', observers.observer_last, '(', observers.organization, ')') AS observer, i.holds_replaceable_sensors, i.serial_no, i.asset_tag, i.date_in_service, i.date_purchased, i.retired_by, i.date_retired, instrument_make.make, instrument_model.model, instrument_type.type, i.owner FROM instruments AS i LEFT JOIN instrument_make ON i.make = instrument_make.make_id LEFT JOIN instrument_model ON i.model = instrument_model.model_id LEFT JOIN instrument_type ON i.type = instrument_type.type_id LEFT JOIN observers ON i.observer = observers.observer_id ORDER BY i.instrument_id"
        )
        instruments_data$sheet <- instruments_sheet
        instruments_data$handhelds <- instruments_sheet[
          instruments_sheet$type == "Handheld" &
            is.na(instruments_sheet$date_retired),
        ]
        instruments_data$others <- instruments_sheet[
          instruments_sheet$type != "Handheld" &
            is.na(instruments_sheet$date_retired),
        ]
        instruments_data$maintainable_sensors <- instruments_sheet[
          instruments_sheet$holds_replaceable_sensors,
        ]

        #Reset some fields, show/hide others
        updateSelectizeInput(
          session,
          "existing_serial_no",
          choices = c("New record", instruments_data$sheet$serial_no),
          selected = input$existing_serial_no
        )
        updateTextInput(session, "serial_no", value = "")
        output$observer <- renderUI({
          selectizeInput(
            ns("observer"),
            label = "Calibrator name",
            choices = select_data$recorder
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
        shinyjs::hide("serial_no")
        shinyjs::show("date_retired")
        shinyjs::show("retired_by")
        updateActionButton(session, "save_cal_instrument", "Save edits")

        #Re-render the tables
        instruments_data$manage_instruments <- instruments_data$sheet[,
          !colnames(instruments_data$sheet) %in%
            c("instrument_id", "observer", "obs_datetime")
        ]
        output$manage_instruments_table <- DT::renderDT(
          instruments_data$manage_instruments,
          rownames = FALSE,
          selection = "single"
        )
        temp_table <- instruments_data$maintainable_sensors[, c(
          "make",
          "model",
          "type",
          "serial_no",
          "owner"
        )]
        temp_table$type <- gsub(" .*", "", temp_table$type)
        output$manage_sensors_table <- DT::renderDT(
          temp_table,
          rownames = FALSE,
          selection = "single"
        )

        instruments_data$calibrate_instruments <- instruments_data$sheet[,
          !colnames(instruments_data$sheet) %in%
            c(
              "instrument_id",
              "observer",
              "obs_datetime",
              "retired_by",
              "date_retired",
              "date_purchased",
              "date_in_service"
            )
        ]
        output$calibration_instruments_table <- DT::renderDT(
          {
            DT::datatable(
              instruments_data$calibrate_instruments,
              rownames = FALSE,
              selection = "multiple",
              callback = htmlwidgets::JS(table_reset)
            )
          },
          server = TRUE
        )
      },
      ignoreInit = TRUE
    )

    # Restart a calibration ##############################################################################
    observeEvent(
      input$restart_calibration,
      {
        restart_value <- as.numeric(input$restart_index)
        if (restart_value == 0) {
          alert("0 is not a valid selection!", type = "error", timer = 3000)
        } else if (!(restart_value %in% complete$incomplete$Index)) {
          alert(
            "The number you entered does not correspond to a row/index number",
            type = "error",
            timer = 3000
          )
        } else {
          shinyjs::show("restart_table")
          send_table$restarted_cal <- data.frame(
            "Saved calibrations (recovered session)" = "Basic info",
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
                output_name <- "Temperature calibration"
                complete$temperature <- TRUE
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
                output_name <- "Conductivity calibration"
                complete$spc <- TRUE
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
                shinyjs::show("delete_spc")
              } else if (i == "calibrate_ph") {
                output_name <- "pH calibration"
                complete$ph <- TRUE
                updateNumericInput(session, "ph1_std", value = sheet$ph1_std)
                updateNumericInput(session, "ph2_std", value = sheet$ph2_std)
                updateNumericInput(session, "ph3_std", value = sheet$ph3_std)
                updateNumericInput(
                  session,
                  "ph1_pre_val",
                  label = paste0("pH ", sheet$ph1_std, " Pre-Cal Value"),
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
                  label = paste0("pH ", sheet$ph2_std, " Pre-Cal Value"),
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
                  label = paste0("pH ", sheet$ph3_std, " Pre-Cal Value"),
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
                  label = paste0("pH ", sheet$ph1_std, " Post-Cal Value"),
                  value = sheet$ph1_post_val
                )
                updateNumericInput(
                  session,
                  "ph2_post_val",
                  label = paste0("pH ", sheet$ph2_std, " Post-Cal Value"),
                  value = sheet$ph2_post_val
                )
                updateNumericInput(
                  session,
                  "ph3_post_val",
                  label = paste0("pH ", sheet$ph3_std, " Post-Cal Value"),
                  value = sheet$ph3_post_val
                )
                shinyjs::show("delete_ph")
              } else if (i == "calibrate_orp") {
                output_name <- "ORP calibration"
                complete$orp <- TRUE
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
                shinyjs::show("delete_orp")
              } else if (i == "calibrate_turbidity") {
                output_name <- "Turbidity calibration"
                complete$turbidity <- TRUE
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
                shinyjs::show("delete_turb")
              } else if (i == "calibrate_dissolved_oxygen") {
                output_name <- "DO calibration"
                complete$do <- TRUE
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
                shinyjs::show("delete_do")
              } else if (i == "calibrate_depth") {
                output_name <- "Depth calibration"
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
            value = calibrations$incomplete_calibrations[
              restart_value,
              "obs_datetime"
            ] -
              7 * 60 * 60
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
          colnames(send_table$saved) <- "Saved calibrations (this session)" #Update the name for clarity since we're restarting a calibration
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
          updateTabsetPanel(session, "tab_panel", selected = "Calibrate") # Changing this selection brings the user right to the calibration page
        }
      },
      ignoreInit = TRUE
    )

    # Delete a calibration ##############################################################################
    observeEvent(
      input$delete_calibration,
      {
        delete_value <- as.numeric(input$restart_index)
        if (delete_value == 0) {
          alert("0 is not a valid selection!", type = "error", timer = 2000)
        } else if (!(delete_value %in% complete$incomplete$Index)) {
          alert(
            "The number you entered does not correspond to a row/index number",
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
            "Index" = seq_len(nrow(calibrations$incomplete_calibrations)),
            "Calibrator" = as.vector(
              calibrations$incomplete_calibrations$observer_string
            ),
            "Date/time UTC" = calibrations$incomplete_calibrations$obs_datetime,
            "Purpose" = calibrations$incomplete_calibrations$purpose,
            check.names = FALSE
          )

          if (nrow(complete$incomplete) == 0) {
            complete$incomplete <- data.frame(
              "Index" = 0,
              "Calibrator" = "No unsaved calibrations!",
              "Date/time UTC" = "No unsaved calibrations!",
              "Purpose" = "No unsaved calibrations!",
              check.names = FALSE
            )
          }
          output$incomplete_table <- DT::renderDT(
            complete$incomplete,
            rownames = FALSE,
            selection = "single"
          )
          updateNumericInput(
            session,
            "restart_index",
            min = 0,
            max = nrow(calibrations$incomplete_calibrations),
            value = 0
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
        if (input$spc_or_not) {
          post_condy_val <- input$spc2_std /
            (1 + 0.02 * (calibration_data$temp$temp_reference - 25))
          updateNumericInput(
            session,
            "spc1_pre",
            label = "Conductivity Low-Range Pre-Cal Value"
          )
          updateNumericInput(
            session,
            "spc1_post",
            label = "Conductivity Low-Range Post-Cal Value"
          )
          updateNumericInput(
            session,
            "spc2_pre",
            label = "Conductivity High-Range Pre-Cal Value"
          )
          updateNumericInput(
            session,
            "spc2_post",
            label = "Conductivity High-Range Post-Cal Value",
            value = round(post_condy_val, 0)
          )
        } else {
          updateNumericInput(
            session,
            "spc1_pre",
            label = "SpC Low-Range Pre-Cal Value"
          )
          updateNumericInput(
            session,
            "spc1_post",
            label = "SpC Low-Range Post-Cal Value"
          )
          updateNumericInput(
            session,
            "spc2_pre",
            label = "SpC High-Range Pre-Cal Value"
          )
          updateNumericInput(
            session,
            "spc2_post",
            label = "SpC High-Range Post-Cal Value",
            value = input$spc2_std
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
          if (pre_post == "pre") {
            alert(
              "Pre-calibration baro pressure out of range",
              "Enter pressure in mmHg only",
              type = "error",
              timer = 3000
            )
          } else {
            alert(
              "Post-calibration baro pressure out of range",
              "Enter pressure in mmHg only",
              type = "error",
              timer = 3000
            )
          }
        } else {
          go_baro <- TRUE
        }
      } else {
        if (pre_post == "pre") {
          alert(
            "Enter pre-cal baro pressures in mmHg first!",
            type = "error",
            timer = 3000
          )
        } else {
          alert(
            "Enter post-cal baro pressures in mmHg first!",
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
              "Temp should be between 0 and 30 degrees C; review temperature calibration",
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
            "Go to the temperature calibration",
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
        DO_calc(pre_post = "post", prct_abs = "prct", messages = FALSE)
      },
      ignoreInit = T
    )
    observeEvent(
      input$calc_prct_do,
      {
        DO_calc(pre_post = "pre", prct_abs = "abs")
        DO_calc(pre_post = "post", prct_abs = "abs", messages = FALSE)
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

        dt <- input$obs_datetime + 7 * 60 * 60
        attr(dt, "tzone") <- "UTC"

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
          if (is.na(id_handheld_meter)) {
            DBI::dbExecute(
              session$userData$AquaCache,
              paste0(
                "UPDATE calibrations SET observer = '",
                input$observer,
                "', obs_datetime = '",
                dt,
                "', id_sensor_holder = ",
                id_sensor_holder,
                " WHERE calibration_id = ",
                calibration_data$next_id,
                ";"
              )
            )
          } else {
            DBI::dbExecute(
              session$userData$AquaCache,
              paste0(
                "UPDATE calibrations SET observer = '",
                input$observer,
                "', obs_datetime = '",
                dt,
                "', id_sensor_holder = ",
                id_sensor_holder,
                ", id_handheld_meter = ",
                id_handheld_meter,
                " WHERE calibration_id = ",
                calibration_data$next_id,
                ";"
              )
            )
          }
          if (!is.na(purpose_text)) {
            DBI::dbExecute(
              session$userData$AquaCache,
              paste0(
                "UPDATE calibrations SET purpose = '",
                purpose_text,
                "' WHERE calibration_id = ",
                calibration_data$next_id,
                ";"
              )
            )
          }
        }
        if (
          "Basic info" %in%
            send_table$saved[, 1] |
            "Basic info" %in% send_table$restarted_cal[, 1]
        ) {
          alert(
            title = "Basic info overwritten",
            type = "success",
            timer = 2000
          )
        } else {
          alert(title = "Basic info saved", type = "success", timer = 2000)
        }
        if (send_table$saved[1, 1] == "Nothing saved yet") {
          send_table$saved[1, 1] <- "Basic info"
        } else if (!("Basic info" %in% send_table$saved[, 1])) {
          send_table$saved[nrow(send_table$saved) + 1, 1] <- "Basic info"
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
            value1 <- as.numeric(input$ph1_post_val)
            if (
              value1 < (std1 - 0.1) | value1 > (std1 + 0.1) | is.null(value1)
            ) {
              #tolerance of 0.1 pH units from the stated calibration standard value
              shinyjs::js$backgroundCol("ph1_post_val", "red")
              warn_ph_post <- TRUE
            } else {
              shinyjs::js$backgroundCol("ph1_post_val", "white")
            }
            value2 <- as.numeric(input$ph2_post_val)
            if (
              value2 < (std2 - 0.1) | value2 > (std2 + 0.1) | is.null(value2)
            ) {
              #tolerance of 0.1 pH units from the stated calibration standard value
              shinyjs::js$backgroundCol("ph2_post_val", "red")
              warn_ph_post <- TRUE
            } else {
              shinyjs::js$backgroundCol("ph2_post_val", "white")
            }
            value3 <- as.numeric(input$ph3_post_val)
            if (
              value3 < (std3 - 0.1) | value3 > (std3 + 0.1) | is.null(value3)
            ) {
              #tolerance of 0.1 pH units from the stated calibration standard value
              shinyjs::js$backgroundCol("ph3_post_val", "red")
              warn_ph_post <- TRUE
            } else {
              shinyjs::js$backgroundCol("ph3_post_val", "white")
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
                  "Some of your post calibration pH values are > 0.1 units from their standards! Check your inputs.<br><br>"
                } else {
                  ""
                },
                if (warn_mv_post) {
                  "Post calibration mV values are outside of the valid range; consider replacing the electrode or sensor."
                }
              )
              # Show a modal to the user to confirm that they are sure about their entries
              showModal(modalDialog(
                title = "Are you sure?",
                HTML(warnings),
                footer = tagList(
                  actionButton(ns("ok_check_ph"), "Yes, I'm sure"),
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
        removeModal()
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
            ph1_post_val = input$ph1_post_val,
            ph2_post_val = input$ph2_post_val,
            ph3_post_val = input$ph3_post_val
          )
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
                input$ph1_post_val,
                ", ph2_post_val = ",
                input$ph2_post_val,
                ", ph3_post_val = ",
                input$ph3_post_val,
                " WHERE calibration_id = ",
                calibration_data$next_id
              )
            )
          }
          if (
            "pH calibration" %in%
              send_table$saved[, 1] |
              "pH calibration" %in% send_table$restarted_cal[, 1]
          ) {
            alert(
              title = "pH calibration overwritten",
              type = "success",
              timer = 2000
            )
          } else {
            alert(
              title = "pH calibration saved",
              type = "success",
              timer = 2000
            )
          }
          if (send_table$saved[1, 1] == "Nothing saved yet") {
            send_table$saved[1, 1] <- "pH calibration"
          } else if (!("pH calibration" %in% send_table$saved[, 1])) {
            send_table$saved[nrow(send_table$saved) + 1, 1] <- "pH calibration"
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
        send_table$saved <- send_table$saved[
          send_table$saved[1] != "pH calibration",
          ,
          drop = FALSE
        ]
        send_table$restarted_cal <- send_table$restarted_cal[
          send_table$restarted_cal[1] != "pH calibration",
          ,
          drop = FALSE
        ]
        if (nrow(send_table$saved) == 0) {
          if (!restarted$restarted) {
            send_table$saved <- data.frame(
              "Saved calibrations" = "Nothing saved yet",
              check.names = FALSE
            )
          } else {
            send_table$saved <- data.frame(
              "Saved calibrations (this session)" = "Nothing saved yet",
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
                  actionButton(ns("ok_check_temp"), "Yes, I'm sure"),
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
        removeModal()
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
                "UPDATE calibrate_temperature SET temp_reference_desc = '",
                input$temp_reference_desc,
                "', temp_reference = ",
                input$temp_reference,
                ", temp_observed = ",
                input$temp_observed,
                " WHERE calibration_id = ",
                calibration_data$next_id
              )
            )
          }
          if (
            "Temperature calibration" %in%
              send_table$saved[, 1] |
              "Temperature calibration" %in% send_table$restarted_cal[, 1]
          ) {
            alert(
              title = "Temperature calibration overwritten",
              type = "success",
              timer = 2000
            )
          } else {
            alert(
              title = "Temperature calibration saved",
              type = "success",
              timer = 2000
            )
          }
          if (send_table$saved[1, 1] == "Nothing saved yet") {
            send_table$saved[1, 1] <- "Temperature calibration"
          } else if (!("Temperature calibration" %in% send_table$saved[, 1])) {
            send_table$saved[
              nrow(send_table$saved) + 1,
              1
            ] <- "Temperature calibration"
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
        send_table$saved <- send_table$saved[
          send_table$saved[1] != "Temperature calibration",
          ,
          drop = FALSE
        ] #drop = FALSE is necessary to return a table and not a vector, which is default if returning only one col
        send_table$restarted_cal <- send_table$restarted_cal[
          send_table$restarted_cal[1] != "Temperature calibration",
          ,
          drop = FALSE
        ]
        if (nrow(send_table$saved) == 0) {
          if (!restarted$restarted) {
            send_table$saved <- data.frame(
              "Saved calibrations" = "Nothing saved yet",
              check.names = FALSE
            )
          } else {
            send_table$saved <- data.frame(
              "Saved calibrations (this session)" = "Nothing saved yet",
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
            orp_std <- input$orp_std
            orp_post <- input$orp_post_mv
            orp_diff <- abs(orp_std - orp_post)
            if (orp_diff > 10) {
              shinyjs::js$backgroundCol("orp_std", "red")
              shinyjs::js$backgroundCol("orp_post_mv", "red")
            } else if (orp_diff > 5) {
              shinyjs::js$backgroundCol("orp_std", "lemonchiffon")
              shinyjs::js$backgroundCol("orp_post_mv", "lemonchiffon")
            } else {
              shinyjs::js$backgroundCol("orp_std", "white")
              shinyjs::js$backgroundCol("orp_post_mv", "white")
            }
            if (orp_diff > 5) {
              # Show a modal to the user to confirm that they are sure about their entries
              showModal(modalDialog(
                title = "Are you sure?",
                "ORP difference is > 5 mV; are you sure about your entries?",
                footer = tagList(
                  actionButton(ns("ok_check_orp"), "Yes, I'm sure"),
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
        removeModal()
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
          calibration_data$orp <- data.frame(
            calibration_id = calibration_data$next_id,
            orp_std = input$orp_std,
            orp_pre_mv = input$orp_pre_mv,
            orp_post_mv = input$orp_post_mv
          )
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
                input$orp_post_mv,
                " WHERE calibration_id = ",
                calibration_data$next_id
              )
            )
          }
          if (
            "ORP calibration" %in%
              send_table$saved[, 1] |
              "ORP calibration" %in% send_table$restarted_cal[, 1]
          ) {
            alert(
              title = "ORP calibration overwritten",
              type = "success",
              timer = 2000
            )
          } else {
            alert(
              title = "ORP calibration saved",
              type = "success",
              timer = 2000
            )
          }
          if (send_table$saved[1, 1] == "Nothing saved yet") {
            send_table$saved[1, 1] <- "ORP calibration"
          } else if (!("ORP calibration" %in% send_table$saved[, 1])) {
            send_table$saved[nrow(send_table$saved) + 1, 1] <- "ORP calibration"
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
        send_table$saved <- send_table$saved[
          send_table$saved[1] != "ORP calibration",
          ,
          drop = FALSE
        ]
        send_table$restarted_cal <- send_table$restarted_cal[
          send_table$restarted_cal[1] != "ORP calibration",
          ,
          drop = FALSE
        ]
        if (nrow(send_table$saved) == 0) {
          if (!restarted$restarted) {
            send_table$saved <- data.frame(
              "Saved calibrations" = "Nothing saved yet",
              check.names = FALSE
            )
          } else {
            send_table$saved <- data.frame(
              "Saved calibrations (this session)" = "Nothing saved yet",
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
        if (input$spc_or_not & is.null(calibration_data$temp)) {
          alert(
            "Calibrate temperature first!",
            "You must calibrate temperature first if entering non-specific conductivity",
            type = "error",
            timer = 3000
          )
          updateSelectizeInput(
            session,
            "selection",
            selected = "Temperature calibration"
          )
          return()
        } else {
          if (is.na(input$spc1_post)) {
            updateNumericInput(session, "spc1_post", value = input$spc1_std)
          }
          if (is.na(input$spc2_post)) {
            updateNumericInput(session, "spc2_post", value = input$spc2_std)
          }
          if (is.na(input$spc1_pre)) {
            alert(
              "You must enter a pre-calibration value for the low-range conductivity/conductance",
              type = "error",
              timer = 3000
            )
            return()
          }
          if (is.na(input$spc2_pre)) {
            alert(
              "You must enter a pre-calibration value for the high-range conductivity/conductance",
              type = "error",
              timer = 3000
            )
            return()
          }

          tryCatch(
            {
              spc1_ref <- input$spc1_std
              spc2_ref <- input$spc2_std
              spc1_post <- if (input$spc_or_not) {
                input$spc1_post /
                  (1 + 0.02 * (calibration_data$temp$temp_reference - 25))
              } else {
                input$spc1_post
              }
              spc2_post <- if (input$spc_or_not) {
                input$spc2_post /
                  (1 + 0.02 * (calibration_data$temp$temp_reference - 25))
              } else {
                input$spc2_post
              }
              spc1_diff <- abs(spc1_ref - spc1_post)
              spc2_diff <- abs(spc2_ref - spc2_post)
              message1 <- character(0)
              message2 <- character(0)
              if (spc1_diff > 10) {
                shinyjs::js$backgroundCol("spc1_std", "red")
                shinyjs::js$backgroundCol("spc1_post", "red")
                if (input$spc_or_not) {
                  message1 <- paste0(
                    "Double check your values: your low-range input converts to an SpC of ",
                    round(spc1_post, 0),
                    " versus the expected ",
                    spc1_ref
                  )
                } else {
                  message1 <- "Warning: double check your low-range values"
                }
              } else if (spc1_diff > 5) {
                shinyjs::js$backgroundCol("spc1_std", "lemonchiffon")
                shinyjs::js$backgroundCol("spc1_post", "lemonchiffon")
                if (input$spc_or_not) {
                  message1 <- paste0(
                    "Double check your values: your low-range input converts to an SpC of ",
                    round(spc1_post, 0),
                    " versus the expected ",
                    spc1_ref
                  )
                } else {
                  message1 <- "Warning: double check your values"
                }
              } else {
                shinyjs::js$backgroundCol("spc1_std", "white")
                shinyjs::js$backgroundCol("spc1_post", "white")
              }
              if (spc2_diff > 10) {
                shinyjs::js$backgroundCol("spc2_std", "red")
                shinyjs::js$backgroundCol("spc2_post", "red")
                if (input$spc_or_not) {
                  message1 <- paste0(
                    "Double check your values: your high-range input converts to an SpC of ",
                    round(spc2_post, 0),
                    " versus the expected ",
                    spc2_ref
                  )
                } else {
                  message1 <- "Warning: double check your high-range values"
                }
              } else if (spc2_diff > 5) {
                shinyjs::js$backgroundCol("spc2_std", "lemonchiffon")
                shinyjs::js$backgroundCol("spc2_post", "lemonchiffon")
                alert(
                  title = "Warning: double check your values",
                  type = "warning",
                  timer = 2000
                )
                if (input$spc_or_not) {
                  message1 <- paste0(
                    "Double check your values: your high-range input converts to an SpC of ",
                    round(spc2_post, 0),
                    " versus the expected ",
                    spc2_ref
                  )
                } else {
                  message1 <- "Warning: double check your high-range values"
                }
              } else {
                shinyjs::js$backgroundCol("spc2_std", "white")
                shinyjs::js$backgroundCol("spc2_post", "white")
              }
              if (spc1_diff > 5 | spc2_diff > 5) {
                # Show a modal to the user to confirm that they are sure about their entries
                message <- paste0(
                  if (length(message1) > 0) {
                    paste0(message1, "<br><br>")
                  } else {
                    ""
                  },
                  message2
                )
                showModal(modalDialog(
                  title = "Are you sure?",
                  message,
                  footer = tagList(
                    actionButton(ns("ok_check_spc"), "Yes, I'm sure"),
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
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$ok_check_spc,
      {
        removeModal()
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
          calibration_data$spc <- data.frame(
            calibration_id = calibration_data$next_id,
            spc1_std = input$spc1_std,
            spc1_pre = if (input$spc_or_not) {
              input$spc1_pre /
                (1 + 0.02 * (calibration_data$temp$temp_reference - 25))
            } else {
              input$spc1_pre
            },
            spc1_post = if (input$spc_or_not) {
              input$spc1_post /
                (1 + 0.02 * (calibration_data$temp$temp_reference - 25))
            } else {
              input$spc1_post
            },
            spc2_std = input$spc2_std,
            spc2_pre = if (input$spc_or_not) {
              input$spc2_pre /
                (1 + 0.02 * (calibration_data$temp$temp_reference - 25))
            } else {
              input$spc2_pre
            },
            spc2_post = if (input$spc_or_not) {
              input$spc2_post /
                (1 + 0.02 * (calibration_data$temp$temp_reference - 25))
            } else {
              input$spc2_post
            }
          )

          if (!complete$spc) {
            tryCatch(
              {
                DBI::dbAppendTable(
                  session$userData$AquaCache,
                  "calibrate_specific_conductance",
                  calibration_data$spc
                )

                if (
                  "Conductivity calibration" %in%
                    send_table$saved[, 1] |
                    "Conductivity calibration" %in%
                      send_table$restarted_cal[, 1]
                ) {
                  alert(
                    title = "Conductivity calibration overwritten",
                    type = "success",
                    timer = 2000
                  )
                } else {
                  alert(
                    title = "Conductivity calibration saved",
                    type = "success",
                    timer = 2000
                  )
                }
                if (send_table$saved[1, 1] == "Nothing saved yet") {
                  send_table$saved[1, 1] <- "Conductivity calibration"
                } else if (
                  !("Conductivity calibration" %in% send_table$saved[, 1])
                ) {
                  send_table$saved[
                    nrow(send_table$saved) + 1,
                    1
                  ] <- "Conductivity calibration"
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
                DBI::dbExecute(
                  session$userData$AquaCache,
                  paste0(
                    "UPDATE calibrate_specific_conductance SET spc1_std = ",
                    input$spc1_std,
                    ", spc1_pre = ",
                    if (input$spc_or_not) {
                      input$spc1_pre /
                        (1 + 0.02 * (calibration_data$temp$temp_reference - 25))
                    } else {
                      input$spc1_pre
                    },
                    ", spc1_post = ",
                    if (input$spc_or_not) {
                      input$spc1_post /
                        (1 + 0.02 * (calibration_data$temp$temp_reference - 25))
                    } else {
                      input$spc1_post
                    },
                    ", spc2_std = ",
                    input$spc2_std,
                    ", spc2_pre = ",
                    if (input$spc_or_not) {
                      input$spc2_pre /
                        (1 + 0.02 * (calibration_data$temp$temp_reference - 25))
                    } else {
                      input$spc2_pre
                    },
                    ", spc2_post = ",
                    if (input$spc_or_not) {
                      input$spc2_post /
                        (1 + 0.02 * (calibration_data$temp$temp_reference - 25))
                    } else {
                      input$spc2_post
                    },
                    " WHERE calibration_id = ",
                    calibration_data$next_id
                  )
                )

                if (
                  "Conductivity calibration" %in%
                    send_table$saved[, 1] |
                    "Conductivity calibration" %in%
                      send_table$restarted_cal[, 1]
                ) {
                  alert(
                    title = "Conductivity calibration overwritten",
                    type = "success",
                    timer = 2000
                  )
                } else {
                  alert(
                    title = "Conductivity calibration saved",
                    type = "success",
                    timer = 2000
                  )
                }
                if (send_table$saved[1, 1] == "Nothing saved yet") {
                  send_table$saved[1, 1] <- "Conductivity calibration"
                } else if (
                  !("Conductivity calibration" %in% send_table$saved[, 1])
                ) {
                  send_table$saved[
                    nrow(send_table$saved) + 1,
                    1
                  ] <- "Conductivity calibration"
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
        send_table$saved <- send_table$saved[
          send_table$saved[1] != "Conductivity calibration",
          ,
          drop = FALSE
        ]
        send_table$restarted_cal <- send_table$restarted_cal[
          send_table$restarted_cal[1] != "Conductivity calibration",
          ,
          drop = FALSE
        ]
        if (nrow(send_table$saved) == 0) {
          if (!restarted$restarted) {
            send_table$saved <- data.frame(
              "Saved calibrations" = "Nothing saved yet",
              check.names = FALSE
            )
          } else {
            send_table$saved <- data.frame(
              "Saved calibrations (this session)" = "Nothing saved yet",
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
        tryCatch(
          {
            turb1_ref <- input$turb1_std
            turb2_ref <- input$turb2_std
            turb1_post <- input$turb1_post
            turb2_post <- input$turb2_post
            turb1_diff <- abs(turb1_ref - turb1_post)
            turb2_diff <- abs(turb2_ref - turb2_post)
            gtg1 <- FALSE
            gtg2 <- FALSE
            if (turb1_diff > 10) {
              shinyjs::js$backgroundCol("turb1_std", "red")
              shinyjs::js$backgroundCol("turb1_post", "red")
            } else if (turb1_diff > 5) {
              shinyjs::js$backgroundCol("turb1_std", "lemonchiffon")
              shinyjs::js$backgroundCol("turb1_post", "lemonchiffon")
            } else {
              shinyjs::js$backgroundCol("turb1_std", "white")
              shinyjs::js$backgroundCol("turb1_post", "white")
              gtg1 <- TRUE
            }
            if (turb2_diff > 10) {
              shinyjs::js$backgroundCol("turb2_std", "red")
              shinyjs::js$backgroundCol("turb2_post", "red")
            } else if (turb2_diff > 5) {
              shinyjs::js$backgroundCol("turb2_std", "lemonchiffon")
              shinyjs::js$backgroundCol("turb2_post", "lemonchiffon")
            } else {
              shinyjs::js$backgroundCol("turb2_std", "white")
              shinyjs::js$backgroundCol("turb2_post", "white")
              gtg2 <- TRUE
            }
            if (!gtg1 | !gtg2) {
              # Show a modal to the user to confirm that they are sure about their entries
              showModal(modalDialog(
                title = "Are you sure?",
                "Your post-cal values are a bit off from the standard. Please check you entries before moving on.",
                footer = tagList(
                  actionButton(ns("ok_check_turb"), "Yes, I'm sure"),
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
      removeModal()
      validation_check$turb <- TRUE
    })

    observeEvent(
      validation_check$turb,
      {
        if (!validation_check$turb) {
          return()
        } else {
          calibration_data$turb <- data.frame(
            calibration_id = calibration_data$next_id,
            turb1_std = input$turb1_std,
            turb1_pre = input$turb1_pre,
            turb1_post = input$turb1_post,
            turb2_std = input$turb2_std,
            turb2_pre = input$turb2_pre,
            turb2_post = input$turb2_post
          )
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
                input$turb1_post,
                ", turb2_std = ",
                input$turb2_std,
                ", turb2_pre = ",
                input$turb2_pre,
                ", turb2_post = ",
                input$turb2_post,
                " WHERE calibration_id = ",
                calibration_data$next_id
              )
            )
          }
          if (
            "Turbidity calibration" %in%
              send_table$saved[, 1] |
              "Turbidity calibration" %in% send_table$restarted_cal[, 1]
          ) {
            alert(
              title = "Turbidity calibration overwritten",
              type = "success",
              timer = 2000
            )
          } else {
            alert(
              title = "Turbidity calibration saved",
              type = "success",
              timer = 2000
            )
          }
          if (send_table$saved[1, 1] == "Nothing saved yet") {
            send_table$saved[1, 1] <- "Turbidity calibration"
          } else if (!("Turbidity calibration" %in% send_table$saved[, 1])) {
            send_table$saved[
              nrow(send_table$saved) + 1,
              1
            ] <- "Turbidity calibration"
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
        send_table$saved <- send_table$saved[
          send_table$saved[1] != "Turbidity calibration",
          ,
          drop = FALSE
        ]
        send_table$restarted_cal <- send_table$restarted_cal[
          send_table$restarted_cal[1] != "Turbidity calibration",
          ,
          drop = FALSE
        ]
        if (nrow(send_table$saved) == 0) {
          if (!restarted$restarted) {
            send_table$saved <- data.frame(
              "Saved calibrations" = "Nothing saved yet",
              check.names = FALSE
            )
          } else {
            send_table$saved <- data.frame(
              "Saved calibrations (this session)" = "Nothing saved yet",
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
        tryCatch(
          {
            baro_post <- input$baro_press_post
            do_post <- input$do_post
            message1 <- character(0)
            message2 <- character(0)
            if (baro_post < 600 | baro_post > 800) {
              message1 <- "Baro pressures are not in range, are you sure?"
            }
            if (do_post < 1 | do_post > 15) {
              message2 <- "DO values are not in range, are you sure you entered % and mg/l in the right boxes? Only mg/l is saved, use the Fill/recalculate button."
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
                  actionButton(ns("ok_check_DO"), "Yes, I'm sure"),
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
              "Baro pressure and DO in mg/l are mandatory",
              type = "error",
              timer = 4000
            )
          }
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(input$ok_check_do, {
      removeModal()
      validation_check$do <- TRUE
    })

    observeEvent(
      validation_check$do,
      {
        if (!validation_check$do) {
          return()
        } else {
          calibration_data$do <- data.frame(
            calibration_id = calibration_data$next_id,
            baro_press_pre = input$baro_press_pre,
            baro_press_post = input$baro_press_post,
            do_pre_mgl = input$do_pre,
            do_post_mgl = input$do_post
          )
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
                input$baro_press_post,
                ", do_pre_mgl = ",
                input$do_pre,
                ", do_post_mgl = ",
                input$do_post,
                " WHERE calibration_id = ",
                calibration_data$next_id
              )
            )
          }
          if (
            "DO calibration" %in%
              send_table$saved[, 1] |
              "DO calibration" %in% send_table$restarted_cal[, 1]
          ) {
            alert(
              title = "DO calibration overwritten",
              type = "success",
              timer = 2000
            )
          } else {
            alert(
              title = "DO calibration saved",
              type = "success",
              timer = 2000
            )
          }
          if (send_table$saved[1, 1] == "Nothing saved yet") {
            send_table$saved[1, 1] <- "DO calibration"
          } else if (!("DO calibration" %in% send_table$saved[, 1])) {
            send_table$saved[nrow(send_table$saved) + 1, 1] <- "DO calibration"
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
        send_table$saved <- send_table$saved[
          send_table$saved[1] != "DO calibration",
          ,
          drop = FALSE
        ]
        send_table$restarted_cal <- send_table$restarted_cal[
          send_table$restarted_cal[1] != "DO calibration",
          ,
          drop = FALSE
        ]
        if (nrow(send_table$saved) == 0) {
          if (!restarted$restarted) {
            send_table$saved <- data.frame(
              "Saved calibrations" = "Nothing saved yet",
              check.names = FALSE
            )
          } else {
            send_table$saved <- data.frame(
              "Saved calibrations (this session)" = "Nothing saved yet",
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
                  actionButton(ns("ok_check_depth"), "Yes, I'm sure"),
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
      removeModal()
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
            "Depth calibration" %in%
              send_table$saved[, 1] |
              "Depth calibration" %in% send_table$restarted_cal[, 1]
          ) {
            alert(
              title = "Depth calibration overwritten",
              type = "success",
              timer = 2000
            )
          } else {
            alert(
              title = "Depth calibration saved",
              type = "success",
              timer = 2000
            )
          }
          if (send_table$saved[1, 1] == "Nothing saved yet") {
            send_table$saved[1, 1] <- "Depth calibration"
          } else if (!("Depth calibration" %in% send_table$saved[, 1])) {
            send_table$saved[
              nrow(send_table$saved) + 1,
              1
            ] <- "Depth calibration"
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
        send_table$saved <- send_table$saved[
          send_table$saved[1] != "Depth calibration",
          ,
          drop = FALSE
        ]
        send_table$restarted_cal <- send_table$restarted_cal[
          send_table$restarted_cal[1] != "Depth calibration",
          ,
          drop = FALSE
        ]
        if (nrow(send_table$saved) == 0) {
          if (!restarted$restarted) {
            send_table$saved <- data.frame(
              "Saved calibrations" = "Nothing saved yet",
              check.names = FALSE
            )
          } else {
            send_table$saved <- data.frame(
              "Saved calibrations (this session)" = "Nothing saved yet",
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

    ### Finalize calibration and submit #####################################
    observeEvent(
      {
        input$submit_btn
      },
      {
        if (
          !("Basic info" %in%
            send_table$saved[, 1] |
            "Basic info" %in% send_table$restarted_cal[, 1])
        ) {
          alert(
            title = "There is no basic information yet!!!",
            text = "Fill in your name, calibration time and date, and required serial numbers.",
            type = "error"
          )
        } else if (
          !("Temperature calibration" %in%
            send_table$saved[, 1] |
            "Temperature calibration" %in% send_table$restarted_cal[, 1])
        ) {
          alert(
            title = "Temperature calibration is mandatory",
            text = "If you've filled it in already you probably forgot to save it!",
            type = "error"
          )
        } else {
          showModal(
            modalDialog(
              title = "Are you sure?",
              "Finalized calibrations cannot be edited.",
              footer = tagList(
                modalButton("Cancel"),
                actionButton(ns("confirm_finalize"), "OK")
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
        removeModal()
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
        alert(title = paste0("Calibration finalized."), type = "success")
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
        calibration_data$temperature <- NULL
        calibration_data$spc <- NULL
        calibration_data$ph <- NULL
        calibration_data$orp <- NULL
        calibration_data$turbidity <- NULL
        calibration_data$do <- NULL
        calibration_data$depth <- NULL
        # Reset tables
        send_table$saved <- data.frame(
          "Saved calibrations" = "Nothing saved yet",
          check.names = FALSE
        ) #Title is modified later for clarity if user want to restart a cal
        output$saved <- renderTable({
          # Display local calibrations table
          send_table$saved
        })
        shinyjs::hide("restart_table")
        updateNumericInput(session, "restart_index", value = 0) #in case the user was restarting an old calibration
        updateTabsetPanel(session, "tab_panel", selected = "Calibrate")
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
