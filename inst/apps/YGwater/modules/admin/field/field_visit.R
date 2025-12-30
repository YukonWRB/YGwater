# UI and server code for field visit module

# TODO modifications to module
# instrument table should have data types of factor for easier searching
# Add owner, contributor, commissioner to field visit
# Add 'reason for no field measurements' if none taken
# Remove field visit start/end, just add sample time
# map search + select for location/sub-location
# Add ability to add new location/sub-location from within the module??
# field visit -> sampling event

# Samples need depth

# Make way to retire instruments, apply with 15B104601 (yellow)

visitUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    title = "Field Visits",
    p(
      "Field visit module is under development. Anything you see here may not work as expected."
    ),
    uiOutput(ns("ui")) # Created in server so that menus can be populated right away
  ) # End of page_fluid
}

visit <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    moduleData <- reactiveValues()
    visitData <- reactiveValues(
      instruments = NULL,
      images = NULL,
      field_measurements = NULL,
      samples = NULL
    )

    # Functions for reuse within the module
    convert_to_utc <- function(datetime_value, tz_offset) {
      if (is.null(datetime_value) || all(is.na(datetime_value))) {
        return(NA)
      }
      tz_offset <- as.numeric(tz_offset)
      if (!length(tz_offset) || is.na(tz_offset)) {
        tz_offset <- 0
      }
      converted <- datetime_value - tz_offset * 60 * 60
      attr(converted, "tzone") <- "UTC"
      converted
    }

    collect_visit_inputs <- function() {
      start_utc <- convert_to_utc(input$visit_datetime_start, input$timezone)
      end_utc <- convert_to_utc(input$visit_datetime_end, input$timezone)
      location_id <- as.integer(input$location)
      sub_location_id <- as.integer(input$sub_location)
      purpose <- if (isTruthy(input$visit_purpose)) {
        input$visit_purpose
      } else {
        NA_character_
      }
      precip_type <- if (nchar(input$precip > 0) && !is.na(input$precip)) {
        input$precip
      } else {
        "None"
      }
      precip_rate <- if (
        !is.null(input$precip_rate) &&
          !is.na(precip_rate) &&
          !identical(precip_type, "None")
      ) {
        input$precip_rate
      } else {
        "None"
      }
      note <- if (isTruthy(input$visit_notes)) {
        input$visit_notes
      } else {
        NA_character_
      }
      list(
        start_utc = start_utc,
        end_utc = end_utc,
        location_id = location_id,
        sub_location_id = sub_location_id,
        purpose = purpose,
        precip_current_type = precip_type,
        precip_current_rate = precip_rate,
        precip_24 = input$precip_24,
        precip_48 = as.numeric(input$precip_48),
        air_temp = as.numeric(input$air_temp),
        wind = input$weather_wind,
        note = note,
        share_with = format_share_with(input$share_with)
      )
    }

    reset_visit_form <- function() {
      tz_offset <- as.numeric(input$timezone)
      if (!length(tz_offset) || is.na(tz_offset)) {
        tz_offset <- 0
      }
      now_utc <- .POSIXct(Sys.time(), tz = "UTC")
      display_time <- now_utc
      display_time <- display_time + tz_offset * 3600
      shinyWidgets::updateAirDateInput(
        session,
        "visit_datetime_start",
        value = display_time
      )
      shinyWidgets::updateAirDateInput(
        session,
        "visit_datetime_end",
        value = display_time
      )
      updateSelectizeInput(session, "location", selected = character(0))
      updateSelectizeInput(session, "sub_location", selected = character(0))
      updateTextInput(session, "visit_purpose", value = "")
      updateNumericInput(session, "air_temp", value = NA)
      updateSelectizeInput(session, "weather_wind", selected = character(0))
      updateSelectizeInput(session, "precip", selected = "None")
      updateSelectizeInput(session, "precip_rate", selected = "None")
      updateNumericInput(session, "precip_24", value = 0)
      updateNumericInput(session, "precip_48", value = 0)
      updateTextAreaInput(session, "visit_notes", value = "")
      updateSelectizeInput(session, "share_with", selected = "public_reader")
      visitData$instruments <- NULL
      visitData$images <- NULL
      visitData$samples <- NULL
      output$exist_sample_ui <- renderUI({})
    }

    getModuleData <- function() {
      moduleData$locations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT l.location_id, l.location, l.name, lt.type, l.latitude, l.longitude FROM locations l INNER JOIN location_types lt ON l.location_type = lt.type_id"
      )
      moduleData$sub_locations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT sub_location_id, sub_location_name, location_id FROM sub_locations"
      )
      moduleData$parameters <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT parameter_id, param_name FROM parameters"
      )
      moduleData$media <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT media_id, media_type FROM media_types"
      )
      moduleData$organizations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT organization_id, name FROM organizations"
      )
      moduleData$instruments <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT i.instrument_id, i.serial_no, instrument_make.make, instrument_model.model, instrument_type.type, i.owner FROM instruments AS i LEFT JOIN instrument_make ON i.make = instrument_make.make_id LEFT JOIN instrument_model ON i.model = instrument_model.model_id LEFT JOIN instrument_type ON i.type = instrument_type.type_id ORDER BY i.instrument_id"
      )
      moduleData$users <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT * FROM public.get_shareable_principals_for('field.field_visits');"
      ) # This is a helper function run with SECURITY DEFINER and created by postgres that pulls all user groups (plus public_reader) with select privileges on a table
      moduleData$visit_display <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "
        SELECT v.field_visit_id, l.name AS location_name, sl.sub_location_name, v.start_datetime AS start_datetime_MST
        FROM field.field_visits v
        INNER JOIN locations l ON v.location_id = l.location_id
        LEFT JOIN sub_locations sl ON v.sub_location_id = sl.sub_location_id
        "
      )
    }

    # Initial data load
    getModuleData() # Initial data load

    # Main UI rendering ###############
    output$ui <- renderUI({
      tagList(
        actionButton(
          ns("reload_module"),
          "Reload module data",
          icon = icon("refresh")
        ),
        radioButtons(
          ns("mode"),
          NULL,
          choices = c("Add new" = "add", "Modify existing" = "modify"),
          selected = "add",
          inline = TRUE
        ),
        conditionalPanel(
          condition = "input.mode == 'modify'",
          ns = ns,
          DT::DTOutput(ns("visit_table"))
        ),
        h3("Basic field visit information"),
        fluidRow(
          column(
            2,
            selectizeInput(
              ns("timezone"),
              "Timezone",
              choices = c(-12:12),
              selected = -7,
              multiple = FALSE,
            )
          ),
          column(
            5,
            shinyWidgets::airDatepickerInput(
              ns("visit_datetime_start"),
              label = "Visit start",
              value = .POSIXct(Sys.time(), tz = "UTC") - 7 * 3600, # Default to current time UTC-7
              range = FALSE,
              multiple = FALSE,
              timepicker = TRUE,
              maxDate = Sys.Date() + 1,
              startView = Sys.Date(),
              update_on = "change",
              tz = "UTC",
              timepickerOpts = shinyWidgets::timepickerOptions(
                minutesStep = 15,
                timeFormat = "HH:mm"
              )
            )
          ),
          column(
            5,
            shinyWidgets::airDatepickerInput(
              ns("visit_datetime_end"),
              label = "Visit end (optional)",
              value = .POSIXct(Sys.time(), tz = "UTC") - 7 * 3600, # Default to current time UTC-7
              range = FALSE,
              multiple = FALSE,
              timepicker = TRUE,
              maxDate = Sys.Date() + 1,
              startView = Sys.Date(),
              update_on = "change",
              tz = "UTC",
              timepickerOpts = shinyWidgets::timepickerOptions(
                minutesStep = 15,
                timeFormat = "HH:mm"
              )
            )
          )
        ), # End of data/time fluidRow
        fluidRow(
          column(
            6,
            selectizeInput(
              ns("location"),
              "Location (add new in 'locations' menu)",
              choices = stats::setNames(
                moduleData$locations$location_id,
                moduleData$locations$name
              ),
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = 'Select a location'),
              width = "100%"
            )
          ),
          column(
            6,
            selectizeInput(
              ns("sub_location"),
              "Sub-location (add new in 'locations' menu)",
              choices = NULL, # Populated by observer when location is selected
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = 'Optional'),
              width = "100%"
            )
          )
        ), # End of location/sub-location fluidRow
        textInput(
          ns("visit_purpose"),
          "Purpose of visit",
          placeholder = "Enter the purpose of the visit here",
          width = "100%",
        ),
        hr(),
        h3("Weather conditions during visit"),
        fluidRow(
          column(
            6,
            numericInput(
              ns("air_temp"),
              "Air temperature (°C)",
              value = NA,
              step = 0.1
            )
          ),
          column(
            6,
            selectizeInput(
              ns("weather_wind"),
              "Wind",
              choices = c("Calm", "Breezy", "Windy", "Very windy"),
              multiple = TRUE,
              options = list(
                maxItems = 1,
                placeholder = 'Select wind conditions'
              )
            )
          )
        ),
        fluidRow(
          column(
            6,
            selectizeInput(
              ns("precip"),
              "Precipitation during visit",
              choices = c(
                "None",
                "Rain",
                "Snow",
                "Mixed",
                "Hail",
                "Freezing rain"
              ),
              multiple = FALSE,
              selected = "None",
            )
          ),
          column(
            6,
            conditionalPanel(
              condition = "input.precip != 'None' && input.precip != ''",
              ns = ns,
              selectizeInput(
                ns("precip_rate"),
                "Precip rate",
                choices = c("None", "Light", "Moderate", "Heavy"),
                multiple = FALSE,
                selected = "None",
              )
            )
          )
        ),
        h3("Recent precipitation"),
        fluidRow(
          column(
            6,
            numericInput(
              ns("precip_24"),
              "24-hr precip (mm)",
              value = 0,
              step = 0.1
            )
          ),
          column(
            6,
            numericInput(
              ns("precip_48"),
              "48-hr precip (mm)",
              value = 0,
              step = 0.1
            )
          )
        ), # End of precip fluidRow
        hr(),
        radioButtons(
          ns("field_measurements"),
          "Were field measurements taken?",
          choices = c("Yes" = "yes", "No" = "no"),
          selected = "no"
        ),
        conditionalPanel(
          condition = "input.field_measurements == 'yes'",
          ns = ns,
          # Let the user associate the visit with an existing sample via a modal
          actionButton(
            ns("exist_sample"),
            "Associate with existing sample"
          ),
          uiOutput(ns("exist_sample_ui")), # Defaults to telling user a new sample will be created, changes if they associate to show the sample details

          # Actionbutton linking to modal to choose instrument
          actionButton(
            ns("choose_instruments"),
            "Select instruments used",
          ),
          # Show the chosen instruments here
          uiOutput(ns("instruments_chosen_ui")),
          # Actionbutton linking to modal to input measurements

          actionButton(
            ns("add_measurements"),
            "Add field measurements"
          ),
          uiOutput(ns("measurements_ui"))
        ), # End of conditionalPanel for field measurements

        radioButtons(
          ns("photos_taken"),
          "Were photos taken?",
          choices = c("Yes" = "yes", "No" = "no"),
          selected = "no"
        ),
        conditionalPanel(
          condition = "input.photos_taken == 'yes'",
          ns = ns,
          p("This functionality is being built.")
        ),

        textAreaInput(
          ns("visit_notes"),
          "Notes (optional)",
          placeholder = "Enter any notes about the visit here",
          width = "100%",
          height = "80px"
        ),
        selectizeInput(
          ns("share_with"),
          "Share with groups (1 or more, type your own if not in list)",
          choices = moduleData$users$role_name,
          selected = "public_reader",
          multiple = TRUE,
          width = "100%"
        ),
        conditionalPanel(
          condition = "input.mode == 'add'",
          ns = ns,
          bslib::input_task_button(ns("add_visit"), label = "Add new visit")
        ),
        conditionalPanel(
          condition = "input.mode == 'modify'",
          ns = ns,
          bslib::input_task_button(ns("modify_visit"), label = "Modify visit")
        )
      )
    }) # End of main renderUI

    # Render the timeseries table for modification
    output$visit_table <- DT::renderDT({
      # Convert some data types to factors for better filtering in DT
      df <- moduleData$visit_display

      DT::datatable(
        df,
        selection = "single",
        options = list(
          columnDefs = list(list(targets = 0, visible = FALSE)), # hide the id column
          scrollX = TRUE,
          initComplete = htmlwidgets::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({",
            "  'background-color': '#079',",
            "  'color': '#fff',",
            "  'font-size': '100%',",
            "});",
            "$(this.api().table().body()).css({",
            "  'font-size': '90%',",
            "});",
            "}"
          )
        ),
        filter = 'top',
        rownames = FALSE
      )
    }) |>
      bindEvent(moduleData$visit_display)

    # Worker observers and reactives ##################
    # Keep track of the currently selected visit ID
    selected_visit <- reactiveVal(NULL)

    observeEvent(
      input$mode,
      {
        if (identical(input$mode, "add")) {
          selected_visit(NULL)
          reset_visit_form()
          DT::dataTableProxy(ns("visit_table")) |> DT::selectRows(NULL)
        } else if (identical(input$mode, "modify")) {
          selected_visit(NULL)
          DT::dataTableProxy(ns("visit_table")) |> DT::selectRows(NULL)
        }
      },
      ignoreNULL = TRUE
    )

    observeEvent(
      input$reload_module,
      {
        getModuleData()
        selected_visit(NULL)
        # Clear table row selection
        DT::dataTableProxy(ns("visit_table")) |> DT::selectRows(NULL)
        reset_visit_form()
        updateSelectizeInput(
          session,
          "location",
          choices = stats::setNames(
            moduleData$locations$location_id,
            moduleData$locations$name
          )
        )
        # sub_location gets updated by the observer for input$location
        updateSelectizeInput(
          session,
          "share_with",
          choices = moduleData$users$role_name,
          selected = "public_reader"
        )
        showNotification("Module reloaded", type = "message")
      },
      ignoreInit = TRUE
    )

    # observe the location and limit the sub-locations based on those already existing
    observeEvent(
      input$location,
      {
        possibilities <- moduleData$sub_locations[
          moduleData$sub_locations$location_id == input$location,
        ]
        updateSelectizeInput(
          session,
          "sub_location",
          choices = stats::setNames(
            possibilities$sub_location_id,
            possibilities$sub_location_name
          )
        )
      },
      ignoreInit = TRUE
    )

    # Ensure that if public_reader is selected in share_with it is the only option selected
    observeEvent(
      input$share_with,
      {
        if (
          length(input$share_with) > 1 & 'public_reader' %in% input$share_with
        ) {
          showModal(modalDialog(
            "If public_reader is selected it must be the only group selected.",
            easyClose = TRUE
          ))
          updateSelectizeInput(
            session,
            "share_with",
            selected = "public_reader"
          )
        }
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$precip,
      {
        if (nchar(input$precip) == 0) {
          return()
        }
        if (identical(input$precip, "None")) {
          updateSelectizeInput(session, "precip_rate", selected = "None")
        }
      },
      ignoreNULL = TRUE
    )

    # Observe visit table row selection and populate inputs for modification
    observeEvent(
      input$visit_table_rows_selected,
      {
        selected <- input$visit_table_rows_selected
        if (length(selected) == 0) {
          selected_visit(NULL)
        } else {
          visit_id <- moduleData$visit_display$field_visit_id[selected]
          selected_visit(visit_id)

          # Find the instruments used in this visit, if any
          visitData$instruments <- DBI::dbGetQuery(
            session$userData$AquaCache,
            "SELECT instrument_id FROM field.field_visit_instruments WHERE field_visit_id = $1",
            params = list(visit_id)
          )$instrument_id
          # This will update the data.table below the button to show the instruments used

          # Find the images taken in this visit, if any
          visitData$images <- DBI::dbGetQuery(
            session$userData$AquaCache,
            "SELECT image_id FROM field.field_visit_images WHERE field_visit_id = $1",
            params = list(visit_id)
          )$image_id

          # Find any existing sample associated with this visit
          visitData$samples <- DBI::dbGetQuery(
            session$userData$AquaCache,
            "SELECT * FROM discrete.samples WHERE field_visit_id = $1",
            params = list(visit_id)
          )

          # Populate inputs with data from selected visit
          visit_data <- DBI::dbGetQuery(
            session$userData$AquaCache,
            "SELECT * FROM field.field_visits WHERE field_visit_id = $1",
            params = list(visit_id)
          )
          updateSelectizeInput(
            session,
            "location",
            selected = visit_data$location_id
          )
          updateSelectizeInput(
            session,
            "sub_location",
            selected = visit_data$sub_location_id
          )

          tz_offset <- as.numeric(input$timezone)
          if (!length(tz_offset) || is.na(tz_offset)) {
            tz_offset <- 0
          }
          start_value <- .POSIXct(visit_data$start_datetime, tz = "UTC")
          if (!all(is.na(start_value))) {
            start_value <- start_value + tz_offset * 3600
          }
          shinyWidgets::updateAirDateInput(
            session,
            "visit_datetime_start",
            value = start_value
          )
          end_value <- .POSIXct(visit_data$end_datetime, tz = "UTC")
          if (!all(is.na(end_value))) {
            end_value <- end_value + tz_offset * 3600
          }
          shinyWidgets::updateAirDateInput(
            session,
            "visit_datetime_end",
            value = end_value
          )

          updateTextInput(
            session,
            "visit_purpose",
            value = visit_data$purpose
          )
          updateNumericInput(
            session,
            "air_temp",
            value = visit_data$air_temp_c
          )
          updateSelectizeInput(
            session,
            "weather_wind",
            selected = visit_data$wind
          )
          updateSelectizeInput(
            session,
            "precip",
            selected = visit_data$precip_current_type
          )
          updateSelectizeInput(
            session,
            "precip_rate",
            selected = visit_data$precip_current_rate
          )
          updateNumericInput(
            session,
            "precip_24",
            value = visit_data$precip_24h_mm
          )
          updateNumericInput(
            session,
            "precip_48",
            value = visit_data$precip_48h_mm
          )
          updateTextAreaInput(
            session,
            "visit_notes",
            value = visit_data$note
          )
          share_groups <- parse_share_with(visit_data$share_with)
          if (!length(share_groups)) {
            share_groups <- "public_reader"
          }
          updateSelectizeInput(
            session,
            "share_with",
            selected = share_groups
          )
          has_samples <- !is.null(visitData$samples) &&
            nrow(visitData$samples) > 0
          has_instruments <- !is.null(visitData$instruments) &&
            length(visitData$instruments) > 0
          if (has_samples || has_instruments) {
            updateRadioButtons(
              session,
              "field_measurements",
              selected = "yes"
            )
            # Populate measurements UI with existing sample data
            output$exist_sample_ui <- renderUI({
              ui <- list(h4("Existing samples associated with this visit"))
              if (has_samples) {
                ui <- append(
                  ui,
                  list(
                    DT::datatable(
                      visitData$samples[, c("sample_id", "datetime", "note")],
                      options = list(
                        scrollX = TRUE,
                        initComplete = htmlwidgets::JS(
                          "function(settings, json) {",
                          "$(this.api().table().header()).css({",
                          "  'background-color': '#079',",
                          "  'color': '#fff',",
                          "  'font-size': '100%',",
                          "});",
                          "$(this.api().table().body()).css({",
                          "  'font-size': '90%',",
                          "});",
                          "}"
                        )
                      ),
                      filter = "none",
                      selection = "single",
                      rownames = FALSE
                    ),
                    fluidRow(
                      column(
                        4,
                        actionButton(
                          ns("view_sample"),
                          "View sample details"
                        )
                      ),
                      column(
                        4,
                        actionButton(
                          ns("remove_sample"),
                          "Remove association with this visit"
                        )
                      ),
                      column(
                        4,
                        actionButton(
                          ns("delete_sample"),
                          "Delete sample (cannot be undone)",
                        )
                      )
                    )
                  )
                )
              } else {
                ui <- append(
                  ui,
                  list(p(
                    "No samples are currently associated with this visit."
                  ))
                )
              }
              do.call(tagList, ui)
            })
          } else {
            updateRadioButtons(
              session,
              "field_measurements",
              selected = "no"
            )
          }
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$add_visit,
      {
        if (input$mode != "add") {
          showNotification(
            "Switch to 'Add new' mode to create a field visit.",
            type = "error"
          )
          return()
        }

        form <- collect_visit_inputs()

        if (is.na(form$start_utc)) {
          showNotification("Visit start date/time is required.", type = "error")
          return()
        }

        if (!is.na(form$end_utc) && form$end_utc <= form$start_utc) {
          showNotification(
            "End date/time must be after the start date/time.",
            type = "error"
          )
          return()
        }

        if (is.na(form$location_id)) {
          showNotification(
            "Please select a location for the visit.",
            type = "error"
          )
          return()
        }

        params <- list(
          form$start_utc,
          form$end_utc,
          form$location_id,
          form$sub_location_id,
          form$purpose,
          form$precip_current_type,
          form$precip_current_rate,
          form$precip_24,
          form$precip_48,
          form$air_temp,
          form$wind,
          form$note,
          form$share_with
        )

        insert_sql <- "
          INSERT INTO field.field_visits (
            start_datetime,
            end_datetime,
            location_id,
            sub_location_id,
            purpose,
            precip_current_type,
            precip_current_rate,
            precip_24h_mm,
            precip_48h_mm,
            air_temp_c,
            wind,
            note,
            share_with
          ) VALUES (
            $1, $2, $3, $4, $5, $6 $7, $8, $9, $10, $11, $12, $13::text[]
          )
          RETURNING field_visit_id;
        "

        tryCatch(
          {
            DBI::dbWithTransaction(
              con,
              {
                res <- DBI::dbGetQuery(con, insert_sql, params = params)
                visit_id <- res$field_visit_id[1]
                if (length(visitData$instruments) > 0) {
                  for (instrument_id in visitData$instruments) {
                    DBI::dbExecute(
                      con,
                      "INSERT INTO field.field_visit_instruments (field_visit_id, instrument_id) VALUES ($1, $2)",
                      params = list(visit_id, instrument_id)
                    )
                  }
                }
                visit_id
              }
            )

            showNotification(
              "Field visit added successfully.",
              type = "message"
            )
            getModuleData()
            selected_visit(NULL)
            reset_visit_form()
            DT::dataTableProxy(ns("visit_table")) |> DT::selectRows(NULL)
          },
          error = function(e) {
            showNotification(
              paste("Failed to add field visit:", e$message),
              type = "error"
            )
          }
        )
      },
      ignoreNULL = TRUE
    )

    observeEvent(
      input$modify_visit,
      {
        if (input$mode != "modify") {
          showNotification(
            "Switch to 'Modify existing' mode to update a visit.",
            type = "error"
          )
          return()
        }

        visit_id <- selected_visit()
        if (is.null(visit_id)) {
          showNotification("Select a field visit to modify.", type = "error")
          return()
        }

        form <- collect_visit_inputs()

        if (is.na(form$start_utc)) {
          showNotification("Visit start date/time is required.", type = "error")
          return()
        }

        if (!is.na(form$end_utc) && form$end_utc <= form$start_utc) {
          showNotification(
            "End date/time must be after the start date/time.",
            type = "error"
          )
          return()
        }

        if (is.na(form$location_id)) {
          showNotification(
            "Please select a location for the visit.",
            type = "error"
          )
          return()
        }

        update_sql <- "
          UPDATE field.field_visits
          SET
            start_datetime = $1,
            end_datetime = $2,
            location_id = $3,
            sub_location_id = $4,
            purpose = $5,
            precip_current_type = $6,
            precip_current_rate = $7
            precip_24h_mm = $7,
            precip_48h_mm = $8,
            air_temp_c = $9,
            wind = $10,
            note = $11,
            share_with = $12::text[]
          WHERE field_visit_id = $13;
        "

        params <- list(
          form$start_utc,
          form$end_utc,
          form$location_id,
          form$sub_location_id,
          form$purpose,
          form$current_precip,
          form$precip_24,
          form$precip_48,
          form$air_temp,
          form$wind,
          form$note,
          form$share_with,
          visit_id
        )

        tryCatch(
          {
            DBI::dbWithTransaction(
              con,
              {
                DBI::dbExecute(con, update_sql, params = params)
                DBI::dbExecute(
                  con,
                  "DELETE FROM field.field_visit_instruments WHERE field_visit_id = $1",
                  params = list(visit_id)
                )
                if (length(visitData$instruments) > 0) {
                  for (instrument_id in visitData$instruments) {
                    DBI::dbExecute(
                      con,
                      "INSERT INTO field.field_visit_instruments (field_visit_id, instrument_id) VALUES ($1, $2)",
                      params = list(visit_id, instrument_id)
                    )
                  }
                }
              }
            )

            showNotification(
              "Field visit updated successfully.",
              type = "message"
            )
            getModuleData()
            if (nrow(moduleData$visit_display) > 0) {
              row_index <- which(
                moduleData$visit_display$field_visit_id == visit_id
              )
              if (length(row_index) == 1) {
                DT::dataTableProxy(ns("visit_table")) |>
                  DT::selectRows(row_index)
              }
            }
          },
          error = function(e) {
            showNotification(
              paste("Failed to update field visit:", e$message),
              type = "error"
            )
          }
        )
      },
      ignoreNULL = TRUE
    )

    # Observe instrument selection button and show modal
    observeEvent(
      input$choose_instruments,
      {
        showModal(modalDialog(
          title = "Select instruments used",
          size = "l",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("instruments_chosen"), "Done")
          ),
          DT::DTOutput(ns("instruments_table"))
        ))
      },
      ignoreInit = TRUE
    )
    # Render instruments table in modal
    output$instruments_table <- DT::renderDT({
      df <- moduleData$instruments

      DT::datatable(
        df,
        selection = "multiple",
        options = list(
          columnDefs = list(list(targets = 0, visible = FALSE)), # hide the id column
          scrollX = TRUE,
          initComplete = htmlwidgets::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({",
            "  'background-color': '#079',",
            "  'color': '#fff',",
            "  'font-size': '100%',",
            "});",
            "$(this.api().table().body()).css({",
            "  'font-size': '90%',",
            "});",
            "}"
          )
        ),
        filter = 'top',
        rownames = FALSE
      )
    }) |>
      bindEvent(input$choose_instruments)
    # When done choosing instruments, save selection and close modal

    # Observe row selection in instruments table and save selected instrument IDs
    observeEvent(
      input$instruments_chosen,
      {
        selected <- input$instruments_table_rows_selected
        if (length(selected) == 0) {
          visitData$instruments <- NULL
        } else {
          instrument_ids <- moduleData$instruments$instrument_id[selected]
          visitData$instruments <- instrument_ids
        }
        removeModal()
      },
      ignoreInit = TRUE
    )

    # Render the chosen instruments below the button
    output$instruments_chosen_ui <- renderUI({
      if (is.null(visitData$instruments)) {
        return()
      } else {
        chosen <- moduleData$instruments[
          moduleData$instruments$instrument_id %in%
            visitData$instruments,
          c("serial_no", "make", "model")
        ]
        tagList(
          h4("Instruments used for field visit"),
          DT::datatable(
            chosen,
            options = list(
              scrollX = TRUE,
              initComplete = htmlwidgets::JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({",
                "  'background-color': '#079',",
                "  'color': '#fff',",
                "  'font-size': '100%',",
                "});",
                "$(this.api().table().body()).css({",
                "  'font-size': '90%',",
                "});",
                "}"
              )
            ),
            filter = "none",
            selection = "none",
            rownames = FALSE
          )
        )
      }
    }) |>
      bindEvent(visitData$instruments)
  }) # End of moduleServer
}
