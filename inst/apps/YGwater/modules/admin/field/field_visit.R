# UI and server code for field visit module

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
      instruments_chosen = NULL,
      images_taken = NULL,
      field_measurements = NULL,
      samples = NULL
    )

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

    getModuleData() # Initial data load

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
              label = "Visit start MST",
              value = .POSIXct(Sys.time(), tz = "MST"),
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
              label = "Visit end MST (optional)",
              value = .POSIXct(Sys.time(), tz = "MST"),
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
              choices = c("None", "Rain", "Snow", "Mixed"),
              multiple = TRUE,
              options = list(
                maxItems = 1,
                placeholder = 'Select precip conditions'
              )
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
                choices = c("Light", "Moderate", "Heavy"),
                multiple = TRUE,
                options = list(
                  maxItems = 1,
                  placeholder = 'Select precip rate'
                )
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
              value = NA,
              step = 0.1
            )
          ),
          column(
            6,
            numericInput(
              ns("precip_48"),
              "48-hr precip (mm)",
              value = NA,
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

    # Keep track of the currently selected visit ID
    selected_visit <- reactiveVal(NULL)

    observeEvent(
      input$reload_module,
      {
        getModuleData()
        selected_visit(NULL)
        # Clear table row selection
        DT::dataTableProxy("visit_table") |> DT::selectRows(NULL)
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
            con,
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
          shinyWidgets::updateAirDateInput(
            session,
            "visit_datetime_start",
            value = .POSIXct(visit_data$start_datetime, tz = "UTC") +
              as.integer(input$timezone * 3600)
          )
          shinyWidgets::updateAirDateInput(
            session,
            "visit_datetime_end",
            value = .POSIXct(visit_data$end_datetime, tz = "UTC") +
              as.integer(input$timezone * 3600)
          )
          updateTextInput(
            session,
            "visit_purpose",
            value = visit_data$purpose
          )
          updateNumericInput(
            session,
            "air_temp",
            value = visit_data$air_temp
          )
          updateSelectizeInput(
            session,
            "weather_wind",
            selected = visit_data$wind
          )
          updateSelectizeInput(
            session,
            "precip",
            selected = visit_data$precip
          )
          updateSelectizeInput(
            session,
            "precip_rate",
            selected = visit_data$precip_rate
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
          if (
            nrow(visitData$samples) > 0 || length(visitData$instruments) > 0
          ) {
            updateRadioButtons(
              session,
              "field_measurements",
              selected = "yes"
            )
            # Populate measurements UI with existing sample data
            output$exist_sample_ui <- renderUI({
              tagList(
                h4("Existing samples associated with this visit"),
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
        print(selected)
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
      print("Rendering chosen instruments UI")
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
