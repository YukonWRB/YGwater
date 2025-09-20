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
      moduleData$visits <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT * FROM field.field_visits"
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
        splitLayout(
          cellWidths = c("0%", "20%", "40%", "40%"),
          tags$head(tags$style(HTML(
            ".shiny-split-layout > div {overflow: visible;}"
          ))),
          selectizeInput(
            ns("timezone"),
            "Timezone",
            choices = c(-12:12),
            selected = -7,
            multiple = FALSE,
          ),
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
            tz = -7,
            timepickerOpts = shinyWidgets::timepickerOptions(
              minutesStep = 15,
              timeFormat = "HH:mm"
            )
          ),
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
            tz = -7,
            timepickerOpts = shinyWidgets::timepickerOptions(
              minutesStep = 15,
              timeFormat = "HH:mm"
            )
          )
        ), # End of data/time splitLayout
        splitLayout(
          cellWidths = c("0%", "50%", "50%"),
          tags$head(tags$style(HTML(
            ".shiny-split-layout > div {overflow: visible;}"
          ))),
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
          ),
          selectizeInput(
            ns("sub_location"),
            "Sub-location (add new in 'locations' menu)",
            choices = NULL, # Populated by observer when location is selected
            multiple = TRUE,
            options = list(maxItems = 1, placeholder = 'Optional'),
            width = "100%"
          )
        ), # End of location/sub-location splitLayout
        textInput(
          ns("visit_purpose"),
          "Purpose of visit",
          placeholder = "Enter the purpose of the visit here",
          width = "100%",
        ),
        hr(),
        h3("Weather conditions during visit"),
        splitLayout(
          cellWidths = c("0%", "50%", "50%"),
          tags$head(tags$style(HTML(
            ".shiny-split-layout > div {overflow: visible;}"
          ))),
          numericInput(
            ns("air_temp"),
            "Air temperature (°C)",
            value = NA,
            step = 0.1
          ),
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
        ),
        selectizeInput(
          ns("precip"),
          "Precipitation during visit",
          choices = c("None", "Rain", "Snow", "Mixed"),
          multiple = TRUE,
          options = list(
            maxItems = 1,
            placeholder = 'Select precip conditions'
          )
        ),
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
        ),
        splitLayout(
          cellWidths = c("0%", "50%", "50%"),
          tags$head(tags$style(HTML(
            ".shiny-split-layout > div {overflow: visible;}"
          ))),
          numericInput(
            ns("precip_24"),
            "24-hr precip (mm)",
            value = NA,
            step = 0.1
          ),
          numericInput(
            ns("precip_48"),
            "48-hr precip (mm)",
            value = NA,
            step = 0.1
          )
        ), # End of precip splitLayout
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
          selectizeInput(
            ns("media"),
            "Media type",
            choices = stats::setNames(
              moduleData$media$media_id,
              moduleData$media$media_type
            ),
            multiple = TRUE,
            options = list(maxItems = 1, placeholder = 'Select media type'),
            width = "100%"
          ),
          # Actionbutton linking to modal to choose instrument
          actionButton(
            ns("choose_instrument"),
            "Select instruments used",
          ),
          # Show the chosen instruments here
          uiOutput(ns("instruments_chosen_ui")),
          selectizeInput(
            ns("parameters"),
            "Parameters measured",
            choices = stats::setNames(
              moduleData$parameters$parameter_id,
              moduleData$parameters$param_name
            ),
            multiple = TRUE,
            options = list(placeholder = 'Select parameters'),
            width = "100%"
          )
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
    })

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

    # Observe timezone changes and update date inputs accordingly
    observeEvent(
      input$timezone,
      {
        tz_offset <- as.integer(input$timezone)
        shinyWidgets::updateAirDateInput(
          session,
          "visit_datetime_start",
          tz = tz_offset
        )
        shinyWidgets::updateAirDateInput(
          session,
          "visit_datetime_end",
          tz = tz_offset
        )
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

    # Observe instrument selection button and show modal
    observeEvent(
      input$choose_instrument,
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
      bindEvent(input$choose_instrument)
    # When done choosing instruments, save selection and close modal

    # Observe row selection in instruments table and save selected instrument IDs
    instruments_chosen <- reactiveVal(NULL)
    observeEvent(
      input$instruments_chosen,
      {
        selected <- input$instruments_table_rows_selected
        if (length(selected) == 0) {
          instruments_chosen(NULL)
        } else {
          instrument_ids <- moduleData$instruments$instrument_id[selected]
          instruments_chosen(instrument_ids)
        }
        removeModal()
      },
      ignoreInit = TRUE
    )

    # Render the chosen instruments below the button
    output$instruments_chosen_ui <- renderUI({
      if (is.null(instruments_chosen())) {
        p("No instruments selected")
      } else {
        chosen <- moduleData$instruments[
          moduleData$instruments$instrument_id %in%
            instruments_chosen(),
          c("serial_no", "make", "model")
        ]
        tagList(
          h4("Instruments selected:"),
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
            rownames = FALSE
          )
        )
      }
    }) |>
      bindEvent(instruments_chosen)

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
          instruments_used <- DBI::dbGetQuery(
            session$userData$AquaCache,
            "SELECT instrument_id FROM field.field_visit_instruments WHERE field_visit_id = $1",
            params = list(visit_id)
          )$instrument_id
          instruments_chosen(instruments_used) # Updates the chosen instruments because of bindEvent

          # Find the images taken in this visit, if any
          images_taken <- DBI::dbGetQuery(
            session$userData$AquaCache,
            "SELECT image_id, FROM field.field_visit_images WHERE field_visit_id = $1",
            params = list(visit_id)
          )$image_id
          images_chosen(images_taken) # Placeholder until image functionality is built

          # Populate inputs with data from selected visit
          showNotification(
            "Populating inputs for modification is not yet implemented.",
            type = "warning"
          )
        }
      },
      ignoreInit = TRUE
    )
  }) # End of moduleServer
}
