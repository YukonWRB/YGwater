# UI and server code for adding new continuous measurements

addContDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(
      HTML(sprintf(
        "
     /* Add colors to the accordion. Using ns() makes it specific to this module */
      #%s.accordion {
        /* body background */
        --bs-accordion-bg:          #FFFCF5;
        /* collapsed header */
        --bs-accordion-btn-bg:      #FBE5B2;
        /* expanded header */
        --bs-accordion-active-bg:   #FBE5B2;
      }
    ",
        ns("accordion1")
      )),
      HTML(sprintf(
        "
     /* Add colors to the accordion. Using ns() makes it specific to this module */
      #%s.accordion {
        /* body background */
        --bs-accordion-bg:          #E5F4F6;
        /* collapsed header */
        --bs-accordion-btn-bg:      #0097A9;
        /* expanded header */
        --bs-accordion-active-bg:   #0097A9;
      }
    ",
        ns("accordion2")
      ))
    ),
    tags$head(tags$style(HTML(
      ".shiny-split-layout > div {overflow: visible;}"
    ))),
    page_fluid(
      uiOutput(ns("banner")),
      actionButton(
        ns("reload_module"),
        "Reload module data",
        icon = icon("refresh")
      ),
      accordion(
        id = ns("accordion1"),
        open = "ts_panel",
        accordion_panel(
          id = ns("ts_panel"),
          title = "Timeseries selection",
          actionButton(ns("addNewTS"), "Click here to add a new timeseries"),
          DT::DTOutput(ns("ts_table"))
        )
      ),

      accordion(
        id = ns("accordion2"),
        open = "data_entry_panel",
        accordion_panel(
          id = ns("data_entry_panel"),
          title = "Add data",
          icon = icon("table"),
          uiOutput(ns("selected_units_warning")),
          radioButtons(
            ns("entry_mode"),
            "Input method",
            choices = c("File" = "file", "Manual" = "manual"),
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.entry_mode == 'file'",
            ns = ns,
            fileInput(
              ns("file"),
              "Upload .csv or .xlsx",
              accept = c(".csv", ".xlsx")
            )
          ),
          conditionalPanel(
            condition = "input.entry_mode == 'manual'",
            ns = ns,
            div(
              actionButton(ns("add_row"), "Add row to end"),
              actionButton(ns("add_row_above"), "Add row above selection"),
              actionButton(ns("add_row_below"), "Add row below selection"),
              actionButton(ns("delete_rows_table"), "Delete selected rows")
            ),
            tags$br()
          ),

          DT::DTOutput(ns("data_table")),

          uiOutput(ns("data_table_note")),
          tags$br(),
          splitLayout(
            cellWidths = c("70%", "30%"),
            selectizeInput(
              ns("UTC_offset"),
              "UTC offset of data",
              choices = input_timezone_choices(),
              selected = format_utc_offset(0L),
              multiple = FALSE
            ),
            div(
              style = "padding-top: 25px;",
              actionButton(
                ns("open_unit_conversion"),
                "Convert units",
                icon = icon("calculator"),
                class = "btn-warning"
              )
            )
          ),
          splitLayout(
            cellWidths = c("50%", "50%"),
            selectizeInput(
              ns("owner"),
              "Owner organization (applies to these data only)",
              choices = NULL,
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Select owner"),
              width = "100%"
            ),
            selectizeInput(
              ns("contributor"),
              "Contributor organization (applies to these data only)",
              choices = NULL,
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Select contributor"),
              width = "100%"
            )
          ),
          radioButtons(
            ns("no_update"),
            "Prevent updates to these data by automatic processes?",
            choices = c("Yes" = "yes", "No" = "no"),
            inline = TRUE,
            selected = "no"
          ),
          tags$div(
            "Note: data visibility is controlled by the timeseries visibility parameters."
          )
        ),
        # accordion to hold a plotly plot of the uploaded/added data for quick visual checks before upload
        accordion_panel(
          id = ns("preview_panel"),
          title = "Preview data",
          icon = icon("chart-line"),
          checkboxInput(
            ns("preview_historic_range"),
            "Show historic range",
            value = TRUE
          ),
          splitLayout(
            cellWidths = c("33%", "33%", "33%"),
            selectizeInput(
              ns("preview_utc_offset"),
              "Preview UTC offset",
              choices = input_timezone_choices(),
              selected = format_utc_offset(0L),
              multiple = FALSE,
              width = "100%"
            ),
            shinyWidgets::airDatepickerInput(
              ns("preview_start_datetime"),
              "Preview start datetime",
              value = NULL,
              range = FALSE,
              multiple = FALSE,
              timepicker = TRUE,
              update_on = "change",
              tz = air_datetime_widget_timezone(format_utc_offset(0L)),
              timepickerOpts = shinyWidgets::timepickerOptions(
                minutesStep = 15,
                timeFormat = "HH:mm"
              )
            ),
            shinyWidgets::airDatepickerInput(
              ns("preview_end_datetime"),
              "Preview end datetime",
              value = NULL,
              range = FALSE,
              multiple = FALSE,
              timepicker = TRUE,
              update_on = "change",
              tz = air_datetime_widget_timezone(format_utc_offset(0L)),
              timepickerOpts = shinyWidgets::timepickerOptions(
                minutesStep = 15,
                timeFormat = "HH:mm"
              )
            )
          ),
          bslib::input_task_button(
            ns("make_plot"),
            "Refresh plot",
            icon = icon("refresh")
          ),
          uiOutput(ns("plot_refresh_warning")),
          plotly::plotlyOutput(ns("data_preview"))
        ),

        # Add delete/grade/approval/qualifier functionality within accordions
        # Delete regions panel
        accordion_panel(
          id = ns("delete_panel"),
          title = "Delete data",
          icon = icon("trash"),
          selectizeInput(
            ns("delete_utc_offset"),
            "Delete UTC offset",
            choices = input_timezone_choices(),
            selected = format_utc_offset(0L),
            multiple = FALSE,
            width = "100%"
          ),
          shinyWidgets::airDatepickerInput(
            ns("delete_cutoff_datetime"),
            "Delete data before/after datetime",
            value = NULL,
            range = FALSE,
            multiple = FALSE,
            timepicker = TRUE,
            update_on = "change",
            tz = air_datetime_widget_timezone(format_utc_offset(0L)),
            timepickerOpts = shinyWidgets::timepickerOptions(
              minutesStep = 15,
              timeFormat = "HH:mm"
            )
          ),
          div(
            conditionalPanel(
              condition = "input.entry_mode == 'manual'",
              ns = ns,
              actionButton(ns("delete_rows_accordion"), "Delete selected rows")
            ),
            actionButton(
              ns("delete_before_datetime"),
              "Delete rows before datetime"
            ) |>
              tooltip(
                "Only delete data that has no possible later use, such as pre/post deployment data. Data that has a non-zero chance of being useful later should be uploaded and can be suppressed using a delete region correction or graded/qualified appropriately."
              ),
            actionButton(
              ns("delete_after_datetime"),
              "Delete rows after datetime"
            ) |>
              tooltip(
                "Only delete data that has no possible later use, such as pre/post deployment data. Data that has a non-zero chance of being useful later should be uploaded and can be suppressed using a delete region correction or graded/qualified appropriately."
              )
          )
        ), # end delete accordion panel

        # Add approvals panel
        accordion_panel(
          id = ns("approval_panel"),
          title = "Add/modify approval status",
          icon = icon("thumbs-up"),
          selectizeInput(
            ns("approval_utc_offset"),
            "Approval UTC offset",
            choices = input_timezone_choices(),
            selected = format_utc_offset(0L),
            multiple = FALSE,
            width = "100%"
          ),
          div(
            actionButton(ns("add_approval_range"), "Add approval range"),
            actionButton(ns("edit_approval_range"), "Edit selected"),
            actionButton(ns("delete_approval_range"), "Delete selected")
          ),
          DT::DTOutput(ns("approval_ranges_table")),
          uiOutput(ns("approval_ranges_warning"))
        ), # End approval panel

        # Add grade panel
        accordion_panel(
          id = ns("grade_panel"),
          title = "Add/modify grades",
          icon = icon("check"),
          selectizeInput(
            ns("grade_utc_offset"),
            "Grade UTC offset",
            choices = input_timezone_choices(),
            selected = format_utc_offset(0L),
            multiple = FALSE,
            width = "100%"
          ),
          div(
            actionButton(ns("add_grade_range"), "Add grade range"),
            actionButton(ns("edit_grade_range"), "Edit selected"),
            actionButton(ns("delete_grade_range"), "Delete selected")
          ),
          DT::DTOutput(ns("grade_ranges_table")),
          uiOutput(ns("grade_ranges_warning"))
        ), # end grade accordion panel

        # Add qualifiers panel
        accordion_panel(
          id = ns("qualifier_panel"),
          title = "Add/modify qualifiers",
          icon = icon("flag"),
          selectizeInput(
            ns("qualifier_utc_offset"),
            "Qualifier UTC offset",
            choices = input_timezone_choices(),
            selected = format_utc_offset(0L),
            multiple = FALSE,
            width = "100%"
          ),
          div(
            actionButton(ns("add_qualifier_range"), "Add qualifier range"),
            actionButton(ns("edit_qualifier_range"), "Edit selected"),
            actionButton(ns("delete_qualifier_range"), "Delete selected")
          ),
          DT::DTOutput(ns("qualifier_ranges_table")),
          uiOutput(ns("qualifier_ranges_warning"))
        ) # End qualifiers accordion panel
      ), # end accordion for data manipulation options

      br(),
      uiOutput(ns("selected_units_warning_last")),

      bslib::input_task_button(
        ns("upload"),
        "Upload to AquaCache (no overwrite)",
        type = "primary",
        style = "font-size: 14px;",
        label_busy = "Uploading..."
      ),
      bslib::input_task_button(
        ns("upload_overwrite_all"),
        "Upload to AquaCache (replace all points in new data range)",
        type = "primary",
        style = "font-size: 14px;",
        label_busy = "Uploading..."
      ),
      bslib::input_task_button(
        ns("upload_overwrite_some"),
        "Upload to AquaCache (overwrite conflicting points only)",
        type = "primary",
        style = "font-size: 14px;",
        label_busy = "Uploading..."
      )
    )
  )
}

addContData <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "addContData"
      )
    })

    outputs <- reactiveValues() # Used to pass the user on to adding a timeseries directly

    moduleData <- reactiveValues(
      organizations = DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT organization_id, name FROM public.organizations ORDER BY name ASC"
      ),
      unit_conversions = DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT
           uc.conversion_id,
           u_from.unit_name AS from_unit,
           u_to.unit_name AS to_unit,
           uc.conversion_type,
           uc.scale_a,
           uc.scale_b
         FROM public.unit_conversions uc
         JOIN public.units u_from
           ON u_from.unit_id = uc.from_unit_id
         JOIN public.units u_to
           ON u_to.unit_id = uc.to_unit_id
         ORDER BY u_from.unit_name, u_to.unit_name"
      )
    )

    check <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT has_table_privilege(current_user, 'continuous.measurements_continuous', 'INSERT') AS can_insert"
    )
    can_insert <- isTRUE(check$can_insert[[1]])
    if (!can_insert) {
      showModal(modalDialog(
        title = 'Insufficient Privileges',
        'You do not have write privileges to add measurements.',
        easyClose = TRUE,
        footer = modalButton('Close')
      ))
      shinyjs::disable('upload')
      shinyjs::disable('upload_overwrite_all')
      shinyjs::disable('upload_overwrite_some')
    }

    ts_meta <- reactiveVal({
      dbGetQueryDT(
        session$userData$AquaCache,
        # "SELECT timeseries_id, location_name AS location, alias_name AS alias, parameter_name AS parameter, media_type AS media, aggregation_type AS aggregation, recording_rate AS record_rate_minutes, sensor_priority FROM continuous.timeseries_metadata_en ORDER BY location_name, parameter_name, media_type, aggregation_type, record_rate_minutes ASC"
        "SELECT timeseries_id, location_name AS location, parameter_name AS parameter, units, media_type AS media, aggregation_type AS aggregation, recording_rate AS record_rate_minutes FROM continuous.timeseries_metadata_en ORDER BY location_name, parameter_name, media_type, aggregation_type, record_rate_minutes ASC"
      )
    })

    # Reload module data when asked
    observeEvent(input$reload_module, {
      ts_meta(dbGetQueryDT(
        session$userData$AquaCache,
        # "SELECT timeseries_id, location_name AS location, alias_name AS alias, parameter_name AS parameter, media_type AS media, aggregation_type AS aggregation, recording_rate AS record_rate_minutes, sensor_priority FROM continuous.timeseries_metadata_en ORDER BY location_name, parameter_name, media_type, aggregation_type, record_rate_minutes ASC"
        "SELECT timeseries_id, location_name AS location, parameter_name AS parameter, units, media_type AS media, aggregation_type AS aggregation, recording_rate AS record_rate_minutes FROM continuous.timeseries_metadata_en ORDER BY location_name, parameter_name, media_type, aggregation_type, record_rate_minutes ASC"
      ))
      moduleData$organizations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT organization_id, name FROM public.organizations ORDER BY name ASC"
      )
      moduleData$unit_conversions <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT
           uc.conversion_id,
           u_from.unit_name AS from_unit,
           u_to.unit_name AS to_unit,
           uc.conversion_type,
           uc.scale_a,
           uc.scale_b
         FROM public.unit_conversions uc
         JOIN public.units u_from
           ON u_from.unit_id = uc.from_unit_id
         JOIN public.units u_to
           ON u_to.unit_id = uc.to_unit_id
         ORDER BY u_from.unit_name, u_to.unit_name"
      )
    })

    # Change to add timeseries tab when button clicked
    observeEvent(input$addNewTS, {
      outputs$change_tab <- "addTimeseries"
    })

    output$ts_table <- DT::renderDT({
      # Convert some data types to factors for better filtering in DT
      df <- ts_meta()
      df$record_rate_minutes <- as.factor(df$record_rate_minutes)
      df$location <- as.factor(df$location)
      # df$alias <- as.factor(df$alias)
      df$media <- as.factor(df$media)
      df$aggregation <- as.factor(df$aggregation)
      df$parameter <- as.factor(df$parameter)
      # df$sensor_priority <- as.factor(df$sensor_priority)
      DT::datatable(
        df,
        selection = 'single',
        options = list(
          columnDefs = list(list(targets = 0, visible = FALSE)),
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
    })

    # Observe timeseries selection and assign to reactiveVal
    timeseries <- reactiveVal(NULL)
    observeEvent(input$ts_table_rows_selected, {
      sel <- input$ts_table_rows_selected
      if (length(sel) > 0) {
        timeseries(ts_meta()[sel, 'timeseries_id'][[1]])
      } else {
        timeseries(NULL)
      }
    })

    selected_timeseries_meta <- reactive({
      req(timeseries())
      meta <- ts_meta()
      meta[meta$timeseries_id == timeseries(), , drop = FALSE]
    })

    selected_timeseries_units <- reactive({
      meta <- selected_timeseries_meta()
      if (!nrow(meta)) {
        return(NA_character_)
      }
      unit <- meta$units[[1]]
      if (is.na(unit) || !nzchar(unit)) {
        return(NA_character_)
      }
      unit
    })

    selected_units_warning_tag <- function() {
      if (is.null(timeseries())) {
        return(div(
          class = "alert alert-warning",
          style = "margin-bottom: 10px;",
          tags$strong("Select a timeseries before entering or uploading data."),
        ))
      }

      unit <- selected_timeseries_units()
      unit_text <- if (is.na(unit)) {
        "No database unit is set for this timeseries."
      } else {
        paste0("Database unit for this timeseries: ", unit)
      }
      detail <- "Enter or convert values to this unit before upload."

      div(
        class = if (is.na(unit)) {
          "alert alert-danger"
        } else {
          "alert alert-warning"
        },
        style = "margin-bottom: 10px;",
        tags$strong(unit_text),
        tags$div(detail)
      )
    }

    output$selected_units_warning <- renderUI({
      selected_units_warning_tag()
    })
    output$selected_units_warning_last <- renderUI({
      selected_units_warning_tag()
    })

    # Update owner and contributor selectize inputs when organizations data is loaded
    observe({
      req(moduleData$organizations)
      updateSelectizeInput(
        session,
        "owner",
        choices = stats::setNames(
          moduleData$organizations$organization_id,
          moduleData$organizations$name
        ),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "contributor",
        choices = stats::setNames(
          moduleData$organizations$organization_id,
          moduleData$organizations$name
        ),
        server = TRUE
      )
    })

    # When timeseries is selected, update owner and contributor to default owner from timeseries table
    observeEvent(timeseries(), {
      req(timeseries())
      default_owner <- DBI::dbGetQuery(
        session$userData$AquaCache,
        sprintf(
          "SELECT default_owner FROM timeseries WHERE timeseries_id = %s",
          as.integer(timeseries())
        )
      )
      if (nrow(default_owner) > 0) {
        updateSelectizeInput(
          session,
          "owner",
          selected = default_owner$default_owner[[1]]
        )
        updateSelectizeInput(
          session,
          "contributor",
          selected = default_owner$default_owner[[1]]
        )
      }
    })

    shift_datetime_inputs <- function(input_ids, tz_name) {
      for (input_id in input_ids) {
        shift_air_datetime_input_timezone(session, input, input_id, tz_name)
      }
    }

    shift_class_modal_inputs <- function(class_name, tz_name) {
      shift_datetime_inputs(
        c(
          paste0(class_name, "_modal_start"),
          paste0(class_name, "_modal_end")
        ),
        tz_name
      )
    }

    ### Observe the owner selectizeInput for new owners ############
    addOrgModal <- function(name) {
      # Called when adding owner or contributor
      showModal(modalDialog(
        textInput(
          ns("org_name"),
          "Organization name",
          value = input[[name]]
        ),
        textInput(ns("org_name_fr"), "Organization name French (optional)"),
        textInput(ns("org_contact_name"), "Contact name (optional)"),
        textInput(ns("org_contact_phone"), "Contact phone (optional)"),
        textInput(ns("org_contact_email"), "Contact email (optional)"),
        textInput(
          ns("org_contact_note"),
          "Contact note (optional, for context)"
        ),
        actionButton(ns(paste0("add_", name)), "Add organization")
      ))
    }
    observeEvent(
      input$owner,
      {
        # Check for new organization (not in the list already)
        if (
          input$owner %in%
            moduleData$organizations$organization_id ||
            nchar(input$owner) == 0
        ) {
          return()
        }
        # If new, show modal dialog to add organization details
        addOrgModal(name = "owner")
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )
    observeEvent(
      input$contributor,
      {
        # Check for new organization (not in the list already)
        if (
          input$contributor %in%
            moduleData$organizations$organization_id ||
            nchar(input$contributor) == 0
        ) {
          return()
        }
        # If new, show modal dialog to add organization details
        addOrgModal(name = "contributor")
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )
    observeEvent(
      input$add_owner,
      {
        # Check that mandatory fields are filled in
        if (!isTruthy(input$org_name)) {
          shinyjs::js$backgroundCol(ns("org_name"), "#fdd")
          return()
        }
        # Add the owner to the database
        df <- data.frame(
          name = input$org_name,
          name_fr = if (isTruthy(input$org_name_fr)) {
            input$org_name_fr
          } else {
            NA
          },
          contact_name = if (isTruthy(input$org_contact_name)) {
            input$org_contact_name
          } else {
            NA
          },
          phone = if (isTruthy(input$org_contact_phone)) {
            input$org_contact_phone
          } else {
            NA
          },
          email = if (isTruthy(input$org_contact_email)) {
            input$org_contact_email
          } else {
            NA
          },
          note = if (isTruthy(input$org_contact_note)) {
            input$org_contact_note
          } else {
            NA
          }
        )
        DBI::dbExecute(
          session$userData$AquaCache,
          "INSERT INTO public.organizations (name, name_fr, contact_name, phone, email, note) VALUES ($1, $2, $3, $4, $5, $6)",
          params = list(
            df$name,
            ifelse(is.na(df$name_fr), NA, df$name_fr),
            ifelse(is.na(df$contact_name), NA, df$contact_name),
            ifelse(is.na(df$phone), NA, df$phone),
            ifelse(is.na(df$email), NA, df$email),
            ifelse(is.na(df$note), NA, df$note)
          )
        )

        # Update the moduleData reactiveValues
        moduleData$organizations <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT organization_id, name FROM organizations"
        )
        # Update the selectizeInput to the new value
        updateSelectizeInput(
          session,
          "owner",
          choices = stats::setNames(
            moduleData$organizations$organization_id,
            moduleData$organizations$name
          ),
          selected = moduleData$organizations[
            moduleData$organizations$name == df$name,
            "organization_id"
          ]
        )
        removeModal()
        showModal(modalDialog(
          "New organization added.",
          easyClose = TRUE
        ))
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$add_contributor,
      {
        # Check that mandatory fields are filled in
        if (!isTruthy(input$org_name)) {
          shinyjs::js$backgroundCol(ns("org_name"), "#fdd")
          return()
        }
        # Add the owner to the database
        df <- data.frame(
          name = input$org_name,
          name_fr = if (isTruthy(input$org_name_fr)) {
            input$org_name_fr
          } else {
            NA
          },
          contact_name = if (isTruthy(input$org_contact_name)) {
            input$org_contact_name
          } else {
            NA
          },
          phone = if (isTruthy(input$org_contact_phone)) {
            input$org_contact_phone
          } else {
            NA
          },
          email = if (isTruthy(input$org_contact_email)) {
            input$org_contact_email
          } else {
            NA
          },
          note = if (isTruthy(input$org_contact_note)) {
            input$org_contact_note
          } else {
            NA
          }
        )
        DBI::dbExecute(
          session$userData$AquaCache,
          "INSERT INTO public.organizations (name, name_fr, contact_name, phone, email, note) VALUES ($1, $2, $3, $4, $5, $6)",
          params = list(
            df$name,
            ifelse(is.na(df$name_fr), NA, df$name_fr),
            ifelse(is.na(df$contact_name), NA, df$contact_name),
            ifelse(is.na(df$phone), NA, df$phone),
            ifelse(is.na(df$email), NA, df$email),
            ifelse(is.na(df$note), NA, df$note)
          )
        )

        # Update the moduleData reactiveValues
        moduleData$organizations <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT organization_id, name FROM organizations"
        )
        # Update the selectizeInput to the new value
        updateSelectizeInput(
          session,
          "contributor",
          choices = stats::setNames(
            moduleData$organizations$organization_id,
            moduleData$organizations$name
          ),
          selected = moduleData$organizations[
            moduleData$organizations$name == df$name,
            "organization_id"
          ]
        )
        removeModal()
        showModal(modalDialog(
          "New organization added.",
          easyClose = TRUE
        ))
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    # Reactive values to hold uploaded data and parsed data
    data <- reactiveValues(
      df = data.frame(
        datetime = character(),
        value = numeric(),
        grade = character(),
        approval = character(),
        qualifier = character(),
        stringsAsFactors = FALSE
      ),
      parsed_datetime = NULL,
      parsed_value = NULL
    )

    unit_conversion_state <- reactiveValues(
      previous_values = NULL,
      previous_label = NULL
    )

    table_render_tick <- reactiveVal(0L)
    refresh_data_table <- function() {
      table_render_tick(isolate(table_render_tick()) + 1L)
    }

    observeEvent(timeseries(), {
      unit_conversion_state$previous_values <- NULL
      unit_conversion_state$previous_label <- NULL
    })

    unit_conversion_choices <- reactive({
      req(moduleData$unit_conversions)
      unit <- selected_timeseries_units()
      if (is.na(unit)) {
        return(moduleData$unit_conversions[0, , drop = FALSE])
      }

      moduleData$unit_conversions[
        moduleData$unit_conversions$to_unit == unit,
        ,
        drop = FALSE
      ]
    })

    unit_conversion_controls <- function() {
      req(timeseries())
      unit <- selected_timeseries_units()
      if (is.na(unit)) {
        return(div(
          class = "alert alert-danger",
          "Unit conversion is unavailable because the selected timeseries has no database unit."
        ))
      }

      choices_df <- unit_conversion_choices()
      choices <- character()
      if (nrow(choices_df) > 0) {
        choices <- stats::setNames(
          as.character(choices_df$conversion_id),
          paste0(
            choices_df$from_unit,
            " to ",
            choices_df$to_unit,
            " (value * ",
            signif(as.numeric(choices_df$scale_a), 8),
            ifelse(
              as.numeric(choices_df$scale_b) == 0,
              "",
              paste0(" + ", signif(as.numeric(choices_df$scale_b), 8))
            ),
            ")"
          )
        )
      }
      default_mode <- if (length(choices) > 0) "database" else "custom"

      tagList(
        div(
          class = "well",
          style = "padding: 10px; margin-bottom: 10px;",
          tags$strong("Convert table values to database units"),
          tags$div(
            class = "text-muted small",
            paste0(
              "Use this if the value column is not already in ",
              unit,
              ". Only the value column is changed."
            )
          ),
          radioButtons(
            ns("unit_conversion_mode"),
            "Conversion source",
            choices = c(
              "Database conversion" = "database",
              "Custom factor" = "custom"
            ),
            selected = default_mode,
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.unit_conversion_mode == 'database'",
            ns = ns,
            if (length(choices) > 0) {
              selectizeInput(
                ns("unit_conversion_id"),
                "Convert from",
                choices = choices,
                selected = choices[[1]],
                options = list(placeholder = "No database conversion listed")
              )
            } else {
              div(
                class = "text-muted small",
                "No database conversions are listed for this unit. Use a custom factor."
              )
            }
          ),
          conditionalPanel(
            condition = "input.unit_conversion_mode == 'custom'",
            ns = ns,
            numericInput(
              ns("custom_unit_factor"),
              paste0("Custom factor to ", unit),
              value = NA_real_,
              min = 0,
              step = "any"
            )
          ),
          div(
            actionButton(ns("convert_units"), "Convert value column"),
            actionButton(ns("rollback_unit_conversion"), "Roll back conversion")
          ),
          uiOutput(ns("unit_conversion_status"))
        )
      )
    }

    output$unit_conversion_modal_body <- renderUI({
      unit_conversion_controls()
    })

    observeEvent(input$open_unit_conversion, {
      if (is.null(timeseries())) {
        showNotification(
          "Select a timeseries before converting units.",
          type = "error"
        )
        return()
      }

      showModal(modalDialog(
        title = "Convert table values to database units",
        uiOutput(ns("unit_conversion_modal_body")),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "m"
      ))
    })

    output$unit_conversion_status <- renderUI({
      if (is.null(unit_conversion_state$previous_values)) {
        return(NULL)
      }

      div(
        class = "text-muted small",
        paste("Last conversion:", unit_conversion_state$previous_label)
      )
    })

    observeEvent(input$convert_units, {
      if (!is.null(unit_conversion_state$previous_values)) {
        showNotification(
          "Values have already been converted. Roll back before converting again.",
          type = "error",
          duration = 8
        )
        return()
      }

      if (nrow(data$df) == 0) {
        showNotification("No table values to convert.", type = "error")
        return()
      }

      values <- suppressWarnings(as.numeric(data$df$value))
      if (any(is.na(values))) {
        showNotification(
          "Value column must be numeric with no missing values before conversion.",
          type = "error",
          duration = 8
        )
        return()
      }

      unit <- selected_timeseries_units()
      if (is.na(unit)) {
        showNotification(
          "No database unit is set for the selected timeseries.",
          type = "error",
          duration = 8
        )
        return()
      }

      if (identical(input$unit_conversion_mode, "custom")) {
        factor <- suppressWarnings(as.numeric(input$custom_unit_factor))
        if (length(factor) != 1 || is.na(factor) || factor <= 0) {
          showNotification(
            "Enter a positive custom conversion factor.",
            type = "error"
          )
          return()
        }
        new_values <- values * factor
        label <- paste0("custom factor ", signif(factor, 8), " to ", unit)
      } else {
        if (!isTruthy(input$unit_conversion_id)) {
          showNotification(
            paste0("No database conversion to ", unit, " is selected."),
            type = "error",
            duration = 8
          )
          return()
        }
        choices_df <- unit_conversion_choices()
        idx <- match(
          as.integer(input$unit_conversion_id),
          choices_df$conversion_id
        )
        if (is.na(idx)) {
          showNotification(
            paste0("No database conversion to ", unit, " is selected."),
            type = "error",
            duration = 8
          )
          return()
        }
        new_values <- as.numeric(choices_df$scale_a[[idx]]) *
          values +
          as.numeric(choices_df$scale_b[[idx]])
        label <- paste0(
          choices_df$from_unit[[idx]],
          " to ",
          choices_df$to_unit[[idx]]
        )
      }

      unit_conversion_state$previous_values <- data$df$value
      unit_conversion_state$previous_label <- label
      data$df$value <- new_values
      refresh_data_table()
      showNotification(
        paste("Converted value column:", label),
        type = "message"
      )
    })

    observeEvent(input$rollback_unit_conversion, {
      if (is.null(unit_conversion_state$previous_values)) {
        showNotification("No unit conversion to roll back.", type = "message")
        return()
      }

      if (length(unit_conversion_state$previous_values) != nrow(data$df)) {
        showNotification(
          "Cannot roll back because the table row count has changed.",
          type = "error",
          duration = 8
        )
        return()
      }

      data$df$value <- unit_conversion_state$previous_values
      refresh_data_table()
      showNotification(
        paste("Rolled back conversion:", unit_conversion_state$previous_label),
        type = "message"
      )
      unit_conversion_state$previous_values <- NULL
      unit_conversion_state$previous_label <- NULL
    })

    upload_raw <- reactive({
      req(input$file)
      req(input$raw_start_row)
      # Set starting row to 1 if input is null, so we don't have to catch empty inputs in validate
      starting_row <- ifelse(
        length(input$raw_start_row) < 1,
        1,
        input$raw_start_row
      )

      ext <- tools::file_ext(input$file$name)
      if (ext == 'xlsx') {
        out <- readxl::read_xlsx(
          input$file$datapath,
          sheet = 1,
          skip = starting_row - 1
        ) |>
          as.data.frame()

        return(out)
      } else if (ext == "csv") {
        # .csv files more complex due to ungraceful handling of non-equal
        #  number of columns and column names by read.table

        # Read data without header, skip to first row below specified header row
        out <- read.csv(
          input$file$datapath,
          header = FALSE,
          skip = starting_row
        )

        # Read in header row and convert to vector
        out_names <- read.csv(
          input$file$datapath,
          header = FALSE,
          nrows = 1,
          skip = starting_row - 1
        ) |>
          unlist() |>
          unname()
        # Apply header rows to data
        names(out) <- out_names

        return(out)
      }
    })

    class_type_choices <- reactive({
      list(
        grade = DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT grade_type_id AS id, grade_type_code AS code, grade_type_description AS description, color_code FROM public.grade_types ORDER BY grade_type_id"
        ),
        approval = DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT approval_type_id AS id, approval_type_code AS code, approval_type_description AS description, color_code FROM public.approval_types ORDER BY approval_type_id"
        ),
        qualifier = DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT qualifier_type_id AS id, qualifier_type_code AS code, qualifier_type_description AS description, color_code FROM public.qualifier_types ORDER BY qualifier_type_id"
        )
      )
    })

    map_modal_state <- reactiveValues(
      step = "columns",
      pending_df = NULL,
      class_values = list(
        grade = character(),
        approval = character(),
        qualifier = character()
      )
    )

    build_df_from_column_mapping <- function() {
      req(input$upload_datetime_col, input$upload_value_col)

      df_mapped <- data.frame(
        datetime = upload_raw()[[input$upload_datetime_col]],
        value = upload_raw()[[input$upload_value_col]]
      )

      if (
        isTruthy(input$upload_grade_col) &&
          input$upload_grade_col %in% names(upload_raw())
      ) {
        df_mapped$grade <- upload_raw()[[input$upload_grade_col]]
      } else {
        df_mapped$grade <- NULL
      }
      if (
        isTruthy(input$upload_approval_col) &&
          input$upload_approval_col %in% names(upload_raw())
      ) {
        df_mapped$approval <- upload_raw()[[input$upload_approval_col]]
      } else {
        df_mapped$approval <- NULL
      }
      if (
        isTruthy(input$upload_qualifier_col) &&
          input$upload_qualifier_col %in% names(upload_raw())
      ) {
        df_mapped$qualifier <- upload_raw()[[input$upload_qualifier_col]]
      } else {
        df_mapped$qualifier <- NULL
      }

      df_mapped
    }

    selected_class_cols <- reactive({
      cols <- c()
      if (isTruthy(input$upload_grade_col)) {
        cols <- c(cols, "grade")
      }
      if (isTruthy(input$upload_approval_col)) {
        cols <- c(cols, "approval")
      }
      if (isTruthy(input$upload_qualifier_col)) {
        cols <- c(cols, "qualifier")
      }
      cols
    })

    # Error checking, all possible conditions of start row and upload_raw
    #  in which the confirm mapping button should be disabled
    observe({
      if (map_modal_state$step != "columns") {
        return()
      }

      target_id <- if (length(selected_class_cols()) > 0) {
        "next_mapping"
      } else {
        "confirm_mapping"
      }

      if (is.null(input$raw_start_row)) {
        shinyjs::disable(target_id)
      } else if (is.na(input$raw_start_row)) {
        shinyjs::disable(target_id)
      } else if (input$raw_start_row < 1) {
        shinyjs::disable(target_id)
      } else if (ncol(upload_raw()) < 2) {
        shinyjs::disable(target_id)
      } else {
        shinyjs::enable(target_id)
      }
    })

    observe({
      if (map_modal_state$step != "class_mapping") {
        return()
      }

      all_mapped <- TRUE
      for (class_name in names(map_modal_state$class_values)) {
        values <- map_modal_state$class_values[[class_name]]
        if (length(values) == 0) {
          next
        }
        for (i in seq_along(values)) {
          if (!isTruthy(input[[paste0("map_", class_name, "_", i)]])) {
            all_mapped <- FALSE
            break
          }
        }
      }

      if (all_mapped) {
        shinyjs::enable('confirm_mapping')
      } else {
        shinyjs::disable('confirm_mapping')
      }
    })

    output$map_col_inputs <- renderUI({
      validate(
        need(
          input$raw_start_row > 0,
          'Invalid header row'
        ),
        need(
          ncol(upload_raw()) >= 2,
          'Uploaded file must have at least two columns (one containing date time, and one containing measurement value)'
        )
      )

      uploaded_names <- names(upload_raw())

      pick_col <- function(candidates, default = "") {
        out <- uploaded_names[tolower(uploaded_names) %in% tolower(candidates)]
        if (length(out) > 0) out[[1]] else default
      }

      choices_optional <- uploaded_names

      tagList(
        selectizeInput(
          ns('upload_datetime_col'),
          'Select the column for datetime:',
          choices = uploaded_names,
          selected = pick_col(
            c('datetime', 'date_time', 'date', 'time'),
            uploaded_names[[1]]
          )
        ),
        selectizeInput(
          ns('upload_value_col'),
          'Select the column for values:',
          choices = uploaded_names,
          selected = pick_col(
            c('value', 'values', 'measurement', 'measured_value'),
            uploaded_names[[2]]
          )
        ),
        if (length(uploaded_names) > 2) {
          div(
            selectizeInput(
              ns('upload_grade_col'),
              'Optional: select the column for grades:',
              choices = choices_optional,
              selected = pick_col(c('grade', 'grades'))
            ),
            selectizeInput(
              ns('upload_approval_col'),
              'Optional: select the column for approvals:',
              choices = choices_optional,
              selected = pick_col(c('approval', 'approvals'))
            ),
            selectizeInput(
              ns('upload_qualifier_col'),
              'Optional: select the column for qualifiers:',
              choices = choices_optional,
              selected = pick_col(c('qualifier', 'qualifiers'))
            )
          )
        }
      )
    })

    parse_datetime <- function(x) {
      if (inherits(x, "POSIXct")) {
        return(x)
      }
      if (inherits(x, "Date")) {
        return(as.POSIXct(x, tz = "UTC"))
      }
      if (is.list(x) && !is.null(x$date)) {
        x <- unlist(x$date, use.names = FALSE)
      }
      x <- trimws(as.character(x))
      # Switch T character with space for ISO like formats
      x <- gsub("T", " ", x)
      x <- gsub("(\\d{2}:\\d{2}:\\d{2})\\.\\d+", "\\1", x)
      x <- gsub("Z$", " +0000", x)
      x <- gsub("\\s+UTC([+-]\\d{2}:?\\d{2})$", " \\1", x)
      x <- gsub("([+-]\\d{2}):(\\d{2})$", "\\1\\2", x)

      lubridate::parse_date_time(
        x,
        orders = c(
          "Ymd HMS",
          "Ymd HM",
          "mdY HMS",
          "mdY HM",
          "Ymd HMS z",
          "Ymd HM z",
          "mdY HMS z",
          "mdY HM z",
          "Ymd IMS p",
          "mdY IMS p",
          "Ymd IM p",
          "mdY IM p"
        ),
        exact = FALSE,
        train = TRUE,
        tz = "UTC"
      )
    }

    prepare_table_data <- function(df) {
      out <- df[, c("datetime", "value")]

      out$datetime <- if (inherits(out$datetime, "POSIXct")) {
        format(out$datetime, "%Y-%m-%d %H:%M:%S")
      } else {
        as.character(out$datetime)
      }

      out$value <- suppressWarnings(as.numeric(out$value))
      for (class_name in c("grade", "approval", "qualifier")) {
        out[[class_name]] <- if (class_name %in% names(df)) {
          as.character(df[[class_name]])
        } else {
          ""
        }
      }
      out
    }

    selected_offset_tz <- function(value, default = input$UTC_offset) {
      fallback <- normalize_input_timezone(
        default,
        default = format_utc_offset(0L)
      )
      normalize_input_timezone(value, default = fallback)
    }

    selected_offset_seconds <- function(value, default = input$UTC_offset) {
      parse_utc_offset_minutes(
        selected_offset_tz(value, default),
        default = "UTC+00:00"
      ) *
        60
    }

    class_offset_tz <- function(class_name) {
      selected_offset_tz(input[[paste0(class_name, "_utc_offset")]])
    }

    table_datetimes_to_utc <- function(x, tz_name = input$UTC_offset) {
      out <- parse_datetime(x)
      valid_idx <- !is.na(out)
      if (any(valid_idx)) {
        out[valid_idx] <- out[valid_idx] -
          selected_offset_seconds(tz_name, default = format_utc_offset(0L))
      }
      attr(out, "tzone") <- "UTC"
      out
    }

    parse_utc_datetime_value <- function(datetime_value) {
      if (is.null(datetime_value) || length(datetime_value) == 0) {
        return(empty_utc_datetime())
      }

      out <- tryCatch(
        suppressWarnings(coerce_utc_datetime(datetime_value)),
        error = function(e) empty_utc_datetime(length(datetime_value))
      )
      needs_parse <- is.na(out)
      if (any(needs_parse)) {
        parsed <- parse_datetime(datetime_value[needs_parse])
        out[needs_parse] <- parsed
      }
      attr(out, "tzone") <- "UTC"
      out
    }

    scalar_utc_datetime_value <- function(datetime_value) {
      out <- parse_utc_datetime_value(datetime_value)
      if (!length(out) || is.na(out[1])) {
        return(empty_utc_datetime())
      }
      out[1]
    }

    input_datetime_has_timezone <- function(datetime_value) {
      if (is.null(datetime_value) || inherits(datetime_value, "POSIXct")) {
        return(TRUE)
      }
      if (is.list(datetime_value) && !is.null(datetime_value$tz)) {
        return(TRUE)
      }
      x <- trimws(as.character(datetime_value))
      any(grepl("(Z|UTC|GMT|[+-]\\d{2}:?\\d{2})$", x))
    }

    scalar_display_datetime_to_utc <- function(datetime_value, tz_name) {
      if (is.list(datetime_value) && !is.null(datetime_value$date)) {
        input_tz <- if (!is.null(datetime_value$tz)) {
          selected_offset_tz(datetime_value$tz[[1]], default = tz_name)
        } else {
          tz_name
        }
        out <- table_datetimes_to_utc(
          unlist(datetime_value$date, use.names = FALSE),
          input_tz
        )
        if (!length(out) || is.na(out[1])) {
          return(empty_utc_datetime())
        }
        return(out[1])
      }
      if (input_datetime_has_timezone(datetime_value)) {
        return(scalar_utc_datetime_value(datetime_value))
      }
      out <- table_datetimes_to_utc(datetime_value, tz_name)
      if (!length(out) || is.na(out[1])) {
        return(empty_utc_datetime())
      }
      out[1]
    }

    format_utc_datetimes_for_display <- function(datetime_value, tz_name) {
      utc_values <- parse_utc_datetime_value(datetime_value)
      out <- rep(NA_character_, length(utc_values))
      valid_idx <- !is.na(utc_values)
      if (any(valid_idx)) {
        display_values <- utc_values[valid_idx] +
          selected_offset_seconds(tz_name, default = format_utc_offset(0L))
        out[valid_idx] <- format(
          display_values,
          "%Y-%m-%d %H:%M:%S",
          tz = "UTC"
        )
      }
      out
    }

    shift_display_datetime_strings <- function(x, from_tz, to_tz) {
      out <- as.character(x)
      utc_values <- table_datetimes_to_utc(x, from_tz)
      valid_idx <- !is.na(utc_values)
      if (any(valid_idx)) {
        out[valid_idx] <- format_utc_datetimes_for_display(
          utc_values[valid_idx],
          to_tz
        )
      }
      out
    }

    uploaded_data_bounds <- function(tz_name = input$UTC_offset) {
      if (nrow(data$df) == 0 || !("datetime" %in% names(data$df))) {
        return(NULL)
      }

      utc_values <- table_datetimes_to_utc(data$df$datetime, input$UTC_offset)
      valid_values <- utc_values[!is.na(utc_values)]
      if (!length(valid_values)) {
        return(NULL)
      }

      tz_name <- selected_offset_tz(tz_name, default = input$UTC_offset)
      start_utc <- min(valid_values)
      end_utc <- max(valid_values)

      list(
        start_utc = start_utc,
        end_utc = end_utc,
        start_display = format_utc_datetimes_for_display(start_utc, tz_name),
        end_display = format_utc_datetimes_for_display(end_utc, tz_name),
        tz = tz_name
      )
    }

    last_preview_bounds_signature <- reactiveVal(NULL)

    update_preview_datetime_bounds <- function() {
      bounds <- uploaded_data_bounds(input$preview_utc_offset)
      if (is.null(bounds)) {
        last_preview_bounds_signature(NULL)
        for (input_id in c("preview_start_datetime", "preview_end_datetime")) {
          shinyWidgets::updateAirDateInput(
            session,
            inputId = input_id,
            clear = TRUE,
            options = list(minDate = FALSE, maxDate = FALSE)
          )
        }
        return(invisible(NULL))
      }

      bounds_signature <- paste(
        as.numeric(bounds$start_utc),
        as.numeric(bounds$end_utc),
        bounds$tz,
        sep = "|"
      )
      if (identical(last_preview_bounds_signature(), bounds_signature)) {
        return(invisible(NULL))
      }

      date_options <- list(
        minDate = bounds$start_utc,
        maxDate = bounds$end_utc
      )
      shinyWidgets::updateAirDateInput(
        session,
        inputId = "preview_start_datetime",
        value = bounds$start_utc,
        tz = air_datetime_widget_timezone(bounds$tz),
        options = date_options
      )
      shinyWidgets::updateAirDateInput(
        session,
        inputId = "preview_end_datetime",
        value = bounds$end_utc,
        tz = air_datetime_widget_timezone(bounds$tz),
        options = date_options
      )
      last_preview_bounds_signature(bounds_signature)
      invisible(NULL)
    }

    uploaded_data_bounds_ui <- function(class_name) {
      bounds <- uploaded_data_bounds(class_offset_tz(class_name))
      if (is.null(bounds)) {
        return(tags$p(
          class = "text-muted small",
          "Uploaded data range unavailable until valid datetime rows exist."
        ))
      }

      tags$div(
        class = "text-muted small mb-2",
        tags$div(tags$strong("Uploaded data range")),
        tags$div(paste("Start:", bounds$start_display)),
        tags$div(paste("End:", bounds$end_display)),
        tags$div(paste("UTC offset:", bounds$tz))
      )
    }

    previous_data_timezone <- reactiveVal(format_utc_offset(0L))

    observeEvent(input$UTC_offset, {
      master_tz <- selected_offset_tz(
        input$UTC_offset,
        default = format_utc_offset(0L)
      )
      previous_tz <- previous_data_timezone()

      if (!identical(previous_tz, master_tz) && nrow(data$df) > 0) {
        data$df$datetime <- shift_display_datetime_strings(
          data$df$datetime,
          previous_tz,
          master_tz
        )
        refresh_data_table()
      }

      previous_data_timezone(master_tz)
      shift_datetime_inputs("delete_cutoff_datetime", master_tz)

      for (input_id in c(
        "preview_utc_offset",
        "delete_utc_offset",
        "approval_utc_offset",
        "grade_utc_offset",
        "qualifier_utc_offset"
      )) {
        updateSelectizeInput(session, input_id, selected = master_tz)
      }
    })

    observeEvent(
      input$preview_utc_offset,
      {
        update_preview_datetime_bounds()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      data$df,
      {
        update_preview_datetime_bounds()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$delete_utc_offset,
      {
        shift_datetime_inputs(
          "delete_cutoff_datetime",
          selected_offset_tz(
            input$delete_utc_offset,
            default = input$UTC_offset
          )
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$approval_utc_offset,
      {
        update_class_modal_datetime_limits("approval")
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$grade_utc_offset,
      {
        update_class_modal_datetime_limits("grade")
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$qualifier_utc_offset,
      {
        update_class_modal_datetime_limits("qualifier")
      },
      ignoreInit = TRUE
    )

    class_ranges <- reactiveValues(
      grade = data.frame(
        code = character(),
        description = character(),
        start_datetime = character(),
        end_datetime = character(),
        stringsAsFactors = FALSE
      ),
      approval = data.frame(
        code = character(),
        description = character(),
        start_datetime = character(),
        end_datetime = character(),
        stringsAsFactors = FALSE
      ),
      qualifier = data.frame(
        code = character(),
        description = character(),
        start_datetime = character(),
        end_datetime = character(),
        stringsAsFactors = FALSE
      )
    )

    ensure_class_cols <- function() {
      for (nm in c("grade", "approval", "qualifier")) {
        if (!(nm %in% names(data$df))) {
          data$df[[nm]] <- ""
        }
      }
    }

    code_to_desc <- function(class_name, code) {
      types <- class_type_choices()[[class_name]]
      idx <- match(as.character(code), as.character(types$code))
      ifelse(is.na(idx), "", as.character(types$description[idx]))
    }

    ranges_from_table_classes <- function(df, class_name) {
      if (!(class_name %in% names(df)) || nrow(df) == 0) {
        return(class_ranges[[class_name]][0, , drop = FALSE])
      }
      dt <- table_datetimes_to_utc(df$datetime, input$UTC_offset)
      vals <- trimws(as.character(df[[class_name]]))
      ok <- !is.na(dt) & nzchar(vals)
      if (!any(ok)) {
        return(class_ranges[[class_name]][0, , drop = FALSE])
      }
      x <- data.table::data.table(datetime = dt[ok], code = vals[ok])
      data.table::setorder(x, datetime)
      x[, run := data.table::rleid(code)]
      out <- x[,
        .(
          code = data.table::first(code),
          start_datetime = format(min(datetime), "%Y-%m-%d %H:%M:%S"),
          end_datetime = format(max(datetime), "%Y-%m-%d %H:%M:%S")
        ),
        by = run
      ]
      out[, description := code_to_desc(class_name, code)]
      out[, run := NULL]
      as.data.frame(out[, .(code, description, start_datetime, end_datetime)])
    }

    validate_ranges <- function(df, class_name) {
      if (nrow(df) == 0) {
        return(character())
      }
      sdt <- parse_utc_datetime_value(df$start_datetime)
      edt <- parse_utc_datetime_value(df$end_datetime)
      msgs <- character()
      bad <- which(is.na(sdt) | is.na(edt) | edt < sdt)
      if (length(bad) > 0) {
        msgs <- c(
          msgs,
          sprintf(
            "Invalid start/end datetime row(s): %s.",
            paste(bad, collapse = ", ")
          )
        )
      }
      valid <- which(!(is.na(sdt) | is.na(edt) | edt < sdt))
      if (length(valid) > 1 && class_name %in% c("grade", "approval")) {
        o <- order(sdt[valid], edt[valid])
        v <- valid[o]
        for (i in seq_along(v)[-1]) {
          if (sdt[v[i]] <= edt[v[i - 1]]) {
            msgs <- c(
              msgs,
              sprintf(
                "Overlapping or touching %s ranges in rows %s and %s.",
                class_name,
                v[i - 1],
                v[i]
              )
            )
          }
        }
      }
      unique(msgs)
    }

    ranges_valid <- reactive({
      vapply(
        c("grade", "approval", "qualifier"),
        function(nm) {
          length(validate_ranges(class_ranges[[nm]], nm)) == 0
        },
        logical(1)
      )
    })

    observe({
      if (!can_insert) {
        shinyjs::disable("upload")
        shinyjs::disable("upload_overwrite_all")
        shinyjs::disable("upload_overwrite_some")
        return()
      }
      if (all(ranges_valid())) {
        shinyjs::enable("upload")
        shinyjs::enable("upload_overwrite_all")
        shinyjs::enable("upload_overwrite_some")
      } else {
        shinyjs::disable("upload")
        shinyjs::disable("upload_overwrite_all")
        shinyjs::disable("upload_overwrite_some")
      }
    })

    output$grade_ranges_warning <- renderUI({
      msgs <- validate_ranges(class_ranges$grade, "grade")
      if (length(msgs) == 0) {
        return(NULL)
      }
      div(style = "color:#b30000;", paste(msgs, collapse = " "))
    })
    output$approval_ranges_warning <- renderUI({
      msgs <- validate_ranges(class_ranges$approval, "approval")
      if (length(msgs) == 0) {
        return(NULL)
      }
      div(style = "color:#b30000;", paste(msgs, collapse = " "))
    })
    output$qualifier_ranges_warning <- renderUI({
      msgs <- validate_ranges(class_ranges$qualifier, "qualifier")
      if (length(msgs) == 0) {
        return(NULL)
      }
      div(style = "color:#b30000;", paste(msgs, collapse = " "))
    })

    sync_table_classes_from_ranges <- function() {
      if (nrow(data$df) == 0) {
        return()
      }
      new_df <- data$df
      for (nm in c("grade", "approval", "qualifier")) {
        if (!(nm %in% names(new_df))) {
          new_df[[nm]] <- ""
        }
      }
      dt <- table_datetimes_to_utc(new_df$datetime, input$UTC_offset)
      if (!any(!is.na(dt))) {
        data$df <- new_df
        refresh_data_table()
        return()
      }
      for (nm in c("grade", "approval", "qualifier")) {
        new_df[[nm]] <- ""
        rr <- class_ranges[[nm]]
        if (nrow(rr) == 0) {
          next
        }
        sdt <- parse_utc_datetime_value(rr$start_datetime)
        edt <- parse_utc_datetime_value(rr$end_datetime)
        for (i in seq_len(nrow(rr))) {
          if (is.na(sdt[i]) || is.na(edt[i])) {
            next
          }
          idx <- which(!is.na(dt) & dt >= sdt[i] & dt <= edt[i])
          if (length(idx) == 0) {
            next
          }
          if (nm == "qualifier") {
            existing <- trimws(as.character(new_df[[nm]][idx]))
            new_df[[nm]][idx] <- ifelse(
              nzchar(existing),
              paste0(existing, ";", rr$code[i]),
              rr$code[i]
            )
          } else {
            new_df[[nm]][idx] <- rr$code[i]
          }
        }
      }
      if (!isTRUE(all.equal(data$df, new_df, check.attributes = FALSE))) {
        data$df <- new_df
        refresh_data_table()
      }
    }

    range_table <- function(class_name) {
      out <- class_ranges[[class_name]]
      out$description <- code_to_desc(class_name, out$code)
      if (nrow(out) > 0) {
        tz_name <- class_offset_tz(class_name)
        out$start_datetime <- format_utc_datetimes_for_display(
          out$start_datetime,
          tz_name
        )
        out$end_datetime <- format_utc_datetimes_for_display(
          out$end_datetime,
          tz_name
        )
      }
      out
    }

    output$grade_ranges_table <- DT::renderDT(
      {
        DT::datatable(
          range_table("grade"),
          selection = "single",
          rownames = FALSE,
          options = list(scrollX = TRUE)
        )
      },
      server = FALSE
    )
    output$approval_ranges_table <- DT::renderDT(
      {
        DT::datatable(
          range_table("approval"),
          selection = "single",
          rownames = FALSE,
          options = list(scrollX = TRUE)
        )
      },
      server = FALSE
    )
    output$qualifier_ranges_table <- DT::renderDT(
      {
        DT::datatable(
          range_table("qualifier"),
          selection = "single",
          rownames = FALSE,
          options = list(scrollX = TRUE)
        )
      },
      server = FALSE
    )

    class_modal_bounds <- function(class_name) {
      uploaded_data_bounds(class_offset_tz(class_name))
    }

    class_modal_date_options <- function(bounds) {
      if (is.null(bounds)) {
        return(list())
      }
      list(
        minDate = bounds$start_utc,
        maxDate = bounds$end_utc
      )
    }

    class_modal_datetime_value <- function(class_name, input_id) {
      scalar_display_datetime_to_utc(
        input[[input_id]],
        class_offset_tz(class_name)
      )
    }

    update_class_modal_datetime_limits <- function(class_name) {
      bounds <- class_modal_bounds(class_name)
      if (is.null(bounds)) {
        return(invisible(NULL))
      }

      update_one <- function(input_id, fallback) {
        current_value <- class_modal_datetime_value(class_name, input_id)
        if (is.na(current_value)) {
          current_value <- fallback
        }
        current_value <- min(
          max(current_value, bounds$start_utc),
          bounds$end_utc
        )
        shinyWidgets::updateAirDateInput(
          session,
          inputId = input_id,
          value = current_value,
          tz = air_datetime_widget_timezone(bounds$tz),
          options = class_modal_date_options(bounds)
        )
      }

      update_one(paste0(class_name, "_modal_start"), bounds$start_utc)
      update_one(paste0(class_name, "_modal_end"), bounds$end_utc)
      invisible(NULL)
    }

    range_inside_data_bounds <- function(st, en, bounds) {
      if (is.null(bounds)) {
        return(FALSE)
      }
      !is.na(st) &&
        !is.na(en) &&
        st >= bounds$start_utc &&
        en <= bounds$end_utc
    }

    open_range_modal <- function(
      class_name,
      mode = c("add", "edit"),
      row_idx = NULL
    ) {
      mode <- match.arg(mode)
      rows <- class_ranges[[class_name]]
      edit_row <- if (
        mode == "edit" && !is.null(row_idx) && nrow(rows) >= row_idx
      ) {
        rows[row_idx, ]
      } else {
        data.frame(code = "", start_datetime = "", end_datetime = "")
      }
      bounds <- class_modal_bounds(class_name)
      start_value <- if (nzchar(edit_row$start_datetime)) {
        scalar_utc_datetime_value(edit_row$start_datetime)
      } else if (!is.null(bounds)) {
        bounds$start_utc
      } else {
        NULL
      }
      end_value <- if (nzchar(edit_row$end_datetime)) {
        scalar_utc_datetime_value(edit_row$end_datetime)
      } else if (!is.null(bounds)) {
        bounds$end_utc
      } else {
        NULL
      }
      types <- class_type_choices()[[class_name]]
      showModal(modalDialog(
        title = paste(
          ifelse(mode == "add", "Add", "Edit"),
          class_name,
          "range"
        ),
        selectizeInput(
          ns(paste0(class_name, "_modal_code")),
          "Level",
          choices = stats::setNames(
            types$code,
            paste0(types$code, ": ", types$description)
          ),
          selected = edit_row$code,
          multiple = FALSE
        ),
        shinyWidgets::airDatepickerInput(
          ns(paste0(class_name, "_modal_start")),
          "Start datetime",
          value = start_value,
          range = FALSE,
          multiple = FALSE,
          timepicker = TRUE,
          update_on = "change",
          tz = air_datetime_widget_timezone(isolate(class_offset_tz(
            class_name
          ))),
          minDate = if (!is.null(bounds)) bounds$start_utc else NULL,
          maxDate = if (!is.null(bounds)) bounds$end_utc else NULL,
          timepickerOpts = shinyWidgets::timepickerOptions(
            minutesStep = 15,
            timeFormat = "HH:mm"
          )
        ),
        shinyWidgets::airDatepickerInput(
          ns(paste0(class_name, "_modal_end")),
          "End datetime",
          value = end_value,
          range = FALSE,
          multiple = FALSE,
          timepicker = TRUE,
          update_on = "change",
          tz = air_datetime_widget_timezone(isolate(class_offset_tz(
            class_name
          ))),
          minDate = if (!is.null(bounds)) bounds$start_utc else NULL,
          maxDate = if (!is.null(bounds)) bounds$end_utc else NULL,
          timepickerOpts = shinyWidgets::timepickerOptions(
            minutesStep = 15,
            timeFormat = "HH:mm"
          )
        ),
        uploaded_data_bounds_ui(class_name),
        tags$div(
          class = "d-flex gap-2 flex-wrap mb-3",
          actionButton(
            ns(paste0(class_name, "_modal_use_data_start")),
            "Use data start"
          ),
          actionButton(
            ns(paste0(class_name, "_modal_use_data_end")),
            "Use data end"
          )
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns(paste0("save_", class_name, "_modal")), "Save")
        )
      ))
      session$userData[[paste0("edit_", class_name, "_row")]] <- row_idx
    }

    for (nm in c("grade", "approval", "qualifier")) {
      local({
        class_name <- nm
        observeEvent(input[[paste0("add_", class_name, "_range")]], {
          open_range_modal(class_name, "add")
        })
        observeEvent(input[[paste0("edit_", class_name, "_range")]], {
          idx <- input[[paste0(class_name, "_ranges_table_rows_selected")]]
          req(length(idx) == 1)
          open_range_modal(class_name, "edit", idx[[1]])
        })
        observeEvent(input[[paste0("delete_", class_name, "_range")]], {
          idx <- input[[paste0(class_name, "_ranges_table_rows_selected")]]
          req(length(idx) == 1)
          class_ranges[[class_name]] <- class_ranges[[class_name]][
            -idx[[1]],
            ,
            drop = FALSE
          ]
          sync_table_classes_from_ranges()
        })
        observeEvent(input[[paste0(class_name, "_modal_use_data_start")]], {
          bounds <- uploaded_data_bounds(class_offset_tz(class_name))
          if (is.null(bounds)) {
            return()
          }
          shinyWidgets::updateAirDateInput(
            session,
            inputId = paste0(class_name, "_modal_start"),
            value = bounds$start_utc,
            tz = air_datetime_widget_timezone(bounds$tz),
            options = class_modal_date_options(bounds)
          )
        })
        observeEvent(input[[paste0(class_name, "_modal_use_data_end")]], {
          bounds <- uploaded_data_bounds(class_offset_tz(class_name))
          if (is.null(bounds)) {
            return()
          }
          shinyWidgets::updateAirDateInput(
            session,
            inputId = paste0(class_name, "_modal_end"),
            value = bounds$end_utc,
            tz = air_datetime_widget_timezone(bounds$tz),
            options = class_modal_date_options(bounds)
          )
        })
        observeEvent(input[[paste0("save_", class_name, "_modal")]], {
          code <- as.character(input[[paste0(class_name, "_modal_code")]])
          st_value <- class_modal_datetime_value(
            class_name,
            paste0(class_name, "_modal_start")
          )
          en_value <- class_modal_datetime_value(
            class_name,
            paste0(class_name, "_modal_end")
          )
          bounds <- class_modal_bounds(class_name)

          if (
            is.na(st_value) ||
              is.na(en_value) ||
              en_value < st_value
          ) {
            showNotification(
              paste("Invalid", class_name, "start/end datetime."),
              type = "error",
              duration = 8
            )
            return()
          }

          if (!range_inside_data_bounds(st_value, en_value, bounds)) {
            showNotification(
              paste(
                "The",
                class_name,
                "range must stay within the uploaded/entered data range."
              ),
              type = "error",
              duration = 8
            )
            return()
          }

          st <- if (length(st_value) && !is.na(st_value[[1]])) {
            format(st_value[[1]], "%Y-%m-%d %H:%M:%S", tz = "UTC")
          } else {
            ""
          }
          en <- if (length(en_value) && !is.na(en_value[[1]])) {
            format(en_value[[1]], "%Y-%m-%d %H:%M:%S", tz = "UTC")
          } else {
            ""
          }
          new_row <- data.frame(
            code = code,
            description = code_to_desc(class_name, code),
            start_datetime = st,
            end_datetime = en,
            stringsAsFactors = FALSE
          )
          rows <- class_ranges[[class_name]]
          edit_idx <- session$userData[[paste0("edit_", class_name, "_row")]]
          if (
            !is.null(edit_idx) && !is.na(edit_idx) && nrow(rows) >= edit_idx
          ) {
            rows[edit_idx, ] <- new_row
          } else {
            rows <- rbind(rows, new_row)
          }

          range_msgs <- validate_ranges(rows, class_name)
          if (length(range_msgs) > 0) {
            showNotification(
              paste(range_msgs, collapse = " "),
              type = "error",
              duration = 10
            )
            return()
          }

          class_ranges[[class_name]] <- rows
          removeModal()
          sync_table_classes_from_ranges()
        })
      })
    }

    output$map_modal_body <- renderUI({
      if (map_modal_state$step == "columns") {
        tagList(
          'Identify which columns represent date-time and value (and optionally grade/approval/qualifier):',
          hr(),
          numericInput(ns('raw_start_row'), label = 'Header Row', value = 1) |>
            tooltip("The row number which contains your data's column names"),
          uiOutput(ns('map_col_inputs'))
        )
      } else {
        mapping_ui <- list(
          tags$p('Map your uploaded classes to database classes.')
        )

        db_types <- class_type_choices()

        for (class_name in names(map_modal_state$class_values)) {
          class_vals <- map_modal_state$class_values[[class_name]]
          if (length(class_vals) == 0) {
            next
          }

          db_df <- db_types[[class_name]]
          db_choices <- stats::setNames(
            as.character(db_df$code),
            paste0(db_df$code, ": ", db_df$description)
          )

          mapping_ui[[length(mapping_ui) + 1]] <- tags$h5(
            paste0(
              toupper(substring(class_name, 1, 1)),
              substring(class_name, 2),
              " mapping"
            )
          )

          for (i in seq_along(class_vals)) {
            mapping_ui[[length(mapping_ui) + 1]] <- selectizeInput(
              ns(paste0("map_", class_name, "_", i)),
              paste0("Uploaded '", class_vals[[i]], "' maps to:"),
              choices = db_choices,
              multiple = TRUE,
              options = list(
                maxItems = 1,
                placeholder = "Select class to map to"
              )
            )
          }
        }

        do.call(tagList, mapping_ui)
      }
    })

    output$map_modal_footer <- renderUI({
      if (map_modal_state$step == "columns") {
        button_label <- if (length(selected_class_cols()) > 0) {
          "Next"
        } else {
          "Confirm"
        }
        button_id <- if (length(selected_class_cols()) > 0) {
          "next_mapping"
        } else {
          "confirm_mapping"
        }

        tagList(
          modalButton('Cancel'),
          actionButton(ns(button_id), button_label)
        )
      } else {
        tagList(
          modalButton('Cancel'),
          actionButton(ns('confirm_mapping'), 'Confirm')
        )
      }
    })

    # Store modal to be shown upon user uploading .csv or .xlsx
    map_col_modal <- modalDialog(
      title = 'Identify columns',
      uiOutput(ns('map_modal_body')),
      easyClose = FALSE,
      footer = uiOutput(ns('map_modal_footer'))
    )

    # Show modal when user adds file
    observeEvent(input$file, {
      req(input$file)
      map_modal_state$step <- "columns"
      map_modal_state$pending_df <- NULL
      map_modal_state$class_values <- list(
        grade = character(),
        approval = character(),
        qualifier = character()
      )
      showModal(map_col_modal)
    })

    observeEvent(input$next_mapping, {
      df_mapped <- build_df_from_column_mapping()
      map_modal_state$pending_df <- df_mapped
      sanitize_class_vals <- function(x) {
        vals <- trimws(as.character(x))
        sort(unique(vals[!is.na(vals) & nzchar(vals)]))
      }
      map_modal_state$class_values <- list(
        grade = sanitize_class_vals(df_mapped$grade),
        approval = sanitize_class_vals(df_mapped$approval),
        qualifier = sanitize_class_vals(df_mapped$qualifier)
      )
      map_modal_state$step <- "class_mapping"
    })

    observeEvent(
      input$confirm_mapping,
      {
        removeModal() # Close the modal dialog
        df_mapped <- if (map_modal_state$step == "class_mapping") {
          req(map_modal_state$pending_df)
          out <- map_modal_state$pending_df

          for (class_name in names(map_modal_state$class_values)) {
            class_vals <- map_modal_state$class_values[[class_name]]
            if (length(class_vals) == 0 || !(class_name %in% names(out))) {
              next
            }

            mapped_values <- vapply(
              seq_along(class_vals),
              function(i) {
                as.character(input[[paste0("map_", class_name, "_", i)]])
              },
              character(1)
            )
            names(mapped_values) <- class_vals

            current_vals <- trimws(as.character(out[[class_name]]))
            non_missing <- !is.na(current_vals) & nzchar(current_vals)
            out[[class_name]][non_missing] <- unname(
              mapped_values[current_vals[non_missing]]
            )
          }
          out
        } else {
          build_df_from_column_mapping()
        }

        unit_conversion_state$previous_values <- NULL
        unit_conversion_state$previous_label <- NULL
        data$df <- prepare_table_data(df_mapped)
        if ("grade" %in% names(df_mapped)) {
          data$df$grade <- as.character(df_mapped$grade)
        }
        if ("approval" %in% names(df_mapped)) {
          data$df$approval <- as.character(df_mapped$approval)
        }
        if ("qualifier" %in% names(df_mapped)) {
          data$df$qualifier <- as.character(df_mapped$qualifier)
        }
        class_ranges$grade <- ranges_from_table_classes(data$df, "grade")
        class_ranges$approval <- ranges_from_table_classes(data$df, "approval")
        class_ranges$qualifier <- ranges_from_table_classes(
          data$df,
          "qualifier"
        )
        sync_table_classes_from_ranges()
        refresh_data_table()
      },
      ignoreInit = TRUE
    )

    new_data_row <- function() {
      data.frame(
        datetime = format_utc_datetimes_for_display(
          coerce_utc_datetime(Sys.time()),
          selected_offset_tz(input$UTC_offset)
        )[[1]],
        value = NA_real_,
        grade = "",
        approval = "",
        qualifier = "",
        stringsAsFactors = FALSE
      )
    }

    add_row_relative_to_selection <- function(position = c("above", "below")) {
      position <- match.arg(position)
      if (nrow(data$df) == 0) {
        data$df <- new_data_row()
        refresh_data_table()
        showNotification("Table was empty; added a new row.", type = "message")
        return(invisible(NULL))
      }

      selected <- input$data_table_rows_selected
      if (length(selected) < 1) {
        showNotification(
          "Select one row first to add above or below.",
          type = "error"
        )
        return(invisible(NULL))
      }
      if (length(selected) > 1) {
        if (position == "above") {
          showNotification(
            "Multiple rows selected; using the first selected row.",
            type = "warning"
          )
          anchor <- selected[[1]]
          insert_idx <- anchor
        } else {
          showNotification(
            "Multiple rows selected; using the last selected row.",
            type = "warning"
          )
          anchor <- selected[[length(selected)]]
          insert_idx <- anchor + 1
        }
      } else {
        anchor <- selected[[1]]
        insert_idx <- if (position == "above") {
          anchor
        } else {
          anchor + 1
        }
      }

      top <- if (insert_idx > 1) {
        data$df[seq_len(insert_idx - 1), , drop = FALSE]
      } else {
        data$df[0, , drop = FALSE]
      }
      bottom <- if (insert_idx <= nrow(data$df)) {
        data$df[insert_idx:nrow(data$df), , drop = FALSE]
      } else {
        data$df[0, , drop = FALSE]
      }
      data$df <- rbind(top, new_data_row(), bottom)
      refresh_data_table()
    }

    observeEvent(input$add_row, {
      if (!identical(input$entry_mode, "manual")) {
        return()
      }
      data$df <- rbind(data$df, new_data_row())
      refresh_data_table()
    })

    observeEvent(input$add_row_above, {
      add_row_relative_to_selection("above")
    })

    observeEvent(input$add_row_below, {
      add_row_relative_to_selection("below")
    })

    observeEvent(
      list(input$delete_rows_table, input$delete_rows_accordion),
      {
        click_count <- sum(
          as.numeric(c(input$delete_rows_table, input$delete_rows_accordion)),
          na.rm = TRUE
        )
        if (click_count < 1) {
          return()
        }

        if (!identical(input$entry_mode, "manual")) {
          showNotification(
            "Selected row deletion is available for manual entry only.",
            type = "message"
          )
          return()
        }
        req(input$data_table_rows_selected)
        data$df <- data$df[-input$data_table_rows_selected, , drop = FALSE]
        refresh_data_table()
      },
      ignoreInit = TRUE
    )

    apply_datetime_cutoff <- function(mode = c("before", "after")) {
      mode <- match.arg(mode)
      if (nrow(data$df) == 0) {
        showNotification("No rows to delete.", type = "message")
        return(invisible(NULL))
      }

      cutoff <- scalar_utc_datetime(input$delete_cutoff_datetime)
      if (is.na(cutoff)) {
        showNotification(
          "Invalid cutoff datetime.",
          type = "error"
        )
        return(invisible(NULL))
      }

      parsed_dt <- table_datetimes_to_utc(data$df$datetime, input$UTC_offset)
      if (any(is.na(parsed_dt))) {
        showNotification(
          "Cannot apply cutoff: one or more datetime values in the table are invalid.",
          type = "error"
        )
        return(invisible(NULL))
      }

      keep_idx <- if (mode == "before") {
        parsed_dt >= cutoff
      } else {
        parsed_dt <= cutoff
      }
      removed_n <- sum(!keep_idx)
      data$df <- data$df[keep_idx, , drop = FALSE]
      refresh_data_table()
      showNotification(
        sprintf("Removed %s row(s).", removed_n),
        type = "message"
      )
    }

    observeEvent(input$delete_before_datetime, {
      apply_datetime_cutoff("before")
    })

    observeEvent(input$delete_after_datetime, {
      apply_datetime_cutoff("after")
    })

    class_code_choices <- function(class_name) {
      types <- class_type_choices()[[class_name]]
      out <- as.character(types$code)
      stats::setNames(
        c("", out),
        c("", paste0(out, " - ", types$description))
      )
    }

    table_cell_input <- function(row, col, value) {
      value <- if (is.na(value)) "" else as.character(value)
      escaped_value <- htmltools::htmlEscape(value, attribute = TRUE)

      if (col %in% c("grade", "approval", "qualifier")) {
        choices <- class_code_choices(col)
        if (!(value %in% unname(choices))) {
          value <- ""
        }
        options <- paste0(
          vapply(
            seq_along(choices),
            function(i) {
              selected <- if (identical(unname(choices)[[i]], value)) {
                " selected"
              } else {
                ""
              }
              paste0(
                "<option value=\"",
                htmltools::htmlEscape(unname(choices)[[i]], attribute = TRUE),
                "\"",
                selected,
                ">",
                htmltools::htmlEscape(names(choices)[[i]]),
                "</option>"
              )
            },
            character(1)
          ),
          collapse = ""
        )
        return(paste0(
          "<select class=\"cont-data-cell cont-data-select\" style=\"width:100%;min-width:110px;box-sizing:border-box;\" data-row=\"",
          row,
          "\" data-col=\"",
          col,
          "\">",
          options,
          "</select>"
        ))
      }

      input_type <- if (identical(col, "value")) "number" else "text"
      step_attr <- if (identical(col, "value")) " step=\"any\"" else ""
      paste0(
        "<input class=\"cont-data-cell\" style=\"width:100%;min-width:110px;box-sizing:border-box;\" type=\"",
        input_type,
        "\"",
        step_attr,
        " data-row=\"",
        row,
        "\" data-col=\"",
        col,
        "\" value=\"",
        escaped_value,
        "\">"
      )
    }

    display_table_data <- reactive({
      table_render_tick()
      df <- isolate(data$df)
      if (nrow(df) == 0) {
        return(list(
          data = df,
          editable = identical(input$entry_mode, "manual"),
          preview = FALSE
        ))
      }

      if (!identical(input$entry_mode, "manual")) {
        preview_rows_per_end <- 10L
        total_rows <- nrow(df)
        row_idx <- if (total_rows <= preview_rows_per_end * 2L) {
          seq_len(total_rows)
        } else {
          c(
            seq_len(preview_rows_per_end),
            seq.int(total_rows - preview_rows_per_end + 1L, total_rows)
          )
        }

        preview_df <- df[row_idx, , drop = FALSE]
        preview_df <- data.frame(
          row = row_idx,
          preview_df,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )

        return(list(
          data = preview_df,
          editable = FALSE,
          preview = total_rows > nrow(preview_df),
          total_rows = total_rows,
          displayed_rows = nrow(preview_df)
        ))
      }

      for (col in names(df)) {
        df[[col]] <- vapply(
          seq_len(nrow(df)),
          function(i) table_cell_input(i, col, df[[col]][[i]]),
          character(1)
        )
      }
      list(
        data = df,
        editable = TRUE,
        preview = FALSE
      )
    })

    output$data_table_note <- renderUI({
      table_state <- display_table_data()
      if (isTRUE(table_state$editable)) {
        return(tags$div(
          "Hint: type directly in the table cells. Press Enter or leave the cell to save."
        ))
      }
      if (nrow(table_state$data) == 0) {
        return(NULL)
      }

      if (isTRUE(table_state$preview)) {
        return(tags$div(
          class = "text-muted small",
          sprintf(
            "Showing the first and last %s rows of %s uploaded rows. Use the preview plot for full-data review.",
            table_state$displayed_rows / 2L,
            format(table_state$total_rows, big.mark = ",")
          )
        ))
      }

      tags$div(
        class = "text-muted small",
        "Uploaded data are shown read-only. Use the preview plot for review."
      )
    })

    output$data_table <- DT::renderDT(
      {
        table_state <- display_table_data()
        DT::datatable(
          table_state$data,
          escape = !isTRUE(table_state$editable),
          selection = if (isTRUE(table_state$editable)) {
            list(
              mode = "multiple",
              target = "row",
              selector = "td:first-child"
            )
          } else {
            "none"
          },
          options = list(
            scrollX = TRUE,
            ordering = FALSE,
            columnDefs = list(list(targets = "_all", orderable = FALSE))
          ),
          callback = if (isTRUE(table_state$editable)) {
            htmlwidgets::JS(
              sprintf(
                "var ns = '%s';
                table.on('change', '.cont-data-cell', function() {
                  Shiny.setInputValue(ns + 'data_table_cell_update', {
                    row: parseInt(this.dataset.row, 10),
                    col: this.dataset.col,
                    value: this.value,
                    nonce: Math.random()
                  }, {priority: 'event'});
                });
                table.on('keydown', '.cont-data-cell', function(e) {
                  if (e.key === 'Enter') {
                    e.preventDefault();
                    $(this).trigger('change');
                    this.blur();
                  }
                });",
                ns("")
              ),
              "table.on('draw.dt', function() {",
              "  table.$('.cont-data-cell').css({",
              "    'width': '100%',",
              "    'min-width': '110px',",
              "    'box-sizing': 'border-box'",
              "  });",
              "});"
            )
          } else {
            htmlwidgets::JS("")
          },
          rownames = FALSE
        )
      },
      server = FALSE
    )

    observeEvent(input$data_table_cell_update, {
      if (!identical(input$entry_mode, "manual")) {
        return()
      }

      info <- input$data_table_cell_update
      row <- as.integer(info$row)
      col <- as.character(info$col)
      if (
        is.na(row) ||
          row < 1 ||
          row > nrow(data$df) ||
          !(col %in% names(data$df))
      ) {
        return()
      }

      value <- as.character(info$value)
      if (identical(col, "value")) {
        data$df[[col]] <- as.character(data$df[[col]])
        data$df[[col]][row] <- value
      } else if (col %in% c("grade", "approval", "qualifier")) {
        allowed <- unname(class_code_choices(col))
        data$df[[col]][row] <- if (value %in% allowed) value else ""
        class_ranges[[col]] <- ranges_from_table_classes(data$df, col)
      } else {
        data$df[[col]][row] <- value
      }
    })

    plot_data <- reactiveVal(NULL)
    last_plot_signature <- reactiveVal(NULL)

    current_plot_signature <- reactive({
      list(
        timeseries_id = timeseries(),
        df = data$df,
        grade = class_ranges$grade,
        approval = class_ranges$approval,
        qualifier = class_ranges$qualifier,
        preview_historic_range = input$preview_historic_range,
        preview_start_datetime = input$preview_start_datetime,
        preview_end_datetime = input$preview_end_datetime,
        preview_utc_offset = input$preview_utc_offset
      )
    })

    preview_request <- reactive({
      req(timeseries())
      req(nrow(data$df) > 0)

      parsed_dt <- table_datetimes_to_utc(data$df$datetime, input$UTC_offset)
      parsed_val <- suppressWarnings(as.numeric(data$df$value))
      valid_idx <- !(is.na(parsed_dt) | is.na(parsed_val))
      req(any(valid_idx))

      df_new <- data$df[valid_idx, , drop = FALSE]
      df_new$datetime <- parsed_dt[valid_idx]
      df_new$value <- parsed_val[valid_idx]
      df_new$source <- "New upload"
      preview_offset_seconds <- selected_offset_seconds(
        input$preview_utc_offset,
        default = input$UTC_offset
      )
      preview_tz <- selected_offset_tz(
        input$preview_utc_offset,
        default = input$UTC_offset
      )

      if ("grade" %in% names(df_new)) {
        df_new$grade <- as.character(df_new$grade)
      }
      if ("approval" %in% names(df_new)) {
        df_new$approval <- as.character(df_new$approval)
      }
      if ("qualifier" %in% names(df_new)) {
        df_new$qualifier <- as.character(df_new$qualifier)
      }

      data_start <- min(df_new$datetime, na.rm = TRUE)
      data_end <- max(df_new$datetime, na.rm = TRUE)
      range_start <- data_start
      range_end <- data_end

      custom_start <- scalar_utc_datetime_value(input$preview_start_datetime)
      if (!is.na(custom_start)) {
        range_start <- min(max(custom_start, data_start), data_end)
      }
      custom_end <- scalar_utc_datetime_value(input$preview_end_datetime)
      if (!is.na(custom_end)) {
        range_end <- min(max(custom_end, data_start), data_end)
      }
      if (range_end < range_start) {
        range_start <- data_start
        range_end <- data_end
      }

      in_preview_range <- df_new$datetime >= range_start &
        df_new$datetime <= range_end
      df_new <- df_new[in_preview_range, , drop = FALSE]
      req(nrow(df_new) > 0)

      list(
        timeseries_id = timeseries(),
        new_data = df_new,
        range_start = range_start,
        range_end = range_end,
        display_offset_seconds = preview_offset_seconds,
        display_tz = preview_tz,
        show_historic_range = isTRUE(input$preview_historic_range),
        class_ranges = list(
          grade = class_ranges$grade,
          approval = class_ranges$approval,
          qualifier = class_ranges$qualifier
        ),
        class_types = class_type_choices(),
        config = session$userData$config,
        signature = current_plot_signature()
      )
    })

    output$plot_refresh_warning <- renderUI({
      if (is.null(last_plot_signature())) {
        return(NULL)
      }
      changed <- !isTRUE(all.equal(
        last_plot_signature(),
        current_plot_signature(),
        check.attributes = FALSE
      ))
      if (!changed) {
        return(NULL)
      }
      div(
        style = "color:#b30000;margin-top:6px;",
        "Newly entered data or preview settings have changed since the last refresh plot action."
      )
    })

    make_preview_plot <- function(pv) {
      db_config <- pv$config
      pv$config <- NULL
      con <- AquaConnect(
        name = db_config$dbName,
        host = db_config$dbHost,
        port = db_config$dbPort,
        username = db_config$dbUser,
        password = db_config$dbPass,
        silent = TRUE
      )
      db_config <- NULL
      on.exit(
        {
          if (!is.null(con) && DBI::dbIsValid(con)) {
            DBI::dbDisconnect(con)
          }
        },
        add = TRUE
      )

      extra <- dbGetQueryDT(
        con,
        "SELECT datetime, value_corrected AS value FROM continuous.measurements_continuous_corrected($1, $2, $3)",
        params = list(pv$timeseries_id, pv$range_start, pv$range_end)
      )

      if (nrow(extra) == 0) {
        extra <- NULL
      }

      hist_out <- NULL
      if (isTRUE(pv$show_historic_range)) {
        # Add a day to the end for the historic query to ensure enough ribbon.
        hist_out <- dbGetQueryDT(
          con,
          "SELECT date AS datetime, min, max, q25, q75 FROM continuous.measurements_calculated_daily WHERE timeseries_id = $1 AND date >= $2 AND date <= $3",
          params = list(pv$timeseries_id, pv$range_start, pv$range_end + 86400)
        )
      }

      pv$new_data$datetime <- pv$new_data$datetime + pv$display_offset_seconds
      if (!is.null(extra)) {
        extra$datetime <- coerce_utc_datetime(extra$datetime) +
          pv$display_offset_seconds
      }
      if (!is.null(hist_out) && nrow(hist_out) > 0) {
        hist_out$datetime <- coerce_utc_datetime(hist_out$datetime) +
          pv$display_offset_seconds
      }

      parameter <- dbGetQueryDT(
        con,
        paste(
          "SELECT p.param_name,",
          ac_parameter_unit_select_sql(
            con,
            "p",
            "unit",
            matrix_state_alias = "ts",
            media_alias = "ts"
          ),
          ", p.plot_default_y_orientation",
          "FROM public.parameters p",
          "JOIN continuous.timeseries ts ON p.parameter_id = ts.parameter_id",
          "WHERE ts.timeseries_id = $1"
        ),
        params = list(pv$timeseries_id)
      )

      pv$db <- extra
      pv$historic <- hist_out
      pv$parameter <- parameter
      class_ranges <- pv$class_ranges

      # Start with the range ribbons. Like in plotTimeseries, create ranges within the historic range data so that discontinuous range data doesn't connect across gaps.
      historic_range <- FALSE
      if (!is.null(pv$historic)) {
        pv$historic[,
          has_stats := !is.na(q25) & !is.na(q75) & !is.na(min) & !is.na(max)
        ]

        # Create a run id that increments each time has_stats changes
        pv$historic[, run := data.table::rleid(has_stats)]

        # Keep only runs with data
        range_runs <- split(
          pv$historic[has_stats == TRUE],
          by = "run",
          keep.by = FALSE
        )
        if (length(range_runs) > 0) {
          historic_range <- TRUE
        }
      }

      plot <- plotly::plot_ly()

      if (historic_range) {
        for (rd in range_runs) {
          plot <- plot |>
            plotly::add_ribbons(
              data = rd,
              x = ~datetime,
              ymin = ~q25,
              ymax = ~q75,
              name = "IQR",
              color = I("#5f9da6"),
              line = list(width = 0.2),
              hoverinfo = "text",
              text = ~ paste0(
                "Q25: ",
                round(q25, 2),
                ", Q75: ",
                round(q75, 2),
                " (",
                as.Date(datetime),
                ")"
              ),
              showlegend = FALSE
            ) |>
            plotly::add_ribbons(
              data = rd,
              x = ~datetime,
              ymin = ~min,
              ymax = ~max,
              name = "Historic",
              color = I("#D4ECEF"),
              line = list(width = 0.2),
              hoverinfo = "text",
              text = ~ paste0(
                "Min: ",
                round(min, 2),
                ", Max: ",
                round(max, 2),
                " (",
                as.Date(datetime),
                ")"
              ),
              showlegend = FALSE
            )
        }

        # Add *visible* dummy legend keys (one point is enough)
        key_rd <- range_runs[[1]][1]

        plot <- plot |>
          plotly::add_ribbons(
            data = key_rd,
            x = ~datetime,
            ymin = ~q25,
            ymax = ~q75,
            name = "IQR",
            color = I("#5f9da6"),
            line = list(width = 0.2),
            hoverinfo = "none",
            showlegend = TRUE
          ) |>
          plotly::add_ribbons(
            data = key_rd,
            x = ~datetime,
            ymin = ~min,
            ymax = ~max,
            name = "Historic",
            color = I("#D4ECEF"),
            line = list(width = 0.2),
            hoverinfo = "none",
            showlegend = TRUE
          )
      }

      # Now add in the existing data
      if (!is.null(pv$db)) {
        plot <- plot |>
          plotly::add_trace(
            data = pv$db,
            x = ~datetime,
            y = ~value,
            type = "scatter",
            mode = "lines",
            line = list(width = 2.5),
            name = "Existing corrected",
            color = I("#fa9906ff"),
            hoverinfo = "text",
            text = ~ paste0(
              pv$parameter$param_name,
              ": ",
              round(.data$value, 4),
              " (",
              .data$datetime,
              ")"
            )
          )
      }

      # Finally, add the new data
      plot <- plot |>
        plotly::add_trace(
          data = pv$new_data,
          x = ~datetime,
          y = ~value,
          type = "scatter",
          mode = "lines",
          line = list(width = 2.5),
          name = "New upload",
          color = I("#00454e"),
          hoverinfo = "text",
          text = ~ paste0(
            pv$parameter$param_name,
            ": ",
            round(.data$value, 4),
            " (",
            .data$datetime,
            ")"
          )
        )

      # Add class bands (grade/approval/qualifier) for newly entered data
      has_class_bands <- nrow(class_ranges$grade) > 0 ||
        nrow(class_ranges$approval) > 0 ||
        nrow(class_ranges$qualifier) > 0
      if (has_class_bands && nrow(pv$new_data) > 0) {
        mindt <- min(pv$new_data$datetime, na.rm = TRUE)
        maxdt <- max(pv$new_data$datetime, na.rm = TRUE)
        type_map <- pv$class_types
        poly_list <- list()
        new_data_dt <- sort(unique(pv$new_data$datetime))
        approval_y <- NULL
        grade_y <- NULL
        qualifier_y <- NULL

        add_band <- function(class_name, yset, label_prefix) {
          rr <- class_ranges[[class_name]]
          if (nrow(rr) == 0) {
            return(invisible(NULL))
          }
          sdt <- parse_utc_datetime_value(rr$start_datetime) +
            pv$display_offset_seconds
          edt <- parse_utc_datetime_value(rr$end_datetime) +
            pv$display_offset_seconds
          rr$start_dt <- pmax(sdt, mindt)
          rr$end_dt <- pmin(edt, maxdt)

          # Extend each class band to the next data point to avoid visible gaps
          next_idx <- findInterval(rr$end_dt, new_data_dt) + 1
          has_next <- next_idx <= length(new_data_dt)
          rr$end_dt[has_next] <- new_data_dt[next_idx[has_next]]

          rr <- rr[
            !is.na(rr$start_dt) & !is.na(rr$end_dt) & rr$end_dt >= rr$start_dt,
            ,
            drop = FALSE
          ]
          if (nrow(rr) == 0) {
            return(invisible(NULL))
          }
          rr$id <- paste0(class_name, "_", seq_len(nrow(rr)))
          idx <- match(rr$code, type_map[[class_name]]$code)
          rr$color <- ifelse(
            is.na(idx),
            "#BBBBBB",
            as.character(type_map[[class_name]]$color_code[idx])
          )
          rr$description <- ifelse(
            is.na(idx),
            rr$description,
            as.character(type_map[[class_name]]$description[idx])
          )
          rr_dt <- data.table::as.data.table(rr)
          poly_list[[length(poly_list) + 1]] <<- rr_dt[,
            .(
              datetime = c(start_dt[1L], start_dt[1L], end_dt[1L], end_dt[1L]),
              y = yset,
              color = color[1L],
              text = paste0(
                label_prefix,
                ": ",
                code[1L],
                " (",
                description[1L],
                ")"
              ),
              id = id[1L]
            ),
            by = id
          ]
        }

        if (nrow(class_ranges$approval) > 0) {
          approval_y <- if (
            nrow(class_ranges$grade) > 0 && nrow(class_ranges$qualifier) > 0
          ) {
            c(2.2, 3.2, 3.2, 2.2)
          } else if (nrow(class_ranges$grade) > 0) {
            c(1.1, 2.1, 2.1, 1.1)
          } else {
            c(0, 1, 1, 0)
          }
          add_band("approval", approval_y, "Approval")
        }
        if (nrow(class_ranges$grade) > 0) {
          grade_y <- if (nrow(class_ranges$qualifier) > 0) {
            c(1.1, 2.1, 2.1, 1.1)
          } else {
            c(0, 1, 1, 0)
          }
          add_band("grade", grade_y, "Grade")
        }
        if (nrow(class_ranges$qualifier) > 0) {
          qualifier_y <- c(0, 1, 1, 0)
          add_band("qualifier", qualifier_y, "Qualifier")
        }

        if (length(poly_list) > 0) {
          polygons_df <- data.table::rbindlist(poly_list, use.names = TRUE)
          annotation_list <- list()
          if (!is.null(approval_y)) {
            annotation_list <- c(
              annotation_list,
              list(list(
                x = 0,
                y = mean(approval_y[c(1, 2)]),
                xref = "paper",
                yref = "y",
                text = "Approval",
                showarrow = FALSE,
                xanchor = "right",
                yanchor = "middle",
                font = list(size = 10)
              ))
            )
          }
          if (!is.null(grade_y)) {
            annotation_list <- c(
              annotation_list,
              list(list(
                x = 0,
                y = mean(grade_y[c(1, 2)]),
                xref = "paper",
                yref = "y",
                text = "Grade",
                showarrow = FALSE,
                xanchor = "right",
                yanchor = "middle",
                font = list(size = 10)
              ))
            )
          }
          if (!is.null(qualifier_y)) {
            annotation_list <- c(
              annotation_list,
              list(list(
                x = 0,
                y = mean(qualifier_y[c(1, 2)]),
                xref = "paper",
                yref = "y",
                text = "Qualifier",
                showarrow = FALSE,
                xanchor = "right",
                yanchor = "middle",
                font = list(size = 10)
              ))
            )
          }

          bands_plot <- plotly::plot_ly() |>
            plotly::add_polygons(
              data = polygons_df,
              x = ~datetime,
              y = ~y,
              split = ~id,
              fill = "toself",
              fillcolor = ~color,
              line = list(width = 1, color = "black"),
              hoverinfo = "text",
              hoveron = "fills",
              text = ~text,
              showlegend = FALSE
            ) |>
            plotly::layout(
              yaxis = list(
                showticklabels = FALSE,
                showgrid = FALSE,
                zeroline = FALSE
              ),
              xaxis = list(showgrid = FALSE, showticklabels = FALSE),
              annotations = annotation_list,
              margin = list(t = 0, b = 20, l = 80)
            )
          plot <- plotly::subplot(
            plot,
            bands_plot,
            nrows = 2,
            shareX = TRUE,
            heights = c(0.8, 0.2),
            margin = 0.02
          )
        }
      }

      plot <- plot |>
        plotly::layout(
          title = NULL,
          xaxis = list(
            title = list(
              text = paste0("Datetime (", pv$display_tz, ")"),
              standoff = 0
            ),
            showgrid = FALSE,
            showline = TRUE,
            tickformat = "%b %-d '%y",
            titlefont = list(size = 14),
            tickfont = list(size = 12),
            nticks = 10,
            rangeslider = list(
              visible = FALSE
            ),
            ticks = "outside",
            ticklen = 5,
            tickwidth = 1,
            tickcolor = "black"
          ),
          # Main plot yaxis layout
          yaxis = list(
            title = list(
              text = paste0(
                pv$parameter$param_name,
                " (",
                pv$parameter$unit,
                ")"
              ),
              standoff = 10
            ),
            showgrid = FALSE,
            showline = TRUE,
            zeroline = FALSE,
            titlefont = list(size = 14),
            tickfont = list(size = 12),
            autorange = if (
              pv$parameter$plot_default_y_orientation == "inverted"
            ) {
              "reversed"
            } else {
              TRUE
            },
            ticks = "outside",
            ticklen = 5,
            tickwidth = 1,
            tickcolor = "black"
          ),
          margin = list(b = 0, t = 40, l = 50),
          hovermode = "x unified",
          legend = list(
            font = list(size = 12),
            orientation = "v"
          ),
          font = list(family = "Nunito Sans")
        ) |>
        plotly::config(locale = "en")

      plot
    }

    preview_plot_task <- ExtendedTask$new(function(pv) {
      promises::future_promise({
        tryCatch(
          {
            list(
              plot = make_preview_plot(pv),
              signature = pv$signature
            )
          },
          error = function(e) {
            conditionMessage(e)
          }
        )
      })
    }) |>
      bslib::bind_task_button("make_plot")

    observeEvent(input$make_plot, {
      req(timeseries())
      preview_plot_task$invoke(preview_request())
    })

    observeEvent(preview_plot_task$result(), {
      result <- preview_plot_task$result()
      if (inherits(result, "character")) {
        showNotification(
          paste("Preview plot failed:", result),
          type = "error",
          duration = 10
        )
        return()
      }

      plot_data(result$plot)
      last_plot_signature(result$signature)
    })

    output$data_preview <- plotly::renderPlotly({
      plot_data()
    })

    # function to check data validity before upload
    check_fx <- function() {
      if (is.null(timeseries())) {
        showNotification('Please select a timeseries first.', type = 'error')
        return(FALSE)
      }
      if (nrow(data$df) == 0) {
        showNotification('Empty data table!', type = 'error')
        return(FALSE)
      }

      if (is.null(input$owner) || is.null(input$contributor)) {
        showNotification(
          'Please select owner and contributor organizations.',
          type = 'error'
        )
        return(FALSE)
      }

      # Check for duplicated rows and drop them; warn the user
      duplicated_rows <- duplicated(data$df)
      if (any(duplicated_rows)) {
        data$df <- data$df[!duplicated_rows, ]
        showNotification(
          paste0(
            'There were ',
            sum(duplicated_rows),
            ' duplicated (completely identical) rows. Only the first occurence of each unique row was retained'
          ),
          type = 'message',
          duration = 8
        )
      }
      duplicated_datetimes <- data$df$datetime[duplicated(data$df$datetime)]
      if (length(duplicated_datetimes) > 0) {
        showNotification(
          paste0(
            'There is more than one datetime for ',
            paste(unique(duplicated_datetimes), collapse = ", ")
          ),
          type = 'error',
          duration = 10
        )
        return(FALSE)
      }

      parsed_value <- suppressWarnings(as.numeric(data$df$value))
      if (any(is.na(parsed_value))) {
        showNotification(
          'Value column must be numeric with no missing values.',
          type = 'error',
          duration = 8
        )
        return(FALSE)
      }
      # Make sure datetime is in POSIXct format or can be converted to it
      parsed_datetime <- table_datetimes_to_utc(
        data$df$datetime,
        input$UTC_offset
      )
      if (any(is.na(parsed_datetime))) {
        showNotification(
          'Datetime column is not in the correct format. Please check your data: it should be of form YYYY-MM-DD HH:MM.',
          type = 'error',
          duration = 10
        )
        return(FALSE)
      }
      data$parsed_datetime <- parsed_datetime
      data$parsed_value <- parsed_value
      return(TRUE)
    }

    empty_continuous_upload_df <- function() {
      data.frame(
        datetime = character(),
        value = numeric(),
        grade = character(),
        approval = character(),
        qualifier = character(),
        stringsAsFactors = FALSE
      )
    }

    reset_upload_state <- function() {
      data$df <- empty_continuous_upload_df()
      data$parsed_datetime <- NULL
      data$parsed_value <- NULL
      unit_conversion_state$previous_values <- NULL
      unit_conversion_state$previous_label <- NULL
      class_ranges$grade <- class_ranges$grade[0, , drop = FALSE]
      class_ranges$approval <- class_ranges$approval[0, , drop = FALSE]
      class_ranges$qualifier <- class_ranges$qualifier[0, , drop = FALSE]
      plot_data(NULL)
      last_plot_signature(NULL)
      refresh_data_table()
    }

    build_upload_request <- function(overwrite) {
      upload_data <- data$df
      upload_data$datetime <- data$parsed_datetime
      upload_data$value <- data$parsed_value
      upload_data$owner <- as.integer(input$owner)
      upload_data$contributor <- as.integer(input$contributor)
      upload_data$no_update <- data.table::fifelse(
        input$no_update == "yes",
        TRUE,
        FALSE
      )

      list(
        timeseries_id = timeseries(),
        data = upload_data,
        overwrite = overwrite,
        config = session$userData$config
      )
    }

    upload_task <- ExtendedTask$new(function(req) {
      promises::future_promise({
        warnings <- character()
        messages <- character()

        tryCatch(
          {
            con <- AquaConnect(
              name = req$config$dbName,
              host = req$config$dbHost,
              port = req$config$dbPort,
              username = req$config$dbUser,
              password = req$config$dbPass,
              silent = TRUE
            )
            on.exit(DBI::dbDisconnect(con), add = TRUE)

            withCallingHandlers(
              AquaCache::addNewContinuous(
                tsid = req$timeseries_id,
                df = req$data,
                con = con,
                overwrite = req$overwrite
              ),
              warning = function(w) {
                warnings <<- c(warnings, conditionMessage(w))
                invokeRestart("muffleWarning")
              },
              message = function(m) {
                messages <<- c(messages, conditionMessage(m))
                invokeRestart("muffleMessage")
              }
            )

            list(
              ok = TRUE,
              overwrite = req$overwrite,
              warnings = unique(warnings),
              messages = unique(messages)
            )
          },
          error = function(e) {
            list(
              ok = FALSE,
              message = conditionMessage(e)
            )
          }
        )
      })
    }) |>
      bslib::bind_task_button("upload") |>
      bslib::bind_task_button("upload_overwrite_all") |>
      bslib::bind_task_button("upload_overwrite_some")

    invoke_upload_task <- function(overwrite) {
      check <- check_fx()
      if (!check) {
        return()
      }

      upload_task$invoke(build_upload_request(overwrite))
    }

    observeEvent(input$upload, {
      invoke_upload_task("no")
    })

    observeEvent(upload_task$result(), {
      result <- upload_task$result()

      if (!isTRUE(result$ok)) {
        showNotification(
          paste('Upload failed:', result$message),
          type = 'error',
          duration = 10
        )
        return()
      }

      if (length(result$warnings)) {
        showNotification(
          paste('Warning on upload:', paste(result$warnings, collapse = " ")),
          type = 'warning',
          duration = 10
        )
      }
      if (length(result$messages)) {
        showNotification(
          paste('Message on upload:', paste(result$messages, collapse = " ")),
          type = 'message',
          duration = 8
        )
      }

      notification <- switch(
        result$overwrite,
        all = 'Data added with overwrite.',
        conflict = 'Data added with selective overwrite.',
        'Data added.'
      )
      showNotification(notification, type = 'message')
      reset_upload_state()
    })

    observeEvent(input$upload_overwrite_all, {
      check <- check_fx()
      if (!check) {
        return()
      }
      # Show a modal dialog to confirm overwritting all points in time range of new data
      showModal(modalDialog(
        title = 'Confirm Overwrite',
        'This will overwrite all points in the time range of the new data, including any points that do not conflict with the new data (if any). Are you sure?',
        easyClose = TRUE,
        footer = tagList(
          modalButton('Cancel'),
          actionButton(
            ns('confirm_overwrite_all'),
            'Confirm Overwrite',
            class = 'btn-danger'
          )
        )
      ))
    })
    observeEvent(input$confirm_overwrite_all, {
      removeModal() # Close the modal dialog
      invoke_upload_task("all")
    })

    observeEvent(input$upload_overwrite_some, {
      check <- check_fx()
      if (!check) {
        return()
      }
      # Show a modal dialog to confirm overwriting conflicting points only
      showModal(modalDialog(
        title = 'Confirm Selective Overwrite',
        'This will overwrite only the points in the new data that conflict with existing points in the time range of the new data. Are you sure?',
        easyClose = TRUE,
        footer = tagList(
          modalButton('Cancel'),
          actionButton(
            ns('confirm_overwrite_some'),
            'Confirm Selective Overwrite',
            class = 'btn-warning'
          )
        )
      ))
    })
    observeEvent(input$confirm_overwrite_some, {
      removeModal() # Close the modal dialog
      invoke_upload_task("conflict")
    })

    return(outputs)
  }) # End of moduleServer
}
