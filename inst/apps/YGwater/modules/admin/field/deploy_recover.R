# UI and server code for instrument deployment / recovery module

deploy_recover_UI <- function(id) {
  ns <- NS(id)

  page_fluid(
    uiOutput(ns("banner")),
    title = "Instrument deployment / recovery",
    p(
      "Deploy instruments to a location, optional sub-location, and optional elevation/depth.",
      "Select an existing record below to edit or recover it."
    ),
    uiOutput(ns("module_ui"))
  )
}

deploy_recover <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "deploy_recover"
      )
    })

    table_header_style <- htmlwidgets::JS(
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

    moduleData <- reactiveValues(
      permissions = list(),
      instruments = NULL,
      locations = NULL,
      sub_locations = NULL,
      locations_z = NULL,
      deployment_records = NULL
    )

    selected_metadata_id <- reactiveVal(NULL)
    selection_sync_in_progress <- reactiveVal(FALSE)

    null_if_empty <- function(x) {
      if (is.null(x) || length(x) == 0) {
        return(NULL)
      }
      x <- x[[1]]
      if (is.character(x)) {
        x <- trimws(x)
        if (!nzchar(x)) {
          return(NULL)
        }
      }
      if (is.na(x)) {
        return(NULL)
      }
      x
    }

    as_nullable_integer <- function(x) {
      value <- null_if_empty(x)
      if (is.null(value)) {
        return(NA_integer_)
      }
      as.integer(value)
    }

    as_nullable_text <- function(x) {
      value <- null_if_empty(x)
      if (is.null(value)) {
        return(NA_character_)
      }
      as.character(value)
    }

    default_datetime <- function() {
      .POSIXct(Sys.time(), tz = "UTC")
    }

    shift_datetime_input_timezone <- function(input_id, tz_name) {
      current_value <- coerce_utc_datetime(input[[input_id]])
      if (
        is.null(current_value) ||
          !length(current_value) ||
          all(is.na(current_value))
      ) {
        return(invisible(NULL))
      }
      shinyWidgets::updateAirDateInput(
        session,
        inputId = input_id,
        value = current_value,
        tz = tz_name
      )
    }

    format_timestamp <- function(x) {
      ifelse(
        is.na(x),
        "",
        format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%d %H:%M:%S %Z")
      )
    }

    safe_text <- function(x) {
      ifelse(is.na(x), "", as.character(x))
    }

    deployment_status <- function(start_datetime, end_datetime) {
      now_utc <- as.POSIXct(Sys.time(), tz = "UTC")
      start_datetime <- as.POSIXct(start_datetime, tz = "UTC")
      end_datetime <- as.POSIXct(end_datetime, tz = "UTC")

      status <- rep("Recovered", length(start_datetime))
      status[!is.na(start_datetime) & start_datetime > now_utc] <- "Scheduled"
      status[
        !is.na(start_datetime) &
          start_datetime <= now_utc &
          (is.na(end_datetime) | end_datetime > now_utc)
      ] <- "Currently deployed"
      status
    }

    build_instrument_choices <- function() {
      if (
        is.null(moduleData$instruments) || nrow(moduleData$instruments) == 0
      ) {
        return(character(0))
      }

      labels <- sprintf(
        "%s | %s | %s | %s",
        moduleData$instruments$serial_no,
        safe_text(moduleData$instruments$make),
        safe_text(moduleData$instruments$model),
        safe_text(moduleData$instruments$instrument_type)
      )

      retired_idx <- !is.na(moduleData$instruments$date_retired)
      if (any(retired_idx)) {
        labels[retired_idx] <- paste0(
          labels[retired_idx],
          " [retired ",
          moduleData$instruments$date_retired[retired_idx],
          "]"
        )
      }

      stats::setNames(moduleData$instruments$instrument_id, labels)
    }

    build_location_choices <- function() {
      if (is.null(moduleData$locations) || nrow(moduleData$locations) == 0) {
        return(character(0))
      }

      stats::setNames(
        moduleData$locations$location_id,
        sprintf(
          "%s [%s]",
          moduleData$locations$location_name,
          moduleData$locations$location_code
        )
      )
    }

    update_sub_location_choices <- function(
      selected_location = isolate(input$location_id),
      selected_sub_location = NULL
    ) {
      if (is.null(selected_sub_location)) {
        selected_sub_location <- isolate(input$sub_location_id)
      }

      location_id <- as_nullable_integer(selected_location)
      if (
        is.na(location_id) ||
          is.null(moduleData$sub_locations) ||
          nrow(moduleData$sub_locations) == 0
      ) {
        updateSelectizeInput(
          session,
          "sub_location_id",
          choices = character(0),
          selected = character(0)
        )
        return(invisible(NULL))
      }

      available <- moduleData$sub_locations[
        moduleData$sub_locations$location_id == location_id,
        ,
        drop = FALSE
      ]
      available <- available[order(available$sub_location_name), , drop = FALSE]

      updateSelectizeInput(
        session,
        "sub_location_id",
        choices = stats::setNames(
          available$sub_location_id,
          available$sub_location_name
        ),
        selected = if (length(selected_sub_location)) {
          as.character(selected_sub_location[[1]])
        } else {
          character(0)
        }
      )
    }

    update_z_choices <- function(
      selected_location = isolate(input$location_id),
      selected_sub_location = NULL,
      selected_z = NULL
    ) {
      if (is.null(selected_sub_location)) {
        selected_sub_location <- isolate(input$sub_location_id)
      }
      if (is.null(selected_z)) {
        selected_z <- isolate(input$z_id)
      }

      location_id <- as_nullable_integer(selected_location)
      sub_location_id <- as_nullable_integer(selected_sub_location)

      if (
        is.na(location_id) ||
          is.null(moduleData$locations_z) ||
          nrow(moduleData$locations_z) == 0
      ) {
        updateSelectizeInput(
          session,
          "z_id",
          choices = character(0),
          selected = character(0)
        )
        return(invisible(NULL))
      }

      available <- moduleData$locations_z[
        moduleData$locations_z$location_id == location_id,
        ,
        drop = FALSE
      ]

      if (is.na(sub_location_id)) {
        available <- available[is.na(available$sub_location_id), , drop = FALSE]
      } else {
        available <- available[
          is.na(available$sub_location_id) |
            available$sub_location_id == sub_location_id,
          ,
          drop = FALSE
        ]
      }

      if (nrow(available) == 0) {
        updateSelectizeInput(
          session,
          "z_id",
          choices = character(0),
          selected = character(0)
        )
        return(invisible(NULL))
      }

      sub_loc_names <- stats::setNames(
        moduleData$sub_locations$sub_location_name,
        moduleData$sub_locations$sub_location_id
      )
      z_scope <- ifelse(
        is.na(available$sub_location_id),
        "site-level",
        safe_text(sub_loc_names[as.character(available$sub_location_id)])
      )
      z_note <- ifelse(
        is.na(available$note) | !nzchar(available$note),
        "",
        paste0(" | ", available$note)
      )
      labels <- paste0(
        format(available$z_meters, trim = TRUE),
        " m | ",
        z_scope,
        z_note
      )

      updateSelectizeInput(
        session,
        "z_id",
        choices = stats::setNames(available$z_id, labels),
        selected = if (length(selected_z)) {
          as.character(selected_z[[1]])
        } else {
          character(0)
        }
      )
    }

    reset_form <- function(clear_selection = TRUE) {
      if (clear_selection) {
        selected_metadata_id(NULL)
      }

      updateSelectizeInput(session, "instrument_id", selected = character(0))
      updateSelectizeInput(session, "location_id", selected = character(0))
      updateSelectizeInput(
        session,
        "sub_location_id",
        choices = character(0),
        selected = character(0)
      )
      updateSelectizeInput(
        session,
        "z_id",
        choices = character(0),
        selected = character(0)
      )
      shinyWidgets::updateAirDateInput(
        session,
        "start_datetime",
        value = default_datetime(),
        tz = air_datetime_widget_timezone(input$start_timezone)
      )
      updateCheckboxInput(session, "has_end_datetime", value = FALSE)
      shinyWidgets::updateAirDateInput(
        session,
        "end_datetime",
        value = default_datetime(),
        tz = air_datetime_widget_timezone(input$end_timezone)
      )
      updateTextAreaInput(session, "note", value = "")
    }

    refresh_module_data <- function() {
      moduleData$permissions <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "
        SELECT
          has_table_privilege(current_user, 'public.locations_metadata_instruments', 'SELECT') AS can_select,
          has_table_privilege(current_user, 'public.locations_metadata_instruments', 'INSERT') AS can_insert,
          has_table_privilege(current_user, 'public.locations_metadata_instruments', 'UPDATE') AS can_update
        "
      )

      moduleData$locations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "
        SELECT
          location_id,
          location_code,
          name AS location_name
        FROM public.locations
        ORDER BY name ASC
        "
      )

      moduleData$sub_locations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "
        SELECT
          sub_location_id,
          location_id,
          sub_location_name
        FROM public.sub_locations
        ORDER BY sub_location_name ASC
        "
      )

      moduleData$locations_z <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "
        SELECT
          z_id,
          location_id,
          sub_location_id,
          z_meters,
          note
        FROM public.locations_z
        ORDER BY location_id, sub_location_id NULLS FIRST, z_meters
        "
      )

      moduleData$instruments <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "
        SELECT
          i.instrument_id,
          i.serial_no,
          mk.make,
          mdl.model,
          it.type AS instrument_type,
          i.date_retired
        FROM instruments.instruments AS i
        LEFT JOIN instruments.instrument_make AS mk
          ON i.make = mk.make_id
        LEFT JOIN instruments.instrument_model AS mdl
          ON i.model = mdl.model_id
        LEFT JOIN instruments.instrument_type AS it
          ON i.type = it.type_id
        ORDER BY i.serial_no ASC, i.instrument_id ASC
        "
      )

      moduleData$deployment_records <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "
        SELECT
          lmi.metadata_id,
          lmi.location_id,
          l.location_code,
          l.name AS location_name,
          lmi.sub_location_id,
          sl.sub_location_name,
          lmi.z_id,
          lz.z_meters,
          lmi.instrument_id,
          i.serial_no,
          mk.make,
          mdl.model,
          it.type AS instrument_type,
          lmi.start_datetime,
          lmi.end_datetime,
          lmi.note,
          lmi.created,
          lmi.created_by,
          lmi.modified,
          lmi.modified_by
        FROM public.locations_metadata_instruments AS lmi
        LEFT JOIN public.locations AS l
          ON lmi.location_id = l.location_id
        LEFT JOIN public.sub_locations AS sl
          ON lmi.sub_location_id = sl.sub_location_id
        LEFT JOIN public.locations_z AS lz
          ON lmi.z_id = lz.z_id
        LEFT JOIN instruments.instruments AS i
          ON lmi.instrument_id = i.instrument_id
        LEFT JOIN instruments.instrument_make AS mk
          ON i.make = mk.make_id
        LEFT JOIN instruments.instrument_model AS mdl
          ON i.model = mdl.model_id
        LEFT JOIN instruments.instrument_type AS it
          ON i.type = it.type_id
        ORDER BY lmi.start_datetime DESC, lmi.metadata_id DESC
        "
      )

      if (nrow(moduleData$deployment_records) > 0) {
        moduleData$deployment_records$deployment_status <- deployment_status(
          moduleData$deployment_records$start_datetime,
          moduleData$deployment_records$end_datetime
        )
      } else {
        moduleData$deployment_records$deployment_status <- character(0)
      }

      updateSelectizeInput(
        session,
        "instrument_id",
        choices = build_instrument_choices(),
        selected = isolate(input$instrument_id)
      )
      updateSelectizeInput(
        session,
        "location_id",
        choices = build_location_choices(),
        selected = isolate(input$location_id)
      )
      update_sub_location_choices()
      update_z_choices()
    }

    current_records <- reactive({
      req(moduleData$deployment_records)
      moduleData$deployment_records[
        moduleData$deployment_records$deployment_status == "Currently deployed",
        ,
        drop = FALSE
      ]
    })

    selected_record <- reactive({
      metadata_id <- selected_metadata_id()
      if (
        is.null(metadata_id) ||
          is.null(moduleData$deployment_records) ||
          nrow(moduleData$deployment_records) == 0
      ) {
        return(NULL)
      }

      record <- moduleData$deployment_records[
        moduleData$deployment_records$metadata_id == metadata_id,
        ,
        drop = FALSE
      ]
      if (nrow(record) == 0) {
        return(NULL)
      }
      record
    })

    current_table_data <- reactive({
      df <- current_records()
      if (nrow(df) == 0) {
        return(data.frame(
          metadata_id = integer(0),
          serial_no = character(0),
          make = character(0),
          model = character(0),
          instrument_type = character(0),
          location_code = character(0),
          location_name = character(0),
          sub_location_name = character(0),
          z_meters = character(0),
          start_datetime = character(0),
          note = character(0)
        ))
      }

      data.frame(
        metadata_id = df$metadata_id,
        serial_no = safe_text(df$serial_no),
        make = safe_text(df$make),
        model = safe_text(df$model),
        instrument_type = safe_text(df$instrument_type),
        location_code = safe_text(df$location_code),
        location_name = safe_text(df$location_name),
        sub_location_name = safe_text(df$sub_location_name),
        z_meters = ifelse(
          is.na(df$z_meters),
          "",
          format(df$z_meters, trim = TRUE)
        ),
        start_datetime = format_timestamp(df$start_datetime),
        note = safe_text(df$note),
        stringsAsFactors = FALSE
      )
    })

    all_table_data <- reactive({
      df <- moduleData$deployment_records
      req(df)

      if (nrow(df) == 0) {
        return(data.frame(
          metadata_id = integer(0),
          deployment_status = character(0),
          serial_no = character(0),
          make = character(0),
          model = character(0),
          instrument_type = character(0),
          location_code = character(0),
          location_name = character(0),
          sub_location_name = character(0),
          z_meters = character(0),
          start_datetime = character(0),
          end_datetime = character(0),
          note = character(0)
        ))
      }

      data.frame(
        metadata_id = df$metadata_id,
        deployment_status = safe_text(df$deployment_status),
        serial_no = safe_text(df$serial_no),
        make = safe_text(df$make),
        model = safe_text(df$model),
        instrument_type = safe_text(df$instrument_type),
        location_code = safe_text(df$location_code),
        location_name = safe_text(df$location_name),
        sub_location_name = safe_text(df$sub_location_name),
        z_meters = ifelse(
          is.na(df$z_meters),
          "",
          format(df$z_meters, trim = TRUE)
        ),
        start_datetime = format_timestamp(df$start_datetime),
        end_datetime = format_timestamp(df$end_datetime),
        note = safe_text(df$note),
        stringsAsFactors = FALSE
      )
    })

    populate_form_from_record <- function(record) {
      if (is.null(record) || nrow(record) == 0) {
        reset_form(clear_selection = FALSE)
        return(invisible(NULL))
      }

      updateSelectizeInput(
        session,
        "instrument_id",
        selected = as.character(record$instrument_id[[1]])
      )
      updateSelectizeInput(
        session,
        "location_id",
        selected = as.character(record$location_id[[1]])
      )
      update_sub_location_choices(
        selected_location = record$location_id[[1]],
        selected_sub_location = if (is.na(record$sub_location_id[[1]])) {
          character(0)
        } else {
          as.character(record$sub_location_id[[1]])
        }
      )
      update_z_choices(
        selected_location = record$location_id[[1]],
        selected_sub_location = if (is.na(record$sub_location_id[[1]])) {
          character(0)
        } else {
          as.character(record$sub_location_id[[1]])
        },
        selected_z = if (is.na(record$z_id[[1]])) {
          character(0)
        } else {
          as.character(record$z_id[[1]])
        }
      )
      shinyWidgets::updateAirDateInput(
        session,
        "start_datetime",
        value = coerce_utc_datetime(record$start_datetime[[1]]),
        tz = air_datetime_widget_timezone(input$start_timezone)
      )
      updateCheckboxInput(
        session,
        "has_end_datetime",
        value = !is.na(record$end_datetime[[1]])
      )
      shinyWidgets::updateAirDateInput(
        session,
        "end_datetime",
        value = if (is.na(record$end_datetime[[1]])) {
          default_datetime()
        } else {
          coerce_utc_datetime(record$end_datetime[[1]])
        },
        tz = air_datetime_widget_timezone(input$end_timezone)
      )
      updateTextAreaInput(
        session,
        "note",
        value = if (is.na(record$note[[1]])) "" else record$note[[1]]
      )
    }

    sync_table_selection <- function(metadata_id = selected_metadata_id()) {
      current_proxy <- DT::dataTableProxy("current_table")
      all_proxy <- DT::dataTableProxy("all_table")

      selection_sync_in_progress(TRUE)
      on.exit(selection_sync_in_progress(FALSE), add = TRUE)

      if (is.null(metadata_id)) {
        DT::selectRows(current_proxy, selected = NULL)
        DT::selectRows(all_proxy, selected = NULL)
        return(invisible(NULL))
      }

      current_idx <- which(current_table_data()$metadata_id == metadata_id)
      all_idx <- which(all_table_data()$metadata_id == metadata_id)

      current_proxy |>
        DT::selectRows(if (length(current_idx) == 1) current_idx else NULL)
      all_proxy |>
        DT::selectRows(if (length(all_idx) == 1) all_idx else NULL)
    }

    collect_form_values <- function() {
      list(
        instrument_id = as_nullable_integer(input$instrument_id),
        location_id = as_nullable_integer(input$location_id),
        sub_location_id = as_nullable_integer(input$sub_location_id),
        z_id = as_nullable_integer(input$z_id),
        start_datetime = scalar_utc_datetime(input$start_datetime),
        end_datetime = if (isTRUE(input$has_end_datetime)) {
          scalar_utc_datetime(input$end_datetime)
        } else {
          empty_utc_datetime()
        },
        note = as_nullable_text(input$note)
      )
    }

    validate_form_values <- function(values, require_selected = FALSE) {
      if (require_selected && is.null(selected_metadata_id())) {
        stop("Select an existing deployment record first.")
      }
      if (is.na(values$instrument_id)) {
        stop("Select an instrument.")
      }
      if (is.na(values$location_id)) {
        stop("Select a location.")
      }
      if (is.na(values$start_datetime)) {
        stop("Deployment start date/time is required.")
      }
      if (!is.na(values$sub_location_id)) {
        sub_loc <- moduleData$sub_locations[
          moduleData$sub_locations$sub_location_id == values$sub_location_id,
          ,
          drop = FALSE
        ]
        if (
          nrow(sub_loc) == 0 || sub_loc$location_id[[1]] != values$location_id
        ) {
          stop(
            "The selected sub-location does not belong to the selected location."
          )
        }
      }
      if (!is.na(values$z_id)) {
        z_row <- moduleData$locations_z[
          moduleData$locations_z$z_id == values$z_id,
          ,
          drop = FALSE
        ]
        if (nrow(z_row) == 0 || z_row$location_id[[1]] != values$location_id) {
          stop(
            "The selected elevation/depth does not belong to the selected location."
          )
        }
        if (
          is.na(values$sub_location_id) && !is.na(z_row$sub_location_id[[1]])
        ) {
          stop(
            "Choose the matching sub-location for the selected elevation/depth."
          )
        }
        if (
          !is.na(values$sub_location_id) &&
            !is.na(z_row$sub_location_id[[1]]) &&
            z_row$sub_location_id[[1]] != values$sub_location_id
        ) {
          stop(
            "The selected elevation/depth does not match the selected sub-location."
          )
        }
      }
      if (
        !is.na(values$end_datetime) &&
          values$end_datetime <= values$start_datetime
      ) {
        stop("Recovery time must be after the deployment start time.")
      }
      invisible(TRUE)
    }

    upsert_record <- function(
      values,
      metadata_id = NULL,
      recovery_only = FALSE
    ) {
      validate_form_values(values, require_selected = !is.null(metadata_id))

      if (recovery_only) {
        current_record <- selected_record()
        if (
          is.null(current_record) ||
            current_record$deployment_status[[1]] != "Currently deployed"
        ) {
          stop("Recovery is only available for currently deployed instruments.")
        }
        if (is.na(values$end_datetime)) {
          values$end_datetime <- as.POSIXct(Sys.time(), tz = "UTC")
        }
        if (values$end_datetime <= values$start_datetime) {
          stop("Recovery time must be after the deployment start time.")
        }
      }

      if (is.null(metadata_id)) {
        inserted <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "
          INSERT INTO public.locations_metadata_instruments (
            location_id, sub_location_id, instrument_id, start_datetime,
            end_datetime, note, z_id
          ) VALUES ($1, $2, $3, $4, $5, $6, $7)
          RETURNING metadata_id
          ",
          params = list(
            values$location_id,
            values$sub_location_id,
            values$instrument_id,
            values$start_datetime,
            values$end_datetime,
            values$note,
            values$z_id
          )
        )
        inserted$metadata_id[[1]]
      } else {
        DBI::dbExecute(
          session$userData$AquaCache,
          "
          UPDATE public.locations_metadata_instruments
          SET
            location_id = $1,
            sub_location_id = $2,
            instrument_id = $3,
            start_datetime = $4,
            end_datetime = $5,
            note = $6,
            z_id = $7
          WHERE metadata_id = $8
          ",
          params = list(
            values$location_id,
            values$sub_location_id,
            values$instrument_id,
            values$start_datetime,
            values$end_datetime,
            values$note,
            values$z_id,
            metadata_id
          )
        )
        metadata_id
      }
    }

    output$module_ui <- renderUI({
      can_select <- isTRUE(moduleData$permissions$can_select[[1]])
      if (!can_select) {
        return(div(
          class = "alert alert-warning",
          "You do not have sufficient privileges to manage instrument deployments."
        ))
      }

      tagList(
        fluidRow(
          column(
            4,
            actionButton(
              ns("reload_module"),
              "Reload module data",
              icon = icon("refresh"),
              width = "100%"
            ),
            br(),
            br(),
            actionButton(
              ns("clear_selection"),
              "Clear selected record",
              width = "100%"
            ),
            br(),
            br(),
            uiOutput(ns("selected_record_ui")),
            selectizeInput(
              ns("instrument_id"),
              "Instrument",
              choices = isolate(build_instrument_choices()),
              multiple = TRUE,
              options = list(
                maxItems = 1,
                placeholder = "Select an instrument"
              ),
              width = "100%"
            ),
            selectizeInput(
              ns("location_id"),
              "Location",
              choices = isolate(build_location_choices()),
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Select a location"),
              width = "100%"
            ),
            selectizeInput(
              ns("sub_location_id"),
              "Sub-location",
              choices = NULL,
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Optional"),
              width = "100%"
            ),
            selectizeInput(
              ns("z_id"),
              "Elevation / depth",
              choices = NULL,
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Optional"),
              width = "100%"
            ),
            fluidRow(
              column(
                5,
                selectizeInput(
                  ns("start_timezone"),
                  "Start timezone",
                  choices = input_timezone_choices(),
                  selected = default_input_timezone(),
                  multiple = FALSE,
                  width = "100%"
                )
              ),
              column(
                7,
                shinyWidgets::airDatepickerInput(
                  ns("start_datetime"),
                  label = "Deployment start",
                  value = default_datetime(),
                  range = FALSE,
                  multiple = FALSE,
                  timepicker = TRUE,
                  maxDate = Sys.Date() + 365,
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
            checkboxInput(
              ns("has_end_datetime"),
              "Deployment has ended / recovered",
              value = FALSE
            ),
            conditionalPanel(
              condition = "input.has_end_datetime",
              ns = ns,
              fluidRow(
                column(
                  5,
                  selectizeInput(
                    ns("end_timezone"),
                    "End timezone",
                    choices = input_timezone_choices(),
                    selected = default_input_timezone(),
                    multiple = FALSE,
                    width = "100%"
                  )
                ),
                column(
                  7,
                  shinyWidgets::airDatepickerInput(
                    ns("end_datetime"),
                    label = "Recovery time",
                    value = default_datetime(),
                    range = FALSE,
                    multiple = FALSE,
                    timepicker = TRUE,
                    maxDate = Sys.Date() + 365,
                    startView = Sys.Date(),
                    update_on = "change",
                    tz = air_datetime_widget_timezone(default_input_timezone()),
                    timepickerOpts = shinyWidgets::timepickerOptions(
                      minutesStep = 15,
                      timeFormat = "HH:mm"
                    )
                  )
                )
              )
            ),
            textAreaInput(
              ns("note"),
              "Notes",
              width = "100%",
              height = "120px"
            ),
            actionButton(
              ns("deploy_btn"),
              "Deploy new instrument",
              width = "100%",
              class = "btn-primary"
            ),
            br(),
            br(),
            actionButton(
              ns("update_btn"),
              "Update selected record",
              width = "100%"
            ),
            br(),
            br(),
            actionButton(
              ns("recover_btn"),
              "Recover selected deployment",
              width = "100%"
            )
          ),
          column(
            8,
            tabsetPanel(
              id = ns("table_tabs"),
              tabPanel("Currently deployed", DT::DTOutput(ns("current_table"))),
              tabPanel("All deployment records", DT::DTOutput(ns("all_table")))
            )
          )
        )
      )
    })

    output$selected_record_ui <- renderUI({
      record <- selected_record()
      if (is.null(record)) {
        return(div(
          class = "alert alert-info",
          "No deployment record selected. Fill in the form and click 'Deploy new instrument' to add one."
        ))
      }

      div(
        class = "alert alert-secondary",
        tags$strong(paste0(
          "Editing record #",
          record$metadata_id[[1]],
          " (",
          record$deployment_status[[1]],
          ")"
        )),
        tags$br(),
        paste(
          safe_text(record$serial_no[[1]]),
          safe_text(record$location_name[[1]]),
          sep = " | "
        ),
        if (!is.na(record$sub_location_name[[1]])) {
          tagList(tags$br(), safe_text(record$sub_location_name[[1]]))
        }
      )
    })

    output$current_table <- DT::renderDT({
      DT::datatable(
        current_table_data(),
        selection = "single",
        rownames = FALSE,
        filter = "top",
        options = list(
          columnDefs = list(list(targets = 0, visible = FALSE)),
          scrollX = TRUE,
          pageLength = 8,
          initComplete = table_header_style
        )
      )
    })

    output$all_table <- DT::renderDT({
      DT::datatable(
        all_table_data(),
        selection = "single",
        rownames = FALSE,
        filter = "top",
        options = list(
          columnDefs = list(list(targets = 0, visible = FALSE)),
          scrollX = TRUE,
          pageLength = 12,
          initComplete = table_header_style
        )
      )
    })

    observe({
      can_insert <- isTRUE(moduleData$permissions$can_insert[[1]])
      can_update <- isTRUE(moduleData$permissions$can_update[[1]])
      has_selected <- !is.null(selected_metadata_id())
      record <- selected_record()
      can_recover <- can_update &&
        !is.null(record) &&
        record$deployment_status[[1]] == "Currently deployed"

      if (can_insert && !has_selected) {
        shinyjs::enable(ns("deploy_btn"))
      } else {
        shinyjs::disable(ns("deploy_btn"))
      }

      if (can_update && has_selected) {
        shinyjs::enable(ns("update_btn"))
      } else {
        shinyjs::disable(ns("update_btn"))
      }

      if (can_recover) {
        shinyjs::enable(ns("recover_btn"))
      } else {
        shinyjs::disable(ns("recover_btn"))
      }
    })

    observeEvent(
      input$start_timezone,
      {
        new_timezone <- normalize_input_timezone(input$start_timezone)
        shift_datetime_input_timezone("start_datetime", new_timezone)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$end_timezone,
      {
        new_timezone <- normalize_input_timezone(input$end_timezone)
        shift_datetime_input_timezone("end_datetime", new_timezone)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$location_id,
      {
        update_sub_location_choices(selected_location = input$location_id)
        update_z_choices(
          selected_location = input$location_id,
          selected_sub_location = character(0),
          selected_z = character(0)
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$sub_location_id,
      {
        update_z_choices(
          selected_location = input$location_id,
          selected_sub_location = input$sub_location_id,
          selected_z = character(0)
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$reload_module,
      {
        refresh_module_data()
        if (!is.null(selected_metadata_id())) {
          if (is.null(selected_record())) {
            reset_form(clear_selection = TRUE)
            sync_table_selection(NULL)
          } else {
            populate_form_from_record(selected_record())
            sync_table_selection()
          }
        }
        showNotification("Module reloaded.", type = "message", duration = 10)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$clear_selection,
      {
        reset_form(clear_selection = TRUE)
        sync_table_selection(NULL)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$current_table_rows_selected,
      {
        if (isTRUE(selection_sync_in_progress())) {
          return()
        }
        selected_row <- input$current_table_rows_selected
        if (length(selected_row) != 1) {
          return()
        }
        metadata_id <- current_table_data()$metadata_id[[selected_row]]
        selected_metadata_id(metadata_id)
        populate_form_from_record(selected_record())
        sync_table_selection(metadata_id)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$all_table_rows_selected,
      {
        if (isTRUE(selection_sync_in_progress())) {
          return()
        }
        selected_row <- input$all_table_rows_selected
        if (length(selected_row) != 1) {
          return()
        }
        metadata_id <- all_table_data()$metadata_id[[selected_row]]
        selected_metadata_id(metadata_id)
        populate_form_from_record(selected_record())
        sync_table_selection(metadata_id)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$deploy_btn,
      {
        tryCatch(
          {
            metadata_id <- upsert_record(values = collect_form_values())
            refresh_module_data()
            selected_metadata_id(metadata_id)
            populate_form_from_record(selected_record())
            sync_table_selection(metadata_id)
            showNotification(
              "Deployment record added.",
              type = "message",
              duration = 10
            )
          },
          error = function(e) {
            showNotification(
              paste("Failed to add deployment record:", e$message),
              type = "error",
              duration = 15
            )
          }
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$update_btn,
      {
        tryCatch(
          {
            metadata_id <- selected_metadata_id()
            upsert_record(
              values = collect_form_values(),
              metadata_id = metadata_id
            )
            refresh_module_data()
            selected_metadata_id(metadata_id)
            populate_form_from_record(selected_record())
            sync_table_selection(metadata_id)
            showNotification(
              "Deployment record updated.",
              type = "message",
              duration = 10
            )
          },
          error = function(e) {
            showNotification(
              paste("Failed to update deployment record:", e$message),
              type = "error",
              duration = 15
            )
          }
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$recover_btn,
      {
        tryCatch(
          {
            metadata_id <- selected_metadata_id()
            values <- collect_form_values()
            if (is.na(values$end_datetime)) {
              values$end_datetime <- as.POSIXct(Sys.time(), tz = "UTC")
              updateCheckboxInput(session, "has_end_datetime", value = TRUE)
              shinyWidgets::updateAirDateInput(
                session,
                "end_datetime",
                value = values$end_datetime,
                tz = air_datetime_widget_timezone(input$end_timezone)
              )
            }
            upsert_record(
              values = values,
              metadata_id = metadata_id,
              recovery_only = TRUE
            )
            refresh_module_data()
            selected_metadata_id(metadata_id)
            populate_form_from_record(selected_record())
            sync_table_selection(metadata_id)
            showNotification(
              "Deployment recovered.",
              type = "message",
              duration = 10
            )
          },
          error = function(e) {
            showNotification(
              paste("Failed to recover deployment:", e$message),
              type = "error",
              duration = 15
            )
          }
        )
      },
      ignoreInit = TRUE
    )

    refresh_module_data()
    reset_form(clear_selection = TRUE)
    sync_table_selection(NULL)
  }) # End of moduleServer
}
