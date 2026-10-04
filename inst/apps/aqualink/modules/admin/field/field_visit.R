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
    uiOutput(ns("banner")),
    title = "Field Visits",
    p(
      "Record visit conditions, link one or more samples, and enter field readings on the sample they describe."
    ),
    uiOutput(ns("ui")) # Created in server so that menus can be populated right away
  ) # End of page_fluid
}

visit <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "visit"
      )
    })

    moduleData <- reactiveValues()
    visitData <- reactiveValues(
      instruments = NULL,
      images = NULL,
      samples = NULL,
      current_visit = NULL
    )
    visit_photo_extensions <- c("jpg", "jpeg", "png", "gif", "bmp", "tiff")

    make_visit_photo_thumbnail <- function(path) {
      if (!requireNamespace("magick", quietly = TRUE)) {
        return(path)
      }
      thumbnail <- tempfile(fileext = ".png")
      converted <- tryCatch({
        image <- magick::image_read(path)
        image <- magick::image_resize(image, "480x360>")
        magick::image_write(image, path = thumbnail, format = "png")
        file.exists(thumbnail) && file.size(thumbnail) > 0
      }, error = function(e) FALSE)
      if (converted) {
        thumbnail
      } else {
        unlink(thumbnail)
        path
      }
    }

    visit_photo_mime_type <- function(extension) {
      switch(
        tolower(extension),
        jpg = "image/jpeg",
        jpeg = "image/jpeg",
        png = "image/png",
        gif = "image/gif",
        bmp = "image/bmp",
        tif = "image/tiff",
        tiff = "image/tiff",
        "application/octet-stream"
      )
    }

    current_visit_photo_files <- function() {
      photos <- input$visit_photos
      if (is.null(photos) || !nrow(photos)) {
        return(data.frame())
      }
      photos
    }

    validate_visit_photos <- function(saved_count = 0L) {
      photos <- current_visit_photo_files()
      has_saved <- saved_count > 0L
      has_uploads <- nrow(photos) > 0L
      photos_taken <- identical(input$photos_taken, "yes")

      if (photos_taken && !has_saved && !has_uploads) {
        stop(
          "Select at least one photo, or choose No if no photos were taken.",
          call. = FALSE
        )
      }
      if (!photos_taken && has_uploads) {
        stop(
          "Photos are selected. Choose Yes, or clear the selected photos before saving.",
          call. = FALSE
        )
      }
      if (has_saved && !photos_taken) {
        stop(
          "This visit already has saved photos. Keep Were photos taken? set to Yes.",
          call. = FALSE
        )
      }
      if (has_uploads) {
        extension <- tolower(tools::file_ext(photos$name))
        invalid_extension <- !extension %in% visit_photo_extensions
        missing_file <- !file.exists(photos$datapath) |
          is.na(file.info(photos$datapath)$size) |
          file.info(photos$datapath)$size <= 0
        if (any(invalid_extension)) {
          stop(
            "Photos must be JPG, PNG, GIF, BMP, or TIFF files.",
            call. = FALSE
          )
        }
        if (any(missing_file)) {
          stop("One or more selected photos could not be read. Select them again.", call. = FALSE)
        }
        if (any(extension %in% c("tif", "tiff")) &&
            !requireNamespace("magick", quietly = TRUE)) {
          stop(
            "TIFF photos cannot be previewed on this server. Select JPG, PNG, GIF, or BMP photos instead.",
            call. = FALSE
          )
        }
      }
      photos
    }

    save_visit_photos <- function(visit_id, form, photos) {
      if (!nrow(photos)) {
        return(invisible(NULL))
      }
      con <- session$userData$AquaCache
      image_type <- DBI::dbGetQuery(
        con,
        "SELECT image_type_id
           FROM files.image_types
          WHERE LOWER(image_type) IN ('field visit', 'sampling event')
          ORDER BY CASE LOWER(image_type)
                     WHEN 'field visit' THEN 0
                     ELSE 1
                   END,
                   image_type_id
          LIMIT 1"
      )$image_type_id
      if (!length(image_type) || is.na(image_type[[1]])) {
        stop("The Field visit image type is not configured.", call. = FALSE)
      }

      for (i in seq_len(nrow(photos))) {
        AquaCache::insertACImage(
          object = photos$datapath[[i]],
          datetime = form$start_utc,
          image_type = as.integer(image_type[[1]]),
          description = paste("Photo from field visit", visit_id),
          tags = c("field visit"),
          # insertACImage expects one plain group name per vector element. The
          # visit form stores a PostgreSQL text[] literal for its SQL writes.
          share_with = array_to_text(form$share_with),
          location = as.integer(form$location_id),
          con = con
        )
        file_hash <- unname(tools::md5sum(photos$datapath[[i]]))
        image_id <- DBI::dbGetQuery(
          con,
          "SELECT image_id FROM files.images WHERE file_hash = $1",
          params = list(file_hash)
        )$image_id
        if (!length(image_id) || is.na(image_id[[1]])) {
          stop("A photo was uploaded but its image record could not be found.", call. = FALSE)
        }
        DBI::dbExecute(
          con,
          "INSERT INTO field.field_visit_images (field_visit_id, image_id)
           VALUES ($1, $2)
           ON CONFLICT (field_visit_id, image_id) DO NOTHING",
          params = list(as.integer(visit_id), as.integer(image_id[[1]]))
        )
      }
      invisible(NULL)
    }

    # Functions for reuse within the module
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

    collect_visit_inputs <- function() {
      scalar_integer <- function(value) {
        if (is.null(value) || !length(value) || is.na(value[[1]])) {
          return(NA_integer_)
        }
        suppressWarnings(as.integer(value[[1]]))
      }
      start_utc <- scalar_utc_datetime(input$visit_datetime_start)
      end_utc <- scalar_utc_datetime(input$visit_datetime_end)
      location_id <- scalar_integer(input$location)
      sub_location_id <- scalar_integer(input$sub_location)
      purpose <- if (isTruthy(input$visit_purpose)) {
        input$visit_purpose
      } else {
        NA_character_
      }
      precip <- input$precip
      precip_type <- if (
        !is.null(precip) &&
          length(precip) &&
          !is.na(precip[[1]]) &&
          nzchar(as.character(precip[[1]]))
      ) {
        as.character(precip[[1]])
      } else {
        "None"
      }
      precip_rate_input <- input$precip_rate
      precip_rate <- if (
        !is.null(precip_rate_input) &&
          length(precip_rate_input) &&
          !is.na(precip_rate_input[[1]]) &&
          nzchar(as.character(precip_rate_input[[1]])) &&
          !identical(precip_type, "None")
      ) {
        as.character(precip_rate_input[[1]])
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
        wind = if (isTruthy(input$weather_wind)) {
          as.character(input$weather_wind[[1]])
        } else {
          NA_character_
        },
        note = note,
        share_with = share_with_to_array(input$share_with)
      )
    }

    reset_visit_form <- function() {
      now_utc <- .POSIXct(Sys.time(), tz = "UTC")
      shinyWidgets::updateAirDateInput(
        session,
        "visit_datetime_start",
        value = now_utc,
        tz = air_datetime_widget_timezone(input$timezone)
      )
      shinyWidgets::updateAirDateInput(
        session,
        "visit_datetime_end",
        clear = TRUE,
        tz = air_datetime_widget_timezone(input$timezone)
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
      visitData$current_visit <- NULL
      selected_sample(NULL)
      shinyjs::reset(ns("visit_photos"))
      updateRadioButtons(session, "photos_taken", selected = "no")
    }

    getModuleData <- function() {
      moduleData$locations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT l.location_id, l.location_code AS location, l.name, lt.type, l.latitude, l.longitude FROM public.locations l INNER JOIN public.location_types lt ON l.location_type = lt.type_id"
      )
      moduleData$sub_locations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT sub_location_id, sub_location_name, location_id FROM public.sub_locations"
      )
      moduleData$parameters <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT p.parameter_id,
                p.param_name,
                p.sample_fraction,
                p.result_speciation,
                u.unit_name AS unit_liquid
           FROM public.parameters AS p
           LEFT JOIN public.units AS u
             ON u.unit_id = p.units_liquid
          ORDER BY p.param_name"
      )
      moduleData$media <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT media_id, media_type FROM public.media_types"
      )
      moduleData$organizations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT organization_id, name FROM public.organizations"
      )
      moduleData$sample_types <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT sample_type_id,
                sample_type,
                requires_location,
                requires_sample_group
           FROM discrete.sample_types
          ORDER BY sample_type"
      )
      moduleData$collection_methods <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT collection_method_id, collection_method
           FROM discrete.collection_methods
          ORDER BY collection_method"
      )
      moduleData$result_types <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT result_type_id, result_type
           FROM discrete.result_types
          ORDER BY result_type"
      )
      moduleData$result_conditions <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT result_condition_id, result_condition
           FROM discrete.result_conditions
          ORDER BY result_condition"
      )
      moduleData$result_value_types <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT result_value_type_id, result_value_type
           FROM discrete.result_value_types
          ORDER BY result_value_type"
      )
      moduleData$sample_fractions <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT sample_fraction_id, sample_fraction
           FROM discrete.sample_fractions
          ORDER BY sample_fraction"
      )
      moduleData$result_speciations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT result_speciation_id, result_speciation
           FROM discrete.result_speciations
          ORDER BY result_speciation"
      )
      moduleData$protocols_methods <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT protocol_id, protocol_name
           FROM discrete.protocols_methods
          ORDER BY protocol_name"
      )
      moduleData$instruments <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT i.instrument_id, i.serial_no, instrument_makes.make, instrument_models.model, instrument_types.type, i.owner FROM instruments.instruments AS i LEFT JOIN instruments.instrument_makes ON i.make = instrument_makes.make_id LEFT JOIN instruments.instrument_models ON i.model = instrument_models.model_id LEFT JOIN instruments.instrument_types ON i.type = instrument_types.type_id ORDER BY i.instrument_id"
      )
      moduleData$users <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT * FROM public.get_shareable_principals_for('field.field_visits');"
      ) # This is a helper function run with SECURITY DEFINER and created by postgres that pulls all user groups (plus public_reader) with select privileges on a table
      moduleData$visit_display <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "
        SELECT v.field_visit_id,
               l.name AS location_name,
               sl.sub_location_name,
               v.start_datetime AS start_datetime_MST,
               v.purpose
        FROM field.field_visits v
        INNER JOIN public.locations l ON v.location_id = l.location_id
        LEFT JOIN public.sub_locations sl ON v.sub_location_id = sl.sub_location_id
        "
      )
    }

    load_visit_samples <- function(visit_id, visit = NULL) {
      con <- session$userData$AquaCache
      visitData$current_visit <- if (is.null(visit)) {
        DBI::dbGetQuery(
          con,
          "SELECT *
             FROM field.field_visits
            WHERE field_visit_id = $1",
          params = list(as.integer(visit_id))
        )
      } else {
        visit
      }
      visitData$samples <- DBI::dbGetQuery(
        con,
        "SELECT s.sample_id,
                s.location_id,
                s.sub_location_id,
                s.media_id,
                s.datetime,
                s.note,
                s.share_with,
                st.sample_type AS sample_type_name,
                count(r.result_id) FILTER (WHERE rt.result_type = 'field')::integer
                  AS field_result_count
           FROM discrete.samples AS s
           JOIN discrete.sample_types AS st
             ON st.sample_type_id = s.sample_type
           LEFT JOIN discrete.results AS r
             ON r.sample_id = s.sample_id
           LEFT JOIN discrete.result_types AS rt
             ON rt.result_type_id = r.result_type
          WHERE s.field_visit_id = $1
          GROUP BY s.sample_id, st.sample_type
          ORDER BY s.datetime, s.sample_id",
        params = list(as.integer(visit_id))
      )
      selected_sample(NULL)
      invisible(NULL)
    }

    load_visit_images <- function(visit_id) {
      visitData$images <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT i.image_id, i.format, i.datetime, i.description
           FROM field.field_visit_images AS vi
           JOIN files.images AS i ON i.image_id = vi.image_id
          WHERE vi.field_visit_id = $1
          ORDER BY i.datetime, i.image_id",
        params = list(as.integer(visit_id))
      )
      invisible(NULL)
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
            3,
            selectizeInput(
              ns("timezone"),
              "Input timezone",
              choices = input_timezone_choices(),
              selected = default_input_timezone(),
              multiple = FALSE,
            )
          ),
          column(
            4,
            shinyWidgets::airDatepickerInput(
              ns("visit_datetime_start"),
              label = "Visit start",
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
          ),
          column(
            5,
            shinyWidgets::airDatepickerInput(
              ns("visit_datetime_end"),
              label = "Visit end (optional)",
              value = NULL,
              range = FALSE,
              multiple = FALSE,
              clearButton = TRUE,
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
        uiOutput(ns("sample_tools")),
        h3("Instruments used"),
        helpText(
          "Instrument association is optional. Save the visit first; the sample and field measurement controls will then appear below."
        ),
        actionButton(ns("choose_instruments"), "Select instruments used"),
        uiOutput(ns("instruments_chosen_ui")),

        radioButtons(
          ns("photos_taken"),
          "Were photos taken?",
          choices = c("Yes" = "yes", "No" = "no"),
          selected = "no"
        ),
        conditionalPanel(
          condition = "input.photos_taken == 'yes'",
          ns = ns,
          helpText(
            "Select one or more JPG, PNG, GIF, BMP, or TIFF photos. Photos are attached to this visit and dated with the visit start time."
          ),
          fileInput(
            ns("visit_photos"),
            "Upload visit photos",
            multiple = TRUE,
            accept = paste0(".", visit_photo_extensions)
          ),
          actionButton(ns("clear_visit_photos"), "Clear selected photos")
        ),
        uiOutput(ns("visit_photo_previews")),
        tags$style(HTML(
          ".visit-photo-card { display: inline-block; vertical-align: top; margin: 0 12px 12px 0; padding: 8px; border: 1px solid #ddd; border-radius: 4px; }\n.visit-photo-card img { width: 180px; height: 135px; object-fit: contain; background: #f7f7f7; }"
        )),

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
    selected_sample <- reactiveVal(NULL)
    pending_field_results <- reactiveVal(data.frame())
    visit_context_is_saved <- function(visit = visitData$current_visit) {
      if (is.null(visit) || !nrow(visit)) {
        return(FALSE)
      }
      form <- collect_visit_inputs()
      saved_location <- suppressWarnings(as.integer(visit$location_id[[1]]))
      saved_sub_location <- suppressWarnings(
        as.integer(visit$sub_location_id[[1]])
      )
      same_integer <- function(left, right) {
        if (is.na(left) || is.na(right)) {
          return(is.na(left) && is.na(right))
        }
        identical(as.integer(left), as.integer(right))
      }
      saved_start <- as.POSIXct(visit$start_datetime[[1]], tz = "UTC")
      form_start <- as.POSIXct(form$start_utc, tz = "UTC")
      start_matches <- !is.na(saved_start) && !is.na(form_start) &&
        abs(as.numeric(saved_start) - as.numeric(form_start)) < 60
      normalize_share <- function(value) {
        value <- array_to_text(value)
        value <- as.character(value)
        sort(unique(value[!is.na(value) & nzchar(value)]))
      }
      identical(normalize_share(form$share_with), normalize_share(visit$share_with)) &&
        same_integer(form$location_id, saved_location) &&
        same_integer(form$sub_location_id, saved_sub_location) &&
        start_matches
    }

    require_saved_visit_context <- function(visit = visitData$current_visit) {
      if (visit_context_is_saved(visit)) {
        return(TRUE)
      }
      showNotification(
        "Save the visit changes before adding or linking samples.",
        type = "warning"
      )
      FALSE
    }

    output$sample_tools <- renderUI({
      visit_id <- selected_visit()
      visit_saved <- !is.null(visit_id) &&
        !is.null(visitData$current_visit) &&
        nrow(visitData$current_visit) > 0L
      tagList(
        hr(),
        h3("Samples and field measurements"),
        if (visit_saved) {
          helpText(
            "A visit can link to many samples. Each field result is stored once on the sample it describes; it is not copied to every sample from the visit."
          )
        } else {
          helpText(
            "Save the visit first to enable sample linking and field measurements. These controls are available here after the visit is saved."
          )
        },
        fluidRow(
          column(
            6,
            if (visit_saved) {
              actionButton(
                ns("create_field_sample"),
                "Create a field sample with a measurement"
              )
            } else {
              shinyjs::disabled(actionButton(
                ns("create_field_sample"),
                "Create a field sample with a measurement"
              ))
            }
          ),
          column(
            6,
            if (visit_saved) {
              actionButton(
                ns("associate_existing_samples"),
                "Associate existing sample(s)"
              )
            } else {
              shinyjs::disabled(actionButton(
                ns("associate_existing_samples"),
                "Associate existing sample(s)"
              ))
            }
          )
        ),
        DT::DTOutput(ns("visit_samples")),
        uiOutput(ns("selected_sample_controls"))
      )
    })

    output$visit_photo_previews <- renderUI({
      saved_images <- visitData$images
      cards <- list()

      if (!is.null(saved_images) && nrow(saved_images)) {
        for (i in seq_len(nrow(saved_images))) {
          image_id <- as.integer(saved_images$image_id[[i]])
          output_id <- paste0("saved_visit_photo_", image_id)
          local({
            id <- output_id
            saved_id <- image_id
            output[[id]] <- renderImage({
              image <- DBI::dbGetQuery(
                session$userData$AquaCache,
                "SELECT format, file FROM files.images WHERE image_id = $1",
                params = list(saved_id)
              )
              if (!nrow(image) || is.null(image$file[[1]]) || !length(image$file[[1]])) {
                return(list(src = NULL))
              }
              extension <- tolower(image$format[[1]])
              source <- tempfile(fileext = paste0(".", extension))
              writeBin(image$file[[1]], source)
              thumbnail <- make_visit_photo_thumbnail(source)
              if (!identical(thumbnail, source)) {
                unlink(source)
              }
              list(
                src = thumbnail,
                alt = paste("Saved photo", saved_id),
                contentType = if (identical(thumbnail, source)) {
                  visit_photo_mime_type(extension)
                } else {
                  "image/png"
                }
              )
            }, deleteFile = TRUE)
          })
          cards[[length(cards) + 1L]] <- tags$div(
            class = "visit-photo-card",
            tags$strong(paste("Saved photo", image_id)),
            imageOutput(ns(output_id), width = "180px", height = "135px")
          )
        }
      }

      if (identical(input$photos_taken, "yes")) {
        photos <- current_visit_photo_files()
        if (nrow(photos)) {
          for (i in seq_len(nrow(photos))) {
            output_id <- paste0("pending_visit_photo_", i)
            photo_path <- photos$datapath[[i]]
            photo_name <- as.character(photos$name[[i]])
            extension <- tolower(tools::file_ext(photo_name))
            local({
              id <- output_id
              path <- photo_path
              name <- photo_name
              ext <- extension
              output[[id]] <- renderImage({
                thumbnail <- if (ext %in% c("tif", "tiff")) {
                  make_visit_photo_thumbnail(path)
                } else {
                  path
                }
                if (ext %in% c("tif", "tiff") && identical(thumbnail, path)) {
                  return(list(src = NULL))
                }
                list(
                  src = thumbnail,
                  alt = name,
                  contentType = if (identical(thumbnail, path)) {
                    visit_photo_mime_type(ext)
                  } else {
                    "image/png"
                  }
                )
              }, deleteFile = extension %in% c("tif", "tiff"))
            })
            cards[[length(cards) + 1L]] <- tags$div(
              class = "visit-photo-card",
              tags$strong(photo_name),
              imageOutput(ns(output_id), width = "180px", height = "135px")
            )
          }
        }
      }

      if (!length(cards)) {
        return()
      }
      tagList(
        h4("Photo previews"),
        tags$div(cards)
      )
    })

    output$visit_samples <- DT::renderDT({
      samples <- visitData$samples
      if (is.null(samples) || !nrow(samples)) {
        return(DT::datatable(
          data.frame(Message = "No samples are associated with this visit yet."),
          rownames = FALSE,
          selection = "none",
          options = list(dom = "t")
        ))
      }
      display <- data.frame(
        `Sample ID` = samples$sample_id,
        `Sample time (UTC)` = format(
          as.POSIXct(samples$datetime, tz = "UTC"),
          "%Y-%m-%d %H:%M",
          tz = "UTC"
        ),
        `Sample type` = samples$sample_type_name,
        `Field results` = samples$field_result_count,
        Note = samples$note,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      display[is.na(display)] <- ""
      DT::datatable(
        display,
        selection = "single",
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    }, server = FALSE)

    output$selected_sample_controls <- renderUI({
      sample <- selected_sample()
      if (is.null(sample)) {
        return(helpText("Select a sample to add a field result to it."))
      }
      tagList(
        p(paste("Selected sample:", sample$sample_id[[1]])),
        actionButton(ns("add_field_result"), "Add a field result to selected sample")
      )
    })

    field_id_choices <- function(
      rows,
      id_column,
      label_column,
      include_blank = FALSE
    ) {
      choices <- stats::setNames(
        as.character(rows[[id_column]]),
        as.character(rows[[label_column]])
      )
      if (include_blank) {
        choices <- c("None" = "", choices)
      }
      choices
    }

    field_result_inputs <- function(include_sample = FALSE) {
      visit <- visitData$current_visit
      existing_sample <- selected_sample()
      result_datetime_default <- if (include_sample && !is.null(visit)) {
        coerce_utc_datetime(visit$start_datetime)
      } else if (!include_sample && !is.null(existing_sample)) {
        coerce_utc_datetime(existing_sample$datetime[[1]])
      } else {
        .POSIXct(Sys.time(), tz = "UTC")
      }
      result_value_type_id <- moduleData$result_value_types$result_value_type_id[
        match("actual", tolower(moduleData$result_value_types$result_value_type))
      ]
      parameter_labels <- moduleData$parameters$param_name
      has_liquid_units <- !is.na(moduleData$parameters$unit_liquid) &
        nzchar(moduleData$parameters$unit_liquid)
      parameter_labels[has_liquid_units] <- paste0(
        parameter_labels[has_liquid_units],
        " (",
        moduleData$parameters$unit_liquid[has_liquid_units],
        ")"
      )
      parameter_choices <- stats::setNames(
        as.character(moduleData$parameters$parameter_id),
        parameter_labels
      )
      common_fields <- tagList(
        h4("Field measurement"),
        fluidRow(
          column(
            6,
            selectizeInput(
              ns("field_parameter"),
              "Parameter",
              choices = parameter_choices,
              options = list(placeholder = "Choose a parameter", maxItems = 1)
            )
          ),
          column(3, numericInput(ns("field_result"), "Result value", value = NA_real_)),
          column(
            3,
            selectInput(
              ns("field_result_value_type"),
              "Result value type",
              choices = field_id_choices(
                moduleData$result_value_types,
                "result_value_type_id",
                "result_value_type"
              ),
              selected = if (length(result_value_type_id)) {
                as.character(result_value_type_id)
              } else {
                NULL
              }
            )
          )
        ),
        fluidRow(
          column(
            6,
            selectInput(
              ns("field_result_condition"),
              "Result condition (optional)",
              choices = field_id_choices(
                moduleData$result_conditions,
                "result_condition_id",
                "result_condition",
                include_blank = TRUE
              ),
              selected = ""
            )
          ),
          column(
            6,
            numericInput(
              ns("field_result_condition_value"),
              "Condition value (when required)",
              value = NA_real_
            )
          )
        ),
        fluidRow(
          column(
            6,
            selectInput(
              ns("field_sample_fraction"),
              "Sample fraction",
              choices = field_id_choices(
                moduleData$sample_fractions,
                "sample_fraction_id",
                "sample_fraction",
                include_blank = TRUE
              ),
              selected = ""
            )
          ),
          column(
            6,
            selectInput(
              ns("field_result_speciation"),
              "Result speciation",
              choices = field_id_choices(
                moduleData$result_speciations,
                "result_speciation_id",
                "result_speciation",
                include_blank = TRUE
              ),
              selected = ""
            )
          )
        ),
        fluidRow(
          column(
            6,
            selectInput(
              ns("field_protocol"),
              "Protocol/method (optional)",
              choices = field_id_choices(
                moduleData$protocols_methods,
                "protocol_id",
                "protocol_name",
                include_blank = TRUE
              ),
              selected = ""
            )
          ),
          column(
            6,
            shinyWidgets::airDatepickerInput(
              ns("field_result_datetime"),
              "Measurement time",
              value = result_datetime_default,
              timepicker = TRUE,
              update_on = "change",
              tz = "UTC",
              timepickerOpts = shinyWidgets::timepickerOptions(
                minutesStep = 1,
                timeFormat = "HH:mm"
              )
            )
          )
        ),
        textAreaInput(
          ns("field_result_note"),
          "Result note (optional)",
          rows = 2
        ),
        actionButton(ns("stage_field_result"), "Add measurement to sample"),
        DT::DTOutput(ns("field_result_preview")),
        actionButton(
          ns("remove_staged_field_result"),
          "Remove selected measurement"
        )
      )
      if (!include_sample) {
        return(common_fields)
      }
      eligible_sample_type_rows <- which(
        !is.na(moduleData$sample_types$requires_location) &
          as.logical(moduleData$sample_types$requires_location) &
          !is.na(moduleData$sample_types$requires_sample_group) &
          !as.logical(moduleData$sample_types$requires_sample_group)
      )
      eligible_sample_types <- moduleData$sample_types[
        eligible_sample_type_rows,
        ,
        drop = FALSE
      ]
      sample_type_labels <- eligible_sample_types$sample_type
      sample_type_default <- which(grepl("field", sample_type_labels, ignore.case = TRUE))
      method_default <- which(grepl(
        "observation",
        moduleData$collection_methods$collection_method,
        ignore.case = TRUE
      ))
      media_default <- which(grepl("water", moduleData$media$media_type, ignore.case = TRUE))
      tagList(
        h4("Sample details"),
        helpText("This creates one sample linked to this visit, then stores the field result on that sample."),
        fluidRow(
          column(
            6,
            shinyWidgets::airDatepickerInput(
              ns("field_sample_datetime"),
              "Sample time",
              value = coerce_utc_datetime(visit$start_datetime),
              timepicker = TRUE,
              update_on = "change",
              tz = "UTC",
              timepickerOpts = shinyWidgets::timepickerOptions(
                minutesStep = 1,
                timeFormat = "HH:mm"
              )
            )
          ),
          column(
            6,
            selectInput(
              ns("field_sample_type"),
              "Sample type",
              choices = field_id_choices(
                eligible_sample_types,
                "sample_type_id",
                "sample_type"
              ),
              selected = if (length(sample_type_default)) {
                as.character(eligible_sample_types$sample_type_id[sample_type_default[[1]]])
              } else {
                NULL
              }
            )
          )
        ),
        fluidRow(
          column(
            4,
            selectInput(
              ns("field_sample_media"),
              "Media",
              choices = field_id_choices(moduleData$media, "media_id", "media_type"),
              selected = if (length(media_default)) {
                as.character(moduleData$media$media_id[media_default[[1]]])
              } else {
                NULL
              }
            )
          ),
          column(
            4,
            selectInput(
              ns("field_sample_method"),
              "Collection method",
              choices = field_id_choices(
                moduleData$collection_methods,
                "collection_method_id",
                "collection_method"
              ),
              selected = if (length(method_default)) {
                as.character(moduleData$collection_methods$collection_method_id[method_default[[1]]])
              } else {
                NULL
              }
            )
          ),
          column(
            4,
            selectInput(
              ns("field_sample_owner"),
              "Sample owner",
              choices = c(
                "Choose an owner" = "",
                field_id_choices(moduleData$organizations, "organization_id", "name")
              ),
              selected = ""
            )
          )
        ),
        textAreaInput(ns("field_sample_note"), "Sample note (optional)", rows = 2),
        common_fields
      )
    }

    collect_field_result <- function() {
      scalar_id <- function(value) {
        if (
          is.null(value) ||
            !length(value) ||
            is.na(value[[1]]) ||
            !nzchar(trimws(as.character(value[[1]])))
        ) {
          return(NA_integer_)
        }
        suppressWarnings(as.integer(value[[1]]))
      }
      scalar_number <- function(value) {
        if (is.null(value) || !length(value) || is.na(value[[1]])) {
          return(NA_real_)
        }
        suppressWarnings(as.numeric(value[[1]]))
      }
      parameter_id <- scalar_id(input$field_parameter)
      result_type_id <- moduleData$result_types$result_type_id[
        match("field", tolower(moduleData$result_types$result_type))
      ]
      parameter_index <- match(parameter_id, moduleData$parameters$parameter_id)
      if (is.na(parameter_id) || is.na(parameter_index) || !length(result_type_id)) {
        stop("Choose a field result parameter.", call. = FALSE)
      }
      result_value_type <- scalar_id(input$field_result_value_type)
      if (is.na(result_value_type)) {
        stop("Choose a result value type.", call. = FALSE)
      }
      result_value <- scalar_number(input$field_result)
      result_condition <- scalar_id(input$field_result_condition)
      if (is.na(result_value) && is.na(result_condition)) {
        stop("Enter a result value or choose a result condition.", call. = FALSE)
      }
      if (!is.na(result_value) && !is.finite(result_value)) {
        stop("Result value must be finite.", call. = FALSE)
      }
      if (!is.na(result_value) && !is.na(result_condition)) {
        stop(
          "Enter a result value or a result condition, not both.",
          call. = FALSE
        )
      }
      if (
        !is.na(result_condition) &&
          !result_condition %in% moduleData$result_conditions$result_condition_id
      ) {
        stop("Choose a valid result condition.", call. = FALSE)
      }
      condition_value <- scalar_number(input$field_result_condition_value)
      if (result_condition %in% c(1L, 2L) && is.na(condition_value)) {
        stop(
          "A condition value is required for below/above-limit results.",
          call. = FALSE
        )
      }
      if (!is.na(condition_value) && !is.finite(condition_value)) {
        stop("Condition value must be finite.", call. = FALSE)
      }
      if (!result_condition %in% c(1L, 2L)) {
        condition_value <- NA_real_
      }
      sample_fraction_id <- scalar_id(input$field_sample_fraction)
      result_speciation_id <- scalar_id(input$field_result_speciation)
      parameter <- moduleData$parameters[parameter_index, , drop = FALSE]
      if (
        isTRUE(as.logical(parameter$sample_fraction[[1]])) &&
          is.na(sample_fraction_id)
      ) {
        stop("Choose the required sample fraction.", call. = FALSE)
      }
      if (
        isTRUE(as.logical(parameter$result_speciation[[1]])) &&
          is.na(result_speciation_id)
      ) {
        stop("Choose the required result speciation.", call. = FALSE)
      }
      measurement_datetime <- scalar_utc_datetime(input$field_result_datetime)
      result <- data.frame(
        parameter_id = parameter_id,
        result_type = as.integer(result_type_id[[1]]),
        protocol_method = scalar_id(input$field_protocol),
        sample_fraction_id = sample_fraction_id,
        result = result_value,
        result_condition = result_condition,
        result_condition_value = condition_value,
        result_value_type = result_value_type,
        result_speciation_id = result_speciation_id,
        laboratory = NA_integer_,
        analysis_datetime = measurement_datetime,
        lab_report_no = NA_character_,
        lab_sample_no = NA_character_,
        grade_type_id = NA_integer_,
        approval_type_id = NA_integer_,
        no_source_update = FALSE,
        note = if (isTruthy(input$field_result_note)) {
          as.character(input$field_result_note)
        } else {
          NA_character_
        },
        stringsAsFactors = FALSE
      )
      result
    }

    output$field_result_preview <- DT::renderDT({
      results <- pending_field_results()
      if (!nrow(results)) {
        return(DT::datatable(
          data.frame(Message = "No measurements have been added yet."),
          rownames = FALSE,
          selection = "none",
          options = list(dom = "t")
        ))
      }
      parameter_index <- match(
        results$parameter_id,
        moduleData$parameters$parameter_id
      )
      condition_index <- match(
        results$result_condition,
        moduleData$result_conditions$result_condition_id
      )
      fraction_index <- match(
        results$sample_fraction_id,
        moduleData$sample_fractions$sample_fraction_id
      )
      speciation_index <- match(
        results$result_speciation_id,
        moduleData$result_speciations$result_speciation_id
      )
      value_label <- ifelse(
        is.na(results$result),
        moduleData$result_conditions$result_condition[condition_index],
        as.character(results$result)
      )
      display <- data.frame(
        Parameter = moduleData$parameters$param_name[parameter_index],
        Result = value_label,
        `Sample fraction` = moduleData$sample_fractions$sample_fraction[fraction_index],
        Speciation = moduleData$result_speciations$result_speciation[speciation_index],
        `Measurement time (UTC)` = format(
          as.POSIXct(results$analysis_datetime, tz = "UTC"),
          "%Y-%m-%d %H:%M",
          tz = "UTC"
        ),
        Note = results$note,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      display[is.na(display)] <- ""
      DT::datatable(
        display,
        selection = "single",
        rownames = FALSE,
        options = list(pageLength = 5, scrollX = TRUE)
      )
    }, server = FALSE)

    observeEvent(input$stage_field_result, {
      tryCatch({
        result <- collect_field_result()
        pending <- pending_field_results()
        if (nrow(pending)) {
          key_columns <- c(
            "parameter_id",
            "protocol_method",
            "sample_fraction_id",
            "result_value_type",
            "result_speciation_id",
            "analysis_datetime"
          )
          make_keys <- function(rows) {
            values <- lapply(key_columns, function(name) {
              value <- as.character(rows[[name]])
              value[is.na(value)] <- ""
              value
            })
            do.call(paste, c(values, sep = "\r"))
          }
          if (make_keys(rbind(pending, result))[[nrow(pending) + 1L]] %in% make_keys(pending)) {
            stop(
              "That parameter and measurement time are already in this sample preview.",
              call. = FALSE
            )
          }
        }
        pending_field_results(rbind(pending, result))
        updateSelectizeInput(session, "field_parameter", selected = character())
        updateNumericInput(session, "field_result", value = NA_real_)
        updateSelectInput(session, "field_result_condition", selected = "")
        updateNumericInput(session, "field_result_condition_value", value = NA_real_)
        updateSelectInput(session, "field_sample_fraction", selected = "")
        updateSelectInput(session, "field_result_speciation", selected = "")
        updateTextAreaInput(session, "field_result_note", value = "")
        showNotification("Measurement added to the sample preview.", type = "message")
      }, error = function(e) {
        showNotification(paste("Could not add measurement:", e$message), type = "error")
      })
    }, ignoreInit = TRUE)

    observeEvent(input$remove_staged_field_result, {
      pending <- pending_field_results()
      selected <- input$field_result_preview_rows_selected
      if (!length(selected) || selected > nrow(pending)) {
        showNotification("Select a preview measurement to remove.", type = "warning")
        return()
      }
      pending_field_results(pending[-selected, , drop = FALSE])
    }, ignoreInit = TRUE)

    available_samples <- reactiveVal(data.frame())

    output$available_samples <- DT::renderDT({
      samples <- available_samples()
      if (!nrow(samples)) {
        return(DT::datatable(
          data.frame(Message = "No unlinked samples were found at this location."),
          rownames = FALSE,
          selection = "none",
          options = list(dom = "t")
        ))
      }
      display <- data.frame(
        `Sample ID` = samples$sample_id,
        `Sample time (UTC)` = format(
          as.POSIXct(samples$datetime, tz = "UTC"),
          "%Y-%m-%d %H:%M",
          tz = "UTC"
        ),
        `Sub-location` = samples$sub_location_name,
        `Sample type` = samples$sample_type,
        Note = samples$note,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      display[is.na(display)] <- ""
      DT::datatable(
        display,
        selection = "multiple",
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 10, scrollX = TRUE)
      )
    }, server = FALSE)

    observeEvent(input$associate_existing_samples, {
      visit <- visitData$current_visit
      if (is.null(visit) || !nrow(visit)) {
        showNotification("Select a field visit first.", type = "warning")
        return()
      }
      if (!require_saved_visit_context(visit)) {
        return()
      }
      available_samples(DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT s.sample_id,
                s.datetime,
                s.sub_location_id,
                sl.sub_location_name,
                st.sample_type,
                s.note
           FROM discrete.samples AS s
           JOIN discrete.sample_types AS st
             ON st.sample_type_id = s.sample_type
           LEFT JOIN public.sub_locations AS sl
             ON sl.sub_location_id = s.sub_location_id
          WHERE s.location_id = $1
            AND s.field_visit_id IS NULL
            AND (
              $2::integer IS NULL
              OR s.sub_location_id IS NOT DISTINCT FROM $2
            )
          ORDER BY s.datetime DESC, s.sample_id DESC",
        params = list(
          as.integer(visit$location_id[[1]]),
          as.integer(visit$sub_location_id[[1]])
        )
      ))
      showModal(modalDialog(
        title = "Associate existing samples",
        size = "l",
        DT::DTOutput(ns("available_samples")),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("link_existing_samples"), "Link selected samples")
        )
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$link_existing_samples, {
      visit <- visitData$current_visit
      samples <- available_samples()
      selected <- input$available_samples_rows_selected
      if (
        is.null(visit) ||
          !length(selected) ||
          any(selected < 1L | selected > nrow(samples))
      ) {
        showNotification("Select one or more samples to link.", type = "warning")
        return()
      }
      if (!require_saved_visit_context(visit)) {
        return()
      }
      tryCatch({
        DBI::dbWithTransaction(session$userData$AquaCache, {
          for (sample_id in samples$sample_id[selected]) {
            updated <- DBI::dbExecute(
              session$userData$AquaCache,
              "UPDATE discrete.samples
                  SET field_visit_id = $1
                WHERE sample_id = $2
                  AND location_id = $3
                  AND (
                    $4::integer IS NULL
                    OR sub_location_id IS NOT DISTINCT FROM $4
                  )
                  AND field_visit_id IS NULL",
              params = list(
                as.integer(visit$field_visit_id[[1]]),
                as.integer(sample_id),
                as.integer(visit$location_id[[1]]),
                as.integer(visit$sub_location_id[[1]])
              )
            )
            if (updated != 1L) {
              stop(
                paste("Sample", sample_id, "is no longer available to link."),
                call. = FALSE
              )
            }
          }
        })
        load_visit_samples(visit$field_visit_id[[1]], visit)
        removeModal()
        showNotification("Selected samples linked to the field visit.", type = "message")
      }, error = function(e) {
        showNotification(paste("Could not link samples:", e$message), type = "error")
      })
    }, ignoreInit = TRUE)

    observeEvent(input$create_field_sample, {
      if (is.null(selected_visit()) || is.null(visitData$current_visit)) {
        showNotification("Save or select a field visit first.", type = "warning")
        return()
      }
      if (!require_saved_visit_context()) {
        return()
      }
      pending_field_results(data.frame())
      showModal(modalDialog(
        title = "Create a field sample with a measurement",
        size = "l",
        field_result_inputs(include_sample = TRUE),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_field_sample"), "Create sample and save field results")
        )
      ))
    }, ignoreInit = TRUE)

    observeEvent(input$save_field_sample, {
      visit <- visitData$current_visit
      visit_id <- selected_visit()
      if (is.null(visit) || is.null(visit_id)) {
        showNotification("Save or select a field visit first.", type = "warning")
        return()
      }
      if (
        !identical(as.integer(visit$field_visit_id[[1]]), as.integer(visit_id)) ||
          !require_saved_visit_context(visit)
      ) {
        return()
      }
      tryCatch({
        sample_datetime <- scalar_utc_datetime(input$field_sample_datetime)
        if (is.na(sample_datetime)) {
          stop("Sample time is required.", call. = FALSE)
        }
        sample_type <- suppressWarnings(as.integer(input$field_sample_type))
        media_id <- suppressWarnings(as.integer(input$field_sample_media))
        collection_method <- suppressWarnings(as.integer(input$field_sample_method))
        owner <- suppressWarnings(as.integer(input$field_sample_owner))
        if (anyNA(c(sample_type, media_id, collection_method, owner))) {
          stop(
            "Choose a sample type, media, collection method, and owner.",
            call. = FALSE
          )
        }
        share_groups <- array_to_text(visit$share_with)
        if (!length(share_groups)) {
          share_groups <- "public_reader"
        }
        results <- pending_field_results()
        if (!nrow(results)) {
          stop("Add at least one field measurement.", call. = FALSE)
        }
        sample <- data.frame(
          location_id = as.integer(visit$location_id[[1]]),
          sub_location_id = as.integer(visit$sub_location_id[[1]]),
          media_id = media_id,
          datetime = sample_datetime,
          collection_method = collection_method,
          sample_type = sample_type,
          owner = owner,
          note = if (isTruthy(input$field_sample_note)) {
            as.character(input$field_sample_note)
          } else {
            NA_character_
          },
          field_visit_id = as.integer(visit_id),
          stringsAsFactors = FALSE
        )
        share_with <- paste0("{", paste(share_groups, collapse = ", "), "}")
        sample$share_with <- share_with
        results$share_with <- rep(share_with, nrow(results))
        sample_id <- AquaCache::addNewDiscrete(
          con = session$userData$AquaCache,
          sample = sample,
          results = results
        )
        load_visit_samples(visit_id, visit)
        pending_field_results(data.frame())
        removeModal()
        showNotification(
          paste(
            "Field sample",
            sample_id,
            "and",
            nrow(results),
            "field result(s) were saved."
          ),
          type = "message"
        )
      }, error = function(e) {
        showNotification(paste("Could not create field sample:", e$message), type = "error")
      })
    }, ignoreInit = TRUE)

    observeEvent(input$add_field_result, {
      sample <- selected_sample()
      if (is.null(sample)) {
        showNotification("Select a sample first.", type = "warning")
        return()
      }
      pending_field_results(data.frame())
      showModal(modalDialog(
        title = paste("Add field result to sample", sample$sample_id[[1]]),
        size = "l",
        field_result_inputs(include_sample = FALSE),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_field_result"), "Save field results")
        )
      ))
      sample_datetime <- coerce_utc_datetime(sample$datetime[[1]])
      shinyWidgets::updateAirDateInput(
        session,
        "field_result_datetime",
        value = sample_datetime,
        tz = "UTC"
      )
    }, ignoreInit = TRUE)

    observeEvent(input$save_field_result, {
      sample <- selected_sample()
      if (is.null(sample)) {
        showNotification("Select a sample first.", type = "warning")
        return()
      }
      tryCatch({
        results <- pending_field_results()
        if (!nrow(results)) {
          stop("Add at least one field measurement.", call. = FALSE)
        }
        results <- AquaCache:::normalize_discrete_result_matrix_states(
          con = session$userData$AquaCache,
          sample_media_id = as.integer(sample$media_id[[1]]),
          results = results
        )
        share_groups <- array_to_text(sample$share_with[[1]])
        if (!length(share_groups)) {
          share_groups <- "public_reader"
        }
        share_with <- paste0("{", paste(share_groups, collapse = ", "), "}")
        DBI::dbWithTransaction(session$userData$AquaCache, {
          for (i in seq_len(nrow(results))) {
            result <- results[i, , drop = FALSE]
            DBI::dbGetQuery(
              session$userData$AquaCache,
              "INSERT INTO discrete.results (
               sample_id,
               result_type,
               parameter_id,
               protocol_method,
               sample_fraction_id,
               result,
               result_condition,
               result_condition_value,
               result_value_type,
               result_speciation_id,
               laboratory,
               analysis_datetime,
               lab_report_no,
               lab_sample_no,
               grade_type_id,
               approval_type_id,
               matrix_state_id,
               no_source_update,
               note,
               share_with
             ) VALUES (
               $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13,
               $14, $15, $16, $17, $18, $19, $20::text[]
             )
             RETURNING result_id",
            params = list(
              as.integer(sample$sample_id[[1]]),
              as.integer(result$result_type[[1]]),
              as.integer(result$parameter_id[[1]]),
              as.integer(result$protocol_method[[1]]),
              as.integer(result$sample_fraction_id[[1]]),
              as.numeric(result$result[[1]]),
              as.integer(result$result_condition[[1]]),
              as.numeric(result$result_condition_value[[1]]),
              as.integer(result$result_value_type[[1]]),
              as.integer(result$result_speciation_id[[1]]),
              as.integer(result$laboratory[[1]]),
              as.POSIXct(result$analysis_datetime[[1]], tz = "UTC"),
              as.character(result$lab_report_no[[1]]),
              as.character(result$lab_sample_no[[1]]),
              as.integer(result$grade_type_id[[1]]),
              as.integer(result$approval_type_id[[1]]),
              as.integer(result$matrix_state_id[[1]]),
              isTRUE(result$no_source_update[[1]]),
              as.character(result$note[[1]]),
              as.character(share_with)
              )
            )
          }
        })
        load_visit_samples(selected_visit(), visitData$current_visit)
        pending_field_results(data.frame())
        removeModal()
        showNotification(
          paste(nrow(results), "field result(s) saved."),
          type = "message"
        )
      }, error = function(e) {
        showNotification(paste("Could not save field result:", e$message), type = "error")
      })
    }, ignoreInit = TRUE)

    observeEvent(input$visit_samples_rows_selected, {
      selected <- input$visit_samples_rows_selected
      samples <- visitData$samples
      if (
        length(selected) != 1L ||
          is.null(samples) ||
          selected > nrow(samples)
      ) {
        selected_sample(NULL)
      } else {
        selected_sample(samples[selected, , drop = FALSE])
      }
    }, ignoreInit = TRUE)

    observeEvent(
      input$timezone,
      {
        new_timezone <- normalize_input_timezone(input$timezone)
        shift_datetime_input_timezone("visit_datetime_start", new_timezone)
        shift_datetime_input_timezone("visit_datetime_end", new_timezone)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$mode,
      {
        if (identical(input$mode, "add")) {
          selected_visit(NULL)
          reset_visit_form()
          DT::dataTableProxy("visit_table") |> DT::selectRows(NULL)
        } else if (identical(input$mode, "modify")) {
          DT::dataTableProxy("visit_table") |> DT::selectRows(NULL)
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
        DT::dataTableProxy("visit_table") |> DT::selectRows(NULL)
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
          selected_sample(NULL)
          visitData$samples <- NULL
          visitData$images <- NULL
          visitData$current_visit <- NULL
          shinyjs::reset(ns("visit_photos"))
          updateRadioButtons(session, "photos_taken", selected = "no")
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
          load_visit_images(visit_id)
          shinyjs::reset(ns("visit_photos"))
          updateRadioButtons(
            session,
            "photos_taken",
            selected = if (nrow(visitData$images)) "yes" else "no"
          )

          # Populate inputs with data from selected visit
          visit_data <- DBI::dbGetQuery(
            session$userData$AquaCache,
            "SELECT * FROM field.field_visits WHERE field_visit_id = $1",
            params = list(visit_id)
          )
          load_visit_samples(visit_id, visit_data)
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

          start_value <- coerce_utc_datetime(visit_data$start_datetime)
          shinyWidgets::updateAirDateInput(
            session,
            "visit_datetime_start",
            value = start_value,
            tz = air_datetime_widget_timezone(input$timezone)
          )
          end_value <- coerce_utc_datetime(visit_data$end_datetime)
          if (is.na(end_value)) {
            shinyWidgets::updateAirDateInput(
              session,
              "visit_datetime_end",
              clear = TRUE,
              tz = air_datetime_widget_timezone(input$timezone)
            )
          } else {
            shinyWidgets::updateAirDateInput(
              session,
              "visit_datetime_end",
              value = end_value,
              tz = air_datetime_widget_timezone(input$timezone)
            )
          }

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
          share_groups <- array_to_text(visit_data$share_with)
          if (!length(share_groups)) {
            share_groups <- "public_reader"
          }
          updateSelectizeInput(
            session,
            "share_with",
            selected = share_groups
          )
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
        if (
          !is.na(form$sub_location_id) &&
            !any(
              moduleData$sub_locations$sub_location_id == form$sub_location_id &
                moduleData$sub_locations$location_id == form$location_id
            )
        ) {
          showNotification(
            "The selected sub-location does not belong to this location.",
            type = "error"
          )
          return()
        }
        photos <- tryCatch(
          validate_visit_photos(),
          error = function(e) {
            showNotification(e$message, type = "error")
            NULL
          }
        )
        if (is.null(photos)) {
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
            $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13::text[]
          )
          RETURNING field_visit_id;
        "

        tryCatch(
          {
            created_visit_id <- DBI::dbWithTransaction(
              session$userData$AquaCache,
              {
                res <- DBI::dbGetQuery(
                  session$userData$AquaCache,
                  insert_sql,
                  params = params
                )
                visit_id <- res$field_visit_id[1]
                if (length(visitData$instruments) > 0) {
                  for (instrument_id in visitData$instruments) {
                    DBI::dbExecute(
                      session$userData$AquaCache,
                      "INSERT INTO field.field_visit_instruments (field_visit_id, instrument_id) VALUES ($1, $2)",
                      params = list(visit_id, instrument_id)
                    )
                  }
                }
                save_visit_photos(visit_id, form, photos)
                visit_id
              }
            )

            getModuleData()
            selected_visit(as.integer(created_visit_id))
            load_visit_samples(created_visit_id)
            load_visit_images(created_visit_id)
            shinyjs::reset(ns("visit_photos"))
            if (nrow(visitData$images)) {
              updateRadioButtons(session, "photos_taken", selected = "yes")
            }
            showNotification(
              "Field visit added. You can now create a field sample or link existing samples below.",
              type = "message"
            )
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
        if (
          !is.na(form$sub_location_id) &&
            !any(
              moduleData$sub_locations$sub_location_id == form$sub_location_id &
                moduleData$sub_locations$location_id == form$location_id
            )
        ) {
          showNotification(
            "The selected sub-location does not belong to this location.",
            type = "error"
          )
          return()
        }
        photos <- tryCatch(
          validate_visit_photos(
            saved_count = if (is.null(visitData$images)) 0L else nrow(visitData$images)
          ),
          error = function(e) {
            showNotification(e$message, type = "error")
            NULL
          }
        )
        if (is.null(photos)) {
          return()
        }
        linked_sample_mismatch <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT EXISTS (
             SELECT 1
               FROM discrete.samples
              WHERE field_visit_id = $1
                AND (
                  location_id IS DISTINCT FROM $2
                  OR (
                    $3::integer IS NOT NULL
                    AND sub_location_id IS DISTINCT FROM $3
                  )
                )
           ) AS has_mismatch",
          params = list(
            as.integer(visit_id),
            as.integer(form$location_id),
            as.integer(form$sub_location_id)
          )
        )$has_mismatch[[1]]
        if (isTRUE(linked_sample_mismatch)) {
          showNotification(
            "The visit location or sub-location cannot be changed while linked samples would no longer match it.",
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
            precip_current_rate = $7,
            precip_24h_mm = $8,
            precip_48h_mm = $9,
            air_temp_c = $10,
            wind = $11,
            note = $12,
            share_with = $13::text[]
          WHERE field_visit_id = $14;
        "

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
          form$share_with,
          visit_id
        )

        tryCatch(
          {
            DBI::dbWithTransaction(
              session$userData$AquaCache,
              {
                updated <- DBI::dbExecute(
                  session$userData$AquaCache,
                  update_sql,
                  params = params
                )
                if (updated != 1L) {
                  stop(
                    "The selected field visit was not updated.",
                    call. = FALSE
                  )
                }
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "DELETE FROM field.field_visit_instruments WHERE field_visit_id = $1",
                  params = list(visit_id)
                )
                if (length(visitData$instruments) > 0) {
                  for (instrument_id in visitData$instruments) {
                    DBI::dbExecute(
                      session$userData$AquaCache,
                      "INSERT INTO field.field_visit_instruments (field_visit_id, instrument_id) VALUES ($1, $2)",
                      params = list(visit_id, instrument_id)
                    )
                  }
                }
                save_visit_photos(visit_id, form, photos)
              }
            )

            load_visit_samples(visit_id)
            load_visit_images(visit_id)
            shinyjs::reset(ns("visit_photos"))
            if (nrow(visitData$images)) {
              updateRadioButtons(session, "photos_taken", selected = "yes")
            }
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
                DT::dataTableProxy("visit_table") |>
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

    observeEvent(
      input$clear_visit_photos,
      shinyjs::reset(ns("visit_photos")),
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
