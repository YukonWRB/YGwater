# UI and server code for adding or modifying compound continuous timeseries

addCompoundTimeseriesUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(
      HTML(sprintf(
        "
      #%s.accordion {
        --bs-accordion-bg:          #FFFCF5;
        --bs-accordion-btn-bg:      #FBE5B2;
        --bs-accordion-active-bg:   #FBE5B2;
      }
    ",
        ns("accordion1")
      )),
      HTML(sprintf(
        "
      #%s.accordion {
        --bs-accordion-bg:          #E5F4F6;
        --bs-accordion-btn-bg:      #0097A9;
        --bs-accordion-active-bg:   #0097A9;
      }
    ",
        ns("accordion2")
      ))
    ),
    page_fluid(
      uiOutput(ns("banner")),
      uiOutput(ns("ui"))
    )
  )
}

addCompoundTimeseries <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "addCompoundTimeseries"
      )
    })

    moduleData <- reactiveValues()
    selected_tsid <- reactiveVal(NULL)
    member_rows <- reactiveVal(data.frame(
      member_alias = character(0),
      member_timeseries_id = integer(0),
      member_priority = integer(0),
      use_from = as.POSIXct(character(0), tz = "UTC"),
      use_to = as.POSIXct(character(0), tz = "UTC"),
      alignment_tolerance_seconds = numeric(0),
      reuse_member_values = logical(0),
      stringsAsFactors = FALSE
    ))

    safe_text <- function(x) {
      ifelse(is.na(x), "", as.character(x))
    }

    nullable_integer <- function(x) {
      if (is.null(x) || !length(x)) {
        return(NA_integer_)
      }
      value <- x[[1]]
      if (is.character(value)) {
        value <- trimws(value)
        if (!nzchar(value)) {
          return(NA_integer_)
        }
      }
      if (is.na(value)) {
        return(NA_integer_)
      }
      as.integer(value)
    }

    nullable_numeric <- function(x) {
      if (is.null(x) || !length(x)) {
        return(NA_real_)
      }
      value <- x[[1]]
      if (is.numeric(value)) {
        if (is.na(value)) {
          return(NA_real_)
        }
        return(as.numeric(value))
      }
      value <- trimws(as.character(value))
      if (!nzchar(value) || identical(tolower(value), "na")) {
        return(NA_real_)
      }
      value <- suppressWarnings(as.numeric(value))
      if (is.na(value)) {
        return(NA_real_)
      }
      value
    }

    nullable_text <- function(x) {
      if (is.null(x) || !length(x)) {
        return(NA_character_)
      }
      value <- trimws(as.character(x[[1]]))
      if (is.na(value) || !nzchar(value)) {
        return(NA_character_)
      }
      value
    }

    nullable_logical <- function(x) {
      if (is.null(x) || !length(x) || is.na(x[[1]])) {
        return(FALSE)
      }
      isTRUE(x[[1]])
    }

    tolerance_unit_seconds <- function(unit) {
      unit <- nullable_text(unit)
      if (is.na(unit)) {
        return(60)
      }
      switch(
        unit,
        seconds = 1,
        minutes = 60,
        hours = 3600,
        days = 86400,
        60
      )
    }

    member_tolerance_seconds <- function(value, unit) {
      value <- nullable_numeric(value)
      if (is.na(value)) {
        return(NA_real_)
      }
      value * tolerance_unit_seconds(unit)
    }

    split_tolerance_seconds <- function(seconds) {
      seconds <- nullable_numeric(seconds)
      if (is.na(seconds)) {
        return(list(value = NA_real_, unit = "minutes"))
      }
      units <- c(days = 86400, hours = 3600, minutes = 60, seconds = 1)
      for (unit in names(units)) {
        converted <- seconds / units[[unit]]
        if (abs(converted - round(converted)) < .Machine$double.eps^0.5) {
          return(list(value = converted, unit = unit))
        }
      }
      list(value = seconds, unit = "seconds")
    }

    tolerance_label <- function(seconds) {
      seconds <- nullable_numeric(seconds)
      if (is.na(seconds)) {
        return("")
      }
      split <- split_tolerance_seconds(seconds)
      unit_label <- sub("s$", "", split$unit)
      if (!isTRUE(all.equal(split$value, 1))) {
        unit_label <- split$unit
      }
      paste(format(split$value, scientific = FALSE, trim = TRUE), unit_label)
    }

    format_z_value <- function(x) {
      x <- as.numeric(x)
      x <- x[!is.na(x)]
      if (!length(x)) {
        return(character(0))
      }
      trimws(format(x, scientific = FALSE, digits = 15, trim = TRUE))
    }

    utc_label <- function(x) {
      x <- as.POSIXct(x, tz = "UTC")
      ifelse(
        is.na(x),
        "",
        format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")
      )
    }

    update_z_selectize <- function(
      selected = input$z,
      location_id = nullable_integer(input$location),
      sub_location_id = nullable_integer(input$sub_location)
    ) {
      z_values <- numeric(0)

      if (
        !is.null(moduleData$locations_z) &&
          nrow(moduleData$locations_z) > 0 &&
          !is.na(location_id)
      ) {
        z_rows <- moduleData$locations_z[
          moduleData$locations_z$location_id == location_id,
          ,
          drop = FALSE
        ]
        if (is.na(sub_location_id)) {
          z_rows <- z_rows[is.na(z_rows$sub_location_id), , drop = FALSE]
        } else {
          z_rows <- z_rows[
            z_rows$sub_location_id == sub_location_id,
            ,
            drop = FALSE
          ]
        }
        if (nrow(z_rows) > 0) {
          z_values <- sort(unique(as.numeric(z_rows$z_meters)))
        }
      }

      selected_numeric <- nullable_numeric(selected)
      if (!is.na(selected_numeric)) {
        z_values <- sort(unique(c(z_values, selected_numeric)))
      }

      choice_labels <- format_z_value(z_values)
      updateSelectizeInput(
        session,
        "z",
        choices = stats::setNames(choice_labels, choice_labels),
        selected = if (is.na(selected_numeric)) {
          character(0)
        } else {
          format_z_value(selected_numeric)
        }
      )
    }

    supported_matrix_state_ids <- function(parameter_id) {
      parameter_id <- nullable_integer(parameter_id)
      if (
        is.na(parameter_id) ||
          is.null(moduleData$parameters) ||
          nrow(moduleData$parameters) == 0 ||
          is.null(moduleData$matrix_states) ||
          nrow(moduleData$matrix_states) == 0
      ) {
        return(integer(0))
      }

      param_row <- moduleData$parameters[
        moduleData$parameters$parameter_id == parameter_id,
        ,
        drop = FALSE
      ]
      if (nrow(param_row) == 0) {
        return(integer(0))
      }

      supported <- vapply(
        seq_len(nrow(moduleData$matrix_states)),
        function(i) {
          unit_col <- paste0(
            "units_",
            moduleData$matrix_states$matrix_state_code[[i]]
          )
          unit_col %in%
            names(param_row) &&
            !is.na(param_row[[unit_col]][[1]]) &&
            nzchar(param_row[[unit_col]][[1]])
        },
        logical(1)
      )

      as.integer(moduleData$matrix_states$matrix_state_id[supported])
    }

    update_matrix_state_selectize <- function(
      selected = nullable_integer(input$matrix_state),
      parameter_id = nullable_integer(input$parameter)
    ) {
      available_ids <- supported_matrix_state_ids(parameter_id)
      if (!length(available_ids) && is.na(parameter_id)) {
        available_ids <- as.integer(moduleData$matrix_states$matrix_state_id)
      }

      matrix_rows <- moduleData$matrix_states[
        moduleData$matrix_states$matrix_state_id %in% available_ids,
        ,
        drop = FALSE
      ]
      matrix_rows <- matrix_rows[
        order(matrix_rows$matrix_state_name),
        ,
        drop = FALSE
      ]

      updateSelectizeInput(
        session,
        "matrix_state",
        choices = stats::setNames(
          matrix_rows$matrix_state_id,
          matrix_rows$matrix_state_name
        ),
        selected = if (is.na(selected)) character(0) else selected
      )
    }

    validate_timeseries_matrix_state <- function(
      parameter_id,
      media_id,
      matrix_state_id
    ) {
      parameter_id <- nullable_integer(parameter_id)
      media_id <- nullable_integer(media_id)
      matrix_state_id <- nullable_integer(matrix_state_id)

      if (is.na(parameter_id)) {
        return("Please select a parameter.")
      }
      if (is.na(media_id)) {
        return("Please select a media type.")
      }
      if (is.na(matrix_state_id)) {
        return("Please select a matrix state.")
      }
      if (!matrix_state_id %in% supported_matrix_state_ids(parameter_id)) {
        return(
          "The selected matrix state has no unit configured for this parameter."
        )
      }

      NULL
    }

    get_or_create_location_z_id <- function(
      con,
      location_id,
      sub_location_id = NA_integer_,
      z_value
    ) {
      location_id <- nullable_integer(location_id)
      sub_location_id <- nullable_integer(sub_location_id)
      z_value <- nullable_numeric(z_value)

      if (is.na(z_value)) {
        return(NA_integer_)
      }
      if (is.na(location_id)) {
        stop("A location is required to resolve elevation/depth.")
      }

      existing <- DBI::dbGetQuery(
        con,
        "
        SELECT z_id
        FROM public.locations_z
        WHERE location_id = $1
          AND sub_location_id IS NOT DISTINCT FROM $2
          AND z_meters = $3
        ",
        params = list(location_id, sub_location_id, z_value)
      )
      if (nrow(existing) > 0) {
        return(as.integer(existing$z_id[[1]]))
      }

      as.integer(DBI::dbGetQuery(
        con,
        "INSERT INTO public.locations_z (location_id, sub_location_id, z_meters) VALUES ($1, $2, $3) RETURNING z_id;",
        params = list(location_id, sub_location_id, z_value)
      )[[1]])
    }

    timeseries_labels <- function(df) {
      if (is.null(df) || nrow(df) == 0) {
        return(character(0))
      }

      sprintf(
        "ID %s | %s | %s | %s | %s | %s | %s",
        df$timeseries_id,
        safe_text(df$location_name),
        safe_text(df$parameter_name),
        safe_text(df$units),
        safe_text(df$aggregation_type),
        safe_text(df$recording_rate),
        safe_text(df$timeseries_type)
      )
    }

    getModuleData <- function() {
      con <- session$userData$AquaCache

      moduleData$timeseries <- DBI::dbGetQuery(
        con,
        paste(
          "SELECT ts.timeseries_id, ts.location_id, ts.sub_location_id,",
          "ts.timezone_daily_calc, lz.z_meters AS z, ts.z_id, ts.media_id,",
          "ts.parameter_id, ts.matrix_state_id, ts.aggregation_type_id,",
          "ts.sensor_priority,",
          "ts.share_with, ts.note,",
          "ts.publicly_visible,",
          "ts.timeseries_type",
          "FROM continuous.timeseries ts",
          "LEFT JOIN public.locations_z lz ON ts.z_id = lz.z_id"
        )
      )

      moduleData$timeseries_display <- DBI::dbGetQuery(
        con,
        paste(
          "SELECT md.timeseries_id, md.location_id,",
          "md.location_name, md.alias_name, md.depth_height_m,",
          "md.parameter_name, md.units, md.media_type,",
          "md.aggregation_type, md.recording_rate, md.sensor_priority,",
          "md.start_datetime, md.end_datetime, md.last_new_data,",
          "md.timeseries_type_code, md.timeseries_type,",
          "ts.active, ts.publicly_visible",
          "FROM continuous.timeseries_metadata_en md",
          "INNER JOIN continuous.timeseries ts",
          "ON md.timeseries_id = ts.timeseries_id",
          "ORDER BY md.location_name, md.parameter_name,",
          "md.media_type, md.aggregation_type,",
          "md.recording_rate, md.timeseries_id"
        )
      )

      moduleData$compound_display <- moduleData$timeseries_display[
        moduleData$timeseries_display$timeseries_type_code == "compound",
        ,
        drop = FALSE
      ]

      moduleData$compound_headers <- DBI::dbGetQuery(
        con,
        "SELECT timeseries_id, expression_sql FROM continuous.timeseries_compounds"
      )
      moduleData$compound_members <- DBI::dbGetQuery(
        con,
        paste(
          "SELECT timeseries_id, member_alias, member_timeseries_id,",
          "member_priority, use_from, use_to,",
          "EXTRACT(EPOCH FROM alignment_tolerance)::double precision",
          "AS alignment_tolerance_seconds, reuse_member_values",
          "FROM continuous.timeseries_compound_members",
          "ORDER BY timeseries_id, member_priority, member_alias"
        )
      )

      moduleData$locations <- DBI::dbGetQuery(
        con,
        "SELECT l.location_id, l.name, lt.type FROM public.locations l INNER JOIN public.location_types lt ON l.location_type = lt.type_id ORDER BY l.name ASC"
      )
      moduleData$sub_locations <- DBI::dbGetQuery(
        con,
        "SELECT sub_location_id, sub_location_name, location_id FROM public.sub_locations ORDER BY sub_location_name ASC"
      )
      moduleData$locations_z <- DBI::dbGetQuery(
        con,
        paste(
          "SELECT z_id, location_id, sub_location_id, z_meters",
          "FROM public.locations_z",
          "ORDER BY location_id ASC, sub_location_id ASC NULLS FIRST, z_meters ASC"
        )
      )
      moduleData$parameters <- DBI::dbGetQuery(
        con,
        paste(
          "SELECT p.parameter_id, p.param_name,",
          "ul.unit_name AS units_liquid,",
          "us.unit_name AS units_solid,",
          "ug.unit_name AS units_gas",
          "FROM public.parameters p",
          "LEFT JOIN public.units ul ON p.units_liquid = ul.unit_id",
          "LEFT JOIN public.units us ON p.units_solid = us.unit_id",
          "LEFT JOIN public.units ug ON p.units_gas = ug.unit_id",
          "ORDER BY p.param_name ASC"
        )
      )
      moduleData$matrix_states <- DBI::dbGetQuery(
        con,
        paste(
          "SELECT matrix_state_id, matrix_state_code, matrix_state_name",
          "FROM public.matrix_states",
          "ORDER BY matrix_state_name ASC"
        )
      )
      moduleData$media <- DBI::dbGetQuery(
        con,
        paste(
          "SELECT media_id, media_type, default_matrix_state_id",
          "FROM public.media_types ORDER BY media_type ASC"
        )
      )
      moduleData$aggregation_types <- DBI::dbGetQuery(
        con,
        "SELECT aggregation_type_id, aggregation_type FROM continuous.aggregation_types ORDER BY aggregation_type ASC"
      )
      moduleData$users <- DBI::dbGetQuery(
        con,
        "SELECT * FROM public.get_shareable_principals_for('continuous.timeseries') ORDER BY role_name ASC;"
      )
    }

    getModuleData()

    member_display <- function(df = member_rows()) {
      if (is.null(df) || nrow(df) == 0) {
        return(data.frame(
          member_alias = character(0),
          member_timeseries_id = integer(0),
          member_timeseries = character(0),
          timeseries_type = character(0),
          active = logical(0),
          sensor_priority = integer(0),
          member_priority = integer(0),
          member_start_datetime = character(0),
          member_end_datetime = character(0),
          use_from = character(0),
          use_to = character(0),
          alignment_tolerance = character(0),
          reuse_member_values = logical(0),
          stringsAsFactors = FALSE
        ))
      }

      md <- moduleData$timeseries_display
      idx <- match(df$member_timeseries_id, md$timeseries_id)
      out <- data.frame(
        member_alias = df$member_alias,
        member_timeseries_id = df$member_timeseries_id,
        member_timeseries = timeseries_labels(md[idx, , drop = FALSE]),
        timeseries_type = md$timeseries_type[idx],
        active = md$active[idx],
        sensor_priority = md$sensor_priority[idx],
        member_priority = df$member_priority,
        member_start_datetime = utc_label(md$start_datetime[idx]),
        member_end_datetime = utc_label(md$end_datetime[idx]),
        use_from = utc_label(df$use_from),
        use_to = utc_label(df$use_to),
        alignment_tolerance = vapply(
          df$alignment_tolerance_seconds,
          tolerance_label,
          character(1)
        ),
        reuse_member_values = df$reuse_member_values,
        stringsAsFactors = FALSE
      )
      out
    }

    output$ui <- renderUI({
      req(
        moduleData$locations,
        moduleData$sub_locations,
        moduleData$locations_z,
        moduleData$parameters,
        moduleData$matrix_states,
        moduleData$media,
        moduleData$aggregation_types,
        moduleData$users,
        moduleData$timeseries_display,
        moduleData$compound_display
      )

      tagList(
        actionButton(
          ns("reload_module"),
          "Reload module data",
          icon = icon("refresh")
        ),
        tags$div(
          class = "alert alert-info",
          "Derived (calculated) or composite (stitched-together) timeseries are calculated on-the-fly from basic or other derived/composite timeseries. They are resolved from member timeseries and a priority order or equation and cannot receive uploaded measurements or instrument deployment links directly."
        ),
        radioButtons(
          ns("mode"),
          NULL,
          choices = c(
            "Add new derived timeseries" = "add",
            "Modify existing derived timeseries" = "modify"
          ),
          inline = TRUE
        ),
        conditionalPanel(
          condition = "input.mode == 'modify'",
          ns = ns,
          accordion(
            id = ns("accordion1"),
            open = "compound_table_panel",
            accordion_panel(
              id = ns("compound_table_panel"),
              title = "Select derived timeseries to modify",
              DT::DTOutput(ns("compound_table"))
            )
          )
        ),
        fluidRow(
          column(
            4,
            selectizeInput(
              ns("location"),
              "Location",
              choices = stats::setNames(
                moduleData$locations$location_id,
                moduleData$locations$name
              ),
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Select a location"),
              width = "100%"
            )
          ),
          column(
            4,
            selectizeInput(
              ns("sub_location"),
              "Sub-location",
              choices = stats::setNames(
                moduleData$sub_locations$sub_location_id,
                moduleData$sub_locations$sub_location_name
              ),
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = "Optional"),
              width = "100%"
            )
          ),
          column(
            4,
            selectizeInput(
              ns("tz"),
              "Timezone for daily aggregation",
              choices = c(-12:14),
              selected = -7
            )
          )
        ),
        selectizeInput(
          ns("z"),
          "Elevation or depth, m",
          choices = character(0),
          multiple = TRUE,
          options = list(
            maxItems = 1,
            placeholder = "Optional",
            create = TRUE,
            createFilter = "^-?(?:[0-9]+|[0-9]*[.][0-9]+)$",
            createOnBlur = TRUE,
            persist = FALSE,
            plugins = list("clear_button")
          ),
          width = "100%"
        ),
        splitLayout(
          cellWidths = c("34%", "33%", "33%"),
          selectizeInput(
            ns("parameter"),
            "Parameter",
            choices = stats::setNames(
              moduleData$parameters$parameter_id,
              moduleData$parameters$param_name
            ),
            multiple = TRUE,
            options = list(
              maxItems = 1,
              placeholder = "Select a parameter",
              dropdownParent = "body"
            ),
            width = "100%"
          ),
          selectizeInput(
            ns("media"),
            "Media",
            choices = stats::setNames(
              moduleData$media$media_id,
              moduleData$media$media_type
            ),
            multiple = TRUE,
            options = list(
              maxItems = 1,
              placeholder = "Select media type",
              dropdownParent = "body"
            ),
            width = "100%"
          ),
          selectizeInput(
            ns("matrix_state"),
            "Matrix state",
            choices = stats::setNames(
              moduleData$matrix_states$matrix_state_id,
              moduleData$matrix_states$matrix_state_name
            ),
            multiple = TRUE,
            options = list(
              maxItems = 1,
              placeholder = "Select matrix state",
              dropdownParent = "body"
            ),
            width = "100%"
          )
        ),
        selectizeInput(
          ns("aggregation_type"),
          "Aggregation type",
          choices = stats::setNames(
            moduleData$aggregation_types$aggregation_type_id,
            moduleData$aggregation_types$aggregation_type
          ),
          multiple = TRUE,
          options = list(
            maxItems = 1,
            placeholder = "Select aggregation",
            dropdownParent = "body"
          ),
          width = "100%"
        ),
        selectizeInput(
          ns("sensor_priority"),
          "Timeseries priority metadata",
          choices = c("Primary" = 1, "Secondary" = 2, "Tertiary" = 3),
          selected = 1,
          multiple = TRUE,
          options = list(
            maxItems = 1,
            placeholder = "Select priority",
            dropdownParent = "body"
          ),
          width = "100%"
        ),
        selectizeInput(
          ns("share_with"),
          "Share with groups",
          choices = moduleData$users$role_name,
          selected = "public_reader",
          multiple = TRUE,
          width = "100%"
        ),
        checkboxInput(
          ns("publicly_visible"),
          "Publicly visible",
          value = TRUE
        ),
        accordion(
          id = ns("accordion2"),
          open = "definition_panel",
          accordion_panel(
            id = ns("definition_panel"),
            title = "Derived timeseries definition",
            textAreaInput(
              ns("expression_sql"),
              "Expression SQL (blank uses first available member by priority)",
              value = "",
              rows = 3,
              placeholder = "Leave empty if not using (e.g. using priority-based stitching) OR enter SQL expression using the 'Member alias' defined below for each timeseries, i.e. cond / (1 + 0.0191 * (temp - 25))",
              width = "100%"
            ),
            fluidRow(
              column(
                3,
                textInput(
                  ns("member_alias"),
                  "Member alias",
                  value = "",
                  placeholder = "primary_level"
                )
              ),
              column(
                5,
                selectizeInput(
                  ns("member_timeseries_id"),
                  "Member timeseries",
                  choices = stats::setNames(
                    moduleData$timeseries_display$timeseries_id,
                    timeseries_labels(moduleData$timeseries_display)
                  ),
                  multiple = TRUE,
                  options = list(
                    maxItems = 1,
                    placeholder = "Select a source timeseries"
                  ),
                  width = "100%"
                )
              ),
              column(
                2,
                numericInput(
                  ns("member_priority"),
                  "Priority",
                  value = 1,
                  min = 1,
                  step = 1,
                  width = "100%"
                )
              ),
              column(
                2,
                selectizeInput(
                  ns("member_timezone"),
                  "Window timezone",
                  choices = input_timezone_choices(),
                  selected = default_input_timezone(),
                  multiple = FALSE,
                  width = "100%"
                )
              )
            ),
            splitLayout(
              cellWidths = c("50%", "50%"),
              shinyWidgets::airDatepickerInput(
                ns("member_use_from"),
                "Use from",
                value = NULL,
                range = FALSE,
                multiple = FALSE,
                timepicker = TRUE,
                update_on = "change",
                tz = air_datetime_widget_timezone(default_input_timezone()),
                timepickerOpts = shinyWidgets::timepickerOptions(
                  minutesStep = 15,
                  timeFormat = "HH:mm"
                )
              ),
              shinyWidgets::airDatepickerInput(
                ns("member_use_to"),
                "Use to",
                value = NULL,
                range = FALSE,
                multiple = FALSE,
                timepicker = TRUE,
                update_on = "change",
                tz = air_datetime_widget_timezone(default_input_timezone()),
                timepickerOpts = shinyWidgets::timepickerOptions(
                  minutesStep = 15,
                  timeFormat = "HH:mm"
                )
              )
            ),
            splitLayout(
              cellWidths = c("34%", "33%", "33%"),
              numericInput(
                ns("member_alignment_tolerance"),
                "Alignment tolerance",
                value = NA,
                min = 0,
                step = 1,
                width = "100%"
              ),
              selectizeInput(
                ns("member_alignment_tolerance_unit"),
                "Tolerance unit",
                choices = c(
                  "Seconds" = "seconds",
                  "Minutes" = "minutes",
                  "Hours" = "hours",
                  "Days" = "days"
                ),
                selected = "minutes",
                multiple = FALSE,
                options = list(dropdownParent = "body"),
                width = "100%"
              ),
              checkboxInput(
                ns("member_reuse_values"),
                "Reuse matched values",
                value = FALSE,
                width = "100%"
              )
            ),
            tags$p(
              class = "text-muted small",
              "Alignment tolerance uses the nearest member measurement within the tolerance window. If 'Reuse matched values' is checked, the same member measurement can be matched to multiple derived timestamps. Consider this when combining different recording rates, such as hourly and 5-minute timeseries."
            ),
            div(
              actionButton(ns("add_member"), "Add member"),
              actionButton(ns("update_member"), "Update selected member"),
              actionButton(ns("remove_member"), "Remove selected member"),
              actionButton(ns("clear_member"), "Clear member form")
            ),
            uiOutput(ns("definition_warning")),
            DT::DTOutput(ns("members_table"))
          )
        ),
        textAreaInput(
          ns("note"),
          "Note",
          value = "",
          rows = 3,
          placeholder = "Any additional information about this timeseries",
          width = "100%"
        ),
        conditionalPanel(
          condition = "input.mode == 'add'",
          ns = ns,
          bslib::input_task_button(
            ns("add_compound_timeseries"),
            label = "Add derived timeseries"
          )
        ),
        conditionalPanel(
          condition = "input.mode == 'modify'",
          ns = ns,
          bslib::input_task_button(
            ns("modify_compound_timeseries"),
            label = "Modify derived timeseries"
          )
        )
      )
    })

    output$compound_table <- DT::renderDT({
      df <- moduleData$compound_display
      if (is.null(df) || nrow(df) == 0) {
        df <- data.frame(
          message = "No derived timeseries found.",
          stringsAsFactors = FALSE
        )
        return(DT::datatable(df, rownames = FALSE, selection = "none"))
      }

      df$timeseries_type <- as.factor(df$timeseries_type)
      df$publicly_visible <- as.factor(df$publicly_visible)
      df$sensor_priority <- as.factor(df$sensor_priority)
      df$active <- NULL

      DT::datatable(
        df,
        selection = "single",
        options = list(
          columnDefs = list(
            list(targets = 0, visible = FALSE),
            list(
              targets = which(names(df) == "timeseries_type_code") - 1L,
              visible = FALSE
            )
          ),
          scrollX = TRUE,
          initComplete = htmlwidgets::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({",
            "  'background-color': '#079',",
            "  'color': '#fff',",
            "  'font-size': '90%',",
            "});",
            "$(this.api().table().body()).css({'font-size': '80%'});",
            "}"
          )
        ),
        filter = "top",
        rownames = FALSE
      )
    })

    output$members_table <- DT::renderDT({
      df <- member_display()
      if (nrow(df) > 0) {
        df$timeseries_type <- as.factor(df$timeseries_type)
        df$active <- as.factor(df$active)
        df$sensor_priority <- as.factor(df$sensor_priority)
      }

      DT::datatable(
        df,
        selection = "single",
        options = list(
          columnDefs = list(list(targets = 1, visible = FALSE)),
          pageLength = 10,
          scrollX = TRUE
        ),
        filter = "top",
        rownames = FALSE
      )
    })

    definition_errors <- reactive({
      validate_member_rows(member_rows(), expression_sql = input$expression_sql)
    })

    output$definition_warning <- renderUI({
      errors <- definition_errors()
      if (!length(errors)) {
        return(NULL)
      }

      div(
        class = "alert alert-warning",
        paste(errors, collapse = " ")
      )
    })

    observeEvent(
      input$member_timezone,
      {
        shift_air_datetime_input_timezone(
          session,
          input,
          "member_use_from",
          input$member_timezone
        )
        shift_air_datetime_input_timezone(
          session,
          input,
          "member_use_to",
          input$member_timezone
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      list(input$location, input$sub_location),
      {
        update_z_selectize()
        loc <- nullable_integer(input$location)
        if (is.na(loc)) {
          updateSelectizeInput(
            session,
            "sub_location",
            choices = stats::setNames(
              moduleData$sub_locations$sub_location_id,
              moduleData$sub_locations$sub_location_name
            ),
            selected = character(0)
          )
          return()
        }

        sub_rows <- moduleData$sub_locations[
          moduleData$sub_locations$location_id == loc,
          ,
          drop = FALSE
        ]
        updateSelectizeInput(
          session,
          "sub_location",
          choices = stats::setNames(
            sub_rows$sub_location_id,
            sub_rows$sub_location_name
          ),
          selected = if (
            nullable_integer(input$sub_location) %in%
              sub_rows$sub_location_id
          ) {
            input$sub_location
          } else {
            character(0)
          }
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$parameter,
      {
        update_matrix_state_selectize(parameter_id = input$parameter)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$share_with,
      {
        if (
          length(input$share_with) > 1 &&
            "public_reader" %in% input$share_with
        ) {
          updateSelectizeInput(
            session,
            "share_with",
            selected = "public_reader"
          )
          showNotification(
            "Use either public_reader or specific groups, not both.",
            type = "warning"
          )
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(input$reload_module, {
      getModuleData()
      member_rows(member_rows()[0, , drop = FALSE])
      selected_tsid(NULL)
      updateSelectizeInput(
        session,
        "member_timeseries_id",
        choices = stats::setNames(
          moduleData$timeseries_display$timeseries_id,
          timeseries_labels(moduleData$timeseries_display)
        )
      )
      showNotification("Module reloaded.", type = "message")
    })

    clear_member_inputs <- function() {
      updateTextInput(session, "member_alias", value = "")
      updateSelectizeInput(
        session,
        "member_timeseries_id",
        selected = character(0)
      )
      updateNumericInput(session, "member_priority", value = 1)
      shinyWidgets::updateAirDateInput(session, "member_use_from", value = NULL)
      shinyWidgets::updateAirDateInput(session, "member_use_to", value = NULL)
      updateNumericInput(session, "member_alignment_tolerance", value = NA)
      updateSelectizeInput(
        session,
        "member_alignment_tolerance_unit",
        selected = "minutes"
      )
      updateCheckboxInput(session, "member_reuse_values", value = FALSE)
      DT::dataTableProxy("members_table", session = session) |>
        DT::selectRows(NULL)
    }

    current_member_input <- function() {
      member_alias <- nullable_text(input$member_alias)
      member_timeseries_id <- nullable_integer(input$member_timeseries_id)
      member_priority <- nullable_integer(input$member_priority)
      use_from <- scalar_utc_datetime(input$member_use_from)
      use_to <- scalar_utc_datetime(input$member_use_to)
      alignment_tolerance_seconds <- member_tolerance_seconds(
        input$member_alignment_tolerance,
        input$member_alignment_tolerance_unit
      )
      reuse_member_values <- nullable_logical(input$member_reuse_values)

      data.frame(
        member_alias = member_alias,
        member_timeseries_id = member_timeseries_id,
        member_priority = member_priority,
        use_from = use_from,
        use_to = use_to,
        alignment_tolerance_seconds = alignment_tolerance_seconds,
        reuse_member_values = reuse_member_values,
        stringsAsFactors = FALSE
      )
    }

    validate_member_input <- function(
      row,
      existing,
      replace_row = NA_integer_
    ) {
      errors <- character()

      if (is.na(row$member_alias[[1]])) {
        errors <- c(errors, "Enter a member alias.")
      } else if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", row$member_alias[[1]])) {
        errors <- c(
          errors,
          "Member aliases must start with a letter and contain only letters, numbers, and underscores."
        )
      }
      if (is.na(row$member_timeseries_id[[1]])) {
        errors <- c(errors, "Select a member timeseries.")
      }
      if (
        !is.na(row$member_timeseries_id[[1]]) &&
          !row$member_timeseries_id[[1]] %in%
            moduleData$timeseries_display$timeseries_id
      ) {
        errors <- c(errors, "Selected member timeseries is not available.")
      }
      if (
        identical(input$mode, "modify") &&
          !is.na(row$member_timeseries_id[[1]]) &&
          row$member_timeseries_id[[1]] == nullable_integer(selected_tsid())
      ) {
        errors <- c(
          errors,
          "A derived timeseries cannot be a member of itself."
        )
      }
      if (is.na(row$member_priority[[1]]) || row$member_priority[[1]] <= 0) {
        errors <- c(errors, "Member priority must be a positive integer.")
      }
      if (
        !is.na(row$use_from[[1]]) &&
          !is.na(row$use_to[[1]]) &&
          row$use_to[[1]] <= row$use_from[[1]]
      ) {
        errors <- c(errors, "Use-to must be later than use-from.")
      }
      if (
        !is.na(row$alignment_tolerance_seconds[[1]]) &&
          row$alignment_tolerance_seconds[[1]] < 0
      ) {
        errors <- c(errors, "Alignment tolerance cannot be negative.")
      }
      if (
        isTRUE(row$reuse_member_values[[1]]) &&
          is.na(row$alignment_tolerance_seconds[[1]])
      ) {
        errors <- c(
          errors,
          "Reuse matched values requires an alignment tolerance."
        )
      }

      check_rows <- existing
      if (!is.na(replace_row) && nrow(check_rows) >= replace_row) {
        check_rows <- check_rows[-replace_row, , drop = FALSE]
      }
      if (
        !is.na(row$member_alias[[1]]) &&
          row$member_alias[[1]] %in% check_rows$member_alias
      ) {
        errors <- c(errors, "Member aliases must be unique.")
      }

      unique(errors)
    }

    validate_member_rows <- function(rows, expression_sql = "") {
      errors <- character()

      if (is.null(rows) || nrow(rows) == 0) {
        errors <- c(errors, "Add at least one member timeseries.")
      }
      if (anyDuplicated(rows$member_alias)) {
        errors <- c(errors, "Member aliases must be unique.")
      }
      bad_alias <- is.na(rows$member_alias) |
        !grepl("^[A-Za-z][A-Za-z0-9_]*$", rows$member_alias)
      if (any(bad_alias)) {
        errors <- c(errors, "One or more member aliases are invalid.")
      }
      bad_priority <- is.na(rows$member_priority) | rows$member_priority <= 0
      if (any(bad_priority)) {
        errors <- c(errors, "All member priorities must be positive integers.")
      }
      bad_range <- !is.na(rows$use_from) &
        !is.na(rows$use_to) &
        rows$use_to <= rows$use_from
      if (any(bad_range)) {
        errors <- c(
          errors,
          "One or more member use windows end before they start."
        )
      }
      bad_tolerance <- !is.na(rows$alignment_tolerance_seconds) &
        rows$alignment_tolerance_seconds < 0
      if (any(bad_tolerance)) {
        errors <- c(errors, "One or more alignment tolerances are negative.")
      }
      bad_reuse <- isTRUE(any(
        rows$reuse_member_values &
          is.na(rows$alignment_tolerance_seconds)
      ))
      if (bad_reuse) {
        errors <- c(
          errors,
          "Reuse matched values requires an alignment tolerance."
        )
      }

      parent_id <- nullable_integer(selected_tsid())
      if (
        identical(input$mode, "modify") &&
          !is.na(parent_id) &&
          parent_id %in% rows$member_timeseries_id
      ) {
        errors <- c(
          errors,
          "A derived timeseries cannot be a member of itself."
        )
      }

      expression_sql <- nullable_text(expression_sql)
      if (
        !is.na(expression_sql) &&
          grepl(";|--|/\\*|\\*/", expression_sql)
      ) {
        errors <- c(
          errors,
          "Expression SQL must be a single expression without comments or semicolons."
        )
      }

      unique(errors)
    }

    observeEvent(input$add_member, {
      row <- current_member_input()
      errors <- validate_member_input(row, member_rows())
      if (length(errors)) {
        showNotification(errors[[1]], type = "error", duration = 8)
        return()
      }

      new_rows <- rbind(member_rows(), row)
      new_rows <- new_rows[
        order(new_rows$member_priority, new_rows$member_alias),
      ]
      row.names(new_rows) <- NULL
      member_rows(new_rows)
      clear_member_inputs()
    })

    observeEvent(input$update_member, {
      sel <- input$members_table_rows_selected
      if (is.null(sel) || length(sel) != 1) {
        showNotification("Select one member row to update.", type = "error")
        return()
      }

      rows <- member_rows()
      row <- current_member_input()
      errors <- validate_member_input(row, rows, replace_row = sel)
      if (length(errors)) {
        showNotification(errors[[1]], type = "error", duration = 8)
        return()
      }

      rows[sel, ] <- row
      rows <- rows[order(rows$member_priority, rows$member_alias), ]
      row.names(rows) <- NULL
      member_rows(rows)
      clear_member_inputs()
    })

    observeEvent(input$remove_member, {
      sel <- input$members_table_rows_selected
      if (is.null(sel) || length(sel) != 1) {
        showNotification("Select one member row to remove.", type = "error")
        return()
      }
      rows <- member_rows()
      rows <- rows[-sel, , drop = FALSE]
      row.names(rows) <- NULL
      member_rows(rows)
      clear_member_inputs()
    })

    observeEvent(input$clear_member, {
      clear_member_inputs()
    })

    observeEvent(input$members_table_rows_selected, {
      sel <- input$members_table_rows_selected
      if (is.null(sel) || length(sel) != 1) {
        return()
      }
      row <- member_rows()[sel, , drop = FALSE]
      updateTextInput(session, "member_alias", value = row$member_alias[[1]])
      updateSelectizeInput(
        session,
        "member_timeseries_id",
        selected = as.character(row$member_timeseries_id[[1]])
      )
      updateNumericInput(
        session,
        "member_priority",
        value = row$member_priority[[1]]
      )
      shinyWidgets::updateAirDateInput(
        session,
        "member_use_from",
        value = if (is.na(row$use_from[[1]])) NULL else row$use_from[[1]],
        tz = air_datetime_widget_timezone(input$member_timezone)
      )
      shinyWidgets::updateAirDateInput(
        session,
        "member_use_to",
        value = if (is.na(row$use_to[[1]])) NULL else row$use_to[[1]],
        tz = air_datetime_widget_timezone(input$member_timezone)
      )
      tolerance <- split_tolerance_seconds(row$alignment_tolerance_seconds[[1]])
      updateNumericInput(
        session,
        "member_alignment_tolerance",
        value = tolerance$value
      )
      updateSelectizeInput(
        session,
        "member_alignment_tolerance_unit",
        selected = tolerance$unit
      )
      updateCheckboxInput(
        session,
        "member_reuse_values",
        value = isTRUE(row$reuse_member_values[[1]])
      )
    })

    reset_form <- function() {
      selected_tsid(NULL)
      member_rows(member_rows()[0, , drop = FALSE])
      updateSelectizeInput(session, "location", selected = character(0))
      updateSelectizeInput(session, "sub_location", selected = character(0))
      updateSelectizeInput(session, "tz", selected = -7)
      updateSelectizeInput(session, "z", selected = character(0))
      updateSelectizeInput(session, "parameter", selected = character(0))
      updateSelectizeInput(session, "media", selected = character(0))
      update_matrix_state_selectize(selected = NA_integer_)
      updateSelectizeInput(session, "aggregation_type", selected = character(0))
      updateSelectizeInput(session, "sensor_priority", selected = 1)
      updateSelectizeInput(session, "share_with", selected = "public_reader")
      updateCheckboxInput(session, "publicly_visible", value = TRUE)
      updateTextAreaInput(session, "expression_sql", value = "")
      updateTextAreaInput(session, "note", value = "")
      clear_member_inputs()
    }

    observeEvent(input$compound_table_rows_selected, {
      sel <- input$compound_table_rows_selected
      if (is.null(sel) || length(sel) != 1) {
        selected_tsid(NULL)
        return()
      }

      tsid <- moduleData$compound_display$timeseries_id[[sel]]
      selected_tsid(tsid)

      details <- moduleData$timeseries[
        moduleData$timeseries$timeseries_id == tsid,
        ,
        drop = FALSE
      ]
      if (nrow(details) != 1) {
        showNotification(
          "Selected derived timeseries is unavailable.",
          type = "error"
        )
        return()
      }

      header <- moduleData$compound_headers[
        moduleData$compound_headers$timeseries_id == tsid,
        ,
        drop = FALSE
      ]
      rows <- moduleData$compound_members[
        moduleData$compound_members$timeseries_id == tsid,
        c(
          "member_alias",
          "member_timeseries_id",
          "member_priority",
          "use_from",
          "use_to",
          "alignment_tolerance_seconds",
          "reuse_member_values"
        ),
        drop = FALSE
      ]
      member_rows(rows)

      updateSelectizeInput(session, "location", selected = details$location_id)
      updateSelectizeInput(
        session,
        "sub_location",
        selected = if (is.na(details$sub_location_id[[1]])) {
          character(0)
        } else {
          details$sub_location_id[[1]]
        }
      )
      updateSelectizeInput(
        session,
        "tz",
        selected = details$timezone_daily_calc
      )
      update_z_selectize(selected = details$z)
      updateSelectizeInput(
        session,
        "parameter",
        selected = details$parameter_id
      )
      updateSelectizeInput(session, "media", selected = details$media_id)
      update_matrix_state_selectize(
        selected = details$matrix_state_id,
        parameter_id = details$parameter_id
      )
      updateSelectizeInput(
        session,
        "aggregation_type",
        selected = details$aggregation_type_id
      )
      updateSelectizeInput(
        session,
        "sensor_priority",
        selected = details$sensor_priority
      )
      updateSelectizeInput(
        session,
        "share_with",
        selected = array_to_text(details$share_with)
      )
      updateCheckboxInput(
        session,
        "publicly_visible",
        value = isTRUE(details$publicly_visible[[1]])
      )
      updateTextAreaInput(
        session,
        "expression_sql",
        value = if (nrow(header) && !is.na(header$expression_sql[[1]])) {
          header$expression_sql[[1]]
        } else {
          ""
        }
      )
      updateTextAreaInput(
        session,
        "note",
        value = if (is.na(details$note[[1]])) "" else details$note[[1]]
      )
    })

    build_request <- function(mode) {
      required_errors <- c(
        if (!isTruthy(input$location)) "Please select a location.",
        if (!isTruthy(input$parameter)) "Please select a parameter.",
        if (!isTruthy(input$media)) "Please select a media type.",
        if (!isTruthy(input$matrix_state)) "Please select a matrix state.",
        if (!isTruthy(input$aggregation_type)) {
          "Please select an aggregation type."
        },
        if (!isTruthy(input$sensor_priority)) {
          "Please select sensor priority metadata."
        }
      )
      if (
        identical(mode, "modify") && is.na(nullable_integer(selected_tsid()))
      ) {
        required_errors <- c(
          required_errors,
          "Please select a derived timeseries to modify."
        )
      }
      if (length(required_errors)) {
        stop(required_errors[[1]])
      }

      matrix_state_error <- validate_timeseries_matrix_state(
        parameter_id = input$parameter,
        media_id = input$media,
        matrix_state_id = input$matrix_state
      )
      if (!is.null(matrix_state_error)) {
        stop(matrix_state_error)
      }

      z_value <- nullable_numeric(input$z)
      if (length(normalize_selectize_values(input$z)) > 0 && is.na(z_value)) {
        stop("Elevation/depth must be numeric.")
      }

      errors <- validate_member_rows(
        member_rows(),
        expression_sql = input$expression_sql
      )
      if (length(errors)) {
        stop(errors[[1]])
      }

      list(
        mode = mode,
        timeseries_id = nullable_integer(selected_tsid()),
        location_id = nullable_integer(input$location),
        sub_location_id = nullable_integer(input$sub_location),
        timezone_daily_calc = nullable_integer(input$tz),
        z_value = z_value,
        parameter_id = nullable_integer(input$parameter),
        media_id = nullable_integer(input$media),
        matrix_state_id = nullable_integer(input$matrix_state),
        sensor_priority = nullable_integer(input$sensor_priority),
        aggregation_type_id = nullable_integer(input$aggregation_type),
        share_with = share_with_to_array(input$share_with),
        publicly_visible = isTRUE(input$publicly_visible),
        expression_sql = nullable_text(input$expression_sql),
        note = nullable_text(input$note),
        members = member_rows()
      )
    }

    save_compound_timeseries <- function(req) {
      con <- session$userData$AquaCache
      DBI::dbBegin(con)
      committed <- FALSE
      on.exit(
        {
          if (!committed) {
            try(DBI::dbRollback(con), silent = TRUE)
          }
        },
        add = TRUE
      )

      z_id <- get_or_create_location_z_id(
        con = con,
        location_id = req$location_id,
        sub_location_id = req$sub_location_id,
        z_value = req$z_value
      )

      if (identical(req$mode, "add")) {
        req$timeseries_id <- as.integer(DBI::dbGetQuery(
          con,
          paste(
            "INSERT INTO continuous.timeseries (",
            "location_id, sub_location_id, timezone_daily_calc, z_id,",
            "parameter_id, media_id, matrix_state_id, sensor_priority,",
            "aggregation_type_id, share_with, note, active,",
            "sync_remote, publicly_visible, timeseries_type, source_fx,",
            "source_fx_args",
            ") VALUES (",
            "$1, $2, $3, $4, $5, $6, $7, $8, $9,",
            "$10, $11, TRUE, FALSE, $12, 'compound', NULL, NULL",
            ") RETURNING timeseries_id"
          ),
          params = list(
            req$location_id,
            if (is.na(req$sub_location_id)) {
              NA_integer_
            } else {
              req$sub_location_id
            },
            req$timezone_daily_calc,
            if (is.na(z_id)) NA_integer_ else z_id,
            req$parameter_id,
            req$media_id,
            req$matrix_state_id,
            req$sensor_priority,
            req$aggregation_type_id,
            req$share_with,
            if (is.na(req$note)) NA_character_ else req$note,
            req$publicly_visible
          )
        )[[1]])
      } else {
        existing <- DBI::dbGetQuery(
          con,
          paste(
            "SELECT timeseries_id",
            "FROM continuous.timeseries",
            "WHERE timeseries_id = $1 AND timeseries_type = 'compound'"
          ),
          params = list(req$timeseries_id)
        )
        if (nrow(existing) != 1) {
          stop("Selected timeseries is no longer a compound timeseries.")
        }

        DBI::dbExecute(
          con,
          paste(
            "UPDATE continuous.timeseries SET",
            "location_id = $1, sub_location_id = $2,",
            "timezone_daily_calc = $3, z_id = $4,",
            "parameter_id = $5, media_id = $6, matrix_state_id = $7,",
            "sensor_priority = $8, aggregation_type_id = $9,",
            "default_owner = NULL,",
            "default_data_sharing_agreement_id = NULL,",
            "share_with = $10, note = $11, active = TRUE,",
            "sync_remote = FALSE, publicly_visible = $12,",
            "source_fx = NULL, source_fx_args = NULL,",
            "modified = CURRENT_TIMESTAMP, modified_by = CURRENT_USER",
            "WHERE timeseries_id = $13"
          ),
          params = list(
            req$location_id,
            if (is.na(req$sub_location_id)) {
              NA_integer_
            } else {
              req$sub_location_id
            },
            req$timezone_daily_calc,
            if (is.na(z_id)) NA_integer_ else z_id,
            req$parameter_id,
            req$media_id,
            req$matrix_state_id,
            req$sensor_priority,
            req$aggregation_type_id,
            req$share_with,
            if (is.na(req$note)) NA_character_ else req$note,
            req$publicly_visible,
            req$timeseries_id
          )
        )
      }

      DBI::dbExecute(
        con,
        paste(
          "INSERT INTO continuous.timeseries_compounds (",
          "timeseries_id, expression_sql",
          ") VALUES ($1, $2)",
          "ON CONFLICT (timeseries_id) DO UPDATE SET",
          "expression_sql = EXCLUDED.expression_sql,",
          "modified = CURRENT_TIMESTAMP, modified_by = CURRENT_USER"
        ),
        params = list(
          req$timeseries_id,
          if (is.na(req$expression_sql)) NA_character_ else req$expression_sql
        )
      )

      DBI::dbExecute(
        con,
        "DELETE FROM continuous.timeseries_compound_members WHERE timeseries_id = $1",
        params = list(req$timeseries_id)
      )
      for (i in seq_len(nrow(req$members))) {
        DBI::dbExecute(
          con,
          paste(
            "INSERT INTO continuous.timeseries_compound_members (",
            "timeseries_id, member_alias, member_timeseries_id,",
            "member_priority, use_from, use_to,",
            "alignment_tolerance, reuse_member_values",
            ") VALUES (",
            "$1, $2, $3, $4, $5, $6,",
            "CASE",
            "WHEN $7::double precision IS NULL THEN NULL",
            "ELSE make_interval(secs => $7::double precision)",
            "END,",
            "$8",
            ")"
          ),
          params = list(
            req$timeseries_id,
            req$members$member_alias[[i]],
            as.integer(req$members$member_timeseries_id[[i]]),
            as.integer(req$members$member_priority[[i]]),
            if (is.na(req$members$use_from[[i]])) {
              empty_utc_datetime()
            } else {
              req$members$use_from[[i]]
            },
            if (is.na(req$members$use_to[[i]])) {
              empty_utc_datetime()
            } else {
              req$members$use_to[[i]]
            },
            if (is.na(req$members$alignment_tolerance_seconds[[i]])) {
              NA_real_
            } else {
              req$members$alignment_tolerance_seconds[[i]]
            },
            isTRUE(req$members$reuse_member_values[[i]])
          )
        )
      }

      DBI::dbCommit(con)
      committed <- TRUE
      req$timeseries_id
    }

    handle_save <- function(mode) {
      req <- tryCatch(
        build_request(mode),
        error = function(e) {
          showNotification(conditionMessage(e), type = "error", duration = 8)
          NULL
        }
      )
      if (is.null(req)) {
        return()
      }

      tsid <- tryCatch(
        save_compound_timeseries(req),
        error = function(e) {
          showNotification(
            paste("Save failed:", conditionMessage(e)),
            type = "error",
            duration = 12
          )
          NULL
        }
      )
      if (is.null(tsid)) {
        return()
      }

      getModuleData()
      selected_tsid(tsid)
      showNotification(
        sprintf("Derived timeseries %s saved.", tsid),
        type = "message",
        duration = 8
      )
      if (identical(mode, "add")) {
        reset_form()
      }
    }

    observeEvent(input$add_compound_timeseries, {
      if (!identical(input$mode, "add")) {
        showNotification(
          "Select add mode before adding a timeseries.",
          type = "error"
        )
        return()
      }
      handle_save("add")
    })

    observeEvent(input$modify_compound_timeseries, {
      if (!identical(input$mode, "modify")) {
        showNotification(
          "Select modify mode before modifying a timeseries.",
          type = "error"
        )
        return()
      }
      handle_save("modify")
    })
  })
}
