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
      timeseries = NULL,
      deployment_records = NULL
    )

    selected_metadata_id <- reactiveVal(NULL)
    selected_timeseries_id <- reactiveVal(NA_integer_)
    selection_sync_in_progress <- reactiveVal(FALSE)
    pending_update_values <- reactiveVal(NULL)

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

    plural_suffix <- function(n) {
      if (isTRUE(!is.na(n) && as.integer(n) == 1L)) "" else "s"
    }

    same_nullable_integer <- function(x, y) {
      x <- as_nullable_integer(x)
      y <- as_nullable_integer(y)

      if (is.na(x) && is.na(y)) {
        return(TRUE)
      }
      if (is.na(x) || is.na(y)) {
        return(FALSE)
      }

      identical(x, y)
    }

    format_parameter_label <- function(parameter_name, units = NA_character_) {
      parameter_name <- safe_text(parameter_name)
      units <- safe_text(units)

      if (!nzchar(parameter_name)) {
        return("")
      }
      if (!nzchar(units)) {
        return(parameter_name)
      }

      paste0(parameter_name, " (", units, ")")
    }

    lookup_location_label <- function(location_id) {
      location_id <- as_nullable_integer(location_id)
      if (
        is.na(location_id) ||
          is.null(moduleData$locations) ||
          nrow(moduleData$locations) == 0
      ) {
        return("Not selected")
      }

      row <- moduleData$locations[
        moduleData$locations$location_id == location_id,
        ,
        drop = FALSE
      ]

      if (nrow(row) == 0) {
        return(paste("Location ID", location_id))
      }

      sprintf(
        "%s [%s]",
        safe_text(row$location_name[[1]]),
        safe_text(row$location_code[[1]])
      )
    }

    lookup_instrument_label <- function(instrument_id) {
      instrument_id <- as_nullable_integer(instrument_id)
      if (
        is.na(instrument_id) ||
          is.null(moduleData$instruments) ||
          nrow(moduleData$instruments) == 0
      ) {
        return("No instrument selected")
      }

      row <- moduleData$instruments[
        moduleData$instruments$instrument_id == instrument_id,
        ,
        drop = FALSE
      ]

      if (nrow(row) == 0) {
        return(paste("Instrument ID", instrument_id))
      }

      paste(
        safe_text(row$serial_no[[1]]),
        safe_text(row$make[[1]]),
        safe_text(row$model[[1]]),
        safe_text(row$instrument_type[[1]]),
        sep = " | "
      )
    }

    lookup_sub_location_label <- function(sub_location_id) {
      sub_location_id <- as_nullable_integer(sub_location_id)
      if (is.na(sub_location_id)) {
        return("No sub-location selected")
      }
      if (
        is.null(moduleData$sub_locations) ||
          nrow(moduleData$sub_locations) == 0
      ) {
        return(paste("Sub-location ID", sub_location_id))
      }

      row <- moduleData$sub_locations[
        moduleData$sub_locations$sub_location_id == sub_location_id,
        ,
        drop = FALSE
      ]

      if (nrow(row) == 0) {
        return(paste("Sub-location ID", sub_location_id))
      }

      safe_text(row$sub_location_name[[1]])
    }

    lookup_z_label <- function(z_id) {
      z_id <- as_nullable_integer(z_id)
      if (is.na(z_id)) {
        return("No elevation / depth selected")
      }
      if (
        is.null(moduleData$locations_z) ||
          nrow(moduleData$locations_z) == 0
      ) {
        return(paste("z_id", z_id))
      }

      row <- moduleData$locations_z[
        moduleData$locations_z$z_id == z_id,
        ,
        drop = FALSE
      ]

      if (nrow(row) == 0) {
        return(paste("z_id", z_id))
      }

      label <- paste0(format(row$z_meters[[1]], trim = TRUE), " m")
      if (!is.na(row$note[[1]]) && nzchar(row$note[[1]])) {
        label <- paste(label, row$note[[1]], sep = " | ")
      }
      label
    }

    record_associated_timeseries_id <- function(record) {
      if (is.null(record) || nrow(record) == 0) {
        return(NA_integer_)
      }

      if ("associated_timeseries_id" %in% names(record)) {
        associated_tsid <- as_nullable_integer(record$associated_timeseries_id[[1]])
        if (!is.na(associated_tsid)) {
          return(associated_tsid)
        }
      }

      as_nullable_integer(record$timeseries_id[[1]])
    }

    deployment_has_signal_rows <- function(record) {
      !is.null(record) &&
        nrow(record) > 0 &&
        "signal_row_count" %in% names(record) &&
        !is.na(record$signal_row_count[[1]]) &&
        record$signal_row_count[[1]] > 0
    }

    deployment_association_mode <- function(record) {
      if (is.null(record) || nrow(record) == 0) {
        return("")
      }

      if (deployment_has_signal_rows(record)) {
        if (
          !is.na(record$distinct_signal_timeseries_count[[1]]) &&
            record$distinct_signal_timeseries_count[[1]] > 1
        ) {
          return("Signal metadata (multiple timeseries)")
        }
        return("Signal metadata")
      }

      if (!is.na(as_nullable_integer(record$timeseries_id[[1]]))) {
        return("Legacy deployment link")
      }

      ""
    }

    deployment_has_active_topology <- function(record) {
      if (is.null(record) || nrow(record) == 0) {
        return(FALSE)
      }

      count_columns <- c(
        "connection_count",
        "signal_row_count",
        "logger_connection_count",
        "transmission_setup_count",
        "transmission_component_count",
        "telemetry_component_setup_count"
      )
      count_columns <- count_columns[count_columns %in% names(record)]

      if (length(count_columns) == 0) {
        return(FALSE)
      }

      any(vapply(
        count_columns,
        function(column_name) {
          value <- record[[column_name]][[1]]
          !is.na(value) && value > 0
        },
        logical(1)
      ))
    }

    deployment_topology_summary <- function(record) {
      if (is.null(record) || nrow(record) == 0) {
        return("")
      }

      pieces <- character(0)

      if (!is.na(record$connection_count[[1]]) && record$connection_count[[1]] > 0) {
        pieces <- c(
          pieces,
          paste0(
            "Sensor connection",
            plural_suffix(record$connection_count[[1]]),
            ": ",
            record$connection_count[[1]]
          )
        )
      }

      if (!is.na(record$signal_row_count[[1]]) && record$signal_row_count[[1]] > 0) {
        signal_piece <- paste0(
          "Signal row",
          plural_suffix(record$signal_row_count[[1]]),
          ": ",
          record$signal_row_count[[1]]
        )
        if (!is.na(record$mapped_signal_count[[1]]) && record$mapped_signal_count[[1]] > 0) {
          signal_piece <- paste0(
            signal_piece,
            " (",
            record$mapped_signal_count[[1]],
            " mapped to timeseries"
          )
          if (
            !is.na(record$distinct_signal_timeseries_count[[1]]) &&
              record$distinct_signal_timeseries_count[[1]] > 0 &&
              record$distinct_signal_timeseries_count[[1]] !=
                record$mapped_signal_count[[1]]
          ) {
            signal_piece <- paste0(
              signal_piece,
              ", ",
              record$distinct_signal_timeseries_count[[1]],
              " distinct"
            )
          }
          signal_piece <- paste0(signal_piece, ")")
        }
        pieces <- c(pieces, signal_piece)
      }

      if (
        !is.na(record$logger_connection_count[[1]]) &&
          record$logger_connection_count[[1]] > 0
      ) {
        pieces <- c(
          pieces,
          paste0(
            "Logger-side connection",
            plural_suffix(record$logger_connection_count[[1]]),
            ": ",
            record$logger_connection_count[[1]]
          )
        )
      }

      if (
        !is.na(record$transmission_setup_count[[1]]) &&
          record$transmission_setup_count[[1]] > 0
      ) {
        telemetry_piece <- paste0(
          "Telemetry setup",
          plural_suffix(record$transmission_setup_count[[1]]),
          ": ",
          record$transmission_setup_count[[1]]
        )
        if (
          !is.na(record$transmission_route_count[[1]]) &&
            record$transmission_route_count[[1]] > 0
        ) {
          telemetry_piece <- paste0(
            telemetry_piece,
            ", route",
            plural_suffix(record$transmission_route_count[[1]]),
            ": ",
            record$transmission_route_count[[1]]
          )
        }
        if (
          !is.na(record$transmission_component_count[[1]]) &&
            record$transmission_component_count[[1]] > 0
        ) {
          telemetry_piece <- paste0(
            telemetry_piece,
            ", attached component",
            plural_suffix(record$transmission_component_count[[1]]),
            ": ",
            record$transmission_component_count[[1]]
          )
        }
        pieces <- c(pieces, telemetry_piece)
      }

      if (
        !is.na(record$telemetry_component_setup_count[[1]]) &&
          record$telemetry_component_setup_count[[1]] > 0
      ) {
        pieces <- c(
          pieces,
          paste0(
            "Used as telemetry component in ",
            record$telemetry_component_setup_count[[1]],
            " setup",
            plural_suffix(record$telemetry_component_setup_count[[1]])
          )
        )
      }

      paste(pieces, collapse = " | ")
    }

    selected_timeseries_persist_value <- function() {
      record <- selected_record()

      if (deployment_has_signal_rows(record)) {
        return(as_nullable_integer(record$timeseries_id[[1]]))
      }

      as_nullable_integer(selected_timeseries_id())
    }

    timeseries_association_locked <- function(record = selected_record()) {
      deployment_has_signal_rows(record)
    }

    find_timeseries_row <- function(timeseries_id) {
      timeseries_id <- as_nullable_integer(timeseries_id)
      if (
        is.na(timeseries_id) ||
          is.null(moduleData$timeseries) ||
          nrow(moduleData$timeseries) == 0
      ) {
        return(NULL)
      }

      row <- moduleData$timeseries[
        moduleData$timeseries$timeseries_id == timeseries_id,
        ,
        drop = FALSE
      ]

      if (nrow(row) == 0) {
        return(NULL)
      }

      row
    }

    format_timeseries_title <- function(timeseries_row) {
      if (is.null(timeseries_row) || nrow(timeseries_row) == 0) {
        return("No timeseries selected")
      }

      pieces <- c(
        paste0("Timeseries #", timeseries_row$timeseries_id[[1]]),
        format_parameter_label(
          timeseries_row$parameter_name[[1]],
          timeseries_row$units[[1]]
        ),
        safe_text(timeseries_row$matrix_state[[1]]),
        safe_text(timeseries_row$media_type[[1]]),
        safe_text(timeseries_row$aggregation_type[[1]])
      )

      pieces <- pieces[nzchar(pieces)]
      paste(pieces, collapse = " | ")
    }

    format_timeseries_meta <- function(timeseries_row) {
      if (is.null(timeseries_row) || nrow(timeseries_row) == 0) {
        return("")
      }

      pieces <- c(
        paste(
          "Location:",
          lookup_location_label(timeseries_row$location_id[[1]])
        ),
        paste(
          "Sub-location:",
          lookup_sub_location_label(timeseries_row$sub_location_id[[1]])
        ),
        paste("Elevation / depth:", lookup_z_label(timeseries_row$z_id[[1]])),
        paste("Matrix state:", safe_text(timeseries_row$matrix_state[[1]])),
        if (!is.na(timeseries_row$record_rate[[1]])) {
          paste("Record rate:", safe_text(timeseries_row$record_rate[[1]]))
        } else {
          ""
        },
        if (!is.na(timeseries_row$sensor_priority[[1]])) {
          paste("Sensor priority:", timeseries_row$sensor_priority[[1]])
        } else {
          ""
        }
      )

      pieces <- pieces[nzchar(pieces)]
      paste(pieces, collapse = " | ")
    }

    timeseries_matches_values <- function(timeseries_row, values) {
      if (is.null(timeseries_row) || nrow(timeseries_row) == 0) {
        return(FALSE)
      }

      same_nullable_integer(
        timeseries_row$location_id[[1]],
        values$location_id
      ) &&
        same_nullable_integer(
          timeseries_row$sub_location_id[[1]],
          values$sub_location_id
        ) &&
        same_nullable_integer(timeseries_row$z_id[[1]], values$z_id)
    }

    get_filtered_timeseries <- function(
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

      empty <- if (is.null(moduleData$timeseries)) {
        data.frame()
      } else {
        moduleData$timeseries[0, , drop = FALSE]
      }

      location_id <- as_nullable_integer(selected_location)
      sub_location_id <- as_nullable_integer(selected_sub_location)
      z_id <- as_nullable_integer(selected_z)

      if (
        is.na(location_id) ||
          is.null(moduleData$timeseries) ||
          nrow(moduleData$timeseries) == 0
      ) {
        return(empty)
      }

      available <- moduleData$timeseries[
        moduleData$timeseries$location_id == location_id,
        ,
        drop = FALSE
      ]

      sub_location_matches <- vapply(
        available$sub_location_id,
        same_nullable_integer,
        logical(1),
        y = sub_location_id
      )
      z_matches <- vapply(
        available$z_id,
        same_nullable_integer,
        logical(1),
        y = z_id
      )

      available <- available[sub_location_matches & z_matches, , drop = FALSE]

      if (nrow(available) == 0) {
        return(available)
      }

      available[
        order(
          safe_text(available$parameter_name),
          safe_text(available$matrix_state),
          safe_text(available$media_type),
          safe_text(available$aggregation_type),
          safe_text(available$record_rate),
          available$sensor_priority,
          available$timeseries_id
        ),
        ,
        drop = FALSE
      ]
    }

    deployment_identity_changed <- function(values, record) {
      if (is.null(record) || nrow(record) == 0) {
        return(FALSE)
      }

      !same_nullable_integer(values$instrument_id, record$instrument_id[[1]]) ||
        !same_nullable_integer(values$location_id, record$location_id[[1]]) ||
        !same_nullable_integer(
          values$sub_location_id,
          record$sub_location_id[[1]]
        ) ||
        !same_nullable_integer(values$z_id, record$z_id[[1]]) ||
        !same_nullable_integer(values$timeseries_id, record$timeseries_id[[1]])
    }

    summarize_identity_changes <- function(values, record) {
      if (is.null(record) || nrow(record) == 0) {
        return(character(0))
      }

      changes <- character(0)

      if (
        !same_nullable_integer(values$instrument_id, record$instrument_id[[1]])
      ) {
        changes <- c(
          changes,
          paste(
            "Instrument:",
            lookup_instrument_label(record$instrument_id[[1]]),
            "->",
            lookup_instrument_label(values$instrument_id)
          )
        )
      }

      if (!same_nullable_integer(values$location_id, record$location_id[[1]])) {
        changes <- c(
          changes,
          paste(
            "Location:",
            lookup_location_label(record$location_id[[1]]),
            "->",
            lookup_location_label(values$location_id)
          )
        )
      }

      if (
        !same_nullable_integer(
          values$sub_location_id,
          record$sub_location_id[[1]]
        )
      ) {
        changes <- c(
          changes,
          paste(
            "Sub-location:",
            lookup_sub_location_label(record$sub_location_id[[1]]),
            "->",
            lookup_sub_location_label(values$sub_location_id)
          )
        )
      }

      if (!same_nullable_integer(values$z_id, record$z_id[[1]])) {
        changes <- c(
          changes,
          paste(
            "Elevation / depth:",
            lookup_z_label(record$z_id[[1]]),
            "->",
            lookup_z_label(values$z_id)
          )
        )
      }

      if (
        !same_nullable_integer(values$timeseries_id, record$timeseries_id[[1]])
      ) {
        current_tsid <- record_associated_timeseries_id(record)
        current_ts <- find_timeseries_row(current_tsid)
        new_ts <- find_timeseries_row(values$timeseries_id)

        changes <- c(
          changes,
          paste(
            "Timeseries:",
            if (is.null(current_ts)) {
              if (is.na(current_tsid)) {
                "None"
              } else {
                paste("Timeseries #", current_tsid)
              }
            } else {
              format_timeseries_title(current_ts)
            },
            "->",
            if (is.null(new_ts)) {
              if (is.na(values$timeseries_id)) {
                "None"
              } else {
                paste("Timeseries #", values$timeseries_id)
              }
            } else {
              format_timeseries_title(new_ts)
            }
          )
        )
      }

      changes
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
      selected_timeseries_id(NA_integer_)
      pending_update_values(NULL)

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

      moduleData$timeseries <- DBI::dbGetQuery(
        session$userData$AquaCache,
        paste0(
          "
        SELECT
          ts.timeseries_id,
          ts.location_id,
          ts.sub_location_id,
          ts.z_id,
          l.location_code,
          l.name AS location_name,
          sl.sub_location_name,
          lz.z_meters,
          p.param_name AS parameter_name,
          ",
          ac_parameter_unit_select_sql(
            session$userData$AquaCache,
            "p",
            "units",
            matrix_state_alias = "ts",
            media_alias = "ts"
          ),
          ",
          ts.matrix_state_id,
          ms.matrix_state_name AS matrix_state,
          m.media_type,
          at.aggregation_type,
          ts.record_rate,
          ts.sensor_priority,
          ts.start_datetime,
          ts.end_datetime,
          ts.note
        FROM continuous.timeseries AS ts
        INNER JOIN public.locations AS l
          ON ts.location_id = l.location_id
        LEFT JOIN public.sub_locations AS sl
          ON ts.sub_location_id = sl.sub_location_id
        LEFT JOIN public.locations_z AS lz
          ON ts.z_id = lz.z_id
        LEFT JOIN public.parameters AS p
          ON ts.parameter_id = p.parameter_id
        LEFT JOIN public.matrix_states AS ms
          ON ts.matrix_state_id = ms.matrix_state_id
        LEFT JOIN public.media_types AS m
          ON ts.media_id = m.media_id
        LEFT JOIN continuous.aggregation_types AS at
          ON ts.aggregation_type_id = at.aggregation_type_id
        ORDER BY
          l.name ASC,
          sl.sub_location_name ASC NULLS FIRST,
          lz.z_meters ASC NULLS FIRST,
          p.param_name ASC,
          m.media_type ASC,
          at.aggregation_type ASC,
          ts.record_rate ASC NULLS FIRST,
          ts.sensor_priority ASC NULLS FIRST,
          ts.timeseries_id ASC
        "
        )
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
          lmi.timeseries_id,
          COALESCE(
            lmi.timeseries_id,
            CASE
              WHEN COALESCE(conn.distinct_signal_timeseries_count, 0) = 1
                THEN conn.signal_timeseries_id
            END
          ) AS associated_timeseries_id,
          COALESCE(conn.connection_count, 0) AS connection_count,
          COALESCE(conn.signal_row_count, 0) AS signal_row_count,
          COALESCE(conn.mapped_signal_count, 0) AS mapped_signal_count,
          COALESCE(conn.distinct_signal_timeseries_count, 0)
            AS distinct_signal_timeseries_count,
          COALESCE(conn.logger_connection_count, 0) AS logger_connection_count,
          COALESCE(tx.transmission_setup_count, 0) AS transmission_setup_count,
          COALESCE(tx.transmission_route_count, 0) AS transmission_route_count,
          COALESCE(tx.transmission_component_count, 0)
            AS transmission_component_count,
          COALESCE(comp.telemetry_component_setup_count, 0)
            AS telemetry_component_setup_count,
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
        LEFT JOIN LATERAL (
          SELECT
            COUNT(DISTINCT c.connection_id)
              FILTER (WHERE c.instrument_metadata_id = lmi.metadata_id)
              AS connection_count,
            COUNT(s.connection_signal_id)
              FILTER (WHERE c.instrument_metadata_id = lmi.metadata_id)
              AS signal_row_count,
            COUNT(*) FILTER (
              WHERE c.instrument_metadata_id = lmi.metadata_id
                AND s.timeseries_id IS NOT NULL
            ) AS mapped_signal_count,
            COUNT(DISTINCT s.timeseries_id) FILTER (
              WHERE c.instrument_metadata_id = lmi.metadata_id
                AND s.timeseries_id IS NOT NULL
            ) AS distinct_signal_timeseries_count,
            MIN(s.timeseries_id) FILTER (
              WHERE c.instrument_metadata_id = lmi.metadata_id
                AND s.timeseries_id IS NOT NULL
            ) AS signal_timeseries_id,
            COUNT(DISTINCT c.connection_id)
              FILTER (WHERE c.logger_metadata_id = lmi.metadata_id)
              AS logger_connection_count
          FROM public.locations_metadata_instrument_connections AS c
          LEFT JOIN public.locations_metadata_instrument_connection_signals AS s
            ON s.connection_id = c.connection_id
          WHERE
            (c.instrument_metadata_id = lmi.metadata_id OR
               c.logger_metadata_id = lmi.metadata_id)
        ) AS conn ON TRUE
        LEFT JOIN LATERAL (
          SELECT
            COUNT(DISTINCT ts.transmission_setup_id)
              AS transmission_setup_count,
            COUNT(DISTINCT tr.transmission_route_id)
              AS transmission_route_count,
            COUNT(DISTINCT tc.transmission_component_id)
              AS transmission_component_count
          FROM public.locations_metadata_transmission_setups AS ts
          LEFT JOIN public.locations_metadata_transmission_routes AS tr
            ON tr.transmission_setup_id = ts.transmission_setup_id
          LEFT JOIN public.locations_metadata_transmission_components AS tc
            ON tc.transmission_setup_id = ts.transmission_setup_id
          WHERE
            ts.logger_metadata_id = lmi.metadata_id
        ) AS tx ON TRUE
        LEFT JOIN LATERAL (
          SELECT
            COUNT(DISTINCT tc.transmission_setup_id)
              AS telemetry_component_setup_count
          FROM public.locations_metadata_transmission_components AS tc
          INNER JOIN public.locations_metadata_transmission_setups AS ts
            ON ts.transmission_setup_id = tc.transmission_setup_id
          WHERE
            tc.component_metadata_id = lmi.metadata_id
        ) AS comp ON TRUE
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
    refresh_module_data()

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

    selected_timeseries_details <- reactive({
      find_timeseries_row(selected_timeseries_id())
    })

    timeseries_matches_current_filters <- reactive({
      values <- list(
        location_id = as_nullable_integer(input$location_id),
        sub_location_id = as_nullable_integer(input$sub_location_id),
        z_id = as_nullable_integer(input$z_id)
      )

      if (is.na(values$location_id)) {
        return(TRUE)
      }

      timeseries_row <- selected_timeseries_details()
      if (is.null(timeseries_row)) {
        return(TRUE)
      }

      timeseries_matches_values(timeseries_row, values)
    })

    timeseries_table_data <- reactive({
      available <- get_filtered_timeseries()

      if (nrow(available) == 0) {
        return(data.frame(
          timeseries_id = integer(0),
          parameter = character(0),
          matrix_state = character(0),
          media = character(0),
          aggregation = character(0),
          record_rate = character(0),
          sensor_priority = character(0),
          start_datetime = character(0),
          end_datetime = character(0),
          note = character(0),
          stringsAsFactors = FALSE
        ))
      }

      data.frame(
        timeseries_id = available$timeseries_id,
        parameter = unname(mapply(
          format_parameter_label,
          available$parameter_name,
          available$units
        )),
        matrix_state = safe_text(available$matrix_state),
        media = safe_text(available$media_type),
        aggregation = safe_text(available$aggregation_type),
        record_rate = safe_text(available$record_rate),
        sensor_priority = ifelse(
          is.na(available$sensor_priority),
          "",
          as.character(available$sensor_priority)
        ),
        start_datetime = format_timestamp(available$start_datetime),
        end_datetime = format_timestamp(available$end_datetime),
        note = safe_text(available$note),
        stringsAsFactors = FALSE
      )
    })

    current_table_data <- reactive({
      df <- current_records()
      if (nrow(df) == 0) {
        return(data.frame(
          metadata_id = integer(0),
          timeseries_id = character(0),
          association_mode = character(0),
          acquisition_telemetry = character(0),
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
        timeseries_id = ifelse(
          is.na(df$associated_timeseries_id),
          "",
          as.character(df$associated_timeseries_id)
        ),
        association_mode = vapply(
          seq_len(nrow(df)),
          function(i) deployment_association_mode(df[i, , drop = FALSE]),
          character(1)
        ),
        acquisition_telemetry = vapply(
          seq_len(nrow(df)),
          function(i) deployment_topology_summary(df[i, , drop = FALSE]),
          character(1)
        ),
        serial_no = safe_text(df$serial_no),
        make = as.factor(safe_text(df$make)),
        model = as.factor(safe_text(df$model)),
        instrument_type = as.factor(safe_text(df$instrument_type)),
        location_code = as.factor(safe_text(df$location_code)),
        location_name = as.factor(safe_text(df$location_name)),
        sub_location_name = as.factor(safe_text(df$sub_location_name)),
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
          timeseries_id = character(0),
          association_mode = character(0),
          acquisition_telemetry = character(0),
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
        deployment_status = as.factor(safe_text(df$deployment_status)),
        timeseries_id = ifelse(
          is.na(df$associated_timeseries_id),
          "",
          as.character(df$associated_timeseries_id)
        ),
        association_mode = vapply(
          seq_len(nrow(df)),
          function(i) deployment_association_mode(df[i, , drop = FALSE]),
          character(1)
        ),
        acquisition_telemetry = vapply(
          seq_len(nrow(df)),
          function(i) deployment_topology_summary(df[i, , drop = FALSE]),
          character(1)
        ),
        serial_no = safe_text(df$serial_no),
        make = as.factor(safe_text(df$make)),
        model = as.factor(safe_text(df$model)),
        instrument_type = as.factor(safe_text(df$instrument_type)),
        location_code = as.factor(safe_text(df$location_code)),
        location_name = as.factor(safe_text(df$location_name)),
        sub_location_name = as.factor(safe_text(df$sub_location_name)),
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

      selected_timeseries_id(record_associated_timeseries_id(record))

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
        timeseries_id = selected_timeseries_persist_value(),
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
      current_record <- selected_record()
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
      if (
        require_selected &&
          deployment_has_signal_rows(current_record) &&
          !same_nullable_integer(values$timeseries_id, current_record$timeseries_id[[1]])
      ) {
        stop(
          paste(
            "This deployment's timeseries association is managed through",
            "signal-level connection metadata. Update it under",
            "Acquisition / telemetry -> Connection signals."
          )
        )
      }
      if (!is.na(values$timeseries_id)) {
        timeseries_row <- find_timeseries_row(values$timeseries_id)
        if (is.null(timeseries_row)) {
          stop("The selected timeseries could not be found.")
        }
        if (!timeseries_matches_values(timeseries_row, values)) {
          stop(
            paste(
              "The selected timeseries does not match the selected location,",
              "sub-location, and elevation/depth."
            )
          )
        }
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
            end_datetime, note, z_id, timeseries_id
          ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
          RETURNING metadata_id
          ",
          params = list(
            values$location_id,
            values$sub_location_id,
            values$instrument_id,
            values$start_datetime,
            values$end_datetime,
            values$note,
            values$z_id,
            values$timeseries_id
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
            z_id = $7,
            timeseries_id = $8
          WHERE metadata_id = $9
          ",
          params = list(
            values$location_id,
            values$sub_location_id,
            values$instrument_id,
            values$start_datetime,
            values$end_datetime,
            values$note,
            values$z_id,
            values$timeseries_id,
            metadata_id
          )
        )
        metadata_id
      }
    }

    end_current_and_redeploy <- function(values, metadata_id) {
      validate_form_values(values, require_selected = TRUE)

      current_record <- selected_record()
      if (
        is.null(current_record) ||
          current_record$deployment_status[[1]] != "Currently deployed"
      ) {
        stop(
          paste(
            "Ending the current deployment and re-deploying is only available",
            "for instruments that are currently deployed."
          )
        )
      }

      current_start <- as.POSIXct(
        current_record$start_datetime[[1]],
        tz = "UTC"
      )
      if (values$start_datetime <= current_start) {
        stop(
          paste(
            "To end the current deployment and re-deploy, set the new",
            "deployment start after the current deployment start."
          )
        )
      }

      DBI::dbWithTransaction(
        session$userData$AquaCache,
        {
          DBI::dbExecute(
            session$userData$AquaCache,
            "
            UPDATE public.locations_metadata_instruments
            SET end_datetime = $1
            WHERE metadata_id = $2
            ",
            params = list(values$start_datetime, metadata_id)
          )

          inserted <- DBI::dbGetQuery(
            session$userData$AquaCache,
            "
            INSERT INTO public.locations_metadata_instruments (
              location_id, sub_location_id, instrument_id, start_datetime,
              end_datetime, note, z_id, timeseries_id
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
            RETURNING metadata_id
            ",
            params = list(
              values$location_id,
              values$sub_location_id,
              values$instrument_id,
              values$start_datetime,
              values$end_datetime,
              values$note,
              values$z_id,
              values$timeseries_id
            )
          )

          inserted$metadata_id[[1]]
        }
      )
    }

    save_selected_record <- function(
      values,
      strategy = c("modify", "redeploy")
    ) {
      strategy <- match.arg(strategy)
      metadata_id <- selected_metadata_id()

      if (is.null(metadata_id)) {
        stop("Select an existing deployment record first.")
      }

      updated_metadata_id <- switch(
        strategy,
        modify = upsert_record(values = values, metadata_id = metadata_id),
        redeploy = end_current_and_redeploy(
          values = values,
          metadata_id = metadata_id
        )
      )

      refresh_module_data()
      selected_metadata_id(updated_metadata_id)
      populate_form_from_record(selected_record())
      sync_table_selection(updated_metadata_id)
      pending_update_values(NULL)

      updated_metadata_id
    }

    output$timeseries_selection_ui <- renderUI({
      record <- selected_record()
      timeseries_row <- selected_timeseries_details()
      selected_tsid <- selected_timeseries_id()
      signal_managed <- deployment_has_signal_rows(record)
      note_ui <- p(
        class = "help-block",
        paste(
          "Available timeseries are filtered to the selected location,",
          "sub-location, and elevation/depth."
        )
      )

      signal_ui <- if (signal_managed) {
        div(
          class = "alert alert-info",
          paste(
            "This deployment uses connection-signal metadata. Manage",
            "timeseries links in Acquisition / telemetry -> Connection",
            "signals."
          )
        )
      } else {
        NULL
      }

      if (signal_managed && is.na(selected_tsid)) {
        return(tagList(
          note_ui,
          signal_ui,
          div(
            class = "alert alert-warning",
            if (
              !is.na(record$distinct_signal_timeseries_count[[1]]) &&
                record$distinct_signal_timeseries_count[[1]] > 1
            ) {
              paste(
                "This deployment currently maps to",
                record$distinct_signal_timeseries_count[[1]],
                "distinct timeseries through connection-signal metadata."
              )
            } else if (
              !is.na(record$mapped_signal_count[[1]]) &&
                record$mapped_signal_count[[1]] > 0
            ) {
              "This deployment's connection-signal metadata points to a timeseries that could not be loaded."
            } else {
              "This deployment has signal-level metadata but no linked timeseries yet."
            }
          )
        ))
      }

      if (is.null(timeseries_row) && is.na(selected_tsid)) {
        return(tagList(
          note_ui,
          signal_ui,
          div(
            class = "alert alert-info",
            "No timeseries currently associated."
          )
        ))
      }

      if (is.null(timeseries_row)) {
        return(tagList(
          note_ui,
          signal_ui,
          div(
            class = "alert alert-warning",
            paste0(
              "Timeseries #",
              selected_tsid,
              " is associated with this form but could not be loaded."
            )
          )
        ))
      }

      status_ui <- if (!isTRUE(timeseries_matches_current_filters())) {
        div(
          class = "alert alert-warning",
          paste(
            "The selected timeseries no longer matches the current location,",
            "sub-location, and elevation/depth."
          )
        )
      } else {
        NULL
      }

      tagList(
        note_ui,
        signal_ui,
        status_ui,
        div(
          class = "well well-sm",
          tags$strong(format_timeseries_title(timeseries_row)),
          tags$br(),
          format_timeseries_meta(timeseries_row),
          if (
            !is.na(timeseries_row$note[[1]]) && nzchar(timeseries_row$note[[1]])
          ) {
            tagList(
              tags$br(),
              tags$em(timeseries_row$note[[1]])
            )
          }
        )
      )
    })

    output$timeseries_modal_context <- renderUI({
      available <- timeseries_table_data()

      if (is.na(as_nullable_integer(input$location_id))) {
        return(div(
          class = "alert alert-warning",
          paste(
            "Select a location first. The modal only shows timeseries that",
            "match the selected location, sub-location, and elevation/depth."
          )
        ))
      }

      tagList(
        div(
          class = "alert alert-info",
          tags$p(
            paste(
              "Showing",
              nrow(available),
              "timeseries that match the current deployment context."
            )
          ),
          tags$ul(
            tags$li(
              tags$strong("Location: "),
              lookup_location_label(input$location_id)
            ),
            tags$li(
              tags$strong("Sub-location: "),
              lookup_sub_location_label(input$sub_location_id)
            ),
            tags$li(
              tags$strong("Elevation / depth: "),
              lookup_z_label(input$z_id)
            )
          )
        ),
        if (nrow(available) == 0) {
          div(
            class = "alert alert-warning",
            paste(
              "No timeseries match the selected location, sub-location, and",
              "elevation/depth."
            )
          )
        }
      )
    })

    output$timeseries_table <- DT::renderDT({
      DT::datatable(
        timeseries_table_data(),
        selection = "single",
        rownames = FALSE,
        filter = "top",
        options = list(
          scrollX = TRUE,
          pageLength = 8,
          initComplete = table_header_style
        )
      )
    })

    output$timeseries_action_button <- renderUI({
      button_label <- if (timeseries_association_locked()) {
        "Timeseries managed in Connection signals"
      } else if (is.na(selected_timeseries_id())) {
        "Associate with a timeseries"
      } else {
        "Modify timeseries association"
      }

      button <- actionButton(
        ns("associate_timeseries"),
        button_label,
        width = "100%"
      )

      if (timeseries_association_locked()) {
        return(shinyjs::disabled(button))
      }

      button
    })

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
              width = "100%",
              style = "margin-bottom: 5px;"
            ),
            actionButton(
              ns("clear_selection"),
              "Clear selected record",
              width = "100%",
              style = "margin-bottom: 15px;"
            ),
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
            uiOutput(ns("timeseries_action_button")),
            uiOutput(ns("timeseries_selection_ui")),
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
            uiOutput(ns("record_action_button"))
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
        class = "alert alert-primary",
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
        },
        {
          topology_summary <- deployment_topology_summary(record)
          association_mode <- deployment_association_mode(record)

          if (nzchar(association_mode) || nzchar(topology_summary)) {
            tagList(
              tags$br(),
              tags$small(
                paste(
                  c(
                    if (nzchar(association_mode)) {
                      paste("Timeseries association:", association_mode)
                    },
                    if (nzchar(topology_summary)) {
                      topology_summary
                    }
                  ),
                  collapse = " | "
                )
              )
            )
          }
        },
        if (deployment_has_active_topology(record)) {
          tagList(
            tags$br(),
            tags$small(
              "Changing this deployment record will also affect linked acquisition / telemetry metadata."
            )
          )
        }
      )
    })

    output$record_action_button <- renderUI({
      has_selected <- !is.null(selected_metadata_id())
      can_insert <- isTRUE(moduleData$permissions$can_insert[[1]])
      can_update <- isTRUE(moduleData$permissions$can_update[[1]])

      if (has_selected) {
        update_button <- actionButton(
          ns("update_btn"),
          "Update selected record",
          width = "100%",
          class = "btn-primary"
        )
        if (!can_update) {
          update_button <- shinyjs::disabled(update_button)
        }
        return(update_button)
      }

      deploy_button <- actionButton(
        ns("deploy_btn"),
        "Deploy new instrument",
        width = "100%",
        class = "btn-primary"
      )
      if (!can_insert) {
        deploy_button <- shinyjs::disabled(deploy_button)
      }
      deploy_button
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

    show_timeseries_modal <- function() {
      if (timeseries_association_locked()) {
        showNotification(
          paste(
            "This deployment uses connection-signal metadata. Update",
            "timeseries links under Acquisition / telemetry -> Connection",
            "signals."
          ),
          type = "warning",
          duration = 12
        )
        return(invisible(NULL))
      }

      available_timeseries_ids <- timeseries_table_data()$timeseries_id
      current_timeseries_id <- selected_timeseries_id()

      showModal(
        modalDialog(
          title = "Associate with a timeseries",
          uiOutput(ns("timeseries_modal_context")),
          DT::DTOutput(ns("timeseries_table")),
          easyClose = TRUE,
          size = "l",
          footer = tagList(
            modalButton("Cancel"),
            actionButton(
              ns("clear_timeseries_selection"),
              "Clear association"
            ),
            actionButton(
              ns("confirm_timeseries_selection"),
              "Use selected timeseries",
              class = "btn-primary"
            )
          )
        )
      )

      session$onFlushed(
        function() {
          proxy <- DT::dataTableProxy("timeseries_table", session = session)
          selected_idx <- which(
            available_timeseries_ids == current_timeseries_id
          )

          DT::selectRows(
            proxy,
            if (length(selected_idx) == 1) selected_idx else NULL
          )
        },
        once = TRUE
      )
    }

    show_identity_change_modal <- function(values, record) {
      changes <- summarize_identity_changes(values, record)
      can_redeploy <- identical(
        record$deployment_status[[1]],
        "Currently deployed"
      )

      showModal(
        modalDialog(
          title = "Deployment change detected",
          p(
            paste(
              "You're modifying the instrument, deployment location, elevation/depth, and/or timeseries association. Is this a new deployment or a correction to the current deployment?"
            )
          ),
          if (length(changes) > 0) {
            tags$ul(lapply(changes, tags$li))
          },
          if (deployment_has_active_topology(record)) {
            div(
              class = "alert alert-info",
              paste(
                "This deployment currently has linked acquisition /",
                "telemetry metadata. Choosing 'Modify existing deployment'",
                "keeps those child records attached to this same deployment;",
                "choose re-deploy to start a new deployment record instead."
              )
            )
          },
          if (!can_redeploy) {
            div(
              class = "alert alert-info",
              paste(
                "This record is not currently deployed, so only modifying the",
                "existing deployment is available."
              )
            )
          },
          easyClose = TRUE,
          size = "l",
          footer = tagList(
            modalButton("Cancel"),
            if (can_redeploy) {
              actionButton(
                ns("confirm_redeploy_update"),
                "End current deployment and re-deploy",
                class = "btn-warning"
              )
            },
            actionButton(
              ns("confirm_modify_existing"),
              "Modify existing deployment",
              class = "btn-primary"
            )
          )
        )
      )
    }

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
      input$associate_timeseries,
      {
        show_timeseries_modal()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$clear_timeseries_selection,
      {
        selected_timeseries_id(NA_integer_)
        removeModal()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$confirm_timeseries_selection,
      {
        selected_row <- input$timeseries_table_rows_selected
        if (length(selected_row) != 1) {
          showNotification(
            "Select a timeseries in the table first.",
            type = "warning",
            duration = 10
          )
          return()
        }

        selected_timeseries_id(
          timeseries_table_data()$timeseries_id[[selected_row]]
        )
        removeModal()
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
            values <- collect_form_values()
            record <- selected_record()

            if (deployment_identity_changed(values, record)) {
              pending_update_values(values)
              show_identity_change_modal(values, record)
            } else {
              save_selected_record(values, strategy = "modify")
              showNotification(
                "Deployment record updated.",
                type = "message",
                duration = 10
              )
            }
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
      input$confirm_modify_existing,
      {
        tryCatch(
          {
            removeModal()
            save_selected_record(
              values = pending_update_values(),
              strategy = "modify"
            )
            pending_update_values(NULL)
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
      input$confirm_redeploy_update,
      {
        tryCatch(
          {
            removeModal()
            save_selected_record(
              values = pending_update_values(),
              strategy = "redeploy"
            )
            pending_update_values(NULL)
            showNotification(
              "Current deployment ended and new deployment record added.",
              type = "message",
              duration = 10
            )
          },
          error = function(e) {
            showNotification(
              paste("Failed to end and re-deploy:", e$message),
              type = "error",
              duration = 15
            )
          }
        )
      },
      ignoreInit = TRUE
    )
  }) # End of moduleServer
}
