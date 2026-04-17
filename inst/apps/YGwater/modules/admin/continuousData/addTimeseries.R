# UI and server code for add new location module

addTimeseriesUI <- function(id) {
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
      uiOutput(ns("ui"))
    )
  )
}

addTimeseries <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "addTimeseries"
      )
    })

    moduleData <- reactiveValues()
    selected_tsid <- reactiveVal(NULL)
    instrument_association_cleared <- reactiveVal(FALSE)
    pending_default_owner_selection <- reactiveVal(character(0))
    pending_default_owner_new <- reactiveVal(NULL)

    safe_text <- function(x) {
      ifelse(is.na(x), "", as.character(x))
    }

    nullable_text <- function(x) {
      if (is.null(x) || !length(x)) {
        return(NA_character_)
      }

      value <- x[[1]]
      if (is.na(value)) {
        return(NA_character_)
      }

      value <- trimws(as.character(value))
      if (!nzchar(value)) {
        return(NA_character_)
      }

      value
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

      value_num <- suppressWarnings(as.numeric(value))
      if (is.na(value_num)) {
        return(NA_real_)
      }

      value_num
    }

    update_default_owner_selectize <- function(selected = character(0)) {
      updateSelectizeInput(
        session,
        "default_owner",
        choices = stats::setNames(
          moduleData$organizations$organization_id,
          moduleData$organizations$name
        ),
        selected = normalize_selectize_values(selected)
      )
    }

    matrix_state_label <- function(matrix_state_id) {
      matrix_state_id <- nullable_integer(matrix_state_id)
      if (
        is.na(matrix_state_id) ||
          is.null(moduleData$matrix_states) ||
          nrow(moduleData$matrix_states) == 0
      ) {
        return(NA_character_)
      }

      row <- moduleData$matrix_states[
        moduleData$matrix_states$matrix_state_id == matrix_state_id,
        ,
        drop = FALSE
      ]
      if (nrow(row) == 0) {
        return(NA_character_)
      }

      row$matrix_state_name[[1]]
    }

    parameter_matrix_state_unit <- function(parameter_id, matrix_state_id) {
      parameter_id <- nullable_integer(parameter_id)
      matrix_state_id <- nullable_integer(matrix_state_id)

      if (
        is.na(parameter_id) ||
          is.na(matrix_state_id) ||
          is.null(moduleData$parameters) ||
          nrow(moduleData$parameters) == 0 ||
          is.null(moduleData$matrix_states) ||
          nrow(moduleData$matrix_states) == 0
      ) {
        return(NA_character_)
      }

      param_row <- moduleData$parameters[
        moduleData$parameters$parameter_id == parameter_id,
        ,
        drop = FALSE
      ]
      state_row <- moduleData$matrix_states[
        moduleData$matrix_states$matrix_state_id == matrix_state_id,
        ,
        drop = FALSE
      ]

      if (nrow(param_row) == 0 || nrow(state_row) == 0) {
        return(NA_character_)
      }

      unit_col <- paste0("units_", state_row$matrix_state_code[[1]])
      if (!unit_col %in% names(param_row)) {
        return(NA_character_)
      }

      unit_value <- param_row[[unit_col]][[1]]
      if (is.null(unit_value) || is.na(unit_value) || !nzchar(unit_value)) {
        return(NA_character_)
      }

      unit_value
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
          unit_col %in% names(param_row) &&
            !is.na(param_row[[unit_col]][[1]]) &&
            nzchar(param_row[[unit_col]][[1]])
        },
        logical(1)
      )

      as.integer(moduleData$matrix_states$matrix_state_id[supported])
    }

    resolve_matrix_state_selection <- function(
      parameter_id = nullable_integer(input$parameter),
      media_id = nullable_integer(input$media),
      current_matrix_state_id = nullable_integer(input$matrix_state)
    ) {
      supported_states <- supported_matrix_state_ids(parameter_id)
      if (!length(supported_states)) {
        return(NA_integer_)
      }

      if (
        !is.na(current_matrix_state_id) &&
          current_matrix_state_id %in% supported_states
      ) {
        return(current_matrix_state_id)
      }

      media_default <- NA_integer_
      if (
        !is.na(media_id) &&
          !is.null(moduleData$media) &&
          nrow(moduleData$media) > 0
      ) {
        media_row <- moduleData$media[
          moduleData$media$media_id == media_id,
          ,
          drop = FALSE
        ]
        if (nrow(media_row) == 1) {
          media_default <- nullable_integer(media_row$default_matrix_state_id)
        }
      }

      if (!is.na(media_default) && media_default %in% supported_states) {
        return(media_default)
      }

      if (length(supported_states) == 1) {
        return(supported_states[[1]])
      }

      NA_integer_
    }

    update_matrix_state_selectize <- function(
      selected = nullable_integer(input$matrix_state),
      parameter_id = nullable_integer(input$parameter)
    ) {
      available_ids <- supported_matrix_state_ids(parameter_id)
      if (!length(available_ids) && !is.na(parameter_id)) {
        available_ids <- integer(0)
      } else if (!length(available_ids)) {
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

    same_nullable_integer <- function(x, y) {
      x <- nullable_integer(x)
      y <- nullable_integer(y)

      if (is.na(x) && is.na(y)) {
        return(TRUE)
      }
      if (is.na(x) || is.na(y)) {
        return(FALSE)
      }

      identical(x, y)
    }

    same_nullable_text <- function(x, y) {
      x <- nullable_text(x)
      y <- nullable_text(y)

      if (is.na(x) && is.na(y)) {
        return(TRUE)
      }
      if (is.na(x) || is.na(y)) {
        return(FALSE)
      }

      identical(x, y)
    }

    same_nullable_numeric <- function(x, y, tol = 1e-9) {
      x <- nullable_numeric(x)
      y <- nullable_numeric(y)

      if (is.na(x) && is.na(y)) {
        return(TRUE)
      }
      if (is.na(x) || is.na(y)) {
        return(FALSE)
      }

      abs(x - y) < tol
    }

    format_z_value <- function(x) {
      x <- as.numeric(x)
      x <- x[!is.na(x)]
      if (!length(x)) {
        return(character(0))
      }

      trimws(format(
        x,
        scientific = FALSE,
        digits = 15,
        trim = TRUE
      ))
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
        return(
          paste(
            "Please select a matrix state that matches the parameter",
            "and media type."
          )
        )
      }

      supported_states <- supported_matrix_state_ids(parameter_id)
      if (!matrix_state_id %in% supported_states) {
        parameter_name <- moduleData$parameters$param_name[
          match(parameter_id, moduleData$parameters$parameter_id)
        ]
        return(
          sprintf(
            "The selected matrix state does not have units configured for %s.",
            parameter_name
          )
        )
      }

      NULL
    }

    empty_deployed_instruments <- function() {
      if (!is.null(moduleData$deployed_instruments)) {
        return(moduleData$deployed_instruments[0, , drop = FALSE])
      }

      data.frame(
        metadata_id = integer(0),
        location_id = integer(0),
        sub_location_id = integer(0),
        z_id = integer(0),
        z_meters = numeric(0),
        timeseries_id = integer(0),
        associated_timeseries_id = integer(0),
        connection_count = integer(0),
        signal_row_count = integer(0),
        mapped_signal_count = integer(0),
        distinct_signal_timeseries_count = integer(0),
        instrument_id = integer(0),
        serial_no = character(0),
        make = character(0),
        model = character(0),
        instrument_type = character(0),
        start_datetime = as.POSIXct(character(0), tz = "UTC"),
        stringsAsFactors = FALSE
      )
    }

    active_deployment_association_rows <- function(
      con,
      metadata_id = NA_integer_
    ) {
      metadata_id <- nullable_integer(metadata_id)

      sql <- paste(
        "SELECT",
        "  lmi.metadata_id,",
        "  lmi.timeseries_id,",
        paste(
          "COALESCE(",
          "  lmi.timeseries_id,",
          "  CASE",
          "    WHEN COALESCE(sig.distinct_signal_timeseries_count, 0) = 1",
          "      THEN sig.signal_timeseries_id",
          "  END",
          ") AS associated_timeseries_id,"
        ),
        "  COALESCE(sig.connection_count, 0) AS connection_count,",
        "  COALESCE(sig.signal_row_count, 0) AS signal_row_count,",
        "  COALESCE(sig.mapped_signal_count, 0) AS mapped_signal_count,",
        paste(
          "COALESCE(sig.distinct_signal_timeseries_count, 0)",
          "AS distinct_signal_timeseries_count"
        ),
        "FROM public.locations_metadata_instruments AS lmi",
        "LEFT JOIN LATERAL (",
        "  SELECT",
        "    COUNT(DISTINCT c.connection_id) AS connection_count,",
        "    COUNT(s.connection_signal_id) AS signal_row_count,",
        paste(
          "COUNT(*) FILTER (WHERE s.timeseries_id IS NOT NULL)",
          "AS mapped_signal_count,"
        ),
        paste(
          "COUNT(DISTINCT s.timeseries_id)",
          "FILTER (WHERE s.timeseries_id IS NOT NULL)",
          "AS distinct_signal_timeseries_count,"
        ),
        paste(
          "MIN(s.timeseries_id)",
          "FILTER (WHERE s.timeseries_id IS NOT NULL)",
          "AS signal_timeseries_id"
        ),
        "  FROM public.locations_metadata_instrument_connections AS c",
        "  LEFT JOIN public.locations_metadata_instrument_connection_signals AS s",
        "    ON s.connection_id = c.connection_id",
        "  WHERE c.instrument_metadata_id = lmi.metadata_id",
        "    AND c.start_datetime <= NOW()",
        "    AND (c.end_datetime IS NULL OR c.end_datetime > NOW())",
        ") AS sig ON TRUE",
        "WHERE lmi.start_datetime <= NOW()",
        "  AND (lmi.end_datetime IS NULL OR lmi.end_datetime > NOW())",
        if (is.na(metadata_id)) {
          ""
        } else {
          "  AND lmi.metadata_id = $1"
        }
      )

      DBI::dbGetQuery(
        con,
        sql,
        params = if (is.na(metadata_id)) NULL else list(metadata_id)
      )
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
      )[1, 1])
    }

    deployment_has_signal_rows <- function(record) {
      !is.null(record) &&
        nrow(record) > 0 &&
        !is.na(record$signal_row_count[[1]]) &&
        record$signal_row_count[[1]] > 0
    }

    current_z_value <- reactive({
      nullable_numeric(input$z)
    })

    current_z_has_input <- reactive({
      length(normalize_selectize_values(input$z)) > 0
    })

    current_timeseries_id_for_association <- reactive({
      if (identical(input$mode, "modify")) {
        return(nullable_integer(selected_tsid()))
      }

      NA_integer_
    })

    current_timeseries_association <- reactive({
      tsid <- current_timeseries_id_for_association()

      if (
        is.na(tsid) ||
          is.null(moduleData$deployed_instruments) ||
          nrow(moduleData$deployed_instruments) == 0
      ) {
        return(empty_deployed_instruments())
      }

      moduleData$deployed_instruments[
        !is.na(moduleData$deployed_instruments$associated_timeseries_id) &
          moduleData$deployed_instruments$associated_timeseries_id == tsid,
        ,
        drop = FALSE
      ]
    })

    available_instrument_deployments <- reactive({
      location_id <- nullable_integer(input$location)
      sub_location_id <- nullable_integer(input$sub_location)
      z_value <- current_z_value()
      current_tsid <- current_timeseries_id_for_association()

      if (
        is.na(location_id) ||
          is.null(moduleData$deployed_instruments) ||
          nrow(moduleData$deployed_instruments) == 0
      ) {
        return(empty_deployed_instruments())
      }

      available <- moduleData$deployed_instruments[
        moduleData$deployed_instruments$location_id == location_id,
        ,
        drop = FALSE
      ]

      if (is.na(sub_location_id)) {
        available <- available[is.na(available$sub_location_id), , drop = FALSE]
      } else {
        available <- available[
          available$sub_location_id == sub_location_id,
          ,
          drop = FALSE
        ]
      }

      if (is.na(z_value)) {
        available <- available[is.na(available$z_id), , drop = FALSE]
      } else {
        available <- available[
          !is.na(available$z_meters) &
            abs(as.numeric(available$z_meters) - z_value) < 1e-9,
          ,
          drop = FALSE
        ]
      }

      available <- available[
        is.na(available$associated_timeseries_id) |
          (!is.na(current_tsid) &
            available$associated_timeseries_id == current_tsid),
        ,
        drop = FALSE
      ]

      if (nrow(available) == 0) {
        return(available)
      }

      available[
        order(
          available$serial_no,
          safe_text(available$make),
          safe_text(available$model),
          safe_text(available$instrument_type),
          available$metadata_id
        ),
        ,
        drop = FALSE
      ]
    })

    build_instrument_association_choices <- function(
      df,
      current_tsid = NA_integer_
    ) {
      if (is.null(df) || nrow(df) == 0) {
        return(character(0))
      }

      labels <- sprintf(
        "%s | %s | %s | %s | deployed %s",
        df$serial_no,
        safe_text(df$make),
        safe_text(df$model),
        safe_text(df$instrument_type),
        format(as.POSIXct(df$start_datetime, tz = "UTC"), "%Y-%m-%d")
      )

      signal_idx <- !is.na(df$signal_row_count) & df$signal_row_count > 0
      if (any(signal_idx, na.rm = TRUE)) {
        labels[signal_idx] <- paste0(labels[signal_idx], " [signal metadata]")
      }

      current_idx <- !is.na(current_tsid) &
        df$associated_timeseries_id == current_tsid
      if (any(current_idx, na.rm = TRUE)) {
        labels[current_idx] <- paste0(
          labels[current_idx],
          ifelse(signal_idx[current_idx], " [currently associated]", "")
        )
      }

      legacy_current_idx <- current_idx & !signal_idx
      if (any(legacy_current_idx, na.rm = TRUE)) {
        labels[legacy_current_idx] <- paste0(
          labels[legacy_current_idx],
          " [currently associated]"
        )
      }

      stats::setNames(df$metadata_id, labels)
    }

    update_timeseries_instrument_association <- function(
      con,
      timeseries_id,
      deployment_metadata_id = NA_integer_
    ) {
      timeseries_id <- nullable_integer(timeseries_id)
      deployment_metadata_id <- nullable_integer(deployment_metadata_id)

      if (is.na(timeseries_id)) {
        stop("A timeseries_id is required to update instrument associations.")
      }

      active_deployments <- active_deployment_association_rows(con)

      if (is.na(deployment_metadata_id)) {
        current_signal_assoc <- active_deployments[
          !is.na(active_deployments$associated_timeseries_id) &
            active_deployments$associated_timeseries_id == timeseries_id &
            active_deployments$signal_row_count > 0,
          ,
          drop = FALSE
        ]

        if (nrow(current_signal_assoc) > 0) {
          stop(
            paste(
              "This timeseries is associated through signal-level connection",
              "metadata. Manage that association under",
              "Acquisition / telemetry -> Connection signals."
            )
          )
        }

        DBI::dbExecute(
          con,
          "
          UPDATE public.locations_metadata_instruments
          SET timeseries_id = NULL
          WHERE timeseries_id = $1
            AND start_datetime <= NOW()
            AND (end_datetime IS NULL OR end_datetime > NOW())
          ",
          params = list(timeseries_id)
        )
        return(invisible(NULL))
      }

      selected_deployment <- active_deployment_association_rows(
        con,
        metadata_id = deployment_metadata_id
      )

      if (nrow(selected_deployment) == 0) {
        stop(
          paste(
            "The selected instrument deployment is no longer currently deployed.",
            "Reload the module and try again."
          )
        )
      }

      if (deployment_has_signal_rows(selected_deployment)) {
        if (
          !is.na(selected_deployment$associated_timeseries_id[[1]]) &&
            selected_deployment$associated_timeseries_id[[1]] == timeseries_id
        ) {
          return(invisible(NULL))
        }

        stop(
          paste(
            "The selected deployment already uses signal-level connection",
            "metadata. Manage timeseries links under",
            "Acquisition / telemetry -> Connection signals."
          )
        )
      }

      if (
        !is.na(selected_deployment$timeseries_id[[1]]) &&
          selected_deployment$timeseries_id[[1]] != timeseries_id
      ) {
        stop(
          paste(
            "The selected instrument already has a different timeseries",
            "association. Use Field -> Deploy/recover instruments to",
            "change existing associations."
          )
        )
      }

      other_signal_assoc <- active_deployments[
        active_deployments$metadata_id != deployment_metadata_id &
          !is.na(active_deployments$associated_timeseries_id) &
          active_deployments$associated_timeseries_id == timeseries_id &
          active_deployments$signal_row_count > 0,
        ,
        drop = FALSE
      ]

      if (nrow(other_signal_assoc) > 0) {
        stop(
          paste(
            "This timeseries is already linked through signal-level",
            "connection metadata. Reassign it in",
            "Acquisition / telemetry -> Connection signals."
          )
        )
      }

      DBI::dbExecute(
        con,
        "
        UPDATE public.locations_metadata_instruments
        SET timeseries_id = NULL
        WHERE timeseries_id = $1
          AND metadata_id <> $2
          AND start_datetime <= NOW()
          AND (end_datetime IS NULL OR end_datetime > NOW())
        ",
        params = list(timeseries_id, deployment_metadata_id)
      )

      DBI::dbExecute(
        con,
        "
        UPDATE public.locations_metadata_instruments
        SET timeseries_id = $1
        WHERE metadata_id = $2
        ",
        params = list(timeseries_id, deployment_metadata_id)
      )

      invisible(NULL)
    }

    getModuleData <- function() {
      moduleData$timeseries <- DBI::dbGetQuery(
        session$userData$AquaCache,
        paste(
          "SELECT ts.timeseries_id, ts.location_id, ts.sub_location_id,",
          "ts.timezone_daily_calc, lz.z_meters AS z, ts.z_id, ts.media_id,",
          "ts.parameter_id, ts.matrix_state_id, ts.aggregation_type_id,",
          "ts.sensor_priority, ts.default_owner, ts.record_rate,",
          "ts.share_with, ts.source_fx, ts.source_fx_args, ts.note,",
          "ts.default_data_sharing_agreement_id",
          "FROM timeseries ts",
          "LEFT JOIN public.locations_z lz ON ts.z_id = lz.z_id"
        )
      )
      moduleData$locations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT l.location_id, l.name, lt.type, l.latitude, l.longitude FROM locations l INNER JOIN location_types lt ON l.location_type = lt.type_id ORDER BY l.name ASC"
      )
      moduleData$sub_locations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT sub_location_id, sub_location_name, location_id FROM sub_locations ORDER BY sub_location_name ASC"
      )
      moduleData$locations_z <- DBI::dbGetQuery(
        session$userData$AquaCache,
        paste(
          "SELECT z_id, location_id, sub_location_id, z_meters",
          "FROM public.locations_z",
          "ORDER BY location_id ASC, sub_location_id ASC NULLS FIRST, z_meters ASC"
        )
      )
      moduleData$matrix_states <- DBI::dbGetQuery(
        session$userData$AquaCache,
        paste(
          "SELECT matrix_state_id, matrix_state_code, matrix_state_name",
          "FROM public.matrix_states",
          "ORDER BY matrix_state_name ASC"
        )
      )
      moduleData$parameters <- DBI::dbGetQuery(
        session$userData$AquaCache,
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
      moduleData$media <- DBI::dbGetQuery(
        session$userData$AquaCache,
        paste(
          "SELECT media_id, media_type, default_matrix_state_id",
          "FROM media_types ORDER BY media_type ASC"
        )
      )
      moduleData$aggregation_types <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT aggregation_type_id, aggregation_type FROM aggregation_types ORDER BY aggregation_type ASC"
      )
      moduleData$organizations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT organization_id, name FROM organizations ORDER BY name ASC"
      )
      moduleData$users <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT * FROM public.get_shareable_principals_for('continuous.timeseries') ORDER BY role_name ASC;"
      ) # This is a helper function run with SECURITY DEFINER and created by postgres that pulls all user groups (plus public_reader) with select privileges on a table

      moduleData$timeseries_display <- DBI::dbGetQuery(
        session$userData$AquaCache,
        paste(
          "SELECT ts.timeseries_id, l.name AS location_name,",
          "sl.sub_location_name, ts.timezone_daily_calc AS time_zone,",
          "p.param_name AS parameter,",
          ac_parameter_unit_select_sql(
            session$userData$AquaCache,
            "p",
            "units",
            matrix_state_alias = "ts",
            media_alias = "ts"
          ),
          ", ms.matrix_state_name AS matrix_state, m.media_type AS media,",
          "at.aggregation_type, lz.z_meters AS depth_height_m,",
          "ts.sensor_priority, o.name AS owner, ts.record_rate",
          "FROM timeseries ts",
          "INNER JOIN locations l ON ts.location_id = l.location_id",
          "LEFT JOIN sub_locations sl ON ts.sub_location_id = sl.sub_location_id",
          "LEFT JOIN locations_z lz ON ts.z_id = lz.z_id",
          "INNER JOIN parameters p ON ts.parameter_id = p.parameter_id",
          "LEFT JOIN public.matrix_states ms",
          "ON ts.matrix_state_id = ms.matrix_state_id",
          "INNER JOIN media_types m ON ts.media_id = m.media_id",
          "INNER JOIN aggregation_types at",
          "ON ts.aggregation_type_id = at.aggregation_type_id",
          "INNER JOIN organizations o ON ts.default_owner = o.organization_id"
        )
      )
      # Join on files.document_types.document_type_en = 'data sharing agreement' to get only data sharing agreements
      moduleData$agreements <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT * FROM files.documents as f LEFT JOIN files.document_types as dt ON f.type = dt.document_type_id WHERE dt.document_type_en = 'data sharing agreement';"
      )
      moduleData$owners_agreements <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT * FROM public.organization_data_sharing_agreements;"
      )
      moduleData$deployed_instruments <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "
        SELECT
          lmi.metadata_id,
          lmi.location_id,
          lmi.sub_location_id,
          lmi.z_id,
          lz.z_meters,
          lmi.timeseries_id,
          COALESCE(
            lmi.timeseries_id,
            CASE
              WHEN COALESCE(sig.distinct_signal_timeseries_count, 0) = 1
                THEN sig.signal_timeseries_id
            END
          ) AS associated_timeseries_id,
          COALESCE(sig.connection_count, 0) AS connection_count,
          COALESCE(sig.signal_row_count, 0) AS signal_row_count,
          COALESCE(sig.mapped_signal_count, 0) AS mapped_signal_count,
          COALESCE(sig.distinct_signal_timeseries_count, 0)
            AS distinct_signal_timeseries_count,
          lmi.instrument_id,
          i.serial_no,
          mk.make,
          mdl.model,
          it.type AS instrument_type,
          lmi.start_datetime
        FROM public.locations_metadata_instruments AS lmi
        LEFT JOIN LATERAL (
          SELECT
            COUNT(DISTINCT c.connection_id) AS connection_count,
            COUNT(s.connection_signal_id) AS signal_row_count,
            COUNT(*) FILTER (WHERE s.timeseries_id IS NOT NULL)
              AS mapped_signal_count,
            COUNT(DISTINCT s.timeseries_id)
              FILTER (WHERE s.timeseries_id IS NOT NULL)
              AS distinct_signal_timeseries_count,
            MIN(s.timeseries_id)
              FILTER (WHERE s.timeseries_id IS NOT NULL)
              AS signal_timeseries_id
          FROM public.locations_metadata_instrument_connections AS c
          LEFT JOIN public.locations_metadata_instrument_connection_signals AS s
            ON s.connection_id = c.connection_id
          WHERE c.instrument_metadata_id = lmi.metadata_id
            AND c.start_datetime <= NOW()
            AND (c.end_datetime IS NULL OR c.end_datetime > NOW())
        ) AS sig ON TRUE
        INNER JOIN instruments.instruments AS i
          ON lmi.instrument_id = i.instrument_id
        LEFT JOIN instruments.instrument_make AS mk
          ON i.make = mk.make_id
        LEFT JOIN instruments.instrument_model AS mdl
          ON i.model = mdl.model_id
        LEFT JOIN instruments.instrument_type AS it
          ON i.type = it.type_id
        LEFT JOIN public.locations_z AS lz
          ON lmi.z_id = lz.z_id
        WHERE lmi.start_datetime <= NOW()
          AND (lmi.end_datetime IS NULL OR lmi.end_datetime > NOW())
        ORDER BY i.serial_no ASC, lmi.start_datetime DESC, lmi.metadata_id DESC
        "
      )
    }

    getModuleData() # Initial data load

    choices <- ls(getNamespace("AquaCache"))
    moduleData$source_fx <- choices[grepl("^download", choices)]

    output$ui <- renderUI({
      orgs <- isolate(moduleData$organizations)

      req(
        moduleData$locations,
        moduleData$parameters,
        moduleData$matrix_states,
        moduleData$media,
        moduleData$aggregation_types,
        moduleData$organizations,
        moduleData$users,
        moduleData$timeseries,
        moduleData$locations_z,
        orgs,
        moduleData$agreements
      )
      tagList(
        actionButton(
          ns("reload_module"),
          "Reload module data",
          icon = icon("refresh")
        ),
        radioButtons(
          ns("mode"),
          NULL,
          choices = c(
            "Add new timeseries" = "add",
            "Modify existing timeseries" = "modify"
          ),
          inline = TRUE
        ),
        conditionalPanel(
          condition = "input.mode == 'modify'",
          ns = ns,
          accordion(
            id = ns("accordion1"),
            open = "timeseries_table_panel",
            accordion_panel(
              id = ns("timeseries_table_panel"),
              title = "Select timeseries to modify",
              DT::DTOutput(ns("ts_table"))
            )
          )
        ),
        conditionalPanel(
          condition = "input.mode == 'add'",
          ns = ns,
          tags$div(
            class = "alert alert-info",
            "Tip: if you add a new timeseries with a source_fx and appropriate arguments, data will automatically be fetched from the source when you click 'Add timeseries'. If you leave the source_fx blank, you can enter data manually or use other methods. Note that WSC timeseries will get daily mean measurements as well as realtime measurements as far back as exist."
          )
        ),
        fluidRow(
          column(
            width = 4,
            selectizeInput(
              ns("location"),
              "Location (add new under the 'locations' menu)",
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
            4,
            selectizeInput(
              ns("sub_location"),
              "Sub-location (add new under the 'locations' menu)",
              choices = stats::setNames(
                moduleData$sub_locations$sub_location_id,
                moduleData$sub_locations$sub_location_name
              ),
              multiple = TRUE,
              options = list(maxItems = 1, placeholder = 'Optional'),
              width = "100%"
            )
          ),
          column(
            4,
            selectizeInput(
              ns("tz"),
              "Timezone for daily aggregation",
              choices = c(-12:14),
              selected = -7 # Default to MST (UTC-7)
            )
          ) |>
            tooltip(
              "The timezone used for calculating daily statistics. This should usually be the local timezone of the location.  Used to set the UTC times to capture a day. Note that this does not affect the timestamps of the raw data and is applied year-round (no daylight savings time adjustment)."
            )
        ),
        selectizeInput(
          ns("z"),
          "Elevation or depth, m (choose existing or type a new value)",
          choices = character(0),
          multiple = TRUE,
          options = list(
            maxItems = 1,
            placeholder = "Optional",
            create = TRUE,
            createFilter = htmlwidgets::JS(
              "function(input) { return /^-?(?:\\d+|\\d*\\.\\d+)$/.test($.trim(input)); }"
            ),
            createOnBlur = TRUE,
            persist = FALSE,
            plugins = list("clear_button")
          ),
          width = "100%"
        ) |>
          tooltip(
            paste(
              "If the height/depth at which this timeseries is measured is",
              "important to the data interpretation (e.g., wind tower",
              "anemometer height), specify it here. Existing values for the",
              "selected location/sub-location are listed, and you can type a",
              "new number if needed."
            )
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
            options = list(maxItems = 1, placeholder = 'Select a parameter'),
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
            options = list(maxItems = 1, placeholder = 'Select media type'),
            width = "100%"
          ),
          selectizeInput(
            ns("matrix_state"),
            tagList(
              "Matrix state ",
              tooltip(
                trigger = list(
                  tags$span(
                    "Why can't I see other states?",
                    style = paste(
                      "font-weight: normal;",
                      "font-size: 85%;",
                      "margin-left: 4px;"
                    )
                  ),
                  bsicons::bs_icon("info-circle-fill")
                ),
                paste(
                  "Matrix states are only visible if units have been",
                  "specified for this parameter in these states. Go to",
                  "the Reference data -> Parameters to add units. You'll",
                  "have to reload this module afterwards."
                )
              )
            ),
            choices = stats::setNames(
              moduleData$matrix_states$matrix_state_id,
              moduleData$matrix_states$matrix_state_name
            ),
            multiple = TRUE,
            options = list(maxItems = 1, placeholder = 'Select matrix state'),
            width = "100%"
          )
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
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
              placeholder = 'Select aggregation type'
            ),
            width = "100%"
          ) |>
            tooltip(
              "The aggregation type defines how the raw data is collected. For example, 'Instantaneous' means the data is collected at specific time points, while 'Mean' indicates that the data represents an average over the period between measurements. Choosing the correct aggregation type is crucial for accurate data analysis and interpretation."
            ),
          textInput(
            ns("record_rate"),
            "Rough record rate (5 minutes, 1 hour, 1 day, 1 week, etc.)",
            value = "",
            width = "100%"
          ) |>
            tooltip(
              "This should be reasonably accurate for the **recent** data collection period. Specifying a record rate greater than '1 day' will result in the timeseries having no calculated historical ranges."
            )
        ),
        selectizeInput(
          ns("sensor_priority"),
          "Sensor priority",
          choices = c("Primary" = 1, "Secondary" = 2, "Tertiary" = 3),
          selected = 1,
          multiple = TRUE,
          options = list(maxItems = 1, placeholder = 'Select sensor priority'),
          width = "100%"
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          selectizeInput(
            ns("default_owner"),
            "Default owner (type your own if not in list)",
            choices = stats::setNames(
              orgs$organization_id,
              orgs$name
            ),
            multiple = TRUE,
            options = list(
              maxItems = 1,
              placeholder = 'Select default owner',
              create = TRUE
            ),
            width = "100%"
          ) |>
            tooltip(
              "This is used to set the owner for uploaded data if it's not specified at upload time."
            ),
          selectizeInput(
            ns("data_sharing_agreement"),
            "Default data sharing agreement",
            choices = stats::setNames(
              moduleData$agreements$document_id,
              moduleData$agreements$name
            ),
            options = list(
              placeholder = "Optional - add the document first if needed",
              render = I(list(
                option = htmlwidgets::JS(
                  "function(item, escape) { return item.label; }"
                )
              ))
            ),
            width = "100%",
            multiple = FALSE
          ) |>
            tooltip(
              "Linking a default data sharing agreement will help us track compliance with data sharing agreements. Note that a different data sharing agreement can always be assigned when needed."
            )
        ),
        selectizeInput(
          ns("share_with"),
          "Share with groups (1 or more, or 'public_reader' to share with everyone)",
          choices = moduleData$users$role_name,
          selected = "public_reader",
          multiple = TRUE,
          width = "100%"
        ) |>
          tooltip(
            "Select the user groups that should have access to this timeseries data. 'public_reader' allows anyone with access to the system to view the data. You can select multiple groups IF public_reader is not one of them."
          ),
        accordion(
          id = ns("accordion2"),
          open = FALSE,
          accordion_panel(
            id = ns("instrument_association_panel"),
            title = "Instrument association (optional)",
            uiOutput(ns("instrument_association_ui")),
            selectizeInput(
              ns("instrument_deployment"),
              paste(
                "Associate this timeseries with a currently deployed",
                "instrument"
              ),
              choices = NULL,
              multiple = TRUE,
              options = list(
                maxItems = 1,
                placeholder = "Optional - select a deployed instrument",
                plugins = list("clear_button")
              ),
              width = "100%"
            ),
            actionButton(
              ns("remove_instrument_association"),
              "Remove instrument association",
              width = "100%"
            )
          ),
          accordion_panel(
            id = ns("source_fx_panel"),
            title = "Auto-download options",
            splitLayout(
              cellWidths = c("50%", "50%"),
              verticalLayout(
                # htmlOutput to tell the user when they should use the source functions and what the arguments are
                tags$div(
                  class = "alert alert-info",
                  "The source function is used to download data using the AquaCache R package. Leave blank if entering data manually or using other methods. For more information refer to the AquaCache package documentation."
                ),
                selectizeInput(
                  ns("source_fx"),
                  "Source function (see AquaCache package documentation for details)",
                  choices = moduleData$source_fx,
                  multiple = TRUE,
                  options = list(
                    maxItems = 1,
                    placeholder = 'Select source function (optional)'
                  ),
                  width = "100%"
                ),
                actionButton(
                  ns("source_fx_doc"),
                  "Open function documentation"
                )
              ),
              verticalLayout(
                # htmlOutput to tell the user how the source function arguments should be formatted
                tags$div(
                  class = "alert alert-info",
                  "Arguments must be formatted as key-value pairs for conversion to JSON, e.g. 'arg1: value1, arg2: value2'. Leave blank if not using a source_fx, otherwise refer to the function documentation in AquaCache."
                ),
                textInput(
                  ns("source_fx_args"),
                  "Source function arguments",
                  value = "",
                  placeholder = "arg1: value1, arg2: value2",
                  width = "100%"
                ),
                actionButton(
                  ns("args_example"),
                  "Show example arguments"
                )
              )
            )
          ),
          accordion_panel(
            id = ns("corrections_panel"),
            title = "Automatic corrections/filters",

            tags$p(
              # class = "text-muted",
              # "Use these bounds to automatically filter out values below/above specified thresholds. Raw data will not be altered. If left blank, no bound is applied."
              "This functionality is being developed."
            )
          )
        ),
        textAreaInput(
          ns("note"),
          "Note (optional)",
          value = "",
          rows = 3,
          placeholder = "Any additional information about this timeseries (optional)",
          width = "100%"
        ),
        conditionalPanel(
          condition = "input.mode == 'add'",
          ns = ns,
          bslib::input_task_button(
            ns("add_timeseries"),
            label = "Add timeseries"
          )
        ),
        conditionalPanel(
          condition = "input.mode == 'modify'",
          ns = ns,
          bslib::input_task_button(
            ns("modify_timeseries"),
            label = "Modify timeseries"
          )
        )
      )
    }) # End of output$ui

    # Render the timeseries table for modification
    output$ts_table <- DT::renderDT({
      # Convert some data types to factors for better filtering in DT
      df <- moduleData$timeseries_display
      df$location_name <- as.factor(df$location_name)
      df$record_rate <- as.factor(df$record_rate)
      df$media <- as.factor(df$media)
      df$matrix_state <- as.factor(df$matrix_state)
      df$units <- as.factor(df$units)
      df$aggregation_type <- as.factor(df$aggregation_type)
      df$parameter <- as.factor(df$parameter)
      df$owner <- as.factor(df$owner)
      df$sensor_priority <- as.factor(df$sensor_priority)
      df$time_zone <- as.factor(df$time_zone)
      df$z_id <- NULL # remove z_id as it's not useful to the user

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
            "  'font-size': '90%',",
            "});",
            "$(this.api().table().body()).css({",
            "  'font-size': '80%',",
            "});",
            "}"
          )
        ),
        filter = 'top',
        rownames = FALSE
      )
    }) |>
      bindEvent(moduleData$timeseries_display)

    output$instrument_association_ui <- renderUI({
      available <- available_instrument_deployments()
      current_assoc <- current_timeseries_association()
      z_value <- current_z_value()
      z_has_input <- current_z_has_input()
      current_tsid <- current_timeseries_id_for_association()

      tagList(
        div(
          class = "alert alert-info",
          tags$p(
            paste(
              "This list only includes instruments that are currently deployed",
              "at the same location, sub-location, and elevation/depth and do",
              "not already have a timeseries association."
            )
          ),
          tags$p(
            paste(
              "For new deployments, re-deployments, or changing instrument",
              "timeseries associations outside this form, use",
              "Field -> Deploy/recover instruments."
            )
          ),
          tags$p(
            paste(
              "If a deployment already has connection-signal metadata, manage",
              "the timeseries link in",
              "Acquisition / telemetry -> Connection signals."
            )
          )
        ),
        if (identical(input$mode, "modify") && is.na(current_tsid)) {
          div(
            class = "alert alert-warning",
            "Select a timeseries to modify before changing its instrument association."
          )
        },
        if (
          identical(input$mode, "modify") &&
            !is.na(current_tsid) &&
            nrow(current_assoc) > 0
        ) {
          div(
            class = "alert alert-primary",
            tags$strong("Current deployed association"),
            tags$br(),
            sprintf(
              "%s | %s | %s | %s",
              current_assoc$serial_no[[1]],
              safe_text(current_assoc$make[[1]]),
              safe_text(current_assoc$model[[1]]),
              safe_text(current_assoc$instrument_type[[1]])
            ),
            tags$br(),
            paste("Deployment metadata_id:", current_assoc$metadata_id[[1]]),
            if (deployment_has_signal_rows(current_assoc)) {
              tagList(
                tags$br(),
                tags$em(
                  paste(
                    "This association is managed through signal-level",
                    "connection metadata."
                  )
                )
              )
            }
          )
        } else if (identical(input$mode, "modify") && !is.na(current_tsid)) {
          div(
            class = "alert alert-secondary",
            "No deployed instruments associated with this timeseries."
          )
        },
        if (is.na(nullable_integer(input$location))) {
          div(
            class = "alert alert-warning",
            "Select a location to load eligible deployed instruments."
          )
        } else if (z_has_input && is.na(z_value)) {
          div(
            class = "alert alert-warning",
            "Elevation/depth must be a number."
          )
        } else if (nrow(available) == 0) {
          div(
            class = "alert alert-warning",
            paste(
              "No currently deployed instruments without a timeseries",
              "association match the current location, sub-location, and",
              "elevation/depth."
            )
          )
        } else {
          div(
            class = "alert alert-success",
            paste(
              nrow(available),
              "eligible deployed instrument(s) match the current location,",
              "sub-location, and elevation/depth."
            )
          )
        }
      )
    })

    observe({
      available <- available_instrument_deployments()
      current_tsid <- current_timeseries_id_for_association()
      current_input <- if (is.null(input$instrument_deployment)) {
        character(0)
      } else {
        as.character(input$instrument_deployment)
      }
      choices <- build_instrument_association_choices(available, current_tsid)
      choice_values <- unname(as.character(available$metadata_id))

      preferred <- character(0)
      current_assoc <- current_timeseries_association()
      if (nrow(current_assoc) > 0) {
        preferred <- as.character(current_assoc$metadata_id[[1]])
      }

      selected <- if (
        length(current_input) &&
          current_input[[1]] %in% choice_values
      ) {
        current_input[[1]]
      } else if (isTRUE(instrument_association_cleared())) {
        character(0)
      } else if (
        length(preferred) &&
          preferred[[1]] %in% choice_values
      ) {
        preferred[[1]]
      } else {
        character(0)
      }

      updateSelectizeInput(
        session,
        "instrument_deployment",
        choices = choices,
        selected = selected
      )
    })

    observeEvent(
      input$instrument_deployment,
      {
        if (length(input$instrument_deployment)) {
          instrument_association_cleared(FALSE)
        } else {
          instrument_association_cleared(TRUE)
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$remove_instrument_association,
      {
        instrument_association_cleared(TRUE)
        updateSelectizeInput(
          session,
          "instrument_deployment",
          selected = character(0)
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      selected_tsid(),
      {
        instrument_association_cleared(FALSE)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$mode,
      {
        instrument_association_cleared(FALSE)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$reload_module,
      {
        instrument_association_cleared(FALSE)
        getModuleData()
        selected_tsid(NULL)
        # Clear table row selection
        DT::dataTableProxy("ts_table") |>
          DT::selectRows(NULL)
        updateSelectizeInput(
          session,
          "location",
          choices = stats::setNames(
            moduleData$locations$location_id,
            moduleData$locations$name
          )
        )
        updateSelectizeInput(
          session,
          "parameter",
          choices = stats::setNames(
            moduleData$parameters$parameter_id,
            moduleData$parameters$param_name
          )
        )
        updateSelectizeInput(
          session,
          "media",
          choices = stats::setNames(
            moduleData$media$media_id,
            moduleData$media$media_type
          )
        )
        update_matrix_state_selectize(selected = NA_integer_)
        updateSelectizeInput(
          session,
          "aggregation_type",
          choices = stats::setNames(
            moduleData$aggregation_types$aggregation_type_id,
            moduleData$aggregation_types$aggregation_type
          )
        )
        updateTextInput(session, "record_rate", value = "")
        updateSelectizeInput(
          session,
          "default_owner",
          choices = stats::setNames(
            moduleData$organizations$organization_id,
            moduleData$organizations$name
          )
        )
        updateSelectizeInput(
          session,
          "data_sharing_agreement",
          selected = character(0)
        )
        updateSelectizeInput(
          session,
          "share_with",
          choices = moduleData$users$role_name
        )
        updateSelectizeInput(
          session,
          "source_fx",
          choices = moduleData$source_fx
        )
        showNotification("Module reloaded", type = "message")
      },
      ignoreInit = TRUE
    )

    observeEvent(
      list(input$location, input$sub_location, moduleData$locations_z),
      {
        update_z_selectize()
      }
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

    observe({
      req(
        moduleData$parameters,
        moduleData$media,
        moduleData$matrix_states
      )

      selected_state <- resolve_matrix_state_selection(
        parameter_id = nullable_integer(input$parameter),
        media_id = nullable_integer(input$media),
        current_matrix_state_id = nullable_integer(input$matrix_state)
      )
      update_matrix_state_selectize(
        selected = selected_state,
        parameter_id = nullable_integer(input$parameter)
      )
    })

    # Make sure share_with is either public_reader or other groups, not both
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

    # Show the user a modal with example arguments for the selected source function
    observeEvent(input$args_example, {
      if (is.null(input$source_fx) || input$source_fx == "") {
        showModal(modalDialog(
          "Select a source function to view example arguments.",
          easyClose = TRUE
        ))
        return()
      }
      ex_args <- moduleData$timeseries[
        moduleData$timeseries$source_fx == input$source_fx,
        "source_fx_args"
      ]
      ex_args <- ex_args[!is.na(ex_args)][1:10]
      ex_args <- ex_args[nzchar(ex_args)]
      # strip the [], {}, and "" from the json strings
      ex_args <- gsub("\\[|\\]|\\{|\\}|\"", "", ex_args)
      showModal(modalDialog(
        title = paste("Example arguments for", input$source_fx),
        if (length(ex_args) > 0) {
          tags$pre(paste(unique(ex_args), collapse = "\n"))
        } else {
          "No example arguments found in existing timeseries. Please refer to the AquaCache package documentation for details on the required arguments."
        },
        easyClose = TRUE
      ))
    })

    # Open the documentation for the selected source function in a new browser tab
    observeEvent(input$source_fx_doc, {
      if (is.null(input$source_fx) || input$source_fx == "") {
        showModal(modalDialog(
          "Please select a source function to see its documentation.",
          easyClose = TRUE
        ))
        return()
      }
      package <- tools::Rd_db("AquaCache")
      file <- paste0(input$source_fx, ".Rd")
      if (!file %in% names(package)) {
        showModal(modalDialog(
          "Documentation not found for the selected function.",
          easyClose = TRUE
        ))
        return()
      }
      # output path under the served directory, set up in globals
      out <- file.path(.rd_dir, paste0(input$source_fx, ".html"))
      tools::Rd2HTML(
        package[[file]],
        out,
        no_links = TRUE,
        package = "AquaCache"
      )
      # URL that the client can reach
      rdoc_url <- function(session, filename) {
        path <- session$clientData$url_pathname
        if (is.null(path) || !nzchar(path)) {
          path <- "/"
        }
        if (!grepl("/$", path)) {
          path <- paste0(path, "/")
        }
        paste0(path, "rdocs/", filename)
      }
      url <- rdoc_url(session, basename(out)) # not namespaced
      shinyjs::runjs(sprintf("window.open('%s','_blank');", url))
    })

    # Observe row selection and update inputs accordingly
    observeEvent(input$ts_table_rows_selected, {
      sel <- input$ts_table_rows_selected
      if (length(sel) > 0) {
        tsid <- moduleData$timeseries_display[sel, "timeseries_id"]
        selected_tsid(tsid)
        # Fetch the record from the basic timeseries table, not the timeseries_display as we need the numeric keys
        details <- moduleData$timeseries[
          moduleData$timeseries$timeseries_id == tsid,
        ]
        if (nrow(details) > 0) {
          # Update inputs with the selected timeseries details
          updateSelectizeInput(
            session,
            "location",
            selected = details$location_id
          )
          updateSelectizeInput(
            session,
            "sub_location",
            selected = details$sub_location_id
          )
          updateSelectizeInput(
            session,
            "tz",
            selected = details$timezone_daily_calc
          )
          update_z_selectize(
            selected = details$z,
            location_id = details$location_id,
            sub_location_id = details$sub_location_id
          )
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
          updateTextInput(
            session,
            "record_rate",
            value = ifelse(is.na(details$record_rate), "", details$record_rate)
          )
          updateSelectizeInput(
            session,
            "sensor_priority",
            selected = details$sensor_priority
          )
          updateSelectizeInput(
            session,
            "default_owner",
            selected = details$default_owner
          )

          # Based on the default owner, find the associated agreements if any
          owner_agreements <- moduleData$owners_agreements[
            moduleData$owners_agreements$organization_id ==
              details$default_owner,
            "document_id"
          ]
          owner_agreements <- unique(owner_agreements)
          owner_agreements <- owner_agreements[!is.na(owner_agreements)]

          agreements_df <- moduleData$agreements
          agreements_df$name <- as.character(agreements_df$name)

          is_owner <- agreements_df$document_id %in% owner_agreements
          agreements_df <- agreements_df[order(!is_owner, agreements_df$name), ]

          # Create labels with (recommended) for owner agreements
          labels <- ifelse(
            is_owner[match(
              agreements_df$document_id,
              moduleData$agreements$document_id
            )],
            paste0(
              "<b>",
              htmltools::htmlEscape(agreements_df$name),
              " (associated with default owner)</b>"
            ),
            htmltools::htmlEscape(agreements_df$name)
          )

          updateSelectizeInput(
            session,
            "data_sharing_agreement",
            choices = stats::setNames(agreements_df$document_id, labels),
            selected = details$data_sharing_agreement_id
          )

          updateSelectizeInput(
            session,
            "share_with",
            selected = array_to_text(details$share_with)
          )
          updateSelectizeInput(
            session,
            "source_fx",
            selected = details$source_fx
          )
          updateTextInput(
            session,
            "source_fx_args",
            value = ifelse(
              is.na(details$source_fx_args),
              "",
              parse_source_args(details$source_fx_args)
            )
          )
          updateTextAreaInput(
            session,
            "note",
            value = ifelse(is.na(details$note), "", details$note)
          )
        } else {
          showNotification(
            "Selected timeseries not found in the database.",
            type = "error"
          )
        }
      } else {
        selected_tsid(NULL)
      }
    })

    ### Observe the owner selectizeInput for new owners. If owner exists, find associated agreements and update selectizeInput ############
    observeEvent(
      input$default_owner,
      {
        resolved <- resolve_selectize_lookup_values(
          input$default_owner,
          moduleData$organizations$organization_id,
          moduleData$organizations$name
        )
        pending_default_owner_selection(resolved$existing_selection)

        if (!length(resolved$submitted_values)) {
          pending_default_owner_new(NULL)
          return()
        }

        if (length(resolved$new_values)) {
          pending_default_owner_new(resolved$last_new_value)
          showModal(
            modalDialog(
              title = "Add new owner",
              textInput(
                ns("owner_name"),
                "Owner name",
                value = resolved$last_new_value
              ),
              textInput(ns("owner_name_fr"), "Owner name French (optional)"),
              textInput(ns("contact_name"), "Contact name (optional)"),
              textInput(ns("contact_phone"), "Contact phone (optional)"),
              textInput(ns("contact_email"), "Contact email (optional)"),
              textInput(
                ns("contact_note"),
                "Contact note (optional, for context)"
              ),
              footer = tagList(
                actionButton(ns("cancel_add_owner"), "Cancel"),
                actionButton(ns("add_owner"), "Add owner")
              ),
              easyClose = FALSE
            )
          )
          return()
        }

        pending_default_owner_new(NULL)
        if (resolved$used_label_match) {
          update_default_owner_selectize(resolved$existing_selection)
          return()
        } else {
          # Find associated agreements for this owner
          owner_id <- as.numeric(resolved$existing_selection[[1]])
          owner_agreements <- moduleData$owners_agreements[
            moduleData$owners_agreements$organization_id == owner_id,
            "document_id"
          ]
          owner_agreements <- unique(owner_agreements)
          owner_agreements <- owner_agreements[!is.na(owner_agreements)]

          agreements_df <- moduleData$agreements
          agreements_df$name <- as.character(agreements_df$name)

          is_owner <- agreements_df$document_id %in% owner_agreements
          agreements_df <- agreements_df[order(!is_owner, agreements_df$name), ]

          # Create labels with (recommended) for owner agreements
          labels <- ifelse(
            is_owner[match(
              agreements_df$document_id,
              moduleData$agreements$document_id
            )],
            paste0(
              "<b>",
              htmltools::htmlEscape(agreements_df$name),
              " (associated with default owner)</b>"
            ),
            htmltools::htmlEscape(agreements_df$name)
          )

          updateSelectizeInput(
            session,
            "data_sharing_agreement",
            choices = stats::setNames(agreements_df$document_id, labels)
          )
        }
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$cancel_add_owner,
      {
        update_default_owner_selectize(pending_default_owner_selection())
        pending_default_owner_new(NULL)
        removeModal()
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    observeEvent(
      input$add_owner,
      {
        # Check that mandatory fields are filled in
        if (!isTruthy(input$owner_name)) {
          shinyjs::js$backgroundCol(ns("owner_name"), "#fdd")
          return()
        }
        owner_name <- trimws(input$owner_name)
        existing_id <- match_lookup_id_by_label(
          owner_name,
          moduleData$organizations$organization_id,
          moduleData$organizations$name
        )
        if (length(existing_id)) {
          update_default_owner_selectize(existing_id[[1]])
          pending_default_owner_selection(existing_id[[1]])
          pending_default_owner_new(NULL)
          removeModal()
          showNotification("Existing owner selected.", type = "message")
          return()
        }
        # Add the owner to the database
        df <- data.frame(
          name = owner_name,
          name_fr = if (isTruthy(input$owner_name_fr)) {
            trimws(input$owner_name_fr)
          } else {
            NA
          },
          contact_name = if (isTruthy(input$contact_name)) {
            trimws(input$contact_name)
          } else {
            NA
          },
          phone = if (isTruthy(input$contact_phone)) {
            trimws(input$contact_phone)
          } else {
            NA
          },
          email = if (isTruthy(input$contact_email)) {
            trimws(input$contact_email)
          } else {
            NA
          },
          note = if (isTruthy(input$contact_note)) trimws(input$contact_note) else NA
        )
        DBI::dbExecute(
          session$userData$AquaCache,
          "INSERT INTO public.organizations (name, name_fr, contact_name, phone, email, note) VALUES ($1, $2, $3, $4, $5, $6);",
          params = list(
            df$name,
            df$name_fr,
            df$contact_name,
            df$phone,
            df$email,
            df$note
          )
        )

        # Update the moduleData reactiveValues
        moduleData$organizations <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT organization_id, name FROM organizations"
        )
        # Update the selectizeInput to the new value
        new_id <- match_lookup_id_by_label(
          df$name,
          moduleData$organizations$organization_id,
          moduleData$organizations$name
        )
        update_default_owner_selectize(new_id)
        pending_default_owner_selection(new_id)
        pending_default_owner_new(NULL)
        removeModal()
        showModal(modalDialog(
          "New owner added.",
          easyClose = TRUE
        ))
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    # Add a new timeseries #############
    # Create an extendedTask to add a new timeseries
    addNewTimeseries <- ExtendedTask$new(
      function(
        config,
        loc,
        sub_loc,
        tz,
        z,
        parameter,
        media,
        matrix_state,
        priority,
        agg_type,
        rate,
        owner,
        note,
        source_fx,
        source_fx_args,
        instrument_deployment,
        data,
        share_with
      ) {
        promises::future_promise(seed = TRUE, expr = {
          tryCatch(
            {
              # Make a connection
              con <- AquaConnect(
                name = config$dbName,
                host = config$dbHost,
                port = config$dbPort,
                username = config$dbUser,
                password = config$dbPass,
                silent = TRUE
              )
              on.exit(DBI::dbDisconnect(con)) # Disconnect when done

              # start a transaction
              DBI::dbBegin(con)

              if (is.null(sub_loc)) {
                sub_loc <- NA
              } else if (nzchar(sub_loc)) {
                sub_loc <- as.numeric(sub_loc)
              } else {
                sub_loc <- NA
              }

              if (!is.null(source_fx_args)) {
                if (nzchar(source_fx_args)) {
                  args <- format_source_args(source_fx_args)
                } else {
                  args <- NA
                }
              } else {
                # if the source_fx_args is NULL, we set it to NA
                args <- NA
              }

              if (!is.null(source_fx)) {
                if (nzchar(source_fx)) {
                  source_fx <- source_fx
                } else {
                  source_fx <- NA
                }
                if (source_fx == "downloadNWIS") {
                  # NWIS data is only available from 2007 onwards, and errors if a date in the 1900s or earlier is specified.
                  end_datetime <- "2000-01-01"
                } else {
                  end_datetime <- "1800-01-01"
                }
              } else {
                source_fx <- NA
                end_datetime <- NA
              }

              z <- nullable_numeric(z)
              existing_z <- get_or_create_location_z_id(
                con = con,
                location_id = loc,
                sub_location_id = sub_loc,
                z_value = z
              )

              # Make a new entry to the timeseries table
              new_timeseries_id <- DBI::dbGetQuery(
                con,
                "INSERT INTO continuous.timeseries (location_id, sub_location_id, timezone_daily_calc, z_id, parameter_id, media_id, matrix_state_id, sensor_priority, aggregation_type_id, record_rate, default_owner, share_with, source_fx, source_fx_args, note, end_datetime) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16) RETURNING timeseries_id;",
                params = list(
                  as.numeric(loc),
                  ifelse(is.na(sub_loc), NA, sub_loc),
                  as.numeric(tz),
                  ifelse(is.na(existing_z), NA, existing_z),
                  as.numeric(parameter),
                  as.numeric(media),
                  as.numeric(matrix_state),
                  as.numeric(priority),
                  as.numeric(agg_type),
                  rate,
                  as.numeric(owner),
                  share_with_to_array(share_with),
                  ifelse(is.na(source_fx), NA, source_fx),
                  ifelse(is.na(args), NA, args),
                  if (nzchar(note)) note else NA,
                  ifelse(is.na(end_datetime), NA, end_datetime)
                )
              )[1, 1]

              update_timeseries_instrument_association(
                con = con,
                timeseries_id = new_timeseries_id,
                deployment_metadata_id = instrument_deployment
              )

              # Fetch historical data if source_fx is provided
              if (!is.na(source_fx)) {
                AquaCache::getNewContinuous(
                  con = con,
                  timeseries_id = new_timeseries_id
                )
                new_start <- DBI::dbGetQuery(
                  con,
                  "SELECT MIN(datetime) FROM continuous.measurements_continuous WHERE timeseries_id = $1",
                  params = list(new_timeseries_id)
                )[1, 1]
                # If new_start is NA if means there's no data, so set it to end_datetime
                if (!is.na(new_start)) {
                  DBI::dbExecute(
                    con,
                    "UPDATE timeseries SET start_datetime = $1 WHERE timeseries_id = $2",
                    params = list(new_start, new_timeseries_id)
                  )
                }

                # Now conditionally check for HYDAT historical data
                if (source_fx == "downloadWSC") {
                  param_name <- data$parameters[
                    data$parameters$parameter_id == parameter,
                    "param_name"
                  ]
                  if (param_name %in% c("water level", "water flow")) {
                    suppressMessages(AquaCache::update_hydat(
                      timeseries_id = new_timeseries_id,
                      force_update = TRUE,
                      con = con
                    ))
                  }
                }

                # Ensure that there are records in measurements_continuous and/or measurements_calculated_daily
                mcd <- DBI::dbGetQuery(
                  con,
                  "SELECT MIN(date) FROM continuous.measurements_calculated_daily WHERE timeseries_id = $1",
                  params = list(new_timeseries_id)
                )[1, 1]
                mc <- DBI::dbGetQuery(
                  con,
                  "SELECT MIN(datetime) FROM continuous.measurements_continuous WHERE timeseries_id = $1",
                  params = list(new_timeseries_id)
                )[1, 1]

                if (is.na(mcd) && is.na(mc)) {
                  stop(
                    "Could not find any data for this timeseries. Try different parameters."
                  )
                }

                # Now calculate stats
                if (lubridate::period(rate) <= lubridate::period("1 day")) {
                  AquaCache::calculate_stats(
                    timeseries_id = new_timeseries_id,
                    con = con,
                    start_recalc = NULL
                  )
                }
                DBI::dbCommit(con)
                return("success")
              } else {
                DBI::dbCommit(con)
                return("successNoData")
              }
            },
            error = function(e) {
              DBI::dbRollback(con)
              return(paste("Error adding timeseries:", e$message))
            },
            warning = function(w) {
              DBI::dbRollback(con)
              return(paste(
                "Failure due to warning when adding timeseries:",
                w$message
              ))
            }
          )
        })
      } # end of ExtendedTask$new
    ) |>
      bslib::bind_task_button("add_timeseries")
    # End of ExtendedTask$new

    observeEvent(input$add_timeseries, {
      # validate inputs
      validate(
        need(input$location, "Please select a location."),
        need(input$parameter, "Please select a parameter."),
        need(input$media, "Please select a media type."),
        need(input$matrix_state, "Please select a matrix state."),
        need(input$aggregation_type, "Please select an aggregation type."),
        need(input$default_owner, "Please select a default owner."),
        need(input$sensor_priority, "Please select a sensor priority."),
        need(input$record_rate, "Please specify a record rate.")
      )

      matrix_state_error <- validate_timeseries_matrix_state(
        parameter_id = input$parameter,
        media_id = input$media,
        matrix_state_id = input$matrix_state
      )
      if (!is.null(matrix_state_error)) {
        showNotification(
          matrix_state_error,
          type = "error",
          duration = 8
        )
        return()
      }

      if (input$mode != "add") {
        # This is an error: show the user a notification to select 'add' mode
        showNotification(
          "Please select 'Add new' mode to add a timeseries.",
          type = "error",
          duration = 8
        )
        return()
      }

      if (current_z_has_input() && is.na(current_z_value())) {
        showNotification(
          "Elevation/depth must be numeric.",
          type = "error",
          duration = 8
        )
        return()
      }

      # if input$source_fx_args is not blank, validate that it is in the correct format.
      # Should have no =, no "" or '', and have : separating key and value
      if (nzchar(input$source_fx_args)) {
        if (grepl("=", input$source_fx_args)) {
          showNotification(
            "Source function arguments should use ':' to separate keys and values, not '='.",
            type = "error",
            duration = 8
          )
          return()
        }
        if (grepl("\"|'", input$source_fx_args)) {
          showNotification(
            "Source function arguments should not contain quotes (\") or (').",
            type = "error",
            duration = 8
          )
          return()
        }
        if (!all(grepl(":", unlist(strsplit(input$source_fx_args, ",\\s*"))))) {
          showNotification(
            "Source function arguments should use ':' to separate keys and values.",
            type = "error",
            duration = 8
          )
          return()
        }
      }

      # Call the extendedTask to add a new timeseries
      addNewTimeseries$invoke(
        config = session$userData$config,
        loc = input$location,
        sub_loc = input$sub_location,
        tz = input$tz,
        z = current_z_value(),
        parameter = input$parameter,
        media = input$media,
        matrix_state = input$matrix_state,
        priority = input$sensor_priority,
        agg_type = input$aggregation_type,
        rate = input$record_rate,
        owner = input$default_owner,
        note = input$note,
        source_fx = input$source_fx,
        source_fx_args = input$source_fx_args,
        instrument_deployment = input$instrument_deployment,
        data = reactiveValuesToList(moduleData),
        share_with = input$share_with
      )
    })

    # Observe the result of the ExtendedTask
    observeEvent(addNewTimeseries$result(), {
      if (is.null(addNewTimeseries$result())) {
        return() # No result yet, do nothing
      } else if (
        !addNewTimeseries$result() %in% c("successNoData", "success")
      ) {
        # If the result is not "success", show an error notification
        showNotification(addNewTimeseries$result(), type = "error")
        return()
      } else if (addNewTimeseries$result() == "successNoData") {
        showNotification(
          "Timeseries added successfully with no data fetched. REMEMBER TO ADD DATA NOW.",
          type = "warning",
          duration = 10
        )
      } else if (addNewTimeseries$result() == "success") {
        # If the result is "success", show a success notification
        showNotification(
          "Timeseries added successfully! Historical data was fetched and daily means calculated if you provided a source_fx.",
          type = "message",
          duration = 8
        )

        getModuleData()

        # Reset all fields
        updateSelectizeInput(session, "location", selected = character(0))
        updateSelectizeInput(session, "sub_location", selected = character(0))
        updateSelectizeInput(session, "tz", selected = -7)
        updateSelectizeInput(session, "z", selected = character(0))
        updateSelectizeInput(session, "parameter", selected = character(0))
        updateSelectizeInput(session, "media", selected = character(0))
        update_matrix_state_selectize(selected = NA_integer_)
        updateSelectizeInput(
          session,
          "aggregation_type",
          selected = character(0)
        )
        updateTextInput(session, "record_rate", value = "")
        updateSelectizeInput(session, "sensor_priority", selected = 1)
        updateSelectizeInput(session, "default_owner", selected = character(0))
        updateSelectizeInput(
          session,
          "data_sharing_agreement",
          selected = character(0)
        )
        updateSelectizeInput(session, "share_with", selected = "public_reader")
        updateSelectizeInput(session, "source_fx", selected = character(0))
        updateTextInput(session, "source_fx_args", value = "")
        updateSelectizeInput(
          session,
          "instrument_deployment",
          selected = character(0)
        )
        updateTextAreaInput(session, "note", value = "")
      }
    })

    # Modify existing timeseries ###############
    observeEvent(
      input$modify_timeseries,
      {
        required_errors <- c(
          if (!isTruthy(input$location)) "Please select a location.",
          if (!isTruthy(input$parameter)) "Please select a parameter.",
          if (!isTruthy(input$media)) "Please select a media type.",
          if (!isTruthy(input$matrix_state)) "Please select a matrix state.",
          if (!isTruthy(input$aggregation_type)) {
            "Please select an aggregation type."
          },
          if (!isTruthy(input$default_owner)) {
            "Please select a default owner."
          },
          if (!isTruthy(input$sensor_priority)) {
            "Please select a sensor priority."
          }
        )
        if (length(required_errors) > 0) {
          showNotification(
            required_errors[[1]],
            type = "error",
            duration = 8
          )
          return()
        }

        matrix_state_error <- validate_timeseries_matrix_state(
          parameter_id = input$parameter,
          media_id = input$media,
          matrix_state_id = input$matrix_state
        )
        if (!is.null(matrix_state_error)) {
          showNotification(
            matrix_state_error,
            type = "error",
            duration = 8
          )
          return()
        }

        if (input$mode != "modify") {
          # This is an error: show the user a notification to select 'modify' mode
          showNotification(
            "Please select 'Modify existing' mode to modify a timeseries.",
            type = "error",
            duration = 8
          )
          return()
        }

        if (current_z_has_input() && is.na(current_z_value())) {
          showNotification(
            "Elevation/depth must be numeric.",
            type = "error",
            duration = 8
          )
          return()
        }

        # If we are modifying an existing timeseries, we need to check if it exists
        selected_row <- input$ts_table_rows_selected
        if (is.null(selected_row) || length(selected_row) != 1) {
          showNotification(
            "Please select a single timeseries to modify.",
            type = "error",
            duration = 8
          )
          return()
        }
        tsid <- moduleData$timeseries_display[selected_row, "timeseries_id"]
        selected_timeseries <- moduleData$timeseries[
          moduleData$timeseries$timeseries_id == tsid,
        ]
        # Check if the timeseries already exists
        existing_timeseries <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT * FROM timeseries WHERE timeseries_id = $1;",
          params = list(selected_timeseries$timeseries_id)
        )
        if (nrow(existing_timeseries) == 0) {
          showNotification(
            "Selected timeseries does not exist in the database.",
            type = "error",
            duration = 8
          )
          return()
        }

        # If it exists, update the timeseries
        DBI::dbBegin(session$userData$AquaCache)

        tryCatch(
          {
            input_location_id <- nullable_integer(input$location)
            input_sub_location_id <- nullable_integer(input$sub_location)
            selected_timeseries_id <- nullable_integer(
              selected_timeseries$timeseries_id
            )
            input_parameter_id <- nullable_integer(input$parameter)
            input_media_id <- nullable_integer(input$media)
            input_matrix_state_id <- nullable_integer(input$matrix_state)
            input_aggregation_type_id <- nullable_integer(
              input$aggregation_type
            )
            input_sensor_priority <- nullable_integer(input$sensor_priority)
            input_default_owner <- nullable_integer(input$default_owner)
            input_data_sharing_agreement_id <- nullable_integer(
              input$data_sharing_agreement
            )
            input_record_rate <- nullable_text(input$record_rate)
            input_source_fx <- nullable_text(input$source_fx)
            input_source_fx_args <- nullable_text(input$source_fx_args)
            input_note <- nullable_text(input$note)
            input_z_value <- current_z_value()
            input_share_with_values <- if (
              is.null(input$share_with) || !length(input$share_with)
            ) {
              character(0)
            } else {
              as.character(input$share_with)
            }

            if (
              !same_nullable_integer(
                input_location_id,
                selected_timeseries$location_id
              )
            ) {
              DBI::dbExecute(
                session$userData$AquaCache,
                "UPDATE timeseries SET location_id = $1 WHERE timeseries_id = $2;",
                params = list(input_location_id, selected_timeseries_id)
              )
            }

            if (
              !same_nullable_integer(
                input_sub_location_id,
                selected_timeseries$sub_location_id
              )
            ) {
              if (is.na(input_sub_location_id)) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "UPDATE timeseries SET sub_location_id = NULL WHERE timeseries_id = $1;",
                  params = list(selected_timeseries_id)
                )
              } else {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "UPDATE timeseries SET sub_location_id = $1 WHERE timeseries_id = $2;",
                  params = list(
                    input_sub_location_id,
                    selected_timeseries_id
                  )
                )
              }
            }

            # If a change is made to tz, AquaCache::calculate_stats will need to be rerun from the beginning of the timeseries
            recalc_stats <- FALSE
            if (input$tz != selected_timeseries$timezone_daily_calc) {
              DBI::dbExecute(
                session$userData$AquaCache,
                "UPDATE timeseries SET timezone_daily_calc = $1 WHERE timeseries_id = $2;",
                params = list(input$tz, selected_timeseries$timeseries_id)
              )
              recalc_stats <- TRUE
            }

            if (!is.na(input_z_value)) {
              target_z_id <- get_or_create_location_z_id(
                con = session$userData$AquaCache,
                location_id = input_location_id,
                sub_location_id = input_sub_location_id,
                z_value = input_z_value
              )

              if (
                !same_nullable_integer(target_z_id, selected_timeseries$z_id) ||
                  !same_nullable_integer(
                    input_location_id,
                    selected_timeseries$location_id
                  ) ||
                  !same_nullable_integer(
                    input_sub_location_id,
                    selected_timeseries$sub_location_id
                  ) ||
                  !same_nullable_numeric(input_z_value, selected_timeseries$z)
              ) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "UPDATE timeseries SET z_id = $1 WHERE timeseries_id = $2",
                  params = list(target_z_id, selected_timeseries$timeseries_id)
                )
              }
            } else {
              if (!is.na(selected_timeseries$z_id)) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "UPDATE timeseries SET z_id = NULL WHERE timeseries_id = $1",
                  params = list(selected_timeseries_id)
                )
              }
            }

            if (
              !same_nullable_integer(
                input_parameter_id,
                selected_timeseries$parameter_id
              ) ||
                !same_nullable_integer(
                  input_media_id,
                  selected_timeseries$media_id
                ) ||
                !same_nullable_integer(
                  input_matrix_state_id,
                  selected_timeseries$matrix_state_id
                )
            ) {
              DBI::dbExecute(
                session$userData$AquaCache,
                paste(
                  "UPDATE timeseries",
                  "SET parameter_id = $1, media_id = $2, matrix_state_id = $3",
                  "WHERE timeseries_id = $4"
                ),
                params = list(
                  input_parameter_id,
                  input_media_id,
                  input_matrix_state_id,
                  selected_timeseries_id
                )
              )
            }

            if (
              !same_nullable_integer(
                input_aggregation_type_id,
                selected_timeseries$aggregation_type_id
              )
            ) {
              DBI::dbExecute(
                session$userData$AquaCache,
                "UPDATE timeseries SET aggregation_type_id = $1 WHERE timeseries_id = $2",
                params = list(
                  input_aggregation_type_id,
                  selected_timeseries_id
                )
              )
            }

            if (
              !same_nullable_text(
                input_record_rate,
                selected_timeseries$record_rate
              )
            ) {
              if (!is.na(input_record_rate)) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "UPDATE timeseries SET record_rate = $1 WHERE timeseries_id = $2",
                  params = list(
                    input_record_rate,
                    selected_timeseries_id
                  )
                )
              } else {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "UPDATE timeseries SET record_rate = NULL WHERE timeseries_id = $1",
                  params = list(selected_timeseries_id)
                )
              }
            }

            if (
              !same_nullable_integer(
                input_sensor_priority,
                selected_timeseries$sensor_priority
              )
            ) {
              DBI::dbExecute(
                session$userData$AquaCache,
                "UPDATE timeseries SET sensor_priority = $1 WHERE timeseries_id = $2",
                params = list(
                  input_sensor_priority,
                  selected_timeseries_id
                )
              )
            }

            # Changes to default_owner
            if (
              !same_nullable_integer(
                input_default_owner,
                selected_timeseries$default_owner
              )
            ) {
              DBI::dbExecute(
                session$userData$AquaCache,
                "UPDATE timeseries SET default_owner = $1 WHERE timeseries_id = $2",
                params = list(
                  input_default_owner,
                  selected_timeseries_id
                )
              )
            }

            # Changes to data sharing agreement
            if (
              !same_nullable_integer(
                input_data_sharing_agreement_id,
                selected_timeseries$default_data_sharing_agreement_id
              )
            ) {
              if (is.na(input_data_sharing_agreement_id)) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "UPDATE timeseries SET default_data_sharing_agreement_id = NULL WHERE timeseries_id = $1",
                  params = list(selected_timeseries_id)
                )
              } else {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "UPDATE timeseries SET default_data_sharing_agreement_id = $1 WHERE timeseries_id = $2",
                  params = list(
                    input_data_sharing_agreement_id,
                    selected_timeseries_id
                  )
                )
              }
            }

            # Changes to share_with
            parsed_exist_share_with <- array_to_text(
              selected_timeseries$share_with
            )
            parsed_exist_share_with <- if (
              is.null(parsed_exist_share_with) ||
                length(parsed_exist_share_with) == 0
            ) {
              character(0)
            } else {
              as.character(parsed_exist_share_with[
                !is.na(parsed_exist_share_with)
              ])
            }

            if (
              !identical(
                sort(unique(input_share_with_values)),
                sort(unique(parsed_exist_share_with))
              )
            ) {
              input_share_with <- share_with_to_array(input_share_with_values)
              if (
                is.null(input_share_with) ||
                  length(input_share_with) == 0 ||
                  (length(input_share_with) == 1 && is.na(input_share_with))
              ) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "UPDATE timeseries SET share_with = NULL WHERE timeseries_id = $1;",
                  params = list(selected_timeseries_id)
                )
              } else {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "UPDATE timeseries SET share_with = $1 WHERE timeseries_id = $2;",
                  params = list(
                    input_share_with,
                    selected_timeseries_id
                  )
                )
              }
            }

            # Changes to source_fx and source_fx_args
            if (
              !same_nullable_text(
                input_source_fx,
                selected_timeseries$source_fx
              )
            ) {
              if (!is.na(input_source_fx)) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "UPDATE timeseries SET source_fx = $1 WHERE timeseries_id = $2",
                  params = list(
                    input_source_fx,
                    selected_timeseries_id
                  )
                )
              } else {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "UPDATE timeseries SET source_fx = NULL WHERE timeseries_id = $1",
                  params = list(
                    selected_timeseries_id
                  )
                )
              }
            }

            if (!is.na(selected_timeseries$source_fx_args)) {
              if (is.na(input_source_fx_args)) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "UPDATE timeseries SET source_fx_args = NULL WHERE timeseries_id = $1",
                  params = list(
                    selected_timeseries_id
                  )
                )
              } else {
                # source_fx_args are fetched from DB as json, so we need to handle them accordingly for comparison
                # The following gives us a data.frame with column names as the keys and values as the values
                parsed_json <- jsonlite::fromJSON(
                  selected_timeseries$source_fx_args,
                  simplifyVector = TRUE,
                  flatten = TRUE
                )

                # Now work the input$source_fx_args into a data.frame with the same shape as parsed_json
                # split into “arg:value” pairs, trim whitespace
                arg_pairs <- strsplit(input_source_fx_args, ",")[[1]] # e.g. c("arg1: value1", "arg2: value2")
                arg_pairs <- trimws(arg_pairs) # remove leading/trailing spaces
                # split each on “:”, extract keys & values
                kv <- strsplit(arg_pairs, ":")
                keys <- sapply(kv, `[`, 1)
                vals <- sapply(kv, `[`, 2)
                # build a named list and then a one-row data.frame
                input_df <- stats::setNames(as.list(vals), keys)
                input_df <- as.data.frame(input_df, stringsAsFactors = FALSE)
                # now `parsed_json` and `input_df` have the same shape

                if (!identical(parsed_json, input_df)) {
                  # Make the source_fx_args a json object
                  args <- input_source_fx_args
                  # split into "argument1: value1" etc.
                  args <- strsplit(args, ",\\s*")[[1]]

                  # split only on first colon
                  keys <- sub(":.*", "", args)
                  vals <- sub("^[^:]+:\\s*", "", args)

                  # build named list
                  args <- stats::setNames(as.list(vals), keys)

                  # convert to JSON
                  args <- jsonlite::toJSON(args, auto_unbox = TRUE)

                  DBI::dbExecute(
                    session$userData$AquaCache,
                    "UPDATE timeseries SET source_fx_args = $1 WHERE timeseries_id = $2",
                    params = list(
                      args,
                      selected_timeseries_id
                    )
                  )
                }
              }
            } else if (!is.na(input_source_fx_args)) {
              args <- strsplit(input_source_fx_args, ",\\s*")[[1]]
              keys <- sub(":.*", "", args)
              vals <- sub("^[^:]+:\\s*", "", args)
              args <- stats::setNames(as.list(vals), keys)
              args <- jsonlite::toJSON(args, auto_unbox = TRUE)

              DBI::dbExecute(
                session$userData$AquaCache,
                "UPDATE timeseries SET source_fx_args = $1 WHERE timeseries_id = $2",
                params = list(
                  args,
                  selected_timeseries_id
                )
              )
            }

            if (!same_nullable_text(input_note, selected_timeseries$note)) {
              if (!is.na(input_note)) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "UPDATE timeseries SET note = $1 WHERE timeseries_id = $2",
                  params = list(
                    input_note,
                    selected_timeseries_id
                  )
                )
              } else {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "UPDATE continuous.timeseries SET note = NULL WHERE timeseries_id = $1",
                  params = list(
                    selected_timeseries_id
                  )
                )
              }
            }

            update_timeseries_instrument_association(
              con = session$userData$AquaCache,
              timeseries_id = selected_timeseries$timeseries_id,
              deployment_metadata_id = input$instrument_deployment
            )

            # If recalc_stats is TRUE, we need to recalculate stats from the beginning of the timeseries
            if (recalc_stats) {
              showNotification(
                "Recalculating statistics from the beginning of the timeseries due to timezone change. Please be patient.",
                type = "message",
                duration = 8
              )
              earliest <- DBI::dbGetQuery(
                session$userData$AquaCache,
                "SELECT MIN(datetime) FROM continuous.measurements_continuous WHERE timeseries_id = $1",
                params = list(selected_timeseries$timeseries_id)
              )[1, 1]
              AquaCache::calculate_stats(
                timeseries_id = selected_timeseries$timeseries_id,
                con = session$userData$AquaCache,
                start_recalc = earliest
              )
            }

            DBI::dbCommit(session$userData$AquaCache)
            showNotification(
              "Timeseries updated successfully!",
              type = "message"
            )
            getModuleData()
          },
          error = function(e) {
            DBI::dbRollback(session$userData$AquaCache)
            showNotification(
              paste("Error updating timeseries:", e$message),
              type = "error",
              duration = 10
            )
          }
        )
      },
      ignoreInit = TRUE
    )

    # HEADS UP! find the modules which depend on timeseries. These will have cached data, which will need to be cleared when a new location or timeseries is added using the clear_cached function (R/app_cache.R)
  }) # End of moduleServer
}
