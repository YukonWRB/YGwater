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
    pending_transmission_mapping <- reactiveVal(NULL)
    preferred_transmission_route_id <- reactiveVal(NULL)
    preferred_secondary_transmission_route_id <- reactiveVal(NULL)
    route_creation_target <- reactiveVal("primary")
    transmission_choices_version <- reactiveVal(0L)
    source_args_existing <- reactiveVal(NA_character_)
    source_args_existing_source <- reactiveVal(NA_character_)
    source_args_secondary_existing <- reactiveVal(NA_character_)
    source_args_secondary_existing_source <- reactiveVal(NA_character_)

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

    source_args_transmission_route_id <- function(source_fx_args) {
      source_fx_args <- nullable_text(source_fx_args)
      if (is.na(source_fx_args) || !jsonlite::validate(source_fx_args)) {
        return(NA_integer_)
      }
      args <- tryCatch(
        jsonlite::fromJSON(source_fx_args, simplifyVector = FALSE),
        error = function(e) NULL
      )
      if (is.null(args) || is.null(args$transmission_route_id)) {
        return(NA_integer_)
      }
      nullable_integer(args$transmission_route_id)
    }

    normalize_integer_vector <- function(x) {
      if (is.null(x) || length(x) == 0) {
        return(integer(0))
      }
      if (is.list(x) && length(x) == 1) {
        x <- x[[1]]
      }
      if (is.null(x) || length(x) == 0) {
        return(integer(0))
      }
      if (is.character(x) && length(x) == 1 && grepl("^\\{.*\\}$", x)) {
        x <- strsplit(sub("^\\{(.*)\\}$", "\\1", x), ",", fixed = TRUE)[[1]]
      }

      x <- x[!is.na(x)]
      if (!length(x)) {
        return(integer(0))
      }

      sort(unique(as.integer(x)))
    }

    row_has_timeseries <- function(row, timeseries_id, column_name) {
      timeseries_id <- nullable_integer(timeseries_id)
      if (
        is.na(timeseries_id) ||
          is.null(row) ||
          nrow(row) == 0 ||
          !column_name %in% names(row)
      ) {
        return(FALSE)
      }

      timeseries_id %in% normalize_integer_vector(row[[column_name]][[1]])
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
          unit_col %in%
            names(param_row) &&
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
        direct_timeseries_count = integer(0),
        direct_timeseries_ids = I(vector("list", 0)),
        signal_timeseries_ids = I(vector("list", 0)),
        associated_timeseries_ids = I(vector("list", 0)),
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
        "  COALESCE(lmit.timeseries_count, 0) AS direct_timeseries_count,",
        paste(
          "COALESCE(lmit.timeseries_ids, ARRAY[]::integer[])",
          "AS direct_timeseries_ids,"
        ),
        paste(
          "COALESCE(sig.signal_timeseries_ids, ARRAY[]::integer[])",
          "AS signal_timeseries_ids,"
        ),
        paste(
          "CASE",
          "  WHEN COALESCE(sig.signal_row_count, 0) > 0",
          "    THEN COALESCE(sig.signal_timeseries_ids, ARRAY[]::integer[])",
          "  ELSE COALESCE(lmit.timeseries_ids, ARRAY[]::integer[])",
          "END AS associated_timeseries_ids,"
        ),
        paste(
          "CASE",
          "  WHEN COALESCE(sig.signal_row_count, 0) > 0 AND",
          "    COALESCE(sig.distinct_signal_timeseries_count, 0) = 1",
          "    THEN sig.signal_timeseries_id",
          "  WHEN COALESCE(sig.signal_row_count, 0) = 0 AND",
          "    COALESCE(lmit.timeseries_count, 0) = 1",
          "    THEN lmit.single_timeseries_id",
          "END AS associated_timeseries_id,"
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
          "AS signal_timeseries_id,"
        ),
        paste(
          "ARRAY_AGG(DISTINCT s.timeseries_id ORDER BY s.timeseries_id)",
          "FILTER (WHERE s.timeseries_id IS NOT NULL)",
          "AS signal_timeseries_ids"
        ),
        "  FROM public.locations_metadata_instrument_connections AS c",
        "  LEFT JOIN public.locations_metadata_instrument_connection_signals AS s",
        "    ON s.connection_id = c.connection_id",
        "  WHERE c.instrument_metadata_id = lmi.metadata_id",
        "    AND c.start_datetime <= NOW()",
        "    AND (c.end_datetime IS NULL OR c.end_datetime > NOW())",
        ") AS sig ON TRUE",
        "LEFT JOIN LATERAL (",
        "  SELECT",
        "    COUNT(*)::integer AS timeseries_count,",
        "    MIN(lmit.timeseries_id) AS single_timeseries_id,",
        paste(
          "ARRAY_AGG(lmit.timeseries_id ORDER BY lmit.timeseries_id)",
          "AS timeseries_ids"
        ),
        "  FROM public.locations_metadata_instrument_timeseries AS lmit",
        "  WHERE lmit.metadata_id = lmi.metadata_id",
        ") AS lmit ON TRUE",
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

      rows <- vapply(
        seq_len(nrow(moduleData$deployed_instruments)),
        function(i) {
          row_has_timeseries(
            moduleData$deployed_instruments[i, , drop = FALSE],
            tsid,
            "associated_timeseries_ids"
          )
        },
        logical(1)
      )

      moduleData$deployed_instruments[rows, , drop = FALSE]
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

      if (nrow(available) > 0) {
        rows <- vapply(
          seq_len(nrow(available)),
          function(i) {
            row <- available[i, , drop = FALSE]
            !deployment_has_signal_rows(row) ||
              row_has_timeseries(
                row,
                current_tsid,
                "signal_timeseries_ids"
              )
          },
          logical(1)
        )
        available <- available[rows, , drop = FALSE]
      }

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

      current_idx <- if (is.na(current_tsid)) {
        rep(FALSE, nrow(df))
      } else {
        vapply(
          seq_len(nrow(df)),
          function(i) {
            row_has_timeseries(
              df[i, , drop = FALSE],
              current_tsid,
              "associated_timeseries_ids"
            )
          },
          logical(1)
        )
      }
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
        current_signal_assoc <- if (nrow(active_deployments) == 0) {
          active_deployments
        } else {
          active_deployments[
            vapply(
              seq_len(nrow(active_deployments)),
              function(i) {
                row <- active_deployments[i, , drop = FALSE]
                deployment_has_signal_rows(row) &&
                  row_has_timeseries(
                    row,
                    timeseries_id,
                    "signal_timeseries_ids"
                  )
              },
              logical(1)
            ),
            ,
            drop = FALSE
          ]
        }

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
          DELETE FROM public.locations_metadata_instrument_timeseries AS lmit
          USING public.locations_metadata_instruments AS lmi
          WHERE lmit.metadata_id = lmi.metadata_id
            AND lmit.timeseries_id = $1
            AND lmi.start_datetime <= NOW()
            AND (lmi.end_datetime IS NULL OR lmi.end_datetime > NOW())
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
          row_has_timeseries(
            selected_deployment,
            timeseries_id,
            "signal_timeseries_ids"
          )
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

      other_signal_assoc <- if (nrow(active_deployments) == 0) {
        active_deployments
      } else {
        active_deployments[
          active_deployments$metadata_id != deployment_metadata_id &
            vapply(
              seq_len(nrow(active_deployments)),
              function(i) {
                row <- active_deployments[i, , drop = FALSE]
                deployment_has_signal_rows(row) &&
                  row_has_timeseries(
                    row,
                    timeseries_id,
                    "signal_timeseries_ids"
                  )
              },
              logical(1)
            ),
          ,
          drop = FALSE
        ]
      }

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
        DELETE FROM public.locations_metadata_instrument_timeseries AS lmit
        USING public.locations_metadata_instruments AS lmi
        WHERE lmit.metadata_id = lmi.metadata_id
          AND lmit.timeseries_id = $1
          AND lmit.metadata_id <> $2
          AND lmi.start_datetime <= NOW()
          AND (lmi.end_datetime IS NULL OR lmi.end_datetime > NOW())
        ",
        params = list(timeseries_id, deployment_metadata_id)
      )

      DBI::dbExecute(
        con,
        "
        INSERT INTO public.locations_metadata_instrument_timeseries (
          metadata_id,
          timeseries_id
        ) VALUES ($1, $2)
        ON CONFLICT (metadata_id, timeseries_id) DO NOTHING
        ",
        params = list(deployment_metadata_id, timeseries_id)
      )

      invisible(NULL)
    }

    getModuleData <- function() {
      moduleData$source_adapters <- AquaCache::getSourceAdapterCapabilities(
        con = session$userData$AquaCache,
        data_domain = "continuous"
      )
      moduleData$source_fx <- sort(unique(
        as.character(moduleData$source_adapters$source_fx)
      ))
      moduleData$timeseries <- DBI::dbGetQuery(
        session$userData$AquaCache,
        paste(
          "SELECT ts.timeseries_id, ts.location_id, ts.sub_location_id,",
          "ts.timezone_daily_calc, lz.z_meters AS z, ts.z_id, ts.media_id,",
          "ts.parameter_id, ts.matrix_state_id, ts.aggregation_type_id,",
          "ts.sensor_priority, ts.default_owner, ts.record_rate,",
          "ts.share_with, source.source_fx, source.source_fx_args, ts.note,",
          "ts.default_data_sharing_agreement_id",
          "FROM continuous.timeseries ts",
          "LEFT JOIN LATERAL (",
          "  SELECT source_fx, source_fx_args",
          "  FROM continuous.timeseries_source_adapters tsa",
          "  WHERE tsa.timeseries_id = ts.timeseries_id",
          "  ORDER BY COALESCE(fetch_priority, 32767),",
          "    COALESCE(synchronize_priority, 32767),",
          "    timeseries_source_adapter_id",
          "  LIMIT 1",
          ") source ON TRUE",
          "LEFT JOIN public.locations_z lz ON ts.z_id = lz.z_id"
        )
      )
      moduleData$timeseries_source_assignments <- DBI::dbGetQuery(
        session$userData$AquaCache,
        paste(
          "SELECT *",
          "FROM continuous.timeseries_source_adapters",
          "ORDER BY timeseries_id, COALESCE(fetch_priority, 32767),",
          "COALESCE(synchronize_priority, 32767),",
          "timeseries_source_adapter_id"
        )
      )
      moduleData$locations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT l.location_id, l.name, lt.type, l.latitude, l.longitude FROM public.locations l INNER JOIN public.location_types lt ON l.location_type = lt.type_id ORDER BY l.name ASC"
      )
      moduleData$sub_locations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT sub_location_id, sub_location_name, location_id FROM public.sub_locations ORDER BY sub_location_name ASC"
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
          "FROM public.media_types ORDER BY media_type ASC"
        )
      )
      moduleData$aggregation_types <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT aggregation_type_id, aggregation_type FROM continuous.aggregation_types ORDER BY aggregation_type ASC"
      )
      moduleData$correction_types <- DBI::dbGetQuery(
        session$userData$AquaCache,
        paste(
          "SELECT correction_type_id, correction_type, description, priority,",
          "value1, value1_description, value2, value2_description,",
          "timestep_window, equation",
          "FROM continuous.correction_types",
          "WHERE correction_type IN ('trim', 'offset linear')",
          "ORDER BY priority"
        )
      )
      moduleData$organizations <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT organization_id, name FROM public.organizations ORDER BY name ASC"
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
          "FROM continuous.timeseries ts",
          "INNER JOIN public.locations l ON ts.location_id = l.location_id",
          "LEFT JOIN public.sub_locations sl ON ts.sub_location_id = sl.sub_location_id",
          "LEFT JOIN public.locations_z lz ON ts.z_id = lz.z_id",
          "INNER JOIN public.parameters p ON ts.parameter_id = p.parameter_id",
          "LEFT JOIN public.matrix_states ms",
          "ON ts.matrix_state_id = ms.matrix_state_id",
          "INNER JOIN public.media_types m ON ts.media_id = m.media_id",
          "INNER JOIN continuous.aggregation_types at",
          "ON ts.aggregation_type_id = at.aggregation_type_id",
          "INNER JOIN public.organizations o ON ts.default_owner = o.organization_id"
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
          COALESCE(lmit.timeseries_count, 0) AS direct_timeseries_count,
          COALESCE(lmit.timeseries_ids, ARRAY[]::integer[])
            AS direct_timeseries_ids,
          COALESCE(sig.signal_timeseries_ids, ARRAY[]::integer[])
            AS signal_timeseries_ids,
          CASE
            WHEN COALESCE(sig.signal_row_count, 0) > 0
              THEN COALESCE(sig.signal_timeseries_ids, ARRAY[]::integer[])
            ELSE COALESCE(lmit.timeseries_ids, ARRAY[]::integer[])
          END AS associated_timeseries_ids,
          CASE
            WHEN COALESCE(sig.signal_row_count, 0) > 0 AND
              COALESCE(sig.distinct_signal_timeseries_count, 0) = 1
              THEN sig.signal_timeseries_id
            WHEN COALESCE(sig.signal_row_count, 0) = 0 AND
              COALESCE(lmit.timeseries_count, 0) = 1
              THEN lmit.single_timeseries_id
          END AS associated_timeseries_id,
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
              AS signal_timeseries_id,
            ARRAY_AGG(DISTINCT s.timeseries_id ORDER BY s.timeseries_id)
              FILTER (WHERE s.timeseries_id IS NOT NULL)
              AS signal_timeseries_ids
          FROM public.locations_metadata_instrument_connections AS c
          LEFT JOIN public.locations_metadata_instrument_connection_signals AS s
            ON s.connection_id = c.connection_id
          WHERE c.instrument_metadata_id = lmi.metadata_id
            AND c.start_datetime <= NOW()
            AND (c.end_datetime IS NULL OR c.end_datetime > NOW())
        ) AS sig ON TRUE
        LEFT JOIN LATERAL (
          SELECT
            COUNT(*)::integer AS timeseries_count,
            MIN(lmit.timeseries_id) AS single_timeseries_id,
            ARRAY_AGG(lmit.timeseries_id ORDER BY lmit.timeseries_id)
              AS timeseries_ids
          FROM public.locations_metadata_instrument_timeseries AS lmit
          WHERE lmit.metadata_id = lmi.metadata_id
        ) AS lmit ON TRUE
        INNER JOIN instruments.instruments AS i
          ON lmi.instrument_id = i.instrument_id
        LEFT JOIN instruments.instrument_makes AS mk
          ON i.make = mk.make_id
        LEFT JOIN instruments.instrument_models AS mdl
          ON i.model = mdl.model_id
        LEFT JOIN instruments.instrument_types AS it
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

    current_adapter_capability <- reactive({
      source_fx <- nullable_text(input$source_fx)
      if (
        is.na(source_fx) ||
          is.null(moduleData$source_adapters) ||
          nrow(moduleData$source_adapters) == 0L
      ) {
        return(NULL)
      }

      timeseries_source_adapter_capability(
        moduleData$source_adapters,
        source_fx
      )
    })

    current_stored_source_args <- reactive({
      source_fx <- nullable_text(input$source_fx)
      if (
        !is.na(source_fx) &&
          identical(source_fx, source_args_existing_source())
      ) {
        source_args_existing()
      } else {
        NA_character_
      }
    })

    output$source_fx_args_ui <- renderUI({
      capability <- current_adapter_capability()
      if (is.null(capability)) {
        return(tags$div(
          class = "alert alert-secondary",
          "Select a source function to see its arguments."
        ))
      }
      source_adapter_argument_ui(
        ns,
        capability,
        current_stored_source_args()
      )
    })

    collect_source_fx_args <- function() {
      source_adapter_args_json(source_adapter_collect_args(
        input,
        current_adapter_capability(),
        current_stored_source_args()
      ))
    }

    secondary_adapter_capability <- reactive({
      if (!isTRUE(input$source_secondary_enabled)) {
        return(NULL)
      }
      source_fx <- nullable_text(input$source_fx_secondary)
      if (
        is.na(source_fx) ||
          is.null(moduleData$source_adapters) ||
          nrow(moduleData$source_adapters) == 0L
      ) {
        return(NULL)
      }
      timeseries_source_adapter_capability(
        moduleData$source_adapters,
        source_fx
      )
    })

    secondary_stored_source_args <- reactive({
      source_fx <- nullable_text(input$source_fx_secondary)
      if (
        !is.na(source_fx) &&
          identical(source_fx, source_args_secondary_existing_source())
      ) {
        source_args_secondary_existing()
      } else {
        NA_character_
      }
    })

    output$source_fx_secondary_args_ui <- renderUI({
      capability <- secondary_adapter_capability()
      if (is.null(capability)) {
        return(tags$div(
          class = "alert alert-secondary",
          "Select a secondary source function to see its arguments."
        ))
      }
      source_adapter_argument_ui(
        ns,
        capability,
        secondary_stored_source_args(),
        input_prefix = "secondary_"
      )
    })

    collect_secondary_source_fx_args <- function() {
      source_adapter_args_json(source_adapter_collect_args(
        input,
        secondary_adapter_capability(),
        secondary_stored_source_args(),
        input_prefix = "secondary_"
      ))
    }

    collect_source_assignments <- function() {
      if (isTRUE(input$source_secondary_enabled)) {
        if (is.na(nullable_text(input$source_fx))) {
          stop(
            "Select a primary source before configuring a secondary source."
          )
        }
        if (is.na(nullable_text(input$source_fx_secondary))) {
          stop(
            "Select a secondary source function or turn off the secondary ",
            "source adapter."
          )
        }
      }
      rows <- list()
      add_assignment <- function(
        source_fx,
        args,
        fetch_enabled,
        fetch_priority,
        synchronize_enabled,
        synchronize_priority,
        active,
        requires_transmission_mapping = FALSE,
        transmission_route_id = NULL
      ) {
        source_fx <- nullable_text(source_fx)
        if (is.na(source_fx)) {
          return(NULL)
        }
        if (!isTRUE(fetch_enabled) && !isTRUE(synchronize_enabled)) {
          stop(
            "Each configured source must be enabled for fetching, ",
            "synchronization, or both."
          )
        }
        if (isTRUE(requires_transmission_mapping)) {
          args <- timeseries_source_args_with_transmission_route(
            args,
            transmission_route_id
          )
        }
        data.frame(
          source_fx = source_fx,
          source_fx_args = args,
          fetch_priority = if (isTRUE(fetch_enabled)) {
            as.integer(fetch_priority)
          } else {
            NA_integer_
          },
          synchronize_priority = if (isTRUE(synchronize_enabled)) {
            as.integer(synchronize_priority)
          } else {
            NA_integer_
          },
          active = isTRUE(active),
          stringsAsFactors = FALSE
        )
      }
      rows[[1L]] <- add_assignment(
        input$source_fx,
        collect_source_fx_args(),
        input$source_fetch_enabled,
        input$source_fetch_priority,
        input$source_sync_enabled,
        input$source_sync_priority,
        input$source_active,
        requires_transmission_mapping = !is.null(
          current_adapter_capability()
        ) &&
          isTRUE(
            current_adapter_capability()$requires_transmission_mapping[[1L]]
          ),
        transmission_route_id = input$transmission_route
      )
      rows[[2L]] <- if (isTRUE(input$source_secondary_enabled)) {
        add_assignment(
          input$source_fx_secondary,
          collect_secondary_source_fx_args(),
          input$source_secondary_fetch_enabled,
          input$source_secondary_fetch_priority,
          input$source_secondary_sync_enabled,
          input$source_secondary_sync_priority,
          input$source_secondary_active,
          requires_transmission_mapping = !is.null(
            secondary_adapter_capability()
          ) &&
            isTRUE(
              secondary_adapter_capability()$requires_transmission_mapping[[1L]]
            ),
          transmission_route_id = input$secondary_transmission_route
        )
      } else {
        NULL
      }
      rows <- Filter(Negate(is.null), rows)
      if (!length(rows)) {
        return(data.frame())
      }
      assignments <- do.call(rbind, rows)
      active_assignments <- assignments[assignments$active, , drop = FALSE]
      for (column in c("fetch_priority", "synchronize_priority")) {
        priority <- active_assignments[[column]]
        priority <- priority[!is.na(priority)]
        if (anyDuplicated(priority)) {
          stop("Active source assignments cannot repeat ", column, ".")
        }
      }
      assignments
    }

    transmission_adapter <- reactive({
      capability <- current_adapter_capability()
      if (
        is.null(capability) ||
          !isTRUE(capability$requires_transmission_mapping[[1L]])
      ) {
        return(NULL)
      }
      capability
    })

    secondary_transmission_adapter <- reactive({
      capability <- secondary_adapter_capability()
      if (
        is.null(capability) ||
          !isTRUE(capability$requires_transmission_mapping[[1L]])
      ) {
        return(NULL)
      }
      capability
    })

    uses_transmission_mapping <- reactive({
      !is.null(transmission_adapter())
    })

    secondary_uses_transmission_mapping <- reactive({
      !is.null(secondary_transmission_adapter())
    })

    adapter_ui_default <- function(
      name,
      default = "",
      capability = transmission_adapter()
    ) {
      if (is.null(capability)) {
        return(default)
      }
      ui_config <- capability$ui_config[[1]]
      value <- ui_config[[name]]
      if (is.null(value) || !length(value) || is.na(value[[1]])) {
        return(default)
      }
      as.character(value[[1]])
    }

    transmission_choices_for <- function(capability) {
      transmission_choices_version()
      if (is.null(capability)) {
        return(list(
          routes = data.frame(),
          setups = data.frame(),
          loggers = data.frame(),
          methods = data.frame()
        ))
      }

      location_id <- nullable_integer(input$location)
      source_fx <- if (is.null(capability)) {
        NA_character_
      } else {
        as.character(capability$source_fx[[1L]])
      }
      if (is.na(location_id) || is.na(source_fx)) {
        return(list(
          routes = data.frame(),
          setups = data.frame(),
          loggers = data.frame(),
          methods = data.frame()
        ))
      }

      timeseries_transmission_choices(
        con = session$userData$AquaCache,
        location_id = location_id,
        source_fx = source_fx
      )
    }

    transmission_choices <- reactive({
      transmission_choices_for(transmission_adapter())
    })

    secondary_transmission_choices <- reactive({
      transmission_choices_for(secondary_transmission_adapter())
    })

    transmission_mapping_input <- function(input_prefix = "") {
      timeseries_normalize_transmission_mapping(
        route_id = input[[paste0(input_prefix, "transmission_route")]],
        source_field = input[[paste0(
          input_prefix,
          "transmission_source_field"
        )]],
        value_multiplier = input[[paste0(
          input_prefix,
          "transmission_value_multiplier"
        )]],
        value_offset = input[[paste0(
          input_prefix,
          "transmission_value_offset"
        )]],
        missing_values = input[[paste0(
          input_prefix,
          "transmission_missing_values"
        )]],
        mapping_config = input[[paste0(
          input_prefix,
          "transmission_mapping_config"
        )]]
      )
    }

    current_transmission_mappings_input <- function() {
      mappings <- list()
      if (uses_transmission_mapping()) {
        mappings[[length(mappings) + 1L]] <- transmission_mapping_input()
      }
      if (secondary_uses_transmission_mapping()) {
        mappings[[length(mappings) + 1L]] <- transmission_mapping_input(
          "secondary_"
        )
      }
      if (length(mappings) > 1L) {
        route_ids <- vapply(
          mappings,
          function(x) as.integer(x$transmission_route_id),
          integer(1)
        )
        if (anyDuplicated(route_ids)) {
          stop(
            "Primary and secondary transmission sources must use different ",
            "routes."
          )
        }
      }
      mappings
    }

    same_transmission_mapping_row <- function(mapping, existing) {
      existing_missing <- tryCatch(
        as.character(jsonlite::fromJSON(existing$missing_values[[1]])),
        error = function(e) character()
      )
      input_missing <- tryCatch(
        as.character(jsonlite::fromJSON(mapping$missing_values)),
        error = function(e) character()
      )
      existing_config <- tryCatch(
        jsonlite::fromJSON(
          existing$mapping_config[[1]],
          simplifyVector = FALSE
        ),
        error = function(e) NULL
      )
      input_config <- tryCatch(
        jsonlite::fromJSON(mapping$mapping_config, simplifyVector = FALSE),
        error = function(e) NULL
      )

      same_nullable_integer(
        mapping$transmission_route_id,
        existing$transmission_route_id[[1]]
      ) &&
        identical(
          mapping$source_field,
          trimws(existing$source_field[[1]])
        ) &&
        same_nullable_numeric(
          mapping$value_multiplier,
          existing$value_multiplier[[1]]
        ) &&
        same_nullable_numeric(
          mapping$value_offset,
          existing$value_offset[[1]]
        ) &&
        identical(existing_missing, input_missing) &&
        identical(existing_config, input_config) &&
        isTRUE(existing$enabled[[1]])
    }

    same_transmission_mappings <- function(mappings, existing_rows) {
      if (is.null(existing_rows)) {
        existing_rows <- data.frame()
      }
      if (length(mappings) != nrow(existing_rows)) {
        return(FALSE)
      }
      if (!length(mappings)) {
        return(TRUE)
      }

      existing_route_ids <- as.integer(existing_rows$transmission_route_id)
      all(vapply(
        mappings,
        function(mapping) {
          row_index <- match(
            as.integer(mapping$transmission_route_id),
            existing_route_ids
          )
          !is.na(row_index) &&
            same_transmission_mapping_row(
              mapping,
              existing_rows[row_index, , drop = FALSE]
            )
        },
        logical(1)
      ))
    }

    correction_type_row <- function(correction_type) {
      if (
        is.null(moduleData$correction_types) ||
          nrow(moduleData$correction_types) == 0
      ) {
        return(data.frame())
      }

      moduleData$correction_types[
        moduleData$correction_types$correction_type == correction_type,
        ,
        drop = FALSE
      ]
    }

    correction_type_id <- function(correction_type) {
      row <- correction_type_row(correction_type)
      if (nrow(row) != 1) {
        return(NA_integer_)
      }

      as.integer(row$correction_type_id[[1]])
    }

    correction_description <- function(correction_type, fallback) {
      row <- correction_type_row(correction_type)
      if (
        nrow(row) != 1 ||
          is.na(row$description[[1]]) ||
          !nzchar(row$description[[1]])
      ) {
        return(fallback)
      }

      row$description[[1]]
    }

    default_correction_bounds <- function() {
      list(
        start_dt = as.POSIXct("1800-01-01 00:00:00", tz = "UTC"),
        end_dt = as.POSIXct("2100-01-01 00:00:00", tz = "UTC")
      )
    }

    empty_default_corrections <- function() {
      data.frame(
        correction_type = integer(0),
        correction_type_name = character(0),
        start_dt = as.POSIXct(character(0), tz = "UTC"),
        end_dt = as.POSIXct(character(0), tz = "UTC"),
        value1 = numeric(0),
        value2 = numeric(0),
        stringsAsFactors = FALSE
      )
    }

    build_default_corrections <- function() {
      bounds <- default_correction_bounds()
      corrections <- empty_default_corrections()

      if (isTRUE(input$add_trim_correction)) {
        trim_id <- correction_type_id("trim")
        trim_min <- nullable_numeric(input$trim_value_min)
        trim_max <- nullable_numeric(input$trim_value_max)

        if (is.na(trim_id)) {
          stop("Correction type 'trim' is not available in the database.")
        }
        if (is.na(trim_min)) {
          stop("Enter a lower trim bound before adding a trim correction.")
        }
        if (!is.na(trim_max) && trim_max <= trim_min) {
          stop("The upper trim bound must be greater than the lower bound.")
        }

        corrections <- rbind(
          corrections,
          data.frame(
            correction_type = trim_id,
            correction_type_name = "trim",
            start_dt = bounds$start_dt,
            end_dt = bounds$end_dt,
            value1 = trim_min,
            value2 = trim_max,
            stringsAsFactors = FALSE
          )
        )
      }

      if (isTRUE(input$add_offset_linear_correction)) {
        offset_id <- correction_type_id("offset linear")
        offset_value <- nullable_numeric(input$offset_linear_value)

        if (is.na(offset_id)) {
          stop(
            "Correction type 'offset linear' is not available in the database."
          )
        }
        if (is.na(offset_value)) {
          stop(
            "Enter an offset value before adding an offset linear correction."
          )
        }

        corrections <- rbind(
          corrections,
          data.frame(
            correction_type = offset_id,
            correction_type_name = "offset linear",
            start_dt = bounds$start_dt,
            end_dt = bounds$end_dt,
            value1 = offset_value,
            value2 = NA_real_,
            stringsAsFactors = FALSE
          )
        )
      }

      corrections
    }

    default_corrections_display <- function(corrections) {
      if (is.null(corrections) || nrow(corrections) == 0) {
        return(data.frame(
          message = "No default corrections selected.",
          stringsAsFactors = FALSE
        ))
      }

      out <- corrections
      names(out) <- c(
        "Correction type ID",
        "Correction type",
        "Start datetime",
        "End datetime",
        "Value 1",
        "Value 2"
      )
      out[["Start datetime"]] <- format(
        out[["Start datetime"]],
        "%Y-%m-%d %H:%M:%S %Z",
        tz = "UTC"
      )
      out[["End datetime"]] <- format(
        out[["End datetime"]],
        "%Y-%m-%d %H:%M:%S %Z",
        tz = "UTC"
      )
      out
    }

    existing_corrections <- reactive({
      tsid <- nullable_integer(selected_tsid())
      if (is.na(tsid)) {
        return(data.frame())
      }

      DBI::dbGetQuery(
        session$userData$AquaCache,
        paste(
          "SELECT c.correction_id, c.start_dt, c.end_dt,",
          "ct.priority, ct.correction_type, c.value1, c.value2,",
          "c.timestep_window, c.equation",
          "FROM continuous.corrections c",
          "LEFT JOIN continuous.correction_types ct",
          "ON ct.correction_type_id = c.correction_type",
          "WHERE c.timeseries_id = $1",
          "ORDER BY c.start_dt, ct.priority, c.correction_id"
        ),
        params = list(tsid)
      )
    })

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
        moduleData$correction_types,
        moduleData$source_adapters,
        orgs,
        moduleData$agreements
      )
      trim_description <- correction_description(
        "trim",
        "Remove data points outside of a specified value range."
      )
      offset_description <- correction_description(
        "offset linear",
        "Apply a linear offset correction."
      )
      bounds <- default_correction_bounds()
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
            createFilter = "^-?(?:[0-9]+|[0-9]*[.][0-9]+)$",
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
                  "Primary source function",
                  choices = moduleData$source_fx,
                  multiple = TRUE,
                  options = list(
                    maxItems = 1,
                    placeholder = 'Select source function (optional)'
                  ),
                  width = "100%"
                ),
                checkboxInput(
                  ns("source_active"),
                  "Assignment active",
                  value = TRUE
                ),
                fluidRow(
                  column(
                    6,
                    checkboxInput(
                      ns("source_fetch_enabled"),
                      "Use for fetching",
                      value = TRUE
                    ),
                    numericInput(
                      ns("source_fetch_priority"),
                      "Fetch priority",
                      value = 1,
                      min = 1,
                      step = 1
                    )
                  ),
                  column(
                    6,
                    checkboxInput(
                      ns("source_sync_enabled"),
                      "Use for synchronization",
                      value = TRUE
                    ),
                    numericInput(
                      ns("source_sync_priority"),
                      "Synchronization priority",
                      value = 1,
                      min = 1,
                      step = 1
                    )
                  )
                ),
                tags$p(
                  class = "text-muted small",
                  "Missing download function? Download functions must be ",
                  "registered in the database's ",
                  tags$code("public.source_adapter_capabilities"),
                  " table to show up here. Developers: see AquaCache::registerSourceAdapterArguments()."
                ),
                actionButton(
                  ns("source_fx_doc"),
                  "Open function documentation"
                )
              ),
              verticalLayout(
                tags$div(
                  class = "alert alert-info",
                  "Enter the catalogued source arguments below. AquaCache supplies the read-only managed arguments automatically. Transmission route and field settings, if applicable, remain separate."
                ),
                uiOutput(ns("source_fx_args_ui")),
                actionButton(
                  ns("args_example"),
                  "Show example arguments"
                )
              )
            ),
            uiOutput(ns("transmission_mapping_ui")),
            tags$hr(),
            checkboxInput(
              ns("source_secondary_enabled"),
              "Configure a secondary source adapter",
              value = FALSE
            ),
            conditionalPanel(
              condition = "input.source_secondary_enabled",
              ns = ns,
              tags$h5("Secondary source adapter"),
              splitLayout(
                cellWidths = c("50%", "50%"),
                verticalLayout(
                  selectizeInput(
                    ns("source_fx_secondary"),
                    "Secondary source function",
                    choices = moduleData$source_fx,
                    multiple = TRUE,
                    options = list(
                      maxItems = 1,
                      placeholder = "Select secondary source"
                    ),
                    width = "100%"
                  ),
                  checkboxInput(
                    ns("source_secondary_active"),
                    "Assignment active",
                    value = TRUE
                  ),
                  fluidRow(
                    column(
                      6,
                      checkboxInput(
                        ns("source_secondary_fetch_enabled"),
                        "Use for fetching",
                        value = FALSE
                      ),
                      numericInput(
                        ns("source_secondary_fetch_priority"),
                        "Fetch priority",
                        value = 2,
                        min = 1,
                        step = 1
                      )
                    ),
                    column(
                      6,
                      checkboxInput(
                        ns("source_secondary_sync_enabled"),
                        "Use for synchronization",
                        value = TRUE
                      ),
                      numericInput(
                        ns("source_secondary_sync_priority"),
                        "Synchronization priority",
                        value = 1,
                        min = 1,
                        step = 1
                      )
                    )
                  )
                ),
                verticalLayout(
                  tags$div(
                    class = "alert alert-info",
                    "Secondary arguments are stored independently from the primary adapter."
                  ),
                  uiOutput(ns("source_fx_secondary_args_ui"))
                )
              ),
              uiOutput(ns("secondary_transmission_mapping_ui"))
            )
          ),
          accordion_panel(
            id = ns("corrections_panel"),
            title = "Automatic corrections/filters",
            tags$p(
              class = "text-muted",
              paste(
                "Default corrections are inserted when creating a new",
                "timeseries. Existing corrections are shown when modifying",
                "a timeseries so you can avoid adding duplicates elsewhere."
              )
            ),
            conditionalPanel(
              condition = "input.mode == 'add'",
              ns = ns,
              tags$p(
                class = "text-muted small",
                paste(
                  "New default corrections use broad datetime bounds:",
                  format(bounds$start_dt, "%Y-%m-%d %H:%M:%S %Z", tz = "UTC"),
                  "to",
                  format(bounds$end_dt, "%Y-%m-%d %H:%M:%S %Z", tz = "UTC")
                )
              ),
              checkboxInput(
                ns("add_trim_correction"),
                "Add trim correction",
                value = FALSE
              ),
              conditionalPanel(
                condition = "input.add_trim_correction",
                ns = ns,
                tags$p(class = "text-muted small", trim_description),
                splitLayout(
                  cellWidths = c("50%", "50%"),
                  numericInput(
                    ns("trim_value_min"),
                    "Lower bound",
                    value = NA,
                    step = "any",
                    width = "100%"
                  ),
                  numericInput(
                    ns("trim_value_max"),
                    "Upper bound (optional)",
                    value = NA,
                    step = "any",
                    width = "100%"
                  )
                )
              ),
              checkboxInput(
                ns("add_offset_linear_correction"),
                "Add offset linear correction",
                value = FALSE
              ),
              conditionalPanel(
                condition = "input.add_offset_linear_correction",
                ns = ns,
                tags$p(class = "text-muted small", offset_description),
                numericInput(
                  ns("offset_linear_value"),
                  "Shift for linear offset",
                  value = NA,
                  step = "any",
                  width = "100%"
                )
              ),
              uiOutput(ns("default_corrections_warning")),
              DT::DTOutput(ns("default_corrections_preview"))
            ),
            conditionalPanel(
              condition = "input.mode == 'modify'",
              ns = ns,
              uiOutput(ns("existing_corrections_message")),
              DT::DTOutput(ns("existing_corrections_table"))
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

    transmission_mapping_ui <- function(
      role = c("primary", "secondary")
    ) {
      role <- match.arg(role)
      secondary <- identical(role, "secondary")
      uses_mapping <- if (secondary) {
        secondary_uses_transmission_mapping()
      } else {
        uses_transmission_mapping()
      }
      if (!uses_mapping) {
        return(NULL)
      }

      input_prefix <- if (secondary) "secondary_" else ""
      capability <- if (secondary) {
        secondary_transmission_adapter()
      } else {
        transmission_adapter()
      }

      location_id <- nullable_integer(input$location)
      mapping_rows <- pending_transmission_mapping()
      selected_route <- if (secondary) {
        preferred_secondary_transmission_route_id()
      } else {
        preferred_transmission_route_id()
      }
      if (is.null(selected_route)) {
        stored_route <- source_args_transmission_route_id(
          if (secondary) {
            secondary_stored_source_args()
          } else {
            current_stored_source_args()
          }
        )
        if (!is.na(stored_route)) {
          selected_route <- stored_route
        }
      }
      mapping <- NULL
      if (!is.null(mapping_rows) && nrow(mapping_rows) > 0L) {
        row_index <- if (!is.null(selected_route)) {
          match(
            as.integer(selected_route),
            as.integer(mapping_rows$transmission_route_id)
          )
        } else if (
          secondary &&
            uses_transmission_mapping() &&
            !is.null(preferred_transmission_route_id())
        ) {
          candidates <- which(
            as.integer(mapping_rows$transmission_route_id) !=
              as.integer(preferred_transmission_route_id())
          )
          if (length(candidates)) candidates[[1L]] else NA_integer_
        } else {
          1L
        }
        if (!is.na(row_index)) {
          mapping <- mapping_rows[row_index, , drop = FALSE]
        }
      }

      if (is.null(selected_route) && !is.null(mapping)) {
        if (
          is.na(location_id) ||
            identical(
              as.integer(mapping$route_location_id[[1]]),
              location_id
            )
        ) {
          selected_route <- mapping$transmission_route_id[[1]]
        }
      }

      choices <- if (secondary) {
        secondary_transmission_choices()
      } else {
        transmission_choices()
      }
      route_values <- choices$routes$transmission_route_id
      route_labels <- choices$routes$label
      if (
        !is.null(selected_route) &&
          length(selected_route) == 1L &&
          !is.na(selected_route) &&
          !selected_route %in% route_values
      ) {
        route_values <- c(route_values, selected_route)
        route_labels <- c(
          route_labels,
          if (!is.null(mapping)) {
            paste0(
              mapping$route_label[[1]],
              " (not currently effective)"
            )
          } else {
            paste0(
              "Current route #",
              selected_route,
              " (not currently effective)"
            )
          }
        )
      }

      missing_values <- ""
      mapping_config <- "{}"
      source_field <- ""
      value_multiplier <- 1
      value_offset <- 0
      if (!is.null(mapping)) {
        source_field <- safe_text(mapping$source_field[[1]])
        value_multiplier <- as.numeric(mapping$value_multiplier[[1]])
        value_offset <- as.numeric(mapping$value_offset[[1]])
        mapping_config <- safe_text(mapping$mapping_config[[1]])
        parsed_missing <- tryCatch(
          jsonlite::fromJSON(mapping$missing_values[[1]]),
          error = function(e) character()
        )
        missing_values <- paste(parsed_missing, collapse = ", ")
      }

      tagList(
        tags$hr(),
        tags$h5(paste(
          if (secondary) "Secondary" else "Primary",
          "transmission route and field mapping"
        )),
        tags$div(
          class = "alert alert-info",
          paste(
            "One transmission route can feed many timeseries. This form",
            "adds only the field mapping for the current timeseries; route",
            "and provider settings remain shared."
          )
        ),
        if (is.na(location_id)) {
          tags$div(
            class = "alert alert-warning",
            "Select a location before choosing or creating a transmission route."
          )
        },
        if (
          !is.na(location_id) &&
            nrow(choices$routes) == 0L
        ) {
          tags$div(
            class = "alert alert-warning",
            paste(
              "No currently effective compatible route exists at this",
              "location. Create one below."
            )
          )
        },
        if (
          !is.null(mapping) &&
            !is.na(location_id) &&
            !identical(
              as.integer(mapping$route_location_id[[1]]),
              location_id
            )
        ) {
          tags$div(
            class = "alert alert-warning",
            paste(
              "The existing mapping belongs to another location.",
              "Select a compatible route before saving."
            )
          )
        },
        selectizeInput(
          ns(paste0(input_prefix, "transmission_route")),
          "Transmission route",
          choices = stats::setNames(route_values, route_labels),
          selected = selected_route,
          multiple = TRUE,
          options = list(
            maxItems = 1,
            placeholder = "Select a compatible route",
            plugins = list("clear_button")
          ),
          width = "100%"
        ),
        actionButton(
          ns(paste0(input_prefix, "new_transmission_route")),
          "Create transmission route",
          icon = icon("plus"),
          disabled = is.na(location_id)
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          textInput(
            ns(paste0(input_prefix, "transmission_source_field")),
            adapter_ui_default(
              "source_field_label",
              "Provider field",
              capability = capability
            ),
            value = source_field,
            placeholder = "Exact payload field name",
            width = "100%"
          ),
          textInput(
            ns(paste0(input_prefix, "transmission_missing_values")),
            "Missing-value codes",
            value = missing_values,
            placeholder = "-9999, MISSING",
            width = "100%"
          )
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          numericInput(
            ns(paste0(input_prefix, "transmission_value_multiplier")),
            "Value multiplier",
            value = value_multiplier,
            step = "any",
            width = "100%"
          ),
          numericInput(
            ns(paste0(input_prefix, "transmission_value_offset")),
            "Value offset",
            value = value_offset,
            step = "any",
            width = "100%"
          )
        ),
        textAreaInput(
          ns(paste0(input_prefix, "transmission_mapping_config")),
          "Advanced mapping configuration (JSON object)",
          value = mapping_config,
          rows = 3,
          width = "100%"
        )
      )
    }

    output$transmission_mapping_ui <- renderUI({
      transmission_mapping_ui("primary")
    })

    output$secondary_transmission_mapping_ui <- renderUI({
      transmission_mapping_ui("secondary")
    })

    observeEvent(
      list(input$location, input$source_fx),
      {
        preferred_transmission_route_id(NULL)
        if (identical(input$mode, "add")) {
          pending_transmission_mapping(NULL)
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      list(
        input$location,
        input$source_secondary_enabled,
        input$source_fx_secondary
      ),
      {
        preferred_secondary_transmission_route_id(NULL)
        if (identical(input$mode, "add")) {
          pending_transmission_mapping(NULL)
        }
      },
      ignoreInit = TRUE
    )

    show_transmission_route_modal <- function(
      role = c("primary", "secondary")
    ) {
      role <- match.arg(role)
      secondary <- identical(role, "secondary")
      capability <- if (secondary) {
        secondary_transmission_adapter()
      } else {
        transmission_adapter()
      }
      req(!is.null(capability))
      route_creation_target(role)
      choices <- if (secondary) {
        secondary_transmission_choices()
      } else {
        transmission_choices()
      }
      location_id <- nullable_integer(input$location)
      req(!is.na(location_id))

      setup_choices <- c(
        "Create a new transmission setup" = "__new__",
        stats::setNames(
          choices$setups$transmission_setup_id,
          choices$setups$label
        )
      )
      logger_choices <- c(
        "No deployed logger recorded" = "",
        stats::setNames(
          choices$loggers$metadata_id,
          choices$loggers$label
        )
      )
      method_choices <- stats::setNames(
        choices$methods$transmission_method_id,
        choices$methods$method_name
      )
      setup_start <- format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC")

      showModal(modalDialog(
        title = "Create transmission route",
        size = "l",
        easyClose = FALSE,
        selectizeInput(
          ns("new_route_setup"),
          "Transmission setup",
          choices = setup_choices,
          selected = if (nrow(choices$setups) > 0L) {
            choices$setups$transmission_setup_id[[1]]
          } else {
            "__new__"
          },
          width = "100%"
        ),
        conditionalPanel(
          condition = "input.new_route_setup == '__new__'",
          ns = ns,
          tags$div(
            class = "alert alert-info",
            paste(
              "A setup describes the deployed logger, provider, and platform.",
              "The route below describes one delivery schedule or endpoint."
            )
          ),
          splitLayout(
            cellWidths = c("50%", "50%"),
            selectizeInput(
              ns("new_route_logger"),
              "Deployed logger (optional)",
              choices = logger_choices,
              selected = "",
              multiple = TRUE,
              options = list(
                maxItems = 1,
                placeholder = "Optional: select a logger"
              ),
              width = "100%"
            ) |>
              tooltip(
                "Optional logger deployment that originates this transmission. The route remains attached to the selected location when no logger is recorded."
              ),
            selectizeInput(
              ns("new_route_method"),
              "Transmission method",
              choices = method_choices,
              multiple = TRUE,
              options = list(
                maxItems = 1,
                placeholder = "Select a method"
              ),
              width = "100%"
            )
          ),
          splitLayout(
            cellWidths = c("50%", "50%"),
            textInput(
              ns("new_route_provider"),
              "Provider",
              value = adapter_ui_default(
                "provider_name",
                "",
                capability = capability
              )
            ),
            textInput(
              ns("new_route_platform"),
              "Platform identifier",
              value = "",
              placeholder = "DCP address, IMEI, terminal ID, etc."
            ) |>
              tooltip(
                "Transmission platform identifier. For GOES DCS, enter the eight-character DCP address."
              )
          ),
          textInput(
            ns("new_route_setup_start"),
            "Setup start datetime (UTC)",
            value = setup_start,
            placeholder = "YYYY-MM-DD HH:MM:SS"
          ),
          textAreaInput(
            ns("new_route_transmission_config"),
            "Advanced setup configuration (JSON object)",
            value = "{}",
            rows = 2
          ) |>
            tooltip(
              "Provider-wide JSON settings shared by every route in this transmission setup. Usually left as {} for GOES."
            )
        ),
        tags$hr(),
        splitLayout(
          cellWidths = c("50%", "50%"),
          textInput(
            ns("new_route_name"),
            "Route name",
            value = paste(
              if (secondary) "Secondary" else "Primary",
              "transmission route"
            )
          ),
          textInput(
            ns("new_route_endpoint"),
            "Endpoint or route identifier",
            value = ""
          )
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          textInput(
            ns("new_route_message_format"),
            "Message format",
            value = "",
            placeholder = "JSON, CSV, SHEF, custom, etc."
          ),
          textInput(
            ns("new_route_schedule_reference"),
            "Schedule reference time (UTC, if known)",
            value = "",
            placeholder = "HH:MM:SS (optional)"
          )
        ),
        splitLayout(
          cellWidths = c("34%", "33%", "33%"),
          numericInput(
            ns("new_route_interval"),
            "Transmit interval, seconds",
            value = NA,
            min = 1
          ),
          numericInput(
            ns("new_route_window"),
            "Transmit window, seconds (if known)",
            value = NA,
            min = 0
          ),
          numericInput(
            ns("new_route_payload_size"),
            "Payload size, bytes (if known)",
            value = NA,
            min = 1
          )
        ),
        textAreaInput(
          ns("new_route_config"),
          "Advanced route configuration (JSON object)",
          value = "{}",
          rows = 3
        ) |>
          tooltip(
            "Route-specific parser and retrieval settings. SHEF normally uses {}; max_days defaults to 14."
          ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("save_transmission_route"),
            "Create route",
            class = "btn-primary"
          )
        )
      ))
    }

    observeEvent(input$new_transmission_route, {
      show_transmission_route_modal("primary")
    })

    observeEvent(input$secondary_new_transmission_route, {
      show_transmission_route_modal("secondary")
    })

    observeEvent(input$save_transmission_route, {
      route_id <- tryCatch(
        {
          capability <- if (
            identical(
              route_creation_target(),
              "secondary"
            )
          ) {
            secondary_adapter_capability()
          } else {
            current_adapter_capability()
          }
          if (
            identical(input$new_route_setup, "__new__") &&
              !is.null(capability) &&
              identical(
                capability$parallel_group_strategy[[1]],
                "transmission_platform"
              ) &&
              is.na(nullable_text(input$new_route_platform))
          ) {
            stop("Enter the provider platform identifier for this setup.")
          }

          timeseries_save_transmission_route(
            con = session$userData$AquaCache,
            location_id = nullable_integer(input$location),
            setup_id = if (identical(input$new_route_setup, "__new__")) {
              NA_integer_
            } else {
              nullable_integer(input$new_route_setup)
            },
            logger_metadata_id = nullable_integer(input$new_route_logger),
            transmission_method_id = nullable_integer(input$new_route_method),
            provider_name = safe_text(input$new_route_provider),
            platform_identifier = safe_text(input$new_route_platform),
            setup_start_datetime = safe_text(input$new_route_setup_start),
            transmission_config = safe_text(
              input$new_route_transmission_config
            ),
            route_name = safe_text(input$new_route_name),
            endpoint_identifier = safe_text(input$new_route_endpoint),
            message_format = safe_text(input$new_route_message_format),
            schedule_reference_time_utc = safe_text(
              input$new_route_schedule_reference
            ),
            transmit_interval_seconds = input$new_route_interval,
            transmit_window_seconds = input$new_route_window,
            payload_size_bytes = input$new_route_payload_size,
            route_config = safe_text(input$new_route_config)
          )
        },
        error = function(e) {
          showNotification(
            conditionMessage(e),
            type = "error",
            duration = 10
          )
          NA_integer_
        }
      )
      if (is.na(route_id)) {
        return()
      }

      target <- route_creation_target()
      if (identical(target, "secondary")) {
        preferred_secondary_transmission_route_id(route_id)
      } else {
        preferred_transmission_route_id(route_id)
      }
      transmission_choices_version(transmission_choices_version() + 1L)
      removeModal()
      session$onFlushed(
        function() {
          updateSelectizeInput(
            session,
            if (identical(target, "secondary")) {
              "secondary_transmission_route"
            } else {
              "transmission_route"
            },
            selected = route_id
          )
        },
        once = TRUE
      )
      showNotification(
        paste("Transmission route", route_id, "created."),
        type = "message"
      )
    })

    output$default_corrections_warning <- renderUI({
      tryCatch(
        {
          build_default_corrections()
          NULL
        },
        error = function(e) {
          tags$div(
            class = "text-danger small mb-2",
            conditionMessage(e)
          )
        }
      )
    })

    output$default_corrections_preview <- DT::renderDT({
      corrections <- tryCatch(
        build_default_corrections(),
        error = function(e) empty_default_corrections()
      )

      DT::datatable(
        default_corrections_display(corrections),
        rownames = FALSE,
        selection = "none",
        options = list(
          dom = "t",
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          scrollX = TRUE
        )
      )
    })

    output$existing_corrections_message <- renderUI({
      if (!identical(input$mode, "modify")) {
        return(NULL)
      }
      if (is.na(nullable_integer(selected_tsid()))) {
        return(tags$p(
          class = "text-muted small",
          "Select a timeseries to see its existing corrections."
        ))
      }
      if (nrow(existing_corrections()) == 0) {
        return(tags$p(
          class = "text-muted small",
          "No existing corrections for this timeseries."
        ))
      }

      tags$p(
        class = "text-muted small",
        paste(nrow(existing_corrections()), "existing correction(s).")
      )
    })

    output$existing_corrections_table <- DT::renderDT({
      corrections <- existing_corrections()
      hide_first_column <- TRUE
      if (nrow(corrections) == 0) {
        corrections <- data.frame(
          message = "No existing corrections to display.",
          stringsAsFactors = FALSE
        )
        hide_first_column <- FALSE
      } else {
        keep <- colSums(is.na(corrections)) < nrow(corrections)
        corrections <- corrections[, keep, drop = FALSE]
      }

      DT::datatable(
        corrections,
        rownames = FALSE,
        selection = "none",
        options = list(
          columnDefs = if (hide_first_column) {
            list(list(targets = 0, visible = FALSE))
          } else {
            list()
          },
          pageLength = 5,
          lengthChange = FALSE,
          scrollX = TRUE
        )
      )
    })

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
              "at the same location, sub-location, and elevation/depth."
            )
          ),
          tags$p(
            paste(
              "For new deployments, re-deployments, or changing instrument",
              "timeseries associations outside this form, use",
              "Equipment -> Deploy/recover instruments."
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
              "No currently deployed instruments match the current location, sub-location, and",
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
          mapping_rows <- timeseries_transmission_mapping(
            session$userData$AquaCache,
            tsid
          )
          pending_transmission_mapping(mapping_rows)
          preferred_transmission_route_id(NULL)
          preferred_secondary_transmission_route_id(NULL)

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
          assignments <- moduleData$timeseries_source_assignments[
            moduleData$timeseries_source_assignments$timeseries_id == tsid,
            ,
            drop = FALSE
          ]
          if (nrow(assignments) > 2L) {
            showNotification(
              "This timeseries has more than two source assignments. The first two are shown; remove or consolidate the additional assignments before modifying it here.",
              type = "warning",
              duration = 10
            )
          }
          primary_assignment <- if (nrow(assignments) >= 1L) {
            assignments[1L, , drop = FALSE]
          } else {
            NULL
          }
          secondary_assignment <- if (nrow(assignments) >= 2L) {
            assignments[2L, , drop = FALSE]
          } else {
            NULL
          }
          assignment_requires_mapping <- function(assignment) {
            if (is.null(assignment)) {
              return(FALSE)
            }
            capability <- timeseries_source_adapter_capability(
              moduleData$source_adapters,
              assignment$source_fx[[1L]]
            )
            !is.null(capability) &&
              isTRUE(capability$requires_transmission_mapping[[1L]])
          }
          primary_route_id <- if (is.null(primary_assignment)) {
            NA_integer_
          } else {
            source_args_transmission_route_id(
              primary_assignment$source_fx_args[[1L]]
            )
          }
          secondary_route_id <- if (is.null(secondary_assignment)) {
            NA_integer_
          } else {
            source_args_transmission_route_id(
              secondary_assignment$source_fx_args[[1L]]
            )
          }
          available_route_ids <- if (nrow(mapping_rows)) {
            as.integer(mapping_rows$transmission_route_id)
          } else {
            integer()
          }
          if (
            is.na(primary_route_id) &&
              assignment_requires_mapping(primary_assignment) &&
              length(available_route_ids)
          ) {
            primary_route_id <- available_route_ids[[1L]]
          }
          if (
            is.na(secondary_route_id) &&
              assignment_requires_mapping(secondary_assignment)
          ) {
            secondary_candidates <- setdiff(
              available_route_ids,
              primary_route_id
            )
            if (length(secondary_candidates)) {
              secondary_route_id <- secondary_candidates[[1L]]
            }
          }
          preferred_transmission_route_id(
            if (is.na(primary_route_id)) NULL else primary_route_id
          )
          preferred_secondary_transmission_route_id(
            if (is.na(secondary_route_id)) NULL else secondary_route_id
          )
          updateCheckboxInput(
            session,
            "source_secondary_enabled",
            value = !is.null(secondary_assignment)
          )
          source_args_existing(
            if (is.null(primary_assignment)) {
              NA_character_
            } else {
              primary_assignment$source_fx_args
            }
          )
          source_args_existing_source(
            if (is.null(primary_assignment)) {
              NA_character_
            } else {
              as.character(primary_assignment$source_fx)
            }
          )
          updateSelectizeInput(
            session,
            "source_fx",
            selected = if (is.null(primary_assignment)) {
              character(0)
            } else {
              primary_assignment$source_fx
            }
          )
          updateCheckboxInput(
            session,
            "source_active",
            value = is.null(primary_assignment) ||
              isTRUE(primary_assignment$active)
          )
          updateCheckboxInput(
            session,
            "source_fetch_enabled",
            value = !is.null(primary_assignment) &&
              !is.na(primary_assignment$fetch_priority)
          )
          updateNumericInput(
            session,
            "source_fetch_priority",
            value = if (
              is.null(primary_assignment) ||
                is.na(primary_assignment$fetch_priority)
            ) {
              1
            } else {
              primary_assignment$fetch_priority
            }
          )
          updateCheckboxInput(
            session,
            "source_sync_enabled",
            value = !is.null(primary_assignment) &&
              !is.na(primary_assignment$synchronize_priority)
          )
          updateNumericInput(
            session,
            "source_sync_priority",
            value = if (
              is.null(primary_assignment) ||
                is.na(primary_assignment$synchronize_priority)
            ) {
              1
            } else {
              primary_assignment$synchronize_priority
            }
          )
          source_args_secondary_existing(
            if (is.null(secondary_assignment)) {
              NA_character_
            } else {
              secondary_assignment$source_fx_args
            }
          )
          source_args_secondary_existing_source(
            if (is.null(secondary_assignment)) {
              NA_character_
            } else {
              as.character(secondary_assignment$source_fx)
            }
          )
          updateSelectizeInput(
            session,
            "source_fx_secondary",
            selected = if (is.null(secondary_assignment)) {
              character(0)
            } else {
              secondary_assignment$source_fx
            }
          )
          updateCheckboxInput(
            session,
            "source_secondary_active",
            value = is.null(secondary_assignment) ||
              isTRUE(secondary_assignment$active)
          )
          updateCheckboxInput(
            session,
            "source_secondary_fetch_enabled",
            value = !is.null(secondary_assignment) &&
              !is.na(secondary_assignment$fetch_priority)
          )
          updateNumericInput(
            session,
            "source_secondary_fetch_priority",
            value = if (
              is.null(secondary_assignment) ||
                is.na(secondary_assignment$fetch_priority)
            ) {
              2
            } else {
              secondary_assignment$fetch_priority
            }
          )
          updateCheckboxInput(
            session,
            "source_secondary_sync_enabled",
            value = !is.null(secondary_assignment) &&
              !is.na(secondary_assignment$synchronize_priority)
          )
          updateNumericInput(
            session,
            "source_secondary_sync_priority",
            value = if (
              is.null(secondary_assignment) ||
                is.na(secondary_assignment$synchronize_priority)
            ) {
              1
            } else {
              secondary_assignment$synchronize_priority
            }
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
        pending_transmission_mapping(NULL)
        preferred_transmission_route_id(NULL)
        preferred_secondary_transmission_route_id(NULL)
        updateCheckboxInput(
          session,
          "source_secondary_enabled",
          value = FALSE
        )
        source_args_existing(NA_character_)
        source_args_existing_source(NA_character_)
        source_args_secondary_existing(NA_character_)
        source_args_secondary_existing_source(NA_character_)
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
          note = if (isTruthy(input$contact_note)) {
            trimws(input$contact_note)
          } else {
            NA
          }
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
          "SELECT organization_id, name FROM public.organizations"
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
        source_assignments,
        allow_empty_initial_fetch,
        transmission_mappings,
        instrument_deployment,
        data,
        share_with,
        default_corrections
      ) {
        promises::future_promise(seed = TRUE, expr = {
          con <- NULL
          transaction_active <- FALSE
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
              transaction_active <- TRUE

              if (is.null(sub_loc)) {
                sub_loc <- NA
              } else if (nzchar(sub_loc)) {
                sub_loc <- as.numeric(sub_loc)
              } else {
                sub_loc <- NA
              }

              active_fetch <- source_assignments[
                source_assignments$active &
                  !is.na(source_assignments$fetch_priority),
                ,
                drop = FALSE
              ]
              if (nrow(active_fetch) > 0L) {
                active_fetch <- active_fetch[
                  order(active_fetch$fetch_priority),
                  ,
                  drop = FALSE
                ]
                source_fx <- active_fetch$source_fx[[1L]]
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
                "INSERT INTO continuous.timeseries (location_id, sub_location_id, timezone_daily_calc, z_id, parameter_id, media_id, matrix_state_id, sensor_priority, aggregation_type_id, record_rate, default_owner, share_with, note, end_datetime) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14) RETURNING timeseries_id;",
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
                  if (nzchar(note)) note else NA,
                  ifelse(is.na(end_datetime), NA, end_datetime)
                )
              )[1, 1]

              if (nrow(source_assignments) > 0L) {
                for (row_idx in seq_len(nrow(source_assignments))) {
                  DBI::dbExecute(
                    con,
                    "INSERT INTO continuous.timeseries_source_adapters (
                       timeseries_id, source_fx, source_fx_args,
                       fetch_priority, synchronize_priority, active
                     ) VALUES ($1, $2, $3::jsonb, $4, $5, $6)",
                    params = list(
                      new_timeseries_id,
                      source_assignments$source_fx[[row_idx]],
                      source_assignments$source_fx_args[[row_idx]],
                      source_assignments$fetch_priority[[row_idx]],
                      source_assignments$synchronize_priority[[row_idx]],
                      source_assignments$active[[row_idx]]
                    )
                  )
                }
              }

              timeseries_sync_transmission_mapping(
                con = con,
                timeseries_id = new_timeseries_id,
                mapping = transmission_mappings
              )

              update_timeseries_instrument_association(
                con = con,
                timeseries_id = new_timeseries_id,
                deployment_metadata_id = instrument_deployment
              )

              if (
                !is.null(default_corrections) &&
                  nrow(default_corrections) > 0
              ) {
                for (row_idx in seq_len(nrow(default_corrections))) {
                  DBI::dbExecute(
                    con,
                    paste(
                      "INSERT INTO continuous.corrections",
                      "(timeseries_id, start_dt, end_dt, correction_type,",
                      " value1, value2, timestep_window, equation)",
                      "VALUES ($1, $2, $3, $4, $5, $6, NULL, NULL)"
                    ),
                    params = list(
                      as.integer(new_timeseries_id),
                      default_corrections$start_dt[[row_idx]],
                      default_corrections$end_dt[[row_idx]],
                      as.integer(default_corrections$correction_type[[
                        row_idx
                      ]]),
                      as.numeric(default_corrections$value1[[row_idx]]),
                      if (is.na(default_corrections$value2[[row_idx]])) {
                        NA_real_
                      } else {
                        as.numeric(default_corrections$value2[[row_idx]])
                      }
                    )
                  )
                }
              }

              # Commit configuration before external network work. A temporary
              # provider failure must not erase a valid timeseries and mapping.
              DBI::dbCommit(con)
              transaction_active <- FALSE

              # Fetch historical data if source_fx is provided.
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
                    "UPDATE continuous.timeseries SET start_datetime = $1 WHERE timeseries_id = $2",
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
                  return(list(
                    status = "saved_no_data",
                    timeseries_id = as.integer(new_timeseries_id),
                    message = if (isTRUE(allow_empty_initial_fetch)) {
                      paste(
                        "Timeseries configuration was saved. The source had",
                        "no measurements available in the current retrieval",
                        "window."
                      )
                    } else {
                      paste(
                        "Timeseries configuration was saved, but the source",
                        "returned no measurements. Verify its arguments."
                      )
                    }
                  ))
                }
                return(list(
                  status = "saved_data",
                  timeseries_id = as.integer(new_timeseries_id),
                  message = "Timeseries and initial measurements were saved."
                ))
              } else {
                return(list(
                  status = "saved_no_source",
                  timeseries_id = as.integer(new_timeseries_id),
                  message = "Timeseries was saved without an automatic source."
                ))
              }
            },
            error = function(e) {
              if (
                isTRUE(transaction_active) &&
                  !is.null(con) &&
                  DBI::dbIsValid(con)
              ) {
                try(DBI::dbRollback(con), silent = TRUE)
              }
              if (!isTRUE(transaction_active) && !is.null(con)) {
                return(list(
                  status = "saved_fetch_failed",
                  timeseries_id = if (exists("new_timeseries_id")) {
                    as.integer(new_timeseries_id)
                  } else {
                    NA_integer_
                  },
                  message = paste(
                    "Timeseries configuration was saved, but the initial",
                    "fetch failed:",
                    conditionMessage(e)
                  )
                ))
              } else {
                return(list(
                  status = "error",
                  timeseries_id = NA_integer_,
                  message = paste(
                    "Error adding timeseries:",
                    conditionMessage(e)
                  )
                ))
              }
            },
            warning = function(w) {
              if (
                isTRUE(transaction_active) &&
                  !is.null(con) &&
                  DBI::dbIsValid(con)
              ) {
                try(DBI::dbRollback(con), silent = TRUE)
                return(list(
                  status = "error",
                  timeseries_id = NA_integer_,
                  message = paste(
                    "Failure due to warning when adding timeseries:",
                    conditionMessage(w)
                  )
                ))
              }
              list(
                status = "saved_fetch_failed",
                timeseries_id = if (exists("new_timeseries_id")) {
                  as.integer(new_timeseries_id)
                } else {
                  NA_integer_
                },
                message = paste(
                  "Timeseries configuration was saved, but the initial",
                  "fetch raised a warning:",
                  conditionMessage(w)
                )
              )
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

      selected_source_fx <- nullable_text(input$source_fx)
      if (
        !is.na(selected_source_fx) &&
          is.null(current_adapter_capability())
      ) {
        showNotification(
          paste0(
            "Source function '",
            selected_source_fx,
            "' is not enabled in the source-adapter registry."
          ),
          type = "error",
          duration = 8
        )
        return()
      }
      selected_secondary_source_fx <- if (
        isTRUE(
          input$source_secondary_enabled
        )
      ) {
        nullable_text(input$source_fx_secondary)
      } else {
        NA_character_
      }
      if (
        !is.na(selected_secondary_source_fx) &&
          is.null(secondary_adapter_capability())
      ) {
        showNotification(
          paste0(
            "Secondary source function '",
            selected_secondary_source_fx,
            "' is not enabled in the source-adapter registry."
          ),
          type = "error",
          duration = 8
        )
        return()
      }

      source_assignments <- tryCatch(
        collect_source_assignments(),
        error = function(e) {
          showNotification(e$message, type = "error", duration = 8)
          NULL
        }
      )
      if (is.null(source_assignments)) {
        return()
      }

      transmission_mappings <- tryCatch(
        current_transmission_mappings_input(),
        error = function(e) {
          showNotification(
            conditionMessage(e),
            type = "error",
            duration = 10
          )
          structure(list(), class = "transmission_mapping_error")
        }
      )
      if (inherits(transmission_mappings, "transmission_mapping_error")) {
        return()
      }

      default_corrections <- tryCatch(
        build_default_corrections(),
        error = function(e) {
          showNotification(
            conditionMessage(e),
            type = "error",
            duration = 8
          )
          NULL
        }
      )
      if (is.null(default_corrections)) {
        return()
      }

      # Call the extendedTask to add a new timeseries
      fetch_source <- if (nrow(source_assignments)) {
        fetch_rows <- source_assignments[
          source_assignments$active &
            !is.na(source_assignments$fetch_priority),
          ,
          drop = FALSE
        ]
        if (nrow(fetch_rows)) {
          fetch_rows <- fetch_rows[
            order(fetch_rows$fetch_priority),
            ,
            drop = FALSE
          ]
          fetch_rows$source_fx[[1L]]
        } else {
          NA_character_
        }
      } else {
        NA_character_
      }
      capability <- if (is.na(fetch_source)) {
        NULL
      } else {
        timeseries_source_adapter_capability(
          moduleData$source_adapters,
          fetch_source
        )
      }
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
        source_assignments = source_assignments,
        allow_empty_initial_fetch = !is.null(capability) &&
          isTRUE(capability$allow_empty_initial_fetch[[1]]),
        transmission_mappings = transmission_mappings,
        instrument_deployment = input$instrument_deployment,
        data = reactiveValuesToList(moduleData),
        share_with = input$share_with,
        default_corrections = default_corrections
      )
    })

    # Observe the result of the ExtendedTask
    observeEvent(addNewTimeseries$result(), {
      result <- addNewTimeseries$result()
      if (is.null(result)) {
        return() # No result yet, do nothing
      }
      if (
        !is.list(result) ||
          is.null(result$status) ||
          !nzchar(result$status)
      ) {
        showNotification(
          "The timeseries task returned an invalid result.",
          type = "error"
        )
        return()
      }
      if (identical(result$status, "error")) {
        showNotification(result$message, type = "error", duration = 10)
        return()
      }

      if (identical(result$status, "saved_data")) {
        showNotification(
          paste0(result$message, " Timeseries ID: ", result$timeseries_id, "."),
          type = "message",
          duration = 10
        )
      } else if (identical(result$status, "saved_no_source")) {
        showNotification(
          paste0(result$message, " Timeseries ID: ", result$timeseries_id, "."),
          type = "warning",
          duration = 10
        )
      } else {
        showNotification(
          paste0(
            result$message,
            " Timeseries ID: ",
            result$timeseries_id,
            ". The configuration remains available for retry."
          ),
          type = "warning",
          duration = 15
        )
      }

      getModuleData()
      pending_transmission_mapping(NULL)
      preferred_transmission_route_id(NULL)
      preferred_secondary_transmission_route_id(NULL)

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
      source_args_existing(NA_character_)
      source_args_existing_source(NA_character_)
      updateSelectizeInput(
        session,
        "source_fx_secondary",
        selected = character(0)
      )
      updateCheckboxInput(
        session,
        "source_secondary_enabled",
        value = FALSE
      )
      source_args_secondary_existing(NA_character_)
      source_args_secondary_existing_source(NA_character_)
      updateCheckboxInput(session, "source_active", value = TRUE)
      updateCheckboxInput(session, "source_fetch_enabled", value = TRUE)
      updateNumericInput(session, "source_fetch_priority", value = 1)
      updateCheckboxInput(session, "source_sync_enabled", value = TRUE)
      updateNumericInput(session, "source_sync_priority", value = 1)
      updateCheckboxInput(session, "source_secondary_active", value = TRUE)
      updateCheckboxInput(
        session,
        "source_secondary_fetch_enabled",
        value = FALSE
      )
      updateNumericInput(
        session,
        "source_secondary_fetch_priority",
        value = 2
      )
      updateCheckboxInput(
        session,
        "source_secondary_sync_enabled",
        value = TRUE
      )
      updateNumericInput(
        session,
        "source_secondary_sync_priority",
        value = 1
      )
      updateCheckboxInput(session, "add_trim_correction", value = FALSE)
      updateNumericInput(session, "trim_value_min", value = NA)
      updateNumericInput(session, "trim_value_max", value = NA)
      updateCheckboxInput(
        session,
        "add_offset_linear_correction",
        value = FALSE
      )
      updateNumericInput(session, "offset_linear_value", value = NA)
      updateSelectizeInput(
        session,
        "instrument_deployment",
        selected = character(0)
      )
      updateTextAreaInput(session, "note", value = "")
    })

    fetch_modified_timeseries <- function(timeseries_id) {
      config <- session$userData$config
      fetch_promise <- promises::future_promise(seed = TRUE, expr = {
        con <- AquaCache::AquaConnect(
          name = config$dbName,
          host = config$dbHost,
          port = config$dbPort,
          username = config$dbUser,
          password = config$dbPass,
          silent = TRUE
        )
        on.exit(DBI::dbDisconnect(con))
        AquaCache::getNewContinuous(
          con = con,
          timeseries_id = timeseries_id
        )
      })

      promises::then(
        fetch_promise,
        onFulfilled = function(result) {
          added <- if (
            is.data.frame(result) &&
              "rows_added" %in% names(result)
          ) {
            sum(result$rows_added, na.rm = TRUE)
          } else {
            NA_integer_
          }
          showNotification(
            if (is.na(added)) {
              paste(
                "Timeseries updated and the source fetch completed.",
                "Review synchronization history for details."
              )
            } else {
              paste(
                "Timeseries updated and source fetch completed:",
                added,
                "new measurement(s)."
              )
            },
            type = "message",
            duration = 10
          )
          invisible(result)
        },
        onRejected = function(error) {
          showNotification(
            paste(
              "Timeseries configuration was saved, but the source fetch",
              "failed:",
              conditionMessage(error)
            ),
            type = "warning",
            duration = 15
          )
          invisible(NULL)
        }
      )
      invisible(fetch_promise)
    }

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

        selected_source_fx <- nullable_text(input$source_fx)
        if (
          !is.na(selected_source_fx) &&
            is.null(current_adapter_capability())
        ) {
          showNotification(
            paste0(
              "Source function '",
              selected_source_fx,
              "' is not enabled in the source-adapter registry."
            ),
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
        existing_assignment_count <- sum(
          moduleData$timeseries_source_assignments$timeseries_id == tsid
        )
        if (existing_assignment_count > 2L) {
          showNotification(
            "This editor supports two assignments and will not overwrite a timeseries that currently has more than two.",
            type = "error",
            duration = 10
          )
          return()
        }
        selected_timeseries <- moduleData$timeseries[
          moduleData$timeseries$timeseries_id == tsid,
        ]
        # Check if the timeseries already exists
        existing_timeseries <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT * FROM continuous.timeseries WHERE timeseries_id = $1;",
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
        selected_secondary_source_fx <- if (
          isTRUE(
            input$source_secondary_enabled
          )
        ) {
          nullable_text(input$source_fx_secondary)
        } else {
          NA_character_
        }
        if (
          !is.na(selected_secondary_source_fx) &&
            is.null(secondary_adapter_capability())
        ) {
          showNotification(
            paste0(
              "Secondary source function '",
              selected_secondary_source_fx,
              "' is not enabled in the source-adapter registry."
            ),
            type = "error",
            duration = 8
          )
          return()
        }

        input_source_assignments <- tryCatch(
          collect_source_assignments(),
          error = function(e) {
            showNotification(e$message, type = "error", duration = 8)
            NULL
          }
        )
        if (is.null(input_source_assignments)) {
          return()
        }

        transmission_mappings <- tryCatch(
          current_transmission_mappings_input(),
          error = function(e) {
            showNotification(
              conditionMessage(e),
              type = "error",
              duration = 10
            )
            structure(list(), class = "transmission_mapping_error")
          }
        )
        if (inherits(transmission_mappings, "transmission_mapping_error")) {
          return()
        }
        existing_mapping_rows <- timeseries_transmission_mapping(
          session$userData$AquaCache,
          selected_timeseries$timeseries_id
        )
        existing_source_assignments <-
          moduleData$timeseries_source_assignments[
            moduleData$timeseries_source_assignments$timeseries_id ==
              selected_timeseries$timeseries_id,
            c(
              "source_fx",
              "source_fx_args",
              "fetch_priority",
              "synchronize_priority",
              "active"
            ),
            drop = FALSE
          ]
        assignment_signature <- function(x) {
          if (!nrow(x)) {
            return("[]")
          }
          x <- x[
            order(
              ifelse(is.na(x$fetch_priority), 32767, x$fetch_priority),
              ifelse(
                is.na(x$synchronize_priority),
                32767,
                x$synchronize_priority
              ),
              x$source_fx
            ),
            ,
            drop = FALSE
          ]
          as.character(jsonlite::toJSON(x, dataframe = "rows", na = "null"))
        }
        fetch_after_update <- (!identical(
          assignment_signature(input_source_assignments),
          assignment_signature(existing_source_assignments)
        ) ||
          !same_transmission_mappings(
            transmission_mappings,
            existing_mapping_rows
          ))

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
            active_fetch <- input_source_assignments[
              input_source_assignments$active &
                !is.na(input_source_assignments$fetch_priority),
              ,
              drop = FALSE
            ]
            input_source_fx <- if (nrow(active_fetch)) {
              active_fetch <- active_fetch[
                order(active_fetch$fetch_priority),
                ,
                drop = FALSE
              ]
              active_fetch$source_fx[[1L]]
            } else {
              NA_character_
            }
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
                "UPDATE continuous.timeseries SET location_id = $1 WHERE timeseries_id = $2;",
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
                  "UPDATE continuous.timeseries SET sub_location_id = NULL WHERE timeseries_id = $1;",
                  params = list(selected_timeseries_id)
                )
              } else {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "UPDATE continuous.timeseries SET sub_location_id = $1 WHERE timeseries_id = $2;",
                  params = list(
                    input_sub_location_id,
                    selected_timeseries_id
                  )
                )
              }
            }

            refresh_daily_stats <- FALSE
            if (input$tz != selected_timeseries$timezone_daily_calc) {
              DBI::dbExecute(
                session$userData$AquaCache,
                "UPDATE continuous.timeseries SET timezone_daily_calc = $1 WHERE timeseries_id = $2;",
                params = list(input$tz, selected_timeseries$timeseries_id)
              )
              refresh_daily_stats <- TRUE
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
                  "UPDATE continuous.timeseries SET z_id = $1 WHERE timeseries_id = $2",
                  params = list(target_z_id, selected_timeseries$timeseries_id)
                )
              }
            } else {
              if (!is.na(selected_timeseries$z_id)) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "UPDATE continuous.timeseries SET z_id = NULL WHERE timeseries_id = $1",
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
                  "UPDATE continuous.timeseries",
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
                "UPDATE continuous.timeseries SET aggregation_type_id = $1 WHERE timeseries_id = $2",
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
                  "UPDATE continuous.timeseries SET record_rate = $1 WHERE timeseries_id = $2",
                  params = list(
                    input_record_rate,
                    selected_timeseries_id
                  )
                )
              } else {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "UPDATE continuous.timeseries SET record_rate = NULL WHERE timeseries_id = $1",
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
                "UPDATE continuous.timeseries SET sensor_priority = $1 WHERE timeseries_id = $2",
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
                "UPDATE continuous.timeseries SET default_owner = $1 WHERE timeseries_id = $2",
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
                  "UPDATE continuous.timeseries SET default_data_sharing_agreement_id = NULL WHERE timeseries_id = $1",
                  params = list(selected_timeseries_id)
                )
              } else {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "UPDATE continuous.timeseries SET default_data_sharing_agreement_id = $1 WHERE timeseries_id = $2",
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
                  "UPDATE continuous.timeseries SET share_with = NULL WHERE timeseries_id = $1;",
                  params = list(selected_timeseries_id)
                )
              } else {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "UPDATE continuous.timeseries SET share_with = $1 WHERE timeseries_id = $2;",
                  params = list(
                    input_share_with,
                    selected_timeseries_id
                  )
                )
              }
            }

            DBI::dbExecute(
              session$userData$AquaCache,
              "DELETE FROM continuous.timeseries_source_adapters
               WHERE timeseries_id = $1",
              params = list(selected_timeseries_id)
            )
            if (nrow(input_source_assignments) > 0L) {
              for (row_idx in seq_len(nrow(input_source_assignments))) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "INSERT INTO continuous.timeseries_source_adapters (
                     timeseries_id, source_fx, source_fx_args,
                     fetch_priority, synchronize_priority, active
                   ) VALUES ($1, $2, $3::jsonb, $4, $5, $6)",
                  params = list(
                    selected_timeseries_id,
                    input_source_assignments$source_fx[[row_idx]],
                    input_source_assignments$source_fx_args[[row_idx]],
                    input_source_assignments$fetch_priority[[row_idx]],
                    input_source_assignments$synchronize_priority[[row_idx]],
                    input_source_assignments$active[[row_idx]]
                  )
                )
              }
            }

            timeseries_sync_transmission_mapping(
              con = session$userData$AquaCache,
              timeseries_id = selected_timeseries_id,
              mapping = transmission_mappings
            )

            if (!same_nullable_text(input_note, selected_timeseries$note)) {
              if (!is.na(input_note)) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "UPDATE continuous.timeseries SET note = $1 WHERE timeseries_id = $2",
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

            if (refresh_daily_stats) {
              showNotification(
                "Refreshing daily calculations from the beginning of the timeseries due to timezone change. Please be patient.",
                type = "message",
                duration = 8
              )
              DBI::dbGetQuery(
                session$userData$AquaCache,
                "SELECT continuous.refresh_calculated_daily($1::integer, NULL::date, NULL::date);",
                params = list(selected_timeseries$timeseries_id)
              )
            }

            DBI::dbCommit(session$userData$AquaCache)
            if (fetch_after_update && !is.na(input_source_fx)) {
              showNotification(
                paste(
                  "Timeseries configuration updated successfully.",
                  "Fetching from the source in the background."
                ),
                type = "message",
                duration = 8
              )
              fetch_modified_timeseries(selected_timeseries_id)
            } else {
              showNotification(
                "Timeseries updated successfully!",
                type = "message"
              )
            }
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
