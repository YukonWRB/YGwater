# Discrete sample search
#
# Filter state is independent of the widgets. Date, location, sub-location,
# media, and sample-type constraints are applied together, in that order, to an
# immutable catalogue. A selector keeps its own available choices; upstream
# changes retain compatible downstream selections. All removes a constraint,
# while an empty network/project or parameter-group intersection matches nothing.
# Date bounds include both UTC calendar days.
#
# Catalogue loading, parameter lookup, sample retrieval, result previews/counts,
# and export preparation use session-owned ExtendedTasks on AquaLink's shared
# mirai pool. Workers receive snapshots and current login credentials, open their
# own read-only transactions, and always roll back/disconnect. Request revisions
# reject results made obsolete by filter or connection changes. Parameter lookups
# coalesce rapid changes instead of queuing every intermediate selection.
#
# Downloads use the displayed table/preview snapshot. Prepare download builds the
# selected format in a worker; the download handler only serves the finished file.
# Worker functions are copied into a small environment with a package-namespace
# parent, so neither a live DBI connection nor a Shiny session is serialized.

# Helpers operate on plain snapshots, independently of a Shiny session.
# All means no constraint. An empty modal intersection means no matching IDs.
disc_data_id_predicate <- function(column, values) {
  if (identical(values, "all")) {
    return("TRUE")
  }
  ids <- unique(suppressWarnings(as.integer(values)))
  ids <- ids[!is.na(ids) & ids > 0L]
  if (!length(ids)) {
    return("FALSE")
  }
  sprintf("%s IN (%s)", column, paste(ids, collapse = ","))
}

# Recompute from the immutable catalogue, preserving compatible downstream
# selections. Each selector retains all of its own available choices.
disc_data_filter_catalogue <- function(cached, state) {
  out <- cached
  samples <- data.table::as.data.table(cached$samples)
  dates <- as.Date(state$date_range)
  if (length(dates) == 2L && !anyNA(dates)) {
    start <- as.POSIXct(dates[1], tz = "UTC")
    end <- as.POSIXct(dates[2] + 1, tz = "UTC")
    samples <- samples[datetime >= start & datetime < end]
  }
  specs <- list(
    locations = c("locs", "location_id", "location_id"),
    sub_locations = c("sub_locs", "sub_location_id", "sub_location_id"),
    media = c("media", "media_id", "media_id"),
    sample_types = c("sample_types", "sample_type_id", "sample_type")
  )
  for (id in names(specs)) {
    spec <- specs[[id]]
    choices <- cached[[spec[1]]]
    choices <- choices[
      choices[[spec[2]]] %in% samples[[spec[3]]],
      ,
      drop = FALSE
    ]
    out[[spec[1]]] <- choices
    selected <- state[[id]]
    if (!identical(selected, "all")) {
      valid <- intersect(selected, as.character(choices[[spec[2]]]))
      if (length(selected) && !length(valid)) {
        valid <- "all"
      }
      state[[id]] <- valid
      if (!identical(valid, "all")) {
        samples <- samples[samples[[spec[3]]] %in% valid]
      }
    }
  }
  out$samples <- samples
  list(data = out, state = state)
}

disc_data_run <- function(request) {
  config <- request$config
  con <- YGwater::AquaConnect(
    name = config$dbName,
    host = config$dbHost,
    port = config$dbPort,
    username = config$dbUser,
    password = config$dbPass,
    silent = TRUE
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbBegin(con)
  on.exit(DBI::dbRollback(con), add = TRUE, after = FALSE)
  DBI::dbExecute(con, "SET TRANSACTION READ ONLY")
  kind <- request$kind
  if (kind == "catalogue") {
    return(disc_data_module_data(con, env = new.env(parent = emptyenv())))
  }
  if (kind == "parameters") {
    where <- disc_data_id_predicate("s.sample_id", request$sample_ids)
    return(
      DBI::dbGetQuery(
        con,
        paste0(
          "SELECT DISTINCT r.parameter_id FROM discrete.results r
       JOIN discrete.samples s USING (sample_id) WHERE ",
          where
        )
      )$parameter_id
    )
  }
  if (kind == "samples") {
    state <- request$state
    where <- vapply(
      c("locations", "sub_locations", "media", "sample_types"),
      function(id) {
        column <- switch(
          id,
          locations = "s.location_id",
          sub_locations = "s.sub_location_id",
          media = "s.media_id",
          sample_types = "s.sample_type_id"
        )
        disc_data_id_predicate(column, state[[id]])
      },
      character(1)
    )
    dates <- as.Date(state$date_range)
    if (length(dates) == 2L && !anyNA(dates)) {
      where <- c(
        where,
        sprintf(
          "s.datetime >= %s::date AND s.datetime < %s::date",
          DBI::dbQuoteLiteral(con, as.character(dates[1])),
          DBI::dbQuoteLiteral(con, as.character(dates[2] + 1))
        )
      )
    }
    where <- paste(where, collapse = " AND ")
    parameter_where <- disc_data_id_predicate(
      "r.parameter_id",
      request$parameter_ids
    )
    samples <- dbGetQueryDT(
      con,
      paste0(
        "SELECT DISTINCT ON (s.sample_id)
          s.sample_id, l.location_id, l.name, sl.sub_location_name, s.depth_height_m AS depth, s.datetime AS sample_datetime_utc, s.target_datetime AS target_datetime_utc, m.media_type, st.sample_type, cm.collection_method, s.sample_volume_ml, s.purge_volume_l, s.purge_time_min, s.flow_rate_l_min, s.wave_height_m AS wave_hgt_m, s.grade_type_id AS sample_grade, s.approval_type_id AS sample_approval, qualifiers.sample_qualifier_ids, qualifiers.sample_qualifiers, s.note FROM discrete.samples s
           JOIN public.locations l ON s.location_id = l.location_id
           LEFT JOIN public.sub_locations sl ON s.sub_location_id = sl.sub_location_id
           JOIN public.media_types m ON s.media_id = m.media_id
           JOIN discrete.sample_types st ON s.sample_type_id = st.sample_type_id
           JOIN discrete.collection_methods cm ON s.collection_method_id = cm.collection_method_id
           LEFT JOIN public.organizations orgs ON s.owner = orgs.organization_id
           LEFT JOIN LATERAL (
             SELECT
               string_agg(
                 sq.qualifier_type_id::text,
                 ', ' ORDER BY sq.qualifier_type_id
               ) AS sample_qualifier_ids,
               string_agg(
                 qt.qualifier_type_description,
                 '; ' ORDER BY sq.qualifier_type_id
               ) AS sample_qualifiers
             FROM discrete.sample_qualifiers sq
             JOIN public.qualifier_types qt USING (qualifier_type_id)
             WHERE sq.sample_id = s.sample_id
           ) qualifiers ON TRUE
           WHERE ",
        where,
        "
           AND EXISTS (SELECT 1 FROM discrete.results r
                       WHERE r.sample_id = s.sample_id AND ",
        parameter_where,
        ")
           ORDER BY s.sample_id;"
      )
    )
    na_cols <- setdiff(
      names(samples)[vapply(
        samples,
        function(x) {
          length(x) > 0L && all(is.na(x))
        },
        logical(1)
      )],
      c("sample_id", "location_id")
    )
    samples[, (na_cols) := NULL]
    return(samples)
  }
  selected_sampleids <- request$sample_ids
  selected_loc_ids <- request$location_ids
  if (kind == "preview") {
    sample_ids_str <- paste(selected_sampleids, collapse = ",")
    # Single query using a window function to limit to 3 rows/results per sample_id
    query <- paste0(
      "SELECT * FROM (SELECT r.result_id, r.sample_id, r.result, p.param_name AS parameter, ",
      ac_parameter_unit_select_sql(
        con,
        "p",
        "units",
        matrix_state_alias = "r",
        media_alias = "s"
      ),
      ",
          rs.reporting_basis, spec.result_speciation, target.result_target,
          r.endpoint_mode, r.endpoint_ph, r.analysis_conditions,
          rt.result_type,
          sf.sample_fraction,
          rc.result_condition,
          r.result_condition_value,
          rvt.result_value_type,
          pm.protocol_name,
          l.lab_name AS laboratory,
          r.analysis_datetime,
          r.lab_report_no,
          r.lab_sample_no,
          r.grade_type_id AS result_grade_id,
          rg.grade_type_code AS result_grade_code,
          rg.grade_type_description AS result_grade_description,
          rg.grade_type_description_fr AS result_grade_description_fr,
          r.approval_type_id AS result_approval_id,
          rap.approval_type_code AS result_approval_code,
          rap.approval_type_description AS result_approval_description,
          rap.approval_type_description_fr AS result_approval_description_fr,
          rat.aggregation_type,
          ra.calculation_version,
          ra.calculation_arguments::text AS calculation_arguments,
          ra.expected_count,
          ROW_NUMBER() OVER (
            PARTITION BY r.sample_id ORDER BY r.result_id
          ) AS rn
        FROM discrete.results r
        JOIN discrete.samples s ON r.sample_id = s.sample_id
        JOIN public.parameters p ON r.parameter_id = p.parameter_id
        JOIN discrete.result_types rt ON r.result_type_id = rt.result_type_id
        LEFT JOIN discrete.sample_fractions sf ON r.sample_fraction_id = sf.sample_fraction_id
        LEFT JOIN discrete.result_conditions rc ON r.result_condition_id = rc.result_condition_id
        LEFT JOIN discrete.result_value_types rvt ON r.result_value_type_id = rvt.result_value_type_id
        LEFT JOIN discrete.reporting_bases rs ON r.reporting_basis_id = rs.reporting_basis_id
        LEFT JOIN discrete.result_speciations spec ON r.result_speciation_id = spec.result_speciation_id
        LEFT JOIN discrete.result_targets target ON r.result_target_id = target.result_target_id
        LEFT JOIN discrete.protocols_methods pm ON r.protocol_method_id = pm.protocol_method_id
        LEFT JOIN discrete.laboratories l ON r.lab_id = l.lab_id
        LEFT JOIN public.grade_types rg ON r.grade_type_id = rg.grade_type_id
        LEFT JOIN public.approval_types rap ON r.approval_type_id = rap.approval_type_id
        LEFT JOIN discrete.result_aggregations ra ON r.result_id = ra.result_id
        LEFT JOIN discrete.result_aggregation_types rat USING (result_aggregation_type_id)
        WHERE r.sample_id IN (",
      sample_ids_str,
      ") AND r.parameter_id IN (",
      paste(request$parameter_ids, collapse = ","),
      ")) sub WHERE rn <= 3;"
    )

    subset <- dbGetQueryDT(con, query)
    subset[, rn := NULL] # Drop column 'rn', left over from the window function

    # Get location metadata
    location <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT * FROM ",
        if (request$lang == "fr") {
          "location_metadata_fr"
        } else {
          "location_metadata_en"
        },
        " WHERE location_id IN (",
        paste(selected_loc_ids, collapse = ", "),
        ") LIMIT 3;"
      )
    ) # Get the location metadata

    qaqc_error <- NULL
    qaqc <- tryCatch(
      disc_plot_related_qaqc_data(con, selected_sampleids, request$lang),
      error = function(e) {
        qaqc_error <<- conditionMessage(e)
        disc_plot_empty_qaqc_data()
      }
    )
    rows <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT COUNT(*) FROM discrete.results r WHERE ",
        disc_data_id_predicate("r.sample_id", selected_sampleids),
        " AND ",
        disc_data_id_predicate("r.parameter_id", request$parameter_ids)
      )
    )[[1]]
    return(list(
      subset = subset,
      location = location,
      qaqc = qaqc,
      rows = as.numeric(rows),
      qaqc_error = qaqc_error
    ))
  }
  if (kind == "download") {
    file <- request$file
    data <- list()
    data$location_metadata <- dbGetQueryDT(
      con,
      paste0(
        "SELECT * FROM ",
        if (request$lang == "fr") {
          "location_metadata_fr"
        } else {
          "location_metadata_en"
        },
        " WHERE location_id IN (",
        paste(selected_loc_ids, collapse = ", "),
        ");"
      )
    ) # Get the location metadata
    data$samples <- disc_sample_metadata(
      con,
      selected_sampleids,
      request$lang
    )
    data$results <- disc_result_metadata(
      con,
      selected_sampleids,
      request$lang,
      parameter_ids = request$parameter_ids
    )
    data$result_components <- disc_result_components(
      con,
      data$results$result_id,
      lang = request$lang
    )
    if (!nrow(data$result_components)) {
      data$result_components <- NULL
    }
    data$sample_documents <- disc_sample_documents(
      con,
      selected_sampleids
    )
    if (!nrow(data$sample_documents)) {
      data$sample_documents <- NULL
    }
    data$sample_group_memberships <- disc_sample_group_memberships(
      con,
      selected_sampleids,
      request$lang
    )
    if (!nrow(data$sample_group_memberships)) {
      data$sample_group_memberships <- NULL
    }

    qaqc_data <- request$qaqc
    if (nrow(qaqc_data$samples) > 0L) {
      data$qaqc_group_links <- qaqc_data$group_links
      data$qaqc_samples <- qaqc_data$samples
      data$qaqc_results <- qaqc_data$results
      if (nrow(qaqc_data$result_components) > 0L) {
        data$qaqc_result_components <- qaqc_data$result_components
      }
      if (nrow(qaqc_data$documents) > 0L) {
        data$qaqc_documents <- qaqc_data$documents
      }
    }

    if ("sample_grade_id" %in% names(data$samples)) {
      data$grades <- dbGetQueryDT(
        con,
        "SELECT * FROM public.grade_types;"
      )
    }
    if ("sample_approval_id" %in% names(data$samples)) {
      data$approvals <- dbGetQueryDT(
        con,
        "SELECT * FROM public.approval_types;"
      )
    }
    if ("sample_qualifier_ids" %in% names(data$samples)) {
      data$qualifiers <- dbGetQueryDT(
        con,
        "SELECT * FROM public.qualifier_types;"
      )
    }

    if (request$format == "xlsx") {
      openxlsx::write.xlsx(data, file)
    } else if (request$format == "csv") {
      # Temporary directory to store CSV files
      temp_dir <- tempfile("discrete-export-")
      dir.create(temp_dir)
      on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
      csv_files <- lapply(names(data), function(name) {
        file_name <- file.path(temp_dir, paste0(name, ".csv"))
        data.table::fwrite(data[[name]], file_name)
        return(file_name)
      })
      # Use zip to compress the files
      zip::zipr(file, unlist(csv_files), root = temp_dir)
    } else if (request$format == "sqlite") {
      # Create an sqlite database and write the data tables to it
      db <- DBI::dbConnect(RSQLite::SQLite(), dbname = file)
      on.exit(DBI::dbDisconnect(db), add = TRUE)
      lapply(names(data), function(name) {
        DBI::dbWriteTable(
          conn = db,
          name = name,
          value = data[[name]],
          overwrite = TRUE
        )
      })
    }

    return(file)
  }
  stop("Unknown discrete-data task.")
}

discDataUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(
      HTML(
        "/* Make the ...preparing download... notification stand out more */
        .shiny-notification {
          font-size: 24px;
          font-weight: bold;
          background-color: #f9f9f9;  /* Light grey background */
          color: #333;  /* Dark grey text */
          padding: 15px;  /* Larger padding for more space */
          border-left: 5px solid #007BFF;  /* Blue left border */
          border-right: 5px solid #007BFF;  /* Blue right border */
          border-top: 5px solid #007BFF;  /* Blue top border */
          border-bottom: 5px solid #007BFF;  /* Blue bottom border */
          border-radius: 10px;  /* Rounded corners */
        }"
      ),
      HTML(sprintf(
        "
     /* Add colors to the accordion. Using ns() makes it specific to each accordion */
      #%s.accordion {
        /* body background */
        --bs-accordion-bg:          #E5F4F6;
        /* collapsed header */
        --bs-accordion-btn-bg:      #0097A9;
        /* expanded header */
        --bs-accordion-active-bg:   #0097A9;
      }
    ",
        ns("accordion")
      ))
    ),
    uiOutput(ns("banner")),
    uiOutput(ns("top")),
    page_sidebar(
      sidebar = sidebar(
        title = NULL,
        width = 400,
        bg = config$sidebar_bg,
        open = list(mobile = "always-above"),
        uiOutput(ns("sidebar")) # UI is rendered in the server function below so that it can use database information as well as language selections.
      ),
      uiOutput(ns("main"))
    )
  )
}

discData <- function(id, language, inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    moduleData <- reactiveValues()
    filteredData <- reactiveValues()
    defaults <- list(
      date_range = as.Date(character()),
      locations = "all",
      sub_locations = "all",
      media = "all",
      sample_types = "all",
      params = "all"
    )
    filter_state <- reactiveVal(defaults)
    revision <- reactiveVal(0L)
    table_data <- reactiveVal(NULL)
    table_parameters <- reactiveVal(integer())
    selected_qaqc_data <- reactiveVal(disc_plot_empty_qaqc_data())
    preview_data <- reactiveVal(NULL)
    preview_request <- reactiveVal(NULL)
    parameter_request <- reactiveVal(NULL)
    parameter_started <- NULL
    download_file <- reactiveVal(NULL)
    task_files <- character()
    task_handles <- new.env(parent = emptyenv())
    selector_choices <- new.env(parent = emptyenv())

    # Copy only pure helper functions to the daemon. In particular, neither
    # session nor the live connection is reachable through this environment.
    worker_env <- new.env(parent = asNamespace("YGwater"))
    helper_names <- c(
      "disc_data_run",
      "disc_data_id_predicate",
      "disc_data_module_data",
      "disc_plot_tabularize",
      "disc_sample_metadata",
      "disc_result_metadata",
      "disc_result_components",
      "disc_sample_documents",
      "disc_sample_group_memberships",
      "disc_plot_empty_qaqc_data",
      "disc_plot_related_qaqc_data"
    )
    for (name in helper_names) {
      fn <- get(name, mode = "function")
      environment(fn) <- worker_env
      worker_env[[name]] <- fn
    }
    run_task <- worker_env$disc_data_run
    new_task <- function() {
      ExtendedTask$new(function(request) {
        job <- mirai::mirai(
          {
            tryCatch(
              list(ok = TRUE, id = request$id, value = run_task(request)),
              error = function(e) {
                list(ok = FALSE, id = request$id, message = conditionMessage(e))
              }
            )
          },
          request = request,
          run_task = run_task,
          .compute = "YGwater_aqualink"
        )
        task_handles[[request$kind]] <- job
        job
      })
    }
    catalogue_task <- new_task()
    parameter_task <- new_task()
    samples_task <- new_task() |> bslib::bind_task_button("filter")
    preview_task <- new_task() |> bslib::bind_task_button("view_data")
    download_task <- new_task() |> bslib::bind_task_button("prepare_download")

    request <- function(kind, id, ...) {
      # Snapshot the current login, including credentials changed after startup.
      cfg <- session$userData$config
      list(
        kind = kind,
        id = id,
        config = cfg[c(
          "dbName",
          "dbHost",
          "dbPort",
          "dbUser",
          "dbPass"
        )],
        ...
      )
    }
    task_result <- function(task) {
      result <- tryCatch(task$result(), error = function(e) {
        if (inherits(e, "shiny.silent.error")) {
          stop(e)
        }
        showNotification(conditionMessage(e), type = "error", duration = 10)
        NULL
      })
      req(!is.null(result))
      if (!isTRUE(result$ok)) {
        showNotification(result$message, type = "error", duration = 10)
        req(FALSE)
      }
      result
    }
    invalidate_results <- function() {
      revision(isolate(revision()) + 1L)
      table_data(NULL)
      preview_request(NULL)
      preview_data(NULL)
      download_file(NULL)
      selected_qaqc_data(disc_plot_empty_qaqc_data())
      DT::selectRows(proxy, NULL)
      shinyjs::hide("select_all")
      shinyjs::hide("view_data")
    }
    update_selector <- function(id, data, id_col, label_col) {
      choices <- stats::setNames(
        c("all", as.character(data[[id_col]])),
        c(tr("all_m", language$language), data[[label_col]])
      )
      selected <- filter_state()[[id]]
      choices_changed <- !identical(selector_choices[[id]], choices)
      selection_changed <- !identical(as.character(input[[id]]), selected)
      if (!choices_changed && !selection_changed) {
        return(invisible(NULL))
      }
      selector_choices[[id]] <- choices
      freezeReactiveValue(input, id)
      # These catalogues are small enough to keep in the browser. Re-registering
      # server-side Selectize AJAX URLs causes late load callbacks to restore old
      # selections, which then trigger another filter/update cycle.
      updateSelectizeInput(
        session,
        id,
        choices = if (choices_changed) choices else NULL,
        selected = selected
      )
    }
    refresh_filters <- function() {
      cached <- reactiveValuesToList(moduleData)
      next_state <- disc_data_filter_catalogue(cached, filter_state())
      filter_state(next_state$state)
      for (name in names(next_state$data)) {
        filteredData[[name]] <- next_state$data[[name]]
      }
      update_selector(
        "locations",
        filteredData$locs,
        "location_id",
        tr("generic_name_col", language$language)
      )
      update_selector(
        "sub_locations",
        filteredData$sub_locs,
        "sub_location_id",
        tr("sub_location_col", language$language)
      )
      update_selector(
        "media",
        filteredData$media,
        "media_id",
        tr("media_type_col", language$language)
      )
      update_selector(
        "sample_types",
        filteredData$sample_types,
        "sample_type_id",
        tr("sample_type_col", language$language)
      )
      ids <- filteredData$samples$sample_id
      old <- parameter_request()
      if (is.null(old) || !identical(ids, old$sample_ids)) {
        parameter_request(list(id = revision(), sample_ids = ids))
      }
    }
    set_filter <- function(id, values) {
      state <- filter_state()
      if (identical(state[[id]], values)) {
        return(invisible(NULL))
      }
      state[[id]] <- values
      filter_state(state)
      invalidate_results()
      if (id != "params") refresh_filters()
    }

    # Initial catalogue loading also runs outside the Shiny process.
    observe({
      version <- session$userData$AquaCache_version
      if (is.function(version)) {
        version()
      }
      isolate({
        invalidate_results()
        moduleData$range <- NULL
        parameter_request(NULL)
        parameter_started <<- NULL
        catalogue_task$invoke(request("catalogue", revision()))
      })
    })
    observe({
      result <- task_result(catalogue_task)
      isolate({
        req(identical(result$id, revision()))
        for (name in names(result$value)) {
          moduleData[[name]] <- result$value[[name]]
          filteredData[[name]] <- result$value[[name]]
        }
        state <- defaults
        state$date_range <- as.Date(
          c(moduleData$range$min_date, moduleData$range$max_date),
          tz = "UTC"
        )
        if (
          identical(inputs$location_target, "discData") &&
            length(inputs$location_id) == 1L &&
            inputs$location_id %in% moduleData$locs$location_id
        ) {
          state$locations <- as.character(inputs$location_id)
        }
        filter_state(state)
        refresh_filters()
      })
    })

    # Coalesce rapid selector changes: at most one running query and one latest
    # pending snapshot. Old results are discarded, never applied to newer inputs.
    observe({
      pending <- parameter_request()
      status <- parameter_task$status()
      req(!is.null(pending), status != "running")
      if (identical(pending, parameter_started)) {
        return()
      }
      parameter_started <<- pending
      parameter_task$invoke(request(
        "parameters",
        pending$id,
        sample_ids = pending$sample_ids
      ))
    })
    observe({
      result <- task_result(parameter_task)
      isolate({
        req(identical(result$id, parameter_request()$id))
        filteredData$params <- moduleData$params[
          moduleData$params$parameter_id %in% result$value,
          ,
          drop = FALSE
        ]
        rel <- moduleData$parameter_relationships
        rel <- rel[rel$parameter_id %in% result$value, , drop = FALSE]
        filteredData$parameter_relationships <- rel
        filteredData$param_groups <- moduleData$param_groups[
          moduleData$param_groups$group_id %in% rel$group_id,
          ,
          drop = FALSE
        ]
        filteredData$param_sub_groups <- moduleData$param_sub_groups[
          moduleData$param_sub_groups$sub_group_id %in% rel$sub_group_id,
          ,
          drop = FALSE
        ]
        state <- filter_state()
        if (!identical(state$params, "all") && length(state$params)) {
          keep <- intersect(state$params, as.character(result$value))
          state$params <- if (length(keep)) keep else "all"
          if (!identical(state, filter_state())) {
            filter_state(state)
            invalidate_results()
          }
        }
        update_selector(
          "params",
          filteredData$params,
          "parameter_id",
          tr("param_name_col", language$language)
        )
      })
    })

    output$banner <- renderUI({
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "discData"
      )
    })

    output$top <- renderUI({
      tagList(
        accordion(
          id = ns("accordion"),
          open = TRUE,
          accordion_panel(
            title = tr("instructions", language$language),
            tags$p(HTML(tr(
              "view_data_instructions_discrete",
              language$language
            ))),
            tags$div(style = "height: 10px;"),
          )
        )
      )
    }) |> # End of renderUI for instructions
      bindEvent(language$language)

    output$sidebar <- renderUI({
      req(moduleData$range, language)

      tags <- tagList(
        # start and end datetime
        dateRangeInput(
          ns("date_range"),
          tr("date_range_lab", language$language),
          start = filter_state()$date_range[1],
          end = filter_state()$date_range[2],
          min = as.Date(filteredData$range$min_date),
          format = "yyyy-mm-dd",
          language = language$abbrev,
          separator = tr("date_sep", language$language)
        ),
        # Selectize input for locations
        selectizeInput(
          ns("locations"),
          label = tr("loc(s)", language$language),
          choices = stats::setNames(
            c("all", filteredData$locs$location_id),
            c(
              tr("all_m", language$language),
              filteredData$locs[, tr("generic_name_col", language$language)]
            )
          ),
          multiple = TRUE,
          selected = filter_state()$locations
        ),
        # Button for modal to let users filter locations by network or project
        actionButton(
          ns("loc_modal"),
          label = tr("loc_modal", language$language),
          width = "100%",
          style = "font-size: 14px; margin-top: 5px;"
        ),
        # Selectize input for sub-locations
        selectizeInput(
          ns("sub_locations"),
          label = tr("sub_loc(s)", language$language),
          choices = stats::setNames(
            c("all", filteredData$sub_locs$sub_location_id),
            c(
              tr("all_m", language$language),
              filteredData$sub_locs[, tr("sub_location_col", language$language)]
            )
          ),
          multiple = TRUE,
          selected = filter_state()$sub_locations
        ),
        # Selectize input for media type
        selectizeInput(
          ns("media"),
          label = tr("media_type(s)", language$language),
          choices = stats::setNames(
            c("all", filteredData$media$media_id),
            c(
              tr("all_m", language$language),
              filteredData$media[, tr("media_type_col", language$language)]
            )
          ),
          multiple = TRUE,
          selected = filter_state()$media
        ),
        # Selectize input for sample types
        selectizeInput(
          ns("sample_types"),
          label = tr("sample_type(s)", language$language),
          choices = stats::setNames(
            c("all", filteredData$sample_types$sample_type_id),
            c(
              tr("all_m", language$language),
              filteredData$sample_types[, tr(
                "sample_type_col",
                language$language
              )]
            )
          ),
          multiple = TRUE,
          selected = filter_state()$sample_types
        ),
        selectizeInput(
          ns("params"),
          label = tr("parameter(s)", language$language),
          choices = stats::setNames(
            c("all", filteredData$params$parameter_id),
            c(
              tr("all_m", language$language),
              filteredData$params[, tr("param_name_col", language$language)]
            )
          ),
          multiple = TRUE,
          selected = filter_state()$params
        ),
        # Modal button to let users filter parameters by group or sub-group
        actionButton(
          ns("param_modal"),
          label = tr("param_modal", language$language),
          width = "100%",
          style = "font-size: 14px; margin-top: 5px;"
        ),
        # Divider
        tags$div(style = "height: 15px;"),

        # Side-by-side buttons
        splitLayout(
          cellWidths = c("50%", "50%"),
          actionButton(
            ns("reset"),
            label = tr("reset_filters", language$language),
            width = "100%",
            style = "font-size: 14px;",
            class = "btn btn-primary"
          ),
          bslib::input_task_button(
            ns("filter"),
            label = tr("view_samples", language$language),
            width = "100%",
            style = "font-size: 14px;",
            class = "btn btn-primary"
          )
        )
      ) # End of tagList

      return(tags)
    }) |> # End of renderUI for sidebar
      bindEvent(language$language, moduleData$range)

    output$main <- renderUI({
      tagList(
        DT::DTOutput(ns("tbl")), # Table with sample data, filtered by the sidebar inputs
        actionButton(
          ns("select_all"),
          tr("select_all", language$language),
          style = "display: none;"
        ), # Button will be hidden until a row is selected
        bslib::input_task_button(
          ns("view_data"),
          textOutput(ns("view_data_label"), inline = TRUE),
          style = "display: none;"
        ), # Button will be hidden until a row is selected
        # The modal UI elements are created lower down
      ) # End of tagList
    }) |> # End renderUI
      bindEvent(language$language) # Re-render the UI if the language or changes

    # A single canonical selection drives filtering and normalization. Browser
    # echoes of programmatic updates are no-ops, so no reset flags are needed.
    for (id in c(
      "locations",
      "sub_locations",
      "media",
      "sample_types",
      "params"
    )) {
      local({
        input_id <- id
        observeEvent(
          input[[input_id]],
          {
            req(moduleData$range)
            values <- as.character(input[[input_id]])
            if (identical(values, filter_state()[[input_id]])) {
              return()
            }
            selected <- data_filter_normalize_selection(values)
            changed <- !identical(selected, filter_state()[[input_id]])
            set_filter(input_id, selected)
            # Sample-level changes already synchronize the controls in one pass.
            # Parameters do not change sample choices, so normalize them here.
            if (
              (!changed || input_id == "params") && !identical(values, selected)
            ) {
              freezeReactiveValue(input, input_id)
              updateSelectizeInput(session, input_id, selected = selected)
            }
          },
          ignoreNULL = FALSE
        )
      })
    }
    for (id in c("networks", "projects", "pGrps", "pSubGrps")) {
      local({
        input_id <- id
        observeEvent(
          input[[input_id]],
          {
            values <- as.character(input[[input_id]])
            selected <- data_filter_normalize_selection(values)
            if (!identical(values, selected)) {
              freezeReactiveValue(input, input_id)
              updateSelectizeInput(session, input_id, selected = selected)
            }
          },
          ignoreNULL = TRUE
        )
      })
    }
    observeEvent(input$date_range, {
      req(
        length(input$date_range) == 2L,
        !anyNA(input$date_range),
        moduleData$range
      )
      set_filter("date_range", as.Date(input$date_range))
    })
    observeEvent(input$reset, {
      req(moduleData$range)
      state <- defaults
      state$date_range <- as.Date(
        c(moduleData$range$min_date, moduleData$range$max_date),
        tz = "UTC"
      )
      filter_state(state)
      invalidate_results()
      freezeReactiveValue(input, "date_range")
      updateDateRangeInput(
        session,
        "date_range",
        start = state$date_range[1],
        end = state$date_range[2]
      )
      refresh_filters()
      update_selector(
        "params",
        moduleData$params,
        "parameter_id",
        tr("param_name_col", language$language)
      )
    })
    observeEvent(
      inputs$location_request_id,
      {
        req(identical(inputs$location_target, "discData"), moduleData$range)
        req(
          length(inputs$location_id) == 1L,
          inputs$location_id %in% moduleData$locs$location_id
        )
        state <- defaults
        state$date_range <- as.Date(
          c(moduleData$range$min_date, moduleData$range$max_date),
          tz = "UTC"
        )
        state$locations <- as.character(inputs$location_id)
        filter_state(state)
        invalidate_results()
        freezeReactiveValue(input, "date_range")
        updateDateRangeInput(
          session,
          "date_range",
          start = state$date_range[1],
          end = state$date_range[2]
        )
        refresh_filters()
      },
      ignoreInit = TRUE
    )

    observeEvent(input$filter, {
      req(moduleData$range)
      state <- filter_state()
      params <- if (identical(state$params, "all")) {
        moduleData$params$parameter_id
      } else {
        state$params
      }
      table_parameters(params)
      samples_task$invoke(request(
        "samples",
        revision(),
        state = state,
        parameter_ids = params
      ))
    })
    observe({
      result <- task_result(samples_task)
      isolate({
        req(identical(result$id, revision()))
        table_data(result$value)
        DT::selectRows(proxy, NULL)
      })
    })
    observeEvent(input$loc_modal, {
      showModal(modalDialog(
        title = tr("loc_modal", language$language),
        selectizeInput(
          ns("networks"),
          label = tr("network(s)", language$language),
          choices = stats::setNames(
            c("all", filteredData$networks$network_id),
            c(
              tr("all_m", language$language),
              filteredData$networks[, tr("generic_name_col", language$language)]
            )
          ),
          multiple = TRUE,
          selected = "all"
        ),
        selectizeInput(
          ns("projects"),
          label = tr("project(s)", language$language),
          choices = stats::setNames(
            c("all", filteredData$projects$project_id),
            c(
              tr("all_m", language$language),
              filteredData$projects[, tr("generic_name_col", language$language)]
            )
          ),
          multiple = TRUE,
          selected = "all"
        ),
        footer = tagList(
          actionButton(ns("loc_modal_filter"), tr("filter", language$language)),
          modalButton(tr("close", language$language))
        )
      ))
    })

    # Observer for the locations modal filter button
    observeEvent(input$loc_modal_filter, {
      # Filter the locations based on the selected projects and networks, update the locations selectizeInput
      req(
        input$projects,
        input$networks,
        moduleData$locations_projects,
        moduleData$locations_networks
      )

      remain_locs <- moduleData$locs

      if (!("all" %in% input$networks)) {
        net_ids <- moduleData$locations_networks$location_id[
          moduleData$locations_networks$network_id %in% input$networks
        ]
        remain_locs <- remain_locs[remain_locs$location_id %in% net_ids, ]
      }

      if (!("all" %in% input$projects)) {
        proj_ids <- moduleData$locations_projects$location_id[
          moduleData$locations_projects$project_id %in% input$projects
        ]
        remain_locs <- remain_locs[remain_locs$location_id %in% proj_ids, ]
      }
      selected <- if ("all" %in% input$networks && "all" %in% input$projects) {
        "all"
      } else {
        as.character(remain_locs$location_id)
      }
      removeModal()
      set_filter("locations", selected)
    })

    # Observer for a modal that allows users to filter their parameters by group/sub-group
    observeEvent(input$param_modal, {
      showModal(modalDialog(
        title = tr("param_modal", language$language),
        selectizeInput(
          ns("pGrps"),
          label = tr("param_groups", language$language),
          choices = stats::setNames(
            c("all", filteredData$param_groups$group_id),
            c(
              tr("all_m", language$language),
              filteredData$param_groups[, tr(
                "param_group_col",
                language$language
              )]
            )
          ),
          multiple = TRUE,
          selected = "all"
        ),
        selectizeInput(
          ns("pSubGrps"),
          label = tr("param_sub_groups", language$language),
          choices = stats::setNames(
            c("all", filteredData$param_sub_groups$sub_group_id),
            c(
              tr("all_m", language$language),
              filteredData$param_sub_groups[, tr(
                "param_sub_group_col",
                language$language
              )]
            )
          ),
          multiple = TRUE,
          selected = "all"
        ),
        footer = tagList(
          actionButton(
            ns("param_modal_filter"),
            tr("filter", language$language)
          ),
          modalButton(tr("close", language$language))
        )
      ))
    })

    # Observer for the parameters modal filter button
    observeEvent(input$param_modal_filter, {
      # Filter the parameters based on the selected groups and sub-groups, update the params selectizeInput
      req(
        input$pGrps,
        input$pSubGrps,
        filteredData$parameter_relationships,
        filteredData$params
      )

      remain_params <- moduleData$parameter_relationships
      if (!("all" %in% input$pGrps)) {
        remain_params <- remain_params[
          remain_params$group_id %in% input$pGrps,
        ]
      }
      if (!("all" %in% input$pSubGrps)) {
        remain_params <- remain_params[
          remain_params$sub_group_id %in% input$pSubGrps,
        ]
      }
      remain_params <- filteredData$params[
        filteredData$params$parameter_id %in% remain_params$parameter_id,
      ]
      selected <- if ("all" %in% input$pGrps && "all" %in% input$pSubGrps) {
        "all"
      } else {
        as.character(remain_params$parameter_id)
      }
      set_filter("params", selected)
      freezeReactiveValue(input, "params")
      updateSelectizeInput(session, "params", selected = selected)
      removeModal()
    })

    qaqc_table <- function(name) {
      DT::renderDT({
        data <- selected_qaqc_data()[[name]]
        req(nrow(data) > 0L)
        DT::datatable(
          data,
          rownames = FALSE,
          selection = "none",
          filter = "top",
          options = list(pageLength = 10, scrollX = TRUE)
        )
      })
    }

    output$modal_qaqc_samples <- qaqc_table("samples")
    output$modal_qaqc_results <- qaqc_table("results")
    output$modal_qaqc_result_components <- qaqc_table("result_components")
    output$modal_qaqc_group_links <- qaqc_table("group_links")
    output$modal_qaqc_documents <- qaqc_table("documents")

    output$modal_qaqc_ui <- renderUI({
      data <- selected_qaqc_data()
      if (nrow(data$samples) == 0L) {
        return(NULL)
      }

      tabs <- list(
        tabPanel(
          sprintf(
            "%s (%d)",
            tr("samples", language$language),
            nrow(data$samples)
          ),
          DT::DTOutput(ns("modal_qaqc_samples"))
        ),
        tabPanel(
          sprintf(
            "%s (%d)",
            tr("results", language$language),
            nrow(data$results)
          ),
          DT::DTOutput(ns("modal_qaqc_results"))
        ),
        tabPanel(
          sprintf(
            "%s (%d)",
            tr("disc_qaqc_group_links", language$language),
            nrow(data$group_links)
          ),
          DT::DTOutput(ns("modal_qaqc_group_links"))
        )
      )
      if (nrow(data$result_components) > 0L) {
        tabs[[length(tabs) + 1L]] <- tabPanel(
          sprintf(
            "%s (%d)",
            tr("disc_result_components", language$language),
            nrow(data$result_components)
          ),
          DT::DTOutput(ns("modal_qaqc_result_components"))
        )
      }
      if (nrow(data$documents) > 0L) {
        tabs[[length(tabs) + 1L]] <- tabPanel(
          sprintf(
            "%s (%d)",
            tr("documents", language$language),
            nrow(data$documents)
          ),
          DT::DTOutput(ns("modal_qaqc_documents"))
        )
      }

      tagList(
        tags$hr(),
        h4(tr("disc_qaqc_sample_data", language$language)),
        do.call(tabsetPanel, tabs)
      )
    })

    output$tbl <- DT::renderDT({
      # Clear stale results immediately when filters change.
      req(!is.null(table_data()))

      DT::datatable(
        table_data(),
        rownames = FALSE,
        selection = "multiple",
        filter = "none",
        options = list(
          scrollX = TRUE,
          initComplete = htmlwidgets::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({",
            "  'background-color': '#079',",
            "  'color': '#fff',",
            "  'font-size': '90%',",
            # "  'font-family': 'montserrat'", # Unfortunately this isn't as readable as the default font. Left just in case it's needed later.
            "});",
            "$(this.api().table().body()).css({",
            "  'font-size': '80%',",
            # "  'font-family': 'nunito-sans'", # Unfortunately this isn't as readable as the default font. Left just in case it's needed later.
            "});",
            "}"
          ),
          columnDefs = list(
            list(targets = c(0, 1), visible = FALSE) #Hides the sample_id and location_id columns. Column index numbers start at 0 here!!!
          ),
          language = list(
            info = tr("tbl_info", language$language),
            infoEmpty = tr("tbl_info_empty", language$language),
            paginate = list(previous = "", `next` = ""),
            search = tr("tbl_search", language$language),
            lengthMenu = tr("tbl_length", language$language),
            infoFiltered = tr("tbl_filtered", language$language),
            zeroRecords = tr("tbl_zero", language$language)
          ),
          pageLength = 10
        )
      )
    })
    observe({
      if (is.null(table_data()) || nrow(table_data()) == 0L) {
        shinyjs::hide("select_all")
      } else {
        shinyjs::show("select_all")
      }
    })

    # Create the proxy for datatable manipulations
    proxy <- DT::dataTableProxy("tbl")

    # Show/hide the view button based on if a row is selected #
    observe({
      if (!length(input$tbl_rows_selected) || is.null(table_data())) {
        shinyjs::hide("view_data")
      } else {
        shinyjs::show("view_data")
      }
    })

    output$view_data_label <- renderText({
      paste0(
        tr("view_data1", language$language),
        " ",
        length(input$tbl_rows_selected),
        " ",
        tr("view_data2_discrete", language$language)
      )
    })

    # Select/deselect all rows in the table #
    observeEvent(input$select_all, {
      DT::selectRows(
        proxy,
        data_table_toggle_all_rows(
          input$tbl_rows_selected,
          nrow(table_data())
        )
      )
    })

    observeEvent(input$view_data, {
      req(
        length(input$tbl_rows_selected),
        !is.null(table_data()),
        length(table_parameters())
      )
      rows <- table_data()[input$tbl_rows_selected]
      snapshot <- list(
        revision = revision(),
        sample_ids = rows$sample_id,
        location_ids = unique(rows$location_id),
        lang = language$abbrev
      )
      preview_request(snapshot)
      preview_data(NULL)
      download_file(NULL)
      preview_task$invoke(request(
        "preview",
        snapshot,
        sample_ids = rows$sample_id,
        location_ids = unique(rows$location_id),
        parameter_ids = table_parameters(),
        lang = language$abbrev
      ))
    })
    observe({
      result <- task_result(preview_task)
      isolate({
        req(
          identical(result$id, preview_request()),
          identical(result$id$revision, revision())
        )
        current <- table_data()[input$tbl_rows_selected, sample_id]
        req(identical(result$id$sample_ids, current))
        preview_data(result$value)
        selected_qaqc_data(result$value$qaqc)
        if (!is.null(result$value$qaqc_error)) {
          showNotification(
            paste(
              tr("disc_qaqc_retrieval_error", language$language),
              result$value$qaqc_error
            ),
            type = "warning",
            duration = 15
          )
        }
        subset <- result$value$subset
        location <- result$value$location
        output$modal_subset <- DT::renderDT({
          # Create datatable for the measurements
          DT::datatable(
            subset,
            rownames = FALSE,
            selection = "none",
            filter = "none",
            options = list(
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
              ),
              language = list(
                info = "",
                infoEmpty = tr("tbl_info_empty", language$language),
                paginate = list(previous = "", `next` = ""),
                search = tr("tbl_search", language$language),
                infoFiltered = "",
                zeroRecords = tr("tbl_zero", language$language)
              ),
              layout = list(
                bottomStart = 'info',
                bottomEnd = 'paging'
              )
            )
          )
        }) # End of function creating data subset datatable

        output$modal_location_metadata <- DT::renderDT({
          # Create datatable for the locations
          DT::datatable(
            location,
            rownames = FALSE,
            selection = "none",
            filter = "none",
            options = list(
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
              ),
              columnDefs = list(
                list(
                  targets = c(2, 6:9), # Column index numbers start at 0 here again!!!
                  render = htmlwidgets::JS(
                    # Truncate long strings in the table
                    "function(data, type, row, meta) {",
                    "return type === 'display' && data !== null && data.length > 20 ?",
                    "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                    "}"
                  )
                )
              ),
              language = list(
                info = "",
                infoEmpty = tr("tbl_info_empty", language$language),
                paginate = list(previous = "", `next` = ""),
                search = "",
                infoFiltered = "",
                zeroRecords = tr("tbl_zero", language$language)
              ),
              scrollX = TRUE
            )
          ) |>
            DT::formatRound(
              columns = intersect(c("latitude", "longitude"), names(location)),
              digits = 3
            ) # Round numbers, here index starts at 1 (not javascript)
        }) # End of function creating location metatadata datatable

        # Create the modal
        showModal(modalDialog(
          h4(tr("discrete_subset_msg", language$language)),
          DT::DTOutput(ns("modal_subset")),
          h4(tr("loc_meta_msg", language$language)),
          DT::DTOutput(ns("modal_location_metadata")),
          uiOutput(ns("modal_qaqc_ui")),
          textOutput(ns("num_rows")),
          selectizeInput(
            ns("modal_format"),
            label = tr("dl_format", language$language),
            choices = stats::setNames(
              c("xlsx", "csv", "sqlite"),
              c(
                tr("dl_format_xlsx", language$language),
                tr("dl_format_csv", language$language),
                tr("dl_format_sqlite", language$language)
              )
            ),
            selected = "xlsx"
          ),
          footer = tagList(
            bslib::input_task_button(
              ns("prepare_download"),
              if (identical(language$abbrev, "fr")) {
                "Pr\u00e9parer le t\u00e9l\u00e9chargement"
              } else {
                "Prepare download"
              }
            ),
            uiOutput(ns("download_ready")),
            actionButton(ns("modal_close"), tr("close", language$language))
          ),
          size = "xl"
        ))
      })
    })
    observeEvent(input$modal_close, {
      preview_request(NULL)
      preview_data(NULL)
      download_file(NULL)
      removeModal()
    })
    output$num_rows <- renderText({
      data <- preview_data()
      req(data)
      text <- paste0(tr("dl_num_results", language$language), " ", data$rows)
      if (nrow(data$qaqc$results)) {
        text <- paste(
          text,
          tr("disc_qaqc_linked_results", language$language),
          nrow(data$qaqc$results)
        )
      }
      text
    })
    observe({
      data <- preview_data()
      req(data)
      formats <- if (data$rows + nrow(data$qaqc$results) > 1000000) {
        c("csv", "sqlite")
      } else {
        c("xlsx", "csv", "sqlite")
      }
      updateSelectizeInput(
        session,
        "modal_format",
        label = tr(
          if (length(formats) == 2L) "dl_format_no_xlsx" else "dl_format",
          language$language
        ),
        choices = stats::setNames(
          formats,
          vapply(
            paste0("dl_format_", formats),
            tr,
            character(1),
            lang = language$language
          )
        ),
        selected = formats[1]
      )
    })
    observeEvent(input$modal_format, download_file(NULL))
    observeEvent(input$prepare_download, {
      data <- preview_data()
      snapshot <- preview_request()
      req(data, snapshot, input$modal_format %in% c("xlsx", "csv", "sqlite"))
      req(
        data$rows + nrow(data$qaqc$results) <= 1000000 ||
          input$modal_format != "xlsx"
      )
      file <- tempfile(
        "discrete-download-",
        fileext = if (input$modal_format == "csv") {
          ".zip"
        } else {
          paste0(".", input$modal_format)
        }
      )
      task_files <<- c(task_files, file)
      download_file(NULL)
      download_task$invoke(request(
        "download",
        list(snapshot = snapshot, format = input$modal_format),
        sample_ids = snapshot$sample_ids,
        location_ids = snapshot$location_ids,
        parameter_ids = table_parameters(),
        lang = snapshot$lang,
        qaqc = data$qaqc,
        format = input$modal_format,
        file = file
      ))
    })
    observe({
      result <- task_result(download_task)
      isolate({
        req(
          identical(result$id$snapshot, preview_request()),
          identical(result$id$format, input$modal_format)
        )
        download_file(result$value)
      })
    })
    output$download_ready <- renderUI({
      req(download_file())
      downloadButton(
        ns("download"),
        tr("dl_data", language$language),
        icon = icon("download")
      )
    })
    output$download <- downloadHandler(
      filename = function() {
        req(download_file())
        paste0(
          "discreteData_",
          format(Sys.time(), "%Y%m%d_%H%M%S%Z"),
          ".",
          tools::file_ext(download_file())
        )
      },
      content = function(file) {
        req(download_file(), file.exists(download_file()))
        if (!file.copy(download_file(), file, overwrite = TRUE)) {
          stop("Could not copy prepared download.")
        }
      }
    )
    session$onSessionEnded(function() {
      for (job in as.list(task_handles)) {
        if (mirai::unresolved(job)) mirai::stop_mirai(job)
      }
      unlink(task_files)
    })
  })
}
