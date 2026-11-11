# Tools for reviewing and merging duplicate borehole records.

manageDuplicateBoreholesUI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    tags$style(HTML(sprintf('
      #%1$s .duplicate-same { background: #d1e7dd; }
      #%1$s .duplicate-different { background: #f8d7da; }
      #%1$s .duplicate-value { white-space: pre-wrap; }
      #%1$s .match-filter-row { display: flex; justify-content: flex-start; align-items: end; gap: 0.75rem; flex-wrap: wrap; }
      #%1$s .match-filter-row .form-group { margin-bottom: 0; }
      #%1$s .match-filter-row > div { min-width: 0 !important; flex: 0 0 auto !important; }
      #%1$s .match-filter-row > div:nth-child(2) { width: 440px !important; }
      #%1$s .match-filter-row > div:nth-child(3) { width: 140px !important; }
      #%1$s .match-filter-row > div:nth-child(4), #%1$s .match-filter-row > div:nth-child(5) { width: 250px !important; }
      #%1$s .match-filter-row > div:nth-child(6) { width: 210px !important; }
      #%1$s .match-filter-row .selectize-control { width: 100%%; }
      #%1$s .comparison-grid { display: grid; grid-template-columns: minmax(0, 4fr) minmax(240px, 3fr) minmax(0, 4fr); align-items: center; gap: 0.5rem; }
      #%1$s .unpair-grid { display: grid; grid-template-columns: minmax(0, 1fr) minmax(0, 1fr); align-items: start; gap: 0.75rem; }
      #%1$s .comparison-grid .form-group { margin-bottom: 0; }
      #%1$s .comparison-editor { width: 100%%; text-align: center; }
      #%1$s .comparison-editor .form-group { width: 100%%; }
      #%1$s .comparison-editor input, #%1$s .comparison-editor textarea { width: 100%% !important; text-align: center; }
      #%1$s .comparison-value { display: block; width: 100%%; min-height: 2.5rem; border: 1px solid #adb5bd; text-align: left; white-space: pre-wrap; }
      #%1$s .comparison-value.comparison-selected { outline: 3px solid #0d6efd; outline-offset: 1px; box-shadow: inset 0 0 0 2px rgba(255, 255, 255, 0.8); }
      #%1$s .document-pane { height: 1250px; overflow: hidden; border: 1px solid #dee2e6; padding: 0.75rem; }
      #%1$s .document-pane img { width: 100%%; height: auto; }
      #%1$s .document-pane iframe { width: 100%%; height: 1200px; border: 0; }
    ', ns('page')))),
    div(
      id = ns('page'),
      uiOutput(ns('banner')),
      bslib::accordion(
        bslib::accordion_panel(
          'Find duplicate candidates',
          div(
            class = 'match-filter-row',
            div(actionButton(ns('apply_filters'), 'Find possible matches', icon = icon('filter'), class = 'btn-primary')),
            div(
              style = 'min-width: 450px; flex: 2 1 600px;',
              selectizeInput(
                ns('match_criteria'), 'Match criteria',
                choices = c(
                  'Matching name' = 'name',
                  'Matching ID' = 'id',
                  'Matching location' = 'location',
                  'Matching date' = 'date',
                  'Shared document' = 'document'
                ),
                selected = c('name', 'location'), multiple = TRUE,
                options = list(plugins = list('remove_button'))
              )
            ),
            div(
              style = 'min-width: 130px; flex: 0 1 150px;',
              selectizeInput(
                ns('criteria_match_mode'), 'Match rule',
                choices = c('Any criterion' = 'any', 'All criteria' = 'all'),
                selected = 'any', multiple = FALSE
              )
            ),
            div(
              style = 'min-width: 210px; flex: 1 1 260px;',
              sliderInput(ns('location_distance_m'), 'Matching distance (metres)', min = 0, max = 250, value = 100, step = 5)
            ),
            div(
              style = 'min-width: 210px; flex: 1 1 260px;',
              sliderInput(ns('date_distance_days'), 'Matching date tolerance (days)', min = 0, max = 7, value = 0, step = 1)
            ),
            div(
              style = 'min-width: 210px; flex: 1 1 235px;',
              checkboxInput(ns('hide_identical_attributes'), 'Hide matching fields', TRUE)
            )
          ),
          p(class = 'text-muted', 'A possible match meets any selected criterion.')
        ),
        open = TRUE
      ),
      br(),
      h4('Possible matches'),
      fluidRow(
        column(2, actionButton(ns('previous_pair'), 'Previous', icon = icon('chevron-left'))),
        column(8, div(style = 'text-align: center; padding-top: 8px;', textOutput(ns('pair_counter')))),
        column(2, actionButton(ns('next_pair'), 'Next', icon = icon('chevron-right')))
      ),
      br(),
      DT::DTOutput(ns('pair_table')),
      br(),
      actionButton(ns('load_pair'), 'Load selected records', icon = icon('arrow-down'), class = 'btn-primary'),
      br(),
      uiOutput(ns('comparison'))
    )
  )
}

manageDuplicateBoreholes <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    con <- session$userData$AquaCache
    state <- reactiveValues(refresh = 0L, pairs = NULL, groups = list(), pair_index = 1L, approvals = NULL, drillers = NULL, purposes = NULL, selected_sides = list(), unpair_mode = FALSE, comparison_ids = integer(), pending_ids = integer(), candidate_status = list())

    fields <- c(
      borehole_name = 'Borehole name',
      latitude = 'Latitude',
      longitude = 'Longitude',
      ground_elevation_m = 'Ground elevation (m)',
      elevation_source = 'Elevation source',
      completion_date = 'Completion date',
      depth_m = 'Depth (m)',
      bedrock_reached = 'Bedrock reached',
      depth_to_bedrock_m = 'Depth to bedrock (m)',
      drilled_by = 'Driller',
      borehole_purpose_id = 'Borehole purpose',
      notes = 'Notes',
      location_id = 'Monitoring location ID'
    )
    numeric_fields <- c('latitude', 'longitude', 'ground_elevation_m', 'depth_m', 'depth_to_bedrock_m', 'drilled_by', 'borehole_purpose_id', 'location_id')
    date_fields <- 'completion_date'
    summary_fields <- c(
      well_count = 'Associated well count',
      well_names = 'Associated well names',
      document_count = 'Linked document count'
    )
    comparison_fields <- c(fields, summary_fields)
    well_comparison_fields <- c(
      well_name = 'Well name',
      approval_type_description = 'Approval level',
      purpose_name = 'Well purpose',
      casing_diameter_mm = 'Casing diameter (mm)',
      casing_depth_to_m = 'Casing depth (m)',
      screen_top_depth_m = 'Screen top depth (m)',
      screen_bottom_depth_m = 'Screen bottom depth (m)',
      static_water_level_m = 'Static water level (m)',
      estimated_yield_lps = 'Estimated yield (L/s)',
      notes = 'Well notes',
      share_with_display = 'Shared with'
    )

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns, lang = language$language, con = con,
        module_id = 'manageDuplicateBoreholes'
      )
    })

    display_value <- function(value) {
      if (is.null(value) || !length(value) || is.na(value[[1]]) || !nzchar(trimws(as.character(value[[1]])))) {
        return('—')
      }
      as.character(value[[1]])
    }
    normalized_value <- function(value) {
      value <- display_value(value)
      if (identical(value, '—')) '' else trimws(tolower(value))
    }
    input_date_value <- function(value) {
      if (is.null(value) || !length(value) || is.na(value[[1]])) return(NULL)
      date <- if (inherits(value, 'Date')) value[1] else suppressWarnings(as.Date(as.character(value[[1]])))
      if (length(date) != 1L || is.na(date)) NULL else format(date, '%Y-%m-%d')
    }

    load_reference_data <- function() {
      state$approvals <- DBI::dbGetQuery(
        con,
        'SELECT approval_type_id, approval_type_description FROM public.approval_types ORDER BY approval_type_id;'
      )
      state$drillers <- DBI::dbGetQuery(
        con,
        'SELECT driller_id, name FROM boreholes.drillers ORDER BY name;'
      )
      state$purposes <- DBI::dbGetQuery(
        con,
        'SELECT borehole_well_purpose_id, purpose_name FROM boreholes.borehole_well_purposes ORDER BY purpose_name;'
      )
    }

    observe({
      criteria <- input$match_criteria
      if (is.null(criteria)) criteria <- character()
      shinyjs::toggleState(ns('location_distance_m'), condition = 'location' %in% criteria)
      shinyjs::toggleState(ns('date_distance_days'), condition = 'date' %in% criteria)
    })
    possible_pairs_query <- function() {
      criteria <- character()
      selected_criteria <- input$match_criteria
      if (is.null(selected_criteria)) selected_criteria <- character()
      params <- list()
      next_placeholder <- function() paste0('$', length(params) + 1L)

      if ('name' %in% selected_criteria) {
        criteria <- c(criteria, "coalesce(trim(b1.borehole_name), '') <> '' AND lower(trim(b1.borehole_name)) = lower(trim(b2.borehole_name))")
      }
      if ('id' %in% selected_criteria) {
        criteria <- c(criteria, "EXISTS (SELECT 1 FROM boreholes.wells w1 JOIN boreholes.wells w2 ON w2.borehole_id = b2.borehole_id AND w2.well_id <> w1.well_id AND lower(trim(w2.well_name)) = lower(trim(w1.well_name)) WHERE w1.borehole_id = b1.borehole_id AND coalesce(trim(w1.well_name), '') <> '')")
      }
      if ('location' %in% selected_criteria) {
        distance_placeholder <- next_placeholder()
        distance_metres <- if (is.null(input$location_distance_m)) 100 else input$location_distance_m
        params <- c(params, list(as.numeric(distance_metres)))
        criteria <- c(criteria, paste0(
          'b1.latitude IS NOT NULL AND b1.longitude IS NOT NULL AND b2.latitude IS NOT NULL AND b2.longitude IS NOT NULL AND ',
          '6371000 * acos(least(1.0, greatest(-1.0, ',
          'cos(radians(b1.latitude)) * cos(radians(b2.latitude)) * ',
          'cos(radians(b2.longitude) - radians(b1.longitude)) + ',
          'sin(radians(b1.latitude)) * sin(radians(b2.latitude))))) <= ',
          distance_placeholder
        ))
      }
      if ('date' %in% selected_criteria) {
        date_placeholder <- next_placeholder()
        params <- c(params, list(as.integer(if (is.null(input$date_distance_days)) 0L else input$date_distance_days)))
        criteria <- c(criteria, paste0('b1.completion_date IS NOT NULL AND b2.completion_date IS NOT NULL AND abs(b1.completion_date - b2.completion_date) <= ', date_placeholder))
      }
      if ('document' %in% selected_criteria) {
        criteria <- c(criteria, 'EXISTS (SELECT 1 FROM boreholes.boreholes_documents d1 JOIN boreholes.boreholes_documents d2 ON d2.document_id = d1.document_id AND d2.borehole_id = b2.borehole_id WHERE d1.borehole_id = b1.borehole_id)')
      }
      if (!length(criteria)) return(data.frame())

      sql <- paste0(
        'SELECT b1.borehole_id AS left_borehole_id, b1.borehole_name AS left_borehole_name, ',
        'b1.latitude AS left_latitude, b1.longitude AS left_longitude, b1.location_id AS left_location_id, b1.completion_date AS left_completion_date, ',
        'b2.borehole_id AS right_borehole_id, b2.borehole_name AS right_borehole_name, ',
        'b2.latitude AS right_latitude, b2.longitude AS right_longitude, b2.location_id AS right_location_id, b2.completion_date AS right_completion_date ',
        'FROM boreholes.boreholes b1 JOIN boreholes.boreholes b2 ON b1.borehole_id < b2.borehole_id ',
        'WHERE (', paste(criteria, collapse = if (identical(input$criteria_match_mode, 'all')) ' AND ' else ' OR '), ') ',
        'ORDER BY b1.borehole_name, b2.borehole_name, b1.borehole_id, b2.borehole_id LIMIT 1000;'
      )
      if (length(params)) {
        DBI::dbGetQuery(con, sql, params = params)
      } else {
        DBI::dbGetQuery(con, sql)
      }
    }

    match_groups <- function(pairs) {
      if (is.null(pairs) || !nrow(pairs)) return(list())
      remaining <- seq_len(nrow(pairs))
      groups <- list()
      while (length(remaining)) {
        members <- unique(c(pairs$left_borehole_id[remaining[[1]]], pairs$right_borehole_id[remaining[[1]]]))
        group_rows <- integer()
        changed <- TRUE
        while (changed) {
          related <- remaining[
            pairs$left_borehole_id[remaining] %in% members |
              pairs$right_borehole_id[remaining] %in% members
          ]
          changed <- length(setdiff(related, group_rows)) > 0L
          group_rows <- unique(c(group_rows, related))
          members <- unique(c(members, pairs$left_borehole_id[related], pairs$right_borehole_id[related]))
        }
        groups[[length(groups) + 1L]] <- group_rows
        remaining <- setdiff(remaining, group_rows)
      }
      groups
    }

    refresh_pairs <- function() {
      shiny::withProgress(message = 'Finding possible matches…', value = 0, {
        shiny::incProgress(0.2)
        state$pairs <- possible_pairs_query()
        state$groups <- match_groups(state$pairs)
        state$pair_index <- 1L
        state$comparison_ids <- integer()
        state$pending_ids <- integer()
        state$candidate_status <- list()
        shiny::incProgress(0.8)
      })
    }
    observeEvent(con, load_reference_data(), once = TRUE)
    observeEvent(input$apply_filters, refresh_pairs(), ignoreInit = FALSE)

    current_group_pairs <- reactive({
      pairs <- state$pairs
      groups <- state$groups
      if (is.null(pairs) || !nrow(pairs) || !length(groups)) return(NULL)
      index <- min(max(1L, state$pair_index), length(groups))
      pairs[groups[[index]], , drop = FALSE]
    })
    current_pair <- reactive({
      pairs <- current_group_pairs()
      if (is.null(pairs) || !nrow(pairs)) return(NULL)
      pairs[1, , drop = FALSE]
    })
    candidate_roster <- reactive({
      pairs <- current_group_pairs()
      if (is.null(pairs) || !nrow(pairs)) return(data.frame())
      left <- data.frame(
        borehole_id = pairs$left_borehole_id, borehole_name = pairs$left_borehole_name,
        latitude = pairs$left_latitude, longitude = pairs$left_longitude,
        location_id = pairs$left_location_id, completion_date = pairs$left_completion_date
      )
      right <- data.frame(
        borehole_id = pairs$right_borehole_id, borehole_name = pairs$right_borehole_name,
        latitude = pairs$right_latitude, longitude = pairs$right_longitude,
        location_id = pairs$right_location_id, completion_date = pairs$right_completion_date
      )
      roster <- rbind(left, right)
      roster <- roster[!duplicated(roster$borehole_id), , drop = FALSE]
      roster$status <- vapply(as.character(roster$borehole_id), function(id) {
        status <- state$candidate_status[[id]]
        if (is.null(status)) 'Pending review' else status
      }, character(1))
      roster
    })
    observeEvent(current_group_pairs(), {
      state$selected_sides <- list()
      state$unpair_mode <- FALSE
      state$pending_ids <- integer()
      state$comparison_ids <- integer()
    }, ignoreInit = TRUE)

    observeEvent(input$previous_pair, {
      if (length(state$groups)) {
        state$pair_index <- if (state$pair_index <= 1L) length(state$groups) else state$pair_index - 1L
      }
    })
    observeEvent(input$next_pair, {
      if (length(state$groups)) {
        state$pair_index <- if (state$pair_index >= length(state$groups)) 1L else state$pair_index + 1L
      }
    })

    output$pair_counter <- renderText({
      pairs <- state$pairs
      groups <- state$groups
      if (is.null(pairs)) return('Choose matching criteria, then find possible matches.')
      if (!nrow(pairs)) return('No possible matches found.')
      paste('Possible match group', state$pair_index, 'of', length(groups), '— select up to two records below, then load them.')
    })

    output$pair_table <- DT::renderDT({
      roster <- candidate_roster()
      if (!nrow(roster)) return(DT::datatable(data.frame(Message = 'No duplicate candidates found.'), rownames = FALSE, options = list(dom = 't')))
      DT::datatable(
        roster,
        rownames = FALSE,
        selection = 'multiple',
        options = list(dom = 't', scrollX = TRUE, pageLength = 25)
      )
    })

    observeEvent(input$pair_table_rows_selected, {
      roster <- candidate_roster()
      rows <- input$pair_table_rows_selected
      if (!length(rows) || !nrow(roster)) {
        state$pending_ids <- integer()
        return()
      }
      rows <- rows[rows <= nrow(roster)]
      available <- !startsWith(roster$status[rows], 'Merged into')
      rows <- rows[available]
      if (length(rows) > 2L) {
        state$pending_ids <- integer()
        showNotification('Select no more than two records for comparison.', type = 'warning')
        return()
      }
      state$pending_ids <- as.integer(roster$borehole_id[rows])
    }, ignoreInit = TRUE)

    observeEvent(input$load_pair, {
      if (length(state$pending_ids) != 2L) {
        showNotification('Select exactly two records before loading the comparison.', type = 'warning')
        return()
      }
      state$comparison_ids <- state$pending_ids
      state$selected_sides <- list()
      state$unpair_mode <- FALSE
    })

    selected_ids <- reactive({
      ids <- state$comparison_ids
      if (length(ids) == 2L) return(as.integer(ids))
      integer()
    })

    selected_records <- reactive({
      ids <- selected_ids()
      if (length(ids) != 2L) return(NULL)
      records <- DBI::dbGetQuery(
        con,
        paste(
          'SELECT b.borehole_id, b.borehole_name, b.latitude, b.longitude, b.ground_elevation_m, b.elevation_source,',
          'b.completion_date, b.depth_m, b.bedrock_reached, b.depth_to_bedrock_m, b.drilled_by, d.name AS driller_name,',
          'b.borehole_purpose_id, p.purpose_name AS borehole_purpose_name, b.notes, b.location_id, b.approval_type_id,',
          '(SELECT count(*) FROM boreholes.wells w WHERE w.borehole_id = b.borehole_id) AS well_count,',
          "coalesce((SELECT string_agg(coalesce(nullif(w.well_name, ''), concat('Well ', w.well_id)), ', ' ORDER BY w.well_id) FROM boreholes.wells w WHERE w.borehole_id = b.borehole_id), '') AS well_names,",
          '(SELECT count(*) FROM boreholes.boreholes_documents bd WHERE bd.borehole_id = b.borehole_id) AS document_count',
          'FROM boreholes.boreholes b',
          'LEFT JOIN boreholes.drillers d ON d.driller_id = b.drilled_by',
          'LEFT JOIN boreholes.borehole_well_purposes p ON p.borehole_well_purpose_id = b.borehole_purpose_id',
          'WHERE b.borehole_id IN ($1, $2) ORDER BY b.borehole_id;'
        ),
        params = as.list(ids)
      )
      if (nrow(records) != 2L) return(NULL)
      records[match(ids, records$borehole_id), , drop = FALSE]
    })

    selected_documents <- reactive({
      ids <- selected_ids()
      if (length(ids) != 2L) return(data.frame())
      DBI::dbGetQuery(
        con,
        'SELECT bd.borehole_id, d.document_id, d.name, d.format, d.url FROM boreholes.boreholes_documents bd JOIN files.documents d ON d.document_id = bd.document_id WHERE bd.borehole_id IN ($1, $2) ORDER BY d.name;',
        params = as.list(ids)
      )
    })

    selected_wells <- reactive({
      ids <- selected_ids()
      if (length(ids) != 2L) return(data.frame())
      DBI::dbGetQuery(
        con,
        paste(
          'SELECT w.well_id, w.borehole_id, w.well_name, w.approval_type_id, a.approval_type_description,',
          'w.well_purpose_id, p.purpose_name, w.casing_diameter_mm, w.casing_depth_to_m,',
          'w.screen_top_depth_m, w.screen_bottom_depth_m, w.static_water_level_m, w.estimated_yield_lps,',
          "w.notes, array_to_string(w.share_with, ', ') AS share_with_display",
          'FROM boreholes.wells w',
          'LEFT JOIN public.approval_types a ON a.approval_type_id = w.approval_type_id',
          'LEFT JOIN boreholes.borehole_well_purposes p ON p.borehole_well_purpose_id = w.well_purpose_id',
          'WHERE w.borehole_id IN ($1, $2) ORDER BY w.borehole_id, w.well_id;'
        ),
        params = as.list(ids)
      )
    })

    well_field_summary <- function(wells, borehole_id, field) {
      values <- wells[wells$borehole_id == borehole_id, field, drop = TRUE]
      values <- trimws(as.character(values))
      values <- unique(values[!is.na(values) & nzchar(values)])
      if (!length(values)) '—' else paste(values, collapse = ' | ')
    }

    document_choices_for <- function(docs, borehole_id) {
      rows <- docs[docs$borehole_id == borehole_id, , drop = FALSE]
      if (!nrow(rows)) return(character())
      stats::setNames(
        as.character(rows$document_id),
        paste0(rows$name, ' (ID ', rows$document_id, ')')
      )
    }

    document_viewer <- function(document_id, title, pane_id) {
      if (is.null(document_id) || !length(document_id) || is.na(suppressWarnings(as.integer(document_id)))) {
        return(div(id = pane_id, class = 'document-pane', h5(title), p('No document selected.')))
      }
      doc <- DBI::dbGetQuery(
        con,
        'SELECT name, format, url, document FROM files.documents WHERE document_id = $1;',
        params = list(as.integer(document_id))
      )
      if (nrow(doc) != 1L) return(div(id = pane_id, class = 'document-pane', h5(title), p('Document is unavailable.')))
      format <- tolower(doc$format[[1]])
      body <- if (format %in% c('png', 'jpg', 'jpeg', 'gif', 'webp')) {
        mime <- paste0('image/', if (format == 'jpg') 'jpeg' else format)
        tags$img(src = paste0('data:', mime, ';base64,', openssl::base64_encode(doc$document[[1]])))
      } else if (format == 'pdf') {
        tags$iframe(src = paste0('data:application/pdf;base64,', openssl::base64_encode(doc$document[[1]])))
      } else if (!is.na(doc$url[[1]]) && nzchar(doc$url[[1]])) {
        tags$a('Open linked document', href = doc$url[[1]], target = '_blank', rel = 'noopener noreferrer')
      } else {
        p('Use the document table above to select and download this document.')
      }
      div(id = pane_id, class = 'document-pane', h5(title), p(doc$name[[1]]), body)
    }

    unpair_borehole_input <- function(field, record, side) {
      input_id <- ns(paste0('unpair_', side, '_', field))
      if (identical(field, 'drilled_by')) {
        choices <- if (is.null(state$drillers)) character() else stats::setNames(state$drillers$driller_id, state$drillers$name)
        selectizeInput(input_id, fields[[field]], choices = choices, selected = as.character(record[[field]]), multiple = FALSE)
      } else if (identical(field, 'borehole_purpose_id')) {
        choices <- if (is.null(state$purposes)) character() else stats::setNames(state$purposes$borehole_well_purpose_id, state$purposes$purpose_name)
        selectizeInput(input_id, fields[[field]], choices = choices, selected = as.character(record[[field]]), multiple = FALSE)
      } else if (field %in% numeric_fields) {
        numericInput(input_id, fields[[field]], value = suppressWarnings(as.numeric(record[[field]])))
      } else if (field %in% date_fields) {
        dateInput(input_id, fields[[field]], value = input_date_value(record[[field]]))
      } else {
        textAreaInput(input_id, fields[[field]], value = if (is.na(record[[field]])) '' else as.character(record[[field]]), rows = if (field == 'notes') 3 else 1, width = '100%')
      }
    }

    output$comparison <- renderUI({
      records <- selected_records()
      if (is.null(records)) {
        return(tags$div(class = 'alert alert-info', 'Choose matching criteria, then use the carousel to compare a possible pair.'))
      }
      left <- records[1, , drop = FALSE]
      right <- records[2, , drop = FALSE]
      well_rows <- selected_wells()
      document_rows <- selected_documents()
      document_choices <- if (nrow(document_rows)) {
        stats::setNames(
          as.character(unique(document_rows$document_id)),
          vapply(unique(document_rows$document_id), function(doc_id) {
            rows <- document_rows[document_rows$document_id == doc_id, , drop = FALSE]
            paste0(rows$name[[1]], ' (ID ', doc_id, '; linked to ', paste(rows$borehole_id, collapse = ', '), ')')
          }, character(1))
        )
      } else character()
      approval_choices <- if (!is.null(state$approvals)) stats::setNames(state$approvals$approval_type_id, state$approvals$approval_type_description) else character()
      hide_matching <- isTRUE(input$hide_identical_attributes)

      tagList(
        h4('Compare and merge'),
        div(
          class = 'mb-2',
          if (!isTRUE(state$unpair_mode)) tagList(
            actionButton(ns('select_all_left'), 'Select all left', icon = icon('check-double')),
            actionButton(ns('select_all_right'), 'Select all right', icon = icon('check-double'))
          ),
          actionButton(ns('unpair_wells'), 'Unpair wells', icon = icon('unlink'), class = if (isTRUE(state$unpair_mode)) 'btn-warning' else 'btn-secondary')
        ),
        if (isTRUE(state$unpair_mode)) {
          tagList(
            p(class = 'text-muted', 'Unpair mode keeps both boreholes. Edit their fields independently; no shared merge selection is used.'),
            lapply(names(fields), function(field) {
              tagList(
                h5(class = 'mt-3 mb-1', fields[[field]]),
                tags$div(
                  class = 'unpair-grid',
                  tags$div(class = 'border rounded p-2', unpair_borehole_input(field, left, 'left')),
                  tags$div(class = 'border rounded p-2', unpair_borehole_input(field, right, 'right'))
                )
              )
            }),
            uiOutput(ns('unpair_well_editors'))
          )
        } else lapply(names(comparison_fields), function(field) {
          display_field <- switch(
            field,
            drilled_by = 'driller_name',
            borehole_purpose_id = 'borehole_purpose_name',
            field
          )
          left_value <- display_value(left[[display_field]])
          right_value <- display_value(right[[display_field]])
          same <- identical(normalized_value(left[[field]]), normalized_value(right[[field]]))
          if (hide_matching && same) return(NULL)
          selected_side <- state$selected_sides[[field]]
          if (is.null(selected_side)) selected_side <- 'left'
          value_class <- if (same) 'duplicate-same' else 'duplicate-different'
          left_class <- paste('comparison-value', value_class, if (identical(selected_side, 'left')) 'comparison-selected' else '')
          right_class <- paste('comparison-value', value_class, if (identical(selected_side, 'right')) 'comparison-selected' else '')
          input_control <- if (identical(field, 'drilled_by')) {
            choices <- if (is.null(state$drillers)) character() else stats::setNames(state$drillers$driller_id, state$drillers$name)
            selectizeInput(ns(paste0('merged_', field)), NULL, choices = choices, selected = as.character(left[[field]]), multiple = FALSE)
          } else if (identical(field, 'borehole_purpose_id')) {
            choices <- if (is.null(state$purposes)) character() else stats::setNames(state$purposes$borehole_well_purpose_id, state$purposes$purpose_name)
            selectizeInput(ns(paste0('merged_', field)), NULL, choices = choices, selected = as.character(left[[field]]), multiple = FALSE)
          } else if (field %in% names(summary_fields)) {
            textAreaInput(ns(paste0('merged_', field)), NULL, value = if (identical(left_value, '—')) '' else left_value, width = '100%', rows = if (field == 'well_names') 2 else 1)
          } else if (field %in% numeric_fields) {
            numericInput(ns(paste0('merged_', field)), NULL, value = suppressWarnings(as.numeric(left[[field]])))
          } else if (field %in% date_fields) {
            dateInput(ns(paste0('merged_', field)), NULL, value = input_date_value(left[[field]]))
          } else {
            textAreaInput(ns(paste0('merged_', field)), NULL, value = if (identical(left_value, '—')) '' else left_value, width = '100%', rows = if (field == 'notes') 3 else 1)
          }
          tagList(
            h5(class = 'mt-3 mb-1', comparison_fields[[field]]),
            tags$div(
              class = 'comparison-grid',
              actionButton(
                ns(paste0('use_left_', field)), left_value,
                class = left_class
              ),
              tags$div(class = 'comparison-editor border rounded p-2', input_control),
              actionButton(
                ns(paste0('use_right_', field)), right_value,
                class = right_class
              )
            )
          )
        }),
        if (!isTRUE(state$unpair_mode) && nrow(well_rows)) tagList(
          tags$hr(),
          h4('Associated well attributes'),
          p(class = 'text-muted', 'Individual well values can be retained and edited in the merge review.'),
          lapply(names(well_comparison_fields), function(field) {
            left_value <- well_field_summary(well_rows, left$borehole_id[[1]], field)
            right_value <- well_field_summary(well_rows, right$borehole_id[[1]], field)
            same <- identical(trimws(tolower(left_value)), trimws(tolower(right_value)))
            if (hide_matching && same) return(NULL)
            tagList(
              h5(class = 'mt-3 mb-1', well_comparison_fields[[field]]),
              tags$div(
                class = 'comparison-grid',
                tags$div(class = if (same) 'duplicate-same duplicate-value p-2' else 'duplicate-different duplicate-value p-2', left_value),
                tags$div(class = 'comparison-editor border rounded p-2 text-muted', 'Manage in merge review'),
                tags$div(class = if (same) 'duplicate-same duplicate-value p-2' else 'duplicate-different duplicate-value p-2', right_value)
              )
            )
          })
        ),
        tags$hr(),
        h5('Documents'),
        checkboxGroupInput(ns('keep_documents'), 'Keep document links on the merged record', choices = document_choices, selected = unname(document_choices)),
        if (nrow(document_rows)) tagList(
          downloadButton(ns('download_document'), 'Download selected document'),
          DT::DTOutput(ns('documents')),
          fluidRow(
            column(6, selectizeInput(ns('left_document_id'), 'Left document', choices = document_choices_for(document_rows, left$borehole_id), selected = head(unname(document_choices_for(document_rows, left$borehole_id)), 1), multiple = FALSE)),
            column(6, selectizeInput(ns('right_document_id'), 'Right document', choices = document_choices_for(document_rows, right$borehole_id), selected = head(unname(document_choices_for(document_rows, right$borehole_id)), 1), multiple = FALSE))
          ),
          uiOutput(ns('document_comparer'))
        ) else p(class = 'text-muted', 'Neither record has linked documents.'),
        selectizeInput(ns('merged_approval_type_id'), 'Approval level for the merged record', choices = approval_choices, selected = as.character(left$approval_type_id), multiple = FALSE),
        selectizeInput(
          ns('keep_record'), 'Records to keep',
          choices = stats::setNames(as.character(records$borehole_id), paste0(ifelse(is.na(records$borehole_name), 'Unnamed borehole', as.character(records$borehole_name)), ' (ID ', records$borehole_id, ')')),
          selected = as.character(records$borehole_id), multiple = TRUE,
          options = list(plugins = list('remove_button'))
        ),
        p(class = 'text-muted', 'The first selected record is the merge destination; information from the compared records is retained there.'),
        actionButton(ns('review_merge'), 'Review merge', icon = icon('code-merge'), class = 'btn-danger'),
        actionButton(ns('review_unpair'), 'Review unpair', icon = icon('unlink'), class = 'btn-secondary')
      )
    })

    output$document_comparer <- renderUI({
      records <- selected_records()
      req(!is.null(records))
      left_pane <- ns('left_document_pane')
      right_pane <- ns('right_document_pane')
      tagList(
        tags$script(HTML(sprintf("(function() { var left = document.getElementById('%s'); var right = document.getElementById('%s'); if (!left || !right || left.dataset.syncReady) return; function sync(source, target) { if (source.dataset.syncing === '1') return; target.dataset.syncing = '1'; target.scrollTop = source.scrollTop; target.scrollLeft = source.scrollLeft; window.setTimeout(function() { target.dataset.syncing = '0'; }, 0); } left.addEventListener('scroll', function() { sync(left, right); }); right.addEventListener('scroll', function() { sync(right, left); }); left.dataset.syncReady = '1'; })();", left_pane, right_pane))),
        fluidRow(
          column(6, document_viewer(input$left_document_id, paste0('Documents for ', display_value(records$borehole_name[[1]])), left_pane)),
          column(6, document_viewer(input$right_document_id, paste0('Documents for ', display_value(records$borehole_name[[2]])), right_pane))
        )
      )
    })

    output$documents <- DT::renderDT({
      docs <- selected_documents()
      DT::datatable(
        docs,
        rownames = FALSE,
        selection = 'single',
        options = list(dom = 't', scrollX = TRUE)
      )
    })

    selected_document_id <- reactive({
      row <- input$documents_rows_selected
      docs <- selected_documents()
      req(length(row) == 1L, nrow(docs) >= row)
      as.integer(docs$document_id[[row]])
    })

    output$download_document <- downloadHandler(
      filename = function() {
        doc <- DBI::dbGetQuery(
          con,
          'SELECT name, format FROM files.documents WHERE document_id = $1;',
          params = list(selected_document_id())
        )
        req(nrow(doc) == 1L)
        paste0(gsub('[^A-Za-z0-9_-]', '_', doc$name[[1]]), '.', doc$format[[1]])
      },
      content = function(file) {
        doc <- DBI::dbGetQuery(
          con,
          'SELECT document FROM files.documents WHERE document_id = $1;',
          params = list(selected_document_id())
        )
        req(nrow(doc) == 1L)
        writeBin(doc$document[[1]], file)
      }
    )

    output$well_merge_choices <- renderUI({
      wells <- selected_wells()
      if (!nrow(wells)) {
        return(p(class = "text-muted", "Neither borehole has associated wells."))
      }
      approval_choices <- stats::setNames(
        state$approvals$approval_type_id,
        state$approvals$approval_type_description
      )
      cards <- vector("list", nrow(wells))
      for (i in seq_len(nrow(wells))) {
        well <- wells[i, , drop = FALSE]
        well_id <- well$well_id[[1]]
        cards[[i]] <- tags$div(
          class = "card mb-3 p-3",
          tags$h5(paste0("Well ", display_value(well$well_name), " (well ID ", well_id, "; borehole ID ", well$borehole_id[[1]], ")")),
          checkboxInput(ns(paste0("keep_well_", well_id)), "Keep this well on the merged borehole", TRUE),
          fluidRow(
            column(4, textInput(ns(paste0("well_name_", well_id)), "Well name", value = ifelse(is.na(well$well_name[[1]]), "", well$well_name[[1]]))),
            column(4, selectizeInput(ns(paste0("well_approval_", well_id)), "Approval level", choices = approval_choices, selected = as.character(well$approval_type_id[[1]]), multiple = FALSE)),
            column(4, numericInput(ns(paste0("well_purpose_", well_id)), "Purpose ID", value = well$well_purpose_id[[1]]))
          ),
          fluidRow(
            column(3, numericInput(ns(paste0("well_casing_diameter_", well_id)), "Casing diameter (mm)", value = well$casing_diameter_mm[[1]])),
            column(3, numericInput(ns(paste0("well_casing_depth_", well_id)), "Casing depth (m)", value = well$casing_depth_to_m[[1]])),
            column(3, numericInput(ns(paste0("well_screen_top_", well_id)), "Screen top (m)", value = well$screen_top_depth_m[[1]])),
            column(3, numericInput(ns(paste0("well_screen_bottom_", well_id)), "Screen bottom (m)", value = well$screen_bottom_depth_m[[1]]))
          ),
          fluidRow(
            column(4, numericInput(ns(paste0("well_static_water_", well_id)), "Static water level (m)", value = well$static_water_level_m[[1]])),
            column(4, numericInput(ns(paste0("well_yield_", well_id)), "Estimated yield (L/s)", value = well$estimated_yield_lps[[1]])),
            column(4, textAreaInput(ns(paste0("well_notes_", well_id)), "Notes", value = ifelse(is.na(well$notes[[1]]), "", well$notes[[1]]), rows = 2))
          )
        )
      }
      do.call(tagList, cards)
    })

    output$unpair_well_editors <- renderUI({
      req(isTRUE(state$unpair_mode))
      wells <- selected_wells()
      if (!nrow(wells)) return(p(class = 'text-muted', 'Neither borehole has associated wells.'))
      approval_choices <- stats::setNames(state$approvals$approval_type_id, state$approvals$approval_type_description)
      purpose_choices <- if (is.null(state$purposes)) character() else stats::setNames(state$purposes$borehole_well_purpose_id, state$purposes$purpose_name)
      cards <- lapply(seq_len(nrow(wells)), function(i) {
        well <- wells[i, , drop = FALSE]
        well_id <- well$well_id[[1]]
        tagList(
          tags$hr(), h5(paste0('Edit well ', well_id, ' on borehole ', well$borehole_id[[1]])),
          textInput(ns(paste0('unpair_well_name_', well_id)), 'Well name', value = ifelse(is.na(well$well_name[[1]]), '', well$well_name[[1]])),
          selectizeInput(ns(paste0('unpair_well_approval_', well_id)), 'Approval level', choices = approval_choices, selected = as.character(well$approval_type_id[[1]]), multiple = FALSE),
          selectizeInput(ns(paste0('unpair_well_purpose_', well_id)), 'Well purpose', choices = purpose_choices, selected = as.character(well$well_purpose_id[[1]]), multiple = FALSE),
          numericInput(ns(paste0('unpair_well_casing_diameter_', well_id)), 'Casing diameter (mm)', value = well$casing_diameter_mm[[1]]),
          numericInput(ns(paste0('unpair_well_casing_depth_', well_id)), 'Casing depth (m)', value = well$casing_depth_to_m[[1]]),
          numericInput(ns(paste0('unpair_well_screen_top_', well_id)), 'Screen top (m)', value = well$screen_top_depth_m[[1]]),
          numericInput(ns(paste0('unpair_well_screen_bottom_', well_id)), 'Screen bottom (m)', value = well$screen_bottom_depth_m[[1]]),
          numericInput(ns(paste0('unpair_well_static_water_', well_id)), 'Static water level (m)', value = well$static_water_level_m[[1]]),
          numericInput(ns(paste0('unpair_well_yield_', well_id)), 'Estimated yield (L/s)', value = well$estimated_yield_lps[[1]]),
          textAreaInput(ns(paste0('unpair_well_notes_', well_id)), 'Notes', value = ifelse(is.na(well$notes[[1]]), '', well$notes[[1]]), rows = 2)
        )
      })
      do.call(tagList, c(list(h4('Edit associated wells')), cards))
    })

    observeEvent(input$review_merge, {
      records <- selected_records()
      req(!is.null(records))
      keep <- primary_record(records)
      remove <- records[records$borehole_id != keep$borehole_id[[1]], , drop = FALSE]
      wells <- selected_wells()
      selected_documents_text <- input$keep_documents
      showModal(modalDialog(
        title = 'Review merge',
        tagList(
          p(paste0('Borehole ', remove$borehole_id, ' will be merged into borehole ', keep$borehole_id, '. The source borehole record will be deleted.')),
          tags$hr(), h5('Borehole fields'),
          tags$ul(lapply(names(fields), function(field) tags$li(paste0(fields[[field]], ': ', display_value(input[[paste0('merged_', field)]]))))),
          h5('Associated wells'),
          if (nrow(wells)) tags$ul(lapply(seq_len(nrow(wells)), function(i) tags$li(paste0('Well ', wells$well_id[[i]], ' will be retained and associated with borehole ', keep$borehole_id)))) else p('No associated wells.'),
          h5('Documents'),
          if (length(selected_documents_text)) p(paste('Selected document links retained:', paste(selected_documents_text, collapse = ', '))) else p('No document links selected.'),
          p('No wells are scheduled for deletion.')
        ),
        footer = tagList(modalButton('Cancel'), actionButton(ns('confirm_merge'), 'Merge records', class = 'btn-danger')),
        easyClose = FALSE
      ))
    })

    chosen_value <- function(field, records) {
      value <- input[[paste0('merged_', field)]]
      if (is.null(value)) records[[field]][[1]] else value
    }

    primary_record <- function(records) {
      selected_ids <- suppressWarnings(as.integer(input$keep_record))
      selected_ids <- selected_ids[selected_ids %in% records$borehole_id]
      keep_id <- if (length(selected_ids)) selected_ids[[1]] else records$borehole_id[[1]]
      records[match(keep_id, records$borehole_id), , drop = FALSE]
    }

    copy_field_value <- function(field, value_index) {
      records <- selected_records()
      req(!is.null(records))
      value <- records[[field]][[value_index]]
      input_id <- paste0('merged_', field)
      if (field %in% c('drilled_by', 'borehole_purpose_id')) {
        updateSelectizeInput(session, input_id, selected = as.character(value))
      } else if (field %in% numeric_fields) {
        updateNumericInput(session, input_id, value = suppressWarnings(as.numeric(value)))
      } else if (field %in% date_fields) {
        updateDateInput(session, input_id, value = input_date_value(value))
      } else {
        updateTextAreaInput(session, input_id, value = if (is.na(value)) '' else as.character(value))
      }
    }

    select_field_side <- function(field, side) {
      selected_sides <- state$selected_sides
      selected_sides[[field]] <- side
      state$selected_sides <- selected_sides
      copy_field_value(field, if (identical(side, 'right')) 2L else 1L)
    }

    lapply(names(comparison_fields), function(field) {
      observeEvent(input[[paste0('use_left_', field)]], {
        select_field_side(field, 'left')
      }, ignoreInit = TRUE)
      observeEvent(input[[paste0('use_right_', field)]], {
        select_field_side(field, 'right')
      }, ignoreInit = TRUE)
    })

    observeEvent(input$select_all_left, {
      for (field in names(comparison_fields)) {
        select_field_side(field, 'left')
      }
    })
    observeEvent(input$select_all_right, {
      for (field in names(comparison_fields)) {
        select_field_side(field, 'right')
      }
    })

    observeEvent(input$unpair_wells, {
      state$unpair_mode <- !isTRUE(state$unpair_mode)
      state$selected_sides <- list()
    })

    unpair_borehole_value <- function(field, side, record) {
      value <- input[[paste0('unpair_', side, '_', field)]]
      if (is.null(value)) record[[field]][[1]] else value
    }

    observeEvent(input$review_unpair, {
      if (!isTRUE(state$unpair_mode)) {
        showNotification('Select Unpair wells to edit the two records independently before reviewing.', type = 'warning')
        return()
      }
      records <- selected_records()
      req(!is.null(records))
      changes <- unlist(lapply(seq_len(nrow(records)), function(i) {
        side <- if (i == 1L) 'left' else 'right'
        vapply(names(fields), function(field) {
          old <- display_value(records[[field]][[i]])
          new <- display_value(unpair_borehole_value(field, side, records[i, , drop = FALSE]))
          if (identical(old, new)) NA_character_ else paste0('Borehole ', records$borehole_id[[i]], ': ', fields[[field]], ' — ', old, ' → ', new)
        }, character(1))
      }), use.names = FALSE)
      changes <- changes[!is.na(changes)]
      wells <- selected_wells()
      showModal(modalDialog(
        title = 'Review unpair',
        tagList(
          p('Both borehole records and all associated wells will remain. The direct edits below will be applied.'),
          h5('Borehole changes'),
          if (length(changes)) tags$ul(lapply(changes, tags$li)) else p('No borehole field changes.'),
          h5('Associated wells'),
          if (nrow(wells)) tags$ul(lapply(wells$well_id, function(id) tags$li(paste0('Well ', id, ' remains with its current borehole and will use any direct edits.')))) else p('No associated wells.'),
          p('No boreholes, wells, or documents will be deleted.')
        ),
        footer = tagList(modalButton('Cancel'), actionButton(ns('confirm_unpair'), 'Apply unpair changes', class = 'btn-primary')),
        easyClose = FALSE
      ))
    })

    observeEvent(input$confirm_unpair, {
      records <- selected_records()
      req(!is.null(records), isTRUE(state$unpair_mode))
      wells <- selected_wells()
      tryCatch({
        DBI::dbWithTransaction(con, {
          for (i in seq_len(nrow(records))) {
            side <- if (i == 1L) 'left' else 'right'
            values <- lapply(names(fields), unpair_borehole_value, side = side, record = records[i, , drop = FALSE])
            names(values) <- names(fields)
            for (field in numeric_fields) values[[field]] <- suppressWarnings(as.numeric(values[[field]]))
            values[['completion_date']] <- suppressWarnings(as.Date(values[['completion_date']]))
            for (field in names(fields)) {
              DBI::dbExecute(con, paste0('UPDATE boreholes.boreholes SET ', field, ' = $1 WHERE borehole_id = $2;'), params = list(values[[field]], records$borehole_id[[i]]))
            }
          }
          for (i in seq_len(nrow(wells))) {
            well <- wells[i, , drop = FALSE]
            well_id <- well$well_id[[1]]
            numeric_input <- function(name) suppressWarnings(as.numeric(input[[paste0('unpair_', name, well_id)]]))
            DBI::dbExecute(
              con,
              'UPDATE boreholes.wells SET well_name = $1, approval_type_id = $2, well_purpose_id = $3, casing_diameter_mm = $4, casing_depth_to_m = $5, screen_top_depth_m = $6, screen_bottom_depth_m = $7, static_water_level_m = $8, estimated_yield_lps = $9, notes = $10 WHERE well_id = $11;',
              params = list(
                input[[paste0('unpair_well_name_', well_id)]], suppressWarnings(as.integer(input[[paste0('unpair_well_approval_', well_id)]])), suppressWarnings(as.integer(input[[paste0('unpair_well_purpose_', well_id)]])),
                numeric_input('well_casing_diameter_'), numeric_input('well_casing_depth_'), numeric_input('well_screen_top_'), numeric_input('well_screen_bottom_'), numeric_input('well_static_water_'), numeric_input('well_yield_'), input[[paste0('unpair_well_notes_', well_id)]], well_id
              )
            )
          }
        })
        removeModal()
        showNotification('Unpair changes applied.', type = 'message')
        statuses <- state$candidate_status
        for (borehole_id in records$borehole_id) {
          statuses[[as.character(borehole_id)]] <- 'Reviewed — not merged'
        }
        state$candidate_status <- statuses
        state$comparison_ids <- integer()
      }, error = function(error) {
        showNotification(paste('The unpair changes were not applied:', conditionMessage(error)), type = 'error', duration = NULL)
      })
    })

    observeEvent(input$confirm_merge, {
      records <- selected_records()
      req(!is.null(records))
      keep_id <- primary_record(records)$borehole_id[[1]]
      remove_id <- records$borehole_id[records$borehole_id != keep_id][[1]]
      values <- lapply(names(fields), chosen_value, records = records)
      names(values) <- names(fields)
      for (field in numeric_fields) values[[field]] <- suppressWarnings(as.numeric(values[[field]]))
      values[['completion_date']] <- suppressWarnings(as.Date(values[['completion_date']]))
      values[['approval_type_id']] <- suppressWarnings(as.integer(input$merged_approval_type_id))
      keep_docs <- suppressWarnings(as.integer(input$keep_documents))
      keep_docs <- keep_docs[!is.na(keep_docs)]

      tryCatch({
        DBI::dbWithTransaction(con, {
          for (field in c(names(fields), 'approval_type_id')) {
            DBI::dbExecute(con, paste0('UPDATE boreholes.boreholes SET ', field, ' = $1 WHERE borehole_id = $2;'), params = list(values[[field]], keep_id))
          }
          if (length(keep_docs)) {
            for (document_id in keep_docs) {
              DBI::dbExecute(con, 'INSERT INTO boreholes.boreholes_documents (borehole_id, document_id) VALUES ($1, $2) ON CONFLICT DO NOTHING;', params = list(keep_id, document_id))
            }
          }
          wells <- selected_wells()
          if (nrow(wells)) {
            for (i in seq_len(nrow(wells))) {
              well <- wells[i, , drop = FALSE]
              well_id <- well$well_id[[1]]
              DBI::dbExecute(
                con,
                'UPDATE boreholes.wells SET borehole_id = $1 WHERE well_id = $2;',
                params = list(keep_id, well_id)
              )
            }
          }
          DBI::dbExecute(con, 'UPDATE boreholes.permafrost SET borehole_id = $1 WHERE borehole_id = $2;', params = list(keep_id, remove_id))
          DBI::dbExecute(con, 'DELETE FROM boreholes.boreholes_documents WHERE borehole_id = $1;', params = list(remove_id))
          DBI::dbExecute(con, 'DELETE FROM boreholes.boreholes WHERE borehole_id = $1;', params = list(remove_id))
        })
        removeModal()
        showNotification('Borehole records merged.', type = 'message')
        statuses <- state$candidate_status
        statuses[[as.character(keep_id)]] <- paste0('Merged record (ID ', keep_id, ')')
        statuses[[as.character(remove_id)]] <- paste0('Merged into ID ', keep_id)
        state$candidate_status <- statuses
        state$comparison_ids <- integer()
        state$pending_ids <- integer()
      }, error = function(error) {
        showNotification(paste('The merge was not applied:', conditionMessage(error)), type = 'error', duration = NULL)
      })
    })
  })
}
