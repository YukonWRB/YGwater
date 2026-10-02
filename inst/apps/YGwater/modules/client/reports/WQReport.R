WQReportUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("banner")),
    # Custom CSS below is for consistency with the look elsewhere in the app.
    tags$head(tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "css/card_background.css"
    )),
    uiOutput(ns("info")), # Information about this module, rendered in the same manner as 'banner'
    card(
      card_body(
        class = "custom-card",
        uiOutput(ns("main"))
      ) # End card_body
    ) # End card
  ) # End tagList
} # End WQReportUI


WQReport <- function(id, mdb_files, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    configured_mdb_files <- unique(as.character(mdb_files))
    configured_mdb_files <- configured_mdb_files[
      !is.na(configured_mdb_files) &
        nzchar(configured_mdb_files) &
        file.exists(configured_mdb_files)
    ]
    eqwin_available <- length(configured_mdb_files) > 0L

    selected_data_source <- reactive({
      if (!eqwin_available) {
        return("AC")
      }
      value <- input$data_source
      if (length(value) != 1L || is.na(value) || !value %in% c("AC", "EQ")) {
        return("EQ")
      }
      value
    })

    saved_inputs <- reactiveValues()
    preserved_input_ids <- c(
      "data_source",
      "date",
      "date_to_add_AC",
      "dates_AC",
      "format_AC",
      "date_approx_ac",
      "date_approx_eq",
      "date_approx_mode_AC",
      "matrix_states_AC",
      "sample_fractions_AC",
      "EQWin_source",
      "locs_groups",
      "locations_EQ",
      "location_groups",
      "params_groups",
      "parameters_EQ",
      "parameter_groups",
      "stds",
      "stnStds",
      "locations_AC",
      "parameters_AC",
      "guidelines_AC",
      "SD_SD",
      "SD_start",
      "SD_end",
      "SD_date_range"
    )
    for (input_id in preserved_input_ids) {
      local({
        id <- input_id
        observeEvent(
          input[[id]],
          {
            saved_inputs[[id]] <- input[[id]]
          },
          ignoreInit = TRUE
        )
      })
    }
    saved_input <- function(id, default = NULL) {
      value <- isolate(saved_inputs[[id]])
      if (is.null(value)) default else value
    }

    tooltip_label <- function(label, key, lang) {
      help_text <- tr(key, lang)
      tags$span(
        label,
        tags$span(
          class = "wq-report-tooltip",
          `data-bs-toggle` = "tooltip",
          `data-bs-placement` = "right",
          `data-bs-trigger` = "hover focus",
          title = help_text,
          `aria-label` = help_text,
          tabindex = "0",
          icon(
            "info-circle",
            style = "font-size: 100%; margin-left: 5px; cursor: help;"
          )
        )
      )
    }

    resolve_eqwin_source <- function(value) {
      if (
        length(value) != 1L ||
          is.na(value) ||
          !nzchar(value) ||
          !value %in% configured_mdb_files
      ) {
        stop(
          tr("wq_err_select_configured_eqwin", language$language),
          call. = FALSE
        )
      }
      normalizePath(
        configured_mdb_files[match(value, configured_mdb_files)],
        winslash = "/",
        mustWork = TRUE
      )
    }

    moduleData <- reactiveValues()
    ac_metadata_loaded <- reactiveVal(FALSE)

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "WQReport"
      )
    })

    output$info <- renderUI({
      req(language$language)
      text <- HTML(tr("gen_wqReport_info", language$language))
      dismissible_banner_ui(
        ns = ns,
        msg_text = text,
        banner_id = "wqReport_info",
        banner_key_prefix = "wqReport_info"
      )
    })

    output$main <- renderUI({
      req(language$language)
      lang <- language$language
      locations <- if (isTRUE(ac_metadata_loaded())) {
        moduleData$AC_locs
      } else {
        NULL
      }
      location_choices <- if (!is.null(locations) && nrow(locations)) {
        location_names <- if (identical(lang, "Français")) {
          locations$name_fr
        } else {
          locations$name
        }
        missing_names <- is.na(location_names) | !nzchar(location_names)
        location_names[missing_names] <- locations$location_code[missing_names]
        stats::setNames(
          as.character(locations$location_id),
          paste0(locations$location_code, " (", location_names, ")")
        )
      } else {
        character()
      }
      selected_dates <- saved_input(
        "dates_AC",
        format(Sys.Date() - 30, "%Y-%m-%d")
      )
      tagList(
        uiOutput(ns("data_source_ui")),
        conditionalPanel(
          ns = ns,
          condition = "input.data_source == 'EQ'",
          dateInput(
            ns("date"),
            tooltip_label(
              tr("report_date", lang),
              "wq_tooltip_report_date_eq",
              lang
            ),
            value = saved_input("date", Sys.Date() - 30),
            language = language$abbrev
          ),
          numericInput(
            ns("date_approx_eq"),
            tooltip_label(
              tr("wq_date_match_window", lang),
              "wq_tooltip_date_match_window",
              lang
            ),
            value = saved_input("date_approx_eq", 1),
            min = 0,
            step = 1
          )
        ),
        conditionalPanel(
          ns = ns,
          condition = "input.data_source == 'AC' || input.data_source == null",
          dateInput(
            ns("date_to_add_AC"),
            tooltip_label(
              tr("report_date", lang),
              "wq_tooltip_report_date_ac",
              lang
            ),
            value = saved_input("date_to_add_AC", Sys.Date() - 30),
            format = "yyyy-mm-dd",
            language = language$abbrev
          ),
          actionButton(
            ns("add_date_AC"),
            tooltip_label(
              tr("wq_add_report_date", lang),
              "wq_tooltip_add_report_date",
              lang
            ),
            icon = icon("plus")
          ),
          selectizeInput(
            ns("dates_AC"),
            tooltip_label(
              tr("wq_report_dates", lang),
              "wq_tooltip_report_dates",
              lang
            ),
            choices = stats::setNames(selected_dates, selected_dates),
            selected = selected_dates,
            multiple = TRUE,
            width = "100%"
          ),
          selectInput(
            ns("date_approx_mode_AC"),
            tooltip_label(
              tr("wq_date_approx_mode", lang),
              "wq_tooltip_date_approx_mode",
              lang
            ),
            choices = stats::setNames(
              c("shared", "per_date"),
              c(
                tr("wq_date_approx_shared", lang),
                tr("wq_date_approx_per_date", lang)
              )
            ),
            selected = saved_input("date_approx_mode_AC", "shared"),
            width = "100%"
          ),
          uiOutput(ns("date_approx_ac_ui")),
          selectizeInput(
            ns("format_AC"),
            tooltip_label(
              tr("workbook_layout", lang),
              "wq_tooltip_workbook_layout",
              lang
            ),
            choices = stats::setNames(
              c("by_date", "by_location", "by_parameter"),
              c(
                tr("wq_workbook_by_date", lang),
                tr("wq_workbook_by_location", lang),
                tr("wq_workbook_by_parameter", lang)
              )
            ),
            selected = saved_input("format_AC", "by_date"),
            multiple = FALSE,
            width = "100%"
          )
        ),
        conditionalPanel(
          ns = ns,
          condition = "input.data_source == 'EQ'",
          uiOutput(ns("EQWin_source_ui")),
          radioButtons(
            ns("locs_groups"),
            NULL,
            choices = stats::setNames(
              c("Locations", "Location Groups"),
              c(tr("locs", lang), tr("location_groups", lang))
            ),
            selected = saved_input("locs_groups", "Locations")
          ),
          selectizeInput(
            ns("locations_EQ"),
            tooltip_label(
              tr("select_locs", lang),
              "wq_tooltip_locations",
              lang
            ),
            choices = character(),
            selected = saved_input("locations_EQ", character()),
            multiple = TRUE,
            width = "100%"
          ),
          selectizeInput(
            ns("location_groups"),
            tr("select_loc_group", lang),
            choices = character(),
            selected = saved_input("location_groups"),
            multiple = FALSE,
            width = "100%"
          ),
          radioButtons(
            ns("params_groups"),
            NULL,
            choices = stats::setNames(
              c("Parameters", "Parameter Groups"),
              c(tr("parameters", lang), tr("parameter_groups", lang))
            ),
            selected = saved_input("params_groups", "Parameters")
          ),
          selectizeInput(
            ns("parameters_EQ"),
            tooltip_label(
              tr("select_params", lang),
              "wq_tooltip_parameters",
              lang
            ),
            choices = character(),
            selected = saved_input("parameters_EQ", character()),
            multiple = TRUE,
            width = "100%"
          ),
          selectizeInput(
            ns("parameter_groups"),
            tr("select_param_group", lang),
            choices = character(),
            selected = saved_input("parameter_groups"),
            multiple = FALSE,
            width = "100%"
          ),
          tags$br(),
          htmlOutput(ns("standard_note")),
          selectizeInput(
            ns("stds"),
            tr("select_standard_opt", lang),
            choices = character(),
            selected = saved_input("stds", character()),
            multiple = TRUE,
            width = "100%"
          ),
          checkboxInput(
            ns("stnStds"),
            tr("wq_station_standards", lang),
            value = saved_input("stnStds", FALSE)
          )
        ),
        conditionalPanel(
          ns = ns,
          condition = "input.data_source == 'AC' || input.data_source == null",
          selectizeInput(
            ns("locations_AC"),
            tooltip_label(
              tr("select_locs", lang),
              "wq_tooltip_locations",
              lang
            ),
            choices = location_choices,
            selected = saved_input("locations_AC", character()),
            multiple = TRUE,
            width = "100%"
          ),
          uiOutput(ns("AC_selectors_ui")),
          uiOutput(ns("AC_guidelines_ui"))
        ),
        uiOutput(ns("SD_inputs_ui")),
        bslib::input_task_button(
          ns("go"),
          tr("create_report", lang),
          label_busy = tr("generating_working", lang)
        ),
        downloadButton(
          ns("download"),
          tr("download_button", lang),
          style = "visibility: hidden;"
        ) # Hidden; triggered automatically if 'go' is successful
      )
    }) %>%
      bindEvent(language$language, ac_metadata_loaded())

    output$data_source_ui <- renderUI({
      if (!eqwin_available) {
        return(NULL)
      }
      req(language$language)
      radioButtons(
        ns("data_source"),
        tooltip_label(
          tr("data_source", language$language),
          "wq_tooltip_data_source",
          language$language
        ),
        choices = stats::setNames(
          c("AC", "EQ"),
          c(
            tr("aquacache", language$language),
            tr("EQWin_db", language$language)
          )
        ),
        selected = saved_input("data_source", "EQ")
      )
    }) %>%
      bindEvent(language$language)

    output$EQWin_source_ui <- renderUI({
      if (!eqwin_available) {
        return(NULL)
      }
      req(language$language)
      selectizeInput(
        ns("EQWin_source"),
        tooltip_label(
          tr("EQWin_db", language$language),
          "wq_tooltip_eqwin_source",
          language$language
        ),
        choices = stats::setNames(
          configured_mdb_files,
          basename(configured_mdb_files)
        ),
        selected = saved_input("EQWin_source", configured_mdb_files[[1]])
      )
    }) %>%
      bindEvent(language$language)

    output$SD_inputs_ui <- renderUI({
      req(language$language)
      tagList(
        tags$br(),
        htmlOutput(ns("SD_note")),
        tags$label(
          tr("wq_sd_threshold", language$language),
          class = "form-label"
        ),
        numericInput(ns("SD_SD"), NULL, value = saved_input("SD_SD")),
        tags$label(tr("wq_sd_start", language$language), class = "form-label"),
        dateInput(
          ns("SD_start"),
          NULL,
          value = saved_input("SD_start", NA),
          language = language$abbrev
        ),
        tags$label(tr("wq_sd_end", language$language), class = "form-label"),
        dateInput(
          ns("SD_end"),
          NULL,
          value = saved_input("SD_end", NA),
          language = language$abbrev
        ),
        tags$label(
          tr("wq_sd_day_range", language$language),
          class = "form-label"
        ),
        dateRangeInput(
          ns("SD_date_range"),
          label = NULL,
          start = saved_input(
            "SD_date_range",
            as.Date(c("2000-01-01", "2000-12-31"))
          )[[1]],
          end = saved_input(
            "SD_date_range",
            as.Date(c("2000-01-01", "2000-12-31"))
          )[[2]],
          format = "yyyy-mm-dd",
          language = language$abbrev,
          separator = tr("date_sep", language$language)
        )
      )
    }) %>%
      bindEvent(language$language)

    output$standard_note <- renderUI({
      req(language$language)
      HTML(tr("wq_standard_help", language$language))
    }) %>%
      bindEvent(language$language)
    output$SD_note <- renderUI({
      req(language$language)
      HTML(tr("wq_sd_help", language$language))
    }) %>%
      bindEvent(language$language)

    observeEvent(
      selected_data_source(),
      {
        if (
          !identical(selected_data_source(), "AC") ||
            isTRUE(ac_metadata_loaded())
        ) {
          return()
        }

        con <- session$userData$AquaCache
        tryCatch(
          {
            moduleData$AC_locs <- DBI::dbGetQuery(
              con,
              paste(
                "SELECT DISTINCT l.location_id, l.location_code, l.name,",
                "COALESCE(l.name_fr, l.name, l.location_code) AS name_fr",
                "FROM public.locations AS l",
                "INNER JOIN discrete.samples AS s ON s.location_id = l.location_id",
                "INNER JOIN discrete.results AS r ON r.sample_id = s.sample_id",
                "ORDER BY l.location_code"
              )
            )
            moduleData$AC_params <- DBI::dbGetQuery(
              con,
              paste(
                "SELECT p.parameter_id, p.param_name,",
                "COALESCE(p.param_name_fr, p.param_name) AS param_name_fr",
                "FROM public.parameters AS p",
                "WHERE EXISTS (",
                "SELECT 1 FROM discrete.results AS r",
                "WHERE r.parameter_id = p.parameter_id",
                ")",
                "ORDER BY p.param_name"
              )
            )
            moduleData$AC_matrix_states <- DBI::dbGetQuery(
              con,
              paste(
                "SELECT DISTINCT ms.matrix_state_id, ms.matrix_state_name",
                "FROM discrete.results AS r",
                "INNER JOIN public.matrix_states AS ms",
                "ON ms.matrix_state_id = r.matrix_state_id",
                "ORDER BY ms.matrix_state_name"
              )
            )
            moduleData$AC_sample_fractions <- DBI::dbGetQuery(
              con,
              paste(
                "SELECT DISTINCT sf.sample_fraction_id, sf.sample_fraction",
                "FROM discrete.results AS r",
                "INNER JOIN discrete.sample_fractions AS sf",
                "ON sf.sample_fraction_id = r.sample_fraction_id",
                "ORDER BY sf.sample_fraction"
              )
            )
            ac_metadata_loaded(TRUE)
          },
          error = function(e) {
            showNotification(
              paste(
                tr("wq_err_load_aquacache_choices", language$language),
                e$message
              ),
              type = "error",
              duration = NULL,
              closeButton = TRUE
            )
          }
        )
      },
      ignoreNULL = FALSE
    )

    observeEvent(
      input$add_date_AC,
      {
        selected_dates <- as.character(input$dates_AC)
        new_date <- suppressWarnings(as.Date(input$date_to_add_AC))
        if (length(new_date) != 1L || is.na(new_date)) {
          return()
        }
        selected_dates <- unique(c(
          selected_dates,
          format(new_date, "%Y-%m-%d")
        ))
        updateSelectizeInput(
          session,
          "dates_AC",
          choices = stats::setNames(selected_dates, selected_dates),
          selected = selected_dates,
          server = FALSE
        )
      },
      ignoreInit = TRUE
    )

    ac_selector_choices <- reactive({
      empty <- list(
        parameters = integer(),
        matrix_states = integer(),
        sample_fractions = integer()
      )
      if (!identical(selected_data_source(), "AC") || !ac_metadata_loaded()) {
        return(NULL)
      }
      dates <- suppressWarnings(as.Date(input$dates_AC))
      locations <- suppressWarnings(as.integer(input$locations_AC))
      if (
        !length(dates) ||
          anyNA(dates) ||
          anyDuplicated(dates) ||
          !length(locations) ||
          anyNA(locations)
      ) {
        return(NULL)
      }

      tolerances <- if (identical(input$date_approx_mode_AC, "per_date")) {
        date_ids <- format(dates, "%Y%m%d")
        vapply(
          date_ids,
          function(date_id) {
            value <- input[[paste0("date_approx_", date_id)]]
            if (is.null(value)) NA_real_ else suppressWarnings(as.numeric(value))
          },
          numeric(1)
        )
      } else {
        rep(suppressWarnings(as.numeric(input$date_approx_ac)), length(dates))
      }
      if (
        anyNA(tolerances) ||
          any(!is.finite(tolerances)) ||
          any(tolerances < 0) ||
          any(tolerances > .Machine$integer.max) ||
          any(tolerances != trunc(tolerances))
      ) {
        return(NULL)
      }

      date_json <- as.character(jsonlite::toJSON(
        data.frame(
          requested_date = format(dates, "%Y-%m-%d"),
          date_approx = as.integer(tolerances)
        ),
        dataframe = "rows",
        auto_unbox = TRUE
      ))
      location_json <- as.character(jsonlite::toJSON(
        as.character(unique(locations)),
        auto_unbox = FALSE
      ))
      sql <- paste0(
        "WITH date_requests AS (",
        " SELECT requested_date, date_approx",
        " FROM jsonb_to_recordset($1::jsonb)",
        " AS d(requested_date date, date_approx integer)",
        "), matching_results AS (",
        " SELECT DISTINCT r.parameter_id, r.matrix_state_id,",
        " r.sample_fraction_id",
        " FROM date_requests d",
        " JOIN discrete.samples s ON s.datetime::date BETWEEN",
        " d.requested_date - d.date_approx AND d.requested_date + d.date_approx",
        " JOIN discrete.results r ON r.sample_id = s.sample_id",
        " LEFT JOIN discrete.sample_types st ON st.sample_type_id = s.sample_type",
        " WHERE s.location_id IN (SELECT value::integer FROM",
        " jsonb_array_elements_text($2::jsonb))",
        " AND (st.sample_type IS NULL OR st.sample_type !~* 'blank')",
        ") SELECT DISTINCT parameter_id, matrix_state_id, sample_fraction_id",
        " FROM matching_results"
      )
      tryCatch(
        {
          available <- DBI::dbGetQuery(
            session$userData$AquaCache,
            sql,
            params = list(date_json, location_json)
          )
          if (!nrow(available)) {
            empty
          } else {
            list(
              parameters = unique(as.integer(
                available$parameter_id[!is.na(available$parameter_id)]
              )),
              matrix_states = unique(as.integer(
                available$matrix_state_id[!is.na(available$matrix_state_id)]
              )),
              sample_fractions = unique(as.integer(
                available$sample_fraction_id[!is.na(available$sample_fraction_id)]
              ))
            )
          }
        },
        error = function(e) {
          showNotification(
            paste(
              tr("wq_err_load_aquacache_choices", language$language),
              e$message
            ),
            type = "error",
            duration = NULL,
            closeButton = TRUE
          )
          empty
        }
      )
    })

    output$AC_selectors_ui <- renderUI({
      req(language$language)
      choices <- ac_selector_choices()
      if (is.null(choices)) {
        return(NULL)
      }

      choices_from_metadata <- function(
        ids,
        metadata,
        id_column,
        label_column,
        fallback_column = NULL
      ) {
        if (!length(ids) || is.null(metadata) || !nrow(metadata)) {
          return(character())
        }
        keep <- as.character(metadata[[id_column]]) %in% as.character(ids)
        metadata <- metadata[keep, , drop = FALSE]
        labels <- as.character(metadata[[label_column]])
        if (!is.null(fallback_column)) {
          fallback <- as.character(metadata[[fallback_column]])
          missing <- is.na(labels) | !nzchar(labels)
          labels[missing] <- fallback[missing]
        }
        missing <- is.na(labels) | !nzchar(labels)
        labels[missing] <- as.character(metadata[[id_column]][missing])
        stats::setNames(as.character(metadata[[id_column]]), labels)
      }
      param_label_column <- if (identical(language$language, "Français")) {
        "param_name_fr"
      } else {
        "param_name"
      }
      parameter_choices <- choices_from_metadata(
        choices$parameters,
        moduleData$AC_params,
        "parameter_id",
        param_label_column,
        "param_name"
      )
      matrix_choices <- choices_from_metadata(
        choices$matrix_states,
        moduleData$AC_matrix_states,
        "matrix_state_id",
        "matrix_state_name"
      )
      fraction_choices <- choices_from_metadata(
        choices$sample_fractions,
        moduleData$AC_sample_fractions,
        "sample_fraction_id",
        "sample_fraction"
      )
      selected_input <- function(id) {
        value <- isolate(input[[id]])
        if (is.null(value)) saved_input(id, character()) else value
      }
      tagList(
        selectizeInput(
          ns("parameters_AC"),
          tooltip_label(
            tr("select_params", language$language),
            "wq_tooltip_parameters",
            language$language
          ),
          choices = parameter_choices,
          selected = intersect(
            as.character(selected_input("parameters_AC")),
            unname(parameter_choices)
          ),
          multiple = TRUE,
          width = "100%"
        ),
        selectizeInput(
          ns("matrix_states_AC"),
          tooltip_label(
            tr("wq_matrix_states", language$language),
            "wq_tooltip_matrix_states",
            language$language
          ),
          choices = matrix_choices,
          selected = intersect(
            as.character(selected_input("matrix_states_AC")),
            unname(matrix_choices)
          ),
          multiple = TRUE,
          width = "100%"
        ),
        selectizeInput(
          ns("sample_fractions_AC"),
          tooltip_label(
            tr("sample_fraction(s)", language$language),
            "wq_tooltip_sample_fractions",
            language$language
          ),
          choices = fraction_choices,
          selected = intersect(
            as.character(selected_input("sample_fractions_AC")),
            unname(fraction_choices)
          ),
          multiple = TRUE,
          width = "100%"
        )
      )
    })

    ac_guideline_choices <- reactive({
      empty <- data.frame(
        guideline_id = integer(),
        guideline_code = character(),
        guideline_name = character(),
        param_name = character(),
        publisher_name = character(),
        stringsAsFactors = FALSE
      )
      if (!identical(selected_data_source(), "AC") || !ac_metadata_loaded()) {
        return(empty)
      }
      dates <- suppressWarnings(as.Date(input$dates_AC))
      locations <- suppressWarnings(as.integer(input$locations_AC))
      parameters <- suppressWarnings(as.integer(input$parameters_AC))
      if (
        !length(dates) ||
          anyNA(dates) ||
          anyDuplicated(dates) ||
          !length(locations) ||
          anyNA(locations) ||
          !length(parameters) ||
          anyNA(parameters)
      ) {
        return(empty)
      }
      if (identical(input$date_approx_mode_AC, "per_date")) {
        date_ids <- format(dates, "%Y%m%d")
        tolerances <- vapply(
          date_ids,
          function(date_id) {
            value <- input[[paste0("date_approx_", date_id)]]
            if (is.null(value)) {
              NA_integer_
            } else {
              suppressWarnings(as.integer(value))
            }
          },
          integer(1)
        )
      } else {
        tolerances <- rep(
          suppressWarnings(as.integer(input$date_approx_ac)),
          length(dates)
        )
      }
      if (anyNA(tolerances) || any(tolerances < 0L)) {
        return(empty)
      }
      matrix_states <- suppressWarnings(as.integer(input$matrix_states_AC))
      sample_fractions <- suppressWarnings(as.integer(
        input$sample_fractions_AC
      ))
      matrix_states <- matrix_states[!is.na(matrix_states)]
      sample_fractions <- sample_fractions[!is.na(sample_fractions)]
      to_json <- function(ids) {
        as.character(jsonlite::toJSON(
          as.character(unique(ids)),
          auto_unbox = FALSE
        ))
      }
      date_json <- as.character(jsonlite::toJSON(
        data.frame(
          requested_date = format(dates, "%Y-%m-%d"),
          date_approx = tolerances
        ),
        dataframe = "rows",
        auto_unbox = TRUE
      ))
      sql <- paste0(
        "WITH date_requests AS (",
        " SELECT requested_date, date_approx",
        " FROM jsonb_to_recordset($1::jsonb)",
        " AS d(requested_date date, date_approx integer) ",
        "), candidates AS (",
        " SELECT d.requested_date, s.location_id, s.datetime::date AS sample_date",
        " FROM date_requests d JOIN discrete.samples s",
        " ON s.datetime::date BETWEEN d.requested_date - d.date_approx",
        " AND d.requested_date + d.date_approx",
        " LEFT JOIN discrete.sample_types st ON st.sample_type_id = s.sample_type",
        " WHERE s.location_id IN (SELECT value::integer FROM",
        " jsonb_array_elements_text($2::jsonb))",
        " AND (st.sample_type IS NULL OR st.sample_type !~* 'blank')",
        " AND EXISTS (SELECT 1 FROM discrete.results er",
        " WHERE er.sample_id = s.sample_id AND er.parameter_id IN (",
        " SELECT value::integer FROM jsonb_array_elements_text($3::jsonb))",
        " AND (jsonb_array_length($4::jsonb) = 0 OR er.matrix_state_id IN (",
        " SELECT value::integer FROM jsonb_array_elements_text($4::jsonb)))",
        " AND (jsonb_array_length($5::jsonb) = 0 OR er.sample_fraction_id IN (",
        " SELECT value::integer FROM jsonb_array_elements_text($5::jsonb))))",
        "), best_dates AS (",
        " SELECT DISTINCT ON (requested_date, location_id)",
        " requested_date, location_id, sample_date FROM candidates",
        " ORDER BY requested_date, location_id,",
        " abs(sample_date - requested_date),",
        " (sample_date >= requested_date) DESC, sample_date",
        "), selected_results AS (",
        " SELECT DISTINCT r.result_id, s.datetime::date AS sample_date",
        " FROM best_dates b JOIN discrete.samples s",
        " ON s.location_id = b.location_id AND s.datetime::date = b.sample_date",
        " JOIN discrete.results r ON r.sample_id = s.sample_id",
        " WHERE r.parameter_id IN (SELECT value::integer FROM",
        " jsonb_array_elements_text($3::jsonb))",
        " AND (jsonb_array_length($4::jsonb) = 0 OR r.matrix_state_id IN (",
        " SELECT value::integer FROM jsonb_array_elements_text($4::jsonb)))",
        " AND (jsonb_array_length($5::jsonb) = 0 OR r.sample_fraction_id IN (",
        " SELECT value::integer FROM jsonb_array_elements_text($5::jsonb)))",
        ") SELECT DISTINCT g.guideline_id, g.guideline_code, g.guideline_name,",
        " p.param_name, gp.publisher_name",
        " FROM selected_results sr",
        " CROSS JOIN LATERAL criteria.applicable_guideline_rules_for_result(",
        " sr.result_id, sr.sample_date, TRUE, FALSE) applicable",
        " JOIN criteria.guidelines g ON g.guideline_id = applicable.guideline_id",
        " JOIN public.parameters p ON p.parameter_id = g.parameter_id",
        " LEFT JOIN criteria.guideline_publishers gp ON gp.publisher_id = g.publisher_id",
        " WHERE g.active AND g.review_status = 'approved'",
        " AND g.parameter_id IN (SELECT value::integer FROM",
        " jsonb_array_elements_text($3::jsonb))",
        " AND (g.valid_from IS NULL OR sr.sample_date >= g.valid_from)",
        " AND (g.valid_to IS NULL OR sr.sample_date <= g.valid_to)",
        " ORDER BY p.param_name, g.guideline_code, g.guideline_name"
      )
      tryCatch(
        DBI::dbGetQuery(
          session$userData$AquaCache,
          sql,
          params = list(
            date_json,
            to_json(locations),
            to_json(parameters),
            to_json(matrix_states),
            to_json(sample_fractions)
          )
        ),
        error = function(e) empty
      )
    })

    output$AC_guidelines_ui <- renderUI({
      req(language$language)
      if (!identical(selected_data_source(), "AC")) {
        return(NULL)
      }
      if (!ac_metadata_loaded()) {
        return(tags$p(tr("wq_loading_guidelines", language$language)))
      }
      guidelines <- ac_guideline_choices()
      code <- ifelse(
        is.na(guidelines$guideline_code) | !nzchar(guidelines$guideline_code),
        "",
        paste0(guidelines$guideline_code, " - ")
      )
      labels <- paste0(
        code,
        guidelines$guideline_name,
        " (",
        guidelines$param_name,
        ")"
      )
      labels <- ifelse(
        is.na(guidelines$publisher_name) | !nzchar(guidelines$publisher_name),
        labels,
        paste0(labels, " | ", guidelines$publisher_name)
      )
      choices <- stats::setNames(as.character(guidelines$guideline_id), labels)
      selected <- intersect(as.character(input$guidelines_AC), unname(choices))
      selectizeInput(
        ns("guidelines_AC"),
        tooltip_label(
          tr("wq_select_guidelines", language$language),
          "wq_tooltip_guidelines",
          language$language
        ),
        choices = choices,
        selected = selected,
        multiple = TRUE,
        width = "100%"
      )
    })

    output$date_approx_ac_ui <- renderUI({
      dates <- suppressWarnings(as.Date(input$dates_AC))
      if (
        !identical(selected_data_source(), "AC") ||
          !length(dates) ||
          anyNA(dates)
      ) {
        return(NULL)
      }
      if (!identical(input$date_approx_mode_AC, "per_date")) {
        numericInput(
          ns("date_approx_ac"),
          tooltip_label(
            tr("wq_date_match_window", language$language),
            "wq_tooltip_date_match_window",
            language$language
          ),
          value = saved_input("date_approx_ac", 1),
          min = 0,
          step = 1
        )
      } else {
        date_ids <- format(dates, "%Y%m%d")
        date_labels <- format(dates, "%Y-%m-%d")
        tagList(lapply(seq_along(date_ids), function(i) {
          input_id <- paste0("date_approx_", date_ids[[i]])
          numericInput(
            ns(input_id),
            tooltip_label(
              sprintf(
                tr("wq_date_approx_for_date", language$language),
                date_labels[[i]]
              ),
              "wq_tooltip_date_match_window",
              language$language
            ),
            value = saved_input(input_id, 1),
            min = 0,
            step = 1
          )
        }))
      }
    })

    observe({
      dates <- suppressWarnings(as.Date(input$dates_AC))
      if (!length(dates) || anyNA(dates)) {
        return()
      }

      for (date_id in format(dates, "%Y%m%d")) {
        input_id <- paste0("date_approx_", date_id)
        value <- input[[input_id]]
        if (!is.null(value)) {
          saved_inputs[[input_id]] <- value
        }
      }
    })

    observeEvent(
      input$EQWin_source,
      {
        if (!eqwin_available || is.null(input$EQWin_source)) {
          return()
        }
        source <- tryCatch(
          resolve_eqwin_source(input$EQWin_source),
          error = function(e) {
            showNotification(
              paste(
                tr("wq_err_resolve_eqwin_source", language$language),
                e$message
              ),
              type = "error",
              duration = 8
            )
            NULL
          }
        )
        if (is.null(source) || identical(source, moduleData$EQ_loaded_source)) {
          return()
        }

        moduleData$EQ_locs <- NULL
        moduleData$EQ_loc_grps <- NULL
        moduleData$EQ_params <- NULL
        moduleData$EQ_param_grps <- NULL
        moduleData$EQ_stds <- NULL
        moduleData$EQ_loaded_source <- NULL
        tryCatch(
          {
            EQWin <- AccessConnect(source, silent = TRUE)
            on.exit(DBI::dbDisconnect(EQWin), add = TRUE)
            moduleData$EQ_locs <- DBI::dbGetQuery(
              EQWin,
              "SELECT StnCode, StnDesc FROM eqstns;"
            )
            moduleData$EQ_loc_grps <- DBI::dbGetQuery(
              EQWin,
              "SELECT groupname, groupdesc, groupitems FROM eqgroups WHERE dbtablename = 'eqstns'"
            )
            moduleData$EQ_params <- DBI::dbGetQuery(
              EQWin,
              "SELECT ParamId, ParamCode, ParamDesc, Units AS unit FROM eqparams;"
            )
            moduleData$EQ_param_grps <- DBI::dbGetQuery(
              EQWin,
              "SELECT groupname, groupdesc, groupitems FROM eqgroups WHERE dbtablename = 'eqparams'"
            )
            moduleData$EQ_stds <- DBI::dbGetQuery(
              EQWin,
              "SELECT StdName, StdCode FROM eqstds"
            )
            moduleData$EQ_loaded_source <- source
          },
          error = function(e) {
            showNotification(
              paste(
                tr("wq_err_load_eqwin_choices", language$language),
                e$message
              ),
              type = "error",
              duration = NULL,
              closeButton = TRUE
            )
          }
        )
      },
      ignoreNULL = TRUE
    )

    observe({
      if (!identical(selected_data_source(), "EQ")) {
        return()
      }
      req(
        moduleData$EQ_params,
        moduleData$EQ_param_grps,
        moduleData$EQ_locs,
        moduleData$EQ_loc_grps,
        moduleData$EQ_stds
      )
      updateSelectizeInput(
        session,
        "parameters_EQ",
        choices = stats::setNames(
          moduleData$EQ_params$ParamCode,
          paste0(
            moduleData$EQ_params$ParamCode,
            " (",
            moduleData$EQ_params$ParamDesc,
            ")"
          )
        ),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "parameter_groups",
        choices = moduleData$EQ_param_grps$groupname,
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "locations_EQ",
        choices = stats::setNames(
          moduleData$EQ_locs$StnCode,
          paste0(
            moduleData$EQ_locs$StnCode,
            " (",
            moduleData$EQ_locs$StnDesc,
            ")"
          )
        ),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "location_groups",
        choices = moduleData$EQ_loc_grps$groupname,
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "stds",
        choices = stats::setNames(
          moduleData$EQ_stds$StdCode,
          moduleData$EQ_stds$StdName
        ),
        server = TRUE
      )
    })

    observeEvent(input$locs_groups, {
      if (identical(input$locs_groups, "Location Groups")) {
        shinyjs::show("location_groups")
        shinyjs::hide("locations_EQ")
      } else {
        shinyjs::hide("location_groups")
        shinyjs::show("locations_EQ")
      }
    })
    observeEvent(input$params_groups, {
      if (identical(input$params_groups, "Parameter Groups")) {
        shinyjs::show("parameter_groups")
        shinyjs::hide("parameters_EQ")
      } else {
        shinyjs::hide("parameter_groups")
        shinyjs::show("parameters_EQ")
      }
    })

    download_bundle <- reactiveVal(NULL)

    show_validation_modal <- function(messages) {
      messages <- unique(messages[!is.na(messages) & nzchar(messages)])
      if (!length(messages)) {
        return(invisible(FALSE))
      }
      showModal(modalDialog(
        title = tr("wq_validation_title", language$language),
        tags$p(tr("wq_validation_intro", language$language)),
        tags$ul(lapply(messages, function(msg) tags$li(msg))),
        easyClose = TRUE,
        footer = modalButton(tr("close", language$language))
      ))
      invisible(TRUE)
    }

    normalize_optional_date <- function(value) {
      if (length(value) == 0 || all(is.na(value))) {
        return(NULL)
      }
      as.Date(value)[[1]]
    }

    validate_report_request <- function() {
      issues <- character()
      source <- selected_data_source()

      report_dates <- if (identical(source, "AC")) {
        suppressWarnings(as.Date(input$dates_AC))
      } else {
        as.Date(input$date)
      }
      if (identical(source, "AC")) {
        if (
          !length(report_dates) ||
            anyNA(report_dates) ||
            anyDuplicated(report_dates)
        ) {
          issues <- c(
            issues,
            tr("wq_err_aquacache_dates", language$language)
          )
        }
        if (
          length(input$format_AC) != 1L ||
            is.na(input$format_AC) ||
            !input$format_AC %in% c("by_date", "by_location", "by_parameter")
        ) {
          issues <- c(issues, tr("wq_err_aquacache_layout", language$language))
        }
      } else if (length(report_dates) != 1L || is.na(report_dates)) {
        issues <- c(issues, tr("wq_err_report_date", language$language))
      }
      tolerances <- if (
        identical(source, "AC") &&
          identical(input$date_approx_mode_AC, "per_date") &&
          length(report_dates) &&
          !anyNA(report_dates)
      ) {
        date_ids <- format(report_dates, "%Y%m%d")
        vapply(
          date_ids,
          function(date_id) {
            value <- input[[paste0("date_approx_", date_id)]]
            if (is.null(value)) NA_real_ else as.numeric(value)
          },
          numeric(1)
        )
      } else if (
        identical(source, "AC") &&
          !is.null(input$date_approx_ac) &&
          length(input$date_approx_ac) == 1L
      ) {
        as.numeric(input$date_approx_ac)
      } else if (
        identical(source, "EQ") &&
          !is.null(input$date_approx_eq) &&
          length(input$date_approx_eq) == 1L
      ) {
        as.numeric(input$date_approx_eq)
      } else {
        numeric()
      }
      expected_tolerance_count <- if (
        identical(source, "AC") &&
          identical(input$date_approx_mode_AC, "per_date")
      ) {
        length(report_dates)
      } else {
        1L
      }
      if (
        !length(tolerances) ||
          anyNA(tolerances) ||
          any(!is.finite(tolerances)) ||
          any(tolerances < 0) ||
          any(tolerances > .Machine$integer.max) ||
          any(tolerances != trunc(tolerances)) ||
          length(tolerances) != expected_tolerance_count
      ) {
        issues <- c(
          issues,
          tr("wq_err_date_match_days", language$language)
        )
      }

      if (
        !is.null(input$SD_SD) &&
          length(input$SD_SD) == 1L &&
          !is.na(input$SD_SD)
      ) {
        if (!is.numeric(input$SD_SD) || input$SD_SD <= 0) {
          issues <- c(
            issues,
            tr("wq_err_sd_threshold", language$language)
          )
        }
        sd_start <- normalize_optional_date(input$SD_start)
        sd_end <- normalize_optional_date(input$SD_end)
        if (!is.null(sd_start) && !is.null(sd_end) && sd_start > sd_end) {
          issues <- c(
            issues,
            tr("wq_err_sd_dates", language$language)
          )
        }
        sd_range <- as.Date(input$SD_date_range)
        if (
          length(sd_range) != 2L ||
            any(is.na(sd_range)) ||
            sd_range[[1]] > sd_range[[2]]
        ) {
          issues <- c(
            issues,
            tr("wq_err_sd_day_range", language$language)
          )
        }
      }

      if (identical(source, "AC")) {
        if (!isTRUE(ac_metadata_loaded())) {
          issues <- c(
            issues,
            tr("wq_err_aquacache_loading", language$language)
          )
        } else {
          if (
            is.null(input$locations_AC) ||
              !length(input$locations_AC) ||
              anyNA(input$locations_AC) ||
              !any(nzchar(input$locations_AC))
          ) {
            issues <- c(
              issues,
              tr("wq_err_aquacache_location_required", language$language)
            )
          } else if (
            length(setdiff(
              input$locations_AC,
              as.character(moduleData$AC_locs$location_id)
            ))
          ) {
            issues <- c(
              issues,
              tr("wq_err_aquacache_location_invalid", language$language)
            )
          }
          if (
            is.null(input$parameters_AC) ||
              !length(input$parameters_AC) ||
              anyNA(input$parameters_AC) ||
              !any(nzchar(input$parameters_AC))
          ) {
            issues <- c(
              issues,
              tr("wq_err_aquacache_parameter_required", language$language)
            )
          } else if (
            length(setdiff(
              input$parameters_AC,
              as.character(moduleData$AC_params$parameter_id)
            ))
          ) {
            issues <- c(
              issues,
              tr("wq_err_aquacache_parameter_invalid", language$language)
            )
          }
          if (
            length(input$matrix_states_AC) &&
              length(setdiff(
                input$matrix_states_AC,
                as.character(moduleData$AC_matrix_states$matrix_state_id)
              ))
          ) {
            issues <- c(
              issues,
              tr("wq_err_matrix_state_invalid", language$language)
            )
          }
          if (
            length(input$sample_fractions_AC) &&
              length(setdiff(
                input$sample_fractions_AC,
                as.character(moduleData$AC_sample_fractions$sample_fraction_id)
              ))
          ) {
            issues <- c(
              issues,
              tr("wq_err_sample_fraction_invalid", language$language)
            )
          }
          if (
            length(input$guidelines_AC) &&
              length(setdiff(
                input$guidelines_AC,
                as.character(ac_guideline_choices()$guideline_id)
              ))
          ) {
            issues <- c(
              issues,
              tr("wq_err_aquacache_guideline_invalid", language$language)
            )
          }
        }
      } else {
        if (!eqwin_available) {
          issues <- c(issues, tr("wq_err_no_eqwin_database", language$language))
        }
        if (
          is.null(input$EQWin_source) ||
            !length(input$EQWin_source) ||
            anyNA(input$EQWin_source) ||
            !nzchar(input$EQWin_source[[1]])
        ) {
          issues <- c(
            issues,
            tr("wq_err_select_valid_eqwin", language$language)
          )
        } else if (
          inherits(
            try(resolve_eqwin_source(input$EQWin_source[[1]]), silent = TRUE),
            "try-error"
          )
        ) {
          issues <- c(
            issues,
            tr("wq_err_select_available_eqwin", language$language)
          )
        }

        if (
          is.null(moduleData$EQ_loaded_source) ||
            is.null(moduleData$EQ_locs) ||
            is.null(moduleData$EQ_params)
        ) {
          issues <- c(
            issues,
            tr("wq_err_eqwin_loading", language$language)
          )
        } else {
          if (identical(input$locs_groups, "Locations")) {
            if (
              is.null(input$locations_EQ) ||
                !length(input$locations_EQ) ||
                all(is.na(input$locations_EQ)) ||
                !any(nzchar(input$locations_EQ))
            ) {
              issues <- c(
                issues,
                tr("wq_err_station_required", language$language)
              )
            } else if (
              length(setdiff(
                input$locations_EQ,
                moduleData$EQ_locs$StnCode
              ))
            ) {
              issues <- c(
                issues,
                tr("wq_err_station_invalid", language$language)
              )
            }
          } else if (identical(input$locs_groups, "Location Groups")) {
            if (
              is.null(input$location_groups) ||
                !length(input$location_groups) ||
                anyNA(input$location_groups) ||
                !nzchar(input$location_groups[[1]])
            ) {
              issues <- c(
                issues,
                tr("wq_err_location_group_required", language$language)
              )
            } else if (
              !input$location_groups[[1]] %in% moduleData$EQ_loc_grps$groupname
            ) {
              issues <- c(
                issues,
                tr("wq_err_location_group_invalid", language$language)
              )
            }
          } else {
            issues <- c(
              issues,
              tr("wq_err_location_filter_mode", language$language)
            )
          }

          if (identical(input$params_groups, "Parameters")) {
            if (
              is.null(input$parameters_EQ) ||
                !length(input$parameters_EQ) ||
                all(is.na(input$parameters_EQ)) ||
                !any(nzchar(input$parameters_EQ))
            ) {
              issues <- c(
                issues,
                tr("wq_err_parameter_required", language$language)
              )
            } else if (
              length(setdiff(
                input$parameters_EQ,
                moduleData$EQ_params$ParamCode
              ))
            ) {
              issues <- c(
                issues,
                tr("wq_err_parameter_invalid", language$language)
              )
            }
          } else if (identical(input$params_groups, "Parameter Groups")) {
            if (
              is.null(input$parameter_groups) ||
                !length(input$parameter_groups) ||
                anyNA(input$parameter_groups) ||
                !nzchar(input$parameter_groups[[1]])
            ) {
              issues <- c(
                issues,
                tr("wq_err_parameter_group_required", language$language)
              )
            } else if (
              !input$parameter_groups[[1]] %in%
                moduleData$EQ_param_grps$groupname
            ) {
              issues <- c(
                issues,
                tr("wq_err_parameter_group_invalid", language$language)
              )
            }
          } else {
            issues <- c(
              issues,
              tr("wq_err_parameter_filter_mode", language$language)
            )
          }
          if (
            length(input$stds) &&
              length(setdiff(input$stds, moduleData$EQ_stds$StdCode))
          ) {
            issues <- c(
              issues,
              tr("wq_err_standard_invalid", language$language)
            )
          }
        }
      }
      unique(issues)
    }

    cleanup_download_bundle <- function(bundle) {
      if (is.null(bundle) || is.null(bundle$path)) {
        return(invisible(NULL))
      }
      bundle_dir <- dirname(bundle$path)
      if (dir.exists(bundle_dir)) {
        unlink(bundle_dir, recursive = TRUE, force = TRUE)
      } else if (file.exists(bundle$path)) {
        unlink(bundle$path, force = TRUE)
      }
      invisible(NULL)
    }

    pick_generated_file <- function(files, pattern = NULL, lang) {
      files <- files[file.exists(files)]
      if (!length(files)) {
        stop(tr("wq_err_no_report_files", lang), call. = FALSE)
      }
      if (!is.null(pattern)) {
        matched <- files[grepl(pattern, basename(files), ignore.case = TRUE)]
        if (length(matched) == 1L) {
          return(matched[[1]])
        }
        if (length(matched) > 1L) {
          stop(tr("wq_err_multiple_report_files", lang), call. = FALSE)
        }
      }
      if (length(files) != 1L) {
        stop(tr("wq_err_expected_report_file", lang), call. = FALSE)
      }
      files[[1]]
    }

    report_task <- ExtendedTask$new(function(req, db_config) {
      promises::future_promise({
        work_dir <- tempfile("WQReport_")
        dir.create(work_dir, recursive = TRUE)
        tryCatch(
          {
            report_warning <- NULL
            if (identical(req$data_source, "AC")) {
              con <- AquaConnect(
                name = db_config$dbName,
                host = db_config$dbHost,
                port = db_config$dbPort,
                username = db_config$dbUser,
                password = db_config$dbPass,
                silent = TRUE
              )
              on.exit(DBI::dbDisconnect(con), add = TRUE)
              DBI::dbExecute(
                con,
                "SET application_name TO 'YGwater_shiny'"
              )
              output_path <- file.path(work_dir, "water-quality-report.xlsx")
              result <- AquaCacheReport(
                date = req$date,
                location_ids = req$location_ids,
                parameter_ids = req$parameter_ids,
                matrix_state_ids = req$matrix_state_ids,
                sample_fraction_ids = req$sample_fraction_ids,
                guideline_ids = req$guideline_ids,
                date_approx = req$date_approx,
                format = req$format,
                sd_multiplier = req$sd_multiplier,
                sd_start = req$sd_start,
                sd_end = req$sd_end,
                sd_day_of_year = req$sd_day_of_year,
                output_path = output_path,
                lang = req$lang,
                con = con
              )
              report_path <- result$xlsx_path
              if (result$reused_sample_count > 0L) {
                report_warning <- if (result$reused_sample_count == 1L) {
                  tr("wq_report_sample_reuse_one", req$ui_language)
                } else {
                  sprintf(
                    tr("wq_report_sample_reuse_many", req$ui_language),
                    result$reused_sample_count
                  )
                }
                report_warning <- paste(
                  report_warning,
                  tr(
                    if (identical(req$format, "by_date")) {
                      "wq_report_reuse_worksheets"
                    } else {
                      "wq_report_reuse_date_columns"
                    },
                    req$ui_language
                  )
                )
              }
            } else {
              EQWin <- AccessConnect(req$eqwin_source, silent = TRUE)
              on.exit(DBI::dbDisconnect(EQWin), add = TRUE)
              EQWinReport(
                date = req$date,
                date_approx = req$date_approx,
                stations = req$stations,
                stnGrp = req$stnGrp,
                parameters = req$parameters,
                paramGrp = req$paramGrp,
                stds = req$stds,
                stnStds = req$stnStds,
                SD_exceed = req$sd_multiplier,
                SD_start = req$sd_start,
                SD_end = req$sd_end,
                SD_doy = req$sd_day_of_year,
                save_path = work_dir,
                con = EQWin
              )
              report_path <- pick_generated_file(
                list.files(work_dir, full.names = TRUE),
                pattern = "\\.xlsx$",
                lang = req$ui_language
              )
            }

            date_label <- paste(format(req$date, "%Y-%m-%d"), collapse = ", ")
            list(
              path = report_path,
              filename = sprintf(
                tr("wq_report_filename", req$ui_language),
                date_label,
                Sys.Date()
              ),
              warning_message = report_warning
            )
          },
          error = function(e) {
            if (dir.exists(work_dir)) {
              unlink(work_dir, recursive = TRUE, force = TRUE)
            }
            e$message
          }
        )
      })
    }) |>
      bind_task_button("go")

    observeEvent(input$go, {
      if (show_validation_modal(validate_report_request())) {
        return()
      }

      source <- selected_data_source()
      sd_multiplier <- if (
        !is.null(input$SD_SD) &&
          length(input$SD_SD) == 1L &&
          !is.na(input$SD_SD)
      ) {
        as.numeric(input$SD_SD)
      } else {
        NULL
      }
      sd_day_of_year <- if (!is.null(sd_multiplier)) {
        as.integer(c(
          lubridate::yday(as.Date(input$SD_date_range[[1]])),
          lubridate::yday(as.Date(input$SD_date_range[[2]]))
        ))
      } else {
        NULL
      }
      req <- list(
        data_source = source,
        date = if (identical(source, "AC")) {
          as.Date(input$dates_AC)
        } else {
          as.Date(input$date)
        },
        date_approx = if (identical(source, "AC")) {
          if (identical(input$date_approx_mode_AC, "per_date")) {
            date_ids <- format(as.Date(input$dates_AC), "%Y%m%d")
            as.integer(vapply(
              date_ids,
              function(date_id) {
                input[[paste0("date_approx_", date_id)]]
              },
              numeric(1)
            ))
          } else {
            as.integer(input$date_approx_ac)
          }
        } else {
          as.integer(input$date_approx_eq)
        },
        format = if (identical(source, "AC")) input$format_AC else NULL,
        sd_multiplier = sd_multiplier,
        sd_start = normalize_optional_date(input$SD_start),
        sd_end = normalize_optional_date(input$SD_end),
        sd_day_of_year = sd_day_of_year,
        lang = if (identical(language$language, "Français")) "fr" else "en",
        ui_language = language$language
      )

      if (identical(source, "AC")) {
        req$location_ids <- input$locations_AC
        req$parameter_ids <- input$parameters_AC
        req$matrix_state_ids <- input$matrix_states_AC
        req$sample_fraction_ids <- input$sample_fractions_AC
        req$guideline_ids <- if (length(input$guidelines_AC)) {
          input$guidelines_AC
        } else {
          NULL
        }
      } else {
        req$eqwin_source <- resolve_eqwin_source(input$EQWin_source)
        req$stations <- if (identical(input$locs_groups, "Locations")) {
          input$locations_EQ
        } else {
          NULL
        }
        req$stnGrp <- if (identical(input$locs_groups, "Location Groups")) {
          input$location_groups
        } else {
          NULL
        }
        req$parameters <- if (identical(input$params_groups, "Parameters")) {
          input$parameters_EQ
        } else {
          NULL
        }
        req$paramGrp <- if (
          identical(input$params_groups, "Parameter Groups")
        ) {
          input$parameter_groups
        } else {
          NULL
        }
        req$stds <- input$stds
        req$stnStds <- input$stnStds
      }

      current_config <- session$userData$config
      db_config <- list(
        dbName = current_config$dbName,
        dbHost = current_config$dbHost,
        dbPort = current_config$dbPort,
        dbUser = current_config$dbUser,
        dbPass = current_config$dbPass
      )
      report_task$invoke(req = req, db_config = db_config)
    })

    observeEvent(report_task$result(), {
      result <- report_task$result()
      if (inherits(result, "character")) {
        showNotification(
          paste(tr("wq_err_generating_prefix", language$language), result),
          type = "error",
          duration = NULL,
          closeButton = TRUE
        )
        return()
      }
      if (is.null(result$path) || !file.exists(result$path)) {
        showNotification(
          tr("wq_err_download_missing", language$language),
          type = "error",
          duration = NULL,
          closeButton = TRUE
        )
        return()
      }
      cleanup_download_bundle(download_bundle())
      if (!is.null(result$warning_message)) {
        showNotification(
          result$warning_message,
          type = "warning",
          duration = 12,
          closeButton = TRUE
        )
      }
      download_bundle(result)
      shinyjs::click("download")
    })

    output$download <- downloadHandler(
      filename = function() {
        req(download_bundle())
        download_bundle()$filename
      },
      content = function(file) {
        bundle <- download_bundle()
        req(bundle)
        if (!file.exists(bundle$path)) {
          stop(tr("wq_err_download_source_missing", language$language))
        }
        copied <- file.copy(bundle$path, file, overwrite = TRUE)
        if (!isTRUE(copied)) {
          stop(tr("wq_err_download_copy", language$language))
        }
        cleanup_download_bundle(bundle)
        download_bundle(NULL)
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
    outputOptions(output, "download", suspendWhenHidden = FALSE)
  })
}
