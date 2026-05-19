discPlotUI <- function(id) {
  ns <- NS(id)

  banner_key <- paste0("discPlot_banner_v2026_01_14__", id)

  tagList(
    uiOutput(ns("banner")),
    page_sidebar(
      sidebar = sidebar(
        title = NULL,
        width = 350,
        bg = config$sidebar_bg,
        open = list(mobile = "always-above"),
        uiOutput(ns("sidebar"))
      ),
      uiOutput(ns("main"))
    )
  )
}

discPlot <- function(id, mdb_files, language, windowDims, inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # Used to create UI elements within the server code

    EQWin_selector <- reactiveVal(FALSE) # flags whether the EQWin source UI is already rendered

    # Static lookup data is shared across sessions; availability is queried
    # narrowly below as users choose location, media, date range, and parameters.
    if (isTRUE(session$userData$user_logged_in)) {
      cached <- disc_plot_module_data(
        con = session$userData$AquaCache,
        env = session$userData$app_cache
      )
    } else {
      cached <- disc_plot_module_data(con = session$userData$AquaCache)
    }

    moduleData <- reactiveValues(
      AC_locs = cached$locs,
      AC_params = cached$params,
      AC_sub_locs = cached$sub_locs,
      AC_media = cached$media,
      AC_sample_types = cached$sample_types,
      AC_collection_methods = cached$collection_methods,
      AC_result_types = cached$result_types,
      AC_sample_fractions = cached$sample_fractions,
      AC_result_value_types = cached$result_value_types,
      AC_result_speciations = cached$result_speciations
    )

    browse_selected_sample_ids <- reactiveVal(numeric(0))

    ac_option_rows <- function(scope, lookup, scope_col, lookup_col) {
      if (is.null(scope) || nrow(scope) == 0 || nrow(lookup) == 0) {
        return(lookup[0, , drop = FALSE])
      }
      ids <- unique(scope[[scope_col]])
      ids <- ids[!is.na(ids)]
      lookup[lookup[[lookup_col]] %in% ids, , drop = FALSE]
    }

    ac_keep_selection <- function(selected, choices) {
      if (is.null(selected) || length(selected) == 0) {
        return("all")
      }
      selected <- as.character(selected)
      if ("all" %in% selected) {
        return("all")
      }
      kept <- selected[selected %in% as.character(choices)]
      if (length(kept) == 0) {
        return("all")
      }
      kept
    }

    ac_selectize <- function(input_id, label, choices, selected = NULL) {
      if (length(choices) <= 1) {
        return(NULL)
      }
      choices <- c(
        stats::setNames("all", tr("all_m", language$language)),
        choices
      )
      selectizeInput(
        ns(input_id),
        label,
        choices = choices,
        selected = ac_keep_selection(selected, choices),
        multiple = TRUE
      )
    }

    ac_numeric_values <- function(values, include_all = FALSE) {
      if (is.null(values) || length(values) == 0) {
        return(numeric(0))
      }
      values <- as.character(values)
      if (!include_all) {
        values <- values[values != "all"]
      }
      values <- suppressWarnings(as.numeric(values))
      unique(values[!is.na(values)])
    }

    ac_in_clause <- function(column, values) {
      values <- ac_numeric_values(values)
      if (length(values) == 0) {
        return(NULL)
      }
      paste0(column, " IN (", paste(values, collapse = ", "), ")")
    }

    ac_date_clause <- function(column, date_range) {
      if (is.null(date_range) || length(date_range) != 2) {
        return(NULL)
      }
      date_range <- as.Date(date_range)
      if (any(is.na(date_range))) {
        return(NULL)
      }
      paste0(
        column,
        " >= ",
        DBI::dbQuoteLiteral(session$userData$AquaCache, date_range[1]),
        "::date AND ",
        column,
        " < (",
        DBI::dbQuoteLiteral(session$userData$AquaCache, date_range[2]),
        "::date + INTERVAL '1 day')"
      )
    }

    ac_where <- function(
      locations = NULL,
      media = NULL,
      date_range = NULL,
      parameters = NULL
    ) {
      clauses <- Filter(
        Negate(is.null),
        list(
          ac_in_clause("s.location_id", locations),
          ac_in_clause("s.media_id", media),
          ac_date_clause("s.datetime", date_range),
          ac_in_clause("r.parameter_id", parameters)
        )
      )
      if (length(clauses) == 0) {
        return("")
      }
      paste("WHERE", paste(clauses, collapse = " AND "))
    }

    ac_query <- function(sql, empty = data.frame()) {
      tryCatch(
        DBI::dbGetQuery(session$userData$AquaCache, sql),
        error = function(e) {
          warning(e$message, call. = FALSE)
          empty
        }
      )
    }

    ac_available_media <- reactive({
      if (length(ac_numeric_values(input$locations_AC)) == 0) {
        return(moduleData$AC_media[0, , drop = FALSE])
      }
      sql <- paste(
        "SELECT DISTINCT m.media_id, m.media_type, m.media_type_fr",
        "FROM media_types AS m",
        "INNER JOIN samples AS s ON m.media_id = s.media_id",
        ac_where(locations = input$locations_AC),
        "ORDER BY m.media_type ASC"
      )
      ac_query(sql, moduleData$AC_media[0, , drop = FALSE])
    })

    ac_available_date_range <- reactive({
      if (length(ac_numeric_values(input$locations_AC)) == 0) {
        return(NULL)
      }
      sql <- paste(
        "SELECT MIN(s.datetime)::date AS start_date,",
        "MAX(s.datetime)::date AS end_date",
        "FROM samples AS s",
        ac_where(
          locations = input$locations_AC,
          media = input$media_AC
        )
      )
      range <- ac_query(
        sql,
        data.frame(start_date = as.Date(NA), end_date = as.Date(NA))
      )
      if (
        nrow(range) == 0 ||
          is.na(range$start_date[1]) ||
          is.na(range$end_date[1])
      ) {
        return(NULL)
      }
      range
    })

    ac_available_parameters <- reactive({
      if (length(ac_numeric_values(input$locations_AC)) == 0) {
        return(moduleData$AC_params[0, , drop = FALSE])
      }
      sql <- paste(
        "SELECT DISTINCT p.parameter_id, p.param_name",
        "FROM parameters AS p",
        "INNER JOIN results AS r ON p.parameter_id = r.parameter_id",
        "INNER JOIN samples AS s ON r.sample_id = s.sample_id",
        ac_where(
          locations = input$locations_AC,
          media = input$media_AC,
          date_range = input$date_range_AC
        ),
        "ORDER BY p.param_name ASC"
      )
      ac_query(sql, moduleData$AC_params[0, , drop = FALSE])
    })

    ac_scope <- reactive({
      if (
        length(ac_numeric_values(input$locations_AC)) == 0 ||
          is.null(input$date_range_AC) ||
          length(ac_numeric_values(input$parameters_AC, include_all = TRUE)) == 0
      ) {
        return(data.frame())
      }
      sql <- paste(
        "SELECT DISTINCT",
        "s.sub_location_id, s.media_id, s.sample_type, s.collection_method,",
        "r.result_type, r.sample_fraction_id, r.result_value_type,",
        "r.result_speciation_id",
        "FROM samples AS s",
        "INNER JOIN results AS r ON s.sample_id = r.sample_id",
        ac_where(
          locations = input$locations_AC,
          media = input$media_AC,
          date_range = input$date_range_AC,
          parameters = input$parameters_AC
        )
      )
      ac_query(sql, data.frame())
    })

    ac_sample_id_clause <- function(values, column = "s.sample_id") {
      values <- unique(suppressWarnings(as.numeric(values)))
      values <- values[!is.na(values)]
      if (length(values) == 0) {
        return(NULL)
      }
      paste0(column, " IN (", paste(values, collapse = ", "), ")")
    }

    ac_browse_parameter_filter <- function() {
      parameter_ids <- ac_numeric_values(input$browse_parameters_AC)
      if (length(parameter_ids) == 0) {
        return(NULL)
      }
      ids_sql <- paste(parameter_ids, collapse = ", ")
      if (identical(input$browse_parameter_match, "all")) {
        paste0(
          "(SELECT COUNT(DISTINCT rpf.parameter_id)",
          " FROM results AS rpf",
          " WHERE rpf.sample_id = s.sample_id",
          " AND rpf.parameter_id IN (",
          ids_sql,
          ")) = ",
          length(parameter_ids)
        )
      } else {
        paste0(
          "EXISTS (SELECT 1 FROM results AS rpf",
          " WHERE rpf.sample_id = s.sample_id",
          " AND rpf.parameter_id IN (",
          ids_sql,
          "))"
        )
      }
    }

    ac_browse_where <- function(include_parameter_filter = TRUE) {
      clauses <- Filter(
        Negate(is.null),
        list(
          ac_date_clause("s.datetime", input$browse_date_range),
          if (isTRUE(include_parameter_filter)) {
            ac_browse_parameter_filter()
          }
        )
      )
      if (length(clauses) == 0) {
        return("")
      }
      paste("WHERE", paste(clauses, collapse = " AND "))
    }

    ac_browse_sample_table <- reactive({
      limit <- suppressWarnings(as.integer(input$browse_sample_limit))
      if (length(limit) == 0 || is.na(limit) || limit < 100) {
        limit <- 5000L
      }
      limit <- min(limit, 50000L)

      sql <- paste(
        "WITH base AS (",
        "SELECT s.sample_id, s.location_id, l.name AS location,",
        "l.location_code, sl.sub_location_name AS sub_location,",
        "s.datetime::date AS sample_date, s.datetime,",
        "s.media_id, mt.media_type AS media,",
        "s.sample_type AS sample_type_id, st.sample_type,",
        "s.collection_method AS collection_method_id, cm.collection_method",
        "FROM samples AS s",
        "INNER JOIN locations AS l ON s.location_id = l.location_id",
        "LEFT JOIN sub_locations AS sl ON s.sub_location_id = sl.sub_location_id",
        "LEFT JOIN media_types AS mt ON s.media_id = mt.media_id",
        "LEFT JOIN sample_types AS st ON s.sample_type = st.sample_type_id",
        "LEFT JOIN collection_methods AS cm ON",
        "s.collection_method = cm.collection_method_id",
        ac_browse_where(include_parameter_filter = TRUE),
        "ORDER BY s.datetime DESC",
        "LIMIT",
        limit,
        ")",
        "SELECT base.sample_id, base.location, base.location_code,",
        "base.sub_location, base.sample_date, base.media,",
        "base.sample_type, base.collection_method,",
        "COALESCE(params.result_count, 0) AS result_count,",
        "COALESCE(params.parameters, '') AS parameters",
        "FROM base",
        "LEFT JOIN LATERAL (",
        "SELECT COUNT(*) AS result_count,",
        "STRING_AGG(DISTINCT p.param_name, ', ' ORDER BY p.param_name)",
        "AS parameters",
        "FROM results AS r",
        "INNER JOIN parameters AS p ON r.parameter_id = p.parameter_id",
        "WHERE r.sample_id = base.sample_id",
        ") AS params ON TRUE",
        "ORDER BY base.datetime DESC"
      )
      ac_query(sql, data.frame())
    })

    ac_browse_parameter_choices <- reactive({
      selected <- browse_selected_sample_ids()
      sample_clause <- ac_sample_id_clause(selected, "s.sample_id")

      where <- if (!is.null(sample_clause)) {
        paste("WHERE", sample_clause)
      } else {
        if (
          is.null(input$browse_date_range) ||
            length(input$browse_date_range) != 2
        ) {
          return(data.frame(
            parameter_id = numeric(),
            param_name = character(),
            n = numeric()
          ))
        }
        ac_browse_where(include_parameter_filter = FALSE)
      }

      sql <- paste(
        "SELECT p.parameter_id, p.param_name, COUNT(DISTINCT s.sample_id) AS n",
        "FROM samples AS s",
        "INNER JOIN results AS r ON s.sample_id = r.sample_id",
        "INNER JOIN parameters AS p ON r.parameter_id = p.parameter_id",
        where,
        "GROUP BY p.parameter_id, p.param_name",
        "ORDER BY p.param_name ASC"
      )
      ac_query(
        sql,
        data.frame(parameter_id = numeric(), param_name = character(), n = numeric())
      )
    })

    ac_selected_sample_rows <- reactive({
      selected <- browse_selected_sample_ids()
      sample_clause <- ac_sample_id_clause(selected, "s.sample_id")
      if (is.null(sample_clause)) {
        return(data.frame())
      }
      sql <- paste(
        "SELECT s.sample_id, l.name AS location,",
        "s.datetime::date AS sample_date, mt.media_type AS media",
        "FROM samples AS s",
        "INNER JOIN locations AS l ON s.location_id = l.location_id",
        "LEFT JOIN media_types AS mt ON s.media_id = mt.media_id",
        "WHERE",
        sample_clause,
        "ORDER BY s.datetime DESC"
      )
      ac_query(sql, data.frame())
    })

    ensure_discrete_plot_function <- function() {
      new_args <- c(
        "sub_location_ids",
        "media",
        "sample_types",
        "collection_methods",
        "result_types",
        "sample_fractions",
        "result_value_types",
        "result_speciations",
        "include_blanks",
        "duplicate_action",
        "sample_ids",
        "season_ranges"
      )
      if (all(new_args %in% names(formals(plotDiscrete)))) {
        return(invisible(TRUE))
      }

      candidates <- unique(c(
        file.path(getwd(), "R", "plotDiscrete.R"),
        file.path(dirname(getwd()), "R", "plotDiscrete.R"),
        "C:/Users/g_del/Documents/R/YGwater/R/plotDiscrete.R"
      ))
      candidates <- candidates[file.exists(candidates)]
      for (path in candidates) {
        root <- dirname(dirname(path))
        for (helper in c("titleCase.R", "utils.R", "AquaConnect.R")) {
          helper_path <- file.path(root, "R", helper)
          if (file.exists(helper_path)) {
            source(helper_path, local = .GlobalEnv)
          }
        }
        source(path, local = .GlobalEnv)
        if (all(new_args %in% names(formals(plotDiscrete)))) {
          return(invisible(TRUE))
        }
      }

      stop(
        "The loaded plotDiscrete() function is older than the discrete plot ",
        "module. Restart the app from the current YGwater source tree or ",
        "reinstall/reload YGwater so R/plotDiscrete.R is current."
      )
    }

    output$banner <- renderUI({
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "discPlot"
      )
    })

    output$sidebar <- renderUI({
      tagList(
        # Toggle for data source
        if (is.null(mdb_files)) {
          shinyjs::hidden(radioButtons(
            ns("data_source"),
            NULL,
            choices = stats::setNames(c("AC", "EQ"), c("AquaCache", "EQWin")),
            selected = "AC"
          ))
        } else {
          radioButtons(
            ns("data_source"),
            NULL,
            choices = stats::setNames(c("AC", "EQ"), c("AquaCache", "EQWin")),
            selected = "AC"
          )
        },
        uiOutput(ns("EQWin_source_ui")),
        conditionalPanel(
          ns = ns,
          condition = "input.data_source == 'AC'",
          radioButtons(
            ns("AC_selector_mode"),
            tooltip(
              trigger = list(
                "Selection mode",
                bsicons::bs_icon("info-circle-fill")
              ),
              paste(
                "Guided selectors step through the common plotting filters.",
                "Browse samples lets you search a sample table, keep selected",
                "rows across pages and filters, then choose parameters from",
                "those samples."
              )
            ),
            choices = c(
              "Guided selectors" = "guided",
              "Browse samples" = "browse"
            ),
            selected = "guided"
          )
        ),
        conditionalPanel(
          ns = ns,
          condition = "input.data_source == 'EQ'",
          dateRangeInput(
            ns("date_range_EQ"),
            tr("date_range_lab", language$language),
            start = Sys.Date() - 30,
            end = Sys.Date(),
            max = Sys.Date() + 1,
            format = "yyyy-mm-dd",
            language = language$abbrev,
            separator = tr("date_sep", language$language)
          ),
          # Toggle button for locations or location groups (only show if data source == EQWin)
          radioButtons(
            ns("locs_groups"),
            NULL,
            choices = stats::setNames(
              c("locations", "loc_groups"),
              c(
                tr("locs", language$language),
                tr("loc_groups", language$language)
              )
            ),
            selected = "locations"
          ),
          # Selectize input for locations, populated once connection is established
          selectizeInput(
            ns("locations_EQ"),
            tr("select_locs", language$language),
            choices = NULL,
            multiple = TRUE
          ),
          # Selectize input for location groups, populated once connection is established. only shown if data source is EQWin
          selectizeInput(
            ns("location_groups"),
            tr("select_loc_group", language$language),
            choices = NULL,
            multiple = TRUE,
            options = list(maxItems = 1)
          ), # This fixes a bug where the 'Placeholder' value remains after updating values

          # Toggle button for parameters or parameter groups (only show if data source == EQWin)
          radioButtons(
            ns("params_groups"),
            NULL,
            choices = stats::setNames(
              c("parameters", "param_groups"),
              c(
                tr("parameters", language$language),
                tr("param_groups", language$language)
              )
            ),
            selected = "parameters"
          ),
          # Selectize input for parameters, populated once connection is established
          selectizeInput(
            ns("parameters_EQ"),
            tr("select_params", language$language),
            choices = NULL,
            multiple = TRUE
          ),
          # Selectize input for parameter groups, populated once connection is established. only shown if data source is EQWin
          selectizeInput(
            ns("parameter_groups"),
            tr("select_param_group", language$language),
            choices = NULL,
            multiple = TRUE,
            options = list(maxItems = 1)
          ), # This fixes a bug where the 'Placeholder' value remains after updating values
          # Selectize input for a standard to apply
          div(
            style = "display: flex; align-items: center;",
            tags$label(
              tr("select_standard_opt", language$language),
              class = "form-label",
              style = "margin-right: 5px;"
            ),
            span(
              id = ns("standard_info"),
              `data-bs-toggle` = "tooltip",
              `data-bs-placement` = "right",
              `data-bs-trigger` = "click hover",
              title = tr("standard_warning", language$language),
              icon("info-circle", style = "font-size: 100%; margin-left: 5px;")
            )
          ),
          selectizeInput(
            ns("standard"),
            NULL,
            choices = NULL,
            multiple = TRUE,
            options = list(maxItems = 1)
          ) # This is to be able to use the default no selection upon initialization but only have one possible selection anyways.
        ),

        conditionalPanel(
          ns = ns,
          condition = "input.data_source == 'AC' && (input.AC_selector_mode == 'guided' || input.AC_selector_mode == null)",
          # Selectize input for locations, populated once connection is established
          selectizeInput(
            ns("locations_AC"),
            tr("loc(s)", language$language),
            choices = NULL,
            multiple = TRUE
          ),
          uiOutput(ns("AC_media_ui")),
          uiOutput(ns("AC_date_range_ui")),
          checkboxInput(
            ns("season_filter_enabled"),
            "Restrict to season/day-of-year ranges",
            value = FALSE
          ),
          uiOutput(ns("AC_season_ranges_ui")),
          # Selectize input for parameters, populated once connection is established
          selectizeInput(
            ns("parameters_AC"),
            tr("parameter(s)", language$language),
            choices = NULL,
            multiple = TRUE
          ),
          uiOutput(ns("AC_options_ui"))
        ),
        conditionalPanel(
          ns = ns,
          condition = "input.data_source == 'AC' && input.AC_selector_mode == 'browse'",
          helpText(
            "Use the table filters to find samples, then click rows to add",
            "them to the selection. The parameter list beside the table is",
            "based on the selected samples."
          ),
          dateRangeInput(
            ns("browse_date_range"),
            tr("date_range_lab", language$language),
            start = Sys.Date() - 365,
            end = Sys.Date() + 1,
            max = Sys.Date() + 1,
            format = "yyyy-mm-dd",
            language = language$abbrev,
            separator = tr("date_sep", language$language)
          ),
          numericInput(
            ns("browse_sample_limit"),
            "Maximum table rows",
            value = 5000,
            min = 100,
            max = 50000,
            step = 500
          )
        ),
        radioButtons(
          ns("facet_on"),
          label = tooltip(
            trigger = list(
              tr("facet_on", language$language),
              bsicons::bs_icon("info-circle-fill")
            ),
            tr("facet_on_tooltip", language$language)
          ),
          choices = stats::setNames(
            c("locs", "params"),
            c(
              tr("locs", language$language),
              tr("parameters", language$language)
            )
          ),
          selected = "locs"
        ),
        checkboxInput(
          ns("log_scale"),
          label = tooltip(
            trigger = list(
              tr("use_log_scale", language$language),
              bsicons::bs_icon("info-circle-fill")
            ),
            tr("log_scale_warning", language$language),
          )
        ),
        checkboxInput(
          ns("shareX"),
          label = tooltip(
            trigger = list(
              tr("share_x_axis", language$language),
              bsicons::bs_icon("info-circle-fill")
            ),
            tr("share_x_axis_tooltip", language$language)
          ),
          value = TRUE
        ),
        checkboxInput(
          ns("shareY"),
          label = tooltip(
            trigger = list(
              tr("share_y_axis", language$language),
              bsicons::bs_icon("info-circle-fill")
            ),
            tr("share_y_axis_tooltip", language$language)
          ),
          value = FALSE
        ),
        div(
          selectizeInput(
            ns("loc_code"),
            label = tr("loc_code", language$language),
            choices = stats::setNames(
              c("name", "code", "nameCode", "codeName"),
              c(
                tr("name", language$language),
                tr("code", language$language),
                tr("loc_code_nameCode", language$language),
                tr("loc_code_codeName", language$language)
              )
            ),
            selected = "name"
          ),
          style = "display: flex; align-items: center;"
        ),

        # div(
        checkboxInput(
          ns("target_datetime"),
          label = tooltip(
            trigger = list(
              tr("target_datetime", language$language),
              bsicons::bs_icon("info-circle-fill")
            ),
            tr("target_datetime_tooltip", language$language)
          )
        ),
        div(
          actionButton(
            ns("extra_aes"),
            tr("modify_plot_aes", language$language),
            style = "display: block; width: 100%; margin-bottom: 10px;"
          ), # Ensure block display and full width
          input_task_button(
            ns("make_plot"),
            label = tr("create_plot", language$language),
            label_busy = tr("processing", language$language),
            style = "display: block; width: 100%;", # Ensure block display and full width
            class = "btn btn-primary"
          )
        )
      ) # End of tagList
    }) %>% # End of renderUI for sidebar
      bindEvent(language$language) #TODO: bindEvent should also be on moduleData, but moduleData is not being used in the creation of lists yet

    output$AC_media_ui <- renderUI({
      req(input$data_source == "AC")

      media <- ac_available_media()
      choices <- stats::setNames(
        media$media_id,
        media[, tr("media_type_col", language$language)]
      )
      ac_selectize(
        "media_AC",
        tr("media_type(s)", language$language),
        choices,
        input$media_AC
      )
    }) %>%
      bindEvent(
        language$language,
        input$data_source,
        input$locations_AC,
        ignoreNULL = FALSE
      )

    output$AC_date_range_ui <- renderUI({
      req(input$data_source == "AC")
      if (length(ac_numeric_values(input$locations_AC)) == 0) {
        return(NULL)
      }

      available <- ac_available_date_range()
      if (is.null(available)) {
        start_date <- Sys.Date() - 30
        end_date <- Sys.Date()
      } else {
        start_date <- as.Date(available$start_date[1])
        end_date <- as.Date(available$end_date[1])
      }
      current <- input$date_range_AC
      if (!is.null(current) && length(current) == 2) {
        current <- as.Date(current)
        if (!any(is.na(current))) {
          start_date <- max(start_date, current[1])
          end_date <- min(end_date, current[2])
          if (!is.null(available) && start_date > end_date) {
            start_date <- as.Date(available$start_date[1])
            end_date <- as.Date(available$end_date[1])
          }
        }
      }

      dateRangeInput(
        ns("date_range_AC"),
        tr("date_range_lab", language$language),
        start = start_date,
        end = end_date,
        min = if (is.null(available)) NULL else as.Date(available$start_date[1]),
        max = if (is.null(available)) Sys.Date() + 1 else as.Date(available$end_date[1]),
        format = "yyyy-mm-dd",
        language = language$abbrev,
        separator = tr("date_sep", language$language)
      )
    }) %>%
      bindEvent(
        language$language,
        input$data_source,
        input$locations_AC,
        input$media_AC,
        ignoreNULL = FALSE
      )

    output$AC_season_ranges_ui <- renderUI({
      req(input$data_source == "AC")
      if (!isTRUE(input$season_filter_enabled)) {
        return(NULL)
      }

      current_year <- lubridate::year(Sys.Date())
      count <- suppressWarnings(as.integer(input$season_range_count))
      if (length(count) == 0 || is.na(count)) {
        count <- 1L
      }
      count <- max(1L, min(count, 3L))

      tagList(
        helpText(
          "Only the month/day portion of these ranges is used. Use a",
          "previous-year start and current-year end for seasons that cross",
          "New Year, such as September 1 to June 1."
        ),
        selectInput(
          ns("season_range_count"),
          "Number of season ranges",
          choices = stats::setNames(1:3, c("One", "Two", "Three")),
          selected = count
        ),
        lapply(seq_len(count), function(i) {
          dateRangeInput(
            ns(paste0("season_range_", i)),
            paste("Season", i),
            start = as.Date(sprintf("%d-01-01", current_year)),
            end = as.Date(sprintf("%d-12-31", current_year)),
            min = as.Date(sprintf("%d-01-01", current_year - 1L)),
            max = as.Date(sprintf("%d-12-31", current_year)),
            format = "yyyy-mm-dd",
            language = language$abbrev,
            separator = tr("date_sep", language$language)
          )
        })
      )
    }) %>%
      bindEvent(
        language$language,
        input$data_source,
        input$season_filter_enabled,
        input$season_range_count,
        ignoreNULL = FALSE
      )

    output$AC_options_ui <- renderUI({
      req(input$data_source == "AC")

      scope <- ac_scope()
      if (is.null(scope) || nrow(scope) == 0) {
        return(NULL)
      }
      plot_lang <- if (identical(language$language, "Français")) {
        "fr"
      } else {
        "en"
      }

      sub_locs <- ac_option_rows(
        scope,
        moduleData$AC_sub_locs,
        "sub_location_id",
        "sub_location_id"
      )
      sample_types <- ac_option_rows(
        scope,
        moduleData$AC_sample_types,
        "sample_type",
        "sample_type_id"
      )
      collection_methods <- ac_option_rows(
        scope,
        moduleData$AC_collection_methods,
        "collection_method",
        "collection_method_id"
      )
      result_types <- ac_option_rows(
        scope,
        moduleData$AC_result_types,
        "result_type",
        "result_type_id"
      )
      sample_fractions <- ac_option_rows(
        scope,
        moduleData$AC_sample_fractions,
        "sample_fraction_id",
        "sample_fraction_id"
      )
      result_value_types <- ac_option_rows(
        scope,
        moduleData$AC_result_value_types,
        "result_value_type",
        "result_value_type_id"
      )
      result_speciations <- ac_option_rows(
        scope,
        moduleData$AC_result_speciations,
        "result_speciation_id",
        "result_speciation_id"
      )

      sub_loc_choices <- stats::setNames(
        sub_locs$sub_location_id,
        sub_locs[, tr("sub_location_col", language$language)]
      )
      sample_type_choices <- stats::setNames(
        sample_types$sample_type_id,
        sample_types[, tr("sample_type_col", language$language)]
      )
      collection_method_choices <- stats::setNames(
        collection_methods$collection_method_id,
        collection_methods$collection_method
      )
      result_type_choices <- stats::setNames(
        result_types$result_type_id,
        titleCase(result_types$result_type, plot_lang)
      )
      sample_fraction_choices <- stats::setNames(
        sample_fractions$sample_fraction_id,
        titleCase(sample_fractions$sample_fraction, plot_lang)
      )
      result_value_type_choices <- stats::setNames(
        result_value_types$result_value_type_id,
        titleCase(result_value_types$result_value_type, plot_lang)
      )
      result_speciation_choices <- stats::setNames(
        result_speciations$result_speciation_id,
        result_speciations$result_speciation
      )

      blank_available <- any(
        grepl("blank", sample_types$sample_type, ignore.case = TRUE)
      )
      duplicate_available <- any(
        grepl(
          "duplicate|replicate",
          sample_types$sample_type,
          ignore.case = TRUE
        )
      )

      advanced_inputs <- Filter(
        Negate(is.null),
        list(
          ac_selectize(
            "sub_locations_AC",
            tr("sub_loc(s)", language$language),
            sub_loc_choices,
            input$sub_locations_AC
          ),
          ac_selectize(
            "sample_types_AC",
            tr("sample_type(s)", language$language),
            sample_type_choices,
            input$sample_types_AC
          ),
          ac_selectize(
            "collection_methods_AC",
            "Collection method(s)",
            collection_method_choices,
            input$collection_methods_AC
          ),
          ac_selectize(
            "result_speciations_AC",
            "Result speciation(s)",
            result_speciation_choices,
            input$result_speciations_AC
          )
        )
      )
      primary_inputs <- Filter(
        Negate(is.null),
        list(
          ac_selectize(
            "sample_fractions_AC",
            "Sample fraction(s)",
            sample_fraction_choices,
            input$sample_fractions_AC
          ),
          ac_selectize(
            "result_value_types_AC",
            "Result value type(s)",
            result_value_type_choices,
            input$result_value_types_AC
          ),
          ac_selectize(
            "result_types_AC",
            "Result type(s)",
            result_type_choices,
            input$result_types_AC
          )
        )
      )

      tagList(
        tags$hr(),
        tags$h6("Sample/result filters"),
        tagList(primary_inputs),
        if (blank_available) {
          checkboxInput(
            ns("include_blanks"),
            "Show blank samples",
            value = if (is.null(input$include_blanks)) {
              TRUE
            } else {
              isTRUE(input$include_blanks)
            }
          )
        },
        if (duplicate_available) {
          radioButtons(
            ns("duplicate_action"),
            "Duplicate/replicate samples",
            choices = c(
              "Show individually" = "show",
              "Average matching samples" = "average",
              "Hide duplicate/replicate samples" = "hide"
            ),
            selected = if (!is.null(input$duplicate_action)) {
              input$duplicate_action
            } else {
              "show"
            }
          )
        },
        if (
          length(primary_inputs) == 0 &&
            length(advanced_inputs) == 0 &&
            !blank_available &&
            !duplicate_available
        ) {
          tags$small(
            class = "text-muted",
            "No additional filters are available for the current selection."
          )
        },
        if (length(advanced_inputs) > 0) {
          accordion(
            id = ns("AC_advanced_options"),
            open = character(0),
            accordion_panel(
              title = "More sample/result filters",
              tagList(advanced_inputs)
            )
          )
        }
      )
    }) %>%
      bindEvent(
        language$language,
        input$data_source,
        input$locations_AC,
        input$media_AC,
        input$date_range_AC,
        input$parameters_AC,
        ignoreNULL = FALSE
      )

    output$main <- renderUI({
      plot_outputs <- tagList(
        plotly::plotlyOutput(
          ns("plot"),
          width = "100%",
          height = if (
            identical(input$data_source, "AC") &&
              identical(input$AC_selector_mode, "browse")
          ) {
            "650px"
          } else {
            "800px"
          },
          inline = TRUE
        ),
        page_fluid(
          div(
            class = "d-inline-block",
            actionButton(
              ns("full_screen"),
              tr("full_screen", language$language)
            ),
            style = "display: none;"
          ),
          div(
            class = "d-inline-block",
            downloadButton(
              ns("download_data"),
              tr("dl_data", language$language)
            ),
            style = "display: none;"
          )
        )
      )

      if (
        identical(input$data_source, "AC") &&
          identical(input$AC_selector_mode, "browse")
      ) {
        tagList(
          layout_columns(
            col_widths = c(8, 4),
            card(
              full_screen = TRUE,
              card_header("Samples"),
              DT::dataTableOutput(ns("AC_sample_table"))
            ),
            card(
              card_header("Selected samples and parameters"),
              uiOutput(ns("AC_browse_parameter_ui")),
              tags$hr(),
              uiOutput(ns("AC_selected_samples_ui"))
            )
          ),
          tags$hr(),
          plot_outputs
        )
      } else {
        plot_outputs
      }
    }) %>% # End renderUI
      bindEvent(language$language, input$data_source, input$AC_selector_mode)

    output$AC_sample_table <- DT::renderDataTable({
      samples <- ac_browse_sample_table()
      validate(need(nrow(samples) > 0, "No samples match the current filters."))

      column_labels <- c(
        sample_id = "sample_id",
        location = tr("loc", language$language),
        location_code = tr("code", language$language),
        sub_location = tr("sub_loc", language$language),
        sample_date = tr("date", language$language),
        media = tr("media", language$language),
        sample_type = "Sample type",
        collection_method = "Collection method",
        result_count = "Results",
        parameters = tr("parameters", language$language)
      )

      visible_cols <- names(samples)
      DT::datatable(
        samples,
        rownames = FALSE,
        selection = list(mode = "multiple", selected = NULL),
        colnames = unname(column_labels[visible_cols]),
        filter = "top",
        options = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 25, 50),
          columnDefs = list(list(visible = FALSE, targets = 0)),
          scrollX = TRUE,
          order = list(list(match("sample_date", visible_cols) - 1, "desc")),
          initComplete = htmlwidgets::JS(
            sprintf(
              "function(settings, json) {
                 var api = this.api();
                 $(api.table().header()).css({'font-size': '90%%'});
                 $(api.table().body()).css({'font-size': '80%%'});
                 setTimeout(function() {
                   $(api.table().container())
                     .find('thead input[type=\"search\"]')
                     .attr('placeholder', '%s');
                 }, 0);
               }",
              tr("all_m", language$language)
            )
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
          stateSave = FALSE
        )
      )
    })

    observeEvent(input$AC_sample_table_rows_selected, {
      samples <- ac_browse_sample_table()
      rows <- input$AC_sample_table_rows_selected
      if (is.null(rows) || length(rows) == 0 || nrow(samples) == 0) {
        return()
      }
      rows <- rows[rows <= nrow(samples)]
      sample_ids <- unique(c(
        browse_selected_sample_ids(),
        as.numeric(samples$sample_id[rows])
      ))
      browse_selected_sample_ids(sample_ids[!is.na(sample_ids)])
    })

    observeEvent(input$AC_selector_mode, {
      if (!identical(input$AC_selector_mode, "browse")) {
        return()
      }
      shinyjs::hide("full_screen")
      shinyjs::hide("download_data")
    }, ignoreInit = TRUE)

    observeEvent(input$clear_selected_samples, {
      browse_selected_sample_ids(numeric(0))
      DT::selectRows(DT::dataTableProxy("AC_sample_table"), NULL)
    }, ignoreInit = TRUE)

    lapply(seq_len(50), function(i) {
      observeEvent(
        input[[paste0("remove_selected_sample_", i)]],
        {
          rows <- ac_selected_sample_rows()
          if (i > nrow(rows)) {
            return()
          }
          keep <- setdiff(browse_selected_sample_ids(), rows$sample_id[[i]])
          browse_selected_sample_ids(keep)
        },
        ignoreInit = TRUE
      )
    })

    observeEvent(
      list(browse_selected_sample_ids(), ac_browse_sample_table()),
      {
        samples <- ac_browse_sample_table()
        selected <- browse_selected_sample_ids()
        proxy <- DT::dataTableProxy("AC_sample_table")
        if (nrow(samples) == 0 || length(selected) == 0) {
          DT::selectRows(proxy, NULL)
          return()
        }
        rows <- which(samples$sample_id %in% selected)
        DT::selectRows(proxy, rows)
      },
      ignoreInit = TRUE
    )

    output$AC_browse_parameter_ui <- renderUI({
      req(input$data_source == "AC", input$AC_selector_mode == "browse")

      params <- ac_browse_parameter_choices()
      if (
        is.null(params) ||
          nrow(params) == 0 ||
          !all(c("parameter_id", "param_name", "n") %in% names(params))
      ) {
        params <- data.frame(
          parameter_id = character(),
          param_name = character(),
          n = numeric(),
          stringsAsFactors = FALSE
        )
      }
      selected <- ac_keep_selection(
        input$browse_parameters_AC,
        as.character(params$parameter_id)
      )
      choices <- character(0)
      if (nrow(params) > 0) {
        choices <- stats::setNames(
          as.character(params$parameter_id),
          paste0(params$param_name, " (", params$n, ")")
        )
      }

      tagList(
        if (length(browse_selected_sample_ids()) == 0) {
          tags$small(
            class = "text-muted",
            "Select sample rows to focus this parameter list."
          )
        },
        selectizeInput(
          ns("browse_parameters_AC"),
          "Parameters",
          choices = c(
            stats::setNames("all", tr("all_m", language$language)),
            choices
          ),
          selected = selected,
          multiple = TRUE
        ),
        radioButtons(
          ns("browse_parameter_match"),
          "Sample must include",
          choices = c("Any selected parameter" = "any", "All selected parameters" = "all"),
          selected = if (is.null(input$browse_parameter_match)) {
            "any"
          } else {
            input$browse_parameter_match
          }
        )
      )
    }) %>%
      bindEvent(
        language$language,
        input$data_source,
        input$AC_selector_mode,
        browse_selected_sample_ids(),
        input$browse_date_range,
        ignoreNULL = FALSE
      )

    output$AC_selected_samples_ui <- renderUI({
      rows <- ac_selected_sample_rows()
      count <- nrow(rows)
      if (count == 0) {
        return(tags$small(class = "text-muted", "No samples selected."))
      }

      shown <- utils::head(rows, 50)
      tagList(
        div(
          style = "display: flex; gap: 8px; align-items: center;",
          tags$strong(paste(count, "sample(s) selected")),
          actionButton(
            ns("clear_selected_samples"),
            "Clear",
            class = "btn btn-outline-danger btn-sm"
          )
        ),
        tags$div(
          style = "max-height: 360px; overflow-y: auto; margin-top: 8px;",
          lapply(seq_len(nrow(shown)), function(i) {
            label <- paste(
              shown$location[[i]],
              shown$sample_date[[i]],
              shown$media[[i]],
              paste0("ID: ", shown$sample_id[[i]]),
              sep = " | "
            )
            fluidRow(
              style = "margin-bottom: 6px;",
              column(width = 9, tags$small(label)),
              column(
                width = 3,
                actionButton(
                  ns(paste0("remove_selected_sample_", i)),
                  "Remove",
                  class = "btn btn-outline-secondary btn-sm"
                )
              )
            )
          })
        ),
        if (count > nrow(shown)) {
          tags$small(
            class = "text-muted",
            paste("Showing first", nrow(shown), "selected samples.")
          )
        }
      )
    })

    observeEvent(
      input$data_source,
      {
        if (input$data_source == "AC") {
          shinyjs::hide("EQWin_source_ui")
        } else {
          if (!EQWin_selector()) {
            # Only renders the ui element once
            output$EQWin_source_ui <- renderUI({
              selectizeInput(
                ns("EQWin_source"),
                tr("EQWin_db", language$language),
                choices = stats::setNames(mdb_files, basename(mdb_files)),
                selected = mdb_files[1]
              )
            })
            EQWin_selector(TRUE)
          }
          shinyjs::show("EQWin_source_ui")
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$EQWin_source,
      {
        EQWin <- AccessConnect(input$EQWin_source, silent = TRUE)
        EQ_locs <- DBI::dbGetQuery(
          EQWin,
          paste0("SELECT StnCode, StnDesc FROM eqstns ORDER BY StnCode;")
        )
        EQ_loc_grps <- DBI::dbGetQuery(
          EQWin,
          "SELECT groupname, groupdesc, groupitems FROM eqgroups WHERE dbtablename = 'eqstns' ORDER BY groupname;"
        )
        EQ_params <- DBI::dbGetQuery(
          EQWin,
          paste0(
            "SELECT ParamId, ParamCode, ParamDesc, Units AS unit FROM eqparams ORDER BY ParamDesc;"
          )
        )
        EQ_param_grps <- DBI::dbGetQuery(
          EQWin,
          "SELECT groupname, groupdesc, groupitems FROM eqgroups WHERE dbtablename = 'eqparams' ORDER BY groupname;"
        )
        EQ_stds <- DBI::dbGetQuery(
          EQWin,
          "SELECT StdName, StdCode FROM eqstds ORDER BY StdName;"
        )
        DBI::dbDisconnect(EQWin)

        # Check encoding and if necessary convert to UTF-8
        locale_info <- Sys.getlocale("LC_CTYPE")
        encoding <- sub(".*\\.([^@]+).*", "\\1", locale_info)
        tryCatch(
          {
            grepl("[^\x01-\x7F]", EQ_locs$StnDesc)
          },
          warning = function(w) {
            if (encoding != "utf8") {
              EQ_locs$StnDesc <<- iconv(
                EQ_locs$StnDesc,
                from = encoding,
                to = "UTF-8"
              )
            }
          }
        )

        moduleData$EQ_locs <- EQ_locs
        moduleData$EQ_loc_grps <- EQ_loc_grps
        moduleData$EQ_params <- EQ_params
        moduleData$EQ_param_grps <- EQ_param_grps
        moduleData$EQ_stds <- EQ_stds

        # Update the selectize inputs
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
          server = TRUE,
          selected = character(0)
        )
        updateSelectizeInput(
          session,
          "parameter_groups",
          choices = moduleData$EQ_param_grps$groupname,
          server = TRUE,
          selected = character(0)
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
          server = TRUE,
          selected = character(0)
        )
        updateSelectizeInput(
          session,
          "location_groups",
          choices = moduleData$EQ_loc_grps$groupname,
          server = TRUE,
          selected = character(0)
        )
        updateSelectizeInput(
          session,
          "standard",
          choices = stats::setNames(
            moduleData$EQ_stds$StdCode,
            moduleData$EQ_stds$StdName
          ),
          server = TRUE,
          selected = character(0)
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    # Helper function to update the list of available parameters based on selected locations
    update_parameters <- function() {
      if (
        !identical(input$data_source, "AC") ||
          identical(input$AC_selector_mode, "browse")
      ) {
        return(invisible(NULL))
      }
      params <- ac_available_parameters()
      selected <- input$parameters_AC[
        input$parameters_AC %in% params$parameter_id
      ]
      updateSelectizeInput(
        session,
        "parameters_AC",
        choices = stats::setNames(params$parameter_id, params$param_name),
        selected = selected,
        server = TRUE
      )
    }

    observeEvent(input$data_source, {
      req(moduleData)
      if (input$data_source == "EQ") {
        # These updates are performed in the observeEvent for input$EQWin_source
      } else if (input$data_source == "AC") {
        # AC selected
        updateSelectizeInput(
          session,
          "locations_AC",
          choices = stats::setNames(
            moduleData$AC_locs$location_id,
            moduleData$AC_locs$name
          ),
          selected = if (!is.null(inputs$location_id)) {
            inputs$location_id
          } else {
            NULL
          },
          server = TRUE
        )
        update_parameters()
      }
    })

    # Update parameters after the location, sample media, and date range scope is set.
    observeEvent(
      list(input$locations_AC, input$media_AC, input$date_range_AC),
      {
        if (
          identical(input$data_source, "AC") &&
            !identical(input$AC_selector_mode, "browse")
        ) {
          update_parameters()
        }
      },
      ignoreNULL = FALSE
    )

    # Toggle visibility of location and location group inputs
    observeEvent(input$locs_groups, {
      if (input$locs_groups == "loc_groups") {
        shinyjs::show("location_groups")
        shinyjs::hide("locations_EQ")
      } else {
        shinyjs::hide("location_groups")
        shinyjs::show("locations_EQ")
      }
    })
    observeEvent(input$params_groups, {
      if (input$params_groups == "param_groups") {
        shinyjs::show("parameter_groups")
        shinyjs::hide("parameters_EQ")
      } else {
        shinyjs::hide("parameter_groups")
        shinyjs::show("parameters_EQ")
      }
    })

    # Modal dialog for extra aesthetics  ####

    # Create a list with default aesthetic values
    plot_aes <- reactiveValues(
      lang = "en",
      showgridx = FALSE,
      showgridy = FALSE,
      colorblind = FALSE,
      nrows = NULL,
      point_scale = 1,
      guideline_scale = 1,
      axis_scale = 1,
      legend_scale = 1
    )

    observeEvent(input$extra_aes, {
      showModal(modalDialog(
        title = tr("modify_plot_aes", language$language),
        tags$div(
          tags$h5(tr("language", language$language)),
          radioButtons(
            ns("lang"),
            NULL,
            choices = stats::setNames(
              c("en", "fr"),
              c(
                tr("english", language$language),
                tr("francais", language$language)
              )
            ),
            selected = plot_aes$lang
          ),
          checkboxInput(
            ns("showgridx"),
            tr("show_x_grid", language$language),
            value = plot_aes$showgridx
          ),
          checkboxInput(
            ns("showgridy"),
            tr("show_y_grid", language$language),
            value = plot_aes$showgridy
          ),
          numericInput(
            ns("nrows"),
            tr("num_rows", language$language),
            value = plot_aes$nrows,
            min = 1
          ),
          checkboxInput(
            ns("colorblind"),
            tr("colorblind_friend", language$language),
            value = plot_aes$colorblind
          ),
          tags$hr(),
          sliderInput(
            ns("point_scale"),
            tr("point_scale", language$language),
            min = 0.2,
            max = 3,
            value = plot_aes$point_scale,
            step = 0.1
          ),
          sliderInput(
            ns("guideline_scale"),
            tr("guideline_scale", language$language),
            min = 0.2,
            max = 3,
            value = plot_aes$guideline_scale,
            step = 0.1
          ),
          sliderInput(
            ns("axis_scale"),
            tr("axis_scale", language$language),
            min = 0.2,
            max = 3,
            value = plot_aes$axis_scale,
            step = 0.1
          ),
          sliderInput(
            ns("legend_scale"),
            tr("legend_scale", language$language),
            min = 0.2,
            max = 3,
            value = plot_aes$legend_scale,
            step = 0.1
          )
        ),
        easyClose = FALSE,
        footer = tagList(
          actionButton(ns("aes_apply"), tr("apply", language$language)),
          actionButton(ns("cancel"), tr("cancel", language$language))
        )
      ))
    })

    observeEvent(input$aes_apply, {
      plot_aes$lang <- input$lang
      plot_aes$colorblind <- input$colorblind
      plot_aes$showgridx <- input$showgridx
      plot_aes$showgridy <- input$showgridy
      if (!is.na(input$nrows)) {
        plot_aes$nrows <- if (input$nrows > 0) input$nrows else NULL
      }
      plot_aes$point_scale <- input$point_scale
      plot_aes$guideline_scale <- input$guideline_scale
      plot_aes$axis_scale <- input$axis_scale
      plot_aes$legend_scale <- input$legend_scale
      removeModal()
    })

    observeEvent(input$cancel, {
      removeModal()
    })

    ac_guided_season_ranges <- function() {
      if (!isTRUE(input$season_filter_enabled)) {
        return(NULL)
      }
      count <- suppressWarnings(as.integer(input$season_range_count))
      if (length(count) == 0 || is.na(count)) {
        count <- 1L
      }
      count <- max(1L, min(count, 3L))
      ranges <- lapply(seq_len(count), function(i) {
        range <- input[[paste0("season_range_", i)]]
        if (is.null(range) || length(range) != 2) {
          return(NULL)
        }
        as.Date(range)
      })
      ranges <- Filter(Negate(is.null), ranges)
      if (length(ranges) == 0) {
        return(NULL)
      }
      ranges
    }

    # Create and render the plot ############################################################
    ## ExtendedTask for plot generation ######################################################
    plot_output_discrete <- ExtendedTask$new(
      function(
        start,
        end,
        locations,
        locGrp,
        parameters,
        paramGrp,
        standard,
        log,
        facet_on,
        loc_code,
        shareX,
        shareY,
        rows,
        target_datetime,
        colorblind,
        lang,
        point_scale,
        guideline_scale,
        axis_scale,
        legend_scale,
        legend_position,
        gridx,
        gridy,
        sub_location_ids,
        media,
        sample_types,
        collection_methods,
        result_types,
        sample_fractions,
        result_value_types,
        result_speciations,
        include_blanks,
        duplicate_action,
        sample_ids,
        season_ranges,
        dbSource,
        dbPath,
        config
      ) {
        promises::future_promise({
          tryCatch(
            {
              if (is.null(dbPath)) {
                con <- AquaConnect(
                  name = config$dbName,
                  host = config$dbHost,
                  port = config$dbPort,
                  username = config$dbUser,
                  password = config$dbPass,
                  silent = TRUE
                )
                on.exit(DBI::dbDisconnect(con))
              } else {
                con = NULL
              }

              ensure_discrete_plot_function()

              plot <- plotDiscrete(
                start = start,
                end = end,
                locations = locations,
                locGrp = locGrp,
                parameters = parameters,
                paramGrp = paramGrp,
                standard = standard,
                log = log,
                facet_on = facet_on,
                loc_code = loc_code,
                shareX = shareX,
                shareY = shareY,
                rows = rows,
                target_datetime = target_datetime,
                colorblind = colorblind,
                lang = lang,
                point_scale = point_scale,
                guideline_scale = guideline_scale,
                axis_scale = axis_scale,
                legend_scale = legend_scale,
                legend_position = legend_position,
                gridx = gridx,
                gridy = gridy,
                sub_location_ids = sub_location_ids,
                media = media,
                sample_types = sample_types,
                collection_methods = collection_methods,
                result_types = result_types,
                sample_fractions = sample_fractions,
                result_value_types = result_value_types,
                result_speciations = result_speciations,
                include_blanks = include_blanks,
                duplicate_action = duplicate_action,
                sample_ids = sample_ids,
                season_ranges = season_ranges,
                dbSource = dbSource,
                dbPath = dbPath,
                dbCon = con,
                data = TRUE
              )
              return(plot)
            },
            error = function(e) {
              return(e$message)
            }
          ) # End of tryCatch
        }) # End of future_promise
      }
    ) |>
      bind_task_button("make_plot")
    # --- End ExtendedTask -------------------------------------------------------------------

    observeEvent(
      input$make_plot,
      {
        shinyjs::hide("full_screen")
        shinyjs::hide("download_data")

        # Validate required inputs based on data source
        if (input$data_source == "EQ") {
          if (input$locs_groups == "locations") {
            if (is.null(input$locations_EQ)) {
              showModal(modalDialog(
                tr("pl_select_loc", language$language),
                footer = tagList(
                  actionButton(ns("cancel"), tr("cancel", language$language))
                ),
                easyClose = TRUE
              ))
              return()
            }
          } else {
            if (is.null(input$location_groups)) {
              showModal(modalDialog(
                tr("select_loc_group_msg", language$language),
                footer = tagList(
                  actionButton(ns("cancel"), tr("cancel", language$language))
                ),
                easyClose = TRUE
              ))
              return()
            }
          }
          # Same treatment for parameters/parameter_groups
          if (input$params_groups == "parameters") {
            if (is.null(input$parameters_EQ)) {
              showModal(modalDialog(
                tr("pl_select_param", language$language),
                footer = tagList(
                  actionButton(ns("cancel"), tr("cancel", language$language))
                ),
                easyClose = TRUE
              ))
              return()
            }
          } else {
            if (is.null(input$parameter_groups)) {
              showModal(modalDialog(
                tr("select_param_group_msg", language$language),
                footer = tagList(
                  actionButton(ns("cancel"), tr("cancel", language$language))
                ),
                easyClose = TRUE
              ))
              return()
            }
          }
        } else if (input$data_source == "AC") {
          ac_mode <- if (is.null(input$AC_selector_mode)) {
            "guided"
          } else {
            input$AC_selector_mode
          }
          if (identical(ac_mode, "browse")) {
            if (length(browse_selected_sample_ids()) == 0) {
              showModal(modalDialog(
                "Please select one or more samples from the table.",
                footer = tagList(
                  actionButton(ns("cancel"), tr("cancel", language$language))
                ),
                easyClose = TRUE
              ))
              return()
            }
          } else {
            if (is.null(input$locations_AC)) {
              showModal(modalDialog(
                tr("pl_select_loc", language$language),
                footer = tagList(
                  actionButton(ns("cancel"), tr("cancel", language$language))
                ),
                easyClose = TRUE
              ))
              return()
            }
            if (
              is.null(input$date_range_AC) ||
                length(input$date_range_AC) != 2
            ) {
              showModal(modalDialog(
                tr("date_range_lab", language$language),
                footer = tagList(
                  actionButton(ns("cancel"), tr("cancel", language$language))
                ),
                easyClose = TRUE
              ))
              return()
            }
            if (is.null(input$parameters_AC)) {
              showModal(modalDialog(
                tr("pl_select_param", language$language),
                footer = tagList(
                  actionButton(ns("cancel"), tr("cancel", language$language))
                ),
                easyClose = TRUE
              ))
              return()
            }
          }
        }

        if (input$data_source == "EQ") {
          plot_output_discrete$invoke(
            start = input$date_range_EQ[1],
            end = input$date_range_EQ[2],
            locations = if (input$locs_groups == "locations") {
              input$locations_EQ
            } else {
              NULL
            },
            locGrp = if (input$locs_groups == "loc_groups") {
              input$location_groups
            } else {
              NULL
            },
            parameters = if (input$params_groups == "parameters") {
              input$parameters_EQ
            } else {
              NULL
            },
            paramGrp = if (input$params_groups == "param_groups") {
              input$parameter_groups
            } else {
              NULL
            },
            standard = if (length(input$standard) == 0) {
              NULL
            } else {
              input$standard
            },
            log = input$log_scale,
            facet_on = input$facet_on,
            loc_code = input$loc_code,
            shareX = input$shareX,
            shareY = input$shareY,
            rows = if (is.null(plot_aes$nrows)) "auto" else plot_aes$nrows,
            target_datetime = input$target_datetime,
            colorblind = plot_aes$colorblind,
            lang = plot_aes$lang,
            point_scale = plot_aes$point_scale,
            guideline_scale = plot_aes$guideline_scale,
            axis_scale = plot_aes$axis_scale,
            legend_scale = plot_aes$legend_scale,
            legend_position = if (
              windowDims()$width > 1.3 * windowDims()$height
            ) {
              "v"
            } else {
              "h"
            },
            gridx = plot_aes$showgridx,
            gridy = plot_aes$showgridy,
            sub_location_ids = NULL,
            media = NULL,
            sample_types = NULL,
            collection_methods = NULL,
            result_types = NULL,
            sample_fractions = NULL,
            result_value_types = NULL,
            result_speciations = NULL,
            include_blanks = TRUE,
            duplicate_action = "show",
            sample_ids = NULL,
            season_ranges = NULL,
            dbSource = input$data_source,
            dbPath = input$EQWin_source, # EQWin connection so no need to pass config
            config = NULL # EQWin connection so no need to pass config
          )
        } else if (input$data_source == "AC") {
          ac_mode <- if (is.null(input$AC_selector_mode)) {
            "guided"
          } else {
            input$AC_selector_mode
          }
          scope <- isolate(ac_scope())
          keep_ac_ids <- function(values, lookup, scope_col, lookup_col) {
            if (is.null(values) || length(values) == 0) {
              return(NULL)
            }
            values <- as.character(values)
            if ("all" %in% values) {
              return(NULL)
            }
            allowed <- as.character(
              ac_option_rows(scope, lookup, scope_col, lookup_col)[[lookup_col]]
            )
            values <- values[values %in% allowed]
            if (length(values) == 0) {
              return(NULL)
            }
            as.numeric(values)
          }

          browse_sample_ids <- NULL
          browse_parameters <- NULL
          browse_start <- input$date_range_AC[1]
          browse_end <- input$date_range_AC[2]
          if (identical(ac_mode, "browse")) {
            selected_rows <- ac_selected_sample_rows()
            browse_sample_ids <- browse_selected_sample_ids()
            browse_parameters <- ac_numeric_values(input$browse_parameters_AC)
            if (length(browse_parameters) == 0) {
              browse_parameters <- NULL
            }
            if (nrow(selected_rows) > 0) {
              selected_dates <- as.Date(selected_rows$sample_date)
              browse_start <- min(selected_dates, na.rm = TRUE) - 1
              browse_end <- max(selected_dates, na.rm = TRUE) + 1
            } else {
              browse_start <- input$browse_date_range[1]
              browse_end <- input$browse_date_range[2]
            }
          }
          use_guided_filters <- !identical(ac_mode, "browse")

          plot_output_discrete$invoke(
            start = if (identical(ac_mode, "browse")) {
              browse_start
            } else {
              input$date_range_AC[1]
            },
            end = if (identical(ac_mode, "browse")) {
              browse_end
            } else {
              input$date_range_AC[2]
            },
            locations = if (identical(ac_mode, "browse")) {
              NULL
            } else {
              as.numeric(input$locations_AC)
            },
            locGrp = NULL,
            parameters = if (identical(ac_mode, "browse")) {
              browse_parameters
            } else {
              as.numeric(input$parameters_AC)
            },
            paramGrp = NULL,
            standard = NULL, # No standards in AquaCache yet
            log = input$log_scale,
            facet_on = input$facet_on,
            loc_code = input$loc_code,
            shareX = input$shareX,
            shareY = input$shareY,
            rows = if (is.null(plot_aes$nrows)) "auto" else plot_aes$nrows,
            target_datetime = input$target_datetime,
            colorblind = plot_aes$colorblind,
            lang = plot_aes$lang,
            point_scale = plot_aes$point_scale,
            guideline_scale = plot_aes$guideline_scale,
            axis_scale = plot_aes$axis_scale,
            legend_scale = plot_aes$legend_scale,
            legend_position = if (
              windowDims()$width > 1.3 * windowDims()$height
            ) {
              "v"
            } else {
              "h"
            },
            gridx = plot_aes$showgridx,
            gridy = plot_aes$showgridy,
            sub_location_ids = if (use_guided_filters) keep_ac_ids(
              input$sub_locations_AC,
              moduleData$AC_sub_locs,
              "sub_location_id",
              "sub_location_id"
            ) else NULL,
            media = if (use_guided_filters) keep_ac_ids(
              input$media_AC,
              moduleData$AC_media,
              "media_id",
              "media_id"
            ) else NULL,
            sample_types = if (use_guided_filters) keep_ac_ids(
              input$sample_types_AC,
              moduleData$AC_sample_types,
              "sample_type",
              "sample_type_id"
            ) else NULL,
            collection_methods = if (use_guided_filters) keep_ac_ids(
              input$collection_methods_AC,
              moduleData$AC_collection_methods,
              "collection_method",
              "collection_method_id"
            ) else NULL,
            result_types = if (use_guided_filters) keep_ac_ids(
              input$result_types_AC,
              moduleData$AC_result_types,
              "result_type",
              "result_type_id"
            ) else NULL,
            sample_fractions = if (use_guided_filters) keep_ac_ids(
              input$sample_fractions_AC,
              moduleData$AC_sample_fractions,
              "sample_fraction_id",
              "sample_fraction_id"
            ) else NULL,
            result_value_types = if (use_guided_filters) keep_ac_ids(
              input$result_value_types_AC,
              moduleData$AC_result_value_types,
              "result_value_type",
              "result_value_type_id"
            ) else NULL,
            result_speciations = if (use_guided_filters) keep_ac_ids(
              input$result_speciations_AC,
              moduleData$AC_result_speciations,
              "result_speciation_id",
              "result_speciation_id"
            ) else NULL,
            include_blanks = if (!use_guided_filters || is.null(input$include_blanks)) {
              TRUE
            } else {
              isTRUE(input$include_blanks)
            },
            duplicate_action = if (!use_guided_filters || is.null(input$duplicate_action)) {
              "show"
            } else {
              input$duplicate_action
            },
            sample_ids = browse_sample_ids,
            season_ranges = if (use_guided_filters) {
              ac_guided_season_ranges()
            } else {
              NULL
            },
            dbSource = input$data_source,
            dbPath = NULL, # AquaCache connection so no need to pass database path
            config = session$userData$config
          )
        }
      },
      ignoreInit = TRUE
    ) # End of plot rendering loop

    # flags
    plot_created <- reactiveVal(FALSE) # Flags if a plot has been created so that window dimensions can be checked for legend position
    first_plot <- reactiveVal(TRUE) # Flags if this is the first plot generated by the user in this session, in which case a modal is shown
    first_plot_with_standards <- reactiveVal(TRUE) # Flags if this is the first plot generated by the user in this session with standards, in which case a modal is shown
    plotData <- reactiveVal() # Holds the data for the plot in case the user wants to download it

    observeEvent(plot_output_discrete$result(), {
      if (inherits(plot_output_discrete$result(), "character")) {
        showModal(modalDialog(
          title = tr("error", language$language),
          plot_output_discrete$result(),
          footer = tagList(
            actionButton(ns("cancel"), tr("cancel", language$language))
          ),
          easyClose = TRUE
        ))
        return()
      }
      output$plot <- plotly::renderPlotly({
        isolate(plot_output_discrete$result()$plot)
      })
      plotData(plot_output_discrete$result()$data)

      shinyjs::show("full_screen")
      shinyjs::show("download_data")

      # If this is the first plot generated by the user in this session show them a modal
      if (first_plot()) {
        if (first_plot_with_standards()) {
          showModal(
            modalDialog(
              HTML(tr("first_plot_hints_standards", language$language)),
              footer = tagList(
                actionButton(ns("cancel"), tr("cancel", language$language))
              ),
              easyClose = TRUE
            )
          )
          first_plot_with_standards(FALSE)
        } else {
          showModal(
            modalDialog(
              HTML(tr("first_plot_hints_no_standards", language$language)),
              footer = tagList(
                actionButton(ns("cancel"), tr("cancel", language$language))
              ),
              easyClose = TRUE
            )
          )
        }
        first_plot(FALSE)
      }

      if (first_plot_with_standards()) {
        showModal(
          modalDialog(
            HTML(tr("first_plot_hints_standards_short", language$language)),
            footer = tagList(
              actionButton(ns("cancel"), tr("cancel", language$language))
            ),
            easyClose = TRUE
          )
        )
        first_plot_with_standards(FALSE)
      }
    })

    # Observe changes to the windowDims reactive value and update the legend position using plotlyProxy
    # The js function takes care of debouncing the window resize event and also reacts to a change in orientation or full screen event

    observeEvent(
      windowDims(),
      {
        req(plot_created())
        if (is.null(windowDims())) {
          return()
        }
        if (windowDims()$width > 1.3 * windowDims()$height) {
          plotly::plotlyProxy("plot", session) %>%
            plotly::plotlyProxyInvoke(
              "relayout",
              legend = list(orientation = "v")
            )
        } else {
          plotly::plotlyProxy("plot", session) %>%
            plotly::plotlyProxyInvoke(
              "relayout",
              legend = list(orientation = "h")
            )
        }
      },
      ignoreNULL = TRUE
    )

    # Observe the full screen button and run the javascript function to make the plot full screen
    observeEvent(
      input$full_screen,
      {
        shinyjs::runjs(paste0("toggleFullScreen('", ns("plot"), "');"))

        # Manually trigger a window resize event after some delay
        shinyjs::runjs(
          "
                      setTimeout(function() {
                        sendWindowSizeToShiny();
                      }, 700);
                    "
        )
      },
      ignoreInit = TRUE
    )

    # Send the user the plotting data
    output$download_data <- downloadHandler(
      filename = function() {
        time <- Sys.time()
        attr(time, "tzone") <- "UTC"
        paste0(
          "discrete_plot_data_",
          gsub("-", "", gsub(" ", "_", gsub(":", "", substr(time, 0, 16)))),
          "_UTC.xlsx"
        )
      },
      content = function(file) {
        openxlsx::write.xlsx(plotData(), file)
      }
    )
  }) # End of moduleServer
}
