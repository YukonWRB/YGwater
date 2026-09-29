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
    card(
      card_body(
        class = "custom-card",
        uiOutput(ns("data_source_ui")),
        dateInput(ns("date"), "Report Date", value = Sys.Date() - 30),
        div(
          style = "display: flex; align-items: center;",
          tags$label(
            "Look for data within this many days of the report date",
            class = "form-label",
            style = "margin-right: 5px;"
          ),
          span(
            id = ns("date_approx_info"),
            `data-bs-toggle` = "tooltip",
            `data-bs-placement` = "right",
            `data-bs-trigger` = "click hover",
            title = "Use this to include data up to a certain number of days of the report date. For example, you may want a single report with data from multiple locations sampled within 2-3 days. If multiple samples for a location fall within the date range, the one closest to the report date will be used.",
            icon("info-circle", style = "font-size: 150%; margin-left: 5px;")
          )
        ),
        numericInput(ns("date_approx"), NULL, value = 1),
        conditionalPanel(
          ns = ns,
          condition = "input.data_source == 'EQ'",
          uiOutput(ns("EQWin_source_ui")),
          # Toggle button for locations or location groups (only show if data source  == EQWin)
          radioButtons(
            ns("locs_groups"),
            NULL,
            choices = c("Locations", "Location Groups"),
            selected = "Locations"
          ),
          # Selectize input for locations, populated once connection is established
          selectizeInput(
            ns("locations_EQ"),
            "Select locations",
            choices = "Placeholder",
            multiple = TRUE
          ),
          # Selectize input for location groups, populated once connection is established. only shown if data source is EQWin
          selectizeInput(
            ns("location_groups"),
            "Select a location group",
            choices = "Placeholder",
            multiple = FALSE,
            width = "100%"
          ),

          # Toggle button for parameters or parameter groups (only show if data source == EQWin)
          radioButtons(
            ns("params_groups"),
            NULL,
            choices = c("Parameters", "Parameter Groups"),
            selected = "Parameters"
          ),
          # Selectize input for parameters, populated once connection is established
          selectizeInput(
            ns("parameters_EQ"),
            "Select parameters",
            choices = "Placeholder",
            multiple = TRUE,
            width = "100%"
          ),
          # Selectize input for parameter groups, populated once connection is established. only shown if data source is EQWin
          selectizeInput(
            ns("parameter_groups"),
            "Select a parameter group",
            choices = "Placeholder",
            multiple = FALSE,
            width = "100%"
          ),

          # Add a bit of space between the mandatory inputs and the optional ones
          tags$br(),

          # Selectize input for standards, populated once connection is established
          htmlOutput(ns("standard_note")),
          selectizeInput(
            ns("stds"),
            "Select one or more standards to apply (optional)",
            choices = "Placeholder",
            multiple = TRUE,
            width = "100%"
          ),
          # TRUE/FALSE selection for station-specific standards
          checkboxInput(
            ns("stnStds"),
            "Apply station-specific standards?",
            value = FALSE
          )
        ),

        conditionalPanel(
          ns = ns,
          condition = "input.data_source == 'AC' || input.data_source == null",
          # Selectize input for locations, populated once connection is established
          selectizeInput(
            ns("locations_AC"),
            "Select locations",
            choices = character(),
            multiple = TRUE,
            width = "100%"
          ),
          # Selectize input for parameters, populated once connection is established
          selectizeInput(
            ns("parameters_AC"),
            "Select parameters",
            choices = character(),
            multiple = TRUE,
            width = "100%"
          ),
          uiOutput(ns("AC_guidelines_ui"))
        ),
        uiOutput(ns("SD_inputs_ui")),
        bslib::input_task_button(
          ns("go"),
          "Create report",
          label_busy = "Working..."
        ),
        downloadButton(
          ns("download"),
          "download",
          style = "visibility: hidden;"
        ) # Hidden; triggered automatically if 'go' is successful
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

    resolve_eqwin_source <- function(value) {
      if (
        length(value) != 1L ||
          is.na(value) ||
          !nzchar(value) ||
          !value %in% configured_mdb_files
      ) {
        stop("Select a configured EQWin database.", call. = FALSE)
      }
      normalizePath(
        configured_mdb_files[match(value, configured_mdb_files)],
        winslash = "/",
        mustWork = TRUE
      )
    }

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "WQReport"
      )
    })

    output$data_source_ui <- renderUI({
      if (!eqwin_available) {
        return(NULL)
      }
      radioButtons(
        ns("data_source"),
        NULL,
        choices = stats::setNames(c("AC", "EQ"), c("AquaCache", "EQWin")),
        selected = "EQ"
      )
    })

    output$EQWin_source_ui <- renderUI({
      if (!eqwin_available) {
        return(NULL)
      }
      selectizeInput(
        ns("EQWin_source"),
        "EQWin database",
        choices = stats::setNames(
          configured_mdb_files,
          basename(configured_mdb_files)
        ),
        selected = configured_mdb_files[[1]]
      )
    })

    output$SD_inputs_ui <- renderUI({
      tagList(
        tags$br(),
        htmlOutput(ns("SD_note")),
        tags$label(
          "Standard deviation threshold (leave empty to not calculate)",
          class = "form-label"
        ),
        numericInput(ns("SD_SD"), NULL, value = NULL),
        tags$label("Start date for SD calculation", class = "form-label"),
        dateInput(ns("SD_start"), NULL, value = NA),
        tags$label("End date for SD calculation", class = "form-label"),
        dateInput(ns("SD_end"), NULL, value = NA),
        tags$label("Select date range (year is ignored)", class = "form-label"),
        dateRangeInput(
          ns("SD_date_range"),
          label = NULL,
          start = "2000-01-01",
          end = "2000-12-31",
          format = "yyyy-mm-dd"
        )
      )
    })

    output$standard_note <- renderUI({
      HTML(
        "<p>
      <i><b>Optional:</b> Select standards/guidelines and station specific standards/guidelines to apply.<br>
      General standards show up as an additional column in the report with values for each parameter. <br>
      Station-specific standards show up as notes in the report for each station.<br>
      Reported values which exceed standards/guidelines are highlighted in red with a note provided.
      </p>"
      )
    })
    output$SD_note <- renderUI({
      HTML(
        "<p>
      <i><b>Optional:</b> Select a standard deviation threshold to flag outlier values.<br>
      A mean and standard deviation will be calculated using past measurements if they exist.<br>
      <b>This can add a lot of time to the report generation process, be patient!</b>
      </p>"
      )
    })

    moduleData <- reactiveValues()
    ac_metadata_loaded <- reactiveVal(FALSE)

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
                "SELECT l.location_id, l.location_code, l.name,",
                "COALESCE(l.name_fr, l.name, l.location_code) AS name_fr",
                "FROM public.locations AS l",
                "WHERE EXISTS (",
                "SELECT 1 FROM discrete.samples AS s",
                "INNER JOIN discrete.results AS r ON r.sample_id = s.sample_id",
                "WHERE s.location_id = l.location_id",
                ")",
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
            moduleData$AC_guidelines <- tryCatch(
              DBI::dbGetQuery(
                con,
                paste(
                  "SELECT g.guideline_id, g.guideline_code,",
                  "g.guideline_name, p.param_name, gp.publisher_name",
                  "FROM criteria.guidelines AS g",
                  "INNER JOIN public.parameters AS p",
                  "ON p.parameter_id = g.parameter_id",
                  "LEFT JOIN criteria.guideline_publishers AS gp",
                  "ON gp.publisher_id = g.publisher_id",
                  "WHERE g.active AND g.review_status = 'approved'",
                  "ORDER BY p.param_name, g.guideline_code, g.guideline_name"
                )
              ),
              error = function(e) {
                data.frame(
                  guideline_id = integer(),
                  guideline_code = character(),
                  guideline_name = character(),
                  param_name = character(),
                  publisher_name = character()
                )
              }
            )
            ac_metadata_loaded(TRUE)
          },
          error = function(e) {
            showNotification(
              paste("Unable to load AquaCache report choices:", e$message),
              type = "error",
              duration = NULL,
              closeButton = TRUE
            )
          }
        )
      },
      ignoreNULL = FALSE
    )

    observe({
      if (!identical(selected_data_source(), "AC") || !ac_metadata_loaded()) {
        return()
      }
      locations <- moduleData$AC_locs
      parameters <- moduleData$AC_params
      if (is.null(locations) || is.null(parameters)) {
        return()
      }

      location_names <- if (identical(language$language, "Français")) {
        locations$name_fr
      } else {
        locations$name
      }
      missing_names <- is.na(location_names) | !nzchar(location_names)
      location_names[missing_names] <- locations$location_code[missing_names]
      location_labels <- paste0(
        locations$location_code,
        " (",
        location_names,
        ")"
      )

      parameter_names <- if (identical(language$language, "Français")) {
        parameters$param_name_fr
      } else {
        parameters$param_name
      }
      missing_names <- is.na(parameter_names) | !nzchar(parameter_names)
      parameter_names[missing_names] <- parameters$param_name[missing_names]

      updateSelectizeInput(
        session,
        "locations_AC",
        choices = stats::setNames(
          as.character(locations$location_id),
          location_labels
        ),
        server = TRUE
      )
      updateSelectizeInput(
        session,
        "parameters_AC",
        choices = stats::setNames(
          as.character(parameters$parameter_id),
          parameter_names
        ),
        server = TRUE
      )
    })

    output$AC_guidelines_ui <- renderUI({
      if (!identical(selected_data_source(), "AC")) {
        return(NULL)
      }
      if (!ac_metadata_loaded()) {
        return(tags$p("Loading AquaCache guidelines..."))
      }

      guidelines <- moduleData$AC_guidelines
      if (is.null(guidelines)) {
        return(NULL)
      }
      code <- ifelse(
        is.na(guidelines$guideline_code) |
          !nzchar(guidelines$guideline_code),
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
        is.na(guidelines$publisher_name) |
          !nzchar(guidelines$publisher_name),
        labels,
        paste0(labels, " | ", guidelines$publisher_name)
      )
      choices <- stats::setNames(
        as.character(guidelines$guideline_id),
        labels
      )
      selected <- input$guidelines_AC
      if (length(selected)) {
        selected <- selected[selected %in% unname(choices)]
      }

      selectizeInput(
        ns("guidelines_AC"),
        "Select AquaCache guidelines to apply (optional)",
        choices = choices,
        selected = selected,
        multiple = TRUE,
        width = "100%"
      )
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
            showNotification(e$message, type = "error", duration = 8)
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
              paste("Unable to load EQWin report choices:", e$message),
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
        title = "Cannot Generate Water Quality Report",
        tags$p("Please correct the following before starting the report:"),
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

      report_date <- as.Date(input$date)
      if (length(report_date) != 1L || is.na(report_date)) {
        issues <- c(issues, "Provide a valid report date.")
      }
      if (
        is.null(input$date_approx) ||
          length(input$date_approx) != 1L ||
          is.na(input$date_approx) ||
          input$date_approx < 0 ||
          input$date_approx != trunc(input$date_approx)
      ) {
        issues <- c(
          issues,
          "Days around the report date must be a non-negative whole number."
        )
      }

      if (!is.null(input$SD_SD) && length(input$SD_SD) == 1L && !is.na(input$SD_SD)) {
        if (!is.numeric(input$SD_SD) || input$SD_SD <= 0) {
          issues <- c(
            issues,
            "Standard deviation threshold must be a number greater than zero."
          )
        }
        sd_start <- normalize_optional_date(input$SD_start)
        sd_end <- normalize_optional_date(input$SD_end)
        if (!is.null(sd_start) && !is.null(sd_end) && sd_start > sd_end) {
          issues <- c(
            issues,
            "Standard deviation start date must be on or before the end date."
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
            "Provide a valid day-of-year range for the standard deviation filter."
          )
        }
      }

      if (identical(source, "AC")) {
        if (!isTRUE(ac_metadata_loaded())) {
          issues <- c(
            issues,
            "AquaCache choices are still loading. Please wait and try again."
          )
        } else {
          if (
            is.null(input$locations_AC) ||
              !length(input$locations_AC) ||
              anyNA(input$locations_AC) ||
              !any(nzchar(input$locations_AC))
          ) {
            issues <- c(issues, "Select at least one AquaCache location.")
          } else if (length(setdiff(
            input$locations_AC,
            as.character(moduleData$AC_locs$location_id)
          ))) {
            issues <- c(issues, "One or more selected AquaCache locations are invalid.")
          }
          if (
            is.null(input$parameters_AC) ||
              !length(input$parameters_AC) ||
              anyNA(input$parameters_AC) ||
              !any(nzchar(input$parameters_AC))
          ) {
            issues <- c(issues, "Select at least one AquaCache parameter.")
          } else if (length(setdiff(
            input$parameters_AC,
            as.character(moduleData$AC_params$parameter_id)
          ))) {
            issues <- c(issues, "One or more selected AquaCache parameters are invalid.")
          }
          if (
            length(input$guidelines_AC) &&
              length(setdiff(
                input$guidelines_AC,
                as.character(moduleData$AC_guidelines$guideline_id)
              ))
          ) {
            issues <- c(issues, "One or more selected AquaCache guidelines are invalid.")
          }
        }
      } else {
        if (!eqwin_available) {
          issues <- c(issues, "No configured EQWin database is available.")
        }
        if (
          is.null(input$EQWin_source) ||
            !length(input$EQWin_source) ||
            anyNA(input$EQWin_source) ||
            !nzchar(input$EQWin_source[[1]])
        ) {
          issues <- c(issues, "Select a valid EQWin database.")
        } else if (inherits(
          try(resolve_eqwin_source(input$EQWin_source[[1]]), silent = TRUE),
          "try-error"
        )) {
          issues <- c(issues, "Select an available configured EQWin database.")
        }

        if (
          is.null(moduleData$EQ_loaded_source) ||
            is.null(moduleData$EQ_locs) ||
            is.null(moduleData$EQ_params)
        ) {
          issues <- c(
            issues,
            "EQWin selections are still loading. Please wait a moment and try again."
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
                "Select at least one station, or switch to location groups."
              )
            } else if (length(setdiff(
              input$locations_EQ,
              moduleData$EQ_locs$StnCode
            ))) {
              issues <- c(issues, "One or more selected stations are invalid.")
            }
          } else if (identical(input$locs_groups, "Location Groups")) {
            if (
              is.null(input$location_groups) ||
                !length(input$location_groups) ||
                anyNA(input$location_groups) ||
                !nzchar(input$location_groups[[1]])
            ) {
              issues <- c(issues, "Select a location group.")
            } else if (
              !input$location_groups[[1]] %in% moduleData$EQ_loc_grps$groupname
            ) {
              issues <- c(issues, "The selected location group is invalid.")
            }
          } else {
            issues <- c(
              issues,
              "Choose whether to filter by stations or by location groups."
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
                "Select at least one parameter, or switch to parameter groups."
              )
            } else if (length(setdiff(
              input$parameters_EQ,
              moduleData$EQ_params$ParamCode
            ))) {
              issues <- c(issues, "One or more selected parameters are invalid.")
            }
          } else if (identical(input$params_groups, "Parameter Groups")) {
            if (
              is.null(input$parameter_groups) ||
                !length(input$parameter_groups) ||
                anyNA(input$parameter_groups) ||
                !nzchar(input$parameter_groups[[1]])
            ) {
              issues <- c(issues, "Select a parameter group.")
            } else if (
              !input$parameter_groups[[1]] %in% moduleData$EQ_param_grps$groupname
            ) {
              issues <- c(issues, "The selected parameter group is invalid.")
            }
          } else {
            issues <- c(
              issues,
              "Choose whether to filter by parameters or by parameter groups."
            )
          }
          if (
            length(input$stds) &&
              length(setdiff(input$stds, moduleData$EQ_stds$StdCode))
          ) {
            issues <- c(issues, "One or more selected standards are invalid.")
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

    pick_generated_file <- function(files, pattern = NULL) {
      files <- files[file.exists(files)]
      if (!length(files)) {
        stop("No files were generated for the report.")
      }
      if (!is.null(pattern)) {
        matched <- files[grepl(pattern, basename(files), ignore.case = TRUE)]
        if (length(matched) == 1L) {
          return(matched[[1]])
        }
        if (length(matched) > 1L) {
          stop("Multiple report files were generated where only one was expected.")
        }
      }
      if (length(files) != 1L) {
        stop("Expected a single generated report file.")
      }
      files[[1]]
    }

    report_task <- ExtendedTask$new(function(req, db_config) {
      promises::future_promise({
        work_dir <- tempfile("WQReport_")
        dir.create(work_dir, recursive = TRUE)
        tryCatch(
          {
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
                guideline_ids = req$guideline_ids,
                date_approx = req$date_approx,
                sd_multiplier = req$sd_multiplier,
                sd_start = req$sd_start,
                sd_end = req$sd_end,
                sd_day_of_year = req$sd_day_of_year,
                output_path = output_path,
                lang = req$lang,
                con = con
              )
              report_path <- result$xlsx_path
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
                pattern = "\\.xlsx$"
              )
            }

            list(
              path = report_path,
              filename = paste0(
                "water quality report for ",
                req$date,
                " Issued ",
                Sys.Date(),
                ".xlsx"
              )
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
        date = as.Date(input$date),
        date_approx = as.integer(input$date_approx),
        sd_multiplier = sd_multiplier,
        sd_start = normalize_optional_date(input$SD_start),
        sd_end = normalize_optional_date(input$SD_end),
        sd_day_of_year = sd_day_of_year,
        lang = if (identical(language$language, "Français")) "fr" else "en"
      )

      if (identical(source, "AC")) {
        req$location_ids <- input$locations_AC
        req$parameter_ids <- input$parameters_AC
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
        req$paramGrp <- if (identical(input$params_groups, "Parameter Groups")) {
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
          paste("Error generating water quality report:", result),
          type = "error",
          duration = NULL,
          closeButton = TRUE
        )
        return()
      }
      if (is.null(result$path) || !file.exists(result$path)) {
        showNotification(
          "Report was generated, but the output file could not be found for download.",
          type = "error",
          duration = NULL,
          closeButton = TRUE
        )
        return()
      }
      cleanup_download_bundle(download_bundle())
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
          stop("Generated report file could not be found for download.")
        }
        copied <- file.copy(bundle$path, file, overwrite = TRUE)
        if (!isTRUE(copied)) {
          stop("Unable to copy the generated report to the download location.")
        }
        cleanup_download_bundle(bundle)
        download_bundle(NULL)
      },
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    )
    outputOptions(output, "download", suspendWhenHidden = FALSE)
  })
}
