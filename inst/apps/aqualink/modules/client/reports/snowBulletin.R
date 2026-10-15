snowBulletinUIMod <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("banner")),
    # Custom CSS below is for consistency with the sidebarPanel look elsewhere in the app.
    tags$head(tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "css/card_background.css"
    )),
    card(
      card_body(
        class = "custom-card",
        uiOutput(ns("menu")) # UI is rendered in the server function below so that it can use database information as well as language selections.
      )
    )
  )
}

snowBulletinMod <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # Used to create UI elements in the server code

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "snowBulletin"
      )
    })

    # Create reactiveValues to store the user's selections. Used if switching between languages.
    selections <- reactiveValues(
      stats = TRUE,
      year = lubridate::year(Sys.Date()),
      month = if (lubridate::month(Sys.Date()) %in% c(3:5)) {
        lubridate::month(Sys.Date())
      } else {
        3
      },
      basins = "all",
      language = "English",
      precip_period = "last 40 years",
      cddf_period = "last 40 years",
      scale = 1
    )

    # This observe block is used to render the UI elements for the menu. It is reactive to the language selection.
    output$menu <- renderUI({
      req(data, language$language, language$abbrev)
      tagList(
        textOutput(ns("info")), # Information about the app
        tags$hr(), # dividing blank space
        # Toggle for stats/snow bulletin
        radioButtons(
          ns("stats"),
          label = NULL,
          choices = stats::setNames(
            c(TRUE, FALSE),
            c(
              tr("gen_snowBul_toggle_stats", language$language),
              tr("gen_snowBul_toggle_bulletin", language$language)
            )
          ),
          selected = selections$stats,
          inline = TRUE,
          width = "100%"
        ),
        # selector for year
        selectizeInput(
          ns("year"),
          label = tr("gen_snowBul_year", language$language),
          choices = c(1980:lubridate::year(Sys.Date())),
          selected = selections$year,
          multiple = FALSE,
          width = "100%"
        ),
        # Selector for month
        selectizeInput(
          ns("month"),
          label = tr("month", language$language),
          choices = stats::setNames(
            c(3:5),
            c(
              tr("mar", language$language),
              tr("apr", language$language),
              tr("may", language$language)
            )
          ),
          selected = selections$month,
          multiple = FALSE,
          width = "100%"
        ),
        # Selector for basins
        selectizeInput(
          ns("basins"),
          label = tr("gen_snowBul_basins", language$language),
          choices = stats::setNames(
            c(
              "all",
              "Upper Yukon",
              "Teslin",
              "Central Yukon",
              "Pelly",
              "Stewart",
              "White",
              "Lower Yukon",
              "Porcupine",
              "Peel",
              "Liard",
              "Alsek"
            ),
            c(
              tr("all_m", language$language),
              "Upper Yukon",
              "Teslin",
              "Central Yukon",
              "Pelly",
              "Stewart",
              "White",
              "Lower Yukon",
              "Porcupine",
              "Peel",
              "Liard",
              "Alsek"
            )
          ),
          selected = selections$basins,
          multiple = TRUE,
          width = "100%"
        ),
        # Selector for language
        selectizeInput(
          ns("language"),
          label = tr("gen_snowBul_lang", language$language),
          choices = stats::setNames(
            c("English", "French"),
            c(
              tr("english", language$language),
              tr("francais", language$language)
            )
          ),
          selected = selections$language,
          multiple = FALSE,
          width = "100%"
        ),
        # Selector for precipitation period
        selectizeInput(
          ns("precip_period"),
          label = tr("gen_snowBul_precip_period", language$language),
          choices = stats::setNames(
            c("last 40 years", "all years", "1981-2010", "1991-2020"),
            c(
              tr("gen_snowBul_period1", language$language),
              tr("all_yrs_record", language$language),
              tr("gen_snowBul_period3", language$language),
              tr("gen_snowBul_period4", language$language)
            )
          ),
          selected = selections$precip_period,
          multiple = FALSE,
          width = "100%"
        ),
        # Selector for CDDF period
        selectizeInput(
          ns("cddf_period"),
          label = tr("gen_snowBul_cddf_period", language$language),
          choices = stats::setNames(
            c("last 40 years", "all years", "1981-2010", "1991-2020"),
            c(
              tr("gen_snowBul_period1", language$language),
              tr("all_yrs_record", language$language),
              tr("gen_snowBul_period3", language$language),
              tr("gen_snowBul_period4", language$language)
            )
          ),
          selected = selections$cddf_period,
          multiple = FALSE,
          width = "100%"
        ),
        # Plot scale
        numericInput(
          ns("scale"),
          label = tr("gen_snowBul_scale", language$language),
          value = selections$scale,
          min = 0.5,
          max = 3,
          step = 0.1,
          width = "100%"
        ),

        # Make it happen
        bslib::input_task_button(
          ns("go"),
          label = tr("create_report", language$language),
          label_busy = tr("generating_working", language$language)
        ),
        downloadButton(
          ns("download"),
          tr("download_button", language$language),
          style = "visibility: hidden;"
        ) # Hidden; triggered automatically but left hidden if 'go' is successful
      ) # End tagList
    }) %>% # End renderUI
      bindEvent(language$language) # Re-render the UI if the language or data changes

    output$info <- renderText({
      tr("gen_snowBul_info", language$language)
    }) %>%
      bindEvent(language$language) # Re-render the text if the language changes

    # Show/hide elements depending on input$stats

    # Observe inputs and store in object 'selections'
    observeEvent(
      input$stats,
      {
        # Also Show/hide elements depending on input$stats
        selections$stats <- as.logical(input$stats)
        if (!selections$stats) {
          shinyjs::show("language")
          shinyjs::show("precip_period")
          shinyjs::show("cddf_period")
          shinyjs::show("scale")
        } else {
          shinyjs::hide("language")
          shinyjs::hide("precip_period")
          shinyjs::hide("cddf_period")
          shinyjs::hide("scale")
        }
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$year,
      {
        selections$year <- as.numeric(input$year)
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$month,
      {
        selections$month <- as.numeric(input$month)
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$basins,
      {
        selections$basins <- input$basins
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$language,
      {
        selections$language <- input$language
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$precip_period,
      {
        selections$precip_period <- input$precip_period
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$cddf_period,
      {
        selections$cddf_period <- input$cddf_period
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$scale,
      {
        selections$scale <- input$scale
      },
      ignoreInit = TRUE
    )

    # Adjust filter selections based on if 'all' is selected (remove selections other than 'all') ################
    observeFilterInput <- function(inputId) {
      observeEvent(
        input[[inputId]],
        {
          values <- input[[inputId]]
          if (is.null(values) || length(values) == 0) {
            updateSelectizeInput(session, inputId, selected = "all")
            return()
          }
          values <- as.character(values)
          if (length(values) > 1 && "all" %in% values) {
            selected <- if (identical(values[[length(values)]], "all")) {
              "all"
            } else {
              setdiff(values, "all")
            }
            updateSelectizeInput(session, inputId, selected = selected)
          }
        },
        ignoreNULL = FALSE
      )
    }
    observeFilterInput("basins")

    download_bundle <- reactiveVal(NULL)

    show_validation_modal <- function(messages) {
      messages <- unique(messages[!is.na(messages) & nzchar(messages)])
      if (!length(messages)) {
        return(invisible(FALSE))
      }

      showModal(modalDialog(
        title = tr("snow_bulletin_validation_title", language$language),
        tags$p(tr("report_validation_intro", language$language)),
        tags$ul(lapply(messages, function(msg) tags$li(msg))),
        easyClose = TRUE,
        footer = modalButton(tr("close", language$language))
      ))

      invisible(TRUE)
    }

    validate_report_request <- function() {
      issues <- character()

      if (
        is.null(selections$stats) ||
          length(selections$stats) != 1 ||
          is.na(selections$stats)
      ) {
        issues <- c(
          issues,
          tr("snow_bulletin_output_required", language$language)
        )
      }

      if (
        is.null(selections$year) ||
          length(selections$year) != 1 ||
          is.na(selections$year)
      ) {
        issues <- c(issues, tr("snow_bulletin_year_invalid", language$language))
      }

      if (
        is.null(selections$month) ||
          length(selections$month) != 1 ||
          is.na(selections$month) ||
          !selections$month %in% 3:5
      ) {
        issues <- c(issues, tr("snow_bulletin_month_invalid", language$language))
      }

      if (
        is.null(selections$basins) ||
          length(selections$basins) == 0 ||
          all(is.na(selections$basins)) ||
          !any(nzchar(selections$basins))
      ) {
        issues <- c(
          issues,
          tr("snow_bulletin_basin_required", language$language)
        )
      }

      if (!isTRUE(selections$stats)) {
        if (
          is.null(selections$language) ||
            length(selections$language) == 0 ||
            anyNA(selections$language) ||
            !nzchar(selections$language[[1]])
        ) {
          issues <- c(issues, tr("snow_bulletin_language_required", language$language))
        }

        if (
          is.null(selections$precip_period) ||
            length(selections$precip_period) == 0 ||
            anyNA(selections$precip_period) ||
            !nzchar(selections$precip_period[[1]])
        ) {
          issues <- c(issues, tr("snow_bulletin_precip_period_required", language$language))
        }

        if (
          is.null(selections$cddf_period) ||
            length(selections$cddf_period) == 0 ||
            anyNA(selections$cddf_period) ||
            !nzchar(selections$cddf_period[[1]])
        ) {
          issues <- c(issues, tr("snow_bulletin_cddf_period_required", language$language))
        }

        if (
          is.null(selections$scale) ||
            length(selections$scale) != 1 ||
            is.na(selections$scale) ||
            selections$scale < 0.5 ||
            selections$scale > 3
        ) {
          issues <- c(
            issues,
            tr("snow_bulletin_scale_range", language$language)
          )
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
        stop(tr("report_error_no_files", lang), call. = FALSE)
      }

      if (!is.null(pattern)) {
        matched <- files[grepl(pattern, basename(files), ignore.case = TRUE)]
        if (length(matched) == 1) {
          return(matched[[1]])
        }
        if (length(matched) > 1) {
          stop(tr("report_error_multiple_files", lang), call. = FALSE)
        }
      }

      if (length(files) != 1) {
        stop(tr("report_error_single_file_expected", lang), call. = FALSE)
      }

      files[[1]]
    }

    report_task <- ExtendedTask$new(function(req, config) {
      promises::future_promise({
        tryCatch(
          {
            con <- AquaConnect(
              name = config$dbName,
              host = config$dbHost,
              port = config$dbPort,
              username = config$dbUser,
              password = config$dbPass,
              silent = TRUE
            )
            on.exit(DBI::dbDisconnect(con), add = TRUE)

            work_dir <- tempfile("snowBulletinOutput_")
            dir.create(work_dir, recursive = TRUE)

            suppressWarnings({
              if (isTRUE(req$stats)) {
                snowBulletinStats(
                  year = req$year,
                  month = req$month,
                  basins = req$basins,
                  save_path = work_dir,
                  excel_output = TRUE,
                  con = con,
                  source = "aquacache",
                  synchronize = FALSE
                )

                files <- list.files(work_dir, full.names = FALSE)
                if (!length(files)) {
                  stop(tr("report_error_no_files", req$ui_language), call. = FALSE)
                }

                zip_path <- file.path(work_dir, "report.zip")
                zip::zip(
                  zipfile = zip_path,
                  files = files,
                  mode = "cherry-pick",
                  include_directories = FALSE,
                  root = work_dir
                )

                return(list(
                  path = zip_path,
                  filename = sprintf(
                    tr("snow_bulletin_stats_filename", req$ui_language),
                    Sys.Date()
                  )
                ))
              }

              snowBulletin(
                year = req$year,
                month = req$month,
                basins = req$basins,
                scale = req$scale,
                save_path = work_dir,
                con = con,
                precip_period = req$precip_period,
                cddf_period = req$cddf_period,
                language = req$report_language
              )

              report_path <- pick_generated_file(
                list.files(work_dir, full.names = TRUE),
                pattern = "\\.docx$",
                lang = req$ui_language
              )

              list(
                path = report_path,
                filename = sprintf(
                  tr("snow_bulletin_filename", req$ui_language),
                  Sys.Date()
                )
              )
            })
          },
          error = function(e) {
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

      report_task$invoke(
        req = list(
          stats = isTRUE(selections$stats),
          year = selections$year,
          month = selections$month,
          basins = if (identical(selections$basins, "all")) {
            NULL
          } else {
            selections$basins
          },
          scale = selections$scale,
          precip_period = selections$precip_period,
          cddf_period = selections$cddf_period,
          report_language = tolower(selections$language),
          ui_language = language$language
        ),
        config = session$userData$config
      )
    })

    observeEvent(report_task$result(), {
      result <- report_task$result()

      if (inherits(result, "character")) {
        showNotification(
          paste(tr("snow_bulletin_error_prefix", language$language), result),
          type = "error",
          duration = NULL,
          closeButton = TRUE
        )
        return()
      }

      if (is.null(result$path) || !file.exists(result$path)) {
        showNotification(
          tr("report_download_file_missing", language$language),
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

    # Listen for the 'go' and make the report when called
    output$download <- downloadHandler(
      filename = function() {
        req(download_bundle())
        download_bundle()$filename
      },
      content = function(file) {
        bundle <- download_bundle()
        req(bundle)

        if (!file.exists(bundle$path)) {
          stop(tr("report_download_source_missing", language$language))
        }

        copied <- file.copy(bundle$path, file, overwrite = TRUE)
        if (!isTRUE(copied)) {
          stop(tr("report_download_copy_failed", language$language))
        }

        cleanup_download_bundle(bundle)
        download_bundle(NULL)
      } # End content
    ) # End downloadHandler
    outputOptions(output, "download", suspendWhenHidden = FALSE)
  }) # End moduleServer
} # End snowInfoServer
