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
          choices = c(3:5),
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
          "download",
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
      observeEvent(input[[inputId]], {
        # Check if 'all' is selected and adjust accordingly
        if (length(input[[inputId]]) > 1) {
          # If 'all' was selected last, remove all other selections
          if (input[[inputId]][length(input[[inputId]])] == "all") {
            updateSelectizeInput(session, inputId, selected = "all")
          } else if ("all" %in% input[[inputId]]) {
            # If 'all' is already selected and another option is selected, remove 'all'
            updateSelectizeInput(
              session,
              inputId,
              selected = input[[inputId]][length(input[[inputId]])]
            )
          }
        }
      })
    }
    observeFilterInput("basins")

    download_bundle <- reactiveVal(NULL)

    show_validation_modal <- function(messages) {
      messages <- unique(messages[!is.na(messages) & nzchar(messages)])
      if (!length(messages)) {
        return(invisible(FALSE))
      }

      showModal(modalDialog(
        title = "Cannot Generate Snow Bulletin Report",
        tags$p("Please correct the following before starting the report:"),
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
          "Choose whether to create bulletin statistics or the bulletin report."
        )
      }

      if (
        is.null(selections$year) ||
          length(selections$year) != 1 ||
          is.na(selections$year)
      ) {
        issues <- c(issues, "Select a valid year.")
      }

      if (
        is.null(selections$month) ||
          length(selections$month) != 1 ||
          is.na(selections$month) ||
          !selections$month %in% 3:5
      ) {
        issues <- c(issues, "Select a valid spring month.")
      }

      if (
        is.null(selections$basins) ||
          length(selections$basins) == 0 ||
          all(is.na(selections$basins)) ||
          !any(nzchar(selections$basins))
      ) {
        issues <- c(
          issues,
          "Select at least one basin, or choose 'All'."
        )
      }

      if (!isTRUE(selections$stats)) {
        if (
          is.null(selections$language) ||
            length(selections$language) == 0 ||
            anyNA(selections$language) ||
            !nzchar(selections$language[[1]])
        ) {
          issues <- c(issues, "Select a report language.")
        }

        if (
          is.null(selections$precip_period) ||
            length(selections$precip_period) == 0 ||
            anyNA(selections$precip_period) ||
            !nzchar(selections$precip_period[[1]])
        ) {
          issues <- c(issues, "Select a precipitation comparison period.")
        }

        if (
          is.null(selections$cddf_period) ||
            length(selections$cddf_period) == 0 ||
            anyNA(selections$cddf_period) ||
            !nzchar(selections$cddf_period[[1]])
        ) {
          issues <- c(issues, "Select a CDDF comparison period.")
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
            "Plot scale must be a number between 0.5 and 3."
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

    pick_generated_file <- function(files, pattern = NULL) {
      files <- files[file.exists(files)]
      if (!length(files)) {
        stop("No files were generated for the report.")
      }

      if (!is.null(pattern)) {
        matched <- files[grepl(pattern, basename(files), ignore.case = TRUE)]
        if (length(matched) == 1) {
          return(matched[[1]])
        }
        if (length(matched) > 1) {
          stop("Multiple report files were generated where only one was expected.")
        }
      }

      if (length(files) != 1) {
        stop("Expected a single generated report file.")
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
                  stop("No files were generated for the report.")
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
                  filename = paste0("Snow bulletin stats issued ", Sys.Date(), ".zip")
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
                pattern = "\\.docx$"
              )

              list(
                path = report_path,
                filename = paste0("Snow bulletin issued ", Sys.Date(), ".docx")
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
          report_language = tolower(selections$language)
        ),
        config = session$userData$config
      )
    })

    observeEvent(report_task$result(), {
      result <- report_task$result()

      if (inherits(result, "character")) {
        showNotification(
          paste("Error generating snow bulletin report:", result),
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
          stop("Generated report file could not be found for download.")
        }

        copied <- file.copy(bundle$path, file, overwrite = TRUE)
        if (!isTRUE(copied)) {
          stop("Unable to copy the generated report to the download location.")
        }

        cleanup_download_bundle(bundle)
        download_bundle(NULL)
      } # End content
    ) # End downloadHandler
    outputOptions(output, "download", suspendWhenHidden = FALSE)
  }) # End moduleServer
} # End snowInfoServer
