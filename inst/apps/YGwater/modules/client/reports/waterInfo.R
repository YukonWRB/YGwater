waterInfoUIMod <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("banner")),
    # Custom CSS below is for consistency with the look elsewhere in the app.
    tags$head(tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "css/card_background.css"
    )),
    uiOutput(ns("info")),
    card(
      card_body(
        class = "custom-card",
        uiOutput(ns("menu"))
      )
    )
  )
}

waterInfoMod <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # Used to create UI elements in the server code

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "waterInfo"
      )
    })

    moduleData <- reactiveValues(
      locs = dbGetQueryDT(
        session$userData$AquaCache,
        "SELECT DISTINCT ts.location_id, l.location_code AS location, l.name, l.name_fr, ts.parameter_id, p.param_name, p.param_name_fr FROM timeseries AS ts JOIN parameters AS p ON ts.parameter_id = p.parameter_id JOIN locations AS l on ts.location_id = l.location_id WHERE ts.parameter_id IN (1150, 1165)"
      )
    )

    # Create reactiveValues to store the user's selections. Used if switching between languages.
    selections <- reactiveValues(
      param = "one",
      loc = NULL,
      end = Sys.Date(),
      min_m = c(1:4),
      max_m = c(5:9),
      prct = 10,
      plots = TRUE,
      ptype = "combined"
    )

    # This observe block is used to render the UI elements for the menu. It is reactive to the language selection.
    output$menu <- renderUI({
      req(moduleData, language$language, language$abbrev)
      tagList(
        # selector for one parameter (flow if exists, else level) or both
        selectizeInput(
          ns("param"),
          label = tr("gen_waterInfo_param_select", language$language),
          choices = stats::setNames(
            c("one", "both"),
            c(
              tr("gen_waterInfo_param_select_one", language$language),
              tr("gen_waterInfo_param_select_both", language$language)
            )
          ),
          selected = selections$param,
          multiple = FALSE,
          width = "100%"
        ),

        # selector for location
        selectizeInput(
          ns("loc"),
          label = tr("gen_loc_select", language$language),
          choices = stats::setNames(
            c("all", moduleData$locs$location),
            c(
              tr("all_locs", language$language),
              moduleData$locs[[tr("generic_name_col", language$language)]]
            )
          ),
          selected = selections$loc,
          multiple = TRUE,
          width = "100%"
        ),

        # end date to use in calculations
        dateInput(
          ns("end"),
          label = tr("gen_waterInfo_end_date", language$language),
          value = selections$end,
          width = "100%"
        ),

        # month ranges
        selectizeInput(
          ns("min_m"),
          label = tr("gen_waterInfo_min_months", language$language),
          choices = stats::setNames(
            c(1:12),
            c(
              tr("jan", language$language),
              tr("feb", language$language),
              tr("mar", language$language),
              tr("apr", language$language),
              tr("may", language$language),
              tr("jun", language$language),
              tr("jul", language$language),
              tr("aug", language$language),
              tr("sep", language$language),
              tr("oct", language$language),
              tr("nov", language$language),
              tr("dec", language$language)
            )
          ),
          selected = selections$min_m,
          multiple = TRUE,
          width = "100%"
        ),

        selectizeInput(
          ns("max_m"),
          label = tr("gen_waterInfo_max_months", language$language),
          choices = stats::setNames(
            c(1:12),
            c(
              tr("jan", language$language),
              tr("feb", language$language),
              tr("mar", language$language),
              tr("apr", language$language),
              tr("may", language$language),
              tr("jun", language$language),
              tr("jul", language$language),
              tr("aug", language$language),
              tr("sep", language$language),
              tr("oct", language$language),
              tr("nov", language$language),
              tr("dec", language$language)
            )
          ),
          selected = selections$max_m,
          multiple = TRUE,
          width = "100%"
        ),

        # percent allowed missing
        numericInput(
          ns("prct"),
          label = tr("gen_waterInfo_prct_miss", language$language),
          min = 1,
          max = 100,
          value = selections$prct,
          width = "100%"
        ),

        # Generate plots?
        checkboxInput(
          ns("plots"),
          label = tr("gen_generate_plots", language$language),
          value = selections$plots,
          width = "100%"
        ),

        # Type of plot
        selectizeInput(
          ns("ptype"),
          label = tr("gen_waterInfo_ptype", language$language),
          choices = stats::setNames(
            c("combined", "separate"),
            c(
              tr("gen_waterInfo_ptype_combine", language$language),
              tr("separate", language$language)
            )
          ),
          selected = selections$ptype,
          multiple = FALSE,
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
      bindEvent(language, moduleData) # Re-render the UI if the language or moduleData changes

    output$info <- renderUI({
      text <- HTML(tr("gen_waterInfo_info", language$language))
      div(
        style = paste(
          "background-color: #F7FAFC;",
          "border-left: 4px solid #0097A9;",
          "border-radius: 6px;",
          "padding: 12px 16px;",
          "margin-bottom: 12px;"
        ),
        tags$p(style = "margin-bottom: 0;", text)
      )
    }) %>%
      bindEvent(language$language) # Re-render the text if the language changes

    # Observe inputs and store in object 'selections'
    observeEvent(
      input$param,
      {
        selections$param <- input$param
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$loc,
      {
        selections$loc <- input$loc
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$end,
      {
        selections$end <- input$end
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$min_m,
      {
        selections$min_m <- input$min_m
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$max_m,
      {
        selections$max_m <- input$max_m
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$prct,
      {
        selections$prct <- input$prct
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$plots,
      {
        selections$plots <- input$plots
        if (input$plots) {
          shinyjs::show("ptype")
        } else {
          shinyjs::hide("ptype")
        }
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$ptype,
      {
        selections$ptype <- input$ptype
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
    observeFilterInput("loc")

    download_bundle <- reactiveVal(NULL)

    show_validation_modal <- function(messages) {
      messages <- unique(messages[
        !is.na(messages) & gen_waterInfo_infonzchar(messages)
      ])
      if (!length(messages)) {
        return(invisible(FALSE))
      }

      showModal(modalDialog(
        title = "Cannot Generate Water Quantity/Info Report",
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
        is.null(selections$param) ||
          length(selections$param) == 0 ||
          anyNA(selections$param) ||
          !nzchar(selections$param[[1]])
      ) {
        issues <- c(
          issues,
          "Select whether to report one parameter or both parameters."
        )
      }

      if (
        is.null(selections$loc) ||
          length(selections$loc) == 0 ||
          all(is.na(selections$loc)) ||
          !any(nzchar(selections$loc))
      ) {
        issues <- c(
          issues,
          "Select at least one location, or choose 'All locations'."
        )
      }

      end_date <- as.Date(selections$end)
      if (length(end_date) != 1 || is.na(end_date)) {
        issues <- c(issues, "Provide a valid end date.")
      }

      if (is.null(selections$min_m) || length(selections$min_m) == 0) {
        issues <- c(
          issues,
          "Select at least one month for the low-flow period."
        )
      }

      if (is.null(selections$max_m) || length(selections$max_m) == 0) {
        issues <- c(
          issues,
          "Select at least one month for the high-flow period."
        )
      }

      if (
        is.null(selections$prct) ||
          length(selections$prct) != 1 ||
          is.na(selections$prct) ||
          selections$prct < 1 ||
          selections$prct > 100
      ) {
        issues <- c(
          issues,
          "Allowed missing data must be a number between 1 and 100."
        )
      }

      if (
        isTRUE(selections$plots) &&
          (is.null(selections$ptype) ||
            length(selections$ptype) == 0 ||
            anyNA(selections$ptype) ||
            !nzchar(selections$ptype[[1]]))
      ) {
        issues <- c(
          issues,
          "Select a plot type, or turn off plot generation."
        )
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

            work_dir <- tempfile("waterInfoOutput_")
            dir.create(work_dir, recursive = TRUE)

            suppressWarnings(waterInfo(
              con = con,
              locations = req$loc,
              level_flow = req$param,
              end_date = req$end,
              months_min = as.numeric(req$min_m),
              months_max = as.numeric(req$max_m),
              allowed_missing = req$prct,
              plots = req$plots,
              plot_type = req$ptype,
              save_path = work_dir,
              quiet = TRUE
            ))

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

            list(
              path = zip_path,
              filename = paste0("water info report issued ", Sys.Date(), ".zip")
            )
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
          param = selections$param,
          loc = selections$loc,
          end = selections$end,
          min_m = selections$min_m,
          max_m = selections$max_m,
          prct = selections$prct,
          plots = selections$plots,
          ptype = selections$ptype
        ),
        config = session$userData$config
      )
    })

    observeEvent(report_task$result(), {
      result <- report_task$result()

      if (inherits(result, "character")) {
        showNotification(
          paste("Error generating water quantity/info report:", result),
          type = "error",
          duration = NULL,
          closeButton = TRUE
        )
        return()
      }

      if (is.null(result$path) || !file.exists(result$path)) {
        showNotification(
          "Report was generated, but the zip archive could not be found for download.",
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
          stop("Generated report archive could not be found for download.")
        }

        copied <- file.copy(bundle$path, file, overwrite = TRUE)
        if (!isTRUE(copied)) {
          stop(
            "Unable to copy the generated report archive to the download location."
          )
        }

        cleanup_download_bundle(bundle)
        download_bundle(NULL)
      }, # End content
      contentType = "application/zip"
    ) # End downloadHandler
    outputOptions(output, "download", suspendWhenHidden = FALSE)
  }) # End moduleServer
} # End waterInfoServer
