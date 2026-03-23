snowInfoUIMod <- function(id) {
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

snowInfoMod <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # Used to create UI elements in the server code

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "snowInfo"
      )
    })

    moduleData <- reactiveValues(
      locs = dbGetQueryDT(
        session$userData$AquaCache,
        "SELECT DISTINCT l.location_id, l.location_code AS location, l.name, l.name_fr
         FROM locations AS l
         JOIN locations_networks AS ln ON l.location_id = ln.location_id
         JOIN networks AS n ON ln.network_id = n.network_id
         JOIN samples AS s ON l.location_id = s.location_id
         WHERE n.name = 'Snow Survey Network'
         ORDER BY l.name;"
      )
    )

    # Create reactiveValues to store the user's selections. Used if switching between languages.
    selections <- reactiveValues(
      loc = "all",
      inactive = FALSE,
      complete = TRUE,
      stats = TRUE,
      plots = TRUE,
      plot_type = "combined"
    )

    # This observe block is used to render the UI elements for the menu. It is reactive to the language selection.
    output$menu <- renderUI({
      req(moduleData, language$language, language$abbrev)
      tagList(
        textOutput(ns("info")), # Information about the app
        # dividing blank space
        tags$hr(),
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
        # Inactive locations?
        checkboxInput(
          ns("inactive"),
          label = tr("gen_snowInfo_inactive", language$language),
          value = selections$inactive,
          width = "100%"
        ),
        # Complete years only?
        checkboxInput(
          ns("complete"),
          label = tr("gen_snowInfo_complete", language$language),
          value = selections$complete,
          width = "100%"
        ),
        # Generate stats?
        # Left blank for now (default TRUE)
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
          label = tr("gen_snowInfo_ptype", language$language),
          choices = stats::setNames(
            c("combined", "separate"),
            c(
              tr("gen_snowInfo_ptype_combine", language$language),
              tr("separate", language$language)
            )
          ),
          selected = selections$plot_type,
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
      bindEvent(language$language, moduleData$locs) # Re-render the UI if the language or moduleData changes

    output$info <- renderText({
      tr("gen_snowInfo_info", language$language)
    }) %>%
      bindEvent(language$language) # Re-render the text if the language changes

    # Observe inputs and store in object 'selections'
    observeEvent(
      input$loc,
      {
        selections$loc <- input$loc
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$inactive,
      {
        selections$inactive <- input$inactive
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$complete,
      {
        selections$complete <- input$complete
      },
      ignoreInit = TRUE
    )
    # observeEvent(input$stats, {
    #   selections$stats <- input$stats
    # }, ignoreInit = TRUE)
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
        selections$plot_type <- input$ptype
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
      messages <- unique(messages[!is.na(messages) & nzchar(messages)])
      if (!length(messages)) {
        return(invisible(FALSE))
      }

      showModal(modalDialog(
        title = "Cannot Generate Snow Information Report",
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
        is.null(selections$loc) ||
          length(selections$loc) == 0 ||
          all(is.na(selections$loc)) ||
          !any(nzchar(selections$loc))
      ) {
        issues <- c(
          issues,
          "Select at least one snow survey location, or choose 'All locations'."
        )
      }

      if (
        isTRUE(selections$plots) &&
          (
            is.null(selections$plot_type) ||
              length(selections$plot_type) == 0 ||
              anyNA(selections$plot_type) ||
              !nzchar(selections$plot_type[[1]])
          )
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

            work_dir <- tempfile("snowInfoOutput_")
            dir.create(work_dir, recursive = TRUE)

            suppressWarnings(snowInfo(
              con = con,
              locations = req$loc,
              inactive = req$inactive,
              stats = req$stats,
              complete_yrs = req$complete,
              plots = req$plots,
              plot_type = req$plot_type,
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
              filename = paste0("Snow info report issued ", Sys.Date(), ".zip")
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
          loc = selections$loc,
          inactive = selections$inactive,
          stats = selections$stats,
          complete = selections$complete,
          plots = selections$plots,
          plot_type = selections$plot_type
        ),
        config = session$userData$config
      )
    })

    observeEvent(report_task$result(), {
      result <- report_task$result()

      if (inherits(result, "character")) {
        showNotification(
          paste("Error generating snow information report:", result),
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
          stop("Unable to copy the generated report archive to the download location.")
        }

        cleanup_download_bundle(bundle)
        download_bundle(NULL)
      }, # End content
      contentType = "application/zip"
    ) # End downloadHandler
    outputOptions(output, "download", suspendWhenHidden = FALSE)
  }) # End moduleServer
} # End snowInfoServer
