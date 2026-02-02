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
        "SELECT DISTINCT ts.location_id, ts.location_code AS location, l.name, l.name_fr, ts.parameter_id, p.param_name, p.param_name_fr FROM timeseries AS ts JOIN parameters AS p ON ts.parameter_id = p.parameter_id JOIN locations AS l on ts.location_id = l.location_id WHERE ts.parameter_id IN (1150, 1165)"
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
        textOutput(ns("info")),
        tags$hr(), # dividing blank space
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
        actionButton(ns("go"), label = tr("create_report", language$language)),
        downloadButton(
          ns("download"),
          "download",
          style = "visibility: hidden;"
        ) # Hidden; triggered automatically but left hidden if 'go' is successful
      ) # End tagList
    }) %>% # End renderUI
      bindEvent(language, moduleData) # Re-render the UI if the language or moduleData changes

    output$info <- renderText({
      tr("gen_waterInfo_info", language$language)
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

    outputFile <- reactiveVal(NULL) # Will hold path to the file if successful at creating

    observeEvent(input$go, {
      tryCatch(
        {
          withProgress(
            message = tr("dl_prep", language$language),
            value = 0,
            {
              incProgress(0.2)

              # 1. Create a temporary folder
              dir <- paste0(tempdir(), "/waterInfoOutput")
              dir.create(dir, showWarnings = FALSE)
              # Delete all files in the folder (if any)
              files <- list.files(dir, full.names = TRUE)
              if (length(files) > 0) {
                unlink(files, recursive = TRUE, force = TRUE)
              }

              # 3. Call waterInfo() so it writes all files to 'dir'
              suppressWarnings(waterInfo(
                con = session$userData$AquaCache,
                locations = selections$loc,
                level_flow = selections$param,
                end_date = selections$end,
                months_min = as.numeric(selections$min_m),
                months_max = as.numeric(selections$max_m),
                allowed_missing = selections$prct,
                plots = selections$plots,
                plot_type = selections$ptype,
                save_path = dir,
                quiet = TRUE
              ))

              incProgress(0.7)

              # 4. Zip up everything in 'dir' and write the zip to `file`
              files <- list.files(dir, full.names = FALSE)
              if (!length(files)) {
                stop("No files were generated for the report.")
              }

              zip::zip(
                zipfile = file.path(dir, "report.zip"),
                files = files,
                mode = "cherry-pick",
                include_directories = FALSE,
                root = dir
              )
              outputFile(file.path(dir, "report.zip"))

              # Delete everything in the 'dir' except for the .zip file
              files <- list.files(dir, full.names = TRUE)
              files <- files[!grepl("report.zip", files)]
              if (length(files) > 0) {
                unlink(files, recursive = TRUE, force = TRUE)
              }

              # 5. Now programmatically click the hidden download button
              shinyjs::click("download")

              incProgress(1)
            } # End withProgress content
          ) # End withProgress
        },
        error = function(e) {
          showNotification(
            paste("Error generating water quantity/info report:", e$message),
            type = "error",
            duration = NULL,
            closeButton = TRUE
          )
        }
      )
    })

    # Listen for the 'go' and make the report when called
    output$download <- downloadHandler(
      filename = function() {
        paste0("water info report issued ", Sys.Date(), ".zip")
      },
      content = function(file) {
        file.copy(outputFile(), file)
        # Now delete the zip file and the directory it's in
        unlink(dirname(outputFile()), recursive = TRUE, force = TRUE)
      }, # End content
      contentType = "application/zip"
    ) # End downloadHandler
  }) # End moduleServer
} # End waterInfoServer
