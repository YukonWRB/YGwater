WQReportUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Custom CSS below is for consistency with the look elsewhere in the app.
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/card_background.css")),
    card(
      card_body(
        class = "custom-card",
        # Toggle for data source
        # ! Data source 'AC' is not implemented in the report generation function yet; the code is here but a modal dialogue will appear if AC is selected and the user will get shunted back to EQ.
        radioButtons(ns("data_source"),
                     NULL,
                     choices = stats::setNames(c("AC", "EQ"), c("AquaCache", "EQWin")),
                     selected = "EQ"),
        uiOutput(ns("EQWin_source_ui")),
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
        conditionalPanel(ns = ns,
                         condition = "input.data_source == 'EQ'",
                         # Toggle button for locations or location groups (only show if data source  == EQWin)
                         radioButtons(ns("locs_groups"),
                                      NULL,
                                      choices = c("Locations", "Location Groups"),
                                      selected = "Locations"),
                         # Selectize input for locations, populated once connection is established
                         selectizeInput(ns("locations_EQ"),
                                        "Select locations",
                                        choices = "Placeholder",
                                        multiple = TRUE),
                         # Selectize input for location groups, populated once connection is established. only shown if data source is EQWin
                         selectizeInput(ns("location_groups"),
                                        "Select a location group",
                                        choices = "Placeholder",
                                        multiple = FALSE,
                                        width = "100%"),
                         
                         # Toggle button for parameters or parameter groups (only show if data source == EQWin)
                         radioButtons(ns("params_groups"),
                                      NULL,
                                      choices = c("Parameters", "Parameter Groups"),
                                      selected = "Parameters"),
                         # Selectize input for parameters, populated once connection is established
                         selectizeInput(ns("parameters_EQ"),
                                        "Select parameters",
                                        choices = "Placeholder",
                                        multiple = TRUE,
                                        width = "100%"),
                         # Selectize input for parameter groups, populated once connection is established. only shown if data source is EQWin
                         selectizeInput(ns("parameter_groups"),
                                        "Select a parameter group",
                                        choices = "Placeholder",
                                        multiple = FALSE,
                                        width = "100%"),
                         
                         # Add a bit of space between the mandatory inputs and the optional ones
                         tags$br(),
                         
                         # Selectize input for standards, populated once connection is established
                         htmlOutput(ns("standard_note")),
                         selectizeInput(ns("stds"),
                                        "Select one or more standards to apply (optional)",
                                        choices = "Placeholder",
                                        multiple = TRUE,
                                        width = "100%"),
                         # TRUE/FALSE selection for station-specific standards
                         checkboxInput(ns("stnStds"), "Apply station-specific standards?", value = FALSE),
                         
                         tags$br(),
                         
                         # inputs for values outside a given SD
                         htmlOutput(ns("SD_note")),
                         div(
                           style = "display: flex; align-items: center;",
                           tags$label(
                             "Standard deviation threshold (leave empty to not calculate)", 
                             class = "form-label",
                             style = "margin-right: 5px;"
                           ),
                           span(
                             id = ns("SD_info"),
                             `data-bs-toggle` = "tooltip",
                             `data-bs-placement` = "right",
                             `data-bs-trigger` = "click hover",
                             title = "1 SD = 68% of data distribution, 2 SD = 95%, 3 SD = 99.7%. etc.",
                             icon("info-circle", style = "font-size: 150%; margin-left: 5px;")
                           )
                           
                         ),
                         numericInput(ns("SD_SD"), NULL, value = NULL),
                         
                         div(
                           style = "display: flex; align-items: center;",
                           tags$label(
                             "Start date for SD calculation", 
                             class = "form-label",
                             style = "margin-right: 5px;"
                           ),
                           span(
                             id = ns("SD_start_info"),
                             `data-bs-toggle` = "tooltip",
                             `data-bs-placement` = "right",
                             `data-bs-trigger` = "click hover",
                             title = "Leave empty to use data from the start of records.",
                             icon("info-circle", style = "font-size: 150%; margin-left: 5px;")
                           )
                         ),
                         dateInput(ns("SD_start"), NULL, value = NA),
                         
                         div(
                           style = "display: flex; align-items: center;",
                           tags$label(
                             "End date for SD calculation", 
                             class = "form-label",
                             style = "margin-right: 5px;"
                           ),
                           span(
                             id = ns("SD_end_info"),
                             `data-bs-toggle` = "tooltip",
                             `data-bs-placement` = "right",
                             `data-bs-trigger` = "click hover",
                             title = "Leave empty to use data to the end of records.",
                             icon("info-circle", style = "font-size: 150%; margin-left: 5px;")
                           )
                         ),
                         dateInput(ns("SD_end"), NULL, value = NA),
                         div(
                           style = "display: flex; align-items: center;",
                           tags$label(
                             "Select date range (year is ignored)", 
                             class = "form-label",
                             style = "margin-right: 5px;"
                           ),
                           span(
                             id = ns("SD_end_info"),
                             `data-bs-toggle` = "tooltip",
                             `data-bs-placement` = "right",
                             `data-bs-trigger` = "click hover",
                             title = "Use this to exclude data outside of a certain range. For example, if you only want to include data from May to September, select May 1st and September 30th. The year is ignored.",
                             icon("info-circle", style = "font-size: 150%; margin-left: 5px;")
                           )
                         ),
                         dateRangeInput(
                           ns("SD_date_range"), 
                           label = NULL, 
                           start = "2000-01-01", 
                           end = "2000-12-31"
                         )
                         
        ),
        
        conditionalPanel(ns = ns,
                         condition = "input.data_source == 'AC'",
                         # Selectize input for locations, populated once connection is established
                         selectizeInput(ns("locations_AC"),
                                        "Select locations",
                                        choices = "Placeholder",
                                        multiple = TRUE,
                                        width = "100%"),
                         # Selectize input for parameters, populated once connection is established
                         selectizeInput(ns("parameters_AC"),
                                        "Select parameters",
                                        choices = "Placeholder",
                                        multiple = TRUE,
                                        width = "100%")
        ),
        actionButton(ns("go"), "Create report"),
        downloadButton(ns("download"), "download", style = "visibility: hidden;") # Hidden; triggered automatically if 'go' is successful
      ) # End card_body
    ) # End card
  ) # End tagList
} # End WQReportUI


WQReport <- function(id, mdb_files, language) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns  # Used to create UI elements in the server code
    
    EQWin_selector <- reactiveVal(FALSE)
    observeEvent(input$data_source, {
      if (input$data_source == "AC") {
        shinyjs::hide("EQWin_source")
      } else {
        if (!EQWin_selector()) {
          EQWin_selector(TRUE)
          output$EQWin_source_ui <- renderUI({
            selectizeInput(ns("EQWin_source"), "EQWin database", choices = stats::setNames(mdb_files, basename(mdb_files)), selected = mdb_files[1])
          })
        }
        shinyjs::show("EQWin_source")
      }
    })
    
    # Fill in some htmlOutputs
    output$standard_note <- renderUI({
      HTML("<p>
      <i><b>Optional:</b> Select standards/guidelines and station specific standards/guidelines to apply.<br>
      General standards show up as an additional column in the report with values for each parameter. <br>
      Station-specific standards show up as notes in the report for each station.<br>
      Reported values which exceed standards/guidelines are highlighted in red with a note provided.
      </p>")
    })
    
    output$SD_note <- renderUI({
      HTML("<p>
      <i><b>Optional:</b> Select a standard deviation threshold to flag outlier values.<br>
      A mean and standard deviation will be calculated using past measurements if they exist.<br>
      <b>This can add a lot of time to the report generation process, be patient!</b>
      </p>")
    })
    
    # Get the data to populate drop-downs. Runs every time this module is loaded.
    moduleData <- reactiveValues()
    # moduleData$AC_locs <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT loc.location_id, loc.name FROM locations loc INNER JOIN timeseries ts ON loc.location_id = ts.location_id")
    # moduleData$AC_params <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT DISTINCT p.parameter_id, p.param_name, p.unit_default AS unit FROM parameters p INNER JOIN timeseries ts ON p.parameter_id = ts.parameter_id")
    
    observeEvent(input$EQWin_source, {
      EQWin <- AccessConnect(input$EQWin_source, silent = TRUE)
      EQ_locs <- DBI::dbGetQuery(EQWin, paste0("SELECT StnCode, StnDesc FROM eqstns;"))
      EQ_loc_grps <- DBI::dbGetQuery(EQWin, "SELECT groupname, groupdesc, groupitems FROM eqgroups WHERE dbtablename = 'eqstns'")
      EQ_params <- DBI::dbGetQuery(EQWin, paste0("SELECT ParamId, ParamCode, ParamDesc, Units AS unit FROM eqparams;"))
      EQ_param_grps <- DBI::dbGetQuery(EQWin, "SELECT groupname, groupdesc, groupitems FROM eqgroups WHERE dbtablename = 'eqparams'")
      EQ_stds <- DBI::dbGetQuery(EQWin, "SELECT StdName, StdCode FROM eqstds")
      DBI::dbDisconnect(EQWin)
      
      # Check encoding and if necessary convert to UTF-8
      locale_info <- Sys.getlocale("LC_CTYPE")
      encoding <- sub(".*\\.([^@]+).*", "\\1", locale_info)
      tryCatch({
        grepl("[^\x01-\x7F]", EQ_locs$StnDesc)
      }, warning = function(w) {
        if (encoding != "utf8") {
          EQ_locs$StnDesc <<- iconv(EQ_locs$StnDesc, from = encoding, to = "UTF-8")
        }
      })
      
      moduleData$EQ_locs <- EQ_locs
      moduleData$EQ_loc_grps <- EQ_loc_grps
      moduleData$EQ_params <- EQ_params
      moduleData$EQ_param_grps <- EQ_param_grps
      moduleData$EQ_stds <- EQ_stds
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    observe({
      if (is.null(mdb_files)) {
        shinyjs::hide("data_source")
        shinyjs::hide("EQWin_source")
        updateRadioButtons(session, "data_source", selected = "AC")
      }
    })
    
    observe({
      req(input$data_source, moduleData)
      # !Since the report generation function is not yet compatible with AC, a modal is shown to the user and the data source is reset to EQ.
      if (input$data_source == "EQ") {
        req(moduleData$EQ_params)
        updateSelectizeInput(session, "parameters_EQ", choices = stats::setNames(moduleData$EQ_params$ParamCode, paste0(moduleData$EQ_params$ParamCode, " (", moduleData$EQ_params$ParamDesc, ")")), server = TRUE)
        updateSelectizeInput(session, "parameter_groups", choices = moduleData$EQ_param_grps$groupname, server = TRUE)
        updateSelectizeInput(session,"locations_EQ", choices = stats::setNames(moduleData$EQ_locs$StnCode, paste0(moduleData$EQ_locs$StnCode, " (", moduleData$EQ_locs$StnDesc, ")")), server = TRUE)
        updateSelectizeInput(session, "location_groups", choices = moduleData$EQ_loc_grps$groupname, server = TRUE)
        updateSelectizeInput(session, "stds", choices = stats::setNames(moduleData$EQ_stds$StdCode, moduleData$EQ_stds$StdName), server = TRUE)
      } else if (input$data_source == "AC") { # AC selected
        showModal(modalDialog(
          title = "Data Source Not Implemented",
          "This report generation function is not yet compatible with AquaCache.",
          easyClose = TRUE
        ))
        updateRadioButtons(session, "data_source", selected = "EQ")
        # updateSelectizeInput(session, "parameters_AC", choices = moduleData$AC_params$param_name, server = TRUE)
        # updateSelectizeInput(session, "locations_AC", choices = moduleData$AC_locs$name, server = TRUE)
      }
    })
    
    # Toggle visibility of location and location group inputs
    observeEvent(input$locs_groups, {
      if (input$locs_groups == "Location Groups") {
        shinyjs::show("location_groups")
        shinyjs::hide("locations_EQ")
      } else {
        shinyjs::hide("location_groups")
        shinyjs::show("locations_EQ")
      }
    })
    observeEvent(input$params_groups, {
      if (input$params_groups == "Parameter Groups") {
        shinyjs::show("parameter_groups")
        shinyjs::hide("parameters_EQ")
      } else {
        shinyjs::hide("parameter_groups")
        shinyjs::show("parameters_EQ")
      }
    })
    
    outputFile <- reactiveVal(NULL) # Will hold path to the file if successful at creating
    
    observeEvent(input$go, {
      tryCatch({
        
        withProgress(
          message = tr("generating_working", language$language), value = 0, 
          {
            incProgress(0.5)
            
            if (!is.na(input$SD_SD)) {
              SD_doy_range <- c(lubridate::yday(input$SD_date_range[1]), lubridate::yday(input$SD_date_range[2]))
            } else {
              SD_doy_range <- NULL
            }
            
            dir <- paste0(tempdir(), "/WQReport")
            
            dir.create(dir, showWarnings = FALSE)
            
            EQWin <- AccessConnect(input$EQWin_source, silent = TRUE)
            EQWinReport(date = input$date,
                        date_approx = as.numeric(input$date_approx),
                        stations = if (input$locs_groups == "Locations") input$locations_EQ else NULL,
                        stnGrp = if (input$locs_groups == "Location Groups") input$location_groups else NULL,
                        parameters = if (input$params_groups == "Parameters") input$parameters_EQ else NULL,
                        paramGrp = if (input$params_groups == "Parameter Groups") input$parameter_groups else NULL,
                        stds = input$stds,
                        stnStds = input$stnStds,
                        SD_exceed = if (!is.na(input$SD_SD)) input$SD_SD else NULL,
                        SD_start = if (length(input$SD_start) > 0) input$SD_start else NULL,
                        SD_end = if (length(input$SD_end) > 0) input$SD_end else NULL,
                        SD_doy = SD_doy_range,
                        save_path = dir,
                        con = EQWin)
            DBI::dbDisconnect(EQWin)
            
            tmpfile <- list.files(dir, full.names = TRUE)
            outputFile(tmpfile)
            
            # Now programmatically click the hidden download button
            shinyjs::click("download")
            
            incProgress(1)
          } # End withProgress content
        ) # End withProgress
        
      }, error = function(e) {
        showNotification(
          paste("Error generating water quality report:", e$message),
          type = "error",
          duration = NULL,
          closeButton = TRUE
        )
      })
    })
    # Create the download
    output$download <- downloadHandler(
      
      filename = function() {
        paste0("water quality report for ", input$date, " Issued ", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        file.copy(outputFile(), file)
      }, # End content
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
    ) # End downloadHandler
    
  }) # End moduleServer
} # End WQReportServer


