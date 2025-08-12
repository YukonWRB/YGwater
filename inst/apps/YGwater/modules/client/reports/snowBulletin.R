snowBulletinUIMod <- function(id) {
  ns <- NS(id)
  tagList(
    # Custom CSS below is for consistency with the sidebarPanel look elsewhere in the app.
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/card_background.css")),
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
    
    ns <- session$ns  # Used to create UI elements in the server code
    
    # Create reactiveValues to store the user's selections. Used if switching between languages.
    selections <- reactiveValues(
      stats = TRUE,
      year = lubridate::year(Sys.Date()),
      month = if (lubridate::month(Sys.Date()) %in% c(3:5)) lubridate::month(Sys.Date()) else 3,
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
        radioButtons(ns("stats"),
                     label = NULL,
                     choices = stats::setNames(
                       c(TRUE, FALSE),
                       c(tr("gen_snowBul_toggle_stats", language$language), tr("gen_snowBul_toggle_bulletin", language$language))
                     ),
                     selected = selections$stats,
                     inline = TRUE,
                     width = "100%"),
        # selector for year
        selectizeInput(ns("year"), 
                       label = tr("gen_snowBul_year", language$language),
                       choices = c(1980:lubridate::year(Sys.Date())),
                       selected = selections$year,
                       multiple = FALSE,
                       width = "100%"),
        # Selector for month
        selectizeInput(ns("month"), 
                       label = tr("gen_snowBul_month", language$language),
                       choices = c(3:5),
                       selected = selections$month,
                       multiple = FALSE,
                       width = "100%"),
        # Selector for basins
        selectizeInput(ns("basins"), 
                       label = tr("gen_snowBul_basins", language$language),
                       choices = stats::setNames(c("all", "Upper Yukon", "Teslin", "Central Yukon", "Pelly", "Stewart", "White", 
                                                   "Lower Yukon", "Porcupine", "Peel", "Liard", "Alsek"),
                                                 c(tr("all", language$language), "Upper Yukon", "Teslin", "Central Yukon", "Pelly", "Stewart", "White", 
                                                   "Lower Yukon", "Porcupine", "Peel", "Liard", "Alsek")
                       ),
                       selected = selections$basins,
                       multiple = TRUE,
                       width = "100%"),
        # Selector for language
        selectizeInput(ns("language"),
                       label = tr("gen_snowBul_lang", language$language),
                       choices = stats::setNames(c("English", "French"),
                                                 c(tr("english", language$language), tr("francais", language$language))
                       ),
                       selected = selections$language,
                       multiple = FALSE,
                       width = "100%"),
        # Selector for precipitation period
        selectizeInput(ns("precip_period"),
                       label = tr("gen_snowBul_precip_period", language$language),
                       choices = stats::setNames(c("last 40 years", "all years", "1981-2010", "1991-2020"),
                                                 c(tr("gen_snowBul_period1", language$language),
                                                   tr("gen_snowBul_period2", language$language),
                                                   tr("gen_snowBul_period3", language$language),
                                                   tr("gen_snowBul_period4", language$language))),
                       selected = selections$precip_period,
                       multiple = FALSE,
                       width = "100%"),
        # Selector for CDDF period
        selectizeInput(ns("cddf_period"),
                       label = tr("gen_snowBul_cddf_period", language$language),
                       choices = stats::setNames(c("last 40 years", "all years", "1981-2010", "1991-2020"),
                                                 c(tr("gen_snowBul_period1", language$language),
                                                   tr("gen_snowBul_period2", language$language),
                                                   tr("gen_snowBul_period3", language$language),
                                                   tr("gen_snowBul_period4", language$language))),
                       selected = selections$cddf_period,
                       multiple = FALSE,
                       width = "100%"),
        # Plot scale
        numericInput(ns("scale"),
                     label = tr("gen_snowBul_scale", language$language),
                     value = selections$scale,
                     min = 0.5,
                     max = 3,
                     step = 0.1,
                     width = "100%"),
        
        # Make it happen
        actionButton(ns("go"), label = tr("create_report", language$language)),
        downloadButton(ns("download"), "download", style = "visibility: hidden;") # Hidden; triggered automatically but left hidden if 'go' is successful
        
      ) # End tagList
    }) %>% # End renderUI
      bindEvent(language$language) # Re-render the UI if the language or data changes
    
    output$info <- renderText({
      tr("gen_snowBul_info", language$language)
    }) %>% bindEvent(language$language) # Re-render the text if the language changes
    
    
    # Show/hide elements depending on input$stats
    
    
    # Observe inputs and store in object 'selections'
    observeEvent(input$stats, { # Also Show/hide elements depending on input$stats
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
    }, ignoreInit = TRUE)
    observeEvent(input$year, {
      selections$year <- as.numeric(input$year)
    }, ignoreInit = TRUE)
    observeEvent(input$month, {
      selections$month <- as.numeric(input$month)
    }, ignoreInit = TRUE)
    observeEvent(input$basins, {
      selections$basins <- input$basins
    }, ignoreInit = TRUE)
    observeEvent(input$language, {
      selections$language <- input$language
    }, ignoreInit = TRUE)
    observeEvent(input$precip_period, {
      selections$precip_period <- input$precip_period
    }, ignoreInit = TRUE)
    observeEvent(input$cddf_period, {
      selections$cddf_period <- input$cddf_period
    }, ignoreInit = TRUE)
    observeEvent(input$scale, {
      selections$scale <- input$scale
    }, ignoreInit = TRUE)
    
    # Adjust filter selections based on if 'all' is selected (remove selections other than 'all') ################
    observeFilterInput <- function(inputId) {
      observeEvent(input[[inputId]], {
        # Check if 'all' is selected and adjust accordingly
        if (length(input[[inputId]]) > 1) { # If 'all' was selected last, remove all other selections
          if (input[[inputId]][length(input[[inputId]])] == "all") {
            updateSelectizeInput(session, inputId, selected = "all")
          } else if ("all" %in% input[[inputId]]) { # If 'all' is already selected and another option is selected, remove 'all'
            updateSelectizeInput(session, inputId, selected = input[[inputId]][length(input[[inputId]])])
          }
        }
      })
    }
    observeFilterInput("basins")
    
    outputFile <- reactiveVal(NULL) # Will hold path to the file if successful at creating
    
    observeEvent(input$go, {
      
      tryCatch({
        withProgress(
          message = tr("dl_prep", language$language), 
          value = 0, 
          {
            incProgress(0.2)
            
            # 1. Create a temporary folder
            dir <- paste0(tempdir(), "/snowBulletinOutput")
            dir.create(dir, showWarnings = FALSE)
            # Delete all files in the folder (if any)
            files <- list.files(dir, full.names = TRUE)
            if (length(files) > 0) unlink(files, recursive = TRUE, force = TRUE)
            
            # 3. Call the appropriate function so it writes all files to 'dir'
            suppressWarnings({
              if (selections$stats) {
                snowBulletinStats(year = selections$year,
                                  month = selections$month,
                                  basins = if (selections$basins == "all") NULL else selections$basins,
                                  save_path = dir,
                                  excel_output = TRUE,
                                  con = session$userData$AquaCache,
                                  source = "aquacache",
                                  synchronize = FALSE)
              } else {
                snowBulletin(year = selections$year,
                             month = selections$month,
                             basins = if (selections$basins == "all") NULL else selections$basins,
                             scale = selections$scale,
                             save_path = dir,
                             con = session$userData$AquaCache,
                             precip_period = selections$precip_period,
                             cddf_period = selections$cddf_period,
                             language = tolower(selections$language)
                )
              }
            })
            
            incProgress(0.7)
            
            if (selections$stats) {
              # Zip up everything in 'dir' and write the zip to `file`
              files <- list.files(dir, full.names = TRUE)
              
              zip::zip(zipfile = paste0(dir, "/report.zip", files = files, mode = "cherry-pick", include_directories = FALSE))
              outputFile(paste0(dir, "/report.zip"))
              
              # Delete everything in the 'dir' except for the .zip file
              files <- list.files(dir, full.names = TRUE)
              files <- files[!grepl("report.zip", files)]
              if (length(files) > 0) unlink(files, recursive = TRUE, force = TRUE)
            } else {
              file <- list.files(dir, full.names = TRUE)
              outputFile(file)
            }
            
            # 5. Now programmatically click the hidden download button
            shinyjs::click("download")
            
            incProgress(1)
            
          } # End withProgress content
        ) # End withProgress
        
      }, error = function(e) {
        showNotification(
          if (selections$stats) {
            paste0(tr("gen_snowBul_error_stats", language$language), ": ", e$message)
          } else {
            paste0(tr("gen_snowBul_error_report", language$language), ": ", e$message)
          },
          type = "error",
          duration = NULL,
          closeButton = TRUE
        )
      })
    })
    
    # Listen for the 'go' and make the report when called
    output$download <- downloadHandler(
      filename = function() {
        if (selections$stats) {
          paste0("Snow bulletin stats issued ", Sys.Date(), ".zip")
        } else {
          paste0("Snow bulletin issued ", Sys.Date(), ".docx")
        }
      },
      content = function(file) {
        file.copy(outputFile(), file)
        # Now delete the zip file and the directory it's in
        unlink(dirname(outputFile()), recursive = TRUE, force = TRUE)
      }, # End content
      if (selections$stats) {
        contentType = "application/zip"
      } else {
        contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
      }
      
    ) # End downloadHandler
    
  }) # End moduleServer
} # End snowInfoServer


