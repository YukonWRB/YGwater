snowInfoUIMod <- function(id) {
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

snowInfoMod <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns  # Used to create UI elements in the server code
    
    moduleData <- reactiveValues(
      locs = dbGetQueryDT(session$userData$AquaCache, "
              SELECT DISTINCT l.location_id, l.location, l.name, l.name_fr
              FROM locations AS l
              JOIN locations_networks AS ln ON l.location_id = ln.location_id
              JOIN networks AS n ON ln.network_id = n.network_id
              JOIN samples AS s ON l.location_id = s.location_id
              WHERE n.name = 'Snow Survey Network';"
      )
    )
    
    # Create reactiveValues to store the user's selections. Used if switching between languages.
    selections <- reactiveValues(
      locations = "all",
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
        selectizeInput(ns("loc"), 
                       label = tr("gen_loc_select", language$language),
                       choices = stats::setNames(
                         c("all", moduleData$locs$location),
                         c(tr("all_locs", language$language),
                           titleCase(moduleData$locs[[tr("generic_name_col", language$language)]], language$abbrev))
                       ),
                       selected = selections$loc,
                       multiple = TRUE,
                       width = "100%"),
        # Inactive locations?
        checkboxInput(ns("inactive"),
                      label = tr("gen_snowInfo_inactive", language$language),
                      value = selections$inactive,
                      width = "100%"),
        # Complete years only?
        checkboxInput(ns("complete"),
                      label = tr("gen_snowInfo_complete", language$language),
                      value = selections$complete,
                      width = "100%"),
        # Generate stats?
        # Left blank for now (default TRUE)
        # Generate plots?
        checkboxInput(ns("plots"),
                      label = tr("gen_generate_plots", language$language),
                      value = selections$plots,
                      width = "100%"),
        
        # Type of plot
        selectizeInput(ns("ptype"),
                       label = tr("gen_snowInfo_ptype", language$language),
                       choices = stats::setNames(
                         c("combined", "separate"),
                         c(tr("gen_snowInfo_ptype_combine", language$language), tr("gen_snowInfo_ptype_separate", language$language))
                       ),
                       selected = selections$ptype,
                       multiple = FALSE,
                       width = "100%"),
        
        # Make it happen
        actionButton(ns("go"), label = tr("create_report", language$language)),
        downloadButton(ns("download"), "download", style = "visibility: hidden;") # Hidden; triggered automatically but left hidden if 'go' is successful
        
      ) # End tagList
    }) %>% # End renderUI
      bindEvent(language$language, moduleData$locs) # Re-render the UI if the language or moduleData changes
    
    output$info <- renderText({
      tr("gen_snowInfo_info", language$language)
    }) %>% bindEvent(language$language) # Re-render the text if the language changes
    
    # Observe inputs and store in object 'selections'
    observeEvent(input$loc, {
      selections$loc <- input$loc
    }, ignoreInit = TRUE)
    observeEvent(input$inactive, {
      selections$inactive <- input$inactive
    }, ignoreInit = TRUE)
    observeEvent(input$complete, {
      selections$complete <- input$complete
    }, ignoreInit = TRUE)
    # observeEvent(input$stats, {
    #   selections$stats <- input$stats
    # }, ignoreInit = TRUE)
    observeEvent(input$plots, {
      selections$plots <- input$plots
    }, ignoreInit = TRUE)
    observeEvent(input$plot_type, {
      selections$plot_type <- input$plot_type
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
    observeFilterInput("loc")
    
    outputFile <- reactiveVal(NULL) # Will hold path to the file if successful at creating
    
    observeEvent(input$go, {
      
      if (nchar(selections$loc) == 0) {
        showNotification(
          tr("gen_no_loc", language$language),
          type = "error",
          duration = NULL,
          closeButton = TRUE
        )
        return()
      }
      
      tryCatch({
        withProgress(
          message = tr("dl_prep", language$language), 
          value = 0, 
          {
            incProgress(0.2)
            
            # 1. Create a temporary folder
            dir <- paste0(tempdir(), "/snowInfoOutput")
            dir.create(dir, showWarnings = FALSE)
            # Delete all files in the folder (if any)
            files <- list.files(dir, full.names = TRUE)
            if (length(files) > 0) unlink(files, recursive = TRUE, force = TRUE)
            
            # 3. Call snowInfo() so it writes all files to 'dir'
            suppressWarnings(snowInfo(
              con = session$userData$AquaCache,
              locations = selections$loc,
              inactive = selections$inactive,
              stats = selections$stats,
              complete_yrs = selections$complete,
              plots = selections$plots,
              plot_type = selections$plot_type,
              save_path = dir,
              quiet = TRUE
            ))
            
            incProgress(0.7)
            
            # 4. Zip up everything in 'dir' and write the zip to `file`
            files <- list.files(dir, full.names = TRUE)
            
            zip::zipr(zipfile = paste0(dir, "/report.zip"), files = files)
            outputFile(paste0(dir, "/report.zip"))
            
            # Delete everything in the 'dir' except for the .zip file
            files <- list.files(dir, full.names = TRUE)
            files <- files[!grepl("report.zip", files)]
            if (length(files) > 0) unlink(files, recursive = TRUE, force = TRUE)
            
            # 5. Now programmatically click the hidden download button
            shinyjs::click("download")
            
            incProgress(1)
            
          } # End withProgress content
        ) # End withProgress
        
      }, error = function(e) {
        showNotification(
          paste("Error generating snow information report:", e$message),
          type = "error",
          duration = NULL,
          closeButton = TRUE
        )
      })
    })
    
    # Listen for the 'go' and make the report when called
    output$download <- downloadHandler(
      filename = function() {
        paste0("Snow info report issued ", Sys.Date(), ".zip")
      },
      content = function(file) {
        file.copy(outputFile(), file)
        # Now delete the zip file and the directory it's in
        unlink(dirname(outputFile()), recursive = TRUE, force = TRUE)
      }, # End content
      contentType = "application/zip"
    ) # End downloadHandler
    
  }) # End moduleServer
} # End snowInfoServer


