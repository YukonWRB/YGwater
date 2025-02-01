waterInfoUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    # Custom CSS below is for consistency with the sidebarPanel look elsewhere in the app.
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/background_style.css")),
    div(class = "custom-panel container",   # This div holds all UI elements for the menu
        uiOutput(ns("menu")) # UI is rendered in the server function below so that it can use database information as well as language selections.
        
    )
  )
}

waterInfoServer <- function(id, con, language) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns  # Used to create UI elements in the server code
    
    data <- reactiveValues(
      locs = dbGetQueryDT(con, "SELECT DISTINCT ts.location_id, ts.location, l.name, l.name_fr, ts.parameter_id, p.param_name, p.param_name_fr FROM timeseries AS ts JOIN parameters AS p ON ts.parameter_id = p.parameter_id JOIN locations AS l on ts.location_id = l.location_id WHERE ts.parameter_id IN (1150, 1165)")
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
        req(data, language$language, language$abbrev)
        tagList(
          # selector for one parameter (flow if exists, else level) or both
          selectizeInput(ns("param"),
                         label = t("gen_waterInfo_param_select", language$language),
                         choices = stats::setNames(
                           c("one", "both"),
                           c(t("gen_waterInfo_param_select_one", language$language), t("gen_waterInfo_param_select_both", language$language))
                         ), 
                         selected = selections$param,
                         multiple = FALSE,
                         width = "100%"),
          
          # selector for location
          selectizeInput(ns("loc"), 
                         label = t("gen_waterInfo_loc_select", language$language),
                         choices = stats::setNames(
                           c("all", data$locs$location),
                           c(t("all_locs", language$language),
                             titleCase(data$locs[[t("generic_name_col", language$language)]], language$abbrev))
                         ),
                         selected = selections$loc,
                         multiple = TRUE,
                         width = "100%"),
          
          # end date to use in calculations
          dateInput(ns("end"), 
                    label = t("gen_waterInfo_end_date", language$language),
                    value = selections$end,
                    width = "100%"),
        
        # month ranges
        selectizeInput(ns("min_m"),
                       label = t("gen_waterInfo_min_months", language$language),
                       choices = stats::setNames(
                         c(1:12),
                         c(t("jan", language$language), t("feb", language$language), t("mar", language$language), t("apr", language$language), t("may", language$language), t("jun", language$language), t("jul", language$language), t("aug", language$language), t("sep", language$language), t("oct", language$language), t("nov", language$language), t("dec", language$language))
                       ),
                       selected = selections$min_m,
                       multiple = TRUE,
                       width = "100%"),
        
        selectizeInput(ns("max_m"),
                       label = t("gen_waterInfo_max_months", language$language),
                       choices = stats::setNames(
                         c(1:12),
                         c(t("jan", language$language), t("feb", language$language), t("mar", language$language), t("apr", language$language), t("may", language$language), t("jun", language$language), t("jul", language$language), t("aug", language$language), t("sep", language$language), t("oct", language$language), t("nov", language$language), t("dec", language$language))
                       ),
                       selected = selections$max_m,
                       multiple = TRUE,
                       width = "100%"),
        
        # percent allowed missing
        numericInput(ns("prct"),
                     label = t("gen_waterInfo_prct_miss", language$language),
                     min = 1,
                     max = 100,
                     value = selections$prct,
                     width = "100%"),
        
        # Generate plots?
        checkboxInput(ns("plots"),
                      label = t("gen_waterInfo_plots", language$language),
                      value = selections$plots,
                      width = "100%"),
        
        # Type of plot
        selectizeInput(ns("ptype"),
                       label = t("gen_waterInfo_ptype", language$language),
                       choices = stats::setNames(
                         c("combined", "separate"),
                         c(t("gen_waterInfo_ptype_combine", language$language), t("gen_waterInfo_ptype_separate", language$language))
                       ),
                       selected = selections$ptype,
                       multiple = FALSE,
                       width = "100%"),
        
        # Make it happen
        actionButton(ns("go"), label = t("create_report", language$language)),
        downloadButton(ns("download"), "download", style = "visibility: hidden;") # Hidden; triggered automatically but left hidden if 'go' is successful
      
        ) # End tagList
      }) %>% # End renderUI
        bindEvent(language$language, data$locs) # Re-render the UI if the language or data changes
      
      # Observe inputs and store in object 'selections'
      observeEvent(input$param, {
        selections$param <- input$param
      }, ignoreInit = TRUE)
      observeEvent(input$loc, {
        selections$loc <- input$loc
      }, ignoreInit = TRUE)
      observeEvent(input$end, {
        selections$end <- input$end
      }, ignoreInit = TRUE)
      observeEvent(input$min_m, {
        selections$min_m <- input$min_m
      }, ignoreInit = TRUE)
      observeEvent(input$max_m, {
        selections$max_m <- input$max_m
      }, ignoreInit = TRUE)
      observeEvent(input$prct, {
        selections$prct <- input$prct
      }, ignoreInit = TRUE)
      observeEvent(input$plots, {
        selections$plots <- input$plots
        if (input$plots) {
          shinyjs::show("ptype")
        } else {
          shinyjs::hide("ptype")
        }
      }, ignoreInit = TRUE)
      observeEvent(input$ptype, {
        selections$ptype <- input$ptype
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
        tryCatch({
          withProgress(
            message = t("generating_working", language$language), 
            value = 0, 
            {
              incProgress(0.2)
              
              # 1. Create a temporary folder
              dir <- paste0(tempdir(), "/waterInfoOutput")
              dir.create(dir, showWarnings = FALSE)
              # Delete all files in the folder (if any)
              files <- list.files(dir, full.names = TRUE)
              if (length(files) > 0) unlink(files, recursive = TRUE, force = TRUE)
              
              # 3. Call waterInfo() so it writes all files to 'dir'
              suppressWarnings(waterInfo(
                con = con,
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
            paste("Error generating water quantity/info report:", e$message),
            type = "error",
            duration = NULL,
            closeButton = TRUE
          )
        })
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


