discretePlotUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      # Toggle for data source
      radioButtons(ns("data_source"),
                   NULL,
                   choices = stats::setNames(c("AC", "EQ"), c("AquaCache", "EQWin")),
                   selected = "AC"),
      # start and end datetime
      dateRangeInput(ns("date_range"),
                     "Select date range",
                     start = Sys.Date() - 30,
                     end = Sys.Date(),
                     max = Sys.Date() + 1,
                     format = "yyyy-mm-dd"),
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
                                      multiple = FALSE),
                       
                       # Toggle button for parameters or parameter groups (only show if data source == EQWin)
                       radioButtons(ns("params_groups"),
                                    NULL,
                                    choices = c("Parameters", "Parameter Groups"),
                                    selected = "Parameters"),
                       # Selectize input for parameters, populated once connection is established
                       selectizeInput(ns("parameters_EQ"),
                                      "Select parameters",
                                      choices = "Placeholder",
                                      multiple = TRUE),
                       # Selectize input for parameter groups, populated once connection is established. only shown if data source is EQWin
                       selectizeInput(ns("parameter_groups"),
                                      "Select a parameter group",
                                      choices = "Placeholder",
                                      multiple = FALSE)
                       ),
      conditionalPanel(ns = ns,
                       condition = "input.data_source == 'AC'",
                       # Selectize input for locations, populated once connection is established
                       selectizeInput(ns("locations_AC"),
                                      "Select locations",
                                      choices = "Placeholder",
                                      multiple = TRUE),
                       # Selectize input for parameters, populated once connection is established
                       selectizeInput(ns("parameters_AC"),
                                      "Select parameters",
                                      choices = "Placeholder",
                                      multiple = TRUE)
                       ),
      radioButtons(ns("facet_on"),
                   "Facet on",
                   choices = stats::setNames(c("locs", "params"), c("Locations", "Parameters")),
                   selected = "locs"),
      checkboxInput(ns("log_scale"),
                    "Log scale"),
      checkboxInput(ns("target_datetime"),
                    "Use target instead of actual datetime"),
      checkboxInput(ns("colorblind"),
                    "Colorblind friendly"),
      radioButtons(ns("lang"),
                   NULL,
                   choices = stats::setNames(c("en", "fr"), c("English", "French")),
                   selected = "en"),
      actionButton(ns("make_plot"),
                   "Create Plot")
    ),
    mainPanel(
      plotly::plotlyOutput(ns("plot"), width = "100%", height = "800px", inline = TRUE),
      uiOutput(ns("full_screen_ui"))
    )
  )
}

discretePlotServer <- function(id, EQWin, AquaCache) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns  # Used to create UI elements within the server code
    
    # Get the data to populate drop-downs. Runs every time this module is loaded.
    data <- reactiveValues()
    observe({
      print("here")
      EQ_locs <- DBI::dbGetQuery(EQWin, paste0("SELECT StnCode, StnDesc FROM eqstns;"))
      EQ_loc_grps <- DBI::dbGetQuery(EQWin, "SELECT groupname, groupdesc, groupitems FROM eqgroups WHERE dbtablename = 'eqstns'")
      EQ_params <- DBI::dbGetQuery(EQWin, paste0("SELECT ParamId, ParamCode, ParamDesc, Units AS unit FROM eqparams;"))
      EQ_param_grps <- DBI::dbGetQuery(EQWin, "SELECT groupname, groupdesc, groupitems FROM eqgroups WHERE dbtablename = 'eqparams'")
      AC_locs <- DBI::dbGetQuery(AquaCache, "SELECT loc.location_id, loc.name FROM locations loc INNER JOIN timeseries ts ON loc.location_id = ts.location_id WHERE ts.category = 'discrete'")
      AC_params <- DBI::dbGetQuery(AquaCache, "SELECT DISTINCT p.param_code, p.param_name, p.unit_default AS unit FROM parameters p INNER JOIN timeseries ts ON p.param_code = ts.parameter WHERE ts.category = 'discrete'")
      
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
      
      data$EQ_locs <- EQ_locs
      data$EQ_loc_grps <- EQ_loc_grps
      data$EQ_params <- EQ_params
      data$EQ_param_grps <- EQ_param_grps
      data$AC_locs <- AC_locs
      data$AC_params <- AC_params
    })
    
    
    
    observeEvent(input$data_source, {
      if (input$data_source == "EQ") {
        updateSelectizeInput(session, "parameters_EQ", choices = stats::setNames(data$EQ_params$ParamCode, paste0(data$EQ_params$ParamCode, " (", data$EQ_params$ParamDesc, ")")), server = TRUE)
        updateSelectizeInput(session, "parameter_groups", choices = data$EQ_param_grps$groupname, server = TRUE)
        updateSelectizeInput(session,"locations_EQ", choices = stats::setNames(data$EQ_locs$StnCode, paste0(data$EQ_locs$StnCode, " (", data$EQ_locs$StnDesc, ")")), server = TRUE)
        updateSelectizeInput(session, "location_groups", choices = data$EQ_loc_grps$groupname, server = TRUE)
      } else if (input$data_source == "AC") { # AC selected
        updateSelectizeInput(session, "parameters_AC", choices = data$AC_params$param_name, server = TRUE)
        updateSelectizeInput(session, "locations_AC", choices = data$AC_locs$name, server = TRUE)
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
    
    # Create and render the plot ############################################################
    observeEvent(input$make_plot, {
      shinyjs::hide("full_screen")
      
      # Check if locations/location_group is not NULL, depending on the selection for input$locs_groups
      if (input$data_source == "EQ") {
        if (input$locs_groups == "Locations") {
          if (is.null(input$locations_EQ)) {
            showModal(modalDialog("Please select at least one location.", easyClose = TRUE))
            return()
          }
        } else {
          if (is.null(input$location_groups)) {
            showModal(modalDialog("Please select one location group.", easyClose = TRUE))
            return()
          }
        }
        # Same treatment for parameters/parameter_groups
        if (input$params_groups == "Parameters") {
          if (is.null(input$parameters_EQ)) {
            showModal(modalDialog("Please select at least one parameter.", easyClose = TRUE))
            return()
          }
        } else {
          if (is.null(input$parameter_groups)) {
            showModal(modalDialog("Please select one parameter group.", easyClose = TRUE))
            return()
          }
        }
      } else if (input$data_source == "AC") {
        if (is.null(input$locations_AC)) {
          showModal(modalDialog("Please select at least one location.", easyClose = TRUE))
          return()
        }
        if (is.null(input$parameters_AC)) {
          showModal(modalDialog("Please select at least one parameter.", easyClose = TRUE))
          return()
        }
      }

      tryCatch({
        if (input$data_source == "EQ") {
          plot <- plotDiscrete(start = input$date_range[1],
                               end = input$date_range[2],
                               locations = if (input$locs_groups == "Locations") input$locations_EQ else NULL,
                               locGrp = if (input$locs_groups == "Location Groups") input$location_groups else NULL,
                               parameters = if (input$params_groups == "Parameters") input$parameters_EQ else NULL,
                               paramGrp = if (input$params_groups == "Parameter Groups") input$parameter_groups else NULL,
                               log = input$log_scale,
                               facet_on = input$facet_on,
                               target_datetime = input$target_datetime,
                               colorblind = input$colorblind,
                               lang = input$lang,
                               dbSource = input$data_source)
        } else if (input$data_source == "AC") {
          plot <- plotDiscrete(start = input$date_range[1],
                               end = input$date_range[2],
                               locations =input$locations_AC,
                               locGrp = NULL,
                               parameters = input$parameters_AC,
                               paramGrp = NULL,
                               log = input$log_scale,
                               facet_on = input$facet_on,
                               target_datetime = input$target_datetime,
                               colorblind = input$colorblind,
                               lang = input$lang,
                               dbSource = input$data_source)
        }
        
        
        output$plot <- plotly::renderPlotly(plot)
        
        output$full_screen_ui <- renderUI({
          actionButton(ns("full_screen"), "Full screen")
        })
        
        shinyjs::show("full_screen")
      }, error = function(e) {
        showModal(modalDialog(paste0("An error occurred while creating the plot. Please check your inputs and try again.\n  \n  Error: ", e$message), easyClose = TRUE))
        return()
      })
    }, ignoreInit = TRUE) # End of plot rendering loop
    
    observeEvent(input$full_screen, {
      shinyjs::runjs(paste0("toggleFullScreen('", session$ns("plot"), "');"))
    }, ignoreInit = TRUE)
    
  }) # End of moduleServer
}
