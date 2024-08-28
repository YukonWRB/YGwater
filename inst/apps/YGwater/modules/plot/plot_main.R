plotUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectizeInput(ns("plot_type"),
                       "Select a data type to plot",
                       choices = c("Discrete", "Continuous"),
                       selected = "Discrete"),
        conditionalPanel(ns = ns,
                         condition = "input.plot_type == 'Discrete'",
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
                         
                         # Toggle button for locations or location groups (only show if data source  == EQWin)
                         radioButtons(ns("locs_groups"),
                                      NULL,
                                      choices = c("Locations", "Location Groups"),
                                      selected = "Locations"),
                         # Selectize input for locations, populated once connection is established
                         selectizeInput(ns("locations"),
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
                         selectizeInput(ns("parameters"),
                                        "Select parameters",
                                        choices = "Placeholder",
                                        multiple = TRUE),
                         # Selectize input for parameter groups, populated once connection is established. only shown if data source is EQWin
                         selectizeInput(ns("parameter_groups"),
                                        "Select a parameter group",
                                        choices = "Placeholder",
                                        multiple = FALSE),
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
        conditionalPanel(ns = ns,
                      condition = "input.plot_type == 'Continuous'",
                      actionButton(ns("make_plot"),
                                   "Create Plot")
      )
      ),
      
      mainPanel(
        conditionalPanel(ns = ns,
                         condition = "input.plot_type == 'Discrete'",
                         plotly::plotlyOutput(ns("plot"), width = "100%", height = "800px", inline = TRUE),
                         actionButton(ns("full_screen"),
                                      "Full screen")
        ),
        conditionalPanel(ns = ns,
                         condition = "input.plot_type == 'Continuous'",
                         plotly::plotlyOutput(ns("plot"), width = "100%", height = "800px", inline = TRUE),
                         actionButton(ns("full_screen"),
                                      "Full screen")
        )
        
      )
    )
  )
}

plot <- function(id, EQWin, AquaCache) {
  
  moduleServer(id, function(input, output, session) {
    
    # hide some things right off the bat
    shinyjs::hide("full_screen")
    
    # Get the data to populate drop-downs. Runs every time this module is loaded.
    data <- reactiveValues(EQ_locs = DBI::dbGetQuery(EQWin, paste0("SELECT StnCode, StnDesc FROM eqstns;")),
                 EQ_loc_grps = DBI::dbGetQuery(EQWin, "SELECT groupname, groupdesc, groupitems FROM eqgroups WHERE dbtablename = 'eqstns'"),
                 EQ_params = DBI::dbGetQuery(EQWin, paste0("SELECT ParamId, ParamCode, ParamDesc, Units AS unit FROM eqparams;")),
                 EQ_param_grps = DBI::dbGetQuery(EQWin, "SELECT groupname, groupdesc, groupitems FROM eqgroups WHERE dbtablename = 'eqparams'"),
                 AC_locs = DBI::dbGetQuery(AquaCache, "SELECT loc.location_id, loc.name FROM locations loc INNER JOIN timeseries ts ON loc.location_id = ts.location_id WHERE ts.category = 'discrete'"),
                 AC_params = DBI::dbGetQuery(AquaCache, "SELECT DISTINCT p.param_code, p.param_name, p.unit_default AS unit FROM parameters p INNER JOIN timeseries ts ON p.param_code = ts.parameter WHERE ts.category = 'discrete'"))
    
    
    observeEvent(input$data_source, {
      if (input$data_source == "EQ") {
        out <<- setNames(data$EQ_params$ParamCode, paste0(data$EQ_params$ParamCode, " (", data$EQ_params$ParamDesc, ")"))
        updateSelectizeInput(session, "parameters", choices = setNames(data$EQ_params$ParamCode, paste0(data$EQ_params$ParamCode, " (", data$EQ_params$ParamDesc, ")")), server = TRUE)
        updateSelectizeInput(session, "parameter_groups", choices = data$EQ_param_grps$groupname, server = TRUE)
        updateSelectizeInput(session, "locations", choices = setNames(data$EQ_locs$StnCode, paste0(data$EQ_locs$StnCode, " (", data$EQ_locs$StnDesc, ")")), server = TRUE)
        updateSelectizeInput(session, "location_groups", choices = data$EQ_loc_grps$groupname, server = TRUE)
        shinyjs::show("locs_groups")
        shinyjs::show("params_groups")
        if (input$locs_groups == "Location Groups") {
          shinyjs::hide("locations")
          shinyjs::show("location_groups")
        } else {
          shinyjs::hide("location_groups")
          shinyjs::show("locations")
        }
        if (input$params_groups == "Parameter Groups") {
          shinyjs::hide("parameters")
          shinyjs::show("parameter_groups")
        } else {
          shinyjs::hide("parameter_groups")
          shinyjs::show("parameters")
        }
      } else { # AC selected
        updateSelectizeInput(session, "parameters", choices = data$AC_params$param_name, server = TRUE)
        updateSelectizeInput(session, "locations", choices = data$AC_locs$name, server = TRUE)
        shinyjs::hide("locs_groups")
        shinyjs::hide("params_groups")
        shinyjs::hide("location_groups")
        shinyjs::hide("parameter_groups")
        shinyjs::show("locations")
        shinyjs::show("parameters")
      }
    })
    
    # Toggle visibility of location and location group inputs
    observeEvent(input$locs_groups, {
      if (input$locs_groups == "Location Groups") {
        shinyjs::show("location_groups")
        shinyjs::hide("locations")
      } else {
        shinyjs::hide("location_groups")
        shinyjs::show("locations")
      }
    })
    observeEvent(input$params_groups, {
      if (input$params_groups == "Parameter Groups") {
        shinyjs::show("parameter_groups")
        shinyjs::hide("parameters")
      } else {
        shinyjs::hide("parameter_groups")
        shinyjs::show("parameters")
      }
    })
    
    # Create and render the plot ############################################################
    observeEvent(input$make_plot, {
      shinyjs::hide("full_screen")
      
      # Check if locations/location_group is not NULL, depending on the selection for input$locs_groups
      if (input$locs_groups == "Locations") {
        if (is.null(input$locations)) {
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
        if (is.null(input$parameters)) {
          showModal(modalDialog("Please select at least one parameter.", easyClose = TRUE))
          return()
        }
      } else {
        if (is.null(input$parameter_groups)) {
          showModal(modalDialog("Please select one parameter group.", easyClose = TRUE))
          return()
        }
      }
      
      # print(input$date_range)
      # print(input$locations)
      # print(input$location_groups)
      # print(input$parameters)
      # print(input$parameter_groups)
      print(input$log_scale)
      print(class(input$log_scale))
      # print(input$facet_on)
      # print(input$target_datetime)
      # print(input$colorblind)
      # print(input$lang)
      # print(input$data_source)
      tryCatch({
        plot <- plotDiscrete(start = input$date_range[1],
                             end = input$date_range[2],
                             locations = if (input$locs_groups == "Locations") input$locations else NULL,
                             locGrp = if (input$locs_groups == "Location Groups") input$location_groups else NULL,
                             parameters = if (input$params_groups == "Parameters") input$parameters else NULL,
                             paramGrp = if (input$params_groups == "Parameter Groups") input$parameter_groups else NULL,
                             log = input$log_scale,
                             facet_on = input$facet_on,
                             target_datetime = input$target_datetime,
                             colorblind = input$colorblind,
                             lang = input$lang,
                             dbSource = input$data_source)
        
        print("Plot created")
        output$plot <- plotly::renderPlotly(plot)
        
        shinyjs::show("full_screen")
      }, error = function(e) {
        showModal(modalDialog(paste0("An error occurred while creating the plot. Please check your inputs and try again.\n  \n  Error: ", e$message), easyClose = TRUE))
        return()
      })
    })
    
    observeEvent(input$full_screen, {
      shinyjs::runjs(paste0("toggleFullScreen('", session$ns("plot"), "');"))
    })
    
  }) # End of moduleServer
  

}
