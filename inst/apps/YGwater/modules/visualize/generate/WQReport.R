WQReportUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    # Custom CSS below is for consistency with the sidebarPanel look elsewhere in the app.
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/background_style.css")),
      div(class = "custom-panel container",
          
                # Toggle for data source
          # ! Data source 'AC' is not implemented in the report generation function yet; the code is here but a modal dialogue will appear if AC is selected and the user will get shunted back to EQ.
      radioButtons(ns("data_source"),
                   NULL,
                   choices = stats::setNames(c("AC", "EQ"), c("AquaCache", "EQWin")),
                   selected = "EQ"),
      dateInput(ns("date"), "Report Date", value = Sys.Date() - 30,
                width = "100%"),
      numericInput(ns("date_approx"), "Look for data within this many days of the report date", value = 1,
                   width = "100%"),
      conditionalPanel(ns = ns,
                       condition = "input.data_source == 'EQ'",
                       # Toggle button for locations or location groups (only show if data source  == EQWin)
                       radioButtons(ns("locs_groups"),
                                    NULL,
                                    choices = c("Locations", "Location Groups"),
                                    selected = "Locations",
                                    width = "100%"),
                       # Selectize input for locations, populated once connection is established
                       selectizeInput(ns("locations_EQ"),
                                      "Select locations",
                                      choices = "Placeholder",
                                      multiple = TRUE,
                                      width = "100%"),
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
                                    selected = "Parameters",
                                    width = "100%"),
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
                       # Selectize input for standards, populated once connection is established
                       selectizeInput(ns("stds"),
                                      "Select one or more standards to apply",
                                      choices = "Placeholder",
                                      multiple = TRUE,
                                      width = "100%"),
                       # TRUE/FALSE selection for station-specific standards
                       checkboxInput(ns("stnStds"), "Apply station-specific standards?", value = FALSE)
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
      
      downloadButton(ns("create_report"), "Create Report")
      )
    )

  
}

WQReportServer <- function(id, EQWin, AquaCache) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns  # Used to create UI elements in the server code
    
    # Get the data to populate drop-downs. Runs every time this module is loaded.
    data <- reactiveValues()
    observe({
      EQ_locs <- DBI::dbGetQuery(EQWin, paste0("SELECT StnCode, StnDesc FROM eqstns;"))
      EQ_loc_grps <- DBI::dbGetQuery(EQWin, "SELECT groupname, groupdesc, groupitems FROM eqgroups WHERE dbtablename = 'eqstns'")
      EQ_params <- DBI::dbGetQuery(EQWin, paste0("SELECT ParamId, ParamCode, ParamDesc, Units AS unit FROM eqparams;"))
      EQ_param_grps <- DBI::dbGetQuery(EQWin, "SELECT groupname, groupdesc, groupitems FROM eqgroups WHERE dbtablename = 'eqparams'")
      EQ_stds <- DBI::dbGetQuery(EQWin, "SELECT StdName, StdCode FROM eqstds")
      # AC_locs <- DBI::dbGetQuery(AquaCache, "SELECT loc.location_id, loc.name FROM locations loc INNER JOIN timeseries ts ON loc.location_id = ts.location_id WHERE ts.category = 'discrete'")
      # AC_params <- DBI::dbGetQuery(AquaCache, "SELECT DISTINCT p.parameter_id, p.param_name, p.unit_default AS unit FROM parameters p INNER JOIN timeseries ts ON p.parameter_id = ts.parameter_id WHERE ts.category = 'discrete'")
      
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
      data$EQ_stds <- EQ_stds
      # data$AC_locs <- AC_locs
      # data$AC_params <- AC_params
    })
    
    observeEvent(input$data_source, {
      # !Since the report generation function is not yet compatible with AC, a modal is shown to the user and the data source is reset to EQ.
      if (input$data_source == "EQ") {
        updateSelectizeInput(session, "parameters_EQ", choices = stats::setNames(data$EQ_params$ParamCode, paste0(data$EQ_params$ParamCode, " (", data$EQ_params$ParamDesc, ")")), server = TRUE)
        updateSelectizeInput(session, "parameter_groups", choices = data$EQ_param_grps$groupname, server = TRUE)
        updateSelectizeInput(session,"locations_EQ", choices = stats::setNames(data$EQ_locs$StnCode, paste0(data$EQ_locs$StnCode, " (", data$EQ_locs$StnDesc, ")")), server = TRUE)
        updateSelectizeInput(session, "location_groups", choices = data$EQ_loc_grps$groupname, server = TRUE)
        updateSelectizeInput(session, "stds", choices = stats::setNames(data$EQ_stds$StdCode, data$EQ_stds$StdName), server = TRUE)
      } else if (input$data_source == "AC") { # AC selected
        showModal(modalDialog(
          title = "Data Source Not Implemented",
          "This report generation function is not yet compatible with AquaCache.",
          easyClose = TRUE
        ))
        updateRadioButtons(session, "data_source", selected = "EQ")
        # updateSelectizeInput(session, "parameters_AC", choices = data$AC_params$param_name, server = TRUE)
        # updateSelectizeInput(session, "locations_AC", choices = data$AC_locs$name, server = TRUE)
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
    
    output$create_report <- downloadHandler(
      filename = function() {
        paste0("EQWin Report for ", input$date, " Issued ", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        withProgress(message = "Generating report...", value = 0, {

          incProgress(0.5)
          
          EQWinReport(date = input$date,
                      date_approx = as.numeric(input$date_approx),
                      stations = if (input$locs_groups == "Locations") input$locations_EQ else NULL,
                      stnGrp = if (input$locs_groups == "Location Groups") input$location_groups else NULL,
                      parameters = if (input$params_groups == "Parameters") input$parameters_EQ else NULL,
                      paramGrp = if (input$params_groups == "Parameter Groups") input$parameter_groups else NULL,
                      stds = input$stds,
                      stnStds = input$stnStds,
                      shiny_file_path = file,
                      con = EQWin)
          
          incProgress(1)
        }) # End withProgress\
      } # End content
    ) # End downloadHandler
    
  }) # End moduleServer
} # End WQReportServer


