discretePlotUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      # Toggle for data source
      radioButtons(ns("data_source"),
                   NULL,
                   choices = stats::setNames(c("AC", "EQ"), c("AquaCache", "EQWin")),
                   selected = "AC"),
      uiOutput(ns("EQWin_source_ui")),
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
                                      multiple = TRUE,
                                      options = list(maxItems = 1)), # This fixes a bug where the 'Placeholder' value remains after updating values,
                       
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
                                      multiple = TRUE,
                                      options = list(maxItems = 1)), # This fixes a bug where the 'Placeholder' value remains after updating values
                       # Selectize input for a standard to apply
                       div(
                         style = "display: flex; align-items: center;",
                         tags$label(
                           "Select a standard to apply (optional)", 
                           class = "control-label",
                           style = "margin-right: 5px;"
                         ),
                         span(
                           id = ns("standard_info"),
                           `data-toggle` = "tooltip",
                           `data-placement` = "right",
                           `data-trigger` = "click hover",
                           title = "Warning: this adds a lot of time for plot generation!!!",
                           icon("info-circle", style = "font-size: 100%; margin-left: 5px;")
                         )
                       ),
                       selectizeInput(ns("standard"),
                                      NULL,
                                      choices = "Placeholder",
                                      multiple = TRUE,
                                      options = list(maxItems = 1)) # This is to be able to use the default no selection upon initialization but only have one possible selection anyways.
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
      
      # Below inputs are not conditional on data source
      div(
        style = "display: flex; align-items: center;",
        tags$label(
          "Facet on", 
          class = "control-label",
          style = "margin-right: 5px;"
        ),
        span(
          id = ns("facet_info"),
          `data-toggle` = "tooltip",
          `data-placement` = "right",
          `data-trigger` = "click hover",
          title = "Multiple plots are built where each plot represents a different location or parameter, with the other variable represented as different traces.",
          icon("info-circle", style = "font-size: 100%; margin-left: 5px;")
        )
      ),
      radioButtons(ns("facet_on"),
                   NULL,
                   choices = stats::setNames(c("locs", "params"), c("Locations", "Parameters")),
                   selected = "locs"),
      
      div(
        checkboxInput(ns("log_scale"),
                      NULL),
        style = "display: flex; align-items: center;",
        tags$label(
          "Use log scale", 
          class = "control-label",
          style = "margin-right: 5px;"
        ),
        span(
          id = ns("log_info"),
          `data-toggle` = "tooltip",
          `data-placement` = "right",
          `data-trigger` = "click hover",
          title = "Warning: negative values will be removed for log transformation.",
          icon("info-circle", style = "font-size: 100%; margin-left: 5px;")
        )
      ),
      div(
        checkboxInput(ns("shareX"),
                      NULL,
                      value = TRUE),
        style = "display: flex; align-items: center;",
        tags$label(
          "Share X axis between subplots (dates are aligned)", 
          class = "control-label",
          style = "margin-right: 5px;"
        )
      ),
      div(
        checkboxInput(ns("shareY"),
                      NULL,
                      value = FALSE),
        style = "display: flex; align-items: center;",
        tags$label(
          "Share Y axis between subplots (values are aligned)", 
          class = "control-label",
          style = "margin-right: 5px;"
        )
      ),
      div(
        selectizeInput(ns("loc_code"),
                       label = "Choose how to display location codes/names",
                       choices = stats::setNames(
                         c("name", "code", "nameCode", "codeName"),
                         c("Name", "Code", "Name (Code)", "Code (Name)")
                       ),
                       selected = "Name"),
        style = "display: flex; align-items: center;"
      ),
      
      div(
        checkboxInput(ns("target_datetime"),
                      label = NULL),
        style = "display: flex; align-items: center;",
        tags$label(
          "Use target instead of actual datetime", 
          class = "control-label",
          style = "margin-right: 5px;"
        ),
        span(
          id = ns("target_datetime_info"),
          `data-toggle` = "tooltip",
          `data-placement` = "right",
          `data-trigger` = "click hover",
          title = "Some measurements have a 'fake' datetime to line up repeated measurements with a certain date. An example are snow survey measurements: these can be taken with a few days of the 1st of the month but visualize nicely when lined up with the 1st.",
          icon("info-circle", style = "font-size: 100%; margin-left: 5px;")
        )
      ),
      
      div(
        actionButton(ns("extra_aes"),
                     "Modify plot aesthetics",
                     title = "Modify plot aesthetics such as language, color palette, point/line size, text size.",
                     style = "display: block; width: 100%; margin-bottom: 10px;"), # Ensure block display and full width
        actionButton(ns("make_plot"),
                     "Create Plot",
                     style = "display: block; width: 100%;") # Ensure block display and full width
      )
      
    ),
    
    mainPanel(
      plotly::plotlyOutput(ns("plot"), width = "100%", height = "800px", inline = TRUE),
      uiOutput(ns("full_screen_ui"))
    )
  )
}

discretePlotServer <- function(id, mdb_files, AquaCache, language) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns  # Used to create UI elements within the server code
    
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
    
    # Get the data to populate drop-downs. Runs every time this module is loaded.
    data <- reactiveValues()
    
    data$AC_locs <- DBI::dbGetQuery(AquaCache, "SELECT loc.location_id, loc.name FROM locations loc INNER JOIN timeseries ts ON loc.location_id = ts.location_id ORDER BY loc.name ASC")
    data$AC_params <- DBI::dbGetQuery(AquaCache, "SELECT DISTINCT p.parameter_id, p.param_name, p.unit_default AS unit FROM parameters p INNER JOIN timeseries ts ON p.parameter_id = ts.parameter_id ORDER BY p.param_name ASC")
    
    observeEvent(input$EQWin_source, {
      EQWin <- AccessConnect(input$EQWin_source, silent = TRUE)
      EQ_locs <- DBI::dbGetQuery(EQWin, paste0("SELECT StnCode, StnDesc FROM eqstns ORDER BY StnCode;"))
      EQ_loc_grps <- DBI::dbGetQuery(EQWin, "SELECT groupname, groupdesc, groupitems FROM eqgroups WHERE dbtablename = 'eqstns' ORDER BY groupname;")
      EQ_params <- DBI::dbGetQuery(EQWin, paste0("SELECT ParamId, ParamCode, ParamDesc, Units AS unit FROM eqparams ORDER BY ParamDesc;"))
      EQ_param_grps <- DBI::dbGetQuery(EQWin, "SELECT groupname, groupdesc, groupitems FROM eqgroups WHERE dbtablename = 'eqparams' ORDER BY groupname;")
      EQ_stds <- DBI::dbGetQuery(EQWin, "SELECT StdName, StdCode FROM eqstds ORDER BY StdName;")
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
      
      data$EQ_locs <- EQ_locs
      data$EQ_loc_grps <- EQ_loc_grps
      data$EQ_params <- EQ_params
      data$EQ_param_grps <- EQ_param_grps
      data$EQ_stds <- EQ_stds
    }, ignoreInit = TRUE, ignoreNULL = TRUE)


    observe({
      if (is.null(mdb_files)) {
        shinyjs::hide("data_source")
        shinyjs::hide("EQWin_source")
        updateRadioButtons(session, "data_source", selected = "AC")
      }
    })
    
    
    
    observe({
      req(input$data_source, data)
      if (input$data_source == "EQ") {
        req(data$EQ_params)
        updateSelectizeInput(session, "parameters_EQ", choices = stats::setNames(data$EQ_params$ParamCode, paste0(data$EQ_params$ParamCode, " (", data$EQ_params$ParamDesc, ")")), server = TRUE)
        updateSelectizeInput(session, "parameter_groups", choices = data$EQ_param_grps$groupname, server = TRUE)
        updateSelectizeInput(session,"locations_EQ", choices = stats::setNames(data$EQ_locs$StnCode, paste0(data$EQ_locs$StnCode, " (", data$EQ_locs$StnDesc, ")")), server = TRUE)
        updateSelectizeInput(session, "location_groups", choices = data$EQ_loc_grps$groupname, server = TRUE)
        updateSelectizeInput(session, "standard", choices = stats::setNames(data$EQ_stds$StdCode, data$EQ_stds$StdName), server = TRUE, selected = "")
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
    

    # Modal dialog for extra aesthetics  ####
    
    # Create a list with default aesthetic values
    plot_aes <- reactiveValues(lang = "en",
                               showgridx = FALSE,
                               showgridy = FALSE,
                               colorblind = FALSE,
                               nrows = NULL,
                               point_scale = 1,
                               guideline_scale = 1,
                               axis_scale = 1,
                               legend_scale = 1)
    
    
    observeEvent(input$extra_aes, {
      showModal(modalDialog(
        title = "Modify plot aesthetics",
        tags$div(
          tags$h5("Language"),
          radioButtons(ns("lang"),
                       NULL,
                       choices = stats::setNames(c("en", "fr"), c("English", "French")),
                       selected = plot_aes$lang),
          checkboxInput(ns("showgridx"),
                        "Show x-axis gridlines",
                        value = plot_aes$showgridx),
          checkboxInput(ns("showgridy"),
                        "Show y-axis gridlines",
                        value = plot_aes$showgridy),
          numericInput(ns("nrows"),
                       "Number of rows (leave blank for auto)",
                       value = plot_aes$nrows,
                       min = 1),
          checkboxInput(ns("colorblind"),
                        "Colorblind friendly",
                        value = plot_aes$colorblind),
          tags$hr(),
          sliderInput(ns("point_scale"),
                      "Point scale factor",
                      min = 0.2,
                      max = 3,
                      value = plot_aes$point_scale,
                      step = 0.1),
          sliderInput(ns("guideline_scale"),
                      "Guideline/standard scale factor",
                      min = 0.2,
                      max = 3,
                      value = plot_aes$guideline_scale,
                      step = 0.1),
          sliderInput(ns("axis_scale"),
                      "Axes scale factor (text and values)",
                      min = 0.2,
                      max = 3,
                      value = plot_aes$axis_scale,
                      step = 0.1),
          sliderInput(ns("legend_scale"),
                      "Legend text scale factor",
                      min = 0.2,
                      max = 3,
                      value = plot_aes$legend_scale,
                      step = 0.1)
        ),
        easyClose = FALSE,
        footer = tagList(
          actionButton(ns("aes_apply"), "Apply"),
          actionButton(ns("aes_cancel"), "Cancel")
        )
      ))
    })
    
    observeEvent(input$aes_apply, {
      plot_aes$lang <- input$lang
      plot_aes$colorblind <- input$colorblind
      plot_aes$showgridx <- input$showgridx
      plot_aes$showgridy <- input$showgridy
      plot_aes$nrows <- if (input$nrows > 0) input$nrows else NULL
      plot_aes$point_scale <- input$point_scale
      plot_aes$guideline_scale <- input$guideline_scale
      plot_aes$axis_scale <- input$axis_scale
      plot_aes$legend_scale <- input$legend_scale
      removeModal()
    })
    
    observeEvent(input$aes_cancel, {
      removeModal()
    })
    
    # Create and render the plot ############################################################
    first_plot <- reactiveVal(TRUE)
    first_plot_with_standards <- reactiveVal(TRUE)
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
        withProgress(message = translations[id == "generating_working", get(language$language)][[1]], value = 0, {
          print(plot_aes$nrows)
          incProgress(0.5)
          if (input$data_source == "EQ") {
            plot <- plotDiscrete(start = input$date_range[1],
                                 end = input$date_range[2],
                                 locations = if (input$locs_groups == "Locations") input$locations_EQ else NULL,
                                 locGrp = if (input$locs_groups == "Location Groups") input$location_groups else NULL,
                                 parameters = if (input$params_groups == "Parameters") input$parameters_EQ else NULL,
                                 paramGrp = if (input$params_groups == "Parameter Groups") input$parameter_groups else NULL,
                                 standard = if (length(input$standard) == 0) NULL else input$standard,
                                 log = input$log_scale,
                                 facet_on = input$facet_on,
                                 loc_code = input$loc_code,
                                 shareX = input$shareX,
                                 shareY = input$shareY,
                                 rows = if (is.null(plot_aes$nrows)) "auto" else plot_aes$nrows,
                                 target_datetime = input$target_datetime,
                                 colorblind = plot_aes$colorblind,
                                 lang = plot_aes$lang,
                                 point_scale = plot_aes$point_scale,
                                 guideline_scale = plot_aes$guideline_scale,
                                 axis_scale = plot_aes$axis_scale,
                                 legend_scale = plot_aes$legend_scale,
                                 gridx = plot_aes$showgridx,
                                 gridy = plot_aes$showgridy,
                                 dbSource = input$data_source,
                                 dbPath = input$EQWin_source,
                                 dbCon = NULL)
          } else if (input$data_source == "AC") {
            plot <- plotDiscrete(start = input$date_range[1],
                                 end = input$date_range[2],
                                 locations = input$locations_AC,
                                 locGrp = NULL,
                                 parameters = input$parameters_AC,
                                 paramGrp = NULL,
                                 log = input$log_scale,
                                 facet_on = input$facet_on,
                                 loc_code = input$loc_code,
                                 shareX = input$shareX,
                                 shareY = input$shareY,
                                 rows = if (is.null(plot_aes$nrows)) "auto" else plot_aes$nrows,
                                 target_datetime = input$target_datetime,
                                 colorblind = plot_aes$colorblind,
                                 lang = plot_aes$lang,
                                 point_scale = plot_aes$point_scale,
                                 guideline_scale = plot_aes$guideline_scale,
                                 axis_scale = plot_aes$axis_scale,
                                 legend_scale = plot_aes$legend_scale,
                                 gridx = plot_aes$showgridx,
                                 gridy = plot_aes$showgridy,
                                 dbSource = input$data_source,
                                 dbCon = AquaCache)
          }
          
          
          output$plot <- plotly::renderPlotly(plot)
          
          incProgress(1)
        }) # End withProgress\
        
        output$full_screen_ui <- renderUI({
          actionButton(ns("full_screen"), "Full screen")
        })
        # If this is the first plot generated by the user in this session show them a modal
        
        if (first_plot()) {
          if (first_plot_with_standards()) {
            showModal(
              modalDialog(
                HTML("This plot is interactive; you can zoom, pan, etc. either by using the buttons at the top left or by clicking and dragging with your mouse. To select only a single timeseries, double click on its legend entry; double click again to reselect all. Toggle timeseries one at a time by clicking on their legend entries.<br><br>
                                  Values above/below the detection limit are represented with stars<br><br>
                                  Standard/guideline values are represented with lines."),
                easyClose = TRUE)
            )
            first_plot_with_standards(FALSE)
          } else {
            showModal(
              modalDialog(
                HTML("This plot is interactive; you can zoom, pan, etc. either by using the buttons at the top left or by clicking and dragging with your mouse. To select only a single timeseries, double click on its legend entry; double click again to reselect all. Toggle timeseries one at a time by clicking on their legend entries.<br><br>
                                  Values above/below the detection limit are represented with stars"),
                easyClose = TRUE)
            )
          }
          first_plot(FALSE)
        }
        
        if (first_plot_with_standards()) {
          showModal(
            modalDialog(
            HTML("Values above/below the detection limit are represented with stars<br><br>Standard/guideline values are represented with lines."),
            easyClose = TRUE))
          first_plot_with_standards(FALSE)
        }
        
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
