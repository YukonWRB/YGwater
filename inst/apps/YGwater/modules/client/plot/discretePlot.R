discretePlotUI <- function(id) {
  ns <- NS(id)
  
  page_sidebar(
    sidebar = sidebar(
      title = NULL,
      width = 350,
      bg = config$sidebar_bg,
      open = list(mobile = "always-above"),
      uiOutput(ns("sidebar"))
    ),
    uiOutput(ns("main"))
  )
}

discretePlot <- function(id, mdb_files, language, windowDims, inputs) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns  # Used to create UI elements within the server code
    
    EQWin_selector <- reactiveVal(FALSE)  # flags whether the EQWin source UI is already rendered
    
    # Get the data to populate drop-downs. Runs every time this module is loaded.
    moduleData <- reactiveValues()
    
    moduleData$AC_locs <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT DISTINCT loc.location_id, loc.name, loc.name_fr FROM locations AS loc INNER JOIN samples ON loc.location_id = samples.location_id ORDER BY loc.name ASC")
    moduleData$AC_params <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT DISTINCT p.parameter_id, p.param_name, p.unit_default AS unit FROM parameters p INNER JOIN results AS r ON p.parameter_id = r.parameter_id ORDER BY p.param_name ASC")
    moduleData$AC_loc_params <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT DISTINCT s.location_id, r.parameter_id FROM samples s INNER JOIN results r ON s.sample_id = r.sample_id")
    
    output$sidebar <- renderUI({
      tagList(
        # Toggle for data source
        if (is.null(mdb_files)) {
          shinyjs::hidden(radioButtons(ns("data_source"),
                                       NULL,
                                       choices = stats::setNames(c("AC", "EQ"), c("AquaCache", "EQWin")),
                                       selected = "AC"))
        } else {
          radioButtons(ns("data_source"),
                       NULL,
                       choices = stats::setNames(c("AC", "EQ"), c("AquaCache", "EQWin")),
                       selected = "AC")
        },
        uiOutput(ns("EQWin_source_ui")),
        # start and end datetime
        dateRangeInput(ns("date_range"),
                       tr("date_range_lab", language$language),
                       start = Sys.Date() - 30,
                       end = Sys.Date(),
                       max = Sys.Date() + 1,
                       format = "yyyy-mm-dd"),
        conditionalPanel(ns = ns,
                         condition = "input.data_source == 'EQ'",
                         # Toggle button for locations or location groups (only show if data source == EQWin)
                         radioButtons(ns("locs_groups"),
                                      NULL,
                                      choices = setNames(c("locations", "loc_groups"),
                                                         c(tr("locs", language$language), tr("loc_groups", language$language))),
                                      selected = "locations"),
                         # Selectize input for locations, populated once connection is established
                         selectizeInput(ns("locations_EQ"),
                                        tr("select_locs", language$language),
                                        choices = NULL,
                                        multiple = TRUE),
                         # Selectize input for location groups, populated once connection is established. only shown if data source is EQWin
                         selectizeInput(ns("location_groups"),
                                        tr("select_loc_group", language$language),
                                        choices = NULL,
                                        multiple = TRUE,
                                        options = list(maxItems = 1)), # This fixes a bug where the 'Placeholder' value remains after updating values
                         
                         # Toggle button for parameters or parameter groups (only show if data source == EQWin)
                         radioButtons(ns("params_groups"),
                                      NULL,
                                      choices = setNames(c("parameters", "param_groups"),
                                                         c(tr("parameters", language$language), tr("param_groups", language$language))),
                                      selected = "parameters"),
                         # Selectize input for parameters, populated once connection is established
                         selectizeInput(ns("parameters_EQ"),
                                        tr("select_params", language$language),
                                        choices = NULL,
                                        multiple = TRUE),
                         # Selectize input for parameter groups, populated once connection is established. only shown if data source is EQWin
                         selectizeInput(ns("parameter_groups"),
                                        tr("select_param_group", language$language),
                                        choices = NULL,
                                        multiple = TRUE,
                                        options = list(maxItems = 1)), # This fixes a bug where the 'Placeholder' value remains after updating values
                         # Selectize input for a standard to apply
                         div(
                           style = "display: flex; align-items: center;",
                           tags$label(
                             tr("select_standard_opt", language$language),
                             class = "form-label",
                             style = "margin-right: 5px;"
                           ),
                           span(
                             id = ns("standard_info"),
                             `data-bs-toggle` = "tooltip",
                             `data-bs-placement` = "right",
                             `data-bs-trigger` = "click hover",
                             title = tr("standard_warning", language$language),
                             icon("info-circle", style = "font-size: 100%; margin-left: 5px;")
                           )
                         ),
                         selectizeInput(ns("standard"),
                                        NULL,
                                        choices = NULL,
                                        multiple = TRUE,
                                        options = list(maxItems = 1)) # This is to be able to use the default no selection upon initialization but only have one possible selection anyways.
        ),
        
        conditionalPanel(ns = ns,
                         condition = "input.data_source == 'AC'",
                         # Selectize input for locations, populated once connection is established
                         selectizeInput(ns("locations_AC"),
                                        tr("loc(s)", language$language),
                                        choices = NULL,
                                        multiple = TRUE),
                         # Selectize input for parameters, populated once connection is established
                         selectizeInput(ns("parameters_AC"),
                                        tr("parameter(s)", language$language),
                                        choices = NULL,
                                        multiple = TRUE)
        ),
        radioButtons(ns("facet_on"),
                     label = tooltip(
                       trigger = list(
                         tr("facet_on", language$language),
                         bsicons::bs_icon("info-circle-fill")
                       ),
                       tr("facet_on_tooltip", language$language)
                     ),
                     choices = stats::setNames(c("locs", "params"), c(tr("locs", language$language), tr("parameters", language$language))),
                     selected = "locs"),
        checkboxInput(ns("log_scale"),
                      label = tooltip(
                        trigger = list(
                          tr("use_log_scale", language$language),
                          bsicons::bs_icon("info-circle-fill")
                        ),
                        tr("log_scale_warning", language$language),
                      )),
        checkboxInput(ns("shareX"),
                      label = tooltip(
                        trigger = list(
                          tr("share_x_axis", language$language),
                          bsicons::bs_icon("info-circle-fill")
                        ),
                        tr("share_x_axis_tooltip", language$language)
                      ),
                      value = TRUE),
        checkboxInput(ns("shareY"),
                      label = tooltip(
                        trigger = list(
                          tr("share_y_axis", language$language),
                          bsicons::bs_icon("info-circle-fill")
                        ),
                        tr("share_y_axis_tooltip", language$language)
                      ),
                      value = FALSE),
        div(
          selectizeInput(ns("loc_code"),
                         label = tr("loc_code", language$language),
                         choices = stats::setNames(
                           c("name", "code", "nameCode", "codeName"),
                           c(tr("loc_code_name", language$language), tr("loc_code_code", language$language), tr("loc_code_nameCode", language$language), tr("loc_code_codeName", language$language))
                         ),
                         selected = "name"),
          style = "display: flex; align-items: center;"
        ),
        
        # div(
        checkboxInput(ns("target_datetime"),
                      label = tooltip(
                        trigger = list(
                          tr("target_datetime", language$language),
                          bsicons::bs_icon("info-circle-fill")
                        ),
                        tr("target_datetime_tooltip", language$language)
                      )
        ),
        div(
          actionButton(ns("extra_aes"),
                       tr("modify_plot_aes", language$language),
                       style = "display: block; width: 100%; margin-bottom: 10px;"), # Ensure block display and full width
          input_task_button(ns("make_plot"),
                            label = tr("create_plot", language$language),
                            style = "display: block; width: 100%;", # Ensure block display and full width
                            class = "btn btn-primary")
        )
      ) # End of tagList
    })  %>% # End of renderUI for sidebar
      bindEvent(language$language)  #TODO: bindEvent should also be on moduleData, but moduleData is not being used in the creation of lists yet
    
    
    output$main <- renderUI({
      tagList(
        plotly::plotlyOutput(ns("plot"), width = "100%", height = "800px", inline = TRUE),
        page_fluid(
          div(class = "d-inline-block", actionButton(ns("full_screen"), tr("full_screen", language$language)), style = "display: none;"),
          div(class = "d-inline-block", downloadButton(ns("download_data"), tr("dl_data", language$language)), style = "display: none;")
        )
      ) # End of tagList
    }) %>% # End renderUI
      bindEvent(language$language)
    
    
    observeEvent(input$data_source, {
      if (input$data_source == "AC") {
        shinyjs::hide("EQWin_source_ui")
      } else {
        if (!EQWin_selector()) { # Only renders the ui element once
          output$EQWin_source_ui <- renderUI({
            selectizeInput(ns("EQWin_source"), tr("EQWin_db", language$language), choices = stats::setNames(mdb_files, basename(mdb_files)), selected = mdb_files[1])
          })
          EQWin_selector(TRUE)
        }
        shinyjs::show("EQWin_source_ui")
      }
    }, ignoreInit = TRUE)
    
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
      
      moduleData$EQ_locs <- EQ_locs
      moduleData$EQ_loc_grps <- EQ_loc_grps
      moduleData$EQ_params <- EQ_params
      moduleData$EQ_param_grps <- EQ_param_grps
      moduleData$EQ_stds <- EQ_stds
      
      # Update the selectize inputs
      updateSelectizeInput(session, "parameters_EQ", choices = stats::setNames(moduleData$EQ_params$ParamCode, paste0(moduleData$EQ_params$ParamCode, " (", moduleData$EQ_params$ParamDesc, ")")), server = TRUE, selected = character(0))
      updateSelectizeInput(session, "parameter_groups", choices = moduleData$EQ_param_grps$groupname, server = TRUE, selected = character(0))
      updateSelectizeInput(session,"locations_EQ", choices = stats::setNames(moduleData$EQ_locs$StnCode, paste0(moduleData$EQ_locs$StnCode, " (", moduleData$EQ_locs$StnDesc, ")")), server = TRUE, selected = character(0))
      updateSelectizeInput(session, "location_groups", choices = moduleData$EQ_loc_grps$groupname, server = TRUE, selected = character(0))
      updateSelectizeInput(session, "standard", choices = stats::setNames(moduleData$EQ_stds$StdCode, moduleData$EQ_stds$StdName), server = TRUE, selected = character(0))
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    
    # Helper function to update the list of available parameters based on selected locations
    update_parameters <- function() {
      if (is.null(input$locations_AC) || length(input$locations_AC) == 0) {
        params <- moduleData$AC_params
      } else {
        param_ids <- unique(moduleData$AC_loc_params$parameter_id[moduleData$AC_loc_params$location_id %in% input$locations_AC])
        params <- moduleData$AC_params[moduleData$AC_params$parameter_id %in% param_ids, , drop = FALSE]
      }
      selected <- input$parameters_AC[input$parameters_AC %in% params$parameter_id]
      updateSelectizeInput(
        session,
        "parameters_AC",
        choices = stats::setNames(params$parameter_id, params$param_name),
        selected = selected,
        server = TRUE
      )
    }
    
    observeEvent(input$data_source, {
      req(moduleData)
      if (input$data_source == "EQ") {
        # These updates are performed in the observeEvent for input$EQWin_source
      } else if (input$data_source == "AC") { # AC selected
        updateSelectizeInput(session, "locations_AC", choices = stats::setNames(moduleData$AC_locs$location_id, moduleData$AC_locs$name), selected = if (!is.null(inputs$location_id)) inputs$location_id else NULL, server = TRUE)
        update_parameters()
      }
    })
    
    # Update the list of available parameters in response to location selection
    observeEvent(input$locations_AC, {
      update_parameters()
    }, ignoreNULL = FALSE)
    
    # Toggle visibility of location and location group inputs
    observeEvent(input$locs_groups, {
      if (input$locs_groups == "loc_groups") {
        shinyjs::show("location_groups")
        shinyjs::hide("locations_EQ")
      } else {
        shinyjs::hide("location_groups")
        shinyjs::show("locations_EQ")
      }
    })
    observeEvent(input$params_groups, {
      if (input$params_groups == "param_groups") {
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
        title = tr("modify_plot_aes", language$language),
        tags$div(
          tags$h5(tr("language", language$language)),
          radioButtons(ns("lang"),
                       NULL,
                       choices = stats::setNames(c("en", "fr"), c(tr("english", language$language), tr("francais", language$language))),
                       selected = plot_aes$lang),
          checkboxInput(ns("showgridx"),
                        tr("show_x_grid", language$language),
                        value = plot_aes$showgridx),
          checkboxInput(ns("showgridy"),
                        tr("show_y_grid", language$language),
                        value = plot_aes$showgridy),
          numericInput(ns("nrows"),
                       tr("num_rows", language$language),
                       value = plot_aes$nrows,
                       min = 1),
          checkboxInput(ns("colorblind"),
                        tr("colorblind_friend", language$language),
                        value = plot_aes$colorblind),
          tags$hr(),
          sliderInput(ns("point_scale"),
                      tr("point_scale", language$language),
                      min = 0.2,
                      max = 3,
                      value = plot_aes$point_scale,
                      step = 0.1),
          sliderInput(ns("guideline_scale"),
                      tr("guideline_scale", language$language),
                      min = 0.2,
                      max = 3,
                      value = plot_aes$guideline_scale,
                      step = 0.1),
          sliderInput(ns("axis_scale"),
                      tr("axis_scale", language$language),
                      min = 0.2,
                      max = 3,
                      value = plot_aes$axis_scale,
                      step = 0.1),
          sliderInput(ns("legend_scale"),
                      tr("legend_scale", language$language),
                      min = 0.2,
                      max = 3,
                      value = plot_aes$legend_scale,
                      step = 0.1)
        ),
        easyClose = FALSE,
        footer = tagList(
          actionButton(ns("aes_apply"), tr("apply", language$language)),
          actionButton(ns("cancel"), tr("cancel", language$language))
        )
      ))
    })
    
    observeEvent(input$aes_apply, {
      plot_aes$lang <- input$lang
      plot_aes$colorblind <- input$colorblind
      plot_aes$showgridx <- input$showgridx
      plot_aes$showgridy <- input$showgridy
      if (!is.na(input$nrows)) {
        plot_aes$nrows <- if (input$nrows > 0) input$nrows else NULL
      }
      plot_aes$point_scale <- input$point_scale
      plot_aes$guideline_scale <- input$guideline_scale
      plot_aes$axis_scale <- input$axis_scale
      plot_aes$legend_scale <- input$legend_scale
      removeModal()
    })
    
    observeEvent(input$cancel, {
      removeModal()
    })
    
    # Create and render the plot ############################################################
    ## ExtendedTask for plot generation ######################################################
    plot_output_discrete <- ExtendedTask$new(
      function(start, end, locations, locGrp, parameters, paramGrp, standard, log, facet_on, loc_code, shareX, shareY, rows, target_datetime, colorblind, lang, point_scale, guideline_scale, axis_scale, legend_scale, legend_position, gridx, gridy, dbSource, dbPath, config) {
        promises::future_promise({
          
          tryCatch({
            if (is.null(dbPath)) {
              con <- AquaConnect(name = config$dbName, 
                                 host = config$dbHost,
                                 port = config$dbPort,
                                 username = config$dbUser,
                                 password = config$dbPass,
                                 silent = TRUE)
              on.exit(DBI::dbDisconnect(con))
            } else {
              con = NULL
            }
            
            plot <- plotDiscrete(
              start = start,
              end = end,
              locations = locations,
              locGrp = locGrp,
              parameters = parameters,
              paramGrp = paramGrp,
              standard = standard,
              log = log,
              facet_on = facet_on,
              loc_code = loc_code,
              shareX = shareX,
              shareY = shareY,
              rows = rows,
              target_datetime = target_datetime,
              colorblind = colorblind,
              lang = lang,
              point_scale = point_scale,
              guideline_scale = guideline_scale,
              axis_scale = axis_scale,
              legend_scale = legend_scale,
              legend_position = legend_position,
              gridx = gridx,
              gridy = gridy,
              dbSource = dbSource,
              dbPath = dbPath,
              dbCon = con,
              data = TRUE
            )
            return(plot)
          }, error = function(e) {
            return(e$message)
          }) # End of tryCatch
        }) # End of future_promise
        
      }
    ) |> bind_task_button("make_plot")
    # --- End ExtendedTask -------------------------------------------------------------------
    
    
    observeEvent(input$make_plot, {
      shinyjs::hide("full_screen")
      shinyjs::hide("download_data")
      
      # Validate required inputs based on data source
      if (input$data_source == "EQ") {
        if (input$locs_groups == "locations") {
          if (is.null(input$locations_EQ)) {
            showModal(modalDialog(tr("pl_select_loc", language$language), 
                                  footer = tagList(
                                    actionButton(ns("cancel"), tr("cancel", language$language))
                                  ),
                                  easyClose = TRUE))
            return()
          }
        } else {
          if (is.null(input$location_groups)) {
            showModal(modalDialog(tr("select_loc_group_msg", language$language),
                                  footer = tagList(
                                    actionButton(ns("cancel"), tr("cancel", language$language))
                                  ),
                                  easyClose = TRUE))
            return()
          }
        }
        # Same treatment for parameters/parameter_groups
        if (input$params_groups == "parameters") {
          if (is.null(input$parameters_EQ)) {
            showModal(modalDialog(tr("pl_select_param", language$language),
                                  footer = tagList(
                                    actionButton(ns("cancel"), tr("cancel", language$language))
                                  ),
                                  easyClose = TRUE))
            return()
          }
        } else {
          if (is.null(input$parameter_groups)) {
            showModal(modalDialog(tr("select_param_group_msg", language$language),
                                  footer = tagList(
                                    actionButton(ns("cancel"), tr("cancel", language$language))
                                  ),
                                  easyClose = TRUE))
            return()
          }
        }
      } else if (input$data_source == "AC") {
        if (is.null(input$locations_AC)) {
          showModal(modalDialog(tr("pl_select_loc", language$language),
                                footer = tagList(
                                  actionButton(ns("cancel"), tr("cancel", language$language))
                                ),
                                easyClose = TRUE))
          return()
        }
        if (is.null(input$parameters_AC)) {
          showModal(modalDialog(tr("pl_select_param", language$language), 
                                footer = tagList(
                                  actionButton(ns("cancel"), tr("cancel", language$language))
                                ),
                                easyClose = TRUE))
          return()
        }
      }
      
      if (input$data_source == "EQ") {
        plot_output_discrete$invoke(
          start = input$date_range[1],
          end = input$date_range[2],
          locations = if (input$locs_groups == "locations") input$locations_EQ else NULL,
          locGrp = if (input$locs_groups == "loc_groups") input$location_groups else NULL,
          parameters = if (input$params_groups == "parameters") input$parameters_EQ else NULL,
          paramGrp = if (input$params_groups == "param_groups") input$parameter_groups else NULL,
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
          legend_position = if (windowDims()$width > 1.3 * windowDims()$height) "v" else "h",
          gridx = plot_aes$showgridx,
          gridy = plot_aes$showgridy,
          dbSource = input$data_source,
          dbPath = input$EQWin_source, # EQWin connection so no need to pass config
          config = NULL # EQWin connection so no need to pass config
        )
      } else if (input$data_source == "AC") {
        plot_output_discrete$invoke(
          start = input$date_range[1],
          end = input$date_range[2],
          locations = as.numeric(input$locations_AC),
          locGrp = NULL,
          parameters = as.numeric(input$parameters_AC),
          paramGrp = NULL,
          standard = NULL,  # No standards in AquaCache yet
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
          legend_position = if (windowDims()$width > 1.3 * windowDims()$height) "v" else "h",
          gridx = plot_aes$showgridx,
          gridy = plot_aes$showgridy,
          dbSource = input$data_source,
          dbPath = NULL, # AquaCache connection so no need to pass database path
          config = session$userData$config
        )
      }
    }, ignoreInit = TRUE) # End of plot rendering loop
    
    
    # flags
    plot_created <- reactiveVal(FALSE) # Flags if a plot has been created so that window dimensions can be checked for legend position
    first_plot <- reactiveVal(TRUE) # Flags if this is the first plot generated by the user in this session, in which case a modal is shown
    first_plot_with_standards <- reactiveVal(TRUE) # Flags if this is the first plot generated by the user in this session with standards, in which case a modal is shown
    plotData <- reactiveVal() # Holds the data for the plot in case the user wants to download it
    
    observeEvent(plot_output_discrete$result(), {
      if (inherits(plot_output_discrete$result(), "character")) {
        showModal(modalDialog(
          title = tr("error", language$language),
          plot_output_discrete$result(),
          footer = tagList(
            actionButton(ns("cancel"), tr("cancel", language$language))
          ),
          easyClose = TRUE
        ))
        return()
      }
      output$plot <- plotly::renderPlotly({ isolate(plot_output_discrete$result()$plot)})
      plotData(plot_output_discrete$result()$data)
      
      shinyjs::show("full_screen")
      shinyjs::show("download_data")
      
      # If this is the first plot generated by the user in this session show them a modal
      if (first_plot()) {
        if (first_plot_with_standards()) {
          showModal(
            modalDialog(
              HTML(tr("first_plot_hints_standards", language$language)),
              footer = tagList(
                actionButton(ns("cancel"), tr("cancel", language$language))
              ),
              easyClose = TRUE)
          )
          first_plot_with_standards(FALSE)
        } else {
          showModal(
            modalDialog(
              HTML(tr("first_plot_hints_no_standards", language$language)),
              footer = tagList(
                actionButton(ns("cancel"), tr("cancel", language$language))
              ),
              easyClose = TRUE)
          )
        }
        first_plot(FALSE)
      }
      
      if (first_plot_with_standards()) {
        showModal(
          modalDialog(
            HTML(tr("first_plot_hints_standards_short", language$language)),
            footer = tagList(
              actionButton(ns("cancel"), tr("cancel", language$language))
            ),
            easyClose = TRUE))
        first_plot_with_standards(FALSE)
      }
    })
    
    
    # Observe changes to the windowDims reactive value and update the legend position using plotlyProxy
    # The js function takes care of debouncing the window resize event and also reacts to a change in orientation or full screen event
    
    observeEvent(windowDims(), {
      req(plot_created())
      if (is.null(windowDims())) return()
      if (windowDims()$width > 1.3 * windowDims()$height) {
        plotly::plotlyProxy("plot", session) %>%
          plotly::plotlyProxyInvoke("relayout", legend = list(orientation = "v"))
      } else {
        plotly::plotlyProxy("plot", session) %>%
          plotly::plotlyProxyInvoke("relayout", legend = list(orientation = "h"))
      }
    }, ignoreNULL = TRUE)
    
    # Observe the full screen button and run the javascript function to make the plot full screen
    observeEvent(input$full_screen, {
      shinyjs::runjs(paste0("toggleFullScreen('", session$ns("plot"), "');"))
      
      # Manually trigger a window resize event after some delay
      shinyjs::runjs("
                      setTimeout(function() {
                        sendWindowSizeToShiny();
                      }, 700);
                    ")
    }, ignoreInit = TRUE)
    
    # Send the user the plotting data
    output$download_data <- downloadHandler(
      filename = function() {
        time <- Sys.time()
        attr(time, "tzone") <- "UTC"
        paste0("discrete_plot_data_", gsub("-", "", gsub(" ", "_", gsub(":", "", substr(time, 0, 16)))), "_UTC.xlsx")
      },
      content = function(file) {
        openxlsx::write.xlsx(plotData(), file)
      }
    )
    
  }) # End of moduleServer
}
