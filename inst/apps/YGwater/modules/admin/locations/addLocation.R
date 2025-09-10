# UI and server code for add new location module

addLocationUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    page_fluid(
      uiOutput(ns("ui"))
    )
  )
}


addLocation <- function(id, inputs) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Assign the input value to a reactive right away (passed in from the main server) as it's reset to NULL as soon as this module is loaded
    moduleInputs <- reactiveValues(location = if (!is.null(inputs$location)) inputs$location else NULL)
    
    shinyjs::hide("hydat_fill") # Hide the button right away, it's shown if applicable
    
    # Get some data from aquacache
    moduleData <- reactiveValues()
    
    getModuleData <- function() {
      moduleData$exist_locs = DBI::dbGetQuery(session$userData$AquaCache, "SELECT location_id, location, name, name_fr, latitude, longitude, note, contact, share_with, location_type, data_sharing_agreement_id, install_purpose, current_purpose, location_images, jurisdictional_relevance, anthropogenic_influence, sentinel_location FROM locations")
      moduleData$loc_types = DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM location_types")
      moduleData$organizations = DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM organizations")
      # limit documents to those that are data sharing agreements, which requires a join on table document_types
      moduleData$agreements = DBI::dbGetQuery(session$userData$AquaCache, "SELECT document_id, name, description FROM documents WHERE type = (SELECT document_type_id FROM document_types WHERE document_type_en = 'data sharing agreement')")
      moduleData$datums = DBI::dbGetQuery(session$userData$AquaCache, "SELECT datum_id, datum_name_en FROM datum_list")
      moduleData$datum_conversions = DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM datum_conversions WHERE current IS TRUE")
      moduleData$networks = DBI::dbGetQuery(session$userData$AquaCache, "SELECT network_id, name FROM networks")
      moduleData$projects = DBI::dbGetQuery(session$userData$AquaCache, "SELECT project_id, name FROM projects")
      moduleData$users = DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM public.get_shareable_principals_for('public.locations');")  # This is a helper function run with SECURITY DEFINER and created by postgres that pulls all user groups (plus public_reader) with select privileges on a table
    }
    
    getModuleData() # Initial data load
    
    output$ui <- renderUI({
      tagList(
        radioButtons(ns("mode"), NULL,
                     choices = c("Add new" = "add", "Modify existing" = "modify"),
                     inline = TRUE),
        conditionalPanel(
          condition = "input.mode == 'modify'",
          ns = ns,
          DT::DTOutput(ns("loc_table"))
        ),
        htmlOutput(ns("hydat_note")
        ),
        textInput(ns("loc_code"), 
                  "Location code (must not exist already)",
                  width = "100%"
                  
        ),
        actionButton(ns("hydat_fill"), "Auto-fill from HYDAT"
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          textInput(ns("loc_name"), 
                    "Location name (must not exist already)",
                    if (isTruthy(moduleInputs$location)) moduleInputs$location else NULL,
                    width = "100%"
          ),
          textInput(ns("loc_name_fr"), 
                    "French location name (must not exist already)",
                    width = "100%"
          )
        ),
        
        splitLayout(
          cellWidths = c("50%", "50%"),
          numericInput(ns("lat"), 
                       "Latitude (decimal degrees)", 
                       value = NA,
                       width = "100%"
          ),
          numericInput(ns("lon"), 
                       "Longitude (decimal degrees)", 
                       value = NA,
                       width = "100%"
          )
        ),
        uiOutput(ns("lat_warning")),
        uiOutput(ns("lon_warning")),
        
        selectizeInput(ns("loc_type"), 
                       "Location type",
                       choices = stats::setNames(moduleData$loc_types$type_id, moduleData$loc_types$type),
                       multiple = TRUE, # This is to force a default of nothing selected - overridden with options
                       options = list(maxItems = 1),
                       width = "100%"
        ),
        selectizeInput(ns("share_with"), 
                       "Share with groups (1 or more, type your own if not in list)", 
                       choices = moduleData$users$role_name,
                       selected = "public_reader",
                       multiple = TRUE,
                       options = list(create = TRUE),
                       width = "100%"
        ),
        
        splitLayout(
          cellWidths = c("0%", "50%", "50%"),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
          selectizeInput(ns("loc_owner"), 
                         "Owner (type your own if not in list)", 
                         choices = stats::setNames(moduleData$organizations$organization_id, 
                                                   moduleData$organizations$name),
                         multiple = TRUE, # This is to force a default of nothing selected - overridden with options
                         options = list(maxItems = 1,
                                        create = TRUE),
                         width = "100%"
          ),
          textInput(ns("loc_contact"),
                    "Contact details if different than owner default (optional)",
                    width = "100%"
          )
        ),
        
        selectizeInput(ns("data_sharing_agreement"), 
                       "Data sharing agreement", 
                       choices = stats::setNames(moduleData$agreements$document_id, 
                                                 moduleData$agreements$name),
                       options = list(placeholder = "Optional - add the document first if needed",
                                      maxItems = 1),
                       width = "100%",
                       multiple = TRUE # This is to force a default of nothing selected - overridden with options
        ),
        
        splitLayout(
          cellWidths = c("0%", "33.3%", "33.3%", "33.3%"),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
          selectizeInput(ns("datum_id_from"), 
                         "Datum ID from (Assumed datum is station 0)", 
                         choices = stats::setNames(moduleData$datums$datum_id, 
                                                   titleCase(moduleData$datums$datum_name_en, "en")), 
                         selected = 10,
                         width = "100%"
          ),
          selectizeInput(ns("datum_id_to"), 
                         "Datum ID to (Use assumed datum if no conversion to apply)",
                         multiple = TRUE, # This is to force a default of nothing selected - overridden with options
                         choices = stats::setNames(moduleData$datums$datum_id, 
                                                   titleCase(moduleData$datums$datum_name_en, "en")),
                         options = list(maxItems = 1), # Overrides multiple selection
                         width = "100%"
          ), 
          numericInput(ns("elev"), 
                       "Elevation conversion (meters, use 0 if not converting)", 
                       value = NA,
                       width = "100%"
          )
        ),
        uiOutput(ns("elev_warning")),
        
        splitLayout(
          cellWidths = c("0%", "50%", "50%"),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
          selectizeInput(ns("network"), 
                         "Network (type your own if not in list)", 
                         choices = stats::setNames(moduleData$networks$network_id, moduleData$networks$name),
                         multiple = TRUE, # This is to force a default of nothing selected - overridden with options
                         options = list(create = TRUE, 
                                        placeholder = "Optional but recommended",
                                        maxItems = 1),  # With a choice to allow users to add a network
                         width = "100%"
          ),
          selectizeInput(ns("project"), 
                         "Project(s) (type your own if not in list)", 
                         choices = stats::setNames(moduleData$projects$project_id, moduleData$projects$name),
                         multiple = TRUE,
                         options = list(create = TRUE, 
                                        placeholder = "Optional",
                                        maxItems = 1),  # With a choice to allow users to add a project
                         width = "100%"
          )
        ),
        
        splitLayout(
          cellWidths = c("0%", "50%", "50%"),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}"))),
          checkboxInput(ns("loc_jurisdictional_relevance"), 
                        "Check if this location is publicly relevant to your jurisdiction (i.e. should be seen by the public)", 
                        value = TRUE
          ),
          checkboxInput(ns("loc_anthropogenic_influence"), 
                        "Check if this location is influenced by human activity (dams, upstream mining, etc.)", 
                        value = FALSE
          )
        ),
        
        textInput(ns("loc_install_purpose"), 
                  "Installation or establishment purpose (optional)",
                  placeholder = "Optional",
                  width = "100%"
        ),
        textInput(ns("loc_current_purpose"), 
                  "Current purpose (optional)",
                  placeholder = "Optional",
                  width = "100%"
        ),
        textInput(ns("loc_note"), 
                  "Location note",
                  width = "100%"
        ),
        actionButton(ns("add_loc"), 
                     "Add location",
                     width = "100%"
        )
      )
    })
    
    ## Observers to modify existing entry ##########################################
    selected_loc <- reactiveVal(NULL)
    
    output$loc_table <- DT::renderDT({
      DT::datatable(moduleData$exist_locs,
                    selection = "single",
                    options = list(
                      columnDefs = list(list(targets = 0, visible = FALSE)),
                      scrollX = TRUE,
                      initComplete = htmlwidgets::JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({",
                        "  'background-color': '#079',",
                        "  'color': '#fff',",
                        "  'font-size': '100%',",
                        "});",
                        "$(this.api().table().body()).css({",
                        "  'font-size': '90%',",
                        "});",
                        "}"
                      )
                    ),
                    rownames = FALSE)
    }) |> bindEvent(moduleData$exist_locs)
    
    # Observe row selection and update inputs accordingly
    observeEvent(input$loc_table_rows_selected, {
      sel <- input$loc_table_rows_selected
      if (length(sel) > 0) {
        loc_id <- moduleData$exist_locs[sel, "location_id"]
        selected_loc(loc_id)
        details <- moduleData$exist_locs[moduleData$exist_locs$location_id == loc_id, ]
        datum_details <- moduleData$datum_conversions[moduleData$datum_conversions$location_id == loc_id, ]
        
        if (nrow(details) > 0) {
          updateTextInput(session, "loc_code", value = details$location)
          updateTextInput(session, "loc_name", value = details$name)
          updateTextInput(session, "loc_name_fr", value = details$name_fr)
          updateSelectizeInput(session, "loc_type", selected = details$location_type)
          updateNumericInput(session, "lat", value = details$latitude)
          updateNumericInput(session, "lon", value = details$longitude)
          updateSelectizeInput(session, "share_with", selected = details$share_with)
          updateSelectizeInput(session, "loc_owner", selected = details$owner)
          updateTextInput(session, "loc_contact", value = details$contact)
          updateSelectizeInput(session, "data_sharing_agreement", selected = details$data_sharing_agreement_id)
          updateSelectizeInput(session, "datum_id_from", selected = datum_details$datum_id_from)
          updateSelectizeInput(session, "datum_id_to", selected = datum_details$datum_id_to)
          updateNumericInput(session, "elev", value = datum_details$conversion_m)
          nids <- DBI::dbGetQuery(session$userData$AquaCache,
                                  sprintf("SELECT network_id FROM locations_networks WHERE location_id = %d", loc_id))
          updateSelectizeInput(session, "network", selected = nids$network_id)
          pids <- DBI::dbGetQuery(session$userData$AquaCache,
                                  sprintf("SELECT project_id FROM locations_projects WHERE location_id = %d", loc_id))
          updateSelectizeInput(session, "project", selected = pids$project_id)
          updateCheckboxInput(session, "loc_jurisdictional_relevance", value = details$jurisdictional_relevance)
          updateCheckboxInput(session, "loc_anthropogenic_influence", value = details$anthropogenic_influence)
          updateTextInput(session, "loc_install_purpose", value = details$install_purpose)
          updateTextInput(session, "loc_current_purpose", value = details$current_purpose)
          updateTextInput(session, "loc_note", value = details$note)
        }
      } else {
        selected_loc(NULL)
      }
    })
    
    observeEvent(input$mode, {
      if (input$mode == "modify") {
        updateActionButton(session, "add_loc", label = "Update location")
        updateTextInput(session, "loc_code", label = "Location code")
        updateTextInput(session, "loc_name", label = "Location name")
        updateTextInput(session, "loc_name_fr", label = "French location name")
      } else {
        updateActionButton(session, "add_loc", label = "Add location")
        updateTextInput(session, "loc_code", label = "Location code (must not exist already)")
        updateTextInput(session, "loc_name", label = "Location name (must not exist already)")
        updateTextInput(session, "loc_name_fr", label = "French location name (must not exist already)")
      }
    })
    
    
    ## Hydat fill ###############################################################
    # Detect if the user's location code is present in hydat. If so, show a button to enable them to auto-populate fields with hydat info
    output$hydat_note <- renderUI({HTML("<b>Entering a WSC code will allow you to auto-populate fields with HYDAT information.</b><br>")})
    hydat <- reactiveValues(exists = FALSE,
                            stns = NULL)
    
    
    if ((file.exists(tidyhydat::hy_downloaded_db()))) {
      # Check the age of the hydat database
      hydat_path <- tidyhydat::hy_downloaded_db()
      local_hydat <- as.Date(tidyhydat::hy_version(hydat_path)$Date)
      local_hydat <- gsub("-", "", as.character(local_hydat))
      remote_hydat <- tidyhydat::hy_remote()
      if (local_hydat != remote_hydat) { #if remote version is not the same, download new version
        showNotification("A newer version of the HYDAT database is available. Attempting to update the local copy now.", type = "warning")
        try(tidyhydat::download_hydat(ask = FALSE))
        hydat_path <- tidyhydat::hy_downloaded_db() #reset the hydat path just in case the new DB is not named exactly as the old one (guard against tidyhydat package changes in future)
        local_hydat <- as.Date(tidyhydat::hy_version(hydat_path)$Date) #check the HYDAT version again just in case. It can fail to update without actually creating an error and stopping.
        local_hydat <- gsub("-", "", as.character(local_hydat))
        if (local_hydat == remote_hydat) {
          new_hydat <- TRUE
          showNotification("The local WSC HYDAT database was updated.", type = "message")
        } else {
          showNotification("The local WSC HYDAT database could not be updated. Continuing to use the existing local copy.", type = "error")
        }
      }
      hydat$exists <- TRUE
      showNotification("HYDAT database found. You can auto-populate fields for WSC stations.", type = "message")
      hydat$stns <- tidyhydat::hy_stations()$STATION_NUMBER
    } else {
      showNotification("HYDAT database not found, attempting to download it.", type = "warning")
      tidyhydat::hy_download_db()
      if (file.exists(tidyhydat::hy_downloaded_db())) {
        hydat$exists <- TRUE
        showNotification("HYDAT database successfully downloaded. You can auto-populate fields for WSC stations.", type = "message")
        hydat$stns <- tidyhydat::hy_stations()$STATION_NUMBER
      } else {
        showNotification("HYDAT database still not found. You will not be able to auto-populate fields for WSC stations.", type = "error")
      }
      shinyjs::hide("hydat_note")
    }
    
    observeEvent(input$loc_code, {# Observe loc_code inputs and, if possible, show the button to auto-populate fields
      req(input$loc_code)
      if (hydat$exists) {
        if (input$loc_code %in% hydat$stns) {
          shinyjs::show("hydat_fill")
        } else {
          shinyjs::hide("hydat_fill")
        }
      } else {
        shinyjs::hide("hydat_fill")  # If HYDAT is not available, hide the button
      }
      
      # Check if the location code already exists in the database. If yes, make the selectizeInput pink
      if (input$mode == "modify") {
        shinyjs::js$backgroundCol(ns("loc_code"), "#fff")
      } else {
        if (input$loc_code %in% moduleData$exist_locs$location) {
          shinyjs::js$backgroundCol(ns("loc_code"), "#fdd")
        } else {
          shinyjs::js$backgroundCol(ns("loc_code"), "#fff")
        }
      }
    }, ignoreInit = TRUE)
    
    ## Validation helpers for other inputs -----------------------------------
    observeEvent(input$loc_name, {
      req(input$loc_name)
      if (input$mode == "modify") {
        shinyjs::js$backgroundCol(ns("loc_name"), "#fff")
      } else {
        if (input$loc_name %in% moduleData$exist_locs$name) {
          shinyjs::js$backgroundCol(ns("loc_name"), "#fdd")
        } else {
          shinyjs::js$backgroundCol(ns("loc_name"), "#fff")
        }
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$loc_name_fr, {
      req(input$loc_name_fr)
      if (input$mode == "modify") {
        shinyjs::js$backgroundCol(ns("loc_name_fr"), "#fff")
      } else {
        if (input$loc_name_fr %in% moduleData$exist_locs$name_fr) {
          shinyjs::js$backgroundCol(ns("loc_name_fr"), "#fdd")
        } else {
          shinyjs::js$backgroundCol(ns("loc_name_fr"), "#fff")
        }
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$hydat_fill, {
      req(input$loc_code)
      # Get the station info from hydat
      stn <- tidyhydat::hy_stations(input$loc_code)
      if (nrow(stn) == 0) {
        return()
      }
      if (hydat$exists) {
        datum <- tidyhydat::hy_stn_datum_conv(input$loc_code)
      } else {
        datum <- data.frame()
      }
      if (nrow(datum) == 0) {
        showModal(modalDialog(
          "No datum conversion found for this station in HYDAT."
        ))
        updateSelectizeInput(session, "datum_id_from", selected = 10)
        updateSelectizeInput(session, "datum_id_to", selected = 10)
        updateNumericInput(session, "elev", value = 0)
      } else {
        datum_list <- tidyhydat::hy_datum_list()
        # Replace DATUM_FROM with DATUM_ID
        datum$DATUM_FROM_ID <- datum_list$DATUM_ID[match(datum$DATUM_FROM, datum_list$DATUM_EN)]
        # Replace DATUM_TO with DATUM_ID
        datum$DATUM_TO_ID <- datum_list$DATUM_ID[match(datum$DATUM_TO, datum_list$DATUM_EN)]
        
        # Drop original DATUM_FROM and DATUM_TO columns
        datum <- datum[, c("STATION_NUMBER", "DATUM_FROM_ID", "DATUM_TO_ID", "CONVERSION_FACTOR")]
        
        updateTextInput(session, "loc_name", value = titleCase(stn$STATION_NAME, "en"))
        updateNumericInput(session, "lat", value = stn$LATITUDE)
        updateNumericInput(session, "lon", value = stn$LONGITUDE)
        
        updateSelectizeInput(session, "datum_id_from", selected = datum$DATUM_FROM_ID[nrow(datum)])
        updateSelectizeInput(session, "datum_id_to", selected = datum$DATUM_TO_ID[nrow(datum)])
        updateNumericInput(session, "elev", value = datum$CONVERSION_FACTOR[nrow(datum)])
      }
      
      updateSelectizeInput(session, "loc_owner", selected = moduleData$organizations[moduleData$organizations$name == "Water Survey of Canada", "organization_id"])
      updateTextInput(session, "loc_note", value = paste0("Station metadata from HYDAT version ", substr(tidyhydat::hy_version()$Date[1], 1, 10)))
      updateSelectizeInput(session, "network", selected = moduleData$networks[moduleData$networks$name == "Canada Yukon Hydrometric Network", "network_id"])
    }, ignoreInit = TRUE)
    
    
    ## Make messages for lat/lon warnings #########################################
    # Reactive values to track warnings
    warnings <- reactiveValues(lat = NULL, 
                               lon = NULL,
                               elev = NULL)
    
    # Update reactive values for latitude warning
    observe({
      req(input$lat)
      if (input$lat < 0) {
        warnings$lat <- "Warning: Latitude is negative. Are you sure your location is in the southern hemisphere?"
        shinyjs::js$backgroundCol(ns("lat"), "#fdd")
      } else if (input$lat > 90 || input$lat < -90) {
        warnings$lat <- "Error: Latitude cannot exceed + or - 90 degrees."
        shinyjs::js$backgroundCol(ns("lat"), "#fdd")
      } else {
        warnings$lat <- NULL
        shinyjs::js$backgroundCol(ns("lat"), "#fff")
      }
    })
    # Update reactive values for longitude warning
    
    observe({
      req(input$lon)
      if (input$lon < -180 || input$lon > 180) {
        warnings$lon <- "Error: Longitude must be between -180 and 180 degrees."
        shinyjs::js$backgroundCol(ns("lon"), "#fdd")
      } else if (input$lon > 0) {
        warnings$lon <- "Warning: Longitude is positive. Are you sure your location is east of the prime meridian?"
        shinyjs::js$backgroundCol(ns("lon"), "#fdd")
      } else {
        warnings$lon <- NULL
        shinyjs::js$backgroundCol(ns("lon"), "#fff")
      }
    })
    
    # Render latitude and longitude warnings dynamically
    output$lat_warning <- renderUI({
      if (!is.null(warnings$lat)) {
        div(
          style = "color: red; font-size: 12px; margin-top: -10px; margin-bottom: 10px",
          warnings$lat
        )
      }
    })
    
    observe({
      req(input$datum_id_from, input$datum_id_to, input$elev)
      if (input$datum_id_from == input$datum_id_to && input$elev != 0) {
        shinyjs::js$backgroundCol(ns("elev"), "#fdd")
        warnings$elev <- "Warning: Elevation conversion is set to a non-zero value but the from/to datums are the same. Are you sure you want to do this?"
      } else {
        shinyjs::js$backgroundCol(ns("elev"), "#fff")
        warnings$elev <- NULL
      }
    })
    
    output$elev_warning <- renderUI({
      if (!is.null(warnings$elev)) {
        div(
          style = "color: red; font-size: 12px; margin-top: -10px; margin-bottom: 10px;",
          warnings$elev
        )
      }
    })
    
    output$lon_warning <- renderUI({
      if (!is.null(warnings$lon)) {
        div(
          style = "color: red; font-size: 12px; margin-top: -10px; margin-bottom: 10px;",
          warnings$lon
        )
      }
    })
    
    ## Allow users to add a few things to the DB besides locations ###################################
    ## If user types in their own network/project/owner/share_with, bring up a modal to add it to the database. This requires updating moduleData and the selectizeInput choices
    
    ### Observe the network selectizeInput for new networks #######################
    observeEvent(input$network, {
      if (input$network %in% moduleData$networks$network_id || nchar(input$network) == 0) {
        return()
      }
      net_types <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT id, name FROM network_project_types")
      showModal(modalDialog(
        textInput(ns("network_name"), "Network name", value = input$loc_name),
        textInput(ns("network_name_fr"), "Network name French (optional)"),
        textInput(ns("network_description"), "Network description"),
        textInput(ns("network_description_fr"), "Network description French (optional)"),
        selectizeInput(ns("network_type"), "Network type", stats::setNames(net_types$id, net_types$name), multiple = FALSE),
        actionButton(ns("add_network"), "Add network")
      ))
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    observeEvent(input$add_network, {
      # Close the modal dialog
      # Check that mandatory fields are filled in
      if (!isTruthy(input$network_name)) {
        shinyjs::js$backgroundCol(ns("network_name"), "#fdd")
        return()
      }
      if (!isTruthy(input$network_description)) {
        shinyjs::js$backgroundCol(ns("network_description"), "#fdd")
        return()
      }
      # Add the network to the database
      df <- data.frame(name = input$network_name,
                       name_fr = if (isTruthy(input$network_name_fr)) input$network_name_fr else NA,
                       description = input$network_description,
                       description_fr = if (isTruthy(input$network_description_fr)) input$network_description_fr else NA,
                       type = input$network_type)
      DBI::dbAppendTable(session$userData$AquaCache, "networks", df, append = TRUE)
      
      # Update the moduleData reactiveValues
      moduleData$networks <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT network_id, name FROM networks")
      # Update the selectizeInput to the new value
      updateSelectizeInput(session, "network", choices = stats::setNames(moduleData$networks$network_id, moduleData$networks$name), selected = moduleData$networks[moduleData$networks$name == df$name, "network_id"])
      removeModal()
      showModal(modalDialog(
        "New network added.",
        easyClose = TRUE
      ))
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    ### Observe the project selectizeInput for new projects #######################
    observeEvent(input$project, {
      if (input$project %in% moduleData$projects$project_id || nchar(input$project) == 0) {
        return()
      }
      proj_types <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT id, name FROM network_project_types")
      
      showModal(modalDialog(
        textInput(ns("project_name"), "Project name", value = input$loc_name),
        textInput(ns("project_name_fr"), "Project name French (optional)"),
        textInput(ns("project_description"), "Project description"),
        textInput(ns("project_description_fr"), "Project description French (optional)"),
        selectizeInput(ns("project_type"), "Project type", stats::setNames(proj_types$id, proj_types$name), multiple = FALSE),
        actionButton(ns("add_project"), "Add project")
      ))
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    observeEvent(input$add_project, {
      # Check that mandatory fields are filled in
      if (!isTruthy(input$project_name)) {
        shinyjs::js$backgroundCol(ns("project_name"), "#fdd")
        return()
      }
      if (!isTruthy(input$project_description)) {
        shinyjs::js$backgroundCol(ns("project_description"), "#fdd")
        return()
      }
      # Add the project to the database
      df <- data.frame(name = input$project_name,
                       name_fr = if (isTruthy(input$project_name_fr)) input$project_name_fr else NA,
                       description = input$project_description,
                       description_fr = if (isTruthy(input$project_description_fr)) input$project_description_fr else NA,
                       type = input$project_type)
      DBI::dbAppendTable(session$userData$AquaCache, "projects", df, append = TRUE)
      
      # Update the moduleData reactiveValues
      moduleData$projects <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT project_id, name FROM projects")
      # Update the selectizeInput to the new value
      updateSelectizeInput(session, "project", choices = stats::setNames(moduleData$projects$project_id, moduleData$projects$name), selected = moduleData$projects[moduleData$projects$name == df$name, "project_id"])
      removeModal()
      showModal(modalDialog(
        "New project added.",
        easyClose = TRUE
      ))
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    ### Observe the owner selectizeInput for new owners ############
    observeEvent(input$loc_owner, {
      if (input$loc_owner %in% moduleData$organizations$organization_id || nchar(input$loc_owner) == 0) {
        return()
      }
      showModal(modalDialog(
        textInput(ns("owner_name"), "Owner name", value = input$loc_owner),
        textInput(ns("owner_name_fr"), "Owner name French (optional)"),
        textInput(ns("contact_name"), "Contact name (optional)"),
        textInput(ns("contact_phone"), "Contact phone (optional)"),
        textInput(ns("contact_email"), "Contact email (optional)"),
        textInput(ns("contact_note"), "Contact note (optional, for context)"),
        actionButton(ns("add_owner"), "Add owner")
      ))
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    observeEvent(input$add_owner, {
      # Check that mandatory fields are filled in
      if (!isTruthy(input$owner_name)) {
        shinyjs::js$backgroundCol(ns("owner_name"), "#fdd")
        return()
      }
      # Add the owner to the database
      df <- data.frame(name = input$owner_name,
                       name_fr = if (isTruthy(input$owner_name_fr)) input$owner_name_fr else NA,
                       contact_name = if (isTruthy(input$contact_name)) input$contact_name else NA,
                       phone = if (isTruthy(input$contact_phone)) input$contact_phone else NA,
                       email = if (isTruthy(input$contact_email)) input$contact_email else NA,
                       note = if (isTruthy(input$contact_note)) input$contact_note else NA)
      DBI::dbAppendTable(session$userData$AquaCache, "organizations", df, append = TRUE)
      
      # Update the moduleData reactiveValues
      moduleData$organizations <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT organization_id, name FROM organizations")
      # Update the selectizeInput to the new value
      updateSelectizeInput(session, "loc_owner", choices = stats::setNames(moduleData$organizations$organization_id, moduleData$organizations$name), selected = moduleData$organizations[moduleData$organizations$name == df$name, "organization_id"])
      removeModal()
      showModal(modalDialog(
        "New owner added.",
        easyClose = TRUE
      ))
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    ### Observe the share_with selectizeInput for new user groups ##############################
    observeEvent(input$share_with, {
      if (length(input$share_with) > 1 & 'public_reader' %in% input$share_with) {
        showModal(modalDialog(
          "If public_reader is selected it must be the only group selected.",
          easyClose = TRUE
        ))
        updateSelectizeInput(session, "share_with", selected = "public_reader")
      }
      
      if (input$share_with[length(input$share_with)] %in% moduleData$users$role_name || nchar(input$share_with[length(input$share_with)]) == 0) {
        return()
      }
      showModal(modalDialog(
        "Ask a database admin to create a new user or user group"
      ))
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    
    ## Observe the add_location click #################
    # Run checks, if everything passes call AquaCache::addACLocation or update the location details
    observeEvent(input$add_loc, {
      
      # Disable the button to prevent multiple clicks
      shinyjs::disable("add_loc")
      on.exit(shinyjs::enable("add_loc"))  # Re-enable the button when the observer exits
      
      # Ensure lat + lon are truthy
      if (!isTruthy(input$lat) || !isTruthy(input$lon)) {
        showModal(modalDialog(
          "Latitude and longitude are mandatory",
          easyClose = TRUE
        ))
        return()
      }
      # Check that lat and lon are within bounds. For lat, -90 to 90. For lon, -180 to 180
      if (input$lat < -90 || input$lat > 90) {
        showModal(modalDialog(
          "Latitude must be between -90 and 90 degrees",
          easyClose = TRUE
        ))
        return()
      }
      if (input$lon < -180 || input$lon > 180) {
        showModal(modalDialog(
          "Longitude must be between -180 and 180 degrees",
          easyClose = TRUE
        ))
        return()
      }
      
      # Ensure that datum_id_from and datum_id_to are truthy
      if (!isTruthy(input$datum_id_from) || !isTruthy(input$datum_id_to)) {
        showModal(modalDialog(
          "Datum ID from and to are mandatory (use assumed datum for both if there is no conversion to apply)",
          easyClose = TRUE
        ))
        return()
      }
      
      # If datums are both the same make sure elevation is 0
      if (input$datum_id_from == input$datum_id_to && input$elev != 0) {
        showModal(modalDialog(
          "Elevation conversion must be 0 if the datums are the same",
          easyClose = TRUE
        ))
        return()
      }
      
      
      if (input$mode == "modify") {
        req(selected_loc())
        
        # Start a transaction
        DBI::dbBegin(session$userData$AquaCache)
        tryCatch({
          # Check each field to see if it's been modified; if so, update the DB entry by targeting the location_id and appropriate column name
          # Changes to the location code
          if (input$loc_code != moduleData$exist_locs[which(moduleData$exist_locs$location_id == selected_loc()), "location"]) {
            DBI::dbExecute(
              session$userData$AquaCache,
              glue::glue_sql(
                "UPDATE locations SET location = {input$loc_code} WHERE location_id = {selected_loc()};",
                .con = session$userData$AquaCache
              )
            )
            # Update the corresponding entry in the 'vectors' table. the layer_name is 'Locations', should match on 'feature_name' = input$loc_code
            DBI::dbExecute(
              session$userData$AquaCache,
              glue::glue_sql(
                "UPDATE vectors SET feature_name = {input$loc_code} WHERE layer_name = 'Locations' AND feature_name = {moduleData$exist_locs[which(moduleData$exist_locs$location_id == selected_loc()), 'location']};",
                .con = session$userData$AquaCache
              )
            )
          }
          
          # Changes to the location english name
          if (input$loc_name != moduleData$exist_locs[which(moduleData$exist_locs$location_id == selected_loc()), "name"]) {
            DBI::dbExecute(
              session$userData$AquaCache,
              glue::glue_sql(
                "UPDATE locations SET name = {input$loc_name} WHERE location_id = {selected_loc()};",
                .con = session$userData$AquaCache
              )
            )
            # Update the corresponding entry in the 'vectors' table. the layer_name is 'Locations', should match on 'feature_name' = input$loc_code
            DBI::dbExecute(
              session$userData$AquaCache,
              glue::glue_sql(
                "UPDATE vectors SET description = {input$loc_name} WHERE layer_name = 'Locations' AND feature_name = {input$loc_code};",
                .con = session$userData$AquaCache
              )
            )
          }
          
          # Changes to the location french name
          if (input$loc_name_fr != moduleData$exist_locs[which(moduleData$exist_locs$location_id == selected_loc()), "name_fr"]) {
            DBI::dbExecute(
              session$userData$AquaCache,
              glue::glue_sql(
                "UPDATE locations SET name_fr = {input$loc_name_fr} WHERE location_id = {selected_loc()};",
                .con = session$userData$AquaCache
              )
            )
          }
          
          # Changes to the location type
          if (input$loc_type != moduleData$exist_locs[which(moduleData$exist_locs$location_id == selected_loc()), "location_type"]) {
            DBI::dbExecute(
              session$userData$AquaCache,
              glue::glue_sql(
                "UPDATE locations SET location_type = {input$loc_type} WHERE location_id = {selected_loc()};",
                .con = session$userData$AquaCache
              )
            )
          }
          
          # Changes to coordinates
          updated_coords <- FALSE
          if (input$lat != moduleData$exist_locs[which(moduleData$exist_locs$location_id == selected_loc()), "latitude"]) {
            DBI::dbExecute(
              session$userData$AquaCache,
              glue::glue_sql(
                "UPDATE locations SET latitude = {input$lat} WHERE location_id = {selected_loc()};",
                .con = session$userData$AquaCache
              )
            )
            updated_coords <- TRUE
          }
          if (input$lon != moduleData$exist_locs[which(moduleData$exist_locs$location_id == selected_loc()), "longitude"]) {
            DBI::dbExecute(
              session$userData$AquaCache,
              glue::glue_sql(
                "UPDATE locations SET longitude = {input$lon} WHERE location_id = {selected_loc()};",
                .con = session$userData$AquaCache
              )
            )
            updated_coords <- TRUE
          }
          if (updated_coords) {
            # Update the corresponding entry in the 'vectors' table. the layer_name is 'Locations', match it on 'feature_name' = input$loc_code. the 'geom' field (geometry data type) will be updated with the new coordinates
            DBI::dbExecute(
              session$userData$AquaCache,
              glue::glue_sql(
                "UPDATE vectors SET geom = ST_SetSRID(ST_MakePoint({input$lon}, {input$lat}), 4269) WHERE layer_name = 'Locations' AND feature_name = {input$loc_code};",
                .con = session$userData$AquaCache
              )
            )
          }
          
          # Changes to share_with
          if (!paste0("{", paste(input$share_with, collapse = ","), "}") == moduleData$exist_locs[which(moduleData$exist_locs$location_id == selected_loc()), "share_with"]) {
            share_with_sql <- DBI::SQL(paste0("{", paste(input$share_with, collapse = ", "), "}"))
            DBI::dbExecute(
              session$userData$AquaCache,
              glue::glue_sql(
                "UPDATE locations SET share_with = {share_with_sql} WHERE location_id = {selected_loc()};",
                .con = session$userData$AquaCache
              )
            )
          }
          
          # Changes to owner
          
          # TODO: this needs to touch the location_metadata_owner table, not the locations table
          # if (isTruthy(input$loc_owner)) {
          #   if (input$loc_owner != moduleData$exist_locs[which(moduleData$exist_locs$location_id == selected_loc()), "owner"]) {
          #     DBI::dbExecute(session$userData$AquaCache, 
          #                    sprintf("UPDATE locations SET owner = %d WHERE location_id = %d", as.numeric(input$loc_owner), selected_loc()))
          #   }
          # }
          
          # Changes to contact
          if (isTruthy(input$loc_contact)) {
            if (!is.na(moduleData$exist_locs[which(moduleData$exist_locs$location_id == selected_loc()), "contact"])) {
              # If the contact is not empty, update it
              if (input$loc_contact != moduleData$exist_locs[which(moduleData$exist_locs$location_id == selected_loc()), "contact"]) {
                DBI::dbExecute(session$userData$AquaCache, 
                               sprintf("UPDATE locations SET contact = '%s' WHERE location_id = %d", input$loc_contact, selected_loc()))
              }
            } else {
              # If the contact is empty, insert it
              DBI::dbExecute(session$userData$AquaCache, 
                             sprintf("UPDATE locations SET contact = '%s' WHERE location_id = %d", input$loc_contact, selected_loc()))
            }
          }
          
          # Changes to data sharing agreement
          if (isTruthy(input$data_sharing_agreement)) {
            if (input$data_sharing_agreement != moduleData$exist_locs[which(moduleData$exist_locs$location_id == selected_loc()), "data_sharing_agreement_id"]) {
              DBI::dbExecute(session$userData$AquaCache, 
                             sprintf("UPDATE locations SET data_sharing_agreement_id = %d WHERE location_id = %d", as.numeric(input$data_sharing_agreement), selected_loc()))
            }
          }
          
          # datum and elevation changes
          if (input$datum_id_from != moduleData$datum_conversions[which(moduleData$datum_conversions$location_id == selected_loc()), "datum_id_from"]) {
            DBI::dbExecute(session$userData$AquaCache, 
                           sprintf("UPDATE datum_conversions SET datum_id_from = %d WHERE location_id = %d", as.numeric(input$datum_id_from), selected_loc()))
          }
          if (input$datum_id_to != moduleData$datum_conversions[which(moduleData$datum_conversions$location_id == selected_loc()), "datum_id_to"]) {
            DBI::dbExecute(session$userData$AquaCache, 
                           sprintf("UPDATE datum_conversions SET datum_id_to = %d WHERE location_id = %d", as.numeric(input$datum_id_to), selected_loc()))
          }
          if (input$elev != moduleData$datum_conversions[which(moduleData$datum_conversions$location_id == selected_loc()), "conversion_m"]) {
            DBI::dbExecute(session$userData$AquaCache, 
                           sprintf("UPDATE datum_conversions SET conversion_m = %f WHERE location_id = %d", input$elev, selected_loc()))
          }
          
          # Changes to network
          if (isTruthy(input$network)) {
            # Remove all existing networks for this location
            DBI::dbExecute(session$userData$AquaCache, 
                           sprintf("DELETE FROM locations_networks WHERE location_id = %d", selected_loc()))
            # Add the new network
            DBI::dbExecute(session$userData$AquaCache, 
                           sprintf("INSERT INTO locations_networks (location_id, network_id) VALUES (%d, %d)", selected_loc(), as.numeric(input$network)))
          }
          
          # Changes to project
          if (isTruthy(input$project)) {
            # Remove all existing projects for this location
            DBI::dbExecute(session$userData$AquaCache, 
                           sprintf("DELETE FROM locations_projects WHERE location_id = %d", selected_loc()))
            # Add the new project. Build a df because more than one project can be associated with a location
            df <- data.frame(location_id = selected_loc(),
                             project_id = as.numeric(input$project))
            DBI::dbAppendTable(session$userData$AquaCache, "locations_projects", df, append = TRUE)
          }
          
          # Changes to jurisdictional relevance
          if (isTruthy(input$loc_jurisdictional_relevance)) {
            if (input$loc_jurisdictional_relevance != moduleData$exist_locs[which(moduleData$exist_locs$location_id == selected_loc()), "jurisdictional_relevance"]) {
              DBI::dbExecute(session$userData$AquaCache, 
                             sprintf("UPDATE locations SET jurisdictional_relevance = %s WHERE location_id = %d", input$loc_jurisdictional_relevance, selected_loc()))
            }
          }
          
          # Changes to anthropogenic influence
          if (isTruthy(input$loc_anthropogenic_influence)) {
            if (input$loc_anthropogenic_influence != moduleData$exist_locs[which(moduleData$exist_locs$location_id == selected_loc()), "anthropogenic_influence"]) {
              DBI::dbExecute(session$userData$AquaCache, 
                             sprintf("UPDATE locations SET anthropogenic_influence = %s WHERE location_id = %d", input$loc_anthropogenic_influence, selected_loc()))
            }
          }
          
          # Changes to install purpose
          if (isTruthy(input$loc_install_purpose)) {
            # If the current install_purpose is not NA, check if it has changed
            if (!is.na(moduleData$exist_locs[which(moduleData$exist_locs$location_id == selected_loc()), "install_purpose"])) {
              if (input$loc_install_purpose != moduleData$exist_locs[which(moduleData$exist_locs$location_id == selected_loc()), "install_purpose"]) {
                DBI::dbExecute(session$userData$AquaCache, 
                               sprintf("UPDATE locations SET install_purpose = '%s' WHERE location_id = %d", input$loc_install_purpose, selected_loc()))
              } 
            } else {
              # If the install_purpose was NA, just set it
              DBI::dbExecute(session$userData$AquaCache, 
                             sprintf("UPDATE locations SET install_purpose = '%s' WHERE location_id = %d", input$loc_install_purpose, selected_loc()))
            }
          }
          
          # Changes to current purpose
          if (isTruthy(input$loc_current_purpose)) {
            # If the current purpose is not NA, check if it has changed
            if (!is.na(moduleData$exist_locs[which(moduleData$exist_locs$location_id == selected_loc()), "current_purpose"])) {
              if (input$loc_current_purpose != moduleData$exist_locs[which(moduleData$exist_locs$location_id == selected_loc()), "current_purpose"]) {
                DBI::dbExecute(session$userData$AquaCache, 
                               sprintf("UPDATE locations SET current_purpose = '%s' WHERE location_id = %d", input$loc_current_purpose, selected_loc()))
              }
            } else {
              # If the current purpose was NA, just set it
              DBI::dbExecute(session$userData$AquaCache, 
                             sprintf("UPDATE locations SET current_purpose = '%s' WHERE location_id = %d", input$loc_current_purpose, selected_loc()))
            }
          }
          
          # Changes to note
          if (isTruthy(input$loc_note)) {
            if (!is.na(moduleData$exist_locs[which(moduleData$exist_locs$location_id == selected_loc()), "note"])) { # There might not be a note already
              if (input$loc_note != moduleData$exist_locs[which(moduleData$exist_locs$location_id == selected_loc()), "note"]) {
                DBI::dbExecute(session$userData$AquaCache, 
                               sprintf("UPDATE locations SET note = '%s' WHERE location_id = %d", input$loc_note, selected_loc()))
              }
            } else {
              DBI::dbExecute(session$userData$AquaCache, 
                             sprintf("UPDATE locations SET note = '%s' WHERE location_id = %d", input$loc_note, selected_loc()))
            }
          }
          
          # Show a notification that the location was updated
          showNotification("Location updated successfully", type = "message")
          # Commit the transaction
          DBI::dbCommit(session$userData$AquaCache)
          
          # Update the moduleData reactiveValues
          getModuleData() # This should trigger an update to the table
          
        }, error = function(e) {
          # If there was an error, rollback the transaction
          DBI::dbRollback(session$userData$AquaCache)
          showModal(modalDialog(
            "Error updating location: ", e$message
          ))
        })
        
        return()
      }
      
      
      if (!isTruthy(input$loc_code)) {
        showModal(modalDialog(
          "Location code is mandatory",
          easyClose = TRUE
        ))
        return()
      } else {
        if (input$loc_code %in% moduleData$exist_locs[moduleData$exist_locs$location != input$loc_code, "location"]) {
          showModal(modalDialog(
            "Location code already exists",
            easyClose = TRUE
          ))
          return()
        }
      }
      if (!isTruthy(input$loc_name)) {
        showModal(modalDialog(
          "Location name is mandatory",
          easyClose = TRUE
        ))
        return()
      } else {
        if (input$loc_name %in% moduleData$exist_locs$name) {
          showModal(modalDialog(
            "Location name already exists",
            easyClose = TRUE
          ))
          return()
        }
      }
      if (!isTruthy(input$loc_name_fr)) {
        showModal(modalDialog(
          "Location name (French) is mandatory",
          easyClose = TRUE
        ))
        return()
      } else {
        if (input$loc_name_fr %in% moduleData$exist_locs$name_fr) {
          showModal(modalDialog(
            "Location name (French) already exists",
            easyClose = TRUE
          ))
          return()
        }
      }
      if (!isTruthy(input$loc_type)) {
        showModal(modalDialog(
          "Location type is mandatory",
          easyClose = TRUE
        ))
        return()
      }
      
      # If we are here, we are adding a new location
      # Make a data.frame to pass to addACLocation
      df <- data.frame(location = input$loc_code,
                       name = input$loc_name,
                       name_fr = input$loc_name_fr,
                       latitude = input$lat,
                       longitude = input$lon,
                       share_with = input$share_with,
                       owner = if (isTruthy(input$loc_owner)) as.numeric(input$loc_owner) else NA,
                       data_sharing_agreement_id = if (isTruthy(input$data_sharing_agreement)) as.numeric(input$data_sharing_agreement) else NA,
                       location_type = as.numeric(input$loc_type),
                       note = if (isTruthy(input$loc_note)) input$loc_note else NA,
                       contact = if (isTruthy(input$loc_contact)) input$loc_contact else NA,
                       datum_id_from = as.numeric(input$datum_id_from),
                       datum_id_to = as.numeric(input$datum_id_to),
                       conversion_m = input$elev,
                       current = TRUE,
                       network = if (isTruthy(input$network)) as.numeric(input$network) else NA,
                       project = if (isTruthy(input$project)) as.numeric(input$project) else NA,
                       jurisdictional_relevance = if (isTruthy(input$loc_jurisdictional_relevance)) input$loc_jurisdictional_relevance else NA,
                       anthropogenic_influence = if (isTruthy(input$loc_anthropogenic_influence)) input$loc_anthropogenic_influence else NA,
                       install_purpose = if (isTruthy(input$loc_install_purpose)) input$loc_install_purpose else NA,
                       current_purpose = if (isTruthy(input$loc_current_purpose)) input$loc_current_purpose else NA)
      
      tryCatch({
        # Start a transaction
        DBI::dbBegin(session$userData$AquaCache)
        
        AquaCache::addACLocation(con = session$userData$AquaCache,
                                 df = df)
        
        # Close transaction
        DBI::dbCommit(session$userData$AquaCache)
        
        # Show a modal to the user that the location was added
        showModal(modalDialog(
          "Location added successfully",
          easyClose = TRUE
        ))
        
        # Update the moduleData reactiveValues
        getModuleData() # This should trigger an update to the table
        
        # Reset all fields
        updateTextInput(session, "loc_code", value = character(0))
        updateTextInput(session, "loc_name", value = character(0))
        updateTextInput(session, "loc_name_fr", value = character(0))
        updateSelectizeInput(session, "loc_type", selected = character(0))
        updateNumericInput(session, "lat", value = NA)
        updateNumericInput(session, "lon", value = NA)
        updateSelectizeInput(session, "viz", selected = "exact")
        updateSelectizeInput(session, "share_with", selected = "public_reader")
        updateSelectizeInput(session, "loc_owner", selected = character(0))
        updateTextInput(session, "loc_contact", value = character(0))
        updateSelectizeInput(session, "data_sharing_agreement", selected = character(0))
        updateSelectizeInput(session, "datum_id_from", selected = 10)
        updateSelectizeInput(session, "datum_id_to", selected = 10)
        updateNumericInput(session, "elev", value = 0)
        updateSelectizeInput(session, "network", selected = character(0))
        updateSelectizeInput(session, "project", selected = character(0))
        updateTextInput(session, "loc_note", value = character(0))
        
      }, error = function(e) {
        # Rollback the transaction if there was an error
        DBI::dbRollback(session$userData$AquaCache)
        showModal(modalDialog(
          "Error adding location: ", e$message
        ))
      })
    })
    
  }) # End of moduleServer
}
