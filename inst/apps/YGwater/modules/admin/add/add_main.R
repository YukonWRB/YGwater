# UI and server code for new timeseries/location creation module


addUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    selectizeInput(ns("ts_loc"),
                   NULL,
                   choices = stats::setNames(c("loc", "ts", "users"), c("Add/edit location", "Add/edit timeseries", "Add users/user groups")),
                   selected = "loc"),
    
    # Use a different sidebar and main panel depending on the selection. Sidebar allows user to create new locations/timeseries, mainpanel to see what already exists in tables
    sidebarLayout(
      sidebarPanel(
        # Panels are loaded in the server so that they can be populated immediately with the possible choices
        conditionalPanel(
          ns = ns,
          condition = "input.ts_loc == 'loc'",
          uiOutput(ns("locSidebarUI"))
        ),
        conditionalPanel(
          ns = ns,
          condition = "input.ts_loc == 'ts'",
          uiOutput(ns("tsSidebarUI"))
        )
      ),
      mainPanel(
        conditionalPanel(
          ns = ns,
          condition = "input.ts_loc == 'loc'",
          uiOutput(ns("locMainUI"))
        ),
        conditionalPanel(
          ns = ns,
          condition = "input.ts_loc == 'ts'",
          uiOutput(ns("tsMainUI"))
        )
      )
    )
  )
}

add <- function(id, AquaCache) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns # Use to create UI elements within the server function
    
    # Get some data from the aquacache
    moduleData <- reactiveValues(
      exist_locs = dbGetQueryDT(AquaCache, "SELECT location_id, location, name, name_fr FROM locations"),
      loc_types = dbGetQueryDT(AquaCache, "SELECT * FROM location_types"),
      owners_contributors = dbGetQueryDT(AquaCache, "SELECT * FROM owners_contributors"),
      # limit documents to those that are data sharing agreements, which requires a join on table document_types
      agreements = dbGetQueryDT(AquaCache, "SELECT document_id, name, description FROM documents WHERE type = (SELECT document_type_id FROM document_types WHERE document_type_en = 'data sharing agreement')"),
      # parameters are linked to parameter_groups and parameter_sub_groups via parameter_relationships (to allow a many to many relationships)
      parameters = dbGetQueryDT(AquaCache, "SELECT parameter_id, param_name FROM parameters"),
      parameter_groups = dbGetQueryDT(AquaCache, "SELECT group_id, group_name, description FROM parameter_groups"),
      parameter_sub_groups = dbGetQueryDT(AquaCache, "SELECT sub_group_id, sub_group_name, description FROM parameter_sub_groups"),
      parameter_relationships = dbGetQueryDT(AquaCache, "SELECT * FROM parameter_relationships"),
      media = dbGetQueryDT(AquaCache, "SELECT media_id, media_type FROM media_types"),
      datums = dbGetQueryDT(AquaCache, "SELECT datum_id, datum_name_en FROM datum_list"),
      networks = dbGetQueryDT(AquaCache, "SELECT network_id, name FROM networks"),
      projects = dbGetQueryDT(AquaCache, "SELECT project_id, name FROM projects"),
      user_groups = dbGetQueryDT(AquaCache, "SELECT group_id, group_name, group_description FROM user_groups")
    )
    
    
    # Location add portion ######################################################
    output$locSidebarUI <- renderUI({
      tagList(
        textOutput(ns("hydat_note")
        ),
        textInput(ns("loc_code"), 
                  "Location code (must not exist already)"
        ),
        actionButton(ns("hydat_fill"), "Auto-fill from HYDAT"
        ),
        textInput(ns("loc_name"), 
                  "Location name (must not exist already)"
        ),
        textInput(ns("loc_name_fr"), 
                  "French location name (must not exist already)"
        ),
        selectizeInput(ns("loc_type"), 
                       "Location type",
                       choices = stats::setNames(moduleData$loc_types$type_id, moduleData$loc_types$type),
                       multiple = TRUE, # This is to force a default of nothing selected - overridden with options
                       options = list(maxItems = 1)
        ),
        numericInput(ns("lat"), 
                     "Latitude (decimal degrees)", 
                     value = NULL
        ),
        uiOutput(ns("lat_warning")),
        numericInput(ns("lon"), 
                     "Longitude (decimal degrees)", 
                     value = NULL
        ),
        uiOutput(ns("lon_warning")),
        selectizeInput(ns("viz"), 
                       "Public visibility", 
                       choices = stats::setNames(c("exact", "region", "jitter"), 
                                                 c("Exact", "Within general region", "At random within a 5 km radius of true location"))
        ),
        selectizeInput(ns("share_with"), 
                       "Share with groups (1 or more, type your own if not in list)", 
                       choices = stats::setNames(moduleData$user_groups$group_id,
                                                 paste0(moduleData$user_groups$group_name, " (", moduleData$user_groups$group_description, ")")),
                       multiple = TRUE,
                       selected = 1,
                       options = list(create = TRUE)
        ),
        selectizeInput(ns("loc_owner"), 
                       "Owner (type your own if not in list)", 
                       choices = stats::setNames(moduleData$owners_contributors$owner_contributor_id, 
                                                 moduleData$owners_contributors$name),
                       multiple = TRUE, # This is to force a default of nothing selected - overridden with options
                       options = list(maxItems = 1,
                                      create = TRUE)
        ),
        textInput(ns("loc_contact"),
                  "Contact details if different than owner default (optional)"
        ),
        selectizeInput(ns("data_sharing_agreement"), 
                       "Data sharing agreement", 
                       choices = stats::setNames(moduleData$agreements$document_id, 
                                                 moduleData$agreements$name),
                       options = list(placeholder = "Optional - add the document first if needed")
        ),
        selectizeInput(ns("datum_id_from"), 
                       "Datum ID from (Assumed datum is station 0)", 
                       choices = stats::setNames(moduleData$datums$datum_id, 
                                                 titleCase(moduleData$datums$datum_name_en, "en")), 
                       selected = 10
        ),
        selectizeInput(ns("datum_id_to"), 
                       "Datum ID to (Use assumed datum if no conversion to apply)",
                       multiple = TRUE, # This is to force a default of nothing selected - overridden with options
                       choices = stats::setNames(moduleData$datums$datum_id, 
                                                 titleCase(moduleData$datums$datum_name_en, "en")),
                       options = list(maxItems = 1) # Overrides multiple selection
        ), 
        numericInput(ns("elev"), 
                     "Elevation conversion (meters, use 0 if not converting)", 
                     value = NULL
        ),
        selectizeInput(ns("network"), 
                       "Network (type your own if not in list)", 
                       choices = stats::setNames(moduleData$networks$network_id, moduleData$networks$name),
                       multiple = TRUE, # This is to force a default of nothing selected - overridden with options
                       options = list(create = TRUE, 
                                      placeholder = "Optional but recommended",
                                      maxItems = 1)  # With a choice to allow users to add a network
        ),
        selectizeInput(ns("project"), 
                       "Project (type your own if not in list)", 
                       choices = stats::setNames(moduleData$projects$project_id, moduleData$projects$name), 
                       options = list(create = TRUE, 
                                      placeholder = "Optional")  # With a choice to allow users to add a project
        ),
        textInput(ns("loc_note"), 
                  "Location note"
        ),
        actionButton(ns("add_loc"), 
                     "Add location"
        )
      )
    })
    
    shinyjs::hide("hydat_fill") # Hide the button right away, it's shown if applicable
    
    ## Hydat fill ###############################################################
    # Detect if the user's location code is present in hydat. If so, show a button to enable them to auto-populate fields with hydat info
    hydat_note <- renderText("Entering a WSC code will allow you to auto-populate fields with HYDAT information.")
    hydat <- reactiveValues(exists = FALSE,
                            stns = NULL)
    if ((file.exists(tidyhydat::hy_downloaded_db()))) {
      hydat$exists <- TRUE
      hydat$stns <- tidyhydat::hy_stations()$STATION_NUMBER
    } else {
      shinyjs::hide(hydat_note)
    }
    
    observeEvent(input$loc_code, {# Observe loc_code inputs and, if possible, show the button to auto-populate fields
      if (!hydat$exists) {  # obviously can't do anything here...
        return()
      }
      if (input$loc_code %in% hydat$stns) {
        shinyjs::show("hydat_fill")
      } else {
        shinyjs::hide("hydat_fill")
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$hydat_fill, {
      # Get the station info from hydat
      stn <- tidyhydat::hy_stations(input$loc_code)
      if (nrow(stn) == 0) {
        return()
      }
      datum <- tidyhydat::hy_stn_datum_conv(input$loc_code)
      datum_list <- tidyhydat::hy_datum_list()
      # Replace DATUM_FROM with DATUM_ID
      datum$DATUM_FROM_ID <- datum_list$DATUM_ID[match(datum$DATUM_FROM, datum_list$DATUM_EN)]
      # Replace DATUM_TO with DATUM_ID
      datum$DATUM_TO_ID <- datum_list$DATUM_ID[match(datum$DATUM_TO, datum_list$DATUM_EN)]
      print(datum)
      
      # Drop original DATUM_FROM and DATUM_TO columns
      datum <- datum[, c("STATION_NUMBER", "DATUM_FROM_ID", "DATUM_TO_ID", "CONVERSION_FACTOR")]
      
      updateTextInput(session, "loc_name", value = titleCase(stn$STATION_NAME, "en"))
      updateNumericInput(session, "lat", value = stn$LATITUDE)
      updateNumericInput(session, "lon", value = stn$LONGITUDE)
      if (nrow(datum) == 0) {
        showModal(modalDialog(
          "No datum conversion found for this station in HYDAT."
        ))
        updateSelectizeInput(session, "datum_id_from", selected = 10)
        updateSelectizeInput(session, "datum_id_to", selected = 10)
        updateNumericInput(session, "elev", value = 0)
      } else {
        updateSelectizeInput(session, "datum_id_from", selected = datum$DATUM_FROM_ID[nrow(datum)])
        updateSelectizeInput(session, "datum_id_to", selected = datum$DATUM_TO_ID[nrow(datum)])
        updateNumericInput(session, "elev", value = datum$CONVERSION_FACTOR[nrow(datum)])
      }
      
      updateSelectizeInput(session, "loc_owner", selected = moduleData$owners_contributors[moduleData$owners_contributors$name == "Water Survey of Canada", "owner_contributor_id"])
      updateTextInput(session, "loc_note", value = paste0("Station metadata from HYDAT version ", substr(tidyhydat::hy_version()$Date[1], 1, 10)))
    }, ignoreInit = TRUE)
    
    
    ## Make messages for lat/lon warnings #########################################
    # Reactive values to track warnings
    warnings <- reactiveValues(lat = NULL, lon = NULL)
    
    # Update reactive values for latitude warning
    observe({
      req(input$lat)
      if (input$lat < 0) {
        warnings$lat <- "Warning: Latitude is negative. Are you sure you're in the southern hemisphere?"
      } else if (input$lat > 90) {
        warnings$lat <- "Error: Latitude cannot exceed 90 degrees."
      } else if (input$lat < -90) {
        warnings$lat <- "Error: Latitude cannot be less than -90 degrees."
      } else {
        warnings$lat <- NULL
      }
    })
    # Update reactive values for longitude warning
    observe({
      req(input$lon)
      if (input$lon < -180 || input$lon > 180) {
        warnings$lon <- "Error: Longitude must be between -180 and 180 degrees."
      } else if (input$lon > 0) {
        warnings$lon <- "Warning: Longitude is positive. Are you sure you're in the eastern hemisphere?"
      } else {
        warnings$lon <- NULL
      }
    })
    
    # Render latitude and longitude warnings dynamically
    output$lat_warning <- renderUI({
      if (!is.null(warnings$lat)) {
        div(
          style = "color: red; font-size: 12px; margin-top: -10; margin-bottom: 10px",
          warnings$lat
        )
      }
    })
    
    output$lon_warning <- renderUI({
      if (!is.null(warnings$lon)) {
        div(
          style = "color: red; font-size: 12px; margin-top: -10; margin-bottom: 10px;",
          warnings$lon
        )
      }
    })
    
    ## Allow users to add a few things to the DB ###################################
    ## If user types in their own network/project/owner/share_with, bring up a modal to add it to the database. This requires updating moduleData and the selectizeInput choices
    
    ### Observe the network selectizeInput for new networks #######################
    observeEvent(input$network, {
      if (input$network %in% moduleData$networks$network_id || nchar(input$network) == 0) {
        return()
      }
      net_types <- dbGetQueryDT(AquaCache, "SELECT id, name FROM network_project_types")
      showModal(modalDialog(
        textInput(ns("network_name"), "Network name"),
        textInput(ns("network_name_fr"), "Network name French (optional)"),
        textInput(ns("network_description"), "Network description"),
        textInput(ns("network_description_fr"), "Network description French (optional)"),
        selectizeInput(ns("network_type"), "Network type", stats::setNames(net_types$id, net_types$name), multiple = FALSE),
        actionButton(ns("add_network"), "Add network")
      ))
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    observeEvent(input$add_network, {
      # Check that mandatory fields are filled in
      if (!isTruthy(input$network_name)) {
        return()
      }
      if (!isTruthy(input$network_description)) {
        return()
      }
      # Add the network to the database
      df <- data.frame(name = input$network_name,
                       name_fr = if (isTruthy(input$network_name_fr)) input$network_name_fr else NA,
                       description = input$network_description,
                       description_fr = if (isTruthy(input$network_description_fr)) input$network_description_fr else NA,
                       type = input$network_type)
      DBI::dbAppendTable(AquaCache, "networks", df, append = TRUE)
      # Update the moduleData reactiveValues
      moduleData$networks <- dbGetQueryDT(AquaCache, "SELECT network_id, name FROM networks")
      # Update the selectizeInput to the new value
      updateSelectizeInput(session, "network", choices = stats::setNames(moduleData$networks$network_id, moduleData$networks$name), selected = moduleData$networks[moduleData$networks$name == df$name, "network_id"])
      showModal(modalDialog(
        "New network added."
      ))
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    ### Observe the project selectizeInput for new projects #######################
    observeEvent(input$project, {
      if (input$project %in% moduleData$projects$project_id || nchar(input$project) == 0) {
        return()
      }
      proj_types <- dbGetQueryDT(AquaCache, "SELECT id, name FROM network_project_types")
      
      showModal(modalDialog(
        textInput(ns("project_name"), "Project name"),
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
        
        return()
      }
      if (!isTruthy(input$project_description)) {
        showModal(modalDialog(
          "Project description is mandatory"
        ))
        return()
      }
      # Add the project to the database
      df <- data.frame(name = input$project_name,
                       name_fr = if (isTruthy(input$project_name_fr)) input$project_name_fr else NA,
                       description = input$project_description,
                       description_fr = if (isTruthy(input$project_description_fr)) input$project_description_fr else NA,
                       type = input$project_type)
      DBI::dbAppendTable(AquaCache, "projects", df, append = TRUE)
      # Update the moduleData reactiveValues
      moduleData$projects <- dbGetQueryDT(AquaCache, "SELECT project_id, name FROM projects")
      # Update the selectizeInput to the new value
      updateSelectizeInput(session, "project", choices = stats::setNames(moduleData$projects$project_id, moduleData$projects$name), selected = moduleData$projects[moduleData$projects$name == df$name, "project_id"])
      showModal(modalDialog(
        "New project added."
      ))
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    ### Observe the owner selectizeInput for new owners ############
    observeEvent(input$loc_owner, {
      if (input$loc_owner %in% moduleData$owners_contributors$owner_contributor_id || nchar(input$loc_owner) == 0) {
        return()
      }
      showModal(modalDialog(
        textInput(ns("owner_name"), "Owner name"), value = input$loc_owner,
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
        return()
      }
      # Add the owner to the database
      df <- data.frame(name = input$owner_name,
                       name_fr = if (isTruthy(input$owner_name_fr)) input$owner_name_fr else NA,
                       contact_name = if (isTruthy(input$contact_name)) input$contact_name else NA,
                       phone = if (isTruthy(input$contact_phone)) input$contact_phone else NA,
                       email = if (isTruthy(input$contact_email)) input$contact_email else NA,
                       note = if (isTruthy(input$contact_note)) input$contact_note else NA)
      DBI::dbAppendTable(AquaCache, "owners_contributors", df, append = TRUE)
      # Update the moduleData reactiveValues
      moduleData$owners_contributors <- dbGetQueryDT(AquaCache, "SELECT owner_contributor_id, name FROM owners_contributors")
      # Update the selectizeInput to the new value
      updateSelectizeInput(session, "loc_owner", choices = stats::setNames(moduleData$owners_contributors$owner_contributor_id, moduleData$owners_contributors$name), selected = moduleData$owners_contributors[moduleData$owners_contributors$name == df$name, "owner_contributor_id"])
      showModal(modalDialog(
        "New owner added."
      ))
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    ### Observe the share_with selectizeInput for new user groups ##############################
    observeEvent(input$share_with, {
      if (input$share_with[length(input$share_with)] %in% moduleData$user_groups$group_id || nchar(input$share_with[length(input$share_with)]) == 0) {
        return()
      }
      showModal(modalDialog(
        textInput(ns("group_name"), "Group name"),
        textInput(ns("group_description"), "Group description"),
        actionButton(ns("add_group"), "Add group")
      ))
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    observeEvent(input$add_group, {
      # Check that mandatory fields are filled in
      if (!isTruthy(input$group_name)) {
        return()
      }
      if (!isTruthy(input$group_description)) {
        return()
      }
      # Add the group to the database
      df <- data.frame(group_name = input$group_name,
                       group_description = input$group_description)
      DBI::dbAppendTable(AquaCache, "user_groups", df, append = TRUE)
      # Update the moduleData reactiveValues
      moduleData$user_groups <- dbGetQueryDT(AquaCache, "SELECT group_id, group_name, group_description FROM user_groups")
      # Update the selectizeInput to the new value
      updateSelectizeInput(session, "share_with", choices = stats::setNames(moduleData$user_groups$group_id, paste0(moduleData$user_groups$group_name, " (", moduleData$user_groups$group_description, ")")), selected = c(input$share_with, moduleData$user_groups[moduleData$user_groups$group_name == df$group_name, "group_id"]))
      showModal(modalDialog(
        "New group added."
      ))
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    
    ## Observe the add_location click #################
    # Run checks, if everything passes call AquaCache::addACLocation
    observeEvent(input$add_loc, {
      if (!isTruthy(input$loc_code)) {
        showModal(modalDialog(
          "Location code is mandatory"
        ))
        return()
      } else {
        if (input$loc_code %in% moduleData$exist_locs$location_id) {
          showModal(modalDialog(
            "Location code already exists"
          ))
          return()
        }
      }
      if (!isTruthy(input$loc_name)) {
        showModal(modalDialog(
          "Location name is mandatory"
        ))
        return()
      } else {
        if (input$loc_name %in% moduleData$exist_locs$name) {
          showModal(modalDialog(
            "Location name already exists"
          ))
          return()
        }
      }
      if (!isTruthy(input$loc_name_fr)) {
        showModal(modalDialog(
          "Location name (French) is mandatory"
        ))
        return()
      } else {
        if (input$loc_name_fr %in% moduleData$exist_locs$name_fr) {
          showModal(modalDialog(
            "Location name (French) already exists"
          ))
          return()
        }
      }
      if (!isTruthy(input$loc_type)) {
        showModal(modalDialog(
          "Location type is mandatory"
        ))
        return()
      }
      
      # Check that lat and lon are within bounds. For lat, -90 to 90. For lon, -180 to 180
      if (input$lat < -90 || input$lat > 90) {
        showModal(modalDialog(
          "Latitude must be between -90 and 90 degrees"
        ))
        return()
      }
      if (input$lon < -180 || input$lon > 180) {
        showModal(modalDialog(
          "Longitude must be between -180 and 180 degrees"
        ))
        return()
      }
      
      # If datums are both the same make sure elevation is 0
      if (input$datum_id_from == input$datum_id_to && input$elev != 0) {
        showModal(modalDialog(
          "Elevation conversion must be 0 if the datums are the same"
        ))
        return()
      }
      
      
      # Make a data.frame to pass to addACLocation
      df <- data.frame(location = input$loc_code,
                       name = input$loc_name,
                       name_fr = input$loc_name_fr,
                       latitude = input$lat,
                       longitude = input$lon,
                       visibility_public = input$viz,
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
                       project = if (isTruthy(input$project)) as.numeric(input$project) else NA)                
      print(df)
      
      tryCatch( {
        AquaCache::addACLocation(con = AquaCache,
                                 df = df)
        # Update the moduleData reactiveValues
        moduleData$exist_locs <- dbGetQueryDT(AquaCache, "SELECT location_id, location, name, name_fr FROM locations")
        
        # Show a modal to the user that the location was added
        showModal(modalDialog(
          "Location added successfully"
        ))
        
        # Reset all fields
        updateTextInput(session, "loc_code", value = NULL)
        updateTextInput(session, "loc_name", value = NULL)
        updateTextInput(session, "loc_name_fr", value = NULL)
        updateSelectizeInput(session, "loc_type", selected = NULL)
        updateNumericInput(session, "lat", value = NULL)
        updateNumericInput(session, "lon", value = NULL)
        updateSelectizeInput(session, "viz", selected = "exact")
        updateSelectizeInput(session, "share_with", selected = 1)
        updateSelectizeInput(session, "loc_owner", selected = NULL)
        updateTextInput(session, "loc_contact", value = NULL)
        updateSelectizeInput(session, "data_sharing_agreement", selected = NULL)
        updateSelectizeInput(session, "datum_id_from", selected = 10)
        updateSelectizeInput(session, "datum_id_to", selected = NULL)
        updateNumericInput(session, "elev", value = NULL)
        updateSelectizeInput(session, "network", selected = NULL)
        updateSelectizeInput(session, "project", selected = NULL)
        updateTextInput(session, "loc_note", value = NULL)
      }, error = function(e) {
        showModal(modalDialog(
          "Error adding location: ", e$message
        ))
      })
    })
    
    # Timeseries add portion ########################################################
    output$tsSidebarUI <- renderUI({
      tagList(
        selectizeInput(ns("loc_code_ts"), 
                       "Location code", 
                       choices = stats::setNames(moduleData$exist_locs$location_id, moduleData$exist_locs$location),
                       multiple = TRUE, # This is to force a default of nothing selected - overridden with options
                       options = list(maxItems = 1)
        ), 
        selectizeInput(ns("loc_name_ts"), 
                       "Location name", 
                       choices = stats::setNames(moduleData$exist_locs$location_id, moduleData$exist_locs$name),
                       multiple = TRUE, # This is to force a default of nothing selected - overridden with options
                       options = list(maxItems = 1)
        ),
        dateInput(ns("start_date_ts"), 
                  "Start date to look for data", 
                  value = "1900-01-01"
        ),
        selectizeInput(ns("param_ts"), 
                       "Parameter", 
                       choices = stats::setNames(moduleData$parameters$parameter_id, moduleData$parameters$param_name),
                       multiple = TRUE, # This is to force a default of nothing selected - overridden with options
                       options = list(maxItems = 1)
        ),
        actionButton(ns("param_modal"), 
                     "Filter parameters by group/sub group"  # Loads a modal where the user can filter parameters based on their group and sub-group
        ),
        selectizeInput(ns("media_ts"),
                       "Media", 
                       stats::setNames(moduleData$media$media_id, moduleData$media$media_type),
                       multiple = TRUE, # This is to force a default of nothing selected - overridden with options
                       options = list(maxItems = 1)
        ),
        actionButton(ns("add_ts"), 
                     "Add timeseries"
        )
      )
    })
    
    ## observeEvent for parameter filter modal ###########
    observeEvent(input$param_modal, {
      showModal(modalDialog(
        selectizeInput(ns("param_group"), 
                       "Parameter group", 
                       stats::setNames(moduleData$parameter_groups$group_id, moduleData$parameter_groups$group_name),
                       multiple = TRUE
        ),
        selectizeInput(ns("param_sub_group"), 
                       "Parameter sub-group", 
                       stats::setNames(moduleData$parameter_sub_groups$sub_group_id, moduleData$parameter_sub_groups$sub_group_name),
                       multiple = TRUE
        ),
        actionButton(ns("param_filter"), 
                     "Filter parameters"
        )
      ))
    })
    
    # Filter sub-group based on group selection
    observeEvent(input$param_group, {
      if (!isTruthy(input$param_group)) {
        return()
      }
      valid_sub_groups <- unique(moduleData$parameter_relationships[moduleData$parameter_relationships$group_id %in% input$param_group, c("group_id", "sub_group_id")])
      choices <- stats::setNames(moduleData$parameter_sub_groups[moduleData$parameter_sub_groups$sub_group_id %in% valid_sub_groups$sub_group_id, ]$sub_group_id, moduleData$parameter_sub_groups[moduleData$parameter_sub_groups$sub_group_id %in% valid_sub_groups$sub_group_id, ]$sub_group_name)
      updateSelectizeInput(session, "param_sub_group", choices = choices)
      
    }, ignoreInit = TRUE)
    
    # observeEvent for param_filter button
    observeEvent(input$param_filter, {
      # Get the parameters that match the filter
      params <- moduleData$parameters
      if (isTruthy(input$param_group)) {
        params <- params[params$parameter_id %in% moduleData$parameter_relationships$parameter_id[moduleData$parameter_relationships$group_id %in% input$param_group], ]
      }
      if (isTruthy(input$param_sub_group)) {
        params <- params[params$parameter_id %in% moduleData$parameter_relationships$parameter_id[moduleData$parameter_relationships$sub_group_id %in% input$param_sub_group], ]
      }
      updateSelectizeInput(session, "param_ts", choices = stats::setNames(params$parameter_id, params$param_name))
      removeModal()
    })
    
    # observeEvent for location name/code cross-selection
    observeEvent(input$loc_code_ts, {
      updateSelectizeInput(session, "loc_name_ts", selected = input$loc_code_ts)
    }, ignoreInit = TRUE)
    observeEvent(input$loc_name_ts, {
      updateSelectizeInput(session, "loc_code_ts", selected = input$loc_name_ts)
    }, ignoreInit = TRUE)
    
  }) # End of moduleServer
}
