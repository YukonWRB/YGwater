# UI and server code for new timeseries/location creation module


addTsLocUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    selectizeInput(ns("ts_loc"),
                   NULL,
                   choices = stats::setNames(c("loc", "ts"), c("Add new location", "Add new timeseries")),
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

addTsLoc <- function(id, AquaCache) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns # Use to create UI elements within the server function
    
    # Get some data from the AquaCache
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
        textInput(ns("loc_code"), 
                  "Location code (must not exist already)"
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
        textInput(ns("note"), 
                  "Location note"
        ),
        actionButton(ns("add_loc"), 
                     "Add location"
        )
      )
    })
    
    
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
    
    ## If user types in their own network/project/owner/share_with, bring up a modal to add it to the database. This requires updating moduleData and the selectizeInput choices
    # Observe the network click
    observeEvent(input$network, {
      if (input$network %in% moduleData$networks$network_id) {
        return()
      }
      showModal(modalDialog(
        textInput("network_name", "Network name"),
        actionButton("add_network", "Add network")
      ))
    })
    observeEvent("add_network", {
      # Add the network to the database
      # Update moduleData$networks
      # Update the selectizeInput choices
    })

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
      
      print("Checks passed!")
      return()  # Remove this line to actually run the function
      
      AquaCache::addACLocation(con = AquaCache,
                               df = NULL,
                               location = input$loc_code,
                               name = input$loc_name,
                               name_fr = input$loc_name_fr,
                               latitude = input$lat,
                               longitude = input$lon,
                               visibility_public = input$viz,
                               share_with = input$share_with,
                               owner = if (isTruthy(input$loc_owner)) input$loc_owner else NA,
                               data_sharing_agreement_id = if (isTruthy(input$data_sharing_agreement)) input$data_sharing_agreement else NA,
                               location_type = input$loc_type,
                               note = if (isTruthy(input$note)) input$note else NA,
                               contact = if (isTruthy(input$loc_contact)) input$loc_contact else NA,
                               datum_id_from = input$datum_id_from,
                               datum_id_to = input$datum_id_to,
                               conversion_m = input$elev,
                               current = TRUE,
                               network = if (isTruthy(input$network)) input$network else NA,
                               project = if (isTruthy(input$project)) input$project else NA)
      
    })
    
    # Timeseries add portion ########################################################
    output$tsSidebarUI <- renderUI({
      tagList(
        selectizeInput(ns("loc_code_select"), 
                       "Location code", 
                       choices = stats::setNames(moduleData$exist_locs$location_id, moduleData$exist_locs$location),
                       multiple = TRUE, # This is to force a default of nothing selected - overridden with options
                       options = list(maxItems = 1)
        ), 
        selectizeInput(ns("loc_name_select"), 
                       "Location name", 
                       choices = stats::setNames(moduleData$exist_locs$location_id, moduleData$exist_locs$name),
                       multiple = TRUE, # This is to force a default of nothing selected - overridden with options
                       options = list(maxItems = 1)
        ),
        dateInput(ns("start_date"), 
                  "Start date to look for data", 
                  value = "1900-01-01"
        ),
        selectizeInput(ns("param"), 
                       "Parameter", 
                       choices = stats::setNames(moduleData$parameters$parameter_id, moduleData$parameters$param_name),
                       multiple = TRUE, # This is to force a default of nothing selected - overridden with options
                       options = list(maxItems = 1)
        ),
        actionButton(ns("param_modal"), 
                     "Use filters to select parameter"  # Loads a modal where the user can filter parameters based on their group and sub-group, On save, input$param is updated
        ),
        selectizeInput(ns("media"),
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
    
    # observeEvent for parameter filter
    
    # observeEvent for location name/code cross-selection
    
    
  }) # End of moduleServer
}
