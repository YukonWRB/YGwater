# UI and server code for add new sub_location module

addSubLocationUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    page_fluid(
      uiOutput(ns("ui"))
    )
  )
}


addSubLocation <- function(id, inputs) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Assign the input value to a reactive right away (passed in from the main server) as it's reset to NULL as soon as this module is loaded
    moduleInputs <- reactiveValues(sublocation = if (!is.null(inputs$sublocation)) inputs$sublocation else NULL)
    
    # Get some data from aquacache
    moduleData <- reactiveValues()
    
    getModuleData <- function() {
      moduleData$exist_locs = DBI::dbGetQuery(session$userData$AquaCache, "SELECT location_id, location, name, name_fr, latitude, longitude, note, contact, visibility_public, share_with, location_type, data_sharing_agreement_id, install_purpose, current_purpose, location_images, jurisdictional_relevance, anthropogenic_influence, sentinel_location FROM locations")
      moduleData$exist_sub_locs  = DBI::dbGetQuery(session$userData$AquaCache, "SELECT sub_location_id, location_id, sub_location_name, sub_location_name_fr, latitude, longitude, note, share_with FROM sub_locations;")
      moduleData$users = DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM get_roles_with_select_on_locations();")  # This is a helper function run with SECURITY DEFINER and created by postgres that pulls all users with select privileges on locations table
    }
    
    getModuleData()
    
    output$ui <- renderUI({
      tagList(
        radioButtons(ns("mode"), NULL,
                     choices = c("Add new" = "add", "Modify existing" = "modify"),
                     inline = TRUE),
        conditionalPanel(
          condition = "input.mode == 'modify'",
          ns = ns,
          DT::DTOutput(ns("table"))
        ),
        
        splitLayout(
          cellWidths = c("50%", "50%"),
          textInput(ns("subloc_name"), 
                    "Sub-location name (must not exist already)",
                    if (isTruthy(moduleInputs$sublocation)) moduleInputs$sublocation else NULL,
                    width = "100%"
          ),
          textInput(ns("subloc_name_fr"), 
                    "French sub-location name (must not exist already)",
                    width = "100%"
          )
        ),
        
        selectizeInput(ns("location"), 
                       "Associated location",
                       choices = stats::setNames(moduleData$exist_locs$location_id, moduleData$exist_locs$name),
                       multiple = FALSE,
                       options = list(maxItems = 1),
                       width = "100%"
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
        
        selectizeInput(ns("share_with"), 
                       "Share with groups (1 or more, type your own if not in list)", 
                       choices = moduleData$users$role_name,
                       selected = "public_reader",
                       multiple = TRUE,
                       options = list(create = TRUE),
                       width = "100%"
        ),
        
        textInput(ns("subloc_note"), 
                  "Sub-location note",
                  width = "100%"
        ),
        actionButton(ns("add_subloc"), 
                     "Add sub-location",
                     width = "100%"
        )
      )
    })
    
    ## Observers to modify existing entry ##########################################
    selected_sub_loc <- reactiveVal(NULL)
    
    output$table <- DT::renderDT({
      DT::datatable(moduleData$exist_sub_locs,
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
    }) |> bindEvent(moduleData$exist_sub_locs)
    
    observeEvent(input$table_rows_selected, {
      sel <- input$table_rows_selected
      if (length(sel) > 0) {
        sub_loc_id <- moduleData$exist_sub_locs[sel, "sub_location_id"]
        selected_sub_loc(sub_loc_id)
        details <- moduleData$exist_sub_locs[moduleData$exist_sub_locs$sub_location_id == sub_loc_id, ]
        if (nrow(details) > 0) {
          updateSelectizeInput(session, "location", selected = details$location_id)
          updateTextInput(session, "subloc_name", value = details$sub_location_name)
          updateTextInput(session, "subloc_name_fr", value = details$sub_location_name_fr)
          updateNumericInput(session, "lat", value = details$latitude)
          updateNumericInput(session, "lon", value = details$longitude)
          updateSelectizeInput(session, "share_with", selected = details$share_with)
          updateTextInput(session, "sub_loc_note", value = details$note)
        }
      } else {
        selected_sub_loc(NULL)
      }
    })
    
    observeEvent(input$mode, {
      if (input$mode == "modify") {
        # Disable the location input as it should not be changed when modifying a sub-location
        shinyjs::disable(ns("location"))
        updateActionButton(session, "add_subloc", label = "Update sub-location")
        updateTextInput(session, "subloc_name", label = "Sub-location name")
        updateTextInput(session, "subloc_name_fr", label = "French sub-location name")
      } else {
        # Enable the location input as it can be changed when adding a new sub-location
        shinyjs::enable(ns("location"))
        updateActionButton(session, "add_subloc", label = "Add sub-location")
        updateTextInput(session, "subloc_name", label = "Sub-location name (must not exist already)")
        updateTextInput(session, "subloc_name_fr", label = "French sub-location name (must not exist already)")
      }
    })
    
    ## Validation helpers for other inputs -----------------------------------
    observeEvent(input$subloc_name, {
      req(input$subloc_name)
      if (input$mode == "modify") {
        shinyjs::js$backgroundCol(ns("subloc_name"), "#fff")
      } else {
        if (input$subloc_name %in% moduleData$exist_sub_locs$sub_location_name) {
          shinyjs::js$backgroundCol(ns("subloc_name"), "#fdd")
        } else {
          shinyjs::js$backgroundCol(ns("subloc_name"), "#fff")
        }
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$subloc_name_fr, {
      req(input$subloc_name_fr)
      if (input$mode == "modify") {
        shinyjs::js$backgroundCol(ns("subloc_name_fr"), "#fff")
      } else {
        if (input$subloc_name_fr %in% moduleData$exist_sub_locs$sub_location_name_fr) {
          shinyjs::js$backgroundCol(ns("subloc_name_fr"), "#fdd")
        } else {
          shinyjs::js$backgroundCol(ns("subloc_name_fr"), "#fff")
        }
      }
    }, ignoreInit = TRUE)
    
    
    ## Make messages for lat/lon warnings #########################################
    # Reactive values to track warnings
    warnings <- reactiveValues(lat = NULL, 
                               lon = NULL)
    
    # Update reactive values for latitude warning
    observe({
      req(input$lat)
      if (input$lat < 0) {
        warnings$lat <- "Warning: Latitude is negative. Are you sure your sub-location is in the southern hemisphere?"
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
        warnings$lon <- "Warning: Longitude is positive. Are you sure your sub-location is east of the prime meridian?"
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
    
    output$lon_warning <- renderUI({
      if (!is.null(warnings$lon)) {
        div(
          style = "color: red; font-size: 12px; margin-top: -10px; margin-bottom: 10px;",
          warnings$lon
        )
      }
    })
    
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
    
    
    ## Observe the add_sub_location click #################
    # Run checks, if everything passes call AquaCache::addACLocation or update the location details
    observeEvent(input$add_subloc, {
      
      # Disable and re-enable the button to prevent multiple clicks
      shinyjs::disable("add_subloc")
      on.exit(shinyjs::enable("add_subloc"), add = TRUE)
      
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
      
      
      if (input$mode == "modify") {
        req(selected_sub_loc())
        
        # Start a transaction
        DBI::dbBegin(session$userData$AquaCache)
        tryCatch({
          # Check each field to see if it's been modified; if so, update the DB entry by targeting the location_id and appropriate column name
          # Changes to the location english sub_location_name
          if (input$subloc_name != moduleData$exist_sub_locs[which(moduleData$exist_sub_locs$sub_location_id == selected_sub_loc()), "sub_location_name"]) {
            DBI::dbExecute(session$userData$AquaCache, paste0("UPDATE sub_locations SET sub_location_name = '", input$subloc_name, "' WHERE sub_location_id = ", selected_sub_loc(), ";"))
          }
          
          # Changes to the location french sub_location_name
          if (input$subloc_name_fr != moduleData$exist_sub_locs[which(moduleData$exist_sub_locs$sub_location_id == selected_sub_loc()), "sub_location_name_fr"]) {
            DBI::dbExecute(session$userData$AquaCache, 
                           sprintf("UPDATE sub_locations SET sub_location_name_fr = '%s' WHERE sub_location_id = %d", input$subloc_name_fr, selected_sub_loc()))
          }
          
          # Changes to coordinates
          if (input$lat != moduleData$exist_sub_locs[which(moduleData$exist_sub_locs$sub_location_id == selected_sub_loc()), "latitude"]) {
            DBI::dbExecute(session$userData$AquaCache, 
                           sprintf("UPDATE sub_locations SET latitude = %f WHERE sub_location_id = %d", input$lat, selected_sub_loc()))
          }
          if (input$lon != moduleData$exist_sub_locs[which(moduleData$exist_sub_locs$sub_location_id == selected_sub_loc()), "longitude"]) {
            DBI::dbExecute(session$userData$AquaCache, 
                           sprintf("UPDATE sub_locations SET longitude = %f WHERE sub_location_id = %d", input$lon, selected_sub_loc()))
          }
          
          # Changes to share_with
          # if (!all(input$share_with %in% moduleData$exist_sub_locs[which(moduleData$exist_sub_locs$sub_location_id == selected_sub_loc()), "share_with"])) {
          #   # Remove all existing share_with entries for this location
          #   DBI::dbExecute(session$userData$AquaCache, 
          #                  sprintf("DELETE FROM locations_users WHERE location_id = %d", selected_sub_loc()))
          #   # Add the new share_with entries
          #   for (group in input$share_with) {
          #     DBI::dbExecute(session$userData$AquaCache, 
          #                    sprintf("INSERT INTO locations_users (location_id, role_name) VALUES (%d, '%s')", selected_sub_loc(), group))
          #   }
          # }
          
          # Changes to note
          if (isTruthy(input$subloc_note)) {
            if (!is.na(moduleData$exist_sub_locs[which(moduleData$exist_sub_locs$sub_location_id == selected_sub_loc()), "note"])) { # There might not be a note already
              if (input$subloc_note != moduleData$exist_sub_locs[which(moduleData$exist_sub_locs$sub_location_id == selected_sub_loc()), "note"]) {
                DBI::dbExecute(session$userData$AquaCache, 
                               sprintf("UPDATE sub_locations SET note = '%s' WHERE sub_location_id = %d", input$subloc_note, selected_sub_loc()))
              }
            } else {
              DBI::dbExecute(session$userData$AquaCache, 
                             sprintf("UPDATE sub_locations SET note = '%s' WHERE sub_location_id = %d", input$subloc_note, selected_sub_loc()))
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
      
      
      # If we are here, we are adding a new location
      if (!isTruthy(input$subloc_name)) {
        showModal(modalDialog(
          "Location name is mandatory",
          easyClose = TRUE
        ))
        return()
      } else {
        if (input$subloc_name %in% moduleData$exist_sub_locs$sub_location_name) {
          showModal(modalDialog(
            "Location name already exists",
            easyClose = TRUE
          ))
          return()
        }
      }
      if (!isTruthy(input$subloc_name_fr)) {
        showModal(modalDialog(
          "Location name (French) is mandatory",
          easyClose = TRUE
        ))
        return()
      } else {
        if (input$subloc_name_fr %in% moduleData$exist_sub_locs$sub_location_name_fr) {
          showModal(modalDialog(
            "Location name (French) already exists",
            easyClose = TRUE
          ))
          return()
        }
      }
      
      tryCatch({
        
        # Make a data.frame to pass in using dbAppendTable
        df <- data.frame(location_id = input$location,
                         sub_location_name = input$subloc_name,
                         sub_location_name_fr = input$subloc_name_fr,
                         latitude = input$lat,
                         longitude = input$lon,
                         share_with = paste0("{", paste(input$share_with, collapse = ", "), "}"),
                         note = if (isTruthy(input$subloc_note)) input$subloc_note else NA)
        
        DBI::dbAppendTable(session$userData$AquaCache, "sub_locations", df)
        
        # Show a modal to the user that the location was added
        showModal(modalDialog(
          "Sub-location added successfully",
          easyClose = TRUE
        ))
        
        # Update the moduleData reactiveValues
        getModuleData() # This should trigger an update to the table
        
        # Reset all fields
        updateTextInput(session, "subloc_name", value = character(0))
        updateTextInput(session, "subloc_name_fr", value = character(0))
        updateNumericInput(session, "lat", value = NA)
        updateNumericInput(session, "lon", value = NA)
        updateSelectizeInput(session, "share_with", selected = "public_reader")
        updateTextInput(session, "subloc_note", value = character(0))
        
      }, error = function(e) {
        showModal(modalDialog(
          "Error adding sub_location: ", e$message
        ))
      })
    })
    
  }) # End of moduleServer
}
