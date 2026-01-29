# UI and server code for add new sub_location module

addSubLocationUI <- function(id) {
  ns <- NS(id)

  tagList(
    page_fluid(
      uiOutput(ns("banner")),
      uiOutput(ns("ui"))
    )
  )
}


addSubLocation <- function(id, inputs, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "addSubLocation"
      )
    })

    # Assign the input value to a reactive right away (passed in from the main server) as it's reset to NULL as soon as this module is loaded
    moduleInputs <- reactiveValues(
      sublocation = if (!is.null(inputs$sublocation)) {
        inputs$sublocation
      } else {
        NULL
      }
    )

    # Get some data from aquacache
    moduleData <- reactiveValues()

    getModuleData <- function() {
      moduleData$exist_locs = DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT location_id, name, latitude, longitude FROM locations ORDER BY name"
      )
      moduleData$exist_sub_locs = DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT sub_location_id, location_id, sub_location_name, sub_location_name_fr, latitude, longitude, note, share_with FROM sub_locations ORDER BY sub_location_name;"
      )
      moduleData$users = DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT * FROM public.get_shareable_principals_for('public.sub_locations');"
      ) # This is a helper function run with SECURITY DEFINER and created by postgres that pulls all user groups (plus public_reader) with select privileges on a table
    }

    getModuleData()

    output$ui <- renderUI({
      tagList(
        radioButtons(
          ns("mode"),
          NULL,
          choices = c("Add new" = "add", "Modify existing" = "modify"),
          inline = TRUE
        ),
        conditionalPanel(
          condition = "input.mode == 'modify'",
          ns = ns,
          DT::DTOutput(ns("table"))
        ),

        selectizeInput(
          ns("location"),
          "Associated location",
          choices = stats::setNames(
            moduleData$exist_locs$location_id,
            moduleData$exist_locs$name
          ),
          multiple = TRUE,
          options = list(maxItems = 1),
          width = "100%"
        ),

        splitLayout(
          cellWidths = c("50%", "50%"),
          textInput(
            ns("subloc_name"),
            "Sub-location name (must not exist already)",
            if (isTruthy(moduleInputs$sublocation)) {
              moduleInputs$sublocation
            } else {
              NULL
            },
            width = "100%"
          ),
          textInput(
            ns("subloc_name_fr"),
            "French sub-location name (must not exist already)",
            width = "100%"
          )
        ),

        splitLayout(
          cellWidths = c("40%", "40%", "20%"),
          numericInput(
            ns("lat"),
            "Latitude (decimal degrees, WGS84)",
            value = NA,
            width = "100%"
          ) |>
            tooltip(
              "Latitude in decimal degrees, e.g. 62.1234. Positive values indicate northern hemisphere."
            ),
          numericInput(
            ns("lon"),
            "Longitude (decimal degrees, WGS84)",
            value = NA,
            width = "100%"
          ) |>
            tooltip(
              "Longitude in decimal degrees, e.g. -135.1234. Negative values indicate west of the prime meridian."
            ),
          actionButton(
            ns("open_map"),
            "Choose or show coordinates on map",
            icon = icon("map-location-dot"),
            width = "100%",
            # Bump it down a bit to align with numericInputs
            style = "margin-top: 30px;"
          )
        ),
        uiOutput(ns("lat_warning")),
        uiOutput(ns("lon_warning")),
        uiOutput(ns("proximity_warning")),

        selectizeInput(
          ns("share_with"),
          "Share with groups (1 or more, type your own if not in list)",
          choices = moduleData$users$role_name,
          selected = "public_reader",
          multiple = TRUE,
          options = list(create = TRUE),
          width = "100%"
        ),

        textInput(ns("subloc_note"), "Sub-location note", width = "100%"),
        uiOutput(ns("distance_ack")),
        actionButton(ns("add_subloc"), "Add sub-location", width = "100%")
      )
    })

    ## Observers to modify existing entry ##########################################
    selected_sub_loc <- reactiveVal(NULL)

    output$table <- DT::renderDT({
      DT::datatable(
        moduleData$exist_sub_locs,
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
        filter = 'top',
        rownames = FALSE
      )
    }) |>
      bindEvent(moduleData$exist_sub_locs)

    observeEvent(input$table_rows_selected, {
      sel <- input$table_rows_selected
      if (length(sel) > 0) {
        sub_loc_id <- moduleData$exist_sub_locs[sel, "sub_location_id"]
        selected_sub_loc(sub_loc_id)
        details <- moduleData$exist_sub_locs[
          moduleData$exist_sub_locs$sub_location_id == sub_loc_id,
        ]
        if (nrow(details) > 0) {
          updateSelectizeInput(
            session,
            "location",
            selected = details$location_id
          )
          updateTextInput(
            session,
            "subloc_name",
            value = details$sub_location_name
          )
          updateTextInput(
            session,
            "subloc_name_fr",
            value = details$sub_location_name_fr
          )
          updateNumericInput(session, "lat", value = details$latitude)
          updateNumericInput(session, "lon", value = details$longitude)
          updateSelectizeInput(
            session,
            "share_with",
            selected = array_to_text(details$share_with)
          )
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
        updateTextInput(
          session,
          "subloc_name_fr",
          label = "French sub-location name"
        )
      } else {
        # Enable the location input as it can be changed when adding a new sub-location
        shinyjs::enable(ns("location"))
        updateActionButton(session, "add_subloc", label = "Add sub-location")
        updateTextInput(
          session,
          "subloc_name",
          label = "Sub-location name (must not exist already)"
        )
        updateTextInput(
          session,
          "subloc_name_fr",
          label = "French sub-location name (must not exist already)"
        )
      }
    })

    ## Validation helpers for other inputs -----------------------------------
    observeEvent(
      input$subloc_name,
      {
        req(input$subloc_name)
        if (input$mode == "modify") {
          shinyjs::js$backgroundCol(ns("subloc_name"), "#fff")
        } else {
          if (
            input$subloc_name %in% moduleData$exist_sub_locs$sub_location_name
          ) {
            shinyjs::js$backgroundCol(ns("subloc_name"), "#fdd")
          } else {
            shinyjs::js$backgroundCol(ns("subloc_name"), "#fff")
          }
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$subloc_name_fr,
      {
        req(input$subloc_name_fr)
        if (input$mode == "modify") {
          shinyjs::js$backgroundCol(ns("subloc_name_fr"), "#fff")
        } else {
          if (
            input$subloc_name_fr %in%
              moduleData$exist_sub_locs$sub_location_name_fr
          ) {
            shinyjs::js$backgroundCol(ns("subloc_name_fr"), "#fdd")
          } else {
            shinyjs::js$backgroundCol(ns("subloc_name_fr"), "#fff")
          }
        }
      },
      ignoreInit = TRUE
    )

    ## Make messages for lat/lon warnings #########################################
    # Reactive values to track warnings
    warnings <- reactiveValues(lat = NULL, lon = NULL)

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

    distance_meters <- function(lat1, lon1, lat2, lon2) {
      earth_radius <- 6371000
      to_rad <- pi / 180
      lat1 <- lat1 * to_rad
      lon1 <- lon1 * to_rad
      lat2 <- lat2 * to_rad
      lon2 <- lon2 * to_rad
      delta_lat <- lat2 - lat1
      delta_lon <- lon2 - lon1
      a <- sin(delta_lat / 2)^2 +
        cos(lat1) * cos(lat2) * sin(delta_lon / 2)^2
      c <- 2 * atan2(sqrt(a), sqrt(1 - a))
      earth_radius * c
    }

    ## Map picker ##############################################################
    map_center <- reactiveVal(list(lat = 64.0, lon = -135.0, zoom = 4))
    map_selection <- reactiveVal(NULL)

    primary_location <- reactive({
      req(moduleData$exist_locs)
      req(!is.null(input$location))
      location_id <- suppressWarnings(as.integer(input$location))
      if (is.na(location_id)) {
        return(NULL)
      }
      loc <- moduleData$exist_locs[
        moduleData$exist_locs$location_id == location_id,
      ]
      if (nrow(loc) == 0 || is.na(loc$latitude) || is.na(loc$longitude)) {
        return(NULL)
      }
      list(lat = loc$latitude, lon = loc$longitude, name = loc$name)
    })

    sub_location_distance <- reactive({
      req(isTruthy(input$lat))
      req(isTruthy(input$lon))
      primary <- primary_location()
      if (is.null(primary)) {
        return(NULL)
      }
      distance_meters(input$lat, input$lon, primary$lat, primary$lon)
    })

    output$proximity_warning <- renderUI({
      distance <- sub_location_distance()
      if (is.null(distance) || distance <= 10) {
        return(NULL)
      }
      div(
        style = "margin-top: -6px; margin-bottom: 10px;",
        tags$strong(
          sprintf(
            "Warning: Sub-location is %.1f meters from the parent location. It should be within 10 meters.",
            distance
          )
        )
      )
    })

    output$distance_ack <- renderUI({
      distance <- sub_location_distance()
      if (is.null(distance) || distance <= 10 || distance >= 100) {
        return(NULL)
      }
      checkboxInput(
        ns("distance_ack"),
        "I acknowledge that the sub-location is > 10 meters from it's parent location",
        value = FALSE,
        width = "100%"
      )
    })

    observe({
      distance <- sub_location_distance()
      requires_ack <- !is.null(distance) && distance > 10 && distance < 100
      too_far <- !is.null(distance) && distance >= 100

      if (too_far) {
        shinyjs::disable("add_subloc")
      } else if (requires_ack && !isTRUE(input$distance_ack)) {
        shinyjs::disable("add_subloc")
      } else {
        shinyjs::enable("add_subloc")
      }
    })

    output$location_map <- leaflet::renderLeaflet({
      center <- map_center()
      sel <- isolate(map_selection())
      primary <- isolate(primary_location())

      m <- leaflet::leaflet(options = leaflet::leafletOptions(maxZoom = 19)) %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery,
          group = "Satellite"
        ) %>%
        leaflet::addLayersControl(
          baseGroups = c("Esri.WorldTopoMap", "Satellite"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) %>%
        leaflet::addScaleBar(
          options = leaflet::scaleBarOptions(imperial = FALSE)
        ) %>%
        leaflet::setView(lng = center$lon, lat = center$lat, zoom = center$zoom)

      if (!is.null(primary)) {
        m <- m %>%
          leaflet::addCircleMarkers(
            lng = primary$lon,
            lat = primary$lat,
            radius = 7,
            color = "#b42318",
            fillOpacity = 0.9,
            group = "primary_point",
            label = primary$name
          )
      }

      if (!is.null(sel)) {
        m <- m %>%
          leaflet::addCircleMarkers(
            lng = sel$lon,
            lat = sel$lat,
            radius = 6,
            color = "#007B8A",
            fillOpacity = 0.9,
            group = "selected_point"
          )
      }

      m
    }) %>%
      bindEvent(input$open_map)

    draw_selected_point <- function() {
      sel <- isolate(map_selection())
      if (is.null(sel)) {
        return(invisible(NULL))
      }

      leaflet::leafletProxy(ns("location_map"), session = session) %>%
        leaflet::clearGroup("selected_point") %>%
        leaflet::addCircleMarkers(
          lng = sel$lon,
          lat = sel$lat,
          radius = 6,
          color = "#007B8A",
          fillOpacity = 0.9,
          group = "selected_point"
        )
    }

    draw_primary_point <- function() {
      primary <- isolate(primary_location())
      proxy <- leaflet::leafletProxy(ns("location_map"), session = session) %>%
        leaflet::clearGroup("primary_point")
      if (is.null(primary)) {
        return(invisible(NULL))
      }
      proxy %>%
        leaflet::addCircleMarkers(
          lng = primary$lon,
          lat = primary$lat,
          radius = 7,
          color = "#b42318",
          fillOpacity = 0.9,
          group = "primary_point",
          label = primary$name
        )
    }

    output$map_zoom_note <- renderUI({
      zoom <- input$location_map_zoom
      if (is.null(zoom)) {
        return(NULL)
      }
      if (zoom < 14) {
        div(
          style = "color: #b42318; font-size: 14px; margin-top: 8px;",
          "Zoom in to level 14 or higher to save this location."
        )
      } else {
        div(
          style = "color: #027a48; font-size: 14px; margin-top: 8px;",
          "Zoom level is sufficient to save."
        )
      }
    })

    observeEvent(input$open_map, {
      current_lat <- input$lat
      current_lon <- input$lon
      primary <- primary_location()

      if (isTruthy(current_lat) && isTruthy(current_lon)) {
        map_center(list(lat = current_lat, lon = current_lon, zoom = 12))
        map_selection(list(lat = current_lat, lon = current_lon))
      } else if (!is.null(primary)) {
        map_center(list(lat = primary$lat, lon = primary$lon, zoom = 12))
        map_selection(NULL)
      } else {
        map_center(list(lat = 64.0, lon = -135.0, zoom = 4))
        map_selection(NULL)
      }

      showModal(modalDialog(
        title = "Select sub-location on map",
        leaflet::leafletOutput(ns("location_map"), height = "400px"),
        uiOutput(ns("map_zoom_note")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_location_map"), "Use selected location")
        ),
        size = "l",
        easyClose = TRUE
      ))
    })

    observeEvent(input$location_map_click, {
      click <- input$location_map_click
      map_selection(list(lat = click$lat, lon = click$lng))
      draw_selected_point()
    })

    observeEvent(primary_location(), {
      if (is.null(input$open_map) || input$open_map == 0) {
        return()
      }
      draw_primary_point()
    })

    observeEvent(input$location_map_zoom, {
      req(input$location_map_zoom)
      if (input$location_map_zoom < 14) {
        shinyjs::disable("save_location_map")
      } else {
        shinyjs::enable("save_location_map")
      }
    })

    observeEvent(input$save_location_map, {
      if (is.null(input$location_map_zoom) || input$location_map_zoom < 14) {
        showNotification(
          "Zoom in to level 14 or higher before saving.",
          type = "warning"
        )
        return()
      }
      selection <- map_selection()
      if (is.null(selection)) {
        showNotification("Click a point on the map to select a location.")
        return()
      }
      updateNumericInput(session, "lat", value = selection$lat)
      updateNumericInput(session, "lon", value = selection$lon)
      removeModal()
    })

    ### Observe the share_with selectizeInput for new user groups ##############################
    observeEvent(
      input$share_with,
      {
        if (
          length(input$share_with) > 1 & 'public_reader' %in% input$share_with
        ) {
          showModal(modalDialog(
            "If public_reader is selected it must be the only group selected.",
            easyClose = TRUE
          ))
          updateSelectizeInput(
            session,
            "share_with",
            selected = "public_reader"
          )
        }

        if (
          input$share_with[length(input$share_with)] %in%
            moduleData$users$role_name ||
            nchar(input$share_with[length(input$share_with)]) == 0
        ) {
          return()
        }
        showModal(modalDialog(
          "Ask a database admin to create a new user or user group"
        ))
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

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
        tryCatch(
          {
            # Check each field to see if it's been modified; if so, update the DB entry by targeting the location_id and appropriate column name
            # Changes to the location english sub_location_name
            if (
              input$subloc_name !=
                moduleData$exist_sub_locs[
                  which(
                    moduleData$exist_sub_locs$sub_location_id ==
                      selected_sub_loc()
                  ),
                  "sub_location_name"
                ]
            ) {
              DBI::dbExecute(
                session$userData$AquaCache,
                glue::glue_sql(
                  "UPDATE sub_locations SET sub_location_name = {input$subloc_name} WHERE sub_location_id = {selected_sub_loc()};",
                  .con = session$userData$AquaCache
                )
              )
            }

            # Changes to the location french sub_location_name
            if (
              input$subloc_name_fr !=
                moduleData$exist_sub_locs[
                  which(
                    moduleData$exist_sub_locs$sub_location_id ==
                      selected_sub_loc()
                  ),
                  "sub_location_name_fr"
                ]
            ) {
              DBI::dbExecute(
                session$userData$AquaCache,
                glue::glue_sql(
                  "UPDATE sub_locations SET sub_location_name_fr = {input$subloc_name_fr} WHERE sub_location_id = {selected_sub_loc()};",
                  .con = session$userData$AquaCache
                )
              )
            }

            # Changes to coordinates
            if (
              input$lat !=
                moduleData$exist_sub_locs[
                  which(
                    moduleData$exist_sub_locs$sub_location_id ==
                      selected_sub_loc()
                  ),
                  "latitude"
                ]
            ) {
              DBI::dbExecute(
                session$userData$AquaCache,
                sprintf(
                  "UPDATE sub_locations SET latitude = %f WHERE sub_location_id = %d",
                  input$lat,
                  selected_sub_loc()
                )
              )
            }
            if (
              input$lon !=
                moduleData$exist_sub_locs[
                  which(
                    moduleData$exist_sub_locs$sub_location_id ==
                      selected_sub_loc()
                  ),
                  "longitude"
                ]
            ) {
              DBI::dbExecute(
                session$userData$AquaCache,
                sprintf(
                  "UPDATE sub_locations SET longitude = %f WHERE sub_location_id = %d",
                  input$lon,
                  selected_sub_loc()
                )
              )
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
              if (
                !is.na(moduleData$exist_sub_locs[
                  which(
                    moduleData$exist_sub_locs$sub_location_id ==
                      selected_sub_loc()
                  ),
                  "note"
                ])
              ) {
                # There might not be a note already
                if (
                  input$subloc_note !=
                    moduleData$exist_sub_locs[
                      which(
                        moduleData$exist_sub_locs$sub_location_id ==
                          selected_sub_loc()
                      ),
                      "note"
                    ]
                ) {
                  DBI::dbExecute(
                    session$userData$AquaCache,
                    sprintf(
                      "UPDATE sub_locations SET note = '%s' WHERE sub_location_id = %d",
                      input$subloc_note,
                      selected_sub_loc()
                    )
                  )
                }
              } else {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  sprintf(
                    "UPDATE sub_locations SET note = '%s' WHERE sub_location_id = %d",
                    input$subloc_note,
                    selected_sub_loc()
                  )
                )
              }
            }

            # Show a notification that the location was updated
            showNotification("Location updated successfully", type = "message")
            # Commit the transaction
            DBI::dbCommit(session$userData$AquaCache)

            # Update the moduleData reactiveValues
            getModuleData() # This should trigger an update to the table
          },
          error = function(e) {
            # If there was an error, rollback the transaction
            DBI::dbRollback(session$userData$AquaCache)
            showModal(modalDialog(
              "Error updating location: ",
              e$message
            ))
          }
        )

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
        if (
          input$subloc_name %in% moduleData$exist_sub_locs$sub_location_name
        ) {
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
        if (
          input$subloc_name_fr %in%
            moduleData$exist_sub_locs$sub_location_name_fr
        ) {
          showModal(modalDialog(
            "Location name (French) already exists",
            easyClose = TRUE
          ))
          return()
        }
      }

      tryCatch(
        {
          DBI::dbExecute(
            session$userData$AquaCache,
            "INSERT INTO sub_locations (location_id, sub_location_name, sub_location_name_fr, latitude, longitude, share_with, note) VALUES ($1, $2, $3, $4, $5, $6, $7);",
            params = list(
              input$location,
              input$subloc_name,
              input$subloc_name_fr,
              input$lat,
              input$lon,
              paste0(
                "{",
                paste(input$share_with, collapse = ", "),
                "}"
              ),
              if (isTruthy(input$subloc_note)) input$subloc_note else NA
            )
          )

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
          updateSelectizeInput(
            session,
            "share_with",
            selected = "public_reader"
          )
          updateTextInput(session, "subloc_note", value = character(0))
        },
        error = function(e) {
          showModal(modalDialog(
            "Error adding sub_location: ",
            e$message
          ))
        }
      )
    })
  }) # End of moduleServer
}
