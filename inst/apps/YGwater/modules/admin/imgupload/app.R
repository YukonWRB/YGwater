# Load necessary libraries
# library(shiny)
# library(leaflet)
# library(jpeg)
# library(exifr)
# library(dplyr)
# library(DT)
# library(YGwater)
# library(AquaCache)

# Define the UI layout

imguploadUI <- function(id) {
      # Define a dictionary to map checkbox choices to tags
    tag_list = list("Head", "Toe", "Smooth ice", "In flood")
    share_list = list("Private", "Public")
    # Define a list of UTC timezone corrections
    tz_corrections = list(-12, -11, -10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)


    ns <- NS(id)
    fluidPage(
      br(),
      mainPanel(
        fluidRow(
          column(width = 6,
            fileInput(ns("files"), "Select image files (ctrl+A to select all)", multiple = TRUE)
          ),
          column(width = 6,
            br(), br(), br(),
            fluidRow(
              div(style = "float: left;", uiOutput(ns("edit_button"))),
              div(style = "float: left; margin-left: 10px;", uiOutput(ns("toggle_locations_button"))),
              div(style = "float: right;", actionButton(ns("clear_pos"), "Clear coordinate selection"))
            )
          )
        ),
        fluidRow(
          column(width = 6,
            imageOutput(ns("imgprev"), width = "100%", height = "auto"),
            textOutput(ns("image_index"))
          ),
          column(width = 6,
            leafletOutput(ns("map"), width = "100%"),
            br()
          )
        ),
        fluidRow(
          column(
            width = 6,
            actionButton(ns("select_all"), "Select All"),
            actionButton(ns("clear_selection"), "Clear Selection"),
            br(),
            selectizeInput(ns("image_tags"), "Select tags:", choices = tag_list, 
                           multiple = TRUE, width = "100%", options = list(create = TRUE)),
            fluidRow(
              column(width = 6, selectizeInput(ns("image_location"), "Select location:", 
                                               choices = "Placeholder", multiple = FALSE, 
                                               width = "100%", options = list(create = FALSE, placeholder = "Select location"))),
              column(width = 3, numericInput(ns("image_latitude"), "Latitude:", value = NULL, width = "100%", step = 0.1)),
              column(width = 3, numericInput(ns("image_longitude"), "Longitude:", value = NULL, width = "100%", step = 0.1))
            ),
            fluidRow(
              column(width = 8, selectInput(ns("share_tag"), "Select visibility:", choices = share_list, multiple = FALSE)),
              column(width = 4, selectInput(ns("tz_correction"), "UTC offset:", choices = tz_corrections, multiple = FALSE, selected = -7))
            ),
            textAreaInput(ns("notes"), "Notes:", value = "", width = "100%", height = "100px"),
            div(style = "float: right;", actionButton(ns("apply"), "Apply"))
          ),
          column(
            width = 6,
            DT::dataTableOutput(ns("table")),
            actionButton(ns("upload_to_ac"), "Upload to AquaCache", style = "background-color: #007BFF; color: white;")
          )
        )
      )
    )
}

config <<- list(
  dbName = "aquacache",
  dbHost = Sys.getenv("aquacacheHost"),
  dbPort = Sys.getenv("aquacachePort"),
  dbUser = Sys.getenv("aquacacheAdminUser"),
  dbPass = Sys.getenv("aquacacheAdminPass")
)


# Increase the maximum upload size to 30MB
options(shiny.maxRequestSize = 30 * 1024^2)





imgupload() {

  ns <- session$ns
# Define the server logic
server <- function(input, output, session) {

  session$userData$config <- config

  session$userData$AquaCache <- AquaConnect(name = config$dbName,
                                            host = config$dbHost,
                                            port = config$dbPort,
                                            username = config$dbUser,
                                            password = config$dbPass,
                                            silent = TRUE)

  # issue warning if user does not have write privileges
  check <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT has_table_privilege(current_user, 'files.images', 'INSERT') AS can_insert")

  if (!check$can_insert) {
    showModal(modalDialog(
      title = "Insufficient Privileges",
      "You do not have the necessary privileges to insert data into this database.",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close")
      )
    ))
    shinyjs::disable("upload_to_ac")
  }



  locations = DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM public.locations")
  
  # Reactive value to track the current image index
  imgid <- reactiveVal(1)
  
  # Initialize reactive values to track the state of the app
  toggles <- reactiveValues(locations = FALSE, mapclicked = TRUE, edit = FALSE)
  
  # Initiaze dataframe to store exif data and updated attributes
  dat <- reactiveValues(df = data.frame())
  
  # Render placeholder image
  output$imgprev <- renderImage({
    filename <- normalizePath("welcome.jpg")
    list(src = filename, contentType = 'image/jpeg', width = "100%", height = "100%")
  }, deleteFile = FALSE)

  # Render an empty leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "World Aerial") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
      addLayersControl(
      baseGroups = c("World Aerial", "World Imagery"),
      options = layersControlOptions(collapsed = FALSE)
      ) %>%
      setView(lng = DEFAULT_COORDS[1], lat = DEFAULT_COORDS[2], zoom = 4)
    })

  # Observe file input and update the data table
  observeEvent(input$files, {
    files <- input$files
    if (is.null(files)) {
      return(NULL)
    }

    dat$df <- read_exif(files$datapath)
    dat$df$FileName <- files$name

    # Grab only necessary attributes and add placeholders for new ones
    dat$df = preprocess_exif(dat$df)

    # Check for null latitude or longitude values; issue a warning if any are found.
    dat$df$Latitude[dat$df$Latitude == 0] <- NA
    dat$df$Longitude[dat$df$Longitude == 0] <- NA
    dat$df$Altitude[dat$df$Longitude == 0] <- NA

      # Once the images are loaded into the app, render the GUI elements
    renderDataTable()
    renderImagePlot()
    renderLeafletMap()
    renderImageIndex()
    })



  # Render the data table
  renderDataTable <- function() {
    output$table <- DT::renderDataTable({
      datatable(
        dat$df %>%
          select(-Sourcefile) %>%
          mutate(across(where(is.numeric), ~ round(., 3))) %>%
          mutate(Datetime = format(as.POSIXct(Datetime, format="%Y:%m:%d %H:%M:%S"), "%d-%b-%Y %H:%M")),
        selection = list(mode = "multiple", target = "row"),
        editable = TRUE,
        options = list(
          dom = 't',
          paging = FALSE,
          ordering = FALSE,
          scrollX = TRUE,
          scrollY = "400px",
          autoWidth = TRUE
        )
      )
    })
  }

  # Render the toggle buttons here, which change colour based on the state of the app
  output$edit_button <- renderUI({
    actionButton(ns("edit"), "Manual coordinate selection", icon = icon("map-location-dot"), 
           style = if (toggles$edit) "background-color: #007BFF; color: white;" else "background-color: white; color: black;")
  })

  output$toggle_locations_button <- renderUI({
    actionButton(ns("toggle_locations"), "Toggle locations", 
           style = if (toggles$locations) "background-color: #007BFF; color: white;" else "background-color: white; color: black;")
  })


  # Observe 'Select coordinates' button click and toggle edit mode
  observeEvent(input$edit, {
    toggles$edit <- !toggles$edit
    if (toggles$edit) {
      showNotification("Map interactivity enabled.", type = "message", duration = 3)
    } else {
      showNotification("Map interactivity disabled.", type = "message", duration = 3)
    }
  })

  # Add the 'current' lat-lon values (from the Latitute, Longitude input boxes) to the map
  observeEvent(list(input$image_latitude, input$image_longitude), {
    if (is_valid_latlon(input$image_latitude, input$image_longitude)) {
      clearLinesFromMap()
      leafletProxy("map") %>%
        clearGroup("manual_marker") %>%
        addCircleMarkers(
          lng = input$image_longitude,
          lat = input$image_latitude,
          radius = 5,
          color = color_list$click,
          fill = TRUE,
          fillOpacity = 0.8,
          popup = paste("Lat:", round(input$image_latitude, 3), "Lng:", round(input$image_longitude, 3)),
          group = "manual_marker"
        )

        # Add lines connecting 'current' lat lon with selected images (from the data table)
        addLinesToMap()
    }
  }, ignoreInit = TRUE)



  # Toggle aquacache locations on the map
  observeEvent(input$toggle_locations, {
    if (toggles$locations) {
        leafletProxy("map") %>% clearGroup("locations")
    }
    else {
        addLocationsToMap()
    }
    toggles$locations <- !toggles$locations
  })
  

  # Observe changes in the data table and re-render the map
  observeEvent(input$table_cell_edit, {
    info <- input$table_cell_edit
    if (info$col %in% c("Latitude", "Longitude")) {
      dat$df[info$row, info$col] <- as.numeric(info$value)
      renderLeafletMap()
    }
  })

  # Observe select all button click and select all rows in the data table
  observeEvent(input$select_all, {
    proxy <- dataTableProxy("table")
    selectRows(proxy, seq_len(nrow(dat$df)))
  })

  # Observe clear selection button click and clear all selected rows in the data table
  observeEvent(input$clear_selection, {
    proxy <- dataTableProxy("table")
    selectRows(proxy, NULL)

    # Update the map to reflect deselection
    updateMapView()
    leafletProxy(mapId = "map") %>% clearGroup("photos")
    addCirclesToMap(selection = seq_len(nrow(dat$df)), color=color_list$unselected)
    clearLinesFromMap()
  })

  # Update the 'locations' list based on the data loaded from Aquacache
  observe({
    updateSelectizeInput(session, "image_location", choices = locations$name, selected = "Placeholder", options = list(create = FALSE, placeholder="Select location", maxItems = 1))
  })

  # Observe row selection in the data table and update the imgid
  observeEvent(input$table_rows_selected, ignoreNULL = FALSE, {
    # if table selection changes, reset the image coordinateds
    updateNumericInput(session, "image_latitude", value = "")
    updateNumericInput(session, "image_longitude", value = "")

    if (is.null(input$table_rows_selected)) {
      selection <- 0
    } else {
      selection <- input$table_rows_selected
    }

    # update lines to reflect selection (showing georeference corrections)
    clearLinesFromMap()
    if (length(selection) > 0) {
      addLinesToMap()
    }

    # update the displayed image upon each new row selection
    if (length(selection) > 1) {
      imgid(selection[length(selection)])
    } else {
      imgid(1)
    }

    # case for no selection
    #if (is.null(selection)) {
    #  updateMapView()
    #  leafletProxy(mapId = "map") %>% clearGroup("photos")
    #  addCirclesToMap(selection = seq_len(nrow(dat$df)), color = color_list$unselected)
    #}
  })


  # Observe the upload button and upload data to AquaCache
  observeEvent(input$upload_to_ac, {
    showModal(modalDialog(
      title = "Confirm Upload",
      "Are you sure you want to upload the images to AquaCache?",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("confirm_upload"), "Confirm")
      )
    ))
  })

  # Validate the uploaded dataframe and upload to AquaCache
  observeEvent(input$confirm_upload, {
    removeModal()
    dat$df <- standardizeDataFrame(dat$df)
    validation_result <- validateDataFrame(dat$df)
    if (!validation_result$success) {
      showNotification(validation_result$message, type = "error", duration = 5)
    }
    uploadAquacache$invoke(df=dat$df, config=session$userData$config)
  })

  uploadAquacache <- ExtendedTask$new(
    function(df, config) {
      promises::future_promise({
        
        con <- AquaConnect(name = config$dbName,
                                          host = config$dbHost,
                                          port = config$dbPort,
                                          username = config$dbUser,
                                          password = config$dbPass,
                                          silent = TRUE)
        on.exit(DBI::dbDisconnect(con))

        # for converting app text to AquaCache reader names
        for (i in seq_len(nrow(df))) {
          
            success <- tryCatch({
            insertACImage(
              object = df$Sourcefile[i],
              datetime = df$Datetime[i],
              latitude = df$Latitude[i],
              longitude = df$Longitude[i],
              elevation_msl = df$Altitude[i],
              azimuth_true = df$Azimuth[i],
              share_with = df$Visibility[i],
              con = con,
              location = if (df$Location[i] == "") NULL else df$Location[i],
              tags = if (length(df$Tags[[i]]) == 0) NULL else df$Tags[[i]],
              description = if (df$Notes[i] == "") NULL else df$Notes[i],
              owner = 2
            )
            }, error = function(e) {
            print(paste("Error uploading image:", e$message))
            FALSE
            })
          print(success)
          if (!success) {showNotification("Error uploading image to AquaCache.", type = "error", duration = 5)}
          if (success) {showNotification("Images uploaded to AquaCache.", type = "message", duration = 5)}
        }
      })
    }
  )

# standardize the dataframe to match AquaCache's format
standardizeDataFrame <- function(df){
  reader_list <- c("Private" = "yg_reader", "Public" = "public_reader")
  df$Sourcefile <- as.character(df$Sourcefile)
  df$Tags <- as.character(df$Tags)
  df$Visibility <- reader_list[df$Visibility]
  df$Datetime <- as.POSIXct(df$Datetime, format="%Y:%m:%d %H:%M:%S", tz="UTC")
  df$Datetime <- df$Datetime - as.difftime(df$UTC, units = "hours")
  return(df)
  }

# verify that the dataframe is valid for upload to AquaCache
validateDataFrame <- function(df) {
  if (any(is.na(df$Latitude)) || any(is.na(df$Longitude))) {
    return(list(success = FALSE, message = "Error: One or more images do not contain a georeference. Please add locations using the drop-down menu or input coordinates manually."))
  }
  return(list(success = TRUE, message = "Validation successful."))
  }

  # keep track of whether the location was queried from AquaCache, or set by the user
  # if the cooridnates are changed by the user, or changes via map click, reset the location selection
  queried_laton <- reactiveValues(lat=0.0, lon=0.0)

  observeEvent(input$image_location, {
      # if the location is default, don't update latlon
      if (input$image_location == "Placeholder" | input$image_location == "") {
        return()
      }

      lat <- locations[locations$name == input$image_location, "latitude"]
      lon <- locations[locations$name == input$image_location, "longitude"]
      
      updateNumericInput(session, "image_latitude", value = lat)
      updateNumericInput(session, "image_longitude", value = lon)

      queried_laton$lat <- lat
      queried_laton$lon <- lon

      # update the map view to the selected location
      leafletProxy("map") %>%
        setView(lng = lon, lat = lat, zoom = 4)
  }, ignoreInit = TRUE)
  

  # if the user manually changes the lat lon, reset the location
  observeEvent(list(input$image_latitude, input$image_longitude), {
    
    if (is.na(input$image_latitude) | is.na(input$image_longitude)) {
      updateSelectInput(session, "image_location", selected = "")
    }

    else if ((queried_laton$lat != input$image_latitude) | (queried_laton$lon != input$image_longitude)) {
      updateSelectInput(session, "image_location", selected = "")
    }

  }, ignoreInit = TRUE)

  # button used to apply changes in attribute values to the table; this is safer than modifying the table directly
  observeEvent(input$apply, {
    if (length(input$table_rows_selected) > 0) {
      for (row in input$table_rows_selected) {
        
        # only write attributes that are not set to their default values (typically empty)
        # this way, the user can leave attributes blank, such as lat-lon, without worrying about overwriting them
        # while you could write everything to the table, since the table already has default values, this created a problem when you want to change multiple images at once
        # since multiple images won't have the same 'default' lat-lons
        if (input$notes != "") {dat$df$Notes[row] <- input$notes}
        if (!is.null(input$image_tags)) {dat$df$Tags[row] <- paste(input$image_tags, collapse = ", ")}

        # check whether the latitude and longitude are valid
        if (!is_valid_latlon(input$image_latitude, input$image_longitude) && !is.na(input$image_latitude) && !is.na(input$image_longitude)) {
          showModal(modalDialog(
            title = "Invalid Coordinates",
            "The latitude and/or longitude values are not valid. Please enter valid coordinates.",
            easyClose = TRUE,
            footer = tagList(
              modalButton("Close")
            )
          ))
          
          return()
        }
        if (!is.na(input$image_latitude) && !is.na(input$image_longitude)){
          dat$df$Latitude[row] <- input$image_latitude
          dat$df$Longitude[row] <- input$image_longitude
          dat$df$Location[input$table_rows_selected] <- input$image_location
        }
        if (input$share_tag != "Private") {dat$df$Visibility[row] <- input$share_tag}
        if (input$tz_correction != -7) {dat$df$UTC[row] <- input$tz_correction}
      }
      
      # Reset all of the input fields after applying changes and re-render the table/map
      updateTextAreaInput(session, "notes", value = "")
      updateSelectizeInput(session, "image_tags", selected = "Placeholder")
      updateNumericInput(session, "image_latitude", value = NA)
      updateNumericInput(session, "image_longitude", value = NA)
      updateSelectInput(session, "image_location", selected = "Placeholder")
      updateSelectInput(session, "share_tag", selected = "Private")
      updateSelectInput(session, "tz_correction", selected = -7)
      renderDataTable()
      renderLeafletMap()

    # warning for no images selected case
    } else {
      showModal(modalDialog(
        title = "No Rows Selected",
        "Please select row(s) in the data table to save notes.",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Close")
        )
      ))
    }
  })

  # simple button to clear objects from map
  observeEvent(input$clear_pos, {
    leafletProxy("map") %>%
      clearGroup("manual_marker") %>%
      clearGroup("click_line")
    toggles$mapclicked <- FALSE
  })

  # Set the view of the map to the current image's coordinates on button press
  updateMapView <- function() {
    # If no rows are selected, set the view to the centroid of all points
      if (is.null(input$table_rows_selected)) {
      selection <- seq_len(nrow(dat$df))
    } else {
      selection <- input$table_rows_selected
    }

    # if none of the selected images have valid coordinates, set view to entire table
    if (all(!is_valid_latlon(dat$df$Latitude[selection], dat$df$Longitude[selection]))) {
      selection <- seq_len(nrow(dat$df))
    }
    
    # if nothing is selected, set view to entire table
    if (length(selection) == 0) {
      selection <- seq_len(nrow(dat$df))
    }

    # if the map is clicked, include the clicked point in the centroid calculation
    if (toggles$edit && !is.null(input$map_click) && toggles$mapclicked) {
      # Calculate the centroid of selection
      centroid_lat <- mean(c(dat$df$Latitude[selection], input$map_click$lat), na.rm = TRUE)
      centroid_lon <- mean(c(dat$df$Longitude[selection], input$map_click$lng), na.rm = TRUE)
    } 
    else {
      # Calculate the centroid of selection
      centroid_lat <- mean(dat$df$Latitude[selection], na.rm = TRUE)
      centroid_lon <- mean(dat$df$Longitude[selection], na.rm = TRUE)
    }

      # Handle case where centroid is NA
      if (is.na(centroid_lat) || is.na(centroid_lon)) {
        centroid_lat <- DEFAULT_COORDS[1]
        centroid_lon <- DEFAULT_COORDS[2]
      }

    # Infer zoom level based on the spread of the points
    zoom <- infer_zoom_level(dat$df$Latitude[selection], dat$df$Longitude[selection])

    # Update the map view
    leafletProxy(mapId = "map") %>%
      setView(lng = centroid_lon, lat = centroid_lat, zoom = zoom)
  }

  # Render the image plot
  # This output will update automatically based on the imgid() React var
  renderImagePlot <- function() {
    output$imgprev <- renderImage({
      filename <- normalizePath(dat$df$Sourcefile[imgid()])
      img <- readJPEG(filename, native = TRUE)

      # this is a sort of janky way to handle vertical images
      img_dims <- dim(img)
      if (img_dims[1] > img_dims[2]) {
        list(src = filename, contentType = 'image/jpeg', width = "50%", height = "auto")
      } else {
        list(src = filename, contentType = 'image/jpeg', width = "100%", height = "auto")
      }
    }, deleteFile = FALSE)
  }

  # print image n/x
  renderImageIndex <- function() {
    output$image_index <- renderText({
      paste("Image", imgid(), "of", nrow(dat$df))
    })
  }
  # Function to render the leaflet map
  renderLeafletMap <- function() {

    # only render images with valid latlon
    valid_data <- dat$df %>% filter(is_valid_latlon(Latitude, Longitude))

    # if no images with valid latlon, exit map rendering function
    if (nrow(valid_data) == 0) {
      return(NULL)
    }

    output$map <- renderLeaflet({
      leaflet(data = valid_data) %>%
        addProviderTiles(providers$Esri.WorldImagery) %>%
        addLayersControl(
          baseGroups = c("World Topo Map","World Imagery"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "World Aerial") %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "World Topo Map") %>%
        addCircleMarkers(
          lng = valid_data$Longitude,
          lat = valid_data$Latitude,
          radius = 2,
          color = color_list$unselected,
          fill = TRUE,
          fillOpacity = 0.5,
          popup = ~paste("Filename:", Filename, "<br>",
                         "Date/Time:", Datetime, "<br>",
                         "Latitude:", round(Latitude, 5), "<br>",
                         "Longitude:", round(Longitude, 5), "<br>",
                         "Altitude:", round(Altitude, 2), "m")
        ) %>%
        addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE, maxWidth = 100, metric = TRUE, updateWhenIdle = TRUE)) %>%
        htmlwidgets::onRender("function(el, x) {
          var credits = el.querySelector('.leaflet-control-attribution');
          if (credits) {
            credits.style.fontSize = '4px';
          }
        }")
    })

    #addLocationsToMap()
  }


  # Observe clicks on existing markers and update the data table selection based on the clicked marker


  # observeEvent(input$map_marker_click, {
  #   click <- input$map_marker_click
  #   if (!is.null(click)) {
  #     # Find the index of the clicked marker in the data table

  #     # CURRENTLY THERE'S A BUG WHERE AN EDITED POINT LOCATION CANNOT BE SELECTED VIA CLICK
  #     # WHILE THE LAT LONS SEEM TO MATCH UP, THE WHICH FUNCTION RETURNS INTEGER(0)
  #     # FOR NOW, THIS FUNCTIONALITY IS DISABLED (BY ADDING A NULL CASE)
  #     clicked_index <- which(
  #       dat$df$Longitude == click$lng & dat$df$Latitude == click$lat
  #     )

  #     if (length(clicked_index) == 0) {
  #       clicked_index <- NULL
  #     }

  #     if (!is.null(clicked_index)) {
  #       if (clicked_index %in% input$table_rows_selected) {
  #         selected_rows <- setdiff(input$table_rows_selected, clicked_index)
  #       } else {
  #         selected_rows <- c(input$table_rows_selected, clicked_index)
  #       }
  #       proxy <- dataTableProxy("table")
  #       selectRows(proxy, selected_rows)
  #     }
  #   }
  # })

  
  # Generic-ish function to add circles to the map based on selection
  addCirclesToMap <- function(selection, color, radius=2.5) {
    leafletProxy(mapId = "map") %>%
      addCircleMarkers(
        lng = dat$df$Longitude[selection],
        lat = dat$df$Latitude[selection],
        radius = radius,
        color = color,
        fillColor = color,
        fill = TRUE,
        fillOpacity = 1,
        group = "photos"
      )
  }

  # add 'locations' table from AquaCache to the map
  addLocationsToMap <- function() {
    leafletProxy(mapId = "map") %>%
      addCircleMarkers(
        lng = locations$longitude,
        lat = locations$latitude,
        radius = 5,
        color = color_list$locations,
        opacity = 1,
        fillColor = color_list$locations,
        weight = 2,
        fill = FALSE,
        fillOpacity = 1,
        popup = locations$name,
        group = "locations"
      )
  }


  # Observe row selection in the data table and update the map marker colours
  observeEvent(input$table_rows_selected, {
    updateMapView()
    leafletProxy(mapId = "map") %>% clearGroup("photos")
    if (length(input$table_rows_selected) > 0) {
      addCirclesToMap(selection = input$table_rows_selected, color = color_list$selected, radius=5)
      addCirclesToMap(selection = -input$table_rows_selected, color = color_list$unselected)
    } else {
      addCirclesToMap(selection = seq_len(nrow(dat$df)), color = color_list$unselected)
    }

  },
  ignoreNULL = FALSE,
  ignoreInit = TRUE
  )

  # Observe map clicks and set mapclicked to TRUE
  observeEvent(input$map_click, {
    toggles$mapclicked <- TRUE
  })

  # Observe map clicks, add marker on click, lines from click to selection
  observeEvent(input$map_click, {
    if (toggles$edit == TRUE) {
    click <- input$map_click
    if (!is.null(click)) {
      updateNumericInput(session, "image_latitude", value = click$lat)
      updateNumericInput(session, "image_longitude", value = click$lng)
    }
      # Add a line between the clicked point and the selected point
    }
  })

  # Add lines that connect GUI image coords to table seleciton coords
  addLinesToMap <- function() {
      if (length(input$table_rows_selected) > 0) {
        selected_rows <- input$table_rows_selected
        for (row in selected_rows) {
          selected_lat <- dat$df$Latitude[row]
          selected_lon <- dat$df$Longitude[row]
          
          leafletProxy("map") %>%
            addPolylines(
                lng = c(selected_lon, input$map_click$lng),
                lat = c(selected_lat, input$map_click$lat),
              color = color_list$click,
              weight = 2,
              dashArray = "5, 10",
              group = "click_line"
            )
        }

    }
  }

  # Function to clear lines from the map
  clearLinesFromMap <- function() {
    leafletProxy("map") %>% clearGroup("click_line")
  }

  # Confirmation action for modal popup triggered by multiple simultaneous location updates
  observeEvent(input$confirm_update, {
    removeModal()

    # update the coordinates in the data table
    dat$df$Latitude[input$table_rows_selected] <- input$map_click$lat
    dat$df$Longitude[input$table_rows_selected] <- input$map_click$lng
    renderDataTable()
    renderLeafletMap()
  })
  
  # Cancel action for modal popup triggered by multiple simultaneous location updates
  observeEvent(input$Cancel, {
    removeModal()
  })
  
# end of server






# Define a named list of colors
color_list <- list(
  "selected" = "cyan",
  "unselected" = "black",
  "click" = "green",
  "other" = "magenta",
  "locations" = "orange"
)

if (Sys.info()["sysname"] == "Windows" | interactive()) {
} else {
  future::plan("multicore")
}

# Default coordinates to repositon the map around Whitehorse
DEFAULT_COORDS = c(-135.0568, 60.7212)

# Function to convert latitude and longitude to meters
latlon_to_meters <- function(lat1, lon1, lat2, lon2) {
  R <- 6378137 # Radius of the Earth in meters
  dLat <- (lat2 - lat1) * pi / 180
  dLon <- (lon2 - lon1) * pi / 180
  a <- sin(dLat / 2) * sin(dLat / 2) +
    cos(lat1 * pi / 180) * cos(lat2 * pi / 180) *
    sin(dLon / 2) * sin(dLon / 2)
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  d <- R * c
  return(d)
}


# Function to check if latitude and longitude are valid
is_valid_latlon <- function(lat, lon) {
  valid_lat <- !is.na(lat) & !is.null(lat) & lat >= -90 & lat <= 90
  valid_lon <- !is.na(lon) & !is.null(lat) & lon >= -180 & lon <= 180
  return(valid_lat & valid_lon)
}

# Function to infer zoom level based on latitude and longitude arrays
infer_zoom_level <- function(latitudes, longitudes) {
  if (length(latitudes) == 0 || length(longitudes) == 0) {
    return(1) # Default zoom level if no points are provided
  }
  
  # Calculate the bounding box
  min_lat <- min(latitudes, na.rm = TRUE)
  max_lat <- max(latitudes, na.rm = TRUE)
  min_lon <- min(longitudes, na.rm = TRUE)
  max_lon <- max(longitudes, na.rm = TRUE)
  
  # Calculate the zoom level based on the bounding box size
  lat_diff <- max_lat - min_lat
  lon_diff <- max_lon - min_lon
  max_diff <- max(lat_diff, lon_diff)
  
  # Define zoom levels based on the maximum difference
  if (max_diff < 0.01) {
    zoom <- 10
  } else if (max_diff < 0.1) {
    zoom <- 10
  } else {
    zoom <- 5
  }
  return(zoom)
}

# Function to preprocess EXIF data
preprocess_exif <- function(dat) {
  stopifnot("FileName" %in% colnames(dat))
  stopifnot("GPSLatitude" %in% colnames(dat))
  stopifnot("GPSLongitude" %in% colnames(dat))
  stopifnot("GPSAltitude" %in% colnames(dat))
  stopifnot("DateTimeOriginal" %in% colnames(dat))

  dat_out <- dat %>%
    select(SourceFile, FileName, DateTimeOriginal, GPSLatitude, GPSLongitude, GPSAltitude, GPSImgDirection) %>%
    rename(Sourcefile = SourceFile, Filename = FileName, Latitude = GPSLatitude, Longitude = GPSLongitude, Altitude = GPSAltitude, Datetime = DateTimeOriginal, Azimuth = GPSImgDirection) %>%
    mutate(Tags = list(character(0)), Notes = "", Visibility = "Private", UTC = -7, Location = "")
  return(dat_out)
}

}

} # end imgupload function

# Run the Shiny app


shinyApp(ui = ui, server = server)