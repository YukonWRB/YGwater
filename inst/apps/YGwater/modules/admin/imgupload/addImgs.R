# Define the UI layout


# TODO: once aquacache is updated with 'visit type', update insertACimage call with visit type
# TODO: pull tag_lists from AquaCache database, so that it is not hardcoded here

addImgsUI <- function(id) {
  # Define a dictionary to map checkbox choices to tags
  
  visit_list <- list("Ice observation flight", "Audit", "Field visit", "Sampling event", "Other")
  share_list <- list("Private", "Public")
  
  # Define a list of UTC timezone corrections
  tz_corrections <- list(-12, -11, -10, -9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
  
  ns <- NS(id)
  
  fluidPage(
    br(),
    fluidRow(
      column(
        width = 6,
        fileInput(ns("files"), label = NULL, multiple = TRUE, accept = c("image/jpeg", "image/png", "image/tiff")) |> tooltip("Select image files (ctrl+A to select all)"),
      ),
      column(
        width = 6,
        fluidPage(
          div(class = "d-inline-block", uiOutput(ns("edit_button"))),
          div(class = "d-inline-block", uiOutput(ns("toggle_locations_button"))),
          div(class = "d-inline-block", actionButton(ns("clear_pos"), "Clear coordinate selection") |> tooltip("Clear coordinate selection")),
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        imageOutput(ns("imgprev"), width = "100%", height = "auto"),
        textOutput(ns("image_index"))
      ),
      column(
        width = 6,
        leaflet::leafletOutput(ns("map"), width = "100%", height = "70vh"),
        br()
      )
    ),
    fluidRow(
      column(
        width = 6,
        actionButton(ns("select_all"), "Select All") |> tooltip("Select all images in the table"),
        actionButton(ns("clear_selection"), "Clear Selection") |> tooltip("Clear all selected images from the table"),
        actionButton(ns("remove_selection"), "Remove Selection") |> tooltip("Remove selected images from the table"),
        div(style = "float: right;", actionButton(ns("get_location_coordinates"), "Fetch location coordinates") |> tooltip("Populates the lat/lon fields with coordinates of selected location")),
        br(),
        
        fluidRow(
          column(
            width = 8,
            selectizeInput(
              ns("image_type"),
              "Visit type:", choices = NULL, multiple = TRUE, width = "100%",
              options = list(maxItems = 1, create = FALSE, placeholder = "Select visit type")
            ) |> tooltip("Select visit type.", id = "image_type_tt")
          ),
          column(
            width = 4,
            selectInput(
              ns("share_tag"),
              "Select visibility:", choices = share_list, multiple = FALSE
            )
          )
        ),
        
        
        selectizeInput(ns("image_tags"), "Tag(s):",
                       multiple = TRUE,
                       choices = NULL, width = "100%", options = list(create = TRUE, placeholder = "Select tag(s)") |> tooltip("Add tags to image(s); you can also create new tags.")
        ),
        fluidRow(
          column(width = 6, selectizeInput(ns("image_location"), "Location:", choices = NULL, multiple = TRUE, width = "100%", options = list(maxItems=1, create = FALSE, placeholder = "Select location")) |> tooltip("Press 'Fetch location coordinates' to add location coordinates to image(s)")
          ),
          column(width = 6, selectizeInput(ns("image_sublocation"), "Sublocation:", choices = NULL, multiple = TRUE, width = "100%", options = list(maxItems=1, create = FALSE, placeholder = "Optional - not all locations have sublocations")))
        ),
        fluidRow(
          column(width = 2, numericInput(ns("image_latitude"), "Latitude:", value = NULL, width = "100%", step = 0.1)),
          column(width = 2, numericInput(ns("image_longitude"), "Longitude:", value = NULL, width = "100%", step = 0.1)),
          column(width = 3, dateInput(ns("image_date"), "Date:", value = as.Date(NA), width = "100%")),
          column(width = 3, textInput(ns("image_time"), "Time:", value = NULL, width = "100%")),
          column(width = 2, selectInput(ns("tz_correction"), "UTC:", choices = tz_corrections, multiple = FALSE, selected = -7))
        ),
        textAreaInput(ns("notes"), "Notes:", value = "", width = "100%", height = "100px"),
        div(style = "float: right;", actionButton(ns("apply"), "Apply") |> tooltip("Apply changes to selected row(s) in the table")),
      ),
      column(
        width = 6,
        DT::dataTableOutput(ns("table")),
        actionButton(ns("upload_to_ac"), "Upload to AquaCache", style = "background-color: #007BFF; color: white;") |> tooltip("Upload images and metadata to the AquaCache database"),
      )
    )
    
  )
}



# Define the server logic
addImgs <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    tag_lists <- list(
      `Ice observation flight` = list("Head", "Toe", "Smooth ice", "In flood"),
      `Audit` = list("Other", "Placeholder", "Smooth ice", "In flood"),
      `Field visit` = list("Placeholder"),
      `Sampling event` = list("Head", "Toe", "Smooth ice", "In flood"),
      `Other` = NULL
    )
    
    # Increase the maximum upload size to 100MB
    options(shiny.maxRequestSize = 100 * 1024^2)
    
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
    
    
    types <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM files.image_types")  
    
    locations <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM public.locations")
    sublocations <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM public.sub_locations")
    
    # Update the 'locations' list based on the data loaded from Aquacache
    updateSelectizeInput(session, "image_location", choices = stats::setNames(locations$location_id, locations$name))
    
    updateSelectizeInput(session, "image_type", choices = stats::setNames(types$image_type_id, types$image_type))
    
    # Reactive value to track the current image index
    imgid <- reactiveVal(1)
    
    # Initialize reactive values to track the state of the app
    toggles <- reactiveValues(locations = FALSE, mapclicked = TRUE, edit = FALSE)
    
    # Initiaze dataframe to store exif data and updated attributes
    dat <- reactiveValues(df = data.frame())
    
    # Render an empty leaflet map
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap, group = "World Aerial") %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "World Imagery") %>%
        leaflet::addLayersControl(
          baseGroups = c("World Aerial", "World Imagery"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) %>%
        leaflet::setView(lng = DEFAULT_COORDS[1], lat = DEFAULT_COORDS[2], zoom = 4)
    })
    
    # Observe file input and update the data table
    observeEvent(input$files, ignoreNULL = TRUE, ignoreInit = TRUE, {
      files <- input$files
      #if (is.null(files)) {
      #  return(NULL)
      #}
      
      dat$df <- exifr::read_exif(files$datapath)
      dat$df$FileName <- files$name
      
      # Grab only necessary attributes and add placeholders for new ones
      dat$df <- preprocess_exif(dat$df)
      
      # Check for null latitude or longitude values; issue a warning if any are found.
      dat$df$Latitude[dat$df$Latitude == 0] <- NA
      dat$df$Longitude[dat$df$Longitude == 0] <- NA
      dat$df$Altitude[dat$df$Longitude == 0] <- NA
      
      # Once the images are loaded into the app, render the GUI elements
      renderDataTable()
      # renderImage()
      renderLeafletMap()
      renderImageIndex()
    })
    
    # Render the data table
    renderDataTable <- function() {
      output$table <- DT::renderDataTable({
        DT::datatable(
          dat$df %>%
            dplyr::mutate(across(where(is.numeric), ~ round(., 3))) %>%
            dplyr::mutate(Datetime = format(as.POSIXct(Datetime, format = "%Y:%m:%d %H:%M:%S"), "%d-%b-%Y %H:%M")),
          selection = list(mode = "multiple", target = "row"),
          editable = TRUE,
          options = list(
            dom = "t",
            paging = FALSE,
            ordering = FALSE,
            scrollX = TRUE,
            scrollY = "400px",
            autoWidth = TRUE,
            columnDefs = list(
              list(visible = FALSE, targets = which(names(dat$df) %in% c("SourceFile", "Location_ID", "ImageType_ID","ImageWidth", "ImageHeight")))
            )
          )
        )
      })
    }
    
    # Render the toggle buttons here, which change colour based on the state of the app
    output$edit_button <- renderUI({
      actionButton(ns("edit"), "Manual coordinate selection",
                   icon = icon("map-location-dot"),
                   style = if (toggles$edit) "background-color: #007BFF; color: white;" else "background-color: white; color: black;"
      ) |> tooltip("Click to toggle map interactivity - map click populates the lat/lon fields")
    })
    
    output$toggle_locations_button <- renderUI({
      actionButton(ns("toggle_locations"), "Toggle locations",
                   style = if (toggles$locations) "background-color: #007BFF; color: white;" else "background-color: white; color: black;"
      ) |> tooltip("Display locations stored in AquaCache")
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
    observeEvent(list(input$image_latitude, input$image_longitude),
                 {
                   if (is_valid_latlon(input$image_latitude, input$image_longitude)) {
                     leaflet::leafletProxy("map") %>%
                       leaflet::clearGroup("manual_marker") %>%
                       leaflet::addCircleMarkers(
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
                     
                   }
                 },
                 ignoreInit = TRUE
    )
    
    
    
    observeEvent(list(input$image_latitude, input$image_longitude, input$table_rows_selected), {
      if (is_valid_latlon(input$image_latitude, input$image_longitude) && length(input$table_rows_selected) > 0) {
        selected_rows <- input$table_rows_selected
        for (row in selected_rows) {
          selected_lat <- dat$df$Latitude[row]
          selected_lon <- dat$df$Longitude[row]
          if (is.na(selected_lat) | is.na(selected_lon)){
            return()
          }
          leaflet::leafletProxy("map") %>%
            leaflet::clearGroup("manual_line") %>%
            leaflet::addPolylines(
              lng = c(selected_lon, input$image_longitude),
              lat = c(selected_lat, input$image_latitude),
              color = color_list$click,
              weight = 2,
              dashArray = "5, 10",
              group = "manual_line"
            )
        }
      } else{
        leaflet::leafletProxy("map") %>%
          leaflet::clearGroup("manual_line")
      }
    })
    
    
    
    # Toggle aquacache locations on the map
    observeEvent(input$toggle_locations, {
      if (toggles$locations) {
        leaflet::leafletProxy("map") %>% leaflet::clearGroup("locations")
      } else {
        addLocationsToMap()
      }
      toggles$locations <- !toggles$locations
    })
    
    
    # Observe changes in the data table and re-render the map
    observeEvent(input$table_cell_edit, {
      info <- input$table_cell_edit
      if (info$col %in% c("Latitude", "Longitude")) {
        dat$df[info$row, info$col] <- as.numeric(info$value)
        #renderLeafletMap()
      }
    })
    
    # Observe select all button click and select all rows in the data table
    observeEvent(input$select_all, {
      proxy <- DT::dataTableProxy("table")
      DT::selectRows(proxy, seq_len(nrow(dat$df)))
    })
    
    observeEvent(input$remove_selection, {
      if (is.null(input$table_rows_selected)) {
        showModal(modalDialog(
          title = "No Images Selected",
          "Please select images from the table before attempting to remove.",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close")
          )
        ))
        return()
      }
      
      # Remove selected rows from the data table
      dat$df <- dat$df[-input$table_rows_selected, ]
    })
    
    # Observe clear selection button click and clear all selected rows in the data table
    observeEvent(input$clear_selection, {
      proxy <- DT::dataTableProxy("table")
      DT::selectRows(proxy, NULL)
      
      # Update the map to reflect deselection
      #updateMapView()
      leaflet::leafletProxy(mapId = "map") %>% leaflet::clearGroup("photos")
      addCirclesToMap(selection = seq_len(nrow(dat$df)), color = color_list$unselected)
    })
    
    observeEvent(input$get_location_coordinates, {
      if (is.null(input$image_location)) {
        showModal(modalDialog(
          title = "No Location Selected",
          "Please select a location from the drop-down menu.",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close")
          )
        ))
        return()
      }
      
      lat <- locations$latitude[locations$location_id == input$image_location]
      lon <- locations$longitude[locations$location_id == input$image_location]
      
      updateNumericInput(session, "image_latitude", value = lat)
      updateNumericInput(session, "image_longitude", value = lon)
    })
    
    observeEvent(input$image_type, ignoreNULL = TRUE, {
      types_out <<- types
      
      image_tags <- types[types$image_type_id == as.integer(input$image_type), "default_tag_options"] %>%
        strsplit(",") %>%
        unlist() %>%
        trimws() %>%
        gsub("[{}]", "", .) %>%
        gsub('\"', "", .)
      
      updateSelectizeInput(session, "image_tags", choices = image_tags, options = list(placeholder = "Select tags"))
      
      #if (!is.na(input$image_type)){
      #  bslib::update_tooltip(session, inputId = "image_type", title = "test", placement = "right")
      #}      
      
    })
    
    observeEvent(input$image_location, ignoreNULL = TRUE, {
      updateSelectizeInput(session, "image_sublocation",
                           choices = stats::setNames(
                             sublocations[sublocations$location_id == input$image_location, "sub_location_id"],
                             sublocations[sublocations$location_id == input$image_location, "name"]
                           ),
                           options = list(create = TRUE, multiple = FALSE, placeholder = "Select sublocation")
      )
      
      
      leaflet::leafletProxy("map") %>%
        leaflet::clearGroup("selected_location") %>%
        leaflet::addCircleMarkers(
          lng = locations$longitude[locations$location_id == input$image_location],
          lat = locations$latitude[locations$location_id == input$image_location],
          radius = 5,
          color = color_list$location_selected,
          fill = TRUE,
          fillOpacity = 0.8,
          popup = locations$name[locations$location_id == input$image_location],
          group = "selected_location"
        )
    })
    
    # Observe row selection in the data table and update the imgid
    observeEvent(input$table_rows_selected, ignoreNULL = FALSE, {
      # if table selection changes, reset the image coordinateds
      updateNumericInput(session, "image_latitude", value = "")
      updateNumericInput(session, "image_longitude", value = "")
      
      # if (is.null(input$table_rows_selected)) {
      #  selection <- 0
      # } else {
      #  selection <- input$table_rows_selected
      # }
      
      
      
      
      # update the displayed image upon each new row selection
      if (is.null(input$table_rows_selected)) {
        imgid(1)
      } else if (length(input$table_rows_selected) == 1) {
        imgid(input$table_rows_selected)
      } else {
        imgid(input$table_rows_selected[length(input$table_rows_selected)])
      }
      
      
      # case for no selection
      # if (is.null(selection)) {
      #  updateMapView()
      #  leafletProxy(mapId = "map") %>% clearGroup("photos")
      #  addCirclesToMap(selection = seq_len(nrow(dat$df)), color = color_list$unselected)
      # }
    })
    
    
    # Observe the upload button and upload data to AquaCache
    observeEvent(input$upload_to_ac, {
      if (is.null(dat$df) || nrow(dat$df) == 0) {
        showModal(modalDialog(
          title = "No Images to Upload",
          "Please add some images before attempting to upload to AquaCache.",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close")
          )
        ))
        return()
      }
      
      if (any(is.na(dat$df$Latitude)) | any(is.na(dat$df$Longitude))) {
        oops <- FALSE
        for (i in 1:nrow(dat$df)) {
          if (is.na(dat$df$Latitude[i]) | is.na(dat$df$Longitude[i])) {
            loc_id <- dat$df$Location_ID[i]
            if (is.na(loc_id)) {
              oops <- TRUE
              break
            }
            dat$df$Latitude[i] <- locations$latitude[locations$location_id == loc_id]
            dat$df$Longitude[i] <- locations$longitude[locations$location_id == loc_id]
          }
        }
        if (oops) {
          showModal(modalDialog(
            title = "Error: Missing Data",
            "One or more images is missing latitude and longitude and has no associated location. Either assign a location or add latitude/longitude to the offending images.",
            easyClose = TRUE,
            footer = tagList(
              modalButton("Close")
            )
          ))
          return()
        }
      }
      
      showModal(modalDialog(
        title = "Confirm Upload",
        "Are you sure you want to upload the images to AquaCache?",
        easyClose = FALSE,
        footer = tagList(
          actionButton(ns("confirm_upload"), "Upload all images"),
          actionButton(ns("confirm_upload_selection"), "Upload selected row(s)"),
          modalButton("Cancel")
        )
      ))
    })
    
    # Validate the uploaded dataframe and upload to AquaCache
    observeEvent(input$confirm_upload, {
      removeModal()
      upload(dat$df)
    })
    
    
    # Validate the uploaded dataframe and upload to AquaCache
    observeEvent(input$confirm_upload_selection, {
      removeModal()
      if (length(input$table_rows_selected) == 0) {
        showModal(modalDialog(
          title = "No Images Selected",
          "Please select images from the table before attempting to upload.",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close")
          )
        ))
        return()
      }
      
      upload(dat$df[input$table_rows_selected, ])
    })
    
    # Define the upload ExtendedTask
    uploadAquacache <- ExtendedTask$new(
      function(df, config) {
        promises::future_promise({
          con <- AquaConnect(
            name = config$dbName,
            host = config$dbHost,
            port = config$dbPort,
            username = config$dbUser,
            password = config$dbPass,
            silent = TRUE
          )
          on.exit(DBI::dbDisconnect(con))
          
          out <- list()
          for (i in seq_len(nrow(df))) {
            tryCatch({
              print(df$SourceFile[i])
              ret <- AquaCache::insertACImage(
                object = df$SourceFile[i],
                datetime = df$Datetime[i],
                latitude = df$Latitude[i],
                longitude = df$Longitude[i],
                elevation_msl = if (is.na(df$Altitude[i])) NULL else df$Altitude[i],
                azimuth_true = if (is.na(df$Azimuth[i])) NULL else df$Azimuth[i],
                share_with = df$Visibility[i],
                image_type = if (is.na(df$ImageType_ID[i])) NULL else df$ImageType_ID[i],
                con = con,
                location = if (is.na(df$Location_ID[i])) NULL else df$Location_ID[i],
                tags = if (is.na(df$Tags[i])) NULL else paste(df$Tags[i], collapse = ", "),
                description = if (is.na(df$Notes[i])) NULL else df$Notes[i],
                owner = 2
              )
              out[[df$FileName[i]]] <- TRUE
            },
            error = function(e) {
              out[[df$FileName[i]]] <<- e$message
            }) # End of tryCatch
          }
          return(out)
        })
      }
    )   # End of extendedTask$new
    
    # Function to handle the upload process
    upload <- function(df) {
      # Validate the data frame before upload
      validation <- validateDataFrame(df)
      if (!validation$success) {
        showModal(modalDialog(
          title = "Validation Error",
          validation$message,
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close")
          )
        ))
        return()
      }
      
      # Standardize the data frame for AquaCache
      standardized_df <- standardizeDataFrame(df)
      
      # Perform the upload task
      uploadAquacache$invoke(standardized_df, session$userData$config)
    }
    
    observeEvent(uploadAquacache$result(), {
      
      success_count <- sum(unlist(uploadAquacache$result()) == TRUE)
      failure_count <- length(uploadAquacache$result()) - success_count
      out <<- uploadAquacache$result()
      
      showModal(modalDialog(
        title = "Upload Complete",
        paste(success_count, "images uploaded successfully."),
        if (failure_count > 0) {
          HTML(paste0(failure_count, " images failed to upload.<br><br>Failure messages:<br>", paste(unlist(uploadAquacache$result()[!unlist(uploadAquacache$result()) == TRUE]), collapse = "<br>")))
        },
        easyClose = TRUE,
        size = "xl",
        footer = tagList(
          modalButton("Close")
        )
      ))
    })
    
    
    # standardize the dataframe to match AquaCache's format
    standardizeDataFrame <- function(df) {
      reader_list <- c("Private" = "yg_reader", "Public" = "public_reader")
      df$Visibility <- reader_list[df$Visibility]
      
      # photo exif data is recorded in local time (based on phone) and no time zone is stored
      # UTC offset is a user-input, which defaults to -7 for the yukon
      # here, apply the datetime offset so that the uploaded datetime in AquaCache is UTC
      df$Datetime <- as.POSIXct(df$Datetime, format = "%Y:%m:%d %H:%M:%S", tz = "UTC")
      df$Datetime <- df$Datetime - df$UTC * 3600
      # df$Notes <- as.character(df$Notes)
      
      for (ii in seq_len(nrow(df))) {
        df$Tags[ii] <- if (length(df$Tags[[ii]]) == 0) "" else df$Tags[[ii]]
      }
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
    queried_laton <- reactiveValues(lat = 0.0, lon = 0.0)
    
    # NOTE_ES: commenting this out for now, since we don't necessarily want the lat/lon to update when we select a location
    # observeEvent(input$image_location, {
    #   # if the location is default, don't update latlon
    #   if (input$image_location == "Placeholder" | input$image_location == "") {
    #     return()
    #   }
    #
    #   lat <- locations[locations$name == input$image_location, "latitude"]
    #   lon <- locations[locations$name == input$image_location, "longitude"]
    #
    #   updateNumericInput(session, "image_latitude", value = lat)
    #   updateNumericInput(session, "image_longitude", value = lon)
    #
    #   queried_laton$lat <- lat
    #   queried_laton$lon <- lon
    #
    #   # update the map view to the selected location
    #   leaflet::leafletProxy("map") %>%
    #     leaflet::setView(lng = lon, lat = lat, zoom = 4)
    # }, ignoreInit = TRUE)
    
    
    # if the user manually changes the lat lon, reset the location
    # observeEvent(list(input$image_latitude, input$image_longitude), {
    #
    #   if (is.na(input$image_latitude) | is.na(input$image_longitude)) {
    #     updateSelectInput(session, "image_location", selected = "")
    #   }
    #
    #   else if ((queried_laton$lat != input$image_latitude) | (queried_laton$lon != input$image_longitude)) {
    #     updateSelectInput(session, "image_location", selected = "")
    #   }
    #
    # }, ignoreInit = TRUE)
    
    
    
    # button used to apply changes in attribute values to the table; this is safer than modifying the table directly
    observeEvent(input$apply, {
      if (length(input$table_rows_selected) > 0) {
        for (row in input$table_rows_selected) {
          # distance <- latlon_to_meters(dat$df$Latitude[row], dat$df$Longitude[row], input$image_latitude, input$image_longitude)
          
          # if (distance > 1000) {
          #  showNotification("Warning: The distance between the selected image and the entered coordinates exceeds 1000 meters.", type = "warning", duration = 5)
          # }
          
          # only write attributes that are not set to their default values (typically empty)
          # this way, the user can leave attributes blank, such as lat-lon, without worrying about overwriting them
          # while you could write everything to the table, since the table already has default values, this created a problem when you want to change multiple images at once
          # since multiple images won't have the same 'default' lat-lons
          
          if (input$notes != "") {
            dat$df$Notes[row] <- input$notes
          }
          
          if (!is.null(input$image_tags)) {
            dat$df$Tags[row] <- paste(input$image_tags, collapse = ", ")
          }
          
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
          
          # check whether the time is valid
          if (!is.null(input$image_time) && input$image_time != "") {
            if (!is_valid_time(input$image_time)) {
              showModal(modalDialog(
                title = "Invalid Time",
                "The time value is not valid. Make sure time is in format HH:MM",
                easyClose = TRUE,
                footer = tagList(
                  modalButton("Close")
                )
              ))
              return()
            }
          }
          
          # check whether the date is valid
          if (!identical(input$image_date, as.Date(character(0)))) {
            
            if (input$image_time == "") {
              image_time = "00:00"
            } else{
              image_time = input$image_time
            }
            
            image_datetime = as.POSIXct(paste(input$image_date, image_time), format = "%Y-%m-%d %H:%M")
            
            dat$df$Datetime[row] <- format(image_datetime, "%Y:%m:%d %H:%M:%S")
          }
          
          if (!is.null(input$image_type)){            
            dat$df$ImageType_ID[row] <- as.numeric(input$image_type)
            dat$df$ImageType[row] <- types[input$image_type == types$image_type_id, "image_type"]
          }
          
          if (!is.null(input$image_location)){            
            dat$df$Location_ID[row] <- as.numeric(input$image_location)
            dat$df$Location[row] <- locations[input$image_location == locations$location_id, "name"]
          }
          
          if (!is.na(input$image_latitude) && !is.na(input$image_longitude)) {
            dat$df$Latitude[row] <- input$image_latitude
            dat$df$Longitude[row] <- input$image_longitude
          }
          if (input$share_tag != "Private") {
            dat$df$Visibility[row] <- input$share_tag
          }
          if (input$tz_correction != -7) {
            dat$df$UTC[row] <- input$tz_correction
          }
        }
        
        # Reset all of the input fields after applying changes and re-render the table/map
        updateTextAreaInput(session, "notes", value = "")
        updateSelectizeInput(session, "image_tags", selected = NULL)
        updateNumericInput(session, "image_latitude", value = NA)
        updateNumericInput(session, "image_longitude", value = NA)
        updateSelectInput(session, "image_location", selected = NULL)
        updateSelectInput(session, "image_sublocation", selected = NULL)
        updateSelectInput(session, "image_type", selected = NULL)
        updateSelectInput(session, "image_date", selected = as.Date(NA))
        updateTextInput(session, "image_time", value = NULL)
        updateSelectInput(session, "share_tag", selected = "Private")
        updateSelectInput(session, "tz_correction", selected = -7)
        #renderDataTable()
        #renderLeafletMap()
        
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
      leaflet::leafletProxy("map") %>%
        leaflet::clearGroup("manual_marker") %>%
        leaflet::clearGroup("click_line")
      toggles$mapclicked <- FALSE
      
      updateNumericInput(session, "image_latitude", value = NA)
      updateNumericInput(session, "image_longitude", value = NA)
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
      } else {
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
      leaflet::leafletProxy(mapId = "map") %>%
        leaflet::setView(lng = centroid_lon, lat = centroid_lat, zoom = zoom)
    }
    
    # Render the image plot
    # This output will update automatically based on the imgid() React var
    output$imgprev <- renderImage(
      {
        if (is.null(dat$df) || nrow(dat$df) == 0) {
          image <- file.path(getwd(), "www", "imgs", "imgupload_welcome.jpg")
          
          image_height <- 100
          image_width <- 200
        } else {
          image <- dat$df$SourceFile[imgid()]
          
          image_height <- dat$df$ImageHeight[imgid()]
          image_width <- dat$df$ImageWidth[imgid()]
        }
        
        # handle viewing landscape images
        if (image_width > image_height) {
          width <- "100%"
          height <- "auto"
        } else {
          width <- "50%"
          height <- "auto"
        }
        
        list(src = image, alt = "User selected image", width = width, height = height)
      },
      deleteFile = FALSE
    )
    
    
    # print image n/x
    renderImageIndex <- function() {
      output$image_index <- renderText({
        paste("Image", imgid(), "of", nrow(dat$df))
      })
    }
    # Function to render the leaflet map
    renderLeafletMap <- function() {
      # only render images with valid latlon
      valid_data <- dat$df[is_valid_latlon(dat$df$Latitude, dat$df$Longitude), ]
      
      # if no images with valid latlon, exit map rendering function\
      if (nrow(valid_data) == 0) {
        showNotification("No images with valid coordinates to display on the map.", type = "warning", duration = 5)
      } else {
        output$map <- leaflet::renderLeaflet({
          leaflet::leaflet(data = valid_data) %>%
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery) %>%
            leaflet::addLayersControl(
              baseGroups = c("World Topo Map", "World Imagery"),
              options = leaflet::layersControlOptions(collapsed = FALSE)
            ) %>%
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "World Aerial") %>%
            leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap, group = "World Topo Map") %>%
            leaflet::addCircleMarkers(
              lng = valid_data$Longitude,
              lat = valid_data$Latitude,
              radius = 2,
              color = color_list$unselected,
              fill = TRUE,
              fillOpacity = 0.5,
              popup = ~ paste(
                "FileName:", FileName, "<br>",
                "Date/Time:", Datetime, "<br>",
                "Latitude:", round(Latitude, 5), "<br>",
                "Longitude:", round(Longitude, 5), "<br>",
                "Altitude:", round(Altitude, 2), "m"
              )
            ) %>%
            leaflet::addScaleBar(position = "bottomleft", options = leaflet::scaleBarOptions(imperial = FALSE, maxWidth = 100, metric = TRUE, updateWhenIdle = TRUE)) %>%
            htmlwidgets::onRender("function(el, x) {
          var credits = el.querySelector('.leaflet-control-attribution');
          if (credits) {
            credits.style.fontSize = '4px';
          }
        }")
        })
      }
      # addLocationsToMap()
    }
    
    
    # Generic-ish function to add circles to the map based on selection
    addCirclesToMap <- function(selection, color, radius = 2.5) {
      valid_rows <- dat$df[selection, ]
      valid_rows <- valid_rows[!is.na(valid_rows$Latitude) & !is.na(valid_rows$Longitude), ]
      if (nrow(valid_rows) > 0){
        leaflet::leafletProxy(mapId = "map") %>%
          leaflet::addCircleMarkers(
            lng = valid_rows$Longitude,
            lat = valid_rows$Latitude,
            radius = radius,
            color = color,
            fillColor = color,
            fill = TRUE,
            fillOpacity = 1,
            group = "photos"
          )
      }
    }
    
    # add 'locations' table from AquaCache to the map
    addLocationsToMap <- function() {
      leaflet::leafletProxy(mapId = "map") %>%
        leaflet::addCircleMarkers(
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
    observeEvent(input$table_rows_selected,
                 {
                   #updateMapView()
                   leaflet::leafletProxy(mapId = "map") %>% leaflet::clearGroup("photos")
                   if (length(input$table_rows_selected) > 0) {
                     addCirclesToMap(selection = input$table_rows_selected, color = color_list$selected, radius = 5)
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
      }
    })
    
    # Define a named list of colors
    color_list <- list(
      "selected" = "cyan",
      "unselected" = "black",
      "click" = "green",
      "other" = "magenta",
      "locations" = "orange",
      "location_selected" = "yellow"
    )
    
    if (Sys.info()["sysname"] == "Windows" | interactive()) {
    } else {
      future::plan("multicore")
      future::plan(future::tweak(future::multicore, seed = TRUE))
    }
    
    # Default coordinates to repositon the map around Whitehorse
    DEFAULT_COORDS <- c(-135.0568, 60.7212)
    
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
    
    is_valid_time <- function(time) {
      # Check if the time is in the correct format (HH:MM)
      if (grepl("^\\d{2}:\\d{2}$", time)) {
        parts <- strsplit(time, ":")[[1]]
        if (as.integer(parts[1]) >= 0 && as.integer(parts[1]) < 24 &&
            as.integer(parts[2]) >= 0 && as.integer(parts[2]) < 60) {
          # Check if hours and minutes are within valid ranges
          return(TRUE)
        } else {
          return(FALSE)
        }
      } else {
        return(FALSE)
      }
    }
    # end of is_valid_time()
    
    
    # Function to infer zoom level based on latitude and longitude arrays
    infer_zoom_level <- function(latitudes, longitudes) {
      if (length(latitudes) == 0 || length(longitudes) == 0) {
        return(1) # Default zoom level if no points are provided
      }
      
      # Calculate the bounding box
      suppressWarnings({
        min_lat <- min(latitudes, na.rm = TRUE)
        max_lat <- max(latitudes, na.rm = TRUE)
        min_lon <- min(longitudes, na.rm = TRUE)
        max_lon <- max(longitudes, na.rm = TRUE)
      })
      
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
    
    
    # Preprocess exif data table add new attributes and rename columns
    preprocess_exif <- function(dat) {
      # Ensure required columns exist, create them if they don't
      required_columns <- c("SourceFile", "FileName", "GPSLatitude", "GPSLongitude", "GPSAltitude", "DateTimeOriginal", "GPSImgDirection", "ImageWidth", "ImageHeight")
      for (col in required_columns) {
        if (!col %in% colnames(dat)) {
          dat[[col]] <- NA
        }
      }
      
      dat_out <- dat %>%
        dplyr::select(SourceFile, FileName, DateTimeOriginal, GPSLatitude, GPSLongitude, GPSAltitude, GPSImgDirection, ImageWidth, ImageHeight) %>%
        dplyr::rename(Latitude = GPSLatitude, Longitude = GPSLongitude, Altitude = GPSAltitude, Datetime = DateTimeOriginal, Azimuth = GPSImgDirection) %>%
        dplyr::mutate(Tags = NA, ImageType = NA, Notes = NA, Visibility = "Private", UTC = -7, Location = NA, Location_ID = NA, ImageType_ID = NA)
      return(dat_out)
    }
  }) # end of moduleServer
}
