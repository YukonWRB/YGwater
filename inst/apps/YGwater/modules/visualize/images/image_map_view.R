# This module allows users to view images by clicking on points on a map. It is possible to filter images based on several attributes: tags, image types, ??

imgMapViewUI <- function(id) {
  ns <- NS(id)
  
  # All UI elements are rendered in the server function to allow multi-language functionality
  page_fluid(
    # Top row with filters (collapsible using bslib accordion)
    uiOutput(ns("accordion")),
    # Map and selected image in a side-by-side layout, with collapsible map.
    uiOutput(ns("sidebar_page"))
  )
}

imgMapView <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
   # Initial data fetch (reactive so it can be observed or bound to)
    images <- reactiveValues(
      images = DBI::dbGetQuery(session$userData$AquaCache, "SELECT image_id, img_meta_id, datetime, latitude, longitude, location_id FROM files.images")
    )
    attr(images$images$datetime, "tzone") <- "MST"
    
    selections = reactiveValues()
    selections$img_id = 14077
    selections$filter <- rep(TRUE, nrow(images$images))
    
    renderSelectedImage <- function(id) {
      if (is.na(id)) {
        return(NULL)  # Return NULL if no image is selected
      } else {
        output$img <- renderImage({
          image <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT format, file FROM images WHERE image_id = ",id))
          # Check if image retrieval was successful and if there is a file to display
          if (nrow(image) == 1 && !is.null(image$file)) {
            outfile <- tempfile(fileext = paste0(".", image$format))
            writeBin(image$file[[1]], outfile)
            list(src = outfile, alt = "User selected image", width = "100%", height = "auto")
          } else {
            list(src = NULL)  # Return NULL if no image or an error occurs
          }}, deleteFile = TRUE)
      }}
    
    
    # Render the UI elements, re-rendered on language selection ################
    output$accordion <- renderUI({
      accordion(
        open = ns("filters"),
        accordion_panel(
          id = ns("filters"),
          title = tr("filters", language$language),
          
          fluidPage(
            div(
              class = "d-inline-block me-3 align-items-start",
              style = "vertical-align: top; display: inline-flex;",
              dateRangeInput(
                ns("dates"),
                label = tr("date_range_lab", language$language),
                start = Sys.Date() - 30,
                end = Sys.Date(),
                language = language$abbrev,
                separator = tr("date_sep", language$language)
              )
            ),
            div(
              class = "d-inline-block me-3 align-items-start",
              style = "vertical-align: top; display: inline-flex;",
              selectInput(
                ns("months"),
                label = "Month",
                choices = month.name,
                selected = NULL,
                multiple = TRUE
              )
            ),
            div(
              class = "d-inline-block me-3 align-items-start",
              style = "vertical-align: top; display: inline-flex;",
              sliderInput(
                ns("toy_lab"),
                label = "Time of day (UTC-7)",
                min = 0,
                max = 23,
                value = c(0, 23),
                step = 1,
                ticks = TRUE,
                sep = "",
                dragRange = TRUE
              )
            ),
            div(
              class = "d-inline-block me-3 align-items-start",
              style = "vertical-align: top; display: inline-flex;",
              selectizeInput(
                ns("tags"),
                label = "Tag",
                choices = c("No tags associated with these images"),
                selected = NULL,
                multiple = TRUE
              )
            )
          ) 
        )
      )
    }) |> bindEvent(language$language)
    
    output$sidebar_page <- renderUI({
      page_sidebar(
        sidebar = sidebar(
          title = NULL,
          leaflet::leafletOutput(ns("map"), width = "100%", height = "600px"),
          # Row for buttons below the map
          div(
            style = "display: flex; gap: 10px; margin-top: 10px;",
            actionButton(ns("reset_view"), "Reset view"),
            actionButton(ns("load_additional_layers"), "Load hydrometric layers") |> bslib::tooltip("Loads basins and location layers, requires good internet connection"),
            actionButton(ns("refresh_images"), "Refresh images") |> bslib::tooltip("Refresh the list of images from the database"),
          ),
          width = "40%",
          bg = "#f8f8f8",
          position = "left",
          open = TRUE
        ),
        imageOutput(ns("img"), fill = TRUE)
      )
    })
    
    output$map <- leaflet::renderLeaflet({
      
      m <- leaflet::leaflet() %>%
        leaflet::addTiles(group = "OSM (default)") %>%
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Esri Satellite") %>%
        leaflet::addProviderTiles("CartoDB.Positron", group = "CartoDB Light") %>%
        leaflet::addLayersControl(
          baseGroups = c("OSM (default)", "Esri Satellite", "CartoDB Light"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) %>%
        leaflet::setView(
          lng = -135.0,
          lat = 64.0,
          zoom = 4
        )
    })
    
    # Function to render the map with markers
    updateMap <- function(selection) {
      # Filter images based on current selection
      filtered_images <- images$images[selection, ]
      
      # Create the leaflet map with markers
      if (nrow(filtered_images) == 0) {
        # No filtered images: clear markers and reset view to default
        leaflet::leafletProxy("map", session) %>% leaflet::clearMarkerClusters()
      } else {
        leaflet::leafletProxy("map", session) %>%
          leaflet::clearMarkerClusters() %>%
          leaflet::addMarkers(
            data = filtered_images,
            lng = ~longitude,
            lat = ~latitude,
            layerId = ~image_id,
            group = "Images",
            popup = ~paste("ID:", image_id, "<br>Date:", datetime),
            label = ~as.character(image_id),
            icon = leaflet::icons(
              iconUrl = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/icons/camera-fill.svg",
              iconWidth = 24, iconHeight = 24
            ),
            clusterOptions = leaflet::markerClusterOptions(spiderfyDistanceMultiplier = 1.2, showCoverageOnHover = TRUE)
          ) %>%
          leaflet::fitBounds(
            min(filtered_images$longitude, na.rm = TRUE),
            min(filtered_images$latitude, na.rm = TRUE),
            max(filtered_images$longitude, na.rm = TRUE),
            max(filtered_images$latitude, na.rm = TRUE)
          )
      }
    }
    
    observeEvent(input$map_marker_click, {
      
      # Only respond if the marker is from the "Images" group
      selections$img_id <- input$map_marker_click$id
      renderSelectedImage(selections$img_id)
      
    })
    
    
    
    observe({  # This observer will run on module initialization
      # Filter by date range
      date_filter <- images$images$datetime >= input$dates[1] & images$images$datetime <= input$dates[2]
      
      # Filter by months
      if (!is.null(input$months) && length(input$months) > 0) {
        month_filter <- format(as.Date(images$images$datetime), "%B") %in% input$months
      } else {
        month_filter <- TRUE
      }
      
      # Filter by time of day
      img_hour <- as.integer(format(as.POSIXct(images$images$datetime), "%H"))
      toy_filter <- img_hour >= input$toy_lab[1] & img_hour <= input$toy_lab[2]
      
      # Filter by tags (placeholder logic, update as needed)
      if (!is.null(input$tags) && length(input$tags) > 0) {
        tag_filter <- rep(TRUE, nrow(images$images)) # Replace with actual tag filtering
      } else {
        tag_filter <- TRUE
      }
      
      # Combine all filters
      selections$filter <- date_filter & month_filter & toy_filter & tag_filter
      
      updateMap(selections$filter)
    }) 
    
    
    observeEvent(input$refresh_images, {
      # Refresh the list of images from AquaCache
      images$images <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT image_id, img_meta_id, datetime, latitude, longitude, location_id FROM files.images")
      attr(images$images$datetime, "tzone") <- "MST"
      
      # Update the map with the new images
      updateMap(selections$filter)
    })

    observeEvent(input$load_additional_layers, {
      # Load additional layers (e.g., hydrometric layers) when the button is clicked
      # This is a placeholder; replace with actual loading logic
      showNotification("Loading additional layers (30-120 seconds)...", type = "message")
      
      
      
      
      #basins = DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM public.basins")
      basins <- getVector(layer_name="Drainage basins")
      locations <- getVector(layer_name="Locations")
      
      
      # Example: Add a new layer to the map (replace with actual layer loading)
      m <- leaflet::leafletProxy("map", session)
      
      if (!is.null(basins)) {
        # Sort basins by area (largest first), assuming 'area' column exists
        if ("area" %in% names(basins)) {
          basins <- basins[order(basins$area, decreasing = TRUE), ]
        }
        # Add each basin as a polygon, largest first (bottom), smallest last (top)
        for (i in seq_len(nrow(basins))) {
          m <- m %>%
            leaflet::addPolygons(
              data = basins[i, ],
              color = "#1f78b4",
              weight = 2,
              opacity = 0.7,
              fillOpacity = 0.2,
              group = "Basins",
              label = ~as.character(feature_name)
            )
        }
      }
      
      if (!is.null(locations)) {
        m <- m %>%
          leaflet::addCircleMarkers(
            data = locations,
            color = "#444444",
            weight = 2,
            opacity = 1,
            fillOpacity = 0.2,
            group = "Locations",
            label = ~as.character(feature_name), # Show location_id as label on hover
            options = leaflet::markerOptions(interactive = FALSE, keyboard = FALSE, riseOnHover = TRUE),
          ) 
        
        
        m <- m %>%
          leaflet::addLayersControl(
            overlayGroups = c("OSM (default)", "Esri Satellite", "CartoDB Light", "Basins", "Locations"),
            options = leaflet::layersControlOptions(collapsed = FALSE)
          ) %>%
          leaflet::hideGroup("Locations")
      }
    })
    
    observeEvent(input$reset_view, {
      leaflet::leafletProxy("map", session) %>%
        leaflet::setView(lng = -135.0, lat = 64.0, zoom = 4)
    })
    
  }) # End of moduleServer
} # End of img server function
