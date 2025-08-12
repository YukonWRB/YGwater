# This module allows users to view images by clicking on points on a map. It is possible to filter images based on several attributes: tags, image types, ??

imgMapViewUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(sprintf("
      /* Add colors to the accordion. Using ns() makes it specific to this module. */
      #%s .accordion {
        /* body background */
        --bs-accordion-bg:          #FFFCF5;
        /* collapsed header */
        --bs-accordion-btn-bg:      #FBE5B2;
        /* expanded header */
        --bs-accordion-active-bg:   #FBE5B2;
      }
    ", ns("accordion")))),
    # All UI elements are rendered in the server function to allow multi-language functionality
    page_fluid(
      # Top row with filters (collapsible using bslib accordion)
      uiOutput(ns("accordion")),
      # Map and selected image in a side-by-side layout, with collapsible map.
      uiOutput(ns("sidebar_page"))
    )
  )
}

imgMapView <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initial data fetch (reactive so it can be observed or bound to)
    images <- reactiveValues(
      images = DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT i.image_id, i.img_meta_id, i.datetime, i.latitude, i.longitude, i.location_id, i.tags, i.image_type AS image_type_id, it.image_type FROM files.images i LEFT JOIN files.image_types it on i.image_type = it.image_type_id WHERE i.datetime > NOW() - INTERVAL '7 days'"
      )
    )
    images$unique_types <- images$images[!duplicated(images$images$image_type), c("image_type_id", "image_type")]

    images$images <- images$images[order(images$images$datetime), ]

    images$images$tags_list <- lapply(images$images$tags, function(raw) {
      if (is.na(raw) || raw == "") return(character(0))
      inner <- gsub('["{}]', "", raw)
      strsplit(inner, ",", fixed = TRUE)[[1]]
    })

    attr(images$images$datetime, "tzone") <- "MST"

    selections = reactiveValues()
    selections$filter <- rep(TRUE, nrow(images$images))
    selected_colour = rgb(0, 1, 1, 0.8)  # Cyan with 0.5 transparency for the selected image in the mini timeline plot

    renderSelectedImage <- function(id) {
      if (is.na(id)) {
        return(NULL)
      } else {
        output$img <- renderImage({
          image <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT format, file FROM images WHERE image_id = ", id))
          if (nrow(image) == 1 && !is.null(image$file)) {
            outfile <- tempfile(fileext = paste0(".", image$format))
            writeBin(image$file[[1]], outfile)
            list(src = outfile, alt = "User selected image", width = "100%", height = "auto")
          } else {
            list(src = NULL)
          }
        }, deleteFile = TRUE)
      }
    }

    # Render the UI elements, re-rendered on language selection
    output$accordion <- renderUI({
      accordion(
        id = "accordion",
        open = ns("filters"),
        accordion_panel(
          id = ns("filters"),
          title = tr("filters", language$language),
          page_fluid(
            div(
              class = "d-inline-block me-3 align-items-start",
              style = "vertical-align: top; display: inline-flex;",
              dateRangeInput(
                ns("dates"),
                label = tr("date_range_lab", language$language),
                start = Sys.Date() - 7,
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
                ns("tod"),
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
                ns("img_type"),
                label = "Image type",
                choices = stats::setNames(images$unique_types$image_type_id, images$unique_types$image_type),
                selected = NULL,
                multiple = TRUE
              )
            ),
            div(
              class = "d-inline-block me-3 align-items-start",
              style = "vertical-align: top; display: inline-flex;",
              selectizeInput(
                ns("tags"),
                label = "Tag",
                choices = unique(unlist(images$images$tags_list)),
                selected = NULL,
                multiple = TRUE
              )
            )
          )
        ),
        accordion_panel(
          id = ns("map_options"),
          title = "Map options",
          page_fluid(
            actionButton(ns("load_additional_layers"), "Load additional layers"),
            actionButton(ns("reset_view"), "Reset view"),
            actionButton(ns("refresh_images"), "Refresh images"),
            checkboxInput(ns("cluster_map"), "Cluster map", value = TRUE)
          )
        )
      )
    }) |> bindEvent(language$language)

    output$sidebar_page <- renderUI({
      page_sidebar(
        sidebar = sidebar(
          title = NULL,
          width = 600,
          leaflet::leafletOutput(ns("map"), width = "100%", height = "300px"),
          div(
            style = "margin-top: 10px;",
            verbatimTextOutput(ns("img_info"))
          )
        ),
        div(
          style = "display: flex; justify-content: stretch; align-items: left; margin-bottom: 10px; gap: 10px; width: 100%;",
          actionButton(ns("prev_img"), "Previous"),
          actionButton(ns("next_img"), "Next"),
          plotOutput(ns("img_graph"), height = "40px", width = "100%")
        ),
        imageOutput(ns("img"), fill = TRUE)
      )
    })

    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet(options = leaflet::leafletOptions(maxZoom = 13)) %>%
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite", options = leaflet::providerTileOptions(attribution = "")) %>%
        leaflet::addProviderTiles("USGS.USTopo", group = "USGS", options = leaflet::providerTileOptions(attribution = "")) %>%
        leaflet::addProviderTiles("Stadia.StamenTerrain", group = "Terrain", options = leaflet::providerTileOptions(attribution = "")) %>%
        leaflet::addLayersControl(
          baseGroups = c("Satellite", "USGS", "Terrain"),
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
      filtered_images <- images$images[selection, ]
      if (nrow(filtered_images) == 0) {
        leaflet::leafletProxy("map", session) %>% leaflet::clearMarkerClusters()
      } else {
        leaflet::leafletProxy("map", session) %>%
          leaflet::clearMarkerClusters() %>%
          leaflet::clearMarkers() %>%
          leaflet::addCircleMarkers(
            data = filtered_images,
            lng = ~longitude,
            lat = ~latitude,
            layerId = ~image_id,
            group = "Images",
            radius = 5,
            color = "#3182bd",
            fillColor = "#3182bd",
            fillOpacity = 0.9,
            label = ~as.character(image_id),
            clusterOptions = if (isTRUE(input$cluster_map)) leaflet::markerClusterOptions(spiderfyDistanceMultiplier = 1.2, showCoverageOnHover = TRUE) else NULL
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
      selections$img_id <- input$map_marker_click$id
    })

    observe({
      req(selections$filter)
      filtered_ids <- images$images$image_id[selections$filter]
      if (!is.null(selections$img_id) && !(selections$img_id %in% filtered_ids)) {
        selections$img_id <- NA
      }
    })

    observe({
      req(selections$img_id)
      renderSelectedImage(selections$img_id)
      leaflet::leafletProxy("map", session) %>%
        leaflet::clearGroup("selected_marker") %>%
        leaflet::addCircleMarkers(
          lng = images$images$longitude[images$images$image_id == selections$img_id],
          lat = images$images$latitude[images$images$image_id == selections$img_id],
          radius = 12,
          color = selected_colour,
          fillColor = selected_colour,
          fillOpacity = 0.7,
          weight = 3,
          group = "selected_marker"
        )
    })

    # prev and next buttons for the image carousel
    observeEvent(input$prev_img, {
      images_filtered <- images$images[selections$filter & selections$in_view, ]
      current_index <- which(images_filtered$image_id == selections$img_id)
      if (length(current_index) == 0) {
        selections$img_id <- images_filtered$image_id[1]
        renderSelectedImage(selections$img_id)
        return()
      }
      if (length(current_index) > 0 && current_index > 1) {
        selections$img_id <- images_filtered$image_id[current_index - 1]
        renderSelectedImage(selections$img_id)
      }
    })

    observeEvent(input$next_img, {
      images_filtered <- images$images[selections$filter & selections$in_view, ]
      current_index <- which(images_filtered$image_id == selections$img_id)
      if (length(current_index) == 0) {
        selections$img_id <- images_filtered$image_id[1]
        renderSelectedImage(selections$img_id)
        return()
      }
      if (length(current_index) > 0 && current_index < nrow(images_filtered)) {
        selections$img_id <- images_filtered$image_id[current_index + 1]
        renderSelectedImage(selections$img_id)
      }
    })

    # Observe changes to the map bounds to update the in_view filter
    observe({
      req(input$map_bounds, images$images)
      bounds <- input$map_bounds
      in_bounds <- images$images$latitude <= bounds$north &
        images$images$latitude >= bounds$south &
        images$images$longitude <= bounds$east &
        images$images$longitude >= bounds$west
      selections$in_view <- in_bounds & selections$filter
    })

    # Render the mini timeline plot showing images in view
    observe({
      req(selections$in_view, selections$img_id)
      output$img_graph <- renderPlot({
        images_filtered <- images$images[selections$in_view, ]
        if (nrow(images_filtered) == 0) return(NULL)
        min_datetime <- min(images_filtered$datetime, na.rm = TRUE)
        max_datetime <- max(images_filtered$datetime, na.rm = TRUE)
        par(mar = c(2, 0, 0, 0))
        plot(
          images_filtered$datetime,
          rep(1, nrow(images_filtered)),
          type = "n", axes = FALSE, ylab = "",
          xlim = c(min_datetime, max_datetime),
          ylim = c(0.9, 1.1),
          xlab = "Date/Time (UTC-7)"
        )
        unique_dates <- unique(as.Date(images_filtered$datetime))
        n_ticks <- max(3, min(length(unique_dates), 7, na.rm = TRUE), na.rm = TRUE)
        x_ticks <- pretty(range(images_filtered$datetime, na.rm = TRUE), n = n_ticks)
        axis(1, at = x_ticks, labels = format(x_ticks, "%d-%b-%Y"), las = 0, cex.axis = 0.7)
        x_minor <- pretty(range(images_filtered$datetime, na.rm = TRUE), n = 10)
        x_minor <- setdiff(x_minor, x_ticks)
        axis(1, at = x_minor, labels = FALSE, tcl = -0.2, lwd = 0.5)
        abline(v = images_filtered$datetime, col = rgb(0.5, 0.5, 0.5, 0.5), lwd = 3)
        selected_idx <- which(images_filtered$image_id == selections$img_id)
        if (length(selected_idx) == 1) {
          abline(v = images_filtered$datetime[selected_idx], col = selected_colour, lwd = 3, lty = 2)
        }
      })
    })

    observe({
      req(input$dates, images$images)
      end_date <- as.POSIXct(paste0(input$dates[2], " 23:59"))
      date_filter <- images$images$datetime >= input$dates[1] & images$images$datetime <= end_date

      if (!is.null(input$months) && length(input$months) > 0) {
        month_filter <- format(as.Date(images$images$datetime), "%B") %in% input$months
      } else {
        month_filter <- TRUE
      }

      img_hour <- as.integer(format(as.POSIXct(images$images$datetime), "%H"))
      toy_filter <- img_hour >= input$tod[1] & img_hour <= input$tod[2]

      if (!is.null(input$img_type) && length(input$img_type) > 0) {
        type_filter <- images$images$image_type_id %in% input$img_type
      } else {
        type_filter <- TRUE
      }

      if (!is.null(input$tags) && length(input$tags) > 0) {
        tag_filter <- sapply(images$images$tags_list, function(v) { any(v %in% input$tags) })
      } else {
        tag_filter <- TRUE
      }

      selections$filter <- date_filter & month_filter & toy_filter & tag_filter & type_filter
      updateMap(selections$filter)
    })

    output$img_info <- renderText({
      req(selections$img_id)
      img_data <- images$images[images$images$image_id == selections$img_id, ]
      if (nrow(img_data) == 0) {
        return("No image selected.")
      }
      paste(
        "Image ID:", img_data$image_id, "\n",
        "Date/Time:", format(img_data$datetime, "%Y-%m-%d %H:%M:%S"), "\n",
        "Location ID:", img_data$location_id, "\n",
        "Latitude:", img_data$latitude, "\n",
        "Longitude:", img_data$longitude, "\n",
        "Image Type:", img_data$image_type, "\n",
        "Azimuth:", if ("azimuth" %in% names(img_data)) {
          az <- img_data$azimuth_true
          if (is.na(az)) {
            "N/A"
          } else {
            dirs <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N")
            idx <- round(az / 45) + 1
            paste0(az, "° (", dirs[idx], ")")
          }
        } else {
          "N/A"
        }, "\n",
        "Elevation (MSL):", if ("elevation_msl" %in% names(img_data)) img_data$elevation_msl_m else "N/A"
      )
    })

    observeEvent(input$refresh_images, {
      images$images <- DBI::dbGetQuery(
        session$userData$AquaCache,
        paste0("SELECT i.image_id, i.img_meta_id, i.datetime, i.latitude, i.longitude, i.location_id, i.tags, i.image_type AS image_type_id, it.image_type FROM files.images i LEFT JOIN files.image_types it on i.image_type = it.image_type_id WHERE datetime > '", as.POSIXct(input$dates[1], tz = "MST") - 7*60, "';")
      )
      attr(images$images$datetime, "tzone") <- "MST"
      images$unique_types <- images$images[!duplicated(images$images$image_type), c("image_type_id", "image_type")]
      images$images$tags_list <- lapply(images$images$tags, function(raw) {
        if (is.na(raw) || raw == "") return(character(0))
        inner <- gsub('["{}]', "", raw)
        strsplit(inner, ",", fixed = TRUE)[[1]]
      })
      updateSelectizeInput(session, "img_type", choices = stats::setNames(images$unique_types$image_type_id, images$unique_types$image_type))
      updateSelectizeInput(session, "tags", choices = unique(unlist(images$images$tags_list)))
    }, ignoreInit = TRUE)

    datesControl <- reactive(FALSE)
    observeEvent(input$dates, {
      if (datesControl()) {
        return()
        datesControl(TRUE)
      }
      if (as.POSIXct(input$dates[1], tz = "MST") - 7*60 < min(images$images$datetime, na.rm = TRUE)) {
        images$images <- DBI::dbGetQuery(
          session$userData$AquaCache,
          paste0("SELECT i.image_id, i.img_meta_id, i.datetime, i.latitude, i.longitude, i.location_id, i.tags, i.image_type AS image_type_id, it.image_type FROM files.images i LEFT JOIN files.image_types it on i.image_type = it.image_type_id WHERE datetime > '", as.POSIXct(input$dates[1], tz = "MST") - 7*60, "';")
        )
        attr(images$images$datetime, "tzone") <- "MST"
        images$unique_types <- images$images[!duplicated(images$images$image_type), c("image_type_id", "image_type")]
        images$images$tags_list <- lapply(images$images$tags, function(raw) {
          if (is.na(raw) || raw == "") return(character(0))
          inner <- gsub('["{}]', "", raw)
          strsplit(inner, ",", fixed = TRUE)[[1]]
        })
        updateSelectizeInput(session, "img_type", choices = stats::setNames(images$unique_types$image_type_id, images$unique_types$image_type))
        updateSelectizeInput(session, "tags", choices = unique(unlist(images$images$tags_list)))
      }
    }, ignoreInit = TRUE)

    observeEvent(input$load_additional_layers, {
      showNotification("Loading additional layers (30-120 seconds)...", type = "message")
      basins <- getVector(layer_name = "Drainage basins")
      #locations <- getVector(layer_name = "Locations")
      roads <- getVector(layer_name = "Roads")
      watercourses <- getVector(layer_name = "Watercourses")

      m <- leaflet::leafletProxy("map", session)
      if (!is.null(basins)) {
        if ("area" %in% names(basins)) {
          basins <- basins[order(basins$area, decreasing = TRUE), ]
        }
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


      # if (!is.null(locations)) {
      #   m <- m %>%
      #     leaflet::addCircleMarkers(
      #       data = locations,
      #       color = "#444444",
      #       weight = 2,
      #       opacity = 1,
      #       fillOpacity = 0.2,
      #       group = "Locations",
      #       label = ~as.character(feature_name),
      #       options = leaflet::markerOptions(interactive = FALSE, keyboard = FALSE, riseOnHover = TRUE)
      #     )
      # }

      
      if (!is.null(roads)) {
        m <- leaflet::leafletProxy("map", session)
        m <- m %>%
          leaflet::addPolylines(
            data = roads,
            color = "#FF0000",
            weight = 1,
            opacity = 0.5,
            group = "Roads",
            label = ~as.character(feature_name)
          )
      }

      if (!is.null(watercourses)) {
        m <- leaflet::leafletProxy("map", session)
        m <- m %>%
          leaflet::addPolylines(
            data = watercourses,
            color = "#134bc4",
            weight = 1,
            opacity = 0.5,
            group = "Watercourses",
            label = ~as.character(feature_name)
          )
      }


      m <- m %>%
        leaflet::addLayersControl(
          overlayGroups = c("Satellite", "USGS","Terrain", "Basins", "Watercourses", "Roads"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) %>%
        leaflet::hideGroup("Locations")
    })

    observeEvent(input$reset_view, {
      leaflet::leafletProxy("map", session) %>%
        leaflet::setView(lng = -135.0, lat = 64.0, zoom = 4)
    })
  }) # End of moduleServer
} # End of img server function
