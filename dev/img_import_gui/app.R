# Load necessary libraries
library(shiny)
library(leaflet)
library(jpeg)
library(exifr)
library(dplyr)
library(DT)


# Define a named list of colors
color_list <- list(
  "selected" = "cyan",
  "unselected" = "black",
  "click" = "magenta",
  "other" = "green"
)

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

# Load image file paths from the "data" directory
files <- list.files("data/", pattern="*jpg", recursive = TRUE, full.names = TRUE)
dat <- read_exif(files)

# Function to create a new data frame with GPS data and new empty columns
initialize_csv <- function(dat, filename) {
  # Assertions to check the presence of necessary columns
  stopifnot("FileName" %in% colnames(dat))
  stopifnot("GPSLatitude" %in% colnames(dat))
  stopifnot("GPSLongitude" %in% colnames(dat))
  stopifnot("GPSAltitude" %in% colnames(dat))
  stopifnot("DateTimeOriginal" %in% colnames(dat))

  dat_with_tags_notes <- dat %>%
    select(FileName, DateTimeOriginal, GPSLatitude, GPSLongitude, GPSAltitude) %>%
    rename(filename = FileName, latitude = GPSLatitude, longitude = GPSLongitude, altitude = GPSAltitude, datetime = DateTimeOriginal) %>%
    mutate(Tags = "", Notes = "")
  
  # Write the new data frame to a CSV file
  write.csv(dat_with_tags_notes, filename, row.names = FALSE)
}

preprocess_exif <- function(dat) {
  dat <- dat %>%
    select(SourceFile, FileName, DateTimeOriginal, GPSLatitude, GPSLongitude, GPSAltitude) %>%
    rename(sourcefile = SourceFile, filename = FileName, latitude = GPSLatitude, longitude = GPSLongitude, altitude = GPSAltitude, datetime = DateTimeOriginal) %>%
    mutate(Tags = "", Notes = "") %>%
    filter(latitude != 0 & longitude != 0)
    
  return(dat)
}

# Define a dictionary to map checkbox choices to tags
tag_list = list(" ", "Head", "Tail", "Smooth Ice")

# Create a temporary directory and CSV file
temp_dir <- tempdir()
csv_file <- file.path("image_data.csv")

# Call the function to create the data frame and write to CSV
initialize_csv(dat, csv_file)

load_data <- function() {
  return(read.csv(csv_file, stringsAsFactors = FALSE))
}

# Define the UI layout
ui <- fluidPage(
  br(),
  mainPanel(
    fluidRow(
      column(width = 6,
        fileInput("files", "Select image files (ctrl+A to select all)", multiple = TRUE, accept = NULL)
      ),
      column(width = 6,
        br(),
        br(),
        br(),
        uiOutput("edit_button")
      )
    ),
    fluidRow(
      column(width = 6,
        imageOutput("imgprev"),
        textOutput("image_index")
      ),
      column(width = 6,
        leafletOutput("map", width = "100%"),
        fluidRow(
          actionButton("update_pos", "Update location of selection"),
          actionButton("clear_pos", "Clear")
        )
      )
    ),
  fluidRow(
    column(
      width = 4,
      actionButton("select_all", "Select All"),
      actionButton("clear_selection", "Clear Selection"),
      br(),
      br(),
      selectInput("image_tags", "Select tags:", choices = tag_list, multiple = FALSE),
      textAreaInput("notes", "Notes:", value = "", width = "100%", height = "100px"),
      actionButton("save_notes", "Save Notes")
    ),
    column(width = 8, 
      DT::dataTableOutput("table")
    )
  )
)
)

# Increase the maximum upload size to 30MB
options(shiny.maxRequestSize = 30 * 1024^2)

# Define the server logic
server <- function(input, output, session) {
  # Reactive value to track the current image index
  imgid <- reactiveVal(1)
  
  # Reactive value to track whether the map was clicked (since afaik cannot reset map click input directly..)
  # unintuitively, this is initialized as TRUE, since we always check that click is not null before proceeding
  mapclicked <- reactiveVal(TRUE)

  edit_mode <- reactiveVal(FALSE)
  # Initiaze dat
  dat <- reactiveValues(df = data.frame())

  # Render placeholder image
  output$imgprev <- renderImage({
    filename <- normalizePath("welcome.jpg")
    list(src = filename, contentType = 'image/jpeg', width = "100%", height = "100%")
  }, deleteFile = FALSE)



  # Render an empty leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      setView(lng = -135.0568, lat = 60.7212, zoom = 10)
  })

  # Observe file input and update the data table
  observeEvent(input$files, {
    files <- input$files
    if (is.null(files)) {
      return(NULL)
    }

    dat$df <- read_exif(files$datapath)
    dat$df$FileName <- files$name

    # Preprocess the exif data to remove images with missing data and add new attributes
    dat$df = preprocess_exif(dat$df)

    # Warn user if images are filtered
    if (length(dat$df$filename) < length(files$datapath)) {
      showNotification(
        "Some images do not have GPS data and will not be displayed.",
        type = "warning",
        duration = 5
      )
    }

    # Once the table is loaded, render the shiny elements
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
          select(-sourcefile) %>%
          mutate(across(where(is.numeric), ~ round(., 3))) %>%
          mutate(datetime = format(as.POSIXct(datetime, format="%Y:%m:%d %H:%M:%S"), "%d-%b-%Y %H:%M")),
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


  # Function to issue a modal warning if edit mode is FALSE
  issue_edit_mode_warning <- function() {
      showModal(modalDialog(
        title = "Edit Mode Disabled",
        "Please enable edit mode to make changes.",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Close")
        )
      ))
  }


  output$edit_button <- renderUI({
    actionButton("edit", "Edit Mode", icon = icon("edit"), 
           style = if (edit_mode()) "background-color: #007BFF; color: white;" else "background-color: white; color: black;")
  })

  # Observe edit button click and toggle edit mode
  observeEvent(input$edit, {
    edit_mode(!edit_mode())
    if (edit_mode()) {
      showNotification("Edit mode enabled.", type = "message", duration = 3)
    } else {
      showNotification("Edit mode disabled.", type = "message", duration = 3)
      leafletProxy("map") %>% clearGroup("click_marker") %>% clearGroup("click_line")
    }
  })

  # Observe changes in the data table and update the CSV file
  # NOTE: should edits be lockable via a toggle?
  observeEvent(input$table_cell_edit, {
    info <- input$table_cell_edit
    dat$df[info$row, info$col] <- info$value
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

    # update the map to reflect deselection
    updateMapView()
    leafletProxy(mapId = "map") %>% clearGroup("photos")
    addCirclesToMap(selection = seq_len(nrow(dat$df)), color=color_list$unselected)
  })

  # Observe row selection in the data table and update the imgid
  observeEvent(input$table_rows_selected, {


    # update lines to reflect seleciton (showing geoference corrections)
    clearLinesFromMap()

    if (edit_mode() == TRUE) {
      addLinesToMap()
    }

    # update map view coordinates and zoom level
    updateMapView()

    # update the displayed image upon each new row selection
    if (length(input$table_rows_selected) > 1) {
      imgid(input$table_rows_selected[length(input$table_rows_selected)])
    } else {
      imgid(input$table_rows_selected)
    }

    if (is.null(input$table_rows_selected)) {
      updateMapView()
      leafletProxy(mapId = "map") %>% clearGroup("photos")
      addCirclesToMap(selection = seq_len(nrow(dat$df)), color = color_list$unselected)
      }
  })

  

  # Observe changes in the drop-down input and update the tags on save button press
  observeEvent(input$image_tags, {
    if (length(input$table_rows_selected) > 0) {
      for (row in input$table_rows_selected) {
        dat$df$Tags[row] <- input$image_tags
      }
      
      # Reset the dropdown menu and reload table (to show updated tags)
      updateSelectInput(session, "image_tags", selected = " ")
      renderDataTable()
    }
  })

  # Observe changes in the text area input and update the notes on save button press
  observeEvent(input$save_notes, {
    if (length(input$table_rows_selected) > 0) {
      for (row in input$table_rows_selected) {
        dat$df$Notes[row] <- input$notes
      }
      
      # Reset the text area input and reload table (to show updated notes)
      updateTextAreaInput(session, "notes", value = "")
      renderDataTable()
    }
  })

  # Observe clear button click and remove map click marker and lines
  observeEvent(input$clear_pos, {
    leafletProxy("map") %>%
      clearGroup("click_marker") %>%
      clearGroup("click_line")
    mapclicked(FALSE)
  })

  # Set the view of the map to the current image's coordinates on button press
  updateMapView <- function() {

    # If no rows are selected, set the view to the centroid of all points
    selection <- input$table_rows_selected
    if (length(selection) == 0) {
      selection <- seq_len(nrow(dat$df))
    }


    # if the map is clicked, include the clicked point in the centroid calculation
    if (edit_mode() && !is.null(input$map_click) && mapclicked()) {
      # Calculate the centroid of selection
      centroid_lat <- mean(c(dat$df$latitude[selection], input$map_click$lat), na.rm = TRUE)
      centroid_lon <- mean(c(dat$df$longitude[selection], input$map_click$lng), na.rm = TRUE)
    } 
    else {
      # Calculate the centroid of selection
      centroid_lat <- mean(dat$df$latitude[selection], na.rm = TRUE)
      centroid_lon <- mean(dat$df$longitude[selection], na.rm = TRUE)
    }


    # Infer zoom level based on the spread of the points
    zoom <- infer_zoom_level(dat$df$latitude[selection], dat$df$longitude[selection])

    # Update the map view
    leafletProxy(mapId = "map") %>%
      setView(lng = centroid_lon, lat = centroid_lat, zoom = zoom)
  }

  # Render the image plot
  # This output will update automatically based on the imgid() React var
  renderImagePlot <- function() {
    output$imgprev <- renderImage({
      filename <- normalizePath(dat$df$sourcefile[imgid()])
      width <- session$clientData$output_imgprev_width
      height <- session$clientData$output_imgprev_height 
      img <- readJPEG(filename, native = TRUE)
      aspect_ratio <- dim(img)[2] / dim(img)[1]
      height <- session$clientData$output_imgprev_height
      width <- height * aspect_ratio
      list(src = filename, contentType = 'image/jpeg', width = width, height = height)
    }, deleteFile = FALSE)
  }


  # Render the image position text, also based on React var
  renderImageIndex <- function() {
    output$image_index <- renderText({
      paste("Image", imgid(), "of", nrow(dat$df))
    })
  }

  # Function to render the leaflet map
  renderLeafletMap <- function() {
    output$map <- renderLeaflet({
      leaflet(data = dat$df) %>%
        addProviderTiles(providers$Esri.WorldImagery) %>%
        addLayersControl(
          baseGroups = c("World Topo Map","World Imagery"),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "World Aerial") %>%
        addProviderTiles(providers$Esri.WorldTopoMap, group = "World Topo Map") %>%
        addCircleMarkers(
          lng = dat$df$longitude,
          lat = dat$df$latitude,
          radius = 2,
          color = color_list$unselected,
          fill = TRUE,
            fillOpacity = 0.5,
            popup = ~paste("Filename:", filename, "<br>",
                   "Date/Time:", datetime, "<br>",
                   "Latitude:", round(latitude, 5), "<br>",
                   "Longitude:", round(longitude, 5), "<br>",
                   "Altitude:", round(altitude, 2), "m")
        ) %>%
        addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE, maxWidth = 100, metric = TRUE, updateWhenIdle = TRUE)) %>%
        htmlwidgets::onRender("function(el, x) {
          var credits = el.querySelector('.leaflet-control-attribution');
          if (credits) {
            credits.style.fontSize = '4px';
          }
        }")
    })
  }


  # Observe clicks on existing markers and update the data table selection based on the clicked marker
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (!is.null(click)) {
      # Find the index of the clicked marker in the data table

      # CURRENTLY THERE'S A BUG WHERE AN EDITED POINT LOCATION CANNOT BE SELECTED VIA CLICK
      # WHILE THE LAT LONS SEEM TO MATCH UP, THE WHICH FUNCTION RETURNS INTEGER(0)
      # FOR NOW, THIS FUNCTIONALITY IS DISABLED (BY ADDING A NULL CASE)
      clicked_index <- which(
        dat$df$longitude == click$lng & dat$df$latitude == click$lat
      )




      if (length(clicked_index) == 0) {
        clicked_index <- NULL
      }

      if (!is.null(clicked_index)) {
        if (clicked_index %in% input$table_rows_selected) {
          selected_rows <- setdiff(input$table_rows_selected, clicked_index)
        } else {
          selected_rows <- c(input$table_rows_selected, clicked_index)
        }
        proxy <- dataTableProxy("table")
        selectRows(proxy, selected_rows)
      }
    }
  })

  
  # Generic-ish function to add circles to the map based on selection
  addCirclesToMap <- function(selection, color, radius=2.5) {
    leafletProxy(mapId = "map") %>%
      addCircleMarkers(
        lng = dat$df$longitude[selection],
        lat = dat$df$latitude[selection],
        radius = radius,
        color = color,
        fillColor = color,
        fill = TRUE,
        fillOpacity = 1,
        group = "photos"
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
    mapclicked(TRUE)
  })

  # Observe map clicks, add marker on click, lines from click to selection
  observeEvent(input$map_click, {
    if (edit_mode() == TRUE) {

    clearLinesFromMap()
    click <- input$map_click
    if (!is.null(click)) {
      leafletProxy("map") %>%
        clearGroup("click_marker") %>%
        addCircleMarkers(lng = click$lng, lat = click$lat, radius = 5, color = color_list$click, fill = TRUE, fillOpacity = 0.8, popup = paste("Lat:", round(click$lat, 3), "Lng:", round(click$lng, 3)), group = "click_marker")
      # Add a line between the clicked point and the selected point
      addLinesToMap()
    }
    }

  })

  addLinesToMap <- function() {
    click = input$map_click
    if (!is.null(click) && mapclicked() == TRUE) {
      if (length(input$table_rows_selected) > 0) {
        selected_rows <- input$table_rows_selected
        for (row in selected_rows) {
          selected_lat <- dat$df$latitude[row]
          selected_lon <- dat$df$longitude[row]
          
          leafletProxy("map") %>%
            addPolylines(
              lng = c(selected_lon, click$lng),
              lat = c(selected_lat, click$lat),
              color = color_list$click,
              weight = 2,
              dashArray = "5, 10",
              group = "click_line"
            )
        }
      }
    }
  }


  # Function to clear lines from the map
  clearLinesFromMap <- function() {
    leafletProxy("map") %>% clearGroup("click_line")
  }

error_no_map_click <- function() {
showModal(modalDialog(
          title = "Error",
          "Please click on the map to update the position.",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close")
          )
        ))
}


error_no_rows_selected <- function() {
showModal(modalDialog(
          title = "Error",
          "Please select row(s) in the data table to update the position.",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close")
          )
        ))
}
warning_multiple_image_edit <- function() {
showModal(modalDialog(
            title = "Confirm georeference changes",
            "Are you sure you want to update the coordinates for multiple images?",
            easyClose = FALSE,
            footer = tagList(
              modalButton("Cancel"),
              actionButton("confirm_update", "Confirm")
            )
          ))
}

  # Confirmation action for modal popup triggered by multiple simultaneous location updates
  observeEvent(input$confirm_update, {
    removeModal()
    # update the coordinates in the data table
    dat$df$latitude[input$table_rows_selected] <- input$map_click$lat
    dat$df$longitude[input$table_rows_selected] <- input$map_click$lng
    renderDataTable()
    renderLeafletMap()
  })
  
  # Cancel action for modal popup triggered by multiple simultaneous location updates
  observeEvent(input$Cancel, {
    removeModal()
  }
  
  )

  # Push georefence updates to the data table
  observeEvent(input$update_pos, {
    if (edit_mode() == FALSE) {
      issue_edit_mode_warning()
      return()
    }
    # make sure a map click has been made
    if (!is.null(input$map_click) && mapclicked() == TRUE) {
      # make sure at least one row is selected
      if (length(input$table_rows_selected) > 0) {

        # if there are multiple images selected, throw up a cofirmation warning
        if (length(input$table_rows_selected) > 1) {
          warning_multiple_image_edit()
        }
        else{
        # update the coordinates in the data table
        dat$df$latitude[input$table_rows_selected] <- input$map_click$lat
        dat$df$longitude[input$table_rows_selected] <- input$map_click$lng
        renderDataTable()
        renderLeafletMap()
        }
      } 
      
      # case for no selection
      else {
        error_no_rows_selected()
      }
    }

    # case for no click
    else {
      error_no_map_click()
    }
    
  }
  )
  }

# Run the Shiny app
shinyApp(ui = ui, server = server)

