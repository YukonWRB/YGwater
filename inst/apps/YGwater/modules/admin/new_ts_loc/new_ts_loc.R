# UI and server code for basin generation module

# This will allow the user to select one or more points on a leaflet map and generate a basin polygon for each point. Basic layout: mainPanel has a leaflet map, sidebarPanel has a button to toggle point selection on/off, a list of points selected with coords, a button to begin delineation process, and a button to download generated polygons. A modal should open when the user first opens this module to explain how to use it. Data will come from the AquaCache database.

new_ts_locUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    sidebarPanel(
      actionButton(ns("toggle"), "Toggle point selection"),
      # Add some whitespace between buttons
      div(style = "height: 10px;"),
      uiOutput(ns("points")),
      actionButton(ns("delineate"), "Delineate basins"),
      div(style = "height: 10px;"),
      downloadButton(ns("download"), "Download basins")
    ),
    mainPanel(
      leaflet::leafletOutput(ns("map"))
    )
  )
}

new_ts_loc <- function(id, AquaCache) {
  
  moduleServer(id, function(input, output, session) {
    
    layers <- reactiveValues()
    
    ns <- session$ns # Use to create UI elements within the server function
    
  }) # End of moduleServer
}
