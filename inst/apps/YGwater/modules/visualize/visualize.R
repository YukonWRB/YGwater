# UI and server code for basin generation module

# This will allow the user to select one or more points on a leaflet map and generate a basin polygon for each point. Basic layout: mainPanel has a leaflet map, sidebarPanel has a button to toggle point selection on/off, a list of points selected with coords, a button to begin delineation process, and a button to download generated polygons. A modal should open when the user first opens this module to explain how to use it. Data will come from the aquacache.

vizUI <- function(id) {
  ns <- NS(id)
  fluidPage(

  )
}

viz <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    layers <- reactiveValues()
    
    ns <- session$ns # Use to create UI elements within the server function
    
  }) # End of moduleServer
}
