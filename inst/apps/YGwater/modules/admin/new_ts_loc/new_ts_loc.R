# UI and server code for new timeseries/location creation module


new_ts_locUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    sidebarPanel(
      
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
