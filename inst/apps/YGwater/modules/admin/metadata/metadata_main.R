# UI and server code for basin generation module

# This will allow the user to select one or more points on a leaflet map and generate a basin polygon for each point. Basic layout: mainPanel has a leaflet map, sidebarPanel has a button to toggle point selection on/off, a list of points selected with coords, a button to begin delineation process, and a button to download generated polygons. A modal should open when the user first opens this module to explain how to use it. Data will come from the AquaCache database.

metadataUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    sidebarPanel(
      uiOutput(ns("location_selector")),
      actionButton(ns("summary"), "Summary (pop-up)"),  # This will be a pop-up or new tab for better viewing
      actionButton(ns("access"), "Access info"),
      actionButton(ns("infra_maintain"), "Infrastructure + maintenance info"),
      actionButton(ns("transmission"), "Transmitter setup info")
    ),
    mainPanel(
      DT::DTOutput(ns("metadata_overview"))
    )
  )
}

metadata <- function(id, AquaCache) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns # Use to create UI elements within the server function
    
    metadata <- reactiveValues(locations = DBI::dbGetQuery(AquaCache, "SELECT * FROM locations"))
    
    observe({
      output$controls <- renderUI({
        selectizeInput(ns("location"), label = "Location", choices = stats::setNames(metadata$locations$location_id, metadata$locations$location))
      })
    })
    
    observeEvent(input$summary, {
      
    })
  }) # End of moduleServer
}
