
mapViewUI <- function(id, translations) {
  ns <- NS(id)
  tagList(
    leaflet::leafletOutput(ns("map"), height = '80vh'),
    absolutePanel(id = ns("controls"), class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 100, left = "auto", right = 20, bottom = "auto",
                  # Panel content
                  h4("Map Filters", class = "panel-title"),
                  selectInput(ns("filter1"), "Filter 1", choices = c("choice 1", "choice 2")),
                  # You can add additional filter controls here
                  style = "opacity: 0.9; z-index: 400;") # Adjust styling as needed
  )
}


mapView <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addTiles() %>%  # Add default OpenStreetMap map tiles
        leaflet::setView(lng = -135.05, lat = 65.00, zoom = 5)  # Center on Yukon
    })
    
    observe({
      filterValue <- input$filter
      # Update map based on filterValue
      # This might involve re-querying your database or adjusting the displayed data
    })
    
  })
}
