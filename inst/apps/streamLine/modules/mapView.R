
mapViewUI <- function(id, translations) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
      .leaflet-left .leaflet-control{
        visibility: hidden;
      }
    "))
    ),
    leaflet::leafletOutput(ns("map"), height = '80vh'),
    absolutePanel(id = ns("controls"), class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 150, left = "auto", right = "auto", bottom = "auto",
                  # Panel content
                  selectInput(ns("data_type_filter"), "Data Type", choices = c("All")),
                  selectInput(ns("parameter_filter"), "Parameter", choices = c("All")),
                  selectInput(ns("project_filter"), "Project", choices = c("All")),
                  selectInput(ns("network_filter"), "Network", choices = c("All")),
                  # You can add additional filter controls here
                  style = "opacity: 0.9; z-index: 400;") # Adjust styling as needed
  )
}


mapView <- function(id, con, translations) {
  moduleServer(id, function(input, output, session) {
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::addTiles() %>% 
        leaflet::addProviderTiles("Esri.WorldTopoMap") %>%
        leaflet::setView(lng = -135.05, lat = 65.00, zoom = 5)  %>% # Center on Yukon
        htmlwidgets::onRender(
          "function(el, x) {
          L.control.zoom({position:'bottomright'}).addTo(this);
        }")
    })
    
    observe({
      filterValue <- input$filter
      # Update map based on filterValue
      # This might involve re-querying your database or adjusting the displayed data
    })
    
  })
}
