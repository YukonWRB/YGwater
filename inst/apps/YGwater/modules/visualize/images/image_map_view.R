# This module allows users to view images by clicking on points on a map. It is possible to filter images based on several attributes: tags, image types, ??


imgMapViewUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    # Top row with filters (collapsible using bslib accordion)
    uiOutput(ns("accordion")),
    uiOutput(ns("sidebar_page"))
  )
}

imgMapView <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$accordion <- renderUI({
      accordion(
        open = ns("filters"),
        accordion_panel(
          id = ns("filters"),
          title = "Filters",
          dateRangeInput(ns("dates"),
                         label = tr("date_range_lab", language$language),
                         start = Sys.Date() - 14,
                         end = Sys.Date(),
                         language = language$abbrev,
                         separator = tr("date_sep", language$language))
        )
      )
    })
    
    output$sidebar_page <- renderUI({
      page_sidebar( # contains a map on the left and the selected image on the right
        sidebar = sidebar( # leaflet map
          title = "Map",
          leaflet::leafletOutput(ns("map"), height = "100vh"),
          width = "40%",
          bg = "#f8f8f8",
          position = "left",
          open = TRUE,
        ),
        "Main content"
        # imageOutput(ns("img"), fill = TRUE)
      )
    })
    
    output$map <- leaflet::renderLeaflet({
      print("rendering map")
      leaflet::leaflet(options = leaflet::leafletOptions(maxZoom = 18)) %>%
        leaflet::addTiles() %>%
        leaflet::addProviderTiles("Esri.WorldTopoMap", group = "Topographic") %>%
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
        leaflet::addLayersControl(baseGroups = c("Topographic", "Satellite")) %>%
        leaflet::addScaleBar(position = "bottomleft", 
                             options = leaflet::scaleBarOptions(imperial = FALSE)) %>%
        leaflet::setView(lng = -135.05, lat = 64.00, zoom = 5)
    })
    
    # output$img <- renderImage({
    # 
    # }, deleteFile = FALSE)
    
  }) # End of moduleServer
} # End of img server function
