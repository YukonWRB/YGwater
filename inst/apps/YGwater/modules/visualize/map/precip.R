mapPrecipUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      actionButton(ns("make_map"),
                   "Create Map")
    ),
    mainPanel(
      leaflet::leafletOutput(ns("map")),
      actionButton(ns("full_screen"),
                   "Full screen")
    )
  )
}

mapPrecipServer <- function(id, AquaCache, data, language) {
  moduleServer(id, function(input, output, session) {
    
  })
}

