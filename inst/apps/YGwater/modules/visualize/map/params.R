mapParamUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      selectizeInput(ns("map_param"),
                     "Select a parameter to map",
                     choices = "Placeholder"),
      numericInput(ns("within"),
                   "With data within ... days",
                   value = 1,
                   min = 0.1,
                   step = 0.1,
                   max = 365),
      radioButtons(ns("prct_abs"),
                   "Display as ...",
                   choices = c("Percent historic range", "Absolute")),
      actionButton(ns("create_map"),
                   "Create map")
    ),
    mainPanel(
      leaflet::leafletOutput(ns("map")),
      uiOutput(ns("full_screen_ui"))
      
    )
  )
}

mapParamServer <- function(id, AquaCache, data, language) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns  # Used to create UI elements in the server code

    observeEvent(input$create_map, {
      
      # Code to create the map...
      
      output$full_screen_ui <- renderUI({
        actionButton(ns("full_screen"), "Full Screen")
      })
    })
    
  })
}



