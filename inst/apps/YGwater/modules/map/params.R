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
                   NULL,
                   choices = c("Percent historic range", "Absolute")),
    ),
    mainPanel(
      leaflet::leafletOutput(ns("map")),
      actionButton(ns("full_screen"),
                   "Full screen")
    )
  )
}

mapParamServer <- function(id, AquaCache) {
  moduleServer(id, function(input, output, session) {
    
  })
}



