# UI and server code for mapping tab

mapUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    selectizeInput(ns("map_type"),
                   label = "Map Type",
                   choices = c("Precipitation", "Parameters"),
                   selected = "Parameters"),
    uiOutput(ns("submoduleUI"))
  )
}

map <- function(id, EQWin, AquaCache) {
  
  moduleServer(id, function(input, output, session) {
    
    # Load the submodule UI based on the plot type selected
    output$submoduleUI <- renderUI({
      ns <- session$ns
      if (input$map_type == "Precipitation") {
        mapPrecipUI(ns(id))
      } else if (input$map_type == "Parameters") {
        mapParamUI(ns(id))
      }
    })
    
    # Load the submodule server based on the plot type selected
    observe({
      if (input$map_type == "Precipitation") {
        mapPrecipServer(id, AquaCache)
      } else if (input$map_type == "Parameters") {
        mapParamServer(id, AquaCache)
      }
    })
  }) # End of moduleServer
}
