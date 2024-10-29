# UI and server code for mapping tab

mapUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    selectizeInput(ns("map_type"),
                   label = "Map Type",
                   choices = c("Monitoring locations", "Precipitation", "Other parameter values"),
                   selected = "Monitoring locations"),
    uiOutput(ns("submoduleUI"))
  )
}

map <- function(id, EQWin, AquaCache) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns

    # Reactive value to store the selected submodule type
    submodule <- reactiveVal(NULL)
    
    # Load the submodule server and UI based on the plot type selected
    observeEvent(input$map_type,{
      if (is.null(submodule())) { # Nothing has been loaded yet and submodule is NULL
        if (input$map_type == "Precipitation") {
          output$submoduleUI <- renderUI({
            mapPrecipUI(ns("precip"))
          })
          submodule("Precipitation")
          mapPrecipServer("precip", AquaCache)
        } else if (input$map_type == "Other parameter values") {
          output$submoduleUI <- renderUI({
            mapParamUI(ns("param"))
          })
          mapParamServer("param", AquaCache)
          submodule("Parameters")
        }
      } else { # Submodule has been loaded already, so only redo if we are calling a different submodule
        if (input$map_type == "Precipitation" && submodule() != "Precipitation") {
          output$submoduleUI <- renderUI({
            mapPrecipUI(ns("precip"))
          })
          submodule("Precipitation")
          mapPrecipServer("precip", AquaCache)
        } else if (input$map_type == "Other parameter values" && submodule() != "Parameters") {
          output$submoduleUI <- renderUI({
            mapParamUI(ns("param"))
          })
          mapParamServer("param", AquaCache)
          submodule("Parameters")
        }
      }
      
    })
    
  }) # End of moduleServer
}
