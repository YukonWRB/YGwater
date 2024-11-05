# UI and server code for data extraction tab. Modules are called depending on the data type selected.

dataUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    selectizeInput(ns("data_type"),
                   "Select a data type or product to extract",
                   choices = c("Discrete", "Continuous", "Spatial", "Document"),
                   selected = "Discrete"),
    uiOutput(ns("submoduleUI"))
  )
}

data <- function(id, AquaCache) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    data <- reactiveValues()
    
    # Load the submodule server and UI based on the plot type selected
    observeEvent(input$data_type, {
      
      if (input$data_type == "Discrete") {
        
        output$submoduleUI <- renderUI({
          discreteDataUI(ns("discreteData"))
        })
        discreteDataServer("discreteData", AquaCache)
        
      } else if (input$data_type == "Continuous") {
        
        output$submoduleUI <- renderUI({
          continuousDataUI(ns("continuousData"))
        })
        continuousDataServer("continuousData", AquaCache = AquaCache)
        
      } else if (input$data_type == "??") {

        output$submoduleUI <- renderUI({
          spatialDataUI(ns("spatialData"))
        })
        spatialDataServer("spatialData", AquaCache = AquaCache)
        
      } else if (input$data_type == "Document") {
        
        output$submoduleUI <- renderUI({
          documentDataUI(ns("documentData"))
        })
        documentDataServer("documentData", AquaCache = AquaCache)
      }
    })
    
  }) # End of moduleServer
}
