# UI and server code for plotting tab. Modules are called depending on the plot type selected.

plotUI <- function(id) {
  ns <- NS(id)
  fluidPage(
        selectizeInput(ns("plot_type"),
                       "Select a data type to plot",
                       choices = c("Discrete", "Continuous", "Mix"),
                       selected = "Discrete"),
        uiOutput(ns("submoduleUI"))
      )
}

plot <- function(id, EQWin, AquaCache) {
  
  moduleServer(id, function(input, output, session) {
    
    # Load the submodule UI based on the plot type selected
    output$submoduleUI <- renderUI({
      ns <- session$ns
      if (input$plot_type == "Discrete") {
        discretePlotUI(ns(id))
      } else if (input$plot_type == "Continuous") {
        continuousPlotUI(ns(id))
      } else if (input$plot_type == "Mix") {
        mixPlotUI(ns(id))
      }
    })
    
    # Load the submodule server based on the plot type selected
    observe({
      if (input$plot_type == "Discrete") {
        discretePlotServer(id, EQWin, AquaCache)
      } else if (input$plot_type == "Continuous") {
        continuousPlotServer(id, AquaCache)
      } else if (input$plot_type == "Mix") {
        mixPlotServer(id, EQWin, AquaCache)
      }
    })
  }) # End of moduleServer
}
