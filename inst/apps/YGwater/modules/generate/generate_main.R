# UI and server code for report generation tab. Modules are called depending on the plot type selected.

generateUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    selectizeInput(ns("report_type"),
                   "Select a report or product to generate",
                   choices = c("WQ Exceedance Report", "Snowpack Info Report", "Water Info Report", "Drainage Basin"),
                   selected = "WQ Exceedance"),
    uiOutput(ns("submoduleUI"))
  )
}

generate <- function(id, EQWin, AquaCache) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Load the submodule server and UI based on the report type selected
    observeEvent(input$report_type, {
      
      if (input$report_type == "WQ Exceedance Report") {
        
        output$submoduleUI <- renderUI({
          exceedanceReportUI(ns("exceedanceReport"))
        })
        exceedanceReportServer("exceedanceReport", EQWin, AquaCache)
        
      } else if (input$report_type == "Snowpack Info Report") {
        
        output$submoduleUI <- renderUI({
          snowReportUI(ns("snowReport"))
        })
        snowReportServer("snowReport", AquaCache = AquaCache)
        
      } else if (input$report_type == "Water Info Report") {
        output$submoduleUI <- renderUI({
          waterReportUI(ns("waterReport"))
        })
        waterReportServer("waterReport", AquaCache)
      }
    })
  }) # End of moduleServer
}
