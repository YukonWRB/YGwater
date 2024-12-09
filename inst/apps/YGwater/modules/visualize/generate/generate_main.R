# UI and server code for report generation tab. Modules are called depending on the plot type selected.

genUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    selectizeInput(ns("report_type"),
                   "Select a report or product to generate",
                   choices = c("Water Quality Report", "Snowpack Info Report", "Water Info Report", "Drainage Basins"),
                   selected = "Water Quality Report",
                   width = "80%"),
    uiOutput(ns("submoduleUI"))
  )
}

gen <- function(id, mdb_files, AquaCache) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Hide the water quality report if mdb_files is not available (at least until that data goes to AquaCache)
    if (is.null(mdb_files)) {
      updateSelectizeInput(session, "report_type", choices = c("Snowpack Info Report", "Water Info Report", "Drainage Basins"))
    }

    # Load the submodule server and UI based on the report type selected
    observeEvent(input$report_type, {
      if (input$report_type == "Water Quality Report") {
        output$submoduleUI <- renderUI({
          WQReportUI(ns("WQReport"))
        })
        WQReportServer("WQReport", mdb_files, AquaCache)
        
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
