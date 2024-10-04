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

    ns <- session$ns
    
    contData <- reactiveValues()
    discData <- reactiveValues()
    shareData <- reactiveValues()
    datums <- DBI::dbGetQuery(AquaCache, "SELECT l.location, dc.location_id, dc.datum_id_to, dc.conversion_m, dc.current, dl.datum_name_en FROM datum_conversions dc INNER JOIN locations l ON dc.location_id = l.location_id INNER JOIN datum_list dl ON dc.datum_id_to = dl.datum_id;")
      datums$datum_name_en <- gsub("GEODETIC SURVEY OF CANADA DATUM", "CGVD28 (assumed)", datums$datum_name_en)
      datums$datum_name_en <- gsub("CANADIAN GEODETIC VERTICAL DATUM 2013:EPOCH2010", "CGVD2013:2010", datums$datum_name_en)
      datums$datum_name_en <- gsub("APPROXIMATE", "approx.", datums$datum_name_en)
    shareData$datums <- datums
    
    # Load the submodule server and UI based on the plot type selected
    observeEvent(input$plot_type, {
      
      if (input$plot_type == "Discrete") {
        
        # discData$parameters_discrete <- DBI::dbGetQuery(AquaCache, "SELECT DISTINCT parameters.parameter_id, parameters.param_name FROM timeseries INNER JOIN parameters ON timeseries.parameter_id = parameters.parameter_id WHERE timeseries.category = 'discrete';")
        # discData$parameters_discrete <- discData$parameters_discrete[order(discData$parameters_discrete$param_name), ]
        
        output$submoduleUI <- renderUI({
          discretePlotUI(ns("discretePlot"))
        })
        discretePlotServer("discretePlot", EQWin, AquaCache)
        
      } else if (input$plot_type == "Continuous") {
        
        contData$all_ts <- DBI::dbGetQuery(AquaCache, "SELECT ts.timeseries_id, ts.location_id, ts.location, ts.parameter_id, ts.media_id, ts.category, ts.start_datetime, ts.end_datetime, loc.name FROM timeseries AS ts INNER JOIN locations AS loc ON ts.location_id = loc.location_id AND ts.location = loc.location WHERE ts.category = 'continuous';")
        contData$all_ts <- contData$all_ts[order(contData$all_ts$name), ]
        
        contData$parameters <- DBI::dbGetQuery(AquaCache, "SELECT DISTINCT parameters.parameter_id, parameters.param_name FROM timeseries INNER JOIN parameters ON timeseries.parameter_id = parameters.parameter_id WHERE timeseries.category = 'continuous';")
        contData$parameters <- contData$parameters[order(contData$parameters$param_name), ]
        contData$datums <- shareData$datums
        
        output$submoduleUI <- renderUI({
          continuousPlotUI(ns("continuousPlot"))
        })
        continuousPlotServer("continuousPlot", AquaCache = AquaCache, data = contData)
        
      } else if (input$plot_type == "Mix") {
        output$submoduleUI <- renderUI({
          mixPlotUI(ns("mixPlot"))
        })
        mixPlotServer("mixPlot", EQWin, AquaCache)
      }
    })
  }) # End of moduleServer
}
