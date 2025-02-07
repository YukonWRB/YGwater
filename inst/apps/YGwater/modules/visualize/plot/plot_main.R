# UI and server code for plotting tab. Modules are called depending on the plot type selected.

plotUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    selectizeInput(ns("plot_type"),
                   "Select a data type to plot",
                   choices = c("Discrete", "Continuous", "Mix"),
                   selected = "Discrete"),
    # placeholder divs for dynamically loaded UIs
    div(id = ns("discrete_placeholder"), style = "display: none;"),
    div(id = ns("continuous_placeholder"), style = "display: none;"),
    div(id = ns("mix_placeholder"), style = "display: none;")
  )
}

plot <- function(id, mdb_files, language, windowDims) {
  
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    
    # Reactive to track if the data has been loaded yet (when user changes tabs it doesn't reload each time)
    loaded <- reactiveValues(contData = FALSE, discData = FALSE, shareData = FALSE, submodule_discrete = FALSE, submodule_continuous = FALSE, submodule_mix = FALSE)
    
    contData <- reactiveValues()
    discData <- reactiveValues()
    shareData <- reactiveValues()
    
    if (!loaded$shareData) {
      datums <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT l.location, dc.location_id, dc.datum_id_to, dc.conversion_m, dc.current, dl.datum_name_en FROM datum_conversions dc INNER JOIN locations l ON dc.location_id = l.location_id INNER JOIN datum_list dl ON dc.datum_id_to = dl.datum_id;")
      datums$datum_name_en <- gsub("GEODETIC SURVEY OF CANADA DATUM", "CGVD28 (assumed)", datums$datum_name_en)
      datums$datum_name_en <- gsub("CANADIAN GEODETIC VERTICAL DATUM 2013:EPOCH2010", "CGVD2013:2010", datums$datum_name_en)
      datums$datum_name_en <- gsub("APPROXIMATE", "approx.", datums$datum_name_en)
      shareData$datums <- datums
      loaded$shareData <- TRUE
    }

    # Load the submodule server and UI based on the plot type selected
    observeEvent(input$plot_type, {
      
      if (input$plot_type == "Discrete" && !loaded$submodule_discrete) {
        
        # discData$parameters_discrete <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT DISTINCT parameters.parameter_id, parameters.param_name FROM timeseries INNER JOIN parameters ON timeseries.parameter_id = parameters.parameter_id;")
        # discData$parameters_discrete <- discData$parameters_discrete[order(discData$parameters_discrete$param_name), ]
        
        insertUI(
          selector = paste0("#", ns("discrete_placeholder")),
          where = "beforeEnd",
          ui = discretePlotUI(ns("discretePlot"))
        )
        loaded$submodule_discrete <- TRUE
        discretePlotServer("discretePlot", mdb_files, language, windowDims)
      } 
      
      if (input$plot_type == "Continuous" && !loaded$submodule_continuous) {
        
        contData$all_ts <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT ts.timeseries_id, ts.location_id, ts.location, ts.parameter_id, ts.media_id, ts.start_datetime, ts.end_datetime, loc.name FROM timeseries AS ts INNER JOIN locations AS loc ON ts.location_id = loc.location_id AND ts.location = loc.location;")
        contData$all_ts <- contData$all_ts[order(contData$all_ts$name), ]
        
        contData$parameters <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT DISTINCT parameters.parameter_id, parameters.param_name FROM timeseries INNER JOIN parameters ON timeseries.parameter_id = parameters.parameter_id;")
        contData$parameters <- contData$parameters[order(contData$parameters$param_name), ]
        contData$datums <- shareData$datums
        
        insertUI(
          selector = paste0("#", ns("continuous_placeholder")),
          where = "beforeEnd",
          ui = continuousPlotUI(ns("continuousPlot"))
        )
        loaded$submodule_continuous <- TRUE
        continuousPlotServer("continuousPlot", data = contData, language = language, windowDims = windowDims)
      } 
      
      if (input$plot_type == "Mix") {
        
        insertUI(
          selector = paste0("#", ns("mix_placeholder")),
          where = "beforeEnd",
          ui = mixPlotUI(ns("mixPlotUI"))
        )
        loaded$submodule_mix <- TRUE
        mixPlotServer("mixPlot", mdb_files, language, windowDims)
      }
      
      # Show only the relevant module using shinyjs
      shinyjs::hide(selector = paste0("#", ns("discrete_placeholder")))
      shinyjs::hide(selector = paste0("#", ns("continuous_placeholder")))
      shinyjs::hide(selector = paste0("#", ns("mix_placeholder")))
      shinyjs::show(selector = paste0("#", ns(paste0(tolower(input$plot_type), "_placeholder"))))
    })
  }) # End of moduleServer
}
