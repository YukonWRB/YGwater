# UI and server code for mapping tab

mapUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    selectizeInput(ns("map_type"),
                   label = "Map Type",
                   choices = stats::setNames(c("locs", "precip", "params"), c("Monitoring locations", "Precipitation", "Other parameter values")),
                   selected = "Monitoring locations"),
    uiOutput(ns("submoduleUI"))
  )
}

map <- function(id, AquaCache, language) {
  
  moduleServer(id, function(input, output, session) {
    
    setBookmarkExclude(c("reset", "map_bounds", "map_center", "map_zoom", "map_marker_mouseover", "map_marker_mouseout", "map_marker_click"))
    
    ns <- session$ns
    
    # Load common mapping data
    data <- reactiveValues(
      locations = dbGetQueryDT(AquaCache, "SELECT location, name, name_fr, latitude, longitude, location_id, geom_id, visibility_public, location_type FROM locations"),
      timeseries = dbGetQueryDT(AquaCache, "SELECT ts.timeseries_id, ts.location_id, p.param_name, p.param_name_fr, m.media_type, ts.category, ts.period_type, ts.start_datetime, ts.end_datetime, z FROM timeseries AS ts LEFT JOIN parameters AS p ON ts.parameter_id = p.parameter_id LEFT JOIN media_types AS m ON ts.media_id = m.media_id"),
      projects = dbGetQueryDT(AquaCache, "SELECT p.* FROM projects AS p WHERE EXISTS (SELECT 1 FROM locations_projects lp WHERE lp.project_id = p.project_id);"),
      networks =  dbGetQueryDT(AquaCache, "SELECT n.* FROM networks AS n WHERE EXISTS (SELECT 1 FROM locations_networks ln WHERE ln.network_id = n.network_id);"),
      locations_projects = dbGetQueryDT(AquaCache, "SELECT * FROM locations_projects;"),
      locations_networks = dbGetQueryDT(AquaCache, "SELECT * FROM locations_networks;"),
      media_types = dbGetQueryDT(AquaCache, "SELECT p.* FROM media_types AS p WHERE EXISTS (SELECT 1 FROM timeseries t WHERE t.media_id = p.media_id);"),
      has_image_series = dbGetQueryDT(AquaCache, "SELECT DISTINCT location_id FROM images_index;"),
      has_documents = dbGetQueryDT(AquaCache, "SELECT DISTINCT locations.location_id FROM locations JOIN documents_spatial ON locations.geom_id = documents_spatial.geom_id JOIN documents ON documents_spatial.document_id = documents.document_id;")
    )
    
    # Store information to pass between modules
    subModuleOutputs <- reactiveValues() # Holds the stuff that needs to be output from the sub-modules back tot this server
    mainModuleOutputs <- reactiveValues() # Hold the stuff that needs to be output from this module back to the main server

    # Reactive value to store the selected submodule type
    submodule <- reactiveVal(NULL)
    
    # Load the submodule server and UI based on the plot type selected
    observeEvent(input$map_type, {
      if (is.null(submodule())) { # Nothing has been loaded yet and submodule is NULL
        if (input$map_type == "precip") {
          output$submoduleUI <- renderUI({
            mapPrecipUI(ns("precip"))
          })
          submodule("precip")
          mapPrecipServer("precip", AquaCache, data, language)
        } else if (input$map_type == "params") {
          output$submoduleUI <- renderUI({
            mapParamUI(ns("params"))
          })
          mapParamServer("params", AquaCache, data, language)
          submodule("params")
        } else if (input$map_type == "locs") {
          output$submoduleUI <- renderUI({
            mapLocsUI(ns("locs"))
          })
          subModuleOutputs$locs <- mapLocsServer("locs", AquaCache, data, language)
          submodule("locs")
          observe({
            if (!is.null(subModuleOutputs$locs$change_tab)) {
              mainModuleOutputs$change_tab <- subModuleOutputs$locs$change_tab
            }
          })
          
        }
      } else { # Submodule has been loaded already, so only redo if we are calling a different submodule
        if (input$map_type == "precip" && submodule() != "precip") {
          output$submoduleUI <- renderUI({
            mapPrecipUI(ns("precip"))
          })
          submodule("precip")
          mapPrecipServer("precip", AquaCache, data, language)
        } else if (input$map_type == "params" && submodule() != "params") {
          output$submoduleUI <- renderUI({
            mapParamUI(ns("param"))
          })
          mapParamServer("params", AquaCache, data, language)
          submodule("params")
        } else if (input$map_type == "locs" && submodule() != "locs") {
          output$submoduleUI <- renderUI({
            mapLocsUI(ns("locs"))
          })
          subModuleOutputs$locs <- mapLocsServer("locs", AquaCache, data, language)
          submodule("locs")
          observe({
            if (!is.null(subModuleOutputs$locs$change_tab)) {
              mainModuleOutputs$change_tab <- subModuleOutputs$locs$change_tab
            }
          })
          
        }
      }
    }) # End of observeEvent that loads submodules

    return(mainModuleOutputs)

  }) # End of moduleServer
}
