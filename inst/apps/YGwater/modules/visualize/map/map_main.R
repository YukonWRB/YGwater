# UI and server code for mapping tab

mapUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    selectizeInput(ns("map_type"),
                   label = NULL,
                   choices = stats::setNames(c("locs", "precip", "params"), c("Monitoring locations", "Precipitation", "Other parameter values")),
                   selected = "Monitoring locations"),
    conditionalPanel(ns = ns,
      condition = "input.map_type == 'locs'",
      mapLocsUI(ns("locs"))
    ),
    conditionalPanel(ns = ns,
      condition = "input.map_type == 'precip'",
      mapPrecipUI(ns("precip"))
    ),
    conditionalPanel(ns = ns,
      condition = "input.map_type == 'params'",
      mapParamUI(ns("params"))
    )
  )
}

map <- function(id, AquaCache, language) {
  
  moduleServer(id, function(input, output, session) {
    
    setBookmarkExclude(c("reset", "map_bounds", "map_center", "map_zoom", "map_marker_mouseover", "map_marker_mouseout", "map_marker_click"))
    
    ns <- session$ns
    
    # Load common mapping data
    data <- reactiveValues(
      locations = dbGetQueryDT(AquaCache, "SELECT location, name, name_fr, latitude, longitude, location_id, geom_id, visibility_public, location_type FROM locations"),
      timeseries = dbGetQueryDT(AquaCache, "SELECT ts.timeseries_id, ts.location_id, p.param_name, p.param_name_fr, m.media_type, ts.media_id, ts.parameter_id, ts.category, ts.period_type, ts.start_datetime, ts.end_datetime, z FROM timeseries AS ts LEFT JOIN parameters AS p ON ts.parameter_id = p.parameter_id LEFT JOIN media_types AS m ON ts.media_id = m.media_id"),
      projects = dbGetQueryDT(AquaCache, "SELECT p.* FROM projects AS p WHERE EXISTS (SELECT 1 FROM locations_projects lp WHERE lp.project_id = p.project_id);"),
      networks =  dbGetQueryDT(AquaCache, "SELECT n.* FROM networks AS n WHERE EXISTS (SELECT 1 FROM locations_networks ln WHERE ln.network_id = n.network_id);"),
      locations_projects = dbGetQueryDT(AquaCache, "SELECT * FROM locations_projects;"),
      locations_networks = dbGetQueryDT(AquaCache, "SELECT * FROM locations_networks;"),
      media_types = dbGetQueryDT(AquaCache, "SELECT p.* FROM media_types AS p WHERE EXISTS (SELECT 1 FROM timeseries t WHERE t.media_id = p.media_id);"),
      parameters = dbGetQueryDT(AquaCache, "SELECT DISTINCT p.parameter_id, p.param_name, p.param_name_fr, p.unit_default, pr.group_id, pr.sub_group_id FROM parameters AS p RIGHT JOIN timeseries AS ts ON p.parameter_id = ts.parameter_id LEFT JOIN parameter_relationships AS pr ON p.parameter_id = pr.parameter_id;"),
      parameter_groups = dbGetQueryDT(AquaCache, "SELECT DISTINCT pg.group_id, pg.group_name, pg.group_name_fr FROM parameter_groups AS pg LEFT JOIN parameter_relationships AS pr ON pg.group_id = pr.group_id WHERE pr.parameter_id IN (SELECT DISTINCT parameter_id FROM timeseries);"),
      parameter_sub_groups = dbGetQueryDT(AquaCache, "SELECT psg.sub_group_id, psg.sub_group_name, psg.sub_group_name_fr FROM parameter_sub_groups AS psg LEFT JOIN parameter_relationships AS pr ON psg.sub_group_id = pr.sub_group_id WHERE pr.parameter_id IN (SELECT DISTINCT parameter_id FROM timeseries);"),
      has_image_series = dbGetQueryDT(AquaCache, "SELECT DISTINCT location_id FROM images_index;"),
      has_documents = dbGetQueryDT(AquaCache, "SELECT DISTINCT locations.location_id FROM locations JOIN documents_spatial ON locations.geom_id = documents_spatial.geom_id JOIN documents ON documents_spatial.document_id = documents.document_id;")
    )
    
    # Store information to pass between modules
    subModuleOutputs <- reactiveValues() # Holds the stuff that needs to be output from the sub-modules back tot this server
    mainModuleOutputs <- reactiveValues() # Hold the stuff that needs to be output from this module back to the main server
    
    # Reactive value to store the selected submodule type
    submodules <- reactiveValues(precip = FALSE,
                                 params = FALSE,
                                 locs = FALSE)
    
    # Load the submodule server and UI based on the plot type selected
    observeEvent(input$map_type, {
      if (input$map_type == "precip") {
        if (!submodules$precip) {
          submodules$precip <- TRUE
          mapPrecipServer("precip", AquaCache, data, language)
        }
      } else if (input$map_type == "params") {
        if (!submodules$params) {
          submodules$params <- TRUE
          mapParamServer("params", AquaCache, data, language)
        }
      } else if (input$map_type == "locs") {
        if (!submodules$locs) {
          submodules$locs <- TRUE
          mapLocsServer("locs", AquaCache, data, language)
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
