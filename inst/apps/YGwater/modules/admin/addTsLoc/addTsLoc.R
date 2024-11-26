# UI and server code for new timeseries/location creation module


addTsLoc_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    selectizeInput(ns("ts_loc"),
                   NULL,
                   choices = stats::setNames(c("loc", "ts"), c("Add new location", "Add new timeseries")),
                   selected = "loc"),
    conditionalPanel(
      condition = "input.ts_loc == 'loc'",
      uiOutput(ns("locUI")),
      condition = "input.ts_loc == 'ts'",
      uiOutput(ns("tsUI"))
    ),
    
  )
}

addTsLoc <- function(id, AquaCache) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns # Use to create UI elements within the server function
    
    # Get some data from the AquaCache
    moduleData <- reactiveValues(exist_locs = dbGetQueryDT(AquaCache, "SELECT location_id, location, name, name_fr FROM locations"),
                           loc_types = dbGetQueryDT(AquaCache, "SELECT * FROM location_types"),
                           owners_contributors = dbGetQueryDT(AquaCache, "SELECT * FROM owners_contributors"),
                           # limit documents to those that are data sharing agreements, which requires a join on table document_types
                           agreements = dbGetQueryDT(AquaCache, "SELECT document_id, name, description FROM documents WHERE type = (SELECT document_type_id FROM document_types WHERE document_type_en = 'data sharing agreement')"),
                           # parameters are linked to parameter_groups and parameter_sub_groups via parameter_relationships (to allow a many to many relationships)
                           parameters = dbGetQueryDT(AquaCache, "SELECT parameter_id, param_name FROM parameters"),
                           parameter_groups = dbGetQueryDT(AquaCache, "SELECT group_id, group_name, description FROM parameter_groups"),
                           parameter_sub_groups = dbGetQueryDT(AquaCache, "SELECT sub_group_id, sub_group_name, description FROM parameter_sub_groups"),
                           parameter_relationships = dbGetQueryDT(AquaCache, "SELECT * FROM parameter_relationships"),
                           media = dbGetQueryDT(AquaCache, "SELECT media_id, media_type FROM media_types"),
                           datums = dbGetQueryDT(AquaCache, "SELECT datum_id, datum_name_en FROM datum_list"),
                           networks = dbGetQueryDT(AquaCache, "SELECT network_id, name FROM networks"),
                           projects = dbGetQueryDT(AquaCache, "SELECT project_id, name FROM projects"))
    
    
    # Location add portion ######################################################
    output$locUI <- renderUI({
      tagList(
        textInput(ns("loc_code"), "Location code"),
        textInput(ns("loc_name"), "Location name"),
        textInput(ns("loc_name_fr"), "French location name"),
        selectizeInput(ns("loc_type"), "Location type", choices = stats::setNames(moduleData$loc_types$type_id, moduleData$loc_types$type)),
        numericInput(ns("lat"), "Latitude (decimal degrees)"),
        numdricInput(ns("long"), "Longitude (decimal degrees)"),
        selectizeInput(ns("viz"), "Public visibility", choices = stats::setNames(c("exact", "region", "jitter"), c("Exact", "Within general region", "At random within a 5 km radius of true location"))),
        selectizeInput(ns("owner"), "Owner", choices = stats::setNames(moduleData$owners_contributors$owner_contributor_id, moduleData$owners_contributors$name)),
        selectizeInput(ns("data_sharing_agreement"), "Data sharing agreement", choices = "placeholder"),
        textInput(ns("note"), "Note"),
        selectizeInput(ns("datum_id_from"), "Datum ID from", choices = stats::setNames(moduleData$datums$datum_id, moduleData$datums$datum_name_en), selected = 10),
        selectizeInput(ns("datum_id_to"), "Datum ID to", choices = stats::setNames(moduleData$datums$datum_id, moduleData$datums$datum_name_en)),
        numericInput(ns("elev"), "Elevation conversion (meters)"),
        selectizeInput(ns("network"), "Network", choices = stats::setNames(moduleData$networks$network_id, moduleData$networks$name), options = list(create = TRUE)),  # With a choice to allow users to add a network
        selectizeInput(ns("project"), "Project", choices = stats::setNames(moduleData$networks$project_id, moduleData$projects$name), options = list(create = TRUE))  # With a choice to allow users to add a project
      )
    })
    
    
    
    
    
    
    # Timeseries add portion ########################################################
    output$tsUI <- renderUI({
      tagList(
        selectizeInput(ns("loc_code_select"), "Location code", choices = stats::setNames(moduleData$exist_locs$location_id, moduleData$exist_locs$location)),
        selectizeInput(ns("loc_name_select"), "Location name", choices = stats::setNames(moduleData$exist_locs$location_id, moduleData$exist_locs$name)),
        dateInput(ns("start_date"), "Start date to look for data", value = "1900-01-01"),
        selectizeInput(ns("param"), "Parameter", choices = stats::setNames(moduleData$parameters$parameter_id, moduleData$parameters$param_name)),
        actionButton(ns("param_modal"), "Use filters to select parameter"),  # Loads a modal where the user can filter parameters based on their group and sub-group, On save, input$param is updated
        selectizeInput(ns("media"), )
        
        
      )
    })
        
        
  }) # End of moduleServer
}
