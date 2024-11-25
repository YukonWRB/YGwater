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
    moduleData <- reactiveValues(exist_locs = dbGetQueryDT(AquaCache, "SELECT location, name, name_fr FROM locations"),
                           loc_types = dbGetQueryDT(AquaCache, "SELECT * FROM location_types"),
                           owners_contributors = dbGetQuery(AquaCache, "SELECT * FROM owners_contributors")
                           agreements = dbGetQuery(AquaCache, "SELECT "))
    
    output$locUI <- renderUI({
      tagList(
        textInput(ns("loc_code"), "Location Code"),
        textInput(ns("loc_name"), "Location name"),
        textInput(ns("loc_name_fr"), "French location name"),
        selectizeInput(ns("loc_type"), "Location type", choices = stats::setNames(moduleData$loc_types$type_id, moduleData$loc_types$type)),
        numericInput(ns("lat"), "Latitude (decimal degrees)"),
        numdricInput(ns("long"), "Longitude (decimal degrees)"),
        selectizeInput(ns("viz"), "Public visibility", choices = stats::setNames(c("exact", "region", "jitter"), c("Exact", "Within general region", "At random within a 5 km radius of true location"))),
        selectizeInput(ns("owner"), "Owner", choices = stats::setNames(moduleData$owners_contributors$owner_contributor_id, moduleData$owners_contributors$name),
        selectizeInput(ns("data_sharing_agreement"), "Data sharing agreement", choices = "placeholder"),
        textInput(ns("note"), "Note"),
        selectizeInput(ns("datum_id_from"), "Datum ID from", choices = "placeholder"),
        selectizeInput(ns("datum_id_to"), "Datum ID to", choices = "placeholder"),
        numericInput(ns("elev"), "Elevation conversion (meters)"),
        selectizeInput(ns("network"), "Network", choices = "placeholder"),
        selectizeInput(ns("project"), "Project", choices = "placeholder")
      )
    })
    
    output$tsUI <- renderUI({
      tagList(
        
      )
    })
    
    
  }) # End of moduleServer
}
