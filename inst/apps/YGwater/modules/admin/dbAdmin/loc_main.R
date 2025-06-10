# UI and server code for main locations module

locsUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    selectizeInput(ns("select"), NULL, choices = stats::setNames(c("main", "meta", "new_loc", "new_ts"), c("View/edit location data", "View/edit location metadata", "Add new location", "Add new timeseries at a location")), selected = "main"),
    hr(style = "border-top: 1px solid #000000;"),
    # Placeholder divs for dynamically loaded UIs
    div(id = ns("main_placeholder"), style = "display: none;"),
    div(id = ns("meta_placeholder"), style = "display: none;"),
    div(id = ns("new_loc_placeholder"), style = "display: none;"),
    div(id = ns("new_ts_placeholder"), style = "display: none;")
    
  )
}

locs <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    
    data <- reactiveValues(locs = DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM locations"),
                           networks = DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM networks"),
                           projects = DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM projects"),
                           location_types = DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM location_types"),
                           location_networks = DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM locations_networks"),
                           location_projects = DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM locations_projects"))
    
    # Store information to pass between modules
    subModuleOutputs <- reactiveValues() # Holds the stuff that needs to be output from the sub-modules back to this server
    
    # Reactive value to track if submodule has been loaded
    submodules <- reactiveValues(main = FALSE,
                                 meta = FALSE,
                                 new_loc = FALSE,
                                 new_ts = FALSE)
    
    # Observe input to dynamically load UI and server logic
    observeEvent(input$select, {
      # Main locations module
      if (input$select == "main" && !submodules$main) {
        submodules$main <- TRUE
        # Dynamically insert the UI
        insertUI(
          selector = paste0("#", ns("main_placeholder")),
          where = "beforeEnd",
          ui = locsMainUI(ns("main"))
        )
        subModuleOutputs$main <- locsMainServer("main", data)
      }
      
      # Metadata module
      if (input$select == "meta" && !submodules$meta) {
        submodules$meta <- TRUE
        insertUI(
          selector = paste0("#", ns("meta_placeholder")),
          where = "beforeEnd",
          ui = locsMetaUI(ns("meta"))
        )
        subModuleOutputs$meta <- locsMetaServer("meta", data)
      }
      
      # Add new location module
      if (input$select == "new_loc" && !submodules$new_loc) {
        submodules$new_loc <- TRUE
        insertUI(
          selector = paste0("#", ns("new_loc_placeholder")),
          where = "beforeEnd",
          ui = locsNewLocUI(ns("new_loc"))
        )
        subModuleOutputs$new_loc <- locsNewLocServer("new_loc")
        

        observe({
          req(subModuleOutputs$new_loc$added)
          if (subModuleOutputs$new_loc$added) { # If a new location has been added, refresh the data
            data <- reactiveValues(locs = DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM locations"),
                                   networks = DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM networks"),
                                   projects = DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM projects"),
                                   location_types = DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM location_types"),
                                   location_networks = DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM locations_networks"),
                                   location_projects = DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM locations_projects"))
          }
        })
      }
      
      # Add new timeseries module
      if (input$select == "new_ts" && !submodules$new_ts) {
        submodules$new_ts <- TRUE
        insertUI(
          selector = paste0("#", ns("new_ts_placeholder")),
          where = "beforeEnd",
          ui = locsNewTSUI(ns("new_ts"))
        )
        subModuleOutputs$new_ts <- locsNewTSServer("new_ts")

      }
      
      # Show only the relevant module using shinyjs
      shinyjs::hide(selector = paste0("#", ns("main_placeholder")))
      shinyjs::hide(selector = paste0("#", ns("meta_placeholder")))
      shinyjs::hide(selector = paste0("#", ns("new_loc_placeholder")))
      shinyjs::hide(selector = paste0("#", ns("new_ts_placeholder")))
      shinyjs::show(selector = paste0("#", ns(paste0(input$select, "_placeholder"))))
    })
    
    # return(mainModuleOutputs)

  }) # End of moduleServer
}
