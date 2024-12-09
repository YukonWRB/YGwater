# UI and server code for main locations module

locsUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    selectizeInput(ns("select"), NULL, choices = stats::setNames(c("main", "meta", "new"), c("View/edit location data", "View/edit location metadata", "Add new location")), selected = "main"),
    # Placeholder divs for dynamically loaded UIs
    div(id = ns("main_placeholder"), style = "display: none;"),
    div(id = ns("meta_placeholder"), style = "display: none;"),
    div(id = ns("new_placeholder"), style = "display: none;")
    
  )
}

locs <- function(id, AquaCache) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    setBookmarkExclude()
    
    data <- reactiveValues(locs = DBI::dbGetQuery(AquaCache, "SELECT * FROM locations"),
                           networks = DBI::dbGetQuery(AquaCache, "SELECT * FROM networks"),
                           projects = DBI::dbGetQuery(AquaCache, "SELECT * FROM projects"),
                           location_types = DBI::dbGetQuery(AquaCache, "SELECT * FROM location_types"),
                           location_networks = DBI::dbGetQuery(AquaCache, "SELECT * FROM locations_networks"),
                           location_projects = DBI::dbGetQuery(AquaCache, "SELECT * FROM locations_projects"))
    
    # Store information to pass between modules
    subModuleOutputs <- reactiveValues() # Holds the stuff that needs to be output from the sub-modules back tot this server
    
    # Reactive value to track if submodule has been loaded
    submodules <- reactiveValues(main = FALSE,
                                 meta = FALSE,
                                 new = FALSE)
    
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
        subModuleOutputs <- locsMainServer("main", AquaCache, data)
      }
      
      # Metadata module
      if (input$select == "meta" && !submodules$meta) {
        submodules$meta <- TRUE
        insertUI(
          selector = paste0("#", ns("meta_placeholder")),
          where = "beforeEnd",
          ui = locsMetaUI(ns("meta"))
        )
        locsMetaServer("meta", AquaCache, data)
      }
      
      # Add new location module
      if (input$select == "new" && !submodules$new) {
        submodules$new <- TRUE
        insertUI(
          selector = paste0("#", ns("new_placeholder")),
          where = "beforeEnd",
          ui = locsNewUI(ns("new"))
        )
        submoduleOutputs <- locsNewServer("new", AquaCache, data)
        
        print(submoduleOutputs$new$added)
        
        observe({
          req(submoduleOutputs$new$added)
          if (subModuleOutputs$new$added) { # If a new location has been added, refresh the data
            data <- reactiveValues(locs = DBI::dbGetQuery(AquaCache, "SELECT * FROM locations"),
                                   networks = DBI::dbGetQuery(AquaCache, "SELECT * FROM networks"),
                                   projects = DBI::dbGetQuery(AquaCache, "SELECT * FROM projects"),
                                   location_types = DBI::dbGetQuery(AquaCache, "SELECT * FROM location_types"),
                                   location_networks = DBI::dbGetQuery(AquaCache, "SELECT * FROM locations_networks"),
                                   location_projects = DBI::dbGetQuery(AquaCache, "SELECT * FROM locations_projects"))
          }
        })
      }
      
      # Show only the relevant module using shinyjs
      shinyjs::hide(selector = paste0("#", ns("main_placeholder")))
      shinyjs::hide(selector = paste0("#", ns("meta_placeholder")))
      shinyjs::hide(selector = paste0("#", ns("new_placeholder")))
      shinyjs::show(selector = paste0("#", ns(paste0(input$select, "_placeholder"))))
    })
    
    # return(mainModuleOutputs)

  }) # End of moduleServer
}
