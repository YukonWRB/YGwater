# UI and server code for report generation tab. Modules are called depending on the plot type selected.

reportsUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    selectizeInput(ns("type"),
                   "Select a report or product to generate",
                   choices = stats::setNames(
                     # c("WQ", "snowInfo", "waterInfo", "basins"), 
                     # c("Water Quality Report", "Snowpack Info Report", "Water Info Report", "Drainage Basins")
                     c("WQ", "snowInfo", "waterInfo"), 
                     c("Water Quality Report", "Snowpack Info Report", "Water Info Report")
                   ),
                   selected = "WQ",
                   width = "80%"),
    # placeholder divs for dynamically loaded UIs
    div(id = ns("WQ_placeholder"), style = "display: none;"),
    div(id = ns("snowInfo_placeholder"), style = "display: none;"),
    div(id = ns("waterInfo_placeholder"), style = "display: none;"),
    div(id = ns("basins_placeholder"), style = "display: none;"),
  )
}

reports <- function(id, mdb_files,language) {
  
  moduleServer(id, function(input, output, session) {
    
    setBookmarkExclude(c())
    
    ns <- session$ns
    
    # Reactive value to track if submodule has been loaded
    submodules <- reactiveValues(
      WQ = FALSE,
      snowInfo = FALSE,
      waterInfo = FALSE
      # basins = FALSE
    )
    
    # Hide the water quality report if mdb_files is not available (at least until that data goes to AquaCache)
    if (is.null(mdb_files)) {
      updateSelectizeInput(session, 
                           "type", 
                           choices = stats::setNames(
                             # c("snowInfo", "waterInfo", "basins"), 
                             # c("Snowpack Info Report", "Water Info Report", "Drainage Basins")
                             c("snowInfo", "waterInfo"), 
                             c("Snowpack Info Report", "Water Info Report")
                           ),
      )
    }
    
    
    # Observe input to dynamically load UI and server logic, without reloading.
    observeEvent(input$type, {
      # WQ module
      if (input$type == "WQ" && !submodules$WQ) {
        submodules$WQ <- TRUE
        # Dynamically insert the UI
        insertUI(
          selector = paste0("#", ns("WQ_placeholder")),
          where = "beforeEnd",
          ui = WQReportUI(ns("WQReport"))
        )
        WQReportServer("WQReport", mdb_files, language = language)
      } 
      
      # snowInfo module
      if (input$type == "snowInfo" && !submodules$snowInfo) {
        submodules$snowInfo <- TRUE
        # Dynamically insert the UI
        insertUI(
          selector = paste0("#", ns("snowInfo_placeholder")),
          where = "beforeEnd",
          ui = snowInfoUI(ns("snowInfo"))
        )
        snowInfoServer("snowInfo", language = language)
      }
      
      # waterInfo module
      if (input$type == "waterInfo" && !submodules$waterInfo) {
        submodules$waterInfo <- TRUE
        # Dynamically insert the UI
        insertUI(
          selector = paste0("#", ns("waterInfo_placeholder")),
          where = "beforeEnd",
          ui = waterInfoUI(ns("waterInfo"))
        )
        waterInfoServer("waterInfo", language = language)
      }
      

      # Show only the relevant module using shinyjs
      shinyjs::hide(selector = paste0("#", ns("WQ_placeholder")))
      shinyjs::hide(selector = paste0("#", ns("snowInfo_placeholder")))
      shinyjs::hide(selector = paste0("#", ns("waterInfo_placeholder")))
      # shinyjs::hide(selector = paste0("#", ns("basins_placeholder")))
      shinyjs::show(selector = paste0("#", ns(paste0(input$type, "_placeholder"))))
    })

  }) # End of moduleServer
}
