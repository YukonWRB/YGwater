# UI and server code for add new location module

addTimeseriesUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    page_fluid(
      uiOutput(ns("ui"))
    )
  )
}

addTimeseries <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Show a placeholder text
    output$ui <- renderUI({
      tagList(
        h2("Add New Timeseries"),
        p("This module allows you to add new continuous data timeseries to the database.")
      )
    })
    
    # HEADS UP! find the modules which depend on timeseries. These will have cached data, which will need to be cleared when a new location or timeseries is added using the clear_cached function (R/app_cache.R)

  }) # End of moduleServer
}
