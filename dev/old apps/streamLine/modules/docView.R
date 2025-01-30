docUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("placeholder"))
  )
}

doc <- function(id, con, language, restoring, data) {
  moduleServer(id, function(input, output, session) {
    
    setBookmarkExclude(c())
    ns <- session$ns
    
    output$placeholder <- renderUI({
      HTML("This is a placeholder for the document module. <br> <br>
             The idea here is that we can share documents such as audits, research papers, flood maps, reports, etc in a way that's easy to search. Since the database is spatially-aware and documents can be associated with points, polygons, and/or lines, it's possible to search for documents associated with a location or within a certain distance of it, within a drainage basin, or within a user's arbitrarily drawn polygon or point. This functionality would likely be implemented with a map that pops up when the user clicks an action button. <br>
           It'll also be possible to search for documents by type (i.e. audit, well log, map, etc.), author, published date, or name. It's likely that all of these filters (including the map search) will be available to the user with available choices cross-updating depending on the user's selections.")
    })
  })
}
