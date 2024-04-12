imgUI <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("placeholder"))
  )
}

img <- function(id, con, language) {
  moduleServer(id, function(input, output, session) {
    
    setBookmarkExclude(c())
    ns <- session$ns
    
    output$placeholder <- renderText({
      paste0("This is a placeholder for the image module.")
    })
  })
}
