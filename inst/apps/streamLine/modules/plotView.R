plotUI <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("placeholder"))
  )
}

plot <- function(id, con, language) {
    moduleServer(id, function(input, output, session) {
      
      setBookmarkExclude(c())
      ns <- session$ns
      stop("This is here to test error catching.")
      
      output$placeholder <- renderText({
        paste0("This is a placeholder for the plot module.")
      })
    })
}
