plotUI <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("placeholder"))
  )
}

plot <- function(id, con, language, restoring, data) {
    moduleServer(id, function(input, output, session) {
      
      setBookmarkExclude(c())
      ns <- session$ns
      
      output$placeholder <- renderText({
        paste0("This is a placeholder for the plot module.")
      })
    })
}
