library(shiny)

downloadDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("dataset"), "Choose a dataset:", 
                choices = c("Dataset 1", "Dataset 2")),
    downloadButton(ns("download"), "Download"),
    actionButton(ns("home"), "Home", icon = icon("home"))
  )
}


downloadData <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$download <- downloadHandler(
      filename = function() { paste(input$dataset, ".csv", sep = "") },
      content = function(file) {
        # In reality, fetch and write the selected dataset to 'file'
        write.csv(mtcars, file)  # Example dataset
      }
    )
    
    # Example of navigating back to the home tab
    observeEvent(input$home, {
      # This would need to be updated to reflect how you're handling navigation.
      # Direct client-side navigation or a server-side approach if needed.
      # For direct navigation without server logic, consider using `shinyjs` to trigger client-side actions or update your UI to navigate without needing a server callback.
    })
  })
}

