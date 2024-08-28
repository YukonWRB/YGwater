# To be created: will allow user to make plots combining discrete and continuous data.
mixPlotUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      actionButton(ns("make_plot"),
                   "Create Plot")
    ),
    mainPanel(
      plotly::plotlyOutput(ns("plot"), width = "100%", height = "800px", inline = TRUE),
      actionButton(ns("full_screen"),
                   "Full screen")
    )
  )
}

mixPlotServer <- function(id, EQWin, AquaCache) {
  moduleServer(id, function(input, output, session) {
    
  })
}

