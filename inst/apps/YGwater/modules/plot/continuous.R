continuousPlotUI <- function(id) {
  ns <- NS(id)
  conditionalPanel(ns = ns,
                   condition = "input.plot_type == 'Continuous'",
                   actionButton(ns("make_plot"),
                                "Create Plot")
  )
}

continuousPlotServer <- function(input, output, session) {
  
}
