plotUI <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("placeholder"))
  )
}

plot <- function(id, con, language, restoring, data) {
    moduleServer(id, function(input, output, session) {
      
      setBookmarkExclude(c())
      ns <- session$ns
      
      output$placeholder <- renderUI({
        HTML("This is a placeholder for the plot and map generation module. <br> <br>
             This module will allow the user to do two things: <br> <br>
             1. Select a single parameter and generate a map of that parameter, with points color coded to represent the percent of historical range. This will be a pop-up map with three filters: one for the parameter, one for the date (so a user could go back in time to see what the parameter looked like), and one for 'within x days of selected date', which will enable viewing data homogenously even when it may be sporaticaly missing. <br>
             At the same time it will be possible for users to select points/lines/polygons to plot, such as watersheds and snow bulletin polygons. <br> <br>
             2. Select a one or more timeseries and generate a plot for it/them. To narrow down to the desired timeseries there will be filters like those used in the 'Data' tab and a table; when the user clicks on a timeseries and hits 'plot' they'll either get one plot (if that's all we can do) or get a selection for the plot type if more than one option is suitable. For multiple row selection, limits will be imposed on the number of parameters able to be plotted so that the plot only has two y scales (so two different parameters only), and not too many traces also.")
      })
    })
}
