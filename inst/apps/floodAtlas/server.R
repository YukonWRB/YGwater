#' The floodAtlas app server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
  
  # Initial setup #############################################################
  
  # Parse URL query parameters on app load
  observeEvent(session, {
    query <- parseQueryString(session$clientData$url_search)
    
    # Set input values based on URL parameters if they exist
    if (!is.null(query$loc_code)) {
      updateTextInput(session, "loc_code", value = query$loc_code)
    }
    if (!is.null(query$param_code)) {
      updateNumericInput(session, "param_code", value = as.numeric(query$param_code))
    }
    if (!is.null(query$start_dt)) {
      updateTextInput(session, "start_dt", value = query$start_dt)
    }
    if (!is.null(query$end_dt)) {
      updateTextInput(session, "end_dt", value = query$end_dt)
    }
    if (!is.null(query$lang)) {
      updateTextInput(session, "lang", value = query$lang)
    }
  }, once = TRUE)
  
  # Automatically update URL every time an input changes
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  # setBookmarkExclude(c())
  
  # Update the query string
  onBookmarked(updateQueryString)
  
  # Make an observer that gets triggered when the URL changes
  observe({
    
    print(input$loc_code)
    print(input$param_code)
    print(input$start_dt)
    print(input$end_dt)
    print(input$lang)
    # Check that all necessary inputs are not NULL before proceeding. This is the case when first initialized!
    req(input$loc_code, input$param_code, input$start_dt, input$end_dt, input$lang)

    tryCatch({
      plot <- plotTimeseries(location = input$loc_code,
                             parameter = as.numeric(input$param_code),
                             start_date = input$start_dt,
                             end_date = input$end_dt,
                             historic_range = TRUE,
                             datum = FALSE,
                             filter = 20,
                             lang = input$lang,
                             line_scale = 1,
                             axis_scale = 1,
                             legend_scale = 1,
                             gridx = FALSE,
                             gridy = FALSE,
                             con = pool)
      shinyjs::hide("error")
      shinyjs::show("plot_plotly")
      output$plot_plotly <- plotly::renderPlotly(plot)
    }, error = function(e) {
      shinyjs::hide("plot_plotly")
      shinyjs::show("error")
      output$error <- renderText({
        paste("An error occurred: ", e$message)
      })
    })
  })
  
}
