#' The floodAtlas app server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd


# http://127.0.0.1:5637/?_inputs_&info=0&yrs=2024&param_code=1165&loc_code="09AB004"&lang="en"

app_server <- function(input, output, session) {
  
  # Initial setup #############################################################
  
  # Remove irrelevant bookmarks
  setBookmarkExclude(c("plot_plotly", "error", "info", "redo", ".clientValue-default-plotlyCrosstalkOpts", "plotly_afterplot-A", "plotly_hover-A", "plotly_relayout-A", "plotly_doubleclick-A"))
  
  # Automatically update URL every time an input changes
  observe({
    req(input$loc_code, input$param_code, input$yrs, input$lang)
    session$doBookmark()
  })

  # Update the query string
  onBookmarked(updateQueryString)
  
    # Clean and save parameters into the URL bookmark
  # onBookmarked(function(state) {
  #   state$values$loc_code <- gsub("[\"\\\\]", "", input$loc_code)
  #   state$values$param_code <- input$param_code
  #   state$values$yrs <- paste(input$yrs, collapse = ",")
  #   state$values$lang <- gsub("[\"\\\\]", "", input$lang)
  # })

  # Parse URL query parameters on app load
  params <- reactiveValues()
  observeEvent(session, {
    query <- parseQueryString(session$clientData$url_search)
    
    # Set input values based on URL parameters if they exist
    if (!is.null(query$loc_code)) {
      params$loc_code <- query$loc_code
      updateTextInput(session, "loc_code", value = params$loc_code)
    }
    if (!is.null(query$param_code)) {
      params$param_code <- as.numeric(query$param_code)
      updateNumericInput(session, "param_code", value = params$param_code)
    }
    if (!is.null(query$lang)) {
      sub <-  gsub("[[:punct:]]", "", query$lang)
      params$lang <- sub
      updateTextInput(session, "lang", value = params$lang)
      updateActionButton(session, "info", label = if (params$lang == "en") "Plot info" else "Info sur le graphique")
      updateSelectizeInput(session, "yrs", label = if (params$lang == "en") "Add years" else "Ajouter des années")
    }
    if (!is.null(query$yrs)) {
      sub_yrs <- gsub("[[:punct:]]", "", query$yrs)
      params$yrs <- as.numeric(sub_yrs)
    }
    
    if (!is.null(query$loc_code)) {
      sub_loc <- gsub("[[:punct:]]", "", query$loc_code)
      params$loc_code <- sub_loc
      sub_param <- gsub("[[:punct:]]", "", query$param_code)
      params$param_code <- as.numeric(sub_param)
      # Find the years of record for the location
      tsid <- DBI::dbGetQuery(pool, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", params$loc_code, "' AND parameter_id = ",  params$param_code, ";"))[1,1]
      yrs <- DBI::dbGetQuery(pool, paste0("SELECT DISTINCT EXTRACT(YEAR FROM date) AS year FROM measurements_calculated_daily_corrected WHERE timeseries_id = ", tsid, " ORDER BY year DESC;"))
      updateSelectizeInput(session, "yrs", choices = yrs$year, selected = params$yrs)
      }
  }, once = TRUE)
  
  
  # Observers run after initial app load ######################################
  
  # Debounce the 'yrs' input
  debounced_years <- reactive({
    input$yrs
  }) %>% debounce(500) # delay in ms
  
  # Update the years if the user changes them
  observeEvent(debounced_years(), {
    params$yrs <- debounced_years()
  })
  
  # Show a modal with explanatory info when the user clicks the info button
  observeEvent(input$info, {
    if (params$lang == "en") {
      showModal(modalDialog(
        title = "Information",
        HTML("<ul>
              <li>These plots (hydrographs) allow you to plot up to 10 years of data to compare water level or flow traces to each other and to historical ranges.</li>
              <li>Expect a delay when changing the number of years plotted; this prevents re-rendering the plot each time a click is made.</li>
              <li>The term 'IQR' refers to the interquartile range, which is the range of values between the 25th and 75th percentiles of the data (i.e. the 'typical' range).</li>
              <li>The 'Min-Max' range is the full range of daily mean values.</li>
              <li>The IQR and min/max ranges are calculated using all data <i><b>prior</i> </b>to the last year selected, giving the most recent year historical context.</li>
              <li>A filter is applied to the data to remove extreme outliers and negative values.</li>
            </ul>"),
        easyClose = TRUE,
        footer = NULL
      ))
    } else if (params$lang == "fr") {
      showModal(modalDialog(
        title = "Information",
        HTML("<ul>
              <li>Ces graphiques (hydrographes) vous permettent de tracer jusqu'à 10 ans de données pour comparer les traces de niveau d'eau ou de débit les unes aux autres et aux plages historiques.</li>
              <li>Prévoyez un délai lors du changement du nombre d'années tracées; cela évite de redessiner le graphique à chaque clic.</li>
              <li>Le terme 'EIQ' fait référence à l'écart interquartile, qui est la plage de valeurs entre les 25e et 75e percentiles des données (c'est-à-dire la plage 'typique').</li>
              <li>La plage 'Min-Max' est la plage complète des valeurs moyennes journalières.</li>
              <li>L'EIQ et la plage min/max sont calculées en utilisant toutes les données <i><b>antérieures</i></b> à la dernière année sélectionnée, donnant un contexte historique à l'année la plus récente.</li>
              <li>Un filtre est appliqué aux données pour éliminer les valeurs aberrantes extrêmes ou négatives.</li>
            </ul>"),
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      showModal(modalDialog(
        title = "Error",
        HTML("<ul>
              <li>An error occurred: Language not recognized.</li>
            </ul>"),
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  
  
  # Make an observer that gets triggered when the parameters change
  observe({
    # Check that all necessary inputs are not NULL before proceeding. This is the case when first initialized!
    req(params$loc_code, params$param_code, params$yrs, params$lang)

    tryCatch({
      plot <- plotOverlap(location = params$loc_code,
                                sub_location = NULL,
                                parameter = params$param_code,
                                startDay = 1,
                                endDay = 365,
                                years = params$yrs,
                                datum = FALSE,
                                filter = 20,
                                lang = params$lang,
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
        if (input$lang == "en") {
          paste("An error occurred: ", e$message)
        } else if (input$lang == "fr") {
          paste("Une erreur s'est produite: ", e$message)
        }
      })
    })
  })
  
  # plot <- reactive({
  #   req(params$loc_code, params$param_code, params$yrs, params$lang)
  #   print("creating plot")
  #   future::future(
  #     plotOverlap(location = params$loc_code,
  #                 sub_location = NULL,
  #                 parameter = params$param_code,
  #                 startDay = 1,
  #                 endDay = 365,
  #                 years = params$yrs,
  #                 datum = FALSE,
  #                 filter = 20,
  #                 lang = params$lang,
  #                 line_scale = 1,
  #                 axis_scale = 1,
  #                 legend_scale = 1,
  #                 gridx = FALSE,
  #                 gridy = FALSE,
  #                 con = pool)
  #   ) # End future
  # }) # End reactive
  # 
  # output$plot_plotly <- plotly::renderPlotly({
  #   print("rendering plot")
  #   plot()
  # })
  
}
