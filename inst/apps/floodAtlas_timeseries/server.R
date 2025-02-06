#' The floodAtlas timeseries app server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
  
  # Initial setup #############################################################
  
  # Remove irrelevant bookmarks
  setBookmarkExclude(c("plot", "error", "info", "redo", ".clientValue-default-plotlyCrosstalkOpts", "plotly_afterplot-A", "plotly_hover-A", "plotly_relayout-A", "plotly_doubleclick-A"))
  
  # Automatically update URL every time an input changes
  observe({
    req(input$loc_code, input$param_code, input$lang)
    session$doBookmark()
  })
  
  output$visible_buttons <- renderUI({
    fluidRow(
      class = "button-row",
      actionButton("info", label = "Info", style = 'margin-bottom: 15px'),
    )
  })
  
  # Update the query string
  onBookmarked(updateQueryString)

  # Parse URL query parameters on app load from URL and trigger plot creation
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
    }
    
    if (!is.null(query$loc_code)) {
      sub_loc <- gsub("[[:punct:]]", "", query$loc_code)
      params$loc_code <- sub_loc
      sub_param <- gsub("[[:punct:]]", "", query$param_code)
      params$param_code <- as.numeric(sub_param)
    }
    
    # Trigger the plot creation
    params$render <- TRUE
  }, once = TRUE)
  
  
  # Observers run after initial app load ######################################
  
  # Show a modal with explanatory info when the user clicks the info button
  observeEvent(input$info, {
    if (params$lang == "en") {
      showModal(modalDialog(
        title = "Information",
        HTML("<ul>
              <li>This plots shows of water level or flow at full resolution (5 minutes to 1 hour) for the past 30 days.</li>
              <li>The 'Typical' range is the interquartile range, which is the range of values between the 25th and 75th percentiles of the data.</li>
              <li>The 'Historic' range is the full range of daily mean values.</li>
              <li>The typical and historic ranges are calculated using all data <i><b>prior</i> </b>to the last year selected, giving relevant historical context.</li>
              <li>A filter is applied to the data to remove extreme outliers and negative values.</li>
            </ul>"),
        easyClose = TRUE,
        footer = NULL
      ))
    } else if (params$lang == "fr") {
      showModal(modalDialog(
        title = "Information",
        HTML("<ul>
              <li>Ce graphique démontre le niveau d'eau ou débit durant les 30 derniers jours .</li>
              <li>La plage typique représente l'écart interquartile, qui est la plage de valeurs entre les 25e et 75e percentiles des données.</li>
              <li>La plage historique est la plage complète des valeurs moyennes journalières.</li>
              <li>Les plages typiques et historiques sont calculées en utilisant toutes les données <i><b>antérieures</i></b> à la dernière année sélectionnée, donnant un contexte historique.</li>
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
  
  
  # Define the ExtendedTask to generate the plot
  plot_output <- ExtendedTask$new(
    function(loc, param, lang, config) {

    promises::future_promise({
      
      con <- AquaConnect(name = config$dbName,
                         host = config$dbHost,
                         port = config$dbPort,
                         username = config$dbUser,
                         password = config$dbPass,
                         silent = TRUE)
      
      p <- plotTimeseries(location = loc,
                  sub_location = NULL,
                  parameter = param,
                  start_date = Sys.Date() - 30,
                  datum = FALSE,
                  filter = 20,
                  lang = lang,
                  line_scale = 1,
                  axis_scale = 1,
                  legend_scale = 1,
                  gridx = FALSE,
                  gridy = FALSE,
                  slider = FALSE,
                  title = TRUE,
                  custom_title = if (lang == "en") "Last 30 days" else "Derniers 30 jours",
                  con = NULL)
      DBI::dbDisconnect(con)
      return(p)  # have to explicitly tell it to return the plot, otherwise it returns the result of the last line (DBI::dbDisconnect(con))
    })
  })
  
  # Trigger the plot creation when the render changes
  observeEvent(params$render, {
    plot_output$invoke(params$loc_code, params$param_code, params$lang, config)
  })
  
  output$plot <- plotly::renderPlotly({
    plot_output$result()
  })
  
  
}
