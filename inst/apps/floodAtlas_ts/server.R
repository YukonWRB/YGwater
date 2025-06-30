#' The floodAtlas timeseries app server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {

  # Initial setup #############################################################

  output$keep_alive <- renderText({
    invalidateLater(5000, session)
    Sys.time()
  })
  
  # Remove irrelevant bookmarks
  setBookmarkExclude(c("plot", "error", "info", ".clientValue-default-plotlyCrosstalkOpts", "plotly_afterplot-A", "plotly_hover-A", "plotly_relayout-A", "plotly_doubleclick-A"))
  
  # Automatically update URL every time an input changes
  observe({
    req(input$loc_code, input$param_code, input$lang)
    session$doBookmark()
  })
  
  output$visible_buttons <- renderUI({
    fluidRow(
      class = "button-row",
      actionButton("info", label = "Info", style = 'margin-bottom: 15px', class = "btn btn-primary"),
    )
  })
  
  # Update the query string
  onBookmarked(updateQueryString)

  # Parse URL query parameters on app load from URL and trigger plot creation
  params <- reactiveValues()
  session$userData$use_webgl <- !grepl('Android', session$request$HTTP_USER_AGENT, ignore.case = TRUE)
  observeEvent(session, {
    query <- parseQueryString(session$clientData$url_search)
    
    # loc_code
    if (!is.null(query$loc_code)) {
      # remove punctuation
      sub_loc <- gsub("[[:punct:]]", "", query$loc_code)
      params$loc_code <- sub_loc
      updateTextInput(session, "loc_code", value = params$loc_code)
    }
    
    # param_code
    if (!is.null(query$param_code)) {
      sub_param <- gsub("[[:punct:]]", "", query$param_code)
      params$param_code <- as.numeric(sub_param)
      updateNumericInput(session, "param_code", value = params$param_code)
    }
    
    # lang
    if (!is.null(query$lang)) {
      sub_lang <- gsub("[[:punct:]]", "", query$lang)
      params$lang <- sub_lang
      updateTextInput(session, "lang", value = params$lang)
    }
    
    # Then trigger speed check
    session$sendCustomMessage("checkSpeed", list())
  }, once = TRUE)
  
  
  # Observers run after initial app load ######################################
  
  # Show a modal with explanatory info when the user clicks the info button
  observeEvent(input$info, {
    if (params$lang == "en") {
      showModal(modalDialog(
        HTML("<div style='font-size: 14px;'>
                <ul>
                  <li>This plots shows water level or flow for the past 30 days; resolution depends on your internet speed.</li>
                  <li>The 'Typical' range is the range between the 25th and 75th percentiles of daily mean values.</li>
                  <li>The 'Historic' range is the full range of daily mean values.</li>
                  <li>Typical and historic ranges are calculated using all data <i><b>prior</i> </b>to the year displayed.</li>
                </ul>
              </div>"),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    } else if (params$lang == "fr") {
      showModal(modalDialog(
        HTML("<div style='font-size: 14px;'>
                <ul>
                  <li>Ce graphique démontre le niveau ou débit d'eau durant les 30 derniers jours; la résolution dépends de la vitese de votre internet.</li>
                  <li>La plage typique représente la plage de valeurs entre les 25e et 75e percentiles des données.</li>
                  <li>La plage historique est la plage complète des valeurs moyennes journalières.</li>
                  <li>Les plages typiques et historiques sont calculées en utilisant toutes les données <i><b>antérieures</i></b> à la dernière année sélectionnée.</li>
                </ul>
              </div>"),
        easyClose = TRUE,
        footer = modalButton("Fermer")
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
    function(loc, param, lang, webgl, config, rate) {

    promises::future_promise({
      
      con <- AquaConnect(name = config$dbName,
                         host = config$dbHost,
                         port = config$dbPort,
                         username = config$dbUser,
                         password = config$dbPass,
                         silent = TRUE)
      
      loc_name <- DBI::dbGetQuery(con, paste0("SELECT ", if (lang == "en") "name" else "name_fr", " FROM locations WHERE location = '", loc, "';"))[1,1]
      if (nchar(loc_name) > 30) {
        loc_name <- paste0(substr(loc_name, 1, 25), "...")
      }
      
      
      p <- plotTimeseries(location = loc,
                  sub_location = NULL,
                  parameter = param,
                  start_date = Sys.Date() - 30,
                  datum = TRUE,
                  # filter = 20,
                  rate = rate,
                  lang = lang,
                  line_scale = 1,
                  axis_scale = 1,
                  legend_scale = 1,
                  gridx = FALSE,
                  gridy = FALSE,
                  slider = FALSE,
                  webgl = webgl,
                  title = TRUE,
                  hover = FALSE,
                  tzone = "MST",
                  custom_title = paste0(if (lang == "en") "Last 30 days: " else "30 derniers jours: ", loc_name),
                  con = con)
      
      # Remove the plotly logo and other buttons from the top right of the plot
      p <- plotly::config(p, displayModeBar = FALSE)
      
      DBI::dbDisconnect(con)
      return(p)  # have to explicitly tell it to return the plot, otherwise it returns the result of the last line (which used to be DBI::dbDisconnect(con))
    })
  })
  
  # Trigger the plot creation when the render changes
  observeEvent(input$user_speed, {
    req(params$loc_code, params$param_code, params$lang)
    rate <- if (input$user_speed < 0.0003) "day" else if (input$user_speed < 0.002) "hour" else "max"
    plot_output$invoke(params$loc_code, params$param_code, params$lang, FALSE, config, rate)
  })
  
  output$plot <- plotly::renderPlotly({
    plot_output$result()
  })
  
  
}
