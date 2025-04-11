#' The floodAtlas overlapping years app server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd


app_server <- function(input, output, session) {
  
  # Initial setup #############################################################
  
  # Connection to DB
  session$userData$con <- AquaConnect(name = config$dbName,
                                      host = config$dbHost,
                                      port = config$dbPort,
                                      username = config$dbUser,
                                      password = config$dbPass,
                                      silent = TRUE)
  

  session$onUnhandledError(function() {
    DBI::dbDisconnect(session$userData$con)
  })
  
  session$onSessionEnded(function() {
    DBI::dbDisconnect(session$userData$con)
  })
  
  # Remove irrelevant bookmarks
  setBookmarkExclude(c("go", "plot", "error", "info", ".clientValue-default-plotlyCrosstalkOpts", "plotly_afterplot-A", "plotly_hover-A", "plotly_relayout-A", "plotly_doubleclick-A"))
  
  # Automatically update URL every time an input changes
  observe({
    req(input$loc_code, input$param_code, input$yrs, input$lang)
    session$doBookmark()
  })
  
  output$visible_buttons <- renderUI({
    fluidRow(
      class = "button-row",
      # actionButton("info", label = if (input$lang == "en") "Plot info" else "Info sur le graphique", style = 'margin-bottom: 15px'),
      actionButton("info", label = "Info", style = 'margin-bottom: 15px', class = "btn btn-primary"),
      selectizeInput("yrs", label = NULL, choices = as.character(format(Sys.Date(), "%Y")), selected = as.character(format(Sys.Date(), "%Y")), multiple = TRUE, options = list(maxItems = 10)),
      bslib::input_task_button("go", label = if (input$lang == "en") "Re-draw" else "Re-dessiner", label_busy = if (input$lang == "en") "Processing..." else "SVP patienter...", style = 'margin-bottom: 15px', class = "btn btn-primary")
    )
  })
  
  # Update the query string
  onBookmarked(updateQueryString)

  # Parse URL query parameters on app load from URL and trigger plot creation
  params <- reactiveValues()
  observeEvent(session, {
    query <- parseQueryString(session$clientData$url_search)
    
    # Set input values based on URL parameters if they exist
    # yrs
    if (!is.null(query$yrs)) {
      sub_yrs <- gsub("[[:punct:]]", "", query$yrs)
      params$yrs <- as.numeric(sub_yrs)
    }
    
    # param_code
    if (!is.null(query$param_code)) {
      sub_param <- gsub("[[:punct:]]", "", query$param_code)
      params$param_code <- as.numeric(sub_param)
      updateNumericInput(session, "param_code", value = params$param_code)
    }
    
    # loc_code
    if (!is.null(query$loc_code)) {
      # remove punctuation
      sub_loc <- gsub("[[:punct:]]", "", query$loc_code)
      params$loc_code <- sub_loc
      updateTextInput(session, "loc_code", value = params$loc_code)
      
      # Find the years of record for the location
      tsid <- DBI::dbGetQuery(session$userData$con, paste0("SELECT timeseries_id FROM timeseries WHERE location = '", params$loc_code, "' AND parameter_id = ",  params$param_code, ";"))[1,1]
      yrs <- DBI::dbGetQuery(session$userData$con, paste0("SELECT DISTINCT EXTRACT(YEAR FROM date) AS year FROM measurements_calculated_daily_corrected WHERE timeseries_id = ", tsid, " ORDER BY year DESC;"))
      updateSelectizeInput(session, "yrs", choices = yrs$year, selected = params$yrs)
    }
    
    # language
    if (!is.null(query$lang)) {
      sub <-  gsub("[[:punct:]]", "", query$lang)
      params$lang <- sub
      updateTextInput(session, "lang", value = params$lang)
    }
    
    # Trigger the plot creation
    params$render <- TRUE
  }, once = TRUE)
  
  
  # Observers run after initial app load ######################################
  
  # Update the years if the user changes them
  observeEvent(input$yrs, {
    params$yrs <- input$yrs
  })
  
  # input$go is triggering a reactiveValues change, which triggers the plot to be re-rendered. This is done to allow for triggering the reactive upon loading from a URL as well as when clicking the go button.
  observeEvent(input$go, {
    if (params$render) {
      params$render <- FALSE
    } else {
    params$render <- TRUE
    }
  })
  
  # Show a modal with explanatory info when the user clicks the info button
  observeEvent(input$info, {
    if (params$lang == "en") {
      showModal(modalDialog(
        HTML("<div style='font-size: 14px;'>
                <ul>
                  <li>This plot allows you to plot up to 10 years of water level or flow data.</li>
                  <li>The 'Typical' range is the range between the 25th and 75th percentiles of daily mean values.</li>
                  <li>The 'Historic' range is the full range of daily mean values.</li>
                  <li>Typical and historic ranges are calculated using all data <i><b>prior</i></b> to the last year selected.</li>
                </ul>
              </div>"),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    } else if (params$lang == "fr") {
      showModal(modalDialog(
        HTML("<div style='font-size: 14px;'>
                 <ul>
                  <li>Ce grahique vous permettent de tracer jusqu'à 10 ans de données de niveau d'eau ou de débit.</li>
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
    function(loc, param, yrs, lang, config) {

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
      
      p <- plotOverlap(location = loc,
                  sub_location = NULL,
                  parameter = param,
                  startDay = 1,
                  endDay = 365,
                  years = yrs,
                  rate = "day",
                  datum = TRUE,
                  # filter = 20,
                  lang = lang,
                  line_scale = 1,
                  axis_scale = 1,
                  legend_scale = 1,
                  title = TRUE,
                  custom_title = loc_name,
                  gridx = FALSE,
                  gridy = FALSE,
                  slider = FALSE,
                  hover = FALSE,
                  tzone = "MST",
                  con = con)
      DBI::dbDisconnect(con)
      
      # Remove the plotly logo and other buttons from the top right of the plot
      p <- plotly::config(p, displayModeBar = FALSE)
      
      return(p)  # have to explicitly tell it to return the plot, otherwise it returns the result of the last line (DBI::dbDisconnect(con))
    })
  }) |> bslib::bind_task_button("go") # Changes the look of the task button and disables it while the task is running
  
  observeEvent(params$render, {
    plot_output$invoke(params$loc_code, params$param_code, params$yrs, params$lang, config)
  })
  
  output$plot <- plotly::renderPlotly({
    plot_output$result()
  })
  
}
