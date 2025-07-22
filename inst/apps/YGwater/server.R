#' The YGwater app server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
  
  # Initial setup #############################################################
  
  # Heartbeat every 5 seconds to keep app alive, prevent disconnects while doing queries and rendering plots. Note: time-consuming operations can still time out unless they use ExtendedTasks as the task otherwise blocks the server.
  output$keep_alive <- renderText({
    invalidateLater(5000, session)
    Sys.time()
  })
  
  # Allow reconnection to the same state the app was in if disconnected (e.g. computer put to sleep, etc.)
  session$allowReconnect(TRUE)
  
  
  # Hide all 'admin' side tabs if they were generated
  
  # Show relevant tabs for viz mode
  showViz <- function(show = TRUE) {
    if (show) {
      nav_show(id = "navbar", target = "home")
      nav_show(id = "navbar", target = "plot") # Actually a nav_menu, and this targets the tabs 'discrete', 'continuous', and 'mix' as well
      nav_show(id = "navbar", target = "maps") # Actually a nav_menu, and this targets the tabs 'mapParamValues' and 'mapMonitoringLocations' as well
      if (!config$public & config$g_drive) { # If not public AND g drive access is possible
        nav_show(id = "navbar", target = "FOD")
      }
      nav_show(id = "navbar", target = "reports") # Actually a nav_menu, and this targets the tabs 'snowInfo', 'waterInfo', 'WQReport', and 'snowBulletin' as well
      nav_show(id = "navbar", target = "images") # Actually a nav_menu, and this targets the tabs 'imgTableView' and 'imgMapView' as well
      nav_show(id = "navbar", target = "data") # Actually a nav_menu, and this targets the tabs 'discData' and 'contData' as well
      nav_show(id = "navbar", target = "info") # Actually a nav_menu, and this targets the tabs 'news' and 'about' as well
      nav_show(id = "navbar", target = "feedback")
    } else {
      nav_hide(id = "navbar", target = "home")
      nav_hide(id = "navbar", target = "plot") # Actually a nav_menu, and this targets the tabs 'discrete', 'continuous', and 'mix' as well
      nav_hide(id = "navbar", target = "maps") # Actually a nav_menu, and this targets the tabs 'mapParamValues' and 'mapMonitoringLocations' as well
      if (!config$public & config$g_drive) { # If not public AND g drive access is possible
        nav_hide(id = "navbar", target = "FOD")
      }
      nav_hide(id = "navbar", target = "reports") # Actually a nav_menu, and this targets the tabs 'snowInfo', 'waterInfo', 'WQReport', and 'snowBulletin' as well
      nav_hide(id = "navbar", target = "images") # Actually a nav_menu, and this targets the tabs 'imgTableView' and 'imgMapView' as well
      nav_hide(id = "navbar", target = "data") # Actually a nav_menu, and this targets the tabs 'discData' and 'contData' as well
      nav_hide(id = "navbar", target = "info") # Actually a nav_menu, and this targets the tabs 'news' and 'about' as well
      nav_hide(id = "navbar", target = "feedback")
    }
  }
  showAdmin <- function(show = TRUE, logout = FALSE) {
    if (show) {
      nav_show(id = "navbar", target = "dbAdmin") # Actually a nav_menu, and this targets the sync modules as well as timeeseries and locations add/edit modules
      nav_show(id = "navbar", target = "equip")
      nav_show(id = "navbar", target = "cal")
      nav_show(id = "navbar", target = "continuousData") # Actually a nav_menu
      nav_show(id = "navbar", target = "discreteData") # Actually a nav_menu
      nav_show(id = "navbar", target = "addFiles") # Actually a nav_menu, and this targets the tabs 'addDocs' and 'addImgs' as well
      nav_show(id = "navbar", target = "visit")
      nav_show(id = "navbar", target = "appTasks")
    } else {
      # Hide irrelevant tabs for viz mode
      nav_hide(id = "navbar", target = "dbAdmin")
      nav_hide(id = "navbar", target = "equip")
      nav_hide(id = "navbar", target = "cal")
      nav_hide(id = "navbar", target = "continuousData") # Actually a nav_menu
      nav_hide(id = "navbar", target = "discreteData") # Actually a nav_menu
      nav_hide(id = "navbar", target = "addFiles") # Actually a nav_menu, and this targets the tabs 'addDocs' and 'addImgs' as well
      nav_hide(id = "navbar", target = "visit")
      nav_hide(id = "navbar", target = "appTasks")
      if (logout) {
        shinyjs::hide("admin")
      }
    }
  }
  
  if (!config$public) {
    showAdmin(show = FALSE)
  }
  
  
  # Track window dimensions (used to modify plot appearance)
  windowDims <- reactive({
    req(input$window_dimensions)
    input$window_dimensions
  })
  
  # Initialize reactive flags to track whether each UI has been loaded
  reset_ui_loaded <- function() {
    ui_loaded$viz <- FALSE
    ui_loaded$admin <- FALSE
    ui_loaded$home <- FALSE
    ui_loaded$discretePlot <- FALSE
    ui_loaded$continuousPlot <- FALSE
    ui_loaded$mixPlot <- FALSE
    ui_loaded$mapParamValues <- FALSE
    ui_loaded$mapMonitoringLocations <- FALSE
    ui_loaded$FOD <- FALSE
    ui_loaded$imgTableView <- FALSE
    ui_loaded$imgMapView <- FALSE
    ui_loaded$snowInfo <- FALSE
    ui_loaded$waterInfo <- FALSE
    ui_loaded$WQReport <- FALSE
    ui_loaded$snowBulletin <- FALSE
    ui_loaded$discData <- FALSE
    ui_loaded$contData <- FALSE
    ui_loaded$news <- FALSE
    ui_loaded$about <- FALSE
    ui_loaded$feedback <- FALSE # !!! THIS TAB TO BE DELETED ONCE TESTING IS COMPLETE
    ui_loaded$addLocation <- FALSE
    ui_loaded$addSubLocation <- FALSE
    ui_loaded$equip <- FALSE
    ui_loaded$deploy_recover <- FALSE
    ui_loaded$cal <- FALSE
    
    ui_loaded$addContData <- FALSE
    ui_loaded$continuousCorrections <- FALSE
    ui_loaded$imputeMissing <- FALSE
    ui_loaded$editContData <- FALSE
    ui_loaded$grades_approvals_qualifiers <- FALSE
    ui_loaded$syncCont <- FALSE
    ui_loaded$addTimeseries <- FALSE
    
    ui_loaded$addDiscData <- FALSE
    ui_loaded$editDiscData <- FALSE
    ui_loaded$syncDisc <- FALSE
    
    ui_loaded$addDocs <- FALSE
    ui_loaded$addImgs <- FALSE
    
    ui_loaded$manageNewsContent <- FALSE
    ui_loaded$viewFeedback <- FALSE 
    ui_loaded$visit <- FALSE
  }
  
  ui_loaded <- reactiveValues()
  reset_ui_loaded() # Initialize the ui_loaded reactive values
  
  ## database connections ###########
  # Look for .mdb files in the AccessPath directories
  if (dir.exists(config$accessPath1) & !config$public) {
    # List the *.mdb files in the directory
    mdb_files1 <- list.files(config$accessPath1, pattern = "*.mdb", full.names = TRUE)
    if (length(mdb_files1) == 0) {
      mdb_files1 <- NULL
    }
  } else {
    mdb_files1 <- NULL
  }
  if (dir.exists(config$accessPath2) & !config$public) {
    # List the *.mdb files in the directory
    mdb_files2 <- list.files(config$accessPath2, pattern = "*.mdb", full.names = TRUE)
    if (length(mdb_files2) == 0) {
      mdb_files2 <- NULL
    }
  } else {
    mdb_files2 <- NULL
  }
  
  mdb_files <- c(mdb_files1, mdb_files2)
  
  if (is.null(mdb_files) & !config$public) {
    print("No .mdb files found in the accessPath1 directory.")
  }
  
  
  # Store the config info in the session. If the user connects with their own credentials these need to be used for plot rendering wrapped in an ExtendedTask or future/promises
  session$userData$config <- config
  
  # Initial database connections without edit privileges
  
  session$userData$AquaCache <- AquaConnect(name = config$dbName,
                                            host = config$dbHost,
                                            port = config$dbPort,
                                            username = config$dbUser,
                                            password = config$dbPass,
                                            silent = TRUE)
  
  print("Connected to AquaCache")
  
  # session$userData$use_webgl <- !grepl('Android', session$request$HTTP_USER_AGENT, ignore.case = TRUE) # This does not work with Shiny Server open source
  session$userData$use_webgl <- FALSE # Force webgl to FALSE for now, as it causes issues from Shiny Server
  
  session$onUnhandledError(function() {
    DBI::dbDisconnect(session$userData$AquaCache)
    print("Disconnected from AquaCache after unhandled error")
  })
  
  session$onSessionEnded(function() {
    DBI::dbDisconnect(session$userData$AquaCache)
    print("Disconnected from AquaCache after session end")
  })
  
  # Language selection ########################################################
  
  # Language selection reactives and observers based on the user's selected language (which is automatically set to the browser's language on load)
  languageSelection <- reactiveValues() # holds language and abbreviation
  
  # Populate the language selection dropdown
  session$sendCustomMessage("updateLangMenu", names(translation_cache))
  
  # Determine user's browser language. This should only run once when the app is loaded.
  observe({
    shinyjs::runjs("
      var language =  window.navigator.userLanguage || window.navigator.language;
      console.log('Detected browser language: ' + language);
      Shiny.setInputValue('userLang', language, {priority: 'event'});
                     ")
  })
  
  # Set initial language based on browser language
  # Check if userLang contains en or fr in the string and set the language accordingly
  observeEvent(input$userLang, { #userLang is the language of the user's browser. input$userLang is created by the runjs function above and not in the UI.
    lang_code <- substr(input$userLang, 1, 2)
    
    selected_lang <- if (lang_code == "fr") "Français" else "English"
    
    # Send the selected language to JavaScript so it updates input$langSelect
    session$sendCustomMessage(type = 'setSelectedLanguage', message = selected_lang)
    
    # Also update the HTML <head> for language settings
    session$sendCustomMessage(type = 'updateLang', message = list(lang = ifelse(lang_code == "fr", "fr", "en")))
  }, ignoreInit = TRUE, ignoreNULL = TRUE, once = TRUE) # This observeEvent should only run once when the app is loaded.
  
  # In contrast to input$userLang, input$langSelect is created in the UI and is the language selected by the user.
  # Observe user selection of language
  observeEvent(input$langSelect, { # Set the language based on the user's selection. This is done in an if statement in case the user types in something which isn't a language option.
    if (input$langSelect %in% names(translation_cache)) {
      languageSelection$language <- input$langSelect
      languageSelection$abbrev <- tr("titleCase", languageSelection$language)
      
      # Render the navigation bar titles based on the language
      output$homeNavTitle <- renderUI({tr("home", languageSelection$language)})
      output$mapsNavMenuTitle <- renderUI({tr("maps", languageSelection$language)})
      output$mapsNavParamsTitle <- renderUI({tr("maps_params", languageSelection$language)})
      output$mapsNavLocsTitle <- renderUI({tr("maps_locs", languageSelection$language)})
      
      output$plotsNavMenuTitle <- renderUI({tr("plots", languageSelection$language)})
      output$plotsNavDiscTitle <- renderUI({tr("plots_discrete", languageSelection$language)})
      output$plotsNavContTitle <- renderUI({tr("plots_continuous", languageSelection$language)})
      # output$plotsNavMixTitle <- renderUI({tr("plots_mix", languageSelection$language)})
      
      output$reportsNavMenuTitle <- renderUI({tr("reports", languageSelection$language)})
      output$reportsNavSnowstatsTitle <- renderUI({tr("reports_snow", languageSelection$language)})
      output$reportsNavWaterTitle <- renderUI({tr("reports_water", languageSelection$language)})
      output$reportsNavWQTitle <- renderUI({tr("reports_wq", languageSelection$language)})
      output$reportsNavSnowbullTitle <- renderUI({tr("reports_snowbull", languageSelection$language)})
      
      output$dataNavMenuTitle <- renderUI({tr("data", languageSelection$language)})
      output$dataNavDiscTitle <- renderUI({tr("data_discrete", languageSelection$language)})
      output$dataNavContTitle <- renderUI({tr("data_continuous", languageSelection$language)})
      
      output$imagesNavMenuTitle <- renderUI({tr("images", languageSelection$language)})
      output$imagesNavTableTitle <- renderUI({tr("images_table", languageSelection$language)})
      output$imagesNavMapTitle <- renderUI({tr("images_map", languageSelection$language)})
      
      output$infoNavMenuTitle <- renderUI({tr("info", languageSelection$language)})
      output$infoNavNewsTitle <- renderUI({tr("info_news", languageSelection$language)})
      output$infoNavAboutTitle <- renderUI({tr("info_about", languageSelection$language)})
      
      session$sendCustomMessage("updateTitle", tr("title", languageSelection$language)) # Update the browser title of the app based on the selected language
      
      # Render the footer based on the language
      output$footer_ui <- renderUI({
        div(
          span("Was this page helpful?",
               # Make 'buttons' that are bs_icons with a thumbs up and thumbs down and add a click event to them
               actionButton(
                 "thumbs_up",
                 label = bsicons::bs_icon("hand-thumbs-up", 
                                          size = "2em", 
                                          fill = "white"),
                 class = "btn btn-link",
                 width = "50px"),
               actionButton(
                 "thumbs_down",
                 label = bsicons::bs_icon("hand-thumbs-down", 
                                          size = "2em", 
                                          fill = "white"),
                 class = "btn btn-link",
                 width = "50px"),
               style = "color: white;"
          ),
          # Set background color of div
          style = "background-color: #244C5A; color: white; padding: 10px; text-align: left; margin-bottom: 5px;",
          # Make a placehold
          uiOutput("feedback_ui")
        )
      })
    }
  })
  
  # ObserveEvents for thumbs up/down buttons
  # add a textAreaInput to allow the user to write something, and a 'submit feedback' button
  feedback_open <- reactiveVal(FALSE)
  feedback_rendered <- reactiveVal(FALSE)
  observeEvent(input$thumbs_up, {
    if (feedback_open()) { # If TRUE, user is asking to close feedback
      # No need to check if already rendered at this point
      shinyjs::hide("feedback_ui")
      feedback_open(FALSE)
    } else {
      if (feedback_rendered()) { #if already rendered just show the element
        shinyjs::show("feedback_ui")
        feedback_open(TRUE)
      } else { # Render the element and set flags
        output$feedback_ui <- renderUI({
          div(
            textAreaInput("feedback_text", 
                          label = NULL, 
                          placeholder = "placeholder", 
                          rows = 3, 
                          width = "100%"),
            actionButton("submit_feedback", "Submit feedback")
          )
        })
        feedback_rendered(TRUE)
        feedback_open(TRUE)
      }
    }
  })
  observeEvent(input$thumbs_down, {
    if (feedback_open()) { # If TRUE, user is asking to close feedback
      # No need to check if already rendered at this point
      shinyjs::hide("feedback_ui")
      feedback_open(FALSE)
    } else {
      if (feedback_rendered()) { #if already rendered just show the element
        shinyjs::show("feedback_ui")
        feedback_open(TRUE)
      } else { # Render the element and set flags
        output$feedback_ui <- renderUI({
          div(
            textAreaInput("feedback_text", 
                          label = NULL, 
                          placeholder = "placeholder", 
                          rows = 3, 
                          width = "100%"),
            actionButton("submit_feedback", "Submit feedback")
          )
        })
        feedback_rendered(TRUE)
        feedback_open(TRUE)
      }
    }
  })
  
  # Log in/out for edits ##########################################
  log_attempts <- reactiveVal(0) # counter for login attempts
  session$userData$user_logged_in <- FALSE # value to track login status
  
  ## Log in #########
  # Login UI elements are not created if YGwater() is launched in public mode, in which case this code would not run
  observeEvent(input$loginBtn, {
    req(languageSelection$language) # Ensure language is set before proceeding
    if (log_attempts() > 5) {
      showModal(modalDialog(
        title = tr("login_fail", languageSelection$language),
        tr("login_fail_attempts", languageSelection$language),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    } else {
      showModal(modalDialog(
        tags$script(HTML('
$(document).keyup(function(event) {
  if ($("#password").is(":focus") && (event.keyCode == 13)) {
                         $("#confirmLogin").click();
    }
  });
  ')),
        title = "Login",
        renderUI(HTML(tr("login_txt", languageSelection$language), "<br> <br>")),
        textInput("username", tr("un", languageSelection$language)),
        passwordInput("password", tr("pwd", languageSelection$language)),
        footer = tagList(
          modalButton(tr("close", languageSelection$language)),
          actionButton("confirmLogin", tr("login_confirm", languageSelection$language), class = "btn-primary")
        )
      ))
    }
  })
  
  # Log in attempt if the button is clicked
  observeEvent(input$confirmLogin, {
    if (nchar(input$username) == 0 || nchar(input$password) == 0) {
      showModal(modalDialog(
        title = tr("login_fail", languageSelection$language),
        tr("login_fail_missing", languageSelection$language),
        easyClose = TRUE,
        footer = modalButton(tr("close", languageSelection$language))
      ))
      return()
    }
    log_attempts(log_attempts() + 1)
    
    tryCatch({
      session$userData$AquaCache_new <- AquaConnect(name = session$userData$config$dbName, 
                                                    host = session$userData$config$dbHost,
                                                    port = session$userData$config$dbPort,
                                                    username = input$username, 
                                                    password = input$password, 
                                                    silent = TRUE)
      test <- DBI::dbGetQuery(session$userData$AquaCache_new, "SELECT 1;")
      # Test the connection
      if (nrow(test) > 0) {
        removeModal()
        showModal(modalDialog(
          title = tr("login_success", languageSelection$language),
          paste0(tr("login_success_msg", languageSelection$language), " ", input$username),
          easyClose = TRUE,
          footer = modalButton(tr("close", languageSelection$language))
        ))
        # Drop the old connection
        DBI::dbDisconnect(session$userData$AquaCache)
        session$userData$AquaCache <- session$userData$AquaCache_new
        session$userData$AquaCache_new <- NULL
        
        # Update the session with the new user's credentials
        session$userData$config$dbUser <- input$username
        session$userData$config$dbPass <- input$password
        
        session$userData$user_logged_in <- TRUE
        
        shinyjs::hide("loginBtn")
        shinyjs::show("logoutBtn")
        
        # Create the new element for the 'admin' mode
        # Other tabs are created if/when the user clicks on the 'admin' tab
        nav_insert("navbar",
                   nav_item(tagList(actionButton("admin", "Switch to Admin mode", style = "color: #F2A900;"))),
                   target = "home", position = "before")
        
        # Initialize a fresh cache environment for the session
        session$userData$app_cache <- new.env(parent = emptyenv())
        # Reset all ui_loaded flags to FALSE so that they all reload data when the user clicks on them
        reset_ui_loaded()
        # Send the user back to the 'home' tab if they were elsewhere
        updateTabsetPanel(session, "navbar", selected = "home")
        
        # Select the last tab the user was on in viz mode. This will reload the module since the tab was previously set to 'home'.
        updateTabsetPanel(session, "navbar", selected = last_viz_tab())
        
        return()
      } else {
        removeModal()
        showModal(modalDialog(
          title = "Login Failed",
          "Invalid username or password or insufficient privileges.",
          easyClose = TRUE,
          footer = modalButton(tr("close", languageSelection$language))
        ))
        # attempt a disconnect of the new connection
        try({
          DBI::dbDisconnect(session$userData$AquaCache_new)
        })
        return()
      }
    }, error = function(e) {
      removeModal()
      showModal(modalDialog(
        title = "Login Failed",
        "Invalid username or password.",
        easyClose = TRUE,
        footer = modalButton(tr("close", languageSelection$language))
      ))
      # attempt a disconnect of the new connection
      try({
        DBI::dbDisconnect(session$userData$AquaCache_new)
      })
      return()
    })
  })
  
  ## Log out #####################################################
  observeEvent(input$logoutBtn, {
    
    session$userData$user_logged_in <- FALSE  # Set login status to FALSE
    
    # change the 'Logout' button back to 'Login'
    shinyjs::hide("logoutBtn")
    shinyjs::show("loginBtn")
    # Remove the 'admin' button upon logout
    removeUI(selector = "button:contains('Switch to ')")
    
    # Drop old connection
    DBI::dbDisconnect(session$userData$AquaCache)
    # Re-create the connection with the base 'config' parameters, no edit privileges
    session$userData$AquaCache <- AquaConnect(name = config$dbName, 
                                              host = config$dbHost,
                                              port = config$dbPort,
                                              username = config$dbUser,
                                              password = config$dbPass,
                                              silent = TRUE)
    
    # Reset the session userData with the default credentials
    session$userData$config$dbUser <- config$dbUser
    session$userData$config$dbPass <- config$dbPass
    
    showAdmin(show = FALSE, logout = TRUE) # Hide admin tabs and remove logout button
    
    # Clear the app_cache environment
    session$userData$app_cache <- new.env(parent = emptyenv())
    # Reset all ui_loaded flags to FALSE so that they all reload data when the user clicks on them
    reset_ui_loaded()
    # Send the user back to the 'home' tab if they were elsewhere
    updateTabsetPanel(session, "navbar", selected = "home")
    
    # Reset admin_vis_flag to 'viz', and trigger an observeEvent to switch to the 'viz' mode and on the last viz tab they were on. This will reload the module since the tab was previously set to 'home'.
    admin_vis_flag("viz")
    shinyjs::click("admin")
  })
  
  # Load modules based on input$navbar ################################
  # Store information to pass between modules
  moduleOutputs <- reactiveValues()
  
  # Initialize reactive values to store last tabs for each mode
  last_viz_tab <- reactiveVal("home")      # Default tab for viz mode
  last_admin_tab <- reactiveVal("manageNewsContent")      # Default tab for admin mode
  
  # Move between admin/visualize modes
  admin_vis_flag <- reactiveVal("admin")
  observeEvent(input$admin, {
    if (admin_vis_flag() == "viz") {
      # Set the flag before changing the tab programmatically
      
      updateActionButton(session, "admin", label = "Switch to Admin mode")
      
      # Show relevant tabs for viz mode
      showViz(show = TRUE)
      
      # Hide irrelevant tabs for viz mode
      showAdmin(show = FALSE)
      
      # Select the last tab the user was on in viz mode
      updateTabsetPanel(session, "navbar", selected = last_viz_tab())
      
      admin_vis_flag("admin")
      
    } else if (admin_vis_flag() == "admin") {
      
      updateActionButton(session, "admin", label = "Switch to Vizualize mode")
      
      # Show relevant tabs for admin mode
      showAdmin(show = TRUE)
      
      # Hide irrelevant tabs/menus
      showViz(show = FALSE)
      
      # Select the last tab the user was on in admin mode
      updateTabsetPanel(session, "navbar", selected = last_admin_tab())
      
      admin_vis_flag("viz")
    }
  })
  
  
  observeEvent(input$navbar, {
    req(languageSelection) # Ensure language is set before proceeding
    # Observe opening of the navbar (usually by a click through from another module) and close the navbar. Clicks by the user should have no effect as the navbar menu closes as soon as they click.
    session$sendCustomMessage(
      type = "toggleDropdown",
      message = list(msg = "hide dropdown"))
    
    # When user selects any a tab, update the last active tab for the current mode
    if (input$navbar %in% c("home", "discrete", "continuous", "mix", "map", "FOD", "snowInfo", "waterInfo", "WQReport", "snowBulletin", "imgTableView", "imgMapView", "about", "news", "discData", "contData", "feedback")) { # !!! the feedback tab is only for testing purposes and will be removed once the app is ready for production
      # User is in viz mode
      last_viz_tab(input$navbar)
    } else if (input$navbar %in% c("syncCont", "syncDisc", "addLocation", "addSubLocation", "addTimeseries", "equip", "cal", "addContData", "continuousCorrections", "imputeMissing", "editContData", "grades_approvals_qualifiers", "addDiscData", "editDiscData", "addDocs", "addImgs", "manageNewsContent", "viewFeedback", "visit")) {
      # User is in admin mode
      last_admin_tab(input$navbar)
    }
    
    # Load modules when the corresponding tabs are selected
    ## Visulize mode tabs ##########################
    ### Home nav_menu ##########################
    if (input$navbar == "home") {
      if (!ui_loaded$home) {
        output$home_ui <- renderUI(homeUI("home"))
        ui_loaded$home <- TRUE
        moduleOutputs$home <- home("home", language = languageSelection) # Call the server
      }
      observe({
        if (!is.null(moduleOutputs$home$change_tab)) {
          target <- moduleOutputs$home$change_tab
          nav_select(session = session, "navbar", selected = target)
          moduleOutputs$home$change_tab <- NULL
        }
      })
    }
    
    ### Plots nav_menu ##########################
    if (input$navbar == "discrete") {  # This is reached through a nav_menu
      if (!ui_loaded$discretePlot) {
        output$plotDiscrete_ui <- renderUI(discretePlotUI("discretePlot"))
        ui_loaded$discretePlot <- TRUE
        discretePlot("discretePlot", mdb_files, language = languageSelection, windowDims, inputs = moduleOutputs$mapLocs) # Call the server
        if (!is.null(moduleOutputs$mapLocs)) {
          moduleOutputs$mapLocs$location_id <- NULL
          moduleOutputs$mapLocs$change_tab <- NULL
        }
      }
    }
    if (input$navbar == "continuous") { # This is reached through a nav_menu
      if (!ui_loaded$continuousPlot) {
        output$plotContinuous_ui <- renderUI(continuousPlotUI("continuousPlot"))
        ui_loaded$continuousPlot <- TRUE
        continuousPlot("continuousPlot", language = languageSelection, windowDims, inputs = moduleOutputs$mapLocs) # Call the server
        if (!is.null(moduleOutputs$mapLocs)) {
          moduleOutputs$mapLocs$location_id <- NULL
          moduleOutputs$mapLocs$change_tab <- NULL
        }
      }
    }
    if (input$navbar == "mix") { # This is reached through a nav_menu
      if (!ui_loaded$mixPlot) {
        output$plotMix_ui <- renderUI(mixPlotUI("mixPlot"))
        ui_loaded$mixPlot <- TRUE
        mixPlot("mixPlot", mdb_files, language = languageSelection, windowDims) # Call the server
      }
    }
    
    ### Maps nav_menu ##########################
    if (input$navbar == "monitoringLocations") { # This is reached through a nav_menu
      if (!ui_loaded$mapMonitoringLocations) {
        output$mapLocs_ui <- renderUI(mapLocsUI("mapLocs"))
        ui_loaded$mapMonitoringLocations <- TRUE
        moduleOutputs$mapLocs <- mapLocs("mapLocs", language = languageSelection) # Call the server
      }
      observe({
        if (!is.null(moduleOutputs$mapLocs$change_tab)) {
          target <- moduleOutputs$mapLocs$change_tab
          if (target == "discData") ui_loaded$discData <- FALSE
          if (target == "contData") ui_loaded$contData <- FALSE
          if (target == "discrete") ui_loaded$discretePlot <- FALSE
          if (target == "continuous") ui_loaded$continuousPlot <- FALSE
          nav_select(session = session, "navbar", selected = target) # Change tabs
          moduleOutputs$mapLocs$change_tab <- NULL
        }
      })
    }
    if (input$navbar == "parameterValues") {
      if (!ui_loaded$mapParamValues) {
        output$mapParams_ui <- renderUI(mapParamsUI("mapParams"))
        ui_loaded$mapParamValues <- TRUE
        mapParams("mapParams", language = languageSelection) # Call the server
      }
    }
    
    ### FOD nav_menu ##########################
    if (input$navbar == "FOD") {
      if (!ui_loaded$FOD) {
        output$fod_ui <- renderUI(FODUI("FOD"))
        ui_loaded$FOD <- TRUE
        FOD("FOD") # Call the server
      }
    }
    ### Image nav_menu ##########################
    if (input$navbar == "imgTableView") {
      if (!ui_loaded$imgTableView) {
        output$imgTableView_ui <- renderUI(imgTableViewUI("imgTableView"))
        ui_loaded$imgTableView <- TRUE
        imgTableView("imgTableView", language = languageSelection) # Call the server
      }
    }
    if (input$navbar == "imgMapView") {
      if (!ui_loaded$imgMapView) {
        output$imgMapView_ui <- renderUI(imgMapViewUI("imgMapView"))
        ui_loaded$imgMapView <- TRUE
        imgMapView("imgMapView", language = languageSelection) # Call the server
      }
    }
    
    ### Reports nav_menu ##########################
    if (input$navbar == "snowInfo") {
      if (!ui_loaded$snowInfo) {
        output$snowInfo_ui <- renderUI(snowInfoUIMod("snowInfo"))
        ui_loaded$snowInfo <- TRUE
        snowInfoMod("snowInfo", language = languageSelection) # Call the server
      }
    }
    if (input$navbar == "waterInfo") {
      if (!ui_loaded$waterInfo) {
        output$waterInfo_ui <- renderUI(waterInfoUIMod("waterInfo"))
        ui_loaded$waterInfo <- TRUE
        waterInfoMod("waterInfo",language = languageSelection) # Call the server
      }
    }
    if (input$navbar == "WQReport") {
      if (!ui_loaded$WQReport) {
        output$WQReport_ui <- renderUI(WQReportUI("WQReport"))
        ui_loaded$WQReport <- TRUE
        WQReport("WQReport", mdb_files = mdb_files, language = languageSelection) # Call the server
      }
    }
    if (input$navbar == "snowBulletin") {
      if (!ui_loaded$snowBulletin) {
        output$snowBulletin_ui <- renderUI(snowBulletinUIMod("snowBulletin"))
        ui_loaded$snowBulletin <- TRUE
        snowBulletinMod("snowBulletin", language = languageSelection) # Call the server
      }
    }
    
    ### Data download nav_menu ##########################
    if (input$navbar == "discData") {
      if (!ui_loaded$discData) {
        output$discData_ui <- renderUI(discDataUI("discData"))
        ui_loaded$discData <- TRUE
        discData("discData", language = languageSelection, inputs = moduleOutputs$mapLocs) # Call the server
        if (!is.null(moduleOutputs$mapLocs)) {
          moduleOutputs$mapLocs$location_id <- NULL
          moduleOutputs$mapLocs$change_tab <- NULL
        }
      }
    }
    if (input$navbar == "contData") {
      if (!ui_loaded$contData) {
        output$contData_ui <- renderUI(contDataUI("contData"))
        ui_loaded$contData <- TRUE
        contData("contData", language = languageSelection, inputs = moduleOutputs$mapLocs) # Call the server
        if (!is.null(moduleOutputs$mapLocs)) {
          moduleOutputs$mapLocs$location_id <- NULL
          moduleOutputs$mapLocs$change_tab <- NULL
        }
      }
    }
    
    ### Info nav_menu ##########################
    if (input$navbar == "about") {
      if (!ui_loaded$about) {
        output$about_ui <- renderUI(aboutUI("about"))
        ui_loaded$about <- TRUE
        about("about", language = languageSelection) # Call the server
      }
    }
    if (input$navbar == "news") {
      if (!ui_loaded$news) {
        output$news_ui <- renderUI(newsUI("news"))
        ui_loaded$news <- TRUE
        news("news", language = languageSelection) # Call the server
      }
    }
    if (input$navbar == "feedback") { # !!! the feedback tab is only for testing purposes and will be removed once the app is ready for production
      if (!ui_loaded$feedback) {
        output$feedback_ui <- renderUI(feedbackUI("feedback"))
        ui_loaded$feedback <- TRUE
        feedback("feedback") # Call the server
      }
    }
    
    ## Admin mode tabs ##########################
    if (input$navbar == "syncCont") {
      if (!ui_loaded$syncCont) {
        output$syncCont_ui <- renderUI(syncContUI("syncCont"))
        ui_loaded$syncCont <- TRUE
        syncCont("syncCont") # Call the server
      }
    }
    if (input$navbar == "syncDisc") {
      if (!ui_loaded$syncDisc) {
        output$syncDisc_ui <- renderUI(syncDiscUI("syncDisc"))
        ui_loaded$syncDisc <- TRUE
        syncDisc("syncDisc") # Call the server
      }
    }
    if (input$navbar == "addLocation") {
      if (!ui_loaded$addLocation) {
        output$addLocation_ui <- renderUI(addLocationUI("addLocation"))
        ui_loaded$addLocation <- TRUE
        addLocation("addLocation", inputs = moduleOutputs$addDiscData) # Call the server
        if (!is.null(moduleOutputs$addDiscData)) {
          moduleOutputs$addDiscData$location <- NULL
          moduleOutputs$addDiscData$change_tab <- NULL
        }
      }
    }
    if (input$navbar == "addSubLocation") {
      if (!ui_loaded$addSubLocation) {
        output$addSubLocation_ui <- renderUI(addSubLocationUI("addSubLocation"))
        ui_loaded$addSubLocation <- TRUE
        addSubLocation("addSubLocation", inputs = moduleOutputs$addDiscData) # Call the server
        if (!is.null(moduleOutputs$addDiscData)) {
          moduleOutputs$addDiscData$sublocation <- NULL
          moduleOutputs$addDiscData$change_tab <- NULL
        }
      }
    }
    if (input$navbar == "addTimeseries") {
      if (!ui_loaded$addTimeseries) {
        output$addTimeseries_ui <- renderUI(addTimeseriesUI("addTimeseries"))
        ui_loaded$addTimeseries <- TRUE
        addTimeseries("addTimeseries") # Call the server
        if (!is.null(moduleOutputs$addContData)) {
          moduleOutputs$addContData$change_tab <- NULL
        }
      }
    }
    if (input$navbar == "deploy_recover") {
      if (!ui_loaded$deploy_recover) {
        output$deploy_recover_ui <- renderUI(deploy_recover_UI("deploy_recover"))  # Render the UI
        ui_loaded$deploy_recover <- TRUE
        deploy_recover("deploy_recover")  # Call the server
      }
    }
    if (input$navbar == "cal") {
      if (!ui_loaded$cal) {
        output$cal_ui <- renderUI(calUI("cal"))  # Render the UI
        ui_loaded$cal <- TRUE
        cal("cal")  # Call the server
      }
    }
    if (input$navbar == "addContData") {
      if (!ui_loaded$addContData) {
        output$addContData_ui <- renderUI(addContDataUI("addContData"))  # Render the UI
        ui_loaded$addContData <- TRUE
        moduleOutputs$addContData <- addContData("addContData")  # Call the server
      }
      # Observe the change_tab output from the addContData module
      observe({
        if (!is.null(moduleOutputs$addContData$change_tab)) {
          nav_select(session = session, "navbar", selected = moduleOutputs$addContData$change_tab)
          moduleOutputs$addContData$change_tab <- NULL
        }
      })
    }
    if (input$navbar == "continuousCorrections") {
      if (!ui_loaded$continuousCorrections) {
        output$continuousCorrections_ui <- renderUI(continuousCorrectionsUI("continuousCorrections"))
        ui_loaded$continuousCorrections <- TRUE
        continuousCorrections("continuousCorrections")
      }
    }
    if (input$navbar == "imputeMissing") {
      if (!ui_loaded$imputeMissing) {
        output$imputeMissing_ui <- renderUI(imputeMissingUI("imputeMissing"))  # Render the UI
        ui_loaded$imputeMissing <- TRUE
        imputeMissing("imputeMissing")  # Call the server
      }
    }
    if (input$navbar == "editContData") {
      if (!ui_loaded$editContData) {
        output$editContData_ui <- renderUI(editContDataUI("editContData"))  # Render the UI
        ui_loaded$editContData <- TRUE
        editContData("editContData")  # Call the server
      }
    }
    if (input$navbar == "grades_approvals_qualifiers") {
      if (!ui_loaded$grades_approvals_qualifiers) {
        output$grades_approvals_qualifiers_ui <- renderUI(grades_approvals_qualifiersUI("grades_approvals_qualifiers"))  # Render the UI
        ui_loaded$grades_approvals_qualifiers <- TRUE
        grades_approvals_qualifiers("grades_approvals_qualifiers")  # Call the server
      }
    }
    if (input$navbar == "addDiscData") {
      if (!ui_loaded$addDiscData) {
        output$addDiscData_ui <- renderUI(addDiscDataUI("addDiscData"))  # Render the UI
        ui_loaded$addDiscData <- TRUE
        moduleOutputs$addDiscData <- addDiscData("addDiscData")  # Call the server
      }
      # Observe the change_tab output from the addDiscData module
      observe({
        if (!is.null(moduleOutputs$addDiscData$change_tab)) {
          nav_select(session = session, "navbar", selected = moduleOutputs$addDiscData$change_tab)
          moduleOutputs$addDiscData$change_tab <- NULL
        }
      })
    }
    if (input$navbar == "editDiscData") {
      if (!ui_loaded$editDiscData) {
        output$editDiscData_ui <- renderUI(editDiscDataUI("editDiscData"))  # Render the UI
        ui_loaded$editDiscData <- TRUE
        editDiscData("editDiscData")  # Call the server
      }
    }
    if (input$navbar == "addDocs") {
      if (!ui_loaded$addDocs) {
        output$addDocs_ui <- renderUI(addDocsUI("addDocs"))  # Render the UI
        ui_loaded$addDocs <- TRUE
        addDocs("addDocs")  # Call the server
      }
    }
    if (input$navbar == "addImgs") {
      if (!ui_loaded$addImgs) {
        output$addImgs_ui <- renderUI(addImgsUI("addImgs"))  # Render the UI
        ui_loaded$addImgs <- TRUE
        addImgs("addImgs")  # Call the server
      }
    }
    if (input$navbar == "manageNewsContent") {
      if (!ui_loaded$manageNewsContent) {
        output$manageNewsContent_ui <- renderUI(manageNewsContentUI("manageNewsContent"))
        ui_loaded$manageNewsContent <- TRUE
        manageNewsContent("manageNewsContent")
      }
    }
    if (input$navbar == "viewFeedback") {
      if (!ui_loaded$viewFeedback) {
        output$viewFeedback_ui <- renderUI(viewFeedbackUI("viewFeedback"))
        ui_loaded$viewFeedback <- TRUE
        viewFeedback("viewFeedback")
      }
    }
    if (input$navbar == "visit") {
      if (!ui_loaded$visit) {
        output$visit_ui <- renderUI(visitUI("visit"))  # Render the UI
        ui_loaded$visit <- TRUE
        visit("visit")  # Call the server
      }
    }
  }) # End of observeEvent for loading modules based on navbar
  
} # End of main server
