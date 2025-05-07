#' The YGwater app server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
  
  
  # Initial setup #############################################################
  
  # Hide all 'admin' side tabs if they were generated
  
  # Show relevant tabs for viz mode
  showViz <- function(show = TRUE) {
    if (show) {
      nav_show(id = "navbar", target = "home")
      nav_show(id = "navbar", target = "plot") # Actually a nav_menu, and this targets the tabs 'discrete', 'continuous', and 'mix' as well
      nav_show(id = "navbar", target = "map")
      if (!config$public & config$g_drive) { # If not public AND g drive access is possible
        nav_show(id = "navbar", target = "FOD")
      }
      nav_show(id = "navbar", target = "reports") # Actually a nav_menu, and this targets the tabs 'snowInfo', 'waterInfo', 'WQReport', and 'snowBulletin' as well
      nav_show(id = "navbar", target = "imgTableView")
      nav_show(id = "navbar", target = "imgMapView")
      nav_show(id = "navbar", target = "data") # Actually a nav_menu, and this targets the tabs 'discData' and 'contData' as well
      nav_show(id = "navbar", target = "info") # Actually a nav_menu, and this targets the tabs 'news' and 'about' as well
    } else {
      nav_hide(id = "navbar", target = "home")
      nav_hide(id = "navbar", target = "plot") # Actually a nav_menu, and this targets the tabs 'discrete', 'continuous', and 'mix' as well
      nav_hide(id = "navbar", target = "map")
      if (!config$public & config$g_drive) { # If not public AND g drive access is possible
        nav_hide(id = "navbar", target = "FOD")
      }
      nav_hide(id = "navbar", target = "reports") # Actually a nav_menu, and this targets the tabs 'snowInfo', 'waterInfo', 'WQReport', and 'snowBulletin' as well
      nav_hide(id = "navbar", target = "imgTableView")
      nav_hide(id = "navbar", target = "imgMapView")
      nav_hide(id = "navbar", target = "data") # Actually a nav_menu, and this targets the tabs 'discData' and 'contData' as well
      nav_hide(id = "navbar", target = "info") # Actually a nav_menu, and this targets the tabs 'news' and 'about' as well
    }
  }
  showAdmin <- function(show = TRUE, logout = FALSE) {
    if (show) {
      nav_show(id = "navbar", target = "locs")
      nav_show(id = "navbar", target = "ts")
      nav_show(id = "navbar", target = "equip")
      nav_show(id = "navbar", target = "cal")
      nav_show(id = "navbar", target = "addData") # Actually a nav_menu, and this targets the tabs 'addContData' and 'addDiscData' as well
      nav_show(id = "navbar", target = "addFiles") # Actually a nav_menu, and this targets the tabs 'addDocs' and 'addImgs' as well
      nav_show(id = "navbar", target = "visit")
    } else {
      # Hide irrelevant tabs for viz mode
      nav_hide(id = "navbar", target = "locs")
      nav_hide(id = "navbar", target = "ts")
      nav_hide(id = "navbar", target = "equip")
      nav_hide(id = "navbar", target = "cal")
      nav_hide(id = "navbar", target = "addData") # Actually a nav_menu, and this targets the tabs 'addContData' and 'addDiscData' as well
      nav_hide(id = "navbar", target = "addFiles") # Actually a nav_menu, and this targets the tabs 'addDocs' and 'addImgs' as well
      nav_hide(id = "navbar", target = "visit")
      if (logout) {
        shinyjs::hide("admin")
      }
    }
  }
  
  if (!config$public) {
    showAdmin(show = FALSE)
  }
  
  
  # Automatically update URL every time an input changes
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  setBookmarkExclude(c("userLang", "loginBtn", "logoutBtn", "window_dimensions"))
  
  # Update the query string
  onBookmarked(updateQueryString)
  
  isRestoring <- reactiveVal(FALSE)
  isRestoring_img <- reactiveVal(FALSE)
  
  onRestore(function(state) {
    isRestoring(TRUE)
    isRestoring_img(TRUE)
  })
  
  # Track window dimensions (used to modify plot appearance)
  windowDims <- reactive({
    req(input$window_dimensions)
    input$window_dimensions
  })

  # Initialize reactive flags to track whether each UI has been loaded
  ui_loaded <- reactiveValues(
    viz = FALSE,
    admin = FALSE,
    home = FALSE,
    discretePlot = FALSE,
    continuousPlot = FALSE,
    mixPlot = FALSE,
    map = FALSE,
    FOD = FALSE,
    imgTableView = FALSE,
    imgMapView = FALSE,
    snowInfo = FALSE,
    waterInfo = FALSE,
    WQReport = FALSE,
    snowBulletin = FALSE,
    discData = FALSE,
    contData = FALSE,
    news = FALSE,
    about = FALSE,
    locs = FALSE,
    ts = FALSE,
    equip = FALSE,
    deploy_recover = FALSE,
    cal = FALSE,
    addContData = FALSE,
    addDiscData = FALSE,
    addDocs = FALSE,
    addImgs = FALSE,
    visit = FALSE)
  
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
    if (!isRestoring()) {
      shinyjs::runjs("
      var language =  window.navigator.userLanguage || window.navigator.language;
      console.log('Detected browser language: ' + language);
      Shiny.setInputValue('userLang', language, {priority: 'event'});
                     ")
    }
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
      output$mapNavMenuTitle <- renderUI({tr("map", languageSelection$language)})
      
      output$plotsNavMenuTitle <- renderUI({tr("plots", languageSelection$language)})
      output$plotsNavDiscTitle <- renderUI({tr("plots_discrete", languageSelection$language)})
      output$plotsNavContTitle <- renderUI({tr("plots_continuous", languageSelection$language)})
      output$plotsNavMixTitle <- renderUI({tr("plots_mix", languageSelection$language)})
      
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
    }
  })
  
  # Log in/out for edits ##########################################
  log_attempts <- reactiveVal(0) # counter for login attempts
  session$userData$user_logged_in <- FALSE # value to track login status
  
  ## Log in #########
  # Login UI elements are not created if YGwater() is launched in public mode, in which case this code would not run
  observeEvent(input$loginBtn, {
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
        
        # Check if the user has admin privileges. Inspect the 'timeseries' table to see if they have write privileges (checks are also performed in each writing/editing module).
        result <- DBI::dbGetQuery(session$userData$AquaCache, paste0("SELECT has_table_privilege('timeseries', 'UPDATE') AS can_write;"))
        if (result$can_write) {
          session$userData$config$admin <- TRUE
          
        } else {
          session$userData$config$admin <- FALSE
        }
        # Create the new element for the 'admin' mode
        nav_insert("navbar",
                   nav_item(tagList(actionButton("admin", "Switch to Admin mode", style = "color: #F2A900;"))),
                   target = "home", position = "before")
        
        # Other tabs are created if/when the user clicks on the 'admin' tab
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
    
    # Redirect to last 'viz' tab
    updateTabsetPanel(session, "navbar", selected = last_viz_tab())
    
    showAdmin(show = FALSE, logout = TRUE) # Hide admin tabs and remove logout button
    
    
    # Reset admin_vis_flag to 'viz', and trigger an observeEvent to switch to the 'viz' mode
    admin_vis_flag("viz")
    shinyjs::click("admin")
  })
  
  # Load modules based on input$navbar ################################
  # Store information to pass between modules
  primary_outputs <- reactiveValues()
  
  # Initialize reactive values to store last tabs for each mode
  last_viz_tab <- reactiveVal("home")      # Default tab for viz mode
  last_admin_tab <- reactiveVal("locs")      # Default tab for admin mode
  
  
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
      
      shinyjs::runjs("
  document.querySelectorAll('#navbar a[data-value=\"equip\"] b.caret')
    .forEach(el => el.remove());
")
    }
  })
  
  
  observeEvent(input$navbar, {
    # When user selects any a tab, update the last active tab for the current mode
    if (input$navbar %in% c("home", "discrete", "continuous", "mix", "map", "FOD", "snowInfo", "waterInfo", "WQReport", "snowBulletin", "imgTableView", "imgMapView", "about", "news", "discData", "contData")) {
      # User is in viz mode
      last_viz_tab(input$navbar)
    } else if (input$navbar %in% c("locs", "ts", "equip", "cal", "addContData", "addDiscData", "addDocs", "addImgs", "visit")) {
      # User is in admin mode
      last_admin_tab(input$navbar) 
    }
    
    # Load modules when the corresponding tabs are selected
    if (input$navbar == "home") {
      if (!ui_loaded$home) {
        output$home_ui <- renderUI(homeUI("home"))
        ui_loaded$home <- TRUE
        home("home", language = languageSelection) # Call the server
      }
    }
    if (input$navbar == "discrete") {
      if (!ui_loaded$discretePlot) {
        output$discrete_ui <- renderUI(discretePlotUI("discretePlot"))
        ui_loaded$discretePlot <- TRUE
        discretePlot("discretePlot", mdb_files, language = languageSelection, windowDims) # Call the server
      }
    }
    if (input$navbar == "continuous") {
      if (!ui_loaded$continuousPlot) {
        output$continuous_ui <- renderUI(continuousPlotUI("continuousPlot"))
        ui_loaded$continuousPlot <- TRUE
        continuousPlot("continuousPlot", language = languageSelection, windowDims) # Call the server
      }
    }
    if (input$navbar == "mix") {
      if (!ui_loaded$mixPlot) {
        output$mix_ui <- renderUI(mixPlotUI("mixPlot"))
        ui_loaded$mixPlot <- TRUE
        mixPlot("mixPlot", mdb_files, language = languageSelection, windowDims) # Call the server
      }
    }
    if (input$navbar == "map") {
      if (!ui_loaded$map) {
        output$map_ui <- renderUI(mapUI("map"))
        ui_loaded$map <- TRUE
        primary_outputs$map_main <- map("map", language = languageSelection) # Call the server
      }
      observe({  # Observe the map_outputs reactive to see if the tab should be changed, for example when the user clicks on a location's pop-up links to go to data or plot tabs.
        if (!is.null(primary_outputs$map_main$change_tab)) {
          updateTabsetPanel(session, "navbar", selected = (primary_outputs$map_main$change_tab))
          primary_outputs$map_main$change_tab <- NULL
        }
      })
    }
    if (input$navbar == "FOD") {
      if (!ui_loaded$FOD) {
        output$fod_ui <- renderUI(FODUI("FOD"))
        ui_loaded$FOD <- TRUE
        FOD("FOD") # Call the server
      }
    }
    if (input$navbar == "imgTableView") {
      if (!ui_loaded$imgTableView) {
        output$imgTableView_ui <- renderUI(imgTableViewUI("imgTableView"))
        ui_loaded$imgTableView <- TRUE
        imgTableView("imgTableView", language = languageSelection, restoring = isRestoring_img) # Call the server
      }
    }
    if (input$navbar == "imgMapView") {
      if (!ui_loaded$imgMapView) {
        output$imgMapView_ui <- renderUI(imgMapViewUI("imgMapView"))
        ui_loaded$imgMapView <- TRUE
        imgMapView("imgMapView", language = languageSelection) # Call the server
      }
    }
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
    if (input$navbar == "discData") {
      if (!ui_loaded$discData) {
        output$discData_ui <- renderUI(discDataUI("discData"))
        ui_loaded$discData <- TRUE
        discData("discData", language = languageSelection) # Call the server
      }
    }
    if (input$navbar == "contData") {
      if (!ui_loaded$contData) {
        output$contData_ui <- renderUI(contDataUI("contData"))
        ui_loaded$contData <- TRUE
        contData("contData", language = languageSelection) # Call the server
      }
    }
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
    if (input$navbar == "locs") {
      if (!ui_loaded$locs) {
        output$locs_ui <- renderUI(locsUI("locs"))
        ui_loaded$locs <- TRUE
        locs("locs") # Call the server
      }
    }
    if (input$navbar == "ts") {
      if (!ui_loaded$ts) {
        output$ts_ui <- renderUI(tsUI("ts"))
        ui_loaded$ts <- TRUE
        ts("ts") # Call the server
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
        addContData("addContData")  # Call the server
      }
    }
    if (input$navbar == "addDiscData") {
      if (!ui_loaded$addDiscData) {
        output$addDiscData_ui <- renderUI(addDiscDataUI("addDiscData"))  # Render the UI
        ui_loaded$addDiscData <- TRUE
        addDiscData("addDiscData")  # Call the server
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
    if (input$navbar == "visit") {
      if (!ui_loaded$visit) {
        output$visit_ui <- renderUI(visitUI("visit"))  # Render the UI
        ui_loaded$visit <- TRUE
        visit("visit")  # Call the server
      }
    }
  }) # End of observeEvent for loading modules based on navbar
  
} # End of main server
