#' The YGwater app server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
  
  
  # Initial setup #############################################################
  
  # Automatically update URL every time an input changes
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  setBookmarkExclude(c("userLang", "loginBtn", "logoutBtn"))
  
  # All other bookmark exclusions need to go here because exclusions don't seem to run until the server of each module is called. Since the UI is loaded right away, this results in the bookmarking of the excluded inputs before the exclusions are set.
  # A possible work-around that would also decrease load times is to load the UI of each module only when the tab is selected via the server.
  
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
    img = FALSE,
    gen = FALSE,
    news = FALSE,
    about = FALSE,
    locs = FALSE,
    ts = FALSE,
    equip = FALSE,
    cal = FALSE,
    contData = FALSE,
    discData = FALSE,
    addDocs = FALSE,
    addImgs = FALSE,
    visit = FALSE)
  
  # reactive to see if 'admin' side tabs have been created already
  tab_created <- reactiveValues(
    locs = FALSE,
    ts = FALSE,
    data = FALSE,
    files = FALSE,
    equip = FALSE,
    cal = FALSE,
    contData = FALSE,
    discData = FALSE,
    addDocs = FALSE,
    addImgs = FALSE,
    visit = FALSE
  )
  
  ## database connections ###########
  # Look for .mdb files in the AccessPath directory
  if (dir.exists(config$accessPath) & !config$public) {
    # List the *.mdb files in the directory
    mdb_files <- list.files(config$accessPath, pattern = "*.mdb", full.names = TRUE)
    if (length(mdb_files) == 0) {
      mdb_files <- NULL
    }
  } else {
    mdb_files <- NULL
  }
  
  if (is.null(mdb_files) & !config$public) {
    print("No .mdb files found in the AccessPath directory.")
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
      session$sendCustomMessage("updateTitle", tr("title", languageSelection$language)) # Update the title of the app based on the selected language
    }
  })
  
  # Log in/out for edits ##########################################
  log_attempts <- reactiveVal(0) # counter for login attempts
  user_logged_in <- reactiveVal(FALSE) # Reactive value to track login status
  
  ## Log in #########
  # Login UI elements are not created if YGwater() is launched in public mode, in which case this code would not run
  observeEvent(input$loginBtn, {
    if (log_attempts() > 5) {
      showModal(modalDialog(
        title = "Login Failed",
        "You've exceeded the maximum number of login attempts.",
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
        renderUI(HTML("Log in to add/modify data and administer assets.", "<br> <br>")),
        textInput("username", "Username"),
        passwordInput("password", "Password"),
        footer = tagList(
          modalButton("Close"),
          actionButton("confirmLogin", "Log In", class = "btn-primary")
        )
      ))
    }
  })
  
  # Log in attempt if the button is clicked
  observeEvent(input$confirmLogin, {
    log_attempts(log_attempts() + 1)
    
    if (nchar(input$username) == 0 || nchar(input$password) == 0) {
      showModal(modalDialog(
        title = "Login Failed",
        "Please enter both a username and password.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }
    tryCatch({
      # Drop old connection
      # NOTE! Double assignment is used when (re)creating the connection to get out of the observer's scope into the environment.
      session$userData$AquaCache_new <- AquaConnect(name = session$userData$config$dbName, 
                                host = session$userData$config$dbHost,
                                port = session$userData$config$dbPort,
                                username = input$username, 
                                password = input$password, 
                                silent = TRUE)
      test <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT 1;")
      # Test the connection
      if (nrow(test) > 0) {
        showModal(modalDialog(
          title = "Login Successful",
          "You are now logged in.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        
        # Drop the old connection
        DBI::dbDisconnect(session$userData$AquaCache)
        session$userData$AquaCache <- session$userData$AquaCache_new
        session$userData$AquaCache_new <- NULL
        
        # Update the session with the new user's credentials
        session$userData$config$dbUser <- input$username
        session$userData$config$dbPass <- input$password
        
        user_logged_in(TRUE)
        shinyjs::hide("loginBtn")
        shinyjs::show("logoutBtn")
        
        # Check if the user has admin privileges. Inspect the 'timeseries' table to see if they have write privileges.
        result <- DBI::dbGetQuery(con, paste0("SELECT has_table_privilege('timeseries', 'UPDATE') AS can_write;"))
        if (result$can_write) {
          session$userData$config$admin <- TRUE
          # Create the new tabs for the 'admin' mode
          insertTab("navbar",
                    tabPanel(title = "Switch to Admin mode", value = "admin",
                             uiOutput("admin_ui")),
                    target = "home", position = "before")
          # Other tabs are created if/when the user clicks on the 'admin' tab
        } else {
          session$userData$config$admin <- FALSE
        }
        # Return to exit this observeEvent
        return()
      } else {
        showModal(modalDialog(
          title = "Login Failed",
          "Invalid username or password.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        # attempt a disconnect of the new connection
        try({
          DBI::dbDisconnect(session$userData$AquaCache_new)
        })
        return()
      }
    }, error = function(e) {
      showModal(modalDialog(
        title = "Login Failed",
        "Invalid username or password.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      try({
        DBI::dbDisconnect(session$userData$AquaCache_new)
        session$userData$AquaCache_new <- NULL
      })
      return()
    })
  })
  
  ## Log out #####################################################
  observeEvent(input$logoutBtn, {
    
    user_logged_in(FALSE)  # Set login status to FALSE
    # Hide the 'admin' tabs upon logout
    hideTab(inputId = "navbar", target = "admin")
    showTab(inputId = "navbar", target = "viz", select = TRUE)
    
    # change the 'Logout' button back to 'Login'
    shinyjs::hide("logoutBtn")
    shinyjs::show("loginBtn")
    
    # Drop old connection
    DBI::dbDisconnect(session$userData$AquaCache)
    # Re-create the connection with the base 'config' parameters, no edit privileges
    session$userData$AquaCache <- AquaConnect(name = config$dbName, 
                              host = config$dbHost,
                              port = config$dbPort,
                              username = config$dbUser,
                              password = config$dbPass,
                              silent = TRUE)
    # Redirect to 'viz' tab
    updateTabsetPanel(session, "navbar", selected = "viz")
    # Remove admin-related tabs on logout
    removeTab("navbar", "admin", session = session)
    removeTab("navbar", "locs", session = session)
    removeTab("navbar", "ts", session = session)
    removeTab("navbar", "equip", session = session)
    removeTab("navbar", "cal", session = session)
    removeTab("navbar", "data", session = session)
    removeTab("navbar", "files", session = session)
    removeTab("navbar", "visit", session = session)
  })
  
  # Load modules based on input$navbar ################################
  # Store information to pass between modules
  primary_outputs <- reactiveValues()
  
  # Initialize a flag to track programmatic tab changes
  programmatic_change <- reactiveVal(FALSE)
  
  # Initialize reactive values to store last tabs for each mode
  last_viz_tab <- reactiveVal("home")      # Default tab for viz mode
  last_admin_tab <- reactiveVal("locs")      # Default tab for admin mode
  initial_tab <- reactiveVal(NULL)
    
  # Move between tabs/modules
  observeEvent(input$navbar, {
    if (programmatic_change()) {
      # Reset the flag and exit to prevent looping
      if (!is.null(initial_tab())) {
        programmatic_change(FALSE)
        return()
      } else {
        initial_tab(FALSE)
        programmatic_change(FALSE)
      }
    }
    
    if (input$navbar == "viz") {
      # Set the flag before changing the tab programmatically
      programmatic_change(TRUE)
      
      # Show relevant tabs for viz mode
      showTab(inputId = "navbar", target = "home")
      showTab(inputId = "navbar", target = "plot")
      showTab(inputId = "navbar", target = "map")
      if (!config$public & config$g_drive) { # If not public AND g drive access is possible
        showTab(inputId = "navbar", target = "FOD")
      }
      showTab(inputId = "navbar", target = "gen")
      showTab(inputId = "navbar", target = "img")
      showTab(inputId = "navbar", target = "info") # Actually a navbarMenu, and this targets the tabs 'news' and 'about' as well
      # don't show 'admin' tab unless logged in
      if (user_logged_in()) {  # this UI element is generated upon successful login
        showTab(inputId = "navbar", target = "admin")
      }
      
      # Hide irrelevant tabs for viz mode
      hideTab(inputId = "navbar", target = "viz")
      hideTab(inputId = "navbar", target = "locs")
      hideTab(inputId = "navbar", target = "ts")
      hideTab(inputId = "navbar", target = "equip")
      hideTab(inputId = "navbar", target = "cal")
      hideTab(inputId = "navbar", target = "data") # Actually a navbarMenu, and this targets the tabs 'contData' and 'discData' as well
      hideTab(inputId = "navbar", target = "files") # Actually a navbarMenu, and this targets the tabs 'addDocs' and 'addImgs' as well
      hideTab(inputId = "navbar", target = "visit")
      
      # Select the last tab the user was on in viz mode
      updateTabsetPanel(session, "navbar", selected = last_viz_tab())
      
    } else if (input$navbar == "admin") {
      programmatic_change(TRUE)
      
      # Create the tabs if they're not there yet
      if (!tab_created$locs) {
        insertTab("navbar",
                  tabPanel(title = "Manage locations", value = "locs",
                           uiOutput("locs_ui")),
                  target = "gen", position = "after")
        tab_created$locs <- TRUE
      }
      if (!tab_created$ts) {
        insertTab("navbar",
                  tabPanel(title = "Manage timeseries", value = "ts",
                           uiOutput("ts_ui")),
                  target = "locs", position = "after")
        tab_created$ts <- TRUE
      }
      if (!tab_created$equip) {
        insertTab("navbar",
                  tabPanel(title = "Manage equipment", value = "equip",
                           uiOutput("equip_ui")),
                  target = "ts", position = "after")
        tab_created$equip <- TRUE
      }
      if (!tab_created$cal) {
        insertTab("navbar",
                  tabPanel(title = "Enter checks/calibrations", value = "cal",
                           uiOutput("cal_ui")),
                  target = "equip", position = "after")
        tab_created$cal <- TRUE
      }
      # Create the navbarMenu that holds the continuous and discrete data tabs
      if (!tab_created$data) { 
        insertTab("navbar",
                  navbarMenu(title = "Manage data", menuName = "data",
                             tabPanel(title = "Continuous data", value = "contData",
                                      uiOutput("contData_ui")),
                             tabPanel(title = "Discrete data", value = "discData",
                                      uiOutput("discData_ui"))),
                  target = "cal", position = "after")
        tab_created$data <- TRUE
        tab_created$contData <- TRUE
        tab_created$discData <- TRUE
      }
      # Create the navbarMenu for docs/images/vectors/rasters
      if (!tab_created$files) {
        insertTab("navbar",
                  navbarMenu(title = "Manage files/docs", menuName = "files",
                             tabPanel(title = "Documents", value = "addDocs",
                                      uiOutput("addDocs_ui")),
                             tabPanel(title = "Images", value = "addImgs",
                                      uiOutput("addImgs_ui"))
                             #.... plus extra tabs for vectors and rasters when built
                             ),
                  target = "data", position = "after")
        tab_created$files <- TRUE
        tab_created$addDocs <- TRUE
        tab_created$addImgs <- TRUE
      }
      if (!tab_created$visit) {
        insertTab("navbar",
                  tabPanel(title = "Add/modify field visit", value = "visit",
                           uiOutput("visit_ui")),
                  target = "files", position = "after")
        tab_created$visit <- TRUE
      }
      
      # Show relevant tabs for admin mode
      showTab(inputId = "navbar", target = "viz")
      showTab(inputId = "navbar", target = "locs")
      showTab(inputId = "navbar", target = "ts")
      showTab(inputId = "navbar", target = "equip")
      showTab(inputId = "navbar", target = "cal")
      showTab(inputId = "navbar", target = "data") # Actually a navbarMenu, and this targets the tabs 'contData' and 'discData' as well
      showTab(inputId = "navbar", target = "files") # Actually a navbarMenu, and this targets the tabs 'addDocs' and 'addImgs' as well
      showTab(inputId = "navbar", target = "visit")
      
      # Hide irrelevant tabs/menus
      hideTab(inputId = "navbar", target = "admin")
      hideTab(inputId = "navbar", target = "home")
      hideTab(inputId = "navbar", target = "plot")
      hideTab(inputId = "navbar", target = "map")
      hideTab(inputId = "navbar", target = "FOD")
      hideTab(inputId = "navbar", target = "gen")
      hideTab(inputId = "navbar", target = "img")
      hideTab(inputId = "navbar", target = "info") # Actually a navbarMenu, and this targets the tabs 'news' and 'about' as well
      
      # Select the last tab the user was on in admin mode
      updateTabsetPanel(session, "navbar", selected = last_admin_tab())
      
    } else {
      # When user selects any other tab, update the last active tab for the current mode
      if (input$navbar %in% c("home", "plot", "map", "FOD", "gen", "img", "about", "news")) {
        # User is in viz mode
        last_viz_tab(input$navbar)
      } else if (input$navbar %in% c("locs", "ts", "equip", "cal", "contData", "discData", "addDocs", "addImgs", "visit")) {
        # User is in admin mode
        last_admin_tab(input$navbar) 
      }
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
          updateNavbarPage(session, "navbar", selected = (primary_outputs$map_main$change_tab))
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
    if (input$navbar == "img") {
      if (!ui_loaded$img) {
        output$img_ui <- renderUI(imgUI("img"))
        ui_loaded$img <- TRUE
        img("img", language = languageSelection, restoring = isRestoring_img) # Call the server
      }
    }
    if (input$navbar == "gen") {
      if (!ui_loaded$gen) {
        output$gen_ui <- renderUI(genUI("gen"))
        ui_loaded$gen <- TRUE
        gen("gen", mdb_files, language = languageSelection) # Call the server
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
    if (input$navbar == "equip") {
      if (!ui_loaded$equip) {
        output$equip_ui <- renderUI(equipUI("equip"))  # Render the UI
        ui_loaded$equip <- TRUE
        equip("equip")  # Call the server
      }
    }
    if (input$navbar == "cal") {
      if (!ui_loaded$cal) {
        output$cal_ui <- renderUI(calUI("cal"))  # Render the UI
        ui_loaded$cal <- TRUE
        cal("cal")  # Call the server
      }
    }
    if (input$navbar == "contData") {
      if (!ui_loaded$contData) {
        output$contData_ui <- renderUI(contDataUI("contData"))  # Render the UI
        ui_loaded$contData <- TRUE
        contData("contData")  # Call the server
      }
    }
    if (input$navbar == "discData") {
      if (!ui_loaded$discData) {
        output$discData_ui <- renderUI(discDataUI("discData"))  # Render the UI
        ui_loaded$discData <- TRUE
        discData("discData")  # Call the server
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
