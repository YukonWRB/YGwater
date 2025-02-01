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
  
  # Initialize reactive flags to track whether each UI has been loaded
  ui_loaded <- reactiveValues(
    viz = FALSE,
    admin = FALSE,
    plot = FALSE,
    map = FALSE,
    FOD = FALSE,
    img = FALSE,
    gen = FALSE,
    locs = FALSE,
    ts = FALSE,
    equip = FALSE,
    cal = FALSE,
    contData = FALSE,
    discData = FALSE,
    addDocs = FALSE,
    addImgs = FALSE,
    visit = FALSE)
  
  ## database connections ###########
  # Initial database connections without edit privileges
  if (dir.exists(config$accessPath)) {
    # List the *.mdb files in the directory
    mdb_files <- list.files(config$accessPath, pattern = "*.mdb", full.names = TRUE)
    if (length(mdb_files) == 0) {
      mdb_files <- NULL
    }
  } else {
    mdb_files <- NULL
  }
  
  if (is.null(mdb_files)) {
    print("No .mdb files found in the AccessPath directory.")
  }
  
  AquaCache <- AquaConnect(name = config$dbName, 
                           host = config$dbHost,
                           port = config$dbPort,
                           username = config$dbUser,
                           password = config$dbPass,
                           silent = TRUE)
  print("Connected to AquaCache")
  
  session$onUnhandledError(function() {
    DBI::dbDisconnect(AquaCache)
    print("Disconnected from AquaCache after unhandled error")
  })
  
  session$onSessionEnded(function() {
    DBI::dbDisconnect(AquaCache)
    print("Disconnected from AquaCache after session end")
  })
  
  # Language selection ########################################################
  
  # Language selection reactives and observers based on the user's selected language (which is automatically set to the browser's language on load)
  languageSelection <- reactiveValues() # holds language and abbreviation
  
  # Populate the language selection dropdown
  session$sendCustomMessage("updateLangMenu", names(translations)[-c(1,2)])
  
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
      DBI::dbDisconnect(AquaCache)
      AquaCache <<- AquaConnect(name = config$dbName, 
                                host = config$dbHost,
                                port = config$dbPort,
                                username = input$username, 
                                password = input$password, 
                                silent = TRUE)
      test <- DBI::dbGetQuery(AquaCache, "SELECT 1;")
      # Test the connection
      if (nrow(test) > 0) {
        showModal(modalDialog(
          title = "Login Successful",
          "You are now logged in.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        user_logged_in(TRUE)
        shinyjs::hide("loginBtn")
        shinyjs::show("logoutBtn")
        
        # Create the new tabs for the 'admin' mode
        # In the login success handler, after user_logged_in(TRUE):
        insertTab("navbar",
                  tabPanel(title = "Switch to Admin mode", value = "admin",
                           uiOutput("admin_ui")),
                  target = "map", position = "before")
        insertTab("navbar",
                  tabPanel(title = "Manage locations", value = "locs",
                           uiOutput("locs_ui")),
                  target = "gen", position = "after")
        insertTab("navbar",
                  tabPanel(title = "Manage timeseries", value = "ts",
                           uiOutput("ts_ui")),
                  target = "locs", position = "after")
        insertTab("navbar",
                  tabPanel(title = "Manage equipment", value = "equip",
                           uiOutput("equip_ui")),
                  target = "ts", position = "after")
        insertTab("navbar",
                  tabPanel(title = "Enter checks/calibrations", value = "cal",
                           uiOutput("cal_ui")),
                  target = "equip", position = "after")
        insertTab("navbar",
                  tabPanel(title = "Manage continuous data", value = "contData",
                           uiOutput("contData_ui")),
                  target = "cal", position = "after")
        insertTab("navbar",
                  tabPanel(title = "Manage discrete data", value = "discData",
                           uiOutput("discData_ui")),
                  target = "contData", position = "after")
        insertTab("navbar",
                  tabPanel(title = "Manage docs", value = "addDocs",
                           uiOutput("addDocs_ui")),
                  target = "discData", position = "after")
        insertTab("navbar",
                  tabPanel(title = "Manage images", value = "addImgs",
                           uiOutput("addImgs_ui")),
                  target = "addDocs", position = "after")
        insertTab("navbar",
                  tabPanel(title = "Add/modify field visit", value = "visit",
                           uiOutput("visit_ui")),
                  target = "addImgs", position = "after")
        
        updateTabsetPanel(session, "navbar", selected = "admin")
        return()
      } else {
        showModal(modalDialog(
          title = "Login Failed",
          "Invalid username or password.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        AquaCache <<- AquaConnect(name = config$dbName, 
                                  host = config$dbHost,
                                  port = config$dbPort,
                                  username = config$dbUser,
                                  password = config$dbPass,
                                  silent = TRUE)
        return()
      }
    }, error = function(e) {
      showModal(modalDialog(
        title = "Login Failed",
        "Invalid username or password.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      # Check to see if the connection is still open, if not reconnect
      if (!DBI::dbIsValid(AquaCache)) {
        AquaCache <<- AquaConnect(name = config$dbName, 
                                  host = config$dbHost,
                                  port = config$dbPort,
                                  username = config$dbUser,
                                  password = config$dbPass,
                                  silent = TRUE)
      }
      return()
    })
  })
  
  ## Log out #####################################################
  observeEvent(input$logoutBtn, {
    
    # NOTE! Double assignment is used when (re)creating the connection to get out of the observer's scope into the environment.
    user_logged_in(FALSE)  # Set login status to FALSE
    # Hide the 'admin' tabs upon logout
    hideTab(inputId = "navbar", target = "admin")
    showTab(inputId = "navbar", target = "viz", select = TRUE)
    
    # change the 'Logout' button back to 'Login'
    shinyjs::hide("logoutBtn")
    shinyjs::show("loginBtn")
    
    DBI::dbDisconnect(AquaCache)
    AquaCache <<- AquaConnect(name = config$dbName, 
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
    removeTab("navbar", "contData", session = session)
    removeTab("navbar", "discData", session = session)
    removeTab("navbar", "addDocs", session = session)
    removeTab("navbar", "addImgs", session = session)
    removeTab("navbar", "visit", session = session)
  })
  
  # Load modules based on input$navbar ################################
  # Store information to pass between modules
  primary_outputs <- reactiveValues()
  
  # Initialize a flag to track programmatic tab changes
  programmatic_change <- reactiveVal(FALSE)
  
  # Initialize reactive values to store last tabs for each mode
  last_viz_tab <- reactiveVal("map")      # Default tab for viz mode
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
      showTab(inputId = "navbar", target = "plot")
      showTab(inputId = "navbar", target = "map")
      if (!config$public & config$g_drive) { # If not public AND g drive access is possible
        showTab(inputId = "navbar", target = "FOD")
      }
      showTab(inputId = "navbar", target = "gen")
      showTab(inputId = "navbar", target = "img")
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
      hideTab(inputId = "navbar", target = "contData")
      hideTab(inputId = "navbar", target = "discData")
      hideTab(inputId = "navbar", target = "addDocs")
      hideTab(inputId = "navbar", target = "addImgs")
      hideTab(inputId = "navbar", target = "visit")
      
      # Select the last tab the user was on in viz mode
      updateTabsetPanel(session, "navbar", selected = last_viz_tab())
      
    } else if (input$navbar == "admin") {
      programmatic_change(TRUE)
      
      # Show relevant tabs for admin mode
      showTab(inputId = "navbar", target = "viz")
      showTab(inputId = "navbar", target = "locs")
      showTab(inputId = "navbar", target = "ts")
      showTab(inputId = "navbar", target = "equip")
      showTab(inputId = "navbar", target = "cal")
      showTab(inputId = "navbar", target = "contData")
      showTab(inputId = "navbar", target = "discData")
      showTab(inputId = "navbar", target = "addDocs")
      showTab(inputId = "navbar", target = "addImgs")
      showTab(inputId = "navbar", target = "visit")
      
      # Hide irrelevant tabs
      hideTab(inputId = "navbar", target = "admin")
      hideTab(inputId = "navbar", target = "plot")
      hideTab(inputId = "navbar", target = "map")
      hideTab(inputId = "navbar", target = "FOD")
      hideTab(inputId = "navbar", target = "gen")
      hideTab(inputId = "navbar", target = "img")
      
      # Select the last tab the user was on in admin mode
      updateTabsetPanel(session, "navbar", selected = last_admin_tab())
      
    } else {
      # When user selects any other tab, update the last active tab for the current mode
      if (input$navbar %in% c("plot", "map", "FOD", "gen", "img")) {
        # User is in viz mode
        last_viz_tab(input$navbar)
      } else if (input$navbar %in% c("locs", "ts", "equip", "cal", "contData", "discData", "addDocs", "addImgs", "visit")) {
        # User is in admin mode
        last_admin_tab(input$navbar) 
      }
    }
    
    # Load modules when the corresponding tabs are selected
    if (input$navbar == "plot") {
      if (!ui_loaded$plot) {
        output$plot_ui <- renderUI(plotUI("plot"))
        ui_loaded$plot <- TRUE
        plot("plot", mdb_files, AquaCache, language = languageSelection) # Call the server
      }
    }
    if (input$navbar == "map") {
      if (!ui_loaded$map) {
        output$map_ui <- renderUI(mapUI("map"))
        ui_loaded$map <- TRUE
        primary_outputs$map_main <- map("map", AquaCache, language = languageSelection) # Call the server
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
        img("img", con = AquaCache, language = languageSelection, restoring = isRestoring_img) # Call the server
      }
    }
    if (input$navbar == "gen") {
      if (!ui_loaded$gen) {
        output$gen_ui <- renderUI(genUI("gen"))
        ui_loaded$gen <- TRUE
        gen("gen", mdb_files, con = AquaCache, language = languageSelection) # Call the server
      }
    }
    if (input$navbar == "locs") {
      if (!ui_loaded$locs) {
        output$locs_ui <- renderUI(locsUI("locs"))
        ui_loaded$locs <- TRUE
        locs("locs", AquaCache) # Call the server
      }
    }
    if (input$navbar == "ts") {
      if (!ui_loaded$ts) {
        output$ts_ui <- renderUI(tsUI("ts"))
        ui_loaded$ts <- TRUE
        ts("ts", AquaCache) # Call the server
      }
    }
    if (input$navbar == "equip") {
      if (!ui_loaded$equip) {
        output$equip_ui <- renderUI(equipUI("equip"))  # Render the UI
        ui_loaded$equip <- TRUE
        equip("equip", AquaCache)  # Call the server
      }
    }
    if (input$navbar == "cal") {
      if (!ui_loaded$cal) {
        output$cal_ui <- renderUI(calUI("cal"))  # Render the UI
        ui_loaded$cal <- TRUE
        cal("cal", AquaCache)  # Call the server
      }
    }
    if (input$navbar == "contData") {
      if (!ui_loaded$contData) {
        output$contData_ui <- renderUI(contDataUI("contData"))  # Render the UI
        ui_loaded$contData <- TRUE
        contData("contData", AquaCache)  # Call the server
      }
    }
    if (input$navbar == "discData") {
      if (!ui_loaded$discData) {
        output$discData_ui <- renderUI(discDataUI("discData"))  # Render the UI
        ui_loaded$discData <- TRUE
        discData("discData", AquaCache)  # Call the server
      }
    }
    if (input$navbar == "addDocs") {
      if (!ui_loaded$addDocs) {
        output$addDocs_ui <- renderUI(addDocsUI("addDocs"))  # Render the UI
        ui_loaded$addDocs <- TRUE
        addDocs("addDocs", AquaCache)  # Call the server
      }
    }
    if (input$navbar == "addImgs") {
      if (!ui_loaded$addImgs) {
        output$addImgs_ui <- renderUI(addImgsUI("addImgs"))  # Render the UI
        ui_loaded$addImgs <- TRUE
        addImgs("addImgs", AquaCache)  # Call the server
      }
    }
    if (input$navbar == "visit") {
      if (!ui_loaded$visit) {
        output$visit_ui <- renderUI(visitUI("visit"))  # Render the UI
        ui_loaded$visit <- TRUE
        visit("visit", AquaCache)  # Call the server
      }
    }
  }) # End of observeEvent for loading modules based on navbar
  
} # End of main server
