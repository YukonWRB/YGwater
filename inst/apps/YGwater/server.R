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
    metadata = FALSE,
    addTsLoc = FALSE,
    addImg = FALSE,
    basins = FALSE)
  
  ## database connections ###########
  # Initial database connections without edit privileges
  if (file.exists(config$accessPath)) {
    tryCatch({
      EQWin <- AccessConnect(config$accessPath)
      valid <- DBI::dbGetQuery(EQWin, "SELECT 1;")
      if (nrow(valid) == 0) {
        EQWin <- NULL
      }
      print("Connected to EQWin")
    }, error = function(e) {
      EQWin <<- NULL
    })
  } else {
    EQWin <- NULL
  }
  
  if (is.null(EQWin)) {
    print("Failed to connect to EQWin")
  }
  
  AquaCache <- AquaConnect(name = config$dbName, 
                           host = config$dbHost,
                           port = config$dbPort,
                           username = config$dbUser,
                           password = config$dbPass,
                           RLS_user = config$RLS_user,
                           RLS_pass = config$RLS_pass,
                           silent = TRUE)
  print("Connected to AquaCache")
  
  session$onUnhandledError(function() {
    DBI::dbDisconnect(EQWin)
    DBI::dbDisconnect(AquaCache)
    print("Disconnected from EQWin after unhandled error")
    print("Disconnected from AquaCache after unhandled error")
  })
  
  session$onSessionEnded(function() {
    DBI::dbDisconnect(EQWin)
    DBI::dbDisconnect(AquaCache)
    print("Disconnected from EQWin after session end")
    print("Disconnected from AquaCache after session end")
  })
  
  # Language selection ########################################################
  
  # Language selection reactives and observers based on the user's selected language (which is automatically set to the browser's language on load)
  languageSelection <- reactiveValues() # holds language and abbreviation
  
  # Determine user's browser language. This should only run once when the app is loaded.
  observe({
    if (!isRestoring()) {
      shinyjs::runjs("var language =  window.navigator.userLanguage || window.navigator.language;
Shiny.onInputChange('userLang', language);
console.log(language);")
    }
  })
  # Check if userLang contains en or fr in the string and set the language accordingly
  observeEvent(input$userLang, { #userLang is the language of the user's browser. input$userLang is created by the runjs function above and not in the UI.
    if (substr(input$userLang , 1, 2) == "en") {
      updateSelectizeInput(session, "langSelect", selected = "English")
      session$sendCustomMessage(type = 'updateLang', message = list(lang = "en"))  # Updates the language in the web page html head.
    } else if (substr(input$userLang , 1, 2) == "fr") {
      updateSelectizeInput(session, "langSelect", selected = "Français")
      session$sendCustomMessage(type = 'updateLang', message = list(lang = "fr"))  # Updates the language in the web page html head.
      
    } else {
      updateSelectizeInput(session, "langSelect", selected = "English")
      session$sendCustomMessage(type = 'updateLang', message = list(lang = "en"))  # Updates the language in the web page html head.
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  # In contrast to input$userLang, input$langSelect is created in the UI and is the language selected by the user.
  observeEvent(input$langSelect, { # Set the language based on the user's selection. This is done in an if statement in case the user types in something which isn't a language option.
    if (input$langSelect %in% names(translations)[-c(1,2)]) {
      languageSelection$language <- input$langSelect
    }
  })
  
  observe({ # Find the abbreviation for use in the 'titleCase' function
    languageSelection$abbrev <- translations[id == "titleCase", get(languageSelection$language)][[1]]
  })
  
  # Log in/out for edits ##########################################
  log_attempts <- reactiveVal(0) # counter for login attempts
  user_logged_in <- reactiveVal(FALSE) # Reactive value to track login status
  
  ## Log in #########
  observeEvent(input$loginBtn, {
    if (log_attempts() > 3) {
      showModal(modalDialog(
        title = "Login Failed",
        "You've exceeded the maximum number of login attempts.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    } else {
      showModal(modalDialog(
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
                                RLS_user = config$RLS_user,
                                RLS_pass = config$RLS_pass,
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
                  tabPanel(title = "Admin mode", value = "admin",
                           uiOutput("admin_ui")),
                  target = "map", position = "before")
        insertTab("navbar",
                  tabPanel(title = "View/edit metadata", value = "metadata",
                           uiOutput("metadata_ui")),
                  target = "gen", position = "after")
        insertTab("navbar",
                  tabPanel(title = "Add images", value = "addImg",
                           uiOutput("addImg_ui")),
                  target = "metadata", position = "after")
        insertTab("navbar",
                  tabPanel(title = "Add location/timeseries", value = "addTsLoc",
                           uiOutput("addTsLoc_ui")),
                  target = "addImg", position = "after")
        insertTab("navbar",
                  tabPanel(title = "Create basins", value = "basins",
                           uiOutput("basins_ui")),
                  target = "addTsLoc", position = "after")
        
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
                                  RLS_user = config$RLS_user,
                                  RLS_pass = config$RLS_pass,
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
      test <- DBI::dbGetQuery(AquaCache, "SELECT 1;")
      if (nrow(test) == 0) {
        AquaCache <<- AquaConnect(name = config$dbName, 
                                  host = config$dbHost,
                                  port = config$dbPort,
                                  username = config$dbUser,
                                  password = config$dbPass,
                                  RLS_user = config$RLS_user,
                                  RLS_pass = config$RLS_pass,
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
                              RLS_user = config$RLS_user,
                              RLS_pass = config$RLS_pass,
                              silent = TRUE)
    # Redirect to 'viz' tab
    updateTabsetPanel(session, "navbar", selected = "viz")
    # Remove admin-related tabs on logout
    removeTab("navbar", "admin", session = session)
    removeTab("navbar", "metadata", session = session)
    removeTab("navbar", "addImg", session = session)
    removeTab("navbar", "addTsLoc", session = session)
    removeTab("navbar", "basins", session = session)
  })
  
  # Load modules based on input$navbar ################################
  # Store information to pass between modules
  primary_outputs <- reactiveValues()
  
  # Initialize a flag to track programmatic tab changes
  programmatic_change <- reactiveVal(FALSE)
  
  # Initialize reactive values to store last tabs for each mode
  last_viz_tab <- reactiveVal("map")      # Default tab for viz mode
  last_admin_tab <- reactiveVal("metadata")      # Default tab for admin mode
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
      showTab(inputId = "navbar", target = "FOD")
      showTab(inputId = "navbar", target = "gen")
      showTab(inputId = "navbar", target = "img")
      # Hide 'admin' tab unless logged in
      if (user_logged_in()) {  # this UI element is generated upon successful login
        showTab(inputId = "navbar", target = "admin")
      }
      
      # Hide irrelevant tabs
      hideTab(inputId = "navbar", target = "metadata")
      hideTab(inputId = "navbar", target = "addImg")
      hideTab(inputId = "navbar", target = "addTsLoc")
      hideTab(inputId = "navbar", target = "basins")
      hideTab(inputId = "navbar", target = "viz")
      
      # Select the last tab the user was on in viz mode
      updateTabsetPanel(session, "navbar", selected = last_viz_tab())
      
    } else if (input$navbar == "admin") {
      programmatic_change(TRUE)
      
      # Show relevant tabs for admin mode
      showTab(inputId = "navbar", target = "metadata")
      showTab(inputId = "navbar", target = "addImg")
      showTab(inputId = "navbar", target = "addTsLoc")
      showTab(inputId = "navbar", target = "basins")
      showTab(inputId = "navbar", target = "viz")
      
      
      # Hide irrelevant tabs
      hideTab(inputId = "navbar", target = "plot")
      hideTab(inputId = "navbar", target = "map")
      hideTab(inputId = "navbar", target = "FOD")
      hideTab(inputId = "navbar", target = "admin")
      hideTab(inputId = "navbar", target = "gen")
      hideTab(inputId = "navbar", target = "img")
      
      # Select the last tab the user was on in admin mode
      updateTabsetPanel(session, "navbar", selected = last_admin_tab())
      
    } else {
      # When user selects any other tab, update the last active tab for the current mode
      if (input$navbar %in% c("plot", "map", "FOD", "gen", "img")) {
        # User is in viz mode
        last_viz_tab(input$navbar)
      } else if (input$navbar %in% c("metadata", "addImg", "addTsLoc", "basins")) {
        # User is in admin mode
        last_admin_tab(input$navbar) 
      }
    }
    
    # Load modules when the corresponding tabs are selected
    if (input$navbar == "plot") {
      if (!ui_loaded$plot) {
        output$plot_ui <- renderUI(plotUI("plot"))
        ui_loaded$plot <- TRUE
        plot("plot", EQWin, AquaCache) # Call the server
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
        gen("gen", EQWin, AquaCache) # Call the server
      }
    }
    if (input$navbar == "basins") {
      if (!ui_loaded$basins) {
        output$basins_ui <- renderUI(basinsUI("basins"))
        ui_loaded$basins <- TRUE
        basins("basins", AquaCache) # Call the server
      }
    }
    if (input$navbar == "metadata") {
      if (!ui_loaded$metadata) {
        output$metadata_ui <- renderUI(metadataUI("metadata"))
        ui_loaded$metadata <- TRUE
        metadata("metadata", AquaCache) # Call the server
      }
    }
    if (input$navbar == "addImg") {
      if (!ui_loaded$addImg) {
        output$addImg_ui <- renderUI(addImgUI("addImg"))  # Render the UI
        ui_loaded$addImg <- TRUE
        addImg("addImg", AquaCache)  # Call the server
      }
    }
  }) # End of observeEvent for loading modules based on navbar
  
} # End of main server
