#' The YGwater app server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
  
  shinyjs::useShinyjs()
  
  # Initial database connections without edit privileges
  EQWin <- AccessConnect("//env-fs/env-data/corp/water/Data/Databases_virtual_machines/databases/EQWinDB/WaterResources.mdb", silent = TRUE)
  AquaCache <- AquaConnect(silent = TRUE)
  print("Connected to EQWin")
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
  
  # Initialize a flag to track programmatic tab changes
  programmatic_change <- reactiveVal(FALSE)
  
  # Initialize reactive values to store last tabs for each mode
  last_visualize_tab <- reactiveVal("plot")      # Default tab for visualize mode
  last_admin_tab <- reactiveVal("metadata")      # Default tab for admin mode
  initial_tab <- reactiveVal(NULL)
  
  # Initially hide all tabs except "visualize"
  hideTab(inputId = "navbar", target = "plot")
  hideTab(inputId = "navbar", target = "map")
  hideTab(inputId = "navbar", target = "FOD")
  hideTab(inputId = "navbar", target = "generate")
  hideTab(inputId = "navbar", target = "metadata")
  hideTab(inputId = "navbar", target = "new_ts_loc")
  hideTab(inputId = "navbar", target = "basins")
  hideTab(inputId = "navbar", target = "admin")  # Hide 'admin' tab initially as it should only be seen upon login
  
  # Log in to the database (optional for editing privileges) ##########################################
  log_attempts <- reactiveVal(0) # counter for login attempts
  user_logged_in <- reactiveVal(FALSE) # Reactive value to track login status
  
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
        AquaCache <- AquaConnect(username = input$username, password = input$password, silent = TRUE)
        # Test the connection
        test <- DBI::dbGetQuery(AquaCache, "SELECT 1;")
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
          
          # Redirect to 'admin' tab
          showTab(inputId = "navbar", target = "admin")
          updateTabsetPanel(session, "navbar", selected = "admin")
          return()
        } else {
          showModal(modalDialog(
            title = "Login Failed",
            "Invalid username or password.",
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          AquaCache <- AquaConnect(silent = TRUE)
          return()
        }
      }, error = function(e) {
        showModal(modalDialog(
          title = "Login Failed",
          "Invalid username or password.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        AquaCache <- AquaConnect(silent = TRUE)
        return()
      })
  })
  
  # Logout functionality #####################################################
  observeEvent(input$logoutBtn, {
    user_logged_in(FALSE)  # Set login status to FALSE
    # Hide the 'admin' tabs upon logout
    hideTab(inputId = "navbar", target = "admin")
    showTab(inputId = "navbar", target = "visualize", select = TRUE)
    
    # change the 'Logout' button back to 'Login'
    shinyjs::hide("logoutBtn")
    shinyjs::show("loginBtn")
    
    AquaCache <- AquaConnect(silent = TRUE)
    # Redirect to 'visualize' tab
    updateTabsetPanel(session, "navbar", selected = "visualize")
    hideTab(inputId = "navbar", target = "admin")
  })
  
  
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
    
    if (input$navbar == "visualize") {
      # Set the flag before changing the tab programmatically
      programmatic_change(TRUE)
      
      # Show relevant tabs for visualize mode
      showTab(inputId = "navbar", target = "plot")
      showTab(inputId = "navbar", target = "map")
      showTab(inputId = "navbar", target = "FOD")
      showTab(inputId = "navbar", target = "generate")
      # Hide 'admin' tab unless logged in
      if (user_logged_in()) {
        showTab(inputId = "navbar", target = "admin")
      }

      # Hide irrelevant tabs
      hideTab(inputId = "navbar", target = "metadata")
      hideTab(inputId = "navbar", target = "new_ts_loc")
      hideTab(inputId = "navbar", target = "basins")
      hideTab(inputId = "navbar", target = "visualize")
      
      # Select the last tab the user was on in visualize mode
      updateTabsetPanel(session, "navbar", selected = last_visualize_tab())
      
    } else if (input$navbar == "admin") {
      programmatic_change(TRUE)
      
      # Show relevant tabs for admin mode
      showTab(inputId = "navbar", target = "metadata")
      showTab(inputId = "navbar", target = "new_ts_loc")
      showTab(inputId = "navbar", target = "basins")
      showTab(inputId = "navbar", target = "visualize")

      
      # Hide irrelevant tabs
      hideTab(inputId = "navbar", target = "plot")
      hideTab(inputId = "navbar", target = "map")
      hideTab(inputId = "navbar", target = "FOD")
      hideTab(inputId = "navbar", target = "admin")
      hideTab(inputId = "navbar", target = "generate")
      
      # Select the last tab the user was on in admin mode
      updateTabsetPanel(session, "navbar", selected = last_admin_tab())
      
    } else {
      # When user selects any other tab, update the last active tab for the current mode
      if (input$navbar %in% c("plot", "map", "FOD", "generate")) {
        # User is in visualize mode
        last_visualize_tab(input$navbar)
      } else if (input$navbar %in% c("metadata", "new_ts_loc", "basins")) {
        # User is in admin mode
        last_admin_tab(input$navbar)
      }
    }
    
    # Load modules when the corresponding tabs are selected
    if (input$navbar == "plot") {
      plot("plot", EQWin, AquaCache)
    }
    if (input$navbar == "map") {
      map("map", EQWin, AquaCache)
    }
    if (input$navbar == "FOD") {
      FOD("FOD")
    }
    if (input$navbar == "generate") {
      generate("generate", EQWin, AquaCache)
    }
    if (input$navbar == "basins") {
      basins("basins", AquaCache)
    }
    if (input$navbar == "metadata") {
      metadata("metadata", AquaCache)
    }
  })
}
