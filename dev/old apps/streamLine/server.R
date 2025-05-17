# StreamLine application main server

server <- function(input, output, session) {
  
  # Initial setup #############################################################
  # Automatically update URL every time an input changes
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  setBookmarkExclude(c("userLang", "loginBtn"))
  
  # Update the query string
  onBookmarked(updateQueryString)
  
  isRestoring <- reactiveVal(FALSE)
  isRestoring_home <- reactiveVal(FALSE)
  isRestoring_map <- reactiveVal(FALSE)
  isRestoring_data <- reactiveVal(FALSE)
  isRestoring_plot <- reactiveVal(FALSE)
  isRestoring_img <- reactiveVal(FALSE)
  isRestoring_doc <- reactiveVal(FALSE)
  isRestoring_about <- reactiveVal(FALSE)
  
  onRestore(function(state) {
    isRestoring(TRUE)
    isRestoring_home(TRUE)
    isRestoring_map(TRUE)
    isRestoring_data(TRUE)
    isRestoring_plot(TRUE)
    isRestoring_img(TRUE)
    isRestoring_doc(TRUE)
    isRestoring_about(TRUE)
  })
  
  # Language selection ########################################################
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
  
  # Language selection reactives and observers based on the user's selected language (which is automatically set to the browser's language on load)
  languageSelection <- reactiveValues() # holds language and abbreviation
  
  # In contrast to input$userLang, input$langSelect is created in the UI and is the language selected by the user.
  observeEvent(input$langSelect, { # Set the language based on the user's selection. This is done in an if statement in case the user types in something which isn't a language option.
    if (input$langSelect %in% names(translations)[-c(1,2)]) {
      languageSelection$language <- input$langSelect
    }
  })
  
  observe({ # Find the abbreviation for use in the 'titleCase' function
    languageSelection$abbrev <- translations[id == "titleCase", get(languageSelection$language)][[1]]
  })
  
  # Update bits of text based on the selected language
  observe({
    session$sendCustomMessage(type = 'updateLang', message = list(lang = ifelse(languageSelection$language == "Français", "fr", "en")))  # Updates the language in the web page html head.
    output$home_title <- renderText({
      HTML(paste0('<div class="nunito-sans" style="font-size: 17px; font-weight: 500; font-style: normal;">',
                  translations[id == "home", get(languageSelection$language)][[1]],
                  '</div>'))
    })
    output$map_title <- renderText({
      HTML(paste0('<div class="nunito-sans" style="font-size: 17px; font-weight: 500; font-style: normal;">',
                  translations[id == "map_view_title", get(languageSelection$language)][[1]],
                  '</div>'))
    })
    output$data_title <- renderText({
      HTML(paste0('<div class="nunito-sans" style="font-size: 17px; font-weight: 500; font-style: normal;">',
                  translations[id == "data_view_title", get(languageSelection$language)][[1]],
                  '</div>'))
    })
    output$plot_title <- renderText({
      HTML(paste0('<div class="nunito-sans" style="font-size: 17px; font-weight: 500; font-style: normal;">',
                  translations[id == "plot_view_title", get(languageSelection$language)][[1]],
                  '</div>'))
    })
    output$img_title <- renderText({
      HTML(paste0('<div class="nunito-sans" style="font-size: 17px; font-weight: 500; font-style: normal;">',
                  translations[id == "img_view_title", get(languageSelection$language)][[1]],
                  '</div>'))
    })
    output$doc_title <- renderText({
      HTML(paste0('<div class="nunito-sans" style="font-size: 17px; font-weight: 500; font-style: normal;">',
                  translations[id == "doc_view_title", get(languageSelection$language)][[1]],
                  '</div>'))    
    })
    output$about_title <- renderText({
      HTML(paste0('<div class="nunito-sans" style="font-size: 17px; font-weight: 500; font-style: normal;">',
                  translations[id == "about_view_title", get(languageSelection$language)][[1]],
                  '</div>'))
    })
    
    # Update the mailto link with the correct language
    subject <- translations[id == "feedback", get(languageSelection$language)][[1]]
    body <- translations[id == "feedback_text", get(languageSelection$language)][[1]]
    mailtoLink <- sprintf("mailto:waterlevels@yukon.ca?subject=%s&body=%s", URLencode(subject), URLencode(body))
    session$sendCustomMessage("updateMailtoLink", mailtoLink) #Update using custom JS handler defined in the UI
  })
  
  # Some elements lack attributes that screen readers use to identify them. This adds an aria-label to the language selector.
  observe({
    shinyjs::runjs('$("#langSelect-selectized").attr("aria-label", "Language Selector");')
    shinyjs::runjs('$("#langSelect").attr("title", "Language Selector");')
  })
  
  
  # Log in as public user by default #################################################
  # has to happen before any data is fetched
  DBI::dbExecute(pool, "SET logged_in_user.username = 'public';")
  
  # Get data from the database #################################################
  # Get info from database (passed to multiple modules). Data is re-fetched after successful login via an observeEvent.
  DBdata <- reactiveValues(
    locations = dbGetQueryDT(pool, "SELECT location, location_id, name, latitude, longitude, geom_id, name_fr FROM locations;"),
    timeseries = dbGetQueryDT(pool, "SELECT timeseries_id, location_id, parameter_id, media_id, aggregation_type, record_rate, category, start_datetime, end_datetime FROM timeseries;"),
    locations_projects = dbGetQueryDT(pool, "SELECT * FROM locations_projects;"),
    locations_networks = dbGetQueryDT(pool, "SELECT * FROM locations_networks;"),
    media_types = dbGetQueryDT(pool, "SELECT p.* FROM media_types AS p WHERE EXISTS (SELECT 1 FROM timeseries t WHERE t.media_id = p.media_id);"),
    param_groups = dbGetQueryDT(pool, "SELECT DISTINCT p.group_id, p.group_name, p.group_name_fr FROM parameter_groups AS p WHERE EXISTS (SELECT 1 FROM parameter_relationships r WHERE r.group_id = p.group_id);"),
    parameters = dbGetQueryDT(pool, "SELECT p.parameter_id, p.param_name, p.param_name_fr, p.unit_default AS unit, p.unit_solid AS unit_solid FROM parameters AS p WHERE EXISTS (SELECT 1 FROM timeseries t WHERE t.parameter_id = p.parameter_id);"),
    projects = dbGetQueryDT(pool, "SELECT p.* FROM projects AS p WHERE EXISTS (SELECT 1 FROM locations_projects lp WHERE lp.project_id = p.project_id);"),
    networks =  dbGetQueryDT(pool, "SELECT n.* FROM networks AS n WHERE EXISTS (SELECT 1 FROM locations_networks ln WHERE ln.network_id = n.network_id);"),
    has_images = dbGetQueryDT(pool, "SELECT DISTINCT location_id FROM image_series;"),
    has_documents = dbGetQueryDT(pool, "SELECT DISTINCT locations.location_id FROM locations JOIN documents_spatial ON locations.geom_id = documents_spatial.geom_id JOIN documents ON documents_spatial.document_id = documents.document_id;")
  )
  
  # Log in to the database (optional) ##########################################
  observeEvent(input$loginBtn, {
    showModal(modalDialog(
      title = "Login",
      renderUI(HTML(translations[id == "login_txt", get(languageSelection$language)][[1]], "<br> <br>")),
      
      textInput("username", translations[id == "un", get(languageSelection$language)][[1]]),
      passwordInput("password", translations[id == "pwd", get(languageSelection$language)][[1]]),
      footer = tagList(
        modalButton(translations[id == "close", get(languageSelection$language)][[1]]),
        actionButton("confirmLogin", translations[id == "login_confirm", get(languageSelection$language)][[1]], class = "btn-primary")
      )
    ))
  })
  # Log in attempt if the button is clicked
  log_attempts <- reactiveVal(0) # counter for login attempts
  observeEvent(input$confirmLogin, {
    log_attempts(log_attempts() + 1)
    if (log_attempts() > 3) {
      showModal(modalDialog(
        title = translations[id == "login_fail", get(languageSelection$language)][[1]],
        translations[id == "login_fail_attempts", get(languageSelection$language)][[1]],
        easyClose = TRUE,
        footer = modalButton(translations[id == "close", get(languageSelection$language)][[1]])
      ))
      return()
    } else {
      res <- validateACUser(input$username, input$password, pool)
      if (res) {
        # Using parametrization to prevent injection attacks
        safe_username <- DBI::dbQuoteIdentifier(pool, input$username)
        query <- glue::glue_sql("SET logged_in_user.username = {safe_username}", .con = pool)
        DBI::dbExecute(pool, query)
        
        # Show a pop-up message to the user that they are logged in
        showModal(modalDialog(
          title = translations[id == "login_success", get(languageSelection$language)][[1]],
          paste0(translations[id == "login_success_msg", get(languageSelection$language)][[1]], " '", input$username, "'."),
          easyClose = TRUE,
          footer = modalButton(translations[id == "close", get(languageSelection$language)][[1]])
        ))
        
        # Re-fetch data from the database after successful login
        DBdata$locations <- dbGetQueryDT(pool, "SELECT location, location_id, name, latitude, longitude, geom_id, name_fr FROM locations;")
        DBdata$timeseries <- dbGetQueryDT(pool, "SELECT timeseries_id, location_id, parameter_id, media_id, aggregation_type, record_rate, category, start_datetime, end_datetime FROM timeseries;")
        DBdata$locations_projects <- dbGetQueryDT(pool, "SELECT * FROM locations_projects;")
        DBdata$locations_networks <- dbGetQueryDT(pool, "SELECT * FROM locations_networks;")
        DBdata$media_types <- dbGetQueryDT(pool, "SELECT p.* FROM media_types AS p WHERE EXISTS (SELECT 1 FROM timeseries t WHERE t.media_id = p.media_id);")
        DBdata$param_groups <- dbGetQueryDT(pool, "SELECT DISTINCT p.group, p.group_fr FROM parameters AS p WHERE EXISTS (SELECT 1 FROM timeseries t WHERE t.parameter_id = p.parameter_id);")
        DBdata$parameters <- dbGetQueryDT(pool, "SELECT p.parameter_id, p.param_name, p.param_name_fr, p.unit_default AS unit, p.unit_solid AS unit_solid FROM parameters AS p WHERE EXISTS (SELECT 1 FROM timeseries t WHERE t.parameter_id = p.parameter_id);")
        DBdata$projects <- dbGetQueryDT(pool, "SELECT p.* FROM projects AS p WHERE EXISTS (SELECT 1 FROM locations_projects lp WHERE lp.project_id = p.project_id);")
        DBdata$networks <- dbGetQueryDT(pool, "SELECT n.* FROM networks AS n WHERE EXISTS (SELECT 1 FROM locations_networks ln WHERE ln.network_id = n.network_id);")
        DBdata$has_images <- dbGetQueryDT(pool, "SELECT DISTINCT location_id FROM image_series;")
        DBdata$has_documents <- dbGetQueryDT(pool, "SELECT DISTINCT locations.location_id FROM locations JOIN documents_spatial ON locations.geom_id = documents_spatial.geom_id JOIN documents ON documents_spatial.document_id = documents.document_id;")
        
        # Reload the tab the user is on to incorporate the new data
        updateNavbarPage(session, "navbar", selected = input$navbar)
        
      } else {
        DBI::dbExecute(pool, "SET logged_in_user.username = 'public';")
        # Show a pop-up message to the user that the login failed
        showModal(modalDialog(
          title = translations[id == "login_fail", get(languageSelection$language)][[1]],
          translations[id == "login_fail_msg", get(languageSelection$language)][[1]],
          easyClose = TRUE,
          footer = modalButton(translations[id == "close", get(languageSelection$language)][[1]])
        ))
      } 
    }
  })
  

  # Load specific modules based on input$navbar ################################
  # Store information to pass between modules
  moduleOutputs <- reactiveValues()
  lastWorkingTab <- reactiveVal("home")  # Initial or safe tab
  observeEvent(input$navbar, {
    if (input$navbar == "home") {
      tryCatch({
        home("home", language = languageSelection, restoring = isRestoring_home)
      }, error = function(e) {
        showModal(modalDialog(
          title = translations[id == "errorModalTitle", get(languageSelection$language)][[1]],
          translations[id == "errorModalMsg", get(languageSelection$language)][[1]],
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        # Reset to last good tab
        updateNavbarPage(session, "navbar", selected = lastWorkingTab())
      })
    }
    if (input$navbar == "map") {
      tryCatch({
        moduleOutputs$map_outputs <- map("map", language = languageSelection, restoring = isRestoring_map, data = DBdata)  # Assigning the module to a variable enables it to send values back to the server.
        observe({  # Observe the map_outputs reactive to see if the tab should be changed, for example when the user clicks on a location's pop-up links to go to data or plot tabs.
          if (!is.null(moduleOutputs$map_outputs$change_tab)) {
            updateNavbarPage(session, "navbar", selected = moduleOutputs$map_outputs$change_tab)
            moduleOutputs$map_outputs$change_tab <- NULL
          }
        })
      }, error = function(e) {
        showModal(modalDialog(
          title = translations[id == "errorModalTitle", get(languageSelection$language)][[1]],
          translations[id == "errorModalMsg", get(languageSelection$language)][[1]],
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        updateNavbarPage(session, "navbar", selected = lastWorkingTab())
      })
    }
    if (input$navbar == "data") {
      tryCatch({
        data("data", con = pool, language = languageSelection, restoring = isRestoring_data, data = DBdata, inputs = moduleOutputs$map_outputs$location_id)
      }, error = function(e) {
        showModal(modalDialog(
          title = translations[id == "errorModalTitle", get(languageSelection$language)][[1]],
          translations[id == "errorModalMsg", get(languageSelection$language)][[1]],
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        updateNavbarPage(session, "navbar", selected = lastWorkingTab())
      })
    }
    if (input$navbar == "plot") {
      tryCatch({
        plot("plot", con = pool, language = languageSelection, restoring = isRestoring_plot, data = DBdata, inputs = moduleOutputs$map_outputs$location_id)
      }, error = function(e) {
        showModal(modalDialog(
          title = translations[id == "errorModalTitle", get(languageSelection$language)][[1]],
          translations[id == "errorModalMsg", get(languageSelection$language)][[1]],
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        updateNavbarPage(session, "navbar", selected = lastWorkingTab())
      })
    }
    if (input$navbar == "img") {
      tryCatch({
        img("img", con = pool, language = languageSelection, restoring = isRestoring_img)  # Images module fetches its own data as it has to be very up to date.
      }, error = function(e) {
        showModal(modalDialog(
          title = translations[id == "errorModalTitle", get(languageSelection$language)][[1]],
          translations[id == "errorModalMsg", get(languageSelection$language)][[1]],
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        updateNavbarPage(session, "navbar", selected = lastWorkingTab())
      })
    }
    if (input$navbar == "doc") {
      tryCatch({
        doc("doc", con = pool, language = languageSelection, restoring = isRestoring_doc, data = DBdata)
      }, error = function(e) {
        showModal(modalDialog(
          title = translations[id == "errorModalTitle", get(languageSelection$language)][[1]],
          translations[id == "errorModalMsg", get(languageSelection$language)][[1]],
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        updateNavbarPage(session, "navbar", selected = lastWorkingTab())
      })
    }
    if (input$navbar == "about") {
      tryCatch({
        about("about", con = pool, language = languageSelection, restoring = isRestoring_about)
      }, error = function(e) {
        showModal(modalDialog(
          title = translations[id == "errorModalTitle", get(languageSelection$language)][[1]],
          translations[id == "errorModalMsg", get(languageSelection$language)][[1]],
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        updateNavbarPage(session, "navbar", selected = lastWorkingTab())
      })
    }
    # Update last working tab on successful tab switch
    lastWorkingTab(input$navbar)
  }, ignoreNULL = TRUE) # End of observeEvent that loads modules based on navbar actions (or input$navbar changes passed from modules)
  
}
