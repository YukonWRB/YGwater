# StreamLine application main server

server <- function(input, output, session) {
  
  observeEvent(input$loginBtn, {
    showModal(modalDialog(
      title = "This doesn't work yet",
      renderUI(HTML("Login is reserved for Yukon Government users and partner organizations. Contact us if you think you should have access (access doesn't do anything special yet, so hold off until this message changes). <br> <br>")),
      textInput("username", "Username", "Nope, not working"),
      passwordInput("password", "Password", "Nope, not working"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirmLogin", "Log in", class = "btn-primary")
      )
    ))
  })
  # login <- reactiveVal(FALSE)
  # observeEvent(input$loginBtn, {
  #   if (!login) {
  #     showModal(modalDialog(
  #       title = "Login",
  #       textInput("username", "Username"),
  #       passwordInput("password", "Password"),
  #       footer = tagList(
  #         modalButton("Cancel"),
  #         actionButton("confirmLogin", "Log in", class = "btn-primary")
  #       )
  #     ))
  #   } else {
  #     login <- TRUE
  #     shinyjs::removeClass(selector = "body", class = "logged-in")
  #   }
  # })
  # 
  # observeEvent(input$confirmLogin, {
  #   if (input$username == "admin" && input$password == "pass") {  # Simplified check
  #     login <- TRUE
  #     shinyjs::addClass(selector = "body", class = "logged-in")
  #     removeModal()
  #   } else {
  #     showModal(modalDialog(
  #       title = "Error",
  #       "Incorrect username or password!",
  #       easyClose = TRUE,
  #       footer = modalButton("Close")
  #     ))
  #   }
  # })
  
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
  
  # Get info from database (passed to multiple modules)
  
  DBdata <- reactiveValues(
    locations = dbGetQueryDT(pool, "SELECT location, location_id, name, latitude, longitude, geom_id, name_fr FROM locations;"),
    timeseries = dbGetQueryDT(pool, "SELECT timeseries_id, location_id, parameter, param_type, period_type, category, start_datetime, end_datetime FROM timeseries;"),
    locations_projects = dbGetQueryDT(pool, "SELECT * FROM locations_projects;"),
    locations_networks = dbGetQueryDT(pool, "SELECT * FROM locations_networks;"),
    param_types = dbGetQueryDT(pool, "SELECT p.* FROM param_types AS p WHERE EXISTS (SELECT 1 FROM timeseries t WHERE t.param_type = p.param_type_code);"),
    param_groups = dbGetQueryDT(pool, "SELECT DISTINCT p.group, p.group_fr FROM parameters AS p WHERE EXISTS (SELECT 1 FROM timeseries t WHERE t.parameter = p.param_code);"),
    parameters = dbGetQueryDT(pool, "SELECT p.param_code, p.param_name, p.param_name_fr, p.group, p.group_fr, unit FROM parameters AS p WHERE EXISTS (SELECT 1 FROM timeseries t WHERE t.parameter = p.param_code);"),
    projects = dbGetQueryDT(pool, "SELECT p.* FROM projects AS p WHERE EXISTS (SELECT 1 FROM locations_projects lp WHERE lp.project_id = p.project_id);"),
    networks =  dbGetQueryDT(pool, "SELECT n.* FROM networks AS n WHERE EXISTS (SELECT 1 FROM locations_networks ln WHERE ln.network_id = n.network_id);"),
    has_images = dbGetQueryDT(pool, "SELECT DISTINCT location_id FROM images_index;"),
    has_documents = dbGetQueryDT(pool, "SELECT DISTINCT locations.location_id FROM locations JOIN documents_spatial ON locations.geom_id = documents_spatial.geom_id JOIN documents ON documents_spatial.document_id = documents.document_id;")
  )
  
  # Store information to pass between modules
  moduleOutputs <- reactiveValues()
  
  
  # Language selection ########################################################
  # Determine user's browser language. This should only run once when the app is loaded.
  observe({
    if (!isRestoring()) {
      shinyjs::runjs("var language =  window.navigator.userLanguage || window.navigator.language;
Shiny.onInputChange('userLang', language);
console.log(language);")
    }
  })
  
  # Some elements lack attributes that screen readers use to identify them. This adds an aria-label to the language selector.
  observe({
    shinyjs::runjs('$("#langSelect-selectized").attr("aria-label", "Language Selector");')
    shinyjs::runjs('$("#langSelect").attr("title", "Language Selector");')
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
  
  # Language selection reactive based on the user's selected language (which is automatically set to the browser's language on load)
  languageSelection <- reactiveValues()
  observe({
    languageSelection$language <- input$langSelect
    languageSelection$abbrev <- translations[id == "titleCase", get(input$langSelect)][[1]]
  })
  
  
  # In contrast to input$userLang, input$langSelect is created in the UI and is the language selected by the user.
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
  

  # Load specific modules based on input$navbar ################################
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
        observe({  # Observe the map_outputs reactive to see if the tab should be changed
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
        plot("plot", con = pool, language = languageSelection, restoring = isRestoring_plot, data = DBdata)
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
