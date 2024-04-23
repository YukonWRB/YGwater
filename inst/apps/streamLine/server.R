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
  
  # In contrast to input$userLang, input$langSelect is created in the UI and is the language selected by the user.
  observeEvent(input$langSelect, {
    newLang <- input$langSelect
    session$sendCustomMessage(type = 'updateLang', message = list(lang = ifelse(newLang == "Français", "fr", "en")))  # Updates the language in the web page html head.
    output$home_title <- renderText({
      HTML(paste0('<div class="nunito-sans" style="font-size: 17px; font-weight: 500; font-style: normal;">',
                  translations[translations$id == "home", ..newLang][[1]],
                  '</div>'))
    })
    output$map_title <- renderText({
      HTML(paste0('<div class="nunito-sans" style="font-size: 17px; font-weight: 500; font-style: normal;">',
                  translations[translations$id == "map_view_title", ..newLang][[1]],
                  '</div>'))
    })
    output$data_title <- renderText({
      HTML(paste0('<div class="nunito-sans" style="font-size: 17px; font-weight: 500; font-style: normal;">',
                  translations[translations$id == "data_view_title", ..newLang][[1]],
                  '</div>'))
    })
    output$plot_title <- renderText({
      HTML(paste0('<div class="nunito-sans" style="font-size: 17px; font-weight: 500; font-style: normal;">',
                  translations[translations$id == "plot_view_title", ..newLang][[1]],
                  '</div>'))
    })
    output$img_title <- renderText({
      HTML(paste0('<div class="nunito-sans" style="font-size: 17px; font-weight: 500; font-style: normal;">',
                  translations[translations$id == "img_view_title", ..newLang][[1]],
                  '</div>'))
    })
    output$doc_title <- renderText({
      HTML(paste0('<div class="nunito-sans" style="font-size: 17px; font-weight: 500; font-style: normal;">',
                  translations[translations$id == "doc_view_title", ..newLang][[1]],
                  '</div>'))    
    })
    output$about_title <- renderText({
      HTML(paste0('<div class="nunito-sans" style="font-size: 17px; font-weight: 500; font-style: normal;">',
                  translations[translations$id == "about_view_title", ..newLang][[1]],
                  '</div>'))
    })
    
    # Update the mailto link with the correct language
    subject <- translations[translations$id == "feedback", ..newLang][[1]]
    body <- translations[translations$id == "feedback_text", ..newLang][[1]]
    mailtoLink <- sprintf("mailto:waterlevels@yukon.ca?subject=%s&body=%s", URLencode(subject), URLencode(body))
    session$sendCustomMessage("updateMailtoLink", mailtoLink) #Update using custom JS handler defined in the UI
  })
  
  # Language selection reactive based on the user's selected language (which is automatically set to the browser's language on load)
  languageSelection <- reactive({
    input$langSelect
  })
  
  # Load specific modules based on input$navbar ################################
  lastWorkingTab <- reactiveVal("home")  # Initial or safe tab
  observeEvent(input$navbar, {
    newLang <- input$langSelect
    if (input$navbar == "home") {
      tryCatch({
        home("home", language = languageSelection, restoring = isRestoring_home)
      }, error = function(e) {
        showModal(modalDialog(
          title = translations[translations$id == "errorModalTitle", ..newLang][[1]],
          translations[translations$id == "errorModalMsg", ..newLang][[1]],
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        # Reset to last good tab
        updateNavbarPage(session, "navbar", selected = lastWorkingTab())
      })
    }
    if (input$navbar == "map") {
      tryCatch({
        map_outputs <- map("map", con = pool, language = languageSelection, restoring = isRestoring_map)
        observe({
          if (!is.null(map_outputs$change_tab)) {
            updateNavbarPage(session, "navbar", selected = map_outputs$change_tab)
          }
        })
      }, error = function(e) {
        showModal(modalDialog(
          title = translations[translations$id == "errorModalTitle", ..newLang][[1]],
          translations[translations$id == "errorModalMsg", ..newLang][[1]],
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        updateNavbarPage(session, "navbar", selected = lastWorkingTab())
      })
    }
    if (input$navbar == "data") {
      tryCatch({
        data("data", con = pool, language = languageSelection, restoring = isRestoring_data)      
      }, error = function(e) {
        showModal(modalDialog(
          title = translations[translations$id == "errorModalTitle", ..newLang][[1]],
          translations[translations$id == "errorModalMsg", ..newLang][[1]],
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        updateNavbarPage(session, "navbar", selected = lastWorkingTab())
      })
    }
    if (input$navbar == "plot") {
      tryCatch({
        plot("plot", con = pool, language = languageSelection, restoring = isRestoring_plot)
      }, error = function(e) {
        showModal(modalDialog(
          title = translations[translations$id == "errorModalTitle", ..newLang][[1]],
          translations[translations$id == "errorModalMsg", ..newLang][[1]],
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        updateNavbarPage(session, "navbar", selected = lastWorkingTab())
      })
    }
    if (input$navbar == "img") {
      tryCatch({
        img("img", con = pool, language = languageSelection, restoring = isRestoring_img)
      }, error = function(e) {
        showModal(modalDialog(
          title = translations[translations$id == "errorModalTitle", ..newLang][[1]],
          translations[translations$id == "errorModalMsg", ..newLang][[1]],
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        updateNavbarPage(session, "navbar", selected = lastWorkingTab())
      })
    }
    if (input$navbar == "doc") {
      tryCatch({
        doc("doc", con = pool, language = languageSelection, restoring = isRestoring_doc)
      }, error = function(e) {
        showModal(modalDialog(
          title = translations[translations$id == "errorModalTitle", ..newLang][[1]],
          translations[translations$id == "errorModalMsg", ..newLang][[1]],
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
          title = translations[translations$id == "errorModalTitle", ..newLang][[1]],
          translations[translations$id == "errorModalMsg", ..newLang][[1]],
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        updateNavbarPage(session, "navbar", selected = lastWorkingTab())
      })
    }
    # Update last working tab on successful tab switch
    lastWorkingTab(input$navbar)
  }, ignoreNULL = TRUE)
  
}
