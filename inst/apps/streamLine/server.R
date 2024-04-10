server <- function(input, output, session) {
  
  # Initial setup #
  # Automatically update URL every time an input changes
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  setBookmarkExclude("userLang")
  
  # Update the query string
  onBookmarked(updateQueryString)
  
  isRestoring <- reactiveVal(FALSE)
  onRestore(function(state) {
    isRestoring(TRUE)
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
     translations[translations$id == "home", ..newLang][[1]]
    })
    output$map_title <- renderText({
      translations[translations$id == "map_view_title", ..newLang][[1]]
    })
    output$data_title <- renderText({
     translations[translations$id == "data_view_title", ..newLang][[1]]
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
  
  # Home View Module ##########################################################
  home("home", language = languageSelection)
  
  # Map View Module ###########################################################
  map("map", con = pool, language = languageSelection)
  
  # Data view module ##########################################################
  data("data", con = pool, language = languageSelection)
}
