server <- function(input, output, session) {
  
  # Initial setup #
  # Automatically update URL every time an input changes
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  # Update the query string
  onBookmarked(updateQueryString)
  
  # Language selection ########################################################
  
  # Determine user's browser language. This should only run once when the app is loaded.
  shinyjs::runjs("var language =  window.navigator.userLanguage || window.navigator.language;
Shiny.onInputChange('userLang', language);
console.log(language);")
  
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
  
  languageSelection <- reactive({
    input$langSelect
  })
  
  observeEvent(input$langSelect, {
    newLang <- input$langSelect
    session$sendCustomMessage(type = 'updateLang', message = list(lang = ifelse(newLang == "English", "en", "fr")))  # Updates the language in the web page html head.
    output$home_title <- renderText({
     translations[translations$id == "home", ..newLang][[1]]
    })
    output$map_title <- renderText({
      translations[translations$id == "map_view_title", ..newLang][[1]]
    })
    output$data_title <- renderText({
     translations[translations$id == "data_view_title", ..newLang][[1]]
    })
  })
  
  
  # Map View Module ###########################################################
  map("map", con = pool, language = languageSelection)
  
  # Data view module ##########################################################
  data("data", con = pool, language = languageSelection)
}
