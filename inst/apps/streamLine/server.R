
# Establish database connection
pool <- pool::dbPool(
  drv = RPostgres::Postgres(),
  dbname = "hydromet",
  host = Sys.getenv("hydrometHost"),
  port = Sys.getenv("hydrometPort"),
  user = Sys.getenv("hydrometUser"),
  password = Sys.getenv("hydrometPass"))

server <- function(input, output, session) {
  
  # Language selection ########################################################
  
  # Determine user's browser language
  shinyjs::runjs("var language =  window.navigator.userLanguage || window.navigator.language;
Shiny.onInputChange('user_lang', language);
console.log(language);")
  
  # Check if lang_check contains en or fr in the string and set the language accordingly
  observeEvent(input$user_lang, {
    if (substr(input$user_lang , 1, 2) == "en") {
      updateSelectInput(session, "languageSelect", selected = "English")
    } else if (substr(input$user_lang , 1, 2) == "fr") {
      updateSelectInput(session, "languageSelect", selected = "Français")
    } else {
      updateSelectInput(session, "languageSelect", selected = "English")
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$languageSelect, {
    newLang <- input$languageSelect
    output$home_title <- renderText({
     as.character(translations[translations$id == "home", ..newLang])
    })
    output$map_title <- renderText({
      as.character(translations[translations$id == "map_view_title", ..newLang])
    })
    output$settings_title <- renderText({
      as.character(translations[translations$id == "settings", ..newLang])
    })
  })
  
  # Map View Module ###########################################################
  mapView("map_view", con = pool, translations = translations)
  
  # Download Data Module
  # downloadData("download_data")
  
  # Include other modules as needed
}
