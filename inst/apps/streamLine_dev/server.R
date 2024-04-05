
# Establish database connection
# pool <- pool::dbPool(
#   drv = RPostgres::Postgres(),
#   dbname = "hydromet",
#   host = Sys.getenv("hydrometHost"),
#   port = Sys.getenv("hydrometPort"),
#   user = Sys.getenv("hydrometUser"),
#   password = Sys.getenv("hydrometPass"))

server <- function(input, output, session) {
  
  # Language selection ########################################################
  # Determine user's browser language
  lang_check <- "var language =  window.navigator.userLanguage || window.navigator.language;
Shiny.onInputChange('user_lang', language);
console.log(language);"
  shinyjs::runjs(lang_check)
  # Check if lang_check contains en or fr in the string and set the language accordingly
  observeEvent(input$user_lang, {
    if (substr(input$user_lang , 1, 2) == "en") {
      updateSelectInput(session, "languageSelect", selected = "English")
    } else {
      updateSelectInput(session, "languageSelect", selected = "Français")
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
  
  observeEvent(input$languageToggle, {
    newLang <- input$languageToggle
    output$home_title <- renderText({
     as.character(translations[translations$English == "Home", ..newLang])
    })
    output$map_title <- renderText({
      as.character(translations[translations$English == "Map View", ..newLang])
    })
  }, ignoreInit = TRUE)
  
  # Map View Module ###########################################################
  mapView("map_view", con = pool)
  
  # Download Data Module
  # downloadData("download_data")
  
  # Include other modules as needed
}
