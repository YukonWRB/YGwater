ui <- function(request) {
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
      # HTML("<html lang='en'>"),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('updateLang', function(message) {
          $('html').attr('lang', message.lang);
        });
      ")),
      tags$style(HTML("
    /* Custom styling for positioning and z-index */
    .top-bar-container {
      margin-bottom: 3px; /* Adjust to reduce/increase space above the navbar */
      position: relative; /* Ensure container is positioned relative for absolute child positioning */
    }
    .logo-container img {
      height: 100px; /* Increase height as needed */
      width: auto; /* Maintain aspect ratio */
      margin-top: -15px; /* Adjust if necessary */
      position: absolute; /* Position logo absolutely within its container */
      top: 0; /* Align top edge with container */
      left: 0px; /* Maintain some spacing from the screen edge */
    }
    .language-select-container {
      text-align: left;
      margin-top: 10px;
    }
    .selectize-control, .selectize-dropdown {
      width: auto !important; /* Auto-adjust the dropdown width */
    }
    .selectize-dropdown {
      z-index: 1050 !important; /* Ensure the dropdown overlays other content */
    }
  "))
    ),
    
    # Language selector positioned above navbarPage but styled to appear integrated
    fluidRow( class = "top-bar-container",
              column(2,
                     div(class = "logo-container",
                         # Replace the src attribute with the path to your logo image
                         img(src = "imgs/YG_Aurora_resized.png", .noWS = "outside", alt = "Your Logo")),
                     class = "left-aligned-logo"),
              column(2, offset = 9, 
                     div(class = "lang-select-container",
                         selectizeInput(inputId = "langSelect", label = NULL, 
                                     choices = names(translations)[-c(1,2)], 
                                     selected = "English")),
                     class = "right-aligned-lang-select")
    ),
    navbarPage("StreamLine",
               id = "navbar",
               theme = shinythemes::shinytheme("flatly"), # Optional theme
               windowTitle = "StreamLine",
               tabPanel(title = uiOutput("home_title"), value = "HomeView"),
               tabPanel(title = uiOutput("map_title"), value = "map", 
                        mapUI("map")),
               tabPanel(title = uiOutput("data_title"), value = "data",
                        dataUI("data"))
               
    )
  )
}
