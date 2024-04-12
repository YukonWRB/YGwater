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
    /* Custom styling for positioning and z-index of top bar containers */
    .top-bar-container {
      margin-bottom: 3px; /* Adjust to reduce/increase space above the navbar */
      position: relative; /* Ensure container is positioned relative for absolute child positioning */
    }
    .logo-container img {
      height: 100px; /* Increase height as needed */
      width: auto; /* Maintain aspect ratio */
      margin-top: -22px; /* Adjust if necessary */
      position: absolute; /* Position logo absolutely within its container */
      top: 0; /* Align top edge with container */
      left: 0px; /* Maintain some spacing from the screen edge */
    }
    /* Adjust the position and priority of the language selector */
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
    
    
    /* Custom font loading */
    @font-face {
      font-family: 'Montserrat';
      src: url('www/fonts/Montserrat/Montserrat-VariableFont_wght.woff2') format('woff2-variations');
      font-weight: 100 900;
      font-style: normal;
    }
    @font-face {
      font-family: 'Montserrat';
      src: url('www/fonts/Montserrat/Montserrat-Italic-VariableFont_wght.woff2') format('woff2-variations');
      font-weight: 100 900;
      font-style: italic;
    }
    @font-face {
      font-family: 'Nunito Sans';
      src: url('www/fonts/NunitoSans/NunitoSans-Variable_YTLC.woff2') format('woff2-variations');
      font-weight: 100 900;
      font-style: normal;
    }
    @font-face {
      font-family: 'Nunito Sans';
      src: url('www/fonts/NunitoSans/NunitoSans-Italic-Variable_YTLC.woff2') format('woff2-variations');
      font-weight: 100 900;
      font-style: italic;
    }
    .montserrat {
      font-family: 'Montserrat', sans-serif;
    }
    .montserrat-italic {
      font-family: 'Montserrat', sans-serif;
      font-style: italic;
    }
    .nunito-sans {
      font-family: 'Nunito Sans', sans-serif;
    }
    .nunito-sans {
      font-family: 'Nunito Sans', sans-serif;
      font-style: italic;
    }
  "))
    ),
    
    # Language selector and logo positioned above navbarPage
    fluidRow(class = "top-bar-container",
              column(2,
                     div(class = "logo-container",
                         htmltools::img(src = "imgs/YG_Aurora_resized.png", .noWS = "outside", alt = "Aurora")),
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
               tabPanel(title = uiOutput("home_title"), value = "home",
                        homeUI("home")),
               tabPanel(title = uiOutput("map_title"), value = "map", 
                        mapUI("map")),
               tabPanel(title = uiOutput("data_title"), value = "data",
                        dataUI("data")),
               tabPanel(title = uiOutput("plot_title"), value = "plot",
                        plotUI("plot")),
               tabPanel(title = uiOutput("img_title"), value = "img",
                        imgUI("img")),
               tabPanel(title = uiOutput("doc_title"), value = "doc",
                        docUI("doc")),
               
    ),
    tags$a(id = "feedback_btn", href = "mailto:waterlevels@yukon.ca?subject=Placeholder&body=Placeholder", 
           icon("envelope"), class = "feedback-btn", target = "_blank"),
    # Make a custom JS handler to update the email text (called from the server upon lang change)
    tags$script(' 
  Shiny.addCustomMessageHandler("updateMailtoLink", function(message) {
    document.getElementById("feedback_btn").href = message;
  });
'),
    tags$style(HTML("
    #feedback_btn {
      position: fixed;
      right: 7px;
      bottom: 7px;
      z-index: 1000;
      padding: 10px;
      background-color: #007bff; /* Bootstrap primary color */
      color: white; /* Text color */
      border-radius: 5px; /* Rounded corners */
      text-decoration: none; /* No underline */
    }
    #feedback_btn:hover {
      background-color: #0056b3; /* Darker on hover */
    }"
    ))
  )
}
