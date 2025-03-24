#' The YGwater User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {
  
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
      tags$script(src = "js/fullscreen.js"),  # Include the JavaScript file for full screen button
      # The Javascript below contains functions to detect window size and continuously send these dimensions to the server (debounced) as variable input$window_dimensions. There's also a function that allows tracking container sizes: trackContainerSize(containerId, shinyInputId)  continerId = the id of the container to track, shinyInputId = the id of the input to send the size to (will create or update if existing)
      tags$script(src = "js/window_resize.js"),  # Include the JavaScript file to report screen dimensions
      tags$script(HTML("
      Shiny.addCustomMessageHandler('updateTitle', function(newTitle) {
        document.title = newTitle;
      });
    ")),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/fonts.css") # Fonts
      # css for z index control is at the bottom because it must come after the css passed on silently by navbarPage
    ),
    # Add custom CSS to adjust tab font sizes: 'admin' and 'visualize' tabs are larger (these are created in server)
    tags$style(HTML("
        /* Reduce font size for all tabs except 'visualize' and 'admin' */
        .navbar .nav > li > a:not([data-value='visualize']):not([data-value='admin']) {
          font-size: 14px; /* Adjust the font size as needed */
        }
      ")),
    fluidRow(class = "top-bar-container hidden-xs",
             column(3,
                    div(class = "logo",
                        htmltools::img(src = "imgs/Yukon_logo.png", .noWS = "outside", alt = "Yukon Government logo")
                        
                    ),
                    class = "logo-container"),
             column(9,
                    div(class = "aurora",
                        htmltools::img(src = "imgs/YG_Aurora_resized_flipped.png", .noWS = "outside", alt = "Aurora")
                    ),
                    div(class = "login-container",
                        if (!config$public) { # 'public' is a global variable established in the globals file
                          div(class = "login-btn-container",
                              actionButton("loginBtn", "Login", class = "btn btn-primary"),
                              actionButton("logoutBtn", "Logout", class = "btn btn-primary", style = "display: none;")) # Initially hidden
                        }
                    ),
                    class = "aurora-login-container")
    ),
    navbarPage(title = tags$a(
      class = "visible-xs", href = "#",
      tags$img(src = "imgs/Yukon_logo.png", style = "height: 50px; margin-right: 10px; margin-top: -15px;")
    ),
    id = "navbar",
    theme = "css/bootstrap3.css",  # Note that this uses the 'bootstrap3' theme because of compatibility issues. The file 'bootstrap5.css' could be used if/when navbarPage is updated to use BS5. Also the default dark blue color has been changed to YG-specific colors.
    windowTitle = NULL,
    collapsible = TRUE,
    fluid = TRUE,
    lang = "en",
    # tabPanel(title = "Switch to View mode", value = "viz"),
    tabPanel(title = "Home", value = "home",
             uiOutput("home_ui")),
    tabPanel(title = "Map", value = "map",
             uiOutput("map_ui")),
    navbarMenu(title = "Plot", menuName = "plot",
               tabPanel(title = "Discrete", value = "discrete",
                        uiOutput("discrete_ui")),
               tabPanel(title = "Continuous", value = "continuous",
                        uiOutput("continuous_ui")),
               tabPanel(title = "Mix", value = "mix",
                        uiOutput("mix_ui"))
    ),
    navbarMenu(title = "Reports", menuName = "reports",
               tabPanel(title = "Snowpack info", value = "snowInfo",
                        uiOutput("snowInfo_ui")),
               tabPanel(title = "Water level/flow info", value = "waterInfo",
                        uiOutput("waterInfo_ui")),
               tabPanel(title = "Water quality", value = "WQReport",
                        uiOutput("WQReport_ui")),
               if (!config$public) {
                 tabPanel(title = "Snow Bulletin + stats", value = "snowBulletin",
                          uiOutput("snowBulletin_ui"))
               }
    ), # End reports navbarMenu
    tabPanel(title = "Images", value = "img",
             uiOutput("img_ui")),
    navbarMenu(title = "Data", menuName = "data",
               tabPanel(title = "Continuous data", value = "contData",
                        uiOutput("contData_ui")),
               tabPanel(title = "Discrete (lab/field) data", value = "discData",
                        uiOutput("discData_ui"))
    ), # End data navbarMenu
    if (!config$public & config$g_drive) { # if public or if g drive access is not possible, don't show the tab
      tabPanel(title = "FOD comments", value = "FOD",
               uiOutput("fod_ui"))
    },
    navbarMenu(title = "Info", menuName = "info",
               tabPanel("News", value = "news",
                        uiOutput("news_ui")),
               tabPanel("About", value = "about",
                        uiOutput("about_ui"))
    ),
    ), # End navbarPage (though it's modified below)
    
    # Insert language selector into the navbar
    # Proof of concept that inserts a "v0.1" version number in the navbar
    #     HTML("<script>var parent = document.getElementsByClassName('navbar-nav');
    # parent[0].insertAdjacentHTML( 'afterend', '<ul class=\"nav navbar-nav navbar-right\"><li class=\"disabled\"><a href=\"#\">v0.1</a></li></ul>' );</script>"),
    HTML("<script>
  document.addEventListener('DOMContentLoaded', function() {
    var parent = document.getElementsByClassName('navbar-nav');
    
    if (parent.length > 0) {
      parent[0].insertAdjacentHTML('afterend', 
        '<ul class=\"nav navbar-nav navbar-right\">' +
        '<li class=\"dropdown\">' +
        '<a href=\"#\" class=\"dropdown-toggle\" data-toggle=\"dropdown\" role=\"button\" aria-haspopup=\"true\" aria-expanded=\"false\">' +
        'Language <span class=\"caret\"></span></a>' +
        '<ul class=\"dropdown-menu\" id=\"lang-dropdown\">' +
        '</ul>' +
        '</li>' +
        '</ul>'
      );
    }

    // Define the function globally so it can be used in the onclick event
    window.setLang = function(lang) {
      Shiny.setInputValue(\"langSelect\", lang, {priority: \"event\"});
      
      // Remove active class from all menu items
      var items = document.querySelectorAll('#lang-dropdown li');
      items.forEach(function(item) {
        item.classList.remove('active');
      });
    
      // Find and highlight the selected language
      var links = document.querySelectorAll('#lang-dropdown a');
      links.forEach(function(link) {
        if (link.textContent.trim() === lang) {
          link.parentElement.classList.add('active');
        }
      });
    };

    // Function to dynamically update the dropdown menu from Shiny
    Shiny.addCustomMessageHandler(\"updateLangMenu\", function(langs) {
      var dropdown = document.getElementById('lang-dropdown');
      dropdown.innerHTML = '';  // Clear existing options
      langs.forEach(function(lang) {
        var li = document.createElement('li');
        var a = document.createElement('a');
        a.href = '#';
        a.textContent = lang;
        a.setAttribute('onclick', \"setLang('\" + lang + \"')\");  // Attach function
        li.appendChild(a);
        dropdown.appendChild(li);
      });
    });
    
        // Function to set the initial selected language based on the user's browser language
    Shiny.addCustomMessageHandler('setSelectedLanguage', function(lang) {
      console.log('Setting initial language: ' + lang);
      Shiny.setInputValue('langSelect', lang, {priority: 'event'});
    });
  });
</script>"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/top-bar.css"), # Top bar size, position, etc - comes after the navbarPage because otherwise the navbarPage css overrides it
    )
  ) # End fluidPage
}
