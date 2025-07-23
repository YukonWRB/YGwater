#' YGwater user interface
#'
#' Constructs the navigation bar and main UI containers used by the
#' application. Called internally by [YGwater()].
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {
  
  tagList(
    shinyjs::useShinyjs(),
    div(id = "keep_alive", style = "display:none;", textOutput("keep_alive")), # Used for a heartbeat every 5 seconds to keep app alive, which occasionally gives issues on mobile devices.
    
    # Define a JavaScript function to change the background color of an element. If used within a module, MUST refer to variables with ns().
    # Uses two parameters: 'id' for the element ID and 'col' for the color. Color can be R-recognized color name or hex code.
    shinyjs::extendShinyjs(
      text = 'shinyjs.backgroundCol = function(params) {
      var defaultParams = {
        id : null,
        col : "red"
      };
      params = shinyjs.getParams(params, defaultParams);
      var el = $("#" + params.id);
                         el.css("background-color", params.col);
}', functions = c("backgroundCol")),
    
    tags$head(
      tags$script(src = "js/fullscreen.js"),  # JS to handle full screen button
      tags$script(src = "js/window_resize.js"),  # Include the JavaScript file to report screen dimensions, used for plot rendering and resizing
      # JS below is for updating the title of the page from the server, when the user changes language
      tags$script(HTML("
      Shiny.addCustomMessageHandler('updateTitle', function(newTitle) {
        document.title = newTitle;
      });
    ")),
      tags$script("Shiny.addCustomMessageHandler(
      'toggleDropdown',
          function toggleDropdown(msg) {
            $('.dropdown-menu').removeClass('show')
          });
        "),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/fonts.css"), # Fonts
      tags$link(rel = "stylesheet", type = "text/css", href = "css/top-bar.css"), # Top bar size, position, etc.
      tags$link(rel = "stylesheet", type = "text/css", href = "css/YG_bs5.css"), # CSS style sheet
      tags$link(rel = "stylesheet", type = "text/css", href = "css/buttons.css"), # styling for hover effects on buttons with YG colors
      # Below css prevents the little triangle (caret) for nav_menus from showing up on a new line when nav_menu text is rendered in the server
      tags$style(HTML("
        a.dropdown-toggle > .shiny-html-output {
        display: inline;
        }
      ")),
      tags$style(HTML("
    .alert { white-space: normal !important; }
  "))
    ),
    # page_fluid is the main container for the app, which contains the top bar, nav bar, content, and footer.
    page_fluid(
      # Make the container for the top bar, which sits above the nav bar
      div(
        class = "top-bar-container d-none d-md-block",
        fluidRow(
          column(3,
                 div(class = "logo",
                     htmltools::img(src = "imgs/Yukon_logo-min.png", .noWS = "outside", alt = "Yukon Government logo")
                     
                 ),
                 class = "logo-container"),
          column(9,
                 div(class = "aurora",
                     htmltools::img(src = "imgs/YG_Aurora_resized_flipped-min.png", .noWS = "outside", alt = "Aurora")
                 ),
                 div(class = "login-container",
                     if (!config$public) { # 'public' is a global variable established in the globals file
                       div(class = "login-btn-container",
                           actionButton("loginBtn", "Login", class = "btn btn-primary"),
                           actionButton("logoutBtn", "Logout", class = "btn btn-primary", style = "display: none;")) # Initially hidden
                     }
                 ),
                 class = "aurora-login-container")
        )
      ),
      # And now the navbar itself
      page_navbar(
        title = tags$a(
          class = "d-md-none",
          href = "#",
          tags$img(src = "imgs/Yukon_logo.png", style = "height: 50px; margin-right: 10px; margin-top: -15px;")
        ),
        id = "navbar",
        window_title = NULL,
        navbar_options = navbar_options(bg = "#244C5A",
                                        collapsible = TRUE),
        fluid = TRUE,
        lang = "en",
        theme = NULL, # Theme is set earlier by css file reference
        gap = "10px",
        nav_panel(title = uiOutput("homeNavTitle"), value = "home",
                  uiOutput("home_ui")),
        nav_menu(title = uiOutput("mapsNavMenuTitle"), value = "maps",
                 nav_panel(title = uiOutput("mapsNavLocsTitle"), value = "monitoringLocations",
                           uiOutput("mapLocs_ui")),
                 nav_panel(title = uiOutput("mapsNavParamsTitle"), value = "parameterValues",
                           uiOutput("mapParams_ui"))
        ),
        nav_menu(title = uiOutput("plotsNavMenuTitle"), value = "plot",
                 nav_panel(title = uiOutput("plotsNavDiscTitle"), value = "discrete",
                           uiOutput("plotDiscrete_ui")),
                 nav_panel(title = uiOutput("plotsNavContTitle"), value = "continuous",
                           uiOutput("plotContinuous_ui"))
                 # nav_panel(title = uiOutput("plotsNavMixTitle"), value = "mix",
                 # uiOutput("plotsMix_ui"))
        ),
        if (!config$public) {
          nav_menu(title = uiOutput("reportsNavMenuTitle"), value = "reports",
                   nav_panel(title = uiOutput("reportsNavSnowstatsTitle"), value = "snowInfo",
                             uiOutput("snowInfo_ui")),
                   nav_panel(title = uiOutput("reportsNavWaterTitle"), value = "waterInfo",
                             uiOutput("waterInfo_ui")),
                   nav_panel(title = uiOutput("reportsNavWQTitle"), value = "WQReport",
                             uiOutput("WQReport_ui")),
                   if (!config$public) {
                     nav_panel(title = uiOutput("reportsNavSnowbullTitle"), value = "snowBulletin",
                               uiOutput("snowBulletin_ui"))
                   }
          ) # End reports nav_menu
        }, # End if !config$public
        nav_menu(title = uiOutput("imagesNavMenuTitle"), value = "images",
                 nav_panel(title = uiOutput("imagesNavTableTitle"), value = "imgTableView",
                           uiOutput("imgTableView_ui")),
                 nav_panel(title = uiOutput("imagesNavMapTitle"), value = "imgMapView",
                           uiOutput("imgMapView_ui")),
        ),
        nav_menu(title = uiOutput("dataNavMenuTitle"), value = "data",
                 nav_panel(title = uiOutput("dataNavContTitle"), value = "contData",
                           uiOutput("contData_ui")),
                 nav_panel(title = uiOutput("dataNavDiscTitle"), value = "discData",
                           uiOutput("discData_ui"))
        ), # End data nav_menu
        if (!config$public & config$g_drive) { # if public or if g drive access is not possible, don't show the tab
          nav_panel(title = "FOD comments", value = "FOD",
                    uiOutput("fod_ui"))
        },
        nav_menu(title = uiOutput("infoNavMenuTitle"), value = "info",
                 nav_panel(title = uiOutput("infoNavNewsTitle"), value = "news",
                           uiOutput("news_ui")),
                 nav_panel(title = uiOutput("infoNavAboutTitle"), value = "about",
                           uiOutput("about_ui"))
        ),
        nav_panel(title = "Feedback", value = "feedback",
                  uiOutput("feedback_ui")),
        if (!config$public) {
          nav_menu(title = "Application tasks",
                   value = "appTasks",
                   nav_panel(title = "News page content",
                             value = "manageNewsContent",
                             uiOutput("manageNewsContent_ui")),
                   nav_panel(title = "View feedback",
                             value = "viewFeedback",
                             uiOutput("viewFeedback_ui"))
          )
        },
        if (!config$public) {
          nav_menu(title = "Continuous data", 
                   value = "continuousData",
                   nav_panel(title = "Add continuous data",
                             value = "addContData",
                             uiOutput("addContData_ui")),
                   nav_panel(title = "Edit/delete continuous data",
                             value = "editContData",
                             uiOutput("editContcData_ui")),
                   nav_panel(title = "Add/modify timeseries corrections",
                             value = "continuousCorrections",
                             uiOutput("continuousCorrections_ui")),
                   nav_panel(title = "Impute missing values",
                             value = "imputeMissing",
                             uiOutput("imputeMissing_ui")),
                   nav_panel(title = "Apply grades, approvals, qualifiers",
                             value = "grades_approvals_qualifiers",
                             uiOutput("grades_approvals_qualifiers_ui")),
                   nav_panel(title = "Sync timeseries",
                             value = "syncCont",
                             uiOutput("syncCont_ui")),
                   nav_panel(title = "Add/edit timeseries",
                             value = "addTimeseries",
                             uiOutput("addTimeseries_ui"))
          )
        },
        if (!config$public) {
          nav_menu(title = "Discrete data", 
                   value = "discreteData",
                   nav_panel(title = "Add discrete data",
                             value = "addDiscData",
                             uiOutput("addDiscData_ui")),
                   nav_panel(title = "Edit/delete discrete data",
                             value = "editDiscData",
                             uiOutput("editDiscData_ui")),
                   nav_panel(title = "Sync sample series",
                             value = "syncDisc",
                             uiOutput("syncDisc_ui"))
          )
        },
        if (!config$public) {
          nav_menu(title = "Location tasks",
                   value = "dbAdmin",
                   nav_panel(title = "Add/modify locations",
                             value = "addLocation",
                             uiOutput("addLocation_ui")
                   ),
                   nav_panel(title = "Add/modify sub-locations",
                             value = "addSubLocation",
                             uiOutput("addSubLocation_ui")
                   )
          )
        },
        if (!config$public) {
          nav_menu(title = "Manage files/docs", 
                   value = "addFiles",
                   nav_panel(title = "Documents",
                             value = "addDocs",
                             uiOutput("addDocs_ui")),
                   nav_panel(title = "Images",
                             value = "addImgs",
                             uiOutput("addImgs_ui"))
          )
        },
        if (!config$public) {
          nav_panel(title = "Add/modify field visit",
                    value = "visit",
                    uiOutput("visit_ui"))
        }, 
        if (!config$public) {
          nav_menu(title = "Equipment/instruments",
                   value = "equip",
                   nav_panel(title = "Checks + calibrations",
                             value = "cal",
                             uiOutput("cal_ui")),
                   nav_panel(title = "Deploy/Recover", 
                             value = "deploy_recover",
                             uiOutput("deploy_recover_ui"))
          )
        }
      ), # End page_navbar (though it's modified below to add a language selector)
      
      # Now a footer, rendered in the server for language support
      div(
        hr(),
        uiOutput("footer_ui"),
      )
    ), # End of page_fluid
    
    # Insert language selector into the navbar
    HTML("<script>
  document.addEventListener('DOMContentLoaded', function() {
    var parent = document.getElementsByClassName('navbar-nav');
    
    if (parent.length > 0) {
      parent[0].insertAdjacentHTML('afterend', 
        '<ul class=\"navbar-nav ms-auto\">' +
        '<li class=\"nav-item dropdown\">' +
        '<a href=\"#\" class=\"nav-link dropdown-toggle\" data-bs-toggle=\"dropdown\" role=\"button\" aria-expanded=\"false\">' +
        'Language</a>' +
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
</script>")
  ) # End tagList
  
}
