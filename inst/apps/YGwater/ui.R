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
      tags$link(rel = "stylesheet", type = "text/css", href = "css/fonts.css") # Fonts
      # css for z index control is at the bottom because it must come after the css passed on silently by navbarPage
    ),
    # Add custom CSS to adjust tab font sizes: 'admin' and 'visualize' tabs are larger
    tags$style(HTML("
        /* Reduce font size for all tabs except 'visualize' and 'admin' */
        .navbar .nav > li > a:not([data-value='visualize']):not([data-value='admin']) {
          font-size: 14px; /* Adjust the font size as needed */
        }
      ")),
    fluidRow(class = "top-bar-container",
             column(3,
                    div(class = "logo-container",
                        htmltools::img(src = "imgs/Yukon_logo.png", .noWS = "outside", alt = "YG logo")),
                    class = "left-aligned-logo"),
             column(9,
                    div(class = "aurora-container",
                        htmltools::img(src = "imgs/YG_Aurora_resized_flipped.png", .noWS = "outside", alt = "Aurora")),
                    div(class = "lang-login-container",
                        div(class = "language-select-container",
                            selectizeInput(inputId = "langSelect", label = NULL, 
                                           choices = names(translations)[-c(1,2)], 
                                           selected = "English")),
                        div(class = "login-btn-container",
                            actionButton("loginBtn", "Login", class = "btn btn-primary"),
                            actionButton("logoutBtn", "Logout", class = "btn btn-primary", style = "display: none;")) # Initially hidden
                    ),
                    class = "right-aligned-aurora")
    ),
    navbarPage(NULL,
               id = "navbar",
               theme = shinythemes::shinytheme("flatly"), # Optional theme
               windowTitle = "YGwater Dev App",
               collapsible = TRUE,
               fluid = TRUE,
               lang = "en",
               tabPanel(title = "Switch to View mode", value = "viz"),
               tabPanel(title = "Map", value = "map",
                        uiOutput("map_ui")),
               tabPanel(title = "Plot", value = "plot", 
                        uiOutput("plot_ui")),
               tabPanel(title = "FOD comments", value = "FOD",
                        uiOutput("fod_ui")),
               tabPanel(title = "Generate", value = "gen",
                        uiOutput("gen_ui")),
               tabPanel(title = "View images", value = "img",
                        uiOutput("img_ui")),
    ), # End navbarPage
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/top-bar.css"), # Top bar size, position, etc
    )
  ) # End fluidPage
}
