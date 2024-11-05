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
    # Add custom CSS to adjust tab font sizes
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
               tabPanel(title = "Viewer Mode", value = "visualize",
                        visualizeUI("visualize")),
               tabPanel(title = "Admin Mode", value = "admin",
                        adminUI("admin")),
               tabPanel(title = "Plot", value = "plot", 
                        plotUI("plot")),
               tabPanel(title = "Map", value = "map",
                        mapUI("map")),
               tabPanel(title = "FOD Comments", value = "FOD",
                        FODUI("FOD")),
               tabPanel(title = "Generate", value = "generate",
                        generateUI("generate")),
               tabPanel(title = "View/edit metadata", value = "metadata",
                        metadataUI("metadata")),
               tabPanel(title = "Add location/timeseries", value = "new_ts_loc",
                        new_ts_locUI("new_ts_loc")),
               tabPanel(title = "Create basins", value = "basins",
                        basinsUI("basins"))
    ), # End navbarPage
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/top-bar.css"), # Top bar size, position, etc
    )
  ) # End fluidPage
}
