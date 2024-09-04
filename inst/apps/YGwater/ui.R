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
      tags$link(rel = "stylesheet", type = "text/css", href = "css/fonts.css"), # Fonts
      # css for z index control is at the bottom because it must come after the css passed on silently by navbarPage
    ),
    fluidRow(class = "top-bar-container",
             column(3,
                    div(class = "logo-container",
                        htmltools::img(src = "imgs/Yukon_logo.png", .noWS = "outside", alt = "YG logo")),
                    class = "left-aligned-logo"),
             column(9,
                    div(class = "aurora-container",
                        htmltools::img(src = "imgs/YG_Aurora_resized_flipped.png", .noWS = "outside", alt = "Aurora")),
                    class = "right-aligned-aurora")
    ),
    navbarPage("YGwater Dev App",
               id = "navbar",
               theme = shinythemes::shinytheme("flatly"), # Optional theme
               windowTitle = "YGwater Dev App",
               collapsible = TRUE,
               fluid = TRUE,
               lang = "en",
               tabPanel(title = "Plot", value = "plot", 
                        plotUI("plot")),
               tabPanel(title = "Map", value = "map",
                        mapUI("map")),
               tabPanel(title = "FOD Comments", value = "FOD",
                        FODUI("FOD")),
               tabPanel(title = "Create Basins", value = "basins",
                        basinsUI("basins"))
    ), # End navbarPage
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/top-bar.css"), # Top bar size, position, etc
    )
  ) # End fluidPage
}
