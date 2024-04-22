# StreamLine application main UI

ui <- function(request) {
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/fonts.css"), # Fonts
      tags$link(rel = "stylesheet", type = "text/css", href = "css/top-bar.css"), # Top bar
      tags$link(rel = "stylesheet", type = "text/css", href = "css/feedback-btn.css"), # Feedback button
      tags$script(src = "js/mailto-update.js"), # Mailto language update script
      tags$script(src = "js/lang-update.js") # Language update script"))
    ),
    
    # Create language selector and logo positioned above navbarPage
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
               collapsible = TRUE,
               fluid = TRUE,
               lang = "en",
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
               tabPanel(title = uiOutput("about_title"), value = "about",
                        aboutUI("about"))
    ),
    tags$a(id = "feedback_btn", 
           href = "mailto:waterlevels@yukon.ca?subject=Placeholder&body=Placeholder", 
           icon("envelope"), 
           class = "feedback-btn", 
           target = "_blank")
    # Make a custom JS handler to update the email text (called from the server upon lang change)
  ) # End fluidPage
}

