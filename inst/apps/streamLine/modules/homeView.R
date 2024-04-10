
homeUI <- function(id) {
  ns <- NS(id)
  verticalLayout(
    htmlOutput(ns("title")),
    tags$div(style = "height: 10px;"),
    htmlOutput(ns("text")),
    tags$div(style = "height: 60px;"),
    htmlOutput(ns("discTitle")),
    tags$div(style = "height: 10px;"),
    htmlOutput(ns("discBody"))
  )
  
}

home <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    observe({
      lang <- language()
      abbrev <- translations[translations$id == "titleCase", ..lang][[1]]
      
      output$title <- renderUI({
        HTML(paste0('<div class="montserrat" style="font-size: 24px; font-weight: 600; font-style: normal">',
                    titleCase(translations[translations$id == "homeTitle", ..lang][[1]], abbrev),
                    '</div>'
        ))
      })
      
      output$text <- renderUI({
        HTML(paste0('<div class="nunito-sans" style="font-size: 16px; font-weight: 500; font-style: normal;">',
                    translations[translations$id == "homeText", ..lang][[1]],
                    '</div>'
        ))
      })
      output$discTitle <- renderUI({
        HTML(paste0('<div class="montserrat" style="font-size: 20px; font-weight: 600; font-style: normal">',
                    titleCase(translations[translations$id == "disclaimer_title", ..lang][[1]], abbrev),
                    '</div>'
        ))
      })
      output$discBody <- renderUI({
        HTML(paste0('<div class="nunito-sans" style="font-size: 14px; font-weight: 500; font-style: normal">',
                    translations[translations$id == "disclaimer_body", ..lang][[1]],
                    '</div>'
        ))
      })
    })
  })
}
