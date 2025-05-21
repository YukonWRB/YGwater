
homeUI <- function(id) {
  ns <- NS(id)
  verticalLayout(
    htmlOutput(ns("betaTitle")),
    tags$div(style = "height: 10px;"),
    htmlOutput(ns("betaText")),
    tags$div(style = "height: 60px;"),
    htmlOutput(ns("title")),
    tags$div(style = "height: 10px;"),
    htmlOutput(ns("text")),
    tags$div(style = "height: 40px;"),
    htmlOutput(ns("discTitle")),
    tags$div(style = "height: 10px;"),
    htmlOutput(ns("discBody"))
  )
}

home <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    observe({
      req(language$language)
      output$betaTitle <- renderUI({
        HTML(paste0('<div class="montserrat" style="font-size: 24px; font-weight: 600; font-style: normal">',
                    tr("betaTitle", language$language),
                    '</div>'
        ))
      })
      output$betaText <- renderUI({
        HTML(paste0('<div class="nunito-sans" style="font-size: 16px; font-weight: 500; font-style: normal;">',
                    tr("betaText", language$language),
                    '</div>'
        ))
      })
      output$title <- renderUI({
        HTML(paste0('<div class="montserrat" style="font-size: 24px; font-weight: 600; font-style: normal">',
                    tr("homeTitle", language$language),
                    '</div>'
        ))
      })
      output$text <- renderUI({
        HTML(paste0('<div class="nunito-sans" style="font-size: 16px; font-weight: 500; font-style: normal;">',
                    tr("homeText", language$language),
                    '</div>'
        ))
      })
      output$discTitle <- renderUI({
        HTML(paste0('<div class="montserrat" style="font-size: 20px; font-weight: 600; font-style: normal">',
                    tr("disclaimer_title", language$language),
                    '</div>'
        ))
      })
      output$discBody <- renderUI({
        HTML(paste0('<div class="nunito-sans" style="font-size: 14px; font-weight: 500; font-style: normal">',
                    tr("disclaimer_body", language$language),
                    '</div>'
        ))
      })
    })
  })
}
