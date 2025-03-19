aboutUI <- function(id) {
  ns <- NS(id)
  verticalLayout(
    htmlOutput(ns("title_about")),
    tags$div(style = "height: 10px;"),
    htmlOutput(ns("content_about")),
    tags$div(style = "height: 60px;"),
    htmlOutput(ns("title_web_page")),
    tags$div(style = "height: 10px;"),
    htmlOutput(ns("content_web_page"))
  )
}

about <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      output$title_about <- renderUI({
        HTML(paste0('<div class="montserrat" style="font-size: 20px; font-weight: 600; font-style: normal">',
                    tr("about_title1", language$language),
                    '</div>'
        ))
      })
      
      output$content_about <- renderUI({
        HTML(paste0('<div class="nunito-sans" style="font-size: 16px; font-weight: 500; font-style: normal;">',
                    tr("about_content1", language$language),
                    '</div>'
        ))
      })
      output$title_web_page <- renderUI({
        HTML(paste0('<div class="montserrat" style="font-size: 20px; font-weight: 600; font-style: normal">',
                    titleCase(tr("about_title2", language$language), language$abbrev),
                    '</div>'
        ))
      })
      output$content_web_page <- renderUI({
        url_text <- "placeholder"
        HTML(paste0('<div class="nunito-sans" style="font-size: 16px; font-weight: 500; font-style: normal">',
                    tr("about_content2", language$language),
                    '<a href="', tr("about_url", language$language), '" target="_blank">', tr("about_url", language$language), '</a>',
                    tr("about_content3", language$language),
                    '</div>'
        ))
      })
    })
  })
}
