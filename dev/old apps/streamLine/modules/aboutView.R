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

about <- function(id, con, language, restoring) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      output$title_about <- renderUI({
        HTML(paste0('<div class="montserrat" style="font-size: 20px; font-weight: 600; font-style: normal">',
                    translations[id == "about_title1", get(language$language)][[1]],
                    '</div>'
        ))
      })
      
      output$content_about <- renderUI({
        HTML(paste0('<div class="nunito-sans" style="font-size: 16px; font-weight: 500; font-style: normal;">',
                    translations[id == "about_content1", get(language$language)][[1]],
                    '</div>'
        ))
      })
      output$title_web_page <- renderUI({
        HTML(paste0('<div class="montserrat" style="font-size: 20px; font-weight: 600; font-style: normal">',
                    titleCase(translations[id == "about_title2", get(language$language)][[1]], language$abbrev),
                    '</div>'
        ))
      })
      output$content_web_page <- renderUI({
        url_text <- "placeholder"
        HTML(paste0('<div class="nunito-sans" style="font-size: 16px; font-weight: 500; font-style: normal">',
                    translations[id == "about_content2", get(language$language)][[1]],
                    '<a href="', translations[id == "about_url", get(language$language)][[1]], '" target="_blank">', translations[id == "about_url", get(language$language)][[1]], '</a>',
                    translations[id == "about_content3", get(language$language)][[1]],
                    '</div>'
        ))
      })
    })
  })
}
