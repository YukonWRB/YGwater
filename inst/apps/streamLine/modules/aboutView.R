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
      lang <- language()
      abbrev <- translations[translations$id == "titleCase", ..lang][[1]]
      
      output$title_about <- renderUI({
        HTML(paste0('<div class="montserrat" style="font-size: 20px; font-weight: 600; font-style: normal">',
                    translations[translations$id == "about_title1", ..lang][[1]],
                    '</div>'
        ))
      })
      
      output$content_about <- renderUI({
        HTML(paste0('<div class="nunito-sans" style="font-size: 16px; font-weight: 500; font-style: normal;">',
                    translations[translations$id == "about_content1", ..lang][[1]],
                    '</div>'
        ))
      })
      output$title_web_page <- renderUI({
        HTML(paste0('<div class="montserrat" style="font-size: 20px; font-weight: 600; font-style: normal">',
                    titleCase(translations[translations$id == "about_title2", ..lang][[1]], abbrev),
                    '</div>'
        ))
      })
      output$content_web_page <- renderUI({
        url_text <- "placeholder"
        HTML(paste0('<div class="nunito-sans" style="font-size: 16px; font-weight: 500; font-style: normal">',
                    translations[translations$id == "about_content2", ..lang][[1]],
                    '<a href="', translations[translations$id == "about_url", ..lang][[1]], '" target="_blank">', translations[translations$id == "about_url", ..lang][[1]], '</a>',
                    translations[translations$id == "about_content3", ..lang][[1]],
                    '</div>'
        ))
      })
    })
  })
}
