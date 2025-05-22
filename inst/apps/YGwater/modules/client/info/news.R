
newsUI <- function(id) {
  ns <- NS(id)
  verticalLayout(
    # tags$div(style = "height: 10px;"),

    uiOutput(ns("content"))
    
  )
}

news <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    output$content <- renderUI({
      order <- DBI::dbGetQuery(session$userData$AquaCache, "SELECT * FROM application.page_content WHERE page = 'news' ORDER BY position")
      
      # Create a list to hold the UI elements
      ui_elements <- lapply(seq_len(nrow(order)), function(i) {
        row <- order[i, ]
        if (row$content_type == "text") {
          # Query the text content
          text_query <- sprintf("SELECT * FROM application.text WHERE id = '%s'", row$content_id)
          text_data <- DBI::dbGetQuery(session$userData$AquaCache, text_query)
          if (nrow(text_data) > 0) {
            # Pick the right language column; assume columns like text_en and text_fr
            lang_col <- paste0("text_", language$abbrev)
            content_text <- text_data[[lang_col]][1]
            HTML(content_text)
          } else {
            tags$p("Text content not found.")
          }
        } else if (row$content_type == "image") {
          # Query the image content
          image_query <- sprintf("SELECT * FROM application.images WHERE id = '%s'", row$content_id)
          image_data <- DBI::dbGetQuery(session$userData$AquaCache, image_query)
          if (nrow(image_data) > 0) {
            # Convert the BYTEA data to a base64 string
            image_binary <- image_data$image[[1]]
            image_format <- image_data$format[[1]]
            base64_string <- base64enc::base64encode(image_binary)
            data_url <- sprintf("data:image/%s;base64,%s", image_format, base64_string)
            tags$img(src = data_url, style = "max-width:100%;")
          } else {
            tags$p("Image content not found.")
          }
        } else {
          tags$p("Unknown content type.")
        }
      })
      
      # Combine the UI elements into a single UI object
      do.call(tagList, ui_elements)
    })
  })
}
