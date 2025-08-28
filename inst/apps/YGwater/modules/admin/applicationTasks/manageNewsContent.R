manageNewsContentUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    tabsetPanel(
      id = ns("tabs"),
      tabPanel(
        title = "Add/Edit/Remove Text",
        textInput(ns("text_id"), "Text ID"),
        textAreaInput(ns("text_en"), "English text", width = "100%", height = "90px"),
        textAreaInput(ns("text_fr"), "French text", width = "100%", height = "90px"),
        div(
          actionButton(ns("save_text"), "Save text"),
          actionButton(ns("delete_text"), "Delete selected")
        ),
        tags$div("Double click cells to edit."),
        DT::DTOutput(ns("text_table"))
      ),
      tabPanel(
        title = "Add/Remove Image",
        fileInput(ns("image_file"), "Select image"),
        textInput(ns("image_id"), "Image ID"),
        div(
          actionButton(ns("save_image"), "Save image"),
          actionButton(ns("delete_image"), "Delete selected")
        ),
        tags$div("Double click cells to edit IDs."),
        DT::DTOutput(ns("image_table"))
      ),
      tabPanel(
        title = "Page Content",
        selectInput(ns("page_select"), "Select page", choices = NULL),
        DT::DTOutput(ns("page_table")),
        numericInput(ns("content_position"), "Position on page", value = 1, min = 1),
        selectInput(ns("content_type"), "Content type", choices = c("text", "image")),
        selectInput(ns("content_id"), "Content ID (reference name)", choices = NULL),
        div(
          actionButton(ns("add_page_content"), "Add new entry"),
          actionButton(ns("delete_page_content"), "Remove selected rows"),
          actionButton(ns("commit_page_content"), "Commit to DB")
        ),
        tags$hr(),
        div(
          tags$h4("Preview"),
          selectizeInput(ns("preview_language"), "Preview language", choices = c("English" = "en", "French" = "fr"), selected = "en")
        ),
        uiOutput(ns("page_preview"))
      )
    )
  )
}

manageNewsContent <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    text_df <- reactiveVal(data.frame())
    image_df <- reactiveVal(data.frame())
    text_ids_val <- reactiveVal(character())
    image_ids_val <- reactiveVal(character())
    preview_data <- reactiveVal(data.frame())
    
    # disable buttons if no insert privilege
    check_text <- DBI::dbGetQuery(session$userData$AquaCache,
                                  "SELECT has_table_privilege(current_user, 'application.text', 'INSERT') AS can_insert")
    if (!check_text$can_insert) shinyjs::disable("save_text")
    
    check_image <- DBI::dbGetQuery(session$userData$AquaCache,
                                   "SELECT has_table_privilege(current_user, 'application.images', 'INSERT') AS can_insert")
    if (!check_image$can_insert) shinyjs::disable("save_image")
    
    check_page <- DBI::dbGetQuery(session$userData$AquaCache,
                                  "SELECT has_table_privilege(current_user, 'application.page_content', 'INSERT') AS can_insert")
    if (!check_page$can_insert) {
      shinyjs::disable("add_page_content")
      shinyjs::disable("delete_page_content")
    }
    
    load_texts <- function(fx_input = input$content_type) {
      df <- DBI::dbGetQuery(session$userData$AquaCache,
                            "SELECT id, text_en, text_fr FROM application.text ORDER BY id")
      text_df(df)
      text_ids_val(df$id)
      if (fx_input == "text") {
        updateSelectInput(session, "content_id", choices = text_ids_val())
      }
    }
    
    load_images <- function(fx_input = input$content_type) {
      df <- DBI::dbGetQuery(session$userData$AquaCache,
                            "SELECT id, format FROM application.images ORDER BY id")
      image_df(df)
      image_ids_val(df$id)
      if (fx_input == "image") {
        updateSelectInput(session, "content_id", choices = image_ids_val())
      }
    }
    
    load_texts("text")
    load_images("image")
    
    output$text_table <- DT::renderDT({
      DT::datatable(text_df(), selection = 'multiple', rownames = FALSE, editable = TRUE, options = list(scrollX = TRUE))
    })
    
    output$image_table <- DT::renderDT({
      DT::datatable(image_df(), selection = 'multiple', rownames = FALSE, editable = TRUE, options = list(scrollX = TRUE))
    })
    
    observeEvent(input$text_table_cell_edit, {
      info <- input$text_table_cell_edit
      i    <- info$row
      j    <- info$col + 1   # <- adjust for zero-based + hidden rownames
      v    <- info$value
      
      df <- text_df()
      old_id <- df$id[i]
      df[i, j] <- v
      text_df(df)
      
      col_name <- names(df)[j]
      if (col_name == "id") {
        DBI::dbExecute(session$userData$AquaCache,
                       "UPDATE application.text SET id = $1 WHERE id = $2",
                       params = list(v, old_id))
      } else {
        DBI::dbExecute(session$userData$AquaCache,
                       sprintf("UPDATE application.text SET %s = $1 WHERE id = $2", col_name),
                       params = list(v, old_id))
      }
      load_texts()
    })
    
    observeEvent(input$image_table_cell_edit, {
      info <- input$image_table_cell_edit
      i    <- info$row
      j    <- info$col + 1   # <- adjust for zero-based + hidden rownames
      v    <- info$value
      
      df <- image_df()
      old_id <- df$id[i]
      df[i, j] <- v
      image_df(df)
      col_name <- names(df)[j]
      if (col_name == "id") {
        DBI::dbExecute(session$userData$AquaCache,
                       "UPDATE application.images SET id = $1 WHERE id = $2",
                       params = list(v, old_id))
      } else {
        DBI::dbExecute(session$userData$AquaCache,
                       sprintf("UPDATE application.images SET %s = $1 WHERE id = $2", col_name),
                       params = list(v, old_id))
      }
      load_images()
    })
    
    observeEvent(input$delete_text, {
      req(input$text_table_rows_selected)
      ids <- text_df()$id[input$text_table_rows_selected]
      for (id in ids) {
        DBI::dbExecute(session$userData$AquaCache,
                       "DELETE FROM application.text WHERE id = $1",
                       params = list(id))
      }
      load_texts()
      if (input$content_type == "text") {
        updateSelectInput(session, "content_id", choices = text_ids_val())
      }
    })
    
    observeEvent(input$delete_image, {
      req(input$image_table_rows_selected)
      ids <- image_df()$id[input$image_table_rows_selected]
      for (id in ids) {
        DBI::dbExecute(session$userData$AquaCache,
                       "DELETE FROM application.images WHERE id = $1",
                       params = list(id))
      }
      load_images()
      if (input$content_type == "image") {
        updateSelectInput(session, "content_id", choices = image_ids_val())
      }
    })
    
    load_pages <- function() {
      df <- DBI::dbGetQuery(session$userData$AquaCache,
                            "SELECT DISTINCT page FROM application.page_content ORDER BY page")
      updateSelectInput(session, "page_select", choices = df$page)
    }
    load_pages()
    
    text_ids <- reactive(text_ids_val())
    image_ids <- reactive(image_ids_val())
    
    observeEvent(input$content_type, {
      if (input$content_type == "text") {
        updateSelectInput(session, "content_id", choices = text_ids())
      } else {
        updateSelectInput(session, "content_id", choices = image_ids())
      }
    }, ignoreInit = TRUE)
    
    preview_data(data.frame())
    
    observeEvent(input$page_select, {
      req(input$page_select)
      q <- sprintf("SELECT * FROM application.page_content WHERE page = '%s' ORDER BY position", input$page_select)
      df <- DBI::dbGetQuery(session$userData$AquaCache, q)
      preview_data(df)
    })
    
    output$page_table <- DT::renderDT({
      DT::datatable(preview_data(), selection = 'multiple', rownames = FALSE, options = list(scrollX = TRUE))
    })
    
    output$page_preview <- renderUI({
      df <- preview_data()
      if (nrow(df) == 0) return(NULL)
      ui_elements <- lapply(seq_len(nrow(df)), function(i) {
        row <- df[i, ]
        if (row$content_type == "text") {
          text_query <- sprintf("SELECT * FROM application.text WHERE id = '%s'", row$content_id)
          text_data <- DBI::dbGetQuery(session$userData$AquaCache, text_query)
          if (nrow(text_data) > 0) {
            if (input$preview_language == "fr") {
              HTML(text_data$text_fr[1])
            } else {
              HTML(text_data$text_en[1])
            }
          } else {
            tags$p("Text not found")
          }
        } else if (row$content_type == "image") {
          image_query <- sprintf("SELECT * FROM application.images WHERE id = '%s'", row$content_id)
          image_data <- DBI::dbGetQuery(session$userData$AquaCache, image_query)
          if (nrow(image_data) > 0) {
            image_binary <- image_data$image[[1]]
            image_format <- image_data$format[[1]]
            base64_string <- base64enc::base64encode(image_binary)
            data_url <- sprintf("data:image/%s;base64,%s", image_format, base64_string)
            tags$img(src = data_url, style = "max-width:100%;")
          } else {
            tags$p("Image not found")
          }
        } else {
          tags$p("Unknown content type")
        }
      })
      do.call(tagList, ui_elements)
    })
    
    observeEvent(input$save_text, {
      req(input$text_id, input$text_en)
      tryCatch({
        DBI::dbExecute(session$userData$AquaCache,
                       "INSERT INTO application.text (id, text_en, text_fr) VALUES ($1,$2,$3)",
                       params = list(input$text_id, input$text_en,
                                     ifelse(nzchar(input$text_fr), input$text_fr, NA)))
        showNotification("Text saved.", type = 'message')
        load_texts()
        if (input$content_type == "text") {
          updateSelectInput(session, "content_id", choices = text_ids_val())
        }
      }, error = function(e) {
        showNotification(paste('Insert failed:', e$message), type = 'error')
      })
    })
    
    observeEvent(input$save_image, {
      req(input$image_file, input$image_id)
      bin <- hexView::readRaw(input$image_file$datapath)$fileRaw
      fmt <- tools::file_ext(input$image_file$name)
      tryCatch({
        DBI::dbExecute(session$userData$AquaCache,
                       "INSERT INTO application.images (id, image, format) VALUES ($1,$2,$3)",
                       params = list(input$image_id, paste0(bin, collapse = ""), fmt))
        showNotification("Image saved.", type = 'message')
        load_images()
        if (input$content_type == "image") {
          updateSelectInput(session, "content_id", choices = image_ids_val())
        }
      }, error = function(e) {
        showNotification(paste('Insert failed:', e$message), type = 'error')
      })
    })
    
    observeEvent(input$add_page_content, {
      req(input$page_select, input$content_id)
      df <- preview_data()
      new_row <- data.frame(page = input$page_select,
                            position = as.integer(input$content_position),
                            content_type = input$content_type,
                            content_id = input$content_id,
                            created = NA,
                            modified = NA,
                            created_by = NA,
                            modified_by = NA,
                            stringsAsFactors = FALSE)
      df <- rbind(df, new_row)
      df <- df[order(df$position), ]
      preview_data(df)
    })
    
    observeEvent(input$delete_page_content, {
      req(input$page_table_rows_selected)
      df <- preview_data()
      df <- df[-input$page_table_rows_selected, , drop = FALSE]
      preview_data(df)
    })
    
    observeEvent(input$commit_page_content, {
      req(input$page_select)
      df <- preview_data()
      DBI::dbExecute(session$userData$AquaCache,
                     "DELETE FROM application.page_content WHERE page = $1",
                     params = list(input$page_select))
      if (nrow(df) > 0) {
        for (i in seq_len(nrow(df))) {
          DBI::dbExecute(session$userData$AquaCache,
                         "INSERT INTO application.page_content (page, position, content_type, content_id) VALUES ($1,$2,$3,$4)",
                         params = list(df$page[i], df$position[i], df$content_type[i], df$content_id[i]))
        }
      }
      showNotification("Page content committed.", type = 'message')
      load_pages()
      q <- sprintf("SELECT * FROM application.page_content WHERE page = '%s' ORDER BY position", input$page_select)
      preview_data(DBI::dbGetQuery(session$userData$AquaCache, q))
    })
  })
}
