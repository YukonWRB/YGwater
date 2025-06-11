manageNewsContentUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    tabsetPanel(
      id = ns("tabs"),
      tabPanel(
        title = "Add Text",
        textInput(ns("text_id"), "Text ID"),
        textAreaInput(ns("text_en"), "English text", width = "100%", height = "120px"),
        textAreaInput(ns("text_fr"), "French text", width = "100%", height = "120px"),
        actionButton(ns("save_text"), "Save text")
      ),
      tabPanel(
        title = "Add Image",
        fileInput(ns("image_file"), "Select image"),
        textInput(ns("image_id"), "Image ID"),
        actionButton(ns("save_image"), "Save image")
      ),
      tabPanel(
        title = "Page Content",
        selectInput(ns("page_select"), "Select page", choices = NULL),
        DT::DTOutput(ns("page_table")),
        numericInput(ns("content_position"), "Position", value = 1, min = 1),
        selectInput(ns("content_type"), "Content type", choices = c("text", "image")),
        selectInput(ns("content_id"), "Content ID", choices = NULL),
        div(
          actionButton(ns("add_page_content"), "Add to page"),
          actionButton(ns("delete_page_content"), "Delete selected")
        )
      )
    )
  )
}

manageNewsContent <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

    load_pages <- function() {
      df <- DBI::dbGetQuery(session$userData$AquaCache,
                             "SELECT DISTINCT page FROM application.page_content ORDER BY page")
      updateSelectInput(session, "page_select", choices = df$page)
    }
    load_pages()

    text_ids <- reactive({
      DBI::dbGetQuery(session$userData$AquaCache,
                      "SELECT id FROM application.text ORDER BY id")$id
    })
    image_ids <- reactive({
      DBI::dbGetQuery(session$userData$AquaCache,
                      "SELECT id FROM application.images ORDER BY id")$id
    })

    observeEvent(input$content_type, {
      if (input$content_type == "text") {
        updateSelectInput(session, "content_id", choices = text_ids())
      } else {
        updateSelectInput(session, "content_id", choices = image_ids())
      }
    }, ignoreInit = TRUE)

    page_data <- reactiveVal(data.frame())

    observeEvent(input$page_select, {
      req(input$page_select)
      q <- sprintf("SELECT * FROM application.page_content WHERE page = '%s' ORDER BY position", input$page_select)
      df <- DBI::dbGetQuery(session$userData$AquaCache, q)
      page_data(df)
    })

    output$page_table <- DT::renderDT({
      DT::datatable(page_data(), selection = 'multiple', options = list(scrollX = TRUE))
    })

    observeEvent(input$save_text, {
      req(input$text_id, input$text_en)
      tryCatch({
        DBI::dbExecute(session$userData$AquaCache,
                        "INSERT INTO application.text (id, text_en, text_fr) VALUES ($1,$2,$3)",
                        params = list(input$text_id, input$text_en,
                                     ifelse(nzchar(input$text_fr), input$text_fr, NA)))
        showNotification("Text saved.", type = 'message')
      }, error = function(e) {
        showNotification(paste('Insert failed:', e$message), type = 'error')
      })
    })

    observeEvent(input$save_image, {
      req(input$image_file, input$image_id)
      bin <- readBin(input$image_file$datapath, 'raw', n = file.size(input$image_file$datapath))
      fmt <- tools::file_ext(input$image_file$name)
      tryCatch({
        DBI::dbExecute(session$userData$AquaCache,
                        "INSERT INTO application.images (id, image, format) VALUES ($1,$2,$3)",
                        params = list(input$image_id, bin, fmt))
        showNotification("Image saved.", type = 'message')
      }, error = function(e) {
        showNotification(paste('Insert failed:', e$message), type = 'error')
      })
    })

    observeEvent(input$add_page_content, {
      req(input$page_select, input$content_id)
      tryCatch({
        DBI::dbExecute(session$userData$AquaCache,
                        "INSERT INTO application.page_content (page, position, content_type, content_id) VALUES ($1,$2,$3,$4)",
                        params = list(input$page_select, as.integer(input$content_position), input$content_type, input$content_id))
        q <- sprintf("SELECT * FROM application.page_content WHERE page = '%s' ORDER BY position", input$page_select)
        page_data(DBI::dbGetQuery(session$userData$AquaCache, q))
        showNotification("Content added.", type = 'message')
      }, error = function(e) {
        showNotification(paste('Insert failed:', e$message), type = 'error')
      })
    })

    observeEvent(input$delete_page_content, {
      req(input$page_table_rows_selected)
      rows <- page_data()[input$page_table_rows_selected, ]
      for (i in seq_len(nrow(rows))) {
        DBI::dbExecute(session$userData$AquaCache,
                        "DELETE FROM application.page_content WHERE page=$1 AND position=$2 AND content_type=$3 AND content_id=$4",
                        params = list(rows$page[i], rows$position[i], rows$content_type[i], rows$content_id[i]))
      }
      q <- sprintf("SELECT * FROM application.page_content WHERE page = '%s' ORDER BY position", input$page_select)
      page_data(DBI::dbGetQuery(session$userData$AquaCache, q))
      showNotification("Content deleted.", type = 'message')
    })
  })
}
