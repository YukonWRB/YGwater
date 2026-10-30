manageNewsContentUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    uiOutput(ns("banner")),
    tabsetPanel(
      id = ns("tabs"),
      tabPanel(
        title = "Add/Edit/Remove Text",
        actionButton(ns("new_text"), "New record"),
        textInput(ns("text_id"), "Text ID"),
        helpText(
          "HTML tags such as <h3>...</h3> are supported. Pasted entity-escaped tags are converted when saved."
        ),
        textAreaInput(
          ns("text_en"),
          "English text",
          width = "100%",
          height = "90px"
        ),
        textAreaInput(
          ns("text_fr"),
          "French text",
          width = "100%",
          height = "90px"
        ),
        div(
          actionButton(ns("save_text"), "Create new entry"),
          uiOutput(ns("delete_text_button"))
        ),
        uiOutput(ns("text_edit_status")),
        tags$div(
          "Select a row to load its English and French text into the boxes above."
        ),
        DT::DTOutput(ns("text_table"))
      ),
      tabPanel(
        title = "Add/Remove Image",
        fileInput(
          ns("image_file"),
          "Choose an image",
          accept = c(".jpg", ".jpeg", ".png", ".gif", ".webp", ".heic", ".heif")
        ),
        helpText(
          "Use JPEG, PNG, GIF, or WebP. HEIC/HEIF uploads are converted to JPEG when this server supports conversion."
        ),
        uiOutput(ns("uploaded_image_preview")),
        textInput(ns("image_id"), "Image ID"),
        div(
          actionButton(ns("save_image"), "Save image"),
          actionButton(ns("delete_image"), "Delete selected")
        ),
        tags$div(
          "Select a row to preview an image. Double-click an ID to edit it; edits and deletions save immediately."
        ),
        DT::DTOutput(ns("image_table")),
        uiOutput(ns("selected_image_preview"))
      ),
      tabPanel(
        title = "Page Content",
        selectInput(ns("page_select"), "Select page", choices = NULL),
        helpText(
          "Positions 1-3 are reserved for the page header. For the remaining content, larger position numbers appear first."
        ),
        uiOutput(ns("page_status")),
        DT::DTOutput(ns("page_table")),
        numericInput(
          ns("content_position"),
          "Position on page",
          value = 4,
          min = 4
        ),
        helpText(
          "The next available position is filled in when you select a page. To move an existing item, remove it from the draft and add it again at the new position."
        ),
        selectInput(
          ns("content_type"),
          "Content type",
          choices = c("Text" = "text", "Image" = "image")
        ),
        selectInput(
          ns("content_id"),
          "Content ID (reference name)",
          choices = NULL
        ),
        div(
          actionButton(ns("add_page_content"), "Add new entry"),
          actionButton(ns("delete_page_content"), "Remove selected rows"),
          actionButton(ns("commit_page_content"), "Save page layout")
        ),
        helpText("Changes to the page layout are a draft until you save them."),
        tags$hr(),
        div(
          tags$h4("Preview"),
          selectizeInput(
            ns("preview_language"),
            "Preview language",
            choices = c("English" = "en", "French" = "fr"),
            selected = "en"
          )
        ),
        uiOutput(ns("page_preview"))
      )
    )
  )
}

manageNewsContent <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "manageNewsContent"
      )
    })

    text_df <- reactiveVal(data.frame(
      id = character(),
      text_en = character(),
      text_fr = character()
    ))
    image_df <- reactiveVal(data.frame(
      id = character(),
      format = character()
    ))
    text_ids_val <- reactiveVal(character())
    image_ids_val <- reactiveVal(character())
    selected_text_id <- reactiveVal(NULL)
    pending_delete_text <- reactiveVal(NULL)
    empty_page_content <- function() {
      data.frame(
        page = character(),
        position = integer(),
        content_type = character(),
        content_id = character(),
        stringsAsFactors = FALSE
      )
    }
    preview_data <- reactiveVal(empty_page_content())
    loaded_page <- reactiveVal(NULL)
    reserved_position_max <- 3

    privileges <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT
         has_table_privilege(current_user, 'application.text', 'INSERT') AS text_insert,
         has_table_privilege(current_user, 'application.text', 'UPDATE') AS text_update,
         has_table_privilege(current_user, 'application.text', 'DELETE') AS text_delete,
         has_table_privilege(current_user, 'application.images', 'INSERT') AS image_insert,
         has_table_privilege(current_user, 'application.page_content', 'INSERT') AS page_insert,
         has_table_privilege(current_user, 'application.page_content', 'DELETE') AS page_delete,
         has_table_privilege(current_user, 'application.page_content', 'UPDATE') AS page_update"
    )
    can_insert_text <- isTRUE(privileges$text_insert[1])
    can_update_text <- isTRUE(privileges$text_update[1])
    if (!can_insert_text && !can_update_text) {
      shinyjs::disable("save_text")
    }
    can_delete_text <- isTRUE(privileges$text_delete[1])
    can_update_page_content <- isTRUE(privileges$page_update[1])
    if (!isTRUE(privileges$image_insert[1])) {
      shinyjs::disable("save_image")
    }
    if (
      !isTRUE(privileges$page_insert[1]) || !isTRUE(privileges$page_delete[1])
    ) {
      shinyjs::disable("commit_page_content")
    }

    update_content_id_choices <- function(
      content_type = input$content_type,
      preferred_id = NULL
    ) {
      ids <- if (identical(content_type, "image")) {
        image_ids_val()
      } else {
        text_ids_val()
      }
      current_id <- if (is.null(preferred_id)) {
        input$content_id
      } else {
        preferred_id
      }
      selected_id <- if (
        length(current_id) == 1L &&
          !is.na(current_id) &&
          current_id %in% ids
      ) {
        current_id
      } else {
        character()
      }
      updateSelectInput(
        session,
        "content_id",
        label = if (identical(content_type, "image")) {
          "Image ID"
        } else {
          "Text ID"
        },
        choices = ids,
        selected = selected_id
      )
    }

    load_texts <- function(fx_input = input$content_type) {
      df <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT id, text_en, text_fr FROM application.text ORDER BY id"
      )
      text_df(df)
      text_ids_val(df$id)
      if (identical(fx_input, "text")) {
        update_content_id_choices("text")
      }
    }

    load_images <- function(fx_input = input$content_type) {
      df <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT id, format FROM application.images ORDER BY id"
      )
      image_df(df)
      image_ids_val(df$id)
      if (identical(fx_input, "image")) {
        update_content_id_choices("image")
      }
    }

    load_texts(fx_input = NULL)
    load_images(fx_input = NULL)
    update_content_id_choices(input$content_type)

    clear_text_form <- function(clear_selection = FALSE) {
      selected_text_id(NULL)
      updateTextInput(session, "text_id", value = "")
      updateTextAreaInput(session, "text_en", value = "")
      updateTextAreaInput(session, "text_fr", value = "")
      shinyjs::enable("text_id")
      updateActionButton(session, "save_text", label = "Create new entry")
      if (can_insert_text) {
        shinyjs::enable("save_text")
      } else {
        shinyjs::disable("save_text")
      }
      if (clear_selection) {
        DT::selectRows(
          DT::dataTableProxy("text_table", session = session),
          NULL
        )
      }
    }

    output$text_table <- DT::renderDT({
      DT::datatable(
        text_df(),
        selection = 'single',
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 10, lengthChange = FALSE)
      )
    })

    output$delete_text_button <- renderUI({
      rows <- input$text_table_rows_selected
      if (length(rows) != 1L || !can_delete_text) {
        return(NULL)
      }
      actionButton(
        ns("delete_text"),
        "Delete selected",
        class = "btn-danger"
      )
    })

    output$text_edit_status <- renderUI({
      if (is.null(selected_text_id())) {
        return(tags$p(
          "New record: enter a Text ID and text, then create the entry."
        ))
      }
      if (!can_update_text) {
        return(tags$p(
          "This database role cannot update existing text. Ask an administrator for UPDATE permission on application.text."
        ))
      }
      if (
        !can_update_page_content &&
          !identical(
            trimws(if (is.null(input$text_id)) "" else input$text_id),
            selected_text_id()
          )
      ) {
        return(tags$p(
          "This database role cannot update page references, so the Text ID cannot be changed."
        ))
      }
      tags$p(paste("Editing text ID:", selected_text_id()))
    })

    output$image_table <- DT::renderDT({
      DT::datatable(
        image_df(),
        selection = 'multiple',
        rownames = FALSE,
        editable = TRUE,
        options = list(scrollX = TRUE)
      )
    })

    observeEvent(
      input$text_table_rows_selected,
      {
        rows <- input$text_table_rows_selected
        if (length(rows) != 1L) {
          clear_text_form()
          return()
        }

        row <- text_df()[rows, , drop = FALSE]
        selected_text_id(row$id[1])
        updateTextInput(session, "text_id", value = row$id[1])
        updateTextAreaInput(
          session,
          "text_fr",
          value = if (is.na(row$text_fr[1])) {
            ""
          } else {
            decode_escaped_html_tags(row$text_fr[1])
          }
        )
        updateTextAreaInput(
          session,
          "text_en",
          value = if (is.na(row$text_en[1])) {
            ""
          } else {
            decode_escaped_html_tags(row$text_en[1])
          }
        )
        shinyjs::enable("text_id")
        updateActionButton(session, "save_text", label = "Save changes")
        if (can_update_text) {
          shinyjs::enable("save_text")
        } else {
          shinyjs::disable("save_text")
        }
      },
      ignoreNULL = FALSE
    )

    observeEvent(input$new_text, clear_text_form(clear_selection = TRUE))

    observeEvent(input$save_text, {
      if (is.null(input$text_en) || !nzchar(trimws(input$text_en))) {
        showNotification("English text cannot be blank.", type = "error")
        return()
      }
      text_id <- trimws(if (is.null(input$text_id)) "" else input$text_id)
      if (!nzchar(text_id)) {
        showNotification("Enter a Text ID.", type = "error")
        return()
      }
      text_en <- decode_escaped_html_tags(input$text_en)
      text_fr <- if (
        length(input$text_fr) == 1L &&
          !is.na(input$text_fr) &&
          nzchar(input$text_fr)
      ) {
        decode_escaped_html_tags(input$text_fr)
      } else {
        NA_character_
      }

      old_text_id <- selected_text_id()
      if (is.null(old_text_id)) {
        if (!isTRUE(privileges$text_insert[1])) {
          showNotification(
            "This database role cannot create text entries.",
            type = "error"
          )
          return()
        }
        created <- tryCatch(
          {
            DBI::dbExecute(
              session$userData$AquaCache,
              "INSERT INTO application.text (id, text_en, text_fr) VALUES ($1,$2,$3)",
              params = list(text_id, text_en, text_fr)
            )
            TRUE
          },
          error = function(e) {
            showNotification(
              paste("Text creation failed:", e$message),
              type = "error"
            )
            FALSE
          }
        )
        if (!created) {
          return()
        }
        load_texts(fx_input = NULL)
        if (identical(input$content_type, "text")) {
          update_content_id_choices("text")
        }
        clear_text_form(clear_selection = TRUE)
        showNotification("Text entry created.", type = "message")
        return()
      }

      if (!can_update_text) {
        showNotification(
          "This database role cannot update application.text.",
          type = "error"
        )
        return()
      }
      if (!identical(old_text_id, text_id) && !can_update_page_content) {
        showNotification(
          "This database role cannot update application.page_content, so the Text ID cannot be changed.",
          type = "error"
        )
        return()
      }

      tryCatch(
        {
          DBI::dbWithTransaction(session$userData$AquaCache, {
            if (!identical(old_text_id, text_id)) {
              duplicate <- DBI::dbGetQuery(
                session$userData$AquaCache,
                "SELECT EXISTS (SELECT 1 FROM application.text WHERE id = $1) AS id_exists",
                params = list(text_id)
              )$id_exists[1]
              if (isTRUE(duplicate)) {
                stop("That Text ID already exists.", call. = FALSE)
              }
            }
            updated <- DBI::dbExecute(
              session$userData$AquaCache,
              "UPDATE application.text
               SET id = $1, text_en = $2, text_fr = $3
               WHERE id = $4",
              params = list(text_id, text_en, text_fr, old_text_id)
            )
            if (updated == 0L) {
              stop(
                "The selected text was not found; reload the list and try again.",
                call. = FALSE
              )
            }
            if (!identical(old_text_id, text_id)) {
              DBI::dbExecute(
                session$userData$AquaCache,
                "UPDATE application.page_content
                 SET content_id = $1
                 WHERE content_type = 'text' AND content_id = $2",
                params = list(text_id, old_text_id)
              )
            }
          })
          selected_text_id(text_id)
          load_texts(fx_input = NULL)
          selected_row <- match(text_id, text_df()$id)
          if (!is.na(selected_row)) {
            DT::selectRows(
              DT::dataTableProxy("text_table", session = session),
              selected_row
            )
          }
          if (identical(input$content_type, "text")) {
            update_content_id_choices("text", preferred_id = text_id)
          }
          if (!is.null(input$page_select)) {
            draft <- preview_data()
            draft$content_id[
              draft$content_type == "text" & draft$content_id == old_text_id
            ] <- text_id
            preview_data(draft)
          }
          updateActionButton(session, "save_text", label = "Save changes")
          showNotification("Text changes saved.", type = "message")
        },
        error = function(e) {
          showNotification(
            paste("Text update failed:", e$message),
            type = "error"
          )
        }
      )
    })

    observeEvent(input$image_table_cell_edit, {
      info <- input$image_table_cell_edit
      i <- info$row
      j <- info$col + 1 # <- adjust for zero-based + hidden rownames
      v <- info$value

      df <- image_df()
      old_id <- df$id[i]
      df[i, j] <- v
      image_df(df)
      col_name <- names(df)[j]
      if (col_name == "id") {
        DBI::dbExecute(
          session$userData$AquaCache,
          "UPDATE application.images SET id = $1 WHERE id = $2",
          params = list(v, old_id)
        )
      } else {
        DBI::dbExecute(
          session$userData$AquaCache,
          sprintf(
            "UPDATE application.images SET %s = $1 WHERE id = $2",
            col_name
          ),
          params = list(v, old_id)
        )
      }
      load_images()
    })

    observeEvent(input$delete_text, {
      rows <- input$text_table_rows_selected
      if (length(rows) != 1L || !can_delete_text) {
        return()
      }
      text_id <- text_df()$id[rows]
      pending_delete_text(text_id)
      showModal(modalDialog(
        title = "Delete text entry?",
        tags$p(sprintf("Delete '%s'? This cannot be undone.", text_id)),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            ns("confirm_delete_text"),
            "Delete entry",
            class = "btn-danger"
          )
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$confirm_delete_text, {
      text_id <- pending_delete_text()
      if (is.null(text_id) || !can_delete_text) {
        removeModal()
        return()
      }
      removeModal()
      pending_delete_text(NULL)
      tryCatch(
        {
          DBI::dbExecute(
            session$userData$AquaCache,
            "DELETE FROM application.text WHERE id = $1",
            params = list(text_id)
          )
          load_texts(fx_input = NULL)
          if (identical(input$content_type, "text")) {
            update_content_id_choices("text")
          }
          clear_text_form(clear_selection = TRUE)
          showNotification("Text entry deleted.", type = "message")
        },
        error = function(e) {
          showNotification(
            paste("Text deletion failed:", e$message),
            type = "error"
          )
        }
      )
    })

    observeEvent(input$delete_image, {
      req(input$image_table_rows_selected)
      ids <- image_df()$id[input$image_table_rows_selected]
      for (id in ids) {
        DBI::dbExecute(
          session$userData$AquaCache,
          "DELETE FROM application.images WHERE id = $1",
          params = list(id)
        )
      }
      load_images()
      if (input$content_type == "image") {
        updateSelectInput(session, "content_id", choices = image_ids_val())
      }
    })

    load_page <- function(page) {
      if (length(page) != 1L || is.na(page) || !nzchar(page)) {
        preview_data(empty_page_content())
        loaded_page(NULL)
        return(invisible(NULL))
      }

      df <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT page, position, content_type, content_id
         FROM application.page_content
         WHERE page = $1
         ORDER BY position",
        params = list(page)
      )
      preview_data(df)
      loaded_page(page)
      next_position <- max(
        c(reserved_position_max, df$position),
        na.rm = TRUE
      ) +
        1L
      updateNumericInput(
        session,
        "content_position",
        value = as.integer(next_position)
      )
      invisible(df)
    }

    load_pages <- function(preferred_page = input$page_select) {
      pages <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT DISTINCT page FROM application.page_content ORDER BY page"
      )$page
      selected_page <- if (
        length(preferred_page) == 1L &&
          !is.na(preferred_page) &&
          preferred_page %in% pages
      ) {
        preferred_page
      } else if ("news" %in% pages) {
        "news"
      } else if (length(pages)) {
        pages[1]
      } else {
        character()
      }
      updateSelectInput(
        session,
        "page_select",
        choices = pages,
        selected = selected_page
      )
      load_page(selected_page)
      invisible(pages)
    }
    load_pages(preferred_page = NULL)

    observeEvent(
      input$content_type,
      update_content_id_choices(input$content_type),
      ignoreInit = TRUE
    )

    observeEvent(
      input$page_select,
      load_page(input$page_select),
      ignoreInit = TRUE
    )

    display_preview_data <- reactive({
      df <- preview_data()
      df <- df[
        !is.na(df$position) & df$position > reserved_position_max,
        ,
        drop = FALSE
      ]
      df[order(-df$position), , drop = FALSE]
    })

    output$page_status <- renderUI({
      if (is.null(input$page_select) || !nzchar(input$page_select)) {
        return(tags$p(
          "No pages are available yet. Add page content rows to create a page."
        ))
      }
      if (nrow(display_preview_data()) == 0L) {
        return(tags$p(
          "This page has no editable content yet. Add the first item below."
        ))
      }
      NULL
    })

    output$page_table <- DT::renderDT({
      df <- display_preview_data()[,
        c("position", "content_type", "content_id"),
        drop = FALSE
      ]
      names(df) <- c("Position", "Type", "Content ID")
      DT::datatable(
        df,
        selection = 'multiple',
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 10, lengthChange = FALSE)
      )
    })

    output$uploaded_image_preview <- renderUI({
      req(input$image_file)
      preview_url <- tryCatch(
        YGwater:::with_heif_upload_as_jpeg(
          input$image_file$datapath,
          function(upload_path) {
            size <- file.info(upload_path)$size
            if (is.na(size) || size < 1) {
              return(NULL)
            }
            image_binary <- readBin(upload_path, what = "raw", n = size)
            format <- tools::file_ext(upload_path)
            if (format %in% c("jpg", "jpe")) {
              format <- "jpeg"
            }
            application_image_data_url(image_binary, format)
          },
          extension = tools::file_ext(input$image_file$name)
        ),
        error = function(e) NULL
      )
      if (is.null(preview_url)) {
        return(tags$p("A preview is not available for this file."))
      }
      tags$div(
        tags$p("Upload preview"),
        tags$img(
          src = preview_url,
          alt = "Selected upload preview",
          style = "max-width: 100%; max-height: 320px; height: auto;"
        )
      )
    })

    output$selected_image_preview <- renderUI({
      selected <- input$image_table_rows_selected
      if (is.null(selected) || !length(selected)) {
        return(tags$p("Select an image row to preview the saved image."))
      }
      image_id <- image_df()$id[selected[1]]
      image_data <- tryCatch(
        DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT image, format FROM application.images WHERE id = $1",
          params = list(image_id)
        ),
        error = function(e) NULL
      )
      if (is.null(image_data) || nrow(image_data) == 0L) {
        return(tags$p("The selected image could not be loaded."))
      }
      data_url <- application_image_data_url(
        image_data$image[[1]],
        image_data$format[1]
      )
      if (is.null(data_url)) {
        return(tags$p("This image format cannot be previewed in the browser."))
      }
      tags$div(
        tags$p(paste("Saved image:", image_id)),
        tags$img(
          src = data_url,
          alt = image_id,
          style = "max-width: 100%; max-height: 320px; height: auto;"
        )
      )
    })

    output$page_preview <- renderUI({
      df <- display_preview_data()
      if (nrow(df) == 0) {
        return(tags$p("Add page content to see a preview."))
      }
      ui_elements <- lapply(seq_len(nrow(df)), function(i) {
        row <- df[i, ]
        if (row$content_type == "text") {
          text_data <- DBI::dbGetQuery(
            session$userData$AquaCache,
            "SELECT * FROM application.text WHERE id = $1",
            params = list(row$content_id)
          )
          if (nrow(text_data) > 0) {
            if (identical(input$preview_language, "fr")) {
              safe_html_fragment_ui(text_data$text_fr[1])
            } else {
              safe_html_fragment_ui(text_data$text_en[1])
            }
          } else {
            tags$p("Text not found")
          }
        } else if (row$content_type == "image") {
          image_data <- DBI::dbGetQuery(
            session$userData$AquaCache,
            "SELECT * FROM application.images WHERE id = $1",
            params = list(row$content_id)
          )
          if (nrow(image_data) > 0) {
            data_url <- application_image_data_url(
              image_data$image[[1]],
              image_data$format[1]
            )
            if (is.null(data_url)) {
              tags$p("This image format cannot be previewed in the browser.")
            } else {
              tags$img(
                src = data_url,
                alt = row$content_id,
                style = "max-width: 100%; max-height: 480px; height: auto;"
              )
            }
          } else {
            tags$p("Image not found")
          }
        } else {
          tags$p("Unknown content type")
        }
      })
      do.call(tagList, ui_elements)
    })

    observeEvent(input$save_image, {
      req(input$image_file, input$image_id)
      image_id <- trimws(input$image_id)
      extension <- tolower(tools::file_ext(input$image_file$name))
      supported_extensions <- c(
        "jpg",
        "jpeg",
        "png",
        "gif",
        "webp",
        "heic",
        "heif"
      )
      if (!nzchar(image_id)) {
        showNotification("Enter an Image ID before saving.", type = "error")
        return()
      }
      if (!extension %in% supported_extensions) {
        showNotification("Choose a supported image file.", type = "error")
        return()
      }
      tryCatch(
        YGwater:::with_heif_upload_as_jpeg(
          input$image_file$datapath,
          function(upload_path) {
            size <- file.info(upload_path)$size
            if (is.na(size) || size < 1) {
              stop("The selected image file is empty.", call. = FALSE)
            }
            image_binary <- readBin(upload_path, what = "raw", n = size)
            image_format <- tolower(tools::file_ext(upload_path))
            if (image_format %in% c("jpg", "jpe")) {
              image_format <- "jpeg"
            }
            DBI::dbExecute(
              session$userData$AquaCache,
              "INSERT INTO application.images (id, image, format) VALUES ($1,$2,$3)",
              params = list(image_id, list(image_binary), image_format)
            )
            showNotification("Image saved.", type = "message")
            load_images()
            if (input$content_type == "image") {
              update_content_id_choices("image")
            }
          },
          extension = extension
        ),
        error = function(e) {
          showNotification(paste("Insert failed:", e$message), type = "error")
        }
      )
    })

    observeEvent(input$add_page_content, {
      req(input$page_select, input$content_id)
      if (!identical(input$page_select, loaded_page())) {
        showNotification(
          "Wait for the selected page to finish loading, then try again.",
          type = "error"
        )
        return()
      }
      if (
        length(input$content_position) != 1L ||
          is.na(input$content_position) ||
          !is.finite(input$content_position) ||
          input$content_position %% 1 != 0
      ) {
        showNotification(
          "Enter a whole number for the position.",
          type = "error"
        )
        return()
      }
      if (input$content_position <= reserved_position_max) {
        showNotification(
          "Positions 1-3 are reserved and cannot be edited here.",
          type = "error"
        )
        return()
      }
      df <- preview_data()
      if (input$content_position %in% df$position) {
        showNotification(
          sprintf(
            "Position %s is already in use. Choose another position.",
            input$content_position
          ),
          type = "error"
        )
        return()
      }
      new_row <- data.frame(
        page = input$page_select,
        position = as.integer(input$content_position),
        content_type = input$content_type,
        content_id = input$content_id,
        stringsAsFactors = FALSE
      )
      df <- rbind(df, new_row)
      df <- df[order(df$position), , drop = FALSE]
      preview_data(df)
    })

    observeEvent(input$delete_page_content, {
      req(input$page_table_rows_selected)
      df <- preview_data()
      display_indices <- which(df$position > reserved_position_max)
      display_order <- order(-df$position[display_indices])
      display_indices <- display_indices[display_order]
      rows_to_delete <- display_indices[input$page_table_rows_selected]
      df <- df[-rows_to_delete, , drop = FALSE]
      preview_data(df)
    })

    observeEvent(input$commit_page_content, {
      req(input$page_select)
      if (!identical(input$page_select, loaded_page())) {
        showNotification(
          "The selected page has not finished loading. Reload it and try again.",
          type = "error"
        )
        return()
      }
      df <- preview_data()
      editable_rows <- df[
        !is.na(df$position) & df$position > reserved_position_max,
        ,
        drop = FALSE
      ]
      saved <- tryCatch(
        {
          DBI::dbWithTransaction(session$userData$AquaCache, {
            DBI::dbExecute(
              session$userData$AquaCache,
              "DELETE FROM application.page_content WHERE page = $1 AND position > $2",
              params = list(input$page_select, reserved_position_max)
            )
            if (nrow(editable_rows) > 0L) {
              for (i in seq_len(nrow(editable_rows))) {
                DBI::dbExecute(
                  session$userData$AquaCache,
                  "INSERT INTO application.page_content (page, position, content_type, content_id) VALUES ($1,$2,$3,$4)",
                  params = list(
                    editable_rows$page[i],
                    editable_rows$position[i],
                    editable_rows$content_type[i],
                    editable_rows$content_id[i]
                  )
                )
              }
            }
          })
          TRUE
        },
        error = function(e) {
          showNotification(
            paste("Page layout was not saved:", e$message),
            type = "error"
          )
          FALSE
        }
      )
      if (saved) {
        load_pages(preferred_page = input$page_select)
        showNotification("Page layout saved.", type = "message")
      }
    })
  })
}
