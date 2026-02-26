# UI and server code for associating documents with boreholes/wells

manageBoreholeDocumentsUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    uiOutput(ns("banner")),
    tags$style(HTML(".modal-lg { width: 95% !important; max-width: 95% !important; }")),
    fluidRow(
      column(
        4,
        actionButton(ns("reload"), "Reload", icon = icon("refresh")),
        selectizeInput(
          ns("borehole_id"),
          "Select borehole/well",
          choices = NULL,
          multiple = FALSE,
          options = list(placeholder = "Choose a borehole/well")
        ),
        uiOutput(ns("preview")),
        h5("Add existing document association"),
        actionButton(
          ns("open_doc_selector"),
          "Find and select existing document",
          icon = icon("table")
        ),
        tags$hr(),
        h5("Upload and associate new document"),
        fileInput(ns("doc_file"), "Document file", multiple = FALSE),
        textInput(ns("doc_name"), "Document name"),
        selectizeInput(ns("doc_type"), "Document type", choices = NULL, multiple = FALSE),
        textAreaInput(ns("doc_description"), "Description", width = "100%"),
        textInput(ns("doc_authors"), "Authors (comma separated)"),
        textInput(ns("doc_tags"), "Tags (comma separated)"),
        dateInput(ns("doc_publish_date"), "Publish date", value = NULL),
        textInput(ns("doc_url"), "URL/DOI"),
        selectizeInput(
          ns("doc_share_with"),
          "Share with groups",
          choices = NULL,
          multiple = TRUE,
          selected = "public_reader"
        ),
        actionButton(ns("upload_and_associate"), "Upload + associate", class = "btn-primary")
      ),
      column(
        8,
        h4("Associated documents"),
        downloadButton(ns("download_doc"), "Download selected document"),
        DT::DTOutput(ns("associated_docs"))
      )
    )
  )
}

manageBoreholeDocuments <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "manageBoreholeDocuments"
      )
    })

    moduleData <- reactiveValues()
    docs_refresh <- reactiveVal(0L)

    split_comma_text <- function(value) {
      if (is.null(value) || !length(value) || all(is.na(value))) {
        return(character(0))
      }
      value <- gsub("[{}\"]", "", as.character(value))
      value <- trimws(value)
      value <- value[nzchar(value)]
      if (!length(value)) return(character(0))
      unique(trimws(unlist(strsplit(value, ","))))
    }

    format_array <- function(values) {
      if (is.null(values)) {
        return(NA_character_)
      }
      if (is.list(values)) {
        return(vapply(
          values,
          function(value) {
            if (is.null(value) || length(value) == 0) {
              return(NA_character_)
            }
            paste(value, collapse = ", ")
          },
          character(1)
        ))
      }
      if (all(is.na(values))) {
        return(values)
      }
      cleaned <- gsub("[{}\"]", "", values)
      cleaned <- trimws(cleaned)
      vapply(
        strsplit(cleaned, ","),
        function(value) paste(trimws(value), collapse = ", "),
        character(1)
      )
    }

    load_data <- function() {
      moduleData$boreholes <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT b.borehole_id, b.borehole_name,
                EXISTS(SELECT 1 FROM boreholes.wells w WHERE w.borehole_id = b.borehole_id) AS is_well
         FROM boreholes.boreholes b
         ORDER BY b.borehole_name ASC;"
      )
      moduleData$all_documents <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT d.document_id,
                d.name,
                d.description,
                d.publish_date,
                d.tags,
                d.authors,
                d.url,
                d.format,
                d.type,
                dt.document_type_en,
                octet_length(d.document) AS document_size_bytes
         FROM files.documents d
         LEFT JOIN files.document_types dt ON dt.document_type_id = d.type
         ORDER BY d.publish_date DESC NULLS LAST, d.name ASC;"
      )
      moduleData$document_types <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT document_type_id, document_type_en FROM files.document_types ORDER BY document_type_en ASC;"
      )
      moduleData$share_principals <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT role_name FROM public.get_shareable_principals_for('files.documents');"
      )
    }

    load_data()

    associated_docs <- reactive({
      req(input$borehole_id)
      docs_refresh()
      DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT bd.borehole_id,
                d.document_id,
                d.name,
                dt.document_type_en,
                d.publish_date,
                d.format,
                d.url,
                d.description,
                d.tags,
                d.authors,
                d.document
         FROM boreholes.boreholes_documents bd
         JOIN files.documents d ON d.document_id = bd.document_id
         LEFT JOIN files.document_types dt ON dt.document_type_id = d.type
         WHERE bd.borehole_id = $1
         ORDER BY d.name ASC;",
        params = list(as.integer(input$borehole_id))
      )
    })

    selectable_documents <- reactive({
      req(moduleData$all_documents, input$borehole_id)
      docs <- data.table::as.data.table(moduleData$all_documents)
      linked <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT document_id FROM boreholes.boreholes_documents WHERE borehole_id = $1;",
        params = list(as.integer(input$borehole_id))
      )
      if (nrow(linked)) {
        docs <- docs[!document_id %in% linked$document_id]
      }
      docs[, authors := format_array(authors)]
      docs[, tags := format_array(tags)]
      docs
    })

    observe({
      req(moduleData$boreholes)
      borehole_labels <- sprintf(
        "%s%s (ID %s)",
        moduleData$boreholes$borehole_name,
        ifelse(moduleData$boreholes$is_well, " [well]", ""),
        moduleData$boreholes$borehole_id
      )
      updateSelectizeInput(
        session,
        "borehole_id",
        choices = stats::setNames(moduleData$boreholes$borehole_id, borehole_labels),
        selected = isolate(input$borehole_id),
        server = TRUE
      )

      updateSelectizeInput(
        session,
        "doc_type",
        choices = stats::setNames(moduleData$document_types$document_type_en, moduleData$document_types$document_type_en)
      )
      updateSelectizeInput(
        session,
        "doc_share_with",
        choices = moduleData$share_principals$role_name,
        selected = c("public_reader")
      )
    })

    output$associated_docs <- DT::renderDT({
      docs <- associated_docs()
      display <- docs[, c(
        "document_id", "name", "document_type_en", "publish_date", "format", "url"
      )]
      display$remove <- sprintf("<button class='btn btn-sm btn-danger' onclick=\"Shiny.setInputValue('%s', %s, {priority: 'event'})\">Remove</button>", ns("remove_doc_id"), display$document_id)

      DT::datatable(
        display,
        escape = FALSE,
        rownames = FALSE,
        options = list(pageLength = 8, scrollX = TRUE)
      )
    })

    output$preview <- renderUI({
      docs <- associated_docs()
      if (!nrow(docs)) {
        return(tags$p("No associated documents yet."))
      }
      first <- docs[1, ]
      if (tolower(first$format) %in% c("png", "jpg", "jpeg", "gif", "webp")) {
        mime <- paste0("image/", ifelse(tolower(first$format) == "jpg", "jpeg", tolower(first$format)))
        raw <- first$document[[1]]
        encoded <- openssl::base64_encode(raw)
        return(tagList(
          tags$p(sprintf("Previewing: %s", first$name)),
          tags$img(src = sprintf("data:%s;base64,%s", mime, encoded), style = "max-width: 100%; border: 1px solid #ddd;")
        ))
      }
      tags$p("Thumbnail preview is currently available for image documents only.")
    })

    selected_doc_id <- reactive({
      idx <- input$associated_docs_rows_selected
      req(length(idx) == 1)
      docs <- associated_docs()
      req(nrow(docs) >= idx)
      as.integer(docs$document_id[idx])
    })

    output$download_doc <- downloadHandler(
      filename = function() {
        doc_id <- selected_doc_id()
        doc <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT name, format FROM files.documents WHERE document_id = $1;",
          params = list(doc_id)
        )
        req(nrow(doc) == 1)
        paste0(gsub("[^A-Za-z0-9_-]", "_", doc$name), ".", doc$format)
      },
      content = function(file) {
        doc_id <- selected_doc_id()
        doc <- DBI::dbGetQuery(
          session$userData$AquaCache,
          "SELECT document FROM files.documents WHERE document_id = $1;",
          params = list(doc_id)
        )
        req(nrow(doc) == 1)
        writeBin(doc$document[[1]], file)
      }
    )

    output$existing_docs_selector_table <- DT::renderDT({
      docs <- selectable_documents()
      DT::datatable(
        docs[, .(
          document_id,
          name,
          document_type_en,
          publish_date,
          format,
          authors,
          tags,
          description,
          url
        )],
        rownames = FALSE,
        selection = "single",
        filter = "top",
        options = list(pageLength = 12, scrollX = TRUE)
      )
    })

    observeEvent(input$open_doc_selector, {
      req(input$borehole_id)
      showModal(modalDialog(
        title = "Select an existing document to associate",
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("associate_selected_doc"), "Associate selected document", class = "btn-primary")
        ),
        DT::DTOutput(ns("existing_docs_selector_table"))
      ))
    })

    observeEvent(input$associate_selected_doc, {
      req(input$borehole_id)
      idx <- input$existing_docs_selector_table_rows_selected
      req(length(idx) == 1)
      docs <- selectable_documents()
      req(nrow(docs) >= idx)
      selected_doc <- as.integer(docs$document_id[idx])

      tryCatch(
        {
          DBI::dbExecute(
            session$userData$AquaCache,
            "INSERT INTO boreholes.boreholes_documents (borehole_id, document_id)
             VALUES ($1, $2)
             ON CONFLICT DO NOTHING;",
            params = list(as.integer(input$borehole_id), selected_doc)
          )
          removeModal()
          docs_refresh(isolate(docs_refresh()) + 1L)
          showNotification("Document association added.", type = "message")
        },
        error = function(e) {
          showNotification(paste("Failed to add association:", e$message), type = "error")
        }
      )
    })

    referenced_elsewhere <- function(doc_id) {
      fk_tables <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT ns.nspname AS schema_name,
                cls.relname AS table_name,
                att.attname AS column_name
         FROM pg_constraint con
         JOIN pg_class cls ON cls.oid = con.conrelid
         JOIN pg_namespace ns ON ns.oid = cls.relnamespace
         JOIN unnest(con.conkey) WITH ORDINALITY AS cols(attnum, ord) ON TRUE
         JOIN pg_attribute att ON att.attrelid = con.conrelid AND att.attnum = cols.attnum
         WHERE con.contype = 'f'
           AND con.confrelid = 'files.documents'::regclass;"
      )
      total_refs <- 0L
      if (nrow(fk_tables)) {
        for (i in seq_len(nrow(fk_tables))) {
          q <- paste0(
            "SELECT COUNT(*) AS n FROM ",
            DBI::dbQuoteIdentifier(session$userData$AquaCache, fk_tables$schema_name[i]),
            ".",
            DBI::dbQuoteIdentifier(session$userData$AquaCache, fk_tables$table_name[i]),
            " WHERE ",
            DBI::dbQuoteIdentifier(session$userData$AquaCache, fk_tables$column_name[i]),
            " = $1;"
          )
          n <- DBI::dbGetQuery(
            session$userData$AquaCache,
            q,
            params = list(as.integer(doc_id))
          )$n[[1]]
          total_refs <- total_refs + as.integer(n)
        }
      }

      arr_cols <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT table_schema, table_name, column_name
         FROM information_schema.columns
         WHERE udt_name = '_int4'
           AND column_name = 'documents';"
      )
      if (nrow(arr_cols)) {
        for (i in seq_len(nrow(arr_cols))) {
          q <- paste0(
            "SELECT COUNT(*) AS n FROM ",
            DBI::dbQuoteIdentifier(session$userData$AquaCache, arr_cols$table_schema[i]),
            ".",
            DBI::dbQuoteIdentifier(session$userData$AquaCache, arr_cols$table_name[i]),
            " WHERE $1 = ANY(",
            DBI::dbQuoteIdentifier(session$userData$AquaCache, arr_cols$column_name[i]),
            ");"
          )
          n <- DBI::dbGetQuery(
            session$userData$AquaCache,
            q,
            params = list(as.integer(doc_id))
          )$n[[1]]
          total_refs <- total_refs + as.integer(n)
        }
      }
      total_refs
    }

    observeEvent(input$remove_doc_id, {
      req(input$borehole_id, input$remove_doc_id)
      borehole_id <- as.integer(input$borehole_id)
      doc_id <- as.integer(input$remove_doc_id)
      tryCatch(
        {
          DBI::dbExecute(session$userData$AquaCache, "BEGIN")
          DBI::dbExecute(
            session$userData$AquaCache,
            "DELETE FROM boreholes.boreholes_documents
             WHERE borehole_id = $1 AND document_id = $2;",
            params = list(borehole_id, doc_id)
          )

          if (referenced_elsewhere(doc_id) == 0L) {
            DBI::dbExecute(
              session$userData$AquaCache,
              "DELETE FROM files.documents WHERE document_id = $1;",
              params = list(doc_id)
            )
            showNotification(
              "Association removed and document deleted (no remaining references).",
              type = "message"
            )
          } else {
            showNotification("Association removed.", type = "message")
          }
          DBI::dbExecute(session$userData$AquaCache, "COMMIT")
          load_data()
          docs_refresh(isolate(docs_refresh()) + 1L)
        },
        error = function(e) {
          DBI::dbExecute(session$userData$AquaCache, "ROLLBACK")
          showNotification(paste("Failed to remove association:", e$message), type = "error")
        }
      )
    })

    observeEvent(input$upload_and_associate, {
      req(input$borehole_id, input$doc_file, input$doc_name, input$doc_type, input$doc_description)

      doc_bytes <- readBin(input$doc_file$datapath, "raw", n = file.info(input$doc_file$datapath)$size)
      doc_hash <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT md5($1::bytea) AS md5;",
        params = list(doc_bytes)
      )$md5[[1]]

      existing <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT document_id
         FROM files.documents
         WHERE md5(document) = $1
         LIMIT 1;",
        params = list(doc_hash)
      )

      tags <- split_comma_text(input$doc_tags)
      authors <- split_comma_text(input$doc_authors)
      share_with <- split_comma_text(input$doc_share_with)
      if (!length(share_with)) share_with <- "public_reader"

      tryCatch(
        {
          DBI::dbExecute(session$userData$AquaCache, "BEGIN")
          if (nrow(existing) > 0) {
            document_id <- existing$document_id[[1]]
          } else {
            AquaCache::insertACDocument(
              path = input$doc_file$datapath,
              name = input$doc_name,
              type = input$doc_type,
              description = input$doc_description,
              tags = if (length(tags)) tags else NULL,
              authors = if (length(authors)) authors else NULL,
              publish_date = if (is.null(input$doc_publish_date) || is.na(input$doc_publish_date)) NULL else as.Date(input$doc_publish_date),
              url = if (nzchar(trimws(input$doc_url))) input$doc_url else NULL,
              share_with = share_with,
              geoms = NULL,
              con = session$userData$AquaCache
            )
            type_id <- moduleData$document_types$document_type_id[
              match(input$doc_type, moduleData$document_types$document_type_en)
            ]
            new_doc <- DBI::dbGetQuery(
              session$userData$AquaCache,
              "SELECT document_id
               FROM files.documents
               WHERE name = $1 AND type = $2
               ORDER BY document_id DESC
               LIMIT 1;",
              params = list(input$doc_name, type_id)
            )
            req(nrow(new_doc) == 1)
            document_id <- new_doc$document_id[[1]]
          }

          DBI::dbExecute(
            session$userData$AquaCache,
            "INSERT INTO boreholes.boreholes_documents (borehole_id, document_id)
             VALUES ($1, $2)
             ON CONFLICT DO NOTHING;",
            params = list(as.integer(input$borehole_id), as.integer(document_id))
          )

          DBI::dbExecute(session$userData$AquaCache, "COMMIT")
          load_data()
          docs_refresh(isolate(docs_refresh()) + 1L)
          showNotification("Document associated with borehole/well.", type = "message")
        },
        error = function(e) {
          DBI::dbExecute(session$userData$AquaCache, "ROLLBACK")
          showNotification(paste("Upload/association failed:", e$message), type = "error")
        }
      )
    })

    observeEvent(input$reload, {
      load_data()
      docs_refresh(isolate(docs_refresh()) + 1L)
    })
  })
}
