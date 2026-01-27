# UI and server code for main documents module

addDocsUI <- function(id) {
  ns <- NS(id)
  page_fluid(
    uiOutput(ns("banner")),
    uiOutput(ns("ui"))
  )
}

addDocs <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- renderUI({
      req(language$language)
      application_notifications_ui(
        ns = ns,
        lang = language$language,
        con = session$userData$AquaCache,
        module_id = "addDocs"
      )
    })

    parse_text_array <- function(value) {
      if (is.null(value) || !length(value) || all(is.na(value))) {
        return(character(0))
      }
      if (is.list(value)) {
        value <- unlist(value)
      }
      value <- as.character(value)
      if (!length(value)) {
        return(character(0))
      }
      cleaned <- gsub("[{}\"]", "", value)
      cleaned <- trimws(cleaned)
      cleaned <- cleaned[nzchar(cleaned)]
      if (!length(cleaned)) {
        return(character(0))
      }
      unique(trimws(unlist(strsplit(cleaned, ","))))
    }

    normalize_text_input <- function(value) {
      if (is.null(value) || !length(value)) {
        return(character(0))
      }
      value <- as.character(value)
      value <- value[nzchar(value)]
      unique(value)
    }

    parse_integer_input <- function(value) {
      if (is.null(value) || !length(value)) {
        return(integer(0))
      }
      value <- suppressWarnings(as.integer(value))
      value <- value[!is.na(value)]
      unique(value)
    }

    format_array_sql <- function(values, con) {
      if (is.null(values) || !length(values)) {
        return(DBI::SQL("NULL"))
      }
      glue::glue_sql("ARRAY[{values*}]", values = values, .con = con)
    }

    collect_column_values <- function(column) {
      if (is.null(column) || !length(column)) {
        return(character(0))
      }
      unique(unlist(lapply(column, parse_text_array)))
    }

    moduleData <- reactiveValues()
    selected_document_id <- reactiveVal(NULL)

    getModuleData <- function() {
      moduleData$doc_types <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT document_type_id, document_type_en FROM files.document_types ORDER BY document_type_en ASC;"
      )
      moduleData$docs <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT d.document_id, d.name, d.description, d.publish_date, d.url, d.authors, d.tags, d.share_with, d.type, dt.document_type_en FROM files.documents d LEFT JOIN files.document_types dt ON d.type = dt.document_type_id ORDER BY d.name ASC;"
      )
      moduleData$docs_display <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT d.document_id, d.name, dt.document_type_en AS type, d.publish_date FROM files.documents d LEFT JOIN files.document_types dt ON d.type = dt.document_type_id ORDER BY d.name ASC;"
      )
      moduleData$doc_geoms <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT document_id, geom_id FROM files.documents_spatial;"
      )
      moduleData$geoms <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT geom_id, geom_type, layer_name, feature_name, description FROM spatial.vectors ORDER BY layer_name ASC, feature_name ASC;"
      )
      moduleData$users <- DBI::dbGetQuery(
        session$userData$AquaCache,
        "SELECT * FROM public.get_shareable_principals_for('files.documents');"
      )
    }

    getModuleData()

    output$ui <- renderUI({
      req(
        moduleData$doc_types,
        moduleData$docs,
        moduleData$geoms,
        moduleData$users
      )

      geom_labels <- paste(
        moduleData$geoms$layer_name,
        moduleData$geoms$feature_name,
        sep = " - "
      )
      geom_choices <- stats::setNames(moduleData$geoms$geom_id, geom_labels)

      tagList(
        actionButton(
          ns("reload_module"),
          "Reload module data",
          icon = icon("refresh")
        ),
        radioButtons(
          ns("mode"),
          NULL,
          choices = c("Add new" = "add", "Modify existing" = "modify"),
          inline = TRUE
        ),
        conditionalPanel(
          condition = "input.mode == 'modify'",
          ns = ns,
          DT::DTOutput(ns("documents_table"))
        ),
        conditionalPanel(
          condition = "input.mode == 'add'",
          ns = ns,
          fileInput(
            ns("doc_file"),
            label = "Select document",
            multiple = FALSE
          )
        ),
        textInput(ns("doc_name"), label = "Document name"),
        selectizeInput(
          ns("doc_type"),
          "Document type",
          choices = moduleData$doc_types$document_type_en,
          multiple = TRUE,
          options = list(maxItems = 1, create = FALSE),
          width = "100%"
        ),
        textAreaInput(
          ns("doc_description"),
          label = "Description",
          width = "100%"
        ),
        textInput(
          ns("doc_authors"),
          label = "Author(s), separate by commas",
          width = "100%"
        ),
        selectizeInput(
          ns("doc_tags"),
          label = "Tag(s), separate by commas",
          width = "100%"
        ),
        dateInput(
          ns("doc_publish_date"),
          label = "Publish date",
          value = NULL
        ),
        textInput(ns("doc_url"), label = "URL/DOI"),
        selectizeInput(
          ns("doc_share_with"),
          label = "Share with groups",
          choices = moduleData$users$role_name,
          selected = "public_reader",
          multiple = TRUE,
          width = "100%"
        ),
        selectizeInput(
          ns("doc_geoms"),
          label = "Associate spatial feature(s)",
          choices = geom_choices,
          multiple = TRUE,
          width = "100%"
        ),
        actionButton(ns("clear_form"), label = "Clear form"),
        conditionalPanel(
          condition = "input.mode == 'add'",
          ns = ns,
          actionButton(ns("submit_add"), label = "Add document")
        ),
        conditionalPanel(
          condition = "input.mode == 'modify'",
          ns = ns,
          actionButton(ns("submit_modify"), label = "Update document")
        )
      )
    })

    observeEvent(input$reload_module, {
      getModuleData()
    })

    output$documents_table <- DT::renderDataTable({
      req(moduleData$docs_display)
      tbl <- moduleData$docs_display
      id_target <- which(names(tbl) == "document_id") - 1

      tbl$type <- as.factor(tbl$type)
      DT::datatable(
        tbl,
        rownames = FALSE,
        selection = list(mode = "single"),
        options = list(
          pageLength = 10,
          columnDefs = list(
            list(visible = FALSE, targets = c(id_target)) # Hide the ID column
          ),
          scrollX = TRUE,
          initComplete = htmlwidgets::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({",
            "  'background-color': '#079',",
            "  'color': '#fff',",
            "  'font-size': '90%',",
            "});",
            "$(this.api().table().body()).css({",
            "  'font-size': '80%',",
            "});",
            "}"
          )
        ),
        filter = 'top'
      )
    })

    observeEvent(input$documents_table_rows_selected, {
      req(moduleData$docs)
      if (is.null(input$documents_table_rows_selected)) {
        selected_document_id(NULL)
        return()
      }

      row_index <- input$documents_table_rows_selected
      if (length(row_index) != 1) {
        selected_document_id(NULL)
        return()
      }

      doc_id <- moduleData$docs_display$document_id[row_index]
      selected_document_id(doc_id)

      doc <- moduleData$docs[moduleData$docs$document_id == doc_id, ]
      if (nrow(doc) == 0) {
        return()
      }

      doc <- doc[1, ]
      type_value <- doc$document_type_en

      updateTextInput(session, "doc_name", value = doc$name)
      updateSelectizeInput(session, "doc_type", selected = type_value)
      updateTextAreaInput(
        session,
        "doc_description",
        value = doc$description
      )
      updateTextInput(
        session,
        "doc_authors",
        value = doc$authors
      )
      updateTextInput(
        session,
        "doc_tags",
        value = doc$tags
      )
      updateDateInput(
        session,
        "doc_publish_date",
        value = as.Date(doc$publish_date)
      )
      updateTextInput(
        session,
        "doc_url",
        value = ifelse(is.na(doc$url), "", doc$url)
      )
      updateSelectizeInput(
        session,
        "doc_share_with",
        selected = parse_text_array(doc$share_with)
      )

      doc_geom_ids <- moduleData$doc_geoms$geom_id[
        moduleData$doc_geoms$document_id == doc_id
      ]
      updateSelectizeInput(
        session,
        "doc_geoms",
        selected = as.character(doc_geom_ids)
      )
    })

    observeEvent(input$clear_form, {
      updateTextInput(session, "doc_name", value = "")
      updateSelectizeInput(session, "doc_type", selected = character(0))
      updateTextAreaInput(session, "doc_description", value = "")
      updateTextInput(session, "doc_authors", value = "")
      updateTextInput(session, "doc_tags", value = "")
      updateDateInput(session, "doc_publish_date", value = as.Date(NA))
      updateTextInput(session, "doc_url", value = "")
      updateSelectizeInput(
        session,
        "doc_share_with",
        selected = "public_reader"
      )
      updateSelectizeInput(session, "doc_geoms", selected = character(0))
      selected_document_id(NULL)
    })

    observeEvent(input$submit_add, {
      req(input$doc_file)
      req(input$doc_name, input$doc_type, input$doc_description)

      doc_path <- input$doc_file$datapath
      tags <- normalize_text_input(input$doc_tags)
      authors <- normalize_text_input(input$doc_authors)
      share_with <- normalize_text_input(input$doc_share_with)
      geoms <- parse_integer_input(input$doc_geoms)
      if (!length(share_with)) {
        share_with <- "public_reader"
      }
      publish_date_value <- input$doc_publish_date
      if (is.null(publish_date_value) || is.na(publish_date_value)) {
        publish_date_value <- NULL
      }

      tryCatch(
        {
          insertACDocument(
            path = doc_path,
            name = input$doc_name,
            type = input$doc_type,
            description = input$doc_description,
            tags = if (length(tags)) tags else NULL,
            authors = if (length(authors)) authors else NULL,
            publish_date = publish_date_value,
            url = if (nzchar(input$doc_url)) input$doc_url else NULL,
            share_with = share_with,
            geoms = if (length(geoms)) geoms else NULL,
            con = session$userData$AquaCache
          )
          showNotification("Document added successfully.", type = "message")
          getModuleData()
          updateTextInput(session, "doc_name", value = "")
          updateSelectizeInput(session, "doc_type", selected = character(0))
          updateTextAreaInput(session, "doc_description", value = "")
          updateTextInput(session, "doc_authors", selected = character(0))
          updateTextInput(session, "doc_tags", selected = character(0))
          updateDateInput(session, "doc_publish_date", value = as.Date(NA))
          updateTextInput(session, "doc_url", value = "")
          updateSelectizeInput(
            session,
            "doc_share_with",
            selected = "public_reader"
          )
          updateSelectizeInput(session, "doc_geoms", selected = character(0))
        },
        error = function(err) {
          showNotification(
            paste("Failed to add document:", err$message),
            type = "error"
          )
        }
      )
    })

    observeEvent(input$submit_modify, {
      doc_id <- selected_document_id()
      req(doc_id)
      req(input$doc_name, input$doc_type, input$doc_description)

      con <- session$userData$AquaCache
      type_id <- moduleData$doc_types$document_type_id[
        match(input$doc_type, moduleData$doc_types$document_type_en)
      ]
      if (is.na(type_id)) {
        showNotification(
          "Selected document type is not recognized.",
          type = "error"
        )
        return()
      }
      tags <- normalize_text_input(input$doc_tags)
      authors <- normalize_text_input(input$doc_authors)
      share_with <- normalize_text_input(input$doc_share_with)
      geoms <- parse_integer_input(input$doc_geoms)

      if (!length(share_with)) {
        share_with <- "public_reader"
      }

      publish_date_value <- input$doc_publish_date
      if (is.null(publish_date_value) || is.na(publish_date_value)) {
        publish_date_value <- DBI::SQL("NULL")
      } else {
        publish_date_value <- as.character(publish_date_value)
      }

      url_value <- input$doc_url
      if (is.null(url_value) || !nzchar(url_value)) {
        url_value <- DBI::SQL("NULL")
      }

      update_sql <- glue::glue_sql(
        "UPDATE files.documents
         SET name = {name},
             description = {description},
             type = {type_id},
             publish_date = {publish_date},
             url = {url_value},
             authors = {authors_sql},
             tags = {tags_sql},
             share_with = {share_with_sql}
         WHERE document_id = {doc_id};",
        name = input$doc_name,
        description = input$doc_description,
        type_id = type_id,
        publish_date = publish_date_value,
        url_value = url_value,
        authors_sql = format_array_sql(authors, con),
        tags_sql = format_array_sql(tags, con),
        share_with_sql = format_array_sql(share_with, con),
        doc_id = doc_id,
        .con = con
      )

      tryCatch(
        {
          DBI::dbExecute(con, update_sql)
          DBI::dbExecute(
            con,
            "DELETE FROM files.documents_spatial WHERE document_id = $1;",
            params = list(doc_id)
          )

          if (length(geoms)) {
            doc_spatial <- data.frame(
              document_id = doc_id,
              geom_id = geoms
            )
            DBI::dbAppendTable(
              con,
              DBI::Id(schema = "files", table = "documents_spatial"),
              doc_spatial
            )
          }

          showNotification("Document updated successfully.", type = "message")
          getModuleData()
        },
        error = function(err) {
          showNotification(
            paste("Failed to update document:", err$message),
            type = "error"
          )
        }
      )
    })
  }) # End of moduleServer
}
