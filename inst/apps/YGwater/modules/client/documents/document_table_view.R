# UI and server code for documents table view

docTableViewUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML(sprintf(
      "
      /* Force DT table background to white (scoped to this module/table) */
      #%s table.dataTable,
      #%s table.dataTable tbody,
      #%s table.dataTable tbody tr,
      #%s table.dataTable tbody td {
        background-color: #FFFFFF !important;
      }

      /* keep filters at top white too */
      #%s table.dataTable thead tr,
      #%s table.dataTable thead th,
      #%s table.dataTable thead td {
        background-color: #FFFFFF !important;
      }
      ",
      ns("documents_table"),
      ns("documents_table"),
      ns("documents_table"),
      ns("documents_table"),
      ns("documents_table"),
      ns("documents_table"),
      ns("documents_table")
    ))),
    uiOutput(ns("page"))
  )
}

docTableView <- function(id, language) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    moduleData <- reactiveValues(
      docs = dbGetQueryDT(
        session$userData$AquaCache,
        paste0(
          "SELECT d.document_id, d.name, d.publish_date, d.description, ",
          "d.format, d.url, d.authors, d.tags, ",
          "dt.document_type_en, dt.document_type_fr, ",
          "o.name AS owner_name, c.name AS contributor_name ",
          "FROM files.documents d ",
          "LEFT JOIN files.document_types dt ON d.type = dt.document_type_id ",
          "LEFT JOIN public.organizations o ON d.owner = o.organization_id ",
          "LEFT JOIN public.organizations c ON d.contributor = c.organization_id ",
          "ORDER BY d.publish_date DESC NULLS LAST, d.name ASC;"
        )
      )
    )

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

    table_data <- reactive({
      req(moduleData$docs, language$language)

      type_col <- tr("document_type_col", language$language)

      tbl <- data.table::copy(moduleData$docs)
      tbl[, type := .SD[[type_col]], .SDcols = type_col]
      tbl[, authors := format_array(authors)]
      tbl[, tags := format_array(tags)]
      tbl[, publish_date := as.Date(publish_date)]
      tbl[, owner := owner_name]
      tbl[, contributor := contributor_name]

      tbl <- tbl[, .(
        document_id,
        name,
        type,
        authors,
        publish_date,
        description,
        format,
        url,
        owner,
        contributor,
        tags
      )]

      tbl[, sort_date := publish_date]
      data.table::setorder(tbl, -sort_date, name)
      tbl
    })

    selected_document_id <- reactiveVal(NULL)

    observeEvent(input$documents_table_rows_selected, {
      tbl <- table_data()
      if (
        !is.null(input$documents_table_rows_selected) &&
          length(input$documents_table_rows_selected) == 1 &&
          nrow(tbl) >= input$documents_table_rows_selected
      ) {
        selected_document_id(tbl$document_id[
          input$documents_table_rows_selected
        ])
      } else {
        selected_document_id(NULL)
      }
    })

    output$page <- renderUI({
      tagList(
        layout_sidebar(
          sidebar = sidebar(
            width = "35%",
            bg = config$sidebar_bg,
            open = TRUE,
            fillable = TRUE,
            fillable_mobile = TRUE,
            h4(tr("documents_heading", language$language)),
            DT::dataTableOutput(ns("documents_table")),
            downloadButton(
              ns("download_document"),
              tr("document_download", language$language)
            ),
            uiOutput(ns("document_details"))
          ),
          uiOutput(ns("document_preview")),
          height = "1000px"
        )
      )
    }) %>%
      bindEvent(language$language)

    output$documents_table <- DT::renderDataTable({
      tbl <- table_data()

      col_labels <- c(
        document_id = "document_id",
        name = tr("document_name", language$language),
        type = tr("document_type", language$language),
        authors = tr("document_authors", language$language),
        publish_date = tr("document_published", language$language),
        description = tr("document_description", language$language),
        format = tr("document_format", language$language),
        url = tr("document_url", language$language),
        owner = tr("document_owner", language$language),
        contributor = tr("document_contributor", language$language),
        tags = tr("document_tags", language$language),
        sort_date = "sort_date"
      )

      visible_cols <- names(tbl)
      col_names <- unname(col_labels[visible_cols])

      date_targets <- which(names(tbl) == "publish_date") - 1
      id_target <- which(names(tbl) == "document_id") - 1
      sort_target <- which(names(tbl) == "sort_date") - 1
      name_target <- which(names(tbl) == "name") - 1

      DT::datatable(
        tbl,
        rownames = FALSE,
        selection = list(mode = "single"),
        options = list(
          pageLength = 10,
          lengthMenu = c(5, 10, 20, 50),
          columnDefs = list(
            list(visible = FALSE, targets = c(id_target, sort_target)),
            list(
              targets = date_targets,
              render = htmlwidgets::JS(
                "function(data, type, row){\
                  if (!data) return '';\
                  var d = new Date(data);\
                  if (type === 'display') return d.toISOString().substring(0,10);\
                  return data;\
                }"
              )
            ),
            list(
              targets = name_target,
              render = htmlwidgets::JS(
                "function(data, type, row, meta) {\
                  return type === 'display' && data !== null && data.length > 35 ?\
                  '<span title=\"' + data + '\">' + data.substr(0, 35) + '...</span>' : data;\
                }"
              )
            )
          ),
          order = list(
            list(sort_target, "desc"),
            list(name_target, "asc")
          ),
          language = list(
            info = tr("tbl_info", language$language),
            infoEmpty = tr("tbl_info_empty", language$language),
            paginate = list(previous = "", `next` = ""),
            search = tr("tbl_search", language$language),
            lengthMenu = tr("tbl_length", language$language),
            infoFiltered = tr("tbl_filtered", language$language),
            zeroRecords = tr("tbl_zero", language$language)
          ),
          scrollX = TRUE,
          initComplete = htmlwidgets::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({",
            "  'font-size': '90%',",
            "});",
            "$(this.api().table().body()).css({",
            "  'font-size': '80%',",
            "});",
            "}"
          )
        ),
        colnames = col_names,
        filter = "top"
      )
    })

    selected_document_details <- reactive({
      tbl <- table_data()
      selected_id <- selected_document_id()
      if (is.null(selected_id) || nrow(tbl) == 0) {
        return(NULL)
      }
      tbl[document_id == selected_id][1]
    })

    output$document_details <- renderUI({
      doc <- selected_document_details()
      if (is.null(doc)) {
        return(
          div(
            class = "text-muted",
            tr("document_select_prompt", language$language)
          )
        )
      }

      url_block <- if (!is.null(doc$url) && nzchar(doc$url)) {
        tags$a(
          href = doc$url,
          target = "_blank",
          rel = "noopener",
          tr("document_open_url", language$language)
        )
      } else {
        NULL
      }

      tagList(
        tags$hr(),
        tags$strong(tr("document_name", language$language)),
        tags$p(doc$name),
        tags$strong(tr("document_type", language$language)),
        tags$p(doc$type),
        tags$strong(tr("document_authors", language$language)),
        tags$p(ifelse(is.na(doc$authors), "", doc$authors)),
        tags$strong(tr("document_published", language$language)),
        tags$p(ifelse(is.na(doc$publish_date), "", doc$publish_date)),
        tags$strong(tr("document_description", language$language)),
        tags$p(doc$description),
        tags$strong(tr("document_format", language$language)),
        tags$p(doc$format),
        tags$strong(tr("document_owner", language$language)),
        tags$p(ifelse(is.na(doc$owner), "", doc$owner)),
        tags$strong(tr("document_contributor", language$language)),
        tags$p(ifelse(is.na(doc$contributor), "", doc$contributor)),
        tags$strong(tr("document_tags", language$language)),
        tags$p(ifelse(is.na(doc$tags), "", doc$tags)),
        url_block
      )
    })

    document_content <- reactive({
      selected_id <- selected_document_id()
      if (is.null(selected_id)) {
        return(NULL)
      }

      doc <- DBI::dbGetQuery(
        session$userData$AquaCache,
        paste0(
          "SELECT name, format, document FROM files.documents ",
          "WHERE document_id = ",
          selected_id,
          ";"
        )
      )

      if (nrow(doc) != 1) {
        return(NULL)
      }

      doc
    })

    guess_content_type <- function(format) {
      format <- tolower(format)
      switch(
        format,
        pdf = "application/pdf",
        png = "image/png",
        jpg = "image/jpeg",
        jpeg = "image/jpeg",
        gif = "image/gif",
        tif = "image/tiff",
        tiff = "image/tiff",
        "application/octet-stream"
      )
    }

    output$document_preview <- renderUI({
      doc <- document_content()
      if (is.null(doc)) {
        return(
          div(
            class = "text-muted",
            h4(tr("document_preview_heading", language$language)),
            tr("document_select_prompt", language$language)
          )
        )
      }

      format <- doc$format
      content_type <- guess_content_type(format)
      raw_content <- doc$document[[1]]
      data_uri <- paste0(
        "data:",
        content_type,
        ";base64,",
        base64enc::base64encode(raw_content)
      )

      preview <- if (content_type == "application/pdf") {
        tags$iframe(
          src = data_uri,
          style = "width: 100%; height: 900px; border: none;"
        )
      } else if (startsWith(content_type, "image/")) {
        tags$img(
          src = data_uri,
          style = "max-width: 100%; height: auto;"
        )
      } else {
        div(
          class = "text-muted",
          tr("document_no_preview", language$language)
        )
      }

      tagList(
        h4(tr("document_preview_heading", language$language)),
        preview
      )
    })

    output$download_document <- downloadHandler(
      filename = function() {
        doc <- document_content()
        if (is.null(doc)) {
          return("document")
        }
        format <- doc$format
        name <- gsub("[^A-Za-z0-9_-]", "_", doc$name)
        paste0(name, ".", format)
      },
      content = function(file) {
        doc <- document_content()
        if (is.null(doc)) {
          return(NULL)
        }
        writeBin(doc$document[[1]], file)
      }
    )
  })
}
