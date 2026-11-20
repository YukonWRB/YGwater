#' UI for viewing forecaster commentary.
viewForecasterCommentsUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::page_fluid(
    YGwater:::shiny_ui_ready(ns("ui_ready")),
    shiny::uiOutput(ns("banner")),
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      shiny::selectizeInput(ns("author"), "Author", choices = NULL, multiple = TRUE),
      shiny::selectizeInput(ns("category"), "Category", choices = NULL, multiple = TRUE),
      shiny::selectizeInput(ns("location"), "Location", choices = NULL, multiple = TRUE),
      shiny::selectInput(ns("visibility"), "Visibility", choices = c("All", "Public", "Internal"))
    ),
    shiny::dateRangeInput(
      ns("date_range"), "Comment date range",
      start = Sys.Date() - 30, end = Sys.Date(),
      separator = " to "
    ),
    DT::DTOutput(ns("comments"))
  )
}

#' Server for viewing forecaster commentary.
viewForecasterComments <- function(id, language) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$banner <- shiny::renderUI({
      application_notifications_ui(
        ns = ns, lang = language$language,
        con = session$userData$AquaCache,
        module_id = "viewForecasterComments"
      )
    })

    comments_data <- shiny::reactive({
      tryCatch(
        DBI::dbGetQuery(session$userData$AquaCache, "
          SELECT
            c.id,
            c.timestamp::date AS comment_date,
            c.text_en AS comment,
            c.text_fr AS commentaire_fr,
            concat_ws(' ', a.first_name, a.last_name) AS author,
            c.raw_author,
            cc.name AS category,
            l.location_code AS location,
            l.name AS location_name,
            dt.document_type_en AS document_type,
            c.public,
            c.link
          FROM commentary.comments c
          LEFT JOIN commentary.authors a ON c.author_id = a.author_id
          LEFT JOIN commentary.comment_categories cc ON c.comment_category_id = cc.id
          LEFT JOIN public.locations l ON c.location_id = l.location_id
          LEFT JOIN files.document_types dt ON c.document_type_id = dt.document_type_id
          ORDER BY c.timestamp DESC, c.id DESC;
        "),
        error = function(e) {
          shiny::showNotification(
            "Forecaster commentary is not available on this database.",
            type = "warning", duration = 8
          )
          data.frame()
        }
      )
    })

    observe_choices <- shiny::observeEvent(list(input$ui_ready, comments_data()), {
      shiny::req(input$ui_ready)
      comments <- comments_data()
      if (!nrow(comments)) return()
      shiny::updateSelectizeInput(session, "author",
        choices = sort(unique(stats::na.omit(comments$author))), server = TRUE)
      shiny::updateSelectizeInput(session, "category",
        choices = sort(unique(stats::na.omit(comments$category))), server = TRUE)
      location_choices <- unique(stats::na.omit(ifelse(
        nzchar(comments$location_name),
        paste0(comments$location, " — ", comments$location_name),
        comments$location
      )))
      shiny::updateSelectizeInput(session, "location", choices = sort(location_choices), server = TRUE)
    }, ignoreInit = FALSE)

    filtered_comments <- shiny::reactive({
      comments <- comments_data()
      if (!nrow(comments)) return(comments)
      comments$location_label <- ifelse(
        !is.na(comments$location_name) & nzchar(comments$location_name),
        paste0(comments$location, " — ", comments$location_name), comments$location
      )
      if (length(input$author)) comments <- comments[comments$author %in% input$author, , drop = FALSE]
      if (length(input$category)) comments <- comments[comments$category %in% input$category, , drop = FALSE]
      if (length(input$location)) comments <- comments[comments$location_label %in% input$location, , drop = FALSE]
      if (identical(input$visibility, "Public")) comments <- comments[comments$public %in% TRUE, , drop = FALSE]
      if (identical(input$visibility, "Internal")) comments <- comments[!(comments$public %in% TRUE), , drop = FALSE]
      if (length(input$date_range) == 2L && all(!is.na(input$date_range))) {
        dates <- as.Date(comments$comment_date)
        comments <- comments[dates >= input$date_range[[1]] & dates <= input$date_range[[2]], , drop = FALSE]
      }
      comments
    })

    output$comments <- DT::renderDT({
      comments <- filtered_comments()
      if (nrow(comments)) {
        comments <- comments[, c("comment_date", "author", "category", "location_label", "document_type", "public", "comment", "commentaire_fr"), drop = FALSE]
        names(comments) <- c("Date", "Author", "Category", "Location", "Document type", "Public", "Comment", "Comment (French)")
      }
      DT::datatable(
        comments, rownames = FALSE, filter = "top",
        options = list(pageLength = 25, scrollX = TRUE, order = list(list(0, "desc")))
      )
    })
  })
}
