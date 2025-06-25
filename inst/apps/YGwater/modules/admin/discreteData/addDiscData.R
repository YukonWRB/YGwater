# UI and server code for adding discrete measurements

addDiscDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(
      HTML(sprintf("#%s.accordion {--bs-accordion-bg:#FFFCF5;--bs-accordion-btn-bg:#FBE5B2;--bs-accordion-active-bg:#FBE5B2;}", ns("accordion1"))),
      HTML(sprintf("#%s.accordion {--bs-accordion-bg:#E5F4F6;--bs-accordion-btn-bg:#0097A9;--bs-accordion-active-bg:#0097A9;}", ns("accordion2")))
    ),
    page_fluid(
      accordion(
        id = ns("accordion1"),
        open = "loc_panel",
        accordion_panel(
          id = ns("loc_panel"),
          title = "Location",
          selectInput(ns("location"), "Location", choices = NULL),
          selectInput(ns("sublocation"), "Sub-location", choices = NULL)
        )
      ),
      accordion(
        id = ns("accordion2"),
        open = "data_panel",
        accordion_panel(
          id = ns("data_panel"),
          title = "New data",
          radioButtons(ns("entry_mode"), "Input method", choices = c(File = "file", Manual = "manual"), inline = TRUE),
          conditionalPanel(
            condition = "input.entry_mode == 'file'",
            ns = ns,
            fileInput(ns("file"), "Upload .csv or Excel", accept = c(".csv", ".xls", ".xlsx")),
            selectInput(ns("mapping_select"), "Column mapping", choices = NULL),
            textInput(ns("mapping_name"), "New mapping name"),
            uiOutput(ns("mapping_ui")),
            actionButton(ns("save_mapping"), "Save mapping")
          ),
          conditionalPanel(
            condition = "input.entry_mode == 'manual'",
            ns = ns,
            actionButton(ns("add_row"), "Add row")
          ),
          DT::DTOutput(ns("data_table")),
          fileInput(ns("attach_docs"), "Attach documents", multiple = TRUE),
          actionButton(ns("upload"), "Upload to AquaCache")
        )
      )
    )
  )
}

addDiscData <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    loadMappings <- function(con) {
      if (!DBI::dbExistsTable(con, DBI::Id(schema = "application", table = "discrete_mappings"))) {
        return(list())
      }
      df <- DBI::dbGetQuery(con, "SELECT mapping_name, mapping FROM application.discrete_mappings")
      lapply(stats::setNames(df$mapping, df$mapping_name), jsonlite::fromJSON)
    }

    saveMappingDB <- function(con, name, mapping) {
      DBI::dbExecute(
        con,
        paste(
          "INSERT INTO application.discrete_mappings (mapping_name, mapping)",
          "VALUES ($1, $2)",
          "ON CONFLICT (mapping_name) DO UPDATE",
          "SET mapping = EXCLUDED.mapping,",
          "    modified = now(),",
          "    modified_by = current_user"),
        params = list(name, jsonlite::toJSON(mapping, auto_unbox = TRUE))
      )
    }

    mappings <- reactiveVal(loadMappings(session$userData$AquaCache))

    observe({
      updateSelectInput(session, "mapping_select", choices = c("", names(mappings())))
    })

    check <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT has_table_privilege(current_user, 'discrete.results', 'INSERT') AS can_insert"
    )
    if (!check$can_insert) {
      showModal(modalDialog(
        title = 'Insufficient Privileges',
        'You do not have write privileges to add measurements.',
        easyClose = TRUE,
        footer = modalButton('Close')
      ))
      shinyjs::disable('upload')
    }

    params <- reactive({
      dbGetQueryDT(session$userData$AquaCache,
                   "SELECT parameter_id, param_name FROM public.parameters ORDER BY param_name")
    })

    locations <- DBI::dbGetQuery(session$userData$AquaCache,
                                 "SELECT location_id, name FROM public.locations ORDER BY name")
    sub_locations <- DBI::dbGetQuery(session$userData$AquaCache,
                                     "SELECT sub_location_id, sub_location_name FROM public.sub_locations ORDER BY sub_location_name")

    updateSelectInput(session, "location",
                      choices = stats::setNames(locations$location_id, locations$name))
    updateSelectInput(session, "sublocation",
                      choices = c("" = "", stats::setNames(sub_locations$sub_location_id, sub_locations$sub_location_name)))

    file_data <- reactiveVal(NULL)

    observeEvent(input$file, {
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      df <- if (ext %in% c('xls', 'xlsx')) {
        openxlsx::read.xlsx(input$file$datapath)
      } else {
        readr::read_csv(input$file$datapath, show_col_types = FALSE)
      }
      file_data(df)
    })

    output$mapping_ui <- renderUI({
      req(file_data())
      df <- file_data()
      current <- mappings()[[input$mapping_select]]
      if (is.null(current)) current <- list(datetime = NULL, params = list())
      tagList(
        selectInput(ns("map_datetime"), "Datetime column", choices = names(df), selected = current$datetime),
        lapply(names(df), function(col) {
          if (!is.null(current$datetime) && identical(col, current$datetime)) return(NULL)
          selectInput(ns(paste0("param_", col)), sprintf("Parameter for column '%s'", col),
                      choices = stats::setNames(params()$parameter_id, params()$param_name),
                      selected = current$params[[col]])
        })
      )
    })

    observeEvent(input$save_mapping, {
      req(input$mapping_name, file_data())
      df <- file_data()
      param_cols <- setdiff(names(df), input$map_datetime)
      map_list <- list(datetime = input$map_datetime,
                       params = setNames(lapply(param_cols, function(col) input[[paste0('param_', col)]]), param_cols))
      m <- mappings()
      m[[input$mapping_name]] <- map_list
      mappings(m)
      saveMappingDB(session$userData$AquaCache, input$mapping_name, map_list)
      updateSelectInput(session, "mapping_select", choices = c("", names(mappings())), selected = input$mapping_name)
    })

    data <- reactiveValues(df = data.frame(sample = integer(), datetime = as.POSIXct(character()), parameter_id = integer(), value = numeric()))

    observeEvent(input$add_row, {
      next_sample <- ifelse(nrow(data$df) == 0, 1, max(data$df$sample) + 1)
      data$df <- rbind(data$df, data.frame(sample = next_sample, datetime = Sys.time(), parameter_id = NA_integer_, value = NA_real_))
    })

    observeEvent({input$map_datetime; input$save_mapping; input$mapping_select; file_data()}, {
      req(file_data())
      if (is.null(input$map_datetime)) return()
      df <- file_data()
      mapping <- if (nzchar(input$mapping_select)) mappings()[[input$mapping_select]] else NULL
      if (!is.null(mapping)) {
        param_cols <- names(mapping$params)
        df$row_id <- seq_len(nrow(df))
        long <- tidyr::pivot_longer(df, cols = tidyselect::all_of(param_cols), names_to = "pcol", values_to = "value")
        long$parameter_id <- unlist(mapping$params[long$pcol])
        data$df <- data.frame(sample = long$row_id,
                              datetime = as.POSIXct(long[[mapping$datetime]]),
                              parameter_id = long$parameter_id,
                              value = long$value)
      }
    }, ignoreInit = TRUE)

    output$data_table <- DT::renderDT({
      DT::datatable(data$df, editable = TRUE, options = list(scrollX = TRUE))
    }, server = FALSE)

    observeEvent(input$data_table_cell_edit, {
      info <- input$data_table_cell_edit
      data$df[info$row, info$col] <- info$value
    })

    insertDoc <- function(file) {
      bin <- readBin(file$datapath, "raw", file.info(file$datapath)$size)
      ext <- tools::file_ext(file$name)
      id <- DBI::dbGetQuery(session$userData$AquaCache,
                            "INSERT INTO files.documents (name, type, description, format, document) VALUES ($1, 1, $2, $3, $4) RETURNING document_id",
                            params = list(file$name, paste('Uploaded', file$name), ext, list(bin)))$document_id
      return(id)
    }

    observeEvent(input$upload, {
      if (is.null(input$location)) {
        showNotification('Please select a location.', type = 'error')
        return()
      }
      if (nrow(data$df) == 0) {
        showNotification('Empty data table!', type = 'error')
        return()
      }
      tryCatch({
        con <- session$userData$AquaCache
        doc_ids <- integer()
        if (!is.null(input$file)) {
          doc_ids <- c(doc_ids, insertDoc(input$file))
        }
        if (!is.null(input$attach_docs)) {
          for (ii in seq_len(nrow(input$attach_docs))) {
            doc_ids <- c(doc_ids, insertDoc(list(name = input$attach_docs$name[ii], datapath = input$attach_docs$datapath[ii])))
          }
        }
        doc_array <- if (length(doc_ids) > 0) paste0('{', paste(doc_ids, collapse=','), '}') else NA_character_

        samples <- unique(data$df[, c('sample', 'datetime')])
        for (i in seq_len(nrow(samples))) {
          q <- "INSERT INTO discrete.samples (location_id, sub_location_id, media_id, datetime, collection_method, sample_type, owner, documents) VALUES ($1,$2,$3,$4,$5,$6,$7,$8::integer[]) RETURNING sample_id"
          sid <- DBI::dbGetQuery(con, q,
                                 params = list(as.integer(input$location),
                                               if (nzchar(input$sublocation)) as.integer(input$sublocation) else NA,
                                               1L, samples$datetime[i], 1L, 1L, 1L, doc_array))$sample_id
          rows <- data$df[data$df$sample == samples$sample[i], ]
          for (j in seq_len(nrow(rows))) {
            if (!is.na(rows$parameter_id[j]) && !is.na(rows$value[j])) {
              DBI::dbExecute(con,
                             "INSERT INTO discrete.results (sample_id, result_type, parameter_id, result) VALUES ($1,1,$2,$3)",
                             params = list(sid, rows$parameter_id[j], rows$value[j]))
            }
          }
        }

        showNotification('Data added.', type = 'message')
        data$df <- data.frame(sample = integer(), datetime = as.POSIXct(character()), parameter_id = integer(), value = numeric())
      }, error = function(e) {
        showNotification(paste('Upload failed:', e$message), type = 'error')
      })
    })
  })
}
