# UI and server code for adding discrete measurements
addDiscDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(
      HTML(sprintf("
     /* Add colors to the accordion. Using ns() makes it specific to this module */
      #%s.accordion {
        /* body background */
        --bs-accordion-bg:          #FFFCF5;
        /* collapsed header */
        --bs-accordion-btn-bg:      #FBE5B2;
        /* expanded header */
        --bs-accordion-active-bg:   #FBE5B2;
      }
    ", ns("accordion1"))),
      HTML(sprintf("
     /* Add colors to the accordion. Using ns() makes it specific to this module */
      #%s.accordion {
        /* body background */
        --bs-accordion-bg:          #E5F4F6;
        /* collapsed header */
        --bs-accordion-btn-bg:      #0097A9;
        /* expanded header */
        --bs-accordion-active-bg:   #0097A9;
      }
    ", ns("accordion2")))),
    
    page_fluid(
      accordion(
        id = ns("accordion1"),
        open = "loc_panel",
        accordion_panel(
          id = ns("loc_panel"),
          title = "Location",
          selectizeInput(ns("location"), "Location", 
                         multiple = TRUE,
                         choices = "placeholder",
                         options = list(create = TRUE,
                                      placeholder = "Select a location",
                                      maxItems = 1)),
          selectizeInput(ns("sublocation"), "Sub-location", 
                         multiple = TRUE,
                         choices = "placeholder",
                         options = list(create = TRUE,
                                      placeholder = "Select a sub-location (if exist)",
                                      maxItems = 1)),
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
            radioButtons(ns("file_format"), "File format", choices = c(Wide = "wide", Long = "long"), inline = TRUE),
            conditionalPanel(
              condition = "input.file_format == 'wide'",
              ns = ns,
              numericInput(ns("param_row"), "Row with parameters", value = 1, min = 1),
              numericInput(ns("unit_row"), "Row with units", value = 2, min = 1)
            ),
            conditionalPanel(
              condition = "input.file_format == 'long'",
              ns = ns,
              uiOutput(ns("long_format_cols"))
            ),
            selectizeInput(ns("mapping_select"), "Column mapping", choices = NULL),
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
    
    outputs <- reactiveValues()
    
    check_results <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT has_table_privilege(current_user, 'discrete.results', 'INSERT') AS can_insert"
    )
    check_samples <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT has_table_privilege(current_user, 'discrete.samples', 'INSERT') AS can_insert"
    )
    if (!check_results$can_insert || !check_samples$can_insert) {
      showModal(modalDialog(
        title = "Insufficient Privileges",
        "You do not have write privileges to add samples or results to the database. Please contact your database administrator.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      shinyjs::disable("upload")
    }
    

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
      updateSelectizeInput(session, "mapping_select", choices = c("", names(mappings())))
    })
    
    observeEvent(input$mapping_select, {
      m <- mappings()[[input$mapping_select]]
      if (!is.null(m)) {
        updateRadioButtons(session, "file_format", selected = m$format)
        if (identical(m$format, "long")) {
          updateSelectizeInput(session, "long_param_col", selected = m$param_col)
          updateSelectizeInput(session, "long_unit_col", selected = m$unit_col)
          updateSelectizeInput(session, "long_value_col", selected = m$value_col)
        } else {
          updateNumericInput(session, "param_row", value = m$param_row)
          updateNumericInput(session, "unit_row", value = m$unit_row)
        }
      }
    }, ignoreInit = TRUE)

    params <- reactive({
      dbGetQueryDT(session$userData$AquaCache,
                   "SELECT parameter_id, param_name FROM public.parameters ORDER BY param_name")
    })

    locations <- DBI::dbGetQuery(session$userData$AquaCache,
                                 "SELECT location_id, name FROM public.locations ORDER BY name")
    sub_locations <- DBI::dbGetQuery(session$userData$AquaCache,
                                     "SELECT sub_location_id, sub_location_name FROM public.sub_locations ORDER BY sub_location_name")

    updateSelectizeInput(session, "location",
                      choices = stats::setNames(locations$location_id, locations$name))
    updateSelectizeInput(session, "sublocation", 
                         choices = stats::setNames(sub_locations$sub_location_id, sub_locations$sub_location_name))
    
    
    observeEvent(input$location, {
      if (input$location %in% locations$location_id || nchar(input$location) == 0) {
        return()
      }
      showModal(modalDialog(
        sprintf("Add location '%s'?", input$location),
        footer = tagList(
          modalButton("No"),
          actionButton(ns("goto_add_loc"), "Yes")
        )
      ))
    }, ignoreInit = TRUE)
    
    observeEvent(input$goto_add_loc, {
      removeModal()
      outputs$change_tab <- "addLocation"
      outputs$location <- input$location
    })
    
    observeEvent(input$sublocation, {
      if (input$sublocation %in% sub_locations$sub_location_id || nchar(input$sublocation) == 0) {
        return()
      }
      showModal(modalDialog(
        sprintf("Add sub-location '%s'?", input$sublocation),
        footer = tagList(
          modalButton("No"),
          actionButton(ns("goto_add_subloc"), "Yes")
        )
      ))
    }, ignoreInit = TRUE)
    
    observeEvent(input$goto_add_subloc, {
      removeModal()
      outputs$change_tab <- "addSubLocation"
      output$sub_location <- input$sublocation
    })
    
    file_data <- reactiveVal(NULL)

    observeEvent(input$file, {
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      df <- if (ext %in% c("xls", "xlsx")) {
        openxlsx::read.xlsx(input$file$datapath)
      } else {
        utils::read_csv(input$file$datapath)
      }
      file_data(df)
    })

    output$long_format_cols <- renderUI({
      req(file_data())
      df <- file_data()
      current <- mappings()[[input$mapping_select]]
      tagList(
        selectizeInput(ns("long_param_col"), "Parameter column", choices = names(df), selected = current$param_col),
        selectizeInput(ns("long_unit_col"), "Unit column", choices = c("", names(df)), selected = current$unit_col),
        selectizeInput(ns("long_value_col"), "Value column", choices = names(df), selected = current$value_col)
      )
    })
    
    output$mapping_ui <- renderUI({
      req(file_data())
      df <- file_data()
      current <- mappings()[[input$mapping_select]]
      if (is.null(current)) current <- list(format = input$file_format, datetime = NULL, params = list())
      if (is.null(current$format)) current$format <- input$file_format
      if (identical(current$format, "long")) {
        req(input$long_param_col, input$long_value_col)
        param_names <- unique(df[[input$long_param_col]])
        tagList(
          selectizeInput(ns("map_datetime"), "Datetime column", choices = names(df), selected = current$datetime),
          lapply(param_names, function(p) {
            key <- gsub("[^A-Za-z0-9]", "_", as.character(p))
            par_sel <- if (!is.null(current$params[[as.character(p)]])) current$params[[as.character(p)]]$id else NULL
            conv_sel <- if (!is.null(current$params[[as.character(p)]])) current$params[[as.character(p)]]$conv else 1
            fluidRow(
              column(8,
                     selectizeInput(ns(paste0("param_", key)), sprintf("Map '%s'", p),
                                 choices = stats::setNames(params()$parameter_id, params()$param_name),
                                 selected = par_sel)),
              column(4, numericInput(ns(paste0("conv_", key)), "Conversion", value = conv_sel))
            )
          })
        )
      } else {
        cols <- names(df)
        tagList(
          selectizeInput(ns("map_datetime"), "Datetime column", choices = cols, selected = current$datetime),
          lapply(cols, function(col) {
            if (!is.null(current$datetime) && identical(col, current$datetime)) return(NULL)
            par_sel <- if (!is.null(current$params[[col]])) current$params[[col]]$id else NULL
            conv_sel <- if (!is.null(current$params[[col]])) current$params[[col]]$conv else 1
            fluidRow(
              column(8,
                     selectizeInput(ns(paste0("param_", col)), sprintf("Parameter for column '%s'", col),
                                 choices = stats::setNames(params()$parameter_id, params()$param_name),
                                 selected = par_sel)),
              column(4, numericInput(ns(paste0("conv_", col)), "Conversion", value = conv_sel))
            )
          })
        )
      }
    })

    observeEvent(input$save_mapping, {
      req(input$mapping_name, file_data())
      df <- file_data()
      if (input$file_format == "long") {
        req(input$long_param_col, input$long_value_col)
        pnames <- unique(df[[input$long_param_col]])
        param_list <- setNames(lapply(pnames, function(p) {
          key <- gsub("[^A-Za-z0-9]", "_", as.character(p))
          list(id = input[[paste0("param_", key)]], conv = input[[paste0("conv_", key)]])
        }), pnames)
        map_list <- list(format = "long",
                         datetime = input$map_datetime,
                         param_col = input$long_param_col,
                         unit_col = input$long_unit_col,
                         value_col = input$long_value_col,
                         params = param_list)
      } else {
        cols <- setdiff(names(df), input$map_datetime)
        param_list <- setNames(lapply(cols, function(col) {
          list(id = input[[paste0("param_", col)]], conv = input[[paste0("conv_", col)]])
        }), cols)
        map_list <- list(format = "wide",
                         datetime = input$map_datetime,
                         param_row = input$param_row,
                         unit_row = input$unit_row,
                         params = param_list)
      }
      m <- mappings()
      m[[input$mapping_name]] <- map_list
      mappings(m)
      saveMappingDB(session$userData$AquaCache, input$mapping_name, map_list)
      updateSelectizeInput(session, "mapping_select", choices = c("", names(mappings())), selected = input$mapping_name)
    })

    data <- reactiveValues(df = data.frame(sample = integer(), datetime = as.POSIXct(character()), parameter_id = integer(), value = numeric()))

    observeEvent(input$add_row, {
      next_sample <- ifelse(nrow(data$df) == 0, 1, max(data$df$sample) + 1)
      data$df <- rbind(data$df, data.frame(sample = next_sample, datetime = Sys.time(), parameter_id = NA_integer_, value = NA_real_))
    })

    observeEvent(list(input$map_datetime, input$save_mapping, input$mapping_select, file_data()), {
      req(file_data())
      df <- file_data()
      mapping <- if (nzchar(input$mapping_select)) mappings()[[input$mapping_select]] else NULL
      if (!is.null(mapping)) {
        if (identical(mapping$format, "long")) {
          param_col <- mapping$param_col
          val_col <- mapping$value_col
          dt_col <- mapping$datetime
          df$row_id <- seq_len(nrow(df))
          df$parameter_id <- vapply(as.character(df[[param_col]]), function(p) mapping$params[[p]]$id, numeric(1))
          conv <- vapply(as.character(df[[param_col]]), function(p) mapping$params[[p]]$conv, numeric(1))
          data$df <- data.frame(sample = df$row_id,
                                datetime = as.POSIXct(df[[dt_col]]),
                                parameter_id = df$parameter_id,
                                value = as.numeric(df[[val_col]]) * conv)
        } else {
          start_row <- max(mapping$param_row, mapping$unit_row, na.rm = TRUE) + 1
          param_cols <- names(mapping$params)
          sub <- df[start_row:nrow(df), c(mapping$datetime, param_cols)]
          sub$row_id <- seq_len(nrow(sub))
          long <- tidyr::pivot_longer(sub, cols = tidyselect::all_of(param_cols), names_to = "pcol", values_to = "value")
          long$parameter_id <- vapply(long$pcol, function(pc) mapping$params[[pc]]$id, numeric(1))
          conv <- vapply(long$pcol, function(pc) mapping$params[[pc]]$conv, numeric(1))
          data$df <- data.frame(sample = long$row_id,
                                datetime = as.POSIXct(long[[mapping$datetime]]),
                                parameter_id = long$parameter_id,
                                value = as.numeric(long$value) * conv)
        }
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
                            params = list(file$name, paste("Uploaded", file$name), ext, list(bin)))$document_id
      return(id)
    }

    observeEvent(input$upload, {
      if (is.null(input$location)) {
        showNotification("Please select a location.", type = "error")
        return()
      }
      if (nrow(data$df) == 0) {
        showNotification("Empty data table!", type = "error")
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
        doc_array <- if (length(doc_ids) > 0) paste0("{", paste(doc_ids, collapse = ','), "}") else NA_character_

        samples <- unique(data$df[, c("sample", "datetime")])
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

        showNotification("Data added.", type = "message")
        data$df <- data.frame(sample = integer(), datetime = as.POSIXct(character()), parameter_id = integer(), value = numeric())
      }, error = function(e) {
        showNotification(paste("Upload failed:", e$message), type = "error")
      })
    })
    
    return(outputs)
  })
}
