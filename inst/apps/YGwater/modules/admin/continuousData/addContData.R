# UI and server code for adding new continuous measurements

addContDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(
      HTML(sprintf("
     /* Add colors to the accordion. Using ns() makes it specific to each accordion */
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
     /* Add colors to the accordion. Using ns() makes it specific to each accordion */
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
        open = "ts_panel",
        accordion_panel(
          id = ns("ts_panel"),
          title = "Timeseries selection",
          DT::DTOutput(ns("ts_table"))
        )
      ),
      accordion(
        id = ns("accordion2"),
        open = "data_panel",
        accordion_panel(
          id = ns("data_panel"),
          title = "New data",
          radioButtons(ns("entry_mode"), "Input method", choices = c("File" = "file", "Manual" = "manual"), inline = TRUE),
          conditionalPanel(
            condition = "input.entry_mode == 'file'",
            ns = ns,
            fileInput(ns("file"), "Upload .csv or Excel", accept = c(".csv", ".xls", ".xlsx"))
          ),
          conditionalPanel(
            condition = "input.entry_mode == 'manual'",
            ns = ns,
            actionButton(ns("add_row"), "Add row")
          ),
          DT::DTOutput(ns("data_table")),
          actionButton(ns("upload"), "Upload to AquaCache")
        )
      )
    )
  )
}

addContData <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    
    check <- DBI::dbGetQuery(
      session$userData$AquaCache,
      "SELECT has_table_privilege(current_user, 'continuous.measurements_continuous', 'INSERT') AS can_insert"
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
    
    ts_meta <- reactive({
      dbGetQueryDT(session$userData$AquaCache,
                   "SELECT timeseries_id, location_name AS location, parameter_name AS parameter, media_type AS media, aggregation_type AS aggregation, recording_rate AS nominal_record_rate, note FROM continuous.timeseries_metadata_en")
    })
    
    output$ts_table <- DT::renderDT({
      DT::datatable(ts_meta(),
                    selection = 'single',
                    options = list(
                      columnDefs = list(list(targets = 0, visible = FALSE)),
                      scrollX = TRUE,
                      initComplete = htmlwidgets::JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({",
                        "  'background-color': '#079',",
                        "  'color': '#fff',",
                        "  'font-size': '100%',",
                        "});",
                        "$(this.api().table().body()).css({",
                        "  'font-size': '90%',",
                        "});",
                        "}"
                      )
                    ),
                    rownames = FALSE)
    })
    
    timeseries <- reactiveVal(NULL)
    observeEvent(input$ts_table_rows_selected, {
      sel <- input$ts_table_rows_selected
      if (length(sel) > 0) {
        timeseries(ts_meta()[sel, 'timeseries_id'][[1]])
      } else {
        timeseries(NULL)
      }
    })
    
    data <- reactiveValues(df = data.frame(datetime = as.POSIXct(character()), value = numeric()))
    
    observeEvent(input$file, {
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      df <- if (ext %in% c('xls', 'xlsx')) {
        openxlsx::read.xlsx(input$file$datapath)
      } else {
        readr::read_csv(input$file$datapath, show_col_types = FALSE)
      }
      data$df <- df
    })
    
    observeEvent(input$add_row, {
      data$df <- rbind(data$df, data.frame(datetime = Sys.time(), value = NA))
    })
    
    output$data_table <- DT::renderDT({
      DT::datatable(data$df, editable = TRUE, options = list(scrollX = TRUE))
    }, server = FALSE)
    
    observeEvent(input$data_table_cell_edit, {
      info <- input$data_table_cell_edit
      data$df[info$row, info$col] <- info$value
    })
    
    observeEvent(input$upload, {
      print("timeseries_id is")
      print(timeseries())
      print("data$df is")
      print(data$df)
      if (is.null(timeseries())) {
        showNotification('Please select a timeseries first.', type = 'error')
        return()
      }
      if (nrow(data$df) == 0) {
        showNotification('Empty data table!', type = 'error')
        return()
      }
      if (any(is.na(data$df$value))) {
        showNotification('Data contains NA values. Please fill them in before uploading.', type = 'error')
        return()
      }
      tryCatch({
        AquaCache::addNewContinuous(tsid = timeseries(), df = data$df, con = session$userData$AquaCache, target = "realtime")
        showNotification('Data added.', type = 'message')
        data$df <- data.frame(datetime = as.POSIXct(character()), value = numeric())
      }, error = function(e) {
        showNotification(paste('Upload failed:', e$message), type = 'error')
      }, warning = function(w) {
        showNotification(paste('Warning on upload:', w$message), type = 'warning')
      })
    })
    
  }) # End of moduleServer
}
