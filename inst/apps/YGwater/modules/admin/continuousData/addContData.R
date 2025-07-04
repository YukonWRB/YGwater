# UI and server code for adding new continuous measurements

addContDataUI <- function(id) {
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
        open = "ts_panel",
        accordion_panel(
          id = ns("ts_panel"),
          title = "Timeseries selection",
          actionButton(ns("addNewTS"), "Click here to add a new timeseries"),
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
            div(
              actionButton(ns("add_row"), "Add row"),
              actionButton(ns("delete_rows"), "Delete selected rows")
            ),
            tags$br()
          ),
          # space so the buttons don't overlap the table
          # Text to tell the user they can edit values by double clicking on the desired cell
          tags$div("Hint: double click on a cell to edit its value."),
          tags$br(),
          DT::DTOutput(ns("data_table")),
          selectizeInput(ns("UTC_offset"), "UTC offset (in hours)", choices = seq(-12, 12, by = 1), selected = 0, multiple = FALSE),
          radioButtons(ns("no_update"), "Prevent update to new data by automatic processes?", 
                       choices = c("Yes" = "yes", "No" = "no"), inline = TRUE, selected = "no"),
          div(
            actionButton(ns("upload"), "Upload to AquaCache (no overwrite)",
                         style = "font-size: 14px;"),
            actionButton(ns("upload_overwrite_all"), "Upload to AquaCache (replace all points in new data range)",
                         style = "font-size: 14px;"),
            actionButton(ns("upload_overwrite_some"), "Upload to AquaCache (overwrite conflicting points only)",
                         style = "font-size: 14px;")
          )
        )
      )
    )
  )
}

addContData <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    outputs <- reactiveValues() # Used to pass the user on to adding a timeseries directly
    
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
      shinyjs::disable('upload_overwrite_all')
      shinyjs::disable('upload_overwrite_some')
    }
    
    ts_meta <- reactive({
      dbGetQueryDT(session$userData$AquaCache,
                   "SELECT timeseries_id, location_name AS location, parameter_name AS parameter, media_type AS media, aggregation_type AS aggregation, recording_rate AS nominal_record_rate, note FROM continuous.timeseries_metadata_en")
    })
    
    observeEvent(input$addNewTS, {
      outputs$change_tab <- "addTimeseries"
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
      data$df <- rbind(data$df, data.frame(datetime = .POSIXct(Sys.time(), tz = "UTC"), value = NA))
    })
    
    observeEvent(input$delete_rows, {
      req(input$data_table_rows_selected)
      data$df <- data$df[-input$data_table_rows_selected, , drop = FALSE]
    })
    
    output$data_table <- DT::renderDT({
      DT::datatable(data$df, 
                    editable = TRUE,
                    selection = "multiple",
                    options = list(scrollX = TRUE))
    }, server = FALSE)
    
    observeEvent(input$data_table_cell_edit, {
      info <- input$data_table_cell_edit
      data$df[info$row, info$col] <- info$value
    })
    
    check_fx <- function() {
      if (is.null(timeseries())) {
        showNotification('Please select a timeseries first.', type = 'error')
        return(FALSE)
      }
      if (nrow(data$df) == 0) {
        showNotification('Empty data table!', type = 'error')
        return(FALSE)
      }
      if (any(is.na(data$df$value))) {
        showNotification('Data contains NA values. Please fill them in before uploading.', type = 'error')
        return(FALSE)
      }
      # Make sure datetime is in POSIXct format or can be converted to it
      if (!inherits(data$df$datetime, "POSIXct")) {
        tryCatch({
          data$df$datetime <- as.POSIXct(data$df$datetime, tz = "UTC")
        }, error = function(e) {
          showNotification('Datetime column is not in the correct format. Please check your data: it should be of form YYYY-MM-DD HH:MM.', type = 'error')
          return(FALSE)
        })
      }
      return(TRUE)
    }
    
    observeEvent(input$upload, {
      check <- check_fx()
      if (!check) return()
      tryCatch({
        upload_data <- data$df
        upload_data$datetime <- upload_data$datetime - (as.numeric(input$UTC_offset) * 3600)  # Adjust datetime to UTC 0
        data$no_update <- ifelse(input$no_update == "yes", TRUE, FALSE)
        AquaCache::addNewContinuous(tsid = timeseries(), df = upload_data, con = session$userData$AquaCache, target = "realtime", overwrite = "no")
        showNotification('Data added.', type = 'message')
        data$df <- data.frame(datetime = as.POSIXct(character()), value = numeric())
      }, error = function(e) {
        showNotification(paste('Upload failed:', e$message), type = 'error')
      }, warning = function(w) {
        showNotification(paste('Warning on upload:', w$message), type = 'warning')
      }, message = function(m) {
        showNotification(paste('Message on upload:', m$message), type = 'message')
      })
    })
    observeEvent(input$upload_overwrite_all, {
      check <- check_fx()
      if (!check) return()
      # Show a modal dialog to confirm overwritting all points in time range of new data
      showModal(modalDialog(
        title = 'Confirm Overwrite',
        'This will overwrite all points in the time range of the new data, including any points that do not conflict with the new data (if any). Are you sure?',
        easyClose = TRUE,
        footer = tagList(
          modalButton('Cancel'),
          actionButton(ns('confirm_overwrite_all'), 'Confirm Overwrite', class = 'btn-danger')
        )
      ))
    })
    observeEvent(input$confirm_overwrite_all, {
      removeModal()  # Close the modal dialog
      tryCatch({
        upload_data <- data$df
        upload_data$datetime <- upload_data$datetime - (as.numeric(input$UTC_offset) * 3600)  # Adjust datetime to UTC
        data$no_update <- ifelse(input$no_update == "yes", TRUE, FALSE)        
        AquaCache::addNewContinuous(tsid = timeseries(), df = upload_data, con = session$userData$AquaCache, target = "realtime", overwrite = "all")
        showNotification('Data added with overwrite.', type = 'message')
        data$df <- data.frame(datetime = as.POSIXct(character()), value = numeric())
      }, error = function(e) {
        showNotification(paste('Upload failed:', e$message), type = 'error')
      }, warning = function(w) {
        showNotification(paste('Warning on upload:', w$message), type = 'warning')
      }, message = function(m) {
        showNotification(paste('Message on upload:', m$message), type = 'message')
      })
    })
    
    observeEvent(input$upload_overwrite_some, {
      check <- check_fx()
      if (!check) return()
      # Show a modal dialog to confirm overwriting conflicting points only
      showModal(modalDialog(
        title = 'Confirm Selective Overwrite',
        'This will overwrite only the points in the new data that conflict with existing points in the time range of the new data. Are you sure?',
        easyClose = TRUE,
        footer = tagList(
          modalButton('Cancel'),
          actionButton(ns('confirm_overwrite_some'), 'Confirm Selective Overwrite', class = 'btn-warning')
        )
      ))
    })
    observeEvent(input$confirm_overwrite_some, {
      removeModal()  # Close the modal dialog
      tryCatch({
        upload_data <- data$df
        upload_data$datetime <- upload_data$datetime - (as.numeric(input$UTC_offset) * 3600)  # Adjust datetime to UTC
        data$no_update <- ifelse(input$no_update == "yes", TRUE, FALSE)
        AquaCache::addNewContinuous(tsid = timeseries(), df = upload_data, con = session$userData$AquaCache, target = "realtime", overwrite = "conflict")
        showNotification('Data added with selective overwrite.', type = 'message')
        data$df <- data.frame(datetime = as.POSIXct(character()), value = numeric())
      }, error = function(e) {
        showNotification(paste('Upload failed:', e$message), type = 'error')
      }, warning = function(w) {
        showNotification(paste('Warning on upload:', w$message), type = 'warning')
      }, message = function(m) {
        showNotification(paste('Message on upload:', m$message), type = 'message')
      })
    })
    
    return(outputs)
  }) # End of moduleServer
}
